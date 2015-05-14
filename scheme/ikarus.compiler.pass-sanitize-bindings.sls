;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.pass-sanitize-bindings)
  (export sanitize-bindings)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
;;In this module  we want to make  sure that every CLAMBA struct  appears as direct
;;RHS expression for a FIX struct:
;;
;;   (fix ((?lhs ?clambda)) ?body)
;;
;;so:
;;
;;* CLAMBDA structs that already appear as RHS of FIX structs are left alone.
;;
;;* CLAMBDA structs appearing as RHS in  single binding BIND structs cause the BIND
;;  struct to be replaced by a FIX struct:
;;
;;     (bind ((?lhs ?clambda)) ?body) ==> (fix ((?lhs ?clambda)) ?body)
;;
;;* CLAMBDA  structs appearing as  RHS in multiple  binding BIND structs  cause the
;;  BIND  struct to  be split  into a  BIND struct and  a FIX  struct in  which the
;;  bindings are partitioned:
;;
;;     (bind ((?lhs0 ?clambda)
;;            (?lhs1 ?rhs))
;;       ?body)
;;     ==> (bind ((?lhs1 ?rhs))
;;           (fix ((?lhs0 ?clambda))
;;             ?body))
;;
;;* CLAMBDA structs  appearing as standalone expressions (that is:  not directly as
;;  RHS of a BIND or FIX struct) are "lifted" as follows:
;;
;;     (clambda (?formals ?body) ...)
;;     ==> (fix ((tmp (clambda (?formals ?body) ...)))
;;           tmp)
;;
;;After  this  pass is  complete:  all  the BIND  structs  have  a non-CLAMBDA  RHS
;;expression; all CLAMBDA structs appear as RHS of a FIX struct.
;;
;;Accept as input a nested hierarchy of the following structs:
;;
;;   constant		prelex		primref
;;   bind		fix		conditional
;;   seq		clambda		known
;;   forcall		funcall		typed-expr
;;
;;Examples
;;--------
;;
;;The form:
;;
;;   (bind ((a 123)
;;          (b (lambda (x) (this))))
;;     (that))
;;
;;is transformed into:
;;
;;   (bind ((a 123))
;;     (fix ((b (lambda (x) (this))))
;;       (that)))
;;


(define-syntax __module_who__
  (identifier-syntax 'sanitize-bindings))

;;Make the code more readable.
(define-syntax E
  (identifier-syntax sanitize-bindings))

(define* (sanitize-bindings x)
  ;;Perform code transformation traversing the whole  hierarchy in X, which must be
  ;;a  struct instance  representing  recordized  code in  the  core language,  and
  ;;building  a new  hierarchy  of  transformed, recordized  code;  return the  new
  ;;hierarchy.
  ;;
  (struct-case x
    ((constant)
     x)

    ;;If we are performing this compiler  pass without first having performed "core
    ;;type inference":  there may be  TYPED-EXPR structs  in the input.   We remove
    ;;them.
    ((typed-expr expr)
     (E expr))

    ((prelex)
     x)

    ((primref)
     x)

    ((bind lhs* rhs* body)
     (receive (fixable* bindable*)
	 (partition (lambda (x)
		      (clambda? (cdr x)))
	   ($map/stx cons lhs* rhs*))
       ;;FIXABLE* is  a list  of pairs  (?LHS . ?RHS)  in which  ?RHS is  a CLAMBDA
       ;;struct.  BINDABLE*  is a  list of pairs  (?LHS .  ?RHS)  in which  ?RHS is
       ;;*not* a CLAMBDA struct.
       (%mk-bind ($map/stx car bindable*)
		 ($map/stx (lambda (bindable)
			     (E (cdr bindable)))
		   bindable*)
		 (E-fix ($map/stx car fixable*)
			($map/stx cdr fixable*)
			body))))

    ((fix lhs* rhs* body)
     (E-fix lhs* rhs* body))

    ((conditional test conseq altern)
     (make-conditional (E test) (E conseq) (E altern)))

    ((seq e0 e1)
     (make-seq (E e0) (E e1)))

    ((clambda)
     ;;This is a standalone CLAMBDA struct.
     (let ((tmp (make-prelex-for-tmp-binding)))
       (make-fix (list tmp) (list (E-clambda-rhs x)) tmp)))

    ((forcall op rand*)
     (make-forcall op ($map/stx E rand*)))

    ((funcall rator rand*)
     (make-funcall (E-known rator) ($map/stx E-known rand*)))

    (else
     (compile-time-error __module_who__ __who__
       "invalid expression" (unparse-recordized-code x)))))


;;;; helpers

(define (%mk-bind lhs* rhs* body)
  (if (null? lhs*)
      body
    (make-bind lhs* rhs* body)))

(define (E-fix lhs* rhs* body)
  (if (null? lhs*)
      (E body)
    (make-fix lhs* ($map/stx E-clambda-rhs rhs*) (E body))))

(define (E-clambda-rhs x)
  ;;The argument X  must be a struct  instance of type CLAMBDA  appearing as direct
  ;;RHS of a FIX struct.  The purpose of this function is to apply E to the body of
  ;;each CLAMBDA clause.
  ;;
  (struct-case x
    ((clambda label clause* cp freevar* name)
     (let ((clause*^ ($map/stx
			 (lambda (cls)
			   (struct-case cls
			     ((clambda-case info body)
			      (struct-case info
				((case-info label fml* proper)
				 (let ((info^ (make-case-info label fml* proper)))
				   (make-clambda-case info^ (E body))))))))
		       clause*)))
       (make-clambda label clause*^ cp freevar* name)))))

(define (E-known x)
  (struct-case x
    ((known expr type)
     (make-known (E expr) type))
    (else
     (E x))))


;;;; done

#| end of library |# )

;;; end of file
