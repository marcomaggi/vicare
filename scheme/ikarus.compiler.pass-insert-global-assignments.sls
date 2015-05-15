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
(library (ikarus.compiler.pass-insert-global-assignments)
  (export pass-insert-global-assignments)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
;;This module  inserts global assignments forms  that put the value  of lexical top
;;level bindings in the slot "value"  of the corresponding loc gensym; for bindings
;;whose value is a  closure object: the value is also stored in  the "proc" slot of
;;the loc gensym.
;;
;;Accept as input a nested hierarchy of the following structs:
;;
;;   constant		prelex		primref
;;   bind		fix		conditional
;;   seq		clambda		known
;;   forcall		funcall		jmpcall
;;

(define-syntax __module_who__
  (identifier-syntax 'pass-insert-global-assignments))

;;Make the code more readable.
(define-syntax E
  (identifier-syntax pass-insert-global-assignments))

(define* (pass-insert-global-assignments x)
  ;;Perform code transformations traversing the whole hierarchy in X, which must be
  ;;a struct instance representing recordized code, and building a new hierarchy of
  ;;transformed, recordized code; return the new hierarchy.
  ;;
  (struct-case x
    ((constant)
     x)

    ((prelex)
     x)

    ((primref)
     x)

    ((bind lhs* rhs* body)
     #;(assert (for-all (lambda (rhs) (not (clambda? rhs))) rhs*))
     (make-bind lhs* ($map/stx E rhs*)
       (%process-bind lhs* (E body))))

    ((fix lhs* rhs* body)
     #;(assert (for-all (lambda (rhs) (clambda? rhs)) rhs*))
     (make-fix lhs* ($map/stx E-clambda rhs*)
       (%process-fix lhs* (E body))))

    ((conditional test conseq altern)
     (make-conditional (E test) (E conseq) (E altern)))

    ((seq e0 e1)
     (make-seq (E e0) (E e1)))

    ((forcall op rand*)
     (make-forcall op ($map/stx E rand*)))

    ((funcall rator rand*)
     (make-funcall (E-known rator) ($map/stx E-known rand*)))

    ((jmpcall label rator rand*)
     ;;JMPCALL's  rator and  rand* are  not,  by construction,  wrapped into  KNOWN
     ;;structs.
     (make-jmpcall label (E rator) ($map/stx E rand*)))

    (else
     (compiler-internal-error __module_who__ __who__
       "invalid expression" (unparse-recordized-code x)))))


(define (E-clambda rhs)
  (struct-case rhs
    ((clambda label clause* cp freevar* name)
     ;;Apply E to every body of every CASE-LAMBDA clause.
     (let ((clause*^ ($map/stx (lambda (clause)
				 (struct-case clause
				   ((clambda-case info body)
				    (make-clambda-case info (E body)))))
		       clause*)))
       (make-clambda label clause*^ cp freevar* name)))))

(define (E-known x)
  (struct-case x
    ((known expr type)
     (make-known (E expr) type))
    (else
     (E x))))


(module (%process-bind %process-fix)

  (define-constant INIT-PRIMREF
    (mk-primref '$init-symbol-value!))

  (define-constant SET-PRIMREF
    (mk-primref '$set-symbol-value/proc!))

  (define (%process-bind lhs* body)
    ;;Prepend to the body of a BIND  struct a call to $INIT-SYMBOL-VALUE!  for each
    ;;of the PRELEX structs in LHS* representing top level bindings.
    ;;
    ;;$INIT-SYMBOL-VALUE! stores in the "value" field  of the loc gensym the actual
    ;;binding value; if, at run-time, such binding value is recognised as a closure
    ;;object: it is also stored in the "proc" field.
    ;;
    (%insert-assignments lhs* body INIT-PRIMREF))

  (define (%process-fix lhs* body)
    ;;Prepend to the  body of a FIX  struct a call to  $INIT-SYMBOL-VALUE! for each
    ;;PRELEX struct in  LHS* representing top level bindings;  for efficiency, only
    ;;for the first binding: the call is to $SET-SYMBOL-VALUE/PROC!.
    ;;
    ;;$INIT-SYMBOL-VALUE! stores in the "value" field  of the loc gensym the actual
    ;;binding value; if, at run-time, such binding value is recognised as a closure
    ;;object: it is also stored in the "proc" field.
    ;;
    ;;$SET-SYMBOL-VALUE/PROC!  stores in both the  "value" and "proc" fields of the
    ;;loc gensym  the actual binding value;  it is known at  compile-time that such
    ;;value is a closure object resulting from the evaluation of a CLAMBDA struct.
    ;;
    ;;FIXME   Why   in   hell   only   the  first   binding   can   be   set   with
    ;;$SET-SYMBOL-VALUE/PROC!   and  not  all  of   them?   I  have  tried  to  use
    ;;$SET-SYMBOL-VALUE/PROC! for all the bindings  and the result is some infinite
    ;;loop while compiling the boot image.  I do not understand.  (Marco Maggi; Sun
    ;;Aug 31, 2014)
    ;;
    (cond ((null? lhs*)
	   body)
	  ((prelex-global-location (car lhs*))
	   => (lambda (loc)
		(make-seq (make-funcall SET-PRIMREF (list (make-constant loc) (car lhs*)))
			  (%insert-assignments (cdr lhs*) body INIT-PRIMREF))))
	  (else
	   (%process-fix (cdr lhs*) body))))

  (define (%insert-assignments lhs* body pref)
    ($fold-right/stx (lambda (lhs tail)
		       (cond ((prelex-global-location lhs)
			      => (lambda (loc)
				   (make-seq
				    (make-funcall pref (list (make-constant loc) lhs))
				    tail)))
			     (else tail)))
      body lhs*))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
