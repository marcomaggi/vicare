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
(library (ikarus.compiler.pass-introduce-vars)
  (export introduce-vars)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
;;This module replaces all the PRELEX  structs in recordised code with VAR structs;
;;this is  because from  now on  we need a  different set  of properties  to handle
;;variable bindings.
;;
;;Accept as input a nested hierarchy of the following structs:
;;
;;   constant		prelex		primref
;;   bind		fix		conditional
;;   seq		clambda		known
;;   funcall		forcall		jmpcall
;;
;;NOTE  This module  stores  generated VAR  structs  in the  field  OPERAND of  the
;;associated PRELEX structs.   We do not care about resetting  such field of PRELEX
;;structs, because in subsequent compiler passes the PRELEX structs will be no more
;;used: they will be garbage collected.
;;


(define-syntax __module_who__
  (identifier-syntax 'introduce-vars))

;;Make the code more readable.
(define-syntax E
  (identifier-syntax introduce-vars))

(define* (introduce-vars x)
  ;;Perform code transformation traversing the whole  hierarchy in X, which must be
  ;;a  struct instance  representing  recordized  code in  the  core language,  and
  ;;building  a new  hierarchy  of  transformed, recordized  code;  return the  new
  ;;hierarchy.
  ;;
  (struct-case x
    ((constant)
     x)

    ((prelex)
     (%lookup-already-processed-prelex x))

    ((primref)
     x)

    ((bind lhs* rhs* body)
     #;(assert (for-all (lambda (rhs) (not (clambda? rhs))) rhs*))
     ;;Process the LHS* before everything else!
     (let ((lhs* ($map/stx %prelex->var lhs*)))
       (make-bind lhs* ($map/stx E rhs*) (E body))))

    ((fix lhs* rhs* body)
     #;(assert (for-all (lambda (rhs) (clambda? rhs)) rhs*))
     ;;Process the LHS* before everything else!
     (let ((lhs* ($map/stx %prelex->var lhs*)))
       (make-fix lhs* ($map/stx E-clambda lhs* rhs*) (E body))))

    ((conditional e0 e1 e2)
     (make-conditional (E e0) (E e1) (E e2)))

    ((seq e0 e1)
     (make-seq (E e0) (E e1)))

    ((funcall rator rand*)
     (make-funcall (E-known rator) ($map/stx E-known rand*)))

    ((forcall rator rand*)
     (make-forcall rator ($map/stx E rand*)))

    ((jmpcall label rator rand*)
     ;;JMPCALL's  rator and  rand* are  not,  by construction,  wrapped into  KNOWN
     ;;structs.
     (make-jmpcall label (E rator) ($map/stx E rand*)))

    (else
     (compile-time-error __module_who__ __who__
       "invalid expression" (unparse-recordized-code x)))))


(define (E-clambda lhs rhs)
  ;;Process a FIX-defined binding.
  ;;
  ;;LHS is the struct instance of type VAR to which the function generated from RHS
  ;;will be bound.  This VAR struct is present in reference position in the body of
  ;;the  CLAMBDA clauses;  after  CLAMBDA  lifting, a  further  compiler pass  will
  ;;process these references.
  ;;
  (struct-case rhs
    ((clambda label clause* cp.unset freevar* name)
     (assert (not cp.unset))
     ;;The purpose of  this form is to  apply %PRELEX->VAR to all the  items in the
     ;;ARGS field of all the CASE-INFO structs.  Also we apply E to each body.
     (let ((cp       lhs)
	   (clause*^ ($map/stx (lambda (clause)
				 (struct-case clause
				   ((clambda-case info body)
				    (struct-case info
				      ((case-info label args proper)
				       ;;Process the LHS* before everything else!
				       (let ((info (make-case-info label
								   ($map/stx %prelex->var args)
								   proper)))
					 (make-clambda-case info (E body))))))))
		       clause*)))
       (make-clambda label clause*^ cp freevar* name)))))

(define (E-known x)
  (struct-case x
    ((known expr type)
     (make-known (E expr) type))
    (else
     (E x))))


;;;;

(define (%lookup-already-processed-prelex prel)
  ;;Given a struct  instance of type PRELEX: return the  associated struct instance
  ;;of type VAR.   It is a very bad  error if this function finds a  PRELEX not yet
  ;;processed by %PRELEX->VAR.
  ;;
  (receive-and-return (V)
      ($prelex-operand prel)
    (assert (var? V))))

(define (%prelex->var prel)
  ;;Convert the PRELEX struct PREL into a VAR struct; return the VAR struct.
  ;;
  ;;The generated VAR struct is stored in the field OPERAND of the PRELEX, so that,
  ;;later, references to the PRELEX in  the recordized code can be substituted with
  ;;the VAR.
  ;;
  (assert (not (var? ($prelex-operand prel))))
  (receive-and-return (V)
      (make-unique-var ($prelex-name prel))
    ($set-var-global-location! V ($prelex-global-location    prel))
    ($set-prelex-operand! prel V)))


;;;; done

#| end of library |# )

;;; end of file
