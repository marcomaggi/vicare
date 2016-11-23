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
(library (ikarus.compiler.pass-optimize-for-direct-jumps)
  (export pass-optimize-for-direct-jumps)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code))


;;;; introduction
;;
;;This  module transforms  FUNCALL structs  into  JMPCALL structs  whenever in  the
;;application form:
;;
;;   (funcall ?operator ?operand ...)
;;
;;the ?OPERATOR is a binding reference  known to reference a CLAMBDA struct.  There
;;is a technique that allows the  implementation of this "full closure object call"
;;as a faster "direct jump call" into the closure clause with the correct number of
;;arguments.
;;
;;As example, let's  consider the following code  in which the lambda  sexp has not
;;been integrated at the call site:
;;
;;   (let ((f (lambda (x) x)))
;;     (f 123))
;;
;;it is  known that F  references a  CLAMBDA, so the  application "(f 123)"  can be
;;implemented as direct jump call.  Another  example, when the CLAMBDA has multiple
;;clauses:
;;
;;   (let ((f (case-lambda
;;              ((x)   x)
;;              ((x y) (list x y)))))
;;     (f 1 2))
;;
;;it is known that  F references a CLAMBDA and that it is  called with 2 arguments:
;;there is technique that allows to implement the application "(f 1 2)" as a direct
;;jump to the clause with 2 arguments.
;;
;;Upon entering  this transformation: all  the CLAMBDA  structs must appear  in the
;;input as RHS  init expressions of FIX  structs; all the BIND structs  must have a
;;non-CLAMBDA struct as RHS init expression.
;;
;;Accept as input a nested hierarchy of the following structs:
;;
;;   constant		prelex		primref
;;   bind		fix		conditional
;;   seq		clambda		known
;;   forcall		funcall
;;


(define-syntax __module_who__
  (identifier-syntax 'pass-optimize-for-direct-jumps))

;;Make the code more readable.
(define-syntax E
  (identifier-syntax pass-optimize-for-direct-jumps))

(define* (pass-optimize-for-direct-jumps x)
  ;;Perform code optimisation traversing the whole  hierarchy in X, which must be a
  ;;struct instance representing recordized code in the core language, and building
  ;;a new hierarchy of optimised, recordized code; return the new hierarchy.
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
     (E-bind lhs* rhs* body))

    ((fix lhs* rhs* body)
     #;(assert (for-all (lambda (rhs) (clambda? rhs)) rhs*))
     ;;Here we  know that  RHS* is  a list of  CLAMBDA structs,  because it  is the
     ;;result  of  previous compiler  passes.   We  mark  each  PRELEX in  LHS*  as
     ;;referencing a CLAMBDA  struct, so that later they can  be used for jump-call
     ;;optimisation if they appear in operator position.
     ($for-each/stx $set-prelex-referenced-clambda! lhs* rhs*)
     (make-fix lhs* ($map/stx E-clambda rhs*) (E body)))

    ((conditional test conseq altern)
     (make-conditional (E test) (E conseq) (E altern)))

    ((seq e0 e1)
     (make-seq (E e0) (E e1)))

    ((forcall op rand*)
     (make-forcall op ($map/stx E rand*)))

    ((funcall rator rand*)
     (E-funcall x (E-known rator) ($map/stx E-known rand*)))

    (else
     (compile-time-error __module_who__ __who__
       "invalid expression" (unparse-recordized-code x)))))


(define (E-bind lhs* rhs* body)
  ;;Process LHS*  marking, when  appropriate, the PRELEX  structs as  referencing a
  ;;CLAMBDA struct,  so that  later the PRELEX  in LHS* can  be used  for jump-call
  ;;optimisation if they appear in operator position.
  ;;
  ;;Here we know  that RHS* is *not* a  list of CLAMBDA structs, because  it is the
  ;;result  of  previous  compiler passes;  so  we  try  to  determine if  the  RHS
  ;;expressions will return a CLAMBDA struct.
  ;;
  ;;By default  the PRELEX structs are  marked as *not* referencing  a CLAMBDA upon
  ;;creation,  so if  a PRELEX  does not  reference a  CLAMBDA here  we need  to do
  ;;nothing.
  ;;
  (let ((rhs*^ ($map/stx E rhs*)))
    ($for-each/stx
	(lambda (lhs rhs)
	  (struct-case rhs
	    ((prelex)
	     (cond (($prelex-referenced-clambda rhs)
		    ;;RHS is  a PRELEX  struct referencing  a CLAMBDA  struct; this
		    ;;means LHS  references the same  CLAMBDA struct.  CLAM  is the
		    ;;referenced CLAMBDA struct.
		    => (lambda (clam)
			 ($set-prelex-referenced-clambda! lhs clam)
			 (values)))
		   (else
		    (values))))
	    (else
	     ;;LHS does not reference a CLAMBDA struct.
	     (values))))
      lhs* rhs*^)
    (make-bind lhs* rhs*^ (E body))))


(define (E-clambda x)
  ;;The argument X must be a struct  instance of type CLAMBDA.  The purpose of this
  ;;function is to apply E to the body of each CLAMBDA clause.
  ;;
  (struct-case x
    ((clambda label clause* cp freevar* name)
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


(module (E-funcall)

  (define (E-funcall appform rator rand*)
    ;;RATOR and RAND* have already been processed by E.
    ;;
    (let ((unwrapped-rator (%unwrap-known rator)))
      (cond
       ;;Is UNWRAPPED-RATOR a prelex known to reference a closure?  In this case we
       ;;can attempt an optimization.  CLAM is the referenced CLAMBDA.
       ((and (prelex? unwrapped-rator)
	     ($prelex-referenced-clambda unwrapped-rator))
	=> (lambda (clam)
	     (%optimize-funcall appform clam unwrapped-rator rand*)))

       ;;Is UNWRAPPED-RATOR the low level APPLY operation?  In this case: the first
       ;;RAND* should be a struct  instance representing recordized code which will
       ;;evaluate to a closure.
       ;;
       ;;$$APPLY is  used only  in the  body of the  procedure APPLY,  after having
       ;;validated the  first argument as a  closure object; so, here,  we are sure
       ;;that "(car rand*)" will evaluate to a closure object.
       ((and (primref? unwrapped-rator)
	     (eq? ($primref-name unwrapped-rator) '$$apply))
	;;JMPCALL does not want KNOWN structs as rator and rands.
	(make-jmpcall ((sl-apply-label-func))
		      (%unwrap-known (car rand*))
		      ($map/stx %unwrap-known (cdr rand*))))

       ;;If  we are  here: UNWRAPPED-RATOR  is  just some  unknown struct  instance
       ;;representing recordized code which, when  evaluated, will return a closure
       ;;object.
       (else
	(make-funcall rator rand*)))))

  (define* (%optimize-funcall appform clam prelex-rator rand*)
    ;;Attempt to optimize the function application:
    ;;
    ;;   (PRELEX-RATOR . RAND*)
    ;;
    ;;CLAM is a struct instance of type CLAMBDA.  PRELEX-RATOR is a struct instance
    ;;of type PRELEX which  is known to reference the CLAMBDA in  CLAM.  RAND* is a
    ;;list of struct instances representing  recordized code which, when evaluated,
    ;;will return the operands for the function application.
    ;;
    ;;This function searches for a clause in CLAM which matches the arguments given
    ;;in RAND*:
    ;;
    ;;* If found: return a struct instance of type JMPCALL representing a jump call
    ;;  to the matching clause.
    ;;
    ;;* If not found: just return a  struct instance of type FUNCALL representing a
    ;;  normal function call.
    ;;
    (define num-of-rand* (length rand*))
    (let recur ((clause* ($clambda-cases clam)))
      (define-syntax-rule (%recur-to-next-clause)
	(recur (cdr clause*)))
      (if (pair? clause*)
	  (struct-case ($clambda-case-info (car clause*))
	    ((case-info label fml* proper?)
	     (if proper?
		 ;;This clause has a fixed number of arguments.
		 (if (fx=? num-of-rand* (length fml*))
		     (make-jmpcall label prelex-rator ($map/stx %unwrap-known rand*))
		   (%recur-to-next-clause))
	       ;;This clause has a variable number of arguments.
	       (if (fx<=? (length (cdr fml*)) num-of-rand*)
		   (make-jmpcall label prelex-rator (%prepare-rand* (cdr fml*) rand*))
		 (%recur-to-next-clause)))))
	;;No matching clause found.
	(if (options::strict-r6rs)
	    ;;Just call the closure as always.  A "wrong num args" exception will
	    ;;be raised at run-time as mandated by R6RS.
	    (begin
	      (print-compiler-warning-message "wrong number of arguments in closure object application: ~a"
					      (unparse-recordized-code/pretty appform))
	      (make-funcall prelex-rator rand*))
	  (compile-time-arity-error __module_who__ __who__
	    "wrong number of arguments in closure object application"
	    (unparse-recordized-code/pretty appform))))))

  (define (%prepare-rand* fml* rand*)
    ;;Recursive function.
    ;;
    ;;FML* is a list of structs of type PRELEX representing the formal arguments of
    ;;the CASE-LAMBDA clause.
    ;;
    ;;RAND* is a  list of structs representing the arguments  to the closure object
    ;;application.
    ;;
    ;;This function  processes RAND* and  builds a new list  representing arguments
    ;;that can be assigned to the formals of the clause.  If the clause is:
    ;;
    ;;   ((a b . args) ?body)
    ;;
    ;;and RAND* is:
    ;;
    ;;   (#[constant 1] #[constant 2]
    ;;    #[constant 3] #[constant 4])
    ;;
    ;;this function must prepare a recordized list like:
    ;;
    ;;   (#[constant 1] #[constant 2]
    ;;    #[funcall #[primref list] (#[constant 3] #[constant 4])])
    ;;
    ;;so that the application:
    ;;
    ;;   (?rator ?rand0 ?rand1 ?rand2 ?rand3)
    ;;
    ;;is converted to:
    ;;
    ;;   (?rator ?rand1 ?rand2 (list ?rand3 ?rand4))
    ;;
    ;;where ?RAND1  and ?RAND2 are unwrapped  from KNOWN structs, while  ?RAND3 and
    ;;?RAND4 are not.
    ;;
    (if (pair? fml*)
	(cons (%unwrap-known (car rand*))
	      (%prepare-rand* (cdr fml*) (cdr rand*)))
      ;;FIXME Construct list afterwards.  (Abdulaziz Ghuloum)
      (list (make-funcall (mk-primref 'list) rand*))))

  (define (%unwrap-known x)
    (struct-case x
      ((known expr)
       expr)
      (else x)))

  #| end of module: E-funcall |# )


;;;; done

#| end of library |# )

;;; end of file
