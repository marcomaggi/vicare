;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module (introduce-unsafe-primrefs)
  ;;This optional compiler  pass recognises the application of  *safe* core primitive
  ;;functions  having  operands of  the  correct  type  and  replaces them  with  the
  ;;corresponding  application of  *unsafe* core  primitive functions  or operations.
  ;;The result is faster code.
  ;;
  ;;It makes sense to perform this compiler pass only if CORE-TYPE-INFERENCE has been
  ;;performed first.
  ;;
  ;;Accept as input a nested hierarchy of the following structs:
  ;;
  ;;   constant		prelex		primref
  ;;   bind		fix		conditional
  ;;   seq		clambda		known
  ;;   forcall		funcall
  ;;
  (define-syntax __module_who__
    (identifier-syntax 'introduce-unsafe-primrefs))

  (define (introduce-unsafe-primrefs x)
    (E x))


(define* (E x)
  (struct-case x
    ((constant)
     x)

    ((prelex)
     x)

    ((primref op)
     x)

    ((seq e0 e1)
     (make-seq (E e0) (E e1)))

    ((conditional test conseq altern)
     (make-conditional (E test) (E conseq) (E altern)))

    ((bind lhs* rhs* body)
     (make-bind lhs* ($map/stx E rhs*) (E body)))

    ((fix  lhs* rhs* body)
     (make-fix  lhs* ($map/stx E rhs*) (E body)))

    ((clambda)
     (E-clambda x))

    ((funcall rator rand*)
     (E-funcall rator rand*))

    ((forcall rator rand*)
     (make-forcall rator ($map/stx E rand*)))

    (else
     (compiler-internal-error __module_who__ __who__
       "invalid expression" (unparse-recordized-code x)))))

(define (E-known x)
  (struct-case x
    ((known x.expr x.type)
     (make-known (E x.expr) x.type))
    (else x)))

(module (E-clambda)
  ;;The purpose of this module is to apply E to all the CLAMBDA clause bodies.
  ;;
  (define (E-clambda x)
    (struct-case x
      ((clambda label clause* cp free name)
       (make-clambda label ($map/stx E-clambda-clause clause*) cp free name))))

  (define (E-clambda-clause clause)
    (struct-case clause
      ((clambda-case info body)
       (make-clambda-case info (E body)))))

  #| end of module: E-clambda |# )


(module (E-funcall)
  (module (core-primitive-name->signature*
	   core-primitive-name->replacement*)
    (import CORE-PRIMITIVE-PROPERTIES))

  (define (E-funcall rator rand*)
    (let ((rand*^ ($map/stx E-known rand*)))
      (struct-case rator
	((primref op)
	 (%E-primref-call op rand*^))
	(else
	 (make-funcall (E-known rator) rand*^)))))

  (define* (%E-primref-call safe-prim-name rand*)
    (define (%no-replacement)
      (make-funcall (make-primref safe-prim-name) rand*))
    (parametrise ((%exception-raiser compile-time-error))
      (cond ((null? rand*)
	     (%no-replacement))
	    ((%compatible-operands-for-primitive-call? safe-prim-name rand*)
	     (or (%find-core-primitive-replacement safe-prim-name rand*)
		 (%no-replacement)))
	    ((option.strict-r6rs)
	     ;;The operands do  not match the expected arguments:  resort to run-time
	     ;;error as mandated by R6RS.
	     (print-compiler-warning-message "operands of invalid core type in call to core primitive: ~a"
					     (unparse-recordized-code/sexp (%no-replacement)))
	     (%no-replacement))
	    (else
	     ((%exception-raiser) __module_who__ __who__
	      "operands of invalid core type in call to core primitive"
	      (unparse-recordized-code/sexp (%no-replacement)))))))

  (define %exception-raiser
    ;;Procedure used to raise an exception when the validation or operands fails.
    (make-parameter compile-time-error))

  (define (%compatible-operands-for-primitive-call? prim-name rand*)
    ;;Validate the operands  against the types expected by the  core primitive.  If
    ;;they are compatible: return true, otherwise return false.
    ;;
    (cond ((core-primitive-name->signature* prim-name)
	   => (lambda (signature*)
		;;We expect SIGNATURE* to be a list of pairs with the format:
		;;
		;;   ((?rand-preds . ?rv-preds) ...)
		;;
		;;in which  both ?RAND-PREDS and  ?RV-PREDS are proper  or improper
		;;lists of type predicates and false objects.
		(find (lambda (signature)
			(%compatible-type-predicates-and-operands? (car signature) rand*))
		  signature*)))
	  ;;This  core primitive  has no  registered type  signatures.  Let's  fake
	  ;;matching arguments.
	  ;;
	  ;;FIXME  In future  we should  replace this  with an  exception: all  the
	  ;;primitive should have a type  specification.  (Marco Maggi; Sun Sep 21,
	  ;;2014)
	  (else #t)))

  (define (%matching-operands-for-primitive-call? prim-name rand*)
    ;;Validate the operands  against the types expected by the  core primitive.  If
    ;;they match: return true, otherwise return false.
    ;;
    (cond ((core-primitive-name->signature* prim-name)
	   => (lambda (signature*)
		;;We expect SIGNATURE* to be a list of pairs pair with the format:
		;;
		;;   ((?rand-preds . ?rv-preds) ...)
		;;
		;;in which  both ?RAND-PREDS and  ?RV-PREDS are proper  or improper
		;;lists of type predicates and false objects.
		(find (lambda (signature)
			(%matching-type-predicates-and-operands? (car signature) rand*))
		  signature*)))
	  ;;This core primitive has no  registered type signatures.  We must assume
	  ;;the operands do *not* match.
	  (else #f)))

  (define (%find-core-primitive-replacement original-prim-name rand*)
    ;;This function  should be  called if  we know  the list  of operands  in RAND*
    ;;matches the expected argument types of the ORIGINAL-PRIM-NAME.  Scan the list
    ;;of registered unsafe  primitives that can replace  ORIGINAL-PRIM-NAME for one
    ;;whose  expected argument  types match  the  RAND*.  If  successful: return  a
    ;;FUNCALL struct that must replace the original; otherwise return false.
    ;;
    (cond ((core-primitive-name->replacement* original-prim-name)
	   ;;REPLACEMENT-PRIM-NAME* is a list  of symbols representing public names
	   ;;of unsafe primitives.
	   => (lambda (replacement-prim-name*)
		(exists (lambda (replacement-prim-name)
			  (and (%matching-operands-for-primitive-call? replacement-prim-name rand*)
			       (make-funcall (make-primref replacement-prim-name) rand*)))
		  replacement-prim-name*)))
	  ;;This primitive has no registered unsafe replacements.
	  (else #f)))

  (define* (%compatible-type-predicates-and-operands? preds rand*)
    ;;Recursive  function.   Validate  the  operands  in  RAND*  against  the  type
    ;;predicates in  PREDS; if they  are compatible: return true,  otherwise return
    ;;false.   The  purpose of  this  function  is to  check  if  the operands  are
    ;;*invalid* according to the KNOWN structs.
    ;;
    ;;PREDS must  be a  proper or  improper list of  predicates and  false objects.
    ;;RAND* must be a proper list of structs representing recordised code.
    ;;
    (cond ((pair? rand*)
	   ;;There are further operands to be processed.
	   (cond ((pair? preds)
		  ;;There are further predicates to be processed.
		  (and (%compatible-type-predicate-and-operand?   (car preds) (car rand*))
		       (%compatible-type-predicates-and-operands? (cdr preds) (cdr rand*))))
		 ((null? preds)
		  ;;We  expect a  fixed  number of  arguments.   There are  further
		  ;;operands but no  more predicates.  The operands  do *not* match
		  ;;the predicates.
		  (%exception-raiser compile-time-arity-error)
		  #f)
		 ((procedure? preds)
		  ;;We expect  a variable  number of  arguments; the  rest operands
		  ;;must all satisfy the predicate PREDS.
		  (and (%compatible-type-predicate-and-operand?   preds (car rand*))
		       (%compatible-type-predicates-and-operands? preds (cdr rand*))))
		 ((not preds)
		  ;;We expect  a variable  number of  arguments; the  rest operands
		  ;;must  all  satisfy the  predicate  "T:object?",  which they  do
		  ;;whatever their type.  If we  are here: all the operands matched
		  ;;the type predicates: total success!!!
		  #t)
		 (else
		  (compiler-internal-error __module_who__ __who__
		    "invalid type specification in signature of core primitive"
		    preds))))
	  ((pair? preds)
	   ;;There are  no more  operands to  be processed,  but there  are further
	   ;;predicates.  The operands do *not* match the predicates.
	   #f)
	  (else
	   ;;No more operands to be processed,  no more predicates to be processed.
	   ;;All the operands matched the type predicates: total success!!!
	   #t)))

  (define* (%matching-type-predicates-and-operands? preds rand*)
    ;;Recursive  function.   Validate  the  operands  in  RAND*  against  the  type
    ;;predicates in PREDS; if they match: return true, otherwise return false.  The
    ;;purpose of this function is to check if the operands are *valid* according to
    ;;the KNOWN structs.
    ;;
    ;;PREDS must  be a  proper or  improper list of  predicates and  false objects.
    ;;RAND* must be a proper list of structs representing recordised code.
    ;;
    (cond ((pair? rand*)
	   ;;There are further operands to be processed.
	   (cond ((pair? preds)
		  ;;There are further predicates to be processed.
		  (and (%matching-type-predicate-and-operand?   (car preds) (car rand*))
		       (%matching-type-predicates-and-operands? (cdr preds) (cdr rand*))))
		 ((null? preds)
		  ;;We  expect a  fixed  number of  arguments.   There are  further
		  ;;operands but no  more predicates.  The operands  do *not* match
		  ;;the predicates.
		  #f)
		 ((procedure? preds)
		  ;;We expect  a variable  number of  arguments; the  rest operands
		  ;;must all satisfy the predicate PREDS.
		  (and (%matching-type-predicate-and-operand?   preds (car rand*))
		       (%matching-type-predicates-and-operands? preds (cdr rand*))))
		 ((not preds)
		  ;;We expect  a variable  number of  arguments; the  rest operands
		  ;;must  all  satisfy the  predicate  "T:object?",  which they  do
		  ;;whatever their type.  If we  are here: all the operands matched
		  ;;the type predicates: total success!!!
		  #t)
		 (else
		  (compiler-internal-error __module_who__ __who__
		    "invalid type specification in signature of core primitive"
		    preds))))
	  ((pair? preds)
	   ;;There are  no more  operands to  be processed,  but there  are further
	   ;;predicates.  The operands do *not* match the predicates.
	   #f)
	  (else
	   ;;No more operands to be processed,  no more predicates to be processed.
	   ;;All the operands matched the type predicates: total success!!!
	   #t)))

  (define* (%compatible-type-predicate-and-operand? type? rand)
    ;;Apply the core type specification TYPE? to the operand RAND.  Return false if
    ;;it is known that RAND is not of type TYPE?; otherwise return true.
    ;;
    (cond ((procedure? type?)
	   ;;TYPE? is a type predicate procedure.
	   (struct-case rand
	     ((known _ type)
	      (case (type? type)
		;;Operand's type matches the expected argument's type.
		((yes) #t)
		;;Operand's type does *not* match the expected argument's type.
		((no)
		 (%exception-raiser compile-time-operand-core-type-error)
		 #f)
		;;Operand's type maybe matches  the expected argument's type, maybe
		;;not: it is compatible.
		(else  #t)))
	     ;;Operand of unknown type: let's handle it as compatible.
	     (else #t)))
	  ((not type?)
	   ;;TYPE? is  false, which  means that  this operand can  be of  any type;
	   ;;operand's type matches the expected argument's type.
	   #t)
	  (else
	   (compiler-internal-error __module_who__ __who__
	     "invalid type specification in signature of core primitive" type?))))

  (define* (%matching-type-predicate-and-operand? type? rand)
    ;;Apply the core type specification TYPE?  to the operand RAND.  Return true if
    ;;it is known that RAND is of type TYPE?; otherwise return false.
    ;;
    (cond ((procedure? type?)
	   ;;TYPE? is a type predicate procedure.
	   (struct-case rand
	     ((known _ type)
	      (case (type? type)
		;;Operand's type matches the expected argument's type.
		((yes) #t)
		;;Operand's type does *not* match the expected argument's type.
		((no)  #f)
		;;Operand's type maybe matches  the expected argument's type, maybe
		;;not: let's handle it as *not* matching.
		(else  #f)))
	     ;;Operand of unknown type: let's handle it as *not* matching.
	     (else #f)))
	  ((not type?)
	   ;;TYPE? is  false, which  means that  this operand can  be of  any type;
	   ;;operand's type matches the expected argument's type.
	   #t)
	  (else
	   (compiler-internal-error __module_who__ __who__
	     "invalid type specification in signature of core primitive" type?))))

  #| end of module: E-funcall |# )


;;;; done

#| end of module: INTRODUCE-UNSAFE-PRIMREFS |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
