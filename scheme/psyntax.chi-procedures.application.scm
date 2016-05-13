;;; -*- coding: utf-8-unix -*-
;;;
;;;Copyright (c) 2010-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(module (chi-application chi-application/psi-first-operand)
  (define-module-who chi-application)


;;;; chi procedures: general operator application, nothing already expanded

(define* (chi-application input-form.stx lexenv.run lexenv.expand)
  ;;Expand an  operator application form;  it is  called when INPUT-FORM.STX  has the
  ;;format:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;and ?RATOR is  *not* a macro keyword  identifier.  For example it  is called when
  ;;INPUT-FORM.STX is:
  ;;
  ;;   (?core-prim ?rand ...)
  ;;   ((?sub-rator ?sub-rand ...) ?rand ...)
  ;;
  ;;and the sub-application form can be a SPLICE-FIRST-EXPAND syntax.
  ;;
  (syntax-match input-form.stx (values apply map1 for-each1 for-all1 exists1
				       condition call-with-values
				       cons list car cdr vector
				       foldable-cons foldable-list foldable-vector
				       <nelist>-constructor <nevector>-constructor)
    (((?nested-rator ?nested-rand* ...) ?rand* ...)
     ;;Sub-expression application.  It could be a nested expression application:
     ;;
     ;;   ((return-closure 1 2) 3 4)
     ;;
     ;;or a direct LAMBDA application like:
     ;;
     ;;   ((lambda (a b) (+ a b)) 1 2)
     ;;
     ;;of a SPLICE-FIRST-EXPAND use:
     ;;
     ;;   ((splice-first-expand ?sub-rator ?sub-rand ...) ?rand)
     ;;
     ;;Because of the last case, we process this specially.
     (chi-nested-rator-application input-form.stx lexenv.run lexenv.expand
				   (cons ?nested-rator ?nested-rand*) ?rand*))

    ((values . ?rand*)
     ;;A call to VALUES is special because  VALUES does not have a predefined retvals
     ;;signature, but the retvals signature equals the arguments' signature.
     (chi-values-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((condition . ?rand*)
     (chi-condition-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((cons . ?rand*)
     (chi-cons-application input-form.stx lexenv.run lexenv.expand ?rand* 'cons))

    ((foldable-cons . ?rand*)
     (chi-cons-application input-form.stx lexenv.run lexenv.expand ?rand* 'foldable-cons))

    ((list . ?rand*)
     (chi-list-application input-form.stx lexenv.run lexenv.expand ?rand* 'list))

    ((foldable-list . ?rand*)
     (chi-list-application input-form.stx lexenv.run lexenv.expand ?rand* 'foldable-list))

    ((<nelist>-constructor . ?rand*)
     (chi-nelist-constructor-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((car . ?rand*)
     (chi-car-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((cdr . ?rand*)
     (chi-cdr-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((vector . ?rand*)
     (chi-vector-application input-form.stx lexenv.run lexenv.expand ?rand* 'vector))

    ((foldable-vector . ?rand*)
     (chi-vector-application input-form.stx lexenv.run lexenv.expand ?rand* 'foldable-vector))

    ((<nevector>-constructor . ?rand*)
     (chi-nevector-constructor-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((apply ?rator ?rand* ...)
     (chi-apply-application input-form.stx lexenv.run lexenv.expand
			    ?rator ?rand*))

    ((map1 ?func ?list)
     (expander-option.integrate-special-list-functions?)
     ;;This is to  be considered experimental.  The purpose of  this code integration
     ;;it not to integrate the list  iteration function, but to allow the integration
     ;;of ?FUNC.
     (let ((map1 (gensym))
	   (L (gensym))
	   (H (gensym))
	   (T (gensym))
	   (V (gensym))
	   (P (gensym)))
       (chi-expr (bless
		  `(let ,map1 ((,L ,?list)
			       (,H #f)	;head
			       (,T #f))	;last pair
			(cond ((pair? ,L)
			       (let* ((,V (,?func ($car ,L))) ;value
				      (,P (cons ,V '()))      ;new last pair
				      (,T (if ,T
					      (begin
						($set-cdr! ,T ,P)
						,P)
					    ,P))
				      (,H (or ,H ,P)))
				 (,map1 ($cdr ,L) ,H ,T)))
			      ((null? ,L)
			       (or ,H '()))
			      (else
			       (procedure-argument-violation 'map1 "expected proper list as argument" ,L)))))
		 lexenv.run lexenv.expand)))

    ((for-each1 ?func ?list)
     (expander-option.integrate-special-list-functions?)
     ;;This is to  be considered experimental.  The purpose of  this code integration
     ;;it not  to integrate the list  iteration function FOR-EACH1, but  to allow the
     ;;integration of ?FUNC.
     (let ((for-each1 (gensym))
	   (L (gensym)))
       (chi-expr (bless
		  `(let ,for-each1 ((,L ,?list))
			(cond ((pair? ,L)
			       (,?func ($car ,L))
			       (,for-each1 ($cdr ,L)))
			      ((null? ,L)
			       (void))
			      (else
			       (procedure-argument-violation 'for-each1 "expected proper list as argument" ,L)))))
		 lexenv.run lexenv.expand)))

    ((for-all1 ?func ?list)
     (expander-option.integrate-special-list-functions?)
     ;;This is to  be considered experimental.  The purpose of  this code integration
     ;;it not  to integrate the  list iteration function  FOR-ALL1, but to  allow the
     ;;integration of ?FUNC.
     (let ((for-all1 (gensym))
	   (L (gensym))
	   (R (gensym)))
       (chi-expr (bless
		  `(let ,for-all1 ((,L ,?list)
				   (,R #t))
			(if (pair? ,L)
			    (let ((,R (,?func ($car ,L))))
			      (and ,R (,for-all1 ($cdr ,L) ,R)))
			  ,R)))
		 lexenv.run lexenv.expand)))

    ((exists1 ?func ?list)
     (expander-option.integrate-special-list-functions?)
     ;;This is to  be considered experimental.  The purpose of  this code integration
     ;;it not  to integrate  the list  iteration function EXISTS1,  but to  allow the
     ;;integration of ?FUNC.
     (let ((exists1 (gensym))
	   (L (gensym)))
       (chi-expr (bless
		  `(let ,exists1 ((,L ,?list))
			(and (pair? ,L)
			     (or (,?func ($car ,L))
				 (,exists1 ($cdr ,L))))))
		 lexenv.run lexenv.expand)))

    ((call-with-values . ?rand*)
     (chi-call-with-values-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((?rator ?rand* ...)
     ;;The input form is either a common function application like:
     ;;
     ;;   (list 1 2 3)
     ;;
     (let ((rator.psi (chi-expr ?rator lexenv.run lexenv.expand)))
       (chi-application/psi-rator input-form.stx lexenv.run lexenv.expand
				  rator.psi ?rand*)))

    (_
     (syntax-violation/internal-error __module_who__
       "invalid application syntax" input-form.stx))))


(module CLOSURE-APPLICATION-ERRORS
  ( ;;
   %error-number-of-operands-exceeds-maximum-arguments-count
   %error-number-of-operands-deceeds-minimum-arguments-count
   %error-operand-with-multiple-return-values
   %error-mismatch-between-args-signature-and-operands-signature
   %warning-mismatch-between-args-signature-and-operands-signature)

  (define-condition-type &wrong-number-of-arguments-error
      &error
    make-wrong-number-of-arguments-error-condition
    wrong-number-of-arguments-error-condition?)

  (define-condition-type &maximum-arguments-count
      &condition
    make-maximum-arguments-count-condition
    maximum-arguments-count-condition?
    (count maximum-arguments-count))

  (define-condition-type &minimum-arguments-count
      &condition
    make-minimum-arguments-count-condition
    minimum-arguments-count-condition?
    (count minimum-arguments-count))

  (define-condition-type &given-operands-count
      &condition
    make-given-operands-count-condition
    given-operands-count-condition?
    (count given-operands-count))

  ;;Contains a list of "<type-signature>" instaces representing the possible types of
  ;;a closure's arguments.   To be used to represent the  possible tuples of accepted
  ;;arguments in a closure application.
  ;;
  (define-condition-type &arguments-signatures
      &condition
    make-arguments-signatures-condition
    arguments-signatures-condition?
    (signatures		arguments-signatures-condition.signatures))

  (define-condition-type &operands-signature
      &condition
    make-operands-signature-condition
    operands-signature-condition?
    (operands-signature	operands-signature-condition.operands-signature))

;;; --------------------------------------------------------------------

  (define (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
	    rator.stx rand*.stx maximum-arguments-count given-operands-count)
    (raise-compound-condition-object 'chi-application
      "while expanding, detected wrong number of operands in function application: number of operands exceeds maximum number of arguments"
      input-form.stx
      (condition
	(make-wrong-number-of-arguments-error-condition)
	(make-maximum-arguments-count-condition maximum-arguments-count)
	(make-given-operands-count-condition given-operands-count)
	(make-syntax-violation input-form.stx #f)
	(make-application-operator-expression-condition rator.stx)
	(make-application-operands-expressions-condition rand*.stx))))

  (define (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
	    rator.stx rand*.stx minimum-arguments-count given-operands-count)
    (raise-compound-condition-object 'chi-application
      "while expanding, detected wrong number of operands in function application: number of operands deceeds minimum number of arguments"
      input-form.stx
      (condition
	(make-wrong-number-of-arguments-error-condition)
	(make-minimum-arguments-count-condition minimum-arguments-count)
	(make-given-operands-count-condition given-operands-count)
	(make-syntax-violation input-form.stx #f)
	(make-application-operator-expression-condition rator.stx)
	(make-application-operands-expressions-condition rand*.stx))))

  (define (%error-operand-with-multiple-return-values input-form.stx rand.stx rand.sig)
    (raise-compound-condition-object 'chi-application
      "expand-time error: operand with multiple return values"
      input-form.stx
      (condition
	(make-expand-time-type-signature-violation)
	(make-syntax-violation input-form.stx rand.stx)
	(make-operands-signature-condition (list rand.sig)))))

  (define (%error-mismatch-between-args-signature-and-operands-signature input-form.stx
	    arguments-signature* operands-signature)
    ;;ARGUMENTS-SIGNATURE*   must  be   a   list   of  "<type-signature>"   instances
    ;;representing the closure object's arguments signatures.
    ;;
    ;;OPERANDS-SIGNATURE must  be an  insance of "<type-signature>"  representing the
    ;;signature of the operands, when each operand returns a single value.
    ;;
    (syntax-match input-form.stx ()
      ((?rator . ?rand*)
       (raise-compound-condition-object 'chi-application
	 "expand-time mismatch between closure object's arguments signatures and operands signature"
	 input-form.stx
	 (condition
	   (make-expand-time-type-signature-violation)
	   (make-syntax-violation input-form.stx ?rator)
	   (make-application-operator-expression-condition ?rator)
	   (make-application-operands-expressions-condition ?rand*)
	   (make-arguments-signatures-condition arguments-signature*)
	   (make-operands-signature-condition operands-signature))))))

  (define (%warning-mismatch-between-args-signature-and-operands-signature input-form.stx
	    arguments-signature* operands-signature)
    ;;ARGUMENTS-SIGNATURE*   must  be   a   list   of  "<type-signature>"   instances
    ;;representing the closure object's arguments signatures.
    ;;
    ;;OPERANDS-SIGNATURE must  be an  insance of "<type-signature>"  representing the
    ;;signature of the operands, when each operand returns a single value.
    ;;
    (when (options::warn-about-compatible-operands-signature-in-procedure-application)
      (syntax-match input-form.stx ()
	((?rator . ?rand*)
	 (raise-compound-condition-object/continuable 'chi-application
	   "expand-time mismatch between closure object's arguments signatures and operands signature"
	   input-form.stx
	   (condition
	     (make-expand-time-type-signature-warning)
	     (make-syntax-violation input-form.stx ?rator)
	     (make-application-operator-expression-condition ?rator)
	     (make-application-operands-expressions-condition ?rand*)
	     (make-arguments-signatures-condition arguments-signature*)
	     (make-operands-signature-condition operands-signature)))))))

  #| end of module: CLOSURE-APPLICATION-ERRORS |# )


(define (chi-nested-rator-application input-form.stx lexenv.run lexenv.expand
				      rator.stx rand*.stx)
  ;;Here the input form is:
  ;;
  ;;  ((?nested-rator ?nested-rand* ...) ?rand* ...)
  ;;
  ;;where the  ?NESTED-RATOR can be anything:  a closure application, a  macro use, a
  ;;datum operator application; we expand it  considering the case of rator expanding
  ;;to a SPLICE-FIRST-EXPAND form.
  ;;
  ;;RATOR.STX is the syntax object:
  ;;
  ;;  #'(?nested-rator ?nested-rand* ...)
  ;;
  ;;and RAND*.STX is the list of syntax objects:
  ;;
  ;;  (#'?rand ...)
  ;;
  (let* ((rator.psi  (while-expanding-application-first-subform
		      (chi-expr rator.stx lexenv.run lexenv.expand)))
	 (rator.expr (psi.core-expr rator.psi)))
    ;;Here  RATOR.EXPR is  either an  instance of  "splice-first-envelope" or  a core
    ;;language sexp.
    (import SPLICE-FIRST-ENVELOPE)
    (if (splice-first-envelope? rator.expr)
	(syntax-match (splice-first-envelope-form rator.expr) ()
	  ((?nested-rator ?nested-rand* ...)
	   (chi-expr (cons ?nested-rator (append ?nested-rand* rand*.stx))
		     lexenv.run lexenv.expand))
	  (_
	   (syntax-violation __module_who__
	     "expected list as argument of splice-first-expand"
	     input-form.stx rator.stx)))
      (chi-application/psi-rator input-form.stx lexenv.run lexenv.expand
				 rator.psi rand*.stx))))


(module (chi-values-application)

  (define-module-who chi-values-application)

  (define (chi-values-application input-form.stx lexenv.run lexenv.expand
				  rands.stx)
    ;;The input form has the syntax:
    ;;
    ;;   (values ?rand ...)
    ;;
    ;;and RANDS.STX is the syntax object:
    ;;
    ;;   #'(?rand ...)
    ;;
    ;;which holds a proper list of expressions.
    ;;
    ;;A call to VALUES  is special because VALUES does not  have a predefined retvals
    ;;signature, but the retvals signature equals the operands' signature.
    ;;
    ;;If one  of the  expressions has  a standalone  "<no-return>" as  signature: the
    ;;whole VALUES application must have a standalone "<no-return>" as signature.
    ;;
    (syntax-match rands.stx ()
      (()
       ;;No arguments, zero returned values.  Just evaluate "(values)".
       (make-psi input-form.stx
	 (build-application (syntax-annotation input-form.stx)
	     (build-primref no-source 'values)
	   '())
	 (make-type-signature '())))

      ((?rand)
       ;;Single expression,  single return value.   Just convert "(values  ?expr)" to
       ;;"?expr".
       (chi-expr ?rand lexenv.run lexenv.expand))

      ((?rand ?rand* ...)
       ;;Two or more values.
       (let* ((rand*.stx  (cons ?rand ?rand*))
	      (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	      (rand*.core (map psi.core-expr rand*.psi))
	      (rand*.sig  (map psi.retvals-signature rand*.psi))
	      (rator.core (build-primref no-source 'values)))
	 (let ((application.sig (%rand-signatures->application-signature input-form.stx rand*.stx rand*.sig)))
	   (make-psi input-form.stx
	     (build-application (syntax-annotation input-form.stx)
		 rator.core
	       rand*.core)
	     application.sig))))
      ))

  (define (%rand-signatures->application-signature input-form.stx rand*.stx rand*.sig)
    ;;To be a valid VALUES argument an expression must have as signature:
    ;;
    ;;   (?tag)
    ;;
    ;;which means a single return value.
    ;;
    ;;Here we  also accept allow "<list>"  or a list  sub-type.  If an operand  has a
    ;;standalone "<no-return>" as signature: the whole VALUES application must have a
    ;;standalone "<no-return>" as signature.
    ;;
    (make-type-signature
     (let recur ((rand*.stx	rand*.stx)
		 (rand*.sig	rand*.sig))
       (if (pair? rand*.sig)
	   (cond ((%rand-signature->rand-object-type-spec input-form.stx (car rand*.stx) (car rand*.sig))
		  => (lambda (appl.ots)
		       ;;The  operand   returns  a  single  value   with  object-type
		       ;;specification APPL.OTS.  Good.
		       (cons appl.ots (recur (cdr rand*.stx) (cdr rand*.sig)))))
		 (else
		  ;;The operand has a standalone  "<no-return>" as signature: in this
		  ;;case  the  whole  VALUES   application  must  have  a  standalone
		  ;;"<no-return>" as signature.
		  (<no-return>-ots)))
	 '()))))

  (define (%rand-signature->rand-object-type-spec input-form.stx rand.stx rand.sig)
    (case-signature-specs rand.sig
      ((single-value)
       ;;This operand has a single return value.  Good.
       => (lambda (obj.ots) obj.ots))

      (<no-return>
       ;;If  an expression  has a  standalone "<no-return>"  as signature:  the whole
       ;;VALUES application  must have a  standalone "<no-return>" as  signature.  We
       ;;signal this special case by returning #f.
       #f)

      (<list-of>
       ;;This  operand  has  a  standalone   homogeneous  list  type  as  object-type
       ;;specification.  We accept it and set the specification to a single item OTS.
       ;;We rely on the  compiler to generate code that checks,  at run-time, if this
       ;;operand returns a single value.
       => (lambda (list-of.ots)
	    (list-of-type-spec.item-ots list-of.ots)))

      (<list>
       ;;This operand  has a standalone  "<list>" type as  object-type specification.
       ;;We accept it and set the specification  to a single "<top>" OTS.  We rely on
       ;;the compiler  to generate  code that  checks, at  run-time, if  this operand
       ;;returns a single value.
       (<top>-ots))

      (else
       ;;We have  determined at expand-time  that this  operand returns zero,  two or
       ;;more values or some other invalid value.  Bad.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-who-condition 'values)
			 (make-message-condition "expression used as operand in VALUES application returns multiple values")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise             (condition (make-expand-time-type-signature-violation) (common))))
	   ((default)
	    (raise-continuable (condition (make-expand-time-type-signature-warning)   (common)))
	    (<top>-ots))
	   ((strict-r6rs)
	    (<top>-ots)))))))

  #| end of module: CHI-VALUES-APPLICATION |# )


(module (chi-cons-application)
  ;;The input form has the syntax:
  ;;
  ;;   (cons ?rand ...)
  ;;
  ;;and RANDS.STX is the syntax object:
  ;;
  ;;   #'(?rand ...)
  ;;
  ;;which holds  a proper list  of expressions.  The  application of CONS  is special
  ;;because  we  want  the  expression  to  return  a  type  signature  describing  a
  ;;"<pair-type-spec>".
  ;;
  (import CLOSURE-APPLICATION-ERRORS)

  (define-module-who chi-cons-application)

  (define (chi-cons-application input-form.stx lexenv.run lexenv.expand rands.stx prim-name)
    (syntax-match rands.stx ()
      ((?car-rand ?cdr-rand)
       ;;Two or more values.
       (let* ((car-rand.psi	(chi-expr ?car-rand lexenv.run lexenv.expand))
	      (cdr-rand.psi	(chi-expr ?cdr-rand lexenv.run lexenv.expand))
	      (car-rand.core	(psi.core-expr car-rand.psi))
	      (cdr-rand.core	(psi.core-expr cdr-rand.psi))
	      (car-rand.sig	(psi.retvals-signature car-rand.psi))
	      (cdr-rand.sig	(psi.retvals-signature cdr-rand.psi))
	      (car-rand.ots	(%single-operand-signature->ots input-form.stx ?car-rand car-rand.sig))
	      (cdr-rand.ots	(%single-operand-signature->ots input-form.stx ?cdr-rand cdr-rand.sig)))
	 (let ((application.sig (make-type-signature/single-value (make-pair-type-spec car-rand.ots cdr-rand.ots))))
	   (make-psi input-form.stx
	     (build-application (syntax-annotation input-form.stx)
		 (build-primref no-source prim-name)
	       (list car-rand.core cdr-rand.core))
	     application.sig))))

      ((?rand1 ?rand2 ?rand3 ?rand* ...)
       (let* ((rand*.stx		(cons* ?rand1 ?rand2 ?rand3 ?rand*))
	      (maximum-arguments-count	2)
	      (given-operands-count	(length rand*.stx)))
	 (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
	   input-form.stx rand*.stx maximum-arguments-count given-operands-count)))

      ((?rand* ...)
       (let* ((minimum-arguments-count	2)
	      (given-operands-count	(length ?rand*)))
         (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
	   input-form.stx ?rand* minimum-arguments-count given-operands-count)))
      ))

  (define (%single-operand-signature->ots input-form.stx rand.stx rand.sig)
    (case-signature-specs rand.sig
      ((single-value)
       => (lambda (rand.ots)
	    rand.ots))

      (<no-return>
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand is typed as not returning")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise             (condition (make-expand-time-type-signature-violation) (common))))
	   ((default)
	    (raise-continuable (condition (make-expand-time-type-signature-warning)   (common)))
	    (<top>-ots))
	   ((strict-r6rs)
	    (<top>-ots)))))

      (<list-of>
       ;;The operand expression returns an unspecified number of values of specified,
       ;;homogeneous, type.  We rely on the compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       => (lambda (rand.ots)
	    (list-of-type-spec.item-ots rand.ots)))

      (<list>
       ;;The  operand  expression   returns  an  unspecified  number   of  values  of
       ;;unspecified type.   We relay  on the  automatically generated  validation to
       ;;check at run-time if the expression returns a single value.
       (<top>-ots))

      (else
       ;;The operand expression returns zero, two or more values.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand returns multiple values")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise             (condition (make-expand-time-type-signature-violation) (common))))
	   ((default)
	    (raise-continuable (condition (make-expand-time-type-signature-warning)   (common)))
	    (<top>-ots))
	   ((strict-r6rs)
	    (<top>-ots)))))))

  #| end of module: CHI-CONS-APPLICATION |# )


(module (chi-list-application chi-nelist-constructor-application)
  ;;The input form has the syntax:
  ;;
  ;;   (list ?rand ...)
  ;;
  ;;and RANDS.STX is the syntax object:
  ;;
  ;;   #'(?rand ...)
  ;;
  ;;which holds  a proper list  of expressions.  The  application of LIST  is special
  ;;because  we  want  the  expression  to  return  a  type  signature  describing  a
  ;;"<list-type-spec>".
  ;;
  (import CLOSURE-APPLICATION-ERRORS)

  (define-module-who chi-list-application)

  (define (chi-nelist-constructor-application input-form.stx lexenv.run lexenv.expand rands.stx)
    (syntax-match rands.stx ()
      (()
       (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
	 (core-prim-id '<nelist>-constructor) '() 1 0))

      ((?rand ?rand* ...)
       (chi-list-application input-form.stx lexenv.run lexenv.expand rands.stx '<nelist>-constructor))))

  (define (chi-list-application input-form.stx lexenv.run lexenv.expand rands.stx prim-name)
    (syntax-match rands.stx ()
      (()
       ;;No arguments.  Just return null.
       (make-psi/single-null input-form.stx))

      ((?rand ?rand* ...)
       ;;Two or more values.
       (let* ((rand*.stx  (cons ?rand ?rand*))
	      (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	      (rand*.core (map psi.core-expr rand*.psi))
	      (rand*.sig  (map psi.retvals-signature rand*.psi)))
	 (let ((application.sig (%operand-signatures->application-signature input-form.stx rand*.stx rand*.sig)))
	   (make-psi input-form.stx
	     (build-application (syntax-annotation input-form.stx)
		 (build-primref no-source prim-name)
	       rand*.core)
	     application.sig))))
      ))

  (define (%operand-signatures->application-signature input-form.stx rand*.stx rand*.sig)
    (make-type-signature/single-value
     (make-list-type-spec
      (map (lambda (rand.stx rand.sig)
	     (%single-operand-signature->ots input-form.stx rand.stx rand.sig))
	rand*.stx rand*.sig))))

  (define (%single-operand-signature->ots input-form.stx rand.stx rand.sig)
    (case-signature-specs rand.sig
      ((single-value)
       => (lambda (rand.ots)
	    rand.ots))

      (<no-return>
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand is typed as not returning")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (<top>-ots))
	   ((strict-r6rs)
	    (<top>-ots)))))

      (<list-of>
       ;;The operand expression returns an unspecified number of values of specified,
       ;;homogeneous, type.  We rely on the compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       => (lambda (rand.ots)
	    (list-of-type-spec.item-ots rand.ots)))

      (<list>
       ;;The  operand  expression   returns  an  unspecified  number   of  values  of
       ;;unspecified type.   We relay  on the  automatically generated  validation to
       ;;check at run-time if the expression returns a single value.
       (<top>-ots))

      (else
       ;;The operand expression returns zero, two or more values.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand returns multiple values")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (<top>-ots))
	   ((strict-r6rs)
	    (<top>-ots)))))))

  #| end of module: CHI-LIST-APPLICATION |# )


(module (chi-car-application)
  ;;The input form has the syntax:
  ;;
  ;;   (car ?rand ...)
  ;;
  ;;and RANDS.STX is the syntax object:
  ;;
  ;;   #'(?rand ...)
  ;;
  ;;which holds  a proper  list of  expressions.  The application  of CAR  is special
  ;;because we want  to extract the type of  the returned value from the  type of the
  ;;operand.
  ;;
  (import CLOSURE-APPLICATION-ERRORS)
  (define-module-who chi-car-application)

  (define (%core-prim-id)
    (core-prim-id 'car))

  (define (chi-car-application input-form.stx lexenv.run lexenv.expand rands.stx)
    (syntax-match rands.stx ()
      ((?rand)
       (let* ((rand.psi		(chi-expr ?rand lexenv.run lexenv.expand))
	      (rand.core	(psi.core-expr rand.psi))
	      (rand.sig		(psi.retvals-signature rand.psi))
	      (application.sig	(%operand-signature->application-signature input-form.stx ?rand rand.sig)))
	 (make-psi input-form.stx
	   (build-application (syntax-annotation input-form.stx)
	       (build-primref no-source 'car)
	     (list rand.core))
	   application.sig)))

      (()
       (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
	 (%core-prim-id) '() 1 0))

      ((?rand0 ?rand1 ?rand* ...)
       (let ((rand*.stx (cons* ?rand0 ?rand1 ?rand*)))
	 (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
	   (%core-prim-id) rand*.stx 1 (length rand*.stx))))
      ))

  (define (%operand-signature->application-signature input-form.stx rand.stx rand.sig)
    (case-signature-specs rand.sig
      ((single-value)
       => (lambda (rand.ots)
	    (%single-value-operand-signature->application-signature input-form.stx rand.stx rand.sig rand.ots)))

      (<no-return>
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand is typed as not returning")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (make-type-signature/single-top))
	   ((strict-r6rs)
	    (make-type-signature/single-top)))))

      (<list-of>
       ;;The operand expression returns an unspecified number of values of specified,
       ;;homogeneous, type.  We rely on the compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       => (lambda (rand.ots)
	    (%single-value-operand-signature->application-signature input-form.stx rand.stx rand.sig
								    (list-of-type-spec.item-ots rand.ots))))

      (<list>
       ;;The  operand  expression   returns  an  unspecified  number   of  values  of
       ;;unspecified type.   We relay  on the  automatically generated  validation to
       ;;check at run-time if the expression returns a single value.
       (make-type-signature/single-top))

      (else
       ;;The operand expression returns zero, two or more values.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand returns multiple values")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (make-type-signature/single-top))
	   ((strict-r6rs)
	    (make-type-signature/single-top)))))))

  (define (%single-value-operand-signature->application-signature input-form.stx rand.stx rand.sig rand.ots)
    (make-type-signature/single-value
     (cond ((list-of-type-spec? rand.ots)
	    (list-of-type-spec.item-ots rand.ots))

	   ((list-type-spec? rand.ots)
	    (car (list-type-spec.item-ots* rand.ots)))

	   ((pair-of-type-spec? rand.ots)
	    (pair-of-type-spec.item-ots rand.ots))

	   ((pair-type-spec? rand.ots)
	    (pair-type-spec.car-ots rand.ots))

	   ((or (<nelist>-ots? rand.ots)
		(<pair>-ots?   rand.ots))
	    (<top>-ots))

	   ((or (object-type-spec.compatible-super-and-sub? (<nelist>-ots) rand.ots)
		(object-type-spec.compatible-super-and-sub? (<pair>-ots)   rand.ots))
	    (<top>-ots))

	   (else
	    (let ((common (lambda ()
			    (condition
			      (make-who-condition __module_who__)
			      (make-message-condition
			       "expand-time mismatch between closure object's arguments signatures and operands signature")
			      (make-syntax-violation input-form.stx rand.stx)
			      (make-application-operator-expression-condition (%core-prim-id))
			      (make-application-operands-expressions-condition (list rand.stx))
			      (make-application-argument-type-name-condition (<pair>-type-id))
			      (make-application-operand-signature-condition rand.sig)))))
	      (case-expander-language
		((typed)
		 (raise			(condition (make-expand-time-type-signature-violation) (common))))
		((default)
		 (raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
		 (<top>-ots))
		((strict-r6rs)
		 (<top>-ots))))))))

  #| end of module: CHI-CAR-APPLICATION |# )


(module (chi-cdr-application)
  ;;The input form has the syntax:
  ;;
  ;;   (cdr ?rand ...)
  ;;
  ;;and RANDS.STX is the syntax object:
  ;;
  ;;   #'(?rand ...)
  ;;
  ;;which holds  a proper  list of  expressions.  The application  of CDR  is special
  ;;because we want  to extract the type of  the returned value from the  type of the
  ;;operand.
  ;;
  (import CLOSURE-APPLICATION-ERRORS)
  (define-module-who chi-cdr-application)

  (define (%core-prim-id)
    (core-prim-id 'cdr))

  (define (chi-cdr-application input-form.stx lexenv.run lexenv.expand rands.stx)
    (syntax-match rands.stx ()
      ((?rand)
       (let* ((rand.psi		(chi-expr ?rand lexenv.run lexenv.expand))
	      (rand.core	(psi.core-expr rand.psi))
	      (rand.sig		(psi.retvals-signature rand.psi))
	      (application.sig	(%operand-signature->application-signature input-form.stx ?rand rand.sig)))
	 (make-psi input-form.stx
	   (build-application (syntax-annotation input-form.stx)
	       (build-primref no-source 'cdr)
	     (list rand.core))
	   application.sig)))

      (()
       (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
	 (%core-prim-id) '() 1 0))

      ((?rand0 ?rand1 ?rand* ...)
       (let ((rand*.stx (cons* ?rand0 ?rand1 ?rand*)))
	 (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
	   (%core-prim-id) rand*.stx 1 (length rand*.stx))))
      ))

  (define* ({%operand-signature->application-signature type-signature?} input-form.stx rand.stx rand.sig)
    (case-signature-specs rand.sig
      ((single-value)
       => (lambda (rand.ots)
	    (%single-value-operand-signature->application-signature input-form.stx rand.stx rand.sig rand.ots)))

      (<no-return>
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand is typed as not returning")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (make-type-signature/single-top))
	   ((strict-r6rs)
	    (make-type-signature/single-top)))))

      (<list-of>
       ;;The operand expression returns an unspecified number of values of specified,
       ;;homogeneous, type.  We rely on the compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       => (lambda (rand.ots)
	    (%single-value-operand-signature->application-signature input-form.stx rand.stx rand.sig
								    (list-of-type-spec.item-ots rand.ots))))

      (<list>
       ;;The  operand  expression   returns  an  unspecified  number   of  values  of
       ;;unspecified type.   We relay  on the  automatically generated  validation to
       ;;check at run-time if the expression returns a single value.
       (make-type-signature/single-top))

      (else
       ;;The operand expression returns zero, two or more values.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand returns multiple values")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (make-type-signature/single-top))
	   ((strict-r6rs)
	    (make-type-signature/single-top)))))))

  (define* ({%single-value-operand-signature->application-signature type-signature?} input-form.stx rand.stx rand.sig rand.ots)
    (make-type-signature/single-value
     (cond ((list-of-type-spec? rand.ots)
	    rand.ots)

	   ((list-type-spec? rand.ots)
	    (let ((cdr-ots* (cdr (list-type-spec.item-ots* rand.ots))))
	      (if (null? cdr-ots*)
		  (<null>-ots)
		(make-list-type-spec cdr-ots*))))

	   ((pair-of-type-spec? rand.ots)
	    (pair-of-type-spec.item-ots rand.ots))

	   ((pair-type-spec? rand.ots)
	    (pair-type-spec.cdr-ots rand.ots))

	   ((or (<nelist>-ots? rand.ots)
		(<pair>-ots?   rand.ots))
	    (<top>-ots))

	   ((or (object-type-spec.compatible-super-and-sub? (<nelist>-ots) rand.ots)
		(object-type-spec.compatible-super-and-sub? (<pair>-ots)   rand.ots))
	    (<top>-ots))

	   (else
	    (let ((common (lambda ()
			    (condition
			      (make-who-condition __module_who__)
			      (make-message-condition
			       "expand-time mismatch between closure object's arguments signatures and operands signature")
			      (make-syntax-violation input-form.stx rand.stx)
			      (make-application-operator-expression-condition (%core-prim-id))
			      (make-application-operands-expressions-condition (list rand.stx))
			      (make-application-argument-type-name-condition (<pair>-type-id))
			      (make-application-operand-signature-condition rand.sig)))))
	      (case-expander-language
		((typed)
		 (raise			(condition (make-expand-time-type-signature-violation) (common))))
		((default)
		 (raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
		 (<top>-ots))
		((strict-r6rs)
		 (<top>-ots))))))))

  #| end of module: CHI-CDR-APPLICATION |# )


(module (chi-vector-application chi-nevector-constructor-application)
  ;;The input form has the syntax:
  ;;
  ;;   (vector ?rand ...)
  ;;
  ;;and RANDS.STX is the syntax object:
  ;;
  ;;   #'(?rand ...)
  ;;
  ;;which holds a  proper list of expressions.  The application  of VECTOR is special
  ;;because  we  want  the  expression  to  return  a  type  signature  describing  a
  ;;"<vector-type-spec>".
  ;;
  (import CLOSURE-APPLICATION-ERRORS)

  (define-module-who chi-list-application)

  (define (chi-nevector-constructor-application input-form.stx lexenv.run lexenv.expand rands.stx)
    (syntax-match rands.stx ()
      (()
       (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
	 (core-prim-id '<nevector>-constructor) '() 1 0))

      ((?rand ?rand* ...)
       (chi-vector-application input-form.stx lexenv.run lexenv.expand rands.stx '<nevector>-constructor))))

  (define (chi-vector-application input-form.stx lexenv.run lexenv.expand rands.stx prim-name)
    (syntax-match rands.stx ()
      (()
       ;;No arguments.  Just return the empty vector.
       (make-psi input-form.stx
	 (build-data no-source '#())
	 (make-type-signature/single-value (<empty-vector>-ots))))

      ((?rand ?rand* ...)
       ;;Two or more values.
       (let* ((rand*.stx  (cons ?rand ?rand*))
	      (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	      (rand*.core (map psi.core-expr rand*.psi))
	      (rand*.sig  (map psi.retvals-signature rand*.psi)))
	 (let ((application.sig (%operand-signatures->application-signature input-form.stx rand*.stx rand*.sig)))
	   (make-psi input-form.stx
	     (build-application (syntax-annotation input-form.stx)
		 (build-primref no-source prim-name)
	       rand*.core)
	     application.sig))))
      ))

  (define (%operand-signatures->application-signature input-form.stx rand*.stx rand*.sig)
    (make-type-signature/single-value
     (make-vector-type-spec
      (map (lambda (rand.stx rand.sig)
	     (%single-operand-signature->ots input-form.stx rand.stx rand.sig))
	rand*.stx rand*.sig))))

  (define* ({%single-operand-signature->ots object-type-spec?} input-form.stx rand.stx rand.sig)
    (case-signature-specs rand.sig
      ((single-value)
       => (lambda (rand.ots)
	    rand.ots))

      (<no-return>
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand is typed as not returning")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (<top>-ots))
	   ((strict-r6rs)
	    (<top>-ots)))))

      (<list-of>
       ;;The operand expression returns an unspecified number of values of specified,
       ;;homogeneous, type.  We rely on the compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       => (lambda (rand.ots)
	    (list-of-type-spec.item-ots rand.ots)))

      (<list>
       ;;The  operand  expression   returns  an  unspecified  number   of  values  of
       ;;unspecified type.   We relay  on the  automatically generated  validation to
       ;;check at run-time if the expression returns a single value.
       (<top>-ots))

      (else
       ;;The operand expression returns zero, two or more values.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand returns multiple values")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (<top>-ots))
	   ((strict-r6rs)
	    (<top>-ots)))))))

  #| end of module: CHI-VECTOR-APPLICATION |# )


(module (chi-condition-application)
  ;;The input form has the syntax:
  ;;
  ;;   (condition ?rand ...)
  ;;
  ;;and RANDS.STX is the syntax object:
  ;;
  ;;   #'(?rand ...)
  ;;
  ;;which  holds a  proper  list of  expressions.  The  application  of CONDITION  is
  ;;special because  we want the expression  to return a type  signature describing a
  ;;"<compound-condition-object-type>".
  ;;
  ;;We need to verify that the operands expressions in RANDS.STX are either sub-types
  ;;of "&condition", or instances  of type "<compound-condition-object-type>".  Let's
  ;;remember that the type hierarchy for condition objects is this:
  ;;
  ;;   <condition> --> <compound-condition> --> <compound-condition-object-type>
  ;;        |
  ;;         --------> &condition --> &who
  ;;                       |
  ;;                       |--------> &message
  ;;                       |
  ;;                        --------> ... all the condition types ...
  ;;
  (define-module-who chi-condition-application)

  (define (chi-condition-application input-form.stx lexenv.run lexenv.expand rands.stx)
    (syntax-match rands.stx ()
      (()
       ;;No arguments.  Just evaluate "(condition)".
       (make-psi input-form.stx
	 (build-application (syntax-annotation input-form.stx)
	     (build-primref no-source 'condition)
	   '())
	 (make-type-signature/single-value (make-compound-condition-type-spec '()))))

      ((?rand ?rand* ...)
       ;;Two or more values.
       (let* ((rand*.stx  (cons ?rand ?rand*))
	      (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	      (rand*.core (map psi.core-expr rand*.psi))
	      (rand*.sig  (map psi.retvals-signature rand*.psi)))
	 (let ((application.sig (%operand-signatures->application-signature input-form.stx rand*.stx rand*.sig)))
	   (make-psi input-form.stx
	     (build-application (syntax-annotation input-form.stx)
		 (build-primref no-source 'condition)
	       rand*.core)
	     application.sig))))
      ))

  (define (%operand-signatures->application-signature input-form.stx rand*.stx rand*.sig)
    (make-type-signature/single-value
     (call/cc
	 (lambda (escape)
	   (define specs
	     (fold-right (lambda (rand.stx rand.sig application.specs)
			   (%single-operand-signature->application-signature input-form.stx rand.stx rand.sig application.specs
									     (lambda ()
									       (escape (<compound-condition>-ots)))))
	       '() rand*.stx rand*.sig))
	   ;;We want:
	   ;;
	   ;;   (type-of (make-who-condition 'ciao))
	   ;;   => #[type-signature (&who)]
	   ;;
	   ;;   (type-of (condition (make-who-condition 'ciao)))
	   ;;   => #[type-signature (&who)]
	   ;;
	   (if (list-of-single-item? specs)
	       (car specs)
	     (make-compound-condition-type-spec specs))))))

  (define (%single-operand-signature->application-signature input-form.stx rand.stx rand.sig application.specs unspecified-kont)
    ;;The argument UNSPECIFIED-KONT  is a continuation thunk to be  called when it is
    ;;not  possible to  determine the  type of  an operand;  in this  case the  whole
    ;;CONDITION application signature must  represent a single "<compound-condition>"
    ;;value.
    ;;
    (case-signature-specs rand.sig
      ((single-value)
       => (lambda (rand.ots)
	    (%process-single-value-operand input-form.stx rand.stx rand.sig rand.ots application.specs unspecified-kont)))

      (<no-return>
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand is typed as not returning")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (unspecified-kont))
	   ((strict-r6rs)
	    (unspecified-kont)))))

      (<list-of>
       ;;The operand expression returns an unspecified number of values of specified,
       ;;homogeneous, type.  We rely on the compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       => (lambda (rand.ots)
	    (let ((item.ots (list-of-type-spec.item-ots rand.ots)))
	      (%process-single-value-operand input-form.stx rand.stx rand.sig item.ots application.specs unspecified-kont))))

      (<list>
       ;;The  operand  expression   returns  an  unspecified  number   of  values  of
       ;;unspecified type.  We rely on the  compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       (unspecified-kont))

      (else
       ;;The operand expression returns zero, two or more values.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "expression used as application operand returns multiple values")
			 (make-syntax-violation input-form.stx rand.stx)
			 (make-application-operand-signature-condition rand.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (unspecified-kont))
	   ((strict-r6rs)
	    (unspecified-kont)))))))

  (define (%process-single-value-operand input-form.stx rand.stx rand.sig rand.ots application.specs unspecified-kont)
    (cond ((simple-condition-object-type-spec? rand.ots)
	   (cons rand.ots application.specs))
	  ((compound-condition-type-spec? rand.ots)
	   (append (compound-condition-type-spec.component-ots* rand.ots)
		   application.specs))
	  ((and (union-type-spec? rand.ots)
		(for-all (lambda (item.ots)
			   (or (simple-condition-object-type-spec?	item.ots)
			       (compound-condition-type-spec?		item.ots)
			       (<compound-condition>-ots?		item.ots)
			       (<condition>-ots?			item.ots)))
		  (union-type-spec.component-ots* rand.ots)))
	   (unspecified-kont))
	  ((and (intersection-type-spec? rand.ots)
		(for-all (lambda (item.ots)
			   (or (simple-condition-object-type-spec?	item.ots)
			       (compound-condition-type-spec?		item.ots)
			       (<compound-condition>-ots?		item.ots)
			       (<condition>-ots?			item.ots)))
		  (intersection-type-spec.component-ots* rand.ots)))
	   (unspecified-kont))
	  ((or (<compound-condition>-ots? rand.ots)
	       (<condition>-ots?          rand.ots))
	   (unspecified-kont))
	  (else
	   (let ((common (lambda ()
			   (condition
			     (make-who-condition __module_who__)
			     (make-message-condition
			      "expected condition object-type specification as component of compound condition object-type")
			     (make-syntax-violation input-form.stx rand.stx)
			     (make-application-operand-signature-condition rand.sig)))))
	     (case-expander-language
	       ((typed)
		(raise			(condition (make-expand-time-type-signature-violation) (common))))
	       ((default)
		(raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
		(unspecified-kont))
	       ((strict-r6rs)
		(unspecified-kont)))))))

  #| end of module: CHI-CONDITION-APPLICATION |# )


(module (chi-call-with-values-application)
  (import CLOSURE-APPLICATION-ERRORS)
  (define-module-who chi-call-with-values-application)

  (define (%core-prim-id)
    (core-prim-id 'call-with-values))

  (define* (chi-call-with-values-application input-form.stx lexenv.run lexenv.expand rands.stx)
    ;;The input form has the syntax:
    ;;
    ;;   (call-with-values ?rand ...)
    ;;
    ;;and RANDS.STX is the syntax object:
    ;;
    ;;   #'(?rand ...)
    ;;
    ;;which holds a proper list  of expressions.  The application of CALL-WITH-VALUES
    ;;is  special; CALL-WITH-VALUES  accepts two  arguments: a  producer thunk  and a
    ;;consumer closure object.  We want to  generate a core expression having as type
    ;;signature the type signature of the application of the consumer.
    ;;
    (syntax-match rands.stx ()
      ((?producer ?consumer)
       (let* ((producer.psi	(chi-expr ?producer lexenv.run lexenv.expand))
	      (consumer.psi	(chi-expr ?consumer lexenv.run lexenv.expand))
	      (producer.core	(psi.core-expr producer.psi))
	      (consumer.core	(psi.core-expr consumer.psi)))
	 (define (%handle-producer-error message)
	   (%error (lambda ()
		     (condition
		       (make-who-condition __module_who__)
		       (make-message-condition message)
		       (make-syntax-violation input-form.stx ?producer)
		       (make-application-operand-signature-condition (psi.retvals-signature producer.psi))))))
	 (define (%handle-consumer-error message)
	   (%error (lambda ()
		     (condition
		       (make-who-condition __module_who__)
		       (make-message-condition message)
		       (make-syntax-violation input-form.stx ?consumer)
		       (make-application-operand-signature-condition (psi.retvals-signature consumer.psi))))))
	 (%validate-producer input-form.stx ?producer producer.psi %handle-producer-error)
	 (let ((application.sig (%validate-consumer-return-application-signature input-form.stx ?consumer consumer.psi
										 %handle-consumer-error)))
	   (make-psi input-form.stx
	     (build-application (syntax-annotation input-form.stx)
		 (build-primref no-source 'call-with-values)
	       (list producer.core consumer.core))
	     application.sig))))

      (()
       (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx (%core-prim-id) '() 2 0))

      ((?rand)
       (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx (%core-prim-id) (list ?rand) 2 1))

      ((?rand0 ?rand1 ?rand2 ?rand* ...)
       (let ((rand*.stx (cons* ?rand0 ?rand1 ?rand2 ?rand*)))
	 (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx (%core-prim-id) rand*.stx 2 (length rand*.stx))))
      ))

;;; --------------------------------------------------------------------

  (define (%validate-producer input-form.stx producer.stx producer.psi handle-producer-error)
    (define producer.sig (psi.retvals-signature producer.psi))
    (case-signature-specs producer.sig
      ((single-value)
       => (lambda (producer.ots)
	    (%process-single-value-producer input-form.stx producer.stx producer.psi producer.ots
					    handle-producer-error)))

      (<no-return>
       (handle-producer-error "expression used as producer operand is typed as not returning"))

      (<list-of>
       ;;The operand expression returns an unspecified number of values of specified,
       ;;homogeneous, type.  We rely on the compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       => (lambda (rand.ots)
	    (%process-single-value-producer input-form.stx producer.stx producer.psi (list-of-type-spec.item-ots rand.ots)
					    handle-producer-error)))

      (<list>
       ;;The  operand  expression   returns  an  unspecified  number   of  values  of
       ;;unspecified type.  We rely on the  compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       (void))

      (else
       ;;The operand expression returns zero, two or more values.
       (handle-producer-error "expression used as producer operand returns multiple values"))))

  (define (%validate-consumer-return-application-signature input-form.stx consumer.stx consumer.psi handle-consumer-error)
    (define consumer.sig (psi.retvals-signature consumer.psi))
    (case-signature-specs consumer.sig
      ((single-value)
       => (lambda (consumer.ots)
	    (%process-single-value-consumer input-form.stx consumer.stx consumer.psi consumer.ots
					    handle-consumer-error)))

      (<no-return>
       (handle-consumer-error "expression used as consumer operand is typed as not returning")
       (make-type-signature/fully-unspecified))

      (<list-of>
       ;;The operand expression returns an unspecified number of values of specified,
       ;;homogeneous, type.  We rely on the compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       => (lambda (rand.ots)
	    (%process-single-value-consumer input-form.stx consumer.stx consumer.psi (list-of-type-spec.item-ots rand.ots)
					    handle-consumer-error)))

      (<list>
       ;;The  operand  expression   returns  an  unspecified  number   of  values  of
       ;;unspecified type.  We rely on the  compiler to generate code that checks, at
       ;;run-time, if this operand returns a single value.
       (make-type-signature/fully-unspecified))

      (else
       ;;The operand expression returns zero, two or more values.
       (handle-consumer-error "expression used as consumer operand returns multiple values")
       (make-type-signature/fully-unspecified))))

;;; --------------------------------------------------------------------

  (define (%process-single-value-producer input-form.stx producer.stx producer.psi producer.ots handle-producer-error)
    (cond ((closure-type-spec? producer.ots)
	   ;;Is this a thunk?
	   (unless (closure-type-spec.thunk? producer.ots)
	     (handle-producer-error "expression used as producer returns a procedure, but not a thunk")))
	  ((<procedure>-ots? producer.ots)
	   ;;Good.
	   (void))
	  (else
	   (handle-producer-error "expression used as producer does not return a procedure"))))

  (define (%process-single-value-consumer input-form.stx consumer.stx consumer.psi consumer.ots handle-consumer-error)
    ;;Return a "<type-signature>"  representing the signature of  the values returned
    ;;by the consumer application.
    ;;
    (cond ((closure-type-spec? consumer.ots)
	   (case-lambda-signature.retvals (closure-type-spec.signature consumer.ots)))
	  ((<procedure>-ots? consumer.ots)
	   ;;Good.
	   (make-type-signature/fully-unspecified))
	  (else
	   (handle-consumer-error "expression used as consumer does not return a procedure")
	   (make-type-signature/fully-unspecified))))

;;; --------------------------------------------------------------------

  (define (%error common)
    (case-expander-language
      ((typed)
       (raise			(condition (make-expand-time-type-signature-violation)	(common))))
      ((default)
       (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common))))
      ((strict-r6rs)
       (void))))

  #| end of module: CHI-CALL-WITH-VALUES-APPLICATION |# )


(define (chi-apply-application input-form.stx lexenv.run lexenv.expand
			       rator.stx rand*.stx)
  ;;The input form is a call to the core primitive APPLY:
  ;;
  ;;   (apply ?rator ?rand ...)
  ;;
  ;;the  argument RATOR.STX  is #'?RATOR  and the  argument RAND*.STX  is the  syntax
  ;;object representing the list of operands:
  ;;
  ;;   #'(?rand ...)
  ;;
  (let* ((rator.psi (chi-expr rator.stx lexenv.run lexenv.expand))
	 (rator.sig (psi.retvals-signature rator.psi)))

    (define (%build-default-application)
      (let* ((rator.core (psi.core-expr rator.psi))
	     (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	     (rand*.core (map psi.core-expr rand*.psi)))
	(make-psi input-form.stx
	  (build-application (syntax-annotation input-form.stx)
	      (build-primref no-source 'apply)
	    (cons rator.core rand*.core)))))

    (case-signature-specs rator.sig
      ((<top>)
       ;;The  operator  expression  correctly  returns  a single  value;  it  is  not
       ;;specified to return a procedure, but  the return value is compatible because
       ;;"<top>" is the parent of "<procedure>".
       ;;
       ;;Return a  procedure application and  we will  see at run-time  what happens;
       ;;this is standard Scheme behaviour.
       (%build-default-application))

      ((<procedure>)
       ;;The operator  expression returns  a single  value, marked  as "<procedure>".
       ;;Good.  There are  no further validations possible at  expand-time.  There is
       ;;no optimisation possible.
       (%build-default-application))

      ((<closure>)
       ;;The operator expression returns a closure object.  Good.
       ;;
       ;;FIXME Further signature  validations are possible by inspecting  the type of
       ;;the last operand, which must be  a "<list>" or its sub-type; implement them.
       ;;(Marco Maggi; Wed Mar 9, 2016)
       => (lambda (rator.ots)
	    (let* ((rator.core (psi.core-expr rator.psi))
		   (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
		   (rand*.core (map psi.core-expr rand*.psi)))
	      (make-psi input-form.stx
		(build-application (syntax-annotation input-form.stx)
		    (build-primref no-source 'apply)
		  (cons rator.core rand*.core))
		(case-signature-specs (psi.retvals-signature rator.psi)
		  ((<closure>)
		   ;;The operator  is a "<closure-type-spec>", good.   We extract the
		   ;;generic retvals signature from the value.
		   => (lambda (rator.ots)
			(case-lambda-signature.retvals (closure-type-spec.signature rator.ots))))

		  ((<procedure>)
		   ;;The operator is an untyped procedure, good.
		   (make-type-signature/fully-unspecified))

		  ((single-value)
		   ;;The operator is a single value, but not a procedure.
		   (let ((common (lambda ()
				   (condition
				     (make-who-condition __module_who__)
				     (make-message-condition "expression in operator application position is not a closure type")
				     (make-syntax-violation input-form.stx          (psi.input-form        rator.psi))
				     (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
		     (case-expander-language
		       ((typed)
			(raise			(condition (make-expand-time-type-signature-violation) (common))))
		       ((default)
			(raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
			(make-type-signature/fully-unspecified))
		       ((strict-r6rs)
			(make-type-signature/fully-unspecified)))))

		  (<no-return>
		   ;;The  operator is  marked to  no-return:  it means  it raises  an
		   ;;exception.  For example:
		   ;;
		   ;;   (apply (error #f "bad") 1 '(2))
		   ;;
		   (let ((common (lambda ()
				   (condition
				     (make-who-condition __module_who__)
				     (make-message-condition
				      "expression used as operator in procedure application is typed as not returning")
				     (make-syntax-violation input-form.stx          (psi.input-form        rator.psi))
				     (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
		     (case-expander-language
		       ((typed)
			(raise			(condition (make-expand-time-type-signature-violation) (common))))
		       ((default)
			(raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
			(make-type-signature/fully-unspecified))
		       ((strict-r6rs)
			(make-type-signature/fully-unspecified)))))

		  ((unspecified-values)
		   ;;The operator expression returns fully unspecified values.
		   (make-type-signature/fully-unspecified))

		  (else
		   ;;The operator returns zero, two or more return values.
		   (let ((common (lambda ()
				   (condition
				     (make-who-condition __module_who__)
				     (make-message-condition
				      "expression used as operator in procedure application returns zero, two or more values")
				     (make-syntax-violation input-form.stx          (psi.input-form        rator.psi))
				     (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
		     (case-expander-language
		       ((typed)
			(raise			(condition (make-expand-time-type-signature-violation) (common))))
		       ((default)
			(raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
			(make-type-signature/fully-unspecified))
		       ((strict-r6rs)
			(make-type-signature/fully-unspecified))))))))))

      ((single-value)
       ;;The operator expression correctly returns a  single value, but such value is
       ;;not marked as procedure.  Bad.
       => (lambda (rator.ots)
	    (let ((common (lambda ()
			    (condition
			      (make-who-condition 'apply)
			      (make-message-condition "expression used as operator in APPLY application evaluates to a non-procedure value")
			      (make-syntax-violation input-form.stx (psi.input-form rator.psi))
			      (make-application-operator-signature-condition rator.ots)))))
	      (case-expander-language
		((typed)
		 (raise			(condition (make-expand-time-type-signature-violation) (common))))
		;;According to the standard we must insert a normal rator application
		;;and raise an exception at run-time.  Raise a warning, then do it.
		((default)
		 (raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
		 (%build-default-application))
		((strict-r6rs)
		 (%build-default-application))))))

      ((unspecified-values)
       ;;The  operator  expression  returns  an  unspecified  number  of  values,  of
       ;;unspecified type.  Return a normal procedure  application and we will see at
       ;;run-time what happens; this is standard Scheme behaviour.
       (%build-default-application))

      (<no-return>
       ;;The operator expression  does not return.  This is not  strictly wrong.  For
       ;;example:
       ;;
       ;;   (apply (error #f "bad value") ?rand ...)
       ;;
       (let ((common (lambda ()
		       (condition
			 (make-who-condition 'apply)
			 (make-message-condition "expression used as operator in APPLY application does not return")
			 (make-syntax-violation input-form.stx rator.stx)
			 (make-application-operator-signature-condition rator.sig)))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (%build-default-application))
	   ((strict-r6rs)
	    (%build-default-application)))))

      (else
       ;;The rator is declared to evaluate to zero, two or more values.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition 'apply)
			 (make-message-condition "expression used as operator in APPLY application returns multiple values")
			 (make-syntax-violation input-form.stx (psi.input-form rator.psi))
			 (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
	 (case-expander-language
	   ((typed)
	    ;;Multiple values are invalid in call context: raise an exception.
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ;;According to the standard we must  insert a normal rator application and
	   ;;raise an exception at run-time.  Raise a warning, then do it.
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (%build-default-application))
	   ((strict-r6rs)
	    (%build-default-application))))))))


;;;; chi procedures: operator application processing, operator already expanded

(define* (chi-application/psi-rator input-form.stx lexenv.run lexenv.expand
				    {rator.psi psi?} rand*.stx)
  ;;Expand an  operator application form;  it is  called when INPUT-FORM.STX  has the
  ;;format:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;and ?RATOR is  neither a macro keyword identifier, nor  a VALUES application, nor
  ;;an APPLY application, nor a SPLICE-FIRST-EXPAND syntax.  For example it is called
  ;;when INPUT-FORM.STX is:
  ;;
  ;;   (?core-prim ?rand ...)
  ;;   ((?sub-rator ?sub-rand ...) ?rand ...)
  ;;
  ;;We call this function when the operator has already been expanded.
  ;;

  (define (%build-default-application)
    ;;Build a core language expression to apply  the rator to the rands; return a PSI
    ;;struct.  This  is an  application form in  standard (untyped)  Scheme language.
    ;;Here we know nothing about the values returned by the application.
    ;;
    (let* ((rator.core (psi.core-expr rator.psi))
	   (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	   (rand*.core (map psi.core-expr rand*.psi)))
      (make-psi input-form.stx
	(build-application (syntax-annotation input-form.stx)
	    rator.core
	  rand*.core))))

  (case-signature-specs (psi.retvals-signature rator.psi)
    ((<top>)
     ;;The operator expression correctly returns a  single value; it is not specified
     ;;to return a  procedure, but the return value is  compatible because "<top>" is
     ;;the parent of "<procedure>".
     ;;
     ;;Return a procedure application and we  will see at run-time what happens; this
     ;;is standard Scheme behaviour.
     (%build-default-application))

    ((<procedure>)
     ;;The  operator expression  returns  a single  value,  marked as  "<procedure>".
     ;;Good.  There are no further validations  possible at expand-time.  There is no
     ;;optimisation possible.
     (%build-default-application))

    ((<closure>)
     ;;The operator  expression returns a  closure object.  Good.   Further signature
     ;;validations are possible.
     => (lambda (rator.ots)
	  (let ((rand*.psi (chi-expr* rand*.stx lexenv.run lexenv.expand)))
	    (chi-closure-object-application input-form.stx lexenv.run lexenv.expand
					    rator.psi rator.ots rand*.psi))))

    ((single-value)
     ;;The operator  expression correctly returns a  single value, but such  value is
     ;;not marked as procedure.  Bad.
     => (lambda (rator.ots)
	  (let ((common (lambda ()
			  (condition
			    (make-who-condition __who__)
			    (make-message-condition "expression used as operator in application form evaluates to a non-procedure value")
			    (make-syntax-violation input-form.stx (psi.input-form rator.psi))
			    (make-application-operator-signature-condition rator.ots)))))
	    (case-expander-language
	      ((typed)
	       (raise			(condition (make-expand-time-type-signature-violation) (common))))
	      ;;According to the  standard we must insert a  normal rator application
	      ;;and raise an exception at run-time.  Raise a warning, then do it.
	      ((default)
	       (raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
	       (%build-default-application))
	      ((strict-r6rs)
	       (%build-default-application))))))

    ((unspecified-values)
     ;;The  operator  expression   returns  an  unspecified  number   of  values,  of
     ;;unspecified type.   Return a normal procedure  application and we will  see at
     ;;run-time what happens; this is standard Scheme behaviour.
     (%build-default-application))

    (<no-return>
     ;;The operator expression  does not return.  This is not  strictly wrong.  For
     ;;example:
     ;;
     ;;   ((error #f "bad value") ?rand ...)
     ;;
     (let ((common (lambda ()
		     (condition
		       (make-who-condition __who__)
		       (make-message-condition "expression used as operator in application form does not return")
		       (make-syntax-violation input-form.stx (psi.input-form rator.psi))
		       (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
       (case-expander-language
	 ((typed)
	  (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	 ((default)
	  (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	  (%build-default-application))
	 ((strict-r6rs)
	  (%build-default-application)))))

    (else
     ;;The rator is declared to evaluate to zero, two or more values.
     (let ((common (lambda ()
		     (condition
		       (make-who-condition __who__)
		       (make-message-condition "expression used as operator in application form returns multiple values")
		       (make-syntax-violation input-form.stx (psi.input-form rator.psi))
		       (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
       (case-expander-language
	 ((typed)
	  ;;Multiple values are invalid in call context: raise an exception.
	  (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	 ;;According to  the standard we must  insert a normal rator  application and
	 ;;raise an exception at run-time.  Raise a warning, then do it.
	 ((default)
	  (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	  (%build-default-application))
	 ((strict-r6rs)
	  (%build-default-application)))))))


;;;; chi procedures: operator application processing, first operand already expanded

(define* (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					    rator.stx {first-rand.psi psi?} other-rand*.stx)
  ;;This is an entry point, not a sub-routine of CHI-APPLICATION.  Expand an operator
  ;;application form; it is called when the application to process has the format:
  ;;
  ;;   (?rator ?first-rand ?other-rand ...)
  ;;
  ;;and ?FIRST-RAND has already been expanded.  Here we know that the input form is a
  ;;special syntax like IS-A?, SLOT-REF, SLOT-SET!, METHOD-CALL; so the operator will
  ;;expand into a predicate, accessor, mutator or method.
  ;;
  (let* ((rator.psi (while-not-expanding-application-first-subform
		     (chi-expr rator.stx lexenv.run lexenv.expand)))
	 (rator.sig (psi.retvals-signature rator.psi)))

    (define (%build-default-application)
      ;;Build a core  language expression to apply  the rator to the  rands; return a
      ;;PSI  struct.   This is  an  application  form  in standard  (untyped)  Scheme
      ;;language.  Here we know nothing about the values returned by the application.
      ;;
      (let* ((rator.core       (psi.core-expr rator.psi))
	     (first-rand.core  (psi.core-expr first-rand.psi))
	     (other-rand*.psi  (chi-expr* other-rand*.stx lexenv.run lexenv.expand))
	     (other-rand*.core (map psi.core-expr other-rand*.psi)))
	(make-psi input-form.stx
	  (build-application (syntax-annotation input-form.stx)
	      rator.core
	    (cons first-rand.core other-rand*.core)))))

    (case-signature-specs rator.sig
      ((<top>)
       ;;The  operator  expression  correctly  returns  a single  value;  it  is  not
       ;;specified to return a procedure, but  the return value is compatible because
       ;;"<top>" is the parent of "<procedure>".
       ;;
       ;;Return a  procedure application and  we will  see at run-time  what happens;
       ;;this is standard Scheme behaviour.
       (%build-default-application))

      ((<procedure>)
       ;;The operator  expression returns  a single  value, marked  as "<procedure>".
       ;;Good.  There are  no further validations possible at  expand-time.  There is
       ;;no optimisation possible.
       (%build-default-application))

      ((<closure>)
       ;;The operator expression returns a  closure object.  Good.  Further signature
       ;;validations are possible.
       => (lambda (rator.ots)
	    (let* ((other-rand*.psi (chi-expr* other-rand*.stx lexenv.run lexenv.expand))
		   (rand*.psi       (cons first-rand.psi other-rand*.psi)))
	      (chi-closure-object-application input-form.stx lexenv.run lexenv.expand
					      rator.psi rator.ots rand*.psi))))

      ((single-value)
       ;;The operator expression correctly returns a  single value, but such value is
       ;;not marked as procedure.  Bad.
       => (lambda (rator.ots)
	    (let ((common (lambda ()
			    (condition
			      (make-who-condition __who__)
			      (make-message-condition "expression used as operator in application form evaluates to a non-procedure value")
			      (make-syntax-violation input-form.stx (psi.input-form rator.psi))
			      (make-application-operator-signature-condition rator.ots)))))
	      (case-expander-language
		((typed)
		 (raise			(condition (make-expand-time-type-signature-violation) (common))))
		;;According to the standard we must insert a normal rator application
		;;and raise an exception at run-time.  Raise a warning, then do it.
		((default)
		 (raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
		 (%build-default-application))
		((strict-r6rs)
		 (%build-default-application))))))

      ((unspecified-values)
       ;;The  operator  expression   returns  an  unspecified  number   of  values,  of
       ;;unspecified type.   Return a normal procedure  application and we will  see at
       ;;run-time what happens; this is standard Scheme behaviour.
       (%build-default-application))

      (<no-return>
       ;;The operator  expression does not  return.  This  is not strictly  wrong.  For
       ;;example:
       ;;
       ;;   ((error #f "bad value") ?rand ...)
       ;;
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __who__)
			 (make-message-condition "expression used as operator in application form does not return")
			 (make-syntax-violation input-form.stx (psi.input-form rator.psi))
			 (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
	 (case-expander-language
	   ((typed)
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (%build-default-application))
	   ((strict-r6rs)
	    (%build-default-application)))))

      (else
       ;;The rator is declared to evaluate to zero, two or more values.
       (let ((common (lambda ()
		       (condition
			 (make-who-condition __who__)
			 (make-message-condition "expression used as operator in application form returns multiple values")
			 (make-syntax-violation input-form.stx (psi.input-form rator.psi))
			 (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
	 (case-expander-language
	   ((typed)
	    ;;Multiple values are invalid in call context: raise an exception.
	    (raise		(condition (make-expand-time-type-signature-violation)	(common))))
	   ;;According to  the standard we must  insert a normal rator  application and
	   ;;raise an exception at run-time.  Raise a warning, then do it.
	   ((default)
	    (raise-continuable	(condition (make-expand-time-type-signature-warning)	(common)))
	    (%build-default-application))
	   ((strict-r6rs)
	    (%build-default-application))))))))


;;;; chi procedures: closure object application processing

(module (chi-closure-object-application)
  (import CLOSURE-APPLICATION-ERRORS)
  (define-module-who chi-closure-object-application)

  (define* (chi-closure-object-application input-form.stx lexenv.run lexenv.expand
					   {rator.psi psi?} {rator.ots closure-type-spec?}
					   rand*.psi)
    ;;Handle the  case of closure  object application to  a given tuple  of operands;
    ;;here we know that the application to process is:
    ;;
    ;;   (?rator ?rand ...)
    ;;
    ;;and ?RATOR will evaluate  to a closure object with type  RATOR.OTS, which is an
    ;;instance of "<closure-type-spec>".
    ;;
    (let ((rator.clambda-sig (closure-type-spec.signature rator.ots)))
      ;;RATOR.CLAMBDA-SIG is an instance of "<case-lambda-signature>".
      ;;
      ;;The number of operands must be in  the correct range of arguments accepted by
      ;;the closure object.
      (receive (minimum-arguments-count maximum-arguments-count)
	  (case-lambda-signature.min-and-max-argvals rator.clambda-sig)
	(let ((given-operands-count (length rand*.psi)))
	  (cond ((< maximum-arguments-count given-operands-count)
		 (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
		   (psi.input-form rator.psi) (map psi.input-form rand*.psi)
		   maximum-arguments-count given-operands-count))
		((> minimum-arguments-count given-operands-count)
		 (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
		   (psi.input-form rator.psi) (map psi.input-form rand*.psi)
		   minimum-arguments-count given-operands-count)))))
      ;;All the operands must return a single value.
      (let ((rands.sig (%validate-operands-for-single-return-value input-form.stx rand*.psi)))
	;;Search for a clause whose signature matches the operands' types.
	(%match-case-lambda-signature-against-operands input-form.stx lexenv.run lexenv.expand
						       rator.psi rand*.psi
						       rator.clambda-sig rands.sig))))

;;; --------------------------------------------------------------------

  (module (%validate-operands-for-single-return-value)

    (define* (%validate-operands-for-single-return-value input-form.stx rand*.psi)
      ;;In the  context of  INPUT-FORM.STX the  RAND*.PSI argument is  a list  of psi
      ;;instances  representing  the  already  expanded operand  expressions  for  an
      ;;operator application.  Check that such expressions return a single value.
      ;;
      ;;Return an instance  of "<type-signature>" representing the  type signature of
      ;;the  operands.  Whenever  it is  not  possible to  determine the  type of  an
      ;;operand: the type defaults to "<top>".
      ;;
      (make-type-signature
       (map (lambda (rand.psi)
	      (define rand.sig
		(psi.retvals-signature rand.psi))
	      (define (%handle-error message rv)
		(%error (lambda ()
			  (condition
			    (make-who-condition __module_who__)
			    (make-message-condition message)
			    (make-syntax-violation input-form.stx (psi.input-form rand.psi))
			    (make-application-operand-signature-condition rand.sig)))
			rv))
	      (case-signature-specs rand.sig
		(<no-return>
		 ;;The operand expression will not return.
		 (if (options::warn-about-not-returning-expressions)
		     (%handle-error "expression used as operand in procedure application is typed as not returning"
				    (<top>-ots))
		   (<top>-ots)))
		((<void>)
		 ;;The expression is marked as returning void.
		 (%handle-error "expression used as operand in procedure application is typed as returning void"
				(<top>-ots)))
		((single-value)
		 ;;Single return value.  Good.
		 => (lambda (rand.ots) rand.ots))
		(<list-of>
		 ;;Unspecified number  of return values,  of known type.  We  let the
		 ;;compiler insert  the run-time  check for  the single  return value
		 ;;case.
		 => (lambda (rand.ots)
		      (list-of-type-spec.item-ots rand.ots)))
		(<list>
		 ;;Unspecified number of return values,  of unknown type.  We let the
		 ;;compiler insert  the run-time checks  for the single  return value
		 ;;case.
		 (<top>-ots))
		(else
		 ;;Zero, two or more return values.  Wrong.
		 (%handle-error "expression used as operand in procedure application returns zero, two or more values"
				(<top>-ots)))))
	 rand*.psi)))

    (define (%error common rv)
      (case-expander-language
	((typed)
	 (raise			(condition (make-expand-time-type-signature-violation) (common))))
	((default)
	 (raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
	 rv)
	((strict-r6rs)
	 rv)))

    #| end of module: %VALIDATE-OPERANDS-FOR-SINGLE-RETURN-VALUE |# )

;;; --------------------------------------------------------------------

  (module (%match-case-lambda-signature-against-operands)

    (define (%match-case-lambda-signature-against-operands input-form.stx lexenv.run lexenv.expand
							   rator.psi rand*.psi
							   rator.clambda-sig rands.sig)
      ;;Iterate, in  order, through  the "<lambda-signature>"  instances representing
      ;;the operator's clause signatures, looking  for the first that matches exactly
      ;;the operands' signature;  otherwise accumulate a list  of compatible clauses.
      ;;Build and return a psi instance representing the application form.
      ;;
      ;;The argument RATOR.PSI is the already expanded operator expression.
      ;;
      ;;The  argument  RAND*.PSI  is  a  proper  list  of  already  expanded  operand
      ;;expressions.
      ;;
      ;;The argument  RATOR.CLAMBDA-SIG is  an instance  of "<case-lambda-signature>"
      ;;representing the closure object's signatures.
      ;;
      ;;Assuming each  operand expression  in RAND*.PSI returns  a single  value: the
      ;;argument  RANDS.SIG is  an  instance of  "<type-signature>" representing  the
      ;;operands' types.
      ;;
      (let* ((selected-clause-signature* '())
	     (state (returnable
		      (fold-left (lambda (state clause-signature)
				   (let ((args.sig (lambda-signature.argvals clause-signature)))
				     (case (type-signature.match-arguments-against-operands args.sig rands.sig)
				       ((exact-match)
					(set! selected-clause-signature* (list clause-signature))
					(return 'exact-match))
				       ((possible-match)
					(set-cons! selected-clause-signature* clause-signature)
					'possible-match)
				       (else state))))
			'no-match (case-lambda-signature.clause-signature* rator.clambda-sig)))))
	(define (%default-core-expr)
	  (%build-default-application input-form.stx rator.psi rand*.psi selected-clause-signature*))
	(case state
	  ((exact-match)
	   ;;There is a clause that exactly  matches the operands.  When we are here:
	   ;;we know that SELECTED-CLAUSE-SIGNATURE* holds a single item.
	   (%chi-application-with-matching-signature input-form.stx lexenv.run lexenv.expand
							 rator.psi rand*.psi rands.sig
							 (lambda-signature.retvals (car selected-clause-signature*))))
	  ((possible-match)
	   ;;There is at least one clause with  a possible match.  It is not possible
	   ;;to fully  validate the  signatures at expand-time;  we rely  on run-time
	   ;;checking.
	   (%default-core-expr))
	  (else
	   ;;There are no matching clauses, not even possible matches.  Arguments and
	   ;;operands do *not* match at expand-time.
	   (assert (null? selected-clause-signature*))
	   (let ((make-arguments-signature* (lambda ()
					      (map lambda-signature.argvals
						(case-lambda-signature.clause-signature* rator.clambda-sig)))))
	     (case-expander-language
	       ((typed)
		(%error-mismatch-between-args-signature-and-operands-signature   input-form.stx (make-arguments-signature*) rands.sig))
	       ((default)
		(%warning-mismatch-between-args-signature-and-operands-signature input-form.stx (make-arguments-signature*) rands.sig)
		(%default-core-expr))
	       ((strict-r6rs)
		(%default-core-expr))))))))

    (define (%build-default-application input-form.stx rator.psi rand*.psi selected-clause-signature*)
      (%build-core-expression input-form.stx rator.psi rand*.psi
			      (cond ((list-of-single-item? selected-clause-signature*)
				     ;;There is a single compatible clause signature.
				     (lambda-signature.retvals (car selected-clause-signature*)))
				    ((pair? selected-clause-signature*)
				     ;;There    are   multiple    compatible   clause
				     ;;signatures.  Let's  compute the  union between
				     ;;the retvals type signatures.
				     (apply type-signature.union-same-number-of-operands
					    (lambda (message cnd)
					      (raise (condition (make-who-condition __module_who__)
								(make-message-condition message)
								(make-syntax-violation input-form.stx #f)
								cnd)))
					    (map lambda-signature.retvals selected-clause-signature*)))
				    (else
				     (make-type-signature/fully-unspecified)))))

    #| end of module: %MATCH-CASE-LAMBDA-SIGNATURE-AGAINST-OPERANDS |# )

;;; --------------------------------------------------------------------

  (module (%chi-application-with-matching-signature)

    (define* (%chi-application-with-matching-signature input-form.stx lexenv.run lexenv.expand
						       rator.psi rand*.psi
						       rands.sig {application-retvals.sig type-signature?})
      ;;We are applying an operator RATOR.PSI  to a tuple of operands RAND*.PSI.  The
      ;;operator is  a closure object.  One  of the closure's clauses  has arguments'
      ;;type  signatures  matching  the   operands'  type  signature.   The  matching
      ;;closure's    clause     has    return    values    with     type    signature
      ;;APPLICATION-RETVALS.SIG.
      ;;
      (let ((rator.stx (psi.input-form rator.psi)))
	(cond ((identifier? rator.stx)
	       (%chi-application-of-identifier-rator input-form.stx lexenv.run lexenv.expand
						     rator.psi rand*.psi rator.stx
						     rands.sig application-retvals.sig))
	      (else
	       ;;If we are here the rator  is a non-identifier expression returning a
	       ;;closure object with known signature.
	       (%build-core-expression input-form.stx rator.psi rand*.psi application-retvals.sig)))))

    (define* (%chi-application-of-identifier-rator input-form.stx lexenv.run lexenv.expand
						   rator.psi rand*.psi rator.id
						   rands.sig {application-retvals.sig type-signature?})
      (define (%build-default-application)
	(%build-core-expression input-form.stx rator.psi rand*.psi application-retvals.sig))
      (cond ((id->label rator.id)
	     => (lambda (rator.label)
		  (let ((rator.descr (label->syntactic-binding-descriptor rator.label lexenv.run)))
		    (cond ((syntactic-binding-descriptor/core-prim-typed? rator.descr)
			   ;;The rator is a typed core primitive.  For example:
			   ;;
			   ;;   (fx+ 1 2)
			   ;;
			   (cond (else
				  (%build-default-application))))

			  ((syntactic-binding-descriptor/lexical-closure-var? rator.descr)
			   ;;The rator is a typed lexical variable.  For example:
			   ;;
			   ;;   (define/checked (fun {m <flonum>} {x <flonum>} {q <flonum>})
			   ;;     (fl+ q (fl* m x)))
			   ;;
			   ;;   (fun 1. 2. 3.)
			   ;;
			   (cond ((lexical-closure-variable-spec.replacements
				   (syntactic-binding-descriptor/lexical-closure-var.typed-variable-spec rator.descr))
				  => (lambda (replacements)
				       (%select-application-replacement input-form.stx lexenv.run lexenv.expand
									rator.psi rand*.psi
									rands.sig application-retvals.sig
									replacements)))
				 (else
				  (%build-default-application))))

			  ((syntactic-binding-descriptor/global-closure-var? rator.descr)
			   (cond ((global-closure-variable-spec.replacements
				   (syntactic-binding-descriptor/global-closure-var.typed-variable-spec rator.descr))
				  => (lambda (replacements)
				       (%select-application-replacement input-form.stx lexenv.run lexenv.expand
									rator.psi rand*.psi
									rands.sig application-retvals.sig
									replacements)))
				 (else
				  (%build-default-application))))

			  ;; ((syntactic-binding-descriptor/lexical-typed-var? rator.descr)
			  ;;  (%build-default-application))

			  ;; ((syntactic-binding-descriptor/global-typed-var? rator.descr)
			  ;;  (%build-default-application))

			  (else
			   (%build-default-application))))))
	    (else
	     (error-unbound-identifier __module_who__ rator.id))))

    (define* (%select-application-replacement input-form.stx lexenv.run lexenv.expand
					      rator.psi rand*.psi
					      rands.sig {application-retvals.sig type-signature?}
					      replacements)
      ;;If a syntactic identifier in REPLACEMENTS is bound to a closure object with a
      ;;closure  that  exactly  matches  the   operands:  use  such  closure  in  the
      ;;application; otherwise use the original RATOR.PSI.  Return a psi object.
      ;;
      (let loop ((idx 0)
		 (len (vector-length replacements)))
	(if (fx=? idx len)
	    ;;No matching replacement.
	    (%build-core-expression input-form.stx rator.psi rand*.psi application-retvals.sig)
	  (let* ((repl.psi		(let ((repl.id (vector-ref replacements idx)))
					  (chi-expr repl.id lexenv.run lexenv.expand)))
		 (repl.clambda-sig	(let* ((repl.sig (psi.retvals-signature repl.psi))
					       (repl.ots (car (type-signature.object-type-specs repl.sig))))
					  (closure-type-spec.signature repl.ots))))
	    (cond ((exists (lambda (clause-signature)
			     (let ((args.sig (lambda-signature.argvals clause-signature)))
			       (if (eq? 'exact-match (type-signature.match-arguments-against-operands args.sig rands.sig))
				   (lambda-signature.retvals clause-signature)
				 #f)))
		     (case-lambda-signature.clause-signature* repl.clambda-sig))
		   => (lambda (repl-application-retvals.sig)
			;;Found replacement.
			(%build-core-expression input-form.stx repl.psi rand*.psi repl-application-retvals.sig)))
		  (else
		   (loop (fxadd1 idx) len)))))))

    #| end of module: %CHI-APPLICATION-WITH-MATCHING-SIGNATURE |# )

;;; --------------------------------------------------------------------

  (define* (%build-core-expression input-form.stx rator.psi rand*.psi {application-retvals.sig type-signature?})
    (make-psi input-form.stx
      (build-application (syntax-annotation input-form.stx)
	  (psi.core-expr rator.psi)
	(map psi.core-expr rand*.psi))
      application-retvals.sig))

  #| end of module |# )


;;;; done

#| end of module: CHI-APPLICATION |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;End:
