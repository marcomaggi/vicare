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
				       foldable-cons foldable-list foldable-vector)
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

    ((car . ?rand*)
     (chi-car-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((cdr . ?rand*)
     (chi-cdr-application input-form.stx lexenv.run lexenv.expand ?rand*))

    ((vector . ?rand*)
     (chi-vector-application input-form.stx lexenv.run lexenv.expand ?rand* 'vector))

    ((foldable-vector . ?rand*)
     (chi-vector-application input-form.stx lexenv.run lexenv.expand ?rand* 'foldable-vector))

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
	   (make-operands-signature-condition operands-signature))))))

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
	   (let ((appl.ots (%rand-signature->rand-object-type-spec input-form.stx (car rand*.stx) (car rand*.sig))))
	     (if appl.ots
		 ;;The operand returns a  single value with object-type specification
		 ;;APPL.OTS.  Good.
		 (cons appl.ots (recur (cdr rand*.stx) (cdr rand*.sig)))
	       ;;The operand  has a  standalone "<no-return>"  as signature:  in this
	       ;;case   the  whole   VALUES  application   must  have   a  standalone
	       ;;"<no-return>"  as  signature.
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


(module (chi-list-application)
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
  (define-module-who chi-list-application)

  (define (chi-list-application input-form.stx lexenv.run lexenv.expand rands.stx prim-name)
    (syntax-match rands.stx ()
      (()
       ;;No arguments.  Just return null.
       (make-psi input-form.stx
	 (build-data no-source '())
	 (make-type-signature/single-null)))

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

	   ((or (<list>-ots? rand.ots)
		(<pair>-ots? rand.ots))
	    (<top>-ots))

	   ((or (object-type-spec.compatible-super-and-sub? (<list>-ots) rand.ots)
		(object-type-spec.compatible-super-and-sub? (<pair>-ots) rand.ots))
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

	   ((or (<list>-ots? rand.ots)
		(<pair>-ots? rand.ots))
	    (<top>-ots))

	   ((or (object-type-spec.compatible-super-and-sub? (<list>-ots) rand.ots)
		(object-type-spec.compatible-super-and-sub? (<pair>-ots) rand.ots))
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


(module (chi-vector-application)
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
  (define-module-who chi-list-application)

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
       (make-type-signature/fully-untyped))

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
       (make-type-signature/fully-untyped))

      (else
       ;;The operand expression returns zero, two or more values.
       (handle-consumer-error "expression used as consumer operand returns multiple values")
       (make-type-signature/fully-untyped))))

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
	   ;;Is this a thunk?
	   (callable-signature.retvals (closure-type-spec.signature consumer.ots)))
	  ((<procedure>-ots? consumer.ots)
	   ;;Good.
	   (make-type-signature/fully-untyped))
	  (else
	   (handle-consumer-error "expression used as consumer does not return a procedure")
	   (make-type-signature/fully-untyped))))

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
		   ;;common retvals signature from the callable value.
		   => (lambda (rator.ots)
			(callable-signature.retvals (closure-type-spec.signature rator.ots))))

		  ((<procedure>)
		   ;;The operator is an untyped procedure, good.
		   (make-type-signature/fully-untyped))

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
			(make-type-signature/fully-untyped))
		       ((strict-r6rs)
			(make-type-signature/fully-untyped)))))

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
			(make-type-signature/fully-untyped))
		       ((strict-r6rs)
			(make-type-signature/fully-untyped)))))

		  ((unspecified-values)
		   ;;The operator expression returns fully unspecified values.
		   (make-type-signature/fully-untyped))

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
			(make-type-signature/fully-untyped))
		       ((strict-r6rs)
			(make-type-signature/fully-untyped))))))))))

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
  ;;In this  module we  handle the special  case of closure  object application  to a
  ;;given tuple of operands; here we know that the application to process is:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;and ?RATOR  will evaluate  to a closure  object with type  RATOR.OTS, which  is a
  ;;sub-type of "<procedure>".
  ;;
  (import CLOSURE-APPLICATION-ERRORS)
  (define-module-who chi-closure-object-application)

  (define* (chi-closure-object-application input-form.stx lexenv.run lexenv.expand
					   {rator.psi psi?} {rator.ots closure-type-spec?}
					   rand*.psi)
    (let ((signature (closure-type-spec.signature rator.ots)))
      (cond ((clambda-signature? signature)
	     (%process-clambda-application input-form.stx lexenv.run lexenv.expand
					   signature rator.psi rand*.psi))
	    (else
	     (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi)))))

;;; --------------------------------------------------------------------

  (module (%process-clambda-application)

    (define (%process-clambda-application input-form.stx lexenv.run lexenv.expand
					  rator.clambda-signature rator.psi rand*.psi)
      ;;Here we perform some preliminary validations:
      ;;
      ;;1. The number of operands must be  in the correct range of arguments accepted
      ;;by the closure object.
      ;;
      ;;2. All the operands must return a single value.
      ;;
      (%validate-clambda-number-of-arguments input-form.stx rator.clambda-signature rator.psi rand*.psi)
      (let ((rands.sig (%validate-operands-for-single-return-value input-form.stx rand*.psi)))
	(%match-clambda-signature-against-operands input-form.stx lexenv.run lexenv.expand
						   rator.clambda-signature rator.psi rand*.psi rands.sig)))

    (define* (%validate-clambda-number-of-arguments input-form.stx rator.clambda-signature rator.psi rand*.psi)
      (import CLOSURE-APPLICATION-ERRORS)
      (receive (minimum-arguments-count maximum-arguments-count)
	  (clambda-signature.min-and-max-argvals rator.clambda-signature)
	(let ((given-operands-count (length rand*.psi)))
	  (cond ((< maximum-arguments-count given-operands-count)
		 (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
		   (psi.input-form rator.psi) (map psi.input-form rand*.psi)
		   maximum-arguments-count given-operands-count))
		((> minimum-arguments-count given-operands-count)
		 (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
		   (psi.input-form rator.psi) (map psi.input-form rand*.psi)
		   minimum-arguments-count given-operands-count))
		(else
		 (void))))))

    (define* (%validate-operands-for-single-return-value input-form.stx rand*.psi)
      ;;In the context of INPUT-FORM.STX the  RAND*.PSI argument is a list of objects
      ;;representing  the  operands of  an  operator  application.  Check  that  such
      ;;expressions return a single value.
      ;;
      ;;Return an instance  of "<type-signature>" representing the  type signature of
      ;;the  operands.  Whenever  it is  not  possible to  determine the  type of  an
      ;;operand: the type defaults to "<top>".
      ;;
      (make-type-signature
       (map (lambda (rand.psi)
	      (import CLOSURE-APPLICATION-ERRORS)
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
		((single-value)
		 ;;Single return value.  Good.
		 => (lambda (rand.ots) rand.ots))
		(<list-of>
		 ;;Unspecified number  of return values,  of known type.  We  let the
		 ;;compiler insert the checks for the single return value case.
		 => (lambda (rand.ots)
		      (list-of-type-spec.item-ots rand.ots)))
		(<list>
		 ;;Unspecified number of return values,  of unknown type.  We let the
		 ;;compiler insert the checks for the single return value case.
		 (<top>-ots))
		(<no-return>
		 ;;The operand expression will not return.
		 (%handle-error "expression used as operand in procedure application is typed as not returning"        (<top>-ots)))
		(else
		 ;;Zero, two or more return values.  Wrong.
		 (%handle-error "expression used as operand in procedure application returns zero, two or more values" (<top>-ots)))))
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

    #| end of module: %PROCESS-CLAMBDA-APPLICATION |# )

;;; --------------------------------------------------------------------

  (define (%match-clambda-signature-against-operands input-form.stx lexenv.run lexenv.expand
						     rator.clambda-signature rator.psi rand*.psi rands.sig)
    ;;For operators having  a "<clambda-signature>" signature: we  iterate, in order,
    ;;through  the  "<clambda-clause-signature>"  instances representing  the  clause
    ;;signatures, looking for the first that matches.
    ;;
    ;;The argument  RATOR.CLAMBDA-SIGNATURE is  an instance  of "<clambda-signature>"
    ;;representing the closure object's signatures.
    ;;
    ;;The argument RATOR.PSI is the already expanded operator expression.
    ;;
    ;;The  argument  RAND*.PSI   is  a  proper  list  of   already  expanded  operand
    ;;expressions.
    ;;
    ;;Assuming  each operand  expression in  RAND*.PSI  returns a  single value:  the
    ;;argument  RANDS.SIG  is  an  instance of  "<type-signature>"  representing  the
    ;;operands' types.
    ;;
    (define (%build-default-application)
      (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi #f))
    (receive (state clause-signature)
	(returnable
	  (values (fold-left (lambda (state clause-signature)
			       (let ((args.sig (clambda-clause-signature.argvals clause-signature)))
				 ;; (debug-print (syntax->datum input-form.stx)
				 ;; 	      args.sig rands.sig
				 ;; 	      (type-signature.match-arguments-against-operands args.sig rands.sig))
				 (case (type-signature.match-arguments-against-operands args.sig rands.sig)
				   ((exact-match)
				    (return 'exact-match clause-signature))
				   ((possible-match)
				    'possible-match)
				   (else state))))
		    'no-match (clambda-signature.clause-signature* rator.clambda-signature))
		  #f))
      (case state
	((exact-match)
	 ;;There is a clause that exactly matches the operands.
	 (%process-application-with-matching-signature input-form.stx lexenv.run lexenv.expand
						       rator.psi rand*.psi
						       (clambda-clause-signature.retvals clause-signature)))
	((possible-match)
	 ;;There is at least one clause with a possible match.  It is not possible to
	 ;;fully  validate  the  signatures  at  expand-time;  we  rely  on  run-time
	 ;;checking.
	 (%build-default-application))
	(else
	 ;;There are no  matching clauses, not even possible  matches.  Arguments and
	 ;;operands do *not* match at expand-time.
	 (let ((make-arguments-signature* (lambda ()
					    (map clambda-clause-signature.argvals
					      (clambda-signature.clause-signature* rator.clambda-signature)))))
	   (case-expander-language
	     ((typed)
	      (%error-mismatch-between-args-signature-and-operands-signature   input-form.stx (make-arguments-signature*) rands.sig))
	     ((default)
	      (%warning-mismatch-between-args-signature-and-operands-signature input-form.stx (make-arguments-signature*) rands.sig)
	      (%build-default-application))
	     ((strict-r6rs)
	      (%build-default-application))))))))

;;; --------------------------------------------------------------------

  (define (%process-application-with-matching-signature input-form.stx lexenv.run lexenv.expand
							rator.psi rand*.psi clause-application-retvals.sig)
    ;;We are  applying an operator RATOR.PSI  to a tuple of  operands RAND*.PSI.  The
    ;;operator is a closure object.  One of the closure's clauses has arguments' type
    ;;signature  matching (or  compatible with)  the operands'  type signature.   The
    ;;matching   closure's   clause   has   return   values   with   type   signature
    ;;CLAUSE-APPLICATION-RETVALS.SIG.
    ;;
    ;;If the operator is an identifier: it is possible that it has an unsafe variant.
    ;;Let's  try to  substitute  the application  of the  operantor  with its  unsafe
    ;;variant.
    ;;
    (define (%build-default-application)
      (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi clause-application-retvals.sig))
    (define (%build-unsafe-variant-application unsafe-rator.sexp)
      ;; (let* ((unsafe-rator.stx (bless unsafe-rator.sexp))
      ;;        (unsafe-rator.psi (chi-expr unsafe-rator.stx lexenv.run lexenv.expand)))
      ;;   (%build-core-expression input-form.stx lexenv.run unsafe-rator.psi rand*.psi clause-application-retvals.sig))
      (%build-default-application))
    (define* (%build-typed-variable-application {rator.spec typed-variable-spec?})
      ;; (cond ((typed-variable-spec.unsafe-variant-sexp rator.spec)
      ;;        => %build-unsafe-variant-application)
      ;;       (else
      ;;        (%build-default-application)))
      (%build-default-application))
    (define rator.stx (psi.input-form rator.psi))
    (cond ((identifier? rator.stx)
	   ;;Here we do not want to raise an error if the identifier RATOR.STX is not
	   ;;a typed lexical variable (it might be an identifier expression returning
	   ;;a closure object).
	   (cond ((id->label rator.stx)
		  => (lambda (rator.label)
		       (let ((rator.descr (label->syntactic-binding-descriptor rator.label lexenv.run)))
			 (cond ((syntactic-binding-descriptor/core-prim-typed? rator.descr)
				;;FIXME  Unsafe  variants   for  core  primitives  is
				;;temporarily disabled  until the table  of primitive
				;;properties  declaration is  rewritten  in a  format
				;;that allows  correct selection of  the replacement.
				;;(Marco Maggi; Sun Jan 10, 2016)
				(cond (#f
				       => %build-unsafe-variant-application)
				      (else
				       (%build-default-application))))
			       ((syntactic-binding-descriptor/lexical-typed-var? rator.descr)
				(%build-typed-variable-application
				 (syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec rator.descr)))
			       ((syntactic-binding-descriptor/global-typed-var? rator.descr)
				(%build-typed-variable-application
				 (syntactic-binding-descriptor/global-typed-var.typed-variable-spec rator.descr)))
			       (else
				(%build-default-application))))))
		 (else
		  (error-unbound-identifier __module_who__ rator.stx))))
	  (else
	   ;;If we  are here  the rator  is a  non-identifier expression  returning a
	   ;;closure object with known signature.
	   (%build-default-application))))

;;; --------------------------------------------------------------------

  (module (%build-core-expression)

    (case-define* %build-core-expression
      ((input-form.stx lexenv.run rator.psi rand*.psi)
       (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi #f))
      ((input-form.stx lexenv.run rator.psi rand*.psi clause-application-retvals.sig)
       (let* ((rator.core	(psi.core-expr rator.psi))
	      (rand*.core	(map psi.core-expr rand*.psi)))
	 (make-psi input-form.stx
	   (build-application (syntax-annotation input-form.stx)
	       rator.core
	     rand*.core)
	   (or clause-application-retvals.sig
	       (%build-application-retvals-signature input-form.stx lexenv.run rator.psi))))))

    (define* ({%build-application-retvals-signature type-signature?} input-form.stx lexenv rator.psi)
      ;;This function is used to build the "<type-signature>" of values returned by a
      ;;closure object application, when it was not possible to determine it from the
      ;;types of the operands.
      ;;
      (define (%handle-error message)
	(%error (lambda ()
		  (condition
		    (make-who-condition __module_who__)
		    (make-message-condition message)
		    (make-syntax-violation input-form.stx (psi.input-form rator.psi))
		    (make-application-operator-signature-condition (psi.retvals-signature rator.psi))))))
      (case-signature-specs (psi.retvals-signature rator.psi)
	((<closure>)
	 ;;The  operator is  a "<closure-type-spec>",  good.  We  extract the  common
	 ;;retvals signature from the callable value.
	 => (lambda (rator.ots)
	      (callable-signature.retvals (closure-type-spec.signature rator.ots))))

	((<procedure>)
	 ;;The operator is an untyped procedure, good.
	 (make-type-signature/fully-untyped))

	((single-value)
	 ;;The operator is a single value, but not a procedure.
	 (%handle-error "expression in operator application position is not a closure type"))

	(<no-return>
	 ;;The operator is marked to no-return: it means it raises an exception.  For
	 ;;example:
	 ;;
	 ;;   ((error #f "bad") 1 2)
	 ;;
	 (%handle-error "expression used as operator in procedure application is typed as not returning"))

	((unspecified-values)
	 ;;The operator expression returns fully unspecified values.
	 (make-type-signature/fully-untyped))

	(else
	 ;;The operator returns zero, two or more return values.
	 (%handle-error "expression used as operator in procedure application returns zero, two or more values"))))

    (define (%error common)
      (case-expander-language
	((typed)
	 (raise			(condition (make-expand-time-type-signature-violation) (common))))
	((default)
	 (raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
	 (make-type-signature/fully-untyped))
	((strict-r6rs)
	 (make-type-signature/fully-untyped))))

    #| end of module: %BUILD-CORE-EXPRESSION |# )

  #| end of module: CHI-CLOSURE-OBJECT-APPLICATION |# )


;;;; done

#| end of module: CHI-APPLICATION |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;End:
