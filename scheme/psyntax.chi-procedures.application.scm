;;;Copyright (c) 2010-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (syntax-match input-form.stx (values apply map1 for-each1 for-all1 exists1)
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
     (%chi-nested-rator-application input-form.stx lexenv.run lexenv.expand
				    (cons ?nested-rator ?nested-rand*) ?rand*))

    ((values ?rand* ...)
     ;;A call to VALUES is special because  VALUES does not have a predefined retvals
     ;;signature, but the retvals signature equals the arguments' signature.
     (%chi-values-application input-form.stx lexenv.run lexenv.expand
			      ?rand*))

    ((apply ?rator ?rand* ...)
     (%chi-apply-application input-form.stx lexenv.run lexenv.expand
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


(define (%chi-nested-rator-application input-form.stx lexenv.run lexenv.expand
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


(define (%chi-values-application input-form.stx lexenv.run lexenv.expand
				 rand*.stx)
  ;;The input form has the syntax:
  ;;
  ;;   (values ?rand ...)
  ;;
  ;;and RAND*.STX is the syntax object:
  ;;
  ;;   (#'?rand ...)
  ;;
  ;;A call  to VALUES is  special because VALUES does  not have a  predefined retvals
  ;;signature, but the retvals signature equals the operands' signature.
  ;;
  (let* ((rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	 (rand*.core (map psi.core-expr rand*.psi))
	 (rand*.sig  (map psi.retvals-signature rand*.psi))
	 (rator.core (build-primref no-source 'values)))
    (define application.sig
      (let loop ((rand*.sig rand*.sig)
		 (rand*.stx rand*.stx)
		 (rand*.tag '()))
	(if (pair? rand*.sig)
	    ;;To be a valid VALUES argument an expression must have as signature:
	    ;;
	    ;;   (?tag)
	    ;;
	    ;;which means a single return value.   Here we allow "<list>", too, which
	    ;;means we  accept an  argument having unknown  retval signature,  and we
	    ;;will see what happens at run-time; we accept only "<list>", we reject a
	    ;;signature if it is a standalone sub-tag of "<list>".
	    ;;
	    ;;NOTE  We could  reject "<list>"  as argument  signature and  demand the
	    ;;caller  of VALUES  to cast  the arguments,  but it  would be  too much;
	    ;;remember that  we still do  some signature validation even  when tagged
	    ;;language support is off.  (Marco Maggi; Mon Mar 31, 2014)
	    (syntax-match (type-signature-tags (car rand*.sig)) ()
	      ((?tag)
	       (loop (cdr rand*.sig) (cdr rand*.stx)
		     (cons ?tag rand*.tag)))
	      (?tag
	       (list-tag-id? ?tag)
	       (loop (cdr rand*.sig) (cdr rand*.stx)
		     (cons (top-tag-id) rand*.tag)))
	      (_
	       (expand-time-retvals-signature-violation 'values
							input-form.stx (car rand*.stx)
							(make-type-signature/single-top)
							(car rand*.sig))))
	  (make-type-signature (reverse rand*.tag)))))
    (make-psi input-form.stx
	      (build-application (syntax-annotation input-form.stx)
		rator.core
		rand*.core)
	      application.sig)))


(module (%chi-apply-application)

  (define (%chi-apply-application input-form.stx lexenv.run lexenv.expand
				  rator.stx rand*.stx)
    ;;The input form is a call to the core primitive APPLY:
    ;;
    ;;   (apply ?rator ?rand ...)
    ;;
    ;;the argument  RATOR.STX is #'?RATOR  and the  argument RAND*.STX is  the syntax
    ;;object representing the list of operands:
    ;;
    ;;   #'(?rand ...)
    ;;
    (let* ((rator.psi (chi-expr rator.stx lexenv.run lexenv.expand))
	   (rator.sig (psi.retvals-signature rator.psi)))
      (define (%error-wrong-rator-sig)
	(expand-time-retvals-signature-violation 'apply input-form.stx rator.stx
						 (make-type-signature/single-procedure)
						 rator.sig))
      (syntax-match (type-signature-tags rator.sig) ()
	((?rator.tag)
	 (cond ((type-identifier-is-procedure-or-procedure-sub-type? ?rator.tag lexenv.run)
		;;Procedure application: good.
		(let* ((apply.core (build-primref no-source 'apply))
		       (rator.core (psi.core-expr rator.psi))
		       (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
		       (rand*.core (map psi.core-expr rand*.psi)))
		  (make-psi input-form.stx
			    (build-application (syntax-annotation input-form.stx)
			      apply.core
			      (cons rator.core rand*.core))
			    (psi-application-retvals-signature input-form.stx lexenv.run rator.psi))))
	       ((top-tag-id? ?rator.tag)
		;;Let's do it and we will see at run-time what happens.  Notice that,
		;;in this case: we do not know the signature of the return values.
		(%build-application-no-signature input-form.stx lexenv.run lexenv.expand
						 rator.psi rand*.stx))
	       (else
		;;Non-procedure: bad.
		(%error-wrong-rator-sig))))

	(?rator.tag
	 (list-tag-id? ?rator.tag)
	 ;;Fully unspecified return values.  Let's do  it and we will see at run-time
	 ;;what happens.  Notice that, in this case:  we do not know the signature of
	 ;;the return values.
	 (%build-application-no-signature input-form.stx lexenv.run lexenv.expand
					  rator.psi rand*.stx))

	(_
	 ;;Everything else is wrong.
	 (%error-wrong-rator-sig)))))

  (define (%build-application-no-signature input-form.stx lexenv.run lexenv.expand
					   rator.psi rand*.stx)
    (let* ((apply.core (build-primref no-source 'apply))
	   (rator.core (psi.core-expr rator.psi))
	   (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	   (rand*.core (map psi.core-expr rand*.psi)))
      (make-psi input-form.stx
		(build-application (syntax-annotation input-form.stx)
		  apply.core
		  (cons rator.core rand*.core)))))

  #| end of module: %CHI-APPLY-APPLICATION |# )


;;;; chi procedures: operator application processing, operator already expanded

(module (chi-application/psi-rator)

  (define* (chi-application/psi-rator input-form.stx lexenv.run lexenv.expand
				      {rator.psi psi?} rand*.stx)
    ;;Expand an operator  application form; it is called when  INPUT-FORM.STX has the
    ;;format:
    ;;
    ;;   (?rator ?rand ...)
    ;;
    ;;and ?RATOR is neither a macro keyword identifier, nor a VALUES application, nor
    ;;an  APPLY application,  nor a  SPLICE-FIRST-EXPAND syntax.   For example  it is
    ;;called when INPUT-FORM.STX is:
    ;;
    ;;   (?core-prim ?rand ...)
    ;;   ((?sub-rator ?sub-rand ...) ?rand ...)
    ;;
    ;;We call this function when the operator has already been expanded.
    ;;
    (define rator.sig (psi.retvals-signature rator.psi))
    (syntax-match (type-signature-tags rator.sig) ()
      (?tag
       (list-tag-id? ?tag)
       ;;The rator type  is unknown: evaluating the rator might  return any number of
       ;;values of any  type.  Return a normal  rator application and we  will see at
       ;;run-time what happens; this is standard Scheme behaviour.
       (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					rator.psi rand*.stx))

      ((?tag)
       ;;The rator type is a single value.  Good, this is what it is meant to be.
       (%process-single-value-rator-type input-form.stx lexenv.run lexenv.expand
					 rator.psi rator.sig ?tag rand*.stx))

      (_
       ;;The rator is declared to evaluate to multiple values.
       (if (options::strict-r6rs)
	   ;;According to the standard we must  insert a normal rator application and
	   ;;raise an exception at run-time.
	   (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					    rator.psi rand*.stx)
	 ;;Multiple values are invalid in call context: raise an exception.
	 (raise
	  (condition (make-who-condition __who__)
		     (make-message-condition "call operator declared to evaluate to multiple values")
		     (syntax-match input-form.stx ()
		       ((?rator . ?rands)
			(make-syntax-violation input-form.stx ?rator)))
		     (make-retvals-signature-condition rator.sig)))))
      ))

  (define* (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					    {rator.psi psi?} rand*.stx)
    ;;Build a core language expression to apply  the rator to the rands; return a PSI
    ;;struct.  This is an application form in standard (untyped) Scheme language.  We
    ;;do not know what the retvals signature  of the application is; the returned PSI
    ;;struct  will have  "<list>" as  retvals signature,  which means  any number  of
    ;;values of any type.
    ;;
    (let* ((rator.core (psi.core-expr rator.psi))
	   (rand*.psi  (chi-expr* rand*.stx lexenv.run lexenv.expand))
	   (rand*.core (map psi.core-expr rand*.psi)))
      (make-psi input-form.stx
		(build-application (syntax-annotation input-form.stx)
		  rator.core
		  rand*.core))))

  (define* (%process-single-value-rator-type input-form.stx lexenv.run lexenv.expand
					     {rator.psi psi?} rator.sig rator.tag rand*.stx)
    ;;Build a  core language expression to  apply the rator  to the rands when  it is
    ;;known that the  rator will return a  single value with specified  type.  Do the
    ;;following:
    ;;
    ;;* If  the rator  is a  procedure (its  type is  "<procedure>" or  its sub-type)
    ;;return a PSI struct as per standard Scheme behaviour and, when possible, select
    ;;the appropriate retvals signature for the returned PSI.
    ;;
    ;;* If  the type of rator  is "<top>": expand  to an application as  per standard
    ;;Scheme behaviour.
    ;;
    ;;* Otherwise raise a syntax violation.
    ;;
    (cond ((type-identifier-is-procedure-or-procedure-sub-type? rator.tag lexenv.run)
	   ;;The rator is  a procedure: very good; return  a procedure application.
	   (let ((rand*.psi (chi-expr* rand*.stx lexenv.run lexenv.expand)))
	     (%process-closure-object-application input-form.stx lexenv.run lexenv.expand
						  rator.tag rator.psi rand*.psi)))

	  ((top-tag-id? rator.tag)
	   ;;The rator  type is  unknown, we  only know  that it  is a  single value.
	   ;;Return a procedure application and we will see at run-time what happens.
	   (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					    rator.psi rand*.stx))

	  (else
	   ;;The rator  has a correct single-value  signature and it has  a specified
	   ;;type, but it is not a procedure.
	   (raise
	    (condition (make-who-condition __who__)
		       (make-message-condition "call operator declared to evaluate to non-procedure value")
		       (syntax-match input-form.stx ()
			 ((?rator . ?rands)
			  (make-syntax-violation input-form.stx ?rator)))
		       (make-retvals-signature-condition rator.sig))))))

  #| end of module: CHI-APPLICATION/PSI-RATOR |# )


;;;; chi procedures: operator application processing, first operand already expanded

(module (chi-application/psi-first-operand)

  (define* (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					      rator.stx {first-rand.psi psi?} other-rand*.stx)
    ;;This  is an  entry  point, not  a sub-routine  of  CHI-APPLICATION.  Expand  an
    ;;operator application  form; it is  called when  application to process  has the
    ;;format:
    ;;
    ;;   (?rator ?first-rand ?other-rand ...)
    ;;
    ;;and ?FIRST-RAND has already been expanded.  Here we know that the input form is
    ;;a special syntax like IS-A?,  SLOT-REF, SLOT-SET!, METHOD-CALL; so the operator
    ;;will expand into a predicate, accessor, mutator or method.
    ;;
    (define rator.psi (while-not-expanding-application-first-subform
		       (chi-expr rator.stx lexenv.run lexenv.expand)))
    (define rator.sig (psi.retvals-signature rator.psi))
    (define (%common-rator-application)
      (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
				       rator.psi first-rand.psi other-rand*.stx))
    (syntax-match (type-signature-tags rator.sig) ()
      (?tag
       (list-tag-id? ?tag)
       ;;The rator type  is unknown: evaluating the rator might  return any number of
       ;;values of any  type.  Return a normal  rator application and we  will see at
       ;;run-time what happens; this is standard Scheme behaviour.
       (%common-rator-application))

      ((?tag)
       (top-tag-id? ?tag)
       ;;The rator type is unknown, we only know  that it is a single value; it might
       ;;be a closure object or not.  Return  a procedure application and we will see
       ;;at run-time what happens.
       (%common-rator-application))

      ((?tag)
       ;;The rator type is a single value.  Good, this is what it is meant to be.
       (%process-single-value-rator-type input-form.stx lexenv.run lexenv.expand
					 rator.psi rator.stx ?tag first-rand.psi other-rand*.stx))

      (_
       ;;The rator  is declared to  evaluate to multiple  values; this is  invalid in
       ;;call  context,  so we  raise  an  exception.   This is  non-standard  Scheme
       ;;behaviour:  according  to the  standard  we  should  insert a  normal  rator
       ;;application and raise an exception at run-time.
       (raise
	(condition (make-who-condition __who__)
		   (make-message-condition "call operator declared to evaluate to multiple values")
		   (make-syntax-violation input-form.stx #f)
		   (make-application-operator-condition rator.stx)
		   (make-retvals-signature-condition rator.sig))))
      ))

  (define* (%build-common-rator-application input-form.stx lexenv.run lexenv.expand
					    {rator.psi psi?} {first-rand.psi psi?} other-rand*.stx)
    ;;Build a core language expression to apply  the rator to the rands; return a PSI
    ;;struct.  This is an application form in standard (untyped) Scheme language.  We
    ;;do not know what the retvals signature  of the application is; the returned PSI
    ;;struct  will have  "<list>" as  retvals signature,  which means  any number  of
    ;;values of any type.
    ;;
    (let* ((rator.core       (psi.core-expr rator.psi))
	   (first-rand.core  (psi.core-expr first-rand.psi))
	   (other-rand*.psi  (chi-expr* other-rand*.stx lexenv.run lexenv.expand))
	   (other-rand*.core (map psi.core-expr other-rand*.psi)))
      (make-psi input-form.stx
		(build-application (syntax-annotation input-form.stx)
		  rator.core
		  (cons first-rand.core other-rand*.core)))))

  (define* (%process-single-value-rator-type input-form.stx lexenv.run lexenv.expand
					     {rator.psi psi?} rator.stx rator.tag
					     {first-rand.psi psi?} other-rand*.stx)
    ;;Build a  core language expression to  apply the rator  to the rands when  it is
    ;;known that the rator will return a single value with specified type.
    ;;
    (cond ((type-identifier-is-procedure-or-procedure-sub-type? rator.tag lexenv.run)
	   ;;The rator is a procedure: very good; return a procedure application.
	   (let* ((other-rand*.psi (chi-expr* other-rand*.stx lexenv.run lexenv.expand))
		  (rand*.psi       (cons first-rand.psi other-rand*.psi)))
	     (%process-closure-object-application input-form.stx lexenv.run lexenv.expand
						  rator.tag rator.psi rand*.psi)))

	  (else
	   ;;The rator  has a correct single-value  signature and it has  a specified
	   ;;type, but it is not a procedure.
	   (raise
	    (condition (make-who-condition __who__)
		       (make-message-condition "call operator declared to evaluate to non-procedure value")
		       (make-syntax-violation input-form.stx #f)
		       (make-application-operator-condition rator.stx)
		       (make-type-syntactic-identifier-condition rator.tag))))))

  #| end of module: CHI-APPLICATION/PSI-FIRST-OPERAND |# )


(module CLOSURE-APPLICATION-ERRORS
  ( ;;
   %error-number-of-operands-exceeds-maximum-arguments-count
   %error-number-of-operands-deceeds-minimum-arguments-count
   %error-operand-with-multiple-return-values
   %error-mismatch-between-argvals-signature-and-operands-signature)

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

  ;; (define-condition-type &argument-description
  ;;     &condition
  ;;   make-argument-description-condition
  ;;   argument-description-condition?
  ;;   (zero-based-argument-index argument-description-index)
  ;;   (expected-argument-tag     argument-description-expected-tag))

  (define-condition-type &clambda-signature
      &condition
    make-clambda-signature-condition
    clambda-signature-condition?
    (signature		clambda-signature-condition.signature))

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
       (make-syntax-violation input-form.stx #f)
       (make-wrong-number-of-arguments-error-condition)
       (make-application-operator-condition rator.stx)
       (make-application-operands-condition rand*.stx)
       (make-maximum-arguments-count-condition maximum-arguments-count)
       (make-given-operands-count-condition given-operands-count))))

  (define (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
	    rator.stx rand*.stx minimum-arguments-count given-operands-count)
    (raise-compound-condition-object 'chi-application
      "while expanding, detected wrong number of operands in function application: number of operands deceeds minimum number of arguments"
      input-form.stx
      (condition
       (make-syntax-violation input-form.stx #f)
       (make-wrong-number-of-arguments-error-condition)
       (make-application-operator-condition rator.stx)
       (make-application-operands-condition rand*.stx)
       (make-minimum-arguments-count-condition minimum-arguments-count)
       (make-given-operands-count-condition given-operands-count))))

  (define (%error-operand-with-multiple-return-values input-form.stx rand.stx rand.sig)
    (raise-compound-condition-object 'chi-application
      "expand-time error: operand with multiple return values"
      input-form.stx
      (condition
       (make-expand-time-type-signature-violation)
       (make-syntax-violation input-form.stx rand.stx)
       (make-operands-signature-condition (list rand.sig)))))

  (define (%error-mismatch-between-argvals-signature-and-operands-signature input-form.stx
	    clambda-signature operands-signature)
    (raise-compound-condition-object 'chi-application
      "expand-time mismatch between closure object's arguments signatures and operands signature"
      input-form.stx
      (condition
       (make-expand-time-type-signature-violation)
       (make-syntax-violation input-form.stx #f)
       (make-clambda-signature-condition clambda-signature)
       (make-operands-signature-condition operands-signature))))

  #| end of module: CLOSURE-APPLICATION-ERRORS |# )


;;;; chi procedures: closure object application processing

(module (%process-closure-object-application)
  ;;In this  module we  handle the special  case of closure  object application  to a
  ;;given tuple of operands; here we know that the application to process is:
  ;;
  ;;   (?rator ?rand ...)
  ;;
  ;;and ?RATOR  will evaluate  to a  closure object  with type  identifier RATOR.TAG,
  ;;which is a sub-type of "<procedure>".
  ;;
  (define-module-who %process-closure-object-application)

  (define (%process-closure-object-application input-form.stx lexenv.run lexenv.expand
					       rator.tag rator.psi rand*.psi)
    (define (%no-optimisations-possible)
      (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi))
    (if (predicate-tag-id? rator.tag)
	(%process-predicate-application input-form.stx lexenv.run lexenv.expand
					rator.psi rand*.psi)
      (let* ((label (id->label/or-error __module_who__ input-form.stx rator.tag))
	     (descr (label->syntactic-binding-descriptor label lexenv.run))
	     (spec  (car (syntactic-binding-descriptor.value descr))))
	(if (closure-type-spec? spec)
	    (let ((signature (closure-type-spec.signature spec)))
	      (cond ((clambda-signature? signature)
		     (%process-clambda-application input-form.stx lexenv.run lexenv.expand
						   signature rator.psi rand*.psi))
		    (else
		     (%no-optimisations-possible))))
	  (%no-optimisations-possible)))))

  (define (%process-predicate-application input-form.stx lexenv.run lexenv.expand
					  rator.psi rand*.psi)
    (define (%no-optimisation-possible)
      (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi))
    (if (options::strict-r6rs)
	;;We rely on run-time checking.
	(%no-optimisation-possible))
    (let ((rand*.sig (map psi.retvals-signature rand*.psi)))
      (%validate-clambda-number-of-arguments input-form.stx 1 1 rator.psi rand*.psi rand*.sig)
      (%validate-operands-for-single-return-value input-form.stx rand*.psi rand*.sig)
      (%no-optimisation-possible)))

  (define (%process-clambda-application input-form.stx lexenv.run lexenv.expand
					rator.clambda-signature rator.psi rand*.psi)
    ;;For  rators having  a "<clambda-signature>"  signature: we  iterate, in  order,
    ;;through  the  "<clambda-clause-signature>"  instances representing  the  clause
    ;;signatures, looking for the first that matches.
    ;;
    (import CLOSURE-APPLICATION-ERRORS)
    (case-define %no-optimisation-possible
      (()
       (%no-optimisation-possible #f))
      ((retvals.sig)
       (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi retvals.sig)))
    (if (options::strict-r6rs)
	;;We rely on run-time checking.
	(%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi)
      (let ((rand*.sig (map psi.retvals-signature rand*.psi)))
	(receive (minimum-arguments-count maximum-arguments-count)
	    (clambda-signature.min-and-max-argvals rator.clambda-signature)
	  (%validate-clambda-number-of-arguments input-form.stx minimum-arguments-count maximum-arguments-count
						 rator.psi rand*.psi rand*.sig))
	(%validate-operands-for-single-return-value input-form.stx rand*.psi rand*.sig)
	(let loop ((clause-signature*	(clambda-signature.clause-signature* rator.clambda-signature))
		   ;;In this loop the variable STATE always upgrades: from "no-match"
		   ;;to "exact-match"  or "possible-match"; from  "possible-match" to
		   ;;"exact-match".  It never degrades.
		   (state		'no-match))
	  (if (pair? clause-signature*)
	      (let* ((clause.csig (car clause-signature*))
		     (argvals.sig (clambda-clause-signature.argvals clause.csig)))
		(case (type-signature.match-arguments-against-operands input-form.stx lexenv.run lexenv.expand
								       argvals.sig rand*.sig)
		  ((exact-match)
		   ;;The  operands match  the signature:  we are  applying a  closure
		   ;;object rator to a tuple of rands that have the right type.
		   (let ((retvals.sig (clambda-clause-signature.retvals clause.csig)))
		     (if (type-signature.untyped? argvals.sig)
			 (%no-optimisation-possible retvals.sig)
		       (%process-application-with-matching-signature input-form.stx lexenv.run lexenv.expand
								     rator.psi rand*.psi retvals.sig))))
		  ((possible-match)
		   ;;This clause is a possible match, fine.  Let's see if there is an
		   ;;exact match among the rest of the clauses.
		   (loop (cdr clause-signature*) 'possible-match))
		  ((no-match)
		   ;;This clause  does not  match.  Let's  see if  there is  an exact
		   ;;match among the rest of the clauses.
		   (loop (cdr clause-signature*) state))
		  (else
		   (assertion-violation __module_who__ "internal error, invalid matching state" state))))
	    (case state
	      ((possible-match)
	       ;;It is  not possible  to validate the  signatures at  expand-time; we
	       ;;rely on run-time checking.
	       (%no-optimisation-possible))
	      ((no-match)
	       ;;Arguments and operands do *not* match at expand-time.
	       (%error-mismatch-between-argvals-signature-and-operands-signature input-form.stx
		 rator.clambda-signature rand*.sig))))))))

;;; --------------------------------------------------------------------

  (define* (%validate-clambda-number-of-arguments input-form.stx minimum-arguments-count maximum-arguments-count rator.psi rand*.psi rand*.sig)
    (import CLOSURE-APPLICATION-ERRORS)
    (let ((given-operands-count (length rand*.sig)))
      (cond ((< maximum-arguments-count given-operands-count)
	     (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
	       (psi.input-form rator.psi) (map psi.input-form rand*.psi)
	       maximum-arguments-count given-operands-count))
	    ((> minimum-arguments-count given-operands-count)
	     (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
	       (psi.input-form rator.psi) (map psi.input-form rand*.psi)
	       minimum-arguments-count given-operands-count))
	    (else
	     (void)))))

  (define* (%validate-operands-for-single-return-value input-form.stx rand*.psi rand*.sig)
    (import CLOSURE-APPLICATION-ERRORS)
    (for-each (lambda (rand.psi rand.sig)
		(syntax-match (type-signature-tags rand.sig) (<list>)
		  ((?type0 ?type1 . ?rest)
		   ;;Two or more return values.  Wrong.
		   (%error-operand-with-multiple-return-values input-form.stx (psi.input-form rand.psi) rand.sig))
		  (_
		   ;;Everything   else  is   possibly  good,   maybe  with   run-time
		   ;;validation.
		   (void))))
      rand*.psi rand*.sig))

;;; --------------------------------------------------------------------

  (define (%process-application-with-matching-signature input-form.stx lexenv.run lexenv.expand
							rator.psi rand*.psi retvals.sig)
    ;;We are  applying an operator RATOR.PSI  to a tuple of  operands RAND*.PSI.  The
    ;;operator is a closure object.  One of the closure's clauses has arguments' type
    ;;signature matching the operands' type signature.  The matching closure's clause
    ;;has return values with type signature RETVALS.SIG.
    ;;
    ;;If the operator is an identifier: it is possible that it has an unsafe variant.
    ;;Let's  try to  substitute  the application  of the  operantor  with its  unsafe
    ;;variant.
    ;;
    (define (%no-optimisation-possible)
      (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi retvals.sig))
    (define (%build-unsafe-variant-application unsafe-rator.sexp)
      (let* ((unsafe-rator.stx (bless unsafe-rator.sexp))
	     (unsafe-rator.psi (chi-expr unsafe-rator.stx lexenv.run lexenv.expand)))
	(%build-core-expression input-form.stx lexenv.run unsafe-rator.psi rand*.psi retvals.sig)))
    (define* (%build-typed-variable-application {rator.spec typed-variable-spec?})
      (cond ((typed-variable-spec.unsafe-variant-sexp rator.spec)
	     => %build-unsafe-variant-application)
	    (else
	     (%no-optimisation-possible))))
    (define rator.stx (psi.input-form rator.psi))
    (cond ((identifier? rator.stx)
	   ;;Here we do not want to raise an error if the identifier RATOR.STX is not
	   ;;a typed lexical variable (it might be an identifier expression returning
	   ;;a closure object).
	   (let* ((rator.label (id->label/or-error __module_who__ input-form.stx rator.stx))
		  (rator.descr (label->syntactic-binding-descriptor rator.label lexenv.run)))
	     (cond ((syntactic-binding-descriptor/core-prim-typed? rator.descr)
		    (cond ((core-prim-typed-binding-descriptor.unsafe-variant-id rator.descr)
			   => %build-unsafe-variant-application)
			  (else
			   (%no-optimisation-possible))))
		   ((syntactic-binding-descriptor/lexical-typed-var? rator.descr)
		    (%build-typed-variable-application
		     (syntactic-binding-descriptor/lexical-typed-var.typed-variable-spec rator.descr)))
		   ((syntactic-binding-descriptor/global-typed-var? rator.descr)
		    (%build-typed-variable-application
		     (syntactic-binding-descriptor/global-typed-var.typed-variable-spec rator.descr)))
		   (else
		    (%no-optimisation-possible)))))
	  (else
	   ;;If we  are here  the rator  is a  non-identifier expression  returning a
	   ;;closure object with known signature.
	   (%no-optimisation-possible))))

  (case-define* %build-core-expression
    ((input-form.stx lexenv.run rator.psi rand*.psi)
     (%build-core-expression input-form.stx lexenv.run rator.psi rand*.psi #f))
    ((input-form.stx lexenv.run rator.psi rand*.psi retvals.sig)
     (let* ((rator.core		(psi.core-expr rator.psi))
	    (rand*.core		(map psi.core-expr rand*.psi))
	    (retvals.sig	(or retvals.sig (psi-application-retvals-signature input-form.stx lexenv.run rator.psi))))
       (make-psi input-form.stx
		 (build-application (syntax-annotation input-form.stx)
		   rator.core
		   rand*.core)
		 retvals.sig))))

;;; --------------------------------------------------------------------

  (define* (type-signature.match-arguments-against-operands input-form.stx lexenv.run lexenv.expand
							    argvals.sig rand*.sig)
    ;;In a closure  object application, compare the type signature  of the operator's
    ;;argvals  to the  type  signatures  of the  operands.   Return  a symbol  among:
    ;;exact-match, possible-match, no-match.
    ;;
    ;;ARGVALS.SIG is a "<type-signature>" instance representing the type signature of
    ;;a closure object's clause arguments.
    ;;
    ;;RAND*.SIG must be a list  of "<type-signature>" instances representing the type
    ;;signatures of the operands.
    ;;
    ;;In  this  loop  the  variable  STATE always  degrades:  from  "exact-match"  to
    ;;"possible-match" or "no-match"; from  "possible-match" to "no-match".  It never
    ;;upgrades.
    (define argvals.tags
      (type-signature-tags argvals.sig))
    (let loop ((state		'exact-match)
	       (argvals.tags	argvals.tags)
	       (rand*.sig	rand*.sig))
      (syntax-match argvals.tags (<top> <list>)
	(()
	 (if (null? rand*.sig)
	     ;;No more arguments and no more operands.  Good.
	     state
	   ;;No more arguments and leftover operands.  Bad.
	   'no-match))

	((<top> . ?argvals.tags)
	 (if (pair? rand*.sig)
	     ;;One argument matches one operand.  Good.
	     (loop state ?argvals.tags (cdr rand*.sig))
	   ;;More arguments and no more operands.  Bad.
	   'no-match))

	((?argval.tag . ?argvals.tags)
	 (if (pair? rand*.sig)
	     (let ((rand.tags (type-signature-tags (car rand*.sig))))
	       (syntax-match rand.tags (<top> <list>)
		 ((<top>)
		  ;;One argument possibly matches one operand.  Good.
		  (loop 'possible-match ?argvals.tags (cdr rand*.sig)))
		 ((?rand.type)
		  (cond ((type-identifier-super-and-sub? ?argval.tag ?rand.type lexenv.run)
			 ;;One argument matches one operand.  Good.
			 (loop state ?argvals.tags (cdr rand*.sig)))
			((type-identifier-super-and-sub? ?rand.type ?argval.tag lexenv.run)
			 ;;One argument possibly matches one operand.  Good.
			 ;;
			 ;;This may happen when the operand is built by an expression
			 ;;that does  not specify the  type with the full  depth; for
			 ;;example:  LIST  may  return   a  "<list>"  rather  than  a
			 ;;"<string*>" list sub-type.
			 (loop 'possible-match ?argvals.tags (cdr rand*.sig)))
			(else
			 'no-match)))
		 (<list>
		  ;;Operand with unspecified return values.  Will check at run-time.
		  (loop 'possible-match ?argvals.tags (cdr rand*.sig)))
		 (?rand-list-sub-type
		  ;;The  operand  returns  an   unspecified  number  of  values  with
		  ;;specified type.
		  (let ((ots (id->object-type-specification __who__ input-form.stx ?rand-list-sub-type lexenv.run)))
		    (if (and (list-type-spec? ots)
			     (type-identifier-super-and-sub? ?argval.tag (list-type-spec.type-id ots) lexenv.run))
			(loop 'possible-match ?argvals.tags (cdr rand*.sig))
		      'no-match)))))
	   ;;More arguments and no more operands.  Bad.
	   'no-match))

	(<list>
	 ;;Any number of operands of any type are accepted.
	 state)

	(?arg-list-sub-type
	 ;;Any number of operands of a specified type are accepted.
	 (if (pair? rand*.sig)
	     (let* ((argval.ots   (id->object-type-specification __who__ input-form.stx ?arg-list-sub-type lexenv.run))
		    (argitem.tag  (list-type-spec.type-id argval.ots)))
	       (let inner-loop ((state		state)
				(rand.tags	(type-signature-tags (car rand*.sig)))
				(rand*.sig	(cdr rand*.sig)))
		 (define (%recursion state)
		   (if (pair? rand*.sig)
		       (inner-loop state (type-signature-tags (car rand*.sig)) (cdr rand*.sig))
		     state))
		 (syntax-match rand.tags (<top> <list>)
		   ((<top>)
		    ;;One argument possibly matches one operand.  Good.
		    (%recursion 'possible-match))
		   ((?rand.type)
		    (if (type-identifier-super-and-sub? argitem.tag ?rand.type lexenv.run)
			;;One argument matches one operand.  Good.
			(%recursion state)
		      'no-match))
		   (<list>
		    ;;Operand with unspecified return values.  Will check at run-time.
		    (%recursion 'possible-match))
		   (?rand-list-sub-type
		    ;;The  operand  returns  an   unspecified  number  of  values  with
		    ;;specified type.
		    (let* ((rand.ots      (id->object-type-specification __who__ input-form.stx ?rand-list-sub-type lexenv.run))
			   (randitem.tag  (list-type-spec.type-id rand.ots)))
		      (if (type-identifier-super-and-sub? argitem.tag randitem.tag lexenv.run)
			  (%recursion 'possible-match)
			'no-match))))))
	   ;;No more operands.  Good.
	   state)))))

  #| end of module: PROCESS-CLOSURE-OBJECT-APPLICATION |# )


;;;; done

#| end of module: CHI-APPLICATION |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
;;eval: (put 'raise-compound-condition-object		'scheme-indent-function 1)
;;eval: (put 'assertion-violation/internal-error	'scheme-indent-function 1)
;;eval: (put 'with-who					'scheme-indent-function 1)
;;eval: (put '%error-mismatch-between-argvals-signature-and-operands-signature	'scheme-indent-function 1)
;;eval: (put '%error-number-of-operands-exceeds-maximum-arguments-count		'scheme-indent-function 1)
;;eval: (put '%error-number-of-operands-deceeds-minimum-arguments-count		'scheme-indent-function 1)
;;End:
