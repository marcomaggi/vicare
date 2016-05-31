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

(module (chi-application
	 chi-application/psi-first-operand
	 chi-overloaded-function-application)
  (define-module-who chi-application)


;;;; helpers

(define (%psi-rator-is-values? rator.psi)
  ;;Given a psi object  representing an application operator: return true  if it is a
  ;;reference to the core primitive VALUES; otherwiser return false.
  ;;
  (cond ((core-expr.primref (psi.core-expr rator.psi))
	 => (lambda (prim-name)
	      (eq? prim-name 'values)))
	(else #f)))


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
  (syntax-match input-form.stx (call-with-values lambda lambda/checked)
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

    ((call-with-values ?producer (lambda . ?consumer-stuff))
     (options::typed-language-enabled?)
     (chi-call-with-values-application/stx-operands input-form.stx lexenv.run lexenv.expand
						    ?producer ?consumer-stuff))
    ((call-with-values ?producer (lambda/checked . ?consumer-stuff))
     (options::typed-language-enabled?)
     (chi-call-with-values-application/stx-operands input-form.stx lexenv.run lexenv.expand
						    ?producer ?consumer-stuff))

    ((?rator ?rand* ...)
     ;;The input form is a common function application like:
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
	(make-application-operands-signature-condition (list rand.sig)))))

  (define (%error-mismatch-between-args-signature-and-operands-signature input-form.stx
									 arguments-signature* operands-signature)
    ;;ARGUMENTS-SIGNATURE*   must  be   a   list   of  "<type-signature>"   instances
    ;;representing the closure object's arguments signatures.
    ;;
    ;;OPERANDS-SIGNATURE must  be an  insance of "<type-signature>"  representing the
    ;;signature of the operands, when each operand returns a single value.
    ;;
    (raise-compound-condition-object 'chi-application
      "expand-time mismatch between closure object's arguments signatures and operands signature"
      input-form.stx
      (condition (make-expand-time-type-signature-violation)
		 (common input-form.stx arguments-signature* operands-signature))))

  (define (%warning-mismatch-between-args-signature-and-operands-signature input-form.stx
									   arguments-signature* operands-signature)
    ;;ARGUMENTS-SIGNATURE*   must  be   a   list   of  "<type-signature>"   instances
    ;;representing the closure object's arguments signatures.
    ;;
    ;;OPERANDS-SIGNATURE must  be an  insance of "<type-signature>"  representing the
    ;;signature of the operands, when each operand returns a single value.
    ;;
    (when (options::warn-about-compatible-operands-signature-in-procedure-application)
      (raise-compound-condition-object/continuable 'chi-application
	"expand-time mismatch between closure object's arguments signatures and operands signature"
	input-form.stx
	(condition (make-expand-time-type-signature-warning)
		   (common input-form.stx arguments-signature* operands-signature)))))

  (define (common input-form.stx arguments-signature* operands-signature)
    (syntax-match input-form.stx ()
      ((?rator . ?rand*)
       (condition (make-syntax-violation input-form.stx ?rator)
		  (make-application-operator-expression-condition ?rator)
		  (make-application-operands-expressions-condition ?rand*)
		  (make-procedure-arguments-signatures-condition arguments-signature*)
		  (make-application-operands-signature-condition operands-signature)))))

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
  (case-type-signature-full-structure (psi.retvals-signature rator.psi)
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

    (<list>/<list-of-spec>
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

(module (chi-application/psi-first-operand)

  (define* (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					      rator.stx {first-rand.psi psi?} other-rand*.stx)
    ;;This  is an  entry  point, not  a sub-routine  of  CHI-APPLICATION.  Expand  an
    ;;operator application form; it is called when the application to process has the
    ;;format:
    ;;
    ;;   (?rator ?first-rand ?other-rand ...)
    ;;
    ;;and ?FIRST-RAND has already been expanded.  Here we know that the input form is
    ;;a special syntax like IS-A?,  SLOT-REF, SLOT-SET!, METHOD-CALL; so the operator
    ;;will expand into a predicate, accessor, mutator or method.
    ;;
    (define (%no-overloaded-function)
      (chi-application/psi-first-operand/no-overload input-form.stx lexenv.run lexenv.expand
						     rator.stx first-rand.psi other-rand*.stx))
    (if (identifier? rator.stx)
	(cond ((cond ((id->label rator.stx)
		      => (lambda (rator.lab)
			   (let ((rator.des (label->syntactic-binding-descriptor rator.lab lexenv.run)))
			     (case (syntactic-binding-descriptor.type rator.des)
			       ((local-overloaded-function)
				(syntactic-binding-descriptor/local-overloaded-function.ofs rator.des))
			       ((global-overloaded-function)
				(syntactic-binding-descriptor/global-overloaded-function.ofs rator.des))
			       (else #f)))))
		     (else
		      (syntax-violation __who__
			"unbound operator identifier" input-form.stx rator.stx)))
	       => (lambda (rator.ofs)
		    (let ((other-rand*.psi (chi-expr* other-rand*.stx lexenv.run lexenv.expand)))
		      (chi-overloaded-function-application/psi-rands input-form.stx lexenv.run lexenv.expand
								     rator.ofs rator.stx (cons first-rand.psi other-rand*.psi)))))
	      (else
	       (%no-overloaded-function)))
      (%no-overloaded-function)))

  (define (chi-application/psi-first-operand/no-overload input-form.stx lexenv.run lexenv.expand
							 rator.stx first-rand.psi other-rand*.stx)
    (let* ((rator.psi (while-not-expanding-application-first-subform
		       (chi-expr rator.stx lexenv.run lexenv.expand)))
	   (rator.sig (psi.retvals-signature rator.psi)))

      (define (%build-default-application)
	;;Build a core language expression to apply  the rator to the rands; return a
	;;PSI  struct.  This  is an  application  form in  standard (untyped)  Scheme
	;;language.   Here  we  know  nothing   about  the  values  returned  by  the
	;;application.
	;;
	(let* ((rator.core       (psi.core-expr rator.psi))
	       (first-rand.core  (psi.core-expr first-rand.psi))
	       (other-rand*.psi  (chi-expr* other-rand*.stx lexenv.run lexenv.expand))
	       (other-rand*.core (map psi.core-expr other-rand*.psi)))
	  (make-psi input-form.stx
	    (build-application (syntax-annotation input-form.stx)
		rator.core
	      (cons first-rand.core other-rand*.core)))))

      (case-type-signature-full-structure rator.sig
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

	(<list>/<list-of-spec>
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

  #| end of module |# )


;;;; chi procedures: special CALL-WITH-VALUES handling

(module (chi-call-with-values-application/stx-operands)
  (define-module-who chi-call-with-values-application/stx-operands)

  (define* (chi-call-with-values-application/stx-operands input-form.stx lexenv.run lexenv.expand
							  producer.stx consumer-stuff.stx)
    ;;Special  handling for  CALL-WITH-VALUES  applications when  the  producer is  a
    ;;LAMBDA/CHECKED syntax  and the  typed language is  enabled.  When  possible, we
    ;;perform type propagation from the return  values of the producer to the formals
    ;;of the consumer.  Return a psi object.
    ;;
    (receive (consumer-standard-formals.stx consumer-clause-signature consumer-body*.stx)
	(syntax-match consumer-stuff.stx ()
	  ((?formals ?body0 ?body* ...)
	   (receive (consumer-standard-formals.stx consumer-clause-signature)
	       (syntax-object.parse-typed-clambda-clause-formals ?formals (<untyped>-ots))
	     (values consumer-standard-formals.stx consumer-clause-signature (cons ?body0 ?body*))))
	  (_
	   (syntax-violation __who__ "invalid consumer syntax" consumer-stuff.stx #f)))
      ;;CONSUMER-CLAUSE-SIGNATURE is an instance of "<lambda-signature>".
      (let* ((producer.psi		(chi-expr producer.stx lexenv.run lexenv.expand))
	     (producer.ots		(%validate-producer input-form.stx producer.psi)))
	(if (closure-type-spec? producer.ots)
	    ;;Good, we can attempt type propagation.   Here we know that the producer
	    ;;is a thunk.
	    (%chi-thunk-application input-form.stx lexenv.run lexenv.expand
				    consumer-standard-formals.stx consumer-clause-signature consumer-body*.stx
				    producer.psi producer.ots)
	  ;;We can do nothing here.  Just build the return value.
	  (let ((consumer.psi (chi-lambda/typed/parsed-formals input-form.stx lexenv.run lexenv.expand
							       consumer-standard-formals.stx consumer-clause-signature
							       consumer-body*.stx)))
	    (%build-core-expr input-form.stx producer.psi consumer.psi))))))

;;; --------------------------------------------------------------------

  (define* (%chi-thunk-application input-form.stx lexenv.run lexenv.expand
				   consumer-standard-formals.stx consumer-clause-signature consumer-body*.stx
				   producer.psi producer.ots)
    (let* ((producer-retvals.sig	(case-lambda-signature.retvals (closure-type-spec.signature producer.ots)))
	   (consumer-argvals.sig	(lambda-signature.argvals consumer-clause-signature))
	   (match-symbol		(let ((clear-argvals.sig (type-signature.untyped-to-top consumer-argvals.sig)))
					  (case-type-signature-full-structure producer-retvals.sig
					    (<no-return>
					     'no-return)
					    (else
					     (type-signature.match-arguments-against-operands clear-argvals.sig producer-retvals.sig))))))
      (case match-symbol
	((exact-match)
	 ;;Here we use CHI-LAMBDA/TYPED/PARSED-FORMALS.
	 (%build-thunk-core-expr input-form.stx lexenv.run lexenv.expand
				 producer.psi producer-retvals.sig
				 consumer-standard-formals.stx consumer-clause-signature consumer-body*.stx
				 chi-lambda/typed/parsed-formals))
	((possible-match)
	 ;;Here we use CHI-LAMBDA/CHECKED/PARSED-FORMALS.
	 (%build-thunk-core-expr input-form.stx lexenv.run lexenv.expand
				 producer.psi producer-retvals.sig
				 consumer-standard-formals.stx consumer-clause-signature consumer-body*.stx
				 chi-lambda/checked/parsed-formals))
	((no-return)
	 ;;We expand  the consumer forms for  these reasons: the side  effects of the
	 ;;expansion; to catch expand-time errors in  the source code.  We throw away
	 ;;the result because we do not need it.
	 ;;
	 ;;NOTE Is this what we desire?  Yes,  we want to catch expand-time errors in
	 ;;this code even if we discard it. (Marco Maggi; Tue May 3, 2016)
	 (chi-lambda/typed/parsed-formals input-form.stx lexenv.run lexenv.expand
					  consumer-standard-formals.stx consumer-clause-signature
					  consumer-body*.stx)
	 (make-psi input-form.stx
	   (build-application (syntax-annotation input-form.stx)
	       (psi.core-expr producer.psi)
	     '())
	   producer-retvals.sig))
	((no-match)
	 (raise
	  (condition (make-expand-time-type-signature-violation)
		     (make-who-condition __module_who__)
		     (make-message-condition "type signature mismatch between producer return values and consumer expected arguments")
		     (make-syntax-violation input-form.stx #f)
		     (make-returned-type-signature-condition producer-retvals.sig)
		     (make-expected-type-signature-condition consumer-argvals.sig))))
	(else
	 (assertion-violation __who__ "internal error, invalid match symbol" input-form.stx match-symbol)))))

;;; --------------------------------------------------------------------

  (define (%build-thunk-core-expr input-form.stx lexenv.run lexenv.expand
				  producer.psi producer-retvals.sig
				  consumer-standard-formals.stx consumer-clause-signature consumer-body*.stx
				  chi-lambda)
    (let* ((consumer.psi	(let* ((consumer-retvals.sig	(lambda-signature.retvals consumer-clause-signature))
				       (consumer-argvals.sig	(lambda-signature.argvals consumer-clause-signature))
				       (consumer-retvals.sig	(type-signature.untyped-to-top consumer-retvals.sig))
				       (consumer-argvals.sig	(type-signature.type-propagation consumer-argvals.sig
												 producer-retvals.sig))
				       (clause-signature	(make-lambda-signature consumer-retvals.sig
										       consumer-argvals.sig)))
				  (chi-lambda input-form.stx lexenv.run lexenv.expand
					      consumer-standard-formals.stx clause-signature
					      consumer-body*.stx)))
	   ;;CHI-LAMBDA might have  performed type propagation from the  last form of
	   ;;the consumer body into the consumer  return values.  So we reextract the
	   ;;consumer retvals.
	   (application.sig	(case-lambda-signature.retvals
				 (closure-type-spec.signature
				  (car (type-signature.object-type-specs (psi.retvals-signature consumer.psi)))))))
      (%build-core-expr input-form.stx producer.psi consumer.psi application.sig)))

  (case-define %build-core-expr
    ((input-form.stx producer.psi consumer.psi)
     (%build-core-expr input-form.stx producer.psi consumer.psi (make-type-signature/fully-unspecified)))
    ((input-form.stx producer.psi consumer.psi application.sig)
     (make-psi input-form.stx
       (build-application (syntax-annotation input-form.stx)
	   (build-primref no-source 'call-with-values)
	 (list (psi.core-expr producer.psi)
	       (psi.core-expr consumer.psi)))
       application.sig)))

;;; --------------------------------------------------------------------

  (define (%validate-producer input-form.stx producer.psi)
    ;;Validate PRODUCER.PSI as returning a single value compatible with "<producer>".
    ;;If successful return an instance  of "<object-type-spec>" representing the type
    ;;of the single value; otherwise raise an exception.
    ;;
    (define producer.sig
      (psi.retvals-signature producer.psi))
    (define (%handle-error message)
      (raise
       (condition (make-expand-time-type-signature-violation)
		  (make-who-condition __module_who__)
		  (make-message-condition message)
		  (make-syntax-violation input-form.stx (psi.input-form producer.psi))
		  (make-application-operand-signature-condition producer.sig))))
    (define (%error-non-procedure)
      (%handle-error "expression used as producer operand is typed as returning a non-procedure value"))
    (define (%error-invalid-number-of-return-values)
      (%handle-error "expression used as producer operand returns zero, two or more values"))
    (define (%return-if-compatible-with-procedure item.ots)
      (cond ((closure-type-spec? item.ots)
	     (if (closure-type-spec.thunk? item.ots)
		 item.ots
	       (%handle-error "expression used as producer operand is typed as procedure but not a thunk")))
	    ((or (<procedure>-ots?   item.ots)
		 (<top>-ots?         item.ots))
	     item.ots)
	    (else
	     (%error-non-procedure))))
    (case-type-signature-full-structure* producer.sig
      (<no-return>
       ;;The operand expression will not return.
       (when (options::warn-about-not-returning-expressions)
	 (raise-continuable
	  (condition (make-expand-time-type-signature-warning)
		     (make-who-condition __module_who__)
		     (make-message-condition "expression used as operand in procedure application is typed as not returning")
		     (make-syntax-violation input-form.stx (psi.input-form producer.psi))
		     (make-application-operand-signature-condition producer.sig))))
       (<top>-ots))

      ((<void>)
       ;;The expression is marked as returning void.
       (%handle-error "expression used as operand in procedure application is typed as returning void"))

      ((single-value)
       ;;Single return value.  Good.
       => %return-if-compatible-with-procedure)

      ;; ------------------------------------------------------------

      (<list>
       ;;Unspecified  number of  return  values,  of unspecified  type.   We let  the
       ;;compiler insert the run-time checks for the single return value case.
       (<top>-ots))

      (<null>
       (%error-invalid-number-of-return-values))

      (<list-of-spec>
       ;;Unspecified number  of return values,  of known  type.  We let  the compiler
       ;;insert the run-time checks for the single return value case.
       => (lambda (producer.ots)
	    (%return-if-compatible-with-procedure (list-of-type-spec.item-ots producer.ots))))

      ;; ------------------------------------------------------------

      (<pair-spec>
       ;;One or more return values, of known type.
       => (lambda (producer.ots)
	    (let ((item-cdr.ots (pair-type-spec.cdr-ots producer.ots)))
	      (unless (or (<list>-ots? item-cdr.ots)
			  (<null>-ots? item-cdr.ots)
			  (list-of-type-spec? item-cdr.ots))
		(%error-invalid-number-of-return-values)))
	    (%return-if-compatible-with-procedure (pair-type-spec.car-ots producer.ots))))

      (<nelist>
       ;;One or more return values, of  unspecified type.  We let the compiler insert
       ;;the run-time checks for the single return value case.
       (<top>-ots))

      (<list-spec>
       ;;Known number of values, of known type.
       => (lambda (producer.ots)
	    (let ((item*.ots (list-type-spec.item-ots* producer.ots)))
	      (if (= 1 (length item*.ots))
		  (%return-if-compatible-with-procedure (car item*.ots))
		(%error-invalid-number-of-return-values)))))

      ;; ------------------------------------------------------------

      (else
       (assertion-violation __module_who__ "invalid type signature" producer.sig))))

  #| end of module: CHI-CALL-WITH-VALUES-APPLICATION/STX-OPERANDS |# )


;;;; chi procedures: overloaded function application

(module (chi-overloaded-function-application
	 chi-overloaded-function-application/psi-rands)
  (import CLOSURE-APPLICATION-ERRORS)
  (define-module-who chi-overloaded-function-application)

  (define* (chi-overloaded-function-application input-form.stx lexenv.run lexenv.expand operator.ofs)
    (syntax-match input-form.stx ()
      ((?rator ?rand* ...)
       (let* ((rand*.stx	?rand*)
	      (rand*.psi	(chi-expr* rand*.stx lexenv.run lexenv.expand)))
	 (chi-overloaded-function-application/psi-rands input-form.stx lexenv.run lexenv.expand
							operator.ofs ?rator rand*.psi)))
      (_
       (syntax-violation __who__
	 "invalid overloaded function application syntax" input-form.stx #f))))

  (define* (chi-overloaded-function-application/psi-rands input-form.stx lexenv.run lexenv.expand
							  operator.ofs rator.stx rand*.psi)
    (let ((rands.sig (%validate-operands-for-single-return-value input-form.stx rand*.psi)))
      (cond ((%search-applicable-function operator.ofs rands.sig)
	     => (lambda (P)
		  (let ((spec.id		(car P))
			(spec.lambda-sig	(cdr P)))
		    (let ((spec.psi (chi-expr spec.id lexenv.run lexenv.expand)))
		      (make-psi input-form.stx
			(build-application (syntax-annotation input-form.stx)
			    (psi.core-expr spec.psi)
			  (map psi.core-expr rand*.psi))
			(lambda-signature.retvals spec.lambda-sig))))))
	    (else
	     (raise
	      (condition
		(make-expand-time-type-signature-violation)
		(make-who-condition (syntax->datum rator.stx))
		(make-message-condition
		 "no matching specialised function in overloaded function application")
		(make-syntax-violation input-form.stx #f)
		(make-application-operand-signature-condition rands.sig)))))))

  (define* (%validate-operands-for-single-return-value input-form.stx rand*.psi)
    ;;In  the context  of INPUT-FORM.STX  the  RAND*.PSI argument  is a  list of  psi
    ;;instances  representing  the  already   expanded  operand  expressions  for  an
    ;;overloaded function application.   Check that such expressions  return a single
    ;;value.   Return  an  instance   of  "<type-signature>"  representing  the  type
    ;;signature of the operands.
    ;;
    (make-type-signature
     (map (lambda (rand.psi)
	    (define rand.sig
	      (psi.retvals-signature rand.psi))
	    (case-type-signature-full-structure rand.sig
	      ((single-value)
	       ;;Single return value.  Good.
	       => (lambda (rand.ots) rand.ots))
	      (else
	       (raise
		(condition
		  (make-expand-time-type-signature-violation)
		  (make-who-condition __module_who__)
		  (make-message-condition
		   "expression used as operand in overloaded function application is not typed to return a single value")
		  (make-syntax-violation input-form.stx (psi.input-form rand.psi))
		  (make-application-operand-signature-condition rand.sig))))))
       rand*.psi)))

  (define (%search-applicable-function operator.ofs rands.sig)
    (let loop ((selected.id		#f)
	       (selected.lambda-sig	#f)
	       (selected.formals-sig	#f)
	       (spec*.lambda-sig	(overloaded-function-spec.signature* operator.ofs))
	       (spec*.id		(overloaded-function-spec.id*        operator.ofs)))
      (cond ((pair? spec*.lambda-sig)
	     (let* ((prospective.id		(car spec*.id))
		    (prospective.lambda-sig	(car spec*.lambda-sig))
		    (prospective.formals-sig	(lambda-signature.argvals prospective.lambda-sig)))
	       (define (%grab-new-selection)
		 (loop prospective.id prospective.lambda-sig prospective.formals-sig (cdr spec*.lambda-sig) (cdr spec*.id)))
	       (define (%keep-old-selection)
		 (loop    selected.id    selected.lambda-sig    selected.formals-sig (cdr spec*.lambda-sig) (cdr spec*.id)))
	       (if (eq? 'exact-match (type-signature.match-arguments-against-operands prospective.formals-sig rands.sig))
		   (if selected.id
		       (if (type-signature.matching-super-and-sub? (lambda-signature.argvals selected.lambda-sig)
								   prospective.formals-sig)
			   (%grab-new-selection)
			 (%keep-old-selection))
		     (%grab-new-selection))
		 (%keep-old-selection))))
	    (selected.id
	     (cons selected.id selected.lambda-sig))
	    (else #f))))

  #;(define (%search-applicable-function operator.ofs rands.sig)
    (let ((spec*.id		(overloaded-function-spec.id*        operator.ofs))
	  (spec*.lambda-sig	(overloaded-function-spec.signature* operator.ofs)))
      (let loop ((spec*.lambda-sig	spec*.lambda-sig)
		 (spec*.id		spec*.id))
	(if (pair? spec*.lambda-sig)
	    (if (let ((spec.formals-sig (lambda-signature.argvals (car spec*.lambda-sig))))
		  (eq? 'exact-match (type-signature.match-arguments-against-operands spec.formals-sig rands.sig)))
		(cons (car spec*.id) (car spec*.lambda-sig))
	      (loop (cdr spec*.lambda-sig) (cdr spec*.id)))
	  #f))))

  #| end of module: CHI-OVERLOADED-FUNCTION-APPLICATION |# )


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
	      (define (common-condition-objects message)
		(condition
		  (make-who-condition __module_who__)
		  (make-message-condition message)
		  (make-syntax-violation input-form.stx (psi.input-form rand.psi))
		  (make-application-operand-signature-condition rand.sig)))
	      (define (%handle-error message rv)
		(%error (lambda () (common-condition-objects message)) rv))
	      (case-type-signature-full-structure rand.sig
		(<no-return>
		 ;;The operand expression will not return.
		 (when (options::warn-about-not-returning-expressions)
		   (raise-continuable
		    (condition (make-expand-time-type-signature-warning)
			       (common-condition-objects
				"expression used as operand in procedure application is typed as not returning"))))
		 (<top>-ots))
		((<void>)
		 ;;The expression is marked as returning void.
		 (%handle-error "expression used as operand in procedure application is typed as returning void"
				(<top>-ots)))
		((single-value)
		 ;;Single return value.  Good.
		 => (lambda (rand.ots) rand.ots))
		(<list-of-spec>
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

    (define* (%match-case-lambda-signature-against-operands input-form.stx lexenv.run lexenv.expand
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
      (if (%psi-rator-is-values? rator.psi)
	  ;;VALUES is special because it accepts  any number of arguments of any type
	  ;;and it  returns a different number  of values depending on  the number of
	  ;;operands.  So we handle it specially.
	  (chi-values-application input-form.stx rator.psi rand*.psi rands.sig)
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
	     ;;There is  a clause  that exactly  matches the  operands.  When  we are
	     ;;here: we know that SELECTED-CLAUSE-SIGNATURE* holds a single item.
	     (case-expander-language
	       ((typed)
		(%chi-application-with-matching-signature input-form.stx lexenv.run lexenv.expand
							  rator.psi rand*.psi rands.sig
							  (lambda-signature.retvals (car selected-clause-signature*))))
	       ((default)
		(%default-core-expr))
	       ((strict-r6rs)
		(%default-core-expr))))
	    ((possible-match)
	     ;;There  is at  least  one clause  with  a possible  match.   It is  not
	     ;;possible to fully  validate the signatures at expand-time;  we rely on
	     ;;run-time checking.
	     (case-expander-language
	       ((typed)
		(%chi-application-with-compatible-signature input-form.stx lexenv.run lexenv.expand
							    rator.psi rand*.psi rands.sig
							    %default-core-expr))
	       ((default)
		(%default-core-expr))
	       ((strict-r6rs)
		(%default-core-expr))))
	    (else
	     ;;There are no matching clauses, not even possible matches.  Arguments and
	     ;;operands do *not* match at expand-time.
	     #;(assert (null? selected-clause-signature*))
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
		  (%default-core-expr)))))))))

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
      (define (%chi-identifier)
	(%chi-application-of-identifier-rator input-form.stx lexenv.run lexenv.expand
					      rator.psi rand*.psi rands.sig application-retvals.sig))
      (cond
       ((identifier? (psi.input-form rator.psi))
	(cond ((core-expr.primref (psi.core-expr rator.psi))
	       ;;The operator is a core  primitive reference, PRIM-NAME is the symbol
	       ;;representing its public name.
	       => (lambda (prim-name)
		    (case prim-name
		      ((cons foldable-cons)
		       (chi-cons-application input-form.stx rator.psi rand*.psi rands.sig))
		      ((list foldable-list <nelist>-constructor)
		       (chi-list-application input-form.stx rator.psi rand*.psi rands.sig))
		      ((vector foldable-vector <nevector>-constructor)
		       (chi-vector-application input-form.stx rator.psi rand*.psi rands.sig))
		      ;;
		      ((car)	(chi-car-application input-form.stx rator.psi rand*.psi rands.sig  'car))
		      ((cdr)	(chi-cdr-application input-form.stx rator.psi rand*.psi rands.sig  'cdr))
		      (($car)	(chi-car-application input-form.stx rator.psi rand*.psi rands.sig '$car))
		      (($cdr)	(chi-cdr-application input-form.stx rator.psi rand*.psi rands.sig '$cdr))
		      ;;
		      ((condition)
		       (chi-condition-application		input-form.stx rator.psi rand*.psi rands.sig))
		      ((call-with-values)
		       (chi-call-with-values-application	input-form.stx rator.psi rand*.psi rands.sig))
		      ((apply)
		       (chi-apply-application		input-form.stx rator.psi rand*.psi rands.sig))
		      ((dynamic-wind)
		       (chi-dynamic-wind-application	input-form.stx rator.psi rand*.psi rands.sig))
		      (else
		       ;;The operator  is a core primitive  reference.  The primitive
		       ;;is not special.
		       (%chi-identifier)))))
	      (else
	       ;;The operator is  a variable reference to  some non-primitive closure
	       ;;object.
	       (%chi-identifier))))
       (else
	;;If we are here the rator is a non-identifier expression returning a closure
	;;object with known signature.
	(%build-core-expression input-form.stx rator.psi rand*.psi application-retvals.sig))))

    (define* (%chi-application-of-identifier-rator input-form.stx lexenv.run lexenv.expand
						   rator.psi rand*.psi
						   rands.sig {application-retvals.sig type-signature?})
      (define rator.id (psi.input-form rator.psi))
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
			   (cond ((core-prim-type-spec.replacements
				   (syntactic-binding-descriptor/core-prim-typed.core-prim-type-spec rator.descr))
				  => (lambda (replacements)
				       (%select-application-replacement input-form.stx lexenv.run lexenv.expand
									rator.psi rand*.psi
									rands.sig application-retvals.sig
									replacements)))
				 (else
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
	  (let* ((repl.id		(vector-ref replacements idx))
		 (repl.psi		(chi-expr repl.id lexenv.run lexenv.expand))
		 (repl.ots		(let ((repl.sig (psi.retvals-signature repl.psi)))
					  (car (type-signature.object-type-specs repl.sig)))))
	    (if (closure-type-spec? repl.ots)
		;;Examine a typed closure object as possible replacement.
		(let ((repl.clambda-sig (closure-type-spec.signature repl.ots)))
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
			 (loop (fxadd1 idx) len))))
	      ;;This  replacement is  not a  typed closure  object.  For  example, it
	      ;;could be an UNtyped core primitive.
	      (begin
		(raise-continuable
		 (condition (make-warning)
			    (make-who-condition __module_who__)
			    (make-message-condition
			     "syntactic identifier listed as typed function replacement is not bound to a typed function")
			    (make-syntax-violation input-form.stx (psi.input-form rator.psi))
			    (make-irritants-condition (list repl.id))))
		(loop (fxadd1 idx) len)))))))

    #| end of module: %CHI-APPLICATION-WITH-MATCHING-SIGNATURE |# )

;;; --------------------------------------------------------------------

  (define* (%chi-application-with-compatible-signature input-form.stx lexenv.run lexenv.expand
						       rator.psi rand*.psi rands.sig
						       default-application-maker)
    (cond
     ((identifier? (psi.input-form rator.psi))
      (cond ((core-expr.primref (psi.core-expr rator.psi))
	     ;;The operator  is a core  primitive reference, PRIM-NAME is  the symbol
	     ;;representing its public name.
	     => (lambda (prim-name)
		  (case prim-name
		    ;;NOTE We do *not* do the following primitives here:
		    ;;
		    ;;  cons foldable-cons
		    ;;  list foldable-list <nelist>-constructor
		    ;;  vector foldable-vector <nevector>-constructor
		    ;;  chi-vector-application
		    ;;
		    ;;because they accept operands of  any type, so they always match
		    ;;exactly if the number of operands is right.
		    (( car)	(chi-car-application input-form.stx rator.psi rand*.psi rands.sig  'car))
		    (($car)	(chi-car-application input-form.stx rator.psi rand*.psi rands.sig '$car))
		    (( cdr)	(chi-cdr-application input-form.stx rator.psi rand*.psi rands.sig  'cdr))
		    (($cdr)	(chi-cdr-application input-form.stx rator.psi rand*.psi rands.sig '$cdr))
		    ;;
		    ((condition)
		     (chi-condition-application			input-form.stx rator.psi rand*.psi rands.sig))
		    ((call-with-values)
		     (chi-call-with-values-application		input-form.stx rator.psi rand*.psi rands.sig))
		    ((apply)
		     (chi-apply-application			input-form.stx rator.psi rand*.psi rands.sig))
		    ((dynamic-wind)
		     (chi-dynamic-wind-application		input-form.stx rator.psi rand*.psi rands.sig))
		    (else
		     ;;The operator is a reference to a non-special core primitive.
		     (default-application-maker)))))
	    (else
	     ;;The operator is an identifier  reference to some non-primitive closure
	     ;;object.
	     (default-application-maker))))
     (else
      ;;If we are  here the rator is a non-identifier  expression returning a closure
      ;;object with known signature.
      (default-application-maker))))

;;; --------------------------------------------------------------------

  (define* (%build-core-expression input-form.stx rator.psi rand*.psi {application-retvals.sig type-signature?})
    (make-psi input-form.stx
      (build-application (syntax-annotation input-form.stx)
	  (psi.core-expr rator.psi)
	(map psi.core-expr rand*.psi))
      application-retvals.sig))

  #| end of module |# )


;;;; special applications: CONS, FOLDABLE-CONS

(define (chi-cons-application input-form.stx
			      rator.psi rand*.psi rands.sig)
  ;;The input form has the syntax:
  ;;
  ;;   (cons ?car ?cdr)
  ;;
  ;;We have  already validated  the number  and type of  the operands.   The argument
  ;;RATOR.PSI  represents the  expanded CONS  or FOLDABLE-CONS  syntactic identifier.
  ;;The argument RAND*.PSI  is a list of  two items, each having a  single value type
  ;;signature.  The argument RANDS.SIG is a "<type-signature>" representing the types
  ;;of the operands.
  ;;
  ;;The application  of CONS is  special because we want  the expression to  return a
  ;;type signature describing a "<pair-type-spec>".
  ;;
  (let ((car.core		(psi.core-expr (car  rand*.psi)))
	(cdr.core		(psi.core-expr (cadr rand*.psi)))
	(application.sig	(let* ((rands.specs	(type-signature.object-type-specs rands.sig))
				       (car.ots		(car  rands.specs))
				       (cdr.ots		(cadr rands.specs)))
				  (make-type-signature/single-value (make-pair-type-spec car.ots cdr.ots)))))
    (make-psi input-form.stx
      (build-application (syntax-annotation input-form.stx)
	  (psi.core-expr rator.psi)
	(list car.core cdr.core))
      application.sig)))


;;;; special applications: LIST, FOLDABLE-LIST, <NELIST>-CONSTRUCTOR

(define (chi-list-application input-form.stx
			      rator.psi rand*.psi rands.sig)
  ;;The input form has the syntax:
  ;;
  ;;   (list ?expr ...)
  ;;
  ;;We have  already validated  the number  and type of  the operands.   The argument
  ;;RATOR.PSI  represents the  expanded LIST,  FOLDABLE-LIST or  <NELIST>-CONSTRUCTOR
  ;;syntactic  identifier.  The  argument RAND*.PSI  is  a (possibly  empty) list  of
  ;;items, each  having a single value  type signature.  The argument  RANDS.SIG is a
  ;;"<type-signature>" representing the types of the operands.
  ;;
  ;;The application  of LIST is  special because we want  the expression to  return a
  ;;type signature describing a sub-type of "<list>".
  ;;
  (let ((application.sig (let ((rands.specs (type-signature.object-type-specs rands.sig)))
			   (if (null? rands.specs)
			       (make-type-signature/single-null)
			     (make-type-signature/single-value (make-list-type-spec rands.specs))))))
    (make-psi input-form.stx
      (build-application (syntax-annotation input-form.stx)
	  (psi.core-expr rator.psi)
	(map psi.core-expr rand*.psi))
      application.sig)))


;;;; special applications: CAR, $CAR

(module (chi-car-application)
  (define-module-who chi-car-application)

  (define* (chi-car-application input-form.stx
				rator.psi rand*.psi rands.sig original-prim-name)
    ;;The input form has the syntax:
    ;;
    ;;   (car ?expr)
    ;;
    ;;We have already  validated the number of operands and  determined that the type
    ;;of the operand is either an  exact or compatible match.  The argument RATOR.PSI
    ;;represents  the  expanded  CAR  or $CAR  syntactic  identifier.   The  argument
    ;;RAND*.PSI is a list of single item,  having a single value type signature.  The
    ;;argument  RANDS.SIG  is  a  "<type-signature>" representing  the  type  of  the
    ;;operand.
    ;;
    ;;The application of  CAR is special because  we want to extract the  type of the
    ;;returned value from the type of the operand.
    ;;
    (receive (prim-name application.sig)
	(%single-value-operand-signature->application-signature input-form.stx rands.sig original-prim-name)
      (make-psi input-form.stx
	(build-application (syntax-annotation input-form.stx)
	    (build-primref no-source prim-name)
	  (list (psi.core-expr (car rand*.psi))))
	application.sig)))

  (define (%single-value-operand-signature->application-signature input-form.stx rands.sig original-prim-name)
    (let ((rand.ots (car (type-signature.object-type-specs rands.sig))))
      (cond ((list-of-type-spec? rand.ots)
	     ;;This is not  an exact match: the  operand might be null.   So we apply
	     ;;the original primitive.
	     (values original-prim-name (make-type-signature/single-value (list-of-type-spec.item-ots rand.ots))))

	    ((list-type-spec? rand.ots)
	     (values '$car (make-type-signature/single-value (car (list-type-spec.item-ots* rand.ots)))))

	    ((pair-of-type-spec? rand.ots)
	     (values '$car (make-type-signature/single-value (pair-of-type-spec.item-ots rand.ots))))

	    ((pair-type-spec? rand.ots)
	     (values '$car (make-type-signature/single-value (pair-type-spec.car-ots rand.ots))))

	    ((or (<nelist>-ots? rand.ots)
		 (<pair>-ots?   rand.ots))
	     (values '$car (make-type-signature/single-top)))

	    ((or (object-type-spec.compatible-super-and-sub? (<nelist>-ots) rand.ots)
		 (object-type-spec.compatible-super-and-sub? (<pair>-ots)   rand.ots))
	     ;;This is not  an exact match: the  operand might be null.   So we apply
	     ;;the original primitive.
	     (values original-prim-name (make-type-signature/single-top)))

	    (else
	     ;;This should never happen.
	     (assertion-violation __module_who__
	       "internal error, core primitive operand of wrong type" input-form.stx rands.sig)))))

  #| end of module: CHI-CAR-APPLICATION |# )


;;;; special applications: CDR, $CDR

(module (chi-cdr-application)
  (define-module-who chi-cdr-application)

  (define (chi-cdr-application input-form.stx
			       rator.psi rand*.psi rands.sig original-prim-name)
    ;;The input form has the syntax:
    ;;
    ;;   (cdr ?expr)
    ;;
    ;;We have already  validated the number of operands and  determined that the type
    ;;of the operand is either an  exact or compatible match.  The argument RATOR.PSI
    ;;represents  the  expanded  CDR  or $CDR  syntactic  identifier.   The  argument
    ;;RAND*.PSI is a list of single item,  having a single value type signature.  The
    ;;argument  RANDS.SIG  is  a  "<type-signature>" representing  the  type  of  the
    ;;operand.
    ;;
    ;;The application of  CDR is special because  we want to extract the  type of the
    ;;returned value from the type of the operand.
    ;;
    (receive (prim-name application.sig)
	(%single-value-operand-signature->application-signature input-form.stx rands.sig original-prim-name)
      (make-psi input-form.stx
	(build-application (syntax-annotation input-form.stx)
	    (build-primref no-source prim-name)
	  (list (psi.core-expr (car rand*.psi))))
	application.sig)))

  (define (%single-value-operand-signature->application-signature input-form.stx rands.sig original-prim-name)
    (let ((rand.ots (car (type-signature.object-type-specs rands.sig))))
      (cond ((list-of-type-spec? rand.ots)
	     ;;This is not  an exact match: the  operand might be null.   So we apply
	     ;;the original primitive.
	     (values original-prim-name (make-type-signature/single-value rand.ots)))

	    ((list-type-spec? rand.ots)
	     (values '$cdr (make-type-signature/single-value
			    (let ((cdr-ots* (cdr (list-type-spec.item-ots* rand.ots))))
			      (if (null? cdr-ots*)
				  (<null>-ots)
				(make-list-type-spec cdr-ots*))))))

	    ((pair-of-type-spec? rand.ots)
	     (values '$cdr (make-type-signature/single-value (pair-of-type-spec.item-ots rand.ots))))

	    ((pair-type-spec? rand.ots)
	     (values '$cdr (make-type-signature/single-value (pair-type-spec.cdr-ots rand.ots))))

	    ((or (<nelist>-ots? rand.ots)
		 (<pair>-ots?   rand.ots))
	     (values '$cdr (make-type-signature/single-top)))

	    ((or (object-type-spec.compatible-super-and-sub? (<nelist>-ots) rand.ots)
		 (object-type-spec.compatible-super-and-sub? (<pair>-ots)   rand.ots))
	     ;;This is not  an exact match: the  operand might be null.   So we apply
	     ;;the original primitive.
	     (values original-prim-name (make-type-signature/single-top)))

	    (else
	     ;;This should never happen.
	     (assertion-violation __module_who__
	       "internal error, core primitive operand of wrong type" input-form.stx rands.sig)))))

  #| end of module: CHI-CDR-APPLICATION |# )


;;;; special applications: VECTOR, FOLDABLE-VECTOR, <NEVECTOR>-CONSTRUCTOR

(define (chi-vector-application input-form.stx
				rator.psi rand*.psi rands.sig)
  ;;The input form has the syntax:
  ;;
  ;;   (vector ?expr ...)
  ;;
  ;;We have  already validated  the number  and type of  the operands.   The argument
  ;;RATOR.PSI     represents    the     expanded    VECTOR,     FOLDABLE-VECTOR    or
  ;;<NEVECTOR>-CONSTRUCTOR  syntactic  identifier.   The   argument  RAND*.PSI  is  a
  ;;(possibly empty) list  of items, each having a single  value type signature.  The
  ;;argument  RANDS.SIG  is  a  "<type-signature>"  representing  the  types  of  the
  ;;operands.
  ;;
  ;;The application  of VECTOR is  special because we want  the expression to  return a
  ;;type signature describing a sub-type of "<vector>".
  ;;
  (let ((application.sig (let ((rands.specs (type-signature.object-type-specs rands.sig)))
			   (if (null? rands.specs)
			       (make-type-signature/single-null)
			     (make-type-signature/single-value (make-vector-type-spec rands.specs))))))
    (make-psi input-form.stx
      (build-application (syntax-annotation input-form.stx)
	  (psi.core-expr rator.psi)
	(map psi.core-expr rand*.psi))
      application.sig)))


;;;; special applications: CONDITION

(module (chi-condition-application)
  ;;The input form has the syntax:
  ;;
  ;;   (condition ?rand ...)
  ;;
  ;;We have  already validated  the number  and type of  the operands.   The argument
  ;;RATOR.PSI represents  the expanded CONDITION syntactic  identifier.  The argument
  ;;RAND*.PSI is a  (possibly empty) list of  items, each having a  single value type
  ;;signature.  The argument RANDS.SIG is a "<type-signature>" representing the types
  ;;of the operands.
  ;;
  ;;This function is called both when the operands signature matches exactly and when
  ;;the operands signature is a compatible match.
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

  (define (chi-condition-application input-form.stx
				     rator.psi rand*.psi rands.sig)
    (if (null? rand*.psi)
	;;No arguments.  Just evaluate "(condition)".
	(make-psi input-form.stx
	  (build-application (syntax-annotation input-form.stx)
	      (psi.core-expr rator.psi)
	    '())
	  (make-type-signature/single-value (make-compound-condition-type-spec '())))
      ;;One or more operands.
      (let ((application.sig (%operand-signatures->application-signature input-form.stx (type-signature.object-type-specs rands.sig))))
	(make-psi input-form.stx
	  (build-application (syntax-annotation input-form.stx)
	      (psi.core-expr rator.psi)
	    (map psi.core-expr rand*.psi))
	  application.sig))))

  (define (%operand-signatures->application-signature input-form.stx rand*.ots)
    (make-type-signature/single-value
     (call/cc
	 (lambda (escape)
	   (let* ((punt		  (lambda () (escape (<compound-condition>-ots))))
		  (component*.ots (fold-right
				      (lambda (rand.ots component*.ots)
					(%process-single-value-operand input-form.stx component*.ots rand.ots punt))
				    '() rand*.ots)))
	     ;;We want:
	     ;;
	     ;;   (type-of (make-who-condition 'ciao))
	     ;;   => #[type-signature (&who)]
	     ;;
	     ;;   (type-of (condition (make-who-condition 'ciao)))
	     ;;   => #[type-signature (&who)]
	     ;;
	     (if (list-of-single-item? component*.ots)
		 (car component*.ots)
	       (make-compound-condition-type-spec component*.ots)))))))

  (define (%process-single-value-operand input-form.stx component*.ots rand.ots punt)
    (cond ((simple-condition-object-type-spec? rand.ots)
	   (cons rand.ots component*.ots))

	  ((compound-condition-type-spec? rand.ots)
	   (append (compound-condition-type-spec.component-ots* rand.ots)
		   component*.ots))

	  ((and (union-type-spec? rand.ots)
		(for-all (lambda (item.ots)
			   (or (simple-condition-object-type-spec?	item.ots)
			       (compound-condition-type-spec?		item.ots)
			       (<compound-condition>-ots?		item.ots)
			       (<condition>-ots?			item.ots)))
		  (union-type-spec.component-ots* rand.ots)))
	   (punt))

	  ((and (intersection-type-spec? rand.ots)
		(for-all (lambda (item.ots)
			   (or (simple-condition-object-type-spec?	item.ots)
			       (compound-condition-type-spec?		item.ots)
			       (<compound-condition>-ots?		item.ots)
			       (<condition>-ots?			item.ots)))
		  (intersection-type-spec.component-ots* rand.ots)))
	   (punt))

	  ((or (<compound-condition>-ots? rand.ots)
	       (<condition>-ots?          rand.ots))
	   (punt))

	  ((object-type-spec.compatible-super-and-sub? (<condition>-ots) rand.ots)
	   (cons rand.ots component*.ots))

	  (else
	   ;;This should never happen.
	   (assertion-violation __module_who__
	     "internal error, core primitive operand of wrong type" input-form.stx rand.ots))))

  #| end of module: CHI-CONDITION-APPLICATION |# )


;;;; special applications: VALUES

(define (chi-values-application input-form.stx
				rator.psi rand*.psi rands.sig)
  ;;The input form has the syntax:
  ;;
  ;;   (values ?rand ...)
  ;;
  ;;We have  already validated  the number  and type of  the operands.   The argument
  ;;RATOR.PSI  represents the  expanded  VALUES syntactic  identifier.  The  argument
  ;;RAND*.PSI is a  (possibly empty) list of  items, each having a  single value type
  ;;signature.  The argument RANDS.SIG is a "<type-signature>" representing the types
  ;;of the operands.
  ;;
  (cond ((null? rand*.psi)
	 ;;No arguments, zero returned values.  Just evaluate "(values)".
	 (make-psi input-form.stx
	   (build-application (syntax-annotation input-form.stx)
	       (psi.core-expr rator.psi)
	     '())
	   (make-type-signature '())))

	((list-of-single-item? rand*.psi)
	 ;;Single expression,  single return value.  Just  convert "(values ?expr)"
	 ;;to "?expr".
	 (make-psi input-form.stx
	   (psi.core-expr (car rand*.psi))
	   rands.sig))

	(else
	 ;;Two or more values.
	 (make-psi input-form.stx
	   (build-application (syntax-annotation input-form.stx)
	       (psi.core-expr rator.psi)
	     (map psi.core-expr rand*.psi))
	   rands.sig))))


;;;; special applications: CALL-WITH-VALUES

(define* (chi-call-with-values-application input-form.stx
					   rator.psi rand*.psi rands.sig)
  ;;The input form has the syntax:
  ;;
  ;;   (call-with-values ?rand ...)
  ;;
  ;;We have already validated  the number and type of the  operands; this function is
  ;;called both  when the operands's  signature is  an exact match  and when it  is a
  ;;compatible   match.     The   argument   RATOR.PSI   represents    the   expanded
  ;;CALL-WITH-VALUES syntactic identifier.   The argument RAND*.PSI is a  list of two
  ;;items, each  having a single value  type signature.  The argument  RANDS.SIG is a
  ;;"<type-signature>" representing the types of the operands.
  ;;
  ;;The  application of  CALL-WITH-VALUES  is special;  CALL-WITH-VALUES accepts  two
  ;;arguments: a producer thunk and a consumer closure object.  We want to generate a
  ;;core expression having as type signature the type signature of the application of
  ;;the consumer.
  ;;
  (let* ((consumer.psi		(cadr rand*.psi))
	 (consumer.ots		(cadr (type-signature.object-type-specs rands.sig)))
	 (application.sig	(if (closure-type-spec? consumer.ots)
				    (case-lambda-signature.retvals (closure-type-spec.signature consumer.ots))
				  (begin
				    #;(assert (<procedure>-ots? consumer.ots))
				    (make-type-signature/fully-unspecified)))))
    (make-psi input-form.stx
      (build-application (syntax-annotation input-form.stx)
	  (psi.core-expr rator.psi)
	(list (psi.core-expr (car  rand*.psi))
	      (psi.core-expr consumer.psi)))
      application.sig)))


;;;; special applications: APPLY

(module (chi-apply-application)
  (import CLOSURE-APPLICATION-ERRORS)
  (define-module-who chi-apply-application)

  (define (chi-apply-application input-form.stx
				 rator.psi rand*.psi rands.sig)
    ;;The input form is a call to the core primitive APPLY:
    ;;
    ;;   (apply ?rand0 ?rand ...)
    ;;
    ;;We have already validated the number and type of the operands; this function is
    ;;called both when  the operands's signature is  an exact match and when  it is a
    ;;compatible  match.   The  argument  RATOR.PSI  represents  the  expanded  APPLY
    ;;syntactic identifier.  The  argument RAND*.PSI is a list of  one or more items,
    ;;each  having a  single  value  type signature.   The  argument  RANDS.SIG is  a
    ;;"<type-signature>" representing the types of the operands.
    ;;
    ;;In  a call  to APPLY  we expect  ?RAND0  to be  an expression  evaluating to  a
    ;;procedure.  The last of the operands must be a subtype of "<list>".  The return
    ;;value is the return value of the ?RAND0 application.
    ;;
    (define (%build-default-application application.sig)
      (make-psi input-form.stx
	(build-application (syntax-annotation input-form.stx)
	    (psi.core-expr rator.psi)
	  (map psi.core-expr rand*.psi))
	application.sig))
    #;(assert (<= 2 (length rand*.psi)))
    (let* ((rand*.ots		(type-signature.object-type-specs rands.sig))
	   (proc.ots		(car rand*.ots))
	   (proc-rands.specs	(append (%strip-first-and-last rand*.ots)
					(%last-rand-specs input-form.stx
							  (car (last-pair rand*.psi))
							  (car (last-pair rand*.ots))))))
      (cond ((%psi-rator-is-values? (car rand*.psi))
	     ;;VALUES is  special because it accepts  any number of arguments  of any
	     ;;type and  it returns  a different  number of  values depending  on the
	     ;;number of operands.
	     (%build-default-application (make-type-signature proc-rands.specs)))

	    ((closure-type-spec? proc.ots)
	     ;;The first operand expression returns a closure object.  Good.
	     (%build-closure-application input-form.stx
					 (car rand*.psi) (cdr rand*.psi)
					 proc.ots proc-rands.specs
					 (+ -2 (length rand*.psi))
					 %build-default-application))

	    ((<procedure>-ots? proc.ots)
	     ;;The  first  operand  expression  returns a  single  value,  marked  as
	     ;;"<procedure>".  Good.   There are  no further validations  possible at
	     ;;expand-time.  There is no optimisation possible.
	     (%build-default-application (make-type-signature/fully-unspecified)))

	    (else
	     ;;The first operand  expression has a type that is  only compatible with
	     ;;"<procedure>".   Return a  procedure application  and we  will see  at
	     ;;run-time what happens; this is standard Scheme behaviour.
	     (%build-default-application (make-type-signature/fully-unspecified))))))

;;; --------------------------------------------------------------------

  (define (%last-rand-specs input-form.stx last-rand.psi last-rand.ots)
    (define (common)
      (condition
	(make-who-condition __module_who__)
	(make-message-condition "last operand in call to APPLY is not a sub-type of <list>")
	(make-syntax-violation input-form.stx (psi.input-form last-rand.psi))
	(make-application-operand-signature-condition (psi.retvals-signature last-rand.psi))))
    (cond ((list-type-spec? last-rand.ots)
	   ;;Exact match.
	   (list-type-spec.item-ots* last-rand.ots))
	  ((or (list-of-type-spec?	last-rand.ots)
	       (<nelist>-ots?		last-rand.ots)
	       (<list>-ots?		last-rand.ots))
	   ;;Exact match.
	   last-rand.ots)
	  ((<null>-ots? last-rand.ots)
	   ;;Exact match.
	   '())
	  ((<top>-ots? last-rand.ots)
	   ;;Possible match.
	   (<list>-ots))
	  (else
	   ;;No match.
	   (case-expander-language
	     ((typed)
	      (raise			(condition (make-expand-time-type-signature-violation) (common))))
	     ((default)
	      (raise-continuable	(condition (make-expand-time-type-signature-warning)   (common)))
	      (<list>-ots))
	     ((strict-r6rs)
	      (<list>-ots))))))

;;; --------------------------------------------------------------------

  (define (%build-closure-application input-form.stx
				      proc.psi proc-rand*.psi
				      proc.ots proc-rands.specs
				      min-given-operands-count
				      %build-default-application)
    ;;PROC-RANDS.SPECS is a proper or improper list of "<object-type-spec>" instances
    ;;representing the type of the operands.
    ;;
    (define proc.clambda-sig (closure-type-spec.signature proc.ots))
    (define proc-rands.sig (make-type-signature proc-rands.specs))
    ;;PROC.CLAMBDA-SIG is  an instance  of "<case-lambda-signature>".  The  number of
    ;;operands must  be in  the correct  range of arguments  accepted by  the closure
    ;;object.
    (receive (minimum-arguments-count maximum-arguments-count)
	(case-lambda-signature.min-and-max-argvals proc.clambda-sig)
      (if (list? proc-rands.specs)
	  ;;There is a known number of operands.
	  (let ((given-operands-count (length proc-rands.specs)))
	    (cond ((< maximum-arguments-count given-operands-count)
		   (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
		     (psi.input-form proc.psi) (map psi.input-form proc-rand*.psi)
		     maximum-arguments-count given-operands-count))
		  ((> minimum-arguments-count given-operands-count)
		   (%error-number-of-operands-deceeds-minimum-arguments-count input-form.stx
		     (psi.input-form proc.psi) (map psi.input-form proc-rand*.psi)
		     minimum-arguments-count given-operands-count))))
	;;There is an unknown number of operands, but we know the minimum number.
	(when (< maximum-arguments-count min-given-operands-count)
	  (%error-number-of-operands-exceeds-maximum-arguments-count input-form.stx
	    (psi.input-form proc.psi) (map psi.input-form proc-rand*.psi)
	    maximum-arguments-count min-given-operands-count))))
    ;;Search for matching clauses.
    (let* ((selected-clause-signature* '())
	   (state (returnable
		    (fold-left (lambda (state clause-signature)
				 (let ((formals.sig (lambda-signature.argvals clause-signature)))
				   (case (type-signature.match-arguments-against-operands formals.sig proc-rands.sig)
				     ((exact-match)
				      (set! selected-clause-signature* (list clause-signature))
				      (return 'exact-match))
				     ((possible-match)
				      (set-cons! selected-clause-signature* clause-signature)
				      'possible-match)
				     (else state))))
		      'no-match (case-lambda-signature.clause-signature* proc.clambda-sig)))))
      (define (%default-core-expr)
	(%build-default-application (%compute-application-signature input-form.stx selected-clause-signature*)))
      (case state
	((exact-match possible-match)
	 ;;There is a clause that exactly matches the operands.  When we are here: we
	 ;;know that SELECTED-CLAUSE-SIGNATURE* holds a single item.
	 (%default-core-expr))
	((possible-match)
	 ;;There is at least one clause with a possible match.  It is not possible to
	 ;;fully  validate  the  signatures  at  expand-time;  we  rely  on  run-time
	 ;;checking.
	 (%default-core-expr))
	(else
	 ;;There are no  matching clauses, not even possible  matches.  Arguments and
	 ;;operands do *not* match at expand-time.
	 #;(assert (null? selected-clause-signature*))
	 (let ((make-arguments-signature* (lambda ()
					    (map lambda-signature.argvals
					      (case-lambda-signature.clause-signature* proc.clambda-sig)))))
	   (case-expander-language
	     ((typed)
	      (%error-mismatch-between-args-signature-and-operands-signature   input-form.stx (make-arguments-signature*) proc-rands.sig))
	     ((default)
	      (%warning-mismatch-between-args-signature-and-operands-signature input-form.stx (make-arguments-signature*) proc-rands.sig)
	      (%default-core-expr))
	     ((strict-r6rs)
	      (%default-core-expr))))))))

  (define (%compute-application-signature input-form.stx selected-clause-signature*)
    (cond ((list-of-single-item? selected-clause-signature*)
	   ;;There is a single compatible clause signature.
	   (lambda-signature.retvals (car selected-clause-signature*)))
	  ((pair? selected-clause-signature*)
	   ;;There  are multiple  compatible  clause signatures.   Let's compute  the
	   ;;union between the retvals type signatures.
	   (apply type-signature.union-same-number-of-operands
		  (lambda (message cnd)
		    (raise (condition (make-who-condition __module_who__)
				      (make-message-condition message)
				      (make-syntax-violation input-form.stx #f)
				      cnd)))
		  (map lambda-signature.retvals selected-clause-signature*)))
	  (else
	   (make-type-signature/fully-unspecified))))

;;; --------------------------------------------------------------------

  (define (%strip-first-and-last ell)
    ;;Strip the  first and last  items from ELL.   ELL must be a  list of at  least 2
    ;;items.
    ;;
    (let recur ((ell (cdr ell)))
      (if (null? (cdr ell))
	  '()
	(cons (car ell) (recur (cdr ell))))))

  #| end of module: CHI-APPLY-APPLICATION |# )


;;;; special applications: DYNAMIC-WIND

(define* (chi-dynamic-wind-application input-form.stx
				       rator.psi rand*.psi rands.sig)
  ;;The input form has the syntax:
  ;;
  ;;   (dynamic-wind ?in-guard ?body ?out-guard)
  ;;
  ;;We have already validated  the number and type of the  operands; this function is
  ;;called both  when the operands's  signature is  an exact match  and when it  is a
  ;;compatible match.   The argument  RATOR.PSI represents the  expanded DYNAMIC-WIND
  ;;syntactic identifier.   The argument  RAND*.PSI is  a list  of three  items, each
  ;;having   a  single   value  type   signature.   The   argument  RANDS.SIG   is  a
  ;;"<type-signature>" representing the types of the operands.
  ;;
  ;;The  application  of  DYNAMIC-WIND  is  special.  We  want  to  generate  a  core
  ;;expression having as type signature the type signature of the body thunk.
  ;;
  (let* ((body-thunk.ots	(cadr (type-signature.object-type-specs rands.sig)))
	 (application.sig	(if (closure-type-spec? body-thunk.ots)
				    (case-lambda-signature.retvals (closure-type-spec.signature body-thunk.ots))
				  (make-type-signature/fully-unspecified))))
    (make-psi input-form.stx
      (build-application (syntax-annotation input-form.stx)
	  (psi.core-expr rator.psi)
	(map psi.core-expr rand*.psi))
      application.sig)))


;;;; done

#| end of module: CHI-APPLICATION |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;End:
