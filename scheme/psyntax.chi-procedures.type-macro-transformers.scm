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


(module (new-transformer
	 delete-transformer
	 is-a?-transformer
	 internal-run-time-is-a?-transformer
	 slot-ref-transformer
	 slot-set!-transformer
	 method-call-transformer
	 unsafe-cast-signature-transformer
	 case-type-transformer
	 validate-typed-procedure-argument-transformer
	 validate-typed-return-value-transformer
	 assert-signature-transformer
	 assert-signature-and-return-transformer
	 type-of-transformer
	 type-super-and-sub?-transformer
	 signature-super-and-sub?-transformer)


;;;; helpers

(define-syntax (with-object-type-syntactic-binding stx)
  (sys::syntax-case stx ()
    ((_ (?who ?input-form.stx ?type-id ?lexenv ?object-type-spec)
	. ?body)
     (and (sys::identifier? (sys::syntax ?who))
	  (sys::identifier? (sys::syntax ?input-form.stx))
	  (sys::identifier? (sys::syntax ?type-id))
	  (sys::identifier? (sys::syntax ?lexenv))
	  (sys::identifier? (sys::syntax ?object-type-spec)))
     (sys::syntax
      (let ((?object-type-spec (id->object-type-specification ?who ?input-form.stx ?type-id ?lexenv)))
	. ?body)))
    ))

(define (%fxiota fx item*)
  (if (pair? item*)
      (cons fx (%fxiota (fxadd1 fx) (cdr item*)))
    '()))


;;;; module core-macro-transformer: NEW

(module (new-transformer)

  (define-module-who new)

  (define-core-transformer (new input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function  used to expand NEW  syntaxes from the top-level  built in
    ;;environment.  Expand  the syntax  object INPUT-FORM.STX in  the context  of the
    ;;given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?type-id ?rand* ...)
       (identifier? ?type-id)
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run ots)
	 (cond ((list-type-spec? ots)
		(%build-list-constructor input-form.stx lexenv.run lexenv.expand
					 ?type-id (list-type-spec.type-id ots) ?rand*))
	       ((object-type-spec.constructor-sexp ots)
		=> (lambda (constructor.sexp)
		     (if (boolean? constructor.sexp)
			 (%build-object-with-validator input-form.stx lexenv.run lexenv.expand ?type-id ?rand*)
		       (%build-object-with-constructor input-form.stx lexenv.run lexenv.expand ?type-id constructor.sexp ?rand*))))
	       (else
		(%synner "attempt to instantiate object-type with no constructor (abstract type?)" ?type-id)))))
      ))

;;; --------------------------------------------------------------------

  (define (%build-object-with-validator input-form.stx lexenv.run lexenv.expand object-type.id rand*.stx)
    ;;The  type identifier  OBJECT-TYPE.ID  references  an object-type  specification
    ;;having  no constructor,  but requesting  validation of  a single  operand.  For
    ;;example:
    ;;
    ;;   (new <fixnum> 123)
    ;;
    ;;must expand to:
    ;;
    ;;   (assert-signature-and-return (<fixnum>) 123)
    ;;
    (chi-expr (bless
	       `(assert-signature-and-return (,object-type.id) (values . ,rand*.stx)))
	      lexenv.run lexenv.expand))

;;; --------------------------------------------------------------------

  (define (%build-object-with-constructor input-form.stx lexenv.run lexenv.expand object-type.id constructor.sexp rand*.stx)
    (chi-expr (bless
	       `(assert-signature-and-return (,object-type.id) (,constructor.sexp . ,rand*.stx)))
	      lexenv.run lexenv.expand))

;;; --------------------------------------------------------------------

  (module (%build-list-constructor)

    (define (%build-list-constructor input-form.stx lexenv.run lexenv.expand
				     list-type.id item.id rand*.stx)
      (if (null? rand*.stx)
	  (make-psi input-form.stx
		    (build-data no-source '())
		    (make-type-signature/single-value list-type.id))
	(let ((rand*.core (map (lambda (rand.stx rand.idx)
				 (%process-list-constructor-operand input-form.stx lexenv.run lexenv.expand
								    list-type.id item.id rand.stx rand.idx))
			    rand*.stx (%fxiota 0 rand*.stx))))
	  (make-psi input-form.stx
		    (build-application input-form.stx
		      (build-primref no-source 'list)
		      (cons* rand*.core))
		    (make-type-signature/single-value list-type.id)))))

    (define (%process-list-constructor-operand input-form.stx lexenv.run lexenv.expand
					       list-type.id item.id rand.stx rand.idx)
      (define rand.psi (chi-expr rand.stx lexenv.run lexenv.expand))
      (define rand.sig (psi.retvals-signature rand.psi))
      (define (%run-time-validation)
	(%build-type-run-time-validation input-form.stx lexenv.run lexenv.expand
					 list-type.id item.id rand.stx rand.idx (psi.core-expr rand.psi)))
      (syntax-match (type-signature-tags rand.sig) (<top> <list>)
	((<top>)
	 ;;Integrate a run-time validator for the single return value.
	 (%run-time-validation))

	((?operand-id)
	 (if (type-identifier-super-and-sub? item.id ?operand-id lexenv.run input-form.stx)
	     ;;Success!!!
	     (psi.core-expr rand.psi)
	   (%error-expand-time/argument-and-operand-type-mismatch input-form.stx rand.stx rand.idx item.id ?operand-id)))

	(<list>
	 ;;The  retvals signature  is  fully unspecified.   We  integrate a  run-time
	 ;;validator  expecting a  single  return value.   We  use the  automatically
	 ;;generated machine code to validate the number of return values.
	 (%run-time-validation))

	(?operand-some-list-type
	 (identifier? ?operand-some-list-type)
	 (with-object-type-syntactic-binding (__module_who__ input-form.stx ?operand-some-list-type lexenv.run operand-ots)
	   (let ((operand-item.id (list-type-spec.type-id operand-ots)))
	     (if (type-identifier-super-and-sub? item.id operand-item.id lexenv.run input-form.stx)
		 ;;We let the  integrated single-value validation do the  rest of the
		 ;;job.
		 (psi.core-expr rand.psi)
	       (%error-expand-time/argument-and-operand-type-mismatch input-form.stx rand.stx rand.idx item.id ?operand-some-list-type)))))

	(_
	 ;;The horror!!!   We have established at  expand-time that the
	 ;;expression returns multiple values; type violation.
	 (%error-expand-time/operand-returns-multiple-values input-form.stx rand.stx rand.sig))))

    (define (%build-type-run-time-validation input-form.stx lexenv.run lexenv.expand
					     list-type.id item.id rand.stx rand.idx rand.core)
      (let* ((validator.sexp	(let ((arg.sym (gensym "arg")))
				  `(lambda (,arg.sym)
				     (if (is-a? ,arg.sym ,item.id)
					 ,arg.sym
				       (procedure-signature-argument-violation (quote ,list-type.id)
					 "invalid object type" ,rand.idx '(is-a? _ ,item.id) ,arg.sym)))))
	     (validator.psi	(chi-expr (bless validator.sexp) lexenv.run lexenv.expand))
	     (validator.core	(psi.core-expr validator.psi)))
	(build-application no-source
	  validator.core
	  (list rand.core))))

    #| end of module: %BUILD-LIST-CONSTRUCTOR |# )

;;; --------------------------------------------------------------------

  (define (%error-expand-time/argument-and-operand-type-mismatch input-form.stx rand.stx rand.idx item.id rand.id)
    (raise-compound-condition-object __module_who__
      "expand-time mismatch between list's item type and operand signature"
      input-form.stx
      (condition
       (make-expand-time-type-signature-violation)
       (make-syntax-violation input-form.stx rand.stx)
       (make-argument-index-condition rand.idx)
       (make-argument-type-syntactic-identifier-condition item.id)
       (make-operand-type-syntactic-identifier-condition rand.id))))

  (define (%error-expand-time/operand-returns-multiple-values input-form.stx rand.stx rand.sig)
    (raise
     (condition
      (make-who-condition __module_who__)
      (make-message-condition "the expression used as constructor's operand returns multiple values")
      (make-syntax-violation input-form.stx rand.stx)
      (make-irritants-condition (list rand.sig)))))

  #| end of module |# )


;;;; module core-macro-transformer: DELETE

(module (delete-transformer)

  (define-core-transformer (delete input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand DELETE syntaxes from the top-level built in
    ;;environment.  Expand  the syntax  object INPUT-FORM.STX in  the context  of the
    ;;given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?expr)
       (let* ((expr.psi (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig (psi.retvals-signature expr.psi)))
	 (syntax-match (type-signature-tags expr.sig) (<list>)
	   ((?type-id)
	    ;;We have determined at expand-time  that the expression returns a single
	    ;;value.
	    (%apply-appropriate-destructor __who__ input-form.stx lexenv.run lexenv.expand ?type-id expr.psi))
	   (<list>
	    ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	    ;;signature; we need to insert a run-time dispatch.
	    (%run-time-destruction input-form.stx lexenv.run lexenv.expand expr.psi))
	   (_
	    ;;The horror!!!  We  have established at expand-time  that the expression
	    ;;returns multiple values; type violation.
	    (syntax-violation __who__
	      "the expression used as destructor operand returns multiple values"
	      input-form.stx ?expr))
	   )))
      ))

  (define (%apply-appropriate-destructor who input-form.stx lexenv.run lexenv.expand type-id expr.psi)
    (with-object-type-syntactic-binding (who input-form.stx type-id lexenv.run ots)
      (cond ((object-type-spec.destructor-sexp ots)
	     => (lambda (destructor.sexp)
		  ;;This record type has a default destructor.
		  (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						     (bless destructor.sexp) expr.psi '())))
	    (else
	     ;;This  record type  has *no*  default destructor;  default to  run-time
	     ;;destruction.
	     ;;
	     ;;Example of  usefulness of defaulting  to run-time destruction:  if the
	     ;;object is  a record with a  destructor set at run-time,  this way that
	     ;;destructor will be called.
	     (%run-time-destruction input-form.stx lexenv.run lexenv.expand expr.psi)))))

  (define* (%run-time-destruction input-form.stx lexenv.run lexenv.expand {expr.psi psi?})
    (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
				       (core-prim-id 'internal-delete) expr.psi '()))

  #| end of module: DELETE-TRANSFORMER |# )


;;;; module core-macro-transformer: IS-A?

(module (is-a?-transformer)

  (define-module-who is-a?)

  (define-core-transformer (is-a? input-form.stx lexenv.run lexenv.expand)
    ;;Transformer function used to expand Vicare's IS-A?  syntaxes from the top-level
    ;;built in environment.   Expand the syntax object INPUT-FORM.STX  in the context
    ;;of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?jolly ?type-id)
       (and (identifier? ?type-id)
	    (underscore-id? ?jolly))
       (chi-expr (bless
		  (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run ots)
		    (or (object-type-spec.type-predicate-sexp ots)
			(%synner "type specification has no predicate for run-time use" ?type-id))))
		 lexenv.run lexenv.expand))

      ((_ ?expr ?pred-type-id)
       (identifier? ?pred-type-id)
       (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig  (psi.retvals-signature expr.psi)))
	 (define (%run-time-predicate)
	   (%expand-to-run-time-predicate-application input-form.stx lexenv.run lexenv.expand ?pred-type-id expr.psi %synner))
	 (syntax-match (type-signature-tags expr.sig) (<top> <list> <condition> <compound-condition>)
	   ((<top>)
	    (%run-time-predicate))

	   ((<compound-condition>)
	    (cond ((type-identifier-super-and-sub? ?pred-type-id (compound-condition-type-id) lexenv.run input-form.stx)
		   (%make-true-psi input-form.stx ?expr lexenv.run lexenv.expand))
		  ((type-identifier-super-and-sub? (compound-condition-type-id) ?pred-type-id lexenv.run input-form.stx)
		   (%run-time-predicate))
		  ((type-identifier-super-and-sub? (simple-condition-type-id)   ?pred-type-id lexenv.run input-form.stx)
		   (%run-time-predicate))
		  (else
		   (%make-false-psi input-form.stx ?expr lexenv.run lexenv.expand))))

	   ((?expr-type-id)
	    (if (or (and (null-type-id? ?expr-type-id)
			 (type-identifier-is-list-or-list-sub-type? ?pred-type-id))
		    (type-identifier-super-and-sub? ?pred-type-id ?expr-type-id lexenv.run input-form.stx))
		(%make-true-psi input-form.stx ?expr lexenv.run lexenv.expand)
	      (%make-false-psi input-form.stx ?expr lexenv.run lexenv.expand)))

	   (<list>
	    (%run-time-predicate))

	   (?expr-type-id
	    (type-identifier-is-list-sub-type? ?expr-type-id lexenv.run)
	    (if (type-identifier-super-and-sub? ?pred-type-id ?expr-type-id lexenv.run input-form.stx)
		(%make-true-psi input-form.stx ?expr lexenv.run lexenv.expand)
	      (%make-false-psi input-form.stx ?expr lexenv.run lexenv.expand)))

	   (_
	    ;;We have determined at expand-time  that the expression returns multiple
	    ;;values.
	    (raise
	     (condition (make-who-condition __who__)
			(make-message-condition "subject expression of type predicate returns multiple values")
			(make-syntax-violation input-form.stx ?expr)
			(make-irritants-condition (list expr.sig))))))))
      ))

;;; --------------------------------------------------------------------

  (define (%expand-to-run-time-predicate-application input-form.stx lexenv.run lexenv.expand pred-type-id expr.psi %synner)
    (with-object-type-syntactic-binding (__module_who__ input-form.stx pred-type-id lexenv.run pred-ots)
      (let* ((pred.sexp  (or (object-type-spec.type-predicate-sexp pred-ots)
			     (%synner "type specification has no predicate for run-time use" pred-type-id)))
	     (pred.stx   (bless pred.sexp)))
	(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					   pred.stx expr.psi '()))))

;;; --------------------------------------------------------------------

  (define (%make-true-psi input-form.stx expr.stx lexenv.run lexenv.expand)
    (%make-boolean-psi input-form.stx #t expr.stx lexenv.run lexenv.expand))

  (define (%make-false-psi input-form.stx expr.stx lexenv.run lexenv.expand)
    (%make-boolean-psi input-form.stx #f expr.stx lexenv.run lexenv.expand))

  (define (%make-boolean-psi input-form.stx bool expr.stx lexenv.run lexenv.expand)
    (let* ((expr.psi   (chi-expr expr.stx lexenv.run lexenv.expand))
	   (expr.core  (psi.core-expr expr.psi)))
      (make-psi input-form.stx
		(build-sequence no-source
		  (list expr.core
			(build-data no-source bool)))
		(make-type-signature/single-boolean))))

  #| end of module: IS-A?-TRANSFORMER |# )


;;;; module core-macro-transformer: INTERNAL-RUN-TIME-IS-A?

(define-core-transformer (internal-run-time-is-a? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand Vicare's  INTERNAL-RUN-TIME-IS-A?  syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr ?pred-type-id)
     (identifier? ?pred-type-id)
     (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.sig  (psi.retvals-signature expr.psi))
	    (expr.core (psi.core-expr expr.psi)))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?pred-type-id lexenv.run pred-ots)
	 (let* ((pred.sexp  (or (object-type-spec.type-predicate-sexp pred-ots)
				(%synner "type specification has no predicate for run-time use" ?pred-type-id)))
		(pred.stx   (bless pred.sexp)))
	   (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					      pred.stx expr.psi '())))))
    ))


;;;; module core-macro-transformer: SLOT-REF

(module (slot-ref-transformer)

  (define-module-who slot-ref)

  (define-core-transformer (slot-ref input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function  used  to  expand Vicare's  SLOT-REF  syntaxes  from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ;;Everything included.  It must expand to an accessor application.
      ((_ ?expr ?field-name ?type-id)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name)
	    (identifier? ?type-id))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run ots)
	 (%expand-to-object-accessor-application input-form.stx lexenv.run lexenv.expand ots ?expr ?field-name)))

      ;;Wildcard in place of the subject expression.  It must expand to an expression
      ;;that evaluates to an accessor.
      ((_ ?jolly ?field-name ?type-id)
       (and (identifier? ?type-id)
	    (identifier? ?field-name)
	    (underscore-id? ?jolly))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run ots)
	 (%expand-to-object-accessor input-form.stx lexenv.run lexenv.expand ots ?field-name)))

      ;;Missing type identifier.  Try to retrieve  the type from the signature of the
      ;;subject expression.
      ((_ ?expr ?field-name)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name))
       (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig  (psi.retvals-signature expr.psi)))
	 (define (%error-unknown-type)
	   (%synner "unable to determine type of expression at expand-time" ?expr))
	 (syntax-match (type-signature-tags expr.sig) (<top> <list>)
	   ((<top>)
	    (%error-unknown-type))

	   ((?type-id)
	    (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run ots)
	      (%expand-to-object-accessor-application-post input-form.stx lexenv.run lexenv.expand ots expr.psi ?field-name)))

	   (<list>
	    ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	    ;;signature.
	    (%error-unknown-type))

	   (_
	    ;;We have determined at expand-time  that the expression returns multiple
	    ;;values.
	    (raise
	     (condition (make-who-condition __who__)
			(make-message-condition "subject expression of slot access returns multiple values")
			(make-syntax-violation input-form.stx ?expr)
			(make-irritants-condition (list expr.sig)))))
	   )))
      ))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-accessor-application input-form.stx lexenv.run lexenv.expand
						   ots {expr.stx syntax-object?} field-name.id)
    (cond ((object-type-spec.safe-accessor-sexp ots (identifier->symbol field-name.id) lexenv.run)
	   => (lambda (accessor.sexp)
		(chi-expr (bless
			   `(,accessor.sexp ,expr.stx))
			  lexenv.run lexenv.expand)))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

;;; --------------------------------------------------------------------

  (define (%expand-to-object-accessor input-form.stx lexenv.run lexenv.expand
				      ots field-name.id)
    (cond ((object-type-spec.safe-accessor-sexp ots (identifier->symbol field-name.id) lexenv.run)
	   => (lambda (accessor.sexp)
		(chi-expr (bless accessor.sexp) lexenv.run lexenv.expand)))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-accessor-application-post input-form.stx lexenv.run lexenv.expand ots {expr.psi psi?} field-name.id)
    (cond ((object-type-spec.safe-accessor-sexp ots (identifier->symbol field-name.id) lexenv.run)
	   => (lambda (accessor.sexp)
		(chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						   (bless accessor.sexp) expr.psi '())))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

  #| end of module: SLOT-REF-TRANSFORMER |# )


;;;; module core-macro-transformer: SLOT-SET!

(module (slot-set!-transformer)

  (define-module-who slot-set!)

  (define-core-transformer (slot-set! input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function used  to  expand Vicare's  SLOT-SET!   syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ;;Everything included.  It must expand to an mutator application.
      ((_ ?expr ?field-name ?type-id ?new-value)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name)
	    (identifier? ?type-id))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run ots)
	 (%expand-to-object-mutator-application input-form.stx lexenv.run lexenv.expand ots ?expr ?field-name ?new-value)))

      ;;Wildcard in place of the subject expression.  It must expand to an expression
      ;;that evaluates to an mutator.
      ((_ ?jolly1 ?field-name ?type-id ?jolly2)
       (and (identifier? ?type-id)
	    (identifier? ?field-name)
	    (underscore-id? ?jolly1)
	    (underscore-id? ?jolly2))
       (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run ots)
	 (%expand-to-object-mutator input-form.stx lexenv.run lexenv.expand ots ?field-name)))

      ;;Missing type identifier.  Try to retrieve  the type from the signature of the
      ;;subject expression.
      ((_ ?expr ?field-name ?new-value)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name))
       (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig  (psi.retvals-signature expr.psi)))
	 (define (%error-unknown-type)
	   (%synner "unable to determine type of expression at expand-time" ?expr))
	 (syntax-match (type-signature-tags expr.sig) (<top> <list>)
	   ((<top>)
	    (%error-unknown-type))

	   ((?type-id)
	    (with-object-type-syntactic-binding (__who__ input-form.stx ?type-id lexenv.run ots)
	      (%expand-to-object-mutator-application-post input-form.stx lexenv.run lexenv.expand ots expr.psi ?field-name ?new-value)))

	   (<list>
	    ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	    ;;signature.
	    (%error-unknown-type))

	   (_
	    ;;We have determined at expand-time  that the expression returns multiple
	    ;;values.
	    (raise
	     (condition (make-who-condition __who__)
			(make-message-condition "subject expression of slot access returns multiple values")
			(make-syntax-violation input-form.stx ?expr)
			(make-irritants-condition (list expr.sig)))))
	   )))
      ))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-mutator-application input-form.stx lexenv.run lexenv.expand
						  ots {expr.stx syntax-object?} field-name.id new-value.stx)
    (let ((mutator.sexp (%retrieve-mutator-sexp input-form.stx lexenv.run lexenv.expand ots field-name.id)))
      (chi-expr (bless
		 `(,mutator.sexp ,expr.stx ,new-value.stx))
		lexenv.run lexenv.expand)))

;;; --------------------------------------------------------------------

  (define (%expand-to-object-mutator input-form.stx lexenv.run lexenv.expand
				     ots field-name.id)
    (let ((mutator.sexp (%retrieve-mutator-sexp input-form.stx lexenv.run lexenv.expand ots field-name.id)))
      (chi-expr (bless mutator.sexp) lexenv.run lexenv.expand)))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-mutator-application-post input-form.stx lexenv.run lexenv.expand
						       ots {expr.psi psi?} field-name.id new-value.stx)
    (let ((mutator.sexp (%retrieve-mutator-sexp input-form.stx lexenv.run lexenv.expand ots field-name.id)))
      (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
					 (bless mutator.sexp) expr.psi (list new-value.stx))))

;;; --------------------------------------------------------------------

  (define (%retrieve-mutator-sexp input-form.stx lexenv.run lexenv.expand
				  ots field-name.id)
    (define (%error message)
      (syntax-violation __module_who__ message input-form.stx field-name.id))
    (cond ((object-type-spec.safe-mutator-sexp ots (identifier->symbol field-name.id) lexenv.run)
	   => (lambda (mutator.sexp)
		(if (boolean? mutator.sexp)
		    (%error "attempt to mutate immutable field")
		  mutator.sexp)))
	  (else
	   (%error "unknown field name"))))

  #| end of module: SLOT-SET!-TRANSFORMER |# )


;;;; module core-macro-transformer: METHOD-CALL

(module (method-call-transformer)

  (define-module-who method-call)

  (define-core-transformer (method-call input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function used  to expand  Vicare's METHOD-CALL  syntaxes from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ((_ ?method-name ?subject-expr ?arg* ...)
       (identifier? ?method-name)
       (let* ((method-name.sym	(identifier->symbol ?method-name))
	      (subject-expr.psi	(chi-expr ?subject-expr lexenv.run lexenv.expand))
	      (subject-expr.sig	(psi.retvals-signature subject-expr.psi)))
	 (define-syntax-rule (%late-binding)
	   (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
						method-name.sym subject-expr.psi ?arg*))
	 (syntax-match (type-signature-tags subject-expr.sig) (<top> <list>)
	   ((<top>)
	    (%late-binding))

	   ((?type-id)
	    (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						  ?method-name method-name.sym
						  ?type-id
						  ?subject-expr subject-expr.psi ?arg*))

	   (<list>
	    ;;Damn  it!!!   The expression's  return  values  have fully  UNspecified
	    ;;signature; we need to insert a run-time dispatch.
	    (%late-binding))

	   (_
	    ;;We have determined at expand-time  that the expression returns multiple
	    ;;values.
	    (raise
	     (condition (make-who-condition __module_who__)
			(make-message-condition "subject expression of method call returns multiple values")
			(make-syntax-violation input-form.stx ?subject-expr)
			(make-irritants-condition (list subject-expr.sig)))))
	   )))
      ))

;;; --------------------------------------------------------------------

  (define (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						method-name.id method-name.sym
						type-id subject-expr.stx subject-expr.psi arg*.stx)
    (with-object-type-syntactic-binding (__module_who__ input-form.stx type-id lexenv.run ots)
      (cond
       ;;Look for a matching method name.
       ((object-type-spec.applicable-method-sexp ots method-name.sym lexenv.run)
	=> (lambda (method.sexp)
	     ;;A matching method name exists.
	     (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						(bless method.sexp) subject-expr.psi arg*.stx)))

       ;;If the input form  has the right syntax: look for a  matching field name for
       ;;accessor application.
       ((and (null? arg*.stx)
	     (object-type-spec.safe-accessor-sexp ots method-name.sym lexenv.run))
	=> (lambda (accessor.sexp)
	     ;;A matching field name exists.
	     (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						(bless accessor.sexp) subject-expr.psi arg*.stx)))

       ;;If the input form  has the right syntax: look for a  matching field name for
       ;;mutator application.
       ((and (pair? arg*.stx)
	     (null? (cdr arg*.stx))
	     (object-type-spec.safe-mutator-sexp ots method-name.sym lexenv.run))
	=> (lambda (mutator.sexp)
	     ;;A matching field name exists.
	     (chi-application/psi-first-operand input-form.stx lexenv.run lexenv.expand
						(bless mutator.sexp) subject-expr.psi arg*.stx)))

       (else
	(raise
	 (condition (make-who-condition __module_who__)
		    (make-message-condition "unknown method name for type of subject expression")
		    (make-syntax-violation input-form.stx subject-expr.stx)
		    (make-type-syntactic-identifier-condition type-id)
		    (make-type-method-name-condition method-name.sym)))))))

;;; --------------------------------------------------------------------

  (define (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
					       method-name.sym subject-expr.psi arg*.stx)
    ;;The  type of  the  values returned  by  the subject  expression  is unknown  at
    ;;expand-time; so we  expand to an expression that searches  at run-time a method
    ;;matching the  given name.  In other  words: we default to  "late binding" (also
    ;;known as "run-time dispatching" at some level of abstract reasoning).
    ;;
    (let* ((expr.core   (psi.core-expr subject-expr.psi))
	   (arg*.psi    (chi-expr* arg*.stx lexenv.run lexenv.expand))
	   (arg*.core   (map psi.core-expr arg*.psi)))
      (make-psi input-form.stx
		(build-application input-form.stx
		  (build-primref no-source 'method-call-late-binding)
		  (cons* (build-data no-source method-name.sym)
			 expr.core
			 arg*.core))
		(make-type-signature/fully-untyped))))

  #| end of module |# )


;;;; module core-macro-transformer: CASE-TYPE

(define-core-transformer (case-type input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used  to  expand  Vicare's  CASE-TYPE  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define (main)
    (syntax-match input-form.stx ()
      ((_ ?expr ?case-clause0 ?case-clause* ...)
       (%build-output-form input-form.stx lexenv.run lexenv.expand
			   ?expr (cons ?case-clause0 ?case-clause*)))
      ))

  (define (%build-output-form input-form.stx lexenv.run lexenv.expand
			      expr.stx case-clause*.stx)
    (let* ((expr.psi     (chi-expr expr.stx lexenv.run lexenv.expand))
	   (expr.sig     (psi.retvals-signature expr.psi)))
      (let* ((matcher.sexp	(let ((arg.sym (gensym "arg")))
				  `(lambda/std (,arg.sym)
				     (cond ,@(%build-branches input-form.stx arg.sym case-clause*.stx)))))
	     (matcher.psi	(chi-expr (bless matcher.sexp) lexenv.run lexenv.expand)))
	(make-psi input-form.stx
		  (build-application no-source
		    (psi.core-expr matcher.psi)
		    (list (psi.core-expr expr.psi)))
		  (psi-application-retvals-signature input-form.stx lexenv.run matcher.psi)))))

  (define (%build-branches input-form.stx arg.sym case-clause*.stx)
    ;;This loop is like MAP, but we want  to detect if the ELSE clause (when present)
    ;;is used only as last clause.
    (let recur ((case-clause*.stx case-clause*.stx))
      (if (pair? case-clause*.stx)
	  (let ((case-clause.stx (car case-clause*.stx)))
	    (cons (syntax-match case-clause.stx (=> else)
		    (((?type) => ?receiver-expr)
		     `((is-a? ,arg.sym ,?type)
		       (,?receiver-expr ,arg.sym)))

		    (((?type) ?body0 ?body* ...)
		     `((is-a? ,arg.sym ,?type)
		       ,?body0 . ,?body*))

		    ((else ?body0 ?body* ...)
		     (if (null? (cdr case-clause*.stx))
			 `(else ,?body0 . ,?body*)
		       (synner "the ELSE clause is not the last clause" case-clause.stx)))

		    (else
		     (synner "invalid clause syntax" case-clause.stx)))
		  (recur (cdr case-clause*.stx))))
	'())))

  (case-define synner
    ((message)
     (synner message #f))
    ((message subform)
     (syntax-violation 'case-type message input-form.stx subform)))

  (main))


;;;; module core-macro-transformer: VALIDATE-TYPED-PROCEDURE-ARGUMENT, VALIDATE-TYPED-RETURN-VALUE

(define-core-transformer (validate-typed-procedure-argument input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's VALIDATE-TYPED-PROCEDURE-ARGUMENT
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type ?idx ?arg)
     (let ((ots (id->object-type-specification __who__ input-form.stx ?type lexenv.run)))
       (unless (identifier? ?arg)
	 (%synner "expected identifier" ?arg))
       ;;This syntax is  used to validate a closure object's  application operands at
       ;;run-time; so  we must insert  a run-time validation predicate.   Using IS-A?
       ;;will not do, because IS-A? also performs expand-time type checking.  This is
       ;;why INTERNAL-RUN-TIME-IS-A? exists.
       (chi-expr (bless
		  `(if (internal-run-time-is-a? ,?arg ,?type)
		       ,?arg
		     (procedure-signature-argument-violation __who__ "invalid object type" ,?idx '(is-a? _ ,?type) ,?arg)))
		 lexenv.run lexenv.expand)))
    ))

(define-core-transformer (validate-typed-return-value input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand Vicare's VALIDATE-TYPED-RETURN-VALUE syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type ?idx ?rv)
     (let ((ots (id->object-type-specification __who__ input-form.stx ?type lexenv.run)))
       (unless (identifier? ?rv)
	 (%synner "expected identifier" ?rv))
       (chi-expr (bless
		  `(if (is-a? ,?rv ,?type)
		       ,?rv
		     (procedure-signature-return-value-violation __who__ "invalid object type" ,?idx '(is-a? _ ,?type) ,?rv)))
		 lexenv.run lexenv.expand)))
    ))


;;;; module core-macro-transformer: ASSERT-SIGNATURE, ASSERT-SIGNATURE-AND-RETURN

(module (assert-signature-transformer assert-signature-and-return-transformer)
  ;;Transformer   function    used   to   expand   Vicare's    ASSERT-SIGNATURE   and
  ;;ASSERT-SIGNATURE-AND-RETURN  syntaxes from  the top-level  built in  environment.
  ;;Expand  the syntax  object INPUT-FORM.STX  in the  context of  the given  LEXENV;
  ;;return a PSI struct.
  ;;
  (define-core-transformer (assert-signature input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?assert-signature ?expr)
       (begin
	 (unless (syntax-object.type-signature? ?assert-signature)
	   (%synner "invalid return values signature" ?assert-signature))
	 (%process-expression __who__ input-form.stx lexenv.run lexenv.expand
			      ?assert-signature ?expr #f)))
      ))

  (define-core-transformer (assert-signature-and-return input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?assert-signature ?expr)
       (begin
	 (unless (syntax-object.type-signature? ?assert-signature)
	   (%synner "invalid return values signature" ?assert-signature))
	 (%process-expression __who__ input-form.stx lexenv.run lexenv.expand
			      ?assert-signature ?expr #t)))
      ))

;;; --------------------------------------------------------------------

  (define (%process-expression who input-form.stx lexenv.run lexenv.expand
			       asrt.stx expr.stx return-values?)
    (define asrt.tags  (syntax-unwrap asrt.stx))
    (define expr.psi   (chi-expr expr.stx lexenv.run lexenv.expand))
    (define expr.sig   (psi.retvals-signature expr.psi))
    (define expr.tags  (type-signature-tags expr.sig))
    (define (%error-mismatching-signatures)
      (expand-time-retvals-signature-violation who input-form.stx expr.stx (make-type-signature asrt.tags) expr.sig))
    ;;Most of  the times either  the signatures will match  at expand-time or  a full
    ;;run-time validation is required.  So let's do first the common cases.
    (cond
     ((syntax-object.type-signature.fully-untyped? asrt.tags)
      ;;The assertion's signature always matches expression's signature.
      (%just-evaluate-the-expression expr.psi return-values?))

     ((syntax-object.type-signature.fully-untyped? expr.tags)
      ;;When the  assertion's signature has  types and the expression's  signature is
      ;;unspecified: always do a run-time validation.
      (%run-time-validation who input-form.stx lexenv.run lexenv.expand
			    asrt.tags expr.psi return-values?))

     ((syntax-object.type-signature.no-return? expr.tags)
      (%just-evaluate-the-expression expr.psi return-values?))

     ((and (syntax-object.type-signature.single-identifier? asrt.tags)
	   (syntax-object.type-signature.single-identifier? expr.tags))
      ;;The common case of single return value.
      (let ((asrt.id (car asrt.tags))
	    (expr.id (car expr.tags)))
	(cond ((top-type-id?     asrt.id)
	       ;;Success!!!  The signatures always match.
	       (%just-evaluate-the-expression expr.psi return-values?))
	      ((top-type-id?     expr.id)
	       (%run-time-validation who input-form.stx lexenv.run lexenv.expand
				     asrt.tags expr.psi return-values?))
	      ((type-identifier-super-and-sub? asrt.id expr.id lexenv.run input-form.stx)
	       ;;Success!!!  The signatures do match.
	       (%just-evaluate-the-expression expr.psi return-values?))
	      (else
	       (%error-mismatching-signatures)))))

     ((and (identifier? asrt.tags)
	   (identifier? expr.tags)
	   (type-identifier-is-list-sub-type? asrt.tags)
	   (type-identifier-is-list-sub-type? expr.tags))
      ;;The case of multiple return values with the same type.
      (if (type-identifier-super-and-sub? asrt.tags expr.tags lexenv.run input-form.stx)
	  ;;Success!!!  We have determined at expand-time that the signatures match.
	  (%just-evaluate-the-expression expr.psi return-values?)
	(%error-mismatching-signatures)))

     ((and (null? asrt.tags)
	   (null? expr.tags))
      ;;The uncommon  case of empty  signatures.  The expression returns  zero values
      ;;and the assertion expects zero values.  We just evaluate the expression.
      (%just-evaluate-the-expression expr.psi return-values?))

     ((syntax-object.type-signature.super-and-sub? asrt.tags expr.tags lexenv.run)
      (%just-evaluate-the-expression expr.psi return-values?))

     (else
      (%error-mismatching-signatures))))

;;; --------------------------------------------------------------------

  (define (%just-evaluate-the-expression expr.psi return-values?)
    ;;This  function  is  used  when  it   is  determined  at  expand-time  that  the
    ;;expression's signature matches the asserted signature.
    ;;
    ;;When we  do not need to  return the actual  values, we return the  void object;
    ;;returning a single  value is faster and, sometimes, returning  void might allow
    ;;detection of unwanted use of return values.
    ;;
    (if return-values?
	expr.psi
      (make-psi (psi.input-form expr.psi)
		(build-sequence no-source
		  (list (psi.core-expr expr.psi)
			(build-void)))
		(make-type-signature/single-void))))

;;; --------------------------------------------------------------------

  (module (%run-time-validation)
    ;;Let's see some examples of run-time validation.  The input form:
    ;;
    ;;   (assert-signature-and-return (<fixnum> <flonum> <string>) ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda () ?expr)
    ;;     (lambda (arg0 arg1 arg2)
    ;;       (values (validate-typed-return-value <fixnum> 0 arg0)
    ;;               (validate-typed-return-value <flonum> 1 arg1)
    ;;               (validate-typed-return-value <string> 2 arg2))))
    ;;
    ;;The input form:
    ;;
    ;;   (assert-signature (<fixnum> <flonum> <string>) ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda () ?expr)
    ;;     (lambda (arg0 arg1 arg2)
    ;;       (begin
    ;;         (validate-typed-return-value <fixnum> 0 arg0)
    ;;         (validate-typed-return-value <flonum> 1 arg1)
    ;;         (validate-typed-return-value <string> 2 arg2)
    ;;         (void))))
    ;;
    ;;The input form:
    ;;
    ;;   (assert-signature-and-return <list-of-fixnums> ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda () ?expr)
    ;;     (lambda rest
    ;;       (apply values
    ;;         (begin
    ;; 	         (fold-left (lambda (idx obj)
    ;;                        (validate-typed-return-value <fixnum> idx obj)
    ;;                        (fxadd1 idx))
    ;; 	           0 rest)
    ;; 	         rest))))
    ;;
    ;;The input form:
    ;;
    ;;   (assert-signature-and-return (<fixnum> <flonum> . <list-of-fixnums>) ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda () ?expr)
    ;;     (lambda rest
    ;;       (apply values
    ;;         (validate-typed-return-value <fixnum> 0 arg0)
    ;;         (validate-typed-return-value <flonum> 1 arg1)
    ;;         (begin
    ;; 	         (fold-left (lambda (idx obj)
    ;;                        (validate-typed-return-value <fixnum> idx obj)
    ;;                        (fxadd1 idx))
    ;; 	           2 rest)
    ;; 	         rest))))
    ;;
    ;;The input form:
    ;;
    ;;   (assert-signature (<fixnum> <flonum> . <list-of-fixnums>) ?expr)
    ;;
    ;;is expanded to:
    ;;
    ;;   (call-with-values
    ;;       (lambda () ?expr)
    ;;     (lambda rest
    ;;       (begin
    ;;         (validate-typed-return-value <fixnum> 0 arg0)
    ;;         (validate-typed-return-value <flonum> 1 arg1)
    ;; 	       (fold-left (lambda (idx obj)
    ;;                      (validate-typed-return-value <fixnum> idx obj)
    ;;                      (fxadd1 idx))
    ;; 	         2 rest))))
    ;;
    (define* (%run-time-validation who input-form.stx lexenv.run lexenv.expand
				   asrt.tags {expr.psi psi?} return-values?)
      (receive (validating-form*.sexp consumer-formals.sexp has-rest?)
	  (let recur ((asrt.tags asrt.tags)
		      (idx       0))
	    (cond ((pair? asrt.tags)
		   (let ((consumer-formal.sym (gensym (string-append "arg" (number->string idx)))))
		     (receive (validating-form*.sexp consumer-formals.sexp has-rest?)
			 (recur (cdr asrt.tags) (fxadd1 idx))
		       (values (cons `(validate-typed-return-value ,(car asrt.tags) ,idx ,consumer-formal.sym)
				     validating-form*.sexp)
			       (cons consumer-formal.sym consumer-formals.sexp)
			       has-rest?))))
		  ((null? asrt.tags)
		   (values '() '() #f))
		  ((list-type-id? asrt.tags)
		   (let ((consumer-formal.sym (gensym "rest")))
		     (values (list consumer-formal.sym) consumer-formal.sym #t)))
		  (else
		   ;;Here ASRT.TAGS is a standalone identifier, sub-type of "<list>".
		   (let* ((consumer-formal.sym  (gensym "rest"))
			  (validating-form.sexp (%build-rest-validator who input-form.stx lexenv.run lexenv.expand
								       asrt.tags idx consumer-formal.sym return-values?)))
		     (values (list validating-form.sexp) consumer-formal.sym #t)))))
	(let* ((producer.core		(build-lambda no-source '() (psi.core-expr expr.psi)))
	       (consumer-body.sexp	(if return-values?
					    (cond (has-rest?
						   `(apply values . ,validating-form*.sexp))
						  ((and (pair? validating-form*.sexp)
							(null? (cdr validating-form*.sexp)))
						   (car validating-form*.sexp))
						  (else
						   `(values . ,validating-form*.sexp)))
					  `(begin ,@validating-form*.sexp (void))))
	       (consumer.stx		(bless `(lambda/std ,consumer-formals.sexp ,consumer-body.sexp)))
	       ;;We want "__who__"  to be bound in the consumer  expression.  So that
	       ;;VALIDATE-TYPED-RETURN-VALUE can use it.
	       (consumer.psi		(let* ((id    (core-prim-id '__who__))
					       (label (id->label id))
					       (descr (label->syntactic-binding-descriptor label lexenv.run)))
					  (case (syntactic-binding-descriptor.type descr)
					    ((displaced-lexical)
					     ;;__who__ is unbound.
					     (receive (lexenv.run lexenv.expand)
						 (fluid-syntax-push-who-on-lexenvs input-form.stx lexenv.run lexenv.expand
										   __who__ (core-prim-id 'assert-signature))
					       (chi-expr consumer.stx lexenv.run lexenv.expand)))
					    (else
					     ;;__who__ is bound.
					     (chi-expr consumer.stx lexenv.run lexenv.expand)))))
	       (consumer.core		(psi.core-expr consumer.psi))
	       (output-signature	(if return-values?
					    (make-type-signature asrt.tags)
					  (make-type-signature/single-void))))
	  (make-psi input-form.stx
		    (build-application no-source
		      (build-primref no-source 'call-with-values)
		      (list producer.core consumer.core))
		    output-signature))))

    (define (%build-rest-validator who input-form.stx lexenv.run lexenv.expand
				   asrt.id idx consumer-formal.sym return-values?)
      (let* ((asrt.ots	(id->object-type-specification who input-form.stx asrt.id lexenv.run))
	     (item.id	(list-type-spec.type-id asrt.ots))
	     (idx.sym	(gensym "idx"))
	     (obj.sym	(gensym "obj")))
	(define validator.sexp
	  `(fold-left (lambda/std (,idx.sym ,obj.sym)
			(validate-typed-return-value ,item.id ,idx.sym ,obj.sym)
			(fxadd1 ,idx.sym))
	     ,idx ,consumer-formal.sym))
	(if return-values?
	    `(begin ,validator.sexp ,consumer-formal.sym)
	  validator.sexp)))

    #| end of module: %RUN-TIME-VALIDATION |# )

  #| end of module: ASSERT-SIGNATURE-TRANSFORMER |# )


;;;; module core-macro-transformer: UNSAFE-CAST-SIGNATURE

(define-core-transformer (unsafe-cast-signature input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  Vicare's  UNSAFE-CAST-SIGNATURE syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?target-signature ?expr)
     (begin
       (unless (syntax-object.type-signature? ?target-signature lexenv.run)
	 (syntax-violation __who__ "invalid type signature" input-form.stx ?target-signature))
       (let* ((target.sig	(make-type-signature ?target-signature))
	      (expr.psi		(chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.core	(psi.core-expr expr.psi))
	      (expr.sig		(psi.retvals-signature expr.psi)))
	 (define (%do-unsafe-cast-signature)
	   (make-psi input-form.stx expr.core target.sig))
	 (cond ((type-signature.super-and-sub? target.sig expr.sig lexenv.run)
		;;Good,  matching  type  signatures:   we  are  generalising  the  type
		;;specification.  For example:
		;;
		;;   (unsafe-cast-signature (<number>) 123)
		;;
		;;which generalises from "<fixnum>" to "<number>".
		(%do-unsafe-cast-signature))
	       ((type-signature.super-and-sub? expr.sig target.sig)
		;;Good,   non-matching  but   compatible   type   signatures:  we   are
		;;specialising the type specification.  For example:
		;;
		;;   (define ({fun <number>})
		;;     123)
		;;   (unsafe-cast-signature (<fixnum>) (fun))
		;;
		;;which specialises from "<number>" to "<fixnum>".
		(%do-unsafe-cast-signature))
	       (else
		;;Bad, non-matching and incompatible signatures.  For example:
		;;
		;;   (unsafe-cast-signature (<fixnum>) "ciao")
		;;
		(raise
		 (condition (make-who-condition __who__)
			    (make-message-condition "source expression's signature is incompatible with the requested target signature")
			    (make-syntax-violation input-form.stx ?expr)
			    (make-irritants-condition (list expr.sig))))))
	 )))
    ))


;;;; module core-macro-transformer: TYPE-OF

(define-core-transformer (type-of input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function used to expand  Vicare's TYPE-OF syntaxes from the top-level
  ;;built in environment.  Expand the syntax  object INPUT-FORM.STX in the context of
  ;;the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?expr)
     (let* ((expr.psi (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.sig (psi.retvals-signature expr.psi)))
       (make-psi input-form.stx
		 (build-data no-source
		   expr.sig)
		 (make-type-signature/single-value (core-prim-id '<type-signature>)))))
    ))


;;;; module core-macro-transformer: TYPE-SUPER-AND-SUB?, SIGNATURE-SUPER-AND-SUB?

(define-core-transformer (type-super-and-sub? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  TYPE-SUPER-AND-SUB?  syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-type ?sub-type)
     (begin
       (id->object-type-specification __who__ input-form.stx ?super-type lexenv.run)
       (id->object-type-specification __who__ input-form.stx ?sub-type   lexenv.run)
       (let ((bool (type-identifier-super-and-sub? ?super-type ?sub-type lexenv.run input-form.stx)))
	 (make-psi input-form.stx
		   (build-data no-source bool)
		   (make-type-signature/single-boolean)))))
    ))

(define-core-transformer (signature-super-and-sub? input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to expand Vicare's  SIGNATURE-SUPER-AND-SUB?  syntaxes
  ;;from the top-level built in environment.  Expand the syntax object INPUT-FORM.STX
  ;;in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?super-signature ?sub-signature)
     (let ((super-signature (syntax-unwrap ?super-signature))
	   (sub-signature   (syntax-unwrap ?sub-signature)))
       (unless (syntax-object.type-signature? super-signature lexenv.run)
	 (syntax-violation __who__
	   "invalid super signature argument" input-form.stx ?super-signature))
       (unless (syntax-object.type-signature? sub-signature lexenv.run)
	 (syntax-violation __who__
	   "invalid sub signature argument" input-form.stx ?sub-signature))
       (let ((bool (syntax-object.type-signature.super-and-sub? super-signature sub-signature lexenv.run)))
	 (make-psi input-form.stx
		   (build-data no-source bool)
		   (make-type-signature/single-boolean)))))
    ))


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;End:
