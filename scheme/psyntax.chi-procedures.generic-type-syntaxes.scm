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


(module (new-transformer
	 delete-transformer
	 is-a?-transformer
	 internal-run-time-is-a?-transformer
	 slot-ref-transformer
	 slot-set!-transformer
	 method-call-transformer
	 unsafe-cast-transformer
	 validate-typed-procedure-argument-transformer
	 validate-typed-return-value-transformer
	 assert-retvals-signature-transformer
	 assert-retvals-signature-and-return-transformer)


;;;; helpers

(define-auxiliary-syntaxes object-type struct-type)

(define-syntax (case-object-type-binding stx)
  ;;This syntax is meant to be used as follows:
  ;;
  ;;   (define-constant __who__ ...)
  ;;   (syntax-match input-stx ()
  ;;     ((_ ?type-id)
  ;;      (identifier? ?type-id)
  ;;      (case-object-type-binding __who__ input-stx ?type-id lexenv.run
  ;;        ((object-type)
  ;;         ...)
  ;;        ((struct-type)
  ;;         ...)))
  ;;     )
  ;;
  ;;where  ?TYPE-ID  is  meant to  be  an  identifier  bound  to a  R6RS  record-type
  ;;descriptor or Vicare's struct-type descriptor.
  ;;
  (sys.syntax-case stx (object-type struct-type)
    ((_ (?who ?input-stx ?type-id ?lexenv ?binding-descriptor)
	((object-type)	?object-body0 ?object-body ...)
	((struct-type)	?struct-body0 ?struct-body ...))
     (and (sys.identifier? (sys.syntax ?who))
	  (sys.identifier? (sys.syntax ?expr-stx))
	  (sys.identifier? (sys.syntax ?type-id))
	  (sys.identifier? (sys.syntax ?lexenv)))
     (sys.syntax
      (begin
	(visit-library-of-imported-syntactic-binding ?who ?input-stx ?type-id ?lexenv)
	(let* ((label                (id->label/or-error ?who ?input-stx ?type-id))
	       (?binding-descriptor  (label->syntactic-binding-descriptor label ?lexenv)))
	  (cond ((object-type-name-binding-descriptor? ?binding-descriptor)
		 ?object-body0 ?object-body ...)
		((struct-type-name-binding-descriptor? ?binding-descriptor)
		 ?struct-body0 ?struct-body ...)
		(else
		 (syntax-violation ?who
		   "neither a struct type nor an object type"
		   ?input-stx ?type-id)))))))
    ))


;;;; module core-macro-transformer: NEW

(define-core-transformer (new input-form.stx lexenv.run lexenv.expand)
  ;;Transformer function  used to  expand NEW  syntaxes from  the top-level  built in
  ;;environment.  Expand the syntax object INPUT-FORM.STX in the context of the given
  ;;LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type-id ?arg* ...)
     (identifier? ?type-id)
     (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run binding)
       ;;For records we  can access the record constructor  syntactic identifier.  We
       ;;just expand to an equivalent of:
       ;;
       ;;   (?maker ?arg ...)
       ;;
       ((object-type)
	;;FIXME It is unknown to be why I have to set this parameter to have:
	;;
	;;   (make-retvals-signature/single-value ?type-id)
	;;
	;;below recognise  ?TYPE-ID as  typed identifier.   This whole  transfomer is
	;;called by a  function that sets the parameter to  this very LEXENV.  (Marco
	;;Maggi; Wed Sep 23, 2015)
	(parametrise ((current-run-lexenv (lambda () lexenv.run)))
	  (let ((rts (syntactic-binding-descriptor.value binding)))
	    (cond ((object-type-spec.constructor-sexp rts)
		   => (lambda (maker.sexp)
			(let* ((maker.stx  (bless maker.sexp))
			       (maker.psi  (chi-expr maker.stx lexenv.run lexenv.expand))
			       (args.psi*  (chi-expr* ?arg* lexenv.run lexenv.expand)))
			  (make-psi input-form.stx
				    (build-application no-source
				      (psi-core-expr maker.psi)
				      (map psi-core-expr args.psi*))
				    (make-retvals-signature/single-value ?type-id)))))
		  (else
		   (%synner "attempt to instantiate object-type with no constructor (abstract type?)" ?type-id))))))

       ;;For structs we want to expand to an equivalent of:
       ;;
       ;;   ((struct-constructor (struct-type-descriptor ?type-id)) ?arg* ...)
       ;;
       ((struct-type)
	(let ((args.psi* (chi-expr* ?arg* lexenv.run lexenv.expand)))
	  (make-psi input-form.stx
		    (build-application no-source
		      (build-application no-source
			(build-primref no-source 'struct-constructor)
			(list (build-data no-source
				(syntactic-binding-descriptor.value binding))))
		      (map psi-core-expr args.psi*))
		    (make-retvals-signature/single-value ?type-id))))
       ))))


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
	      (expr.sig (psi-retvals-signature expr.psi)))
	 (syntax-match (retvals-signature.tags expr.sig) ()
	   ((?type-id)
	    ;;We have determined at expand-time  that the expression returns a single
	    ;;value.
	    (%apply-appropriate-destructor __who__ input-form.stx lexenv.run lexenv.expand ?type-id expr.psi))
	   (?tag
	    (list-tag-id? ?tag)
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
    (case-object-type-binding (who input-form.stx type-id lexenv.run binding)

      ((object-type)
       ;;For records, when  we have access to the record-type  specification, we want
       ;;to expand to an equivalent of:
       ;;
       ;;   (?deafult-record-destructor-id ?expr)
       ;;
       (cond ((object-type-spec.destructor-sexp (syntactic-binding-descriptor.value binding))
	      => (lambda (destructor-sexp)
		   ;;This record type has a default destructor.
		   (make-psi input-form.stx
			     (build-application no-source
			       (psi-core-expr (chi-expr destructor-sexp lexenv.run lexenv.expand))
			       (list (psi-core-expr expr.psi)))
			     (make-retvals-signature/single-top))))
	     (else
	      ;;This record  type has  *no* default  destructor; default  to run-time
	      ;;destruction.
	      ;;
	      ;;Example of usefulness  of defaulting to run-time  destruction: if the
	      ;;object is a  record with a destructor set at  run-time, this way that
	      ;;destructor will be called.
	      (%run-time-destruction input-form.stx lexenv.run lexenv.expand expr.psi))))

      ((struct-type)
       ;;For structs we want to expand to an equivalent of:
       ;;
       ;;   ((internal-applicable-struct-type-destructor (struct-type-descriptor ?type-id)) ?expr)
       ;;
       (make-psi input-form.stx
		 (build-application no-source
		   (build-application no-source
		     (build-primref no-source 'internal-applicable-struct-type-destructor)
		     (list (build-data no-source
			     (syntactic-binding-descriptor.value binding))))
		   (list (psi-core-expr expr.psi)))
		 (make-retvals-signature/single-top)))
      ))

  (define* (%run-time-destruction input-form.stx lexenv.run lexenv.expand {expr.psi psi?})
    (make-psi input-form.stx
	      (build-application no-source
		(build-primref no-source 'internal-delete)
		(list (psi-core-expr expr.psi)))
	      (make-retvals-signature/single-top)))

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
		  (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run binding)
		    ((object-type)
		     (or (object-type-spec.type-predicate-sexp (syntactic-binding-descriptor.value binding))
			 (%synner "type specification has no predicate for run-time use" ?type-id)))

		    ((struct-type)
		     (let ((obj (gensym)))
		       `(lambda (,obj)
			  ($struct/rtd? ,obj (struct-type-descriptor ,?type-id)))))))
		 lexenv.run lexenv.expand))

      ((_ ?expr ?pred-type-id)
       (identifier? ?pred-type-id)
       (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig  (psi-retvals-signature expr.psi)))
	 (define (%run-time-predicate)
	   (%expand-to-run-time-predicate-application input-form.stx lexenv.run lexenv.expand ?pred-type-id expr.psi %synner))
	 (syntax-match (retvals-signature.tags expr.sig) ()
	   ((?expr-type-id)
	    (top-tag-id? ?expr-type-id)
	    (%run-time-predicate))

	   ((?expr-type-id)
	    (if (free-identifier=? ?expr-type-id ?pred-type-id)
		(%make-true-psi input-form.stx ?expr lexenv.run lexenv.expand)
	      (case-object-type-binding (__who__ input-form.stx ?expr-type-id lexenv.run expr-descr)
		((object-type)
		 (case-object-type-binding (__who__ input-form.stx ?pred-type-id lexenv.run pred-descr)
		   ((object-type)
		    (if (object-type-spec.subtype-and-supertype? (syntactic-binding-descriptor.value expr-descr)
								 (syntactic-binding-descriptor.value pred-descr)
								 lexenv.run)
			(%make-true-psi input-form.stx ?expr lexenv.run lexenv.expand)
		      (%make-false-psi input-form.stx ?expr lexenv.run lexenv.expand)))
		   ((struct-type)
		    (%make-false-psi input-form.stx ?expr lexenv.run lexenv.expand))))

		((struct-type)
		 (%make-false-psi input-form.stx ?expr lexenv.run lexenv.expand))
		)))

	   (?expr-type-id
	    (list-tag-id? ?expr-type-id)
	    (%run-time-predicate))

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
    (define expr.core
      (psi-core-expr expr.psi))
    (case-object-type-binding (__module_who__ input-form.stx pred-type-id lexenv.run descr)
      ((object-type)
       (let* ((ots        (syntactic-binding-descriptor.value descr))
	      (pred.sexp  (or (object-type-spec.type-predicate-sexp ots)
			      (%synner "type specification has no predicate for run-time use" pred-type-id)))
	      (pred.stx   (bless pred.sexp))
	      (pred.psi   (chi-expr pred.stx lexenv.run lexenv.expand))
	      (pred.core  (psi-core-expr pred.psi)))
	 (make-psi input-form.stx
		   (build-application input-form.stx
		     pred.core
		     (list expr.core))
		   (psi-application-retvals-signature input-form.stx lexenv.run pred.psi))))

      ((struct-type)
       (let* ((obj.sym    (gensym))
	      (pred.stx   (bless
			   `(lambda (,obj.sym)
			      ($struct/rtd? ,obj.sym (struct-type-descriptor ,pred-type-id)))))
	      (pred.psi   (chi-expr pred.stx lexenv.run lexenv.expand))
	      (pred.core  (psi-core-expr pred.psi)))
	 (make-psi input-form.stx
		   (build-application input-form.stx
		     pred.core
		     (list expr.core))
		   (psi-application-retvals-signature input-form.stx lexenv.run pred.psi))))))

;;; --------------------------------------------------------------------

  (define (%make-true-psi input-form.stx expr.stx lexenv.run lexenv.expand)
    (%make-boolean-psi input-form.stx #t expr.stx lexenv.run lexenv.expand))

  (define (%make-false-psi input-form.stx expr.stx lexenv.run lexenv.expand)
    (%make-boolean-psi input-form.stx #f expr.stx lexenv.run lexenv.expand))

  (define (%make-boolean-psi input-form.stx bool expr.stx lexenv.run lexenv.expand)
    (let* ((expr.psi   (chi-expr expr.stx lexenv.run lexenv.expand))
	   (expr.core  (psi-core-expr expr.psi)))
      (make-psi input-form.stx
		(build-sequence no-source
		  (list expr.core
			(build-data no-source bool)))
		(make-retvals-signature/single-boolean))))

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
	    (expr.sig  (psi-retvals-signature expr.psi))
	    (expr.core (psi-core-expr expr.psi)))
       (case-object-type-binding (__who__ input-form.stx ?pred-type-id lexenv.run descr)
	 ((object-type)
	  (let* ((ots        (syntactic-binding-descriptor.value descr))
		 (pred.sexp  (or (object-type-spec.type-predicate-sexp ots)
				 (%synner "type specification has no predicate for run-time use" ?pred-type-id)))
		 (pred.stx   (bless pred.sexp))
		 (pred.psi   (chi-expr pred.stx lexenv.run lexenv.expand))
		 (pred.core  (psi-core-expr pred.psi)))
	    (make-psi input-form.stx
		      (build-application input-form.stx
			pred.core
			(list expr.core))
		      (psi-application-retvals-signature input-form.stx lexenv.run pred.psi))))

	 ((struct-type)
	  (let* ((obj.sym    (gensym))
		 (pred.stx   (bless
			      `(lambda (,obj.sym)
				 ($struct/rtd? ,obj.sym (struct-type-descriptor ,?pred-type-id)))))
		 (pred.psi   (chi-expr pred.stx lexenv.run lexenv.expand))
		 (pred.core  (psi-core-expr pred.psi)))
	    (make-psi input-form.stx
		      (build-application input-form.stx
			pred.core
			(list expr.core))
		      (psi-application-retvals-signature input-form.stx lexenv.run pred.psi)))))))
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
       (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run descr)
	 ((object-type)
	  (%expand-to-object-accessor-application input-form.stx lexenv.run lexenv.expand descr ?expr ?field-name))
	 ((struct-type)
	  (%expand-to-struct-accessor-application input-form.stx lexenv.run lexenv.expand ?type-id ?expr ?field-name))))

      ;;Wildcard in place of the subject expression.  It must expand to an expression
      ;;that evaluates to an accessor.
      ((_ ?jolly ?field-name ?type-id)
       (and (identifier? ?type-id)
	    (identifier? ?field-name)
	    (underscore-id? ?jolly))
       (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run descr)
	 ((object-type)
	  (%expand-to-object-accessor input-form.stx lexenv.run lexenv.expand descr ?field-name))
	 ((struct-type)
	  (%expand-to-struct-accessor input-form.stx lexenv.run lexenv.expand ?type-id ?field-name))))

      ;;Missing type identifier.  Try to retrieve  the type from the signature of the
      ;;subject expression.
      ((_ ?expr ?field-name)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name))
       (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig  (psi-retvals-signature expr.psi)))
	 (define (%error-unknown-type)
	   (%synner "unable to determine type of expression at expand-time" ?expr))
	 (syntax-match (retvals-signature.tags expr.sig) ()
	   ((?type-id)
	    (top-tag-id? ?type-id)
	    (%error-unknown-type))

	   ((?type-id)
	    (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run descr)
	      ((object-type)
	       (%expand-to-object-accessor-application-post input-form.stx lexenv.run lexenv.expand descr    expr.psi ?field-name))
	      ((struct-type)
	       (%expand-to-struct-accessor-application-post input-form.stx lexenv.run lexenv.expand ?type-id expr.psi ?field-name))))


	   (?type-id
	    (list-tag-id? ?type-id)
	    ;;Damn  it!!!   The  expression's  return  values  have  fully  UNspecified
	    ;;signature.
	    (%error-unknown-type))

	   (_
	    ;;We have  determined at expand-time  that the expression  returns multiple
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
						   descr {expr.stx syntax-object?} field-name.id)
    (cond ((let ((rts (syntactic-binding-descriptor.value descr)))
	     (object-type-spec.safe-accessor-sexp rts (identifier->symbol field-name.id) lexenv.run))
	   => (lambda (accessor.sexp)
		(chi-expr (bless
			   `(,accessor.sexp ,expr.stx))
			  lexenv.run lexenv.expand)))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

  (define* (%expand-to-struct-accessor-application input-form.stx lexenv.run lexenv.expand
						   type-id {expr.stx syntax-object?} field-name.id)
    (let* ((std         (%struct-type-id->std __module_who__ input-form.stx type-id lexenv.run))
	   (field-names (struct-type-field-names std))
	   (field-idx   (%struct-field-name->struct-field-idx __module_who__ input-form.stx field-names field-name.id)))
      (chi-expr (bless
		 `(struct-and-std-ref ,expr.stx ,field-idx (struct-type-descriptor ,type-id)))
		lexenv.run lexenv.expand)))

;;; --------------------------------------------------------------------

  (define (%expand-to-object-accessor input-form.stx lexenv.run lexenv.expand
				      descr field-name.id)
    (cond ((let ((rts (syntactic-binding-descriptor.value descr)))
	     (object-type-spec.safe-accessor-sexp rts (identifier->symbol field-name.id) lexenv.run))
	   => (lambda (accessor.sexp)
		(chi-expr (bless accessor.sexp) lexenv.run lexenv.expand)))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

  (define (%expand-to-struct-accessor input-form.stx lexenv.run lexenv.expand
				      type-id field-name.id)
    (let* ((std         (%struct-type-id->std __module_who__ input-form.stx type-id lexenv.run))
	   (field-names (struct-type-field-names std))
	   (field-idx   (%struct-field-name->struct-field-idx __module_who__ input-form.stx field-names field-name.id))
	   (stru.sym    (gensym "stru")))
      (chi-expr (bless
		 `(lambda (,stru.sym)
		    (struct-and-std-ref ,stru.sym ,field-idx (struct-type-descriptor ,type-id))))
		lexenv.run lexenv.expand)))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-accessor-application-post input-form.stx lexenv.run lexenv.expand descr {expr.psi psi?} field-name.id)
    (let ((rts (syntactic-binding-descriptor.value descr)))
      (cond ((object-type-spec.safe-accessor-sexp rts (identifier->symbol field-name.id) lexenv.run)
	     => (lambda (accessor.sexp)
		  (let* ((accessor.psi  (chi-expr (bless accessor.sexp) lexenv.run lexenv.expand))
			 (accessor.core (psi-core-expr accessor.psi))
			 (expr.core     (psi-core-expr expr.psi)))
		    (make-psi input-form.stx
			      (build-application input-form.stx
				accessor.core
				(list expr.core))
			      (psi-application-retvals-signature input-form.stx lexenv.run accessor.psi)))))
	    (else
	     (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id)))))

  (define* (%expand-to-struct-accessor-application-post input-form.stx lexenv.run lexenv.expand type-id {expr.psi psi?} field-name.id)
    (let* ((std         (%struct-type-id->std __module_who__ input-form.stx type-id lexenv.run))
	   (field-names (struct-type-field-names std))
	   (field-idx   (%struct-field-name->struct-field-idx __module_who__ input-form.stx field-names field-name.id))
	   (expr.core   (psi-core-expr expr.psi)))
      (make-psi input-form.stx
		(build-application input-form.stx
		  (build-primref no-source 'struct-and-std-ref)
		  (list expr.core
			(build-data no-source field-idx)
			(build-data no-source std)))
		(make-retvals-signature/single-void))))

  #| end of module: SLOT-REF-TRANSFORMER |# )


;;;; module core-macro-transformer: SLOT-SET!

(module (slot-set!-transformer)

  (define-module-who slot-set!)

  (define-core-transformer (slot-set! input-form.stx lexenv.run lexenv.expand)
    ;;Transformer  function  used  to  expand Vicare's  SLOT-SET!  syntaxes  from  the
    ;;top-level built in environment.  Expand the syntax object INPUT-FORM.STX in the
    ;;context of the given LEXENV; return a PSI struct.
    ;;
    (syntax-match input-form.stx ()
      ;;Everything included.  It must expand to an mutator application.
      ((_ ?expr ?field-name ?type-id ?new-value)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name)
	    (identifier? ?type-id))
       (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run descr)
	 ((object-type)
	  (%expand-to-object-mutator-application input-form.stx lexenv.run lexenv.expand descr ?expr ?field-name ?new-value))
	 ((struct-type)
	  (%expand-to-struct-mutator-application input-form.stx lexenv.run lexenv.expand ?type-id ?expr ?field-name ?new-value))))

      ;;Wildcard in place of the subject expression.  It must expand to an expression
      ;;that evaluates to an mutator.
      ((_ ?jolly1 ?field-name ?type-id ?jolly2)
       (and (identifier? ?type-id)
	    (identifier? ?field-name)
	    (underscore-id? ?jolly1)
	    (underscore-id? ?jolly2))
       (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run descr)
	 ((object-type)
	  (%expand-to-object-mutator input-form.stx lexenv.run lexenv.expand descr ?field-name))
	 ((struct-type)
	  (%expand-to-struct-mutator input-form.stx lexenv.run lexenv.expand ?type-id ?field-name))))

      ;;Missing type identifier.  Try to retrieve  the type from the signature of the
      ;;subject expression.
      ((_ ?expr ?field-name ?new-value)
       (and (not (underscore-id? ?expr))
	    (identifier? ?field-name))
       (let* ((expr.psi  (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig  (psi-retvals-signature expr.psi)))
	 (define (%error-unknown-type)
	   (%synner "unable to determine type of expression at expand-time" ?expr))
	 (syntax-match (retvals-signature.tags expr.sig) ()
	   ((?type-id)
	    (top-tag-id? ?type-id)
	    (%error-unknown-type))

	   ((?type-id)
	    (case-object-type-binding (__who__ input-form.stx ?type-id lexenv.run descr)
	      ((object-type)
	       (%expand-to-object-mutator-application-post input-form.stx lexenv.run lexenv.expand descr    expr.psi ?field-name ?new-value))
	      ((struct-type)
	       (%expand-to-struct-mutator-application-post input-form.stx lexenv.run lexenv.expand ?type-id expr.psi ?field-name ?new-value))))


	   (?type-id
	    (list-tag-id? ?type-id)
	    ;;Damn  it!!!   The  expression's  return  values  have  fully  UNspecified
	    ;;signature.
	    (%error-unknown-type))

	   (_
	    ;;We have  determined at expand-time  that the expression  returns multiple
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
						  descr {expr.stx syntax-object?} field-name.id new-value.stx)
    (cond ((let ((rts (syntactic-binding-descriptor.value descr)))
	     (object-type-spec.safe-mutator-sexp rts (identifier->symbol field-name.id) lexenv.run))
	   => (lambda (mutator.sexp)
		(chi-expr (bless
			   `(,mutator.sexp ,expr.stx ,new-value.stx))
			  lexenv.run lexenv.expand)))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

  (define* (%expand-to-struct-mutator-application input-form.stx lexenv.run lexenv.expand
						  type-id {expr.stx syntax-object?} field-name.id new-value.stx)
    (let* ((std         (%struct-type-id->std __module_who__ input-form.stx type-id lexenv.run))
	   (field-names (struct-type-field-names std))
	   (field-idx   (%struct-field-name->struct-field-idx __module_who__ input-form.stx field-names field-name.id)))
      (chi-expr (bless
		 `(struct-and-std-set! ,expr.stx ,field-idx (struct-type-descriptor ,type-id) ,new-value.stx))
		lexenv.run lexenv.expand)))

;;; --------------------------------------------------------------------

  (define (%expand-to-object-mutator input-form.stx lexenv.run lexenv.expand
				     descr field-name.id)
    (cond ((let ((rts (syntactic-binding-descriptor.value descr)))
	     (object-type-spec.safe-mutator-sexp rts (identifier->symbol field-name.id) lexenv.run))
	   => (lambda (mutator.sexp)
		(chi-expr (bless mutator.sexp) lexenv.run lexenv.expand)))
	  (else
	   (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id))))

  (define (%expand-to-struct-mutator input-form.stx lexenv.run lexenv.expand
				     type-id field-name.id)
    (let* ((std         (%struct-type-id->std __module_who__ input-form.stx type-id lexenv.run))
	   (field-names (struct-type-field-names std))
	   (field-idx   (%struct-field-name->struct-field-idx __module_who__ input-form.stx field-names field-name.id))
	   (stru.sym    (gensym "stru"))
	   (new-val.sym (gensym "new-val")))
      (chi-expr (bless
		 `(lambda (,stru.sym ,new-val.sym)
		    (struct-and-std-set! ,stru.sym ,field-idx (struct-type-descriptor ,type-id) ,new-val.sym)))
		lexenv.run lexenv.expand)))

;;; --------------------------------------------------------------------

  (define* (%expand-to-object-mutator-application-post input-form.stx lexenv.run lexenv.expand
						       descr {expr.psi psi?} field-name.id new-value.stx)
    (let ((rts (syntactic-binding-descriptor.value descr)))
      (cond ((object-type-spec.safe-mutator-sexp rts (identifier->symbol field-name.id) lexenv.run)
	     => (lambda (mutator.sexp)
		  (let* ((mutator.psi    (chi-expr (bless mutator.sexp) lexenv.run lexenv.expand))
			 (mutator.core   (psi-core-expr mutator.psi))
			 (expr.core      (psi-core-expr expr.psi))
			 (new-value.psi  (chi-expr new-value.stx lexenv.run lexenv.expand))
			 (new-value.core (psi-core-expr new-value.psi)))
		    (make-psi input-form.stx
			      (build-application input-form.stx
				mutator.core
				(list expr.core new-value.core))
			      (psi-application-retvals-signature input-form.stx lexenv.run mutator.psi)))))
	    (else
	     (syntax-violation __module_who__ "unknown field name" input-form.stx field-name.id)))))

  (define* (%expand-to-struct-mutator-application-post input-form.stx lexenv.run lexenv.expand
						       type-id {expr.psi psi?} field-name.id new-value.stx)
    (let* ((std             (%struct-type-id->std __module_who__ input-form.stx type-id lexenv.run))
	   (field-names     (struct-type-field-names std))
	   (field-idx       (%struct-field-name->struct-field-idx __module_who__ input-form.stx field-names field-name.id))
	   (expr.core       (psi-core-expr expr.psi))
	   (new-value.psi   (chi-expr new-value.stx lexenv.run lexenv.expand))
	   (new-value.core  (psi-core-expr new-value.psi)))
      (make-psi input-form.stx
		(build-application input-form.stx
		  (build-primref no-source 'struct-and-std-set!)
		  (list expr.core
			(build-data no-source field-idx)
			(build-data no-source std)
			new-value.core))
		(make-retvals-signature/single-void))))

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
	      (subject-expr.sig	(psi-retvals-signature subject-expr.psi)))
	 (define-syntax-rule (%late-binding)
	   (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
						method-name.sym subject-expr.psi ?arg*))
	 (syntax-match (retvals-signature.tags subject-expr.sig) ()
	   ((?type-id)
	    (top-tag-id? ?type-id)
	    (%late-binding))

	   ((?type-id)
	    (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						  ?method-name method-name.sym
						  ?type-id
						  ?subject-expr subject-expr.psi ?arg*))

	   (?type-id
	    (list-tag-id? ?type-id)
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

  (module (%expand-to-early-binding-method-call)

    (define (%expand-to-early-binding-method-call input-form.stx lexenv.run lexenv.expand
						  method-name.id method-name.sym
						  type-id
						  subject-expr.stx subject-expr.psi arg*.stx)
      (case-object-type-binding (__module_who__ input-form.stx type-id lexenv.run type-id-descr)
	((object-type)
	 (%object-type-method-call input-form.stx lexenv.run lexenv.expand
				   method-name.id method-name.sym
				   type-id type-id-descr
				   subject-expr.stx subject-expr.psi arg*.stx))

	((struct-type)
	 (%struct-type-method-call input-form.stx lexenv.run lexenv.expand
				     method-name.id method-name.sym
				     type-id
				     subject-expr.stx subject-expr.psi arg*.stx))
	))

    ;;;

    (define (%object-type-method-call input-form.stx lexenv.run lexenv.expand
				      method-name.id method-name.sym
				      type-id type-id-descr
				      subject-expr.stx subject-expr.psi arg*.stx)
      (let* ((expr.core  (psi-core-expr subject-expr.psi))
	     (rts        (syntactic-binding-descriptor.value type-id-descr)))
	(cond
	 ;;Look for a matching method name.
	 ((object-type-spec.applicable-method-sexp rts method-name.sym lexenv.run)
	  => (lambda (method.sexp)
	       ;;A matching method name exists.
	       (let* ((method.psi  (chi-expr (bless method.sexp) lexenv.run lexenv.expand))
		      (method.core (psi-core-expr method.psi))
		      (arg*.psi    (chi-expr* arg*.stx lexenv.run lexenv.expand))
		      (arg*.core   (map psi-core-expr arg*.psi)))
		 (make-psi input-form.stx
			   (build-application input-form.stx
			     method.core
			     (cons expr.core arg*.core))
			   (psi-application-retvals-signature input-form.stx lexenv.run method.psi)))))
	 ;;If the input form has the right syntax: look for a matching field name for
	 ;;accessor application.
	 ((and (null? arg*.stx)
	       (object-type-spec.safe-accessor-sexp rts method-name.sym lexenv.run))
	  => (lambda (accessor.sexp)
	       ;;A matching field name exists.
	       (let* ((accessor.psi  (chi-expr (bless accessor.sexp) lexenv.run lexenv.expand))
		      (accessor.core (psi-core-expr accessor.psi)))
		 (make-psi input-form.stx
			   (build-application input-form.stx
			     accessor.core
			     (list expr.core))
			   (psi-application-retvals-signature input-form.stx lexenv.run accessor.psi)))))
	 ;;If the input form has the right syntax: look for a matching field name for
	 ;;mutator application.
	 ((and (pair? arg*.stx)
	       (null? (cdr arg*.stx))
	       (object-type-spec.safe-mutator-sexp rts method-name.sym lexenv.run))
	  => (lambda (mutator.sexp)
	       ;;A matching field name exists.
	       (let* ((mutator.psi  (chi-expr (bless mutator.sexp) lexenv.run lexenv.expand))
		      (mutator.core (psi-core-expr mutator.psi))
		      (arg.psi      (chi-expr (car arg*.stx) lexenv.run lexenv.expand))
		      (arg.core     (psi-core-expr arg.psi)))
		 (make-psi input-form.stx
			   (build-application input-form.stx
			     mutator.core
			     (list expr.core arg.core))
			   (psi-application-retvals-signature input-form.stx lexenv.run mutator.psi)))))
	 (else
	  (raise
	   (condition (make-who-condition __module_who__)
		      (make-message-condition "unknown method name for type of subject expression")
		      (make-syntax-violation input-form.stx subject-expr.stx)
		      (make-type-syntactic-identifier-condition type-id)
		      (make-type-method-name-condition method-name.sym)))))))

    ;;;

    (define (%struct-type-method-call input-form.stx lexenv.run lexenv.expand
				      method-name.id method-name.sym
				      type-id
				      subject-expr.stx subject-expr.psi arg*.stx)
      (define expr.core
	(psi-core-expr subject-expr.psi))
      (cond ((null? arg*.stx)
	     ;;No arguments, let's go for an accessor application.
	     (let* ((std         (%struct-type-id->std __module_who__ input-form.stx type-id lexenv.run))
		    (field-names (struct-type-field-names std))
		    (field-idx   (%struct-field-name->struct-field-idx __module_who__ input-form.stx field-names method-name.id)))
	       (make-psi input-form.stx
			 (build-application input-form.stx
			   (build-primref no-source 'struct-and-std-ref)
			   (list expr.core
				 (build-data no-source field-idx)
				 (build-data no-source std)))
			 (make-retvals-signature/single-top))))
	    ((and (pair? arg*.stx)
		  (null? (cdr arg*.stx)))
	     ;;Only one argument, let's go for a mutator application.
	     (let* ((std         (%struct-type-id->std __module_who__ input-form.stx type-id lexenv.run))
		    (field-names (struct-type-field-names std))
		    (field-idx   (%struct-field-name->struct-field-idx __module_who__ input-form.stx field-names method-name.id))
		    (arg.psi     (chi-expr (car arg*.stx) lexenv.run lexenv.expand))
		    (arg.core    (psi-core-expr arg.psi)))
	       (make-psi input-form.stx
			 (build-application input-form.stx
			   (build-primref no-source 'struct-and-std-set!)
			   (list expr.core
				 (build-data no-source field-idx)
				 (build-data no-source std)
				 arg.core))
			 (make-retvals-signature/single-void))))
	    (else
	     (raise
	      (condition (make-who-condition __module_who__)
			 (make-message-condition "unsupported method call operation on struct-type of subject expression")
			 (make-syntax-violation input-form.stx subject-expr.stx)
			 (make-type-syntactic-identifier-condition type-id))))))

    #| end of module: %EXPAND-TO-EARLY-BINDING-METHOD-CALL |# )

;;; --------------------------------------------------------------------

  (define (%expand-to-late-binding-method-call input-form.stx lexenv.run lexenv.expand
					       method-name.sym subject-expr.psi arg*.stx)
    ;;The  type of  the  values returned  by  the subject  expression  is unknown  at
    ;;expand-time; so we  expand to an expression that searches  at run-time a method
    ;;matching the  given name.  In other  words: we default to  "late binding" (also
    ;;known as "run-time dispatching").
    ;;
    (let* ((expr.core	(psi-core-expr subject-expr.psi))
	   (arg*.psi	(chi-expr* arg*.stx lexenv.run lexenv.expand))
	   (arg*.core	(map psi-core-expr arg*.psi)))
      (make-psi input-form.stx
		(build-application input-form.stx
		  (build-primref no-source 'method-call-late-binding)
		  (cons* (build-data no-source method-name.sym)
			 expr.core
			 arg*.core))
		(make-retvals-signature/fully-unspecified))))

  #| end of module |# )


;;;; module core-macro-transformer: UNSAFE-CAST

(define-core-transformer (unsafe-cast input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function  used to  expand  Vicare's  UNSAFE-CAST syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?target-type ?expr)
     (let* ((target-descr (id->object-type-binding-descriptor __who__ input-form.stx ?target-type lexenv.run))
	    (expr.psi     (chi-expr ?expr lexenv.run lexenv.expand))
	    (expr.core    (psi-core-expr expr.psi))
	    (expr.sig     (psi-retvals-signature expr.psi)))
       (define (%do-unsafe-cast)
	 (make-psi input-form.stx expr.core (make-retvals-signature/single-value ?target-type)))
       (syntax-match (retvals-signature.tags expr.sig) ()
	 ((?source-type)
	  (cond ((top-tag-id? ?target-type)
		 ;;Whatever  the  type  of  the expression:  casting  to  "<top>"  is
		 ;;nothing: nothing to do, just return the PSI.
		 expr.psi)
		((top-tag-id? ?source-type)
		 ;;The expression has "<top>" as  single-value type: cast the type to
		 ;;the target one.
		 (%do-unsafe-cast))
		((let* ((target-spec  (syntactic-binding-descriptor.value target-descr))
			(source-descr (id->object-type-binding-descriptor __who__ input-form.stx ?source-type lexenv.run))
			(source-spec  (syntactic-binding-descriptor.value source-descr)))
		   (object-type-spec.subtype-and-supertype? source-spec target-spec lexenv.run))
		 ;;The expression's type is a subtype  of the target type: nothing to
		 ;;do, just return it.
		 expr.psi)
		(else
		 (raise
		  (condition (make-who-condition __who__)
			     (make-message-condition "expression type is incompatible with the requested tag")
			     (make-syntax-violation input-form.stx ?expr)
			     (make-irritants-condition (list expr.sig)))))))

	 (?source-type
	  (list-tag-id? ?source-type)
	  ;;The expression return avlues are fully unspecified.
	  (%do-unsafe-cast))

	 (_
	  ;;We have determined at expand-time  that the expression returns multiple
	  ;;values.
	  (raise
	   (condition (make-who-condition __who__)
		      (make-message-condition "subject expression of unsafe cast returns multiple values")
		      (make-syntax-violation input-form.stx ?expr)
		      (make-irritants-condition (list expr.sig)))))
	 )))
    ))


;;;; module core-macro-transformer: VALIDATE-TYPED-PROCEDURE-ARGUMENT, VALIDATE-TYPED-RETURN-VALUE

(define (%detailed-type-identifier-validation who input-form.stx lexenv.run type.id)
  ;;To  be  used to  validate  TYPE.ID  as  bound  identifier having  an  object-type
  ;;specification in  its syntactic binding's  descriptor.  If successful  return the
  ;;instance of  "<object-type-spec>" as  object-type specification  (OTS), otherwise
  ;;raise an exception.
  ;;
  (unless (identifier? type.id)
    (syntax-violation who
      "expected identifier as type specification"
      input-form.stx type.id))
  (let* ((label (id->label/or-error who input-form.stx type.id))
	 (descr (label->syntactic-binding-descriptor label lexenv.run)))
    (when (eq? 'displaced-lexical (syntactic-binding-descriptor.type descr))
      (syntax-violation who "unbound label for type identifier" input-form.stx type.id))
    (receive-and-return (spec)
	(syntactic-binding-descriptor.value descr)
      (unless (object-type-spec? spec)
	(syntax-violation who
	  "expected type identifier but given identifier does not represent an object-type"
	  input-form.stx type.id)))))

(define-core-transformer (validate-typed-procedure-argument input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function used  to expand  Vicare's VALIDATE-TYPED-PROCEDURE-ARGUMENT
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type ?idx ?arg)
     (let ((ots (%detailed-type-identifier-validation __who__ input-form.stx lexenv.run ?type)))
       (unless (identifier? ?arg)
	 (%synner "expected identifier" ?arg))
       (unless (let ((idx (syntax->datum ?idx)))
		 (or (not idx)
		     (fixnum? idx)
		     (fxpositive? idx)))
	 (%synner "expected 1-based fixnum as index of argument") ?idx)
       ;;This syntax is  used to validate a closure object's  application operands at
       ;;run-time; so  we must insert  a run-time validation predicate.   Using IS-A?
       ;;will not do, because IS-A? also performs expand-time type checking.  This is
       ;;why INTERNAL-RUN-TIME-IS-A? exists.
       (chi-expr (bless
		  `(unless (internal-run-time-is-a? ,?arg ,?type)
		     (procedure-signature-argument-violation __who__ "invalid object type" ,?idx '(is-a? _ ,?type) ,?arg)))
		 lexenv.run lexenv.expand)))
    ))

(define-core-transformer (validate-typed-return-value input-form.stx lexenv.run lexenv.expand)
  ;;Transformer  function   used  to  expand   Vicare's  VALIDATE-TYPED-RETURN-VALUE
  ;;syntaxes  from the  top-level built  in  environment.  Expand  the syntax  object
  ;;INPUT-FORM.STX in the context of the given LEXENV; return a PSI struct.
  ;;
  (syntax-match input-form.stx ()
    ((_ ?type ?idx ?rv)
     (let ((ots (%detailed-type-identifier-validation __who__ input-form.stx lexenv.run ?type)))
       (unless (identifier? ?rv)
	 (%synner "expected identifier" ?rv))
       (unless (let ((idx (syntax->datum ?idx)))
		 (or (not idx)
		     (fixnum? idx)
		     (fxpositive? idx)))
	 (%synner "expected 1-based fixnum as index of return value") ?idx)
       (chi-expr (bless
		  `(unless (is-a? ,?rv ,?type)
		     (procedure-signature-return-value-violation __who__ "invalid object type" ,?idx '(is-a? _ ,?type) ,?rv)))
		 lexenv.run lexenv.expand)))
    ))


;;;; module core-macro-transformer: ASSERT-RETVALS-SIGNATURE

(module (assert-retvals-signature-transformer)
  ;;Transformer  function  used  to  expand Vicare's  ASSERT-RETVALS-SIGNATURE  syntaxes  from  the
  ;;top-level built in  environment.  Expand the syntax object  INPUT-FORM.STX in the
  ;;context of the given LEXENV; return a PSI struct.
  ;;
  (define-core-transformer (assert-retvals-signature input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?retvals-signature ?expr)
       (syntax-object.retvals-signature? ?retvals-signature)
       (let* ((asserted.sig (make-retvals-signature ?retvals-signature))
	      (expr.psi     (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig     (psi-retvals-signature expr.psi)))
	 (cond ((list-tag-id? ?retvals-signature)
		;;If we are here the input form is:
		;;
		;;   (assert-retvals-signature <list> ?expr)
		;;
		;;and  any tuple  of returned  values returned  by ?EXPR  is of  type
		;;"<list>".
		(%just-evaluate-the-expression expr.psi))

	       ((retvals-signature.single-top-tag? asserted.sig)
		;;If we are here the input form is:
		;;
		;;   (assert-retvals-signature (<top>) ?expr)
		;;
		;;so it is  enough to make sure that the  expression returns a single
		;;value, whatever its type.
		(syntax-match (retvals-signature.tags expr.sig) ()
		  ((?tag)
		   ;;Success!!!   We   have  determined   at  expand-time   that  the
		   ;;expression returns a single value.
		   (%just-evaluate-the-expression expr.psi))
		  (?tag
		   (list-tag-id? ?tag)
		   ;;Damn   it!!!   The   expression's  return   values  have   fully
		   ;;unspecified signature; we need to insert a run-time check.
		   (%run-time-validation-of-single-value input-form.stx lexenv.run lexenv.expand
							 expr.psi))
		  (_
		   ;;The  horror!!!   We have  established  at  expand-time that  the
		   ;;expression returns multiple values; assertion failed.
		   (expand-time-retvals-signature-violation __who__ input-form.stx ?expr asserted.sig expr.sig))
		  ))

	       ((retvals-signature.partially-unspecified? expr.sig)
		;;Damn it!!!  The expression has  no type specification or  a partial
		;;type specification; we have to insert a run-time check.
		;;
		;;FIXME We can  do better here by inserting the  run-time checks only
		;;for  the "<top>"  return values,  rather than  for all  the values.
		;;(Marco Maggi; Fri Apr 4, 2014)
		(%run-time-validation input-form.stx lexenv.run lexenv.expand
				      asserted.sig expr.psi))

	       ((retvals-signature.super-and-sub? asserted.sig expr.sig)
		;;Success!!!  We  have established  at expand-time that  the returned
		;;values are valid; assertion succeeded.
		(%just-evaluate-the-expression expr.psi))

	       (else
		;;The horror!!!  We  have established at expand-time  that the returned
		;;values are of the wrong type; assertion failed.
		(expand-time-retvals-signature-violation __who__ input-form.stx ?expr asserted.sig expr.sig)))))

       ((_ ?retvals-signature ?expr)
	;;Let's use a descriptive error message here.
	(%synner "invalid return values signature" ?retvals-signature))
       ))

  (define* (%run-time-validation-of-single-value input-form.stx lexenv.run lexenv.expand
						 expr.psi)
    ;;This function  is used when the  syntax use specifies that  the expression must
    ;;return a single value, of any type.
    ;;
    (let* ((cwv.core     (build-primref no-source 'call-with-values))
	   (expr.core    (psi-core-expr expr.psi))
	   (checker.sexp (let ((arg.sym  (gensym "arg"))
			       (rest.sym (gensym "rest")))
			   `(lambda (,arg.sym . ,rest.sym)
			      (if (null? ,rest.sym)
				  ,arg.sym
				(assertion-violation #f "expected single return value" (cons ,arg.sym ,rest.sym))))))
	   (checker.psi  (chi-expr (bless checker.sexp) lexenv.run lexenv.expand))
	   (checker.core (psi-core-expr checker.psi)))
      (make-psi input-form.stx
		(build-application no-source
		  cwv.core
		  (list (build-lambda no-source
			  '()
			  expr.core)
			checker.core))
		(make-retvals-signature/single-top))))

  (define* (%run-time-validation input-form.stx lexenv.run lexenv.expand
				 {asserted.sig retvals-signature?} {expr.psi psi?})
    (define expr.core (psi-core-expr         expr.psi))
    (define expr.sig  (psi-retvals-signature expr.psi))
    ;;Here we know that ASSERTED.SIG is a  valid retvals signature, so we can be less
    ;;strict in the patterns.
    (syntax-match (retvals-signature.tags asserted.sig) ()
      ((?rv-tag* ...)
       (let* ((TMP*         (generate-temporaries ?rv-tag*))
	      (IDX*         (%fxiota 1 TMP*))
	      (checker.psi  (chi-expr (bless
				       `(lambda ,TMP*
					  ,@(map (lambda (tmp idx tag)
						   `(validate-typed-return-value ,tag ,idx ,tmp))
					      TMP* IDX* ?rv-tag*)
					  (void)))
				      lexenv.run lexenv.expand)))
	 (%run-time-check-output-form input-form.stx lexenv.run lexenv.expand
				      expr.core checker.psi)))

      ((?rv-tag* ... . ?rv-rest-tag)
       (let* ((TMP*         (generate-temporaries ?rv-tag*))
	      (IDX*         (%fxiota 1 TMP*))
	      (rest.sym     (gensym "rest"))
	      (obj.sym      (gensym "obj"))
	      (idx.sym      (gensym "idx"))
	      (checker.psi  (chi-expr (bless
				       `(lambda (,@TMP* . ,rest.sym)
					  ,@(map (lambda (tmp idx tag)
						   `(validate-typed-return-value ,tag ,idx ,tmp))
					      TMP* IDX* ?rv-tag*)
					  (fold-left (lambda (,idx.sym ,obj.sym)
						       (validate-typed-return-value ,?rv-rest-tag ,idx.sym ,obj.sym)
						       (fxadd1 ,idx.sym))
					    ,(add1 (length TMP*)) ,rest.sym)
					  (void)))
				      lexenv.run lexenv.expand)))
	 (%run-time-check-output-form  input-form.stx lexenv.run lexenv.expand
				       expr.core checker.psi)))

      (?rv-args-tag
       (let* ((args.sym     (gensym "args"))
	      (obj.sym      (gensym "obj"))
	      (idx.sym      (gensym "idx"))
	      (checker.psi  (chi-expr (bless
				       `(lambda ,args.sym
					  (fold-left (lambda (,idx.sym ,obj.sym)
						       (validate-typed-return-value ,?rv-args-tag ,idx.sym ,obj.sym)
						       (fxadd1 ,idx.sym))
					    1 ,args.sym)
					  (void)))
				      lexenv.run lexenv.expand)))
	 (%run-time-check-output-form  input-form.stx lexenv.run lexenv.expand
				       expr.core checker.psi)))
      ))

  (define (%just-evaluate-the-expression expr.psi)
    (make-psi (psi-stx expr.psi)
	      (build-sequence no-source
		(list (psi-core-expr expr.psi)
		      (build-void)))
	      ;;We know that we are returning a single void argument.
	      (make-retvals-signature/single-top)))

  (define (%run-time-check-output-form input-form.stx lexenv.run lexenv.expand
				       expr.core checker.psi)
    ;;We build a core language expression as follows:
    ;;
    ;;   (call-with-values
    ;;        (lambda () ?expr)
    ;;     (lambda ?formals
    ;;       ?check-form ...
    ;;       (void)))
    ;;
    (let* ((cwv.core     (build-primref no-source 'call-with-values))
	   (checker.core (psi-core-expr checker.psi)))
      (make-psi input-form.stx
		(build-application no-source
		  cwv.core
		  (list (build-lambda no-source '() expr.core)
			checker.core))
		;;We know that we are returning a single void argument.
		(make-retvals-signature/single-top))))

  #| end of module: ASSERT-RETVALS-SIGNATURE-TRANSFORMER |# )


;;;; module core-macro-transformer: ASSERT-RETVALS-SIGNATURE-AND-RETURN

(module (assert-retvals-signature-and-return-transformer)
  ;;Transformer function used to  expand Vicare's ASSERT-RETVALS-SIGNATURE-AND-RETURN syntaxes from
  ;;the top-level built  in environment.  Expand the syntax  object INPUT-FORM.STX in
  ;;the context of the given LEXENV; return a PSI struct.
  ;;
  (define-core-transformer (assert-retvals-signature-and-return input-form.stx lexenv.run lexenv.expand)
    (syntax-match input-form.stx ()
      ((_ ?retvals-signature ?expr)
       (syntax-object.retvals-signature? ?retvals-signature)
       (let* ((asserted.sig (make-retvals-signature ?retvals-signature))
	      (expr.psi     (chi-expr ?expr lexenv.run lexenv.expand))
	      (expr.sig     (psi-retvals-signature expr.psi)))
	 (cond ((list-tag-id? ?retvals-signature)
		;;If we are here the input form is:
		;;
		;;   (assert-retvals-signature-and-return <list> ?expr)
		;;
		;;and  any tuple  of returned  values returned  by ?EXPR  is of  type
		;;"<list>".  Just evaluate the expression.
		;;
		;;NOTE  The signature  validation has  succeeded at  expand-time: the
		;;returned PSI has the original  ?EXPR signature, not "<list>".  This
		;;just looks nicer.
		expr.psi)

	       ((retvals-signature.single-top-tag? asserted.sig)
		;;If we are here the input form is:
		;;
		;;   (assert-retvals-signature-and-return (<top>) ?expr)
		;;
		;;so it is  enough to make sure that the  expression returns a single
		;;value, whatever its type.
		(syntax-match (retvals-signature.tags expr.sig) ()
		  ((?tag)
		   ;;Success!!!   We   have  determined   at  expand-time   that  the
		   ;;expression returns a single  value.
		   ;;
		   ;;IMPORTANT  NOTE  The  signature   validation  has  succeeded  at
		   ;;expand-time:  the returned  PSI *must*  have the  original ?EXPR
		   ;;signature,  not ASSERTED.SIG;  this  even  when ASSERTED.SIG  is
		   ;;"(<top>)".   This  property is  used  in  binding syntaxes  when
		   ;;propagating a tag from the RHS to the LHS.
		   expr.psi)
		  (?tag
		   (list-tag-id? ?tag)
		   ;;Damn   it!!!   The   expression's  return   values  have   fully
		   ;;unspecified signature; we need to insert a run-time check.
		   (%run-time-validation-of-single-value input-form.stx lexenv.run lexenv.expand
							 expr.psi))
		  (_
		   ;;The  horror!!!   We have  established  at  expand-time that  the
		   ;;expression returns multiple values; assertion failed.
		   (expand-time-retvals-signature-violation __who__ input-form.stx ?expr asserted.sig expr.sig))
		  ))

	       ((retvals-signature.partially-unspecified? expr.sig)
		;;The asserted  signature has type identifiers  that demand enforcing
		;;of constraints.  The expression has  no full type specification; we
		;;have to insert a run-time check.
		(%run-time-validation input-form.stx lexenv.run lexenv.expand
				      asserted.sig expr.psi))

	       ((retvals-signature.super-and-sub? asserted.sig expr.sig)
		;;Fine, we have  established at expand-time that  the returned values
		;;are valid; assertion succeeded.  Just evaluate the expression.
		expr.psi)

	       (else
		;;The horror!!!  We have established at expand-time that the returned
		;;values are of the wrong type; assertion failed.
		(expand-time-retvals-signature-violation __who__ input-form.stx ?expr asserted.sig expr.sig)))))

      ((_ ?retvals-signature ?expr)
       ;;Let's use a descriptive error message here.
       (%synner "invalid return values signature" ?retvals-signature))
      ))

  (define* (%run-time-validation-of-single-value input-form.stx lexenv.run lexenv.expand
						 expr.psi)
    ;;This function  is used when the  syntax use specifies that  the expression must
    ;;return a single value, of any type.
    ;;
    (let* ((cwv.core     (build-primref no-source 'call-with-values))
	   (expr.core    (psi-core-expr expr.psi))
	   (checker.sexp (let ((arg.sym  (gensym "arg"))
			       (rest.sym (gensym "rest")))
			   `(lambda (,arg.sym . ,rest.sym)
			      (if (null? ,rest.sym)
				  ,arg.sym
				(assertion-violation #f "expected single return value" (cons ,arg.sym ,rest.sym))))))
	   (checker.psi  (chi-expr (bless checker.sexp) lexenv.run lexenv.expand))
	   (checker.core (psi-core-expr checker.psi)))
      (make-psi input-form.stx
		(build-application no-source
		  cwv.core
		  (list (build-lambda no-source
			  '()
			  expr.core)
			checker.core))
		(make-retvals-signature/single-top))))

  (define* (%run-time-validation input-form.stx lexenv.run lexenv.expand
				 {asserted.sig retvals-signature?} {expr.psi psi?})
    ;;This function  is called when the  syntax use specifies that  the return values
    ;;must  have  some  specific  types,  but  it  is  impossible  to  determine,  at
    ;;expand-time, the type of the actually returned values.
    ;;
    (define expr.core (psi-core-expr         expr.psi))
    (define expr.sig  (psi-retvals-signature expr.psi))
    ;;Here we know that ASSERTED.SIG is a  valid formals signature, so we can be less
    ;;strict in the patterns.
    (syntax-match (retvals-signature.tags asserted.sig) ()
      ;;Special handling for single value: we have established before that ?RV-TAG is
      ;;not "<top>".
      ((?rv-tag)
       (let* ((obj.sym     (gensym "obj"))
	      (checker.psi (chi-expr (bless
				      `(lambda (,obj.sym)
					 (validate-typed-return-value ,?rv-tag 1 ,obj.sym)
					 ,obj.sym))
				     lexenv.run lexenv.expand))
	      (checker.core (psi-core-expr checker.psi)))
	 (make-psi input-form.stx
		   (build-application no-source
		     checker.core
		     (list expr.core))
		   ;;The type  of the  value returned by  ?EXPR was  unspecified, but
		   ;;after asserting the  type at run-time: we know that  the type is
		   ;;the asserted one.
		   asserted.sig)))

      ((?rv-tag* ...)
       (let* ((TMP*         (generate-temporaries ?rv-tag*))
	      (IDX*         (%fxiota 1 TMP*))
	      (checker.psi  (chi-expr (bless
				       `(lambda ,TMP*
					  ,@(map (lambda (tmp idx tag)
						   `(validate-typed-return-value ,tag ,idx ,tmp))
					      TMP* IDX* ?rv-tag*)
					  (values . ,TMP*)))
				      lexenv.run lexenv.expand)))
	 (%run-time-check-multiple-values-output-form input-form.stx lexenv.run lexenv.expand
						      expr.psi checker.psi asserted.sig)))

      ((?rv-tag* ... . ?rv-rest-tag)
       (let* ((TMP*         (generate-temporaries ?rv-tag*))
	      (IDX*         (%fxiota 1 TMP*))
	      (rest.sym     (gensym "rest"))
	      (idx.sym      (gensym "idx"))
	      (obj.sym      (gensym "obj"))
	      (checker.psi  (chi-expr (bless
				       `(lambda (,@TMP* . ,rest.sym)
					  ,@(map (lambda (tmp idx tag)
						   `(validate-typed-return-value ,tag ,idx ,tmp))
					      TMP* IDX* ?rv-tag*)
					  (fold-left (lambda (,idx.sym ,obj.sym)
						       (validate-typed-return-value ,?rv-rest-tag ,idx.sym ,obj.sym)
						       (fxadd1 ,idx.sym))
					    ,(add1 (length TMP*)) ,rest.sym)
					  (apply values ,@TMP* ,rest.sym)))
				      lexenv.run lexenv.expand)))
	 (%run-time-check-multiple-values-output-form input-form.stx lexenv.run lexenv.expand
						      expr.psi checker.psi asserted.sig)))

      (?rv-args-tag
       (let* ((args.sym     (gensym "args"))
	      (idx.sym      (gensym "idx"))
	      (obj.sym      (gensym "obj"))
	      (checker.psi  (chi-expr (bless
				       `(lambda ,args.sym
					  (fold-left (lambda (,idx.sym ,obj.sym)
						       (validate-typed-return-value ,?rv-args-tag ,idx.sym ,obj.sym)
						       (fxadd1 ,idx.sym))
					    1 ,args.sym)
					  (apply values ,args.sym)))
				      lexenv.run lexenv.expand)))
	 (%run-time-check-multiple-values-output-form input-form.stx lexenv.run lexenv.expand
						      expr.psi checker.psi asserted.sig)))
      ))

  (define* (%run-time-check-multiple-values-output-form input-form.stx lexenv.run lexenv.expand
							{expr.psi psi?} {checker.psi psi?} {asserted.sig retvals-signature?})
    ;;We build a core language expression as follows:
    ;;
    ;;   (call-with-values
    ;;        (lambda () ?expr)
    ;;     (lambda ?formals
    ;;       ?check-form ...
    ;;       (apply values ?formals)))
    ;;
    ;;The returned PSI struct has the given retvals signature.
    ;;
    (let* ((cwv.core     (build-primref no-source 'call-with-values))
	   (expr.core    (psi-core-expr expr.psi))
	   (checker.core (psi-core-expr checker.psi)))
      (make-psi input-form.stx
		(build-application no-source
		  cwv.core
		  (list (build-lambda no-source
			  '()
			  expr.core)
			checker.core))
		;;The type  of values  returned by ?EXPR  was unspecified,  but after
		;;asserting  the type  at  run-time: we  know that  the  type is  the
		;;asserted one.
		asserted.sig)))

  #| end of module: ASSERT-RETVALS-SIGNATURE-AND-RETURN-TRANSFORMER |# )


;;;; done

#| end of module |# )

;;; end of file
;;Local Variables:
;;mode: vicare
;;fill-column: 85
;;eval: (put 'build-library-letrec*		'scheme-indent-function 1)
;;eval: (put 'build-application			'scheme-indent-function 1)
;;eval: (put 'build-conditional			'scheme-indent-function 1)
;;eval: (put 'build-case-lambda			'scheme-indent-function 1)
;;eval: (put 'build-lambda			'scheme-indent-function 1)
;;eval: (put 'build-foreign-call		'scheme-indent-function 1)
;;eval: (put 'build-sequence			'scheme-indent-function 1)
;;eval: (put 'build-global-assignment		'scheme-indent-function 1)
;;eval: (put 'build-lexical-assignment		'scheme-indent-function 1)
;;eval: (put 'build-letrec*			'scheme-indent-function 1)
;;eval: (put 'build-data			'scheme-indent-function 1)
;;eval: (put 'core-lang-builder			'scheme-indent-function 1)
;;eval: (put 'case-object-type-binding		'scheme-indent-function 1)
;;eval: (put 'push-lexical-contour		'scheme-indent-function 1)
;;eval: (put 'syntactic-binding-getprop		'scheme-indent-function 1)
;;eval: (put 'sys.syntax-case			'scheme-indent-function 2)
;;eval: (put 'with-exception-handler/input-form	'scheme-indent-function 1)
;;eval: (put '$map-in-order			'scheme-indent-function 1)
;;End:
