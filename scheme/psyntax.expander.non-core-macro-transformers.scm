;;;Copyright (c) 2010-2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;The  function NON-CORE-MACRO-TRANSFORMER  maps symbols  representing
;;non-core macros to their macro transformers.
;;
;;NOTE This  module is very  long, so it  is split into  multiple code
;;pages.  (Marco Maggi; Sat Apr 27, 2013)
;;
(define* (non-core-macro-transformer x)
  (define (%error-invalid-macro)
    (error __who__ "Vicare: internal error: invalid macro" x))
  (assert (symbol? x))
  (case x
    ((define-struct)			define-struct-macro)
    ((define-record-type)		define-record-type-macro)
    ((record-type-and-record?)		record-type-and-record?-macro)
    ((define-condition-type)		define-condition-type-macro)
    ((cond)				cond-macro)
    ((do)				do-macro)
    ((or)				or-macro)
    ((and)				and-macro)
    ((let*)				let*-macro)
    ((let-values)			let-values-macro)
    ((let*-values)			let*-values-macro)
    ((values->list)			values->list-macro)
    ((syntax-rules)			syntax-rules-macro)
    ((quasiquote)			quasiquote-macro)
    ((quasisyntax)			quasisyntax-macro)
    ((with-syntax)			with-syntax-macro)
    ((when)				when-macro)
    ((unless)				unless-macro)
    ((case)				case-macro)
    ((case-identifiers)			case-identifiers-macro)
    ((identifier-syntax)		identifier-syntax-macro)
    ((time)				time-macro)
    ((delay)				delay-macro)
    ((assert)				assert-macro)
    ((guard)				guard-macro)
    ((define-enumeration)		define-enumeration-macro)
    ((let*-syntax)			let*-syntax-macro)
    ((let-constants)			let-constants-macro)
    ((let*-constants)			let*-constants-macro)
    ((letrec-constants)			letrec-constants-macro)
    ((letrec*-constants)		letrec*-constants-macro)
    ((case-define)			case-define-macro)
    ((define*)				define*-macro)
    ((case-define*)			case-define*-macro)
    ((lambda*)				lambda*-macro)
    ((case-lambda*)			case-lambda*-macro)

    ((trace-lambda)			trace-lambda-macro)
    ((trace-define)			trace-define-macro)
    ((trace-let)			trace-let-macro)
    ((trace-define-syntax)		trace-define-syntax-macro)
    ((trace-let-syntax)			trace-let-syntax-macro)
    ((trace-letrec-syntax)		trace-letrec-syntax-macro)

    ((define-syntax-parameter)		define-syntax-parameter-macro)
    ((syntax-parametrise)		syntax-parametrise-macro)

    ((include)				include-macro)
    ((define-integrable)		define-integrable-macro)
    ((define-inline)			define-inline-macro)
    ((define-constant)			define-constant-macro)
    ((define-inline-constant)		define-inline-constant-macro)
    ((define-values)			define-values-macro)
    ((define-constant-values)		define-constant-values-macro)
    ((receive)				receive-macro)
    ((receive-and-return)		receive-and-return-macro)
    ((begin0)				begin0-macro)
    ((xor)				xor-macro)
    ((define-syntax-rule)		define-syntax-rule-macro)
    ((define-auxiliary-syntaxes)	define-auxiliary-syntaxes-macro)
    ((define-syntax*)			define-syntax*-macro)
    ((unwind-protect)			unwind-protect-macro)
    ((with-implicits)			with-implicits-macro)
    ((set-cons!)			set-cons!-macro)

    ;; non-Scheme style syntaxes
    ((while)				while-macro)
    ((until)				until-macro)
    ((for)				for-macro)
    ((define-returnable)		define-returnable-macro)
    ((lambda-returnable)		lambda-returnable-macro)
    ((begin-returnable)			begin-returnable-macro)
    ((try)				try-macro)

    ((parameterize)			parameterize-macro)
    ((parametrise)			parameterize-macro)

    ;; compensations
    ((with-compensations)		with-compensations-macro)
    ((with-compensations/on-error)	with-compensations/on-error-macro)
    ((compensate)			compensate-macro)
    ((push-compensation)		push-compensation-macro)

    ((eol-style)
     (lambda (x)
       (%allowed-symbol-macro x '(none lf cr crlf nel crnel ls))))

    ((error-handling-mode)
     (lambda (x)
       (%allowed-symbol-macro x '(ignore raise replace))))

    ((buffer-mode)
     (lambda (x)
       (%allowed-symbol-macro x '(none line block))))

    ((endianness)
     endianness-macro)

    ((file-options)
     file-options-macro)

    ((... => _
	  else unquote unquote-splicing
	  unsyntax unsyntax-splicing
	  fields mutable immutable parent protocol
	  sealed opaque nongenerative parent-rtd
	  catch finally)
     (lambda (expr-stx)
       (syntax-violation #f "incorrect usage of auxiliary keyword" expr-stx)))

    ((__file__)
     (lambda (stx)
       (let ((expr (<stx>-expr stx)))
	 (if (annotation? expr)
	     (let ((pos (annotation-textual-position expr)))
	       (if (source-position-condition? pos)
		   (bless
		    `(quote ,(source-position-port-id pos)))
		 (bless
		  `(quote ,(source-code-location)))))
	   (bless
	    `(quote ,(source-code-location)))))))

    ((__line__)
     (lambda (stx)
       (let ((expr (<stx>-expr stx)))
	 (if (annotation? expr)
	     (let ((pos (annotation-textual-position expr)))
	       (if (source-position-condition? pos)
		   (bless
		    `(quote ,(source-position-line pos)))
		 (bless '(quote #f))))
	   (bless '(quote #f))))))

    ;;Expander tags.
    ((<untagged> <top>
      <boolean> <char> <symbol> <keyword> <pointer> <transcoder> <procedure>
      <fixnum> <flonum> <ratnum> <bignum> <compnum> <cflonum>
      <rational-valued> <rational> <integer> <integer-valued>
      <exact-integer> <real> <real-valued> <complex> <number>
      <string> <vector> <pair> <list> <bytevector> <hashtable>
      <record> <record-type-descriptor> <struct> <struct-type-descriptor> <condition>
      <port> <input-port> <output-port> <input/output-port> <textual-port> <binary-port>
      <textual-input-port> <textual-output-port> <textual-input/output-port>
      <binary-input-port> <binary-output-port> <binary-input/output-port>)
     (lambda (expr-stx)
       (syntax-violation #f "incorrect usage of built-in tag keyword" expr-stx)))

    (else
     (%error-invalid-macro))))


;;;; module non-core-macro-transformer: DEFINE-AUXILIARY-SYNTAXES

(define (define-auxiliary-syntaxes-macro expr-stx)
  ;;Transformer      function      used     to      expand      Vicare's
  ;;DEFINE-AUXILIARY-SYNTAXES  macros   from  the  top-level   built  in
  ;;environment.   Expand  the contents  of  EXPR-STX;  return a  syntax
  ;;object that must be further expanded.
  ;;
  ;;Using an empty SYNTAX-RULES as  transformer function makes sure that
  ;;whenever an auxiliary syntax is referenced an error is raised.
  ;;
  (syntax-match expr-stx ()
    ((_ ?id* ...)
     (for-all identifier? ?id*)
     (bless
      `(begin
	 ,@(map (lambda (id)
		  `(define-syntax ,id (syntax-rules ())))
	     ?id*))))))


;;;; module non-core-macro-transformer: control structures macros

(define (when-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?test ?expr ?expr* ...)
     (bless `(if ,?test (begin ,?expr . ,?expr*))))))

(define (unless-macro expr-stx)
  (syntax-match expr-stx ()
    ((_ ?test ?expr ?expr* ...)
     (bless `(if (not ,?test) (begin ,?expr . ,?expr*))))))


;;;; module non-core-macro-transformer: CASE, CASE-IDENTIFIERS

(module (case-macro)
  ;;Transformer  function used  to expand  R6RS's CASE  macros from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;FIXME This should be rewritten to support the model proposed in:
  ;;
  ;;    William   D.   Clinger.    "Rapid  case  dispatch   in  Scheme".
  ;;    Northeastern University.   Proceedings  of the  2006 Scheme  and
  ;;   Functional Programming Workshop.  University of Chicago Technical
  ;;   Report TR-2006-06.
  ;;
  (define (case-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?expr)
       (bless `(let ((t ,?expr))
		 (if #f #f))))
      ((_ ?expr ?clause ?clause* ...)
       (bless
	`(let ((t ,?expr))
	   ,(let recur ((clause  ?clause)
			(clause* ?clause*))
	      (if (null? clause*)
		  (%build-last clause)
		(%build-one clause (recur (car clause*) (cdr clause*))))))))))

  (define (%build-one clause-stx k)
    (syntax-match clause-stx (=>)
      (((?datum* ...) => ?expr)
       (if (option.strict-r6rs)
	   (syntax-violation 'case
	     "invalid usage of auxiliary keyword => in strict R6RS mode"
	     clause-stx)
	 `(if (memv t ',?datum*)
	      (let ((proc ,?expr))
		(if (procedure? proc)
		    (proc t)
		  (assertion-violation #f "not a procedure" (quote ,?expr))))
	    ,k)))
      (((?datum* ...) ?expr ?expr* ...)
       `(if (memv t ',?datum*)
	    (internal-body ,?expr . ,?expr*)
	  ,k))
      ))

  (define (%build-last clause)
    (syntax-match clause (else)
      ((else ?expr ?expr* ...)
       `(internal-body ,?expr . ,?expr*))
      (_
       (%build-one clause '(if #f #f)))))

  #| end of module: CASE-MACRO |# )

(module (case-identifiers-macro)
  (define-constant __who__
    'case-identifiers)

  (define (case-identifiers-macro expr-stx)
    ;;Transformer  function  used  to expand  Vicare's  CASE-IDENTIFIERS
    ;;macros  from  the  top-level  built in  environment.   Expand  the
    ;;contents of EXPR-STX; return a  syntax object that must be further
    ;;expanded.
    ;;
    (syntax-match expr-stx ()
      ((_ ?expr)
       (bless
	`(let ((t ,?expr))
	   (if #f #f))))
      ((_ ?expr ?clause ?clause* ...)
       (bless
	`(let* ((t ,?expr)
		(p (identifier? t)))
	   ,(let recur ((clause  ?clause)
			(clause* ?clause*))
	      (if (null? clause*)
		  (%build-last expr-stx clause)
		(%build-one expr-stx clause (recur (car clause*) (cdr clause*))))))))
      ))

  (define (%build-one expr-stx clause-stx kont)
    (syntax-match clause-stx (=>)
      (((?datum* ...) => ?proc)
       (if (option.strict-r6rs)
	   (syntax-violation __who__
	     "invalid usage of auxiliary keyword => in strict R6RS mode"
	     clause-stx)
	 `(if ,(%build-test expr-stx ?datum*)
	      (,?proc t)
	    ,kont)))

      (((?datum* ...) ?expr ?expr* ...)
       `(if ,(%build-test expr-stx ?datum*)
	    (internal-body ,?expr . ,?expr*)
	  ,kont))
      ))

  (define (%build-last expr-stx clause)
    (syntax-match clause (else)
      ((else ?expr ?expr* ...)
       `(internal-body ,?expr . ,?expr*))
      (_
       (%build-one expr-stx clause '(if #f #f)))))

  (define (%build-test expr-stx datum*)
    `(and p (or . ,(map (lambda (datum)
			  (if (identifier? datum)
			      `(free-identifier=? t (syntax ,datum))
			    (syntax-violation __who__
			      "expected identifiers as datums"
			      expr-stx datum)))
		     datum*))))

  #| end of module: CASE-IDENTIFIERS-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-STRUCT

(module (define-struct-macro)
  ;;Transformer function  used to  expand Vicare's  DEFINE-STRUCT macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR.STX; return a syntax object that must be further expanded.
  ;;
  (define (define-struct-macro expr.stx)
    (syntax-match expr.stx ()
      ((_ (?name ?maker ?predicate) (?field* ...))
       (%build-output-form ?name ?maker ?predicate ?field*))
      ((_ ?name (?field* ...))
       (%build-output-form ?name #f #f ?field*))
      ))

  (define (%build-output-form type-id maker-id predicate-id field-name-id*)
    (let* ((string->id		(lambda (str)
				  ($datum->syntax type-id (string->symbol str))))
	   (namestr		(symbol->string (identifier->symbol type-id)))
	   (field-sym*		(map identifier->symbol field-name-id*))
	   (field-str*		(map symbol->string field-sym*))
	   (rtd			($datum->syntax type-id (make-struct-type namestr field-sym*)))
	   (constructor-id	(or maker-id     (string->id (string-append "make-" namestr))))
	   (predicate-id	(or predicate-id (string->id (string-append namestr "?"))))
	   (field-idx*		(enumerate field-name-id*)))

      (define accessor-id*
	(map (lambda (x)
	       (string->id (string-append namestr "-" x)))
	  field-str*))

      (define mutator-id*
	(map (lambda (x)
	       (string->id (string-append "set-" namestr "-" x "!")))
	  field-str*))

      (define unsafe-accessor-id*
	(map (lambda (x)
	       (string->id (string-append "$" namestr "-" x)))
	  field-str*))

      (define unsafe-mutator-id*
	(map (lambda (x)
	       (string->id (string-append "$set-" namestr "-" x "!")))
	  field-str*))

      (define accessor-sexp*
	(map (lambda (accessor-id unsafe-accessor-id)
	       `(define (,accessor-id stru)
		  (if ($struct/rtd? stru ',rtd)
		      (,unsafe-accessor-id stru)
		    (assertion-violation ',accessor-id
		      "not a struct of required type as struct accessor argument"
		      stru ',rtd))))
	  accessor-id* unsafe-accessor-id*))

      (define mutator-sexp*
	(map (lambda (mutator-id unsafe-mutator-id)
	       `(define (,mutator-id stru val)
		  (if ($struct/rtd? stru ',rtd)
		      (,unsafe-mutator-id stru val)
		    (assertion-violation ',mutator-id
		      "not a struct of required type as struct mutator argument"
		      stru ',rtd))))
	  mutator-id* unsafe-mutator-id*))

      (define unsafe-accessor-sexp*
	(map (lambda (unsafe-accessor-id field-idx)
	       `(define-syntax ,unsafe-accessor-id
		  (syntax-rules ()
		    ((_ ?stru)
		     ($struct-ref ?stru ,field-idx)))))
	  unsafe-accessor-id* field-idx*))

      (define unsafe-mutator-sexp*
	(map (lambda (unsafe-mutator-id field-idx)
	       `(define-syntax ,unsafe-mutator-id
		  (syntax-rules ()
		    ((_ ?stru ?val)
		     ($struct-set! ?stru ,field-idx ?val)))))
	  unsafe-mutator-id* field-idx*))

      (define object-type-spec-form
	(%build-object-type-spec type-id predicate-id field-sym*
				 accessor-id* unsafe-accessor-id*
				 mutator-id*  unsafe-mutator-id*))

      (bless
       `(begin
	  (define (,constructor-id ,@field-name-id*)
	    (let ((S ($struct ',rtd ,@field-name-id*)))
	      (if ($std-destructor ',rtd)
		  ($struct-guardian S)
		S)))
	  (define (,predicate-id obj)
	    ($struct/rtd? obj ',rtd))
	  ;;By putting this  form here we are sure  that PREDICATE-ID is
	  ;;already bound when the object-type-spec is built.
	  (define-syntax ,type-id
	    (cons '$rtd ',rtd))
	  ;;This definition  exists only  to make  sure that  the object
	  ;;type spec form is evaluated in the visit code.
	  (define-syntax dummy
	    (internal-body
	      ,object-type-spec-form
	      (lambda (stx) #f)))
	  ,@accessor-sexp*
	  ,@mutator-sexp*
	  ,@unsafe-accessor-sexp*
	  ,@unsafe-mutator-sexp*))))

  (define (%build-object-type-spec type-id predicate-id field-sym*
				   accessor-id* unsafe-accessor-id*
				   mutator-id*  unsafe-mutator-id*)
    (define uid
      (gensym (syntax->datum type-id)))
    (define type-str
      (symbol->string (syntax->datum type-id)))
    (define %accessor-maker
      (string->symbol (string-append type-str "-accessor-maker")))
    (define %mutator-maker
      (string->symbol (string-append type-str "-mutator-maker")))
    (define %getter-maker
      (string->symbol (string-append type-str "-getter-maker")))
    (define %setter-maker
      (string->symbol (string-append type-str "-setter-maker")))
    (define %dispatcher
      (string->symbol (string-append type-str "-dispatcher")))
    `(internal-body
       (import (vicare)
	 (prefix (vicare expander object-type-specs) typ.))

       (define (,%accessor-maker field.sym input-form.stx)
	 (case field.sym
	   ,@(map (lambda (field-sym accessor-id)
		    `((,field-sym)	(syntax ,accessor-id)))
	       field-sym* accessor-id*)
	   (else #f)))

       (define (,%mutator-maker field.sym input-form.stx)
	 (case field.sym
	   ,@(map (lambda (field-sym mutator-id)
		    `((,field-sym)	(syntax ,mutator-id)))
	       field-sym* mutator-id*)
	   (else #f)))

       (define (,%getter-maker keys.stx input-form.stx)
	 (define (%invalid-keys)
	   (syntax-violation (quote ,type-id) "invalid keys for getter" input-form.stx keys.stx))
	 (syntax-case keys.stx ()
	   (([?field.sym])
	    (identifier? #'?field.sym)
	    (or (,%accessor-maker (syntax->datum #'?field.sym) input-form.stx)
		(%invalid-keys)))
	   (else
	    (%invalid-keys))))

       (define (,%setter-maker keys.stx input-form.stx)
	 (define (%invalid-keys)
	   (syntax-violation (quote ,type-id) "invalid keys for setter" input-form.stx keys.stx))
	 (syntax-case keys.stx ()
	   (([?field.sym])
	    (identifier? #'?field.sym)
	    (or (,%mutator-maker (syntax->datum #'?field.sym) input-form.stx)
		(%invalid-keys)))
	   (else
	    (%invalid-keys))))

       (define %caster-maker #f)

       (define (,%dispatcher method-id arg*.stx input-form.stx)
	 #f)

       (define object-type-spec
	 (typ.make-object-type-spec (quote ,uid)
				    (syntax ,type-id) (typ.top-tag-id) (syntax ,predicate-id)
				    ,%accessor-maker ,%mutator-maker
				    ,%getter-maker ,%setter-maker
				    %caster-maker ,%dispatcher))

       (typ.set-identifier-object-type-spec! (syntax ,type-id) object-type-spec)))

  (define (enumerate ls)
    (let recur ((i 0) (ls ls))
      (if (null? ls)
	  '()
	(cons i (recur (+ i 1) (cdr ls))))))

  #| end of module: DEFINE-STRUCT-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-RECORD-TYPE

(module (define-record-type-macro)
  ;;Transformer function used to expand R6RS's DEFINE-RECORD-TYPE macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define-constant __who__ 'define-record-type)

  (define (define-record-type-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?namespec ?clause* ...)
       (begin
	 (%verify-clauses expr-stx ?clause*)
	 (%do-define-record expr-stx ?namespec ?clause*)))
      ))

;;; --------------------------------------------------------------------

  (define (%do-define-record expr-stx namespec clause*)
    (case-define synner
      ((message)
       (synner message #f))
      ((message subform)
       (syntax-violation __who__ message expr-stx subform)))

    (define-values (foo make-foo foo?)
      (%parse-full-name-spec namespec))
    (define foo-rtd		(%named-gensym foo "-rtd"))
    (define foo-rcd		(%named-gensym foo "-rcd"))
    (define foo-protocol	(gensym))
    (define-values
      (x*
		;A list of identifiers representing all the field names.
       idx*
		;A list  of fixnums  representing all the  field indexes
		;(zero-based).
       foo-x*
		;A list  of identifiers  representing the  safe accessor
		;names.
       unsafe-foo-x*
		;A list of identifiers  representing the unsafe accessor
		;names.
       mutable-x*
		;A list  of identifiers  representing the  mutable field
		;names.
       set-foo-idx*
		;A  list  of  fixnums  representing  the  mutable  field
		;indexes (zero-based).
       foo-x-set!*
		;A list of identifiers representing the mutator names.
       unsafe-foo-x-set!*
		;A list  of identifiers representing the  unsafe mutator
		;names.
       immutable-x*
		;A list of identifiers  representing the immutable field
		;names.
       )
      (%parse-field-specs foo (%get-fields clause*) synner))

    ;;Code  for parent  record-type  descriptor  and parent  record-type
    ;;constructor descriptor retrieval.
    (define-values (foo-parent parent-rtd-code parent-rcd-code)
      (%make-parent-rtd+rcd-code clause* synner))

    (define foo-uid
      (%get-uid foo clause* synner))

    ;;Code  for  record-type   descriptor  and  record-type  constructor
    ;;descriptor.
    (define foo-rtd-code
      (%make-rtd-code foo foo-uid clause* parent-rtd-code synner))
    (define foo-rcd-code
      (%make-rcd-code clause* foo-rtd foo-protocol parent-rcd-code))

    ;;Code for protocol.
    (define protocol-code
      (%get-protocol-code clause* synner))

    (define binding-spec
      (%make-binding-spec x* mutable-x*
			  foo-x* foo-x-set!*
			  unsafe-foo-x* unsafe-foo-x-set!*))

    (define object-type-spec-form
      ;;The object-type-spec stuff is used to  add a tag property to the
      ;;record type identifier.
      (%make-object-type-spec-form foo foo? foo-parent foo-uid
				   x* foo-x* unsafe-foo-x*
				   mutable-x* foo-x-set!* unsafe-foo-x-set!*
				   immutable-x*))

    (bless
     `(begin
	;;Record type descriptor.
	(define ,foo-rtd ,foo-rtd-code)
	;;Protocol function.
	(define ,foo-protocol ,protocol-code)
	;;Record constructor descriptor.
	(define ,foo-rcd ,foo-rcd-code)
	;;Record instance predicate.
	(define ,foo? (record-predicate ,foo-rtd))
	;;Record instance constructor.
	(define ,make-foo (record-constructor ,foo-rcd))
	;;Safe record fields accessors.
	,@(map (lambda (foo-x idx)
		 `(define ,foo-x (record-accessor ,foo-rtd ,idx (quote ,foo-x))))
	    foo-x* idx*)
	;;Safe record fields mutators (if any).
	,@(map (lambda (foo-x-set! idx)
		 `(define ,foo-x-set! (record-mutator ,foo-rtd ,idx (quote ,foo-x-set!))))
	    foo-x-set!* set-foo-idx*)

	;;Binding for record type name.
	(define-syntax ,foo
	  (cons '$rtd
		(cons (syntax ,foo-rtd)
		      (cons (syntax ,foo-rcd) (quote ,binding-spec)))))

	;;This definition exists only to  make sure that the object type
	;;spec form is evaluated in the visit code.
	(define-syntax dummy
	  (internal-body
	    ,object-type-spec-form
	    (lambda (stx) #f)))

	. ,(if (option.strict-r6rs)
	       '()
	     (%gen-unsafe-accessor+mutator-code foo foo-rtd foo-rcd
						unsafe-foo-x*      idx*
						unsafe-foo-x-set!* set-foo-idx*)))))

;;; --------------------------------------------------------------------

  (define (%gen-unsafe-accessor+mutator-code foo foo-rtd foo-rcd
					     unsafe-foo-x*      idx*
					     unsafe-foo-x-set!* set-foo-idx*)
    (define foo-first-field-offset
      (gensym))
    `((define ,foo-first-field-offset
	;;The field  at index 3  in the RTD is:  the index of  the first
	;;field of  this subtype in the  layout of instances; it  is the
	;;total number of fields of the parent type.
	($struct-ref ,foo-rtd 3))

      ;; Unsafe record fields accessors.
      ,@(map (lambda (unsafe-foo-x idx)
	       (let ((t (gensym)))
		 `(begin
		    (define ,t
		      (fx+ ,idx ,foo-first-field-offset))
		    (define-syntax-rule (,unsafe-foo-x x)
		      ($struct-ref x ,t)))))
	  unsafe-foo-x* idx*)

      ;; Unsafe record fields mutators.
      ,@(map (lambda (unsafe-foo-x-set! idx)
	       (let ((t (gensym)))
		 `(begin
		    (define ,t
		      (fx+ ,idx ,foo-first-field-offset))
		    (define-syntax-rule (,unsafe-foo-x-set! x v)
		      ($struct-set! x ,t v)))))
	  unsafe-foo-x-set!* set-foo-idx*)
      ))

;;; --------------------------------------------------------------------

  (define (%parse-full-name-spec spec)
    ;;Given  a  syntax  object  representing  a  full  record-type  name
    ;;specification: return the name identifier.
    ;;
    (syntax-match spec ()
      ((?foo ?make-foo ?foo?)
       (and (identifier? ?foo)
	    (identifier? ?make-foo)
	    (identifier? ?foo?))
       (values ?foo ?make-foo ?foo?))
      (?foo
       (identifier? ?foo)
       (values ?foo
	       (identifier-append ?foo "make-" (syntax->datum ?foo))
	       (identifier-append ?foo ?foo "?")))
      ))

  (define (%get-uid foo clause* synner)
    (let ((clause (%get-clause 'nongenerative clause*)))
      (syntax-match clause ()
	((_)
	 (gensym (syntax->datum foo)))
	((_ ?uid)
	 (identifier? ?uid)
	 (syntax->datum ?uid))
	;;No matching clause found.
	(#f
	 (gensym (syntax->datum foo)))
	(_
	 (synner "expected symbol or no argument in nongenerative clause" clause)))))

  (define (%make-rtd-code name foo-uid clause* parent-rtd-code synner)
    ;;Return a  sexp which,  when evaluated,  will return  a record-type
    ;;descriptor.
    ;;
    (define sealed?
      (let ((clause (%get-clause 'sealed clause*)))
	(syntax-match clause ()
	  ((_ #t)	#t)
	  ((_ #f)	#f)
	  ;;No matching clause found.
	  (#f		#f)
	  (_
	   (synner "invalid argument in SEALED clause" clause)))))
    (define opaque?
      (let ((clause (%get-clause 'opaque clause*)))
	(syntax-match clause ()
	  ((_ #t)	#t)
	  ((_ #f)	#f)
	  ;;No matching clause found.
	  (#f		#f)
	  (_
	   (synner "invalid argument in OPAQUE clause" clause)))))
    (define fields
      (let ((clause (%get-clause 'fields clause*)))
	(syntax-match clause ()
	  ((_ field-spec* ...)
	   `(quote ,(list->vector
		     (map (lambda (field-spec)
			    (syntax-match field-spec (mutable immutable)
			      ((mutable ?name . ?rest)
			       `(mutable ,?name))
			      ((immutable ?name . ?rest)
			       `(immutable ,?name))
			      (?name
			       `(immutable ,?name))))
		       field-spec*))))
	  ;;No matching clause found.
	  (#f
	   (quote (quote #())))

	  (_
	   (synner "invalid syntax in FIELDS clause" clause)))))
    `(make-record-type-descriptor (quote ,name) ,parent-rtd-code
				  (quote ,foo-uid) ,sealed? ,opaque? ,fields))

  (define (%make-rcd-code clause* foo-rtd foo-protocol parent-rcd-code)
    ;;Return a sexp  which, when evaluated, will  return the record-type
    ;;default constructor descriptor.
    ;;
    `(make-record-constructor-descriptor ,foo-rtd ,parent-rcd-code ,foo-protocol))

  (define (%make-parent-rtd+rcd-code clause* synner)
    ;;Return 3 values:
    ;;
    ;;1. An identifier  representing the parent type, or  false if there
    ;;is no  parent or  the parent is  specified through  the procedural
    ;;layer.
    ;;
    ;;2.  A  sexp   which,  when  evaluated,  will   return  the  parent
    ;;record-type descriptor.
    ;;
    ;;3.  A  sexp   which,  when  evaluated,  will   return  the  parent
    ;;record-type default constructor descriptor.
    ;;
    (let ((parent-clause (%get-clause 'parent clause*)))
      (syntax-match parent-clause ()
	;;If there is a PARENT clause insert code that retrieves the RTD
	;;from the parent type name.
	((_ ?name)
	 (identifier? ?name)
	 (values ?name
		 `(record-type-descriptor ,?name)
		 `(record-constructor-descriptor ,?name)))

	;;If there  is no PARENT  clause try to retrieve  the expression
	;;evaluating to the RTD.
	(#f
	 (let ((parent-rtd-clause (%get-clause 'parent-rtd clause*)))
	   (syntax-match parent-rtd-clause ()
	     ((_ ?rtd ?rcd)
	      (values #f ?rtd ?rcd))

	     ;;If  neither the  PARENT  nor the  PARENT-RTD clauses  are
	     ;;present: just return false.
	     (#f
	      (values #f #f #f))

	     (_
	      (synner "invalid syntax in PARENT-RTD clause" parent-rtd-clause)))))

	(_
	 (synner "invalid syntax in PARENT clause" parent-clause)))))

  (define (%get-protocol-code clause* synner)
    ;;Return  a  sexp  which,   when  evaluated,  returns  the  protocol
    ;;function.
    ;;
    (let ((clause (%get-clause 'protocol clause*)))
      (syntax-match clause ()
	((_ ?expr)
	 ?expr)

	;;No matching clause found.
	(#f	#f)

	(_
	 (synner "invalid syntax in PROTOCOL clause" clause)))))

  (define (%get-fields clause*)
    ;;Return   a  list   of  syntax   objects  representing   the  field
    ;;specifications.
    ;;
    (syntax-match clause* (fields)
      (()
       '())
      (((fields ?field-spec* ...) . _)
       ?field-spec*)
      ((_ . ?rest)
       (%get-fields ?rest))))

;;; --------------------------------------------------------------------

  (define (%parse-field-specs foo field-clause* synner)
    ;;Given the  arguments of the  fields specification clause  return 4
    ;;values:
    ;;
    ;;1..The list of identifiers representing all the field names.
    ;;
    ;;2..The  list  of  fixnums  representings all  the  field  relative
    ;;   indexes (zero-based).
    ;;
    ;;3..A list of identifiers representing the safe accessor names.
    ;;
    ;;4..A list of identifiers representing the unsafe accessor names.
    ;;
    ;;5..The list of identifiers representing the mutable field names.
    ;;
    ;;6..The list  of fixnums  representings the mutable  field relative
    ;;   indexes (zero-based).
    ;;
    ;;7..A list of identifiers representing the safe mutator names.
    ;;
    ;;8..A list of identifiers representing the unsafe mutator names.
    ;;
    ;;9..The list of identifiers representing the immutable field names.
    ;;
    ;;Here we assume that FIELD-CLAUSE* is null or a proper list.
    ;;
    (define (gen-safe-accessor-name x)
      (identifier-append  foo foo "-" x))
    (define (gen-unsafe-accessor-name x)
      (identifier-append  foo "$" foo "-" x))
    (define (gen-safe-mutator-name x)
      (identifier-append  foo foo "-" x "-set!"))
    (define (gen-unsafe-mutator-name x)
      (identifier-append  foo "$" foo "-" x "-set!"))
    (let loop ((field-clause*		field-clause*)
	       (i			0)
	       (field*			'())
	       (idx*			'())
	       (accessor*		'())
	       (unsafe-accessor*	'())
	       (mutable-field*		'())
	       (mutable-idx*		'())
	       (mutator*		'())
	       (unsafe-mutator*		'())
	       (immutable-field*	'()))
      (syntax-match field-clause* (mutable immutable)
	(()
	 (values (reverse field*) (reverse idx*) (reverse accessor*) (reverse unsafe-accessor*)
		 (reverse mutable-field*) (reverse mutable-idx*) (reverse mutator*) (reverse unsafe-mutator*)
		 (reverse immutable-field*)))

	(((mutable   ?name ?accessor ?mutator) . ?rest)
	 (and (identifier? ?name)
	      (identifier? ?accessor)
	      (identifier? ?mutator))
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons ?accessor accessor*)	(cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       (cons ?name mutable-field*)	(cons i mutable-idx*)
	       (cons ?mutator mutator*)		(cons (gen-unsafe-mutator-name  ?name) unsafe-mutator*)
	       immutable-field*))

	(((immutable ?name ?accessor) . ?rest)
	 (and (identifier? ?name)
	      (identifier? ?accessor))
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons ?accessor accessor*)	(cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       mutable-field*			mutable-idx*
	       mutator*				unsafe-mutator*
	       (cons ?name immutable-field*)))

	(((mutable   ?name) . ?rest)
	 (identifier? ?name)
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons (gen-safe-accessor-name   ?name) accessor*)
	       (cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       (cons ?name mutable-field*)	(cons i mutable-idx*)
	       (cons (gen-safe-mutator-name    ?name) mutator*)
	       (cons (gen-unsafe-mutator-name  ?name) unsafe-mutator*)
	       immutable-field*))

	(((immutable ?name) . ?rest)
	 (identifier? ?name)
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons (gen-safe-accessor-name   ?name) accessor*)
	       (cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       mutable-field*			mutable-idx*
	       mutator*				unsafe-mutator*
	       (cons ?name immutable-field*)))

	((?name . ?rest)
	 (identifier? ?name)
	 (loop ?rest (+ 1 i)
	       (cons ?name field*)		(cons i idx*)
	       (cons (gen-safe-accessor-name   ?name) accessor*)
	       (cons (gen-unsafe-accessor-name ?name) unsafe-accessor*)
	       mutable-field*			mutable-idx*
	       mutator*				unsafe-mutator*
	       (cons ?name immutable-field*)))

	((?spec . ?rest)
	 (synner "invalid field specification in DEFINE-RECORD-TYPE syntax"
		 ?spec)))))

;;; --------------------------------------------------------------------

  (module (%make-binding-spec)
    (import R6RS-RECORD-TYPE-SPEC)

    (define (%make-binding-spec x* mutable-x*
				foo-x* foo-x-set!*
				unsafe-foo-x* unsafe-foo-x-set!*)

      ;;A sexp which will be BLESSed  in the output code.  The sexp will
      ;;evaluate to an alist in which: keys are symbols representing all
      ;;the  field  names; values  are  identifiers  bound to  the  safe
      ;;accessors.
      (define foo-fields-safe-accessors-table
	(%make-alist x* foo-x*))

      ;;A sexp which will be BLESSed  in the output code.  The sexp will
      ;;evaluate to  an alist  in which:  keys are  symbols representing
      ;;mutable  field  names;  values  are identifiers  bound  to  safe
      ;;mutators.
      (define foo-fields-safe-mutators-table
	(%make-alist mutable-x* foo-x-set!*))

      ;;A sexp which will be BLESSed  in the output code.  The sexp will
      ;;evaluate to an alist in which: keys are symbols representing all
      ;;the  field names;  values are  identifiers bound  to the  unsafe
      ;;accessors.
      (define foo-fields-unsafe-accessors-table
	(%make-alist x* unsafe-foo-x*))

      ;;A sexp which will be BLESSed  in the output code.  The sexp will
      ;;evaluate to  an alist  in which:  keys are  symbols representing
      ;;mutable  field names;  values  are identifiers  bound to  unsafe
      ;;mutators.
      (define foo-fields-unsafe-mutators-table
	(%make-alist mutable-x* unsafe-foo-x-set!*))

      (if (option.strict-r6rs)
	  (make-r6rs-record-type-spec foo-fields-safe-accessors-table
				      foo-fields-safe-mutators-table
				      #f #f)
	(make-r6rs-record-type-spec foo-fields-safe-accessors-table
				    foo-fields-safe-mutators-table
				    foo-fields-unsafe-accessors-table
				    foo-fields-unsafe-mutators-table)))

    (define (%make-alist key-id* operator-id*)
      (map (lambda (key-id operator-id)
	     (cons (syntax->datum key-id) operator-id))
	key-id* operator-id*))

    #| end of module: %MAKE-BINDING-SPEC |# )

;;; --------------------------------------------------------------------

  (define (%make-object-type-spec-form foo foo? foo-parent foo-uid
				       x* foo-x* unsafe-foo-x*
				       mutable-x* foo-x-set!* unsafe-foo-x-set!*
				       immutable-x*)
    (define type-str
      (symbol->string (syntax->datum foo)))
    (define %accessor-maker
      (string->symbol (string-append type-str "-accessor-maker")))
    (define %mutator-maker
      (string->symbol (string-append type-str "-mutator-maker")))
    (define %getter-maker
      (string->symbol (string-append type-str "-getter-maker")))
    (define %setter-maker
      (string->symbol (string-append type-str "-setter-maker")))
    (define %dispatcher
      (string->symbol (string-append type-str "-dispatcher")))
    `(internal-body
       (import (vicare)
	 (prefix (vicare expander object-type-specs) typ.))

       (define (,%accessor-maker field.sym input-form-stx)
	 (case field.sym
	   ,@(map (lambda (field-name accessor-id)
		    `((,field-name)	(syntax ,accessor-id)))
	       x* foo-x*)
	   (else #f)))

       (define (,%mutator-maker field.sym input-form-stx)
	 (case field.sym
	   ,@(map (lambda (field-name mutator-id)
		    `((,field-name)	(syntax ,mutator-id)))
	       mutable-x* foo-x-set!*)
	   ,@(map (lambda (field-name)
		    `((,field-name)
		      (syntax-violation ',foo
			"requested mutator of immutable record field name"
			input-form-stx field.sym)))
	       immutable-x*)
	   (else #f)))

       (define (,%getter-maker keys-stx input-form-stx)
	 (syntax-case keys-stx ()
	   (([?field-id])
	    (identifier? #'?field-id)
	    (,%accessor-maker (syntax->datum #'?field-id) input-form-stx))
	   (else #f)))

       (define (,%setter-maker keys-stx input-form-stx)
	 (syntax-case keys-stx ()
	   (([?field-id])
	    (identifier? #'?field-id)
	    (,%mutator-maker (syntax->datum #'?field-id) input-form-stx))
	   (else #f)))

       (define %caster-maker #f)

       (define (,%dispatcher method.sym arg*.stx input-form-stx)
	 #f)

       (define parent-id
	 ,(if foo-parent
	      `(syntax ,foo-parent)
	    '(typ.top-tag-id)))

       (define object-type-spec
	 (typ.make-object-type-spec (quote ,foo-uid)
				    (syntax ,foo) parent-id (syntax ,foo?)
				    ,%accessor-maker ,%mutator-maker
				    ,%getter-maker ,%setter-maker
				    %caster-maker ,%dispatcher))

       (typ.set-identifier-object-type-spec! (syntax ,foo) object-type-spec)))

;;; --------------------------------------------------------------------

  (module (%verify-clauses)

    (define (%verify-clauses expr-stx cls*)
      (define VALID-KEYWORDS
	(map bless
	  '(fields parent parent-rtd protocol sealed opaque nongenerative)))
      (let loop ((cls*  cls*)
		 (seen* '()))
	(unless (null? cls*)
	  (syntax-match (car cls*) ()
	    ((?kwd . ?rest)
	     (cond ((or (not (identifier? ?kwd))
			(not (%free-id-member? ?kwd VALID-KEYWORDS)))
		    (stx-error ?kwd "not a valid DEFINE-RECORD-TYPE keyword"))
		   ((bound-id-member? ?kwd seen*)
		    (syntax-violation __who__
		      "invalid duplicate clause in DEFINE-RECORD-TYPE"
		      expr-stx ?kwd))
		   (else
		    (loop (cdr cls*) (cons ?kwd seen*)))))
	    (?cls
	     (stx-error ?cls "malformed define-record-type clause"))
	    ))))

    (define (%free-id-member? x ls)
      (and (pair? ls)
	   (or (free-id=? x (car ls))
	       (%free-id-member? x (cdr ls)))))

    #| end of module: %VERIFY-CLAUSES |# )

  (define (%get-clause sym clause*)
    ;;Given a symbol SYM representing the  name of a clause and a syntax
    ;;object  CLAUSE*  representing  the clauses:  search  the  selected
    ;;clause and return it as syntax object.  When no matching clause is
    ;;found: return false.
    ;;
    (let next ((id       (bless sym))
	       (clause*  clause*))
      (syntax-match clause* ()
	(()
	 #f)
	(((?key . ?rest) . ?clause*)
	 (if (free-id=? id ?key)
	     `(,?key . ,?rest)
	   (next id ?clause*))))))

  (define (%named-gensym foo suffix)
    (gensym (string-append
	     (symbol->string (syntax->datum foo))
	     suffix)))

  #| end of module: DEFINE-RECORD-TYPE-MACRO |# )


;;;; module non-core-macro-transformer: RECORD-TYPE-AND-RECORD?

(define (record-type-and-record?-macro expr-stx)
  ;;Transformer function used to expand Vicare's RECORD-TYPE-AND-RECORD?
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?type-name ?record)
     (identifier? ?type-name)
     (bless
      `(record-and-rtd? ,?record (record-type-descriptor ,?type-name))))
    ))


;;;; module non-core-macro-transformer: DEFINE-CONDITION-TYPE

(define (define-condition-type-macro expr-stx)
  ;;Transformer  function  used  to  expand  R6RS  RECORD-CONDITION-TYPE
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((?ctxt ?name ?super ?constructor ?predicate (?field* ?accessor*) ...)
     (and (identifier? ?name)
	  (identifier? ?super)
	  (identifier? ?constructor)
	  (identifier? ?predicate)
	  (for-all identifier? ?field*)
	  (for-all identifier? ?accessor*))
     (let ((aux-accessor* (map (lambda (x)
				 (gensym))
			    ?accessor*)))
       (bless
	`(module (,?name ,?constructor ,?predicate . ,?accessor*)
	   (define-record-type (,?name ,?constructor ,(gensym))
	     (parent ,?super)
	     (fields ,@(map (lambda (field aux)
			      `(immutable ,field ,aux))
			 ?field* aux-accessor*))
	     (nongenerative)
	     (sealed #f)
	     (opaque #f))
	   (define ,?predicate
	     ;;Remember  that the  predicate has  to recognise  a simple
	     ;;condition object embedded in a compound condition object.
	     (condition-predicate (record-type-descriptor ,?name)))
	   ,@(map
		 (lambda (accessor aux)
		   `(define ,accessor
		      ;;Remember  that  the  accessor has  to  access  a
		      ;;simple condition  object embedded in  a compound
		      ;;condition object.
		      (condition-accessor (record-type-descriptor ,?name) ,aux)))
	       ?accessor* aux-accessor*)
	   #| end of module |# )
	)))
    ))


;;;; module non-core-macro-transformer: PARAMETERIZE and PARAMETRISE

(define (parameterize-macro expr-stx)
  ;;Transformer  function used  to expand  Vicare's PARAMETERIZE  macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;Notice that  MAKE-PARAMETER is  a primitive function  implemented in
  ;;"ikarus.compiler.sls"  by   "E-make-parameter".   Under   Vicare,  a
  ;;parameter function  can be  called with  0, 1  or 2  arguments:
  ;;
  ;;* When called with 1 argument: it returns the parameter's value.
  ;;
  ;;* When called with 2 arguments:  it sets the parameter's value after
  ;;  checking the new value with the guard function (if any).
  ;;
  ;;*  When called  with  3  arguments: it  sets  the parameter's  value
  ;;   optionally checking  the new  value with  the guard  function (if
  ;;  any).
  ;;
  ;;Under Vicare,  PARAMETERIZE applies  the guard  function to  the new
  ;;value only the first  time it is set; if the  control flow exits and
  ;;returns multiple times beacuse  escaping continuations are used, the
  ;;guard function is  no more applied; this is achieved  by setting the
  ;;flag variable GUARD?.
  ;;
  (syntax-match expr-stx ()
    ((_ () ?body ?body* ...)
     (bless
      `(internal-body ,?body . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (let ((lhs* (generate-temporaries ?lhs*))
	   (rhs* (generate-temporaries ?rhs*)))
       (bless
	`((lambda ,(append lhs* rhs*)
	    (let* ((guard? #t) ;apply the guard function only the first time
		   (swap   (lambda ()
			     ,@(map (lambda (lhs rhs)
				      `(let ((t (,lhs)))
					 (,lhs ,rhs guard?)
					 (set! ,rhs t)))
				 lhs* rhs*)
			     (set! guard? #f))))
	      (dynamic-wind
		  swap
		  (lambda () ,?body . ,?body*)
		  swap)))
	  ,@(append ?lhs* ?rhs*)))))
    ))


;;;; module non-core-macro-transformer: UNWIND-PROTECT

(define (unwind-protect-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  UNWIND-PROTECT macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;Not a  general UNWIND-PROTECT for Scheme,  but fine where we  do not
  ;;make the body return continuations to  the caller and then come back
  ;;again and again, calling CLEANUP multiple times.
  ;;
  (syntax-match expr-stx ()
    ((_ ?body ?cleanup0 ?cleanup* ...)
     (bless
      `(let ((cleanup (lambda () ,?cleanup0 ,@?cleanup*)))
	 (with-exception-handler
	     (lambda (E)
	       (cleanup)
	       (raise E))
	   (lambda ()
	     (begin0
	       ,?body
	       (cleanup)))))))
    ))


;;;; module non-core-macro-transformer: WITH-IMPLICITS

(define (with-implicits-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  WITH-IMPLICITS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;This  macro  is   a  wrapper  for  WITH-SYNTAX   which  defines  the
  ;;identifiers ?SYMBOL with the same context  of ?CTX.  ?CTX must be an
  ;;expression evaluating to  an identifier; it is  evaluated only once.
  ;;?SYMBOL must be Scheme symbols.  For example:
  ;;
  ;;   (syntax-case stx ()
  ;;     ((id)
  ;;      (identifier? #'id)
  ;;      (with-implicits ((#'id x y))
  ;;        #'(list x y))))
  ;;
  ;;is equivalent to:
  ;;
  ;;   (syntax-case stx ()
  ;;     ((id)
  ;;      (identifier? #'id)
  ;;      (with-syntax ((x (datum->syntax #'id 'x))
  ;;                    (y (datum->syntax #'id 'y)))
  ;;        #'(list x y))))
  ;;
  ;;NOTE This  macro is  derived from  WITH-IMPLICIT, documented  in the
  ;;Chez Scheme User's Guide.  The  two macros have different API; where
  ;;we would use Vicare's variant as:
  ;;
  ;;   (with-implicits ((#'id x y))
  ;;     #'(list x y))
  ;;
  ;;we would use Chez's variant as:
  ;;
  ;;   (with-implicit ((id x y))
  ;;     #'(list x y))
  ;;
  (define (%make-bindings ctx ids)
    (map (lambda (id)
	   `(,id (datum->syntax ,ctx (quote ,id))))
      ids))
  (syntax-match expr-stx ()

    ((_ () ?body0 ?body* ...)
     (bless
      `(begin ,?body0 . ,?body*)))

    ((_ ((?ctx ?symbol0 ?symbol* ...))
	?body0 ?body* ...)
     (let ((BINDINGS (%make-bindings ?ctx (cons ?symbol0 ?symbol*))))
       (bless
	`(with-syntax ,BINDINGS ,?body0 . ,?body*))))

    ((_ ((?ctx ?symbol0 ?symbol* ...) . ?other-clauses)
	?body0 ?body* ...)
     (let ((BINDINGS (%make-bindings ?ctx (cons ?symbol0 ?symbol*))))
       (bless
	`(with-syntax ,BINDINGS (with-implicits ,?other-clauses ,?body0 . ,?body*)))))
    ))


;;;; module non-core-macro-transformer: SET-CONS!

(define (set-cons!-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  SET-CONS! macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?id ?obj)
     (identifier? ?id)
     (bless `(set! ,?id (cons ,?obj ,?id))))
    ))


;;;; module non-core-macro-transformer: compensations

(module (with-compensations/on-error-macro
	 with-compensations-macro)

  (define (with-compensations/on-error-macro expr-stx)
    ;;Transformer     function      used     to      expand     Vicare's
    ;;WITH-COMPENSATIONS/ON-ERROR  macros from  the  top-level built  in
    ;;environment.   Expand the  contents of  EXPR-STX; return  a syntax
    ;;object that must be further expanded.
    ;;
    (syntax-match expr-stx ()
      ((_ ?body0 ?body* ...)
       (bless
	`(let ,(%make-store-binding)
	   (parametrise ((compensations store))
	     ,(%make-with-exception-handler ?body0 ?body*)))))
      ))

  (define (with-compensations-macro expr-stx)
    ;;Transformer  function used  to expand  Vicare's WITH-COMPENSATIONS
    ;;macros  from  the  top-level  built in  environment.   Expand  the
    ;;contents of EXPR-STX; return a  syntax object that must be further
    ;;expanded.
    ;;
    (syntax-match expr-stx ()
      ((_ ?body0 ?body* ...)
       (bless
	`(let ,(%make-store-binding)
	   (parametrise ((compensations store))
	     (begin0
	       ,(%make-with-exception-handler ?body0 ?body*)
	       ;;Better  run  the  cleanup   compensations  out  of  the
	       ;;WITH-EXCEPTION-HANDLER.
	       (run-compensations-store store))))))
      ))

  (define (%make-store-binding)
    '((store (let ((stack '()))
	       (case-lambda
		(()
		 stack)
		((false/thunk)
		 (if false/thunk
		     (set! stack (cons false/thunk stack))
		   (set! stack '()))))))))

  (define (%make-with-exception-handler body0 body*)
    ;;We really have to close the handler upon the STORE function, it is
    ;;wrong to access the COMPENSATIONS parameter from the handler.  The
    ;;dynamic environment  is synchronised with continuations:  when the
    ;;handler is called by  RAISE or RAISE-CONTINUABLE, the continuation
    ;;is the one of the RAISE or RAISE-CONTINUABLE forms.
    ;;
    `(with-exception-handler
	 (lambda (E)
	   (run-compensations-store store)
	   (raise E))
       (lambda ()
	 ,body0 ,@body*)))

  #| end of module |# )

(define (push-compensation-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  PUSH-COMPENSATION
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?release0 ?release* ...)
     (bless
      `(push-compensation-thunk (lambda () ,?release0 ,@?release*))))
    ))

(define (compensate-macro expr-stx)
  ;;Transformer function used to  expand Vicare's COMPENSATE macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define-constant __who__ 'compensate)
  (define (%synner message subform)
    (syntax-violation __who__ message expr-stx subform))
  (syntax-match expr-stx ()
    ((_ ?alloc0 ?form* ...)
     (let ((free #f))
       (define alloc*
	 (let recur ((form-stx ?form*))
	   (syntax-match form-stx (with)
	     (((with ?release0 ?release* ...))
	      (begin
		(set! free `(push-compensation ,?release0 ,@?release*))
		'()))

	     (()
	      (%synner "invalid compensation syntax: missing WITH keyword"
		       form-stx))

	     (((with))
	      (%synner "invalid compensation syntax: empty WITH keyword"
		       (bless '(with))))

	     ((?alloc ?form* ...)
	      (cons ?alloc (recur ?form*)))
	     )))
       (bless
	`(begin0 (begin ,?alloc0 . ,alloc*) ,free))))
    ))


;;;; module non-core-macro-transformer: SYNTAX-RULES, DEFINE-SYNTAX-RULE

(define (syntax-rules-macro expr-stx)
  ;;Transformer function  used to  expand R6RS SYNTAX-RULES  macros from
  ;;the  top-level  built  in  environment.   Process  the  contents  of
  ;;EXPR-STX; return a syntax object that needs to be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?literal* ...)
	(?pattern* ?template*)
	...)
     (begin
       (%verify-literals ?literal* expr-stx)
       (bless
	`(lambda (x)
	   (syntax-case x ,?literal*
	     ,@(map (lambda (pattern template)
		      (syntax-match pattern ()
			((_ . ??rest)
			 `((g . ,??rest)
			   (syntax ,template)))
			(_
			 (syntax-violation #f
			   "invalid syntax-rules pattern"
			   expr-stx pattern))))
		 ?pattern* ?template*))))))))

(define (define-syntax-rule-macro expr-stx)
  ;;Transformer  function  used  to expand  Vicare's  DEFINE-SYNTAX-RULE
  ;;macros  from  the  top-level  built  in  environment.   Process  the
  ;;contents  of EXPR-STX;  return  a  syntax object  that  needs to  be
  ;;further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?name ?arg* ... . ?rest) ?body0 ?body* ...)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name
	 (syntax-rules ()
	   ((_ ,@?arg* . ,?rest)
	    (begin ,?body0 ,@?body*))))))
    ))


;;;; module non-core-macro-transformer: DEFINE-SYNTAX*

(define (define-syntax*-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  DEFINE-SYNTAX* macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name (syntax-rules ()))))

    ((_ ?name ?expr)
     (identifier? ?name)
     (bless
      `(define-syntax ,?name ,?expr)))

    ((_ (?name ?stx) ?body0 ?body* ...)
     (and (identifier? ?name)
	  (identifier? ?stx))
     (let ((SYNNER (datum->syntax ?name 'synner)))
       (bless
	`(define-syntax ,?name
	   (lambda (,?stx)
	     (fluid-let-syntax
		 ((__who__ (identifier-syntax (quote ,?name))))
	       (letrec
		   ((,SYNNER (case-lambda
			      ((message)
			       (,SYNNER message #f))
			      ((message subform)
			       (syntax-violation __who__ message ,?stx subform)))))
		 ,?body0 ,@?body*)))))))
    ))


;;;; module non-core-macro-transformer: WITH-SYNTAX

(define (with-syntax-macro expr-stx)
  ;;Transformer function used to expand R6RS WITH-SYNTAX macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;A WITH-SYNTAX form:
  ;;
  ;;   (with-syntax ((?pat0 ?expr0)
  ;;                 (?pat1 ?expr1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded as follows:
  ;;
  ;;   (syntax-case ?expr0 ()
  ;;     (?pat0
  ;;      (syntax-case ?expr1 ()
  ;;       (?pat1
  ;;        (internal-body ?body0 ?body ...))
  ;;       (_
  ;;        (assertion-violation ---))))
  ;;     (_
  ;;      (assertion-violation ---)))
  ;;
  (syntax-match expr-stx ()
    ((_ ((?pat* ?expr*) ...) ?body ?body* ...)
     (let ((idn* (let recur ((pat* ?pat*))
		   (if (null? pat*)
		       '()
		     (receive (pat idn*)
			 (convert-pattern (car pat*) '())
		       (append idn* (recur (cdr pat*))))))))
       (let ((formals (map car idn*)))
	 (unless (standard-formals-syntax? formals)
	   (%error-invalid-formals-syntax expr-stx formals)))
       (let ((t* (generate-temporaries ?expr*)))
	 (bless
	  `(let ,(map list t* ?expr*)
	     ,(let recur ((pat* ?pat*)
			  (t*   t*))
		(if (null? pat*)
		    `(internal-body ,?body . ,?body*)
		  `(syntax-case ,(car t*) ()
		     (,(car pat*)
		      ,(recur (cdr pat*) (cdr t*)))
		     (_
		      (assertion-violation 'with-syntax
			"pattern does not match value"
			',(car pat*) ,(car t*)))))))))))
    ))


;;;; module non-core-macro-transformer: IDENTIFIER-SYNTAX

(define (identifier-syntax-macro stx)
  ;;Transformer function  used to  expand R6RS  IDENTIFIER-SYNTAX macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match stx (set!)
    ((_ ?expr)
     (bless
      `(lambda (x)
	 (syntax-case x ()
	   (??id
	    (identifier? (syntax ??id))
	    (syntax ,?expr))
	   ((??id ??expr* ...)
	    (identifier? (syntax ??id))
	    (cons (syntax ,?expr) (syntax (??expr* ...))))
	   ))))

    ((_ (?id1
	 ?expr1)
	((set! ?id2 ?expr2)
	 ?expr3))
     (and (identifier? ?id1)
	  (identifier? ?id2)
	  (identifier? ?expr2))
     (bless
      `(make-variable-transformer
	(lambda (x)
	  (syntax-case x (set!)
	    (??id
	     (identifier? (syntax ??id))
	     (syntax ,?expr1))
	    ((set! ??id ,?expr2)
	     (syntax ,?expr3))
	    ((??id ??expr* ...)
	     (identifier? (syntax ??id))
	     (syntax (,?expr1 ??expr* ...))))))))
    ))


;;;; module non-core-macro-transformer: LET*, TRACE-LET

(define (let*-macro expr-stx)
  ;;Transformer  function  used to  expand  R6RS  LET* macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
     ;;Remember that LET* allows bindings with duplicate identifiers, so
     ;;we do *not* use LIST-OF-TAGGED-BINDINGS? here.
     (for-all tagged-identifier-syntax? ?lhs*)
     (bless
      (let recur ((x* (map list ?lhs* ?rhs*)))
	(if (null? x*)
	    `(internal-body ,?body . ,?body*)
	  `(let (,(car x*)) ,(recur (cdr x*)))))))
    ))

(define (trace-let-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  TRACE-LET macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?recur ((?lhs* ?rhs*) ...) ?body ?body* ...)
     (identifier? ?recur)
     (receive (lhs* tag*)
	 (parse-list-of-tagged-bindings ?lhs* expr-stx)
       (bless
	`((letrec ((,?recur (trace-lambda ,?recur ,?lhs*
					  ,?body . ,?body*)))
	    ,?recur)
	  . ,(map (lambda (rhs tag)
		    `(tag-assert-and-return (,tag) ,rhs))
	       ?rhs* tag*)))))
    ))


;;;; module non-core-macro-transformer: LET-VALUES

(module (let-values-macro)
  ;;Transformer function  used to  expand R6RS LET-VALUES  macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  ;;A LET-VALUES syntax like:
  ;;
  ;;   (let-values (((a b c) rhs0)
  ;;                ((d e f) rhs1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded to:
  ;;
  ;;   (call-with-values
  ;;       (lambda () rhs0)
  ;;     (lambda (G.a G.b G.c)
  ;;       (call-with-values
  ;;           (lambda () rhs1)
  ;;         (lambda (G.d G.e G.f)
  ;;           (let ((a G.a) (b G.b) (c G.c)
  ;;                 (c G.c) (d G.d) (e G.e))
  ;;             ?body0 ?body)))))
  ;;
  (define-fluid-override __who__
    (identifier-syntax 'let-values))

  (define (let-values-macro input-form.stx)
    (syntax-match input-form.stx ()
      ((_ () ?body ?body* ...)
       (cons* (bless 'let) '() ?body ?body*))

      ((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
       (receive (lhs*.standard lhs*.signature)
	   (let loop ((lhs*           ?lhs*)
		      (lhs*.standard  '())
		      (lhs*.signature '()))
	     (if (null? lhs*)
		 (values (reverse lhs*.standard)
			 (reverse lhs*.signature))
	       (receive (lhs.standard lhs.signature)
		   (parse-tagged-formals-syntax (car lhs*) input-form.stx)
		 (loop (cdr lhs*)
		       (cons lhs.standard                           lhs*.standard)
		       (cons (formals-signature-tags lhs.signature) lhs*.signature)))))
	 (bless
	  (let recur ((lhs*.standard  lhs*.standard)
		      (lhs*.signature lhs*.signature)
		      (lhs*.tagged    (syntax-unwrap ?lhs*))
		      (rhs*           ?rhs*)
		      (standard-old*  '())
		      (tagged-old*    '())
		      (new*           '()))
	    (if (null? lhs*.standard)
		`(let ,(map list tagged-old* new*)
		   ,?body . ,?body*)
	      (syntax-match (car lhs*.standard) ()
		((?standard-formal* ...)
		 (receive (y* standard-old* tagged-old* new*)
		     (%rename* ?standard-formal* (car lhs*.tagged) standard-old* tagged-old* new* input-form.stx)
		   `(call-with-values
			(lambda ()
			  (tag-assert-and-return ,(car lhs*.signature) ,(car rhs*)))
		      (lambda ,y*
			,(recur (cdr lhs*.standard) (cdr lhs*.signature) (cdr lhs*.tagged)
				(cdr rhs*) standard-old* tagged-old* new*)))))

		((?standard-formal* ... . ?standard-rest-formal)
		 (receive (tagged-formal* tagged-rest-formal)
		     (improper-list->list-and-rest (car lhs*.tagged))
		   (let*-values
		       (((y  standard-old* tagged-old* new*)
			 (%rename  ?standard-rest-formal tagged-rest-formal standard-old* tagged-old* new* input-form.stx))
			((y* standard-old* tagged-old* new*)
			 (%rename* ?standard-formal*     tagged-formal*     standard-old* tagged-old* new* input-form.stx)))
		     `(call-with-values
			  (lambda () ,(car rhs*))
			(lambda ,(append y* y)
			  ,(recur (cdr lhs*.standard) (cdr lhs*.signature) (cdr lhs*.tagged)
				  (cdr rhs*) standard-old* tagged-old* new*))))))
		(?others
		 (syntax-violation __who__ "malformed bindings" input-form.stx ?others))))))))
      ))

  (define (%rename standard-formal tagged-formal standard-old* tagged-old* new* input-form.stx)
    (when (bound-id-member? standard-formal standard-old*)
      (syntax-violation __who__ "duplicate binding" input-form.stx standard-formal))
    (let ((y (gensym (syntax->datum standard-formal))))
      (values y (cons standard-formal standard-old*) (cons tagged-formal tagged-old*) (cons y new*))))

  (define (%rename* standard-formal* tagged-formal* standard-old* tagged-old* new* input-form.stx)
    (if (null? standard-formal*)
	(values '() standard-old* tagged-old* new*)
      (let*-values
	  (((y  standard-old* tagged-old* new*)
	    (%rename  (car standard-formal*) (car tagged-formal*) standard-old* tagged-old* new* input-form.stx))
	   ((y* standard-old* tagged-old* new*)
	    (%rename* (cdr standard-formal*) (cdr tagged-formal*) standard-old* tagged-old* new* input-form.stx)))
	(values (cons y y*) standard-old* tagged-old* new*))))

  #| end of module: LET-VALUES-MACRO |# )


;;;; module non-core-macro-transformer: LET*-VALUES

(define (let*-values-macro expr-stx)
  ;;Transformer function used to expand R6RS LET*-VALUES macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;A LET*-VALUES syntax like:
  ;;
  ;;   (let*-values (((a b c) rhs0)
  ;;                 ((d e f) rhs1))
  ;;     ?body0 ?body ...)
  ;;
  ;;is expanded to:
  ;;
  ;;   (call-with-values
  ;;       (lambda () rhs0)
  ;;     (lambda (a b c)
  ;;       (call-with-values
  ;;           (lambda () rhs1)
  ;;         (lambda (d e f)
  ;;           (begin ?body0 ?body)))))
  ;;
  (syntax-match expr-stx ()
    ((_ () ?body ?body* ...)
     (cons* (bless 'let) '() ?body ?body*))

    ((_ ((?lhs ?rhs)) ?body ?body* ...)
     (bless
      `(let-values ((,?lhs ,?rhs)) ,?body . ,?body*)))

    ((_ ((?lhs0 ?rhs0) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(let-values ((,?lhs0 ,?rhs0))
	 (let*-values ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))
    ))


;;;; module non-core-macro-transformer: VALUES->LIST-MACRO

(define (values->list-macro expr-stx)
  ;;Transformer  function used  to expand  Vicare's VALUES->LIST  macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?expr)
     (bless
      `(call-with-values
	   (lambda () ,?expr)
	 list)))))


;;;; module non-core-macro-transformer: LET*-SYNTAX

(define (let*-syntax-macro expr-stx)
  ;;Transformer function used to expand Vicare's LET*-SYNTAX macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(begin ,?body . ,?body*)))
    ;;Single binding.
    ((_ ((?lhs ?rhs)) ?body ?body* ...)
     (bless
      `(let-syntax ((,?lhs ,?rhs))
	 ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(let-syntax ((,?lhs ,?rhs))
	 (let*-syntax ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))
    ))


;;;; module non-core-macro-transformer: LET-CONSTANTS, LET*-CONSTANTS, LETREC-CONSTANTS, LETREC*-CONSTANTS

(define (let-constants-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  LET-CONSTANTS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(internal-body ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (let ((SHADOW* (generate-temporaries (cons ?lhs ?lhs*))))
       (bless
	`(let ,(map list SHADOW* (cons ?rhs ?rhs*))
	   (let-syntax ,(map (lambda (lhs shadow)
			       `(,lhs (identifier-syntax ,shadow)))
			  (cons ?lhs ?lhs*) SHADOW*)
	     ,?body . ,?body*)))))
    ))

(define (let*-constants-macro expr-stx)
  ;;Transformer function  used to expand Vicare's  LET*-CONSTANTS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ;;No bindings.
    ((_ () ?body ?body* ...)
     (bless
      `(internal-body ,?body . ,?body*)))
    ;;Multiple bindings
    ((_ ((?lhs ?rhs) (?lhs* ?rhs*) ...) ?body ?body* ...)
     (bless
      `(let-constants ((,?lhs ,?rhs))
	 (let*-constants ,(map list ?lhs* ?rhs*)
	   ,?body . ,?body*))))
    ))

(define (letrec-constants-macro expr-stx)
  ;;Transformer function used to expand Vicare's LETREC-CONSTANTS macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ () ?body0 ?body* ...)
     (bless
      `(internal-body ,?body0 . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (let ((TMP* (generate-temporaries ?lhs*))
	   (VAR* (generate-temporaries ?lhs*)))
       (bless
	`(let ,(map (lambda (var)
		      `(,var (void)))
		 VAR*)
	   (let-syntax ,(map (lambda (lhs var)
			       `(,lhs (identifier-syntax ,var)))
			  ?lhs* VAR*)
	     ;;Do not enforce the order of evaluation of ?RHS.
	     (let ,(map list TMP* ?rhs*)
	       ,@(map (lambda (var tmp)
			`(set! ,var ,tmp))
		   VAR* TMP*)
	       (internal-body ,?body0 . ,?body*)))))))
    ))

(define (letrec*-constants-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  LETREC*-CONSTANTS
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ () ?body0 ?body* ...)
     (bless
      `(internal-body ,?body0 . ,?body*)))

    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (let ((TMP* (generate-temporaries ?lhs*))
	   (VAR* (generate-temporaries ?lhs*)))
       (bless
	`(let ,(map (lambda (var)
		      `(,var (void)))
		 VAR*)
	   (let-syntax ,(map (lambda (lhs var)
			       `(,lhs (identifier-syntax ,var)))
			  ?lhs* VAR*)
	     ;;Do enforce the order of evaluation of ?RHS.
	     (let* ,(map list TMP* ?rhs*)
	       ,@(map (lambda (var tmp)
			`(set! ,var ,tmp))
		   VAR* TMP*)
	       (internal-body ,?body0 . ,?body*)))))))
    ))


;;;; module non-core-macro-transformer: CASE-DEFINE

(define (case-define-macro expr-stx)
  ;;Transformer function used to expand Vicare's CASE-DEFINE macros from
  ;;the  top-level  built  in   environment.   Expand  the  contents  of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?who ?cl-clause ?cl-clause* ...)
     (identifier? ?who)
     (bless
      `(define ,?who
	 (case-lambda ,?cl-clause . ,?cl-clause*))))
    ))


;;;; module non-core-macro-transformer: DEFINE*, LAMBDA*, CASE-DEFINE*, CASE-LAMBDA*

(module (lambda*-macro
	 define*-macro
	 case-lambda*-macro
	 case-define*-macro)

  (define-record argument-validation-spec
    (arg-id
		;Identifier representing the formal name of the argument
		;being validated.
     expr
		;Syntax  object  representing   an  argument  validation
		;expression.
     ))

  (define-record retval-validation-spec
    (rv-id
		;Identifier representing the internal formal name of the
		;return value being validated.
     pred
		;Identifier bound to the predicate  to be applied to the
		;return value.
     ))

;;; --------------------------------------------------------------------

  (module (define*-macro)
    ;;Transformer function  used to expand Vicare's  DEFINE* macros from
    ;;the  top-level  built  in  environment.  Expand  the  contents  of
    ;;EXPR-STX.  Return a syntax object that must be further expanded.
    ;;
    ;;We want to implement the following example expansions:
    ;;
    ;;  (define* ?id ?value)	==> (define ?id ?value)
    ;;  (define* ?id)		==> (define ?id)
    ;;
    ;;  (define* (?who . ?common-formals) . ?body)
    ;;  ==> (define (?who . ?common-formals)
    ;;        (fluid-let-syntax
    ;;            ((__who__ (identifier-syntax (quote ?who))))
    ;;          (internal-body . ?body)))
    ;;
    ;;  (define* (?who (brace ?var ?pred)) . ?body)
    ;;  ==> (define (?who ?var)
    ;;        (fluid-let-syntax
    ;;            ((__who__ (identifier-syntax (quote ?who))))
    ;;          (unless (?pred ?var)
    ;; 	          (procedure-argument-violation __who__
    ;; 	            "failed argument validation" '(?pred ?var) ?var))
    ;;          (internal-body . ?body)))
    ;;
    ;;  (define* ((brace ?who ?pred) ?var) . ?body)
    ;;  ==> (define (?who ?var)
    ;;        (fluid-let-syntax
    ;;            ((__who__ (identifier-syntax (quote ?who))))
    ;;          (receive-and-return (rv)
    ;;              (internal-body . ?body)
    ;;            (unless (?pred rv)
    ;;              (expression-return-value-violation __who__
    ;; 	              "failed return value validation" (list '?pred rv))))))
    ;;
    (define (define*-macro stx)
      (define (%synner message subform)
	(syntax-violation 'define* message stx subform))
      (syntax-match stx (brace)
	;;No ret-pred.
	((_ (?who . ?formals) ?body0 ?body* ...)
	 (identifier? ?who)
	 (%generate-define-output-form/without-ret-pred ?who ?formals (cons ?body0 ?body*) %synner))

	;;Return value predicates.
	((_ ((brace ?who ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (identifier? ?who)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-define-output-form/with-ret-pred ?who (cons ?ret-pred0 ?ret-pred*) ?formals (cons ?body0 ?body*) %synner))

	((_ ?who ?expr)
	 (identifier? ?who)
	 (bless
	  `(define ,?who ,?expr)))

	((_ ?who)
	 (identifier? ?who)
	 (bless
	  `(define ,?who (void))))

	))

    (define (%generate-define-output-form/without-ret-pred ?who ?predicate-formals ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner)))
	  (bless
	   `(define (,?who . ,?standard-formals)
	      (fluid-let-syntax
		  ((__who__ (identifier-syntax (quote ,?who))))
		,@ARG-VALIDATION*
		(internal-body . ,?body*)))))))

    (define (%generate-define-output-form/with-ret-pred ?who ?ret-pred* ?predicate-formals ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner))
	       (RET*            (generate-temporaries ?ret-pred*))
	       (RET-VALIDATION  (%make-ret-validation-form (map make-retval-validation-spec RET* ?ret-pred*)
							   synner)))
	  (bless
	   `(define (,?who . ,?standard-formals)
	      (fluid-let-syntax
		  ((__who__ (identifier-syntax (quote ,?who))))
		,@ARG-VALIDATION*
		(receive-and-return (,@RET*)
		    (internal-body . ,?body*)
		  ,RET-VALIDATION)))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (case-define*-macro)

    (define (case-define*-macro stx)
      ;;Transformer function used to expand Vicare's CASE-DEFINE* macros
      ;;from the top-level built in environment.  Expand the contents of
      ;;EXPR-STX.  Return a syntax object that must be further expanded.
      ;;
      (define (%synner message subform)
	(syntax-violation 'case-define* message stx subform))
      (syntax-match stx ()
	((_ ?who ?clause0 ?clause* ...)
	 (identifier? ?who)
	 (bless
	  `(define ,?who
	     (case-lambda
	      ,@(map (lambda (?clause)
		       (%generate-case-define-form ?who ?clause %synner))
		  (cons ?clause0 ?clause*))))))
	))

    (define (%generate-case-define-form ?who ?clause synner)
      (syntax-match ?clause (brace)
	;;Return value predicates.
	((((brace ?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-case-define-clause-form/with-ret-pred ?who (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* synner))

	;;No ret-pred.
	((?formals ?body0 ?body* ...)
	 (%generate-case-define-clause-form/without-ret-pred ?who ?formals ?body0 ?body* synner))
	))

    (define (%generate-case-define-clause-form/without-ret-pred ?who ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner)))
	  `(,?standard-formals
	    (fluid-let-syntax
		((__who__ (identifier-syntax (quote ,?who))))
	      ,@ARG-VALIDATION*
	      (internal-body ,?body0 ,@?body*))))))

    (define (%generate-case-define-clause-form/with-ret-pred ?who ?ret-pred* ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner))
	       (RET*            (generate-temporaries ?ret-pred*))
	       (RET-VALIDATION  (%make-ret-validation-form (map make-retval-validation-spec RET* ?ret-pred*)
							   synner)))
	  `(,?standard-formals
	    (fluid-let-syntax
		((__who__ (identifier-syntax (quote ,?who))))
	      ,@ARG-VALIDATION*
	      (receive-and-return (,@RET*)
		  (internal-body ,?body0 ,@?body*)
		,RET-VALIDATION))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (lambda*-macro)

    (define (lambda*-macro stx)
      ;;Transformer function used to expand Vicare's LAMBDA* macros from
      ;;the  top-level built  in  environment.  Expand  the contents  of
      ;;EXPR-STX.  Return a syntax object that must be further expanded.
      ;;
      (define (%synner message subform)
	(syntax-violation 'lambda* message stx subform))
      (syntax-match stx (brace)
	;;Ret-pred with list spec.
	((?kwd ((brace ?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-lambda-output-form/with-ret-pred ?kwd (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* %synner))

	;;No ret-pred.
	((?kwd ?formals ?body0 ?body* ...)
	 (%generate-lambda-output-form/without-ret-pred ?kwd ?formals ?body0 ?body* %synner))

	))

    (define (%generate-lambda-output-form/without-ret-pred ?ctx ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner)))
	  (bless
	   `(lambda ,?standard-formals
	      (fluid-let-syntax
		  ((__who__ (identifier-syntax (quote _))))
		,@ARG-VALIDATION*
		(internal-body ,?body0 ,@?body*)))))))

    (define (%generate-lambda-output-form/with-ret-pred ?ctx ?ret-pred* ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner))
	       (RET*            (generate-temporaries ?ret-pred*))
	       (RET-VALIDATION  (%make-ret-validation-form (map make-retval-validation-spec RET* ?ret-pred*)
							   synner)))
	  (bless
	   `(lambda ,?standard-formals
	      (fluid-let-syntax
		  ((__who__ (identifier-syntax (quote _))))
		,@ARG-VALIDATION*
		(receive-and-return (,@RET*)
		    (internal-body ,?body0 ,@?body*)
		  ,RET-VALIDATION)))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module (case-lambda*-macro)

    (define (case-lambda*-macro stx)
      ;;Transformer function used to expand Vicare's CASE-LAMBDA* macros
      ;;from the top-level built in environment.  Expand the contents of
      ;;EXPR-STX.  Return a syntax object that must be further expanded.
      ;;
      (define (%synner message subform)
	(syntax-violation 'case-lambda* message stx subform))
      (syntax-match stx ()
	((?kwd ?clause0 ?clause* ...)
	 (bless
	  `(case-lambda
	    ,@(map (lambda (?clause)
		     (%generate-case-lambda-form ?kwd ?clause %synner))
		(cons ?clause0 ?clause*)))))
	))

    (define (%generate-case-lambda-form ?ctx ?clause synner)
      (syntax-match ?clause (brace)
	;;Ret-pred with list spec.
	((((brace ?underscore ?ret-pred0 ?ret-pred* ...) . ?formals) ?body0 ?body* ...)
	 (and (%underscore? ?underscore)
	      (identifier? ?ret-pred0)
	      (for-all identifier? ?ret-pred*))
	 (%generate-case-lambda-clause-form/with-ret-pred ?ctx (cons ?ret-pred0 ?ret-pred*) ?formals ?body0 ?body* synner))

	;;No ret-pred.
	((?formals ?body0 ?body* ...)
	 (%generate-case-lambda-clause-form/without-ret-pred ?ctx ?formals ?body0 ?body* synner))
	))

    (define (%generate-case-lambda-clause-form/without-ret-pred ?ctx ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner)))
	  `(,?standard-formals
	    (fluid-let-syntax
		((__who__ (identifier-syntax (quote _))))
	      ,@ARG-VALIDATION*
	      (internal-body ,?body0 ,@?body*))))))

    (define (%generate-case-lambda-clause-form/with-ret-pred ?ctx ?ret-pred* ?predicate-formals ?body0 ?body* synner)
      (receive (?standard-formals arg-validation-spec*)
	  (%parse-predicate-formals ?predicate-formals synner)
	(let* ((ARG-VALIDATION* (%make-arg-validation-forms arg-validation-spec* synner))
	       (RET*            (generate-temporaries ?ret-pred*))
	       (RET-VALIDATION  (%make-ret-validation-form (map make-retval-validation-spec RET* ?ret-pred*)
							   synner)))
	  `(,?standard-formals
	    (fluid-let-syntax
		((__who__ (identifier-syntax (quote _))))
	      ,@ARG-VALIDATION*
	      (receive-and-return (,@RET*)
		  (internal-body ,?body0 ,@?body*)
		,RET-VALIDATION))))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (%parse-predicate-formals ?predicate-formals synner)
    ;;Split  formals from  tags.   We  rely on  the  DEFINE, LAMBDA  and
    ;;CASE-LAMBDA syntaxes  in the output  form to further  validate the
    ;;formals against duplicate bindings.
    ;;
    ;;We use  the conventions: ?ID,  ?REST-ID and ?ARGS-ID  are argument
    ;;identifiers; ?PRED is a predicate identifier.
    ;;
    ;;We accept the following standard formals formats:
    ;;
    ;;   ?args-id
    ;;   (?id ...)
    ;;   (?id0 ?id ... . ?rest-id)
    ;;
    ;;and in addition the following predicate formals:
    ;;
    ;;   (brace ?args-id ?args-pred)
    ;;   (?arg ...)
    ;;   (?arg0 ?arg ... . ?rest-arg)
    ;;
    ;;where ?ARG is a predicate argument with one of the formats:
    ;;
    ;;   ?id
    ;;   (brace ?id ?pred)
    ;;
    ;;Return 2 values:
    ;;
    ;;* A list  of syntax objects representing the  standard formals for
    ;;  the DEFINE, LAMBDA and CASE-LAMBDA syntaxes.
    ;;
    ;;* A list of  ARGUMENT-VALIDATION-SPEC structures each representing
    ;;  a validation predicate.
    ;;
    (syntax-match ?predicate-formals (brace)

      ;;Tagged args.
      ;;
      ((brace ?args-id ?args-pred)
       (and (identifier? ?args-id)
	    (identifier? ?args-pred))
       (values ?args-id
	       (list (make-argument-validation-spec ?args-id (list ?args-pred ?args-id)))))

      ;;Possibly tagged identifiers with tagged rest argument.
      ;;
      ((?pred-arg* ... . (brace ?rest-id ?rest-pred))
       (begin
	 (unless (and (identifier? ?rest-id)
		      (identifier? ?rest-pred))
	   (synner "invalid rest argument specification" (cons 'brace ?rest-id ?rest-pred)))
	 (let recur ((?pred-arg* ?pred-arg*))
	   (if (pair? ?pred-arg*)
	       (receive (?standard-formals arg-validation-spec*)
		   (recur (cdr ?pred-arg*))
		 (let ((?pred-arg (car ?pred-arg*)))
		   (syntax-match ?pred-arg (brace)
		     ;;Untagged argument.
		     (?id
		      (identifier? ?id)
		      (values (cons ?id ?standard-formals) arg-validation-spec*))
		     ;;Tagged argument.
		     ((brace ?id ?pred)
		      (and (identifier? ?id)
			   (identifier? ?pred))
		      (values (cons ?id ?standard-formals)
			      (cons (make-argument-validation-spec ?id (list ?pred ?id)) arg-validation-spec*)))
		     (else
		      (synner "invalid argument specification" ?pred-arg)))))
	     ;;Process rest argument.
	     (values ?rest-id
		     (list (make-argument-validation-spec ?rest-id (list ?rest-pred ?rest-id))))))))

      ;;Possibly tagged identifiers with UNtagged rest argument.
      ;;
      ((?pred-arg* ... . ?rest-id)
       (identifier? ?rest-id)
       (let recur ((?pred-arg* ?pred-arg*))
	 (if (pair? ?pred-arg*)
	     (receive (?standard-formals arg-validation-spec*)
		 (recur (cdr ?pred-arg*))
	       (let ((?pred-arg (car ?pred-arg*)))
		 (syntax-match ?pred-arg (brace)
		   ;;Untagged argument.
		   (?id
		    (identifier? ?id)
		    (values (cons ?id ?standard-formals) arg-validation-spec*))
		   ;;Tagged argument.
		   ((brace ?id ?pred)
		    (and (identifier? ?id)
			 (identifier? ?pred))
		    (values (cons ?id ?standard-formals)
			    (cons (make-argument-validation-spec ?id (list ?pred ?id)) arg-validation-spec*)))
		   (else
		    (synner "invalid argument specification" ?pred-arg)))))
	   (values ?rest-id '()))))

      ;;Standard formals: untagged identifiers without rest argument.
      ;;
      ((?id* ...)
       (for-all identifier? ?id*)
       (values ?id* '()))

      ;;Standard formals: untagged identifiers with rest argument.
      ;;
      ((?id* ... . ?rest-id)
       (and (for-all identifier? ?id*)
	    (identifier? ?rest-id))
       (values ?predicate-formals '()))

      ;;Standard formals: untagged args.
      ;;
      (?args-id
       (identifier? ?args-id)
       (values ?args-id '()))

      ;;Possibly tagged identifiers without rest argument.
      ;;
      ((?pred-arg* ...)
       (let recur ((?pred-arg* ?pred-arg*))
	 (if (pair? ?pred-arg*)
	     (receive (?standard-formals arg-validation-spec*)
		 (recur (cdr ?pred-arg*))
	       (let ((?pred-arg (car ?pred-arg*)))
		 (syntax-match ?pred-arg (brace)
		   ;;Untagged argument.
		   (?id
		    (identifier? ?id)
		    (values (cons ?id ?standard-formals) arg-validation-spec*))
		   ;;Tagged argument.
		   ((brace ?id ?pred)
		    (and (identifier? ?id)
			 (identifier? ?pred))
		    (values (cons ?id ?standard-formals)
			    (cons (make-argument-validation-spec ?id (list ?pred ?id)) arg-validation-spec*)))
		   (else
		    (synner "invalid argument specification" ?pred-arg)))))
	   (values '() '()))))
      ))

;;; --------------------------------------------------------------------

  (define (%make-arg-validation-forms arg-validation-spec* synner)
    (if (option.enable-arguments-validation?)
	(map (lambda (spec)
	       (let ((?arg-expr (argument-validation-spec-expr   spec))
		     (?arg-id   (argument-validation-spec-arg-id spec)))
		 `(unless ,?arg-expr
		    (procedure-argument-violation __who__
		      "failed argument validation"
		      (quote ,?arg-expr) ,?arg-id))))
	  arg-validation-spec*)
      '()))

  (define (%make-ret-validation-form retval-validation-spec* synner)
    (if (option.enable-arguments-validation?)
	`(begin
	   ,@(map (lambda (spec)
		    (let ((?pred (retval-validation-spec-pred  spec))
			  (?ret  (retval-validation-spec-rv-id spec)))
		      `(unless (,?pred ,?ret)
			 (expression-return-value-violation __who__
			   "failed return value validation"
			   ;;This list  represents the application  of the
			   ;;predicate to the offending value.
			   (list (quote ,?pred) ,?ret)))))
	       retval-validation-spec*))
      '(void)))

  (define (%underscore? stx)
    (and (identifier? stx)
	 (free-identifier=? stx (scheme-stx '_))))

  #| end of module |# )


;;;; module non-core-macro-transformer: TRACE-LAMBDA, TRACE-DEFINE and TRACE-DEFINE-SYNTAX

(define (trace-lambda-macro expr-stx)
  ;;Transformer  function used  to expand  Vicare's TRACE-LAMBDA  macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?who (?formal* ...) ?body ?body* ...)
     (begin
       ;;We parse the formals for validation purposes.
       (parse-tagged-lambda-proto-syntax ?formal* expr-stx)
       (bless
	`(make-traced-procedure ',?who
				(lambda ,?formal*
				  ,?body . ,?body*)))))

    ((_ ?who (?formal* ... . ?rest-formal) ?body ?body* ...)
     (begin
       ;;We parse the formals for validation purposes.
       (parse-tagged-lambda-proto-syntax (append ?formal* ?rest-formal) expr-stx)
       (bless
	`(make-traced-procedure ',?who
				(lambda (,@?formal* . ,?rest-formal)
				  ,?body . ,?body*)))))
    ))

(define (trace-define-macro expr-stx)
  ;;Transformer  function used  to expand  Vicare's TRACE-DEFINE  macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?who ?formal* ...) ?body ?body* ...)
     (begin
       ;;We parse the formals for validation purposes.
       (parse-tagged-lambda-proto-syntax ?formal* expr-stx)
       (bless
	`(define ,?who
	   (make-traced-procedure ',?who
				  (lambda ,?formal*
				    ,?body . ,?body*))))))

    ((_ (?who ?formal* ... . ?rest-formal) ?body ?body* ...)
     (begin
       ;;We parse the formals for validation purposes.
       (parse-tagged-lambda-proto-syntax (append ?formal* ?rest-formal) expr-stx)
       (bless
	`(define ,?who
	   (make-traced-procedure ',?who
				  (lambda (,@?formal* . ,?rest-formal)
				    ,?body . ,?body*))))))

    ((_ ?who ?expr)
     (if (identifier? ?who)
	 (bless `(define ,?who
		   (let ((v ,?expr))
		     (if (procedure? v)
			 (make-traced-procedure ',?who v)
		       v))))
       (stx-error expr-stx "invalid name")))
    ))

(define (trace-define-syntax-macro expr-stx)
  ;;Transformer  function used  to  expand Vicare's  TRACE-DEFINE-SYNTAX
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?who ?expr)
     (if (identifier? ?who)
	 (bless
	  `(define-syntax ,?who
	     (make-traced-macro ',?who ,?expr)))
       (stx-error expr-stx "invalid name")))
    ))


;;;; module non-core-macro-transformer: TRACE-LET-SYNTAX, TRACE-LETREC-SYNTAX

(module (trace-let-syntax-macro
	 trace-letrec-syntax-macro)

  (define (%trace-let/rec-syntax who)
    (lambda (stx)
      (syntax-match stx ()
	((_ ((?lhs* ?rhs*) ...) ?body ?body* ...)
	 (if (valid-bound-ids? ?lhs*)
	     (let ((rhs* (map (lambda (lhs rhs)
				`(make-traced-macro ',lhs ,rhs))
			   ?lhs* ?rhs*)))
	       (bless
		`(,who ,(map list ?lhs* rhs*)
		       ,?body . ,?body*)))
	   (%error-invalid-formals-syntax stx ?lhs*)))
	)))

  (define trace-let-syntax-macro
    ;;Transformer  function  used  to expand  Vicare's  TRACE-LET-SYNTAX
    ;;macros  from  the  top-level  built in  environment.   Expand  the
    ;;contents of EXPR-STX; return a  syntax object that must be further
    ;;expanded.
    ;;
    (%trace-let/rec-syntax 'let-syntax))

  (define trace-letrec-syntax-macro
    ;;Transformer function  used to expand  Vicare's TRACE-LETREC-SYNTAX
    ;;macros  from  the  top-level  built in  environment.   Expand  the
    ;;contents of EXPR-STX; return a  syntax object that must be further
    ;;expanded.
    ;;
    (%trace-let/rec-syntax 'letrec-syntax))

  #| end of module |# )


;;;; module non-core-macro-transformer: GUARD

(module (guard-macro)

  (define (guard-macro x)
    ;;Transformer function  used to  expand R6RS  GUARD macros  from the
    ;;top-level built in environment.   Expand the contents of EXPR-STX;
    ;;return a syntax object that must be further expanded.
    ;;
    ;;A syntax without else clause like:
    ;;
    ;;   (guard (E
    ;;           (?test0 ?expr0)
    ;;           (?test1 ?expr1)))
    ;;     ?body0 ?body ...)
    ;;
    ;;is expanded to:
    ;;
    ;;   ((call/cc
    ;;        (lambda (outerk)
    ;;          (lambda ()
    ;;            (with-exception-handler
    ;; 	              (lambda (raised-obj)
    ;; 	                (let ((E raised-obj))
    ;;                    ((call/cc
    ;; 		               (lambda (raisek)
    ;; 		                 (outerk (lambda ()
    ;; 	                                   (if ?test0
    ;; 	                                       ?expr0
    ;;                  	             (if ?test1
    ;; 	                                         ?expr1
    ;; 	                                       (raisek (lambda ()
    ;;                                                   (raise-continuable raised-obj))))))))))))
    ;;              (lambda ()
    ;; 	              ?body0 ?body ...))))))
    ;;
    (syntax-match x ()
      ((_ (?variable ?clause* ...) ?body ?body* ...)
       (identifier? ?variable)
       (let ((outerk-id     (gensym))
	     (raised-obj-id (gensym)))
	 (bless
	  `((call/cc
		(lambda (,outerk-id)
		  (lambda ()
		    (with-exception-handler
			(lambda (,raised-obj-id)
			  (let ((,?variable ,raised-obj-id))
			    ,(gen-clauses raised-obj-id outerk-id ?clause*)))
		      (lambda ()
			,?body . ,?body*))))))
	  )))
      ))

  (define (gen-clauses raised-obj-id outerk-id clause*)

    (define (%process-single-cond-clause clause kont-code-stx)
      (syntax-match clause (=>)
	((?test => ?proc)
	 (let ((t (gensym)))
	   `(let ((,t ,?test))
	      (if ,t
		  (,?proc ,t)
		,kont-code-stx))))

	((?test)
	 (let ((t (gensym)))
	   `(let ((,t ,?test))
	      (if ,t ,t ,kont-code-stx))))

	((?test ?expr ?expr* ...)
	 `(if ,?test
	      (begin ,?expr . ,?expr*)
	    ,kont-code-stx))

	(_
	 (stx-error clause "invalid guard clause"))))

    (define (%process-multi-cond-clauses clause*)
      (syntax-match clause* (else)
	;;There is no ELSE clause: introduce the raise continuation that
	;;rethrows the exception.
	(()
	 (let ((raisek (gensym)))
	   (values `(,raisek (lambda ()
			       (raise-continuable ,raised-obj-id)))
		   raisek)))

	;;There  is an  ELSE  clause:  no need  to  introduce the  raise
	;;continuation.
	(((else ?else-body ?else-body* ...))
	 (values `(begin ,?else-body . ,?else-body*)
		 #f))

	((?clause . ?clause*)
	 (receive (code-stx raisek)
	     (%process-multi-cond-clauses ?clause*)
	   (values (%process-single-cond-clause ?clause code-stx)
		   raisek)))

	(others
	 (stx-error others "invalid guard clause"))))

    (receive (code-stx raisek)
	(%process-multi-cond-clauses clause*)
      (if raisek
	  `((call/cc
		(lambda (,raisek)
		  (,outerk-id (lambda () ,code-stx)))))
	`(,outerk-id (lambda () ,code-stx)))))

  #| end of module: GUARD-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-ENUMERATION

(define (define-enumeration-macro stx)
  ;;Transformer function  used to expand R6RS  DEFINE-ENUMERATION macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define-constant __who__ 'define-enumeration)
  (define (set? x)
    (or (null? x)
	(and (not (memq (car x) (cdr x)))
	     (set? (cdr x)))))
  (define (remove-dups ls)
    (if (null? ls)
	'()
      (cons (car ls)
	    (remove-dups (remq (car ls) (cdr ls))))))
  (syntax-match stx ()
    ((_ ?name (?id* ...) ?maker)
     (begin
       (unless (identifier? ?name)
	 (syntax-violation __who__
	   "expected identifier as enumeration type name" stx ?name))
       (unless (for-all identifier? ?id*)
	 (syntax-violation __who__
	   "expected list of symbols as enumeration elements" stx ?id*))
       (unless (identifier? ?maker)
	 (syntax-violation __who__
	   "expected identifier as enumeration constructor syntax name" stx ?maker))
       (let ((symbol*		(remove-dups (syntax->datum ?id*)))
	     (the-constructor	(gensym)))
	 (bless
	  `(begin
	     (define ,the-constructor
	       (enum-set-constructor (make-enumeration ',symbol*)))

	     (define-syntax ,?name
	       ;;Check at macro-expansion time whether the symbol ?ARG
	       ;;is in  the universe associated with ?NAME.   If it is,
	       ;;the result  of the  expansion is equivalent  to ?ARG.
	       ;;It is a syntax violation if it is not.
	       ;;
	       (lambda (x)
		 (define universe-of-symbols ',symbol*)
		 (define (%synner message subform)
		   (syntax-violation ',?name message
				     (syntax->datum x) (syntax->datum subform)))
		 (syntax-case x ()
		   ((_ ?arg)
		    (not (identifier? (syntax ?arg)))
		    (%synner "expected symbol as argument to enumeration validator"
			     (syntax ?arg)))

		   ((_ ?arg)
		    (not (memq (syntax->datum (syntax ?arg)) universe-of-symbols))
		    (%synner "expected symbol in enumeration as argument to enumeration validator"
			     (syntax ?arg)))

		   ((_ ?arg)
		    (syntax (quote ?arg)))

		   (_
		    (%synner "invalid enumeration validator form" #f)))))

	     (define-syntax ,?maker
	       ;;Given  any  finite sequence  of  the  symbols in  the
	       ;;universe, possibly  with duplicates, expands  into an
	       ;;expression that  evaluates to the  enumeration set of
	       ;;those symbols.
	       ;;
	       ;;Check  at  macro-expansion  time  whether  every  input
	       ;;symbol is in the universe  associated with ?NAME; it is
	       ;;a syntax violation if one or more is not.
	       ;;
	       (lambda (x)
		 (define universe-of-symbols ',symbol*)
		 (define (%synner message subform-stx)
		   (syntax-violation ',?maker
		     message
		     (syntax->datum x) (syntax->datum subform-stx)))
		 (syntax-case x ()
		   ((_ . ?list-of-symbols)
		    ;;Check the input  symbols one by one partitioning
		    ;;the ones in the universe from the one not in the
		    ;;universe.
		    ;;
		    ;;If  an input element  is not  a symbol:  raise a
		    ;;syntax violation.
		    ;;
		    ;;After   all   the   input  symbols   have   been
		    ;;partitioned,  if the  list of  collected INvalid
		    ;;ones is not null:  raise a syntax violation with
		    ;;that list as  subform, else return syntax object
		    ;;expression   building  a  new   enumeration  set
		    ;;holding the list of valid symbols.
		    ;;
		    (let loop ((valid-symbols-stx	'())
			       (invalid-symbols-stx	'())
			       (input-symbols-stx	(syntax ?list-of-symbols)))
		      (syntax-case input-symbols-stx ()

			;;No more symbols to collect and non-null list
			;;of collected INvalid symbols.
			(()
			 (not (null? invalid-symbols-stx))
			 (%synner "expected symbols in enumeration as arguments \
                                     to enumeration constructor syntax"
				  (reverse invalid-symbols-stx)))

			;;No more symbols to  collect and null list of
			;;collected INvalid symbols.
			(()
			 (quasisyntax
			  (,the-constructor '(unsyntax (reverse valid-symbols-stx)))))

			;;Error if element is not a symbol.
			((?symbol0 . ?rest)
			 (not (identifier? (syntax ?symbol0)))
			 (%synner "expected symbols as arguments to enumeration constructor syntax"
				  (syntax ?symbol0)))

			;;Collect a symbol in the set.
			((?symbol0 . ?rest)
			 (memq (syntax->datum (syntax ?symbol0)) universe-of-symbols)
			 (loop (cons (syntax ?symbol0) valid-symbols-stx)
			       invalid-symbols-stx (syntax ?rest)))

			;;Collect a symbol not in the set.
			((?symbol0 . ?rest)
			 (loop valid-symbols-stx
			       (cons (syntax ?symbol0) invalid-symbols-stx)
			       (syntax ?rest)))

			))))))
	     )))))
    ))


;;;; module non-core-macro-transformer: DO

(define (do-macro expr-stx)
  ;;Transformer  function  used  to  expand  R6RS  DO  macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (define (%normalise-binding binding-stx)
    (syntax-match binding-stx ()
      ((?var ?init)
       (receive (id tag)
	   (parse-tagged-identifier-syntax ?var)
	 `(,?var ,?init ,id)))
      ((?var ?init ?step)
       `(,?var ,?init ,?step))
      (_
       (stx-error expr-stx "invalid binding"))))
  (syntax-match expr-stx ()
    ((_ (?binding* ...)
	(?test ?expr* ...)
	?command* ...)
     (syntax-match (map %normalise-binding ?binding*) ()
       (((?var* ?init* ?step*) ...)
	(receive (id* tag*)
	    (parse-list-of-tagged-bindings ?var* expr-stx)
	  (bless
	   `(letrec ((loop (lambda ,?var*
			     (if ,?test
				 ;;If ?EXPR* is  null: make sure there
				 ;;is  at  least   one  expression  in
				 ;;BEGIN.
				 (begin (if #f #f) . ,?expr*)
			       (begin
				 ,@?command*
				 (loop . ,?step*))))))
	      (loop . ,?init*)))))
       ))
    ))


;;;; module non-core-macro-transformer: WHILE, UNTIL, FOR

(define (while-macro expr-stx)
  ;;Transformer function used  to expand Vicare's WHILE  macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?test ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (begin ,@?body* (loop))
		   (escape))))))))
    ))

(define (until-macro expr-stx)
  ;;Transformer function used  to expand Vicare's UNTIL  macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?test ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (escape)
		   (begin ,@?body* (loop)))))))))
    ))

(define (for-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  FOR macros  from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?init ?test ?incr) ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     ,?init
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ,?test
		     (begin
		       ,@?body* ,?incr
		       (loop))
		   (escape))))))))
    ))


;;;; module non-core-macro-transformer: DEFINE-RETURNABLE, LAMBDA-RETURNABLE

(define (define-returnable-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  DEFINE-RETURNABLE
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ (?name . ?formals) ?body0 ?body* ...)
     (bless
      `(define (,?name . ,?formals)
	 (call/cc
	     (lambda (escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (escape . ?args)))))
		 ,?body0 . ,?body*))))))
    ))

(define (lambda-returnable-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  LAMBDA-RETURNABLE
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?body0 ?body* ...)
     (bless
      `(lambda ,?formals
	 (call/cc
	     (lambda (escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (escape . ?args)))))
		 ,?body0 . ,?body*))))))
    ))

(define (begin-returnable-macro expr-stx)
  ;;Transformer function used to expand Vicare's BEGIN-RETURNABLE macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?body0 ?body* ...)
     (bless
      `(call/cc
	   (lambda (escape)
	     (fluid-let-syntax ((return (syntax-rules ()
					  ((_ . ?args)
					   (escape . ?args)))))
	       ,?body0 . ,?body*)))))
    ))


;;;; module non-core-macro-transformer: TRY

(module (try-macro)
  (define-constant __who__ 'try)

  (define (try-macro expr-stx)
    ;;Transformer  function  used  to  expand Vicare's  TRY  ...   CATCH
    ;;...  FINALLY  macros  from  the top-level  built  in  environment.
    ;;Expand the contents of EXPR-STX;  return a syntax object that must
    ;;be further expanded.
    ;;
    (syntax-match expr-stx (catch finally)
      ;;Full syntax.
      ((_ ?body (catch ?var ?catch-clause0 ?catch-clause* ...) (finally ?finally-body0 ?finally-body* ...))
       (begin
	 (validate-variable expr-stx ?var)
	 (let ((GUARD-CLAUSE* (parse-multiple-catch-clauses expr-stx ?var (cons ?catch-clause0 ?catch-clause*))))
	   (bless
	    `(with-compensations
	       (push-compensation ,?finally-body0 . ,?finally-body*)
	       (guard (,?var . ,GUARD-CLAUSE*) ,?body))))))

      ;;Only catch, no finally.
      ((_ ?body (catch ?var ?catch-clause0 ?catch-clause* ...))
       (begin
	 (validate-variable expr-stx ?var)
	 (let ((GUARD-CLAUSE* (parse-multiple-catch-clauses expr-stx ?var (cons ?catch-clause0 ?catch-clause*))))
	   (bless
	    `(guard (,?var . ,GUARD-CLAUSE*) ,?body)))))))

  (define (parse-multiple-catch-clauses expr-stx var-id clauses-stx)
    (syntax-match clauses-stx (else)
      ;;Match when  there is no  ELSE clause.  Remember that  GUARD will
      ;;reraise the exception when there is no ELSE clause.
      (()
       '())
      ;;This branch  with the ELSE  clause must come first!!!   The ELSE
      ;;clause is valid only if it is the last.
      (((else ?else-body0 ?else-body ...))
       clauses-stx)
      ((((?tag0 ?tag* ...) ?tag-body0 ?tag-body* ...) . ?other-clauses)
       (all-identifiers? (cons ?tag0 ?tag*))
       (cons `((or (is-a? ,var-id ,?tag0)
		   . ,(map (lambda (tag-id)
			     `(is-a? ,var-id ,tag-id))
			?tag*))
	       ,?tag-body0 . ,?tag-body*)
	     (parse-multiple-catch-clauses expr-stx var-id ?other-clauses)))
      ((?clause . ?other-clauses)
       (syntax-violation __who__
	 "invalid catch clause in try syntax" expr-stx ?clause))))

  (define (validate-variable expr-stx var-id)
    (unless (identifier? var-id)
      (syntax-violation __who__
	"expected identifier as variable" expr-stx var-id)))

  #| end of module |# )


;;;; module non-core-macro-transformer: OR, AND

(define (or-macro expr-stx)
  ;;Transformer  function  used  to  expand  R6RS  OR  macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_) #f)

    ((_ ?expr ?expr* ...)
     (bless
      (let recur ((e ?expr) (e* ?expr*))
	(if (null? e*)
	    `(begin #f ,e)
	  `(let ((t ,e))
	     (if t
		 t
	       ,(recur (car e*) (cdr e*))))))))
    ))

(define (and-macro expr-stx)
  ;;Transformer  function  used  to  expand R6RS  AND  macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_) #t)

    ((_ ?expr ?expr* ...)
     (bless
      (let recur ((e ?expr) (e* ?expr*))
	(if (null? e*)
	    `(begin #f ,e)
	  `(if ,e
	       ,(recur (car e*) (cdr e*))
	     #f)))))
    ))


;;;; module non-core-macro-transformer: COND

(define (cond-macro expr-stx)
  ;;Transformer  function  used to  expand  R6RS  COND macros  from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?cls ?cls* ...)
     (bless
      (let recur ((cls ?cls) (cls* ?cls*))
	(if (null? cls*)
	    (syntax-match cls (else =>)
	      ((else ?expr ?expr* ...)
	       `(internal-body ,?expr . ,?expr*))

	      ((?test => ?proc)
	       `(let ((t ,?test))
		  (if t
		      (,?proc t)
		    (void))))

	      ((?expr)
	       `(or ,?expr (void)))

	      ((?test ?expr* ...)
	       `(if ,?test
		    (internal-body . ,?expr*)
		  (void)))

	      (_
	       (stx-error expr-stx "invalid last clause")))

	  (syntax-match cls (else =>)
	    ((else ?expr ?expr* ...)
	     (stx-error expr-stx "incorrect position of keyword else"))

	    ((?test => ?proc)
	     `(let ((t ,?test))
		(if t
		    (,?proc t)
		  ,(recur (car cls*) (cdr cls*)))))

	    ((?expr)
	     `(or ,?expr
		  ,(recur (car cls*) (cdr cls*))))

	    ((?test ?expr* ...)
	     `(if ,?test
		  (internal-body . ,?expr*)
		,(recur (car cls*) (cdr cls*))))

	    (_
	     (stx-error expr-stx "invalid last clause")))))))
    ))


;;;; module non-core-macro-transformer: QUASIQUOTE

(module (quasiquote-macro)
  ;;Transformer function used to expand  R6RS QUASIQUOTE macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;Some example expansions:
  ;;
  ;;   (quasiquote (1 2 (unquote (+ 3 4))))
  ;;   ==> (list '1 '2 (+ '3 '4))
  ;;
  ;;   (quasiquote (1 2 (unquote (+ 3 4))))
  ;;   ==> (vector '1 '2 (+ '3 '4))
  ;;
  ;;NOTE We  can test  QUASIQUOTE expansions by  evaluating at  the REPL
  ;;expressions like:
  ;;
  ;;   (expand-form-to-core-language
  ;;     '(quasiquote ?pattern)
  ;;     (interaction-environment))
  ;;
  (define (quasiquote-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?expr)
       (%quasi ?expr 0))
      ))

  (define (%quasi p nesting-level)
    ;;Process a list of items representing the items from a list.
    ;;
    (syntax-match p (unquote unquote-splicing quasiquote)
      ((unquote p)
       (if (zero? nesting-level)
	   p
	 (%quasicons (%keyword 'unquote)
		     (%quasi (list p) (sub1 nesting-level)))))

      (((unquote p ...) . q)
       (if (zero? nesting-level)
	   (%quasicons* p (%quasi q nesting-level))
	 (%quasicons (%quasicons (%keyword 'unquote)
				 (%quasi p (sub1 nesting-level)))
		     (%quasi q nesting-level))))

      (((unquote-splicing p ...) . q)
       (if (zero? nesting-level)
	   (%quasiappend p (%quasi q nesting-level))
	 (%quasicons (%quasicons (%keyword 'unquote-splicing)
				 (%quasi p (sub1 nesting-level)))
		     (%quasi q nesting-level))))

      ((quasiquote p)
       (%quasicons (%keyword 'quasiquote)
		   (%quasi (list p) (add1 nesting-level))))

      ((p . q)
       (%quasicons (%quasi p nesting-level)
		   (%quasi q nesting-level)))

      (#(x ...)
       (not (<stx>? x))
       (%quasivector (%vector-quasi x nesting-level)))

      (p
       (%application 'quote p))
      ))

  (define (%vector-quasi p nesting-level)
    ;;Process a list of items representing the items from a vector.
    ;;
    (syntax-match p ()
      ((p . q)
       (syntax-match p (unquote unquote-splicing)
  	 ((unquote p ...)
  	  (if (zero? nesting-level)
  	      (%quasicons* p (%vector-quasi q nesting-level))
  	    (%quasicons (%quasicons (%keyword 'unquote)
  				    (%quasi p (sub1 nesting-level)))
  			(%vector-quasi q nesting-level))))

  	 ((unquote-splicing p ...)
  	  (if (zero? nesting-level)
  	      (%quasiappend p (%vector-quasi q nesting-level))
  	    (%quasicons (%quasicons (%keyword 'unquote-splicing)
  				    (%quasi p (sub1 nesting-level)))
  			(%vector-quasi q nesting-level))))

  	 (p
  	  (%quasicons (%quasi p nesting-level)
  		      (%vector-quasi q nesting-level)))
  	 ))

      (()
       (%application 'quote '()))
      ))

  (define (%keyword key)
    ;;Return a  top-marked syntax  object representing a  quoted symbol;
    ;;the  symbol  being the  name  of  a  syntax  from the  boot  image
    ;;EXPORT-ENV.  Expanding  and evaluating the returned  syntax object
    ;;is equivalent to evaluating:
    ;;
    ;;   (quote key)
    ;;
    ;;where KEY is one of: quasiquote, unquote, unquote-splicing.
    ;;
    (list (scheme-stx 'quote) (mkstx key TOP-MARK* '() '())))

  (define-syntax %application
    ;;Expand to an expression which, when evaluated, results in a syntax
    ;;object representing an expression.   Such syntax object expression
    ;;is the application of ?CONSTRUCTOR to the, possibly empty, list of
    ;;arguments ?ARG*.
    ;;
    ;;?CONSTRUCTOR must be a symbol  representing the name of a function
    ;;or syntax  from the boot  image EXPORT-ENV; candidates  are: list,
    ;;vector, list->vector, cons, quote.
    ;;
    (syntax-rules (quote)
      ((_ (quote ?constructor) ?arg* ...)
       (list (scheme-stx '?constructor) ?arg* ...))
      ))

  (define-syntax %application*
    ;;Expand to an expression which, when evaluated, results in a syntax
    ;;object representing an expression.   Such syntax object expression
    ;;is the application of ?CONSTRUCTOR to the, possibly empty, list of
    ;;arguments ?ARG* and the list of arguments ?TAIL-ARG*.
    ;;
    ;;?CONSTRUCTOR must be a symbol  representing the name of a function
    ;;or syntax  from the boot  image EXPORT-ENV; candidates  are: list,
    ;;append, vector.
    ;;
    (syntax-rules (quote)
      ((_ (quote ?constructor) ?arg* ... ?tail-arg*)
       (cons* (scheme-stx '?constructor) ?arg* ... ?tail-arg*))))

  (define (%quasicons* x y)
    (let recur ((x x))
      (if (null? x)
	  y
	(%quasicons (car x) (recur (cdr x))))))

  (define (%quasicons x y)
    (syntax-match y (quote list)
      ((quote ?dy)
       (syntax-match x (quote)
	 ((quote ?dx)
	  (%application 'quote (cons ?dx ?dy)))

	 (_
	  (syntax-match ?dy ()
	    (()
	     (%application 'list x))
	    (_
	     (%application 'cons x y))
	    ))
	 ))

      ((list ?stuff ...)
       (%application* 'list x ?stuff))

      (_
       (%application 'cons x y))
      ))

  (define (%quasiappend x y)
    (let ((ls (let recur ((x x))
		(if (null? x)
		    (syntax-match y (quote)
		      ((quote ())
		       '())
		      (_
		       (list y)))
		  (syntax-match (car x) (quote)
		    ((quote ())
		     (recur (cdr x)))
		    (_
		     (cons (car x) (recur (cdr x)))))))))
      (cond ((null? ls)
	     (%application 'quote '()))
	    ((null? (cdr ls))
	     (car ls))
	    (else
	     (%application* 'append ls)))))

  (define (%quasivector x)
    (let ((pat-x x))
      (syntax-match pat-x (quote)
  	((quote (x* ...))
  	 (%application 'quote (list->vector x*)))

  	(_
  	 (let loop ((x x)
  		    (k (lambda (ls)
  			 (%application* 'vector ls))))
  	   (syntax-match x (list cons quote)
  	     ((quote (x* ...))
  	      (k (map (lambda (x) (%application 'quote x)) x*)))

  	     ((list x* ...)
  	      (k x*))

  	     ((cons x y)
  	      (loop y (lambda (ls)
  			(k (cons x ls)))))

  	     (_
  	      (%application 'list->vector pat-x))
  	     )))
  	)))

  #| end of module: QUASIQUOTE-MACRO |# )


;;;; module non-core-macro-transformer: QUASISYNTAX

(module (quasisyntax-macro)
  ;;Transformer function used to expand R6RS QUASISYNTAX macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  ;;FIXME: not really correct (Abdulaziz Ghuloum).
  ;;
  (define (quasisyntax-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ e)
       (receive (lhs* rhs* v)
	   (quasi e 0)
	 (bless
	  `(syntax-case (list ,@rhs*) ()
	     (,lhs*
	      (syntax ,v))))))
      ))

  (define (quasi p nesting-level)
    (syntax-match p (unsyntax unsyntax-splicing quasisyntax)
      ((unsyntax p)
       (if (zero? nesting-level)
	   (let ((g (gensym)))
	     (values (list g) (list p) g))
	 (receive (lhs* rhs* p)
	     (quasi p (sub1 nesting-level))
	   (values lhs* rhs* (list 'unsyntax p)))))

      (unsyntax
       (zero? nesting-level)
       (stx-error p "incorrect use of unsyntax"))

      (((unsyntax p* ...) . q)
       (receive (lhs* rhs* q)
	   (quasi q nesting-level)
	 (if (zero? nesting-level)
	     (let ((g* (map (lambda (x) (gensym)) p*)))
	       (values (append g* lhs*)
		       (append p* rhs*)
		       (append g* q)))
	   (receive (lhs2* rhs2* p*)
	       (quasi p* (sub1 nesting-level))
	     (values (append lhs2* lhs*)
		     (append rhs2* rhs*)
		     `((unsyntax . ,p*) . ,q))))))

      (((unsyntax-splicing p* ...) . q)
       (receive (lhs* rhs* q)
	   (quasi q nesting-level)
	 (if (zero? nesting-level)
	     (let ((g* (map (lambda (x) (gensym)) p*)))
	       (values (append (map (lambda (g) `(,g ...)) g*)
			       lhs*)
		       (append p* rhs*)
		       (append (apply append
				      (map (lambda (g) `(,g ...)) g*))
			       q)))
	   (receive (lhs2* rhs2* p*)
	       (quasi p* (sub1 nesting-level))
	     (values (append lhs2* lhs*)
		     (append rhs2* rhs*)
		     `((unsyntax-splicing . ,p*) . ,q))))))

      (unsyntax-splicing
       (zero? nesting-level)
       (stx-error p "incorrect use of unsyntax-splicing"))

      ((quasisyntax p)
       (receive (lhs* rhs* p)
	   (quasi p (add1 nesting-level))
	 (values lhs* rhs* `(quasisyntax ,p))))

      ((p . q)
       (let-values
	   (((lhs*  rhs*  p) (quasi p nesting-level))
	    ((lhs2* rhs2* q) (quasi q nesting-level)))
	 (values (append lhs2* lhs*)
		 (append rhs2* rhs*)
		 (cons p q))))

      (#(x* ...)
       (receive (lhs* rhs* x*)
	   (quasi x* nesting-level)
	 (values lhs* rhs* (list->vector x*))))

      (_
       (values '() '() p))
      ))

  #| end of module |# )


;;;; module non-core-macro-transformer: DEFINE-VALUES, DEFINE-CONSTANT-VALUES

(define (define-values-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  DEFINE-VALUES macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?form0 ?form* ...)
     (receive (standard-formals signature)
	 (parse-tagged-formals-syntax ?formals expr-stx)
       (syntax-match standard-formals ()
	 ((?id* ... ?id0)
	  (let ((TMP* (generate-temporaries ?id*)))
	    (receive (tag* tag0)
		(proper-list->head-and-last (formals-signature-tags signature))
	      (bless
	       `(begin
		  ,@(map (lambda (var tag)
			   `(define (brace ,var ,tag)))
		      ?id* tag*)
		  (define (brace ,?id0 ,tag0)
		    (call-with-values
			(lambda () ,?form0 . ,?form*)
		      (lambda (,@TMP* T0)
			,@(map (lambda (var TMP)
				 `(set! ,var ,TMP))
			    ?id* TMP*)
			T0))))))))

	 (?args
	  (identifier? ?args)
	  (bless
	   `(define (brace ,?args ,(formals-signature-tags signature))
	      (call-with-values
		  (lambda () ,?form0 . ,?form*)
		(lambda args args)))))

	 ((?id* ... . ?rest-id)
	  (let ((TMP* (generate-temporaries ?id*)))
	    (receive (tag* rest-tag)
		(improper-list->list-and-rest (formals-signature-tags signature))
	    (bless
	     `(begin
		,@(map (lambda (var tag)
			 `(define (brace ,var ,tag)))
		    ?id* tag*)
		(define (brace ,?rest-id ,rest-tag)
		  (call-with-values
		      (lambda () ,?form0 . ,?form*)
		    (lambda (,@TMP* . rest)
		      ,@(map (lambda (var TMP)
			       `(set! ,var ,TMP))
			  ?id* TMP*)
		      rest))))))))
	 )))
    ))

(define (define-constant-values-macro expr-stx)
  ;;Transformer function used  to expand Vicare's DEFINE-CONSTANT-VALUES
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?form0 ?form* ...)
     (receive (standard-formals signature)
	 (parse-tagged-formals-syntax ?formals expr-stx)
       (syntax-match standard-formals ()
	 ((?id* ... ?id0)
	  (let ((SHADOW* (generate-temporaries ?id*))
		(TMP*    (generate-temporaries ?id*)))
	    (receive (tag* tag0)
		(proper-list->head-and-last (formals-signature-tags signature))
	      (bless
	       `(begin
		  ,@(map (lambda (var tag)
			   `(define (brace ,var ,tag)))
		      SHADOW* tag*)
		  (define (brace SHADOW0 ,tag0)
		    (call-with-values
			(lambda () ,?form0 . ,?form*)
		      (lambda (,@TMP* T0)
			,@(map (lambda (var TMP)
				 `(set! ,var ,TMP))
			    SHADOW* TMP*)
			T0)))
		  ,@(map (lambda (var SHADOW)
			   `(define-syntax ,var
			      (identifier-syntax ,SHADOW)))
		      ?id* SHADOW*)
		  (define-syntax ,?id0
		    (identifier-syntax SHADOW0))
		  )))))

	 (?args
	  (identifier? ?args)
	  (let ((args-tag (formals-signature-tags signature)))
	    (bless
	     `(begin
		(define (brace shadow ,args-tag)
		  (call-with-values
		      (lambda () ,?form0 . ,?form*)
		    (lambda args args)))
		(define-syntax ,?args
		  (identifier-syntax shadow))
		))))

	 ((?id* ... . ?rest-id)
	  (let ((SHADOW* (generate-temporaries ?id*))
		(TMP*    (generate-temporaries ?id*)))
	    (receive (tag* rest-tag)
		(improper-list->list-and-rest (formals-signature-tags signature))
	    (bless
	     `(begin
		,@(map (lambda (var tag)
			 `(define (brace ,var ,tag)))
		    SHADOW* tag*)
		(define (brace rest-shadow ,rest-tag)
		  (call-with-values
		      (lambda () ,?form0 . ,?form*)
		    (lambda (,@TMP* . rest)
		      ,@(map (lambda (var TMP)
			       `(set! ,var ,TMP))
			  SHADOW* TMP*)
		      rest)))
		,@(map (lambda (var SHADOW)
			 `(define-syntax ,var
			    (identifier-syntax ,SHADOW)))
		    ?id* SHADOW*)
		(define-syntax ,?rest-id
		  (identifier-syntax rest-shadow))
		)))))
	 )))
    ))


;;;; module non-core-macro-transformer: RECEIVE, RECEIVE-AND-RETURN, BEGIN0, XOR

(define (receive-macro expr-stx)
  ;;Transformer function used to expand Vicare's RECEIVE macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?producer-expression ?form0 ?form* ...)
     (tagged-formals-syntax? ?formals)
     (bless
      `(call-with-values
	   (lambda () ,?producer-expression)
	 (lambda ,?formals ,?form0 ,@?form*))))
    ))

(define (receive-and-return-macro expr-stx)
  ;;Transformer  function  used  to expand  Vicare's  RECEIVE-AND-RETURN
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?formals ?producer-expression ?body0 ?body* ...)
     (receive (standard-formals signature)
	 (parse-tagged-formals-syntax ?formals expr-stx)
       (let ((rv-form (cond ((list? standard-formals)
			     `(values . ,standard-formals))
			    ((pair? standard-formals)
			     (receive (rv* rv-rest)
				 (improper-list->list-and-rest standard-formals)
			       `(values ,@rv* ,rv-rest)))
			    (else
			     ;;It's a standalone identifier.
			     standard-formals))))
	 (bless
	  `(call-with-values
	       (lambda () ,?producer-expression)
	     (lambda ,?formals
	       ,?body0 ,@?body*
	       ,rv-form))))))
    ))

(define (begin0-macro expr-stx)
  ;;Transformer function used to expand  Vicare's BEGIN0 macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?form0 ?form* ...)
     (bless
      `(call-with-values
	   (lambda () ,?form0)
	 (lambda args
	   ,@?form*
	   (apply values args)))))
    ))

(module (xor-macro)
  ;;Transformer function  used to  expand Vicare's  XOR macros  from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (define (xor-macro expr-stx)
    (syntax-match expr-stx ()
      ((_ ?expr* ...)
       (bless (%xor-aux #f ?expr*)))
      ))

  (define (%xor-aux bool/var expr*)
    (cond ((null? expr*)
	   bool/var)
	  ((null? (cdr expr*))
	   `(let ((x ,(car expr*)))
	      (if ,bool/var
		  (and (not x) ,bool/var)
		x)))
	  (else
	   `(let ((x ,(car expr*)))
	      (and (or (not ,bool/var)
		       (not x))
		   (let ((n (or ,bool/var x)))
		     ,(%xor-aux 'n (cdr expr*))))))))

  #| end of module: XOR-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-INLINE, DEFINE-CONSTANT

(define (define-constant-macro expr-stx)
  ;;Transformer function used to  expand Vicare's DEFINE-CONSTANT macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx (brace)
    ((_ (brace ?name ?tag) ?expr)
     (and (identifier? ?name)
	  (tag-identifier? ?tag))
     (bless
      `(begin
	 (define (brace ghost ?tag) ,?expr)
	 (define-syntax ,?name
	   (identifier-syntax ghost)))))
    ((_ ?name ?expr)
     (identifier? ?name)
     (bless
      `(begin
	 (define ghost ,?expr)
	 (define-syntax ,?name
	   (identifier-syntax ghost)))))
    ))

(define (define-inline-constant-macro expr-stx)
  ;;Transformer function used  to expand Vicare's DEFINE-INLINE-CONSTANT
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;We want to allow a generic expression to generate the constant value
  ;;at expand time.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name ?expr)
     (bless
      `(define-syntax ,?name
	 (let ((const ,?expr))
	   (lambda (stx)
	     (syntax-case stx ()
	       (?id
		(identifier? #'?id)
		#`(quote #,const))))))))
    ))

(define (define-inline-macro expr-stx)
  ;;Transformer function  used to  expand Vicare's  DEFINE-INLINE macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (define (%output name-id arg-stx* rest-stx body-stx)
    (let ((TMP* (generate-temporaries arg-stx*))
	  (REST (gensym)))
      (bless
       `(define-fluid-syntax ,name-id
	  (syntax-rules ()
	    ((_ ,@TMP* . REST)
	     (fluid-let-syntax
		 ((,name-id (lambda (stx)
			      (syntax-violation (quote ,name-id)
				"cannot recursively expand inline expression"
				stx))))
	       (let ,(append (map list arg-stx* TMP*)
			     (let ((rest.datum (syntax->datum rest-stx)))
			       (cond ((null? rest.datum)
				      '())
				     ((symbol? rest.datum)
				      ;;If the  rest argument is untagged,  we tag it
				      ;;by default with "<list>".
				      `(((brace ,rest-stx <list>) (list . REST))))
				     (else
				      `((,rest-stx (list . REST)))))))
		 . ,body-stx))))))))
  (syntax-match expr-stx (brace)
    ((_ (?name ?arg* ... . (brace ?rest ?rest-tag)) ?form0 ?form* ...)
     (and (identifier? ?name)
	  (tagged-lambda-proto-syntax? (append ?arg* (bless `(brace ,?rest ,?rest-tag)))))
     (%output ?name ?arg* (bless `(brace ,?rest ,?rest-tag)) (cons ?form0 ?form*)))
    ((_ (?name ?arg* ... . ?rest) ?form0 ?form* ...)
     (and (identifier? ?name)
	  (tagged-lambda-proto-syntax? (append ?arg* ?rest)))
     (%output ?name ?arg* ?rest (cons ?form0 ?form*)))
    ))


;;;; module non-core-macro-transformer: INCLUDE

(module (include-macro)
  ;;Transformer function used to expand Vicare's INCLUDE macros from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (define (include-macro expr-stx)
    (define (%synner message subform)
      (syntax-violation 'include message expr-stx subform))
    (syntax-match expr-stx ()
      ((?context ?filename)
       (%include-file ?filename ?context #f %synner))
      ((?context ?filename #t)
       (%include-file ?filename ?context #t %synner))
      ))

  (define (%include-file filename-stx context-id verbose? synner)
    (define filename.str
      (syntax->datum filename-stx))
    (unless (string? filename.str)
      (stx-error filename-stx "expected string as include file pathname"))
    (receive (pathname contents)
	;;FIXME Why in  fuck I cannot use the  parameter here?!?  (Marco
	;;Maggi; Tue Feb 11, 2014)
	(default-include-loader #;(current-include-loader)
	  filename.str verbose? synner)
      ;;We expect CONTENTS to be null or a list of annotated datums.
      (bless
       `(stale-when (internal-body
		      (import (only (vicare language-extensions posix)
				    file-modification-time))
		      (or (not (file-exists? ,pathname))
			  (> (file-modification-time ,pathname)
			     ,(file-modification-time pathname))))
	  . ,(map (lambda (item)
		    (datum->syntax context-id item))
	       contents)))))

  #| end of module: INCLUDE-MACRO |# )


;;;; module non-core-macro-transformer: DEFINE-INTEGRABLE

(define (define-integrable-macro expr-stx)
  ;;Transformer  function  used  to  expand  Vicare's  DEFINE-INTEGRABLE
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  ;;The original  syntax was  posted by "leppie"  on the  Ikarus mailing
  ;;list; subject "Macro Challenge of Last Year [Difficulty: *****]", 20
  ;;Oct 2009.
  ;;
  (syntax-match expr-stx (lambda)
    ((_ (?name . ?formals) ?form0 ?form* ...)
     (identifier? ?name)
     (bless
      `(define-integrable ,?name (lambda ,?formals ,?form0 ,@?form*))))

    ((_ ?name (lambda ?formals ?form0 ?form* ...))
     (identifier? ?name)
     (bless
      `(begin
	 (define-fluid-syntax ,?name
	   (lambda (x)
	     (syntax-case x ()
	       (_
		(identifier? x)
		#'xname)

	       ((_ arg ...)
		#'((fluid-let-syntax
		       ((,?name (identifier-syntax xname)))
		     (lambda ,?formals ,?form0 ,@?form*))
		   arg ...)))))
	 (define xname
	   (fluid-let-syntax ((,?name (identifier-syntax xname)))
	     (lambda ,?formals ,?form0 ,@?form*)))
	 )))
    ))


;;;; module non-core-macro-transformer: DEFINE-SYNTAX-PARAMETER, SYNTAX-PARAMETRISE

(define (define-syntax-parameter-macro expr-stx)
  ;;Transformer function used to expand Vicare's DEFINE-SYNTAX-PARAMETER
  ;;macros from the top-level built in environment.  Expand the contents
  ;;of EXPR-STX; return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?param-id ?param-expr)
     (identifier? ?param-id)
     (bless
      `(define-fluid-syntax ,?param-id
	 (make-compile-time-value ,?param-expr))))
    ))

(define (syntax-parametrise-macro expr-stx)
  ;;Transformer      function      used     to      expand      Vicare's
  ;;SYNTAX-PARAMETRISE-MACRO   macros  from   the  top-level   built  in
  ;;environment.   Expand  the contents  of  EXPR-STX;  return a  syntax
  ;;object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ((?lhs* ?rhs*) ...) ?body0 ?body* ...)
     (for-all identifier? ?lhs*)
     (bless
      `(fluid-let-syntax ,(map (lambda (lhs rhs)
				 (list lhs `(make-compile-time-value ,rhs)))
			    ?lhs* ?rhs*)
	 ,?body0 . ,?body*)))
    ))


;;;; module non-core-macro-transformer: miscellanea

(define (time-macro expr-stx)
  ;;Transformer function  used to expand  Vicare's TIME macros  from the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?expr)
     (let ((str (receive (port getter)
		    (open-string-output-port)
		  (write (syntax->datum ?expr) port)
		  (getter))))
       (bless
	`(time-it ,str (lambda () ,?expr)))))))

(define (delay-macro expr-stx)
  ;;Transformer  function used  to  expand R6RS  DELAY  macros from  the
  ;;top-level built  in environment.   Expand the contents  of EXPR-STX;
  ;;return a syntax object that must be further expanded.
  ;;
  (syntax-match expr-stx ()
    ((_ ?expr)
     (bless
      `(make-promise (lambda ()
		       ,?expr))))))

(define (assert-macro expr-stx)
  ;;Defined by R6RS.  An ASSERT  form is evaluated by evaluating EXPR.
  ;;If  EXPR returns a  true value,  that value  is returned  from the
  ;;ASSERT  expression.   If EXPR  returns  false,  an exception  with
  ;;condition  types  "&assertion"  and  "&message"  is  raised.   The
  ;;message  provided  in   the  condition  object  is  implementation
  ;;dependent.
  ;;
  ;;NOTE  Implementations should  exploit the  fact that  ASSERT  is a
  ;;syntax  to  provide as  much  information  as  possible about  the
  ;;location of the assertion failure.
  ;;
  (syntax-match expr-stx ()
    ((_ ?expr)
     (let ((pos (or (expression-position expr-stx)
		    (expression-position ?expr))))
       (bless
	(if (source-position-condition? pos)
	    `(or ,?expr
		 (assertion-error
		  ',?expr ,(source-position-port-id pos)
		  ,(source-position-byte pos) ,(source-position-character pos)
		  ,(source-position-line pos) ,(source-position-column    pos)))
	  `(or ,?expr
	       (assertion-error ',?expr "unknown source" #f #f #f #f))))))))

(define (file-options-macro expr-stx)
  ;;Transformer for  the FILE-OPTIONS macro.  File  options selection is
  ;;implemented   as   an   enumeration  type   whose   constructor   is
  ;;MAKE-FILE-OPTIONS from the boot environment.
  ;;
  (define (valid-option? opt-stx)
    (and (identifier? opt-stx)
	 (memq (identifier->symbol opt-stx) '(no-fail no-create no-truncate))))
  (syntax-match expr-stx ()
    ((_ ?opt* ...)
     (for-all valid-option? ?opt*)
     (bless
      `(make-file-options ',?opt*)))))

(define (endianness-macro expr-stx)
  ;;Transformer of  ENDIANNESS.  Support  the symbols:  "big", "little",
  ;;"network", "native"; convert "network" to "big".
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (and (identifier? ?name)
	  (memq (identifier->symbol ?name) '(big little network native)))
     (case (identifier->symbol ?name)
       ((network)
	(bless '(quote big)))
       ((native)
	(bless '(native-endianness)))
       ((big little)
	(bless `(quote ,?name)))))))

(define (%allowed-symbol-macro expr-stx allowed-symbol-set)
  ;;Helper  function used  to  implement the  transformer of:  EOL-STYLE
  ;;ERROR-HANDLING-MODE, BUFFER-MODE,  ENDIANNESS.  All of  these macros
  ;;should expand to a quoted symbol among a list of allowed ones.
  ;;
  (syntax-match expr-stx ()
    ((_ ?name)
     (and (identifier? ?name)
	  (memq (identifier->symbol ?name) allowed-symbol-set))
     (bless
      `(quote ,?name)))))


;;;; done

;;; end of file
;;Local Variables:
;;mode: vicare
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
;;eval: (put 'case-object-type-binding		'scheme-indent-function 1)
;;eval: (put 'if-wants-descriptive-gensyms	'scheme-indent-function 1)
;;eval: (put 'push-lexical-contour		'scheme-indent-function 1)
;;eval: (put 'set-interaction-env-lab.loc/lex*!	'scheme-indent-function 1)
;;eval: (put 'syntactic-binding-getprop		'scheme-indent-function 1)
;;eval: (put 'sys.syntax-case			'scheme-indent-function 2)
;;End:
