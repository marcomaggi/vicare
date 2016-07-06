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


(module ()
  ;;Transformer function  used to  expand R6RS's  DEFINE-RECORD-TYPE macros  from the
  ;;top-level built in environment.  Expand  the contents of INPUT-FORM.STX; return a
  ;;syntax object that must be further expanded.
  ;;
  (define-constant __module_who__ 'define-record-type)

  (define-macro-transformer (define-record-type input-form.stx)
    (syntax-match input-form.stx ()
      ((_ ?namespec ?clause* ...)
       (with-exception-handler
	   (lambda (E)
	     (raise
	      (condition E
			 (if (who-condition? E)
			     (condition)
			   (make-who-condition __who__))
			 (if (syntax-violation? E)
			     (condition)
			   (make-syntax-violation input-form.stx #f)))))
	 (lambda ()
	   (%validate-definition-clauses ?clause* __synner__)
	   ;; (receive-and-return (out)
	   ;;     (%do-define-record input-form.stx ?namespec ?clause* __synner__)
	   ;;   (debug-print out))
	   (%do-define-record input-form.stx ?namespec ?clause* __synner__))))
      (_
       (__synner__ "invalid syntax in macro use"))))


;;;; helpers

(define (%named-gensym/suffix foo suffix)
  (make-syntactic-identifier-for-temporary-variable (string-append (symbol->string (syntax->datum foo)) suffix)))

(define (%named-gensym/prefix foo prefix)
  (make-syntactic-identifier-for-temporary-variable (string-append prefix (symbol->string (syntax->datum foo)))))

(define (%filter-out-falses ls)
  (if (pair? ls)
      (let ((A (car ls))
	    (D (cdr ls)))
	(if A
	    (cons A (%filter-out-falses D))
	  (%filter-out-falses D)))
    '()))

(define (%build-foo-for-id-generation foo strip-angular-parentheses?)
  ;;It works like this:
  ;;
  ;;  (%build-foo-for-id-generation #'ciao #t)		=> #'ciao
  ;;  (%build-foo-for-id-generation #'ciao #f)		=> #'ciao
  ;;  (%build-foo-for-id-generation #'<ciao> #t)	=> #'ciao
  ;;  (%build-foo-for-id-generation #'<ciao> #f)	=> #'<ciao>
  ;;
  (if strip-angular-parentheses?
      (let* ((str     (symbol->string (identifier->symbol foo)))
	     (str.len (string-length str)))
	(if (and (fx>=? str.len 3)
		 (char=? #\< (string-ref str 0))
		 (char=? #\> (string-ref str (fxsub1 str.len))))
	    (datum->syntax foo (string->symbol (substring str 1 (fxsub1 str.len))))
	  str))
    foo))


(define (%do-define-record input-form.stx namespec input-clause*.stx synner)
  (define clause*.stx
    (let ((foo (syntax-match namespec ()
		 ((?foo ?make-foo ?foo?)
		  (identifier? ?foo)
		  ?foo)
		 (?foo
		  (identifier? ?foo)
		  ?foo)
		 (_
		  (synner "invalid record-type name specification" namespec)))))
      (%introduce-mixin-clauses foo input-clause*.stx synner)))
  (define strip-angular-parentheses?
    (%get-strip-angular-parentheses clause*.stx synner))
  (define-values (foo make-foo foo? foo-for-id-generation)
    (%parse-full-name-spec namespec strip-angular-parentheses? synner))
  (define define-type-descriptors?
    (%get-define-type-descriptors clause*.stx synner))
  (define foo-rtd (identifier-append foo foo-for-id-generation "-rtd"))
  (define foo-rcd (identifier-append foo foo-for-id-generation "-rcd"))
  (define-values
    (field-name*.sym
		;A list of symbols representing all the field names.
     field-relative-idx*
		;A  list  of fixnums  representing  all  the field  relative  indexes
		;(zero-based).
     field-type*.ann
		;The  list of  syntax objects  representing the  type annotations  of
		;fields.
     safe-field-accessor*
		;A  list of  syntactic identifiers  that will  be bound  to the  safe
		;accessors.
     unsafe-field-accessor*
		;A list  of syntactic identifiers  that will  be bound to  the unsafe
		;accessors.
     safe-field-mutator*
		;A  list of  syntactic identifiers  that will  be bound  to the  safe
		;mutators.  This list  holds #f in the position  of immutable fields.
		;This list has one item for each item in SAFE-FIELD-ACCESSOR*.
     unsafe-field-mutator*
		;A list  of syntactic identifiers  that will  be bound to  the unsafe
		;mutators.  This list  holds #f in the position  of immutable fields.
		;This list has one item for each item in UNSAFE-FIELD-ACCESSOR*.
     field-method*
		;A list of syntactic identifiers that will be bound to the safe field
		;accessor/mutator methods.
     fields-vector-spec
		;A  vector to  be  used as  FIELDS argument  for  the core  primitive
		;function MAKE-RECORD-TYPE-DESCRIPTOR.
     )
    (%parse-field-specs foo foo-for-id-generation clause*.stx synner))

  ;;Code  for  parent  record-type  descriptor  and  parent  record-type  constructor
  ;;descriptor retrieval.
  ;;
  ;;FOO-PARENT.ID: an identifier  representing the parent type, or false  if there is
  ;;no parent or the parent is specified through the procedural layer.
  ;;
  ;;PARENT-RTD.ID: false if this record-type has  no parent; otherwise it is a symbol
  ;;representing  the  name  of  the  syntactic  identifier  bound  to  the  parent's
  ;;record-type descriptor.
  ;;
  ;;PARENT-RCD.ID: false if this record-type has  no parent; otherwise it is a symbol
  ;;representing  the  name  of  the  syntactic  identifier  bound  to  the  parent's
  ;;record-constructor descriptor.
  ;;
  ;;PARENT-RTD-DEFINITION: null or  a list holding a DEFINE form  defining the parent
  ;;RTD syntactic binding, the list is spliced in the output.
  ;;
  ;;PARENT-RCD-DEFINITION: null or  a list holding a DEFINE form  defining the parent
  ;;RCD syntactic binding, the list is spliced in the output.
  (define-values (foo-parent.id parent-rtd.id parent-rcd.id parent-rtd-definition parent-rcd-definition)
    (%make-parent-rtd+rcd-code clause*.stx foo input-form.stx synner))

  ;;FOO-UID  is a  syntactic identifier  representing the  record-type UID;  it is  a
  ;;symbol even  when the  record-type is  generative.  GENERATIVE?   can be  true or
  ;;false.
  (define-values (foo-uid generative?)
    (%get-uid foo clause*.stx synner))

  ;;Code for default record-constructor descriptor.
  (define foo-rcd-definitions
    (%make-rcd-definitions clause*.stx foo foo-rtd foo-rcd parent-rcd.id synner))

  (define constructor-signature.stx
    (%get-constructor-signature clause*.stx synner))

  ;;Code for record maker function.
  (define foo-maker-definitions
    (%make-maker-definitions foo foo-rcd make-foo constructor-signature.stx synner))

  ;;Code for predicate and optional custom  predicate.  False or a form evaluating to
  ;;the predicate definitions.
  (define foo-predicate-definitions
    (%make-predicate-definitions clause*.stx foo? foo-rtd synner))

  ;;This definition is null if there is  no constructor protocol to be used when this
  ;;type is the super-type  of another; otherwise it is a list  holding a DEFINE form
  ;;defining the supertype record-constructor  descriptor syntactic binding, the list
  ;;is spliced in the output.
  (define-values (foo-super-rcd.id super-rcd-definition)
    (cond ((%make-super-rcd-code clause*.stx foo foo-rtd foo-parent.id parent-rcd.id synner)
	   => (lambda (foo-super-rcd-code)
		(let ((foo-super-rcd.id (%named-gensym/suffix foo "-super-protocol")))
		  (values foo-super-rcd.id `((define/typed {,foo-super-rcd.id <record-constructor-descriptor>}
					       ,foo-super-rcd-code))))))
	  (else
	   (values #f '()))))

  ;;This definition is null if there is no destructor; otherwise it is a list holding
  ;;a DEFINE  form defining the default  destructor function, the list  is spliced in
  ;;the output.
  (define-values (foo-destructor.id foo-destructor-definition)
    (cond ((%make-destructor-code clause*.stx foo foo-parent.id parent-rtd.id synner)
	   => (lambda (foo-destructor-code)
		(let ((foo-destructor.id (%named-gensym/suffix foo "-destructor")))
		  (values foo-destructor.id `((define ,foo-destructor.id ,foo-destructor-code))))))
	  (else
	   (values #f '()))))

  ;;This definition is null if there is no equality predicate; otherwise it is a list
  ;;holding a DEFINE form defining the equality predicate, the list is spliced in the
  ;;output.
  (define-values (foo-equality-predicate.id foo-equality-predicate-definition)
    (cond ((%make-equality-predicate-code clause*.stx foo parent-rtd.id synner)
	   => (lambda (foo-equality-predicate-code)
		(let ((foo-equality-predicate.id (%named-gensym/suffix foo-for-id-generation "-equality-predicate")))
		  (values foo-equality-predicate.id
			  `((define/typed {,foo-equality-predicate.id (equality-predicate ,foo)}
			      ,foo-equality-predicate-code))))))
	  (else
	   (values #f '()))))

  ;;This definition is  null if there is  no comparison procedure; otherwise  it is a
  ;;list holding a DEFINE form defining the comparison procedure, the list is spliced
  ;;in the output.
  (define-values (foo-comparison-procedure.id foo-comparison-procedure-definition)
    (cond ((%make-comparison-procedure-code clause*.stx foo parent-rtd.id synner)
	   => (lambda (foo-comparison-procedure-code)
		(let ((foo-comparison-procedure.id (%named-gensym/suffix foo-for-id-generation "-comparison-procedure")))
		  (values foo-comparison-procedure.id
			  `((define/typed {,foo-comparison-procedure.id (comparison-procedure ,foo)}
			      ,foo-comparison-procedure-code))))))
	  (else
	   (values #f '()))))

  ;;This definition  is null if  there is  no hash function;  otherwise it is  a list
  ;;holding a  DEFINE form  defining the hash  function, the list  is spliced  in the
  ;;output.
  (define-values (foo-hash-function.id foo-hash-function-definition)
    (cond ((%make-hash-function-code clause*.stx foo parent-rtd.id synner)
	   => (lambda (foo-hash-function-code)
		(let ((foo-hash-function.id (%named-gensym/suffix foo-for-id-generation "-hash-function")))
		  (values foo-hash-function.id `((define/typed {,foo-hash-function.id (hash-function ,foo)}
						   ,foo-hash-function-code))))))
	  (else
	   (values #f '()))))

  ;;Code for methods.
  (define-values (method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist)
    (%parse-method-clauses clause*.stx foo foo-for-id-generation synner))

  ;;False  or a  symbolic  expression (to  be BLESSed  later)  representing a  Scheme
  ;;expression returning a closure object: the methods-retriever function.
  (define methods-retriever-code.sexp
    (%make-methods-retriever-code foo
				  field-name*.sym field-method*
				  methods-late-binding-alist))

  ;;Null or  a proper  list of  syntactic identifiers representing  the names  of the
  ;;interfaces implemented by this record-type.
  (define implemented-interface*.id
    (%get-implemented-interface-names clause*.stx synner))

  ;;Null or a proper  list of forms representing the code needed  to verify that this
  ;;record-type actually implements the specified interfaces.
  (define implemented-interfaces-table.sexp
    (%make-implemented-interfaces-table-code foo implemented-interface*.id))

  ;;Null or a list of definitions to build at run-time the record-type descriptor.
  (define foo-rtd-definitions
    (%make-rtd-definitions foo foo-rtd foo-uid generative? clause*.stx parent-rtd.id fields-vector-spec
			   foo-destructor.id (%make-custom-printer-code clause*.stx foo synner)
			   foo-equality-predicate.id foo-comparison-procedure.id foo-hash-function.id
			   methods-retriever-code.sexp implemented-interfaces-table.sexp synner))

  ;;A  symbolic expression  representing  a  form which,  expanded  and evaluated  at
  ;;expand-time, returns the right-hand side of the record-type name's DEFINE-SYNTAX.
  ;;The value of the right-hand side is the syntactic binding's descriptor.
  (define foo-syntactic-binding-form
    (%make-type-name-syntactic-binding-form foo foo-uid make-foo foo? foo-super-rcd.id foo-destructor.id
					    foo-parent.id foo-rtd foo-rcd
					    foo-equality-predicate.id foo-comparison-procedure.id foo-hash-function.id
					    (append method-name*.sym	field-name*.sym)
					    (append method-procname*.id	field-method*)
					    implemented-interface*.id))

  (bless
   `(module (,foo
	     ,make-foo ,foo?
	     ,@safe-field-accessor*
	     ,@(%filter-out-falses safe-field-mutator*)
	     ,@(if define-type-descriptors?
		   (list foo-rtd foo-rcd)
		 '())
	     ;;We  want to  create the  syntactic  bindings of  unsafe accessors  and
	     ;;mutators only when STRICT-R6RS mode is DISabled.
	     ,@(if (options::strict-r6rs-enabled?)
		   '()
		 (append unsafe-field-accessor* (%filter-out-falses unsafe-field-mutator*))))
      ,@parent-rtd-definition
      ,@parent-rcd-definition
      (define-syntax ,foo ,foo-syntactic-binding-form)
      ;;We want these common function definitions after the definition of FOO.
      ,@foo-destructor-definition
      ,@foo-equality-predicate-definition
      ,@foo-comparison-procedure-definition
      ,@foo-hash-function-definition
      ;;We want  the RTD definition after  the common functions definitions,  so that
      ;;the RTD can reference them.
      ,@foo-rtd-definitions
      ,@foo-rcd-definitions
      ,@foo-maker-definitions
      ,@foo-predicate-definitions
      ,@super-rcd-definition
      ,@method-form*.sexp
      ,@(%make-unsafe-accessor+mutator-code foo foo-rtd
					    field-name*.sym field-relative-idx* field-type*.ann
					    unsafe-field-accessor* unsafe-field-mutator*)
      ,@(%make-safe-accessor+mutator-code foo
					  field-name*.sym field-relative-idx* field-type*.ann
					  safe-field-accessor* unsafe-field-accessor*
					  safe-field-mutator*  unsafe-field-mutator*)
      ,@(%make-safe-method-code foo field-type*.ann
				field-method* unsafe-field-accessor* unsafe-field-mutator*)
      #| end of module |# )))


(module (%validate-definition-clauses)

  (define (%validate-definition-clauses clause* synner)
    (define-constant VALID-KEYWORDS (%valid-keywords))
    (let loop ((clause*  clause*)
	       (seen*    '()))
      (unless (null? clause*)
	(syntax-match (car clause*) ()
	  ((?kwd . ?rest)
	   (cond ((or (not (identifier? ?kwd))
		      (not (free-id-member? ?kwd VALID-KEYWORDS)))
		  (synner "not a valid DEFINE-RECORD-TYPE keyword" ?kwd))
		 ((bound-id-member? ?kwd seen*)
		  (synner "invalid duplicate clause in DEFINE-RECORD-TYPE" ?kwd))
		 (else
		  (loop (cdr clause*)
			(if (keyword-allowed-multiple-times? ?kwd)
			    seen*
			  (cons ?kwd seen*))))))
	  (?cls
	   (synner "malformed DEFINE-RECORD-TYPE clause" ?cls))
	  ))))

  (define %valid-keywords
    ;;Return a  list of  syntactic identifiers representing  the keywords  of clauses
    ;;accepted by DEFINE-RECORD-TYPE.
    ;;
    ;;NOTE We do not care here if the "strict R6RS" option is enabled.  (Marco Maggi;
    ;;Sat Feb 6, 2016)
    ;;
    (let ((cached #f))
      (lambda ()
	(or cached
	    (receive-and-return (rv)
		(map core-prim-id
		  '( ;;These are the R6RS ones.
		    fields parent parent-rtd protocol sealed opaque nongenerative
		     ;;;These are the Vicare extensions.
		    constructor-signature super-protocol destructor-protocol
		    custom-printer type-predicate
		    equality-predicate comparison-procedure hash-function
		    define-type-descriptors strip-angular-parentheses
		    method case-method method/overload mixins implements))
	      (set! cached rv))))))

  (define keyword-allowed-multiple-times?
    (let ((cached #f))
      (lambda (id)
	(free-id-member? id (or cached
				(receive-and-return (rv)
				    (map core-prim-id
				      '(method case-method method/overload fields
					       mixins implements))
				  (set! cached rv)))))))

  #| end of module: %VALIDATE-DEFINITION-CLAUSES |# )


(define (%introduce-mixin-clauses foo input-clause*.stx synner)
  (let clause-recur ((input-clause*.stx	input-clause*.stx))
    (if (pair? input-clause*.stx)
	(syntax-match (car input-clause*.stx) (mixins)
	  ((mixins ?mixin-name* ...)
	   (let mixin-recur ((mixin-name*.id ?mixin-name*))
	     (if (pair? mixin-name*.id)
		 (let ((mixin-name.id (car mixin-name*.id)))
		   (unless (identifier? mixin-name.id)
		     (synner "expected identifier as mixin name in MIXINS clause" mixin-name.id))
		   (let ((obj (retrieve-expand-time-value mixin-name.id)))
		     (syntax-match obj ()
		       ((?kwd ?mixin-name ?clause* ...)
			(eq? 'define-mixin (syntax->datum ?kwd))
			(append (syntax-replace-id ?clause* mixin-name.id foo)
				(mixin-recur (cdr mixin-name*.id))))
		       (_
			(synner "expected mixin name in MIXINS clause" mixin-name.id)))))
	       (clause-recur (cdr input-clause*.stx)))))
	  (_
	   (cons (car input-clause*.stx)
		 (clause-recur (cdr input-clause*.stx)))))
      '())))


;;;; basic parsing functions

(define (%get-clause sym clause*)
  ;;Given a symbol SYM representing the name  of a clause and a syntax object CLAUSE*
  ;;representing the definition clauses: search the  selected clause and return it as
  ;;syntax object.  When no matching clause is found: return false.
  ;;
  (let next ((id       (core-prim-id sym))
	     (clause*  clause*))
    (syntax-match clause* ()
      (()
       #f)
      (((?key . ?rest) . ?clause*)
       (if (~free-identifier=? id ?key)
	   `(,?key . ,?rest)
	 (next id ?clause*))))))

(define (%parse-full-name-spec spec strip-angular-parentheses? synner)
  ;;Given a syntax object representing  a full record-type name specification: return
  ;;the syntactic  identifiers: the  type name, the  constructor name,  the predicate
  ;;name, the type name with stripped angular parentheses (if requested).
  ;;
  (syntax-match spec ()
    ((?foo ?make-foo ?foo?)
     (and (identifier? ?foo)
	  (identifier? ?make-foo)
	  (identifier? ?foo?))
     (values ?foo ?make-foo ?foo? (%build-foo-for-id-generation ?foo strip-angular-parentheses?)))
    (?foo
     (identifier? ?foo)
     (let ((foo-for-id-generation (%build-foo-for-id-generation ?foo strip-angular-parentheses?)))
       (values ?foo
	       (identifier-append ?foo "make-" (syntax->datum foo-for-id-generation))
	       (identifier-append ?foo foo-for-id-generation "?")
	       foo-for-id-generation)))
    (_
     (synner "invalid record-type name specification" spec))))

(define (%get-uid foo clause* synner)
  ;;Return two values:  a syntactic identifier representing the UID;  a boolean, true
  ;;if this record-type is generative.
  ;;
  (let ((clause (%get-clause 'nongenerative clause*)))
    (syntax-match clause ()
      ((_)
       ;;This record-type is nongenerative.
       (values (datum->syntax foo (string->symbol (string-append "vicare:nongenerative:" (symbol->string (syntax->datum foo)))))
	       #f))
      ((_ ?uid)
       ;;This record-type is nongenerative.
       (identifier? ?uid)
       (values (syntax->datum ?uid) #f))
      ;;No matching clause found.  This record type will be non-generative.
      (#f
       ;;This record-type is generative.
       (values (datum->syntax foo (gensym (string-append "vicare:generative:" (symbol->string (syntax->datum foo)))))
	       #t))
      (_
       (synner "expected symbol or no argument in NONGENERATIVE clause" clause)))))

(define (%get-define-type-descriptors clause* synner)
  (let ((clause (%get-clause 'define-type-descriptors clause*)))
    (syntax-match clause ()
      ((_)	#t)
      ;;No matching clause found.
      (#f	#f)
      (_
       (synner "expected no argument in DEFINE-TYPE-DESCRIPTORS clause" clause)))))

(define (%get-strip-angular-parentheses clause* synner)
  (let ((clause (%get-clause 'strip-angular-parentheses clause*)))
    (syntax-match clause ()
      ((_)	#t)
      ;;No matching clause found.
      (#f	#f)
      (_
       (synner "expected no argument in STRIP-ANGULAR-PARENTHESES clause" clause)))))

(define (%get-constructor-signature clause* synner)
  (let ((clause (%get-clause 'constructor-signature clause*)))
    (syntax-match clause ()
      ((_ ?signature)
       (begin
	 ;;NOTE Here  we would  really like  to validate  ?SIGNATURE as  closure type
	 ;;specification.  But the  return value of the signature  is the record-type
	 ;;syntactic identifier, which  is unbound when this function  is called.  So
	 ;;we can do nothing.  We will validated it later.  (Marco Maggi; Sat Jun 25,
	 ;;2016)
	 ;;
	 ;; (unless (closure-type-spec? (type-annotation->object-type-spec ?signature))
	 ;;   (synner "expected closure type signature argument in CONSTRUCTOR-SIGNATURE clause" clause))
	 ?signature))
      ;;No matching clause found.
      (#f		#f)
      (_
       (synner "expected closure type signature argument in CONSTRUCTOR-SIGNATURE clause" clause)))))

(define (%get-implemented-interface-names clause* synner)
  (let loop ((clause*	clause*)
	     (iface*	'()))
    (syntax-match clause* (implements)
      (()
       (cond ((duplicate-identifiers? iface*)
	      => (lambda (id)
		   (synner "implemented interface declared multiple times" id)))
	     (else iface*)))
      (((implements ?interface* ...) . ?clauses)
       (begin
	 (for-all (lambda (obj)
		    (unless (identifier? obj)
		      (synner "expected interface identifier as argument in IMPLEMENTS clause" obj)))
	   ?interface*)
	 (loop ?clauses (append ?interface* iface*))))
      ((_ . ?clauses)
       (loop ?clauses iface*)))))


(define (%parse-field-specs type-id foo-for-id-generation clause* synner)
  ;;Given the definition  clauses CLAUSE* extract the FIELDS clauses  and parse them.
  ;;Return the following values:
  ;;
  ;;1..The list of symbols representing all the field names.
  ;;
  ;;2..The list of fixnums representings all the field relative indexes (zero-based).
  ;;
  ;;3..The list of "<object-type-spec>" instances representing the field types.
  ;;
  ;;4..A list of syntactic identifiers that will be bound to the safe accessors.
  ;;
  ;;5..A list of syntactic identifiers that will be bound to the unsafe accessors.
  ;;
  ;;6..A list of syntactic identifiers that will be bound to the safe mutators.  This
  ;;list holds #f in the position of immutable fields.
  ;;
  ;;7..A list  of syntactic identifiers  that will be  bound to the  unsafe mutators.
  ;;This list holds #f in the position of immutable fields.
  ;;
  ;;8..A  list  of  syntactic identifiers  that  will  be  bound  to the  safe  field
  ;;accessor/mutator methods.
  ;;
  ;;9..A vector to be used as FIELDS argument to MAKE-RECORD-TYPE-DESCRIPTOR.
  ;;
  ;;Here we assume that FIELD-CLAUSE* is null or a proper list.
  ;;
  (define fields-clause*
    (let loop ((clause*       clause*)
	       (field-spec**  '()))
      (syntax-match clause* (fields)
	(()
	 (if (options::strict-r6rs-enabled?)
	     (if (pair? field-spec**)
		 ;;If there is only one list of field specs, fine; otherwise raise an
		 ;;error.
		 (if (null? (cdr field-spec**))
		     (car field-spec**)
		   (synner "invalid multiple FIELDS clauses in strict-R6RS language"
			   (let ((fields.id (core-prim-id 'fields)))
			     (map (lambda (field-spec*)
				    (cons fields.id field-spec*))
			       field-spec**))))
	       '())
	   ;;Non-strict language: we accept any number of FIELDS clauses.
	   (if (pair? field-spec**)
	       (apply append (reverse field-spec**))
	     '())))
	(((fields ?field-spec* ...) . ?clauses)
	 (loop ?clauses (cons ?field-spec* field-spec**)))
	((_ . ?clauses)
	 (loop ?clauses field-spec**)))))
  (define (%gen-safe-accessor-name field-name.id)
    (identifier-append type-id foo-for-id-generation "-" field-name.id))
  (define (%gen-unsafe-accessor-name field-name.id)
    (identifier-append type-id "$" foo-for-id-generation "-" field-name.id))
  (define (%gen-safe-mutator-name field-name.id)
    (identifier-append type-id foo-for-id-generation "-" field-name.id "-set!"))
  (define (%gen-unsafe-mutator-name field-name.id)
    (identifier-append type-id "$" foo-for-id-generation "-" field-name.id "-set!"))
  (define (%gen-safe-method-name field-name.id)
    (identifier-append type-id foo-for-id-generation "-" field-name.id "-method"))

  (define (%parse-typed-field stx)
    (syntax-match stx (brace)
      ((brace ?id ?type)
       ;;FIXME Here we  should do full type annotation validation.   At present it is
       ;;not possible  becase here the syntactic  binding for the record  type is not
       ;;established yet,  so we cannot use  it in field specifications  (but we want
       ;;to).  (Marco Maggi; Fri Apr 29, 2016)
       (begin
	 ;; (unless (syntax-object.type-annotation? ?type)
	 ;;   (synner "invalid field type annotation" ?type))
	 (values ?id ?type)))
      (?id
       (identifier? ?id)
       (values ?id (<top>-type-id)))))

  (let loop ((fields-clause*			fields-clause*)
	     (i					0)
	     (field-name*.sym			'())
	     (field-relative-idx*		'())
	     (field-type*.ann			'())
	     (safe-field-accessor*		'())
	     (unsafe-field-accessor*		'())
	     (safe-field-mutator*		'())
	     (unsafe-field-mutator*		'())
	     (field-method*		'())
	     (fields-vector-spec*		'()))
    (define (%register-field-name field-name.id)
      (let ((field.sym (identifier->symbol field-name.id)))
	(if (memq field.sym field-name*.sym)
	    (synner "multiple field definitions with the same name" field-name.id)
	  (cons field.sym field-name*.sym))))
    (syntax-match fields-clause* (mutable immutable)
      (()
       (values (reverse field-name*.sym)
	       (reverse field-relative-idx*)
	       (reverse field-type*.ann)
	       (reverse safe-field-accessor*)
	       (reverse unsafe-field-accessor*)
	       (reverse safe-field-mutator*)
	       (reverse unsafe-field-mutator*)
	       (reverse field-method*)
	       (list->vector (reverse fields-vector-spec*))))

      (((mutable   ?name ?accessor ?mutator) . ?rest)
       (and (identifier? ?accessor)
	    (identifier? ?mutator))
       (receive (field-name.id field-type.ann)
	   (%parse-typed-field ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.ann field-type*.ann)
	       (cons ?accessor safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons ?mutator safe-field-mutator*)
	       (cons (%gen-unsafe-mutator-name  field-name.id) unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) field-method*)
	       (cons `(mutable ,field-name.id) fields-vector-spec*))))

      (((immutable ?name ?accessor) . ?rest)
       (identifier? ?accessor)
       (receive (field-name.id field-type.ann)
	   (%parse-typed-field ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.ann field-type*.ann)
	       (cons ?accessor safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons #f safe-field-mutator*)
	       (cons #f unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) field-method*)
	       (cons `(immutable ,field-name.id) fields-vector-spec*))))

      (((mutable   ?name) . ?rest)
       (receive (field-name.id field-type.ann)
	   (%parse-typed-field ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.ann field-type*.ann)
	       (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons (%gen-safe-mutator-name    field-name.id) safe-field-mutator*)
	       (cons (%gen-unsafe-mutator-name  field-name.id) unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) field-method*)
	       (cons `(mutable ,field-name.id) fields-vector-spec*))))

      (((immutable ?name) . ?rest)
       (receive (field-name.id field-type.ann)
	   (%parse-typed-field ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.ann field-type*.ann)
	       (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons #f safe-field-mutator*)
	       (cons #f unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) field-method*)
	       (cons `(immutable ,field-name.id) fields-vector-spec*))))

      ((?name . ?rest)
       (receive (field-name.id field-type.ann)
	   (%parse-typed-field ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.ann field-type*.ann)
	       (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons #f safe-field-mutator*)
	       (cons #f unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) field-method*)
	       (cons `(immutable ,field-name.id) fields-vector-spec*))))

      ((?spec . ?rest)
       (synner "invalid field specification in DEFINE-RECORD-TYPE syntax" ?spec)))))


(module (%parse-method-clauses)

  (define (%parse-method-clauses clause* foo foo-for-id-generation synner)
    ;;Parse  the METHOD,  CASE-METHOD and  METHOD/OVERRIDE clauses  in CLAUSE*.   The
    ;;methods defined with METHOD/OVERRIDE are *not* available for late binding.
    ;;
    ;;Return the following values values:
    ;;
    ;;1. A list of symbols representing the method names.
    ;;
    ;;2.   A  list  of  syntactic  identifiers bound  to  the  method  implementation
    ;;procedures.
    ;;
    ;;3.   A  list  of  symbolic  expressions  (to  be  BLESSed  later)  representing
    ;;definitions  which,  expanded and  evaluated  at  run-time, define  the  method
    ;;implementation procedures.
    ;;
    ;;4. An  alist to be  used for  methods late binding.   Its keys are  the methods
    ;;symbol name.  Its values are the methods syntactic identifiers.
    ;;
    (receive (method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist)
	(%parse-methods-and-case-methods clause* foo foo-for-id-generation synner)
      (receive (method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist)
	  (%parse-overloaded-methods clause* foo foo-for-id-generation synner method-name*.sym
				     method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist)
	(values method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist))))

;;; --------------------------------------------------------------------

  (define (%parse-methods-and-case-methods clause* foo foo-for-id-generation synner)
    (define-syntax-rule (recurse ?clause*)
      (%parse-methods-and-case-methods ?clause* foo foo-for-id-generation synner))
    (syntax-match clause* (method case-method)
      (()
       (values '() '() '() '()))

      (((method . ?stuff) . ?clause*)
       (receive (method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist)
	   (recurse ?clause*)
	 (receive (method-name.sym method-procname.id method-form.sexp)
	     (%parse-method ?stuff foo foo-for-id-generation method-name*.sym synner)
	   (values (cons method-name.sym	method-name*.sym)
		   (cons method-procname.id	method-procname*.id)
		   (cons method-form.sexp	method-form*.sexp)
		   (cons (cons method-name.sym method-procname.id)
			 methods-late-binding-alist)))))

      (((case-method . ?stuff) . ?clause*)
       (receive (method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist)
	   (recurse ?clause*)
	 (receive (method-name.sym method-procname.id method-form.sexp)
	     (%parse-case-method ?stuff foo foo-for-id-generation method-name*.sym synner)
	   (values (cons method-name.sym	method-name*.sym)
		   (cons method-procname.id	method-procname*.id)
		   (cons method-form.sexp	method-form*.sexp)
		   (cons (cons method-name.sym method-procname.id)
			 methods-late-binding-alist)))))

      ((_ . ?clause*)
       (recurse ?clause*))))

;;; --------------------------------------------------------------------

  (define (%parse-overloaded-methods clause* foo foo-for-id-generation synner other-method-name*.sym
				     method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist)
    ;;We parse  the METHOD/OVERLOAD  clauses knowing that  multiple methods  with the
    ;;same  name are  fine and  expected.  What  is forbidden  is that  an overloaded
    ;;method has the same name of a non-overloaded one.
    ;;
    (define-syntax-rule (recurse ?clause*)
      (%parse-overloaded-methods ?clause* foo foo-for-id-generation synner other-method-name*.sym
				 method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist))
    (syntax-match clause* (method/overload)
      (()
       (values method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist))

      (((method/overload . ?stuff) . ?clause*)
       (receive (method-name*.sym method-procname*.id method-form*.sexp methods-late-binding-alist)
	   (recurse ?clause*)
	 (receive (method-name.sym method-procname.id method-form.sexp)
	     (%parse-method-overload ?stuff foo foo-for-id-generation other-method-name*.sym synner)
	   (if (memq method-name.sym method-name*.sym)
	       ;;An overloaded method with this name already exists.
	       (values method-name*.sym
		       method-procname*.id
		       (cons method-form.sexp	method-form*.sexp)
		       methods-late-binding-alist)
	     ;;An overloaded method with this name does not already exists.
	     (values (cons method-name.sym	method-name*.sym)
		     (cons method-procname.id	method-procname*.id)
		     (cons method-form.sexp	method-form*.sexp)
		     (cons (cons method-name.sym method-procname.id)
			   methods-late-binding-alist))))))

      ((_ . ?clause*)
       (recurse ?clause*))))

;;; --------------------------------------------------------------------

  (define (%parse-method clause.stx foo foo-for-id-generation method-name*.sym synner)
    (define subject.id (make-syntactic-identifier-for-temporary-variable "subject"))
    (syntax-match clause.stx (brace)
      (((?who . ?args) ?body0 ?body* ...)
       (identifier? ?who)
       (let* ((method-name.sym		(identifier->symbol ?who))
	      (method-procname.id	(%mk-method-procname-id foo-for-id-generation method-name.sym))
	      (method-form.sexp		`(define/checked (,method-procname.id {,subject.id ,foo} . ,?args)
					   (fluid-let-syntax ((this (identifier-syntax ,subject.id)))
					     ,?body0 . ,?body*))))
	 (%validate-method-name ?who method-name.sym method-name*.sym synner)
	 (values method-name.sym method-procname.id method-form.sexp)))

      ((((brace ?who ?rv-tag0 . ?rv-tag*) . ?formals) ?body0 . ?body*)
       (identifier? ?who)
       (let* ((method-name.sym		(identifier->symbol ?who))
	      (method-procname.id	(%mk-method-procname-id foo-for-id-generation method-name.sym))
	      (method-form.sexp		`(define/checked ((brace ,method-procname.id ,?rv-tag0 . ,?rv-tag*) {,subject.id ,foo} . ,?formals)
					   (fluid-let-syntax ((this (identifier-syntax ,subject.id)))
					     ,?body0 . ,?body*))))
	 (%validate-method-name ?who method-name.sym method-name*.sym synner)
	 (values method-name.sym method-procname.id method-form.sexp)))

      (_
       (synner "invalid syntax in METHOD clause" (cons (method-id) clause.stx)))))

  (define (%parse-case-method clause.stx foo foo-for-id-generation method-name*.sym synner)
    (syntax-match clause.stx (brace)
      ((?who ?method-clause0 ?method-clause* ...)
       (identifier? ?who)
       (let* ((method-name.sym		(identifier->symbol ?who))
	      (method-procname.id	(%mk-method-procname-id foo-for-id-generation method-name.sym))
	      (method-clause*.stx	(map (lambda (clause.stx)
					       (%add-this-to-clause-formals clause.stx foo synner))
					  (cons ?method-clause0 ?method-clause*)))
	      (method-form.sexp		`(case-define ,method-procname.id . ,method-clause*.stx)))
	 (%validate-method-name ?who method-name.sym method-name*.sym synner)
	 (values method-name.sym method-procname.id method-form.sexp)))

      (_
       (synner "invalid syntax in CASE-METHOD clause" (cons (case-method-id) clause.stx)))))

  (define (%add-this-to-clause-formals clause.stx foo synner)
    (define subject.id (make-syntactic-identifier-for-temporary-variable "subject"))
    (syntax-match clause.stx (brace)
      ((((brace ?underscore . ?rv-types) . ?formals) ?body0 ?body* ...)
       (underscore-id? ?underscore)
       (bless
	`(({,?underscore . ,?rv-types} {,subject.id ,foo} . ,?formals)
	  (fluid-let-syntax ((this (identifier-syntax ,subject.id)))
	    ,?body0 . ,?body*))))

      ((?formals ?body0 ?body* ...)
       (bless
	`(({,subject.id ,foo} . ,?formals)
	  (fluid-let-syntax ((this (identifier-syntax ,subject.id)))
	    ,?body0 . ,?body*))))
      (_
       (synner "invalid CASE-METHOD clause syntax" clause.stx))))

;;; --------------------------------------------------------------------

  (define (%parse-method-overload clause.stx foo foo-for-id-generation other-method-name*.sym synner)
    (define subject.id (make-syntactic-identifier-for-temporary-variable "subject"))
    (syntax-match clause.stx (brace)
      (((?who . ?args) ?body0 ?body* ...)
       (identifier? ?who)
       (let* ((method-name.sym		(identifier->symbol ?who))
	      (method-procname.id	(%mk-method-procname-id foo-for-id-generation method-name.sym))
	      (method-form.sexp		`(define/overload (,method-procname.id {,subject.id ,foo} . ,?args)
					   (fluid-let-syntax ((this (identifier-syntax ,subject.id)))
					     ,?body0 . ,?body*))))
	 (%validate-method-name ?who method-name.sym other-method-name*.sym synner)
	 (values method-name.sym method-procname.id method-form.sexp)))

      ((((brace ?who ?rv-tag0 . ?rv-tag*) . ?formals) ?body0 . ?body*)
       (identifier? ?who)
       (let* ((method-name.sym		(identifier->symbol ?who))
	      (method-procname.id	(%mk-method-procname-id foo-for-id-generation method-name.sym))
	      (method-form.sexp		`(define/overload ((brace ,method-procname.id ,?rv-tag0 . ,?rv-tag*) {,subject.id ,foo} . ,?formals)
					   (fluid-let-syntax ((this (identifier-syntax ,subject.id)))
					     ,?body0 . ,?body*))))
	 (%validate-method-name ?who method-name.sym other-method-name*.sym synner)
	 (values method-name.sym method-procname.id method-form.sexp)))

      (_
       (synner "invalid syntax in METHOD clause" (cons (method-id) clause.stx)))))

;;; --------------------------------------------------------------------

  (define (%validate-method-name who.id method-name.sym method-name*.sym synner)
    (when (memq method-name.sym method-name*.sym)
      (synner "multiple method definitions with the same name" who.id)))

  (define (%mk-method-procname-id foo-for-id-generation method-name.sym)
    (datum->syntax foo-for-id-generation
		   (string->symbol
		    (string-append (symbol->string (syntax->datum foo-for-id-generation))
				   "-"
				   (symbol->string method-name.sym)))))

  #| end of module: %PARSE-METHOD-CLAUSES |# )


(define (%make-unsafe-accessor+mutator-code foo foo-rtd
					    field-name*.sym field-relative-idx* field-type*.ann
					    unsafe-field-accessor* unsafe-field-mutator*)
  ;;Return  a  list holding  a  single  symbolic  expression  (to be  BLESSed  later)
  ;;representing  a Scheme  expression  which, expanded  and  evaluated at  run-time,
  ;;defines the syntactic bindings of the  unsafe fields accessors and mutators.  The
  ;;returned list is meant to be spliced in the output form.
  ;;
  ;;FOO must be  the syntactic identifier represening the  record-type name.  FOO-RTD
  ;;must be a symbol  representing the name of the syntactic  identifier bound to the
  ;;record-type descriptor.
  ;;
  ;;FIELD-NAME*.SYM must be  a list of identifiers representing the  names of all the
  ;;fields and the mutable fields.
  ;;
  ;;FIELD-RELATIVE-IDX* must be  a list of fixnums representing  the relative indexes
  ;;of all the fields and the mutable fields.
  ;;
  ;;FIELD-TYPE*.ANN must  be a list of  syntax objects representing the  fields' type
  ;;annotations.
  ;;
  ;;UNSAFE-FIELD-ACCESSOR*  and   UNSAFE-FIELD-MUTATOR*  must  be  list   of  symbols
  ;;representing the  names of the  syntactic identifiers  bound to the  unsafe field
  ;;accessors and mutators.
  ;;

  (define (%make-field-index-varname x.id)
    (string->symbol (string-append foo.str "-" (symbol->string (syntax->datum x.id)) "-index")))

  (define foo.str
    (symbol->string (identifier->symbol foo)))

  (define foo-first-field-offset
    (%named-gensym/suffix foo "-first-field-offset"))

  (if (null? field-name*.sym)
      '()
    `((module (,@unsafe-field-accessor*
	       ,@(%filter-out-falses unsafe-field-mutator*))
	(define/checked (brace ,foo-first-field-offset <non-negative-fixnum>)
	  ;;The field at index  3 in the RTD is: the index of  the first field of this
	  ;;subtype in the  layout of instances; it  is the total number  of fields of
	  ;;the parent type.
	  (unsafe-cast-signature (<non-negative-fixnum>) ($struct-ref ,foo-rtd 3)))

	;;all fields indexes
	,@(map (lambda (x idx)
		 (let ((the-index (%make-field-index-varname x)))
		   `(define/checked (brace ,the-index <non-negative-fixnum>)
		      (fx+ ,idx ,foo-first-field-offset))))
	    field-name*.sym field-relative-idx*)

	;;unsafe record fields accessors
	,@(map (lambda (unsafe-foo-x x field-type.ann)
		 (let ((the-index	(%make-field-index-varname x))
		       (record.sym	(make-syntactic-identifier-for-temporary-variable "record")))
		   `(define-syntax ,unsafe-foo-x
		      (identifier-syntax
		       (lambda/typed ((brace _ ,field-type.ann) (brace ,record.sym ,foo))
			 ($struct-ref ,record.sym ,the-index))))))
	    unsafe-field-accessor* field-name*.sym field-type*.ann)

	;;unsafe record fields mutators
	,@(fold-right
	      (lambda (unsafe-field-mutator field-name.sym field-type.ann knil)
		(if unsafe-field-mutator
		    (cons (let ((the-index	(%make-field-index-varname field-name.sym))
				(record.sym	(make-syntactic-identifier-for-temporary-variable "record"))
				(value.sym	(make-syntactic-identifier-for-temporary-variable "new-value")))
			    `(define-syntax ,unsafe-field-mutator
			       (identifier-syntax
				(lambda/typed ((brace _ <void>) (brace ,record.sym ,foo) (brace ,value.sym ,field-type.ann))
				  ($struct-set! ,record.sym ,the-index ,value.sym)))))
			  knil)
		  knil))
	    '() unsafe-field-mutator* field-name*.sym field-type*.ann)

	#| end of module: unsafe accessors and mutators |# ))))


(define (%make-safe-accessor+mutator-code foo
					  field-name*.sym field-relative-idx* field-type*.ann
					  safe-field-accessor* unsafe-field-accessor*
					  safe-field-mutator*  unsafe-field-mutator*)
  ;;Return  a  list holding  a  single  symbolic  expression  (to be  BLESSed  later)
  ;;representing  a Scheme  expression  which, expanded  and  evaluated at  run-time,
  ;;defines the syntactic bindings of the  unsafe fields accessors and mutators.  The
  ;;returned list is meant to be spliced in the output form.
  ;;
  ;;FOO must be the syntactic identifier represening the record-type name.
  ;;
  ;;FIELD-NAME*.SYM must  be the list  of symbols representing  the names of  all the
  ;;fields.
  ;;
  ;;FIELD-RELATIVE-IDX* must be the list of fixnums representing the relative indexes
  ;;of all the fields.
  ;;
  ;;FIELD-TYPE*.ANN must be the list of  syntax objects representing the fields' type
  ;;annotations.
  ;;
  ;;SAFE-FIELD-ACCESSOR* and SAFE-FIELD-MUTATOR* must be list of symbols representing
  ;;the names  of the  syntactic identifiers  bound to the  safe field  accessors and
  ;;mutators.
  ;;
  ;;UNSAFE-FIELD-ACCESSOR*  and   UNSAFE-FIELD-MUTATOR*  must  be  list   of  symbols
  ;;representing the  names of the  syntactic identifiers  bound to the  unsafe field
  ;;accessors and mutators.
  ;;
  ;;NOTE The unsafe variant of the field accessor and mutator must be a syntax object
  ;;which, expanded by itself and evaluated, returns an accessor or mutator function.
  ;;We know that when the compiler finds a form like:
  ;;
  ;;   ((lambda (record)           (unsafe-foo-x record))
  ;;    the-record)
  ;;   ((lambda (record new-value) (unsafe-foo-x-set! record new-value))
  ;;    the-record the-new-value)
  ;;
  ;;it integrates the LAMBDA into:
  ;;
  ;;   (unsafe-foo-x the-record)
  ;;   (unsafe-foo-x-set! the-record the-new-value)
  ;;
  (define safe-field-accessor-form*
    (map (lambda (safe-field-accessor unsafe-field-accessor field-type.ann)
	   (let ((record.sym	(make-syntactic-identifier-for-temporary-variable "record")))
	     `(define/checked ((brace ,safe-field-accessor ,field-type.ann) (brace ,record.sym ,foo))
		(unsafe-cast-signature (,field-type.ann) (,unsafe-field-accessor ,record.sym)))))
      safe-field-accessor* unsafe-field-accessor* field-type*.ann))

  (define safe-field-mutator-form*
    (fold-right
	(lambda (safe-field-mutator unsafe-field-mutator field-type.ann knil)
	  (if safe-field-mutator
	      (cons (let ((record.sym	(make-syntactic-identifier-for-temporary-variable "record"))
			  (val.sym	(make-syntactic-identifier-for-temporary-variable "new-value")))
		      `(define/checked ((brace ,safe-field-mutator <void>) (brace ,record.sym ,foo) (brace ,val.sym ,field-type.ann))
			 (,unsafe-field-mutator ,record.sym ,val.sym)))
		    knil)
	    knil))
      '() safe-field-mutator* unsafe-field-mutator* field-type*.ann))

  (if (and (null? safe-field-accessor-form*)
	   (null? safe-field-mutator-form*))
      '()
    (append safe-field-accessor-form* safe-field-mutator-form*)))


(define (%make-safe-method-code foo
				field-type*.ann
				field-method* unsafe-field-accessor* unsafe-field-mutator*)
  ;;Return a list  holding a symbolic expressions (to be  BLESSed later) representing
  ;;Scheme  expressions  which,  expanded  and  evaluated  at  run-time,  define  the
  ;;syntactic bindings of the  safe field methods.  The returned list  is meant to be
  ;;spliced in the output form.  The field methods act as both accessors (for all the
  ;;fields) and mutators (for mutable fields).
  ;;
  ;;FOO must be the syntactic identifier represening the record-type name.
  ;;
  ;;FIELD-TYPE*.ANN must be the list of  syntax objects representing the fields' type
  ;;annotations.
  ;;
  ;;FIELD-METHOD*  must be  a  list of  symbols representing  the  names of  the
  ;;syntactic identifiers the will be bound to the safe field methods.
  ;;
  ;;UNSAFE-FIELD-ACCESSOR*  and   UNSAFE-FIELD-MUTATOR*  must  be  list   of  symbols
  ;;representing the  names of the  syntactic identifiers  bound to the  unsafe field
  ;;accessors and mutators.
  ;;
  (map (lambda (safe-field-method unsafe-field-accessor unsafe-field-mutator field-type.ann)
	 (let ((record.sym	(make-syntactic-identifier-for-temporary-variable "record"))
	       (new-value.sym	(make-syntactic-identifier-for-temporary-variable "new-value")))
	   (if unsafe-field-mutator
	       `(case-define/checked ,safe-field-method
		  (((brace _ ,field-type.ann) (brace ,record.sym ,foo))
		   (unsafe-cast-signature (,field-type.ann) (,unsafe-field-accessor ,record.sym)))
		  (((brace _ <void>) (brace ,record.sym ,foo) (brace ,new-value.sym ,field-type.ann))
		   (,unsafe-field-mutator ,record.sym ,new-value.sym)))
	     `(define/checked ((brace ,safe-field-method ,field-type.ann) (brace ,record.sym ,foo))
		(unsafe-cast-signature (,field-type.ann) (,unsafe-field-accessor ,record.sym))))))
    field-method* unsafe-field-accessor* unsafe-field-mutator* field-type*.ann))


(define (%make-parent-rtd+rcd-code clause* foo input-form.stx synner)
  ;;Parse  the PARENT  and PARENT-RTD  definition clauses  in CLAUSE*.   Return these
  ;;values:
  ;;
  ;;1. FOO-PARENT: an  identifier representing the parent type, or  false if there is
  ;;no parent or the parent is specified through the procedural layer.
  ;;
  ;;2. PARENT-RTD: false if this record-type has  no parent; otherwise it is a symbol
  ;;representing  the  name  of  the  syntactic  identifier  bound  to  the  parent's
  ;;record-type descriptor.
  ;;
  ;;3. PARENT-RCD: false if this record-type has  no parent; otherwise it is a symbol
  ;;representing  the  name  of  the  syntactic  identifier  bound  to  the  parent's
  ;;record-constructor descriptor.
  ;;
  ;;4.  PARENT-RTD-DEFINITION: null  or a  list holding  a DEFINE  form defining  the
  ;;parent RTD syntactic binding, the list is spliced in the output.
  ;;
  ;;5.  PARENT-RCD-DEFINITION: null  or a  list holding  a DEFINE  form defining  the
  ;;parent RCD syntactic binding, the list is spliced in the output.
  ;;
  (let ((parent-clause (%get-clause 'parent clause*)))
    (syntax-match parent-clause ()
      ;;If there  is a  PARENT clause  insert code  that retrieves  the RTD  from the
      ;;parent type name.
      ((_ ?parent-name)
       (identifier? ?parent-name)
       ;;Validate  ?PARENT-NAME  as  syntactic  identifier  bound  to  a  record-type
       ;;syntactic binding.
       (let* ((parent-rts	(id->record-type-spec ?parent-name))
	      (parent-proto	(record-type-spec.super-protocol-id parent-rts))
	      (parent-rtd.id	(%named-gensym/suffix foo "-parent-rtd"))
	      (parent-rcd.id	(%named-gensym/suffix foo "-parent-rcd")))
	 (values ?parent-name parent-rtd.id parent-rcd.id
		 `((define/typed {,parent-rtd.id <record-type-descriptor>}
		     (record-type-descriptor ,?parent-name)))
		 ;;If the  parent has  a super-type  constructor descriptor:  use it;
		 ;;otherwise use the default constructor descriptor.
		 `((define/typed {,parent-rcd.id <record-constructor-descriptor>}
		     ,(or parent-proto `(record-constructor-descriptor ,?parent-name)))))))

      ;;If there is no PARENT clause try to retrieve the expression evaluating to the
      ;;RTD.
      (#f
       (let ((parent-rtd-clause (%get-clause 'parent-rtd clause*)))
	 (syntax-match parent-rtd-clause ()
	   ((_ ?parent-rtd ?parent-rcd)
	    (let ((parent-rtd.id	(%named-gensym/suffix foo "-parent-rtd"))
		  (parent-rcd.id	(%named-gensym/suffix foo "-parent-rcd")))
	      (values #f parent-rtd.id parent-rcd.id
		      `((define/typed {,parent-rtd.id <record-type-descriptor>}
			  ,?parent-rtd))
		      `((define/typed {,parent-rcd.id <record-constructor-descriptor>}
			  ,?parent-rcd)))))

	   ;;If  neither the  PARENT nor  the  PARENT-RTD clauses  are present:  just
	   ;;return false.
	   (#f
	    (values #f #f #f '() '()))

	   (_
	    (synner "invalid syntax in PARENT-RTD clause" parent-rtd-clause)))))

      (_
       (synner "invalid syntax in PARENT clause" parent-clause)))))


(define (%make-rtd-definitions foo foo-rtd foo-uid generative?
			       clause* parent-rtd fields-vector-spec
			       destructor printer
			       equality-predicate comparison-procedure hash-function
			       method-retriever implemented-interfaces-table synner)
  ;;Return a list of symbolic expressions (to be BLESSed later) representing a Scheme
  ;;definitions which,  expanded and  evaluated at  run-time, define  the record-type
  ;;descriptor.
  ;;
  ;;PARENT-RTD must be false if this record-type  has no parent; otherwise it must be
  ;;an identifier representing the syntactic binding of the parent RTD.
  ;;
  (let ((sealed?		(let ((clause (%get-clause 'sealed clause*)))
				  (syntax-match clause ()
				    ((_ #t)	#t)
				    ((_ #f)	#f)
				    ;;No matching clause found.
				    (#f		#f)
				    (_
				     (synner "invalid argument in SEALED clause" clause)))))
	(opaque?		(let ((clause (%get-clause 'opaque clause*)))
				  (syntax-match clause ()
				    ((_ #t)	#t)
				    ((_ #f)	#f)
				    ;;No matching clause found.
				    (#f		#f)
				    (_
				     (synner "invalid argument in OPAQUE clause" clause)))))
	(fields-vec		`(quote ,fields-vector-spec))
	(normalised-fields-vec	`(quote ,(vector-map (lambda (item)
						       (cons (eq? 'mutable (car item)) (cadr item)))
					   fields-vector-spec))))
    `((define/typed (brace ,foo-rtd <record-type-descriptor>)
	($make-record-type-descriptor-ex (quote ,foo) ,parent-rtd
					 (quote ,foo-uid) ,generative? ,sealed? ,opaque?
					 ,fields-vec ,normalised-fields-vec
					 ,destructor ,printer
					 ,equality-predicate ,comparison-procedure ,hash-function
					 ,method-retriever ,implemented-interfaces-table)))))


(define (%make-rcd-definitions clause* foo foo-rtd foo-rcd parent-rcd.id synner)
  ;;Return a list  of symbolic expressions (to be BLESSed  later) representing Scheme
  ;;definitions defining the default record-constructor descriptor.
  ;;
  ;;If this  record-type has no  parent: PARENT-RCD.ID is  false; otherwise it  is an
  ;;identifier  representing  the name  of  the  syntactic  identifier bound  to  the
  ;;parent's record-constructor descriptor.
  ;;
  (let ((protocol-expr (let ((clause (%get-clause 'protocol clause*)))
			 (syntax-match clause ()
			   ((_ ?expr)
			    ?expr)

			   ;;No matching clause found.
			   (#f	#f)

			   (_
			    (synner "invalid syntax in PROTOCOL clause" clause))))))
    `((define/typed (brace ,foo-rcd <record-constructor-descriptor>)
	($make-record-constructor-descriptor ,foo-rtd ,parent-rcd.id ,protocol-expr)))))


(define (%make-maker-definitions foo foo-rcd make-foo constructor-signature.stx synner)
  ;;Return a list  of symbolic expressions (to be BLESSed  later) representing Scheme
  ;;definitions defining the record maker procedure.
  ;;
  (let ((internal-maker.id	(make-syntactic-identifier-for-temporary-variable (identifier->symbol make-foo)))
	(args.id		(make-syntactic-identifier-for-temporary-variable "args")))
    `((define/std ,internal-maker.id
	($record-constructor ,foo-rcd))
      ,(if constructor-signature.stx
	   `(begin
	      (define/checked (brace ,make-foo ,constructor-signature.stx) ,internal-maker.id)
	      (begin-for-syntax
		(unless (type-annotation-super-and-sub? (lambda <bottom> => (,foo))
							,constructor-signature.stx)
		  (syntax-violation 'define-record-type
		    "the record constructor signature is not a closure type with record-type as single return value"
		    (syntax ,constructor-signature.stx) #f))))
	 `(define/checked ((brace ,make-foo ,foo) . ,args.id)
	    (unsafe-cast-signature (,foo) (apply ,internal-maker.id ,args.id)))))))


(define (%make-predicate-definitions clause* foo? foo-rtd synner)
  ;;Return a list  of symbolic expressions (to be BLESSed  later) representing Scheme
  ;;definitions  defining the  record-type  predicate which,  possibly,  is the  type
  ;;custom predicate.
  ;;
  ;;FOO? must  be the syntactic  identifier that will be  bound to the  (custom) type
  ;;predicate.
  ;;
  ;;FOO-RTD must be a symbol representing  the name of the syntactic identifier bound
  ;;to this type's RTD.
  ;;
  (syntax-match (%get-clause 'type-predicate clause*) ()
    ((_ ?type-predicate-expr)
     ;;There is a TYPE-PREDICATE clause in this record-type definition.
     (let ((arg.sym			(make-syntactic-identifier-for-temporary-variable "obj"))
	   (internal-predicate.sym	(make-syntactic-identifier-for-temporary-variable
					 (string-append "internal-predicate-" (identifier->string foo?)))))
       `((define/std ,internal-predicate.sym
	   (,?type-predicate-expr (lambda/checked ({_ <boolean>} ,arg.sym)
				    (unsafe-cast-signature (<boolean>)
				      (and ($struct? ,arg.sym)
					   ($record-and-rtd? ,arg.sym ,foo-rtd))))))
	 (define/checked ((brace ,foo? <boolean>) ,arg.sym)
	   (,internal-predicate.sym ,arg.sym)))))

    (#f
     ;;No TYPE-PREDICATE clause  in this record-type definition.  Return  a list of
     ;;definitions representing the default record-type predicate definition.
     (let ((arg.sym (make-syntactic-identifier-for-temporary-variable "obj")))
       `((define/typed ((brace ,foo? <boolean>) ,arg.sym)
	   (and ($struct? ,arg.sym)
		($record-and-rtd? ,arg.sym ,foo-rtd))))))

    (?invalid-clause
     (synner "invalid syntax in TYPE-PREDICATE clause" ?invalid-clause))))


(define (%make-super-rcd-code clause* foo foo-rtd foo-parent.id parent-rcd.sym synner)
  ;;Return  a  symbolic  expression  (to  be BLESSed  later)  representing  a  Scheme
  ;;expression   which,   expanded   and   evaluated   at   run-time,   returns   the
  ;;record-constructor descriptor  to be  used when  this type  is the  super-type of
  ;;another one.  If there is no SUPER-PROTOCOL clause in CLAUSE*: return false.
  ;;
  ;;FOO must be the identifier representing this record type.
  ;;
  ;;FOO-RTD must be a symbol representing  the name of the syntactic identifier bound
  ;;to this type's RTD.
  ;;
  ;;FOO-PARENT.ID  must be  false if  this record-type  has no  parent or  has parent
  ;;specified through  the procedural interface;  otherwise it must be  the syntactic
  ;;identifier representing the parent's type.
  ;;
  ;;PARENT-RCD.SYM must be false if this record-type has no parent; otherwise it must
  ;;be  a symbol  representing the  name  of the  syntactic identifier  bound to  the
  ;;parent's record-constructor descriptor.
  ;;
  (syntax-match (%get-clause 'super-protocol clause*) ()
    ((_ ?super-protocol-expr)
     (if foo-parent.id
	 ;;This record type has a parent selected with the PARENT clause.
	 (let* ((rts   (id->record-type-spec foo-parent.id))
		(proto (record-type-spec.super-protocol-id rts)))
	   (if proto
	       ;;The parent record-type specification has a super-protocol.
	       `($make-record-constructor-descriptor ,foo-rtd ,proto ,?super-protocol-expr)
	     ;;The parent record-type specification has no super-protocol: let's use
	     ;;the parent's default RCD.
	     `($make-record-constructor-descriptor ,foo-rtd ,parent-rcd.sym ,?super-protocol-expr)))
       ;;This record type has no parent.
       `($make-record-constructor-descriptor ,foo-rtd #f ,?super-protocol-expr)))

    (#f
     ;;No SUPER-PROTOCOL clause in this record-type definition.
     #f)

    (?invalid-clause
     (synner "invalid syntax in SUPER-PROTOCOL clause" ?invalid-clause))))


(define (%make-destructor-code clause* foo foo-parent parent-rtd.sym synner)
  ;;Extract from  the CLAUSE*  the DESTRUCTOR-PROTOCOL one  and return  an expression
  ;;which, expanded and  evaluated at run-time, will return  the destructor function;
  ;;the expression will return false if there is no destructor.
  ;;
  ;;If FOO-PARENT  is false:  this record-type  definition has  no PARENT  clause, so
  ;;either it has  no parent or its  parent is specified with  the PARENT-RTD clause.
  ;;If FOO-PARENT is  true: this record type  has a parent specified  with the PARENT
  ;;clause.
  ;;
  ;;PARENT-RTD.SYM is  false if  this record-type  has no parent;  otherwise it  is a
  ;;symbol representing the name of the syntactic identifier bound to the parent RTD.
  ;;
  (let ((clause                  (%get-clause 'destructor-protocol clause*))
	(foo-destructor-protocol (%named-gensym/suffix foo "-destructor-protocol"))
	(destructor-tmp          (%named-gensym/suffix foo "-destructor")))
    (syntax-match clause ()
      ((_ ?destructor-protocol-expr)
       ;;This record definition has a destructor protocol.
       `(let ((,foo-destructor-protocol ,?destructor-protocol-expr))
	  (unless (procedure? ,foo-destructor-protocol)
	    (assertion-violation (quote ,foo)
	      "expected closure object as result of evaluating the destructor protocol expression"
	      ,foo-destructor-protocol))
	  (receive-and-return (,destructor-tmp)
	      ,(if (or foo-parent parent-rtd.sym)
		   `(,foo-destructor-protocol (internal-applicable-record-type-destructor ,parent-rtd.sym))
		 `(,foo-destructor-protocol))
	    (unless (procedure? ,destructor-tmp)
	      (assertion-violation (quote ,foo)
		"expected closure object as result of applying the destructor protocol function"
		,destructor-tmp)))))

      ;;No  matching  clause  found.   This record  definition  has  no  destructor
      ;;protocol, but the parent (if any) might have one.
      ;;
      ;;*  If  the  parent  record-type  has  a  record  destructor:  the  parent's
      ;;destructor becomes this record-type's destructor.
      ;;
      ;;* If  the parent record-type  has no record destructor:  this record-type's
      ;;record destructor variable is set to false.
      ;;
      (#f
       (if (or foo-parent parent-rtd.sym)
	   `(record-type-destructor ,parent-rtd.sym)
	 ;;Set to false this record-type record destructor variable.
	 #f))

      (_
       (synner "invalid syntax in DESTRUCTOR-PROTOCOL clause" clause)))))


(define (%make-custom-printer-code clause* foo synner)
  ;;Extract from the  definition clauses CLAUSE* the CUSTOM-PRINTER one  and return a
  ;;symbolic expression (to be BLESSed later) representing a Scheme expression which,
  ;;expanded and evaluated  at run-time, will return the custom  printer function and
  ;;register it in the RTD.  Return false if there is no CUSTOM-PRINTER clause.
  ;;
  (let ((clause (%get-clause 'custom-printer clause*)))
    (syntax-match clause ()
      ((_ ?expr)
       (let ((printer (%named-gensym/suffix foo "-custom-printer")))
	 `(receive-and-return (,printer)
	      ,?expr
	    (unless (procedure? ,printer)
	      (assertion-violation (quote ,foo)
		"expected closure object from evaluation of expression in CUSTOM-PRINTER clause"
		,printer)))))

      ;;No matching clause found.
      (#f	#f)

      (_
       (synner "invalid syntax in CUSTOM-PRINTER clause" clause)))))


(define (%make-equality-predicate-code clause* foo parent-rtd synner)
  ;;Extract from the definition clauses CLAUSE* the EQUALITY-PREDICATE one and return
  ;;a  symbolic expression  (to be  BLESSed later)  representing a  Scheme expression
  ;;which, expanded and evaluated at run-time, will return the equality predicate and
  ;;register it in the RTD.  Return false if there is no EQUALITY-PREDICATE clause.
  ;;
  (let ((clause (%get-clause 'equality-predicate clause*)))
    (syntax-match clause ()
      ((_ ?proto-expr)
       (let ((proto-func.sym	(make-syntactic-identifier-for-temporary-variable "proto-func"))
	     (pred-func.sym	(make-syntactic-identifier-for-temporary-variable "pred-func")))
	 (if parent-rtd
	     ;;The new  record-type has a parent:  we apply the protocol  function to
	     ;;the parent's equality predicate function.
	     `(let ((,proto-func.sym ,?proto-expr))
		(receive-and-return (,pred-func.sym)
		    (,proto-func.sym ($record-type-equality-predicate ,parent-rtd))
		  (unless (procedure? ,pred-func.sym)
		    (assertion-violation (quote ,foo)
		      "expected closure object from evaluation of equality predicate's protocol function"
		      ,pred-func.sym))))
	   ;;The  new  record-type has  no  parent:  we  just evaluate  the  protocol
	   ;;function with no arguments.
	   `(let ((,proto-func.sym ,?proto-expr))
	      (receive-and-return (,pred-func.sym)
		  (,proto-func.sym)
		(unless (procedure? ,pred-func.sym)
		  (assertion-violation (quote ,foo)
		    "expected closure object from evaluation of equality predicate's protocol function"
		    ,pred-func.sym)))))))

      ;;No matching clause found.
      (#f	#f)

      (_
       (synner "invalid syntax in EQUALITY-PREDICATE clause" clause)))))


(define (%make-comparison-procedure-code clause* foo parent-rtd synner)
  ;;Extract  from the  definition clauses  CLAUSE* the  COMPARISON-PROCEDURE one  and
  ;;return  a  symbolic  expression  (to  be BLESSed  later)  representing  a  Scheme
  ;;expression which, expanded and evaluated  at run-time, will return the comparison
  ;;procedure  and   register  it  in  the   RTD.   Return  false  if   there  is  no
  ;;COMPARISON-PROCEDURE clause.
  ;;
  (let ((clause (%get-clause 'comparison-procedure clause*)))
    (syntax-match clause ()
      ((_ ?proto-expr)
       (let ((proto-func.sym	(make-syntactic-identifier-for-temporary-variable "proto-func"))
	     (compar-func.sym	(make-syntactic-identifier-for-temporary-variable "compar-func")))
	 (if parent-rtd
	     ;;The new  record-type has a parent:  we apply the protocol  function to
	     ;;the parent's comparison procedure.
	     `(let ((,proto-func.sym ,?proto-expr))
		(receive-and-return (,compar-func.sym)
		    (,proto-func.sym ($record-type-comparison-procedure ,parent-rtd))
		  (unless (procedure? ,compar-func.sym)
		    (assertion-violation (quote ,foo)
		      "expected closure object from evaluation of comparison procedure's protocol function"
		      ,compar-func.sym))))
	   ;;The  new  record-type has  no  parent:  we  just evaluate  the  protocol
	   ;;function with no arguments.
	   `(let ((,proto-func.sym ,?proto-expr))
	      (receive-and-return (,compar-func.sym)
		  (,proto-func.sym)
		(unless (procedure? ,compar-func.sym)
		  (assertion-violation (quote ,foo)
		    "expected closure object from evaluation of comparison procedure's protocol function"
		    ,compar-func.sym)))))))

      ;;No matching clause found.
      (#f	#f)

      (_
       (synner "invalid syntax in COMPARISON-PROCEDURE clause" clause)))))


(define (%make-hash-function-code clause* foo parent-rtd synner)
  ;;Extract from  the definition clauses CLAUSE*  the HASH-FUNCTION one and  return a
  ;;symbolic expression (to be BLESSed later) representing a Scheme expression which,
  ;;expanded and evaluated at run-time, will return the hash function and register it
  ;;in the RTD.  Return false if there is no HASH-FUNCTION clause.
  ;;
  (let ((clause (%get-clause 'hash-function clause*)))
    (syntax-match clause ()
      ((_ ?proto-expr)
       (let ((proto-func.sym	(make-syntactic-identifier-for-temporary-variable "proto-func"))
	     (hash-func.sym	(make-syntactic-identifier-for-temporary-variable "hash-func")))
	 (if parent-rtd
	     ;;The new  record-type has a parent:  we apply the protocol  function to
	     ;;the parent's comparison procedure.
	     `(let ((,proto-func.sym ,?proto-expr))
		(receive-and-return (,hash-func.sym)
		    (,proto-func.sym ($record-type-hash-function ,parent-rtd))
		  (unless (procedure? ,hash-func.sym)
		    (assertion-violation (quote ,foo)
		      "expected closure object from evaluation of hash function's protocol function"
		      ,hash-func.sym))))
	   ;;The  new  record-type has  no  parent:  we  just evaluate  the  protocol
	   ;;function with no arguments.
	   `(let ((,proto-func.sym ,?proto-expr))
	      (receive-and-return (,hash-func.sym)
		  (,proto-func.sym)
		(unless (procedure? ,hash-func.sym)
		  (assertion-violation (quote ,foo)
		    "expected closure object from evaluation of hash function's protocol function"
		    ,hash-func.sym)))))))

      ;;No matching clause found.
      (#f	#f)

      (_
       (synner "invalid syntax in HASH-FUNCTION clause" clause)))))


(define (%make-methods-retriever-code foo field-name*.sym field-method* methods-late-binding-alist)
  ;;Return false  or a  symbolic expression  (to be  BLESSed later)  representing the
  ;;Scheme definition of the methods-retriever function: a LAMBDA syntax use.
  ;;
  ;;The methods retriever function is used when performing late binding of methods.
  ;;
  (if (or (pair? methods-late-binding-alist)
	  (pair? field-name*.sym))
      (let ((method-name.sym (make-syntactic-identifier-for-temporary-variable "method-name")))
	`(lambda/typed ({_ (or <false> <procedure>)} {,method-name.sym <symbol>})
	   (case ,method-name.sym
	     ;;First the methods...
	     ,@(map (lambda (P)
		      (let ((name	(car P))
			    (procname	(cdr P)))
			`((,name) ,procname)))
		 methods-late-binding-alist)
	     ;;... then the fields, so that the methods will be selected first.
	     ,@(map (lambda (name procname)
		      `((,name) ,procname))
		 field-name*.sym field-method*)
	     (else #f))))
    #f))


(define* (%make-type-name-syntactic-binding-form foo.id foo-uid make-foo.id foo?.id
						 foo-super-rcd.sym foo-destructor.sym
						 foo-parent.id foo-rtd.sym foo-rcd.sym
						 foo-equality-predicate.id
						 foo-comparison-procedure.id
						 foo-hash-function.id
						 method-name*.sym method-procname*.id
						 implemented-interface*.id)
  ;;Build and return symbolic expression (to  be BLESSed later) representing a Scheme
  ;;expression which, expanded and evaluated  at expand-time, returns the record-type
  ;;name's syntactic binding's descriptor.
  ;;
  ;;FOO.ID must be the identifier bound to the type name.
  ;;
  ;;FOO-UID must be false or the UID of this record type.
  ;;
  ;;MAKE-FOO.ID must be the identifier bound to the default constructor function.
  ;;
  ;;FOO-SUPER-RCD.SYM must be false if this record-type has no super-type constructor
  ;;descriptor; otherwise it must be a  symbol representing the name of the syntactic
  ;;identifier to which the super-RCD is bound.
  ;;
  ;;FOO-DESTRUCTOR.SYM must be  false if this record-type has  no default destructor;
  ;;otherwise it must  be a symbol representing the name  of the syntactic identifier
  ;;to which the destructor function is bound.
  ;;
  ;;FOO?.ID must be the identifier bound to the type predicate.
  ;;
  ;;FOO-PARENT.ID must be false or the identifier bound to the parent's type name.
  ;;
  ;;FOO-RTD.SYM must be a  gensym: it will become the name  of the identifier bound
  ;;to the record-type descriptor.
  ;;
  ;;FOO-RTD.SYM must be a  gensym: it will become the name  of the identifier bound
  ;;to the record-constructor descriptor.
  ;;
  ;;METHOD-NAME*.SYM must be null or a list of symbols representing the method names.
  ;;
  ;;METHOD-PROCNAME*.ID  must be  null or  list  of identifiers  bound to  procedures
  ;;implementing the methods.
  ;;
  ;;IMPLEMENTED-INTERFACE*.ID  null  or  a   proper  list  of  syntactic  identifiers
  ;;representing the interfaces that this record-type implements.
  ;;
  (define (%make-alist-from-ids field-name*.id operator*.id)
    ;;We want to return a  symbolic expression representing the following expand-time
    ;;expression:
    ;;
    ;;   (list (cons (quote ?field-sym0) (syntax ?operator0))
    ;;         (cons (quote ?field-sym)  (syntax ?operator))
    ;;         ...)
    ;;
    ;;which evaluates  to an alist  whose keys are field  names and whose  values are
    ;;syntactic identifiers bound  to accessors or mutators.  When  an OPERATOR.ID is
    ;;false, an entry with the following format is generated:
    ;;
    ;;  (cons (quote ?field-sym) #t)
    ;;
    ;;so  an attempt  to call  the mutator  of an  immutable field  can be  correctly
    ;;detected and reported (with a meaningful error message).
    ;;
    (cons 'list (fold-right
		    (lambda (key.id operator.id knil)
		      (cons (list 'cons
				  `(quote ,(syntax->datum key.id))
				  (if operator.id
				      `(syntax ,operator.id)
				    #t))
			    knil))
		  '() field-name*.id operator*.id)))

  (define foo-methods.table
    (%make-alist-from-ids method-name*.sym method-procname*.id))

  (define implemented-interfaces
    (if (null? implemented-interface*.id)
	'(quote ())
      `(list ,@(map (lambda (iface.id)
		      `(syntax ,iface.id))
		 implemented-interface*.id))))

  `(make-record-type-spec (syntax ,foo.id)
			  (quote ,foo-uid)
			  (syntax ,foo-rtd.sym)
			  (syntax ,foo-rcd.sym)
			  ,(and foo-super-rcd.sym `(syntax ,foo-super-rcd.sym))
			  ,(if foo-parent.id
			       `(syntax ,foo-parent.id)
			     `(syntax <record>))
			  (syntax ,make-foo.id)
			  ,(and foo-destructor.sym `(syntax ,foo-destructor.sym))
			  (syntax ,foo?.id)
			  (syntax ,foo-equality-predicate.id)
			  (syntax ,foo-comparison-procedure.id)
			  (syntax ,foo-hash-function.id)
			  ,foo-methods.table
			  ,implemented-interfaces))


(module (%make-implemented-interfaces-table-code)

  (define (%make-implemented-interfaces-table-code foo implemented-interface*.id)
    ;;Return false  or a symbolic  expression (to  be blessed later)  representing an
    ;;expression  which,   expanded  and  evaluated,  will   return  the  record-type
    ;;implemented interfaces table.
    ;;
    ;;This expression  must return  a vector  have one  item for  each interface-type
    ;;implemented by this record-type, with format:
    ;;
    ;;   #((?interface-name . ?method-retriever) ...)
    ;;
    ;;where: ?INTERFACE-NAME is  the UID of the  interface-type; ?METHOD-RETRIEVER is
    ;;the method retriever function for  the method implementation procedures defined
    ;;by this record-type.
    ;;
    ;;The core primitive BUILD-TABLE-FOR-INTERFACE-AND-COMPLIANT-OBJECT-TYPE verifies
    ;;that the record-type actually  implements the interface-type.  When successful:
    ;;it returns an alist having:  as keys, symbols representing the interface-type's
    ;;method names; as  values, the syntactic identifiers bound  to the object-type's
    ;;method implementation procedures.  Otherwise it raises an exception.
    ;;
    #f
    #;(cond ((null? implemented-interface*.id)
	   '(quote ()))
	  ((list-of-single-item? implemented-interface*.id)
	   (let ((iface.id (car implemented-interface*.id)))
	     `(quasisyntax (vector (unsyntax ,(%compose-interfaces-table-entry-code foo iface.id))))))
	  (else
	   (let ((iface.id	(make-syntactic-identifier-for-temporary-variable "iface"))
		 (knil.id	(make-syntactic-identifier-for-temporary-variable "knil")))
	     `(quasisyntax
	       (vector (unsyntax-splicing
			(fold-left (lambda (,knil.id ,iface.id)
				     (cons ,(%compose-interfaces-table-entry-code foo iface.id)
					   ,knil.id))
			  '() (list . ,(map (lambda (id)
					      `(syntax ,id))
					 implemented-interface*.id))))))))))

  (define (%compose-interfaces-table-entry-code foo iface.id)
    (let ((methods-table.id	(make-syntactic-identifier-for-temporary-variable "methods-table"))
	  (method-retriever.id	(make-syntactic-identifier-for-temporary-variable "method-retriever"))
	  (interface-uid.id	(make-syntactic-identifier-for-temporary-variable "interface-uid"))
	  (method-name.id	(make-syntactic-identifier-for-temporary-variable "method-name"))
	  (method-procname.id	(make-syntactic-identifier-for-temporary-variable "method-procname"))
	  (entry.id		(make-syntactic-identifier-for-temporary-variable "entry")))
      `(let* ((,interface-uid.id    (car (type-unique-identifiers ,iface.id)))
	      (,methods-table.id    (build-table-for-interface-and-compliant-object-type ,iface.id ,foo))
	      (,method-retriever.id (quasisyntax
				     (lambda (,method-name.id)
				       (case ,method-name.id
					 (unsyntax-splicing
					  (map (lambda (,entry.id)
						 (let ((,method-name.id		(car ,entry.id))
						       (,method-procname.id	(cdr ,entry.id)))
						   (quasisyntax (((unsyntax ,method-name.id)) (unsyntax ,method-procname.id)))))
					    ,methods-table.id))
					 (else #f))))))
	 (cons (unsyntax ,interface-uid.id) (unsyntax ,method-retriever.id)))))

  #| end of module: %MAKE-IMPLEMENTED-INTERFACES-TABLE-CODE |# )


;;;; done

#| end of module: DEFINE-RECORD-TYPE-MACRO |# )


;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
