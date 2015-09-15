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


(module (define-record-type-macro)
  ;;Transformer function used to expand R6RS's DEFINE-RECORD-TYPE macros
  ;;from the  top-level built  in environment.   Expand the  contents of
  ;;INPUT-FORM.STX; return a syntax object that must be further expanded.
  ;;
  (define-constant __module_who__ 'define-record-type)


;;;; helpers

(define (%named-gensym/suffix foo suffix)
  (gensym (string-append (symbol->string (syntax->datum foo)) suffix)))

(define (%named-gensym/prefix foo prefix)
  (gensym (string-append prefix (symbol->string (syntax->datum foo)))))

(define (%filter-out-falses ls)
  (if (pair? ls)
      (let ((A (car ls))
	    (D (cdr ls)))
	(if A
	    (cons A (%filter-out-falses D))
	  (%filter-out-falses D)))
    '()))


(define (define-record-type-macro input-form.stx)
  (case-define synner
    ((message)
     (synner message #f))
    ((message subform)
     (syntax-violation __module_who__ message input-form.stx subform)))
  (syntax-match input-form.stx ()
    ((_ ?namespec ?clause* ...)
     (begin
       (%validate-definition-clauses ?clause* synner)
       (%do-define-record input-form.stx ?namespec ?clause* synner)))
    ))


(define (%do-define-record input-form.stx namespec clause* synner)
  (define-values (foo make-foo foo?)
    (%parse-full-name-spec namespec))
  (define foo-rtd			(%named-gensym/suffix foo "-rtd"))
  (define foo-rcd			(%named-gensym/suffix foo "-rcd"))
  (define foo-constructor-protocol	(%named-gensym/suffix foo "-constructor-protocol"))
  (define foo-custom-printer		(%named-gensym/suffix foo "-custom-printer"))
  (define-values
    (field-name*.sym
		;A list of symbols representing all the field names.
     field-relative-idx*
		;A  list  of fixnums  representing  all  the field  relative  indexes
		;(zero-based).
     field-type*.id
		;The list of syntactic identifiers representing the field types.
     safe-field-accessor*
		;A  list of  syntactic identifiers  that will  be bound  to the  safe
		;accessors.
     unsafe-field-accessor*
		;A list  of syntactic identifiers  that will  be bound to  the unsafe
		;accessors.
     safe-field-mutator*
		;A  list of  syntactic identifiers  that will  be bound  to the  safe
		;mutators.  This list holds #f in the position of immutable fields.
     unsafe-field-mutator*
		;A list  of syntactic identifiers  that will  be bound to  the unsafe
		;mutators.  This list holds #f in the position of immutable fields.
     safe-field-method*
		;A list of syntactic identifiers that will be bound to the safe field
		;accessor/mutator methods.
     fields-vector-spec
		;A  vector to  be  used as  FIELDS argument  for  the core  primitive
		;function MAKE-RECORD-TYPE-DESCRIPTOR.
     )
    (%parse-field-specs foo clause* synner))

  ;;Code  for  parent  record-type  descriptor  and  parent  record-type  constructor
  ;;descriptor retrieval.
  ;;
  ;;FOO-PARENT: an identifier  representing the parent type, or false  if there is no
  ;;parent or the parent is specified through the procedural layer.
  ;;
  ;;PARENT-RTD: false  if this record-type  has no parent;  otherwise it is  a symbol
  ;;representing  the  name  of  the  syntactic  identifier  bound  to  the  parent's
  ;;record-type descriptor.
  ;;
  ;;PARENT-RCD: false  if this record-type  has no parent;  otherwise it is  a symbol
  ;;representing  the  name  of  the  syntactic  identifier  bound  to  the  parent's
  ;;record-constructor descriptor.
  ;;
  ;;PARENT-RTD-CODE: false or a symbolic expression representing an expression which,
  ;;expanded  and  evaluated   at  run-time,  will  return   the  parent  record-type
  ;;descriptor.
  ;;
  ;;PARENT-RCD-CODE: false or a symbolic  expression representing a Scheme expression
  ;;which, expanded  and evaluated  at run-time, will  return the  parent record-type
  ;;default constructor descriptor.
  (define-values (foo-parent parent-rtd parent-rcd parent-rtd-code parent-rcd-code)
    (%make-parent-rtd+rcd-code clause* foo input-form.stx synner))

  ;;This can be  a symbol or false.  When  a symbol: the symbol is  the record type
  ;;UID, which will make this record  type non-generative.  When false: this record
  ;;type is generative.
  (define foo-uid
    (%get-uid foo clause* synner))

  ;;False or code to build at run-time the record-type descriptor.
  (define foo-rtd-code
    (%make-rtd-code foo foo-uid clause* parent-rtd fields-vector-spec synner))

  ;;False or code to build at run-time the record-constructor descriptor.
  (define foo-rcd-code
    (%make-rcd-code clause* foo-rtd foo-constructor-protocol parent-rcd))

  ;;False or code to build at run-time the super-type record-constructor descriptor.
  (define foo-super-rcd-code
    (%make-super-rcd-code clause* foo foo-rtd foo-parent parent-rcd synner))

  ;;If this  record-type has no  parent or no  SUPER-PROTOCOL clause: this  is false;
  ;;otherwise it is a symbol representing  the name of the syntactic identifier bound
  ;;to the super-type record-constructor descriptor.
  (define foo-super-protocol
    (and foo-super-rcd-code
	 (%named-gensym/suffix foo "-super-protocol")))

  ;;False or code to build at run-time the record destructor function.
  (define foo-destructor-code
    (%make-destructor-code clause* foo foo-rtd foo-parent parent-rtd synner))

  ;;If this record-type  has no destructor: this  is false; otherwise it  is a symbol
  ;;representing  the  name of  the  syntactic  identifier  bound to  the  destructor
  ;;function.
  (define foo-destructor
    (and foo-destructor-code
	 (%named-gensym/suffix foo "-destructor")))

  ;;Code for protocol.
  (define constructor-protocol-code
    (%get-constructor-protocol-code clause* synner))

  ;;Code  for custom  printer.  False  or  a form  evaluating to  the custom  printer
  ;;function.
  (define foo-custom-printer-code
    (%make-custom-printer-code clause* foo foo-rtd synner))

  ;;Definition forms.
  ;;
  (begin
    ;;This is  null if there is  no parent; otherwise it  is a list holding  a DEFINE
    ;;form defining  the parent  RTD syntactic  binding, the list  is spliced  in the
    ;;output.
    (define parent-rtd-definition
      (if parent-rtd
	  `((define ,parent-rtd ,parent-rtd-code))
	'()))
    ;;This is  null if there is  no parent; otherwise it  is a list holding  a DEFINE
    ;;form defining  the parent  RCD syntactic  binding, the list  is spliced  in the
    ;;output.
    (define parent-rcd-definition
      (if parent-rcd
	  `((define ,parent-rcd ,parent-rcd-code))
	'()))
    ;;This is null if  there is no supertype constructor protocol;  otherwise it is a
    ;;list holding a DEFINE form defining the supertype record-constructor descriptor
    ;;syntactic binding, the list is spliced in the output.
    (define super-rcd-definition
      (if foo-super-protocol
	  `((define ,foo-super-protocol ,foo-super-rcd-code))
	'()))
    ;;This is null if there is no destructor; otherwise it is a list holding a DEFINE
    ;;form  defining the  default destructor  function, the  list is  spliced in  the
    ;;output.
    (define foo-destructor-definition
      (if foo-destructor
	  `((define ,foo-destructor ,foo-destructor-code))
	'()))
    ;;This is null  if there is no custom  printer; otherwise it is a  list holding a
    ;;DEFINE form  defining the custom printer  function, the list is  spliced in the
    ;;output.
    (define foo-printer-definition
      (if foo-custom-printer-code
	  `((define ,foo-custom-printer ,foo-custom-printer-code))
	'())))

  ;;Code for methods.
  (define-values (method-name*.sym method-procname*.sym method-form*.sexp)
    (%parse-method-clauses clause* foo synner))

  ;;A  symbolic expression  (to be  BLESSed later)  representing a  Scheme expression
  ;;which, expanded and evaluated at run-time, builds a hashtable of methods for this
  ;;record-type  and  registers  it  as  property  in  the  record-type's  UID.   The
  ;;expression must be spliced in the output form.
  ;;
  (define methods-late-binding-table.sexp
    (if (or (pair? method-name*.sym)
	    (pair? field-name*.sym))
	(let ((table.sym (gensym)))
	  `((putprop (record-type-uid ,foo-rtd)
		     'late-binding-methods-table
		     (receive-and-return (,table.sym)
			 (make-eq-hashtable)
		       ;;First the fields...
		       ,@(map (lambda (name procname)
				`(hashtable-set! ,table.sym (quote ,name) ,procname))
			   field-name*.sym safe-field-method*)
		       ;;... then the method, so that the methods will override the
		       ;;symbols in the table.
		       ,@(map (lambda (name procname)
				`(hashtable-set! ,table.sym (quote ,name) ,procname))
			   method-name*.sym method-procname*.sym)))))
      '()))

  ;;A  symbolic expression  representing  a  form which,  expanded  and evaluated  at
  ;;expand-time, returns the right-hand side of the record-type name's DEFINE-SYNTAX.
  ;;The value of the right-hand side is the syntactic binding's descriptor.
  (define foo-syntactic-binding-form
    (%make-type-name-syntactic-binding-form foo make-foo foo? foo-super-protocol foo-destructor
					    foo-parent foo-rtd foo-rcd
					    field-name*.sym field-relative-idx*
					    safe-field-accessor* unsafe-field-accessor*
					    safe-field-mutator*  unsafe-field-mutator*
					    field-type*.id
					    method-name*.sym method-procname*.sym))

  (define tag-type-spec-form
    ;;The  tag-type-spec stuff  is used  to add  a tag  property to  the record  type
    ;;identifier.
    (%make-tag-type-spec-form foo make-foo foo? foo-parent
			      field-name*.sym
			      safe-field-accessor* unsafe-field-accessor*
			      safe-field-mutator* unsafe-field-mutator*
			      input-form.stx))

  (bless
   `(begin
      ;;Parent record-type descriptor.
      ,@parent-rtd-definition
      ;;Parent record-constructor descriptor.
      ,@parent-rcd-definition
      ;;Record-type descriptor.
      (define ,foo-rtd ,foo-rtd-code)
      ;;Protocol function.
      (define ,foo-constructor-protocol ,constructor-protocol-code)
      ;;Record-constructor descriptor.
      (define ,foo-rcd ,foo-rcd-code)
      ;;Super-type record-constructor descriptor.
      ,@super-rcd-definition
      ;;Record destructor function.
      ,@foo-destructor-definition
      ;;Record printer function.
      ,@foo-printer-definition
      ;;Syntactic binding for record-type name.
      (define-syntax ,foo
	,foo-syntactic-binding-form)
      (begin-for-syntax ,tag-type-spec-form)
      ;;Type predicate.
      (define (brace ,foo? <predicate>)
	(record-predicate ,foo-rtd))
      ;;Default constructor.
      (define ,make-foo
	(record-constructor ,foo-rcd))
      ;;Methods.
      ,@method-form*.sexp
      ;;We want the  default constructor function to have a  signature specifying the
      ;;record-type as return type.
      (begin-for-syntax
	(internal-body
	  (import (prefix (vicare expander tag-type-specs) typ.))
	  (define %constructor-signature
	    (typ.make-lambda-signature (typ.make-retvals-signature-single-value (syntax ,foo))
				       (typ.make-formals-signature (syntax <list>))))
	  (define %constructor-tag-id
	    (typ.fabricate-procedure-tag-identifier (quote ,make-foo) %constructor-signature))
	  (typ.override-identifier-tag! (syntax ,make-foo) %constructor-tag-id)))

      ;;When there are  no fields: this form  expands to "(module ())"  which is just
      ;;wiped away with a further expansion.
      (module (,@safe-field-accessor*
	       ,@(%filter-out-falses safe-field-mutator*)
	       ;;We want  to create  the syntactic bindings  of unsafe  accessors and
	       ;;mutators only when STRICT-R6RS mode is DISabled.
	       ,@(if (option.strict-r6rs)
		     '()
		   (append unsafe-field-accessor* (%filter-out-falses unsafe-field-mutator*))))

	,@(%make-unsafe-accessor+mutator-code foo foo-rtd
					      field-name*.sym field-relative-idx* field-type*.id
					      unsafe-field-accessor* unsafe-field-mutator*)

	,@(%make-safe-accessor+mutator-code foo
					    field-name*.sym field-relative-idx* field-type*.id
					    safe-field-accessor* unsafe-field-accessor*
					    safe-field-mutator*  unsafe-field-mutator*)

	,@(%make-safe-method-code foo field-type*.id
				  safe-field-method* unsafe-field-accessor* unsafe-field-mutator*)

	,@methods-late-binding-table.sexp

	#| end of module: safe and unsafe accessors and mutators |# )
      )))


(module (%validate-definition-clauses)

  (define (%validate-definition-clauses clause* synner)
    (define-constant VALID-KEYWORDS
      (if (option.strict-r6rs)
	  (%r6rs-valid-keywords)
	(%extended-valid-keywords)))
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

  (define %r6rs-valid-keywords
    ;;Return a  list of  syntactic identifiers representing  the keywords  of clauses
    ;;accepted by DEFINE-RECORD-TYPE under the strict R6RS language.
    ;;
    (let ((cached #f))
      (lambda ()
	(or cached
	    (receive-and-return (rv)
		(map core-prim-id
		  '(fields parent parent-rtd protocol sealed opaque nongenerative))
	      (set! cached rv))))))

  (define %extended-valid-keywords
    ;;Return a  list of  syntactic identifiers representing  the keywords  of clauses
    ;;accepted by DEFINE-RECORD-TYPE under the non-strict R6RS language.
    ;;
    (let ((cached #f))
      (lambda ()
	(or cached
	    (receive-and-return (rv)
		(append (%r6rs-valid-keywords)
			(map core-prim-id
			  '(destructor-protocol custom-printer method case-method super-protocol)))
	      (set! cached rv))))))

  (define keyword-allowed-multiple-times?
    (let ((cached #f))
      (lambda (id)
	(free-id-member? id (or cached
				(receive-and-return (rv)
				    (map core-prim-id
				      '(method case-method fields))
				  (set! cached rv)))))))

  #| end of module: %VALIDATE-DEFINITION-CLAUSES |# )


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

(define (%parse-full-name-spec spec)
  ;;Given a syntax object representing  a full record-type name specification: return
  ;;the 3 syntactic  identifiers: the type name, the constructor  name, the predicate
  ;;name.
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
      ;;No matching clause found.  This record type will be non-generative.
      (#f
       #f)
      (_
       (synner "expected symbol or no argument in nongenerative clause" clause)))))

(define (%get-constructor-protocol-code clause* synner)
  ;;Return a sexp which, when evaluated, returns the protocol function.
  ;;
  (let ((clause (%get-clause 'protocol clause*)))
    (syntax-match clause ()
      ((_ ?expr)
       ?expr)

      ;;No matching clause found.
      (#f	#f)

      (_
       (synner "invalid syntax in PROTOCOL clause" clause)))))


(define (%parse-field-specs type-id clause* synner)
  ;;Given the definition  clauses CLAUSE* extract the FIELDS clauses  and parse them.
  ;;Return the following values:
  ;;
  ;;1..The list of symbols representing all the field names.
  ;;
  ;;2..The list of fixnums representings all the field relative indexes (zero-based).
  ;;
  ;;3..The list of syntactic identifiers representing the field types.
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
	 (if (option.strict-r6rs)
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
    (identifier-append type-id type-id "-" field-name.id))
  (define (%gen-unsafe-accessor-name field-name.id)
    (identifier-append type-id "$" type-id "-" field-name.id))
  (define (%gen-safe-mutator-name field-name.id)
    (identifier-append type-id type-id "-" field-name.id "-set!"))
  (define (%gen-unsafe-mutator-name field-name.id)
    (identifier-append type-id "$" type-id "-" field-name.id "-set!"))
  (define (%gen-safe-method-name field-name.id)
    (identifier-append type-id type-id "-" field-name.id "-method"))

  (let loop ((fields-clause*			fields-clause*)
	     (i					0)
	     (field-name*.sym			'())
	     (field-relative-idx*		'())
	     (field-type*.id			'())
	     (safe-field-accessor*		'())
	     (unsafe-field-accessor*		'())
	     (safe-field-mutator*		'())
	     (unsafe-field-mutator*		'())
	     (safe-field-method*		'())
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
	       (reverse field-type*.id)
	       (reverse safe-field-accessor*)
	       (reverse unsafe-field-accessor*)
	       (reverse safe-field-mutator*)
	       (reverse unsafe-field-mutator*)
	       (reverse safe-field-method*)
	       (list->vector (reverse fields-vector-spec*))))

      (((mutable   ?name ?accessor ?mutator) . ?rest)
       (and (identifier? ?accessor)
	    (identifier? ?mutator))
       (receive (field-name.id field-type.id)
	   (parse-tagged-identifier-syntax ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.id field-type*.id)
	       (cons ?accessor safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons ?mutator safe-field-mutator*)
	       (cons (%gen-unsafe-mutator-name  field-name.id) unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) safe-field-method*)
	       (cons `(mutable ,field-name.id) fields-vector-spec*))))

      (((immutable ?name ?accessor) . ?rest)
       (identifier? ?accessor)
       (receive (field-name.id field-type.id)
	   (parse-tagged-identifier-syntax ?name)
	 (loop ?rest (fxadd1 i)
	       (cons ?name field-name*.sym)
	       (cons i field-relative-idx*)
	       (cons field-type.id field-type*.id)
	       (cons ?accessor safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name ?name) unsafe-field-accessor*)
	       (cons #f safe-field-mutator*)
	       (cons #f unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) safe-field-method*)
	       (cons `(immutable ,field-name.id) fields-vector-spec*))))

      (((mutable   ?name) . ?rest)
       (receive (field-name.id field-type.id)
	   (parse-tagged-identifier-syntax ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.id field-type*.id)
	       (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons (%gen-safe-mutator-name    field-name.id) safe-field-mutator*)
	       (cons (%gen-unsafe-mutator-name  field-name.id) unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) safe-field-method*)
	       (cons `(mutable ,field-name.id) fields-vector-spec*))))

      (((immutable ?name) . ?rest)
       (receive (field-name.id field-type.id)
	   (parse-tagged-identifier-syntax ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.id field-type*.id)
	       (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons #f safe-field-mutator*)
	       (cons #f unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) safe-field-method*)
	       (cons `(immutable ,field-name.id) fields-vector-spec*))))

      ((?name . ?rest)
       (receive (field-name.id field-type.id)
	   (parse-tagged-identifier-syntax ?name)
	 (loop ?rest (fxadd1 i)
	       (%register-field-name field-name.id)
	       (cons i field-relative-idx*)
	       (cons field-type.id field-type*.id)
	       (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
	       (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
	       (cons #f safe-field-mutator*)
	       (cons #f unsafe-field-mutator*)
	       (cons (%gen-safe-method-name     field-name.id) safe-field-method*)
	       (cons `(immutable ,field-name.id) fields-vector-spec*))))

      ((?spec . ?rest)
       (synner "invalid field specification in DEFINE-RECORD-TYPE syntax" ?spec)))))


(define (%parse-method-clauses clause* foo synner)
  ;;Return two  values: a list  of symbols representing the  method names; a  list of
  ;;symbolic  expressions  (to  be  BLESSed later)  representing  expressions  which,
  ;;expanded and evaluated at run-time, define the method procedures.
  ;;
  (define-syntax-rule (recurse ?clause*)
    (%parse-method-clauses ?clause* foo synner))
  (syntax-match clause* ()
    (()
     (values '() '() '()))

    (((?method (?who . ?args) . ?body) . ?clause*)
     (and (method-id? ?method)
	  (identifier? ?who))
     (receive (method-name*.sym method-procname*.sym method-form*.sexp)
	 (recurse ?clause*)
       (let* ((name.sym		(identifier->symbol ?who))
	      (procname.sym	(%named-gensym/suffix foo (string-append "-" (symbol->string name.sym))))
	      (form.sexp	`(define (,procname.sym . ,?args) . ,?body)))
	 (if (memq name.sym method-name*.sym)
	     (synner "multiple method definitions with the same name" ?who)
	   (values (cons name.sym	method-name*.sym)
		   (cons procname.sym	method-procname*.sym)
		   (cons form.sexp	method-form*.sexp))))))

    (((?method ((?brace ?who ?rv-tag0 . ?rv-tag*) . ?args) . ?body) . ?clause*)
     (and (method-id? ?method)
	  (brace-id?  ?brace)
	  (identifier? ?who))
     (receive (method-name*.sym method-procname*.sym method-form*.sexp)
	 (recurse ?clause*)
       (let* ((name.sym		(identifier->symbol ?who))
	      (procname.sym	(%named-gensym/suffix foo (string-append "-" (symbol->string name.sym))))
	      (form.sexp	`(define ((brace ,procname.sym ,?rv-tag0 . ,?rv-tag*) . ,?args) . ,?body)))
	 (if (memq name.sym method-name*.sym)
	     (synner "multiple method definitions with the same name" ?who)
	   (values (cons name.sym	method-name*.sym)
		   (cons procname.sym	method-procname*.sym)
		   (cons form.sexp	method-form*.sexp))))))

    (((?method . ?wrong-stuff) . ?clause*)
     (method-id? ?method)
     (synner "invalid syntax in METHOD clause" (cons ?method ?wrong-stuff)))

    ;; ------------------------------------------------------------

    (((?case-method ?who . ?stuff) . ?clause*)
     (and (case-method-id? ?case-method)
	  (identifier? ?who))
     (receive (method-name*.sym method-procname*.sym method-form*.sexp)
	 (recurse ?clause*)
       (let* ((name.sym		(identifier->symbol ?who))
	      (procname.sym	(%named-gensym/suffix foo (string-append "-" (symbol->string name.sym))))
	      (form.sexp	`(case-define ,procname.sym . ,?stuff)))
	 (if (memq name.sym method-name*.sym)
	     (synner "multiple method definitions with the same name" ?who)
	   (values (cons name.sym	method-name*.sym)
		   (cons procname.sym	method-procname*.sym)
		   (cons form.sexp	method-form*.sexp))))))

    (((?case-method . ?wrong-stuff) . ?clause*)
     (case-method-id? ?case-method)
     (synner "invalid syntax in METHOD clause" (cons ?case-method ?wrong-stuff)))

    ;; ------------------------------------------------------------

    ((_ . ?clause*)
     (recurse ?clause*))
    ))


(define (%make-unsafe-accessor+mutator-code foo foo-rtd
					    field-name*.sym field-relative-idx* field-type*.id
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
  ;;FIELD-TYPE*.ID must be a list of syntactic identifiers representing all the field
  ;;types.
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
	(define ,foo-first-field-offset
	  ;;The field at index  3 in the RTD is: the index of  the first field of this
	  ;;subtype in the  layout of instances; it  is the total number  of fields of
	  ;;the parent type.
	  ($struct-ref ,foo-rtd 3))

	;;all fields indexes
	,@(map (lambda (x idx)
		 (let ((the-index (%make-field-index-varname x)))
		   `(define (brace ,the-index <fixnum>)
		      (fx+ ,idx ,foo-first-field-offset))))
	    field-name*.sym field-relative-idx*)

	;;unsafe record fields accessors
	,@(map (lambda (unsafe-foo-x x field-type.id)
		 (let ((the-index (%make-field-index-varname x)))
		   `(define-syntax-rule (,unsafe-foo-x ?x)
		      (tag-unsafe-cast ,field-type.id ($struct-ref ?x ,the-index)))))
	    unsafe-field-accessor* field-name*.sym field-type*.id)

	;;unsafe record fields mutators
	,@(fold-right
	      (lambda (unsafe-field-mutator field-name.sym knil)
		(if unsafe-field-mutator
		    (cons (let ((the-index (%make-field-index-varname field-name.sym)))
			    `(define-syntax-rule (,unsafe-field-mutator ?x ?v)
			       ($struct-set! ?x ,the-index ?v)))
			  knil)
		  knil))
	    '() unsafe-field-mutator* field-name*.sym)

	#| end of module: unsafe accessors and mutators |# ))))


(define (%make-safe-accessor+mutator-code foo
					  field-name*.sym field-relative-idx* field-type*.id
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
  ;;FIELD-TYPE*.ID must  be the  list of syntactic  identifiers representing  all the
  ;;field types.
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
    (map (lambda (safe-field-accessor unsafe-field-accessor field-type.id)
	   `(begin
	      (internal-define (safe) ((brace ,safe-field-accessor ,field-type.id) (brace record ,foo))
		(,unsafe-field-accessor record))
	      (begin-for-syntax
		(set-identifier-unsafe-variant! (syntax ,safe-field-accessor)
		  (syntax (lambda (record)
			    (,unsafe-field-accessor record)))))))
      safe-field-accessor* unsafe-field-accessor* field-type*.id))

  (define safe-field-mutator-form*
    (fold-right
	(lambda (safe-field-mutator unsafe-field-mutator field-type.id knil)
	  (if safe-field-mutator
	      (cons `(begin
		       (internal-define (safe) ((brace ,safe-field-mutator <void>) (brace record ,foo) (brace new-value ,field-type.id))
			 (,unsafe-field-mutator record new-value))
		       (begin-for-syntax
			 (set-identifier-unsafe-variant! (syntax ,safe-field-mutator)
			   (syntax (lambda (record new-value)
				     (,unsafe-field-mutator record new-value))))))
		    knil)
	    knil))
      '() safe-field-mutator* unsafe-field-mutator* field-type*.id))

  (if (and (null? safe-field-accessor-form*)
	   (null? safe-field-mutator-form*))
      '()
    (append safe-field-accessor-form* safe-field-mutator-form*)))


(define (%make-safe-method-code foo
				field-type*.id
				safe-field-method* unsafe-field-accessor* unsafe-field-mutator*)
  ;;Return a list  holding a symbolic expressions (to be  BLESSed later) representing
  ;;Scheme  expressions  which,  expanded  and  evaluated  at  run-time,  define  the
  ;;syntactic bindings of the  safe field methods.  The returned list  is meant to be
  ;;spliced in the output form.  The field methods act as both accessors (for all the
  ;;fields) and mutators (for mutable fields).
  ;;
  ;;FOO must be the syntactic identifier represening the record-type name.
  ;;
  ;;FIELD-TYPE*.ID must  be the  list of syntactic  identifiers representing  all the
  ;;field types.
  ;;
  ;;SAFE-FIELD-METHOD*  must be  a  list of  symbols representing  the  names of  the
  ;;syntactic identifiers the will be bound to the safe field methods.
  ;;
  ;;UNSAFE-FIELD-ACCESSOR*  and   UNSAFE-FIELD-MUTATOR*  must  be  list   of  symbols
  ;;representing the  names of the  syntactic identifiers  bound to the  unsafe field
  ;;accessors and mutators.
  ;;
  (map (lambda (safe-field-method unsafe-field-accessor unsafe-field-mutator field-type.id)
	 (if unsafe-field-mutator
	     `(case-define ,safe-field-method
		(((brace _ ,field-type.id) (brace record ,foo))
		 (,unsafe-field-accessor record))
		(((brace _ <void>) (brace record ,foo) (brace new-value ,field-type.id))
		 (,unsafe-field-mutator record new-value)))
	   `(define ((brace ,safe-field-method ,field-type.id) (brace record ,foo))
	      (,unsafe-field-accessor record))))
    safe-field-method* unsafe-field-accessor* unsafe-field-mutator* field-type*.id))


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
  ;;4.   PARENT-RTD-CODE:  false of  a  symbolic  expression  (to be  BLESSed  later)
  ;;representing a Scheme expression which,  expanded and evaluated at run-time, will
  ;;return the parent's record-type descriptor.
  ;;
  ;;5.   PARENT-RCD-CODE:  false or  a  symbolic  expression  (to be  BLESSed  later)
  ;;representing a Scheme expression which,  expanded and evaluated at run-time, will
  ;;return the parent record-type default constructor descriptor.
  ;;
  (let ((parent-clause (%get-clause 'parent clause*)))
    (syntax-match parent-clause ()
      ;;If there  is a  PARENT clause  insert code  that retrieves  the RTD  from the
      ;;parent type name.
      ((_ ?parent-name)
       (identifier? ?parent-name)
       (begin
	 (visit-library-of-imported-syntactic-binding __module_who__ input-form.stx ?parent-name (current-inferior-lexenv))
	 ;;Validate ?PARENT-NAME  as syntactic identifier  bound to a  record-type syntactic
	 ;;binding.
	 (let* ((parent-descr (id->record-type-name-binding-descriptor __module_who__ input-form.stx ?parent-name (current-inferior-lexenv)))
		(parent-rts   (syntactic-binding-descriptor.value parent-descr))
		(parent-proto (r6rs-record-type-spec.super-protocol-id parent-rts)))
	   (values ?parent-name
		   (%named-gensym/suffix foo "-parent-rtd")
		   (%named-gensym/suffix foo "-parent-rcd")
		   `(record-type-descriptor ,?parent-name)
		   ;;If the parent  has a super-type constructor  descriptor: use it;
		   ;;otherwise use the default constructor descriptor.
		   (or parent-proto
		       `(record-constructor-descriptor ,?parent-name))))))

      ;;If there is no PARENT clause try to retrieve the expression evaluating to the
      ;;RTD.
      (#f
       (let ((parent-rtd-clause (%get-clause 'parent-rtd clause*)))
	 (syntax-match parent-rtd-clause ()
	   ((_ ?parent-rtd ?parent-rcd)
	    (values #f
		    (%named-gensym/suffix foo "-parent-rtd")
		    (%named-gensym/suffix foo "-parent-rcd")
		    ?parent-rtd ?parent-rcd))

	   ;;If  neither the  PARENT nor  the  PARENT-RTD clauses  are present:  just
	   ;;return false.
	   (#f
	    (values #f #f #f #f #f))

	   (_
	    (synner "invalid syntax in PARENT-RTD clause" parent-rtd-clause)))))

      (_
       (synner "invalid syntax in PARENT clause" parent-clause)))))


(define (%make-rtd-code name foo-uid clause* parent-rtd fields-vector-spec synner)
  ;;Return  a  symbolic  expression  (to  be BLESSed  later)  representing  a  Scheme
  ;;expression  which, expanded  and  evaluated at  run-time,  returns a  record-type
  ;;descriptor.
  ;;
  ;;PARENT-RTD must be false if this record-type  has no parent; otherwise it must be
  ;;a symbol  representing the name of  the syntactic identifier bound  to the parent
  ;;RTD.
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
    `(quote ,fields-vector-spec))

  `(make-record-type-descriptor (quote ,name) ,parent-rtd (quote ,foo-uid) ,sealed? ,opaque? ,fields))


(define (%make-rcd-code clause* foo-rtd foo-constructor-protocol parent-rcd.sym)
  ;;Return  a  symbolic  expression  (to  be BLESSed  later)  representing  a  Scheme
  ;;expression  which,  expanded  and  evaluated at  run-time,  returns  the  default
  ;;record-constructor descriptor.
  ;;
  ;;If this  record-type has no  parent: PARENT-RCD.SYM is  false; otherwise it  is a
  ;;symbol representing  the name of the  syntactic identifier bound to  the parent's
  ;;record-constructor descriptor.
  ;;
  `(make-record-constructor-descriptor ,foo-rtd ,parent-rcd.sym ,foo-constructor-protocol))


(define (%make-super-rcd-code clause* foo foo-rtd foo-parent.id parent-rcd.sym synner)
  ;;Return  a  symbolic  expression  (to  be BLESSed  later)  representing  a  Scheme
  ;;expression  which, expanded  and evaluated  at run-time,  returns the  super-type
  ;;record-constructor descriptor.  If there is  no SUPER-PROTOCOL clause in CLAUSE*:
  ;;return false.
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
	 (let* ((descr (id->record-type-name-binding-descriptor __module_who__ #f foo-parent.id (current-inferior-lexenv)))
		(rts   (syntactic-binding-descriptor.value descr))
		(proto (r6rs-record-type-spec.super-protocol-id rts)))
	   (if proto
	       ;;The parent record-type specification has a super-protocol.
	       `(make-record-constructor-descriptor ,foo-rtd ,proto ,?super-protocol-expr)
	     ;;The parent record-type specification has no super-protocol: let's use
	     ;;the parent's default RCD.
	     `(make-record-constructor-descriptor ,foo-rtd ,parent-rcd.sym ,?super-protocol-expr)))
       ;;This record type has no parent.
       `(make-record-constructor-descriptor ,foo-rtd #f ,?super-protocol-expr)))

    (#f
     ;;No SUPER-PROTOCOL clause in this record-type definition.
     #f)

    (?invalid-clause
     (synner "invalid syntax in SUPER-PROTOCOL clause" ?invalid-clause))))


(define (%make-destructor-code clause* foo foo-rtd foo-parent parent-rtd.sym synner)
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
	    (if (procedure? ,destructor-tmp)
		(record-type-destructor-set! ,foo-rtd ,destructor-tmp)
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
	   (let ((foo-parent-destructor (%named-gensym/suffix foo "-parent-destructor")))
	     `(cond ((record-type-destructor ,parent-rtd.sym)
		     => (lambda (,foo-parent-destructor)
			  (record-type-destructor-set! ,foo-rtd ,foo-parent-destructor)
			  ,foo-parent-destructor))))
	 ;;Set to false this record-type record destructor variable.
	 #f))

      (_
       (synner "invalid syntax in DESTRUCTOR-PROTOCOL clause" clause)))))


(define (%make-custom-printer-code clause* foo foo-rtd synner)
  ;;Extract from the  definition clauses CLAUSE* the CUSTOM-PRINTER one  and return a
  ;;symbolic expression (to be BLESSed) later representing a Scheme expression which,
  ;;expanded and evaluated  at run-time, will return the custom  printer function and
  ;;register it in the RTD.  Return false if there is no CUSTOM-PRINTER clause.
  ;;
  (let ((clause (%get-clause 'custom-printer clause*)))
    (syntax-match clause ()
      ((_ ?expr)
       (let ((printer (%named-gensym/suffix foo "-custom-printer")))
	 `(receive-and-return (,printer)
	      ,?expr
	    (if (procedure? ,printer)
		(record-type-printer-set! ,foo-rtd ,printer)
	      (assertion-violation (quote ,foo)
		"expected closure object from evaluation of expression in CUSTOM-PRINTER clause"
		,printer)))))

      ;;No matching clause found.
      (#f	#f)

      (_
       (synner "invalid syntax in CUSTOM-PRINTER clause" clause)))))


(define* (%make-type-name-syntactic-binding-form foo.id make-foo.id foo?.id
						 foo-super-protocol.sym foo-destructor.sym
						 foo-parent.id foo-rtd.sym foo-rcd.sym
						 field-name*.sym field-relative-idx*
						 safe-field-accessor* unsafe-field-accessor*
						 safe-field-mutator*  unsafe-field-mutator*
						 field-type*.id
						 method-name*.sym method-procname*.sym)
  ;;Build and return symbolic expression (to  be BLESSed later) representing a Scheme
  ;;expression which, expanded and evaluated  at expand-time, returns the record-type
  ;;name's syntactic binding's descriptor.
  ;;
  ;;FOO.ID must be the identifier bound to the type name.
  ;;
  ;;MAKE-FOO.ID must be the identifier bound to the default constructor function.
  ;;
  ;;FOO-SUPER-PROTOCOL.SYM  must  be false  if  this  record-type has  no  super-type
  ;;constructor descriptor;  otherwise it must be  a symbol representing the  name of
  ;;the syntactic identifier to which the super-RCD is bound.
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
  ;;FIELD-NAME*.SYM must be a list of symbols representing all the field names.
  ;;
  ;;FIELD-RELATIVE-IDX* must  be a  list of fixnums  representing the  relative field
  ;;indexes (zero based).
  ;;
  ;;SAFE-FIELD-ACCESSOR*  and  UNSAFE-FIELD-ACCESSOR*  must  be  lists  of  syntactic
  ;;identifiers that will be bound to the field accessors.
  ;;
  ;;SAFE-FIELD-MUTATOR*  and   UNSAFE-FIELD-MUTATOR*  must  be  lists   of  syntactic
  ;;identifiers  that will  be bound  to  the field  mutators.  In  the positions  of
  ;;immutable fields: these lists contain #f.
  ;;
  ;;FIELD-TYPE*.ID a list of syntactic identifiers representing the field types.
  ;;
  ;;METHOD-NAME*.SYM  and  METHOD-PROCNAME*.SYM  must  be null  or  list  of  symbols
  ;;representing: method names, names of procedures implementing the methods.
  ;;
  (define (%make-alist-from-ids field-name*.id operator*.id)
    ;;We want to return a  symbolic expression representing the following expand-time
    ;;expression:
    ;;
    ;;   (list (cons (quote ?field-sym0) (syntax ?operator0))
    ;;         (cons (quote ?field-sym)  (syntax ?operator))
    ;;         ...)
    ;;
    ;;which evaluates to  an aslist whose keys  are field names and  whose values are
    ;;syntactic identifiers bound to accessors or mutators.
    ;;
    (cons 'list (fold-right
		    (lambda (key.id operator.id knil)
		      (if operator.id
			  (cons (list 'cons `(quote ,(syntax->datum key.id)) `(syntax ,operator.id))
				knil)
			knil))
		  '() field-name*.id operator*.id)))

  (define (%make-alist-from-syms key*.sym value*.sym)
    (cons 'list (map (lambda (key.sym value.sym)
		       (list 'cons `(quote ,key.sym) `(syntax ,value.sym)))
		  key*.sym value*.sym)))

  ;;A sexp which will be BLESSed in the  output code.  The sexp will evaluate to an
  ;;alist in which:  keys are symbols representing all the  field names; values are
  ;;identifiers bound to the safe accessors.
  (define foo-fields-safe-accessors.table
    (%make-alist-from-ids field-name*.sym safe-field-accessor*))

  ;;A sexp which will be BLESSed in the  output code.  The sexp will evaluate to an
  ;;alist in which:  keys are symbols representing mutable field  names; values are
  ;;identifiers bound to safe mutators.
  (define foo-fields-safe-mutators.table
    (%make-alist-from-ids field-name*.sym safe-field-mutator*))

  ;;A sexp which will be BLESSed in the  output code.  The sexp will evaluate to an
  ;;alist in which:  keys are symbols representing all the  field names; values are
  ;;identifiers bound to the unsafe accessors.
  (define foo-fields-unsafe-accessors.table
    (if (option.strict-r6rs)
	'(quote ())
      (%make-alist-from-ids field-name*.sym unsafe-field-accessor*)))

  ;;A sexp which will be BLESSed in the  output code.  The sexp will evaluate to an
  ;;alist in which:  keys are symbols representing mutable field  names; values are
  ;;identifiers bound to unsafe mutators.
  (define foo-fields-unsafe-mutators.table
    (if (option.strict-r6rs)
	'(quote ())
      (%make-alist-from-ids field-name*.sym unsafe-field-mutator*)))

  (define foo-methods.table
    (%make-alist-from-syms method-name*.sym method-procname*.sym))

  `(make-syntactic-binding-descriptor/record-type-name
    (make-r6rs-record-type-spec (syntax ,foo-rtd.sym)
				(syntax ,foo-rcd.sym)
				,(and foo-super-protocol.sym `(syntax ,foo-super-protocol.sym))
				,(and foo-parent.id          `(syntax ,foo-parent.id))
				(syntax ,make-foo.id)
				,(and foo-destructor.sym `(syntax ,foo-destructor.sym))
				(syntax ,foo?.id)
				,foo-fields-safe-accessors.table
				,foo-fields-safe-mutators.table
				,foo-fields-unsafe-accessors.table
				,foo-fields-unsafe-mutators.table
				,foo-methods.table)))


(define (%make-tag-type-spec-form foo make-foo foo? foo-parent
				  field-name*.sym
				  safe-field-accessor* unsafe-field-accessor*
				  safe-field-mutator* unsafe-field-mutator*
				  input-form.stx)

  (define type.str
    (symbol->string (syntax->datum foo)))
  (define %constructor-maker
    (string->symbol (string-append type.str "-constructor-maker")))
  (define %accessor-maker
    (string->symbol (string-append type.str "-accessor-maker")))
  (define %mutator-maker
    (string->symbol (string-append type.str "-mutator-maker")))
  (define %getter-maker
    (string->symbol (string-append type.str "-getter-maker")))
  (define %setter-maker
    (string->symbol (string-append type.str "-setter-maker")))
  `(internal-body
     (import (vicare)
       (prefix (vicare expander tag-type-specs) typ.))

     (define (,%constructor-maker input-form.stx)
       (syntax ,make-foo))

     (define (,%accessor-maker field.sym input-form-stx)
       (case field.sym
	 ,@(map (lambda (field-name accessor-id)
		  `((,field-name)	(syntax ,accessor-id)))
	     field-name*.sym safe-field-accessor*)
	 (else #f)))

     (define (,%mutator-maker field.sym input-form-stx)
       (case field.sym
	 ,@(fold-right
	       (lambda (field-name mutator-id knil)
		 (if mutator-id
		     (cons `((,field-name) (syntax ,mutator-id))
			   knil)
		   knil))
	     '() field-name*.sym safe-field-mutator*)
	 ,@(fold-right (lambda (field-name mutator-id knil)
			 (if mutator-id
			     knil
			   (cons `((,field-name)
				   (syntax-violation ',foo
				     "requested mutator of immutable record field name"
				     input-form-stx field.sym))
				 knil)))
	     '() field-name*.sym safe-field-mutator*)
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
     (define %dispatcher   #f)

     (define parent-id
       ,(if foo-parent
	    `(syntax ,foo-parent)
	  '(typ.record-tag-id)))

     (define tag-type-spec
       (typ.make-tag-type-spec (syntax ,foo) parent-id (syntax ,foo?)
			       ,%constructor-maker
			       ,%accessor-maker ,%mutator-maker
			       ,%getter-maker   ,%setter-maker
			       %caster-maker    %dispatcher))

     (typ.set-identifier-tag-type-spec! (syntax ,foo) tag-type-spec)))


;;;; done

#| end of module: DEFINE-RECORD-TYPE-MACRO |# )


;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
