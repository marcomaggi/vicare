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


(module (record-type-process-method-forms)
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
	   ;;     (parametrise ((current-table-of-names (make-hashtable string-hash string=?)))
	   ;; 	 (%do-define-record input-form.stx ?namespec ?clause* __synner__))
	   ;;   (debug-print (syntax->datum out)))
	   (parametrise ((current-table-of-names (make-hashtable string-hash string=?)))
	     (%do-define-record input-form.stx ?namespec ?clause* __synner__))
	   )))
      (_
       (__synner__ "invalid syntax in macro use"))))


;;;; helpers

;;It happens in this module that we have to generate syntactic identifiers associated
;;to  a field  or method  name.   If we  compose the  name  as a  string, create  the
;;syntactic identifier and then store the association  in a hashtable: we can be sure
;;to recreate the same identifier even when we use GENSYM to generate the name of the
;;identifier from the string name.
;;
;;This useful  for methods  defined with  DEFINE/OVERLOAD, which  must have  the same
;;name.
;;
(define current-table-of-names
  (make-parameter #f))

(define (%named-gensym/suffix foo suffix)
  (make-syntactic-identifier-for-temporary-variable (string-append (symbol->string (syntax->datum foo)) suffix)))

(define (%named-gensym/prefix foo prefix)
  (make-syntactic-identifier-for-temporary-variable (string-append prefix (symbol->string (syntax->datum foo)))))

(define* (private-identifier-append {ctxt identifier?} . str*)
  ;;Given  the identifier  CTXT  and a  list of  strings  or symbols  or
  ;;identifiers STR*: concatenate all the items in STR*, with the result
  ;;build and return a new identifier in the same context of CTXT.
  ;;
  (datum->syntax ctxt
		 (gensym
		  (apply string-append
			 (map (lambda (x)
				(cond ((symbol? x)
				       (symbol->string x))
				      ((string? x)
				       x)
				      ((identifier? x)
				       (symbol->string (syntax->datum x)))
				      (else
				       (assertion-violation __who__
					 "expected string, symbol or identifier as item argument" x))))
			   str*)))))

(define (%filter-out-falses ls)
  ;;Remove all the #f items from the list LS and return the resulting list.
  ;;
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
    (%preprocess-input-clauses namespec input-clause*.stx synner))
  (define strip-angular-parentheses?
    (%get-strip-angular-parentheses clause*.stx synner))
  (define-values (foo make-foo foo? foo-for-id-generation)
    (%parse-full-name-spec namespec strip-angular-parentheses? synner))
  (define define-type-descriptors?
    (%get-define-type-descriptors clause*.stx synner))
  (define foo-rtd (if define-type-descriptors?
		      (identifier-append foo foo-for-id-generation "-rtd")
		    (private-identifier-append foo foo-for-id-generation "-rtd")))
  (define foo-rcd (if define-type-descriptors?
		      (identifier-append foo foo-for-id-generation "-rcd")
		    (private-identifier-append foo foo-for-id-generation "-rcd")))

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
  ;;
  ;;PARENT-VIRTUAL-METHOD-SIGNATURES-ALIST an alist having  entries with format:
  ;;
  ;;   (?method-name . (?protection . ?method-signature))
  ;;
  ;;in which:  ?METHOD-NAME is  a symbol  representing a  parent's virtual  or sealed
  ;;method name;  ?PROTECTION is a the  fixnum 0 for  public, 1 for protected,  2 for
  ;;private; ?METHOD-SIGNATURE is:
  ;;
  ;;* When the  method is an open virtual method:  instances of "<closure-type-spec>"
  ;;representing the type signature of the method.
  ;;
  ;;* When the method has been sealed: the boolean false.
  ;;
  (define-values (foo-parent.id parent-rtd.id parent-rcd.id parent-rtd-definition parent-rcd-definition
				parent-virtual-method-signatures-alist)
    (%make-parent-rtd+rcd-code clause*.stx foo input-form.stx synner))

  ;;Parsing fields clauses.
  ;;
  (define-values
    (field-name*.sym
		;A list  of symbols  representing all  the field  names, in  the same
		;order  in  which  the  FIELDS  clauses  appear  in  the  record-type
		;definition.
     field-relative-idx*
		;A  list  of fixnums  representing  all  the field  relative  indexes
		;(zero-based).
     field-type*.ann
		;A  list  of syntax  objects  representing  the type  annotations  of
		;fields.  The type "<top>" is used when no type annotation is given.
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
     field-methods-alist-public
     field-methods-alist-protected
     field-methods-alist-private
		;Alist  for public,  protected and  private fields  having: as  keys,
		;symbols  representing   the  field   names;  as   values,  syntactic
		;identifiers that  will be bound  to the safe  field accessor/mutator
		;methods.
     fields-vector-spec
		;A  vector to  be  used as  FIELDS argument  for  the core  primitive
		;function MAKE-RECORD-TYPE-DESCRIPTOR.
     )
    (%parse-fields-clauses clause*.stx foo foo-for-id-generation
			   parent-virtual-method-signatures-alist
			   synner))

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
  (define-values (foo-super-rcd.id foo-super-rcd-definition)
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

  (define method-retriever-code-public.id
    (%named-gensym/suffix foo-for-id-generation "-method-retriever/public"))

  (define method-retriever-code-private.id
    (%named-gensym/suffix foo-for-id-generation "-method-retriever/private"))

  ;;Parse METHOD and similar clausess.  Build and return a symbolic expression (to be
  ;;BLESSed later)  which, expanded  and evaluated at  expand-time, returns  a syntax
  ;;object representing:
  ;;
  ;;* The definition of the method implementation functions.
  ;;
  ;;* The definition of the method-retriever functions.
  ;;
  ;;*  The  code needed  to  update  the  "<record-type-spec>" instance  with  method
  ;;informations.
  ;;
  ;;This  ugly  double-step  is  needed   to  implement  recursive  record-types  (in
  ;;particular,  record-type  in which  the  record-type  annotation appears  in  the
  ;;methods' type signatures).
  ;;
  (define method-parser.sexp
    (%make-method-parser-sexp clause*.stx foo foo-for-id-generation
			      field-methods-alist-public
			      field-methods-alist-protected
			      field-methods-alist-private
			      method-retriever-code-public.id
			      method-retriever-code-private.id
			      synner))

  ;;Null  or a  proper  list of  "<interface-type-spec>"  instances representing  the
  ;;interfaces implemented by this record-type.
  (define implemented-interface*.ots
    (%get-implemented-interface-specs clause*.stx synner))

  ;;Null or a proper  list of forms representing the code needed  to verify that this
  ;;record-type actually implements the specified interfaces.
  (define implemented-interfaces-table.sexp
    (%make-implemented-interfaces-table-code foo implemented-interface*.ots))

  ;;Null or a list of definitions to build at run-time the record-type descriptor.
  (define foo-rtd-definitions
    (%make-rtd-definitions foo foo-rtd foo-uid generative? clause*.stx parent-rtd.id fields-vector-spec
			   foo-destructor.id (%make-custom-printer-code clause*.stx foo synner)
			   foo-equality-predicate.id foo-comparison-procedure.id foo-hash-function.id
			   method-retriever-code-public.id method-retriever-code-private.id
			   implemented-interfaces-table.sexp synner))

  ;;A  symbolic expression  representing  a  form which,  expanded  and evaluated  at
  ;;expand-time, returns the right-hand side of the record-type name's DEFINE-SYNTAX.
  ;;The value of the right-hand side is the syntactic binding's descriptor.
  (define foo-syntactic-binding-form
    (%make-type-name-syntactic-binding-form foo foo-uid make-foo foo? foo-super-rcd.id foo-destructor.id
					    foo-parent.id foo-rtd foo-rcd
					    foo-equality-predicate.id foo-comparison-procedure.id foo-hash-function.id
					    implemented-interface*.ots))

  (bless
   ;;It would seem a good idea to wrap this form into a MODULE that exports:
   ;;
   ;;   ,foo
   ;; 	,make-foo ,foo?
   ;; 	,@safe-field-accessor*
   ;; 	,@(%filter-out-falses safe-field-mutator*)
   ;; 	,@(if define-type-descriptors?
   ;; 	      (list foo-rtd foo-rcd)
   ;;       '())
   ;;   ,@(if (options::strict-r6rs-enabled?)
   ;;         '()
   ;;       (append unsafe-field-accessor* (%filter-out-falses unsafe-field-mutator*)))
   ;;
   ;;but doing  so introduces  a lexical  contour that  makes it  impossible to  do a
   ;;forward definition  of the record-type.   Forward definitions are useful,  so we
   ;;use a simple BEGIN and use gensysm to make some syntactic bindings "private".
   `(begin
      ,@parent-rtd-definition
      ,@parent-rcd-definition
      (define-type ,foo (constructor ,foo-syntactic-binding-form))
      ;;We want these common function definitions after the definition of FOO.
      ,@foo-destructor-definition
      ,@foo-equality-predicate-definition
      ,@foo-comparison-procedure-definition
      ,@foo-hash-function-definition
      ;;The  RTD needs  to reference  the: destructor  function, equality  predicate,
      ;;comparison procedure, hash function,  method-retriever functions.  So the RTD
      ;;definition must come after these functions definitions.
      ,@foo-rtd-definitions
      ,@foo-rcd-definitions
      ,@foo-super-rcd-definition
      ,@foo-maker-definitions
      ,@foo-predicate-definitions
      ,@(%make-unsafe-accessor+mutator-code foo foo-rtd
					    field-name*.sym field-relative-idx* field-type*.ann
					    unsafe-field-accessor* unsafe-field-mutator*)
      ,@(%make-safe-accessor+mutator-code foo
					  field-name*.sym field-relative-idx* field-type*.ann
					  safe-field-accessor* unsafe-field-accessor*
					  safe-field-mutator*  unsafe-field-mutator*)
      ,@(%make-safe-field-methods-code foo field-type*.ann
				       field-methods-alist-private unsafe-field-accessor* unsafe-field-mutator*)
      ,method-parser.sexp
      #| end of BEGIN |# )))


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
    (let ((strict-r6rs-keys	#f)
	  (extended-keys	#f)
	  (typed-language-keys	#f))
      (lambda ()
	(unless strict-r6rs-keys
	  (set! strict-r6rs-keys
		(map core-prim-id '(fields parent parent-rtd protocol sealed opaque nongenerative)))
	  (set! extended-keys
		(append (map core-prim-id '(define-type-descriptors
					     strip-angular-parentheses constructor-signature
					     super-protocol destructor-protocol mixins
					     custom-printer type-predicate
					     equality-predicate comparison-procedure hash-function))
			strict-r6rs-keys))
	  (set! typed-language-keys
		(append (map core-prim-id '(method virtual-method seal-method implements
						   public protected private))
			extended-keys)))
	(cond ((options::typed-language-enabled?)
	       typed-language-keys)
	      ((options::strict-r6rs-enabled?)
	       strict-r6rs-keys)
	      (else
	       extended-keys)))))

  (define keyword-allowed-multiple-times?
    (let ((cached #f))
      (lambda (id)
	(free-id-member? id (or cached
				(receive-and-return (rv)
				    (map core-prim-id '(method virtual-method seal-method fields mixins implements
							       public protected private))
				  (set! cached rv)))))))

  #| end of module: %VALIDATE-DEFINITION-CLAUSES |# )


;;;; preprocessing of input clauses

(module (%preprocess-input-clauses)

  (define (%preprocess-input-clauses namespec input-clause*.stx synner)
    (let* ((foo		(syntax-match namespec ()
			  ((?foo ?make-foo ?foo?)
			   (identifier? ?foo)
			   ?foo)
			  (?foo
			   (identifier? ?foo)
			   ?foo)
			  (_
			   (synner "invalid record-type name specification" namespec))))
	   ;;We do  these twice to  allow mixin  clauses to contain  protection level
	   ;;specifications.
	   (clause*.stx	(if (options::strict-r6rs-enabled?)
			    input-clause*.stx
			  (%introduce-mixin-clauses foo input-clause*.stx synner)))
	   (clause*.stx	(if (options::typed-language-enabled?)
			    (%splice-protection-levels clause*.stx synner)
			  clause*.stx))
	   (clause*.stx	(if (options::strict-r6rs-enabled?)
			    clause*.stx
			  (%introduce-mixin-clauses foo clause*.stx synner)))
	   (clause*.stx	(if (options::typed-language-enabled?)
			    (%splice-protection-levels clause*.stx synner)
			  clause*.stx)))
      (%check-that-there-are-no-invalid-protection-levels clause*.stx synner)
      clause*.stx))

  (define (%introduce-mixin-clauses foo input-clause*.stx synner)
    (define-syntax-rule (clause-recursion)
      (%introduce-mixin-clauses foo (cdr input-clause*.stx) synner))
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
			(eq? 'define-mixin-type (syntax->datum ?kwd))
			(append (syntax-replace-id ?clause* mixin-name.id foo)
				(mixin-recur (cdr mixin-name*.id))))
		       (_
			(synner "expected mixin name in MIXINS clause" mixin-name.id)))))
	       (clause-recursion))))
	  (_
	   (cons (car input-clause*.stx)
		 (clause-recursion))))
      '()))

;;; --------------------------------------------------------------------

  (module (%splice-protection-levels)

    (define (%splice-protection-levels clause*.stx synner)
      (syntax-match clause*.stx ()
	((?head . ?tail)
	 (append (syntax-match ?head (public protected private)
		   ((public    ?clause . ?clause*)	(%splice-clause (public-id)    (cons ?clause ?clause*)))
		   ((protected ?clause . ?clause*)	(%splice-clause (protected-id) (cons ?clause ?clause*)))
		   ((private   ?clause . ?clause*)	(%splice-clause (private-id)   (cons ?clause ?clause*)))
		   (_					(list ?head)))
		 (%splice-protection-levels ?tail synner)))
	(()
	 '())))

    (define (%splice-clause protection.id clause*.stx)
      (syntax-match clause*.stx ()
	((?head . ?tail)
	 (cons (syntax-match ?head (fields method virtual-method seal-method)
		 ((fields         . ?field-spec*)	(cons* (fields-id)         protection.id ?field-spec*))
		 ((method         . ?stuff)		(cons* (method-id)         protection.id ?stuff))
		 ((virtual-method . ?stuff)		(cons* (virtual-method-id) protection.id ?stuff))
		 ((seal-method    . ?stuff)		(cons* (seal-method-id)    protection.id ?stuff))
		 (_					?head))
	       (%splice-clause protection.id ?tail)))
	(()
	 '())))

    #| end of modoule: %SPLICE-PROTECTION-LEVELS |# )

;;; --------------------------------------------------------------------

  (module (%check-that-there-are-no-invalid-protection-levels)

    (define (%check-that-there-are-no-invalid-protection-levels clause*.stx synner)
    (syntax-match clause*.stx ()
      (()
       (void))
      ((?head . ?tail)
       (syntax-match ?head (fields method virtual-method seal-method)
	 ((fields . ?stuff)
	  (%check-stuff-for-double ?head ?stuff synner))

	 ((method . ?stuff)
	  (%check-stuff-for-double ?head ?stuff synner))

	 ((virtual-method . ?stuff)
	  (%check-stuff-for-double ?head ?stuff synner))

	 ((seal-method . ?stuff)
	  (%check-stuff-for-double ?head ?stuff synner))

	 ((_ ?thing . ?rest)
	  (protection-level-id? ?thing)
	  (synner "found protection level specification in clause not supporting it" ?head))

	 (_
	  (%check-that-there-are-no-invalid-protection-levels ?tail synner))))))

    (define (%check-stuff-for-double head.stx stuff.stx synner)
      (syntax-match stuff.stx ()
	((?thing1 ?thing2 . ?rest)
	 (and (identifier? ?thing1)
	      (identifier? ?thing2))
	 (when (and (protection-level-id? ?thing1)
		    (protection-level-id? ?thing2))
	   (synner "while processing DEFINE-RECORD-TYPE, found double protection level specification" head.stx)))
	(_
	 (void))))

    #| end of module: %CHECK-THAT-THERE-ARE-NO-INVALID-PROTECTION-LEVELS |# )

  #| end of module: %PREPROCESS-INPUT-CLAUSES |# )


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

(define (%get-implemented-interface-specs clause* synner)
  (let loop ((clause*	clause*)
	     (iface*	'()))
    (syntax-match clause* (implements)
      (()
       (begin
	 (cond ((duplicate-identifiers? iface*)
		=> (lambda (id)
		     (synner "implemented interface declared multiple times" id))))
	 (map (lambda (iface.id)
		(let ((iface.ots (with-exception-handler
				     (lambda (E)
				       (let* ((msg "error dereferencing implemented interface name")
					      (msg (if (message-condition? E)
						       (string-append msg ": " (condition-message E))
						     msg)))
					 (synner msg iface.id)))
				   (lambda ()
				     (type-annotation->object-type-spec iface.id)))))
		  (if (interface-type-spec? iface.ots)
		      iface.ots
		    (synner "expected interface-type name as argument in IMPLEMENTS clause" iface.id))))
	   iface*)))

      (((implements ?interface* ...) . ?clauses)
       (begin
	 (for-all (lambda (obj)
		    (unless (identifier? obj)
		      (synner "expected interface identifier as argument in IMPLEMENTS clause" obj)))
	   ?interface*)
	 (loop ?clauses (append ?interface* iface*))))

      ((_ . ?clauses)
       (loop ?clauses iface*)))))


(module (%parse-fields-clauses)
  ;;Given the  definition clauses  CLAUSE*.STX extract the  FIELDS clauses  and parse
  ;;them.  Return the following values:
  ;;
  ;;1. FIELD-NAME*.SYM:  a list of symbols  representing all the field  names, in the
  ;;same order in which the FIELDS clauses appear in the record-type definition.
  ;;
  ;;2. FIELD-RELATIVE-IDX*:  a list  of fixnums representing  all the  field relative
  ;;indexes (zero-based).
  ;;
  ;;3. FIELD-TYPE*.ANN: a list of syntax objects representing the type annotations of
  ;;fields; the type "<top>" is used when no type annotation is given.
  ;;
  ;;4. SAFE-FIELD-ACCESSOR*:  a list of syntactic  identifiers that will be  bound to
  ;;the safe accessors.
  ;;
  ;;5. UNSAFE-FIELD-ACCESSOR*: a list of syntactic  identifiers that will be bound to
  ;;the unsafe accessors.
  ;;
  ;;6. SAFE-FIELD-MUTATOR*: a list of syntactic identifiers that will be bound to the
  ;;safe mutators.   This list holds  #f in the  position of immutable  fields.  This
  ;;list has one item for each item in SAFE-FIELD-ACCESSOR*.
  ;;
  ;;7. UNSAFE-FIELD-MUTATOR*: a  list of syntactic identifiers that will  be bound to
  ;;the unsafe  mutators.  This list  holds #f in  the position of  immutable fields.
  ;;This list has one item for each item in UNSAFE-FIELD-ACCESSOR*.
  ;;
  ;;8. FIELD-METHODS-ALIST-PUBLIC: alist  for public fields having:  as keys, symbols
  ;;representing the field names; as values, syntactic identifiers that will be bound
  ;;to the safe field accessor/mutator methods.
  ;;
  ;;9.  FIELD-METHODS-ALIST-PROTECTED:  alist for  protected fields having:  as keys,
  ;;symbols representing the field names;  as values, syntactic identifiers that will
  ;;be bound to the safe field accessor/mutator methods.
  ;;
  ;;10.   FIELD-METHODS-ALIST-PRIVATE:  alist for  private  fields  having: as  keys,
  ;;symbols representing the field names;  as values, syntactic identifiers that will
  ;;be bound to the safe field accessor/mutator methods.
  ;;
  ;;11.  FIELDS-VECTOR-SPEC:  a vector  to be  used as FIELDS  argument for  the core
  ;;primitive function MAKE-RECORD-TYPE-DESCRIPTOR.
  ;;
  ;;Here we assume that FIELD-CLAUSE* is null or a proper list.
  ;;
  (define (%parse-fields-clauses clause*.stx foo foo-for-id-generation parent-virtual-method-signatures-alist synner)
    (define fields-clause*
      (%parse-clauses clause*.stx synner))

    (define (%gen-safe-accessor-name field-name.id)
      (identifier-append foo foo-for-id-generation "-" field-name.id))
    (define (%gen-unsafe-accessor-name field-name.id)
      (if (options::strict-r6rs-enabled?)
	  (private-identifier-append foo "$" foo-for-id-generation "-" field-name.id)
	(identifier-append foo "$" foo-for-id-generation "-" field-name.id)))
    (define (%gen-safe-mutator-name field-name.id)
      (identifier-append foo foo-for-id-generation "-" field-name.id "-set!"))
    (define (%gen-unsafe-mutator-name field-name.id)
      (if (options::strict-r6rs-enabled?)
	  (private-identifier-append foo "$" foo-for-id-generation "-" field-name.id "-set!")
	(identifier-append foo "$" foo-for-id-generation "-" field-name.id "-set!")))
    (define (%gen-safe-method-name field-name.id)
      (private-identifier-append foo foo-for-id-generation "-" field-name.id "-method"))

    (let loop ((fields-clause*			fields-clause*)
	       (i				0)
	       (field-name*.sym			'())
	       (field-relative-idx*		'())
	       (field-type*.ann			'())
	       (safe-field-accessor*		'())
	       (unsafe-field-accessor*		'())
	       (safe-field-mutator*		'())
	       (unsafe-field-mutator*		'())
	       (field-methods-alist-public	'())
	       (field-methods-alist-protected	'())
	       (field-methods-alist-private	'())
	       (fields-vector-spec*		'()))
      (define-syntax-rule (%%parse-typed-field ?name)
	(%parse-typed-field ?name field-name*.sym parent-virtual-method-signatures-alist synner))
      (syntax-match fields-clause* (mutable immutable)
	(()
	 (values (reverse field-name*.sym)
		 (reverse field-relative-idx*)
		 (reverse field-type*.ann)
		 (reverse safe-field-accessor*)
		 (reverse unsafe-field-accessor*)
		 (reverse safe-field-mutator*)
		 (reverse unsafe-field-mutator*)
		 (reverse field-methods-alist-public)
		 (reverse field-methods-alist-protected)
		 (reverse field-methods-alist-private)
		 (list->vector (reverse fields-vector-spec*))))

	(((?protection mutable ?name ?accessor ?mutator) . ?rest)
	 (and (identifier? ?accessor)
	      (identifier? ?mutator))
	 (receive (field-name.id field-name.sym field-type.ann)
	     (%%parse-typed-field ?name)
	   (let ((field-method-entry (cons field-name.sym (%gen-safe-method-name field-name.id))))
	     (loop ?rest (fxadd1 i)
		   (cons field-name.sym field-name*.sym)
		   (cons i field-relative-idx*)
		   (cons field-type.ann field-type*.ann)
		   (cons ?accessor safe-field-accessor*)
		   (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
		   (cons ?mutator safe-field-mutator*)
		   (cons (%gen-unsafe-mutator-name  field-name.id) unsafe-field-mutator*)
		   ;;FIELD-METHODS-ALIST-PUBLIC
		   (if (eq? ?protection 0)
		       (cons field-method-entry field-methods-alist-public)
		     field-methods-alist-public)
		   ;;FIELD-METHODS-ALIST-PROTECTED
		   (if (or (eq? ?protection 0)
			   (eq? ?protection 1))
		       (cons field-method-entry field-methods-alist-protected)
		     field-methods-alist-protected)
		   ;;FIELD-METHODS-ALIST-PRIVATE
		   (cons field-method-entry field-methods-alist-private)
		   (cons `(mutable ,field-name.id) fields-vector-spec*)))))

	(((?protection immutable ?name ?accessor) . ?rest)
	 (identifier? ?accessor)
	 (receive (field-name.id field-name.sym field-type.ann)
	     (%%parse-typed-field ?name)
	   (let ((field-method-entry (cons field-name.sym (%gen-safe-method-name field-name.id))))
	     (loop ?rest (fxadd1 i)
		   (cons field-name.sym field-name*.sym)
		   (cons i field-relative-idx*)
		   (cons field-type.ann field-type*.ann)
		   (cons ?accessor safe-field-accessor*)
		   (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
		   (cons #f safe-field-mutator*)
		   (cons #f unsafe-field-mutator*)
		   ;;FIELD-METHODS-ALIST-PUBLIC
		   (if (eq? ?protection 0)
		       (cons field-method-entry field-methods-alist-public)
		     field-methods-alist-public)
		   ;;FIELD-METHODS-ALIST-PROTECTED
		   (if (or (eq? ?protection 0)
			   (eq? ?protection 1))
		       (cons field-method-entry field-methods-alist-protected)
		     field-methods-alist-protected)
		   ;;FIELD-METHODS-ALIST-PRIVATE
		   (cons field-method-entry field-methods-alist-private)
		   (cons `(immutable ,field-name.id) fields-vector-spec*)))))

	(((?protection mutable   ?name) . ?rest)
	 (receive (field-name.id field-name.sym field-type.ann)
	     (%%parse-typed-field ?name)
	   (let ((field-method-entry (cons field-name.sym (%gen-safe-method-name field-name.id))))
	     (loop ?rest (fxadd1 i)
		   (cons field-name.sym field-name*.sym)
		   (cons i field-relative-idx*)
		   (cons field-type.ann field-type*.ann)
		   (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
		   (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
		   (cons (%gen-safe-mutator-name    field-name.id) safe-field-mutator*)
		   (cons (%gen-unsafe-mutator-name  field-name.id) unsafe-field-mutator*)
		   ;;FIELD-METHODS-ALIST-PUBLIC
		   (if (eq? ?protection 0)
		       (cons field-method-entry field-methods-alist-public)
		     field-methods-alist-public)
		   ;;FIELD-METHODS-ALIST-PROTECTED
		   (if (or (eq? ?protection 0)
			   (eq? ?protection 1))
		       (cons field-method-entry field-methods-alist-protected)
		     field-methods-alist-protected)
		   ;;FIELD-METHODS-ALIST-PRIVATE
		   (cons field-method-entry field-methods-alist-private)
		   (cons `(mutable ,field-name.id) fields-vector-spec*)))))

	(((?protection immutable ?name) . ?rest)
	 (receive (field-name.id field-name.sym field-type.ann)
	     (%%parse-typed-field ?name)
	   (let ((field-method-entry (cons field-name.sym (%gen-safe-method-name field-name.id))))
	     (loop ?rest (fxadd1 i)
		   (cons field-name.sym field-name*.sym)
		   (cons i field-relative-idx*)
		   (cons field-type.ann field-type*.ann)
		   (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
		   (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
		   (cons #f safe-field-mutator*)
		   (cons #f unsafe-field-mutator*)
		   ;;FIELD-METHODS-ALIST-PUBLIC
		   (if (eq? ?protection 0)
		       (cons field-method-entry field-methods-alist-public)
		     field-methods-alist-public)
		   ;;FIELD-METHODS-ALIST-PROTECTED
		   (if (or (eq? ?protection 0)
			   (eq? ?protection 1))
		       (cons field-method-entry field-methods-alist-protected)
		     field-methods-alist-protected)
		   ;;FIELD-METHODS-ALIST-PRIVATE
		   (cons field-method-entry field-methods-alist-private)
		   (cons `(immutable ,field-name.id) fields-vector-spec*)))))

	(((?protection . ?name) . ?rest)
	 (receive (field-name.id field-name.sym field-type.ann)
	     (%%parse-typed-field ?name)
	   (let ((field-method-entry (cons field-name.sym (%gen-safe-method-name field-name.id))))
	     (loop ?rest (fxadd1 i)
		   (cons field-name.sym field-name*.sym)
		   (cons i field-relative-idx*)
		   (cons field-type.ann field-type*.ann)
		   (cons (%gen-safe-accessor-name   field-name.id) safe-field-accessor*)
		   (cons (%gen-unsafe-accessor-name field-name.id) unsafe-field-accessor*)
		   (cons #f safe-field-mutator*)
		   (cons #f unsafe-field-mutator*)
		   ;;FIELD-METHODS-ALIST-PUBLIC
		   (if (eq? ?protection 0)
		       (cons field-method-entry field-methods-alist-public)
		     field-methods-alist-public)
		   ;;FIELD-METHODS-ALIST-PROTECTED
		   (if (or (eq? ?protection 0)
			   (eq? ?protection 1))
		       (cons field-method-entry field-methods-alist-protected)
		     field-methods-alist-protected)
		   ;;FIELD-METHODS-ALIST-PRIVATE
		   (cons field-method-entry field-methods-alist-private)
		   (cons `(immutable ,field-name.id) fields-vector-spec*)))))

	(((?protection . ?spec) . ?rest)
	 (synner "invalid field specification in DEFINE-RECORD-TYPE syntax" ?spec)))))

;;; --------------------------------------------------------------------

  (define (%parse-clauses clause*.stx synner)
    ;;Given the  full input  clauses in  CLAUSE*.STX, select  the FIELDS  clauses and
    ;;splice them into a list of syntax objects, each having one of the formats:
    ;;
    ;;   (?protection ?field-name)
    ;;   (?protection (immutable ?field-name ?accessor)
    ;;   (?protection (mutable ?field-name ?accessor ?mutator)
    ;;   (?protection (brace ?field-name ?type-ann))
    ;;   (?protection (immutable (brace ?field-name ?type-ann) ?accessor)
    ;;   (?protection (mutable (brace ?field-name ?type-ann) ?accessor ?mutator)
    ;;
    ;;where ?PROTECTION is a fixnum: 0 for public, 1 for protected, 2 for private.
    ;;
    ;;Return the spliced list.
    ;;
    (let loop ((clause*.stx	clause*.stx)
	       (field-spec**	'()))
      (syntax-match clause*.stx (fields public protected private)
	(()
	 (if (options::strict-r6rs-enabled?)
	     (if (pair? field-spec**)
		 ;;If there is only one list of field specs, fine; otherwise raise an
		 ;;error.
		 (if (list-of-single-item? field-spec**)
		     (let* ((field-spec*	(car field-spec**))
			    (protection		(car field-spec*))
			    (spec*		(cdr field-spec*)))
		       (map (lambda (spec)
			      (cons protection spec))
			 spec*))
		   (synner "invalid multiple FIELDS clauses in strict-R6RS language"
			   (map (lambda (field-spec*)
				  (cons (fields-id) field-spec*))
			     field-spec**)))
	       '())
	   ;;Non-strict language: we accept any number of FIELDS clauses.
	   (if (pair? field-spec**)
	       (fold-left (lambda (knil field-spec*)
			    (let ((protection	(car field-spec*))
				  (spec*	(cdr field-spec*)))
			      (append (map (lambda (spec)
					     (cons protection spec))
					spec*)
				      knil)))
		 '() field-spec**)
	     '())))

	(((fields public ?field-spec* ...) . ?clauses)
	 ;;If the language is not typed: we accept fields with name PUBLIC, otherwise
	 ;;it is a protection level specifier.
	 (options::typed-language-enabled?)
	 (loop ?clauses (cons (cons 0 ?field-spec*) field-spec**)))

	(((fields protected ?field-spec* ...) . ?clauses)
	 ;;If  the language  is  not typed:  we accept  fields  with name  PROTECTED,
	 ;;otherwise it is a protection level specifier.
	 (options::typed-language-enabled?)
	 (loop ?clauses (cons (cons 1 ?field-spec*) field-spec**)))

	(((fields private ?field-spec* ...) . ?clauses)
	 ;;If  the  language is  not  typed:  we  accept  fields with  name  PRIVATE,
	 ;;otherwise it is a protection level specifier.
	 (options::typed-language-enabled?)
	 (loop ?clauses (cons (cons 2 ?field-spec*) field-spec**)))

	(((fields ?field-spec* ...) . ?clauses)
	 (loop ?clauses (cons (cons 0 ?field-spec*) field-spec**)))

	((_ . ?clauses)
	 (loop ?clauses field-spec**)))))

;;; --------------------------------------------------------------------

  (define (%parse-typed-field field-name.stx field-name*.sym parent-virtual-method-signatures-alist synner)
    ;;Given a typed field name  specification in FIELD-NAME.STX, return the following
    ;;values:
    ;;
    ;;1. A syntactic identifier representing the field name.
    ;;
    ;;2. A symbol representing the field name.
    ;;
    ;;3. A syntax object representing the  type annotation.  This defaults to "<top>"
    ;;when no annotation is present in the input form.
    ;;
    ;;Check that there are no duplicate field  names.  Check that there are no fields
    ;;with the same name of a parent's virtual or sealed method.
    ;;
    (receive-and-return (field-name.id field-name.sym field-type.ann)
	(syntax-match field-name.stx (brace)
	  ((brace ?id ?type)
	   (values ?id (identifier->symbol ?id) ?type))
	  (?id
	   (identifier? ?id)
	   (values ?id (identifier->symbol ?id) (<top>-type-id))))
      (when (memq field-name.sym field-name*.sym)
	(synner "multiple field definitions with the same name" field-name.id))
      (when (exists (lambda (entry)
		      ;;The ENTRY has the format:
		      ;;
		      ;;   (?method-name . (?protection . ?method-signature))
		      ;;
		      (eq? (car entry) field-name.sym))
	      parent-virtual-method-signatures-alist)
	(synner "forbidden field with name equal to a virtual or sealed parent's method" field-name.id))))


  #| end of module: %PARSE-FIELDS-CLAUSES |# )


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


(define (%make-safe-field-methods-code foo field-type*.ann field-methods-alist unsafe-field-accessor* unsafe-field-mutator*)
  ;;Return a list  holding a symbolic expressions (to be  BLESSed later) representing
  ;;Scheme  expressions  which,  expanded  and  evaluated  at  run-time,  define  the
  ;;syntactic  bindings of  the field  methods.   The returned  list is  meant to  be
  ;;spliced in the output form.  The field methods act as both accessors (for all the
  ;;fields) and mutators (for mutable fields).
  ;;
  ;;FOO must be the syntactic identifier represening the record-type name.
  ;;
  ;;FIELD-TYPE*.ANN must be the list of  syntax objects representing the fields' type
  ;;annotations.
  ;;
  ;;FIELD-METHODS-ALIST must  be an alist  having: as keys, symbols  representing the
  ;;field names;  as values,  syntactic identifiers  that will be  bound to  the safe
  ;;field accessor/mutator methods.
  ;;
  ;;UNSAFE-FIELD-ACCESSOR*  and  UNSAFE-FIELD-MUTATOR*  must   be  lists  of  symbols
  ;;representing the  names of the  syntactic identifiers  bound to the  unsafe field
  ;;accessors and mutators.
  ;;
  (map (lambda (field-method-entry unsafe-field-accessor unsafe-field-mutator field-type.ann)
	 (let ((safe-method-procname.id	(cdr field-method-entry))
	       (record.id		(make-syntactic-identifier-for-temporary-variable "record"))
	       (new-value.id		(make-syntactic-identifier-for-temporary-variable "new-value")))
	   (if unsafe-field-mutator
	       `(begin
		  (define/overload ((brace ,safe-method-procname.id ,field-type.ann) (brace ,record.id ,foo))
		    (unsafe-cast-signature (,field-type.ann) (,unsafe-field-accessor ,record.id)))
		  (define/overload ((brace ,safe-method-procname.id <void>) (brace ,record.id ,foo) (brace ,new-value.id ,field-type.ann))
		    (,unsafe-field-mutator ,record.id ,new-value.id)))
	     `(define/checked ((brace ,safe-method-procname.id ,field-type.ann) (brace ,record.id ,foo))
		(unsafe-cast-signature (,field-type.ann) (,unsafe-field-accessor ,record.id))))))
    field-methods-alist unsafe-field-accessor* unsafe-field-mutator* field-type*.ann))


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
  ;;6.   PARENT-VIRTUAL-METHODS-ALIST: an  alist  representing  the parent's  virtual
  ;;methods and sealed methods.
  ;;
  (let ((parent-clause (%get-clause 'parent clause*)))
    (syntax-match parent-clause ()
      ;;If there  is a  PARENT clause  insert code  that retrieves  the RTD  from the
      ;;parent type name.
      ((_ ?parent-name)
       (identifier? ?parent-name)
       ;;Validate  ?PARENT-NAME  as  syntactic  identifier  bound  to  a  record-type
       ;;syntactic binding.
       (let* ((parent.ots	(id->record-type-spec ?parent-name))
	      (parent-proto	(record-type-spec.super-protocol-id parent.ots))
	      (parent-rtd.id	(%named-gensym/suffix foo "-parent-rtd"))
	      (parent-rcd.id	(%named-gensym/suffix foo "-parent-rcd")))
	 (values ?parent-name parent-rtd.id parent-rcd.id
		 ;;parent-rtd-definition
		 `((define/typed {,parent-rtd.id <record-type-descriptor>}
		     (record-type-descriptor ,?parent-name)))
		 ;;parent-rcd-definition
		 `((define/typed {,parent-rcd.id  <record-constructor-descriptor>}
		     ;;If the parent has a super-type constructor descriptor: use it;
		     ;;otherwise use the default constructor descriptor.
		     ,(or parent-proto `(record-constructor-descriptor ,?parent-name))))
		 ;;parent-virtual-methods-alist
		 (record-type-spec.virtual-method-signatures parent.ots)
		 )))

      ;;If there is no PARENT clause try to retrieve the expression evaluating to the
      ;;RTD.
      (#f
       (let ((parent-rtd-clause (%get-clause 'parent-rtd clause*)))
	 (syntax-match parent-rtd-clause ()
	   ((_ ?parent-rtd ?parent-rcd)
	    (let ((parent-rtd.id	(%named-gensym/suffix foo "-parent-rtd"))
		  (parent-rcd.id	(%named-gensym/suffix foo "-parent-rcd")))
	      (values #f parent-rtd.id parent-rcd.id
		      ;;parent-rtd-definition
		      `((define/typed {,parent-rtd.id <record-type-descriptor>}
			  ,?parent-rtd))
		      ;;parent-rcd-definition
		      `((define/typed {,parent-rcd.id <record-constructor-descriptor>}
			  ,?parent-rcd))
		      '() ;parent-virtual-methods-alist
		      )))

	   ;;If  neither the  PARENT nor  the  PARENT-RTD clauses  are present:  just
	   ;;return false.
	   (#f
	    (values #f	;foo-parent
		    #f	;parent-rtd
		    #f	;parent-rcd
		    '() ;parent-rtd-definition
		    '() ;parent-rcd-definition
		    '() ;parent-virtual-methods-alist
		    ))

	   (_
	    (synner "invalid syntax in PARENT-RTD clause" parent-rtd-clause)))))

      (_
       (synner "invalid syntax in PARENT clause" parent-clause)))))


(define (%make-rtd-definitions foo foo-rtd foo-uid generative?
			       clause* parent-rtd fields-vector-spec
			       destructor printer
			       equality-predicate comparison-procedure hash-function
			       method-retriever-code-public.id method-retriever-code-private.id
			       implemented-interfaces-table synner)
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
					   fields-vector-spec)))
	(method-name.id		(make-syntactic-identifier-for-temporary-variable "method-name")))
    `((define/typed (brace ,foo-rtd <record-type-descriptor>)
	($make-record-type-descriptor-ex (quote ,foo) ,parent-rtd
					 (quote ,foo-uid) ,generative? ,sealed? ,opaque?
					 ,fields-vec ,normalised-fields-vec
					 ,destructor ,printer
					 ,equality-predicate ,comparison-procedure ,hash-function
					 (lambda/typed ({_ (or <false> <procedure>)} {,method-name.id <symbol>})
					   (,method-retriever-code-public.id  ,method-name.id))
					 (lambda/typed ({_ (or <false> <procedure>)} {,method-name.id <symbol>})
					   (,method-retriever-code-private.id ,method-name.id))
					 ,implemented-interfaces-table)))))


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
    `((define/typed ,internal-maker.id
	($record-constructor ,foo-rcd))
      ,(if constructor-signature.stx
	   `(begin
	      (define/typed ,make-foo
		(cast-signature (,constructor-signature.stx) ,internal-maker.id))
	      (begin-for-syntax
		;;Does  the user-specified  construtor signature  actually returns  a
		;;single value of type FOO?  We check it here.
		(unless (type-annotation-super-and-sub? (lambda <bottom> => (,foo)) ,constructor-signature.stx)
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
     (let ((obj.id			(make-syntactic-identifier-for-temporary-variable "obj"))
	   (internal-predicate.id	(make-syntactic-identifier-for-temporary-variable
					 (string-append "internal-predicate-" (identifier->string foo?)))))
       `((define/typed {,internal-predicate.id <type-predicate>}
	   (cast-signature (<type-predicate>)
	     (,?type-predicate-expr (lambda/typed ({_ <boolean>} ,obj.id)
				      (and ($struct? ,obj.id)
					   ($record-and-rtd? (unsafe-cast-signature (<struct>) ,obj.id) ,foo-rtd))))))
	 (define/checked ((brace ,foo? <boolean>) ,obj.id)
	   (,internal-predicate.id ,obj.id)))))

    (#f
     ;;No TYPE-PREDICATE clause  in this record-type definition.  Return  a list of
     ;;definitions representing the default record-type predicate definition.
     (let ((obj.id (make-syntactic-identifier-for-temporary-variable "obj")))
       `((define/typed ((brace ,foo? <boolean>) ,obj.id)
	   (and ($struct? ,obj.id)
		($record-and-rtd? (unsafe-cast-signature (<struct>) ,obj.id) ,foo-rtd))))))

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
       `(let/checked ((,foo-destructor-protocol ,?destructor-protocol-expr))
	  (unless (procedure? ,foo-destructor-protocol)
	    (assertion-violation (quote ,foo)
	      "expected closure object as result of evaluating the destructor protocol expression"
	      ,foo-destructor-protocol))
	  (receive-and-return/checked (,destructor-tmp)
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
	     `(let/checked ((,proto-func.sym ,?proto-expr))
		(receive-and-return/checked ({,pred-func.sym (equality-predicate ,foo)})
		    (,proto-func.sym ($record-type-equality-predicate ,parent-rtd))
		  (unless (procedure? ,pred-func.sym)
		    (assertion-violation (quote ,foo)
		      "expected closure object from evaluation of equality predicate's protocol function"
		      ,pred-func.sym))))
	   ;;The  new  record-type has  no  parent:  we  just evaluate  the  protocol
	   ;;function with no arguments.
	   `(let/checked ((,proto-func.sym ,?proto-expr))
	      (receive-and-return/checked ({,pred-func.sym (equality-predicate ,foo)})
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
	     `(let/checked ((,proto-func.sym ,?proto-expr))
		(receive-and-return/checked ({,compar-func.sym (comparison-procedure ,foo)})
		    (,proto-func.sym ($record-type-comparison-procedure ,parent-rtd))
		  (unless (procedure? ,compar-func.sym)
		    (assertion-violation (quote ,foo)
		      "expected closure object from evaluation of comparison procedure's protocol function"
		      ,compar-func.sym))))
	   ;;The  new  record-type has  no  parent:  we  just evaluate  the  protocol
	   ;;function with no arguments.
	   `(let/checked ((,proto-func.sym ,?proto-expr))
	      (receive-and-return/checked ({,compar-func.sym (comparison-procedure ,foo)})
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
	     `(let/checked ((,proto-func.sym ,?proto-expr))
		(receive-and-return/checked ({,hash-func.sym (hash-function ,foo)})
		    (,proto-func.sym ($record-type-hash-function ,parent-rtd))
		  (unless (procedure? ,hash-func.sym)
		    (assertion-violation (quote ,foo)
		      "expected closure object from evaluation of hash function's protocol function"
		      ,hash-func.sym))))
	   ;;The  new  record-type has  no  parent:  we  just evaluate  the  protocol
	   ;;function with no arguments.
	   `(let/checked ((,proto-func.sym ,?proto-expr))
	      (receive-and-return/checked ({,hash-func.sym (hash-function ,foo)})
		  (,proto-func.sym)
		(unless (procedure? ,hash-func.sym)
		  (assertion-violation (quote ,foo)
		    "expected closure object from evaluation of hash function's protocol function"
		    ,hash-func.sym)))))))

      ;;No matching clause found.
      (#f	#f)

      (_
       (synner "invalid syntax in HASH-FUNCTION clause" clause)))))


(module (%make-method-parser-sexp)
  ;;Parse METHOD and similar clauses.  Build  and return a symbolic expression (to be
  ;;BLESSed later)  which, expanded  and evaluated at  expand-time, returns  a syntax
  ;;object representing:
  ;;
  ;;* The definition of the method implementation functions.
  ;;
  ;;* The definition of the method-retriever functions.
  ;;
  ;;*  The  code needed  to  update  the  "<record-type-spec>" instance  with  method
  ;;informations.
  ;;
  ;;This  ugly  double-step  is  needed   to  implement  recursive  record-types  (in
  ;;particular,  record-type  in which  the  record-type  annotation appears  in  the
  ;;methods' type signatures).
  ;;
  ;;The arguments FIELD-METHODS-ALIST-* are alists  for public, protected and private
  ;;fields  having:  as  keys,  symbols  representing the  field  names;  as  values,
  ;;syntactic  identifiers that  will be  bound  to the  safe field  accessor/mutator
  ;;methods.
  ;;
  ;;The arguments METHOD-RETRIEVER-CODE-*.ID are syntactic identifiers that are meant
  ;;to be bound to the method-retriever functions for this record-type.
  ;;
  (define* (%make-method-parser-sexp clause*.stx foo foo-for-id-generation
				     field-methods-alist-public field-methods-alist-protected field-methods-alist-private
				     method-retriever-code-public.id method-retriever-code-private.id
				     synner)
    (define method-clause*.stx
      (%parse-clauses clause*.stx synner))

    (define field-methods-alist-public.sexp
      `(list . ,(map (lambda (entry)
		       `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
		  field-methods-alist-public)))

    (define field-methods-alist-protected.sexp
      `(list . ,(map (lambda (entry)
		       `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
		  field-methods-alist-protected)))

    (define field-methods-alist-private.sexp
      `(list . ,(map (lambda (entry)
		       `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
		  field-methods-alist-private)))

    (define output-form.sexp
      `(expand-time-expr
	(record-type-process-method-forms (syntax ,foo)
					  (syntax ,foo-for-id-generation)
					  (syntax ,method-clause*.stx)
					  ,field-methods-alist-public.sexp
					  ,field-methods-alist-protected.sexp
					  ,field-methods-alist-private.sexp
					  (syntax ,method-retriever-code-public.id)
					  (syntax ,method-retriever-code-private.id))))

    ;;(debug-print __who__ (syntax->datum output-form.sexp))
    output-form.sexp)

  (define (%parse-clauses clause*.stx synner)
    ;;Recursive function.  Select and parse the METHOD clauses in CLAUSE*.STX, return
    ;;a list of method clauses.
    ;;
    (define-syntax-rule (recurse)
      (%parse-clauses (cdr clause*.stx) synner))
    (if (pair? clause*.stx)
	(let ((clause.stx (car clause*.stx)))
	  (syntax-match clause.stx (method virtual-method seal-method)
	    ((method         . ?stuff)	(cons clause.stx (recurse)))
	    ((virtual-method . ?stuff)	(cons clause.stx (recurse)))
	    ((seal-method    . ?stuff)	(cons clause.stx (recurse)))
	    (_				(recurse))))
      '()))

  #| end of module: %MAKE-METHOD-PARSER-SEXP |# )


(define* (record-type-process-method-forms foo foo-for-id-generation
					   method-clause*.stx
					   field-methods-alist-public
					   field-methods-alist-protected
					   field-methods-alist-private
					   method-retriever-code-public.id
					   method-retriever-code-private.id)
  ;;Parse METHOD and  similar clauses coming from the original  input form.  Return a
  ;;single syntax object representing a BEGIN form.
  ;;
  ;;Inside the returned BEGIN form there are Scheme definitions representing:
  ;;
  ;;* The definition of the method implementation functions.
  ;;
  ;;* The definition of the method-retriever functions.
  ;;
  ;;This function updates the "<record-type-spec>" instance with method informations.
  ;;
  ;;The arguments FIELD-METHODS-ALIST-* are alists  for public, protected and private
  ;;fields  having:  as  keys,  symbols  representing the  field  names;  as  values,
  ;;syntactic  identifiers that  will be  bound  to the  safe field  accessor/mutator
  ;;methods.
  ;;
  ;;The arguments METHOD-RETRIEVER-CODE-*.ID are syntactic identifiers that are meant
  ;;to be bound to the method-retriever functions for this record-type.
  ;;
  (define record.ots
    (type-annotation->object-type-spec foo))

  ;;We  need to  remember that  record-types always  have a  parent: either  a proper
  ;;record-type or the core-type "<record>".
  (define parent.ots			(object-type-spec.parent-ots record.ots))
  ;;We need this multiple times below.
  (define has-record-type-parent?	(record-type-spec?           parent.ots))
  ;;False or the syntactic identifier bound to the parent record-type descriptor.  It
  ;;is used to access the parent's method-retriever procedure.  This must be false if
  ;;the parent of the record-type is "<record>".
  (define parent-rtd.id
    (and has-record-type-parent?
	 (record-type-spec.rtd-id parent.ots)))

  ;;PARENT-EARLY-BINDING-METHODS-ALIST-PUBLIC: null or an  alist associated to public
  ;;methods having: as keys, symbols  representing method names; as values, syntactic
  ;;identifiers bound to the method implementation functions.
  ;;
  ;;PARENT-EARLY-BINDING-METHODS-ALIST-PROTECTED:  null  or  an alist  associated  to
  ;;protected methods.
  ;;
  (define parent-early-binding-methods-alist-public
    (if has-record-type-parent?
	(object-type-spec.methods-table-public parent.ots)
      '()))
  (define parent-early-binding-methods-alist-protected
    (if has-record-type-parent?
	(object-type-spec.methods-table-protected parent.ots)
      '()))

  ;;A possibly empty alist having entries with format:
  ;;
  ;;   (?method-name . (?protection . ?method-signature))
  ;;
  ;;in which:  ?METHOD-NAME is  a symbol  representing a  parent's virtual  or sealed
  ;;method name;  ?PROTECTION is a the  fixnum 0 for  public, 1 for protected,  2 for
  ;;private; ?METHOD-SIGNATURE is:
  ;;
  ;;* When the method is an open virtual method: instances of "<closure-type-spec>"
  ;;representing the type signature of the method.
  ;;
  ;;* When the method has been sealed: the boolean false.
  ;;
  (define parent-virtual-method-signatures-alist
    (if has-record-type-parent?
	(record-type-spec.virtual-method-signatures parent.ots)
      ;;The core-type "<record>" does not have a virtual methods table.
      '()))

  ;;EARLY-BINDING-METHODS-ALIST-PUBLIC an  alist for public methods  having: as keys,
  ;;symbols  representing the  method names,  including the  field names;  as values,
  ;;syntactic identifiers that will be bound to the method implementation procedures,
  ;;including public field methods.
  ;;
  ;;EARLY-BINDING-METHODS-ALIST-PROTECTED an  alist for protected methods  having: as
  ;;keys, symbols representing the method names, including the protected field names;
  ;;as values, syntactic identifiers that will  be bound to the method implementation
  ;;procedures, including protected field methods.
  ;;
  ;;EARLY-BINDING-METHODS-ALIST-PRIVATE an alist for private methods having: as keys,
  ;;symbols  representing the  method names,  including the  private field  names; as
  ;;values, syntactic  identifiers that  will be bound  to the  method implementation
  ;;procedures, including private field methods.
  ;;
  (define early-binding-methods-alist-public.tail
    (append field-methods-alist-public	parent-early-binding-methods-alist-public))
  (define early-binding-methods-alist-protected.tail
    (append field-methods-alist-protected	parent-early-binding-methods-alist-protected))
  (define early-binding-methods-alist-private.tail
    (append field-methods-alist-private	parent-early-binding-methods-alist-protected))

  (case-define synner
    ((message)
     (synner message #f))
    ((message subform)
     (syntax-violation __module_who__ message (bless `(define-record-type ,foo ...)) subform)))

  (define-values (method-form*.sexp
		  early-binding-methods-alist-public
		  early-binding-methods-alist-protected
		  early-binding-methods-alist-private
		  virtual-method-signatures-alist)
    (parametrise ((current-table-of-names (make-hashtable string-hash string=?)))
      (%parse-method-clauses method-clause*.stx foo foo-for-id-generation
			     method-retriever-code-public.id method-retriever-code-private.id
			     parent-rtd.id parent-virtual-method-signatures-alist
			     early-binding-methods-alist-public.tail
			     early-binding-methods-alist-protected.tail
			     early-binding-methods-alist-private.tail
			     field-methods-alist-public
			     field-methods-alist-private
			     synner)))

  ;;NOTE  We  know  that  the  implementation  of  DEFINE-TYPE  embeds  the  prebuilt
  ;;RECORD.OTS in the visit code of the library (or program) being compiled.  So here
  ;;we mutate  that very object with  method informations.  Below, commented  out, is
  ;;the code  we should use  to include in  the visit code  the mutation of  the OTS.
  ;;(Marco Maggi; Sat Sep 3, 2016)
  ;;
  (begin
    ;;Build the return syntax object.
    (define output-form.stx (bless `(begin ,@method-form*.sexp)))
    (object-type-spec.methods-table-public-set!		record.ots early-binding-methods-alist-public)
    (object-type-spec.methods-table-protected-set!	record.ots early-binding-methods-alist-protected)
    (object-type-spec.methods-table-private-set!	record.ots early-binding-methods-alist-private)
    (record-type-spec.virtual-method-signatures-set!	record.ots virtual-method-signatures-alist))

  #;(begin
    (define foo-methods-table-public
      `(list . ,(map (lambda (entry)
		       `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
		  early-binding-methods-alist-public)))
    (define foo-methods-table-protected
      `(list . ,(map (lambda (entry)
		       `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
		  early-binding-methods-alist-protected)))
    (define foo-methods-table-private
      `(list . ,(map (lambda (entry)
		       `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
		  early-binding-methods-alist-private)))
    (define foo-virtual-method-signatures.table
      `(quote ,virtual-method-signatures-alist))
    ;;Build the return syntax object.
    (define output-form.stx
      (let ((record-ots.id	(make-syntactic-identifier-for-temporary-variable "record.ots")))
	(bless
	 `(begin
	    (begin-for-syntax
	      (let ((,record-ots.id (make-type-specification (syntax ,foo))))
	        ;;Update the record-type spec instance with method tables.
	        (object-type-spec.methods-table-public-set!		,record-ots.id ,foo-methods-table-public)
	        (object-type-spec.methods-table-protected-set!		,record-ots.id ,foo-methods-table-protected)
	        (object-type-spec.methods-table-private-set!		,record-ots.id ,foo-methods-table-private)
	        (record-type-spec.virtual-method-signatures-set!	,record-ots.id ,foo-virtual-method-signatures.table)))
	    ,@method-form*.sexp)))))

  ;;(debug-print __who__ (syntax->datum output-form.stx))
  output-form.stx)


(module (%parse-method-clauses)
  ;;Parse METHOD and similar clauses in  CLAUSE*.STX; build a list of "<method-spec>"
  ;;instances  representing  the  specifications;  perform some  validations  on  the
  ;;specifications; finally build and return the following values:
  ;;
  ;;*  METHOD-FORM*.SEXP, a  list of  forms representing  the definitions  of: method
  ;;implementation procedures and related  stuff; method-retriever functions for both
  ;;public methods  and private methods, to  be used when performing  late binding of
  ;;methods.
  ;;
  ;;* EARLY-BINDING-METHODS-ALIST-PUBLIC an alist for public methods having: as keys,
  ;;symbols  representing the  method names,  including the  field names;  as values,
  ;;syntactic identifiers that will be bound to the method implementation procedures,
  ;;including public field methods.
  ;;
  ;;* EARLY-BINDING-METHODS-ALIST-PROTECTED an alist for protected methods having: as
  ;;keys, symbols representing the method names, including the protected field names;
  ;;as values, syntactic identifiers that will  be bound to the method implementation
  ;;procedures, including protected field methods.
  ;;
  ;;*  EARLY-BINDING-METHODS-ALIST-PRIVATE an  alist for  private methods  having: as
  ;;keys, symbols representing  the method names, including the  private field names;
  ;;as values, syntactic identifiers that will  be bound to the method implementation
  ;;procedures, including private field methods.
  ;;
  ;;* VIRTUAL-METHOD-SIGNATURES-ALIST an alist  having: as keys, symbols representing
  ;;the virtual and sealed method names; as values:
  ;;
  ;;** When the method is an  open virtual method: instances of "<closure-type-spec>"
  ;;representing the type signature of the method.
  ;;
  ;;** When the method has been sealed: the boolean false.
  ;;
  ;;the alist has, as tail, the argument PARENT-VIRTUAL-METHOD-SIGNATURES-ALIST.
  ;;
  (define (%parse-method-clauses clause*.stx foo foo-for-id-generation
				 method-retriever-code-public.id method-retriever-code-private.id
				 parent-rtd.id parent-virtual-method-signatures-alist
				 early-binding-methods-alist-public
				 early-binding-methods-alist-protected
				 early-binding-methods-alist-private
				 field-methods-alist-public
				 field-methods-alist-private
				 synner)
    ;;The arguments are:
    ;;
    ;;FOO, the syntactic identifier representing the record-type name.
    ;;
    ;;FOO-FOR-ID-GENERATION,  the syntactic  identifier representing  the record-type
    ;;name preprocessed to be used as prefix in procnames generation.
    ;;
    ;;METHOD-RETRIEVER-CODE-PUBLIC.ID and METHOD-RETRIEVER-CODE-PRIVATE.ID, syntactic
    ;;identifiers  to which  the  method  retriever functions  will  be bound.   This
    ;;function generates the definition forms for these functions.
    ;;
    ;;PARENT-RTD.ID,  false  or   the  syntactic  identifier  bound   to  the  parent
    ;;record-type descriptor.   It is  used to  access the  parent's method-retriever
    ;;procedure.  This must be false if the parent of the record-type is "<record>".
    ;;
    ;;PARENT-VIRTUAL-METHOD-SIGNATURES-ALIST an alist having  entries with format:
    ;;
    ;;   (?method-name . (?protection . ?method-signature))
    ;;
    ;;in which:  ?METHOD-NAME is a symbol  representing a parent's virtual  or sealed
    ;;method name; ?PROTECTION is  a the fixnum 0 for public, 1  for protected, 2 for
    ;;private; ?METHOD-SIGNATURE is:
    ;;
    ;;* When the method is an open virtual method: instances of "<closure-type-spec>"
    ;;representing the type signature of the method.
    ;;
    ;;* When the method has been sealed: the boolean false.
    ;;
    ;;EARLY-BINDING-METHODS-ALIST-PUBLIC:  null  or  an alist  associated  to  public
    ;;methods  having:  as  keys,  symbols  representing  method  names;  as  values,
    ;;syntactic identifiers bound to the method implementation functions.  This alist
    ;;already holds entries for:  the fields of this record type;  the methods of the
    ;;parent record-type.  New entries must be prepended to this list.
    ;;
    ;;EARLY-BINDING-METHODS-ALIST-PROTECTED: null  or an  alist associated  to public
    ;;methods
    ;;
    ;;EARLY-BINDING-METHODS-ALIST-PRIVATE:  null or  an alist  associated to  private
    ;;methods.
    ;;
    ;;FIELD-METHODS-ALIST-PUBLIC:  null  or  an  alist associated  to  public  fields
    ;;having:  as  keys,  symbols  representing field  names;  as  values,  syntactic
    ;;identifiers bound to the field-method  implementation functions.  This alist is
    ;;the tail of LATE-BINDING-METHODS-ALIST-PUBLIC.
    ;;
    ;;FIELD-METHODS-ALIST-PRIVATE:  null or  an alist  associated to  private fields.
    ;;This alist is the tail of LATE-BINDING-METHODS-ALIST-PRIVATE.
    ;;
    (let* ((method-spec*	(%parse-clauses clause*.stx foo foo-for-id-generation synner))
	   (group*		(%partition-method-specs method-spec*)))
      (%check-that-no-method-has-the-same-name-of-a-field group* field-methods-alist-private synner)
      (%check-that-methods-with-the-same-name-have-homogeneous-type-and-protection group* synner)
      (%check-that-parent-sealed-methods-are-not-overridden group* parent-virtual-method-signatures-alist synner)
      (%check-that-overriding-methods-have-the-same-protection-of-the-parent-virtual-method
       group* parent-virtual-method-signatures-alist synner)
      (%process-method-specs group* foo parent-rtd.id parent-virtual-method-signatures-alist
			     method-retriever-code-public.id method-retriever-code-private.id
			     early-binding-methods-alist-public
			     early-binding-methods-alist-protected
			     early-binding-methods-alist-private
			     field-methods-alist-public
			     field-methods-alist-private
			     synner)))

  (define (%partition-method-specs method-spec*)
    ;;Given a  list of  "<method-spec>" instances:  return a  list of  sublists, each
    ;;sublist being a  non-empty proper list of "<method-spec>"  instances having the
    ;;same method name.
    ;;
    (if (pair? method-spec*)
	(let recur ((first (car method-spec*))
		    (spec* (cdr method-spec*)))
	  (receive (group rest)
	      (partition (lambda (spec)
			   (eq? (<method-spec>-name-sym first) (<method-spec>-name-sym spec)))
		spec*)
	    (cons (cons first group)
		  (if (pair? rest)
		      (recur (car rest) (cdr rest))
		    '()))))
      '()))

  (define (%check-that-no-method-has-the-same-name-of-a-field group* field-methods-alist synner)
    (for-each (lambda (group)
		(let ((method-name.sym (<method-spec>-name-sym (car group))))
		  (when (exists (lambda (field-method-entry)
				  (let ((field-name.sym (car field-method-entry)))
				    (eq? field-name.sym method-name.sym)))
			  field-methods-alist)
		    (synner "forbidden method with name equal to a field name" method-name.sym))))
      group*))

  (define (%check-that-methods-with-the-same-name-have-homogeneous-type-and-protection group* synner)
    (for-each (lambda (group)
		(let* ((head		(car group))
		       (tail		(cdr group))
		       (spec-pred	(cond ((<virtual-method-spec>? head)	<virtual-method-spec>?)
					      ((<seal-method-spec>?    head)	<seal-method-spec>?)
					      (else				<concrete-method-spec>?)))
		       (head-protection	(<method-spec>-protection head)))
		  (for-all (lambda (spec)
			     (unless (spec-pred spec)
			       (synner "forbidden methods with the same name but different type" (<method-spec>-name-sym head)))
			     (unless (eq? head-protection (<method-spec>-protection spec))
			       (synner "forbidden methods with the same name but different protection level" (<method-spec>-name-sym head))))
		    tail)))
      group*))

  (define (%check-that-parent-sealed-methods-are-not-overridden group* parent-virtual-method-signatures-alist synner)
    ;;Here we already know that every  group in GROUP* holds method specifications of
    ;;homogeneous type; so: if the head is  concrete all are concrete, if the head is
    ;;virtual all are virtual, if the head is sealed all are sealed.
    ;;
    (for-each
	(lambda (group)
	  (let ((method-name.sym (<method-spec>-name-sym (car group))))
	    (cond ((assq method-name.sym parent-virtual-method-signatures-alist)
		   => (lambda (entry)
			(if (vmsa.signature entry)
			    ;;The parent has a virtual method with this name.
			    (void)
			  ;;The parent has a sealed method with this name.
			  (synner "attempt to override a method declared sealed by the parent record-type"
				  method-name.sym))))
		  (else
		   ;;Fine, the  parent has  neither virtual  nor sealed  methods with
		   ;;this name.
		   (void)))))
      group*))

  (define (%check-that-overriding-methods-have-the-same-protection-of-the-parent-virtual-method
	   group* parent-virtual-method-signatures-alist synner)
    ;;Here we already know that every  group in GROUP* holds method specifications of
    ;;homogeneous  type and  protection level;  so: if  the head  is private  all are
    ;;private, if the head is protected all  are protected, if the head is public all
    ;;are public.
    ;;
    (for-each
	(lambda (group)
	  (let* ((head			(car group))
		 (method-name.sym	(<method-spec>-name-sym head))
		 (protection		(<method-spec>-protection head)))
	    (cond ((assq method-name.sym parent-virtual-method-signatures-alist)
		   => (lambda (entry)
			(cond ((vmsa.private-method? entry)
			       (synner "attempt to override a parent's method declared as private"
				       method-name.sym))
			      ((eq? protection (vmsa.protection-level entry))
			       ;;The parent has a virtual  or sealed method with this
			       ;;name  and the  same protection  level of  the group.
			       ;;Good.
			       (void))
			      (else
			       ;;The parent has a virtual  or sealed method with this
			       ;;name but different protection level.
			       (synner "attempt to override a parent's method with different protection level"
				       method-name.sym)))))
		  (else
		   ;;Fine, the  parent has  neither virtual  nor sealed  methods with
		   ;;this name.
		   (void)))))
      group*))

;;; --------------------------------------------------------------------

  (define-core-record-type <method-spec>
    (fields
      (immutable	name-id)
		;A syntactic identifier representing the method name.
      (immutable	name-sym)
		;A symbol representing the method name.
      (immutable	protection)
		;A  fixnum representing  the protection  level: 0  for public,  1 for
		;protected, 2 for private.
      (immutable	early-binding-procname)
		;The syntactic  identifier that  will be bound  to the  early binding
		;method implementation procedure.
      (immutable	early-binding-implementation)
		;A symbolic expression (to be BLESSed later) with the format:
		;
		;   (({?early-binding-procname . ?rv-types} . ?formals) . ?body)
		;
		;that  can  be  turned   into  a  method's  implementation  procedure
		;definition just by preconsing DEFINE/CHECKED or DEFINE/OVERLOAD.
      (immutable	late-binding-procname)
		;The  syntactic identifier  that will  be bound  to the  late binding
		;method implementation procedure.
      (immutable	late-binding-implementation)
		;A symbolic expression (to be BLESSed later) with the format:
		;
		;   (({?late-binding-procname . ?rv-types} . ?formals) . ?body)
		;
		;that  can  be  turned   into  a  method's  implementation  procedure
		;definition just by preconsing DEFINE/CHECKED or DEFINE/OVERLOAD.
      (immutable	closure-ots)
		;An instance of "<closure-type-spec>" representing the type signature
		;of this method.
      #| end of FIELDS |# )

    (protocol
      (lambda (make-record)
	(lambda (name-id name-sym protection early-binding-procname early-binding-meat late-binding-procname late-binding-meat
		    closure.ots)
	  (make-record name-id name-sym
		       (case protection
			 ((public)	0)
			 ((protected)	1)
			 ((private)	2)
			 (else
			  (assertion-violation 'make-<method-spec>
			    "internal error, invalid protection level" name-sym protection)))
		       early-binding-procname early-binding-meat
		       late-binding-procname late-binding-meat
		       closure.ots)))))

  (define-core-record-type <concrete-method-spec>
    (parent <method-spec>))

  (define-core-record-type <virtual-method-spec>
    (parent <method-spec>))

  (define-core-record-type <seal-method-spec>
    (parent <method-spec>))

  (define (<method-spec>-protection-public? spec)
    (eq? 0 (<method-spec>-protection spec)))

  (define (<method-spec>-protection-protected? spec)
    (eq? 1 (<method-spec>-protection spec)))

  (define (<method-spec>-protection-private? spec)
    (eq? 2 (<method-spec>-protection spec)))

;;; --------------------------------------------------------------------

  (module (%parse-clauses)

    (define (%parse-clauses clause*.stx foo foo-for-id-generation synner)
      ;;Parse  the  METHOD clauses  in  CLAUSE*,  return  a list  of  "<method-spec>"
      ;;instances.
      ;;
      (define-syntax-rule (recurse)
	(%parse-clauses (cdr clause*.stx) foo foo-for-id-generation synner))
      (if (pair? clause*.stx)
	  (let ((clause.stx (car clause*.stx)))
	    (if (syntax-match clause.stx (method virtual-method seal-method)
		  ((method         . ?stuff)	#t)
		  ((virtual-method . ?stuff)	#t)
		  ((seal-method    . ?stuff)	#t)
		  (_				#f))
		(cons (%parse-single-clause clause.stx foo foo-for-id-generation synner)
		      (recurse))
	      (recurse)))
	'()))

    (define (%parse-single-clause clause.stx foo foo-for-id-generation synner)
      (receive (clause.stx protection)
	  (syntax-match clause.stx (private public protected)
	    ((?keyword public . ?stuff)
	     (values (cons ?keyword ?stuff) 'public))
	    ((?keyword protected . ?stuff)
	     (values (cons ?keyword ?stuff) 'protected))
	    ((?keyword private . ?stuff)
	     (values (cons ?keyword ?stuff) 'private))
	    (_
	     (values clause.stx 'public)))
	(syntax-match clause.stx (brace)
	  ((?keyword (?who . ?formals) ?body0 ?body* ...)
	   (identifier? ?who)
	   (%process-clause-contents foo foo-for-id-generation ?keyword protection
				     (identifier->symbol ?who) ?who
				     #f ?formals `(,?body0 . ,?body*)
				     synner))

	  ((?keyword ((brace ?who ?rv-tag0 . ?rv-tag*) . ?formals) ?body0 . ?body*)
	   (identifier? ?who)
	   (%process-clause-contents foo foo-for-id-generation ?keyword protection
				     (identifier->symbol ?who) ?who
				     `(,?rv-tag0 . ,?rv-tag*) ?formals `(,?body0 . ,?body*)
				     synner))

	  (_
	   (synner "invalid syntax in method clause" clause.stx)))))

    (define (%process-clause-contents foo foo-for-id-generation keyword.id protection
				      method-name.sym method-name.id retvals.stx formals.stx body*.stx
				      synner)
      (let* ((early-method-procname.id	(%mk-method-procname-id foo-for-id-generation method-name.sym "-early"))
	     (late-method-procname.id	(%mk-method-procname-id foo-for-id-generation method-name.sym "-late"))
	     (subject.id		(make-syntactic-identifier-for-temporary-variable "subject")))

	(define early-binding-implementation-form.sexp
	  (let ((lhs.stx (if retvals.stx
			     `(brace ,early-method-procname.id . ,retvals.stx)
			   early-method-procname.id)))
	    `((,lhs.stx {,subject.id ,foo} . ,formals.stx)
	      ;;This  is the  only use  of TYPED-VARIABLE-WITH-PRIVATE-ACCESS!,  this
	      ;;syntax has  been designed to be  used here and nowhere  else.  (It is
	      ;;also used in label definitions with the same purpose.)
	      (typed-variable-with-private-access! ,subject.id)
	      ;;We really have to use the fluid syntax THIS, otherwise we will not be
	      ;;able to process correctly the clauses imported from mixins.
	      (fluid-let-syntax ((this (make-synonym-transformer (syntax ,subject.id))))
		. ,body*.stx))))

	(define late-binding-implementation-form.sexp
	  (receive (args.stx improper?)
	      (typed-formals-syntax->args-list formals.stx)
	    (let ((lhs.stx		(if retvals.stx
					    `(brace ,late-method-procname.id . ,retvals.stx)
					  late-method-procname.id))
		  (late-body.sexp	(let ((B (if (eq? protection 'public)
						     `(method-call-late-binding
						       (quote ,method-name.sym) #f ,subject.id ,@args.stx)
						   `(record-type-method-call-late-binding-private
						     (quote ,method-name.sym) ,subject.id ,@args.stx))))
					  (if improper? (cons 'apply B) B))))
	      `((,lhs.stx {,subject.id ,foo} . ,formals.stx)
		,late-body.sexp))))

	(define closure.ots
	  (make-closure-type-spec/from-typed-formals
	   (bless (let ((lhs.stx (if retvals.stx
				     `(brace _ . ,retvals.stx)
				   `(brace _ . <list>))))
		    `(,lhs.stx {,subject.id <bottom>} . ,formals.stx)))))

	(case (identifier->symbol keyword.id)
	  ((method)
	   (make-<concrete-method-spec> method-name.id method-name.sym protection
					early-method-procname.id early-binding-implementation-form.sexp
					late-method-procname.id   late-binding-implementation-form.sexp
					closure.ots))
	  ((virtual-method)
	   (make-<virtual-method-spec> method-name.id method-name.sym protection
				       early-method-procname.id early-binding-implementation-form.sexp
				       late-method-procname.id   late-binding-implementation-form.sexp
				       closure.ots))
	  ((seal-method)
	   (make-<seal-method-spec> method-name.id method-name.sym protection
				    early-method-procname.id early-binding-implementation-form.sexp
				    late-method-procname.id   late-binding-implementation-form.sexp
				    closure.ots))
	  (else
	   (synner "internal error, invalid method type" keyword.id)))))

    (case-define %mk-method-procname-id
      ((foo-for-id-generation method-name.sym)
       (%mk-method-procname-id foo-for-id-generation method-name.sym ""))
      ((foo-for-id-generation method-name.sym tail.str)
       (let* ((key	(string-append (identifier->string foo-for-id-generation)
				       "-"
				       (symbol->string method-name.sym)
				       tail.str)))
	 (or (hashtable-ref (current-table-of-names) key #f)
	     (receive-and-return (name)
		 (private-identifier-append foo-for-id-generation key)
	       (hashtable-set! (current-table-of-names) key name))))))

    #| end of module: %PARSE-CLAUSES |# )

;;; --------------------------------------------------------------------

  (module (%process-method-specs)

    (define (%process-method-specs init-group* foo parent-rtd.id parent-virtual-method-signatures-alist
				   method-retriever-code-public.id method-retriever-code-private.id
				   early-binding-methods-alist-public
				   early-binding-methods-alist-protected
				   early-binding-methods-alist-private
				   field-methods-alist-public
				   field-methods-alist-private
				   synner)
      ;;Process the method specifications, return the following values:
      ;;
      ;;* EARLY-BINDING-METHODS-ALIST-PUBLIC, the return value of the whole module.
      ;;
      ;;*  EARLY-BINDING-METHODS-ALIST-PROTECTED,  the  return  value  of  the  whole
      ;;module.
      ;;
      ;;* EARLY-BINDING-METHODS-ALIST-PRIVATE, the return value of the whole module.
      ;;
      ;;* METHOD-FORM*.SEXP, the return value of the whole module.
      ;;
      ;;* VIRTUAL-METHOD-SIGNATURES-ALIST  an alist having  the format of  the return
      ;;value of the  whole module, but not  yet including the entries  of the parent
      ;;record-type.
      ;;

      ;;Build the methods alist and the definitions forms.
      (let loop ((group*				init-group*)
		 ;;Only the public methods go in the public methods alist.
		 (early-binding-methods-alist-public	early-binding-methods-alist-public)
		 ;;Both the public and protected  methods go in the protected methods
		 ;;alist.
		 (early-binding-methods-alist-protected	early-binding-methods-alist-protected)
		 ;;All the methods go in the private alist.
		 (early-binding-methods-alist-private	early-binding-methods-alist-private)
		 ;;Only the public methods go in this late-binding alist.
		 (late-binding-methods-alist-public	field-methods-alist-public)
		 ;;All the methods go in this late-binding alist.
		 (late-binding-methods-alist-private	field-methods-alist-private)
		 (method-form*.sexp			'()))
	(if (pair? group*)
	    (let* ((group	(car group*))
		   (virtual?	(or (<virtual-method-spec>? (car group))
				    (let ((method-name.sym (<method-spec>-name-sym (car group))))
				      (exists (lambda (entry)
						(and (eq? method-name.sym (vmsa.method-name entry))
						     (vmsa.signature entry)
						     #t))
					parent-virtual-method-signatures-alist)))))
	      (if (list-of-single-item? group)
		  (%process-method-group-with-single-item (car group) virtual?
							  early-binding-methods-alist-public
							  early-binding-methods-alist-protected
							  early-binding-methods-alist-private
							  late-binding-methods-alist-public
							  late-binding-methods-alist-private
							  method-form*.sexp
							  (lambda (a b c d e f)
							    (loop (cdr group*) a b c d e f)))
		(%process-method-group-with-multiple-items group virtual?
							   early-binding-methods-alist-public
							   early-binding-methods-alist-protected
							   early-binding-methods-alist-private
							   late-binding-methods-alist-public
							   late-binding-methods-alist-private
							   method-form*.sexp
							   (lambda (a b c d e f)
							     (loop (cdr group*) a b c d e f)))))
	  ;;No more groups.
	  (let ((method-form*.sexp (cons* (%make-method-retriever-code foo parent-rtd.id method-retriever-code-public.id
								       late-binding-methods-alist-public   #f)
					  (%make-method-retriever-code foo parent-rtd.id method-retriever-code-private.id
								       late-binding-methods-alist-private #t)
					  method-form*.sexp)))
	    (values method-form*.sexp
		    early-binding-methods-alist-public
		    early-binding-methods-alist-protected
		    early-binding-methods-alist-private
		    (%make-virtual-method-signatures-alist init-group* parent-virtual-method-signatures-alist synner))))))

    ;;; --------------------------------------------------------------------

    (define (%process-method-group-with-single-item single virtual?
						    early-binding-methods-alist-public
						    early-binding-methods-alist-protected
						    early-binding-methods-alist-private
						    late-binding-methods-alist-public
						    late-binding-methods-alist-private
						    method-form*.sexp kont)
      ;;VIRTUAL? is true if the parent record-type has a virtual method with the same
      ;;name.  When VIRTUAL?  is true: this method  is virtual too, even  when it was
      ;;defined using METHOD rather than VIRTUAL-METHOD.
      ;;
      (receive (early-binding-method-entry late-binding-method-entry form*.sexp)
	  (if virtual?
	      ;;This is a virtual method.
	      (values
	       ;;early-binding-method-entry
	       (cons (<method-spec>-name-sym single) (<method-spec>-late-binding-procname  single))
	       ;;late-binding-method-entry
	       (cons (<method-spec>-name-id  single) (<method-spec>-early-binding-procname single))
	       ;;form*.sexp
	       (list (cons (core-prim-id 'define/checked) (<method-spec>-early-binding-implementation single))
		     (cons (core-prim-id 'define/checked) (<method-spec>-late-binding-implementation  single))))
	    ;;This is a concrete method.
	    (values
	     ;;early-binding-method-entry
	     (cons (<method-spec>-name-sym single) (<method-spec>-early-binding-procname single))
	     ;;late-binding-method-entry
	     (cons (<method-spec>-name-id  single) (<method-spec>-early-binding-procname single))
	     ;;form*.sexp
	     (list (cons (core-prim-id 'define/checked) (<method-spec>-early-binding-implementation single)))))
	(kont
	 ;;early-binding-methods-alist-public
	 (if (<method-spec>-protection-public? single)
	     (cons early-binding-method-entry early-binding-methods-alist-public)
	   early-binding-methods-alist-public)
	 ;;early-binding-methods-alist-protected
	 (if (or (<method-spec>-protection-public?    single)
		 (<method-spec>-protection-protected? single))
	     (cons early-binding-method-entry early-binding-methods-alist-protected)
	   early-binding-methods-alist-protected)
	 ;;early-binding-methods-alist-private
	 (cons early-binding-method-entry early-binding-methods-alist-private)
	 ;;late-binding-methods-alist-public
	 (if (<method-spec>-protection-public? single)
	     (cons late-binding-method-entry late-binding-methods-alist-public)
	   late-binding-methods-alist-public)
	 ;;late-binding-methods-alist-private
	 (cons late-binding-method-entry late-binding-methods-alist-private)
	 ;;method-form*.sexp
	 (append form*.sexp method-form*.sexp))))

    (define (%process-method-group-with-multiple-items group virtual?
						       early-binding-methods-alist-public
						       early-binding-methods-alist-protected
						       early-binding-methods-alist-private
						       late-binding-methods-alist-public
						       late-binding-methods-alist-private
						       method-form*.sexp kont)
      ;;VIRTUAL? is true if the parent record-type has a virtual method with the same
      ;;name.
      ;;
      (define first (car group))
      (receive (early-binding-method-entry late-binding-method-entry form*.sexp)
	  (if virtual?
	      ;;This is a virtual method.
	      (values
	       ;;early-binding-method-entry
	       (cons (<method-spec>-name-sym first) (<method-spec>-late-binding-procname  first))
	       ;;late-binding-method-entry
	       (cons (<method-spec>-name-id  first) (<method-spec>-early-binding-procname first))
	       ;;form*.sexp
	       (fold-left (lambda (knil spec)
			    (cons* (cons (core-prim-id 'define/overload) (<method-spec>-early-binding-implementation spec))
				   (cons (core-prim-id 'define/overload) (<method-spec>-late-binding-implementation  spec))
				   knil))
		 '() group))
	    ;;This is a concrete method.
	    (values
	     ;;early-binding-method-entry
	     (cons (<method-spec>-name-sym first) (<method-spec>-early-binding-procname first))
	     ;;late-binding-method-entry
	     (cons (<method-spec>-name-id  first) (<method-spec>-early-binding-procname first))
	     ;;form*.sexp
	     (map (lambda (spec)
		    (cons (core-prim-id 'define/overload) (<method-spec>-early-binding-implementation spec)))
	       group)))
	(kont
	 ;;early-binding-methods-alist-public
	 (if (<method-spec>-protection-public? first)
	     (cons early-binding-method-entry early-binding-methods-alist-public)
	   early-binding-methods-alist-public)
	 ;;early-binding-methods-alist-protected
	 (if (or (<method-spec>-protection-public?    first)
		 (<method-spec>-protection-protected? first))
	     (cons early-binding-method-entry early-binding-methods-alist-protected)
	   early-binding-methods-alist-protected)
	 ;;early-binding-methods-alist-private
	 (cons early-binding-method-entry early-binding-methods-alist-private)
	 ;;late-binding-methods-alist-public
	 (if (<method-spec>-protection-public? first)
	     (cons late-binding-method-entry late-binding-methods-alist-public)
	   late-binding-methods-alist-public)
	 ;;late-binding-methods-alist-private
	 (cons late-binding-method-entry late-binding-methods-alist-private)
	 ;;method-form*.sexp
	 (append form*.sexp method-form*.sexp))))

    ;;; --------------------------------------------------------------------

    (define (%make-method-retriever-code foo parent-rtd.id procname.id late-binding-methods-alist private?)
      ;;Build and return a symbolic expression (to be BLESSed later) representing the
      ;;definition of  a method  retriever function.  Given  a symbol  representing a
      ;;method name: such function returns the corresponding implementation function,
      ;;if any.
      ;;
      ;;If PRIVATE? is true: the generated function is for private methods, otherwise
      ;;it is for public methods.
      ;;
      (define method-name.id		(make-syntactic-identifier-for-temporary-variable "method-name"))
      (define parent-retriever.id	(make-syntactic-identifier-for-temporary-variable "parent-retriever"))
      `(define/typed {,procname.id <type-method-retriever>}
	 ,(cond ((pair? late-binding-methods-alist)
		 (let ((retriever-maker.sexp `(lambda/typed ({_ <type-method-retriever>} {,parent-retriever.id <type-method-retriever>})
						(lambda/typed ({_ (or <false> <procedure>)} {,method-name.id <symbol>})
						  (case ,method-name.id
						    ,@(map (lambda (P)
							     (let ((name (car P))
								   (proc (cdr P)))
							       `((,name) ,proc)))
							late-binding-methods-alist)
						    (else
						     (,parent-retriever.id ,method-name.id)))))))
		   `(,retriever-maker.sexp ,(if parent-rtd.id
						(if private?
						    `($record-type-method-retriever-private ,parent-rtd.id)
						  `($record-type-method-retriever ,parent-rtd.id))
					      '(core-type-descriptor.method-retriever <record>-ctd)))))

		(parent-rtd.id
		 ;;This record-type has no methods, but it has a parent.
		 (if private?
		     `($record-type-method-retriever-private ,parent-rtd.id)
		   `($record-type-method-retriever ,parent-rtd.id)))

		(else
		 ;;This record-type has no methods, and no parent.  The default parent is
		 ;;"<record>".
		 '(core-type-descriptor.method-retriever <record>-ctd)))))

    ;;; --------------------------------------------------------------------

    (define (%make-virtual-method-signatures-alist group* parent-virtual-method-signatures-alist synner)
      ;;Build  and return  VIRTUAL-METHOD-SIGNATURES-ALIST, an  alist having  entries
      ;;with format:
      ;;
      ;;   (?method-name . (?protection . ?method-signature))
      ;;
      ;;in which:  ?METHOD-NAME is a symbol  representing a virtual or  sealed method
      ;;name;  ?PROTECTION is  a the  fixnum 0  for public,  1 for  protected, 2  for
      ;;private; ?METHOD-SIGNATURE is:
      ;;
      ;;*   When   the   method   is   an   open   virtual   method:   instances   of
      ;;"<closure-type-spec>" representing the type signature of the method.
      ;;
      ;;* When the method has been sealed: the boolean false.
      ;;
      (fold-left (lambda (knil group)
		   (if (list-of-single-item? group)
		       (let ((single (car group)))
			 (%check-that-overriding-method-has-signature-compatible-with-overridden-method
			  (<method-spec>-name-sym single) (<method-spec>-closure-ots single)
			  parent-virtual-method-signatures-alist synner)
			 (cond ((<virtual-method-spec>? single)
				(cons (cons* (<method-spec>-name-sym    single)
					     (<method-spec>-protection  single)
					     (<method-spec>-closure-ots single))
				      knil))
			       ((<seal-method-spec>? single)
				(cons (cons* (<method-spec>-name-sym    single)
					     (<method-spec>-protection  single)
					     #f)
				      knil))
			       (else knil)))
		     (let* ((head		(car group))
			    (tail		(cdr group))
			    (signature.ots	(fold-left (lambda (closure.ots spec)
							     (closure-type-spec.join closure.ots (<method-spec>-closure-ots spec)))
						  (<method-spec>-closure-ots head) tail)))
		       (%check-that-overriding-method-has-signature-compatible-with-overridden-method
			(<method-spec>-name-sym head) signature.ots
			parent-virtual-method-signatures-alist synner)
		       (cond ((<virtual-method-spec>? head)
			      (cons (cons* (<method-spec>-name-sym   head)
					   (<method-spec>-protection head)
					   signature.ots)
				    knil))
			     ((<seal-method-spec>? head)
			      (cons (cons* (<method-spec>-name-sym   head)
					   (<method-spec>-protection head)
					   #f)
				    knil))
			     (else knil)))))
	parent-virtual-method-signatures-alist group*))

    (define (%check-that-overriding-method-has-signature-compatible-with-overridden-method
	     method-name.sym method-signature.ots parent-virtual-method-signatures-alist
	     synner)
      (cond ((assq method-name.sym parent-virtual-method-signatures-alist)
	     => (lambda (entry)
		  ;;There exists a  parent's method (virtual or sealed)  that has the
		  ;;same name.
		  (cond ((vmsa.signature entry)
			 => (lambda (parent-signature.ots)
			      ;;There exists  a parent's virtual method  that has the
			      ;;same name.
			      (unless (object-type-spec.matching-super-and-sub? parent-signature.ots method-signature.ots)
				(synner "method overriding parent's virtual method has wrong signature"
					method-name.sym)))))))))

    #| end of module: %PROCESS-METHOD-SPECS |# )

;;; --------------------------------------------------------------------
;;; accessors for the entries of PARENT-VIRTUAL-METHOD-SIGNATURES-ALIST

  ;;The ?ENTRY has the format:
  ;;
  ;;   (?method-name . (?protection . ?method-signature))
  ;;

  (define-syntax-rule (vmsa.method-name ?entry)
    (car ?entry))

  (define-syntax-rule (vmsa.protection-level ?entry)
    (cadr ?entry))

  (define-syntax-rule (vmsa.signature ?entry)
    (cddr ?entry))

  (define-syntax-rule (vmsa.public-method? ?entry)
    (eq? 0 (vmsa.protection-level ?entry)))

  (define-syntax-rule (vmsa.protected-method? ?entry)
    (eq? 1 (vmsa.protection-level ?entry)))

  (define-syntax-rule (vmsa.private-method? ?entry)
    (eq? 2 (vmsa.protection-level ?entry)))

  #| end of module: %PARSE-METHOD-CLAUSES |# )


(define* (%make-type-name-syntactic-binding-form foo.id foo-uid make-foo.id foo?.id
						 foo-super-rcd.sym foo-destructor.sym
						 foo-parent.id foo-rtd.sym foo-rcd.sym
						 foo-equality-predicate.id
						 foo-comparison-procedure.id
						 foo-hash-function.id
						 implemented-interface*.ots)
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
  ;;IMPLEMENTED-INTERFACE*.OTS  null  or  a proper  list  of  "<interface-type-spec>"
  ;;instances representing the interfaces that this record-type implements.
  ;;

  (define hash-func.id
    (or foo-hash-function.id
	(core-prim-id 'record-hash)))

  ;; (define foo-methods-table-public
  ;;   `(list . ,(map (lambda (entry)
  ;; 		     `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
  ;; 		early-binding-methods-alist-public)))

  ;; (define foo-methods-table-protected
  ;;   `(list . ,(map (lambda (entry)
  ;; 		     `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
  ;; 		early-binding-methods-alist-protected)))

  ;; (define foo-methods-table-private
  ;;   `(list . ,(map (lambda (entry)
  ;; 		     `(cons (quote ,(car entry)) (syntax ,(cdr entry))))
  ;; 		early-binding-methods-alist-private)))

  ;; (define foo-virtual-method-signatures.table
  ;;   `(quote ,virtual-method-signatures-alist))

  (define implemented-interfaces
    (if (null? implemented-interface*.ots)
	'(quote ())
      `(list ,@(map (lambda (iface.ots)
		      `(quote ,iface.ots))
		 implemented-interface*.ots))))

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
			  '() ;foo-methods-table-public
			  '() ;foo-methods-table-protected
			  '() ;foo-methods-table-private
			  '() ;foo-virtual-method-signatures.table
			  ,implemented-interfaces))


(define (%make-implemented-interfaces-table-code foo implemented-interface*.ots)
  ;;Return  false or  a symbolic  expression (to  be BLESSed  later) representing  an
  ;;expression  which,   expanded  and  evaluated,  will   return  the  record-type's
  ;;implemented interfaces table.
  ;;
  ;;This expression  must return  a vector  having one  item for  each interface-type
  ;;implemented by this record-type, with format:
  ;;
  ;;   #((?interface-uid . ?method-retriever) ...)
  ;;
  ;;where: ?INTERFACE-UID is the UID  of the interface-type; ?METHOD-RETRIEVER is the
  ;;method retriever  function for  the method  implementation procedures  defined by
  ;;this record-type.
  ;;
  ;;The  core  primitive  BUILD-TABLE-FOR-INTERFACE-TYPES-AND-IMPLEMENTER-OBJECT-TYPE
  ;;verifies  that  the record-type  actually  implements  the interface-type.   When
  ;;successful:  it  returns an  alist  having:  as  keys, symbols  representing  the
  ;;interface-type's method names; as values,  the syntactic identifiers bound to the
  ;;object-type's  method   implementation  procedures.    Otherwise  it   raises  an
  ;;exception.
  ;;
  (if (null? implemented-interface*.ots)
      #f
    `(expand-time-expr
      (quasisyntax
       (vector (unsyntax-splicing
		(build-table-for-interface-types-and-implementer-object-type
		 (quote ,implemented-interface*.ots)
		 (make-type-specification (syntax ,foo)))))))))


;;;; done

#| end of module: DEFINE-RECORD-TYPE-MACRO |# )


;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
