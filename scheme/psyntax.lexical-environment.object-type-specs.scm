;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module (<object-type-spec>
	 object-type-spec?
	 object-type-spec.parent-id
	 object-type-spec.constructor-sexp		object-type-spec.destructor-sexp
	 object-type-spec.type-predicate-sexp
	 object-type-spec.safe-accessor-sexp		object-type-spec.safe-mutator-sexp
	 object-type-spec.applicable-method-sexp

	 object-type-spec.subtype-and-supertype?	object-type-spec-override-predicate

	 <scheme-type-spec>				scheme-type-spec?

	 <closure-type-spec>
	 make-closure-type-spec				closure-type-spec?
	 closure-type-spec.signature

	 <core-scheme-type-spec>
	 make-core-scheme-type-spec			core-scheme-type-spec?

	 <struct-type-spec>
	 make-struct-type-spec				struct-type-spec?
	 struct-type-spec.std

	 <record-type-spec>				record-type-spec?
	 record-type-spec.rtd-id			record-type-spec.rcd-id
	 record-type-spec.super-protocol-id

	 <syntactic-record-type-spec>
	 make-syntactic-record-type-spec		syntactic-record-type-spec?

	 <core-record-type-spec>
	 make-core-record-type-spec			core-record-type-spec?

	 <core-condition-type-spec>
	 make-core-condition-type-spec			core-condition-type-spec?)


;;;; basic object-type specification

;;This  record-type is  used as  root  type for  all the  Scheme objects  expand-time
;;specification.
;;
;;We must handle this type as if it  is an "abstract" type: we must never instantiate
;;it directly, rather we  must define subtype and instantiate that.   This is why the
;;maker of "<object-type-spec>" is not exported by the module.
;;
(module (<object-type-spec>
	 object-type-spec?
	 object-type-spec.parent-id
	 object-type-spec.constructor-sexp
	 object-type-spec.destructor-sexp
	 object-type-spec.type-predicate-sexp
	 object-type-spec.type-predicate-sexp-set!
	 object-type-spec.safe-accessors-table
	 object-type-spec.safe-mutators-table
	 object-type-spec.methods-table)

  (define-record-type (<object-type-spec> make-object-type-spec object-type-spec?)
    (nongenerative vicare:expander:<object-type-spec>)
    (fields
     (immutable parent-id		object-type-spec.parent-id)
		;False  or a  syntactic identifier  representing the  parent of  this
		;record-type.
     (immutable constructor-sexp		object-type-spec.constructor-sexp)
		;False or a symbolic expression  (to be BLESSed later) representing a
		;Scheme expression that, expanded  and evaluated at run-time, returns
		;the default constructor.  The constructor is meant to be used as:
		;
		;   (?constructor ?arg ...)
		;
		;and called explicitly with the NEW syntax.
		;
		;The   constructor  can   be  a   syntax  or   core  operation   like
		;"$make-clean-vector" or a closure object  like "vector" or the maker
		;of R6RS records.

     (immutable destructor-sexp		object-type-spec.destructor-sexp)
		;False or a symbolic expression  (to be BLESSed later) representing a
		;Scheme expression that, expanded  and evaluated at run-time, returns
		;a destructor function.  The destructor is meant to be used as:
		;
		;   (?destructor ?instance)
		;
		;and called explicitly with the DELETE syntax.

     (mutable type-predicate-sexp
	      object-type-spec.type-predicate-sexp
	      object-type-spec.type-predicate-sexp-set!)
		;False or a symbolic expression  (to be BLESSed later) representing a
		;Scheme expression that, expanded  and evaluated at run-time, returns
		;a type predicate.  The predicate is meant to be used as:
		;
		;   (?predicate ?object)
		;
		;and called explicitly with the IS-A? syntax.
		;
		;The type  predicate can be a  syntax or core operation  or a closure
		;object like "vector?" or the predicate of R6RS records.

     (immutable safe-accessors-table		object-type-spec.safe-accessors-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;symbolic  expressions  (to  be BLESSed  later)  representing  Scheme
		;expression  that, expanded  and  evaluated at  run-time, return  the
		;associated safe  field accessor.   A field accessor  is meant  to be
		;used as:
		;
		;   (?accessor ?instance)
		;
		;and called explicitly with the SLOT-REF syntax.

     (immutable safe-mutators-table		object-type-spec.safe-mutators-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;symbolic  expressions  (to  be BLESSed  later)  representing  Scheme
		;expression  that, expanded  and  evaluated at  run-time, return  the
		;associated safe field mutator.  A field  mutator is meant to be used
		;as:
		;
		;   (?mutator ?instance ?new-field-value)
		;
		;and called explicitly with the SLOT-SET! syntax.

     (immutable methods-table			object-type-spec.methods-table)
		;Null or  an alist mapping  symbols representing the method  names to
		;symboli  expression  (to  be   BLESSed  later)  representing  Scheme
		;expressions  that, expanded  and evaluated  at run-time,  return the
		;associated method  applicable.  A method  applicable is meant  to be
		;used as:
		;
		;   (?method ?instance ?arg ...)
		;
		;and called explicitly with the METHOD-CALL syntax.

     #| end of FIELDS |# )

    (protocol
      (lambda (make-record)
	(case-lambda
	 (()
	  (make-record #f  ;parent-id
		       #f  ;constructor-sexp
		       #f  ;destructor-sexp
		       #f  ;type-predicate-sexp
		       '() ;safe-accessors-table
		       '() ;safe-mutators-table
		       '() ;methods-table
		       ))
	 ((parent-id
	   constructor-sexp destructor-sexp type-predicate-sexp
	   safe-accessors-table safe-mutators-table methods-table)
	  (make-record parent-id
		       constructor-sexp destructor-sexp type-predicate-sexp
		       safe-accessors-table safe-mutators-table methods-table)))))

    #| end of DEFINE-RECORD-TYPE |# )

  #| end of module |# )


;;;; basic object-type specification: accessor, mutator, method retrieval

(module (object-type-spec.safe-accessor-sexp
	 object-type-spec.safe-mutator-sexp
	 object-type-spec.applicable-method-sexp)

  (define* (object-type-spec.safe-accessor-sexp {ots object-type-spec?} field-name.sym lexenv)
    ;;OTS must an object-type specification  record.  FIELD-NAME.SYM must be a symbol
    ;;representing a field name in the object-type specification.
    ;;
    ;;If FIELD-NAME.SYM is  EQ?  to the name  of a object's field:  return a symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded  and  evaluated  at  run-time,  returns  the  field's  safe  accessor;
    ;;otherwise return false.
    ;;
    (%spec-actor ots field-name.sym lexenv object-type-spec.safe-accessors-table))

  (define* (object-type-spec.safe-mutator-sexp {ots object-type-spec?} field-name.sym lexenv)
    ;;OTS must an object-type specification  record.  FIELD-NAME.SYM must be a symbol
    ;;representing a field name in the object-type specification.
    ;;
    ;;If FIELD-NAME.SYM is  EQ?  to the name  of a object's field:  return a symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated at run-time, returns the field's safe mutator; otherwise
    ;;return false.
    ;;
    (%spec-actor ots field-name.sym lexenv object-type-spec.safe-mutators-table))

  (define* (object-type-spec.applicable-method-sexp {ots object-type-spec?} method-name.sym lexenv)
    ;;OTS must an object-type specification record.  METHOD-NAME.SYM must be a symbol
    ;;representing a method name in the object-type specification.
    ;;
    ;;If METHOD-NAME.SYM is EQ?  to the name  of a object's method: return a symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated at run-time,  returns the method's applicable; otherwise
    ;;return false.
    ;;
    (%spec-actor ots method-name.sym lexenv object-type-spec.methods-table))

  (define (%spec-actor ots name.sym lexenv table-getter)
    ;;TABLE-GETTER must be a function which, applied to the OTS, returns the required
    ;;association list.
    (cond ((assq name.sym (table-getter ots))
	   ;;The field name is known; extract  the symbolic expression from the alist
	   ;;entry and return it.
	   => cdr)
	  ((let loop ((parent-id (object-type-spec.parent-id ots)))
	     (and parent-id
		  (let ((ots^ (id->object-type-specification #f #f parent-id lexenv)))
	  	    (cond ((assq name.sym (table-getter ots^))
	  		   => cdr)
	  		  (else
	  		   (loop (object-type-spec.parent-id ots^))))))))
	  (else #f)))

  #| end of module |# )


;;;; basic object-type specification: ancestor predicate

(define* (object-type-spec.subtype-and-supertype? {sub-ots object-type-spec?} {super-ots object-type-spec?} lexenv)
  ;;Return true if SUB-OTS is a subtype of SUPER-OTS; otherwise return false.
  ;;
  (define-syntax-rule (recurse ?sub-ots)
    (object-type-spec.subtype-and-supertype? ?sub-ots super-ots lexenv))
  (or (eq? sub-ots super-ots)
      (cond ((object-type-spec.parent-id sub-ots)
	     => (lambda (parent-id)
		  (recurse (id->object-type-specification #f #f parent-id lexenv))))
	    (else #f))))


;;;; basic object-type specification: miscellaneous operations

(define (object-type-spec-override-predicate name.id predicate.stx)
  ;;Assume  NAME.ID  is a  syntactic  identifier  representing an  object-type  name.
  ;;Override the predicate  of the object-type with the  syntax object PREDICATE.STX.
  ;;Return unspecified values.
  ;;
  ;;This is  used to override the  predicate of condition object  record-types, which
  ;;must work with both simple conditions and compound conditions.
  ;;
  (let ((ots (id->object-type-specification #f #f name.id (current-inferior-lexenv))))
    (object-type-spec.type-predicate-sexp-set! ots predicate.stx)))


;;;; built-in Scheme object-type specification
;;
;;This record-type  is the  base type  for all  the type  specifications representing
;;Scheme objects.  It must not be instantiated directly.
;;
(define-record-type (<scheme-type-spec> dummy-make-scheme-type-spec scheme-type-spec?)
  (nongenerative vicare:expander:<scheme-type-spec>)
  (parent <object-type-spec>)
  (protocol
    (lambda (make-object-type-spec)
      (define (make-scheme-type-spec type-id parent-id constructor.sexp predicate.sexp methods-table)
	(let ((constructor.sexp         (or constructor.sexp
					    (let ((arg (gensym)))
					      `(named-lambda* ,type-id ({,arg ,predicate.sexp})
						 ,arg))))
	      (destructor.sexp		#f)
	      (safe-accessors-table	'())
	      (safe-mutators-table	'()))
	  ((make-object-type-spec parent-id
				  constructor.sexp destructor.sexp predicate.sexp
				  safe-accessors-table safe-mutators-table methods-table))))
      make-scheme-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; closure object signature spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<procedure>" representing closure objects defined  in the source code.  The lexenv
;;entry has the format:
;;
;;   (local-object-type-name . (#<closure-type-spec> . ?expanded-expr))
;;
;;It is built  when expanding a DEFINE,  LAMBDA or CASE-LAMBDA form  to represent the
;;signature of arguments and return values.
;;
;;NOTE There is  no predicate sexp because,  at run-time, there is no  way to inspect
;;the signature of a closure object.
;;
(define-record-type (<closure-type-spec> make-closure-type-spec closure-type-spec?)
  (nongenerative vicare:expander:<closure-type-spec>)
  (parent <scheme-type-spec>)

  (fields
   (immutable signature		closure-type-spec.signature)
		;An instance of "<callable-signature>".
   #| end of FIELDS |# )

  (protocol
    (lambda (make-scheme-type-spec)
      (define (make-closure-type-spec type-id signature)
	(let ((parent-id		(procedure-tag-id))
	      (constructor.sexp		#f)
	      (predicate.sexp		#f)
	      (methods-table		'()))
	  ((make-scheme-type-spec type-id parent-id constructor.sexp predicate.sexp methods-table)
	   signature)))
      make-closure-type-spec))

  #| end of DEFINE-RECORD-TYPE |# )


;;;; core Scheme object-type specification
;;
;;This  record-type is  used as  syntactic binding  descriptor's values  for built-in
;;Vicare object types: fixnums, pairs, strings, vectors, et cetera.  The LEXENV entry
;;has the format:
;;
;;   (local-object-type-name . (#<core-scheme-type-spec> . ?hard-coded-sexp))
;;
;;It is built at run-time by converting entries with format:
;;
;;   ($core-scheme-type-name . ?hard-coded-sexp)
;;
;;where ?HARD-CODED-SEXP has the format:
;;
;;   (?parent-name ?constructor-name ?type-predicate-name ?methods-alist)
;;
;;The source entries are  defined by the boot image's makefile  and are hard-coded in
;;the boot  image itself.  Whenever the  function LABEL->SYNTACTIC-BINDING-DESCRIPTOR
;;is used to retrieve the descriptor from the label: the descriptor is converted from
;;the hard-coded format to the format holding this value.
;;
(define-record-type (<core-scheme-type-spec> make-core-scheme-type-spec core-scheme-type-spec?)
  (nongenerative vicare:expander:<core-scheme-type-spec>)
  (parent <scheme-type-spec>)
  (protocol
    (lambda (make-scheme-type-spec)
      (define (make-core-scheme-type-spec type-id parent-id
					  constructor.sexp predicate.sexp methods-table)
	((make-scheme-type-spec type-id parent-id constructor.sexp predicate.sexp methods-table)))
      make-core-scheme-type-spec)))


;;;; Vicare's struct-type specification

;;This record type is used as  syntactic binding descriptor's values for struct types
;;defined by DEFINE-STRUCT.  The lexenv entry has the format:
;;
;;   (local-object-type-name . (#<struct-type-spec> . ?expanded-expr))
;;
;;Lexical variables  bound to  instances of  this type  should be  called STS  (as in
;;"Struct-Type Spec").
;;
(define-record-type (<struct-type-spec> make-struct-type-spec struct-type-spec?)
  (nongenerative vicare:expander:<struct-type-spec>)
  (parent <object-type-spec>)
  (fields
   (immutable std			struct-type-spec.std)
		;The struct-type descriptor object.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define (make-struct-type-spec std
				     constructor.id predicate.id
				     safe-accessors-table safe-mutators-table methods-table)
	(let ((parent-id         (core-prim-id '<struct>))
	      (destructor.sexp   `(internal-applicable-struct-type-destructor ,std)))
	  ((make-object-type-spec parent-id
				  constructor.id destructor.sexp predicate.id
				  safe-accessors-table safe-mutators-table methods-table)
	   std)))
      make-struct-type-spec))
  #| end of DEFINE-STRUCT-TYPE |# )


;;;; R6RS's record-type specification

;;This record type  is used as syntactic binding descriptor's  values for R6RS record
;;types.  This  type must never  be instantiated  directly, rather sub-types  must be
;;created and instantiated.
;;
;;Lexical  variables bound  to instances  of this  type and  its sub-types  should be
;;called RTS (as in "Record-Type Spec").
;;
(define-record-type (<record-type-spec> dummy-make-record-type-spec record-type-spec?)
  (nongenerative vicare:expander:<record-type-spec>)
  (parent <object-type-spec>)
  (fields
   (immutable rtd-id			record-type-spec.rtd-id)
		;The syntactic identifier bound to the record-type descriptor.
   (immutable rcd-id			record-type-spec.rcd-id)
		;The syntactic identifier bound to the record-constructor descriptor.
   (immutable super-protocol-id		record-type-spec.super-protocol-id)
		;False if this record-type  has no super-type constructor descriptor;
		;otherwise the syntactic identifier to which the super-RCD is bound.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define make-record-type-spec
	((rtd-id rcd-id)
	 ((make-object-type-spec) rtd-id rcd-id #f))

	((rtd-id rcd-id super-protocol-id parent-id
		 constructor.sexp destructor.sexp predicate.sexp
		 safe-accessors-table safe-mutators-table methods-table)
	 (let ((constructor.sexp  (or constructor.sexp
				      `(record-constructor ,rcd-id)))
	       (predicate.sexp    (or predicate.sexp
				      (let ((arg.sym (gensym)))
					`(internal-lambda (unsafe) (,arg.sym)
					   (record-and-rtd? ,arg.sym ,rtd-id))))))
	   ((make-object-type-spec parent-id
				   constructor.sexp destructor.sexp predicate.sexp
				   safe-accessors-table safe-mutators-table methods-table)
	    rtd-id rcd-id super-protocol-id))))
      make-record-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; syntactic R6RS's record-type specification
;;
;;This record-type is  used as syntactic binding descriptor's values  for R6RS record
;;types defined with the syntactic layer.  The lexenv entry has the format:
;;
;;   (local-object-type-name . (#<syntactic-record-type-spec> . ?expanded-expr))
;;
(define-record-type (<syntactic-record-type-spec> make-syntactic-record-type-spec syntactic-record-type-spec?)
  (nongenerative vicare:expander:<syntactic-record-type-spec>)
  (parent <record-type-spec>)
  (protocol
    (lambda (make-record-type-spec)
      (define (make-syntactic-record-type-spec rtd-id rcd-id
					       super-protocol-id parent-id
					       constructor.sexp destructor.sexp predicate.sexp
					       safe-accessors-table safe-mutators-table methods-table)
	((make-record-type-spec rtd-id rcd-id
				super-protocol-id parent-id
				constructor.sexp destructor.sexp predicate.sexp
				safe-accessors-table safe-mutators-table methods-table)))
      make-syntactic-record-type-spec)))


;;;; core R6RS's record-type specification

;;This record type  is used as syntactic binding descriptor's  values for R6RS record
;;types defined by the boot image.  The lexenv entry has the format:
;;
;;   (local-object-type-name . (#<core-record-type-spec> . ?hard-coded-sexp))
;;
;;where ?HARD-CODED-SEXP has one of the formats:
;;
;;   (?rtd-name ?rcd-name)
;;
;;Instances of this type are built by the expander from syntactic binding descriptors
;;of type "$core-rtd" and "$core-record-type-name",  which are hard-coded in the boot
;;image and  generated directly by the  makefile at boot image  build-time.  Whenever
;;the function LABEL->SYNTACTIC-BINDING-DESCRIPTOR is used to retrieve the descriptor
;;from  the label:  the descriptor  is converted  from the  hard-coded format  to the
;;format holding this value.
;;
(define-record-type (<core-record-type-spec> make-core-record-type-spec core-record-type-spec?)
  (nongenerative vicare:expander:<core-record-type-spec>)
  (parent <record-type-spec>)
  (protocol
    (lambda (make-record-type-spec)
      (case-define make-core-record-type-spec
	((rtd-id rcd-id)
	 ((make-record-type-spec rtd-id rcd-id)))

	((rtd-id rcd-id super-protocol-id parent-id
		 constructor.sexp destructor.sexp predicate.sexp
		 safe-accessors-table safe-mutators-table methods-table)
	 ((make-record-type-spec rtd-id rcd-id super-protocol-id parent-id
				 constructor.sexp destructor.sexp predicate.sexp
				 safe-accessors-table safe-mutators-table methods-table))))
      make-core-record-type-spec)))


;;;; core R6RS's condition object record-type specification

;;This record type is used as syntactic binding descriptor's value for R6RS condition
;;object types defined by the boot image (exmples: "&who", "&error", et cetera).  The
;;lexenv entry has the format:
;;
;;   (local-object-type-name . (#<core-condition-type-spec> . #f))
;;
;;Instances of this type are built by the expander from syntactic binding descriptors
;;of type "$core-condition-object-type-name", which are  hard-coded in the boot image
;;and generated  directly by  the makefile  at boot  image build-time.   Whenever the
;;function  LABEL->SYNTACTIC-BINDING-DESCRIPTOR is  used to  retrieve the  descriptor
;;from  the label:  the descriptor  is converted  from the  hard-coded format  to the
;;format holding this value.
;;
(define-record-type (<core-condition-type-spec> make-core-condition-type-spec core-condition-type-spec?)
  (nongenerative vicare:expander:<core-condition-type-spec>)
  (parent <core-record-type-spec>)
  (protocol
    (lambda (make-core-record-type-spec)
      (case-define make-core-condition-type-spec
	((rtd-id rcd-id)
	 ((make-core-record-type-spec rtd-id rcd-id)))

	((rtd-id rcd-id super-protocol-id parent-id
		 constructor.sexp destructor.sexp predicate.sexp
		 safe-accessors-table safe-mutators-table methods-table)
	 ((make-core-record-type-spec rtd-id rcd-id super-protocol-id parent-id
				      constructor.sexp destructor.sexp predicate.sexp
				      safe-accessors-table safe-mutators-table methods-table))))
      make-core-condition-type-spec)))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
