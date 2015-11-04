;;;Copyright (c) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module (<object-type-spec>
	 object-type-spec?
	 object-type-spec.parent-id
	 object-type-spec.constructor-sexp		object-type-spec.destructor-sexp
	 object-type-spec.type-predicate-sexp
	 object-type-spec.safe-accessor-sexp		object-type-spec.safe-mutator-sexp
	 object-type-spec.applicable-method-sexp
	 object-type-spec.memoised-list-id		object-type-spec.memoised-list-id-set!

	 object-type-spec.subtype-and-supertype?	object-type-spec-override-predicate

	 <scheme-type-spec>
	 make-scheme-type-spec				scheme-type-spec?

	 <closure-type-spec>
	 make-closure-type-spec				closure-type-spec?
	 closure-type-spec.signature

	 <list-type-spec>
	 <list-type-spec>-rtd				<list-type-spec>-rcd
	 make-list-type-spec				list-type-spec?
	 list-type-spec.type-id

	 <vector-type-spec>
	 <vector-type-spec>-rtd				<vector-type-spec>-rcd
	 make-vector-type-spec				vector-type-spec?
	 vector-type-spec.type-id

	 <struct-type-spec>
	 make-struct-type-spec				struct-type-spec?
	 struct-type-spec.std

	 <record-type-spec>
	 make-record-type-spec				record-type-spec?
	 record-type-spec.rtd-id			record-type-spec.rcd-id
	 record-type-spec.super-protocol-id)


;;;; basic object-type specification
;;
;;This  record-type is  used as  base  type for  all the  Scheme objects  expand-time
;;specifications.
;;
;;We must handle this type as if it  is an "abstract" type: we must never instantiate
;;it directly, rather we  must define subtype and instantiate that.   This is why the
;;maker of "<object-type-spec>" is not exported by the module.
;;
(define-record-type (<object-type-spec> make-object-type-spec object-type-spec?)
  (nongenerative vicare:expander:<object-type-spec>)
  (fields
   (immutable parent-id			object-type-spec.parent-id)
		;False  or a  syntactic identifier  representing the  parent of  this
		;record-type.

   (immutable constructor-sexp		object-type-spec.constructor-sexp)
		;A  boolean value  or a  symbolic  expression (to  be BLESSed  later)
		;representing  a Scheme  expression that,  expanded and  evaluated at
		;run-time, returns the default constructor function.
		;
		;When  this field  is #f:  this  object-type has  no constructor,  so
		;trying to use the syntax NEW will cause an expand-time exception.
		;
		;When  this field  is #t:  this object-type  has no  constructor, but
		;requires the  object to be  supplied in its already-built  form. For
		;example:
		;
		;   (new <fixnum> 123)
		;
		;must expand to:
		;
		;   (assert-signature-and-return (<fixnum>) 123)
		;
		;When this field  is a symbolic expression: the  constructor is meant
		;to be used as:
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
		;
		;This field is mutable because:  when defining the type specification
		;of record-types  representing condition  objects, we  want to  set a
		;predicate  that  works  with  both simple  conditions  and  compound
		;conditions.

   (immutable safe-accessors-table		object-type-spec.safe-accessors-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;symbolic  expressions  (to  be BLESSed  later)  representing  Scheme
		;expressions  that, expanded  and evaluated  at run-time,  return the
		;associated safe  field accessor.   A field accessor  is meant  to be
		;used as:
		;
		;   (?accessor ?instance)
		;
		;and called explicitly with the SLOT-REF syntax.

   (immutable safe-mutators-table		object-type-spec.safe-mutators-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;symbolic  expressions  (to  be BLESSed  later)  representing  Scheme
		;expressions  that, expanded  and evaluated  at run-time,  return the
		;associated safe field mutator.  A field  mutator is meant to be used
		;as:
		;
		;   (?mutator ?instance ?new-field-value)
		;
		;and called explicitly with the SLOT-SET! syntax.

   (immutable methods-table			object-type-spec.methods-table)
		;Null or  an alist mapping  symbols representing the method  names to
		;symbolic  expressions  (to  be BLESSed  later)  representing  Scheme
		;expressions  that, expanded  and evaluated  at run-time,  return the
		;associated method.  A method is meant to be used as:
		;
		;   (?method ?instance ?arg ...)
		;
		;and called explicitly with the METHOD-CALL syntax.

   (mutable memoised-list-id
	    object-type-spec.memoised-list-id
	    object-type-spec.memoised-list-id-set!)
		;False or  a type identifier  representing a (possibly  empty) proper
		;list of objects of this type.
		;
		;For example, if we create a list type identifier with:
		;
		;   (define-syntax <fixnum*>
		;      (make-list-type-spec #'<fixnum>))
		;
		;the  syntactic  identifier  "<fixnum*>"   is  stored  in  the  field
		;"memoised-list-id" of  the "<object-type-spec>"  instance associated
		;to the  type identifier "<fixnum>".  This  way multiple applications
		;of  MAKE-LIST-TYPE-SPEC  to #'<fixnum>  return  the  same list  type
		;specification object.

   #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (case-lambda
       (()
	(make-record #f	   ;parent-id
		     #f	   ;constructor-sexp
		     #f	   ;destructor-sexp
		     #f	   ;type-predicate-sexp
		     '()   ;safe-accessors-table
		     '()   ;safe-mutators-table
		     '()   ;methods-table
		     #f	   ;memoised-list-id
		     ))
       ((parent-id
	 constructor-sexp destructor-sexp type-predicate-sexp
	 safe-accessors-table safe-mutators-table methods-table)
	(make-record parent-id
		     constructor-sexp destructor-sexp type-predicate-sexp
		     safe-accessors-table safe-mutators-table methods-table
		     #f #;memoised-list-id
		     )))))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(module (object-type-spec.safe-accessor-sexp
	 object-type-spec.safe-mutator-sexp
	 object-type-spec.applicable-method-sexp)

  (define* (object-type-spec.safe-accessor-sexp {ots object-type-spec?} field-name.sym lexenv)
    ;;OTS must an object-type specification  record.  FIELD-NAME.SYM must be a symbol
    ;;representing a field name in the object-type specification.
    ;;
    ;;If  FIELD-NAME.SYM  is EQ?   to  an  object's  field  name: return  a  symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded  and  evaluated  at  run-time,  returns  the  field's  safe  accessor;
    ;;otherwise return false.
    ;;
    (%sexp-retriever ots field-name.sym lexenv object-type-spec.safe-accessors-table))

  (define* (object-type-spec.safe-mutator-sexp {ots object-type-spec?} field-name.sym lexenv)
    ;;OTS must an object-type specification  record.  FIELD-NAME.SYM must be a symbol
    ;;representing a field name in the object-type specification.
    ;;
    ;;If  FIELD-NAME.SYM  is EQ?   to  an  object's  field  name: return  a  symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated at run-time, returns the field's safe mutator; otherwise
    ;;return false.
    ;;
    (%sexp-retriever ots field-name.sym lexenv object-type-spec.safe-mutators-table))

  (define* (object-type-spec.applicable-method-sexp {ots object-type-spec?} method-name.sym lexenv)
    ;;OTS must an object-type specification record.  METHOD-NAME.SYM must be a symbol
    ;;representing a method name in the object-type specification.
    ;;
    ;;If  METHOD-NAME.SYM is  EQ?   to an  object's method  name:  return a  symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated at run-time,  returns the method's applicable; otherwise
    ;;return false.
    ;;
    (%sexp-retriever ots method-name.sym lexenv object-type-spec.methods-table))

  (define (%sexp-retriever ots name.sym lexenv table-getter)
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

;;; --------------------------------------------------------------------
;;; ancestor predicate

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

;;; --------------------------------------------------------------------
;;; miscellaneous operations

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
;;Scheme objects, not records, not structs.
;;
(define-record-type (<scheme-type-spec> make-scheme-type-spec scheme-type-spec?)
  (nongenerative vicare:expander:<scheme-type-spec>)
  (parent <object-type-spec>)
  (protocol
    (lambda (make-object-type-spec)
      (define (make-scheme-type-spec parent-id constructor.sexp predicate.sexp methods-table)
	(let ((destructor.sexp		#f)
	      (safe-accessors-table	'())
	      (safe-mutators-table	'()))
	  ((make-object-type-spec parent-id
				  constructor.sexp destructor.sexp predicate.sexp
				  safe-accessors-table safe-mutators-table methods-table))))
      make-scheme-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; list object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<nlist>" representing non-empty proper list objects holding items of a known type.
;;
;;There  can be  only one  instance of  type "<list-type-spec>"  representing a  list
;;collecting items of a specific type.
;;
(define-record-type (<list-type-spec> make-list-type-spec list-type-spec?)
  (nongenerative vicare:expander:<list-type-spec>)
  (parent <scheme-type-spec>)
  (sealed #t)
  (fields (immutable type-id		list-type-spec.type-id))
		;A type identifier representing the type of items.
  (protocol
    (lambda (make-scheme-type-spec)
      ;;This table maps the label gensym of  item's type identifier to the OTS of the
      ;;associated list type.
      ;;
      ;;FIXME  Entries should  be  removed from  this table  whenever  the list  type
      ;;identifier is garbage collected.  (Marco Maggi; Sun Nov 1, 2015)
      (define table
	(make-eq-hashtable))
      (define* (make-list-type-spec {type-id type-identifier?})
	(let ((label (id->label type-id)))
	  (or (hashtable-ref table label #f)
	      (receive-and-return (ots)
		  (let ((parent-id		(nlist-tag-id))
			(constructor.sexp	#f)
			(predicate.sexp		`(make-list-of-predicate (is-a? _ ,type-id)))
			(methods-table		'()))
		    ((make-scheme-type-spec parent-id constructor.sexp predicate.sexp methods-table)
		     type-id))
		(hashtable-set! table label ots)))))
      make-list-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define <list-type-spec>-rtd
  (record-type-descriptor <list-type-spec>))

(define <list-type-spec>-rcd
  (record-constructor-descriptor <list-type-spec>))


;;;; vector object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<vector>" representing vector objects holding items of a known type.
;;
;;There can be  only one instance of type "<vector-type-spec>"  representing a vector
;;collecting items of a specific type.
;;
(define-record-type (<vector-type-spec> make-vector-type-spec vector-type-spec?)
  (nongenerative vicare:expander:<vector-type-spec>)
  (parent <scheme-type-spec>)
  (sealed #t)
  (fields (immutable type-id		vector-type-spec.type-id))
		;A type identifier representing the type of items.
  (protocol
    (lambda (make-scheme-type-spec)
      ;;This table maps the label gensym of  item's type identifier to the OTS of the
      ;;associated vector type.
      ;;
      ;;FIXME Entries  should be  removed from  this table  whenever the  vector type
      ;;identifier is garbage collected.  (Marco Maggi; Sun Nov 1, 2015)
      (define table
	(make-eq-hashtable))
      (define* (make-vector-type-spec {item-type-id type-identifier?})
	(let ((label (id->label item-type-id)))
	  (or (hashtable-ref table label #f)
	      (receive-and-return (ots)
		  (let ((parent-id		(vector-tag-id))
			(constructor.sexp	#f)
			(predicate.sexp		#f)
			(methods-table		'()))
		    ((make-scheme-type-spec parent-id constructor.sexp predicate.sexp methods-table)
		     item-type-id))
		(hashtable-set! table label ots)))))
      make-vector-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define <vector-type-spec>-rtd
  (record-type-descriptor <vector-type-spec>))

(define <vector-type-spec>-rcd
  (record-constructor-descriptor <vector-type-spec>))


;;;; closure object signature spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<procedure>" representing closure objects defined in the source code.
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
      (define* (make-closure-type-spec {signature callable-signature?})
	(let ((parent-id		(procedure-tag-id))
	      (constructor.sexp		#f)
	      (predicate.sexp		#f)
	      (methods-table		'()))
	  ((make-scheme-type-spec parent-id constructor.sexp predicate.sexp methods-table)
	   signature)))
      make-closure-type-spec))

  #| end of DEFINE-RECORD-TYPE |# )


;;;; Vicare's struct-type specification
;;
;;This record-type is  used as syntactic binding descriptor's  value for struct-types
;;defined by DEFINE-STRUCT.
;;
;;Lexical variables  bound to  instances of  this type  should be  called STS  (as in
;;"Struct-Type Spec") or STRUCT-OTS.
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
;;
;;This  record-type  is  used  as  syntactic  binding  descriptor's  value  for  R6RS
;;record-types.
;;
;;Lexical  variables bound  to instances  of this  type and  its sub-types  should be
;;called RTS (as in "Record-Type Spec") or RECORD-OTS.
;;
(define-record-type (<record-type-spec> make-record-type-spec record-type-spec?)
  (nongenerative vicare:expander:<record-type-spec>)
  (parent <object-type-spec>)
  (fields
   (immutable rtd-id			record-type-spec.rtd-id)
		;The syntactic identifier bound to the record-type descriptor.
   (immutable rcd-id			record-type-spec.rcd-id)
		;The syntactic identifier bound to the record-constructor descriptor.
   (immutable super-protocol-id		record-type-spec.super-protocol-id)
		;False  if  this  record-type has  no  super-type  record-constructor
		;descriptor;  otherwise   the  syntactic  identifier  to   which  the
		;super-RCD is bound.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define (make-record-type-spec rtd-id rcd-id super-protocol-id parent-id
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
	   rtd-id rcd-id super-protocol-id)))
      make-record-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
