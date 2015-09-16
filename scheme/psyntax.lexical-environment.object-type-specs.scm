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

	 ;; object-type utilities
	 object-type-spec.subtype-and-supertype?	object-type-spec-override-predicate

	 <r6rs-record-type-spec>
	 make-r6rs-record-type-spec			r6rs-record-type-spec?
	 r6rs-record-type-spec.rtd-id			r6rs-record-type-spec.rcd-id
	 r6rs-record-type-spec.super-protocol-id

	 <built-in-object-type-spec>
	 make-built-in-object-type-spec			built-in-object-type-spec?)


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
     (immutable constructor-stx		object-type-spec.constructor-sexp)
		;False or a  syntax object representing an  expression that, expanded
		;and  evaluated at  run-time, returns  the default  constructor.  The
		;constructor is meant to be used as:
		;
		;   (?constructor ?arg ...)
		;
		;and called explicitly with the NEW syntax.
		;
		;The   constructor  can   be  a   syntax  or   core  operation   like
		;"$make-clean-vector" or a closure object  like "vector" or the maker
		;of R6RS records.

     (immutable destructor-stx		object-type-spec.destructor-sexp)
		;False or a  syntax object representing an  expression that, expanded
		;and  evaluated  at run-time,  returns  a  destructor function.   The
		;constructor is meant to be used as:
		;
		;   (?destructor ?instance)
		;
		;and called explicitly with the DELETE syntax.
		;
		;At present only structs and records have a destructor.

     (mutable type-predicate-stx
	      object-type-spec.type-predicate-sexp
	      object-type-spec.type-predicate-sexp-set!)
		;False or a  syntax object representing an  expression that, expanded
		;and evaluated at run-time, returns  a type predicate.  The predicate
		;is meant to be used as:
		;
		;   (?predicate ?object)
		;
		;and called explicitly with the IS-A? syntax.
		;
		;The type  predicate can be a  syntax or core operation  or a closure
		;object like "vector?" or the predicate of R6RS records.

     (immutable safe-accessors-table		object-type-spec.safe-accessors-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;syntax objects which, expanded and evaluated at run-time, return the
		;associated safe  field accessor.   A field accessor  is meant  to be
		;used as:
		;
		;   (?accessor ?instance)
		;
		;and called explicitly with the SLOT-REF syntax.

     (immutable safe-mutators-table		object-type-spec.safe-mutators-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;syntax objects which, expanded and evaluated at run-time, return the
		;associated safe field mutator.  A field  mutator is meant to be used
		;as:
		;
		;   (?mutator ?instance ?new-field-value)
		;
		;and called explicitly with the SLOT-SET! syntax.

     (immutable methods-table			object-type-spec.methods-table)
		;Null or  an alist mapping  symbols representing the method  names to
		;syntax objects which, expanded and evaluated at run-time, return the
		;associated method applicable.  A method is meant to be used as:
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
		       #f  ;constructor-stx
		       #f  ;destructor-stx
		       #f  ;type-predicate-stx
		       '() ;safe-accessors-table
		       '() ;safe-mutators-table
		       '() ;methods-table
		       ))
	 ((parent-id
	   constructor-stx destructor-stx type-predicate-stx
	   safe-accessors-table safe-mutators-table methods-table)
	  (make-record parent-id
		       constructor-stx destructor-stx type-predicate-stx
		       safe-accessors-table safe-mutators-table methods-table)))))

    #| end of DEFINE-RECORD-TYPE |# )

  #| end of module |# )


;;;; basic object-type specification: accessor, mutator, method retrieval

(module (object-type-spec.safe-accessor-sexp
	 object-type-spec.safe-mutator-sexp
	 object-type-spec.applicable-method-sexp)

  (define* (object-type-spec.safe-accessor-sexp {spec object-type-spec?} field-name.sym lexenv)
    ;;SPEC must an object-type specification record.  FIELD-NAME.SYM must be a symbol
    ;;representing a field name in the object-type specification.
    ;;
    ;;If FIELD-NAME.SYM is  EQ?  to the name  of a object's field:  return a symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded  and  evaluated  at  run-time,  returns  the  field's  safe  accessor;
    ;;otherwise return false.
    ;;
    (%spec-actor spec field-name.sym lexenv object-type-spec.safe-accessors-table))

  (define* (object-type-spec.safe-mutator-sexp {spec object-type-spec?} field-name.sym lexenv)
    ;;SPEC must an object-type specification record.  FIELD-NAME.SYM must be a symbol
    ;;representing a field name in the object-type specification.
    ;;
    ;;If FIELD-NAME.SYM is  EQ?  to the name  of a object's field:  return a symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated at run-time, returns the field's safe mutator; otherwise
    ;;return false.
    ;;
    (%spec-actor spec field-name.sym lexenv object-type-spec.safe-mutators-table))

  (define* (object-type-spec.applicable-method-sexp {spec object-type-spec?} method-name.sym lexenv)
    ;;SPEC  must an  object-type  specification record.   METHOD-NAME.SYM  must be  a
    ;;symbol representing a method name in the object-type specification.
    ;;
    ;;If METHOD-NAME.SYM is EQ?  to the name  of a object's method: return a symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated at run-time,  returns the method's applicable; otherwise
    ;;return false.
    ;;
    (%spec-actor spec method-name.sym lexenv object-type-spec.methods-table))
    ;; (cond ((assq method-name.sym (object-type-spec.methods-table spec))
    ;; 	   ;;The method name  is known; extract the symbolic expression  from the alist
    ;; 	   ;;entry and return it.
    ;; 	   => cdr)
    ;; 	  ((let loop ((parent-id (object-type-spec.parent-id spec)))
    ;; 	     (and parent-id
    ;; 		  (let* ((descr (id->object-type-binding-descriptor #f #f parent-id lexenv))
    ;; 			 (spec^ (syntactic-binding-descriptor.value descr)))
    ;; 		    (cond ((assq method-name.sym (object-type-spec.methods-table spec^))
    ;; 			   => cdr)
    ;; 			  (else
    ;; 			   (loop (object-type-spec.parent-id spec^))))))))
    ;; 	  (else #f)))


  (define (%spec-actor spec name.sym lexenv table-getter)
    ;;TABLE-GETTER  must be  a  function  which, applied  to  the  SPEC, returns  the
    ;;required association list.
    (cond ((assq name.sym (table-getter spec))
	   ;;The field name is known; extract  the symbolic expression from the alist
	   ;;entry and return it.
	   => cdr)
	  ((let loop ((parent-id (object-type-spec.parent-id spec)))
	     (and parent-id
		  (let* ((descr (id->object-type-binding-descriptor #f #f parent-id lexenv))
			 (spec^ (syntactic-binding-descriptor.value descr)))
	  	    (cond ((assq name.sym (table-getter spec^))
	  		   => cdr)
	  		  (else
	  		   (loop (object-type-spec.parent-id spec^))))))))
	  (else #f)))

  #| end of module |# )


;;;; basic object-type specification: ancestor predicate

(define* (object-type-spec.subtype-and-supertype? {sub-spec object-type-spec?} {super-spec object-type-spec?} lexenv)
  ;;Return true if SUB-SPEC is a subtype of SUPER-SPEC; otherwise return false.
  ;;
  (define-syntax-rule (recurse ?sub-spec)
    (object-type-spec.subtype-and-supertype? ?sub-spec super-spec lexenv))
  (or (eq? sub-spec super-spec)
      (cond ((object-type-spec.parent-id sub-spec)
	     => (lambda (parent-id)
		  (recurse (syntactic-binding-descriptor.value
			    (id->object-type-binding-descriptor #f #f parent-id lexenv)))))
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
  (let* ((descr  (id->object-type-binding-descriptor #f #f name.id (current-inferior-lexenv)))
	 (spec   (syntactic-binding-descriptor.value descr)))
    (object-type-spec.type-predicate-sexp-set! spec predicate.stx)))


;;;; R6RS record-type specification

;;This record  type is  used as  syntactic binding descriptor  for R6RS  record types
;;defined with the syntactic layer.  The lexenv entry has the format:
;;
;;   ($record-type-name . #<r6rs-record-type-spec>)
;;
;;It is built  when expanding DEFINE-RECORD-TYPE forms, or by  converting a syntactic
;;binding "$core-rtd" or  "$core-record-type-name" (a buit-in record  type defined by
;;the boot image) into a syntactic binding "$record-type-name".
;;
;;Lexical variables  bound to  instances of  this type  should be  called RTS  (as in
;;"record-type spec").
;;
(define-record-type (<r6rs-record-type-spec> make-r6rs-record-type-spec r6rs-record-type-spec?)
  (nongenerative vicare:expander:<r6rs-record-type-spec>)
  (parent <object-type-spec>)
  (fields
   (immutable rtd-id			r6rs-record-type-spec.rtd-id)
		;The syntactic identifier bound to the record-type descriptor.
   (immutable rcd-id			r6rs-record-type-spec.rcd-id)
		;The syntactic identifier bound to the record-constructor descriptor.
   (immutable super-protocol-id		r6rs-record-type-spec.super-protocol-id)
		;False if this record-type  has no super-type constructor descriptor;
		;otherwise the syntactic identifier to which the super-RCD is bound.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-lambda
       ((rtd-id rcd-id)
	((make-object-type-spec) rtd-id rcd-id #f))

       ((rtd-id rcd-id super-protocol-id parent-id
		constructor.sexp destructor.sexp predicate.sexp
		safe-accessors-table safe-mutators-table methods-table)
	(let ((constructor.sexp  (or constructor.sexp
				     `(record-constructor ,rcd-id)))
	      (predicate.sexp    (or predicate.sexp
				     (let ((arg.sym (gensym)))
				       `(lambda (,arg.sym)
					  (record-and-rtd? ,arg.sym ,rtd-id))))))
	  ((make-object-type-spec parent-id
				  constructor.sexp destructor.sexp predicate.sexp
				  safe-accessors-table safe-mutators-table methods-table)
	   rtd-id rcd-id super-protocol-id))))))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; built-in object-type specification

;;This record type is used as syntactic binding descriptor for built-in Vicare object
;;types:  fixnums, pairs,  strings, vectors,  et cetera.   The lexenv  entry has  the
;;format:
;;
;;   ($built-in-object-type-name . #<built-in-object-type-spec>)
;;
;;It is built  when expanding DEFINE-RECORD-TYPE forms, or by  converting a syntactic
;;binding "$core-rtd" or  "$core-record-type-name" (a buit-in record  type defined by
;;the boot image) into a syntactic binding "$record-type-name".
;;
;;Lexical variables  bound to  instances of  this type  should be  called OTS  (as in
;;"object-type spec").
;;
(define-record-type (<built-in-object-type-spec> make-built-in-object-type-spec built-in-object-type-spec?)
  (nongenerative vicare:expander:<built-in-object-type-spec>)
  (parent <object-type-spec>)
  (protocol
    (lambda (make-object-type-spec)
      (lambda (parent-id constructor.sexp predicate.sexp methods-table)
	(let ((destructor.sexp		#f)
	      (safe-accessors-table	'())
	      (safe-mutators-table	'()))
	  ((make-object-type-spec parent-id
				  constructor.sexp destructor.sexp predicate.sexp
				  safe-accessors-table safe-mutators-table methods-table))))))
  #| end of DEFINE-RECORD-TYPE |# )


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
