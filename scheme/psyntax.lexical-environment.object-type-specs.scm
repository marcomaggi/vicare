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
	 object-type-spec.constructor-stx		object-type-spec.destructor-stx
	 object-type-spec.type-predicate-stx
	 object-type-spec.safe-accessors-table		object-type-spec.safe-mutators-table
	 object-type-spec.unsafe-accessors-table	object-type-spec.unsafe-mutators-table

	 <r6rs-record-type-spec>
	 make-r6rs-record-type-spec			r6rs-record-type-spec?
	 r6rs-record-type-spec.rtd-id			r6rs-record-type-spec.rcd-id
	 r6rs-record-type-spec.parent-id
	 r6rs-record-type-spec.default-constructor-id	r6rs-record-type-spec.default-destructor-id
	 r6rs-record-type-spec.type-predicate-id
	 r6rs-record-type-spec.safe-accessors-table	r6rs-record-type-spec.safe-mutators-table
	 r6rs-record-type-spec.unsafe-accessors-table	r6rs-record-type-spec.unsafe-mutators-table

	 r6rs-record-type-spec.safe-accessor		r6rs-record-type-spec.safe-mutator
	 r6rs-record-type-spec.unsafe-accessor		r6rs-record-type-spec.unsafe-mutator
	 )


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
	 object-type-spec.constructor-stx
	 object-type-spec.destructor-stx
	 object-type-spec.type-predicate-stx
	 object-type-spec.safe-accessors-table
	 object-type-spec.safe-mutators-table
	 object-type-spec.unsafe-accessors-table
	 object-type-spec.unsafe-mutators-table)

  (define-record-type (<object-type-spec> make-object-type-spec object-type-spec?)
    (nongenerative vicare:expander:<object-type-spec>)
    (fields
     (immutable constructor-stx		object-type-spec.constructor-stx)
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

     (immutable destructor-stx		object-type-spec.destructor-stx)
		;False or a  syntax object representing an  expression that, expanded
		;and  evaluated  at run-time,  returns  a  destructor function.   The
		;constructor is meant to be used as:
		;
		;   (?destructor ?instance)
		;
		;and called explicitly with the DELETE syntax.
		;
		;At present only structs and records have a destructor.

     (immutable type-predicate-stx		object-type-spec.type-predicate-stx)
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

     (immutable unsafe-accessors-table		object-type-spec.unsafe-accessors-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;syntax objects which, expanded and evaluated at run-time, return the
		;associated unsafe field  accessor.  A field accessor is  meant to be
		;used as:
		;
		;   (?accessor ?instance)
		;
		;and called explicitly with the $SLOT-REF syntax.

     (immutable unsafe-mutators-table		object-type-spec.unsafe-mutators-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;syntax objects which, expanded and evaluated at run-time, return the
		;associated unsafe  field mutator.   A field mutator  is meant  to be
		;used as:
		;
		;   (?mutator ?instance ?new-field-value)
		;
		;and called explicitly with the $SLOT-SET! syntax.

     #| end of FIELDS |# )

    (protocol
      (lambda (make-record)
	(case-lambda
	 (()
	  (make-record #f  ;constructor-stx
		       #f  ;destructor-stx
		       #f  ;type-predicate-stx
		       '() ;safe-accessors-table
		       '() ;safe-mutators-table
		       '() ;unsafe-accessors-table
		       '() ;unsafe-mutators-table
		       ))
	 ((constructor-stx destructor-stx type-predicate-stx
			   safe-accessors-table safe-mutators-table
			   unsafe-accessors-table unsafe-mutators-table)
	  (make-record constructor-stx destructor-stx type-predicate-stx
		       safe-accessors-table safe-mutators-table
		       unsafe-accessors-table unsafe-mutators-table)))))

    #| end of DEFINE-RECORD-TYPE |# )

  #| end of module |# )


;;;; R6RS record-type specification

;;This record  type is  used as  syntactic binding descriptor  for R6RS  record types
;;defined with the syntactic layer.  The lexenv entry has the format:
;;
;;   ($record-type-name . #<r6rs-record-type-spec>)
;;
;;It is built  when expanding DEFINE-RECORD-TYPE forms, or by  converting a syntactic
;;binding  "$core-rtd" (a  buit-in record  type  defined by  the boot  image) into  a
;;syntactic binding "$record-type-name".
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
   (immutable parent-id			r6rs-record-type-spec.parent-id)
		;False  or a  syntactic identifier  representing the  parent of  this
		;record-type.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-lambda
       ((rtd-id rcd-id)
	((make-object-type-spec) rtd-id rcd-id #f))

       ((rtd-id rcd-id parent-id
		default-constructor-id default-destructor-id type-predicate-id
		safe-accessors-table safe-mutators-table
		unsafe-accessors-table unsafe-mutators-table)
	((make-object-type-spec default-constructor-id default-destructor-id type-predicate-id
				safe-accessors-table safe-mutators-table
				unsafe-accessors-table unsafe-mutators-table)
	 rtd-id rcd-id parent-id)))))
  #| end of DEFINE-RECORD-TYPE |# )

(define (r6rs-record-type-spec.default-constructor-id spec)
  (object-type-spec.constructor-stx spec))

(define (r6rs-record-type-spec.default-destructor-id spec)
  (object-type-spec.destructor-stx spec))

(define (r6rs-record-type-spec.type-predicate-id spec)
  (object-type-spec.type-predicate-stx spec))

(define (r6rs-record-type-spec.safe-accessors-table spec)
  (object-type-spec.safe-accessors-table spec))

(define (r6rs-record-type-spec.safe-mutators-table spec)
  (object-type-spec.safe-mutators-table spec))

(define (r6rs-record-type-spec.unsafe-accessors-table spec)
  (object-type-spec.unsafe-accessors-table spec))

(define (r6rs-record-type-spec.unsafe-mutators-table spec)
  (object-type-spec.unsafe-mutators-table spec))


;;;; R6RS record-type specification: accessor and mutator retrieval

(module (r6rs-record-type-spec.safe-accessor
	 r6rs-record-type-spec.safe-mutator
	 r6rs-record-type-spec.unsafe-accessor
	 r6rs-record-type-spec.unsafe-mutator)

  (define* (r6rs-record-type-spec.safe-accessor {rts r6rs-record-type-spec?} field-name.sym synner)
    ;;RTS must a R6RS record-type specification.
    ;;
    ;;FIELD-NAME.SYM must  be a symbol representing  a field name in  the record-type
    ;;definition.   SYNNER must  be  the  closure object  usable  to raise  syntactic
    ;;violations.
    ;;
    ;;If FIELD-NAME.SYM is EQ?  to the name of a record's field: return the syntactic
    ;;identifier bound to  its safe accessor; otherwise return  a symbolic expression
    ;;(to be BLESSed later) representing the expression:
    ;;
    ;;   (record-accessor ?rtd-id '?field-name)
    ;;
    ;;which, expanded and evaluated, returns a fresh safe accessor.
    ;;
    (%spec-actor rts field-name.sym r6rs-record-type-spec.safe-accessors-table 'record-accessor synner))

  (define (r6rs-record-type-spec.safe-mutator rts field-name.sym synner)
    ;;RTS must a R6RS record-type specification.
    ;;
    ;;FIELD-NAME.SYM must  be a symbol representing  a field name in  the record-type
    ;;definition.   SYNNER must  be  the  closure object  usable  to raise  syntactic
    ;;violations.
    ;;
    ;;If FIELD-NAME.SYM is EQ?  to the name of a record's field: return the syntactic
    ;;identifier bound  to its safe  mutator; otherwise return a  symbolic expression
    ;;(to be BLESSed later) representing the expression:
    ;;
    ;;   (record-mutator ?rtd-id '?field-name)
    ;;
    ;;which, expanded and evaluated, returns a fresh safe mutator.
    ;;
    (%spec-actor rts field-name.sym r6rs-record-type-spec.safe-mutators-table  'record-mutator synner))

  (define (r6rs-record-type-spec.unsafe-accessor rts field-name.sym synner)
    ;;RTS must a R6RS record-type specification.
    ;;
    ;;FIELD-NAME.SYM must  be a symbol representing  a field name in  the record-type
    ;;definition.   SYNNER must  be  the  closure object  usable  to raise  syntactic
    ;;violations.
    ;;
    ;;If FIELD-NAME.SYM is EQ?  to the name of a record's field: return the syntactic
    ;;identifier bound to its unsafe accessor; otherwise return a symbolic expression
    ;;(to be BLESSed later) representing the expression:
    ;;
    ;;   (record-accessor ?rtd-id '?field-name)
    ;;
    ;;which, expanded and evaluated, returns a fresh unsafe accessor.
    ;;
    (%spec-actor rts field-name.sym r6rs-record-type-spec.unsafe-accessors-table 'unsafe-record-accessor synner))

  (define (r6rs-record-type-spec.unsafe-mutator rts field-name.sym synner)
    ;;RTS must a R6RS record-type specification.
    ;;
    ;;FIELD-NAME.SYM must  be a symbol representing  a field name in  the record-type
    ;;definition.   SYNNER must  be  the  closure object  usable  to raise  syntactic
    ;;violations.
    ;;
    ;;If FIELD-NAME.SYM is EQ?  to the name of a record's field: return the syntactic
    ;;identifier bound to its unsafe  mutator; otherwise return a symbolic expression
    ;;(to be BLESSed later) representing the expression:
    ;;
    ;;   (record-mutator ?rtd-id '?field-name)
    ;;
    ;;which, expanded and evaluated, returns a fresh unsafe mutator.
    ;;
    (%spec-actor rts field-name.sym r6rs-record-type-spec.unsafe-mutators-table  'unsafe-record-mutator synner))

  (define (%spec-actor rts field-name.sym table-getter actor-constructor synner)
    ;;Given  an  R6RS record-type  specification  and  an identifier  representing  a
    ;;record's field name: return a symbolic  expression (that must be BLESSed later)
    ;;representing an expression which, expanded  and evaluated, returns the accessor
    ;;or mutator for the named field.
    ;;
    ;;TABLE-GETTER must be a function which,  applied to the record spec, returns the
    ;;required association list.
    ;;
    ;;ACTOR-CONSTRUCTOR must be one  of the symbols: record-accessor, record-mutator,
    ;;unsafe-record-accessor, unsafe-record-mutator;  these are  the public  names of
    ;;the core primitives building the accessors and mutators.
    ;;
    (cond ((assq field-name.sym (table-getter rts))
	   ;;The field name  is known and known is the  syntactic identifier bound to
	   ;;its accessor  or mutator.  Extract  the identifier from the  alist entry
	   ;;and return it.
	   => cdr)
	  ;; ((let loop ((parent-id (r6rs-record-type-spec.parent-id rts)))
	  ;;    (and parent-id
	  ;; 	  (let* ((descr  (id->record-type-name-binding-descriptor #f #f parent-id (current-inferior-lexenv)))
	  ;; 		 (rts    (syntactic-binding-descriptor.value descr)))
	  ;; 	    (cond ((assq field-name.sym (table-getter rts))
	  ;; 		   => cdr)
	  ;; 		  (else
	  ;; 		   (loop (r6rs-record-type-spec.parent-id rts))))))))
	  (else
	   ;;Fallback to the common field accessor or mutator constructor.
	   (let ((rtd-id (r6rs-record-type-spec.rtd-id rts)))
	     `(,actor-constructor ,rtd-id (quote ,field-name.sym))))))

  #| end of module |# )


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
