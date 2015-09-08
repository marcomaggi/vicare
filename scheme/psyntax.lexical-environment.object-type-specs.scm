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


(module (<r6rs-record-type-spec>
	 make-r6rs-record-type-spec			r6rs-record-type-spec?
	 r6rs-record-type-spec.rtd-id			r6rs-record-type-spec.rcd-id
	 r6rs-record-type-spec.parent-id
	 r6rs-record-type-spec.default-constructor-id	r6rs-record-type-spec.type-predicate-id
	 r6rs-record-type-spec.safe-accessors-table	r6rs-record-type-spec.safe-mutators-table
	 r6rs-record-type-spec.unsafe-accessors-table	r6rs-record-type-spec.unsafe-mutators-table

	 r6rs-record-type-spec.safe-accessor		r6rs-record-type-spec.safe-mutator
	 r6rs-record-type-spec.unsafe-accessor		r6rs-record-type-spec.unsafe-mutator
	 )


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
  (fields
   (immutable rtd-id r6rs-record-type-spec.rtd-id)
		;The syntactic identifier bound to the record-type descriptor.
   (immutable rcd-id r6rs-record-type-spec.rcd-id)
		;The syntactic identifier bound to the record-constructor descriptor.
   (immutable parent-id r6rs-record-type-spec.parent-id)
		;If this record  type has no parent or the  parent is unknown: false.
		;If this  record type has a  known parent defined with  the syntactic
		;layer: a syntactic identifier representing the parent type name.
   (immutable default-constructor-id r6rs-record-type-spec.default-constructor-id)
		;False  or  the syntactic  identifier  bound  to the  default  record
		;constructor function.
   (immutable type-predicate-id r6rs-record-type-spec.type-predicate-id)
		;False  or  the syntactic  identifier  bound  to the  type  predicate
		;function.
   (immutable safe-accessors-table r6rs-record-type-spec.safe-accessors-table)
		;Alist mapping all  field names to the identifiers to  which the safe
		;accessors are bound.
   (immutable safe-mutators-table r6rs-record-type-spec.safe-mutators-table)
		;Alist mapping mutable  field names to the identifiers  to which safe
		;mutators are bound.
   (immutable unsafe-accessors-table r6rs-record-type-spec.unsafe-accessors-table)
		;False or alist  mapping all field names to the  identifiers to which
		;the unsafe accessors are bound.
   (immutable unsafe-mutators-table r6rs-record-type-spec.unsafe-mutators-table)
		;False or  alist mapping  mutable field names  to the  identifiers to
		;which unsafe mutators are bound.
   #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (case-lambda
       ((rtd-id rcd-id)
	(make-record rtd-id rcd-id
		     #f ;parent-id
		     #f ;default-constructor-id
		     #f ;type-predicate-id
		     '() ;safe-accessors-table
		     '() ;safe-mutators-table
		     '() ;unsafe-accessors-table
		     '() ;unsafe-mutators-table
		     ))
       ((rtd-id rcd-id parent-id default-constructor-id type-predicate-id
		safe-accessors-table safe-mutators-table unsafe-accessors-table unsafe-mutators-table)
	(make-record rtd-id rcd-id parent-id default-constructor-id type-predicate-id
		     safe-accessors-table safe-mutators-table
		     unsafe-accessors-table unsafe-mutators-table)))))
  #| end of DEFINE-RECORD-TYPE |# )


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
