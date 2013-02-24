;;;
;;;Part of: Vicare Scheme
;;;Contents: struct type definition for foreign pointer wrappers
;;;Date: Sun Feb 24, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare ffi foreign-pointer-wrapper)
  (export

    define-struct-wrapping-foreign-pointer

    ;; auxiliary syntaxes
    foreign-destructor
    collecting-type
    table-field)
  (import (vicare)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    $)
    (vicare arguments validation))

  (define-auxiliary-syntaxes
    foreign-destructor
    collecting-type
    table-field)


(define-syntax* (define-struct-wrapping-foreign-pointer stx)
  (define (main stx)
    (syntax-case stx (foreign-destructor
		      collecting-struct
		      table-field)
      ((_ ?type-id
	  (foreign-destructor ?foreign-destructor)
	  (collecting-type ?collecting-type)
	  (table-field ?table-field-id)
	  ...)
       (let ((type-id		#'?type-id)
	     ;;Normalisation:  either #f  or a  syntax object  holding a
	     ;;non-#f expression.
	     (collecting-type	(let ((ct #'?collecting-type))
				  (if (syntax->datum ct)
				      ct
				    #f)))
	     (table-field-ids	(syntax->list #'(?table-field-id ...))))
	 (%validate-input type-id collecting-type table-field-ids)
	 (with-syntax
	     ((STRUCT-DEFINITION
	       (%make-struct-definition type-id collecting-type table-field-ids))
	      (ARGUMENT-VALIDATIONS
	       (%make-argument-validations type-id))
	      (MAKER-DEFINITIONS
	       (%make-makers-definitions type-id table-field-ids
					 collecting-type))
	      (ALIVE-PRED-DEFINITIONS
	       (%make-alive-pred-definitions type-id))
	      (DESTRUCTOR-DEFINITION
	       (%make-struct-destructor type-id #'?foreign-destructor table-field-ids))
	      (PRINTER-DEFINITION
	       (%make-struct-printer type-id))
	      ((TABLE-FIELD-VECTOR-GETTER ...)
	       (map (lambda (table-field)
		      (%make-table-field-vector-getter type-id table-field))
		 table-field-ids))
	      ((REGISTRATION-DEFINITION ...)
	       (map (lambda (table-field)
		      (%make-table-field-registration-definitions type-id table-field))
		 table-field-ids)))
	   #'(begin
	       STRUCT-DEFINITION
	       ARGUMENT-VALIDATIONS
	       MAKER-DEFINITIONS
	       ALIVE-PRED-DEFINITIONS
	       DESTRUCTOR-DEFINITION
	       PRINTER-DEFINITION
	       TABLE-FIELD-VECTOR-GETTER ...
	       REGISTRATION-DEFINITION ...)
	   )))))

  (define (%make-struct-definition type-id collecting-type table-field-ids)
    ;;Return a syntax object representing the definition of the Scheme
    ;;data structure.
    ;;
    (with-syntax
	(((COLLECTOR ...)
	  (if collecting-type
	      #'(collector)
	    '()))
	 ((TABLE-FIELD-ID ...)
	  (map %make-table-field-id table-field-ids)))
      #`(define-struct #,type-id
	  (pointer
		;False or  a pointer  object referencing a  foreign data
		;structure.  When the foreign  data struct is finalised:
		;this field is set to false.
	   pointer-owner?
		;Boolean, true if this Scheme  structure is the owner of
		;the foreign data structure  referenced by the "pointer"
		;field.
	   custom-destructor
		;False or a user-supplied function to be called whenever
		;this instance  is closed.  The function  must accept at
		;least one argument being the data structure itself.
	   uid
		;A gensym to be used as hash table key.
	   COLLECTOR ...
		;False or a Scheme data struct that collects this one.
	   TABLE-FIELD-ID
		;Hashtable holding  Scheme data  structures of  the same
		;type.  Each  data structure hold wraps  pointer objects
		;to foreign data structures owned by this structure.
	   ...
	   ))))

  (define (%make-argument-validations type-id)
    ;;Return a  syntax object representing the  definition of argument
    ;;validators to be used with the facilities of the library (vicare
    ;;argument validation).
    ;;
    ;;The function call:
    ;;
    ;;   (%make-argument-validations #'gsasl #'gsasl? #'gsasl?/alive)
    ;;
    ;;returns a syntax object representing:
    ;;
    ;;   (begin
    ;;     (define-argument-validation (gsasl who obj)
    ;;       (gsasl? obj)
    ;;       (assertion-violation who
    ;;         "expected \"gsasl\" struct as argument" obj))
    ;;     (define-argument-validation (gsasl/alive who obj)
    ;;       (gsasl/alive? obj)
    ;;       (assertion-violation who
    ;;         "expected alive \"gsasl\" struct as argument" obj)))
    ;;
    (let ((type-string (symbol->string (syntax->datum type-id))))
      (with-syntax
	  ((PRED-VALIDATOR	type-id)
	   (ALIVE_VALIDATOR	(%id->id type-id (lambda (type-string)
						   (string-append type-string "/alive"))))
	   (PRED-MSG		(string-append "expected \""
					       type-string
					       "\" struct as argument"))
	   (ALIVE-MSG		(string-append "expected alive \""
					       type-string
					       "\" struct as argument")))
	(let-values
	    (((pred pred/alive unsafe-alive-pred)
	      (%make-pred-ids  type-id)))
	  #`(begin
	      (define-argument-validation (PRED-VALIDATOR who obj)
		(#,pred obj)
		(assertion-violation who PRED-MSG obj))
	      (define-argument-validation (ALIVE-VALIDATOR who obj)
		(#,pred/alive obj)
		(assertion-violation who ALIVE-MSG obj)))))))

  (define (%make-makers-definitions type-id table-field-ids collecting-type)
    ;;Return  a  syntax object  representing  the  definitions of  the
    ;;simplified data struct makers:
    ;;
    ;;1. The one that initialises the struct to own the pointer.
    ;;
    ;;2. The one that initialises the struct not to own the pointer.
    ;;
    (let-values
	(((maker maker/owner maker/not-owner)
	  (%make-maker-ids type-id)))
      (with-syntax
	  (((TABLE-MAKER ...)
	    (map (lambda (table-id)
		   #'(make-eq-hashtable))
	      table-field-ids))
	   ((ARG ...)
	    (if collecting-type
		#'(collector-struct)
	      '()))
	   ((REGISTER-LAMBDA ...)
	    (if collecting-type
		#`(lambda (struct collector-struct)
		    (#,(%make-register-sub-struct-id collecting-type type-id)
		     collector-struct struct))
	      '())))
	#`(begin
	    (define (#,maker/owner pointer ARG ...)
	      ;;Build and return  a new data struct  instance owning the
	      ;;POINTER.
	      ;;
	      (%simplified-maker pointer #t ARG ...))
	    (define (#,maker/not-owner pointer ARG ...)
	      ;;Build and return  a new data struct  instance not owning
	      ;;the POINTER.
	      ;;
	      (%simplified-maker pointer #f ARG ...))
	    (define (%simplified-maker pointer owner? ARG ...)
	      (let ((struct (#,maker pointer owner? #f (gensym) ARG ... TABLE-MAKER ...)))
		(REGISTER-LAMBDA struct collector-struct)
		...
		struct))))))

  (define (%make-alive-pred-definitions type-id)
    ;;Return  a  syntax  object  representing the  definition  of  the
    ;;predicates used to check if the data struct is still alive.
    ;;
    ;;The function call:
    ;;
    ;;   (%make-alive-pred-definitions #'gsasl
    ;;                                 #'gsasl? #'gsasl?/alive
    ;;                                 #'$live-gsasl?
    ;;                                 #'$gsasl-pointer)
    ;;
    ;;returns a syntax object representing:
    ;;
    ;;   (begin
    ;;     (define (gsasl?/alive obj)
    ;;       (and (gsasl? obj)
    ;;            ($live-gsasl? obj)))
    ;;     (define ($live-gsasl? struct)
    ;;       (not (pointer-null? ($gsasl-pointer struct)))))
    ;;
    (let-values
	(((pred pred/alive unsafe-alive-pred)
	  (%make-pred-ids  type-id))
	 ((unsafe-getter-pointer
	   unsafe-getter-pointer-owner?
	   unsafe-getter-custom-destructor
	   unsafe-getter-uid)
	  (%make-unsafe-Getter-ids type-id)))
      #`(begin
	  (define (#,pred/alive obj)
	    ;;Return true if OBJ is an instance of the data struct and
	    ;;its POINTER field contains a non-NULL pointer object.
	    ;;
	    (and (#,pred obj)
		 (#,unsafe-alive-pred obj)))
	  (define (#,unsafe-alive-pred struct)
	    ;;Evaluate to true if the data struct argument contains an
	    ;;object pointer to foreign struct not yet nullified.
	    ;;
	    (and (#,unsafe-getter-pointer struct)
		 #t)))))

  (module (%make-struct-destructor)

    (define (%make-struct-destructor type-id foreign-destructor table-field-ids)
      ;;Return  a  syntax  object   representing  the  definition  and
      ;;registration of the Scheme  destructor function for the Scheme
      ;;data struct.  The Scheme  destructor function exists even when
      ;;the referenced  foreign data  struct does  not have  a foreign
      ;;destructor function.
      ;;
      (let-values
	  (((unsafe-scheme-destructor)
	    (%make-unsafe-scheme-destructor-id type-id))
	   ((pred pred/alive unsafe-alive-pred)
	    (%make-pred-ids type-id))
	   ((unsafe-getter-pointer
	     unsafe-getter-pointer-owner?
	     unsafe-getter-custom-destructor
	     unsafe-getter-uid)
	    (%make-unsafe-Getter-ids type-id))
	   ((unsafe-pointer-setter
	     unsafe-owner-setter
	     unsafe-custom-destructor-setter)
	    (%make-unsafe-Setter-ids type-id)))
	(with-syntax
	    ((STRUCT #'struct))
	  (with-syntax
	      (((TABLE-DESTRUCTION-FORM ...)
		(map (lambda (table-field-id)
		       (%make-table-destruction #'STRUCT type-id table-field-id))
		  table-field-ids))
	       ((FOREIGN-DESTRUCTOR-CALL ...)
		(%make-foreign-destructor-call #'STRUCT foreign-destructor
					       unsafe-getter-pointer-owner?)))
	    #`(begin
		(define (#,unsafe-scheme-destructor STRUCT)
		  ;;This  function  is  called by  the  proper  struct
		  ;;destructor  function (if  any) or  by the  garbage
		  ;;collector to finalise a  data struct instance.  It
		  ;;is safe  to apply this function  multiple times to
		  ;;the same STRUCT argument.
		  ;;
		  ;;The   referenced   foreign   data   structure   is
		  ;;finalised, too, if STRUCT owns it.
		  ;;
		  ;;Notes:
		  ;;
		  ;;*  We  ignore  the  return value  of  the  foreign
		  ;;  destructor function.
		  ;;
		  (when (#,unsafe-alive-pred STRUCT)
		    ;;Apply the custom destructor to STRUCT.
		    (cond ((#,unsafe-getter-custom-destructor STRUCT)
			   => (lambda (custom-destructor)
				(guard (E (else (void)))
				  (custom-destructor STRUCT)))))
		    ;;Finalise the  tables of subordinate  structs, if
		    ;;any.
		    TABLE-DESTRUCTION-FORM ...
		    ;;Finalise the foreign data structure, if there is
		    ;;a foreign destructor.
		    FOREIGN-DESTRUCTOR-CALL ...
		    ;;Nullify the pointer.
		    (#,unsafe-pointer-setter STRUCT #f))
		  ;;Always return #<void>.
		  (void))
		(module ()
		  (set-rtd-destructor! (type-descriptor #,type-id)
				       #,unsafe-scheme-destructor)))))))

    (define (%make-table-destruction struct-id type-id table-field-id)
      (with-syntax
	  ((TABLE-GETTER
	    (%make-table-getter-id type-id table-field-id))
	   (TABLE-FIELD-VECTOR-GETTER
	    (%make-table-field-vector-getter-id type-id table-field-id))
	   (SUB-DESTRUCTOR
	    (%make-unsafe-scheme-destructor-id table-field-id)))
	#`(let* ((sub-structs	(TABLE-FIELD-VECTOR-GETTER #,struct-id))
		 (len		($vector-length sub-structs)))
	    (do ((i 0 (+ 1 i)))
		((= i len)
		 (hashtable-clear! (TABLE-GETTER #,struct-id)))
	      (SUB-DESTRUCTOR ($vector-ref sub-structs i))))))

    (define (%make-foreign-destructor-call struct-id foreign-destructor-id
					   unsafe-getter-pointer-owner?)
      (if (identifier? foreign-destructor-id)
	  #`((when (#,unsafe-getter-pointer-owner? #,struct-id)
	       (#,foreign-destructor-id #,struct-id)))
	'()))

    #| end of module: %make-struct-destructor |# )

  (define (%make-struct-printer type-id)
    ;;Return  a   syntax  object   representing  the   definition  and
    ;;registration of the printer function for the data struct type.
    ;;
    (let-values
	(((unsafe-getter-pointer
	   unsafe-getter-pointer-owner?
	   unsafe-getter-custom-destructor
	   unsafe-getter-uid)
	  (%make-unsafe-Getter-ids type-id)))
      (with-syntax
	  ((TYPE-STRING (symbol->string (syntax->datum type-id))))
	#`(begin
	    (define (the-printer S port sub-printer)
	      (define (%display thing)
		(display thing port))
	      (define (%write thing)
		(write thing port))
	      (%display "#[")
	      (%display TYPE-STRING)
	      (%display " pointer=")
	      (%display (#,unsafe-getter-pointer S))
	      (%display " pointer-owner?=")
	      (%display (#,unsafe-getter-pointer-owner? S))
	      (%display " uid=")
	      (%display (#,unsafe-getter-uid S))
	      (%display "]"))
	    (module ()
	      (set-rtd-printer! (type-descriptor #,type-id) the-printer))))))

  (define (%make-table-field-vector-getter type-id table-field-id)
    ;;Return  a  syntax  object representing  a  function  definition;
    ;;applying  the  function to  a  data  struct instance  returns  a
    ;;(possibly  empty)  vector of  structs  registered  in the  field
    ;;TABLE-FIELD-ID.
    ;;
    (with-syntax
	((TABLE-GETTER
	  (%make-table-getter-id type-id table-field-id))
	 (TABLE-FIELD-VECTOR-GETTER
	  (%make-table-field-vector-getter-id type-id table-field-id)))
      #'(define (TABLE-FIELD-VECTOR-GETTER struct)
	  (receive (keys vals)
	      (hashtable-entries (TABLE-GETTER struct))
	    vals))))

  (define (%make-table-field-registration-definitions type-id table-field-id)
    ;;Return  a  syntax  object   representing  the  definition  of  2
    ;;functions:
    ;;
    ;;1. A function that registers  a sub-struct of type TYPE-FIELD-ID
    ;;   in the appropriate table of structs of type TYPE-ID.
    ;;
    ;;2. A  function that removes  a sub-struct of  type TYPE-FIELD-ID
    ;;   from the appropriate table of structs of type TYPE-ID.
    ;;
    (with-syntax
	((TABLE-GETTER
	  (%make-table-getter-id type-id table-field-id))
	 (REGISTER-SUB-STRUCT
	  (%make-register-sub-struct-id type-id table-field-id))
	 (FORGET-SUB-STRUCT
	  (%make-forget-sub-struct-id type-id table-field-id)))
      (let-values
	  (((unsafe-getter-pointer
	     unsafe-getter-pointer-owner?
	     unsafe-getter-custom-destructor
	     unsafe-getter-uid)
	    (%make-unsafe-Getter-ids table-field-id)))
	#`(begin
	    (define (REGISTER-SUB-STRUCT struct sub-struct)
	      (hashtable-set! (TABLE-GETTER struct)
			      (#,unsafe-getter-uid sub-struct)
			      sub-struct))
	    (define (FORGET-SUB-STRUCT struct sub-struct)
	      (hashtable-delete! (TABLE-GETTER struct)
				 (#,unsafe-getter-uid sub-struct)))))))

  ;; ------------------------------------------------------------

  (define (%make-maker-ids type-id)
    ;;Given an identifier  representing the struct type  name return 3
    ;;identifiers in the same context of TYPE-ID:
    ;;
    ;;1. The name of the default struct maker.
    ;;
    ;;2. The name of the struct maker for structs owning the pointer.
    ;;
    ;;3.  The name  of the  struct maker  for structs  not owning  the
    ;;pointer.
    ;;
    (values (%id->id type-id (lambda (type-string)
			       (string-append "make-" type-string)))
	    (%id->id type-id (lambda (type-string)
			       (string-append "make-" type-string "/owner")))
	    (%id->id type-id (lambda (type-string)
			       (string-append "make-" type-string "/not-owner")))))

  (define (%make-pred-ids type-id)
    ;;Given an identifier  representing the struct type  name return 3
    ;;identifiers in the same context of TYPE-ID:
    ;;
    ;;1. The name of the default struct predicate.
    ;;
    ;;2. The name  of the struct predicate cheking that  the struct is
    ;;   also alive.
    ;;
    ;;3. The name of the  unsafe predicate expecting a struct instance
    ;;   and checking if the pointer field contains a NULL pointer.
    ;;
    (values (%id->id type-id (lambda (type-string)
			       (string-append type-string "?")))
	    (%id->id type-id (lambda (type-string)
			       (string-append type-string "?/alive")))
	    (%id->id type-id (lambda (type-string)
			       (string-append "$live-" type-string "?")))))

  (define (%make-unsafe-scheme-destructor-id type-id)
    ;;Given an identifier representing the struct type name return 1
    ;;identifier in the same context of TYPE-ID:
    ;;
    ;;1. The name of the unsafe destructor function.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$destroy-" type-string))))

  (define (%make-unsafe-Getter-ids type-id)
    ;;Given an identifier  representing the struct type  name return 3
    ;;identifiers in the same context of TYPE-ID:
    ;;
    ;;1. The name of the unsafe getter for the field POINTER.
    ;;
    ;;2. The name of the unsafe getter for the field POINTER-OWNER?.
    ;;
    ;;3. The name of the unsafe getter for the field DESTRUCTOR.
    ;;
    (values (%id->id type-id (lambda (type-string)
			       (string-append "$" type-string "-pointer")))
	    (%id->id type-id (lambda (type-string)
			       (string-append "$" type-string "-pointer-owner?")))
	    (%id->id type-id (lambda (type-string)
			       (string-append "$" type-string "-custom-destructor")))
	    (%id->id type-id (lambda (type-string)
			       (string-append "$" type-string "-uid")))
	    ))

  (define (%make-unsafe-Setter-ids type-id)
    ;;Given an identifier representing the struct type name return 3
    ;;identifiers in the same context of TYPE-ID:
    ;;
    ;;1. The name of the unsafe setter for the field POINTER.
    ;;
    ;;2. The name of the unsafe setter for the field POINTER-OWNER?.
    ;;
    ;;3. The name of the unsafe setter for the field DESTRUCTOR.
    ;;
    (values (%id->id type-id (lambda (type-string)
			       (string-append "$set-" type-string "-pointer!")))
	    (%id->id type-id (lambda (type-string)
			       (string-append "$set-" type-string "-pointer-owner?!")))
	    (%id->id type-id (lambda (type-string)
			       (string-append "$set-" type-string "-custom-destructor!")))
	    ))

  (define (%make-table-getter-id type-id table-field-id)
    ;;Given an identifier representing the  struct type name return an
    ;;identifier,  in the  same context  of TYPE-ID,  representing the
    ;;name of the table getter for the field TABLE-FIELD-ID.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-table-of-"
				      (symbol->string (syntax->datum table-field-id))))))

  (define (%make-table-field-id table-field)
    ;;Given  an identifier  representing  the name  of  a struct  type
    ;;collected  by this  struct type:  return the  identifier of  the
    ;;field referencing the collecting hash table.
    ;;
    (%id->id table-field (lambda (field-string)
			   (string-append "table-of-" field-string))))

  (define (%make-table-field-vector-getter-id type-id table-field-id)
    ;;Given an  identifier representing  the struct  type name  and an
    ;;identifier  representing a  sub-structs table  field: return  an
    ;;identifier,  in the  same context  of TYPE-ID,  representing the
    ;;name  of   a  function  returning  the   vector  of  sub-structs
    ;;registered in TABLE-FIELD-ID.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-vector-of-"
				      (symbol->string (syntax->datum table-field-id))))))

  (define (%make-register-sub-struct-id type-id table-field-id)
    ;;Given an  identifier representing  the struct  type name  and an
    ;;identifier  representing a  sub-structs table  field: return  an
    ;;identifier,  in the  same context  of TYPE-ID,  representing the
    ;;name of a function that registers a sub-struct in the table of a
    ;;struct.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-register-"
				      (symbol->string (syntax->datum table-field-id))
				      "!"))))

  (define (%make-forget-sub-struct-id type-id table-field-id)
    ;;Given an  identifier representing  the struct  type name  and an
    ;;identifier  representing a  sub-structs table  field: return  an
    ;;identifier,  in the  same context  of TYPE-ID,  representing the
    ;;name of a function that removes a sub-struct from the table of a
    ;;struct.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-forget-"
				      (symbol->string (syntax->datum table-field-id))
				      "!"))))

  ;; ------------------------------------------------------------

  (define (%validate-input type-id collecting-type table-field-ids)
    (unless (identifier? type-id)
      (%synner "expected identifier as struct type name" type-id))
    (unless (or (not collecting-type)
		(identifier? collecting-type))
      (%synner "expected #f or identifier as collecting struct type"
	       collecting-type))
    (unless (for-all identifier? table-field-ids)
      (%synner "expected identifiers as table field names" table-field-ids)))

  (define (%id->id src-id string-maker)
    (datum->syntax src-id (string->symbol
			   (string-maker
			    (symbol->string (syntax->datum src-id))))))

  (define (syntax->list stx)
    ;;Given a syntax  object STX holding a list, return  a proper list
    ;;holding the component syntax objects.
    ;;
    (syntax-case stx ()
      ((?car . ?cdr)
       ;;Yes, it  is not  tail recursive; "they"  say it  is efficient
       ;;this way.
       (cons #'?car (syntax->list #'?cdr)))
      (() '())))

  (define (%synner message subform)
    (syntax-violation who message stx subform))

  (define who 'define-struct-wrapping-foreign-pointer)

  (main stx))


;;;; done

)

;;; end of file
