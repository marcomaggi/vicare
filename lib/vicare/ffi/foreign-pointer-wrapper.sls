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

    define-foreign-pointer-wrapper

    ;; auxiliary syntaxes
    foreign-destructor
    collecting-struct-type
    collected-struct-type)
  (import (vicare)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    $)
    (vicare arguments validation))

  (define-auxiliary-syntaxes
    foreign-destructor
    collecting-struct-type
    collected-struct-type)


(define-syntax* (define-foreign-pointer-wrapper stx)
  (define (main stx)
    (syntax-case stx (foreign-destructor
		      collecting-struct-type
		      collected-struct-type)
      ((_ ?type-id
	  (foreign-destructor ?foreign-destructor)
	  (collecting-type ?collecting-type)
	  (table-field ?collected-field-id)
	  ...)
       (let ((type-id #'?type-id))
	 ;;Normalisation: either #f or a  syntax object holding a non-#f
	 ;;expression.
	 (define foreign-destructor
	   (let ((ct #'?foreign-destructor))
	     (if (syntax->datum ct)
		 ct
	       #f)))
	 ;;Normalisation: either #f or a  syntax object holding a non-#f
	 ;;expression.
	 (define collecting-type
	   (let ((ct #'?collecting-type))
	     (if (syntax->datum ct)
		 ct
	       #f)))
	 (define collected-field-ids
	   (syntax->list #'(?collected-field-id ...)))
	 (%validate-input type-id foreign-destructor collecting-type collected-field-ids)
	 (with-syntax
	     ((STRUCT-DEFINITION
	       (%make-struct-definition type-id collecting-type collected-field-ids))
	      (ARGUMENT-VALIDATIONS
	       (%make-argument-validations type-id))
	      (MAKER-DEFINITIONS
	       (%make-makers-definitions type-id collected-field-ids
					 collecting-type))
	      (ALIVE-PRED-DEFINITIONS
	       (%make-alive-pred-definitions type-id))
	      (DESTRUCTOR-DEFINITION
	       (%make-struct-finaliser type-id foreign-destructor
				       collecting-type collected-field-ids))
	      (PRINTER-DEFINITION
	       (%make-struct-printer type-id))
	      ((TABLE-FIELD-VECTOR-GETTER ...)
	       (map (lambda (table-field)
		      (%make-table-field-vector-getter type-id table-field))
		 collected-field-ids))
	      ((REGISTRATION-DEFINITION ...)
	       (map (lambda (table-field)
		      (%make-table-field-registration-definitions type-id table-field))
		 collected-field-ids)))
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

  (define (%make-struct-definition type-id collecting-type collected-field-ids)
    ;;Return a syntax object representing the definition of the Scheme
    ;;data structure.
    ;;
    (with-syntax
	(((COLLECTOR ...)
	  (if collecting-type
	      #'(collector)
	    '()))
	 ((COLLECTED-FIELD-ID ...)
	  (map %make-collected-field-id collected-field-ids)))
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
	   COLLECTED-FIELD-ID
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
	  ((PRED-VALIDATOR	(%make-argument-validator-pred-id type-id))
	   (ALIVE-VALIDATOR	(%make-argument-validator-alive-id type-id))
	   (PRED-MSG		(string-append "expected \""
					       type-string
					       "\" struct as argument"))
	   (ALIVE-MSG		(string-append "expected alive \""
					       type-string
					       "\" struct as argument")))
	(let ((pred		(%make-pred-id/type? type-id))
	      (pred/alive	(%make-pred-id/alive-type? type-id)))
	  #`(begin
	      (define-argument-validation (PRED-VALIDATOR who obj)
		(#,pred obj)
		(assertion-violation who PRED-MSG obj))
	      (define-argument-validation (ALIVE-VALIDATOR who obj)
		(#,pred/alive obj)
		(assertion-violation who ALIVE-MSG obj)))))))

  (define (%make-makers-definitions type-id collected-field-ids collecting-type)
    ;;Return  a  syntax object  representing  the  definitions of  the
    ;;simplified data struct makers:
    ;;
    ;;1. The one that initialises the struct to own the pointer.
    ;;
    ;;2. The one that initialises the struct not to own the pointer.
    ;;
    (define maker
      (%make-maker-id/default type-id))
    (define maker/owner
      (%make-maker-id/owner type-id))
    (define maker/not-owner
      (%make-maker-id/not-owner type-id))
    (define register-sub-struct
      (if collecting-type
	  (%make-register-sub-struct-id collecting-type type-id)
	#f))
    (with-syntax
	(((TABLE-MAKER ...)	(map (lambda (table-id)
				       #'(make-eq-hashtable))
				  collected-field-ids))
	 ((ARG ...)		(if collecting-type
				    #'(collector-struct)
				  '()))
	 ((REGISTER-LAMBDA ...)	(if collecting-type
				    #`(lambda (struct collector-struct)
					(#,register-sub-struct collector-struct struct))
				  '())))
      #`(begin
	  (define (#,maker/owner pointer ARG ...)
	    ;;Build and return  a new data struct  instance owning the
	    ;;POINTER.
	    ;;
	    (%simplified-maker (quote #,maker/owner) pointer #t ARG ...))
	  (define (#,maker/not-owner pointer ARG ...)
	    ;;Build and return  a new data struct  instance not owning
	    ;;the POINTER.
	    ;;
	    (%simplified-maker (quote #,maker/not-owner) pointer #f ARG ...))
	  (define (%simplified-maker who pointer owner? ARG ...)
	    (with-arguments-validation (who)
		((pointer	pointer))
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
    (let ((pred				(%make-pred-id/type? type-id))
	  (pred/alive			(%make-pred-id/alive-type? type-id))
	  (unsafe-alive-pred		(%make-pred-id/unsafe-alive? type-id))
	  (unsafe-getter-pointer	(%make-unsafe-Getter-id/pointer type-id)))
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

  (module (%make-struct-finaliser)

    (define (%make-struct-finaliser type-id foreign-destructor
				    collecting-type collected-field-ids)
      ;;Return  a  syntax  object   representing  the  definition  and
      ;;registration of the Scheme  destructor function for the Scheme
      ;;data struct.  The Scheme  destructor function exists even when
      ;;the referenced  foreign data  struct does  not have  a foreign
      ;;destructor function.
      ;;
      (define unsafe-scheme-destructor
	(%make-unsafe-finaliser-id type-id))
      (define pred
	(%make-pred-id/type? type-id))
      (define pred/alive
	(%make-pred-id/alive-type? type-id))
      (define unsafe-alive-pred
	(%make-pred-id/unsafe-alive? type-id))
      (define unsafe-getter-pointer
	(%make-unsafe-Getter-id/pointer type-id))
      (define unsafe-getter-pointer-owner?
	(%make-unsafe-Getter-id/pointer-owner? type-id))
      (define unsafe-getter-custom-destructor
	(%make-unsafe-Getter-id/custom-destructor type-id))
      (define unsafe-pointer-setter
	(%make-unsafe-Setter-id/pointer type-id))
      (with-syntax
	  ((STRUCT #'struct))
	(with-syntax
	    (((TABLE-DESTRUCTION-FORM ...)
	      (map (lambda (collected-field-id)
		     (%make-table-destruction #'STRUCT type-id collected-field-id))
		collected-field-ids))
	     ((UNREGISTER-FROM-COLLECTOR ...)
	      (%make-unregistration-from-collector #'STRUCT type-id collecting-type))
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
		  ;;Unregister this struct  instance from its collector,
		  ;;if any.
		  UNREGISTER-FROM-COLLECTOR ...
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
				     #,unsafe-scheme-destructor))))))

    (define (%make-table-destruction struct-id type-id collected-field-id)
      (with-syntax
	  ((TABLE-GETTER
	    (%make-unsafe-Getter-id/collected-thing type-id collected-field-id))
	   (TABLE-FIELD-VECTOR-GETTER
	    (%make-table-field-vector-getter-id type-id collected-field-id))
	   (SUB-DESTRUCTOR
	    (%make-unsafe-finaliser-id collected-field-id)))
	#`(let* ((sub-structs	(TABLE-FIELD-VECTOR-GETTER #,struct-id))
		 (len		($vector-length sub-structs)))
	    (do ((i 0 (+ 1 i)))
		((= i len)
		 (hashtable-clear! (TABLE-GETTER #,struct-id)))
	      (guard (E (else (void)))
		(SUB-DESTRUCTOR ($vector-ref sub-structs i)))))))

    (define (%make-unregistration-from-collector struct-id type-id collecting-type)
      (if (identifier? collecting-type)
	  (with-syntax
	      ((COLLECTOR-GETTER
		(%make-unsafe-Getter-id/collector type-id))
	       (FORGET-SUB-STRUCT
		(%make-forget-sub-struct-id collecting-type type-id)))
	    #`(cond ((COLLECTOR-GETTER #,struct-id)
		     => (lambda (collector)
			  (FORGET-SUB-STRUCT collector #,struct-id)))))
	'()))

    (define (%make-foreign-destructor-call struct-id foreign-destructor-id
					   unsafe-getter-pointer-owner?)
      ;;Return  a  syntax object  representing  the  application of  the
      ;;foreign   data  struct   destructor  to   the  structure;   such
      ;;application  is  performed only  if  the  data struct  owns  the
      ;;pointer object.
      ;;
      (if (identifier? foreign-destructor-id)
	  #`((when (#,unsafe-getter-pointer-owner? #,struct-id)
	       (guard (E (else (void)))
		 (#,foreign-destructor-id #,struct-id))))
	'()))

    #| end of module: %make-struct-finaliser |# )

  (define (%make-struct-printer type-id)
    ;;Return  a   syntax  object   representing  the   definition  and
    ;;registration of the printer function for the data struct type.
    ;;
    (let ((unsafe-getter-pointer	(%make-unsafe-Getter-id/pointer type-id))
	  (unsafe-getter-pointer-owner?	(%make-unsafe-Getter-id/pointer-owner? type-id))
	  (unsafe-getter-uid		(%make-unsafe-Getter-id/uid type-id)))
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

  (define (%make-table-field-vector-getter type-id collected-field-id)
    ;;Return  a  syntax  object representing  a  function  definition;
    ;;applying  the  function to  a  data  struct instance  returns  a
    ;;(possibly  empty)  vector of  structs  registered  in the  field
    ;;COLLECTED-FIELD-ID.
    ;;
    (with-syntax
	((TABLE-GETTER
	  (%make-unsafe-Getter-id/collected-thing type-id collected-field-id))
	 (TABLE-FIELD-VECTOR-GETTER
	  (%make-table-field-vector-getter-id type-id collected-field-id)))
      #'(define (TABLE-FIELD-VECTOR-GETTER struct)
	  (receive (keys vals)
	      (hashtable-entries (TABLE-GETTER struct))
	    vals))))

  (define (%make-table-field-registration-definitions type-id collected-field-id)
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
	  (%make-unsafe-Getter-id/collected-thing type-id collected-field-id))
	 (REGISTER-SUB-STRUCT
	  (%make-register-sub-struct-id type-id collected-field-id))
	 (FORGET-SUB-STRUCT
	  (%make-forget-sub-struct-id type-id collected-field-id)))
      (let ((unsafe-getter-uid (%make-unsafe-Getter-id/uid collected-field-id)))
	#`(begin
	    (define (REGISTER-SUB-STRUCT struct sub-struct)
	      (hashtable-set! (TABLE-GETTER struct)
			      (#,unsafe-getter-uid sub-struct)
			      sub-struct))
	    (define (FORGET-SUB-STRUCT struct sub-struct)
	      (hashtable-delete! (TABLE-GETTER struct)
				 (#,unsafe-getter-uid sub-struct)))))))

;;; --------------------------------------------------------------------
;;; maker and finaliser ids

  (define (%make-maker-id/default type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the default maker function.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "make-" type-string))))

  (define (%make-maker-id/owner type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the maker function specialised to build structures "owning" the
    ;;referenced foreign structure.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "make-" type-string "/owner"))))

  (define (%make-maker-id/not-owner type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the maker function specialised to build structures "not owning"
    ;;the referenced foreign structure.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "make-" type-string "/not-owner"))))

  (define (%make-unsafe-finaliser-id type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe Scheme finaliser function.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-finalise"))))

;;; --------------------------------------------------------------------
;;; type predicate ids

  (define (%make-pred-id/type? type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the default type predicate.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append type-string "?"))))

  (define (%make-pred-id/alive-type? type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the type and alive predicate.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append type-string "?/alive"))))

  (define (%make-pred-id/unsafe-alive? type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe alive predicate.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-alive?"))))

;;; --------------------------------------------------------------------
;;; argument validation ids

  (define (%make-argument-validator-pred-id type-id)
    ;;Given an identifier  representing the struct type  name: return an
    ;;identifier representing  the argument validation clause  using the
    ;;type predicate.
    ;;
    (%id->id type-id (lambda (type-string)
		       type-string)))

  (define (%make-argument-validator-alive-id type-id)
    ;;Given an identifier  representing the struct type  name: return an
    ;;identifier representing  the argument validation clause  using the
    ;;alive type predicate.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append type-string "/alive"))))

;;; --------------------------------------------------------------------
;;; getter ids

  (define (%make-unsafe-Getter-id/pointer type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe getter for the field POINTER.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-pointer"))))

  (define (%make-unsafe-Getter-id/pointer-owner? type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe getter for the field POINTER-OWNER?.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-pointer-owner?"))))

  (define (%make-unsafe-Getter-id/custom-destructor type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe getter for the field CUSTOM-DESTRUCTOR.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-custom-destructor"))))

  (define (%make-unsafe-Getter-id/uid type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe getter for the field UID.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-uid"))))

  (define (%make-unsafe-Getter-id/collected-thing type-id collected-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the collection getter for the field COLLECTED-ID.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-collected-"
				      (%id->string collected-id)))))

  (define (%make-unsafe-Getter-id/collector type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe getter for the field COLLECTOR.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-collector"))))


;;; --------------------------------------------------------------------
;;; setter ids

  (define (%make-unsafe-Setter-id/pointer type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe setter for the field POINTER.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$set-" type-string "-pointer!"))))

  (define (%make-unsafe-Setter-id/pointer-owner? type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe setter for the field POINTER-OWNER?.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$set-" type-string "-pointer-owner?!"))))

  (define (%make-unsafe-Setter-id/custom-destructor type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe setter for the field CUSTOM-DESTRUCTOR.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$set-" type-string "-custom-destructor!"))))

;;; --------------------------------------------------------------------
;;; collector struct ids


;;; --------------------------------------------------------------------
;;; collected struct ids

  (define (%make-collected-field-id table-field)
    ;;Given  an identifier  representing  the name  of  a struct  type
    ;;collected  by this  struct type:  return the  identifier of  the
    ;;field referencing the collecting hash table.
    ;;
    (%id->id table-field (lambda (field-string)
			   (string-append "collected-" field-string))))

  (define (%make-table-field-vector-getter-id type-id collected-field-id)
    ;;Given an  identifier representing  the struct  type name  and an
    ;;identifier  representing a  sub-structs table  field: return  an
    ;;identifier,  in the  same context  of TYPE-ID,  representing the
    ;;name  of   a  function  returning  the   vector  of  sub-structs
    ;;registered in COLLECTED-FIELD-ID.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-vector-of-"
				      (symbol->string (syntax->datum collected-field-id))))))

  (define (%make-register-sub-struct-id type-id collected-field-id)
    ;;Given an  identifier representing  the struct  type name  and an
    ;;identifier  representing a  sub-structs table  field: return  an
    ;;identifier,  in the  same context  of TYPE-ID,  representing the
    ;;name of a function that registers a sub-struct in the table of a
    ;;struct.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-register-"
				      (symbol->string (syntax->datum collected-field-id))
				      "!"))))

  (define (%make-forget-sub-struct-id type-id collected-field-id)
    ;;Given an  identifier representing  the struct  type name  and an
    ;;identifier  representing a  sub-structs table  field: return  an
    ;;identifier,  in the  same context  of TYPE-ID,  representing the
    ;;name of a function that removes a sub-struct from the table of a
    ;;struct.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-forget-"
				      (symbol->string (syntax->datum collected-field-id))
				      "!"))))

  ;; ------------------------------------------------------------

  (define (%validate-input type-id foreign-destructor collecting-type collected-field-ids)
    (unless (identifier? type-id)
      (%synner "expected identifier as struct type name" type-id))
    (unless (or (not collecting-type)
		(identifier? collecting-type))
      (%synner "expected #f or identifier as collecting struct type"
	       collecting-type))
    (unless (for-all identifier? collected-field-ids)
      (%synner "expected identifiers as table field names" collected-field-ids)))

  (define (%id->id src-id string-maker)
    (datum->syntax src-id (string->symbol
			   (string-maker
			    (symbol->string (syntax->datum src-id))))))

  (define (%id->string id)
    (symbol->string (syntax->datum id)))

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

  (define who 'define-foreign-pointer-wrapper)

  (main stx))


;;;; done

)

;;; end of file
