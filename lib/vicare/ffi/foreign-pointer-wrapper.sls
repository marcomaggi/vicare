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
    collector-struct-type
    collected-struct-type
    fields)
  (import (vicare)
    (vicare language-extensions syntaxes)
    (vicare unsafe operations)
    (vicare arguments validation))

  (define-auxiliary-syntaxes
    foreign-destructor
    collector-struct-type
    collected-struct-type)


(define-syntax (define-foreign-pointer-wrapper stx)
  (define (main stx)
    (syntax-case stx (foreign-destructor
		      collector-struct-type
		      collected-struct-type
		      fields)
      ((?kwd ?type-id
	     (foreign-destructor ?foreign-destructor)
	     (collector-struct-type ?collector-type-id)
	     (collected-struct-type ?collected-type-id)
	     ...)
       #'(?kwd ?type-id
	       (fields)
	       (foreign-destructor ?foreign-destructor)
	       (collector-struct-type ?collector-type-id)
	       (collected-struct-type ?collected-type-id)
	       ...))
      ((_ ?type-id
	  (fields ?field-id ...)
	  (foreign-destructor ?foreign-destructor)
	  (collector-struct-type ?collector-type-id)
	  (collected-struct-type ?collected-type-id)
	  ...)
       (let ((type-id #'?type-id))
	 (define field-ids
	   (syntax->list #'(?field-id ...)))
	 ;;Normalisation: either #f or a  syntax object holding a non-#f
	 ;;expression.
	 (define foreign-destructor
	   (let ((ct #'?foreign-destructor))
	     (if (syntax->datum ct)
		 ct
	       #f)))
	 ;;Normalisation: either #f or a  syntax object holding a non-#f
	 ;;expression.
	 (define collector-type-id
	   (let ((ct #'?collector-type-id))
	     (if (syntax->datum ct)
		 ct
	       #f)))
	 (define collected-type-ids
	   (syntax->list #'(?collected-type-id ...)))
	 (%validate-input type-id field-ids
			  foreign-destructor collector-type-id collected-type-ids)
	 (with-syntax
	     ((STRUCT-DEFINITION
	       (%make-struct-definition type-id field-ids
					collector-type-id collected-type-ids))
	      (ARGUMENT-VALIDATIONS
	       (%make-argument-validation-definitions type-id))
	      (MAKER-DEFINITIONS
	       (%make-makers-definitions type-id field-ids
					 collector-type-id collected-type-ids))
	      (PREDICATE-DEFINITIONS
	       (%make-predicate-definitions type-id))
	      (DESTRUCTOR-DEFINITION
	       (%make-finaliser-definitions type-id foreign-destructor
						   collector-type-id collected-type-ids))
	      (PRINTER-DEFINITION
	       (%make-struct-printer-definition type-id))
	      ((COLLECTED-STRUCTS-DEFINITIONS ...)
	       (map (lambda (collected-type-id)
		      (%make-collected-structs-definitions type-id collected-type-id))
		 collected-type-ids)))
	   (let ((output-form #'(begin
				  STRUCT-DEFINITION
				  ARGUMENT-VALIDATIONS
				  MAKER-DEFINITIONS
				  PREDICATE-DEFINITIONS
				  DESTRUCTOR-DEFINITION
				  PRINTER-DEFINITION
				  COLLECTED-STRUCTS-DEFINITIONS ...)))
	     #;(pretty-print (syntax->datum output-form) (current-error-port))
	     output-form))))))

  (define (%make-struct-definition type-id field-ids collector-type-id collected-type-ids)
    ;;Return a syntax object representing the definition of the Scheme
    ;;data structure.
    ;;
    (with-syntax
	(((COLLECTOR-FIELD ...)
	  (if collector-type-id
	      (list (%make-collector-field-id collector-type-id))
	    '()))
	 ((TABLE-OF-COLLECTED-STRUCTS-FIELD ...)
	  (map %make-table-of-collected-structs-field-id collected-type-ids))
	 ((FIELD ...)
	  field-ids))
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
	   COLLECTOR-FIELD ...
		;False or a Scheme data struct that collects this one.
	   TABLE-OF-COLLECTED-STRUCTS-FIELD
		;Hashtable holding  Scheme data  structures of  the same
		;type.  Each  data structure hold wraps  pointer objects
		;to foreign data structures owned by this structure.
	   ...
	   FIELD
		;Custom field.
	   ...))))

  (define (%make-argument-validation-definitions type-id)
    ;;Return a  syntax object representing the  definition of argument
    ;;validators to be used with the facilities of the library (vicare
    ;;argument validation).
    ;;
    ;;The function call:
    ;;
    ;;   (%make-argument-validation-definitions
    ;;       #'gsasl #'gsasl? #'gsasl?/alive)
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
    (let ((type-string (%id->string type-id)))
      (with-syntax
	  ((PRED-VALIDATOR
	    (%make-argument-validator-pred-id type-id))
	   (ALIVE-VALIDATOR
	    (%make-argument-validator-alive-id type-id))
	   (FALSE-OR-PRED-VALIDATOR
	    (%make-argument-validator-false-or-pred-id type-id))
	   (FALSE-OR-ALIVE-VALIDATOR
	    (%make-argument-validator-false-or-alive-id type-id))
	   (PRED-MSG
	    (string-append "expected \"" type-string "\" struct as argument"))
	   (ALIVE-MSG
	    (string-append "expected alive \"" type-string "\" struct as argument"))
	   (FALSE-OR-PRED-MSG
	    (string-append "expected #f or \"" type-string "\" struct as argument"))
	   (FALSE-OR-ALIVE-MSG
	    (string-append "expected #f or alive \"" type-string "\" struct as argument")))
	(let ((pred		(%make-pred-id/type? type-id))
	      (pred/alive	(%make-pred-id/alive-type? type-id)))
	  #`(begin
	      (define-argument-validation (PRED-VALIDATOR who obj)
		(#,pred obj)
		(assertion-violation who PRED-MSG obj))
	      (define-argument-validation (ALIVE-VALIDATOR who obj)
		(#,pred/alive obj)
		(assertion-violation who ALIVE-MSG obj))
	      (define-argument-validation (FALSE-OR-PRED-VALIDATOR who obj)
		(or (not obj) (#,pred obj))
		(assertion-violation who FALSE-OR-PRED-MSG obj))
	      (define-argument-validation (FALSE-OR-ALIVE-VALIDATOR who obj)
		(or (not obj) (#,pred/alive obj))
		(assertion-violation who FALSE-OR-ALIVE-MSG obj)))))))

  (define (%make-makers-definitions type-id field-ids
				    collector-type-id collected-type-ids)
    ;;Return  a  syntax  object  representing  the  definitions  of  the
    ;;simplified data struct makers:
    ;;
    ;;1. The one that initialises the struct to own the pointer.
    ;;
    ;;2. The one that initialises the struct not to own the pointer.
    ;;
    (define has-collector?
      (identifier? collector-type-id))
    (define collector-struct
      (and has-collector?
	   (%id->id collector-type-id
		    (lambda (collector-type-string)
		      (string-append "collector-" collector-type-string "-instance")))))
    (with-syntax
	((MAKER
	  (%make-maker-id/default type-id))
	 (MAKER/OWNER
	  (%make-maker-id/owner type-id))
	 (MAKER/NOT-OWNER
	  (%make-maker-id/not-owner type-id))
	 (INSTANCE
	  (%id->id type-id (lambda (type-string)
			     (string-append type-string "-instance"))))
	 ((TABLE-MAKER ...)
	  (map (lambda (table-id)
		 #'(make-eq-hashtable))
	    collected-type-ids))
	 ((COLLECTOR-STRUCT ...)
	  (if has-collector?
	      (list collector-struct)
	    '()))
	 ((VALIDATOR-CLAUSE ...)
	  (if has-collector?
	      `((,(%make-argument-validator-false-or-alive-id collector-type-id)
		 ,collector-struct))
	    '()))
	 ((REGISTER-COLLECTED-IN-COLLECTOR ...)
	  (if has-collector?
	      (list (%make-register-collected-in-collector!-id collector-type-id type-id))
	    '()))
	 ((FIELD ...)
	  field-ids))
      #`(begin
	  (define (MAKER/OWNER pointer COLLECTOR-STRUCT ... FIELD ...)
	    ;;Build and return  a new data struct  instance owning the
	    ;;POINTER.
	    ;;
	    (%simplified-maker 'MAKER/OWNER pointer #t COLLECTOR-STRUCT ... FIELD ...))
	  (define (MAKER/NOT-OWNER pointer COLLECTOR-STRUCT ... FIELD ...)
	    ;;Build and return  a new data struct  instance not owning
	    ;;the POINTER.
	    ;;
	    (%simplified-maker 'MAKER/NOT-OWNER pointer #f COLLECTOR-STRUCT ... FIELD ...))
	  (define (%simplified-maker who pointer owner? COLLECTOR-STRUCT ... FIELD ...)
	    (with-arguments-validation (who)
		((pointer	pointer)
		 VALIDATOR-CLAUSE
		 ...)
	      (let ((INSTANCE (MAKER pointer owner? #f (gensym)
				     COLLECTOR-STRUCT ...
				     TABLE-MAKER ...
				     FIELD ...)))
		(when COLLECTOR-STRUCT
		  (REGISTER-COLLECTED-IN-COLLECTOR COLLECTOR-STRUCT INSTANCE))
		...
		INSTANCE))))))

  (define (%make-predicate-definitions type-id)
    ;;Return  a  syntax  object  representing the  definition  of  the
    ;;predicates used to check if the data struct is still alive.
    ;;
    ;;The function call:
    ;;
    ;;   (%make-predicate-definitions #'gsasl
    ;;                                #'gsasl? #'gsasl?/alive
    ;;                                #'$live-gsasl?
    ;;                                #'$gsasl-pointer)
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

  (module (%make-finaliser-definitions)

    (define (%make-finaliser-definitions type-id foreign-destructor
					 collector-type collected-type-ids)
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
	  ((STRUCT (%id->id type-id (lambda (type-string)
				      (string-append type-string "-instance")))))
	(with-syntax
	    (((TABLE-DESTRUCTION-FORM ...)
	      (map (lambda (collected-type-id)
		     (%make-table-destruction #'STRUCT type-id collected-type-id))
		collected-type-ids))
	     ((UNREGISTER-FROM-COLLECTOR ...)
	      (%make-unregistration-from-collector #'STRUCT type-id collector-type))
	     (FOREIGN-DESTRUCTOR-CALL
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
		(if (#,unsafe-alive-pred STRUCT)
		    (begin
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
		      (let ((rv FOREIGN-DESTRUCTOR-CALL))
			;;Nullify the pointer.
			(#,unsafe-pointer-setter STRUCT #f)
			rv))
		  #f))
	      (module ()
		(set-rtd-destructor! (type-descriptor #,type-id)
				     #,unsafe-scheme-destructor))))))

    (define (%make-table-destruction struct-id type-id collected-type-id)
      (with-syntax
	  ((TABLE-GETTER
	    (%make-unsafe-Getter-id/table-of-collected type-id collected-type-id))
	   (TABLE-FIELD-VECTOR-GETTER
	    (%make-vector-of-collected-structs-getter-id type-id collected-type-id))
	   (COLLECTED-DESTRUCTOR
	    (%make-unsafe-finaliser-id collected-type-id)))
	#`(let ((collected-structs (TABLE-FIELD-VECTOR-GETTER #,struct-id)))
	    (do ((i 0 ($fxadd1 i)))
		(($fx= i ($vector-length collected-structs))
		 (hashtable-clear! (TABLE-GETTER #,struct-id)))
	      (guard (E (else (void)))
		(COLLECTED-DESTRUCTOR ($vector-ref collected-structs i)))))))

    (define (%make-unregistration-from-collector struct-id type-id collector-type-id)
      ;;When the  struct definition  includes a collecting  struct type:
      ;;return  a  syntax  object  representing  code  that  unregisters
      ;;instances   of   TYPE-ID   from   the   selected   instance   of
      ;;COLLECTOR-TYPE, if any.
      ;;
      ;;When the struct definition does  not include a collecting struct
      ;;type: return a null syntax object.
      ;;
      (if (identifier? collector-type-id)
	  (with-syntax
	      ((COLLECTOR-GETTER
		(%make-unsafe-Getter-id/collector type-id collector-type-id))
	       (FORGET-COLLECTED-STRUCT
		(%make-forget-collected-struct!-id collector-type-id type-id))
	       (COLLECTOR-INSTANCE
		(%id->id collector-type-id
			 (lambda (collector-type-string)
			   (string-append "collector-" collector-type-string "-instance")))))
	    #`((cond ((COLLECTOR-GETTER #,struct-id)
		      => (lambda (COLLECTOR-INSTANCE)
			   (FORGET-COLLECTED-STRUCT COLLECTOR-INSTANCE #,struct-id))))))
	'()))

    (define (%make-foreign-destructor-call struct-id foreign-destructor-id
					   unsafe-getter-pointer-owner?)
      ;;Return  a  syntax object  representing  the  application of  the
      ;;foreign   data  struct   destructor  to   the  structure;   such
      ;;application  is  performed only  if  the  data struct  owns  the
      ;;pointer object.
      ;;
      (if (identifier? foreign-destructor-id)
	  #`(when (#,unsafe-getter-pointer-owner? #,struct-id)
	      (guard (E (else #f))
		(#,foreign-destructor-id #,struct-id)))
	#f))

    #| end of module: %make-finaliser-definitions |# )

  (define (%make-struct-printer-definition type-id)
    ;;Return   a  syntax   object   representing   the  definition   and
    ;;registration of the printer function for the data struct type.
    ;;
    (with-syntax
	((UNSAFE-GETTER-POINTER		(%make-unsafe-Getter-id/pointer        type-id))
	 (UNSAFE-GETTER-POINTER-OWNER?	(%make-unsafe-Getter-id/pointer-owner? type-id))
	 (UNSAFE-GETTER-UID		(%make-unsafe-Getter-id/uid            type-id))
	 (TYPE-STRING			(%id->string                           type-id)))
      #`(begin
	  (define (the-printer S port sub-printer)
	    (define (%display thing)
	      (display thing port))
	    (define (%write thing)
	      (write thing port))
	    (%display "#[")
	    (%display TYPE-STRING)
	    (%display " pointer=")
	    (%display (UNSAFE-GETTER-POINTER S))
	    (%display " pointer-owner?=")
	    (%display (UNSAFE-GETTER-POINTER-OWNER? S))
	    (%display " uid=")
	    (%display (UNSAFE-GETTER-UID S))
	    (%display "]"))
	  (module ()
	    (set-rtd-printer! (type-descriptor #,type-id) the-printer)))))

  (define (%make-collected-structs-definitions collector-type-id collected-type-id)
    ;;Return a syntax object representing the definition of 3 functions:
    ;;
    ;;1. A function that registers a sub-struct of type TYPE-FIELD-ID in
    ;;   the appropriate table of structs of type TYPE-ID.
    ;;
    ;;2. A function that removes a sub-struct of type TYPE-FIELD-ID from
    ;;   the appropriate table of structs of type TYPE-ID.
    ;;
    ;;3. A  function that returns  a (possibly empty) vector  of structs
    ;;   registered in the field COLLECTED-TYPE-ID.
    ;;
    (with-syntax
	((COLLECTOR-STRUCT
	  (%id->id collector-type-id
		   (lambda (collector-type-string)
		     (string-append "collector-" collector-type-string "-instance"))))
	 (COLLECTED-STRUCT
	  (%id->id collected-type-id
		   (lambda (collected-type-string)
		     (string-append "collected-" collected-type-string "-instance"))))
	 (TABLE-GETTER
	  (%make-unsafe-Getter-id/table-of-collected collector-type-id collected-type-id))
	 (REGISTER-COLLECTED-IN-COLLECTOR
	  (%make-register-collected-in-collector!-id collector-type-id collected-type-id))
	 (FORGET-COLLECTED-STRUCT
	  (%make-forget-collected-struct!-id collector-type-id collected-type-id))
	 (COLLECTOR-CONTAINS-STRUCT?
	  (%make-contains-collected-struct?-id collector-type-id collected-type-id))
	 (UNSAFE-GETTER-UID
	  (%make-unsafe-Getter-id/uid collected-type-id))
	 (VECTOR-OF-COLLECTED-STRUCTS-GETTER
	  (%make-vector-of-collected-structs-getter-id collector-type-id collected-type-id)))
      #`(begin
	  (define (COLLECTOR-CONTAINS-STRUCT? COLLECTOR-STRUCT INSTANCE-STRUCT)
	    (hashtable-contains? (TABLE-GETTER COLLECTOR-STRUCT)
				 (UNSAFE-GETTER-UID INSTANCE-STRUCT)))
	  (define (REGISTER-COLLECTED-IN-COLLECTOR COLLECTOR-STRUCT COLLECTED-STRUCT)
	    (hashtable-set! (TABLE-GETTER COLLECTOR-STRUCT)
			    (UNSAFE-GETTER-UID COLLECTED-STRUCT)
			    COLLECTED-STRUCT))
	  (define (FORGET-COLLECTED-STRUCT COLLECTOR-STRUCT COLLECTED-STRUCT)
	    (hashtable-delete! (TABLE-GETTER COLLECTOR-STRUCT)
			       (UNSAFE-GETTER-UID COLLECTED-STRUCT)))
	  (define (VECTOR-OF-COLLECTED-STRUCTS-GETTER COLLECTOR-STRUCT)
	    (receive (keys vector-of-collected-structs)
		(hashtable-entries (TABLE-GETTER COLLECTOR-STRUCT))
	      vector-of-collected-structs))
	  )))

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

  (define (%make-argument-validator-false-or-pred-id type-id)
    ;;Given an identifier  representing the struct type  name: return an
    ;;identifier representing  the argument validation clause  using the
    ;;type predicate but also accepting false.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "false-or-" type-string))))

  (define (%make-argument-validator-alive-id type-id)
    ;;Given an identifier  representing the struct type  name: return an
    ;;identifier representing  the argument validation clause  using the
    ;;alive type predicate.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append type-string "/alive"))))

  (define (%make-argument-validator-false-or-alive-id type-id)
    ;;Given an identifier  representing the struct type  name: return an
    ;;identifier representing  the argument validation clause  using the
    ;;alive type predicate but also accepting false.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "false-or-" type-string "/alive"))))

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

  (define (%make-unsafe-Getter-id/table-of-collected type-id collected-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the collection getter for the field COLLECTED-ID.
    ;;
    (%id->id type-id (lambda (type-string)
		       (string-append "$" type-string "-table-of-collected-"
				      (%id->string collected-id)))))

  (define (%make-unsafe-Getter-id/collector type-id collector-type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe getter for the field COLLECTOR.
    ;;
    (%id->id type-id
	     (lambda (type-string)
	       (string-append "$" type-string "-"
			      (%id->string (%make-collector-field-id collector-type-id))))))

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

  (define (%make-unsafe-Setter-id/collector type-id collector-type-id)
    ;;Given an  identifier representing the  struct type name  return an
    ;;identifier, in the same context  of TYPE-ID, representing the name
    ;;of the unsafe setter for the field COLLECTOR.
    ;;
    (%id->id type-id
	     (lambda (type-string)
	       (string-append "$set-" type-string "-"
			      (%id->string (%make-collector-field-id collector-type-id))))))

;;; --------------------------------------------------------------------
;;; collector and collected struct ids

  (define (%make-collector-field-id collector-type-id)
    ;;Given  an  identifier  representing  the name  of  a  struct  type
    ;;collector of this struct type:  return the identifier of the field
    ;;referencing the collector struct instance.
    ;;
    (%id->id collector-type-id
	     (lambda (collector-type-string)
	       (string-append "collector-" collector-type-string))))

  (define (%make-table-of-collected-structs-field-id collected-type-id)
    ;;Given  an  identifier  representing  the name  of  a  struct  type
    ;;collected by this struct type:  return the identifier of the field
    ;;referencing the collecting table.
    ;;
    (%id->id collected-type-id
	     (lambda (collected-type-string)
	       (string-append "table-of-collected-" collected-type-string))))

  (define (%make-vector-of-collected-structs-getter-id collector-type-id collected-type-id)
    ;;Given  an identifier  representing  the struct  type  name and  an
    ;;identifier representing a collected-structs table field: return an
    ;;identifier, in the same context of COLLECTOR-TYPE-ID, representing
    ;;the name of  a function returning the  vector of collected-structs
    ;;registered in COLLECTED-TYPE-ID.
    ;;
    (%id->id collector-type-id
	     (lambda (collector-type-string)
	       (string-append "$" collector-type-string "-vector-of-collected-"
			      (%id->string collected-type-id)))))

  (define (%make-register-collected-in-collector!-id collector-type-id collected-type-id)
    ;;Given  an identifier  representing  the struct  type  name and  an
    ;;identifier representing a collected-structs table field: return an
    ;;identifier, in the same context of COLLECTOR-TYPE-ID, representing
    ;;the name of a function that registers a sub-struct in the table of
    ;;a struct.
    ;;
    (%id->id collector-type-id
	     (lambda (collector-type-string)
	       (string-append "$" collector-type-string "-register-"
			      (%id->string collected-type-id)
			      "!"))))

  (define (%make-forget-collected-struct!-id collector-type-id collected-type-id)
    ;;Given  an identifier  representing  the struct  type  name and  an
    ;;identifier representing a collected-structs table field: return an
    ;;identifier, in the same context of COLLECTOR-TYPE-ID, representing
    ;;the name of a function that removes a sub-struct from the table of
    ;;a struct.
    ;;
    (%id->id collector-type-id
	     (lambda (collector-type-string)
	       (string-append "$" collector-type-string "-forget-"
			      (%id->string collected-type-id)
			      "!"))))

  (define (%make-contains-collected-struct?-id collector-type-id collected-type-id)
    ;;Given  an identifier  representing  the struct  type  name and  an
    ;;identifier representing a collected-structs table field: return an
    ;;identifier, in the same context of COLLECTOR-TYPE-ID, representing
    ;;the name of a function that queries the internal table and returns
    ;;true if an object is already collected.
    ;;
    (%id->id collector-type-id
	     (lambda (collector-type-string)
	       (string-append "$" collector-type-string "-contains-"
			      (%id->string collected-type-id)
			      "?"))))

  ;; ------------------------------------------------------------

  (define (%validate-input type-id field-ids
			   foreign-destructor-id collector-type-id collected-type-ids)
    (unless (identifier? type-id)
      (%synner "expected identifier as struct type name" type-id))
    (unless (for-all identifier? field-ids)
      (%synner "expected identifiers as field names" field-ids))
    (unless (or (not foreign-destructor-id)
		(identifier? foreign-destructor-id))
      (%synner "expected #f or identifier as foreign destructor specification"
	       foreign-destructor-id))
    (unless (or (not collector-type-id)
		(identifier? collector-type-id))
      (%synner "expected #f or identifier as collecting struct type"
	       collector-type-id))
    (unless (for-all identifier? collected-type-ids)
      (%synner "expected identifiers as table field names" collected-type-ids)))

  (define (%id->id src-id string-maker)
    (datum->syntax src-id (string->symbol (string-maker (%id->string src-id)))))

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
