;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: internal implementation of DEFINE-RECORD-TYPE
;;;Date: Sun Jul  3, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (ikarus records syntactic)
  (export define-core-record-type
	  define-type-descriptors
	  strip-angular-parentheses)
  (import (vicare)
    (only (ikarus records procedural)
	  $make-record-type-descriptor-ex
	  $make-record-constructor-descriptor
	  $record-and-rtd?)
    (only (vicare system $structs)
	  $struct?
	  $struct-ref
	  $struct-set!)
    (only (vicare expander)
	  syntax-clauses-unwrap
	  syntax-clauses-collapse
	  syntax-clauses-validate-specs
	  syntax-clauses-fold-specs
	  syntax-clauses-validate-specs
	  syntax-clause-spec-keyword
	  syntax-clause-spec?
	  make-syntax-clause-spec))

  (include "cond-boot-expansion.scm" #t)


(define-syntax (define-core-record-type stx)
  ;;This implementation is used to define record-types in the boot image.
  ;;
  ;;NOTE This macro is *not* the one exported by "(rnrs records syntactic (6))", that
  ;;macro's implementation is integrated in the expander.

  (define synner
    (case-lambda
      ((message)
       (syntax-violation 'define-core-record-type message stx #f))
      ((message subform)
       (syntax-violation 'define-core-record-type message stx subform))))

  (define (mkname prefix name suffix)
    (datum->syntax name (string->symbol (string-append prefix (symbol->string (syntax->datum name)) suffix))))

  (define (iota idx stx)
    (syntax-case stx ()
      (()	'())
      ((?x . ?x*)
       (cons idx (iota (fxadd1 idx) #'?x*)))))


(define-constant CLAUSE-SPEC*
  (syntax-clauses-validate-specs
   (list
    ;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
    (make-syntax-clause-spec #'parent				0 1      1 1      '() '())
    (make-syntax-clause-spec #'nongenerative			0 1      0 1      '() '())
    (make-syntax-clause-spec #'define-type-descriptors		0 1      0 0      '() '())
    (make-syntax-clause-spec #'strip-angular-parentheses	0 1      0 0      '() '())
    (make-syntax-clause-spec #'sealed				0 1      1 1      '() '())
    (make-syntax-clause-spec #'opaque				0 1      1 1      '() '())
    (make-syntax-clause-spec #'protocol				0 1      1 1      '() '())
    (make-syntax-clause-spec #'super-protocol			0 1      1 1      '() '())
    (make-syntax-clause-spec #'fields				0 +inf.0 1 +inf.0 '() '())
    (make-syntax-clause-spec #'method				0 +inf.0 2 +inf.0 '() '())
    ;;
    (make-syntax-clause-spec #'custom-printer			0 1      1 1      '() '())
    (make-syntax-clause-spec #'type-predicate			0 1      1 1      '() '())
    (make-syntax-clause-spec #'equality-predicate		0 1      1 1      '() '())
    (make-syntax-clause-spec #'comparison-procedure		0 1      1 1      '() '())
    (make-syntax-clause-spec #'hash-function			0 1      1 1      '() '())
    #| end of LIST |# )))


;;;; record-type definitions: field specification

(define-record-type <field-spec>
  (fields
    (immutable	mutable?)
		;A boolean.
    (immutable	name)
		;A syntactic identifier representing the field name.
    (immutable	accessor)
		;False if the accessor's name was not explicitly specified; otherwise
		;a syntactic identifier to bind to the field's accessor.
    (immutable	mutator)
		;False  if this  field is  immutable or  the mutator's  name was  not
		;explicitly specified;  otherwise a  syntactic identifier to  bind to
		;the field's mutator.
    (immutable	slot-idx)
		;A non-negative fixnum representing the relative offset of this field
		;in the record implementation struct.
    (mutable	accessor-name)
		;A syntactic identifier to bind to the field's accessor.
    (mutable	mutator-name)
		;False or a syntactic identifier to bind to the field's mutator.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (lambda (mutable? name accessor mutator slot-idx)
	(make-record mutable? name accessor mutator slot-idx #f #f))))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (<field-spec>-compute-accessor-name! spec type-name.id)
  (<field-spec>-accessor-name-set! spec (or (<field-spec>-accessor spec)
					    (mkname "" type-name.id
						    (string-append "-"
								   (symbol->string
								    (syntax->datum
								     (<field-spec>-name spec))))))))

(define (<field-spec>-compute-mutator-name! spec type-name.id)
  (<field-spec>-mutator-name-set! spec (or (<field-spec>-mutator spec)
					   (mkname "" type-name.id
						   (string-append "-"
								  (symbol->string
								   (syntax->datum
								    (<field-spec>-name spec)))
								  "-set!")))))

;;; --------------------------------------------------------------------

(define (<field-spec>-method-name spec)
  (mkname "" (<field-spec>-name spec) "-field-method"))

(define (<field-spec>-compose-field-accessor-and-mutator spec first-field-offset.id type-predicate.id)
  (if (<field-spec>-mutable? spec)
      (with-syntax
	  ((ACCESSOR-NAME	(<field-spec>-accessor-name spec))
	   (MUTATOR-NAME	(<field-spec>-mutator-name spec))
	   (METHOD-NAME		(<field-spec>-method-name spec))
	   (TYPE-PREDICATE-ID	type-predicate.id)
	   (SLOT-OFFSET-ID	(mkname "slot-offset-" (<field-spec>-name spec) ""))
	   (SLOT-OFFSET-EXPR	#`(fx+ #,first-field-offset.id #,(<field-spec>-slot-idx spec))))
	#'(begin
	    (define SLOT-OFFSET-ID SLOT-OFFSET-EXPR)
	    (define* (ACCESSOR-NAME {reco TYPE-PREDICATE-ID})
	      ($struct-ref  reco SLOT-OFFSET-ID))
	    (define* (MUTATOR-NAME {reco TYPE-PREDICATE-ID} new-val)
	      ($struct-set! reco SLOT-OFFSET-ID new-val))
	    (case-define* METHOD-NAME
	      (({reco TYPE-PREDICATE-ID})
	       ($struct-ref  reco SLOT-OFFSET-ID))
	      (({reco TYPE-PREDICATE-ID} new-val)
	       ($struct-set! reco SLOT-OFFSET-ID new-val)))
	    #| end of BEGIN |# ))
    (with-syntax
	((ACCESSOR-NAME		(<field-spec>-accessor-name spec))
	 (METHOD-NAME		(<field-spec>-method-name spec))
	 (TYPE-PREDICATE-ID	type-predicate.id)
	 (SLOT-OFFSET-ID	(mkname "slot-offset-" (<field-spec>-name spec) ""))
	 (SLOT-OFFSET-EXPR	#`(fx+ #,first-field-offset.id #,(<field-spec>-slot-idx spec))))
      #'(begin
	  (define SLOT-OFFSET-ID SLOT-OFFSET-EXPR)
	  (define* (ACCESSOR-NAME {reco TYPE-PREDICATE-ID})
	    ($struct-ref reco SLOT-OFFSET-ID))
	  (define METHOD-NAME ACCESSOR-NAME)
	  #| end of BEGIN |# ))))


;;;; record-type definitions: method specification

(define-record-type <method-spec>
  (fields
    (immutable	method-name-id)
		;A syntactic identifier representing the method name.
    (immutable	formals-stx)
		;A  syntax  object representing  the  method  formals for  a  DEFINE*
		;definition.
    (immutable	body-stx)
		;A syntax object representing a list of body forms.
    (mutable	procname)
		;Initialised  to  false.   A syntactic  identifier  representing  the
		;method's procedure name.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (named-lambda make-<method-spec> (method-name.id formals.stx body*.stx)
	(make-record method-name.id formals.stx body*.stx #f))))

  #| end of DEFINE-RECORD-TYPE |# )

(define (<method-spec>-compute-procname! spec type-name.id)
  (<method-spec>-procname-set! spec (mkname "" type-name.id
					    (string-append "-"
							   (symbol->string
							    (syntax->datum
							     (<method-spec>-method-name-id spec)))))))

(define (<method-spec>-compose-definition spec)
  (with-syntax
      ((PROCNAME	(<method-spec>-procname spec))
       (FORMALS		(<method-spec>-formals-stx spec))
       ((BODY ...)	(<method-spec>-body-stx spec)))
    #`(define* (PROCNAME . FORMALS) BODY ...)))


;;;; record-type definitions: clauses parsing results

(define-record-type <parsing-results>
  (fields
    (mutable	type-name)
		;A syntactic identifier representing the name of the record-type.
    (mutable	stripped-type-name)
		;A  syntactic identifier  representing  the name  of the  record-type
		;optionally stripped of angular parentheses.
    (mutable	maker-name)
		;A syntactic identifier to bind to the record constructor.
    (mutable	pred-name)
		;A syntactic identifier to bind to the type predicate.
    (mutable	parent-name)
		;False or a syntactic identifier  representing the name of the parent
		;record-type.
    (mutable	uid)
		;A symbol representing the UID of this record-type.
    (mutable	generative)
		;A boolean.
    (mutable	sealed)
		;A boolean.
    (mutable	opaque)
		;A boolean.
    (mutable	define-type-descriptors)
		;A boolean.  True  if the output form must  include visible syntactic
		;bindings for the RTD and RCD.
    (mutable	strip-angular-parentheses)
		;A boolean.  True if the accessor and mutator syntactic bindings must
		;strip angular parentheses from the record-type name.
    (mutable	field-spec*)
		;A possibly empty list of "<field-spec>" instances.
    (mutable	method-spec*)
		;A possibly empty list of "<field-spec>" instances.
    (mutable	protocol)
		;False or a syntax object  representing an expression which, expanded
		;and evaluated, returns the default constructor protocol.
    (mutable	super-protocol)
		;False or a syntax object  representing an expression which, expanded
		;and evaluated, returns the constructor protocol used by sub-types.
    (mutable	custom-printer)
		;False or a syntax object  representing an expression which, expanded
		;and evaluated, returns the custom-printer function.
    (mutable	type-predicate)
		;False or a syntax object  representing an expression which, expanded
		;and evaluated, returns the type predicate protocol function.
    (mutable	equality-predicate)
		;False or a syntax object  representing an expression which, expanded
		;and evaluated, returns the equality predicate protocol function.
    (mutable	comparison-procedure)
		;False or a syntax object  representing an expression which, expanded
		;and evaluated, returns the comparison procedure protocol function.
    (mutable	hash-function)
		;False or a syntax object  representing an expression which, expanded
		;and evaluated, returns the hash function protocol function.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (lambda ()
	(make-record #f	 ;type-name
		     #f	 ;stripped-type-name
		     #f	 ;maker-name
		     #f	 ;pred-name
		     #f	 ;parent-name
		     #f	 ;uid
		     #t	 ;generative
		     #f	 ;sealed
		     #f	 ;opaque
		     #f	 ;define-type-descriptors
		     #f	 ;strip-angular-parentheses
		     '() ;field-spec*
		     '() ;method-spec*
		     #f	 ;protocol
		     #f	 ;super-protocol
		     #f	 ;custom-printer
		     #f	 ;type-predicate
		     #f	 ;equality-predicate
		     #f	 ;comparison-procedure
		     #f	 ;hash-function
		     ))))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (%strip-angular-parentheses results type-name.id)
  (if (<parsing-results>-strip-angular-parentheses results)
      (let* ((str     (symbol->string (syntax->datum type-name.id)))
	     (str.len (string-length str)))
	(if (and (fx>=? str.len 3)
		 (char=? #\< (string-ref str 0))
		 (char=? #\> (string-ref str (fxsub1 str.len))))
	    (datum->syntax type-name.id (string->symbol (substring str 1 (fxsub1 str.len))))
	  type-name.id))
    type-name.id))

;;; --------------------------------------------------------------------

(define (<parsing-results>-make-parent-ids results)
  (define parent-name.id
    (<parsing-results>-parent-name results))
  (if parent-name.id
      (list (mkname "" parent-name.id "-parent-rtd")
	    (mkname "" parent-name.id "-parent-super-rcd"))
    '(#f #f)))

(define (<parsing-results>-compose-parent-definitions results parent-rtd.id parent-super-rcd.id)
  ;;Return null or a syntax object representing a list of definitions.
  ;;
  (define parent-name.id
    (<parsing-results>-parent-name results))
  (if parent-name.id
      (with-syntax
	  ((PARENT-NAME		parent-name.id))
	#`((define #,parent-rtd.id
	     (PARENT-NAME rtd))
	   (define #,parent-super-rcd.id
	     (PARENT-NAME super-rcd))
	   ))
    #'()))

;;; --------------------------------------------------------------------

(define (<parsing-results>-fields-vec results)
  (list->vector (map (lambda (spec)
		       (list (if (<field-spec>-mutable? spec) #'mutable #'immutable)
			     (<field-spec>-name spec)))
		  (<parsing-results>-field-spec* results))))

(define (<parsing-results>-normalised-fields-vec results)
  (list->vector (map (lambda (spec)
		       (cons (<field-spec>-mutable? spec) (<field-spec>-name spec)))
		  (<parsing-results>-field-spec* results))))

(define (<parsing-results>-compose-method-retriever results)
  (with-syntax
      (((FIELD-NAME		...) (map <field-spec>-name		(<parsing-results>-field-spec*  results)))
       ((FIELD-METHOD-NAME	...) (map <field-spec>-method-name	(<parsing-results>-field-spec*  results)))
       ((METHOD-NAME		...) (map <method-spec>-method-name-id	(<parsing-results>-method-spec* results)))
       ((METHOD-PROCNAME	...) (map <method-spec>-procname	(<parsing-results>-method-spec* results))))
    #'(lambda (field-name)
	(case field-name
	  ((FIELD-NAME)		FIELD-METHOD-NAME)
	  ...
	  ((METHOD-NAME)	METHOD-PROCNAME)
	  ...
	  (else #f)))))

;;; --------------------------------------------------------------------

(define (<parsing-results>-compose-type-predicate results rtd.id)
  (cond ((<parsing-results>-type-predicate results)
	 => (lambda (type-predicate-protocol)
	      #`(#,type-predicate-protocol (lambda (obj) (and ($struct? obj) ($record-and-rtd? obj #,rtd.id))))))
	(else
	 #`(lambda (obj) (and ($struct? obj) ($record-and-rtd? obj #,rtd.id))))))

(define (<parsing-results>-compose-equality-predicate-expr results)
  (define parent-name.id		(<parsing-results>-parent-name        results))
  (define equality-predicate-protocol	(<parsing-results>-equality-predicate results))
  (if parent-name.id
      (if equality-predicate-protocol
	  ;;With parent, equality predicate protocol  defined.  We apply the protocol
	  ;;to the parent's function (or false).
	  #`(#,equality-predicate-protocol (#,parent-name.id equality-predicate))
	;;With parent, equality predicate protocol undefined.
	#`(#,parent-name.id equality-predicate))
    (if equality-predicate-protocol
	;;No parent, equality predicate protocol defined.  We evaluate it as a thunk.
	#`(#,equality-predicate-protocol)
      ;;No parent, no equality predicate protocol undefined.
      #f)))

(define (<parsing-results>-compose-comparison-procedure-expr results)
  (define parent-name.id		(<parsing-results>-parent-name          results))
  (define comparison-procedure-protocol	(<parsing-results>-comparison-procedure results))
  (if parent-name.id
      (if comparison-procedure-protocol
	  ;;With  parent,  comparison  procedure  protocol  defined.   We  apply  the
	  ;;protocol to the parent's function (or false).
	  #`(#,comparison-procedure-protocol (#,parent-name.id comparison-procedure))
	;;With parent,  comparison procedure protocol  undefined.  We just  reuse the
	;;parent's comparison procedure.
	#`(#,parent-name.id comparison-procedure))
    (if comparison-procedure-protocol
	;;No  parent, comparison  procedure protocol  defined.  We  evaluate it  as a
	;;thunk.
	#`(#,comparison-procedure-protocol)
      ;;No parent, no comparison procedure protocol undefined.
      #f)))

(define (<parsing-results>-compose-hash-function-expr results)
  (define parent-name.id		(<parsing-results>-parent-name   results))
  (define hash-function-protocol	(<parsing-results>-hash-function results))
  (if parent-name.id
      (if hash-function-protocol
	  ;;With parent,  hash function protocol  defined.  We apply the  protocol to
	  ;;the parent's function (or false).
	  #`(#,hash-function-protocol (#,parent-name.id hash-function))
	;;With parent, hash function protocol  undefined.  We just reuse the parent's
	;;hash function.
	#`(#,parent-name.id hash-function))
    (if hash-function-protocol
	;;No parent, hash function protocol defined.  We evaluate it as a thunk.
	#`(#,hash-function-protocol)
      ;;No parent, no hash function protocol.
      #'record-hash)))


(define (main input-form.stx)
  (syntax-case input-form.stx ()
    ((_ ?type-spec . ?clauses)
     (let* ((clause*.stx	(syntax-clauses-unwrap #'?clauses synner))
	    (results		(syntax-clauses-fold-specs (lambda (results clause-spec args)
							     (combine results clause-spec args))
				  (make-<parsing-results>)
				  CLAUSE-SPEC* clause*.stx synner)))
       (receive (type-name.id maker-name.id pred-name.id)
	   (%parse-type-spec #'?type-spec)
	 (let ((stripped-type-name.id (%strip-angular-parentheses results type-name.id)))
	   (<parsing-results>-type-name-set!		results type-name.id)
	   (<parsing-results>-stripped-type-name-set!	results stripped-type-name.id)
	   (<parsing-results>-maker-name-set!		results (or maker-name.id (mkname "make-" stripped-type-name.id "")))
	   (<parsing-results>-pred-name-set!		results (or pred-name.id  (mkname ""      stripped-type-name.id "?")))
	   (unless (<parsing-results>-uid results)
	     (<parsing-results>-uid-set! results
					 (datum->syntax type-name.id
							(if (<parsing-results>-generative results)
							    ;;Compose a generative UID.
							    (gensym (syntax->datum type-name.id))
							  ;;Compose a non-generative UID.
							  (string-append "vicare:record-type:"
									 (symbol->string (syntax->datum type-name.id)))))))
	   ;; (receive-and-return (out)
	   ;;     (%build-output results synner)
	   ;;   (debug-print (syntax->datum out)))
	   (%build-output results synner)))))

    (_
     (synner "invalid DEFINE-CORE-RECORD-TYPE syntax use"))))


(define (%build-output results synner)
  (define type-name.id
    (<parsing-results>-type-name results))
  (define stripped-type-name.id
    (<parsing-results>-stripped-type-name results))
  (for-each (lambda (spec)
	      (<field-spec>-compute-accessor-name! spec stripped-type-name.id)
	      (<field-spec>-compute-mutator-name!  spec stripped-type-name.id))
    (<parsing-results>-field-spec* results))
  (for-each (lambda (spec)
	      (<method-spec>-compute-procname! spec stripped-type-name.id))
    (<parsing-results>-method-spec* results))
  (with-syntax
      ((TYPE-NAME		type-name.id)
       (UID			(<parsing-results>-uid results))
       (GENERATIVE		(<parsing-results>-generative results))
       (SEALED			(<parsing-results>-sealed results))
       (OPAQUE			(<parsing-results>-opaque results))
       (PROTOCOL-EXPR		(<parsing-results>-protocol results))
       ((PARENT-RTD-ID PARENT-SUPER-RCD-ID) (<parsing-results>-make-parent-ids results))
       (RTD-ID			(mkname "" type-name.id "-rtd"))
       (RCD-ID			(mkname "" type-name.id "-rcd"))
       (SUPER-RCD-ID		(mkname "" type-name.id "-super-rcd"))
       (MAKER-ID		(<parsing-results>-maker-name results))
       (TYPE-PREDICATE-ID	(<parsing-results>-pred-name  results))
       (FIELDS-VEC		(<parsing-results>-fields-vec results))
       (NORMALISED-FIELDS-VEC	(<parsing-results>-normalised-fields-vec results))
       ((ACCESSOR-NAME ...)	(map <field-spec>-accessor-name (<parsing-results>-field-spec* results)))
       ((MUTATOR-NAME ...)	(fold-right (lambda (field-spec knil)
					      (if (<field-spec>-mutable? field-spec)
						  (cons (<field-spec>-mutator-name field-spec) knil)
						knil))
				  '() (<parsing-results>-field-spec* results)))
       ((METHOD-PROCNAME ...)	(map <method-spec>-procname (<parsing-results>-method-spec* results)))
       (FIRST-FIELD-OFFSET-ID	(mkname "" type-name.id "-first-field-offset"))
       (CUSTOM-PRINTER		(<parsing-results>-custom-printer results))
       (EQUALITY-PREDICATE-ID	(mkname "" type-name.id "-equality-predicate"))
       (COMPARISON-PROCEDURE-ID	(mkname "" type-name.id "-comparison-procedure"))
       (HASH-FUNCTION-ID	(mkname "" type-name.id "-hash-function"))
       (METHOD-RETRIEVER	(<parsing-results>-compose-method-retriever results))
       (IMPLEMENTED-INTERFACES	#f)
       #| end of clauses |# )
    (with-syntax
	(((VISIBLE-BINDING ...)		(if (<parsing-results>-define-type-descriptors results)
					    (list #'RTD-ID #'RCD-ID)
					  '()))
	 ((FIELD-DEFS ...)		(map (lambda (spec)
					       (<field-spec>-compose-field-accessor-and-mutator
						spec #'FIRST-FIELD-OFFSET-ID #'TYPE-PREDICATE-ID))
					  (<parsing-results>-field-spec* results)))
	 ((METHOD-DEFS ...)		(map <method-spec>-compose-definition (<parsing-results>-method-spec* results)))
	 ((PARENT-DEF ...)		(<parsing-results>-compose-parent-definitions results #'PARENT-RTD-ID #'PARENT-SUPER-RCD-ID))
	 (SUPER-PROTOCOL-EXPR		(or (<parsing-results>-super-protocol results) #'PROTOCOL-EXPR))
	 (TYPE-PREDICATE-EXPR		(<parsing-results>-compose-type-predicate            results #'RTD-ID))
	 (EQUALITY-PREDICATE-EXPR	(<parsing-results>-compose-equality-predicate-expr   results))
	 (COMPARISON-PROCEDURE-EXPR	(<parsing-results>-compose-comparison-procedure-expr results))
	 (HASH-FUNCTION-EXPR		(<parsing-results>-compose-hash-function-expr        results))
	 #| end of clauses |# )
      #'(module (TYPE-NAME
		 MAKER-ID TYPE-PREDICATE-ID
		 ACCESSOR-NAME ...
		 MUTATOR-NAME ...
		 METHOD-PROCNAME ...
		 EQUALITY-PREDICATE-ID
		 COMPARISON-PROCEDURE-ID
		 HASH-FUNCTION-ID
		 VISIBLE-BINDING ...)
	  PARENT-DEF ...

	  (define EQUALITY-PREDICATE-ID		EQUALITY-PREDICATE-EXPR)
	  (define COMPARISON-PROCEDURE-ID	COMPARISON-PROCEDURE-EXPR)
	  (define HASH-FUNCTION-ID		HASH-FUNCTION-EXPR)

	  (define RTD-ID
	    (let ((method-retriever METHOD-RETRIEVER))
	      ($make-record-type-descriptor-ex (quote TYPE-NAME) PARENT-RTD-ID
					       (quote UID) GENERATIVE SEALED OPAQUE
					       (quote FIELDS-VEC) (quote NORMALISED-FIELDS-VEC)
					       #f ;destructor
					       CUSTOM-PRINTER
					       EQUALITY-PREDICATE-ID COMPARISON-PROCEDURE-ID HASH-FUNCTION-ID
					       method-retriever method-retriever IMPLEMENTED-INTERFACES)))

	  (define RCD-ID
	    ($make-record-constructor-descriptor RTD-ID PARENT-SUPER-RCD-ID PROTOCOL-EXPR))

	  (define SUPER-RCD-ID
	    ($make-record-constructor-descriptor RTD-ID PARENT-SUPER-RCD-ID SUPER-PROTOCOL-EXPR))

	  (begin
	    (define constructor
	      (record-constructor RCD-ID))
	    (define (MAKER-ID . args)
	      (apply constructor args)))

	  (define TYPE-PREDICATE-ID
	    TYPE-PREDICATE-EXPR)

	  (define FIRST-FIELD-OFFSET-ID
	    ;;The field  at index 3 in  the RTD is: the  index of the first  field of
	    ;;this subtype  in the  layout of  instances; it is  the total  number of
	    ;;fields of the parent type.
	    ($struct-ref RTD-ID 3))

	  FIELD-DEFS ...
	  METHOD-DEFS ...

	  (define-syntax TYPE-NAME
	    (lambda (stx)
	      (syntax-case stx ()
		((_ ?command)
		 (identifier? #'?command)
		 (case (syntax->datum #'?command)
		   ((rtd)			#'RTD-ID)
		   ((rcd)			#'RCD-ID)
		   ((super-rcd)			#'SUPER-RCD-ID)
		   ((equality-predicate)	#'EQUALITY-PREDICATE-ID)
		   ((comparison-procedure)	#'COMPARISON-PROCEDURE-ID)
		   ((hash-function)		#'HASH-FUNCTION-ID)
		   (else
		    (syntax-violation (quote TYPE-NAME)
		      "invalid command for core condition-object type name" stx #'?command))))
		(_
		 (syntax-violation (quote TYPE-NAME) "invalid use of core condition-object type name" stx #f)))))

	  #| end of module |# ))))


(define (%parse-type-spec type-spec.stx)
  (syntax-case type-spec.stx ()
    ((?type-name ?maker-name ?pred-name)
     (begin
       (unless (identifier? #'type-name)
	 (synner "expected identifier as record-type name" #'type-name))
       (unless (identifier? #'maker-name)
	 (synner "expected identifier as record-type constructor name" #'maker-name))
       (unless (identifier? #'pred-name)
	 (synner "expected identifier as record-type predicate name" #'pred-name))
       (values #'?type-name #'?maker-name #'?pred-name)))

    (?type-name
     (begin
       (unless (identifier? #'type-name)
	 (synner "expected identifier as record-type name" #'type-name))
       (values #'?type-name #f #f)))

    (_
     (synner "invalid record-type specification" type-spec.stx))))


(define (combine results clause-spec args)
  ((case-identifiers (syntax-clause-spec-keyword clause-spec)
     ((parent)				%process-clause/parent)
     ((nongenerative)			%process-clause/nongenerative)
     ((define-type-descriptors)		%process-clause/define-type-descriptors)
     ((strip-angular-parentheses)	%process-clause/strip-angular-parentheses)
     ((sealed)				%process-clause/sealed)
     ((opaque)				%process-clause/opaque)
     ((protocol)			%process-clause/protocol)
     ((super-protocol)			%process-clause/super-protocol)
     ((fields)				%process-clause/fields)
     ((method)				%process-clause/method)
     ;;
     ((custom-printer)			%process-clause/custom-printer)
     ((type-predicate)			%process-clause/type-predicate)
     ((equality-predicate)		%process-clause/equality-predicate)
     ((comparison-procedure)		%process-clause/comparison-procedure)
     ((hash-function)			%process-clause/hash-function)
     ;;
     (else
      (assertion-violation 'define-core-record-type "invalid clause spec" clause-spec)))
   results clause-spec args)
  results)


(define (%process-clause/parent results clause-spec args)
  ;;The input clause must have the format:
  ;;
  ;;   (parent ?parent-name)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?parent-name))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (if (identifier? obj)
	(<parsing-results>-parent-name-set! results obj)
      (synner "expected identifier as argument in PARENT clause" obj))))

(define (%process-clause/nongenerative results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (nongenerative)
  ;;   (nongenerative ?uid)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#())
  ;;   #(#(?uid))
  ;;
  ;;If this clause is found: the record-type is non-generative.
  (<parsing-results>-generative-set! results #f)
  (let ((first-item (vector-ref args 0)))
    (unless (vector-empty? first-item)
      (let ((uid (vector-ref first-item 0)))
	(if (identifier? uid)
	    (cond-boot-expansion "setting UID for non-generative record-type"
	      ((inclusion-in-normal-boot-image)
	       (<parsing-results>-uid-set! results uid))
	      ((inclusion-in-rotation-boot-image)
	       (<parsing-results>-uid-set! results (datum->syntax uid (gensym))))
	      ((bootstrapping-for-normal-boot-image)
	       ;;This is never exercised.
	       (<parsing-results>-uid-set! results uid))
	      ((bootstrapping-for-rotation-boot-image)
	       ;;This is never exercised.
	       (<parsing-results>-uid-set! results uid)))
	  (synner "expected empty clause or single identifier argument in NONGENERATIVE clause"
		  (list (syntax-clause-spec-keyword clause-spec) uid)))))))


(define (%process-clause/define-type-descriptors results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (define-type-descriptors)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#())
  ;;
  (<parsing-results>-define-type-descriptors-set! results #t))

(define (%process-clause/strip-angular-parentheses results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (strip-angular-parentheses)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#())
  ;;
  (<parsing-results>-strip-angular-parentheses-set! results #t))

(define (%process-clause/sealed results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (sealed #t)
  ;;   (sealed #f)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(#t))
  ;;   #(#(#f))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (if (boolean? (syntax->datum obj))
	(<parsing-results>-sealed-set! results obj)
      (synner "expected boolean argument in SEALED clause" (list (syntax-clause-spec-keyword clause-spec) obj)))))

(define (%process-clause/opaque results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (opaque #t)
  ;;   (opaque #f)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(#t))
  ;;   #(#(#f))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (if (boolean? (syntax->datum obj))
	(<parsing-results>-opaque-set! results obj)
      (synner "expected boolean argument in OPAQUE clause" (list (syntax-clause-spec-keyword clause-spec) obj)))))

(define (%process-clause/protocol results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (protocol ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (<parsing-results>-protocol-set! results obj)))

(define (%process-clause/super-protocol results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (super-protocol ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (<parsing-results>-super-protocol-set! results obj)))


(module (%process-clause/fields)

  (define (%process-clause/fields results clause-spec args)
    ;;This clause can be present multiple times.   Each input clause must have one of
    ;;the formats:
    ;;
    ;;   (fields ?field-spec ...)
    ;;
    ;;and we expect ARGS to have one of the formats:
    ;;
    ;;   #(#(?field-spec ...) ...)
    ;;
    (let* ((field-spec*.stx (vector-fold-right
				(lambda (arg knil)
				  (append (vector->list arg) knil))
			      '() args)))
      (define make-slot-idx
	(let ((slot-idx (length field-spec*.stx)))
	  (lambda ()
	    (receive-and-return (idx)
		(fxsub1 slot-idx)
	      (set! slot-idx idx)))))
      (<parsing-results>-field-spec*-set! results (fold-right
						      (lambda (field-spec.stx knil)
							(cons (%process-field-spec field-spec.stx (make-slot-idx))
							      knil))
						    '() field-spec*.stx))))

  (define (%process-field-spec field-spec.stx slot-idx)
    (syntax-case field-spec.stx (mutable immutable)
      ((immutable ?field-name)
       (make-<field-spec> #f
			  (%validate-field-name #'?field-name field-spec.stx)
			  #f
			  #f
			  slot-idx))

      ((immutable ?field-name ?accessor)
       (identifier? #'?accessor)
       (make-<field-spec> #f
			  (%validate-field-name #'?field-name field-spec.stx)
			  #'?accessor
			  #f
			  slot-idx))

      ((mutable ?field-name)
       (make-<field-spec> #t
			  (%validate-field-name #'?field-name field-spec.stx)
			  #f
			  #f
			  slot-idx))

      ((mutable ?field-name ?accessor ?mutator)
       (and (identifier? #'?accessor)
	    (identifier? #'?mutator))
       (make-<field-spec> #t
			  (%validate-field-name #'?field-name field-spec.stx)
			  #'?accessor
			  #'?mutator
			  slot-idx))

      (?field-name
       (make-<field-spec> #f
			  (%validate-field-name #'?field-name field-spec.stx)
			  #f
			  #f
			  slot-idx))

      (_
       (synner "invalid field specification" field-spec.stx))))

  (define (%validate-field-name name-spec.stx field-spec.stx)
    (syntax-case name-spec.stx (brace)
      (?field-name
       (identifier? #'?field-name)
       #'?field-name)
      (_
       (synner "invalid field specification" field-spec.stx))))

  #| end of module: %PROCESS-CLAUSE/FIELDS |# )


(module (%process-clause/method)

  (define (%process-clause/method results clause-spec args)
    ;;This clause can be present multiple times.   Each input clause must have one of
    ;;the formats:
    ;;
    ;;   (method (?who . ?formals) ?body0 ?body ...)
    ;;
    ;;and we expect ARGS to have the format:
    ;;
    ;;   #(#((?who . ?formals) ?body0 ?body ...) ...)
    ;;
    (<parsing-results>-method-spec*-set!
     results (vector-fold-left
		 (lambda (knil arg)
		   (cons (%process-method arg) knil))
	       '() args)))

  (define (%process-method vector.stx)
    (syntax-case vector.stx ()
      (#((?method-name . ?formals) ?body0 ?body ...)
       (identifier? #'?method-name)
       (make-<method-spec> #'?method-name #'?formals #'(?body0 ?body ...)))

      (#(?stuff ...)
       (synner "invalid method specification" #'(method ?stuff ...)))))

  #| end of module |# )


(define (%process-clause/custom-printer results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (custom-printer ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (<parsing-results>-custom-printer-set! results obj)))

(define (%process-clause/type-predicate results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (type-predicate ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (<parsing-results>-type-predicate-set! results obj)))

(define (%process-clause/equality-predicate results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (equality-predicate ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (<parsing-results>-equality-predicate-set! results obj)))

(define (%process-clause/comparison-procedure results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (comparison-procedure ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (<parsing-results>-comparison-procedure-set! results obj)))

(define (%process-clause/hash-function results clause-spec args)
  ;;The input clause must have one of the formats:
  ;;
  ;;   (hash-function ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  (let ((obj (vector-ref (vector-ref args 0) 0)))
    (<parsing-results>-hash-function-set! results obj)))


;;;; done DEFINE-CORE-RECORD-TYPE

;; (receive-and-return (out)
;;     (main stx)
;;   (debug-print 'define-core-record-type (syntax->datum out)))
(main stx))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
