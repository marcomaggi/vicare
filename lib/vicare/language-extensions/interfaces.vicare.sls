;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: interfaces for record-types
;;;Date: Sat Jun 25, 2016
;;;
;;;Abstract
;;;
;;;	Interfaces  are  collections  of  method  signatures  that  record-types  can
;;;	implement to expose a common API.
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
(library (vicare language-extensions interfaces (0 4 2016 6 25))
  (options typed-language)
  (export define-interface-type
	  method case-method method/overload
	  method-prototype implements import
	  this

	  &interface-method-late-binding-error
	  make-interface-method-late-binding-error
	  interface-method-late-binding-error?
	  interface-method-late-binding-error.interface-uid
	  interface-method-late-binding-error.method-name
	  interface-method-late-binding-error.subject
	  interface-method-late-binding-error.type-descriptor)
  (import (vicare)
    (prefix (vicare expander)			xp::)
    (prefix (vicare system type-descriptors)	td::)
    (for (vicare expander) expand))

  (define-auxiliary-syntaxes method-prototype)


;;;; run-time utilities: run-time interface method application

(module (interface-method-call-late-binding
	 &interface-method-late-binding-error
	 make-interface-method-late-binding-error
	 interface-method-late-binding-error?
	 interface-method-late-binding-error.interface-uid
	 interface-method-late-binding-error.method-name
	 interface-method-late-binding-error.subject
	 interface-method-late-binding-error.type-descriptor)

  (define* (interface-method-call-late-binding interface.uid method-name.sym subject operands)
    ;;Implement run-time dynamic dispatching of method calls to interface types.
    ;;
    ;;The argument INTERFACE.UID must be the UID of the interface-type.  The argument
    ;;METHOD-NAME.SYM must  be a  symbol representing  the name  of the  method.  The
    ;;argument SUBJECT must be the subject of the method call, the value that will be
    ;;bound to THIS.  The argument OPERANDS must  be a list of additional operands to
    ;;be appended to the call after SUBJECT.
    ;;
    (let ((des (td::type-descriptor-of subject)))
      (define (%error-no-interfaces)
	(%error "the subject's object-type does not implement interfaces"
		interface.uid method-name.sym subject des operands))
      (%method-call des (or (cond ((record-type-descriptor? des)
				   (td::record-type-implemented-interfaces des))
				  ((td::core-type-descriptor? des)
				   (td::core-type-descriptor.implemented-interfaces des))
				  (else
				   (%error-no-interfaces)))
			    (%error-no-interfaces))
		    interface.uid method-name.sym subject operands)))

;;; --------------------------------------------------------------------

  (define (%method-call des table interface.uid method-name.sym subject operands)
    (cond ((vector-find (lambda (entry)
			  (eq? (car entry) interface.uid))
	     table)
	   => (lambda (table-entry)
		;;TABLE-ENTRY is a pair having: as  car, the interface UID; as cdr, a
		;;method retriever procedure.
		(cond (((cdr table-entry) method-name.sym)
		       => (lambda (method-implementation)
			    ;;Method found.   Apply it  to the operands  and return
			    ;;the application's return values.
			    (apply method-implementation subject operands)))
		      (else
		       (%error "the subject's object-type descriptor does not implement the requested interface method"
			       interface.uid method-name.sym subject des operands)))))
	  (else
	   (%error "the subject's object-type descriptor does not implement the requested interface"
		   interface.uid method-name.sym subject des operands))))

;;; --------------------------------------------------------------------

  (define-condition-type &interface-method-late-binding-error
      &method-late-binding-error
    make-interface-method-late-binding-error
    interface-method-late-binding-error?
    (interface-uid	interface-method-late-binding-error.interface-uid)
    (method-name	interface-method-late-binding-error.method-name)
    (subject		interface-method-late-binding-error.subject)
    (descriptor		interface-method-late-binding-error.type-descriptor))

  (define (%error message interface.uid method-name.sym subject des operands)
    (raise
     (condition (make-interface-method-late-binding-error interface.uid method-name.sym subject des)
		(make-who-condition 'interface-method-call-late-binding)
		(make-message-condition message)
		(make-irritants-condition operands))))

  #| end of module: INTERFACE-METHOD-CALL-LATE-BINDING |# )


(define-syntax (define-interface-type input-form.stx)
  (define-constant __module_who__ 'define-interface-type)

  (case-define synner
    ((message)
     (syntax-violation __module_who__ message input-form.stx #f))
    ((message subform)
     (syntax-violation __module_who__ message input-form.stx subform)))

;;; --------------------------------------------------------------------

  (define-constant CLAUSE-SPEC*
    (syntax-clauses-validate-specs
     (list
      ;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
      (new <syntax-clause-spec> #'nongenerative		0 1      0 1      '() '())
      (new <syntax-clause-spec> #'parent		0 1      1 1      '() '())
      (new <syntax-clause-spec> #'implements		0 +inf.0 0 +inf.0 '() '())
      (new <syntax-clause-spec> #'method-prototype	0 +inf.0 2 2      '() '())
      (new <syntax-clause-spec> #'method		0 +inf.0 2 +inf.0 '() '())
      (new <syntax-clause-spec> #'case-method		0 +inf.0 2 +inf.0 '() '())
      (new <syntax-clause-spec> #'method/overload	0 +inf.0 2 +inf.0 '() '())
      #| end of LIST |# )))

;;; --------------------------------------------------------------------

  (define-record-type <method-prototype>
    (fields
      (immutable	{name		xp::<syntactic-identifier>})
		;A syntactic identifier representing the method name.
      (immutable	{procname	xp::<syntactic-identifier>})
		;A syntactic  identifier to be  bound to the  method's implementation
		;procedure.
      (immutable	{signature	xp::<closure-type-spec>})
		;An  instance   of  "<closure-type-spec>"  representing   the  method
		;signature.
      #| end of FIELDS |# )

    (constructor-signature
      (lambda (xp::<syntactic-identifier> xp::<syntactic-identifier> xp::<closure-type-spec>) => (<method-prototype>)))

    (method (signature-annotation)
      ;;Return a syntax object representing the method's signature type annotation.
      (xp::object-type-spec.type-annotation (.signature this))))

;;; --------------------------------------------------------------------

  (define-record-type <parsing-results>
    (fields
      (immutable	type-name)
		;Syntactic identifier representing the name of this interface.
      (mutable		uid)
		;Unique identifier associated to this type.
      (mutable		parent-ots)
		;False or an instance "<interface-type-spec>" representing the parent
		;interface-type.
      (mutable		implemented-interfaces)
		;A   possibly  empty   list   of  "<interface-type-spec>"   instances
		;representing the interfaces implemented by this interface-type.
      (mutable		definitions)
		;Proper  list of  syntax objects  representing definition  forms that
		;must go in the output of this macro.
      (mutable		required-prototypes)
		;A   list   of   "<method-prototype>"  instances   representing   the
		;required-method specifications.  This list  holds an object for each
		;method that must  be implemented by the  object-types that implement
		;this interface-type.
      (mutable		method-prototypes-table)
		;An  alist having:  as keys,  symbols representing  method names;  as
		;values, instances of "<closure-type-spec>" representing the methods'
		;required type signatures.
		;
		;This table is initialised with the  parent's alist, if any, and with
		;this interface-type's prototypes by the FINALISE method.
      (mutable		default-prototypes)
		;A   list   of   "<method-prototype>"  instances   representing   the
		;default-method specifications.   This list holds an  object for each
		;default method implemented by this interface-type.
      #| end of FIELDS |# )

    (protocol
      (lambda (make-record)
	(lambda (type-name.id)
	  (make-record type-name.id
		       #f  ;uid
		       #f  ;parent-ots
		       '() ;implemented-interfaces
		       '() ;definitions
		       '() ;required-prototypes
		       '() ;method-prototypes-table
		       '() ;default-prototypes
		       ))))

    (constructor-signature
      (lambda (xp::<syntactic-identifier>) => (<parsing-results>)))

    (method (push-definition! definition.stx)
      (.definitions this (cons definition.stx (.definitions this))))

    (method (type-descriptor-id)
      ;;Return  the syntactic  identifier that  will be  bound to  the run-time  type
      ;;descriptor for this interface-type.
      ;;
      (identifier-suffix (.type-name this) "-type-descriptor"))

    ;; ------------------------------------------------------------

    (method (implemented-interfaces-append! {iface*.ots (list-of xp::<interface-type-spec>)})
      ;;Register  a list  of  interface-type  specifications representing  interfaces
      ;;implemented by this interface-type.
      ;;
      (.implemented-interfaces this (append iface*.ots (.implemented-interfaces this))))

    (method (implemented-interfaces-ots)
      ;;Return  a  syntax  object  representing an  expression  which,  expanded  and
      ;;evaluated, returns a proper  list of "<interface-type-spec>" representing the
      ;;implemented interface-types.
      ;;
      (if (null? (.implemented-interfaces this))
	  #'(quote ())
	#`(list #,@(map (lambda (ots)
			  #`(quote #,ots))
		     (.implemented-interfaces this)))))

    (method (interface-validation-forms)
      ;;Return a  syntax object representing  a (possibly empty) list  of expressions
      ;;which, expanded and evaluated,  validates this interface-type as implementing
      ;;the interface-types declared in the IMPLEMENTS clauses.
      ;;
      (cond ((null? (.implemented-interfaces this))
	     #'())
	    ((list-of-single-item? (.implemented-interfaces this))
	     (with-syntax
		 ((IMPLEMENTED-IFACE-OTS	(car (.implemented-interfaces this)))
		  (IMPLEMENTER-IFACE-ID		(.type-name this)))
	       #`((begin-for-syntax
		    (xp::assert-implemented-interface-type-and-implementer-interface-type
		     (quote IMPLEMENTED-IFACE-OTS)
		     (xp::type-annotation->object-type-spec (syntax IMPLEMENTER-IFACE-ID)))))))
	    (else
	     (with-syntax
		 ((IMPLEMENTED-INTERFACES-OTS	(.implemented-interfaces-ots this))
		  (IMPLEMENTER-IFACE-ID		(.type-name this)))
	       #`((begin-for-syntax
		    (let ((implementer-iface.ots (xp::type-annotation->object-type-spec (syntax IMPLEMENTER-IFACE-ID))))
		      (for-each (lambda (implemented-iface.ots)
				  (xp::assert-implemented-interface-type-and-implementer-interface-type
				   implemented-iface.ots implementer-iface.ots))
			IMPLEMENTED-INTERFACES-OTS))))))))

    ;; (method (implemented-interface-uids)
    ;; 	;;Return  a syntax  object  representing an  expression  which, expanded  and
    ;; 	;;evaluated, returns a  proper list of symbols representing the  list of UIDs
    ;; 	;;associated to the implemented interfaces.
    ;; 	;;
    ;; 	#`(list #,@(map (lambda (id)
    ;; 			  #`(car (type-unique-identifiers #,id)))
    ;; 		     (.implemented-interfaces this))))

    ;; ------------------------------------------------------------

    (method (push-required-prototype! {proto <method-prototype>})
      ;;Register a  method prototype's  specification parsed from  a METHOD-PROTOTYPE
      ;;clause.  Further validations will be performed later.
      ;;
      (.required-prototypes this (cons proto (.required-prototypes this))))

    (method (method-prototypes-table-stx)
      ;;Build and return  a syntax object representing an  expression which, expanded
      ;;and evaluated, builds an alist representing the method prototypes table.  The
      ;;alist includes the entries from the parent's table, if any.
      ;;
      (if (null? (.method-prototypes-table this))
	  #'(quote ())
	#`(list . #,(map (lambda ({entry (pair <symbol> <closure-type-spec>)})
			   #`(cons (quote #,(datum->syntax (.type-name this) (car entry)))
				   (quote #,(cdr entry))))
		      (.method-prototypes-table this)))))

    (method (methods-table-stx)
      ;;Build and return  a syntax object representing an  expression which, expanded
      ;;and evaluated, builds a methods  table for this interface-type specification.
      ;;
      ;;The table is an alist having: as keys, symbols representing the method names;
      ;;as  values,   syntactic  identifiers  bound  to   the  method  implementation
      ;;functions.  The method implementation functions will perform run-time dynamic
      ;;dispatch to the concrete method's implementations.
      ;;
      ;;The table will be stored in the "<interface-type-spec>" instance representing
      ;;this interface-type  and used at expand-time;  it includes only an  entry for
      ;;each method  prototype defined  in this  interface-type, the  parent's method
      ;;prototypes are left out.
      ;;
      (if (null? (.required-prototypes this))
	  #'(quote ())
	#`(list . #,(map (lambda ({proto <method-prototype>})
			   #`(cons (quote  #,(.name     proto))
				   (syntax #,(.procname proto))))
		      (.required-prototypes this)))))

    ;; ------------------------------------------------------------

    (method (finalise)
      ;;After  all   the  clauses  have  been   parsed,  we  need  to   perform  some
      ;;post-processing finalisation.  This method does it.
      ;;
      (define parent-method-prototypes-table
	(if (.parent-ots this)
	    (let (({parent.ots <interface-type-spec>} (.parent-ots this)))
	      ;;The  parent's  method prototypes  table  becomes  the base  for  this
	      ;;interface-type's table.
	      (.method-prototypes-table parent.ots))
	  '()))

      ;;If there is no UID: generate one.
      (.uid this (or (.uid this) (%build-generative-uid (.type-name this))))

      (.method-prototypes-table this
	(fold-left (lambda (method-prototypes-table {proto <method-prototype>})
		     (let ((method-name.sym (syntax->datum (.name proto))))
		       ;;Check   for  duplicate   method   names   with  the   parent's
		       ;;specification.   We want  a descriptive  error message,  so we
		       ;;check both the parent's table and this type's table.
		       (cond ((assq method-name.sym parent-method-prototypes-table)
			      (synner "duplicate method name between this interface-type and its parent"
				      method-name.sym)))
		       ;;Check for duplicate method names in the defined methods.
		       (cond ((assq method-name.sym method-prototypes-table)
			      (synner "duplicate method name in interface-type definition" method-name.sym)))
		       ;;For  every required-method  prototype  we  create the  dynamic
		       ;;dispatch function.
		       (.push-definition! this
			 #`(define/std (#,(.procname proto) subject . args)
			     (interface-method-call-late-binding (quote #,(.uid this)) (quote #,(.name proto)) subject args)))
		       ;;Add an entry to the method prototypes table.
		       (acons method-name.sym (.signature proto) method-prototypes-table)))
	  parent-method-prototypes-table (.required-prototypes this)))

      #| end of FINALISE |# )

    #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

  (define (main input-form.stx)
    (syntax-case input-form.stx ()
      ((_ ?type-name . ?clauses)
       (begin
	 (unless (identifier? #'?type-name)
	   (synner "expected identifier as interface type name" #'?type-name))
	 ;; (receive-and-return (out)
	 ;;     (%build-output (%parse-clauses #'?type-name #'?clauses))
	 ;;   (debug-print (syntax->datum out)))
	 (%build-output (%parse-clauses #'?type-name #'?clauses))))

      (_
       (synner "invalid DEFINE-INTERFACE-TYPE syntax use"))))

  (define ({%parse-clauses <parsing-results>} {type-name.id xp::<syntactic-identifier>} clauses.stx)
    (syntax-clauses-fold-specs combine (new <parsing-results> type-name.id)
			       CLAUSE-SPEC* (syntax-clauses-unwrap clauses.stx synner)
			       synner))

  (define (%build-output {results <parsing-results>})
    (.finalise results)
    (with-syntax
	((TYPE-NAME				(.type-name			results))
	 (UID					(.uid				results))
	 (TYPE-DESCRIPTOR			(.type-descriptor-id		results))
	 (PARENT-OTS				(.parent-ots			results))
	 (METHOD-PROTOTYPES-TABLE		(.method-prototypes-table-stx	results))
	 (IMPLEMENTED-INTERFACES		(.implemented-interfaces-ots	results))
	 (METHODS-TABLE				(.methods-table-stx		results))
	 ((DEFINITION ...)			(.definitions			results))
	 ((INTERFACE-VALIDATION-FORM ...)	(.interface-validation-forms	results)))
      #'(module (TYPE-NAME)
	  ;; (define/typed {TYPE-DESCRIPTOR td::<interface-type-descr>}
	  ;;   (td::make-interface-type-descr (quote TYPE-NAME) (quote UID)
	  ;; 				     (quote METHOD-PROTOTYPE-NAMES) (quote IMPLEMENTED-METHOD-NAMES)
	  ;; 				     IMPLEMENTED-INTERFACE-UIDS METHOD-RETRIEVER))
	  (define-syntax TYPE-NAME
	    (xp::make-interface-type-spec (syntax TYPE-NAME) (quote UID) (syntax TYPE-DESCRIPTOR) (quote PARENT-OTS)
					  METHOD-PROTOTYPES-TABLE METHODS-TABLE IMPLEMENTED-INTERFACES))
	  DEFINITION ...
	  ;;We want this validation code after the definitions.
	  INTERFACE-VALIDATION-FORM ...)))

;;; --------------------------------------------------------------------

  (define-type <parsed-args>
    (vector-of (vector-of <top>)))

  (define (combine {results <parsing-results>} {clause-spec <syntax-clause-spec>} {args <parsed-args>})
    ((case-identifiers (.keyword clause-spec)
       ((method)		%process-clause/method)
       ((case-method)		%process-clause/case-method)
       ((method/overload)	%process-clause/method-overload)
       ((method-prototype)	%process-clause/method-prototype)
       ((nongenerative)		%process-clause/nongenerative)
       ((implements)		%process-clause/implements)
       ((parent)		%process-clause/parent)
       (else
	(assertion-violation __module_who__ "invalid clause spec" clause-spec)))
     results args))

;;; --------------------------------------------------------------------

  (define (%process-clause/nongenerative {results <parsing-results>} {args <parsed-args>})
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
    (let ((arg (vector-ref args 0)))
      (.uid results (if (vector-empty? arg)
			(%build-nongenerative-uid (.type-name results))
		      (receive-and-return (obj)
			  (vector-ref arg 0)
			(unless (identifier? obj)
			  (synner "expected identifier as argument in NONGENERATIVE clause" obj))))))
    results)

;;; --------------------------------------------------------------------

  (define (%process-clause/parent {results <parsing-results>} {args <parsed-args>})
    ;;The input clause must have the format:
    ;;
    ;;   (parent ?interface-name)
    ;;
    ;;and we expect ARGS to have one of the format:
    ;;
    ;;   #(#(?interface-name))
    ;;
    (let ((parent.id (vector-ref (vector-ref args 0) 0)))
      (unless (identifier? parent.id)
	(synner "expected identifier as argument in PARENT clause" parent.id))
      (let (({parent.ots xp::<object-type-spec>} (with-exception-handler
						     (lambda (E)
						       (let* ((msg "error dereferencing parent name")
							      (msg (if (message-condition? E)
								       (string-append msg ": " (condition-message E))
								     msg)))
							 (synner msg parent.id)))
						   (lambda ()
						     (xp::type-annotation->object-type-spec parent.id)))))
	(unless (interface-type-spec? parent.ots)
	  (synner "expected interface-type name as argument in PARENT clause" parent.id))
	(.parent-ots results parent.ots)))
    results)

;;; --------------------------------------------------------------------

  (define (%process-clause/implements {results <parsing-results>} {args <parsed-args>})
    ;;This input clause can appear multiple times, each clause must have the format:
    ;;
    ;;   (implements ?interface)
    ;;
    ;;and we expect ARGS to have one of the format:
    ;;
    ;;   #(#(?interface ...) ...)
    ;;
    (let ((iface*.id (vector-fold-right
			 (lambda (arg knil)
			   (vector-fold-right
			       (lambda (iface.id knil)
				 (unless (identifier? iface.id)
				   (synner "expected identifier as argument of IMPLEMENTS clause" iface.id))
				 (cons iface.id knil))
			     knil arg))
		       '() args)))
      (cond ((duplicate-identifiers? iface*.id)
	     => (lambda (id)
		  (synner "implemented interface declared multiple times" id)))
	    (else
	     (let ((iface*.ots (map (lambda (iface.id)
				      (let (({iface.ots xp::<object-type-spec>}
					     (with-exception-handler
						 (lambda (E)
						   (let* ((msg "error dereferencing implemented interface name")
							  (msg (if (message-condition? E)
								   (string-append msg ": " (condition-message E))
								 msg)))
						     (synner msg iface.id)))
					       (lambda ()
						 (xp::type-annotation->object-type-spec iface.id)))))
					(if (interface-type-spec? iface.ots)
					    iface.ots
					  (synner "expected interface-type name as argument in IMPLEMENTS clause" iface.id))))
				 iface*.id)))
	       (.implemented-interfaces-append! results iface*.ots)
	       results)))))

;;; --------------------------------------------------------------------

  (module (%process-clause/method-prototype)

    (define (%process-clause/method-prototype {results <parsing-results>} {args <parsed-args>})
      ;;This clause can  be present multiple times.  Each input  clause must have the
      ;;format:
      ;;
      ;;   (method-prototype ?name ?signature)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?name ?signature) ...)
      ;;
      (vector-fold-left (lambda (results arg)
			  (%process-method-prototype-spec results arg synner))
	results args))

    (define (%process-method-prototype-spec {results <parsing-results>} arg synner)
      ;;We expect ARG to have the format:
      ;;
      ;;   #((?name ?signature))
      ;;
      ;;Update results with the result of parsing.
      ;;
      ;;NOTE  We  prepend  a  "<bottom>"  type  annotation  to  the  arguments'  type
      ;;signature; such annotation will match the method's implicit THIS argument.
      ;;
      (syntax-case arg ()
	(#(?method-name ?signature)
	 (identifier? #'?method-name)
	 (let* ((signature.stx	(%add-bottom-arguments #'?signature synner))
		(signature.ots	(xp::type-annotation->object-type-spec signature.stx))
		(method-procname.id	(xp::identifier-method-procname (.type-name results) #'?method-name)))
	   (.push-required-prototype! results (new <method-prototype> #'?method-name method-procname.id signature.ots))
	   results))

	(#(?stuff ...)
	 (synner "invalid METHOD-PROTOTYPE specification" #'(method-prototype ?stuff ...)))))

    (define (%add-bottom-arguments signature.stx synner)
      (syntax-case signature.stx (case-lambda lambda =>)
	((lambda ?formals => ?retvals)
	 #'(lambda (<bottom> . ?formals) => ?retvals))

	((case-lambda ?clause-signature0 ?clause-signature ...)
	 #`(case-lambda
	     #,(map (lambda (clause.stx)
		      (syntax-case clause.stx ()
			((?formals => ?retvals)
			 #'((<bottom> . ?formals) => ?retvals))
			(_
			 (synner "invalid method prototype signature" signature.stx))))
		 (syntax->list #'(?clause-signature0 ?clause-signature ...)))))

	(_
	 (synner "invalid method prototype signature" signature.stx))))

    #| end of module: %PROCESS-CLAUSE/METHOD-PROTOTYPE |# )

;;; --------------------------------------------------------------------

  (module (%process-clause/method)

    (define (%process-clause/method {results <parsing-results>} {args <parsed-args>})
      ;;This clause can  be present multiple times.  Each input  clause must have the
      ;;format:
      ;;
      ;;   (method (?who . ?formals) . ?body)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #((?who . ?formals) . ?body)
      ;;
      (vector-fold-left (lambda (results arg)
			  (%process-method-spec results arg synner))
	results args))

    (define (%process-method-spec {results <parsing-results>} arg synner)
      ;;The METHOD clause can be present multiple times.  Each input clause must have
      ;;the format:
      ;;
      ;;   (method (?who . ?formals) . ?body)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #((?who . ?formals) . ?body)
      ;;
      (syntax-case arg ()
	(#((?who . ?formals) ?body0 ?body ...)
	 (receive (method-name.id method-procname.id method-who.stx)
	     (%parse-method-who results #'?who synner)
	   ;; (.push-definition! results #`(define/checked (#,method-who.stx {subject #,(.type-name results)} . ?formals)
	   ;; 				    (fluid-let-syntax ((this (identifier-syntax subject)))
	   ;; 				      ?body0 ?body ...)))
	   ;; (.push-default-prototype! results (vector method-name.id method-procname.id signature.stx signture.ots))
	   results))
	(#(?stuff ...)
	 (synner "invalid METHOD specification" #'(method ?stuff ...)))))

    #| end of module: %PROCESS-CLAUSE/METHOD-PROTOTYPE |# )

;;; --------------------------------------------------------------------

  (module (%process-clause/case-method)

    (define (%process-clause/case-method {results <parsing-results>} {args <parsed-args>})
      ;;This clause can  be present multiple times.  Each input  clause must have the
      ;;format:
      ;;
      ;;   (case-method ?who . ?case-method-clauses)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?who ?case-method-clause0 ?case-method-clause ...) ...)
      ;;
      (vector-fold-left (lambda (results arg)
			  (%process-case-method-spec results arg synner))
	results args))

    (define (%process-case-method-spec {results <parsing-results>} arg synner)
      ;;We expect the ARG argument to have the format:
      ;;
      ;;   #(?who ?case-method-clause0 ?case-method-clause ...)
      ;;
      (syntax-case arg ()
	(#(?who ?case-method-clause0 ?case-method-clause ...)
	 (let* ((method-name.id	#'?who)
		(method-procname.id	(identifier-method-procname (.type-name results) #'?who))
		(clause*.stx		(map (lambda (clause.stx)
					       (%add-this-to-clause-formals results clause.stx synner))
					  (syntax->list #'(?case-method-clause0 ?case-method-clause ...)))))
	   ;; (.push-definition!	results #`(case-define/checked #,method-procname.id . #,clause*.stx))
	   ;; (.push-default-prototype! results (vector method-name.id method-procname.id signature.ots))
	   results))

	(#(?stuff ...)
	 (synner "invalid CASE-METHOD specification" #'(case-method ?stuff ...)))))

    (define (%add-this-to-clause-formals {results <parsing-results>} clause.stx synner)
      (syntax-case clause.stx (brace)
	((((brace ?underscore . ?rv-types) . ?formals) ?body0 ?body ...)
	 (%underscore-id? #'?underscore)
	 #`(({?underscore . ?rv-types} {subject #,(.type-name results)} . ?formals)
	    (fluid-let-syntax ((this (identifier-syntax subject)))
	      ?body0 ?body ...)))

	((?formals ?body0 ?body ...)
	 #`(({subject #,(.type-name results)} . ?formals)
	    (fluid-let-syntax ((this (identifier-syntax subject)))
	      ?body0 ?body ...)))
	(_
	 (synner "invalid CASE-METHOD clause syntax" clause.stx))))

    (define (%underscore-id? stx)
      (and (identifier? stx)
	   (eq? '_ (syntax->datum stx))))

    #| end of module: %PROCESS-CASE-METHOD-SPEC |# )

;;; --------------------------------------------------------------------

  (module (%process-clause/method-overload)

    (define (%process-clause/method-overload {results <parsing-results>} {args <parsed-args>})
      ;;This clause can  be present multiple times.  Each input  clause must have the
      ;;format:
      ;;
      ;;   (method/overload (?who . ?formals) . ?body)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#((?who . ?formals) . ?body) ...)
      ;;
      (vector-fold-left (lambda (results arg)
			  (%process-method-overload-spec results arg synner))
	results args))

    (define (%process-method-overload-spec {results <parsing-results>} arg synner)
      ;;We expect the ARG argument to have the format:
      ;;
      ;;   #((?who . ?formals) . ?body)
      ;;
      (syntax-case arg ()
	(#((?who . ?formals) ?body0 ?body ...)
	 (receive (method-name.id method-procname.id method-who.stx)
	     (%parse-method-who results #'?who synner)
	   ;; (.push-definition! results #`(define/overload (#,method-who.stx {subject #,(.type-name results)} . ?formals)
	   ;; 				    (fluid-let-syntax ((this (identifier-syntax subject)))
	   ;; 				      ?body0 ?body ...)))
	   ;; (.push-default-prototype! results (vector method-name.id method-procname.id signature.ots))
	   results))

	(#(?stuff ...)
	 (synner "invalid METHOD/OVERLOAD specification" #'(method/overload ?stuff ...)))))

    #| end of module: %PROCESS-CLAUSE/METHOD-OVERLOAD |# )

;;; --------------------------------------------------------------------

  (define (%parse-method-who {results <parsing-results>} who.stx synner)
    (syntax-case who.stx (brace)
      (?method-name
       (identifier? #'?method-name)
       (let ((method-procname.id (identifier-method-procname (.type-name results) #'?method-name)))
	 (values #'?method-name method-procname.id method-procname.id)))
      ((brace ?method-name . ?rv-types)
       (identifier? #'?method-name)
       (let ((method-procname.id (identifier-method-procname (.type-name results) #'?method-name)))
	 (values #'?method-name method-procname.id #`(brace #,method-procname.id . ?rv-types))))
      (_
       (synner "invalid method name specification" who.stx))))

  (define (%build-nongenerative-uid type-name.id)
    ;;Build and  return a symbol to  be used as  UID for this interface-type  for the
    ;;case of: non-generative type.
    ;;
    (datum->syntax type-name.id
		   (string->symbol (string-append "vicare:nongenerative:"
						  (symbol->string (syntax->datum type-name.id))))))

  (define (%build-generative-uid type-name.id)
    ;;Build and  return a symbol to  be used as  UID for this interface-type  for the
    ;;case of: generative type.
    ;;
    (datum->syntax type-name.id
		   (gensym (syntax->datum type-name.id))))

  (define-syntax-rule (acons ?key ?val ?alist)
    (cons (cons ?key ?val) ?alist))

;;; --------------------------------------------------------------------

  (main input-form.stx))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put '.push-definition!		'scheme-indent-function 1)
;; eval: (put '.methods-table			'scheme-indent-function 1)
;; eval: (put '.method-prototypes-table		'scheme-indent-function 1)
;; End:
