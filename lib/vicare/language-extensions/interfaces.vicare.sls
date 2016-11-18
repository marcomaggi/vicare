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
  (export
    define-interface-type this
    method method-prototype implements parent)
  (import (vicare)
    (prefix (vicare expander)			xp::)
    (prefix (vicare system type-descriptors)	td::)
    (prefix (only (psyntax system $all)
		  make-interface-type-spec)
	    sys::)
    (for (vicare expander) expand))

  (define-auxiliary-syntaxes method-prototype)


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
      #| end of LIST |# )))

;;; --------------------------------------------------------------------

  (define-record-type <method-prototype>
    (fields
      (immutable	{name-id		xp::<syntactic-identifier>})
		;A syntactic identifier representing the method name.
      (immutable	{name-sym		<symbol>})
		;A symbol representing the method name.
      (immutable	{signature		xp::<closure-type-spec>})
		;An  instance   of  "<closure-type-spec>"  representing   the  method
		;signature.
      (immutable	{default-procname-id	(or <false> xp::<syntactic-identifier>)})
		;False  or  a  syntactic  identifier  bound  to  the  default  method
		;implementation procedure.
      #| end of FIELDS |# )

    (constructor-signature
      (lambda (xp::<syntactic-identifier> xp::<closure-type-spec> (or <false> xp::<syntactic-identifier>))
	=> (<method-prototype>)))

    (protocol
      (lambda (make-record)
	(lambda (name-id signature default-procname.id)
	  (make-record name-id (syntax->datum name-id) signature default-procname.id)))))

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
      (mutable		parsed-prototypes)
		;A   list   of   "<method-prototype>"  instances   representing   the
		;required-method specifications.  This list  holds an object for each
		;method that must  be implemented by the  object-types that implement
		;this interface-type.
      (mutable		method-prototypes-table)
		;An  alist having:  as keys,  symbols representing  method names;  as
		;values, pairs with the format:
		;
		;   (?closure-ots . ?has-default)
		;
		;where:   ?CLOSURE-OTS  is   an  instance   of  "<closure-type-spec>"
		;representing the methods' required  type signatures; ?HAS-DEFAULT is
		;a boolean, true if this method has a default implementation.
		;
		;This table is initialised with the  parent's alist, if any, and with
		;this interface-type's prototypes by the FINALISE method.
      (mutable		default-prototypes)
		;A   list   of   "<method-prototype>"  instances   representing   the
		;default-method specifications.   This list holds an  object for each
		;default method implemented by this interface-type.
      (mutable		methods-table)
		;An  alist having:  as keys,  symbols representing  method names;  as
		;values,  syntactic identifiers  bound to  the method  implementation
		;functions.
      #| end of FIELDS |# )

    (protocol
      (lambda (make-record)
	(lambda (type-name.id)
	  (make-record type-name.id
		       #f  ;uid
		       #f  ;parent-ots
		       '() ;implemented-interfaces
		       '() ;definitions
		       '() ;parsed-prototypes
		       '() ;method-prototypes-table
		       '() ;default-prototypes
		       '() ;methods-table
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

    (method (parent-type-descriptor-id)
      ;;Return a syntax  object representing a Scheme expression  which, expanded and
      ;;evaluated, returns  the parent's  type descriptor,  if any.   If there  is no
      ;;parent: the expression returns false.
      ;;
      (if (.parent-ots this)
	  (interface-type-spec.type-descriptor-id (.parent-ots this))
	#f))

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
	#`(list #,@(map (lambda ({_ <top>} ots)
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
		     (xp::make-type-specification (syntax IMPLEMENTER-IFACE-ID)))))))
	    (else
	     (with-syntax
		 ((IMPLEMENTED-INTERFACES-OTS	(.implemented-interfaces-ots this))
		  (IMPLEMENTER-IFACE-ID		(.type-name this)))
	       #`((begin-for-syntax
		    (let ((implementer-iface.ots (xp::make-type-specification (syntax IMPLEMENTER-IFACE-ID))))
		      (for-each (lambda (implemented-iface.ots)
				  (xp::assert-implemented-interface-type-and-implementer-interface-type
				   implemented-iface.ots implementer-iface.ots))
			IMPLEMENTED-INTERFACES-OTS))))))))

    (method (implemented-interface-uids-stx)
      ;;Return  a syntax  object  representing an  expression  which, expanded  and
      ;;evaluated, returns a  proper list of symbols representing the  list of UIDs
      ;;associated to the implemented interfaces.
      ;;
      #`(quote #,(map (lambda ({_ <top>} iface.ots)
			(datum->syntax (.type-name this) (car (xp::object-type-spec.uids-list iface.ots))))
		   (.implemented-interfaces this))))

    ;; ------------------------------------------------------------

    (method (push-required-prototype! {proto <method-prototype>})
      ;;Register a  method prototype's  specification parsed from  a METHOD-PROTOTYPE
      ;;clause.  Further validations will be performed later.
      ;;
      (.parsed-prototypes this (cons proto (.parsed-prototypes this))))

    (method (method-prototypes-table-stx)
      ;;Build and return  a syntax object representing an  expression which, expanded
      ;;and evaluated, builds an alist representing the method prototypes table.  The
      ;;alist includes the entries from the parent's table, if any.
      ;;
      (if (null? (.method-prototypes-table this))
	  #'(quote ())
	#`(list . #,(map (lambda ({_ <top>} {entry (pair <symbol> (pair <closure-type-spec> <boolean>))})
			   (with-syntax
			       ((METHOD-NAME	(datum->syntax (.type-name this) (car entry)))
				(CLOSURE-OTS	(cadr entry))
				(HAS-DEFAULT?	(cddr entry)))
			     #`(cons (quote METHOD-NAME) (cons (quote CLOSURE-OTS) HAS-DEFAULT?))))
		      (.method-prototypes-table this)))))

    (method (methods-table-stx)
      ;;Build and return  a syntax object representing an  expression which, expanded
      ;;and evaluated, builds a methods  table for this interface-type specification.
      ;;The table will be stored in the "<interface-type-spec>" instance representing
      ;;this interface-type and used at expand-time.
      ;;
      ;;The table is an alist having: as keys, symbols representing the method names;
      ;;as  values,   syntactic  identifiers  bound  to   the  method  implementation
      ;;functions.  The method implementation functions will perform run-time dynamic
      ;;dispatch to the concrete method's implementations.
      ;;
      (if (null? (.methods-table this))
	  #'(quote ())
	#`(list . #,(map (lambda ({_ <top>} entry)
			   (with-syntax
			       ((METHOD-NAME		(datum->syntax (.type-name this) (car entry)))
				(METHOD-PROCNAME	(cdr entry)))
			     #`(cons (quote METHOD-NAME) (syntax METHOD-PROCNAME))))
		      (.methods-table this)))))

    (method (method-prototype-names-stx)
      ;;Build and return  a syntax object representing an  expression which, expanded
      ;;and evaluated,  returns a list  of symbols representing the  prototype names.
      ;;The list will be stored in the run-time interface-type type-descriptor.
      ;;
      (if (null? (.methods-table this))
	  #'(quote ())
	#`(quote #,(map (lambda ({_ <top>} entry)
			  (datum->syntax (.type-name this) (car entry)))
		     (.methods-table this)))))

    (method (method-retriever-stx)
      ;;Build and return  a syntax object representing an  expression which, expanded
      ;;and evaluated, returns a method  retriever procedure for this interface-type.
      ;;The procedure will be stored in the run-time interface-type type-descriptor.
      ;;
      ;;NOTE  the   METHODS-TABLE  holds   an  entry  for   every  method   in:  this
      ;;interface-type,   this    interface-type's   parent,    this   interface-type
      ;;grand-parent's, et cetera.  So the method retriever procedure does *not* call
      ;;the parent's method retriever when a method is not found.
      ;;
      #`(lambda (method-name)
	  (case method-name
	    #,@(map (lambda ({_ <top>} entry)
		      (with-syntax
			  ((METHOD-NAME	(datum->syntax (.type-name this) (car entry)))
			   (PROCNAME	(cdr entry)))
			#'((METHOD-NAME) PROCNAME)))
		 (.methods-table this))
	    (else #f))))

    ;; ------------------------------------------------------------

    (method (finalise)
      ;;After  all   the  clauses  have  been   parsed,  we  need  to   perform  some
      ;;post-processing finalisation.  This method does it.
      ;;

      ;;If there is no UID: generate one.
      (.uid this (or (.uid this) (%build-generative-uid (.type-name this))))

      ;;Build  a value  for the  field METHOD-PROTOTYPES-TABLE  putting together  the
      ;;parent's table with the method prototypes from this type.
      (let* ((raw-method-prototypes-table
	      (let ((parent-method-prototypes-table
		     (if (.parent-ots this)
			 (let (({parent.ots <interface-type-spec>} (.parent-ots this)))
			   (.method-prototypes-table parent.ots))
		       '()))
		    (this-method-prototypes-table
		     (fold-left (lambda ({_ <top>} table {proto <method-prototype>})
				  ;; ((method-name . (closure-ots . default-procname-id)) ...)
				  (acons (.name-sym proto) (cons (.signature proto) (.default-procname-id proto))
					 table))
		       '() (.parsed-prototypes this))))
		(append this-method-prototypes-table parent-method-prototypes-table)))
	     (group*
	      ;;GROUP*  is a  list of  alists.  Every  alist is  composed by  entries
	      ;;having the same method name.
	      (if (pair? raw-method-prototypes-table)
		  (begin
		    (let recur ((group-head (car raw-method-prototypes-table))
				(rest       (cdr raw-method-prototypes-table)))
		      (receive (group-tail rest)
			  (partition (lambda (rest-entry)
				       (eq? (car group-head) (car rest-entry)))
			    rest)
			(cons (cons group-head group-tail)
			      (if (pair? rest)
				  (recur (car rest) (cdr rest))
				'())))))
		'())))
	(.method-prototypes-table this
	  ;;Here we compress every alist in GROUP* into a single entry.
	  ;;
	  ;;It is forbidden to method prototypes with the same name to have a default
	  ;;implementation.
	  (map (lambda ({_ <top>} group)
		 (if (list-of-single-item? group)
		     (let ((entry (car group)))
		       ;; (method-name . (closure-ots . has-default?))
		       (cons (car entry) (cons (cadr entry) (if (cddr entry) #t #f))))
		   (fold-left (lambda ({_ <top>} entry1 entry2)
				(when (or (cddr entry1) (cddr entry2))
				  (synner "cannot extend method with default implementation" (car entry2)))
				;; (method-name . (closure-ots . has-default?))
				(cons (car entry1)
				      (cons (xp::closure-type-spec.join (cadr entry1) (cadr entry2))
					    #f)))
		     (car group)
		     (cdr group))))
	    group*))

	;;Build the  methods table.  An  alist having: as keys,  symbols representing
	;;method  names;  as  values,  syntactic  identifiers  bound  to  the  method
	;;implementation functions.
	(.methods-table this
	  (map (lambda ({_ <top>} entry)
		 (let ((method-name.sym (car entry)))
		   (cons method-name.sym (xp::identifier-method-procname (.type-name this) method-name.sym))))
	    (.method-prototypes-table this)))

	;;For every method prototype we create the dynamic dispatch function.
	(for-each (lambda (entry)
		    (with-syntax
			((UID			(.uid this))
			 (METHOD-NAME		(datum->syntax (.type-name this) (car entry)))
			 (PROCNAME		(cdr entry))
			 (DEFAULT-PROCNAME	(exists (lambda ({spec <method-prototype>})
							  (if (eq? (car entry) (.name-sym spec))
							      (.default-procname-id spec)
							    #f))
						  (.parsed-prototypes this))))
		      (.push-definition! this
			#`(define/std (PROCNAME subject . args)
			    (td::interface-method-call-late-binding (quote UID) (quote METHOD-NAME) DEFAULT-PROCNAME subject args)))))
	  (.methods-table this)))

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
	((TYPE-NAME				(.type-name				results))
	 (UID					(.uid					results))
	 (PARENT-OTS				(.parent-ots				results))
	 (METHOD-PROTOTYPES-TABLE		(.method-prototypes-table-stx		results))
	 (IMPLEMENTED-INTERFACES		(.implemented-interfaces-ots		results))
	 (METHODS-TABLE				(.methods-table-stx			results))
	 ((DEFINITION ...)			(.definitions				results))
	 ((INTERFACE-VALIDATION-FORM ...)	(.interface-validation-forms		results))
	 ;;
	 (TYPE-DESCRIPTOR-ID			(.type-descriptor-id			results))
	 (PARENT-TYPE-DESCRIPTOR-ID		(.parent-type-descriptor-id		results))
	 (IMPLEMENTED-INTERFACE-UIDS		(.implemented-interface-uids-stx	results))
	 (METHOD-PROTOTYPE-NAMES		(.method-prototype-names-stx		results))
	 (METHOD-RETRIEVER			(.method-retriever-stx			results)))
      #'(module (TYPE-NAME)
	  (define/typed {TYPE-DESCRIPTOR-ID td::<interface-type-descr>}
	    (td::make-interface-type-descr (quote TYPE-NAME) (quote UID) PARENT-TYPE-DESCRIPTOR-ID
					   IMPLEMENTED-INTERFACE-UIDS METHOD-PROTOTYPE-NAMES METHOD-RETRIEVER))
	  (define-type TYPE-NAME
	    (constructor
		(sys::make-interface-type-spec (syntax TYPE-NAME) (quote UID) (syntax TYPE-DESCRIPTOR-ID) (quote PARENT-OTS)
					       METHOD-PROTOTYPES-TABLE METHODS-TABLE IMPLEMENTED-INTERFACES)))
	  DEFINITION ...
	  ;;We want this validation code after the definitions.
	  INTERFACE-VALIDATION-FORM ...)))

;;; --------------------------------------------------------------------

  (define-type <parsed-args>
    (vector-of (vector-of <top>)))

  (define (combine {results <parsing-results>} {clause-spec <syntax-clause-spec>} {args <parsed-args>})
    ((case-identifiers (.keyword clause-spec)
       ((method)		%process-clause/method)
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
						     (xp::make-type-specification parent.id)))))
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
			 (lambda ({_ <top>} arg knil)
			   (vector-fold-right
			       (lambda ({_ <top>} iface.id knil)
				 (unless (identifier? iface.id)
				   (synner "expected identifier as argument of IMPLEMENTS clause" iface.id))
				 (cons iface.id knil))
			     knil arg))
		       '() args)))
      (cond ((duplicate-identifiers? iface*.id)
	     => (lambda (id)
		  (synner "implemented interface declared multiple times" id)))
	    (else
	     (let ((iface*.ots (map (lambda ({_ <top>} iface.id)
				      (let (({iface.ots xp::<object-type-spec>}
					     (with-exception-handler
						 (lambda (E)
						   (let* ((msg "error dereferencing implemented interface name")
							  (msg (if (message-condition? E)
								   (string-append msg ": " (condition-message E))
								 msg)))
						     (synner msg iface.id)))
					       (lambda ()
						 (xp::make-type-specification iface.id)))))
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
      (vector-fold-left (lambda ({_ <top>} results arg)
			  (%process-method-prototype-spec results arg synner))
	results args))

    (define ({%process-method-prototype-spec <top>} {results <parsing-results>} arg synner)
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
	 (let* ((signature.stx		(%add-bottom-arguments #'?signature synner))
		(signature.ots		(xp::make-type-specification signature.stx)))
	   (.push-required-prototype! results (new <method-prototype> #'?method-name signature.ots #f))
	   results))

	(#(?stuff ...)
	 (synner "invalid METHOD-PROTOTYPE specification" #'(method-prototype ?stuff ...)))))

    (define (%add-bottom-arguments signature.stx synner)
      (syntax-case signature.stx (case-lambda lambda =>)
	((lambda ?formals => ?retvals)
	 #'(lambda (<bottom> . ?formals) => ?retvals))

	((case-lambda ?clause-signature0 ?clause-signature ...)
	 #`(case-lambda
	     #,@(map (lambda ({_ <top>} clause.stx)
		       (syntax-case clause.stx (=>)
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
      (vector-fold-left (lambda ({_ <top>} results arg)
			  (%process-method-spec results arg synner))
	results args))

    (define ({%process-method-spec <top>} {results <parsing-results>} arg synner)
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
	 (receive (method-name.id default-procname.id default-method-who.stx underscore-who.stx)
	     (%parse-method-who results #'?who synner)
	   (.push-definition! results #`(define/overload (#,default-method-who.stx {subject #,(.type-name results)} . ?formals)
					  ;;We really  have to  use the  fluid syntax
					  ;;THIS, otherwise  we will  not be  able to
					  ;;process  correctly  the clauses  imported
					  ;;from mixins.
					  (fluid-let-syntax ((this (make-synonym-transformer #'subject)))
					    ?body0 ?body ...)))
	   (.push-required-prototype! results (new <method-prototype> method-name.id
						   (make-closure-type-spec/from-typed-formals
						    #`(#,underscore-who.stx {subject <bottom>} . ?formals))
						   default-procname.id))
	   results))
	(#(?stuff ...)
	 (synner "invalid METHOD specification" #'(method ?stuff ...)))))

    (define (%parse-method-who {results <parsing-results>} who.stx synner)
      (syntax-case who.stx (brace)
	(?method-name
	 (identifier? #'?method-name)
	 (let ((default-procname.id (%make-default-procname-id results #'?method-name)))
	   (values #'?method-name default-procname.id default-procname.id #'(brace _ . <list>))))
	((brace ?method-name . ?rv-types)
	 (identifier? #'?method-name)
	 (let ((default-procname.id (%make-default-procname-id results #'?method-name)))
	   (values #'?method-name default-procname.id
		   #`(brace #,default-procname.id . ?rv-types)
		   #'(brace _                     . ?rv-types))))
	(_
	 (synner "invalid method name specification" who.stx))))

    (define (%make-default-procname-id {results <parsing-results>} method-name.id)
      (datum->syntax (.type-name results)
		     (gensym
		      (string-append (symbol->string (syntax->datum (.type-name results)))
				     "-"
				     (symbol->string (syntax->datum method-name.id))
				     "-default"))))

    #| end of module: %PROCESS-CLAUSE/METHOD-PROTOTYPE |# )

;;; --------------------------------------------------------------------

  (define ({%build-nongenerative-uid xp::<syntactic-identifier>} type-name.id)
    ;;Build and  return a symbol to  be used as  UID for this interface-type  for the
    ;;case of: non-generative type.
    ;;
    (datum->syntax type-name.id
		   (string->symbol (string-append "vicare:nongenerative:"
						  (symbol->string (syntax->datum type-name.id))))))

  (define ({%build-generative-uid xp::<syntactic-identifier>} type-name.id)
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
