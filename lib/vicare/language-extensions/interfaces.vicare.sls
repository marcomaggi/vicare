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
  (export define-interface
	  method case-method method/overload
	  method-prototype implements import
	  this)
  (import (vicare)
    (prefix (vicare expander)			xp::)
    (prefix (vicare system type-descriptors)	td::)
    (for (vicare expander) expand))

  (define-auxiliary-syntaxes method-prototype)


;;;; run-time utilities: interface implementation registration

(define (record-type-descriptor.register-interface rtd interface.uid iface-methods-table)
  ;;Register in  the record-type descriptor RTD  the methods table for  the interface
  ;;whose  UID is  INTERFACE.UID.  IFACE-METHODS-TABLE  is an  alist having:  as keys
  ;;symbols  representing  the method  names;  as  values the  method  implementation
  ;;procedures.  Return unspecified values.
  ;;
  (putprop (record-type-uid rtd) interface.uid iface-methods-table))

(define (core-type-descr.register-interface ctd interface.uid iface-methods-table)
  ;;Register in  the core-type  descriptor RTD  the methods  table for  the interface
  ;;whose  UID is  INTERFACE.UID.  IFACE-METHODS-TABLE  is an  alist having:  as keys
  ;;symbols  representing  the method  names;  as  values the  method  implementation
  ;;procedures.  Return unspecified values.
  ;;
  (putprop (td::core-type-descriptor.uid ctd) interface.uid iface-methods-table))


;;;; run-time utilities: run-time interface method application

(module (interface-method-call)

  (define* (interface-method-call interface.uid method-name.sym subject operands)
    ;;Implement run-time dynamic dispatching of method calls to interface types.
    ;;
    ;;The argument INTERFACE.UID must be the UID of the interface-type.  The argument
    ;;METHOD-NAME.SYM must  be a  symbol representing  the name  of the  method.  The
    ;;argument SUBJECT must be the subject of the method call, the value that will be
    ;;bound to THIS.  The argument OPERANDS must  be a list of additional operands to
    ;;be appended to the call after SUBJECT.
    ;;
    (let ((des (td::type-descriptor-of subject)))
      (cond ((record-type-descriptor? des)
	     (%rtd-method-call des interface.uid method-name.sym subject operands))

	    ((td::core-type-descriptor? des)
	     (%ctd-method-call des interface.uid method-name.sym subject operands))

	    (else
	     (%error "the subject is an object whose type does not implement interfaces" interface.uid subject)))))

  (define (%rtd-method-call rtd interface.uid method-name.sym subject operands)
    (cond ((getprop (record-type-uid rtd) interface.uid)
	   => (lambda (iface-methods-table)
		;;IFACE-METHODS-TABLE   is  an   alist   having:   as  keys   symbols
		;;representing the method names;  as values the method implementation
		;;procedures.
		(cond ((assq method-name.sym iface-methods-table)
		       => (lambda (entry)
			    ;;Method found.  Apply it to  the operands and return the
			    ;;application's return values.
			    (apply (cdr entry) subject operands)))
		      (else
		       (%error "the subject's record-type descriptor does not implement the requested interface method"
			       interface.uid method-name.sym rtd)))))
	  (else
	   (%error "the subject's record-type descriptor does not implement the requested interface"
		   interface.uid subject rtd))))

  (define (%ctd-method-call ctd interface.uid method-name.sym subject operands)
    (cond ((getprop (td::core-type-descriptor.uid ctd) interface.uid)
	   => (lambda (iface-methods-table)
		;;IFACE-METHODS-TABLE   is  an   alist   having:   as  keys   symbols
		;;representing the method names;  as values the method implementation
		;;procedures.
		(cond ((assq method-name.sym iface-methods-table)
		       => (lambda (entry)
			    ;;Method found.  Apply it to  the operands and return the
			    ;;application's return values.
			    (apply (cdr entry) subject operands)))
		      (else
		       (%error "the subject's core-type descriptor does not implement the requested interface method"
			       interface.uid method-name.sym ctd)))))
	  (else
	   (%error "the subject's core-type descriptor does not implement the requested interface"
		   interface.uid subject ctd))))

  (define (%error message . irritants)
    (apply assertion-violation 'interface-method-call message irritants))

  #| end of module: INTERFACE-METHOD-CALL |# )


(define-syntax define-interface
  (internal-body
    (define-constant __module_who__ 'define-interface)

    (define-constant CLAUSE-SPEC*
      (syntax-clauses-validate-specs
       (list
	;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
	(new <syntax-clause-spec> #'nongenerative		0 1      0 1      '() '())
	(new <syntax-clause-spec> #'implements			0 +inf.0 0 +inf.0 '() '())
	(new <syntax-clause-spec> #'method-prototype		0 +inf.0 2 2      '() '())
	(new <syntax-clause-spec> #'method			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'case-method			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'method/overload		0 +inf.0 2 +inf.0 '() '())
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
	(mutable	uid)
		;Unique identifier associated to this type.
	(mutable	implemented-interfaces)
		;A  possibly   empty  list  of  syntactic   identifiers  representing
		;interface implemented by this interface-type.
	(mutable	definitions)
		;Proper  list of  syntax objects  representing definition  forms that
		;must go in the output of this macro.
	(mutable	required-prototypes)
		;A   list   of   "<method-prototype>"  instances   representing   the
		;required-method specifications.  This list  holds an object for each
		;method that must  be implemented by the  object-types that implement
		;this interface-type.
	(mutable	default-prototypes)
		;A   list   of   "<method-prototype>"  instances   representing   the
		;default-method specifications.   This list holds an  object for each
		;default method implemented by this interface-type.
	#| end of FIELDS |# )

      (protocol
	(lambda (make-record)
	  (lambda (type-name.id)
	    (make-record type-name.id
			 #f  ;uid
			 '() ;implemented-interfaces
			 '() ;definitions
			 '() ;required-prototypes
			 '() ;default-prototypes
			 ))))

      (constructor-signature
	(lambda (xp::<syntactic-identifier>) => (<parsing-results>)))

      (method (push-definition! definition.stx)
	(.definitions this (cons definition.stx (.definitions this))))

      (method (type-descriptor-id)
	;;Return the  syntactic identifier that  must be  bound to the  run-time type
	;;descriptor for this interface type.
	;;
	(identifier-suffix (.type-name this) "-type-descriptor"))

      ;; ------------------------------------------------------------

      (method (implemented-interfaces-append! {iface*.id (list-of xp::<syntactic-identifier>)})
	(.implemented-interfaces this (append iface*.id (.implemented-interfaces this))))

      (method (implemented-interfaces-stx)
	;;Return  a syntax  object  representing an  expression  which, expanded  and
	;;evaluated,  returns a  proper list  of syntactic  identifiers bound  to the
	;;implemented interface's names.
	;;
	(if (null? (.implemented-interfaces this))
	    #'(quote ())
	  #`(list #,@(map (lambda (id)
			    #`(syntax #,id))
		       (.implemented-interfaces this)))))

      (method (interfaces-validation-forms)
	;;Return a syntax object representing  a (possibly empty) list of expressions
	;;which,   expanded  and   evaluated,   validates   this  interface-type   as
	;;implementing the declared interfaces.
	;;
	(cond ((null? (.implemented-interfaces this))
	       #'())
	      ((list-of-single-item? (.implemented-interfaces this))
	       (with-syntax
		   ((TYPE-NAME	(.type-name this))
		    (IFACE-NAME	(car (.implemented-interfaces this))))
		 #`((begin-for-syntax
		      (xp::build-table-for-interface-and-compliant-object-type (syntax IFACE-NAME) (syntax TYPE-NAME))))))
	      (else
	       (with-syntax
		   ((TYPE-NAME			(.type-name this))
		    (IMPLEMENTED-INTERFACES-STX	(.implemented-interfaces-stx this)))
		 #`((begin-for-syntax
		      (for-each (lambda (iface.id)
				  (xp::build-table-for-interface-and-compliant-object-type iface.id (syntax TYPE-NAME)))
			IMPLEMENTED-INTERFACES-STX)))))))

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
	(.required-prototypes this (cons proto (.required-prototypes this))))

      (method (interface-methods-table)
	;;Return  a syntax  object  representing an  expression  which, expanded  and
	;;evaluated, returns  an alist  having: as  keys symbols  representing method
	;;names;  as  values syntactic  identifiers  bound  to the  interface  method
	;;implementation functions.
	;;
	;;This  alist  becomes  the METHODS-TABLE  field  in  "<interface-type-spec>"
	;;instances.  So that, with the definitions:
	;;
	;;   (define-interface <Arith>
	;;     (nongenerative interface:<Arith>)
	;;     (method-prototype add (lambda () => (<number>))))
	;;
	;;   (define (fun {O <Arith>})
	;;     (.add O))
	;;
	;;the following implementation procedure is generated:
	;;
	;;   (define/std (<Arith>-add subject . args)
	;;     (interface-method-call 'interface:<Arith> 'add subject args))
	;;
	;;and the method call "(.add O)" is expanded to:
	;;
	;;   (<Arith>-add O)
	;;
	#`(list #,@(map (lambda ({proto <method-prototype>})
			  #`(cons (quote #,(.name proto)) (syntax #,(.procname proto))))
		     (.required-prototypes this))))

      (method (methods-name/signature-table)
	;;Return  a syntax  object  representing an  expression  which, expanded  and
	;;evaluated,  returns   an  alist  having:  as   keys,  symbols  representing
	;;required-method's  names;  as  values, instances  of  "<closure-type-spec>"
	;;representing required-method's signatures.
	;;
	;;This   alist  is   used   to:  verify   if   object-types  implement   this
	;;interface-type;   verify   if    this   interface-type   implements   other
	;;interface-types.
	;;
	#`(quote #,(map (lambda ({proto <method-prototype>})
			  #`(#,(.name proto) . #,(.signature proto)))
		     (.required-prototypes this))))

      ;; ------------------------------------------------------------

      ;; (method (method-retriever)
      ;; 	;;Return  a syntax  object  representing an  expression  which, expanded  and
      ;; 	;;evaluated, returns the method retriever function.
      ;; 	;;
      ;; 	#`(lambda (method-name.sym)
      ;; 	    (case method-name.sym
      ;; 	      #,@(map (lambda (entry)
      ;; 			(let ((method-name.id		(car entry))
      ;; 			      (method-procname.id	(cdr entry)))
      ;; 			  #`((#,method-name.id) #,method-procname.id)))
      ;; 		   (.methods-table this))
      ;; 	      (else #f))))

      ;; ------------------------------------------------------------


      ;; (method (push-method-prototype-name!   {name xp::<syntactic-identifier>})
      ;; 	(.method-prototype-names   this (cons name (.method-prototype-names   this))))

      ;; (method (push-implemented-method-name! {name xp::<syntactic-identifier>})
      ;; 	(.implemented-method-names this (cons name (.implemented-method-names this))))

      ;; ------------------------------------------------------------

      (method (finalise synner)
	;;After  all  the  clauses  have  been   parsed,  we  need  to  perform  some
	;;post-processing finalisation.  This method does it.
	;;

	;;If there is no UID: generate one.
	(.uid this (or (.uid this) (%build-generative-uid (.type-name this))))

	;;For  every  required-method  prototype   we  create  the  dynamic  dispatch
	;;function.  Also check for duplicate method names.
	(fold-left (lambda (method-name*.id {proto <method-prototype>})
		     (cond ((identifier-memq (.name proto) method-name*.id)
			    => (lambda (sublist)
				 (synner "duplicate method name" (.name proto)))))
		     (.push-definition! this
		       #`(define/std (#,(.procname proto) subject . args)
			   (interface-method-call (quote #,(.uid this)) (quote #,(.name proto)) subject args)))
		     (cons (.name proto) method-name*.id))
	  '() (.required-prototypes this)))

      #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

    (define (main input-form.stx synner)
      (syntax-case input-form.stx ()
	((_ ?type-name . ?clauses)
	 (begin
	   (unless (identifier? #'?type-name)
	     (synner "expected identifier as interface type name" #'?type-name))
	   ;; (receive-and-return (out)
	   ;;     (%build-output (%parse-clauses #'?type-name #'?clauses synner) synner)
	   ;;   (debug-print (syntax->datum out)))
	   (%build-output (%parse-clauses #'?type-name #'?clauses synner)
			  synner)))

	(_
	 (synner "invalid DEFINE-INTERFACE syntax use"))))

    (define ({%parse-clauses <parsing-results>} {type-name.id xp::<syntactic-identifier>} clauses.stx synner)
      (syntax-clauses-fold-specs
	  (lambda ({results <parsing-results>} {clause-spec <syntax-clause-spec>} args)
	    (combine results clause-spec args synner))
	(new <parsing-results> type-name.id)
	CLAUSE-SPEC*
	(syntax-clauses-unwrap clauses.stx synner)
	synner))

    (define (%build-output {results <parsing-results>} synner)
      (.finalise results synner)
      (with-syntax
	  ((TYPE-NAME				(.type-name			results))
	   (UID					(.uid				results))
	   (TYPE-DESCRIPTOR			(.type-descriptor-id		results))
	   (METHODS-NAME/SIGNATURE-TABLE	(.methods-name/signature-table	results))
	   (INTERFACE-METHODS-TABLE		(.interface-methods-table	results))
	   ;;(METHOD-PROTOTYPE-NAMES		(.method-prototype-names	results))
	   ;;(IMPLEMENTED-METHOD-NAMES		(.implemented-method-names	results))
	   ;;(IMPLEMENTED-INTERFACE-UIDS		(.implemented-interface-uids	results))
	   ;;(METHOD-RETRIEVER			(.method-retriever		results))
	   ((DEFINITION ...)			(.definitions			results))
	   (IMPLEMENTED-INTERFACES-STX		(.implemented-interfaces-stx	results))
	   ((INTERFACE-VALIDATION-FORM ...)	(.interfaces-validation-forms	results)))
	#'(module (TYPE-NAME)
	    ;; (define/typed {TYPE-DESCRIPTOR td::<interface-type-descr>}
	    ;;   (td::make-interface-type-descr (quote TYPE-NAME) (quote UID)
	    ;; 				     (quote METHOD-PROTOTYPE-NAMES) (quote IMPLEMENTED-METHOD-NAMES)
	    ;; 				     IMPLEMENTED-INTERFACE-UIDS METHOD-RETRIEVER))
	    (define-syntax TYPE-NAME
	      (xp::make-interface-type-spec (syntax TYPE-NAME) (quote UID) (syntax TYPE-DESCRIPTOR)
					    METHODS-NAME/SIGNATURE-TABLE INTERFACE-METHODS-TABLE
					    IMPLEMENTED-INTERFACES-STX))
	    DEFINITION ...
	    ;;We want this validation code after the definitions.
	    INTERFACE-VALIDATION-FORM ...)))

;;; --------------------------------------------------------------------

    (define (combine {results <parsing-results>} {clause-spec <syntax-clause-spec>} args synner)
      ((case-identifiers (.keyword clause-spec)
	 ((method)			%process-clause/method)
	 ((case-method)			%process-clause/case-method)
	 ((method/overload)		%process-clause/method-overload)
	 ((method-prototype)		%process-clause/method-prototype)
	 ((nongenerative)		%process-clause/nongenerative)
	 ((implements)			%process-clause/implements)
	 (else
	  (assertion-violation __module_who__ "invalid clause spec" clause-spec)))
       results args synner))

;;; --------------------------------------------------------------------

    (define (%process-clause/nongenerative {results <parsing-results>} args synner)
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

    (define (%process-clause/implements {results <parsing-results>} args synner)
      ;;This  input clause  can  appear multiple  times, each  clause  must have  the
      ;;format:
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
	       (.implemented-interfaces-append! results iface*.id)
	       results))))

;;; --------------------------------------------------------------------

    (module (%process-clause/method-prototype)

      (define (%process-clause/method-prototype {results <parsing-results>} args synner)
	;;This clause can be present multiple times.  Each input clause must have the
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
	(syntax-case arg ()
	  (#(?method-name ?signature)
	   (identifier? #'?method-name)
	   (let* ((signature.stx	(%add-bottom-arguments #'?signature synner))
		  (signature.ots	(xp::type-annotation->object-type-spec signature.stx))
		  (method-procname.id	(identifier-method-procname (.type-name results) #'?method-name)))
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

      (define (%process-clause/method {results <parsing-results>} args synner)
	;;This clause can be present multiple times.  Each input clause must have the
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
	;;The METHOD  clause can be present  multiple times.  Each input  clause must
	;;have the format:
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

      (define (%process-clause/case-method {results <parsing-results>} args synner)
	;;This clause can be present multiple times.  Each input clause must have the
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

      (define (%process-clause/method-overload {results <parsing-results>} args synner)
	;;This clause can be present multiple times.  Each input clause must have the
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
      ;;Build and return a  symbol to be used as UID for  this interface-type for the
      ;;case of: non-generative type.
      ;;
      (datum->syntax type-name.id
		     (string->symbol (string-append "vicare:nongenerative:"
						    (symbol->string (syntax->datum type-name.id))))))

    (define (%build-generative-uid type-name.id)
      ;;Build and return a  symbol to be used as UID for  this interface-type for the
      ;;case of: generative type.
      ;;
      (datum->syntax type-name.id
		     (gensym (syntax->datum type-name.id))))

;;; --------------------------------------------------------------------

    (lambda (input-form.stx)
      (case-define synner
	((message)
	 (syntax-violation __module_who__ message input-form.stx #f))
	((message subform)
	 (syntax-violation __module_who__ message input-form.stx subform)))
      (main input-form.stx synner))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put '.push-definition!	'scheme-indent-function 1)
;; End:
