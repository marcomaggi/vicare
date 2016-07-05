;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of DEFINE-LABEL
;;;Date: Mon Apr 25, 2016
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
(library (vicare language-extensions labels (0 4 2016 6 1))
  (options typed-language)
  (export
    define-label
    parent
    constructor
    destructor
    type-predicate
    equality-predicate
    comparison-procedure
    hash-function
    method
    case-method
    method/overload
    nongenerative
    this)
  (import (vicare)
    (vicare language-extensions mixins (0 4))
    (for (vicare expander)
      expand))


(define-syntax define-label
  (internal-body
    (import (only (psyntax system $all)
		  make-label-type-spec)
      (prefix (only (vicare expander)
		    <syntactic-identifier>)
	      expander::))
    (define-constant __module_who__
      'define-label)

    (define-constant CLAUSE-SPEC*
      (syntax-clauses-validate-specs
       (list
	;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
	(new <syntax-clause-spec> #'parent			1 1      1 +inf.0 '() '())
	(new <syntax-clause-spec> #'constructor			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'destructor			0 1      2 +inf.0 '() '())
	(new <syntax-clause-spec> #'type-predicate		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'equality-predicate		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'comparison-procedure	0 1      1 1      '() '())
	(new <syntax-clause-spec> #'hash-function		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'method			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'case-method			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'method/overload		0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'nongenerative		0 1      1 1      '() '())
	#| end of LIST |# )))

    (define-record-type <parsing-results>
      (fields
	(immutable	type-name)
		;Identifier representing the label type name.
	(mutable	uid)
		;Unique identifier associated to this type.
	(mutable	parent)
		;After  parsing:  a  syntax   object  representing  the  parent  type
		;annotation.
	(mutable	constructor-clauses)
		;After parsing:  null or a list  of CASE-DEFINE clauses for  the type
		;constructor.
	(mutable	constructor-id)
		;After parsing and  finalising: false or the  syntactic identifier to
		;which the constructor function is bound.
	(mutable	destructor-clause)
		;After  parsing: false  or a  list representin  a single  CASE-DEFINE
		;clause for the type destructor.
	(mutable	destructor-id)
		;After parsing and  finalising: false or the  syntactic identifier to
		;which the destructor function is bound.
	(mutable	type-predicate-id)
		;After parsing: false  or the syntactic identifier to  which the type
		;predicate is bound.
	(mutable	equality-predicate-id)
		;After  parsing:  false or  the  syntactic  identifier to  which  the
		;equality predicate is bound.
	(mutable	comparison-procedure-id)
		;After  parsing:  false or  the  syntactic  identifier to  which  the
		;comparison procedure is bound.
	(mutable	hash-function-id)
		;After parsing: false  or the syntactic identifier to  which the hash
		;function is bound.
	(mutable	definitions)
		;Proper  list of  syntax objects  representing definition  forms that
		;must go in the output of this macro.
	(mutable	methods-table)
		;An  alist having:  as  keys syntactic  identifiers representing  the
		;method names;  as values syntactic  identifiers bound to  the method
		;implementation functions.
	#| end of FIELDS |# )

      (protocol
	(lambda (make-record)
	  (lambda (type-name)
	    (make-record type-name
			 #f	      ;uid
			 #f	      ;parent
			 '()	      ;constructor clauses
			 #f	      ;constructor-id
			 #f	      ;destructor clause
			 #f	      ;destructor-id
			 #f	      ;type predicate id
			 #f	      ;equality predicate id
			 #f	      ;comparison procedure id
			 #f	      ;hash function id
			 '()	      ;definitions
			 '()	      ;methods table
			 ))))

      (constructor-signature
	(lambda (expander::<syntactic-identifier>) => (<parsing-results>)))

      (method (definitions-push! definition.stx)
	(.definitions this (cons definition.stx (.definitions this))))

      (method (constructor-clauses-push! clause)
	(.constructor-clauses this (cons clause (.constructor-clauses this))))

      (method (methods-table-push! {method-name.id	expander::<syntactic-identifier>}
				   {method-procname.id	expander::<syntactic-identifier>})
	(.methods-table this `((,method-name.id . ,method-procname.id) . ,(.methods-table this))))

      (method (methods-table-alist)
	;;Return  a syntax  object  representing an  expression  which, expanded  and
	;;evaluated, returns  an alist  having: as  keys symbols  representing method
	;;names; as values  syntactic identifiers bound to  the method implementation
	;;functions.
	;;
	#`(list #,@(map (lambda (entry)
			  (let ((method-name.id		(car entry))
				(method-procname.id	(cdr entry)))
			    #`(cons (quote #,method-name.id) (syntax #,method-procname.id))))
		     (.methods-table this))))

      (method (finalise-definitions)
	;;After  all  the  clauses  have  been   parsed,  we  need  to  perform  some
	;;post-processing  to  generate  definition   forms.   For  example  for  the
	;;CONSTRUCTOR and DESTRUCTOR clauses.  This method does it.
	;;
	(.uid this (or (.uid this)
		       (datum->syntax (.type-name this) (gensym (syntax->datum (.type-name this))))))

	;;Constructor finalisation.
	(let ((clause*.stx (.constructor-clauses this)))
	  (if (null? clause*.stx)
	      (.constructor-id this #f)
	    (let ((clause*.stx (map (lambda (clause.stx)
				      (cons (cons #`(brace _ #,(.type-name this)) (car clause.stx))
					    (cdr clause.stx)))
				 (reverse clause*.stx))))
	      (with-syntax ((FUNC (identifier-method-procname (.type-name this) #'constructor)))
		(begin
		  (.constructor-id    this #'(syntax FUNC))
		  (.definitions-push! this #`(case-define/checked FUNC . #,clause*.stx)))))))

	;;Destructor finalisation.
	(cond ((.destructor-clause this)
	       => (lambda (clause.stx)
		    (with-syntax ((FUNC (identifier-method-procname (.type-name this) #'destructor)))
		      (.destructor-id     this #'(syntax FUNC))
		      (.definitions-push! this #`(case-define/checked FUNC #,clause.stx)))))
	      (else
	       (.destructor-id this #f))))

      #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

    (define (main stx synner)
      (syntax-case stx ()
	((_ ?type-name . ?clauses)
	 (begin
	   (unless (identifier? #'?type-name)
	     (synner "expected identifier as label type name" #'?type-name))
	   (%build-output (%parse-clauses (new <parsing-results> #'?type-name) #'?clauses synner))))

	(_
	 (synner "invalid DEFINE-LABEL syntax use"))))

    (define ({%parse-clauses <parsing-results>} {results <parsing-results>} clauses.stx synner)
      (let* ((clause*.stx	(syntax-clauses-unwrap clauses.stx synner))
	     (clause*.stx	(%merge-mixins-clauses results clause*.stx synner)))
	(syntax-clauses-fold-specs
	    (lambda ({results <parsing-results>} {clause-spec <syntax-clause-spec>} args)
	      (combine results clause-spec args synner))
	  results CLAUSE-SPEC* clause*.stx synner)))

;;; --------------------------------------------------------------------

    (define (%build-output {results <parsing-results>})
      (.finalise-definitions results)
      (with-syntax
	  ((TYPE-NAME			(.type-name			results))
	   (PARENT-STX			(.parent			results))
	   (UID				(.uid				results))
	   (CONSTRUCTOR-ID		(.constructor-id		results))
	   (DESTRUCTOR-ID		(.destructor-id			results))
	   (TYPE-PREDICATE-ID		(.type-predicate-id		results))
	   (EQUALITY-PREDICATE-ID	(.equality-predicate-id		results))
	   (COMPARISON-PROCEDURE-ID	(.comparison-procedure-id	results))
	   (HASH-FUNCTION-ID		(.hash-function-id		results))
	   (METHODS-TABLE-ALIST		(.methods-table-alist		results))
	   ((DEFINITION ...)		(.definitions			results)))
	#`(module (TYPE-NAME)
	    (define-syntax TYPE-NAME
	      (make-label-type-spec (syntax TYPE-NAME) (quote UID) (syntax PARENT-STX)
				    CONSTRUCTOR-ID DESTRUCTOR-ID TYPE-PREDICATE-ID
				    EQUALITY-PREDICATE-ID COMPARISON-PROCEDURE-ID HASH-FUNCTION-ID
				    METHODS-TABLE-ALIST))
	    DEFINITION ...)))

;;; --------------------------------------------------------------------

    (module (%merge-mixins-clauses)

      (define (%merge-mixins-clauses {results <parsing-results>} input-clause*.stx synner)
	;;The MIXINS clause can be present multiple times.  Each clause must have one
	;;of the formats:
	;;
	;;   (mixins ?mixin-name ...)
	;;
	;;NOTE  We  cannot  process  the MIXINS  clauses  with  the  SYNTAX-CLAUSES-*
	;;facilities because  such facilities change  the order in which  the clauses
	;;are  processed; instead,  the MIXINS  clauses  need to  be spliced  without
	;;changing the order.
	;;
	(fold-right
	    (lambda (input-clause.stx parsed-clause*.stx)
	      (syntax-case input-clause.stx (mixins)
		((mixins . ?mixins)
		 (%splice-mixins results input-clause.stx #'?mixins parsed-clause*.stx synner))
		(_
		 (cons input-clause.stx parsed-clause*.stx))))
	  '() input-clause*.stx))

      (define (%splice-mixins {results <parsing-results>} input-clause.stx
			      mixins.stx parsed-clause*.stx synner)
	(syntax-case mixins.stx ()
	  ((?mixin-name . ?mixins)
	   (if (identifier? #'?mixin-name)
	       (append (%splice-single-mixin results #'?mixin-name synner)
		       (%splice-mixins results input-clause.stx #'?mixins parsed-clause*.stx synner))
	     (synner "expected identifier as mixin name in MIXINS clause" input-clause.stx)))
	  (()
	   parsed-clause*.stx)
	  (_
	   (synner "invalid syntax in MIXINS clause" input-clause.stx))))

      (define (%splice-single-mixin {results <parsing-results>} mixin-name.id synner)
	(let ((obj (retrieve-expand-time-value mixin-name.id)))
	  (syntax-case obj (define-mixin)
	    ((define-mixin ?mixin-name . ?clauses)
	     (syntax-replace-id #'?clauses mixin-name.id (.type-name results)))
	    (_
	     (synner "identifier in MIXINS clause is not a mixin name" mixin-name.id)))))

      #| end of module: %MERGE-MIXINS-CLAUSES |# )

;;; --------------------------------------------------------------------

    (define ({combine <parsing-results>} {results <parsing-results>} {clause-spec <syntax-clause-spec>} args synner)
      ((case-identifiers (.keyword clause-spec)
	 ((method)			%process-clause/method)
	 ((case-method)			%process-clause/case-method)
	 ((method/overload)		%process-clause/method-overload)
	 ((parent)			%process-clause/parent)
	 ((constructor)			%process-clause/constructor)
	 ((destructor)			%process-clause/destructor)
	 ((type-predicate)		%process-clause/type-predicate)
	 ((equality-predicate)		%process-clause/equality-predicate)
	 ((comparison-procedure)	%process-clause/comparison-procedure)
	 ((hash-function)		%process-clause/hash-function)
	 ((nongenerative)		%process-clause/nongenerative)
	 (else
	  (assertion-violation __module_who__ "invalid clause spec" clause-spec)))
       results args synner)
      results)

;;; --------------------------------------------------------------------

    (define (%process-clause/parent {results <parsing-results>} args synner)
      ;;The input clause must have the format:
      ;;
      ;;   (parent ?parent-id)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?parent-id))
      ;;
      (.parent results (vector-ref (vector-ref args 0) 0)))

;;; --------------------------------------------------------------------

    (define (%process-clause/constructor {results <parsing-results>} args synner)
      ;;An input clause must have the format:
      ;;
      ;;   (constructor ?args ?body0 ?body ...)
      ;;
      ;;and there can be any number of them; we expect ARGS to have the format:
      ;;
      ;;   #(#(?args ?body0 ?body ...) ...)
      ;;
      (vector-for-each
	  (lambda (arg)
	    (syntax-case arg ()
	      (#(?args ?body0 ?body ...)
	       (.constructor-clauses-push! results #'(?args ?body0 ?body ...)))
	      (#(?stuff ...)
	       (synner "invalid constructor specification" #'(constructor ?stuff ...)))))
	args))

;;; --------------------------------------------------------------------

    (define (%process-clause/destructor {results <parsing-results>} args synner)
      ;;The input clause must have the format:
      ;;
      ;;   (destructor ?args ?body0 ?body ...)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?args ?body0 ?body ...))
      ;;
      (syntax-case (vector-ref args 0) ()
	(#(?args ?body0 ?body ...)
	 (.destructor-clause results #'(?args ?body0 ?body ...)))
	(#(?stuff ...)
	 (synner "invalid destructor specification" #'(destructor ?stuff ...)))))

;;; --------------------------------------------------------------------

    (define (%process-clause/type-predicate {results <parsing-results>} args synner)
      ;;The input clause must have the format:
      ;;
      ;;   (predicate ?predicate-stx)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?predicate-stx))
      ;;
      (let ((protocol-stx (vector-ref (vector-ref args 0) 0)))
	(with-syntax
	    ((FUNC (identifier-method-procname (.type-name results) #'type-predicate)))
	  (.type-predicate-id results #'(syntax FUNC))
	  (.definitions-push! results #`(define/typed {FUNC <type-predicate>}
					  (#,protocol-stx (is-a? _ #,(.parent results))))))))

;;; --------------------------------------------------------------------

    (define (%process-clause/equality-predicate {results <parsing-results>} args synner)
      ;;The input clause must have the format:
      ;;
      ;;   (equality-predicate ?func-stx)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?func-stx))
      ;;
      (let ((equality-predicate-protocol (vector-ref (vector-ref args 0) 0)))
	(with-syntax
	    ((FUNC (identifier-method-procname (.type-name results) #'equality-predicate)))
	  (.equality-predicate-id results #'(syntax FUNC))
	  (.definitions-push!     results #`(define/typed {FUNC (equality-predicate #,(.type-name results))}
					      (#,equality-predicate-protocol (equality-predicate #,(.parent results))))))))

;;; --------------------------------------------------------------------

    (define (%process-clause/comparison-procedure {results <parsing-results>} args synner)
      ;;The input clause must have the format:
      ;;
      ;;   (comparison-procedure ?func-stx)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?func-stx))
      ;;
      (let ((comparison-procedure-protocol (vector-ref (vector-ref args 0) 0)))
	(with-syntax
	    ((FUNC (identifier-method-procname (.type-name results) #'comparison-procedure)))
	  (.comparison-procedure-id results #'(syntax FUNC))
	  (.definitions-push!       results #`(define/typed {FUNC (comparison-procedure #,(.type-name results))}
						(#,comparison-procedure-protocol (comparison-procedure #,(.parent results))))))))

;;; --------------------------------------------------------------------

    (define (%process-clause/hash-function {results <parsing-results>} args synner)
      ;;The input clause must have the format:
      ;;
      ;;   (hash-function ?func-stx)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?func-stx))
      ;;
      (let ((hash-function-protocol (vector-ref (vector-ref args 0) 0)))
	(with-syntax
	    ((FUNC (identifier-method-procname (.type-name results) #'hash-function)))
	  (.hash-function-id  results #'(syntax FUNC))
	  (.definitions-push! results #`(define/typed {FUNC (hash-function #,(.type-name results))}
					  (#,hash-function-protocol (hash-function #,(.parent results))))))))

;;; --------------------------------------------------------------------

    (define (%process-clause/nongenerative {results <parsing-results>} args synner)
      ;;The input clause must have the format:
      ;;
      ;;   (nongenerative ?uid)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#(?uid))
      ;;
      (.uid results (vector-ref (vector-ref args 0) 0)))

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
	     (.definitions-push!   results #`(define/checked (#,method-who.stx {subject #,(.type-name results)} . ?formals)
					       (fluid-let-syntax ((this (identifier-syntax subject)))
						 ?body0 ?body ...)))
	     (.methods-table-push! results method-name.id method-procname.id)
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
	     (.definitions-push!	results #`(case-define/checked #,method-procname.id . #,clause*.stx))
	     (.methods-table-push!	results method-name.id method-procname.id)
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
	     (.definitions-push!   results #`(define/overload (#,method-who.stx {subject #,(.type-name results)} . ?formals)
					       (fluid-let-syntax ((this (identifier-syntax subject)))
						 ?body0 ?body ...)))
	     (.methods-table-push! results method-name.id method-procname.id)
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
