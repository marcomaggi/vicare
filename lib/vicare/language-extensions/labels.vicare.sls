;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of DEFINE-LABEL-TYPE
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
(library (vicare language-extensions labels (0 4 2016 7 19))
  (options typed-language)
  (export
    define-label-type
    parent
    constructor
    destructor
    type-predicate
    equality-predicate
    comparison-procedure
    hash-function
    method
    nongenerative
    this)
  (import (vicare)
    (vicare language-extensions mixins (0 4))
    (only (psyntax system $all)
	  typed-variable-with-private-access!))


(define-syntax (define-label-type input-form.stx)
  (import (only (psyntax system $all)
		make-label-type-spec)
    (prefix (vicare expander)
	    xp::))

  (define-constant __module_who__
    'define-label-type)


(define-constant CLAUSE-SPEC*
  (xp::syntax-clauses-validate-specs
   (list
    ;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
    (new xp::<syntax-clause-spec> #'parent			1 1      1 +inf.0 '() '())
    (new xp::<syntax-clause-spec> #'constructor			0 +inf.0 2 +inf.0 '() '())
    (new xp::<syntax-clause-spec> #'destructor			0 1      2 +inf.0 '() '())
    (new xp::<syntax-clause-spec> #'type-predicate		0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'equality-predicate		0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'comparison-procedure	0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'hash-function		0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'method			0 +inf.0 2 +inf.0 '() '())
    (new xp::<syntax-clause-spec> #'nongenerative		0 1      1 1      '() '())
    #| end of LIST |# )))


(define-record-type <method-spec>
  (fields
    (immutable	name-id)
		;A syntactic identifier representing the method name.
    (immutable	name-sym)
		;A symbol representing the method name.
    (immutable	procname)
		;The  syntactic  identifier   that  will  be  bound   to  the  method
		;implementation procedure.
    (immutable	implementation-meat)
		;A syntax object with one of the formats:
		;
		;   ((?method-procname               . ?formals) . ?body)
		;   (({?method-procname . ?rv-types} . ?formals) . ?body)
		;
		;that  can  be  turned   into  a  method's  implementation  procedure
		;definition just be prepending DEFINE/CHECKED or DEFINE/OVERLOAD.
    #| end of FIELDS |# )

  (constructor-signature
    (lambda (xp::<syntactic-identifier> xp::<syntactic-identifier> <top>) => (<method-spec>)))

  (protocol
    (lambda (make-record)
      (lambda (name procname meat)
	(make-record name (syntax->datum name) procname meat)))))


(define-record-type <parsing-results>
  (fields
    (immutable	type-name)
		;Syntactic identifier representing the label type name.
    (mutable	uid)
		;False or a  symbol representing the unique  identifier associated to
		;this type.
    (mutable	parent)
		;After  parsing:  a  syntax   object  representing  the  parent  type
		;annotation.
    (mutable	constructor-clauses)
		;After parsing:  null or a list  of CASE-DEFINE clauses for  the type
		;constructor.  Each item in the list has the format:
		;
		;   (?formals . ?body)
		;
    (mutable	constructor-id)
		;After parsing and  finalising: false or the  syntactic identifier to
		;which the constructor function is bound.
    (mutable	destructor-clause)
		;After parsing: false or a pair with format:
		;
		;   (?formals . ?body)
		;
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
    (mutable	method-specs)
		;Proper list of "<method-spec>" instances.
    (mutable	methods-table)
		;An  alist having:  as  keys syntactic  identifiers representing  the
		;method names;  as values syntactic  identifiers bound to  the method
		;implementation functions.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (lambda (type-name)
	(make-record type-name
		     #f	   ;uid
		     #f	   ;parent
		     '()   ;constructor clauses
		     #f	   ;constructor-id
		     #f	   ;destructor clause
		     #f	   ;destructor-id
		     #f	   ;type predicate id
		     #f	   ;equality predicate id
		     #f	   ;comparison procedure id
		     #f	   ;hash function id
		     '()   ;definitions
		     '()   ;method specs
		     '()   ;methods table
		     ))))

  (constructor-signature
    (lambda (xp::<syntactic-identifier>) => (<parsing-results>)))


;;;; <parting-results>: some methods

(method (push-definition! definition.stx)
  (.definitions this (cons definition.stx (.definitions this))))

(method (push-constructor-clause! clause)
  (.constructor-clauses this (cons clause (.constructor-clauses this))))


;;;; <parting-results>: some methods

(method (push-method-spec! {spec <method-spec>})
  (.method-specs this (cons spec (.method-specs this))))

(method (methods-table-push! {method-name.id		xp::<syntactic-identifier>}
			     {method-procname.id	xp::<syntactic-identifier>})
  (.methods-table this `((,method-name.id . ,method-procname.id) . ,(.methods-table this))))

(method (methods-table-alist)
  ;;Return  a  syntax  object  representing an  expression  which,  expanded  and
  ;;evaluated,  returns an  alist  having: as  keys  symbols representing  method
  ;;names; as  values syntactic  identifiers bound  to the  method implementation
  ;;functions.
  ;;
  #`(list #,@(map (lambda (entry)
		    (let ((method-name.id	(car entry))
			  (method-procname.id	(cdr entry)))
		      #`(cons (quote #,method-name.id) (syntax #,method-procname.id))))
	       (.methods-table this))))


;;;; <parting-results>: finalisation method

(method (finalise)
  ;;After all the  clauses have been parsed, we need  to perform some post-processing
  ;;to generate  definition forms.   For example for  the CONSTRUCTOR  and DESTRUCTOR
  ;;clauses.  This method does it.  Return unspecified values.
  ;;

  ;;If no UID was selected: generate one.
  (unless (.uid this)
    (.uid this (datum->syntax (.type-name this) (gensym (syntax->datum (.type-name this))))))

  ;;Constructor  finalisation.  Each  item in  the list  CONSTRUCTOR-CLAUSES has  the
  ;;format:
  ;;
  ;;   (?formals . ?body)
  ;;
  ;;For each clause we compose an overloaded function definition as follows:
  ;;
  ;;   (define/overload ((<type>-constructor <type>) . ?formals)
  ;;     . ?body)
  ;;
  (let ((clause*.stx (.constructor-clauses this)))
    (if (null? clause*.stx)
	(.constructor-id this #f)
      (with-syntax ((FUNC (xp::identifier-method-procname (.type-name this) #'constructor)))
	(.constructor-id this #'(syntax FUNC))
	(for-each (lambda (clause.stx)
		    (let ((formals.stx	(cons #`(brace FUNC #,(.type-name this)) (car clause.stx)))
			  (body*.stx	(cdr clause.stx)))
		      (.push-definition! this #`(define/overload #,formals.stx . #,body*.stx))))
	  clause*.stx))))

  ;;Destructor finalisation.
  (cond ((.destructor-clause this)
	 => (lambda (clause.stx)
	      ;;CLAUSE.STX is pair with format:
	      ;;
	      ;;   (?formals . ?body)
	      ;;
	      (with-syntax ((FUNC (xp::identifier-method-procname (.type-name this) #'destructor)))
		(.destructor-id    this #'(syntax FUNC))
		(.push-definition! this #`(define/checked (FUNC . #,(car clause.stx)) . #,(cdr clause.stx))))))
	(else
	 (.destructor-id this #f)))

  (internal-body
    (define ({%compress-method-specs (list-of (nelist-of <method-spec>))} {spec* (list-of <method-spec>)})
      ;;Given  a  non-empty list  of  "<method-spec>"  instances: partition  it  into
      ;;sublists having method  specs with the same method name.   Return the list of
      ;;sublists.
      ;;
      (if (pair? spec*)
	  (let recur ((head (car spec*))
		      (tail (cdr spec*)))
	    (receive (group rest)
		(partition (lambda ({spec <method-spec>})
			     (eq? (.name-sym head) (.name-sym spec)))
		  tail)
	      (cons (cons head group)
		    (if (pair? rest)
			(recur (car rest) (cdr rest))
		      '()))))
	'()))

    (define {method-group* (list-of (nelist-of <method-spec>))}
      ;;Every  METHOD-GROUP  is  a  non-empty  list.  All  the  items  in  GROUP  are
      ;;"<method-spec>" instances having the same method name.
      (%compress-method-specs (.method-specs this)))

    ;;For each  method: add  an entry  to the methods  table; compose  the definition
    ;;forms for the implementation functions.
    (for-each (lambda ({group (nelist-of <method-spec>)})
		(cond ((list-of-single-item? group)
		       (let (({single <method-spec>} (car group)))
			 (.methods-table-push! this (.name-id single) (.procname single))
			 (.push-definition!    this (cons #'define/checked (.implementation-meat single)))))
		      (else
		       ;;For the whole  group: we push only one entry  on the methods
		       ;;table.
		       (.methods-table-push! this (.name-id (car group)) (.procname (car group)))
		       (for-each (lambda ({spec <method-spec>})
				   (.push-definition! this (cons #'define/overload (.implementation-meat spec))))
			 group))))
      method-group*)

    #| end of INTERNAL-BODY |# )

  #| end of FINALISE |# )

#| end of DEFINE-RECORD-TYPE |# )


(define (main stx)
  (syntax-case stx ()
    ((_ ?type-name . ?clauses)
     (begin
       (unless (identifier? #'?type-name)
	 (synner "expected identifier as label type name" #'?type-name))
       (%build-output (%parse-clauses (new <parsing-results> #'?type-name) #'?clauses))))

    (_
     (synner "invalid DEFINE-LABEL-TYPE syntax use"))))

(define ({%parse-clauses <parsing-results>} {results <parsing-results>} clauses.stx)
  (let* ((clause*.stx	(xp::syntax-clauses-unwrap clauses.stx synner))
	 (clause*.stx	(%merge-mixins-clauses results clause*.stx)))
    (xp::syntax-clauses-fold-specs combine results CLAUSE-SPEC* clause*.stx synner)))

(define (%build-output {results <parsing-results>})
  (.finalise results)
  (with-syntax
      ((TYPE-NAME			(.type-name			results))
       (PARENT-STX			(.parent			results))
       (UID				(.uid				results))
       (CONSTRUCTOR-ID			(.constructor-id		results))
       (DESTRUCTOR-ID			(.destructor-id			results))
       (TYPE-PREDICATE-ID		(.type-predicate-id		results))
       (EQUALITY-PREDICATE-ID		(.equality-predicate-id		results))
       (COMPARISON-PROCEDURE-ID		(.comparison-procedure-id	results))
       (HASH-FUNCTION-ID		(.hash-function-id		results))
       (METHODS-TABLE-ALIST		(.methods-table-alist		results))
       ((DEFINITION ...)		(reverse (.definitions		results))))
    #`(module (TYPE-NAME)
	(define-type TYPE-NAME
	  (constructor
	      (make-label-type-spec (syntax TYPE-NAME) (quote UID) (syntax PARENT-STX)
				    CONSTRUCTOR-ID DESTRUCTOR-ID TYPE-PREDICATE-ID
				    EQUALITY-PREDICATE-ID COMPARISON-PROCEDURE-ID HASH-FUNCTION-ID
				    METHODS-TABLE-ALIST)))
	DEFINITION ...)))


(module (%merge-mixins-clauses)

  (define (%merge-mixins-clauses {results <parsing-results>} input-clause*.stx)
    ;;The MIXINS clause can be present multiple  times.  Each clause must have one of
    ;;the formats:
    ;;
    ;;   (mixins ?mixin-name ...)
    ;;
    ;;NOTE We cannot process the  MIXINS clauses with the SYNTAX-CLAUSES-* facilities
    ;;because such  facilities change the order  in which the clauses  are processed;
    ;;instead, the MIXINS clauses need to be spliced without changing the order.
    ;;
    (fold-right
	(lambda (input-clause.stx parsed-clause*.stx)
	  (syntax-case input-clause.stx (mixins)
	    ((mixins . ?mixins)
	     (%splice-mixins results input-clause.stx #'?mixins parsed-clause*.stx))
	    (_
	     (cons input-clause.stx parsed-clause*.stx))))
      '() input-clause*.stx))

  (define (%splice-mixins {results <parsing-results>} input-clause.stx
			  mixins.stx parsed-clause*.stx)
    (syntax-case mixins.stx ()
      ((?mixin-name . ?mixins)
       (if (identifier? #'?mixin-name)
	   (append (%splice-single-mixin results #'?mixin-name)
		   (%splice-mixins results input-clause.stx #'?mixins parsed-clause*.stx))
	 (synner "expected identifier as mixin name in MIXINS clause" input-clause.stx)))
      (()
       parsed-clause*.stx)
      (_
       (synner "invalid syntax in MIXINS clause" input-clause.stx))))

  (define (%splice-single-mixin {results <parsing-results>} mixin-name.id)
    (let ((obj (retrieve-expand-time-value mixin-name.id)))
      (syntax-case obj (define-mixin-type)
	((define-mixin-type ?mixin-name . ?clauses)
	 (xp::syntax-replace-id #'?clauses mixin-name.id (.type-name results)))
	(_
	 (synner "identifier in MIXINS clause is not a mixin name" mixin-name.id)))))

  #| end of module: %MERGE-MIXINS-CLAUSES |# )


(define-type <parsed-args>
  (vector-of (vector-of <top>)))

(define ({combine <parsing-results>} {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ((case-identifiers (.keyword clause-spec)
     ((method)			%process-clause/method)
     ((parent)			%process-clause/parent)
     ((constructor)		%process-clause/constructor)
     ((destructor)		%process-clause/destructor)
     ((type-predicate)		%process-clause/type-predicate)
     ((equality-predicate)	%process-clause/equality-predicate)
     ((comparison-procedure)	%process-clause/comparison-procedure)
     ((hash-function)		%process-clause/hash-function)
     ((nongenerative)		%process-clause/nongenerative)
     (else
      (assertion-violation __module_who__ "invalid clause spec" clause-spec)))
   results args)
  results)


(define (%process-clause/parent {results <parsing-results>} {args <parsed-args>})
  ;;The input clause must have the format:
  ;;
  ;;   (parent ?parent-id)
  ;;
  ;;and we expect ARGS to have the format:
  ;;
  ;;   #(#(?parent-id))
  ;;
  (.parent results (vector-ref (vector-ref args 0) 0)))


(define (%process-clause/constructor {results <parsing-results>} {args <parsed-args>})
  ;;An input clause must have the format:
  ;;
  ;;   (constructor ?formals ?body0 ?body ...)
  ;;
  ;;and there can be any number of them; we expect ARGS to have the format:
  ;;
  ;;   #(#(?formals ?body0 ?body ...) ...)
  ;;
  (vector-for-each
      (lambda (arg)
	(syntax-case arg ()
	  (#(?formals ?body0 ?body ...)
	   (.push-constructor-clause! results (cons #'?formals #'(?body0 ?body ...))))
	  (#(?stuff ...)
	   (synner "invalid constructor specification" #'(constructor ?stuff ...)))))
    args))


(define (%process-clause/destructor {results <parsing-results>} {args <parsed-args>})
  ;;The input clause must have the format:
  ;;
  ;;   (destructor ?formals ?body0 ?body ...)
  ;;
  ;;and we expect ARGS to have the format:
  ;;
  ;;   #(#(?formals ?body0 ?body ...))
  ;;
  (syntax-case (vector-ref args 0) ()
    (#(?formals ?body0 ?body ...)
     (.destructor-clause results (cons #'?formals #'(?body0 ?body ...))))
    (#(?stuff ...)
     (synner "invalid destructor specification" #'(destructor ?stuff ...)))))


(define (%process-clause/type-predicate {results <parsing-results>} {args <parsed-args>})
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
	((FUNC (xp::identifier-method-procname (.type-name results) #'type-predicate)))
      (.type-predicate-id results #'(syntax FUNC))
      (.push-definition!  results #`(define/typed {FUNC <type-predicate>}
				      (#,protocol-stx (is-a? _ #,(.parent results))))))))


(define (%process-clause/equality-predicate {results <parsing-results>} {args <parsed-args>})
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
	((FUNC (xp::identifier-method-procname (.type-name results) #'equality-predicate)))
      (.equality-predicate-id results #'(syntax FUNC))
      (.push-definition!      results #`(define/typed {FUNC (equality-predicate #,(.type-name results))}
					  (#,equality-predicate-protocol (equality-predicate #,(.parent results))))))))


(define (%process-clause/comparison-procedure {results <parsing-results>} {args <parsed-args>})
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
	((FUNC (xp::identifier-method-procname (.type-name results) #'comparison-procedure)))
      (.comparison-procedure-id results #'(syntax FUNC))
      (.push-definition!        results #`(define/typed {FUNC (comparison-procedure #,(.type-name results))}
					    (#,comparison-procedure-protocol (comparison-procedure #,(.parent results))))))))


(define (%process-clause/hash-function {results <parsing-results>} {args <parsed-args>})
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
	((FUNC (xp::identifier-method-procname (.type-name results) #'hash-function)))
      (.hash-function-id results #'(syntax FUNC))
      (.push-definition! results #`(define/typed {FUNC (hash-function #,(.type-name results))}
				     (#,hash-function-protocol (hash-function #,(.parent results))))))))


(define (%process-clause/nongenerative {results <parsing-results>} {args <parsed-args>})
  ;;The input clause must have the format:
  ;;
  ;;   (nongenerative ?uid)
  ;;
  ;;and we expect ARGS to have the format:
  ;;
  ;;   #(#(?uid))
  ;;
  (.uid results (vector-ref (vector-ref args 0) 0)))


(module (%process-clause/method)

  (define (%process-clause/method {results <parsing-results>} {args <parsed-args>})
    ;;This clause can be present multiple times.  Each input clause must have the
    ;;format:
    ;;
    ;;   (method (?who . ?formals) . ?body)
    ;;
    ;;and we expect ARGS to have the format:
    ;;
    ;;   #(#((?who . ?formals) . ?body) ...)
    ;;
    (vector-fold-left (lambda (results arg)
			(%process-method-spec results arg synner))
      results args))

  (define (%process-method-spec {results <parsing-results>} arg synner)
    ;;We expect ARG to have the format:
    ;;
    ;;   #((?who . ?formals) . ?body)
    ;;
    (syntax-case arg ()
      (#((?who . ?formals) ?body0 ?body ...)
       (receive (method-name.id method-procname.id method-who.stx)
	   (%parse-method-who results #'?who synner)
	 (.push-method-spec! results
	   (new <method-spec>
		method-name.id method-procname.id
		#`((#,method-who.stx {subject #,(.type-name results)} . ?formals)
		   (typed-variable-with-private-access! subject)
		   (fluid-let-syntax ((this (make-synonym-transformer (syntax subject))))
		     ?body0 ?body ...))))
	 results))
      (#(?stuff ...)
       (synner "invalid METHOD specification" #'(method ?stuff ...)))))

  (define (%parse-method-who {results <parsing-results>} who.stx synner)
    (syntax-case who.stx (brace)
      (?method-name
       (identifier? #'?method-name)
       (let ((method-procname.id (xp::identifier-method-procname (.type-name results) #'?method-name)))
	 (values #'?method-name method-procname.id method-procname.id)))
      ((brace ?method-name . ?rv-types)
       (identifier? #'?method-name)
       (let ((method-procname.id (xp::identifier-method-procname (.type-name results) #'?method-name)))
	 (values #'?method-name method-procname.id #`(brace #,method-procname.id . ?rv-types))))
      (_
       (synner "invalid method name specification" who.stx))))

  #| end of module: %PROCESS-CLAUSE/METHOD |# )


;;;; let's go

(case-define synner
  ((message)
   (synner message #f))
  ((message subform)
   (syntax-violation __module_who__ message input-form.stx subform)))

(main input-form.stx))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put '.push-method-spec!			'scheme-indent-function 1)
;; eval: (put '.push-definition!			'scheme-indent-function 1)
;; End:
