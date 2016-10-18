;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: mixins for record-types
;;;Date: Wed Jun  1, 2016
;;;
;;;Abstract
;;;
;;;	Mixins  are collections  of  clauses  to be  inserted  in  the definition  of
;;;	record-types.  They are meant to be used as follows:
;;;
;;;	   (define-mixin-type <adder>
;;;	     (method (add {self <adder>})
;;;	       (+ (.one self) (.two self))))
;;;
;;;	   (define-record-type <duo>
;;;	     (fields one two)
;;;	     (mixins <adder>))
;;;
;;;	   (define O (new <duo> 1 2))
;;;	   (.add O)	=> 3
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
(library (vicare language-extensions mixins (0 4 2016 6 1))
  (options typed-language)
  (export
    define-mixin-type
    nongenerative fields sealed opaque protocol super-protocol
    method virtual-method seal-method
    public protected private
    define-type-descriptors
    strip-angular-parentheses
    custom-printer
    type-predicate
    equality-predicate
    comparison-procedure
    hash-function
    mixins
    implements)
  (import (vicare)
    (for (prefix (vicare expander) xp::)
      expand))


(define-syntax (define-mixin-type input-form.stx)
  (define-constant __module_who__ 'define-mixin-type)

  (case-define synner
    ((message)
     (syntax-violation __module_who__ message input-form.stx #f))
    ((message subform)
     (syntax-violation __module_who__ message input-form.stx subform)))


(define-constant CLAUSE-SPEC*
  (xp::syntax-clauses-validate-specs
   (list
    ;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
    (new xp::<syntax-clause-spec> #'nongenerative		0 1      0 1      '() '())
    (new xp::<syntax-clause-spec> #'implements			0 +inf.0 0 +inf.0 '() '())
    (new xp::<syntax-clause-spec> #'define-type-descriptors	0 1      0 0      '() '())
    (new xp::<syntax-clause-spec> #'strip-angular-parentheses	0 1      0 0      '() '())
    (new xp::<syntax-clause-spec> #'sealed			0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'opaque			0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'protocol			0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'super-protocol		0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'fields			0 +inf.0 1 +inf.0 '() '())
    ;;
    (new xp::<syntax-clause-spec> #'method			0 +inf.0 2 +inf.0 '() '())
    (new xp::<syntax-clause-spec> #'virtual-method		0 +inf.0 2 +inf.0 '() '())
    (new xp::<syntax-clause-spec> #'seal-method			0 +inf.0 2 +inf.0 '() '())
    ;;
    (new xp::<syntax-clause-spec> #'custom-printer		0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'type-predicate		0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'equality-predicate		0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'comparison-procedure	0 1      1 1      '() '())
    (new xp::<syntax-clause-spec> #'hash-function		0 1      1 1      '() '())
    #| end of LIST |# )))


(define-record-type <parsing-results>
  (fields
    (immutable	type-name)
		;A syntactic identifier representing the mixin type name.
    (mutable	rev-parsed-clauses)
		;A  list  of  syntax  objects representing  the  parsed  clauses,  in
		;reversed order.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (named-lambda make-<parsing-results>
	  ({type-name xp::<syntactic-identifier>})
	(make-record type-name '()))))

  (constructor-signature
    (lambda (xp::<syntactic-identifier>) => (<parsing-results>)))

  (method ({push-clause! <void>} clause.stx)
    (.rev-parsed-clauses this (cons clause.stx (.rev-parsed-clauses this))))

  (method (parsed-clauses)
    (reverse (.rev-parsed-clauses this)))

  #| end of DEFINE-RECORD-TYPE |# )


(define (main input-form.stx)
  (syntax-case input-form.stx ()
    ((_ ?type-name . ?clauses)
     (begin
       (unless (identifier? #'?type-name)
	 (synner "expected identifier as mixin type name" #'?type-name))
       (%parse-clauses #'?type-name #'?clauses synner)))
    (_
     (synner "invalid DEFINE-MIXIN-TYPE syntax use"))))

(define (%parse-clauses type-name.id clauses.stx synner)
  (let* ((clause*.stx (xp::syntax-clauses-unwrap clauses.stx synner))
	 (clause*.stx (%merge-mixins-clauses type-name.id clause*.stx synner))
	 (clause*.stx (%splice-protection-levels clause*.stx synner)))
    (%build-output (xp::syntax-clauses-fold-specs combine (new <parsing-results> type-name.id) CLAUSE-SPEC* clause*.stx synner))))

(define (%build-output {results <parsing-results>})
  #`(define-syntax #,(.type-name results)
      (make-expand-time-value (cons* (syntax define-mixin-type)
				     (syntax #,(.type-name results))
				     (syntax #,(.parsed-clauses results))))))


(module (%merge-mixins-clauses)

  (define (%merge-mixins-clauses type-name.id input-clause*.stx synner)
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
	  (syntax-case input-clause.stx (mixins public protected private)
	    ((mixins . ?mixins)
	     (%splice-mixins type-name.id input-clause.stx
			     #'?mixins parsed-clause*.stx synner))

	    ((public . ?clauses)
	     (cons (cons #'public (%merge-mixins-clauses type-name.id #'?clauses synner))
		   parsed-clause*.stx))

	    ((protected . ?clauses)
	     (cons (cons #'protected (%merge-mixins-clauses type-name.id #'?clauses synner))
		   parsed-clause*.stx))

	    ((private . ?clauses)
	     (cons (cons #'private (%merge-mixins-clauses type-name.id #'?clauses synner))
		   parsed-clause*.stx))

	    (_
	     (cons input-clause.stx parsed-clause*.stx))))
      '() input-clause*.stx))

  (define (%splice-mixins type-name.id input-clause.stx
			  mixins.stx parsed-clause*.stx synner)
    (syntax-case mixins.stx ()
      ((?mixin-name . ?mixins)
       (if (identifier? #'?mixin-name)
	   (append (%splice-single-mixin type-name.id #'?mixin-name synner)
		   (%splice-mixins type-name.id input-clause.stx
				   #'?mixins parsed-clause*.stx synner))
	 (synner "expected identifier as mixin name in MIXINS clause" input-clause.stx)))
      (()
       parsed-clause*.stx)
      (_
       (synner "invalid syntax in MIXINS clause" input-clause.stx))))

  (define (%splice-single-mixin type-name.id mixin-name.id synner)
    (let ((obj (retrieve-expand-time-value mixin-name.id)))
      (syntax-case obj (define-mixin-type)
	((define-mixin-type ?mixin-name . ?clauses)
	 (xp::syntax-replace-id #'?clauses mixin-name.id type-name.id))
	(_
	 (synner "identifier in MIXINS clause is not a mixin name" mixin-name.id)))))

  #| end of module: %MERGE-MIXINS-CLAUSES |# )


(module (%splice-protection-levels)

  (define (%splice-protection-levels input-clause*.stx synner)
    (receive-and-return (spliced-clause*.stx)
	(fold-right
	    (lambda (input-clause.stx parsed-clause*.stx)
	      (syntax-case input-clause.stx (public protected private)
		((public . ?clauses)
		 (%splice-protection #'public    #'?clauses parsed-clause*.stx synner))

		((protected . ?clauses)
		 (%splice-protection #'protected #'?clauses parsed-clause*.stx synner))

		((private . ?clauses)
		 (%splice-protection #'private   #'?clauses parsed-clause*.stx synner))

		(_
		 (cons input-clause.stx parsed-clause*.stx))))
	  '() input-clause*.stx)
      (%validate-protection-levels spliced-clause*.stx synner)))

  (define (%splice-protection protection.id nested-clause*.stx parsed-clause*.stx synner)
    (fold-right
	(lambda (nested-clause.stx parsed-clause*.stx)
	  (syntax-case nested-clause.stx (fields method virtual-method seal-method)
	    ((fields . ?stuff)
	     (cons #`(fields #,protection.id . ?stuff)
		   parsed-clause*.stx))

	    ((method . ?stuff)
	     (cons #`(method #,protection.id . ?stuff)
		   parsed-clause*.stx))

	    ((virtual-method . ?stuff)
	     (cons #`(virtual-method #,protection.id . ?stuff)
		   parsed-clause*.stx))

	    ((seal-method . ?stuff)
	     (cons #`(seal-method #,protection.id . ?stuff)
		   parsed-clause*.stx))

	    (_
	     (synner "wrapping protection level syntax around clause not accepting protection level specification"
		     nested-clause.stx))))
      parsed-clause*.stx nested-clause*.stx))

  (define (%validate-protection-levels spliced-clause*.stx synner)
    (for-each (lambda (spliced-clause.stx)
		(syntax-case spliced-clause.stx (fields method virtual-method seal-method)
		  ((fields . ?stuff)
		   (%validate-stuff spliced-clause.stx #'?stuff synner))

		  ((method . ?stuff)
		   (%validate-stuff spliced-clause.stx #'?stuff synner))

		  ((virtual-method . ?stuff)
		   (%validate-stuff spliced-clause.stx #'?stuff synner))

		  ((seal-method . ?stuff)
		   (%validate-stuff spliced-clause.stx #'?stuff synner))

		  (_
		   (void))))
      spliced-clause*.stx))

  (define (%validate-stuff clause.stx stuff.stx synner)
    (syntax-case stuff.stx ()
      ((?thing1 ?thing2 . ?rest)
       (when (and (protection-level-id #'?thing1)
		  (protection-level-id #'?thing2))
	 (synner "found double protection level specification in clause"
		 clause.stx)))
      (_
       (void))))

  (define (protection-level-id stx)
    (and (identifier? stx)
	 (or (free-identifier=? stx #'public)
	     (free-identifier=? stx #'protected)
	     (free-identifier=? stx #'private))))

  #| end of module: %SPLICE-PROTECTION-LEVELS |# )


(define-type <parsed-args>
  (vector-of (vector-of <top>)))

(define ({combine <parsing-results>} {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ((case-identifiers (.keyword clause-spec)
     ((nongenerative)			%process-clause/nongenerative)
     ((implements)			%process-clause/implements)
     ((define-type-descriptors)		%process-clause/define-type-descriptors)
     ((strip-angular-parentheses)	%process-clause/strip-angular-parentheses)
     ((sealed)				%process-clause/sealed)
     ((opaque)				%process-clause/opaque)
     ((protocol)			%process-clause/protocol)
     ((super-protocol)			%process-clause/super-protocol)
     ((fields)				%process-clause/fields)
     ;;
     ((method)				%process-clause/method)
     ((virtual-method)			%process-clause/virtual-method)
     ((seal-method)			%process-clause/seal-method)
     ;;
     ((custom-printer)			%process-clause/custom-printer)
     ((type-predicate)			%process-clause/type-predicate)
     ((equality-predicate)		%process-clause/equality-predicate)
     ((comparison-procedure)		%process-clause/comparison-procedure)
     ((hash-function)			%process-clause/hash-function)
     ;;
     (else
      (assertion-violation __module_who__ "invalid clause spec" clause-spec)))
   results clause-spec args)
  results)


(define (%process-clause/nongenerative {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This input clause is present at most once.  The input clause must have one of the
  ;;formats:
  ;;
  ;;   (nongenerative)
  ;;   (nongenerative ?uid)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#())
  ;;   #(#(?uid))
  ;;
  ;;We validate the ?UID and register the clause as is but fully unwrapped.
  ;;
  (let ((first-item (vector-ref args 0)))
    (if (vector-empty? first-item)
	;;No UID specified.
	(.push-clause! results (list (.keyword clause-spec)))
      (let ((uid (vector-ref first-item 0)))
	(if (identifier? uid)
	    (.push-clause! results (list (.keyword clause-spec) uid))
	  (synner "expected empty clause or single identifier argument in NONGENERATIVE clause"
		  (list (.keyword clause-spec) uid)))))))


(define (%process-clause/implements {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This input clause can appear any number of times and it must have the format:
  ;;
  ;;   (implements ?interface ...)
  ;;
  ;;we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?interface ...) ...)
  ;;
  ;;We collapse the  multiple clauses into a single IMPLEMENTS  clause, then register
  ;;it fully unwrapped.
  ;;
  (.push-clause! results (cons (.keyword clause-spec)
			       (vector-fold-right
				   (lambda (arg knil)
				     (vector-fold-right
					 (lambda (iface.id knil)
					   (if (identifier? iface.id)
					       (cons iface.id knil)
					     (synner "expected syntactic identifier as interface name" iface.id)))
				       knil arg))
				 '() args))))


(define (%process-clause/define-type-descriptors {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This input clause is present at most once.  The input clause must have one of the
  ;;formats:
  ;;
  ;;   (define-type-descriptors)
  ;;
  ;;and we expect ARGS to have one of the format:
  ;;
  ;;   #(#())
  ;;
  ;;We register the clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec))))


(define (%process-clause/strip-angular-parentheses {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This input clause is present at most once.  The input clause must have one of the
  ;;formats:
  ;;
  ;;   (strip-angular-parentheses)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#())
  ;;
  ;;We register the clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec))))


(define (%process-clause/sealed {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (sealed #t)
  ;;   (sealed #f)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(#t))
  ;;   #(#(#f))
  ;;
  ;;We validate the value then register the clause as is, fully unwrapped.
  ;;
  (let* ((obj		(vector-ref (vector-ref args 0) 0))
	 (clause	(list (.keyword clause-spec) obj)))
    (if (boolean? (syntax->datum obj))
	(.push-clause! results clause)
      (synner "expected boolean argument in SEALED clause" clause))))


(define (%process-clause/opaque {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (opaque #t)
  ;;   (opaque #f)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(#t))
  ;;   #(#(#f))
  ;;
  ;;We validate the value then register the clause as is, fully unwrapped.
  ;;
  (let* ((obj		(vector-ref (vector-ref args 0) 0))
	 (clause	(list (.keyword clause-spec) obj)))
    (if (boolean? (syntax->datum obj))
	(.push-clause! results clause)
      (synner "expected boolean argument in OPAQUE clause" clause))))


(define (%process-clause/protocol {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (protocol ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  ;;We register the clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec) (vector-ref (vector-ref args 0) 0))))


(define (%process-clause/super-protocol {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (super-protocol ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  ;;We register the clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec) (vector-ref (vector-ref args 0) 0))))


(module (%process-clause/fields)

  (define (%process-clause/fields {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
    ;;This clause can be present multiple times.   Each input clause must have one of
    ;;the formats:
    ;;
    ;;   (fields ?protection-level ?field-spec ...)
    ;;   (fields ?field-spec ...)
    ;;
    ;;and we expect ARGS to have one of the formats:
    ;;
    ;;   #(?meat ...)
    ;;
    ;;where ?MEAT as one of the formats:
    ;;
    ;;   #(?protection-level ?field-spec ...)
    ;;   #(?field-spec ...)
    ;;
    ;;We validate the  field specifications and register each FIELDS  clauses as they
    ;;are, fully unwrapped.   To preserve the protection level: we  do *not* collapse
    ;;multiple FIELDS clauses into a single clause.
    ;;
    (vector-fold-left
	(lambda (unused arg)
	  (.push-clause! results (cons #'fields (%process-fields-spec arg))))
      #f args))

  (define (%process-fields-spec arg)
    ;;We expect ARG to have one of the formats:
    ;;
    ;;    #(?field-spec ...)
    ;;    #(?protection-level ?field-spec ...)
    ;;
    (syntax-case arg (public protected private)
      (#(public ?field-spec ...)
       (cons #'public		(fold-right %parse-field-spec '() (xp::syntax->list #'(?field-spec ...)))))

      (#(protected ?field-spec ...)
       (cons #'protected	(fold-right %parse-field-spec '() (xp::syntax->list #'(?field-spec ...)))))

      (#(private ?field-spec ...)
       (cons #'private		(fold-right %parse-field-spec '() (xp::syntax->list #'(?field-spec ...)))))

      (#(?field-spec ...)
       (fold-right %parse-field-spec '() (xp::syntax->list #'(?field-spec ...))))

      (#(?stuff ...)
       (synner "invalid fields specification" #'(fields ?stuff ...)))))

  (define (%parse-field-spec field-spec.stx knil)
    (syntax-case field-spec.stx (brace mutable immutable)
      ((immutable ?field-name)
       (%validate-field-name #'?field-name field-spec.stx synner))
      ((immutable ?field-name ?accessor)
       (identifier? #'?accessor)
       (%validate-field-name #'?field-name field-spec.stx synner))
      ((mutable ?field-name)
       (%validate-field-name #'?field-name field-spec.stx synner))
      ((mutable ?field-name ?accessor ?mutator)
       (and (identifier? #'?accessor)
	    (identifier? #'?mutator))
       (%validate-field-name #'?field-name field-spec.stx synner))
      (?field-name
       (%validate-field-name #'?field-name field-spec.stx synner))
      (_
       (synner "invalid field specification" field-spec.stx)))
    (cons field-spec.stx knil))

  (define (%validate-field-name name-spec.stx field-spec.stx synner)
    (syntax-case name-spec.stx (brace)
      (?field-name
       (identifier? #'?field-name)
       (void))
      ((brace ?field-name ?type-annotation)
       (identifier? #'?field-name)
       (void))
      (_
       (synner "invalid field specification" field-spec.stx))))

  #| end of module: %PROCESS-CLAUSE/FIELDS |# )


(module (%process-clause/method)

  (define (%process-clause/method {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
    ;;This clause can be present multiple times.   Each input clause must have one of
    ;;the formats:
    ;;
    ;;   (method (?who . ?args) . ?body)
    ;;   (method ?protection-level (?who . ?args) . ?body)
    ;;
    ;;and we expect ARGS to have the format:
    ;;
    ;;   #(?meat ...)
    ;;
    ;;with ?MEAT having one of the formats:
    ;;
    ;;   #((?who . ?formals) . ?body)
    ;;   #(?protection-level (?who . ?formals) . ?body)
    ;;
    (vector-fold-left %process-method-spec results args))

  (define (%process-method-spec {results <parsing-results>} {arg <vector>})
    ;;We expect ARG to have one of the formats:
    ;;
    ;;   #((?who . ?formals) . ?body)
    ;;   #(?protection-level (?who . ?formals) . ?body)
    ;;
    (.push-clause! results
		   (syntax-case arg (public protected private)
		     (#((?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(method (?who . ?formals) ?body0 ?body ...)))

		     (#(public (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(method public (?who . ?formals) ?body0 ?body ...)))

		     (#(protected (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(method protected (?who . ?formals) ?body0 ?body ...)))

		     (#(private (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(method private (?who . ?formals) ?body0 ?body ...)))

		     (#(?stuff ...)
		      (synner "invalid method specification" #'(method ?stuff ...)))))
    results)

  (define (%parse-who who.stx synner)
    (syntax-case who.stx (brace)
      (?method-name
       (identifier? #'?method-name)
       (void))
      ((brace ?method-name . ?rv-types)
       (identifier? #'?method-name)
       (void))
      (_
       (synner "invalid method name and/or return values specification" #'?who))))

  #| end of module: %PROCESS-CLAUSE/METHOD |# )


(module (%process-clause/virtual-method)

  (define (%process-clause/virtual-method {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
    ;;This clause can be present multiple times.   Each input clause must have one of
    ;;the formats:
    ;;
    ;;   (virtual-method (?who . ?args) . ?body)
    ;;   (virtual-method ?protection-level (?who . ?args) . ?body)
    ;;
    ;;and we expect ARGS to have the format:
    ;;
    ;;   #(?meat ...)
    ;;
    ;;with ?MEAT having one of the formats:
    ;;
    ;;   #((?who . ?formals) . ?body)
    ;;   #(?protection-level (?who . ?formals) . ?body)
    ;;
    (vector-fold-left %process-method-spec results args))

  (define (%process-method-spec {results <parsing-results>} {arg <vector>})
    ;;We expect ARG to have one of the formats:
    ;;
    ;;   #((?who . ?formals) . ?body)
    ;;   #(?protection-level (?who . ?formals) . ?body)
    ;;
    (.push-clause! results
		   (syntax-case arg (public protected private)
		     (#((?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(virtual-method (?who . ?formals) ?body0 ?body ...)))

		     (#(public (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(virtual-method public (?who . ?formals) ?body0 ?body ...)))

		     (#(protected (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(virtual-method protected (?who . ?formals) ?body0 ?body ...)))

		     (#(private (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(virtual-method private (?who . ?formals) ?body0 ?body ...)))

		     (#(?stuff ...)
		      (synner "invalid method specification" #'(virtual-method ?stuff ...)))))
    results)

  (define (%parse-who who.stx synner)
    (syntax-case who.stx (brace)
      (?method-name
       (identifier? #'?method-name)
       (void))
      ((brace ?method-name . ?rv-types)
       (identifier? #'?method-name)
       (void))
      (_
       (synner "invalid method name and/or return values specification" #'?who))))

  #| end of module: %PROCESS-CLAUSE/VIRTUAL-METHOD |# )


(module (%process-clause/seal-method)

  (define (%process-clause/seal-method {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
    ;;This clause can be present multiple times.   Each input clause must have one of
    ;;the formats:
    ;;
    ;;   (seal-method (?who . ?args) . ?body)
    ;;   (seal-method ?protection-level (?who . ?args) . ?body)
    ;;
    ;;and we expect ARGS to have the format:
    ;;
    ;;   #(?meat ...)
    ;;
    ;;with ?MEAT having one of the formats:
    ;;
    ;;   #((?who . ?formals) . ?body)
    ;;   #(?protection-level (?who . ?formals) . ?body)
    ;;
    (vector-fold-left %process-method-spec results args))

  (define (%process-method-spec {results <parsing-results>} {arg <vector>})
    ;;We expect ARG to have one of the formats:
    ;;
    ;;   #((?who . ?formals) . ?body)
    ;;   #(?protection-level (?who . ?formals) . ?body)
    ;;
    (.push-clause! results
		   (syntax-case arg (public protected private)
		     (#((?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(seal-method (?who . ?formals) ?body0 ?body ...)))

		     (#(public (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(seal-method public (?who . ?formals) ?body0 ?body ...)))

		     (#(protected (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(seal-method protected (?who . ?formals) ?body0 ?body ...)))

		     (#(private (?who . ?formals) ?body0 ?body ...)
		      (begin
			(%parse-who #'?who synner)
			#'(seal-method private (?who . ?formals) ?body0 ?body ...)))

		     (#(?stuff ...)
		      (synner "invalid method specification" #'(seal-method ?stuff ...)))))
    results)

  (define (%parse-who who.stx synner)
    (syntax-case who.stx (brace)
      (?method-name
       (identifier? #'?method-name)
       (void))
      ((brace ?method-name . ?rv-types)
       (identifier? #'?method-name)
       (void))
      (_
       (synner "invalid method name and/or return values specification" #'?who))))

  #| end of module: %PROCESS-CLAUSE/SEAL-METHOD |# )


(define (%process-clause/custom-printer {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (custom-printer ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  ;;We register this clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec) (vector-ref (vector-ref args 0) 0))))


(define (%process-clause/type-predicate {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (type-predicate ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  ;;We register this clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec) (vector-ref (vector-ref args 0) 0))))


(define (%process-clause/equality-predicate {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (equality-predicate ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  ;;We register this clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec) (vector-ref (vector-ref args 0) 0))))


(define (%process-clause/comparison-procedure {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (comparison-procedure ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  ;;We register this clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec) (vector-ref (vector-ref args 0) 0))))


(define (%process-clause/hash-function {results <parsing-results>} {clause-spec xp::<syntax-clause-spec>} {args <parsed-args>})
  ;;This clause  is present  at most  once.  The input  clause must  have one  of the
  ;;formats:
  ;;
  ;;   (hash-function ?expr)
  ;;
  ;;and we expect ARGS to have one of the formats:
  ;;
  ;;   #(#(?expr))
  ;;
  ;;We register this clause as is, fully unwrapped.
  ;;
  (.push-clause! results (list (.keyword clause-spec) (vector-ref (vector-ref args 0) 0))))


(main input-form.stx))


;;;; done

#| end of library |# )

;;; end of file
