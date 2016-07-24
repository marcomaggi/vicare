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
    (for (vicare expander)
      expand))


(define-syntax define-mixin-type
  (internal-body
    (define-constant __module_who__ 'define-mixin-type)

    (define-constant CLAUSE-SPEC*
      (syntax-clauses-validate-specs
       (list
	;;KEYWORD MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
	(new <syntax-clause-spec> #'nongenerative		0 1      0 1      '() '())
	(new <syntax-clause-spec> #'implements			0 +inf.0 0 +inf.0 '() '())
	(new <syntax-clause-spec> #'define-type-descriptors	0 1      0 0      '() '())
	(new <syntax-clause-spec> #'strip-angular-parentheses	0 1      0 0      '() '())
	(new <syntax-clause-spec> #'sealed			0 1      1 1      '() '())
	(new <syntax-clause-spec> #'opaque			0 1      1 1      '() '())
	(new <syntax-clause-spec> #'protocol			0 1      1 1      '() '())
	(new <syntax-clause-spec> #'super-protocol		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'fields			0 +inf.0 1 +inf.0 '() '())
	;;
	(new <syntax-clause-spec> #'method			0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'virtual-method		0 +inf.0 2 +inf.0 '() '())
	(new <syntax-clause-spec> #'seal-method			0 +inf.0 2 +inf.0 '() '())
	;;
	(new <syntax-clause-spec> #'custom-printer		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'type-predicate		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'equality-predicate		0 1      1 1      '() '())
	(new <syntax-clause-spec> #'comparison-procedure	0 1      1 1      '() '())
	(new <syntax-clause-spec> #'hash-function		0 1      1 1      '() '())
	#| end of LIST |# )))

;;; --------------------------------------------------------------------

    (define (main input-form.stx synner)
      (syntax-case input-form.stx ()
	((_ ?type-name . ?clauses)
	 (%parse-clauses #'?type-name #'?clauses synner))
	(_
	 (synner "invalid DEFINE-MIXIN-TYPE syntax use"))))

    (define (%parse-clauses type-name.id clauses.stx synner)
      (unless (identifier? type-name.id)
	(synner "expected identifier as mixin type name" type-name.id))
      (let* ((clause*.stx (syntax-clauses-unwrap clauses.stx synner))
	     (clause*.stx (%merge-mixins-clauses type-name.id clause*.stx synner))
	     (clause*.stx (reverse
			   (syntax-clauses-fold-specs (lambda (rev-parsed-clause*.stx {spec <syntax-clause-spec>} args)
							(combine type-name.id rev-parsed-clause*.stx spec args synner))
						      '() ;REV-PARSED-CLAUSE*.STX, list of parsed clauses
						      CLAUSE-SPEC* clause*.stx synner))))
	#`(define-syntax #,type-name.id
	    (make-expand-time-value (cons* (syntax define-mixin-type) (syntax #,type-name.id) (syntax #,clause*.stx))))))

;;; --------------------------------------------------------------------

    (module (%merge-mixins-clauses)

      (define (%merge-mixins-clauses type-name.id input-clause*.stx synner)
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
		 (%splice-mixins type-name.id input-clause.stx
				 #'?mixins parsed-clause*.stx synner))

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
	     (syntax-replace-id #'?clauses mixin-name.id type-name.id))
	    (_
	     (synner "identifier in MIXINS clause is not a mixin name" mixin-name.id)))))

      #| end of module: %MERGE-MIXINS-CLAUSES |# )

;;; --------------------------------------------------------------------

    (define (combine type-name.id rev-parsed-clause*.stx {spec <syntax-clause-spec>} args synner)
      ((case-identifiers (.keyword spec)
	 ((nongenerative)		%process-clause/nongenerative)
	 ((implements)			%process-clause/implements)
	 ((define-type-descriptors)	%process-clause/define-type-descriptors)
	 ((strip-angular-parentheses)	%process-clause/strip-angular-parentheses)
	 ((sealed)			%process-clause/sealed)
	 ((opaque)			%process-clause/opaque)
	 ((protocol)			%process-clause/protocol)
	 ((super-protocol)		%process-clause/super-protocol)
	 ((fields)			%process-clause/fields)
	 ;;
	 ((method)			%process-clause/method)
	 ((virtual-method)		%process-clause/virtual-method)
	 ((seal-method)			%process-clause/seal-method)
	 ;;
	 ((custom-printer)		%process-clause/custom-printer)
	 ((type-predicate)		%process-clause/type-predicate)
	 ((equality-predicate)		%process-clause/equality-predicate)
	 ((comparison-procedure)	%process-clause/comparison-procedure)
	 ((hash-function)		%process-clause/hash-function)
	 ;;
	 (else
	  (assertion-violation __module_who__ "invalid clause spec" spec)))
       type-name.id args spec rev-parsed-clause*.stx synner))

;;; --------------------------------------------------------------------

    (define (%process-clause/nongenerative type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
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
      (let ((first-item (vector-ref args 0)))
	(if (vector-empty? first-item)
	    (cons (list (.keyword spec))
		  rev-parsed-clause*.stx)
	  (let ((uid (vector-ref first-item 0)))
	    (if (identifier? uid)
		(cons (list (.keyword spec) uid)
		      rev-parsed-clause*.stx)
	      (synner "expected empty clause or single identifier argument in NONGENERATIVE clause"
		      (list (.keyword spec) uid)))))))

    (define (%process-clause/implements type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;This  input clause  can appear  any  number of  times  and it  must have  the
      ;;format:
      ;;
      ;;   (implements ?interface ...)
      ;;
      ;;we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?interface ...) ...)
      ;;
      (cons (cons (.keyword spec)
		  (vector-fold-right
		      (lambda (arg knil)
			(vector-fold-right
			    (lambda (iface.id knil)
			      (unless (identifier? iface.id)
				(synner "expected syntactic identifier as interface name" iface.id))
			      (cons iface.id knil))
			  knil arg))
		    '() args))
	    rev-parsed-clause*.stx))

    (define (%process-clause/define-type-descriptors type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (define-type-descriptors)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#())
      ;;
      (cons (list (.keyword spec)) rev-parsed-clause*.stx))

    (define (%process-clause/strip-angular-parentheses type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (strip-angular-parentheses)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#())
      ;;
      (cons (list (.keyword spec)) rev-parsed-clause*.stx))

    (define (%process-clause/sealed type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
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
	    (cons (list (.keyword spec) obj) rev-parsed-clause*.stx)
	  (synner "expected boolean argument in SEALED clause" (list (.keyword spec) obj)))))

    (define (%process-clause/opaque type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
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
	    (cons (list (.keyword spec) obj) rev-parsed-clause*.stx)
	  (synner "expected boolean argument in OPAQUE clause" (list (.keyword spec) obj)))))

    (define (%process-clause/protocol type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (protocol ?expr)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?expr))
      ;;
      (let ((obj (vector-ref (vector-ref args 0) 0)))
	(cons (list (.keyword spec) obj) rev-parsed-clause*.stx)))

    (define (%process-clause/super-protocol type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (super-protocol ?expr)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?expr))
      ;;
      (let ((obj (vector-ref (vector-ref args 0) 0)))
	(cons (list (.keyword spec) obj) rev-parsed-clause*.stx)))

;;; --------------------------------------------------------------------

    (define (%process-clause/fields type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;This clause can  be present multiple times.  Each input  clause must have one
      ;;of the formats:
      ;;
      ;;   (fields ?field-spec ...)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?field-spec ...) ...)
      ;;
      (cons (cons #'fields
		  (reverse
		   (vector-fold-left (lambda (knil arg)
				       (append (%process-fields-spec arg synner)
					       knil))
		     '() args)))
	    rev-parsed-clause*.stx))

    (define (%process-fields-spec arg synner)
      ;;We expect ARG to have the format:
      ;;
      ;;    #(?field-spec ...)
      ;;
      (define (%validate-field-name name-spec.stx field-spec.stx)
	(syntax-case name-spec.stx (brace)
	  (?field-name
	   (identifier? #'?field-name)
	   (void))
	  ((brace ?field-name ?type-annotation)
	   (identifier? #'?field-name)
	   (void))
	  (_
	   (synner "invalid field specification" field-spec.stx))))
      (syntax-case arg ()
	(#(?field-spec ...)
	 (fold-left (lambda (knil field-spec.stx)
		      (syntax-case field-spec.stx (brace mutable immutable)
			((immutable ?field-name)
			 (%validate-field-name #'?field-name field-spec.stx))
			((immutable ?field-name ?accessor)
			 (identifier? #'?accessor)
			 (%validate-field-name #'?field-name field-spec.stx))
			((mutable ?field-name)
			 (%validate-field-name #'?field-name field-spec.stx))
			((mutable ?field-name ?accessor ?mutator)
			 (and (identifier? #'?accessor)
			      (identifier? #'?mutator))
			 (%validate-field-name #'?field-name field-spec.stx))
			(?field-name
			 (%validate-field-name #'?field-name field-spec.stx))
			(_
			 (synner "invalid field specification" field-spec.stx)))
		      (cons field-spec.stx knil))
	   '()
	   (syntax->list #'(?field-spec ...))))
	(#(?stuff ...)
	 (synner "invalid fields specification" #'(fields ?stuff ...)))))

;;; --------------------------------------------------------------------

    (define (%process-clause/method type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;This clause can  be present multiple times.  Each input  clause must have the
      ;;format:
      ;;
      ;;   (method (?who . ?args) . ?body)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#((?who . ?args) . ?body) ...)
      ;;
      (vector-fold-left (lambda (rev-parsed-clause*.stx arg)
			  (cons (%process-method-spec arg synner)
				rev-parsed-clause*.stx))
	rev-parsed-clause*.stx
	args))

    (define (%process-method-spec arg synner)
      ;;We expect ARG to have the format:
      ;;
      ;;   #((?who . ?formals) . ?body)
      ;;
      (syntax-case arg ()
	(#((?who . ?formals) ?body0 ?body ...)
	 (begin
	   (syntax-case #'?who (brace)
	     (?method-name
	      (identifier? #'?method-name)
	      (void))
	     ((brace ?method-name . ?rv-types)
	      (identifier? #'?method-name)
	      (void))
	     (_
	      (synner "invalid method name and/or return values specification" #'?who)))
	   #'(method (?who . ?formals) ?body0 ?body ...)))
	(#(?stuff ...)
	 (synner "invalid method specification" #'(method ?stuff ...)))))

;;; --------------------------------------------------------------------

    (define (%process-clause/virtual-method type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;This clause can  be present multiple times.  Each input  clause must have the
      ;;format:
      ;;
      ;;   (virtual-method (?who . ?args) . ?body)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#((?who . ?args) . ?body) ...)
      ;;
      (vector-fold-left (lambda (rev-parsed-clause*.stx arg)
			  (cons (%process-virtual-method-spec arg synner)
				rev-parsed-clause*.stx))
	rev-parsed-clause*.stx
	args))

    (define (%process-virtual-method-spec arg synner)
      ;;We expect ARG to have the format:
      ;;
      ;;   #((?who . ?formals) . ?body)
      ;;
      (syntax-case arg ()
	(#((?who . ?formals) ?body0 ?body ...)
	 (begin
	   (syntax-case #'?who (brace)
	     (?method-name
	      (identifier? #'?method-name)
	      (void))
	     ((brace ?method-name . ?rv-types)
	      (identifier? #'?method-name)
	      (void))
	     (_
	      (synner "invalid method name and/or return values specification" #'?who)))
	   #'(virtual-method (?who . ?formals) ?body0 ?body ...)))
	(#(?stuff ...)
	 (synner "invalid virtual method specification" #'(virtual-method ?stuff ...)))))

;;; --------------------------------------------------------------------

    (define (%process-clause/seal-method type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;This clause can  be present multiple times.  Each input  clause must have the
      ;;format:
      ;;
      ;;   (seal-method (?who . ?args) . ?body)
      ;;
      ;;and we expect ARGS to have the format:
      ;;
      ;;   #(#((?who . ?args) . ?body) ...)
      ;;
      (vector-fold-left (lambda (rev-parsed-clause*.stx arg)
			  (cons (%process-seal-method-spec arg synner)
				rev-parsed-clause*.stx))
	rev-parsed-clause*.stx
	args))

    (define (%process-seal-method-spec arg synner)
      ;;We expect ARG to have the format:
      ;;
      ;;   #((?who . ?formals) . ?body)
      ;;
      (syntax-case arg ()
	(#((?who . ?formals) ?body0 ?body ...)
	 (begin
	   (syntax-case #'?who (brace)
	     (?method-name
	      (identifier? #'?method-name)
	      (void))
	     ((brace ?method-name . ?rv-types)
	      (identifier? #'?method-name)
	      (void))
	     (_
	      (synner "invalid method name and/or return values specification" #'?who)))
	   #'(seal-method (?who . ?formals) ?body0 ?body ...)))
	(#(?stuff ...)
	 (synner "invalid seal method specification" #'(seal-method ?stuff ...)))))

;;; --------------------------------------------------------------------

    (define (%process-clause/custom-printer type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (custom-printer ?expr)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?expr))
      ;;
      (let ((obj (vector-ref (vector-ref args 0) 0)))
	(cons (list (.keyword spec) obj) rev-parsed-clause*.stx)))

    (define (%process-clause/type-predicate type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (type-predicate ?expr)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?expr))
      ;;
      (let ((obj (vector-ref (vector-ref args 0) 0)))
	(cons (list (.keyword spec) obj) rev-parsed-clause*.stx)))

    (define (%process-clause/equality-predicate type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (equality-predicate ?expr)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?expr))
      ;;
      (let ((obj (vector-ref (vector-ref args 0) 0)))
	(cons (list (.keyword spec) obj) rev-parsed-clause*.stx)))

    (define (%process-clause/comparison-procedure type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (comparison-procedure ?expr)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?expr))
      ;;
      (let ((obj (vector-ref (vector-ref args 0) 0)))
	(cons (list (.keyword spec) obj) rev-parsed-clause*.stx)))

    (define (%process-clause/hash-function type-name.id args {spec <syntax-clause-spec>} rev-parsed-clause*.stx synner)
      ;;The input clause must have one of the formats:
      ;;
      ;;   (hash-function ?expr)
      ;;
      ;;and we expect ARGS to have one of the formats:
      ;;
      ;;   #(#(?expr))
      ;;
      (let ((obj (vector-ref (vector-ref args 0) 0)))
	(cons (list (.keyword spec) obj) rev-parsed-clause*.stx)))

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
