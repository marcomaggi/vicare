;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of built-in record types and condition object types
;;Date: Tue Dec 22, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;


;;;; syntaxes

(define-syntax (define-built-in-record-type input-form.stx)
  (define (main stx)
    (syntax-case stx ()
      ((?kwd ?type-name . ?clauses)
       (let* ((type-name.str	(symbol->string (syntax->datum #'?type-name)))
	      (clause*.stx	(expander::syntax-clauses-unwrap #'?clauses synner))
	      (clause*.stx	(expander::syntax-clauses-collapse clause*.stx))
	      (parsed-specs	(%parse-clauses clause*.stx)))
	 (with-syntax
	     ((UID			(expander::identifier-append #'?kwd "vicare:core-type:" type-name.str))
	      (PARENT			(parsed-specs-parent			parsed-specs))
	      (CONSTRUCTOR		(parsed-specs-constructor		parsed-specs))
	      (DESTRUCTOR		(parsed-specs-destructor		parsed-specs))
	      (TYPE-PREDICATE		(parsed-specs-type-predicate		parsed-specs))
	      (EQUALITY-PREDICATE	(parsed-specs-equality-predicate	parsed-specs))
	      (COMPARISON-PROCEDURE	(parsed-specs-comparison-procedure	parsed-specs))
	      (HASH-FUNCTION		(parsed-specs-hash-function		parsed-specs))
	      (METHODS			(parsed-specs-methods			parsed-specs))
	      (TYPE-RTD			(expander::identifier-append #'?kwd type-name.str "-rtd"))
	      (TYPE-RCD			(expander::identifier-append #'?kwd type-name.str "-rcd")))
	 #'(set-cons! VICARE-CORE-BUILT-IN-RECORD-TYPES-SYNTACTIC-BINDING-DESCRIPTORS
		      (quote (?type-name
			      ($core-record-type-name
			       . #(?type-name UID TYPE-RTD TYPE-RCD PARENT CONSTRUCTOR TYPE-PREDICATE
					      EQUALITY-PREDICATE COMPARISON-PROCEDURE HASH-FUNCTION
					      METHODS))))))))
      (_
       (synner "invalid syntax use"))))

;;; --------------------------------------------------------------------

  (define-constant LIST-OF-CLAUSES
    (expander::syntax-clauses-validate-specs
     (list
      ;; NAME MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
      (expander::make-syntax-clause-spec #'parent			1 1 1 1      '() '())
      (expander::make-syntax-clause-spec #'constructor			0 1 0 1      '() '())
      (expander::make-syntax-clause-spec #'destructor			0 1 0 1      '() '())
      (expander::make-syntax-clause-spec #'type-predicate		0 1 1 1      '() '())
      (expander::make-syntax-clause-spec #'equality-predicate		0 1 1 1      '() '())
      (expander::make-syntax-clause-spec #'comparison-procedure		0 1 1 1      '() '())
      (expander::make-syntax-clause-spec #'hash-function		0 1 1 1      '() '())
      (expander::make-syntax-clause-spec #'methods			0 1 1 +inf.0 '() '()))))

  (define-record-type parsed-specs
    (fields
      (mutable parent)
		;A syntax object representing a type annotation.  It is the parent of
		;the label-type.
      (mutable constructor)
		;A  boolean or  an  identifier representing  the object  constructor.
		;When  #f:  this  object-type  has no  constructor.   When  #t:  this
		;object-type has no constructor, but  the syntax NEW must verify that
		;its single argument is already an instance of this type.
      (mutable destructor)
		;False or an identifier representing the object destructor.  When #f:
		;this object-type has no destructor.
      (mutable type-predicate)
		;False or an identifier representing  the object predicate.  When #f:
		;this object type has no predicate.
      (mutable equality-predicate)
		;False or an identifier representing the equality predicate function.
		;When #f: this object type has no equality predicate.
      (mutable comparison-procedure)
		;False or an identifier  representing the comparison procedure.  When
		;#f: this object type has no comparison procedure.
      (mutable hash-function)
		;False or an identifier representing  the object hash function.  When
		;#f: this object type has no hash function.
      (mutable methods)
		;A possibly empty association vector of method specifications.
      #| end of FIELDS |# )
    (protocol
      (lambda (make-record)
	(lambda ()
	  (make-record #f	;parent
		       #f	;constructor
		       #f	;destructor
		       #f	;type-predicate
		       #f	;equality-predicate
		       #f	;comparison-procedure
		       #f	;hash-function
		       '#()	;methods
		       ))))
    #| end of DEFINE-RECORD-TYPE |# )

  (define (%parse-clauses clause*.stx)
    (expander::syntax-clauses-fold-specs combine (make-parsed-specs) LIST-OF-CLAUSES clause*.stx))

  (define* (combine {parsed-specs parsed-specs?} {clause-spec expander::syntax-clause-spec?} args)
    ;;ARGS  is a  vector of  vectors  holding the  values from  the clauses  matching
    ;;CLAUSE-SPEC.
    ;;
    (define arg (vector-ref args 0))
    (case-identifiers (expander::syntax-clause-spec-keyword clause-spec)
      ((parent)
       (parsed-specs-parent-set! parsed-specs (vector-ref arg 0)))

      ((constructor)
       (if (fxzero? (vector-length arg))
	   (parsed-specs-constructor-set! parsed-specs #f)
	 (let ((id (vector-ref arg 0)))
	   (unless (%boolean-or-id? id)
	     (synner "invalid constructor specification" id))
	   (parsed-specs-constructor-set! parsed-specs id))))

      ((destructor)
       (if (fxzero? (vector-length arg))
	   (parsed-specs-destructor-set! parsed-specs #f)
	 (let ((id (vector-ref arg 0)))
	   (unless (%false-or-id? id)
	     (synner "invalid destructor specification" id))
	   (parsed-specs-destructor-set! parsed-specs id))))

      ((type-predicate)
       (let ((id (vector-ref arg 0)))
	 (unless (%false-or-id? id)
	   (synner "invalid predicate specification" id))
	 (parsed-specs-type-predicate-set! parsed-specs id)))

      ((equality-predicate)
       (let ((id (vector-ref arg 0)))
	 (unless (%false-or-id? id)
	   (synner "invalid equality predicate specification" id))
	 (parsed-specs-equality-predicate-set! parsed-specs id)))

      ((comparison-procedure)
       (let ((id (vector-ref arg 0)))
	 (unless (%false-or-id? id)
	   (synner "invalid comparison procedure specification" id))
	 (parsed-specs-comparison-procedure-set! parsed-specs id)))

      ((hash-function)
       (let ((id (vector-ref arg 0)))
	 (unless (%false-or-id? id)
	   (synner "invalid hash function specification" id))
	 (parsed-specs-hash-function-set! parsed-specs id)))

      ((methods)
       (syntax-case arg ()
	 (#((?method-name ?method-implementation-procedure) ...)
	  (if (and (expander::all-identifiers? #'(?method-name ...))
		   (expander::all-identifiers? #'(?method-implementation-procedure ...)))
	      (parsed-specs-methods-set! parsed-specs #'#((?method-name . ?method-implementation-procedure) ...))
	    (synner "expected identifiers as method names and implementation procedure names")))
	 (_
	  (synner "invalid syntax in METHODS clause" arg))))

      (else
       (synner "invalid syntax clause" (expander::syntax-clause-spec-keyword clause-spec))))
    parsed-specs)

;;; --------------------------------------------------------------------

  (define (%boolean-or-id? obj)
    (or (identifier? obj)
	(boolean?    obj)))

  (define (%false-or-id? obj)
    (or (identifier? obj)
	(not obj)))

  (case-define synner
    ((message)
     (synner message #f))
    ((message subform)
     (syntax-violation (quote define-built-in-record-type) message input-form.stx subform)))

;;; --------------------------------------------------------------------

  (main input-form.stx))


;;;; built-in record types

(define-built-in-record-type <library>
    (parent <record>)
  (constructor make-library)
  (type-predicate library?)
  (methods
   (uid				library-uid)
   (name			library-name)
   (imp-lib*			library-imp-lib*)
   (vis-lib*			library-vis-lib*)
   (inv-lib*			library-inv-lib*)
   (export-subst		library-export-subst)
   (global-env			library-global-env)
   (typed-locs			library-typed-locs)
   (visit-state			library-visit-state)
   (invoke-state		library-invoke-state)
   (visit-code			library-visit-code)
   (invoke-code			library-invoke-code)
   (guard-code			library-guard-code)
   (guard-lib*			library-guard-lib*)
   (visible?			library-visible?)
   (source-file-name		library-source-file-name)
   (option*			library-option*)
   (foreign-library*		library-foreign-library*)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <time>
    (parent <record>)
  (constructor make-time)
  (type-predicate time?)
  (equality-predicate	<time>-equality-predicate)
  (comparison-procedure	<time>-comparison-procedure)
  (hash-function	<time>-hash-function)
  (methods
   (second		time-seconds)
   (nanosecond		time-nanoseconds)
   (ratnum		time-ratnum)
   (flonum		time-flonum)
   (+			time-addition)
   (-			time-difference)
   (=			time=?)
   (!=			time!=?)
   (<			time<?)
   (<=			time<=?)
   (>			time>?)
   (>=			time>=?)
   (min			time-min)
   (max			time-max)))

(define-built-in-record-type <epoch-time>
    (parent <time>)
  (constructor make-epoch-time)
  (type-predicate epoch-time?)
  (equality-predicate <epoch-time>-equality-predicate)
  (comparison-procedure <epoch-time>-comparison-procedure)
  (hash-function <epoch-time>-hash-function)
  (methods
   (+			epoch-time-addition)
   (-			epoch-time-subtraction)))


;;;; built-in record types: expander stuff

(define-built-in-record-type <expander-options>
    (parent <record>)
  (constructor make-expander-options)
  (type-predicate expander-options?))

(define-built-in-record-type <compiler-options>
    (parent <record>)
  (constructor make-compiler-options)
  (type-predicate compiler-options?))

;;; --------------------------------------------------------------------

(define-built-in-record-type <lexical-environment>
    (parent <record>)
  (type-predicate environment?))

(define-built-in-record-type <interaction-lexical-environment>
    (parent <lexical-environment>)
  (constructor new-interaction-environment)
  (type-predicate interaction-lexical-environment?))

(define-built-in-record-type <non-interaction-lexical-environment>
    (parent <lexical-environment>)
  (constructor environment)
  (type-predicate non-interaction-lexical-environment?))

;;; --------------------------------------------------------------------

(define-built-in-record-type <stx>
    (parent <record>)
  (type-predicate stx?)
  (methods
   (expr			stx-expr)
   (mark*			stx-mark*)
   (rib*			stx-rib*)
   (annotated-expr*		stx-annotated-expr*)))

(define-built-in-record-type <syntactic-identifier>
    (parent <stx>)
  (type-predicate syntactic-identifier?)
  (methods
   (string			identifier->string)
   (label			syntactic-identifier->label)))

(define-built-in-record-type <syntax-clause-spec>
    (parent <record>)
  (constructor make-syntax-clause-spec)
  (type-predicate syntax-clause-spec?)
  (methods
   (keyword			syntax-clause-spec-keyword)
   (min-number-of-occurrences	syntax-clause-spec-min-number-of-occurrences)
   (max-number-of-occurrences	syntax-clause-spec-max-number-of-occurrences)
   (min-number-of-arguments	syntax-clause-spec-min-number-of-arguments)
   (max-number-of-arguments	syntax-clause-spec-max-number-of-arguments)
   (mutually-inclusive		syntax-clause-spec-mutually-inclusive)
   (mutually-exclusive		syntax-clause-spec-mutually-exclusive)
   (custom-data			syntax-clause-spec-custom-data)))


;;;; object type specifications: built-in record types

(define-built-in-record-type <object-type-spec>
    (parent <record>)
  (type-predicate object-type-spec?)
  (methods
   (name				object-type-spec.name)
   (uids-list				object-type-spec.uids-list)
   (type-annotation			object-type-spec.type-annotation)
   (parent-ots				object-type-spec.parent-ots)
   (constructor-stx			object-type-spec.constructor-stx)
   (destructor-stx			object-type-spec.destructor-stx)
   (type-predicate-stx			object-type-spec.type-predicate-stx)
   (implemented-interfaces		object-type-spec.implemented-interfaces)
   (equality-predicate			object-type-spec.equality-predicate)
   (comparison-procedure		object-type-spec.comparison-procedure)
   (hash-function			object-type-spec.hash-function)
   (applicable-hash-function		object-type-spec.applicable-hash-function)
   (safe-accessor-stx			object-type-spec.safe-accessor-stx)
   (safe-mutator-stx			object-type-spec.safe-mutator-stx)
   (applicable-method-stx		object-type-spec.applicable-method-stx)
   (single-value-validator-lambda-stx	object-type-spec.single-value-validator-lambda-stx)
   (list-validator-lambda-stx		object-type-spec.list-validator-lambda-stx)
   (procedure?				object-type-spec.procedure?)
   (list-sub-type?			object-type-spec.list-sub-type?)
   (vector-sub-type?			object-type-spec.vector-sub-type?)))

(define-built-in-record-type <core-type-spec>
    (parent <object-type-spec>)
  (type-predicate core-type-spec?)
  (methods
   (type-descriptor-id		core-type-spec.type-descriptor-id)
   (parent-and-child?		core-type-spec.parent-and-child?)))

(define-built-in-record-type <closure-type-spec>
    (parent <object-type-spec>)
  (constructor make-closure-type-spec)
  (type-predicate closure-type-spec?)
  (methods
   (signature			closure-type-spec.signature)))

(define-built-in-record-type <struct-type-spec>
    (parent <object-type-spec>)
  (constructor make-struct-type-spec)
  (type-predicate struct-type-spec?)
  (methods
   (std				struct-type-spec.std)))

(define-built-in-record-type <record-type-spec>
    (parent <object-type-spec>)
  (constructor make-record-type-spec)
  (type-predicate record-type-spec?)
  (methods
   (rtd-id				record-type-spec.rtd-id)
   (rcd-id				record-type-spec.rcd-id)
   (parent-and-child?			record-type-spec.parent-and-child?)))

(define-built-in-record-type <compound-condition-type-spec>
    (parent <objct-type-spec>)
  (constructor make-compound-condition-type-spec)
  (type-predicate compound-condition-type-spec?)
  (methods
   (component-ots*	compound-condition-type-spec.component-ots*)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <union-type-spec>
    (parent <object-type-spec>)
  (type-predicate union-type-spec?)
  (methods
   (item-ots*			union-type-spec.item-ots*)))

(define-built-in-record-type <intersection-type-spec>
    (parent <object-type-spec>)
  (type-predicate intersection-type-spec?)
  (methods
   (item-ots*			intersection-type-spec.item-ots*)))

(define-built-in-record-type <complement-type-spec>
    (parent <object-type-spec>)
  (constructor make-complement-type-spec)
  (type-predicate complement-type-spec?)
  (methods
   (item-ots			complement-type-spec.item-ots)))

(define-built-in-record-type <ancestor-of-type-spec>
    (parent <object-type-spec>)
  (constructor make-ancestor-of-type-spec)
  (type-predicate ancestor-of-type-spec?)
  (methods
   (item-ots			ancestor-of-type-spec.item-ots)
   (ancestor-ots*		ancestor-of-type-spec.ancestor-ots*)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <pair-type-spec>
    (parent <object-type-spec>)
  (constructor make-pair-type-spec)
  (type-predicate pair-type-spec?)
  (methods
   (car-ots			pair-type-spec.car-ots)
   (cdr-ots			pair-type-spec.cdr-ots)))

(define-built-in-record-type <pair-of-type-spec>
    (parent <object-type-spec>)
  (constructor make-pair-of-type-spec)
  (type-predicate pair-of-type-spec?)
  (methods
   (item-ots			pair-of-type-spec.item-ots)))

(define-built-in-record-type <list-type-spec>
    (parent <object-type-spec>)
  (constructor make-list-type-spec)
  (type-predicate list-type-spec?)
  (methods
   (item-ots*		list-type-spec.item-ots*)
   (length		list-type-spec.length)))

(define-built-in-record-type <list-of-type-spec>
    (parent <object-type-spec>)
  (constructor make-list-of-type-spec)
  (type-predicate list-of-type-spec?)
  (methods
   (item-ots			list-of-type-spec.item-ots)))

(define-built-in-record-type <vector-type-spec>
    (parent <object-type-spec>)
  (constructor make-vector-type-spec)
  (type-predicate vector-type-spec?)
  (methods
   (item-ots*		vector-type-spec.item-ots*)
   (length		vector-type-spec.length)))

(define-built-in-record-type <vector-of-type-spec>
    (parent <object-type-spec>)
  (constructor make-vector-of-type-spec)
  (type-predicate vector-of-type-spec?)
  (methods
   (item-ots			vector-of-type-spec.item-ots)))

(define-built-in-record-type <nevector-of-type-spec>
    (parent <object-type-spec>)
  (constructor make-nevector-of-type-spec)
  (type-predicate nevector-of-type-spec?)
  (methods
   (item-ots			nevector-of-type-spec.item-ots)))

(define-built-in-record-type <hashtable-type-spec>
    (parent <object-type-spec>)
  (constructor make-hashtable-type-spec)
  (type-predicate hashtable-type-spec?)
  (methods
   (key-ots			hashtable-type-spec.key-ots)
   (value-ots			hashtable-type-spec.val-ots)))

(define-built-in-record-type <alist-type-spec>
    (parent <object-type-spec>)
  (constructor make-alist-type-spec)
  (type-predicate alist-type-spec?)
  (methods
   (key-ots			alist-type-spec.key-ots)
   (value-ots			alist-type-spec.val-ots)))

(define-built-in-record-type <enumeration-type-spec>
    (parent <object-type-spec>)
  (constructor make-enumeration-type-spec)
  (type-predicate enumeration-type-spec?)
  (methods
   (symbol*			enumeration-type-spec.symbol*)
   (member?			enumeration-type-spec.member?)))

(define-built-in-record-type <label-type-spec>
    (parent <object-type-spec>)
  (type-predicate label-type-spec?))

(define-built-in-record-type <interface-type-spec>
    (parent <object-type-spec>)
  (type-predicate interface-type-spec?)
  (methods
   (method-prototypes-table	interface-type-spec.method-prototypes-table)
   (type-descriptor-id		interface-type-spec.type-descriptor-id)))

(define-built-in-record-type <reference-type-spec>
    (parent <object-type-spec>)
  (constructor make-reference-type-spec)
  (type-predicate reference-type-spec?)
  (methods
   (dereference			reference-type-spec.dereference)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <type-signature>
    (parent <record>)
  (constructor make-type-signature)
  (type-predicate type-signature?)
  (methods
   (object-type-specs			type-signature.object-type-specs)
   (syntax-object			type-signature.syntax-object)
   (fully-untyped?			type-signature.fully-untyped?)
   (partially-untyped?			type-signature.partially-untyped?)
   (untyped?				type-signature.untyped?)
   (empty?				type-signature.empty?)
   (super-and-sub?			type-signature.super-and-sub?)
   (compatible-super-and-sub?		type-signature.compatible-super-and-sub?)
   (single-type?			type-signature.single-type?)
   (single-top-tag?			type-signature.single-top-tag?)
   (single-type-or-fully-untyped?	type-signature.single-type-or-fully-untyped?)
   (no-return?				type-signature.no-return?)
   (match-formals-against-operands	type-signature.match-formals-against-operands)
   (min-count				type-signature.min-count)
   (max-count				type-signature.max-count)
   (min-and-max-counts			type-signature.min-and-max-counts)
   (common-ancestor			type-signature.common-ancestor)
   (union				type-signature.union)))


;;;; built-in record types: object type descriptors

(define-built-in-record-type <core-type-descriptor>
    (parent <record>)
  (constructor make-core-type-descriptor)
  (type-predicate core-type-descriptor?)
  (methods
   (name				core-type-descriptor.name)
   (parent				core-type-descriptor.parent)
   (type-predicate			core-type-descriptor.type-predicate)
   (equality-predicate			core-type-descriptor.equality-predicate)
   (comparison-procedure		core-type-descriptor.comparison-procedure)
   (hash-function			core-type-descriptor.hash-function)
   (uid					core-type-descriptor.uid)
   (uids-list				core-type-descriptor.uids-list)
   (method-retriever			core-type-descriptor.method-retriever)
   (implemented-interfaces		core-type-descriptor.implemented-interfaces)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <descriptors-signature>
    (parent <record>)
  (constructor make-descriptors-signature)
  (type-predicate descriptors-signature?)
  (methods
   (object-type-descrs		descriptors-signature.object-type-descrs)))

(define-built-in-record-type <lambda-descriptors>
    (parent <record>)
  (constructor make-lambda-descriptors)
  (type-predicate lambda-descriptors?)
  (methods
   (retvals			lambda-descriptors.retvals)
   (argvals			lambda-descriptors.argvals)))

(define-built-in-record-type <case-lambda-descriptors>
    (parent <record>)
  (constructor make-case-lambda-descriptors)
  (type-predicate case-lambda-descriptors?)
  (methods
   (clause-signature*		case-lambda-descriptors.clause-signature*)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <compound-condition-type-descr>
    (parent <record>)
  (constructor make-compound-condition-type-descr)
  (type-predicate compound-condition-type-descr?)
  (methods
   (component-des*		compound-condition-type-descr.component-des*)))

(define-built-in-record-type <hashtable-type-descr>
    (parent <record>)
  (constructor make-hashtable-type-descr)
  (type-predicate hashtable-type-descr?)
  (methods
   (key-des			hashtable-type-descr.key-des)
   (val-des			hashtable-type-descr.val-des)))

(define-built-in-record-type <alist-type-descr>
    (parent <record>)
  (constructor make-alist-type-descr)
  (type-predicate alist-type-descr?)
  (methods
   (key-des			alist-type-descr.key-des)
   (val-des			alist-type-descr.val-des)))

(define-built-in-record-type <enumeration-type-descr>
    (parent <record>)
  (constructor make-enumeration-type-descr)
  (type-predicate enumeration-type-descr?)
  (methods
   (symbol*		enumeration-type-descr.symbol*)
   (length		enumeration-type-descr.length)))

(define-built-in-record-type <closure-type-descr>
    (parent <record>)
  (constructor make-closure-type-descr)
  (type-predicate closure-type-descr?)
  (methods
   (signature		closure-type-descr.signature)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <pair-type-descr>
    (parent <record>)
  (constructor make-pair-type-descr)
  (type-predicate pair-type-descr?)
  (methods
   (car-des		pair-type-descr.car-des)
   (cdr-des		pair-type-descr.cdr-des)))

(define-built-in-record-type <pair-of-type-descr>
    (parent <record>)
  (constructor make-pair-of-type-descr)
  (type-predicate pair-of-type-descr?)
  (methods
   (item-des		pair-of-type-descr.item-des)))

(define-built-in-record-type <list-type-descr>
    (parent <record>)
  (constructor make-list-type-descr)
  (type-predicate list-type-descr?)
  (methods
   (item-des*		list-type-descr.item-des*)
   (length		list-type-descr.length)))

(define-built-in-record-type <list-of-type-descr>
    (parent <record>)
  (constructor make-list-of-type-descr)
  (type-predicate list-of-type-descr?)
  (methods
   (item-des		list-of-type-descr.item-des)))

(define-built-in-record-type <vector-type-descr>
    (parent <record>)
  (constructor make-vector-type-descr)
  (type-predicate vector-type-descr?)
  (methods
   (item-des*		vector-type-descr.item-des*)
   (length		vector-type-descr.length)))

(define-built-in-record-type <vector-of-type-descr>
    (parent <record>)
  (constructor make-vector-of-type-descr)
  (type-predicate vector-of-type-descr?)
  (methods
   (item-des		vector-of-type-descr.item-des)))

(define-built-in-record-type <nevector-of-type-descr>
    (parent <record>)
  (constructor make-nevector-of-type-descr)
  (type-predicate nevector-of-type-descr?)
  (methods
   (item-des		nevector-of-type-descr.item-des)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <ancestor-of-type-descr>
    (parent <record>)
  (constructor make-ancestor-of-type-descr)
  (type-predicate ancestor-of-type-descr?)
  (methods
   (item-des		ancestor-of-type-descr.item-des)
   (ancestors-des*	ancestor-of-type-descr.ancestors-des*)))

(define-built-in-record-type <interface-type-descr>
    (parent <record>)
  (constructor make-interface-type-descr)
  (type-predicate interface-type-descr?)
  (methods
   (type-name			interface-type-descr.type-name)
   (uid				interface-type-descr.uid)
   (implemented-interface-uids	interface-type-descr.implemented-interface-uids)
   (parent-type-descriptor	interface-type-descr.parent-type-descriptor)
   (method-prototype-names	interface-type-descr.method-prototype-names)
   (method-retriever		interface-type-descr.method-retriever)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <union-type-descr>
    (parent <record>)
  (constructor make-union-type-descr)
  (type-predicate union-type-descr?)
  (methods
   (item-des*		union-type-descr.item-des*)))

(define-built-in-record-type <intersection-type-descr>
    (parent <record>)
  (constructor make-intersection-type-descr)
  (type-predicate intersection-type-descr?)
  (methods
   (item-des*		intersection-type-descr.item-des*)))

(define-built-in-record-type <complement-type-descr>
    (parent <record>)
  (constructor make-complement-type-descr)
  (type-predicate complement-type-descr?)
  (methods
   (item-des		complement-type-descr.item-des)))


;;;; built-in record types: overloaded functions descriptors

(define-built-in-record-type <overloaded-function-descriptor>
    (parent <record>)
  (constructor make-overloaded-function-descriptor)
  (type-predicate overloaded-function-descriptor?)
  (methods
   (register!			overloaded-function-descriptor.register!)
   (select-matching-entry	overloaded-function-descriptor.select-matching-entry)))


;;;; done


;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
