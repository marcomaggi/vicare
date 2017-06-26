;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: compile-time property definitions for core primitives
;;;Date: Mon Sep 22, 2014
;;;
;;;Abstract
;;;
;;;	The purpose of this module is to  associate values to the public name of core
;;;	primitive.  The values represent core  primitive properties: the arity of the
;;;	primitive; the  number of  returned values;  the core  types of  the expected
;;;	arguments; the  core types of  the returned values;  miscellaneous properties
;;;	used by the source optimiser.
;;;
;;;	  Scheme  object's core  types  are  defined by  the  module "Scheme  objects
;;;	ontology".  This file contains a table  of core primitive properties for both
;;;	primitive functions and primitive operations.
;;;
;;;Copyright (C) 2014, 2015, 2016, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
(library (ikarus.compiler.core-primitive-properties.base)
  (export
    core-primitive-name->core-type-tag
    core-primitive-name->application-attributes*
    core-primitive-name->core-type-signature*
    core-primitive-name->replacement*
    tuple-tags-arity
    tuple-tags-rest-objects-tag
    tuple-tags-ref
    application-attributes-operands-template
    application-attributes-foldable?
    application-attributes-effect-free?
    application-attributes-result-true?
    application-attributes-result-false?
    application-attributes-identity?
    CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES

    declare-core-primitive
    safe unsafe
    signatures attributes replacements
    foldable effect-free result-true result-false

    section /section

    declare-type-predicate
    declare-type-predicate/maybe
    declare-type-predicate/list
    declare-type-predicate/false
    define-object-predicate-declarer
    declare-object-predicate
    declare-fixnum-predicate
    declare-bignum-predicate
    declare-flonum-predicate
    declare-ratnum-predicate
    declare-compnum-predicate
    declare-cflonum-predicate
    declare-number-predicate
    declare-char-predicate
    declare-string-predicate
    declare-keyword-predicate
    declare-vector-predicate
    declare-bytevector-predicate
    declare-struct-predicate
    declare-record-predicate
    declare-port-predicate

    define-object-binary-comparison-declarer
    declare-object-binary-comparison
    declare-fixnum-binary-comparison
    declare-flonum-binary-comparison
    declare-number-binary-comparison
    declare-pointer-binary-comparison
    declare-char-binary-comparison
    declare-string-binary-comparison
    declare-keyword-binary-comparison
    declare-bytevector-binary-comparison

    define-object-unary/multi-comparison-declarer
    declare-number-unary/multi-comparison
    declare-fixnum-unary/multi-comparison
    declare-flonum-unary/multi-comparison
    declare-string-unary/multi-comparison
    declare-pointer-unary/multi-comparison
    declare-boolean-unary/multi-comparison

    define-object-binary/multi-comparison-declarer
    declare-number-binary/multi-comparison
    declare-char-binary/multi-comparison
    declare-string-binary/multi-comparison
    declare-boolean-binary/multi-comparison

    define-object-unary-operation-declarer
    declare-number-unary
    declare-fixnum-unary
    declare-flonum-unary
    declare-exact-integer-unary
    declare-char-unary
    declare-string-unary

    define-object-binary-operation-declarer
    declare-number-binary
    declare-fixnum-binary
    declare-flonum-binary
    declare-string-binary

    define-object-unary/binary-operation-declarer
    declare-number-unary/binary
    declare-fixnum-unary/binary
    declare-flonum-unary/binary
    declare-string-unary/binary

    define-object-unary/multi-operation-declarer
    declare-number-unary/multi
    declare-fixnum-unary/multi
    declare-flonum-unary/multi
    declare-string-unary/multi

    define-object-multi-operation-declarer
    declare-number-multi
    declare-fixnum-multi
    declare-flonum-multi
    declare-string-multi

    declare-unsafe-unary-operation
    declare-unsafe-unary-operation/2rv
    declare-unsafe-binary-operation
    declare-unsafe-binary-operation/2rv

    declare-pair-accessor
    declare-pair-mutator
    declare-alist-accessor
    declare-list-finder
    declare-safe-bytevector-conversion
    declare-unsafe-bytevector-accessor
    declare-unsafe-bytevector-mutator
    declare-unsafe-bytevector-conversion

    declare-parameter
    declare-object-retriever)
  (import (rnrs)
    ;;NOTE Here  we should  import only "(ikarus.compiler.*)"  libraries.  But  we do
    ;;not.
    (ikarus.compiler.compat)
    (only (vicare system $fx)
	  $fxzero?
	  $fxand
	  $fxsub1)
    (only (vicare system $pairs)
	  $car
	  $cdr)
    (only (vicare system $vectors)
	  $vector-length
	  $vector-ref)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)


;;;; helpers

(define-syntax (expand-time-gensym stx)
  ;;Generate a gensym at expand time and expand to the quoted symbol.
  ;;
  (syntax-case stx ()
    ((_ ?template)
     (let* ((tmp (syntax->datum #'?template))
	    (fxs (vector->list (foreign-call "ikrt_current_time_fixnums_2")))
	    (str (apply string-append tmp (map (lambda (N)
						 (string-append "." (number->string N)))
					    fxs)))
	    (sym (gensym str)))
       (with-syntax
	   ((SYM (datum->syntax #'here sym)))
	 (fprintf (current-error-port) "expand-time gensym ~a\n" sym)
	 #'(quote SYM))))))


;;;; properties from core primitive public names

;;This API puts the structure holding  core primitive properties in the property list
;;of core primitive's public names.
;;
(begin
  (define-constant CORE-PRIMITIVE-PROPKEY
    (expand-time-gensym "applicable-core-primitive-properties"))

  (define-syntax-rule (associate-prim-name-with-prim-props ?prim-name ?prim-props)
    (putprop (quote ?prim-name) CORE-PRIMITIVE-PROPKEY ?prim-props))

  (define-syntax-rule (retrieve-prim-props-from-prim-name ?prim-name)
    (getprop ?prim-name CORE-PRIMITIVE-PROPKEY))

  #| end of BEGIN |# )

;;This API puts the structure holding core  primitive properties in the VALUE slot of
;;core primitive's public names.
;;
;; (begin
;;   (define-syntax-rule (associate-prim-name-with-prim-props ?prim-name ?prim-props)
;;     (set-symbol-value! (quote ?prim-name) ?prim-props))
;;
;;   (define-syntax-rule (retrieve-prim-props-from-prim-name ?prim-name)
;;     (and (symbol-bound? ?prim-name)
;; 	 (symbol-value  ?prim-name)))
;;
;;   #| end of BEGIN |# )

;;; --------------------------------------------------------------------

(define (core-primitive-name->core-type-tag prim-name)
  ;;Given a symbol  representing the name of a core  primitive: return the associated
  ;;CORE-TYPE-TAG value.  As default return "T:object".
  ;;
  ;;At present, given a  symbol representing the public name of  a core primitive, we
  ;;need to distinguish between:
  ;;
  ;;1. Core primitive procedures and core primitive operations.
  ;;
  ;;2. Core primitive non-procedures and non-operations.
  ;;
  (cond ((retrieve-prim-props-from-prim-name prim-name)
	 => (lambda (P)
	      (if (applicable-core-primitive-properties? P)
		  T:procedure
		T:object)))
	(else T:object)))

(define* (core-primitive-name->application-attributes* {prim-name symbol?})
  ;;Return the APPLICATION-ATTRIBUTES* list of the core primitive PRIM-NAME; return
  ;;false if PRIM-NAME has  no attributes associated or it is  not a core primitive
  ;;name.
  ;;
  (cond ((retrieve-prim-props-from-prim-name prim-name)
	 => applicable-core-primitive-properties-application-attributes*)
	(else #f)))

(define* (core-primitive-name->core-type-signature* {prim-name symbol?})
  ;;Return the  SIGNATURE* list of  the core  primitive PRIM-NAME; return  false if
  ;;PRIM-NAME has no registered signatures or it is not a core primitive name.
  ;;
  (cond ((retrieve-prim-props-from-prim-name prim-name)
	 => applicable-core-primitive-properties-core-type-signature*)
	(else #f)))

(define* (core-primitive-name->replacement* {prim-name symbol?})
  ;;Return the REPLACEMENT*  list of the core primitive PRIM-NAME;  return false if
  ;;PRIM-NAME has no registered replacements or it is not a core primitive name.
  ;;
  (cond ((retrieve-prim-props-from-prim-name prim-name)
	 => applicable-core-primitive-properties-replacement*)
	(else #f)))


;;;; core primitive properties representation
;;
;;It  would be  really neat  to use  proper type  definitions to  represent the  core
;;primitive properties, for example:
;;
;;   (define-struct applicable-core-primitive-properties
;;     (safe? core-type-signature* application-attributes* replacement*))
;;
;;But initialisation  speed, usage speed  and memory consumption are  also important,
;;given that there are is a big number of core primitives.  So we opt for core Scheme
;;object types, which  can be easily precomputed  and stored in fasl  files as simple
;;constants.  This  means defining a  number of  syntaxes, rather than  normal DEFINE
;;bindings,  so  that the  most  constant  values  are  inlined even  across  library
;;boundaries.
;;
;;(Marco Maggi; Fri Dec 5, 2014)
;;

(define-syntax-rule (make-applicable-core-primitive-properties safe? core-type-signature* application-attributes* replacement*)
  `#(,safe? ,core-type-signature* ,application-attributes* ,replacement*))

(define-syntax-rule (applicable-core-primitive-properties? ?obj)
  ;;This is quite weak: it can easily return wrong results.
  ;;
  (vector? ?obj))

(define-syntax-rule (applicable-core-primitive-properties-safe? ?cpp)
  ;;Boolean.  True if this core primitive is safe.
  ;;
  ($vector-ref ?cpp 0))

(define-syntax-rule (applicable-core-primitive-properties-core-type-signature* ?cpp)
  ;;A list of pairs with the format:
  ;;
  ;;   ((?operands-tags . ?return-values-tags) ...)
  ;;
  ;;in which both ?OPERANDS-TAGS and ?RETURN-VALUES-TAGS are proper or improper lists
  ;;of  type tags.   Every pair  represents an  alternative core  type signature  for
  ;;operands and return values.
  ;;
  ($vector-ref ?cpp 1))

(define-syntax-rule (applicable-core-primitive-properties-application-attributes* ?cpp)
  ;;A proper list:
  ;;
  ;;   (?application-attributes ...)
  ;;
  ;;in which every ?APPLICATION-ATTRIBUTES is a value of type APPLICATION-ATTRIBUTES.
  ;;
  ($vector-ref ?cpp 2))

(define-syntax-rule (applicable-core-primitive-properties-replacement* ?cpp)
  ;;List of  symbols representing public  names of  core primitives that  can replace
  ;;this one if the operands are of the correct type.
  ;;
  ($vector-ref ?cpp 3))


;;;; tuple tags
;;
;;A "tuple-tags" value represents the core type tags signature of a tuple of objects;
;;it is used to represent:
;;
;;*  The core  type  tags of  operands  in  a core  primitive  function or  operation
;;application.
;;
;;* The core type  tags of return values resulting from a  core primitive function or
;;operation application.
;;
;;Examples of TUPLE-TAGS values for operands:
;;
;;(fx+ ?fx1 ?fx2) --> `#(#f ,T:fixnum ,T:fixnum)
;;
;;   The core primitive FX+ accepts 2 mandatory arguments and no optional arguments.
;;
;;(+ ?number ...) --> `#(,T:number)
;;
;;   The core  primitive + accepts 0  mandatory arguments and any  number of optional
;;   arguments.
;;
;;(- ?number0 ?number ...) --> `#(,T:number ,T:number)
;;
;;   The core  primitive - accepts  1 mandatory argument  and any number  of optional
;;   arguments.
;;
;;A TUPLE-TAGS value is a Scheme vector with the format:
;;
;;   #(?rest-objects-tags ?mandatory-object-tag ...)
;;
;;the standard LAMBDA formals:
;;
;;   (?mandatory-arg ...)
;;   (?mandatory-arg ... . ?rest-arg)
;;   ?args
;;
;;are represented as:
;;
;;   #(#f ?mandatory-arg-tag ...)
;;   #(?rest-arg-tag ?mandatory-arg-tag ...)
;;   #(?args-tag)
;;

(define-syntax-rule (tuple-tags-arity ?tags)
  ;;Given a TUPLE-TAGS value: return the  number of mandatory objects needed to match
  ;;it.
  ;;
  ($fxsub1 ($vector-length ?tags)))

(define-syntax-rule (tuple-tags-rest-objects-tag ?tags)
  ;;Given a TUPLE-TAGS value:
  ;;
  ;;*  If the  tuple matches  any number  of objects:  return a  core type  tag value
  ;;representing the type of the objects.
  ;;
  ;;* If the tuple matches a fixed number of objects: return false.
  ;;
  ($vector-ref ?tags 0))

(define-syntax-rule (tuple-tags-ref ?tags ?index)
  ($vector-ref ?tags (fxadd1 ?index)))


;;;; core primitive application attributes

(define-syntax-rule (make-application-attributes ?operands-template ?attributes)
  (cons ?operands-template ?attributes))

;;; --------------------------------------------------------------------

(define-inline-constant FOLDABLE-MASK		#b00000001)
(define-inline-constant EFFECT-FREE-MASK	#b00000010)
(define-inline-constant RESULT-TRUE-MASK	#b00000100)
(define-inline-constant RESULT-FALSE-MASK	#b00001000)
(define-inline-constant IDENTITY-MASK		#b00010000)

(define-syntax-rule (application-attributes-operands-template ?attr)
  ;;A proper or improper list of items representing a template of application for the
  ;;core primitive.  The items are: "_" representing any operand; "0" representing an
  ;;operand  equal to  the fixnum  zero; "#f"  representing an  operand equal  to the
  ;;boolean false; "()" representing an operand equal to null.
  ;;
  (car ?attr))

(define-syntax-rule (application-attributes-foldable? ?attr)
  ;;Boolean.  True if the associated core primitive application can be precomputed at
  ;;compile-time when the operands are constants.
  ;;
  (not ($fxzero? ($fxand ($cdr ?attr) FOLDABLE-MASK))))

(define-syntax-rule (application-attributes-effect-free? ?attr)
  ;;Boolean.  True if the associated core primitive application has no side effects.
  ;;
  (not ($fxzero? ($fxand ($cdr ?attr) EFFECT-FREE-MASK))))

(define-syntax-rule (application-attributes-result-true? ?attr)
  ;;Boolean.  True if the associated  core primitive application always has non-false
  ;;as return value.
  ;;
  (not ($fxzero? ($fxand ($cdr ?attr) RESULT-TRUE-MASK))))

(define-syntax-rule (application-attributes-result-false? ?attr)
  ;;Boolean.  True if  the associated core primitive application always  has false as
  ;;return value.
  ;;
  (not ($fxzero? ($fxand ($cdr ?attr) RESULT-FALSE-MASK))))

(define-syntax-rule (application-attributes-identity? ?attr)
  ;;Boolean.  True if the associated core  primitive accepts a single argument and it
  ;;is an identity, returning its argument.
  ;;
  (not ($fxzero? ($fxand ($cdr ?attr) IDENTITY-MASK))))

;;; --------------------------------------------------------------------

(define-constant CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES
  (make-application-attributes '_ 0))


;;;; syntaxes for parsing tables

(define-auxiliary-syntaxes
  safe unsafe
  signatures attributes replacements
  foldable effect-free result-true result-false)

(define-syntax (declare-core-primitive input-form.stx)
  (case-define synner
    ((message)
     (syntax-violation (quote declare-core-primitive) message input-form.stx #f))
    ((message subform)
     (syntax-violation (quote declare-core-primitive) message input-form.stx subform)))

  (define (main stx)
    (syntax-case stx ()
      ((?ctx ?prim-name . ?clause*)
       (let ((prim-name (%parse-prim-name #'?prim-name)))
	 (receive (safe? signature* attribute* replacement-prim-name*)
	     (%parse-clauses #'?clause*)
	   #`(associate-prim-name-with-prim-props ?prim-name
						  (make-applicable-core-primitive-properties
						   #,safe?
						   (quasiquote #,signature*)
						   (quote #,attribute*)
						   (quote #,(list->vector replacement-prim-name*))))
	   )))
      (_
       (synner "invalid syntax in applicable core primitive declaration"))))

;;; --------------------------------------------------------------------
;;; input form parsers

  (define (%parse-prim-name stx)
    (if (identifier? stx)
	stx
      (synner "expected identifier as applicable core primitive name" stx)))

  (module (%parse-clauses)

    (define (%parse-clauses clause*.stx)
      (syntax-case clause*.stx (safe unsafe)
	(((safe)   . ?other-clause*)
	 (%parse-rest-clauses #'?other-clause* #t))
	(((unsafe) . ?other-clause*)
	 (%parse-rest-clauses #'?other-clause* #f))
	(_
	 (synner "first clause must be a safety specification, \"(safe)\" or \"(unsafe)\""
		 clause*.stx))))

    (define %parse-rest-clauses
      (case-lambda
	((clause*.stx safe?)
	 (%parse-rest-clauses clause*.stx safe? '() '() '()))
	((clause*.stx safe? signature* attribute* replacement-prim-name*)
	 (syntax-case clause*.stx (signatures attributes replacements)
	   (()
	    (values safe? signature* attribute* replacement-prim-name*))

	   (((signatures ?signature ...) . ?other-clause*)
	    (%parse-rest-clauses #'?other-clause* safe?
				 (append signature* (%parse-signatures-sexp #'(?signature ...)))
				 attribute*
				 replacement-prim-name*))

	   (((attributes ?attr-spec ...) . ?other-clause*)
	    (%parse-rest-clauses #'?other-clause* safe?
				 signature*
				 (append attribute* (%parse-application-attributes-sexp #'(?attr-spec ...)))
				 replacement-prim-name*))

	   (((replacements ?replacement-prim-name ...) . ?other-clause*)
	    (%parse-rest-clauses #'?other-clause* safe?
				 signature*
				 attribute*
				 (append replacement-prim-name* (%parse-replacements-sexp #'(?replacement-prim-name ...)))))

	   ((?bad-clause . ?other-clause*)
	    (synner "invalid clause" #'?bad-clause))))))

    #| end of module: %PARSE-CLAUSES |# )

;;; --------------------------------------------------------------------

  (define (%parse-signatures-sexp stx)
    ;;A SIGNATURES form has the format:
    ;;
    ;;   (signatures ?signature ...)
    ;;
    ;;where each ?SIGNATURE has the format:
    ;;
    ;;   (?operands-types => ?return-values-types)
    ;;
    ;;in which both  ?OPERANDS-TYPES and ?RETURN-VALUES-TYPES are  proper or improper
    ;;lists of logic expressions with the format:
    ;;
    ;;   ?logic-expr = ?type-tag-id
    ;;               | (and ?logic-expr0 ?logic-expr ...)
    ;;               | (or  ?logic-expr0 ?logic-expr ...)
    ;;
    ;;representing the core type of an argument or return value; the type identifiers
    ;;are defined by the Scheme objects ontology.  The identifiers "_" and "T:object"
    ;;are wildcards meaning "an object of any  type".  The identifiers AND and OR are
    ;;the ones exported by "(rnrs base (6))".
    ;;
    ;;The specification of  the return value types is the  specification of the types
    ;;of the consumer procedure:
    ;;
    ;;   (call-with-values
    ;;       (lambda ()
    ;;         (?call-to-core-primitive))
    ;;     (lambda ?return-values-types
    ;;       (consume-return-values)))
    ;;
    ;;When the list is proper:
    ;;
    ;;   (?rand-type ...)
    ;;   (?rv-type ...)
    ;;
    ;;the primitive  has a  fixed number of  arguments/return values,  possibly zero.
    ;;When the list is improper:
    ;;
    ;;   (?rand-type0 ?rand-type ... . ?rest-arg-type)
    ;;   (?rv-type0   ?rv-type   ... . ?rest-rv-type)
    ;;   ?args-rand-type
    ;;   ?args-rv-type
    ;;
    ;;the primitive has a variable  number of arguments/return values, possibly zero;
    ;;the  ?REST-*-TYPE and  ?ARGS-*-TYPE  represent  the type  of  all the  optional
    ;;arguments/return values.
    ;;
    ;;This function parses the SIGNATURES form, raising a "&syntax" exception in case
    ;;of error, and returns a syntax object representing a proper list of pairs:
    ;;
    ;;   ((?operands-tuple-tags . ?return-values-tuple-tags) ...)
    ;;
    ;;which is meant  to be wrapped into a QUASIQUOTE;  internal unquoted expressions
    ;;will evaluate to  an exact integer representing the naked  bits defining a core
    ;;type specification.
    ;;
    ;;The returned object has "T:object" in place of the wildcard "_".
    ;;
    ;;NOTE The order of  the returned signatures is *the* *same*  order in which they
    ;;appear in the input form!
    ;;
    (define (%syntax-error)
      (synner "invalid signatures specification in core primitive declaration" stx))
    (syntax-case stx ()
      ((?signature ...)
       (map (lambda (signature)
	      (syntax-case signature (=>)
		((?operands-types => ?return-values-types)
		 (cons (%parse-proper-or-improper-list-of-core-types #'?operands-types)
		       (%parse-proper-or-improper-list-of-core-types #'?return-values-types)))
		(_
		 (%syntax-error))))
	 (syntax->list #'(?signature ...))))
      (_
       (%syntax-error))))

  (define (%parse-proper-or-improper-list-of-core-types stx)
    ;;Parse a  proper or  improper list  of logic  type expressions  representing the
    ;;types of  arguments or  return values  for a core  primitive.  Return  a syntax
    ;;object representing an expression that will evaluate to a TUPLE-TAGS value; the
    ;;expression needs to be wrapped into a QUASIQUOTE form.
    ;;
    (define-syntax-rule (%wrap ?type-spec)
      #`(unquote (core-type-tag-bits #,?type-spec)))
    (let loop ((stx   stx)
	       (spec* '()))
      (syntax-case stx (and or)
	((and ?tag0 ?tag ...)
	 ;;It is an improper list having one of the formats:
	 ;;
	 ;;   (?item0 ?item ... . (and ?tag0 ?tag ...))
	 ;;   (and ?tag0 ?tag ...)
	 ;;
	 #`#(#,(%wrap (%parse-core-object-type stx)) #,@(reverse spec*)))

	((or ?tag0 ?tag ...)
	 ;;It is an improper list having one of the formats:
	 ;;
	 ;;   (?item0 ?item ... . (or ?tag0 ?tag ...))
	 ;;   (or ?tag0 ?tag ...)
	 ;;
	 #`#(#,(%wrap (%parse-core-object-type stx)) #,@(reverse spec*)))

	((?car . ?cdr)
	 ;;The list is not finished yet.
	 (loop #'?cdr (cons (%wrap (%parse-core-object-type #'?car)) spec*)))

	(()
	 ;;It is a proper list.
	 #`#(#f #,@(reverse spec*)))

	(?tag
	 (identifier? #'?tag)
	 ;;It is an improper list having one of the formats:
	 ;;
	 ;;   (?item0 ?item ... . ?tag)
	 ;;   ?tag
	 ;;
	 #`#(#,(%wrap (%parse-core-object-type stx)) #,@(reverse spec*)))

	(_
	 (synner "invalid signatures component in core primitive declaration" stx)))))

  (define (%parse-core-object-type type)
    ;;We accept a  core type identifier and  return it; "T:object" and  "_" mean "any
    ;;type".
    (syntax-case type
	( ;;
	 and or

	 T:object		T:other-object		T:immediate	T:nonimmediate
	 T:non-false		T:false			T:true		T:void
	 T:boolean		T:char			T:symbol	T:string
	 T:vector		T:bytevector		T:keyword
	 T:procedure		T:transcoder		T:pointer	T:hashtable
	 T:eof			T:would-block		T:code		T:pathname
	 T:syntax-object	T:identifier

	 T:struct-type-descriptor
	 T:record-type-descriptor	T:record-constructor-descriptor
	 T:struct		T:other-record		T:record	T:other-struct
	 T:enum-set		T:condition		T:library	T:lexical-environment
	 T:stats		T:time			T:promise	T:utsname

	 T:memory-block		T:pointer/memory-block		T:pointer/bytevector

	 T:null			T:standalone-pair	T:non-empty-proper-list
	 T:pair			T:proper-list		T:improper-list

	 T:port			T:textual-port		T:binary-port
	 T:input-port		T:output-port		T:input/output-port
	 T:textual-input-port	T:textual-output-port	T:textual-input/output-port
	 T:binary-input-port	T:binary-output-port	T:binary-input/output-port

	 T:number		T:exact			T:inexact
	 T:fixnum		T:bignum		T:ratnum
	 T:flonum		T:flonum-integer	T:flonum-fractional
	 T:flonum-finite	T:flonum-infinite	T:flonum-nan
	 T:cflonum
	 T:exact-integer	T:integer
	 T:real			T:exact-real
	 T:compnum		T:exact-compnum
	 T:non-real		T:complex

	 T:zero		T:positive	T:negative	T:non-positive		T:non-negative

	 T:zero-fixnum		T:zero-flonum
	 T:positive-fixnum	T:negative-fixnum	T:non-positive-fixnum	T:non-negative-fixnum
	 T:positive-bignum	T:negative-bignum
	 T:positive-flonum	T:negative-flonum	T:non-positive-flonum	T:non-negative-flonum
	 T:positive-exact-integer	T:non-positive-exact-integer
	 T:negative-exact-integer	T:non-negative-exact-integer

	 T:octet		T:byte			T:octet/byte
	 T:sint8		T:uint8
	 T:sint16		T:uint16
	 T:sint32		T:uint32
	 T:sint64		T:uint64
	 T:file-descriptor

	 T:pointer/false	T:string/false		T:number/false		T:fixnum/false)
      (T:object				type)
      (T:other-object			type)
      (T:immediate			type)
      (T:nonimmediate			type)
      (T:non-false			type)
      (T:false				type)
      (T:true				type)
      (T:void				type)
      (T:boolean			type)
      (T:char				type)
      (T:symbol				type)
      (T:string				type)
      (T:null				type)
      (T:standalone-pair		type)
      (T:non-empty-proper-list		type)
      (T:pair				type)
      (T:proper-list			type)
      (T:improper-list			type)
      (T:vector				type)
      (T:bytevector			type)
      (T:keyword			type)
      (T:procedure			type)
      (T:port				type)
      (T:textual-port			type)
      (T:binary-port			type)
      (T:input-port			type)
      (T:output-port			type)
      (T:input/output-port		type)
      (T:textual-input-port		type)
      (T:textual-output-port		type)
      (T:textual-input/output-port	type)
      (T:binary-input-port		type)
      (T:binary-output-port		type)
      (T:binary-input/output-port	type)
      (T:eof				type)
      (T:would-block			type)
      (T:code				type)
      (T:pathname			type)

      (T:struct-type-descriptor		type)
      (T:record-type-descriptor		type)
      (T:record-constructor-descriptor	type)
      (T:struct				type)
      (T:other-struct			type)
      (T:record				type)
      (T:other-record			type)
      (T:enum-set			type)
      (T:condition			type)
      (T:library			type)
      (T:lexical-environment		type)
      (T:stats				type)
      (T:time				type)
      (T:promise			type)
      (T:utsname			type)
      (T:syntax-object			type)
      (T:identifier			type)

      (T:transcoder			type)

      (T:pointer			type)
      (T:memory-block			type)
      (T:pointer/memory-block		type)
      (T:pointer/bytevector		type)

      (T:hashtable			type)
      (T:number				type)
      (T:complex			type)
      (T:exact				type)
      (T:inexact			type)
      (T:fixnum				type)
      (T:bignum				type)
      (T:ratnum				type)
      (T:flonum				type)
      (T:flonum-integer			type)
      (T:flonum-fractional		type)
      (T:flonum-finite			type)
      (T:flonum-infinite		type)
      (T:flonum-nan			type)
      (T:compnum			type)
      (T:cflonum			type)
      (T:zero				type)
      (T:positive			type)
      (T:negative			type)
      (T:non-positive			type)
      (T:non-negative			type)
      (T:exact-integer			type)
      (T:integer			type)
      (T:real				type)
      (T:exact-compnum			type)
      (T:exact-real			type)
      (T:non-real			type)
      (T:zero-fixnum			type)
      (T:positive-fixnum		type)
      (T:negative-fixnum		type)
      (T:non-positive-fixnum		type)
      (T:non-negative-fixnum		type)
      (T:positive-bignum		type)
      (T:negative-bignum		type)
      (T:positive-flonum		type)
      (T:negative-flonum		type)
      (T:zero-flonum			type)
      (T:non-positive-flonum		type)
      (T:non-negative-flonum		type)
      (T:positive-exact-integer		type)
      (T:negative-exact-integer		type)
      (T:non-positive-exact-integer	type)
      (T:non-negative-exact-integer	type)

      (T:octet				type)
      (T:byte				type)
      (T:octet/byte			type)
      (T:sint8				type)
      (T:uint8				type)
      (T:sint16				type)
      (T:uint16				type)
      (T:sint32				type)
      (T:uint32				type)
      (T:sint64				type)
      (T:uint64				type)
      (T:file-descriptor		type)

      (T:pointer/false			type)
      (T:string/false			type)
      (T:number/false			type)
      (T:fixnum/false			type)

      ((and ?tag0 ?tag ...)
       #`(core-type-tag-and . #,(map %parse-core-object-type (syntax->list #'(?tag0 ?tag ...)))))

      ((or  ?tag0 ?tag ...)
       #`(core-type-tag-ior . #,(map %parse-core-object-type (syntax->list #'(?tag0 ?tag ...)))))

      (_
       (if (identifier? type)
	   (if (eq? '_ (syntax->datum type))
	       #'T:object
	     (synner "unknown identifier as core type name in core primitive declaration" type))
	 (synner "expected identifier as core type name in core primitive declaration" type)))
      ))

;;; --------------------------------------------------------------------

  (module (%parse-application-attributes-sexp)

    (define-inline-constant FOLDABLE-BITS	#b00000001)
    (define-inline-constant EFFECT-FREE-BITS	#b00000010)
    (define-inline-constant RESULT-TRUE-BITS	#b00000100)
    (define-inline-constant RESULT-FALSE-BITS	#b00001000)
    (define-inline-constant IDENTITY-BITS	#b00010000)

    (define (%parse-application-attributes-sexp stx)
      ;;Parse   a  syntax   object   representing  the   core  primitive   attributes
      ;;specification.  The format must be:
      ;;
      ;;   (attributes ?attr-spec ...)
      ;;
      ;;in which every ?ATTR-SPEC has the format:
      ;;
      ;;   (?args . ?attr*)
      ;;
      ;;where: ?ARGS is a proper or improper  list of symbols "_", constant #f, 0 and
      ;;null; ?ATTR* is a proper list of the identifiers:
      ;;
      ;;   foldable	effect-free	identity
      ;;   result-true	result-false
      ;;
      ;;Return a list of pairs with the format:
      ;;
      ;;   ((?args . ?attr*) ...)
      ;;
      (define (%syntax-error)
	(synner "invalid attributes specification in core primitive declaration" stx))
      (syntax-case stx ()
	((?attr-spec ...)
	 (map (lambda (attr-spec)
		(syntax-case attr-spec ()
		  ((?args-spec . ?attr*)
		   (cons (%parse-application-attributes-operands-template  #'?args-spec)
			 (%parse-application-attributes-attributes-specification #'?attr*)))
		  (_
		   (%syntax-error))))
	   (syntax->list #'(?attr-spec ...))))
	(_
	 (%syntax-error))))

    (define (%parse-application-attributes-operands-template args-spec)
      ;;Non-tail recursive function.  Parse the arguments specification ARGS-SPEC for
      ;;the attributes of a core primitive  application.  Return a proper or improper
      ;;list representing the arguments specification.
      ;;
      ;;The  attributes arguments  specification  is  a proper  or  improper list  of
      ;;symbols "_",  constant #f, 0  and null: a  symbol "_" represents  an argument
      ;;that  accepts operands  of  any type;  the constants  #f,  0, null  represent
      ;;arguments that accept as operands #f, 0, null respecitely.
      ;;
      (define (%error-invalid-argument-spec)
	(synner "invalid core primitive argument specification for attributes" args-spec))
      (define (%arg-spec? A)
	(or (eq? '_ A)
	    (null? A)
	    (not A)
	    (and (fixnum? A)
		 (fxzero? A))))
      (syntax-case args-spec ()
	((?car . ?cdr)
	 (let ((A (syntax->datum #'?car)))
	   (if (%arg-spec? A)
	       (cons #'?car (%parse-application-attributes-operands-template #'?cdr))
	     (%error-invalid-argument-spec))))
	(()
	 '())
	(?rest
	 (let ((A (syntax->datum #'?rest)))
	   (if (%arg-spec? A)
               #'?rest
	     (%error-invalid-argument-spec))))
	(_
	 (%error-invalid-argument-spec))))

    (define (%parse-application-attributes-attributes-specification attr*)
      (let recur ((flags 0)
		  (attr* attr*))
	(syntax-case attr* (foldable identity effect-free result-true result-false)
	  ((foldable . ?rest)
	   (recur (fxior flags FOLDABLE-BITS) #'?rest))

	  ((effect-free . ?rest)
	   (recur (fxior flags EFFECT-FREE-BITS) #'?rest))

	  ((result-true . ?rest)
	   (recur (fxior flags RESULT-TRUE-BITS) #'?rest))

	  ((result-false . ?rest)
	   (recur (fxior flags RESULT-FALSE-BITS) #'?rest))

	  ((identity . ?rest)
	   (recur (fxior flags IDENTITY-BITS) #'?rest))

	  (()
	   flags)

	  (_
	   (synner "invalid core primitive attributes specification" attr*)))))

    #| end of module: %PARSE-APPLICATION-ATTRIBUTES-SEXP |# )

;;; --------------------------------------------------------------------

  (define (%parse-replacements-sexp stx)
    (syntax-case stx ()
      ((?replacement-prim-name ...)
       (receive-and-return (replacement-prim-name*)
	   (syntax->list #'(?replacement-prim-name ...))
	 (for-each (lambda (id)
		     (unless (identifier? id)
		       (synner "expected identifier as replacement name in core primitive declaration" id)))
	   replacement-prim-name*)))
      (_
       (synner "invalid replacements specification in core primitive declaration" stx))))

;;; --------------------------------------------------------------------

  ;;This is the last form in the definition of DECLARE-CORE-PRIMITIVE.
  ;; (receive-and-return (output-form)
  ;;     (main input-form.stx)
  ;;   (debug-print (syntax->datum output-form)))
  (main input-form.stx))


;;;Core type signatures
;;;--------------------
;;;
;;;Used by  the compiler  passes "core type  inference", "introduce  unsafe primitive
;;;references" and "specify representation" to:
;;;
;;;*  Validate the  input  code, detecting  operands  of invalid  core  type in  core
;;;primitive  applications.
;;;
;;;*  Substitute  the  application  of   safe  primitives  with  application  of  the
;;;corresponding unsafe  primitives, whenever  it is known  at compile-time  that the
;;;operands are of the correct type.
;;;
;;;*  Generate  optimised  integrated  code  to implement  the  application  of  core
;;;primitive operations.
;;;
;;;Core  type  signatures  are   specified  via  the  ?CORE-SIGNATURES-SPEC  symbolic
;;;expression:
;;;
;;;   ?core-signatures-spec  = (signatures ?sign-spec ...)
;;;   ?sign-spec             = (?arguments-spec => ?return-values-spec)
;;;   ?arguments-spec        = (?core-type-tag ... . ())
;;;                          | (?core-type-tag ... . ?core-type-tag)
;;;   ?return-values-spec    = (?core-type-tag ... . ())
;;;                          | (?core-type-tag ... . ?core-type-tag)
;;;
;;;where SIGNATURES is an auxiliary syntax and ?CORE-TYPE-TAG is an identifier among:
;;;"T:object", "T:fixnum", ..., with "_" acting as wildcard equivalent to "T:object".
;;;
;;;Whenever a  core type signature  is examined by  the compiler: the  ?SIGN-SPEC are
;;;considered in the order  in which they appear in the  declaration (left to right),
;;;stopping at the first that matches the operands.
;;;
;;;Example:
;;;
;;;   (signatures
;;;	((T:fixnum)	=> (T:true))
;;;	((_)		=> (T:boolean)))
;;;
;;;this ?CORE-SIGNATURES-SPEC specifies that:
;;;
;;;* When the core primitive is applied  to a single operand of type "T:fixnum" there
;;;is a single return value of type "T:true".
;;;
;;;* When the core primitive is applied  to a single operand of unspecified type (but
;;;not "T:fixnum") there is a single return value of type "T:boolean".
;;;


;;;Core primitive attributes
;;;-------------------------
;;;
;;;Used  by  the  source  optimiser  to  precompute  the  result  of  core  primitive
;;;applications.   A tuple  of  attributes  can be  specified  for  a core  primitive
;;;invocation with  selected number  of operands;  if a primitive  can be  applied to
;;;different  numbers  of  operands,  each  arity  can  have  a  different  tuple  of
;;;attributes.
;;;
;;;Attributes are specified via the ?ATTRIBUTES-SPEC symbolic expression:
;;;
;;;   ?attributes-spec  = (attributes ?attr-spec)
;;;   ?attr-spec        = (?attr-signature . ?attr-tuple)
;;;   ?attr-signature   = (?operand-spec ... . ?operand-spec)
;;;   ?operand-spec     = _ | 0 | #f | ()
;;;   ?attr-tuple       = (?attr-symbol ...)
;;;   ?attr-symbol      = effect-free | foldable | result-true | result-false | identity
;;;
;;;where ATTRIBUTES is an auxiliary syntax and the attributes are the symbols:
;;;
;;;   effect-free -  The application produces no side effects.
;;;
;;;   foldable -     The application can be precomputed at compile time.
;;;
;;;   result-true -  The application always has non-#f result.
;;;
;;;   result-false - The application always has #f result.
;;;
;;;   identity -     The primitive is an identity, returning its signle argument.
;;;
;;;The ?OPERAND-SPEC can be the symbol "_"  to represent any operand or one among: 0,
;;;#f, () to represent an operand that is known at compile-time to be such datum.
;;;
;;;Example:
;;;
;;;   (attributes
;;;     (()		foldable)
;;;     ((_)		effect-free)
;;;     ((_ _)		result-true)
;;;     ((_ _ _ . _)    result-false))
;;;
;;;this ?ATTRIBUTES-SPEC specifies that:
;;;
;;;* The core primitive can be applied to 0, 1, 2, 3 or more operands.
;;;
;;;* When the number of operands is 0: the application is foldable.
;;;
;;;* When the number of operands is 1: the application has no side effects.
;;;
;;;* When the number of operands is 2: the application always returns non-false.
;;;
;;;* When the number of operands is 3 or more: the application always returns false.
;;;


;;;; syntax helpers

(define-syntax /section
  (syntax-rules ()))

(define-syntax section
  ;;By enclosing code in:
  ;;
  ;;   (section ?body ... /section)
  ;;
  ;;we can comment out a section by just commenting out the form:
  ;;
  ;;   #;(section ?body ... /section)
  ;;
  ;;This is sometimes useful when debugging.
  ;;
  (syntax-rules (/section)
    ((section ?body ... /section)
     (begin ?body ...))))


;;;; syntax helpers: predicates

(define-syntax declare-type-predicate
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate fixnum? T:fixnum)
  ;;   (declare-type-predicate vector? T:vector)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ((_ ?who ?obj-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((?obj-tag)	=> (T:true))
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ))

(define-syntax declare-type-predicate/maybe
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate/maybe maybe-pointer? T:pointer)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((T:void)	=> (T:true))
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ((_ ?who ?obj-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((T:void)	=> (T:true))
	((?obj-tag)	=> (T:true))
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ))

(define-syntax declare-type-predicate/false
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate/false false-or-pointer? T:pointer)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((T:false)	=> (T:true))
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ((_ ?who ?obj-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((T:false)	=> (T:true))
	((?obj-tag)	=> (T:true))
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ))

(define-syntax declare-type-predicate/list
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate/list list-of-pointers? T:pointer)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((T:null)	=> (T:true))
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ((_ ?who ?obj-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((T:null)	=> (T:true))
	((_)		=> (T:boolean)))
       (attributes
	((_)		foldable effect-free))))
    ))

;;; --------------------------------------------------------------------

(module (define-object-predicate-declarer)

  (define-syntax define-object-predicate-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-predicate-declarer declare-number-predicate T:number)
    ;;   (declare-number-predicate zero?)
    ;;   (declare-number-predicate positive?)
    ;;   (declare-number-predicate negative?)
    ;;
    (syntax-rules ()
      ((_ ?declarer ?obj-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-predicate ?who ?obj-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-predicate ?who ?obj-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-predicate ?who ?obj-tag unsafe (replacements)))

	   ((_ ?who		(replacements . ?replacements))	(%define-predicate ?who ?obj-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe	(replacements . ?replacements))	(%define-predicate ?who ?obj-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe	(replacements . ?replacements))	(%define-predicate ?who ?obj-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-predicate
    (syntax-rules (replacements)
      ((_ ?who ?obj-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?obj-tag)		=> (T:boolean)))
	 (attributes
	  ((_)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-PREDICATE-DECLARER |# )

(define-object-predicate-declarer declare-object-predicate		T:object)

(define-object-predicate-declarer declare-fixnum-predicate		T:fixnum)
(define-object-predicate-declarer declare-bignum-predicate		T:bignum)
(define-object-predicate-declarer declare-flonum-predicate		T:flonum)
(define-object-predicate-declarer declare-ratnum-predicate		T:ratnum)
(define-object-predicate-declarer declare-compnum-predicate		T:compnum)
(define-object-predicate-declarer declare-cflonum-predicate		T:cflonum)
(define-object-predicate-declarer declare-number-predicate		T:number)

(define-object-predicate-declarer declare-char-predicate		T:char)
(define-object-predicate-declarer declare-string-predicate		T:string)
(define-object-predicate-declarer declare-keyword-predicate		T:keyword)
(define-object-predicate-declarer declare-vector-predicate		T:vector)
(define-object-predicate-declarer declare-bytevector-predicate		T:bytevector)
(define-object-predicate-declarer declare-struct-predicate		T:struct)
(define-object-predicate-declarer declare-record-predicate		T:record)
(define-object-predicate-declarer declare-port-predicate		T:port)


;;;; syntax helpers: comparison functions

(module (define-object-binary-comparison-declarer)

  (define-syntax define-object-binary-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-binary-comparison-declarer declare-object-binary-comparison _)
    ;;   (declare-object-binary-comparison eq?)
    ;;   (declare-object-binary-comparison eqv?)
    ;;   (declare-object-binary-comparison equal?)
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?declarer ?type-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	   ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag ?type-tag)	=> (T:boolean)))
	 (attributes
	  ((_ _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-BINARY-COMPARISON-DECLARER |# )

(define-object-binary-comparison-declarer declare-object-binary-comparison T:object)
(define-object-binary-comparison-declarer declare-fixnum-binary-comparison T:fixnum)
(define-object-binary-comparison-declarer declare-flonum-binary-comparison T:flonum)
(define-object-binary-comparison-declarer declare-number-binary-comparison T:number)
(define-object-binary-comparison-declarer declare-pointer-binary-comparison T:pointer)
(define-object-binary-comparison-declarer declare-char-binary-comparison T:char)
(define-object-binary-comparison-declarer declare-string-binary-comparison T:string)
(define-object-binary-comparison-declarer declare-keyword-binary-comparison T:keyword)
(define-object-binary-comparison-declarer declare-bytevector-binary-comparison T:bytevector)

;;; --------------------------------------------------------------------

(module (define-object-unary/multi-comparison-declarer)

  (define-syntax define-object-unary/multi-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/multi-comparison-declarer declare-flonum-unary/multi-comparison T:flonum)
    ;;   (declare-flonum-unary/multi-comparison fl=?)
    ;;   (declare-flonum-unary/multi-comparison fl<?)
    ;;   (declare-flonum-unary/multi-comparison fl>?)
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?declarer ?type-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	   ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)				=> (T:boolean))
	  ((?type-tag ?type-tag)		=> (T:boolean))
	  ((?type-tag ?type-tag . ?type-tag)	=> (T:boolean)))
	 (attributes
	  ((_)				foldable effect-free)
	  ((_ _)			foldable effect-free)
	  ((_ _ . _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/MULTI-COMPARISON-DECLARER |# )

(define-object-unary/multi-comparison-declarer declare-number-unary/multi-comparison T:number)
(define-object-unary/multi-comparison-declarer declare-fixnum-unary/multi-comparison T:fixnum)
(define-object-unary/multi-comparison-declarer declare-flonum-unary/multi-comparison T:flonum)
(define-object-unary/multi-comparison-declarer declare-string-unary/multi-comparison T:string)
(define-object-unary/multi-comparison-declarer declare-pointer-unary/multi-comparison T:pointer)
(define-object-unary/multi-comparison-declarer declare-boolean-unary/multi-comparison T:boolean)

;;; --------------------------------------------------------------------

(module (define-object-binary/multi-comparison-declarer)

  (define-syntax define-object-binary/multi-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-binary/multi-comparison-declarer declare-char-binary/multi-comparison T:char)
    ;;   (declare-char-binary/multi-comparison char=?)
    ;;   (declare-char-binary/multi-comparison char<?)
    ;;   (declare-char-binary/multi-comparison char>?)
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?declarer ?type-tag)
       (define-syntax ?declarer
	 (syntax-rules (safe unsafe replacements)
	   ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	   ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	   ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	   ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	   )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag ?type-tag)		=> (T:boolean))
	  ((?type-tag ?type-tag . ?type-tag)	=> (T:boolean)))
	 (attributes
	  ((_ _)			foldable effect-free)
	  ((_ _ . _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-BINARY/MULTI-COMPARISON-DECLARER |# )

(define-object-binary/multi-comparison-declarer declare-number-binary/multi-comparison T:number)
(define-object-binary/multi-comparison-declarer declare-char-binary/multi-comparison T:char)
(define-object-binary/multi-comparison-declarer declare-string-binary/multi-comparison T:string)
(define-object-binary/multi-comparison-declarer declare-boolean-binary/multi-comparison T:boolean)


;;;; syntax helpers: math operations

(module (define-object-unary-operation-declarer)

  (define-syntax (define-object-unary-operation-declarer stx)
    ;;Usage example:
    ;;
    ;;   (define-object-unary-operation-declarer declare-flonum-unary T:flonum)
    ;;   (declare-flonum-unary flsin)
    ;;   (declare-flonum-unary flcos)
    ;;   (declare-flonum-unary fltan)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY-OPERATION-DECLARER |# )

(define-object-unary-operation-declarer declare-number-unary T:number)
(define-object-unary-operation-declarer declare-fixnum-unary T:fixnum)
(define-object-unary-operation-declarer declare-flonum-unary T:flonum)
(define-object-unary-operation-declarer declare-exact-integer-unary T:exact-integer)
(define-object-unary-operation-declarer declare-char-unary T:char)
(define-object-unary-operation-declarer declare-string-unary T:string)

;;; --------------------------------------------------------------------

(module (define-object-binary-operation-declarer)

  (define-syntax (define-object-binary-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-binary-operation-declarer declare-flonum-binary T:flonum)
    ;;   (declare-flonum-binary flexpt)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_ _)			foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-BINARY-OPERATION-DECLARER |# )

(define-object-binary-operation-declarer declare-number-binary T:number)
(define-object-binary-operation-declarer declare-fixnum-binary T:fixnum)
(define-object-binary-operation-declarer declare-flonum-binary T:flonum)
(define-object-binary-operation-declarer declare-string-binary T:string)

;;; --------------------------------------------------------------------

(module (define-object-unary/binary-operation-declarer)

  (define-syntax (define-object-unary/binary-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/binary-operation-declarer declare-flonum-unary/binary T:flonum)
    ;;   (declare-flonum-unary/binary fllog)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)			=> (?type-tag))
	  ((?type-tag ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)			foldable effect-free result-true)
	  ((_ _)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/BINARY-OPERATION-DECLARER |# )

(define-object-unary/binary-operation-declarer declare-number-unary/binary T:number)
(define-object-unary/binary-operation-declarer declare-fixnum-unary/binary T:fixnum)
(define-object-unary/binary-operation-declarer declare-flonum-unary/binary T:flonum)
(define-object-unary/binary-operation-declarer declare-string-unary/binary T:string)

;;; --------------------------------------------------------------------

(module (define-object-unary/multi-operation-declarer)

  (define-syntax (define-object-unary/multi-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/multi-operation-declarer declare-flonum-unary/multi T:flonum)
    ;;   (declare-flonum-unary/multi fl+)
    ;;   (declare-flonum-unary/multi fl-)
    ;;   (declare-flonum-unary/multi fl*)
    ;;   (declare-flonum-unary/multi fl/)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?type-tag)			=> (?type-tag))
	  ((?type-tag . ?type-tag)	=> (?type-tag)))
	 (attributes
	  ((_)				foldable effect-free result-true)
	  ((_ . _)			foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/MULTI-OPERATION-DECLARER |# )

(define-object-unary/multi-operation-declarer declare-number-unary/multi T:number)
(define-object-unary/multi-operation-declarer declare-fixnum-unary/multi T:fixnum)
(define-object-unary/multi-operation-declarer declare-flonum-unary/multi T:flonum)
(define-object-unary/multi-operation-declarer declare-string-unary/multi T:string)

;;; --------------------------------------------------------------------

(module (define-object-multi-operation-declarer)

  (define-syntax (define-object-multi-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-multi-operation-declarer declare-fixnum-multi T:fixnum)
    ;;   (declare-fixnum-multi fxior)
    ;;
    (syntax-case stx ()
      ((_ ?declarer ?type-tag)
       (all-identifiers? #'(?declarer ?type-tag))
       #'(define-syntax ?declarer
	   (syntax-rules (safe unsafe replacements)
	     ((_ ?who)						(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who safe)					(%define-declarer ?who ?type-tag safe   (replacements)))
	     ((_ ?who unsafe)					(%define-declarer ?who ?type-tag unsafe (replacements)))

	     ((_ ?who        (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who safe   (replacements . ?replacements))	(%define-declarer ?who ?type-tag safe   (replacements . ?replacements)))
	     ((_ ?who unsafe (replacements . ?replacements))	(%define-declarer ?who ?type-tag unsafe (replacements . ?replacements)))
	     )))
      ))

  (define-syntax %define-declarer
    (syntax-rules (replacements)
      ((_ ?who ?type-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  (()			=> (?type-tag))
	  (?type-tag		=> (?type-tag)))
	 (attributes
	  (()			foldable effect-free result-true)
	  ((_ . _)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-MULTI-OPERATION-DECLARER |# )

(define-object-multi-operation-declarer declare-number-multi T:number)
(define-object-multi-operation-declarer declare-fixnum-multi T:fixnum)
(define-object-multi-operation-declarer declare-flonum-multi T:flonum)
(define-object-multi-operation-declarer declare-string-multi T:string)

;;; --------------------------------------------------------------------

(define-syntax declare-unsafe-unary-operation
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-unary-operation $neg-fixnum T:fixnum T:exact-integer)
  ;;
  (syntax-rules ()
    ((_ ?who ?operand-tag ?result-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((?operand-tag)		=> (?result-tag)))
       (attributes
	((_)			foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-unary-operation/2rv
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-unary-operation/2rv $exact-integer-sqrt-fixnum T:fixnum T:exact-integer T:exact-integer)
  ;;
  (syntax-rules ()
    ((_ ?who ?operand-tag ?result1-tag ?result2-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((?operand-tag)		=> (?result1-tag ?result2-tag)))
       (attributes
	((_)			foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-binary-operation
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-binary-operation $add-fixnum-fixnum T:fixnum T:fixnum T:exact-integer)
  ;;
  (syntax-rules ()
    ((_ ?who ?operand1-tag ?operand2-tag ?result-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((?operand1-tag ?operand2-tag)	=> (?result-tag)))
       (attributes
	((_ _)				foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-binary-operation/2rv
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-binary-operation $quotient+remainder-fixnum-fixnum T:fixnum T:fixnum T:exact-integer T:fixnum)
  ;;
  (syntax-rules ()
    ((_ ?who ?operand1-tag ?operand2-tag ?result1-tag ?result2-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((?operand1-tag ?operand2-tag)	=> (?result1-tag ?result2-tag)))
       (attributes
	((_ _)				foldable effect-free result-true))))
    ))


;;;; syntax helpers: pairs, lists, alists

(module (declare-pair-accessor)

  (define-syntax declare-pair-accessor
    ;;This is for: CAR, CDR, CAAR, CADR, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who)						(%declare-pair-accessor ?who safe   (replacements)))
      ((_ ?who safe)					(%declare-pair-accessor ?who safe   (replacements)))
      ((_ ?who unsafe)					(%declare-pair-accessor ?who unsafe (replacements)))
      ((_ ?who        (replacements . ?replacements))	(%declare-pair-accessor ?who safe   (replacements . ?replacements)))
      ((_ ?who safe   (replacements . ?replacements))	(%declare-pair-accessor ?who safe   (replacements . ?replacements)))
      ((_ ?who unsafe (replacements . ?replacements))	(%declare-pair-accessor ?who unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-pair-accessor
    (syntax-rules (replacements)
      ((_ ?who ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((T:pair)		=> (_)))
	 (attributes
	  ((_)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module |# )

;;; --------------------------------------------------------------------

(module (declare-pair-mutator)

  (define-syntax declare-pair-mutator
    ;;This is for: SET-CAR!, SET-CDR!, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who)						(%declare-pair-mutator ?who safe   (replacements)))
      ((_ ?who safe)					(%declare-pair-mutator ?who safe   (replacements)))
      ((_ ?who unsafe)					(%declare-pair-mutator ?who unsafe (replacements)))
      ((_ ?who        (replacements . ?replacements))	(%declare-pair-mutator ?who safe   (replacements . ?replacements)))
      ((_ ?who safe   (replacements . ?replacements))	(%declare-pair-mutator ?who safe   (replacements . ?replacements)))
      ((_ ?who unsafe (replacements . ?replacements))	(%declare-pair-mutator ?who unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-pair-mutator
    (syntax-rules (replacements)
      ((_ ?who ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((T:pair _)		=> ()))
	 (attributes
	  ((_ _)		result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DECLARE-PAIR-MUTATOR |# )

;;; --------------------------------------------------------------------

(module (declare-alist-accessor)

  (define-syntax declare-alist-accessor
    ;;This is for: ASSQ, ASSV, ...
    ;;
    (syntax-rules (safe unsafe replacements)
      ((_ ?who ?obj-tag)					(%declare-alist-accessor ?who ?obj-tag safe   (replacements)))
      ((_ ?who ?obj-tag safe)					(%declare-alist-accessor ?who ?obj-tag safe   (replacements)))
      ((_ ?who ?obj-tag unsafe)					(%declare-alist-accessor ?who ?obj-tag unsafe (replacements)))
      ((_ ?who ?obj-tag        (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag safe   (replacements . ?replacements)))
      ((_ ?who ?obj-tag safe   (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag safe   (replacements . ?replacements)))
      ((_ ?who ?obj-tag unsafe (replacements . ?replacements))	(%declare-alist-accessor ?who ?obj-tag unsafe (replacements . ?replacements)))
      ))

  (define-syntax %declare-alist-accessor
    (syntax-rules (replacements)
      ((_ ?who ?obj-tag ?safety (replacements . ?replacements))
       (declare-core-primitive ?who
	   (?safety)
	 (signatures
	  ((?obj-tag T:proper-list)	=> (_)))
	 (attributes
	  ((_ _)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module: DECLARE-ALIST-ACCESSOR |# )

;;; --------------------------------------------------------------------

(define-syntax declare-list-finder
  ;;This is for: MEMQ, MEMV, ...
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag)
     (declare-list-finder ?who ?obj-tag safe))
    ((_ ?who ?obj-tag ?safety)
     (declare-core-primitive ?who
	 (?safety)
       (signatures
	((?obj-tag T:null)			=> (T:false))
	((?obj-tag T:non-empty-proper-list)	=> (_)))
       (attributes
	((_ ())			foldable effect-free result-false)
	((_ _)			foldable effect-free))))
    ))


;;;; syntax helpers: bytevectors

(define-syntax declare-safe-bytevector-conversion
  ;;Usage examples:
  ;;
  ;;   (declare-safe-bytevector-conversion uri-encode T:bytevector)
  ;;   (declare-safe-bytevector-conversion uri-decode T:bytevector)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((T:bytevector)		=> (?return-value-tag)))
       (attributes
	;;Not foldable because it must return a new bytevector every time.
	((_)			effect-free result-true))))
    ))

;;; --------------------------------------------------------------------

(define-syntax declare-unsafe-bytevector-accessor
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-accessor $bytevector-u8-ref T:octet)
  ;;   (declare-unsafe-bytevector-accessor $bytevector-s8-ref T:byte)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((T:bytevector T:fixnum)	=> (?return-value-tag)))
       (attributes
	((_ _)			foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-bytevector-mutator
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-mutator $bytevector-set! T:octet/byte)
  ;;
  (syntax-rules ()
    ((_ ?who ?new-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((T:bytevector T:fixnum ?new-value-tag)	=> ()))))
    ))

(define-syntax declare-unsafe-bytevector-conversion
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-conversion $uri-encode T:bytevector)
  ;;   (declare-unsafe-bytevector-conversion $uri-decode T:bytevector)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((T:bytevector)		=> (?return-value-tag)))
       (attributes
	;;Not foldable because it must return a new bytevector every time.
	((_)			effect-free result-true))))
    ))


;;;; syntax helpers: miscellaneous

(define-syntax declare-parameter
  ;;Usage examples:
  ;;
  ;;   (declare-parameter current-input-port	T:textual-input-port)
  ;;   (declare-parameter native-transcoder	T:transcoder)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-parameter ?who T:object))
    ((_ ?who ?value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(()			=> (?value-tag))
	((?value-tag)		=> ())
	((?value-tag T:boolean)	=> ()))
       (attributes
	(()			effect-free))))
    ))

(define-syntax declare-object-retriever
  ;;Usage examples:
  ;;
  ;;   (declare-object-retriever console-input-port	     T:binary-input-port)
  ;;   (declare-object-retriever native-eol-style   foldable T:symbol)
  ;;
  ;;NOTE The returned object must *not* be false.
  ;;
  (syntax-rules (foldable)
    ((_ ?who foldable)
     (declare-object-retriever ?who foldable T:object))
    ((_ ?who foldable ?return-value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(()		=> (?return-value-tag)))
       (attributes
	(()		foldable effect-free result-true))))

    ((_ ?who)
     (declare-object-retriever ?who T:object))
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(()		=> (?return-value-tag)))
       (attributes
	;;Not foldable, we want the object built and returned at run-time.
	(()		effect-free result-true))))
    ))






;;;; done

#| end of library |# )

;;; end of file
