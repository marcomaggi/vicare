;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: compile-time property definitions for core primitives
;;;Date: Mon Sep 22, 2014
;;;
;;;Abstract
;;;
;;;	The purpose of this module is to  put values in the property lists of symbols
;;;	representing  core  primitive  public   names.   The  values  represent  core
;;;	primitive  properties: the  arity of  the primitive;  the number  of returned
;;;	values;  the core  types of  the expected  arguments; the  core types  of the
;;;	returned values; miscellaneous properties used by the source optimiser.
;;;
;;;	  Scheme  object's core  types  are  defined by  the  module "Scheme  objects
;;;	ontology".  The actual core primitive tables are in a different source file.
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(module CORE-PRIMITIVE-PROPERTIES
  (core-primitive-name->core-type-tag
   core-primitive-name->application-attributes*
   core-primitive-name->core-type-signature*
   core-primitive-name->replacement*
   core-type-tag?
   core-type-tag-is-a?
   application-attributes-operands-template
   application-attributes-foldable?
   application-attributes-effect-free?
   application-attributes-result-true?
   application-attributes-result-false?
   application-attributes-identity?
   CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES)
  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (core-primitive-name->core-type-tag name)
    ;;Given a symbol representing the name of a core primitive: return the associated
    ;;CORE-TYPE-TAG value.  As default return "T:object".
    ;;
    (if (and (symbol-bound? name)
	     (core-primitive-properties? (symbol-value name)))
	T:procedure
      T:object))

  (define* (core-primitive-name->application-attributes* {prim-name symbol?})
    ;;Return the APPLICATION-ATTRIBUTES* list of the core primitive PRIM-NAME; return
    ;;false if PRIM-NAME has  no attributes associated or it is  not a core primitive
    ;;name.
    ;;
    (and (symbol-bound? prim-name)
	 (core-primitive-properties-application-attributes* (symbol-value prim-name))))

  (define* (core-primitive-name->core-type-signature* {prim-name symbol?})
    ;;Return the  SIGNATURE* list of  the core  primitive PRIM-NAME; return  false if
    ;;PRIM-NAME has no registered signatures or it is not a core primitive name.
    ;;
    (and (symbol-bound? prim-name)
	 (core-primitive-properties-core-type-signature* (symbol-value prim-name))))

  (define* (core-primitive-name->replacement* {prim-name symbol?})
    ;;Return the REPLACEMENT*  list of the core primitive PRIM-NAME;  return false if
    ;;PRIM-NAME has no registered replacements or it is not a core primitive name.
    ;;
    (and (symbol-bound? prim-name)
	 (core-primitive-properties-replacement* (symbol-value prim-name))))

;;; --------------------------------------------------------------------

  ;;NOTE This API was once using property  lists; the old implementation is below.  I
  ;;know that it is bad to keep  old, uncommented code around.  (Marco Maggi; Tue Dec
  ;;2, 2014)

  ;; (define-constant CORE-PRIMITIVE-PROPKEY
  ;;   (compile-time-gensym "core-primitive-properties"))

  ;; (define* (core-primitive-name->application-attributes* {prim-name symbol?})
  ;;   ;;Return the APPLICATION-ATTRIBUTES* list of the core primitive PRIM-NAME; return
  ;;   ;;false if PRIM-NAME has  no attributes associated or it is  not a core primitive
  ;;   ;;name.
  ;;   ;;
  ;;   (cond ((getprop prim-name CORE-PRIMITIVE-PROPKEY)
  ;;   	   => core-primitive-properties-application-attributes*)
  ;;   	  (else #f)))

  ;; (define* (core-primitive-name->core-type-signature* {prim-name symbol?})
  ;;   ;;Return the  SIGNATURE* list of  the core  primitive PRIM-NAME; return  false if
  ;;   ;;PRIM-NAME has no registered signatures or it is not a core primitive name.
  ;;   ;;
  ;;   (cond ((getprop prim-name CORE-PRIMITIVE-PROPKEY)
  ;;   	   => core-primitive-properties-core-type-signature*)
  ;;   	  (else #f)))

  ;; (define* (core-primitive-name->replacement* {prim-name symbol?})
  ;;   ;;Return the REPLACEMENT*  list of the core primitive PRIM-NAME;  return false if
  ;;   ;;PRIM-NAME has no registered replacements or it is not a core primitive name.
  ;;   ;;
  ;;   (cond ((getprop prim-name CORE-PRIMITIVE-PROPKEY)
  ;;   	   => core-primitive-properties-replacement*)
  ;;   	  (else #f)))


;;;; core primitive properties representation

(define-struct core-primitive-properties
  (safe?
		;Boolean.  True if this core primitive is safe.
   core-type-signature*
		;A list of pairs with the format:
		;
		;   ((?operands-preds . ?return-values-preds) ...)
		;
		;in which both ?OPERANDS-PREDS and ?RETURN-VALUES-PREDS are proper or
		;improper lists of type predicates and false objects.
		;
		;Every  pair  represents  an  alternative  core  type  signature  for
		;operands and return values.
   application-attributes*
		;A proper list:
		;
		;   (?application-attributes ...)
		;
		;in  which   every  ?APPLICATION-ATTRIBUTES  is  a   struct  of  type
		;APPLICATION-ATTRIBUTES.
   replacement*
		;List of symbols  representing core primitives that  can replace this
		;one if the operands are of the correct type.
   ))

(define-struct application-attributes
  (operands-template
		;A  proper or  improper  list  of items  representing  a template  of
		;application for the core primitive.  The items are: "_" representing
		;any operand; "0"  representing an operand equal to  the fixnum zero;
		;"#f"  representing  an operand  equal  to  the boolean  false;  "()"
		;representing an operand equal to null.
   foldable?
		;Boolean.  True if  the associated core primitive  application can be
		;precomputed at compile-time when the operands are constants.
   effect-free?
		;Boolean.  True if  the associated core primitive  application has no
		;side effects.
   result-true?
		;Boolean.  True  if the associated core  primitive application always
		;has non-false as return value.
   result-false?
		;Boolean.  True  if the associated core  primitive application always
		;has false as return value.
   identity?
		;Boolean.  True  if the  associated core  primitive accepts  a single
		;argument and it is an identity, returning its argument.
   ))

(define-constant CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES
  (make-application-attributes '_ #f #f #f #f #f))


;;;; syntaxes for parsing tables

(define-auxiliary-syntaxes
  safe unsafe
  signatures attributes replacements
  foldable effect-free result-true result-false)

(define-syntax* (declare-core-primitive input-form)

  (module (main)

    (define (main stx)
      (syntax-case stx ()
	((?ctx ?prim-name . ?clause*)
	 (let ((prim-name (%parse-prim-name #'?prim-name)))
	   (receive (safe? signature* attribute* replacement-prim-name*)
	       (%parse-clauses #'?clause*)
	     (receive-and-return (out)
		 #`(begin
		     (set-symbol-value! (quote ?prim-name)
					(make-core-primitive-properties #,safe?
									(quasiquote #,signature*)
									#,(%compose-attributes-output-form attribute*)
									(quote #,replacement-prim-name*)))
		     ;; (putprop (quote ?prim-name) CORE-PRIMITIVE-PROPKEY
		     ;; 	      (make-core-primitive-properties #,safe?
		     ;; 					      (quasiquote #,signature*)
		     ;; 					      #,(%compose-attributes-output-form attribute*)
		     ;; 					      (quote #,replacement-prim-name*)))
		     )
	       #;(fprintf (current-error-port) "output: ~a\n" (syntax->datum out))
	       (void)))))

	(_
	 (synner "invalid syntax in core primitive declaration"))))

    (define (%compose-attributes-output-form attribute*)
      (if (pair? attribute*)
	  #`(quasiquote #,(map (lambda (attribute*)
				 (let ((operands-template (car attribute*))
				       (attributes-vector (cdr attribute*)))
				   #`(unquote (make-application-attributes
					       (quote #,operands-template)
					       #,(vector-ref attributes-vector 0)
					       #,(vector-ref attributes-vector 1)
					       #,(vector-ref attributes-vector 2)
					       #,(vector-ref attributes-vector 3)
					       #,(vector-ref attributes-vector 4)))))
			    attribute*))
	#'(quote ())))

    #| end of module: MAIN |# )

;;; --------------------------------------------------------------------
;;; input form parsers

  (define (%parse-prim-name stx)
    (if (identifier? stx)
	stx
      (synner "expected identifier as core primitive name" stx)))

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
    ;;   ((?operands-types . ?return-values-types) ...)
    ;;
    ;;which is meant  to be wrapped into a QUASIQUOTE;  internal unquoted expressions
    ;;will evaluate to  an exact integer representing the naked  bits defining a core
    ;;type specification.
    ;;
    ;;the returned object has "T:object" in place of the wildcard "_".
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
    ;;Non-tail recursive  function.  Parse a  proper or  improper list of  logic type
    ;;expressions representing  the types of  arguments or  return values for  a core
    ;;primitive.
    ;;
    (define-syntax-rule (%maybe-wrap ?type-spec)
      #`(unquote (core-type-tag-bits #,?type-spec)))
    (syntax-case stx ()
      ((?car . ?cdr)
       (cons (%maybe-wrap (%parse-core-object-type #'?car))
	     (%parse-proper-or-improper-list-of-core-types #'?cdr)))
      (()  '())
      (?rest
       (%maybe-wrap (%parse-core-object-type #'?rest)))
      (_
       (synner "invalid signatures component in core primitive declaration" stx))))

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
	 T:eof			T:would-block


	 T:struct		T:struct-type-descriptor	T:other-struct
	 T:record		T:record-type-descriptor	T:enum-set
	 T:library		T:lexical-environment		T:record-constructor-descriptor
	 T:condition		T:other-record

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

      (T:struct				type)
      (T:struct-type-descriptor		type)
      (T:other-struct			type)
      (T:record				type)
      (T:record-type-descriptor		type)
      (T:record-constructor-descriptor	type)
      (T:enum-set			type)
      (T:library			type)
      (T:lexical-environment		type)
      (T:condition			type)
      (T:other-record			type)

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
      (T:positive-fixnum		type)
      (T:negative-fixnum		type)
      (T:non-positive-fixnum		type)
      (T:non-negative-fixnum		type)
      (T:positive-bignum		type)
      (T:negative-bignum		type)
      (T:positive-flonum		type)
      (T:negative-flonum		type)
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
			 (%parse-appliaction-attributes-attributes-specification #'?attr*)))
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

    (define (%parse-appliaction-attributes-attributes-specification attr*)
      (let recur ((flags (make-vector 5 #f))
		  (attr* attr*))
	(syntax-case attr* (foldable identity effect-free result-true result-false)
	  ((foldable . ?rest)
	   (vector-set! flags 0 #t)
	   (recur flags #'?rest))

	  ((effect-free . ?rest)
	   (vector-set! flags 1 #t)
	   (recur flags #'?rest))

	  ((result-true . ?rest)
	   (vector-set! flags 2 #t)
	   (recur flags #'?rest))

	  ((result-false . ?rest)
	   (vector-set! flags 3 #t)
	   (recur flags #'?rest))

	  ((identity . ?rest)
	   (vector-set! flags 4 #t)
	   (recur flags #'?rest))

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
  (receive-and-return (output-form)
      (main input-form)
    #;(debug-print (syntax->datum output-form))
    (void)))


;;;; include core primitive tables

(include "ikarus.compiler.core-primitive-tables.scm" #t)


;;;; done

#| end of module: CORE-PRIMITIVE-PROPERTIES |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'struct-case			'scheme-indent-function 1)
;; eval: (put '$map/stx				'scheme-indent-function 1)
;; eval: (put '$for-each/stx			'scheme-indent-function 1)
;; eval: (put '$fold-right/stx			'scheme-indent-function 1)
;; eval: (put 'compile-time-error		'scheme-indent-function 1)
;; eval: (put 'compiler-internal-error		'scheme-indent-function 1)
;; eval: (put 'compile-time-arity-error		'scheme-indent-function 1)
;; eval: (put 'compile-time-operand-core-type-error 'scheme-indent-function 1)
;; End:
