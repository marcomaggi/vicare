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
  (core-primitive-name->application-attributes*
   core-primitive-name->core-type-signature*
   core-primitive-name->replacement*
   application-attributes-operands-template
   application-attributes-foldable?
   application-attributes-effect-free?
   application-attributes-result-true?
   application-attributes-result-false?
   CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES)
  (import SCHEME-OBJECTS-ONTOLOGY)

  (define-constant CORE-PRIMITIVE-PROPKEY
    (compile-time-gensym "core-primitive-properties"))

  (define* (core-primitive-name->application-attributes* {prim-name symbol?})
    ;;Return the APPLICATION-ATTRIBUTES* list of the core primitive PRIM-NAME; return
    ;;false if PRIM-NAME has  no attributes associated or it is  not a core primitive
    ;;name.
    ;;
    (cond ((getprop prim-name CORE-PRIMITIVE-PROPKEY)
	   => core-primitive-properties-application-attributes*)
	  (else #f)))

  (define* (core-primitive-name->core-type-signature* {prim-name symbol?})
    ;;Return the  SIGNATURE* list of  the core  primitive PRIM-NAME; return  false if
    ;;PRIM-NAME has no registered signatures or it is not a core primitive name.
    ;;
    (cond ((getprop prim-name CORE-PRIMITIVE-PROPKEY)
	   => core-primitive-properties-core-type-signature*)
	  (else #f)))

  (define* (core-primitive-name->replacement* {prim-name symbol?})
    ;;Return the REPLACEMENT*  list of the core primitive PRIM-NAME;  return false if
    ;;PRIM-NAME has no registered replacements or it is not a core primitive name.
    ;;
    (cond ((getprop prim-name CORE-PRIMITIVE-PROPKEY)
	   => core-primitive-properties-replacement*)
	  (else #f)))


;;;; core primitive properties representation

(define-struct core-primitive-properties
  (core-type-signature*
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
   ))

(define-constant CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES
  (make-application-attributes '_ #f #f #f #f))


;;;; syntaxes for parsing tables

(define-auxiliary-syntaxes
  safe unsafe
  signatures attributes replacements
  foldable effect-free result-true result-false)

(define-syntax* (declare-core-primitive input-form)
  (define (main stx)
    (syntax-case stx ()
      ((?ctx ?prim-name . ?clause*)
       (let ((prim-name (%parse-prim-name #'?prim-name)))
	 ;; (fprintf (current-error-port)
	 ;; 	  "parsing core primitive declaration: ~a\n"
	 ;; 	  (syntax->datum prim-name))
	 (receive (safe? signature* attribute* replacement-prim-name*)
	     (%parse-clauses #'?clause*)
	   (let ((signature-pred* (%signatures->signature-pred* #'?ctx signature*)))
	     (with-syntax
		 ((SIGNATURES-FORM	(%compose-signature-output-form signature-pred*))
		  (ATTRIBUTES-FORM	(%compose-attributes-output-form attribute*))
		  (REPLACEMENTS-FORM	(%compose-replacements-output-orm replacement-prim-name*)))
	       #;(fprintf (current-error-port) "registering core primitive properties: ~a\n" (syntax->datum prim-name))
	       #`(putprop (quote ?prim-name) CORE-PRIMITIVE-PROPKEY
			  (make-core-primitive-properties SIGNATURES-FORM ATTRIBUTES-FORM REPLACEMENTS-FORM)))))))

      (_
       (synner "invalid syntax in core primitive declaration"))))

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

  (define (%compose-signature-output-form signature-pred*)
    (if (pair? signature-pred*)
	#`(quasiquote #,signature-pred*)
      #'(quote ())))

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
					     #,(vector-ref attributes-vector 3)))))
			  attribute*))
      #'(quote ())))

  (define (%compose-replacements-output-orm replacement*)
    (if (pair? replacement*)
	#`(quote #,replacement*)
      #'(quote ())))

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
    ;;lists of identifiers, each identifier representing the core type of an argument
    ;;or  return value;  the  type  identifiers are  defined  by  the Scheme  objects
    ;;ontology.  The identifiers "_" and  "T:object" are wildcards meaning "an object
    ;;of any type".
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
    ;;of error, and returns a proper list of pairs:
    ;;
    ;;   ((?operands-types . ?return-values-types) ...)
    ;;
    ;;in collecting the arguments and return value types; the returned expression has
    ;;a false object in place of the wildcards "_" and "T:object".
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
    ;;Non-tail recursive  function.  Parse  a proper  or improper  list of  core type
    ;;identifiers representing  the types of  arguments or  return values for  a core
    ;;primitive.  If successful: return a proper  or improper list of identifiers and
    ;;false  objects in  which the  false objects  substitute the  wildcards "_"  and
    ;;"T:object".  If an invalid syntax is found: raise a "&syntax" exception.
    ;;
    (syntax-case stx ()
      ((?car . ?cdr)
       (cons (%parse-core-object-type #'?car)
	     (%parse-proper-or-improper-list-of-core-types #'?cdr)))
      (()  '())
      (?rest
       (%parse-core-object-type #'?rest))
      (_
       (synner "invalid signatures component in core primitive declaration" stx))))

  (define (%parse-core-object-type type)
    ;;We accept a  core type identifier and  return it; "T:object" and  "_" mean "any
    ;;type" and we convert them to false.
    (syntax-case type
	( ;;
	 T:object		T:other-object		T:immediate	T:nonimmediate
	 T:non-false		T:false			T:true		T:void
	 T:boolean		T:char			T:symbol	T:string
	 T:vector		T:bytevector
	 T:procedure		T:transcoder		T:pointer	T:hashtable

	 T:struct		T:struct-type-descriptor	T:other-struct
	 T:record		T:record-type-descriptor

	 T:null			T:standalone-pair	T:non-empty-proper-list
	 T:pair			T:proper-list		T:improper-list

	 T:port			T:textual-port		T:binary-port
	 T:input-port		T:output-port		T:input-output-port
	 T:textual-input-port	T:textual-output-port	T:textual-input/output-port
	 T:binary-input-port	T:binary-output-port	T:binary-input/output-port

	 T:number		T:exact			T:inexact
	 T:fixnum		T:bignum		T:ratnum
	 T:flonum		T:flonum-integer	T:flonum-fractional
	 T:flonum-finite	T:flonum-infinite	T:flonum-nan
	 T:compnum		T:cflonum
	 T:positive		T:zero			T:negative
	 T:exact-integer
	 T:real			T:exact-real
	 T:complex

	 T:positive-fixnum	T:negative-fixnum)
      (T:object				#f)
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
      (T:procedure			type)
      (T:port				type)
      (T:textual-port			type)
      (T:binary-port			type)
      (T:input-port			type)
      (T:output-port			type)
      (T:input-output-port		type)
      (T:textual-input-port		type)
      (T:textual-output-port		type)
      (T:textual-input/output-port	type)
      (T:binary-input-port		type)
      (T:binary-output-port		type)
      (T:binary-input/output-port	type)

      (T:struct				type)
      (T:struct-type-descriptor		type)
      (T:other-struct			type)
      (T:record				type)
      (T:record-type-descriptor		type)

      (T:transcoder			type)
      (T:pointer			type)
      (T:hashtable			type)
      (T:number				type)
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
      (T:positive			type)
      (T:zero				type)
      (T:negative			type)
      (T:exact-integer			type)
      (T:real				type)
      (T:exact-real			type)
      (T:complex			type)
      (T:positive-fixnum		type)
      (T:negative-fixnum		type)
      (_
       ;; (if (identifier? type)
       ;; 	   (if (eq? '_ (syntax->datum type))
       ;; 	       #f
       ;; 	     type)
       ;; 	 (synner "expected identifier as core type name in core primitive declaration" type))
       (if (and (identifier? type)
		(eq? '_ (syntax->datum type)))
	   #f
	 (synner "expected identifier as core type name in core primitive declaration" type))
       )))

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
      ;;   foldable	effect-free
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
      (let recur ((flags (make-vector 4 #f))
		  (attr* attr*))
	(syntax-case attr* (foldable effect-free result-true result-false)
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

  (define (%signatures->signature-pred* ctx signature*)
    ;;Given a list  of (already parsed) core primitive  signature specifications with
    ;;the format:
    ;;
    ;;   ((?operands-types . ?return-values-types) ...)
    ;;
    ;;build and return  a new list of  pairs in which the type  identifiers have been
    ;;replaced by the  identifiers of the corresponding type  predicates wrapped into
    ;;an UNQUOTE syntax.
    ;;
    ;;For example, the original signature:
    ;;
    ;;   ((T:fixnum T:string _) => (T:string _))
    ;;
    ;;is parsed into:
    ;;
    ;;   ((T:fixnum T:string #f) . (T:string #f))
    ;;
    ;;and this function transforms it into:
    ;;
    ;;   ((,T:fixnum? ,T:string? #f) . (,T:string? #f))
    ;;
    (define (%type->pred type)
      (cond ((identifier? type)
	     (list #'unquote
		   (datum->syntax ctx (string->symbol
				       (string-append (symbol->string (syntax->datum type))
						      "?")))))
	    (else
	     (assert (not type))
	     type)))
    (map (lambda (signature)
	   ;;RAND-TYPES is  a proper or  improper list  of core type  identifiers and
	   ;;false objects.
	   (cons (let recur ((rand-types (car signature)))
		   (cond ((pair? rand-types)
			  (cons (%type->pred (car rand-types))
				(recur (cdr rand-types))))
			 ((null? rand-types)
			  '())
			 (else
			  (%type->pred rand-types))))
		 (let recur ((rand-types (cdr signature)))
		   (cond ((pair? rand-types)
			  (cons (%type->pred (car rand-types))
				(recur (cdr rand-types))))
			 ((null? rand-types)
			  '())
			 (else
			  (%type->pred rand-types))))))
      signature*))

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
