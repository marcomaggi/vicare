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
  (CORE-PRIMITIVE-PROPKEY:CORE-TYPE-SIGNATURES
   CORE-PRIMITIVE-PROPKEY:CALL-REPLACEMENTS)
  (import SCHEME-OBJECTS-ONTOLOGY)

  (define-constant CORE-PRIMITIVE-PROPKEY:CORE-TYPE-SIGNATURES
    (compile-time-gensym "core-primitive/core-types-signature"))

  (define-constant CORE-PRIMITIVE-PROPKEY:CALL-REPLACEMENTS
    (compile-time-gensym "core-primitive/replacements-signatures"))


;;;; syntaxes for parsing tables

(define-auxiliary-syntaxes safe unsafe signatures replacements)

(define-syntax* (declare-core-primitive input-form)
  (define (main stx)
    (syntax-case stx (safe unsafe signatures replacements)

      ;;Unsafe primitive.
      ((?ctx ?prim-name (unsafe)
	     (signatures ?signature ...))
       (let* ((prim-name	(%parse-prim-name #'?prim-name))
	      (signature*	(%parse-signatures-sexp #'(signatures ?signature ...)))
	      (signature-pred*	(%signatures->signature-pred* #'?ctx signature*)))
	 (with-syntax
	     ((SIGNATURE-FORM	(%compose-signature-output-form prim-name signature-pred*)))
	   #'(begin SIGNATURE-FORM))))

      ;;Safe primitive.
      ((?ctx ?prim-name (safe)
	     (signatures ?signature ...)
	     (replacements ?replacement-prim-name ...))
       (let* ((prim-name		(%parse-prim-name #'?prim-name))
	      (signature*		(%parse-signatures-sexp #'(signatures ?signature ...)))
	      (replacement-prim-name*	(%parse-replacements-sexp #'(replacements ?replacement-prim-name ...)))
	      (signature-pred*		(%signatures->signature-pred* #'?ctx signature*)))
	 (with-syntax
	     ((SIGNATURE-FORM		(%compose-signature-output-form prim-name signature-pred*))
	      (REPLACEMENTS-FORM	(%compose-replacements-output-orm prim-name replacement-prim-name*)))
	   #'(begin SIGNATURE-FORM REPLACEMENTS-FORM))))

      (_
       (synner "invalid syntax in core primitive declaration"))))

  (define (%compose-signature-output-form prim-name signature-pred*)
    (if (pair? signature-pred*)
	#`(putprop (quote #,prim-name) CORE-PRIMITIVE-PROPKEY:CORE-TYPE-SIGNATURES (quasiquote #,signature-pred*))
      #'(void)))

  (define (%compose-replacements-output-orm prim-name replacement-prim-name*)
    (if (pair? replacement-prim-name*)
	#`(putprop (quote #,prim-name) CORE-PRIMITIVE-PROPKEY:CALL-REPLACEMENTS (quote #,replacement-prim-name*))
      #'(void)))

  (define (%parse-prim-name stx)
    (if (identifier? stx)
	stx
      (synner "expected identifier as core primitive name" stx)))

  (define (%parse-signatures-sexp stx)
    ;;A SIGNATURES form has the format:
    ;;
    ;;   (signatures ?signature ...)
    ;;
    ;;where each ?SIGNATURE has the format:
    ;;
    ;;   (?rand-types => ?rv-types)
    ;;
    ;;in  which both  ?RAND-TYPES  and  ?RV-TYPES are  proper  or  improper lists  of
    ;;identifiers,  each identifier  representing the  core  type of  an argument  or
    ;;return value; the type identifiers are  defined by the Scheme objects ontology.
    ;;The identifiers  "_" and  "T:object" are  wildcards meaning  "an object  of any
    ;;type".
    ;;
    ;;The specification of  the return value types is the  specification of the types
    ;;of the consumer procedure:
    ;;
    ;;   (call-with-values
    ;;       (lambda ()
    ;;         (?call-to-core-primitive))
    ;;     (lambda ?rv-types
    ;;       (consume-rvs)))
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
    ;;   ((?rand-types . ?rv-types) ...)
    ;;
    ;;in collecting the arguments and return value types; the returned expression has
    ;;a false object in place of the wildcards "_" and "T:object".
    ;;
    (define (%syntax-error)
      (synner "invalid signatures specification in core primitive declaration" stx))
    (syntax-case stx (signatures)
      ((signatures ?signature ...)
       (map (lambda (signature)
	      (syntax-case signature (=>)
		((?rand-types => ?rv-types)
		 (cons (%parse-proper-or-improper-list-of-core-types #'?rand-types)
		       (%parse-proper-or-improper-list-of-core-types #'?rv-types)))
		(_
		 (%syntax-error))))
	 (syntax->list #'(?signature ...))))
      (_
       (%syntax-error))))

  (define (%parse-proper-or-improper-list-of-core-types stx)
    ;;Parse a proper or improper list of core type identifiers representing the types
    ;;of arguments  or return values for  a core primitive.  If  successful: return a
    ;;proper or  improper list of  identifiers and false  objects in which  the false
    ;;objects substitute the  wildcards "_" and "T:object".  If an  invalid syntax is
    ;;found: raise a "&syntax" exception.
    ;;
    (syntax-case stx ()
      ((?type0 ?type ...)
       (map %parse-core-object-type (syntax->list #'(?type0 ?type ...))))
      ((?type0 ?type ... . ?rest-type)
       (let recur ((stx stx))
	 (syntax-case stx ()
	   ((?car . ?cdr)
	    (cons (%parse-core-object-type #'?car) (recur #'?cdr)))
	   (()  '())
	   (?rest
	    (%parse-core-object-type #'?rest)))))
      (?args-type
       (%parse-core-object-type #'?args-type))
      (_
       (synner "invalid signatures component in core primitive declaration" stx))))

  (define (%parse-core-object-type type)
    ;;We accept a  core type identifier and  return it; "T:object" and  "_" mean "any
    ;;type" and we convert them to false.
    (syntax-case type (T:object
		       T:immediate		T:boolean
		       T:number			T:exact			T:inexact
		       T:nonimmediate		T:non-false		T:other-object
		       T:symbol			T:bytevector		T:void
		       T:char			T:null			T:pair
		       T:vector			T:string		T:procedure
		       T:false			T:true			T:other-exact
		       T:fixnum			T:other-inexact		T:flonum
		       T:ratnum			T:bignum		T:compnum
		       T:cflonum		T:other-number
		       T:positive		T:zero			T:negative)
      (T:object			#f)
      (T:immediate		type)
      (T:boolean		type)
      (T:number			type)
      (T:exact			type)
      (T:inexact		type)
      (T:nonimmediate		type)
      (T:non-false		type)
      (T:other-object		type)
      (T:symbol			type)
      (T:bytevector		type)
      (T:void			type)
      (T:char			type)
      (T:null			type)
      (T:pair			type)
      (T:vector			type)
      (T:string			type)
      (T:procedure		type)
      (T:false			type)
      (T:true			type)
      (T:other-exact		type)
      (T:fixnum			type)
      (T:other-inexact		type)
      (T:flonum			type)
      (T:ratnum			type)
      (T:bignum			type)
      (T:compnum		type)
      (T:cflonum		type)
      (T:other-number		type)
      (T:positive		type)
      (T:zero			type)
      (T:negative		type)
      (_
       (if (identifier? type)
	   (if (free-identifier=? type #'_)
	       #f
	     type)
	 (synner "expected identifier as core type name in core primitive declaration" type)))))

  (define (%parse-replacements-sexp stx)
    (syntax-case stx (replacements)
      ((replacements ?replacement-prim-name ...)
       (receive-and-return (replacement-prim-name*)
	   (syntax->list #'(?replacement-prim-name ...))
	 (for-each (lambda (id)
		     (unless (identifier? id)
		       (synner "expected identifier as replacement name in core primitive declaration" id)))
	   replacement-prim-name*)))
      (_
       (synner "invalid replacements specification in core primitive declaration" stx))))

  (define (%signatures->signature-pred* ctx signature*)
    ;;Given a list  of (already parsed) core primitive  signature specifications with
    ;;the format:
    ;;
    ;;   ((?rand-types . ?rv-types) ...)
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
