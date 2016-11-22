;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: helper syntaxes for typed core primitives specification
;;;Date: Tue Jan  5, 2016
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
(library (typed-core-primitives syntaxes)
  (export
    declare-primitive
    declare-core-rtd			declare-core-rcd
    declare-core-type-descriptor

    ;; helpers
    comment				/comment
    section				/section

    ;; declarers
    declare-core-primitive

    declare-type-predicate
    declare-type-predicate/false
    declare-condition-type-predicate
    declare-list-of-type-predicate

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

    declare-hash-function

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
    declare-transcoder-binary-comparison
    declare-struct-binary-comparison

    define-object-unary/multi-comparison-declarer
    declare-number-unary/multi-comparison
    declare-fixnum-unary/multi-comparison
    declare-flonum-unary/multi-comparison
    declare-string-unary/multi-comparison
    declare-pointer-unary/multi-comparison

    define-object-binary/multi-comparison-declarer
    declare-number-binary/multi-comparison
    declare-char-binary/multi-comparison
    declare-string-binary/multi-comparison
    declare-struct-binary/multi-comparison
    declare-transcoder-binary/multi-comparison

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
    declare-unsafe-bytevector-accessor/endianness
    declare-unsafe-bytevector-mutator/endianness
    declare-unsafe-bytevector-conversion

    declare-parameter
    declare-object-retriever

    ;; auxiliary keywords
    safe unsafe
    signatures attributes replacements
    <top> <bottom> <null> <list> <nelist>
    pair pair-of list-of nelist-of vector-of
    alist hashtable enumeration
    type-predicate equality-predicate comparison-procedure hash-function)
  (import (except (vicare)
		  signatures attributes replacements safe unsafe
		  <top> <bottom> <null> <list> <nelist>
		  pair pair-of list-of nelist-of vector-of
		  alist hashtable enumeration
		  type-predicate equality-predicate comparison-procedure hash-function)
    (only (vicare expander)
	  syntax->list
	  all-identifiers?))


;;;; helpers

(define* (declare-primitive prim-name spec)
  ;;Print to  stdout the  symbolic expression representing  the typed  core primitive
  ;;specification.  Use symbol properties to detect duplicated definitions.
  ;;
  (define-constant KEY '$core-prim-typed)
  (cond ((getprop prim-name KEY)
	 => (lambda (sexp)
	      (error __who__ "typed core primitive already defined" prim-name spec sexp)))
	(else
	 (let ((sexp (cons* prim-name KEY spec)))
	   (display sexp)
	   (newline)
	   (putprop prim-name KEY sexp)))))

(define* (declare-primitive-rtd prim-name)
  ;;Print  to  stdout  the  symbolic expression  representing  the  core  record-type
  ;;descriptor  name  specification.   Use  symbol properties  to  detect  duplicated
  ;;definitions.
  ;;
  (define-constant KEY '$core-rtd)
  (cond ((getprop prim-name KEY)
	 => (lambda (sexp)
	      (error __who__ "primitive core RTD already defined" sexp prim-name)))
	(else
	 (let ((sexp (cons* prim-name KEY prim-name)))
	   (display sexp)
	   (newline)
	   (putprop prim-name KEY sexp)))))

(define* (declare-primitive-rcd prim-name)
  ;;Print to stdout the symbolic  expression representing the core record-constructor
  ;;descriptor  name  specification.   Use  symbol properties  to  detect  duplicated
  ;;definitions.
  ;;
  (define-constant KEY '$core-rcd)
  (cond ((getprop prim-name KEY)
	 => (lambda (sexp)
	      (error __who__ "primitive core RCD already defined" sexp prim-name)))
	(else
	 (let ((sexp (cons* prim-name KEY prim-name)))
	   (display sexp)
	   (newline)
	   (putprop prim-name KEY sexp)))))

(define* (declare-primitive-core-type-descriptor prim-name)
  ;;Print to stdout the symbolic  expression representing the core Scheme object-type
  ;;descriptor  name  specification.   Use  symbol properties  to  detect  duplicated
  ;;definitions.
  ;;
  (define-constant KEY '$core-type-descriptor)
  (cond ((getprop prim-name KEY)
	 => (lambda (sexp)
	      (error __who__
		"primitive core Scheme object-type descriptor already defined"
		sexp prim-name)))
	(else
	 (let ((sexp (cons* prim-name KEY prim-name)))
	   (display sexp)
	   (newline)
	   (putprop prim-name KEY sexp)))))

;;; --------------------------------------------------------------------

(define-auxiliary-syntaxes /comment)

(define-syntax comment
  (syntax-rules (/comment)
    ((comment ?form ... /comment)
     (void))))

;;; --------------------------------------------------------------------

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


;;;; built-in object-type descriptors and record-constructor descriptors

(define-syntax (declare-core-type-descriptor stx)
  (syntax-case stx ()
    ((_ ?prim-name)
     (identifier? #'?prim-name)
     #'(declare-primitive-core-type-descriptor (quote ?prim-name)))))

(define-syntax (declare-core-rtd stx)
  (syntax-case stx ()
    ((_ ?prim-name)
     (identifier? #'?prim-name)
     #'(declare-primitive-rtd (quote ?prim-name)))))

(define-syntax (declare-core-rcd stx)
  (syntax-case stx ()
    ((_ ?prim-name)
     (identifier? #'?prim-name)
     #'(declare-primitive-rcd (quote ?prim-name)))))


;;;; basic definition syntaxes

(define-auxiliary-syntaxes signatures attributes replacements
  safe unsafe
  <top> <bottom> <null> <list> <nelist>
  pair pair-of list-of nelist-of vector-of
  alist hashtable enumeration
  type-predicate equality-predicate comparison-procedure hash-function)

(define-syntax (declare-core-primitive stx)
  (define (main input-form.stx)
    (syntax-case input-form.stx (replacements signatures attributes)
      ;; signatures
      ((_ ?prim-name ?safety (signatures . ?signatures))
       (%output #'?prim-name #'?safety #'?signatures '()))

      ;; signatures, replacements
      ((_ ?prim-name ?safety (signatures . ?signatures) (replacements . ?replacements))
       (%output #'?prim-name #'?safety #'?signatures #'?replacements))

      ;; signatures, attributes
      ((_ ?prim-name ?safety (signatures . ?signatures) (attributes . ?stuff))
       (%output #'?prim-name #'?safety #'?signatures '()))

      ;; signatures, attributes, replacements
      ((_ ?prim-name ?safety (signatures . ?signatures) (attributes . ?stuff) (replacements . ?replacements))
       (%output #'?prim-name #'?safety #'?signatures #'?replacements))
      ))

  (define (%output prim-name.id safety.id signatures.stx replacements.stx)
    ;;The syntactic  binding's descriptor's value is  a vector: it is  more efficient
    ;;than a list, memory-wise.
    ;;
    (with-syntax
	((PRIM-NAME		prim-name.id)
	 (SAFETY		(%validate-safety safety.id))
	 ((SIGNATURE ...)	(%validate-and-format-signatures signatures.stx))
	 ((REPLACEMENT ...)	(%validate-replacements replacements.stx)))
      #'(declare-primitive (quote PRIM-NAME) (quote #(PRIM-NAME SAFETY #(SIGNATURE ...) #(REPLACEMENT ...))))))

  (define (%validate-safety safety.stx)
    (syntax-case safety.stx (safe unsafe)
      ((safe)		#t)
      ((unsafe)		#f)
      (_
       (synner "invalid safety specification" safety.stx))))

  (define (%validate-and-format-signatures signatures.stx)
    ;;Validate SIGNATURES.STX  as syntax  object representing  a list  of CASE-LAMBDA
    ;;clauses type signatures:
    ;;
    ;;   ((?argvals-signature0 => ?retvals-signature0)
    ;;    (?argvals-signature  => ?retvals-signature)
    ;;    ...)
    ;;
    ;;When successful, return a syntax object representing a list with the format:
    ;;
    ;;   ((?retvals-signature0 . ?argvals-signature0)
    ;;    (?retvals-signature  . ?argvals-signature)
    ;;    ...)
    ;;
    ;;in which the order of the signatures is unchanged.
    ;;
    (syntax-case signatures.stx (=>)
      (()
       '())

      (((?argvals-signature => ?retvals-signature) . ?rest)
       (cons (cons (%validate-type-signature #'?retvals-signature)
		   (%validate-type-signature #'?argvals-signature))
	     (%validate-and-format-signatures #'?rest)))

      ((?signature . ?rest)
       (synner "invalid signature specification" #'?signature))))

  (define (%validate-type-signature type-signature.stx)
    ;;Validate TYPE-SIGNATURE.STX as syntax object representing a type signature:
    ;;
    ;;   (?type ...)
    ;;   (?type ... . ?tail-type)
    ;;   ?type
    ;;
    ;;When successful:  return a proper or  improper list representing the  same type
    ;;signature, with the identifiers "_" replaced with "<top>" or "<list>".
    ;;
    (let recur ((sig type-signature.stx))
      (syntax-case sig (<bottom> <null> <list> <nelist> list list-of pair pair-of)
	(()
	 '())

	(<bottom>
	 sig)

	(<null>
	 sig)

	(<list>
	 sig)

	(<nelist>
	 sig)

	(?type
	 (and (identifier? #'?type)
	      (free-identifier=? #'_ #'?type))
	 (%replace-type-alias #'?type #t))

	((list ?item-type0 ?item-type ...)
	 #`(list . #,(map %validate-type-annotation (syntax->list #'(?item-type0 ?item-type ...)))))

	((list-of ?item-type)
	 #`(list-of #,(%validate-type-annotation #'?item-type)))

	((pair-of ?item-type)
	 #`(pair-of #,(%validate-type-annotation #'?item-type)))

	((pair ?car-type ?cdr-type)
	 #`(pair #,(%validate-type-annotation #'?car-type)
		 #,(%validate-type-annotation #'?cdr-type)))

	((?car . ?cdr)
	 (cons (%validate-type-annotation #'?car)
	       (recur #'?cdr)))

	(_
	 (synner "invalid type signature" type-signature.stx)))))

  (define (%validate-type-annotation type-annotation.stx)
    (syntax-case type-annotation.stx
	(pair list vector pair-of list-of nelist-of vector-of <list>
	      condition or and not alist hashtable lambda case-lambda =>
	      enumeration type-predicate equality-predicate comparison-procedure hash-function)

      (?type
       (identifier? #'?type)
       (%replace-type-alias #'?type))

      ((pair-of ?item-type)
       #`(pair-of #,(%validate-type-annotation #'?item-type)))

      ((list-of ?item-type)
       #`(list-of #,(%validate-type-annotation #'?item-type)))

      ((nelist-of ?item-type)
       (%validate-type-annotation #'(pair ?item-type (list-of ?item-type))))

      ((vector-of ?item-type)
       #`(vector-of #,(%validate-type-annotation #'?item-type)))

      ((pair ?car-type ?cdr-type)
       #`(pair #,(%validate-type-annotation #'?car-type)
	       #,(%validate-type-annotation #'?cdr-type)))

      ((list ?item-type0 ?item-type ...)
       #`(list . #,(map %validate-type-annotation (syntax->list #'(?item-type0 ?item-type ...)))))

      ((vector ?item-type0 ?item-type ...)
       #`(vector . #,(map %validate-type-annotation (syntax->list #'(?item-type0 ?item-type ...)))))

      ((hashtable ?key-type ?value-type)
       (list #'hashtable
	     (%validate-type-annotation #'?key-type)
	     (%validate-type-annotation #'?value-type)))

      ((alist ?key-type ?value-type)
       (list #'alist
	     (%validate-type-annotation #'?key-type)
	     (%validate-type-annotation #'?value-type)))

      ((enumeration ?symbol0 ?symbol ...)
       (all-identifiers? #'(?symbol0 ?symbol ...))
       (cons #'enumeration #'(?symbol0 ?symbol ...)))

      ((lambda ?argtypes => ?rettypes)
       (list #'lambda (%validate-type-signature #'?argtypes) #'=> (%validate-type-signature #'?rettypes)))

      ((case-lambda
	 (?argtypes0 => ?rettypes0)
	 (?argtypes  => ?rettypes)
	 ...)
       (let ((argtypes*.stx (map %validate-type-signature (syntax->list #'(?argtypes0 ?argtypes ...))))
	     (rettypes*.stx (map %validate-type-signature (syntax->list #'(?rettypes0 ?rettypes ...)))))
	 (cons #'case-lambda
	       (map (lambda (argtypes.stx rettypes.stx)
		      (list argtypes.stx #'=> rettypes.stx))
		 argtypes*.stx rettypes*.stx))))

      ((condition ?item-type0 ?item-type ...)
       #`(condition . #,(map %validate-type-annotation (syntax->list #'(?item-type0 ?item-type ...)))))

      ((or ?item-type0 ?item-type ...)
       #`(or . #,(map %validate-type-annotation (syntax->list #'(?item-type0 ?item-type ...)))))

      ((and ?item-type0 ?item-type ...)
       #`(and . #,(map %validate-type-annotation (syntax->list #'(?item-type0 ?item-type ...)))))

      ((not ?item-type)
       #`(not #,(%validate-type-annotation #'?item-type)))

      ((parent-of ?item-type)
       #`(parent-of #,(%validate-type-annotation #'?item-type)))

      ((ancestor-of ?item-type)
       #`(ancestor-of #,(%validate-type-annotation #'?item-type)))

      ((type-predicate ?type)
       #`(type-predicate #,(%validate-type-annotation #'?type)))

      ((equality-predicate ?type)
       #`(equality-predicate #,(%validate-type-annotation #'?type)))

      ((comparison-procedure ?type)
       #`(comparison-procedure #,(%validate-type-annotation #'?type)))

      ((hash-function ?type)
       #`(hash-function #,(%validate-type-annotation #'?type)))

      (_
       (synner "invalid type annotation" type-annotation.stx))))

  (case-define %replace-type-alias
    ((id)
     (%replace-type-alias id #f))
    ((id in-tail-position?)
     (cond ((free-identifier=? #'_ id)
	    (if in-tail-position?
		#'<list>
	      ;;The underscore is fine as type annotation.
	      id))
	   ;;FIXME  This  is   a  temporary  substitution.   When   type  unions  are
	   ;;implemented  we should  remove  this  and use  a  proper definition  for
	   ;;"<syntax-object>".  (Marco Maggi; Sun Dec 27, 2015)
	   ((free-identifier=? #'<syntax-object> id)
	    #'<top>)
	   (else id))))

  (define (%validate-replacements replacements.stx)
    ;;Validate REPLACEMENTS.STX as syntax object  representing a list of identifiers.
    ;;When successful: return REPLACEMENTS.STX itself.
    ;;
    (syntax-case replacements.stx ()
      ((?replacement ...)
       (all-identifiers? replacements.stx)
       replacements.stx)
      (_
       (synner "invalid replacements specification" replacements.stx))))

  (case-define synner
    ((message subform)
     (syntax-violation 'declare-core-primitive message stx subform))
    ((message)
     (syntax-violation 'declare-core-primitive message stx)))

  (main stx))


;;;; syntax helpers: type predicates

(define-syntax declare-type-predicate
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate fixnum? <fixnum>)
  ;;   (declare-type-predicate vector? <vector>)
  ;;   (declare-type-predicate exact-integer? <exact-integer>)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((<top>)				=> (<boolean>)))))
    ((_ ?who ?obj-tag)
     (declare-core-primitive ?who
	 (safe)
       ;;NOTE This way of writing the signature  of a type predicate (with the use of
       ;;ANCESTOR-OF) works here when there are no labels and unions in the hierarchy
       ;;of ?TYPE; othewise things get messy.  (Marco Maggi; Fri Apr 29, 2016)
       (signatures
	((?obj-tag)				=> (<true>))
	(((ancestor-of ?obj-tag))		=> (<boolean>))
	(((not ?obj-tag))			=> (<false>))
	((<top>)				=> (<boolean>))
	#| end of SIGNATURES |# )
       (attributes
	((_)		foldable effect-free))))
    ))

(define-syntax declare-type-predicate/false
  ;;Usage examples:
  ;;
  ;;   (declare-type-predicate/false false-or-pointer? <pointer>)
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag)
     (declare-core-primitive ?who
	 (safe)
       ;;NOTE This way of writing the signature  of a type predicate (with the use of
       ;;ANCESTOR-OF) works here when there are no labels and unions in the hierarchy
       ;;of ?TYPE; othewise things get messy.  (Marco Maggi; Fri Apr 29, 2016)
       (signatures
	(((or ?obj-tag <false>))		=> (<true>))
	(((or (ancestor-of ?obj-tag)
	      (ancestor-of <false>)))		=> (<boolean>))
	(((and (not ?obj-tag)
	       (not <false>)))			=> (<boolean>))
	((<top>)				=> (<boolean>)))
       (attributes
	((_)		foldable effect-free))))
    ))

(define-syntax declare-condition-type-predicate
  ;;Usage examples:
  ;;
  ;;   (declare-condition-type-predicate error? &error)
  ;;
  (syntax-rules ()
    ((_ ?who ?tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((?tag)				=> (<true>))
	(((or (ancestor-of ?tag)
	      <compound-condition>))	=> (<boolean>))
	(((not ?tag))			=> (<false>)))
       (attributes
	((_)				foldable effect-free))))
    ))

(define-syntax declare-list-of-type-predicate
  ;;Usage examples:
  ;;
  ;;   (declare-list-of-type-predicate list-of-fixnums? <fixnum>)
  ;;
  (syntax-rules ()
    ((_ ?who ?obj-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(((list-of ?obj-tag))			=> (<true>))
	(((list-of (ancestor-of ?obj-tag)))	=> (<boolean>))
	(((list-of (not ?obj-tag)))		=> (<false>))
	(((and (not <list>)
	       (not <pair>)))			=> (<false>))
	((<top>)				=> (<boolean>))
	#| end of SIGNATURES |# )
       (attributes
	((_)		foldable effect-free))))
    ))

;;; --------------------------------------------------------------------

(module (define-object-predicate-declarer)

  (define-syntax define-object-predicate-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-predicate-declarer declare-number-predicate <number>)
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
	  ((?obj-tag)		=> (<boolean>)))
	 (attributes
	  ((_)			foldable effect-free))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-PREDICATE-DECLARER |# )

(define-object-predicate-declarer declare-object-predicate		<top>)

(define-object-predicate-declarer declare-fixnum-predicate		<fixnum>)
(define-object-predicate-declarer declare-bignum-predicate		<bignum>)
(define-object-predicate-declarer declare-flonum-predicate		<flonum>)
(define-object-predicate-declarer declare-ratnum-predicate		<ratnum>)
(define-object-predicate-declarer declare-compnum-predicate		<compnum>)
(define-object-predicate-declarer declare-cflonum-predicate		<cflonum>)
(define-object-predicate-declarer declare-number-predicate		<number>)

(define-object-predicate-declarer declare-char-predicate		<char>)
(define-object-predicate-declarer declare-string-predicate		<string>)
(define-object-predicate-declarer declare-keyword-predicate		<keyword>)
(define-object-predicate-declarer declare-vector-predicate		<vector>)
(define-object-predicate-declarer declare-bytevector-predicate		<bytevector>)
(define-object-predicate-declarer declare-struct-predicate		<struct>)
(define-object-predicate-declarer declare-record-predicate		<record>)
(define-object-predicate-declarer declare-port-predicate		<port>)


;;;; syntax helpers: hash functions

(define-syntax-rule (declare-hash-function ?prim-name ?type ?safety)
  (declare-core-primitive ?prim-name
      (?safety)
    (signatures
     ((?type)		=> (<non-negative-fixnum>)))))


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
	  ((?type-tag ?type-tag)	=> (<boolean>)))
	 (attributes
	  ((_ _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-BINARY-COMPARISON-DECLARER |# )

(define-object-binary-comparison-declarer declare-object-binary-comparison <top>)
(define-object-binary-comparison-declarer declare-fixnum-binary-comparison <fixnum>)
(define-object-binary-comparison-declarer declare-flonum-binary-comparison <flonum>)
(define-object-binary-comparison-declarer declare-number-binary-comparison <number>)
(define-object-binary-comparison-declarer declare-pointer-binary-comparison <pointer>)
(define-object-binary-comparison-declarer declare-char-binary-comparison <char>)
(define-object-binary-comparison-declarer declare-transcoder-binary-comparison <transcoder>)
(define-object-binary-comparison-declarer declare-string-binary-comparison <string>)
(define-object-binary-comparison-declarer declare-keyword-binary-comparison <keyword>)
(define-object-binary-comparison-declarer declare-bytevector-binary-comparison <bytevector>)
(define-object-binary-comparison-declarer declare-struct-binary-comparison <struct>)

;;; --------------------------------------------------------------------

(module (define-object-unary/multi-comparison-declarer)

  (define-syntax define-object-unary/multi-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/multi-comparison-declarer declare-flonum-unary/multi-comparison <flonum>)
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
	  ((?type-tag)					=> (<boolean>))
	  ((?type-tag ?type-tag)			=> (<boolean>))
	  ((?type-tag ?type-tag . (list-of ?type-tag))	=> (<boolean>)))
	 (attributes
	  ((_)				foldable effect-free)
	  ((_ _)			foldable effect-free)
	  ((_ _ . _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/MULTI-COMPARISON-DECLARER |# )

(define-object-unary/multi-comparison-declarer declare-number-unary/multi-comparison <number>)
(define-object-unary/multi-comparison-declarer declare-fixnum-unary/multi-comparison <fixnum>)
(define-object-unary/multi-comparison-declarer declare-flonum-unary/multi-comparison <flonum>)
(define-object-unary/multi-comparison-declarer declare-string-unary/multi-comparison <string>)
(define-object-unary/multi-comparison-declarer declare-pointer-unary/multi-comparison <pointer>)

;;; --------------------------------------------------------------------

(module (define-object-binary/multi-comparison-declarer)

  (define-syntax define-object-binary/multi-comparison-declarer
    ;;Usage examples:
    ;;
    ;;   (define-object-binary/multi-comparison-declarer declare-char-binary/multi-comparison <char>)
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
	  ((?type-tag ?type-tag)			=> (<boolean>))
	  ((?type-tag ?type-tag . (list-of ?type-tag))	=> (<boolean>)))
	 (attributes
	  ((_ _)			foldable effect-free)
	  ((_ _ . _)			foldable effect-free))))
      ))

  #| end of module: DEFINE-OBJECT-BINARY/MULTI-COMPARISON-DECLARER |# )

(define-object-binary/multi-comparison-declarer declare-number-binary/multi-comparison <number>)
(define-object-binary/multi-comparison-declarer declare-char-binary/multi-comparison <char>)
(define-object-binary/multi-comparison-declarer declare-string-binary/multi-comparison <string>)
(define-object-binary/multi-comparison-declarer declare-transcoder-binary/multi-comparison <transcoder>)
(define-object-binary/multi-comparison-declarer declare-struct-binary/multi-comparison <struct>)


;;;; syntax helpers: math operations

(module (define-object-unary-operation-declarer)

  (define-syntax (define-object-unary-operation-declarer stx)
    ;;Usage example:
    ;;
    ;;   (define-object-unary-operation-declarer declare-flonum-unary <flonum>)
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

(define-object-unary-operation-declarer declare-number-unary <number>)
(define-object-unary-operation-declarer declare-fixnum-unary <fixnum>)
(define-object-unary-operation-declarer declare-flonum-unary <flonum>)
(define-object-unary-operation-declarer declare-exact-integer-unary <exact-integer>)
(define-object-unary-operation-declarer declare-char-unary <char>)
(define-object-unary-operation-declarer declare-string-unary <string>)

;;; --------------------------------------------------------------------

(module (define-object-binary-operation-declarer)

  (define-syntax (define-object-binary-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-binary-operation-declarer declare-flonum-binary <flonum>)
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

(define-object-binary-operation-declarer declare-number-binary <number>)
(define-object-binary-operation-declarer declare-fixnum-binary <fixnum>)
(define-object-binary-operation-declarer declare-flonum-binary <flonum>)
(define-object-binary-operation-declarer declare-string-binary <string>)

;;; --------------------------------------------------------------------

(module (define-object-unary/binary-operation-declarer)

  (define-syntax (define-object-unary/binary-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/binary-operation-declarer declare-flonum-unary/binary <flonum>)
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

(define-object-unary/binary-operation-declarer declare-number-unary/binary <number>)
(define-object-unary/binary-operation-declarer declare-fixnum-unary/binary <fixnum>)
(define-object-unary/binary-operation-declarer declare-flonum-unary/binary <flonum>)
(define-object-unary/binary-operation-declarer declare-string-unary/binary <string>)

;;; --------------------------------------------------------------------

(module (define-object-unary/multi-operation-declarer)

  (define-syntax (define-object-unary/multi-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-unary/multi-operation-declarer declare-flonum-unary/multi <flonum>)
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
	  ((?type-tag)				=> (?type-tag))
	  ((?type-tag . (list-of ?type-tag))	=> (?type-tag)))
	 (attributes
	  ((_)				foldable effect-free result-true)
	  ((_ . _)			foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-UNARY/MULTI-OPERATION-DECLARER |# )

(define-object-unary/multi-operation-declarer declare-number-unary/multi <number>)
(define-object-unary/multi-operation-declarer declare-fixnum-unary/multi <fixnum>)
(define-object-unary/multi-operation-declarer declare-flonum-unary/multi <flonum>)
(define-object-unary/multi-operation-declarer declare-string-unary/multi <string>)

;;; --------------------------------------------------------------------

(module (define-object-multi-operation-declarer)

  (define-syntax (define-object-multi-operation-declarer stx)
    ;;Usage examples:
    ;;
    ;;   (define-object-multi-operation-declarer declare-fixnum-multi <fixnum>)
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
	  ((list-of ?type-tag)	=> (?type-tag)))
	 (attributes
	  (()			foldable effect-free result-true)
	  ((_ . _)		foldable effect-free result-true))
	 (replacements . ?replacements)))
      ))

  #| end of module: DEFINE-OBJECT-MULTI-OPERATION-DECLARER |# )

(define-object-multi-operation-declarer declare-number-multi <number>)
(define-object-multi-operation-declarer declare-fixnum-multi <fixnum>)
(define-object-multi-operation-declarer declare-flonum-multi <flonum>)
(define-object-multi-operation-declarer declare-string-multi <string>)

;;; --------------------------------------------------------------------

(define-syntax declare-unsafe-unary-operation
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-unary-operation $neg-fixnum <fixnum> <exact-integer>)
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
  ;;   (declare-unsafe-unary-operation/2rv $exact-integer-sqrt-fixnum <fixnum> <exact-integer> <exact-integer>)
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
  ;;   (declare-unsafe-binary-operation $add-fixnum-fixnum <fixnum> <fixnum> <exact-integer>)
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
  ;;   (declare-unsafe-binary-operation $quotient+remainder-fixnum-fixnum <fixnum> <fixnum> <exact-integer> <fixnum>)
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
	  ((<pair>)		=> (<top>))
	  ((<nelist>)		=> (<top>)))
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
	  ((<pair> <top>)		=> (<void>))
	  ((<nelist> <top>)		=> (<top>)))
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
	  ((?obj-tag <list>)	=> (<top>)))
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
	((?obj-tag <null>)	=> (<false>))
	((?obj-tag <nelist>)	=> (<top>)))
       (attributes
	((_ ())			foldable effect-free result-false)
	((_ _)			foldable effect-free))))
    ))


;;;; syntax helpers: bytevectors

(define-syntax declare-safe-bytevector-conversion
  ;;Usage examples:
  ;;
  ;;   (declare-safe-bytevector-conversion uri-encode <bytevector>)
  ;;   (declare-safe-bytevector-conversion uri-decode <bytevector>)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	((<bytevector>)		=> (?return-value-tag)))
       (attributes
	;;Not foldable because it must return a new bytevector every time.
	((_)			effect-free result-true))))
    ))

;;; --------------------------------------------------------------------

(define-syntax declare-unsafe-bytevector-accessor
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-accessor $bytevector-u8-ref <non-negative-fixnum>)
  ;;   (declare-unsafe-bytevector-accessor $bytevector-s8-ref <fixnum>)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((<nebytevector> <non-negative-fixnum>)	=> (?return-value-tag)))
       (attributes
	((_ _)			foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-bytevector-accessor/endianness
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-accessor/endianness $bytevector-u16-ref <non-negative-fixnum>)
  ;;   (declare-unsafe-bytevector-accessor/endianness $bytevector-s16-ref <fixnum>)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((<nebytevector> <non-negative-fixnum> <symbol>)	=> (?return-value-tag)))
       (attributes
	((_ _)			foldable effect-free result-true))))
    ))

(define-syntax declare-unsafe-bytevector-mutator
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-mutator $bytevector-set! <fixnum>)
  ;;
  (syntax-rules ()
    ((_ ?who ?new-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((<nebytevector> <non-negative-fixnum> ?new-value-tag)	=> (<void>)))))
    ))

(define-syntax declare-unsafe-bytevector-mutator/endianness
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-mutator/endianness $bytevector-u16-set! <fixnum>)
  ;;
  (syntax-rules ()
    ((_ ?who ?new-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((<nebytevector> <non-negative-fixnum> ?new-value-tag <symbol>)	=> (<void>)))))
    ))

(define-syntax declare-unsafe-bytevector-conversion
  ;;Usage examples:
  ;;
  ;;   (declare-unsafe-bytevector-conversion $uri-encode <bytevector>)
  ;;   (declare-unsafe-bytevector-conversion $uri-decode <bytevector>)
  ;;
  (syntax-rules ()
    ((_ ?who ?return-value-tag)
     (declare-core-primitive ?who
	 (unsafe)
       (signatures
	((<empty-bytevector>)	=> (<empty-bytevector>))
	((<bytevector>)		=> (?return-value-tag)))
       (attributes
	;;Not foldable because it must return a new bytevector every time.
	((_)			effect-free result-true))))
    ))


;;;; syntax helpers: miscellaneous

(define-syntax declare-parameter
  ;;Usage examples:
  ;;
  ;;   (declare-parameter current-input-port	<textual-input-port>)
  ;;   (declare-parameter native-transcoder	<transcoder>)
  ;;
  (syntax-rules ()
    ((_ ?who)
     (declare-parameter ?who <top>))
    ((_ ?who ?value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(()			=> (?value-tag))
	((?value-tag)		=> (<void>))
	((?value-tag <boolean>)	=> (<void>)))
       (attributes
	(()			effect-free)
	((_)			result-true)
	((_ _)			result-true))))
    ))

(define-syntax declare-object-retriever
  ;;Usage examples:
  ;;
  ;;   (declare-object-retriever console-input-port <binary-input-port>)
  ;;   (declare-object-retriever native-eol-style foldable <symbol>)
  ;;
  ;;NOTE The returned object must *not* be false.
  ;;
  (syntax-rules (foldable)
    ((_ ?who foldable)
     (declare-object-retriever ?who foldable <top>))
    ((_ ?who foldable ?return-value-tag)
     (declare-core-primitive ?who
	 (safe)
       (signatures
	(()		=> (?return-value-tag)))
       (attributes
	(()		foldable effect-free result-true))))

    ((_ ?who)
     (declare-object-retriever ?who <top>))
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
;; Local Variables:
;; mode: vicare
;; End:
