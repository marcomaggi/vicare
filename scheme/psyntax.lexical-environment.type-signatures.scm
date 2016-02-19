;;;Copyright (c) 2010-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(module PSYNTAX-TYPE-SIGNATURES
  (
   <type-signature>
   <type-signature>-rtd					<type-signature>-rcd
   make-type-signature					type-signature?
   type-signature-tags

;;; special constructors
   make-type-signature/single-top			make-type-signature/single-void
   make-type-signature/single-boolean			make-type-signature/single-procedure
   make-type-signature/single-stx			make-type-signature/single-syntactic-identifier
   make-type-signature/standalone-list			make-type-signature/fully-untyped
   make-type-signature/single-value

;;; comparison
   type-signature=?

;;; predicates
   type-signature.fully-untyped?			type-signature.partially-untyped?
   type-signature.untyped?
   type-signature.super-and-sub?
   type-signature.single-type?				type-signature.single-top-tag?
   type-signature.single-type-or-fully-untyped?

;;; accessors
   type-signature.min-count				type-signature.max-count
   type-signature.min-and-max-counts

   type-signature.common-ancestor			datum-type-signature

   #| end of exports |# )

  (import PSYNTAX-TYPE-IDENTIFIERS)
  (import PSYNTAX-TYPE-SYNTAX-OBJECTS)


;;;; type signature: type definition

(define-record-type (<type-signature> make-type-signature type-signature?)
  (nongenerative vicare:expander:<type-signature>)
  (fields
    (immutable tags	type-signature-tags)
		;A  fully  unwrapped  syntax  object representing  a  type  signature
		;according to SYNTAX-OBJECT.TYPE-SIGNATURE?.
    (mutable memoised-fully-untyped?		type-signature.memoised-fully-untyped?		type-signature.memoised-fully-untyped?-set!)
    (mutable memoised-partially-untyped?	type-signature.memoised-partially-untyped?	type-signature.memoised-partially-untyped?-set!)
    (mutable memoised-untyped?			type-signature.memoised-untyped?		type-signature.memoised-untyped?-set!)
    (mutable memoised-min-count			type-signature.memoised-min-count		type-signature.memoised-min-count-set!)
		;Memoised minimum number of values matching this signature.
    (mutable memoised-max-count			type-signature.memoised-max-count		type-signature.memoised-max-count-set!)
		;Memoised maximum number of values matching this signature.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-type-signature {tags syntax-object.type-signature?})
	(make-record (syntax-unwrap tags) (void) (void) (void) #f #f))
      make-type-signature))
  (custom-printer
    (lambda (S port sub-printer)
      (sub-printer `(<type-signature> ,(type-signature-tags S))))))

(define <type-signature>-rtd
  (record-type-descriptor <type-signature>))

(define <type-signature>-rcd
  (record-constructor-descriptor <type-signature>))


;;;; type signature: special constructors

(let*-syntax
    ((define-cached-signature-maker
       (syntax-rules ()
	 ((_ ?who ?stx-maker)
	  (define ?who
	    (let ((rvs #f))
	      (lambda ()
		(or rvs
		    (receive-and-return (S)
			(make-type-signature ?stx-maker)
		      (set! rvs S)))))))))
     (define-single-type-signature-maker
       (syntax-rules ()
	 ((_ ?who ?type-id-maker)
	  (define-cached-signature-maker ?who (list (?type-id-maker)))))))
  (define-single-type-signature-maker make-type-signature/single-top			top-type-id)
  (define-single-type-signature-maker make-type-signature/single-void			void-type-id)
  (define-single-type-signature-maker make-type-signature/single-boolean		boolean-type-id)
  (define-single-type-signature-maker make-type-signature/single-procedure		procedure-type-id)
  (define-single-type-signature-maker make-type-signature/single-predicate		predicate-type-id)
  (define-single-type-signature-maker make-type-signature/single-stx			stx-type-id)
  (define-single-type-signature-maker make-type-signature/single-syntactic-identifier	syntactic-identifier-type-id)
  (define-cached-signature-maker make-type-signature/standalone-list			(list-type-id))
  #| end of LET-SYNTAX |# )

(define-syntax-rule (make-type-signature/fully-untyped)
  (make-type-signature/standalone-list))

(define* (make-type-signature/single-value {type type-identifier?})
  (make-type-signature (list type)))


;;;; type signature: predicates

(define* (type-signature.fully-untyped? {signature type-signature?})
  ;;Return true  if the type  signature specifies  neither object types,  nor objects
  ;;count; otherwise return false.
  ;;
  (let ((obj (type-signature.memoised-fully-untyped? signature)))
    (if (void-object? obj)
	(receive-and-return (bool)
	    (syntax-object.type-signature.fully-untyped? (type-signature-tags signature))
	  (type-signature.memoised-fully-untyped?-set! signature bool))
      obj)))

(define* (type-signature.partially-untyped? {signature type-signature?})
  ;;Return true if the type signature as at least one untyped item, either "<top>" or
  ;;"<list>"; otherwise return false.
  ;;
  (let ((obj (type-signature.memoised-partially-untyped? signature)))
    (if (void-object? obj)
	(receive-and-return (bool)
	    (syntax-object.type-signature.partially-untyped? (type-signature-tags signature))
	  (type-signature.memoised-partially-untyped?-set! signature bool))
      obj)))

(define* (type-signature.untyped? {signature type-signature?})
  ;;Return  true if  the type  signature  as only  untyped items,  either "<top>"  or
  ;;"<list>"; otherwise return false.
  ;;
  (let ((obj (type-signature.memoised-untyped? signature)))
    (if (void-object? obj)
	(receive-and-return (bool)
	    (syntax-object.type-signature.untyped? (type-signature-tags signature))
	  (type-signature.memoised-untyped?-set! signature bool))
      obj)))

(case-define* type-signature.super-and-sub?
  ((super-signature sub-signature)
   (type-signature.super-and-sub? super-signature sub-signature (current-inferior-lexenv)))
  (({super-signature type-signature?} {sub-signature type-signature?} lexenv)
   ;;Return true if SUPER-SIGNATURE and SUB-SIGNATURE have the same structure and the
   ;;identifiers in  the homologous position  are super-type and  sub-type; otherwise
   ;;return false.
   ;;
   (syntax-object.type-signature.super-and-sub? (type-signature-tags super-signature)
						(type-signature-tags sub-signature)
						lexenv)))

(define* (type-signature.single-type? {signature type-signature?})
  ;;Return  true if  SIGNATURE represents  a  single return  value; otherwise  return
  ;;false.
  ;;
  (syntax-object.type-signature.single-identifier? (type-signature-tags signature)))

(define* (type-signature.single-top-tag? {signature type-signature?})
  ;;Return  true if  SIGNATURE represents  a single  return value  with tag  "<top>",
  ;;otherwise return false.
  ;;
  (syntax-match (type-signature-tags signature) (<top>)
    ((<top>)  #t)
    (_        #f)))

(define* (type-signature.single-type-or-fully-untyped? {signature type-signature?})
  ;;Return true if SIGNATURE represents a single return value or it is the standalone
  ;;"<list>" identifier, otherwise return false.
  ;;
  (syntax-match (type-signature-tags signature) (<list>)
    ((?tag)	#t)
    (<list>	#t)
    (_		#f)))


;;;; type signature: accessors

(define* (type-signature.min-count {signature type-signature?})
  ;;Return a non-negative  fixnum representing the minimum number of  values that can
  ;;match the signature.
  ;;
  (or (type-signature.memoised-min-count signature)
      (receive (min-count max-count)
	  (type-signature.min-and-max-counts signature)
	min-count)))

(define* (type-signature.max-count {signature type-signature?})
  ;;Return a non-negative  fixnum representing the maximum number of  values that can
  ;;match the signature; if the number of values is infinite: return +inf.0.
  ;;
  (or (type-signature.memoised-max-count signature)
      (receive (min-count max-count)
	  (type-signature.min-and-max-counts signature)
	max-count)))

(define (type-signature.min-and-max-counts signature)
  (receive-and-return (min-count max-count)
      (syntax-object.type-signature.min-and-max-count (type-signature-tags signature))
    (type-signature.memoised-min-count-set! signature min-count)
    (type-signature.memoised-max-count-set! signature max-count)))


;;;; type signature: comparison

(define* (type-signature=? {signature1 type-signature?} {signature2 type-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  (define (%syntax=? stx1 stx2)
    (cond ((and (identifier? stx1)
		(identifier? stx2))
	   (type-identifier=? stx1 stx2))
	  ((and (pair? stx1)
		(pair? stx2))
	   (and (type-identifier=? (car stx1) (car stx2))
		(%syntax=?         (cdr stx1) (cdr stx2))))
	  (else
	   (and (null? stx1)
		(null? stx2)))))
  (%syntax=? (type-signature-tags signature1)
	     (type-signature-tags signature2)))


;;;; type signature: inspection

(case-define* type-signature.common-ancestor
  ;;Given a  multitude of type signatures:  return a new type  signature representing
  ;;their common ancestor.
  ;;
  (({sig type-signature?})
   sig)

  (({sig1 type-signature?} {sig2 type-signature?})
   (make-type-signature
    (syntax-object.type-signature.common-ancestor (type-signature-tags sig1)
						  (type-signature-tags sig2))))
  (({sig1 type-signature?} {sig2 type-signature?} . sig*)
   (fold-left (lambda (sig-a sig-b)
		(type-signature.common-ancestor sig-a sig-b))
     (type-signature.common-ancestor sig1 sig2)
     sig*)))


;;;; helpers and utilities

(define (datum-type-signature datum)
  ;;Build and  return a new instance  of "<type-signature>" representing the  type of
  ;;the single value returned by the expression  DATUM, which must be a Scheme object
  ;;extracted from a syntax object representing a literal expression.
  ;;
  (make-type-signature
   (list (cond ((boolean? datum)	(cond (datum
					       (core-prim-id '<true>))
					      (else
					       (core-prim-id '<false>))))
	       ((char?    datum)	(core-prim-id '<char>))
	       ((symbol?  datum)	(core-prim-id '<symbol>))
	       ((keyword? datum)	(core-prim-id '<keyword>))

	       ((fixnum?  datum)	(cond ((fxpositive? datum)
					       (core-prim-id '<positive-fixnum>))
					      ((fxzero? datum)
					       (core-prim-id '<zero-fixnum>))
					      (else
					       (core-prim-id '<fixnum>))))
	       ((flonum?  datum)	(cond ((flpositive? datum)
					       (core-prim-id '<positive-flonum>))
					      ((flzero?/positive datum)
					       (core-prim-id '<positive-zero-flonum>))
					      ((flzero?/negative datum)
					       (core-prim-id '<negative-zero-flonum>))
					      (else
					       (core-prim-id '<flonum>))))
	       ((ratnum?  datum)	(core-prim-id '<ratnum>))
	       ((bignum?  datum)	(core-prim-id '<bignum>))
	       ((compnum? datum)	(cond ((exact-compnum? datum)
					       (core-prim-id '<exact-compnum>))
					      (else
					       (core-prim-id '<compnum>))))
	       ((cflonum? datum)	(core-prim-id '<cflonum>))

	       ((string?  datum)	(core-prim-id '<string>))
	       ((vector?  datum)	(core-prim-id '<vector>))
	       ((bytevector? datum)	(core-prim-id '<bytevector>))

	       ((list?    datum)	(cond ((null? datum)		(core-prim-id '<null>))
					      ((for-all char?   datum)	(core-prim-id '<char*>))
					      ((for-all string? datum)	(core-prim-id '<string*>))
					      ((for-all symbol? datum)	(core-prim-id '<symbol*>))
					      (else			(core-prim-id '<nlist>))))
	       ((pair?    datum)	(cond ((standalone-pair? datum)
					       (core-prim-id '<standalone-pair>))
					      (else
					       (core-prim-id '<pair>))))

	       ((eq? datum (void))	(core-prim-id '<void>))
	       (else			(top-type-id))))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
