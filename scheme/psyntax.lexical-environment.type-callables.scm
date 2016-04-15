;;; -*- coding: utf-8-unix -*-
;;;
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


(module PSYNTAX-TYPE-CALLABLES
    (
     <callable-signature>
     callable-signature?
     callable-signature.retvals

     <clambda-clause-signature>
     make-clambda-clause-signature			clambda-clause-signature?
     clambda-clause-signature=?
     clambda-clause-signature.retvals			clambda-clause-signature.retvals.specs
     clambda-clause-signature.argvals			clambda-clause-signature.argvals.specs
     clambda-clause-signature.fully-untyped?		clambda-clause-signature.untyped?
     clambda-signature.min-and-max-argvals

     <clambda-signature>
     make-clambda-signature				clambda-signature?
     clambda-signature=?
     clambda-signature.retvals				clambda-signature.clause-signature*

     #| end of exports |# )

(import PSYNTAX-TYPE-SIGNATURES)


;;;; type definition: callable signature

;;This is the  base type of every object  that can be in operator position  in a form
;;like:
;;
;;   (?rator ?rand ...)
;;
;;representing a function application.
;;
(define-record-type (<callable-signature> dummy-make-callable-signature callable-signature?)
  (nongenerative vicare:expander:<callable-signature>)
  (fields
    (immutable retvals	callable-signature.retvals)
		;An instance of "<type-signature>".
		;
		;For the  "<clambda-signature>" sub-type it represents  the signature
		;of the  common retvals from  all the clambda clauses  represented by
		;this struct.  For example:
		;
		;   (case-lambda
		;    (({_ <fixnum>}) . ?body)
		;     ({_ <fixnum>}) . ?body)))
		;
		;has common retvals "(<fixnum>)", while:
		;
		;   (case-lambda
		;    (({_ <fixnum>}) . ?body)
		;     ({_ <bignum>}) . ?body)))
		;
		;has common retvals "(<exact-integer>)".  When  it is not possible to
		;determine a common retvals signature: the default value is "<list>",
		;which means any number of objects of any type.
    #| end of FIELDS |# ))


;;;; type definition: CASE-LAMBDA clause signature

(define-record-type (<clambda-clause-signature> make-clambda-clause-signature clambda-clause-signature?)
  (nongenerative vicare:expander:<clambda-clause-signature>)
  (fields
    (immutable retvals	clambda-clause-signature.retvals)
		;An instance of "<type-signature>"  representing the signature of the
		;return values.
    (immutable argvals	clambda-clause-signature.argvals)
		;An instance of "<type-signature>"  representing the signature of the
		;argument values.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-clambda-clause-signature {retvals type-signature?} {argvals type-signature?})
	(make-record retvals argvals))
      make-clambda-clause-signature))
  (custom-printer
    (lambda (S port sub-printer)
      (sub-printer `(<clambda-clause-signature>
		     (:retvals ,(type-signature.syntax-object (clambda-clause-signature.retvals S)))
		     (:argvals ,(type-signature.syntax-object (clambda-clause-signature.argvals S))))))))

(define (not-empty-list-of-clambda-clause-signatures? obj)
  (and (pair? obj)
       (and (clambda-clause-signature? (car obj))
	    (let loop ((obj (cdr obj)))
	      (if (pair? obj)
		  (and (clambda-clause-signature? (car obj))
		       (loop (cdr obj)))
		(null? obj))))))

(define* (clambda-clause-signature.argvals.specs {signature clambda-clause-signature?})
  (type-signature.object-type-specs (clambda-clause-signature.argvals signature)))

(define* (clambda-clause-signature.retvals.specs {signature clambda-clause-signature?})
  (type-signature.object-type-specs (clambda-clause-signature.retvals signature)))

;;; --------------------------------------------------------------------

(define* (clambda-clause-signature=? {signature1 clambda-clause-signature?} {signature2 clambda-clause-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  (and (type-signature=? (clambda-clause-signature.argvals signature1)
			 (clambda-clause-signature.argvals signature2))
       (type-signature=? (clambda-clause-signature.retvals signature1)
			 (clambda-clause-signature.retvals signature2))))

;;; --------------------------------------------------------------------

(define* (clambda-clause-signature.fully-untyped? {clause-signature clambda-clause-signature?})
  ;;A  clambda clause  signature  has fully  unspecified types  if  its retvals  type
  ;;signature  is the  standalone  "<list>" and  its argvals  type  signature is  the
  ;;standalone "<list>".
  ;;
  (and (type-signature.fully-untyped? (clambda-clause-signature.argvals clause-signature))
       (type-signature.fully-untyped? (clambda-clause-signature.retvals clause-signature))))

(define* (clambda-clause-signature.untyped? {clause-signature clambda-clause-signature?})
  ;;A  clambda  clause has  "untyped"  signature  if  both  its argvals  and  retvals
  ;;signatures only use "<top>" and "<list>" as type identifiers.
  ;;
  (and (type-signature.untyped? (clambda-clause-signature.argvals clause-signature))
       (type-signature.untyped? (clambda-clause-signature.retvals clause-signature))))


;;;; type definition: CLAMBDA signature

;;Type representing the full type signature of closure objects.
;;
(define-record-type (<clambda-signature> make-clambda-signature clambda-signature?)
  (nongenerative vicare:expander:<clambda-signature>)
  (parent <callable-signature>)
  (fields
    (immutable clause-signature*	clambda-signature.clause-signature*)
		;A proper list of "<clambda-clause-signature>" instances representing
		;the signatures of the CASE-LAMBDA clauses.
    (mutable memoised-min-count	clambda-signature.memoised-min-count	clambda-signature.memoised-min-count-set!)
    (mutable memoised-max-count	clambda-signature.memoised-max-count	clambda-signature.memoised-max-count-set!)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-callable-signature)
      (define (reduce func identity ell)
	(if (pair? ell)
	    (fold-left func (car ell) (cdr ell))
	  identity))
      (define* (make-clambda-signature {signature* not-empty-list-of-clambda-clause-signatures?})
	((make-callable-signature (reduce type-signature.union
					  (make-type-signature/fully-untyped)
					  (map clambda-clause-signature.retvals signature*)))
	 signature* #f #f))
      make-clambda-signature))
  (custom-printer
    (lambda (S port sub-printer)
      (sub-printer `(<clambda-signature>
		     (:common-retvals ,(callable-signature.retvals S))
		     (:clause-signatures . ,(clambda-signature.clause-signature* S)))))))

(define* (clambda-signature=? {sig1 clambda-signature?} {sig2 clambda-signature?})
  (let ((csig1* (clambda-signature.clause-signature* sig1))
	(csig2* (clambda-signature.clause-signature* sig2)))
    (and (for-all (lambda (csig1)
		    (exists (lambda (csig2)
			      (clambda-clause-signature=? csig1 csig2))
		      csig2*))
	   csig1*)
	 (for-all (lambda (csig2)
		    (exists (lambda (csig1)
			      (clambda-clause-signature=? csig1 csig2))
		      csig1*))
	   csig2*))))

(define* (clambda-signature.retvals {sig clambda-signature?})
  (callable-signature.retvals sig))

(define* (clambda-signature.min-and-max-argvals {sig clambda-signature?})
  ;;Return two non-negative real numbers  representing the minimum and maximum number
  ;;of values that can match the argvals type signatures of all the clauses.
  ;;
  (cond ((clambda-signature.memoised-min-count sig)
	 => (lambda (min-count)
	      (values min-count (clambda-signature.memoised-max-count sig))))
	(else
	 (let ((clause-signature* (clambda-signature.clause-signature* sig)))
	   (receive (min-count max-count)
	       (if (and (pair? clause-signature*)
			(null? (cdr clause-signature*)))
		   ;;There is only one clause signature.
		   (type-signature.min-and-max-counts (clambda-clause-signature.argvals (car clause-signature*)))
		 ;;There are two or more clause signatures.
		 (let ((cnts (fold-left
				 (lambda (knil clause-signature)
				   (receive (min-count max-count)
				       (type-signature.min-and-max-counts (clambda-clause-signature.argvals clause-signature))
				     (cons (min (car knil) min-count)
					   (max (cdr knil) max-count))))
			       (receive (min-count max-count)
				   (type-signature.min-and-max-counts (clambda-clause-signature.argvals (car clause-signature*)))
				 (cons min-count max-count))
			       (cdr clause-signature*))))
		   (values (car cnts) (cdr cnts))))
	     (clambda-signature.memoised-min-count-set! sig min-count)
	     (clambda-signature.memoised-max-count-set! sig max-count)
	     (values min-count max-count))))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
