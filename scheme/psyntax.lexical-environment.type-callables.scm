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
  (<lambda-signature>
   make-lambda-signature			lambda-signature?
   lambda-signature=?
   lambda-signature.retvals			lambda-signature.retvals.specs
   lambda-signature.argvals			lambda-signature.argvals.specs
   lambda-signature.fully-untyped?		lambda-signature.only-<untyped>-and-<list>?
   lambda-signature.untyped-to-top

   <case-lambda-signature>
   make-case-lambda-signature			case-lambda-signature?
   case-lambda-signature=?
   case-lambda-signature.retvals		case-lambda-signature.clause-signature*
   case-lambda-signature.min-and-max-argvals

   #| end of exports |# )

(import PSYNTAX-TYPE-SIGNATURES)


;;;; type definition: applicable object clause signature

(define-record-type (<lambda-signature> make-lambda-signature lambda-signature?)
  (nongenerative vicare:expander:<lambda-signature>)
  (fields
    (immutable retvals	lambda-signature.retvals)
		;An instance of "<type-signature>"  representing the signature of the
		;return values.
    (immutable argvals	lambda-signature.argvals)
		;An instance of "<type-signature>"  representing the signature of the
		;argument values.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-lambda-signature {retvals type-signature?} {argvals type-signature?})
	(make-record retvals argvals))
      make-lambda-signature))
  (custom-printer
    (lambda (S port sub-printer)
      (display "#[lambda-signature retvals=" port)
      (display (lambda-signature.retvals S) port)
      (display " argvals=" port)
      (display (lambda-signature.argvals S) port)
      (display "]" port)))
  #| end of DEFINE-RECORD-TYPE |# )

(define (not-empty-list-of-lambda-signatures? obj)
  (and (pair? obj)
       (and (lambda-signature? (car obj))
	    (let loop ((obj (cdr obj)))
	      (if (pair? obj)
		  (and (lambda-signature? (car obj))
		       (loop (cdr obj)))
		(null? obj))))))

(define* (lambda-signature.argvals.specs {signature lambda-signature?})
  (type-signature.object-type-specs (lambda-signature.argvals signature)))

(define* (lambda-signature.retvals.specs {signature lambda-signature?})
  (type-signature.object-type-specs (lambda-signature.retvals signature)))

;;; --------------------------------------------------------------------

(define* (lambda-signature=? {signature1 lambda-signature?} {signature2 lambda-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  (and (type-signature=? (lambda-signature.argvals signature1)
			 (lambda-signature.argvals signature2))
       (type-signature=? (lambda-signature.retvals signature1)
			 (lambda-signature.retvals signature2))))

;;; --------------------------------------------------------------------

(define* (lambda-signature.fully-untyped? {lambda-signature lambda-signature?})
  ;;A clause signature  has fully unspecified types if its  retvals type signature is
  ;;the  standalone  "<list>"  and  its  argvals type  signature  is  the  standalone
  ;;"<list>".
  ;;
  (and (type-signature.fully-untyped? (lambda-signature.argvals lambda-signature))
       (type-signature.fully-untyped? (lambda-signature.retvals lambda-signature))))

(define* (lambda-signature.only-<untyped>-and-<list>? {lambda-signature lambda-signature?})
  ;;A  clause signature  has  "untyped" signature  if both  its  argvals and  retvals
  ;;signatures only use "<untyped>" and "<list>" as type identifiers.
  ;;
  (and (type-signature.only-<untyped>-and-<list>? (lambda-signature.argvals lambda-signature))
       (type-signature.only-<untyped>-and-<list>? (lambda-signature.retvals lambda-signature))))

;;; --------------------------------------------------------------------

(define* (lambda-signature.untyped-to-top {lambda-signature lambda-signature?})
  (make-lambda-signature (type-signature.untyped-to-top (lambda-signature.retvals lambda-signature))
				 (type-signature.untyped-to-top (lambda-signature.argvals lambda-signature))))

(define* (lambda-signature.untyped-to-top! {lambda-signature lambda-signature?})
  (type-signature.untyped-to-top! (lambda-signature.retvals lambda-signature))
  (type-signature.untyped-to-top! (lambda-signature.argvals lambda-signature)))


;;;; type definition: applicable object signatures

(define-record-type (<case-lambda-signature> make-case-lambda-signature case-lambda-signature?)
  (nongenerative vicare:expander:<case-lambda-signature>)
  (fields
    (immutable clause-signature*	case-lambda-signature.clause-signature*)
		;A  proper list  of "<lambda-signature>"  instances representing  the
		;signatures of the clauses of an applicable object.
    (mutable memoised-retvals	case-lambda-signature.memoised-retvals		case-lambda-signature.memoised-retvals-set!)
    (mutable memoised-min-count	case-lambda-signature.memoised-min-count	case-lambda-signature.memoised-min-count-set!)
    (mutable memoised-max-count	case-lambda-signature.memoised-max-count	case-lambda-signature.memoised-max-count-set!)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (define* (make-case-lambda-signature {clause*.sig not-empty-list-of-lambda-signatures?})
	(make-record clause*.sig #f #f #f))
      make-case-lambda-signature))
  (custom-printer
    (lambda (S port sub-printer)
      (display "#[case-lambda-signature " port)
      (for-each (lambda (clause-signature)
		  (display clause-signature port)
		  (display #\space port))
	(case-lambda-signature.clause-signature* S))
      (display "]" port)))
  #| end of DEFINE-RECORD-TYPE |# )

(define* (case-lambda-signature=? {sig1 case-lambda-signature?} {sig2 case-lambda-signature?})
  (let ((csig1* (case-lambda-signature.clause-signature* sig1))
	(csig2* (case-lambda-signature.clause-signature* sig2)))
    (and (for-all (lambda (csig1)
		    (exists (lambda (csig2)
			      (lambda-signature=? csig1 csig2))
		      csig2*))
	   csig1*)
	 (for-all (lambda (csig2)
		    (exists (lambda (csig1)
			      (lambda-signature=? csig1 csig2))
		      csig1*))
	   csig2*))))

;;; --------------------------------------------------------------------

(define* (case-lambda-signature.min-and-max-argvals {sig case-lambda-signature?})
  ;;Return two non-negative real numbers  representing the minimum and maximum number
  ;;of values that can match the argvals type signatures of all the clauses.
  ;;
  (cond ((case-lambda-signature.memoised-min-count sig)
	 => (lambda (min-count)
	      (values min-count (case-lambda-signature.memoised-max-count sig))))
	(else
	 (let ((clause-signature* (case-lambda-signature.clause-signature* sig)))
	   (receive (min-count max-count)
	       (if (and (pair? clause-signature*)
			(null? (cdr clause-signature*)))
		   ;;There is only one clause signature.
		   (type-signature.min-and-max-counts (lambda-signature.argvals (car clause-signature*)))
		 ;;There are two or more clause signatures.
		 (let ((cnts (fold-left
				 (lambda (knil lambda-signature)
				   (receive (min-count max-count)
				       (type-signature.min-and-max-counts (lambda-signature.argvals lambda-signature))
				     (cons (min (car knil) min-count)
					   (max (cdr knil) max-count))))
			       (receive (min-count max-count)
				   (type-signature.min-and-max-counts (lambda-signature.argvals (car clause-signature*)))
				 (cons min-count max-count))
			       (cdr clause-signature*))))
		   (values (car cnts) (cdr cnts))))
	     (case-lambda-signature.memoised-min-count-set! sig min-count)
	     (case-lambda-signature.memoised-max-count-set! sig max-count)
	     (values min-count max-count))))))

;;; --------------------------------------------------------------------

(define* (case-lambda-signature.retvals {sig case-lambda-signature?})
  ;;Return  an instance  of  "<type-signature>" representing  the  union between  the
  ;;retvals signatures from all the clauses.  For example:
  ;;
  ;;   (case-lambda
  ;;    (({_ <fixnum>}) . ?body)
  ;;     ({_ <fixnum>}) . ?body)))
  ;;
  ;;has retvals signature "(<fixnum>)", while:
  ;;
  ;;   (case-lambda
  ;;    (({_ <fixnum>}) . ?body)
  ;;     ({_ <bignum>}) . ?body)))
  ;;
  ;;has retvals signature "(<exact-integer>)".  When  it is not possible to determine
  ;;a common retvals signature: the default value is "<list>", which means any number
  ;;of values of any type.
  ;;
  (or (case-lambda-signature.memoised-retvals sig)
      (receive-and-return (retvals.sig)
	  (apply type-signature.union-same-number-of-operands
		 (lambda (message cnd)
		   (raise (condition (make-who-condition 'case-lambda-signature.retvals)
				     (make-message-condition message)
				     cnd)))
		 (map lambda-signature.retvals (case-lambda-signature.clause-signature* sig)))
	(case-lambda-signature.memoised-retvals-set! sig retvals.sig))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
