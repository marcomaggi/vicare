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


(module PSYNTAX-LAMBDA-SIGNATURES
  (<lambda-signature>
   <lambda-signature>-rtd			<lambda-signature>-rcd
   make-lambda-signature			lambda-signature?
   lambda-signature=?
   lambda-signature.retvals			lambda-signature.retvals.specs
   lambda-signature.argvals			lambda-signature.argvals.specs
   lambda-signature.fully-unspecified?		lambda-signature.only-<untyped>-and-<list>?
   lambda-signature.untyped-to-top
   lambda-signature.super-and-sub?
   lambda-signature.match-formals-against-operands

   <case-lambda-signature>
   <case-lambda-signature>-rtd			<case-lambda-signature>-rcd
   make-case-lambda-signature			case-lambda-signature?
   case-lambda-signature=?
   case-lambda-signature.retvals		case-lambda-signature.clause-signature*
   case-lambda-signature.min-and-max-argvals
   case-lambda-signature.super-and-sub?		case-lambda-signature.match-formals-against-operands

   #| end of exports |# )

(import PSYNTAX-TYPE-SIGNATURES)


;;;; type definition: applicable object clause signature

(define-core-record-type <lambda-signature>
  (nongenerative vicare:expander:<lambda-signature>)
  (define-type-descriptors)
  (strip-angular-parentheses)
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
  #| end of DEFINE-CORE-RECORD-TYPE |# )

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

(define* (lambda-signature.fully-unspecified? {lambda-signature lambda-signature?})
  ;;A clause signature  has fully unspecified types if its  retvals type signature is
  ;;the  standalone  "<list>"  and  its  argvals type  signature  is  the  standalone
  ;;"<list>".
  ;;
  (and (type-signature.fully-unspecified? (lambda-signature.argvals lambda-signature))
       (type-signature.fully-unspecified? (lambda-signature.retvals lambda-signature))))

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

;;; --------------------------------------------------------------------

(define* (lambda-signature=? {signature1 lambda-signature?} {signature2 lambda-signature?})
  ;;Return true if the signatures are equal; otherwise return false.
  ;;
  (and (type-signature=? (lambda-signature.argvals signature1)
			 (lambda-signature.argvals signature2))
       (type-signature=? (lambda-signature.retvals signature1)
			 (lambda-signature.retvals signature2))))

(define* (lambda-signature.super-and-sub? {super.ots lambda-signature?} {sub.ots lambda-signature?})
  ;;Compare two closure's clauses type signatures to determine if they are super-type
  ;;and sub-type.  Return a boolean, true if they are super and sub.
  ;;
  ;;This happens when a  formal argument (super.ots) must be a closure  and the operand (S2)
  ;;is a closure.  Example:
  ;;
  ;;   (define (fun {S2 (lambda (<fixnum>) => (<number>))})
  ;;     ---)
  ;;
  ;;   (define ({S2 <fixnum>} {A <number>})
  ;;     ---)
  ;;
  ;;   (type-of S2)	=> (lambda (<number>) => (<fixnum>))
  ;;   (fun S2)
  ;;
  ;;We want:
  ;;
  ;;* The formal's argvals to be sub-types of the operands's argvals.
  ;;
  ;;* The formal's retvals to be super-types of the operand's retvals.
  ;;
  ;;Like this:
  ;;
  ;;	   (super-and-sub? (lambda (<fixnum>) => (<number>))
  ;;	                   (lambda (<number>) => (<fixnum>)))	=> #t
  ;;
  ;;	   (super-and-sub? (lambda (<number>) => (<string>))
  ;;	                   (lambda (<fixnum>) => (<string>)))	=> #f
  ;;
  ;;	   (super-and-sub? (lambda (<string>) => (<fixnum>))
  ;;	                   (lambda (<string>) => (<number>)))	=> #f
  ;;
  (and (type-signature.matching-super-and-sub? (lambda-signature.retvals super.ots) (lambda-signature.retvals   sub.ots))
       (type-signature.matching-super-and-sub? (lambda-signature.argvals   sub.ots) (lambda-signature.argvals super.ots))))

(define* (lambda-signature.match-formals-against-operands {formals.ots lambda-signature?} {operands.ots type-signature?})
  ;;Compare  formals' and  operands' type  signatures to  determine if  the closure's
  ;;clause represented by  "<lambda-signature>" matches the operands.   Return one of
  ;;the symbols: exact-match, possible-match, no-match.
  ;;
  ;;In a function application, we want the function's arguments to be super-types and
  ;;the operands to be sub-types.  We do not care about the return values' types.
  ;;
  ;;   (match-formals-against-operands?
  ;;      (lambda (<number> <struct>) => (<string>))
  ;;      (<fixnum> <record>))				=> exact-match
  ;;
  ;;   (match-formals-against-operands?
  ;;      (lambda (<fixnum> <struct>) => (<string>))
  ;;      (<number> <record>))				=> possible-match
  ;;
  ;;   (match-formals-against-operands?
  ;;      (lambda (<fixnum> <struct>) => (<string>))
  ;;      (<string> <record>))				=> no-match
  ;;
  (type-signature.match-formals-against-operands (lambda-signature.argvals formals.ots) operands.ots))


;;;; type definition: applicable object signatures

(define-core-record-type <case-lambda-signature>
  (nongenerative vicare:expander:<case-lambda-signature>)
  (define-type-descriptors)
  (strip-angular-parentheses)
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
  #| end of DEFINE-CORE-RECORD-TYPE |# )

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
				     (make-irritants-condition sig)
				     cnd)))
		 (map lambda-signature.retvals (case-lambda-signature.clause-signature* sig)))
	(case-lambda-signature.memoised-retvals-set! sig retvals.sig))))

;;; --------------------------------------------------------------------

(define* (case-lambda-signature=? {D1 case-lambda-signature?} {D2 case-lambda-signature?})
  (let super-loop ((clause1*.ots (case-lambda-signature.clause-signature* D1))
		   (clause2*.ots (case-lambda-signature.clause-signature* D2)))
    (if (pair? clause1*.ots)
	(let sub-loop ((clause2*.ots	clause2*.ots)
		       (leftover2*.ots	'()))
	  (if (pair? clause2*.ots)
	      (if (lambda-signature=? (car clause1*.ots) (car clause2*.ots))
		  ;;We discard this CLAUSE2.  Go to the outer loop with the leftovers
		  ;;as CLAUSE2*.
		  (super-loop (cdr clause1*.ots) (append (cdr clause2*.ots) leftover2*.ots))
		;;We add this CLAUSE2 to the leftovers.
		(sub-loop (cdr clause2*.ots) (cons (car clause2*.ots) leftover2*.ots)))
	    ;;There are more CLAUSE2, but no more CLAUSE1.
	    #f))
      ;;There are no more CLAUSE1: are there more CLAUSE2?.
      (null? clause2*.ots))))

(define* (case-lambda-signature.super-and-sub? {super.ots case-lambda-signature?} {sub.ots case-lambda-signature?})
  ;;Compare two  closure's type signatures  to determine  if they are  super-type and
  ;;sub-type.  Return a boolean, true if they are super and sub.  For every clause in
  ;;the super there must be a matching clause in the sub.
  ;;
  (let ((super-clause-signature*	(case-lambda-signature.clause-signature* super.ots))
	(sub-clause-signature*		(case-lambda-signature.clause-signature*   sub.ots)))
    (for-all (lambda (super-clause-signature)
	       (exists (lambda (sub-clause-signature)
			 (lambda-signature.super-and-sub? super-clause-signature sub-clause-signature))
		 sub-clause-signature*))
      super-clause-signature*)))

;;; --------------------------------------------------------------------

(define* (case-lambda-signature.match-formals-against-operands {formals.ots case-lambda-signature?} {operands.ots type-signature?})
  ;;Compare formals' and operands' type signatures to determine if a closure's clause
  ;;exists  that matches  the  operands.   Return one  of  the symbols:  exact-match,
  ;;possible-match, no-match.
  ;;
  (returnable
    (fold-left (lambda (state formals.clause-signature)
		 (case (lambda-signature.match-formals-against-operands formals.clause-signature operands.ots)
		   ((exact-match)
		    (return 'exact-match))
		   ((possible-match)
		    'possible-match)
		   (else
		    state)))
      'no-match (case-lambda-signature.clause-signature* formals.ots))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
