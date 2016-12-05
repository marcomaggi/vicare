;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus ratnums)
  (export
    $ratnum->flonum
    ratnum-positive?			$ratnum-positive?
    ratnum-negative?			$ratnum-negative?
    ratnum-non-positive?		$ratnum-non-positive?
    ratnum-non-negative?		$ratnum-non-negative?
    positive-ratnum?
    negative-ratnum?
    non-positive-ratnum?
    non-negative-ratnum?)
  (import (except (vicare)
		  ratnum-positive?
		  ratnum-negative?
		  ratnum-non-positive?
		  ratnum-non-negative?
		  positive-ratnum?
		  negative-ratnum?
		  non-positive-ratnum?
		  non-negative-ratnum?)
    (except (vicare system $ratnums)
	    $ratnum->flonum
	    $ratnum-positive?
	    $ratnum-negative?
	    $ratnum-non-positive?
	    $ratnum-non-negative?)
    (only (vicare system $flonums)
	  $fl+
	  $fl-
	  $fl/
	  $fixnum->flonum)
    (only (vicare system $bignums)
	  $bignum->flonum)
    (only (vicare system $numerics)
	  $neg-fixnum
	  $neg-bignum
	  $quotient+remainder-fixnum-fixnum
	  $quotient+remainder-fixnum-bignum
	  $quotient+remainder-bignum-fixnum
	  $quotient+remainder-bignum-bignum)
    #| end of IMPORT |# )


(module ($ratnum->flonum)
  ;;This implementation could be better.  In some case involving bignums the rounding
  ;;error in  the least significant digit  of the resulting big  flonum causes flonum
  ;;functions to yield "wrong" results.

  (define ($ratnum->flonum rn)
    ;;We assume that RN is a ratnum object.
    ;;
    (let ((N ($ratnum-num rn))
	  (D ($ratnum-den rn)))
      ;;Here we  know that:  N and  D are either  fixnums or  bignums; D  is strictly
      ;;positive; N is non-zero.
      (if (positive? N)
	  (pos N D)
	($fl- (pos (int-neg N) D)))))

  (define-syntax-rule (int-inexact ?N)
    (if (fixnum? ?N)
	($fixnum->flonum ?N)
      ($bignum->flonum ?N)))

  (define-syntax-rule (int-neg ?N)
    (if (fixnum? ?N)
	($neg-fixnum ?N)
      ($neg-bignum ?N)))

  ;;The flonum representation  in double-precision format (IEEE 754  binary64) has 53
  ;;bits of significand.
  (define-constant DOUBLE-PRECISION-NBITS 53)

  (define (long-div1 N D)
    ;;About the quotient Q and the remainder R, we remember that:
    ;;
    ;;  N = D * Q + R     N/D = N'/D + R/D     N/D = Q + R/D
    ;;
    ;;where N'/D = Q is an exact integer number.
    ;;
    (receive (Q R)
	(if (fixnum? N)
	    (if (fixnum? D)
		($quotient+remainder-fixnum-fixnum N D)
	      ($quotient+remainder-fixnum-bignum N D))
	  (if (fixnum? D)
	      ($quotient+remainder-bignum-fixnum N D)
	    ($quotient+remainder-bignum-bignum N D)))
      ;;If N < D then Q is zero and the result is just FRAC.
      (let ((frac ($fl/ (int-inexact R)
			(int-inexact D))))
	(if (zero-fixnum? Q)
	    frac
	  ($fl+ (int-inexact Q) frac)))))

  (define (long-div2 N D bits)
    (let loop ((bits	bits)
	       (ac	(long-div1 N D)))
      (if (fxzero? bits)
	  ac
	(loop (fxsub1 bits) ($fl/ ac 2.0)))))

  (define (pos N D)
    (let ((nbits (bitwise-length N))
	  (dbits (bitwise-length D)))
      (let ((diff-bits (- nbits dbits)))
	(if (fx>=? diff-bits DOUBLE-PRECISION-NBITS)
	    (long-div1 N D)
	  (let ((extra-bits (fx- DOUBLE-PRECISION-NBITS diff-bits)))
	    (long-div2 (bitwise-arithmetic-shift-left N extra-bits) D extra-bits))))))

  #| end of module |# )


;;;; predicates

(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?unsafe-who)
			  (define (?who obj)
			    (and (ratnum? obj)
				 (?unsafe-who obj))))
			 )))
  (define-predicate positive-ratnum?		$ratnum-positive?)
  (define-predicate negative-ratnum?		$ratnum-negative?)
  (define-predicate non-positive-ratnum?	$ratnum-non-positive?)
  (define-predicate non-negative-ratnum?	$ratnum-non-negative?)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?unsafe-who)
			  (define* (?who {obj ratnum?})
			    (?unsafe-who obj)))
			 )))
  (define-predicate ratnum-positive?		$ratnum-positive?)
  (define-predicate ratnum-negative?		$ratnum-negative?)
  (define-predicate ratnum-non-positive?	$ratnum-non-positive?)
  (define-predicate ratnum-non-negative?	$ratnum-non-negative?)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(define ($ratnum-positive? Q)
  ;;The denominator of a properly constructed ratnum is always positive;
  ;;the sign of a ratnum is the sign of the numerator.
  ;;
  (positive? ($ratnum-num Q)))

(define ($ratnum-negative? Q)
  ;;The denominator of a properly constructed ratnum is always positive;
  ;;the sign of a ratnum is the sign of the numerator.
  ;;
  (negative? ($ratnum-num Q)))

;;; --------------------------------------------------------------------

(define ($ratnum-non-positive? x)
  ;;The numerator  of a ratnum  is always non-zero.  The  denominator of a  ratnum is
  ;;always strictly positive.
  ;;
  (negative? ($ratnum-num x)))

(define ($ratnum-non-negative? x)
  ;;The numerator  of a ratnum  is always non-zero.  The  denominator of a  ratnum is
  ;;always strictly positive.
  ;;
  (positive? ($ratnum-num x)))


;;;; done

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.ratnums"))

#| end of library |# )

;;; end of file
