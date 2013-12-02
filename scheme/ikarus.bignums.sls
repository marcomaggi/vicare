;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus bignums)
  (export
    bignum-positive?
    bignum-negative?
    (rename (bignum-positive?	bignum-non-negative?)
	    (bignum-negative?	bignum-non-positive?))
    bignum-odd?
    bignum-even?
    least-positive-bignum
    greatest-negative-bignum

    $bignum-positive?	$bignum-negative?
    (rename ($bignum-positive?	$bignum-non-negative?)
	    ($bignum-negative?	$bignum-non-positive?))
    $bignum-even?	$bignum-odd?
    $bignum->flonum)
  (import (except (ikarus)
		  bignum-positive?
		  bignum-negative?
		  bignum-non-negative?
		  bignum-non-positive?
		  bignum-odd?
		  bignum-even?
		  least-positive-bignum
		  greatest-negative-bignum)
    (except (ikarus system $bignums)
	    $bignum-positive?		$bignum-negative?
	    $bignum-non-positive?	$bignum-non-negative?
	    $bignum-even?		$bignum-odd?
	    $bignum->flonum)
    (ikarus system $flonums)
    (vicare arguments validation))


;;;; limits

(define (least-positive-bignum)
  (+ +1 (greatest-fixnum)))

(define (greatest-negative-bignum)
  (+ -1 (least-fixnum)))


;;;; predicates

(define-syntax define-bn-operation/one
  (syntax-rules ()
    ((_ ?safe-who ?unsafe-who)
     (define* (?safe-who (x bignum?))
       (?unsafe-who x)))))

(define-bn-operation/one bignum-positive?	$bignum-positive?)
(define-bn-operation/one bignum-negative?	$bignum-negative?)
(define-bn-operation/one bignum-odd?		$bignum-odd?)
(define-bn-operation/one bignum-even?		$bignum-even?)

;;; --------------------------------------------------------------------

(define ($bignum-positive? x)
  (foreign-call "ikrt_positive_bn" x))

(define ($bignum-negative? x)
  (not ($bignum-positive? x)))

(define ($bignum-even? x)
  (foreign-call "ikrt_even_bn" x))

(define ($bignum-odd? x)
  (not ($bignum-even? x)))


;;;; conversion

(define ($bignum->flonum x)
  (foreign-call "ikrt_bignum_to_flonum" x 0 ($make-flonum)))


;;;; done

)

;;; end of file
