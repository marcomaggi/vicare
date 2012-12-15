;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: max
;;;Date: Sat Dec  8, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (ikarus system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: max\n")


;;;; constants

(define SMALLEST-POSITIVE-BIGNUM	(-    (least-fixnum)))
(define SMALLEST-NEGATIVE-BIGNUM	(+ -1 (least-fixnum)))

(define BN1	(+ +1  SMALLEST-POSITIVE-BIGNUM))
(define BN2	(+ +10 SMALLEST-POSITIVE-BIGNUM))
(define BN3	(+ -1  SMALLEST-NEGATIVE-BIGNUM))
(define BN4	(+ -10 SMALLEST-NEGATIVE-BIGNUM))

(define RN1	+13/11)
(define RN2	+17/11)
(define RN3	-13/11)
(define RN4	-17/11)


(parametrise ((check-test-name	'generic))

  (check (max 1)		=> 1)
  (check (max BN1)		=> BN1)
  (check (max 1.)		=> 1.)
  (check (max 1/2)		=> 1/2)

  (check (max 1 2 3)		=> 3)
  (check (max 1 3 2)		=> 3)
  (check (max 3 1 2)		=> 3)

  (check (max 1. 2 3)		=> 3.)
  (check (max 1 3. 2)		=> 3.)
  (check (max 3 1 2.)		=> 3.)

  (check (max 3 1/2 2.)		=> 3.)
  (check (max 3 13/3 2.)	=> 4.333333333333334)

  (check
      (max +nan.0 3 13/3 2.)
    => +nan.0)

  #t)


(parametrise ((check-test-name	'fixnums))

;;; fixnums

  (check (max 1 2)			=> 2)
  (check (max 2 1)			=> 2)
  (check ($max-number-fixnum 1 2)	=> 2)
  (check ($max-number-fixnum 2 1)	=> 2)
  (check ($max-fixnum-number 1 2)	=> 2)
  (check ($max-fixnum-number 2 1)	=> 2)
  (check ($max-fixnum-fixnum 1 2)	=> 2)
  (check ($max-fixnum-fixnum 2 1)	=> 2)

;;; --------------------------------------------------------------------
;;; bignums

  (check (max 1 BN1)			=> BN1)
  (check (max BN1 1)			=> BN1)
  (check ($max-number-fixnum BN1 1)	=> BN1)
  (check ($max-fixnum-number 1 BN1)	=> BN1)
  (check ($max-fixnum-bignum 1 BN1)	=> BN1)

  (check (max 1 BN2)			=> BN2)
  (check (max BN2 1)			=> BN2)
  (check ($max-number-fixnum BN2 1)	=> BN2)
  (check ($max-fixnum-number 1 BN2)	=> BN2)
  (check ($max-fixnum-bignum 1 BN2)	=> BN2)

  (check (max 1 BN3)			=> 1)
  (check (max BN3 1)			=> 1)
  (check ($max-number-fixnum BN3 1)	=> 1)
  (check ($max-fixnum-number 1 BN3)	=> 1)
  (check ($max-fixnum-bignum 1 BN3)	=> 1)

  (check (max 1 BN4)			=> 1)
  (check (max BN4 1)			=> 1)
  (check ($max-number-fixnum BN4 1)	=> 1)
  (check ($max-fixnum-number 1 BN4)	=> 1)
  (check ($max-fixnum-bignum 1 BN4)	=> 1)

  (check (max (least-fixnum) BN1)			=> BN1)
  (check (max BN1 (least-fixnum))			=> BN1)
  (check ($max-number-fixnum BN1 (least-fixnum))	=> BN1)
  (check ($max-fixnum-number (least-fixnum) BN1)	=> BN1)
  (check ($max-fixnum-bignum (least-fixnum) BN1)	=> BN1)

  (check (max (least-fixnum) BN2)			=> BN2)
  (check (max BN2 (least-fixnum))			=> BN2)
  (check ($max-number-fixnum BN2 (least-fixnum))	=> BN2)
  (check ($max-fixnum-number (least-fixnum) BN2)	=> BN2)
  (check ($max-fixnum-bignum (least-fixnum) BN2)	=> BN2)

  (check (max (least-fixnum) BN3)			=> (least-fixnum))
  (check (max BN3 (least-fixnum))			=> (least-fixnum))
  (check ($max-number-fixnum BN3 (least-fixnum))	=> (least-fixnum))
  (check ($max-fixnum-number (least-fixnum) BN3)	=> (least-fixnum))
  (check ($max-fixnum-bignum (least-fixnum) BN3)	=> (least-fixnum))

  (check (max (least-fixnum) BN4)			=> (least-fixnum))
  (check (max BN4 (least-fixnum))			=> (least-fixnum))
  (check ($max-number-fixnum BN4 (least-fixnum))	=> (least-fixnum))
  (check ($max-fixnum-number (least-fixnum) BN4)	=> (least-fixnum))
  (check ($max-fixnum-bignum (least-fixnum) BN4)	=> (least-fixnum))

  (check (max (greatest-fixnum) BN1)			=> BN1)
  (check (max BN1 (greatest-fixnum))			=> BN1)
  (check ($max-number-fixnum BN1 (greatest-fixnum))	=> BN1)
  (check ($max-fixnum-number (greatest-fixnum) BN1)	=> BN1)
  (check ($max-fixnum-bignum (greatest-fixnum) BN1)	=> BN1)

  (check (max (greatest-fixnum) BN2)			=> BN2)
  (check (max BN2 (greatest-fixnum))			=> BN2)
  (check ($max-number-fixnum BN2 (greatest-fixnum))	=> BN2)
  (check ($max-fixnum-number (greatest-fixnum) BN2)	=> BN2)
  (check ($max-fixnum-bignum (greatest-fixnum) BN2)	=> BN2)

  (check (max (greatest-fixnum) BN3)			=> (greatest-fixnum))
  (check (max BN3 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($max-number-fixnum BN3 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($max-fixnum-number (greatest-fixnum) BN3)	=> (greatest-fixnum))
  (check ($max-fixnum-bignum (greatest-fixnum) BN3)	=> (greatest-fixnum))

  (check (max (greatest-fixnum) BN4)			=> (greatest-fixnum))
  (check (max BN4 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($max-number-fixnum BN4 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($max-fixnum-number (greatest-fixnum) BN4)	=> (greatest-fixnum))
  (check ($max-fixnum-bignum (greatest-fixnum) BN4)	=> (greatest-fixnum))

;;; --------------------------------------------------------------------
;;; ratnums

  (check (max 1 3/2)				=> 3/2)
  (check (max 3/2 1)				=> 3/2)
  (check ($max-fixnum-number 1 3/2)		=> 3/2)
  (check ($max-number-fixnum 3/2 1)		=> 3/2)
  (check ($max-fixnum-ratnum 1 3/2)		=> 3/2)
  (check ($max-ratnum-fixnum 3/2 1)		=> 3/2)

  (check (max 1 -3/2)				=> 1)
  (check (max -3/2 1)				=> 1)
  (check ($max-fixnum-number 1 -3/2)		=> 1)
  (check ($max-number-fixnum -3/2 1)		=> 1)
  (check ($max-fixnum-ratnum 1 -3/2)		=> 1)
  (check ($max-ratnum-fixnum -3/2 1)		=> 1)

;;; --------------------------------------------------------------------
;;; flonums

  (check (max 1 2.3)				=> 2.3)
  (check (max 2.3 1)				=> 2.3)
  (check ($max-fixnum-number 1 2.3)		=> 2.3)
  (check ($max-number-fixnum 2.3 1)		=> 2.3)
  (check ($max-fixnum-flonum 1 2.3)		=> 2.3)
  (check ($max-flonum-fixnum 2.3 1)		=> 2.3)

  (check (max 1 -2.3)				=> 1.0)
  (check (max -2.3 1)				=> 1.0)
  (check ($max-fixnum-number 1 -2.3)		=> 1.0)
  (check ($max-number-fixnum -2.3 1)		=> 1.0)
  (check ($max-fixnum-flonum 1 -2.3)		=> 1.0)
  (check ($max-flonum-fixnum -2.3 1)		=> 1.0)

  (check (max 1 +inf.0)				=> +inf.0)
  (check (max +inf.0 1)				=> +inf.0)
  (check ($max-fixnum-number 1 +inf.0)		=> +inf.0)
  (check ($max-number-fixnum +inf.0 1)		=> +inf.0)
  (check ($max-fixnum-flonum 1 +inf.0)		=> +inf.0)
  (check ($max-flonum-fixnum +inf.0 1)		=> +inf.0)

  (check (max 1 -inf.0)				=> 1.0)
  (check (max -inf.0 1)				=> 1.0)
  (check ($max-fixnum-number 1 -inf.0)		=> 1.0)
  (check ($max-number-fixnum -inf.0 1)		=> 1.0)
  (check ($max-fixnum-flonum 1 -inf.0)		=> 1.0)
  (check ($max-flonum-fixnum -inf.0 1)		=> 1.0)

  (check (max 1 +nan.0)				=> +nan.0)
  (check (max +nan.0 1)				=> +nan.0)
  (check ($max-fixnum-number 1 +nan.0)		=> +nan.0)
  (check ($max-number-fixnum +nan.0 1)		=> +nan.0)
  (check ($max-fixnum-flonum 1 +nan.0)		=> +nan.0)
  (check ($max-flonum-fixnum +nan.0 1)		=> +nan.0)

  #t)


(parametrise ((check-test-name	'bignums))

;;; fixnums

  (check (max 1 BN1)			=> BN1)
  (check (max BN1 1)			=> BN1)
  (check ($max-number-fixnum BN1 1)	=> BN1)
  (check ($max-fixnum-number 1 BN1)	=> BN1)
  (check ($max-fixnum-bignum 1 BN1)	=> BN1)

  (check (max 1 BN2)			=> BN2)
  (check (max BN2 1)			=> BN2)
  (check ($max-number-fixnum BN2 1)	=> BN2)
  (check ($max-fixnum-number 1 BN2)	=> BN2)
  (check ($max-fixnum-bignum 1 BN2)	=> BN2)

  (check (max 1 BN3)			=> 1)
  (check (max BN3 1)			=> 1)
  (check ($max-number-fixnum BN3 1)	=> 1)
  (check ($max-fixnum-number 1 BN3)	=> 1)
  (check ($max-fixnum-bignum 1 BN3)	=> 1)

  (check (max 1 BN4)			=> 1)
  (check (max BN4 1)			=> 1)
  (check ($max-number-fixnum BN4 1)	=> 1)
  (check ($max-fixnum-number 1 BN4)	=> 1)
  (check ($max-fixnum-bignum 1 BN4)	=> 1)

  (check (max (least-fixnum) BN1)			=> BN1)
  (check (max BN1 (least-fixnum))			=> BN1)
  (check ($max-number-fixnum BN1 (least-fixnum))	=> BN1)
  (check ($max-fixnum-number (least-fixnum) BN1)	=> BN1)
  (check ($max-fixnum-bignum (least-fixnum) BN1)	=> BN1)

  (check (max (least-fixnum) BN2)			=> BN2)
  (check (max BN2 (least-fixnum))			=> BN2)
  (check ($max-number-fixnum BN2 (least-fixnum))	=> BN2)
  (check ($max-fixnum-number (least-fixnum) BN2)	=> BN2)
  (check ($max-fixnum-bignum (least-fixnum) BN2)	=> BN2)

  (check (max (least-fixnum) BN3)			=> (least-fixnum))
  (check (max BN3 (least-fixnum))			=> (least-fixnum))
  (check ($max-number-fixnum BN3 (least-fixnum))	=> (least-fixnum))
  (check ($max-fixnum-number (least-fixnum) BN3)	=> (least-fixnum))
  (check ($max-fixnum-bignum (least-fixnum) BN3)	=> (least-fixnum))

  (check (max (least-fixnum) BN4)			=> (least-fixnum))
  (check (max BN4 (least-fixnum))			=> (least-fixnum))
  (check ($max-number-fixnum BN4 (least-fixnum))	=> (least-fixnum))
  (check ($max-fixnum-number (least-fixnum) BN4)	=> (least-fixnum))
  (check ($max-fixnum-bignum (least-fixnum) BN4)	=> (least-fixnum))

  (check (max (greatest-fixnum) BN1)			=> BN1)
  (check (max BN1 (greatest-fixnum))			=> BN1)
  (check ($max-number-fixnum BN1 (greatest-fixnum))	=> BN1)
  (check ($max-fixnum-number (greatest-fixnum) BN1)	=> BN1)
  (check ($max-fixnum-bignum (greatest-fixnum) BN1)	=> BN1)

  (check (max (greatest-fixnum) BN2)			=> BN2)
  (check (max BN2 (greatest-fixnum))			=> BN2)
  (check ($max-number-fixnum BN2 (greatest-fixnum))	=> BN2)
  (check ($max-fixnum-number (greatest-fixnum) BN2)	=> BN2)
  (check ($max-fixnum-bignum (greatest-fixnum) BN2)	=> BN2)

  (check (max (greatest-fixnum) BN3)			=> (greatest-fixnum))
  (check (max BN3 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($max-number-fixnum BN3 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($max-fixnum-number (greatest-fixnum) BN3)	=> (greatest-fixnum))
  (check ($max-fixnum-bignum (greatest-fixnum) BN3)	=> (greatest-fixnum))

  (check (max (greatest-fixnum) BN4)			=> (greatest-fixnum))
  (check (max BN4 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($max-number-fixnum BN4 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($max-fixnum-number (greatest-fixnum) BN4)	=> (greatest-fixnum))
  (check ($max-fixnum-bignum (greatest-fixnum) BN4)	=> (greatest-fixnum))

;;; --------------------------------------------------------------------
;;; bignums

  (check (max BN1 BN2)			=> BN2)
  (check (max BN2 BN1)			=> BN2)
  (check ($max-number-bignum BN1 BN2)	=> BN2)
  (check ($max-number-bignum BN2 BN1)	=> BN2)
  (check ($max-bignum-number BN1 BN2)	=> BN2)
  (check ($max-bignum-number BN2 BN1)	=> BN2)
  (check ($max-bignum-bignum BN1 BN2)	=> BN2)
  (check ($max-bignum-bignum BN2 BN1)	=> BN2)

  (check (max BN3 BN2)			=> BN2)
  (check (max BN2 BN3)			=> BN2)
  (check ($max-number-bignum BN3 BN2)	=> BN2)
  (check ($max-number-bignum BN2 BN3)	=> BN2)
  (check ($max-bignum-number BN3 BN2)	=> BN2)
  (check ($max-bignum-number BN2 BN3)	=> BN2)
  (check ($max-bignum-bignum BN3 BN2)	=> BN2)
  (check ($max-bignum-bignum BN2 BN3)	=> BN2)

  (check (max BN4 BN2)			=> BN2)
  (check (max BN2 BN4)			=> BN2)
  (check ($max-number-bignum BN4 BN2)	=> BN2)
  (check ($max-number-bignum BN2 BN4)	=> BN2)
  (check ($max-bignum-number BN4 BN2)	=> BN2)
  (check ($max-bignum-number BN2 BN4)	=> BN2)
  (check ($max-bignum-bignum BN4 BN2)	=> BN2)
  (check ($max-bignum-bignum BN2 BN4)	=> BN2)

  (check (max BN4 BN3)			=> BN3)
  (check (max BN3 BN4)			=> BN3)
  (check ($max-number-bignum BN4 BN3)	=> BN3)
  (check ($max-number-bignum BN3 BN4)	=> BN3)
  (check ($max-bignum-number BN4 BN3)	=> BN3)
  (check ($max-bignum-number BN3 BN4)	=> BN3)
  (check ($max-bignum-bignum BN4 BN3)	=> BN3)
  (check ($max-bignum-bignum BN3 BN4)	=> BN3)

;;; --------------------------------------------------------------------
;;; ratnums

  (check (max BN1 3/2)				=> BN1)
  (check (max 3/2 BN1)				=> BN1)
  (check ($max-bignum-number BN1 3/2)		=> BN1)
  (check ($max-number-bignum 3/2 BN1)		=> BN1)
  (check ($max-bignum-ratnum BN1 3/2)		=> BN1)
  (check ($max-ratnum-bignum 3/2 BN1)		=> BN1)

  (check (max BN1 -3/2)				=> BN1)
  (check (max -3/2 BN1)				=> BN1)
  (check ($max-bignum-number BN1 -3/2)		=> BN1)
  (check ($max-number-bignum -3/2 BN1)		=> BN1)
  (check ($max-bignum-ratnum BN1 -3/2)		=> BN1)
  (check ($max-ratnum-bignum -3/2 BN1)		=> BN1)

;;; --------------------------------------------------------------------
;;; flonums

  (check (max BN1 2.3)				=> (inexact BN1))
  (check (max 2.3 BN1)				=> (inexact BN1))
  (check ($max-bignum-number BN1 2.3)		=> (inexact BN1))
  (check ($max-number-bignum 2.3 BN1)		=> (inexact BN1))
  (check ($max-bignum-flonum BN1 2.3)		=> (inexact BN1))
  (check ($max-flonum-bignum 2.3 BN1)		=> (inexact BN1))

  (check (max BN1 -2.3)				=> (inexact BN1))
  (check (max -2.3 BN1)				=> (inexact BN1))
  (check ($max-bignum-number BN1 -2.3)		=> (inexact BN1))
  (check ($max-number-bignum -2.3 BN1)		=> (inexact BN1))
  (check ($max-bignum-flonum BN1 -2.3)		=> (inexact BN1))
  (check ($max-flonum-bignum -2.3 BN1)		=> (inexact BN1))

  (check (max BN1 +inf.0)			=> +inf.0)
  (check (max +inf.0 BN1)			=> +inf.0)
  (check ($max-bignum-number BN1 +inf.0)	=> +inf.0)
  (check ($max-number-bignum +inf.0 BN1)	=> +inf.0)
  (check ($max-bignum-flonum BN1 +inf.0)	=> +inf.0)
  (check ($max-flonum-bignum +inf.0 BN1)	=> +inf.0)

  (check (max BN1 -inf.0)			=> (inexact BN1))
  (check (max -inf.0 BN1)			=> (inexact BN1))
  (check ($max-bignum-number BN1 -inf.0)	=> (inexact BN1))
  (check ($max-number-bignum -inf.0 BN1)	=> (inexact BN1))
  (check ($max-bignum-flonum BN1 -inf.0)	=> (inexact BN1))
  (check ($max-flonum-bignum -inf.0 BN1)	=> (inexact BN1))

  (check (max BN1 +nan.0)			=> +nan.0)
  (check (max +nan.0 BN1)			=> +nan.0)
  (check ($max-bignum-number BN1 +nan.0)	=> +nan.0)
  (check ($max-number-bignum +nan.0 BN1)	=> +nan.0)
  (check ($max-bignum-flonum BN1 +nan.0)	=> +nan.0)
  (check ($max-flonum-bignum +nan.0 BN1)	=> +nan.0)
;;;
  (check (max BN3 2.3)				=> 2.3)
  (check (max 2.3 BN3)				=> 2.3)
  (check ($max-bignum-number BN3 2.3)		=> 2.3)
  (check ($max-number-bignum 2.3 BN3)		=> 2.3)
  (check ($max-bignum-flonum BN3 2.3)		=> 2.3)
  (check ($max-flonum-bignum 2.3 BN3)		=> 2.3)

  (check (max BN3 -2.3)				=> -2.3)
  (check (max -2.3 BN3)				=> -2.3)
  (check ($max-bignum-number BN3 -2.3)		=> -2.3)
  (check ($max-number-bignum -2.3 BN3)		=> -2.3)
  (check ($max-bignum-flonum BN3 -2.3)		=> -2.3)
  (check ($max-flonum-bignum -2.3 BN3)		=> -2.3)

  (check (max BN3 +inf.0)			=> +inf.0)
  (check (max +inf.0 BN3)			=> +inf.0)
  (check ($max-bignum-number BN3 +inf.0)	=> +inf.0)
  (check ($max-number-bignum +inf.0 BN3)	=> +inf.0)
  (check ($max-bignum-flonum BN3 +inf.0)	=> +inf.0)
  (check ($max-flonum-bignum +inf.0 BN3)	=> +inf.0)

  (check (max BN3 -inf.0)			=> (inexact BN3))
  (check (max -inf.0 BN3)			=> (inexact BN3))
  (check ($max-bignum-number BN3 -inf.0)	=> (inexact BN3))
  (check ($max-number-bignum -inf.0 BN3)	=> (inexact BN3))
  (check ($max-bignum-flonum BN3 -inf.0)	=> (inexact BN3))
  (check ($max-flonum-bignum -inf.0 BN3)	=> (inexact BN3))

  (check (max BN3 +nan.0)			=> +nan.0)
  (check (max +nan.0 BN3)			=> +nan.0)
  (check ($max-bignum-number BN3 +nan.0)	=> +nan.0)
  (check ($max-number-bignum +nan.0 BN3)	=> +nan.0)
  (check ($max-bignum-flonum BN3 +nan.0)	=> +nan.0)
  (check ($max-flonum-bignum +nan.0 BN3)	=> +nan.0)

  #t)


(parametrise ((check-test-name	'ratnums))

;;; fixnums

  (check (max 1 RN1)			=> RN1)
  (check (max RN1 1)			=> RN1)
  (check ($max-number-fixnum RN1 1)	=> RN1)
  (check ($max-fixnum-number 1 RN1)	=> RN1)
  (check ($max-fixnum-ratnum 1 RN1)	=> RN1)

  (check (max 1 RN2)			=> RN2)
  (check (max RN2 1)			=> RN2)
  (check ($max-number-fixnum RN2 1)	=> RN2)
  (check ($max-fixnum-number 1 RN2)	=> RN2)
  (check ($max-fixnum-ratnum 1 RN2)	=> RN2)

  (check (max 1 RN3)			=> 1)
  (check (max RN3 1)			=> 1)
  (check ($max-number-fixnum RN3 1)	=> 1)
  (check ($max-fixnum-number 1 RN3)	=> 1)
  (check ($max-fixnum-ratnum 1 RN3)	=> 1)

  (check (max 1 RN4)			=> 1)
  (check (max RN4 1)			=> 1)
  (check ($max-number-fixnum RN4 1)	=> 1)
  (check ($max-fixnum-number 1 RN4)	=> 1)
  (check ($max-fixnum-ratnum 1 RN4)	=> 1)

  (check (max (least-fixnum) RN1)			=> RN1)
  (check (max RN1 (least-fixnum))			=> RN1)
  (check ($max-number-fixnum RN1 (least-fixnum))	=> RN1)
  (check ($max-fixnum-number (least-fixnum) RN1)	=> RN1)
  (check ($max-fixnum-ratnum (least-fixnum) RN1)	=> RN1)

  (check (max (least-fixnum) RN2)			=> RN2)
  (check (max RN2 (least-fixnum))			=> RN2)
  (check ($max-number-fixnum RN2 (least-fixnum))	=> RN2)
  (check ($max-fixnum-number (least-fixnum) RN2)	=> RN2)
  (check ($max-fixnum-ratnum (least-fixnum) RN2)	=> RN2)

  (check (max (least-fixnum) RN3)			=> RN3)
  (check (max RN3 (least-fixnum))			=> RN3)
  (check ($max-number-fixnum RN3 (least-fixnum))	=> RN3)
  (check ($max-fixnum-number (least-fixnum) RN3)	=> RN3)
  (check ($max-fixnum-ratnum (least-fixnum) RN3)	=> RN3)

  (check (max (least-fixnum) RN4)			=> RN4)
  (check (max RN4 (least-fixnum))			=> RN4)
  (check ($max-number-fixnum RN4 (least-fixnum))	=> RN4)
  (check ($max-fixnum-number (least-fixnum) RN4)	=> RN4)
  (check ($max-fixnum-ratnum (least-fixnum) RN4)	=> RN4)

  (check (max (greatest-fixnum) RN1)			=> (greatest-fixnum))
  (check (max RN1 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($max-number-fixnum RN1 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($max-fixnum-number (greatest-fixnum) RN1)	=> (greatest-fixnum))
  (check ($max-fixnum-ratnum (greatest-fixnum) RN1)	=> (greatest-fixnum))

  (check (max (greatest-fixnum) RN2)			=> (greatest-fixnum))
  (check (max RN2 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($max-number-fixnum RN2 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($max-fixnum-number (greatest-fixnum) RN2)	=> (greatest-fixnum))
  (check ($max-fixnum-ratnum (greatest-fixnum) RN2)	=> (greatest-fixnum))

  (check (max (greatest-fixnum) RN3)			=> (greatest-fixnum))
  (check (max RN3 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($max-number-fixnum RN3 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($max-fixnum-number (greatest-fixnum) RN3)	=> (greatest-fixnum))
  (check ($max-fixnum-ratnum (greatest-fixnum) RN3)	=> (greatest-fixnum))

  (check (max (greatest-fixnum) RN4)			=> (greatest-fixnum))
  (check (max RN4 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($max-number-fixnum RN4 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($max-fixnum-number (greatest-fixnum) RN4)	=> (greatest-fixnum))
  (check ($max-fixnum-ratnum (greatest-fixnum) RN4)	=> (greatest-fixnum))

;;; --------------------------------------------------------------------
;;; ratnums

  (check (max RN1 RN2)			=> RN2)
  (check (max RN2 RN1)			=> RN2)
  (check ($max-number-ratnum RN1 RN2)	=> RN2)
  (check ($max-number-ratnum RN2 RN1)	=> RN2)
  (check ($max-ratnum-number RN1 RN2)	=> RN2)
  (check ($max-ratnum-number RN2 RN1)	=> RN2)
  (check ($max-ratnum-ratnum RN1 RN2)	=> RN2)
  (check ($max-ratnum-ratnum RN2 RN1)	=> RN2)

  (check (max RN3 RN2)			=> RN2)
  (check (max RN2 RN3)			=> RN2)
  (check ($max-number-ratnum RN3 RN2)	=> RN2)
  (check ($max-number-ratnum RN2 RN3)	=> RN2)
  (check ($max-ratnum-number RN3 RN2)	=> RN2)
  (check ($max-ratnum-number RN2 RN3)	=> RN2)
  (check ($max-ratnum-ratnum RN3 RN2)	=> RN2)
  (check ($max-ratnum-ratnum RN2 RN3)	=> RN2)

  (check (max RN4 RN2)			=> RN2)
  (check (max RN2 RN4)			=> RN2)
  (check ($max-number-ratnum RN4 RN2)	=> RN2)
  (check ($max-number-ratnum RN2 RN4)	=> RN2)
  (check ($max-ratnum-number RN4 RN2)	=> RN2)
  (check ($max-ratnum-number RN2 RN4)	=> RN2)
  (check ($max-ratnum-ratnum RN4 RN2)	=> RN2)
  (check ($max-ratnum-ratnum RN2 RN4)	=> RN2)

  (check (max RN4 RN3)			=> RN3)
  (check (max RN3 RN4)			=> RN3)
  (check ($max-number-ratnum RN4 RN3)	=> RN3)
  (check ($max-number-ratnum RN3 RN4)	=> RN3)
  (check ($max-ratnum-number RN4 RN3)	=> RN3)
  (check ($max-ratnum-number RN3 RN4)	=> RN3)
  (check ($max-ratnum-ratnum RN4 RN3)	=> RN3)
  (check ($max-ratnum-ratnum RN3 RN4)	=> RN3)

;;; --------------------------------------------------------------------
;;; ratnums

  (check (max RN1 3/2)				=> 3/2)
  (check (max 3/2 RN1)				=> 3/2)
  (check ($max-ratnum-number RN1 3/2)		=> 3/2)
  (check ($max-number-ratnum 3/2 RN1)		=> 3/2)
  (check ($max-ratnum-ratnum RN1 3/2)		=> 3/2)
  (check ($max-ratnum-ratnum 3/2 RN1)		=> 3/2)

  (check (max RN1 -3/2)				=> RN1)
  (check (max -3/2 RN1)				=> RN1)
  (check ($max-ratnum-number RN1 -3/2)		=> RN1)
  (check ($max-number-ratnum -3/2 RN1)		=> RN1)
  (check ($max-ratnum-ratnum RN1 -3/2)		=> RN1)
  (check ($max-ratnum-ratnum -3/2 RN1)		=> RN1)

;;; --------------------------------------------------------------------
;;; flonums

  (check (max RN1 2.3)				=> 2.3)
  (check (max 2.3 RN1)				=> 2.3)
  (check ($max-ratnum-number RN1 2.3)		=> 2.3)
  (check ($max-number-ratnum 2.3 RN1)		=> 2.3)
  (check ($max-ratnum-flonum RN1 2.3)		=> 2.3)
  (check ($max-flonum-ratnum 2.3 RN1)		=> 2.3)

  (check (max RN1 -2.3)				=> (inexact RN1))
  (check (max -2.3 RN1)				=> (inexact RN1))
  (check ($max-ratnum-number RN1 -2.3)		=> (inexact RN1))
  (check ($max-number-ratnum -2.3 RN1)		=> (inexact RN1))
  (check ($max-ratnum-flonum RN1 -2.3)		=> (inexact RN1))
  (check ($max-flonum-ratnum -2.3 RN1)		=> (inexact RN1))

  (check (max RN1 +inf.0)			=> +inf.0)
  (check (max +inf.0 RN1)			=> +inf.0)
  (check ($max-ratnum-number RN1 +inf.0)	=> +inf.0)
  (check ($max-number-ratnum +inf.0 RN1)	=> +inf.0)
  (check ($max-ratnum-flonum RN1 +inf.0)	=> +inf.0)
  (check ($max-flonum-ratnum +inf.0 RN1)	=> +inf.0)

  (check (max RN1 -inf.0)			=> (inexact RN1))
  (check (max -inf.0 RN1)			=> (inexact RN1))
  (check ($max-ratnum-number RN1 -inf.0)	=> (inexact RN1))
  (check ($max-number-ratnum -inf.0 RN1)	=> (inexact RN1))
  (check ($max-ratnum-flonum RN1 -inf.0)	=> (inexact RN1))
  (check ($max-flonum-ratnum -inf.0 RN1)	=> (inexact RN1))

  (check (max RN1 +nan.0)			=> +nan.0)
  (check (max +nan.0 RN1)			=> +nan.0)
  (check ($max-ratnum-number RN1 +nan.0)	=> +nan.0)
  (check ($max-number-ratnum +nan.0 RN1)	=> +nan.0)
  (check ($max-ratnum-flonum RN1 +nan.0)	=> +nan.0)
  (check ($max-flonum-ratnum +nan.0 RN1)	=> +nan.0)
;;;
  (check (max RN3 2.3)				=> 2.3)
  (check (max 2.3 RN3)				=> 2.3)
  (check ($max-ratnum-number RN3 2.3)		=> 2.3)
  (check ($max-number-ratnum 2.3 RN3)		=> 2.3)
  (check ($max-ratnum-flonum RN3 2.3)		=> 2.3)
  (check ($max-flonum-ratnum 2.3 RN3)		=> 2.3)

  (check (max RN3 -2.3)				=> (inexact RN3))
  (check (max -2.3 RN3)				=> (inexact RN3))
  (check ($max-ratnum-number RN3 -2.3)		=> (inexact RN3))
  (check ($max-number-ratnum -2.3 RN3)		=> (inexact RN3))
  (check ($max-ratnum-flonum RN3 -2.3)		=> (inexact RN3))
  (check ($max-flonum-ratnum -2.3 RN3)		=> (inexact RN3))

  (check (max RN3 +inf.0)			=> +inf.0)
  (check (max +inf.0 RN3)			=> +inf.0)
  (check ($max-ratnum-number RN3 +inf.0)	=> +inf.0)
  (check ($max-number-ratnum +inf.0 RN3)	=> +inf.0)
  (check ($max-ratnum-flonum RN3 +inf.0)	=> +inf.0)
  (check ($max-flonum-ratnum +inf.0 RN3)	=> +inf.0)

  (check (max RN3 -inf.0)			=> (inexact RN3))
  (check (max -inf.0 RN3)			=> (inexact RN3))
  (check ($max-ratnum-number RN3 -inf.0)	=> (inexact RN3))
  (check ($max-number-ratnum -inf.0 RN3)	=> (inexact RN3))
  (check ($max-ratnum-flonum RN3 -inf.0)	=> (inexact RN3))
  (check ($max-flonum-ratnum -inf.0 RN3)	=> (inexact RN3))

  (check (max RN3 +nan.0)			=> +nan.0)
  (check (max +nan.0 RN3)			=> +nan.0)
  (check ($max-ratnum-number RN3 +nan.0)	=> +nan.0)
  (check ($max-number-ratnum +nan.0 RN3)	=> +nan.0)
  (check ($max-ratnum-flonum RN3 +nan.0)	=> +nan.0)
  (check ($max-flonum-ratnum +nan.0 RN3)	=> +nan.0)

  #t)


(parametrise ((check-test-name	'flonums))

;;; flonums

  (check (max 1.0 2.0)			=> 2.0)
  (check (max 2.0 1.0)			=> 2.0)
  (check ($max-number-flonum 1.0 2.0)	=> 2.0)
  (check ($max-number-flonum 2.0 1.0)	=> 2.0)
  (check ($max-flonum-number 1.0 2.0)	=> 2.0)
  (check ($max-flonum-number 2.0 1.0)	=> 2.0)
  (check ($max-flonum-flonum 1.0 2.0)	=> 2.0)
  (check ($max-flonum-flonum 2.0 1.0)	=> 2.0)

;;; --------------------------------------------------------------------
;;; bignums

  (check (max 1.0 BN1)			=> (inexact BN1))
  (check (max BN1 1.0)			=> (inexact BN1))
  (check ($max-number-flonum BN1 1.0)	=> (inexact BN1))
  (check ($max-flonum-number 1.0 BN1)	=> (inexact BN1))
  (check ($max-flonum-bignum 1.0 BN1)	=> (inexact BN1))

  (check (max 1.0 BN2)			=> (inexact BN2))
  (check (max BN2 1.0)			=> (inexact BN2))
  (check ($max-number-flonum BN2 1.0)	=> (inexact BN2))
  (check ($max-flonum-number 1.0 BN2)	=> (inexact BN2))
  (check ($max-flonum-bignum 1.0 BN2)	=> (inexact BN2))

  (check (max 1.0 BN3)			=> 1.0)
  (check (max BN3 1.0)			=> 1.0)
  (check ($max-number-flonum BN3 1.0)	=> 1.0)
  (check ($max-flonum-number 1.0 BN3)	=> 1.0)
  (check ($max-flonum-bignum 1.0 BN3)	=> 1.0)

  (check (max 1.0 BN4)			=> 1.0)
  (check (max BN4 1.0)			=> 1.0)
  (check ($max-number-flonum BN4 1.0)	=> 1.0)
  (check ($max-flonum-number 1.0 BN4)	=> 1.0)
  (check ($max-flonum-bignum 1.0 BN4)	=> 1.0)

;;; --------------------------------------------------------------------
;;; ratnums

  (check (max 1.0 3/2)			=> (inexact 3/2))
  (check (max 3/2 1.0)			=> (inexact 3/2))
  (check ($max-flonum-number 1.0 3/2)	=> (inexact 3/2))
  (check ($max-number-flonum 3/2 1.0)	=> (inexact 3/2))
  (check ($max-flonum-ratnum 1.0 3/2)	=> (inexact 3/2))
  (check ($max-ratnum-flonum 3/2 1.0)	=> (inexact 3/2))

  (check (max 1.0 -3/2)			=> 1.0)
  (check (max -3/2 1.0)			=> 1.0)
  (check ($max-flonum-number 1.0 -3/2)	=> 1.0)
  (check ($max-number-flonum -3/2 1.0)	=> 1.0)
  (check ($max-flonum-ratnum 1.0 -3/2)	=> 1.0)
  (check ($max-ratnum-flonum -3/2 1.0)	=> 1.0)

;;; --------------------------------------------------------------------
;;; flonums

  (check (max 1.0 2.0)			=> 2.0)
  (check (max 2.0 1.0)			=> 2.0)
  (check ($max-flonum-number 1.0 2.0)	=> 2.0)
  (check ($max-number-flonum 2.0 1.0)	=> 2.0)
  (check ($max-flonum-flonum 1.0 2.0)	=> 2.0)
  (check ($max-flonum-flonum 2.0 1.0)	=> 2.0)

  (check (max 1.0 -2.3)			=> 1.0)
  (check (max -2.3 1.0)			=> 1.0)
  (check ($max-flonum-number 1.0 -2.3)	=> 1.0)
  (check ($max-number-flonum -2.3 1.0)	=> 1.0)
  (check ($max-flonum-flonum 1.0 -2.3)	=> 1.0)
  (check ($max-flonum-flonum -2.3 1.0)	=> 1.0)

  (check (max 1.0 +inf.0)			=> +inf.0)
  (check (max +inf.0 1.0)			=> +inf.0)
  (check ($max-flonum-number 1.0 +inf.0)	=> +inf.0)
  (check ($max-number-flonum +inf.0 1.0)	=> +inf.0)
  (check ($max-flonum-flonum 1.0 +inf.0)	=> +inf.0)
  (check ($max-flonum-flonum +inf.0 1.0)	=> +inf.0)

  (check (max 1.0 -inf.0)			=> 1.0)
  (check (max -inf.0 1.0)			=> 1.0)
  (check ($max-flonum-number 1.0 -inf.0)	=> 1.0)
  (check ($max-number-flonum -inf.0 1.0)	=> 1.0)
  (check ($max-flonum-flonum 1.0 -inf.0)	=> 1.0)
  (check ($max-flonum-flonum -inf.0 1.0)	=> 1.0)

  (check (max 1.0 +nan.0)			=> +nan.0)
  (check (max +nan.0 1.0)			=> +nan.0)
  (check ($max-flonum-number 1.0 +nan.0)	=> +nan.0)
  (check ($max-number-flonum +nan.0 1.0)	=> +nan.0)
  (check ($max-flonum-flonum 1.0 +nan.0)	=> +nan.0)
  (check ($max-flonum-flonum +nan.0 1.0)	=> +nan.0)

  #t)


;;;; done

(check-report)

;;; end of file
