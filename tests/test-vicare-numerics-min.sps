;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: min
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
(check-display "*** testing Vicare numerics functions: min\n")


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

  (check (min 1)		=> 1)
  (check (min BN1)		=> BN1)
  (check (min 1.)		=> 1.)
  (check (min 1/2)		=> 1/2)

  (check (min 1 2 3)		=> 1)
  (check (min 1 3 2)		=> 1)
  (check (min 3 1 2)		=> 1)

  (check (min 1. 2 3)		=> 1.)
  (check (min 1 3. 2)		=> 1.)
  (check (min 3 1 2.)		=> 1.)

  (check (min 3 1/2 2.)		=> 0.5)
  (check (min 3 13/3 2.)	=> 2.0)

  #t)


(parametrise ((check-test-name	'fixnums))

;;; fixnums

  (check (min 1 2)			=> 1)
  (check (min 2 1)			=> 1)
  (check ($min-number-fixnum 1 2)	=> 1)
  (check ($min-number-fixnum 2 1)	=> 1)
  (check ($min-fixnum-number 1 2)	=> 1)
  (check ($min-fixnum-number 2 1)	=> 1)
  (check ($min-fixnum-fixnum 1 2)	=> 1)
  (check ($min-fixnum-fixnum 2 1)	=> 1)

;;; --------------------------------------------------------------------
;;; bignums

  (check (min 1 BN1)			=> 1)
  (check (min BN1 1)			=> 1)
  (check ($min-number-fixnum BN1 1)	=> 1)
  (check ($min-fixnum-number 1 BN1)	=> 1)
  (check ($min-fixnum-bignum 1 BN1)	=> 1)

  (check (min 1 BN2)			=> 1)
  (check (min BN2 1)			=> 1)
  (check ($min-number-fixnum BN2 1)	=> 1)
  (check ($min-fixnum-number 1 BN2)	=> 1)
  (check ($min-fixnum-bignum 1 BN2)	=> 1)

  (check (min 1 BN3)			=> BN3)
  (check (min BN3 1)			=> BN3)
  (check ($min-number-fixnum BN3 1)	=> BN3)
  (check ($min-fixnum-number 1 BN3)	=> BN3)
  (check ($min-fixnum-bignum 1 BN3)	=> BN3)

  (check (min 1 BN4)			=> BN4)
  (check (min BN4 1)			=> BN4)
  (check ($min-number-fixnum BN4 1)	=> BN4)
  (check ($min-fixnum-number 1 BN4)	=> BN4)
  (check ($min-fixnum-bignum 1 BN4)	=> BN4)

  (check (min (least-fixnum) BN1)			=> (least-fixnum))
  (check (min BN1 (least-fixnum))			=> (least-fixnum))
  (check ($min-number-fixnum BN1 (least-fixnum))	=> (least-fixnum))
  (check ($min-fixnum-number (least-fixnum) BN1)	=> (least-fixnum))
  (check ($min-fixnum-bignum (least-fixnum) BN1)	=> (least-fixnum))

  (check (min (least-fixnum) BN2)			=> (least-fixnum))
  (check (min BN2 (least-fixnum))			=> (least-fixnum))
  (check ($min-number-fixnum BN2 (least-fixnum))	=> (least-fixnum))
  (check ($min-fixnum-number (least-fixnum) BN2)	=> (least-fixnum))
  (check ($min-fixnum-bignum (least-fixnum) BN2)	=> (least-fixnum))

  (check (min (least-fixnum) BN3)			=> BN3)
  (check (min BN3 (least-fixnum))			=> BN3)
  (check ($min-number-fixnum BN3 (least-fixnum))	=> BN3)
  (check ($min-fixnum-number (least-fixnum) BN3)	=> BN3)
  (check ($min-fixnum-bignum (least-fixnum) BN3)	=> BN3)

  (check (min (least-fixnum) BN4)			=> BN4)
  (check (min BN4 (least-fixnum))			=> BN4)
  (check ($min-number-fixnum BN4 (least-fixnum))	=> BN4)
  (check ($min-fixnum-number (least-fixnum) BN4)	=> BN4)
  (check ($min-fixnum-bignum (least-fixnum) BN4)	=> BN4)

  (check (min (greatest-fixnum) BN1)			=> (greatest-fixnum))
  (check (min BN1 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($min-number-fixnum BN1 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($min-fixnum-number (greatest-fixnum) BN1)	=> (greatest-fixnum))
  (check ($min-fixnum-bignum (greatest-fixnum) BN1)	=> (greatest-fixnum))

  (check (min (greatest-fixnum) BN2)			=> (greatest-fixnum))
  (check (min BN2 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($min-number-fixnum BN2 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($min-fixnum-number (greatest-fixnum) BN2)	=> (greatest-fixnum))
  (check ($min-fixnum-bignum (greatest-fixnum) BN2)	=> (greatest-fixnum))

  (check (min (greatest-fixnum) BN3)			=> BN3)
  (check (min BN3 (greatest-fixnum))			=> BN3)
  (check ($min-number-fixnum BN3 (greatest-fixnum))	=> BN3)
  (check ($min-fixnum-number (greatest-fixnum) BN3)	=> BN3)
  (check ($min-fixnum-bignum (greatest-fixnum) BN3)	=> BN3)

  (check (min (greatest-fixnum) BN4)			=> BN4)
  (check (min BN4 (greatest-fixnum))			=> BN4)
  (check ($min-number-fixnum BN4 (greatest-fixnum))	=> BN4)
  (check ($min-fixnum-number (greatest-fixnum) BN4)	=> BN4)
  (check ($min-fixnum-bignum (greatest-fixnum) BN4)	=> BN4)

;;; --------------------------------------------------------------------
;;; ratnums

  (check (min 1 3/2)				=> 1)
  (check (min 3/2 1)				=> 1)
  (check ($min-fixnum-number 1 3/2)		=> 1)
  (check ($min-number-fixnum 3/2 1)		=> 1)
  (check ($min-fixnum-ratnum 1 3/2)		=> 1)
  (check ($min-ratnum-fixnum 3/2 1)		=> 1)

  (check (min 1 -3/2)				=> -3/2)
  (check (min -3/2 1)				=> -3/2)
  (check ($min-fixnum-number 1 -3/2)		=> -3/2)
  (check ($min-number-fixnum -3/2 1)		=> -3/2)
  (check ($min-fixnum-ratnum 1 -3/2)		=> -3/2)
  (check ($min-ratnum-fixnum -3/2 1)		=> -3/2)

;;; --------------------------------------------------------------------
;;; flonums

  (check (min 1 2.3)				=> 1.)
  (check (min 2.3 1)				=> 1.)
  (check ($min-fixnum-number 1 2.3)		=> 1.)
  (check ($min-number-fixnum 2.3 1)		=> 1.)
  (check ($min-fixnum-flonum 1 2.3)		=> 1.)
  (check ($min-flonum-fixnum 2.3 1)		=> 1.)

  (check (min 1 -2.3)				=> -2.3)
  (check (min -2.3 1)				=> -2.3)
  (check ($min-fixnum-number 1 -2.3)		=> -2.3)
  (check ($min-number-fixnum -2.3 1)		=> -2.3)
  (check ($min-fixnum-flonum 1 -2.3)		=> -2.3)
  (check ($min-flonum-fixnum -2.3 1)		=> -2.3)

  (check (min 1 +inf.0)				=> +1.0)
  (check (min +inf.0 1)				=> +1.0)
  (check ($min-fixnum-number 1 +inf.0)		=> +1.0)
  (check ($min-number-fixnum +inf.0 1)		=> +1.0)
  (check ($min-fixnum-flonum 1 +inf.0)		=> +1.0)
  (check ($min-flonum-fixnum +inf.0 1)		=> +1.0)

  (check (min 1 -inf.0)				=> -inf.0)
  (check (min -inf.0 1)				=> -inf.0)
  (check ($min-fixnum-number 1 -inf.0)		=> -inf.0)
  (check ($min-number-fixnum -inf.0 1)		=> -inf.0)
  (check ($min-fixnum-flonum 1 -inf.0)		=> -inf.0)
  (check ($min-flonum-fixnum -inf.0 1)		=> -inf.0)

  (check (min 1 +nan.0)				=> +nan.0)
  (check (min +nan.0 1)				=> +nan.0)
  (check ($min-fixnum-number 1 +nan.0)		=> +nan.0)
  (check ($min-number-fixnum +nan.0 1)		=> +nan.0)
  (check ($min-fixnum-flonum 1 +nan.0)		=> +nan.0)
  (check ($min-flonum-fixnum +nan.0 1)		=> +nan.0)

  #t)


(parametrise ((check-test-name	'bignums))

;;; fixnums

  (check (min 1 BN1)			=> 1)
  (check (min BN1 1)			=> 1)
  (check ($min-number-fixnum BN1 1)	=> 1)
  (check ($min-fixnum-number 1 BN1)	=> 1)
  (check ($min-fixnum-bignum 1 BN1)	=> 1)

  (check (min 1 BN2)			=> 1)
  (check (min BN2 1)			=> 1)
  (check ($min-number-fixnum BN2 1)	=> 1)
  (check ($min-fixnum-number 1 BN2)	=> 1)
  (check ($min-fixnum-bignum 1 BN2)	=> 1)

  (check (min 1 BN3)			=> BN3)
  (check (min BN3 1)			=> BN3)
  (check ($min-number-fixnum BN3 1)	=> BN3)
  (check ($min-fixnum-number 1 BN3)	=> BN3)
  (check ($min-fixnum-bignum 1 BN3)	=> BN3)

  (check (min 1 BN4)			=> BN4)
  (check (min BN4 1)			=> BN4)
  (check ($min-number-fixnum BN4 1)	=> BN4)
  (check ($min-fixnum-number 1 BN4)	=> BN4)
  (check ($min-fixnum-bignum 1 BN4)	=> BN4)

  (check (min (least-fixnum) BN1)			=> (least-fixnum))
  (check (min BN1 (least-fixnum))			=> (least-fixnum))
  (check ($min-number-fixnum BN1 (least-fixnum))	=> (least-fixnum))
  (check ($min-fixnum-number (least-fixnum) BN1)	=> (least-fixnum))
  (check ($min-fixnum-bignum (least-fixnum) BN1)	=> (least-fixnum))

  (check (min (least-fixnum) BN2)			=> (least-fixnum))
  (check (min BN2 (least-fixnum))			=> (least-fixnum))
  (check ($min-number-fixnum BN2 (least-fixnum))	=> (least-fixnum))
  (check ($min-fixnum-number (least-fixnum) BN2)	=> (least-fixnum))
  (check ($min-fixnum-bignum (least-fixnum) BN2)	=> (least-fixnum))

  (check (min (least-fixnum) BN3)			=> BN3)
  (check (min BN3 (least-fixnum))			=> BN3)
  (check ($min-number-fixnum BN3 (least-fixnum))	=> BN3)
  (check ($min-fixnum-number (least-fixnum) BN3)	=> BN3)
  (check ($min-fixnum-bignum (least-fixnum) BN3)	=> BN3)

  (check (min (least-fixnum) BN4)			=> BN4)
  (check (min BN4 (least-fixnum))			=> BN4)
  (check ($min-number-fixnum BN4 (least-fixnum))	=> BN4)
  (check ($min-fixnum-number (least-fixnum) BN4)	=> BN4)
  (check ($min-fixnum-bignum (least-fixnum) BN4)	=> BN4)

  (check (min (greatest-fixnum) BN1)			=> (greatest-fixnum))
  (check (min BN1 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($min-number-fixnum BN1 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($min-fixnum-number (greatest-fixnum) BN1)	=> (greatest-fixnum))
  (check ($min-fixnum-bignum (greatest-fixnum) BN1)	=> (greatest-fixnum))

  (check (min (greatest-fixnum) BN2)			=> (greatest-fixnum))
  (check (min BN2 (greatest-fixnum))			=> (greatest-fixnum))
  (check ($min-number-fixnum BN2 (greatest-fixnum))	=> (greatest-fixnum))
  (check ($min-fixnum-number (greatest-fixnum) BN2)	=> (greatest-fixnum))
  (check ($min-fixnum-bignum (greatest-fixnum) BN2)	=> (greatest-fixnum))

  (check (min (greatest-fixnum) BN3)			=> BN3)
  (check (min BN3 (greatest-fixnum))			=> BN3)
  (check ($min-number-fixnum BN3 (greatest-fixnum))	=> BN3)
  (check ($min-fixnum-number (greatest-fixnum) BN3)	=> BN3)
  (check ($min-fixnum-bignum (greatest-fixnum) BN3)	=> BN3)

  (check (min (greatest-fixnum) BN4)			=> BN4)
  (check (min BN4 (greatest-fixnum))			=> BN4)
  (check ($min-number-fixnum BN4 (greatest-fixnum))	=> BN4)
  (check ($min-fixnum-number (greatest-fixnum) BN4)	=> BN4)
  (check ($min-fixnum-bignum (greatest-fixnum) BN4)	=> BN4)

;;; --------------------------------------------------------------------
;;; bignums

  (check (min BN1 BN2)			=> BN1)
  (check (min BN2 BN1)			=> BN1)
  (check ($min-number-bignum BN1 BN2)	=> BN1)
  (check ($min-number-bignum BN2 BN1)	=> BN1)
  (check ($min-bignum-number BN1 BN2)	=> BN1)
  (check ($min-bignum-number BN2 BN1)	=> BN1)
  (check ($min-bignum-bignum BN1 BN2)	=> BN1)
  (check ($min-bignum-bignum BN2 BN1)	=> BN1)

  (check (min BN3 BN2)			=> BN3)
  (check (min BN2 BN3)			=> BN3)
  (check ($min-number-bignum BN3 BN2)	=> BN3)
  (check ($min-number-bignum BN2 BN3)	=> BN3)
  (check ($min-bignum-number BN3 BN2)	=> BN3)
  (check ($min-bignum-number BN2 BN3)	=> BN3)
  (check ($min-bignum-bignum BN3 BN2)	=> BN3)
  (check ($min-bignum-bignum BN2 BN3)	=> BN3)

  (check (min BN4 BN2)			=> BN4)
  (check (min BN2 BN4)			=> BN4)
  (check ($min-number-bignum BN4 BN2)	=> BN4)
  (check ($min-number-bignum BN2 BN4)	=> BN4)
  (check ($min-bignum-number BN4 BN2)	=> BN4)
  (check ($min-bignum-number BN2 BN4)	=> BN4)
  (check ($min-bignum-bignum BN4 BN2)	=> BN4)
  (check ($min-bignum-bignum BN2 BN4)	=> BN4)

  (check (min BN4 BN3)			=> BN4)
  (check (min BN3 BN4)			=> BN4)
  (check ($min-number-bignum BN4 BN3)	=> BN4)
  (check ($min-number-bignum BN3 BN4)	=> BN4)
  (check ($min-bignum-number BN4 BN3)	=> BN4)
  (check ($min-bignum-number BN3 BN4)	=> BN4)
  (check ($min-bignum-bignum BN4 BN3)	=> BN4)
  (check ($min-bignum-bignum BN3 BN4)	=> BN4)

;;; --------------------------------------------------------------------
;;; ratnums

  (check (min BN1 3/2)				=> 3/2)
  (check (min 3/2 BN1)				=> 3/2)
  (check ($min-bignum-number BN1 3/2)		=> 3/2)
  (check ($min-number-bignum 3/2 BN1)		=> 3/2)
  (check ($min-bignum-ratnum BN1 3/2)		=> 3/2)
  (check ($min-ratnum-bignum 3/2 BN1)		=> 3/2)

  (check (min BN3 3/2)				=> BN3)
  (check (min 3/2 BN3)				=> BN3)
  (check ($min-bignum-number BN3 3/2)		=> BN3)
  (check ($min-number-bignum 3/2 BN3)		=> BN3)
  (check ($min-bignum-ratnum BN3 3/2)		=> BN3)
  (check ($min-ratnum-bignum 3/2 BN3)		=> BN3)

;;; --------------------------------------------------------------------
;;; flonums

  (check (min BN1 2.3)				=> 2.3)
  (check (min 2.3 BN1)				=> 2.3)
  (check ($min-bignum-number BN1 2.3)		=> 2.3)
  (check ($min-number-bignum 2.3 BN1)		=> 2.3)
  (check ($min-bignum-flonum BN1 2.3)		=> 2.3)
  (check ($min-flonum-bignum 2.3 BN1)		=> 2.3)

  (check (min BN1 -2.3)				=> -2.3)
  (check (min -2.3 BN1)				=> -2.3)
  (check ($min-bignum-number BN1 -2.3)		=> -2.3)
  (check ($min-number-bignum -2.3 BN1)		=> -2.3)
  (check ($min-bignum-flonum BN1 -2.3)		=> -2.3)
  (check ($min-flonum-bignum -2.3 BN1)		=> -2.3)

  (check (min BN1 +inf.0)			=> (inexact BN1))
  (check (min +inf.0 BN1)			=> (inexact BN1))
  (check ($min-bignum-number BN1 +inf.0)	=> (inexact BN1))
  (check ($min-number-bignum +inf.0 BN1)	=> (inexact BN1))
  (check ($min-bignum-flonum BN1 +inf.0)	=> (inexact BN1))
  (check ($min-flonum-bignum +inf.0 BN1)	=> (inexact BN1))

  (check (min BN1 -inf.0)			=> -inf.0)
  (check (min -inf.0 BN1)			=> -inf.0)
  (check ($min-bignum-number BN1 -inf.0)	=> -inf.0)
  (check ($min-number-bignum -inf.0 BN1)	=> -inf.0)
  (check ($min-bignum-flonum BN1 -inf.0)	=> -inf.0)
  (check ($min-flonum-bignum -inf.0 BN1)	=> -inf.0)

  (check (min BN1 +nan.0)			=> +nan.0)
  (check (min +nan.0 BN1)			=> +nan.0)
  (check ($min-bignum-number BN1 +nan.0)	=> +nan.0)
  (check ($min-number-bignum +nan.0 BN1)	=> +nan.0)
  (check ($min-bignum-flonum BN1 +nan.0)	=> +nan.0)
  (check ($min-flonum-bignum +nan.0 BN1)	=> +nan.0)
;;;
  (check (min BN2 2.3)				=> 2.3)
  (check (min 2.3 BN2)				=> 2.3)
  (check ($min-bignum-number BN2 2.3)		=> 2.3)
  (check ($min-number-bignum 2.3 BN2)		=> 2.3)
  (check ($min-bignum-flonum BN2 2.3)		=> 2.3)
  (check ($min-flonum-bignum 2.3 BN2)		=> 2.3)

  (check (min BN2 -2.3)				=> -2.3)
  (check (min -2.3 BN2)				=> -2.3)
  (check ($min-bignum-number BN2 -2.3)		=> -2.3)
  (check ($min-number-bignum -2.3 BN2)		=> -2.3)
  (check ($min-bignum-flonum BN2 -2.3)		=> -2.3)
  (check ($min-flonum-bignum -2.3 BN2)		=> -2.3)

  (check (min BN2 +inf.0)			=> (inexact BN2))
  (check (min +inf.0 BN2)			=> (inexact BN2))
  (check ($min-bignum-number BN2 +inf.0)	=> (inexact BN2))
  (check ($min-number-bignum +inf.0 BN2)	=> (inexact BN2))
  (check ($min-bignum-flonum BN2 +inf.0)	=> (inexact BN2))
  (check ($min-flonum-bignum +inf.0 BN2)	=> (inexact BN2))

  (check (min BN2 -inf.0)			=> -inf.0)
  (check (min -inf.0 BN2)			=> -inf.0)
  (check ($min-bignum-number BN2 -inf.0)	=> -inf.0)
  (check ($min-number-bignum -inf.0 BN2)	=> -inf.0)
  (check ($min-bignum-flonum BN2 -inf.0)	=> -inf.0)
  (check ($min-flonum-bignum -inf.0 BN2)	=> -inf.0)

  (check (min BN2 +nan.0)			=> +nan.0)
  (check (min +nan.0 BN2)			=> +nan.0)
  (check ($min-bignum-number BN2 +nan.0)	=> +nan.0)
  (check ($min-number-bignum +nan.0 BN2)	=> +nan.0)
  (check ($min-bignum-flonum BN2 +nan.0)	=> +nan.0)
  (check ($min-flonum-bignum +nan.0 BN2)	=> +nan.0)
;;;
  (check (min BN3 2.3)				=> (inexact BN3))
  (check (min 2.3 BN3)				=> (inexact BN3))
  (check ($min-bignum-number BN3 2.3)		=> (inexact BN3))
  (check ($min-number-bignum 2.3 BN3)		=> (inexact BN3))
  (check ($min-bignum-flonum BN3 2.3)		=> (inexact BN3))
  (check ($min-flonum-bignum 2.3 BN3)		=> (inexact BN3))

  (check (min BN3 -2.3)				=> (inexact BN3))
  (check (min -2.3 BN3)				=> (inexact BN3))
  (check ($min-bignum-number BN3 -2.3)		=> (inexact BN3))
  (check ($min-number-bignum -2.3 BN3)		=> (inexact BN3))
  (check ($min-bignum-flonum BN3 -2.3)		=> (inexact BN3))
  (check ($min-flonum-bignum -2.3 BN3)		=> (inexact BN3))

  (check (min BN3 +inf.0)			=> (inexact BN3))
  (check (min +inf.0 BN3)			=> (inexact BN3))
  (check ($min-bignum-number BN3 +inf.0)	=> (inexact BN3))
  (check ($min-number-bignum +inf.0 BN3)	=> (inexact BN3))
  (check ($min-bignum-flonum BN3 +inf.0)	=> (inexact BN3))
  (check ($min-flonum-bignum +inf.0 BN3)	=> (inexact BN3))

  (check (min BN3 -inf.0)			=> -inf.0)
  (check (min -inf.0 BN3)			=> -inf.0)
  (check ($min-bignum-number BN3 -inf.0)	=> -inf.0)
  (check ($min-number-bignum -inf.0 BN3)	=> -inf.0)
  (check ($min-bignum-flonum BN3 -inf.0)	=> -inf.0)
  (check ($min-flonum-bignum -inf.0 BN3)	=> -inf.0)

  (check (min BN3 +nan.0)			=> +nan.0)
  (check (min +nan.0 BN3)			=> +nan.0)
  (check ($min-bignum-number BN3 +nan.0)	=> +nan.0)
  (check ($min-number-bignum +nan.0 BN3)	=> +nan.0)
  (check ($min-bignum-flonum BN3 +nan.0)	=> +nan.0)
  (check ($min-flonum-bignum +nan.0 BN3)	=> +nan.0)
;;;
  (check (min BN4 2.3)				=> (inexact BN4))
  (check (min 2.3 BN4)				=> (inexact BN4))
  (check ($min-bignum-number BN4 2.3)		=> (inexact BN4))
  (check ($min-number-bignum 2.3 BN4)		=> (inexact BN4))
  (check ($min-bignum-flonum BN4 2.3)		=> (inexact BN4))
  (check ($min-flonum-bignum 2.3 BN4)		=> (inexact BN4))

  (check (min BN4 -2.3)				=> (inexact BN4))
  (check (min -2.3 BN4)				=> (inexact BN4))
  (check ($min-bignum-number BN4 -2.3)		=> (inexact BN4))
  (check ($min-number-bignum -2.3 BN4)		=> (inexact BN4))
  (check ($min-bignum-flonum BN4 -2.3)		=> (inexact BN4))
  (check ($min-flonum-bignum -2.3 BN4)		=> (inexact BN4))

  (check (min BN4 +inf.0)			=> (inexact BN4))
  (check (min +inf.0 BN4)			=> (inexact BN4))
  (check ($min-bignum-number BN4 +inf.0)	=> (inexact BN4))
  (check ($min-number-bignum +inf.0 BN4)	=> (inexact BN4))
  (check ($min-bignum-flonum BN4 +inf.0)	=> (inexact BN4))
  (check ($min-flonum-bignum +inf.0 BN4)	=> (inexact BN4))

  (check (min BN4 -inf.0)			=> -inf.0)
  (check (min -inf.0 BN4)			=> -inf.0)
  (check ($min-bignum-number BN4 -inf.0)	=> -inf.0)
  (check ($min-number-bignum -inf.0 BN4)	=> -inf.0)
  (check ($min-bignum-flonum BN4 -inf.0)	=> -inf.0)
  (check ($min-flonum-bignum -inf.0 BN4)	=> -inf.0)

  (check (min BN4 +nan.0)			=> +nan.0)
  (check (min +nan.0 BN4)			=> +nan.0)
  (check ($min-bignum-number BN4 +nan.0)	=> +nan.0)
  (check ($min-number-bignum +nan.0 BN4)	=> +nan.0)
  (check ($min-bignum-flonum BN4 +nan.0)	=> +nan.0)
  (check ($min-flonum-bignum +nan.0 BN4)	=> +nan.0)

  #t)


(parametrise ((check-test-name	'ratnums))

;;; fixnums

  (check (min 1 RN1)			=> 1)
  (check (min RN1 1)			=> 1)
  (check ($min-number-fixnum RN1 1)	=> 1)
  (check ($min-fixnum-number 1 RN1)	=> 1)
  (check ($min-fixnum-ratnum 1 RN1)	=> 1)

  (check (min 1 RN2)			=> 1)
  (check (min RN2 1)			=> 1)
  (check ($min-number-fixnum RN2 1)	=> 1)
  (check ($min-fixnum-number 1 RN2)	=> 1)
  (check ($min-fixnum-ratnum 1 RN2)	=> 1)

  (check (min 1 RN3)			=> RN3)
  (check (min RN3 1)			=> RN3)
  (check ($min-number-fixnum RN3 1)	=> RN3)
  (check ($min-fixnum-number 1 RN3)	=> RN3)
  (check ($min-fixnum-ratnum 1 RN3)	=> RN3)

  (check (min 1 RN4)			=> RN4)
  (check (min RN4 1)			=> RN4)
  (check ($min-number-fixnum RN4 1)	=> RN4)
  (check ($min-fixnum-number 1 RN4)	=> RN4)
  (check ($min-fixnum-ratnum 1 RN4)	=> RN4)

  (check (min (least-fixnum) RN1)			=> (least-fixnum))
  (check (min RN1 (least-fixnum))			=> (least-fixnum))
  (check ($min-number-fixnum RN1 (least-fixnum))	=> (least-fixnum))
  (check ($min-fixnum-number (least-fixnum) RN1)	=> (least-fixnum))
  (check ($min-fixnum-ratnum (least-fixnum) RN1)	=> (least-fixnum))

  (check (min (least-fixnum) RN2)			=> (least-fixnum))
  (check (min RN2 (least-fixnum))			=> (least-fixnum))
  (check ($min-number-fixnum RN2 (least-fixnum))	=> (least-fixnum))
  (check ($min-fixnum-number (least-fixnum) RN2)	=> (least-fixnum))
  (check ($min-fixnum-ratnum (least-fixnum) RN2)	=> (least-fixnum))

  (check (min (least-fixnum) RN3)			=> (least-fixnum))
  (check (min RN3 (least-fixnum))			=> (least-fixnum))
  (check ($min-number-fixnum RN3 (least-fixnum))	=> (least-fixnum))
  (check ($min-fixnum-number (least-fixnum) RN3)	=> (least-fixnum))
  (check ($min-fixnum-ratnum (least-fixnum) RN3)	=> (least-fixnum))

  (check (min (least-fixnum) RN4)			=> (least-fixnum))
  (check (min RN4 (least-fixnum))			=> (least-fixnum))
  (check ($min-number-fixnum RN4 (least-fixnum))	=> (least-fixnum))
  (check ($min-fixnum-number (least-fixnum) RN4)	=> (least-fixnum))
  (check ($min-fixnum-ratnum (least-fixnum) RN4)	=> (least-fixnum))

  (check (min (greatest-fixnum) RN1)			=> RN1)
  (check (min RN1 (greatest-fixnum))			=> RN1)
  (check ($min-number-fixnum RN1 (greatest-fixnum))	=> RN1)
  (check ($min-fixnum-number (greatest-fixnum) RN1)	=> RN1)
  (check ($min-fixnum-ratnum (greatest-fixnum) RN1)	=> RN1)

  (check (min (greatest-fixnum) RN2)			=> RN2)
  (check (min RN2 (greatest-fixnum))			=> RN2)
  (check ($min-number-fixnum RN2 (greatest-fixnum))	=> RN2)
  (check ($min-fixnum-number (greatest-fixnum) RN2)	=> RN2)
  (check ($min-fixnum-ratnum (greatest-fixnum) RN2)	=> RN2)

  (check (min (greatest-fixnum) RN3)			=> RN3)
  (check (min RN3 (greatest-fixnum))			=> RN3)
  (check ($min-number-fixnum RN3 (greatest-fixnum))	=> RN3)
  (check ($min-fixnum-number (greatest-fixnum) RN3)	=> RN3)
  (check ($min-fixnum-ratnum (greatest-fixnum) RN3)	=> RN3)

  (check (min (greatest-fixnum) RN4)			=> RN4)
  (check (min RN4 (greatest-fixnum))			=> RN4)
  (check ($min-number-fixnum RN4 (greatest-fixnum))	=> RN4)
  (check ($min-fixnum-number (greatest-fixnum) RN4)	=> RN4)
  (check ($min-fixnum-ratnum (greatest-fixnum) RN4)	=> RN4)

;;; --------------------------------------------------------------------
;;; ratnums

  (check (min RN1 RN2)			=> RN1)
  (check (min RN2 RN1)			=> RN1)
  (check ($min-number-ratnum RN1 RN2)	=> RN1)
  (check ($min-number-ratnum RN2 RN1)	=> RN1)
  (check ($min-ratnum-number RN1 RN2)	=> RN1)
  (check ($min-ratnum-number RN2 RN1)	=> RN1)
  (check ($min-ratnum-ratnum RN1 RN2)	=> RN1)
  (check ($min-ratnum-ratnum RN2 RN1)	=> RN1)

  (check (min RN3 RN2)			=> RN3)
  (check (min RN2 RN3)			=> RN3)
  (check ($min-number-ratnum RN3 RN2)	=> RN3)
  (check ($min-number-ratnum RN2 RN3)	=> RN3)
  (check ($min-ratnum-number RN3 RN2)	=> RN3)
  (check ($min-ratnum-number RN2 RN3)	=> RN3)
  (check ($min-ratnum-ratnum RN3 RN2)	=> RN3)
  (check ($min-ratnum-ratnum RN2 RN3)	=> RN3)

  (check (min RN4 RN2)			=> RN4)
  (check (min RN2 RN4)			=> RN4)
  (check ($min-number-ratnum RN4 RN2)	=> RN4)
  (check ($min-number-ratnum RN2 RN4)	=> RN4)
  (check ($min-ratnum-number RN4 RN2)	=> RN4)
  (check ($min-ratnum-number RN2 RN4)	=> RN4)
  (check ($min-ratnum-ratnum RN4 RN2)	=> RN4)
  (check ($min-ratnum-ratnum RN2 RN4)	=> RN4)

  (check (min RN4 RN3)			=> RN4)
  (check (min RN3 RN4)			=> RN4)
  (check ($min-number-ratnum RN4 RN3)	=> RN4)
  (check ($min-number-ratnum RN3 RN4)	=> RN4)
  (check ($min-ratnum-number RN4 RN3)	=> RN4)
  (check ($min-ratnum-number RN3 RN4)	=> RN4)
  (check ($min-ratnum-ratnum RN4 RN3)	=> RN4)
  (check ($min-ratnum-ratnum RN3 RN4)	=> RN4)

;;; --------------------------------------------------------------------
;;; ratnums

  (check (min RN1 3/2)				=> RN1)
  (check (min 3/2 RN1)				=> RN1)
  (check ($min-ratnum-number RN1 3/2)		=> RN1)
  (check ($min-number-ratnum 3/2 RN1)		=> RN1)
  (check ($min-ratnum-ratnum RN1 3/2)		=> RN1)
  (check ($min-ratnum-ratnum 3/2 RN1)		=> RN1)

  (check (min RN1 -3/2)				=> -3/2)
  (check (min -3/2 RN1)				=> -3/2)
  (check ($min-ratnum-number RN1 -3/2)		=> -3/2)
  (check ($min-number-ratnum -3/2 RN1)		=> -3/2)
  (check ($min-ratnum-ratnum RN1 -3/2)		=> -3/2)
  (check ($min-ratnum-ratnum -3/2 RN1)		=> -3/2)

;;; --------------------------------------------------------------------
;;; flonums

  (check (min RN1 2.3)				=> (inexact RN1))
  (check (min 2.3 RN1)				=> (inexact RN1))
  (check ($min-ratnum-number RN1 2.3)		=> (inexact RN1))
  (check ($min-number-ratnum 2.3 RN1)		=> (inexact RN1))
  (check ($min-ratnum-flonum RN1 2.3)		=> (inexact RN1))
  (check ($min-flonum-ratnum 2.3 RN1)		=> (inexact RN1))

  (check (min RN1 -2.3)				=> -2.3)
  (check (min -2.3 RN1)				=> -2.3)
  (check ($min-ratnum-number RN1 -2.3)		=> -2.3)
  (check ($min-number-ratnum -2.3 RN1)		=> -2.3)
  (check ($min-ratnum-flonum RN1 -2.3)		=> -2.3)
  (check ($min-flonum-ratnum -2.3 RN1)		=> -2.3)

  (check (min RN1 +inf.0)			=> (inexact RN1))
  (check (min +inf.0 RN1)			=> (inexact RN1))
  (check ($min-ratnum-number RN1 +inf.0)	=> (inexact RN1))
  (check ($min-number-ratnum +inf.0 RN1)	=> (inexact RN1))
  (check ($min-ratnum-flonum RN1 +inf.0)	=> (inexact RN1))
  (check ($min-flonum-ratnum +inf.0 RN1)	=> (inexact RN1))

  (check (min RN1 -inf.0)			=> -inf.0)
  (check (min -inf.0 RN1)			=> -inf.0)
  (check ($min-ratnum-number RN1 -inf.0)	=> -inf.0)
  (check ($min-number-ratnum -inf.0 RN1)	=> -inf.0)
  (check ($min-ratnum-flonum RN1 -inf.0)	=> -inf.0)
  (check ($min-flonum-ratnum -inf.0 RN1)	=> -inf.0)

  (check (min RN1 +nan.0)			=> +nan.0)
  (check (min +nan.0 RN1)			=> +nan.0)
  (check ($min-ratnum-number RN1 +nan.0)	=> +nan.0)
  (check ($min-number-ratnum +nan.0 RN1)	=> +nan.0)
  (check ($min-ratnum-flonum RN1 +nan.0)	=> +nan.0)
  (check ($min-flonum-ratnum +nan.0 RN1)	=> +nan.0)
;;;
  (check (min RN3 2.3)				=> (inexact RN3))
  (check (min 2.3 RN3)				=> (inexact RN3))
  (check ($min-ratnum-number RN3 2.3)		=> (inexact RN3))
  (check ($min-number-ratnum 2.3 RN3)		=> (inexact RN3))
  (check ($min-ratnum-flonum RN3 2.3)		=> (inexact RN3))
  (check ($min-flonum-ratnum 2.3 RN3)		=> (inexact RN3))

  (check (min RN3 -2.3)				=> -2.3)
  (check (min -2.3 RN3)				=> -2.3)
  (check ($min-ratnum-number RN3 -2.3)		=> -2.3)
  (check ($min-number-ratnum -2.3 RN3)		=> -2.3)
  (check ($min-ratnum-flonum RN3 -2.3)		=> -2.3)
  (check ($min-flonum-ratnum -2.3 RN3)		=> -2.3)

  (check (min RN3 +inf.0)			=> (inexact RN3))
  (check (min +inf.0 RN3)			=> (inexact RN3))
  (check ($min-ratnum-number RN3 +inf.0)	=> (inexact RN3))
  (check ($min-number-ratnum +inf.0 RN3)	=> (inexact RN3))
  (check ($min-ratnum-flonum RN3 +inf.0)	=> (inexact RN3))
  (check ($min-flonum-ratnum +inf.0 RN3)	=> (inexact RN3))

  (check (min RN3 -inf.0)			=> -inf.0)
  (check (min -inf.0 RN3)			=> -inf.0)
  (check ($min-ratnum-number RN3 -inf.0)	=> -inf.0)
  (check ($min-number-ratnum -inf.0 RN3)	=> -inf.0)
  (check ($min-ratnum-flonum RN3 -inf.0)	=> -inf.0)
  (check ($min-flonum-ratnum -inf.0 RN3)	=> -inf.0)

  (check (min RN3 +nan.0)			=> +nan.0)
  (check (min +nan.0 RN3)			=> +nan.0)
  (check ($min-ratnum-number RN3 +nan.0)	=> +nan.0)
  (check ($min-number-ratnum +nan.0 RN3)	=> +nan.0)
  (check ($min-ratnum-flonum RN3 +nan.0)	=> +nan.0)
  (check ($min-flonum-ratnum +nan.0 RN3)	=> +nan.0)

  #t)


(parametrise ((check-test-name	'flonums))

;;; flonums

  (check (min 1.0 2.0)			=> 1.0)
  (check (min 2.0 1.0)			=> 1.0)
  (check ($min-number-flonum 1.0 2.0)	=> 1.0)
  (check ($min-number-flonum 2.0 1.0)	=> 1.0)
  (check ($min-flonum-number 1.0 2.0)	=> 1.0)
  (check ($min-flonum-number 2.0 1.0)	=> 1.0)
  (check ($min-flonum-flonum 1.0 2.0)	=> 1.0)
  (check ($min-flonum-flonum 2.0 1.0)	=> 1.0)

;;; --------------------------------------------------------------------
;;; bignums

  (check (min 1.0 BN1)			=> 1.0)
  (check (min BN1 1.0)			=> 1.0)
  (check ($min-number-flonum BN1 1.0)	=> 1.0)
  (check ($min-flonum-number 1.0 BN1)	=> 1.0)
  (check ($min-flonum-bignum 1.0 BN1)	=> 1.0)

  (check (min 1.0 BN2)			=> 1.0)
  (check (min BN2 1.0)			=> 1.0)
  (check ($min-number-flonum BN2 1.0)	=> 1.0)
  (check ($min-flonum-number 1.0 BN2)	=> 1.0)
  (check ($min-flonum-bignum 1.0 BN2)	=> 1.0)

  (check (min 1.0 BN3)			=> (inexact BN3))
  (check (min BN3 1.0)			=> (inexact BN3))
  (check ($min-number-flonum BN3 1.0)	=> (inexact BN3))
  (check ($min-flonum-number 1.0 BN3)	=> (inexact BN3))
  (check ($min-flonum-bignum 1.0 BN3)	=> (inexact BN3))

  (check (min 1.0 BN4)			=> (inexact BN4))
  (check (min BN4 1.0)			=> (inexact BN4))
  (check ($min-number-flonum BN4 1.0)	=> (inexact BN4))
  (check ($min-flonum-number 1.0 BN4)	=> (inexact BN4))
  (check ($min-flonum-bignum 1.0 BN4)	=> (inexact BN4))

;;; --------------------------------------------------------------------
;;; ratnums

  (check (min 1.0 3/2)			=> 1.0)
  (check (min 3/2 1.0)			=> 1.0)
  (check ($min-flonum-number 1.0 3/2)	=> 1.0)
  (check ($min-number-flonum 3/2 1.0)	=> 1.0)
  (check ($min-flonum-ratnum 1.0 3/2)	=> 1.0)
  (check ($min-ratnum-flonum 3/2 1.0)	=> 1.0)

  (check (min 1.0 -3/2)			=> (inexact -3/2))
  (check (min -3/2 1.0)			=> (inexact -3/2))
  (check ($min-flonum-number 1.0 -3/2)	=> (inexact -3/2))
  (check ($min-number-flonum -3/2 1.0)	=> (inexact -3/2))
  (check ($min-flonum-ratnum 1.0 -3/2)	=> (inexact -3/2))
  (check ($min-ratnum-flonum -3/2 1.0)	=> (inexact -3/2))

;;; --------------------------------------------------------------------
;;; flonums

  (check (min 1.0 2.0)			=> 1.0)
  (check (min 2.0 1.0)			=> 1.0)
  (check ($min-flonum-number 1.0 2.0)	=> 1.0)
  (check ($min-number-flonum 2.0 1.0)	=> 1.0)
  (check ($min-flonum-flonum 1.0 2.0)	=> 1.0)
  (check ($min-flonum-flonum 2.0 1.0)	=> 1.0)

  (check (min 1.0 -2.3)			=> -2.3)
  (check (min -2.3 1.0)			=> -2.3)
  (check ($min-flonum-number 1.0 -2.3)	=> -2.3)
  (check ($min-number-flonum -2.3 1.0)	=> -2.3)
  (check ($min-flonum-flonum 1.0 -2.3)	=> -2.3)
  (check ($min-flonum-flonum -2.3 1.0)	=> -2.3)

  (check (min 1.0 +inf.0)			=> 1.0)
  (check (min +inf.0 1.0)			=> 1.0)
  (check ($min-flonum-number 1.0 +inf.0)	=> 1.0)
  (check ($min-number-flonum +inf.0 1.0)	=> 1.0)
  (check ($min-flonum-flonum 1.0 +inf.0)	=> 1.0)
  (check ($min-flonum-flonum +inf.0 1.0)	=> 1.0)

  (check (min 1.0 -inf.0)			=> -inf.0)
  (check (min -inf.0 1.0)			=> -inf.0)
  (check ($min-flonum-number 1.0 -inf.0)	=> -inf.0)
  (check ($min-number-flonum -inf.0 1.0)	=> -inf.0)
  (check ($min-flonum-flonum 1.0 -inf.0)	=> -inf.0)
  (check ($min-flonum-flonum -inf.0 1.0)	=> -inf.0)

  (check (min 1.0 +nan.0)			=> +nan.0)
  (check (min +nan.0 1.0)			=> +nan.0)
  (check ($min-flonum-number 1.0 +nan.0)	=> +nan.0)
  (check ($min-number-flonum +nan.0 1.0)	=> +nan.0)
  (check ($min-flonum-flonum 1.0 +nan.0)	=> +nan.0)
  (check ($min-flonum-flonum +nan.0 1.0)	=> +nan.0)

  #t)


;;;; done

(check-report)

;;; end of file
