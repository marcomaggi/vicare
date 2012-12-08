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
(display "*** testing Vicare numerics functions: max\n")


;;;; constants

(define SMALLEST-POSITIVE-BIGNUM	(-    (least-fixnum)))
(define SMALLEST-NEGATIVE-BIGNUM	(+ -1 (least-fixnum)))

(define BN1	(+ +1  SMALLEST-POSITIVE-BIGNUM))
(define BN2	(+ +10 SMALLEST-POSITIVE-BIGNUM))
(define BN3	(+ -1  SMALLEST-NEGATIVE-BIGNUM))
(define BN4	(+ -10 SMALLEST-NEGATIVE-BIGNUM))


(parametrise ((check-test-name	'fixnums))

  (let-syntax
      ((test (syntax-rules ()
	       ((_ ?op1 ?op2 ?expected)
		(begin
		  (check (max ?op1 ?op2)		=> ?expected)
		  (check (max ?op2 ?op1)		=> ?expected)
		  (check ($max-number-fixnum ?op1 ?op2)	=> ?expected)
		  (check ($max-number-fixnum ?op2 ?op1)	=> ?expected)
		  (check ($max-fixnum-number ?op1 ?op2)	=> ?expected)
		  (check ($max-fixnum-number ?op2 ?op1)	=> ?expected)
		  (check ($max-fixnum-fixnum ?op1 ?op2)	=> ?expected)
		  (check ($max-fixnum-fixnum ?op2 ?op1)	=> ?expected)
		  )))))
    (test 1 2 2))

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


;;;; done

(check-report)

;;; end of file
