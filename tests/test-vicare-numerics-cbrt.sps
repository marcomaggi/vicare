;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numeric functions: cbrt
;;;Date: Fri Nov 29, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks)
  (only (vicare platform words)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numeric functions: cbrt, cubic root\n")


;;;; helpers

(define epsilon 1e-5)

(define (%quasi=? x y)
  (and (< (abs (- (real-part x)
		  (real-part y)))
	  epsilon)
       (< (abs (- (imag-part x)
		  (imag-part y)))
	  epsilon)))

(define (%almost=? x y)
  (let ((x.rep (real-part x))
	(x.imp (imag-part x))
	(y.rep (real-part y))
	(y.imp (imag-part y)))
    (and (if (or (zero? x.rep)
		 (zero? y.rep))
	     (%quasi=? x.rep y.rep)
	   (%almost-real=? x.rep y.rep))
	 (if (or (zero? x.imp)
		 (zero? y.imp))
	     (%quasi=? x.imp y.imp)
	   (%almost-real=? x.imp y.imp)))))

(define (%almost-real=? x y)
  (< (/ (abs (- x y))
	(abs x))
     epsilon))

(define (general=? x y)
  (or (= x y)
      (and (nan? (real-part x)) (nan? (imag-part x))
	   (nan? (real-part y)) (nan? (imag-part y)))))

(define (nan=? x y)
  (and (nan? x)
       (nan? y)))


;;;; constants

(define SMALLEST-POSITIVE-BIGNUM	(-    (least-fixnum)))
(define SMALLEST-NEGATIVE-BIGNUM	(+ -1 (least-fixnum)))

(define BN1	(+ +1  SMALLEST-POSITIVE-BIGNUM))
(define BN2	(+ +10 SMALLEST-POSITIVE-BIGNUM))
(define BN3	(+ -1  SMALLEST-NEGATIVE-BIGNUM))
(define BN4	(+ -10 SMALLEST-NEGATIVE-BIGNUM))


(parametrise ((check-test-name	'fixnums))

  (check (cbrt 0)		=> 0)

  (check (cbrt +1)		=> +1)
  (check (cbrt -1)		=> -1)

  (check (cbrt +4)		(=> %quasi=?) +1.58740105)
  (check (cbrt -4)		(=> %quasi=?) -1.58740105)

  (check (cbrt 5)		(=> %quasi=?) +1.70997594)
  (check (cbrt -5)		(=> %quasi=?) -1.70997594)

;;; --------------------------------------------------------------------

  (check ($cbrt-fixnum 0)	=> 0)

  (check ($cbrt-fixnum +1)	=> +1)
  (check ($cbrt-fixnum -1)	=> -1)

  (check ($cbrt-fixnum +4)	(=> %quasi=?) +1.58740105)
  (check ($cbrt-fixnum -4)	(=> %quasi=?) -1.58740105)

  (check ($cbrt-fixnum 5)	(=> %quasi=?) +1.70997594)
  (check ($cbrt-fixnum -5)	(=> %quasi=?) -1.70997594)

  #f)


(parametrise ((check-test-name	'bignums))

  #t)


(parametrise ((check-test-name	'ratnums))

  (check (cbrt +1/2)		(=> %quasi=?) +0.7937005)
  (check (cbrt -1/2)		(=> %quasi=?) -0.7937005)

  (check (cbrt +1/4)		(=> %quasi=?) +0.6299605249)
  (check (cbrt -1/4)		(=> %quasi=?) -0.6299605249)

;;; --------------------------------------------------------------------

  (check ($cbrt-ratnum +1/2)	(=> %quasi=?) +0.7937005)
  (check ($cbrt-ratnum -1/2)	(=> %quasi=?) -0.7937005)

  (check ($cbrt-ratnum +1/4)	(=> %quasi=?) +0.6299605249)
  (check ($cbrt-ratnum -1/4)	(=> %quasi=?) -0.6299605249)

  #t)


(parametrise ((check-test-name	'flonums))

  (check (cbrt +0.0)		=> +0.0)
  (check (cbrt -0.0)		=> -0.0)

  (check (cbrt +1.0)		=> +1.0)
  (check (cbrt -1.0)		=> -1.0)

  (check (cbrt +4.0)		(=> %quasi=?) +1.58740)
  (check (cbrt -4.0)		(=> %quasi=?) -1.58740)

  (check (cbrt +5.0)		(=> %quasi=?) +1.70998)
  (check (cbrt -5.0)		(=> %quasi=?) -1.70998)

  (check (cbrt +inf.0)		=> +inf.0)
  (check (cbrt -inf.0)		=> -inf.0)

  (check (cbrt +nan.0)		=> +nan.0)

;;; --------------------------------------------------------------------

  (check ($cbrt-flonum +0.0)	=> +0.0)
  (check ($cbrt-flonum -0.0)	=> -0.0)

  (check ($cbrt-flonum +1.0)	=> +1.0)
  (check ($cbrt-flonum -1.0)	=> -1.0)

  (check ($cbrt-flonum +4.0)	(=> %quasi=?) +1.58740)
  (check ($cbrt-flonum -4.0)	(=> %quasi=?) -1.58740)

  (check ($cbrt-flonum +5.0)	(=> %quasi=?) +1.70998)
  (check ($cbrt-flonum -5.0)	(=> %quasi=?) -1.70998)

  (check ($cbrt-flonum +inf.0)	=> +inf.0)
  (check ($cbrt-flonum -inf.0)	=> -inf.0)

  (check ($cbrt-flonum +nan.0)	=> +nan.0)

  #f)


(parametrise ((check-test-name	'compnums))

  (check (cbrt 1+2i)		(=> %quasi=?) 1.21962+0.47171i)
  (check (cbrt 1.+2i)		(=> %quasi=?) 1.21962+0.47171i)
  (check (cbrt 1+2.i)		(=> %quasi=?) 1.21962+0.47171i)

;;; --------------------------------------------------------------------

  (check ($cbrt-compnum 1+2i)		(=> %quasi=?) 1.21962+0.47171i)
  (check ($cbrt-compnum 1.+2i)		(=> %quasi=?) 1.21962+0.47171i)
  (check ($cbrt-compnum 1+2.i)		(=> %quasi=?) 1.21962+0.47171i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (check (cbrt 1.+2.i)		(=> %quasi=?) 1.21962+0.47171i)

;;; --------------------------------------------------------------------

  (check ($cbrt-cflonum 1.+2.i)		(=> %quasi=?) 1.21962+0.47171i)

  #t)


;;;; done

(check-report)

;;; end of file
