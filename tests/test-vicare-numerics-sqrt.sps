;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numeric functions: sqrt
;;;Date: Sat Nov 17, 2012
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
  (vicare checks)
  (only (vicare syntactic-extensions)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numeric functions: sqrt, square root\n")


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

  (check (sqrt 0)		=> 0)

  (check (sqrt +1)		=> +1)
  (check (sqrt -1)		=> +1i)

  (check (sqrt +4)		=> +2)
  (check (sqrt -4)		=> +2i)

  (check (sqrt 5)		(=> %quasi=?) 2.23606797749979)
  (check (sqrt -5)		(=> %quasi=?) 0.0+2.23606797749979i)

;;; --------------------------------------------------------------------

  (check ($sqrt-fixnum 0)	=> 0)

  (check ($sqrt-fixnum +1)	=> +1)
  (check ($sqrt-fixnum -1)	=> +1i)

  (check ($sqrt-fixnum +4)	=> +2)
  (check ($sqrt-fixnum -4)	=> +2i)

  (check ($sqrt-fixnum 5)	(=> %quasi=?) 2.23606797749979)
  (check ($sqrt-fixnum -5)	(=> %quasi=?) 0.0+2.23606797749979i)

  #f)


(parametrise ((check-test-name	'bignums))

  (case-word-size
   ((32)
    (check (sqrt BN1)		(=> %quasi=?) 23170.475027499975)
    (check (sqrt BN2)		(=> %quasi=?) 23170.47522171265)
    (check (sqrt BN3)		(=> %quasi=?) 0.0+23170.475049079163i)
    (check (sqrt BN4)		(=> %quasi=?) 0.0+23170.47524329184i))
   ((64)
    (void)))

;;; --------------------------------------------------------------------

  (case-word-size
   ((32)
    (check ($sqrt-bignum BN1)	(=> %quasi=?) 23170.475027499975)
    (check ($sqrt-bignum BN2)	(=> %quasi=?) 23170.47522171265)
    (check ($sqrt-bignum BN3)	(=> %quasi=?) 0.0+23170.475049079163i)
    (check ($sqrt-bignum BN4)	(=> %quasi=?) 0.0+23170.47524329184i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (check (sqrt +1/2)		(=> %quasi=?) 0.7071067811865476)
  (check (sqrt -1/2)		(=> %quasi=?) 0+0.7071067811865476i)

  (check (sqrt +1/4)		=> 1/2)
  (check (sqrt -1/4)		=> 0+1/2i)

  (check (sqrt (/ BN1 BN2))	(=> %almost=?) 0.9999999916180969)

;;; --------------------------------------------------------------------

  (check ($sqrt-ratnum +1/2)	(=> %quasi=?) 0.7071067811865476)
  (check ($sqrt-ratnum -1/2)	(=> %quasi=?) 0+0.7071067811865476i)

  (check ($sqrt-ratnum +1/4)	=> 1/2)
  (check ($sqrt-ratnum -1/4)	=> 0+1/2i)

  #t)


(parametrise ((check-test-name	'flonums))

  (check (sqrt +0.0)		=> +0.0)
  (check (sqrt -0.0)		=> +0.0i)

  (check (sqrt +1.0)		=> +1.0)
  (check (sqrt -1.0)		=> +1.0i)

  (check (sqrt +4.0)		=> +2.0)
  (check (sqrt -4.0)		=> +2.0i)

  (check (sqrt +5.0)		(=> %quasi=?) 2.23606797749979)
  (check (sqrt -5.0)		(=> %quasi=?) 0.0+2.23606797749979i)

  (check (sqrt +inf.0)		=> +inf.0)
  (check (sqrt -inf.0)		=> +inf.0i)

  (check (sqrt +nan.0)		=> +nan.0)

;;; --------------------------------------------------------------------

  (check ($sqrt-flonum +0.0)	=> +0.0)
  (check ($sqrt-flonum -0.0)	=> +0.0i)

  (check ($sqrt-flonum +1.0)	=> +1.0)
  (check ($sqrt-flonum -1.0)	=> +1.0i)

  (check ($sqrt-flonum +4.0)	=> +2.0)
  (check ($sqrt-flonum -4.0)	=> +2.0i)

  (check ($sqrt-flonum +5.0)	(=> %quasi=?) 2.23606797749979)
  (check ($sqrt-flonum -5.0)	(=> %quasi=?) 0.0+2.23606797749979i)

  (check ($sqrt-flonum +inf.0)	=> +inf.0)
  (check ($sqrt-flonum -inf.0)	=> +inf.0i)

  (check ($sqrt-flonum +nan.0)	=> +nan.0)

  #f)


(parametrise ((check-test-name	'compnums))

  (check (sqrt 1+2i)		(=> %quasi=?) 1.272019649514069+0.7861513777574233i)
  (check (sqrt 1.+2i)		(=> %quasi=?) 1.272019649514069+0.7861513777574233i)
  (check (sqrt 1+2.i)		(=> %quasi=?) 1.272019649514069+0.7861513777574233i)

;;; --------------------------------------------------------------------

  (case-word-size
   ((32)
    (check
	(sqrt (make-rectangular 123 BN1))
      (=> %quasi=?) 16384.00189208995+16383.998138427844i)
    (check
	(sqrt (make-rectangular 123 BN2))
      (=> %quasi=?) 16384.002029419036+16383.998275756958i)
    (check
	(sqrt (make-rectangular 123 BN3))
      (=> %quasi=?) 16384.001907348735-16383.998153686636i)
    (check
	(sqrt (make-rectangular 123 BN4))
      (=> %quasi=?) 16384.00204467782-16383.99829101575i))
   ((64)
    (void)))

;;; --------------------------------------------------------------------

  (case-word-size
   ((32)
    (check
	(sqrt (make-rectangular BN1 123))
      (=> %quasi=?) 23170.475027500128+0.0026542399293500918i)
    (check
	(sqrt (make-rectangular BN2 123))
      (=> %quasi=?) 23170.475221712804+0.00265423990710251i)
    (check
	(sqrt (make-rectangular BN3 123))
      (=> %quasi=?) 0.0026542399268781377+23170.475049079316i)
    (check
	(sqrt (make-rectangular BN4 123))
      (=> %quasi=?) 0.0026542399046305563+23170.475243291992i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'cflonums))

  (check (sqrt 12+inf.0i)	=> +inf.0+inf.0i)
  (check (sqrt +inf.0+12i)	(=> nan=?) +nan.0)
  (check (sqrt +inf.0+inf.0i)	(=> nan=?) +nan.0)
  (check (sqrt +inf.0-inf.0i)	(=> nan=?) +nan.0)
  (check (sqrt -inf.0+inf.0i)	(=> nan=?) +nan.0)

  (check (sqrt +nan.0+12i)	(=> general=?) +nan.0+nan.0i)
  (check (sqrt 12+nan.0i)	(=> general=?) +nan.0+nan.0i)
  (check (sqrt +nan.0+nan.0i)	(=> general=?) +nan.0+nan.0i)

  #t)


(parametrise ((check-test-name	'exact-integer-sqrt))

  (check
      (let-values (((S R) (exact-integer-sqrt 0)))
	(list S R))
    => '(0 0))

  (check
      (let-values (((S R) (exact-integer-sqrt 4)))
	(list S R))
    => '(2 0))

  (check
      (let-values (((S R) (exact-integer-sqrt 5)))
	(list S R))
    => '(2 1))

;;; --------------------------------------------------------------------

  #t)


;;;; done

(check-report)

;;; end of file
