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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numeric functions: sqrt, square root\n")


;;;; helpers

(define epsilon 1e-5)

(define (quasi=? x y)
  (and (< (abs (- (real-part x)
		  (real-part y)))
	  epsilon)
       (< (abs (- (imag-part x)
		  (imag-part y)))
	  epsilon)))

(define (general=? x y)
  (or (= x y)
      (and (nan? (real-part x)) (nan? (imag-part x))
	   (nan? (real-part y)) (nan? (imag-part y)))))

(define (nan=? x y)
  (and (nan? x)
       (nan? y)))


(parametrise ((check-test-name	'sqrt))

;;; fixnums

  (check
      (sqrt 0)
    => 0)

  (check
      (sqrt 5)
    (=> quasi=?) 2.23606797749979)

  (check
      (sqrt -5)
    (=> quasi=?) 0.0+2.23606797749979i)

;;; --------------------------------------------------------------------
;;; flonums

  (check
      (sqrt +0.0)
    => +0.0)

  (check
      (sqrt -0.0)
    => -0.0)

  (check
      (sqrt +5.0)
    (=> quasi=?) 2.23606797749979)

  (check
      (sqrt -5.0)
    (=> quasi=?) 0.0+2.23606797749979i)

;;; --------------------------------------------------------------------
;;; infinities

  (check
      (sqrt +inf.0)
    => +inf.0)

  (check
      (sqrt -inf.0)
    => +inf.0i)

  (check
      (sqrt 12+inf.0i)
    => +inf.0+inf.0i)

  (check
      (sqrt +inf.0+12i)
    (=> nan=?) +nan.0)

  (check
      (sqrt +inf.0+inf.0i)
    (=> nan=?) +nan.0)

  (check
      (sqrt +inf.0-inf.0i)
    (=> nan=?) +nan.0)

  (check
      (sqrt -inf.0+inf.0i)
    (=> nan=?) +nan.0)

;;; --------------------------------------------------------------------
;;; not-a-number

  (check
      (sqrt +nan.0)
    => +nan.0)

  (check
      (sqrt +nan.0+12i)
    (=> general=?) +nan.0+nan.0i)

  (check
      (sqrt 12+nan.0i)
    (=> general=?) +nan.0+nan.0i)

  (check
      (sqrt +nan.0+nan.0i)
    (=> general=?) +nan.0+nan.0i)

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

  #t)


;;;; done

(check-report)

;;; end of file
