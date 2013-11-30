;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for mehve language, part functions
;;;Date: Fri Jun 17, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (nausicaa mehve)
  (prefix (nausicaa mehve language numerics parts) mehve.)
  (prefix (rnrs) rnrs.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Mehve language: numerics part functions\n")

(initialise-mehve)


;;;; helpers

(define (quasi=? X Y)
  (define (sgn x)
    (cond ((zero? x)		0)
	  ((positive? x)	+1)
	  ((negative? x)	-1)
	  (else			#f)))
  (define epsilon 1e-6)
  (define (zero? x)
    (< (abs x) epsilon))
  (cond ((and (real? X) (real? Y))
	 (or (and (nan? X) (nan? Y))
	     (and (infinite? X) (infinite? Y) (= (sgn X) (sgn Y)))
	     (and (eqv? (sgn X) (sgn Y))
		  (< (abs (- X Y)) epsilon))))
	((and (complex? X) (complex? Y))
	 (and (quasi=? (real-part X) (real-part Y))
	      (quasi=? (imag-part X) (imag-part Y))))
	((complex? X)
	 (and (zero? (imag-part X))
	      (quasi=? (real-part X) Y)))
	((complex? Y)
	 (and (zero? (imag-part Y))
	      (quasi=? (real-part Y) X)))
	(else
	 (assertion-violation 'quasi=? "invalid arguments" X Y))))


(parametrise ((check-test-name	'part-rationals))

  (define-method (mehve.numerator (o <vector>))
    (o map numerator))
  (define-method (mehve.numerator (o <spine>))
    (o map numerator))

  (define-method (mehve.denominator (o <vector>))
    (o map denominator))
  (define-method (mehve.denominator (o <spine>))
    (o map denominator))

  (define-method (mehve.rationalize (a <vector>) (epsilon <real>))
    (a map (lambda (x)
	     (rationalize x epsilon))))
  (define-method (mehve.rationalize (a <spine>) (epsilon <real>))
    (a map (lambda (x)
	     (rationalize x epsilon))))

;;; --------------------------------------------------------------------

  (check (numerator 1)			=> (rnrs.numerator 1))
  (check (numerator 1.1)		=> (rnrs.numerator 1.1))
  (check (numerator 2/3)		=> (rnrs.numerator 2/3))
  (check (numerator '(1 2 3))		=> (map rnrs.numerator '(1 2 3)))
  (check (numerator '#(1 2 3))		=> (vector-map rnrs.numerator '#(1 2 3)))

  (check (denominator 1)		=> (rnrs.denominator 1))
  (check (denominator 1.1)		=> (rnrs.denominator 1.1))
  (check (denominator 2/3)		=> (rnrs.denominator 2/3))
  (check (denominator '(1 2 3))		=> (map rnrs.denominator '(1 2 3)))
  (check (denominator '#(1 2 3))	=> (vector-map rnrs.denominator '#(1 2 3)))

  (check (rationalize 1 0.1)		=> (rnrs.rationalize 1 0.1))
  (check (rationalize 1.1 0.1)		=> (rnrs.rationalize 1.1 0.1))
  (check (rationalize 2/3 0.1)		=> (rnrs.rationalize 2/3 0.1))
  (check (rationalize '(1 2 3) 0.1)	=> (map (lambda (x)
						  (rnrs.rationalize x 0.1))
					     '(1 2 3)))
  (check (rationalize '#(1 2 3) 0.1)	=> (vector-map (lambda (x)
							 (rnrs.rationalize x 0.1))
						       '#(1 2 3)))

  #t)


(parametrise ((check-test-name	'part-rounding))

  (define-method (mehve.floor (o <vector>))
    (o map floor))
  (define-method (mehve.floor (o <spine>))
    (o map floor))

  (define-method (mehve.ceiling (o <vector>))
    (o map ceiling))
  (define-method (mehve.ceiling (o <spine>))
    (o map ceiling))

  (define-method (mehve.truncate (o <vector>))
    (o map truncate))
  (define-method (mehve.truncate (o <spine>))
    (o map truncate))

  (define-method (mehve.round (o <vector>))
    (o map round))
  (define-method (mehve.round (o <spine>))
    (o map round))

;;; --------------------------------------------------------------------

  (check (floor 1)			=> (rnrs.floor 1))
  (check (floor 1.1)			=> (rnrs.floor 1.1))
  (check (floor 2/3)			=> (rnrs.floor 2/3))
  (check (floor '(1 2 3))		=> (map rnrs.floor '(1 2 3)))
  (check (floor '#(1 2 3))		=> (vector-map rnrs.floor '#(1 2 3)))

  (check (ceiling 1)			=> (rnrs.ceiling 1))
  (check (ceiling 1.1)			=> (rnrs.ceiling 1.1))
  (check (ceiling 2/3)			=> (rnrs.ceiling 2/3))
  (check (ceiling '(1 2 3))		=> (map rnrs.ceiling '(1 2 3)))
  (check (ceiling '#(1 2 3))		=> (vector-map rnrs.ceiling '#(1 2 3)))

  (check (truncate 1)			=> (rnrs.truncate 1))
  (check (truncate 1.1)			=> (rnrs.truncate 1.1))
  (check (truncate 2/3)			=> (rnrs.truncate 2/3))
  (check (truncate '(1 2 3))		=> (map rnrs.truncate '(1 2 3)))
  (check (truncate '#(1 2 3))		=> (vector-map rnrs.truncate '#(1 2 3)))

  (check (round 1)			=> (rnrs.round 1))
  (check (round 1.1)			=> (rnrs.round 1.1))
  (check (round 2/3)			=> (rnrs.round 2/3))
  (check (round '(1 2 3))		=> (map rnrs.round '(1 2 3)))
  (check (round '#(1 2 3))		=> (vector-map rnrs.round '#(1 2 3)))

  #t)


(parametrise ((check-test-name	'part-complex))

  (define-method (mehve.real-part (o <vector>))
    (o map real-part))
  (define-method (mehve.real-part (o <spine>))
    (o map real-part))

  (define-method (mehve.imag-part (o <vector>))
    (o map imag-part))
  (define-method (mehve.imag-part (o <spine>))
    (o map imag-part))

  (define-method (mehve.magnitude (o <vector>))
    (o map magnitude))
  (define-method (mehve.magnitude (o <spine>))
    (o map magnitude))

  (define-method (mehve.angle (o <vector>))
    (o map angle))
  (define-method (mehve.angle (o <spine>))
    (o map angle))

  (define-method (mehve.make-rectangular (R <vector>) (I <vector>))
    (R map make-rectangular I))
  (define-method (mehve.make-rectangular (R <spine>) (I <spine>))
    (R map make-rectangular I))

  (define-method (mehve.make-polar (M <vector>) (A <vector>))
    (M map make-polar A))
  (define-method (mehve.make-polar (M <spine>) (A <spine>))
    (M map make-polar A))

;;; --------------------------------------------------------------------

;; (write (make-rectangular '(1 2 3) '(4 5 6)))(newline)
;; (write (make-polar '(1 2 3) '(4 5 6)))(newline)


  (check (real-part 1)			=> (rnrs.real-part 1))
  (check (real-part 1.1)		=> (rnrs.real-part 1.1))
  (check (real-part 2/3)		=> (rnrs.real-part 2/3))
  (check (real-part 1.1+2.2i)		=> (rnrs.real-part 1.1+2.2i))
  (check (real-part '(1 2 3))		=> (map rnrs.real-part '(1 2 3)))
  (check (real-part '#(1 2 3))		=> (vector-map rnrs.real-part '#(1 2 3)))

  (check (imag-part 1)			=> (rnrs.imag-part 1))
  (check (imag-part 1.1)		=> (rnrs.imag-part 1.1))
  (check (imag-part 2/3)		=> (rnrs.imag-part 2/3))
  (check (imag-part 1.1+2.2i)		=> (rnrs.imag-part 1.1+2.2i))
  (check (imag-part '(1 2 3))		=> (map rnrs.imag-part '(1 2 3)))
  (check (imag-part '#(1 2 3))		=> (vector-map rnrs.imag-part '#(1 2 3)))

  (check (magnitude 1)			=> (rnrs.magnitude 1))
  (check (magnitude 1.1)		=> (rnrs.magnitude 1.1))
  (check (magnitude 2/3)		=> (rnrs.magnitude 2/3))
  (check (magnitude 1.1+2.2i)		=> (rnrs.magnitude 1.1+2.2i))
  (check (magnitude '(1 2 3))		=> (map rnrs.magnitude '(1 2 3)))
  (check (magnitude '#(1 2 3))		=> (vector-map rnrs.magnitude '#(1 2 3)))

  (check (angle 1)			=> (rnrs.angle 1))
  (check (angle 1.1)			=> (rnrs.angle 1.1))
  (check (angle 2/3)			=> (rnrs.angle 2/3))
  (check (angle 1.1+2.2i)		=> (rnrs.angle 1.1+2.2i))
  (check (angle '(1 2 3))		=> (map rnrs.angle '(1 2 3)))
  (check (angle '#(1 2 3))		=> (vector-map rnrs.angle '#(1 2 3)))

  (check (make-rectangular 1 2)		=> (rnrs.make-rectangular 1 2))
  (check (make-rectangular 1.1 2.2)	=> (rnrs.make-rectangular 1.1 2.2))
  (check
      (make-rectangular '(1 2 3) '(4 5 6))
    => (map rnrs.make-rectangular '(1 2 3) '(4 5 6)))
  (check
      (make-rectangular '#(1 2 3) '#(4 5 6))
    => (vector-map rnrs.make-rectangular '#(1 2 3) '#(4 5 6)))

  (check (make-polar 1 2)		=> (rnrs.make-polar 1 2))
  (check (make-polar 1.1 2.2)		=> (rnrs.make-polar 1.1 2.2))
  (check
      (make-polar '(1 2 3) '(4 5 6))
    => (map rnrs.make-polar '(1 2 3) '(4 5 6)))
  (check
      (make-polar '#(1 2 3) '#(4 5 6))
    => (vector-map rnrs.make-polar '#(1 2 3) '#(4 5 6)))


  #t)


;;;; done

(check-report)

;;; end of file
