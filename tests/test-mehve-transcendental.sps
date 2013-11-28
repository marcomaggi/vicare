;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for mehve transcendental functions
;;;Date: Wed Jun 15, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (nausicaa mehve transcendental) mehve.)
  (prefix (rnrs) rnrs.)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing Mehve language, transcendental\n")


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


(parametrise ((check-test-name	'trigonometric))

  (define-method (mehve.sin (o <vector>))	(o.map sin))
  (define-method (mehve.sin (o <list>))	(o.map sin))

  (define-method (mehve.cos (o <vector>))	(o.map cos))
  (define-method (mehve.cos (o <list>))	(o.map cos))

  (define-method (mehve.tan (o <vector>))	(o.map tan))
  (define-method (mehve.tan (o <list>))	(o.map tan))

  (define-method (mehve.asin (o <vector>))	(o.map asin))
  (define-method (mehve.asin (o <list>))	(o.map asin))

  (define-method (mehve.acos (o <vector>))	(o.map acos))
  (define-method (mehve.acos (o <list>))	(o.map acos))

  (define-method (mehve.atan-1 (o <vector>))	(o.map atan))
  (define-method (mehve.atan-1 (o <list>))	(o.map atan))
  (define-method (mehve.atan-2 (x <vector>) (y <vector>))
    (x.map atan y))
  (define-method (mehve.atan-2 (x <list>) (y <list>))
    (x.map atan y))

;; (write (sin '(1.1 2.2 3.3)))(newline)
;; (write (cos '(1.1 2.2 3.3)))(newline)
;; (write (tan '(1.1 2.2 3.3)))(newline)

;;; --------------------------------------------------------------------

  (check (sin 1.1)			=> (rnrs.sin 1.1))
  (check (sin 1.1+2.2i)			=> (rnrs.sin 1.1+2.2i))
  (check (sin '(1.1 2.2 3.3))		=> (map rnrs.sin '(1.1 2.2 3.3)))
  (check (sin '#(1.1 2.2 3.3))		=> (vector-map rnrs.sin '#(1.1 2.2 3.3)))

  (check (cos 1.1)			=> (rnrs.cos 1.1))
  (check (cos 1.1+2.2i)			=> (rnrs.cos 1.1+2.2i))
  (check (cos '(1.1 2.2 3.3))		=> (map rnrs.cos '(1.1 2.2 3.3)))
  (check (cos '#(1.1 2.2 3.3))		=> (vector-map rnrs.cos '#(1.1 2.2 3.3)))

  (check (tan 1.1)			=> (rnrs.tan 1.1))
  (check (tan 1.1+2.2i)			=> (rnrs.tan 1.1+2.2i))
  (check (tan '(1.1 2.2 3.3))		=> (map rnrs.tan '(1.1 2.2 3.3)))
  (check (tan '#(1.1 2.2 3.3))		=> (vector-map rnrs.tan '#(1.1 2.2 3.3)))

  (check (asin 1.1)			=> (rnrs.asin 1.1))
  (check (asin 1.1+2.2i)		=> (rnrs.asin 1.1+2.2i))
  (check (asin '(1.1 2.2 3.3))		=> (map rnrs.asin '(1.1 2.2 3.3)))
  (check (asin '#(1.1 2.2 3.3))		=> (vector-map rnrs.asin '#(1.1 2.2 3.3)))

  (check (acos 1.1)			=> (rnrs.acos 1.1))
  (check (acos 1.1+2.2i)		=> (rnrs.acos 1.1+2.2i))
  (check (acos '(1.1 2.2 3.3))		=> (map rnrs.acos '(1.1 2.2 3.3)))
  (check (acos '#(1.1 2.2 3.3))		=> (vector-map rnrs.acos '#(1.1 2.2 3.3)))

  (check (atan 1.1)			=> (rnrs.atan 1.1))
  (check (atan 1.1+2.2i)		=> (rnrs.atan 1.1+2.2i))
  (check (atan '(1.1 2.2 3.3))		=> (map rnrs.atan '(1.1 2.2 3.3)))
  (check (atan '#(1.1 2.2 3.3))		=> (vector-map rnrs.atan '#(1.1 2.2 3.3)))

  (check (atan 1.1 2.2)			=> (rnrs.atan 1.1 2.2))
  (check (atan '(1.1 2.2 3.3)
  	       '(4.4 5.5 6.6))		=> (map rnrs.atan
  					     '(1.1 2.2 3.3)
  					     '(4.4 5.5 6.6)))
  (check (atan '#(1.1 2.2 3.3)
  	       '#(4.4 5.5 6.6))		=> (vector-map rnrs.atan
  						       '#(1.1 2.2 3.3)
  						       '#(4.4 5.5 6.6)))

  #t)


(parametrise ((check-test-name	'exponentiation))

  (define-method (mehve.expt (A <vector>) (B <vector>))
    (A.map expt B))
  (define-method (mehve.expt (A <list>) (B <list>))
    (A.map expt B))

  (define-method (mehve.sqrt (A <vector>))
    (A.map sqrt))
  (define-method (mehve.sqrt (A <list>))
    (A.map sqrt))

  (define-method (mehve.exp (o <vector>))
    (o.map exp))
  (define-method (mehve.exp (o <list>))
    (o.map exp))

  (define-method (mehve.log-1 (o <vector>))
    (o.map log))
  (define-method (mehve.log-1 (o <list>))
    (o.map log))

  (define-method (mehve.log-2 (o <vector>) (base <complex>))
    (vector-map (lambda (x)
		  (log x base))
		o))
  (define-method (mehve.log-2 (o <list>) (base <complex>))
    (map (lambda (x)
	   (log x base))
      o))

;;; --------------------------------------------------------------------

  (check (expt 2 3)			=> (rnrs.expt 2 3))
  (check (expt 2.1 3.1)			=> (rnrs.flexpt 2.1 3.1))
  (check (expt 1.1+2.2i 3.3+4.4i)	=> (rnrs.expt 1.1+2.2i 3.3+4.4i))
  (check
      (expt '(1 2 3) '(4 5 6))
    => (map rnrs.expt '(1 2 3) '(4 5 6)))
  (check
      (expt '#(1 2 3) '#(4 5 6))
    => (vector-map rnrs.expt '#(1 2 3) '#(4 5 6)))

;;; --------------------------------------------------------------------

  (check (exp 2)			=> (rnrs.exp 2))
  (check (exp 2.1)			=> (rnrs.flexp 2.1))
  (check (exp 1.1+2.2i)			=> (rnrs.exp 1.1+2.2i))
  (check
      (exp '(1 2 3))
    => (map rnrs.exp '(1 2 3)))
  (check
      (exp '#(1 2 3))
    => (vector-map rnrs.exp '#(1 2 3)))

;;; --------------------------------------------------------------------

  (check (sqrt 2)			=> (rnrs.sqrt 2))
  (check (sqrt 2.1)			=> (rnrs.flsqrt 2.1))
  (check (sqrt 1.1+2.2i)		=> (rnrs.sqrt 1.1+2.2i))
  (check
      (sqrt '(1 2 3))
    => (map rnrs.sqrt '(1 2 3)))
  (check
      (sqrt '#(1 2 3))
    => (vector-map rnrs.sqrt '#(1 2 3)))

;;; --------------------------------------------------------------------

  (check (log 2)			=> (rnrs.log 2))
  (check (log 2.1)			=> (rnrs.fllog 2.1))
  (check (log 1.1+2.2i)			=> (rnrs.log 1.1+2.2i))
  (check
      (log '(1 2 3))
    => (map rnrs.log '(1 2 3)))
  (check
      (log '#(1 2 3))
    => (vector-map rnrs.log '#(1 2 3)))

;;; --------------------------------------------------------------------

  (check (log 2 3)			=> (rnrs.log 2 3))
  (check (log 2.1 3.1)			=> (rnrs.fllog 2.1 3.1))
  (check (log 1.1+2.2i 3.3+4.4i)	=> (rnrs.log 1.1+2.2i 3.3+4.4i))
  (check
      (log '(1 2 3) 4)
    => (map rnrs.log '(1 2 3) '(4 4 4)))
  (check
      (log '#(1 2 3) 4)
    => (vector-map rnrs.log '#(1 2 3) '#(4 4 4)))

  #t)


;;;; done

(check-report)

;;; end of file
