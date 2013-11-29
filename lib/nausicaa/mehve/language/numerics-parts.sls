;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: numerics parts functions for Mehve
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
(library (nausicaa mehve language numerics-parts)
  (export
    initialise-mehve-numerics-parts

    numerator denominator rationalize sign
    floor ceiling truncate round
    real-part imag-part magnitude angle
    make-rectangular make-polar complex-conjugate)
  (import (except (nausicaa)
		  numerator denominator rationalize sign
		  floor ceiling truncate round
		  real-part imag-part magnitude angle
		  make-rectangular make-polar complex-conjugate)
    (prefix (only (nausicaa)
		  numerator denominator rationalize sign
		  floor ceiling truncate round
		  real-part imag-part magnitude angle
		  make-rectangular make-polar complex-conjugate)
	    nau.)
    (vicare unsafe operations)
    (vicare system $numerics))


;;;; parts: rational numbers

(define-generic numerator	(x))
(define-generic denominator	(x))
(define-generic rationalize	(x y))
(define-generic sign		(x))

;;;; parts: rounding

(define-generic floor (x))
(define-generic ceiling (x))
(define-generic truncate (x))
(define-generic round (x))

;;;; parts: complex numbers

(define-generic real-part (x))
(define-generic imag-part (x))
(define-generic magnitude (x))
(define-generic angle (x))
(define-generic make-rectangular (R I))
(define-generic make-polar (M A))
(define-generic complex-conjugate	(x))

(define-method (imag-part (o <real>))
  0)

(define-method (angle (o <integer>))
  0)

;;According  to  R6RS:  all  the  reals except  Inf  and  NaN  are  also
;;rationals.
;;
#;(define-method (angle (o <rational>))
  0)

#;(define-method (angle (o <real>))
  0.0)


(define (initialise-mehve-numerics-parts)

  (add-method numerator		(<rational>)	nau.numerator)
  (add-method numerator		(<integer>)	values)
  (add-method denominator	(<rational>)	nau.denominator)
  (add-method denominator	(<integer>)	nau.denominator)
  (add-method rationalize	(<real> <real>)	nau.rationalize)

  (add-method sign		(<fixnum>)	$sign-fixnum)
  (add-method sign		(<real>)	$sign-flonum)
  (add-method sign		(<number>)	nau.sign)

;;;; rounding

  (add-method floor		(<flonum>)	$flfloor)
  (add-method floor		(<real>)	nau.floor)
  (add-method ceiling		(<flonum>)	$flceiling)
  (add-method ceiling		(<real>)	nau.ceiling)
  (add-method truncate		(<flonum>)	$fltruncate)
  (add-method truncate		(<real>)	nau.truncate)
  (add-method round		(<flonum>)	$flround)
  (add-method round		(<real>)	nau.round)

;;;; complex numbers

  (add-method real-part		(<real>)	values)
  (add-method real-part		(<complex>)	nau.real-part)
  (add-method imag-part		(<complex>)	nau.imag-part)
  (add-method magnitude		(<real>)	abs)
  (add-method magnitude		(<complex>)	nau.magnitude)
  (add-method angle		(<complex>)	nau.angle)

  (add-method make-rectangular	(<real> <real>)	nau.make-rectangular)
  (add-method make-polar	(<real> <real>)	nau.make-polar)

  (add-method complex-conjugate	(<real>)	values)
  (add-method complex-conjugate	(<complex>)	nau.complex-conjugate)

  #| end of initialisation function |# )


;;;; done

)

;;; end of file
