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

(define-generic floor		(x))
(define-generic ceiling		(x))
(define-generic truncate	(x))
(define-generic round		(x))

;;;; parts: complex numbers

(define-generic real-part	(x))
(define-generic imag-part	(x))
(define-generic magnitude	(x))
(define-generic angle		(x))
(define-generic make-rectangular (R I))
(define-generic make-polar	(M A))
(define-generic complex-conjugate (x))

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

  (add-method numerator		(<fixnum>)		values)
  (add-method numerator		(<bignum>)		values)
  (add-method numerator		(<ratnum>)		nau.numerator)
  (add-method numerator		(<flonum>)		nau.numerator)
  (add-method numerator		(<integer>)		values)
  (add-method numerator		(<real>)		nau.numerator)

  (add-method denominator	(<fixnum>)		(lambda (A) 1))
  (add-method denominator	(<bignum>)		(lambda (A) 1))
  (add-method denominator	(<ratnum>)		nau.denominator)
  (add-method denominator	(<flonum>)		nau.denominator)
  (add-method denominator	(<integer>)		nau.denominator)
  (add-method denominator	(<real>)		nau.denominator)

  (add-method rationalize	(<real> <real>)		nau.rationalize)

  (add-method sign		(<fixnum>)		$sign-fixnum)
  (add-method sign		(<flonum>)		$sign-flonum)
  (add-method sign		(<bignum>)		$sign-bignum)
  (add-method sign		(<ratnum>)		$sign-ratnum)
  (add-method sign		(<real>)		nau.sign)

;;;; rounding

  (add-method floor		(<fixnum>)		values)
  (add-method floor		(<bignum>)		values)
  (add-method floor		(<flonum>)		$flfloor)
  (add-method floor		(<real>)		nau.floor)
  (add-method ceiling		(<fixnum>)		values)
  (add-method ceiling		(<bignum>)		values)
  (add-method ceiling		(<flonum>)		$flceiling)
  (add-method ceiling		(<real>)		nau.ceiling)
  (add-method truncate		(<fixnum>)		values)
  (add-method truncate		(<bignum>)		values)
  (add-method truncate		(<flonum>)		$fltruncate)
  (add-method truncate		(<real>)		nau.truncate)
  (add-method round		(<fixnum>)		values)
  (add-method round		(<bignum>)		values)
  (add-method round		(<flonum>)		$flround)
  (add-method round		(<real>)		nau.round)

;;;; complex numbers

  (add-method real-part		(<fixnum>)		values)
  (add-method real-part		(<bignum>)		values)
  (add-method real-part		(<flonum>)		values)
  (add-method real-part		(<ratnum>)		values)
  (add-method real-part		(<compnum>)		(lambda (Z) ($compnum-real Z)))
  (add-method real-part		(<cflonum>)		(lambda (Z) ($cflonum-real Z)))
  (add-method real-part		(<complex>)		nau.real-part)

  (add-method imag-part		(<fixnum>)		(lambda (X) 0))
  (add-method imag-part		(<bignum>)		(lambda (X) 0))
  (add-method imag-part		(<flonum>)		(lambda (X) 0))
  (add-method imag-part		(<ratnum>)		(lambda (X) 0))
  (add-method imag-part		(<compnum>)		(lambda (Z) ($compnum-imag Z)))
  (add-method imag-part		(<cflonum>)		(lambda (Z) ($cflonum-imag Z)))
  (add-method imag-part		(<complex>)		nau.imag-part)

  (add-method magnitude		(<fixnum>)		$magnitude-fixnum)
  (add-method magnitude		(<bignum>)		$magnitude-bignum)
  (add-method magnitude		(<flonum>)		$magnitude-flonum)
  (add-method magnitude		(<ratnum>)		$magnitude-ratnum)
  (add-method magnitude		(<cflonum>)		$magnitude-cflonum)
  (add-method magnitude		(<compnum>)		$magnitude-compnum)
  (add-method magnitude		(<complex>)		nau.magnitude)

  (add-method angle		(<fixnum>)		$angle-fixnum)
  (add-method angle		(<bignum>)		$angle-bignum)
  (add-method angle		(<flonum>)		$angle-flonum)
  (add-method angle		(<ratnum>)		$angle-ratnum)
  (add-method angle		(<cflonum>)		$angle-cflonum)
  (add-method angle		(<compnum>)		$angle-compnum)
  (add-method angle		(<complex>)		nau.angle)

  (add-method make-rectangular	(<real> <real>)		nau.make-rectangular)
  (add-method make-polar	(<real> <real>)		nau.make-polar)

  (add-method complex-conjugate	(<fixnum>)		values)
  (add-method complex-conjugate	(<bignum>)		values)
  (add-method complex-conjugate	(<ratnum>)		values)
  (add-method complex-conjugate	(<flonum>)		values)
  (add-method complex-conjugate	(<real>)		values)
  (add-method complex-conjugate	(<compnum>)		$complex-conjugate-compnum)
  (add-method complex-conjugate	(<cflonum>)		$complex-conjugate-cflonum)
  (add-method complex-conjugate	(<complex>)		nau.complex-conjugate)

  #| end of initialisation function |# )


;;;; done

)

;;; end of file
