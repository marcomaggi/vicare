;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus numerics complex-numbers)
  (export
    make-rectangular		make-polar
    real-part			imag-part
    angle			magnitude
    $make-rectangular)
  (import (except (ikarus)
		  make-rectangular	make-polar
		  real-part		imag-part
		  angle			magnitude)
    (except (ikarus system $compnums)
	    $make-rectangular)
    (vicare arguments validation)
    (only (vicare syntactic-extensions)
	  cond-numeric-operand))


(define ($make-rectangular r i)
  ;; should be called with 2 exacts
  (if (eqv? i 0)
      r
    ($make-compnum r i)))


(module (make-rectangular)

  (define who 'make-rectangular)

  (define (make-rectangular rep imp)
    (cond-numeric-operand imp
      ((flonum?)
       (cond-numeric-operand rep
	 ((flonum?)		($make-cflonum rep imp))
	 ((real? exact?)	($make-compnum rep imp))
	 (else			(%error-real rep))))
      ((zero?)
       rep)
      ((real? exact?)
       (cond-numeric-operand rep
	 ((flonum?)		($make-compnum rep imp))
	 ((real? exact?)	($make-compnum rep imp))
	 (else			(%error-real rep))))
      (else
       (%error-imag imp))))

  (define (%error-real x)
    (assertion-violation who "invalid real part argument" x))

  (define (%error-imag x)
    (assertion-violation who "invalid imag part argument" x))

  #| end of module |# )


(define (make-polar mag angle)
  (define who 'make-polar)
  (with-arguments-validation (who)
      ((real	mag)
       (real	angle))
    (make-rectangular (* mag (cos angle))
		      (* mag (sin angle)))))

(define (magnitude x)
  (define who 'magnitude)
  (cond-numeric-operand x
    ((real?)
     (abs x))
    ((compnum?)
     (let ((r ($compnum-real x))
	   (i ($compnum-imag x)))
       (sqrt (+ (* r r) (* i i)))))
    ((cflonum?)
     (let ((r ($cflonum-real x))
	   (i ($cflonum-imag x)))
       (sqrt (+ (* r r) (* i i)))))
    (else
     (assertion-violation who "expected number as argument" x))))

(define (angle x)
  (import (ikarus system $fx)
    (ikarus system $bignums)
    (ikarus system $ratnums))
  (define who 'angle)
  (define PI  (acos -1))
  (cond-numeric-operand x
    ((fixnum?)
     (cond (($fx> x 0)	0)
	   (($fx< x 0)	PI)
	   (else
	    (assertion-violation who "undefined for 0"))))
    ((bignum?)
     (if ($bignum-positive? x) 0 PI))
    ((ratnum?)
     (let ((n ($ratnum-n x)))
       (if (> n 0) 0 PI)))
    ((flonum?)
     (atan 0.0 x))
    ((compnum?)
     (let ((r ($compnum-real x))
	   (i ($compnum-imag x)))
       (atan i r)))
    ((cflonum?)
     (let ((r ($cflonum-real x))
	   (i ($cflonum-imag x)))
       (atan i r)))
    (else
     (assertion-violation who "expected number as argument" x))))

(define (real-part x)
  (define who 'real-part)
  (cond-numeric-operand x
    ((fixnum?)	0)
    ((bignum?)	0)
    ((ratnum?)	0)
    ((flonum?)	0)
    ((compnum?)	($compnum-real x))
    ((cflonum?)	($cflonum-real x))
    (else
     (assertion-violation who "expected number as argument" x))))

(define (imag-part x)
  (define who 'imag-part)
  (cond-numeric-operand x
    ((fixnum?)	0)
    ((bignum?)	0)
    ((ratnum?)	0)
    ((flonum?)	0)
    ((compnum?)	($compnum-imag x))
    ((cflonum?)	($cflonum-imag x))
    (else
     (assertion-violation who "expected number as argument" x))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'cond-numeric-operand 'scheme-indent-function 1)
;; End:
