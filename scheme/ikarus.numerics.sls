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
	    $make-rectangular))


(define ($make-rectangular r i)
  ;; should be called with 2 exacts
  (if (eqv? i 0)
      r
    ($make-compnum r i)))

(define (make-rectangular r i)
  (define who 'make-rectangular)
  (define (err x)
    (die who "invalid argument" x))
  (cond
   ((flonum? i)
    (cond
     ((flonum? r) ($make-cflonum r i))
     ((or (fixnum? r) (bignum? r) (ratnum? r))
      ($make-cflonum (inexact r) i))
     (else (err r))))
   ((eqv? i 0) (if (number? r) r (err r)))
   ((or (fixnum? i) (bignum? i) (ratnum? i))
    (cond
     ((or (fixnum? r) (bignum? r) (ratnum? r))
      ($make-rectangular r i))
     ((flonum? r)
      ($make-cflonum r (inexact i)))
     (else (err r))))
   (else (err i))))

(define (make-polar mag angle)
  (define who 'make-polar)
  (unless (real? mag)
    (die who "not a real number" mag))
  (unless (real? angle)
    (die who "not a real number" angle))
  (make-rectangular
   (* mag (cos angle))
   (* mag (sin angle))))

(define (magnitude x)
  (cond
   ((or (fixnum? x) (bignum? x) (ratnum? x) (flonum? x))
    (abs x))
   ((compnum? x)
    (let ((r ($compnum-real x))
	  (i ($compnum-imag x)))
      (sqrt (+ (* r r) (* i i)))))
   ((cflonum? x)
    (let ((r ($cflonum-real x))
	  (i ($cflonum-imag x)))
      (sqrt (+ (* r r) (* i i)))))
   (else
    (die 'magnitude "not a number" x))))

(define (angle x)
  (import (ikarus system $bignums)
    (ikarus system $ratnums))
  (define PI (acos -1))
  (cond
   ((fixnum? x)
    (if (fx>? x 0)
	0
      (if (fx<? x 0)
	  PI
	(die 'angle "undefined for 0"))))
   ((bignum? x)
    (if ($bignum-positive? x) 0 PI))
   ((ratnum? x)
    (let ((n ($ratnum-n x)))
      (if (> n 0) 0 PI)))
   ((flonum? x)
    (atan 0.0 x))
   ((compnum? x)
    (let ((r ($compnum-real x))
	  (i ($compnum-imag x)))
      (atan i r)))
   ((cflonum? x)
    (let ((r ($cflonum-real x))
	  (i ($cflonum-imag x)))
      (atan i r)))
   (else
    (die 'angle "not a number" x))))

(define real-part
  (lambda (x)
    (cond
     ((fixnum? x) x)
     ((bignum? x) x)
     ((ratnum? x) x)
     ((flonum? x) x)
     ((compnum? x) ($compnum-real x))
     ((cflonum? x) ($cflonum-real x))
     (else
      (die 'real-part "not a number" x)))))

(define imag-part
  (lambda (x)
    (cond
     ((fixnum? x) 0)
     ((bignum? x) 0)
     ((ratnum? x) 0)
     ((flonum? x) 0)
     ((compnum? x) ($compnum-imag x))
     ((cflonum? x) ($cflonum-imag x))
     (else
      (die 'imag-part "not a number" x)))))


;;;; done

)

;;; end of file
