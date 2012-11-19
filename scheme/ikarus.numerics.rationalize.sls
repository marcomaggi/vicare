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


(library (ikarus rationalize)
  (export rationalize)
  (import (except (ikarus)
		  rationalize)
    (vicare syntactic-extensions))


(define who 'rationalize)

(define (rationalize x eps)
  (cond ((flonum? x)
	 (if (flfinite? x)
	     (cond ((flonum? eps)
		    (if (flfinite? eps)
			(%go x eps)
		      +0.0))
		   ((or (fixnum? eps)
			(bignum? eps)
			(ratnum? eps))
		    (%go x eps))
		   (else
		    (%error-not-a-number eps)))
	   (cond ((flonum? eps)
		  (if (flfinite? eps)
		      x
		    +nan.0))
		 ((or (fixnum? eps)
		      (bignum? eps)
		      (ratnum? eps))
		  x)
		 (else
		  (%error-not-a-number eps)))))

	((or (fixnum? x)
	     (bignum? x)
	     (ratnum? x))
	 (cond ((flonum? eps)
		(if (flfinite? eps)
		    (%go x eps)
		  +0.0))
	       ((or (fixnum? eps)
		    (bignum? eps)
		    (ratnum? eps))
		(%go x eps))
	       (else
		(%error-not-a-number eps))))

	(else
	 (%error-not-a-number eps))))

(define (%go x eps)
  (%simplest (- x eps) (+ x eps)))

(define (%simplest^ n d n^ d^)
  (receive (q r)
      (div-and-mod n d)
    (if (= r 0)
	q
      (receive (q^ r^)
	  (div-and-mod n^ d^)
	(if (= q q^)
	    (let* ((v   (%simplest^ d^ r^ d r))
		   (n^^ (numerator   v))
		   (d^^ (denominator v)))
	      (/ (+ (* q n^^) d^^) n^^))
	  (+ q 1))))))

(define (%simplest x y)
  (cond ((< y x)
	 (%simplest y x))
	((= x y)
	 x)
	((> x 0)
	 (let ((n  (numerator   x))
	       (d  (denominator x))
	       (n^ (numerator   y))
	       (d^ (denominator y)))
	   (%simplest^ n d n^ d^)))
	((< y 0)
	 (let ((n  (numerator   x))
	       (d  (denominator x))
	       (n^ (numerator   y))
	       (d^ (denominator y)))
	   (- (%simplest^ (- n^) d^ (- n) d))))
	(else
	 0)))

(define (%error-not-a-number obj)
  (assertion-violation who "expected number as argument" obj))


;;;; done

)

;;; end of file
