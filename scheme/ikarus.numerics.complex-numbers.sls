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
    magnitude			angle
    complex-conjugate

;;; --------------------------------------------------------------------

    $magnitude-fixnum		$magnitude-bignum	$magnitude-ratnum
    $magnitude-flonum		$magnitude-compnum	$magnitude-cflonum

    $angle-fixnum		$angle-bignum		$angle-ratnum
    $angle-flonum		$angle-compnum		$angle-cflonum

    $complex-conjugate-compnum	$complex-conjugate-cflonum

    $make-rectangular)
  (import (except (vicare)
		  make-rectangular	make-polar
		  real-part		imag-part
		  angle			magnitude
		  complex-conjugate)
    (only (vicare system $compnums)
	  $make-compnum		$make-cflonum
	  $compnum-real		$compnum-imag
	  $cflonum-real		$cflonum-imag)
    (vicare system $fx)
    (vicare system $bignums)
    (vicare system $ratnums)
    (vicare system $flonums)
    (rename (only (ikarus generic-arithmetic)
		  $abs-fixnum
		  $abs-bignum
		  $abs-ratnum
		  $abs-flonum
		  $atan2-real-real)
	    ($abs-fixnum		$magnitude-fixnum)
	    ($abs-bignum		$magnitude-bignum)
	    ($abs-ratnum		$magnitude-ratnum)
	    ($abs-flonum		$magnitude-flonum))
    (vicare arguments validation)
    (only (vicare language-extensions syntaxes)
	  cond-numeric-operand))


;;;; helpers

;;From Wikipedia.
(define greek-pi	3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679)
(define greek-pi/2	(/ greek-pi 2.0))


;;;; constructors

(define* (make-rectangular {rep real?} {imp real?})
  ($make-rectangular rep imp))

(define ($make-rectangular rep imp)
  ;;REP and IMP can be any combination of real numbers.  If IMP is exact
  ;;zero: the returned value is REP, a real.
  ;;
  (cond ((eq? imp 0)
	 rep)
	((and (flonum? rep)
	      (flonum? imp))
	 ($make-cflonum rep imp))
	(else
	 ($make-compnum rep imp))))

(define* (make-polar {mag real?} {angle real?})
  ($make-rectangular (* mag (cos angle))
		     (* mag (sin angle))))


(module (magnitude
	 $magnitude-compnum
	 $magnitude-cflonum)

  (define* (magnitude x)
    (cond-numeric-operand x
      ((compnum?)	($magnitude-compnum x))
      ((cflonum?)	($magnitude-cflonum x))
      ((fixnum?)	($magnitude-fixnum  x))
      ((bignum?)	($magnitude-bignum  x))
      ((ratnum?)	($magnitude-ratnum  x))
      ((flonum?)	($magnitude-flonum  x))
      (else
       (procedure-argument-violation __who__ "expected number object as argument" x))))

  (define ($magnitude-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      (sqrt (+ (square x.rep) (square x.imp)))))

  (define ($magnitude-cflonum x)
    ($flhypot ($cflonum-real x)
	      ($cflonum-imag x))
    ;; ($flsqrt ($fl+ ($flsquare ($cflonum-real x))
    ;; 		   ($flsquare ($cflonum-imag x))))
    )

  #| end of module: magnitude |# )


(module (angle
	 $angle-fixnum		$angle-bignum		$angle-ratnum
	 $angle-flonum		$angle-compnum		$angle-cflonum)

  (define* (angle Z)
    (cond-numeric-operand Z
      ((compnum?)	($angle-compnum Z))
      ((cflonum?)	($angle-cflonum Z))
      ((fixnum?)	($angle-fixnum  Z))
      ((bignum?)	($angle-bignum  Z))
      ((ratnum?)	($angle-ratnum  Z))
      ((flonum?)	($angle-flonum  Z))
      (else
       (procedure-argument-violation __who__ "expected number object as argument" Z))))

  (define* ($angle-fixnum Z)
    (cond (($fxpositive? Z)	0)
	  (($fxnegative? Z)	greek-pi)
	  (else
	   (assertion-violation __who__ "undefined for 0"))))

  (define ($angle-bignum Z)
    (if ($bignum-positive? Z) 0 greek-pi))

  (define ($angle-ratnum Z)
    (let ((n ($ratnum-n Z)))
      (if (> n 0) 0 greek-pi)))

  (define ($angle-flonum Z)
    (if (or ($flpositive?      Z)
	    ($flzero?/positive Z))
	0.0
      greek-pi))

  (define ($angle-compnum Z)
    (let ((Z.rep ($compnum-real Z))
	  (Z.imp ($compnum-imag Z)))
      (atan Z.imp Z.rep)))

  (define ($angle-cflonum Z)
    (let ((Z.rep ($cflonum-real Z))
	  (Z.imp ($cflonum-imag Z)))
      ($atan2-real-real Z.imp Z.rep)))

  #| end of module: angle |# )


(define* (real-part x)
  (cond-numeric-operand x
    ((compnum?)	($compnum-real x))
    ((cflonum?)	($cflonum-real x))
    ((fixnum?)	x)
    ((bignum?)	x)
    ((ratnum?)	x)
    ((flonum?)	x)
    (else
     (procedure-argument-violation __who__ "expected number object as argument" x))))

(define* (imag-part x)
  (cond-numeric-operand x
    ((fixnum?)	0)
    ((bignum?)	0)
    ((ratnum?)	0)
    ((flonum?)	0)
    ((compnum?)	($compnum-imag x))
    ((cflonum?)	($cflonum-imag x))
    (else
     (procedure-argument-violation __who__ "expected number object as argument" x))))


(module (complex-conjugate
	 $complex-conjugate-compnum	$complex-conjugate-cflonum)

  (define* (complex-conjugate Z)
    (cond-numeric-operand Z
      ((compnum?)	($complex-conjugate-compnum Z))
      ((cflonum?)	($complex-conjugate-cflonum Z))
      ((fixnum?)	Z)
      ((bignum?)	Z)
      ((ratnum?)	Z)
      ((flonum?)	Z)
      (else
       (procedure-argument-violation __who__ "expected number object as argument" Z))))

  (define ($complex-conjugate-compnum Z)
    (let ((Z.rep ($compnum-real Z))
	  (Z.imp ($compnum-imag Z)))
      ($make-rectangular Z.rep (- Z.imp))))

  (define ($complex-conjugate-cflonum Z)
    (let ((Z.rep ($cflonum-real Z))
	  (Z.imp ($cflonum-imag Z)))
      ($make-cflonum Z.rep ($fl- Z.imp))))

  #| end of module: complex-conjugate |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'cond-numeric-operand 'scheme-indent-function 1)
;; End:
