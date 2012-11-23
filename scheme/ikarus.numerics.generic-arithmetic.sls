;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Implementation of BITWISE-REVERSE-BIT-FIELD from:
;;;
;;;  Ypsilon Scheme System
;;;  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;;  See below for terms and conditions of use of this function.
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

;;;License notice for BITWISE-REVERSE-BIT-FIELD
;;;
;;;Copyright (c) 2004-2009 Yoshikatsu Fujita. All rights reserved.
;;;Copyright (c) 2004-2009 LittleWing Company Limited. All rights reserved.
;;;
;;;Redistribution and  use in source  and binary forms, with  or without
;;;modification,  are permitted provided  that the  following conditions
;;;are met:
;;;
;;;1.  Redistributions  of source code  must retain the  above copyright
;;;notice, this list of conditions and the following disclaimer.
;;;
;;;2. Redistributions in binary  form must reproduce the above copyright
;;;notice, this list  of conditions and the following  disclaimer in the
;;;documentation and/or other materials provided with the distribution.
;;;
;;;3. Neither the name of the  authors nor the names of its contributors
;;;may be used to endorse or promote products derived from this software
;;;without specific prior written permission.
;;;
;;;THIS SOFTWARE  IS PROVIDED BY THE COPYRIGHT  HOLDERS AND CONTRIBUTORS
;;;"AS IS"  AND ANY  EXPRESS OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
;;;LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;SPECIAL,  EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES  (INCLUDING, BUT  NOT
;;;LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;THEORY OF  LIABILITY, WHETHER IN CONTRACT, STRICT  LIABILITY, OR TORT
;;;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;


(library (ikarus generic-arithmetic)
  (export
    + - * /
    = < <= > >=
    min				max
    add1			sub1
    quotient			remainder
    quotient+remainder		modulo
    zero?
    positive?			negative?
    even?			odd?

    ;; exactness
    exact->inexact		inexact

    ;; part functions
    abs
    floor			ceiling
    round			truncate
    numerator			denominator
    gcd				lcm

    ;; bitwise operations
    bitwise-and			bitwise-not
    bitwise-ior			bitwise-xor
    bitwise-if
    bitwise-arithmetic-shift-right
    bitwise-arithmetic-shift-left
    bitwise-arithmetic-shift
    bitwise-length		bitwise-copy-bit-field
    bitwise-copy-bit		bitwise-bit-field
    bitwise-reverse-bit-field	bitwise-rotate-bit-field
    sra				sll

    ;; powers and square roots
    expt			sqr
    sqrt			exact-integer-sqrt

    ;; logarithms and exponentials
    log				exp

    ;; trigonometric functions
    sin				asin
    cos				acos
    tan				atan

    ;; hyperbolic functions
    sinh			asinh
    cosh			acosh
    tanh			atanh

    ;; other functions
    number->string		real->flonum
    random
    error@add1			error@sub1
    bytevector->bignum		bignum->bytevector

    ;; the following go in (ikarus system $numerics)
    $sqrt-fixnum
    $sqrt-flonum
    $sqrt-bignum
    $sqrt-ratnum
    $sqrt-compnum
    $sqrt-cflonum

    $exact-integer-sqrt/fixnum
    $exact-integer-sqrt/bignum)
  (import (except (ikarus)
		  + - * / = < <= > >=
		  min				max
		  add1				sub1
		  quotient			remainder
		  quotient+remainder		modulo
		  zero?
		  positive?			negative?
		  even?				odd?

		  ;; exactness
		  exact->inexact		inexact

		  ;; part functions
		  abs
		  floor				ceiling
		  round				truncate
		  numerator			denominator
		  gcd				lcm

		  ;; bitwise operations
		  bitwise-and			bitwise-not
		  bitwise-ior			bitwise-xor
		  bitwise-if
		  bitwise-arithmetic-shift-right
		  bitwise-arithmetic-shift-left
		  bitwise-arithmetic-shift
		  bitwise-length		bitwise-copy-bit-field
		  bitwise-copy-bit		bitwise-bit-field
		  bitwise-reverse-bit-field	bitwise-rotate-bit-field
		  sra				sll

		  ;; powers and square roots
		  expt				sqr
		  sqrt				exact-integer-sqrt

		  ;; logarithms and exponentials
		  log exp

		  ;; trigonometric functions
		  sin				asin
		  cos				acos
		  tan				atan

		  ;; hyperbolic functions
		  sinh				asinh
		  cosh				acosh
		  tanh				atanh

		  ;; other functions
		  number->string		real->flonum
		  random
		  error@add1			error@sub1
		  bytevector->bignum		bignum->bytevector)
    (ikarus system $pairs)
    (except (ikarus system $fx)
	    $fxpositive?	$fxnegative?
	    $fxeven?		$fxodd?
	    $fxabs
	    $fxmodulo		$fxremainder)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Nov 18, 2012)
    (only (ikarus fixnums)
	  $fxpositive?		$fxnegative?
	  $fxeven?		$fxodd?
	  $fxabs
	  $fxmodulo		$fxremainder)
    (except (ikarus system $flonums)
	    $flonum->exact
	    $flzero?		$flzero?/negative
	    $flpositive?	$flnegative?
	    $fleven?		$flodd?
	    $flsqr		$flsqrt
	    $flnumerator	$fldenominator
	    $flround
	    $fllog		$flexp
	    $flsin		$flasin
	    $flcos		$flacos
	    $fltan		$flatan
	    $flsinh		$flasinh
	    $flcosh		$flacosh
	    $fltanh		$flatanh
	    $flatan2)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Nov 17, 2012)
    (only (ikarus flonums)
	  $flonum->exact
	  $flzero?		$flzero?/negative
	  $flpositive?		$flnegative?
	  $fleven?		$flodd?
	  $flsqr		$flsqrt
	  $flnumerator		$fldenominator
          $flround
	  $fllog		$flexp
	  $flsin		$flasin
	  $flcos		$flacos
	  $fltan		$flatan
	  $flsinh		$flasinh
	  $flcosh		$flacosh
	  $fltanh		$flatanh
	  $flatan2)
    (except (ikarus system $ratnums)
	    $ratnum->flonum)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Nov 21, 2012)
    (only (ikarus ratnums)
	  $ratnum->flonum)
    (except (ikarus system $bignums)
	    $bignum-positive?	$bignum-negative?
	    $bignum-even?	$bignum-odd?
	    $bignum->flonum)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Nov 21, 2012)
    (only (ikarus bignums)
	  $bignum-positive?	$bignum-negative?
	  $bignum-even?		$bignum-odd?
	  $bignum->flonum)
    (ikarus system $compnums)
    (ikarus system $chars)
    (ikarus system $strings)
    (vicare arguments validation)
    (vicare syntactic-extensions))


;;;; helpers

(define (err who x)
  (die who (if (number? x) "invalid argument" "not a number") x))

(module (PI PI/2)
  (import (ikarus))
  (define PI (acos -1))
  (define PI/2 (/ PI 2)))


(module (real->flonum)
  (define who 'real->flonum)

  (define (real->flonum x)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum->flonum x))
      ((bignum?)	($bignum->flonum x))
      ((ratnum?)	($ratnum->flonum x))
      ((flonum?)	x)
      ((compnum?)	(%error-not-real x))
      ((cflonum?)	(%error-not-real x))
      (else
       (%error-not-real x))))

  (define (%error-not-real x)
    (assertion-violation who "expected real number as argument" x))

  #| end of module: real->flonum |# )


;;;; generic arithmetic functions

(define +
  (case-lambda
   ((x y)
    (binary+ x y))
   ((x y z)
    (binary+ (binary+ x y) z))
   ((a)
    (cond ((fixnum? a)
	   a)
	  ((number? a)
	   a)
	  (else
	   (assertion-violation '+ "not a number" a))))
   (()
    0)
   ((a b c d . e*)
    (let loop ((ac (binary+ (binary+ (binary+ a b) c) d))
	       (e* e*))
      (if (null? e*)
	  ac
	(loop (binary+ ac ($car e*)) ($cdr e*)))))))

(define -
  (case-lambda
   ((x y)
    (binary- x y))
   ((x y z)
    (binary- (binary- x y) z))
   ((a)
    (binary- 0 a))
   ((a b c d . e*)
    (let loop ((ac (binary- (binary- (binary- a b) c) d))
	       (e* e*))
      (if (null? e*)
	  ac
	(loop (binary- ac ($car e*)) ($cdr e*)))))))

(define *
  (case-lambda
   ((x y)
    (binary* x y))
   ((x y z)
    (binary* (binary* x y) z))
   ((a)
    (cond ((fixnum? a) a)
	  ((number? a) a)
	  (else
	   (assertion-violation '* "not a number" a))))
   (()
    1)
   ((a b c d . e*)
    (let loop ((ac (binary* (binary* (binary* a b) c) d))
	       (e* e*))
      (if (null? e*)
	  ac
	(loop (binary* ac ($car e*)) ($cdr e*)))))))

(define /
  (case-lambda
   ((x y)
    (binary/ x y))
   ((x)
    (unary/ x))
   ((x y z . ls)
    (let loop ((a  (binary/ x y))
	       (b  z)
	       (ls ls))
      (if (null? ls)
	  (binary/ a b)
	(loop (binary/ a b) ($car ls) ($cdr ls)))))
   ))


(module (unary-
	 $fixnum-	$bignum-	$flonum-
	 $ratnum-	$compnum-	$cflonum-)
  (define who (quote -))

  (define (unary- x)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum-  x))
      ((bignum?)	($bignum-  x))
      ((flonum?)	($flonum-  x))
      ((ratnum?)	($ratnum-  x))
      ((compnum?)	($compnum- x))
      ((cflonum?)	($cflonum- x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($fixnum- x)
    (if ($fx= x (least-fixnum))
	(- (least-fixnum))
      ($fx- x)))

  (define ($bignum- x)
    ($fixnum-bignum 0 x))

  (define ($flonum- x)
    ($fl- 0.0 x))

  (define ($ratnum- x)
    ($fixnum-ratnum 0 x))

  (define ($compnum- x)
    ($fixnum-compnum 0 x))

  (define ($cflonum- x)
    ($flonum-cflonum 0.0 x))

  #| end of module: unary- |# )


(module (unary/
	 $fixnum/	$bignum/	$flonum/
	 $ratnum/	$compnum/	$cflonum/)
  (define who (quote /))

  (define (unary/ x)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum/  x))
      ((bignum?)	($bignum/  x))
      ((flonum?)	($flonum/  x))
      ((ratnum?)	($ratnum/  x))
      ((compnum?)	($compnum/ x))
      ((cflonum?)	($cflonum/ x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($fixnum/ x)
    (cond (($fxzero? x)
	   (assertion-violation who "unary division by 0"))
	  (($fx> x 0)
	   (if ($fx= x 1)
	       1
	     ($make-ratnum 1 x)))
	  (else
	   (if ($fx= x -1)
	       -1
	     ($make-ratnum -1 ($fixnum- x))))))

  (define ($bignum/ x)
    (if ($bignum-positive? x)
	($make-ratnum 1 x)
      ($make-ratnum -1 (- x))))

  (define ($flonum/ x)
    (foreign-call "ikrt_fl_invert" x))

  (define ($ratnum/ x)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (cond (($fx= x.num +1) ;this works also when X.NUM is not a fixnum
	     x.den)
	    (($fx= x.den -1) ;this works also when X.NUM is not a fixnum
	     (- x.den))
	    (else
	     (if (> 0 x.num)
		 ($make-ratnum (- x.den) (- x.num))
	       ($make-ratnum x.den x.num))))))

  (define ($compnum/ x)
    ($fixnum/compnum 1 x))

  (define ($cflonum/ x)
    ($fixnum/cflonum 1 x))

  #| end of module: unary/ |# )


(module (binary+
	 $fixnum+number		$bignum+number		$flonum+number
	 $ratnum+number		$compnum+number		$cflonum+number
	 $number+fixnum		$number+bignum		$number+flonum
	 $number+ratnum		$number+compnum		$number+cflonum
	 $fixnum+fixnum		$fixnum+bignum		$fixnum+flonum
	 $fixnum+ratnum		$fixnum+compnum		$fixnum+cflonum
	 $bignum+fixnum		$bignum+bignum		$bignum+flonum
	 $bignum+ratnum		$bignum+compnum		$bignum+cflonum
	 $flonum+fixnum		$flonum+bignum		$flonum+flonum
	 $flonum+ratnum		$flonum+compnum		$flonum+cflonum
	 $ratnum+fixnum		$ratnum+bignum		$ratnum+flonum
	 $ratnum+ratnum		$ratnum+compnum		$ratnum+cflonum
	 $compnum+fixnum	$compnum+bignum		$compnum+ratnum
	 $compnum+compnum	$compnum+flonum		$compnum+cflonum
	 $cflonum+fixnum	$cflonum+bignum		$cflonum+ratnum
	 $cflonum+flonum	$cflonum+compnum	$cflonum+cflonum)
  (define who (quote +))

  (define (binary+ x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum+number  x y))
      ((bignum?)	($bignum+number  x y))
      ((flonum?)	($flonum+number  x y))
      ((ratnum?)	($ratnum+number  x y))
      ((compnum?)	($compnum+number x y))
      ((cflonum?)	($cflonum+number x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($fixnum+number x y)
    (cond-numeric-operand y
     ((fixnum?)		($fixnum+fixnum  x y))
     ((bignum?)		($fixnum+bignum  x y))
     ((ratnum?)		($fixnum+ratnum  x y))
     ((flonum?)		($fixnum+flonum  x y))
     ((compnum?)	($fixnum+compnum x y))
     ((cflonum?)	($fixnum+cflonum x y))
     (else
      (err who y))))

  (define ($bignum+number x y)
    (cond-numeric-operand y
      ((fixnum?)	($bignum+fixnum  x y))
      ((bignum?)	($bignum+bignum  x y))
      ((ratnum?)	($bignum+ratnum  x y))
      ((flonum?)	($bignum+flonum  x y))
      ((compnum?)	($bignum+compnum x y))
      ((cflonum?)	($bignum+cflonum x y))
      (else
       (err who y))))

  (define ($flonum+number x y)
    (cond-numeric-operand y
     ((fixnum?)		($flonum+fixnum  x y))
     ((bignum?)		($flonum+bignum  x y))
     ((ratnum?)		($flonum+ratnum  x y))
     ((flonum?)		($flonum+flonum  x y))
     ((compnum?)	($flonum+compnum x y))
     ((cflonum?)	($flonum+cflonum x y))
     (else
      (err who y))))

  (define ($ratnum+number x y)
    (cond-numeric-operand y
      ((fixnum?)	($ratnum+fixnum  x y))
      ((bignum?)	($ratnum+bignum  x y))
      ((flonum?)	($ratnum+flonum  x y))
      ((ratnum?)	($ratnum+ratnum  x y))
      ((compnum?)	($ratnum+compnum x y))
      ((cflonum?)	($ratnum+cflonum x y))
      (else
       (err who y))))

  (define ($compnum+number x y)
    (cond-numeric-operand y
      ((fixnum?)	($compnum+fixnum  x y))
      ((bignum?)	($compnum+bignum  x y))
      ((ratnum?)	($compnum+ratnum  x y))
      ((flonum?)	($compnum+flonum  x y))
      ((compnum?)	($compnum+compnum x y))
      ((cflonum?)	($compnum+cflonum x y))
      (else
       (err who y))))

  (define ($cflonum+number x y)
    (cond-numeric-operand y
      ((flonum?)	($cflonum+flonum  x y))
      ((cflonum?)	($cflonum+cflonum x y))
      ((fixnum?)	($cflonum+fixnum  x y))
      ((bignum?)	($cflonum+bignum  x y))
      ((ratnum?)	($cflonum+ratnum  x y))
      ((compnum?)	($cflonum+compnum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($number+fixnum x y)
    (cond-numeric-operand x
     ((fixnum?)		($fixnum+fixnum  x y))
     ((bignum?)		($bignum+fixnum  x y))
     ((ratnum?)		($ratnum+fixnum  x y))
     ((flonum?)		($flonum+fixnum  x y))
     ((compnum?)	($compnum+fixnum x y))
     ((cflonum?)	($cflonum+fixnum x y))
     (else
      (err who y))))

  (define ($number+bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum+bignum  x y))
      ((bignum?)	($bignum+bignum  x y))
      ((ratnum?)	($ratnum+bignum  x y))
      ((flonum?)	($flonum+bignum  x y))
      ((compnum?)	($compnum+bignum x y))
      ((cflonum?)	($cflonum+bignum x y))
      (else
       (err who y))))

  (define ($number+flonum x y)
    (cond-numeric-operand x
     ((fixnum?)		($fixnum+flonum  x y))
     ((bignum?)		($bignum+flonum  x y))
     ((ratnum?)		($ratnum+flonum  x y))
     ((flonum?)		($flonum+flonum  x y))
     ((compnum?)	($compnum+flonum x y))
     ((cflonum?)	($cflonum+flonum x y))
     (else
      (err who y))))

  (define ($number+ratnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum+ratnum  x y))
      ((bignum?)	($bignum+ratnum  x y))
      ((ratnum?)	($ratnum+ratnum  x y))
      ((flonum?)	($flonum+ratnum  x y))
      ((compnum?)	($compnum+ratnum x y))
      ((cflonum?)	($cflonum+ratnum x y))
      (else
       (err who y))))

  (define ($number+compnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum+compnum  x y))
      ((bignum?)	($bignum+compnum  x y))
      ((ratnum?)	($ratnum+compnum  x y))
      ((flonum?)	($flonum+compnum  x y))
      ((compnum?)	($compnum+compnum x y))
      ((cflonum?)	($cflonum+compnum x y))
      (else
       (err who y))))

  (define ($number+cflonum x y)
    (cond-numeric-operand x
      ((flonum?)	($flonum+cflonum  x y))
      ((cflonum?)	($cflonum+cflonum x y))
      ((fixnum?)	($fixnum+cflonum  x y))
      ((bignum?)	($bignum+cflonum  x y))
      ((ratnum?)	($ratnum+cflonum  x y))
      ((compnum?)	($compnum+cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($fixnum+fixnum x y)
    (foreign-call "ikrt_fxfxplus" x y))

  (define ($fixnum+bignum x y)
    (foreign-call "ikrt_fxbnplus" x y))

  (define ($fixnum+flonum x y)
    ($fl+ ($fixnum->flonum x) y))

  (define ($fixnum+ratnum x y)
    ;;     y.num   x * y.den + y.num
    ;; x + ----- = -----------------
    ;;     y.den         y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($make-ratnum (binary+ ($fixnum*number x y.den)
			     y.num)
		    y.den)))

  (define ($fixnum+compnum x y)
    ($make-compnum ($fixnum+number x ($compnum-real y))
		   ($compnum-imag y)))

  (define ($fixnum+cflonum x y)
    ($make-cflonum ($fixnum+flonum x   ($cflonum-real y))
		   ($cflonum-imag y)))

;;; --------------------------------------------------------------------

  (define ($bignum+fixnum x y)
    (foreign-call "ikrt_fxbnplus" y x))

  (define ($bignum+bignum x y)
    (foreign-call "ikrt_bnbnplus" x y))

  (define ($bignum+flonum x y)
    ($fl+ ($bignum->flonum x) y))

  (define ($bignum+ratnum x y)
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($make-ratnum (+ ($bignum*number x y.den)
		       y.num)
		    y.den)))

  (define ($bignum+compnum x y)
    ($make-compnum ($bignum+number x ($compnum-real y))
		   ($compnum-imag y)))

  (define ($bignum+cflonum x y)
    ($make-cflonum ($bignum+flonum x ($cflonum-real y))
		   ($cflonum-imag y)))

;;; --------------------------------------------------------------------

  (define ($flonum+fixnum x y)
    ($fl+ x ($fixnum->flonum y)))

  (define ($flonum+bignum x y)
    ($fl+ x ($bignum->flonum y)))

  (define ($flonum+flonum x y)
    ($fl+ x y))

  (define ($flonum+ratnum x y)
    ($fl+ x ($ratnum->flonum y)))

  (define ($flonum+compnum x y)
    ($make-rectangular ($flonum+number x ($compnum-real y))
		       ($compnum-imag y)))

  (define ($flonum+cflonum x y)
    ($make-cflonum ($fl+ x ($cflonum-real y))
		   ($cflonum-imag y)))

;;; --------------------------------------------------------------------

  (define ($ratnum+fixnum x y)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      ($make-ratnum (binary+ ($fixnum*number y x.den) x.num)
		    x.den)))

  (define ($ratnum+bignum x y)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      ($make-ratnum (binary+ ($bignum*number y x.den) x.num)
		    x.den)))

  (define ($ratnum+flonum x y)
    ($fl+ y ($ratnum->flonum x)))

  (define ($ratnum+ratnum x y)
    ;; x.num   y.num   x.num * y.den + x.den * y.num
    ;; ----- + ----- = -----------------------------
    ;; x.den   y.den          x.den * y.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (y.num ($ratnum-n y))
	  (x.den ($ratnum-d x))
	  (y.den ($ratnum-d y)))
      (binary/ (binary+ (binary* x.num y.den)
			(binary* x.den y.num))
	       (binary* x.den y.den))))

  (define ($ratnum+compnum x y)
    ($make-rectangular (binary+ x ($compnum-real y))
		       ($compnum-imag y)))

  (define ($ratnum+cflonum x y)
    ($make-cflonum (binary+ x ($cflonum-real y))
		   ($cflonum-imag y)))

;;; --------------------------------------------------------------------

  (define ($compnum+fixnum x y)
    ($make-rectangular ($number+fixnum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($compnum+bignum x y)
    ($make-rectangular ($number+bignum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($compnum+ratnum x y)
    ($make-rectangular ($number+ratnum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($compnum+compnum x y)
    ($make-rectangular (binary+ ($compnum-real x) ($compnum-real y))
		       (binary+ ($compnum-imag x) ($compnum-imag y))))

  (define ($compnum+flonum x y)
    ($make-rectangular ($flonum+number y ($compnum-real x))
		       ($compnum-imag x)))

  (define ($compnum+cflonum x y)
    ($make-cflonum ($number+flonum ($compnum-real x) ($cflonum-real y))
		   ($number+flonum ($compnum-imag x) ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($cflonum+fixnum x y)
    ($make-cflonum ($flonum+fixnum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($cflonum+bignum x y)
    ($make-cflonum ($flonum+bignum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($cflonum+ratnum x y)
    ($make-compnum ($flonum+ratnum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($cflonum+flonum x y)
    ($make-cflonum ($fl+ ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($cflonum+compnum x y)
    ($make-cflonum ($flonum+number ($cflonum-real x) ($compnum-real y))
		   ($flonum+number ($cflonum-imag x) ($compnum-imag y))))

  (define ($cflonum+cflonum x y)
    ($make-cflonum ($fl+ ($cflonum-real x) ($cflonum-real y))
		   ($fl+ ($cflonum-imag x) ($cflonum-imag y))))

  #| end of module: binary+ |# )


(module (binary-
	 $fixnum-number		$bignum-number		$flonum-number
	 $ratnum-number		$compnum-number		$cflonum-number
	 $number-fixnum		$number-bignum		$number-flonum
	 $ratnum-number		$number-compnum		$number-cflonum
	 $fixnum-fixnum		$fixnum-bignum		$fixnum-flonum
	 $fixnum-ratnum		$fixnum-compnum		$fixnum-cflonum
	 $bignum-fixnum		$bignum-bignum		$bignum-flonum
	 $bignum-ratnum		$bignum-compnum		$bignum-cflonum
	 $flonum-fixnum		$flonum-bignum		$flonum-ratnum
	 $flonum-flonum		$flonum-compnum		$flonum-cflonum
	 $ratnum-fixnum		$ratnum-bignum		$ratnum-flonum
	 $ratnum-ratnum		$ratnum-compnum		$ratnum-cflonum
	 $compnum-fixnum	$compnum-bignum		$compnum-ratnum
	 $compnum-compnum	$compnum-flonum		$compnum-cflonum
	 $cflonum-fixnum	$cflonum-bignum		$cflonum-ratnum
	 $cflonum-flonum	$cflonum-compnum	$cflonum-cflonum)
  (define who (quote -))

  (define (binary- x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum-number  x y))
      ((bignum?)	($bignum-number  x y))
      ((flonum?)	($flonum-number  x y))
      ((ratnum?)	($ratnum-number  x y))
      ((compnum?)	($compnum-number x y))
      ((cflonum?)	($cflonum-number x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($fixnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($fixnum-fixnum  x y))
      ((bignum?)	($fixnum-bignum  x y))
      ((flonum?)	($fixnum-flonum  x y))
      ((ratnum?)	($fixnum-ratnum  x y))
      ((compnum?)	($fixnum-compnum x y))
      ((cflonum?)	($fixnum-cflonum x y))
      (else
       (err who y))))

  (define ($bignum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($bignum-fixnum x y))
      ((bignum?)	($bignum-bignum x y))
      ((flonum?)	($bignum-flonum x y))
      ((ratnum?)	($bignum-ratnum x y))
      ((compnum?)	($bignum-compnum x y))
      ((cflonum?)	($bignum-cflonum x y))
      (else
       (err who y))))

  (define ($flonum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($flonum-flonum  x y))
      ((cflonum?)	($flonum-cflonum x y))
      ((fixnum?)	($flonum-fixnum  x y))
      ((bignum?)	($flonum-bignum  x y))
      ((ratnum?)	($flonum-ratnum  x y))
      ((compnum?)	($flonum-compnum x y))
      (else
       (err who y))))

  (define ($ratnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($ratnum-fixnum  x y))
      ((bignum?)	($ratnum-bignum  x y))
      ((flonum?)	($ratnum-flonum  x y))
      ((ratnum?)	($ratnum-ratnum  x y))
      ((compnum?)	($ratnum-compnum x y))
      ((cflonum?)	($ratnum-cflonum x y))
      (else
       (err who y))))

  (define ($compnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($compnum-fixnum  x y))
      ((bignum?)	($compnum-bignum  x y))
      ((flonum?)	($compnum-flonum  x y))
      ((ratnum?)	($compnum-ratnum  x y))
      ((compnum?)	($compnum-compnum x y))
      ((cflonum?)	($compnum-cflonum x y))
      (else
       (err who y))))

  (define ($cflonum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($cflonum-flonum  x y))
      ((cflonum?)	($cflonum-cflonum x y))
      ((fixnum?)	($cflonum-fixnum  x y))
      ((bignum?)	($cflonum-bignum  x y))
      ((ratnum?)	($cflonum-ratnum  x y))
      ((compnum?)	($cflonum-compnum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($number-fixnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum-fixnum  x y))
      ((bignum?)	($bignum-fixnum  x y))
      ((flonum?)	($flonum-fixnum  x y))
      ((ratnum?)	($ratnum-fixnum  x y))
      ((compnum?)	($compnum-fixnum x y))
      ((cflonum?)	($cflonum-fixnum x y))
      (else
       (err who y))))

  (define ($number-bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum-bignum x y))
      ((bignum?)	($bignum-bignum x y))
      ((flonum?)	($flonum-bignum x y))
      ((ratnum?)	($ratnum-bignum x y))
      ((compnum?)	($compnum-bignum x y))
      ((cflonum?)	($cflonum-bignum x y))
      (else
       (err who y))))

  (define ($number-flonum x y)
    (cond-numeric-operand x
      ((flonum?)	($flonum-flonum  x y))
      ((cflonum?)	($cflonum-flonum x y))
      ((fixnum?)	($fixnum-flonum  x y))
      ((bignum?)	($bignum-flonum  x y))
      ((ratnum?)	($ratnum-flonum  x y))
      ((compnum?)	($compnum-flonum x y))
      (else
       (err who y))))

  (define ($number-ratnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum-ratnum  x y))
      ((bignum?)	($bignum-ratnum  x y))
      ((flonum?)	($flonum-ratnum  x y))
      ((ratnum?)	($ratnum-ratnum  x y))
      ((compnum?)	($compnum-ratnum x y))
      ((cflonum?)	($cflonum-ratnum x y))
      (else
       (err who y))))

  (define ($number-compnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum-compnum  x y))
      ((bignum?)	($bignum-compnum  x y))
      ((flonum?)	($flonum-compnum  x y))
      ((ratnum?)	($ratnum-compnum  x y))
      ((compnum?)	($compnum-compnum x y))
      ((cflonum?)	($cflonum-compnum x y))
      (else
       (err who y))))

  (define ($number-cflonum x y)
    (cond-numeric-operand x
      ((flonum?)	($flonum-cflonum  x y))
      ((cflonum?)	($cflonum-cflonum x y))
      ((fixnum?)	($fixnum-cflonum  x y))
      ((bignum?)	($bignum-cflonum  x y))
      ((ratnum?)	($ratnum-cflonum  x y))
      ((compnum?)	($compnum-cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($fixnum-fixnum x y)
    (foreign-call "ikrt_fxfxminus" x y))

  (define ($fixnum-bignum x y)
    (foreign-call "ikrt_fxbnminus" x y))

  (define ($fixnum-flonum x y)
    (if ($fxzero? x)
	($fl* y -1.0)
      ($fl- ($fixnum->flonum x) y)))

  (define ($fixnum-ratnum x y)
    ;;     y.num   x * y.den - y.num
    ;; x - ----- = -----------------
    ;;     y.den        y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      (binary/ (binary- ($fixnum*number x y.den) y.num)
	       y.den)))

  (define ($fixnum-compnum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-compnum ($fixnum-number x ($compnum-real y))
		   ($fixnum-number 0 ($compnum-imag y))))

  (define ($fixnum-cflonum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-cflonum ($fixnum-flonum x   ($cflonum-real y))
		   ($fl-           0.0 ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($bignum-fixnum x y)
    (foreign-call "ikrt_bnfxminus" x y))

  (define ($bignum-bignum x y)
    (foreign-call "ikrt_bnbnminus" x y))

  (define ($bignum-flonum x y)
    ($fl- ($bignum->flonum x) y))

  (define ($bignum-ratnum x y)
    ;;     y.num   x * y.den - y.num
    ;; x - ----- = -----------------
    ;;     y.den        y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      (binary/ (binary- ($bignum*number x y.den) y.num)
	       y.den)))

  (define ($bignum-compnum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-rectangular ($bignum-number x ($compnum-real y))
		       ($fixnum-number 0 ($compnum-imag y))))

  (define ($bignum-cflonum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-cflonum ($bignum-flonum x   ($cflonum-real y))
		   ($fl-           0.0 ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($flonum-fixnum x y)
    ($fl- x ($fixnum->flonum y)))

  (define ($flonum-bignum x y)
    ($fl- x ($bignum->flonum y)))

  (define ($flonum-ratnum x y)
    ;;     y.num   x * y.den - y.num
    ;; x - ----- = -----------------
    ;;     y.den        y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      (binary/ (binary- ($flonum*number x y.den) y.num)
	       y.den)))

  (define ($flonum-flonum x y)
    ($fl- x y))

  (define ($flonum-compnum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-rectangular ($flonum-number x ($compnum-real y))
		       ($fixnum-number 0 ($compnum-imag y))))

  (define ($flonum-cflonum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-cflonum ($fl- x   ($cflonum-real y))
		   ($fl- 0.0 ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($ratnum-fixnum x y)
    ;; x.num       x.num - y * x.den
    ;; ----- - y = -----------------
    ;; x.den            x.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (binary/ (binary- x.num ($fixnum*number y x.den))
	       x.den)))

  (define ($ratnum-bignum x y)
    ;; x.num       x.num - y * x.den
    ;; ----- - y = -----------------
    ;; x.den            x.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (binary/ (binary- x.num ($bignum*number y x.den))
	       x.den)))

  (define ($ratnum-flonum x y)
    ;; x.num       x.num - y * x.den
    ;; ----- - y = -----------------
    ;; x.den            x.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (binary/ (binary- x.num ($flonum*number y x.den))
	       x.den)))

  (define ($ratnum-ratnum x y)
    ;; x.num   y.num   x.num * y.den - x.den * y.num
    ;; ----- - ----- = -----------------------------
    ;; x.den   y.den           x.den * y.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x))
	  (y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      (binary/ (binary- (binary* x.num y.den)
			(binary* y.num x.den))
	       (binary* x.den y.den))))

  (define ($ratnum-compnum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-rectangular ($ratnum-number x ($compnum-real y))
		       ($fixnum-number 0 ($compnum-imag y))))

  (define ($ratnum-cflonum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-cflonum ($ratnum-flonum x   ($cflonum-real y))
		   ($fl-           0.0 ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($compnum-fixnum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-rectangular ($number-fixnum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($compnum-bignum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-rectangular ($number-bignum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($compnum-ratnum x y)
    ;;                       y.num            y.num
    ;; (x.rep + i * x.imp) - ----- = (x.rep - -----) + i * x.imp
    ;;                       y.den            y.den
    ;;
    ($make-compnum ($number-ratnum ($compnum-real x) y)
		   ($compnum-imag x)))

  (define ($compnum-compnum x y)
    ;; (x.rep + i * x.imp) - (y.rep + i * y.imp)
    ;; = (x.rep - y.rep) + i * (x.imp - y.imp)
    ;;
    ($make-rectangular (binary- ($compnum-real x) ($compnum-real y))
		       (binary- ($compnum-imag x) ($compnum-imag y))))

  (define ($compnum-flonum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-rectangular ($number-flonum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($compnum-cflonum x y)
    ;; (x.rep + i * x.imp) - (y.rep + i * y.imp)
    ;; = (x.rep - y.rep) + i * (x.imp - y.imp)
    ;;
    ($make-cflonum ($number-flonum ($compnum-real x) ($cflonum-real y))
		   ($number-flonum ($compnum-imag x) ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($cflonum-fixnum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-cflonum ($flonum-fixnum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($cflonum-bignum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-cflonum ($flonum-bignum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($cflonum-ratnum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-cflonum ($flonum-ratnum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($cflonum-flonum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-cflonum ($fl- ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($cflonum-compnum x y)
    ;; (x.rep + i * x.imp) - (y.rep + i * y.imp)
    ;; = (x.rep - y.rep) + i * (x.imp - y.imp)
    ;;
    ($make-cflonum ($flonum-number ($cflonum-real x) ($compnum-real y))
		   ($flonum-number ($cflonum-imag x) ($compnum-imag y))))

  (define ($cflonum-cflonum x y)
    ;; (x.rep + i * x.imp) - (y.rep + i * y.imp)
    ;; = (x.rep - y.rep) + i * (x.imp - y.imp)
    ;;
    ($make-cflonum ($fl- ($cflonum-real x) ($cflonum-real y))
		   ($fl- ($cflonum-imag x) ($cflonum-imag y))))

  #| end of module: binary- |# )


(module (binary*
	 $fixnum*number		$bignum*number		$flonum*number
	 $ratnum*number		$compnum*number		$cflonum*number
	 $number*fixnum		$number*bignum		$number*flonum
	 $number*ratnum		$number*compnum		$number*cflonum
	 $fixnum*fixnum		$fixnum*bignum		$fixnum*flonum
	 $fixnum*ratnum		$fixnum*compnum		$fixnum*cflonum
	 $bignum*fixnum		$bignum*bignum		$bignum*flonum
	 $bignum*ratnum		$bignum*compnum		$bignum*cflonum
	 $flonum*flonum		$flonum*cflonum		$flonum*fixnum
	 $flonum*bignum		$flonum*ratnum		$flonum*compnum
	 $ratnum*fixnum		$ratnum*bignum		$ratnum*flonum
	 $ratnum*ratnum		$ratnum*compnum		$ratnum*cflonum
	 $compnum*fixnum	$compnum*bignum		$compnum*ratnum
	 $compnum*flonum	$compnum*compnum	$compnum*cflonum
	 $cflonum*fixnum	$cflonum*bignum		$cflonum*ratnum
	 $cflonum*flonum	$cflonum*compnum	$cflonum*cflonum)
  (define who (quote *))

  (define (binary* x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum*number  x y))
      ((bignum?)	($bignum*number  x y))
      ((flonum?)	($flonum*number  x y))
      ((ratnum?)	($ratnum*number  x y))
      ((compnum?)	($compnum*number x y))
      ((cflonum?)	($cflonum*number x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($fixnum*number x y)
    (cond-numeric-operand y
      ((fixnum?)	($fixnum*fixnum  x y))
      ((bignum?)	($fixnum*bignum  x y))
      ((flonum?)	($fixnum*flonum  x y))
      ((ratnum?)	($fixnum*ratnum  x y))
      ((compnum?)	($fixnum*compnum x y))
      ((cflonum?)	($fixnum*cflonum x y))
      (else
       (err who y))))

  (define ($bignum*number x y)
    (cond-numeric-operand y
      ((fixnum?)	($bignum*fixnum  x y))
      ((bignum?)	($bignum*bignum  x y))
      ((flonum?)	($bignum*flonum  x y))
      ((ratnum?)	($bignum*ratnum  x y))
      ((compnum?)	($bignum*compnum x y))
      ((cflonum?)	($bignum*cflonum x y))
      (else
       (err who y))))

  (define ($flonum*number x y)
    (cond-numeric-operand y
      ((flonum?)	($flonum*flonum  x y))
      ((cflonum?)	($flonum*cflonum x y))
      ((fixnum?)	($flonum*fixnum  x y))
      ((bignum?)	($flonum*bignum  x y))
      ((ratnum?)	($flonum*ratnum  x y))
      ((compnum?)	($flonum*compnum x y))
      (else
       (err who y))))

  (define ($ratnum*number x y)
    (cond-numeric-operand y
      ((fixnum?)	($ratnum*fixnum  x y))
      ((bignum?)	($ratnum*bignum  x y))
      ((flonum?)	($ratnum*flonum  x y))
      ((ratnum?)	($ratnum*ratnum  x y))
      ((compnum?)	($ratnum*compnum x y))
      ((cflonum?)	($ratnum*cflonum x y))
      (else
       (err who y))))

  (define ($compnum*number x y)
    (cond-numeric-operand y
      ((fixnum?)	($compnum*fixnum  x y))
      ((bignum?)	($compnum*bignum  x y))
      ((ratnum?)	($compnum*ratnum  x y))
      ((flonum?)	($compnum*flonum  x y))
      ((compnum?)	($compnum*compnum x y))
      ((cflonum?)	($compnum*cflonum x y))
      (else
       (err who y))))

  (define ($cflonum*number x y)
    (cond-numeric-operand y
      ((flonum?)	($cflonum*flonum  x y))
      ((cflonum?)	($cflonum*cflonum x y))
      ((fixnum?)	($cflonum*fixnum  x y))
      ((bignum?)	($cflonum*bignum  x y))
      ((ratnum?)	($cflonum*ratnum  x y))
      ((compnum?)	($cflonum*compnum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($number*fixnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum*fixnum  x y))
      ((bignum?)	($bignum*fixnum  x y))
      ((flonum?)	($flonum*fixnum  x y))
      ((ratnum?)	($ratnum*fixnum  x y))
      ((compnum?)	($compnum*fixnum x y))
      ((cflonum?)	($cflonum*fixnum x y))
      (else
       (err who y))))

  (define ($number*bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum*bignum  x y))
      ((bignum?)	($bignum*bignum  x y))
      ((flonum?)	($flonum*bignum  x y))
      ((ratnum?)	($ratnum*bignum  x y))
      ((compnum?)	($compnum*bignum x y))
      ((cflonum?)	($cflonum*bignum x y))
      (else
       (err who y))))

  (define ($number*flonum x y)
    (cond-numeric-operand x
      ((flonum?)	($flonum*flonum  x y))
      ((cflonum?)	($cflonum*flonum x y))
      ((fixnum?)	($fixnum*flonum  x y))
      ((bignum?)	($bignum*flonum  x y))
      ((ratnum?)	($ratnum*flonum  x y))
      ((compnum?)	($compnum*flonum x y))
      (else
       (err who y))))

  (define ($number*ratnum x y)
    (cond ((ratnum?  x)		($ratnum*ratnum  x y))
	  ((compnum? x)		($compnum*ratnum x y))
	  ((cflonum? x)		($cflonum*ratnum x y))
	  (else
	   (binary* y x))))

  (define ($number*compnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum*compnum  x y))
      ((bignum?)	($bignum*compnum  x y))
      ((ratnum?)	($ratnum*compnum  x y))
      ((flonum?)	($flonum*compnum  x y))
      ((compnum?)	($compnum*compnum x y))
      ((cflonum?)	($cflonum*compnum x y))
      (else
       (err who y))))

  (define ($number*cflonum x y)
    (cond-numeric-operand x
      ((flonum?)	($flonum*cflonum  x y))
      ((cflonum?)	($cflonum*cflonum x y))
      ((fixnum?)	($fixnum*cflonum  x y))
      ((bignum?)	($bignum*cflonum  x y))
      ((ratnum?)	($ratnum*cflonum  x y))
      ((compnum?)	($compnum*cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($fixnum*fixnum x y)
    (foreign-call "ikrt_fxfxmult" x y))

  (define ($fixnum*bignum x y)
    (foreign-call "ikrt_fxbnmult" x y))

  (define ($fixnum*flonum x y)
    ($fl* ($fixnum->flonum x) y))

  (define ($fixnum*ratnum x y)
    (binary/ ($fixnum*number x ($ratnum-n y))
	     ($ratnum-d y)))

  (define ($fixnum*compnum x y)
    ($make-rectangular ($fixnum*number x ($compnum-real y))
		       ($fixnum*number x ($compnum-imag y))))

  (define ($fixnum*cflonum x y)
    ($make-cflonum ($fixnum*flonum x ($cflonum-real y))
		   ($fixnum*flonum x ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($bignum*fixnum x y)
    (foreign-call "ikrt_fxbnmult" y x))

  (define ($bignum*bignum x y)
    (foreign-call "ikrt_bnbnmult" x y))

  (define ($bignum*flonum x y)
    ($fl* ($bignum->flonum x) y))

  (define ($bignum*ratnum x y)
    (binary/ ($bignum*number x ($ratnum-n y))
	     ($ratnum-d y)))

  (define ($bignum*compnum x y)
    ($make-rectangular ($bignum*number x ($compnum-real y))
		       ($bignum*number x ($compnum-imag y))))

  (define ($bignum*cflonum x y)
    ($make-cflonum ($bignum*flonum x ($cflonum-real y))
		   ($bignum*flonum x ($cflonum-imag y))))

;;; --------------------------------------------------------------------
;;; flonum * number = flonum or cflonum
;;; flonum * real   = flonum

  (define ($flonum*flonum x y)
    ($fl* x y))

  (define ($flonum*cflonum x y)
    ($make-cflonum ($fl* x ($cflonum-real y))
		   ($fl* x ($cflonum-imag y))))

  (define ($flonum*fixnum x y)
    ($fl* x ($fixnum->flonum y)))

  (define ($flonum*bignum x y)
    ($fl* x ($bignum->flonum y)))

  (define ($flonum*ratnum x y)
    (binary/ ($flonum*number x ($ratnum-n y))
	     ($ratnum-d y)))

  (define ($flonum*compnum x y)
    ($make-cflonum ($flonum*number x ($compnum-real y))
		   ($flonum*number x ($compnum-imag y))))

;;; --------------------------------------------------------------------

  (define ($ratnum*fixnum x y)
    ($fixnum*ratnum y x))

  (define ($ratnum*bignum x y)
    ($bignum*ratnum y x))

  (define ($ratnum*ratnum x y)
    (binary/ (binary* ($ratnum-n x) ($ratnum-n y))
	     (binary* ($ratnum-d x) ($ratnum-d y))))

  (define ($ratnum*flonum x y)
    ($flonum*ratnum y x))

  (define ($ratnum*compnum x y)
    ($make-rectangular ($ratnum*number x ($compnum-real y))
		       ($ratnum*number x ($compnum-imag y))))

  (define ($ratnum*cflonum x y)
    ($make-cflonum ($flonum*ratnum ($cflonum-real y) x)
		   ($flonum*ratnum ($cflonum-imag y) x)))

;;; --------------------------------------------------------------------

  (define ($compnum*fixnum x y)
    ($make-rectangular ($fixnum*number y ($compnum-real x))
		       ($fixnum*number y ($compnum-imag x))))

  (define ($compnum*bignum x y)
    ($make-rectangular ($bignum*number y ($compnum-real x))
		       ($bignum*number y ($compnum-imag x))))

  (define ($compnum*ratnum x y)
    ($make-rectangular ($ratnum*number y ($compnum-real x))
		       ($ratnum*number y ($compnum-imag x))))

  (define ($compnum*flonum x y)
    ($make-cflonum ($flonum*number y ($compnum-real x))
		   ($flonum*number y ($compnum-imag x))))

  (define ($compnum*compnum x y)
    ;; (x.rep + i * x.imp) * (y.rep + i * y.imp)
    ;; = (x.rep * y.rep - x.imp * y.imp) + i * (x.rep * y.imp + x.imp * y.rep)
    ;;
    (let ((x.rep ($compnum-real x))
	  (y.rep ($compnum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($compnum-imag y)))
      ($make-rectangular (binary- (binary* x.rep y.rep) (binary* x.imp y.imp))
			 (binary+ (binary* x.rep y.imp) (binary* x.imp y.rep)))))

  (define ($compnum*cflonum x y)
    ;; (x.rep + i * x.imp) * (y.rep + i * y.imp)
    ;; = (x.rep * y.rep - x.imp * y.imp) + i * (x.rep * y.imp + x.imp * y.rep)
    ;;
    (let ((x.rep ($compnum-real x))
	  (y.rep ($cflonum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($cflonum-imag y)))
      ($make-cflonum ($fl- ($flonum*number y.rep x.rep) ($flonum*number y.imp x.imp))
		     ($fl+ ($flonum*number y.imp x.rep) ($flonum*number y.rep x.imp)))))

;;; --------------------------------------------------------------------

  (define ($cflonum*fixnum x y)
    ($make-cflonum ($flonum*fixnum ($compnum-real x) y)
		   ($flonum*fixnum ($compnum-imag x) y)))

  (define ($cflonum*bignum x y)
    ($make-cflonum ($flonum*bignum ($compnum-real x) y)
		   ($flonum*bignum ($compnum-imag x) y)))

  (define ($cflonum*ratnum x y)
    ($make-cflonum ($flonum*ratnum ($compnum-real x) y)
		   ($flonum*ratnum ($compnum-imag x) y)))

  (define ($cflonum*flonum x y)
    ($make-cflonum ($fl* ($cflonum-real x) y)
		   ($fl* ($cflonum-imag x) y)))

  (define ($cflonum*compnum x y)
    ;; (x.rep + i * x.imp) * (y.rep + i * y.imp)
    ;; = (x.rep * y.rep - x.imp * y.imp) + i * (x.rep * y.imp + x.imp * y.rep)
    ;;
    (let ((x.rep ($compnum-real x))
	  (y.rep ($compnum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($compnum-imag y)))
      ($make-cflonum ($fl- ($flonum*number x.rep y.rep) ($flonum*number x.imp y.imp))
		     ($fl+ ($flonum*number x.rep y.imp) ($flonum*number x.imp y.rep)))))

  (define ($cflonum*cflonum x y)
    ;; (x.rep + i * x.imp) * (y.rep + i * y.imp)
    ;; = (x.rep * y.rep - x.imp * y.imp) + i * (x.rep * y.imp + x.imp * y.rep)
    ;;
    (let ((r0 ($cflonum-real x))
	  (r1 ($cflonum-real y))
	  (i0 ($cflonum-imag x))
	  (i1 ($cflonum-imag y)))
      ($make-cflonum ($fl- ($fl* r0 r1) ($fl* i0 i1))
		     ($fl+ ($fl* r0 i1) ($fl* i0 r1)))))

  #| end of module: binary* |# )


(module (binary/
	 $flonum/number		$fixnum/number		$bignum/number
	 $ratnum/number		$compnum/number		$cflonum/number
	 $number/flonum		$number/fixnum		$number/bignum
	 $number/ratnum		$number/compnum		$number/cflonum
	 $fixnum/flonum		$fixnum/fixnum		$fixnum/bignum
	 $fixnum/ratnum		$fixnum/compnum		$fixnum/cflonum
	 $bignum/fixnum		$bignum/bignum		$bignum/flonum
	 $bignum/ratnum		$bignum/compnum		$bignum/cflonum
	 $ratnum/fixnum		$ratnum/bignum		$ratnum/ratnum
	 $ratnum/flonum		$ratnum/compnum		$ratnum/cflonum
	 $flonum/flonum		$flonum/cflonum		$flonum/fixnum
	 $flonum/bignum		$flonum/ratnum		$flonum/compnum
	 $compnum/fixnum	$compnum/bignum		$compnum/ratnum
	 $compnum/flonum	$compnum/compnum	$compnum/cflonum
	 $cflonum/fixnum	$cflonum/bignum		$cflonum/ratnum
	 $cflonum/flonum	$cflonum/compnum	$cflonum/cflonum)
  (define who (quote /))

  (define (binary/ x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum/number  x y))
      ((bignum?)	($bignum/number  x y))
      ((flonum?)	($flonum/number  x y))
      ((ratnum?)	($ratnum/number  x y))
      ((compnum?)	($compnum/number x y))
      ((cflonum?)	($cflonum/number x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($flonum/number x y)
    (cond-numeric-operand y
      ((flonum?)	($flonum/flonum  x y))
      ((cflonum?)	($flonum/cflonum x y))
      ((fixnum?)	($flonum/fixnum  x y))
      ((bignum?)	($flonum/bignum  x y))
      ((ratnum?)	($flonum/ratnum  x y))
      ((compnum?)	($flonum/compnum x y))
      (else
       (err who y))))

  (define ($fixnum/number x y)
    (cond-numeric-operand y
      ((flonum?)	($fixnum/flonum  x y))
      ((cflonum?)	($fixnum/cflonum x y))
      ((fixnum?)	($fixnum/fixnum  x y))
      ((bignum?)	($fixnum/bignum  x y))
      ((ratnum?)	($fixnum/ratnum  x y))
      ((compnum?)	($fixnum/compnum x y))
      (else
       (err who y))))

  (define ($bignum/number x y)
    (cond-numeric-operand y
      ((fixnum?)	($bignum/fixnum  x y))
      ((bignum?)	($bignum/bignum  x y))
      ((flonum?)	($bignum/flonum  x y))
      ((ratnum?)	($bignum/ratnum  x y))
      ((compnum?)	($bignum/compnum x y))
      ((cflonum?)	($bignum/cflonum x y))
      (else (err who y))))

  (define ($ratnum/number x y)
    (cond-numeric-operand y
      ((fixnum?)	($ratnum/fixnum  x y))
      ((bignum?)	($ratnum/bignum  x y))
      ((ratnum?)	($ratnum/ratnum  x y))
      ((flonum?)	($ratnum/flonum  x y))
      ((compnum?)	($ratnum/compnum x y))
      ((cflonum?)	($ratnum/cflonum x y))
      (else
       (err who y))))

  (define ($compnum/number x y)
    (cond-numeric-operand y
      ((fixnum?)	($compnum/fixnum  x y))
      ((bignum?)	($compnum/bignum  x y))
      ((ratnum?)	($compnum/ratnum  x y))
      ((flonum?)	($compnum/flonum  x y))
      ((compnum?)	($compnum/compnum x y))
      ((cflonum?)	($compnum/cflonum x y))
      (else
       (err who y))))

  (define ($cflonum/number x y)
    (cond-numeric-operand y
      ((fixnum?)	($cflonum/fixnum  x y))
      ((bignum?)	($cflonum/bignum  x y))
      ((ratnum?)	($cflonum/ratnum  x y))
      ((flonum?)	($cflonum/flonum  x y))
      ((compnum?)	($cflonum/compnum x y))
      ((cflonum?)	($cflonum/cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($number/flonum x y)
    (cond-numeric-operand x
      ((flonum?)	($flonum/flonum  x y))
      ((cflonum?)	($cflonum/flonum x y))
      ((fixnum?)	($fixnum/flonum  x y))
      ((bignum?)	($bignum/flonum  x y))
      ((ratnum?)	($ratnum/flonum  x y))
      ((compnum?)	($compnum/flonum x y))
      (else
       (err who y))))

  (define ($number/fixnum x y)
    (cond-numeric-operand x
      ((flonum?)	($flonum/fixnum  x y))
      ((cflonum?)	($cflonum/fixnum x y))
      ((fixnum?)	($fixnum/fixnum  x y))
      ((bignum?)	($bignum/fixnum  x y))
      ((ratnum?)	($ratnum/fixnum  x y))
      ((compnum?)	($compnum/fixnum x y))
      (else
       (err who y))))

  (define ($number/bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum/bignum  x y))
      ((bignum?)	($bignum/bignum  x y))
      ((ratnum?)	($ratnum/bignum  x y))
      ((flonum?)	($flonum/bignum  x y))
      ((compnum?)	($compnum/bignum x y))
      ((cflonum?)	($cflonum/bignum x y))
      (else
       (err who y))))

  (define ($number/ratnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum/ratnum  x y))
      ((bignum?)	($bignum/ratnum  x y))
      ((ratnum?)	($ratnum/ratnum  x y))
      ((flonum?)	($flonum/ratnum  x y))
      ((compnum?)	($compnum/ratnum x y))
      ((cflonum?)	($cflonum/ratnum x y))
      (else
       (err who y))))

  (define ($number/compnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum/compnum  x y))
      ((bignum?)	($bignum/compnum  x y))
      ((ratnum?)	($ratnum/compnum  x y))
      ((flonum?)	($flonum/compnum  x y))
      ((compnum?)	($compnum/compnum x y))
      ((cflonum?)	($cflonum/compnum x y))
      (else
       (err who y))))

  (define ($number/cflonum x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum/cflonum  x y))
      ((bignum?)	($bignum/cflonum  x y))
      ((ratnum?)	($ratnum/cflonum  x y))
      ((flonum?)	($flonum/cflonum  x y))
      ((compnum?)	($compnum/cflonum x y))
      ((cflonum?)	($cflonum/cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($fixnum/flonum x y)
    ($fl/ ($fixnum->flonum x) y))

  (define ($fixnum/fixnum x y)
    (cond (($fx= y 0)
	   (assertion-violation who "division by 0"))
	  (($fx> y 0)
	   (if ($fx= y 1)
	       x
	     (let ((g (binary-gcd x y)))
	       (cond (($fx= g y)
		      ($fxquotient x g))
		     (($fx= g 1)
		      ($make-ratnum x y))
		     (else
		      ($make-ratnum ($fxquotient x g)
				    ($fxquotient y g)))))))
	  (else
	   ;;Here Y is negative.
	   (if ($fx= y -1)
	       (binary- 0 x)
	     (let ((g (binary-gcd x y)))
	       (cond (($fx= ($fixnum- g) y)
		      ($fixnum- ($fxquotient x g)))
		     (($fx= g 1)
		      ($make-ratnum ($fixnum- x)
				    ($fixnum- y)))
		     (else
		      ($make-ratnum ($fixnum- ($fxquotient x g))
				    ($fixnum- ($fxquotient y g))))))))))

  (define ($fixnum/bignum x y)
    (if ($fx= x 0)
	0
      ;;The GCD between a fixnum and any exact integer is a fixnum.
      (let ((g (binary-gcd x y)))
	(cond ((= g y)
	       (quotient x g))
	      (($bignum-positive? y)
	       (if ($fx= g 1)
		   ($make-ratnum x y)
		 ($make-ratnum ($fxquotient x g)
			       (quotient y g))))
	      (else
	       (if ($fx= g 1)
		   ($make-ratnum ($fixnum- x)
				 (binary- 0 y))
		 ($make-ratnum ($fixnum- ($fxquotient x g))
			       ($fixnum-number 0 (quotient y g)))))))))

  (define ($fixnum/ratnum x y)
    (binary/ ($fixnum*number x ($ratnum-d y))
	     ($ratnum-n y)))

  (define ($fixnum/compnum x y)
    (let ((y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom (binary+ (binary* y.rep y.rep)
			    (binary* y.imp y.imp))))
	($make-rectangular (binary/ ($fixnum*number x y.rep)
				    denom)
			   (binary/ ($fixnum*number ($fixnum- x) y.imp)
				    denom)))))

  (define ($fixnum/cflonum x y)
    (let ((y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom ($flonum+flonum ($flonum*flonum y.rep y.rep)
				   ($flonum*flonum y.imp y.imp))))
	($make-rectangular ($flonum/flonum ($fixnum*flonum x y.rep)
					   denom)
			   ($flonum/flonum ($fixnum*flonum ($fixnum- x) y.imp)
					   denom)))))

;;; --------------------------------------------------------------------

  (define ($bignum/fixnum x y)
    (cond (($fx= y 0)
	   (assertion-violation who "division by 0"))
	  (($fx> y 0)
	   (if ($fx= y 1)
	       x
	     ;;The  GCD between  any exact  integer  and a  fixnum is  a
	     ;;fixnum.
	     (let ((g (binary-gcd x y)))
	       (cond (($fx= g 1)
		      ($make-ratnum x y))
		     (($fx= g y)
		      (quotient x g))
		     (else
		      ($make-ratnum (quotient x g)
				    ($fxquotient y g)))))))
	  (else
	   ;;Here Y is negative.
	   (if ($fx= y -1)
	       ($bignum- x)
	     ;;The  GCD between  any exact  integer  and a  fixnum is  a
	     ;;fixnum.
	     (let ((g (binary-gcd x y)))
	       (if (= ($fixnum- g) y)
		   (unary- (quotient x g))
		 ($make-ratnum (unary- (quotient x g))
			       ($fx- 0 ($fxquotient y g)))))))))

  (define ($bignum/bignum x y)
    (let ((g (binary-gcd x y)))
      (cond (($fx= g 1)
	     (if ($bignum-positive? y)
		 ($make-ratnum x y)
	       ($make-ratnum ($fixnum-bignum 0 x)
			     ($fixnum-bignum 0 y))))
	    (($bignum-positive? y)
	     (if (= g y)
		 (quotient x g)
	       ($make-ratnum (quotient x g)
			     (quotient y g))))
	    (else
	     (let ((y (binary- 0 y)))
	       (if (= g y)
		   (binary- 0 (quotient x g))
		 ($make-ratnum ($fixnum-number 0 (quotient x g))
			       (quotient y g))))))))

  (define ($bignum/flonum x y)
    ($fl/ ($bignum->flonum x) y))

  (define ($bignum/ratnum x y)
    (binary/ (binary* x ($ratnum-d y)) ($ratnum-n y)))

  (define ($bignum/compnum x y)
    (let ((y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom (binary+ (binary* y.rep y.rep)
			    (binary* y.imp y.imp))))
	($make-rectangular (binary/ ($bignum*number x y.rep)
				    denom)
			   (binary/ ($bignum*number ($fixnum-bignum 0 x) y.imp)
				    denom)))))

  (define ($bignum/cflonum x y)
    (let ((y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom ($flonum+flonum ($flonum*flonum y.rep y.rep)
				   ($flonum*flonum y.imp y.imp))))
	($make-rectangular ($fl/ ($bignum*flonum x y.rep)
				 denom)
			   ($fl/ ($bignum*flonum ($fixnum-bignum 0 x) y.imp)
				 denom)))))

;;; --------------------------------------------------------------------

  (define ($ratnum/fixnum x y)
    ($fixnum/number 1 ($fixnum/ratnum y x)))

  (define ($ratnum/bignum x y)
    ($fixnum/number 1 ($bignum/ratnum y x)))

  (define ($ratnum/ratnum x y)
    (binary/ (binary* ($ratnum-n x) ($ratnum-d y))
	     (binary* ($ratnum-n y) ($ratnum-d x))))

  (define ($ratnum/flonum x y)
    ($fixnum/number 1 ($flonum/ratnum y x)))

  (define ($ratnum/compnum x y)
    (let ((y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom (binary+ (binary* y.rep y.rep)
			    (binary* y.imp y.imp))))
	($make-rectangular (binary/ ($ratnum*number x y.rep)
				    denom)
			   (binary/ ($ratnum*number ($fixnum-ratnum 0 x) y.imp)
				    denom)))))

  (define ($ratnum/cflonum x y)
    (let ((y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom ($flonum+flonum ($flonum*flonum y.rep y.rep)
				   ($flonum*flonum y.imp y.imp))))
	($make-rectangular ($flonum/flonum ($ratnum*flonum x y.rep)
					   denom)
			   ($flonum/flonum ($ratnum*flonum ($fixnum-ratnum 0 x) y.imp)
					   denom)))))

;;; --------------------------------------------------------------------

  (define ($flonum/fixnum x y)
    ($fl/ x ($fixnum->flonum y)))

  (define ($flonum/bignum x y)
    ($fl/ x ($bignum->flonum y)))

  (define ($flonum/ratnum x y)
    ($fl/ x ($ratnum->flonum y)))

  (define ($flonum/flonum x y)
    ($fl/ x y))

  (define ($flonum/compnum x y)
    (let ((y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom (binary+ (binary* y.rep y.rep)
			    (binary* y.imp y.imp))))
	($make-cflonum ($fl/ ($flonum*number x y.rep)
			     denom)
		       ($fl/ ($flonum*number ($fl- 0.0 x) y.imp)
			     denom)))))

  (define ($flonum/cflonum x y)
    (let ((y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom ($flonum+flonum ($flonum*flonum y.rep y.rep)
				   ($flonum*flonum y.imp y.imp))))
	($make-cflonum ($fl/ ($fl* x y.rep)
			     denom)
		       ($fl/ ($fl* ($fl- 0.0 x) y.imp)
			     denom)))))

;;; --------------------------------------------------------------------

  (define ($compnum/fixnum x y)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular ($number/fixnum x.rep y)
			 ($number/fixnum x.imp y))))

  (define ($compnum/bignum x y)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular ($number/bignum x.rep y)
			 ($number/bignum x.imp y))))

  (define ($compnum/ratnum x y)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular ($number/ratnum x.rep y)
			 ($number/ratnum x.imp y))))

  (define ($compnum/flonum x y)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-cflonum ($number/flonum x.rep y)
		     ($number/flonum x.imp y))))

  (define ($compnum/compnum x y)
    ;; x.rep + i * x.imp
    ;; ----------------- = z.rep + i * z.imp
    ;; y.rep + i * y.imp
    ;;
    ;;         x.rep * y.rep + x.imp * y.imp
    ;; z.rep = -----------------------------
    ;;              y.rep^2 + y.imp^2
    ;;
    ;;         x.imp * y.rep - x.rep * y.imp
    ;; z.imp = -----------------------------
    ;;              y.rep^2 + y.imp^2
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x))
	  (y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom (binary+ (binary* y.rep y.rep)
			    (binary* y.imp y.imp))))
	($make-rectangular (binary/ (binary+ (binary* x.rep y.rep)
					     (binary* x.imp y.imp))
				    denom)
			   (binary/ (binary- (binary* x.imp y.rep)
					     (binary* x.rep y.imp))
				    denom)))))

  (define ($compnum/cflonum x y)
    ;; x.rep + i * x.imp
    ;; ----------------- = z.rep + i * z.imp
    ;; y.rep + i * y.imp
    ;;
    ;;         x.rep * y.rep + x.imp * y.imp
    ;; z.rep = -----------------------------
    ;;              y.rep^2 + y.imp^2
    ;;
    ;;         x.imp * y.rep - x.rep * y.imp
    ;; z.imp = -----------------------------
    ;;              y.rep^2 + y.imp^2
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x))
	  (y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom ($fl+ ($fl* y.rep y.rep)
			 ($fl* y.imp y.imp))))
	($make-cflonum ($fl/ ($fl+ ($number*flonum x.rep y.rep)
				   ($number*flonum x.imp y.imp))
			     denom)
		       ($fl/ ($fl- ($number*flonum x.imp y.rep)
				   ($number*flonum x.rep y.imp))
			     denom)))))

;;; --------------------------------------------------------------------

  (define ($cflonum/fixnum x y)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($flonum/fixnum x.rep y)
		     ($flonum/fixnum x.imp y))))

  (define ($cflonum/bignum x y)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($flonum/bignum x.rep y)
		     ($flonum/bignum x.imp y))))

  (define ($cflonum/ratnum x y)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($flonum/ratnum x.rep y)
		     ($flonum/ratnum x.imp y))))

  (define ($cflonum/flonum x y)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($flonum/flonum x.rep y)
		     ($flonum/flonum x.imp y))))

  (define ($cflonum/compnum x y)
    ;; x.rep + i * x.imp
    ;; ----------------- = z.rep + i * z.imp
    ;; y.rep + i * y.imp
    ;;
    ;;         x.rep * y.rep + x.imp * y.imp
    ;; z.rep = -----------------------------
    ;;              y.rep^2 + y.imp^2
    ;;
    ;;         x.imp * y.rep - x.rep * y.imp
    ;; z.imp = -----------------------------
    ;;              y.rep^2 + y.imp^2
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x))
	  (y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom ($fl+ ($fl* y.rep y.rep)
			 ($fl* y.imp y.imp))))
	($make-cflonum ($fl/ ($fl+ ($flonum*number x.rep y.rep)
				   ($flonum*number x.imp y.imp))
			     denom)
		       ($fl/ ($fl- ($flonum*number x.imp y.rep)
				   ($flonum*number x.rep y.imp))
			     denom)))))

  (define ($cflonum/cflonum x y)
    ;; x.rep + i * x.imp
    ;; ----------------- = z.rep + i * z.imp
    ;; y.rep + i * y.imp
    ;;
    ;;         x.rep * y.rep + x.imp * y.imp
    ;; z.rep = -----------------------------
    ;;              y.rep^2 + y.imp^2
    ;;
    ;;         x.imp * y.rep - x.rep * y.imp
    ;; z.imp = -----------------------------
    ;;              y.rep^2 + y.imp^2
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x))
	  (y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom ($fl+ ($fl* y.rep y.rep)
			 ($fl* y.imp y.imp))))
	($make-rectangular ($fl/ ($fl+ ($fl* x.rep y.rep)
				       ($fl* x.imp y.imp))
				 denom)
			   ($fl/ ($fl- ($fl* x.imp y.rep)
				       ($fl* x.rep y.imp))
				 denom)))))

;;; --------------------------------------------------------------------

  ;;These were in the original Ikarus code.
  ;;
  ;;(define (%number/complex-number x y)
  ;;   (let ((y.rep (real-part y))
  ;; 	  (y.imp (imag-part y)))
  ;;     (let ((denom (binary+ (binary* y.rep y.rep)
  ;; 			    (binary* y.imp y.imp))))
  ;; 	($make-rectangular (binary/ (binary* x y.rep)
  ;; 				    denom)
  ;; 			   (binary/ (binary* (unary- x) y.imp)
  ;; 				    denom)))))
  ;;
  ;;(define (%complex-number/number x y)
  ;;   (let ((x.rep (real-part x))
  ;; 	  (x.imp (imag-part x)))
  ;;     ($make-rectangular (binary/ x.rep y)
  ;; 			 (binary/ x.imp y))))
  ;;
  ;;(define (%complex-number/complex-number x y)
  ;;   (let ((x.rep (real-part x))
  ;; 	  (x.imp (imag-part x))
  ;; 	  (y.rep (real-part y))
  ;; 	  (y.imp (imag-part y)))
  ;;     (let ((denom (binary+ (binary* y.rep y.rep)
  ;; 			    (binary* y.imp y.imp))))
  ;; 	($make-rectangular (binary/ (binary+ (binary* x.rep y.rep)
  ;; 					     (binary* x.imp y.imp))
  ;; 				    denom)
  ;; 			   (binary/ (binary- (binary* x.imp y.rep)
  ;; 					     (binary* x.rep y.imp))
  ;; 				    denom)))))

  #| end of module: binary/ |# )


;;;; common bitwise operations

(let-syntax
    ((define-bitwise-operation
       (syntax-rules ()
	 ((_ ?who ?binary-who)
	  (define ?who
	    (case-lambda
	     ((x y)
	      (?binary-who x y))
	     ((x y z)
	      (?binary-who (?binary-who x y) z))
	     ((a)
	      (cond ((fixnum? a)
		     a)
		    ((bignum? a) a)
		    (else
		     (assertion-violation (quote ?who)
		       "expected number as argument" a))))
	     (()
	      -1)
	     ((a b c d . e*)
	      (let loop ((ac (?binary-who a (?binary-who b (?binary-who c d))))
			 (e* e*))
		(if (null? e*)
		    ac
		  (loop (?binary-who ac ($car e*))
			($cdr e*)))))))
	  ))))
  (define-bitwise-operation bitwise-and binary-bitwise-and)
  (define-bitwise-operation bitwise-ior binary-bitwise-ior)
  (define-bitwise-operation bitwise-xor binary-bitwise-xor))

(module (bitwise-not
	 $fixnum-bitwise-not
	 $bignum-bitwise-not)
  (define who 'bitwise-not)

  (define (bitwise-not x)
    (cond-exact-integer-operand x
      ((fixnum?)	($fixnum-bitwise-not x))
      ((bignum?)	($bignum-bitwise-not x))
      (else
       (assertion-violation who "expected exact integer as argument" x))))

  (define ($fixnum-bitwise-not x)
    ($fxlognot x))

  (define ($bignum-bitwise-not x)
    (foreign-call "ikrt_bnlognot" x))

  #| end of module: bitwise-not |# )


(module (binary-bitwise-and
	 $bitwise-and-fixnum-number	$bitwise-and-bignum-number
	 $bitwise-and-fixnum-fixnum	$bitwise-and-fixnum-bignum
	 $bitwise-and-bignum-fixnum	$bitwise-and-bignum-bignum)
  (define who 'bitwise-and)

  (define (binary-bitwise-and x y)
    (cond-exact-integer-operand x
      ((fixnum?)	($bitwise-and-fixnum-number x y))
      ((bignum?)	($bitwise-and-bignum-number x y))
      (else
       (%error-expected-integer x))))

;;; --------------------------------------------------------------------

  (define ($bitwise-and-fixnum-number x y)
    (cond-exact-integer-operand y
      ((fixnum?)	($bitwise-and-fixnum-fixnum x y))
      ((bignum?)	($bitwise-and-fixnum-bignum x y))
      (else
       (%error-expected-integer y))))

  (define ($bitwise-and-bignum-number x y)
    (cond-exact-integer-operand y
      ((fixnum?)	($bitwise-and-bignum-fixnum x y))
      ((bignum?)	($bitwise-and-bignum-bignum x y))
      (else
       (%error-expected-integer y))))

;;; --------------------------------------------------------------------

  (define ($bitwise-and-fixnum-fixnum x y)
    ($fxlogand x y))

  (define ($bitwise-and-fixnum-bignum x y)
    (foreign-call "ikrt_fxbnlogand" x y))

;;; --------------------------------------------------------------------

  (define ($bitwise-and-bignum-fixnum x y)
    (foreign-call "ikrt_fxbnlogand" y x))

  (define ($bitwise-and-bignum-bignum x y)
    (foreign-call "ikrt_bnbnlogand" x y))

;;; --------------------------------------------------------------------

  (define (%error-expected-integer x)
    (assertion-violation who "expected exact integer as argument" x))

  #| end of module: binary-bitwise-and |# )


(module (binary-bitwise-ior
	 $bitwise-ior-fixnum-number	$bitwise-ior-bignum-number
	 $bitwise-ior-fixnum-fixnum	$bitwise-ior-fixnum-bignum
	 $bitwise-ior-bignum-fixnum	$bitwise-ior-bignum-bignum)
  (define who 'bitwise-ior)

  (define (binary-bitwise-ior x y)
    (cond-exact-integer-operand x
      ((fixnum?)	($bitwise-ior-fixnum-number x y))
      ((bignum?)	($bitwise-ior-bignum-number x y))
      (else
       (%error-expected-integer x))))

;;; --------------------------------------------------------------------

  (define ($bitwise-ior-fixnum-number x y)
    (cond-exact-integer-operand y
      ((fixnum?)	($bitwise-ior-fixnum-fixnum x y))
      ((bignum?)	($bitwise-ior-fixnum-bignum x y))
      (else
       (%error-expected-integer y))))

  (define ($bitwise-ior-bignum-number x y)
    (cond-exact-integer-operand y
      ((fixnum?)	($bitwise-ior-bignum-fixnum x y))
      ((bignum?)	($bitwise-ior-bignum-bignum x y))
      (else
       (%error-expected-integer y))))

;;; --------------------------------------------------------------------

  (define ($bitwise-ior-fixnum-fixnum x y)
    ($fxlogor x y))

  (define ($bitwise-ior-fixnum-bignum x y)
    (foreign-call "ikrt_fxbnlogor" x y))

;;; --------------------------------------------------------------------

  (define ($bitwise-ior-bignum-fixnum x y)
    (foreign-call "ikrt_fxbnlogor" y x))

  (define ($bitwise-ior-bignum-bignum x y)
    (foreign-call "ikrt_bnbnlogor" x y))

;;; --------------------------------------------------------------------

  (define (%error-expected-integer x)
    (assertion-violation who "expected exact integer as argument" x))

  #| end of module: binary-bitwise-ior |# )


(module (binary-bitwise-xor
	 $bitwise-xor-fixnum-number	$bitwise-xor-bignum-number
	 $bitwise-xor-fixnum-fixnum	$bitwise-xor-fixnum-bignum
	 $bitwise-xor-bignum-fixnum	$bitwise-xor-bignum-bignum)
  (define who 'bitwise-xor)

  (define (binary-bitwise-xor x y)
    (cond-exact-integer-operand x
      ((fixnum?)	($bitwise-xor-fixnum-number x y))
      ((bignum?)	($bitwise-xor-bignum-number x y))
      (else
       (%error-expected-integer x))))

;;; --------------------------------------------------------------------

  (define ($bitwise-xor-fixnum-number x y)
    (cond-exact-integer-operand y
      ((fixnum?)	($bitwise-xor-fixnum-fixnum x y))
      ((bignum?)	($bitwise-xor-fixnum-bignum x y))
      (else
       (%error-expected-integer y))))

  (define ($bitwise-xor-bignum-number x y)
    (cond-exact-integer-operand y
      ((fixnum?)	($bitwise-xor-bignum-fixnum x y))
      ((bignum?)	($bitwise-xor-bignum-bignum x y))
      (else
       (%error-expected-integer y))))

;;; --------------------------------------------------------------------

  (define ($bitwise-xor-fixnum-fixnum x y)
    ($fxlogxor x y))

  (define ($bitwise-xor-fixnum-bignum x y)
    (let* ((D  ($fx- (fixnum-width) 1))
	   (y0 (bitwise-and y (greatest-fixnum)))
	   (y1 (bitwise-arithmetic-shift-right y D)))
      (bitwise-ior ($fxlogand ($fxlogxor x y0) (greatest-fixnum))
		   (bitwise-arithmetic-shift-left
		    (bitwise-arithmetic-shift-right (if ($fx>= x 0) y (bitwise-not y))
						    D)
		    D))))

;;; --------------------------------------------------------------------

  (define ($bitwise-xor-bignum-fixnum x y)
    ($bitwise-xor-fixnum-bignum y x))

  (define ($bitwise-xor-bignum-bignum x y)
    (let* ((D  ($fx- (fixnum-width) 1))
	   (x0 (bitwise-and x (greatest-fixnum)))
	   (y0 (bitwise-and y (greatest-fixnum)))
	   (x1 (bitwise-arithmetic-shift-right x D))
	   (y1 (bitwise-arithmetic-shift-right y D)))
      (bitwise-ior ($fxlogand ($fxlogxor x0 y0) (greatest-fixnum))
		   (bitwise-arithmetic-shift-left (binary-bitwise-xor x1 y1)
						  D))))

;;; --------------------------------------------------------------------

  (define (%error-expected-integer x)
    (assertion-violation who "expected exact integer as argument" x))

  #| end of module: binary-bitwise-xor |# )


;;;; uncommon bitwise operations

(define (bitwise-if x y z)
  (define who 'bitwise-if)
  (with-arguments-validation (who)
      ((exact-integer	x)
       (exact-integer	y)
       (exact-integer	z))
    (bitwise-ior (bitwise-and x y)
		 (bitwise-and (bitwise-not x) z))))

(module (bitwise-copy-bit-field
	 bitwise-reverse-bit-field
	 bitwise-rotate-bit-field)

  (define (bitwise-copy-bit-field x i j n)
    (define who 'bitwise-copy-bit-field)
    (with-arguments-validation (who)
	((exact-integer		x)
	 (exact-integer		i)
	 (exact-integer		j)
	 (exact-integer		n)
	 (index-order		i j))
      (bitwise-if (sll (sub1 (sll 1 (- j i))) i)
		  (sll n i)
		  x)))

  (define (bitwise-reverse-bit-field N start end)
    (define who 'bitwise-reverse-bit-field)
    (with-arguments-validation (who)
	((exact-integer			N)
	 (non-negative-exact-integer	start)
	 (non-negative-exact-integer	end)
	 (index-order			start end))
      (let ((width (- end start)))
	(if (positive? width)
	    (let loop ((reversed	0)
		       (field	(bitwise-bit-field N start end))
		       (width	width))
	      (if (zero? width)
		  (bitwise-copy-bit-field N start end reversed)
		(if (zero? (bitwise-and field 1))
		    (loop (bitwise-arithmetic-shift reversed 1)
			  (bitwise-arithmetic-shift-right field 1)
			  (- width 1))
		  (loop (bitwise-ior (bitwise-arithmetic-shift reversed 1) 1)
			(bitwise-arithmetic-shift-right field 1)
			(- width 1)))))
	  N))))

  (define (bitwise-rotate-bit-field N start end count)
    (define who 'bitwise-rotate-bit-field)
    (with-arguments-validation (who)
	((exact-integer			N)
	 (non-negative-exact-integer	start)
	 (non-negative-exact-integer	end)
	 (non-negative-exact-integer	count)
	 (index-order			start end))
      (let ((width (- end start)))
	(if (positive? width)
	    (let* ((count  (mod count width))
		   (field0 (bitwise-bit-field N start end))
		   (field1 (bitwise-arithmetic-shift-left field0 count))
		   (field2 (bitwise-arithmetic-shift-right field0 (- width count)))
		   (field  (bitwise-ior field1 field2)))
	      (bitwise-copy-bit-field N start end field))
	  N))))

  (define-argument-validation (index-order who i j)
    (<= i j)
    (assertion-violation who "indexes must be in nondescending order" i j))

  #| end of module |# )


(module (gcd
	 binary-gcd
	 $gcd-fixnum-number	$gcd-bignum-number
	 $gcd-number-fixnum	$gcd-number-bignum
	 $gcd-fixnum-fixnum	$gcd-fixnum-bignum
	 $gcd-bignum-fixnum	$gcd-bignum-bignum)
  (define who 'gcd)

  (define gcd
    (case-lambda
     ((x y)
      (cond-exact-integer-operand x
	((fixnum?)	($gcd-fixnum-number x y))
	((bignum?)	($gcd-bignum-number x y))
	(else
	 (%error-not-exact-integer x))))

     ((x)
      (cond-exact-integer-operand x
	((fixnum?)	x)
	((bignum?)	x)
	(else
	 (%error-not-exact-integer x))))

     (() 0)

     ((x y z . ls)
      (let loop ((g  (gcd (gcd x y) z))
		 (ls ls))
	(if (null? ls)
	    g
	  (loop (gcd g ($car ls))
		($cdr ls)))))
     ))

;;; --------------------------------------------------------------------

  (define ($gcd-fixnum-number x y)
    (cond-exact-integer-operand y
      ((fixnum?)	($gcd-fixnum-fixnum x y))
      ((bignum?)	($gcd-fixnum-bignum x y))
      (else
       (%error-not-exact-integer y))))

  (define ($gcd-bignum-number x y)
    (cond-exact-integer-operand y
      ((fixnum?)	($gcd-bignum-fixnum x y))
      ((bignum?)	($gcd-bignum-bignum x y))
      (else
       (%error-not-exact-integer y))))

;;; --------------------------------------------------------------------

  (define ($gcd-number-fixnum x y)
    (cond-exact-integer-operand x
      ((fixnum?)	($gcd-fixnum-fixnum x y))
      ((bignum?)	($gcd-bignum-fixnum x y))
      (else
       (%error-not-exact-integer x))))

  (define ($gcd-number-bignum x y)
    (cond-exact-integer-operand x
      ((fixnum?)	($gcd-fixnum-bignum x y))
      ((bignum?)	($gcd-bignum-bignum x y))
      (else
       (%error-not-exact-integer x))))

;;; --------------------------------------------------------------------

  (define ($gcd-fixnum-fixnum x y)
    (binary-gcd x y))

  (define ($gcd-fixnum-bignum x y)
    (binary-gcd x y))

;;; --------------------------------------------------------------------

  (define ($gcd-bignum-fixnum x y)
    (binary-gcd x y))

  (define ($gcd-bignum-bignum x y)
    (binary-gcd x y))

;;; --------------------------------------------------------------------

  (module (binary-gcd)

    (define (binary-gcd x y)
      (let ((x (if (< x 0) (- x) x))
	    (y (if (< y 0) (- y) y)))
	(cond ((> x y)
	       (%greatest-common-divisor x y))
	      ((< x y)
	       (%greatest-common-divisor y x))
	      (else
	       x))))

    (define (%greatest-common-divisor x y)
      (if ($fxzero? y)
	  x
	(%greatest-common-divisor y (remainder x y))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (%error-not-exact-integer x)
    (assertion-violation who "expected exact integer as argument" x))

  #| end of module |# )


(module (lcm
	 binary-lcm)
  (define who 'lcm)

  (define lcm
    (case-lambda
     ((x y)
      (binary-lcm x y))
     ((x)
      (unary-lcm x))
     (() 1)
     ((x y z . ls)
      ;;FIXME Incorrect for multiple roundings.  (Abdulaziz Ghuloum)
      (let loop ((g  (binary-lcm (binary-lcm x y) z))
		 (ls ls))
	(if (null? ls)
	    g
	  (loop (binary-lcm g ($car ls))
		($cdr ls)))))
     ))

;;; --------------------------------------------------------------------

  (define (unary-lcm x)
    (cond-numeric-operand x
      ((fixnum?)	x)
      ((bignum?)	x)
      ((flonum?)
       (let ((v ($flonum->exact x)))
	 (cond-exact-integer-operand v
	   ((fixnum?)	x)
	   ((bignum?)	x)
	   (else
	    (%error-not-integer x)))))
      ((ratnum?)	(%error-not-integer x))
      ((compnum?)	(%error-not-integer x))
      ((cflonum?)	(%error-not-integer x))
      (else
       (%error-not-exact-integer x))))

  (define (binary-lcm x y)
    (cond-numeric-operand x
      ((fixnum?)	($lcm-fixnum-number x y))
      ((bignum?)	($lcm-bignum-number x y))
      ((flonum?)	($lcm-flonum-number x y))
      ((ratnum?)	(%error-not-integer x))
      ((compnum?)	(%error-not-integer x))
      ((cflonum?)	(%error-not-integer x))
      (else
       (%error-not-integer x))))

;;; --------------------------------------------------------------------

  (define ($lcm-fixnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($lcm-fixnum-fixnum x y))
      ((bignum?)	($lcm-fixnum-bignum x y))
      ((flonum?)	($lcm-fixnum-flonum x y))
      ((ratnum?)	(%error-not-integer y))
      ((compnum?)	(%error-not-integer y))
      ((cflonum?)	(%error-not-integer y))
      (else
       (%error-not-integer y))))

  (define ($lcm-bignum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($lcm-bignum-fixnum x y))
      ((bignum?)	($lcm-bignum-bignum x y))
      ((flonum?)	($lcm-bignum-flonum x y))
      ((ratnum?)	(%error-not-integer y))
      ((compnum?)	(%error-not-integer y))
      ((cflonum?)	(%error-not-integer y))
      (else
       (%error-not-integer y))))

  (define ($lcm-flonum-number x y)
    (let ((v ($flonum->exact x)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact (lcm v y)))
	((bignum?)	(inexact (lcm v y)))
	(else
	 (%error-not-integer x)))))

;;; --------------------------------------------------------------------

  (define ($lcm-fixnum-fixnum x y)
    (let ((x (if (< x 0) (- x) x))
	  (y (if (< y 0) (- y) y)))
      (let ((g (binary-gcd x y)))
	(binary* y (quotient x g)))))

  (define ($lcm-fixnum-bignum x y)
    (let ((x (if (< x 0) (- x) x))
	  (y (if (< y 0) (- y) y)))
      (let ((g (binary-gcd x y)))
	(binary* y (quotient x g)))))

  (define ($lcm-fixnum-flonum x y)
    (let ((v ($flonum->exact y)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact (lcm x v)))
	((bignum?)	(inexact (lcm x v)))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define ($lcm-bignum-fixnum x y)
    (let ((x (if (< x 0) (- x) x))
	  (y (if (< y 0) (- y) y)))
      (let ((g (binary-gcd x y)))
	(binary* y (quotient x g)))))

  (define ($lcm-bignum-bignum x y)
    (let ((x (if (< x 0) (- x) x))
	  (y (if (< y 0) (- y) y)))
      (let ((g (binary-gcd x y)))
	(binary* y (quotient x g)))))

  (define ($lcm-bignum-flonum x y)
    (let ((v ($flonum->exact y)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact (lcm x v)))
	((bignum?)	(inexact (lcm x v)))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define (%error-not-integer x)
    (assertion-violation who "expected integer as argument" x))

  (define (%error-not-exact-integer x)
    (assertion-violation who "expected exact integer as argument" x))

  #| end of module |# )


(module (max
	 $max-fixnum-number $max-bignum-number $max-flonum-number $max-ratnum-number
	 $max-number-fixnum $max-number-bignum $max-number-flonum $max-number-ratnum
	 $max-fixnum-fixnum $max-fixnum-bignum $max-fixnum-flonum $max-fixnum-ratnum
	 $max-bignum-fixnum $max-bignum-bignum $max-bignum-flonum $max-bignum-ratnum
	 $max-flonum-flonum $max-flonum-fixnum $max-flonum-bignum $max-flonum-ratnum
	 $max-ratnum-fixnum $max-ratnum-bignum $max-ratnum-ratnum $max-ratnum-flonum)
  (define who 'max)

  (define max
    (case-lambda
     ((x y)
      (%binary-max x y))
     ((x y z . rest)
      (let loop ((a  (%binary-max x y))
		 (b  z)
		 (ls rest))
	(if (null? ls)
	    (%binary-max a b)
	  (loop (%binary-max a b)
		($car ls)
		($cdr ls)))))
     ((x)
      (if (or (fixnum? x)
	      (bignum? x)
	      (ratnum? x)
	      (flonum? x))
	  x
	(%error-not-real-number x)))
     ))

;;; --------------------------------------------------------------------

  (define (%binary-max x y)
    (cond-real-numeric-operand x
      ((fixnum?)	($max-fixnum-number x y))
      ((bignum?)	($max-bignum-number x y))
      ((flonum?)	($max-flonum-number x y))
      ((ratnum?)	($max-ratnum-number x y))
      (else
       (%error-not-real-number x))))

;;; --------------------------------------------------------------------

  (define ($max-fixnum-number x y)
    (cond-real-numeric-operand y
      ((fixnum?)	($max-fixnum-fixnum x y))
      ((bignum?)	($max-fixnum-bignum x y))
      ((flonum?)	($max-fixnum-flonum x y))
      ((ratnum?)	($max-fixnum-ratnum x y))
      (else
       (%error-not-real-number y))))

  (define ($max-bignum-number x y)
    (cond-real-numeric-operand y
      ((fixnum?)	($max-bignum-fixnum x y))
      ((bignum?)	($max-bignum-bignum x y))
      ((flonum?)	($max-bignum-flonum x y))
      ((ratnum?)	($max-bignum-ratnum x y))
      (else
       (%error-not-real-number y))))

  (define ($max-flonum-number x y)
    (cond-real-numeric-operand y
      ((flonum?)	($max-flonum-flonum x y))
      ((fixnum?)	($max-flonum-fixnum x y))
      ((bignum?)	($max-flonum-bignum x y))
      ((ratnum?)	($max-flonum-ratnum x y))
      (else
       (%error-not-real-number y))))

  (define ($max-ratnum-number x y)
    (cond-real-numeric-operand y
      ((fixnum?)	($max-ratnum-fixnum x y))
      ((bignum?)	($max-ratnum-bignum x y))
      ((ratnum?)	($max-ratnum-ratnum x y))
      ((flonum?)	($max-ratnum-flonum x y))
      (else
       (%error-not-real-number y))))

;;; --------------------------------------------------------------------

  (define ($max-number-fixnum x y)
    (cond-real-numeric-operand x
      ((fixnum?)	($max-fixnum-fixnum x y))
      ((bignum?)	($max-bignum-fixnum x y))
      ((flonum?)	($max-flonum-fixnum x y))
      ((ratnum?)	($max-ratnum-fixnum x y))
      (else
       (%error-not-real-number x))))

  (define ($max-number-bignum x y)
    (cond-real-numeric-operand x
      ((fixnum?)	($max-fixnum-bignum x y))
      ((bignum?)	($max-bignum-bignum x y))
      ((flonum?)	($max-flonum-bignum x y))
      ((ratnum?)	($max-ratnum-bignum x y))
      (else
       (%error-not-real-number x))))

  (define ($max-number-flonum x y)
    (cond-real-numeric-operand x
      ((flonum?)	($max-flonum-flonum x y))
      ((fixnum?)	($max-fixnum-flonum x y))
      ((bignum?)	($max-bignum-flonum x y))
      ((ratnum?)	($max-ratnum-flonum x y))
      (else
       (%error-not-real-number x))))

  (define ($max-number-ratnum x y)
    (cond-real-numeric-operand x
      ((fixnum?)	($max-fixnum-ratnum x y))
      ((bignum?)	($max-bignum-ratnum x y))
      ((ratnum?)	($max-ratnum-ratnum x y))
      ((flonum?)	($max-flonum-ratnum x y))
      (else
       (%error-not-real-number x))))

;;; --------------------------------------------------------------------

  (define ($max-fixnum-fixnum x y)
    (if ($fx> x y)
	x
      y))

  (define ($max-fixnum-bignum x y)
    (if ($bignum-positive? y)
	y
      x))

  (define ($max-fixnum-flonum x y)
    (let ((x ($fixnum->flonum x)))
      (if ($fl>= y x)
	  y
	x)))

  (define ($max-fixnum-ratnum x y)
    (if (>= x y)
	x
      y))

;;; --------------------------------------------------------------------

  (define ($max-bignum-fixnum x y)
    (if ($bignum-positive? x)
	x
      y))

  (define ($max-bignum-bignum x y)
    (if (bnbn> x y)
	x
      y))

  (define ($max-bignum-flonum x y)
    (let ((x ($bignum->flonum x)))
      (if ($fl>= y x)
	  y
	x)))

  (define ($max-bignum-ratnum x y)
    (if (>= x y)
	x
      y))

;;; --------------------------------------------------------------------

  (define ($max-flonum-flonum x y)
    (if ($fl>= x y)
	x
      y))

  (define ($max-flonum-fixnum x y)
    (let ((y ($fixnum->flonum y)))
      (if ($fl>= y x)
	  y
	x)))

  (define ($max-flonum-bignum x y)
    (let ((y ($bignum->flonum y)))
      (if ($fl>= y x)
	  y
	x)))

  (define ($max-flonum-ratnum x y)
    ;;FIXME May be incorrect.  (Abdulaziz Ghuloum)
    (let ((y ($ratnum->flonum y)))
      (if ($fl>= y x)
	  y
	x)))

;;; --------------------------------------------------------------------

  (define ($max-ratnum-fixnum x y)
    (if (>= x y)
	x
      y))

  (define ($max-ratnum-bignum x y)
    (if (>= x y)
	x
      y))

  (define ($max-ratnum-ratnum x y)
    (if (>= x y)
	x
      y))

  (define ($max-ratnum-flonum x y)
    (let ((x ($ratnum->flonum x)))
      (if ($fl>= x y)
	  x
	y)))

;;; --------------------------------------------------------------------

  (define (%error-not-real-number x)
    (assertion-violation who "expected real number as argument" x))

  #| end of module: max |# )


(module (min
	 $min-fixnum-number $min-bignum-number $min-flonum-number $min-ratnum-number
	 $min-number-fixnum $min-number-bignum $min-number-flonum $min-number-ratnum
	 $min-fixnum-fixnum $min-fixnum-bignum $min-fixnum-flonum $min-fixnum-ratnum
	 $min-bignum-fixnum $min-bignum-bignum $min-bignum-flonum $min-bignum-ratnum
	 $min-flonum-flonum $min-flonum-fixnum $min-flonum-bignum $min-flonum-ratnum
	 $min-ratnum-fixnum $min-ratnum-bignum $min-ratnum-ratnum $min-ratnum-flonum)
  (define who 'min)

  (define min
    (case-lambda
     ((x y)
      (%binary-min x y))
     ((x y z . rest)
      (let loop ((a  (%binary-min x y))
		 (b  z)
		 (ls rest))
	(if (null? ls)
	    (%binary-min a b)
	  (loop (%binary-min a b)
		($car ls)
		($cdr ls)))))
     ((x)
      (if (or (fixnum? x)
	      (bignum? x)
	      (ratnum? x)
	      (flonum? x))
	  x
	(%error-not-real-number x)))
     ))

;;; --------------------------------------------------------------------

  (define (%binary-min x y)
    (cond-real-numeric-operand x
      ((fixnum?)	($min-fixnum-number x y))
      ((bignum?)	($min-bignum-number x y))
      ((flonum?)	($min-flonum-number x y))
      ((ratnum?)	($min-ratnum-number x y))
      (else
       (%error-not-real-number x))))

;;; --------------------------------------------------------------------

  (define ($min-fixnum-number x y)
    (cond-real-numeric-operand y
      ((fixnum?)	($min-fixnum-fixnum x y))
      ((bignum?)	($min-fixnum-bignum x y))
      ((flonum?)	($min-fixnum-flonum x y))
      ((ratnum?)	($min-fixnum-ratnum x y))
      (else
       (%error-not-real-number y))))

  (define ($min-bignum-number x y)
    (cond-real-numeric-operand y
      ((fixnum?)	($min-bignum-fixnum x y))
      ((bignum?)	($min-bignum-bignum x y))
      ((flonum?)	($min-bignum-flonum x y))
      ((ratnum?)	($min-bignum-ratnum x y))
      (else
       (%error-not-real-number y))))

  (define ($min-flonum-number x y)
    (cond-real-numeric-operand y
      ((flonum?)	($min-flonum-flonum x y))
      ((fixnum?)	($min-flonum-fixnum x y))
      ((bignum?)	($min-flonum-bignum x y))
      ((ratnum?)	($min-flonum-ratnum x y))
      (else
       (%error-not-real-number y))))

  (define ($min-ratnum-number x y)
    (cond-real-numeric-operand y
      ((fixnum?)	($min-ratnum-fixnum x y))
      ((bignum?)	($min-ratnum-bignum x y))
      ((ratnum?)	($min-ratnum-ratnum x y))
      ((flonum?)	($min-ratnum-flonum x y))
      (else
       (%error-not-real-number y))))

;;; --------------------------------------------------------------------

  (define ($min-number-fixnum x y)
    (cond-real-numeric-operand x
      ((fixnum?)	($min-fixnum-fixnum x y))
      ((bignum?)	($min-bignum-fixnum x y))
      ((flonum?)	($min-flonum-fixnum x y))
      ((ratnum?)	($min-ratnum-fixnum x y))
      (else
       (%error-not-real-number x))))

  (define ($min-number-bignum x y)
    (cond-real-numeric-operand x
      ((fixnum?)	($min-fixnum-bignum x y))
      ((bignum?)	($min-bignum-bignum x y))
      ((flonum?)	($min-flonum-bignum x y))
      ((ratnum?)	($min-ratnum-bignum x y))
      (else
       (%error-not-real-number x))))

  (define ($min-number-flonum x y)
    (cond-real-numeric-operand x
      ((flonum?)	($min-flonum-flonum x y))
      ((fixnum?)	($min-fixnum-flonum x y))
      ((bignum?)	($min-bignum-flonum x y))
      ((ratnum?)	($min-ratnum-flonum x y))
      (else
       (%error-not-real-number x))))

  (define ($min-number-ratnum x y)
    (cond-real-numeric-operand x
      ((fixnum?)	($min-fixnum-ratnum x y))
      ((bignum?)	($min-bignum-ratnum x y))
      ((ratnum?)	($min-ratnum-ratnum x y))
      ((flonum?)	($min-flonum-ratnum x y))
      (else
       (%error-not-real-number x))))

;;; --------------------------------------------------------------------

  (define ($min-fixnum-fixnum x y)
    (if ($fx< x y)
	x
      y))

  (define ($min-fixnum-bignum x y)
    (if ($bignum-negative? y)
	y
      x))

  (define ($min-fixnum-flonum x y)
    (let ((x ($fixnum->flonum x)))
      (if ($fl<= y x)
	  y
	x)))

  (define ($min-fixnum-ratnum x y)
    (if (<= x y)
	x
      y))

;;; --------------------------------------------------------------------

  (define ($min-bignum-fixnum x y)
    (if ($bignum-negative? x)
	x
      y))

  (define ($min-bignum-bignum x y)
    (if (bnbn< x y)
	x
      y))

  (define ($min-bignum-flonum x y)
    (let ((x ($bignum->flonum x)))
      (if ($fl<= y x)
	  y
	x)))

  (define ($min-bignum-ratnum x y)
    (if (<= x y)
	x
      y))

;;; --------------------------------------------------------------------

  (define ($min-flonum-flonum x y)
    (if ($fl<= x y)
	x
      y))

  (define ($min-flonum-fixnum x y)
    (let ((y ($fixnum->flonum y)))
      (if ($fl<= y x)
	  y
	x)))

  (define ($min-flonum-bignum x y)
    (let ((y ($bignum->flonum y)))
      (if ($fl<= y x)
	  y
	x)))

  (define ($min-flonum-ratnum x y)
    ;;FIXME May be incorrect.  (Abdulaziz Ghuloum)
    (let ((y ($ratnum->flonum y)))
      (if ($fl<= y x)
	  y
	x)))

;;; --------------------------------------------------------------------

  (define ($min-ratnum-fixnum x y)
    (if (<= x y)
	x
      y))

  (define ($min-ratnum-bignum x y)
    (if (<= x y)
	x
      y))

  (define ($min-ratnum-ratnum x y)
    (if (<= x y)
	x
      y))

  (define ($min-ratnum-flonum x y)
    (let ((x ($ratnum->flonum x)))
      (if ($fl<= x y)
	  x
	y)))

;;; --------------------------------------------------------------------

  (define (%error-not-real-number x)
    (assertion-violation who "expected real number as argument" x))

  #| end of module: min |# )


(module (abs
	 $bignum-abs		$flonum-abs		$ratnum-abs)
  (define who 'abs)

  (define (abs x)
    (cond-numeric-operand x
      ((fixnum?)	($fxabs x))
      ((bignum?)	($bignum-abs x))
      ((flonum?)	($flonum-abs x))
      ((ratnum?)	($ratnum-abs x))
      ((compnum?)	(%error-not-real-number x))
      ((cflonum?)	(%error-not-real-number x))
      (else
       (%error-not-real-number x))))

;;; --------------------------------------------------------------------

  (define ($bignum-abs x)
    (if ($bignum-positive? x)
	x
      ($bignum- x)))

  (define ($flonum-abs x)
    (if ($fx> ($flonum-u8-ref x 0) 127)
	;;We must also handle corrently -0.0, that is why we use $FX*.
	($fl* x -1.0)
      x))

  (define ($ratnum-abs x)
    (let ((x.num ($ratnum-n x)))
      (if (negative? x.num)
	  ($make-ratnum (- x.num) ($ratnum-d x))
	x)))

;;; --------------------------------------------------------------------

  (define (%error-not-real-number x)
    (assertion-violation who "not a real number" x))

  #| end of module: abs |# )


(module (inexact
	 exact->inexact)

  (define (exact->inexact x)
    (->inexact x 'exact->inexact))

  (define (inexact x)
    (->inexact x 'inexact))

  (define (->inexact x who)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum->flonum x))
      ((bignum?)	($bignum->flonum x))
      ((ratnum?)	($ratnum->flonum x))
      ((flonum?)	x)
      ((compnum?)	($make-rectangular (->inexact ($compnum-real x) who)
					   (->inexact ($compnum-imag x) who)))
      ((cflonum?)	x)
      (else
       (assertion-violation who "not a number" x))))

  #| end of module |# )


(module (even?)
  (define who 'even?)

  (define (even? x)
    (cond-numeric-operand x
      ((fixnum?)	($fxeven? x))
      ((bignum?)	($bignum-even? x))
      ((flonum?)	($fleven? x))
      ((ratnum?)	(%error-not-integer x))
      ((compnum?)	(%error-not-integer x))
      ((cflonum?)	(%error-not-integer x))
      (else
       (%error-not-integer x))))

  (define (%error-not-integer x)
    (assertion-violation who "expected integer as argument" x))

  #| end of module: even? |# )

(module (odd?)
  (define who 'odd?)

  (define (odd? x)
    (cond-numeric-operand x
      ((fixnum?)	($fxodd? x))
      ((bignum?)	($bignum-odd? x))
      ((flonum?)	($flodd? x))
      ((ratnum?)	(%error-not-integer x))
      ((compnum?)	(%error-not-integer x))
      ((cflonum?)	(%error-not-integer x))
      (else
       (%error-not-integer x))))

  (define (%error-not-integer x)
    (assertion-violation who "expected integer as argument" x))

  #| end of module: odd? |# )


(module (number->string)

  (define who 'number->string)

  (module (number->string)

    (define number->string
      (case-lambda
       ((x)
	($number->string x 10))
       ((x r)
	(with-arguments-validation (who)
	    ((radix	r))
	  ($number->string x r)))
       ((x r precision)
	;;(%do-warn)
	(number->string x r))))

    (define (%do-warn)
      ;;Overwrite the binding so that the warning is raised only once.
      (set! %do-warn values)
      (raise-continuable
       (condition (make-warning)
		  (make-who-condition who)
		  (make-message-condition "precision argument is not supported"))))

    (define-argument-validation (radix who obj)
      (memv obj '(2 8 10 16))
      (assertion-violation who "invalid radix" obj))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module ($number->string)

    (define ($number->string x r)
      (import (ikarus system $compnums))
      (cond-numeric-operand x
	((fixnum?)
	 (fixnum->string x r))

	((bignum?)
	 (bignum->string x r))

	((flonum?)
	 (if (eqv? r 10)
	     (flonum->string x)
	   (assertion-violation who "invalid radix for inexact number" r x)))

	((ratnum?)
	 (ratnum->string x r))

	((compnum?)
	 (let ((x.rep ($compnum-real x))
	       (x.imp ($compnum-imag x)))
	   (if (and (fixnum?  x.rep)
		    ($fxzero? x.rep))
	       (string-append (imag x.imp r) "i")
	     (string-append ($number->string x.rep r) (imag x.imp r) "i"))))

	((cflonum?)
	 (let ((x.rep ($cflonum-real x))
	       (x.imp ($cflonum-imag x)))
	   (cond ((flnan? x.imp)
		  (string-append ($number->string x.rep r) "+nan.0i"))
		 ((flinfinite? x.imp)
		  (string-append ($number->string x.rep r) (if ($fl> x.imp 0.0)
							       "+inf.0i"
							     "-inf.0i")))
		 (else
		  (string-append ($number->string x.rep r) (imag x.imp r) "i")))))

	(else
	 (assertion-violation who "expected number as argument" x))))

    (define (imag x radix)
      ;;Compose a string for the imaginary part of a cflonum or compnum.
      ;;X can be any real number, including infinities and NaN.
      ;;
      (cond
       ;;Special case to allow printing: "Rep+i".
       ((and (fixnum? x)
	     ($fx= x +1))
	"+")
       ;;Special case to allow printing: "Rep-i".
       ((and (fixnum? x)
	     ($fx= x -1))
	"-")
       ;;If X is  negative: omit the sign here, a  negative sign will be
       ;;inserted by $NUMBER->STRING.
       ((or (< x 0)
	    (and (flonum? x)
		 ($flzero?/negative x)))
	($number->string x radix))
       ;;If we are here X is exact zero or positive.
       ;;
       ;;If X is +inf.0 avoid prepending an additional positive sign.
       ((and (flonum? x)
	     (flinfinite? x))
	"+inf.0")
       (else
	(string-append "+" ($number->string x radix)))))

    #| end of module: $number->string |# )

;;; --------------------------------------------------------------------

  (module (bignum->string)

    (define (bignum->string x r)
      (case-fixnums r
	((10) (bignum->decimal-string x))
	((2)  (bignum->power-string x  1 1))
	((8)  (bignum->power-string x  7 3))
	((16) (bignum->power-string x 15 4))
	(else
	 (assertion-violation who "BUG"))))

    (define (bignum->decimal-string x)
      (utf8->string (foreign-call "ikrt_bignum_to_bytevector" x)))

    (module (bignum->power-string)
      (define string-map "0123456789ABCDEF")

      (define (init-string x chars)
	(if ($bignum-positive? x)
	    (make-string chars)
	  (let ((s (make-string ($fxadd1 chars))))
	    (string-set! s 0 #\-)
	    s)))

      (define (bignum-bits x)
	(define (add-bits b n)
	  (cond
	   (($fxzero? b) n)
	   (else (add-bits ($fxsra b 1) ($fx+ n 1)))))
	(let f ((i ($fxsub1 ($bignum-size x))))
	  (let ((b ($bignum-byte-ref x i)))
	    (cond
	     (($fxzero? b) (f ($fxsub1 i)))
	     (else (add-bits b ($fxsll i 3)))))))

      (define (bignum->power-string x mask shift)
	(let ((bits (bignum-bits x)))
	  (let ((chars (fxquotient (fx+ bits (fx- shift 1)) shift)))
	    (let* ((s (init-string x chars))
		   (n ($fx- (string-length s) 1)))
	      (let f ((i 0) (j 0) (k 0) (b 0))
		(cond
		 (($fx= i chars) s)
		 (($fx< k 8)
		  (f i ($fxadd1 j) ($fx+ k 8)
		     ($fxlogor b
			       ($fxsll ($bignum-byte-ref x j) k))))
		 (else
		  (string-set! s ($fx- n i)
			       (string-ref string-map
					   ($fxlogand mask b)))
		  (f ($fxadd1 i) j ($fx- k shift) ($fxsra b shift))))))))))

    #| end of module: bignum->string |# )

  (define (ratnum->string x r)
    (string-append ($number->string ($ratnum-n x) r)
		   "/"
		   ($number->string ($ratnum-d x) r)))

  #| end of module: number->string |# )


(module (modulo
	 $modulo-fixnum-number	$modulo-bignum-number	$modulo-flonum-number
	 $modulo-number-fixnum	$modulo-number-bignum	$modulo-number-flonum
	 $modulo-fixnum-fixnum	$modulo-fixnum-bignum	$modulo-fixnum-flonum
	 $modulo-bignum-fixnum	$modulo-bignum-bignum	$modulo-bignum-flonum
	 $modulo-flonum-fixnum	$modulo-flonum-bignum	$modulo-flonum-flonum)
  (define who 'modulo)

  (define (modulo n m)
    (cond-numeric-operand n
      ((fixnum?)	($modulo-fixnum-number n m))
      ((bignum?)	($modulo-bignum-number n m))
      ((flonum?)	($modulo-flonum-number n m))
      ((ratnum?)	(%error-not-integer n))
      ((compnum?)	(%error-not-integer n))
      ((cflonum?)	(%error-not-integer n))
      (else
       (%error-not-integer n))))

;;; --------------------------------------------------------------------

  (define ($modulo-fixnum-number n m)
    (cond-numeric-operand m
      ((fixnum?)	($modulo-fixnum-fixnum n m))
      ((bignum?)	($modulo-fixnum-bignum n m))
      ((flonum?)	($modulo-fixnum-flonum n m))
      ((ratnum?)	(%error-not-integer m))
      ((compnum?)	(%error-not-integer m))
      ((cflonum?)	(%error-not-integer m))
      (else
       (%error-not-integer m))))

  (define ($modulo-bignum-number n m)
    (cond-numeric-operand m
      ((fixnum?)	($modulo-bignum-fixnum n m))
      ((bignum?)	($modulo-bignum-bignum n m))
      ((flonum?)	($modulo-bignum-flonum n m))
      ((ratnum?)	(%error-not-integer m))
      ((compnum?)	(%error-not-integer m))
      ((cflonum?)	(%error-not-integer m))
      (else
       (%error-not-integer m))))

  (define ($modulo-flonum-number n m)
    (let ((v ($flonum->exact n)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact ($modulo-fixnum-number v m)))
	((bignum?)	(inexact ($modulo-bignum-number v m)))
	(else
	 (%error-not-integer n)))))

;;; --------------------------------------------------------------------

  (define ($modulo-number-fixnum n m)
    (cond-numeric-operand n
      ((fixnum?)	($modulo-fixnum-fixnum n m))
      ((bignum?)	($modulo-bignum-fixnum n m))
      ((flonum?)	($modulo-flonum-fixnum n m))
      ((ratnum?)	(%error-not-integer n))
      ((compnum?)	(%error-not-integer n))
      ((cflonum?)	(%error-not-integer n))
      (else
       (%error-not-integer n))))

  (define ($modulo-number-bignum n m)
    (cond-numeric-operand n
      ((fixnum?)	($modulo-fixnum-bignum n m))
      ((bignum?)	($modulo-bignum-bignum n m))
      ((flonum?)	($modulo-flonum-bignum n m))
      ((ratnum?)	(%error-not-integer n))
      ((compnum?)	(%error-not-integer n))
      ((cflonum?)	(%error-not-integer n))
      (else
       (%error-not-integer n))))

  (define ($modulo-number-flonum n m)
    (let ((v ($flonum->exact m)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact ($modulo-number-fixnum n v)))
	((bignum?)	(inexact ($modulo-number-bignum n v)))
	(else
	 (%error-not-integer m)))))

;;; --------------------------------------------------------------------

  (define ($modulo-fixnum-fixnum n m)
    (if ($fxzero? m)
	(assertion-violation who "division by zero" n m)
      ($fxmodulo n m)))

  (define ($modulo-fixnum-bignum n m)
    (if ($fxnegative? n)
	(if ($bignum-positive? m)
	    (foreign-call "ikrt_fxbnplus" n m)
	  n)
      (if ($bignum-positive? m)
	  n
	(foreign-call "ikrt_fxbnplus" n m))))

  (define ($modulo-fixnum-flonum n m)
    (let ((v ($flonum->exact m)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact ($modulo-fixnum-fixnum n v)))
	((bignum?)	(inexact ($modulo-fixnum-bignum n v)))
	(else
	 (%error-not-integer m)))))

;;; --------------------------------------------------------------------

  (define ($modulo-bignum-fixnum n m)
    (if ($fxzero? m)
	(assertion-violation who "division by zero" n m)
      (foreign-call "ikrt_bnfx_modulo" n m)))

  (define ($modulo-bignum-bignum n m)
    ;;FIXME  Use type-specific  functions.   (Marco Maggi;  Thu Nov  22,
    ;;2012)
    (if ($bignum-positive? n)
	(if ($bignum-positive? m)
	    (remainder n m)
	  (+ m (remainder n m)))
      (if ($bignum-positive? m)
	  (+ m (remainder n m))
	(remainder n m))))

  (define ($modulo-bignum-flonum n m)
    (let ((v ($flonum->exact m)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact ($modulo-bignum-fixnum n v)))
	((bignum?)	(inexact ($modulo-bignum-bignum n v)))
	(else
	 (%error-not-integer m)))))

;;; --------------------------------------------------------------------

  (define ($modulo-flonum-fixnum n m)
    (let ((v ($flonum->exact n)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact ($modulo-fixnum-fixnum v m)))
	((bignum?)	(inexact ($modulo-bignum-fixnum v m)))
	(else
	 (%error-not-integer n)))))

  (define ($modulo-flonum-bignum n m)
    (let ((v ($flonum->exact n)))
      (cond-exact-integer-operand v
	((fixnum?)	(inexact ($modulo-fixnum-bignum v m)))
	((bignum?)	(inexact ($modulo-bignum-bignum v m)))
	(else
	 (%error-not-integer n)))))

  (define ($modulo-flonum-flonum n m)
    (let ((v ($flonum->exact n)))
      (cond-exact-integer-operand v
	((fixnum?)	($modulo-fixnum-flonum v m))
	((bignum?)	($modulo-bignum-flonum v m))
	(else
	 (%error-not-integer n)))))

;;; --------------------------------------------------------------------

  (define (%error-not-integer x)
    (assertion-violation who "expected integer as argument" x))

  #| end of module: modulo |# )


(define (quotient x y)
  (receive (q r)
      (quotient+remainder x y)
    q))

(define (remainder x y)
  (receive (q r)
      (quotient+remainder x y)
    r))

(module (quotient+remainder
	 $quotient+remainder-fixnum-number	$quotient+remainder-number-fixnum
	 $quotient+remainder-bignum-number	$quotient+remainder-number-bignum
	 $quotient+remainder-flonum-number	$quotient+remainder-number-flonum
	 $quotient+remainder-fixnum-fixnum	$quotient+remainder-bignum-fixnum
	 $quotient+remainder-fixnum-bignum	$quotient+remainder-bignum-bignum
	 $quotient+remainder-fixnum-flonum	$quotient+remainder-bignum-flonum
	 $quotient+remainder-flonum-fixnum
	 $quotient+remainder-flonum-bignum
	 $quotient+remainder-flonum-flonum)
  (define who 'quotient+remainder)

  (define (quotient+remainder x y)
    (if (eq? y 0)
	(assertion-violation who "second argument must be non-zero")
      (cond-numeric-operand x
       ((fixnum?)	($quotient+remainder-fixnum-number x y))
       ((bignum?)	($quotient+remainder-bignum-number x y))
       ((flonum?)	($quotient+remainder-flonum-number x y))
       ((ratnum?)	(%error-not-integer x))
       ((compnum?)	(%error-not-integer x))
       ((cflonum?)	(%error-not-integer x))
       (else
	(%error-not-integer x)))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-fixnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($quotient+remainder-fixnum-fixnum x y))
      ((bignum?)	($quotient+remainder-fixnum-bignum x y))
      ((flonum?)	($quotient+remainder-fixnum-flonum x y))
      ((ratnum?)	(%error-not-integer y))
      ((compnum?)	(%error-not-integer y))
      ((cflonum?)	(%error-not-integer y))
      (else
       (%error-not-integer y))))

  (define ($quotient+remainder-bignum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($quotient+remainder-bignum-fixnum x y))
      ((bignum?)	($quotient+remainder-bignum-bignum x y))
      ((flonum?)	($quotient+remainder-bignum-flonum x y))
      ((ratnum?)	(%error-not-integer y))
      ((compnum?)	(%error-not-integer y))
      ((cflonum?)	(%error-not-integer y))
      (else
       (%error-not-integer y))))

  (define ($quotient+remainder-flonum-number x y)
    (let ((v ($flonum->exact x)))
      (cond-exact-integer-operand v
	((fixnum?)
	 (receive (q r)
	     ($quotient+remainder-fixnum-number v y)
	   (values (inexact q) (inexact r))))
	((bignum?)
	 (receive (q r)
	     ($quotient+remainder-bignum-number v y)
	   (values (inexact q) (inexact r))))
	(else
	 (%error-not-integer x)))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-number-fixnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($quotient+remainder-fixnum-fixnum x y))
      ((bignum?)	($quotient+remainder-bignum-fixnum x y))
      ((flonum?)	($quotient+remainder-flonum-fixnum x y))
      ((ratnum?)	(%error-not-integer x))
      ((compnum?)	(%error-not-integer x))
      ((cflonum?)	(%error-not-integer x))
      (else
       (%error-not-integer x))))

  (define ($quotient+remainder-number-bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($quotient+remainder-fixnum-bignum x y))
      ((bignum?)	($quotient+remainder-bignum-bignum x y))
      ((flonum?)	($quotient+remainder-flonum-bignum x y))
      ((ratnum?)	(%error-not-integer x))
      ((compnum?)	(%error-not-integer x))
      ((cflonum?)	(%error-not-integer x))
      (else
       (%error-not-integer x))))

  (define ($quotient+remainder-number-flonum x y)
    (let ((v ($flonum->exact y)))
      (cond-exact-integer-operand v
	((fixnum?)
	 (receive (q r)
	     ($quotient+remainder-number-fixnum x v)
	   (values (inexact q) (inexact r))))
	((bignum?)
	 (receive (q r)
	     ($quotient+remainder-number-bignum x v)
	   (values (inexact q) (inexact r))))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-fixnum-fixnum x y)
    (if ($fx= y -1)
	;;We have  to assume that  the result of  negating X may  not be
	;;fixnum!!!  This happens when X is (least-fixnum).
	(values (- x) 0)
      (values ($fxquotient x y) ($fxremainder x y))))

  (define ($quotient+remainder-fixnum-bignum x y)
    (values 0 x))

  (define ($quotient+remainder-fixnum-flonum x y)
    (let ((v ($flonum->exact y)))
      (cond-exact-integer-operand v
	((fixnum?)
	 (receive (q r)
	     ($quotient+remainder-fixnum-fixnum x v)
	   (values (inexact q) (inexact r))))
	((bignum?)
	 (receive (q r)
	     ($quotient+remainder-fixnum-bignum x v)
	   (values (inexact q) (inexact r))))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-bignum-fixnum x y)
    (let ((p (foreign-call "ikrt_bnfxdivrem" x y)))
      (values ($car p) ($cdr p))))

  (define ($quotient+remainder-bignum-bignum x y)
    (let ((p (foreign-call "ikrt_bnbndivrem" x y)))
      (values ($car p) ($cdr p))))

  (define ($quotient+remainder-bignum-flonum x y)
    (let ((v ($flonum->exact y)))
      (cond-exact-integer-operand v
	((fixnum?)
	 (receive (q r)
	     ($quotient+remainder-bignum-fixnum x v)
	   (values (inexact q) (inexact r))))
	((bignum?)
	 (receive (q r)
	     ($quotient+remainder-bignum-bignum x v)
	   (values (inexact q) (inexact r))))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-flonum-fixnum x y)
    (let ((v ($flonum->exact x)))
      (cond-exact-integer-operand v
	((fixnum?)
	 (receive (q r)
	     ($quotient+remainder-fixnum-fixnum x y)
	   (values (inexact q) (inexact r))))
	((bignum?)
	 (receive (q r)
	     ($quotient+remainder-bignum-fixnum x y)
	   (values (inexact q) (inexact r))))
	(else
	 (%error-not-integer x)))))

  (define ($quotient+remainder-flonum-bignum x y)
    (let ((v ($flonum->exact x)))
      (cond-exact-integer-operand v
	((fixnum?)
	 (receive (q r)
	     ($quotient+remainder-fixnum-bignum x y)
	   (values (inexact q) (inexact r))))
	((bignum?)
	 (receive (q r)
	     ($quotient+remainder-bignum-bignum x y)
	   (values (inexact q) (inexact r))))
	(else
	 (%error-not-integer x)))))

  (define ($quotient+remainder-flonum-flonum x y)
    (let ((v ($flonum->exact x)))
      (cond-exact-integer-operand v
	((fixnum?)	($quotient+remainder-fixnum-flonum x y))
	((bignum?)	($quotient+remainder-bignum-flonum x y))
	(else
	 (%error-not-integer x)))))

;;; --------------------------------------------------------------------

  (define (%error-not-integer x)
    (assertion-violation who "expected integer as argument" x))

  #| end of module: quotient+remainder |# )


(module (=)
  (define who (quote =))

  (define =
    (case-lambda
     ((x y)
      (cond-numeric-operand x
	((fixnum?)	($fixnum=number?  x y))
	((bignum?)	($bignum=number?  x y))
	((flonum?)	($flonum=number?  x y))
	((ratnum?)	($ratnum=number?  x y))
	((compnum?)	($compnum=number? x y))
	((cflonum?)	($cflonum=number? x y))
	(else
	 (%error-not-number x))))

     ((x y z)
      (cond ((= x y)
	     (= y z))
	    ((number? z)
	     #f)
	    (else
	     (%error-not-number z))))

     ((x)
      (if (number? x)
	  #t
	(%error-not-number x)))

     ((x y . ls)
      (cond-numeric-operand x
	((fixnum?)	(%doloop $fixnum=number?  x y ls))
	((bignum?)	(%doloop $bignum=number?  x y ls))
	((flonum?)	(%doloop $flonum=number?  x y ls))
	((ratnum?)	(%doloop $ratnum=number?  x y ls))
	((compnum?)	(%doloop $compnum=number? x y ls))
	((cflonum?)	(%doloop $cflonum=number? x y ls))
	(else
	 (%error-not-number x))))))

;;; --------------------------------------------------------------------

  (define-syntax %doloop
    (syntax-rules ()
      ((_ ?cmp ?x0 ?y0 ?ls0)
       (let loop ((x  ?x0)
		  (y  ?y0)
		  (ls ?ls0))
	 (if (?cmp x y)
	     (or (null? ls)
		 (loop x ($car ls) ($cdr ls)))
	   (or (null? ls)
	       (%validate-rest-arguments ($car ls) ($cdr ls))))))))

  (define (%validate-rest-arguments x ls)
    (if (number? x)
	(if (null? ls)
	    #f
	  (%validate-rest-arguments ($car ls) ($cdr ls)))
      (%error-not-number x)))

;;; --------------------------------------------------------------------

  (define ($fixnum=number? x y)
    (cond-numeric-operand y
      ((fixnum?)	($fx= x y))
      ((bignum?)	#f)
      ((flonum?)	(fxfl= x y))
      ((ratnum?)	#f)
      ((compnum?)	#f) ;remember that a compnum has non-zero imag part
      ((cflonum?)
       (and ($flzero? ($cflonum-imag y))
	    (fxfl= x  ($cflonum-real y))))
      (else
       (%error-not-number y))))

  (define ($bignum=number? x y)
    (cond-numeric-operand y
      ((fixnum?)	#f)
      ((bignum?)	(bnbn= x y))
      ((flonum?)	(bnfl= x y))
      ((ratnum?)	#f)
      ((compnum?)	#f) ;remember that a compnum has non-zero imag part
      ((cflonum?)
       (and ($flzero? ($cflonum-imag y))
	    (bnfl= x  ($cflonum-real y))))
      (else
       (%error-not-number y))))

  (define ($flonum=number? x y)
    (cond-numeric-operand y
      ((flonum?)	(flfl= x y))
      ((cflonum?)
       (and ($flzero? ($cflonum-imag y))
	    (flfl= x  ($cflonum-real y))))
      ((fixnum?)	(flfx= x y))
      ((bignum?)	(flbn= x y))
      ((ratnum?)	(flrt= x y))
      ((compnum?)	#f) ;remember that a compnum has non-zero imag part
      (else
       (%error-not-number y))))

  (define ($ratnum=number? x y)
    (cond-numeric-operand y
      ((fixnum?)	#f)
      ((bignum?)	#f)
      ((ratnum?)	(rtrt= x y))
      ((flonum?)	(rtfl= x y))
      ((compnum?)	#f) ;remember that a compnum has non-zero imag part
      ((cflonum?)
       (and ($flzero? ($cflonum-imag y))
	    (rtfl= x  ($cflonum-real y))))
      (else
       (%error-not-number y))))

  (define ($compnum=number? x y)
    (cond-numeric-operand y
      ((fixnum?)	#f)
      ((bignum?)	#f)
      ((ratnum?)	#f)
      ((flonum?)	#f)
      ((compnum?)	(cncn= x y))
      ((cflonum?)	(cncf= x y))
      (else
       (%error-not-number y))))

  (define ($cflonum=number? x y)
    (cond-numeric-operand y
      ((fixnum?)	#f)
      ((bignum?)	#f)
      ((ratnum?)	#f)
      ((flonum?)
       (and ($flzero? ($cflonum-imag x))
	    (flfl= y  ($cflonum-real x))))
      ((compnum?)	(cncf= y x))
      ((cflonum?)	(cfcf= x y))
      (else
       (%error-not-number y))))

;;; --------------------------------------------------------------------

  (define (cncn= x y)
    (and (= ($compnum-real x) ($compnum-real y))
	 (= ($compnum-imag x) ($compnum-imag y))))

  (define (cncf= x y)
    (and (= ($compnum-real x) ($cflonum-real y))
	 (= ($compnum-imag x) ($cflonum-imag y))))

  (define (cfcf= x y)
    (and (= ($cflonum-real x) ($cflonum-real y))
	 (= ($cflonum-imag x) ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define (%error-not-number x)
    (assertion-violation who "expected number as argument" x))

  #| end of module |# )


(module (< <= > >=)

  (define-syntax define-ordered-comparison
    (syntax-rules ()
      ((_ ?who
	  fxfx< fxbn< bnfx< bnbn<
	  fxfl< flfx< bnfl< flbn< flfl<
	  fxrt< rtfx< bnrt< rtbn< flrt< rtfl< rtrt<)
       (module (?who)

	 (define ?who
	   (case-lambda
	    ((x y)
	     (cond-real-numeric-operand x
	       ((fixnum?)
		(cond-real-numeric-operand y
		  ((fixnum?)	(fxfx< x y))
		  ((bignum?)	(fxbn< x y))
		  ((flonum?)	(fxfl< x y))
		  ((ratnum?)	(fxrt< x y))
		  (else
		   (%error-not-real y))))
	       ((bignum?)
		(cond-real-numeric-operand y
		  ((fixnum?)	(bnfx< x y))
		  ((bignum?)	(bnbn< x y))
		  ((flonum?)	(bnfl< x y))
		  ((ratnum?)	(bnrt< x y))
		  (else
		   (%error-not-real y))))
	       ((flonum?)
		(cond-real-numeric-operand y
		  ((fixnum?)	(flfx< x y))
		  ((bignum?)	(flbn< x y))
		  ((flonum?)	(flfl< x y))
		  ((ratnum?)	(flrt< x y))
		  (else
		   (%error-not-real y))))
	       ((ratnum?)
		(cond-real-numeric-operand y
		  ((fixnum?)	(rtfx< x y))
		  ((bignum?)	(rtbn< x y))
		  ((flonum?)	(rtfl< x y))
		  ((ratnum?)	(rtrt< x y))
		  (else
		   (%error-not-real y))))
	       (else
		(%error-not-real x))))

	    ((x y z)
	     (and (?who x y)
		  (?who y z)))

	    ((x)
	     (if (real? x)
		 #t
	       (%error-not-real x)))

	    ((x y . ls)
	     (cond-real-numeric-operand x
	       ((fixnum?)		(%loop-with-fixnum-as-first x y ls))
	       ((bignum?)		(%loop-with-bignum-as-first x y ls))
	       ((flonum?)		(%loop-with-flonum-as-first x y ls))
	       ((ratnum?)		(%loop-with-ratnum-as-first x y ls))
	       (else
		(%error-not-real x))))))

;;; --------------------------------------------------------------------

	 (define (%loop-with-fixnum-as-first x y ls)
	   (cond-real-numeric-operand y
	     ((fixnum?)
	      (cond ((null? ls)
		     (fxfx< x y))
		    ((fxfx< x y)
		     (%loop-with-fixnum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((bignum?)
	      (cond ((null? ls)
		     (fxbn< x y))
		    ((fxbn< x y)
		     (%loop-with-bignum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((flonum?)
	      (cond ((null? ls)
		     (fxfl< x y))
		    ((fxfl< x y)
		     (%loop-with-flonum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((ratnum?)
	      (cond ((null? ls)
		     (fxrt< x y))
		    ((fxrt< x y)
		     (%loop-with-ratnum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     (else
	      (%error-not-real y))))

;;; --------------------------------------------------------------------

	 (define (%loop-with-bignum-as-first x y ls)
	   (cond-real-numeric-operand y
	     ((fixnum?)
	      (cond ((null? ls)
		     (bnfx< x y))
		    ((bnfx< x y)
		     (%loop-with-fixnum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((bignum?)
	      (cond ((null? ls)
		     (bnbn< x y))
		    ((bnbn< x y)
		     (%loop-with-bignum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((flonum?)
	      (cond ((null? ls)
		     (bnfl< x y))
		    ((bnfl< x y)
		     (%loop-with-flonum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((ratnum?)
	      (cond ((null? ls)
		     (bnrt< x y))
		    ((bnrt< x y)
		     (%loop-with-ratnum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     (else
	      (%error-not-real y))))

;;; --------------------------------------------------------------------

	 (define (%loop-with-flonum-as-first x y ls)
	   (cond-real-numeric-operand y
	     ((fixnum?)
	      (cond ((null? ls)
		     (flfx< x y))
		    ((flfx< x y)
		     (%loop-with-fixnum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((bignum?)
	      (cond ((null? ls)
		     (flbn< x y))
		    ((flbn< x y)
		     (%loop-with-bignum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((flonum?)
	      (cond ((null? ls)
		     (flfl< x y))
		    ((flfl< x y)
		     (%loop-with-flonum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((ratnum?)
	      (cond ((null? ls)
		     (flrt< x y))
		    ((flrt< x y)
		     (%loop-with-ratnum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     (else
	      (%error-not-real y))))

;;; --------------------------------------------------------------------

	 (define (%loop-with-ratnum-as-first x y ls)
	   (cond-real-numeric-operand y
	     ((fixnum?)
	      (cond ((null? ls)
		     (rtfx< x y))
		    ((rtfx< x y)
		     (%loop-with-fixnum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((bignum?)
	      (cond ((null? ls)
		     (rtbn< x y))
		    ((rtbn< x y)
		     (%loop-with-bignum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((flonum?)
	      (cond ((null? ls)
		     (rtfl< x y))
		    ((rtfl< x y)
		     (%loop-with-flonum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     ((ratnum?)
	      (cond ((null? ls)
		     (rtrt< x y))
		    ((rtrt< x y)
		     (%loop-with-ratnum-as-first y ($car ls) ($cdr ls)))
		    (else
		     (%validate-rest-arguments ($car ls) ($cdr ls)))))
	     (else
	      (%error-not-real y))))

;;; --------------------------------------------------------------------

	 (define (%validate-rest-arguments x ls)
	   (if (real? x)
	       (if (null? ls)
		   #f
		 (%validate-rest-arguments ($car ls) ($cdr ls)))
	     (%error-not-real x)))

	 (define (%error-not-real x)
	   (assertion-violation '?who "expected real number as argument" x))

	 #| end of module: ?who |# )
       )))

;;; --------------------------------------------------------------------

  (define-ordered-comparison <
    $fx< fxbn< bnfx< bnbn< fxfl< flfx< bnfl< flbn< flfl<
    exrt< rtex< exrt< rtex< flrt< rtfl< rtrt<)

  (define-ordered-comparison >
    $fx> fxbn> bnfx> bnbn> fxfl> flfx> bnfl> flbn> flfl>
    exrt> rtex> exrt> rtex> flrt> rtfl> rtrt>)

  (define-ordered-comparison <=
    $fx<= fxbn< bnfx< bnbn<= fxfl<= flfx<= bnfl<= flbn<= flfl<=
    exrt< rtex< exrt< rtex< flrt<= rtfl<= rtrt<=)

  (define-ordered-comparison >=
    $fx>= fxbn> bnfx> bnbn>= fxfl>= flfx>= bnfl>= flbn>= flfl>=
    exrt> rtex> exrt> rtex> flrt>= rtfl>= rtrt>=)

  #| end of module |# )


(module (bnbn= bnbn< bnbn> bnbn<= bnbn>= fxbn< bnfx< fxbn> bnfx>)

  (define-syntax bnbncmp
    (syntax-rules ()
      ((_ ?x ?y ?fxcmp)
       (?fxcmp (foreign-call "ikrt_bnbncomp" ?x ?y) 0))))

  (define-syntax define-comparison1
    (syntax-rules ()
      ((_ ?who ?fxcmp)
       (define-syntax ?who
	 (syntax-rules ()
	   ((_ x y)
	    (bnbncmp x y ?fxcmp))))
       )))

  (define-syntax define-comparison2
    (syntax-rules ()
      ((_ ?who ?pred ?x ?y ?op)
       (define-syntax ?who
	 (syntax-rules ()
	   ((_ ?x ?y)
	    (?pred ?op))))
       )))

;;; --------------------------------------------------------------------

  (define-comparison1 bnbn=	$fx=)
  (define-comparison1 bnbn<	$fx<)
  (define-comparison1 bnbn>	$fx>)
  (define-comparison1 bnbn<=	$fx<=)
  (define-comparison1 bnbn>=	$fx>=)

  (define-comparison2 fxbn<	$bignum-positive?	x y	y)
  (define-comparison2 bnfx<	$bignum-negative?	x y	x)
  (define-comparison2 fxbn>	$bignum-negative?	x y	y)
  (define-comparison2 bnfx>	$bignum-positive?	x y	x)

  #| end of module |# )


(let-syntax
    ((define-flonum-comparisons
       (syntax-rules ()
	 ((_ flfl? flfx? fxfl? flbn? bnfl? fl?)
	  (begin
	    (define-syntax flfl?
	      (syntax-rules () ((_ x y) (fl? x y))))
	    (define-syntax flfx?
	      (syntax-rules () ((_ x y) (fl? x ($fixnum->flonum y)))))
	    (define-syntax flbn?
	      (syntax-rules () ((_ x y) (fl? x ($bignum->flonum y)))))
	    (define-syntax fxfl?
	      (syntax-rules () ((_ x y) (fl? ($fixnum->flonum x) y))))
	    (define-syntax bnfl?
	      (syntax-rules () ((_ x y) (fl? ($bignum->flonum x) y)))))))))

  (define-flonum-comparisons flfl=  flfx=  fxfl=  flbn=  bnfl=		$fl=)
  (define-flonum-comparisons flfl<  flfx<  fxfl<  flbn<  bnfl<		$fl<)
  (define-flonum-comparisons flfl>  flfx>  fxfl>  flbn>  bnfl>		$fl>)
  (define-flonum-comparisons flfl<= flfx<= fxfl<= flbn<= bnfl<=		$fl<=)
  (define-flonum-comparisons flfl>= flfx>= fxfl>= flbn>= bnfl>=		$fl>=))


(module (flrt= flrt< flrt<= flrt> flrt>=)

  (define-syntax cmp-flonum/ratnum
    (syntax-rules ()
      ((_ ?pred)
       (syntax-rules ()
	 ((_ ?flonum-expr ?ratnum-expr)
	  (let ((fl ?flonum-expr)
		(rn ?ratnum-expr))
	    (if ($flonum-rational? fl)
		(?pred (exact fl) rn)
	      (?pred fl (inexact rn)))))))))

  (define-syntax flrt=  (cmp-flonum/ratnum =))
  (define-syntax flrt<  (cmp-flonum/ratnum <))
  (define-syntax flrt>  (cmp-flonum/ratnum >))
  (define-syntax flrt<= (cmp-flonum/ratnum <=))
  (define-syntax flrt>= (cmp-flonum/ratnum >=))

  #| end of module |# )

(module (rtfl= rtfl< rtfl<= rtfl> rtfl>=)

  (define-syntax cmp-ratnum/flonum
    (syntax-rules ()
      ((_ ?pred)
       (syntax-rules ()
	 ((_ ?ratnum-expr ?flonum-expr)
	  (let ((rn ?ratnum-expr)
		(fl ?flonum-expr))
	    (if ($flonum-rational? fl)
		(?pred rn (exact fl))
	      (?pred (inexact rn) fl))))))))

  (define-syntax rtfl=  (cmp-ratnum/flonum =))
  (define-syntax rtfl<  (cmp-ratnum/flonum <))
  (define-syntax rtfl<= (cmp-ratnum/flonum <=))
  (define-syntax rtfl>  (cmp-ratnum/flonum >))
  (define-syntax rtfl>= (cmp-ratnum/flonum >=))

  #| end of module |# )

;;; --------------------------------------------------------------------
;;; comparisons between ratnum and exact integer

;;Notice that a ratnum can never be equal to an exact integer.

(define (exrt< x y)
  ;;Comparison: exact-integer < ratnum.
  ;;
  ;;     y.num
  ;; x < -----   <=>   x * y.den < y.num
  ;;     y.den
  ;;
  (< (* x ($ratnum-d y))
     ($ratnum-n y)))

(define (rtex< x y)
  ;;Comparison: ratnum < exact-integer.
  ;;
  ;; x.num
  ;; ----- < y   <=>   x.num < y * x.den
  ;; x.den
  ;;
  (< ($ratnum-n x)
     (* y ($ratnum-d x))))

(define (exrt> x y)
  ;;Comparison: exact-integer > ratnum.
  ;;
  ;;     y.num
  ;; x > -----   <=>   x * y.den > y.num
  ;;     y.den
  ;;
  (> (* x ($ratnum-d y))
     ($ratnum-n y)))

(define (rtex> x y)
  ;;Comparison: ratnum > exact-integer.
  ;;
  ;; x.num
  ;; ----- > y   <=>   x.num > y * x.den
  ;; x.den
  ;;
  (> ($ratnum-n x)
     (* y ($ratnum-d x))))

;;; --------------------------------------------------------------------

(define (rtrt= x y)
  ;;Comparison: ratnum = ratnum.
  ;;
  (and (= ($ratnum-n x) ($ratnum-n y))
       (= ($ratnum-d x) ($ratnum-d y))))

(define (rtrt< x y)
  ;;Comparison: ratnum < ratnum.
  ;;
  ;; x.num   y.num
  ;; ----- < -----   <=>   x.num * y.den < y.num * x.den
  ;; x.den   y.den
  ;;
  (< (* ($ratnum-n x) ($ratnum-d y))
     (* ($ratnum-n y) ($ratnum-d x))))

(define (rtrt<= x y)
  ;;Comparison: ratnum <= ratnum.
  ;;
  ;; x.num    y.num
  ;; ----- <= -----   <=>   x.num * y.den <= y.num * x.den
  ;; x.den    y.den
  ;;
  (<= (* ($ratnum-n x) ($ratnum-d y))
      (* ($ratnum-n y) ($ratnum-d x))))

(define (rtrt> x y)
  ;;Comparison: ratnum > ratnum.
  ;;
  ;; x.num   y.num
  ;; ----- > -----   <=>   x.num * y.den > y.num * x.den
  ;; x.den   y.den
  ;;
  (> (* ($ratnum-n x) ($ratnum-d y))
     (* ($ratnum-n y) ($ratnum-d x))))

(define (rtrt>= x y)
  ;;Comparison: ratnum >= ratnum.
  ;;
  ;; x.num    y.num
  ;; ----- >= -----   <=>   x.num * y.den >= y.num * x.den
  ;; x.den    y.den
  ;;
  (>= (* ($ratnum-n x) ($ratnum-d y))
      (* ($ratnum-n y) ($ratnum-d x))))


(define (error@add1 x)
  ;;This is the error handler  function called when an interrupt happens
  ;;while executing ADD1.
  ;;
  ;;By importing  the library here  we shadow the bindings,  causing the
  ;;forms  below to  be expanded  by  the optimizer  with the  primitive
  ;;operations.
  (import (ikarus))
  (cond ((fixnum? x)	(+ (greatest-fixnum) 1))
	((number? x)	(+ x 1))
	(else
	 (assertion-violation 'add1 "not a number" x))))

(define (add1 x)
  ;;By importing  the library here  we shadow the binding  ADD1, causing
  ;;the form  below to be expanded  by the optimizer with  the primitive
  ;;operation ADD1.
  (import (only (ikarus)
		add1))
  (add1 x))

(define (error@sub1 x)
  ;;This is the error handler  function called when an interrupt happens
  ;;while executing ADD1.
  ;;
  ;;By importing  the library here  we shadow the bindings,  causing the
  ;;forms  below to  be expanded  by  the optimizer  with the  primitive
  ;;operations.
  (import (ikarus))
  (cond ((fixnum? x)	(- (least-fixnum) 1))
	((number? x)	(- x 1))
	(else
	 (assertion-violation 'sub1 "not a number" x))))

(define (sub1 x)
  ;;By importing  the library here  we shadow the binding  SUB1, causing
  ;;the form  below to be expanded  by the optimizer with  the primitive
  ;;operation SUB1.
  (import (ikarus))
  (sub1 x))


(define (zero? x)
  (cond-numeric-operand x
    ((fixnum?)	($fxzero? x))
    ((bignum?)	#f)
    ;;The numerator of a ratnum is always non-zero.
    ((ratnum?)	#f)
    ((flonum?)	(or ($fl= x 0.0) ($fl= x -0.0)))
    ((compnum?)	#f)
    ((cflonum?)	(and ($flzero? ($cflonum-real x))
		     ($flzero? ($cflonum-imag x))))
    (else
     (assertion-violation 'zero? "expected number as argument" x))))

(define (positive? x)
  (cond-real-numeric-operand x
    ((fixnum?)	($fxpositive? x))
    ((bignum?)	($bignum-positive? x))
    ((flonum?)	($flpositive? x))
    ;;The denominator of a ratnum is always strictly positive.
    ((ratnum?)	(positive? ($ratnum-n x)))
    (else
     (assertion-violation 'positive? "expected real number as argument" x))))

(define (negative? x)
  (cond-real-numeric-operand x
    ((fixnum?)	($fxnegative? x))
    ((bignum?)	($bignum-negative? x))
    ;;The denominator of a ratnum is always strictly positive.
    ((ratnum?)	(negative? ($ratnum-n x)))
    ((flonum?)	($flnegative? x))
    (else
     (assertion-violation 'negative? "expected real number as argument" x))))


(module (expt
	 $expt-number-fixnum	$expt-number-bignum	$expt-number-flonum
	 $expt-number-ratnum	$expt-number-compnum	$expt-number-cflonum)
  ;;Return N raised to the power  M.  For non-zero N, this is:
  ;;
  ;;    (expt N M) === (exp (log (expt N M)))
  ;;               === (exp (* M (log N)))
  ;;
  ;;for N equal to zero:
  ;;
  ;;    (expt 0.0 Z) = 1.0 if Z = 0.0
  ;;                 = 0.0 if (real-part Z) is positive
  ;;
  ;;for  other cases  in which  the first  argument is  zero, either  an
  ;;exception       is       raised      with       condition       type
  ;;"&implementation-restriction",  or an  unspecified number  object is
  ;;returned.
  ;;
  ;;For an  exact real number  object N and  an exact integer  object M,
  ;;(expt N M)  must return an exact result.  For all  other values of N
  ;;and M, (expt N M) may return an inexact result, even when both N and
  ;;M are exact.
  ;;
  ;;Notice that this definition can  lead to unintuitive results; from a
  ;;discussion with Kent Dybvig: It does seem like:
  ;;
  ;;   (expt +inf.0+2.i 2)
  ;;
  ;;would be equivalent to:
  ;;
  ;;   (* +inf.0+2.i +inf.0+2.i)
  ;;
  ;;which evaluates  to +inf.0+inf.0i.   Nevertheless, I'm not  sure the
  ;;R6RS supports this interpretation.   According to the description of
  ;;expt, when z1 is not zero,
  ;;
  ;;   (expt z1 z2) => e^{z2 log z1}
  ;;
  ;;so,
  ;;
  ;;   (expt +inf.0+2.i 2) => (exp (* 2 (log +inf.0+2.i)))
  ;;
  ;;Meanwhile, the Section 11.7.3 subsection on transcendental functions
  ;;defines log as follows:
  ;;
  ;;   (log z) => log |z| + (angle z)i
  ;;
  ;;so,
  ;;
  ;;   (log +inf.0+2.i) =>
  ;;     (make-rectangular
  ;;       (log (magnitude +inf.0+2.0i))
  ;;       (angle +inf.0+2.0i))
  ;;
  ;;Since:
  ;;
  ;;   (magnitude +inf.0+2.i) => +inf.0,
  ;;   (log +inf.0) => +inf.0, and
  ;;   (angle +inf.0+2.i) => 0.0,
  ;;
  ;;we have:
  ;;
  ;;   (log +inf.0+2.i) => +inf.0+0.0i
  ;;
  ;;and finally:
  ;;
  ;;   (expt +inf.0+2.i 2) => (exp (* 2 +inf.0+0.0i))
  ;;                       => (exp +inf.0+0.0i)
  ;;                       => (* (exp +inf.0) (exp 0.0i))
  ;;                       => (* +inf.0 1.0+0.0i)
  ;;                       => +inf.0+nan.0i
  ;;
  ;;because:
  ;;
  ;;   (* +inf.0 0.0i) => +nan.0
  ;;
  (define who 'expt)

  (define (expt n m)
    (cond-numeric-operand m
      ((fixnum?)	($expt-number-fixnum  n m))
      ((bignum?)	($expt-number-bignum  n m))
      ((flonum?)	($expt-number-flonum  n m))
      ((ratnum?)	($expt-number-ratnum  n m))
      ((compnum?)	($expt-number-compnum n m))
      ((cflonum?)	($expt-number-cflonum n m))
      (else
       (assertion-violation who "expected number as argument" m))))

  (module ($expt-number-fixnum)

    (define ($expt-number-fixnum n m)
      (cond (($fxzero? m)
	     (cond ((nan?   n)	+nan.0)
		   ((exact? n)	1)
		   (else	1.)))
	    (($fx> m 0)
	     (cond ((integer? n)
		    (%expt-fx n m))
		   ((ratnum? n)
		    ($make-ratnum (%expt-fx ($ratnum-n n) m)
				  (%expt-fx ($ratnum-d n) m)))
		   ((real? n) ;this includes the real infinite +inf.0
		    (if (nan? n)
			+nan.0
		      (%expt-fx n m)))
		   ;;In the following clauses N is a non-real.
		   ;;
		   ((nan? n)
		    +nan.0+nan.0i)
		   ((infinite? n) ;this handles correctly some special cases
		    (exp (* m (log n))))
		   (else
		    (%expt-fx n m))))
	    (else ;M is negative
	     (let ((v (expt n (- m))))
	       (if (eq? v 0)
		   0
		 (/ 1 v))))))

    (define (%expt-fx n m)
      ;;Recursive function computing N^M when M is a fixnum and N is:
      ;;
      ;;* A real number, infinite included, NaN excluded.
      ;;
      ;;* A finite complex number.
      ;;
      ;;This function  recurses a number of  times equal to the  bits in
      ;;the representation of M.
      ;;
      ;;Notes about used procedures:
      ;;
      ;;* ($fxlogand m 1)  is 1 for even m and 0 for  odd m, or in other
      ;;  words: the return value is the rightmost bit of M.
      ;;
      ;;* $fxsra means "fixnum shift right arithmetic".
      ;;
      ;;* binary* is the multiplication with two arguments.
      ;;
      (cond (($fxzero? m)
	     1)
	    (($fxzero? ($fxlogand m 1)) ;the rightmost bit in M is zero
	     (%expt-fx (binary* n n) ($fxsra m 1)))
	    (else ;the rightmost bit in M is one
	     (binary* n (%expt-fx (binary* n n) ($fxsra m 1))))))

    #| end of module: $expt-number-fixnum |# )

  (define ($expt-number-bignum n m)
    (cond ((eq? n 0)	0)
	  ((eq? n 1)	1)
	  ((eq? n -1)	(if ($bignum-even? m) 1 -1))
	  ((nan? n)	+nan.0)
	  (else
	   (assertion-violation who "result is too big to compute" n m))))

  (define ($expt-number-flonum n m)
    (cond ((real? n)
	   (cond ((nan? n)
		  +nan.0)
		 ((integer? m)
		  ;;N^M when M is an integer always has a real number as
		  ;;result.
		  (flexpt (inexact n) m))
		 ((negative? n)
		  (exp (* m (log n))))
		 (else
		  (flexpt (inexact n) m))))
	  ;;If we are here: N is complex or NaN.
	  ;;
	  ((nan? n)
	   +nan.0+nan.0i)
	  (else
	   (exp (* m (log n))))))

  (define ($expt-number-ratnum n m)
    ;; (expt (expt n ($ratnum-n m))
    ;;       (inexact ($make-ratnum 1 ($ratnum-d m))))
    ($expt-number-flonum n (inexact m)))

  (define ($expt-number-compnum n m)
    (cond ((eq? n 0)
	   0)
	  ((nan? n)
	   +nan.0+nan.0i)
	  ((zero? n)
	   (if (flonum? n)
	       0.0
	     0.0+0.0i))
	  (else
	   (exp (* m (log n))))))

  (define ($expt-number-cflonum n m)
    (cond ((eq? n 0)
	   0)
	  ((nan? n)
	   +nan.0+nan.0i)
	  ((zero? n)
	   (if (flonum? n)
	       0.0
	     0.0+0.0i))
	  (else
	   (exp (* m (log n))))))

  #| end of module: expt |# )


(module (sqr
	 $sqr-fixnum		$sqr-bignum		$sqr-ratnum
	 $sqr-compnum		$sqr-cflonum)
  (define who 'sqr)

  (define (sqr x)
    (cond-numeric-operand x
      ((fixnum?)	($sqr-fixnum  x))
      ((bignum?)	($sqr-bignum  x))
      ((flonum?)	($flsqr       x))
      ((ratnum?)	($sqr-ratnum  x))
      ((compnum?)	($sqr-compnum x))
      ((cflonum?)	($sqr-cflonum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($sqr-fixnum x)
    ($fixnum*fixnum x x))

  (define ($sqr-bignum x)
    ($bignum*bignum x x))

  (define ($sqr-ratnum x)
    ($ratnum*ratnum x x))

  (define ($sqr-compnum x)
    ($compnum*compnum x x))

  (define ($sqr-cflonum x)
    ($cflonum*cflonum x))

  #| end of module: sqr |# )


(module (log
	 $log-fixnum		$log-flonum		$log-bignum
	 $log-ratnum		$log-compnum		$log-cflonum)
  (define who 'log)

  (define log
    (case-lambda
     ((x)
      (cond-numeric-operand x
	((fixnum?)	($log-fixnum  x))
	((bignum?)	($log-bignum  x))
	((ratnum?)	($log-ratnum  x))
	((flonum?)	($log-flonum  x))
	((compnum?)	($log-compnum x))
	((cflonum?)	($log-cflonum x))
	(else
	 (assertion-violation who "not a number" x))))
     ((x y)
      (let ((ly (log y)))
	(if (eq? ly 0)
	    (assertion-violation who "invalid arguments" x y)
	  (/ (log x) ly))))))

;;; --------------------------------------------------------------------

  (define ($log-fixnum x)
    (cond (($fx= x 1)
	   0)
	  (($fxzero? x)
	   (assertion-violation who "undefined around 0"))
	  (($fxpositive? x)
	   (foreign-call "ikrt_fx_log" x))
	  (else
	   ;;We must assume that the opposite of X may be a bignum.
	   ($make-rectangular (log (- x))
			      (acos -1)))))

  (define ($log-flonum x)
    (cond ((nan? x)
	   +nan.0)
	  (($fl>= x 0.0)
	   (foreign-call "ikrt_fl_log" x))
	  (else
	   ($make-cflonum ($fllog ($fl- x))
			  (acos -1)))))

  (define ($log-bignum x)
    (if ($bignum-positive? x)
	(let ((v (log (inexact x))))
	  (if (infinite? v)
	      (receive (s r)
		  (exact-integer-sqrt x)
		;;Could  the  (dropped)   residual  ever  affect  the
		;;answer?
		(fl* 2.0 (log s)))
	    v))
      (make-rectangular (log (- x))
			(acos -1))))

  (define ($log-ratnum x)
    (- (log ($ratnum-n x))
       (log ($ratnum-d x))))

  (define ($log-compnum x)
    ;;
    ;;         log (x.rep^2 + x.imp^2)
    ;; log x = ----------------------- + i * atan(x.imp, x.rep)
    ;;                    2
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular (/ (log (+ (* x.rep x.rep)
				    (* x.imp x.imp)))
			    2)
			 (atan x.imp x.rep))))

  (define ($log-cflonum x)
    ;;
    ;;         log (x.rep^2 + x.imp^2)
    ;; log x = ----------------------- + i * atan(x.imp, x.rep)
    ;;                    2
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($fl/ ($fllog ($fl+ ($flsqr x.rep)
					 ($flsqr x.imp)))
			   2.0)
		     ($flatan2 x.imp x.rep))))

  #| end of module |# )


(module (exp
	 $exp-fixnum		$exp-bignum		$exp-ratnum
	 $exp-compnum		$exp-cflonum)
  (define who 'exp)

  (define (exp x)
    (cond-numeric-operand x
      ((flonum?)	($flexp       x))
      ((cflonum?)	($exp-cflonum x))
      ((fixnum?)	($exp-fixnum  x))
      ((bignum?)	($exp-bignum  x))
      ((ratnum?)	($exp-ratnum  x))
      ((compnum?)	($exp-compnum x))
      (else
       (assertion-violation who "expected number as argument" x))))

;;; --------------------------------------------------------------------

  (define ($exp-fixnum x)
    (if ($fxzero? x)
	1
      ($flexp (fixnum->flonum x))))

  (define ($exp-bignum x)
    ($flexp ($bignum->flonum x)))

  (define ($exp-ratnum x)
    ($flexp ($ratnum->flonum x)))

  (define ($exp-compnum x)
    ;;
    ;;   e^x = e^(x.rep + x.imp i) = e^x.rep cos(x.imp) + e^x.rep sin(x.imp) i
    ;;
    (let* ((x.rep	($compnum-real x))
	   (x.imp	($compnum-imag x))
	   (e^x.rep	(exp x.rep)))
      ($make-rectangular (* e^x.rep (cos x.imp))
			 (* e^x.rep (sin x.imp)))))

  (define ($exp-cflonum x)
    ;;In general:
    ;;
    ;;   e^x = e^(x.rep + i * x.imp)
    ;;       = e^x.rep cos(x.imp) + i * e^x.rep sin(x.imp)
    ;;
    ;;and:
    ;;
    ;;   e^(x.rep+0.0i) = e^x.rep * e^(0.0 i) = e^x.rep * 1.0+0.0i
    ;;
    ;;so, in the special case x.rep=+inf.0, the imaginary part becomes:
    ;;
    ;;   e^x.rep * sin(x.imp) * i = +inf.0 * 0.0 * i = +nan.0 * i
    ;;
    (let* ((x.rep	($cflonum-real x))
	   (x.imp	($cflonum-imag x))
	   (e^x.rep	($flexp x.rep)))
      ($make-cflonum ($fl* e^x.rep ($flcos x.imp))
		     ($fl* e^x.rep ($flsin x.imp)))))

  #| end of module: exp |# )


(module (sin
	 $sin-fixnum		$sin-bignum		$sin-ratnum
	 $sin-cflonum		$sin-compnum)

  (define (sin x)
    (cond-numeric-operand x
      ((flonum?)	($flsin x))
      ((cflonum?)	($sin-cflonum x))
      ((fixnum?)	($sin-fixnum  x))
      ((bignum?)	($sin-bignum  x))
      ((ratnum?)	($sin-ratnum  x))
      ((compnum?)	($sin-compnum x))
      (else
       (assertion-violation 'sin "expected number as argument" x))))

  (define ($sin-fixnum x)
    (if ($fxzero? x)
	0
      (foreign-call "ikrt_fx_sin" x)))

  (define ($sin-bignum x)
    ($flsin (inexact x)))

  (define ($sin-ratnum x)
    ($flsin (inexact x)))

  (define ($sin-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular (* (sin x.rep) (cosh x.imp))
			 (* (cos x.rep) (sinh x.imp)))))

  (define ($sin-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($fl* ($flsin x.rep) ($flcosh x.imp))
		     ($fl* ($flcos x.rep) ($flsinh x.imp)))))

  #| end of module: sin |# )


(module (cos
	 $cos-fixnum		$cos-bignum		$cos-ratnum
	 $cos-cflonum		$cos-compnum)

  (define (cos x)
    (cond-numeric-operand x
      ((flonum?)	($flcos x))
      ((cflonum?)	($cos-cflonum x))
      ((fixnum?)	($cos-fixnum  x))
      ((bignum?)	($cos-bignum  x))
      ((ratnum?)	($cos-ratnum  x))
      ((compnum?)	($cos-compnum x))
      (else
       (assertion-violation 'cos "expected number as argument" x))))

  (define ($cos-fixnum x)
    (if ($fxzero? x)
	1
      (foreign-call "ikrt_fx_cos" x)))

  (define ($cos-bignum x)
    ($flcos (inexact x)))

  (define ($cos-ratnum x)
    ($flcos (inexact x)))

  (define ($cos-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular (* (cos x.rep) (cosh x.imp))
			 (* (sin x.rep) (sinh x.imp)))))

  (define ($cos-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($fl* ($flcos x.rep) ($flcosh x.imp))
		     ($fl* ($flsin x.rep) ($flsinh x.imp)))))

  #| end of module: cos |# )


(module (tan
	 $tan-fixnum		$tan-bignum		$tan-ratnum
	 $tan-compnum		$tan-cflonum)

  (define (tan x)
    (cond-numeric-operand x
      ((flonum?)	($fltan       x))
      ((cflonum?)	($tan-cflonum x))
      ((fixnum?)	($tan-fixnum  x))
      ((bignum?)	($tan-bignum  x))
      ((ratnum?)	($tan-ratnum  x))
      ((compnum?)	($tan-compnum x))
      (else
       (assertion-violation 'tan "expected number as argument" x))))

  (define ($tan-fixnum x)
    (if ($fxzero? x)
	0
      (foreign-call "ikrt_fx_tan" x)))

  (define ($tan-bignum x)
    ($fltan (inexact x)))

  (define ($tan-ratnum x)
    ($fltan (inexact x)))

  (define ($tan-compnum x)
    ;;
    ;; tan (x) = z.rep + i * z.imp
    ;;
    ;;                  sin (2 * x.rep)
    ;; z.rep = ----------------------------------
    ;;         cos (2 * x.rep) + cosh (2 * x.imp)
    ;;
    ;;           tanh (2 * x.imp)
    ;; z.imp = --------------------
    ;;              cos (2 * x.rep)
    ;;         1 + ----------------
    ;;             cosh (2 * x.imp)
    ;;
    (let ((x.rep ($compnum-real x))
	   (x.imp ($compnum-imag x)))
      (let ((R2 (* 2 x.rep))
	    (I2 (* 2 x.imp)))
	(let ((CR2  (cos  R2))
	      (CHI2 (cosh I2)))
	  ($make-rectangular (/ (sin R2)  (+ CR2 CHI2))
			     (/ (tanh I2) (+ 1 (/ CR2 CHI2))))))))

  (define ($tan-cflonum x)
    ;;
    ;; tan (x) = z.rep + i * z.imp
    ;;
    ;;                  sin (2 * x.rep)
    ;; z.rep = ----------------------------------
    ;;         cos (2 * x.rep) + cosh (2 * x.imp)
    ;;
    ;;           tanh (2 * x.imp)
    ;; z.imp = --------------------
    ;;              cos (2 * x.rep)
    ;;         1 + ----------------
    ;;             cosh (2 * x.imp)
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (let ((R2	($fl* 2.0 x.rep))
	    (I2	($fl* 2.0 x.imp)))
	(let ((COSR2	($flcos R2))
	      (COSHI2	($flcosh I2)))
	  ($make-cflonum ($fl/ ($flsin  R2) ($fl+ COSR2 COSHI2))
			 ($fl/ ($fltanh I2) ($fl+ 1.0 ($fl/ COSR2 COSHI2))))))))

  #| end of module: tan |# )


(module (asin
	 $asin-fixnum		$asin-bignum		$asin-ratnum
	 $asin-flonum		$asin-cflonum		$asin-compnum)

  (define (asin x)
    (cond-numeric-operand x
      ((flonum?)	($asin-flonum  x))
      ((cflonum?)	($asin-cflonum x))
      ((fixnum?)	($asin-fixnum  x))
      ((bignum?)	($asin-bignum  x))
      ((ratnum?)	($asin-ratnum  x))
      ((compnum?)	($asin-compnum x))
      (else
       (assertion-violation 'asin "expected number as argument" x))))

  (define ($asin-fixnum x)
    ($asin-flonum (inexact x)))

  (define ($asin-bignum x)
    ($asin-flonum (inexact x)))

  (define ($asin-ratnum x)
    ($asin-flonum (inexact x)))

  (define ($asin-flonum x)
    ;;This is  different from $FLASIN:  $FLASIN returns a  flonum, while
    ;;$ASIN-FLONUM accepts any  flonum as argument and  returns a flonum
    ;;or cflonum.
    ;;
    (cond (($fl> x 1.0)
	   ;;          pi
	   ;; asin x = -- + i * acosh x
	   ;;          2
	   ($make-cflonum PI/2 ($acosh-flonum x)))
	  (($fl< x -1.0)
	   ;;            pi
	   ;; asin x = - -- - i * acosh (- x)
	   ;;            2
	   ($make-cflonum ($fl- PI/2) ($fl- ($acosh-flonum ($fl- x)))))
	  (else
	   ($asin-flonum x))))

  (define ($asin-compnum x)
    ;; asin(x) = z.rep + i * z.imp
    ;;
    ;; A = x.rep^2 + y.rep^2
    ;; B = A - 1
    ;; C = B^2
    ;; D = sqrt(x.imp)
    ;; Q = sqrt(C + 4 * D)
    ;; z.rep = 1/2 * sgn(x.rep) * acos (Q - A)
    ;; z.imp = 1/2 * sgn(x.imp) * acosh(Q + A)
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      (if ($fxzero? x.rep) ;this works with any object
	  ($make-compnum 0 (asinh x.imp))
	(let* (	;;A is a non-negative real number.
	       (A	(+ (sqr x.rep) (sqr x.imp)))
	       ;;B is a real number.
	       (B	(- A 1.0))
	       ;;C is a non-negative real number.
	       (C	(sqr B))
	       ;;D is a non-negative real number.
	       (D	(sqr x.imp))
	       ;;Q is a non-negative real number.
	       (Q	(sqrt (+ C (* 4 D)))))
	  (define (sgn N)
	    (if (negative? N) -1.0 1.0))
	  ($make-rectangular ($flonum*number ($fl* 0.5 (sgn x.rep)) (acos  (- Q A)))
			     ($flonum*number ($fl* 0.5 (sgn x.imp)) (acosh (+ Q A))))))))

  (define ($asin-cflonum x)
    ;; asin(x) = z.rep + i * z.imp
    ;;
    ;; A = x.rep^2 + y.rep^2
    ;; B = A - 1
    ;; C = B^2
    ;; D = sqrt(x.imp)
    ;; Q = sqrt(C + 4 * D)
    ;; z.rep = 1/2 * sgn(x.rep) * acos (Q - A)
    ;; z.imp = 1/2 * sgn(x.imp) * acosh(Q + A)
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (if ($flzero? x.rep)
	  ($make-cflonum 0.0 ($flasinh x.imp))
	(let* (	;;A is a non-negative flonum.
	       (A	($fl+ ($flsqr x.rep) ($flsqr x.imp)))
	       ;;B is a flonum.
	       (B	($fl- A 1.0))
	       ;;C is a non-negative flonum.
	       (C	($flsqr B))
	       ;;D is a non-negative flonum.
	       (D	($flsqr x.imp))
	       ;;Q is a non-negative flonum.
	       (Q	($sqrt-flonum ($fl+ C ($fl* 4.0 D)))))
	  (define (sgn N)
	    (if ($flnegative? N) -1.0 1.0))
	  ($make-cflonum ($fl* ($fl* 0.5 (sgn x.rep)) ($acos-flonum  ($fl- Q A)))
			 ($fl* ($fl* 0.5 (sgn x.imp)) ($acosh-flonum ($fl+ Q A))))))))

  #| end of module: asin |# )


(module (acos
	 $acos-fixnum		$acos-bignum		$acos-ratnum
	 $acos-flonum		$acos-cflonum		$acos-compnum)

  (define (acos x)
    (cond-numeric-operand x
      ((flonum?)	($acos-flonum  x))
      ((cflonum?)	($acos-cflonum x))
      ((fixnum?)	($acos-fixnum  x))
      ((bignum?)	($acos-bignum  x))
      ((ratnum?)	($acos-ratnum  x))
      ((compnum?)	($acos-compnum x))
      (else
       (assertion-violation 'acos "expected number as argument" x))))

  (define ($acos-fixnum x)
    ($acos-flonum (inexact x)))

  (define ($acos-bignum x)
    ($acos-flonum (inexact x)))

  (define ($acos-ratnum x)
    ($acos-flonum (inexact x)))

  (define ($acos-flonum x)
    ;;This is  different from $FLACOS:  $FLACOS returns a  flonum, while
    ;;$ACOS-FLONUM accepts any  flonum as argument and  returns a flonum
    ;;or cflonum.
    ;;
    (cond (($fl> x 1.0)
	   ($make-compnum 0 ($acosh-flonum x)))
	  (($fl< x -1.0)
	   ($make-cflonum PI ($fl- ($acosh-flonum ($fl- x)))))
	  (else
	   ($flacos x))))

  (define ($acos-compnum x)
    ($flonum-number  PI/2 ($asin-compnum x)))

  (define ($acos-cflonum x)
    ($flonum-cflonum PI/2 ($asin-cflonum x)))

  #| end of module: acos |# )


(module (atan
	 $atan2-real-real
	 $atan-fixnum		$atan-ratnum		$atan-bignum
	 $atan-cflonum		$atan-compnum)
  (define who 'atan)

  (define atan
    (case-lambda
     ((x)
      (cond-numeric-operand x
	((flonum?)	($flatan       x))
	((cflonum?)	($atan-cflonum x))
	((fixnum?)	($atan-fixnum  x))
	((bignum?)	($atan-bignum  x))
	((ratnum?)	($atan-ratnum  x))
	((compnum?)	($atan-compnum x))
	(else
	 (assertion-violation who "expected number as argument" x))))
     ((y x)
      (with-arguments-validation (who)
	  ((real	x)
	   (real	y))
	($atan2-real-real x y)))))

  (define ($atan2-real-real x y)
    (foreign-call "ikrt_atan2" (inexact y) (inexact x)))

;;; --------------------------------------------------------------------

  (define ($atan-fixnum x)
    (foreign-call "ikrt_fx_atan" x))

  (define ($atan-bignum x)
    ($flatan (inexact x)))

  (define ($atan-ratnum x)
    ($flatan (inexact x)))

  (define ($atan-compnum x)
    ($atan-cflonum (inexact x)))

  (define ($atan-cflonum x)
    ;;Formula from Wikipedia section "Logarithmic forms":
    ;;
    ;;	<http://en.wikipedia.org/wiki/Arc_tangent>
    ;;
    ;;  atan x = 1/2 * i * (log (1 - i * x) - log (1 + i * x))
    ;;
    (if ($flzero? ($cflonum-imag x))
	($flatan ($cflonum-real x))
      ($cflonum*cflonum +0.5i
			($cflonum-cflonum
			 ($log-cflonum ($flonum-cflonum 1.0 ($cflonum*cflonum +1.0i x)))
			 ($log-cflonum ($flonum+cflonum 1.0 ($cflonum*cflonum +1.0i x)))))))

  #| end of module |# )


(module (sinh
	 $sinh-fixnum		$sinh-bignum		$sinh-ratnum
	 $sinh-compnum		$sinh-cflonum)
  (define who 'sinh)

  (define (sinh x)
    (cond-numeric-operand x
      ((flonum?)	($flsinh       x))
      ((cflonum?)	($sinh-cflonum x))
      ((fixnum?)	($sinh-fixnum  x))
      ((bignum?)	($sinh-bignum  x))
      ((ratnum?)	($sinh-ratnum  x))
      ((compnum?)	($sinh-compnum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($sinh-fixnum x)
    ($flsinh (inexact x)))

  (define ($sinh-bignum x)
    ($flsinh (inexact x)))

  (define ($sinh-ratnum x)
    ($flsinh (inexact x)))

  (define ($sinh-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular (* (sinh x.rep) (cos x.imp))
			 (* (cosh x.rep) (sin x.imp)))))

  (define ($sinh-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($fl* ($flsinh x.rep) ($flcos x.imp))
		     ($fl* ($flcosh x.rep) ($flsin x.imp)))))

  #| end of module |# )


(module (cosh
	 $cosh-fixnum		$cosh-bignum		$cosh-ratnum
	 $cosh-compnum		$cosh-cflonum)
  (define who 'cosh)

  (define (cosh x)
    (cond-numeric-operand x
      ((flonum?)	($flcosh       x))
      ((cflonum?)	($cosh-cflonum x))
      ((fixnum?)	($cosh-fixnum  x))
      ((bignum?)	($cosh-bignum  x))
      ((ratnum?)	($cosh-ratnum  x))
      ((compnum?)	($cosh-compnum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($cosh-fixnum x)
    ($flcosh (inexact x)))

  (define ($cosh-bignum x)
    ($flcosh (inexact x)))

  (define ($cosh-ratnum x)
    ($flcosh (inexact x)))

  (define ($cosh-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular (* (cosh x.rep) (cos x.imp))
			 (* (sinh x.rep) (sin x.imp)))))

  (define ($cosh-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($fl* ($flcosh x.rep) ($flcos x.imp))
		     ($fl* ($flsinh x.rep) ($flsin x.imp)))))

  #| end of module: cosh |# )


(module (tanh
	 $tanh-fixnum		$tanh-bignum		$tanh-ratnum
	 $tanh-compnum		$tanh-cflonum)
  (define who 'tanh)

  (define (tanh x)
    (cond-numeric-operand x
      ((flonum?)	($fltanh       x))
      ((cflonum?)	($tanh-cflonum x))
      ((fixnum?)	($tanh-fixnum  x))
      ((bignum?)	($tanh-bignum  x))
      ((ratnum?)	($tanh-ratnum  x))
      ((compnum?)	($tanh-compnum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($tanh-fixnum x)
    ($fltanh (inexact x)))

  (define ($tanh-bignum x)
    ($fltanh (inexact x)))

  (define ($tanh-ratnum x)
    ($fltanh (inexact x)))

  (define ($tanh-compnum x)
    ;;
    ;; tanh (x) = z.rep + i * z.imp
    ;;
    ;;           tanh (2 * x.imp)
    ;; z.rep = --------------------
    ;;              cos (2 * x.imp)
    ;;         1 + ---------------
    ;;             cosh (2 * x.rep)
    ;;
    ;;                  sin (2 * x.imp)
    ;; z.imp = ----------------------------------
    ;;         cosh (2 * x.rep) + cos (2 * x.imp)
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      (let ((R2 (* 2 x.rep))
	    (I2 (* 2 x.imp)))
	(let ((cos2i  (cos  I2))
	      (cosh2r (cosh R2)))
	  ($make-rectangular (/ (tanh R2) (+ 1 (/ cos2i cosh2r)))
			     (/ (sin I2)  (+ cosh2r cos2i)))))))

  (define ($tanh-cflonum x)
    ;;
    ;; tanh (x) = z.rep + i * z.imp
    ;;
    ;;           tanh (2 * x.imp)
    ;; z.rep = --------------------
    ;;              cos (2 * x.imp)
    ;;         1 + ---------------
    ;;             cosh (2 * x.rep)
    ;;
    ;;                  sin (2 * x.imp)
    ;; z.imp = ----------------------------------
    ;;         cosh (2 * x.rep) + cos (2 * x.imp)
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (let ((R2 ($fl* 2.0 x.rep))
	    (I2 ($fl* 2.0 x.imp)))
	(let ((cos2i  ($flcos  I2))
	      (cosh2r ($flcosh R2)))
	  ($make-cflonum ($fl/ ($fltanh R2) ($fl+ 1.0 ($fl/ cos2i cosh2r)))
			 ($fl/ ($flsin I2)  ($fl+ cosh2r cos2i)))))))

  #| end of module: tanh |# )


(module (asinh
	 $asinh-fixnum		$asinh-bignum		$asinh-ratnum
	 $asinh-cflonum		$asinh-compnum)
  (define who 'asinh)

  (define (asinh x)
    (cond-numeric-operand x
      ((flonum?)	($flasinh x))
      ((cflonum?)	($asinh-cflonum x))
      ((fixnum?)	($asinh-fixnum x))
      ((bignum?)	($asinh-bignum x))
      ((ratnum?)	($asinh-ratnum x))
      ((compnum?)	($asinh-compnum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($asinh-fixnum x)
    ($flasinh (inexact x)))

  (define ($asinh-bignum x)
    ($flasinh (inexact x)))

  (define ($asinh-ratnum x)
    ($flasinh (inexact x)))

  (define ($asinh-compnum x)
    ;; asinh (x) = z.rep + i * z.imp
    ;;
    ;; D = x.imp^2
    ;; A = x.rep^2 + D
    ;; B = A - 1
    ;; C = B^2
    ;; Q = sqrt(C + 4 * D)
    ;;
    ;; z.rep = 1/2 * sgn(x.rep) * acosh (Q + A)
    ;; z.imp = 1/2 * sgn(x.imp) * acos  (Q - A)
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      (if (zero? x.rep)
	  (let ((v (asin x.imp)))
	    ($make-rectangular (imag-part v) (real-part v)))
	(let* (	;;D is a non-negative real number.
	       (D (sqr x.imp))
	       ;;A is a non-negative real number.
	       (A (+ (sqr x.rep) D))
	       ;;B is a real number.
	       (B (- A 1))
	       ;;C is a non-negative real number.
	       (C (sqr B B))
	       ;;Q is a non-negative real number.
	       (Q (sqrt (+ C (* 4 D)))))
	  (define (sgn N)
	    (if (negative? N) -1 1))
	  ($make-rectangular ($flonum*number ($fl* 0.5 (sgn x.rep)) (acosh (+ Q A)))
			     ($flonum*number ($fl* 0.5 (sgn x.imp)) (acos  (- Q A))))))))

  (define ($asinh-cflonum x)
    ;; asinh (x) = z.rep + i * z.imp
    ;;
    ;; D = x.imp^2
    ;; A = x.rep^2 + D
    ;; B = A - 1
    ;; C = B^2
    ;; Q = sqrt(C + 4 * D)
    ;;
    ;; z.rep = 1/2 * sgn(x.rep) * acosh (Q + A)
    ;; z.imp = 1/2 * sgn(x.imp) * acos  (Q - A)
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (if ($flzero? x)
	  (let* ((v     ($asin-flonum x.imp))
		 (real? (flonum? v)))
	    ($make-cflonum (if real? 0.0 ($cflonum-imag v))
			   (if real? v   ($cflonum-real v))))
	(let* (	;;D is a non-negative flonum.
	       (D ($flsqr x.imp))
	       ;;A is a non-negative flonum.
	       (A ($fl+ ($flsqr x.rep) D))
	       ;;B is a flonum.
	       (B ($fl- A 1.0))
	       ;;C is a non-negative flonum.
	       (C ($fl* B B))
	       ;;Q is a non-negative flonum.
	       (Q ($flsqrt (+ C ($fl* 4.0 D)))))
	  (define (sgn N)
	    (if ($flnegative? N) -1.0 1.0))
	  ($make-cflonum ($fl* ($fl* 0.5 (sgn x.rep)) ($acosh-flonum ($fl+ Q A)))
			 ($fl* ($fl* 0.5 (sgn x.imp)) ($acos-flonum  ($fl- Q A))))))))

  #| end of module: asinh |# )


(module (acosh
	 $acosh-fixnum		$acosh-bignum		$acosh-ratnum
	 $acosh-flonum		$acosh-cflonum		$acosh-compnum)
  (define who 'acosh)

  (define (acosh x)
    (cond-numeric-operand x
      ((flonum?)	($acosh-flonum x))
      ((cflonum?)	($acosh-cflonum x))
      ((fixnum?)	($acosh-fixnum x))
      ((bignum?)	($acosh-bignum x))
      ((ratnum?)	($acosh-ratnum x))
      ((compnum?)	($acosh-compnum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($acosh-fixnum x)
    ($acosh-flonum (inexact x)))

  (define ($acosh-bignum x)
    ($acosh-flonum (inexact x)))

  (define ($acosh-ratnum x)
    ($acosh-flonum (inexact x)))

  (define ($acosh-flonum x)
    (cond (($fl>= x +1.0) ; +1 <= X < +inf
	   ($flacosh x))
	  (($fl>= x -1.0) ; -1 <= X < +1
	   ($make-compnum 0 ($flatan2 ($flsqrt ($fl- 1.0 ($flsqr x x))) x)))
	  (($fl< x -1.0)  ; -inf < X < -1
	   ($make-cflonum ($flacosh ($fl- x)) PI))
	  (else +nan.0)))

  (define ($acosh-compnum x)
    ;;
    ;; acosh (x) = 1/2 * sgn(x.rep) * acosh (Q + A) +
    ;;           + i * 1/2 * sgn(x.imp) * (pi - sgn(x.rep) * acos (Q - A))
    ;;
    ;; D = x.imp^2
    ;; A = x.rep^2 + D
    ;; B = A - 1
    ;; C = B ^2
    ;; Q = sqrt (C + 4 * D)
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      (if (zero? x.rep)
	  ($number+compnum (asinh x.imp) ($make-compnum 0 PI/2))
	(let* (	;;D is a non-negative real number.
	       (D (sqr x.imp))
	       ;;A is a non-negative real number.
	       (A (+ (sqr x.rep) D))
	       ;;B is a real number.
	       (B (- A 1))
	       ;;C is a non-negative real number.
	       (C (sqr B))
	       ;;Q is a non-negative real number.
	       (Q (sqrt (+ C (* 4 D)))))
	  (define (sgn x)
	    (if (negative? x) -1.0 1.0))
	  (+ ($flonum*number ($fl* 0.5 (sgn x.rep)) (acosh (+ Q A)))
	     ($cflonum*number ($make-compnum 0 ($fl* 0.5 (sgn x.imp)))
			      (- PI (* (sgn x.rep) (acos (- Q A))))))))))

  (define ($acosh-cflonum x)
    ;;
    ;; acosh (x) = 1/2 * sgn(x.rep) * acosh (Q + A) +
    ;;           + i * 1/2 * sgn(x.imp) * (pi - sgn(x.rep) * acos (Q - A))
    ;;
    ;; D = x.imp^2
    ;; A = x.rep^2 + D
    ;; B = A - 1
    ;; C = B ^2
    ;; Q = sqrt (C + 4 * D)
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (if ($flzero? x.rep)
	  (+ ($flasinh x.imp) ($make-cflonum 0.0 PI/2))
	(let* ( ;;D is a non-negative flonum.
	       (D ($flsqr x.imp))
	       ;;A is a non-negative flonum.
	       (A ($fl+ ($flsqr x.rep) D))
	       ;;B is a flonum.
	       (B ($fl- A 1.0))
	       ;;C is a non-negative flonum.
	       (C ($flsqr B))
	       ;;Q is a non-negative flonum
	       (Q ($flsqrt ($fl+ C ($fl* 4.0 D)))))
	  (define (sgn N)
	    (if ($flnegative? N) -1.0 1.0))
	  (+ ($flonum*number ($fl* 0.5 (sgn x.rep))
			     ($acosh-flonum ($fl+ Q A)))
	     ($cflonum*number ($make-cflonum 0.0 ($fl* 0.5 (sgn x.imp)))
			      (- PI ($flonum*number (sgn x.rep)
						    ($acos-flonum ($fl- Q A))))))))))

  #| end of module |# )


(module (atanh
	 $atanh-fixnum		$atanh-bignum		$atanh-ratnum
	 $atanh-flonum		$atanh-cflonum		$atanh-compnum)
  (define who 'atanh)

  (define (atanh x)
    (cond-numeric-operand x
      ((flonum?)	($atanh-flonum  x))
      ((cflonum?)	($atanh-cflonum x))
      ((fixnum?)	($atanh-fixnum  x))
      ((bignum?)	($atanh-bignum  x))
      ((ratnum?)	($atanh-ratnum  x))
      ((compnum?)	($atanh-compnum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($atanh-fixnum x)
    ($atanh-flonum (inexact x)))

  (define ($atanh-bignum x)
    ($atanh-flonum (inexact x)))

  (define ($atanh-ratnum x)
    ($atanh-flonum (inexact x)))

  (define ($atanh-flonum x)
    ;;This is different from $flatanh: it accepts any flonum as argument
    ;;and it returns a flonum or cflonum.
    ;;
    (if (and ($fl<= x +1.0)
	     ($fl>= x -1.0))
	($flatanh x)
      ($flonum*cflonum 0.5 ($log-flonum ($fl/ ($fl+ 1.0 x)
					      ($fl- 1.0 x))))
      ;; (- ($atanh-flonum ($fl/ 1.0 x))
      ;; 	 (if ($flnegative? x)
      ;; 	     ($fl* -1.0i PI/2)
      ;; 	   ($fl* +1.0i PI/2)))
      ))

  (define ($atanh-compnum x)
    ($atanh-cflonum (inexact x)))

  (define ($atanh-cflonum x)
    ($flonum*cflonum 0.5 ($log-cflonum ($cflonum/cflonum ($flonum+cflonum 1.0 x)
							 ($flonum-cflonum 1.0 x)))))

  #| end of module: atanh |# )


;;;; square roots

(module (sqrt
	 $sqrt-fixnum
	 $sqrt-flonum
	 $sqrt-bignum
	 $sqrt-ratnum
	 $sqrt-compnum
	 $sqrt-cflonum)

  (define (sqrt x)
    (cond-numeric-operand x
      ((fixnum?)	($sqrt-fixnum x))
      ((bignum?)	($sqrt-bignum x))
      ((ratnum?)	($sqrt-ratnum x))
      ((flonum?)	($sqrt-flonum x))
      ((compnum?)	($sqrt-compnum x))
      ((cflonum?)	($sqrt-cflonum x))
      (else
       (assertion-violation 'sqrt "expected number as argument" x))))

  (define ($sqrt-flonum x)
    ;;This can return both a flonum or a compnum!!!
    ;;
    (if ($fl< x 0.0)
	;;This case includes: X = -0.0
	(make-rectangular 0 (foreign-call "ikrt_fl_sqrt" ($fl- x)))
      (foreign-call "ikrt_fl_sqrt" x)))

  (define ($sqrt-fixnum x)
    (if ($fxnegative? x)
	(make-rectangular 0 ($sqrt-fixnum ($fx- x)))
      (let-values (((root residual) ($exact-integer-sqrt/fixnum x)))
	(if ($fxzero? residual)
	    root
	  (foreign-call "ikrt_fx_sqrt" x)))))

  (define ($sqrt-bignum x)
    (if ($bignum-positive? x)
	(let-values (((root residual) ($exact-integer-sqrt/bignum x)))
	  (if (eq? residual 0)
	      root
	    (let ((v ($sqrt-flonum (inexact x))))
	      ;;Could the (dropped) residual ever affect the answer?
	      (if (flinfinite? v)
		  (if (bignum? root)
		      ;;The argument  1 makes it  round up in case  of a
		      ;;tie.
		      (foreign-call "ikrt_bignum_to_flonum" root 1 ($make-flonum))
		    (inexact root))
		v))))
      (make-rectangular 0 ($sqrt-bignum (- x)))))

  (define ($sqrt-ratnum x)
    (/ (sqrt ($ratnum-n x))
       (sqrt ($ratnum-d x))))

  (define ($sqrt-compnum Z)
    ;;The function:
    ;;
    ;;   R = sqrt(Z) = sqrt(Z.rep + i * Z.imp)
    ;;
    ;;is computed as follows:
    ;;
    ;; magn  = sqrt(Z.rep^2 + Z.imp^2)
    ;;
    ;;              magn + Z.rep                         magn - Z.rep
    ;; R.rep = sqrt ------------ + i * sgn(Z.imp) * sqrt -----------
    ;;                   2.                                   2.
    ;;
    ;;See <http://en.wikipedia.org/wiki/Square_root>.
    ;;
    (let ((Z.rep (real-part Z))
	  (Z.imp (imag-part Z)))
      (let ((magn (sqrt (+ (* Z.rep Z.rep)
			   (* Z.imp Z.imp))))
	    (sgn  (if (> Z.imp 0) 1 -1)))
	(make-rectangular (sqrt (/ (+ magn Z.rep) 2))
			  (* sgn (sqrt (/ (- magn Z.rep) 2)))))))

  (define ($sqrt-cflonum Z)
    ;;The function:
    ;;
    ;;   R = sqrt(Z) = sqrt(Z.rep + i * Z.imp)
    ;;
    ;;is computed as follows:
    ;;
    ;; magn  = sqrt(Z.rep^2 + Z.imp^2)
    ;;
    ;;              magn + Z.rep                         magn - Z.rep
    ;; R.rep = sqrt ------------ + i * sgn(Z.imp) * sqrt -----------
    ;;                   2.                                   2.
    ;;
    ;;See <http://en.wikipedia.org/wiki/Square_root>.
    ;;
    (let ((Z.rep (real-part Z))
	  (Z.imp (imag-part Z)))
      ;;Remember  that  $SQRT-FLONUM  can  return both  a  flonum  or  a
      ;;compnum!!!
      (let* ((magn  ($sqrt-flonum ($fl+ ($flsqr Z.rep) ($flsqr Z.imp))))
	     (sgn   (if ($flpositive? Z.imp) 1 -1))
	     (R.rep (sqrt (/ (+ magn Z.rep) 2)))
	     (R.imp (* sgn (sqrt (/ (- magn Z.rep) 2)))))
	(make-rectangular R.rep R.imp))))

  #| end of module: sqrt |# )

;;; --------------------------------------------------------------------

(module (exact-integer-sqrt
	 $exact-integer-sqrt/fixnum
	 $exact-integer-sqrt/bignum)

  (define who 'exact-integer-sqrt)

  (define (exact-integer-sqrt x)
    (cond ((fixnum? x)
	   ($exact-integer-sqrt/fixnum x))

	  ((bignum? x)
	   ($exact-integer-sqrt/bignum x))

	  (else
	   (assertion-violation who "expected exact integer as argument" x))))

  (define ($exact-integer-sqrt/fixnum x)
    (cond (($fx> x 0)
	   (let ((s (foreign-call "ikrt_exact_fixnum_sqrt" x)))
	     (values s ($fx- x ($fx* s s)))))
	  (($fxzero? x)
	   (values 0 0))
	  (else
	   (%error-negative-operand x))))

  (define ($exact-integer-sqrt/bignum x)
    (if ($bignum-positive? x)
	(let ((r (foreign-call "ikrt_exact_bignum_sqrt" x)))
	  (values ($car r) ($cdr r)))
      (%error-negative-operand x)))

  (define (%error-negative-operand x)
    (assertion-violation who "expected non-negative exact integer as argument" x))

  #| end of module: exact-integer-sqrt |# )


(define (numerator x)
  (cond-real-numeric-operand x
    ((fixnum?)		x)
    ((bignum?)		x)
    ((ratnum?)		($ratnum-n x))
    ((flonum?)		($flnumerator x))
    (else
     (assertion-violation 'numerator "expected exact integer as argument" x))))

(define (denominator x)
  (cond-real-numeric-operand x
    ((fixnum?)		1)
    ((bignum?)		1)
    ((ratnum?)		($ratnum-d x))
    ((flonum?)		($fldenominator x))
    (else
     (assertion-violation 'denominator "expected exact integer as argument" x))))


;;;; rounding functions

(define (floor x)
  (define (ratnum-floor x)
    (let ((n (numerator x)) (d (denominator x)))
      (let ((q (quotient n d)))
	(if (>= n 0) q (- q 1)))))
  (cond
   ((flonum? x)
       ;;; optimize for integer flonums
    (let ((e ($flonum->exact x)))
      (cond
       ((ratnum? e)
	(exact->inexact (ratnum-floor e)))
       (else x))))
   ((ratnum? x) (ratnum-floor x))
   ((or (fixnum? x) (bignum? x)) x)
   (else (assertion-violation 'floor "expected number as argument" x))))

(define (ceiling x)
  (define (ratnum-ceiling x)
    (let ((n (numerator x)) (d (denominator x)))
      (let ((q (quotient n d)))
	(if (< n 0) q (+ q 1)))))
  (cond
   ((flonum? x)
       ;;; optimize for integer flonums
    (let ((e ($flonum->exact x)))
      (cond
       ((ratnum? e) (exact->inexact (ratnum-ceiling e)))
       (else x))))
   ((ratnum? x) (ratnum-ceiling x))
   ((or (fixnum? x) (bignum? x)) x)
   (else (assertion-violation 'ceiling "expected number as argument" x))))


(define ($ratnum-round x)
  (let ((n ($ratnum-n x)) (d ($ratnum-d x)))
    (let-values (((q r) (div-and-mod n d)))
      (let ((r2 (+ r r)))
	(cond
	 ((< r2 d) q)
	 ((> r2 d) (+ q 1))
	 (else (if (even? q) q (+ q 1))))))))

(define ($ratnum-truncate x)
  (let ((n ($ratnum-n x)) (d ($ratnum-d x)))
    (quotient n d)))


(define (round x)
  (cond
   ((flonum? x) ($flround x))
   ((ratnum? x) ($ratnum-round x))
   ((or (fixnum? x) (bignum? x)) x)
   (else (assertion-violation 'round "expected number as argument" x))))

(define (truncate x)
    ;;; FIXME: fltruncate should preserve the sign of -0.0.
    ;;;
  (cond
   ((flonum? x)
    (let ((e ($flonum->exact x)))
      (cond
       ((ratnum? e) (exact->inexact ($ratnum-truncate e)))
       (else x))))
   ((ratnum? x) ($ratnum-truncate x))
   ((or (fixnum? x) (bignum? x)) x)
   (else (assertion-violation 'truncate "expected number as argument" x))))


(define (random n)
  (if (fixnum? n)
      (if (fx> n 1)
	  (foreign-call "ikrt_fxrandom" n)
	(if (fx= n 1)
	    0
	  (die 'random "incorrect argument" n)))
    (die 'random "not a fixnum" n)))


(define (shift-right-arithmetic n m who)
  (cond
   ((fixnum? m)
    (cond
     ((fixnum? n)
      (cond
       (($fx>= m 0) ($fxsra n m))
       (else (die who "offset must be non-negative" m))))
     ((bignum? n)
      (cond
       (($fx> m 0)
	(foreign-call "ikrt_bignum_shift_right" n m))
       (($fx= m 0) n)
       (else (die who "offset must be non-negative" m))))
     (else (die who "not an exact integer" n))))
   ((bignum? m)
    (cond
     ((fixnum? n) (if ($fx>= n 0) 0 -1))
     ((bignum? n) (if ($bignum-positive? n) 0 -1))
     (else (die who "not an exact integer" n))))
   (else (die who "not an exact integer offset" m))))

(define (sra n m)
  (shift-right-arithmetic n m 'sra))

(define (shift-left-logical n m who)
  (unless (fixnum? m)
    (die who "shift amount is not a fixnum"))
  (cond
   ((fixnum? n)
    (cond
     (($fx> m 0)
      (foreign-call "ikrt_fixnum_shift_left" n m))
     (($fx= m 0) n)
     (else (die who "offset must be non-negative" m))))
   ((bignum? n)
    (cond
     (($fx> m 0)
      (foreign-call "ikrt_bignum_shift_left" n m))
     (($fx= m 0) n)
     (else (die who "offset must be non-negative" m))))
   (else (die who "not an exact integer" n))))

(define (sll n m)
  (shift-left-logical n m 'sll))

(define (bitwise-arithmetic-shift-right n m)
  (shift-right-arithmetic n m 'bitwise-arithmetic-shift-right))
(define (bitwise-arithmetic-shift-left n m)
  (shift-left-logical n m 'bitwise-arithmetic-shift-left))
(define (bitwise-arithmetic-shift n m)
  (define who 'bitwise-arithmetic-shift)
  (unless (fixnum? m)
    (die who "shift amount is not a fixnum"))
  (cond
   ((fixnum? n)
    (cond
     (($fx> m 0)
      (foreign-call "ikrt_fixnum_shift_left" n m))
     (($fx= m 0) n)
     (else
      (let ((m^ (- m)))
	(unless (fixnum? m^)
	  (die who "shift amount is too big" m))
	($fxsra n m^)))))
   ((bignum? n)
    (cond
     (($fx> m 0)
      (foreign-call "ikrt_bignum_shift_left" n m))
     (($fx= m 0) n)
     (else
      (let ((m^ (- m)))
	(unless (fixnum? m^)
	  (die who "shift amount is too big" m))
	(foreign-call "ikrt_bignum_shift_right" n m^)))))
   (else (die who "not an exact integer" n))))


(define (bitwise-length n)
  (cond
   ((fixnum? n) (fxlength n))
   ((bignum? n) (foreign-call "ikrt_bignum_length" n))
   (else (die 'bitwise-length "not an exact integer" n))))

(define (bitwise-copy-bit n idx bit)
  (define who 'bitwise-copy-bit)
  (define (do-copy-bit n idx bit)
    (case bit
      ((0)
       (cond
	((bitwise-bit-set? n idx)
	 (bitwise-and n (bitwise-not (sll 1 idx))))
	(else n)))
      ((1)
       (cond
	((bitwise-bit-set? n idx) n)
	((>= n 0) (+ n (sll 1 idx)))
	(else
	 (bitwise-not
	  (bitwise-and
	   (bitwise-not n)
	   (bitwise-not (sll 1 idx)))))))
      (else (die who "bit must be either 0 or 1" bit))))
  (cond
   ((fixnum? idx)
    (cond
     ((fx< idx 0)
      (die who "negative bit index" idx))
     ((or (fixnum? n) (bignum? n))
      (do-copy-bit n idx bit))
     (else (die who "not an exact integer" n))))
   ((bignum? idx)
    (unless (or (fixnum? n) (bignum? n))
      (die who "not an exact integer" n))
    (if ($bignum-positive? idx)
	(case bit
	  ((0)
	   (if (>= n 0)
	       n
	     (die who "unrepresentable result")))
	  ((1)
	   (if (< n 0)
	       n
	     (die who "unrepresentable result")))
	  (else (die who "bit must be either 0 or 1" bit)))
      (die who "negative bit index" idx)))
   (else (die who "index is not an exact integer" idx))))

(define (bitwise-bit-field n idx1 idx2)
  (define who 'bitwise-bit-field)
  (cond
   ((and (fixnum? idx1) (fx>= idx1 0))
    (cond
     ((and (fixnum? idx2) (fx>= idx2 0))
      (cond
       ((fx<= idx1 idx2)
	(cond
	 ((or (fixnum? n) (bignum? n))
	  (bitwise-and
	   (sra n idx1)
	   (- (sll 1 (- idx2 idx1)) 1)))
	 (else (die who "not an exact integer" n))))
       (else (die who "invalid order for indices" idx1 idx2))))
     (else
      (if (not (fixnum? idx2))
	  (die who "invalid index" idx2)
	(die who "negative index" idx2)))))
   (else
    (if (not (fixnum? idx1))
	(die who "invalid index" idx1)
      (die who "negative index" idx1)))))


;;;; debugging functions

(define (bignum->bytevector bigN)
  (define who 'bignum->bytevector)
  (with-arguments-validation (who)
      ((bignum	bigN))
    (foreign-call "ikrt_debug_bignum_to_bytevector" bigN)))

(define (bytevector->bignum bv)
  (define who 'bytevector->bignum)
  (with-arguments-validation (who)
      ((bytevector	bv))
    (foreign-call "ikrt_debug_bignum_from_bytevector" bv)))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'cond-numeric-operand 'scheme-indent-function 1)
;; eval: (put 'cond-exact-integer-operand 'scheme-indent-function 1)
;; eval: (put 'cond-real-numeric-operand 'scheme-indent-function 1)
;; End:
