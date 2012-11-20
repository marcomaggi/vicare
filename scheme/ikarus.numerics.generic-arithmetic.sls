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
    + - * / = < <= > >=
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
    expt
    sqrt			exact-integer-sqrt

    ;; logarithms and exponentials
    log exp

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
    $sqrt/fixnum
    $sqrt/flonum
    $sqrt/bignum
    $sqrt/ratnum
    $sqrt/compnum
    $sqrt/cflonum

    $exact-integer-sqrt/fixnum
    $exact-integer-sqrt/bignum
    )
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
		  expt
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
	    $fxnegative?)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Nov 18, 2012)
    (only (ikarus fixnums)
	  $fxnegative?)
    (except (ikarus system $flonums)
	    $flonum->exact
	    $flzero?
	    $flzero?/negative
	    $flpositive?
	    $flnegative?
	    $flsqr
	    $flround)
    ;;FIXME  To be  removed at  the  next boot  image rotation.   (Marco
    ;;Maggi; Nov 17, 2012)
    (only (ikarus flonums)
	  $flonum->exact
	  $flzero?
	  $flzero?/negative
	  $flpositive?
	  $flnegative?
	  $flsqr
          $flround)
    (ikarus system $ratnums)
    (ikarus system $bignums)
    (ikarus system $compnums)
    (ikarus system $chars)
    (ikarus system $strings)
    (vicare arguments validation)
    (vicare syntactic-extensions))


(define (bignum->flonum x)
  (foreign-call "ikrt_bignum_to_flonum" x 0 ($make-flonum)))

;; (define (ratnum->flonum x)
;;   (define (->flonum n d)
;;     (let-values (((q r) (quotient+remainder n d)))
;;       (if (= r 0)
;;           (inexact q)
;;           (if (= q 0)
;;               (/ (->flonum d n))
;;               (+ q (->flonum r d))))))
;;   (let ((n (numerator x)) (d (denominator x)))
;;     (let ((b (bitwise-first-bit-set n)))
;;       (if (eqv? b 0)
;;           (let ((b (bitwise-first-bit-set d)))
;;             (if (eqv? b 0)
;;                 (->flonum n d)
;;                 (/ (->flonum n (bitwise-arithmetic-shift-right d b))
;;                    (expt 2.0 b))))
;;           (* (->flonum (bitwise-arithmetic-shift-right n b) d)
;;              (expt 2.0 b))))))

;; (define (ratnum->flonum x)
;;   (let f ((n ($ratnum-n x)) (d ($ratnum-d x)))
;;     (let-values (((q r) (quotient+remainder n d)))
;;       (if (= q 0)
;;           (/ 1.0 (f d n))
;;           (if (= r 0)
;;               (inexact q)
;;               (+ q (f r d)))))))

;; (define (ratnum->flonum num)
;;   (define (rat n m)
;;     (let-values (((q r) (quotient+remainder n m)))
;;        (if (= r 0)
;;            (inexact q)
;;            (fl+ (inexact q) (fl/ 1.0 (rat  m r))))))
;;   (define (pos n d)
;;     (cond
;;       ((even? n)
;;        (* (pos (sra n 1) d) 2.0))
;;       ((even? d)
;;        (/ (pos n (sra d 1)) 2.0))
;;       ((> n d) (rat n d))
;;       (else
;;        (/ (rat d n)))))
;;   (let ((n ($ratnum-n num)) (d ($ratnum-d num)))
;;     (if (> n 0)
;;         (pos n d)
;;         (- (pos (- n) d)))))

(define (ratnum->flonum num)
  (define *precision* 53)
  (define (long-div1 n d)
    (let-values (((q r) (quotient+remainder n d)))
      (cond
       ((< (* r 2) d) (inexact q))
       (else (inexact (+ q 1)))
		;(else (error #f "invalid" n d q r))
       )))
  (define (long-div2 n d bits)
    (let f ((bits bits) (ac (long-div1 n d)))
      (cond
       ((= bits 0) ac)
       (else (f (- bits 1) (/ ac 2.0))))))
  (define (pos n d)
    (let ((nbits (bitwise-length n))
	  (dbits (bitwise-length d)))
      (let ((diff-bits (- nbits dbits)))
	(if (>= diff-bits *precision*)
	    (long-div1 n d)
	  (let ((extra-bits (- *precision* diff-bits)))
	    (long-div2 (sll n extra-bits) d extra-bits))))))
  (let ((n ($ratnum-n num)) (d ($ratnum-d num)))
    (if (> n 0)
	(pos n d)
      (- (pos (- n) d)))))

(define (err who x)
  (die who (if (number? x) "invalid argument" "not a number") x))


(define binary+
  (lambda (x y)
    (cond
     ((fixnum? x)
      (cond
       ((fixnum? y)
	(foreign-call "ikrt_fxfxplus" x y))
       ((bignum? y)
	(foreign-call "ikrt_fxbnplus" x y))
       ((flonum? y)
	($fl+ ($fixnum->flonum x) y))
       ((ratnum? y)
	($make-ratnum
	 (+ (* x ($ratnum-d y)) ($ratnum-n y))
	 ($ratnum-d y)))
       ((compnum? y)
	($make-compnum
	 (binary+ x ($compnum-real y))
	 ($compnum-imag y)))
       ((cflonum? y)
	($make-cflonum
	 (binary+ x ($cflonum-real y))
	 ($fl+ 0.0 ($cflonum-imag y))))
       (else (err '+ y))))
     ((bignum? x)
      (cond
       ((fixnum? y)
	(foreign-call "ikrt_fxbnplus" y x))
       ((bignum? y)
	(foreign-call "ikrt_bnbnplus" x y))
       ((flonum? y)
	($fl+ (bignum->flonum x) y))
       ((ratnum? y)
	($make-ratnum
	 (+ (* x ($ratnum-d y)) ($ratnum-n y))
	 ($ratnum-d y)))
       ((compnum? y)
	($make-compnum
	 (binary+ x ($compnum-real y))
	 ($compnum-imag y)))
       ((cflonum? y)
	($make-cflonum
	 (binary+ x ($cflonum-real y))
	 ($cflonum-imag y)))
       (else (err '+ y))))
     ((flonum? x)
      (cond
       ((fixnum? y)
	($fl+ x ($fixnum->flonum y)))
       ((bignum? y)
	($fl+ x (bignum->flonum y)))
       ((flonum? y)
	($fl+ x y))
       ((ratnum? y)
	($fl+ x (ratnum->flonum y)))
       ((cflonum? y)
	($make-cflonum
	 ($fl+ x ($cflonum-real y))
	 ($cflonum-imag y)))
       ((compnum? y)
	($make-cflonum
	 (binary+ x ($compnum-real y))
	 (inexact ($compnum-imag y))))
       (else (err '+ y))))
     ((ratnum? x)
      (cond
       ((or (fixnum? y) (bignum? y))
	($make-ratnum
	 (+ (* y ($ratnum-d x)) ($ratnum-n x))
	 ($ratnum-d x)))
       ((flonum? y)
	($fl+ y (ratnum->flonum x)))
       ((ratnum? y)
	(let ((n0 ($ratnum-n x)) (n1 ($ratnum-n y))
	      (d0 ($ratnum-d x)) (d1 ($ratnum-d y)))
            ;; FIXME: inefficient
	  (/ (+ (* n0 d1) (* n1 d0)) (* d0 d1))))
       ((compnum? y)
	($make-compnum
	 (binary+ x ($compnum-real y))
	 ($compnum-imag y)))
       ((cflonum? y)
	($make-cflonum
	 (binary+ x ($cflonum-real y))
	 ($cflonum-imag y)))
       (else (err '+ y))))
     ((compnum? x)
      (cond
       ((or (fixnum? y) (bignum? y) (ratnum? y))
	($make-compnum
	 (binary+ ($compnum-real x) y)
	 ($compnum-imag x)))
       ((compnum? y)
	($make-rectangular
	 (binary+ ($compnum-real x) ($compnum-real y))
	 (binary+ ($compnum-imag x) ($compnum-imag y))))
       ((flonum? y)
	($make-cflonum
	 (binary+ y ($compnum-real x))
	 (inexact ($compnum-imag x))))
       ((cflonum? y)
	($make-cflonum
	 (binary+ ($compnum-real x) ($cflonum-real y))
	 (binary+ ($compnum-imag x) ($cflonum-imag y))))
       (else (err '+ y))))
     ((cflonum? x)
      (cond
       ((cflonum? y)
	($make-cflonum
	 (binary+ ($cflonum-real x) ($cflonum-real y))
	 (binary+ ($cflonum-imag x) ($cflonum-imag y))))
       ((flonum? y)
	($make-cflonum
	 ($fl+ ($cflonum-real x) y)
	 ($cflonum-imag x)))
       ((or (fixnum? y) (bignum? y) (ratnum? y))
	($make-compnum
	 (binary+ ($compnum-real x) y)
	 ($compnum-imag x)))
       ((compnum? y)
	($make-cflonum
	 (binary+ ($cflonum-real x) ($compnum-real y))
	 (binary+ ($cflonum-imag x) ($compnum-imag y))))
       (else (err '+ y))))
     (else (err '+ x)))))


;;;; binary bitwise operations

(define binary-bitwise-and
  (lambda (x y)
    (cond
     ((fixnum? x)
      (cond
       ((fixnum? y) ($fxlogand x y))
       ((bignum? y)
	(foreign-call "ikrt_fxbnlogand" x y))
       (else
	(die 'bitwise-and "not an exact integer" y))))
     ((bignum? x)
      (cond
       ((fixnum? y)
	(foreign-call "ikrt_fxbnlogand" y x))
       ((bignum? y)
	(foreign-call "ikrt_bnbnlogand" x y))
       (else
	(die 'bitwise-and "not an exact integer" y))))
     (else (die 'bitwise-and "not an exact integer" x)))))

(define binary-bitwise-ior
  (lambda (x y)
    (cond
     ((fixnum? x)
      (cond
       ((fixnum? y) ($fxlogor x y))
       ((bignum? y)
	(foreign-call "ikrt_fxbnlogor" x y))
       (else
	(die 'bitwise-ior "not an exact integer" y))))
     ((bignum? x)
      (cond
       ((fixnum? y)
	(foreign-call "ikrt_fxbnlogor" y x))
       ((bignum? y)
	(foreign-call "ikrt_bnbnlogor" x y))
       (else
	(die 'bitwise-ior "not an exact integer" y))))
     (else (die 'bitwise-ior "not an exact integer" x)))))


(define binary-bitwise-xor
  (lambda (x y)
    (define (fxbn x y)
      (let ((y0 (bitwise-and y (greatest-fixnum)))
	    (y1 (bitwise-arithmetic-shift-right y (- (fixnum-width) 1))))
	(bitwise-ior
	 ($fxlogand ($fxlogxor x y0) (greatest-fixnum))
	 (bitwise-arithmetic-shift-left
	  (bitwise-arithmetic-shift-right
	   (if ($fx>= x 0) y (bitwise-not y))
	   (- (fixnum-width) 1))
	  (- (fixnum-width) 1)))))
    (define (bnbn x y)
      (let ((x0 (bitwise-and x (greatest-fixnum)))
	    (x1 (bitwise-arithmetic-shift-right x (- (fixnum-width) 1)))
	    (y0 (bitwise-and y (greatest-fixnum)))
	    (y1 (bitwise-arithmetic-shift-right y (- (fixnum-width) 1))))
	(bitwise-ior
	 ($fxlogand ($fxlogxor x0 y0) (greatest-fixnum))
	 (bitwise-arithmetic-shift-left
	  (binary-bitwise-xor x1 y1)
	  (- (fixnum-width) 1)))))
    (cond
     ((fixnum? x)
      (cond
       ((fixnum? y) ($fxlogxor x y))
       ((bignum? y) (fxbn x y))
       (else
	(die 'bitwise-xor "not an exact integer" y))))
     ((bignum? x)
      (cond
       ((fixnum? y) (fxbn y x))
       ((bignum? y) (bnbn x y))
       (else
	(die 'bitwise-xor "not an exact integer" y))))
     (else (die 'bitwise-xor "not an exact integer" x)))))


(define binary-
  (lambda (x y)
    (cond
     ((fixnum? x)
      (cond
       ((fixnum? y)
	(foreign-call "ikrt_fxfxminus" x y))
       ((bignum? y)
	(foreign-call "ikrt_fxbnminus" x y))
       ((flonum? y)
	(if ($fx= x 0)
	    ($fl* y -1.0)
	  ($fl- ($fixnum->flonum x) y)))
       ((ratnum? y)
	(let ((n ($ratnum-n y)) (d ($ratnum-d y)))
	  (binary/ (binary- (binary* d x) n) d)))
       ((compnum? y)
	($make-compnum
	 (binary- x ($compnum-real y))
	 (binary- 0 ($compnum-imag y))))
       ((cflonum? y)
	($make-cflonum
	 (binary- x ($cflonum-real y))
	 ($fl- 0.0 ($cflonum-imag y))))
       (else (err '- y))))
     ((bignum? x)
      (cond
       ((fixnum? y)
	(foreign-call "ikrt_bnfxminus" x y))
       ((bignum? y)
	(foreign-call "ikrt_bnbnminus" x y))
       ((flonum? y)
	($fl- (bignum->flonum x) y))
       ((ratnum? y)
	(let ((n ($ratnum-n y)) (d ($ratnum-d y)))
	  (binary/ (binary- (binary* d x) n) d)))
       ((compnum? y)
	($make-compnum
	 (binary- x ($compnum-real y))
	 (binary- 0 ($compnum-imag y))))
       ((cflonum? y)
	($make-cflonum
	 (binary- x ($cflonum-real y))
	 ($fl- 0.0 ($cflonum-imag y))))
       (else (err '- y))))
     ((flonum? x)
      (cond
       ((flonum? y)
	($fl- x y))
       ((cflonum? y)
	($make-cflonum
	 ($fl- x ($cflonum-real y))
	 ($fl- 0.0 ($cflonum-imag y))))
       ((fixnum? y)
	($fl- x ($fixnum->flonum y)))
       ((bignum? y)
	($fl- x (bignum->flonum y)))
       ((ratnum? y)
	(let ((n ($ratnum-n y)) (d ($ratnum-d y)))
	  (binary/ (binary- (binary* d x) n) d)))
       ((compnum? y)
	($make-cflonum
	 (binary- x ($compnum-real y))
	 (binary- 0.0 ($compnum-imag y))))
       (else (err '- y))))
     ((ratnum? x)
      (let ((nx ($ratnum-n x)) (dx ($ratnum-d x)))
	(cond
	 ((or (fixnum? y) (bignum? y) (flonum? y))
	  (binary/ (binary- nx (binary* dx y)) dx))
	 ((ratnum? y)
	  (let ((ny ($ratnum-n y)) (dy ($ratnum-d y)))
	    (binary/ (binary- (binary* nx dy) (binary* ny dx))
		     (binary* dx dy))))
	 ((compnum? y)
	  ($make-compnum
	   (binary- x ($compnum-real y))
	   (binary- 0 ($compnum-imag y))))
	 ((cflonum? y)
	  ($make-cflonum
	   (binary- x ($cflonum-real y))
	   ($fl- 0.0 ($cflonum-imag y))))
	 (else (err '- y)))))
     ((compnum? x)
      (cond
       ((or (fixnum? y) (bignum? y) (ratnum? y))
	($make-compnum
	 (binary- ($compnum-real x) y)
	 ($compnum-imag x)))
       ((compnum? y)
	($make-rectangular
	 (binary- ($compnum-real x) ($compnum-real y))
	 (binary- ($compnum-imag x) ($compnum-imag y))))
       ((flonum? y)
	($make-cflonum
	 (binary- ($compnum-real x) y)
	 (binary- ($compnum-imag x) 0.0)))
       ((cflonum? y)
	($make-cflonum
	 (binary- ($compnum-real x) ($cflonum-real y))
	 (binary- ($compnum-imag x) ($cflonum-imag y))))
       (else
	(err '- y))))
     ((cflonum? x)
      (cond
       ((flonum? y)
	($make-cflonum
	 ($fl- ($cflonum-real x) y)
	 ($cflonum-imag x)))
       ((cflonum? y)
	($make-cflonum
	 (binary- ($cflonum-real x) ($cflonum-real y))
	 (binary- ($cflonum-imag x) ($cflonum-imag y))))
       ((or (fixnum? y) (bignum? y) (ratnum? y))
	($make-cflonum
	 (binary- ($cflonum-real x) y)
	 ($cflonum-imag x)))
       ((compnum? y)
	($make-cflonum
	 (binary- ($cflonum-real x) ($compnum-real y))
	 (binary- ($cflonum-imag x) ($compnum-imag y))))
       (else
	(err '- y))))
     (else (err '- x)))))


(module (binary*
	 $fixnum*Y		$bignum*Y		$flonum*Y
	 $ratnum*Y		$compnum*Y		$cflonum*Y
	 $fixnum*fixnum		$fixnum*bignum		$fixnum*flonum
	 $fixnum*ratnum		$fixnum*compnum		$fixnum*cflonum
	 $bignum*fixnum		$bignum*bignum		$bignum*flonum
	 $bignum*ratnum		$bignum*compnum		$bignum*cflonum
	 $flonum*flonum		$flonum*cflonum		$flonum*fixnum
	 $flonum*bignum		$flonum*ratnum		$flonum*compnum
	 $ratnum*ratnum		$ratnum*compnum		$ratnum*cflonum
	 $compnum*fixnum	$compnum*bignum		$compnum*ratnum
	 $compnum*flonum	$compnum*compnum	$compnum*cflonum
	 $cflonum*fixnum	$cflonum*bignum		$cflonum*ratnum
	 $cflonum*flonum	$cflonum*compnum	$cflonum*cflonum)
  (define who (quote *))

  (define (binary* x y)
    (cond-numeric-operand x
      ((fixnum?)	($fixnum*Y  x y))
      ((bignum?)	($bignum*Y  x y))
      ((flonum?)	($flonum*Y  x y))
      ((ratnum?)	($ratnum*Y  x y))
      ((compnum?)	($compnum*Y x y))
      ((cflonum?)	($cflonum*Y x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($fixnum*Y x y)
    (cond-numeric-operand y
      ((fixnum?)	($fixnum*fixnum  x y))
      ((bignum?)	($fixnum*bignum  x y))
      ((flonum?)	($fixnum*flonum  x y))
      ((ratnum?)	($fixnum*ratnum  x y))
      ((compnum?)	($fixnum*compnum x y))
      ((cflonum?)	($fixnum*cflonum x y))
      (else
       (err who y))))

  (define ($bignum*Y x y)
    (cond-numeric-operand y
      ((fixnum?)	($bignum*fixnum  x y))
      ((bignum?)	($bignum*bignum  x y))
      ((flonum?)	($bignum*flonum  x y))
      ((ratnum?)	($bignum*ratnum  x y))
      ((compnum?)	($bignum*compnum x y))
      ((cflonum?)	($bignum*cflonum x y))
      (else
       (err who y))))

  (define ($flonum*Y x y)
    (cond-numeric-operand y
      ((flonum?)	($flonum*flonum  x y))
      ((cflonum?)	($flonum*cflonum x y))
      ((fixnum?)	($flonum*fixnum  x y))
      ((bignum?)	($flonum*bignum  x y))
      ((ratnum?)	($flonum*ratnum  x y))
      ((compnum?)	($flonum*compnum x y))
      (else
       (err who y))))

  (define ($ratnum*Y x y)
    (cond ((ratnum?  y)		($ratnum*ratnum  x y))
	  ((compnum? y)		($ratnum*compnum x y))
	  ((cflonum? y)		($ratnum*cflonum x y))
	  (else
	   (binary* y x))))

  (define ($compnum*Y x y)
    (cond-numeric-operand y
      ((fixnum?)	($compnum*fixnum  x y))
      ((bignum?)	($compnum*bignum  x y))
      ((ratnum?)	($compnum*ratnum  x y))
      ((flonum?)	($compnum*flonum  x y))
      ((compnum?)	($compnum*compnum x y))
      ((cflonum?)	($compnum*cflonum x y))
      (else
       (err who y))))

  (define ($cflonum*Y x y)
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

  (define ($fixnum*fixnum x y)
    (foreign-call "ikrt_fxfxmult" x y))

  (define ($fixnum*bignum x y)
    (foreign-call "ikrt_fxbnmult" x y))

  (define ($fixnum*flonum x y)
    ($fl* ($fixnum->flonum x) y))

  (define ($fixnum*ratnum x y)
    (binary/ ($fixnum*Y x ($ratnum-n y))
	     ($ratnum-d y)))

  (define ($fixnum*compnum x y)
    ($make-rectangular ($fixnum*Y x ($compnum-real y))
		       ($fixnum*Y x ($compnum-imag y))))

  (define ($fixnum*cflonum x y)
    ($make-cflonum ($fixnum*flonum x ($cflonum-real y))
		   ($fixnum*flonum x ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($bignum*fixnum x y)
    (foreign-call "ikrt_fxbnmult" y x))

  (define ($bignum*bignum x y)
    (foreign-call "ikrt_bnbnmult" x y))

  (define ($bignum*flonum x y)
    ($fl* (bignum->flonum x) y))

  (define ($bignum*ratnum x y)
    (binary/ ($bignum*Y x ($ratnum-n y))
	     ($ratnum-d y)))

  (define ($bignum*compnum x y)
    ($make-rectangular ($bignum*Y x ($compnum-real y))
		       ($bignum*Y x ($compnum-imag y))))

  (define ($bignum*cflonum x y)
    ($make-cflonum ($bignum*flonum x ($cflonum-real y))
		   ($bignum*flonum x ($cflonum-imag y))))

;;; --------------------------------------------------------------------
;;; flonum * Y = flonum or cflonum

  (define ($flonum*flonum x y)
    ($fl* x y))

  (define ($flonum*cflonum x y)
    ($make-cflonum ($fl* x ($cflonum-real y))
		   ($fl* x ($cflonum-imag y))))

  (define ($flonum*fixnum x y)
    ($fl* x ($fixnum->flonum y)))

  (define ($flonum*bignum x y)
    ($fl* x (bignum->flonum y)))

  (define ($flonum*ratnum x y)
    (binary/ ($flonum*Y x ($ratnum-n y))
	     ($ratnum-d y)))

  (define ($flonum*compnum x y)
    ($make-cflonum ($flonum*Y x ($compnum-real y))
		   ($flonum*Y x ($compnum-imag y))))

;;; --------------------------------------------------------------------

  (define ($ratnum*ratnum x y)
    (binary/ (binary* ($ratnum-n x) ($ratnum-n y))
	     (binary* ($ratnum-d x) ($ratnum-d y))))

  (define ($ratnum*compnum x y)
    ($make-rectangular ($ratnum*Y x ($compnum-real y))
		       ($ratnum*Y x ($compnum-imag y))))

  (define ($ratnum*cflonum x y)
    ($make-cflonum ($flonum*ratnum ($cflonum-real y) x)
		   ($flonum*ratnum ($cflonum-imag y) x)))

;;; --------------------------------------------------------------------

  (define ($compnum*fixnum x y)
    ($make-rectangular ($fixnum*Y y ($compnum-real x))
		       ($fixnum*Y y ($compnum-imag x))))

  (define ($compnum*bignum x y)
    ($make-rectangular ($bignum*Y y ($compnum-real x))
		       ($bignum*Y y ($compnum-imag x))))

  (define ($compnum*ratnum x y)
    ($make-rectangular ($ratnum*Y y ($compnum-real x))
		       ($ratnum*Y y ($compnum-imag x))))

  (define ($compnum*flonum x y)
    ($make-cflonum ($flonum*Y y ($compnum-real x))
		   ($flonum*Y y ($compnum-imag x))))

  (define ($compnum*compnum x y)
    (let ((x.rep ($compnum-real x))
	  (y.rep ($compnum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($compnum-imag y)))
      ($make-rectangular (binary- (binary* x.rep y.rep) (binary* x.imp y.imp))
			 (binary+ (binary* x.rep y.imp) (binary* x.imp y.rep)))))

  (define ($compnum*cflonum x y)
    (let ((x.rep ($compnum-real x))
	  (y.rep ($cflonum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($cflonum-imag y)))
      ($make-rectangular (binary- ($flonum*Y y.rep x.rep) ($flonum*Y y.imp x.imp))
			 (binary+ ($flonum*Y y.imp x.rep) ($flonum*Y y.rep x.imp)))))

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
    (let ((x.rep ($compnum-real x))
	  (y.rep ($compnum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($compnum-imag y)))
      ($make-rectangular (binary- ($flonum*Y x.rep y.rep) ($flonum*Y x.imp y.imp))
			 (binary+ ($flonum*Y x.rep y.imp) ($flonum*Y x.imp y.rep)))))

  (define ($cflonum*cflonum x y)
    (let ((r0 ($cflonum-real x))
	  (r1 ($cflonum-real y))
	  (i0 ($cflonum-imag x))
	  (i1 ($cflonum-imag y)))
      ($make-cflonum ($fl- ($fl* r0 r1) ($fl* i0 i1))
		     ($fl+ ($fl* r0 i1) ($fl* i0 r1)))))

  #| end of module: binary* |# )


(define +
  (case-lambda
   ((x y) (binary+ x y))
   ((x y z) (binary+ (binary+ x y) z))
   ((a)
    (cond
     ((fixnum? a) a)
     ((number? a) a)
     (else (die '+ "not a number" a))))
   (() 0)
   ((a b c d . e*)
    (let f ((ac (binary+ (binary+ (binary+ a b) c) d))
	    (e* e*))
      (cond
       ((null? e*) ac)
       (else (f (binary+ ac (car e*)) (cdr e*))))))))


;;;; bitwise operations

(define bitwise-and
  (case-lambda
   ((x y) (binary-bitwise-and x y))
   ((x y z) (binary-bitwise-and (binary-bitwise-and x y) z))
   ((a)
    (cond
     ((fixnum? a) a)
     ((bignum? a) a)
     (else (die 'bitwise-and "not a number" a))))
   (() -1)
   ((a b c d . e*)
    (let f ((ac (binary-bitwise-and a
				    (binary-bitwise-and b
							(binary-bitwise-and c d))))
	    (e* e*))
      (cond
       ((null? e*) ac)
       (else (f (binary-bitwise-and ac (car e*)) (cdr e*))))))))

(define bitwise-ior
  (case-lambda
   ((x y) (binary-bitwise-ior x y))
   ((x y z) (binary-bitwise-ior (binary-bitwise-ior x y) z))
   ((a)
    (cond
     ((fixnum? a) a)
     ((bignum? a) a)
     (else (die 'bitwise-ior "not a number" a))))
   (() 0)
   ((a b c d . e*)
    (let f ((ac (binary-bitwise-ior a
				    (binary-bitwise-ior b
							(binary-bitwise-ior c d))))
	    (e* e*))
      (cond
       ((null? e*) ac)
       (else (f (binary-bitwise-ior ac (car e*)) (cdr e*))))))))

(define bitwise-xor
  (case-lambda
   ((x y) (binary-bitwise-xor x y))
   ((x y z) (binary-bitwise-xor (binary-bitwise-xor x y) z))
   ((a)
    (cond
     ((fixnum? a) a)
     ((bignum? a) a)
     (else (die 'bitwise-xor "not a number" a))))
   (() 0)
   ((a b c d . e*)
    (let f ((ac (binary-bitwise-xor a
				    (binary-bitwise-xor b
							(binary-bitwise-xor c d))))
	    (e* e*))
      (cond
       ((null? e*) ac)
       (else (f (binary-bitwise-xor ac (car e*)) (cdr e*))))))))

(define (bitwise-not x)
  (cond
   ((fixnum? x) ($fxlognot x))
   ((bignum? x) (foreign-call "ikrt_bnlognot" x))
   (else (die 'bitwise-not "invalid argument" x))))

(define (bitwise-if x y z)
  (define who 'bitwise-if)
  (define (err x) (die who "not an exact integer" x))
  (unless (or (fixnum? x) (bignum? x)) (err x))
  (unless (or (fixnum? y) (bignum? y)) (err y))
  (unless (or (fixnum? z) (bignum? z)) (err z))
  (bitwise-ior
   (bitwise-and x y)
   (bitwise-and (bitwise-not x) z)))

(define (bitwise-copy-bit-field x i j n)
  (define who 'bitwise-copy-bit-field)
  (define (err x) (die who "not an exact integer" x))
  (define (err2 x) (die who "index must be nonnegative" x))
  (define (err3 x y) (die who "indices must be in nondescending order" x y))
  (unless (or (fixnum? x) (bignum? x)) (err x))
  (unless (or (fixnum? i) (bignum? i)) (err i))
  (unless (or (fixnum? j) (bignum? j)) (err j))
  (unless (or (fixnum? n) (bignum? n)) (err n))
  (when (< i 0) (err2 i))
  (when (< j i) (err3 i j))
  (bitwise-if (sll (sub1 (sll 1 (- j i))) i) (sll n i) x))

(define (bitwise-reverse-bit-field N start end)
  (define who 'bitwise-reverse-bit-field)
  (%assert-argument-is-exact-integer who N)
  (%assert-argument-is-exact-non-negative-integer who start)
  (%assert-argument-is-exact-non-negative-integer who end)
  (%assert-arguments-are-start-and-end-bit-offsets who start end)
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
      N)))

(define (bitwise-rotate-bit-field N start end count)
  (define who 'bitwise-rotate-bit-field)
  (%assert-argument-is-exact-integer who N)
  (%assert-argument-is-exact-non-negative-integer who start)
  (%assert-argument-is-exact-non-negative-integer who end)
  (%assert-argument-is-exact-non-negative-integer who count)
  (%assert-arguments-are-start-and-end-bit-offsets who start end)

  (let ((width (- end start)))
    (if (positive? width)
	(let* ((count	(mod count width))
	       (field0	(bitwise-bit-field N start end))
	       (field1	(bitwise-arithmetic-shift-left field0 count))
	       (field2	(bitwise-arithmetic-shift-right field0 (- width count)))
	       (field	(bitwise-ior field1 field2)))
	  (bitwise-copy-bit-field N start end field))
      N)))

(define (%assert-argument-is-exact-integer who obj)
  (unless (and (integer? obj) (exact? obj))
    (assertion-violation who "expected exact integer as argument" obj)))

(define (%assert-argument-is-exact-non-negative-integer who obj)
  (%assert-argument-is-exact-integer who obj)
  (unless (<= 0 obj)
    (assertion-violation who "expected non negative exact integer as argument" obj)))

(define (%assert-arguments-are-start-and-end-bit-offsets who start end)
  (unless (<= start end)
    (assertion-violation who
      "expected start bit offset less than or equal to end bit offset" start end)))


(define -
  (case-lambda
   ((x y) (binary- x y))
   ((x y z) (binary- (binary- x y) z))
   ((a) (binary- 0 a))
   ((a b c d . e*)
    (let f ((ac (binary- (binary- (binary- a b) c) d))
	    (e* e*))
      (cond
       ((null? e*) ac)
       (else (f (binary- ac (car e*)) (cdr e*))))))))

(define *
  (case-lambda
   ((x y) (binary* x y))
   ((x y z) (binary* (binary* x y) z))
   ((a)
    (cond
     ((fixnum? a) a)
     ((number? a) a)
     (else (die '* "not a number" a))))
   (() 1)
   ((a b c d . e*)
    (let f ((ac (binary* (binary* (binary* a b) c) d))
	    (e* e*))
      (cond
       ((null? e*) ac)
       (else (f (binary* ac (car e*)) (cdr e*))))))))

(define (binary-gcd x y)
  (define (gcd x y)
    (cond
     (($fx= y 0) x)
     (else (gcd y (remainder x y)))))
  (let ((x (if (< x 0) (- x) x))
	(y (if (< y 0) (- y) y)))
    (cond
     ((> x y) (gcd x y))
     ((< x y) (gcd y x))
     (else x))))

(define gcd
  (case-lambda
   ((x y)
    (cond
     ((or (fixnum? x) (bignum? x))
      (cond
       ((or (fixnum? y) (bignum? y))
	(binary-gcd x y))
       ((number? y)
	(die 'gcd "not an exact integer" y))
       (else
	(die 'gcd "not a number" y))))
     ((number? x)
      (die 'gcd "not an exact integer" x))
     (else
      (die 'gcd "not a number" x))))
   ((x)
    (cond
     ((or (fixnum? x) (bignum? x)) x)
     ((number? x)
      (die 'gcd "not an exact integer" x))
     (else
      (die 'gcd "not a number" x))))
   (() 0)
   ((x y z . ls)
    (let f ((g (gcd (gcd x y) z)) (ls ls))
      (cond
       ((null? ls) g)
       (else (f (gcd g (car ls)) (cdr ls))))))))


(define lcm
  (case-lambda
   ((x y)
    (cond
     ((or (fixnum? x) (bignum? x))
      (cond
       ((or (fixnum? y) (bignum? y))
	(let ((x (if (< x 0) (- x) x))
	      (y (if (< y 0) (- y) y)))
	  (let ((g (binary-gcd x y)))
	    (binary* y (quotient x g)))))
       ((flonum? y)
	(let ((v ($flonum->exact y)))
	  (cond
	   ((or (fixnum? v) (bignum? v))
	    (inexact (lcm x v)))
	   (else (die 'lcm "not an integer" y)))))
       (else
	(die 'lcm "not an integer" y))))
     ((flonum? x)
      (let ((v ($flonum->exact x)))
	(cond
	 ((or (fixnum? v) (bignum? v))
	  (inexact (lcm v y)))
	 (else (die 'lcm "not an integer" x)))))
     (else
      (die 'lcm "not an integer" x))))
   ((x)
    (cond
     ((or (fixnum? x) (bignum? x)) x)
     ((flonum? x)
      (let ((v ($flonum->exact x)))
	(cond
	 ((or (fixnum? v) (bignum? v)) x)
	 (else (die 'lcm "not an integer" x)))))
     (else
      (die 'lcm "not an integer" x))))
   (() 1)
   ((x y z . ls)
       ;;; FIXME: incorrect for multiple roundings
    (let f ((g (lcm (lcm x y) z)) (ls ls))
      (cond
       ((null? ls) g)
       (else (f (lcm g (car ls)) (cdr ls))))))))


(define binary/
  (lambda (x y)
    (define (x/compy x y)
      (let ((yr (real-part y))
	    (yi (imag-part y)))
	(let ((denom (+ (* yr yr) (* yi yi))))
	  (make-rectangular
	   (binary/ (* x yr) denom)
	   (binary/ (* (- x) yi) denom)))))
    (define (compx/y x y)
      (let ((xr (real-part x))
	    (xi (imag-part x)))
	(make-rectangular
	 (binary/ xr y)
	 (binary/ xi y))))
    (define (compx/compy x y)
      (let ((xr (real-part x))
	    (xi (imag-part x))
	    (yr (real-part y))
	    (yi (imag-part y)))
	(let ((denom (+ (* yr yr) (* yi yi))))
	  (make-rectangular
	   (binary/ (+ (* xr yr) (* xi yi)) denom)
	   (binary/ (- (* xi yr) (* xr yi)) denom)))))
    (cond
     ((flonum? x)
      (cond
       ((flonum? y) ($fl/ x y))
       ((fixnum? y) ($fl/ x ($fixnum->flonum y)))
       ((bignum? y) ($fl/ x (bignum->flonum y)))
       ((ratnum? y) ($fl/ x (ratnum->flonum y)))
       ((or (cflonum? y) (compnum? y)) (x/compy x y))
       (else (err '/ y))))
     ((fixnum? x)
      (cond
       ((flonum? y) ($fl/ ($fixnum->flonum x) y))
       ((fixnum? y)
	(cond
	 (($fx= y 0) (die '/ "division by 0"))
	 (($fx> y 0)
	  (if ($fx= y 1)
	      x
	    (let ((g (binary-gcd x y)))
	      (cond
	       (($fx= g y) (fxquotient x g))
	       (($fx= g 1) ($make-ratnum x y))
	       (else
		($make-ratnum (fxquotient x g) (fxquotient y g)))))))
	 (else
	  (if ($fx= y -1)
	      (binary- 0 x)
	    (let ((g (binary-gcd x y)))
	      (cond
	       (($fx= ($fx- 0 g) y) (binary- 0 (fxquotient x g)))
	       (($fx= g 1) ($make-ratnum (binary- 0 x) (binary- 0 y)))
	       (else
		($make-ratnum
		 (binary- 0 (fxquotient x g))
		 (binary- 0 (fxquotient y g))))))))))
       ((bignum? y)
	(if ($fx= x 0)
	    0
	  (let ((g (binary-gcd x y)))
	    (cond
	     ((= g y) (quotient x g))
	     (($bignum-positive? y)
	      (if ($fx= g 1)
		  ($make-ratnum x y)
		($make-ratnum (fxquotient x g) (quotient y g))))
	     (else
	      (if ($fx= g 1)
		  ($make-ratnum (binary- 0 x) (binary- 0 y))
		($make-ratnum
		 (binary- 0 (fxquotient x g))
		 (binary- 0 (quotient y g)))))))))
       ((ratnum? y)
	(/ (* x ($ratnum-d y)) ($ratnum-n y)))
       ((or (compnum? y) (cflonum? y)) (x/compy x y))
       (else (err '/ y))))
     ((bignum? x)
      (cond
       ((fixnum? y)
	(cond
	 (($fx= y 0) (die '/ "division by 0"))
	 (($fx> y 0)
	  (if ($fx= y 1)
	      x
	    (let ((g (binary-gcd x y)))
	      (cond
	       (($fx= g 1) ($make-ratnum x y))
	       (($fx= g y) (quotient x g))
	       (else
		($make-ratnum (quotient x g) (quotient y g)))))))
	 (else
	  (if ($fx= y -1)
	      (- x)
	    (let ((g (binary-gcd x y)))
	      (cond
	       ((= (- g) y) (- (quotient x g)))
	       (else
		($make-ratnum
		 (- (quotient x g))
		 (- (quotient y g))))))))))
       ((bignum? y)
	(let ((g (binary-gcd x y)))
	  (cond
	   (($fx= g 1)
	    (if ($bignum-positive? y)
		($make-ratnum x y)
	      ($make-ratnum
	       (binary- 0 x)
	       (binary- 0 y))))
	   (($bignum-positive? y)
	    (if (= g y)
		(quotient x g)
	      ($make-ratnum (quotient x g) (quotient y g))))
	   (else
	    (let ((y (binary- 0 y)))
	      (if (= g y)
		  (binary- 0 (quotient x g))
		($make-ratnum
		 (binary- 0 (quotient x g))
		 (quotient y g))))))))
       ((flonum? y) ($fl/ (bignum->flonum x) y))
       ((ratnum? y)
	(binary/ (binary* x ($ratnum-d y)) ($ratnum-n y)))
       ((or (compnum? y) (cflonum? y)) (x/compy x y))
       (else (err '/ y))))
     ((ratnum? x)
      (cond
       ((ratnum? y)
	(binary/
	 (binary* ($ratnum-n x) ($ratnum-d y))
	 (binary* ($ratnum-n y) ($ratnum-d x))))
       ((or (compnum? y) (cflonum? y)) (x/compy x y))
       (else (binary/ 1 (binary/ y x)))))
     ((or (compnum? x) (cflonum? x))
      (cond
       ((or (compnum? y) (cflonum? y)) (compx/compy x y))
       ((or (fixnum? y) (bignum? y) (ratnum? y) (flonum? y)) (compx/y x y))
       (else (err '/ y))))
     (else (err '/ x)))))


(define /
  (case-lambda
   ((x y) (binary/ x y))
   ((x)
    (cond
     ((fixnum? x)
      (cond
       (($fxzero? x) (die '/ "division by 0"))
       (($fx> x 0)
	(if ($fx= x 1)
	    1
	  ($make-ratnum 1 x)))
       (else
	(if ($fx= x -1)
	    -1
	  ($make-ratnum -1 (- x))))))
     ((bignum? x)
      (if ($bignum-positive? x)
	  ($make-ratnum 1 x)
	($make-ratnum -1 (- x))))
     ((flonum? x) (foreign-call "ikrt_fl_invert" x))
     ((ratnum? x)
      (let ((n ($ratnum-n x))
	    (d ($ratnum-d x)))
	(cond (($fx= n 1) d)
	      (($fx= n -1) (- d))
	      (else
	       (if (> 0 n)
		   ($make-ratnum (- d) (- n))
		 ($make-ratnum d n))))))
     ((compnum? x) (binary/ 1 x))
     ((cflonum? x) (binary/ 1 x))
     (else (die '/ "not a number" x))))
   ((x y z . ls)
    (let f ((a (binary/ x y)) (b z) (ls ls))
      (cond
       ((null? ls) (binary/ a b))
       (else (f (binary/ a b) (car ls) (cdr ls))))))))


(define max
  (case-lambda
   ((x y)
    (cond
     ((fixnum? x)
      (cond
       ((fixnum? y)
	(if ($fx> x y) x y))
       ((bignum? y)
	(if (positive-bignum? y) y x))
       ((flonum? y)
	(let ((x ($fixnum->flonum x)))
	  (if ($fl>= y x) y x)))
       ((ratnum? y) ;;; FIXME: optimize
	(if (>= x y) x y))
       (else (die 'max "not a number" y))))
     ((bignum? x)
      (cond
       ((fixnum? y)
	(if (positive-bignum? x) x y))
       ((bignum? y)
	(if (bnbn> x y) x y))
       ((flonum? y)
	(let ((x (bignum->flonum x)))
	  (if ($fl>= y x) y x)))
       ((ratnum? y) ;;; FIXME: optimize
	(if (>= x y) x y))
       (else (die 'max "not a number" y))))
     ((flonum? x)
      (cond
       ((flonum? y)
	(if ($fl>= x y) x y))
       ((fixnum? y)
	(let ((y ($fixnum->flonum y)))
	  (if ($fl>= y x) y x)))
       ((bignum? y)
	(let ((y (bignum->flonum y)))
	  (if ($fl>= y x) y x)))
       ((ratnum? y)
             ;;; FIXME: may be incorrect
	(let ((y (ratnum->flonum y)))
	  (if ($fl>= y x) y x)))
       (else (die 'max "not a number" y))))
     ((ratnum? x)
      (cond
       ((or (fixnum? y) (bignum? y) (ratnum? y))
	(if (>= x y) x y))
       ((flonum? y)
	(let ((x (ratnum->flonum x)))
	  (if ($fl>= x y) x y)))
       (else (die 'max "not a number" y))))
     (else (die 'max "not a number" x))))
   ((x y z . rest)
    (let f ((a (max x y)) (b z) (ls rest))
      (cond
       ((null? ls) (max a b))
       (else
	(f (max a b) (car ls) (cdr ls))))))
   ((x)
    (cond
     ((or (fixnum? x) (bignum? x) (ratnum? x) (flonum? x)) x)
     (else (die 'max "not a number" x))))))

(define min
  (case-lambda
   ((x y)
    (cond
     ((fixnum? x)
      (cond
       ((fixnum? y)
	(if ($fx> x y) y x))
       ((bignum? y)
	(if (positive-bignum? y) x y))
       ((flonum? y)
	(let ((x ($fixnum->flonum x)))
	  (if ($fl>= y x) x y)))
       ((ratnum? y) ;;; FIXME: optimize
	(if (>= x y) y x))
       (else (die 'min "not a number" y))))
     ((bignum? x)
      (cond
       ((fixnum? y)
	(if (positive-bignum? x) y x))
       ((bignum? y)
	(if (bnbn> x y) y x))
       ((flonum? y)
	(let ((x (bignum->flonum x)))
	  (if ($fl>= y x) x y)))
       ((ratnum? y) ;;; FIXME: optimize
	(if (>= x y) y x))
       (else (die 'min "not a number" y))))
     ((flonum? x)
      (cond
       ((flonum? y)
	(if ($fl>= x y) y x))
       ((fixnum? y)
	(let ((y ($fixnum->flonum y)))
	  (if ($fl>= y x) x y)))
       ((bignum? y)
	(let ((y (bignum->flonum y)))
	  (if ($fl>= y x) x y)))
       ((ratnum? y)
             ;;; FIXME: may be incorrect
	(let ((y (ratnum->flonum y)))
	  (if ($fl>= y x) x y)))
       (else (die 'min "not a number" y))))
     ((ratnum? x)
      (cond
       ((or (fixnum? y) (bignum? y) (ratnum? y))
	(if (>= x y) y x))
       ((flonum? y)
	(let ((x (ratnum->flonum x)))
	  (if ($fl>= x y) y x)))
       (else (die 'min "not a number" y))))
     (else (die 'min "not a number" x))))
   ((x y z . rest)
    (let f ((a (min x y)) (b z) (ls rest))
      (cond
       ((null? ls) (min a b))
       (else
	(f (min a b) (car ls) (cdr ls))))))
   ((x)
    (cond
     ((or (fixnum? x) (bignum? x) (ratnum? x) (flonum? x)) x)
     (else (die 'min "not a number" x))))))

(define (abs x)
  (cond
   ((fixnum? x)
    (if ($fx< x 0) (- x) x))
   ((bignum? x)
    (if ($bignum-positive? x) x (- x)))
   ((flonum? x)
    (if ($fx> ($flonum-u8-ref x 0) 127)
	($fl* x -1.0)
      x))
   ((ratnum? x)
    (let ((n ($ratnum-n x)))
      (if (< n 0)
	  ($make-ratnum (- n) ($ratnum-d x))
	x)))
   (else (die 'abs "not a real number" x))))

(define (->inexact x who)
  (cond
   ((fixnum? x) ($fixnum->flonum x))
   ((bignum? x) (bignum->flonum x))
   ((ratnum? x) (ratnum->flonum x))
   ((flonum? x) x)
   ((compnum? x)
    (make-rectangular
     (->inexact (real-part x) who)
     (->inexact (imag-part x) who)))
   ((cflonum? x) x)
   (else
    (die who "not a number" x))))

(define (exact->inexact x)
  (->inexact x 'exact->inexact))

(define (inexact x)
  (->inexact x 'inexact))

(define real->flonum
  (lambda (x)
    (cond
     ((fixnum? x) ($fixnum->flonum x))
     ((bignum? x) (bignum->flonum x))
     ((ratnum? x) (ratnum->flonum x))
     ((flonum? x) x)
     (else
      (die 'real->flonum "not a real number" x)))))

(define positive-bignum?
  (lambda (x)
    (foreign-call "ikrt_positive_bn" x)))

(define even-bignum?
  (lambda (x)
    (foreign-call "ikrt_even_bn" x)))

(define ($fxeven? x)
  ($fxzero? ($fxlogand x 1)))

(define (even? x)
  (cond
   ((fixnum? x) ($fxeven? x))
   ((bignum? x) (even-bignum? x))
   ((flonum? x)
    (let ((v ($flonum->exact x)))
      (cond
       ((fixnum? v) ($fxeven? v))
       ((bignum? v) (even-bignum? v))
       (else (die 'even? "not an integer" x)))))
   (else (die 'even? "not an integer" x))))

(define (odd? x)
  (cond
   ((fixnum? x) (not ($fxeven? x)))
   ((bignum? x) (not (even-bignum? x)))
   ((flonum? x)
    (let ((v ($flonum->exact x)))
      (cond
       ((fixnum? v) (not ($fxeven? v)))
       ((bignum? v) (not (even-bignum? v)))
       (else (die 'odd? "not an integer" x)))))
   (else (die 'odd? "not an integer" x))))


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
	;;(do-warn)
	(number->string x r))))

    (define (do-warn)
      (set! do-warn values)
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
      (cond ((fixnum? x)
	     (fixnum->string x r))

	    ((bignum? x)
	     (bignum->string x r))

	    ((flonum? x)
	     (if (eqv? r 10)
		 (flonum->string x)
	       (assertion-violation who "invalid radix for inexact number" r x)))

	    ((ratnum? x)
	     (ratnum->string x r))

	    ((compnum? x)
	     (let ((x.rep ($compnum-real x))
		   (x.imp ($compnum-imag x)))
	       (if (and (fixnum?  x.rep)
			($fxzero? x.rep))
		   (string-append (imag x.imp r) "i")
		 (string-append ($number->string x.rep r) (imag x.imp r) "i"))))

	    ((cflonum? x)
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
	     (assertion-violation who "not a number" x))))

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


(define modulo
  (lambda (n m)
    (cond
     ((fixnum? n)
      (cond
       ((fixnum? m)
	(if (eqv? m 0)
	    (die 'modulo "division by zero" n m)
	  (fxmodulo n m)))
       ((bignum? m)
	(if ($fx< n 0)
	    (if ($bignum-positive? m)
		(foreign-call "ikrt_fxbnplus" n m)
	      n)
	  (if ($bignum-positive? m)
	      n
	    (foreign-call "ikrt_fxbnplus" n m))))
       ((flonum? m)
	(let ((v ($flonum->exact m)))
	  (cond
	   ((or (fixnum? v) (bignum? v))
	    (inexact (modulo n v)))
	   (else
	    (die 'modulo "not an integer" m)))))
       ((ratnum? m) (die 'modulo "not an integer" m))
       (else (die 'modulo "not a number" m))))
     ((bignum? n)
      (cond
       ((fixnum? m)
	(if (eqv? m 0)
	    (die 'modulo "division by zero" n m)
	  (foreign-call "ikrt_bnfx_modulo" n m)))
       ((bignum? m)
	(if ($bignum-positive? n)
	    (if ($bignum-positive? m)
		(remainder n m)
	      (+ m (remainder n m)))
	  (if ($bignum-positive? m)
	      (+ m (remainder n m))
	    (remainder n m))))
       ((flonum? m)
	(let ((v ($flonum->exact m)))
	  (cond
	   ((or (fixnum? v) (bignum? v))
	    (inexact (modulo n v)))
	   (else
	    (die 'modulo "not an integer" m)))))
       ((ratnum? m) (die 'modulo "not an integer" m))
       (else (die 'modulo "not a number" m))))
     ((flonum? n)
      (let ((v ($flonum->exact n)))
	(cond
	 ((or (fixnum? v) (bignum? v))
	  (inexact (modulo v m)))
	 (else
	  (die 'modulo "not an integer" n)))))
     ((ratnum? n) (die 'modulo "not an integer" n))
     (else (die 'modulo "not a number" n)))))

(define-syntax mk<
  (syntax-rules ()
    ((_ name fxfx< fxbn< bnfx< bnbn<
	fxfl< flfx< bnfl< flbn< flfl<
	fxrt< rtfx< bnrt< rtbn< flrt< rtfl< rtrt<)
     (let ()
       (define err
	 (lambda (x) (die 'name "not a real number" x)))
       (define fxloopt
	 (lambda (x y ls)
	   (cond
	    ((fixnum? y)
	     (if (null? ls)
		 (fxfx< x y)
	       (if (fxfx< x y)
		   (fxloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((bignum? y)
	     (if (null? ls)
		 (fxbn< x y)
	       (if (fxbn< x y)
		   (bnloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((flonum? y)
	     (if (null? ls)
		 (fxfl< x y)
	       (if (fxfl< x y)
		   (flloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((ratnum? y)
	     (if (null? ls)
		 (fxrt< x y)
	       (if (fxrt< x y)
		   (rtloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    (else (err y)))))
       (define bnloopt
	 (lambda (x y ls)
	   (cond
	    ((fixnum? y)
	     (if (null? ls)
		 (bnfx< x y)
	       (if (bnfx< x y)
		   (fxloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((bignum? y)
	     (if (null? ls)
		 (bnbn< x y)
	       (if (bnbn< x y)
		   (bnloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((flonum? y)
	     (if (null? ls)
		 (bnfl< x y)
	       (if (bnfl< x y)
		   (flloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((ratnum? y)
	     (if (null? ls)
		 (bnrt< x y)
	       (if (bnrt< x y)
		   (rtloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    (else (err y)))))
       (define flloopt
	 (lambda (x y ls)
	   (cond
	    ((fixnum? y)
	     (if (null? ls)
		 (flfx< x y)
	       (if (flfx< x y)
		   (fxloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((bignum? y)
	     (if (null? ls)
		 (flbn< x y)
	       (if (flbn< x y)
		   (bnloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((flonum? y)
	     (if (null? ls)
		 (flfl< x y)
	       (if (flfl< x y)
		   (flloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((ratnum? y)
	     (if (null? ls)
		 (flrt< x y)
	       (if (flrt< x y)
		   (rtloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    (else (err y)))))
       (define rtloopt
	 (lambda (x y ls)
	   (cond
	    ((fixnum? y)
	     (if (null? ls)
		 (rtfx< x y)
	       (if (rtfx< x y)
		   (fxloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((bignum? y)
	     (if (null? ls)
		 (rtbn< x y)
	       (if (rtbn< x y)
		   (bnloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((flonum? y)
	     (if (null? ls)
		 (rtfl< x y)
	       (if (rtfl< x y)
		   (flloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    ((ratnum? y)
	     (if (null? ls)
		 (rtrt< x y)
	       (if (rtrt< x y)
		   (rtloopt y (car ls) (cdr ls))
		 (loopf (car ls) (cdr ls)))))
	    (else (err y)))))
       (define loopf
	 (lambda (x ls)
	   (cond
	    ((number? x)
	     (if (null? ls)
		 #f
	       (loopf (car ls) (cdr ls))))
	    (else (err x)))))
       (define name
	 (case-lambda
	  ((x y)
	   (cond
	    ((fixnum? x)
	     (cond
	      ((fixnum? y) (fxfx< x y))
	      ((bignum? y) (fxbn< x y))
	      ((flonum? y) (fxfl< x y))
	      ((ratnum? y) (fxrt< x y))
	      (else (err y))))
	    ((bignum? x)
	     (cond
	      ((fixnum? y) (bnfx< x y))
	      ((bignum? y) (bnbn< x y))
	      ((flonum? y) (bnfl< x y))
	      ((ratnum? y) (bnrt< x y))
	      (else (err y))))
	    ((flonum? x)
	     (cond
	      ((fixnum? y) (flfx< x y))
	      ((bignum? y) (flbn< x y))
	      ((flonum? y) (flfl< x y))
	      ((ratnum? y) (flrt< x y))
	      (else (err y))))
	    ((ratnum? x)
	     (cond
	      ((fixnum? y) (rtfx< x y))
	      ((bignum? y) (rtbn< x y))
	      ((flonum? y) (rtfl< x y))
	      ((ratnum? y) (rtrt< x y))
	      (else (err y))))
	    (else (err x))))
	  ((x y z) (and (name x y) (name y z)))
	  ((x) (if (number? x) #t (err x)))
	  ((x y . ls)
	   (cond
	    ((fixnum? x) (fxloopt x y ls))
	    ((bignum? x) (bnloopt x y ls))
	    ((flonum? x) (flloopt x y ls))
	    ((ratnum? x) (rtloopt x y ls))
	    (else (err x))))))
       name))))


(define-syntax false (syntax-rules () ((_ x y) #f)))
(define-syntax bnbncmp
  (syntax-rules ()
    ((_ x y cmp)
     (cmp (foreign-call "ikrt_bnbncomp" x y) 0))))
(define-syntax bnbn= (syntax-rules () ((_ x y) (bnbncmp x y $fx=))))
(define-syntax bnbn< (syntax-rules () ((_ x y) (bnbncmp x y $fx<))))
(define-syntax bnbn> (syntax-rules () ((_ x y) (bnbncmp x y $fx>))))
(define-syntax bnbn<= (syntax-rules () ((_ x y) (bnbncmp x y $fx<=))))
(define-syntax bnbn>= (syntax-rules () ((_ x y) (bnbncmp x y $fx>=))))
(define-syntax fxbn< (syntax-rules () ((_ x y) (positive-bignum? y))))
(define-syntax bnfx< (syntax-rules () ((_ x y) (not (positive-bignum? x)))))
(define-syntax fxbn> (syntax-rules () ((_ x y) (not (positive-bignum? y)))))
(define-syntax bnfx> (syntax-rules () ((_ x y) (positive-bignum? x))))

(define-syntax flcmp
  (syntax-rules ()
    ((_ flfl? flfx? fxfl? flbn? bnfl? fl?)
     (begin
       (define-syntax flfl?
	 (syntax-rules () ((_ x y) (fl? x y))))
       (define-syntax flfx?
	 (syntax-rules () ((_ x y) (fl? x ($fixnum->flonum y)))))
       (define-syntax flbn?
	 (syntax-rules () ((_ x y) (fl? x (bignum->flonum y)))))
       (define-syntax fxfl?
	 (syntax-rules () ((_ x y) (fl? ($fixnum->flonum x) y))))
       (define-syntax bnfl?
	 (syntax-rules () ((_ x y) (fl? (bignum->flonum x) y))))))))

(flcmp flfl= flfx= fxfl= flbn= bnfl= $fl=)
(flcmp flfl< flfx< fxfl< flbn< bnfl< $fl<)
(flcmp flfl> flfx> fxfl> flbn> bnfl> $fl>)
(flcmp flfl<= flfx<= fxfl<= flbn<= bnfl<= $fl<=)
(flcmp flfl>= flfx>= fxfl>= flbn>= bnfl>= $fl>=)


(define-syntax cmp-ex/in
  (syntax-rules ()
    ((_ pred)
     (syntax-rules ()
       ((_ ex in)
	(let ((x ex) (y in))
	  (if ($flonum-rational? y)
	      (pred x (exact y))
	    (pred (inexact x) y))))))))
(define-syntax cmp-in/ex
  (syntax-rules ()
    ((_ pred)
     (syntax-rules ()
       ((_ in ex)
	(let ((x in) (y ex))
	  (if ($flonum-rational? x)
	      (pred (exact x) y)
	    (pred x (inexact y)))))))))

(define-syntax flrt=  (cmp-in/ex =))
(define-syntax rtfl=  (cmp-ex/in =))
(define-syntax flrt<  (cmp-in/ex <))
(define-syntax rtfl<  (cmp-ex/in <))
(define-syntax flrt<= (cmp-in/ex <=))
(define-syntax rtfl<= (cmp-ex/in <=))
(define-syntax flrt>  (cmp-in/ex >))
(define-syntax rtfl>  (cmp-ex/in >))
(define-syntax flrt>= (cmp-in/ex >=))
(define-syntax rtfl>= (cmp-ex/in >=))

(define (exrt< x y) (< (* x ($ratnum-d y)) ($ratnum-n y)))
(define (rtex< x y) (< ($ratnum-n x) (* y ($ratnum-d x))))
(define (rtrt< x y) (< (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
(define (rtrt<= x y) (<= (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
(define (exrt> x y) (> (* x ($ratnum-d y)) ($ratnum-n y)))
(define (rtex> x y) (> ($ratnum-n x) (* y ($ratnum-d x))))
(define (rtrt> x y) (> (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
(define (rtrt>= x y) (>= (* ($ratnum-n x) ($ratnum-d y)) (* ($ratnum-n y) ($ratnum-d x))))
(define (rtrt= x y)
  (and (= ($ratnum-n x) ($ratnum-n y)) (= ($ratnum-d x) ($ratnum-d y))))



(define =
  (let ()
    (define err
      (lambda (x) (die '= "not a number" x)))
    (define (fx? x y)
      (cond
       ((fixnum? y) ($fx= x y))
       ((flonum? y) (fxfl= x y))
       ((or (bignum? y) (ratnum? y) (compnum? y)) #f)
       ((cflonum? y)
	(and (flfl= 0.0 ($cflonum-imag y)) (fxfl= x ($cflonum-real y))))
       (else (err y))))
    (define (bn? x y)
      (cond
       ((bignum? y) (bnbn= x y))
       ((flonum? y) (bnfl= x y))
       ((or (fixnum? y) (ratnum? y) (compnum? y)) #f)
       ((cflonum? y)
	(and (flfl= 0.0 ($cflonum-imag y)) (bnfl= x ($cflonum-real y))))
       (else (err y))))
    (define (fl? x y)
      (cond
       ((flonum? y) (flfl= x y))
       ((fixnum? y) (flfx= x y))
       ((bignum? y) (flbn= x y))
       ((ratnum? y) (flrt= x y))
       ((compnum? y) #f)
       ((cflonum? y)
	(and (flfl= 0.0 ($cflonum-imag y)) (flfl= x ($cflonum-real y))))
       (else (err y))))
    (define (rn? x y)
      (cond
       ((flonum? y) (rtfl= x y))
       ((ratnum? y) (rtrt= x y))
       ((or (fixnum? y) (bignum? y) (compnum? y)) #f)
       ((cflonum? y)
	(and (flfl= 0.0 ($cflonum-imag y)) (rtfl= x ($cflonum-real y))))
       (else (err y))))
    (define (cn? x y)
      (cond
       ((compnum? y) (cncn= x y))
       ((cflonum? y) (cncf= x y))
       ((or (fixnum? y) (bignum? y) (flonum? y) (ratnum? y)) #f)
       (else (err y))))
    (define (cf? x y)
      (cond
       ((cflonum? y) (cfcf= x y))
       ((compnum? y) (cncf= y x))
       ((or (fixnum? y) (bignum? y) (flonum? y) (ratnum? y))
	(and (flfl= 0.0 ($cflonum-imag x)) (= ($cflonum-real x) y)))
       (else (err y))))
    (define-syntax doloop
      (syntax-rules ()
	((_ cmp x0 y0 ls0)
	 (let loop ((x x0) (y y0) (ls ls0))
	   (if (cmp x y)
	       (if (null? ls) #t (loop x (car ls) (cdr ls)))
	     (if (null? ls) #f (loopf (car ls) (cdr ls))))))))
    (define loopf
      (lambda (x ls)
	(if (number? x)
	    (if (null? ls)
		#f
	      (loopf (car ls) (cdr ls)))
	  (err x))))
    (define (cncn= x y)
      (and
       (= ($compnum-real x) ($compnum-real y))
       (= ($compnum-imag x) ($compnum-imag y))))
    (define (cncf= x y)
      (and
       (= ($compnum-real x) ($cflonum-real y))
       (= ($compnum-imag x) ($cflonum-imag y))))
    (define (cfcf= x y)
      (and
       (= ($cflonum-real x) ($cflonum-real y))
       (= ($cflonum-imag x) ($cflonum-imag y))))
    (define =
      (case-lambda
       ((x y)
	(cond
	 ((fixnum? x)  (fx? x y))
	 ((bignum? x)  (bn? x y))
	 ((flonum? x)  (fl? x y))
	 ((ratnum? x)  (rn? x y))
	 ((compnum? x) (cn? x y))
	 ((cflonum? x) (cf? x y))
	 (else (err x))))
       ((x y z) (if (= x y) (= y z) (if (number? z) #f (err z))))
       ((x) (if (number? x) #t (err x)))
       ((x y . ls)
	(cond
	 ((fixnum? x) (doloop fx? x y ls))
	 ((bignum? x) (doloop bn? x y ls))
	 ((flonum? x) (doloop fl? x y ls))
	 ((ratnum? x) (doloop rn? x y ls))
	 ((compnum? x) (doloop cn? x y ls))
	 ((cflonum? x) (doloop cf? x y ls))
	 (else (err x))))))
    =))

		;(define =
		;  (mk< = $fx= false false bnbn= fxfl= flfx= bnfl= flbn= flfl=
		;             false false false false flrt= rtfl= rtrt=))

(define <
  (mk< < $fx< fxbn< bnfx< bnbn< fxfl< flfx< bnfl< flbn< flfl<
       exrt< rtex< exrt< rtex< flrt< rtfl< rtrt<))
(define >
  (mk< > $fx> fxbn> bnfx> bnbn> fxfl> flfx> bnfl> flbn> flfl>
       exrt> rtex> exrt> rtex> flrt> rtfl> rtrt>))
(define <=
  (mk< <= $fx<= fxbn< bnfx< bnbn<= fxfl<= flfx<= bnfl<= flbn<= flfl<=
       exrt< rtex< exrt< rtex< flrt<= rtfl<= rtrt<=))
(define >=
  (mk< >= $fx>= fxbn> bnfx> bnbn>= fxfl>= flfx>= bnfl>= flbn>= flfl>=
       exrt> rtex> exrt> rtex> flrt>= rtfl>= rtrt>=))

(define error@add1
  (lambda (x)
    (import (ikarus))
    (cond
     ((fixnum? x) (+ (greatest-fixnum) 1))
     ((number? x) (+ x 1))
     (else (die 'add1 "not a number" x)))))

(define add1
  (lambda (x)
    (import (ikarus))
    (add1 x)))

(define error@sub1
  (lambda (x)
    (import (ikarus))
    (cond
     ((fixnum? x) (- (least-fixnum) 1))
     ((number? x) (- x 1))
     (else (die 'sub1 "not a number" x)))))

(define sub1
  (lambda (x)
    (import (ikarus))
    (sub1 x)))

(define zero?
  (lambda (x)
    (cond
     ((fixnum? x) (eq? x 0))
     ((bignum? x) #f)
     ((ratnum? x) #f)
     ((flonum? x)
      (or ($fl= x 0.0) ($fl= x -0.0)))
     ((cflonum? x)
      (and ($fl= ($cflonum-real x) 0.0) ($fl= ($cflonum-imag x) 0.0)))
     ((compnum? x) #f)
     (else
      (die 'zero? "not a number" x)))))


(module (expt)
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
    (with-arguments-validation (who)
	((number	n))
      (cond ((fixnum? m)	(%expt-fixnum-exponent n m))
	    ((bignum? m)	(%expt-bignum-exponent n m))
	    ((flonum? m)	(%expt-flonum-exponent n m))
	    ((ratnum? m)	(%expt-ratnum-exponent n m))
	    ((or (compnum? m)
		 (cflonum? m))	(%expt-complex-exponent n m))
	    (else
	     (assertion-violation who "not a number" m)))))

  (module (%expt-fixnum-exponent)

    (define (%expt-fixnum-exponent n m)
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

    #| end of module: %expt-fixnum-exponent |# )

  (define (%expt-bignum-exponent n m)
    (cond ((eq? n 0)	0)
	  ((eq? n 1)	1)
	  ((eq? n -1)	(if (even-bignum? m) 1 -1))
	  ((nan? n)	+nan.0)
	  (else
	   (assertion-violation who "result is too big to compute" n m))))

  (define (%expt-flonum-exponent n m)
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

  (define (%expt-ratnum-exponent n m)
    ;; (expt (expt n ($ratnum-n m))
    ;;       (inexact ($make-ratnum 1 ($ratnum-d m))))
    (expt n (inexact m)))

  (define (%expt-complex-exponent n m)
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


(define quotient
  (lambda (x y)
    (let-values (((q r) (quotient+remainder x y)))
      q)))

(define remainder
  (lambda (x y)
    (let-values (((q r) (quotient+remainder x y)))
      r)))

(define quotient+remainder
  (lambda (x y)
    (cond
     ((eq? y 0)
      (die 'quotient+remainder
	   "second argument must be non-zero"))
     ((fixnum? x)
      (cond
       ((fixnum? y)
	(if (eq? y -1)
	    (values (- x) 0)
	  (values (fxquotient x y) (fxremainder x y))))
       ((bignum? y) (values 0 x))
       ((flonum? y)
	(let ((v ($flonum->exact y)))
	  (cond
	   ((or (fixnum? v) (bignum? v))
	    (let-values (((q r) (quotient+remainder x v)))
	      (values (inexact q) (inexact r))))
	   (else
	    (die 'quotient+remainder "not an integer" y)))))
       (else (die 'quotient+remainder "not an integer" y))))
     ((bignum? x)
      (cond
       ((fixnum? y)
	(let ((p (foreign-call "ikrt_bnfxdivrem" x y)))
	  (values (car p) (cdr p))))
       ((bignum? y)
	(let ((p (foreign-call "ikrt_bnbndivrem" x y)))
	  (values (car p) (cdr p))))
       ((flonum? y)
	(let ((v ($flonum->exact y)))
	  (cond
	   ((or (fixnum? v) (bignum? v))
	    (let-values (((q r) (quotient+remainder x v)))
	      (values (inexact q) (inexact r))))
	   (else
	    (die 'quotient+remainder "not an integer" y)))))
       (else (die 'quotient+remainder "not an integer" y))))
     ((flonum? x)
      (let ((v ($flonum->exact x)))
	(cond
	 ((or (fixnum? v) (bignum? v))
	  (let-values (((q r) (quotient+remainder v y)))
	    (values (inexact q) (inexact r))))
	 (else (die 'quotient+remainder "not an integer" x)))))
     (else (die 'quotient+remainder "not an integer" x)))))

(define positive?
  (lambda (x)
    (cond
     ((fixnum? x) ($fx> x 0))
     ((flonum? x) ($fl> x 0.0))
     ((bignum? x) (positive-bignum? x))
     ((ratnum? x) (positive? ($ratnum-n x)))
     (else (die 'positive? "not a real number" x)))))

(define negative?
  (lambda (x)
    (cond
     ((fixnum? x) ($fx< x 0))
     ((flonum? x) ($fl< x 0.0))
     ((bignum? x) (not (positive-bignum? x)))
     ((ratnum? x) (negative? ($ratnum-n x)))
     (else (die 'negative? "not a real number" x)))))

(define sinh
  (lambda (x)
    (define who 'sinh)
    (cond
     ((flonum? x) (foreign-call "ikrt_fl_sinh" x))
     ((or (fixnum? x) (bignum? x) (ratnum? x))
      (sinh (inexact x)))
     ((or (compnum? x) (cflonum? x))
      (let ((r (real-part x)) (i (imag-part x)))
	(make-rectangular
	 (* (sinh r) (cos i))
	 (* (cosh r) (sin i)))))
     (else (die who "not a number" x)))))

(define cosh
  (lambda (x)
    (define who 'cosh)
    (cond
     ((flonum? x) (foreign-call "ikrt_fl_cosh" x))
     ((or (fixnum? x) (bignum? x) (ratnum? x))
      (cosh (inexact x)))
     ((or (compnum? x) (cflonum? x))
      (let ((r (real-part x)) (i (imag-part x)))
	(make-rectangular
	 (* (cosh r) (cos i))
	 (* (sinh r) (sin i)))))
     (else (die who "not a number" x)))))

(define tanh
  (lambda (x)
    (define who 'tanh)
    (cond
     ((flonum? x) (foreign-call "ikrt_fl_tanh" x))
     ((or (fixnum? x) (bignum? x) (ratnum? x))
      (tanh (inexact x)))
     ((or (compnum? x) (cflonum? x))
      (let ((r (real-part x)) (i (imag-part x)))
	(let ((rr (* 2 r)) (ii (* 2 i)))
	  (let ((cos2i (cos ii)) (cosh2r (cosh rr)))
	    (make-rectangular
	     (/ (tanh rr) (+ 1 (/ cos2i cosh2r)))
	     (/ (sin ii) (+ cosh2r cos2i)))))))
     (else (die who "not a number" x)))))

(define asinh
  (lambda (x)
    (define who 'asinh)
    (cond
     ((flonum? x) (foreign-call "ikrt_fl_asinh" x))
     ((or (fixnum? x) (bignum? x) (ratnum? x))
      (asinh (inexact x)))
     ((or (cflonum? x) (compnum? x))
      (let ((x (real-part x)) (y (imag-part x)))
	(cond
	 ((= x 0)
	  (let ((v (asin y)))
	    (make-rectangular (imag-part v) (real-part v))))
	 (else
	  (let* ((z^2 (+ (* x x) (* y y)))
		 (z^2-1 (- z^2 1))
		 (z^2-1^2 (* z^2-1 z^2-1))
		 (y^2 (* y y))
		 (q (sqrt (+ z^2-1^2 (* 4 y^2)))))
	    (define (sgn x) (if (< x 0) -1 1))
	    (make-rectangular
	     (* 0.5 (sgn x) (acosh (+ q z^2)))
	     (* 0.5 (sgn y) (acos (- q z^2)))))))))
     (else (die who "not a number" x)))))

(define acosh
  (lambda (x)
    (define who 'acosh)
    (cond
     ((flonum? x)
      (cond
       (($fl>= x 1.0) (foreign-call "ikrt_fl_acosh" x))
       (($fl>= x -1.0)
	(make-rectangular 0 (atan (sqrt (- 1 (* x x))) x)))
       (($fl< x -1.0)
	(make-rectangular (acosh (- x)) PI))
       (else +nan.0)))
     ((or (fixnum? x) (bignum? x) (ratnum? x))
      (acosh (inexact x)))
     ((or (cflonum? x) (compnum? x))
      (let ((x (real-part x)) (y (imag-part x)))
	(cond
	 ((= x 0) (+ (asinh y) (make-rectangular 0 PI/2)))
	 (else
	  (let* ((z^2 (+ (* x x) (* y y)))
		 (z^2-1 (- z^2 1))
		 (z^2-1^2 (* z^2-1 z^2-1))
		 (y^2 (* y y))
		 (q (sqrt (+ z^2-1^2 (* 4 y^2)))))
	    (define (sgn x) (if (< x 0) -1 1))
	    (+ (* 0.5 (sgn x) (acosh (+ q z^2)))
	       (* 0.5i (sgn y)
		  (- PI (* (sgn x) (acos (- q z^2)))))))))))
     (else (die who "not a number" x)))))

(define atanh
  (lambda (x)
    (define who 'atanh)
    (cond
     ((flonum? x)
      (cond
       ((and (fl<=? x 1.0) (fl>=? x -1.0))
	(foreign-call "ikrt_fl_atanh" x))
       (else
	(- (atanh (fl/ 1.0 x))
	   (if (fl<? x 0.0) (* -i PI/2) (* +i PI/2))))))
     ((or (fixnum? x) (bignum? x) (ratnum? x))
      (atanh (inexact x)))
     ((number? x) (error who "not implemented" x))
     (else (die who "not a number" x)))))

(define sin
  (lambda (x)
    (cond
     ((flonum? x) (foreign-call "ikrt_fl_sin" x))
     ((fixnum? x)
      (if (fx=? x 0)
	  0
	(foreign-call "ikrt_fx_sin" x)))
     ((or (cflonum? x) (compnum? x))
      (let ((r (real-part x)) (i (imag-part x)))
	(make-rectangular
	 (* (sin r) (cosh i))
	 (* (cos r) (sinh i)))))
     ((number? x) (sin (inexact x)))
     (else (die 'sin "not a number" x)))))

(define cos
  (lambda (x)
    (cond
     ((flonum? x) (foreign-call "ikrt_fl_cos" x))
     ((fixnum? x)
      (if (fx=? x 0)
	  1
	(foreign-call "ikrt_fx_cos" x)))
     ((or (cflonum? x) (compnum? x))
      (let ((r (real-part x)) (i (imag-part x)))
	(make-rectangular
	 (* (cos r) (cosh i))
	 (* (sin r) (sinh i)))))
     ((number? x) (cos (inexact x)))
     (else (die 'cos "not a number" x)))))

(define tan
  (lambda (x)
    (cond
     ((flonum? x) (foreign-call "ikrt_fl_tan" x))
     ((fixnum? x)
      (if (fx=? x 0)
	  0
	(foreign-call "ikrt_fx_tan" x)))
     ((or (cflonum? x) (compnum? x))
      (let ((r (real-part x)) (i (imag-part x)))
	(make-rectangular
	 (/ (sin (* 2 r))
	    (+ (cos (* 2 r)) (cosh (* 2 i))))
	 (/ (tanh (* 2 i))
	    (+ 1 (/ (cos (* 2 r)) (cosh (* 2 i))))))))
     ((number? x) (tan (inexact x)))
     (else (die 'tan "not a number" x)))))

(module (PI PI/2)
  (import (ikarus))
  (define PI (acos -1))
  (define PI/2 (/ PI 2)))

(define asin
  (lambda (x)
    (cond
     ((flonum? x)
      (cond
       (($fl> x 1.0)
	(make-rectangular PI/2 (acosh x)))
       (($fl< x -1.0)
	(make-rectangular (- PI/2) (- (acosh (- x)))))
       (else
	(foreign-call "ikrt_fl_asin" x))))
     ((or (cflonum? x) (compnum? x))
      (let ((x (real-part x)) (y (imag-part x)))
	(cond
	 ((= x 0) (make-rectangular 0 (asinh y)))
	 (else
	  (let* ((z^2 (+ (* x x) (* y y)))
		 (z^2-1 (- z^2 1.0))
		 (z^2-1^2 (* z^2-1 z^2-1))
		 (y^2 (* y y))
		 (q (sqrt (+ z^2-1^2 (* 4.0 y^2)))))
	    (define (sgn x) (if (< x 0) -1.0 1.0))
	    (make-rectangular
	     (* 0.5 (sgn x) (acos (- q z^2)))
	     (* 0.5 (sgn y) (acosh (+ q z^2)))))))))
     ((number? x) (asin (inexact x)))
     (else (die 'asin "not a number" x)))))

(define acos
  (lambda (x)
    (cond
     ((flonum? x)
      (cond
       (($fl> x 1.0)
	(make-rectangular 0 (acosh x)))
       (($fl< x -1.0)
	(make-rectangular PI (- (acosh (- x)))))
       (else
	(foreign-call "ikrt_fl_acos" x))))
     ((or (cflonum? x) (compnum? x))
      (- PI/2 (asin x)))
     ((number? x) (acos (inexact x)))
     (else (die 'acos "not a number" x)))))

(define atan
  (case-lambda
   ((x)
    (cond
     ((flonum? x) (foreign-call "ikrt_fl_atan" x))
     ((fixnum? x) (foreign-call "ikrt_fx_atan" x))
     ((or (ratnum? x) (bignum? x) (compnum? x))
      (atan (inexact x)))
     ((cflonum? x)
      ;;Formula from Wikipedia section "Logarithmic forms":
      ;;
      ;;	<http://en.wikipedia.org/wiki/Arc_tangent>
      ;;
      (if (let ((I (imag-part x)))
	    (and (exact? I) (zero? I)))
	  (flatan (real-part x))
	(* +0.5i
	   (- (log (- 1 (* +1i x)))
	      (log (+ 1 (* +1i x)))))))
     (else (die 'atan "not a number" x))))
   ((y x)
    (unless (real? x) (die 'atan "not a real number" x))
    (unless (real? y) (die 'atan "not a real number" y))
    (foreign-call "ikrt_atan2" (inexact y) (inexact x)))))


;;;; square roots

(module (sqrt
	 $sqrt/fixnum
	 $sqrt/flonum
	 $sqrt/bignum
	 $sqrt/ratnum
	 $sqrt/compnum
	 $sqrt/cflonum)

  (define (sqrt x)
    (cond-numeric-operand x
      ((fixnum?)	($sqrt/fixnum x))
      ((bignum?)	($sqrt/bignum x))
      ((ratnum?)	($sqrt/ratnum x))
      ((flonum?)	($sqrt/flonum x))
      ((compnum?)	($sqrt/compnum x))
      ((cflonum?)	($sqrt/cflonum x))
      (else
       (assertion-violation 'sqrt "expected number as argument" x))))

  (define ($sqrt/flonum x)
    ;;This can return both a flonum or a compnum!!!
    ;;
    (if ($fl< x 0.0)
	;;This case includes: X = -0.0
	(make-rectangular 0 (foreign-call "ikrt_fl_sqrt" ($fl- x)))
      (foreign-call "ikrt_fl_sqrt" x)))

  (define ($sqrt/fixnum x)
    (if ($fxnegative? x)
	(make-rectangular 0 ($sqrt/fixnum ($fx- x)))
      (let-values (((root residual) ($exact-integer-sqrt/fixnum x)))
	(if ($fxzero? residual)
	    root
	  (foreign-call "ikrt_fx_sqrt" x)))))

  (define ($sqrt/bignum x)
    (if ($bignum-positive? x)
	(let-values (((root residual) ($exact-integer-sqrt/bignum x)))
	  (if (eq? residual 0)
	      root
	    (let ((v ($sqrt/flonum (inexact x))))
	      ;;Could the (dropped) residual ever affect the answer?
	      (if (flinfinite? v)
		  (if (bignum? root)
		      ;;The argument  1 makes it  round up in case  of a
		      ;;tie.
		      (foreign-call "ikrt_bignum_to_flonum" root 1 ($make-flonum))
		    (inexact root))
		v))))
      (make-rectangular 0 ($sqrt/bignum (- x)))))

  (define ($sqrt/ratnum x)
    (/ (sqrt ($ratnum-n x))
       (sqrt ($ratnum-d x))))

  (define ($sqrt/compnum Z)
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

  (define ($sqrt/cflonum Z)
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
      ;;Remember  that  $SQRT/FLONUM  can  return both  a  flonum  or  a
      ;;compnum!!!
      (let* ((magn  ($sqrt/flonum ($fl+ ($flsqr Z.rep) ($flsqr Z.imp))))
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


(define numerator
  (lambda (x)
    (cond
     ((ratnum? x) ($ratnum-n x))
     ((or (fixnum? x) (bignum? x)) x)
     ((flonum? x) (flnumerator x))
     (else (die 'numerator "not an exact integer" x)))))

(define denominator
  (lambda (x)
    (cond
     ((ratnum? x) ($ratnum-d x))
     ((or (fixnum? x) (bignum? x)) 1)
     ((flonum? x) (fldenominator x))
     (else (die 'denominator "not an exact integer" x)))))


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
   (else (die 'floor "not a number" x))))

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
   (else (die 'ceiling "not a number" x))))


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
   (else (die 'round "not a number" x))))

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
   (else (die 'truncate "not a number" x))))


(define log
  (case-lambda
   ((x)
    (cond
     ((fixnum? x)
      (cond
       (($fx= x 1) 0)
       (($fx= x 0) (die 'log "undefined around 0"))
       (($fx> x 0) (foreign-call "ikrt_fx_log" x))
       (else (make-rectangular (log (- x)) (acos -1)))))
     ((flonum? x)
      (cond
       ((nan? x)	  +nan.0)
       ((fl>=? x 0.0) (foreign-call "ikrt_fl_log" x))
       (else
	(make-rectangular
	 (log (fl- 0.0 x))
	 (acos -1)))))
     ((bignum? x)
      (if ($bignum-positive? x)
	  (let ((v (log (inexact x))))
	    (cond
	     ((infinite? v)
	      (let-values (((s r) (exact-integer-sqrt x)))
                     ;;; could the (dropped) residual ever affect the answer?
		(fl* 2.0 (log s))))
	     (else v)))
	(make-rectangular (log (- x)) (acos -1))))
     ((ratnum? x)
      (- (log (numerator x)) (log (denominator x))))
     ((or (compnum? x) (cflonum? x))
      (let ((xr (real-part x)) (xi (imag-part x)))
	(make-rectangular
	 (/ (log (+ (* xr xr) (* xi xi))) 2)
	 (atan xi xr))))
     (else (die 'log "not a number" x))))
   ((x y)
    (let ((ly (log y)))
      (if (eqv? ly 0)
	  (die 'log "invalid arguments" x y)
	(/ (log x) ly))))))

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

(define (exp x)
  (cond
   ((flonum? x) (flexp x))
   ((fixnum? x)
    (if ($fx= x 0) 1 (flexp (fixnum->flonum x))))
   ((bignum? x) (flexp (bignum->flonum x)))
   ((ratnum? x) (flexp (ratnum->flonum x)))
   ((or (compnum? x) (cflonum? x))
    ;;In general:
    ;;
    ;;   e^x = e^(xr + xi i)
    ;;       = e^xr cos(xi) + e^xr sin(xi) i
    ;;
    ;;and:
    ;;
    ;;   e^(xr+0.0i) = e^xr * e^(0.0 i) = e^xr * 1.0+0.0i
    ;;
    ;;so, in the special case xr=+inf.0, the imaginary part becomes:
    ;;
    ;;   e^xr * sin(xi) * i = +inf.0 * 0.0 * i = +nan.0 * i
    ;;
    (let ((xr (real-part x)) (xi (imag-part x)))
      (let ((e^xr (exp xr)))
	(make-rectangular
	 (* e^xr (cos xi))
	 (* e^xr (sin xi))))))
   (else (die 'exp "not a number" x))))

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
;; End:
