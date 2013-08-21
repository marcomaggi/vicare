;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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

    fxnonnegative?		fxnonpositive?
    flnonnegative?		flnonpositive?

    non-positive? $fxnonpositive? $bignum-non-positive? $flnonpositive? $ratnum-non-positive?
    non-negative? $fxnonnegative? $bignum-non-negative? $flnonnegative? $ratnum-non-negative?

    ;; exactness
    exact->inexact		inexact

    ;; part functions
    abs				sign
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
    expt			square
    cube
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
    $compnum->cflonum
    random
    error@add1			error@sub1
    bytevector->bignum		bignum->bytevector

;;; --------------------------------------------------------------------
;;; the following go in (ikarus system $numerics)

    $neg-number		$neg-fixnum	$neg-bignum
    $neg-flonum		$neg-ratnum	$neg-compnum
    $neg-cflonum

    $inv-number		$inv-fixnum	$inv-bignum
    $inv-flonum		$inv-ratnum	$inv-compnum
    $inv-cflonum

    $add-number-number		$add-fixnum-number	$add-bignum-number
    $add-flonum-number		$add-ratnum-number	$add-compnum-number
    $add-cflonum-number		$add-number-fixnum	$add-number-bignum
    $add-number-flonum		$add-number-ratnum	$add-number-compnum
    $add-number-cflonum		$add-fixnum-fixnum	$add-fixnum-bignum
    $add-fixnum-flonum		$add-fixnum-ratnum	$add-fixnum-compnum
    $add-fixnum-cflonum		$add-bignum-fixnum	$add-bignum-bignum
    $add-bignum-flonum		$add-bignum-ratnum	$add-bignum-compnum
    $add-bignum-cflonum		$add-flonum-fixnum	$add-flonum-bignum
    $add-flonum-flonum		$add-flonum-ratnum	$add-flonum-compnum
    $add-flonum-cflonum		$add-ratnum-fixnum	$add-ratnum-bignum
    $add-ratnum-flonum		$add-ratnum-ratnum	$add-ratnum-compnum
    $add-ratnum-cflonum		$add-compnum-fixnum	$add-compnum-bignum
    $add-compnum-ratnum		$add-compnum-compnum	$add-compnum-flonum
    $add-compnum-cflonum	$add-cflonum-fixnum	$add-cflonum-bignum
    $add-cflonum-ratnum		$add-cflonum-flonum	$add-cflonum-compnum
    $add-cflonum-cflonum

    $sub-number-number		$sub-fixnum-number	$sub-bignum-number
    $sub-flonum-number		$sub-ratnum-number	$sub-compnum-number
    $sub-cflonum-number		$sub-number-fixnum	$sub-number-bignum
    $sub-number-flonum		$sub-number-ratnum	$sub-number-compnum
    $sub-number-cflonum		$sub-fixnum-fixnum	$sub-fixnum-bignum
    $sub-fixnum-flonum		$sub-fixnum-ratnum	$sub-fixnum-compnum
    $sub-fixnum-cflonum		$sub-bignum-fixnum	$sub-bignum-bignum
    $sub-bignum-flonum		$sub-bignum-ratnum	$sub-bignum-compnum
    $sub-bignum-cflonum		$sub-flonum-fixnum	$sub-flonum-bignum
    $sub-flonum-ratnum		$sub-flonum-flonum	$sub-flonum-compnum
    $sub-flonum-cflonum		$sub-ratnum-fixnum	$sub-ratnum-bignum
    $sub-ratnum-flonum		$sub-ratnum-ratnum	$sub-ratnum-compnum
    $sub-ratnum-cflonum		$sub-compnum-fixnum	$sub-compnum-bignum
    $sub-compnum-ratnum		$sub-compnum-compnum	$sub-compnum-flonum
    $sub-compnum-cflonum	$sub-cflonum-fixnum	$sub-cflonum-bignum
    $sub-cflonum-ratnum		$sub-cflonum-flonum	$sub-cflonum-compnum
    $sub-cflonum-cflonum

    $mul-number-number		$mul-fixnum-number	$mul-bignum-number
    $mul-flonum-number		$mul-ratnum-number	$mul-compnum-number
    $mul-cflonum-number		$mul-number-fixnum	$mul-number-bignum
    $mul-number-flonum		$mul-number-ratnum	$mul-number-compnum
    $mul-number-cflonum		$mul-fixnum-fixnum	$mul-fixnum-bignum
    $mul-fixnum-flonum		$mul-fixnum-ratnum	$mul-fixnum-compnum
    $mul-fixnum-cflonum		$mul-bignum-fixnum	$mul-bignum-bignum
    $mul-bignum-flonum		$mul-bignum-ratnum	$mul-bignum-compnum
    $mul-bignum-cflonum		$mul-flonum-flonum	$mul-flonum-cflonum
    $mul-flonum-fixnum		$mul-flonum-bignum	$mul-flonum-ratnum
    $mul-flonum-compnum		$mul-ratnum-fixnum	$mul-ratnum-bignum
    $mul-ratnum-flonum		$mul-ratnum-ratnum	$mul-ratnum-compnum
    $mul-ratnum-cflonum		$mul-compnum-fixnum	$mul-compnum-bignum
    $mul-compnum-ratnum		$mul-compnum-flonum	$mul-compnum-compnum
    $mul-compnum-cflonum	$mul-cflonum-fixnum	$mul-cflonum-bignum
    $mul-cflonum-ratnum		$mul-cflonum-flonum	$mul-cflonum-compnum
    $mul-cflonum-cflonum

    $div-number-number		$div-flonum-number	$div-fixnum-number
    $div-bignum-number		$div-ratnum-number	$div-compnum-number
    $div-cflonum-number		$div-number-flonum	$div-number-fixnum
    $div-number-bignum		$div-number-ratnum	$div-number-compnum
    $div-number-cflonum		$div-fixnum-flonum	$div-fixnum-fixnum
    $div-fixnum-bignum		$div-fixnum-ratnum	$div-fixnum-compnum
    $div-fixnum-cflonum		$div-bignum-fixnum	$div-bignum-bignum
    $div-bignum-flonum		$div-bignum-ratnum	$div-bignum-compnum
    $div-bignum-cflonum		$div-ratnum-fixnum	$div-ratnum-bignum
    $div-ratnum-ratnum		$div-ratnum-flonum	$div-ratnum-compnum
    $div-ratnum-cflonum		$div-flonum-flonum	$div-flonum-cflonum
    $div-flonum-fixnum		$div-flonum-bignum	$div-flonum-ratnum
    $div-flonum-compnum		$div-compnum-fixnum	$div-compnum-bignum
    $div-compnum-ratnum		$div-compnum-flonum	$div-compnum-compnum
    $div-compnum-cflonum	$div-cflonum-fixnum	$div-cflonum-bignum
    $div-cflonum-ratnum		$div-cflonum-flonum	$div-cflonum-compnum
    $div-cflonum-cflonum

    $square-fixnum		$square-bignum		$square-ratnum
    $square-compnum		$square-cflonum

    $cube-fixnum		$cube-bignum		$cube-ratnum
    $cube-compnum		$cube-cflonum

    $gcd-number
    $gcd-number-number		$gcd-fixnum-number	$gcd-bignum-number
    $gcd-flonum-number		$gcd-number-fixnum	$gcd-number-bignum
    $gcd-number-flonum		$gcd-fixnum-fixnum	$gcd-fixnum-bignum
    $gcd-fixnum-flonum		$gcd-bignum-fixnum	$gcd-bignum-bignum
    $gcd-bignum-flonum		$gcd-flonum-fixnum	$gcd-flonum-bignum
    $gcd-flonum-flonum

    $lcm-number
    $lcm-number-number		$lcm-fixnum-number	$lcm-bignum-number
    $lcm-flonum-number		$lcm-number-fixnum	$lcm-number-bignum
    $lcm-number-flonum		$lcm-fixnum-fixnum	$lcm-fixnum-bignum
    $lcm-fixnum-flonum		$lcm-bignum-fixnum	$lcm-bignum-bignum
    $lcm-bignum-flonum		$lcm-flonum-fixnum	$lcm-flonum-bignum
    $lcm-flonum-flonum

    $quotient+remainder-fixnum-number	$quotient+remainder-number-fixnum
    $quotient+remainder-bignum-number	$quotient+remainder-number-bignum
    $quotient+remainder-flonum-number	$quotient+remainder-number-flonum
    $quotient+remainder-fixnum-fixnum	$quotient+remainder-bignum-fixnum
    $quotient+remainder-fixnum-bignum	$quotient+remainder-bignum-bignum
    $quotient+remainder-fixnum-flonum	$quotient+remainder-bignum-flonum
    $quotient+remainder-flonum-fixnum	$quotient+remainder-flonum-bignum
    $quotient+remainder-flonum-flonum

    $quotient-fixnum-number		$quotient-number-fixnum
    $quotient-bignum-number		$quotient-number-bignum
    $quotient-flonum-number		$quotient-number-flonum
    $quotient-fixnum-fixnum		$quotient-fixnum-bignum
    $quotient-fixnum-flonum		$quotient-bignum-fixnum
    $quotient-bignum-bignum		$quotient-bignum-flonum
    $quotient-flonum-fixnum		$quotient-flonum-bignum
    $quotient-flonum-flonum

    $remainder-fixnum-number		$remainder-number-fixnum
    $remainder-bignum-number		$remainder-number-bignum
    $remainder-flonum-number		$remainder-number-flonum
    $remainder-fixnum-fixnum		$remainder-fixnum-bignum
    $remainder-fixnum-flonum		$remainder-bignum-fixnum
    $remainder-bignum-bignum		$remainder-bignum-flonum
    $remainder-flonum-fixnum		$remainder-flonum-bignum
    $remainder-flonum-flonum

    $modulo-fixnum-number		$modulo-bignum-number
    $modulo-flonum-number		$modulo-number-fixnum
    $modulo-number-bignum		$modulo-number-flonum
    $modulo-fixnum-fixnum		$modulo-fixnum-bignum
    $modulo-fixnum-flonum		$modulo-bignum-fixnum
    $modulo-bignum-bignum		$modulo-bignum-flonum
    $modulo-flonum-fixnum		$modulo-flonum-bignum
    $modulo-flonum-flonum

    $max-fixnum-number		$max-bignum-number	$max-flonum-number
    $max-ratnum-number		$max-number-fixnum	$max-number-bignum
    $max-number-flonum		$max-number-ratnum	$max-fixnum-fixnum
    $max-fixnum-bignum		$max-fixnum-flonum	$max-fixnum-ratnum
    $max-bignum-fixnum		$max-bignum-bignum	$max-bignum-flonum
    $max-bignum-ratnum		$max-flonum-flonum	$max-flonum-fixnum
    $max-flonum-bignum		$max-flonum-ratnum	$max-ratnum-fixnum
    $max-ratnum-bignum		$max-ratnum-ratnum	$max-ratnum-flonum

    $min-fixnum-number		$min-bignum-number	$min-flonum-number
    $min-ratnum-number		$min-number-fixnum	$min-number-bignum
    $min-number-flonum		$min-number-ratnum	$min-fixnum-fixnum
    $min-fixnum-bignum		$min-fixnum-flonum	$min-fixnum-ratnum
    $min-bignum-fixnum		$min-bignum-bignum	$min-bignum-flonum
    $min-bignum-ratnum		$min-flonum-flonum	$min-flonum-fixnum
    $min-flonum-bignum		$min-flonum-ratnum	$min-ratnum-fixnum
    $min-ratnum-bignum		$min-ratnum-ratnum	$min-ratnum-flonum

    $abs-fixnum			$abs-bignum
    $abs-flonum			$abs-ratnum

    $sign-fixnum		$sign-bignum
    $sign-flonum		$sign-ratnum
;;;
    $expt-number-fixnum		$expt-number-bignum	$expt-number-flonum
    $expt-number-ratnum		$expt-number-compnum	$expt-number-cflonum

    $expt-number-zero-fixnum		$expt-fixnum-zero-fixnum
    $expt-flonum-zero-fixnum
    $expt-compnum-zero-fixnum		$expt-cflonum-zero-fixnum

    $expt-number-negative-fixnum
    $expt-fixnum-negative-fixnum	$expt-bignum-negative-fixnum
    $expt-ratnum-negative-fixnum	$expt-flonum-negative-fixnum
    $expt-compnum-negative-fixnum	$expt-cflonum-negative-fixnum

    $expt-number-positive-fixnum
    $expt-fixnum-positive-fixnum	$expt-bignum-positive-fixnum
    $expt-flonum-positive-fixnum	$expt-ratnum-positive-fixnum
    $expt-compnum-positive-fixnum	$expt-cflonum-positive-fixnum

    $expt-fixnum-fixnum		$expt-bignum-fixnum	$expt-ratnum-fixnum
    $expt-flonum-fixnum		$expt-compnum-fixnum	$expt-cflonum-fixnum
    $expt-fixnum-bignum		$expt-bignum-bignum	$expt-ratnum-bignum
    $expt-flonum-bignum		$expt-compnum-bignum	$expt-cflonum-bignum
    $expt-fixnum-flonum		$expt-bignum-flonum	$expt-ratnum-flonum
    $expt-flonum-flonum		$expt-compnum-flonum	$expt-cflonum-flonum
    $expt-fixnum-ratnum		$expt-bignum-ratnum	$expt-ratnum-ratnum
    $expt-flonum-ratnum		$expt-compnum-ratnum	$expt-cflonum-ratnum
    $expt-fixnum-cflonum	$expt-bignum-cflonum	$expt-ratnum-cflonum
    $expt-flonum-cflonum	$expt-compnum-cflonum	$expt-cflonum-cflonum
    $expt-fixnum-compnum	$expt-bignum-compnum	$expt-ratnum-compnum
    $expt-flonum-compnum	$expt-compnum-compnum	$expt-cflonum-compnum
;;;
    $sqrt-fixnum		$sqrt-flonum		$sqrt-bignum
    $sqrt-ratnum		$sqrt-compnum		$sqrt-cflonum

    $exact-integer-sqrt-fixnum	$exact-integer-sqrt-bignum

    $log-fixnum			$log-flonum		$log-bignum
    $log-ratnum			$log-compnum		$log-cflonum

    $exp-fixnum			$exp-bignum		$exp-ratnum
    $exp-flonum			$exp-compnum		$exp-cflonum

    $sin-fixnum			$sin-bignum		$sin-ratnum
    $sin-flonum			$sin-cflonum		$sin-compnum

    $cos-fixnum			$cos-bignum		$cos-ratnum
    $cos-flonum			$cos-cflonum		$cos-compnum

    $tan-fixnum			$tan-bignum		$tan-ratnum
    $tan-flonum			$tan-compnum		$tan-cflonum

    $asin-fixnum		$asin-bignum		$asin-ratnum
    $asin-flonum		$asin-cflonum		$asin-compnum

    $acos-fixnum		$acos-bignum		$acos-ratnum
    $acos-flonum		$acos-cflonum		$acos-compnum

    $atan2-real-real
    $atan-fixnum		$atan-ratnum		$atan-bignum
    $atan-flonum		$atan-cflonum		$atan-compnum

    $sinh-fixnum		$sinh-bignum		$sinh-ratnum
    $sinh-flonum		$sinh-compnum		$sinh-cflonum

    $cosh-fixnum		$cosh-bignum		$cosh-ratnum
    $cosh-flonum		$cosh-compnum		$cosh-cflonum

    $tanh-fixnum		$tanh-bignum		$tanh-ratnum
    $tanh-flonum		$tanh-compnum		$tanh-cflonum

    $asinh-fixnum		$asinh-bignum		$asinh-ratnum
    $asinh-flonum		$asinh-cflonum		$asinh-compnum

    $acosh-fixnum		$acosh-bignum		$acosh-ratnum
    $acosh-flonum		$acosh-cflonum		$acosh-compnum

    $atanh-fixnum		$atanh-bignum		$atanh-ratnum
    $atanh-flonum		$atanh-cflonum		$atanh-compnum

    $bitwise-not-fixnum		$bitwise-not-bignum

    $bitwise-and-fixnum-number		$bitwise-and-bignum-number
    $bitwise-and-fixnum-fixnum		$bitwise-and-fixnum-bignum
    $bitwise-and-bignum-fixnum		$bitwise-and-bignum-bignum

    $bitwise-ior-fixnum-number		$bitwise-ior-bignum-number
    $bitwise-ior-fixnum-fixnum		$bitwise-ior-fixnum-bignum
    $bitwise-ior-bignum-fixnum		$bitwise-ior-bignum-bignum

    $bitwise-xor-fixnum-number		$bitwise-xor-bignum-number
    $bitwise-xor-fixnum-fixnum		$bitwise-xor-fixnum-bignum
    $bitwise-xor-bignum-fixnum		$bitwise-xor-bignum-bignum
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
		exact->inexact			inexact

		;; part functions
		abs				sign
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
		expt				square
		cube
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
		number->string			real->flonum
		random
		error@add1			error@sub1
		bytevector->bignum		bignum->bytevector)
  (ikarus system $pairs)
  (ikarus system $fx)
  (ikarus system $flonums)
  (rename (only (ikarus flonums) #;(ikarus system $flonums)
		$flexp
		$flsin
		$flcos
		$fltan
		$flatan
		$flsinh		$flasinh
		$flcosh
		$fltanh)
	  ($flexp	$exp-flonum)
	  ($flsin	$sin-flonum)
	  ($flcos	$cos-flonum)
	  ($fltan	$tan-flonum)
	  ($flatan	$atan-flonum)
	  ($flsinh	$sinh-flonum)
	  ($flcosh	$cosh-flonum)
	  ($fltanh	$tanh-flonum)
	  ($flasinh	$asinh-flonum))
  (except (ikarus system $ratnums)
	  ;;FIXME At  the next  boot image  rotation the  definitions of
	  ;;these functions  must be  moved to  "ikarus.ratnums.sls" and
	  ;;this import clause removed.  (Marco Maggi; Sat Aug 3, 2013)
	  $ratnum-non-positive?
	  $ratnum-non-negative?)
  (ikarus system $bignums)
  (ikarus system $compnums)
  (ikarus system $chars)
  (ikarus system $strings)
  (vicare arguments validation)
  (vicare language-extensions syntaxes))


;;;; helpers

(define (err who x)
  (assertion-violation who
    (if (number? x)
	"invalid argument"
      "expected number as argument")
    x))

(define-syntax %error-not-number
  (lambda (stx)
    (syntax-case stx ()
      ((?k ?arg)
       (identifier? #'?arg)
       (with-syntax ((WHO (datum->syntax #'?k 'who)))
	 #'(assertion-violation WHO "expected number as argument" ?arg))))))

(define-syntax %error-not-real-number
  (lambda (stx)
    (syntax-case stx ()
      ((?k ?arg)
       (identifier? #'?arg)
       (with-syntax ((WHO (datum->syntax #'?k 'who)))
	 #'(assertion-violation WHO "expected real number as argument" ?arg))))))

(define (%error-not-integer who obj)
  (assertion-violation who "expected exact or inexact integer as argument" obj))

(define-syntax %error-division-by-zero
  (syntax-rules ()
    ((_ ?who . ?irritants)
     ;;According to R6RS this must be an assertion.
     (assertion-violation ?who "division by zero" . ?irritants))))

(define-syntax %error-undefined-operation
  (syntax-rules ()
    ((_ ?who . ?irritants)
     (assertion-violation ?who "undefined operation" . ?irritants))))


;;;; constants

;;From Wikipedia.
(define greek-pi	3.141592653589793)
(define greek-pi/2	1.5707963267948966)


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

(define ($compnum->cflonum Z)
  ($make-cflonum (real->flonum ($compnum-real Z))
		 (real->flonum ($compnum-imag Z))))


;;;; generic arithmetic functions

(define +
  (case-lambda
   ((x y)
    ($add-number-number x y))
   ((x y z)
    ($add-number-number ($add-number-number x y) z))
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
    (let loop ((ac ($add-number-number ($add-number-number ($add-number-number a b) c) d))
	       (e* e*))
      (if (null? e*)
	  ac
	(loop ($add-number-number ac ($car e*)) ($cdr e*)))))))

(define -
  (case-lambda
   ((x y)
    ($sub-number-number x y))
   ((x y z)
    ($sub-number-number ($sub-number-number x y) z))
   ((a)
    ($sub-number-number 0 a))
   ((a b c d . e*)
    (let loop ((ac ($sub-number-number ($sub-number-number ($sub-number-number a b) c) d))
	       (e* e*))
      (if (null? e*)
	  ac
	(loop ($sub-number-number ac ($car e*)) ($cdr e*)))))))

(define *
  (case-lambda
   ((x y)
    ($mul-number-number x y))
   ((x y z)
    ($mul-number-number ($mul-number-number x y) z))
   ((a)
    (cond ((fixnum? a) a)
	  ((number? a) a)
	  (else
	   (assertion-violation '* "not a number" a))))
   (()
    1)
   ((a b c d . e*)
    (let loop ((ac ($mul-number-number ($mul-number-number ($mul-number-number a b) c) d))
	       (e* e*))
      (if (null? e*)
	  ac
	(loop ($mul-number-number ac ($car e*)) ($cdr e*)))))))

(define /
  (case-lambda
   ((x y)
    ($div-number-number x y))
   ((x)
    ($inv-number x))
   ((x y z . ls)
    (let loop ((a  ($div-number-number x y))
	       (b  z)
	       (ls ls))
      (if (null? ls)
	  ($div-number-number a b)
	(loop ($div-number-number a b) ($car ls) ($cdr ls)))))
   ))


(module ($neg-number
	 $neg-fixnum	$neg-bignum	$neg-flonum
	 $neg-ratnum	$neg-compnum	$neg-cflonum)
  (define who (quote -))

  (define ($neg-number x)
    (cond-numeric-operand x
      ((fixnum?)	($neg-fixnum  x))
      ((bignum?)	($neg-bignum  x))
      ((flonum?)	($neg-flonum  x))
      ((ratnum?)	($neg-ratnum  x))
      ((compnum?)	($neg-compnum x))
      ((cflonum?)	($neg-cflonum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($neg-fixnum x)
    (if ($fx= x (least-fixnum))
	;;The following  constant expression will be  precomputed by the
	;;optimizer.
	(- (least-fixnum))
      ($fx- x)))

  (define ($neg-bignum x)
    (if (eq? x (- (least-fixnum)))
	(least-fixnum)
      ($sub-fixnum-bignum 0 x)))

  (define ($neg-flonum x)
    ($fl- x))

  (define ($neg-ratnum x)
    (let ((y.num ($ratnum-n x))
	  (y.den ($ratnum-d x)))
      ($div-number-number ($neg-number y.num)
			  y.den)))

  (define ($neg-compnum x)
    ($make-rectangular ($neg-number ($compnum-real x))
		       ($neg-number ($compnum-imag x))))

  (define ($neg-cflonum x)
    ($make-cflonum ($fl- ($cflonum-real x))
		   ($fl- ($cflonum-imag x))))

  #| end of module: $neg-number |# )


(module ($inv-number
	 $inv-fixnum	$inv-bignum	$inv-flonum
	 $inv-ratnum	$inv-compnum	$inv-cflonum)
  (define who (quote /))

  (define ($inv-number x)
    (cond-numeric-operand x
      ((fixnum?)	($inv-fixnum  x))
      ((bignum?)	($inv-bignum  x))
      ((flonum?)	($inv-flonum  x))
      ((ratnum?)	($inv-ratnum  x))
      ((compnum?)	($inv-compnum x))
      ((cflonum?)	($inv-cflonum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($inv-fixnum x)
    (cond (($fxzero? x)
	   ;;According to R6RS this must be an assertion.
	   (%error-division-by-zero who))
	  (($fx> x 0)
	   (if ($fx= x 1)
	       1
	     ($make-ratnum 1 x)))
	  (else
	   (if ($fx= x -1)
	       -1
	     ;;The denominator of a ratnum is always positive.
	     ($make-ratnum -1 ($neg-fixnum x))))))

  (define ($inv-bignum x)
    (if ($bignum-positive? x)
	($make-ratnum 1 x)
      ($make-ratnum -1 ($neg-bignum x))))

  (define ($inv-flonum x)
    (foreign-call "ikrt_fl_invert" x))

  (define ($inv-ratnum x)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (cond (($fx= x.num +1) ;this works also when X.NUM is not a fixnum
	     x.den)
	    (($fx= x.num -1) ;this works also when X.NUM is not a fixnum
	     ($neg-number x.den))
	    (else
	     (if (negative? x.num)
		 ($make-ratnum ($neg-number x.den) ($neg-number x.num))
	       ($make-ratnum x.den x.num))))))

  (define ($inv-compnum x)
    ;;         1                   1           x.rep - i * x.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; x.rep + i * x.imp   x.rep + i * x.imp   x.rep - i * x.imp
    ;;
    ;;   x.rep - i * x.imp         x.rep                   - x.imp
    ;; = ------------------ = ----------------- + i * -----------------
    ;;   x.rep^2 + x.rep^2    x.rep^2 + x.rep^2       x.rep^2 + x.rep^2
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      (let ((denom   ($add-number-number (square x.rep) (square x.imp)))
	    (num.rep x.rep)
	    (num.imp ($neg-number x.imp)))
	($make-rectangular ($div-number-number num.rep denom)
			   ($div-number-number num.imp denom)))))

  (define ($inv-cflonum x)
    ;;         1                   1           x.rep - i * x.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; x.rep + i * x.imp   x.rep + i * x.imp   x.rep - i * x.imp
    ;;
    ;;   x.rep - i * x.imp         x.rep                   - x.imp
    ;; = ------------------ = ----------------- + i * -----------------
    ;;   x.rep^2 + x.rep^2    x.rep^2 + x.rep^2       x.rep^2 + x.rep^2
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (let ((denom   ($add-flonum-flonum ($flsquare x.rep) ($flsquare x.imp)))
	    (num.rep x.rep)
	    (num.imp ($neg-flonum x.imp)))
	($make-cflonum ($div-flonum-flonum num.rep denom)
		       ($div-flonum-flonum num.imp denom)))))

  #| end of module: $inv-number |# )


(module ($add-number-number
	 $add-fixnum-number	$add-bignum-number	$add-flonum-number
	 $add-ratnum-number	$add-compnum-number	$add-cflonum-number
	 $add-number-fixnum	$add-number-bignum	$add-number-flonum
	 $add-number-ratnum	$add-number-compnum	$add-number-cflonum
	 $add-fixnum-fixnum	$add-fixnum-bignum	$add-fixnum-flonum
	 $add-fixnum-ratnum	$add-fixnum-compnum	$add-fixnum-cflonum
	 $add-bignum-fixnum	$add-bignum-bignum	$add-bignum-flonum
	 $add-bignum-ratnum	$add-bignum-compnum	$add-bignum-cflonum
	 $add-flonum-fixnum	$add-flonum-bignum	$add-flonum-flonum
	 $add-flonum-ratnum	$add-flonum-compnum	$add-flonum-cflonum
	 $add-ratnum-fixnum	$add-ratnum-bignum	$add-ratnum-flonum
	 $add-ratnum-ratnum	$add-ratnum-compnum	$add-ratnum-cflonum
	 $add-compnum-fixnum	$add-compnum-bignum	$add-compnum-ratnum
	 $add-compnum-compnum	$add-compnum-flonum	$add-compnum-cflonum
	 $add-cflonum-fixnum	$add-cflonum-bignum	$add-cflonum-ratnum
	 $add-cflonum-flonum	$add-cflonum-compnum	$add-cflonum-cflonum)
  (define who (quote +))

  (define ($add-number-number x y)
    (cond-numeric-operand x
      ((fixnum?)	($add-fixnum-number  x y))
      ((bignum?)	($add-bignum-number  x y))
      ((flonum?)	($add-flonum-number  x y))
      ((ratnum?)	($add-ratnum-number  x y))
      ((compnum?)	($add-compnum-number x y))
      ((cflonum?)	($add-cflonum-number x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($add-fixnum-number x y)
    (cond-numeric-operand y
     ((fixnum?)		($add-fixnum-fixnum  x y))
     ((bignum?)		($add-fixnum-bignum  x y))
     ((ratnum?)		($add-fixnum-ratnum  x y))
     ((flonum?)		($add-fixnum-flonum  x y))
     ((compnum?)	($add-fixnum-compnum x y))
     ((cflonum?)	($add-fixnum-cflonum x y))
     (else
      (err who y))))

  (define ($add-bignum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($add-bignum-fixnum  x y))
      ((bignum?)	($add-bignum-bignum  x y))
      ((ratnum?)	($add-bignum-ratnum  x y))
      ((flonum?)	($add-bignum-flonum  x y))
      ((compnum?)	($add-bignum-compnum x y))
      ((cflonum?)	($add-bignum-cflonum x y))
      (else
       (err who y))))

  (define ($add-flonum-number x y)
    (cond-numeric-operand y
     ((fixnum?)		($add-flonum-fixnum  x y))
     ((bignum?)		($add-flonum-bignum  x y))
     ((ratnum?)		($add-flonum-ratnum  x y))
     ((flonum?)		($add-flonum-flonum  x y))
     ((compnum?)	($add-flonum-compnum x y))
     ((cflonum?)	($add-flonum-cflonum x y))
     (else
      (err who y))))

  (define ($add-ratnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($add-ratnum-fixnum  x y))
      ((bignum?)	($add-ratnum-bignum  x y))
      ((flonum?)	($add-ratnum-flonum  x y))
      ((ratnum?)	($add-ratnum-ratnum  x y))
      ((compnum?)	($add-ratnum-compnum x y))
      ((cflonum?)	($add-ratnum-cflonum x y))
      (else
       (err who y))))

  (define ($add-compnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($add-compnum-fixnum  x y))
      ((bignum?)	($add-compnum-bignum  x y))
      ((ratnum?)	($add-compnum-ratnum  x y))
      ((flonum?)	($add-compnum-flonum  x y))
      ((compnum?)	($add-compnum-compnum x y))
      ((cflonum?)	($add-compnum-cflonum x y))
      (else
       (err who y))))

  (define ($add-cflonum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($add-cflonum-flonum  x y))
      ((cflonum?)	($add-cflonum-cflonum x y))
      ((fixnum?)	($add-cflonum-fixnum  x y))
      ((bignum?)	($add-cflonum-bignum  x y))
      ((ratnum?)	($add-cflonum-ratnum  x y))
      ((compnum?)	($add-cflonum-compnum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($add-number-fixnum x y)
    (cond-numeric-operand x
     ((fixnum?)		($add-fixnum-fixnum  x y))
     ((bignum?)		($add-bignum-fixnum  x y))
     ((ratnum?)		($add-ratnum-fixnum  x y))
     ((flonum?)		($add-flonum-fixnum  x y))
     ((compnum?)	($add-compnum-fixnum x y))
     ((cflonum?)	($add-cflonum-fixnum x y))
     (else
      (err who y))))

  (define ($add-number-bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($add-fixnum-bignum  x y))
      ((bignum?)	($add-bignum-bignum  x y))
      ((ratnum?)	($add-ratnum-bignum  x y))
      ((flonum?)	($add-flonum-bignum  x y))
      ((compnum?)	($add-compnum-bignum x y))
      ((cflonum?)	($add-cflonum-bignum x y))
      (else
       (err who y))))

  (define ($add-number-flonum x y)
    (cond-numeric-operand x
     ((fixnum?)		($add-fixnum-flonum  x y))
     ((bignum?)		($add-bignum-flonum  x y))
     ((ratnum?)		($add-ratnum-flonum  x y))
     ((flonum?)		($add-flonum-flonum  x y))
     ((compnum?)	($add-compnum-flonum x y))
     ((cflonum?)	($add-cflonum-flonum x y))
     (else
      (err who y))))

  (define ($add-number-ratnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($add-fixnum-ratnum  x y))
      ((bignum?)	($add-bignum-ratnum  x y))
      ((ratnum?)	($add-ratnum-ratnum  x y))
      ((flonum?)	($add-flonum-ratnum  x y))
      ((compnum?)	($add-compnum-ratnum x y))
      ((cflonum?)	($add-cflonum-ratnum x y))
      (else
       (err who y))))

  (define ($add-number-compnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($add-fixnum-compnum  x y))
      ((bignum?)	($add-bignum-compnum  x y))
      ((ratnum?)	($add-ratnum-compnum  x y))
      ((flonum?)	($add-flonum-compnum  x y))
      ((compnum?)	($add-compnum-compnum x y))
      ((cflonum?)	($add-cflonum-compnum x y))
      (else
       (err who y))))

  (define ($add-number-cflonum x y)
    (cond-numeric-operand x
      ((flonum?)	($add-flonum-cflonum  x y))
      ((cflonum?)	($add-cflonum-cflonum x y))
      ((fixnum?)	($add-fixnum-cflonum  x y))
      ((bignum?)	($add-bignum-cflonum  x y))
      ((ratnum?)	($add-ratnum-cflonum  x y))
      ((compnum?)	($add-compnum-cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($add-fixnum-fixnum x y)
    (foreign-call "ikrt_fxfxplus" x y))

  (define ($add-fixnum-bignum x y)
    (foreign-call "ikrt_fxbnplus" x y))

  (define ($add-fixnum-flonum x y)
    ($fl+ ($fixnum->flonum x) y))

  (define ($add-fixnum-ratnum x y)
    ;;
    ;;     y.num   x * y.den + y.num
    ;; x + ----- = -----------------
    ;;     y.den         y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($make-ratnum ($add-number-number ($mul-fixnum-number x y.den)
					y.num)
		    y.den)))

  (define ($add-fixnum-compnum x y)
    ($make-rectangular ($add-fixnum-number x ($compnum-real y))
		       ($compnum-imag y)))

  (define ($add-fixnum-cflonum x y)
    ($make-cflonum ($add-fixnum-flonum x ($cflonum-real y))
		   ($cflonum-imag y)))

;;; --------------------------------------------------------------------

  (define ($add-bignum-fixnum x y)
    (foreign-call "ikrt_fxbnplus" y x))

  (define ($add-bignum-bignum x y)
    (foreign-call "ikrt_bnbnplus" x y))

  (define ($add-bignum-flonum x y)
    ($fl+ ($bignum->flonum x) y))

  (define ($add-bignum-ratnum x y)
    ;;
    ;;     y.num   x * y.den + y.num
    ;; x + ----- = -----------------
    ;;     y.den         y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($make-ratnum ($add-number-number ($mul-bignum-number x y.den)
					y.num)
		    y.den)))

  (define ($add-bignum-compnum x y)
    ($make-rectangular ($add-bignum-number x ($compnum-real y))
		       ($compnum-imag y)))

  (define ($add-bignum-cflonum x y)
    ($make-cflonum ($add-bignum-flonum x ($cflonum-real y))
		   ($cflonum-imag y)))

;;; --------------------------------------------------------------------

  (define ($add-flonum-fixnum x y)
    ($fl+ x ($fixnum->flonum y)))

  (define ($add-flonum-bignum x y)
    ($fl+ x ($bignum->flonum y)))

  (define ($add-flonum-flonum x y)
    ($fl+ x y))

  (define ($add-flonum-ratnum x y)
    ($fl+ x ($ratnum->flonum y)))

  (define ($add-flonum-compnum x y)
    (let ((y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      ($make-rectangular ($add-flonum-number x y.rep)
			 y.imp)))

  (define ($add-flonum-cflonum x y)
    ($make-cflonum ($fl+ x ($cflonum-real y))
		   ($cflonum-imag y)))

;;; --------------------------------------------------------------------

  (define ($add-ratnum-fixnum x y)
    ;;
    ;; x.num       x.num + y * x.den
    ;; ----- + y = -----------------
    ;; x.den            x.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      ($make-ratnum ($add-number-number x.num ($mul-fixnum-number y x.den))
		    x.den)))

  (define ($add-ratnum-bignum x y)
    ;; x.num       x.num + y * x.den
    ;; ----- + y = -----------------
    ;; x.den            x.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      ($make-ratnum ($add-number-number x.num ($mul-bignum-number y x.den))
		    x.den)))

  (define ($add-ratnum-flonum x y)
    ($fl+ y ($ratnum->flonum x)))

  (define ($add-ratnum-ratnum x y)
    ;; x.num   y.num   x.num * y.den + x.den * y.num
    ;; ----- + ----- = -----------------------------
    ;; x.den   y.den          x.den * y.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (y.num ($ratnum-n y))
	  (x.den ($ratnum-d x))
	  (y.den ($ratnum-d y)))
      ($div-number-number ($add-number-number ($mul-number-number x.num y.den)
					      ($mul-number-number x.den y.num))
			  ($mul-number-number x.den y.den))))

  (define ($add-ratnum-compnum x y)
    ;;
    ;; x.num                       x.num + y.rep * x.den
    ;; ----- + y.rep + i * y.imp = --------------------- + i * y.imp
    ;; x.den                              x.den
    ;;
    ($make-rectangular ($add-ratnum-number x ($compnum-real y))
		       ($compnum-imag y)))

  (define ($add-ratnum-cflonum x y)
    ;;
    ;; x.num                       x.num + y.rep * x.den
    ;; ----- + y.rep + i * y.imp = --------------------- + i * y.imp
    ;; x.den                              x.den
    ;;
    ($make-cflonum ($add-ratnum-number x ($cflonum-real y))
		   ($cflonum-imag y)))

;;; --------------------------------------------------------------------

  (define ($add-compnum-fixnum x y)
    ($make-rectangular ($add-number-fixnum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($add-compnum-bignum x y)
    ($make-rectangular ($add-number-bignum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($add-compnum-ratnum x y)
    ($make-rectangular ($add-number-ratnum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($add-compnum-compnum x y)
    ($make-rectangular ($add-number-number ($compnum-real x) ($compnum-real y))
		       ($add-number-number ($compnum-imag x) ($compnum-imag y))))

  (define ($add-compnum-flonum x y)
    ($make-rectangular ($add-flonum-number y ($compnum-real x))
		       ($compnum-imag x)))

  (define ($add-compnum-cflonum x y)
    ($make-cflonum ($add-number-flonum ($compnum-real x) ($cflonum-real y))
		   ($add-number-flonum ($compnum-imag x) ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($add-cflonum-fixnum x y)
    ($make-cflonum ($add-flonum-fixnum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($add-cflonum-bignum x y)
    ($make-cflonum ($add-flonum-bignum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($add-cflonum-ratnum x y)
    ($make-compnum ($add-flonum-ratnum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($add-cflonum-flonum x y)
    ($make-cflonum ($fl+ ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($add-cflonum-compnum x y)
    ($make-cflonum ($add-flonum-number ($cflonum-real x) ($compnum-real y))
		   ($add-flonum-number ($cflonum-imag x) ($compnum-imag y))))

  (define ($add-cflonum-cflonum x y)
    ($make-cflonum ($fl+ ($cflonum-real x) ($cflonum-real y))
		   ($fl+ ($cflonum-imag x) ($cflonum-imag y))))

  #| end of module: $add-number-number |# )


(module ($sub-number-number
	 $sub-fixnum-number	$sub-bignum-number	$sub-flonum-number
	 $sub-ratnum-number	$sub-compnum-number	$sub-cflonum-number
	 $sub-number-fixnum	$sub-number-bignum	$sub-number-flonum
	 $sub-number-ratnum	$sub-number-compnum	$sub-number-cflonum
	 $sub-fixnum-fixnum	$sub-fixnum-bignum	$sub-fixnum-flonum
	 $sub-fixnum-ratnum	$sub-fixnum-compnum	$sub-fixnum-cflonum
	 $sub-bignum-fixnum	$sub-bignum-bignum	$sub-bignum-flonum
	 $sub-bignum-ratnum	$sub-bignum-compnum	$sub-bignum-cflonum
	 $sub-flonum-fixnum	$sub-flonum-bignum	$sub-flonum-ratnum
	 $sub-flonum-flonum	$sub-flonum-compnum	$sub-flonum-cflonum
	 $sub-ratnum-fixnum	$sub-ratnum-bignum	$sub-ratnum-flonum
	 $sub-ratnum-ratnum	$sub-ratnum-compnum	$sub-ratnum-cflonum
	 $sub-compnum-fixnum	$sub-compnum-bignum	$sub-compnum-ratnum
	 $sub-compnum-compnum	$sub-compnum-flonum	$sub-compnum-cflonum
	 $sub-cflonum-fixnum	$sub-cflonum-bignum	$sub-cflonum-ratnum
	 $sub-cflonum-flonum	$sub-cflonum-compnum	$sub-cflonum-cflonum)
  (define who (quote -))

  (define ($sub-number-number x y)
    (cond-numeric-operand x
      ((fixnum?)	($sub-fixnum-number  x y))
      ((bignum?)	($sub-bignum-number  x y))
      ((flonum?)	($sub-flonum-number  x y))
      ((ratnum?)	($sub-ratnum-number  x y))
      ((compnum?)	($sub-compnum-number x y))
      ((cflonum?)	($sub-cflonum-number x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($sub-fixnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($sub-fixnum-fixnum  x y))
      ((bignum?)	($sub-fixnum-bignum  x y))
      ((flonum?)	($sub-fixnum-flonum  x y))
      ((ratnum?)	($sub-fixnum-ratnum  x y))
      ((compnum?)	($sub-fixnum-compnum x y))
      ((cflonum?)	($sub-fixnum-cflonum x y))
      (else
       (err who y))))

  (define ($sub-bignum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($sub-bignum-fixnum x y))
      ((bignum?)	($sub-bignum-bignum x y))
      ((flonum?)	($sub-bignum-flonum x y))
      ((ratnum?)	($sub-bignum-ratnum x y))
      ((compnum?)	($sub-bignum-compnum x y))
      ((cflonum?)	($sub-bignum-cflonum x y))
      (else
       (err who y))))

  (define ($sub-flonum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($sub-flonum-flonum  x y))
      ((cflonum?)	($sub-flonum-cflonum x y))
      ((fixnum?)	($sub-flonum-fixnum  x y))
      ((bignum?)	($sub-flonum-bignum  x y))
      ((ratnum?)	($sub-flonum-ratnum  x y))
      ((compnum?)	($sub-flonum-compnum x y))
      (else
       (err who y))))

  (define ($sub-ratnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($sub-ratnum-fixnum  x y))
      ((bignum?)	($sub-ratnum-bignum  x y))
      ((flonum?)	($sub-ratnum-flonum  x y))
      ((ratnum?)	($sub-ratnum-ratnum  x y))
      ((compnum?)	($sub-ratnum-compnum x y))
      ((cflonum?)	($sub-ratnum-cflonum x y))
      (else
       (err who y))))

  (define ($sub-compnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($sub-compnum-fixnum  x y))
      ((bignum?)	($sub-compnum-bignum  x y))
      ((flonum?)	($sub-compnum-flonum  x y))
      ((ratnum?)	($sub-compnum-ratnum  x y))
      ((compnum?)	($sub-compnum-compnum x y))
      ((cflonum?)	($sub-compnum-cflonum x y))
      (else
       (err who y))))

  (define ($sub-cflonum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($sub-cflonum-flonum  x y))
      ((cflonum?)	($sub-cflonum-cflonum x y))
      ((fixnum?)	($sub-cflonum-fixnum  x y))
      ((bignum?)	($sub-cflonum-bignum  x y))
      ((ratnum?)	($sub-cflonum-ratnum  x y))
      ((compnum?)	($sub-cflonum-compnum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($sub-number-fixnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($sub-fixnum-fixnum  x y))
      ((bignum?)	($sub-bignum-fixnum  x y))
      ((flonum?)	($sub-flonum-fixnum  x y))
      ((ratnum?)	($sub-ratnum-fixnum  x y))
      ((compnum?)	($sub-compnum-fixnum x y))
      ((cflonum?)	($sub-cflonum-fixnum x y))
      (else
       (err who y))))

  (define ($sub-number-bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($sub-fixnum-bignum  x y))
      ((bignum?)	($sub-bignum-bignum  x y))
      ((flonum?)	($sub-flonum-bignum  x y))
      ((ratnum?)	($sub-ratnum-bignum  x y))
      ((compnum?)	($sub-compnum-bignum x y))
      ((cflonum?)	($sub-cflonum-bignum x y))
      (else
       (err who y))))

  (define ($sub-number-flonum x y)
    (cond-numeric-operand x
      ((flonum?)	($sub-flonum-flonum  x y))
      ((cflonum?)	($sub-cflonum-flonum x y))
      ((fixnum?)	($sub-fixnum-flonum  x y))
      ((bignum?)	($sub-bignum-flonum  x y))
      ((ratnum?)	($sub-ratnum-flonum  x y))
      ((compnum?)	($sub-compnum-flonum x y))
      (else
       (err who y))))

  (define ($sub-number-ratnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($sub-fixnum-ratnum  x y))
      ((bignum?)	($sub-bignum-ratnum  x y))
      ((flonum?)	($sub-flonum-ratnum  x y))
      ((ratnum?)	($sub-ratnum-ratnum  x y))
      ((compnum?)	($sub-compnum-ratnum x y))
      ((cflonum?)	($sub-cflonum-ratnum x y))
      (else
       (err who y))))

  (define ($sub-number-compnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($sub-fixnum-compnum  x y))
      ((bignum?)	($sub-bignum-compnum  x y))
      ((flonum?)	($sub-flonum-compnum  x y))
      ((ratnum?)	($sub-ratnum-compnum  x y))
      ((compnum?)	($sub-compnum-compnum x y))
      ((cflonum?)	($sub-cflonum-compnum x y))
      (else
       (err who y))))

  (define ($sub-number-cflonum x y)
    (cond-numeric-operand x
      ((flonum?)	($sub-flonum-cflonum  x y))
      ((cflonum?)	($sub-cflonum-cflonum x y))
      ((fixnum?)	($sub-fixnum-cflonum  x y))
      ((bignum?)	($sub-bignum-cflonum  x y))
      ((ratnum?)	($sub-ratnum-cflonum  x y))
      ((compnum?)	($sub-compnum-cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($sub-fixnum-fixnum x y)
    (foreign-call "ikrt_fxfxminus" x y))

  (define ($sub-fixnum-bignum x y)
    (foreign-call "ikrt_fxbnminus" x y))

  (define ($sub-fixnum-flonum x y)
    (if ($fxzero? x)
	($fl- y)
      ($fl- ($fixnum->flonum x) y)))

  (define ($sub-fixnum-ratnum x y)
    ;;     y.num   x * y.den - y.num
    ;; x - ----- = -----------------
    ;;     y.den        y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($div-number-number ($sub-number-number ($mul-fixnum-number x y.den) y.num)
			  y.den)))

  (define ($sub-fixnum-compnum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-compnum ($sub-fixnum-number x ($compnum-real y))
		   ($neg-number ($compnum-imag y))))

  (define ($sub-fixnum-cflonum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-cflonum ($sub-fixnum-flonum x ($cflonum-real y))
		   ($fl- ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($sub-bignum-fixnum x y)
    (foreign-call "ikrt_bnfxminus" x y))

  (define ($sub-bignum-bignum x y)
    (foreign-call "ikrt_bnbnminus" x y))

  (define ($sub-bignum-flonum x y)
    ($fl- ($bignum->flonum x) y))

  (define ($sub-bignum-ratnum x y)
    ;;     y.num   x * y.den - y.num
    ;; x - ----- = -----------------
    ;;     y.den        y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($div-number-number ($sub-number-number ($mul-bignum-number x y.den) y.num)
			  y.den)))

  (define ($sub-bignum-compnum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-rectangular ($sub-bignum-number x ($compnum-real y))
		       ($neg-number ($compnum-imag y))))

  (define ($sub-bignum-cflonum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-cflonum ($sub-bignum-flonum x ($cflonum-real y))
		   ($fl- ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($sub-flonum-fixnum x y)
    ($fl- x ($fixnum->flonum y)))

  (define ($sub-flonum-bignum x y)
    ($fl- x ($bignum->flonum y)))

  (define ($sub-flonum-ratnum x y)
    ;;     y.num   x * y.den - y.num
    ;; x - ----- = -----------------
    ;;     y.den        y.den
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($div-flonum-number ($sub-flonum-number ($mul-flonum-number x y.den) y.num)
			  y.den)))

  (define ($sub-flonum-flonum x y)
    ($fl- x y))

  (define ($sub-flonum-compnum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-rectangular ($sub-flonum-number x ($compnum-real y))
		       ($neg-number ($compnum-imag y))))

  (define ($sub-flonum-cflonum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-cflonum ($fl- x ($cflonum-real y))
		   ($fl-   ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($sub-ratnum-fixnum x y)
    ;; x.num       x.num - y * x.den
    ;; ----- - y = -----------------
    ;; x.den            x.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      ($div-number-number ($sub-number-number x.num ($mul-fixnum-number y x.den))
			  x.den)))

  (define ($sub-ratnum-bignum x y)
    ;; x.num       x.num - y * x.den
    ;; ----- - y = -----------------
    ;; x.den            x.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      ($div-number-number ($sub-number-number x.num ($mul-bignum-number y x.den))
			  x.den)))

  (define ($sub-ratnum-flonum x y)
    ;; x.num       x.num - y * x.den
    ;; ----- - y = -----------------
    ;; x.den            x.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      ($div-flonum-number ($sub-number-flonum x.num ($mul-flonum-number y x.den))
			  x.den)))

  (define ($sub-ratnum-ratnum x y)
    ;; x.num   y.num   x.num * y.den - x.den * y.num
    ;; ----- - ----- = -----------------------------
    ;; x.den   y.den           x.den * y.den
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x))
	  (y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($div-number-number ($sub-number-number ($mul-number-number x.num y.den)
					      ($mul-number-number y.num x.den))
			  ($mul-number-number x.den y.den))))

  (define ($sub-ratnum-compnum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-rectangular ($sub-ratnum-number x ($compnum-real y))
		       ($neg-number ($compnum-imag y))))

  (define ($sub-ratnum-cflonum x y)
    ;; x - (y.rep + i * y.imp) = (x - y.rep) - i * y.imp
    ;;                         = (x - y.rep) + i * (0 - y.imp)
    ;;
    ($make-cflonum ($sub-ratnum-flonum x ($cflonum-real y))
		   ($fl- ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($sub-compnum-fixnum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-rectangular ($sub-number-fixnum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($sub-compnum-bignum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-rectangular ($sub-number-bignum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($sub-compnum-ratnum x y)
    ;;                       y.num            y.num
    ;; (x.rep + i * x.imp) - ----- = (x.rep - -----) + i * x.imp
    ;;                       y.den            y.den
    ;;
    ($make-rectangular ($sub-number-ratnum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($sub-compnum-compnum x y)
    ;; (x.rep + i * x.imp) - (y.rep + i * y.imp)
    ;; = (x.rep - y.rep) + i * (x.imp - y.imp)
    ;;
    ($make-rectangular ($sub-number-number ($compnum-real x) ($compnum-real y))
		       ($sub-number-number ($compnum-imag x) ($compnum-imag y))))

  (define ($sub-compnum-flonum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-rectangular ($sub-number-flonum ($compnum-real x) y)
		       ($compnum-imag x)))

  (define ($sub-compnum-cflonum x y)
    ;; (x.rep + i * x.imp) - (y.rep + i * y.imp)
    ;; = (x.rep - y.rep) + i * (x.imp - y.imp)
    ;;
    ($make-cflonum ($sub-number-flonum ($compnum-real x) ($cflonum-real y))
		   ($sub-number-flonum ($compnum-imag x) ($cflonum-imag y))))

;;; --------------------------------------------------------------------

  (define ($sub-cflonum-fixnum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-cflonum ($sub-flonum-fixnum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($sub-cflonum-bignum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-cflonum ($sub-flonum-bignum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($sub-cflonum-ratnum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-cflonum ($sub-flonum-ratnum ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($sub-cflonum-flonum x y)
    ;; (x.rep + i * x.imp) - y = (x.rep - y) + i * x.imp
    ;;
    ($make-cflonum ($fl- ($cflonum-real x) y)
		   ($cflonum-imag x)))

  (define ($sub-cflonum-compnum x y)
    ;; (x.rep + i * x.imp) - (y.rep + i * y.imp)
    ;; = (x.rep - y.rep) + i * (x.imp - y.imp)
    ;;
    ($make-cflonum ($sub-flonum-number ($cflonum-real x) ($compnum-real y))
		   ($sub-flonum-number ($cflonum-imag x) ($compnum-imag y))))

  (define ($sub-cflonum-cflonum x y)
    ;; (x.rep + i * x.imp) - (y.rep + i * y.imp)
    ;; = (x.rep - y.rep) + i * (x.imp - y.imp)
    ;;
    ($make-cflonum ($fl- ($cflonum-real x) ($cflonum-real y))
		   ($fl- ($cflonum-imag x) ($cflonum-imag y))))

  #| end of module: $sub-number-number |# )


(module ($mul-number-number
	 $mul-fixnum-number	$mul-bignum-number	$mul-flonum-number
	 $mul-ratnum-number	$mul-compnum-number	$mul-cflonum-number
	 $mul-number-fixnum	$mul-number-bignum	$mul-number-flonum
	 $mul-number-ratnum	$mul-number-compnum	$mul-number-cflonum
	 $mul-fixnum-fixnum	$mul-fixnum-bignum	$mul-fixnum-flonum
	 $mul-fixnum-ratnum	$mul-fixnum-compnum	$mul-fixnum-cflonum
	 $mul-bignum-fixnum	$mul-bignum-bignum	$mul-bignum-flonum
	 $mul-bignum-ratnum	$mul-bignum-compnum	$mul-bignum-cflonum
	 $mul-flonum-flonum	$mul-flonum-cflonum	$mul-flonum-fixnum
	 $mul-flonum-bignum	$mul-flonum-ratnum	$mul-flonum-compnum
	 $mul-ratnum-fixnum	$mul-ratnum-bignum	$mul-ratnum-flonum
	 $mul-ratnum-ratnum	$mul-ratnum-compnum	$mul-ratnum-cflonum
	 $mul-compnum-fixnum	$mul-compnum-bignum	$mul-compnum-ratnum
	 $mul-compnum-flonum	$mul-compnum-compnum	$mul-compnum-cflonum
	 $mul-cflonum-fixnum	$mul-cflonum-bignum	$mul-cflonum-ratnum
	 $mul-cflonum-flonum	$mul-cflonum-compnum	$mul-cflonum-cflonum)
  (define who (quote *))

  (define ($mul-number-number x y)
    (cond-numeric-operand x
      ((fixnum?)	($mul-fixnum-number  x y))
      ((bignum?)	($mul-bignum-number  x y))
      ((flonum?)	($mul-flonum-number  x y))
      ((ratnum?)	($mul-ratnum-number  x y))
      ((compnum?)	($mul-compnum-number x y))
      ((cflonum?)	($mul-cflonum-number x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($mul-fixnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($mul-fixnum-fixnum  x y))
      ((bignum?)	($mul-fixnum-bignum  x y))
      ((flonum?)	($mul-fixnum-flonum  x y))
      ((ratnum?)	($mul-fixnum-ratnum  x y))
      ((compnum?)	($mul-fixnum-compnum x y))
      ((cflonum?)	($mul-fixnum-cflonum x y))
      (else
       (err who y))))

  (define ($mul-bignum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($mul-bignum-fixnum  x y))
      ((bignum?)	($mul-bignum-bignum  x y))
      ((flonum?)	($mul-bignum-flonum  x y))
      ((ratnum?)	($mul-bignum-ratnum  x y))
      ((compnum?)	($mul-bignum-compnum x y))
      ((cflonum?)	($mul-bignum-cflonum x y))
      (else
       (err who y))))

  (define ($mul-flonum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($mul-flonum-flonum  x y))
      ((cflonum?)	($mul-flonum-cflonum x y))
      ((fixnum?)	($mul-flonum-fixnum  x y))
      ((bignum?)	($mul-flonum-bignum  x y))
      ((ratnum?)	($mul-flonum-ratnum  x y))
      ((compnum?)	($mul-flonum-compnum x y))
      (else
       (err who y))))

  (define ($mul-ratnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($mul-ratnum-fixnum  x y))
      ((bignum?)	($mul-ratnum-bignum  x y))
      ((flonum?)	($mul-ratnum-flonum  x y))
      ((ratnum?)	($mul-ratnum-ratnum  x y))
      ((compnum?)	($mul-ratnum-compnum x y))
      ((cflonum?)	($mul-ratnum-cflonum x y))
      (else
       (err who y))))

  (define ($mul-compnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($mul-compnum-fixnum  x y))
      ((bignum?)	($mul-compnum-bignum  x y))
      ((ratnum?)	($mul-compnum-ratnum  x y))
      ((flonum?)	($mul-compnum-flonum  x y))
      ((compnum?)	($mul-compnum-compnum x y))
      ((cflonum?)	($mul-compnum-cflonum x y))
      (else
       (err who y))))

  (define ($mul-cflonum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($mul-cflonum-flonum  x y))
      ((cflonum?)	($mul-cflonum-cflonum x y))
      ((fixnum?)	($mul-cflonum-fixnum  x y))
      ((bignum?)	($mul-cflonum-bignum  x y))
      ((ratnum?)	($mul-cflonum-ratnum  x y))
      ((compnum?)	($mul-cflonum-compnum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($mul-number-fixnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($mul-fixnum-fixnum  x y))
      ((bignum?)	($mul-bignum-fixnum  x y))
      ((flonum?)	($mul-flonum-fixnum  x y))
      ((ratnum?)	($mul-ratnum-fixnum  x y))
      ((compnum?)	($mul-compnum-fixnum x y))
      ((cflonum?)	($mul-cflonum-fixnum x y))
      (else
       (err who y))))

  (define ($mul-number-bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($mul-fixnum-bignum  x y))
      ((bignum?)	($mul-bignum-bignum  x y))
      ((flonum?)	($mul-flonum-bignum  x y))
      ((ratnum?)	($mul-ratnum-bignum  x y))
      ((compnum?)	($mul-compnum-bignum x y))
      ((cflonum?)	($mul-cflonum-bignum x y))
      (else
       (err who y))))

  (define ($mul-number-flonum x y)
    (cond-numeric-operand x
      ((flonum?)	($mul-flonum-flonum  x y))
      ((cflonum?)	($mul-cflonum-flonum x y))
      ((fixnum?)	($mul-fixnum-flonum  x y))
      ((bignum?)	($mul-bignum-flonum  x y))
      ((ratnum?)	($mul-ratnum-flonum  x y))
      ((compnum?)	($mul-compnum-flonum x y))
      (else
       (err who y))))

  (define ($mul-number-ratnum x y)
    (cond ((ratnum?  x)		($mul-ratnum-ratnum  x y))
	  ((compnum? x)		($mul-compnum-ratnum x y))
	  ((cflonum? x)		($mul-cflonum-ratnum x y))
	  (else
	   ($mul-number-number y x))))

  (define ($mul-number-compnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($mul-fixnum-compnum  x y))
      ((bignum?)	($mul-bignum-compnum  x y))
      ((ratnum?)	($mul-ratnum-compnum  x y))
      ((flonum?)	($mul-flonum-compnum  x y))
      ((compnum?)	($mul-compnum-compnum x y))
      ((cflonum?)	($mul-cflonum-compnum x y))
      (else
       (err who y))))

  (define ($mul-number-cflonum x y)
    (cond-numeric-operand x
      ((flonum?)	($mul-flonum-cflonum  x y))
      ((cflonum?)	($mul-cflonum-cflonum x y))
      ((fixnum?)	($mul-fixnum-cflonum  x y))
      ((bignum?)	($mul-bignum-cflonum  x y))
      ((ratnum?)	($mul-ratnum-cflonum  x y))
      ((compnum?)	($mul-compnum-cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($mul-fixnum-fixnum x y)
    (cond (($fx= x 1)	y)
	  (($fx= y 1)	x)
	  (else
	   (foreign-call "ikrt_fxfxmult" x y))))

  (define ($mul-fixnum-bignum x y)
    (if ($fx= x 1)
	y
      (foreign-call "ikrt_fxbnmult" x y)))

  (define ($mul-fixnum-flonum x y)
    (if ($fx= x 1)
	y
      ($fl* ($fixnum->flonum x) y)))

  (define ($mul-fixnum-ratnum x y)
    (if ($fx= x 1)
	y
      ($div-number-number ($mul-fixnum-number x ($ratnum-n y))
			  ($ratnum-d y))))

  (define ($mul-fixnum-compnum x y)
    (if ($fx= x 1)
	y
      ($make-rectangular ($mul-fixnum-number x ($compnum-real y))
			 ($mul-fixnum-number x ($compnum-imag y)))))

  (define ($mul-fixnum-cflonum x y)
    (if ($fx= x 1)
	y
      ($make-cflonum ($mul-fixnum-flonum x ($cflonum-real y))
		     ($mul-fixnum-flonum x ($cflonum-imag y)))))

;;; --------------------------------------------------------------------

  (define ($mul-bignum-fixnum x y)
    (foreign-call "ikrt_fxbnmult" y x))

  (define ($mul-bignum-bignum x y)
    (foreign-call "ikrt_bnbnmult" x y))

  (define ($mul-bignum-flonum x y)
    ($fl* ($bignum->flonum x) y))

  (define ($mul-bignum-ratnum x y)
    ($div-number-number ($mul-bignum-number x ($ratnum-n y))
			($ratnum-d y)))

  (define ($mul-bignum-compnum x y)
    ($make-rectangular ($mul-bignum-number x ($compnum-real y))
		       ($mul-bignum-number x ($compnum-imag y))))

  (define ($mul-bignum-cflonum x y)
    ($make-cflonum ($mul-bignum-flonum x ($cflonum-real y))
		   ($mul-bignum-flonum x ($cflonum-imag y))))

;;; --------------------------------------------------------------------
;;; flonum * number = flonum or cflonum
;;; flonum * real   = flonum

  (define ($mul-flonum-fixnum x y)
    ($fl* x ($fixnum->flonum y)))

  (define ($mul-flonum-bignum x y)
    ($fl* x ($bignum->flonum y)))

  (define ($mul-flonum-ratnum x y)
    ($div-flonum-number ($mul-flonum-number x ($ratnum-n y))
			($ratnum-d y)))

  (define ($mul-flonum-flonum x y)
    ($fl* x y))

  (define ($mul-flonum-cflonum x y)
    ($make-cflonum ($fl* x ($cflonum-real y))
		   ($fl* x ($cflonum-imag y))))

  (define ($mul-flonum-compnum x y)
    ($make-cflonum ($mul-flonum-number x ($compnum-real y))
		   ($mul-flonum-number x ($compnum-imag y))))

;;; --------------------------------------------------------------------

  (define ($mul-ratnum-fixnum x y)
    ($mul-fixnum-ratnum y x))

  (define ($mul-ratnum-bignum x y)
    ($mul-bignum-ratnum y x))

  (define ($mul-ratnum-ratnum x y)
    ;; x.num   y.num   x.num * y.num
    ;; ----- * ----- = -------------
    ;; x.den   y.den   x.den * y.den
    ;;
    ($div-number-number ($mul-number-number ($ratnum-n x) ($ratnum-n y))
			($mul-number-number ($ratnum-d x) ($ratnum-d y))))

  (define ($mul-ratnum-flonum x y)
    ($mul-flonum-ratnum y x))

  (define ($mul-ratnum-compnum x y)
    ($make-rectangular ($mul-ratnum-number x ($compnum-real y))
		       ($mul-ratnum-number x ($compnum-imag y))))

  (define ($mul-ratnum-cflonum x y)
    ($make-cflonum ($mul-flonum-ratnum ($cflonum-real y) x)
		   ($mul-flonum-ratnum ($cflonum-imag y) x)))

;;; --------------------------------------------------------------------

  (define ($mul-compnum-fixnum x y)
    ($make-rectangular ($mul-fixnum-number y ($compnum-real x))
		       ($mul-fixnum-number y ($compnum-imag x))))

  (define ($mul-compnum-bignum x y)
    ($make-rectangular ($mul-bignum-number y ($compnum-real x))
		       ($mul-bignum-number y ($compnum-imag x))))

  (define ($mul-compnum-ratnum x y)
    ($make-rectangular ($mul-ratnum-number y ($compnum-real x))
		       ($mul-ratnum-number y ($compnum-imag x))))

  (define ($mul-compnum-flonum x y)
    ($make-cflonum ($mul-flonum-number y ($compnum-real x))
		   ($mul-flonum-number y ($compnum-imag x))))

  (define ($mul-compnum-compnum x y)
    ;; (x.rep + i * x.imp) * (y.rep + i * y.imp)
    ;; = (x.rep * y.rep - x.imp * y.imp) + i * (x.rep * y.imp + x.imp * y.rep)
    ;;
    (let ((x.rep ($compnum-real x))
	  (y.rep ($compnum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($compnum-imag y)))
      ($make-rectangular ($sub-number-number ($mul-number-number x.rep y.rep)
					     ($mul-number-number x.imp y.imp))
			 ($add-number-number ($mul-number-number x.rep y.imp)
					     ($mul-number-number x.imp y.rep)))))

  (define ($mul-compnum-cflonum x y)
    ;; (x.rep + i * x.imp) * (y.rep + i * y.imp)
    ;; = (x.rep * y.rep - x.imp * y.imp) + i * (x.rep * y.imp + x.imp * y.rep)
    ;;
    (let ((x.rep ($compnum-real x))
	  (y.rep ($cflonum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($cflonum-imag y)))
      ($make-cflonum ($fl- ($mul-flonum-number y.rep x.rep)
			   ($mul-flonum-number y.imp x.imp))
		     ($fl+ ($mul-flonum-number y.imp x.rep)
			   ($mul-flonum-number y.rep x.imp)))))

;;; --------------------------------------------------------------------

  (define ($mul-cflonum-fixnum x y)
    ($make-cflonum ($mul-flonum-fixnum ($compnum-real x) y)
		   ($mul-flonum-fixnum ($compnum-imag x) y)))

  (define ($mul-cflonum-bignum x y)
    ($make-cflonum ($mul-flonum-bignum ($compnum-real x) y)
		   ($mul-flonum-bignum ($compnum-imag x) y)))

  (define ($mul-cflonum-ratnum x y)
    ($make-cflonum ($mul-flonum-ratnum ($compnum-real x) y)
		   ($mul-flonum-ratnum ($compnum-imag x) y)))

  (define ($mul-cflonum-flonum x y)
    ($make-cflonum ($fl* ($cflonum-real x) y)
		   ($fl* ($cflonum-imag x) y)))

  (define ($mul-cflonum-compnum x y)
    ;; (x.rep + i * x.imp) * (y.rep + i * y.imp)
    ;; = (x.rep * y.rep - x.imp * y.imp) + i * (x.rep * y.imp + x.imp * y.rep)
    ;;
    (let ((x.rep ($compnum-real x))
	  (y.rep ($compnum-real y))
	  (x.imp ($compnum-imag x))
	  (y.imp ($compnum-imag y)))
      ($make-cflonum ($fl- ($mul-flonum-number x.rep y.rep)
			   ($mul-flonum-number x.imp y.imp))
		     ($fl+ ($mul-flonum-number x.rep y.imp)
			   ($mul-flonum-number x.imp y.rep)))))

  (define ($mul-cflonum-cflonum x y)
    ;; (x.rep + i * x.imp) * (y.rep + i * y.imp)
    ;;    = (x.rep * y.rep - x.imp * y.imp) + i * (x.rep * y.imp + x.imp * y.rep)
    ;;
    (let ((x.rep ($cflonum-real x))
	  (y.rep ($cflonum-real y))
	  (x.imp ($cflonum-imag x))
	  (y.imp ($cflonum-imag y)))
      ($make-cflonum ($fl- ($fl* x.rep y.rep) ($fl* x.imp y.imp))
		     ($fl+ ($fl* x.rep y.imp) ($fl* x.imp y.rep)))))

  #| end of module: $mul-number-number |# )


(module ($div-number-number
	 $div-flonum-number	$div-fixnum-number	$div-bignum-number
	 $div-ratnum-number	$div-compnum-number	$div-cflonum-number
	 $div-number-flonum	$div-number-fixnum	$div-number-bignum
	 $div-number-ratnum	$div-number-compnum	$div-number-cflonum
	 $div-fixnum-flonum	$div-fixnum-fixnum	$div-fixnum-bignum
	 $div-fixnum-ratnum	$div-fixnum-compnum	$div-fixnum-cflonum
	 $div-bignum-fixnum	$div-bignum-bignum	$div-bignum-flonum
	 $div-bignum-ratnum	$div-bignum-compnum	$div-bignum-cflonum
	 $div-ratnum-fixnum	$div-ratnum-bignum	$div-ratnum-ratnum
	 $div-ratnum-flonum	$div-ratnum-compnum	$div-ratnum-cflonum
	 $div-flonum-flonum	$div-flonum-cflonum	$div-flonum-fixnum
	 $div-flonum-bignum	$div-flonum-ratnum	$div-flonum-compnum
	 $div-compnum-fixnum	$div-compnum-bignum	$div-compnum-ratnum
	 $div-compnum-flonum	$div-compnum-compnum	$div-compnum-cflonum
	 $div-cflonum-fixnum	$div-cflonum-bignum	$div-cflonum-ratnum
	 $div-cflonum-flonum	$div-cflonum-compnum	$div-cflonum-cflonum)
  (define who (quote /))

  (define ($div-number-number x y)
    (cond-numeric-operand x
      ((fixnum?)	($div-fixnum-number  x y))
      ((bignum?)	($div-bignum-number  x y))
      ((flonum?)	($div-flonum-number  x y))
      ((ratnum?)	($div-ratnum-number  x y))
      ((compnum?)	($div-compnum-number x y))
      ((cflonum?)	($div-cflonum-number x y))
      (else
       (err who x))))

;;; --------------------------------------------------------------------

  (define ($div-flonum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($div-flonum-flonum  x y))
      ((cflonum?)	($div-flonum-cflonum x y))
      ((fixnum?)	($div-flonum-fixnum  x y))
      ((bignum?)	($div-flonum-bignum  x y))
      ((ratnum?)	($div-flonum-ratnum  x y))
      ((compnum?)	($div-flonum-compnum x y))
      (else
       (err who y))))

  (define ($div-fixnum-number x y)
    (cond-numeric-operand y
      ((flonum?)	($div-fixnum-flonum  x y))
      ((cflonum?)	($div-fixnum-cflonum x y))
      ((fixnum?)	($div-fixnum-fixnum  x y))
      ((bignum?)	($div-fixnum-bignum  x y))
      ((ratnum?)	($div-fixnum-ratnum  x y))
      ((compnum?)	($div-fixnum-compnum x y))
      (else
       (err who y))))

  (define ($div-bignum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($div-bignum-fixnum  x y))
      ((bignum?)	($div-bignum-bignum  x y))
      ((flonum?)	($div-bignum-flonum  x y))
      ((ratnum?)	($div-bignum-ratnum  x y))
      ((compnum?)	($div-bignum-compnum x y))
      ((cflonum?)	($div-bignum-cflonum x y))
      (else (err who y))))

  (define ($div-ratnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($div-ratnum-fixnum  x y))
      ((bignum?)	($div-ratnum-bignum  x y))
      ((ratnum?)	($div-ratnum-ratnum  x y))
      ((flonum?)	($div-ratnum-flonum  x y))
      ((compnum?)	($div-ratnum-compnum x y))
      ((cflonum?)	($div-ratnum-cflonum x y))
      (else
       (err who y))))

  (define ($div-compnum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($div-compnum-fixnum  x y))
      ((bignum?)	($div-compnum-bignum  x y))
      ((ratnum?)	($div-compnum-ratnum  x y))
      ((flonum?)	($div-compnum-flonum  x y))
      ((compnum?)	($div-compnum-compnum x y))
      ((cflonum?)	($div-compnum-cflonum x y))
      (else
       (err who y))))

  (define ($div-cflonum-number x y)
    (cond-numeric-operand y
      ((fixnum?)	($div-cflonum-fixnum  x y))
      ((bignum?)	($div-cflonum-bignum  x y))
      ((ratnum?)	($div-cflonum-ratnum  x y))
      ((flonum?)	($div-cflonum-flonum  x y))
      ((compnum?)	($div-cflonum-compnum x y))
      ((cflonum?)	($div-cflonum-cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($div-number-flonum x y)
    (cond-numeric-operand x
      ((flonum?)	($div-flonum-flonum  x y))
      ((cflonum?)	($div-cflonum-flonum x y))
      ((fixnum?)	($div-fixnum-flonum  x y))
      ((bignum?)	($div-bignum-flonum  x y))
      ((ratnum?)	($div-ratnum-flonum  x y))
      ((compnum?)	($div-compnum-flonum x y))
      (else
       (err who y))))

  (define ($div-number-fixnum x y)
    (cond-numeric-operand x
      ((flonum?)	($div-flonum-fixnum  x y))
      ((cflonum?)	($div-cflonum-fixnum x y))
      ((fixnum?)	($div-fixnum-fixnum  x y))
      ((bignum?)	($div-bignum-fixnum  x y))
      ((ratnum?)	($div-ratnum-fixnum  x y))
      ((compnum?)	($div-compnum-fixnum x y))
      (else
       (err who y))))

  (define ($div-number-bignum x y)
    (cond-numeric-operand x
      ((fixnum?)	($div-fixnum-bignum  x y))
      ((bignum?)	($div-bignum-bignum  x y))
      ((ratnum?)	($div-ratnum-bignum  x y))
      ((flonum?)	($div-flonum-bignum  x y))
      ((compnum?)	($div-compnum-bignum x y))
      ((cflonum?)	($div-cflonum-bignum x y))
      (else
       (err who y))))

  (define ($div-number-ratnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($div-fixnum-ratnum  x y))
      ((bignum?)	($div-bignum-ratnum  x y))
      ((ratnum?)	($div-ratnum-ratnum  x y))
      ((flonum?)	($div-flonum-ratnum  x y))
      ((compnum?)	($div-compnum-ratnum x y))
      ((cflonum?)	($div-cflonum-ratnum x y))
      (else
       (err who y))))

  (define ($div-number-compnum x y)
    (cond-numeric-operand x
      ((fixnum?)	($div-fixnum-compnum  x y))
      ((bignum?)	($div-bignum-compnum  x y))
      ((ratnum?)	($div-ratnum-compnum  x y))
      ((flonum?)	($div-flonum-compnum  x y))
      ((compnum?)	($div-compnum-compnum x y))
      ((cflonum?)	($div-cflonum-compnum x y))
      (else
       (err who y))))

  (define ($div-number-cflonum x y)
    (cond-numeric-operand x
      ((fixnum?)	($div-fixnum-cflonum  x y))
      ((bignum?)	($div-bignum-cflonum  x y))
      ((ratnum?)	($div-ratnum-cflonum  x y))
      ((flonum?)	($div-flonum-cflonum  x y))
      ((compnum?)	($div-compnum-cflonum x y))
      ((cflonum?)	($div-cflonum-cflonum x y))
      (else
       (err who y))))

;;; --------------------------------------------------------------------

  (define ($div-fixnum-flonum x y)
    ($fl/ ($fixnum->flonum x) y))

  (define ($div-fixnum-fixnum x y)
    (cond (($fxzero? y)
	   (assertion-violation who "division by 0" x y))
	  (($fxzero? x)
	   0)
	  (($fx= 1 y)
	   x)
	  (($fx= x y)
	   1)
	  (($fxpositive? y)
	   (if ($fx= y 1)
	       x
	     ;;The GCD  between two  fixnums is  *not* always  a fixnum:
	     ;;when X and Y are both (least-fixnum) the GCD is a bignum.
	     ;;But we have excluded the case X  = Y above, and here Y is
	     ;;positive, so we are sure that here G is a fixnum.
	     (let ((g ($gcd-fixnum-fixnum x y)))
	       (cond (($fx= g y)
		      ;;X and Y are exactly divisible and their division
		      ;;will not overflow.  Example:
		      ;;
		      ;;   gcd(12, 6) = 6	12/6 = 2
		      ;;
		      ($fxquotient x g))
		     (($fx= g 1)
		      ;;X and Y are not exactly divisible.  Example:
		      ;;
		      ;;   gcd(12, 7) = 1	12/7 = ratnum
		      ;;
		      ($make-ratnum x y))
		     (else
		      ;;X and Y are not  exactly divisible, but they can
		      ;;be exactly divided by the same fixnum.  Example:
		      ;;
		      ;;   gcd(25, 10) = 5	25/10 = 5/2 ratnum
		      ;;
		      ($make-ratnum ($fxquotient x g)
				    ($fxquotient y g)))))))
	  (else
	   ;;Here Y is negative.
	   (if ($fx= y -1)
	       ($neg-fixnum x)
	     ;;The GCD  between two  fixnums is  *not* always  a fixnum:
	     ;;when X and Y are both (least-fixnum) the GCD is a bignum.
	     ;;But we have excluded the case X = Y above, so we are sure
	     ;;that here G is a fixnum.
	     (let ((g ($gcd-fixnum-fixnum x y)))
	       ;;The negation  of a fixnum  is not always a  fixnum, but
	       ;;the following check works  even then ($neg-fixnum g) is
	       ;;a bignum.
	       (cond (($fx= ($neg-fixnum g) y)
		      ;;X and Y are exactly divisible and their division
		      ;;will not overflow.  Example:
		      ;;
		      ;;   gcd(12, -6) = 6	12/-6 = -2
		      ;;
		      ($neg-fixnum ($fxquotient x g)))
		     (($fx= g 1)
		      ;;X and Y are not exactly divisible.  Example:
		      ;;
		      ;;   gcd(12, -7) = 1	12/-7 = ratnum
		      ;;
		      ($make-ratnum ($neg-fixnum x)
				    ($neg-fixnum y)))
		     (else
		      ;;X and Y are not  exactly divisible, but they can
		      ;;be exactly divided by the same fixnum.  Example:
		      ;;
		      ;;   gcd(25, -10) = 5	25/-10 = -5/2 ratnum
		      ;;
		      ($make-ratnum ($neg-fixnum ($fxquotient x g))
				    ($neg-fixnum ($fxquotient y g))))))))))

  (define ($div-fixnum-bignum x y)
    ;;Remember that a bignum cannot be zero, so Y is not zero here.
    (if ($fxzero? x)
	0
      ;;The GCD between a fixnum and any exact integer is *not* always a
      ;;fixnum, when  X is  the least  fixnum the GCD  can be  a bignum;
      ;;example:
      ;;
      ;;   (define smallest-positive-bignum
      ;;     (+ 1 (greatest-fixnum)))
      ;;   (gcd (least-fixnum) smallest-positive-bignum)
      ;;   => smallest-positive-bignum
      ;;
      ;;when X  is not  the least fixnum  the GCD is  a fixnum.   In any
      ;;case: G is strictly positive here.
      (let ((g ($gcd-fixnum-bignum x y)))
	(if (fixnum? g)
	    (if ($bignum-positive? y)
		(if (eq? g 1)
		    ;;X and Y are not exactly divisible.
		    ($make-ratnum x y)
		  ;;X and Y are not exactly divisible by themselves, but
		  ;;they are exactly divisible by their GCD.
		  ($make-ratnum ($quotient-fixnum-fixnum x g)
				($quotient-bignum-fixnum y g)))
	      ;;Here Y is negative.
	      (if (eq? g 1)
		  ;;X and Y are not exactly divisible.  We want a ratnum
		  ;;with positive denominator.
		  ($make-ratnum ($neg-fixnum x)
				($neg-bignum y))
		;;X and Y  are not exactly divisible  by themselves, but
		;;they are  exactly divisible by  their GCD.  We  want a
		;;ratnum with positive denominator.
		($make-ratnum ($neg-fixnum ($quotient-fixnum-fixnum x g))
			      ($neg-number ($quotient-bignum-fixnum y g)))))
	  ;;Here G is a bignum.
	  (begin
	    (assert (= x (least-fixnum)))
	    (assert (= g (- (least-fixnum))))
	    (cond ((bnbn= g y)
		   -1)
		  (($bignum-positive? y)
		   ($make-ratnum -1 ($quotient-bignum-bignum y g)))
		  ;;Here Y is negative.
		  (else
		   ;;The negation of a bignum  is *not* always a bignum;
		   ;;if  Y  is (-  (least-fixnum)),  its  negation is  a
		   ;;fixnum.  But here we know that Y is negative, so we
		   ;;know that its negation is a positive bignum.
		   ($make-ratnum +1 ($quotient-bignum-bignum ($neg-bignum y) g)))))))))

  (define ($div-fixnum-ratnum x y)
    ;;     y.num       y.den
    ;; x / ----- = x * -----
    ;;     y.den       y.num
    ;;
    (let ((y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ;;The case X = Y.NUM is rare, but we do check the equality because
      ;;it is cheap between fixnums.  EQ? is fine to compare fixnums (it
      ;;generates the same assembly code of $fx=).
      (if (eq? x y.num)
	  y.den
	($div-number-number ($mul-fixnum-number x y.den)
			    y.num))))

  (define ($div-fixnum-compnum x y)
    ;;
    ;;         x                   x           y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x * y.rep - i * x * y.imp      x * y.rep              - x * y.imp
    ;; = ------------------------- = ----------------- + i * -----------------
    ;;       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2
    ;;
    (let ((y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom   ($add-number-number (square y.rep) (square y.imp)))
	    (num.rep ($mul-fixnum-number x y.rep))
	    (num.imp (let ((x.neg ($neg-fixnum x)))
		       ;;The  negation of  a  fixnum is  *not* always  a
		       ;;fixnum; when  X is (least-fixnum)  its negation
		       ;;is a bignum.
		       (if (fixnum? x.neg)
			   ($mul-fixnum-number x.neg y.imp)
			 ($mul-bignum-number x.neg y.imp)))))
	($make-rectangular ($div-number-number num.rep denom)
			   ($div-number-number num.imp denom)))))

  (define ($div-fixnum-cflonum x y)
    ;;
    ;;         x                   x           y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x * y.rep - i * x * y.imp      x * y.rep              - x * y.imp
    ;; = ------------------------- = ----------------- + i * -----------------
    ;;       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2
    ;;
    (let ((y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom   ($fl+ ($flsquare y.rep) ($flsquare y.imp)))
	    (num.rep ($mul-fixnum-flonum x y.rep))
	    (num.imp (let ((x.neg ($neg-fixnum x)))
		       ;;The  negation of  a  fixnum is  *not* always  a
		       ;;fixnum; when  X is (least-fixnum)  its negation
		       ;;is a bignum.
		       (if (fixnum? x.neg)
			   ($mul-fixnum-flonum x.neg y.imp)
			 ($mul-bignum-flonum x.neg y.imp)))))
	($make-cflonum ($fl/ num.rep denom)
		       ($fl/ num.imp denom)))))

;;; --------------------------------------------------------------------

  (define ($div-bignum-fixnum x y)
    ;;Remember that a bignum cannot be zero, so X is not zero here.
    (cond (($fxzero? y)
	   (assertion-violation who "division by 0" x y))
	  (($fx= 1 y)
	   x)
	  (($fxpositive? y)
	   (if ($fx= y 1)
	       x
	     ;;The GCD between  any exact integer and a  fixnum is *not*
	     ;;always a fixnum; the GCD is always positive.
	     (let ((g ($gcd-bignum-fixnum x y)))
	       (cond ((eq? g 1)
		      ;;X and Y are not exactly divisible.
		      ($make-ratnum x y))
		     ((= g y)
		      ;;X and Y are exactly divisible.
		      ($quotient-bignum-number x g))
		     (else
		      ;;X and Y are not  exactly divisible, but they are
		      ;;both exactly divisible by their GCD.
		      ($make-ratnum ($quotient-bignum-number x g)
				    ($quotient-fixnum-number y g)))))))
	  (else ;here Y is negative
	   (assert ($fxnegative? y))
	   (if ($fx= y -1)
	       ($neg-bignum x)
	     ;;The GCD between  any exact integer and a  fixnum is *not*
	     ;;always a fixnum; the GCD is always positive.
	     (let ((g ($gcd-bignum-fixnum x y)))
	       (cond ((eq? g 1)
		      ;;X and  Y are not  exactly divisible.  We  want a
		      ;;ratnum with positive denominator.
		      ($make-ratnum ($neg-bignum x)
				    ($neg-fixnum y)))
		     ((= ($neg-number g) y)
		      ;;X and Y are exactly divisible.
		      ($neg-number ($quotient-bignum-number x g)))
		     (else
		      ;;X and Y are not  exactly divisible, but they are
		      ;;both exactly divisible by  their GCD.  We want a
		      ;;ratnum with positive denominator.
		      ($make-ratnum ($neg-number ($quotient-bignum-number x g))
				    ($neg-fixnum ($quotient-fixnum-number y g))))))))))

  (define ($div-bignum-bignum x y)
    ;;Remember that a bignum cannot be zero, so neither X nor Y are zero
    ;;here.
    ;;
    ;;The GCD of  two bignums is a  fixnum or bignum; the  GCD is always
    ;;positive.
    (if (bnbn= x y)
	1
      (let ((g ($gcd-bignum-bignum x y)))
	(cond (($fx= g 1) ;this words with any object G
	       ;;X and  Y are not  exactly divisible.  We want  a ratnum
	       ;;with positive denominator.
	       (if ($bignum-positive? y)
		   ($make-ratnum x y)
		 ($make-ratnum ($neg-bignum x)
			       ($neg-bignum y))))
	      (($bignum-positive? y)
	       (if (= g y)
		   ;;X and Y are exactly divisible.
		   ($quotient-bignum-number x g)
		 ;;X and Y are not  exactly divisible, but they are both
		 ;;divisible by their GCD.
		 ($make-ratnum ($quotient-bignum-number x g)
			       ($quotient-bignum-number y g))))
	      (else ;here Y is negative
	       (let ((y ($neg-number y)))
		 (if (= g y)
		     ;;X and Y are exactly divisible.
		     ($neg-number ($quotient-bignum-number x g))
		   ;;X and  Y are  not exactly  divisible, but  they are
		   ;;both divisible by their GCD.  We want a ratnum with
		   ;;positive denominator.
		   ($make-ratnum ($neg-number ($quotient-bignum-number x g))
				 ($quotient-bignum-number y g)))))))))

  (define ($div-bignum-flonum x y)
    ($fl/ ($bignum->flonum x) y))

  (define ($div-bignum-ratnum x y)
    ;;     y.num       y.den
    ;; x / ----- = x * -----
    ;;     y.den       y.num
    ;;
    ($div-number-number ($mul-bignum-number x ($ratnum-d y))
			($ratnum-n y)))

  (define ($div-bignum-compnum x y)
    ;;         x                   x           y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x * y.rep - i * x * y.imp      x * y.rep              - x * y.imp
    ;; = ------------------------- = ----------------- + i * -----------------
    ;;       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2
    ;;
    (let ((y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom   ($add-number-number (square y.rep) (square y.imp)))
	    (num.rep ($mul-bignum-number x y.rep))
	    (num.imp (let ((x.neg ($neg-bignum x)))
		       ;;The  negation of  a  bignum is  *not* always  a
		       ;;bignum;  when  X  is  (-  (least-fixnum)),  its
		       ;;negation is a fixnum.
		       (if (bignum? x.neg)
			   ($mul-bignum-number x.neg y.imp)
			 ($mul-fixnum-number x.neg y.imp)))))
	($make-rectangular ($div-number-number num.rep denom)
			   ($div-number-number num.imp denom)))))

  (define ($div-bignum-cflonum x y)
    ;;         x                   x           y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x * y.rep - i * x * y.imp      x * y.rep              - x * y.imp
    ;; = ------------------------- = ----------------- + i * -----------------
    ;;       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2
    ;;
    (let ((y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom   ($add-flonum-flonum ($flsquare y.rep) ($flsquare y.imp)))
	    (num.rep ($mul-bignum-flonum x y.rep))
	    (num.imp (let ((x.neg ($neg-bignum x)))
		       ;;The  negation of  a  bignum is  *not* always  a
		       ;;bignum;  when  X  is  (-  (least-fixnum)),  its
		       ;;negation is a fixnum.
		       (if (bignum? x.neg)
			   ($mul-bignum-flonum x.neg y.imp)
			 ($mul-fixnum-flonum x.neg y.imp)))))
	($make-cflonum ($fl/ num.rep denom)
		       ($fl/ num.imp denom)))))

;;; --------------------------------------------------------------------

  (define ($div-ratnum-fixnum x y)
    ;; x.num       x.num   1     x.num
    ;; ----- / y = ----- * - = ---------
    ;; x.den       x.den   y   x.den * y
    ;;
    (if ($fx= 1 y)
	x
      ($div-number-number ($ratnum-n x)
			  ($mul-number-fixnum ($ratnum-d x) y))))

  (define ($div-ratnum-bignum x y)
    ;; x.num       x.num   1     x.num
    ;; ----- / y = ----- * - = ---------
    ;; x.den       x.den   y   x.den * y
    ;;
    ($div-number-number ($ratnum-n x)
			($mul-number-bignum ($ratnum-d x) y)))

  (define ($div-ratnum-ratnum x y)
    ;; x.num   y.num   x.num   y.den
    ;; ----- / ----- = ----- * -----
    ;; x.den   y.den   x.den   y.num
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x))
	  (y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      (if (and (= x.num y.num)
	       (= x.den y.den))
	  1
	($div-number-number ($mul-number-number x.num y.den)
			    ($mul-number-number x.den y.num)))))

  (define ($div-ratnum-flonum x y)
    ;; x.num       x.num   1     x.num
    ;; ----- / y = ----- * - = ---------
    ;; x.den       x.den   y   x.den * y
    ;;
    ($div-number-flonum ($ratnum-n x)
			($mul-number-flonum ($ratnum-d x) y)))

  (define ($div-ratnum-compnum x y)
    ;; x.num                         x.num          1
    ;; ----- / (y.rep + i * y.imp) = ----- * ----------------- =
    ;; x.den                         x.den   y.rep + i * y.imp
    ;;
    ;;
    ;;   x.num           1           y.rep - i * y.imp
    ;; = ----- * ----------------- * ----------------- =
    ;;   x.den   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x.num   y.rep - i * y.imp   x.num * y.rep - i * x.num * y.imp
    ;; = ----- * ----------------- = ---------------------------------
    ;;   x.den   y.rep^2 + y.imp^2     x.den * (y.rep^2 + y.imp^2)
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x))
	  (y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom   ($mul-number-number x.den ($add-number-number (square y.rep)
								   (square y.imp))))
	    (num.rep ($mul-number-number x.num y.rep))
	    (num.imp ($mul-number-number ($neg-number x.num) y.imp)))
	($make-rectangular ($div-number-number num.rep denom)
			   ($div-number-number num.imp denom)))))

  (define ($div-ratnum-cflonum x y)
    ;; x.num                         x.num          1
    ;; ----- / (y.rep + i * y.imp) = ----- * ----------------- =
    ;; x.den                         x.den   y.rep + i * y.imp
    ;;
    ;;
    ;;   x.num           1           y.rep - i * y.imp
    ;; = ----- * ----------------- * ----------------- =
    ;;   x.den   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x.num   y.rep - i * y.imp   x.num * y.rep - i * x.num * y.imp
    ;; = ----- * ----------------- = ---------------------------------
    ;;   x.den   y.rep^2 + y.imp^2     x.den * (y.rep^2 + y.imp^2)
    ;;
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x))
	  (y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom   ($mul-number-flonum x.den ($fl+ ($flsquare y.rep)
						     ($flsquare y.imp))))
	    (num.rep ($mul-number-flonum x.num y.rep))
	    (num.imp ($mul-number-flonum ($neg-number x.num) y.imp)))
	($make-cflonum ($fl/ num.rep denom)
		       ($fl/ num.imp denom)))))

;;; --------------------------------------------------------------------

  (define ($div-flonum-fixnum x y)
    (if ($fx= 1 y)
	x
      ($fl/ x ($fixnum->flonum y))))

  (define ($div-flonum-bignum x y)
    ($fl/ x ($bignum->flonum y)))

  (define ($div-flonum-ratnum x y)
    ($fl/ x ($ratnum->flonum y)))

  (define ($div-flonum-flonum x y)
    ($fl/ x y))

  (define ($div-flonum-compnum x y)
    ;;         x                   x           y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x * y.rep - i * x * y.imp      x * y.rep              - x * y.imp
    ;; = ------------------------- = ----------------- + i * -----------------
    ;;       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2
    ;;
    (let ((y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom   ($add-number-number (square y.rep) (square y.imp)))
	    (num.rep ($mul-flonum-number x        y.rep))
	    (num.imp ($mul-flonum-number ($fl- x) y.imp)))
	($make-cflonum ($div-flonum-number num.rep denom)
		       ($div-flonum-number num.imp denom)))))

  (define ($div-flonum-cflonum x y)
    ;;         x                   x           y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x * y.rep - i * x * y.imp      x * y.rep              - x * y.imp
    ;; = ------------------------- = ----------------- + i * -----------------
    ;;       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2       y.rep^2 + x.rep^2
    ;;
    (let ((y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom   ($fl+ ($flsquare y.rep) ($flsquare y.imp)))
	    (num.rep ($fl* x y.rep))
	    (num.imp ($fl* ($fl- x) y.imp)))
	($make-cflonum ($fl/ num.rep denom)
		       ($fl/ num.imp denom)))))

;;; --------------------------------------------------------------------

  (define ($div-compnum-fixnum x y)
    ;; x.rep + i * x.imp   x.rep       x.imp
    ;; ----------------- = ----- + i * -----
    ;;        y              y           y
    ;;
    (if ($fx= 1 y)
	x
      (let ((x.rep ($compnum-real x))
	    (x.imp ($compnum-imag x)))
	($make-rectangular ($div-number-fixnum x.rep y)
			   ($div-number-fixnum x.imp y)))))

  (define ($div-compnum-bignum x y)
    ;; x.rep + i * x.imp   x.rep       x.imp
    ;; ----------------- = ----- + i * -----
    ;;        y              y           y
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular ($div-number-bignum x.rep y)
			 ($div-number-bignum x.imp y))))

  (define ($div-compnum-ratnum x y)
    ;;                       y.num                         y.den
    ;; (x.rep + i * x.imp) / ----- = (x.rep + i * x.imp) * ----- =
    ;;                       y.den                         y.num
    ;;
    ;;   x.rep * y.den       x.imp * y.den
    ;; = ------------- + i * -------------
    ;;       y.num               y.num
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x))
	  (y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($make-rectangular ($div-number-number ($mul-number-number x.rep y.den)
					     y.num)
			 ($div-number-number ($mul-number-number x.imp y.den)
					     y.num))))

  (define ($div-compnum-flonum x y)
    ;; x.rep + i * x.imp   x.rep       x.imp
    ;; ----------------- = ----- + i * -----
    ;;        y              y           y
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-cflonum ($div-number-flonum x.rep y)
		     ($div-number-flonum x.imp y))))

  (define ($div-compnum-compnum x y)
    ;; x.rep + i * x.imp   x.rep + i * x.imp   y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x.rep * y.rep + x.imp * y.imp       x.imp * y.rep - x.rep * y.imp
    ;; = ----------------------------- + i * -----------------------------
    ;;        y.rep^2 + y.imp^2                     y.rep^2 + y.imp^2
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x))
	  (y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom   ($add-number-number (square y.rep) (square y.imp)))
	    (num.rep ($add-number-number ($mul-number-number x.rep y.rep)
					 ($mul-number-number x.imp y.imp)))
	    (num.imp ($sub-number-number ($mul-number-number x.imp y.rep)
					 ($mul-number-number x.rep y.imp))))
	($make-rectangular ($div-number-number num.rep denom)
			   ($div-number-number num.imp denom)))))

  (define ($div-compnum-cflonum x y)
    ;; x.rep + i * x.imp   x.rep + i * x.imp   y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x.rep * y.rep + x.imp * y.imp       x.imp * y.rep - x.rep * y.imp
    ;; = ----------------------------- + i * -----------------------------
    ;;        y.rep^2 + y.imp^2                     y.rep^2 + y.imp^2
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x))
	  (y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom   ($fl+ ($flsquare y.rep) ($flsquare y.imp)))
	    (num.rep ($fl+ ($mul-number-flonum x.rep y.rep)
			   ($mul-number-flonum x.imp y.imp)))
	    (num.imp ($fl- ($mul-number-flonum x.imp y.rep)
			   ($mul-number-flonum x.rep y.imp))))
	($make-cflonum ($fl/ num.rep denom)
		       ($fl/ num.imp denom)))))

;;; --------------------------------------------------------------------

  (define ($div-cflonum-fixnum x y)
    ;; x.rep + i * x.imp   x.rep       x.imp
    ;; ----------------- = ----- + i * -----
    ;;        y              y           y
    ;;
    (if ($fx= 1 y)
	x
      (let ((x.rep ($cflonum-real x))
	    (x.imp ($cflonum-imag x)))
	($make-cflonum ($div-flonum-fixnum x.rep y)
		       ($div-flonum-fixnum x.imp y)))))

  (define ($div-cflonum-bignum x y)
    ;; x.rep + i * x.imp   x.rep       x.imp
    ;; ----------------- = ----- + i * -----
    ;;        y              y           y
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($div-flonum-bignum x.rep y)
		     ($div-flonum-bignum x.imp y))))

  (define ($div-cflonum-ratnum x y)
    ;;                       y.num                         y.den
    ;; (x.rep + i * x.imp) / ----- = (x.rep + i * x.imp) * ----- =
    ;;                       y.den                         y.num
    ;;
    ;;   x.rep * y.den       x.imp * y.den
    ;; = ------------- + i * -------------
    ;;       y.num               y.num
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x))
	  (y.num ($ratnum-n y))
	  (y.den ($ratnum-d y)))
      ($make-cflonum ($div-flonum-number ($mul-flonum-number x.rep y.den)
					 y.num)
		     ($div-flonum-number ($mul-flonum-number x.imp y.den)
					 y.num))))

  (define ($div-cflonum-flonum x y)
    ;; x.rep + i * x.imp   x.rep       x.imp
    ;; ----------------- = ----- + i * -----
    ;;        y              y           y
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($fl/ x.rep y)
		     ($fl/ x.imp y))))

  (define ($div-cflonum-compnum x y)
    ;; x.rep + i * x.imp   x.rep + i * x.imp   y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x.rep * y.rep + x.imp * y.imp       x.imp * y.rep - x.rep * y.imp
    ;; = ----------------------------- + i * -----------------------------
    ;;        y.rep^2 + y.imp^2                     y.rep^2 + y.imp^2
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x))
	  (y.rep ($compnum-real y))
	  (y.imp ($compnum-imag y)))
      (let ((denom   ($add-number-number (square y.rep) (square y.imp)))
	    (num.rep ($fl+ ($mul-flonum-number x.rep y.rep)
			   ($mul-flonum-number x.imp y.imp)))
	    (num.imp ($fl- ($mul-flonum-number x.imp y.rep)
			   ($mul-flonum-number x.rep y.imp))))
	($make-cflonum ($div-flonum-number num.rep denom)
		       ($div-flonum-number num.imp denom)))))

  (define ($div-cflonum-cflonum x y)
    ;; x.rep + i * x.imp   x.rep + i * x.imp   y.rep - i * y.imp
    ;; ----------------- = ----------------- * ----------------- =
    ;; y.rep + i * y.imp   y.rep + i * y.imp   y.rep - i * y.imp
    ;;
    ;;   x.rep * y.rep + x.imp * y.imp       x.imp * y.rep - x.rep * y.imp
    ;; = ----------------------------- + i * -----------------------------
    ;;        y.rep^2 + y.imp^2                     y.rep^2 + y.imp^2
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x))
	  (y.rep ($cflonum-real y))
	  (y.imp ($cflonum-imag y)))
      (let ((denom   ($fl+ ($flsquare y.rep) ($flsquare y.imp)))
	    (num.rep ($fl+ ($fl* x.rep y.rep)
			   ($fl* x.imp y.imp)))
	    (num.imp ($fl- ($fl* x.imp y.rep)
			   ($fl* x.rep y.imp))))
	($make-rectangular ($fl/ num.rep denom)
			   ($fl/ num.imp denom)))
      ;;
      ;;Does  it make  sense to  introduce branches  for special  cases?
      ;;Probably not: there  are so many checks to  do...  (Marco Maggi;
      ;;Fri Nov 30, 2012)
      ;;
      ;; (cond ((or (flnan? x.rep)
      ;;            (flnan? x.imp)
      ;;            (flnan? y.rep)
      ;;            (flnan? y.imp))
      ;;        +nan.0+nan.0i)
      ;;       ((and ($fl= x.rep y.rep)
      ;;             ($fl= x.imp y.imp)
      ;;             (not (flinfinite? x.rep))
      ;;             (not (flinfinite? x.imp))
      ;;             (not (flinfinite? y.rep))
      ;;             (not (flinfinite? y.imp))
      ;;             (not (and ($flzero? y.rep)
      ;;                       ($flzero? y.imp))))
      ;;        1.0+0.0i)
      ;;       (else
      ;;        (let ((denom   ($fl+ ($flsquare y.rep) ($flsquare y.imp)))
      ;;              (num.rep ($fl+ ($fl* x.rep y.rep)
      ;;                             ($fl* x.imp y.imp)))
      ;;              (num.imp ($fl- ($fl* x.imp y.rep)
      ;;                             ($fl* x.rep y.imp))))
      ;;          ($make-rectangular ($fl/ num.rep denom)
      ;;                             ($fl/ num.imp denom)))))
      ))

  #| end of module: $div-number-number |# )


(module (gcd
	 $gcd-number		$gcd-number-number
	 $gcd-fixnum-number	$gcd-bignum-number	$gcd-flonum-number
	 $gcd-number-fixnum	$gcd-number-bignum	$gcd-number-flonum
	 $gcd-fixnum-fixnum	$gcd-fixnum-bignum	$gcd-fixnum-flonum
	 $gcd-bignum-fixnum	$gcd-bignum-bignum	$gcd-bignum-flonum
	 $gcd-flonum-fixnum	$gcd-flonum-bignum	$gcd-flonum-flonum)
  (define who 'gcd)

  (define gcd
    (case-lambda
     ((x y)
      ($gcd-number-number x y))

     ((x)
      ($gcd-number x))

     (() 0)

     ((x y z . ls)
      (let loop ((g  ($gcd-number-number ($gcd-number-number x y) z))
		 (ls ls))
	(if (null? ls)
	    g
	  (loop ($gcd-number-number g ($car ls))
		($cdr ls)))))
     ))

;;; --------------------------------------------------------------------

  (define ($gcd-number x)
    (cond-inexact-integer-operand x
      ((fixnum?)
       (if ($fxnonnegative? x)
	   x
	 (- x)))
      ((bignum?)
       (if ($bignum-positive? x)
	   x
	 (- x)))
      ((flonum?)
       (inexact ($gcd-number ($flonum->exact x))))
      (else
       (%error-not-integer x))))

  (define ($gcd-number-number x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($gcd-fixnum-number x y))
      ((bignum?)	($gcd-bignum-number x y))
      ((flonum?)	($gcd-flonum-number x y))
      (else
       (%error-not-integer x))))

;;; --------------------------------------------------------------------

  (define ($gcd-fixnum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($gcd-fixnum-fixnum x y))
      ((bignum?)	($gcd-fixnum-bignum x y))
      ((flonum?)	($gcd-fixnum-flonum x y))
      (else
       (%error-not-integer y))))

  (define ($gcd-bignum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($gcd-bignum-fixnum x y))
      ((bignum?)	($gcd-bignum-bignum x y))
      ((flonum?)	($gcd-bignum-flonum x y))
      (else
       (%error-not-integer y))))

  (define ($gcd-flonum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($gcd-flonum-fixnum x y))
      ((bignum?)	($gcd-flonum-bignum x y))
      ((flonum?)	($gcd-flonum-flonum x y))
      (else
       (%error-not-integer y))))

;;; --------------------------------------------------------------------

  (define ($gcd-number-fixnum x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($gcd-fixnum-fixnum x y))
      ((bignum?)	($gcd-bignum-fixnum x y))
      ((flonum?)	($gcd-flonum-fixnum x y))
      (else
       (%error-not-integer x))))

  (define ($gcd-number-bignum x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($gcd-fixnum-bignum x y))
      ((bignum?)	($gcd-bignum-bignum x y))
      ((flonum?)	($gcd-flonum-bignum x y))
      (else
       (%error-not-integer x))))

  (define ($gcd-number-flonum x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($gcd-fixnum-flonum x y))
      ((bignum?)	($gcd-bignum-flonum x y))
      ((flonum?)	($gcd-flonum-flonum x y))
      (else
       (%error-not-integer x))))

;;; --------------------------------------------------------------------

  (define ($gcd-fixnum-fixnum x y)
    ;;The GCD between two fixnums is *not* always a fixnum: when X and Y
    ;;are both (least-fixnum) the GCD is the bignum (- (least-fixnum)).
    ;;
    (let ((x.abs ($abs-fixnum x))
	  (y.abs ($abs-fixnum y)))
      (if (fixnum? x.abs)
	  (if (fixnum? y.abs)
	      (cond (($fx> x.abs y.abs)
		     (%greatest-common-divisor x.abs y.abs))
		    (($fx< x.abs y.abs)
		     (%greatest-common-divisor y.abs x.abs))
		    (else
		     x.abs))
	    ;;Here X.ABS is  a non-negative fixnum, Y.ABS  is a positive
	    ;;bignum, so X.ABS < Y.ABS.
	    (%greatest-common-divisor y.abs x.abs))
	(if (fixnum? y.abs)
	    ;;Here X.ABS is  a positive bignum, Y.ABS  is a non-negative
	    ;;fixnum, so X.ABS > Y.ABS.
	    (%greatest-common-divisor x.abs y.abs)
	  ;;Here both X.ABS and Y.ABS are positive bignums.
	  (cond ((bnbn> x.abs y.abs)
		 (%greatest-common-divisor x.abs y.abs))
		((bnbn< x.abs y.abs)
		 (%greatest-common-divisor y.abs x.abs))
		(else
		 x.abs))))))

  (define ($gcd-fixnum-bignum x y)
    ;;The GCD between a fixnum and a  bignum is a fixnum or bignum.  The
    ;;returned value may be a bignum only when X is (least-fixnum).
    ;;
    (let ((x.abs ($abs-fixnum x))
	  (y.abs ($abs-bignum y)))
      (if (fixnum? x.abs)
	  ;;Here X.ABS  is a  non-negative fixnum,  Y.ABS is  a positive
	  ;;bignum, so X.ABS < Y.ABS.
	  (%greatest-common-divisor y.abs x.abs)
	;;Here both X.ABS and Y.ABS are positive bignums.
	(cond ((bnbn> x.abs y.abs)
	       (%greatest-common-divisor x.abs y.abs))
	      ((bnbn< x.abs y.abs)
	       (%greatest-common-divisor y.abs x.abs))
	      (else
	       x.abs)))))

  (define ($gcd-fixnum-flonum x y)
    (let ((y.exact ($flonum->exact y)))
      (cond-exact-integer-operand y.exact
	((fixnum?)	(inexact ($gcd-fixnum-fixnum x y.exact)))
	((bignum?)	(inexact ($gcd-fixnum-bignum x y.exact)))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define ($gcd-bignum-fixnum x y)
    (let ((x.abs ($abs-bignum x))
	  (y.abs ($abs-fixnum y)))
      (if (fixnum? y.abs)
	  ;;Here X.ABS  is a  positive bignum,  Y.ABS is  a non-negative
	  ;;fixnum, so X.ABS > Y.ABS.
	  (%greatest-common-divisor x.abs y.abs)
	;;Here both X.ABS and Y.ABS are positive bignums.
	(cond ((bnbn> x.abs y.abs)
	       (%greatest-common-divisor x.abs y.abs))
	      ((bnbn< x.abs y.abs)
	       (%greatest-common-divisor y.abs x.abs))
	      (else
	       x.abs)))))

  (define ($gcd-bignum-bignum x y)
    (let ((x.abs ($abs-bignum x))
	  (y.abs ($abs-bignum y)))
      ;;Here both X.ABS and Y.ABS are positive bignums.
      (cond ((bnbn> x.abs y.abs)
	     (%greatest-common-divisor x.abs y.abs))
	    ((bnbn< x.abs y.abs)
	     (%greatest-common-divisor y.abs x.abs))
	    (else
	     x.abs))))

  (define ($gcd-bignum-flonum x y)
    (let ((y.exact ($flonum->exact y)))
      (cond-exact-integer-operand y.exact
	((fixnum?)	(inexact ($gcd-bignum-fixnum x y.exact)))
	((bignum?)	(inexact ($gcd-bignum-bignum x y.exact)))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define ($gcd-flonum-fixnum x y)
    (let ((x.exact ($flonum->exact x)))
      (cond-exact-integer-operand x.exact
	((fixnum?)	(inexact ($gcd-fixnum-fixnum x.exact y)))
	((bignum?)	(inexact ($gcd-bignum-fixnum x.exact y)))
	(else
	 (%error-not-integer x)))))

  (define ($gcd-flonum-bignum x y)
    (let ((x.exact ($flonum->exact x)))
      (cond-exact-integer-operand x.exact
	((fixnum?)	(inexact ($gcd-fixnum-bignum x.exact y)))
	((bignum?)	(inexact ($gcd-bignum-bignum x.exact y)))
	(else
	 (%error-not-integer x)))))

  (define ($gcd-flonum-flonum x y)
    (let ((x.exact ($flonum->exact x)))
      (cond-exact-integer-operand x.exact
	((fixnum?)	($gcd-fixnum-flonum x.exact y))
	((bignum?)	($gcd-bignum-flonum x.exact y))
	(else
	 (%error-not-integer x)))))

;;; --------------------------------------------------------------------

  ;;This is the old generic implementation.
  ;;
  ;; (define ($gcd x y)
  ;;   (let ((x.abs (abs x))
  ;;         (y.abs (abs y)))
  ;;     (cond ((> x.abs y.abs)
  ;;            (%greatest-common-divisor x.abs y.abs))
  ;;           ((< x.abs y.abs)
  ;;            (%greatest-common-divisor y.abs x.abs))
  ;;           (else
  ;;            x.abs))))

  (define (%greatest-common-divisor x y)
    ;;Both  X and  Y must  be non-negative  exact integers.   X must  be
    ;;greater than Y.
    ;;
    (if (zero? y)
	x
      (%greatest-common-divisor y (remainder x y))))

;;; --------------------------------------------------------------------

  (define (%error-not-integer x)
    (assertion-violation who "expected exact or inexact integer as argument" x))

  #| end of module |# )


(module (lcm
	 $lcm-number		$lcm-number-number
	 $lcm-fixnum-number	$lcm-bignum-number	$lcm-flonum-number
	 $lcm-number-fixnum	$lcm-number-bignum	$lcm-number-flonum
	 $lcm-fixnum-fixnum	$lcm-fixnum-bignum	$lcm-fixnum-flonum
	 $lcm-bignum-fixnum	$lcm-bignum-bignum	$lcm-bignum-flonum
	 $lcm-flonum-fixnum	$lcm-flonum-bignum	$lcm-flonum-flonum)
  (define who 'lcm)

  (define lcm
    (case-lambda
     ((x y)
      ($lcm-number-number x y))
     ((x)
      ($lcm-number x))
     (() 1)
     ((x y z . ls)
      ;;FIXME Incorrect for multiple roundings.  (Abdulaziz Ghuloum)
      (let loop ((g  ($lcm-number-number ($lcm-number-number x y) z))
		 (ls ls))
	(if (null? ls)
	    g
	  (loop ($lcm-number-number g ($car ls))
		($cdr ls)))))
     ))

;;; --------------------------------------------------------------------

  (define ($lcm-number x)
    (cond-inexact-integer-operand x
      ((fixnum?)
       (if ($fxnonnegative? x)
	   x
	 (- x)))
      ((bignum?)
       (if ($bignum-positive? x)
	   x
	 (- x)))
      ((flonum?)
       (inexact ($lcm-number ($flonum->exact x))))
      (else
       (%error-not-exact-integer x))))

  (define ($lcm-number-number x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($lcm-fixnum-number x y))
      ((bignum?)	($lcm-bignum-number x y))
      ((flonum?)	($lcm-flonum-number x y))
      (else
       (%error-not-integer x))))

;;; --------------------------------------------------------------------

  (define ($lcm-fixnum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($lcm-fixnum-fixnum x y))
      ((bignum?)	($lcm-fixnum-bignum x y))
      ((flonum?)	($lcm-fixnum-flonum x y))
      (else
       (%error-not-integer y))))

  (define ($lcm-bignum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($lcm-bignum-fixnum x y))
      ((bignum?)	($lcm-bignum-bignum x y))
      ((flonum?)	($lcm-bignum-flonum x y))
      (else
       (%error-not-integer y))))

  (define ($lcm-flonum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($lcm-flonum-fixnum x y))
      ((bignum?)	($lcm-flonum-bignum x y))
      ((flonum?)	($lcm-flonum-flonum x y))
      (else
       (%error-not-integer y))))

;;; --------------------------------------------------------------------

  (define ($lcm-number-fixnum x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($lcm-fixnum-fixnum x y))
      ((bignum?)	($lcm-bignum-fixnum x y))
      ((flonum?)	($lcm-flonum-fixnum x y))
      (else
       (%error-not-integer x))))

  (define ($lcm-number-bignum x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($lcm-fixnum-bignum x y))
      ((bignum?)	($lcm-bignum-bignum x y))
      ((flonum?)	($lcm-flonum-bignum x y))
      (else
       (%error-not-integer x))))

  (define ($lcm-number-flonum x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($lcm-fixnum-flonum x y))
      ((bignum?)	($lcm-bignum-flonum x y))
      ((flonum?)	($lcm-flonum-flonum x y))
      (else
       (%error-not-integer x))))

;;; --------------------------------------------------------------------

;;;NOTE After  negating fixnums and  bignums X and Y  we do not  know if
;;;X.ABS and Y.ABS are still fixnums or still bignums:
;;;
;;;   ($neg-fixnum (least-fixnum))	=> bignum
;;;   ($neg-bignum (- (least-fixnum)))	=> fixnum
;;;

  (define ($lcm-fixnum-fixnum x y)
    (let ((x.abs ($abs-fixnum x))
	  (y.abs ($abs-fixnum y)))
      (if (or (eq? x.abs 0)
	      (eq? y.abs 0))
	  0
	($least-common-multiple x.abs y.abs))))

  (define ($lcm-fixnum-bignum x y)
    (let ((x.abs ($abs-fixnum x)))
      (if (eq? x.abs 0)
	  0
	($least-common-multiple x.abs ($abs-bignum y)))))

  (define ($lcm-fixnum-flonum x y)
    (let ((y.exact ($flonum->exact y)))
      (cond-exact-integer-operand y.exact
	((fixnum?)	(inexact ($lcm-fixnum-fixnum x y.exact)))
	((bignum?)	(inexact ($lcm-fixnum-bignum x y.exact)))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define ($lcm-bignum-fixnum x y)
    (let ((y.abs ($abs-fixnum y)))
      (if (eq? y.abs 0)
	  0
	($least-common-multiple ($abs-bignum x) y.abs))))

  (define ($lcm-bignum-bignum x y)
    ($least-common-multiple ($abs-bignum x) ($abs-bignum y)))

  (define ($lcm-bignum-flonum x y)
    (let ((y.exact ($flonum->exact y)))
      (cond-exact-integer-operand y.exact
	((fixnum?)	(inexact ($lcm-bignum-fixnum x y.exact)))
	((bignum?)	(inexact ($lcm-bignum-bignum x y.exact)))
	(else
	 (%error-not-integer y)))))

;;; --------------------------------------------------------------------

  (define ($lcm-flonum-fixnum x y)
    (let ((x.exact ($flonum->exact x)))
      (cond-exact-integer-operand x.exact
	((fixnum?)	(inexact ($lcm-fixnum-fixnum x.exact y)))
	((bignum?)	(inexact ($lcm-bignum-fixnum x.exact y)))
	(else
	 (%error-not-integer x)))))

  (define ($lcm-flonum-bignum x y)
    (let ((x.exact ($flonum->exact x)))
      (cond-exact-integer-operand x.exact
	((fixnum?)	(inexact ($lcm-fixnum-bignum x.exact y)))
	((bignum?)	(inexact ($lcm-bignum-bignum x.exact y)))
	(else
	 (%error-not-integer x)))))

  (define ($lcm-flonum-flonum x y)
    (let ((x.exact ($flonum->exact x)))
      (cond-exact-integer-operand x.exact
	((fixnum?)	($lcm-fixnum-flonum x.exact y))
	((bignum?)	($lcm-bignum-flonum x.exact y))
	(else
	 (%error-not-integer x)))))

;;; --------------------------------------------------------------------

  (define ($least-common-multiple x.abs y.abs)
    ;;Both X and Y must be non-negative exact integers.
    ;;
    (let ((g ($gcd-number-number x.abs y.abs)))
      ($mul-number-number y.abs (quotient x.abs g))))

;;; --------------------------------------------------------------------

  (define (%error-not-integer x)
    (assertion-violation who "expected integer as argument" x))

  (define (%error-not-exact-integer x)
    (assertion-violation who "expected exact integer as argument" x))

  #| end of module |# )


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
  ;;Note that considering:
  ;;
  ;;   (quotient+remainder X Y)
  ;;
  ;;according to  R6RS the  function MOD  always returns  a non-negative
  ;;integer and:
  ;;
  ;;   (define (sign n)
  ;;     (cond ((negative? n) -1)
  ;;           ((positive? n) 1)
  ;;           (else 0)))
  ;;
  ;;   (define (quotient n1 n2)
  ;;     (* (sign n1) (sign n2) (div (abs n1) (abs n2))))
  ;;
  ;;   (define (remainder n1 n2)
  ;;     (* (sign n1) (mod (abs n1) (abs n2))))
  ;;
  ;;   (define (modulo n1 n2)
  ;;     (* (sign n2) (mod (* (sign n2) n1) (abs n2))))
  ;;
  ;;so we have:
  ;;
  ;;   sign(quotient)  = sign(X) * sign(Y)
  ;;   sign(remainder) = sign(X)
  ;;
  (define who 'quotient+remainder)

  (define (quotient+remainder x y)
    (if (zero? y) ;both exact and inexact
	(assertion-violation who "second argument must be non-zero")
      (cond-inexact-integer-operand x
	((fixnum?)	($quotient+remainder-fixnum-number x y))
	((bignum?)	($quotient+remainder-bignum-number x y))
	((flonum?)	($quotient+remainder-flonum-number x y))
	(else
	 (%error-not-integer x)))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-fixnum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($quotient+remainder-fixnum-fixnum x y))
      ((bignum?)	($quotient+remainder-fixnum-bignum x y))
      ((flonum?)	($quotient+remainder-fixnum-flonum x y))
      (else
       (%error-not-integer y))))

  (define ($quotient+remainder-bignum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($quotient+remainder-bignum-fixnum x y))
      ((bignum?)	($quotient+remainder-bignum-bignum x y))
      ((flonum?)	($quotient+remainder-bignum-flonum x y))
      (else
       (%error-not-integer y))))

  (define ($quotient+remainder-flonum-number x y)
    (cond-inexact-integer-operand y
      ((fixnum?)	($quotient+remainder-flonum-fixnum x y))
      ((bignum?)	($quotient+remainder-flonum-bignum x y))
      ((flonum?)	($quotient+remainder-flonum-flonum x y))
      (else
       (%error-not-integer x))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-number-fixnum x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($quotient+remainder-fixnum-fixnum x y))
      ((bignum?)	($quotient+remainder-bignum-fixnum x y))
      ((flonum?)	($quotient+remainder-flonum-fixnum x y))
      (else
       (%error-not-integer x))))

  (define ($quotient+remainder-number-bignum x y)
    (cond-inexact-integer-operand x
      ((fixnum?)	($quotient+remainder-fixnum-bignum x y))
      ((bignum?)	($quotient+remainder-bignum-bignum x y))
      ((flonum?)	($quotient+remainder-flonum-bignum x y))
      (else
       (%error-not-integer x))))

  (define ($quotient+remainder-number-flonum x y)
    (let ((y.exact ($flonum->exact y)))
      (receive (q r)
	  (cond-exact-integer-operand y.exact
	    ((fixnum?)
	     ($quotient+remainder-number-fixnum x y.exact))
	    ((bignum?)
	     ($quotient+remainder-number-bignum x y.exact))
	    (else
	     (%error-not-integer y)))
	(values (if (inexact? q) q (inexact q))
		(if (inexact? r) r (inexact r))))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-fixnum-fixnum x y)
    (cond (($fx= y -1)
	   (values ($neg-fixnum x) 0))
	  (($fx= x (least-fixnum))
	   ;;Avoid overflow by avoiding $fxquotient in this case.
	   (receive (r q)
	       ($quotient+remainder-bignum-fixnum (- (least-fixnum)) y)
	     (values ($neg-number r) ($neg-number q))))
	  (else
	   (values ($fxquotient x y) ($fxremainder x y)))))

  (define ($quotient+remainder-fixnum-bignum x y)
    (if ($fx= x (least-fixnum))
	(receive (r q)
	    ($quotient+remainder-bignum-bignum (- (least-fixnum)) y)
	  (values ($neg-number r) ($neg-number q)))
      (values 0 x)))

  (define ($quotient+remainder-fixnum-flonum x y)
    (receive (q r)
	(let ((y.exact ($flonum->exact y)))
	  (cond-exact-integer-operand y.exact
	    ((fixnum?)	($quotient+remainder-fixnum-fixnum x y.exact))
	    ((bignum?)	($quotient+remainder-fixnum-bignum x y.exact))
	    (else
	     (%error-not-integer y))))
      (values (if (zero? q)
		  (if (or ($fxpositive? x)
			  ($fxzero? x))
		      (if ($flpositive? y) +0.0 -0.0)
		    (if ($flpositive? y) -0.0 +0.0))
		(inexact q))
	      (if (zero? r)
		  (if (or ($fxpositive? x)
			  ($fxzero? x))
		      +0.0
		    -0.0)
		(inexact r)))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-bignum-fixnum x y)
    (let ((p (foreign-call "ikrt_bnfxdivrem" x y)))
      (values ($car p) ($cdr p))))

  (define ($quotient+remainder-bignum-bignum x y)
    (let ((p (foreign-call "ikrt_bnbndivrem" x y)))
      (values ($car p) ($cdr p))))

  (define ($quotient+remainder-bignum-flonum x y)
    (receive (q r)
	(let ((y.exact ($flonum->exact y)))
	  (cond-exact-integer-operand y.exact
	    ((fixnum?)	($quotient+remainder-bignum-fixnum x y.exact))
	    ((bignum?)	($quotient+remainder-bignum-bignum x y.exact))
	    (else
	     (%error-not-integer y))))
      (values (if (zero? q)
		  (if ($bignum-positive? x)
		      (if ($flpositive? y) +0.0 -0.0)
		    (if ($flpositive? y) -0.0 +0.0))
		(inexact q))
	      (if (zero? r)
		  (if ($bignum-positive? x) 0.0 -0.0)
		(inexact r)))))

;;; --------------------------------------------------------------------

  (define ($quotient+remainder-flonum-fixnum x y)
    (cond (($flzero?/positive x)
	   (values (if ($fxpositive? y) +0.0 -0.0)
		   +0.0))
	  (($flzero?/negative x)
	   (values (if ($fxpositive? y) -0.0 +0.0)
		   -0.0))
	  (else
	   (receive (q r)
	       (let ((x.exact ($flonum->exact x)))
		 (cond-exact-integer-operand x.exact
		   ((fixnum?)	($quotient+remainder-fixnum-fixnum x.exact y))
		   ((bignum?)	($quotient+remainder-bignum-fixnum x.exact y))
		   (else
		    (%error-not-integer x))))
	     (values (if (zero? q)
			 (if ($flpositive? x)
			     (if ($fxpositive? y) +0.0 -0.0)
			   (if ($fxpositive? y) -0.0 +0.0))
		       (inexact q))
		     (if (zero? r)
			 (if ($flpositive? x) +0.0 -0.0)
		       (inexact r)))))))

  (define ($quotient+remainder-flonum-bignum x y)
    (cond (($flzero?/positive x)
	   (values (if ($bignum-positive? y) +0.0 -0.0)
		   +0.0))
	  (($flzero?/negative x)
	   (values (if ($bignum-positive? y) -0.0 +0.0)
		   -0.0))
	  (else
	   (receive (q r)
	       (let ((x.exact ($flonum->exact x)))
		 (cond-exact-integer-operand x.exact
		   ((fixnum?)	($quotient+remainder-fixnum-bignum x.exact y))
		   ((bignum?)	($quotient+remainder-bignum-bignum x.exact y))
		   (else
		    (%error-not-integer x))))
	     (values (if (zero? q)
			 (if ($flpositive? x)
			     (if ($bignum-positive? y) +0.0 -0.0)
			   (if ($bignum-positive? y) -0.0 +0.0))
		       (inexact q))
		     (if (zero? r)
			 (if ($flpositive? x) +0.0 -0.0)
		       (inexact r)))))))

  (define ($quotient+remainder-flonum-flonum x y)
    (cond (($flzero?/positive x)
	   (values (if ($flpositive? y) +0.0 -0.0)
		   +0.0))
	  (($flzero?/negative x)
	   (values (if ($flpositive? y) -0.0 +0.0)
		   -0.0))
	  (else
	   (let ((x.exact ($flonum->exact x)))
	     (cond-exact-integer-operand x.exact
	       ((fixnum?)	($quotient+remainder-fixnum-flonum x.exact y))
	       ((bignum?)	($quotient+remainder-bignum-flonum x.exact y))
	       (else
		(%error-not-integer x)))))))

;;; --------------------------------------------------------------------

  (define (%error-not-integer x)
    (assertion-violation who "expected exact or inexact integer as argument" x))

  #| end of module: quotient+remainder |# )


(module (quotient
	 remainder
	 $quotient-fixnum-number	$remainder-fixnum-number
	 $quotient-number-fixnum	$remainder-number-fixnum
	 $quotient-bignum-number	$remainder-bignum-number
	 $quotient-number-bignum	$remainder-number-bignum
	 $quotient-flonum-number	$remainder-flonum-number
	 $quotient-number-flonum	$remainder-number-flonum
	 $quotient-fixnum-fixnum	$remainder-fixnum-fixnum
	 $quotient-fixnum-bignum	$remainder-fixnum-bignum
	 $quotient-fixnum-flonum	$remainder-fixnum-flonum
	 $quotient-bignum-fixnum	$remainder-bignum-fixnum
	 $quotient-bignum-bignum	$remainder-bignum-bignum
	 $quotient-bignum-flonum	$remainder-bignum-flonum
	 $quotient-flonum-fixnum	$remainder-flonum-fixnum
	 $quotient-flonum-bignum	$remainder-flonum-bignum
	 $quotient-flonum-flonum	$remainder-flonum-flonum)

  (define-syntax define-quotient+remainder
    (syntax-rules ()
      ((_ ?quotient ?remainder ?quotient+remainder)
       (begin
	 (define (?quotient x y)
	   (receive (q r)
	       (?quotient+remainder x y)
	     q))
	 (define (?remainder x y)
	   (receive (q r)
	       (?quotient+remainder x y)
	     r))))))

  (define-quotient+remainder quotient remainder
    quotient+remainder)

;;; --------------------------------------------------------------------

  (define-quotient+remainder $quotient-fixnum-number $remainder-fixnum-number
    $quotient+remainder-fixnum-number)

  (define-quotient+remainder $quotient-number-fixnum $remainder-number-fixnum
    $quotient+remainder-number-fixnum)

;;; --------------------------------------------------------------------

  (define-quotient+remainder $quotient-bignum-number $remainder-bignum-number
    $quotient+remainder-bignum-number)

  (define-quotient+remainder $quotient-number-bignum $remainder-number-bignum
    $quotient+remainder-number-bignum)

;;; --------------------------------------------------------------------

  (define-quotient+remainder $quotient-flonum-number $remainder-flonum-number
    $quotient+remainder-flonum-number)

  (define-quotient+remainder $quotient-number-flonum $remainder-number-flonum
    $quotient+remainder-number-flonum)

;;; --------------------------------------------------------------------

  (define-quotient+remainder $quotient-fixnum-fixnum $remainder-fixnum-fixnum
    $quotient+remainder-fixnum-fixnum)

  (define-quotient+remainder $quotient-fixnum-bignum $remainder-fixnum-bignum
    $quotient+remainder-fixnum-bignum)

  (define-quotient+remainder $quotient-fixnum-flonum $remainder-fixnum-flonum
    $quotient+remainder-fixnum-flonum)

;;; --------------------------------------------------------------------

  (define-quotient+remainder $quotient-bignum-fixnum $remainder-bignum-fixnum
    $quotient+remainder-bignum-fixnum)

  (define-quotient+remainder $quotient-bignum-bignum $remainder-bignum-bignum
    $quotient+remainder-bignum-bignum)

  (define-quotient+remainder $quotient-bignum-flonum $remainder-bignum-flonum
    $quotient+remainder-bignum-flonum)

;;; --------------------------------------------------------------------

  (define-quotient+remainder $quotient-flonum-fixnum $remainder-flonum-fixnum
    $quotient+remainder-flonum-fixnum)

  (define-quotient+remainder $quotient-flonum-bignum $remainder-flonum-bignum
    $quotient+remainder-flonum-bignum)

  (define-quotient+remainder $quotient-flonum-flonum $remainder-flonum-flonum
    $quotient+remainder-flonum-flonum)

  #| end of module: quotient |# )


(module (modulo
	 $modulo-fixnum-number	$modulo-bignum-number	$modulo-flonum-number
	 $modulo-number-fixnum	$modulo-number-bignum	$modulo-number-flonum
	 $modulo-fixnum-fixnum	$modulo-fixnum-bignum	$modulo-fixnum-flonum
	 $modulo-bignum-fixnum	$modulo-bignum-bignum	$modulo-bignum-flonum
	 $modulo-flonum-fixnum	$modulo-flonum-bignum	$modulo-flonum-flonum)
  ;;Note that considering X/Y according to R6RS:
  ;;
  ;;   (define (sign n)
  ;;     (cond ((negative? n) -1)
  ;;           ((positive? n) 1)
  ;;           (else 0)))
  ;;
  ;;   (define (quotient n1 n2)
  ;;     (* (sign n1) (sign n2) (div (abs n1) (abs n2))))
  ;;
  ;;   (define (remainder n1 n2)
  ;;     (* (sign n1) (mod (abs n1) (abs n2))))
  ;;
  ;;   (define (modulo n1 n2)
  ;;     (* (sign n2) (mod (* (sign n2) n1) (abs n2))))
  ;;
  ;;the function  MOD always returns positive results, so we have:
  ;;
  ;;   sign(quotient)  = sign(X) * sign(Y)
  ;;   sign(remainder) = sign(X)
  ;;   sign(modulo)    = sign(Y)
  ;;
  ;;when computing the modulo we want Y to be non-zero.
  ;;
  (define who 'modulo)

  (define (modulo n m)
    (cond-inexact-integer-operand n
      ((fixnum?)	($modulo-fixnum-number n m))
      ((bignum?)	($modulo-bignum-number n m))
      ((flonum?)	($modulo-flonum-number n m))
      (else
       (%error-not-integer n))))

;;; --------------------------------------------------------------------

  (define ($modulo-fixnum-number n m)
    (cond-inexact-integer-operand m
      ((fixnum?)	($modulo-fixnum-fixnum n m))
      ((bignum?)	($modulo-fixnum-bignum n m))
      ((flonum?)	($modulo-fixnum-flonum n m))
      (else
       (%error-not-integer m))))

  (define ($modulo-bignum-number n m)
    (cond-inexact-integer-operand m
      ((fixnum?)	($modulo-bignum-fixnum n m))
      ((bignum?)	($modulo-bignum-bignum n m))
      ((flonum?)	($modulo-bignum-flonum n m))
      (else
       (%error-not-integer m))))

  (define ($modulo-flonum-number n m)
    (cond-inexact-integer-operand m
      ((fixnum?)	($modulo-flonum-fixnum n m))
      ((bignum?)	($modulo-flonum-bignum n m))
      ((flonum?)	($modulo-flonum-flonum n m))
      (else
       (%error-not-integer m))))

;;; --------------------------------------------------------------------

  (define ($modulo-number-fixnum n m)
    (cond-inexact-integer-operand n
      ((fixnum?)	($modulo-fixnum-fixnum n m))
      ((bignum?)	($modulo-bignum-fixnum n m))
      ((flonum?)	($modulo-flonum-fixnum n m))
      (else
       (%error-not-integer n))))

  (define ($modulo-number-bignum n m)
    (cond-inexact-integer-operand n
      ((fixnum?)	($modulo-fixnum-bignum n m))
      ((bignum?)	($modulo-bignum-bignum n m))
      ((flonum?)	($modulo-flonum-bignum n m))
      (else
       (%error-not-integer n))))

  (define ($modulo-number-flonum n m)
    (let ((m.exact ($flonum->exact m)))
      (cond-exact-integer-operand m.exact
	((fixnum?)	(inexact ($modulo-number-fixnum n m.exact)))
	((bignum?)	(inexact ($modulo-number-bignum n m.exact)))
	(else
	 (%error-not-integer m)))))

;;; --------------------------------------------------------------------

  (define ($modulo-fixnum-fixnum n m)
    (if ($fxzero? m)
	(assertion-violation who "division by zero" n m)
      ($fxmodulo n m)))

  (define ($modulo-fixnum-bignum n m)
    ;;When both N  and M are non-negative  or both N and  M are positive
    ;;return N; else return N + M.
    ;;
    (cond (($fxzero? n)
	   0)
	  (($fxnegative? n)
	   (if ($bignum-positive? m)
	       ;;N negative, M positive
	       (foreign-call "ikrt_fxbnplus" n m)
	     ;;N negative, M negative
	     n))
	  (else ;N is positive here
	   (if ($bignum-positive? m)
	       ;;N positive, M positive
	       n
	     ;;N positive, M negative
	     (foreign-call "ikrt_fxbnplus" n m)))))

  (define ($modulo-fixnum-flonum n m)
    (let* ((v ($flonum->exact m))
	   (R (cond-exact-integer-operand v
		((fixnum?)	(inexact ($modulo-fixnum-fixnum n v)))
		((bignum?)	(inexact ($modulo-fixnum-bignum n v)))
		(else
		 (%error-not-integer m)))))
      ;;The return value must have the sign of M.
      (if (and (or ($flzero?/negative m)
		   ($fl< m -0.0))
	       (or ($flzero?/positive R)
		   ($fl> R +0.0)))
	  ($fl- R)
	R)))

;;; --------------------------------------------------------------------

  (define ($modulo-bignum-fixnum n m)
    (if ($fxzero? m)
	(assertion-violation who "division by zero" n m)
      (foreign-call "ikrt_bnfx_modulo" n m)))

  (define ($modulo-bignum-bignum n m)
    ;;If both N and M are positive or both N and M are negative do:
    ;;
    ;;   ($remainder-bignum-bignum n m)
    ;;
    ;;else do:
    ;;
    ;;   ($add-bignum-number m ($remainder-bignum-bignum n m))
    ;;
    (let ((rem ($remainder-bignum-bignum n m)))
      (cond (($bignum-positive? n)
	     (cond (($bignum-positive? m)
		    rem)				;;;both positive
		   ((zero? rem)
		    0)
		   (else
		    ($add-bignum-number m rem))))	;;;N positive, M negative
	    (($bignum-positive? m)
	     (if (zero? rem)
		 0
	       ($add-bignum-number m rem)))		;;;N negative, M positive
	    (else
	     rem))))					;;;both negative

  (define ($modulo-bignum-flonum n m)
    (let* ((m.exact ($flonum->exact m))
	   (R       (cond-exact-integer-operand m.exact
		      ((fixnum?)	(inexact ($modulo-bignum-fixnum n m.exact)))
		      ((bignum?)	(inexact ($modulo-bignum-bignum n m.exact)))
		      (else
		       (%error-not-integer m)))))
      ;;The return value must have the sign of M.
      (if (and (or ($flzero?/negative m)
		   ($fl< m -0.0))
	       (or ($flzero?/positive R)
		   ($fl> R +0.0)))
	  ($fl- R)
	R)))

;;; --------------------------------------------------------------------

  (define ($modulo-flonum-fixnum n m)
    (let* ((n.exact ($flonum->exact n))
	   (R       (cond-exact-integer-operand n.exact
		      ((fixnum?)	(inexact ($modulo-fixnum-fixnum n.exact m)))
		      ((bignum?)	(inexact ($modulo-bignum-fixnum n.exact m)))
		      (else
		       (%error-not-integer n)))))
      (if (zero? R)
	  (if (or ($fxzero? m)
		  ($fxpositive? m))
	      +0.0
	    -0.0)
	(inexact R))))

  (define ($modulo-flonum-bignum n m)
    (let ((n.exact ($flonum->exact n)))
      (cond-exact-integer-operand n.exact
	((fixnum?)	(inexact ($modulo-fixnum-bignum n.exact m)))
	((bignum?)	(inexact ($modulo-bignum-bignum n.exact m)))
	(else
	 (%error-not-integer n)))))

  (define ($modulo-flonum-flonum n m)
    (let ((n.exact ($flonum->exact n)))
      (cond-exact-integer-operand n.exact
	((fixnum?)	($modulo-fixnum-flonum n.exact m))
	((bignum?)	($modulo-bignum-flonum n.exact m))
	(else
	 (%error-not-integer n)))))

;;; --------------------------------------------------------------------

  (define (%error-not-integer x)
    (assertion-violation who "expected exact or inexact integer number as argument" x))

  #| end of module: modulo |# )


(module (square
	 $square-fixnum		$square-bignum		$square-ratnum
	 $square-compnum	$square-cflonum)
  (define who 'square)

  (define (square x)
    (cond-numeric-operand x
      ((fixnum?)	($square-fixnum  x))
      ((bignum?)	($square-bignum  x))
      ((flonum?)	($flsquare       x))
      ((ratnum?)	($square-ratnum  x))
      ((compnum?)	($square-compnum x))
      ((cflonum?)	($square-cflonum x))
      (else
       (assertion-violation who "expected number as argument" x))))

  (define ($square-fixnum x)
    ($mul-fixnum-fixnum x x))

  (define ($square-bignum x)
    ($mul-bignum-bignum x x))

  (define ($square-ratnum x)
    ($div-number-number (square ($ratnum-n x))
			(square ($ratnum-d x))))

  (define ($square-compnum x)
    ;; (x.rep + i * x.imp) * (x.rep + i * x.imp)
    ;; = (x.rep * x.rep - x.imp * x.imp) + i * (x.rep * x.imp + x.imp * x.rep)
    ;; = (x.rep^2 - x.imp^2) + i * (x.rep * x.imp + x.imp * x.rep)
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular ($sub-number-number (square x.rep) (square x.imp))
			 (let ((t ($mul-number-number x.rep x.imp)))
			   ($add-number-number t t)))))

  (define ($square-cflonum x)
    ;; (x.rep + i * x.imp) * (x.rep + i * x.imp)
    ;; = (x.rep * x.rep - x.imp * x.imp) + i * (x.rep * x.imp + x.imp * x.rep)
    ;; = (x.rep^2 - x.imp^2) + i * 2 * x.rep * x.imp
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($fl- ($flsquare x.rep) ($flsquare x.imp))
		     (let ((t ($fl* x.rep x.imp)))
		       ($fl+ t t)))))

  #| end of module: square |# )


(module (cube
	 $cube-fixnum		$cube-bignum		$cube-ratnum
	 $cube-compnum		$cube-cflonum)
  (define who 'cube)

  (define (cube x)
    (cond-numeric-operand x
      ((fixnum?)	($cube-fixnum  x))
      ((bignum?)	($cube-bignum  x))
      ((flonum?)	($flcube       x))
      ((ratnum?)	($cube-ratnum  x))
      ((compnum?)	($cube-compnum x))
      ((cflonum?)	($cube-cflonum x))
      (else
       (%error-not-number x))))

  (define ($cube-fixnum x)
    (let ((x^2 ($square-fixnum x)))
      (if (fixnum? x^2)
	  ($mul-fixnum-fixnum x x^2)
	($mul-fixnum-bignum x x^2))))

  (define ($cube-bignum x)
    ($mul-bignum-bignum x ($square-bignum x)))

  (define ($cube-ratnum x)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (let ((x.num^3 (if (fixnum? x.num) ($cube-fixnum x.num) ($cube-bignum x.num)))
	    (x.den^3 (if (fixnum? x.den) ($cube-fixnum x.den) ($cube-bignum x.den))))
	(if (fixnum? x.num^3)
	    (if (fixnum? x.den^3)
		($div-fixnum-fixnum x.num^3 x.den^3)
	      ($div-fixnum-bignum x.num^3 x.den^3))
	  (if (fixnum? x.den^3)
	      ($div-bignum-fixnum x.num^3 x.den^3)
	    ($div-bignum-bignum x.num^3 x.den^3))))))

  (define ($cube-compnum x)
    ;; (A + i B)^2 = (A^2 - B^2) + i 2 A B = D + i 2 A B
    ;;
    ;; (D + i 2 A B) (A + i B)
    ;; = D A + i D B + i 2 A^2 B - 2 A B^2
    ;; = (D A - 2 A B^2) + i (D B + 2 A^2 B)
    ;; = A (D - 2 B^2) + i B (D + 2 A^2)
    ;;
    ;; rep = A (D - 2 B^2)
    ;; imp = B (D + 2 A^2)
    ;;
    (let ((A ($compnum-real x))
	  (B ($compnum-imag x)))
      (let* ((A^2 (square A))
	     (B^2 (square B))
	     (D   ($sub-number-number A^2 B^2))
	     (P   ($sub-number-number D ($mul-fixnum-number 2 B^2)))
	     (Q   ($add-number-number D ($mul-fixnum-number 2 A^2))))
	(let ((rep ($mul-number-number A P))
	      (imp ($mul-number-number B Q)))
	  ($make-rectangular rep imp)))))

  (define ($cube-cflonum x)
    ;; (A + i B)^2 = (A^2 - B^2) + i 2 A B = D + i 2 A B
    ;;
    ;; (D + i 2 A B) (A + i B)
    ;; = D A + i D B + i 2 A^2 B - 2 A B^2
    ;; = (D A - 2 A B^2) + i (D B + 2 A^2 B)
    ;; = A (D - 2 B^2) + i B (D + 2 A^2)
    ;;
    ;; rep = A (D - 2 B^2)
    ;; imp = B (D + 2 A^2)
    ;;
    (let ((A ($cflonum-real x))
	  (B ($cflonum-imag x)))
      (let* ((A^2 ($flsquare A))
	     (B^2 ($flsquare B))
	     (D   ($fl- A^2 B^2))
	     (P   ($fl- D ($fl* 2.0 B^2)))
	     (Q   ($fl+ D ($fl* 2.0 A^2))))
	(let ((rep ($fl* A P))
	      (imp ($fl* B Q)))
	  ($make-cflonum rep imp)))))

  #| end of module: cube |# )


(module (expt
	 $expt-number-fixnum	$expt-number-bignum	$expt-number-flonum
	 $expt-number-ratnum	$expt-number-compnum	$expt-number-cflonum

	 $expt-number-zero-fixnum		$expt-fixnum-zero-fixnum
	 $expt-flonum-zero-fixnum
	 $expt-compnum-zero-fixnum		$expt-cflonum-zero-fixnum

	 $expt-number-negative-fixnum
	 $expt-fixnum-negative-fixnum		$expt-bignum-negative-fixnum
	 $expt-ratnum-negative-fixnum		$expt-flonum-negative-fixnum
	 $expt-compnum-negative-fixnum		$expt-cflonum-negative-fixnum

	 $expt-number-positive-fixnum
	 $expt-fixnum-positive-fixnum		$expt-bignum-positive-fixnum
	 $expt-flonum-positive-fixnum		$expt-ratnum-positive-fixnum
	 $expt-compnum-positive-fixnum		$expt-cflonum-positive-fixnum

	 $expt-fixnum-fixnum	$expt-bignum-fixnum	$expt-ratnum-fixnum
	 $expt-flonum-fixnum	$expt-compnum-fixnum	$expt-cflonum-fixnum

	 $expt-fixnum-bignum	$expt-bignum-bignum	$expt-ratnum-bignum
	 $expt-flonum-bignum	$expt-compnum-bignum	$expt-cflonum-bignum

	 $expt-fixnum-flonum	$expt-bignum-flonum	$expt-ratnum-flonum
	 $expt-flonum-flonum	$expt-compnum-flonum	$expt-cflonum-flonum

	 $expt-fixnum-ratnum	$expt-bignum-ratnum	$expt-ratnum-ratnum
	 $expt-flonum-ratnum	$expt-compnum-ratnum	$expt-cflonum-ratnum

	 $expt-fixnum-cflonum	$expt-bignum-cflonum	$expt-ratnum-cflonum
	 $expt-flonum-cflonum	$expt-compnum-cflonum	$expt-cflonum-cflonum

	 $expt-fixnum-compnum	$expt-bignum-compnum	$expt-ratnum-compnum
	 $expt-flonum-compnum	$expt-compnum-compnum	$expt-cflonum-compnum)
  ;;Return N raised  to the power M.  According to  R6RS, for non-zero N
  ;;this is:
  ;;
  ;;    (expt N M) === (exp (log (expt N M)))
  ;;               === (exp (* M (log N)))
  ;;
  ;;for N equal to zero:
  ;;
  ;;    (expt 0.0 M) = 1.0	if M = 0.0
  ;;                 = 0.0	if (real-part M) is positive
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
  ;;discussion with Kent Dybvig, it does seem like:
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
       (%error-not-number m))))

;;; --------------------------------------------------------------------

  (module ($expt-number-fixnum
	   $expt-fixnum-fixnum	$expt-bignum-fixnum	$expt-ratnum-fixnum
	   $expt-flonum-fixnum	$expt-compnum-fixnum	$expt-cflonum-fixnum)

    (define ($expt-number-fixnum n m)
      ;;We partition  the computation  in the cases:  positive exponent,
      ;;negative exponent,  zero exponent.  The case  with zero exponent
      ;;is computed by itself; the  case of negative exponent is reduced
      ;;to the case of positive exponent.
      ;;
      (if ($fx= m 1)
	  n
	(cond (($fxzero? m)
	       ($expt-number-zero-fixnum n))
	      (($fxpositive? m)
	       ($expt-number-positive-fixnum n m))
	      (else ;M is negative
	       ($expt-number-negative-fixnum n m)))))

    (define ($expt-fixnum-fixnum n m)
      (if ($fx= m 1)
	  n
	(cond (($fxzero? m)
	       ($expt-fixnum-zero-fixnum n))
	      (($fxpositive? m)
	       ($expt-fixnum-positive-fixnum n m))
	      (else ;M is negative
	       ($expt-fixnum-negative-fixnum n m)))))

    (define ($expt-bignum-fixnum n m)
      (if ($fx= m 1)
	  n
	(cond (($fxzero? m)
	       +1)
	      (($fxpositive? m)
	       ($expt-bignum-positive-fixnum n m))
	      (else ;M is negative
	       ($expt-bignum-negative-fixnum n m)))))

    (define ($expt-ratnum-fixnum n m)
      (if ($fx= m 1)
	  n
	(cond (($fxzero? m)
	       +1)
	      (($fxpositive? m)
	       ($expt-ratnum-positive-fixnum n m))
	      (else ;M is negative
	       ($expt-ratnum-negative-fixnum n m)))))

    (define ($expt-flonum-fixnum n m)
      (if ($fx= m 1)
	  n
	(cond (($fxzero? m)
	       ($expt-flonum-zero-fixnum n))
	      (($fxpositive? m)
	       ($expt-flonum-positive-fixnum n m))
	      (else ;M is negative
	       ($expt-flonum-negative-fixnum n m)))))

    (define ($expt-compnum-fixnum n m)
      (if ($fx= m 1)
	  n
	(cond (($fxzero? m)
	       ($expt-compnum-zero-fixnum n))
	      (($fxpositive? m)
	       ($expt-compnum-positive-fixnum n m))
	      (else ;M is negative
	       ($expt-compnum-negative-fixnum n m)))))

    (define ($expt-cflonum-fixnum n m)
      (if ($fx= m 1)
	  n
	(cond (($fxzero? m)
	       ($expt-cflonum-zero-fixnum n))
	      (($fxpositive? m)
	       ($expt-cflonum-positive-fixnum n m))
	      (else ;M is negative
	       ($expt-cflonum-negative-fixnum n m)))))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module ($expt-number-zero-fixnum
	   $expt-fixnum-zero-fixnum
	   $expt-flonum-zero-fixnum
	   $expt-compnum-zero-fixnum
	   $expt-cflonum-zero-fixnum)

    (define ($expt-number-zero-fixnum n)
      ;;N is a number to be raised to the power of 0.
      ;;
      (cond-numeric-operand n
	((fixnum?)	($expt-fixnum-zero-fixnum n))
	((bignum?)	+1)
	((flonum?)	($expt-flonum-zero-fixnum n))
	((ratnum?)	+1)
	((compnum?)	($expt-compnum-zero-fixnum n))
	((cflonum?)	($expt-cflonum-zero-fixnum n))
	(else
	 (%error-not-number n))))

    (define ($expt-fixnum-zero-fixnum n)
      +1)

    ;;Even when N is exact zero N^0 = 1.
    ;;
    ;; (define ($expt-fixnum-zero-fixnum n)
    ;;   (if ($fxzero? n)
    ;;       (%error-undefined-operation who n 0)
    ;;     +1))

    (define ($expt-flonum-zero-fixnum n)
      (if ($flnan? n) +nan.0 +1.0))

    (define ($expt-compnum-zero-fixnum n)
      (let ((n.rep ($compnum-real n))
	    (n.imp ($compnum-imag n)))
	(cond ((or (and (flonum? n.rep) ($flnan? n.rep))
		   (and (flonum? n.imp) ($flnan? n.imp)))
	       +nan.0+nan.0i)
	      ;;Return exact 1 if the operand is exact.
	      ((and (or (fixnum? n.rep) (bignum? n.rep) (ratnum? n.rep))
		    (or (fixnum? n.imp) (bignum? n.imp) (ratnum? n.imp)))
	       +1)
	      (else
	       1.0+0.0i))))

    (define ($expt-cflonum-zero-fixnum n)
      (let ((n.rep ($cflonum-real n))
	    (n.imp ($cflonum-imag n)))
	(if (or ($flnan? n.rep)
		($flnan? n.imp))
	    +nan.0+nan.0i
	  1.0+0.0i)))

    #| end of module: $expt-number-zero-fixnum |# )

;;; --------------------------------------------------------------------

  (module ($expt-number-negative-fixnum
	   $expt-fixnum-negative-fixnum		$expt-bignum-negative-fixnum
	   $expt-ratnum-negative-fixnum		$expt-flonum-negative-fixnum
	   $expt-compnum-negative-fixnum	$expt-cflonum-negative-fixnum)

    (define ($expt-number-negative-fixnum n m)
      ;;N is a number, M is a negative fixnum.
      ;;
      (let ((m.neg ($fx- m)))
	(if (fixnum? m.neg)
	    ($inv-number ($expt-number-positive-fixnum n m.neg))
	  ($expt-number-bignum n m.neg))))

    (define ($expt-fixnum-negative-fixnum n m)
      ;;N is a fixnum, M is a negative fixnum.
      ;;
      (let ((m.neg ($fx- m)))
	(if (fixnum? m.neg)
	    (let ((v ($expt-fixnum-positive-fixnum n m.neg)))
	      (if (fixnum? v)
		  ($inv-fixnum v)
		($inv-bignum v)))
	  ($expt-fixnum-bignum n m.neg))))

    (define ($expt-bignum-negative-fixnum n m)
      ;;N is a bignum, M is a negative fixnum.
      ;;
      (let ((m.neg ($fx- m)))
	(if (fixnum? m.neg)
	    ($inv-bignum ($expt-bignum-positive-fixnum n m.neg))
	  ($expt-bignum-bignum n m.neg))))

    (define ($expt-ratnum-negative-fixnum n m)
      ;;N is a ratnum, M is a negative fixnum.
      ;;
      (let ((m.neg ($fx- m)))
	(if (fixnum? m.neg)
	    ($inv-ratnum ($expt-ratnum-positive-fixnum n m.neg))
	  ($expt-ratnum-bignum n m.neg))))

    (define ($expt-flonum-negative-fixnum n m)
      ;;N is a flonum, M is a negative fixnum.
      ;;
      (let ((m.neg ($fx- m)))
	(if (fixnum? m.neg)
	    ($inv-flonum ($expt-flonum-positive-fixnum n m.neg))
	  ($expt-flonum-bignum n m.neg))))

    (define ($expt-compnum-negative-fixnum n m)
      ;;N is a compnum, M is a negative fixnum.
      ;;
      (let ((m.neg ($fx- m)))
	(if (fixnum? m.neg)
	    ;;The result of expt can be a compnum or a cflonum.
	    ($inv-number ($expt-compnum-positive-fixnum n m.neg))
	  ($expt-compnum-bignum n m.neg))))

    (define ($expt-cflonum-negative-fixnum n m)
      ;;N is a cflonum, M is a negative fixnum.
      ;;
      (let ((m.neg ($fx- m)))
	(if (fixnum? m.neg)
	    ($inv-cflonum ($expt-cflonum-positive-fixnum n m.neg))
	  ($expt-cflonum-bignum n m.neg))))

    #| end of module: $expt-number-negative-fixnum |# )

;;; --------------------------------------------------------------------

  (module ($expt-number-positive-fixnum
	   $expt-fixnum-positive-fixnum		$expt-bignum-positive-fixnum
	   $expt-flonum-positive-fixnum		$expt-ratnum-positive-fixnum
	   $expt-compnum-positive-fixnum	$expt-cflonum-positive-fixnum)
    ;;The case with positive fixnum  exponent is computed by recursing a
    ;;number of  times equal to  the bits  in the representation  of the
    ;;exponent;  given  the  operation  R  =  N^M  and  considering  the
    ;;representation of the exponent:
    ;;
    ;;   M = b_0 * 2^0 + b_1 * 2^1 + b_2 * 2^2 + b_3 * 2^3 + ...
    ;;
    ;;we can write:
    ;;
    ;;   N^M = N^(b_0 * 2^0 + b_1 * 2^1 + b_2 * 2^2 + b_3 * 2^3 + ...)
    ;;       = N^(b_0 * 2^0) * N^(b_1 * 2^1) * N^(b_2 * 2^2) * N^(b_3 * 2^3) ...
    ;;       = N^b_0 * N^(b_1 * 2) * N^(b_2 * 2 * 2) * N^(b_3 * 2 * 2 * 2) ...
    ;;       = N^b_0 * (N^2)^b_1 * ((N^2)^2)^b_2 * (((N^2)^2)^2)^b_3 ...
    ;;
    ;;in  which we  have to  compute the  term with  bit b_(i+1)  before
    ;;computing the  term with  bit b_i, a  non-tail recursion.   We can
    ;;define:
    ;;
    ;;   N_0 = N
    ;;   N_1 = N^2 = N_0^2
    ;;   N_2 = (N^2)^2 = N_1^2
    ;;   N_3 = ((N^2)^2)^2 = N_2^2
    ;;   ...
    ;;
    ;;so:
    ;;
    ;;   N^M = N_0^b_0 * N_1^b_1 * N_2^b_2 * N_3^b_3 * ...
    ;;
    ;;where:
    ;;
    ;;   N_0 = 0
    ;;   N_i = N_(i-1)^2    i = 1, 2, 3, ...
    ;;
    ;;Notes about used forms:
    ;;
    ;; ($fxeven? m)
    ;;     The result is  1 for even M and 0 for odd  M; in other words:
    ;;     the return value is the rightmost bit of M.
    ;;
    ;; ($fxsra m 1)
    ;;     The  operation is "fixnum  shift right arithmetic";  when the
    ;;     offset  is 1:  the rightmost  bit is  discarded and  the most
    ;;     significant  bit inserted extends the  sign of M, the  bit is
    ;;     zero if M is positive and 1 if M is negative.
    ;;
    (define ($expt-number-positive-fixnum n m)
      ;;N is a number, M is a positive fixnum.
      ;;
      (if ($fx= m 1)
	  n
	(cond-numeric-operand n
	  ((fixnum?)	($expt-fixnum-positive-fixnum  n m))
	  ((bignum?)	($expt-bignum-positive-fixnum  n m))
	  ((flonum?)	($expt-flonum-positive-fixnum  n m))
	  ((ratnum?)	($expt-ratnum-positive-fixnum  n m))
	  ((compnum?)	($expt-compnum-positive-fixnum n m))
	  ((cflonum?)	($expt-cflonum-positive-fixnum n m))
	  (else
	   (%error-not-number n)))))

    (define-syntax define-expt-num-positive-fixnum
      (syntax-rules ()
	((_ ?who ?one ?square-num ?mul-num-number)
	 (define (?who N_i-1 M)
	   (cond (($fxzero? M)
		  ?one)
		 (($fx= M 1)
		  N_i-1)
		 (else
		  (let ((N_i ($expt-number-positive-fixnum (?square-num N_i-1) ($fxsra M 1))))
		    (if ($fxeven? M)
			N_i
		      (?mul-num-number N_i-1 N_i)))))))))

    (define-expt-num-positive-fixnum $expt-fixnum-positive-fixnum
      1 $square-fixnum $mul-fixnum-number)

    (define-expt-num-positive-fixnum $expt-bignum-positive-fixnum
      1 $square-bignum $mul-bignum-number)

    (define ($expt-flonum-positive-fixnum n m)
      ;;N is a flonum, M is a positive fixnum.
      ;;
      (define-expt-num-positive-fixnum $expt-flonum-positive-fixnum/sub
	1.0 $flsquare $fl*)
      (cond (($flzero?/positive n)	+0.0)
	    (($flzero?/negative n)	(if ($fxeven? m) +0.0 -0.0))
	    (($flnan? n)		+nan.0)
	    (($flinfinite? n)
	     (if (or ($flpositive? n)
		     ($fxeven? m))
		 +inf.0
	       -inf.0))
	    (else
	     ($expt-flonum-positive-fixnum/sub n m))))

    (define ($expt-ratnum-positive-fixnum n m)
      ;;N is a ratnum, M is a positive fixnum.
      ;;
      ($make-ratnum ($expt-number-positive-fixnum ($ratnum-n n) m)
		    ($expt-number-positive-fixnum ($ratnum-d n) m)))

    (define ($expt-compnum-positive-fixnum n m)
      ;;N is a compnum, M is a positive fixnum.
      ;;
      (define-expt-num-positive-fixnum $expt-compnum-positive-fixnum/sub
	1 $square-compnum $mul-compnum-number)
      (if ($fx= m 1)
	  n
	(let ((n.rep ($compnum-real n))
	      (n.imp ($compnum-imag n)))
	  (cond ((or (and (flonum? n.rep) ($flnan? n.rep))
		     (and (flonum? n.imp) ($flnan? n.imp)))
		 +nan.0+nan.0i)
		((or (and (flonum? n.rep) ($flinfinite? n.rep))
		     (and (flonum? n.imp) ($flinfinite? n.imp)))
		 (exp ($mul-fixnum-number m ($log-compnum n))))
		(else
		 ($expt-compnum-positive-fixnum/sub n m))))))

    (define ($expt-cflonum-positive-fixnum n m)
      ;;N is a cflonum, M is a positive fixnum.
      ;;
      (define-expt-num-positive-fixnum $expt-cflonum-positive-fixnum/sub
	1.0+0.0i $square-cflonum $mul-cflonum-cflonum)
      (if ($fx= m 1)
	  n
	(let ((n.rep ($cflonum-real n))
	      (n.imp ($cflonum-imag n)))
	  (cond ((or ($flnan? n.rep)
		     ($flnan? n.imp))
		 +nan.0+nan.0i)
		((or ($flinfinite? n.rep)
		     ($flinfinite? n.imp))
		 ;;There are special cases to be handled here.
		 (exp (* m (log n))))
		(else
		 ($expt-cflonum-positive-fixnum/sub n m))))))

    #| end of module: $expt-number-positive-fixnum |# )

;;; --------------------------------------------------------------------

  (module ($expt-number-bignum
	   $expt-fixnum-bignum	$expt-bignum-bignum	$expt-ratnum-bignum
	   $expt-flonum-bignum	$expt-compnum-bignum	$expt-cflonum-bignum)

    (define ($expt-number-bignum n m)
      ;;N is a number, M is a bignum.
      ;;
      (cond-numeric-operand n
	((fixnum?)	($expt-fixnum-bignum   n m))
	((bignum?)	($expt-bignum-bignum   n m))
	((ratnum?)	($expt-ratnum-bignum   n m))
	((flonum?)	($expt-flonum-bignum   n m))
	((compnum?)	($expt-compnum-bignum  n m))
	((cflonum?)	($expt-cflonum-bignum  n m))
	(else
	 (%error-not-number n))))

    (define ($expt-fixnum-bignum n m)
      ;;N is a fixnum, M is a bignum.
      ;;
      (cond (($fxzero? n)	0)
	    (($fx= n +1)	1)
	    (($fx= n -1)	(if ($bignum-even? m) +1 -1))
	    (else
	     (%error-result-too-big n m))))

    (define ($expt-bignum-bignum n m)
      ;;N is a bignum, M is a bignum.
      ;;
      (%error-result-too-big n m))

    (define ($expt-ratnum-bignum n m)
      ;;N is a ratnum, M is a bignum.
      ;;
      ($expt-flonum-flonum ($ratnum->flonum n)
			   ($bignum->flonum m)))

    (define ($expt-flonum-bignum n m)
      ;;N is a flonum, M is a bignum.
      ;;
      (cond (($flzero?/positive n)	+0.0)
	    (($flzero?/negative n)	(if ($bignum-even? m) +0.0 -0.0))
	    (($fl= n +1.0)		+1.0)
	    (($fl= n -1.0)		(if ($bignum-even? m) +1.0 -1.0))
	    (($flnan? n)		+nan.0)
	    (($flinfinite? n)		(if (or ($flpositive? n)
						($bignum-even? m))
					    +inf.0
					  -inf.0))
	    (else
	     ;;Here  we  have  already  excluded the  cases  N=+0.0  and
	     ;;N=-0.0.
	     (if ($flpositive? n)
		 ;;Positive operands have real logarithms.
		 ($exp-flonum ($fl* (inexact m) ($fllog n)))
	       ;;Negative operands have complex logarithms.
	       ($exp-cflonum ($mul-flonum-cflonum (inexact m) ($log-flonum n)))))))

    (define ($expt-compnum-bignum n m)
      ;;N is a compnum, M is a bignum.
      ;;
      (let ((n.rep ($compnum-real n))
	    (n.imp ($compnum-imag n)))
	(if (or (flonum? n.rep)
		(flonum? n.imp))
	    ($expt-cflonum-bignum ($compnum->cflonum n) m)
	  (%error-result-too-big n m))))

    (define ($expt-cflonum-bignum n m)
      ;;N is a cflonum, M is a bignum.
      ;;
      ($exp-cflonum ($mul-bignum-cflonum m ($log-cflonum n))))

    #| end of module: $expt-number-bignum |# )

;;; --------------------------------------------------------------------

  (module ($expt-number-flonum
	   $expt-fixnum-flonum	$expt-bignum-flonum	$expt-ratnum-flonum
	   $expt-flonum-flonum	$expt-compnum-flonum	$expt-cflonum-flonum)

    (define ($expt-number-flonum n m)
      (cond-numeric-operand n
	((fixnum?)	($expt-fixnum-flonum n m))
	((bignum?)	($expt-bignum-flonum n m))
	((ratnum?)	($expt-ratnum-flonum n m))
	((flonum?)	($expt-flonum-flonum n m))
	((compnum?)	($expt-compnum-flonum n m))
	((cflonum?)	($expt-cflonum-flonum n m))
	(else
	 (%error-not-number n))))

    (define ($expt-fixnum-flonum n m)
      (cond (($fxzero? n)	(cond (($flzero?     m)	1.0)
				      (($flpositive? m)	0.0)
				      (else		+inf.0)))
	    (($fx= n +1)	1.0)
	    (else
	     ($expt-flonum-flonum ($fixnum->flonum n) m))))

    (define ($expt-bignum-flonum n m)
      (if ($flonum-integer? m)
	  (let ((m.exact (exact m)))
	    (if (fixnum? m.exact)
		(inexact ($expt-number-fixnum n m.exact))
	      ($expt-flonum-flonum ($bignum->flonum n) m)))
	($expt-flonum-flonum ($bignum->flonum n) m)))

    (define ($expt-ratnum-flonum n m)
      (if ($flonum-integer? m)
	  (let ((m.exact (exact m)))
	    (if (fixnum? m.exact)
		(inexact ($expt-number-fixnum n m.exact))
	      ($expt-flonum-flonum ($ratnum->flonum n) m)))
	($expt-flonum-flonum ($ratnum->flonum n) m)))

    (define ($expt-flonum-flonum n m)
      (cond (($flnan? n)	+nan.0)
	    (($flnan? m)	+nan.0)
	    (($flzero? n)
	     ;;We should do:
	     ;;
	     ;;   (expt N M) = (exp (* M (log N)))
	     ;;
	     ;;and we know that:
	     ;;
	     ;;   (log 0.0)	=> -inf.0
	     ;;   (* M -inf.0)	=> -inf.0
	     ;;   (exp -inf.0)	=> +0.0
	     ;;
	     ;;but according to R6RS, for N equal to zero:
	     ;;
	     ;;   (expt 0.0 M) = 1.0	if M = 0.0
	     ;;                = 0.0	if (real-part M) is positive
	     ;;
	     ;;for  other cases  in which  the first  argument is  zero,
	     ;;either  an  exception  is   raised  with  condition  type
	     ;;"&implementation-restriction",  or an  unspecified number
	     ;;object is returned.
	     ;;
	     ;;We can also say that, with negative M:
	     ;;
	     ;;   (expt N M) = (/ 1 (expt N (- M))) = (/ 1 0.0) = +inf.0
	     ;;
	     ;;WolframAlpha agrees with this.
	     ;;
	     (cond (($flzero? m)	1.0)
		   (($flpositive? m)	0.0)
		   (else
		    (if ($flzero?/positive n) +inf.0 -inf.0))))
	    (($fl= m 1.0)
	     n)
	    (else
	     ;;Notice that the result of  $LOG-FLONUM can be a flonum or
	     ;;cflonum.
	     (exp ($mul-flonum-number m ($log-flonum n))))))

    (define ($expt-compnum-flonum n m)
      ($expt-cflonum-flonum ($compnum->cflonum n) m))

    (define ($expt-cflonum-flonum n m)
      (let ((n.rep ($cflonum-real n))
	    (n.imp ($cflonum-imag n)))
	(cond (($fl= m 1.0)
	       n)
	      ((or ($flnan? m)
		   ($flnan? n.rep)
		   ($flnan? n.imp))
	       +nan.0+nan.0i)
	      ((and ($flzero? n.rep)
		    ($flzero? n.imp))
	       (cond (($flzero? m)	1.0+0.0i)
		     (($flpositive? m)	0.0+0.0i)
		     (else
		      ($exp-cflonum ($mul-flonum-cflonum m ($log-cflonum n))))))
	      (else
	       ($exp-cflonum ($mul-flonum-cflonum m ($log-cflonum n)))))))

    #| end of module: $expt-number-flonum |# )

;;; --------------------------------------------------------------------

  (module ($expt-number-ratnum
	   $expt-fixnum-ratnum	$expt-bignum-ratnum	$expt-ratnum-ratnum
	   $expt-flonum-ratnum	$expt-compnum-ratnum	$expt-cflonum-ratnum)
    ;;In general we can consider doing:
    ;;
    ;;   N^M = N^(P/Q) = (N^P)^(1/Q)
    ;;
    ;;but we have to remember that R6RS states:
    ;;
    ;;   For an  exact real number object N and  an exact integer object
    ;;    M, (expt  N M)  must return  an exact  result.  For  all other
    ;;   values  of N and  M, (expt N M)  may return an  inexact result,
    ;;   even when both N and M are exact.
    ;;
    ;;so let's not try to be heroes and do the inexact conversion.
    ;;
    (define ($expt-number-ratnum n m)
      (cond-numeric-operand n
	((fixnum?)	($expt-fixnum-ratnum  n m))
	((bignum?)	($expt-bignum-ratnum  n m))
	((ratnum?)	($expt-ratnum-ratnum  n m))
	((flonum?)	($expt-flonum-ratnum  n m))
	((compnum?)	($expt-compnum-ratnum n m))
	((cflonum?)	($expt-cflonum-ratnum n m))
	(else
	 (%error-not-number n))))

    (define ($expt-fixnum-ratnum n m)
      (cond (($fxzero? n)
	     (if (positive? ($ratnum-n m))
		 0
	       (%error-division-by-zero who n m)))
	    (($fx= n 1)
	     1)
	    (else
	     ($expt-flonum-flonum ($fixnum->flonum n)
				  ($ratnum->flonum m)))))

    (define ($expt-bignum-ratnum n m)
      ($expt-flonum-flonum ($bignum->flonum n)
			   ($ratnum->flonum m)))

    (define ($expt-ratnum-ratnum n m)
      ($expt-flonum-flonum ($ratnum->flonum n)
			   ($ratnum->flonum m)))

    (define ($expt-flonum-ratnum n m)
      ($expt-flonum-flonum n ($ratnum->flonum m)))

    (define ($expt-compnum-ratnum n m)
      ($expt-cflonum-flonum ($compnum->cflonum n)
			    ($ratnum->flonum m)))

    (define ($expt-cflonum-ratnum n m)
      ($expt-cflonum-flonum n
			    ($ratnum->flonum m)))

    #| end of module: $expt-number-ratnum |# )

;;; --------------------------------------------------------------------

  (module ($expt-number-compnum
	   $expt-fixnum-compnum		$expt-bignum-compnum	$expt-ratnum-compnum
	   $expt-flonum-compnum		$expt-compnum-compnum	$expt-cflonum-compnum)

    (define ($expt-number-compnum n m)
      (cond-numeric-operand n
	((fixnum?)	($expt-fixnum-compnum  n m))
	((bignum?)	($expt-bignum-compnum  n m))
	((ratnum?)	($expt-ratnum-compnum  n m))
	((flonum?)	($expt-flonum-compnum  n m))
	((compnum?)	($expt-compnum-compnum n m))
	((cflonum?)	($expt-cflonum-compnum n m))
	(else
	 (%error-not-number n))))

    (define ($expt-fixnum-compnum n m)
      (cond (($fxzero? n)
	     (let ((m.rep ($compnum-real m)))
	       (cond ((or (and (fixnum? m.rep) ($fxnegative?      m.rep))
			  (and (bignum? m.rep) ($bignum-negative? m.rep)))
		      (%error-division-by-zero who n m))
		     ((eq? m.rep 0)
		      (%error-undefined-operation who n m))
		     (else
		      0))))
	    (($fx= n 1)
	     (let ((m.rep ($compnum-real m))
		   (m.imp ($compnum-imag m)))
	       (if (and (exact? m.rep)
			(exact? m.imp))
		   1
		 1.0)))
	    (else
	     ($expt-flonum-cflonum ($fixnum->flonum   n)
				   ($compnum->cflonum m)))))

    (define ($expt-bignum-compnum n m)
      ($expt-flonum-cflonum ($bignum->flonum   n)
			    ($compnum->cflonum m)))

    (define ($expt-ratnum-compnum n m)
      ($expt-flonum-cflonum ($ratnum->flonum   n)
			    ($compnum->cflonum m)))

    (define ($expt-flonum-compnum n m)
      ($expt-flonum-cflonum n
			    ($compnum->cflonum m)))

    (define ($expt-compnum-compnum n m)
      ($expt-cflonum-cflonum ($compnum->cflonum n)
			     ($compnum->cflonum m)))

    (define ($expt-cflonum-compnum n m)
      ($expt-cflonum-cflonum n
			     ($compnum->cflonum m)))

    #| end of module: $expt-number-cflonum |# )

;;; --------------------------------------------------------------------

  (module ($expt-number-cflonum
	   $expt-fixnum-cflonum		$expt-bignum-cflonum	$expt-ratnum-cflonum
	   $expt-flonum-cflonum		$expt-compnum-cflonum	$expt-cflonum-cflonum)

    (define ($expt-number-cflonum n m)
      (cond-numeric-operand n
	((fixnum?)	($expt-fixnum-cflonum  n m))
	((bignum?)	($expt-bignum-cflonum  n m))
	((ratnum?)	($expt-ratnum-cflonum  n m))
	((flonum?)	($expt-flonum-cflonum  n m))
	((compnum?)	($expt-compnum-cflonum n m))
	((cflonum?)	($expt-cflonum-cflonum n m))
	(else
	 (%error-not-number n))))

    (define ($expt-fixnum-cflonum n m)
      ($expt-flonum-cflonum ($fixnum->flonum n) m))

    (define ($expt-bignum-cflonum n m)
      ($expt-flonum-cflonum ($bignum->flonum n) m))

    (define ($expt-ratnum-cflonum n m)
      ($expt-flonum-cflonum ($ratnum->flonum n) m))

    (define ($expt-flonum-cflonum n m)
      ;;We should do:
      ;;
      ;;   (expt N M) = (exp (* M (log N)))
      ;;
      ;;and we know that:
      ;;
      ;;   (log 0.0)	=> -inf.0
      ;;   (* M -inf.0)	=> -inf.0+0.0i
      ;;   (exp -inf.0)	=> +0.0+0.0i
      ;;
      ;;but according to R6RS, for N equal to zero:
      ;;
      ;;   (expt 0.0 M) = 1.0	if M = 0.0
      ;;                = 0.0	if (real-part M) is positive
      ;;
      ;;for other cases  in which the first argument is  zero, either an
      ;;exception      is      raised      with      condition      type
      ;;"&implementation-restriction", or  an unspecified  number object
      ;;is returned.
      ;;
      ;;We can also say that, with negative M:
      ;;
      ;;   (expt N M) = (/ 1 (expt N (- M))) = (/ 1 0.0) = +inf.0
      ;;
      ;;WolframAlpha agrees with this.
      ;;
      ;;With  M =  0.0+0.0i: WolframAlpha  says "indeterminate",  Petite
      ;;Chez Scheme says 0.0, Ypsilon Scheme says +nan.0+nan.0i, Larceny
      ;;says 1.0, Mosh says 0.0.  We go with raising an exception.
      ;;
      (cond (($flzero? n)
	     (let ((m.rep ($cflonum-real m)))
	       (cond (($flpositive? m.rep)
		      0.0)
		     ((and ($flzero? m.rep)
			   ($flzero? ($cflonum-imag m)))
		      (%error-undefined-result n m))
		     (else
		      +inf.0+0.0i))))
	    (($flnan? n)
	     +nan.0+nan.0i)
	    (else
	     ($exp-cflonum ($mul-cflonum-number m ($log-flonum n))))))

    (define ($expt-compnum-cflonum n m)
      ($expt-cflonum-cflonum ($compnum->cflonum n) m))

    (define ($expt-cflonum-cflonum n m)
      ;;See the discussion for $EXPT-FLONUM-CFLONUM.
      ;;
      (let ((n.rep ($cflonum-real n))
	    (n.imp ($cflonum-imag n))
	    (m.rep ($cflonum-real m))
	    (m.imp ($cflonum-imag m)))
	(cond ((and ($flzero? n.rep)
		    ($flzero? n.imp))
	       (cond (($flpositive? m.rep)
		      0.0+0.0i)
		     ((and ($flzero? m.rep)
			   ($flzero? m.imp))
		      (%error-undefined-result n m))
		     (else
		      ;;According   to  Wolfram   Alpha  this   case  is
		      ;;1.0+0.0i; I  dunno why: it looks  not consistent
		      ;;with  the  result for  N  =  0.0.  Let's  do  it
		      ;;cowardly for now.  (Marco Maggi; Dec 7, 2012)
		      (%error-undefined-result n m))))
	      (else
	       ($exp-cflonum ($mul-cflonum-number m ($log-cflonum n)))))))

    #| end of module: $expt-number-cflonum |# )

;;; --------------------------------------------------------------------

  (define (%error-result-too-big n m)
    (raise
     (condition (make-who-condition who)
		(make-implementation-restriction-violation)
		(make-message-condition "result is too big to compute")
		(make-irritants-condition (list n m)))))

  (define (%error-undefined-result n m)
    (raise
     (condition (make-implementation-restriction-violation)
		(make-who-condition who)
		(make-message-condition "undefined result")
		(make-irritants-condition (list n m)))))

  #| end of module: expt |# )


(module (sqrt
	 $sqrt-fixnum		$sqrt-flonum		$sqrt-bignum
	 $sqrt-ratnum		$sqrt-compnum		$sqrt-cflonum)
  (define who 'sqrt)

  (define (sqrt x)
    (cond-numeric-operand x
      ((fixnum?)	($sqrt-fixnum x))
      ((bignum?)	($sqrt-bignum x))
      ((ratnum?)	($sqrt-ratnum x))
      ((flonum?)	($sqrt-flonum x))
      ((compnum?)	($sqrt-compnum x))
      ((cflonum?)	($sqrt-cflonum x))
      (else
       (%error-not-number x))))

  (define ($sqrt-flonum x)
    ;;This can return both a flonum or a compnum!!!
    ;;
    (if (or ($flzero?/negative x)
	    ($fl< x -0.0))
	($make-rectangular 0 (foreign-call "ikrt_fl_sqrt" ($fl- x)))
      (foreign-call "ikrt_fl_sqrt" x)))

  (define ($sqrt-fixnum x)
    (if ($fxnegative? x)
	(make-rectangular 0 ($sqrt-fixnum ($fx- x)))
      (let-values (((root residual) ($exact-integer-sqrt-fixnum x)))
	(if ($fxzero? residual)
	    root
	  (foreign-call "ikrt_fx_sqrt" x)))))

  (define ($sqrt-bignum x)
    (if ($bignum-positive? x)
	(let-values (((root residual) ($exact-integer-sqrt-bignum x)))
	  (if (eq? residual 0)
	      root
	    (let ((v ($sqrt-flonum ($bignum->flonum x))))
	      ;;Could the (dropped) residual ever affect the answer?
	      (if ($flinfinite? v)
		  (if (bignum? root)
		      ;;The argument  1 makes it  round up in case  of a
		      ;;tie.
		      (foreign-call "ikrt_bignum_to_flonum" root 1 ($make-flonum))
		    (inexact root))
		v))))
      ;;The negation of a negative bignum is always a bignum.
      ($make-compnum 0 ($sqrt-bignum ($neg-bignum x)))))

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
    (let ((Z.rep ($compnum-real Z))
	  (Z.imp ($compnum-imag Z)))
      (let ((magn (sqrt (+ (square Z.rep) (square Z.imp))))
	    (sgn  (sign Z.imp)))
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
    (let ((Z.rep ($cflonum-real Z))
	  (Z.imp ($cflonum-imag Z)))
      ;;Remember  that  $SQRT-FLONUM  can  return both  a  flonum  or  a
      ;;compnum!!!
      (let* ((magn  ($sqrt-flonum ($fl+ ($flsquare Z.rep) ($flsquare Z.imp))))
	     (sgn   ($sign-flonum Z.imp))
	     (R.rep (sqrt ($div-number-flonum ($add-number-flonum magn Z.rep) 2.0)))
	     (Q     (sqrt ($div-number-flonum ($sub-number-flonum magn Z.rep) 2.0)))
	     (R.imp ($mul-flonum-number sgn Q)))
	($make-cflonum R.rep R.imp))))

  #| end of module: sqrt |# )


(module (exact-integer-sqrt
	 $exact-integer-sqrt-fixnum
	 $exact-integer-sqrt-bignum)

  (define who 'exact-integer-sqrt)

  (define (exact-integer-sqrt x)
    (cond ((fixnum? x)
	   ($exact-integer-sqrt-fixnum x))

	  ((bignum? x)
	   ($exact-integer-sqrt-bignum x))

	  (else
	   (assertion-violation who "expected exact integer as argument" x))))

  (define ($exact-integer-sqrt-fixnum x)
    (cond (($fx> x 0)
	   (let ((s (foreign-call "ikrt_exact_fixnum_sqrt" x)))
	     (values s ($fx- x ($fx* s s)))))
	  (($fxzero? x)
	   (values 0 0))
	  (else
	   (%error-negative-operand x))))

  (define ($exact-integer-sqrt-bignum x)
    (if ($bignum-positive? x)
	(let ((r (foreign-call "ikrt_exact_bignum_sqrt" x)))
	  (values ($car r) ($cdr r)))
      (%error-negative-operand x)))

  (define (%error-negative-operand x)
    (assertion-violation who "expected non-negative exact integer as argument" x))

  #| end of module: exact-integer-sqrt |# )


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
    ($max-flonum-flonum ($fixnum->flonum x) y))

  (define ($max-fixnum-ratnum x y)
    (if (fxrt< x y)
	y
      x))

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
    ($max-flonum-flonum ($bignum->flonum x) y))

  (define ($max-bignum-ratnum x y)
    (if (bnrt> x y)
	x
      y))

;;; --------------------------------------------------------------------

  (define ($max-flonum-flonum x y)
    (cond (($flnan? x)		x)
	  (($flnan? y)		y)
	  (($fl>= x y)		x)
	  (else			y)))

  (define ($max-flonum-fixnum x y)
    ($max-flonum-flonum x ($fixnum->flonum y)))

  (define ($max-flonum-bignum x y)
    ($max-flonum-flonum x ($bignum->flonum y)))

  (define ($max-flonum-ratnum x y)
    ;;FIXME May be incorrect.  (Abdulaziz Ghuloum)
    ($max-flonum-flonum x ($ratnum->flonum y)))

;;; --------------------------------------------------------------------

  (define ($max-ratnum-fixnum x y)
    (if (rtfx> x y)
	x
      y))

  (define ($max-ratnum-bignum x y)
    (if (rtbn> x y)
	x
      y))

  (define ($max-ratnum-ratnum x y)
    (if (rtrt> x y)
	x
      y))

  (define ($max-ratnum-flonum x y)
    ($max-flonum-flonum ($ratnum->flonum x) y))

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
    ($min-flonum-flonum ($fixnum->flonum x) y))

  (define ($min-fixnum-ratnum x y)
    (if (fxrt< x y)
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
    ($min-flonum-flonum ($bignum->flonum x) y))

  (define ($min-bignum-ratnum x y)
    (if (bnrt< x y)
	x
      y))

;;; --------------------------------------------------------------------

  (define ($min-flonum-flonum x y)
    (cond (($flnan? x)		x)
	  (($flnan? y)		y)
	  (($fl<= x y)		x)
	  (else			y)))

  (define ($min-flonum-fixnum x y)
    ($min-flonum-flonum x ($fixnum->flonum y)))

  (define ($min-flonum-bignum x y)
    ($min-flonum-flonum x ($bignum->flonum y)))

  (define ($min-flonum-ratnum x y)
    ;;FIXME May be incorrect.  (Abdulaziz Ghuloum)
    ($min-flonum-flonum x ($ratnum->flonum y)))

;;; --------------------------------------------------------------------

  (define ($min-ratnum-fixnum x y)
    (if (rtfx< x y)
	x
      y))

  (define ($min-ratnum-bignum x y)
    (if (rtbn< x y)
	x
      y))

  (define ($min-ratnum-ratnum x y)
    (if (rtrt<= x y)
	x
      y))

  (define ($min-ratnum-flonum x y)
    ($min-flonum-flonum ($ratnum->flonum x) y))

  #| end of module: min |# )


(module (sign
	 $sign-fixnum		$sign-bignum
	 $sign-flonum		$sign-ratnum)
  (define who 'sign)

  (define (sign x)
    (cond-real-numeric-operand x
      ((fixnum?)	($sign-fixnum x))
      ((bignum?)	($sign-bignum x))
      ((ratnum?)	($sign-ratnum x))
      ((flonum?)	($sign-flonum x))
      (else
       (%error-not-real-number x))))

;;; --------------------------------------------------------------------

  (define ($sign-fixnum x)
    (cond (($fxpositive? x)	+1)
	  (($fxnegative? x)	-1)
	  (else			0)))

  (define ($sign-bignum x)
    (if ($bignum-positive? x) +1 -1))

  (define ($sign-flonum x)
    (cond ((flnan? x)
	   +nan.0)
	  ((or ($flzero?/positive x)
	       ($flpositive? x))
	   +1.0)
	  (else
	   -1.0)))

  (define ($sign-ratnum x)
    (sign ($ratnum-n x)))

  #| end of module: sign |# )


(module (abs
	 $abs-fixnum		$abs-bignum
	 $abs-flonum		$abs-ratnum)
  (define who 'abs)

  (define (abs x)
    (cond-real-numeric-operand x
      ((fixnum?)	($abs-fixnum x))
      ((bignum?)	($abs-bignum x))
      ((ratnum?)	($abs-ratnum x))
      ((flonum?)	($abs-flonum x))
      (else
       (%error-not-real-number x))))

;;; --------------------------------------------------------------------

  (define ($abs-fixnum x)
    (if ($fxnegative? x)
	(if ($fx= x (least-fixnum))
	    (- (least-fixnum))
	  ($fx- x))
      x))

  (define ($abs-bignum x)
    ;;Return a bignum or fixnum.
    (if ($bignum-positive? x)
	x
      ($neg-bignum x)))

  (define ($abs-flonum x)
    (if ($fx> ($flonum-u8-ref x 0) 127)
	($fl- x)
      x))

  (define ($abs-ratnum x)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (if (fixnum? x.num)
	  (if ($fxpositive? x.num)
	      x
	    ;;We want a ratnum with positive denominator.
	    ($make-ratnum ($neg-fixnum x.num) x.den))
	;;X.NUM is a bignum.
	(if ($bignum-positive? x.num)
	    x
	  ;;We want a ratnum with positive denominator.
	  ($make-ratnum ($neg-bignum x.num) x.den)))))

  #| end of module: abs |# )


(module (inexact exact->inexact)

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
      ((compnum?)	($compnum->cflonum x))
      ((cflonum?)	x)
      (else
       (%error-not-number x))))

  #| end of module |# )


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
      (case-fixnums obj
	((2 8 10 16)	#t)
	(else		#f))
      (assertion-violation who "invalid radix" obj))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (module ($number->string)

    (define ($number->string x radix)
      (cond-numeric-operand x
	((fixnum?)	(fixnum->string   x radix))
  	((bignum?)	(bignum->string   x radix))
	((flonum?)	(%flonum->string  x radix))
	((ratnum?)	(ratnum->string   x radix))
	((compnum?)	(%compnum->string x radix))
	((cflonum?)	(%cflonum->string x radix))
	(else
	 (%error-not-number x))))

    (define (%flonum->string x radix)
      (with-arguments-validation (who)
	  ((radix10	radix))
	(flonum->string x)))

    (define (%compnum->string x radix)
      (let ((x.rep ($compnum-real x))
	    (x.imp ($compnum-imag x)))
	(if (and (fixnum?  x.rep)
		 ($fxzero? x.rep))
	    (string-append (imag x.imp radix) "i")
	  (string-append ($number->string x.rep radix)
			 (imag x.imp radix) "i"))))

    (define (%cflonum->string x radix)
      (with-arguments-validation (who)
	  ((radix10	radix))
	(let ((x.rep ($cflonum-real x))
	      (x.imp ($cflonum-imag x)))
	  (cond (($flnan? x.imp)
		 (string-append (flonum->string x.rep) "+nan.0i"))
		(($flinfinite? x.imp)
		 (string-append (flonum->string x.rep)
				(if ($flpositive? x.imp) "+inf.0i" "-inf.0i")))
		(else
		 (string-append (flonum->string x.rep) (imag x.imp 10) "i"))))))

    (define (imag x.imp radix)
      ;;Compose a string for the imaginary part of a cflonum or compnum.
      ;;X.IMP can be any real number, including infinities and NaN.  The
      ;;caller will take care of  prepending the real part and appending
      ;;the character #\i:
      ;;
      ;;   (string-append x.rep (imag x.imp radix) "i")
      ;;
      ;;this function has especially to decide what sign to put in front
      ;;of the imaginary part, avoiding insertion of two signs.
      ;;
      (cond
       ;;Special case to allow printing: "Rep+i".
       ((and (fixnum? x.imp)
	     ($fx= x.imp +1))
	"+")
       ;;Special case to allow printing: "Rep-i".
       ((and (fixnum? x.imp)
	     ($fx= x.imp -1))
	"-")
       ;;If X.IMP is negative: omit the  sign here, a negative sign will
       ;;be inserted by the call to $NUMBER->STRING below.
       ((or (< x.imp 0)
	    (and (flonum? x.imp)
		 ($flzero?/negative x.imp)))
	($number->string x.imp radix))
       ;;If  X.IMP is  +inf.0  avoid prepending  an additional  positive
       ;;sign.
       ((and (flonum? x.imp)
	     ($flinfinite? x.imp))
	"+inf.0")
       ;;If  X.IMP is  +nan.0  avoid prepending  an additional  positive
       ;;sign.
       ((and (flonum? x.imp)
	     ($flnan? x.imp))
	"+nan.0")
       ;;If we are here X.IMP is exact zero or positive.
       ;;
       (else
	(string-append "+" ($number->string x.imp radix)))))

    (define-argument-validation (radix10 who obj)
      (eqv? obj 10)
      (assertion-violation who "invalid radix for inexact number, expected 10" obj))

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
	      (let loop ((i 0) (j 0) (k 0) (b 0))
		(cond (($fx= i chars)
		       s)
		      (($fx< k 8)
		       (loop i ($fxadd1 j) ($fx+ k 8)
			  ($fxlogor b ($fxsll ($bignum-byte-ref x j) k))))
		      (else
		       (string-set! s ($fx- n i)
				    (string-ref string-map
						($fxlogand mask b)))
		       (loop ($fxadd1 i) j ($fx- k shift) ($fxsra b shift)))))))))

      #| end of module: bignum->power-string |# )

    #| end of module: bignum->string |# )

  (define (ratnum->string x r)
    (string-append ($number->string ($ratnum-n x) r)
		   "/"
		   ($number->string ($ratnum-d x) r)))

  #| end of module: number->string |# )


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

  (define-ordered-comparison < $fx<
    fxbn< bnfx< bnbn< fxfl<
    flfx< bnfl< flbn< flfl<
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


;;;; comparisons between ratnum and exact integer
;;
;;Notice that a ratnum can never be equal to an exact integer.
;;

(define (exrt< x y)
  ;;Comparison: exact-integer < ratnum.
  ;;
  ;;     y.num
  ;; x < -----   <=>   x * y.den < y.num
  ;;     y.den
  ;;
  (< (* x ($ratnum-d y))
     ($ratnum-n y)))

(define (fxrt< x y)
  ;;Comparison: fixnum < ratnum.
  ;;
  ;;     y.num
  ;; x < -----   <=>   x * y.den < y.num
  ;;     y.den
  ;;
  (< ($mul-fixnum-number x ($ratnum-d y))
     ($ratnum-n y)))

(define (bnrt< x y)
  ;;Comparison: bignum < ratnum.
  ;;
  ;;     y.num
  ;; x < -----   <=>   x * y.den < y.num
  ;;     y.den
  ;;
  (< ($mul-bignum-number x ($ratnum-d y))
     ($ratnum-n y)))

;;; --------------------------------------------------------------------

(define (rtex< x y)
  ;;Comparison: ratnum < exact-integer.
  ;;
  ;; x.num
  ;; ----- < y   <=>   x.num < y * x.den
  ;; x.den
  ;;
  (< ($ratnum-n x)
     (* y ($ratnum-d x))))

(define (rtfx< x y)
  ;;Comparison: ratnum < fixnum.
  ;;
  ;; x.num
  ;; ----- < y   <=>   x.num < y * x.den
  ;; x.den
  ;;
  (< ($ratnum-n x)
     ($mul-fixnum-number y ($ratnum-d x))))

(define (rtbn< x y)
  ;;Comparison: ratnum < bignum.
  ;;
  ;; x.num
  ;; ----- < y   <=>   x.num < y * x.den
  ;; x.den
  ;;
  (< ($ratnum-n x)
     ($mul-bignum-number y ($ratnum-d x))))

;;; --------------------------------------------------------------------

(define (exrt> x y)
  ;;Comparison: exact-integer > ratnum.
  ;;
  ;;     y.num
  ;; x > -----   <=>   x * y.den > y.num
  ;;     y.den
  ;;
  (> (* x ($ratnum-d y))
     ($ratnum-n y)))

(define (fxrt> x y)
  ;;Comparison: fixnum > ratnum.
  ;;
  ;;     y.num
  ;; x > -----   <=>   x * y.den > y.num
  ;;     y.den
  ;;
  (> ($mul-fixnum-number x ($ratnum-d y))
     ($ratnum-n y)))

(define (bnrt> x y)
  ;;Comparison: bignum > ratnum.
  ;;
  ;;     y.num
  ;; x > -----   <=>   x * y.den > y.num
  ;;     y.den
  ;;
  (> ($mul-bignum-number x ($ratnum-d y))
     ($ratnum-n y)))

;;; --------------------------------------------------------------------

(define (rtex> x y)
  ;;Comparison: ratnum > exact-integer.
  ;;
  ;; x.num
  ;; ----- > y   <=>   x.num > y * x.den
  ;; x.den
  ;;
  (> ($ratnum-n x)
     (* y ($ratnum-d x))))

(define (rtfx> x y)
  ;;Comparison: ratnum > fixnum.
  ;;
  ;; x.num
  ;; ----- > y   <=>   x.num > y * x.den
  ;; x.den
  ;;
  (> ($ratnum-n x)
     ($mul-fixnum-number y ($ratnum-d x))))

(define (rtbn> x y)
  ;;Comparison: ratnum > bignum.
  ;;
  ;; x.num
  ;; ----- > y   <=>   x.num > y * x.den
  ;; x.den
  ;;
  (> ($ratnum-n x)
     ($mul-bignum-number y ($ratnum-d x))))

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

(module (non-positive?
	 fxnonpositive?
	 flnonpositive?
	 $fxnonpositive?
	 $bignum-non-positive?
	 $flnonpositive?
	 $ratnum-non-positive?)
  (define who 'non-positive?)

  (define (non-positive? x)
    (cond-real-numeric-operand x
      ((fixnum?)	($fxnonpositive? x))
      ((bignum?)	($bignum-non-positive? x))
      ((flonum?)	($flnonpositive? x))
      ((ratnum?)	($ratnum-non-positive? x))
      (else
       (%error-not-real-number x))))

  (define ($fxnonpositive? x)
    (or ($fxzero? x)
	($fxnegative? x)))

  (define $bignum-non-positive? $bignum-negative?)

  (define ($flnonpositive? x)
    (or ($flzero?/negative x)
	($flnegative? x)))

  (define ($ratnum-non-positive? x)
    ;;The denominator of a ratnum is always strictly positive.
    (non-positive? ($ratnum-n x)))

  (define (fxnonpositive? x)
    (define who 'fxnonpositive?)
    (with-arguments-validation (who)
	((fixnum	x))
      ($fxnonpositive? x)))

  (define (flnonpositive? x)
    (define who 'flnonpositive?)
    (with-arguments-validation (who)
	((flonum	x))
      ($flnonpositive? x)))

  #| end of module: non-positive? |# )

(module (non-negative?
	 fxnonnegative?
	 flnonnegative?
	 $fxnonnegative?
	 $bignum-non-negative?
	 $flnonnegative?
	 $ratnum-non-negative?)
  (define who 'non-negative?)

  (define (non-negative? x)
    (cond-real-numeric-operand x
      ((fixnum?)	($fxnonnegative? x))
      ((bignum?)	($bignum-non-negative? x))
      ((flonum?)	($flnonnegative? x))
      ((ratnum?)	($ratnum-non-negative? x))
      (else
       (%error-not-real-number x))))

  (define ($fxnonnegative? x)
    (or ($fxzero? x)
	($fxpositive? x)))

  (define $bignum-non-negative? $bignum-positive?)

  (define ($flnonnegative? x)
    (or ($flzero?/positive x)
	($flpositive? x)))

  (define ($ratnum-non-negative? x)
    ;;The denominator of a ratnum is always strictly positive.
    (non-negative? ($ratnum-n x)))


  (define (fxnonnegative? x)
    (define who 'fxnonnegative?)
    (with-arguments-validation (who)
	((fixnum	x))
      ($fxnonnegative? x)))

  (define (flnonnegative? x)
    (define who 'flnonnegative?)
    (with-arguments-validation (who)
	((flonum	x))
      ($flnonnegative? x)))

  #| end of module: non-negative? |# )

(define (even? x)
  (cond-inexact-integer-operand x
    ((fixnum?)	($fxeven? x))
    ((bignum?)	($bignum-even? x))
    ((flonum?)	($fleven? x))
    (else
     (assertion-violation 'even? "expected integer as argument" x))))

(define (odd? x)
  (cond-inexact-integer-operand x
    ((fixnum?)	($fxodd? x))
    ((bignum?)	($bignum-odd? x))
    ((flonum?)	($flodd? x))
    (else
     (assertion-violation 'odd? "expected integer as argument" x))))


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
	   ($make-rectangular (log ($neg-fixnum x))
			      greek-pi))))

  (define ($log-bignum x)
    (if ($bignum-positive? x)
	(let ((v (log (inexact x))))
	  (if (infinite? v)
	      (receive (s r)
		  (exact-integer-sqrt x)
		;;Could the (dropped) residual ever affect the answer?
		(* 2 (log s)))
	    v))
      ($make-rectangular (log ($neg-bignum x)) greek-pi)))

  (define ($log-ratnum x)
    ($sub-number-number (log ($ratnum-n x))
			(log ($ratnum-d x))))

  (define ($log-flonum x)
    (cond ((nan? x)
	   +nan.0)
	  ((or ($flpositive? x)
	       ($flzero?/positive x))
	   (foreign-call "ikrt_fl_log" x))
	  (else
	   ($make-cflonum ($fllog ($fl- x)) greek-pi))))

  (define ($log-compnum x)
    ;;
    ;;         log (x.rep^2 + x.imp^2)
    ;; log x = ----------------------- + i * atan(x.imp, x.rep)
    ;;                    2
    ;;
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      (if (zero? x.imp)
	  (let ((y (log x.rep)))
	    (if (real? y)
		($make-rectangular y 0.0)
	      (if (flonum? x.imp)
		  (if ($flzero?/positive x.imp)
		      y
		    ;;Return the conjugate of Y.
		    (let ((y.rep (real-part y))
			  (y.imp (imag-part y)))
		      ($make-rectangular y.rep (- y.imp))))
		;;Here X.IMP is exact zero.
		y)))
	($make-rectangular (/ (log (+ (square x.rep) (square x.imp)))
			      2)
			   (atan x.imp x.rep)))))

  (define ($log-cflonum x)
    ;;
    ;;         log (x.rep^2 + x.imp^2)
    ;; log x = ----------------------- + i * atan(x.imp, x.rep)
    ;;                    2
    ;;
    ;;when x.imp == 0:
    ;;
    ;;         log (x.rep^2)
    ;; log x = ------------- + i * atan(0, x.rep) = log (x.rep^(2/2)) = log (x.rep)
    ;;               2
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum ($fl/ ($fllog ($fl+ ($flsquare x.rep)
					 ($flsquare x.imp)))
			   2.0)
		     ($flatan2 x.imp x.rep))))

  #| end of module |# )


(module (exp
	 $exp-fixnum		$exp-bignum		$exp-ratnum
	 $exp-compnum		$exp-cflonum)
  (define who 'exp)

  (define (exp x)
    (cond-numeric-operand x
      ((flonum?)	($exp-flonum  x))
      ((cflonum?)	($exp-cflonum x))
      ((fixnum?)	($exp-fixnum  x))
      ((bignum?)	($exp-bignum  x))
      ((ratnum?)	($exp-ratnum  x))
      ((compnum?)	($exp-compnum x))
      (else
       (%error-not-number x))))

;;; --------------------------------------------------------------------

  (define ($exp-fixnum x)
    (if ($fxzero? x)
	1
      ($exp-flonum (fixnum->flonum x))))

  (define ($exp-bignum x)
    ($exp-flonum ($bignum->flonum x)))

  (define ($exp-ratnum x)
    ($exp-flonum ($ratnum->flonum x)))

  (define ($exp-compnum x)
    ;;
    ;;   e^x = e^(x.rep + x.imp i) = e^x.rep cos(x.imp) + e^x.rep sin(x.imp) i
    ;;
    (let* ((x.rep	($compnum-real x))
	   (x.imp	($compnum-imag x))
	   (e^x.rep	(exp x.rep)))
      (if (zero? x.imp)
	  ;;Here we are dealing with compnum X; so if the imaginary part
	  ;;is zero, it is inexact zero.   It follows that the real part
	  ;;is not a flonum, else the number X would be a cflonum.
	  ;;
	  ;;If the real part is not  a flonum: it cannot be infinity nor
	  ;;NaN, so we can short  circuit here.  See the computation for
	  ;;cflonums for more details.
	  (if (flonum? e^x.rep)
	      ($make-cflonum e^x.rep 0.0)
	    ($make-rectangular e^x.rep 0.0))
	($make-rectangular (* e^x.rep (cos x.imp))
			   (* e^x.rep (sin x.imp))))))

  (define ($exp-cflonum x)
    ;;In general:
    ;;
    ;;   e^x = e^(x.rep + i * x.imp)
    ;;       = e^x.rep * cos(x.imp) + i * e^x.rep * sin(x.imp)
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
	   (e^x.rep	($exp-flonum x.rep)))
      ;;We short circuit  the case of zero imaginary part,  but look out
      ;;for the infinite  real part; in the latter, case  in the formula
      ;;below:
      ;;
      ;;   e^x.rep * cos (x.imp) = +inf.0 * 1.0 = +inf.0
      ;;   e^x.rep * sin (x.imp) = +inf.0 * 0.0 = +nan.0
      ;;
      (if (and ($flzero? x.imp)
	       (not ($flinfinite? x.rep))
	       (not ($flnan?      x.rep)))
	  ($make-cflonum e^x.rep (if ($flzero?/positive x.imp) +0.0 -0.0))
	($make-cflonum ($fl* e^x.rep ($cos-flonum x.imp))
		       ($fl* e^x.rep ($sin-flonum x.imp))))))

  #| end of module: exp |# )


(module (sin
	 $sin-fixnum		$sin-bignum		$sin-ratnum
	 $sin-cflonum		$sin-compnum)
  (define who 'sin)

  (define (sin x)
    (cond-numeric-operand x
      ((flonum?)	($sin-flonum x))
      ((cflonum?)	($sin-cflonum x))
      ((fixnum?)	($sin-fixnum  x))
      ((bignum?)	($sin-bignum  x))
      ((ratnum?)	($sin-ratnum  x))
      ((compnum?)	($sin-compnum x))
      (else
       (%error-not-number x))))

  (define ($sin-fixnum x)
    (if ($fxzero? x)
	0
      (foreign-call "ikrt_fx_sin" x)))

  (define ($sin-bignum x)
    ($sin-flonum ($bignum->flonum x)))

  (define ($sin-ratnum x)
    ($sin-flonum ($ratnum->flonum x)))

;;; --------------------------------------------------------------------
;;; Complex arguments

  ;;For the real part of a complex number Z we notice that:
  ;;
  ;;   Z + conj(Z) = (A+iB) + (A-iB) = (A+A) + i (B-B) = 2A
  ;;
  ;;and:
  ;;
  ;;   conj(sin(X+iY)) = sin(conj(X+iY)) = sin(X-iY)
  ;;
  ;;so:
  ;;
  ;;   2 Rep{sin(X+iY)} = sin(X+iY) + sin(X-iY)
  ;;
  ;;by definition:
  ;;
  ;;               exp[i(X+iY)] - exp[-i(X+iY)]   exp(iX)exp(-Y) - exp(-iX)exp(+Y)
  ;;   sin(X+iY) = ---------------------------- = --------------------------------
  ;;                           2i                               2i
  ;;
  ;;               exp[i(X-iY)] - exp[-i(X-iY)]   exp(iX)exp(+Y) - exp(-iX)exp(-Y)
  ;;   sin(X-iY) = ---------------------------- = --------------------------------
  ;;                           2i                               2i
  ;;
  ;;so:
  ;;
  ;;   4i Rep{sin(X+iY)} =
  ;;     = exp(iX)exp(-Y) - exp(-iX)exp(+Y) + exp(iX)exp(+Y) - exp(-iX)exp(-Y)
  ;;
  ;;We notice also that:
  ;;
  ;;             exp(iX) - exp(-iX)                exp(Y) + exp(-Y)
  ;;   sin(X)  = ------------------      cosh(Y) = ----------------
  ;;                    2i                                2
  ;;
  ;;so:
  ;;
  ;;   4i sin(X) cosh(Y) = [exp(iX) - exp(-iX)] [exp(+Y) + exp(-Y)]
  ;;     = exp(iX)exp(+Y) - exp(-iX)exp(+Y) + exp(iX)exp(-Y) - exp(-iX)exp(-Y)
  ;;     = exp(iX)exp(-Y) - exp(-iX)exp(+Y) + exp(iX)exp(+Y) - exp(-iX)exp(-Y)
  ;;
  ;;and finally:
  ;;
  ;;   4i Rep{sin(X+iY)} = 4i sin(X) cosh(Y)  =>  Rep{sin(X+iY)} = sin(X) cosh(Y)
  ;;

  ;;For the imaginary part of a complex number Z we notice that:
  ;;
  ;;   Z - conj(Z) = (A+iB) - (A-iB) = (A-A) + iB - (-iB) = iB + iB = 2iB
  ;;
  ;;and:
  ;;
  ;;   conj(sin(X+iY)) = sin(conj(X+iY)) = sin(X-iY)
  ;;
  ;;so:
  ;;
  ;;   2i Imp{sin(X+iY)} = sin(X+iY) - sin(X-iY)
  ;;
  ;;by definition:
  ;;
  ;;               exp[i(X+iY)] - exp[-i(X+iY)]   exp(iX)exp(-Y) - exp(-iX)exp(+Y)
  ;;   sin(X+iY) = ---------------------------- = --------------------------------
  ;;                           2i                               2i
  ;;
  ;;               exp[i(X-iY)] - exp[-i(X-iY)]   exp(iX)exp(+Y) - exp(-iX)exp(-Y)
  ;;   sin(X-iY) = ---------------------------- = --------------------------------
  ;;                           2i                               2i
  ;;
  ;;so, given that 2i * 2i = -4:
  ;;
  ;;   -4 Imp{sin(X+iY)} =
  ;;     = exp(iX)exp(-Y) - exp(-iX)exp(+Y) - exp(iX)exp(+Y) + exp(-iX)exp(-Y)
  ;;
  ;;We notice also that:
  ;;
  ;;            exp(iX) + exp(-iX)                exp(Y) - exp(-Y)
  ;;   cos(X) = ------------------      sinh(Y) = ----------------
  ;;                    2                                2
  ;;
  ;;so:
  ;;
  ;;   4 cos(X) sinh(Y) = [exp(iX) + exp(-iX)] [exp(Y) - exp(-Y)]
  ;;     =   exp(iX)exp(+Y) + exp(-iX)exp(+Y) - exp(iX)exp(-Y) - exp(-iX)exp(-Y)
  ;;     = - exp(iX)exp(-Y) + exp(-iX)exp(+Y) + exp(iX)exp(+Y) - exp(-iX)exp(-Y)
  ;;
  ;;   -4 cos(X) sinh(Y) =
  ;;     =   exp(iX)exp(-Y) - exp(-iX)exp(+Y) - exp(iX)exp(+Y) + exp(-iX)exp(-Y)
  ;;
  ;;and finally:
  ;;
  ;;   -4 Imp{sin(X+iY)} = -4 cos(X) sinh(Y)  =>  Imp{sin(X+iY)} = cos(X) sinh(Y)
  ;;

  (define ($sin-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular (* (sin x.rep) (cosh x.imp))
			 (* (cos x.rep) (sinh x.imp)))))

  (define ($sin-cflonum X)
    (let ((X.rep ($cflonum-real X))
	  (X.imp ($cflonum-imag X)))
      ($make-cflonum ($fl* ($sin-flonum X.rep) ($flcosh X.imp))
		     ($fl* ($cos-flonum X.rep) ($flsinh X.imp)))))

  #| end of module: sin |# )


(module (cos
	 $cos-fixnum		$cos-bignum		$cos-ratnum
	 $cos-cflonum		$cos-compnum)
  (define who 'cos)

  (define (cos x)
    (cond-numeric-operand x
      ((flonum?)	($cos-flonum x))
      ((cflonum?)	($cos-cflonum x))
      ((fixnum?)	($cos-fixnum  x))
      ((bignum?)	($cos-bignum  x))
      ((ratnum?)	($cos-ratnum  x))
      ((compnum?)	($cos-compnum x))
      (else
       (%error-not-number x))))

  (define ($cos-fixnum x)
    (if ($fxzero? x)
	1
      (foreign-call "ikrt_fx_cos" x)))

  (define ($cos-bignum x)
    ($cos-flonum (inexact x)))

  (define ($cos-ratnum x)
    ($cos-flonum (inexact x)))

;;; --------------------------------------------------------------------
;;; Complex arguments

  ;;For the real part of a complex number Z we notice that:
  ;;
  ;;   Z + conj(Z) = (A+iB) + (A-iB) = (A+A) + i (B-B) = 2A
  ;;
  ;;and:
  ;;
  ;;   conj(cos(X+iY)) = cos(conj(X+iY)) = cos(X-iY)
  ;;
  ;;so:
  ;;
  ;;   2 Rep{cos(X+iY)} = cos(X+iY) + cos(X-iY)
  ;;
  ;;by definition:
  ;;
  ;;               exp[i(X+iY)] + exp[-i(X+iY)]   exp(iX)exp(-Y) + exp(-iX)exp(+Y)
  ;;   cos(X+iY) = ---------------------------- = --------------------------------
  ;;                            2                                2
  ;;
  ;;               exp[i(X-iY)] + exp[-i(X-iY)]   exp(iX)exp(+Y) + exp(-iX)exp(-Y)
  ;;   cos(X-iY) = ---------------------------- = --------------------------------
  ;;                            2                                2
  ;;
  ;;so:
  ;;
  ;;   2 Rep{cos(X+iY)} =
  ;;     = exp(iX)exp(-Y) + exp(-iX)exp(+Y) + exp(iX)exp(+Y) + exp(-iX)exp(-Y)
  ;;
  ;;We notice also that:
  ;;
  ;;             exp(iX) + exp(-iX)                exp(Y) + exp(-Y)
  ;;   cos(X)  = ------------------      cosh(Y) = ----------------
  ;;                     2                                2
  ;;
  ;;so:
  ;;
  ;;   4 cos(X) cosh(Y) = [exp(iX) + exp(-iX)] [exp(Y) + exp(-Y)]
  ;;     = exp(iX)exp(+Y) + exp(-iX)exp(+Y) + exp(iX)exp(-Y) + exp(-iX)exp(-Y)
  ;;     = exp(iX)exp(-Y) + exp(-iX)exp(+Y) + exp(iX)exp(+Y) + exp(-iX)exp(-Y)
  ;;
  ;;and finally:
  ;;
  ;;   4 Rep{cos(X+iY)} = 4 cos(X) cosh(Y)  =>  Rep{cos(X+iY)} = cos(X) cosh(Y)
  ;;

  ;;For the imaginary part of a complex number Z we notice that:
  ;;
  ;;   Z - conj(Z) = (A+iB) - (A-iB) = (A-A) + iB - (-iB) = iB + iB = 2iB
  ;;
  ;;and:
  ;;
  ;;   conj(cos(X+iY)) = cos(conj(X+iY)) = cos(X-iY)
  ;;
  ;;so:
  ;;
  ;;   2i Imp{cos(X+iY)} = cos(X+iY) - cos(X-iY)
  ;;
  ;;by definition:
  ;;
  ;;               exp[i(X+iY)] + exp[-i(X+iY)]   exp(iX)exp(-Y) + exp(-iX)exp(+Y)
  ;;   cos(X+iY) = ---------------------------- = --------------------------------
  ;;                            2                                2
  ;;
  ;;               exp[i(X-iY)] + exp[-i(X-iY)]   exp(iX)exp(+Y) + exp(-iX)exp(-Y)
  ;;   cos(X-iY) = ---------------------------- = --------------------------------
  ;;                            2                                2
  ;;
  ;;so:
  ;;
  ;;   4i Imp{cos(X+iY)} =
  ;;     = exp(iX)exp(-Y) + exp(-iX)exp(+Y) - exp(iX)exp(+Y) - exp(-iX)exp(-Y)
  ;;
  ;;We notice also that:
  ;;
  ;;            exp(iX) - exp(-iX)                exp(Y) - exp(-Y)
  ;;   sin(X) = ------------------      sinh(Y) = ----------------
  ;;                   2i                                2
  ;;
  ;;so:
  ;;
  ;;   4i sin(X) sinh(Y) = [exp(iX) - exp(-iX)] [exp(+Y) - exp(-Y)]
  ;;     =   exp(iX)exp(+Y) - exp(-iX)exp(+Y) - exp(iX)exp(-Y) + exp(-iX)exp(-Y)
  ;;     = - exp(iX)exp(-Y) - exp(-iX)exp(+Y) + exp(iX)exp(+Y) + exp(-iX)exp(-Y)
  ;;
  ;;   -4i sin(X) sinh(Y) =
  ;;     =   exp(iX)exp(-Y) + exp(-iX)exp(+Y) - exp(iX)exp(+Y) - exp(-iX)exp(-Y)
  ;;
  ;;and finally:
  ;;
  ;;   4i Imp{cos(X+iY)} = -4i sin(X) sinh(Y)  =>  Imp{cos(X+iY)} = - sin(X) sinh(Y)
  ;;
  (define ($cos-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular    (* (cos x.rep) (cosh x.imp))
			 (- (* (sin x.rep) (sinh x.imp))))))

  (define ($cos-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      ($make-cflonum       ($fl* ($cos-flonum x.rep) ($flcosh x.imp))
		     ($fl- ($fl* ($sin-flonum x.rep) ($flsinh x.imp))))))

  #| end of module: cos |# )


(module (tan
	 $tan-fixnum		$tan-bignum		$tan-ratnum
	 $tan-compnum		$tan-cflonum)
  (define who 'tan)

  (define (tan x)
    (cond-numeric-operand x
      ((flonum?)	($tan-flonum  x))
      ((cflonum?)	($tan-cflonum x))
      ((fixnum?)	($tan-fixnum  x))
      ((bignum?)	($tan-bignum  x))
      ((ratnum?)	($tan-ratnum  x))
      ((compnum?)	($tan-compnum x))
      (else
       (%error-not-number x))))

  (define ($tan-fixnum x)
    (if ($fxzero? x)
	0
      (foreign-call "ikrt_fx_tan" x)))

  (define ($tan-bignum x)
    ($tan-flonum ($bignum->flonum x)))

  (define ($tan-ratnum x)
    ($tan-flonum ($ratnum->flonum x)))

;;; --------------------------------------------------------------------
;;; complex argument

  ;;Verified with WolframAlpha:
  ;;
  ;;   tan (x.rep + i * x.imp) = z.rep + i * z.imp
  ;;
  ;;                    sin (2 * x.rep)
  ;;   z.rep = ----------------------------------
  ;;           cos (2 * x.rep) + cosh (2 * x.imp)
  ;;
  ;;                   sinh (2 * x.imp)
  ;;   z.imp = ----------------------------------
  ;;           cos (2 * x.rep) + cosh (2 * x.imp)
  ;;
  ;;alternatively the following formula avoids  generating a NaN in some
  ;;cases:
  ;;
  ;;             tanh (2 * x.imp)
  ;;   z.imp = --------------------
  ;;                cos (2 * x.rep)
  ;;           1 + ----------------
  ;;               cosh (2 * x.imp)
  ;;
  ;;Using the alternative formula for  Z.IMP is slower because we cannot
  ;;share the denominator with Z.REP.
  ;;

  (define ($tan-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      (let ((R2  ($mul-fixnum-number 2 x.rep))
	    (I2  ($mul-fixnum-number 2 x.imp)))
	(if (and (zero?     x.rep)
		 (infinite? x.imp))
	    ;;Avoid  generating  a  NaN  in   this  case  by  using  the
	    ;;alternative formula for the imaginary part.
	    (let ((A (cos  R2))
		  (B (cosh I2)))
	      ($make-rectangular (/ (sin R2)  (+ A B))
				 (/ (tanh I2) (+ 1 (/ A B)))))
	  ;;This is faster.
	  (let ((den (+ (cos R2) (cosh I2))))
	    ($make-rectangular (/ (sin  R2) den)
			       (/ (sinh I2) den)))))))

  ;;Below is an alternative version.
  ;;
  ;;(define ($tan-compnum x)
  ;;  (let ((x.rep ($compnum-real x))
  ;;        (x.imp ($compnum-imag x)))
  ;;    (let ((R2 (* 2 x.rep))
  ;;          (I2 (* 2 x.imp)))
  ;;      (let ((CR2  (cos  R2))
  ;;            (CHI2 (cosh I2)))
  ;;        ($make-rectangular (/ (sin R2)  (+ CR2 CHI2))
  ;;                           (/ (tanh I2) (+ 1 (/ CR2 CHI2))))))))

  (define ($tan-cflonum x)
    (let ((x.rep ($cflonum-real x))
  	  (x.imp ($cflonum-imag x)))
      (let ((R2  ($fl* 2.0 x.rep))
	    (I2  ($fl* 2.0 x.imp)))
	(if (and ($flzero?     x.rep)
		 ($flinfinite? x.imp))
	    ;;Avoid  generating  a  NaN  in   this  case  by  using  the
	    ;;alternative formula for the imaginary part.
	    (let ((A ($cos-flonum R2))
		  (B ($flcosh I2)))
	      ($make-cflonum ($fl/ ($sin-flonum  R2) ($fl+ A B))
			     ($fl/ ($fltanh I2) ($fl+ 1.0 ($fl/ A B)))))
	  ;;This is faster.
	  (let ((den ($fl+ ($cos-flonum R2) ($flcosh I2))))
	    ($make-cflonum ($fl/ ($sin-flonum  R2) den)
			   ($fl/ ($flsinh      I2) den)))))))

  ;;Below is an alternative version.
  ;;
  ;; (define ($tan-cflonum x)
  ;;   (let ((x.rep ($cflonum-real x))
  ;;         (x.imp ($cflonum-imag x)))
  ;;     (let ((R2 ($fl* 2.0 x.rep))
  ;;           (I2 ($fl* 2.0 x.imp)))
  ;;       (let ((COSR2    ($cos-flonum R2))
  ;;             (COSHI2   ($flcosh I2)))
  ;;         ($make-cflonum ($fl/ ($sin-flonum  R2) ($fl+ COSR2 COSHI2))
  ;;                        ($fl/ ($fltanh I2) ($fl+ 1.0 ($fl/ COSR2 COSHI2))))))))

  #| end of module: tan |# )


(module (asin
	 $asin-fixnum		$asin-bignum		$asin-ratnum
	 $asin-flonum		$asin-cflonum		$asin-compnum)
  (define who 'asin)

  (define (asin x)
    (cond-numeric-operand x
      ((flonum?)	($asin-flonum  x))
      ((cflonum?)	($asin-cflonum x))
      ((fixnum?)	($asin-fixnum  x))
      ((bignum?)	($asin-bignum  x))
      ((ratnum?)	($asin-ratnum  x))
      ((compnum?)	($asin-compnum x))
      (else
       (%error-not-number x))))

  (define ($asin-fixnum x)
    ($asin-flonum ($fixnum->flonum x)))

  (define ($asin-bignum x)
    ($asin-flonum ($bignum->flonum x)))

  (define ($asin-ratnum x)
    ($asin-flonum ($ratnum->flonum x)))

  (define ($asin-flonum x)
    ;;This is  different from $FLASIN:  $FLASIN returns a  flonum, while
    ;;$ASIN-FLONUM accepts any  flonum as argument and  returns a flonum
    ;;or cflonum.
    ;;
    (cond (($fl> x 1.0)
	   ;;          pi
	   ;; asin x = -- - i * acosh x
	   ;;          2
	   ($make-cflonum greek-pi/2 ($fl- ($acosh-flonum x))))
	  (($fl< x -1.0)
	   ;;            pi
	   ;; asin x = - -- + i * acosh (- x)
	   ;;            2
	   ($make-cflonum ($fl- greek-pi/2) ($acosh-flonum ($fl- x))))
	  (else
	   ($flasin x))))

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
	       (A	(+ (square x.rep) (square x.imp)))
	       ;;B is a real number.
	       (B	(- A 1.0))
	       ;;C is a non-negative real number.
	       (C	(square B))
	       ;;D is a non-negative real number.
	       (D	(square x.imp))
	       ;;Q is a non-negative real number.
	       (Q	(sqrt (+ C (* 4 D)))))
	  (define (sgn N)
	    (if (negative? N) -1.0 1.0))
	  ($make-rectangular ($mul-flonum-number ($fl* 0.5 (sgn x.rep)) (acos  (- Q A)))
			     ($mul-flonum-number ($fl* 0.5 (sgn x.imp)) (acosh (+ Q A))))))))

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
	       (A	($fl+ ($flsquare x.rep) ($flsquare x.imp)))
	       ;;B is a flonum.
	       (B	($fl- A 1.0))
	       ;;C is a non-negative flonum.
	       (C	($flsquare B))
	       ;;D is a non-negative flonum.
	       (D	($flsquare x.imp))
	       ;;Q is a non-negative flonum.
	       (Q	($sqrt-flonum ($fl+ C ($fl* 4.0 D)))))
	  ($make-cflonum ($fl* ($fl* 0.5 ($sign-flonum x.rep)) ($acos-flonum  ($fl- Q A)))
			 ($fl* ($fl* 0.5 ($sign-flonum x.imp)) ($acosh-flonum ($fl+ Q A))))))))

  #| end of module: asin |# )


(module (acos
	 $acos-fixnum		$acos-bignum		$acos-ratnum
	 $acos-flonum		$acos-cflonum		$acos-compnum)
  (define who 'acos)

  (define (acos x)
    (cond-numeric-operand x
      ((flonum?)	($acos-flonum  x))
      ((cflonum?)	($acos-cflonum x))
      ((fixnum?)	($acos-fixnum  x))
      ((bignum?)	($acos-bignum  x))
      ((ratnum?)	($acos-ratnum  x))
      ((compnum?)	($acos-compnum x))
      (else
       (%error-not-number x))))

  (define ($acos-fixnum x)
    ($acos-flonum ($fixnum->flonum x)))

  (define ($acos-bignum x)
    ($acos-flonum ($bignum->flonum x)))

  (define ($acos-ratnum x)
    ($acos-flonum ($ratnum->flonum x)))

  (define ($acos-flonum x)
    ;;This is  different from $FLACOS:  $FLACOS returns a  flonum, while
    ;;$ACOS-FLONUM accepts any flonum as  argument and returns a flonum,
    ;;compnum or cflonum.
    ;;
    (cond (($fl> x 1.0)
	   ($make-compnum 0 ($acosh-flonum x)))
	  (($fl< x -1.0)
	   ($make-cflonum greek-pi ($fl- ($acosh-flonum ($fl- x)))))
	  (else
	   ($flacos x))))

  (define ($acos-compnum x)
    (let* ((y     ($asin-compnum x))
	   (y.rep (real-part y))
	   (y.imp (imag-part y)))
      ($make-rectangular ($sub-flonum-number greek-pi/2 y.rep)
			 ($neg-number y.imp))))

  (define ($acos-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (if (and ($flzero?     x.imp)
	       ($flinfinite? x.rep))
	  ;;We want to avoid generating a NaN in this case.
	  (if ($flpositive? x.rep)
	      (if ($flzero?/positive x.imp)
		  +0.0-inf.0i
		+0.0+inf.0i)
	    (if ($flzero?/positive x.imp)
		($make-cflonum greek-pi -inf.0)
	      ($make-cflonum greek-pi +inf.0)))
	(let* ((y     ($asin-cflonum x))
	       (y.rep ($cflonum-real y))
	       (y.imp ($cflonum-imag y)))
	  ($make-cflonum ($fl- greek-pi/2 y.rep) ($fl- y.imp))))))

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
	((flonum?)	($atan-flonum  x))
	((cflonum?)	($atan-cflonum x))
	((fixnum?)	($atan-fixnum  x))
	((bignum?)	($atan-bignum  x))
	((ratnum?)	($atan-ratnum  x))
	((compnum?)	($atan-compnum x))
	(else
	 (%error-not-number x))))
     ((imp rep)
      (with-arguments-validation (who)
	  ((real	imp)
	   (real	rep))
	($atan2-real-real imp rep)))))

  (define ($atan2-real-real imp rep)
    (foreign-call "ikrt_atan2" (inexact imp) (inexact rep)))

;;; --------------------------------------------------------------------

  (define ($atan-fixnum x)
    (foreign-call "ikrt_fx_atan" x))

  (define ($atan-bignum x)
    ($atan-flonum ($bignum->flonum x)))

  (define ($atan-ratnum x)
    ($atan-flonum ($ratnum->flonum x)))

;;; --------------------------------------------------------------------
;;; complex argument

  ;;Formula from Wikipedia section "Logarithmic forms":
  ;;
  ;;	<http://en.wikipedia.org/wiki/Arc_tangent>
  ;;
  ;;and also from the R6RS document:
  ;;
  ;;  atan x = 1/2 * i * (log (1 - i * x) - log (1 + i * x))
  ;;
  ;;  i * x = i * (x.rep + i * x.imp) = i * x.rep - x.imp
  ;;
  ;;  A = 1 - i * x = 1 - (- x.imp + i * x.rep) = (1 + x.imp) - i * x.rep
  ;;  B = 1 + i * x = 1 + (- x.imp + i * x.rep) = (1 - x.imp) + i * x.rep
  ;;  C = log A - log B
  ;;  D = 1/2 * i C = 0.5i * (C.rep + i * C.imp)
  ;;                = 0.5 * C.rep * i - 0.5 * C.imp
  ;;                = (-0.5 * C.imp) + i * (0.5 * C.rep)
  ;;

  (define ($atan-compnum x)
    ($atan-cflonum ($compnum->cflonum x)))

  (define ($atan-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (let* ((A ($make-cflonum ($fl+ 1.0 x.imp) ($fl- x.rep)))
	     (B ($make-cflonum ($fl- 1.0 x.imp) x.rep))
	     (C ($sub-cflonum-cflonum ($log-cflonum A) ($log-cflonum B))))
	(let ((C.rep ($cflonum-real C))
	      (C.imp ($cflonum-imag C)))
	  ($make-cflonum ($fl* -0.5 C.imp)
			 ($fl* +0.5 C.rep))))))

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
       (%error-not-number x))))

  (define ($sinh-fixnum x)
    ($flsinh ($fixnum->flonum x)))

  (define ($sinh-bignum x)
    ($flsinh ($bignum->flonum x)))

  (define ($sinh-ratnum x)
    ($flsinh ($ratnum->flonum x)))

  (define ($sinh-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular (* (sinh x.rep) (cos x.imp))
			 (* (cosh x.rep) (sin x.imp)))))

  (define ($sinh-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (if ($flzero? x.imp)
	  ($make-cflonum ($flsinh x.rep) x.imp)
	($make-cflonum ($fl* ($flsinh x.rep) ($cos-flonum x.imp))
		       ($fl* ($flcosh x.rep) ($sin-flonum x.imp))))))

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
       (%error-not-number x))))

  (define ($cosh-fixnum x)
    ($flcosh ($fixnum->flonum x)))

  (define ($cosh-bignum x)
    ($flcosh ($bignum->flonum x)))

  (define ($cosh-ratnum x)
    ($flcosh ($ratnum->flonum x)))

  (define ($cosh-compnum x)
    (let ((x.rep ($compnum-real x))
	  (x.imp ($compnum-imag x)))
      ($make-rectangular (* (cosh x.rep) (cos x.imp))
			 (* (sinh x.rep) (sin x.imp)))))

  (define ($cosh-cflonum x)
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (if ($flzero? x.imp)
	  ($make-cflonum ($flcosh x.rep) x.imp)
	($make-cflonum ($fl* ($flcosh x.rep) ($cos-flonum x.imp))
		       ($fl* ($flsinh x.rep) ($sin-flonum x.imp))))))

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
       (%error-not-number x))))

  (define ($tanh-fixnum x)
    ($fltanh ($fixnum->flonum x)))

  (define ($tanh-bignum x)
    ($fltanh ($bignum->flonum x)))

  (define ($tanh-ratnum x)
    ($fltanh ($ratnum->flonum x)))

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
	(let ((cos2i  ($cos-flonum I2))
	      (cosh2r ($flcosh R2)))
	  ($make-cflonum ($fl/ ($fltanh R2) ($fl+ 1.0 ($fl/ cos2i cosh2r)))
			 ($fl/ ($sin-flonum I2)  ($fl+ cosh2r cos2i)))))))

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
       (%error-not-number x))))

  (define ($asinh-fixnum x)
    ($flasinh ($fixnum->flonum x)))

  (define ($asinh-bignum x)
    ($flasinh ($bignum->flonum x)))

  (define ($asinh-ratnum x)
    ($flasinh ($ratnum->flonum x)))

;;; --------------------------------------------------------------------
;;; complex argument

  ;;Everybody (including the GNU C Library) makes use of this formula:
  ;;
  ;;   (log (+ z (sqrt (+ 1 (square z)))))
  ;;
  ;;so:
  ;;
  ;;   S     = Z^2 = (X+iY)^2 = (X+iY) (X+iY) = X^2 - Y^2 + 2iXY
  ;;   S.rep = X^2 - Y^2
  ;;   S.imp = 2iXY
  ;;

  (define ($asinh-compnum z)
    (let ((z.rep ($compnum-real z))
	  (z.imp ($compnum-imag z)))
      (if (zero? z.imp)
	  ($make-rectangular (asinh z.rep) z.imp)
	(let* ((S.rep (- (square z.rep) (square z.imp)))
	       (S.imp (let ((t (* z.rep z.imp)))
			(+ t t)))
	       (Q     (sqrt ($make-rectangular (+ 1 S.rep) S.imp))))
	  (log ($add-compnum-number z Q))))))

  (define ($asinh-cflonum z)
    (let ((z.rep ($cflonum-real z))
	  (z.imp ($cflonum-imag z)))
      (if ($flzero? z.imp)
	  ($make-cflonum ($flasinh z.rep) z.imp)
	(let* ((S.rep ($fl- ($flsquare z.rep) ($flsquare z.imp)))
	       (S.imp (let ((t ($fl* z.rep z.imp)))
			($fl+ t t)))
	       (Q     ($sqrt-cflonum ($make-cflonum ($fl+ 1.0 S.rep) S.imp))))
	  ($log-cflonum ($add-cflonum-cflonum z Q))))))

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
       (%error-not-number x))))

  (define ($acosh-fixnum x)
    ($acosh-flonum ($fixnum->flonum x)))

  (define ($acosh-bignum x)
    ($acosh-flonum ($bignum->flonum x)))

  (define ($acosh-ratnum x)
    ($acosh-flonum ($ratnum->flonum x)))

  (define ($acosh-flonum x)
    (cond (($fl>= x +1.0) ; +1 <= X < +inf
	   ($flacosh x))
	  (($fl>= x -1.0) ; -1 <= X < +1
	   ($make-compnum 0 ($flatan2 ($flsqrt ($fl- 1.0 ($flsquare x))) x)))
	  (($fl< x -1.0) ; -inf < X < -1
	   ($make-cflonum ($flacosh ($fl- x)) greek-pi))
	  (else +nan.0)))

;;; --------------------------------------------------------------------
;;; complex arguments

  ;;From Wikipedia
  ;;
  ;;   (log (+ z (* (sqrt (+ z 1)) (sqrt (- z 1)))))
  ;;

  (define ($acosh-compnum z)
    (log (+ z (* (sqrt (+ z 1))
		 (sqrt (- z 1))))))

  (module ($acosh-cflonum)

    (define ($acosh-cflonum z)
      (let ((z.rep ($cflonum-real z))
	    (z.imp ($cflonum-imag z)))
	(if ($flzero? z.imp)
	    (%flacosh z.rep ($flzero?/positive z.imp))
	  (let ((A ($fl+ z.rep 1.0))
		(B ($fl- z.rep 1.0)))
	    (let ((C ($mul-cflonum-cflonum ($sqrt-cflonum ($make-cflonum A z.imp))
					   ($sqrt-cflonum ($make-cflonum B z.imp)))))
	      ($log-cflonum ($add-cflonum-cflonum z C)))))))

    (define (%flacosh x pos?)
      (cond (($fl>= x +1.0) ; +1 <= X < +inf
	     ($make-cflonum ($flacosh x) (if pos? +0.0 -0.0)))
	    (($fl>= x -1.0) ; -1 <= X < +1
	     (let ((t ($flatan2 ($flsqrt ($fl- 1.0 ($flsquare x))) x)))
	       ($make-cflonum 0.0 (if pos? t ($fl- t)))))
	    (($fl< x -1.0) ; -inf < X < -1
	     ($make-cflonum ($flacosh ($fl- x)) (if pos? greek-pi ($fl- greek-pi))))
	    (else
	     +nan.0+nan.0i)))

    #| end of module: $acosh-cflonum |# )

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
       (%error-not-number x))))

  (define ($atanh-fixnum x)
    ($atanh-flonum ($fixnum->flonum x)))

  (define ($atanh-bignum x)
    ($atanh-flonum ($bignum->flonum x)))

  (define ($atanh-ratnum x)
    ($atanh-flonum ($ratnum->flonum x)))

  (define ($atanh-flonum x)
    ;;This is different from $flatanh: it accepts any flonum as argument
    ;;and it returns a flonum or cflonum.
    ;;
    (if (and ($fl<= x +1.0)
	     ($fl>= x -1.0))
	($flatanh x)
      (let ((t ($log-flonum ($fl/ ($fl+ 1.0 x) ($fl- 1.0 x)))))
	(if (flonum? t)
	    ($mul-flonum-flonum 0.5 t)
	  ($mul-flonum-cflonum 0.5 t)))))

  (define ($atanh-compnum x)
    ($atanh-cflonum ($compnum->cflonum x)))

  (define ($atanh-cflonum x)
    ;;From Wikipedia:
    ;;
    ;;                    1 + Z
    ;;  atanh Z = 1/2 log -----
    ;;                    1 - Z
    ;;
    (let ((x.rep ($cflonum-real x))
	  (x.imp ($cflonum-imag x)))
      (if ($flzero? x.imp)
	  (let ((t ($atanh-flonum x.rep)))
	    (if (flonum? t)
		($make-cflonum t x.imp)
	      t))
	($mul-flonum-cflonum 0.5 ($log-cflonum
				  ($div-cflonum-cflonum ($add-flonum-cflonum 1.0 x)
							($sub-flonum-cflonum 1.0 x)))))))

  #| end of module: atanh |# )


(define (numerator x)
  (define who 'numerator)
  (cond-real-numeric-operand x
    ((fixnum?)		x)
    ((bignum?)		x)
    ((ratnum?)		($ratnum-n x))
    ((flonum?)		($flnumerator x))
    (else
     (%error-not-real-number x))))

(define (denominator x)
  (define who 'denominator)
  (cond-real-numeric-operand x
    ((fixnum?)		1)
    ((bignum?)		1)
    ((ratnum?)		($ratnum-d x))
    ((flonum?)		($fldenominator x))
    (else
     (%error-not-real-number x))))


(module (floor)
  ;;Return the largest  integer object not larger than  X.  Return exact
  ;;objects for exact operands and inexact objects for inexact operands.
  ;;
  (define who 'floor)

  (define (floor x)
    (cond-real-numeric-operand x
      ((fixnum?)	x)
      ((bignum?)	x)
      ((flonum?)	($floor-flonum x))
      ((ratnum?)	($floor-ratnum x))
      (else
       (%error-not-real-number x))))

  (define ($floor-ratnum x)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (let ((q (quotient x.num x.den)))
	(if (>= x.num 0)
	    q
	  (sub1 q)))))

  (define ($floor-flonum x)
    (let ((e ($flonum->exact x)))
      (if (ratnum? e)
	  (inexact ($floor-ratnum e))
	x)))

  #| end of module |# )


(module (ceiling)
  ;;Return the smallest integer object not smaller than X.  Return exact
  ;;objects for exact operands and inexact objects for inexact operands.
  ;;
  (define who 'ceiling)

  (define (ceiling x)
    (cond-real-numeric-operand x
      ((fixnum?)	x)
      ((bignum?)	x)
      ((flonum?)	($ceiling-flonum x))
      ((ratnum?)	($ceiling-ratnum x))
      (else
       (%error-not-real-number x))))

  (define ($ceiling-ratnum x)
    (let* ((x.num ($ratnum-n x))
	   (x.den ($ratnum-d x))
	   (q     (quotient x.num x.den)))
      (if (negative? x.num)
	  q
	(add1 q))))

  (define ($ceiling-flonum x)
    (let ((e ($flonum->exact x)))
      (if (ratnum? e)
	  (inexact ($ceiling-ratnum e))
	x)))

  #| end of module |# )


(module (round)
  ;;Return the  integer object  closest to  X; round to  even when  X is
  ;;halfway.   Return  exact  objects  for exact  operands  and  inexact
  ;;objects for inexact operands.
  ;;
  (define who 'round)

  (define (round x)
    (cond-real-numeric-operand x
      ((fixnum?)	x)
      ((bignum?)	x)
      ((flonum?)	($flround x))
      ((ratnum?)	($round-ratnum x))
      (else
       (%error-not-real-number x))))

  (define ($round-ratnum x)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (receive (q r)
	  (div-and-mod x.num x.den)
	(let ((r2 (+ r r)))
	  (cond ((< r2 x.den)
		 q)
		((> r2 x.den)
		 (+ q 1))
		((even? q)
		 q)
		(else
		 (add1 q)))))))

  #| end of module |# )


(module (truncate)
  ;;Return the integer  object closest to X whose absolute  value is not
  ;;larger than the absolute value of X.
  ;;
  (define who 'truncate)

  (define (truncate x)
    (cond-real-numeric-operand x
      ((fixnum?)	x)
      ((bignum?)	x)
      ((flonum?)	($truncate-flonum x))
      ((ratnum?)	($truncate-ratnum x))
      (else
       (%error-not-real-number x))))

  (define ($truncate-ratnum x)
    (let ((x.num ($ratnum-n x))
	  (x.den ($ratnum-d x)))
      (quotient x.num x.den)))

  (define ($truncate-flonum x)
    (let ((e ($flonum->exact x)))
      (if (ratnum? e)
	  (inexact ($truncate-ratnum e))
	x)))

  #| end of module |# )


(define (random n)
  (define who 'random)
  (with-arguments-validation (who)
      ((fixnum	n))
    (cond (($fx> n 1)
	   (foreign-call "ikrt_fxrandom" n))
	  (($fx= n 1)
	   0)
	  (else
	   (assertion-violation who "incorrect argument" n)))))


;;;; common bitwise operations

(let-syntax
    ((define-bitwise-operation
       (syntax-rules ()
	 ((_ ?who ?binary-who ?no-arg-return-value)
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
	      ?no-arg-return-value)
	     ((a b c d . e*)
	      (let loop ((ac (?binary-who a (?binary-who b (?binary-who c d))))
			 (e* e*))
		(if (null? e*)
		    ac
		  (loop (?binary-who ac ($car e*))
			($cdr e*)))))))
	  ))))
  (define-bitwise-operation bitwise-and binary-bitwise-and -1)
  (define-bitwise-operation bitwise-ior binary-bitwise-ior 0)
  (define-bitwise-operation bitwise-xor binary-bitwise-xor 0))

(module (bitwise-not
	 $bitwise-not-fixnum
	 $bitwise-not-bignum)
  (define who 'bitwise-not)

  (define (bitwise-not x)
    (cond-exact-integer-operand x
      ((fixnum?)	($bitwise-not-fixnum x))
      ((bignum?)	($bitwise-not-bignum x))
      (else
       (assertion-violation who "expected exact integer as argument" x))))

  (define ($bitwise-not-fixnum x)
    ($fxlognot x))

  (define ($bitwise-not-bignum x)
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


(module (bitwise-arithmetic-shift-right sra)
  (define (sra integer offset)
    (%shift-right-arithmetic integer offset 'sra))

  (define (bitwise-arithmetic-shift-right integer offset)
    (%shift-right-arithmetic integer offset 'bitwise-arithmetic-shift-right))

  (define (%shift-right-arithmetic integer offset who)
    ;;Shift right  the exact  INTEGER by OFFSET  and return  the result;
    ;;OFFSET must be non-negative.  Fill the introduced most significant
    ;;bits as appropriate to extend the sign of INTEGER.
    ;;
    (unless (fixnum? offset)
      (raise
       (condition (make-implementation-restriction-violation)
		  (make-who-condition who)
		  (make-message-condition "expected fixnum as shift offset argument")
		  (make-irritants-condition (list offset)))))
    (cond-exact-integer-operand integer
      ((fixnum?)
       (if ($fx>= offset 0)
	   ($fxsra integer offset)
	 (%error-offset-non-negative who offset)))
      ((bignum?)
       (cond (($fxpositive? offset)
	      (foreign-call "ikrt_bignum_shift_right" integer offset))
	     (($fxzero? offset)
	      integer)
	     (else
	      (%error-offset-non-negative who offset))))
      (else
       (%error-not-integer who offset))))

  (define (%error-offset-non-negative who obj)
    (assertion-violation who "offset must be non-negative" obj))

  (define (%error-not-integer who obj)
    (assertion-violation who "expected exact integer as argument" obj))

;;; --------------------------------------------------------------------

  ;;The    one    below    is    the    original    implementation    of
  ;;%SHIFT-RIGHT-ARITHMETIC.  (Marco Maggi; Fri Nov 23, 2012)
  ;;
  ;; (cond-exact-integer-operand offset
  ;;   ((fixnum?)
  ;;    (cond-exact-integer-operand integer
  ;;      ((fixnum?)
  ;; 	(if ($fx>= offset 0)
  ;; 	    ($fxsra integer offset)
  ;; 	  (%error-offset-non-negative offset)))
  ;;      ((bignum?)
  ;; 	(cond (($fx> offset 0)
  ;; 	       (foreign-call "ikrt_bignum_shift_right" integer offset))
  ;; 	      (($fxzero? offset)
  ;; 	       integer)
  ;; 	      (else
  ;; 	       (%error-offset-non-negative offset))))
  ;;      (else
  ;; 	(%error-not-integer offset))))
  ;;
  ;;   ((bignum?)
  ;;    (cond-exact-integer-operand integer
  ;;      ((fixnum?)
  ;; 	;;If the OFFSET is a bignum  and the INTEGER a fixnum: all the
  ;; 	;;bits in INTEGER shifted out.
  ;; 	(if ($fx>= integer 0)
  ;; 	    0
  ;; 	  -1))
  ;;      ((bignum?)
  ;; 	(if ($bignum-positive? integer)
  ;; 	    0
  ;; 	  ;;FIXME This  is not correct!!!   (Marco Maggi; Fri  Nov 23,
  ;; 	  ;;2012)
  ;; 	  -1))
  ;;      (else
  ;; 	(assertion-violation who "not an exact integer" integer))))
  ;;
  ;;   (else
  ;;    (%error-not-integer offset)))))

  #| end of module |# )


(module (bitwise-arithmetic-shift-left sll)
  (define (sll integer offset)
    (%shift-left-logical integer offset 'sll))

  (define (bitwise-arithmetic-shift-left integer offset)
    (%shift-left-logical integer offset 'bitwise-arithmetic-shift-left))

  (define (%shift-left-logical integer offset who)
    (unless (fixnum? offset)
      (raise
       (condition (make-implementation-restriction-violation)
		  (make-who-condition who)
		  (make-message-condition "expected fixnum as shift offset argument")
		  (make-irritants-condition (list offset)))))
    (cond-exact-integer-operand integer
      ((fixnum?)
       (cond (($fxpositive? offset)
	      (foreign-call "ikrt_fixnum_shift_left" integer offset))
	     (($fxzero? offset)
	      integer)
	     (else
	      (%error-positive-offset who offset))))
      ((bignum?)
       (cond (($fxpositive? offset)
	      (foreign-call "ikrt_bignum_shift_left" integer offset))
	     (($fxzero? offset)
	      integer)
	     (else
	      (%error-positive-offset who offset))))
      (else
       (assertion-violation who "expected exact integer as argument" integer))))

  (define (%error-positive-offset who offset)
    (assertion-violation who "offset must be non-negative" offset))

  #| end of module |# )


(define (bitwise-arithmetic-shift integer offset)
  (define who 'bitwise-arithmetic-shift)
  (unless (fixnum? offset)
    (raise
     (condition (make-implementation-restriction-violation)
		(make-who-condition who)
		(make-message-condition "expected fixnum as shift offset argument")
		(make-irritants-condition (list offset)))))
  (cond-exact-integer-operand integer
    ((fixnum?)
     (cond (($fxpositive? offset)
	    (foreign-call "ikrt_fixnum_shift_left" integer offset))
	   (($fxzero? offset)
	    integer)
	   (else
	    ;;Remember  that   when  OFFSET  is   (least-fixnum),  its
	    ;;opposite is a bignum!!!
	    (let ((offset^ (- offset)))
	      (if (fixnum? offset^)
		  ($fxsra integer offset^)
		(assertion-violation who "shift amount is too big" offset))))))

    ((bignum?)
     (cond (($fxpositive? offset)
	    (foreign-call "ikrt_bignum_shift_left" integer offset))
	   (($fxzero? offset)
	    integer)
	   (else
	    ;;Remember  that   when  OFFSET  is   (least-fixnum),  its
	    ;;opposite is a bignum!!!
	    (let ((offset^ (- offset)))
	      (if (fixnum? offset^)
		  (foreign-call "ikrt_bignum_shift_right" integer offset^)
		(assertion-violation who "shift amount is too big" offset))))))

    (else
     (assertion-violation who "not an exact integer" integer))))


(define (bitwise-length n)
  (cond-exact-integer-operand n
    ((fixnum?)	(fxlength n))
    ((bignum?)	(foreign-call "ikrt_bignum_length" n))
    (else
     (assertion-violation 'bitwise-length "not an exact integer" n))))


(module (bitwise-copy-bit)
  (define who 'bitwise-copy-bit)

  (define (bitwise-copy-bit n idx bit)
    (cond ((fixnum? idx)
	   (cond ((fx< idx 0)
		  (assertion-violation who "negative bit index" idx))
		 ((or (fixnum? n) (bignum? n))
		  (do-copy-bit n idx bit))
		 (else
		  (assertion-violation who "not an exact integer" n))))
	  ((bignum? idx)
	   (unless (or (fixnum? n) (bignum? n))
	     (assertion-violation who "not an exact integer" n))
	   (if ($bignum-positive? idx)
	       (case-fixnums bit
		 ((0)
		  (if (>= n 0)
		      n
		    (assertion-violation who "unrepresentable result")))
		 ((1)
		  (if (< n 0)
		      n
		    (assertion-violation who "unrepresentable result")))
		 (else
		  (assertion-violation who "bit must be either 0 or 1" bit)))
	     (assertion-violation who "negative bit index" idx)))
	  (else
	   (assertion-violation who "index is not an exact integer" idx))))

  (define (do-copy-bit n idx bit)
    (case-fixnums bit
      ((0)
       (if (bitwise-bit-set? n idx)
	   (bitwise-and n (bitwise-not (sll 1 idx)))
	 n))
      ((1)
       (cond ((bitwise-bit-set? n idx)
	      n)
	     ((>= n 0)
	      (+ n (sll 1 idx)))
	     (else
	      (bitwise-not (bitwise-and (bitwise-not n)
					(bitwise-not (sll 1 idx)))))))
      (else
       (assertion-violation who "bit must be either 0 or 1" bit))))

  #| end of module |# )


(define (bitwise-bit-field integer idx1 idx2)
  (define who 'bitwise-bit-field)
  (cond ((and (fixnum? idx1)
	      (fx>= idx1 0))
	 (cond ((and (fixnum? idx2)
		     ($fx>= idx2 0))
		(if ($fx<= idx1 idx2)
		    (if (or (fixnum? integer)
			    (bignum? integer))
			(bitwise-and (sra integer idx1)
				     (- (sll 1 (- idx2 idx1)) 1))
		      (assertion-violation who "not an exact integer" integer))
		  (assertion-violation who "invalid order for indices" idx1 idx2)))
	       ((not (fixnum? idx2))
		(assertion-violation who "invalid index" idx2))
	       (else
		(assertion-violation who "negative index" idx2))))
	((not (fixnum? idx1))
	 (assertion-violation who "invalid index" idx1))
	(else
	 (assertion-violation who "negative index" idx1))))


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
;; coding: utf-8-unix
;; eval: (put 'cond-numeric-operand 'scheme-indent-function 1)
;; eval: (put 'cond-exact-integer-operand 'scheme-indent-function 1)
;; eval: (put 'cond-inexact-integer-operand 'scheme-indent-function 1)
;; eval: (put 'cond-real-numeric-operand 'scheme-indent-function 1)
;; eval: (put 'cond-exact-real-numeric-operand 'scheme-indent-function 1)
;; End:
