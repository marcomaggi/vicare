;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: sin
;;;Date: Wed Dec 12, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-numerics-trigon-sin)
  (options typed-language)
  (import (vicare)
    (libtest numerics-helpers)
    (vicare system $numerics)
    (vicare checks)
    (only (vicare platform words)
	  case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: trigonometric sine\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sin ?op)		(=> inexact=?) ?expected)
	 (check ($sin-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0)
  (test +1	0.8414709848078965)
  (test -1	-0.8414709848078965)
  (test +2	0.9092974268256817)
  (test -2	-0.9092974268256817)

  (case-word-size
   ((32)
    (test (greatest-fixnum)	0.9717816115808465)
    (test (least-fixnum)	-0.32656766301791007))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sin ?op)		(=> inexact=?) ?expected)
	 (check ($sin-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 0.326567663018563)
    (test BN2 0.957428358343441)
    (test BN3 0.618891088879211)
    (test BN4 -0.760209009293282))

   ((64)
    (test VBN1 -0.83064921763725465057528179558151030107616947443429210225)
    (test VBN2 -0.830649217637255)
    (test VBN3 0.830649217637255)
    (test VBN4 0.830649217637255)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sin ?op)		(=> inexact=?) ?expected)
	 (check ($sin-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 0.479425538604203)
  (test -1/2 -0.479425538604203)

  (test RN01 0.008129991737122616)
  (test RN02 -0.008129991737122616)
  (test RN03 -0.008129991737122616)
  (test RN04 -0.8378922074826136)
  (test RN05 1.862645149230957e-9)
  (test RN06 -1.862645149230957e-9)
  (test RN07 0.8414709838015051)
  (test RN09 1.862644805755772e-9)
  (test RN10 -1.862644805755772e-9)
  (test RN11 0.8414708841687535)
  (test RN12 -0.8414708851751451)
  (test RN13 -1.86264514576151e-9)
  (test RN14 1.86264514576151e-9)
  (test RN15 -0.8414709827951136)
  (test RN16 0.8414709838015051)
  (test RN17 -1.8626448022863264e-9)
  (test RN18 1.8626448022863264e-9)
  (test RN19 -0.8414708831623622)
  (test RN20 0.8414708841687537)
  (test RN29 0.8414709858142879)
  (test RN30 0.8414710854470291)
  (test RN31 -0.8414709868206794)
  (test RN32 -0.8414710864534203)
  (test RN34 -0.8414710844406377)
  (test RN35 0.8414709858142879)
  (test RN36 0.8414710854470289)

  (test (/ BN1 123) 0.837892207482611)
  (test (/ BN2 123) 0.974235137455507)
  (test (/ BN3 123) -0.842302155887543)
  (test (/ BN4 123) -0.97236934492227)
  (test (/ 123 BN1) 2.2910535292866373e-7)
  (test (/ 123 BN2) 2.2910534908798603e-7)
  (test (/ 123 BN3) -2.2910535250192175e-7)
  (test (/ 123 BN4) -2.2910534866124408e-7)
  ;;

  ;;NOTE  Here many  calculators  yield  0.997717450110821, this  is  because of  the
  ;;rounding in:
  ;;
  ;;   ($ratnum->flonum (/ VBN1 123))
  ;;
  ;;a rounding difference in the least significant digit of such a big number changes
  ;;completely the SIN result.  (Marco Maggi; Mon Dec 5, 2016)
  ;;
  (test (/ VBN1 123) -0.3537948735838885)
  (test (/ VBN2 123) -0.3537948735838885)
  (test (/ VBN3 123) 0.3537948735838885)
  (test (/ VBN4 123) 0.3537948735838885)

  (test (/ 123 VBN1) 1.066854937725736363063333556056020620255677174831609e-16)
  (test (/ 123 VBN2) 1.066854937725736271453767412292860585634344541782910e-16)
  (test (/ 123 VBN3) -1.06685493772573636213798440308871589478530022229408e-16)
  (test (/ 123 VBN4) -1.06685493772573627052841825932555601908123260288142e-16)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sin ?op)		(=> inexact=?) ?expected)
	 (check ($sin-flonum ?op)	(=> inexact=?) ?expected)))))

  (test 0.0 0.0)
  (test -0.0 -0.0)

  (test 1.0 0.8414709848078965)
  (test -1.0 -0.8414709848078965)

  (test 2.0 0.9092974268256817)
  (test -2.0 -0.9092974268256817)

  (test +inf.0 +nan.0)
  (test -inf.0 +nan.0)
  (test +nan.0 +nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sin ?op)		(=> inexact=?) ?expected)
	 (check ($sin-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		0.8414709848078965+0.0i)
  (test -1+0.0i		-0.8414709848078965+0.0i)
  (test +1-0.0i		0.8414709848078965-0.0i)
  (test -1-0.0i		-0.8414709848078965-0.0i)

  (test +0.0+1i		0.0+1.1752011936438014i)
  (test +0.0-1i		0.0-1.1752011936438014i)
  (test -0.0+1i		-0.0+1.1752011936438014i)
  (test -0.0-1i		-0.0-1.1752011936438014i)

  (test +1+1.0i		1.2984575814159773+0.6349639147847361i)
  (test +1-1.0i		1.2984575814159773-0.6349639147847361i)
  (test -1.0+1i		-1.2984575814159773+0.6349639147847361i)
  (test -1.0-1i		-1.2984575814159773-0.6349639147847361i)

  (test 0+inf.0i	+nan.0+inf.0i)
  (test 0-inf.0i	+nan.0-inf.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i 8.471645454300148+5.412680923178193i)
  (test 1-3i 8.471645454300148-5.412680923178193i)

  (test -1+3i -8.471645454300148+5.412680923178193i)
  (test -1-3i -8.471645454300148-5.412680923178193i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (sin ?op)		(=> inexact=?) ?expected)
	 (check ($sin-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i		+0.0+0.0i)
  (test -0.0+0.0i		-0.0+0.0i)
  (test +0.0-0.0i		+0.0-0.0i)
  (test -0.0-0.0i		-0.0-0.0i)

  (test +1.0+0.0i		+0.8414709848078965+0.0i)
  (test -1.0+0.0i		-0.8414709848078965+0.0i)
  (test +1.0-0.0i		+0.8414709848078965-0.0i)
  (test -1.0-0.0i		-0.8414709848078965-0.0i)

  (test +0.0+1.0i		+0.0+1.1752011936438014i)
  (test +0.0-1.0i		+0.0-1.1752011936438014i)
  (test -0.0+1.0i		-0.0+1.1752011936438014i)
  (test -0.0-1.0i		-0.0-1.1752011936438014i)

  (test +1.0+1.0i		+1.2984575814159773+0.6349639147847361i)
  (test +1.0-1.0i		+1.2984575814159773-0.6349639147847361i)
  (test -1.0+1.0i		-1.2984575814159773+0.6349639147847361i)
  (test -1.0-1.0i		-1.2984575814159773-0.6349639147847361i)

  (test +inf.0+0.0i		+nan.0+nan.0i)
  (test -inf.0+0.0i		+nan.0+nan.0i)
  (test +inf.0-0.0i		+nan.0+nan.0i)
  (test -inf.0-0.0i		+nan.0+nan.0i)

  (test +0.0+inf.0i		+nan.0+inf.0i)
  (test +0.0-inf.0i		+nan.0-inf.0i)
  (test -0.0+inf.0i		+nan.0+inf.0i)
  (test -0.0-inf.0i		+nan.0-inf.0i)

  (test +inf.0+inf.0i		+nan.0+nan.0i)
  (test +inf.0-inf.0i		+nan.0+nan.0i)
  (test -inf.0+inf.0i		+nan.0+nan.0i)
  (test -inf.0-inf.0i		+nan.0+nan.0i)

  (test +1.0+nan.0i		+nan.0+nan.0i)
  (test +nan.0+1.0i		+nan.0+nan.0i)
  (test +nan.0+nan.0i		+nan.0+nan.0i)

  (test +1.2+3.4i		+13.979408806017995+5.422815472463402i)
  (test +1.2-3.4i		+13.979408806017995-5.422815472463402i)
  (test -1.2+3.4i		-13.979408806017995+5.422815472463402i)
  (test -1.2-3.4i		-13.979408806017995-5.422815472463402i)

  (test +1.0+3.0i		+8.471645454300148+5.412680923178193i)
  (test +1.0-3.0i		+8.471645454300148-5.412680923178193i)
  (test -1.0+3.0i		-8.471645454300148+5.412680923178193i)
  (test -1.0-3.0i		-8.471645454300148-5.412680923178193i)

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
