;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: arc sinh
;;;Date: Fri Dec 14, 2012
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
(import (vicare)
  (libtest numerics-helpers)
  (vicare system $numerics)
  (vicare checks)
  (only (vicare platform words)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: hyperbolic arc sine\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asinh ?op)		(=> inexact=?) ?expected)
	 (check ($asinh-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0)
  (test +1	+0.881373587019543)
  (test -1	-0.881373587019543)
  (test +2	+1.4436354751788103)
  (test -2	-1.4436354751788103)

  (case-word-size
   ((32)
    (test (greatest-fixnum)	+20.794415414935713)
    (test (least-fixnum)	-20.79441541679836))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asinh ?op)		(=> inexact=?) ?expected)
	 (check ($asinh-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 20.794415418661004)
    (test BN2 20.79441543542481)
    (test BN3 -20.79441542052365)
    (test BN4 -20.794415437287455))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asinh ?op)		(=> inexact=?) ?expected)
	 (check ($asinh-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 0.48121182505960347)
  (test -1/2 -0.48121182505960347)

  (test RN01 0.00812999173949052)
  (test RN02 -0.00812999173949052)
  (test RN03 -0.00812999173949052)
  (test RN04 -15.982231061425955)
  (test RN05 1.862645149230957e-9)
  (test RN06 -1.862645149230957e-9)
  (test RN07 0.881373585702454)
  (test RN09 1.862644805755772e-9)
  (test RN10 -1.862644805755772e-9)
  (test RN11 0.8813734553106596)
  (test RN12 -0.8813734566277485)
  (test RN13 -1.86264514576151e-9)
  (test RN14 1.86264514576151e-9)
  (test RN15 -0.881373584385365)
  (test RN16 0.881373585702454)
  (test RN17 -1.8626448022863264e-9)
  (test RN18 1.8626448022863264e-9)
  (test RN19 -0.8813734539935709)
  (test RN20 0.8813734553106598)
  (test RN29 0.881373588336632)
  (test RN30 0.8813737187284388)
  (test RN31 -0.881373589653721)
  (test RN32 -0.8813737200455277)
  (test RN34 -0.8813737174113496)
  (test RN35 0.881373588336632)
  (test RN36 0.8813737187284385)

  (case-word-size
   ((32)
    (test (/ BN1 123) 15.9822310632886)
    (test (/ BN2 123) 15.982231080052406)
    (test (/ BN3 123) -15.982231065151245)
    (test (/ BN4 123) -15.982231081915051)
    (test (/ 123 BN1) 2.2910535292866373e-7)
    (test (/ 123 BN2) 2.2910534908798603e-7)
    (test (/ 123 BN3) -2.2910535250192175e-7)
    (test (/ 123 BN4) -2.2910534866124408e-7))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asinh ?op)		(=> inexact=?) ?expected)
	 (check ($asinh-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	+0.0)
  (test -0.0	-0.0)

  (test +1.0	+0.881373587019543)
  (test -1.0	-0.881373587019543)

  (test +2.0	+1.4436354751788103)
  (test -2.0	-1.4436354751788103)

  (test +inf.0	+inf.0)
  (test -inf.0	-inf.0)

  (test +nan.0	+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asinh ?op)		(=> inexact=?) ?expected)
	 (check ($asinh-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		+0.881373587019543+0.0i)
  (test -1+0.0i		-0.881373587019543+0.0i)
  (test +1-0.0i		+0.881373587019543-0.0i)
  (test -1-0.0i		-0.881373587019543-0.0i)

  (test +0.0+1i		+0.0+1.5707963267948966i)
  (test +0.0-1i		+0.0-1.5707963267948966i)
  (test -0.0+1i		+0.0+1.5707963267948966i)
  (test -0.0-1i		+0.0-1.5707963267948966i)

  (test +1+1.0i		+1.0612750619050357+0.6662394324925153i)
  (test +1-1.0i		+1.0612750619050357-0.6662394324925153i)
  (test -1.0+1i		-1.0612750619050357+0.6662394324925153i)
  (test -1.0-1i		-1.0612750619050357-0.6662394324925153i)

  (test 0+inf.0i	+nan.0+nan.0i)
  (test 0-inf.0i	+nan.0+nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i		1.8241987021938828+1.2330952175293441i)
  (test 1-3i		1.8241987021938828-1.2330952175293441i)

  (test -1+3i		-1.8241987021938828+1.2330952175293441i)
  (test -1-3i		-1.8241987021938828-1.2330952175293441i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asinh ?op)		(=> inexact=?) ?expected)
	 (check ($asinh-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	+0.0+0.0i)
  (test -0.0+0.0i	-0.0+0.0i)
  (test +0.0-0.0i	+0.0-0.0i)
  (test -0.0-0.0i	-0.0-0.0i)

  (test +1.0+0.0i	+0.881373587019543+0.0i)
  (test -1.0+0.0i	-0.881373587019543+0.0i)
  (test +1.0-0.0i	+0.881373587019543-0.0i)
  (test -1.0-0.0i	-0.881373587019543-0.0i)

  (test +0.0+1.0i	+0.0+1.5707963267948966i)
  (test +0.0-1.0i	+0.0-1.5707963267948966i)
  (test -0.0+1.0i	+0.0+1.5707963267948966i)
  (test -0.0-1.0i	+0.0-1.5707963267948966i)

  (test +1.0+1.0i	+1.0612750619050357+0.6662394324925153i)
  (test +1.0-1.0i	+1.0612750619050357-0.6662394324925153i)
  (test -1.0+1.0i	-1.0612750619050357+0.6662394324925153i)
  (test -1.0-1.0i	-1.0612750619050357-0.6662394324925153i)

  (test +inf.0+0.0i	+inf.0+0.0i)
  (test -inf.0+0.0i	-inf.0+0.0i)
  (test +inf.0-0.0i	+inf.0-0.0i)
  (test -inf.0-0.0i	-inf.0-0.0i)

  (test +0.0+inf.0i	+nan.0+nan.0i)
  (test +0.0-inf.0i	+nan.0+nan.0i)
  (test -0.0+inf.0i	-nan.0+nan.0i)
  (test -0.0-inf.0i	-nan.0+nan.0i)

  (test +inf.0+inf.0i	+nan.0+nan.0i)
  (test +inf.0-inf.0i	+nan.0+nan.0i)
  (test -inf.0+inf.0i	-nan.0+nan.0i)
  (test -inf.0-inf.0i	-nan.0+nan.0i)

  (test 1.0+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1.0i	+nan.0+nan.0i)
  (test +nan.0+nan.0i	+nan.0+nan.0i)

  (test +1.2+3.4i	+1.9605456242747565+1.21886891663989i)
  (test +1.2-3.4i	+1.9605456242747565-1.21886891663989i)

  (test -1.2+3.4i	-1.9605456242747565+1.21886891663989i)
  (test -1.2-3.4i	-1.9605456242747565-1.21886891663989i)

  (test +1.0+3.0i	+1.8241987021938828+1.2330952175293441i)
  (test +1.0-3.0i	+1.8241987021938828-1.2330952175293441i)

  (test -1.0+3.0i	-1.8241987021938828+1.2330952175293441i)
  (test -1.0-3.0i	-1.8241987021938828-1.2330952175293441i)

  #t)


;;;; done

(check-report)

;;; end of file
