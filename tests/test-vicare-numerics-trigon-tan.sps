;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: tan
;;;Date: Wed Dec 12, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013, 2014, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(check-display "*** testing Vicare numerics functions: trigonometric tangent\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tan ?op)		(=> inexact=?) ?expected)
	 (check ($tan-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0)
  (test +1	1.5574077246549023)
  (test -1	-1.5574077246549023)
  (test +2	-2.185039863261519)
  (test -2	2.185039863261519)

  (case-word-size
   ((32)
    (test (greatest-fixnum)	-4.119771864175813)
    (test (least-fixnum)	0.3455106923335302))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tan ?op)		(=> inexact=?) ?expected)
	 (check ($tan-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 0.7879177086709692)
    (test BN2 0.24742342787709823)
    (test BN3 10.326865661050702)
    (test BN4 -2.93630410256651))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tan ?op)		(=> inexact=?) ?expected)
	 (check ($tan-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 0.5463024898437905)
  (test -1/2 -0.5463024898437905)

  (test RN01 0.008130260433521937)
  (test RN02 -0.008130260433521937)
  (test RN03 -0.008130260433521937)
  (test RN04 -1.5350629413020256)
  (test RN05 1.862645149230957e-9)
  (test RN06 -1.862645149230957e-9)
  (test RN07 1.5574077182743762)
  (test RN09 1.862644805755772e-9)
  (test RN10 -1.862644805755772e-9)
  (test RN11 1.5574070866026033)
  (test RN12 -1.5574070929831245)
  (test RN13 -1.86264514576151e-9)
  (test RN14 1.86264514576151e-9)
  (test RN15 -1.5574077118938503)
  (test RN16 1.5574077182743762)
  (test RN17 -1.8626448022863264e-9)
  (test RN18 1.8626448022863264e-9)
  (test RN19 -1.5574070802220836)
  (test RN20 1.5574070866026044)
  (test RN29 1.5574077310354282)
  (test RN30 1.5574083627076905)
  (test RN31 -1.5574077374159543)
  (test RN32 -1.5574083690882201)
  (test RN34 -1.5574083563271592)
  (test RN35 1.5574077310354282)
  (test RN36 1.557408362707689)

  (case-word-size
   ((32)
    (test (/ BN1 123) 1.5626963726122678)
    (test (/ BN2 123) 1.847641910802439)
    (test (/ BN3 123) -1.591041012482869)
    (test (/ BN4 123) -1.8840743770510016)
    (test (/ 123 BN1) 2.2910535292866974e-7)
    (test (/ 123 BN2) 2.2910534908799204e-7)
    (test (/ 123 BN3) -2.2910535250192776e-7)
    (test (/ 123 BN4) -2.291053486612501e-7))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tan ?op)		(=> inexact=?) ?expected)
	 (check ($tan-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	+0.0)
  (test -0.0	-0.0)

  (test +1.0	+1.5574077246549023)
  (test -1.0	-1.5574077246549023)

  (test +2.0	-2.185039863261519)
  (test -2.0	+2.185039863261519)

  (test +inf.0	+nan.0)
  (test -inf.0	+nan.0)

  (test +nan.0 +nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tan ?op)		(=> inexact=?) ?expected)
	 (check ($tan-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		+1.5574077246549023+0.0i)
  (test -1+0.0i		-1.5574077246549023+0.0i)
  (test +1-0.0i		+1.5574077246549023-0.0i)
  (test -1-0.0i		-1.5574077246549023-0.0i)

  (test +0.0+1i		+0.0+0.7615941559557649i)
  (test +0.0-1i		+0.0-0.7615941559557649i)
  (test -0.0+1i		-0.0+0.7615941559557649i)
  (test -0.0-1i		-0.0-0.7615941559557649i)

  (test +1+1.0i		+0.27175258531951174+1.0839233273386946i)
  (test +1-1.0i		+0.27175258531951174-1.0839233273386946i)
  (test -1.0+1i		-0.27175258531951174+1.0839233273386946i)
  (test -1.0-1i		-0.27175258531951174-1.0839233273386946i)

  (test 0+inf.0i	+0.0+1.0i)
  (test 0-inf.0i	+0.0-1.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test +1+3i		0.0045171372766584245+1.002054988245812i)
  (test +1-3i		0.0045171372766584245-1.002054988245812i)

  (test -1+3i		-0.0045171372766584245+1.002054988245812i)
  (test -1-3i		-0.0045171372766584245-1.002054988245812i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tan ?op)		(=> inexact=?) ?expected)
	 (check ($tan-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	+0.0+0.0i)
  (test -0.0+0.0i	-0.0+0.0i)
  (test +0.0-0.0i	+0.0-0.0i)
  (test -0.0-0.0i	-0.0-0.0i)

  (test +1.0+0.0i	+1.5574077246549023+0.0i)
  (test -1.0+0.0i	-1.5574077246549023+0.0i)
  (test +1.0-0.0i	+1.5574077246549023-0.0i)
  (test -1.0-0.0i	-1.5574077246549023-0.0i)

  (test +0.0+1.0i	+0.0+0.7615941559557649i)
  (test +0.0-1.0i	+0.0-0.7615941559557649i)
  (test -0.0+1.0i	-0.0+0.7615941559557649i)
  (test -0.0-1.0i	-0.0-0.7615941559557649i)

  (test +1.0+1.0i	+0.27175258531951174+1.0839233273386946i)
  (test +1.0-1.0i	+0.27175258531951174-1.0839233273386946i)
  (test -1.0+1.0i	-0.27175258531951174+1.0839233273386946i)
  (test -1.0-1.0i	-0.27175258531951174-1.0839233273386946i)

  (test +inf.0+0.0i	+nan.0+nan.0i)
  (test -inf.0+0.0i	+nan.0+nan.0i)
  (test +inf.0-0.0i	+nan.0+nan.0i)
  (test -inf.0-0.0i	+nan.0+nan.0i)

  (test +0.0+inf.0i	+0.0+1.0i)
  (test +0.0-inf.0i	+0.0-1.0i)
  (test -0.0+inf.0i	-0.0+1.0i)
  (test -0.0-inf.0i	-0.0-1.0i)

  (test +inf.0+inf.0i	+nan.0+nan.0i)
  (test +inf.0-inf.0i	+nan.0-nan.0i)
  (test -inf.0+inf.0i	-nan.0+nan.0i)
  (test -inf.0-inf.0i	-nan.0-nan.0i)

  (test 1.0+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1.0i	+nan.0+nan.0i)
  (test +nan.0+nan.0i	+nan.0+nan.0i)

  (test +1.2+3.4i	+0.0015071018758057834+1.0016427969891413i)
  (test +1.2-3.4i	+0.0015071018758057834-1.0016427969891413i)
  (test -1.2+3.4i	-0.0015071018758057834+1.0016427969891413i)
  (test -1.2-3.4i	-0.0015071018758057834-1.0016427969891413i)

  (test +1.0+3.0i	+0.0045171372766584245+1.002054988245812i)
  (test +1.0-3.0i	+0.0045171372766584245-1.002054988245812i)
  (test -1.0+3.0i	-0.0045171372766584245+1.002054988245812i)
  (test -1.0-3.0i	-0.0045171372766584245-1.002054988245812i)

  #t)


;;;; done

(check-report)

;;; end of file
