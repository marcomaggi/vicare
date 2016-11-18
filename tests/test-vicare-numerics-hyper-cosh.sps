;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: cosh
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
(check-display "*** testing Vicare numerics functions: hyperbolic cosine\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (cosh ?op)		(=> inexact=?) ?expected)
	 (check ($cosh-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	1)
  (test +1	1.5430806348152437)
  (test -1	1.5430806348152437)
  (test +2	3.7621956910836314)
  (test -2	3.7621956910836314)

  (case-word-size
   ((32)
    (test (greatest-fixnum) +inf.0)
    (test (least-fixnum) +inf.0))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (cosh ?op)		(=> inexact=?) ?expected)
	 (check ($cosh-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 +inf.0)
    (test BN2 +inf.0)
    (test BN3 +inf.0)
    (test BN4 +inf.0))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (cosh ?op)		(=> inexact=?) ?expected)
	 (check ($cosh-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 1.1276259652063807)
  (test -1/2 1.1276259652063807)

  (test RN01 1.0000330492930198)
  (test RN02 1.0000330492930198)
  (test RN03 1.0000330492930198)
  (test RN04 +inf.0)
  (test RN05 1.0)
  (test RN06 1.0)
  (test RN07 1.543080632626261)
  (test RN09 1.0)
  (test RN10 1.0)
  (test RN11 1.5430804159170306)
  (test RN12 1.5430804181060125)
  (test RN13 1.0)
  (test RN14 1.0)
  (test RN15 1.5430806304372782)
  (test RN16 1.543080632626261)
  (test RN17 1.0)
  (test RN18 1.0)
  (test RN19 1.5430804137280492)
  (test RN20 1.543080415917031)
  (test RN29 1.5430806370042265)
  (test RN30 1.5430808537135514)
  (test RN31 1.5430806391932095)
  (test RN32 1.5430808559025346)
  (test RN34 1.5430808515245675)
  (test RN35 1.5430806370042265)
  (test RN36 1.5430808537135507)

  (case-word-size
   ((32)
    (test (/ BN1 123) +inf.0)
    (test (/ BN2 123) +inf.0)
    (test (/ BN3 123) +inf.0)
    (test (/ BN4 123) +inf.0)
    (test (/ 123 BN1) 1.0000000000000262)
    (test (/ 123 BN2) 1.0000000000000262)
    (test (/ 123 BN3) 1.0000000000000262)
    (test (/ 123 BN4) 1.0000000000000262))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (cosh ?op)		(=> inexact=?) ?expected)
	 (check ($cosh-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0 1.0)
  (test -0.0 1.0)

  (test +1.0 1.5430806348152437)
  (test -1.0 1.5430806348152437)

  (test +2.0 3.7621956910836314)
  (test -2.0 3.7621956910836314)

  (test +inf.0 +inf.0)
  (test -inf.0 +inf.0)

  (test +nan.0 +nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (cosh ?op)		(=> inexact=?) ?expected)
	 (check ($cosh-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		1.5430806348152437+0.0i)
  (test -1+0.0i		1.5430806348152437-0.0i)
  (test +1-0.0i		1.5430806348152437-0.0i)
  (test -1-0.0i		1.5430806348152437+0.0i)

  (test +0.0+1i		0.5403023058681398+0.0i)
  (test +0.0-1i		0.5403023058681398-0.0i)
  (test -0.0+1i		0.5403023058681398-0.0i)
  (test -0.0-1i		0.5403023058681398+0.0i)

  (test +1+1.0i		0.8337300251311491+0.9888977057628651i)
  (test +1-1.0i		0.8337300251311491-0.9888977057628651i)
  (test -1.0+1i		0.8337300251311491-0.9888977057628651i)
  (test -1.0-1i		0.8337300251311491+0.9888977057628651i)

  (test 0+inf.0i	+nan.0+nan.0i)
  (test 0-inf.0i	+nan.0+nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i		-1.5276382501165433+0.1658444019189788i)
  (test 1-3i		-1.5276382501165433-0.1658444019189788i)

  (test -1+3i		-1.5276382501165433-0.1658444019189788i)
  (test -1-3i		-1.5276382501165433+0.1658444019189788i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (cosh ?op)		(=> inexact=?) ?expected)
	 (check ($cosh-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	1.0+0.0i)
  (test -0.0+0.0i	1.0+0.0i)
  (test +0.0-0.0i	1.0-0.0i)
  (test -0.0-0.0i	1.0-0.0i)

  (test +1.0+0.0i	1.5430806348152437+0.0i)
  (test -1.0+0.0i	1.5430806348152437+0.0i)
  (test +1.0-0.0i	1.5430806348152437-0.0i)
  (test -1.0-0.0i	1.5430806348152437-0.0i)

  (test +0.0+1.0i	0.5403023058681398+0.0i)
  (test +0.0-1.0i	0.5403023058681398-0.0i)
  (test -0.0+1.0i	0.5403023058681398-0.0i)
  (test -0.0-1.0i	0.5403023058681398+0.0i)

  (test +1.0+1.0i	0.8337300251311491+0.9888977057628651i)
  (test +1.0-1.0i	0.8337300251311491-0.9888977057628651i)
  (test -1.0+1.0i	0.8337300251311491-0.9888977057628651i)
  (test -1.0-1.0i	0.8337300251311491+0.9888977057628651i)

  (test +inf.0+0.0i	+inf.0+0.0i)
  (test -inf.0+0.0i	+inf.0+0.0i)
  (test +inf.0-0.0i	+inf.0-0.0i)
  (test -inf.0-0.0i	+inf.0-0.0i)

  (test +0.0+inf.0i	+nan.0+nan.0i)
  (test +0.0-inf.0i	+nan.0+nan.0i)
  (test -0.0+inf.0i	+nan.0+nan.0i)
  (test -0.0-inf.0i	+nan.0+nan.0i)

  (test +inf.0+inf.0i	+nan.0+nan.0i)
  (test +inf.0-inf.0i	+nan.0+nan.0i)
  (test -inf.0+inf.0i	+nan.0+nan.0i)
  (test -inf.0-inf.0i	+nan.0+nan.0i)

  (test 1.0+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1.0i	+nan.0+nan.0i)
  (test +nan.0+nan.0i	+nan.0+nan.0i)

  (test +1.2+3.4i	-1.7505385298731442-0.3857294182289409i)
  (test +1.2-3.4i	-1.7505385298731442+0.3857294182289409i)

  (test -1.2+3.4i	-1.7505385298731442+0.3857294182289409i)
  (test -1.2-3.4i	-1.7505385298731442-0.3857294182289409i)

  (test +1.0+3.0i	-1.5276382501165433+0.1658444019189788i)
  (test +1.0-3.0i	-1.5276382501165433-0.1658444019189788i)

  (test -1.0+3.0i	-1.5276382501165433-0.1658444019189788i)
  (test -1.0-3.0i	-1.5276382501165433+0.1658444019189788i)

  #t)


;;;; done

(check-report)

;;; end of file
