;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: tanh
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
(check-display "*** testing Vicare numerics functions: hyperbolic tangent\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tanh ?op)		(=> inexact=?) ?expected)
	 (check ($tanh-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0)
  (test +1	+0.7615941559557649)
  (test -1	-0.7615941559557649)
  (test +2	+0.9640275800758169)
  (test -2	-0.9640275800758169)

  (case-word-size
   ((32)
    (test (greatest-fixnum) 1.0)
    (test (least-fixnum) -1.0))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tanh ?op)		(=> inexact=?) ?expected)
	 (check ($tanh-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 1.0)
    (test BN2 1.0)
    (test BN3 -1.0)
    (test BN4 -1.0))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tanh ?op)		(=> inexact=?) ?expected)
	 (check ($tanh-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 0.46211715726000974)
  (test -1/2 -0.46211715726000974)

  (test RN01 0.008129902177576113)
  (test RN02 -0.008129902177576113)
  (test RN03 -0.008129902177576113)
  (test RN04 -1.0)
  (test RN05 1.862645149230957e-9)
  (test RN06 -1.862645149230957e-9)
  (test RN07 0.7615941551735017)
  (test RN09 1.862644805755772e-9)
  (test RN10 -1.862644805755772e-9)
  (test RN11 0.7615940777294512)
  (test RN12 -0.7615940785117145)
  (test RN13 -1.86264514576151e-9)
  (test RN14 1.86264514576151e-9)
  (test RN15 -0.7615941543912386)
  (test RN16 0.7615941551735017)
  (test RN17 -1.8626448022863264e-9)
  (test RN18 1.8626448022863264e-9)
  (test RN19 -0.7615940769471881)
  (test RN20 0.7615940777294513)
  (test RN29 0.7615941567380281)
  (test RN30 0.761594234182071)
  (test RN31 -0.7615941575202912)
  (test RN32 -0.761594234964334)
  (test RN34 -0.7615942333998078)
  (test RN35 0.7615941567380281)
  (test RN36 0.7615942341820708)

  (case-word-size
   ((32)
    (test (/ BN1 123) 1.0)
    (test (/ BN2 123) 1.0)
    (test (/ BN3 123) -1.0)
    (test (/ BN4 123) -1.0)
    (test (/ 123 BN1) 2.2910535292866174e-7)
    (test (/ 123 BN2) 2.2910534908798405e-7)
    (test (/ 123 BN3) -2.2910535250191977e-7)
    (test (/ 123 BN4) -2.291053486612421e-7))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tanh ?op)		(=> inexact=?) ?expected)
	 (check ($tanh-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	+0.0)
  (test -0.0	-0.0)

  (test +1.0	+0.7615941559557649)
  (test -1.0	-0.7615941559557649)

  (test +2.0	+0.9640275800758169)
  (test -2.0	-0.9640275800758169)

  (test +inf.0	+1.0)
  (test -inf.0	-1.0)

  (test +nan.0	+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tanh ?op)		(=> inexact=?) ?expected)
	 (check ($tanh-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		+0.7615941559557649+0.0i)
  (test -1+0.0i		-0.7615941559557649+0.0i)
  (test +1-0.0i		+0.7615941559557649-0.0i)
  (test -1-0.0i		-0.7615941559557649-0.0i)

  (test +0.0+1i		0.0+1.5574077246549023i)
  (test +0.0-1i		0.0-1.5574077246549023i)
  (test -0.0+1i		-0.0+1.5574077246549023i)
  (test -0.0-1i		-0.0-1.5574077246549023i)

  (test +1+1.0i		1.0839233273386946+0.27175258531951174i)
  (test +1-1.0i		1.0839233273386946-0.27175258531951174i)
  (test -1.0+1i		-1.0839233273386946+0.27175258531951174i)
  (test -1.0-1i		-1.0839233273386946-0.27175258531951174i)

  (test 0+inf.0i	+nan.0+nan.0i)
  (test 0-inf.0i	+nan.0+nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test +1+3i		0.7680176472869111-0.05916853956605073i)
  (test +1-3i		0.7680176472869111+0.05916853956605073i)

  (test -1+3i		-0.7680176472869111-0.05916853956605073i)
  (test -1-3i		-0.7680176472869111+0.05916853956605073i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (tanh ?op)		(=> inexact=?) ?expected)
	 (check ($tanh-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	+0.0+0.0i)
  (test -0.0+0.0i	-0.0+0.0i)
  (test +0.0-0.0i	+0.0-0.0i)
  (test -0.0-0.0i	-0.0-0.0i)

  (test +1.0+0.0i	+0.7615941559557649+0.0i)
  (test -1.0+0.0i	-0.7615941559557649+0.0i)
  (test +1.0-0.0i	+0.7615941559557649-0.0i)
  (test -1.0-0.0i	-0.7615941559557649-0.0i)

  (test +0.0+1.0i	+0.0+1.5574077246549023i)
  (test +0.0-1.0i	+0.0-1.5574077246549023i)
  (test -0.0+1.0i	-0.0+1.5574077246549023i)
  (test -0.0-1.0i	-0.0-1.5574077246549023i)

  (test +1.0+1.0i	+1.0839233273386946+0.27175258531951174i)
  (test +1.0-1.0i	+1.0839233273386946-0.27175258531951174i)
  (test -1.0+1.0i	-1.0839233273386946+0.27175258531951174i)
  (test -1.0-1.0i	-1.0839233273386946-0.27175258531951174i)

  (test +inf.0+0.0i	+1.0+0.0i)
  (test -inf.0+0.0i	-1.0+0.0i)
  (test +inf.0-0.0i	+1.0-0.0i)
  (test -inf.0-0.0i	-1.0-0.0i)

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

  (test +1.2+3.4i	+0.8505969575493737+0.07688871006570458i)
  (test +1.2-3.4i	+0.8505969575493737-0.07688871006570458i)
  (test -1.2+3.4i	-0.8505969575493737+0.07688871006570458i)
  (test -1.2-3.4i	-0.8505969575493737-0.07688871006570458i)

  (test +1.0+3.0i	+0.7680176472869111-0.05916853956605073i)
  (test +1.0-3.0i	+0.7680176472869111+0.05916853956605073i)
  (test -1.0+3.0i	-0.7680176472869111-0.05916853956605073i)
  (test -1.0-3.0i	-0.7680176472869111+0.05916853956605073i)

  #t)


;;;; done

(check-report)

;;; end of file
