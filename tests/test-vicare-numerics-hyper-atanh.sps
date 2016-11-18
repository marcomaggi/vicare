;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: arc tanh
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
(check-display "*** testing Vicare numerics functions: hyperbolic arc tangent\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atanh ?op)		(=> inexact=?) ?expected)
	 (check ($atanh-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0)
  (test +1	+inf.0)
  (test -1	-inf.0)
  (test +2	0.5493061443340549+1.5707963267948966i)
  (test -2	-0.5493061443340549+1.5707963267948966i)

  (case-word-size
   ((32)
    (test (greatest-fixnum) 1.8626451422920631e-9+1.5707963267948966i)
    (test (least-fixnum) -1.8626451422920631e-9+1.5707963267948966i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atanh ?op)		(=> inexact=?) ?expected)
	 (check ($atanh-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 1.8626451422920631e-9+1.5707963267948966i)
    (test BN2 1.8626451422920631e-9+1.5707963267948966i)
    (test BN3 -1.8626451422920631e-9+1.5707963267948966i)
    (test BN4 -1.8626450867809123e-9+1.5707963267948966i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atanh ?op)		(=> inexact=?) ?expected)
	 (check ($atanh-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 0.5493061443340549)
  (test -1/2 -0.5493061443340549)

  (test RN01 0.008130260435890154)
  (test RN02 -0.008130260435890154)
  (test RN03 -0.008130260435890154)
  (test RN04 -2.2910535334843677e-7+1.5707963267948966i)
  (test RN05 1.862645149230957e-9)
  (test RN06 -1.862645149230957e-9)
  (test RN07 10.397207707933518)
  (test RN09 1.862644805755772e-9)
  (test RN10 -1.862644805755772e-9)
  (test RN11 8.094622660928197)
  (test RN12 -8.0996478293477)
  (test RN13 -1.86264514576151e-9)
  (test RN14 1.86264514576151e-9)
  (test RN15 -10.050634117187885)
  (test RN16 10.397207707933518)
  (test RN17 -1.8626448022863264e-9)
  (test RN18 1.8626448022863264e-9)
  (test RN19 -8.089647496189684)
  (test RN20 8.094622661822266)
  (test RN29 10.39720770886484+1.5707963267948966i)
  (test RN30 8.094622660779168+1.5707963267948966i)
  (test RN31 -10.05063411905053+1.5707963267948966i)
  (test RN32 -8.089647495830048+1.5707963267948966i)
  (test RN34 -8.09964782943235+1.5707963267948966i)
  (test RN35 10.39720770886484+1.5707963267948966i)
  (test RN36 8.094622661971261+1.5707963267948966i)

  (case-word-size
   ((32)
    (test (/ BN1 123) 2.2910535290434795e-7+1.5707963267948966i)
    (test (/ BN2 123) 2.2910534907408204e-7+1.5707963267948966i)
    (test (/ BN3 123) -2.2910535251577027e-7+1.5707963267948966i)
    (test (/ BN4 123) -2.2910534868550432e-7+1.5707963267948966i)
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
	 (check (atanh ?op)		(=> inexact=?) ?expected)
	 (check ($atanh-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	+0.0)
  (test -0.0	-0.0)

  (test +1.0	+inf.0)
  (test -1.0	-inf.0)

  (test +2.0	+0.5493061443340549+1.5707963267948966i)
  (test -2.0	-0.5493061443340549+1.5707963267948966i)

  (test +inf.0	+nan.0)
  (test -inf.0	-nan.0)

  (test +nan.0	+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atanh ?op)		(=> inexact=?) ?expected)
	 (check ($atanh-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		+inf.0+0.0i)
  (test -1+0.0i		-inf.0+0.0i)
  (test +1-0.0i		+inf.0-0.0i)
  (test -1-0.0i		-inf.0-0.0i)

  (test +0.0+1i		+0.0+0.7853981633974483i)
  (test +0.0-1i		+0.0-0.7853981633974483i)
  (test -0.0+1i		+0.0+0.7853981633974483i)
  (test -0.0-1i		+0.0-0.7853981633974483i)

  (test +1+1.0i		+0.40235947810852507+1.0172219678978514i)
  (test +1-1.0i		+0.40235947810852507-1.0172219678978514i)
  (test -1.0+1i		-0.40235947810852507+1.0172219678978514i)
  (test -1.0-1i		-0.40235947810852507-1.0172219678978514i)

  (test 0+inf.0i	+nan.0+nan.0i)
  (test 0-inf.0i	+nan.0+nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test +1+3i		0.09193119503132943+1.2767950250211129i)
  (test +1-3i		0.09193119503132943-1.2767950250211129i)

  (test -1+3i		-0.09193119503132943+1.2767950250211129i)
  (test -1-3i		-0.09193119503132943-1.2767950250211129i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atanh ?op)		(=> inexact=?) ?expected)
	 (check ($atanh-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	+0.0+0.0i)
  (test -0.0+0.0i	-0.0+0.0i)
  (test +0.0-0.0i	+0.0-0.0i)
  (test -0.0-0.0i	-0.0-0.0i)

  (test +1.0+0.0i	+inf.0+0.0i)
  (test -1.0+0.0i	-inf.0+0.0i)
  (test +1.0-0.0i	+inf.0-0.0i)
  (test -1.0-0.0i	-inf.0-0.0i)

  (test +0.0+1.0i	+0.0+0.7853981633974483i)
  (test +0.0-1.0i	+0.0-0.7853981633974483i)
  (test -0.0+1.0i	+0.0+0.7853981633974483i)
  (test -0.0-1.0i	+0.0-0.7853981633974483i)

  (test +1.0+1.0i	+0.40235947810852507+1.0172219678978514i)
  (test +1.0-1.0i	+0.40235947810852507-1.0172219678978514i)
  (test -1.0+1.0i	-0.40235947810852507+1.0172219678978514i)
  (test -1.0-1.0i	-0.40235947810852507-1.0172219678978514i)

  (test +inf.0+0.0i	+nan.0+0.0i)
  (test -inf.0+0.0i	+nan.0+0.0i)
  (test +inf.0-0.0i	+nan.0-0.0i)
  (test -inf.0-0.0i	+nan.0-0.0i)

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

  (test 1.2+3.4i	0.08656905917945844+1.3130218230654072i)
  (test 1.2-3.4i	0.08656905917945844-1.3130218230654072i)

  (test -1.2+3.4i	-0.08656905917945844+1.3130218230654072i)
  (test -1.2-3.4i	-0.08656905917945844-1.3130218230654072i)

  (test 1.0+3.0i	0.09193119503132943+1.2767950250211129i)
  (test 1.0-3.0i	0.09193119503132943-1.2767950250211129i)

  (test -1.0+3.0i	-0.09193119503132943+1.2767950250211129i)
  (test -1.0-3.0i	-0.09193119503132943-1.2767950250211129i)

  #t)


;;;; done

(check-report)

;;; end of file
