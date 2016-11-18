;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: arc tan
;;;Date: Thu Dec 13, 2012
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


#!vicare
(import (vicare)
  (libtest numerics-helpers)
  (vicare system $numerics)
  (vicare checks)
  (only (vicare platform words)
	case-word-size)
  (vicare numerics constants))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: trigonometric arc tangent\n")


(parametrise ((check-test-name	'atan2))

  (define-syntax test
    (syntax-rules ()
      ((_ ?imp ?rep ?expected)
       (begin
	 (check (atan ?imp ?rep)		(=> inexact=?) ?expected)
	 (check ($atan2-real-real ?imp ?rep)	(=> inexact=?) ?expected)))))

  (test 0 0		0.0)

  (test +0.0 0		0.0)
  (test -0.0 0		-0.0)

  (test 0 +0.0		0.0)
  (test 0 -0.0		greek-pi)

  (test -0.0 -0.0	-greek-pi)

  (test +inf.0 +0.0	greek-pi/2)
  (test +inf.0 -0.0	greek-pi/2)
  (test -inf.0 +0.0	-greek-pi/2)
  (test -inf.0 -0.0	-greek-pi/2)

  (test +0.0 +inf.0	+0.0)
  (test -0.0 +inf.0	-0.0)
  (test +0.0 -inf.0	+greek-pi)
  (test -0.0 -inf.0	-greek-pi)

  #t)


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atan ?op)		(=> inexact=?) ?expected)
	 (check ($atan-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0)
  (test +1	+0.7853981633974483)
  (test -1	-0.7853981633974483)
  (test +2	+1.1071487177940904)
  (test -2	-1.1071487177940904)

  (case-word-size
   ((32)
    (test (greatest-fixnum)	+greek-pi/2)
    (test (least-fixnum)	-greek-pi/2))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atan ?op)		(=> inexact=?) ?expected)
	 (check ($atan-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 1.5707963249322514)
    (test BN2 1.5707963249322514)
    (test BN3 -1.5707963249322514)
    (test BN4 -1.5707963249322514))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atan ?op)		(=> inexact=?) ?expected)
	 (check ($atan-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 0.4636476090008061)
  (test -1/2 -0.4636476090008061)

  (test RN01 0.008129902179943912)
  (test RN02 -0.008129902179943912)
  (test RN03 -0.008129902179943912)
  (test RN04 -1.5707960976895432)
  (test RN05 1.862645149230957e-9)
  (test RN06 -1.862645149230957e-9)
  (test RN07 0.7853981624661257)
  (test RN09 1.862644805755772e-9)
  (test RN10 -1.862644805755772e-9)
  (test RN11 0.7853980702651994)
  (test RN12 -0.7853980711965219)
  (test RN13 -1.86264514576151e-9)
  (test RN14 1.86264514576151e-9)
  (test RN15 -0.7853981615348031)
  (test RN16 0.7853981624661257)
  (test RN17 -1.8626448022863264e-9)
  (test RN18 1.8626448022863264e-9)
  (test RN19 -0.785398069333877)
  (test RN20 0.7853980702651995)
  (test RN29 0.7853981643287709)
  (test RN30 0.7853982565296973)
  (test RN31 -0.7853981652600934)
  (test RN32 -0.7853982574610198)
  (test RN34 -0.7853982555983747)
  (test RN35 0.7853981643287709)
  (test RN36 0.7853982565296971)

  (case-word-size
   ((32)
    (test (/ BN1 123) 1.5707960976895436)
    (test (/ BN2 123) 1.5707960976895474)
    (test (/ BN3 123) -1.570796097689544)
    (test (/ BN4 123) -1.5707960976895479)
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
	 (check (atan ?op)		(=> inexact=?) ?expected)
	 (check ($atan-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	+0.0)
  (test -0.0	-0.0)

  (test +1.0	+0.7853981633974483)
  (test -1.0	-0.7853981633974483)

  (test +2.0	+1.1071487177940904)
  (test -2.0	-1.1071487177940904)

  (test +inf.0	+1.5707963267948966)
  (test -inf.0	-1.5707963267948966)

  (test +nan.0	+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atan ?op)		(=> inexact=?) ?expected)
	 (check ($atan-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		+0.7853981633974483+0.0i)
  (test -1+0.0i		-0.7853981633974483+0.0i)
  (test +1-0.0i		+0.7853981633974483+0.0i)
  (test -1-0.0i		-0.7853981633974483+0.0i)

  (test +0.0+1i		+0.0+inf.0i)
  (test +0.0-1i		+0.0-inf.0i)
  (test -0.0+1i		-0.0+inf.0i)
  (test -0.0-1i		-0.0-inf.0i)

  (test +1+1.0i		+1.0172219678978514+0.40235947810852507i)
  (test +1-1.0i		+1.0172219678978514-0.40235947810852507i)
  (test -1.0+1i		-1.0172219678978514+0.40235947810852507i)
  (test -1.0-1i		-1.0172219678978514-0.40235947810852507i)

  (test 0+inf.0i	1.5707963267948966+nan.0i)
  (test 0-inf.0i	1.5707963267948966-nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test +1+3i		1.4614618538579256+0.3059438579055289i)
  (test +1-3i		1.4614618538579256-0.3059438579055289i)

  (test -1+3i		-1.4614618538579256+0.3059438579055289i)
  (test -1-3i		-1.4614618538579256-0.3059438579055289i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (atan ?op)		(=> inexact=?) ?expected)
	 (check ($atan-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	+0.0+0.0i)
  (test -0.0+0.0i	-0.0+0.0i)
  (test +0.0-0.0i	+0.0+0.0i)
  (test -0.0-0.0i	-0.0+0.0i)

  (test +1.0+0.0i	+0.7853981633974483+0.0i)
  (test -1.0+0.0i	-0.7853981633974483+0.0i)
  (test +1.0-0.0i	+0.7853981633974483+0.0i)
  (test -1.0-0.0i	-0.7853981633974483+0.0i)

  (test +0.0+1.0i	+0.0+inf.0i)
  (test +0.0-1.0i	+0.0-inf.0i)
  (test -0.0+1.0i	-0.0+inf.0i)
  (test -0.0-1.0i	-0.0-inf.0i)

  (test +1.0+1.0i	+1.0172219678978514+0.40235947810852507i)
  (test +1.0-1.0i	+1.0172219678978514-0.40235947810852507i)
  (test -1.0+1.0i	-1.0172219678978514+0.40235947810852507i)
  (test -1.0-1.0i	-1.0172219678978514-0.40235947810852507i)

  (test +inf.0+0.0i	+1.5707963267948966+nan.0i)
  (test -inf.0+0.0i	-1.5707963267948966+nan.0i)
  (test +inf.0-0.0i	+1.5707963267948966+nan.0i)
  (test -inf.0-0.0i	-1.5707963267948966+nan.0i)

  (test +0.0+inf.0i	+1.5707963267948966+nan.0i)
  (test +0.0-inf.0i	+1.5707963267948966-nan.0i)
  (test -0.0+inf.0i	-1.5707963267948966+nan.0i)
  (test -0.0-inf.0i	-1.5707963267948966-nan.0i)

  (test +inf.0+inf.0i	+1.5707963267948966+nan.0i)
  (test +inf.0-inf.0i	+1.5707963267948966-nan.0i)
  (test -inf.0+inf.0i	-1.5707963267948966+nan.0i)
  (test -inf.0-inf.0i	-1.5707963267948966-nan.0i)

  (test 1.0+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1.0i	+nan.0+nan.0i)
  (test +nan.0+nan.0i	+nan.0+nan.0i)

  (test 1.2+3.4i	1.4720985468699563+0.2652179901713157i)
  (test 1.2-3.4i	1.4720985468699563-0.2652179901713157i)

  (test -1.2+3.4i	-1.4720985468699563+0.2652179901713157i)
  (test -1.2-3.4i	-1.4720985468699563-0.2652179901713157i)

  (test 1.0+3.0i	1.4614618538579256+0.3059438579055289i)
  (test 1.0-3.0i	1.4614618538579256-0.3059438579055289i)

  (test -1.0+3.0i	-1.4614618538579256+0.3059438579055289i)
  (test -1.0-3.0i	-1.4614618538579256-0.3059438579055289i)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
