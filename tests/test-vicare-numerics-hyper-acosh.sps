;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: arc cosh
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
(check-display "*** testing Vicare numerics functions: hyperbolic arc cosine\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acosh ?op)		(=> inexact=?) ?expected)
	 (check ($acosh-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0.0+1.5707963267948966i)
  (test +1	0)
  (test -1	0.0+3.141592653589793i)
  (test +2	1.3169578969248168)
  (test -2	1.3169578969248166+3.141592653589793i)

  (case-word-size
   ((32)
    (test (greatest-fixnum) 20.794415414935713)
    (test (least-fixnum) 20.79441541679836+3.141592653589793i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acosh ?op)		(=> inexact=?) ?expected)
	 (check ($acosh-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 20.794415418661004)
    (test BN2 20.79441543542481)
    (test BN3 20.79441542052365+3.141592653589793i)
    (test BN4 20.794415437287455+3.141592653589793i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acosh ?op)		(=> inexact=?) ?expected)
	 (check ($acosh-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test +1/2 +1.0471975511965979i)
  (test -1/2 +2.0943951023931953i)

  (test RN01 +1.5626661559274333i)
  (test RN02 +1.57892649766236i)
  (test RN03 +1.57892649766236i)
  (test RN04 15.982231061425928+3.141592653589793i)
  (test RN05 0.0+1.5707963249322514i)
  (test RN06 0.0+1.5707963286575417i)
  (test RN07 +6.103515625947391e-5i)
  (test RN09 0.0+1.5707963249322519i)
  (test RN10 0.0+1.5707963286575415i)
  (test RN11 +6.103515157671258e-4i)
  (test RN12 +3.1409853614994967i)
  (test RN13 0.0+1.5707963286575417i)
  (test RN14 0.0+1.5707963249322514i)
  (test RN15 +3.141506336844016i)
  (test RN16 +6.103515625947391e-5i)
  (test RN17 0.0+1.5707963286575415i)
  (test RN18 0.0+1.5707963249322519i)
  (test RN19 +3.1409792579085436i)
  (test RN20 +6.103515152214288e-4i)
  (test RN29 6.10351562405261e-5)
  (test RN30 6.103515537536929e-4)
  (test RN31 8.631674572351474e-5+3.141592653589793i)
  (test RN32 6.133957199357982e-4+3.141592653589793i)
  (test RN34 6.072921275737459e-4+3.141592653589793i)
  (test RN35 6.10351562405261e-5)
  (test RN36 6.103515530260972e-4)

  (test (/ BN1 123) 15.982231063288573)
  (test (/ BN2 123) 15.98223108005238)
  (test (/ BN3 123) 15.982231065151218+3.141592653589793i)
  (test (/ BN4 123) 15.982231081915025+3.141592653589793i)
  (test (/ 123 BN1) +1.5707960976895439i)
  (test (/ 123 BN2) +1.5707960976895474i)
  (test (/ 123 BN3) +1.570796555900249i)
  (test (/ 123 BN4) +1.5707965559002453i)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acosh ?op)		(=> inexact=?) ?expected)
	 (check ($acosh-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	0.0+1.5707963267948966i)
  (test -0.0	0.0+1.5707963267948966i)

  (test +1.0	0.0)
  (test -1.0	0.0+3.141592653589793i)

  (test +2.0	1.3169578969248168)
  (test -2.0	1.3169578969248166+3.141592653589793i)

  (test +inf.0	+inf.0)
  (test -inf.0	+inf.0+3.141592653589793i)

  (test +nan.0	+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acosh ?op)		(=> inexact=?) ?expected)
	 (check ($acosh-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		0.0+0.0i)
  (test -1+0.0i		0.0+3.141592653589793i)
  (test +1-0.0i		0.0-0.0i)
  (test -1-0.0i		0.0-3.141592653589793i)

  (test +0.0+1i		0.881373587019543+1.5707963267948966i)
  (test +0.0-1i		0.881373587019543-1.5707963267948966i)
  (test -0.0+1i		0.881373587019543+1.5707963267948966i)
  (test -0.0-1i		0.881373587019543-1.5707963267948966i)

  (test +1+1.0i		1.0612750619050357+0.9045568943023813i)
  (test +1-1.0i		1.0612750619050357-0.9045568943023813i)
  (test -1.0+1i		1.0612750619050357+2.2370357592874117i)
  (test -1.0-1i		1.0612750619050357-2.2370357592874117i)

  (test 0+inf.0i	+nan.0+nan.0i)
  (test 0-inf.0i	+nan.0+nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i		1.8641615441578825+1.2631926772641853i)
  (test 1-3i		1.8641615441578825-1.2631926772641853i)

  (test -1+3i		1.8641615441578825+1.8783999763256076i)
  (test -1-3i		1.8641615441578825-1.8783999763256076i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acosh ?op)		(=> inexact=?) ?expected)
	 (check ($acosh-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	0.0+1.5707963267948966i)
  (test -0.0+0.0i	0.0+1.5707963267948966i)
  (test +0.0-0.0i	0.0-1.5707963267948966i)
  (test -0.0-0.0i	0.0-1.5707963267948966i)

  (test +1.0+0.0i	0.0+0.0i)
  (test -1.0+0.0i	0.0+3.141592653589793i)
  (test +1.0-0.0i	0.0-0.0i)
  (test -1.0-0.0i	0.0-3.141592653589793i)

  (test +0.0+1.0i	0.881373587019543+1.5707963267948966i)
  (test +0.0-1.0i	0.881373587019543-1.5707963267948966i)
  (test -0.0+1.0i	0.881373587019543+1.5707963267948966i)
  (test -0.0-1.0i	0.881373587019543-1.5707963267948966i)

  (test +1.0+1.0i	1.0612750619050357+0.9045568943023813i)
  (test +1.0-1.0i	1.0612750619050357-0.9045568943023813i)
  (test -1.0+1.0i	1.0612750619050357+2.2370357592874117i)
  (test -1.0-1.0i	1.0612750619050357-2.2370357592874117i)

  (test +inf.0+0.0i	+inf.0+0.0i)
  (test -inf.0+0.0i	+inf.0+3.141592653589793i)
  (test +inf.0-0.0i	+inf.0-0.0i)
  (test -inf.0-0.0i	+inf.0-3.141592653589793i)

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

  (test +1.2+3.4i	1.9904650648910687+1.243053274780371i)
  (test +1.2-3.4i	1.9904650648910687-1.243053274780371i)

  (test -1.2+3.4i	1.9904650648910687+1.898539378809422i)
  (test -1.2-3.4i	1.9904650648910687-1.898539378809422i)

  (test +1.0+3.0i	1.8641615441578825+1.2631926772641853i)
  (test +1.0-3.0i	1.8641615441578825-1.2631926772641853i)

  (test -1.0+3.0i	1.8641615441578825+1.8783999763256076i)
  (test -1.0-3.0i	1.8641615441578825-1.8783999763256076i)

  #t)


;;;; done

(check-report)

;;; end of file
