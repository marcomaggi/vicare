;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: arc sin
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


#!r6rs
(import (vicare)
  (libtest numerics-helpers)
  (vicare system $numerics)
  (vicare checks)
  (only (vicare platform words)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: trigonometric arc sine\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asin ?op)		(=> inexact=?) ?expected)
	 (check ($asin-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	0)
  (test +1	1.5707963267948966)
  (test -1	-1.5707963267948966)
  (test +2	1.5707963267948966-1.3169578969248166i)
  (test -2	-1.5707963267948966+1.3169578969248166i)

  (case-word-size
   ((32)
    (test (greatest-fixnum)	+1.5707963267948966-20.794415414935713i)
    (test (least-fixnum)	-1.5707963267948966+20.79441541679836i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asin ?op)		(=> inexact=?) ?expected)
	 (check ($asin-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 1.5707963267948966-20.794415418661004i)
    (test BN2 1.5707963267948966-20.79441543542481i)
    (test BN3 -1.5707963267948966+20.79441542052365i)
    (test BN4 -1.5707963267948966+20.794415437287455i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asin ?op)		(=> inexact=?) ?expected)
	 (check ($asin-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 0.5235987755982989)
  (test -1/2 -0.5235987755982989)

  (test RN01 0.008130170867463515)
  (test RN02 -0.008130170867463515)
  (test RN03 -0.008130170867463515)
  (test RN04 -1.5707963267948966+15.982231061425928i)
  (test RN05 1.862645149230957e-9)
  (test RN06 -1.862645149230957e-9)
  (test RN07 1.5707352916386372)
  (test RN09 1.862644805755772e-9)
  (test RN10 -1.862644805755772e-9)
  (test RN11 1.5701859752791294)
  (test RN12 -1.5701890347046001)
  (test RN13 -1.86264514576151e-9)
  (test RN14 1.86264514576151e-9)
  (test RN15 -1.5707100100491196)
  (test RN16 1.5707352916386372)
  (test RN17 -1.8626448022863264e-9)
  (test RN18 1.8626448022863264e-9)
  (test RN19 -1.5701829311136468)
  (test RN20 1.5701859752796752)
  (test RN29 1.5707963267948966-6.10351562405261e-5i)
  (test RN30 1.5707963267948966-6.103515537536929e-4i)
  (test RN31 -1.5707963267948966+8.631674572351474e-5i)
  (test RN32 -1.5707963267948966+6.133957199357982e-4i)
  (test RN34 -1.5707963267948966+6.072921275737459e-4i)
  (test RN35 1.5707963267948966-6.10351562405261e-5i)
  (test RN36 1.5707963267948966-6.103515530260972e-4i)

  (case-word-size
   ((32)
    (test (/ BN1 123) 1.5707963267948966-15.982231063288573i)
    (test (/ BN2 123) 1.5707963267948966-15.98223108005238i)
    (test (/ BN3 123) -1.5707963267948966+15.982231065151218i)
    (test (/ BN4 123) -1.5707963267948966+15.982231081915025i)
    (test (/ 123 BN1) 2.2910535292866775e-7)
    (test (/ 123 BN2) 2.2910534908799006e-7)
    (test (/ 123 BN3) -2.2910535250192578e-7)
    (test (/ 123 BN4) -2.291053486612481e-7))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asin ?op)		(=> inexact=?) ?expected)
	 (check ($asin-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	+0.0)
  (test -0.0	-0.0)

  (test +1.0	+1.5707963267948966)
  (test -1.0	-1.5707963267948966)

  (test +2.0	+1.5707963267948966-1.3169578969248166i)
  (test -2.0	-1.5707963267948966+1.3169578969248166i)

  (test +inf.0	+1.5707963267948966-inf.0i)
  (test -inf.0	-1.5707963267948966+inf.0i)
  (test +nan.0	+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asin ?op)		(=> inexact=?) ?expected)
	 (check ($asin-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		+1.5707963267948966+0.0i)
  (test -1+0.0i		-1.5707963267948966+0.0i)
  (test +1-0.0i		+1.5707963267948966+0.0i)
  (test -1-0.0i		-1.5707963267948966+0.0i)

  (test +0.0+1i		+0.0+0.881373587019543i)
  (test +0.0-1i		+0.0-0.881373587019543i)
  (test -0.0+1i		+0.0+0.881373587019543i)
  (test -0.0-1i		+0.0-0.881373587019543i)

  (test +1+1.0i		+0.6662394324925153+1.0612750619050357i)
  (test +1-1.0i		+0.6662394324925153-1.0612750619050357i)
  (test -1.0+1i		-0.6662394324925153+1.0612750619050357i)
  (test -1.0-1i		-0.6662394324925153-1.0612750619050357i)

  (test 0+inf.0i	+0.0+inf.0i)
  (test 0-inf.0i	+0.0-inf.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i		+0.30760364953071123+1.8641615441578825i)
  (test 1-3i		+0.30760364953071123-1.8641615441578825i)

  (test -1+3i		-0.30760364953071123+1.8641615441578825i)
  (test -1-3i		-0.30760364953071123-1.8641615441578825i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (asin ?op)		(=> inexact=?) ?expected)
	 (check ($asin-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i +0.0+0.0i)
  (test -0.0+0.0i +0.0+0.0i)
  (test +0.0-0.0i +0.0-0.0i)
  (test -0.0-0.0i +0.0-0.0i)

  (test +1.0+0.0i 1.5707963267948966+0.0i)
  (test -1.0+0.0i -1.5707963267948966+0.0i)
  (test +1.0-0.0i 1.5707963267948966-0.0i)
  (test -1.0-0.0i -1.5707963267948966-0.0i)

  (test +0.0+1.0i +0.0+0.881373587019543i)
  (test +0.0-1.0i +0.0-0.881373587019543i)
  (test -0.0+1.0i +0.0+0.881373587019543i)
  (test -0.0-1.0i +0.0-0.881373587019543i)

  (test 1.0+1.0i 0.6662394324925153+1.0612750619050357i)
  (test 1.0-1.0i 0.6662394324925153-1.0612750619050357i)
  (test -1.0+1.0i -0.6662394324925153+1.0612750619050357i)
  (test -1.0-1.0i -0.6662394324925153-1.0612750619050357i)

  (test +inf.0+0.0i +nan.0+inf.0i)
  (test -inf.0+0.0i +nan.0+inf.0i)
  (test +inf.0-0.0i +nan.0-inf.0i)
  (test -inf.0-0.0i +nan.0-inf.0i)

  (test +0.0+inf.0i +0.0+inf.0i)
  (test +0.0-inf.0i +0.0-inf.0i)
  (test -0.0+inf.0i +0.0+inf.0i)
  (test -0.0-inf.0i +0.0-inf.0i)

  (test +inf.0+inf.0i +nan.0+inf.0i)
  (test +inf.0-inf.0i +nan.0-inf.0i)
  (test -inf.0+inf.0i +nan.0+inf.0i)
  (test -inf.0-inf.0i +nan.0-inf.0i)

  (test 1.0+nan.0i +nan.0+nan.0i)
  (test +nan.0+1.0i +nan.0+nan.0i)
  (test +nan.0+nan.0i +nan.0+nan.0i)

  (test 1.2+3.4i 0.3277430520145252+1.9904650648910687i)
  (test 1.2-3.4i 0.3277430520145252-1.9904650648910687i)

  (test -1.2+3.4i -0.3277430520145252+1.9904650648910687i)
  (test -1.2-3.4i -0.3277430520145252-1.9904650648910687i)

  (test 1.0+3.0i 0.30760364953071123+1.8641615441578825i)
  (test 1.0-3.0i 0.30760364953071123-1.8641615441578825i)

  (test -1.0+3.0i -0.30760364953071123+1.8641615441578825i)
  (test -1.0-3.0i -0.30760364953071123-1.8641615441578825i)

  #t)


;;;; done

(check-report)

;;; end of file
