;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: arc cos
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
(check-display "*** testing Vicare numerics functions: trigonometric arc cosine\n")


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acos ?op)		(=> inexact=?) ?expected)
	 (check ($acos-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0	1.5707963267948966)
  (test +1	0)
  (test -1	3.141592653589793)
  (test +2	0.0+1.3169578969248166i)
  (test -2	3.141592653589793-1.3169578969248166i)

  (case-word-size
   ((32)
    (test (greatest-fixnum)	0.0+20.794415414935713i)
    (test (least-fixnum)	3.141592653589793-20.79441541679836i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acos ?op)		(=> inexact=?) ?expected)
	 (check ($acos-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1 0.0+20.794415418661004i)
    (test BN2 0.0+20.79441543542481i)
    (test BN3 3.141592653589793-20.79441542052365i)
    (test BN4 3.141592653589793-20.794415437287455i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acos ?op)		(=> inexact=?) ?expected)
	 (check ($acos-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test 1/2 1.0471975511965979)
  (test -1/2 2.0943951023931957)

  (test RN01 1.562666155927433)
  (test RN02 1.57892649766236)
  (test RN03 1.57892649766236)
  (test RN04 3.141592653589793-15.982231061425928i)
  (test RN05 1.5707963249322514)
  (test RN06 1.5707963286575417)
  (test RN07 6.103515625947391e-5)
  (test RN09 1.5707963249322519)
  (test RN10 1.5707963286575415)
  (test RN11 6.103515157671153e-4)
  (test RN12 3.1409853614994967)
  (test RN13 1.5707963286575417)
  (test RN14 1.5707963249322514)
  (test RN15 3.141506336844016)
  (test RN16 6.103515625947391e-5)
  (test RN17 1.5707963286575415)
  (test RN18 1.5707963249322519)
  (test RN19 3.1409792579085436)
  (test RN20 6.103515152214183e-4)
  (test RN29 0.0+6.10351562405261e-5i)
  (test RN30 0.0+6.103515537536929e-4i)
  (test RN31 3.141592653589793-8.631674572351474e-5i)
  (test RN32 3.141592653589793-6.133957199357982e-4i)
  (test RN34 3.141592653589793-6.072921275737459e-4i)
  (test RN35 0.0+6.10351562405261e-5i)
  (test RN36 0.0+6.103515530260972e-4i)

  (case-word-size
   ((32)
    (test (/ BN1 123) 0.0+15.982231063288573i)
    (test (/ BN2 123) 0.0+15.98223108005238i)
    (test (/ BN3 123) 3.141592653589793-15.982231065151218i)
    (test (/ BN4 123) 3.141592653589793-15.982231081915025i)
    (test (/ 123 BN1) 1.5707960976895436)
    (test (/ 123 BN2) 1.5707960976895474)
    (test (/ 123 BN3) 1.570796555900249)
    (test (/ 123 BN4) 1.5707965559002453))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acos ?op)		(=> inexact=?) ?expected)
	 (check ($acos-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0	1.5707963267948966)
  (test -0.0	1.5707963267948966)

  (test +1.0	0.0)
  (test -1.0	3.141592653589793)

  (test +2.0	0.0+1.3169578969248166i)
  (test -2.0	3.141592653589793-1.3169578969248166i)

  (test +inf.0	0.0+inf.0i)
  (test -inf.0	3.141592653589793-inf.0i)

  (test +nan.0	+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acos ?op)		(=> inexact=?) ?expected)
	 (check ($acos-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		0.0-0.0i)
  (test -1+0.0i		3.141592653589793-0.0i)
  (test +1-0.0i		0.0-0.0i)
  (test -1-0.0i		3.141592653589793-0.0i)

  (test +0.0+1i		1.5707963267948966-0.881373587019543i)
  (test +0.0-1i		1.5707963267948966+0.881373587019543i)
  (test -0.0+1i		1.5707963267948966-0.881373587019543i)
  (test -0.0-1i		1.5707963267948966+0.881373587019543i)

  (test +1+1.0i		0.9045568943023813-1.0612750619050357i)
  (test +1-1.0i		0.9045568943023813+1.0612750619050357i)
  (test -1.0+1i		2.2370357592874117-1.0612750619050357i)
  (test -1.0-1i		2.2370357592874117+1.0612750619050357i)

  (test 0+inf.0i	1.5707963267948966-inf.0i)
  (test 0-inf.0i	1.5707963267948966+inf.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i		1.2631926772641855-1.8641615441578825i)
  (test 1-3i		1.2631926772641855+1.8641615441578825i)

  (test -1+3i		1.8783999763256076-1.8641615441578825i)
  (test -1-3i		1.8783999763256076+1.8641615441578825i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (acos ?op)		(=> inexact=?) ?expected)
	 (check ($acos-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i	1.5707963267948966-0.0i)
  (test -0.0+0.0i	1.5707963267948966-0.0i)
  (test +0.0-0.0i	1.5707963267948966+0.0i)
  (test -0.0-0.0i	1.5707963267948966+0.0i)

  (test +1.0+0.0i	0.0-0.0i)
  (test -1.0+0.0i	3.141592653589793-0.0i)
  (test +1.0-0.0i	0.0+0.0i)
  (test -1.0-0.0i	3.141592653589793+0.0i)

  (test +0.0+1.0i	1.5707963267948966-0.881373587019543i)
  (test +0.0-1.0i	1.5707963267948966+0.881373587019543i)
  (test -0.0+1.0i	1.5707963267948966-0.881373587019543i)
  (test -0.0-1.0i	1.5707963267948966+0.881373587019543i)

  (test +1.0+1.0i	0.9045568943023813-1.0612750619050357i)
  (test +1.0-1.0i	0.9045568943023813+1.0612750619050357i)
  (test -1.0+1.0i	2.2370357592874117-1.0612750619050357i)
  (test -1.0-1.0i	2.2370357592874117+1.0612750619050357i)

  (test +inf.0+0.0i	0.0-inf.0i)
  (test -inf.0+0.0i	3.141592653589793-inf.0i)
  (test +inf.0-0.0i	0.0+inf.0i)
  (test -inf.0-0.0i	3.141592653589793+inf.0i)

  (test +0.0+inf.0i	+1.5707963267948966-inf.0i)
  (test +0.0-inf.0i	+1.5707963267948966+inf.0i)
  (test -0.0+inf.0i	+1.5707963267948966-inf.0i)
  (test -0.0-inf.0i	+1.5707963267948966+inf.0i)

  (test +inf.0+inf.0i	+nan.0-inf.0i)
  (test +inf.0-inf.0i	+nan.0+inf.0i)
  (test -inf.0+inf.0i	+nan.0-inf.0i)
  (test -inf.0-inf.0i	+nan.0+inf.0i)

  (test 1.0+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1.0i	+nan.0+nan.0i)
  (test +nan.0+nan.0i	+nan.0+nan.0i)

  (test +1.2+3.4i	1.243053274780371-1.9904650648910687i)
  (test +1.2-3.4i	1.243053274780371+1.9904650648910687i)
  (test -1.2+3.4i	1.898539378809422-1.9904650648910687i)
  (test -1.2-3.4i	1.898539378809422+1.9904650648910687i)

  (test +1.0+3.0i	1.2631926772641855-1.8641615441578825i)
  (test +1.0-3.0i	1.2631926772641855+1.8641615441578825i)
  (test -1.0+3.0i	1.8783999763256076-1.8641615441578825i)
  (test -1.0-3.0i	1.8783999763256076+1.8641615441578825i)

  #t)


;;;; done

(check-report)

;;; end of file
