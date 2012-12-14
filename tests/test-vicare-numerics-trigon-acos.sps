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
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (ikarus system $numerics)
  (vicare checks)
  (only (vicare syntactic-extensions)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: trigonometric arc cosine\n")


;;;; helpers

(define C make-rectangular)
(define R real-part)
(define I imag-part)

(define-syntax make-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	=> ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	=> ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	=> ?expected-result)
	  ))))))

(define-syntax make-flonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  ))))))

(define-syntax make-cflonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-func ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  ))))))

(define-syntax make-compnum-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  ))))))

(define-syntax make-inexact-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  ))))))

;;; --------------------------------------------------------------------

(define-syntax catch-implementation-restriction
  (syntax-rules ()
    ((_ ?message . ?body)
     (check
	 (guard (E ((implementation-restriction-violation? E)
		    (condition-message E))
		   (else E))
	   (begin . ?body))
       => ?message))))

;;; --------------------------------------------------------------------

(define (flonum=? x y)
  (cond ((flnan? x)
	 (flnan? y))
	((flzero?/positive x)
	 (flzero?/positive y))
	((flzero?/negative x)
	 (flzero?/negative y))
	((fl=? x y))))

(define (cflonum=? x y)
  (and (flonum=? (real-part x) (real-part y))
       (flonum=? (imag-part x) (imag-part y))))

(define (compnum=? x y)
  (cond ((and (cflonum? x)
	      (cflonum? y))
	 (cflonum=? x y))
	((and (flonum? x)
	      (flonum? y))
	 (flonum=? x y))
	(else
	 (= x y))))

;;; --------------------------------------------------------------------

(define (inexact=? x y)
  (cond ((and (cflonum? x)
	      (cflonum? y))
	 (cflonum-quasi=? x y))
	((and (flonum? x)
	      (flonum? y))
	 (flonum-quasi=? x y))
	((or (compnum? x)
	     (cflonum? x)
	     (compnum? y)
	     (cflonum? y))
	 (complex-quasi=? x y))
	(else
	 (= x y))))

(define (flonum-quasi=? x y)
  (cond ((flnan? x)
	 (flnan? y))
	((infinite? x)
	 (fl=? x y))
	;;Here we cannot consider +0.0 different fro -0.0.
	((flzero? x)
	 (flzero? y))
	(else
	 (fl<? (flabs (fl- x y))
	       1e-5)
	 #;(fl<? (fl/ (flabs (fl- x y))
		    (flabs x))
	       1e-5))))

(define (cflonum-quasi=? x y)
  (and (flonum-quasi=? (real-part x) (real-part y))
       (flonum-quasi=? (imag-part x) (imag-part y))))

(define (complex-quasi=? x y)
  (let ((x.rep (real-part x))
	(x.imp (imag-part x))
	(y.rep (real-part y))
	(y.imp (imag-part y)))
    (and (inexact=? x.rep y.rep)
	 (inexact=? x.imp y.imp))))


;;;; constants

(define SMALLEST-POSITIVE-BIGNUM	(-    (least-fixnum)))
(define SMALLEST-NEGATIVE-BIGNUM	(+ -1 (least-fixnum)))

(define BN1	(+ +1  SMALLEST-POSITIVE-BIGNUM))
(define BN2	(+ +10 SMALLEST-POSITIVE-BIGNUM))
(define BN3	(+ -1  SMALLEST-NEGATIVE-BIGNUM))
(define BN4	(+ -10 SMALLEST-NEGATIVE-BIGNUM))

;;; --------------------------------------------------------------------

(define RN01		1/123			#;(/ FX1 123))
(define RN02		-1/123			#;(/ FX2 123))
(define RN03		-1/123			#;(/ FX2 123))
(define RN04		-536870912/123		#;(/ FX4 123))

(define RN05		1/536870912		#;(/ FX1 BN1))
(define RN06		-1/536870912		#;(/ FX2 BN1))
(define RN07		536870911/536870912	#;(/ FX3 BN1))
;;(define RN08		-1			#;(/ FX4 BN1)) ;not a ratnum

(define RN09		1/536871011		#;(/ FX1 BN2))
(define RN10		-1/536871011		#;(/ FX2 BN2))
(define RN11		536870911/536871011	#;(/ FX3 BN2))
(define RN12		-536870912/536871011	#;(/ FX4 BN2))

(define RN13		-1/536870913		#;(/ FX1 BN3))
(define RN14		1/536870913		#;(/ FX2 BN3))
(define RN15		-536870911/536870913	#;(/ FX3 BN3))
(define RN16		536870912/536870913	#;(/ FX4 BN3))

(define RN17		-1/536871012		#;(/ FX1 BN4))
(define RN18		1/536871012		#;(/ FX2 BN4))
(define RN19		-536870911/536871012	#;(/ FX3 BN4))
(define RN20		134217728/134217753	#;(/ FX4 BN4))

;;(define RN21		536870912		#;(/ BN1 FX1)) ;not a ratnum
;;(define RN22		536871011		#;(/ BN2 FX1)) ;not a ratnum
;;(define RN23		-536870913		#;(/ BN3 FX1)) ;not a ratnum
;;(define RN24		-536871012		#;(/ BN4 FX1)) ;not a ratnum

;;(define RN25		-536870912		#;(/ BN1 FX2)) ;not a ratnum
;;(define RN26		-536871011		#;(/ BN2 FX2)) ;not a ratnum
;;(define RN27		536870913		#;(/ BN3 FX2)) ;not a ratnum
;;(define RN28		536871012		#;(/ BN4 FX2)) ;not a ratnum

(define RN29		536870912/536870911	#;(/ BN1 FX3))
(define RN30		536871011/536870911	#;(/ BN2 FX3))
(define RN31		-536870913/536870911	#;(/ BN3 FX3))
(define RN32		-536871012/536870911	#;(/ BN4 FX3))

;;(define RN33		-1			#;(/ BN1 FX4)) ;not a ratnum
(define RN34		-536871011/536870912	#;(/ BN2 FX4))
(define RN35		536870913/536870912	#;(/ BN3 FX4))
(define RN36		134217753/134217728	#;(/ BN4 FX4))


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
  (test +1-0.0i		0.0+0.0i)
  (test -1-0.0i		3.141592653589793+0.0i)

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
