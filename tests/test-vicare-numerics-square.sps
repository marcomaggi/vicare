;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: square
;;;Date: Thu Nov 29, 2012
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
  (ikarus system $flonums)
  (ikarus system $ratnums)
  (ikarus system $compnums)
  (ikarus system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: square\n")


;;;; helpers

(define C make-rectangular)
(define R real-part)
(define I imag-part)

(define-syntax make-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op ?expected-result)
	(begin
	  (check (?safe-fun   ?op)	=> ?expected-result)
	  (check (?unsafe-fun ?op)	=> ?expected-result)
	  (check (?safe-fun   ?op)	=> (?unsafe-fun ?op))
	  ))))))

(define-syntax make-flonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op ?expected-result)
	(begin
	  (check (?safe-fun   ?op)	(=> flonum=?) ?expected-result)
	  (check (?unsafe-fun ?op)	(=> flonum=?) ?expected-result)
	  (check (?safe-fun   ?op)	(=> flonum=?) (?unsafe-fun ?op))
	  ))))))

(define-syntax make-cflonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op ?expected-result)
	(begin
	  (check (?safe-fun   ?op)	(=> cflonum=?) ?expected-result)
	  (check (?unsafe-fun ?op)	(=> cflonum=?) ?expected-result)
	  (check (?safe-fun   ?op)	(=> cflonum=?) (?unsafe-fun ?op))
	  ))))))

(define-syntax make-compnum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op ?expected-result)
	(begin
	  (check (?safe-fun   ?op)	(=> compnum=?) ?expected-result)
	  (check (?unsafe-fun ?op)	(=> compnum=?) ?expected-result)
	  (check (?safe-fun   ?op)	(=> compnum=?) (?unsafe-fun ?op))
	  ))))))

(define-syntax make-inexact-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op ?expected-result)
	(begin
	  (check (?safe-fun   ?op)	(=> inexact=?) ?expected-result)
	  (check (?unsafe-fun ?op)	(=> inexact=?) ?expected-result)
	  (check (?safe-fun   ?op)	(=> inexact=?) (?unsafe-fun ?op))
	  ))))))

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
	 (fl<? (fl/ (flabs (fl- x y))
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

(define GREATEST-FX	+536870911)
(define LEAST-FX	-536870912)

;;; --------------------------------------------------------------------

(define FX1		+1)
(define FX2		-1)
(define FX3		GREATEST-FX)
(define FX4		LEAST-FX)

;;; --------------------------------------------------------------------

(define BN1		+536870912) ;GREATEST-FX + 1
(define BN2		+536871011) ;GREATEST-FX + 100
(define BN3		-536870913) ;LEAST-FX - 1
(define BN4		-536871012) ;LEAST-FX - 100

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

;;; --------------------------------------------------------------------

(define FL1		+0.0)
(define FL2		-0.0)
(define FL3		+2.123)
(define FL4		-2.123)
(define FL5		+inf.0)
(define FL6		-inf.0)
(define FL7		+nan.0)

;;; --------------------------------------------------------------------

(define CFL01		+0.0+0.0i)
(define CFL02		-0.0+0.0i)
(define CFL03		+0.0-0.0i)
(define CFL04		-0.0-0.0i)

(define CFL05		-1.2-0.0i)
(define CFL06		-1.2+0.0i)
(define CFL07		+0.0-1.2i)
(define CFL08		-0.0-1.2i)

(define CFL09		-1.2-inf.0i)
(define CFL10		-1.2+inf.0i)
(define CFL11		+inf.0-1.2i)
(define CFL12		-inf.0-1.2i)

(define CFL13		-1.2-nan.0i)
(define CFL14		-1.2+nan.0i)
(define CFL15		+nan.0-1.2i)
(define CFL16		-nan.0-1.2i)


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (make-test square $square-fixnum))

  (test 0 0)
  (test 1 1)
  (test -1 1)
  (test FX1 1)
  (test FX2 1)
  (test FX3 288230375077969921)
  (test FX4 288230376151711744)

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (make-test square $square-bignum))

  (test BN1 288230376151711744)
  (test BN2 288230482452162121)
  (test BN3 288230377225453569)
  (test BN4 288230483525904144)

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (make-test square $square-ratnum))

  (test 1/2 1/4)
  (test -1/2 1/4)
  (test RN01 1/15129)
  (test RN02 1/15129)
  (test RN03 1/15129)
  (test RN04 288230376151711744/15129)
  (test RN05 1/288230376151711744)
  (test RN06 1/288230376151711744)
  (test RN07 288230375077969921/288230376151711744)
  (test RN09 1/288230482452162121)
  (test RN10 1/288230482452162121)
  (test RN11 288230375077969921/288230482452162121)
  (test RN12 288230376151711744/288230482452162121)
  (test RN13 1/288230377225453569)
  (test RN14 1/288230377225453569)
  (test RN15 288230375077969921/288230377225453569)
  (test RN16 288230376151711744/288230377225453569)
  (test RN17 1/288230483525904144)
  (test RN18 1/288230483525904144)
  (test RN19 288230375077969921/288230483525904144)
  (test RN20 18014398509481984/18014405220369009)
  (test RN29 288230376151711744/288230375077969921)
  (test RN30 288230482452162121/288230375077969921)
  (test RN31 288230377225453569/288230375077969921)
  (test RN32 288230483525904144/288230375077969921)
  (test RN34 288230482452162121/288230376151711744)
  (test RN35 288230377225453569/288230376151711744)
  (test RN36 18014405220369009/18014398509481984)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-test square $flsquare))

  (test FL1 0.0)
  (test FL2 0.0)
  (test FL3 4.507129000000001)
  (test FL4 4.507129000000001)
  (test FL5 +inf.0)
  (test FL6 +inf.0)
  (test FL7 +nan.0)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test square $square-cflonum))

  (test CFL01 0.0+0.0i)
  (test CFL02 0.0-0.0i)
  (test CFL03 0.0-0.0i)
  (test CFL04 0.0+0.0i)
  (test CFL05 1.44+0.0i)
  (test CFL06 1.44-0.0i)
  (test CFL07 -1.44-0.0i)
  (test CFL08 -1.44+0.0i)
  (test CFL09 -inf.0+inf.0i)
  (test CFL10 -inf.0-inf.0i)
  (test CFL11 +inf.0-inf.0i)
  (test CFL12 +inf.0+inf.0i)
  (test CFL13 +nan.0+nan.0i)
  (test CFL14 +nan.0+nan.0i)
  (test CFL15 +nan.0+nan.0i)
  (test CFL16 +nan.0+nan.0i)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (make-test square $square-compnum))

  (test 10+20i -300+400i)
  (test 1.0+20.0i -399.0+40.0i)
  (test 10.0+2.0i 96.0+40.0i)
  (test 1/2+20i -1599/4+20i)
  (test 10+2/3i 896/9+40/3i)
  (test (C BN1 20) 288230376151711344+21474836480i)
  (test (C 10 BN1) -288230376151711644+10737418240i)

  #t)


;;;; done

(check-report)

;;; end of file
