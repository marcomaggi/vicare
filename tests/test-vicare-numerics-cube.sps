;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: cube
;;;Date: Wed Dec  5, 2012
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
(check-display "*** testing Vicare numerics functions: cube\n")


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

;;; --------------------------------------------------------------------

(define (%cube x)
  (* x x x))


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
    (make-test cube $cube-fixnum))

  (test 0	0)
  (test +1	+1)
  (test -1	-1)
  (test +2	+8)
  (test -2	-8)
  (test +3	+27)
  (test -3	-27)
  (test FX1 (%cube FX1))
  (test FX2 (%cube FX2))
  (test FX3 (%cube FX3))
  (test FX4 (%cube FX4))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (make-test cube $cube-bignum))

  (test BN1 (%cube BN1))
  (test BN2 (%cube BN2))
  (test BN3 (%cube BN3))
  (test BN4 (%cube BN4))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (make-test cube $cube-ratnum))

  (test +1/2	(%cube +1/2))
  (test -1/2	(%cube -1/2))

  (test RN01	(%cube RN01))
  (test RN02	(%cube RN02))
  (test RN03	(%cube RN03))
  (test RN04	(%cube RN04))
  (test RN05	(%cube RN05))
  (test RN06	(%cube RN06))
  (test RN07	(%cube RN07))
  (test RN09	(%cube RN09))
  (test RN10	(%cube RN10))
  (test RN11	(%cube RN11))
  (test RN12	(%cube RN12))
  (test RN13	(%cube RN13))
  (test RN14	(%cube RN14))
  (test RN15	(%cube RN15))
  (test RN16	(%cube RN16))
  (test RN17	(%cube RN17))
  (test RN18	(%cube RN18))
  (test RN19	(%cube RN19))
  (test RN20	(%cube RN20))
  (test RN29	(%cube RN29))
  (test RN30	(%cube RN30))
  (test RN31	(%cube RN31))
  (test RN32	(%cube RN32))
  (test RN34	(%cube RN34))
  (test RN35	(%cube RN35))
  (test RN36	(%cube RN36))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-test cube $flcube))

  (test FL1	(%cube FL1))
  (test FL2	(%cube FL2))
  (test FL3	(%cube FL3))
  (test FL4	(%cube FL4))
  (test FL5	(%cube FL5))
  (test FL6	(%cube FL6))
  (test FL7	(%cube FL7))

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test cube $cube-cflonum))

  (test CFL01	(%cube CFL01))
  (test CFL02	-0.0+0.0i #;(%cube CFL02)) ;different!!!
  (test CFL03	(%cube CFL03))
  (test CFL04	-0.0-0.0i #;(%cube CFL04)) ;different!!!
  (test CFL05	(%cube CFL05))
  (test CFL06	(%cube CFL06))
  (test CFL07	(%cube CFL07))
  (test CFL08	(%cube CFL08))
  (test CFL09	(%cube CFL09))
  (test CFL10	(%cube CFL10))
  (test CFL11	(%cube CFL11))
  (test CFL12	(%cube CFL12))
  (test CFL13	(%cube CFL13))
  (test CFL14	(%cube CFL14))
  (test CFL15	(%cube CFL15))
  (test CFL16	(%cube CFL16))

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (make-test cube $cube-compnum))

  (test 0+0.0i		(%cube 0+0.0i))
  (test 0-0.0i		(%cube 0-0.0i))

  (test 10+20i		(%cube 10+20i))
  (test 1.0+20.0i	(%cube 1.0+20.0i))
  (test 10.0+2.0i	(%cube 10.0+2.0i))
  (test 1/2+20i		(%cube 1/2+20i))
  (test 10+2/3i		(%cube 10+2/3i))
  (test (C BN1 20)	(%cube (C BN1 20)))
  (test (C 10 BN1)	(%cube (C 10 BN1)))
  (test (C BN1 2.0)	(%cube (C BN1 2.0)))
  (test (C 1.0 BN1)	(%cube (C 1.0 BN1)))

  #t)


;;;; done

(check-report)

;;; end of file
