;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: add
;;;Date: Tue Nov 27, 2012
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
  (ikarus system $ratnums)
  (ikarus system $compnums)
  (ikarus system $numerics)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: addition\n")


;;;; helpers

(define-syntax make-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	=> ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	=> ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	=> (?unsafe-fun ?op1 ?op2))
	  ))))))

(define-syntax make-flonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	(=> flonum=?) (?unsafe-fun ?op1 ?op2))
	  ))))))

(define-syntax make-cflonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	(=> cflonum=?) (?unsafe-fun ?op1 ?op2))
	  ))))))

(define-syntax make-compnum-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	(=> compnum=?) (?unsafe-fun ?op1 ?op2))
	  ))))))

(define-syntax make-inexact-test
  (syntax-rules ()
    ((_ ?safe-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  (check (?safe-fun   ?op1 ?op2)	(=> inexact=?) (?unsafe-fun ?op1 ?op2))
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
	(else
	 (= x y))))

(define (flonum-quasi=? x y)
  (cond ((flnan? x)
	 (flnan? y))
	((flzero?/positive x)
	 (flzero?/positive y))
	((flzero?/negative x)
	 (flzero?/negative y))
	(else
	 (fl<? (flabs (fl- x y)) 1e-8))))

(define (cflonum-quasi=? x y)
  (and (flonum-quasi=? (real-part x) (real-part y))
       (flonum-quasi=? (imag-part x) (imag-part y))))


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


;;;; compnum constants

(define CN001		(make-rectangular FX1 FX1))
(define CN002		(make-rectangular FX2 FX1))
(define CN003		(make-rectangular FX3 FX1))
(define CN004		(make-rectangular FX4 FX1))

(define CN005		(make-rectangular FX1 FX2))
(define CN006		(make-rectangular FX2 FX2))
(define CN007		(make-rectangular FX3 FX2))
(define CN008		(make-rectangular FX4 FX2))

(define CN009		(make-rectangular FX1 FX3))
(define CN010		(make-rectangular FX2 FX3))
(define CN011		(make-rectangular FX3 FX3))
(define CN012		(make-rectangular FX4 FX3))

(define CN013		(make-rectangular FX1 FX4))
(define CN014		(make-rectangular FX2 FX4))
(define CN015		(make-rectangular FX3 FX4))
(define CN016		(make-rectangular FX4 FX4))

;;; --------------------------------------------------------------------

(define CN017		(make-rectangular BN1 FX1))
(define CN018		(make-rectangular BN2 FX1))
(define CN019		(make-rectangular BN3 FX1))
(define CN020		(make-rectangular BN4 FX1))

(define CN021		(make-rectangular BN1 FX2))
(define CN022		(make-rectangular BN2 FX2))
(define CN023		(make-rectangular BN3 FX2))
(define CN024		(make-rectangular BN4 FX2))

(define CN025		(make-rectangular BN1 FX3))
(define CN026		(make-rectangular BN2 FX3))
(define CN027		(make-rectangular BN3 FX3))
(define CN028		(make-rectangular BN4 FX3))

(define CN029		(make-rectangular BN1 FX4))
(define CN030		(make-rectangular BN2 FX4))
(define CN031		(make-rectangular BN3 FX4))
(define CN032		(make-rectangular BN4 FX4))

;;; --------------------------------------------------------------------

(define CN033		(make-rectangular FX1 BN1))
(define CN034		(make-rectangular FX2 BN1))
(define CN035		(make-rectangular FX3 BN1))
(define CN036		(make-rectangular FX4 BN1))

(define CN037		(make-rectangular FX1 BN2))
(define CN038		(make-rectangular FX2 BN2))
(define CN039		(make-rectangular FX3 BN2))
(define CN040		(make-rectangular FX4 BN2))

(define CN041		(make-rectangular FX1 BN3))
(define CN042		(make-rectangular FX2 BN3))
(define CN043		(make-rectangular FX3 BN3))
(define CN044		(make-rectangular FX4 BN3))

(define CN045		(make-rectangular FX1 BN4))
(define CN046		(make-rectangular FX2 BN4))
(define CN047		(make-rectangular FX3 BN4))
(define CN048		(make-rectangular FX4 BN4))

;;; --------------------------------------------------------------------

(define CN049		(make-rectangular BN1 FL1))
(define CN050		(make-rectangular BN2 FL1))
(define CN051		(make-rectangular BN3 FL1))
(define CN052		(make-rectangular BN4 FL1))

(define CN053		(make-rectangular BN1 FL2))
(define CN054		(make-rectangular BN2 FL2))
(define CN055		(make-rectangular BN3 FL2))
(define CN056		(make-rectangular BN4 FL2))

(define CN057		(make-rectangular BN1 FL3))
(define CN058		(make-rectangular BN2 FL3))
(define CN059		(make-rectangular BN3 FL3))
(define CN060		(make-rectangular BN4 FL3))

(define CN061		(make-rectangular BN1 FL4))
(define CN062		(make-rectangular BN2 FL4))
(define CN063		(make-rectangular BN3 FL4))
(define CN064		(make-rectangular BN4 FL4))

;;; --------------------------------------------------------------------

(define CN065		(make-rectangular FL1 BN1))
(define CN066		(make-rectangular FL2 BN1))
(define CN067		(make-rectangular FL3 BN1))
(define CN068		(make-rectangular FL4 BN1))

(define CN069		(make-rectangular FL1 BN2))
(define CN070		(make-rectangular FL2 BN2))
(define CN071		(make-rectangular FL3 BN2))
(define CN072		(make-rectangular FL4 BN2))

(define CN073		(make-rectangular FL1 BN3))
(define CN074		(make-rectangular FL2 BN3))
(define CN075		(make-rectangular FL3 BN3))
(define CN076		(make-rectangular FL4 BN3))

(define CN077		(make-rectangular FL1 BN4))
(define CN078		(make-rectangular FL2 BN4))
(define CN079		(make-rectangular FL3 BN4))
(define CN080		(make-rectangular FL4 BN4))

;;; --------------------------------------------------------------------

(define CN081		(make-rectangular RN01 FL1))
(define CN082		(make-rectangular RN02 FL1))
(define CN083		(make-rectangular RN03 FL1))
(define CN084		(make-rectangular RN04 FL1))

(define CN085		(make-rectangular RN01 FL2))
(define CN086		(make-rectangular RN02 FL2))
(define CN087		(make-rectangular RN03 FL2))
(define CN088		(make-rectangular RN04 FL2))

(define CN089		(make-rectangular RN01 FL3))
(define CN090		(make-rectangular RN02 FL3))
(define CN091		(make-rectangular RN03 FL3))
(define CN092		(make-rectangular RN04 FL3))

(define CN093		(make-rectangular RN01 FL4))
(define CN094		(make-rectangular RN02 FL4))
(define CN095		(make-rectangular RN03 FL4))
(define CN096		(make-rectangular RN04 FL4))

;;; --------------------------------------------------------------------

(define CN097		(make-rectangular FL1 RN01))
(define CN098		(make-rectangular FL2 RN01))
(define CN099		(make-rectangular FL3 RN01))
(define CN100		(make-rectangular FL4 RN01))

(define CN101		(make-rectangular FL1 RN02))
(define CN102		(make-rectangular FL2 RN02))
(define CN103		(make-rectangular FL3 RN02))
(define CN104		(make-rectangular FL4 RN02))

(define CN105		(make-rectangular FL1 RN03))
(define CN106		(make-rectangular FL2 RN03))
(define CN107		(make-rectangular FL3 RN03))
(define CN108		(make-rectangular FL4 RN03))

(define CN109		(make-rectangular FL1 RN04))
(define CN110		(make-rectangular FL2 RN04))
(define CN111		(make-rectangular FL3 RN04))
(define CN112		(make-rectangular FL4 RN04))


(parametrise ((check-test-name	'specials))

  (check (+ 1 -1.1-0.0i)       => -0.10000000000000009+0.0i)
  (check (- 1 +1.1+0.0i)       => -0.10000000000000009+0.0i)

  (check (+  0.0 -0.0)           =>  0.0)
  (check (+ -0.0  0.0)           =>  0.0)
  (check (+  0.0  0.0)           =>  0.0)
  (check (+ -0.0 -0.0)           => -0.0)

  #t)


(parametrise ((check-test-name	'fixnums))

  (let-syntax ((test (make-test + $add-fixnum-fixnum)))
    (test 0	0		0)
    (test 0	+1		+1)
    (test +1	0		+1)
    (test 0	-1		-1)
    (test -1	0		-1)

    (test FX1 FX1 2)
    (test FX2 FX1 0)
    (test FX3 FX1 536870912)
    (test FX4 FX1 -536870911)

    (test FX1 FX2 0)
    (test FX2 FX2 -2)
    (test FX3 FX2 536870910)
    (test FX4 FX2 -536870913)

    (test FX1 FX3 536870912)
    (test FX2 FX3 536870910)
    (test FX3 FX3 1073741822)
    (test FX4 FX3 -1)

    (test FX1 FX4 -536870911)
    (test FX2 FX4 -536870913)
    (test FX3 FX4 -1)
    (test FX4 FX4 -1073741824)
    #f)

  (let-syntax ((test (make-test + $add-fixnum-bignum)))
    (test FX1 BN1 536870913)
    (test FX2 BN1 536870911)
    (test FX3 BN1 1073741823)
    (test FX4 BN1 0)

    (test FX1 BN2 536871012)
    (test FX2 BN2 536871010)
    (test FX3 BN2 1073741922)
    (test FX4 BN2 99)

    (test FX1 BN3 -536870912)
    (test FX2 BN3 -536870914)
    (test FX3 BN3 -2)
    (test FX4 BN3 -1073741825)

    (test FX1 BN4 -536871011)
    (test FX2 BN4 -536871013)
    (test FX3 BN4 -101)
    (test FX4 BN4 -1073741924)
    #f)

  (let-syntax ((test (make-test + $add-fixnum-ratnum)))
    (test FX1 RN01 124/123)
    (test FX2 RN01 -122/123)
    (test FX3 RN01 66035122054/123)
    (test FX4 RN01 -66035122175/123)

    (test FX1 RN02 122/123)
    (test FX2 RN02 -124/123)
    (test FX3 RN02 66035122052/123)
    (test FX4 RN02 -66035122177/123)

    (test FX1 RN03 122/123)
    (test FX2 RN03 -124/123)
    (test FX3 RN03 66035122052/123)
    (test FX4 RN03 -66035122177/123)

    (test FX1 RN04 -536870789/123)
    (test FX2 RN04 -536871035/123)
    (test FX3 RN04 65498251141/123)
    (test FX4 RN04 -66571993088/123)
    #f)

  (let-syntax ((test (make-flonum-test + $add-fixnum-flonum)))
    (test FX1 FL1 1.0)
    (test FX2 FL1 -1.0)
    (test FX3 FL1 536870911.0)
    (test FX4 FL1 -536870912.0)

    (test FX1 FL2 1.0)
    (test FX2 FL2 -1.0)
    (test FX3 FL2 536870911.0)
    (test FX4 FL2 -536870912.0)

    (test FX1 FL3 3.123)
    (test FX2 FL3 1.1230000000000002)
    (test FX3 FL3 536870913.123)
    (test FX4 FL3 -536870909.877)

    (test FX1 FL4 -1.1230000000000002)
    (test FX2 FL4 -3.123)
    (test FX3 FL4 536870908.877)
    (test FX4 FL4 -536870914.123)
    #f)

  (let-syntax ((test (make-cflonum-test + $add-fixnum-cflonum)))
    (test FX1 CFL01 1.0+0.0i)
    (test FX2 CFL01 -1.0+0.0i)
    (test FX3 CFL01 536870911.0+0.0i)
    (test FX4 CFL01 -536870912.0+0.0i)

    (test FX1 CFL02 1.0+0.0i)
    (test FX2 CFL02 -1.0+0.0i)
    (test FX3 CFL02 536870911.0+0.0i)
    (test FX4 CFL02 -536870912.0+0.0i)

    (test FX1 CFL03 1.0-0.0i)
    (test FX2 CFL03 -1.0-0.0i)
    (test FX3 CFL03 536870911.0-0.0i)
    (test FX4 CFL03 -536870912.0-0.0i)

    (test FX1 CFL04 1.0-0.0i)
    (test FX2 CFL04 -1.0-0.0i)
    (test FX3 CFL04 536870911.0-0.0i)
    (test FX4 CFL04 -536870912.0-0.0i)

    (test FX1 CFL05 -0.19999999999999996-0.0i)
    (test FX2 CFL05 -2.2-0.0i)
    (test FX3 CFL05 536870909.8-0.0i)
    (test FX4 CFL05 -536870913.2-0.0i)

    (test FX1 CFL06 -0.19999999999999996+0.0i)
    (test FX2 CFL06 -2.2+0.0i)
    (test FX3 CFL06 536870909.8+0.0i)
    (test FX4 CFL06 -536870913.2+0.0i)

    (test FX1 CFL07 1.0-1.2i)
    (test FX2 CFL07 -1.0-1.2i)
    (test FX3 CFL07 536870911.0-1.2i)
    (test FX4 CFL07 -536870912.0-1.2i)

    (test FX1 CFL08 1.0-1.2i)
    (test FX2 CFL08 -1.0-1.2i)
    (test FX3 CFL08 536870911.0-1.2i)
    (test FX4 CFL08 -536870912.0-1.2i)

    (test FX1 CFL09 -0.19999999999999996-inf.0i)
    (test FX1 CFL10 -0.19999999999999996+inf.0i)
    (test FX1 CFL11 +inf.0-1.2i)
    (test FX1 CFL12 -inf.0-1.2i)

    (test FX1 CFL13 -0.19999999999999996+nan.0i)
    (test FX1 CFL14 -0.19999999999999996+nan.0i)
    (test FX1 CFL15 +nan.0-1.2i)
    (test FX1 CFL16 +nan.0-1.2i)
    #f)

  (let-syntax ((test (make-test + $add-fixnum-compnum)))
    (test 1	10+20i		11+20i)

    (test 1 1+20.0i 2+20.0i)
    (test 1 10.0+2i 11.0+2i)

    (test 1	1/2+20i		3/2+20i)
    (test 1	10+2/3i		11+2/3i)

    (test 1	(make-rectangular BN1 20)	(make-rectangular (+ 1 BN1) 20))
    (test 1	(make-rectangular 10 BN1)	(make-rectangular 11 BN1))
    #f)

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-test + $add-bignum-fixnum)))
    (test BN1 FX1 536870913)
    (test BN2 FX1 536871012)
    (test BN3 FX1 -536870912)
    (test BN4 FX1 -536871011)

    (test BN1 FX2 536870911)
    (test BN2 FX2 536871010)
    (test BN3 FX2 -536870914)
    (test BN4 FX2 -536871013)

    (test BN1 FX3 1073741823)
    (test BN2 FX3 1073741922)
    (test BN3 FX3 -2)
    (test BN4 FX3 -101)

    (test BN1 FX4 0)
    (test BN2 FX4 99)
    (test BN3 FX4 -1073741825)
    (test BN4 FX4 -1073741924)
    #f)

  (let-syntax ((test (make-test + $add-bignum-bignum)))
    (test BN1 BN1 1073741824)
    (test BN2 BN1 1073741923)
    (test BN3 BN1 -1)
    (test BN4 BN1 -100)
    (test BN1 BN2 1073741923)
    (test BN2 BN2 1073742022)
    (test BN3 BN2 98)
    (test BN4 BN2 -1)
    (test BN1 BN3 -1)
    (test BN2 BN3 98)
    (test BN3 BN3 -1073741826)
    (test BN4 BN3 -1073741925)
    (test BN1 BN4 -100)
    (test BN2 BN4 -1)
    (test BN3 BN4 -1073741925)
    (test BN4 BN4 -1073742024)
    #f)

  (let-syntax ((test (make-test + $add-bignum-ratnum)))
    (test BN1 RN01 66035122177/123)
    (test BN2 RN01 66035134354/123)
    (test BN3 RN01 -66035122298/123)
    (test BN4 RN01 -66035134475/123)

    (test BN1 RN02 66035122175/123)
    (test BN2 RN02 66035134352/123)
    (test BN3 RN02 -66035122300/123)
    (test BN4 RN02 -66035134477/123)

    (test BN1 RN03 66035122175/123)
    (test BN2 RN03 66035134352/123)
    (test BN3 RN03 -66035122300/123)
    (test BN4 RN03 -66035134477/123)

    (test BN1 RN04 65498251264/123)
    (test BN2 RN04 65498263441/123)
    (test BN3 RN04 -66571993211/123)
    (test BN4 RN04 -66572005388/123)
    #f)

  (let-syntax ((test (make-flonum-test + $add-bignum-flonum)))
    (test BN1 FL1 536870912.0)
    (test BN2 FL1 536871011.0)
    (test BN3 FL1 -536870913.0)
    (test BN4 FL1 -536871012.0)

    (test BN1 FL2 536870912.0)
    (test BN2 FL2 536871011.0)
    (test BN3 FL2 -536870913.0)
    (test BN4 FL2 -536871012.0)

    (test BN1 FL3 536870914.123)
    (test BN2 FL3 536871013.123)
    (test BN3 FL3 -536870910.877)
    (test BN4 FL3 -536871009.877)

    (test BN1 FL4 536870909.877)
    (test BN2 FL4 536871008.877)
    (test BN3 FL4 -536870915.123)
    (test BN4 FL4 -536871014.123)
    #f)

  (let-syntax ((test (make-cflonum-test + $add-bignum-cflonum)))
    (test BN1 CFL01 536870912.0+0.0i)
    (test BN2 CFL01 536871011.0+0.0i)
    (test BN3 CFL01 -536870913.0+0.0i)
    (test BN4 CFL01 -536871012.0+0.0i)

    (test BN1 CFL02 536870912.0+0.0i)
    (test BN2 CFL02 536871011.0+0.0i)
    (test BN3 CFL02 -536870913.0+0.0i)
    (test BN4 CFL02 -536871012.0+0.0i)

    (test BN1 CFL03 536870912.0-0.0i)
    (test BN2 CFL03 536871011.0-0.0i)
    (test BN3 CFL03 -536870913.0-0.0i)
    (test BN4 CFL03 -536871012.0-0.0i)

    (test BN1 CFL04 536870912.0-0.0i)
    (test BN2 CFL04 536871011.0-0.0i)
    (test BN3 CFL04 -536870913.0-0.0i)
    (test BN4 CFL04 -536871012.0-0.0i)

    (test BN1 CFL05 536870910.8-0.0i)
    (test BN2 CFL05 536871009.8-0.0i)
    (test BN3 CFL05 -536870914.2-0.0i)
    (test BN4 CFL05 -536871013.2-0.0i)

    (test BN1 CFL06 536870910.8+0.0i)
    (test BN2 CFL06 536871009.8+0.0i)
    (test BN3 CFL06 -536870914.2+0.0i)
    (test BN4 CFL06 -536871013.2+0.0i)

    (test BN1 CFL07 536870912.0-1.2i)
    (test BN2 CFL07 536871011.0-1.2i)
    (test BN3 CFL07 -536870913.0-1.2i)
    (test BN4 CFL07 -536871012.0-1.2i)

    (test BN1 CFL08 536870912.0-1.2i)
    (test BN2 CFL08 536871011.0-1.2i)
    (test BN3 CFL08 -536870913.0-1.2i)
    (test BN4 CFL08 -536871012.0-1.2i)

    (test BN1 CFL09 536870910.8-inf.0i)
    (test BN1 CFL10 536870910.8+inf.0i)
    (test BN1 CFL11 +inf.0-1.2i)
    (test BN1 CFL12 -inf.0-1.2i)

    (test BN1 CFL13 536870910.8+nan.0i)
    (test BN1 CFL14 536870910.8+nan.0i)
    (test BN1 CFL15 +nan.0-1.2i)
    (test BN1 CFL16 +nan.0-1.2i)
    #f)

  (let-syntax ((test (make-test + $add-bignum-compnum)))
    (test BN1	10+20i		(make-rectangular (+ BN1 10) 20))
    (test BN1 1+20.0i 536870913+20.0i)
    (test BN1 10.0+2i 536870922+2.0i)
    (test BN1	1/2+20i		(make-rectangular (+ BN1 1/2) 20))
    (test BN1	10+2/3i		(make-rectangular (+ BN1 10) 2/3))
    (test BN1	(make-rectangular BN2 20)	(make-rectangular (+ BN1 BN2) 20))
    (test BN1	(make-rectangular 10 BN2)	(make-rectangular (+ BN1 10) BN2))
    #f)

  #t)


(parametrise ((check-test-name	'ratnums))

  (let-syntax ((test (make-test + $add-ratnum-fixnum)))
    (test 1/2 0				1/2)
    (test 1/2 10			21/2)
    (test 1/2 (greatest-fixnum)		1073741823/2)
    (test 1/2 (least-fixnum)		-1073741823/2)
    #f)

  (let-syntax ((test (make-test + $add-ratnum-bignum)))
    (test 1/2 BN1			1073741825/2)
    (test 1/2 BN2			1073742023/2)
    (test 1/2 BN3			-1073741825/2)
    (test 1/2 BN4			-1073742023/2)
    (test -1/2 BN1			1073741823/2)
    (test -1/2 BN2			1073742021/2)
    (test -1/2 BN3			-1073741827/2)
    (test -1/2 BN4			-1073742025/2)
    #f)

  (let-syntax ((test (make-test + $add-ratnum-ratnum)))
    (test 1/2	3/4			5/4)
    (test -1/2	3/4			1/4)
    (test -1/2	-3/4			-5/4)
    #f)

  (let-syntax ((test (make-test + $add-ratnum-flonum)))
    (test 1/2 3.4			3.9)
    (test -1/2 3.4			2.9)

    (test RN01 FL1 0.008130081300813009)
    (test RN02 FL1 -0.008130081300813009)
    (test RN03 FL1 -0.008130081300813009)
    (test RN04 FL1 -4364804.1626016265)
    (test RN01 FL2 0.008130081300813009)
    (test RN02 FL2 -0.008130081300813009)
    (test RN03 FL2 -0.008130081300813009)
    (test RN04 FL2 -4364804.1626016265)
    (test RN01 FL3 2.1311300813008134)
    (test RN02 FL3 2.114869918699187)
    (test RN03 FL3 2.114869918699187)
    (test RN04 FL3 -4364802.039601627)
    (test RN01 FL4 -2.114869918699187)
    (test RN02 FL4 -2.1311300813008134)
    (test RN03 FL4 -2.1311300813008134)
    (test RN04 FL4 -4364806.285601626)

    #f)

  (let-syntax ((test (make-cflonum-test + $add-ratnum-cflonum)))
    (test RN01 CFL01 0.008130081300813009+0.0i)
    (test RN02 CFL01 -0.008130081300813009+0.0i)
    (test RN03 CFL01 -0.008130081300813009+0.0i)
    (test RN04 CFL01 -4364804.1626016265+0.0i)
    (test RN01 CFL02 0.008130081300813009+0.0i)
    (test RN02 CFL02 -0.008130081300813009+0.0i)
    (test RN03 CFL02 -0.008130081300813009+0.0i)
    (test RN04 CFL02 -4364804.1626016265+0.0i)
    (test RN01 CFL03 0.008130081300813009-0.0i)
    (test RN02 CFL03 -0.008130081300813009-0.0i)
    (test RN03 CFL03 -0.008130081300813009-0.0i)
    (test RN04 CFL03 -4364804.1626016265-0.0i)
    (test RN01 CFL04 0.008130081300813009-0.0i)
    (test RN02 CFL04 -0.008130081300813009-0.0i)
    (test RN03 CFL04 -0.008130081300813009-0.0i)
    (test RN04 CFL04 -4364804.1626016265-0.0i)
    (test RN01 CFL05 -1.191869918699187-0.0i)
    (test RN02 CFL05 -1.208130081300813-0.0i)
    (test RN03 CFL05 -1.208130081300813-0.0i)
    (test RN04 CFL05 -4364805.362601627-0.0i)
    (test RN01 CFL06 -1.191869918699187+0.0i)
    (test RN02 CFL06 -1.208130081300813+0.0i)
    (test RN03 CFL06 -1.208130081300813+0.0i)
    (test RN04 CFL06 -4364805.362601627+0.0i)
    (test RN01 CFL07 0.008130081300813009-1.2i)
    (test RN02 CFL07 -0.008130081300813009-1.2i)
    (test RN03 CFL07 -0.008130081300813009-1.2i)
    (test RN04 CFL07 -4364804.1626016265-1.2i)
    (test RN01 CFL08 0.008130081300813009-1.2i)
    (test RN02 CFL08 -0.008130081300813009-1.2i)
    (test RN03 CFL08 -0.008130081300813009-1.2i)
    (test RN04 CFL08 -4364804.1626016265-1.2i)
    (test RN01 CFL09 -1.191869918699187-inf.0i)
    (test RN01 CFL10 -1.191869918699187+inf.0i)
    (test RN01 CFL11 +inf.0-1.2i)
    (test RN01 CFL12 -inf.0-1.2i)
    (test RN01 CFL13 -1.191869918699187+nan.0i)
    (test RN01 CFL14 -1.191869918699187+nan.0i)
    (test RN01 CFL15 +nan.0-1.2i)
    (test RN01 CFL16 +nan.0-1.2i)

    #f)

  (let-syntax ((test (make-inexact-test + $add-ratnum-compnum)))
    (test RN01 10+20i 1231/123+20i)
    (test RN01 1+20.0i 124/123+20.0i)
    (test RN01 10.0+2i 10.008130081300813+2i)
    (test RN01 1/2+20i 125/246+20i)
    (test RN01 10+2/3i 1231/123+2/3i)
    (test RN01 (make-rectangular RN02 20) 0+20i)
    (test RN01 (make-rectangular 10 RN02) 1231/123-1/123i)
    #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (let-syntax ((test (make-test + $add-flonum-fixnum)))
    (test FL1 FX1 1.0)
    (test FL2 FX1 1.0)
    (test FL3 FX1 3.123)
    (test FL4 FX1 -1.1230000000000002)
    (test FL1 FX2 -1.0)
    (test FL2 FX2 -1.0)
    (test FL3 FX2 1.1230000000000002)
    (test FL4 FX2 -3.123)
    (test FL1 FX3 536870911.0)
    (test FL2 FX3 536870911.0)
    (test FL3 FX3 536870913.123)
    (test FL4 FX3 536870908.877)
    (test FL1 FX4 -536870912.0)
    (test FL2 FX4 -536870912.0)
    (test FL3 FX4 -536870909.877)
    (test FL4 FX4 -536870914.123)
    #f)

  (let-syntax ((test (make-test + $add-flonum-bignum)))
    (test FL1 BN1 536870912.0)
    (test FL2 BN1 536870912.0)
    (test FL3 BN1 536870914.123)
    (test FL4 BN1 536870909.877)
    (test FL1 BN2 536871011.0)
    (test FL2 BN2 536871011.0)
    (test FL3 BN2 536871013.123)
    (test FL4 BN2 536871008.877)
    (test FL1 BN3 -536870913.0)
    (test FL2 BN3 -536870913.0)
    (test FL3 BN3 -536870910.877)
    (test FL4 BN3 -536870915.123)
    (test FL1 BN4 -536871012.0)
    (test FL2 BN4 -536871012.0)
    (test FL3 BN4 -536871009.877)
    (test FL4 BN4 -536871014.123)
    #f)

  (let-syntax ((test (make-test + $add-flonum-ratnum)))
    (test FL1 RN01 0.008130081300813009)
    (test FL2 RN01 0.008130081300813009)
    (test FL3 RN01 2.1311300813008134)
    (test FL4 RN01 -2.114869918699187)
    (test FL1 RN02 -0.008130081300813009)
    (test FL2 RN02 -0.008130081300813009)
    (test FL3 RN02 2.114869918699187)
    (test FL4 RN02 -2.1311300813008134)
    (test FL1 RN03 -0.008130081300813009)
    (test FL2 RN03 -0.008130081300813009)
    (test FL3 RN03 2.114869918699187)
    (test FL4 RN03 -2.1311300813008134)
    (test FL1 RN04 -4364804.1626016265)
    (test FL2 RN04 -4364804.1626016265)
    (test FL3 RN04 -4364802.039601627)
    (test FL4 RN04 -4364806.285601626)
    #f)

  (let-syntax ((test (make-flonum-test + $add-flonum-flonum)))
    (test FL1 FL1 0.0)
    (test FL2 FL1 0.0)
    (test FL3 FL1 2.123)
    (test FL4 FL1 -2.123)
    (test FL1 FL2 0.0)
    (test FL2 FL2 -0.0)
    (test FL3 FL2 2.123)
    (test FL4 FL2 -2.123)
    (test FL1 FL3 2.123)
    (test FL2 FL3 2.123)
    (test FL3 FL3 4.246)
    (test FL4 FL3 0.0)
    (test FL1 FL4 -2.123)
    (test FL2 FL4 -2.123)
    (test FL3 FL4 0.0)
    (test FL4 FL4 -4.246)
    #f)

  (let-syntax ((test (make-cflonum-test + $add-flonum-cflonum)))
    (test FL1 CFL01 0.0+0.0i)
    (test FL2 CFL01 0.0+0.0i)
    (test FL3 CFL01 2.123+0.0i)
    (test FL4 CFL01 -2.123+0.0i)
    (test FL1 CFL02 0.0+0.0i)
    (test FL2 CFL02 -0.0+0.0i)
    (test FL3 CFL02 2.123+0.0i)
    (test FL4 CFL02 -2.123+0.0i)
    (test FL1 CFL03 0.0-0.0i)
    (test FL2 CFL03 0.0-0.0i)
    (test FL3 CFL03 2.123-0.0i)
    (test FL4 CFL03 -2.123-0.0i)
    (test FL1 CFL04 0.0-0.0i)
    (test FL2 CFL04 -0.0-0.0i)
    (test FL3 CFL04 2.123-0.0i)
    (test FL4 CFL04 -2.123-0.0i)
    (test FL1 CFL05 -1.2-0.0i)
    (test FL2 CFL05 -1.2-0.0i)
    (test FL3 CFL05 0.9230000000000003-0.0i)
    (test FL4 CFL05 -3.3230000000000004-0.0i)
    (test FL1 CFL06 -1.2+0.0i)
    (test FL2 CFL06 -1.2+0.0i)
    (test FL3 CFL06 0.9230000000000003+0.0i)
    (test FL4 CFL06 -3.3230000000000004+0.0i)
    (test FL1 CFL07 0.0-1.2i)
    (test FL2 CFL07 0.0-1.2i)
    (test FL3 CFL07 2.123-1.2i)
    (test FL4 CFL07 -2.123-1.2i)
    (test FL1 CFL08 0.0-1.2i)
    (test FL2 CFL08 -0.0-1.2i)
    (test FL3 CFL08 2.123-1.2i)
    (test FL4 CFL08 -2.123-1.2i)
    (test FL1 CFL09 -1.2-inf.0i)
    (test FL1 CFL10 -1.2+inf.0i)
    (test FL1 CFL11 +inf.0-1.2i)
    (test FL1 CFL12 -inf.0-1.2i)
    (test FL1 CFL13 -1.2+nan.0i)
    (test FL1 CFL14 -1.2+nan.0i)
    (test FL1 CFL15 +nan.0-1.2i)
    (test FL1 CFL16 +nan.0-1.2i)
    #f)

  (let-syntax ((test (make-inexact-test + $add-flonum-compnum)))
    (test FL1 10+20i 10.0+20.0i)
    (test FL1 1+20.0i 1.0+20.0i)
    (test FL1 10.0+2i 10.0+2i)
    (test FL1 1/2+20i 0.5+20.0i)
    (test FL1 10+2/3i 10.0+2/3i)
    (test FL1 (make-rectangular BN2 20) 536871011.0+20.0i)
    (test FL1 (make-rectangular 10 BN2) 10.0+536871011.0i)
    #f)

  #t)


(parametrise ((check-test-name	'cflonums))

  (let-syntax ((test (make-inexact-test + $add-cflonum-fixnum)))
    (test CFL01 FX1 1.0+0.0i)
    (test CFL02 FX1 1.0+0.0i)
    (test CFL03 FX1 1.0-0.0i)
    (test CFL04 FX1 1.0-0.0i)
    (test CFL01 FX2 -1.0+0.0i)
    (test CFL02 FX2 -1.0+0.0i)
    (test CFL03 FX2 -1.0-0.0i)
    (test CFL04 FX2 -1.0-0.0i)
    (test CFL01 FX3 536870911.0+0.0i)
    (test CFL02 FX3 536870911.0+0.0i)
    (test CFL03 FX3 536870911.0-0.0i)
    (test CFL04 FX3 536870911.0-0.0i)
    (test CFL01 FX4 -536870912.0+0.0i)
    (test CFL02 FX4 -536870912.0+0.0i)
    (test CFL03 FX4 -536870912.0-0.0i)
    (test CFL04 FX4 -536870912.0-0.0i)
    #f)

  (let-syntax ((test (make-inexact-test + $add-cflonum-bignum)))
    (test CFL01 BN1 536870912.0+0.0i)
    (test CFL02 BN1 536870912.0+0.0i)
    (test CFL03 BN1 536870912.0-0.0i)
    (test CFL04 BN1 536870912.0-0.0i)
    (test CFL01 BN2 536871011.0+0.0i)
    (test CFL02 BN2 536871011.0+0.0i)
    (test CFL03 BN2 536871011.0-0.0i)
    (test CFL04 BN2 536871011.0-0.0i)
    (test CFL01 BN3 -536870913.0+0.0i)
    (test CFL02 BN3 -536870913.0+0.0i)
    (test CFL03 BN3 -536870913.0-0.0i)
    (test CFL04 BN3 -536870913.0-0.0i)
    (test CFL01 BN4 -536871012.0+0.0i)
    (test CFL02 BN4 -536871012.0+0.0i)
    (test CFL03 BN4 -536871012.0-0.0i)
    (test CFL04 BN4 -536871012.0-0.0i)
    #f)

  (let-syntax ((test (make-inexact-test + $add-cflonum-ratnum)))
    (test CFL01 RN01 0.008130081300813009+0.0i)
    (test CFL02 RN01 0.008130081300813009+0.0i)
    (test CFL03 RN01 0.008130081300813009-0.0i)
    (test CFL04 RN01 0.008130081300813009-0.0i)
    (test CFL01 RN02 -0.008130081300813009+0.0i)
    (test CFL02 RN02 -0.008130081300813009+0.0i)
    (test CFL03 RN02 -0.008130081300813009-0.0i)
    (test CFL04 RN02 -0.008130081300813009-0.0i)
    (test CFL01 RN03 -0.008130081300813009+0.0i)
    (test CFL02 RN03 -0.008130081300813009+0.0i)
    (test CFL03 RN03 -0.008130081300813009-0.0i)
    (test CFL04 RN03 -0.008130081300813009-0.0i)
    (test CFL01 RN04 -4364804.1626016265+0.0i)
    (test CFL02 RN04 -4364804.1626016265+0.0i)
    (test CFL03 RN04 -4364804.1626016265-0.0i)
    (test CFL04 RN04 -4364804.1626016265-0.0i)
    #f)

  (let-syntax ((test (make-inexact-test + $add-cflonum-flonum)))
    (test CFL01 FL1 0.0+0.0i)
    (test CFL02 FL1 0.0+0.0i)
    (test CFL03 FL1 0.0-0.0i)
    (test CFL04 FL1 0.0-0.0i)
    (test CFL01 FL2 0.0+0.0i)
    (test CFL02 FL2 -0.0+0.0i)
    (test CFL03 FL2 0.0-0.0i)
    (test CFL04 FL2 -0.0-0.0i)
    (test CFL01 FL3 2.123+0.0i)
    (test CFL02 FL3 2.123+0.0i)
    (test CFL03 FL3 2.123-0.0i)
    (test CFL04 FL3 2.123-0.0i)
    (test CFL01 FL4 -2.123+0.0i)
    (test CFL02 FL4 -2.123+0.0i)
    (test CFL03 FL4 -2.123-0.0i)
    (test CFL04 FL4 -2.123-0.0i)
    #f)

  (let-syntax ((test (make-cflonum-test + $add-cflonum-cflonum)))
    (test CFL01 CFL01 0.0+0.0i)
    (test CFL02 CFL01 0.0+0.0i)
    (test CFL03 CFL01 0.0+0.0i)
    (test CFL04 CFL01 0.0+0.0i)
    (test CFL01 CFL02 0.0+0.0i)
    (test CFL02 CFL02 -0.0+0.0i)
    (test CFL03 CFL02 0.0+0.0i)
    (test CFL04 CFL02 -0.0+0.0i)
    (test CFL01 CFL03 0.0+0.0i)
    (test CFL02 CFL03 0.0+0.0i)
    (test CFL03 CFL03 0.0-0.0i)
    (test CFL04 CFL03 0.0-0.0i)
    (test CFL01 CFL04 0.0+0.0i)
    (test CFL02 CFL04 -0.0+0.0i)
    (test CFL03 CFL04 0.0-0.0i)
    (test CFL04 CFL04 -0.0-0.0i)
    (test CFL01 CFL05 -1.2+0.0i)
    (test CFL02 CFL05 -1.2+0.0i)
    (test CFL03 CFL05 -1.2-0.0i)
    (test CFL04 CFL05 -1.2-0.0i)
    (test CFL01 CFL06 -1.2+0.0i)
    (test CFL02 CFL06 -1.2+0.0i)
    (test CFL03 CFL06 -1.2+0.0i)
    (test CFL04 CFL06 -1.2+0.0i)
    (test CFL01 CFL07 0.0-1.2i)
    (test CFL02 CFL07 0.0-1.2i)
    (test CFL03 CFL07 0.0-1.2i)
    (test CFL04 CFL07 0.0-1.2i)
    (test CFL01 CFL08 0.0-1.2i)
    (test CFL02 CFL08 -0.0-1.2i)
    (test CFL03 CFL08 0.0-1.2i)
    (test CFL04 CFL08 -0.0-1.2i)
    (test CFL01 CFL09 -1.2-inf.0i)
    (test CFL01 CFL10 -1.2+inf.0i)
    (test CFL01 CFL11 +inf.0-1.2i)
    (test CFL01 CFL12 -inf.0-1.2i)
    (test CFL01 CFL13 -1.2+nan.0i)
    (test CFL01 CFL14 -1.2+nan.0i)
    (test CFL01 CFL15 +nan.0-1.2i)
    (test CFL01 CFL16 +nan.0-1.2i)
    #f)

  (let-syntax ((test (make-inexact-test + $add-cflonum-compnum)))
    (test CFL01 10+20i 10.0+20.0i)
    (test CFL01 1+20.0i 1.0+20.0i)
    (test CFL01 10.0+2i 10.0+2.0i)
    (test CFL01 1/2+20i 0.5+20.0i)
    (test CFL01 10+2/3i 10.0+0.6666666666666666i)
    (test CFL01 (make-rectangular BN2 20) 536871011.0+20.0i)
    (test CFL01 (make-rectangular 10 BN2) 10.0+536871011.0i)
    #f)

  #t)


(parametrise ((check-test-name	'compnums))

  (let-syntax ((test (make-test + $add-compnum-fixnum)))
    (test 10+20i 1 11+20i)
    (test 1.0+20.0i 1 2.0+20.0i)
    (test 10.0+2.0i 1 11.0+2.0i)
    (test 1/2+20i 1 3/2+20i)
    (test 10+2/3i 1 11+2/3i)
    (test (make-rectangular BN1 20) 1 536870913+20i)
    (test (make-rectangular 10 BN1) 1 11+536870912i)
    #f)

  (let-syntax ((test (make-test + $add-compnum-bignum)))
    (test 10+20i BN1 536870922+20i)
    (test 1.0+20.0i BN1 536870913.0+20.0i)
    (test 10.0+2.0i BN1 536870922.0+2.0i)
    (test 1/2+20i BN1 1073741825/2+20i)
    (test 10+2/3i BN1 536870922+2/3i)
    (test (make-rectangular BN2 20) BN1 1073741923+20i)
    (test (make-rectangular 10 BN2) BN1 536870922+536871011i)
    #f)

  (let-syntax ((test (make-inexact-test + $add-compnum-ratnum)))
    (test 10+20i RN01 1231/123+20i)
    (test 1+20.0i RN01 124/123+20.0i)
    (test 10.0+2.0i RN01 10.008130081300813+2.0i)
    (test 1/2+20i RN01 125/246+20i)
    (test 10+2/3i RN01 1231/123+2/3i)
    (test (make-rectangular RN02 20) RN01 0+20i)
    (test (make-rectangular 10 RN02) RN01 1231/123-1/123i)
    #f)

  (let-syntax ((test (make-inexact-test + $add-compnum-flonum)))
    (test 10+20i FL1 10.0+20.0i)
    (test 1+20.0i FL1 1.0+20.0i)
    (test 10.0+2i FL1 10.0+2.0i)
    (test 1.0+20.0i FL1 1.0+20.0i)
    (test 10.0+2.0i FL1 10.0+2.0i)
    (test 1/2+20i FL1 0.5+20.0i)
    (test 10+2/3i FL1 10.0+2/3i)
    (test (make-rectangular BN2 20) FL1 536871011.0+20.0i)
    (test (make-rectangular 10 BN2) FL1 10.0+536871011.0i)
    #f)

  (let-syntax ((test (make-inexact-test + $add-compnum-cflonum)))
    (test 10+20i CFL01 10.0+20.0i)
    (test 1.0+20.0i CFL01 1.0+20.0i)
    (test 10.0+2.0i CFL01 10.0+2.0i)
    (test 1/2+20i CFL01 0.5+20.0i)
    (test 10+2/3i CFL01 10.0+0.6666666666666666i)
    (test (make-rectangular BN2 20) CFL01 536871011.0+20.0i)
    (test (make-rectangular 10 BN2) CFL01 10.0+536871011.0i)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
