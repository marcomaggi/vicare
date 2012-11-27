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

(define (flonum=? x y)
  (cond ((flzero?/positive x)
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

  #t)


#;(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (make-test / $add-bignum))

  (test BN1 ADD-BN1)
  (test BN2 ADD-BN2)
  (test BN3 ADD-BN3)
  (test BN4 ADD-BN4)

  #t)


#;(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (make-test / $add-ratnum))

  (test RN01	ADD-RN01)
  (test RN02	ADD-RN02)
  (test RN03	ADD-RN03)
  (test RN04	ADD-RN04)
  (test RN05	ADD-RN05)
  (test RN06	ADD-RN06)
  (test RN07	ADD-RN07)
  ;;(test RN08	ADD-RN08) ;not a ratnum
  (test RN09	ADD-RN09)

  (test RN10	ADD-RN10)
  (test RN11	ADD-RN11)
  (test RN12	ADD-RN12)
  (test RN13	ADD-RN13)
  (test RN14	ADD-RN14)
  (test RN15	ADD-RN15)
  (test RN16	ADD-RN16)
  (test RN17	ADD-RN17)
  (test RN18	ADD-RN18)
  (test RN19	ADD-RN19)

  (test RN20	ADD-RN20)
  ;;not ratnums
  ;;
  ;; (test RN21	ADD-RN21)
  ;; (test RN22	ADD-RN22)
  ;; (test RN23	ADD-RN23)
  ;; (test RN24	ADD-RN24)
  ;; (test RN25	ADD-RN25)
  ;; (test RN26	ADD-RN26)
  ;; (test RN27	ADD-RN27)
  ;; (test RN28	ADD-RN28)
  (test RN29	ADD-RN29)

  (test RN30	ADD-RN30)
  (test RN31	ADD-RN31)
  (test RN32	ADD-RN32)
  ;;(test RN33	ADD-RN33) ;not a ratnum
  (test RN34	ADD-RN34)
  (test RN35	ADD-RN35)
  (test RN36	ADD-RN36)

  #t)


#;(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-flonum-test / $add-flonum))

  (test FL1 ADD-FL1)
  (test FL2 ADD-FL2)
  (test FL3 ADD-FL3)
  (test FL4 ADD-FL4)
  (test FL5 ADD-FL5)
  (test FL6 ADD-FL6)
  (test FL7 ADD-FL7)

  #t)


#;(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test / $add-cflonum))

  (test CFL01 ADD-CFL01)
  (test CFL02 ADD-CFL02)
  (test CFL03 ADD-CFL03)
  (test CFL04 ADD-CFL04)
  (test CFL05 ADD-CFL05)
  (test CFL06 ADD-CFL06)
  (test CFL07 ADD-CFL07)
  (test CFL08 ADD-CFL08)
  (test CFL09 ADD-CFL09)

  (test CFL11 ADD-CFL11)
  (test CFL12 ADD-CFL12)
  (test CFL13 ADD-CFL13)
  (test CFL14 ADD-CFL14)
  (test CFL15 ADD-CFL15)
  (test CFL16 ADD-CFL16)

  #t)


#;(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (make-compnum-test / $add-compnum))


  (test CN001 ADD-CN001)
  (test CN002 ADD-CN002)
  (test CN003 ADD-CN003)
  (test CN004 ADD-CN004)

  (test CN005 ADD-CN005)
  (test CN006 ADD-CN006)
  (test CN007 ADD-CN007)
  (test CN008 ADD-CN008)

  (test CN009 ADD-CN009)
  (test CN010 ADD-CN010)
  (test CN011 ADD-CN011)
  (test CN012 ADD-CN012)

  (test CN013 ADD-CN013)
  (test CN014 ADD-CN014)
  (test CN015 ADD-CN015)
  (test CN016 ADD-CN016)

;;; --------------------------------------------------------------------

  (test CN017 ADD-CN017)
  (test CN018 ADD-CN018)
  (test CN019 ADD-CN019)
  (test CN020 ADD-CN020)

  (test CN021 ADD-CN021)
  (test CN022 ADD-CN022)
  (test CN023 ADD-CN023)
  (test CN024 ADD-CN024)

  (test CN025 ADD-CN025)
  (test CN026 ADD-CN026)
  (test CN027 ADD-CN027)
  (test CN028 ADD-CN028)

  (test CN029 ADD-CN029)
  (test CN030 ADD-CN030)
  (test CN031 ADD-CN031)
  (test CN032 ADD-CN032)

;;; --------------------------------------------------------------------

  (test CN033 ADD-CN033)
  (test CN034 ADD-CN034)
  (test CN035 ADD-CN035)
  (test CN036 ADD-CN036)

  (test CN037 ADD-CN037)
  (test CN038 ADD-CN038)
  (test CN039 ADD-CN039)
  (test CN040 ADD-CN040)

  (test CN041 ADD-CN041)
  (test CN042 ADD-CN042)
  (test CN043 ADD-CN043)
  (test CN044 ADD-CN044)

  (test CN045 ADD-CN045)
  (test CN046 ADD-CN046)
  (test CN047 ADD-CN047)
  (test CN048 ADD-CN048)

;;; --------------------------------------------------------------------

  (test CN049 ADD-CN049)
  (test CN050 ADD-CN050)
  (test CN051 ADD-CN051)
  (test CN052 ADD-CN052)

  (test CN053 ADD-CN053)
  (test CN054 ADD-CN054)
  (test CN055 ADD-CN055)
  (test CN056 ADD-CN056)

  (test CN057 ADD-CN057)
  (test CN058 ADD-CN058)
  (test CN059 ADD-CN059)
  (test CN060 ADD-CN060)

  (test CN061 ADD-CN061)
  (test CN062 ADD-CN062)
  (test CN063 ADD-CN063)
  (test CN064 ADD-CN064)

;;; --------------------------------------------------------------------

  (test CN065 ADD-CN065)
  (test CN066 ADD-CN066)
  (test CN067 ADD-CN067)
  (test CN068 ADD-CN068)

  (test CN069 ADD-CN069)
  (test CN070 ADD-CN070)
  (test CN071 ADD-CN071)
  (test CN072 ADD-CN072)

  (test CN073 ADD-CN073)
  (test CN074 ADD-CN074)
  (test CN075 ADD-CN075)
  (test CN076 ADD-CN076)

  (test CN077 ADD-CN077)
  (test CN078 ADD-CN078)
  (test CN079 ADD-CN079)
  (test CN080 ADD-CN080)

;;; --------------------------------------------------------------------

  (test CN081 ADD-CN081)
  (test CN082 ADD-CN082)
  (test CN083 ADD-CN083)
  (test CN084 ADD-CN084)

  (test CN085 ADD-CN085)
  (test CN086 ADD-CN086)
  (test CN087 ADD-CN087)
  (test CN088 ADD-CN088)

  (test CN089 ADD-CN089)
  (test CN090 ADD-CN090)
  (test CN091 ADD-CN091)
  (test CN092 ADD-CN092)

  (test CN093 ADD-CN093)
  (test CN094 ADD-CN094)
  (test CN095 ADD-CN095)
  (test CN096 ADD-CN096)

;;; --------------------------------------------------------------------

  (test CN097 ADD-CN097)
  (test CN098 ADD-CN098)
  (test CN099 ADD-CN099)
  (test CN100 ADD-CN100)

  (test CN101 ADD-CN101)
  (test CN102 ADD-CN102)
  (test CN103 ADD-CN103)
  (test CN104 ADD-CN104)

  (test CN105 ADD-CN105)
  (test CN106 ADD-CN106)
  (test CN107 ADD-CN107)
  (test CN108 ADD-CN108)

  (test CN109 ADD-CN109)
  (test CN110 ADD-CN110)
  (test CN111 ADD-CN111)
  (test CN112 ADD-CN112)

  #t)


;;;; done

(check-report)

;;; end of file
