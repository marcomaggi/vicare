;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: neg
;;;Date: Mon Nov 26, 2012
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
(check-display "*** testing Vicare numerics functions: neg, unary minus\n")


;;;; helpers

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

(define GREATEST-FX		+536870911)
(define LEAST-FX		-536870912)

(define NEG-GREATEST-FX		-536870911)
(define NEG-LEAST-FX		+536870912)

;;; --------------------------------------------------------------------

(define FX1		+1)
(define FX2		-1)
(define FX3		GREATEST-FX)
(define FX4		LEAST-FX)

(define NEG-FX1		-1)
(define NEG-FX2		+1)
(define NEG-FX3		NEG-GREATEST-FX)
(define NEG-FX4		NEG-LEAST-FX)

;;; --------------------------------------------------------------------

(define BN1		+536870912) ;GREATEST-FX + 1
(define BN2		+536871011) ;GREATEST-FX + 100
(define BN3		-536870913) ;LEAST-FX - 1
(define BN4		-536871012) ;LEAST-FX - 100

(define NEG-BN1		-536870912)
(define NEG-BN2		-536871011)
(define NEG-BN3		+536870913)
(define NEG-BN4		+536871012)

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

(define NEG-RN01	-1/123			#;(/ NEG-FX1 123))
(define NEG-RN02	1/123			#;(/ NEG-FX2 123))
(define NEG-RN03	1/123			#;(/ NEG-FX2 123))
(define NEG-RN04	536870912/123		#;(/ NEG-FX4 123))

(define NEG-RN05	-1/536870912		#;(/ NEG-FX1 BN1))
(define NEG-RN06	1/536870912		#;(/ NEG-FX2 BN1))
(define NEG-RN07	-536870911/536870912	#;(/ NEG-FX3 BN1))
;;(define NEG-RN08	1			#;(/ NEG-FX4 BN1)) ;not a ratnum

(define NEG-RN09	-1/536871011		#;(/ NEG-FX1 BN2))
(define NEG-RN10	1/536871011		#;(/ NEG-FX2 BN2))
(define NEG-RN11	-536870911/536871011	#;(/ NEG-FX3 BN2))
(define NEG-RN12	536870912/536871011	#;(/ NEG-FX4 BN2))

(define NEG-RN13	1/536870913		#;(/ NEG-FX1 BN3))
(define NEG-RN14	-1/536870913		#;(/ NEG-FX2 BN3))
(define NEG-RN15	536870911/536870913	#;(/ NEG-FX3 BN3))
(define NEG-RN16	-536870912/536870913	#;(/ NEG-FX4 BN3))

(define NEG-RN17	1/536871012		#;(/ NEG-FX1 BN4))
(define NEG-RN18	-1/536871012		#;(/ NEG-FX2 BN4))
(define NEG-RN19	536870911/536871012	#;(/ NEG-FX3 BN4))
(define NEG-RN20	-134217728/134217753	#;(/ NEG-FX4 BN4))

;;(define NEG-RN21	-536870912		#;(/ NEG-BN1 FX1)) ;not a ratnum
;;(define NEG-RN22	-536871011		#;(/ NEG-BN2 FX1)) ;not a ratnum
;;(define NEG-RN23	536870913		#;(/ NEG-BN3 FX1)) ;not a ratnum
;;(define NEG-RN24	536871012		#;(/ NEG-BN4 FX1)) ;not a ratnum

;;(define NEG-RN25	536870912		#;(/ NEG-BN1 FX2)) ;not a ratnum
;;(define NEG-RN26	536871011		#;(/ NEG-BN2 FX2)) ;not a ratnum
;;(define NEG-RN27	-536870913		#;(/ NEG-BN3 FX2)) ;not a ratnum
;;(define NEG-RN28	-536871012		#;(/ NEG-BN4 FX2)) ;not a ratnum

(define NEG-RN29	-536870912/536870911	#;(/ NEG-BN1 FX3))
(define NEG-RN30	-536871011/536870911	#;(/ NEG-BN2 FX3))
(define NEG-RN31	536870913/536870911	#;(/ NEG-BN3 FX3))
(define NEG-RN32	536871012/536870911	#;(/ NEG-BN4 FX3))

;;(define NEG-RN33	1			#;(/ NEG-BN1 FX4)) ;not a ratnum
(define NEG-RN34	536871011/536870912	#;(/ NEG-BN2 FX4))
(define NEG-RN35	-536870913/536870912	#;(/ NEG-BN3 FX4))
(define NEG-RN36	-134217753/134217728	#;(/ NEG-BN4 FX4))

;;; --------------------------------------------------------------------

(define FL1		+0.0)
(define FL2		-0.0)
(define FL3		+2.123)
(define FL4		-2.123)
(define FL5		+inf.0)
(define FL6		-inf.0)
(define FL7		+nan.0)

(define NEG-FL1		-0.0)
(define NEG-FL2		+0.0)
(define NEG-FL3		-2.123)
(define NEG-FL4		+2.123)
(define NEG-FL5		-inf.0)
(define NEG-FL6		+inf.0)
(define NEG-FL7		+nan.0)

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

(define NEG-CFL01	-0.0-0.0i)
(define NEG-CFL02	+0.0-0.0i)
(define NEG-CFL03	-0.0+0.0i)
(define NEG-CFL04	+0.0+0.0i)

(define NEG-CFL05	+1.2+0.0i)
(define NEG-CFL06	+1.2-0.0i)
(define NEG-CFL07	-0.0+1.2i)
(define NEG-CFL08	+0.0+1.2i)

(define NEG-CFL09	+1.2+inf.0i)
(define NEG-CFL10	+1.2-inf.0i)
(define NEG-CFL11	-inf.0+1.2i)
(define NEG-CFL12	+inf.0+1.2i)

(define NEG-CFL13	+1.2+nan.0i)
(define NEG-CFL14	+1.2-nan.0i)
(define NEG-CFL15	-nan.0+1.2i)
(define NEG-CFL16	+nan.0+1.2i)


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


;;;; negated compnum constants

(define NEG-CN001		(make-rectangular NEG-FX1 NEG-FX1))
(define NEG-CN002		(make-rectangular NEG-FX2 NEG-FX1))
(define NEG-CN003		(make-rectangular NEG-FX3 NEG-FX1))
(define NEG-CN004		(make-rectangular NEG-FX4 NEG-FX1))

(define NEG-CN005		(make-rectangular NEG-FX1 NEG-FX2))
(define NEG-CN006		(make-rectangular NEG-FX2 NEG-FX2))
(define NEG-CN007		(make-rectangular NEG-FX3 NEG-FX2))
(define NEG-CN008		(make-rectangular NEG-FX4 NEG-FX2))

(define NEG-CN009		(make-rectangular NEG-FX1 NEG-FX3))
(define NEG-CN010		(make-rectangular NEG-FX2 NEG-FX3))
(define NEG-CN011		(make-rectangular NEG-FX3 NEG-FX3))
(define NEG-CN012		(make-rectangular NEG-FX4 NEG-FX3))

(define NEG-CN013		(make-rectangular NEG-FX1 NEG-FX4))
(define NEG-CN014		(make-rectangular NEG-FX2 NEG-FX4))
(define NEG-CN015		(make-rectangular NEG-FX3 NEG-FX4))
(define NEG-CN016		(make-rectangular NEG-FX4 NEG-FX4))

;;; --------------------------------------------------------------------

(define NEG-CN017		(make-rectangular NEG-BN1 NEG-FX1))
(define NEG-CN018		(make-rectangular NEG-BN2 NEG-FX1))
(define NEG-CN019		(make-rectangular NEG-BN3 NEG-FX1))
(define NEG-CN020		(make-rectangular NEG-BN4 NEG-FX1))

(define NEG-CN021		(make-rectangular NEG-BN1 NEG-FX2))
(define NEG-CN022		(make-rectangular NEG-BN2 NEG-FX2))
(define NEG-CN023		(make-rectangular NEG-BN3 NEG-FX2))
(define NEG-CN024		(make-rectangular NEG-BN4 NEG-FX2))

(define NEG-CN025		(make-rectangular NEG-BN1 NEG-FX3))
(define NEG-CN026		(make-rectangular NEG-BN2 NEG-FX3))
(define NEG-CN027		(make-rectangular NEG-BN3 NEG-FX3))
(define NEG-CN028		(make-rectangular NEG-BN4 NEG-FX3))

(define NEG-CN029		(make-rectangular NEG-BN1 NEG-FX4))
(define NEG-CN030		(make-rectangular NEG-BN2 NEG-FX4))
(define NEG-CN031		(make-rectangular NEG-BN3 NEG-FX4))
(define NEG-CN032		(make-rectangular NEG-BN4 NEG-FX4))

;;; --------------------------------------------------------------------

(define NEG-CN033		(make-rectangular NEG-FX1 NEG-BN1))
(define NEG-CN034		(make-rectangular NEG-FX2 NEG-BN1))
(define NEG-CN035		(make-rectangular NEG-FX3 NEG-BN1))
(define NEG-CN036		(make-rectangular NEG-FX4 NEG-BN1))

(define NEG-CN037		(make-rectangular NEG-FX1 NEG-BN2))
(define NEG-CN038		(make-rectangular NEG-FX2 NEG-BN2))
(define NEG-CN039		(make-rectangular NEG-FX3 NEG-BN2))
(define NEG-CN040		(make-rectangular NEG-FX4 NEG-BN2))

(define NEG-CN041		(make-rectangular NEG-FX1 NEG-BN3))
(define NEG-CN042		(make-rectangular NEG-FX2 NEG-BN3))
(define NEG-CN043		(make-rectangular NEG-FX3 NEG-BN3))
(define NEG-CN044		(make-rectangular NEG-FX4 NEG-BN3))

(define NEG-CN045		(make-rectangular NEG-FX1 NEG-BN4))
(define NEG-CN046		(make-rectangular NEG-FX2 NEG-BN4))
(define NEG-CN047		(make-rectangular NEG-FX3 NEG-BN4))
(define NEG-CN048		(make-rectangular NEG-FX4 NEG-BN4))

;;; --------------------------------------------------------------------

(define NEG-CN049		(make-rectangular NEG-BN1 NEG-FL1))
(define NEG-CN050		(make-rectangular NEG-BN2 NEG-FL1))
(define NEG-CN051		(make-rectangular NEG-BN3 NEG-FL1))
(define NEG-CN052		(make-rectangular NEG-BN4 NEG-FL1))

(define NEG-CN053		(make-rectangular NEG-BN1 NEG-FL2))
(define NEG-CN054		(make-rectangular NEG-BN2 NEG-FL2))
(define NEG-CN055		(make-rectangular NEG-BN3 NEG-FL2))
(define NEG-CN056		(make-rectangular NEG-BN4 NEG-FL2))

(define NEG-CN057		(make-rectangular NEG-BN1 NEG-FL3))
(define NEG-CN058		(make-rectangular NEG-BN2 NEG-FL3))
(define NEG-CN059		(make-rectangular NEG-BN3 NEG-FL3))
(define NEG-CN060		(make-rectangular NEG-BN4 NEG-FL3))

(define NEG-CN061		(make-rectangular NEG-BN1 NEG-FL4))
(define NEG-CN062		(make-rectangular NEG-BN2 NEG-FL4))
(define NEG-CN063		(make-rectangular NEG-BN3 NEG-FL4))
(define NEG-CN064		(make-rectangular NEG-BN4 NEG-FL4))

;;; --------------------------------------------------------------------

(define NEG-CN065		(make-rectangular NEG-FL1 NEG-BN1))
(define NEG-CN066		(make-rectangular NEG-FL2 NEG-BN1))
(define NEG-CN067		(make-rectangular NEG-FL3 NEG-BN1))
(define NEG-CN068		(make-rectangular NEG-FL4 NEG-BN1))

(define NEG-CN069		(make-rectangular NEG-FL1 NEG-BN2))
(define NEG-CN070		(make-rectangular NEG-FL2 NEG-BN2))
(define NEG-CN071		(make-rectangular NEG-FL3 NEG-BN2))
(define NEG-CN072		(make-rectangular NEG-FL4 NEG-BN2))

(define NEG-CN073		(make-rectangular NEG-FL1 NEG-BN3))
(define NEG-CN074		(make-rectangular NEG-FL2 NEG-BN3))
(define NEG-CN075		(make-rectangular NEG-FL3 NEG-BN3))
(define NEG-CN076		(make-rectangular NEG-FL4 NEG-BN3))

(define NEG-CN077		(make-rectangular NEG-FL1 NEG-BN4))
(define NEG-CN078		(make-rectangular NEG-FL2 NEG-BN4))
(define NEG-CN079		(make-rectangular NEG-FL3 NEG-BN4))
(define NEG-CN080		(make-rectangular NEG-FL4 NEG-BN4))

;;; --------------------------------------------------------------------

(define NEG-CN081		(make-rectangular NEG-RN01 NEG-FL1))
(define NEG-CN082		(make-rectangular NEG-RN02 NEG-FL1))
(define NEG-CN083		(make-rectangular NEG-RN03 NEG-FL1))
(define NEG-CN084		(make-rectangular NEG-RN04 NEG-FL1))

(define NEG-CN085		(make-rectangular NEG-RN01 NEG-FL2))
(define NEG-CN086		(make-rectangular NEG-RN02 NEG-FL2))
(define NEG-CN087		(make-rectangular NEG-RN03 NEG-FL2))
(define NEG-CN088		(make-rectangular NEG-RN04 NEG-FL2))

(define NEG-CN089		(make-rectangular NEG-RN01 NEG-FL3))
(define NEG-CN090		(make-rectangular NEG-RN02 NEG-FL3))
(define NEG-CN091		(make-rectangular NEG-RN03 NEG-FL3))
(define NEG-CN092		(make-rectangular NEG-RN04 NEG-FL3))

(define NEG-CN093		(make-rectangular NEG-RN01 NEG-FL4))
(define NEG-CN094		(make-rectangular NEG-RN02 NEG-FL4))
(define NEG-CN095		(make-rectangular NEG-RN03 NEG-FL4))
(define NEG-CN096		(make-rectangular NEG-RN04 NEG-FL4))

;;; --------------------------------------------------------------------

(define NEG-CN097		(make-rectangular NEG-FL1 NEG-RN01))
(define NEG-CN098		(make-rectangular NEG-FL2 NEG-RN01))
(define NEG-CN099		(make-rectangular NEG-FL3 NEG-RN01))
(define NEG-CN100		(make-rectangular NEG-FL4 NEG-RN01))

(define NEG-CN101		(make-rectangular NEG-FL1 NEG-RN02))
(define NEG-CN102		(make-rectangular NEG-FL2 NEG-RN02))
(define NEG-CN103		(make-rectangular NEG-FL3 NEG-RN02))
(define NEG-CN104		(make-rectangular NEG-FL4 NEG-RN02))

(define NEG-CN105		(make-rectangular NEG-FL1 NEG-RN03))
(define NEG-CN106		(make-rectangular NEG-FL2 NEG-RN03))
(define NEG-CN107		(make-rectangular NEG-FL3 NEG-RN03))
(define NEG-CN108		(make-rectangular NEG-FL4 NEG-RN03))

(define NEG-CN109		(make-rectangular NEG-FL1 NEG-RN04))
(define NEG-CN110		(make-rectangular NEG-FL2 NEG-RN04))
(define NEG-CN111		(make-rectangular NEG-FL3 NEG-RN04))
(define NEG-CN112		(make-rectangular NEG-FL4 NEG-RN04))


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (make-test - $neg-fixnum))

  (test 0	0)
  (test FX1	NEG-FX1)
  (test FX2	NEG-FX2)
  (test FX3	NEG-FX3)
  (test FX4	NEG-FX4)

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (make-test - $neg-bignum))

  (test BN1 NEG-BN1)
  (test BN2 NEG-BN2)
  (test BN3 NEG-BN3)
  (test BN4 NEG-BN4)

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (make-test - $neg-ratnum))

  (test RN01	NEG-RN01)
  (test RN02	NEG-RN02)
  (test RN03	NEG-RN03)
  (test RN04	NEG-RN04)
  (test RN05	NEG-RN05)
  (test RN06	NEG-RN06)
  (test RN07	NEG-RN07)
  ;;(test RN08	NEG-RN08) ;not a ratnum
  (test RN09	NEG-RN09)

  (test RN10	NEG-RN10)
  (test RN11	NEG-RN11)
  (test RN12	NEG-RN12)
  (test RN13	NEG-RN13)
  (test RN14	NEG-RN14)
  (test RN15	NEG-RN15)
  (test RN16	NEG-RN16)
  (test RN17	NEG-RN17)
  (test RN18	NEG-RN18)
  (test RN19	NEG-RN19)

  (test RN20	NEG-RN20)
  ;;not ratnums
  ;;
  ;; (test RN21	NEG-RN21)
  ;; (test RN22	NEG-RN22)
  ;; (test RN23	NEG-RN23)
  ;; (test RN24	NEG-RN24)
  ;; (test RN25	NEG-RN25)
  ;; (test RN26	NEG-RN26)
  ;; (test RN27	NEG-RN27)
  ;; (test RN28	NEG-RN28)
  (test RN29	NEG-RN29)

  (test RN30	NEG-RN30)
  (test RN31	NEG-RN31)
  (test RN32	NEG-RN32)
  ;;(test RN33	NEG-RN33) ;not a ratnum
  (test RN34	NEG-RN34)
  (test RN35	NEG-RN35)
  (test RN36	NEG-RN36)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-flonum-test - $neg-flonum))

  (test FL1 NEG-FL1)
  (test FL2 NEG-FL2)
  (test FL3 NEG-FL3)
  (test FL4 NEG-FL4)
  (test FL5 NEG-FL5)
  (test FL6 NEG-FL6)
  (test FL7 NEG-FL7)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test - $neg-cflonum))

  (test CFL01 NEG-CFL01)
  (test CFL02 NEG-CFL02)
  (test CFL03 NEG-CFL03)
  (test CFL04 NEG-CFL04)
  (test CFL05 NEG-CFL05)
  (test CFL06 NEG-CFL06)
  (test CFL07 NEG-CFL07)
  (test CFL08 NEG-CFL08)
  (test CFL09 NEG-CFL09)

  (test CFL11 NEG-CFL11)
  (test CFL12 NEG-CFL12)
  (test CFL13 NEG-CFL13)
  (test CFL14 NEG-CFL14)
  (test CFL15 NEG-CFL15)
  (test CFL16 NEG-CFL16)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (make-compnum-test - $neg-compnum))


  (test CN001 NEG-CN001)
  (test CN002 NEG-CN002)
  (test CN003 NEG-CN003)
  (test CN004 NEG-CN004)

  (test CN005 NEG-CN005)
  (test CN006 NEG-CN006)
  (test CN007 NEG-CN007)
  (test CN008 NEG-CN008)

  (test CN009 NEG-CN009)
  (test CN010 NEG-CN010)
  (test CN011 NEG-CN011)
  (test CN012 NEG-CN012)

  (test CN013 NEG-CN013)
  (test CN014 NEG-CN014)
  (test CN015 NEG-CN015)
  (test CN016 NEG-CN016)

;;; --------------------------------------------------------------------

  (test CN017 NEG-CN017)
  (test CN018 NEG-CN018)
  (test CN019 NEG-CN019)
  (test CN020 NEG-CN020)

  (test CN021 NEG-CN021)
  (test CN022 NEG-CN022)
  (test CN023 NEG-CN023)
  (test CN024 NEG-CN024)

  (test CN025 NEG-CN025)
  (test CN026 NEG-CN026)
  (test CN027 NEG-CN027)
  (test CN028 NEG-CN028)

  (test CN029 NEG-CN029)
  (test CN030 NEG-CN030)
  (test CN031 NEG-CN031)
  (test CN032 NEG-CN032)

;;; --------------------------------------------------------------------

  (test CN033 NEG-CN033)
  (test CN034 NEG-CN034)
  (test CN035 NEG-CN035)
  (test CN036 NEG-CN036)

  (test CN037 NEG-CN037)
  (test CN038 NEG-CN038)
  (test CN039 NEG-CN039)
  (test CN040 NEG-CN040)

  (test CN041 NEG-CN041)
  (test CN042 NEG-CN042)
  (test CN043 NEG-CN043)
  (test CN044 NEG-CN044)

  (test CN045 NEG-CN045)
  (test CN046 NEG-CN046)
  (test CN047 NEG-CN047)
  (test CN048 NEG-CN048)

;;; --------------------------------------------------------------------

  (test CN049 NEG-CN049)
  (test CN050 NEG-CN050)
  (test CN051 NEG-CN051)
  (test CN052 NEG-CN052)

  (test CN053 NEG-CN053)
  (test CN054 NEG-CN054)
  (test CN055 NEG-CN055)
  (test CN056 NEG-CN056)

  (test CN057 NEG-CN057)
  (test CN058 NEG-CN058)
  (test CN059 NEG-CN059)
  (test CN060 NEG-CN060)

  (test CN061 NEG-CN061)
  (test CN062 NEG-CN062)
  (test CN063 NEG-CN063)
  (test CN064 NEG-CN064)

;;; --------------------------------------------------------------------

  (test CN065 NEG-CN065)
  (test CN066 NEG-CN066)
  (test CN067 NEG-CN067)
  (test CN068 NEG-CN068)

  (test CN069 NEG-CN069)
  (test CN070 NEG-CN070)
  (test CN071 NEG-CN071)
  (test CN072 NEG-CN072)

  (test CN073 NEG-CN073)
  (test CN074 NEG-CN074)
  (test CN075 NEG-CN075)
  (test CN076 NEG-CN076)

  (test CN077 NEG-CN077)
  (test CN078 NEG-CN078)
  (test CN079 NEG-CN079)
  (test CN080 NEG-CN080)

;;; --------------------------------------------------------------------

  (test CN081 NEG-CN081)
  (test CN082 NEG-CN082)
  (test CN083 NEG-CN083)
  (test CN084 NEG-CN084)

  (test CN085 NEG-CN085)
  (test CN086 NEG-CN086)
  (test CN087 NEG-CN087)
  (test CN088 NEG-CN088)

  (test CN089 NEG-CN089)
  (test CN090 NEG-CN090)
  (test CN091 NEG-CN091)
  (test CN092 NEG-CN092)

  (test CN093 NEG-CN093)
  (test CN094 NEG-CN094)
  (test CN095 NEG-CN095)
  (test CN096 NEG-CN096)

;;; --------------------------------------------------------------------

  (test CN097 NEG-CN097)
  (test CN098 NEG-CN098)
  (test CN099 NEG-CN099)
  (test CN100 NEG-CN100)

  (test CN101 NEG-CN101)
  (test CN102 NEG-CN102)
  (test CN103 NEG-CN103)
  (test CN104 NEG-CN104)

  (test CN105 NEG-CN105)
  (test CN106 NEG-CN106)
  (test CN107 NEG-CN107)
  (test CN108 NEG-CN108)

  (test CN109 NEG-CN109)
  (test CN110 NEG-CN110)
  (test CN111 NEG-CN111)
  (test CN112 NEG-CN112)

  #t)


;;;; done

(check-report)

;;; end of file
