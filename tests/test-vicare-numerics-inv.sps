;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: inv
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
(check-display "*** testing Vicare numerics functions: inv, unary division\n")


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

(define INV-GREATEST-FX		-536870911)
(define INV-LEAST-FX		+536870912)

;;; --------------------------------------------------------------------

(define FX1		+1)
(define FX2		-1)
(define FX3		GREATEST-FX)
(define FX4		LEAST-FX)

(define INV-FX1		+1)
(define INV-FX2		-1)
(define INV-FX3		+1/536870911 #;(/ GREATEST-FX))
(define INV-FX4		-1/536870912 #;(/ INV-LEAST-FX))

;;; --------------------------------------------------------------------

(define BN1		+536870912) ;GREATEST-FX + 1
(define BN2		+536871011) ;GREATEST-FX + 100
(define BN3		-536870913) ;LEAST-FX - 1
(define BN4		-536871012) ;LEAST-FX - 100

(define INV-BN1		+1/536870912)
(define INV-BN2		+1/536871011)
(define INV-BN3		-1/536870913)
(define INV-BN4		-1/536871012)

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

(define INV-RN01	123			#;(/ INV-FX1 123))
(define INV-RN02	-123			#;(/ INV-FX2 123))
(define INV-RN03	-123			#;(/ INV-FX2 123))
(define INV-RN04	-123/536870912		#;(/ INV-FX4 123))

(define INV-RN05	536870912		#;(/ INV-FX1 BN1))
(define INV-RN06	-536870912		#;(/ INV-FX2 BN1))
(define INV-RN07	536870912/536870911	#;(/ INV-FX3 BN1))
;;(define INV-RN08	1			#;(/ INV-FX4 BN1)) ;not a ratnum

(define INV-RN09	536871011		#;(/ INV-FX1 BN2))
(define INV-RN10	-536871011		#;(/ INV-FX2 BN2))
(define INV-RN11	536871011/536870911	#;(/ INV-FX3 BN2))
(define INV-RN12	-536871011/536870912	#;(/ INV-FX4 BN2))

(define INV-RN13	-536870913		#;(/ INV-FX1 BN3))
(define INV-RN14	536870913		#;(/ INV-FX2 BN3))
(define INV-RN15	-536870913/536870911	#;(/ INV-FX3 BN3))
(define INV-RN16	536870913/536870912	#;(/ INV-FX4 BN3))

(define INV-RN17	-536871012		#;(/ INV-FX1 BN4))
(define INV-RN18	536871012		#;(/ INV-FX2 BN4))
(define INV-RN19	-536871012/536870911	#;(/ INV-FX3 BN4))
(define INV-RN20	134217753/134217728	#;(/ INV-FX4 BN4))

;;(define INV-RN21	-536870912		#;(/ INV-BN1 FX1)) ;not a ratnum
;;(define INV-RN22	-536871011		#;(/ INV-BN2 FX1)) ;not a ratnum
;;(define INV-RN23	536870913		#;(/ INV-BN3 FX1)) ;not a ratnum
;;(define INV-RN24	536871012		#;(/ INV-BN4 FX1)) ;not a ratnum

;;(define INV-RN25	536870912		#;(/ INV-BN1 FX2)) ;not a ratnum
;;(define INV-RN26	536871011		#;(/ INV-BN2 FX2)) ;not a ratnum
;;(define INV-RN27	-536870913		#;(/ INV-BN3 FX2)) ;not a ratnum
;;(define INV-RN28	-536871012		#;(/ INV-BN4 FX2)) ;not a ratnum

(define INV-RN29	536870911/536870912	#;(/ INV-BN1 FX3))
(define INV-RN30	536870911/536871011	#;(/ INV-BN2 FX3))
(define INV-RN31	-536870911/536870913	#;(/ INV-BN3 FX3))
(define INV-RN32	-536870911/536871012	#;(/ INV-BN4 FX3))

;;(define INV-RN33	-1			#;(/ INV-BN1 FX4)) ;not a ratnum
(define INV-RN34	-536870912/536871011	#;(/ INV-BN2 FX4))
(define INV-RN35	536870912/536870913	#;(/ INV-BN3 FX4))
(define INV-RN36	134217728/134217753	#;(/ INV-BN4 FX4))

;;; --------------------------------------------------------------------

(define FL1		+0.0)
(define FL2		-0.0)
(define FL3		+2.123)
(define FL4		-2.123)
(define FL5		+inf.0)
(define FL6		-inf.0)
(define FL7		+nan.0)

(define INV-FL1		+inf.0)
(define INV-FL2		-inf.0)
(define INV-FL3		0.4710315591144606)
(define INV-FL4		-0.4710315591144606)
(define INV-FL5		+0.0)
(define INV-FL6		-0.0)
(define INV-FL7		+nan.0)

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

(define INV-CFL01	+nan.0+nan.0i)
(define INV-CFL02	+nan.0+nan.0i)
(define INV-CFL03	+nan.0+nan.0i)
(define INV-CFL04	+nan.0+nan.0i)

(define INV-CFL05	-0.8333333333333333+0.0i)
(define INV-CFL06	-0.8333333333333333-0.0i)
(define INV-CFL07	0.0+0.8333333333333333i)
(define INV-CFL08	-0.0+0.8333333333333333i)

(define INV-CFL09	-0.0+nan.0i)
(define INV-CFL10	-0.0+nan.0i)
(define INV-CFL11	+nan.0+0.0i)
(define INV-CFL12	+nan.0+0.0i)

(define INV-CFL13	+nan.0+nan.0i)
(define INV-CFL14	+nan.0+nan.0i)
(define INV-CFL15	+nan.0+nan.0i)
(define INV-CFL16	+nan.0+nan.0i)


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

(define INV-CN001 1/2-1/2i)
(define INV-CN002 -1/2-1/2i)
(define INV-CN003 536870911/288230375077969922-1/288230375077969922i)
(define INV-CN004 -536870912/288230376151711745-1/288230376151711745i)
(define INV-CN005 1/2+1/2i)
(define INV-CN006 -1/2+1/2i)
(define INV-CN007 536870911/288230375077969922+1/288230375077969922i)
(define INV-CN008 -536870912/288230376151711745+1/288230376151711745i)
(define INV-CN009 1/288230375077969922-536870911/288230375077969922i)
(define INV-CN010 -1/288230375077969922-536870911/288230375077969922i)
(define INV-CN011 1/1073741822-1/1073741822i)
(define INV-CN012 -536870912/576460751229681665-536870911/576460751229681665i)
(define INV-CN013 1/288230376151711745+536870912/288230376151711745i)
(define INV-CN014 -1/288230376151711745+536870912/288230376151711745i)
(define INV-CN015 536870911/576460751229681665+536870912/576460751229681665i)
(define INV-CN016 -1/1073741824+1/1073741824i)
(define INV-CN017 536870912/288230376151711745-1/288230376151711745i)
(define INV-CN018 536871011/288230482452162122-1/288230482452162122i)
(define INV-CN019 -536870913/288230377225453570-1/288230377225453570i)
(define INV-CN020 -536871012/288230483525904145-1/288230483525904145i)
(define INV-CN021 536870912/288230376151711745+1/288230376151711745i)
(define INV-CN022 536871011/288230482452162122+1/288230482452162122i)
(define INV-CN023 -536870913/288230377225453570+1/288230377225453570i)
(define INV-CN024 -536871012/288230483525904145+1/288230483525904145i)
(define INV-CN025 536870912/576460751229681665-536870911/576460751229681665i)
(define INV-CN026 536871011/576460857530132042-536870911/576460857530132042i)
(define INV-CN027 -536870913/576460752303423490-536870911/576460752303423490i)
(define INV-CN028 -536871012/576460858603874065-536870911/576460858603874065i)
(define INV-CN029 1/1073741824+1/1073741824i)
(define INV-CN030 536871011/576460858603873865+536870912/576460858603873865i)
(define INV-CN031 -536870913/576460753377165313+536870912/576460753377165313i)
(define INV-CN032 -134217753/144115214919403972+33554432/36028803729850993i)
(define INV-CN033 1/288230376151711745-536870912/288230376151711745i)
(define INV-CN034 -1/288230376151711745-536870912/288230376151711745i)
(define INV-CN035 536870911/576460751229681665-536870912/576460751229681665i)
(define INV-CN036 -1/1073741824-1/1073741824i)
(define INV-CN037 1/288230482452162122-536871011/288230482452162122i)
(define INV-CN038 -1/288230482452162122-536871011/288230482452162122i)
(define INV-CN039 536870911/576460857530132042-536871011/576460857530132042i)
(define INV-CN040 -536870912/576460858603873865-536871011/576460858603873865i)
(define INV-CN041 1/288230377225453570+536870913/288230377225453570i)
(define INV-CN042 -1/288230377225453570+536870913/288230377225453570i)
(define INV-CN043 536870911/576460752303423490+536870913/576460752303423490i)
(define INV-CN044 -536870912/576460753377165313+536870913/576460753377165313i)
(define INV-CN045 1/288230483525904145+536871012/288230483525904145i)
(define INV-CN046 -1/288230483525904145+536871012/288230483525904145i)
(define INV-CN047 536870911/576460858603874065+536871012/576460858603874065i)
(define INV-CN048 -33554432/36028803729850993+134217753/144115214919403972i)
(define INV-CN049 1.862645149230957e-9-0.0i)
(define INV-CN050 1.862644805755772e-9-0.0i)
(define INV-CN051 -1.86264514576151e-9-0.0i)
(define INV-CN052 -1.8626448022863266e-9-0.0i)
(define INV-CN053 1.862645149230957e-9+0.0i)
(define INV-CN054 1.862644805755772e-9+0.0i)
(define INV-CN055 -1.86264514576151e-9+0.0i)
(define INV-CN056 -1.8626448022863266e-9+0.0i)
(define INV-CN057 1.862645149230957e-9-7.365635878997524e-18i)
(define INV-CN058 1.862644805755772e-9-7.365633162524219e-18i)
(define INV-CN059 -1.86264514576151e-9-7.365635851558391e-18i)
(define INV-CN060 -1.8626448022863266e-9-7.365633135085102e-18i)
(define INV-CN061 1.862645149230957e-9+7.365635878997524e-18i)
(define INV-CN062 1.862644805755772e-9+7.365633162524219e-18i)
(define INV-CN063 -1.86264514576151e-9+7.365635851558391e-18i)
(define INV-CN064 -1.8626448022863266e-9+7.365633135085102e-18i)
(define INV-CN065 0.0-1.862645149230957e-9i)
(define INV-CN066 -0.0-1.862645149230957e-9i)
(define INV-CN067 7.365635878997524e-18-1.862645149230957e-9i)
(define INV-CN068 -7.365635878997524e-18-1.862645149230957e-9i)
(define INV-CN069 0.0-1.862644805755772e-9i)
(define INV-CN070 -0.0-1.862644805755772e-9i)
(define INV-CN071 7.365633162524219e-18-1.862644805755772e-9i)
(define INV-CN072 -7.365633162524219e-18-1.862644805755772e-9i)
(define INV-CN073 0.0+1.86264514576151e-9i)
(define INV-CN074 -0.0+1.86264514576151e-9i)
(define INV-CN075 7.365635851558391e-18+1.86264514576151e-9i)
(define INV-CN076 -7.365635851558391e-18+1.86264514576151e-9i)
(define INV-CN077 0.0+1.8626448022863266e-9i)
(define INV-CN078 -0.0+1.8626448022863266e-9i)
(define INV-CN079 7.365633135085102e-18+1.8626448022863266e-9i)
(define INV-CN080 -7.365633135085102e-18+1.8626448022863266e-9i)
(define INV-CN081 123.0-0.0i)
(define INV-CN082 -123.0-0.0i)
(define INV-CN083 -123.0-0.0i)
(define INV-CN084 -2.2910535335540771e-7-0.0i)
(define INV-CN085 123.0+0.0i)
(define INV-CN086 -123.0+0.0i)
(define INV-CN087 -123.0+0.0i)
(define INV-CN088 -2.2910535335540771e-7+0.0i)
(define INV-CN089 0.0018038006173773664-0.47102465141513433i)
(define INV-CN090 -0.0018038006173773664-0.47102465141513433i)
(define INV-CN091 -0.0018038006173773664-0.47102465141513433i)
(define INV-CN092 -2.291053533553535e-7-1.1143470521332715e-13i)
(define INV-CN093 0.0018038006173773664+0.47102465141513433i)
(define INV-CN094 -0.0018038006173773664+0.47102465141513433i)
(define INV-CN095 -0.0018038006173773664+0.47102465141513433i)
(define INV-CN096 -2.291053533553535e-7+1.1143470521332715e-13i)
(define INV-CN097 0.0-123.0i)
(define INV-CN098 -0.0-123.0i)
(define INV-CN099 0.47102465141513433-0.0018038006173773664i)
(define INV-CN100 -0.47102465141513433-0.0018038006173773664i)
(define INV-CN101 0.0+123.0i)
(define INV-CN102 -0.0+123.0i)
(define INV-CN103 0.47102465141513433+0.0018038006173773664i)
(define INV-CN104 -0.47102465141513433+0.0018038006173773664i)
(define INV-CN105 0.0+123.0i)
(define INV-CN106 -0.0+123.0i)
(define INV-CN107 0.47102465141513433+0.0018038006173773664i)
(define INV-CN108 -0.47102465141513433+0.0018038006173773664i)
(define INV-CN109 0.0+2.2910535335540771e-7i)
(define INV-CN110 -0.0+2.2910535335540771e-7i)
(define INV-CN111 1.1143470521332715e-13+2.291053533553535e-7i)
(define INV-CN112 -1.1143470521332715e-13+2.291053533553535e-7i)


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (make-test / $inv-fixnum))

  #;(test 0	0)
  (test FX1	INV-FX1)
  (test FX2	INV-FX2)
  (test FX3	INV-FX3)
  (test FX4	INV-FX4)

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (make-test / $inv-bignum))

  (test BN1 INV-BN1)
  (test BN2 INV-BN2)
  (test BN3 INV-BN3)
  (test BN4 INV-BN4)

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (make-test / $inv-ratnum))

  (test RN01	INV-RN01)
  (test RN02	INV-RN02)
  (test RN03	INV-RN03)
  (test RN04	INV-RN04)
  (test RN05	INV-RN05)
  (test RN06	INV-RN06)
  (test RN07	INV-RN07)
  ;;(test RN08	INV-RN08) ;not a ratnum
  (test RN09	INV-RN09)

  (test RN10	INV-RN10)
  (test RN11	INV-RN11)
  (test RN12	INV-RN12)
  (test RN13	INV-RN13)
  (test RN14	INV-RN14)
  (test RN15	INV-RN15)
  (test RN16	INV-RN16)
  (test RN17	INV-RN17)
  (test RN18	INV-RN18)
  (test RN19	INV-RN19)

  (test RN20	INV-RN20)
  ;;not ratnums
  ;;
  ;; (test RN21	INV-RN21)
  ;; (test RN22	INV-RN22)
  ;; (test RN23	INV-RN23)
  ;; (test RN24	INV-RN24)
  ;; (test RN25	INV-RN25)
  ;; (test RN26	INV-RN26)
  ;; (test RN27	INV-RN27)
  ;; (test RN28	INV-RN28)
  (test RN29	INV-RN29)

  (test RN30	INV-RN30)
  (test RN31	INV-RN31)
  (test RN32	INV-RN32)
  ;;(test RN33	INV-RN33) ;not a ratnum
  (test RN34	INV-RN34)
  (test RN35	INV-RN35)
  (test RN36	INV-RN36)

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (make-flonum-test / $inv-flonum))

  (test FL1 INV-FL1)
  (test FL2 INV-FL2)
  (test FL3 INV-FL3)
  (test FL4 INV-FL4)
  (test FL5 INV-FL5)
  (test FL6 INV-FL6)
  (test FL7 INV-FL7)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (make-cflonum-test / $inv-cflonum))

  (test CFL01 INV-CFL01)
  (test CFL02 INV-CFL02)
  (test CFL03 INV-CFL03)
  (test CFL04 INV-CFL04)
  (test CFL05 INV-CFL05)
  (test CFL06 INV-CFL06)
  (test CFL07 INV-CFL07)
  (test CFL08 INV-CFL08)
  (test CFL09 INV-CFL09)

  (test CFL11 INV-CFL11)
  (test CFL12 INV-CFL12)
  (test CFL13 INV-CFL13)
  (test CFL14 INV-CFL14)
  (test CFL15 INV-CFL15)
  (test CFL16 INV-CFL16)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (make-compnum-test / $inv-compnum))


  (test CN001 INV-CN001)
  (test CN002 INV-CN002)
  (test CN003 INV-CN003)
  (test CN004 INV-CN004)

  (test CN005 INV-CN005)
  (test CN006 INV-CN006)
  (test CN007 INV-CN007)
  (test CN008 INV-CN008)

  (test CN009 INV-CN009)
  (test CN010 INV-CN010)
  (test CN011 INV-CN011)
  (test CN012 INV-CN012)

  (test CN013 INV-CN013)
  (test CN014 INV-CN014)
  (test CN015 INV-CN015)
  (test CN016 INV-CN016)

;;; --------------------------------------------------------------------

  (test CN017 INV-CN017)
  (test CN018 INV-CN018)
  (test CN019 INV-CN019)
  (test CN020 INV-CN020)

  (test CN021 INV-CN021)
  (test CN022 INV-CN022)
  (test CN023 INV-CN023)
  (test CN024 INV-CN024)

  (test CN025 INV-CN025)
  (test CN026 INV-CN026)
  (test CN027 INV-CN027)
  (test CN028 INV-CN028)

  (test CN029 INV-CN029)
  (test CN030 INV-CN030)
  (test CN031 INV-CN031)
  (test CN032 INV-CN032)

;;; --------------------------------------------------------------------

  (test CN033 INV-CN033)
  (test CN034 INV-CN034)
  (test CN035 INV-CN035)
  (test CN036 INV-CN036)

  (test CN037 INV-CN037)
  (test CN038 INV-CN038)
  (test CN039 INV-CN039)
  (test CN040 INV-CN040)

  (test CN041 INV-CN041)
  (test CN042 INV-CN042)
  (test CN043 INV-CN043)
  (test CN044 INV-CN044)

  (test CN045 INV-CN045)
  (test CN046 INV-CN046)
  (test CN047 INV-CN047)
  (test CN048 INV-CN048)

;;; --------------------------------------------------------------------

  (test CN049 INV-CN049)
  (test CN050 INV-CN050)
  (test CN051 INV-CN051)
  (test CN052 INV-CN052)

  (test CN053 INV-CN053)
  (test CN054 INV-CN054)
  (test CN055 INV-CN055)
  (test CN056 INV-CN056)

  (test CN057 INV-CN057)
  (test CN058 INV-CN058)
  (test CN059 INV-CN059)
  (test CN060 INV-CN060)

  (test CN061 INV-CN061)
  (test CN062 INV-CN062)
  (test CN063 INV-CN063)
  (test CN064 INV-CN064)

;;; --------------------------------------------------------------------

  (test CN065 INV-CN065)
  (test CN066 INV-CN066)
  (test CN067 INV-CN067)
  (test CN068 INV-CN068)

  (test CN069 INV-CN069)
  (test CN070 INV-CN070)
  (test CN071 INV-CN071)
  (test CN072 INV-CN072)

  (test CN073 INV-CN073)
  (test CN074 INV-CN074)
  (test CN075 INV-CN075)
  (test CN076 INV-CN076)

  (test CN077 INV-CN077)
  (test CN078 INV-CN078)
  (test CN079 INV-CN079)
  (test CN080 INV-CN080)

;;; --------------------------------------------------------------------

  (test CN081 INV-CN081)
  (test CN082 INV-CN082)
  (test CN083 INV-CN083)
  (test CN084 INV-CN084)

  (test CN085 INV-CN085)
  (test CN086 INV-CN086)
  (test CN087 INV-CN087)
  (test CN088 INV-CN088)

  (test CN089 INV-CN089)
  (test CN090 INV-CN090)
  (test CN091 INV-CN091)
  (test CN092 INV-CN092)

  (test CN093 INV-CN093)
  (test CN094 INV-CN094)
  (test CN095 INV-CN095)
  (test CN096 INV-CN096)

;;; --------------------------------------------------------------------

  (test CN097 INV-CN097)
  (test CN098 INV-CN098)
  (test CN099 INV-CN099)
  (test CN100 INV-CN100)

  (test CN101 INV-CN101)
  (test CN102 INV-CN102)
  (test CN103 INV-CN103)
  (test CN104 INV-CN104)

  (test CN105 INV-CN105)
  (test CN106 INV-CN106)
  (test CN107 INV-CN107)
  (test CN108 INV-CN108)

  (test CN109 INV-CN109)
  (test CN110 INV-CN110)
  (test CN111 INV-CN111)
  (test CN112 INV-CN112)

  #t)


;;;; done

(check-report)

;;; end of file
