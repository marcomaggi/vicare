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


;;;; done

(check-report)

;;; end of file
