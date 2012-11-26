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
(display "*** testing Vicare numerics functions: neg, unary minus\n")


;;;; helpers

(define GREATEST-FX		+536870911)
(define LEAST-FX		-536870912)

(define NEG-GREATEST-FX		-536870911)
(define NEG-LEAST-FX		+536870912)

(define FX1			+1)
(define FX2			-1)
(define FX3			GREATEST-FX)
(define FX4			LEAST-FX)

(define NEG-FX1			-1)
(define NEG-FX2			+1)
(define NEG-FX3			NEG-GREATEST-FX)
(define NEG-FX4			NEG-LEAST-FX)

(define BN1			+536870912) ;GREATEST-FX + 1
(define BN2			+536871011) ;GREATEST-FX + 100
(define BN3			-536870913) ;LEAST-FX - 1
(define BN4			-536871012) ;LEAST-FX - 100

(define NEG-BN1			-536870912)
(define NEG-BN2			-536871011)
(define NEG-BN3			+536870913)
(define NEG-BN4			+536871012)

(define RN1-FX-FX		+1/2)
(define RN2-FX-FX		($make-ratnum GREATEST-FX 123))
(define RN3-FX-FX		($make-ratnum LEAST-FX    123))
(define RN4-FX-BN		($make-ratnum FX2 BN1))

(define NEG-RN1			-1/2)
(define NEG-RN2			($make-ratnum NEG-GREATEST-FX 123))
(define NEG-RN3			($make-ratnum NEG-LEAST-FX    123))


(parametrise ((check-test-name	'neg))

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

;;; --------------------------------------------------------------------

  (let-syntax ((test (make-test - $neg-fixnum)))
    (test 0	0)
    (test FX1	NEG-FX1)
    (test FX2	NEG-FX2)
    (test FX3	NEG-FX3)
    (test FX4	NEG-FX4)
    #f)

;;; --------------------------------------------------------------------

  (let-syntax ((test (make-test - $neg-bignum)))
    (test BN1 NEG-BN1)
    (test BN2 NEG-BN2)
    (test BN3 NEG-BN3)
    (test BN4 NEG-BN4)
    #f)

;;; --------------------------------------------------------------------

  (let-syntax ((test (make-test - $neg-ratnum)))
    (test RN1-FX-FX	NEG-RN1)
    (test RN2-FX-FX	NEG-RN2)
    (test RN3-FX-FX	NEG-RN3)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
