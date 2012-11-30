;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: remainder
;;;Date: Fri Nov 30, 2012
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
(check-display "*** testing Vicare numerics functions: remainder, integer division\n")


;;;; helpers

(define C make-rectangular)
(define R real-part)
(define I imag-part)

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

;;Only integer flonums for this operation.
(define FL1		+0.0)
(define FL2		-0.0)
(define FL3		+1.0)
(define FL4		-1.0)
(define FL5		+2.0)
(define FL6		-2.0)


(parametrise ((check-test-name	'fixnums))

  (let-syntax ((test (make-test remainder $remainder-fixnum-fixnum)))
    (test 0 1 0)
    (test 0 -1 0)
    (test FX1 FX1 0)
    (test FX2 FX1 0)
    (test FX3 FX1 0)
    (test FX4 FX1 0)
    (test FX1 FX2 0)
    (test FX2 FX2 0)
    (test FX3 FX2 0)
    (test FX4 FX2 0)
    (test FX1 FX3 1)
    (test FX2 FX3 -1)
    (test FX3 FX3 0)
    (test FX4 FX3 -1)
    (test FX1 FX4 1)
    (test FX2 FX4 -1)
    (test FX3 FX4 536870911)
    (test FX4 FX4 0)
    #f)

  (let-syntax ((test (make-test remainder $remainder-fixnum-bignum)))
    (test FX1 BN1 1)
    (test FX2 BN1 -1)
    (test FX3 BN1 536870911)
    (test FX4 BN1 0)
    (test FX1 BN2 1)
    (test FX2 BN2 -1)
    (test FX3 BN2 536870911)
    (test FX4 BN2 -536870912)
    (test FX1 BN3 1)
    (test FX2 BN3 -1)
    (test FX3 BN3 536870911)
    (test FX4 BN3 -536870912)
    (test FX1 BN4 1)
    (test FX2 BN4 -1)
    (test FX3 BN4 536870911)
    (test FX4 BN4 -536870912)
    #f)

  (let-syntax ((test (make-flonum-test remainder $remainder-fixnum-flonum)))
    (test FX1 FL3 0.0)
    (test FX2 FL3 -0.0)
    (test FX3 FL3 0.0)
    (test FX4 FL3 -0.0)
    (test FX1 FL4 0.0)
    (test FX2 FL4 -0.0)
    (test FX3 FL4 0.0)
    (test FX4 FL4 -0.0)
    (test FX1 FL5 1.0)
    (test FX2 FL5 -1.0)
    (test FX3 FL5 1.0)
    (test FX4 FL5 -0.0)
    (test FX1 FL6 1.0)
    (test FX2 FL6 -1.0)
    (test FX3 FL6 1.0)
    (test FX4 FL6 -0.0)
    #f)

  #t)


(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-test remainder $remainder-bignum-fixnum)))
    (test BN1 FX1 0)
    (test BN2 FX1 0)
    (test BN3 FX1 0)
    (test BN4 FX1 0)
    (test BN1 FX2 0)
    (test BN2 FX2 0)
    (test BN3 FX2 0)
    (test BN4 FX2 0)
    (test BN1 FX3 1)
    (test BN2 FX3 100)
    (test BN3 FX3 -2)
    (test BN4 FX3 -101)
    (test BN1 FX4 0)
    (test BN2 FX4 99)
    (test BN3 FX4 -1)
    (test BN4 FX4 -100)
    #f)

  (let-syntax ((test (make-test remainder $remainder-bignum-bignum)))
    (test BN1 BN1 0)
    (test BN2 BN1 99)
    (test BN3 BN1 -1)
    (test BN4 BN1 -100)
    (test BN1 BN2 536870912)
    (test BN2 BN2 0)
    (test BN3 BN2 -536870913)
    (test BN4 BN2 -1)
    (test BN1 BN3 536870912)
    (test BN2 BN3 98)
    (test BN3 BN3 0)
    (test BN4 BN3 -99)
    (test BN1 BN4 536870912)
    (test BN2 BN4 536871011)
    (test BN3 BN4 -536870913)
    (test BN4 BN4 0)
    #f)

  (let-syntax ((test (make-inexact-test remainder $remainder-bignum-flonum)))
    (test BN1 FL3 0.0)
    (test BN2 FL3 0.0)
    (test BN3 FL3 0.0)
    (test BN4 FL3 0.0)
    (test BN1 FL4 0.0)
    (test BN2 FL4 0.0)
    (test BN3 FL4 0.0)
    (test BN4 FL4 0.0)
    (test BN1 FL5 0.0)
    (test BN2 FL5 1.0)
    (test BN3 FL5 -1.0)
    (test BN4 FL5 0.0)
    (test BN1 FL6 0.0)
    (test BN2 FL6 1.0)
    (test BN3 FL6 -1.0)
    (test BN4 FL6 0.0)
    #f)

  #t)


(parametrise ((check-test-name	'flonums))

  (let-syntax ((test (make-inexact-test remainder $remainder-flonum-fixnum)))
    (test FL1 FX1 0.0)
    (test FL2 FX1 0.0)
    (test FL3 FX1 0.0)
    (test FL4 FX1 0.0)
    (test FL5 FX1 0.0)
    (test FL6 FX1 0.0)
    (test FL1 FX2 0.0)
    (test FL2 FX2 0.0)
    (test FL3 FX2 0.0)
    (test FL4 FX2 0.0)
    (test FL5 FX2 0.0)
    (test FL6 FX2 0.0)
    (test FL1 FX3 0.0)
    (test FL2 FX3 0.0)
    (test FL3 FX3 1.0)
    (test FL4 FX3 -1.0)
    (test FL5 FX3 2.0)
    (test FL6 FX3 -2.0)
    (test FL1 FX4 0.0)
    (test FL2 FX4 0.0)
    (test FL3 FX4 1.0)
    (test FL4 FX4 -1.0)
    (test FL5 FX4 2.0)
    (test FL6 FX4 -2.0)
    #f)

  (let-syntax ((test (make-inexact-test remainder $remainder-flonum-bignum)))
    (test FL1 BN1 0.0)
    (test FL2 BN1 0.0)
    (test FL3 BN1 1.0)
    (test FL4 BN1 -1.0)
    (test FL5 BN1 2.0)
    (test FL6 BN1 -2.0)
    (test FL1 BN2 0.0)
    (test FL2 BN2 0.0)
    (test FL3 BN2 1.0)
    (test FL4 BN2 -1.0)
    (test FL5 BN2 2.0)
    (test FL6 BN2 -2.0)
    (test FL1 BN3 0.0)
    (test FL2 BN3 0.0)
    (test FL3 BN3 1.0)
    (test FL4 BN3 -1.0)
    (test FL5 BN3 2.0)
    (test FL6 BN3 -2.0)
    (test FL1 BN4 0.0)
    (test FL2 BN4 0.0)
    (test FL3 BN4 1.0)
    (test FL4 BN4 -1.0)
    (test FL5 BN4 2.0)
    (test FL6 BN4 -2.0)
    #f)

  (let-syntax ((test (make-flonum-test remainder $remainder-flonum-flonum)))
    (test 25.0 10.0 5.0)
    (test 10.0 25.0 10.0)
    (test FL1 FL3 0.0)
    (test FL2 FL3 -0.0)
    (test FL3 FL3 0.0)
    (test FL4 FL3 -0.0)
    (test FL5 FL3 0.0)
    (test FL6 FL3 -0.0)
    (test FL1 FL4 0.0)
    (test FL2 FL4 -0.0)
    (test FL3 FL4 0.0)
    (test FL4 FL4 -0.0)
    (test FL5 FL4 0.0)
    (test FL6 FL4 -0.0)
    (test FL1 FL5 0.0)
    (test FL2 FL5 -0.0)
    (test FL3 FL5 1.0)
    (test FL4 FL5 -1.0)
    (test FL5 FL5 0.0)
    (test FL6 FL5 -0.0)
    (test FL1 FL6 0.0)
    (test FL2 FL6 -0.0)
    (test FL3 FL6 1.0)
    (test FL4 FL6 -1.0)
    (test FL5 FL6 0.0)
    (test FL6 FL6 -0.0)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
