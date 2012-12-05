;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: expt
;;;Date: Sun Dec  2, 2012
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
(check-display "*** testing Vicare numerics functions: raising to power\n")


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

;;; --------------------------------------------------------------------

(define (cube x)
  (* x x x))


;;;; constants

(define SMALLEST-POSITIVE-BIGNUM	(-    (least-fixnum)))
(define SMALLEST-NEGATIVE-BIGNUM	(+ -1 (least-fixnum)))

(define BN1	(+ +1  SMALLEST-POSITIVE-BIGNUM))
(define BN2	(+ +10 SMALLEST-POSITIVE-BIGNUM))
(define BN3	(+ -1  SMALLEST-NEGATIVE-BIGNUM))
(define BN4	(+ -10 SMALLEST-NEGATIVE-BIGNUM))


(parametrise ((check-test-name	'zero-fixnum-exponent))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check ($expt-number-zero-fixnum ?op) => ?expected)
	 (check ($expt-number-fixnum	?op 0) => ?expected)
	 (check (expt			?op 0) => ?expected)))
      ((_ ?op ?expected ?equal)
       (begin
	 (check ($expt-number-zero-fixnum ?op) (=> ?equal) ?expected)
	 (check ($expt-number-fixnum	?op 0) (=> ?equal) ?expected)
	 (check (expt			?op 0) (=> ?equal) ?expected)))
      ))

;;; --------------------------------------------------------------------

  ;; fixnums
  (test  0	+1)
  (test +1	+1)
  (test -1	+1)
  (test +21	+1)
  (test -21	+1)

  ;; bignums
  (test SMALLEST-POSITIVE-BIGNUM  +1)
  (test SMALLEST-NEGATIVE-BIGNUM  +1)
  (test BN1  +1)
  (test BN2  +1)
  (test BN3  +1)
  (test BN4  +1)

  ;; ratnum
  (test 123/456					+1)
  (test (/ SMALLEST-POSITIVE-BIGNUM 456)	+1)
  (test (/ 456 SMALLEST-POSITIVE-BIGNUM)	+1)

  ;; flonums
  (test 123.456		 +1.0)
  (test +nan.0		 +nan.0)
  (test +inf.0		 +1.0)
  (test -inf.0		 +1.0)

  ;; compnums
  (test 123+456i	 +1)
  (test 12.3+456i	 +1.0+0.0i)
  (test 123+45.6i	 +1.0+0.0i)
  (test 1/23+4/56i	 +1)

  (test (make-rectangular 123 BN1)	+1)
  (test (make-rectangular BN1 456)	+1)
  (test (make-rectangular 1.23 BN1)	+1.0+0.0i)
  (test (make-rectangular BN1 4.56)	+1.0+0.0i)
  (test (make-rectangular 1/23 BN1)	+1)
  (test (make-rectangular BN1 4/56)	+1)

  ;; cflonums
  (test 12.3+45.6i	+1.0+0.0i		compnum=?)
  (test 12.3+0.0i	+1.0+0.0i		compnum=?)
  (test  0.0+45.6i	+1.0+0.0i		compnum=?)
  (test +inf.0+45.6i	+1.0+0.0i		compnum=?)
  (test 12.3+inf.0i	+1.0+0.0i		compnum=?)
  (test +inf.0+inf.0i	+1.0+0.0i		compnum=?)
  (test +nan.0+45.6i	+nan.0+nan.0i		compnum=?)
  (test 12.3+nan.0i	+nan.0+nan.0i		compnum=?)
  (test +nan.0+nan.0i	+nan.0+nan.0i		compnum=?)

  #t)


(parametrise ((check-test-name	'positive-fixnum-exponent))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op1 ?op2 ?expected)
       (begin
	 (check ($expt-number-positive-fixnum ?op1 ?op2) => ?expected)
	 (check ($expt-number-fixnum	?op1 ?op2) => ?expected)
	 (check (expt			?op1 ?op2) => ?expected)))
      ((_ ?op1 ?op2 ?expected ?equal)
       (begin
	 (check ($expt-number-positive-fixnum ?op1 ?op2) (=> ?equal) ?expected)
	 (check ($expt-number-fixnum	?op1 ?op2) (=> ?equal) ?expected)
	 (check (expt			?op1 ?op2) (=> ?equal) ?expected)))
      ))

;;; --------------------------------------------------------------------
;;; fixnums

  (test		0	+1	0)
  (test		+1	+1	1)
  (test		-1	+1	-1)

  ;; even exponent
  (test		0	+10	0)
  (test		+1	+10	1)
  (test		-1	+10	1)
  (test		+12	+2	+144)
  (test		-12	+2	+144)

  ;; odd exponent
  (test		0	+11	0)
  (test		+1	+11	+1)
  (test		-1	+11	-1)
  (test		+12	+3	+1728)
  (test		-12	+3	-1728)

  ;; even exponent, result bignum
  (test		+123	+10	792594609605189126649)
  (test		-123	+10	792594609605189126649)

  ;; even exponent, result bignum
  (test		+123	+11	+97489136981438262577827)
  (test		-123	+11	-97489136981438262577827)

;;; --------------------------------------------------------------------
;;; bignums

  (test	SMALLEST-POSITIVE-BIGNUM	+1	SMALLEST-POSITIVE-BIGNUM)
  (test	SMALLEST-NEGATIVE-BIGNUM	+1	SMALLEST-NEGATIVE-BIGNUM)
  (test BN1				+1	BN1)
  (test BN2				+1	BN2)
  (test BN3				+1	BN3)
  (test BN4				+1	BN4)

  ;; even exponent
  (test	SMALLEST-POSITIVE-BIGNUM	+2	(square SMALLEST-POSITIVE-BIGNUM))
  (test	SMALLEST-NEGATIVE-BIGNUM	+2	(square SMALLEST-NEGATIVE-BIGNUM))
  (test BN1				+2	(square BN1))
  (test BN2				+2	(square BN2))
  (test BN3				+2	(square BN3))
  (test BN4				+2	(square BN4))

  ;; odd exponent
  (test	SMALLEST-POSITIVE-BIGNUM	+3	(cube SMALLEST-POSITIVE-BIGNUM))
  (test	SMALLEST-NEGATIVE-BIGNUM	+3	(cube SMALLEST-NEGATIVE-BIGNUM))
  (test BN1				+3	(cube BN1))
  (test BN2				+3	(cube BN2))
  (test BN3				+3	(cube BN3))
  (test BN4				+3	(cube BN4))

;;; --------------------------------------------------------------------
;;; ratnums

  (test		+2/3	+1	+2/3)
  (test		-2/3	+1	-2/3)

  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	+1	(/ 7 SMALLEST-POSITIVE-BIGNUM))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	+1	(/ 7 SMALLEST-NEGATIVE-BIGNUM))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	+1	(/ SMALLEST-POSITIVE-BIGNUM 7))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	+1	(/ SMALLEST-NEGATIVE-BIGNUM 7))

  ;; even exponent
  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	+2	(square (/ 7 SMALLEST-POSITIVE-BIGNUM)))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	+2	(square (/ 7 SMALLEST-NEGATIVE-BIGNUM)))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	+2	(square (/ SMALLEST-POSITIVE-BIGNUM 7)))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	+2	(square (/ SMALLEST-NEGATIVE-BIGNUM 7)))

  ;; odd exponent
  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	+3	(cube (/ 7 SMALLEST-POSITIVE-BIGNUM)))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	+3	(cube (/ 7 SMALLEST-NEGATIVE-BIGNUM)))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	+3	(cube (/ SMALLEST-POSITIVE-BIGNUM 7)))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	+3	(cube (/ SMALLEST-NEGATIVE-BIGNUM 7)))

;;; --------------------------------------------------------------------
;;; flonums

  (test		+0.0		+1	+0.0)
  (test		-0.0		+1	-0.0)
  (test		+1.0		+1	+1.0)
  (test		-1.0		+1	-1.0)
  (test		+nan.0		+1	+nan.0)
  (test		+inf.0		+1	+inf.0)
  (test		-inf.0		+1	-inf.0)
  (test		+123.456	+1	+123.456)
  (test		-123.456	+1	-123.456)

  ;; even exponent
  (test		+0.0		+2	+0.0)
  (test		-0.0		+2	+0.0)
  (test		+1.0		+2	+1.0)
  (test		-1.0		+2	+1.0)
  (test		+nan.0		+2	+nan.0)
  (test		+inf.0		+2	+inf.0)
  (test		-inf.0		+2	+inf.0)
  (test		+123.456	+2	(square +123.456))
  (test		-123.456	+2	(square -123.456))

  ;; odd exponent
  (test		+0.0		+3	+0.0)
  (test		-0.0		+3	-0.0)
  (test		+1.0		+3	+1.0)
  (test		-1.0		+3	-1.0)
  (test		+nan.0		+3	+nan.0)
  (test		+inf.0		+3	+inf.0)
  (test		-inf.0		+3	-inf.0)
  (test		+123.456	+3	(cube +123.456))
  (test		-123.456	+3	(cube -123.456))

;;; --------------------------------------------------------------------
;;; compnums

  (test		123+456i	+1	123+456i)
  (test		12.3+456i	+1	12.3+456i)
  (test		123+45.6i	+1	123+45.6i)
  (test		1/23+4/56i	+1	1/23+4/56i)

  (test		+nan.0+456i	+1	+nan.0+456i	inexact=?)
  (test		123+nan.0i	+1	+123+nan.0i	inexact=?)

  (test		+inf.0+456i	+1	+inf.0+456i	inexact=?)
  (test		123+inf.0i	+1	+123+inf.0i	inexact=?)

  (test		-inf.0+456i	+1	-inf.0+456i)
  (test		123-inf.0i	+1	123-inf.0i)

  (test (C 123 BN1)	+1	(C 123 BN1))
  (test (C BN1 456)	+1	(C BN1 456))
  (test (C 1.23 BN1)	+1	(C 1.23 BN1))
  (test (C BN1 4.56)	+1	(C BN1 4.56))
  (test (C 1/23 BN1)	+1	(C 1/23 BN1))
  (test (C BN1 4/56)	+1	(C BN1 4/56))

  (test (C 123 BN2)	+1	(C 123 BN2))
  (test (C BN2 456)	+1	(C BN2 456))
  (test (C 1.23 BN2)	+1	(C 1.23 BN2))
  (test (C BN2 4.56)	+1	(C BN2 4.56))
  (test (C 1/23 BN2)	+1	(C 1/23 BN2))
  (test (C BN2 4/56)	+1	(C BN2 4/56))

  (test (C 123 BN3)	+1	(C 123 BN3))
  (test (C BN3 456)	+1	(C BN3 456))
  (test (C 1.23 BN3)	+1	(C 1.23 BN3))
  (test (C BN3 4.56)	+1	(C BN3 4.56))
  (test (C 1/23 BN3)	+1	(C 1/23 BN3))
  (test (C BN3 4/56)	+1	(C BN3 4/56))

  (test (C 123 BN4)	+1	(C 123 BN4))
  (test (C BN4 456)	+1	(C BN4 456))
  (test (C 1.23 BN4)	+1	(C 1.23 BN4))
  (test (C BN4 4.56)	+1	(C BN4 4.56))
  (test (C 1/23 BN4)	+1	(C 1/23 BN4))
  (test (C BN4 4/56)	+1	(C BN4 4/56))

  ;; even exponent
  (test		123+456i	+2	(square 123+456i))
  (test		12.3+456i	+2	(square 12.3+456.0i))
  (test		123+45.6i	+2	(square 123.0+45.6i))
  (test		1/23+4/56i	+2	(square 1/23+4/56i))

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+456i	+2	+nan.0+nan.0i	inexact=?)
  (test		123+nan.0i	+2	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	+2	+inf.0+nan.0i	inexact=?)
  (test		123+inf.0i	+2	-inf.0+inf.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	+2	+inf.0-inf.0i	inexact=?)
  (test		123-inf.0i	+2	-inf.0-inf.0i	inexact=?)

  (test		(C 123 BN1)	+2	(square (C 123 BN1)))
  (test		(C BN1 456)	+2	(square (C BN1 456)))
  (test		(C 1.23 BN1)	+2	(square (C 1.23 BN1)))
  (test		(C BN1 4.56)	+2	(square (C BN1 4.56)))
  (test		(C 1/23 BN1)	+2	(square (C 1/23 BN1)))
  (test		(C BN1 4/56)	+2	(square (C BN1 4/56)))

  (test		(C 123 BN2)	+2	(square (C 123 BN2)))
  (test		(C BN2 456)	+2	(square (C BN2 456)))
  (test		(C 1.23 BN2)	+2	(square (C 1.23 BN2)))
  (test		(C BN2 4.56)	+2	(square (C BN2 4.56)))
  (test		(C 1/23 BN2)	+2	(square (C 1/23 BN2)))
  (test		(C BN2 4/56)	+2	(square (C BN2 4/56)))

  (test		(C 123 BN3)	+2	(square (C 123 BN3)))
  (test		(C BN3 456)	+2	(square (C BN3 456)))
  (test		(C 1.23 BN3)	+2	(square (C 1.23 BN3)))
  (test		(C BN3 4.56)	+2	(square (C BN3 4.56)))
  (test		(C 1/23 BN3)	+2	(square (C 1/23 BN3)))
  (test		(C BN3 4/56)	+2	(square (C BN3 4/56)))

  (test		(C 123 BN4)	+2	(square (C 123 BN4)))
  (test		(C BN4 456)	+2	(square (C BN4 456)))
  (test		(C 1.23 BN4)	+2	(square (C 1.23 BN4)))
  (test		(C BN4 4.56)	+2	(square (C BN4 4.56)))
  (test		(C 1/23 BN4)	+2	(square (C 1/23 BN4)))
  (test		(C BN4 4/56)	+2	(square (C BN4 4/56)))

  ;; even exponent
  (test		123+456i	+3	(cube 123+456i))
  (test		12.3+456i	+3	(cube 12.3+456.0i))
  (test		123+45.6i	+3	(cube 123.0+45.6i))
  (test		1/23+4/56i	+3	(cube 1/23+4/56i))

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+456i	+3	+nan.0+nan.0i	inexact=?)
  (test		123+nan.0i	+3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	+3	+inf.0+nan.0i	inexact=?)
  (test		123+inf.0i	+3	-inf.0-inf.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	+3	-inf.0+inf.0i	inexact=?)
  (test		123-inf.0i	+3	-inf.0+inf.0i	inexact=?)

  (test		(C 123 BN1)	+3	(cube (C 123 BN1)))
  (test		(C BN1 456)	+3	(cube (C BN1 456)))
  (test		(C 1.23 BN1)	+3	(cube (C 1.23 BN1)))
  (test		(C BN1 4.56)	+3	(cube (C BN1 4.56)))
  (test		(C 1/23 BN1)	+3	(cube (C 1/23 BN1)))
  (test		(C BN1 4/56)	+3	(cube (C BN1 4/56)))

  (test		(C 123 BN2)	+3	(cube (C 123 BN2)))
  (test		(C BN2 456)	+3	(cube (C BN2 456)))
  (test		(C 1.23 BN2)	+3	(cube (C 1.23 BN2)))
  (test		(C BN2 4.56)	+3	(cube (C BN2 4.56)))
  (test		(C 1/23 BN2)	+3	(cube (C 1/23 BN2)))
  (test		(C BN2 4/56)	+3	(cube (C BN2 4/56)))

  (test		(C 123 BN3)	+3	(cube (C 123 BN3)))
  (test		(C BN3 456)	+3	(cube (C BN3 456)))
  (test		(C 1.23 BN3)	+3	(cube (C 1.23 BN3)))
  (test		(C BN3 4.56)	+3	(cube (C BN3 4.56)))
  (test		(C 1/23 BN3)	+3	(cube (C 1/23 BN3)))
  (test		(C BN3 4/56)	+3	(cube (C BN3 4/56)))

  (test		(C 123 BN4)	+3	(cube (C 123 BN4)))
  (test		(C BN4 456)	+3	(cube (C BN4 456)))
  (test		(C 1.23 BN4)	+3	(cube (C 1.23 BN4)))
  (test		(C BN4 4.56)	+3	(cube (C BN4 4.56)))
  (test		(C 1/23 BN4)	+3	(cube (C 1/23 BN4)))
  (test		(C BN4 4/56)	+3	(cube (C BN4 4/56)))

;;; --------------------------------------------------------------------
;;; cflonums

  (test		+nan.0+nan.0i	+1	+nan.0+nan.0i	inexact=?)
  (test		+inf.0+inf.0i	+1	+inf.0+inf.0i	inexact=?)
  (test		-inf.0-inf.0i	+1	-inf.0-inf.0i)

  ;; even exponent
  (test		+inf.0+inf.0i	+2	+inf.0+inf.0i	inexact=?)
  (test		+nan.0+nan.0i	+2	+nan.0+nan.0i	inexact=?)
  (test		-inf.0-inf.0i	+2	-inf.0+inf.0i	inexact=?)

  ;; odd exponent
  (test		+nan.0+nan.0i	+3	+nan.0+nan.0i	inexact=?)
  (test		+inf.0+inf.0i	+3	-inf.0+inf.0i	inexact=?)
  (test		-inf.0-inf.0i	+3	+inf.0-inf.0i	inexact=?)

  #t)


(parametrise ((check-test-name	'negative-fixnum-exponent))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op1 ?op2 ?expected)
       (begin
	 (check ($expt-number-negative-fixnum ?op1 ?op2) => ?expected)
	 (check ($expt-number-fixnum	?op1 ?op2) => ?expected)
	 (check (expt			?op1 ?op2) => ?expected)))
      ((_ ?op1 ?op2 ?expected ?equal)
       (begin
	 (check ($expt-number-negative-fixnum ?op1 ?op2) (=> ?equal) ?expected)
	 (check ($expt-number-fixnum	?op1 ?op2) (=> ?equal) ?expected)
	 (check (expt			?op1 ?op2) (=> ?equal) ?expected)))
      ))

;;; --------------------------------------------------------------------
;;; fixnums

  (test		0	-1	0)
  (test		+1	-1	1)
  (test		-1	-1	-1)

  ;; even exponent
  (test		0	-10	0)
  (test		+1	-10	1)
  (test		-1	-10	1)
  (test		+12	-2	+1/144)
  (test		-12	-2	+1/144)

  ;; odd exponent
  (test		0	-11	0)
  (test		+1	-11	+1)
  (test		-1	-11	-1)
  (test		+12	-3	+1/1728)
  (test		-12	-3	-1/1728)

  ;; even exponent, result bignum
  (test		+123	-10	1/792594609605189126649)
  (test		-123	-10	1/792594609605189126649)

  ;; even exponent, result bignum
  (test		+123	-11	+1/97489136981438262577827)
  (test		-123	-11	-1/97489136981438262577827)

;;; --------------------------------------------------------------------
;;; bignums

  (test	SMALLEST-POSITIVE-BIGNUM	-1	(/ SMALLEST-POSITIVE-BIGNUM))
  (test	SMALLEST-NEGATIVE-BIGNUM	-1	(/ SMALLEST-NEGATIVE-BIGNUM))
  (test BN1				-1	(/ BN1))
  (test BN2				-1	(/ BN2))
  (test BN3				-1	(/ BN3))
  (test BN4				-1	(/ BN4))

  ;; even exponent
  (test	SMALLEST-POSITIVE-BIGNUM	-2	(/ (square SMALLEST-POSITIVE-BIGNUM)))
  (test	SMALLEST-NEGATIVE-BIGNUM	-2	(/ (square SMALLEST-NEGATIVE-BIGNUM)))
  (test BN1				-2	(/ (square BN1)))
  (test BN2				-2	(/ (square BN2)))
  (test BN3				-2	(/ (square BN3)))
  (test BN4				-2	(/ (square BN4)))

  ;; odd exponent
  (test	SMALLEST-POSITIVE-BIGNUM	-3	(/ (cube SMALLEST-POSITIVE-BIGNUM)))
  (test	SMALLEST-NEGATIVE-BIGNUM	-3	(/ (cube SMALLEST-NEGATIVE-BIGNUM)))
  (test BN1				-3	(/ (cube BN1)))
  (test BN2				-3	(/ (cube BN2)))
  (test BN3				-3	(/ (cube BN3)))
  (test BN4				-3	(/ (cube BN4)))

;;; --------------------------------------------------------------------
;;; ratnums

  (test		+2/3	-1	+3/2)
  (test		-2/3	-1	-3/2)

  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-1	(/ SMALLEST-POSITIVE-BIGNUM 7))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-1	(/ SMALLEST-NEGATIVE-BIGNUM 7))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	-1	(/ 7 SMALLEST-POSITIVE-BIGNUM))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-1	(/ 7 SMALLEST-NEGATIVE-BIGNUM))

  ;; even exponent
  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-2	(square (/ SMALLEST-POSITIVE-BIGNUM 7)))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-2	(square (/ SMALLEST-NEGATIVE-BIGNUM 7)))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	-2	(square (/ 7 SMALLEST-POSITIVE-BIGNUM)))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-2	(square (/ 7 SMALLEST-NEGATIVE-BIGNUM)))

  ;; odd exponent
  (test	(/ 7 SMALLEST-POSITIVE-BIGNUM)	-3	(cube (/ SMALLEST-POSITIVE-BIGNUM 7)))
  (test	(/ 7 SMALLEST-NEGATIVE-BIGNUM)	-3	(cube (/ SMALLEST-NEGATIVE-BIGNUM 7)))
  (test	(/ SMALLEST-POSITIVE-BIGNUM 7)	-3	(cube (/ 7 SMALLEST-POSITIVE-BIGNUM)))
  (test	(/ SMALLEST-NEGATIVE-BIGNUM 7)	-3	(cube (/ 7 SMALLEST-NEGATIVE-BIGNUM)))

;;; --------------------------------------------------------------------
;;; flonums

  (test		+0.0		-1	+inf.0)
  (test		-0.0		-1	-inf.0)
  (test		+1.0		-1	+1.0)
  (test		-1.0		-1	-1.0)
  (test		+nan.0		-1	+nan.0)
  (test		+inf.0		-1	+0.0)
  (test		-inf.0		-1	-0.0)
  (test		+123.456	-1	(/ +123.456))
  (test		-123.456	-1	(/ -123.456))

  ;; even exponent
  (test		+0.0		-2	+inf.0)
  (test		-0.0		-2	+inf.0)
  (test		+1.0		-2	+1.0)
  (test		-1.0		-2	+1.0)
  (test		+nan.0		-2	+nan.0)
  (test		+inf.0		-2	+0.0)
  (test		-inf.0		-2	+0.0)
  (test		+123.456	-2	(/ (square +123.456)))
  (test		-123.456	-2	(/ (square -123.456)))

  ;; odd exponent
  (test		+0.0		-3	+inf.0)
  (test		-0.0		-3	-inf.0)
  (test		+1.0		-3	+1.0)
  (test		-1.0		-3	-1.0)
  (test		+nan.0		-3	+nan.0)
  (test		+inf.0		-3	+0.0)
  (test		-inf.0		-3	-0.0)
  (test		+123.456	-3	(/ (cube +123.456)))
  (test		-123.456	-3	(/ (cube -123.456)))

;;; --------------------------------------------------------------------
;;; compnums

  (test		123+456i	-1	(/ 123+456i))
  (test		12.3+456i	-1	(/ 12.3+456i))
  (test		123+45.6i	-1	(/ 123+45.6i))
  (test		1/23+4/56i	-1	(/ 1/23+4/56i))

  (test		+nan.0+456i	-1	(/ +nan.0+456i)		inexact=?)
  (test		123+nan.0i	-1	(/ +123+nan.0i)		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	-1	+nan.0+0.0i		inexact=?)
  (test		123+inf.0i	-1	+0.0+nan.0i		inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	-1	+nan.0-0.0i		inexact=?)
  (test		123-inf.0i	-1	+0.0+nan.0i		inexact=?)

  (test (C 123 BN1)	-1	(/ (C 123 BN1)))
  (test (C BN1 456)	-1	(/ (C BN1 456)))
  (test (C 1.23 BN1)	-1	(/ (C 1.23 BN1)))
  (test (C BN1 4.56)	-1	(/ (C BN1 4.56)))
  (test (C 1/23 BN1)	-1	(/ (C 1/23 BN1)))
  (test (C BN1 4/56)	-1	(/ (C BN1 4/56)))

  (test (C 123 BN2)	-1	(/ (C 123 BN2)))
  (test (C BN2 456)	-1	(/ (C BN2 456)))
  (test (C 1.23 BN2)	-1	(/ (C 1.23 BN2)))
  (test (C BN2 4.56)	-1	(/ (C BN2 4.56)))
  (test (C 1/23 BN2)	-1	(/ (C 1/23 BN2)))
  (test (C BN2 4/56)	-1	(/ (C BN2 4/56)))

  (test (C 123 BN3)	-1	(/ (C 123 BN3)))
  (test (C BN3 456)	-1	(/ (C BN3 456)))
  (test (C 1.23 BN3)	-1	(/ (C 1.23 BN3)))
  (test (C BN3 4.56)	-1	(/ (C BN3 4.56)))
  (test (C 1/23 BN3)	-1	(/ (C 1/23 BN3)))
  (test (C BN3 4/56)	-1	(/ (C BN3 4/56)))

  (test (C 123 BN4)	-1	(/ (C 123 BN4)))
  (test (C BN4 456)	-1	(/ (C BN4 456)))
  (test (C 1.23 BN4)	-1	(/ (C 1.23 BN4)))
  (test (C BN4 4.56)	-1	(/ (C BN4 4.56)))
  (test (C 1/23 BN4)	-1	(/ (C 1/23 BN4)))
  (test (C BN4 4/56)	-1	(/ (C BN4 4/56)))

  ;; even exponent
  (test		123+456i	-2	(/ (square 123+456i)))
  (test		12.3+456i	-2	(/ (square 12.3+456.0i)))
  (test		123+45.6i	-2	(/ (square 123.0+45.6i)))
  (test		1/23+4/56i	-2	(/ (square 1/23+4/56i)))

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+456i	-2	(/ +nan.0+nan.0i)	inexact=?)
  (test		123+nan.0i	-2	(/ +nan.0+nan.0i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	-2	(/ +inf.0+nan.0i)	inexact=?)
  (test		123+inf.0i	-2	(/ -inf.0+inf.0i)	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	-2	(/ +inf.0-inf.0i)	inexact=?)
  (test		123-inf.0i	-2	(/ -inf.0-inf.0i)	inexact=?)

  (test		(C 123 BN1)	-2	(/ (square (C 123 BN1))))
  (test		(C BN1 456)	-2	(/ (square (C BN1 456))))
  (test		(C 1.23 BN1)	-2	(/ (square (C 1.23 BN1))))
  (test		(C BN1 4.56)	-2	(/ (square (C BN1 4.56))))
  (test		(C 1/23 BN1)	-2	(/ (square (C 1/23 BN1))))
  (test		(C BN1 4/56)	-2	(/ (square (C BN1 4/56))))

  (test		(C 123 BN2)	-2	(/ (square (C 123 BN2))))
  (test		(C BN2 456)	-2	(/ (square (C BN2 456))))
  (test		(C 1.23 BN2)	-2	(/ (square (C 1.23 BN2))))
  (test		(C BN2 4.56)	-2	(/ (square (C BN2 4.56))))
  (test		(C 1/23 BN2)	-2	(/ (square (C 1/23 BN2))))
  (test		(C BN2 4/56)	-2	(/ (square (C BN2 4/56))))

  (test		(C 123 BN3)	-2	(/ (square (C 123 BN3))))
  (test		(C BN3 456)	-2	(/ (square (C BN3 456))))
  (test		(C 1.23 BN3)	-2	(/ (square (C 1.23 BN3))))
  (test		(C BN3 4.56)	-2	(/ (square (C BN3 4.56))))
  (test		(C 1/23 BN3)	-2	(/ (square (C 1/23 BN3))))
  (test		(C BN3 4/56)	-2	(/ (square (C BN3 4/56))))

  (test		(C 123 BN4)	-2	(/ (square (C 123 BN4))))
  (test		(C BN4 456)	-2	(/ (square (C BN4 456))))
  (test		(C 1.23 BN4)	-2	(/ (square (C 1.23 BN4))))
  (test		(C BN4 4.56)	-2	(/ (square (C BN4 4.56))))
  (test		(C 1/23 BN4)	-2	(/ (square (C 1/23 BN4))))
  (test		(C BN4 4/56)	-2	(/ (square (C BN4 4/56))))

  ;; even exponent
  (test		123+456i	-3	(/ (cube 123+456i)))
  (test		12.3+456i	-3	(/ (cube 12.3+456.0i)))
  (test		123+45.6i	-3	(/ (cube 123.0+45.6i)))
  (test		1/23+4/56i	-3	(/ (cube 1/23+4/56i)))

		;these are whatever comes out of (exp (* M (log N)))
  (test		+nan.0+456i	-3	+nan.0+nan.0i	inexact=?)
  (test		123+nan.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		+inf.0+456i	-3	+nan.0+nan.0i	inexact=?)
  (test		123+inf.0i	-3	+nan.0+nan.0i	inexact=?)

		;these are whatever comes out of (exp (* M (log N)))
  (test		-inf.0+456i	-3	+nan.0+nan.0i	inexact=?)
  (test		123-inf.0i	-3	-nan.0+nan.0i	inexact=?)

  (test		(C 123 BN1)	-3	(/ (cube (C 123 BN1))))
  (test		(C BN1 456)	-3	(/ (cube (C BN1 456))))
  (test		(C 1.23 BN1)	-3	(/ (cube (C 1.23 BN1))))
  (test		(C BN1 4.56)	-3	(/ (cube (C BN1 4.56))))
  (test		(C 1/23 BN1)	-3	(/ (cube (C 1/23 BN1))))
  (test		(C BN1 4/56)	-3	(/ (cube (C BN1 4/56))))

  (test		(C 123 BN2)	-3	(/ (cube (C 123 BN2))))
  (test		(C BN2 456)	-3	(/ (cube (C BN2 456))))
  (test		(C 1.23 BN2)	-3	(/ (cube (C 1.23 BN2))))
  (test		(C BN2 4.56)	-3	(/ (cube (C BN2 4.56))))
  (test		(C 1/23 BN2)	-3	(/ (cube (C 1/23 BN2))))
  (test		(C BN2 4/56)	-3	(/ (cube (C BN2 4/56))))

  (test		(C 123 BN3)	-3	(/ (cube (C 123 BN3))))
  (test		(C BN3 456)	-3	(/ (cube (C BN3 456))))
  (test		(C 1.23 BN3)	-3	(/ (cube (C 1.23 BN3))))
  (test		(C BN3 4.56)	-3	(/ (cube (C BN3 4.56))))
  (test		(C 1/23 BN3)	-3	(/ (cube (C 1/23 BN3))))
  (test		(C BN3 4/56)	-3	(/ (cube (C BN3 4/56))))

  (test		(C 123 BN4)	-3	(/ (cube (C 123 BN4))))
  (test		(C BN4 456)	-3	(/ (cube (C BN4 456))))
  (test		(C 1.23 BN4)	-3	(/ (cube (C 1.23 BN4))))
  (test		(C BN4 4.56)	-3	(/ (cube (C BN4 4.56))))
  (test		(C 1/23 BN4)	-3	(/ (cube (C 1/23 BN4))))
  (test		(C BN4 4/56)	-3	(/ (cube (C BN4 4/56))))

;;; --------------------------------------------------------------------
;;; cflonums

  (test		+nan.0+nan.0i	-1	+nan.0+nan.0i	inexact=?)
  (test		+inf.0+inf.0i	-1	+nan.0+nan.0i	inexact=?)
  (test		-inf.0-inf.0i	-1	+nan.0+nan.0i	inexact=?)

  ;; even exponent
  (test		+nan.0+nan.0i	-2	+nan.0+nan.0i	inexact=?)
  (test		+inf.0+inf.0i	-2	+nan.0+nan.0i	inexact=?)
  (test		-inf.0-inf.0i	-2	+nan.0+nan.0i	inexact=?)

  ;; odd exponent
  (test		+nan.0+nan.0i	-3	+nan.0+nan.0i	inexact=?)
  (test		+inf.0+inf.0i	-3	-nan.0+nan.0i	inexact=?)
  (test		-inf.0-inf.0i	-3	+nan.0-nan.0i	inexact=?)

  #t)




  ;; (let-syntax ((test (make-test expt $expt-fixnum-bignum)))
  ;;   (test FX1 BN1 1/536870912)
  ;;   (test FX2 BN1 -1/536870912)
  ;;   (test FX3 BN1 536870911/536870912)
  ;;   (test FX4 BN1 -1)
  ;;   (test FX1 BN2 1/536871011)
  ;;   (test FX2 BN2 -1/536871011)
  ;;   (test FX3 BN2 536870911/536871011)
  ;;   (test FX4 BN2 -536870912/536871011)
  ;;   (test FX1 BN3 -1/536870913)
  ;;   (test FX2 BN3 1/536870913)
  ;;   (test FX3 BN3 -536870911/536870913)
  ;;   (test FX4 BN3 536870912/536870913)
  ;;   (test FX1 BN4 -1/536871012)
  ;;   (test FX2 BN4 1/536871012)
  ;;   (test FX3 BN4 -536870911/536871012)
  ;;   (test FX4 BN4 134217728/134217753)
  ;;   #f)

  ;; (let-syntax ((test (make-test expt $expt-fixnum-ratnum)))
  ;;   (test FX1 RN01 123)
  ;;   (test FX2 RN01 -123)
  ;;   (test FX3 RN01 66035122053)
  ;;   (test FX4 RN01 -66035122176)
  ;;   (test FX1 RN02 -123)
  ;;   (test FX2 RN02 123)
  ;;   (test FX3 RN02 -66035122053)
  ;;   (test FX4 RN02 66035122176)
  ;;   (test FX1 RN03 -123)
  ;;   (test FX2 RN03 123)
  ;;   (test FX3 RN03 -66035122053)
  ;;   (test FX4 RN03 66035122176)
  ;;   (test FX1 RN04 -123/536870912)
  ;;   (test FX2 RN04 123/536870912)
  ;;   (test FX3 RN04 -66035122053/536870912)
  ;;   (test FX4 RN04 123)
  ;;   #f)

  ;; (let-syntax ((test (make-flonum-test expt $expt-fixnum-flonum)))
  ;;   (test FX1 FL1 +inf.0)
  ;;   (test FX2 FL1 -inf.0)
  ;;   (test FX3 FL1 +inf.0)
  ;;   (test FX4 FL1 -inf.0)
  ;;   (test FX1 FL2 -inf.0)
  ;;   (test FX2 FL2 +inf.0)
  ;;   (test FX3 FL2 -inf.0)
  ;;   (test FX4 FL2 +inf.0)
  ;;   (test FX1 FL3 0.4710315591144606)
  ;;   (test FX2 FL3 -0.4710315591144606)
  ;;   (test FX3 FL3 252883142.25153083)
  ;;   (test FX4 FL3 -252883142.72256237)
  ;;   (test FX1 FL4 -0.4710315591144606)
  ;;   (test FX2 FL4 0.4710315591144606)
  ;;   (test FX3 FL4 -252883142.25153083)
  ;;   (test FX4 FL4 252883142.72256237)
  ;;   #f)

  ;; (let-syntax ((test (make-inexact-test expt $expt-fixnum-cflonum)))
  ;;   (test FX1 CFL01 +nan.0+nan.0i)
  ;;   (test FX2 CFL01 +nan.0+nan.0i)
  ;;   (test FX3 CFL01 +nan.0+nan.0i)
  ;;   (test FX4 CFL01 +nan.0+nan.0i)
  ;;   (test FX1 CFL02 +nan.0+nan.0i)
  ;;   (test FX2 CFL02 +nan.0+nan.0i)
  ;;   (test FX3 CFL02 +nan.0+nan.0i)
  ;;   (test FX4 CFL02 +nan.0+nan.0i)
  ;;   (test FX1 CFL03 +nan.0+nan.0i)
  ;;   (test FX2 CFL03 +nan.0+nan.0i)
  ;;   (test FX3 CFL03 +nan.0+nan.0i)
  ;;   (test FX4 CFL03 +nan.0+nan.0i)
  ;;   (test FX1 CFL04 +nan.0+nan.0i)
  ;;   (test FX2 CFL04 +nan.0+nan.0i)
  ;;   (test FX3 CFL04 +nan.0+nan.0i)
  ;;   (test FX4 CFL04 +nan.0+nan.0i)
  ;;   (test FX1 CFL05 -0.8333333333333333+0.0i)
  ;;   (test FX2 CFL05 0.8333333333333333-0.0i)
  ;;   (test FX3 CFL05 -447392425.8333333+0.0i)
  ;;   (test FX4 CFL05 447392426.6666666-0.0i)
  ;;   (test FX1 CFL06 -0.8333333333333333-0.0i)
  ;;   (test FX2 CFL06 0.8333333333333333+0.0i)
  ;;   (test FX3 CFL06 -447392425.8333333-0.0i)
  ;;   (test FX4 CFL06 447392426.6666666+0.0i)
  ;;   (test FX1 CFL07 0.0+0.8333333333333333i)
  ;;   (test FX2 CFL07 -0.0-0.8333333333333333i)
  ;;   (test FX3 CFL07 0.0+447392425.8333333i)
  ;;   (test FX4 CFL07 -0.0-447392426.6666666i)
  ;;   (test FX1 CFL08 -0.0+0.8333333333333333i)
  ;;   (test FX2 CFL08 0.0-0.8333333333333333i)
  ;;   (test FX3 CFL08 -0.0+447392425.8333333i)
  ;;   (test FX4 CFL08 0.0-447392426.6666666i)
  ;;   (test FX1 CFL09 -0.0+nan.0i)
  ;;   (test FX1 CFL10 -0.0+nan.0i)
  ;;   (test FX1 CFL11 +nan.0+0.0i)
  ;;   (test FX1 CFL12 +nan.0+0.0i)
  ;;   (test FX1 CFL13 +nan.0+nan.0i)
  ;;   (test FX1 CFL14 +nan.0+nan.0i)
  ;;   (test FX1 CFL15 +nan.0+nan.0i)
  ;;   (test FX1 CFL16 +nan.0+nan.0i)
  ;;   #f)

  ;; (let-syntax ((test (make-test expt $expt-fixnum-compnum)))
  ;;   (test 1 10+20i 1/50-1/25i)
  ;;   (test 1 1+20.0i 0.0024937655860349127-0.04987531172069826i)
  ;;   (test 1 10.0+2i 0.09615384615384616-0.019230769230769232i)
  ;;   (test 1 1/2+20i 2/1601-80/1601i)
  ;;   (test 1 10+2/3i 45/452-3/452i)
  ;;   (test 1 (C BN1 20) 33554432/18014398509482009-5/72057594037928036i)
  ;;   (test 1 (C 10 BN1) 5/144115188075855922-134217728/72057594037927961i)
  ;;   #f)


#;(parametrise ((check-test-name	'bignums))

  (let-syntax ((test (make-test expt $expt-bignum-fixnum)))
    (test BN1 FX1 536870912)
    (test BN2 FX1 536871011)
    (test BN3 FX1 -536870913)
    (test BN4 FX1 -536871012)
    (test BN1 FX2 -536870912)
    (test BN2 FX2 -536871011)
    (test BN3 FX2 536870913)
    (test BN4 FX2 536871012)
    (test BN1 FX3 536870912/536870911)
    (test BN2 FX3 536871011/536870911)
    (test BN3 FX3 -536870913/536870911)
    (test BN4 FX3 -536871012/536870911)
    (test BN1 FX4 -1)
    (test BN2 FX4 -536871011/536870912)
    (test BN3 FX4 536870913/536870912)
    (test BN4 FX4 134217753/134217728)
    #f)

  (let-syntax ((test (make-test expt $expt-bignum-bignum)))
    (test BN1 BN1 1)
    (test BN2 BN1 536871011/536870912)
    (test BN3 BN1 -536870913/536870912)
    (test BN4 BN1 -134217753/134217728)
    (test BN1 BN2 536870912/536871011)
    (test BN2 BN2 1)
    (test BN3 BN2 -536870913/536871011)
    (test BN4 BN2 -536871012/536871011)
    (test BN1 BN3 -536870912/536870913)
    (test BN2 BN3 -536871011/536870913)
    (test BN3 BN3 1)
    (test BN4 BN3 178957004/178956971)
    (test BN1 BN4 -134217728/134217753)
    (test BN2 BN4 -536871011/536871012)
    (test BN3 BN4 178956971/178957004)
    (test BN4 BN4 1)
    #f)

  (let-syntax ((test (make-test expt $expt-bignum-ratnum)))
    (test BN1 RN01 66035122176)
    (test BN2 RN01 66035134353)
    (test BN3 RN01 -66035122299)
    (test BN4 RN01 -66035134476)
    (test BN1 RN02 -66035122176)
    (test BN2 RN02 -66035134353)
    (test BN3 RN02 66035122299)
    (test BN4 RN02 66035134476)
    (test BN1 RN03 -66035122176)
    (test BN2 RN03 -66035134353)
    (test BN3 RN03 66035122299)
    (test BN4 RN03 66035134476)
    (test BN1 RN04 -123)
    (test BN2 RN04 -66035134353/536870912)
    (test BN3 RN04 66035122299/536870912)
    (test BN4 RN04 16508783619/134217728)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-bignum-flonum)))
    (test BN1 FL1 +inf.0)
    (test BN2 FL1 +inf.0)
    (test BN3 FL1 -inf.0)
    (test BN4 FL1 -inf.0)
    (test BN1 FL2 -inf.0)
    (test BN2 FL2 -inf.0)
    (test BN3 FL2 +inf.0)
    (test BN4 FL2 +inf.0)
    (test BN1 FL3 252883142.72256237)
    (test BN2 FL3 252883189.35468674)
    (test BN3 FL3 -252883143.19359395)
    (test BN4 FL3 -252883189.82571828)
    (test BN1 FL4 -252883142.72256237)
    (test BN2 FL4 -252883189.35468674)
    (test BN3 FL4 252883143.19359395)
    (test BN4 FL4 252883189.82571828)
    #f)

  (let-syntax ((test (make-cflonum-test expt $expt-bignum-cflonum)))
    (test BN1 CFL01 +nan.0+nan.0i)
    (test BN2 CFL01 +nan.0+nan.0i)
    (test BN3 CFL01 +nan.0+nan.0i)
    (test BN4 CFL01 +nan.0+nan.0i)
    (test BN1 CFL02 +nan.0+nan.0i)
    (test BN2 CFL02 +nan.0+nan.0i)
    (test BN3 CFL02 +nan.0+nan.0i)
    (test BN4 CFL02 +nan.0+nan.0i)
    (test BN1 CFL03 +nan.0+nan.0i)
    (test BN2 CFL03 +nan.0+nan.0i)
    (test BN3 CFL03 +nan.0+nan.0i)
    (test BN4 CFL03 +nan.0+nan.0i)
    (test BN1 CFL04 +nan.0+nan.0i)
    (test BN2 CFL04 +nan.0+nan.0i)
    (test BN3 CFL04 +nan.0+nan.0i)
    (test BN4 CFL04 +nan.0+nan.0i)
    (test BN1 CFL05 -447392426.6666666+0.0i)
    (test BN2 CFL05 -447392509.1666666+0.0i)
    (test BN3 CFL05 447392427.5-0.0i)
    (test BN4 CFL05 447392510.0-0.0i)
    (test BN1 CFL06 -447392426.6666666-0.0i)
    (test BN2 CFL06 -447392509.1666666-0.0i)
    (test BN3 CFL06 447392427.5+0.0i)
    (test BN4 CFL06 447392510.0+0.0i)
    (test BN1 CFL07 0.0+447392426.6666666i)
    (test BN2 CFL07 0.0+447392509.1666666i)
    (test BN3 CFL07 -0.0-447392427.5i)
    (test BN4 CFL07 -0.0-447392510.0i)
    (test BN1 CFL08 -0.0+447392426.6666666i)
    (test BN2 CFL08 -0.0+447392509.1666666i)
    (test BN3 CFL08 0.0-447392427.5i)
    (test BN4 CFL08 0.0-447392510.0i)
    (test BN1 CFL09 -0.0+nan.0i)
    (test BN1 CFL10 -0.0+nan.0i)
    (test BN1 CFL11 +nan.0+0.0i)
    (test BN1 CFL12 +nan.0+0.0i)
    (test BN1 CFL13 +nan.0+nan.0i)
    (test BN1 CFL14 +nan.0+nan.0i)
    (test BN1 CFL15 +nan.0+nan.0i)
    (test BN1 CFL16 +nan.0+nan.0i)
    #f)

  (let-syntax ((test (make-test expt $expt-bignum-compnum)))
    (test BN1 10+20i 268435456/25-536870912/25i)
    (test BN1 1+20.0i 1338830.204488778-26776604.089775562i)
    (test BN1 10.0+2i 51622203.07692308-10324440.615384616i)
    (test BN1 1/2+20i 1073741824/1601-42949672960/1601i)
    (test BN1 10+2/3i 6039797760/113-402653184/113i)
    (test BN1 (C BN2 20) 288230429301932032/288230482452162521-10737418240/288230482452162521i)
    (test BN1 (C 10 BN2) 5368709120/288230482452162221-288230429301932032/288230482452162221i)
    #f)

  #t)


#;(parametrise ((check-test-name	'ratnums))

  (let-syntax ((test (make-test expt $expt-ratnum-fixnum)))
    (test 1/2 10 1/20)
    (test 1/2 (greatest-fixnum) 1/1073741822)
    (test 1/2 (least-fixnum) -1/1073741824)
    #f)

  (let-syntax ((test (make-test expt $expt-ratnum-bignum)))
    (test 1/2 BN1 1/1073741824)
    (test 1/2 BN2 1/1073742022)
    (test 1/2 BN3 -1/1073741826)
    (test 1/2 BN4 -1/1073742024)
    (test -1/2 BN1 -1/1073741824)
    (test -1/2 BN2 -1/1073742022)
    (test -1/2 BN3 1/1073741826)
    (test -1/2 BN4 1/1073742024)
    #f)

  (let-syntax ((test (make-test expt $expt-ratnum-ratnum)))
    (test 1/2 3/4 2/3)
    (test -1/2 3/4 -2/3)
    (test -1/2 -3/4 2/3)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-ratnum-flonum)))
    (test 1/2 3.4 0.14705882352941177)
    (test -1/2 3.4 -0.14705882352941177)
    (test RN01 FL1 +inf.0)
    (test RN02 FL1 -inf.0)
    (test RN03 FL1 -inf.0)
    (test RN04 FL1 -inf.0)
    (test RN01 FL2 -inf.0)
    (test RN02 FL2 +inf.0)
    (test RN03 FL2 +inf.0)
    (test RN04 FL2 +inf.0)
    (test RN01 FL3 0.0038295248708492737)
    (test RN02 FL3 -0.0038295248708492737)
    (test RN03 FL3 -0.0038295248708492737)
    (test RN04 FL3 -2055960.5099395318)
    (test RN01 FL4 -0.0038295248708492737)
    (test RN02 FL4 0.0038295248708492737)
    (test RN03 FL4 0.0038295248708492737)
    (test RN04 FL4 2055960.5099395318)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-ratnum-cflonum)))
    (test RN01 CFL01 +nan.0+nan.0i)
    (test RN02 CFL01 +nan.0+nan.0i)
    (test RN03 CFL01 +nan.0+nan.0i)
    (test RN04 CFL01 +nan.0+nan.0i)
    (test RN01 CFL02 +nan.0+nan.0i)
    (test RN02 CFL02 +nan.0+nan.0i)
    (test RN03 CFL02 +nan.0+nan.0i)
    (test RN04 CFL02 +nan.0+nan.0i)
    (test RN01 CFL03 +nan.0+nan.0i)
    (test RN02 CFL03 +nan.0+nan.0i)
    (test RN03 CFL03 +nan.0+nan.0i)
    (test RN04 CFL03 +nan.0+nan.0i)
    (test RN01 CFL04 +nan.0+nan.0i)
    (test RN02 CFL04 +nan.0+nan.0i)
    (test RN03 CFL04 +nan.0+nan.0i)
    (test RN04 CFL04 +nan.0+nan.0i)
    (test RN01 CFL05 -0.006775067750677508+0.0i)
    (test RN02 CFL05 0.006775067750677508-0.0i)
    (test RN03 CFL05 0.006775067750677508-0.0i)
    (test RN04 CFL05 3637336.8021680224-0.0i)
    (test RN01 CFL06 -0.006775067750677508-0.0i)
    (test RN02 CFL06 0.006775067750677508+0.0i)
    (test RN03 CFL06 0.006775067750677508+0.0i)
    (test RN04 CFL06 3637336.8021680224+0.0i)
    (test RN01 CFL07 0.0+0.006775067750677508i)
    (test RN02 CFL07 -0.0-0.006775067750677508i)
    (test RN03 CFL07 -0.0-0.006775067750677508i)
    (test RN04 CFL07 -0.0-3637336.8021680224i)
    (test RN01 CFL08 -0.0+0.006775067750677508i)
    (test RN02 CFL08 0.0-0.006775067750677508i)
    (test RN03 CFL08 0.0-0.006775067750677508i)
    (test RN04 CFL08 0.0-3637336.8021680224i)
    (test RN01 CFL09 -0.0+nan.0i)
    (test RN01 CFL10 -0.0+nan.0i)
    (test RN01 CFL11 +nan.0+0.0i)
    (test RN01 CFL12 +nan.0+0.0i)
    (test RN01 CFL13 +nan.0+nan.0i)
    (test RN01 CFL14 +nan.0+nan.0i)
    (test RN01 CFL15 +nan.0+nan.0i)
    (test RN01 CFL16 +nan.0+nan.0i)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-ratnum-compnum)))
    (test RN01 10+20i 1/6150-1/3075i)
    (test RN01 1+20.0i 2.027451695963344e-5-4.054903391926688e-4i)
    (test RN01 10.0+2i 7.817385866166355e-4-1.563477173233271e-4i)
    (test RN01 1/2+20i 2/196923-80/196923i)
    (test RN01 10+2/3i 15/18532-1/18532i)
    (test RN01 (C RN02 20) -1/6051601-2460/6051601i)
    (test RN01 (C 10 RN02) 1230/1512901+1/1512901i)
    #f)

  #t)


#;(parametrise ((check-test-name	'flonums))

  (let-syntax ((test (make-inexact-test expt $expt-flonum-fixnum)))
    (test FL1 FX1 0.0)
    (test FL2 FX1 -0.0)
    (test FL3 FX1 2.123)
    (test FL4 FX1 -2.123)
    (test FL1 FX2 -0.0)
    (test FL2 FX2 0.0)
    (test FL3 FX2 -2.123)
    (test FL4 FX2 2.123)
    (test FL1 FX3 0.0)
    (test FL2 FX3 -0.0)
    (test FL3 FX3 3.954395659182958e-9)
    (test FL4 FX3 -3.954395659182958e-9)
    (test FL1 FX4 -0.0)
    (test FL2 FX4 0.0)
    (test FL3 FX4 -3.954395651817322e-9)
    (test FL4 FX4 3.954395651817322e-9)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-flonum-bignum)))
    (test FL1 BN1 0.0)
    (test FL2 BN1 -0.0)
    (test FL3 BN1 3.954395651817322e-9)
    (test FL4 BN1 -3.954395651817322e-9)
    (test FL1 BN2 0.0)
    (test FL2 BN2 -0.0)
    (test FL3 BN2 3.954394922619505e-9)
    (test FL4 BN2 -3.954394922619505e-9)
    (test FL1 BN3 -0.0)
    (test FL2 BN3 0.0)
    (test FL3 BN3 -3.954395644451687e-9)
    (test FL4 BN3 3.954395644451687e-9)
    (test FL1 BN4 -0.0)
    (test FL2 BN4 0.0)
    (test FL3 BN4 -3.954394915253872e-9)
    (test FL4 BN4 3.954394915253872e-9)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-flonum-ratnum)))
    (test FL1 RN01 0.0)
    (test FL2 RN01 -0.0)
    (test FL3 RN01 261.129)
    (test FL4 RN01 -261.129)
    (test FL1 RN02 -0.0)
    (test FL2 RN02 0.0)
    (test FL3 RN02 -261.129)
    (test FL4 RN02 261.129)
    (test FL1 RN03 -0.0)
    (test FL2 RN03 0.0)
    (test FL3 RN03 -261.129)
    (test FL4 RN03 261.129)
    (test FL1 RN04 -0.0)
    (test FL2 RN04 0.0)
    (test FL3 RN04 -4.863906651735306e-7)
    (test FL4 RN04 4.863906651735306e-7)
    #f)

  (let-syntax ((test (make-flonum-test expt $expt-flonum-flonum)))
    (test FL1 FL1 +nan.0)
    (test FL2 FL1 +nan.0)
    (test FL3 FL1 +inf.0)
    (test FL4 FL1 -inf.0)
    (test FL1 FL2 +nan.0)
    (test FL2 FL2 +nan.0)
    (test FL3 FL2 -inf.0)
    (test FL4 FL2 +inf.0)
    (test FL1 FL3 0.0)
    (test FL2 FL3 -0.0)
    (test FL3 FL3 1.0)
    (test FL4 FL3 -1.0)
    (test FL1 FL4 -0.0)
    (test FL2 FL4 0.0)
    (test FL3 FL4 -1.0)
    (test FL4 FL4 1.0)
    #f)

  (let-syntax ((test (make-cflonum-test expt $expt-flonum-cflonum)))
    (test FL1 CFL01 +nan.0+nan.0i)
    (test FL2 CFL01 +nan.0+nan.0i)
    (test FL3 CFL01 +nan.0+nan.0i)
    (test FL4 CFL01 +nan.0+nan.0i)
    (test FL1 CFL02 +nan.0+nan.0i)
    (test FL2 CFL02 +nan.0+nan.0i)
    (test FL3 CFL02 +nan.0+nan.0i)
    (test FL4 CFL02 +nan.0+nan.0i)
    (test FL1 CFL03 +nan.0+nan.0i)
    (test FL2 CFL03 +nan.0+nan.0i)
    (test FL3 CFL03 +nan.0+nan.0i)
    (test FL4 CFL03 +nan.0+nan.0i)
    (test FL1 CFL04 +nan.0+nan.0i)
    (test FL2 CFL04 +nan.0+nan.0i)
    (test FL3 CFL04 +nan.0+nan.0i)
    (test FL4 CFL04 +nan.0+nan.0i)
    (test FL1 CFL05 -0.0+0.0i)
    (test FL2 CFL05 0.0-0.0i)
    (test FL3 CFL05 -1.7691666666666668+0.0i)
    (test FL4 CFL05 1.7691666666666668-0.0i)
    (test FL1 CFL06 -0.0-0.0i)
    (test FL2 CFL06 0.0+0.0i)
    (test FL3 CFL06 -1.7691666666666668-0.0i)
    (test FL4 CFL06 1.7691666666666668+0.0i)
    (test FL1 CFL07 0.0+0.0i)
    (test FL2 CFL07 -0.0-0.0i)
    (test FL3 CFL07 0.0+1.7691666666666668i)
    (test FL4 CFL07 -0.0-1.7691666666666668i)
    (test FL1 CFL08 -0.0+0.0i)
    (test FL2 CFL08 0.0-0.0i)
    (test FL3 CFL08 -0.0+1.7691666666666668i)
    (test FL4 CFL08 0.0-1.7691666666666668i)
    (test FL1 CFL09 -0.0+nan.0i)
    (test FL1 CFL10 -0.0+nan.0i)
    (test FL1 CFL11 +nan.0+0.0i)
    (test FL1 CFL12 +nan.0+0.0i)
    (test FL1 CFL13 +nan.0+nan.0i)
    (test FL1 CFL14 +nan.0+nan.0i)
    (test FL1 CFL15 +nan.0+nan.0i)
    (test FL1 CFL16 +nan.0+nan.0i)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-flonum-compnum)))
    (test FL3 10+20i 0.042460000000000005-0.08492000000000001i)
    (test FL3 1+20.0i 0.00529426433915212-0.1058852867830424i)
    (test FL3 10.0+2i 0.20413461538461541-0.04082692307692308i)
    (test FL3 1/2+20i 0.0026520924422236106-0.10608369768894442i)
    (test FL3 10+2/3i 0.21136061946902657-0.014090707964601771i)
    (test FL3 (C RN02 20) -4.315039937365336e-5-0.10614998245918725i)
    (test FL3 (C 10 RN02) 0.21229985967356754+1.726015119297297e-4i)
    #f)

  #t)


#;(parametrise ((check-test-name	'cflonums))

  (let-syntax ((test (make-inexact-test expt $expt-cflonum-fixnum)))
    (test CFL01 FX1 0.0+0.0i)
    (test CFL02 FX1 -0.0+0.0i)
    (test CFL03 FX1 0.0-0.0i)
    (test CFL04 FX1 -0.0-0.0i)
    (test CFL01 FX2 -0.0-0.0i)
    (test CFL02 FX2 0.0-0.0i)
    (test CFL03 FX2 -0.0+0.0i)
    (test CFL04 FX2 0.0+0.0i)
    (test CFL01 FX3 0.0+0.0i)
    (test CFL02 FX3 -0.0+0.0i)
    (test CFL03 FX3 0.0-0.0i)
    (test CFL04 FX3 -0.0-0.0i)
    (test CFL01 FX4 -0.0-0.0i)
    (test CFL02 FX4 0.0-0.0i)
    (test CFL03 FX4 -0.0+0.0i)
    (test CFL04 FX4 0.0+0.0i)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-cflonum-bignum)))
    (test CFL01 BN1 0.0+0.0i)
    (test CFL02 BN1 -0.0+0.0i)
    (test CFL03 BN1 0.0-0.0i)
    (test CFL04 BN1 -0.0-0.0i)
    (test CFL01 BN2 0.0+0.0i)
    (test CFL02 BN2 -0.0+0.0i)
    (test CFL03 BN2 0.0-0.0i)
    (test CFL04 BN2 -0.0-0.0i)
    (test CFL01 BN3 -0.0-0.0i)
    (test CFL02 BN3 0.0-0.0i)
    (test CFL03 BN3 -0.0+0.0i)
    (test CFL04 BN3 0.0+0.0i)
    (test CFL01 BN4 -0.0-0.0i)
    (test CFL02 BN4 0.0-0.0i)
    (test CFL03 BN4 -0.0+0.0i)
    (test CFL04 BN4 0.0+0.0i)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-cflonum-ratnum)))
    (test CFL01 RN01 0.0+0.0i)
    (test CFL02 RN01 -0.0+0.0i)
    (test CFL03 RN01 0.0-0.0i)
    (test CFL04 RN01 -0.0-0.0i)
    (test CFL01 RN02 -0.0-0.0i)
    (test CFL02 RN02 0.0-0.0i)
    (test CFL03 RN02 -0.0+0.0i)
    (test CFL04 RN02 0.0+0.0i)
    (test CFL01 RN03 -0.0-0.0i)
    (test CFL02 RN03 0.0-0.0i)
    (test CFL03 RN03 -0.0+0.0i)
    (test CFL04 RN03 0.0+0.0i)
    (test CFL01 RN04 -0.0-0.0i)
    (test CFL02 RN04 0.0-0.0i)
    (test CFL03 RN04 -0.0+0.0i)
    (test CFL04 RN04 0.0+0.0i)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-cflonum-flonum)))
    (test CFL01 FL1 +nan.0+nan.0i)
    (test CFL02 FL1 +nan.0+nan.0i)
    (test CFL03 FL1 +nan.0+nan.0i)
    (test CFL04 FL1 +nan.0+nan.0i)
    (test CFL01 FL2 +nan.0+nan.0i)
    (test CFL02 FL2 +nan.0+nan.0i)
    (test CFL03 FL2 +nan.0+nan.0i)
    (test CFL04 FL2 +nan.0+nan.0i)
    (test CFL01 FL3 0.0+0.0i)
    (test CFL02 FL3 -0.0+0.0i)
    (test CFL03 FL3 0.0-0.0i)
    (test CFL04 FL3 -0.0-0.0i)
    (test CFL01 FL4 -0.0-0.0i)
    (test CFL02 FL4 0.0-0.0i)
    (test CFL03 FL4 -0.0+0.0i)
    (test CFL04 FL4 0.0+0.0i)
    #f)

  (let-syntax ((test (make-cflonum-test expt $expt-cflonum-cflonum)))
    (test CFL01 CFL01 +nan.0+nan.0i)
    (test CFL02 CFL01 +nan.0+nan.0i)
    (test CFL03 CFL01 +nan.0+nan.0i)
    (test CFL04 CFL01 +nan.0+nan.0i)
    (test CFL01 CFL02 +nan.0+nan.0i)
    (test CFL02 CFL02 +nan.0+nan.0i)
    (test CFL03 CFL02 +nan.0+nan.0i)
    (test CFL04 CFL02 +nan.0+nan.0i)
    (test CFL01 CFL03 +nan.0+nan.0i)
    (test CFL02 CFL03 +nan.0+nan.0i)
    (test CFL03 CFL03 +nan.0+nan.0i)
    (test CFL04 CFL03 +nan.0+nan.0i)
    (test CFL01 CFL04 +nan.0+nan.0i)
    (test CFL02 CFL04 +nan.0+nan.0i)
    (test CFL03 CFL04 +nan.0+nan.0i)
    (test CFL04 CFL04 +nan.0+nan.0i)
    (test CFL01 CFL05 -0.0+0.0i)
    (test CFL02 CFL05 0.0-0.0i)
    (test CFL03 CFL05 0.0+0.0i)
    (test CFL04 CFL05 0.0+0.0i)
    (test CFL01 CFL06 0.0-0.0i)
    (test CFL02 CFL06 0.0+0.0i)
    (test CFL03 CFL06 -0.0+0.0i)
    (test CFL04 CFL06 0.0+0.0i)
    (test CFL01 CFL07 0.0+0.0i)
    (test CFL02 CFL07 -0.0+0.0i)
    (test CFL03 CFL07 0.0+0.0i)
    (test CFL04 CFL07 0.0-0.0i)
    (test CFL01 CFL08 -0.0+0.0i)
    (test CFL02 CFL08 0.0-0.0i)
    (test CFL03 CFL08 0.0+0.0i)
    (test CFL04 CFL08 0.0+0.0i)
    (test CFL01 CFL09 +nan.0+nan.0i)
    (test CFL01 CFL10 +nan.0+nan.0i)
    (test CFL01 CFL11 +nan.0+nan.0i)
    (test CFL01 CFL12 +nan.0+nan.0i)
    (test CFL01 CFL13 +nan.0+nan.0i)
    (test CFL01 CFL14 +nan.0+nan.0i)
    (test CFL01 CFL15 +nan.0+nan.0i)
    (test CFL01 CFL16 +nan.0+nan.0i)
    #f)

  (let-syntax ((test (make-inexact-test expt $expt-cflonum-compnum)))
    (test CFL01 10+20i 0.0+0.0i)
    (test CFL01 1+20.0i 0.0+0.0i)
    (test CFL01 10.0+2i 0.0+0.0i)
    (test CFL01 1/2+20i 0.0+0.0i)
    (test CFL01 10+2/3i 0.0+0.0i)
    (test CFL01 (C RN02 20) 0.0-0.0i)
    (test CFL01 (C 10 RN02) 0.0+0.0i)
    #f)

  #t)


#;(parametrise ((check-test-name	'compnums))

  (letrec-syntax ((test (make-inexact-test expt $expt-compnum-fixnum)))
    (test 10+20i 12 5/6+5/3i)
    (test 1+20.0i 12 1/12+1.6666666666666667i)
    (test 10.0+2i 12 0.8333333333333334+1/6i)
    (test 1/2+20i 12 1/24+5/3i)
    (test 10+2/3i 12 5/6+1/18i)
    (test (C BN1 20) 12 134217728/3+5/3i)
    (test (C 10 BN1) 12 5/6+134217728/3i)
    #f)

  (letrec-syntax ((test (make-test expt $expt-compnum-bignum)))
    (test 10+20i BN1 5/268435456+5/134217728i)
    (test 1+20.0i BN1 1/536870912+3.725290298461914e-8i)
    (test 10.0+2i BN1 1.862645149230957e-8+2/536870912i)
    (test 1/2+20i BN1 1/1073741824+5/134217728i)
    (test 10+2/3i BN1 5/268435456+1/805306368i)
    (test (C BN2 20) BN1 536871011/536870912+5/134217728i)
    (test (C 10 BN2) BN1 5/268435456+536871011/536870912i)
    #f)

  (letrec-syntax ((test (make-inexact-test expt $expt-compnum-ratnum)))
    (test 10+20i RN01 1230+2460i)
    (test 1+20.0i RN01 123+2459.9999999999995i)
    (test 10.0+2i RN01 1229.9999999999998+246i)
    (test 1/2+20i RN01 123/2+2460i)
    (test 10+2/3i RN01 1230+82i)
    (test (C RN02 20) RN01 -1+2460i)
    (test (C 10 RN02) RN01 1230-1i)
    #f)

  (letrec-syntax ((test (make-test expt $expt-compnum-flonum)))
    (test 10+20i FL1 +inf.0+inf.0i)
    (test 1+20.0i FL1 +inf.0+inf.0i)
    (test 10.0+2i FL1 +inf.0+inf.0i)
    (test 1/2+20i FL1 +inf.0+inf.0i)
    (test 10+2/3i FL1 +inf.0+inf.0i)
    (test (C BN2 20) FL1 +inf.0+inf.0i)
    (test (C 10 BN2) FL1 +inf.0+inf.0i)
    #f)

  (letrec-syntax ((test (make-inexact-test expt $expt-compnum-cflonum)))
    (test 10+20i CFL01 +nan.0+nan.0i)
    (test 1+20.0i CFL01 +nan.0+nan.0i)
    (test 10.0+2i CFL01 +nan.0+nan.0i)
    (test 1/2+20i CFL01 +nan.0+nan.0i)
    (test 10+2/3i CFL01 +nan.0+nan.0i)
    (test (C BN2 20) CFL01 +nan.0+nan.0i)
    (test (C 10 BN2) CFL01 +nan.0+nan.0i)

    (test 10+20i CFL05 -8.333333333333334-16.666666666666668i)
    (test 1.0+20.0i CFL05 -0.8333333333333334-16.666666666666668i)
    (test 10.0+2.0i CFL05 -8.333333333333334-1.6666666666666667i)
    (test 1/2+20i CFL05 -0.4166666666666667-16.666666666666668i)
    (test 10+2/3i CFL05 -8.333333333333334-0.5555555555555556i)
    (test (C BN2 20) CFL05 -447392509.1666666-16.666666666666668i)
    (test (C 10 BN2) CFL05 -8.333333333333334-447392509.1666666i)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
