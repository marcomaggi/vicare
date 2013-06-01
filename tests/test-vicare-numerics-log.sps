;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: log
;;;Date: Sun Dec  9, 2012
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
  (ikarus system $numerics)
  (vicare checks)
  (only (vicare language-extensions syntaxes)
	case-word-size))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numerics functions: log\n")


;;;; helpers

(define C make-rectangular)
(define R real-part)
(define I imag-part)

(define-syntax make-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	=> ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	=> ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	=> ?expected-result)
	  ))))))

(define-syntax make-flonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  ))))))

(define-syntax make-cflonum-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-func ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	(=> flonum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> cflonum=?) ?expected-result)
	  ))))))

(define-syntax make-compnum-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> compnum=?) ?expected-result)
	  ))))))

(define-syntax make-inexact-test
  (syntax-rules ()
    ((_ ?safe-fun ?middle-fun ?unsafe-fun)
     (syntax-rules ()
       ((_ ?op1 ?op2 ?expected-result)
	(begin
	  (check (?safe-fun   ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  (check (?middle-fun ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  (check (?unsafe-fun ?op1 ?op2)	(=> inexact=?) ?expected-result)
	  ))))))

;;; --------------------------------------------------------------------

(define-syntax catch-implementation-restriction
  (syntax-rules ()
    ((_ ?message . ?body)
     (check
	 (guard (E ((implementation-restriction-violation? E)
		    (condition-message E))
		   (else E))
	   (begin . ?body))
       => ?message))))

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
	 (fl<? (flabs (fl- x y))
	       1e-5)
	 #;(fl<? (fl/ (flabs (fl- x y))
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

(define SMALLEST-POSITIVE-BIGNUM	(-    (least-fixnum)))
(define SMALLEST-NEGATIVE-BIGNUM	(+ -1 (least-fixnum)))

(define BN1	(+ +1  SMALLEST-POSITIVE-BIGNUM))
(define BN2	(+ +10 SMALLEST-POSITIVE-BIGNUM))
(define BN3	(+ -1  SMALLEST-NEGATIVE-BIGNUM))
(define BN4	(+ -10 SMALLEST-NEGATIVE-BIGNUM))

(define RN1	+13/11)
(define RN2	+17/11)
(define RN3	-13/11)
(define RN4	-17/11)


(parametrise ((check-test-name	'fixnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test +1			0)
  (test -1			0.0+3.141592653589793i)

  (case-word-size
   ((32)
    (test (greatest-fixnum)	20.10126823437577)
    (test (least-fixnum)	20.101268236238415+3.141592653589793i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-bignum ?op)	(=> inexact=?) ?expected)))))


  (case-word-size
   ((32)
    (test BN1			20.10126823810106)
    (test BN2			20.101268254864866)
    (test BN3			20.101268239963705+3.141592653589793i)
    (test BN4			20.10126825672751+3.141592653589793i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test +1/2			-0.6931471805599453)
  (test -1/2			-0.6931471805599453+3.141592653589793i)

  (case-word-size
   ((32)
    (test (/ BN1 123)		15.289083882728642)
    (test (/ BN2 123)		15.289083899492448)
    (test (/ BN3 123)		15.289083884591287+3.141592653589793i)
    (test (/ BN4 123)		15.289083901355093+3.141592653589793i)

    (test (/ 123 BN1)		-15.289083882728642)
    (test (/ 123 BN2)		-15.289083899492448)
    (test (/ 123 BN3)		-15.289083884591287+3.141592653589793i)
    (test (/ 123 BN4)		-15.289083901355093+3.141592653589793i))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0			-inf.0)
  (test -0.0			-inf.0+3.141592653589793i)
  (test +1.0			0.0)
  (test -1.0			0.0+3.141592653589793i)

  (test +inf.0			+inf.0)
  (test -inf.0			+inf.0+3.141592653589793i)
  (test +nan.0			+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		0.0+0.0i)
  (test -1+0.0i		0.0+3.141592653589793i)
  (test +1-0.0i		0.0-0.0i)
  (test -1-0.0i		0.0-3.141592653589793i)
  (test +0.0+1i		0.0+1.5707963267948966i)
  (test +0.0-1i		0.0-1.5707963267948966i)
  (test -0.0+1i		0.0+1.5707963267948966i)
  (test -0.0-1i		0.0-1.5707963267948966i)

  (test +1.0+1i		0.34657359027997264+0.7853981633974483i)
  (test +1-1.0i		0.34657359027997264-0.7853981633974483i)
  (test -1.0+1i		0.34657359027997264+2.356194490192345i)
  (test -1-1.0i		0.34657359027997264-2.356194490192345i)

  (test +0+inf.0i	+inf.0+1.5707963267948966i)
  (test +0-inf.0i	+inf.0-1.5707963267948966i)
  (test -0+inf.0i	+inf.0+1.5707963267948966i)
  (test -0-inf.0i	+inf.0-1.5707963267948966i)

  (test +1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test +1+3i		1.151292546497023+1.2490457723982544i)
  (test +1-3i		1.151292546497023-1.2490457723982544i)
  (test -1+3i		1.151292546497023+1.892546881191539i)
  (test -1-3i		1.151292546497023-1.892546881191539i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (log ?op)		(=> inexact=?) ?expected)
	 (check ($log-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i		-inf.0+0.0i)
  (test -0.0+0.0i		-inf.0+3.141592653589793i)
  (test +0.0-0.0i		-inf.0-0.0i)
  (test -0.0-0.0i		-inf.0-3.141592653589793i)

  (test +1.0+0.0i		0.0+0.0i)
  (test -1.0+0.0i		0.0+3.141592653589793i)
  (test +1.0-0.0i		0.0-0.0i)
  (test -1.0-0.0i		0.0-3.141592653589793i)
  (test +0.0+1.0i		0.0+1.5707963267948966i)
  (test +0.0-1.0i		0.0-1.5707963267948966i)
  (test -0.0+1.0i		0.0+1.5707963267948966i)
  (test -0.0-1.0i		0.0-1.5707963267948966i)

  (test +1.0+1.0i		0.34657359027997264+0.7853981633974483i)
  (test +1.0-1.0i		0.34657359027997264-0.7853981633974483i)
  (test -1.0+1.0i		0.34657359027997264+2.356194490192345i)
  (test -1.0-1.0i		0.34657359027997264-2.356194490192345i)

  (test +inf.0+0.0i		+inf.0+0.0i)
  (test -inf.0+0.0i		+inf.0+3.141592653589793i)
  (test +inf.0-0.0i		+inf.0-0.0i)
  (test -inf.0-0.0i		+inf.0-3.141592653589793i)

  (test +0.0+inf.0i		+inf.0+1.5707963267948966i)
  (test +0.0-inf.0i		+inf.0-1.5707963267948966i)
  (test -0.0+inf.0i		+inf.0+1.5707963267948966i)
  (test -0.0-inf.0i		+inf.0-1.5707963267948966i)

  (test +inf.0+inf.0i		+inf.0+0.7853981633974483i)
  (test +inf.0-inf.0i		+inf.0-0.7853981633974483i)
  (test -inf.0+inf.0i		+inf.0+2.356194490192345i)
  (test -inf.0-inf.0i		+inf.0-2.356194490192345i)

  (test +1.0+nan.0i		+nan.0+nan.0i)
  (test +nan.0+1.0i		+nan.0+nan.0i)
  (test +nan.0+nan.0i		+nan.0+nan.0i)

  (test +1.2+3.4i		1.2824746787307684+1.2315037123408519i)
  (test +1.2-3.4i		1.2824746787307684-1.2315037123408519i)
  (test -1.2+3.4i		1.2824746787307684+1.9100889412489412i)
  (test -1.2-3.4i		1.2824746787307684-1.9100889412489412i)

  (test +1.0+3.0i		1.151292546497023+1.2490457723982544i)
  (test +1.0-3.0i		1.151292546497023-1.2490457723982544i)
  (test -1.0+3.0i		1.151292546497023+1.892546881191539i)
  (test -1.0-3.0i		1.151292546497023-1.892546881191539i)

  #t)


;;;; done

(check-report)

;;; end of file
