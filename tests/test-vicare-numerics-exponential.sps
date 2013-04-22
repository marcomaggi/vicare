;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: exp
;;;Date: Mon Dec 10, 2012
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
(check-display "*** testing Vicare numerics functions: exp\n")


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
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-fixnum ?op)	(=> inexact=?) ?expected)))))

  (test 0			1)
  (test +1			2.718281828459045)
  (test -1			0.36787944117144233)
  (test +2			7.38905609893065)
  (test -2			0.1353352832366127)

  (case-word-size
   ((32)
    (test (greatest-fixnum)	+inf.0)
    (test (least-fixnum)	0.0))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-bignum ?op)	(=> inexact=?) ?expected)))))


  (test BN1			+inf.0)
  (test BN2			+inf.0)
  (test BN3			0.0)
  (test BN4			0.0)

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-ratnum ?op)	(=> inexact=?) ?expected)))))

  (test +1/2			1.6487212707001282)
  (test -1/2			0.6065306597126334)

  (test RN1			3.2602966292874007)
  (test RN2			4.6901030075840895)
  (test RN3			0.30672055757655675)
  (test RN4			0.21321493331446215)

  (case-word-size
   ((32)
    (test (/ BN1 123)		+inf.0)
    (test (/ BN2 123)		+inf.0)
    (test (/ BN3 123)		+0.0)
    (test (/ BN4 123)		+0.0)

    (test (/ 123 BN1)		1.0000002291053791)
    (test (/ 123 BN2)		1.0000002291053753)
    (test (/ 123 BN3)		0.9999997708946737)
    (test (/ 123 BN4)		0.9999997708946776))
   ((64)
    (void)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-flonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0			1.0)
  (test -0.0			1.0)
  (test +1.0			2.718281828459045)
  (test -1.0			0.36787944117144233)
  (test +2.0			7.38905609893065)
  (test -2.0			0.1353352832366127)

  (test +inf.0			+inf.0)
  (test -inf.0			0.0)
  (test +nan.0			+nan.0)

  #t)


(parametrise ((check-test-name	'compnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-compnum ?op)	(=> inexact=?) ?expected)))))

  (test +1+0.0i		2.718281828459045+0.0i)
  (test -1+0.0i		0.36787944117144233+0.0i)
  (test +1-0.0i		2.718281828459045+0.0i)
  (test -1-0.0i		0.36787944117144233+0.0i)
  (test +0.0+1i		0.5403023058681398+0.8414709848078965i)
  (test +0.0-1i		0.5403023058681398-0.8414709848078965i)
  (test -0.0+1i		0.5403023058681398+0.8414709848078965i)
  (test -0.0-1i		0.5403023058681398-0.8414709848078965i)

  (test +1.0+1.0i	1.4686939399158851+2.2873552871788423i)
  (test +1.0-1.0i	1.4686939399158851-2.2873552871788423i)
  (test -1.0+1.0i	0.19876611034641298+0.3095598756531122i)
  (test -1.0-1.0i	0.19876611034641298-0.3095598756531122i)

  (test 0+inf.0i	+nan.0+nan.0i)
  (test 0-inf.0i	+nan.0+nan.0i)

  (test 1+nan.0i	+nan.0+nan.0i)
  (test +nan.0+1i	+nan.0+nan.0i)

  (test 1+3i -2.6910786138197937+0.383603953541131i)
  (test 1-3i -2.6910786138197937-0.383603953541131i)
  (test -1+3i -0.36419788641329287+0.05191514970317339i)
  (test -1-3i -0.36419788641329287-0.05191514970317339i)

  #t)


(parametrise ((check-test-name	'cflonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (exp ?op)		(=> inexact=?) ?expected)
	 (check ($exp-cflonum ?op)	(=> inexact=?) ?expected)))))

  (test +0.0+0.0i		1.0+0.0i)
  (test -0.0+0.0i		1.0+0.0i)
  (test +0.0-0.0i		1.0-0.0i)
  (test -0.0-0.0i		1.0-0.0i)

  (test +1.0+0.0i		2.718281828459045+0.0i)
  (test -1.0+0.0i		0.36787944117144233+0.0i)
  (test +1.0-0.0i		2.718281828459045-0.0i)
  (test -1.0-0.0i		0.36787944117144233-0.0i)

  (test +0.0+1.0i		0.5403023058681398+0.8414709848078965i)
  (test +0.0-1.0i		0.5403023058681398-0.8414709848078965i)
  (test -0.0+1.0i		0.5403023058681398+0.8414709848078965i)
  (test -0.0-1.0i		0.5403023058681398-0.8414709848078965i)

  (test +1.0+1.0i		1.4686939399158851+2.2873552871788423i)
  (test +1.0-1.0i		1.4686939399158851-2.2873552871788423i)
  (test -1.0+1.0i		0.19876611034641298+0.3095598756531122i)
  (test -1.0-1.0i		0.19876611034641298-0.3095598756531122i)

  (test +inf.0+0.0i		+inf.0+nan.0i)
  (test -inf.0+0.0i		0.0+0.0i)
  (test +inf.0-0.0i		+inf.0+nan.0i)
  (test -inf.0-0.0i		0.0-0.0i)

  (test +0.0+inf.0i		+nan.0+nan.0i)
  (test +0.0-inf.0i		+nan.0+nan.0i)
  (test -0.0+inf.0i		+nan.0+nan.0i)
  (test -0.0-inf.0i		+nan.0+nan.0i)

  (test +inf.0+inf.0i		+nan.0+nan.0i)
  (test +inf.0-inf.0i		+nan.0+nan.0i)
  (test -inf.0+inf.0i		+nan.0+nan.0i)
  (test -inf.0-inf.0i		+nan.0+nan.0i)

  (test 1.0+nan.0i		+nan.0+nan.0i)
  (test +nan.0+1.0i		+nan.0+nan.0i)
  (test +nan.0+nan.0i		+nan.0+nan.0i)

  (test +1.2+3.4i		-3.209883040054176-0.8484263372940289i)
  (test +1.2-3.4i		-3.209883040054176+0.8484263372940289i)
  (test -1.2+3.4i		-0.2911940196921122-0.07696750083614708i)
  (test -1.2-3.4i		-0.2911940196921122+0.07696750083614708i)

  (test +1.0+3.0i		-2.6910786138197937+0.383603953541131i)
  (test +1.0-3.0i		-2.6910786138197937-0.383603953541131i)
  (test -1.0+3.0i		-0.36419788641329287+0.05191514970317339i)
  (test -1.0-3.0i		-0.36419788641329287-0.05191514970317339i)

  #t)


;;;; done

(check-report)

;;; end of file
