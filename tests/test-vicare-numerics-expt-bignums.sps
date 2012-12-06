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

(define SMALLEST-POSITIVE-BIGNUM	(-    (least-fixnum)))
(define SMALLEST-NEGATIVE-BIGNUM	(+ -1 (least-fixnum)))

(define BN1	(+ +1  SMALLEST-POSITIVE-BIGNUM))
(define BN2	(+ +10 SMALLEST-POSITIVE-BIGNUM))
(define BN3	(+ -1  SMALLEST-NEGATIVE-BIGNUM))
(define BN4	(+ -10 SMALLEST-NEGATIVE-BIGNUM))


(parametrise ((check-test-name	'fixnum-base))

  (define-syntax test
    (make-test expt $expt-number-bignum $expt-fixnum-bignum))

  (test 0	BN1	0)
  (test +1	BN1	+1)
  (test -1	BN1	(if (even? BN1) +1 -1))

  (catch-implementation-restriction
   "result is too big to compute"
   ($expt-fixnum-bignum 123 BN1))

  (test 0	BN2	0)
  (test +1	BN2	+1)
  (test -1	BN2	(if (even? BN2) +1 -1))

  (catch-implementation-restriction
   "result is too big to compute"
   ($expt-fixnum-bignum 123 BN2))

  (test 0	BN3	0)
  (test +1	BN3	+1)
  (test -1	BN3	(if (even? BN3) +1 -1))

  (catch-implementation-restriction
   "result is too big to compute"
   ($expt-fixnum-bignum 123 BN3))

  (test 0	BN4	0)
  (test +1	BN4	+1)
  (test -1	BN4	(if (even? BN4) +1 -1))

  (catch-implementation-restriction
   "result is too big to compute"
   ($expt-fixnum-bignum 123 BN4))

  #t)


(parametrise ((check-test-name	'bignum-base))

  (catch-implementation-restriction
   "result is too big to compute"
   ($expt-fixnum-bignum BN1 BN1))

  #t)


(parametrise ((check-test-name	'ratnum-base))

  (catch-implementation-restriction
   "result is too big to compute"
   ($expt-ratnum-bignum 1/2 BN1))

  #t)


(parametrise ((check-test-name	'flonum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-bignum $expt-flonum-bignum))

  (test	+0.0			BN1	+0.0)
  (test	-0.0			BN1	(if (even? BN1) +0.0 -0.0))
  (test	+1.0			BN1	+1.0)
  (test	-1.0			BN1	(if (even? BN1) +1.0 -1.0))
  (test	+nan.0			BN1	+nan.0)
  (test	+inf.0			BN1	+inf.0)
  (test	-inf.0			BN1	(if (even? BN1) +inf.0 -inf.0))
  (test	1.00000000000001	BN1	1.0000053644324274)

  (test	+0.0			BN2	+0.0)
  (test	-0.0			BN2	(if (even? BN2) +0.0 -0.0))
  (test	+1.0			BN2	+1.0)
  (test	-1.0			BN2	(if (even? BN2) +1.0 -1.0))
  (test	+nan.0			BN2	+nan.0)
  (test	+inf.0			BN2	+inf.0)
  (test	-inf.0			BN2	(if (even? BN2) +inf.0 -inf.0))
  (test	1.00000000000001	BN2	1.0000053644325173)

  (test	+0.0			BN3	+0.0)
  (test	-0.0			BN3	(if (even? BN3) +0.0 -0.0))
  (test	+1.0			BN3	+1.0)
  (test	-1.0			BN3	(if (even? BN3) +1.0 -1.0))
  (test	+nan.0			BN3	+nan.0)
  (test	+inf.0			BN3	+inf.0)
  (test	-inf.0			BN3	(if (even? BN3) +inf.0 -inf.0))
  (test	1.00000000000001	BN3	0.9999946355963379)

  (test	+0.0			BN4	+0.0)
  (test	-0.0			BN4	(if (even? BN4) +0.0 -0.0))
  (test	+1.0			BN4	+1.0)
  (test	-1.0			BN4	(if (even? BN4) +1.0 -1.0))
  (test	+nan.0			BN4	+nan.0)
  (test	+inf.0			BN4	+inf.0)
  (test	-inf.0			BN4	(if (even? BN4) +inf.0 -inf.0))
  (test	1.00000000000001	BN4	0.999994635596248)

  #t)


(parametrise ((check-test-name	'cflonum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-bignum $expt-cflonum-bignum))

  (test		+1.0+2.0i	BN1	-inf.0+inf.0i)
  (test		+0.1+0.2i	BN1	-0.0+0.0i)
  (test		+1e-5+2e-5i	BN1	-0.0+0.0i)

  (test		+1e-5+2e-5i	BN2	-0.0+0.0i)

  #t)


(parametrise ((check-test-name	'compnum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-bignum $expt-compnum-bignum))

  (test		+1+2.i	BN1	-inf.0+inf.0i)

  #t)


;;;; done

(check-report)

;;; end of file
