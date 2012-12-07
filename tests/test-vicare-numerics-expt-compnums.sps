;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numerics functions: expt
;;;Date: Fri Dec  7, 2012
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
(check-display "*** testing Vicare numerics functions: expt, compnum exponent\n")


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


(parametrise ((check-test-name	'fixnum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-compnum $expt-fixnum-compnum))

  (test 0	+1+2i	0)
  (test +1	+1+2i	1.0)
  (test -1	+1+2i	-0.0018674427317079893+2.286882234649021e-19i)
  (test +2	+1+2i	0.36691394948660344+1.9660554808224875i)
  (test -2	+1+2i	-6.851907881310297e-4-0.0036714960177966108i)

  #t)


(parametrise ((check-test-name	'bignum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-compnum $expt-bignum-compnum))

  (test BN1	+1+2i	-431208522.4342323+319827433.79131836i)
  (test BN2	+1+2i	-431208540.3859789+319827424.6954508i)
  (test BN3	+1+2i	805257.2247952196-597259.414747084i)
  (test BN4	+1+2i	805257.258319078-597259.397761072i)

  #t)


(parametrise ((check-test-name	'ratnum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-compnum $expt-ratnum-compnum))

  (test 1/2	+1+2i	0.09172848737165086-0.4915138702056219i)
  (test -1/2	+1+2i	-1.7129769703275736e-4+9.178740044491525e-4i)

  #t)


(parametrise ((check-test-name	'flonum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-compnum $expt-flonum-compnum))

  (test	+0.0		+1+2i	0.0)
  (test	-0.0		+1+2i	-0.0)
  (test	+1.0		+1+2i	1.0+0.0i)
  (test	-1.0		+1+2i	-0.0018674427317079893+2.286882234649021e-19i)
  (test	+nan.0		+1+2i	+nan.0+nan.0i)
  (test	+inf.0		+1+2i	+nan.0+nan.0i)
  (test	-inf.0		+1+2i	+nan.0+nan.0i)
  (test +2.0		+1+2i	0.36691394948660344+1.9660554808224875i)
  (test -2.0		+1+2i	-6.851907881310297e-4-0.0036714960177966108i)

  #t)


(parametrise ((check-test-name	'cflonum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-compnum $expt-cflonum-compnum))

  (test	+0.0+0.0i	+1+2i	+0.0+0.0i)
  (test	-0.0+0.0i	+1+2i	+0.0+0.0i)
  (test	+0.0-0.0i	+1+2i	+0.0+0.0i)
  (test	-0.0-0.0i	+1+2i	+0.0+0.0i)
  (test	+nan.0+2.0i	+1+2i	+nan.0+nan.0i)
  (test	+1.0+nan.0i	+1+2i	+nan.0+nan.0i)
  (test	+nan.0+nan.0i	+1+2i	+nan.0+nan.0i)
  (test	+inf.0+2.0i	+1+2i	+nan.0+nan.0i)
  (test	+1.0+inf.0i	+1+2i	+nan.0+nan.0i)
  (test	+inf.0+inf.0i	+1+2i	+nan.0+nan.0i)
  (test	-inf.0+2.0i	+1+2i	+nan.0+nan.0i)
  (test	+1.0-inf.0i	+1+2i	+nan.0+nan.0i)
  (test	-inf.0-inf.0i	+1+2i	+nan.0+nan.0i)
  (test	+1.0+2.0i	+1+2i	-0.22251715680177267+0.10070913113607541i)

  (catch-implementation-restriction
   "undefined result"
   (expt +0.0+0.0i -1+2i))

  #t)


(parametrise ((check-test-name	'compnum-base))

  (define-syntax test
    (make-inexact-test expt $expt-number-compnum $expt-compnum-compnum))

  (test	+1+2i		+1+2i	-0.22251715680177267+0.10070913113607541i)

  #t)


;;;; done

(check-report)

;;; end of file
