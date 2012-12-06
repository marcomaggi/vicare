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


;;;; constants

(define SMALLEST-POSITIVE-BIGNUM	(-    (least-fixnum)))
(define SMALLEST-NEGATIVE-BIGNUM	(+ -1 (least-fixnum)))

(define BN1	(+ +1  SMALLEST-POSITIVE-BIGNUM))
(define BN2	(+ +10 SMALLEST-POSITIVE-BIGNUM))
(define BN3	(+ -1  SMALLEST-NEGATIVE-BIGNUM))
(define BN4	(+ -10 SMALLEST-NEGATIVE-BIGNUM))


(parametrise ((check-test-name	'bignum-exponent))

  (let-syntax ((test (make-test expt $expt-fixnum-bignum)))
    (test 0	BN1	0)
    (test +1	BN1	+1)
    (test -1	BN1	(if (even? BN1) +1 -1))

    ;; (test FX1 BN1 1)
    ;; (test FX2 BN1 536871011/536870912)
    ;; (test FX3 BN1 -536870913/536870912)
    ;; (test FX4 BN1 -134217753/134217728)
    ;; (test FX1 BN2 536870912/536871011)
    ;; (test FX2 BN2 1)
    ;; (test FX3 BN2 -536870913/536871011)
    ;; (test FX4 BN2 -536871012/536871011)
    ;; (test FX1 BN3 -536870912/536870913)
    ;; (test FX2 BN3 -536871011/536870913)
    ;; (test FX3 BN3 1)
    ;; (test FX4 BN3 178957004/178956971)
    ;; (test FX1 BN4 -134217728/134217753)
    ;; (test FX2 BN4 -536871011/536871012)
    ;; (test FX3 BN4 178956971/178957004)
    ;; (test FX4 BN4 1)
    #f)

  #;(let-syntax ((test (make-test expt $expt-bignum-bignum)))
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

  #;(let-syntax ((test (make-test expt $expt-ratnum-bignum)))
    (test 1/2 BN1 1/1073741824)
    (test 1/2 BN2 1/1073742022)
    (test 1/2 BN3 -1/1073741826)
    (test 1/2 BN4 -1/1073742024)
    (test -1/2 BN1 -1/1073741824)
    (test -1/2 BN2 -1/1073742022)
    (test -1/2 BN3 1/1073741826)
    (test -1/2 BN4 1/1073742024)
    #f)

  #;(let-syntax ((test (make-inexact-test expt $expt-flonum-bignum)))
    (test FL01 BN1 0.0)
    (test FL02 BN1 -0.0)
    (test FL03 BN1 3.954395651817322e-9)
    (test FL04 BN1 -3.954395651817322e-9)
    (test FL01 BN2 0.0)
    (test FL02 BN2 -0.0)
    (test FL03 BN2 3.954394922619505e-9)
    (test FL04 BN2 -3.954394922619505e-9)
    (test FL01 BN3 -0.0)
    (test FL02 BN3 0.0)
    (test FL03 BN3 -3.954395644451687e-9)
    (test FL04 BN3 3.954395644451687e-9)
    (test FL01 BN4 -0.0)
    (test FL02 BN4 0.0)
    (test FL03 BN4 -3.954394915253872e-9)
    (test FL04 BN4 3.954394915253872e-9)
    #f)

  #;(let-syntax ((test (make-inexact-test expt $expt-cflonum-bignum)))
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

  #;(letrec-syntax ((test (make-test expt $expt-compnum-bignum)))
    (test 10+20i BN1 5/268435456+5/134217728i)
    (test 1+20.0i BN1 1/536870912+3.725290298461914e-8i)
    (test 10.0+2i BN1 1.862645149230957e-8+2/536870912i)
    (test 1/2+20i BN1 1/1073741824+5/134217728i)
    (test 10+2/3i BN1 5/268435456+1/805306368i)
    (test (C BN2 20) BN1 536871011/536870912+5/134217728i)
    (test (C 10 BN2) BN1 5/268435456+536871011/536870912i)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
