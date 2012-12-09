;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the numerics functions: abs
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
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare numerics functions: abs\n")


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
	 (check (abs ?op)		=> ?expected)
	 (check ($abs-fixnum ?op)	=> ?expected)))))

  (test 0			0)
  (test +1			+1)
  (test -1			+1)
  (test (greatest-fixnum)	(greatest-fixnum))
  (test (least-fixnum)		(- (least-fixnum)))

  #t)


(parametrise ((check-test-name	'bignums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (abs ?op)		=> ?expected)
	 (check ($abs-bignum ?op)	=> ?expected)))))

  (test BN1			BN1)
  (test BN2			BN2)
  (test BN3			(- BN3))
  (test BN4			(- BN4))

  #t)


(parametrise ((check-test-name	'ratnums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (abs ?op)		=> ?expected)
	 (check ($abs-ratnum ?op)	=> ?expected)))))

  (test +1/2			+1/2)
  (test -1/2			+1/2)

  (test (/ BN1 123)		(/ BN1 123))
  (test (/ BN2 123)		(/ BN2 123))
  (test (/ BN3 123)		(/ (- BN3) 123))
  (test (/ BN4 123)		(/ (- BN4) 123))

  (test (/ 123 BN1)		(/ 123 BN1))
  (test (/ 123 BN2)		(/ 123 BN2))
  (test (/ 123 BN3)		(/ 123 (- BN3)))
  (test (/ 123 BN4)		(/ 123 (- BN4)))

  #t)


(parametrise ((check-test-name	'flonums))

  (define-syntax test
    (syntax-rules ()
      ((_ ?op ?expected)
       (begin
	 (check (abs ?op)		=> ?expected)
	 (check ($abs-flonum ?op)	=> ?expected)))))

  (test +0.0			+0.0)
  (test -0.0			+0.0)
  (test +1.0			+1.0)
  (test -1.0			+1.0)

  (test +inf.0			+inf.0)
  (test -inf.0			+inf.0)
  (test +nan.0			+nan.0)

  #t)


(parametrise ((check-test-name	'misc))

  (check
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(abs 1+2i))
    => "expected real number as argument")

  (check
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(abs 1.0+2.0i))
    => "expected real number as argument")

  #t)


;;;; done

(check-report)

;;; end of file
