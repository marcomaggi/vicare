;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for Vicare equality functions
;;;Date: Thu Nov  3, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare equality functions\n")


;;;; constants

(define BIGNUM0	(+ 1 (greatest-fixnum)))
(define BIGNUM1	(+ 2 (greatest-fixnum)))

(define RATNUM0 1/2)
(define RATNUM1 3/4)

(define FLONUM0 1.)
(define FLONUM1 2.)

(define CFLONUM0 1.2+3.4i)
(define CFLONUM1 5.6+7.8i)

(define COMPNUM0 1+3i)
(define COMPNUM1 5+7i)


(parametrise ((check-test-name	'equal))

  ;;fixnums
  (check (equal? 1 1)	=> #t)
  (check (equal? 1 2)	=> #f)

  ;;bignums
  (check (equal? BIGNUM0 BIGNUM0)	=> #t)
  (check (equal? BIGNUM0 BIGNUM1)	=> #f)

  ;;ratnums
  (check (equal? RATNUM0 RATNUM0)	=> #t)
  (check (equal? RATNUM0 RATNUM1)	=> #f)

  ;;flonums
  (check (equal? FLONUM0 FLONUM0)	=> #t)
  (check (equal? FLONUM0 FLONUM1)	=> #f)

  ;;cflonums
  (check (equal? CFLONUM0 CFLONUM0)	=> #t)
  (check (equal? CFLONUM0 CFLONUM1)	=> #f)

  ;;compnums
  (check (equal? COMPNUM0 COMPNUM0)	=> #t)
  (check (equal? COMPNUM0 COMPNUM1)	=> #f)

;;; --------------------------------------------------------------------
;;; null, pairs and lists

  (check
      (equal? '() '())
    => #t)

  (check
      (equal? '() '(1 . 2))
    => #f)

  (check
      (equal? (cons 1 2) (cons 1 2))
    => #t)

  (check
      (equal? (cons 1 2) (list 1 2))
    => #f)

  (check
      (equal? (cons 1 2) (cons 1 99))
    => #f)

  (check
      (equal? (cons (cons 1 2) (cons 1 2))
	      (let ((p (cons 1 2))) (cons p p)))
    => #t)

  (check
      (equal? (cons (cons 1 2) (cons 1 99))
	      (let ((p (cons 1 2))) (cons p p)))
    => #f)

  (check
      (equal? (let ((x (cons 1 2)))
		(set-car! x x)
		(set-cdr! x x)
		x)
	      (let ((x (cons 1 2)))
		(set-car! x x)
		(set-cdr! x x)
		(cons x x)))
    => #t)

  #t)


(parametrise ((check-test-name	'eqv))

  ;;fixnums
  (check (eqv? 1 1)	=> #t)
  (check (eqv? 1 2)	=> #f)

  ;;bignums
  (check (eqv? BIGNUM0 BIGNUM0)	=> #t)
  (check (eqv? BIGNUM0 BIGNUM1)	=> #f)

  ;;ratnums
  (check (eqv? RATNUM0 RATNUM0)	=> #t)
  (check (eqv? RATNUM0 RATNUM1)	=> #f)

  ;;flonums
  (check (eqv? FLONUM0 FLONUM0)	=> #t)
  (check (eqv? FLONUM0 FLONUM1)	=> #f)

  ;;cflonums
  (check (eqv? CFLONUM0 CFLONUM0)	=> #t)
  (check (eqv? CFLONUM0 CFLONUM1)	=> #f)

  ;;compnums
  (check (eqv? COMPNUM0 COMPNUM0)	=> #t)
  (check (eqv? COMPNUM0 COMPNUM1)	=> #f)

  #t)


(parametrise ((check-test-name	'eq))

  ;;fixnums
  (check (eq? 1 1)	=> #t)
  (check (eq? 1 2)	=> #f)

  ;;bignums
  (check (eq? BIGNUM0 BIGNUM0)	=> #t)
  (check (eq? BIGNUM0 BIGNUM1)	=> #f)

  ;;ratnums
  (check (eq? RATNUM0 RATNUM0)	=> #t)
  (check (eq? RATNUM0 RATNUM1)	=> #f)

  ;;flonums
  (check (eq? FLONUM0 FLONUM0)	=> #t)
  (check (eq? FLONUM0 FLONUM1)	=> #f)

  ;;cflonums
  (check (eq? CFLONUM0 CFLONUM0)	=> #t)
  (check (eq? CFLONUM0 CFLONUM1)	=> #f)

  ;;compnums
  (check (eq? COMPNUM0 COMPNUM0)	=> #t)
  (check (eq? COMPNUM0 COMPNUM1)	=> #f)

  #t)




;;;; done

(check-report)

;;; end of file
