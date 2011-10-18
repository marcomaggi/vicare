;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests from the file  "scheme/tests/fasl.ss" file in the original
;;;	Ikarus distribution.
;;;
;;;Copyright (C) 2006-2010 Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
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

#!ikarus
(import (ikarus)
  (ikarus-test-framework))

(define (equal-objects? x y)
  (define (vector-andmap f v . v*)
    (apply andmap f (vector->list v) (map vector->list v*)))
  (if (and (hashtable? x) (hashtable? y))
      (and (eqv? (hashtable-hash-function x)
		 (hashtable-hash-function y))
	   (eqv? (hashtable-equivalence-function x)
		 (hashtable-equivalence-function y))
	   (let-values ([(keys vals) (hashtable-entries x)])
	     (vector-andmap
	      (lambda (k v)
		(equal-objects? v (hashtable-ref y k (gensym))))
	      keys vals))
	   (let-values ([(keys vals) (hashtable-entries y)])
	     (vector-andmap
	      (lambda (k v)
		(equal-objects? v (hashtable-ref x k (gensym))))
	      keys vals)))
    (equal? x y)))

(define (test x)
  (printf "test-fasl ~s\n" x)
  (let ([y (deserialize (serialize x))])
    (unless (equal-objects? x y)
      (error 'test-fasl "failed/expected" y x))))

(define (serialize x)
  (let-values ([(p e) (open-bytevector-output-port)])
    (fasl-write x p)
    (e)))
(define (deserialize x)
  (fasl-read (open-bytevector-input-port x)))

(define (test-cycle)
  (let ([x (cons 1 2)])
    (set-car! x x)
    (set-cdr! x x)
    (printf "test-fasl ~s\n" x)
    (let ([x (deserialize (serialize x))])
      (assert (pair? x))
      (assert (eq? x (car x)))
      (assert (eq? x (cdr x))))))


(define (run-tests)
  (test 12)
  (test -12)
  (test (greatest-fixnum))
  (test (least-fixnum))
  (test 0)
  (test #t)
  (test #f)
  (test '())
  (test "Hello")
  (test "He\x3bb;\x3bb;o")
  (test 'hello)
  (test '(Hello There))
  (test 3498798327498723894789237489324)
  (test -3498798327498723894789237489324)
  (test 2389478923749872389723894/23498739874892379482374)
  (test -2389478923749872389723894/23498739874892379482374)
  (test 127487384734.4)
  (test (make-rectangular 12 13))
  (test (make-rectangular 12.0 13.0))
  (test (string #\a))
  (test (string #\x3bb))
  (test-cycle)
  (test '#1=((x . #1#) (y . z)))
  (test (let ([h (make-eq-hashtable)])
	  (hashtable-set! h 'foo 12)
	  (hashtable-set! h 'bar 13)
	  (collect)
	  h))
  (test (let ([h (make-eq-hashtable)])
	  (hashtable-set! h (gensym) 12)
	  (hashtable-set! h (gensym) 13)
	  (collect)
	  h))
  (test '(#\x3000)))

(display "*** testing fasl\n" (current-error-port))
(flush-output-port (current-error-port))
(run-tests)
(display "; *** done\n" (current-error-port))
(flush-output-port (current-error-port))

;;; end of file
