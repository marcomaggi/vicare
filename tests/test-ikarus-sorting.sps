;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests  from  the  file  "scheme/tests/sorting.ss"  file  in  the
;;;	original Ikarus distribution.
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
(import (ikarus))

(define (permutations ls)
  (define (rem* ls)
    (cond
     [(null? ls) '()]
     [else
      (cons (cdr ls)
	    (map
		(lambda (a) (cons (car ls) a))
	      (rem* (cdr ls))))]))
  (cond
   [(null? ls) '(())]
   [else
    (apply append
	   (map
	       (lambda (x a*)
		 (map (lambda (a) (cons x a)) a*))
	     ls
	     (map permutations (rem* ls))))]))


(define (test-permutations)
  (define (fact n)
    (if (zero? n)
	1
      (* n (fact (- n 1)))))
  (define (test ls)
    (let ([p* (permutations ls)])
      (when #f
	(printf "Testing ~s permutations of ~s\n" (length p*) ls))
      (unless (= (length p*) (fact (length ls)))
	(error 'test-permutations "incorrect number of permutations"))
      (let f ([p* p*])
	(unless (null? p*)
	  (let ([p (car p*)])
	    (when (member p (cdr p*))
	      (error 'test-permutations "duplicate" p))
	    (f (cdr p*)))))))
  (test '())
  (test '(1))
  (test '(1 2))
  (test '(1 2 3))
  (test '(1 2 3 4))
  (test '(1 2 3 4 5))
  (test '(1 2 3 4 5 6)))



(define (test-vector-sort)
  (define (test ls)
    (let ([v1 (list->vector ls)]
	  [p* (map list->vector (permutations ls))])
      (when #f
	(printf "Testing vector-sort for all ~s permutations of ~s\n" (length p*) v1))
      (for-each
          (lambda (p)
            (let* ([copy (list->vector (vector->list p))]
                   [sv (vector-sort < p)])
              (unless (equal? copy p)
                (error 'test-vector-sort "vector was mutated"))
              (unless (equal? v1 sv)
                (error 'test-vector-sort "failed" p sv))))
	p*)))
  (test '())
  (test '(1))
  (test '(1 2))
  (test '(1 2 3))
  (test '(1 2 3 4))
  (test '(1 2 3 4 5))
  (test '(1 2 3 4 5 6))
  (test '(1 2 3 4 5 6 7))
  (test '(1 2 3 4 5 6 7 8)))

(define (test-list-sort)
  (define (test ls)
    (let ([p* (permutations ls)])
      (when #f
	(printf "Testing list-sort for all ~s permutations of ~s\n" (length p*) ls))
      (for-each
          (lambda (p)
            (let* ([copy (map values p)]
                   [sv (list-sort < p)])
              (unless (equal? copy p)
                (error 'test-list-sort "list was changed"))
              (unless (equal? ls sv)
                (error 'test-list-sort "failed" p sv))))
	p*)))
  (test '())
  (test '(1))
  (test '(1 2))
  (test '(1 2 3))
  (test '(1 2 3 4))
  (test '(1 2 3 4 5))
  (test '(1 2 3 4 5 6))
  (test '(1 2 3 4 5 6 7))
  (test '(1 2 3 4 5 6 7 8)))

(define (run-tests)
  (test-permutations)
  (test-vector-sort)
  (test-list-sort))

(set-port-buffer-mode! (current-output-port) (buffer-mode line))
(display "*** testing Ikarus sorting\n\n" (current-error-port))
(run-tests)
(display "; *** done\n" (current-error-port))

;;; end of file
