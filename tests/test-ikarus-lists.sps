;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests from the file "scheme/tests/lists.ss" file in the original
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

(define (run-tests) (test-lists))

(define-tests test-lists
  [values (equal? (for-all even? '(1 2 3 4)) #f)]
  [values (equal? (for-all even? '(10 12 14 16)) #t)]
  [values (equal? (for-all even? '(2 3 4)) #f)]
  [values (equal? (for-all even? '(12 14 16)) #t)]
  [values (equal? (for-all (lambda (x) x) '(12 14 16)) 16)]
  [values (equal? (for-all (lambda (x) x) '(12 14)) 14)]
  [values (equal? (for-all (lambda (x) x) '(12)) 12)]
  [values (equal? (for-all (lambda (x) x) '()) #t)]
  [values (equal? (for-all even? '(13 . 14)) #f)]
  [values (equal? (for-all cons '(1 2 3) '(a b c)) '(3 . c))]
  [values (equal? (for-all (lambda (a b) (= a 1)) '(1 2 3) '(a b c)) #f)]
  [values (equal? (for-all (lambda (a b) (= a 1)) '(1 2) '(a b c)) #f)]
  [values (equal? (fold-left + 0 '(1 2 3 4 5)) 15)]
  [values (equal? (fold-left (lambda (a b) (cons b a)) '() '(1 2 3 4 5))
		  '(5 4 3 2 1))]
  [values (equal? (fold-left (lambda (count x)
			       (if (odd? x)
				   (+ count 1)
				 count))
		    0
		    '(3 1 4 1 5 9 2 6 5 3))
		  7)]
  [values (equal? (fold-left cons '(q) '(a b c)) '((((q) . a) . b) . c))]
  [values (equal? (fold-left + 0 '(1 2 3) '(4 5 6)) 21)]
  [values (equal? (fold-right + 0 '(1 2 3 4 5)) 15)]
  [values (equal? (fold-right cons '() '(1 2 3 4 5))
		  '(1 2 3 4 5))]
  [values (equal? (fold-right (lambda (x l)
				(if (odd? x)
				    (cons x l)
				  l))
		    '()
		    '(3 1 4 1 5 9 2 6 5 3))
		  '(3 1 1 5 9 5 3))]
  [values (equal? (fold-right + 0 '(1 2 3) '(4 5 6)) 21)]
  )

(display "*** testing lists\n" (current-error-port))
(flush-output-port (current-error-port))
(run-tests)
(display "; *** done\n" (current-error-port))
(flush-output-port (current-error-port))


;;; end of file
