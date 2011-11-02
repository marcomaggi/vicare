;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests  from  the file  "scheme/tests/guardians.ss"  file in  the
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

(define (test1)
  (define (for-each-pair f x)
    (when (pair? x)
      (f x)
      (for-each-pair f (cdr x))))

  (define n 100)

  (define ls (make-list n))

  (define g (make-guardian))

  (for-each-pair g ls)

  (set! ls (cdr ls))

  (let f ([i 1])
    (unless (= i n)
      (collect)
      (cond
       [(g) =>
	(lambda (p)
	  (printf " [~s/~s]" i n)
	  (assert (eq? (cdr p) ls))
	  (set! ls (cdr ls))
	  (f (+ i 1)))]
       [else (f i)])))
  (assert (null? ls)))

(display "*** testing Ikarus guardians\n\n" (current-error-port))
(flush-output-port (current-error-port))
(test1)
(display "\n\n; *** done\n" (current-error-port))
(flush-output-port (current-error-port))

;;; end of file
