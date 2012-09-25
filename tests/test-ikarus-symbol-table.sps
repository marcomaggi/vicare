;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests from  the file "scheme/tests/symbol-table.ss"  file in the
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
(import (ikarus)
  (only (ikarus system $symbols)
	$symbol-table-size))

(define (test-gcable-symbols n)
  ;;Create N new  interned symbols without keeping a  reference to them,
  ;;then explicitly invoke a garbage collection.
  ;;
  (let ([size1 ($symbol-table-size)])
    (do ((i 0 (+ i 1)))
	((= i n))
      (string->symbol (number->string i)))
    (collect)
    (let ([size2 ($symbol-table-size)])
      (assert (< (- size2 size1) n)))))

(define (test-reference-after-gc)
  (define (random-string n)
    (list->string (map (lambda (dummy)
			 (integer->char (+ (char->integer #\a)
					   (random 26))))
		    (make-list n))))
  (newline)
  (let ([str1 (random-string 70)]
	[str2 (random-string 70)])
    ;;Intern a symbol wihout keeping a reference to it.
    (printf "sym1=~s\n" (string->symbol str1))
    ;;Force garbage collection to collect it.
    (do ((i 0 (+ i 1)))
	((= i 1024))
      (collect))
    (let ([sym1 (string->symbol str1)])
      (printf "sym1=~s\n" (string->symbol str1))
      (printf "sym2=~s\n" (string->symbol str2))
      (let ([sym3 (string->symbol str1)])
	(printf "sym3=~s\n" (string->symbol str1))
	(newline)
	(assert (eq? sym1 sym3))))))


(define (run-tests)
  (test-gcable-symbols 1000000)
  (test-reference-after-gc))

(set-port-buffer-mode! (current-output-port) (buffer-mode line))
(display "*** testing symbol table\n" (current-error-port))
(run-tests)
(display "; *** done\n" (current-error-port))

;;; end of file
