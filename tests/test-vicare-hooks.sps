;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for hook objects
;;;Date: Wed Jun 15, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (vicare language-extensions hooks)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: hooks\n")


(parametrise ((check-test-name	'base))

  (check	;no arguments
      (let ((H		(make-hook))
	    (result	0))
        (add-hook! H (lambda () (set! result (+ result 2))))
        (add-hook! H (lambda () (set! result (+ result 3))))
	(run-hook H)
	result)
    => 5)

  (check	;multiple runs
      (let ((H		(make-hook))
	    (result	0))
        (add-hook! H (lambda () (set! result (+ result 2))))
        (add-hook! H (lambda () (set! result (+ result 3))))
	(run-hook H)
	(run-hook H)
	result)
    => 10)

  (check	;one argument
      (let ((H		(make-hook))
	    (result	0))
        (add-hook! H (lambda (x) (set! result (+ result x))))
        (add-hook! H (lambda (x) (set! result (+ result x))))
	(run-hook H 2)
	result)
    => 4)

  (check	;forgetting first
      (let ((H		(make-hook))
	    (result	0))
	(define (p)
	  (set! result (+ result 2)))
	(define (q)
	  (set! result (+ result 3)))
        (add-hook! H p)
        (add-hook! H q)
	(remove-hook! H p)
	(run-hook H)
	result)
    => 3)

  (check	;forgetting second
      (let ((H		(make-hook))
	    (result	0))
	(define (p)
	  (set! result (+ result 2)))
	(define (q)
	  (set! result (+ result 3)))
        (add-hook! H p)
        (add-hook! H q)
	(remove-hook! H q)
	(run-hook H)
	result)
    => 2)

  (check	;order
      (let ((H		(make-hook))
	    (result	'()))
        (add-hook! H (lambda () (set! result (cons 2 result))))
        (add-hook! H (lambda () (set! result (cons 1 result))))
	(run-hook H)
	(reverse result))
    => '(1 2))

  (check	;order
      (let ((H		(make-hook))
	    (result	'()))
        (add-hook! H (lambda () (set! result (cons 2 result))))
        (add-hook! H (lambda () (set! result (cons 1 result))))
	(add-hook! H (lambda () (set! result (cons 3 result))) #t)
	(run-hook H)
	(reverse result))
    => '(1 2 3))

  (check	;list
      (hook->list (make-hook))
    => '())

  (check	;list
      (let ((H		(make-hook))
	    (result	0))
	(define (p)
	  (set! result (+ result 2)))
	(define (q)
	  (set! result (+ result 3)))
        (add-hook! H p)
        (add-hook! H q)
	(equal? (hook->list H) (list q p)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
