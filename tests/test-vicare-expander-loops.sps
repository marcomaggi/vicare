;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the expander
;;;Date: Sun Feb  7, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (test-vicare-expander-loops)
  (options strict-r6rs)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expander loop syntaxes\n")


;;;; helpers

(define (%eval sexp)
  (eval sexp (environment '(vicare))))

(define-syntax check-argument-violation
  (syntax-rules (=>)
    ((_ ?body => ?result)
     (check
	 (guard (E ((procedure-signature-argument-violation? E)
		    #;(print-condition E)
		    (list (condition-who E)
			  (procedure-signature-argument-violation.one-based-argument-index E)
			  (procedure-signature-argument-violation.failed-expression E)
			  (procedure-signature-argument-violation.offending-value E)))
		   ((procedure-signature-return-value-violation? E)
		    #;(print-condition E)
		    (list (condition-who E)
			  (procedure-signature-return-value-violation.one-based-return-value-index E)
			  (procedure-signature-return-value-violation.failed-expression E)
			  (procedure-signature-return-value-violation.offending-value E)))
		   ((procedure-arguments-consistency-violation? E)
		    #;(print-condition E)
		    (condition-irritants E))
		   ((procedure-argument-violation? E)
		    (when #f
		      (debug-print (condition-message E)))
		    (let ((D (cdr (condition-irritants E))))
		      (if (pair? D)
			  (car D)
			(condition-irritants E))))
		   ((assertion-violation? E)
		    (condition-irritants E))
		   (else
		    (print-condition E)
		    E))
	   ?body)
       => ?result))))


(parametrise ((check-test-name	'while))

  (check
      (with-result
       (let ((i 5))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i)))
	 i))
    => '(0 (5 4 3 2 1)))

  (check
      (with-result
       (let ((i 0))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i)))
	 i))
    => '(0 ()))

  (check
      (with-result	;continue
       (let ((i 5))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (continue)
	   (add-result "post"))
	 i))
    => '(0 (5 4 3 2 1)))

  (check
      (with-result	;break
       (let ((i 5))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (break)
	   (add-result "post"))
	 i))
    => '(4 (5)))

  #t)


(parametrise ((check-test-name	'until))

  (check
      (with-result
       (let ((i 5))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i)))
	 i))
    => '(0 (5 4 3 2 1)))

  (check
      (with-result
       (let ((i 0))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i)))
	 i))
    => '(0 ()))

  (check	;continue
      (with-result
       (let ((i 5))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (continue)
	   (add-result "post"))
	 i))
    => '(0 (5 4 3 2 1)))

  (check	;break with no values
      (with-result
       (let ((i 5))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (break)
	   (add-result "post"))
	 i))
    => '(4 (5)))

  #t)


(parametrise ((check-test-name	'for))

  (check	;test true
      (with-result
       (for ((define i 5) (positive? i) (set! i (+ -1 i)))
	 (add-result i))
       #t)
    => '(#t (5 4 3 2 1)))

  (check	;test immediately false
      (with-result
       (for ((define i 0) (positive? i) (set! i (+ -1 i)))
	 (add-result i))
       #t)
    => '(#t ()))

  (check	;continue
      (with-result
       (for ((define i 5) (positive? i) (set! i (+ -1 i)))
	 (add-result i)
	 (continue)
	 (add-result "post"))
       #t)
    => '(#t (5 4 3 2 1)))

  (check	;break with no values
      (with-result
       (for ((define i 5) (positive? i) (set! i (+ -1 i)))
	 (add-result i)
	 (break)
	 (add-result "post"))
       #t)
    => '(#t (5)))

  (check	;multiple bindings
      (with-result
       (for ((begin
	       (define i 5)
	       (define j 10))
	     (positive? i)
	     (begin
	       (set! i (+ -1 i))
	       (set! j (+ -1 j))))
	 (add-result i)
	 (add-result j))
       #t)
    => '(#t (5 10 4 9 3 8 2 7 1 6)))

  (check	;no bindings
      (with-result
       (let ((i #f))
	 (for ((set! i 5) (positive? i) (set! i (+ -1 i)))
	   (add-result i))
	 i))
    => '(0 (5 4 3 2 1)))

  #t)


(parametrise ((check-test-name	'do))

;;; standard do

  (check
      (with-result
	(do ((i 5 (+ -1 i)))
	    ((zero? i)
	     'done)
	  (add-result i)))
    => '(done (5 4 3 2 1)))

  (check	;binding with no step
      (with-result
	(do ((j 123)
	     (i 5 (+ -1 i)))
	    ((zero? i)
	     j)
	  (add-result i)))
    => '(123 (5 4 3 2 1)))

  (check	;break
      (with-result
	(do ((i 0 (+ 1 i)))
	    ((= i 5)
	     i)
	  (add-result i)
	  (when (= i 3)
	    (break 123))))
    => '(123 (0 1 2 3)))

  (check	;break
      (with-result
	(do ((i 0 (+ 1 i)))
	    ((= i 5)
	     i)
	  (add-result i)
	  (when (= i 3)
	    (break 123))))
    => '(123 (0 1 2 3)))

  (check	;continue
      (with-result
	(do ((i 0 (+ 1 i)))
	    ((= i 5)
	     i)
	  (when (= i 3)
	    (continue))
	  (add-result i)))
    => '(5 (0 1 2 4)))

;;; --------------------------------------------------------------------
;;; do ... while

  (check
      (with-result
	(define i 5)
	(do
	    (begin
	      (add-result i)
	      (set! i (+ -1 i)))
	    (while (positive? i))))
    => '((5 4 3 2 1)))

  (check 	;continue
      (with-result
	(define i 5)
	(do
	    (begin
	      (set! i (+ -1 i))
	      (when (= i 3)
		(continue))
	      (add-result i))
	    (while (positive? i))))
    => '((4 2 1 0)))

  (check	;break
      (with-result
	(define i 5)
	(do
	    (begin
	      (set! i (+ -1 i))
	      (when (= i 2)
		(break))
	      (add-result i))
	    (while (positive? i))))
    => '((4 3)))

;;; --------------------------------------------------------------------
;;; do ... until

  (check
      (with-result
	(define i 5)
	(do
	    (begin
	      (add-result i)
	      (set! i (+ -1 i)))
	    (until (zero? i))))
    => '((5 4 3 2 1)))

  (check	;continue
      (with-result
	(define i 5)
	(do
	    (begin
	      (set! i (+ -1 i))
	      (when (= i 3)
		(continue))
	      (add-result i))
	    (until (zero? i))))
    => '((4 2 1 0)))

  (check	;break
      (with-result
	(define i 5)
	(do
	    (begin
	      (set! i (+ -1 i))
	      (when (= i 2)
		(break))
	      (add-result i))
	    (until (zero? i))))
    => '((4 3)))

  #t)


(parametrise ((check-test-name	'do*))

  (check
      (with-result
	(do* ((i 5 (+ -1 i)))
	    ((zero? i)
	     'done)
	  (add-result i)))
    => '(done (5 4 3 2 1)))

  (check	;binding with no step
      (with-result
	(do* ((j 123)
	      (i 5 (+ -1 i)))
	    ((zero? i)
	     j)
	  (add-result i)))
    => '(123 (5 4 3 2 1)))

  (check	;sequential bindings
      (with-result
	(do* ((i 0 (add1 i))
	      (j (* 10 i) (* 10 i)))
	    ((= 3 i)
	     (vector i j))
	  (add-result (vector i j))))
    => '(#(3 30) (#(0 0)
		  #(1 10)
		  #(2 20))))

  (check	;break
      (with-result
	(do* ((i 0 (+ 1 i)))
	    ((= i 5)
	     i)
	  (add-result i)
	  (when (= i 3)
	    (break 123))))
    => '(123 (0 1 2 3)))

  (check	;break
      (with-result
	(do* ((i 0 (+ 1 i)))
	    ((= i 5)
	     i)
	  (add-result i)
	  (when (= i 3)
	    (break 123))))
    => '(123 (0 1 2 3)))

  (check	;continue
      (with-result
	(do* ((i 0 (+ 1 i)))
	    ((= i 5)
	     i)
	  (when (= i 3)
	    (continue))
	  (add-result i)))
    => '(5 (0 1 2 4)))

  #t)


(parametrise ((check-test-name	'dolist))

  (check
      (with-result
	(dolist (A '(1 2 3))
	  (add-result A)))
    => `((1 2 3)))

  (check
      (with-result
	(dolist (A '(1 2 3) A)
	  (add-result A)))
    => '(() (1 2 3)))

  (check
      (with-result
	(let ((rv #f))
	(dolist (A '(1 2 3) rv)
	  (add-result A)
	  (when (even? A)
	    (set! rv A)))))
    => '(2 (1 2 3)))

  (check
      (with-result
	(dolist (A '() A)
	  (add-result A)))
    => `(() ()))

  #t)


(parametrise ((check-test-name	'dotimes))

  (check
      (with-result
	(dotimes (i 3)
	  (add-result i)))
    => '((0 1 2)))

  (check
      (with-result
	(dotimes (i 3 i)
	  (add-result i)))
    => '(3 (0 1 2)))

  #t)


(parametrise ((check-test-name	'named-let))

  (check
      (with-result
	(let loop ((i 0))
	  (if (= i 5)
	      i
	    (begin
	      (add-result i)
	      (loop (+ 1 i))))))
    => '(5 (0 1 2 3 4)))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
