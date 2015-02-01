;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for the expander
;;;Date: Tue Sep 25, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013, 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(check-display "*** testing Vicare: expander, unwind-protection\n")


(parametrise ((check-test-name	'test-with-unwind-protection))

  (define-syntax with-unwind-protection
    (syntax-rules ()
      ((_ ?cleanup ?body)
       (let ((cleanup      ?cleanup)
	     (normal-exit? #f)
	     (except-exit? #f)
	     (escape-exit? #f))
	 (import (only (psyntax system $all)
		       with-escape-handler))
	 (dynamic-wind
	     (lambda ()
	       (when (or normal-exit?
			 except-exit?)
		 (non-reinstatable-violation 'with-unwind-protection
					     "attempt to reenter body")))
	     (lambda ()
	       (begin0
		   (with-exception-handler
		       (lambda (E)
			 (set! except-exit? #t)
			 (receive-and-return args
			     (raise-continuable E)
			   (set! except-exit? #f)))
		     (lambda ()
		       (with-escape-handler
			   (lambda ()
			     (set! escape-exit? #t))
			 ?body)))
		 (set! normal-exit? #t)))
	     (lambda ()
	       #;(debug-print 'out-guard normal-exit? except-exit? escape-exit?)
	       (when (or normal-exit?
			 except-exit?
			 escape-exit?)
		 (cleanup))))))
      ))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(with-unwind-protection
	    (lambda ()
	      (add-result 'out))
	  (lambda ()
	    (add-result 'in)
	    1)))
    => '(1 (in out)))

  (check	;normal exit
      (with-result
	(receive-and-return (flag)
	    #f
	  (with-unwind-protection
	      (lambda ()
		(add-result 'cleanup)
		(set! flag #t))
	    (lambda ()
	      (add-result 'body)))))
    => '(#t (body cleanup)))

  (check
      (with-result
	(with-unwind-protection
	    (lambda ()
	      (add-result 'out1)
	      (add-result 'out2))
	  (lambda ()
	    (add-result 'in)
	    1)))
    => '(1 (in out1 out2)))

  (check	;multiple return values
      (with-result
	(receive (a b)
	    (with-unwind-protection
		(lambda ()
		  (add-result 'out1)
		  (add-result 'out2))
	      (lambda ()
		(add-result 'in)
		(values 1 2)))
	  (list a b)))
    => '((1 2) (in out1 out2)))

  (check	;zero return values
      (with-result
	(with-unwind-protection
	    (lambda ()
	      (add-result 'out1)
	      (add-result 'out2))
	  (lambda ()
	    (add-result 'in)
	    (values)))
	#t)
    => `(#t (in out1 out2)))

;;; --------------------------------------------------------------------
;;; exceptions

  (check	;exception in body
      (with-result
	(guard (E (else #t))
	  (with-unwind-protection
	      (lambda ()
		(add-result 'out))
	    (lambda ()
	      (add-result 'in)
	      (error #f "fail!!!")
	      (add-result 'after)
	      1))))
    => '(#t (in out)))

  (check	;exception in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (guard (E (else
		     (void)))
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body-in)
		(raise 123)
		(add-result 'body-out))))))
    => '(#t (body-in cleanup)))

;;; --------------------------------------------------------------------
;;; non-local exit with RETURN

  (check	;return in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (returnable
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body-in)
		(return 123)
		(add-result 'body-out))))))
    => '(#t (body-in cleanup)))

  (check	;return in body, documentation example
      (internal-body
	(define y #f)
	(define x
	  (returnable
	    (with-unwind-protection
		(lambda ()
		  (set! y #t))
	      (lambda ()
		(return 1)))))
	(values x y))
    => 1 #t)

;;; --------------------------------------------------------------------
;;; non-local exit in WHILE loop

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (while #t
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body-in)
		(break)
		(add-result 'body-out))))))
    => '(#t (body-in cleanup)))

  (check	;continue in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (while (positive? i)
	      (with-unwind-protection
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'body-in)
		  (set! i (sub1 i))
		  (continue)
		  (add-result 'body-out)))))))
    => '(3 (body-in cleanup body-in cleanup body-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y #f)
	(while (positive? x)
	  (with-unwind-protection
	      (lambda ()
		(set! y #t))
	    (lambda ()
	      (-- x)
	      (break)
	      (exit))))
	(values x y))
    => 2 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y 0)
	(while (positive? x)
	  (with-unwind-protection
	      (lambda ()
		(++ y))
	    (lambda ()
	      (-- x)
	      (continue)
	      (exit))))
	(values x y))
    => 0 3)

;;; --------------------------------------------------------------------
;;; non-local exit in UNTIL loop

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (until (zero? i)
	      (with-unwind-protection
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'body-in)
		  (set! i (sub1 i))
		  (break)
		  (add-result 'body-out)))))))
    => '(1 (body-in cleanup)))

  (check	;continue in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (until (zero? i)
	      (with-unwind-protection
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'body-in)
		  (set! i (sub1 i))
		  (continue)
		  (add-result 'body-out)))))))
    => '(3 (body-in cleanup body-in cleanup body-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y #f)
	(until (zero? x)
	  (with-unwind-protection
	      (lambda ()
		(set! y #t))
	    (lambda ()
	      (-- x)
	      (break)
	      (exit))))
	(values x y))
    => 2 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y 0)
	(until (zero? x)
	  (with-unwind-protection
	      (lambda ()
		(++ y))
	    (lambda ()
	      (-- x)
	      (continue)
	      (exit))))
	(values x y))
    => 0 3)

;;; --------------------------------------------------------------------
;;; non-local exit in FOR loop

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (for ((define i 3) (positive? i) (-- i))
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body-in)
		(break)
		(add-result 'body-out))))))
    => '(#t (body-in cleanup)))

  (check	;continue in body
      (with-result
  	(receive-and-return (flag)
  	    0
  	  (for ((define i 3) (positive? i) (-- i))
  	    (with-unwind-protection
  		(lambda ()
  		  (add-result 'cleanup)
  		  (++ flag))
  	      (lambda ()
  		(add-result 'body-in)
  		(continue)
  		(add-result 'body-out))))))
    => '(3 (body-in cleanup body-in cleanup body-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
  	(define y #f)
  	(define x 3)
  	(for ((void) (positive? x) (-- x))
  	  (with-unwind-protection
  	      (lambda ()
  		(set! y #t))
  	    (lambda ()
  	      (break)
  	      (exit))))
  	(values x y))
    => 3 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
  	(define x 3)
  	(define y 0)
  	(for ((void) (positive? x) (-- x))
  	  (with-unwind-protection
  	      (lambda ()
  		(++ y))
  	    (lambda ()
  	      (continue)
  	      (exit))))
  	(values x y))
    => 0 3)

;;; --------------------------------------------------------------------
;;; non-local exit in DO ... WHILE loop

  (check	;break in body
      (with-result
	(define x 3)
	(define y #f)
	(do
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! y #t))
	      (lambda ()
		(add-result 'body-in)
		(break)
		(add-result 'body-out)))
	    (while (positive? x)))
	(values x y))
    => '(3 #t (body-in cleanup)))

  (check	;continue in body
      (with-result
	(define x 3)
	(define y 0)
	(do
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (++ y))
	      (lambda ()
		(add-result 'body-in)
		(-- x)
		(continue)
		(add-result 'body-out)))
	    (while (positive? x)))
	(values x y))
    => '(0 3 (body-in cleanup body-in cleanup body-in cleanup)))

;;; --------------------------------------------------------------------
;;; non-local exit in DO ... UNTIL loop

  (check	;break in body
      (with-result
	(define x 3)
	(define y #f)
	(do
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! y #t))
	      (lambda ()
		(add-result 'body-in)
		(break)
		(add-result 'body-out)))
	    (until (zero? x)))
	(values x y))
    => '(3 #t (body-in cleanup)))

  (check	;continue in body
      (with-result
	(define x 3)
	(define y 0)
	(do
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (++ y))
	      (lambda ()
		(add-result 'body-in)
		(-- x)
		(continue)
		(add-result 'body-out)))
	    (until (zero? x)))
	(values x y))
    => '(0 3 (body-in cleanup body-in cleanup body-in cleanup)))

;;; --------------------------------------------------------------------
;;; non-local exit in DO standard syntax

  (check 	;break in body
      (with-result
	(define y #f)
	(define x
	  (do ((x 3 (-- x)))
	      ((zero? x)
	       (add-result 'do-exit)
	       (values x y))
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup-in)
		  (set! y #t)
		  (add-result 'cleanup-out))
	      (lambda ()
		(add-result 'body-in)
		(break x)
		(add-result 'body-out)))))
	(values x y))
    => '(3 #t (body-in cleanup-in cleanup-out)))

  (check	;continue in body
      (with-result
	(do ((x 3 (-- x))
	     (y 0))
	    ((zero? x)
	     (values x y))
	  (with-unwind-protection
	      (lambda ()
		(add-result 'cleanup)
		(++ y))
	    (lambda ()
	      (add-result 'body-in)
	      (continue)
	      (add-result 'body-out)))))
    => '(0 3 (body-in cleanup body-in cleanup body-in cleanup)))

;;; --------------------------------------------------------------------
;;; exceptions from the cleanup forms

  (check
      (with-result
	(guard (E (else
		   E))
	  (with-unwind-protection
	      (lambda ()
		(add-result 'cleanup-in)
		(raise 2)
		(add-result 'cleanup-out))
	    (lambda ()
	      (add-result 'body-in)
	      (raise 1)
	      (add-result 'body-out)))))
    => '(2 (body-in cleanup-in)))

;;; --------------------------------------------------------------------
;;; continuable exceptions

  (check	;show the mechanism of continuable exceptions
      (with-exception-handler
	  (lambda (E)
	    (+ E 2))
	(lambda ()
	  (raise-continuable 1)))
    => 3)

  (check	;continuable exception from the body
      (with-result
	(guard (E ((non-continuable-violation? E)
		   2)
		  (else E))
	  (with-exception-handler
	      (lambda (E)
		(add-result 'exception-handler)
		(+ E 2))
	    (lambda ()
	      (with-unwind-protection
		  (lambda ()
		    (add-result 'cleanup))
		(lambda ()
		  (add-result 'body-in)
		  (raise-continuable 1)
		  (add-result 'body-out)
		  1))))))
    => '(1 (body-in exception-handler body-out cleanup)))

  (check	;documentation example
      (internal-body
	(define order '())
	(define (add obj)
	  (set-cons! order obj))
	(define result
	  (guard (E ((non-continuable-violation? E)
		     2)
		    (else E))
	    (with-exception-handler
		(lambda (E)
		  (add 'exception-handler)
		  (+ E 2))
	      (lambda ()
		(with-unwind-protection
		    (lambda ()
		      (add 'cleanup))
		  (lambda ()
		    (add 'body-in)
		    (raise-continuable 1)
		    (add 'body-out)
		    1))))))
	(values result (reverse order)))
    => 1 '(body-in exception-handler body-out cleanup))

;;; --------------------------------------------------------------------
;;; wrong handling of reentering full continuations

  (check	;reentering continuation
      (with-result
	(guard (E ((non-reinstatable-violation? E)
		   (add-result 'violation)
		   #t)
		  (else E))
	  (let ((rv (with-unwind-protection
			(lambda ()
			  (add-result 'cleanup))
		      (lambda ()
			(add-result 'body-in)
			(begin0
			    (call/cc values)
			  (add-result 'body-out))))))
	    (cond ((procedure? rv)
		   (add-result 'reinstating)
		   (rv 123))
		  (else
		   (add-result 'returning)
		   rv)))))
    => '(#t (body-in body-out cleanup reinstating violation)))

  (check	;documentation example
      (internal-body
	(define order '())
	(define (add obj)
	  (set-cons! order obj))

	(define rv
	  (guard (E ((non-reinstatable-violation? E)
		     (add 'violation)
		     #t)
		    (else E))
	    (let ((rv (with-unwind-protection
			  (lambda ()
			    (add 'cleanup))
			(lambda ()
			  (add 'body-in)
			  (begin0
			      (call/cc values)
			    (add 'body-out))))))
	      (cond ((procedure? rv)
		     (add 'reinstating)
		     (rv 123))
		    (else
		     (add 'returning)
		     rv)))))
	(values rv (reverse order)))
    => #t '(body-in body-out cleanup reinstating violation))

;;; --------------------------------------------------------------------

  (internal-body
    (import (vicare language-extensions coroutines))

    (define (print template . args)
      (apply fprintf (current-error-port) template args)
      (yield))

    (check
	(let ((a #f) (b #f) (c #f))
	  (parallel
	    (lambda ()
	      (unwind-protect
		  (begin
		    (set! a 1.1)
		    (print "unwind-protect sub 1.1: ~a\n" a)
		    (set! a 1.2)
		    (print "unwind-protect sub 1.2: ~a\n" a)
		    (set! a 1.3)
		    (print "unwind-protect sub 1.3: ~a\n" a))
		(set! a 1.4)))
	    (lambda ()
	      (unwind-protect
		  (begin
		    (set! b 2.1)
		    (print "unwind-protect sub 2.1: ~a\n" b)
		    (set! b 2.2)
		    (print "unwind-protect sub 2.2: ~a\n" b)
		    (set! b 2.3)
		    (print "unwind-protect sub 2.3: ~a\n" b))
		(set! b 2.4)))
	    (lambda ()
	      (unwind-protect
		  (begin
		    (set! c 3.1)
		    (print "unwind-protect sub 3.1: ~a\n" c)
		    (set! c 3.2)
		    (print "unwind-protect sub 3.2: ~a\n" c)
		    (set! c 3.3)
		    (print "unwind-protect sub 3.3: ~a\n" c))
		(set! c 3.4))))
	  (values a b c))
      => 1.4 2.4 3.4))

  #t)


(parametrise ((check-test-name	'with-unwind-protection))

  (check
      (with-result
	(with-unwind-protection
	    (lambda ()
	      (add-result 'out))
	  (lambda ()
	    (add-result 'in)
	    1)))
    => '(1 (in out)))

  (check	;normal exit
      (with-result
	(receive-and-return (flag)
	    #f
	  (with-unwind-protection
	      (lambda ()
		(add-result 'cleanup)
		(set! flag #t))
	    (lambda ()
	      (add-result 'body)))))
    => '(#t (body cleanup)))

  (check
      (with-result
	(with-unwind-protection
	    (lambda ()
	      (add-result 'out1)
	      (add-result 'out2))
	  (lambda ()
	    (add-result 'in)
	    1)))
    => '(1 (in out1 out2)))

  (check	;multiple return values
      (with-result
	(receive (a b)
	    (with-unwind-protection
		(lambda ()
		  (add-result 'out1)
		  (add-result 'out2))
	      (lambda ()
		(add-result 'in)
		(values 1 2)))
	  (list a b)))
    => '((1 2) (in out1 out2)))

  (check	;zero return values
      (with-result
	(with-unwind-protection
	    (lambda ()
	      (add-result 'out1)
	      (add-result 'out2))
	  (lambda ()
	    (add-result 'in)
	    (values)))
	#t)
    => `(#t (in out1 out2)))

;;; --------------------------------------------------------------------
;;; exceptions

  (check	;exception in body
      (with-result
	(guard (E (else #t))
	  (with-unwind-protection
	      (lambda ()
		(add-result 'out))
	    (lambda ()
	      (add-result 'in)
	      (error #f "fail!!!")
	      (add-result 'after)
	      1))))
    => '(#t (in out)))

  (check	;exception in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (guard (E (else
		     (void)))
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body-in)
		(raise 123)
		(add-result 'body-out))))))
    => '(#t (body-in cleanup)))

;;; --------------------------------------------------------------------
;;; non-local exit with RETURN

  (check	;return in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (returnable
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body-in)
		(return 123)
		(add-result 'body-out))))))
    => '(#t (body-in cleanup)))

  (check	;return in body, documentation example
      (internal-body
	(define y #f)
	(define x
	  (returnable
	    (with-unwind-protection
		(lambda ()
		  (set! y #t))
	      (lambda ()
		(return 1)))))
	(values x y))
    => 1 #t)

;;; --------------------------------------------------------------------
;;; non-local exit in WHILE loop

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (while #t
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body-in)
		(break)
		(add-result 'body-out))))))
    => '(#t (body-in cleanup)))

  (check	;continue in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (while (positive? i)
	      (with-unwind-protection
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'body-in)
		  (set! i (sub1 i))
		  (continue)
		  (add-result 'body-out)))))))
    => '(3 (body-in cleanup body-in cleanup body-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y #f)
	(while (positive? x)
	  (with-unwind-protection
	      (lambda ()
		(set! y #t))
	    (lambda ()
	      (-- x)
	      (break)
	      (exit))))
	(values x y))
    => 2 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y 0)
	(while (positive? x)
	  (with-unwind-protection
	      (lambda ()
		(++ y))
	    (lambda ()
	      (-- x)
	      (continue)
	      (exit))))
	(values x y))
    => 0 3)

;;; --------------------------------------------------------------------
;;; non-local exit in UNTIL loop

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (until (zero? i)
	      (with-unwind-protection
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'body-in)
		  (set! i (sub1 i))
		  (break)
		  (add-result 'body-out)))))))
    => '(1 (body-in cleanup)))

  (check	;continue in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (until (zero? i)
	      (with-unwind-protection
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'body-in)
		  (set! i (sub1 i))
		  (continue)
		  (add-result 'body-out)))))))
    => '(3 (body-in cleanup body-in cleanup body-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y #f)
	(until (zero? x)
	  (with-unwind-protection
	      (lambda ()
		(set! y #t))
	    (lambda ()
	      (-- x)
	      (break)
	      (exit))))
	(values x y))
    => 2 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y 0)
	(until (zero? x)
	  (with-unwind-protection
	      (lambda ()
		(++ y))
	    (lambda ()
	      (-- x)
	      (continue)
	      (exit))))
	(values x y))
    => 0 3)

;;; --------------------------------------------------------------------
;;; non-local exit in FOR loop

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (for ((define i 3) (positive? i) (-- i))
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body-in)
		(break)
		(add-result 'body-out))))))
    => '(#t (body-in cleanup)))

  (check	;continue in body
      (with-result
  	(receive-and-return (flag)
  	    0
  	  (for ((define i 3) (positive? i) (-- i))
  	    (with-unwind-protection
  		(lambda ()
  		  (add-result 'cleanup)
  		  (++ flag))
  	      (lambda ()
  		(add-result 'body-in)
  		(continue)
  		(add-result 'body-out))))))
    => '(3 (body-in cleanup body-in cleanup body-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
  	(define y #f)
  	(define x 3)
  	(for ((void) (positive? x) (-- x))
  	  (with-unwind-protection
  	      (lambda ()
  		(set! y #t))
  	    (lambda ()
  	      (break)
  	      (exit))))
  	(values x y))
    => 3 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
  	(define x 3)
  	(define y 0)
  	(for ((void) (positive? x) (-- x))
  	  (with-unwind-protection
  	      (lambda ()
  		(++ y))
  	    (lambda ()
  	      (continue)
  	      (exit))))
  	(values x y))
    => 0 3)

;;; --------------------------------------------------------------------
;;; non-local exit in DO ... WHILE loop

  (check	;break in body
      (with-result
	(define x 3)
	(define y #f)
	(do
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! y #t))
	      (lambda ()
		(add-result 'body-in)
		(break)
		(add-result 'body-out)))
	    (while (positive? x)))
	(values x y))
    => '(3 #t (body-in cleanup)))

  (check	;continue in body
      (with-result
	(define x 3)
	(define y 0)
	(do
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (++ y))
	      (lambda ()
		(add-result 'body-in)
		(-- x)
		(continue)
		(add-result 'body-out)))
	    (while (positive? x)))
	(values x y))
    => '(0 3 (body-in cleanup body-in cleanup body-in cleanup)))

;;; --------------------------------------------------------------------
;;; non-local exit in DO ... UNTIL loop

  (check	;break in body
      (with-result
	(define x 3)
	(define y #f)
	(do
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (set! y #t))
	      (lambda ()
		(add-result 'body-in)
		(break)
		(add-result 'body-out)))
	    (until (zero? x)))
	(values x y))
    => '(3 #t (body-in cleanup)))

  (check	;continue in body
      (with-result
	(define x 3)
	(define y 0)
	(do
	    (with-unwind-protection
		(lambda ()
		  (add-result 'cleanup)
		  (++ y))
	      (lambda ()
		(add-result 'body-in)
		(-- x)
		(continue)
		(add-result 'body-out)))
	    (until (zero? x)))
	(values x y))
    => '(0 3 (body-in cleanup body-in cleanup body-in cleanup)))

;;; --------------------------------------------------------------------
;;; non-local exit in DO standard syntax

  (check	;break in body
      (with-result
	(do ((x 3 (-- x))
	     (y #f))
	    ((zero? x)
	     (values x y))
	  (with-unwind-protection
	      (lambda ()
		(add-result 'cleanup)
		(set! y #t))
	    (lambda ()
	      (add-result 'body-in)
	      (break x y)
	      (add-result 'body-out)))))
    => '(3 #t (body-in cleanup)))

  (check	;continue in body
      (with-result
	(do ((x 3 (-- x))
	     (y 0))
	    ((zero? x)
	     (values x y))
	  (with-unwind-protection
	      (lambda ()
		(add-result 'cleanup)
		(++ y))
	    (lambda ()
	      (add-result 'body-in)
	      (continue)
	      (add-result 'body-out)))))
    => '(0 3 (body-in cleanup body-in cleanup body-in cleanup)))

;;; --------------------------------------------------------------------
;;; exceptions from the cleanup forms

  (check
      (with-result
	(guard (E (else
		   E))
	  (with-unwind-protection
	      (lambda ()
		(add-result 'cleanup-in)
		(raise 2)
		(add-result 'cleanup-out))
	    (lambda ()
	      (add-result 'body-in)
	      (raise 1)
	      (add-result 'body-out)))))
    => '(2 (body-in cleanup-in)))

;;; --------------------------------------------------------------------
;;; continuable exceptions

  (check	;show the mechanism of continuable exceptions
      (with-exception-handler
	  (lambda (E)
	    (+ E 2))
	(lambda ()
	  (raise-continuable 1)))
    => 3)

  (check	;continuable exception from the body
      (with-result
	(guard (E ((non-continuable-violation? E)
		   #t)
		  (else #f))
	  (with-exception-handler
	      (lambda (E)
		(add-result 'exception-handler)
		(+ E 2))
	    (lambda ()
	      (with-unwind-protection
		  (lambda ()
		    (add-result 'cleanup))
		(lambda ()
		  (add-result 'body-in)
		  (raise-continuable 1)
		  (add-result 'body-out)))))))
    => '(#t (body-in cleanup exception-handler)))

  (check	;documentation example
      (internal-body
	(define order '())
	(define (add obj)
	  (set-cons! order obj))
	(define result
	  (guard (E ((non-continuable-violation? E)
		     #t)
		    (else #f))
	    (with-exception-handler
		(lambda (E)
		  (add 'exception-handler)
		  (+ E 2))
	      (lambda ()
		(with-unwind-protection
		    (lambda ()
		      (add 'cleanup))
		  (lambda ()
		    (add 'body-in)
		    (raise-continuable 1)
		    (add 'body-out)))))))
	(values result (reverse order)))
    => #t '(body-in cleanup exception-handler))

;;; --------------------------------------------------------------------
;;; wrong handling of reentering full continuations

  (check	;reentering continuation
      (with-result
	(define rv
	  (with-unwind-protection
	      (lambda ()
		(add-result 'cleanup))
	    (lambda ()
	      (add-result 'body-in)
	      (begin0
		  (call/cc values)
		(add-result 'body-out)))))
	(if (procedure? rv)
	    (rv 123)
	  rv))
    => '(123 (body-in body-out cleanup body-out cleanup)))

  (check	;documentation example
      (internal-body
	(define order '())
	(define (add obj)
	  (set-cons! order obj))
	(define rv
	  (with-unwind-protection
	      (lambda ()
		(add 'cleanup))
	    (lambda ()
	      (add 'body-in)
	      (begin0
		  (call/cc values)
		(add 'body-out)))))
	(if (procedure? rv)
	    (rv 123)
	  (values rv (reverse order))))
    => 123 '(body-in body-out cleanup body-out cleanup))

  #t)


(parametrise ((check-test-name	'unwind-protect))

  (check
      (with-result
	(unwind-protect
	    (begin
	      (add-result 'in)
	      1)
	  (add-result 'out)))
    => '(1 (in out)))

  (check
      (with-result
	(unwind-protect
	    (begin
	      (add-result 'in)
	      1)
	  (add-result 'out1)
	  (add-result 'out2)))
    => '(1 (in out1 out2)))

  (check	;multiple return values
      (with-result
	(receive (a b)
	    (unwind-protect
		(begin
		  (add-result 'in)
		  (values 1 2))
	      (add-result 'out1)
	      (add-result 'out2))
	  (list a b)))
    => '((1 2) (in out1 out2)))

  (check	;zero return values
      (with-result
	(unwind-protect
	    (begin
	      (add-result 'in)
	      (values))
	  (add-result 'out1)
	  (add-result 'out2))
	#t)
    => `(#t (in out1 out2)))

  (check	;exception in body
      (with-result
	(guard (E (else #t))
	  (unwind-protect
	      (begin
		(add-result 'in)
		(error #f "fail!!!")
		(add-result 'after)
		1)
	    (add-result 'out))))
    => '(#t (in out)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; End:
