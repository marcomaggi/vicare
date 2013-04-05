;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the coroutines library
;;;Date: Mon Apr  1, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare coroutines)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare coroutines library\n")


(parametrise ((check-test-name	'basic))

  (check
      (with-result
       (coroutine (lambda ()
		    (add-result 1)))
       #t)
    => `(#t (1)))

  (check
      (with-result
       (coroutine (lambda ()
		    (add-result 1)))
       (finish-coroutines))
    => `(,(void) (1)))

  (check
      (with-result
       (let ()
	 (define (one)
	   (add-result '(one 1))
	   (coroutine two)
	   (add-result '(one 2))
	   (coroutine three)
	   (add-result '(one 3)))
	 (define (two)
	   (add-result '(two 1))
	   (yield)
	   (add-result '(two 2))
	   (yield)
	   (add-result '(two 3)))
	 (define (three)
	   (add-result '(three 1))
	   (yield)
	   (add-result '(three 2))
	   (yield)
	   (add-result '(three 3)))
	 (coroutine one)
	 (finish-coroutines)))
    => `(,(void)
	 ((one 1) (two 1) (one 2) (three 1) (two 2) (one 3)
	  (three 2) (two 3) (three 3))))

  (check
      (with-result
       (letrec* ((one (lambda ()
			(add-result '(one 1))
			(yield)
			(add-result '(one 2))
			(yield)
			(add-result '(one 3))))
		 (two (lambda ()
			(add-result '(two 1))
			(yield)
			(add-result '(two 2))
			(yield)
			(add-result '(two 3)))))
	 (coroutine one)
	 (coroutine two)
	 (finish-coroutines))
       #t)
    => '(#t
	 ((one 1) (two 1)
	  (one 2) (two 2)
	  (one 3) (two 3))))

  #t)


(parametrise ((check-test-name	'examples))

;;; these are examples for the documentation

  (check	;two coroutines
      (call-with-string-output-port
	  (lambda (P)
	    (parametrise ((current-output-port P))
	      (set-port-buffer-mode! (current-output-port)
				     (buffer-mode line))
	      (coroutine
		  (lambda ()
		    (display "one 1\n")
		    (yield)
		    (display "one 2\n")
		    (yield)
		    (display "one 3\n")))
	      (coroutine
		  (lambda ()
		    (display "two 1\n")
		    (yield)
		    (display "two 2\n")
		    (yield)
		    (display "two 3\n")))
	      (finish-coroutines))))
    => "one 1\ntwo 1\none 2\ntwo 2\none 3\ntwo 3\n")

  (check	;main coroutine, one coroutine
      (call-with-string-output-port
	  (lambda (P)
	    (parametrise ((current-output-port P))
	      (set-port-buffer-mode! (current-output-port)
				     (buffer-mode line))
	      (coroutine
		  (lambda ()
		    (display "sub 1\n")
		    (yield)
		    (display "sub 2\n")
		    (yield)
		    (display "sub 3\n")))
	      (display "main 1\n")
	      (yield)
	      (display "main 2\n")
	      (yield)
	      (display "main 3\n")
	      (finish-coroutines))))
    => "sub 1\nmain 1\nsub 2\nmain 2\nsub 3\nmain 3\n")

  (check	;main coroutine alone
      (call-with-string-output-port
	  (lambda (P)
	    (parametrise ((current-output-port P))
	      (set-port-buffer-mode! (current-output-port)
				     (buffer-mode line))
	      (display "main 1\n")
	      (yield)
	      (display "main 2\n")
	      (yield)
	      (display "main 3\n")
	      (finish-coroutines))))
    => "main 1\nmain 2\nmain 3\n")

  (check	;main coroutine, one finishing coroutine, longer main
      (call-with-string-output-port
	  (lambda (P)
	    (parametrise ((current-output-port P))
	      (set-port-buffer-mode! (current-output-port)
				     (buffer-mode line))
	      (coroutine
		  (lambda ()
		    (display "sub 1\n")
		    (yield)
		    (display "sub 2\n")
		    (yield)
		    (display "sub 3\n")
		    (finish-coroutines)))
	      (display "main 1\n")
	      (yield)
	      (display "main 2\n")
	      (yield)
	      (display "main 3\n")
	      (yield)
	      (display "main 4\n")
	      (yield)
	      (display "main 5\n")
	      )))
    => "sub 1\nmain 1\nsub 2\nmain 2\nsub 3\nmain 3\nmain 4\nmain 5\n")

  (check	;main coroutine, one finishing subroutine, longer subroutine
      (call-with-string-output-port
	  (lambda (P)
	    (parametrise ((current-output-port P))
	      (set-port-buffer-mode! (current-output-port)
				     (buffer-mode line))
	      (coroutine
		  (lambda ()
		    (display "sub 1\n")
		    (yield)
		    (display "sub 2\n")
		    (yield)
		    (display "sub 3\n")
		    (yield)
		    (display "sub 4\n")
		    (yield)
		    (display "sub 5\n")
		    (finish-coroutines)))
	      (display "main 1\n")
	      (yield)
	      (display "main 2\n")
	      (yield)
	      (display "main 3\n"))))
    => "sub 1\nmain 1\nsub 2\nmain 2\nsub 3\nmain 3\n")

  #t)


;;;; done

(check-report)

;;; end of file
