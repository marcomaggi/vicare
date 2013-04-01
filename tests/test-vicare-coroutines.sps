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


;;;; done

(check-report)

;;; end of file
