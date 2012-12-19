;;;
;;;Part of: Vicare Scheme
;;;Contents: test for SRFI 37, args-fold
;;;Date: Wed Dec 19, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (srfi :37)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 37, args-fold\n")


(parametrise ((check-test-name	'short-options-no-arg))

  (define (unrecognised-option-proc option name arg seed)
    (error 'test "unknown option" option name arg))

  (define (make-seed)
    (make-vector 3 '()))

  (define (make-option-processor index)
    (lambda (option name arg seed)
      (vector-set! seed index (cons name (vector-ref seed index)))
      seed))

  (define (make-operand-processor index)
    (lambda (operand seed)
      (vector-set! seed index (cons operand (vector-ref seed index)))
      seed))

;;; --------------------------------------------------------------------

  (check
      (args-fold '("-a" "-b" "ciao")
		 (list (option '(#\a) #f #f (make-option-processor 1))
		       (option '(#\b) #f #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(("ciao")
	  (#\a) (#\b)))

  (check
      (args-fold '("salut" "-a" "hello" "-b" "ciao")
		 (list (option '(#\a) #f #f (make-option-processor 1))
		       (option '(#\b) #f #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(("ciao" "hello" "salut")
	  (#\a) (#\b)))

  (check
      (args-fold '("-ab")
		 (list (option '(#\a) #f #f (make-option-processor 1))
		       (option '(#\b) #f #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(()
	  (#\a) (#\b)))

  #t)


(parametrise ((check-test-name	'short-options-required-arg))

  (define (unrecognised-option-proc option name arg seed)
    (error 'test "unknown option" option name arg))

  (define (make-seed)
    (make-vector 3 '()))

  (define (make-option-processor index)
    (lambda (option name arg seed)
      (vector-set! seed index (cons arg (vector-ref seed index)))
      seed))

  (define (make-operand-processor index)
    (lambda (operand seed)
      (vector-set! seed index (cons operand (vector-ref seed index)))
      seed))

;;; --------------------------------------------------------------------

  (check
      (args-fold '("-a" "hello" "-b" "ciao")
		 (list (option '(#\a) #t #f (make-option-processor 1))
		       (option '(#\b) #t #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(()
	  ("hello") ("ciao")))

  (check
      (args-fold '("-ahello" "-bciao")
		 (list (option '(#\a) #t #f (make-option-processor 1))
		       (option '(#\b) #t #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(()
	  ("hello") ("ciao")))

  (check
      (args-fold '("salut" "-a" "hello" "-b" "ciao")
		 (list (option '(#\a) #t #f (make-option-processor 1))
		       (option '(#\b) #t #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(("salut")
	  ("hello") ("ciao")))

  (check
      (args-fold '("-ahello" "-bciao")
		 (list (option '(#\a) #t #f (make-option-processor 1))
		       (option '(#\b) #t #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(()
	  ("hello") ("ciao")))

  #t)


(parametrise ((check-test-name	'short-options-optional-arg))

  (define (unrecognised-option-proc option name arg seed)
    (error 'test "unknown option" option name arg))

  (define (make-seed)
    (make-vector 3 '()))

  (define (make-option-processor index)
    (lambda (option name arg seed)
      (vector-set! seed index (cons arg (vector-ref seed index)))
      seed))

  (define (make-operand-processor index)
    (lambda (operand seed)
      (vector-set! seed index (cons operand (vector-ref seed index)))
      seed))

;;; --------------------------------------------------------------------

  (check
      (args-fold '("-ahello" "-b")
		 (list (option '(#\a) #f #t (make-option-processor 1))
		       (option '(#\b) #f #t (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(()
	  ("hello") (#f)))

  #t)


(parametrise ((check-test-name	'long-options-no-arg))

  (define (unrecognised-option-proc option name arg seed)
    (error 'test "unknown option" option name arg))

  (define (make-seed)
    (make-vector 3 '()))

  (define (make-option-processor index)
    (lambda (option name arg seed)
      (vector-set! seed index (cons name (vector-ref seed index)))
      seed))

  (define (make-operand-processor index)
    (lambda (operand seed)
      (vector-set! seed index (cons operand (vector-ref seed index)))
      seed))

;;; --------------------------------------------------------------------

  (check
      (args-fold '("--alpha" "--beta" "ciao")
		 (list (option '("alpha") #f #f (make-option-processor 1))
		       (option '("beta")  #f #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(("ciao")
	  ("alpha") ("beta")))

  (check
      (args-fold '("salut" "--alpha" "hello" "--beta" "ciao")
		 (list (option '("alpha") #f #f (make-option-processor 1))
		       (option '("beta")  #f #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(("ciao" "hello" "salut")
	  ("alpha") ("beta")))

  #t)


(parametrise ((check-test-name	'long-options-required-arg))

  (define (unrecognised-option-proc option name arg seed)
    (error 'test "unknown option" option name arg))

  (define (make-seed)
    (make-vector 3 '()))

  (define (make-option-processor index)
    (lambda (option name arg seed)
      (vector-set! seed index (cons arg (vector-ref seed index)))
      seed))

  (define (make-operand-processor index)
    (lambda (operand seed)
      (vector-set! seed index (cons operand (vector-ref seed index)))
      seed))

;;; --------------------------------------------------------------------

  (check
      (args-fold '("--alpha" "hello" "--beta" "ciao")
		 (list (option '("alpha") #t #f (make-option-processor 1))
		       (option '("beta") #t #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(()
	  ("hello") ("ciao")))

  (check
      (args-fold '("salut" "--alpha" "hello" "--beta" "ciao")
		 (list (option '("alpha") #t #f (make-option-processor 1))
		       (option '("beta") #t #f (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(("salut")
	  ("hello") ("ciao")))

  #t)


(parametrise ((check-test-name	'long-options-optional-arg))

  (define (unrecognised-option-proc option name arg seed)
    (error 'test "unknown option" option name arg))

  (define (make-seed)
    (make-vector 3 '()))

  (define (make-option-processor index)
    (lambda (option name arg seed)
      (vector-set! seed index (cons arg (vector-ref seed index)))
      seed))

  (define (make-operand-processor index)
    (lambda (operand seed)
      (vector-set! seed index (cons operand (vector-ref seed index)))
      seed))

;;; --------------------------------------------------------------------

  (check
      (args-fold '("--alpha=hello" "--beta")
		 (list (option '("alpha") #f #t (make-option-processor 1))
		       (option '("beta")  #f #t (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(()
	  ("hello") (#f)))

  (check
      (args-fold '("--alpha=hello" "--beta=ciao")
		 (list (option '("alpha") #f #t (make-option-processor 1))
		       (option '("beta")  #f #t (make-option-processor 2)))
		 unrecognised-option-proc
		 (make-operand-processor 0)
		 (make-seed))
    => '#(()
	  ("hello") ("ciao")))

  #t)


;;;; done

(check-report)

;;; end of file
