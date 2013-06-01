;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for compensation stacks
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;	This test file was originally part of Nausicaa/Scheme.
;;;
;;;Copyright (c) 2008-2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (rnrs eval)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare compensations\n")


;;;; helpers

(define-syntax catch-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else exc))
       ?form0 ?form ...))))


(parametrise ((check-test-name	'basic))

  (check
      (with-result
       (with-compensations
	 (compensate
	     (add-result 'alloc)
	   (with
	    (add-result 'free)))
	 #t))
    => `(#t (alloc free)))

  #t)


(parametrise ((check-test-name	'on-error-no-error))

;;;Test WITH-COMPENSATIONS/ON-ERROR when no error is raised.

  (check	;no COMPENSATE forms
      (with-result
       (with-compensations/on-error
	 (add-result 0)
	 1))
    => '(1 (0)))

  (check
      (with-result
       (with-compensations/on-error
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
	 (add-result 0)
	 4))
    => '(4 (1 2 3 0)))


  (check	;no error, explicit compensations invocation
      (with-result
       (with-compensations/on-error
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
	 (add-result 0)
	 (run-compensations))
       4)
    => '(4 (1 2 3 0 -3 -2 -1)))

  #t)


(parametrise ((check-test-name	'on-return-no-error))

;;;Test WITH-COMPENSATIONS when no error is raised.

  (check	;no COMPENSATE forms
      (with-result
       (with-compensations
	 (add-result 0)
	 1))
    => '(1 (0)))

  (check
      (with-result
       (with-compensations
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
	 (add-result 0)
	 4))
    => '(4 (1 2 3 0 -3 -2 -1)))


  (check	;explicit compensations invocation
      (with-result
       (with-compensations
	 (compensate (add-result 1) (with (add-result -1)))
	 (compensate (add-result 2) (with (add-result -2)))
	 (compensate (add-result 3) (with (add-result -3)))
	 (add-result 0)
	 (run-compensations)
	 4))
    => '(4 (1 2 3 0 -3 -2 -1)))

  #t)


(parametrise ((check-test-name	'on-error-with-error))

;;;Test WITH-COMPENSATIONS/ON-ERROR when an error is raised in the body.

  (check	;after the COMPENSATE forms
      (with-result
       (catch-exception
	(with-compensations/on-error
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	  (add-result 4)
	  (raise 'misc-error)
	  (add-result 5)
	  0)))
    => '(misc-error (1 2 3 4 -3 -2 -1)))

  (check	;between the COMPENSATE forms
      (with-result
       (catch-exception
	(with-compensations/on-error
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (add-result 4)
	  (raise 'misc-error)
	  (add-result 5)
	  (compensate (add-result 3) (with (add-result -3)))
	  0)))
    => '(misc-error (1 2 4 -2 -1)))

  (check	;before the COMPENSATE forms
      (with-result
       (catch-exception
	(with-compensations/on-error
	  (add-result 4)
	  (raise 'misc-error)
	  (add-result 5)
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	  0)))
    => '(misc-error (4)))

  #t)


(parametrise ((check-test-name	'on-return-with-error))

;;;Test WITH-COMPENSATIONS when an error is raised in the body.

  (check	;after the COMPENSATE forms
      (with-result
       (catch-exception
	(with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	  (add-result 4)
	  (raise 'misc-error)
	  (add-result 5)
	  0)))
    => '(misc-error (1 2 3 4 -3 -2 -1)))

  (check	;between the COMPENSATE forms
      (with-result
       (catch-exception
	(with-compensations
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (add-result 4)
	  (raise 'misc-error)
	  (add-result 5)
	  (compensate (add-result 3) (with (add-result -3)))
	  0)))
    => '(misc-error (1 2 4 -2 -1)))

  (check	;before the COMPENSATE forms
      (with-result
       (catch-exception
	(with-compensations
	  (add-result 4)
	  (raise 'misc-error)
	  (add-result 5)
	  (compensate (add-result 1) (with (add-result -1)))
	  (compensate (add-result 2) (with (add-result -2)))
	  (compensate (add-result 3) (with (add-result -3)))
	  0)))
    => '(misc-error (4)))

  #t)


(parametrise ((check-test-name	'allocation-form-error))

;;;Test WITH-COMPENSATIONS and WITH-COMPENSATIONS/ON-ERROR when an error
;;;is raised in an allocation form.

  (check
      (with-result
       (catch-exception
	(with-compensations/on-error
	  (compensate
	      (add-result 1)
	    (with
	     (add-result -1)))
	  (compensate
	      (add-result 2.1)
	      (raise 'alloc-error)
	      (add-result 2.2)
	    (with
	     (add-result -2)))
	  (compensate
	      (add-result 3)
	    (with
	     (add-result -3)))
	  (add-result 5)
	  0)))
    => '(alloc-error (1 2.1 -1)))

  (check
      (with-result
       (catch-exception
	(with-compensations
	  (compensate
	      (add-result 1)
	    (with
	     (add-result -1)))
	  (compensate
	      (add-result 2.1)
	      (raise 'alloc-error)
	      (add-result 2.2)
	    (with
	     (add-result -2)))
	  (compensate
	      (add-result 3)
	    (with
	     (add-result -3)))
	  (add-result 5)
	  0)))
    => '(alloc-error (1 2.1 -1)))

  #t)


(parametrise ((check-test-name	 'release-form-error))

;;;Test WITH-COMPENSATIONS/ON-ERROR when an error is raised in a release
;;;form.

  (check	;no error raised in the body
      (with-result
       (catch-exception
	(with-compensations/on-error
	  (compensate
	      (add-result 1)
	    (with
	     (add-result -1)))
	  (compensate
	      (add-result 2)
	    (with
	     (add-result -2.1)
	     (raise 'release-error)
	     (add-result -2.2)))
	  (compensate
	      (add-result 3)
	    (with
	     (add-result -3)))
	  (add-result 5)
	  0)))
    => '(0 (1 2 3 5)))

  (check	;error raised in the body
      (with-result
       (catch-exception
	(with-compensations/on-error
	  (compensate
	      (add-result 1)
	    (with
	     (add-result -1)))
	  (compensate
	      (add-result 2)
	    (with
	     (add-result -2.1)
	     (raise 'release-error)
	     (add-result -2.2)))
	  (compensate
	      (add-result 3)
	    (with
	     (add-result -3)))
	  (add-result 5)
	  (raise 'body-error)
	  (add-result 6)
	  0)))
    => '(body-error (1 2 3 5 -3 -2.1 -1)))

;;; --------------------------------------------------------------------

;;;Test WITH-COMPENSATIONS when an error is raised in a release form.

  (check	;no error raised in the body
      (with-result
       (catch-exception
	(with-compensations
	  (compensate
	      (add-result 1)
	    (with (add-result -1)))
	  (compensate
	      (add-result 2)
	    (with
	     (add-result -2.1)
	     (raise 'release-error)
	     (add-result -2.2)))
	  (compensate
	      (add-result 3)
	    (with
	     (add-result -3)))
	  (add-result 5)
	  0)))
    => '(0 (1 2 3 5 -3 -2.1 -1)))

  (check
      (with-result
       (catch-exception
	(with-compensations
	  (compensate
	      (add-result 1)
	    (with
	     (add-result -1)))
	  (compensate
	      (add-result 2)
	    (with
	     (add-result -2.1)
	     (raise 'release-error)
	     (add-result -2.2)))
	  (compensate
	      (add-result 3)
	    (with
	     (add-result -3)))
	  (add-result 5)
	  (raise 'body-error)
	  (add-result 6)
	  0)))
    => '(body-error (1 2 3 5 -3 -2.1 -1)))

  #t)


(parametrise ((check-test-name	'resources))

  (check	;with DEFINE
      (with-result
       (with-compensations
	 (define item
	   (compensate
	       123
	     (with
	      (add-result item))))
	 (add-result 1)
	 0))
    => '(0 (1 123)))

  (check	;with LETREC
      (with-result
       (with-compensations
	 (letrec
	     ((item (compensate
			123
		      (with
		       (add-result item)))))
	   (add-result 1)
	   0)))
    => '(0 (1 123)))

  (check	;with LETREC*
      (with-result
       (with-compensations
	 (letrec*
	     ((item1 (compensate
			 123
		       (with
			(add-result item2))))
	      (item2 (compensate
			 456
		       (with
			(add-result item1)))))
	   (add-result 1)
	   0)))
    => '(0 (1 123 456)))

;;; --------------------------------------------------------------------

  (check	;with DEFINE
      (with-result
       (with-compensations/on-error
	 (define item
	   (compensate
	       123
	     (with
	      (add-result item))))
	 (add-result 1)
	 0))
    => '(0 (1)))

  (check	;with LETREC
      (with-result
       (with-compensations/on-error
	 (letrec
	     ((item (compensate
			123
		      (with
		       (add-result item)))))
	   (add-result 1)
	   0)))
    => '(0 (1)))

  (check	;with LETREC*
      (with-result
       (with-compensations/on-error
	 (letrec*
	     ((item1 (compensate
			 123
		       (with
			(add-result item2))))
	      (item2 (compensate
			 456
		       (with
			(add-result item1)))))
	   (add-result 1)
	   0)))
    => '(0 (1)))

  #t)


(parametrise ((check-test-name	'nesting))

  (check	;double nesting, no error raised
      (with-result
       (with-compensations
	 (compensate
	     (add-result 1.1)
	   (with
	    (add-result 1.2)))
	 (add-result 1.3)
	 (with-compensations
	   (compensate
	       (add-result 2.1)
	     (with
	      (add-result 2.2)))
	   (add-result 2.3)
	   2.4)
	 1.4))
    => '(1.4 (1.1 1.3 2.1 2.3 2.2 1.2)))

  (check	;triple nesting, no error raised
      (with-result
       (with-compensations
	 (compensate
	     (add-result 1.1)
	   (with
	    (add-result 1.2)))
	 (add-result 1.3)
	 (with-compensations
	   (compensate
	       (add-result 2.1)
	     (with
	      (add-result 2.2)))
	   (add-result 2.3)
	   2.4)
	 (with-compensations
	   (compensate
	       (add-result 3.1)
	     (with
	      (add-result 3.2)))
	   (add-result 3.3)
	   3.4)
	 1.4))
    => '(1.4 (1.1 1.3
		  2.1 2.3 2.2
		  3.1 3.3 3.2
		  1.2)))

;;; --------------------------------------------------------------------

  (check	;double nesting, error raised in nested body
      (with-result
       (catch-exception
	(with-compensations
	  (compensate
	      (add-result 1.1)
	    (with
	     (add-result 1.2)))
	  (add-result 2)
	  (with-compensations
	    (compensate
		(add-result 3.1)
	      (with
	       (add-result 3.2)))
	    (add-result 4)
	    (raise 'inner-body))
	  1.4)))
    => '(inner-body (1.1 2 3.1 4 3.2 1.2)))

  #t)


(parametrise ((check-test-name	'syntax-errors))

  (check	;missing WITH
      (guard (E ((syntax-violation? E)
		 #;(check-pretty-print (condition-message E))
		 (syntax->datum (syntax-violation-subform E))))
	(eval '(with-compensations
		 (letrec ((a (compensate 123)))
		   #t))
	      (environment '(vicare))))
    => '())

  (check	;empty WITH
      (guard (E ((syntax-violation? E)
		 #;(check-pretty-print (condition-message E))
		 (syntax->datum (syntax-violation-subform E))))
	(eval '(with-compensations
		 (letrec ((a (compensate 123 (with))))
		   #t))
	      (environment '(vicare))))
    => '(with))

  #t)


;;;; done

(check-report)

;;; end of file
