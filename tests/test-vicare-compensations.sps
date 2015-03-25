;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for compensation stacks
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;	This test file was originally part of Nausicaa/Scheme.
;;;
;;;Copyright (c) 2008-2010, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(check-display "*** testing Vicare compensations\n")


;;;; helpers

(define-syntax catch-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else exc))
       ?form0 ?form ...))))


(parametrise ((check-test-name	'basic))

  (check	;normal exit
      (with-result
	(with-compensations
	  (compensate
	      (add-result 'alloc)
	    (with
	     (add-result 'free)))
	  #t))
    => `(#t (alloc free)))

  (check	;normal exit
      (with-result
	(with-compensations
	  (with-compensation-handler
	      (lambda ()
		(add-result 'free))
	    (lambda ()
	      (add-result 'alloc)
	      #t))))
    => `(#t (alloc free)))

  (check	;normal exit
      (with-result
	(with-compensations
	  (with-compensation-handler
	      (lambda ()
		(add-result 'out))
	    (lambda ()
	      (add-result 'in)
	      1))))
    => '(1 (in out)))

  (check	;normal exit
      (with-result
	(receive-and-return (flag)
	    #f
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'body))))))
    => '(#t (body cleanup)))

  (check	;multiple return values
      (with-result
	(receive (a b)
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'out))
		(lambda ()
		  (add-result 'in)
		  (values 1 2))))
	  (list a b)))
    => '((1 2) (in out)))

  (check	;zero return values
      (with-result
	(with-compensations
	  (with-compensation-handler
	      (lambda ()
		(add-result 'out))
	    (lambda ()
	      (add-result 'in)
	      (values))))
	#t)
    => `(#t (in out)))
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


(parametrise ((check-test-name	'non-continuable-exceptions-from-thunk))

  (check	;show the mechanism of non-continuable exceptions
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		(lambda (E)
		  (escape E))
	      (lambda ()
		(raise 1)))))
    => 1)

  (check	;show the mechanism of non-continuable exceptions
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		escape
	      (lambda ()
		(with-exception-handler
		    (lambda (E)
		      (raise E))
		  (lambda ()
		    (raise 1)))))))
    => 1)

;;; --------------------------------------------------------------------

  (check 	;exception in body, GUARD's ELSE clause
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup))
	      (lambda ()
		(add-result 'thunk-in)
		(raise 2)
		(add-result 'thunk-out)
		1)))))
    => '(2 (thunk-in cleanup guard-else)))

  (check 	;exception in body, GUARD's clause
      (with-result
	(guard (E ((begin
		     (add-result 'guard-test)
		     #t)
		   (add-result 'guard-expr)
		   E))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup))
	      (lambda ()
		(add-result 'thunk-in)
		(raise 2)
		(add-result 'thunk-out)
		1)))))
    => '(2 (thunk-in guard-test cleanup guard-expr)))

  (check  	;exception in body, nested GUARD uses, nested DYNAMIC-WIND
      (with-result
	(guard (E ((begin
		     (add-result 'guard-outer-test)
		     #t)
		   (add-result 'guard-outer-expr)
		   E))
	  (guard (E ((begin
		       (add-result 'guard-inner-test)
		       #f)
		     (add-result 'guard-inner-expr)
		     E))
	    (dynamic-wind
		(lambda ()
		  (add-result 'outer-before))
		(lambda ()
		  (with-compensations
		    (with-compensation-handler
			(lambda ()
			  (add-result 'cleanup))
		      (lambda ()
			(dynamic-wind
			    (lambda ()
			      (add-result 'inner-before))
			    (lambda ()
			      (add-result 'thunk-in)
			      (raise 2)
			      (add-result 'thunk-out)
			      1)
			    (lambda ()
			      (add-result 'inner-after)))))))
		(lambda ()
		  (add-result 'outer-after))))))
    => '(2 (outer-before inner-before thunk-in inner-after outer-after
			 guard-inner-test
			 outer-before inner-before inner-after outer-after
			 guard-outer-test
			 outer-before inner-before inner-after cleanup outer-after
			 guard-outer-expr)))

;;; --------------------------------------------------------------------
;;; exiting the dynamic extent of ?THUNK

  ;;Exit the dynamic  extent of ?THUNK by reinstating an  escape continuation from an
  ;;exception handler.  The ?CLEANUP is not called.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'exception-handler)
		    (escape E))
		(lambda ()
		  (with-compensations
		    (with-compensation-handler
			(lambda ()
			  (add-result 'cleanup))
		      (lambda ()
			(add-result 'thunk-in)
			(raise 123)
			(add-result 'thunk-out)))))))))
    => '(123 (thunk-in exception-handler)))

  ;;Exit the dynamic  extent of ?THUNK by reinstating an  escape continuation from an
  ;;exception handler.  The ?CLEANUP is not called.
  ;;
  (check
      (with-result
	(receive (rv)
	    (call/cc
		(lambda (escape)
		  (with-exception-handler
		      (lambda (E)
			(add-result 'exception-handler)
			(escape (lambda ()
				  (add-result 'after-escape)
				  E)))
		    (lambda ()
		      (with-compensations
			(with-compensation-handler
			    (lambda ()
			      (add-result 'cleanup))
			  (lambda ()
			    (add-result 'thunk-in)
			    (raise 123)
			    (add-result 'thunk-out))))))))
	  (rv)))
    => '(123 (thunk-in exception-handler after-escape)))

  ;;Exit the dynamic extent of ?THUNK  by reinstating an escape continuation from the
  ;;internals of a GUARD clause.
  ;;
  (check
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup))
	      (lambda ()
		(add-result 'thunk-in)
		(raise 123)
		(add-result 'thunk-out))))))
    => '(123 (thunk-in cleanup guard-else)))

;;; --------------------------------------------------------------------
;;; raising non-continuable exceptions and DYNAMIC-WIND

  ;;The ?CLEANUP is not called.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'exception-handler)
		    (escape E))
		(lambda ()
		  (with-compensations
		    (with-compensation-handler
			(lambda ()
			  (add-result 'cleanup))
		      (lambda ()
			(dynamic-wind
			    (lambda ()
			      (add-result 'in-guard))
			    (lambda ()
			      (add-result 'thunk-in)
			      (raise 123)
			      (add-result 'thunk-out))
			    (lambda ()
			      (add-result 'out-guard)))))))))))
    => '(123 (in-guard thunk-in exception-handler out-guard)))

  #f)


(parametrise ((check-test-name	'continuable-exceptions-from-thunk))

  (check	;show the mechanism of continuable exceptions
      (with-exception-handler
	  (lambda (E)
	    (+ E 2))
	(lambda ()
	  (raise-continuable 1)))
    => 3)

;;; --------------------------------------------------------------------

  (check	;continuable exception from the body
      (with-result
	(with-exception-handler
	    (lambda (E)
	      (add-result 'exception-handler)
	      (+ E 2))
	  (lambda ()
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup))
		(lambda ()
		  (add-result 'thunk-in)
		  (begin0
		      (raise-continuable 1)
		    (add-result 'thunk-out))))))))
    => '(3 (thunk-in exception-handler thunk-out cleanup)))

  (check	;continuable exception from the body
      (with-result
	(guard (E ((non-continuable-violation? E)
		   99)
		  (else E))
	  (with-exception-handler
	      (lambda (E)
		(add-result 'exception-handler)
		(+ E 2))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'cleanup))
		  (lambda ()
		    (add-result 'thunk-in)
		    (begin0
			(raise-continuable 1)
		      (add-result 'thunk-out)))))))))
    => '(3 (thunk-in exception-handler thunk-out cleanup)))

;;; --------------------------------------------------------------------
;;; exiting the dynamic extent of ?THUNK

  ;;Exit the dynamic  extent of ?THUNK by reinstating an  escape continuation from an
  ;;exception handler.  ?CLEANUP is not called.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'exception-handler)
		    (escape E))
		(lambda ()
		  (with-compensations
		    (with-compensation-handler
			(lambda ()
			  (add-result 'cleanup))
		      (lambda ()
			(add-result 'thunk-in)
			(raise-continuable 123)
			(add-result 'thunk-out)))))))))
    => '(123 (thunk-in exception-handler)))

  ;;Exit the dynamic  extent of ?THUNK by reinstating an  escape continuation from an
  ;;exception handler.  ?CLEANUP is not called.
  (check
      (with-result
	(receive (rv)
	    (call/cc
		(lambda (escape)
		  (with-exception-handler
		      (lambda (E)
			(add-result 'exception-handler)
			(escape (lambda ()
				  (add-result 'after-escape)
				  E)))
		    (lambda ()
		      (with-compensations
			(with-compensation-handler
			    (lambda ()
			      (add-result 'cleanup))
			  (lambda ()
			    (add-result 'thunk-in)
			    (raise-continuable 123)
			    (add-result 'thunk-out))))))))
	  (rv)))
    => '(123 (thunk-in exception-handler after-escape)))

  ;;Exit the dynamic extent of ?THUNK  by reinstating an escape continuation from the
  ;;ELSE GUARD clause.
  ;;
  (check
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup))
	      (lambda ()
		(add-result 'thunk-in)
		(raise-continuable 123)
		(add-result 'thunk-out))))))
    => '(123 (thunk-in cleanup guard-else)))

  ;;Raise a continuable exception  in ?THUNK; go through a GUARD  with no ELSE, which
  ;;reraises  the continuable  exception;  execute an  exception  handler; return  to
  ;;?thunk; perform normal return.
  ;;
  (check
      (with-result
	(with-exception-handler
	    (lambda (E)
	      (add-result 'exception-handler)
	      (+ 2 E))
	  (lambda ()
	    (guard (E ((error? E)
		       (add-result 'guard-error)
		       E))
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'cleanup))
		  (lambda ()
		    (add-result 'thunk-in)
		    (begin0
			(raise-continuable 1)
		      (add-result 'thunk-out)))))))))
    => '(3 (thunk-in exception-handler thunk-out cleanup)))

;;; --------------------------------------------------------------------
;;; raising continuable exceptions and DYNAMIC-WIND

  (check	;cleanup not called
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'exception-handler)
		    (escape E))
		(lambda ()
		  (with-compensations
		    (with-compensation-handler
			(lambda ()
			  (add-result 'cleanup))
		      (lambda ()
			(dynamic-wind
			    (lambda ()
			      (add-result 'in-guard))
			    (lambda ()
			      (add-result 'thunk-in)
			      (raise-continuable 123)
			      (add-result 'thunk-out))
			    (lambda ()
			      (add-result 'out-guard)))))))))))
    => '(123 (in-guard thunk-in exception-handler out-guard)))

  #f)


(parametrise ((check-test-name	'exceptions-from-cleanup))

  (check
      (call/cc
	  (lambda (escape)
	    (with-exception-handler
		escape
	      (lambda ()
		(with-exception-handler
		    (lambda (E)
		      (raise 2))
		  (lambda ()
		    (raise 1)))))))
    => 2)

;;; --------------------------------------------------------------------

  (check	;the exception in the cleanup is discarded
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup-in)
		  (raise 2)
		  (add-result 'cleanup-out))
	      (lambda ()
		(add-result 'thunk-in)
		(raise 1)
		(add-result 'thunk-out))))))
    => '(1 (thunk-in cleanup-in guard-else)))

  #f)


(parametrise ((check-test-name	'non-local-exit-with-return))

  (check	;return in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (returnable
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag #t))
		(lambda ()
		  (add-result 'thunk-in)
		  (return 123)
		  (add-result 'thunk-out)))))))
    => '(#t (thunk-in cleanup)))

  (check	;return in body, documentation example
      (internal-body
	(define y #f)
	(define x
	  (returnable
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (set! y #t))
		(lambda ()
		  (return 1))))))
	(values x y))
    => 1 #t)

  #f)


(parametrise ((check-test-name	'non-local-exit-in-while-loop))

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (while #t
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag #t))
		(lambda ()
		  (add-result 'thunk-in)
		  (break)
		  (add-result 'thunk-out)))))))
    => '(#t (thunk-in cleanup)))

  (check	;continue in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (while (positive? i)
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'cleanup)
		      (set! flag (add1 flag)))
		  (lambda ()
		    (add-result 'thunk-in)
		    (set! i (sub1 i))
		    (continue)
		    (add-result 'thunk-out))))))))
    => '(3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y #f)
	(while (positive? x)
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (set! y #t))
	      (lambda ()
		(-- x)
		(break)
		(exit)))))
	(values x y))
    => 2 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y 0)
	(while (positive? x)
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (++ y))
	      (lambda ()
		(-- x)
		(continue)
		(exit)))))
	(values x y))
    => 0 3)

  #f)


(parametrise ((check-test-name	'non-local-exit-in-until-loop))

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (until (zero? i)
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'cleanup)
		      (set! flag (add1 flag)))
		  (lambda ()
		    (add-result 'thunk-in)
		    (set! i (sub1 i))
		    (break)
		    (add-result 'thunk-out))))))))
    => '(1 (thunk-in cleanup)))

  (check	;continue in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (until (zero? i)
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'cleanup)
		      (set! flag (add1 flag)))
		  (lambda ()
		    (add-result 'thunk-in)
		    (set! i (sub1 i))
		    (continue)
		    (add-result 'thunk-out))))))))
    => '(3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y #f)
	(until (zero? x)
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (set! y #t))
	      (lambda ()
		(-- x)
		(break)
		(exit)))))
	(values x y))
    => 2 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y 0)
	(until (zero? x)
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (++ y))
	      (lambda ()
		(-- x)
		(continue)
		(exit)))))
	(values x y))
    => 0 3)

  #f)


(parametrise ((check-test-name	'non-local-exit-in-for-loop))

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (for ((define i 3) (positive? i) (-- i))
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup)
		    (set! flag #t))
		(lambda ()
		  (add-result 'thunk-in)
		  (break)
		  (add-result 'thunk-out)))))))
    => '(#t (thunk-in cleanup)))

  (check	;continue in body
      (with-result
  	(receive-and-return (flag)
  	    0
  	  (for ((define i 3) (positive? i) (-- i))
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup)
		    (++ flag))
		(lambda ()
		  (add-result 'thunk-in)
		  (continue)
		  (add-result 'thunk-out)))))))
    => '(3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
  	(define y #f)
  	(define x 3)
  	(for ((void) (positive? x) (-- x))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (set! y #t))
	      (lambda ()
		(break)
		(exit)))))
  	(values x y))
    => 3 #t)

  (check	;continue in body, simple example for documentation
      (internal-body
  	(define x 3)
  	(define y 0)
  	(for ((void) (positive? x) (-- x))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (++ y))
	      (lambda ()
		(continue)
		(exit)))))
  	(values x y))
    => 0 3)

  #f)


(parametrise ((check-test-name	'non-local-exit-in-do-while-loop))

  (check	;break in body
      (with-result
	(define x 3)
	(define y #f)
	(do
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup)
		    (set! y #t))
		(lambda ()
		  (add-result 'thunk-in)
		  (break)
		  (add-result 'thunk-out))))
	    (while (positive? x)))
	(values x y))
    => '(3 #t (thunk-in cleanup)))

  (check	;continue in body
      (with-result
	(define x 3)
	(define y 0)
	(do
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup)
		    (++ y))
		(lambda ()
		  (add-result 'thunk-in)
		  (-- x)
		  (continue)
		  (add-result 'thunk-out))))
	    (while (positive? x)))
	(values x y))
    => '(0 3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  #f)


(parametrise ((check-test-name	'non-local-exit-in-do-until-loop))

  (check	;break in body
      (with-result
	(define x 3)
	(define y #f)
	(do
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup)
		    (set! y #t))
		(lambda ()
		  (add-result 'thunk-in)
		  (break)
		  (add-result 'thunk-out))))
	    (until (zero? x)))
	(values x y))
    => '(3 #t (thunk-in cleanup)))

  (check	;continue in body
      (with-result
	(define x 3)
	(define y 0)
	(do
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup)
		    (++ y))
		(lambda ()
		  (add-result 'thunk-in)
		  (-- x)
		  (continue)
		  (add-result 'thunk-out))))
	    (until (zero? x)))
	(values x y))
    => '(0 3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  #f)


(parametrise ((check-test-name	'non-local-exit-standard-do-loop))

  (check 	;break in body
      (with-result
	(define y #f)
	(define x
	  (do ((x 3 (-- x)))
	      ((zero? x)
	       (add-result 'do-exit)
	       (values x y))
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup-in)
		    (set! y #t)
		    (add-result 'cleanup-out))
		(lambda ()
		  (add-result 'thunk-in)
		  (break x)
		  (add-result 'thunk-out))))))
	(values x y))
    => '(3 #t (thunk-in cleanup-in cleanup-out)))

  (check	;continue in body
      (with-result
	(do ((x 3 (-- x))
	     (y 0))
	    ((zero? x)
	     (values x y))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup)
		  (++ y))
	      (lambda ()
		(add-result 'thunk-in)
		(continue)
		(add-result 'thunk-out))))))
    => '(0 3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  #f)


(parametrise ((check-test-name	'reentering-continuations))

  (check	;reentering continuation
      (with-result
	(guard (E ((non-reinstatable-violation? E)
		   (add-result 'violation)
		   #t)
		  (else E))
	  (let ((rv (with-compensations
		      (with-compensation-handler
			  (lambda ()
			    (add-result 'cleanup))
			(lambda ()
			  (add-result 'thunk-in)
			  (begin0
			      (call/cc values)
			    (add-result 'thunk-out)))))))
	    (cond ((procedure? rv)
		   (add-result 'reinstating)
		   (rv 123))
		  (else
		   (add-result 'returning)
		   rv)))))
    => '(#t (thunk-in thunk-out cleanup reinstating violation)))

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
	    (let ((rv (with-compensations
			(with-compensation-handler
			    (lambda ()
			      (add 'cleanup))
			  (lambda ()
			    (add 'thunk-in)
			    (begin0
				(call/cc values)
			      (add 'thunk-out)))))))
	      (cond ((procedure? rv)
		     (add 'reinstating)
		     (rv 123))
		    (else
		     (add 'returning)
		     rv)))))
	(values rv (reverse order)))
    => #t '(thunk-in thunk-out cleanup reinstating violation))

  #f)


(parametrise ((check-test-name	'coroutines))

  (define (print template . args)
    (apply fprintf (current-error-port) template args)
    (yield))

;;; --------------------------------------------------------------------

  (check
      (let ((a #f) (b #f) (c #f))
	(concurrently
	  (lambda ()
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (set! a 1.4))
		(lambda ()
		  (set! a 1.1)
		  (print "sub 1.1: ~a\n" a)
		  (set! a 1.2)
		  (print "sub 1.2: ~a\n" a)
		  (set! a 1.3)
		  (print "sub 1.3: ~a\n" a)))))
	  (lambda ()
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (set! b 2.4))
		(lambda ()
		  (set! b 2.1)
		  (print "sub 2.1: ~a\n" b)
		  (set! b 2.2)
		  (print "sub 2.2: ~a\n" b)
		  (set! b 2.3)
		  (print "sub 2.3: ~a\n" b)))))
	  (lambda ()
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (set! c 3.4))
		(lambda ()
		  (set! c 3.1)
		  (print "sub 3.1: ~a\n" c)
		  (set! c 3.2)
		  (print "sub 3.2: ~a\n" c)
		  (set! c 3.3)
		  (print "sub 3.3: ~a\n" c))))))
	(values a b c))
    => 1.4 2.4 3.4)

  #t)


(parametrise ((check-test-name	'dynamic-environment))

  (define parm
    (make-parameter #f))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(parametrise ((parm 'parm))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup-in)
		  (add-result (parm))
		  (add-result 'cleanup-out))
	      (lambda ()
		(add-result 'thunk-in)
		(add-result (parm))
		(add-result 'thunk-out)
		1)))))
    => '(1 (thunk-in parm thunk-out cleanup-in parm cleanup-out)))

  ;;Changing the environment inside ?THUNK does not affect ?CLEANUP.
  ;;
  (check
      (with-result
	(parametrise ((parm 'outer-parm))
	  (with-compensations
	    (with-compensation-handler
		(lambda ()
		  (add-result 'cleanup-in)
		  (add-result (parm))
		  (add-result 'cleanup-out))
	      (lambda ()
		(parametrise ((parm 'inner-parm))
		  (add-result 'thunk-in)
		  (add-result (parm))
		  (add-result 'thunk-out)
		  1))))))
    => '(1 (thunk-in inner-parm thunk-out cleanup-in outer-parm cleanup-out)))

  ;;Changing  the environment  inside ?THUNK  does  not affect  ?CLEANUP.  Exit  with
  ;;RETURN.
  ;;
  (check
      (with-result
	(returnable
	  (parametrise ((parm 'outer-parm))
	    (with-compensations
	      (with-compensation-handler
		  (lambda ()
		    (add-result 'cleanup-in)
		    (add-result (parm))
		    (add-result 'cleanup-out))
		(lambda ()
		  (parametrise ((parm 'inner-parm))
		    (add-result 'thunk-in)
		    (add-result (parm))
		    (return 2)
		    (add-result 'thunk-out)
		    1)))))))
    => '(2 (thunk-in inner-parm cleanup-in outer-parm cleanup-out)))

;;; --------------------------------------------------------------------
;;; exit with RETURN

  (check
      (with-result
	(parametrise ((parm 'outer-parm))
	  (returnable
	    (parametrise ((parm 'inner-parm))
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'cleanup-in)
		      (add-result (parm))
		      (add-result 'cleanup-out))
		  (lambda ()
		    (add-result 'thunk-in)
		    (add-result (parm))
		    (return 2)
		    (add-result 'thunk-out)
		    1)))))))
    => '(2 (thunk-in inner-parm cleanup-in inner-parm cleanup-out)))

;;; --------------------------------------------------------------------
;;; raising exception from thunk

  (check
      (with-result
	(parametrise ((parm 'outer-parm))
	  (guard (E ((begin
		       (add-result 'guard-test-in)
		       (add-result (parm))
		       (add-result 'guard-test-out)
		       #t)
		     (add-result 'guard-expr-in)
		     (add-result (parm))
		     (add-result 'guard-expr-out)
		     E))
	    (parametrise ((parm 'inner-parm))
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'cleanup-in)
		      (add-result (parm))
		      (add-result 'cleanup-out))
		  (lambda ()
		    (add-result 'thunk-in)
		    (add-result (parm))
		    (raise 2)
		    (add-result 'thunk-out)
		    1)))))))
    => '(2 (thunk-in inner-parm
		     guard-test-in outer-parm guard-test-out
		     cleanup-in inner-parm cleanup-out
		     guard-expr-in outer-parm guard-expr-out)))

  #t)


(parametrise ((check-test-name	'exceptions-from-guards))

;;;What  happens when  we  raise an  exception  from the  in-guard  and out-guard  of
;;;DYNAMIC-WIND?

  (check	;no exceptions raised
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'inner/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'inner/inner-in-guard))
			(lambda ()
			  (add-result 'inner/thunk))
			(lambda ()
			  (add-result 'inner/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'middle/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'middle/inner-in-guard))
			(lambda ()
			  (add-result 'middle/thunk-in)
			  (inner)
			  (add-result 'middle/thunk-out))
			(lambda ()
			  (add-result 'middle/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'outer/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'outer/inner-in-guard))
			(lambda ()
			  (add-result 'outer/thunk-in)
			  (middle)
			  (add-result 'outer/thunk-out)
			  1)
			(lambda ()
			  (add-result 'outer/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (outer))
    => '(1 (outer/outer-in-guard
	    outer/inner-in-guard
	    outer/thunk-in
	    middle/outer-in-guard
	    middle/inner-in-guard
	    middle/thunk-in
	    inner/outer-in-guard
	    inner/inner-in-guard
	    inner/thunk
	    inner/inner-out-guard
	    inner/***cleanup***
	    inner/outer-out-guard
	    middle/thunk-out
	    middle/inner-out-guard
	    middle/***cleanup***
	    middle/outer-out-guard
	    outer/thunk-out
	    outer/inner-out-guard
	    outer/***cleanup***
	    outer/outer-out-guard)))

;;; --------------------------------------------------------------------
;;; raise exception from inner thunk

  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'inner/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'inner/inner-in-guard))
			(lambda ()
			  (add-result 'inner/thunk-in/raise)
			  (raise 2)
			  (add-result 'inner/thunk-out))
			(lambda ()
			  (add-result 'inner/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'middle/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'middle/inner-in-guard))
			(lambda ()
			  (add-result 'middle/thunk-in)
			  (inner)
			  (add-result 'middle/thunk-out))
			(lambda ()
			  (add-result 'middle/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'outer/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'outer/inner-in-guard))
			(lambda ()
			  (add-result 'outer/thunk-in)
			  (middle)
			  (add-result 'outer/thunk-out)
			  1)
			(lambda ()
			  (add-result 'outer/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(2 (outer/outer-in-guard
	    outer/inner-in-guard
	    outer/thunk-in
	    middle/outer-in-guard
	    middle/inner-in-guard
	    middle/thunk-in
	    inner/outer-in-guard
	    inner/inner-in-guard

	    inner/thunk-in/raise

	    inner/inner-out-guard
	    inner/outer-out-guard
	    middle/inner-out-guard
	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/outer-out-guard

	    guard-test

	    outer/outer-in-guard
	    outer/inner-in-guard
	    middle/outer-in-guard
	    middle/inner-in-guard
	    inner/outer-in-guard
	    inner/inner-in-guard

	    inner/inner-out-guard
	    inner/***cleanup***
	    inner/outer-out-guard
	    middle/inner-out-guard
	    middle/***cleanup***
	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/***cleanup***
	    outer/outer-out-guard

	    guard-expr)))

;;; --------------------------------------------------------------------
;;; raise exception from middle/inner-out-guard

  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'inner/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'inner/inner-in-guard))
			(lambda ()
			  (add-result 'inner/thunk))
			(lambda ()
			  (add-result 'inner/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'middle/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'middle/inner-in-guard))
			(lambda ()
			  (add-result 'middle/thunk-in)
			  (inner)
			  (add-result 'middle/thunk-out))
			(lambda ()
			  (add-result 'middle/inner-out-guard/raise)
			  (raise 2)))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'outer/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'outer/inner-in-guard))
			(lambda ()
			  (add-result 'outer/thunk-in)
			  (middle)
			  (add-result 'outer/thunk-out)
			  1)
			(lambda ()
			  (add-result 'outer/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(2 (outer/outer-in-guard
	    outer/inner-in-guard
	    outer/thunk-in
	    middle/outer-in-guard
	    middle/inner-in-guard
	    middle/thunk-in
	    inner/outer-in-guard
	    inner/inner-in-guard
	    inner/thunk
	    inner/inner-out-guard
	    inner/***cleanup***
	    inner/outer-out-guard
	    middle/thunk-out

	    middle/inner-out-guard/raise

	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/outer-out-guard

	    guard-test

	    outer/outer-in-guard
	    outer/inner-in-guard
	    middle/outer-in-guard
	    middle/***cleanup***
	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/***cleanup***
	    outer/outer-out-guard

	    guard-expr)))

;;; --------------------------------------------------------------------
;;; raise exception from middle/outer-out-guard

  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'inner/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'inner/inner-in-guard))
			(lambda ()
			  (add-result 'inner/thunk))
			(lambda ()
			  (add-result 'inner/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'middle/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'middle/inner-in-guard))
			(lambda ()
			  (add-result 'middle/thunk-in)
			  (inner)
			  (add-result 'middle/thunk-out))
			(lambda ()
			  (add-result 'middle/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard/raise)
	      (raise 2))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'outer/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'outer/inner-in-guard))
			(lambda ()
			  (add-result 'outer/thunk-in)
			  (middle)
			  (add-result 'outer/thunk-out)
			  1)
			(lambda ()
			  (add-result 'outer/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(2 (outer/outer-in-guard
	    outer/inner-in-guard
	    outer/thunk-in
	    middle/outer-in-guard
	    middle/inner-in-guard
	    middle/thunk-in
	    inner/outer-in-guard
	    inner/inner-in-guard
	    inner/thunk
	    inner/inner-out-guard
	    inner/***cleanup***
	    inner/outer-out-guard
	    middle/thunk-out
	    middle/inner-out-guard
	    middle/***cleanup***

	    middle/outer-out-guard/raise

	    outer/inner-out-guard
	    outer/outer-out-guard

	    guard-test

	    outer/outer-in-guard
	    outer/inner-in-guard
	    outer/inner-out-guard
	    outer/***cleanup***
	    outer/outer-out-guard

	    guard-expr)))

;;; --------------------------------------------------------------------
;;; raise exception from inner/thunk and then from middle/inner-out-guard

  ;;The inner/cleanup is never called.
  ;;
  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'inner/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'inner/inner-in-guard))
			(lambda ()
			  (add-result 'inner/thunk-in/raise)
			  (raise 2)
			  (add-result 'inner/thunk-out))
			(lambda ()
			  (add-result 'inner/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'middle/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'middle/inner-in-guard))
			(lambda ()
			  (add-result 'middle/thunk-in)
			  (inner)
			  (add-result 'middle/thunk-out))
			(lambda ()
			  (add-result 'middle/inner-out-guard/raise)
			  (raise 3)))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'outer/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'outer/inner-in-guard))
			(lambda ()
			  (add-result 'outer/thunk-in)
			  (middle)
			  (add-result 'outer/thunk-out)
			  1)
			(lambda ()
			  (add-result 'outer/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(3 (outer/outer-in-guard
	    outer/inner-in-guard
	    outer/thunk-in
	    middle/outer-in-guard
	    middle/inner-in-guard
	    middle/thunk-in
	    inner/outer-in-guard
	    inner/inner-in-guard

	    inner/thunk-in/raise

	    inner/inner-out-guard
	    inner/outer-out-guard

	    middle/inner-out-guard/raise

	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/outer-out-guard

	    guard-test

	    outer/outer-in-guard
	    outer/inner-in-guard
	    middle/outer-in-guard
	    middle/***cleanup***
	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/***cleanup***
	    outer/outer-out-guard
	    guard-expr)))

;;; --------------------------------------------------------------------
;;; raise exception from inner/thunk and then from middle/inner-out-guard 2nd time

  ;;All the cleanups are called.
  ;;
  (check
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'inner/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'inner/inner-in-guard))
			(lambda ()
			  (add-result 'inner/thunk-in/raise)
			  (raise 2)
			  (add-result 'inner/thunk-out))
			(lambda ()
			  (add-result 'inner/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(define counter 0)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'middle/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'middle/inner-in-guard))
			(lambda ()
			  (add-result 'middle/thunk-in)
			  (inner)
			  (add-result 'middle/thunk-out))
			(lambda ()
			  (add-result 'middle/inner-out-guard/raise)
			  (++ counter)
			  (when (= 2 counter)
			    (raise 3))))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-compensations
		(with-compensation-handler
		    (lambda ()
		      (add-result 'outer/***cleanup***))
		  (lambda ()
		    (dynamic-wind
			(lambda ()
			  (add-result 'outer/inner-in-guard))
			(lambda ()
			  (add-result 'outer/thunk-in)
			  (middle)
			  (add-result 'outer/thunk-out)
			  1)
			(lambda ()
			  (add-result 'outer/inner-out-guard)))))))
	    (lambda ()
	      (add-result 'outer/outer-out-guard))))

      (guard (E ((begin
		   (add-result 'guard-test)
		   #t)
		 (add-result 'guard-expr)
		 E))
	(outer)))
    => '(3 (outer/outer-in-guard
	    outer/inner-in-guard
	    outer/thunk-in
	    middle/outer-in-guard
	    middle/inner-in-guard
	    middle/thunk-in
	    inner/outer-in-guard
	    inner/inner-in-guard
	    inner/thunk-in/raise
	    inner/inner-out-guard
	    inner/outer-out-guard

	    middle/inner-out-guard/raise

	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/outer-out-guard

	    guard-test

	    outer/outer-in-guard
	    outer/inner-in-guard
	    middle/outer-in-guard
	    middle/inner-in-guard
	    inner/outer-in-guard
	    inner/inner-in-guard
	    inner/inner-out-guard
	    inner/***cleanup***
	    inner/outer-out-guard

	    middle/inner-out-guard/raise

	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/outer-out-guard
	    guard-test
	    outer/outer-in-guard
	    outer/inner-in-guard
	    middle/outer-in-guard
	    middle/***cleanup***
	    middle/outer-out-guard
	    outer/inner-out-guard
	    outer/***cleanup***
	    outer/outer-out-guard

	    guard-expr)))

  #t)


;;;; done

(check-report)

;;; end of file
