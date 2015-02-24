;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the expander
;;;Date: Tue Sep 25, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (except (vicare)
		with-unwind-protection)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expander, unwind-protection\n")


;;;; alternative versions

(define-syntax commented-out
  (syntax-rules ()
    ((_ . ?form)
     (module ()))
    ))

;;This imports the built-in version.
(import (only (vicare) with-unwind-protection))

;;The following  is an old  version.  It worked  fine with: escape  continuations and
;;coroutines.   It  correctly  raised   a  "&non-reinstatable"  exception  with  full
;;continuations reentering  after clean-up  evaluation.  It INcorrectly  executes the
;;cleanup  if an  exception handler  exits  the dynamic  extent of  ?THUNK, like  the
;;standard GUARD does.
;;
(commented-out
 (define-syntax with-unwind-protection
   (syntax-rules ()
     ((_ ?cleanup ?thunk)
      (let ((cleanup            ?cleanup)
	    ;;True if the dynamic extent of the call to THUNK is terminated.
	    (terminated?        #f)
	    ;;True  if  the dynamic  extent  of  the call  to  ?THUNK  was exited  by
	    ;;performing a normal return.
	    (normal-exit?       #f)
	    ;;True if the dynamic extent of the  call to ?THUNK was exited by calling
	    ;;an escape handler.
	    (escape-exit?	 #f)
	    ;;True  if  the dynamic  extent  of  the call  to  ?THUNK  was exited  by
	    ;;reinstating an external continuation in an exception handler.
	    (except-exit?	 #f))
	(import (only (psyntax system $all)
		      with-escape-handler))
	(dynamic-wind
	    (lambda ()
	      (when terminated?
		(non-reinstatable-violation 'with-unwind-protection
		  "attempt to reenter thunk with terminated dynamic extent")))
	    (lambda ()
	      (begin0
		  (with-exception-handler
		      (lambda (E)
			(set! except-exit? #t)
			(begin0
			    (raise-continuable E)
			  (set! except-exit? #f)))
		    (lambda ()
		      (with-escape-handler
			  (lambda ()
			    (set! escape-exit? #t))
			?thunk)))
		(set! normal-exit? #t)))
	    (lambda ()
	      (when (or normal-exit?
			escape-exit?
			except-exit?)
		(set! terminated? #t)
		(cleanup))))))
     )))

;;The following  is an  old version.   It worked  fine with:  escaping continuations;
;;non-contiuable  exceptions; coroutines.   It failed  with: continuable  exceptions;
;;reentering continuations after clean-up evaluation.
;;
(commented-out
 (define-syntax with-unwind-protection-macro
   (syntax-rules ()
     ((_ ?cleanup ?body)
      (let ((cleanup ?cleanup))
	(begin0
	    (with-exception-handler
		(lambda (E)
		  (cleanup)
		  (raise E))
	      (lambda ()
		(with-escape-handler
		    cleanup
		  ?body)))
	  (cleanup))))
     )))


(parametrise ((check-test-name	'basic-mechanism))

  (check	;normal exit
      (with-result
	(with-unwind-protection
	    (lambda (why)
	      (add-result 'out))
	  (lambda ()
	    (add-result 'in)
	    1)))
    => '(1 (in out)))

  (check	;normal exit
      (with-result
	(receive-and-return (flag)
	    #f
	  (with-unwind-handler
	      (lambda (why)
		(add-result 'cleanup)
		(set! flag #t))
	    (lambda ()
	      (add-result 'body)))))
    => '(#t (body cleanup)))

  (check	;multiple return values
      (with-result
	(receive (a b)
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'out))
	      (lambda ()
		(add-result 'in)
		(values 1 2)))
	  (list a b)))
    => '((1 2) (in out)))

  (check	;zero return values
      (with-result
	(with-unwind-protection
	    (lambda (why)
	      (add-result 'out))
	  (lambda ()
	    (add-result 'in)
	    (values)))
	#t)
    => `(#t (in out)))

  #f)


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
	  (with-unwind-protection
	      (lambda (why)
		(add-result 'cleanup))
	    (lambda ()
	      (add-result 'thunk-in)
	      (raise 2)
	      (add-result 'thunk-out)
	      1))))
    => '(2 (thunk-in cleanup guard-else)))

  (check 	;exception in body, GUARD's clause
      (with-result
	(guard (E ((begin
		     (add-result 'guard-test)
		     #t)
		   (add-result 'guard-expr)
		   E))
	  (with-unwind-protection
	      (lambda (why)
		(add-result 'cleanup))
	    (lambda ()
	      (add-result 'thunk-in)
	      (raise 2)
	      (add-result 'thunk-out)
	      1))))
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
		  (with-unwind-protection
		      (lambda (why)
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
			    (add-result 'inner-after))))))
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
		  (with-unwind-protection
		      (lambda (why)
			(add-result 'cleanup))
		    (lambda ()
		      (add-result 'thunk-in)
		      (raise 123)
		      (add-result 'thunk-out))))))))
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
		      (with-unwind-protection
			  (lambda (why)
			    (add-result 'cleanup))
			(lambda ()
			  (add-result 'thunk-in)
			  (raise 123)
			  (add-result 'thunk-out)))))))
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
	  (with-unwind-protection
	      (lambda (why)
		(add-result 'cleanup))
	    (lambda ()
	      (add-result 'thunk-in)
	      (raise 123)
	      (add-result 'thunk-out)))))
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
		  (with-unwind-protection
		      (lambda (why)
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
			    (add-result 'out-guard))))))))))
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
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup))
	      (lambda ()
		(add-result 'thunk-in)
		(begin0
		    (raise-continuable 1)
		  (add-result 'thunk-out)))))))
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
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'cleanup))
		(lambda ()
		  (add-result 'thunk-in)
		  (begin0
		      (raise-continuable 1)
		    (add-result 'thunk-out))))))))
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
		  (with-unwind-protection
		      (lambda (why)
			(add-result 'cleanup))
		    (lambda ()
		      (add-result 'thunk-in)
		      (raise-continuable 123)
		      (add-result 'thunk-out))))))))
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
		      (with-unwind-protection
			  (lambda (why)
			    (add-result 'cleanup))
			(lambda ()
			  (add-result 'thunk-in)
			  (raise-continuable 123)
			  (add-result 'thunk-out)))))))
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
	  (with-unwind-protection
	      (lambda (why)
		(add-result 'cleanup))
	    (lambda ()
	      (add-result 'thunk-in)
	      (raise-continuable 123)
	      (add-result 'thunk-out)))))
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
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'cleanup))
		(lambda ()
		  (add-result 'thunk-in)
		  (begin0
		      (raise-continuable 1)
		    (add-result 'thunk-out))))))))
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
		  (with-unwind-protection
		      (lambda (why)
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
			    (add-result 'out-guard))))))))))
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
	  (with-unwind-protection
	      (lambda (why)
		(add-result 'cleanup-in)
		(raise 2)
		(add-result 'cleanup-out))
	    (lambda ()
	      (add-result 'thunk-in)
	      (raise 1)
	      (add-result 'thunk-out)))))
    => '(1 (thunk-in cleanup-in guard-else)))

  #f)


(parametrise ((check-test-name	'non-local-exit-with-return))

  (check	;return in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (returnable
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'thunk-in)
		(return 123)
		(add-result 'thunk-out))))))
    => '(#t (thunk-in cleanup)))

  (check	;return in body, documentation example
      (internal-body
	(define y #f)
	(define x
	  (returnable
	    (with-unwind-protection
		(lambda (why)
		  (set! y #t))
	      (lambda ()
		(return 1)))))
	(values x y))
    => 1 #t)

  #f)


(parametrise ((check-test-name	'non-local-exit-in-while-loop))

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (while #t
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'thunk-in)
		(break)
		(add-result 'thunk-out))))))
    => '(#t (thunk-in cleanup)))

  (check	;continue in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (while (positive? i)
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'thunk-in)
		  (set! i (sub1 i))
		  (continue)
		  (add-result 'thunk-out)))))))
    => '(3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y #f)
	(while (positive? x)
	  (with-unwind-protection
	      (lambda (why)
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
	      (lambda (why)
		(++ y))
	    (lambda ()
	      (-- x)
	      (continue)
	      (exit))))
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
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'thunk-in)
		  (set! i (sub1 i))
		  (break)
		  (add-result 'thunk-out)))))))
    => '(1 (thunk-in cleanup)))

  (check	;continue in body
      (with-result
	(receive-and-return (flag)
	    0
	  (let ((i 3))
	    (until (zero? i)
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'cleanup)
		    (set! flag (add1 flag)))
		(lambda ()
		  (add-result 'thunk-in)
		  (set! i (sub1 i))
		  (continue)
		  (add-result 'thunk-out)))))))
    => '(3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
	(define x 3)
	(define y #f)
	(until (zero? x)
	  (with-unwind-protection
	      (lambda (why)
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
	      (lambda (why)
		(++ y))
	    (lambda ()
	      (-- x)
	      (continue)
	      (exit))))
	(values x y))
    => 0 3)

  #f)


(parametrise ((check-test-name	'non-local-exit-in-for-loop))

  (check	;break in body
      (with-result
	(receive-and-return (flag)
	    #f
	  (for ((define i 3) (positive? i) (-- i))
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup)
		  (set! flag #t))
	      (lambda ()
		(add-result 'thunk-in)
		(break)
		(add-result 'thunk-out))))))
    => '(#t (thunk-in cleanup)))

  (check	;continue in body
      (with-result
  	(receive-and-return (flag)
  	    0
  	  (for ((define i 3) (positive? i) (-- i))
  	    (with-unwind-protection
  		(lambda (why)
  		  (add-result 'cleanup)
  		  (++ flag))
  	      (lambda ()
  		(add-result 'thunk-in)
  		(continue)
  		(add-result 'thunk-out))))))
    => '(3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  (check	;break in body, simple example for documentation
      (internal-body
  	(define y #f)
  	(define x 3)
  	(for ((void) (positive? x) (-- x))
  	  (with-unwind-protection
  	      (lambda (why)
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
  	      (lambda (why)
  		(++ y))
  	    (lambda ()
  	      (continue)
  	      (exit))))
  	(values x y))
    => 0 3)

  #f)


(parametrise ((check-test-name	'non-local-exit-in-do-while-loop))

  (check	;break in body
      (with-result
	(define x 3)
	(define y #f)
	(do
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup)
		  (set! y #t))
	      (lambda ()
		(add-result 'thunk-in)
		(break)
		(add-result 'thunk-out)))
	    (while (positive? x)))
	(values x y))
    => '(3 #t (thunk-in cleanup)))

  (check	;continue in body
      (with-result
	(define x 3)
	(define y 0)
	(do
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup)
		  (++ y))
	      (lambda ()
		(add-result 'thunk-in)
		(-- x)
		(continue)
		(add-result 'thunk-out)))
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
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup)
		  (set! y #t))
	      (lambda ()
		(add-result 'thunk-in)
		(break)
		(add-result 'thunk-out)))
	    (until (zero? x)))
	(values x y))
    => '(3 #t (thunk-in cleanup)))

  (check	;continue in body
      (with-result
	(define x 3)
	(define y 0)
	(do
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup)
		  (++ y))
	      (lambda ()
		(add-result 'thunk-in)
		(-- x)
		(continue)
		(add-result 'thunk-out)))
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
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup-in)
		  (set! y #t)
		  (add-result 'cleanup-out))
	      (lambda ()
		(add-result 'thunk-in)
		(break x)
		(add-result 'thunk-out)))))
	(values x y))
    => '(3 #t (thunk-in cleanup-in cleanup-out)))

  (check	;continue in body
      (with-result
	(do ((x 3 (-- x))
	     (y 0))
	    ((zero? x)
	     (values x y))
	  (with-unwind-protection
	      (lambda (why)
		(add-result 'cleanup)
		(++ y))
	    (lambda ()
	      (add-result 'thunk-in)
	      (continue)
	      (add-result 'thunk-out)))))
    => '(0 3 (thunk-in cleanup thunk-in cleanup thunk-in cleanup)))

  #f)


(parametrise ((check-test-name	'reentering-continuations))

  (check	;reentering continuation
      (with-result
	(guard (E ((non-reinstatable-violation? E)
		   (add-result 'violation)
		   #t)
		  (else E))
	  (let ((rv (with-unwind-protection
			(lambda (why)
			  (add-result 'cleanup))
		      (lambda ()
			(add-result 'thunk-in)
			(begin0
			    (call/cc values)
			  (add-result 'thunk-out))))))
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
	    (let ((rv (with-unwind-protection
			  (lambda (why)
			    (add 'cleanup))
			(lambda ()
			  (add 'thunk-in)
			  (begin0
			      (call/cc values)
			    (add 'thunk-out))))))
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
    => 1.4 2.4 3.4)

  #t)


(parametrise ((check-test-name	'dynamic-environment))

  (define parm
    (make-parameter #f))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(parametrise ((parm 'parm))
	  (with-unwind-protection
	      (lambda (why)
		(add-result 'cleanup-in)
		(add-result (parm))
		(add-result 'cleanup-out))
	    (lambda ()
	      (add-result 'thunk-in)
	      (add-result (parm))
	      (add-result 'thunk-out)
	      1))))
    => '(1 (thunk-in parm thunk-out cleanup-in parm cleanup-out)))

  ;;Changing the environment inside ?THUNK does not affect ?CLEANUP.
  ;;
  (check
      (with-result
	(parametrise ((parm 'outer-parm))
	  (with-unwind-protection
	      (lambda (why)
		(add-result 'cleanup-in)
		(add-result (parm))
		(add-result 'cleanup-out))
	    (lambda ()
	      (parametrise ((parm 'inner-parm))
		(add-result 'thunk-in)
		(add-result (parm))
		(add-result 'thunk-out)
		1)))))
    => '(1 (thunk-in inner-parm thunk-out cleanup-in outer-parm cleanup-out)))

  ;;Changing  the environment  inside ?THUNK  does  not affect  ?CLEANUP.  Exit  with
  ;;RETURN.
  ;;
  (check
      (with-result
	(returnable
	  (parametrise ((parm 'outer-parm))
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'cleanup-in)
		  (add-result (parm))
		  (add-result 'cleanup-out))
	      (lambda ()
		(parametrise ((parm 'inner-parm))
		  (add-result 'thunk-in)
		  (add-result (parm))
		  (return 2)
		  (add-result 'thunk-out)
		  1))))))
    => '(2 (thunk-in inner-parm cleanup-in outer-parm cleanup-out)))

;;; --------------------------------------------------------------------
;;; exit with RETURN

  (check
      (with-result
	(parametrise ((parm 'outer-parm))
	  (returnable
	    (parametrise ((parm 'inner-parm))
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'cleanup-in)
		    (add-result (parm))
		    (add-result 'cleanup-out))
		(lambda ()
		  (add-result 'thunk-in)
		  (add-result (parm))
		  (return 2)
		  (add-result 'thunk-out)
		  1))))))
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
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'cleanup-in)
		    (add-result (parm))
		    (add-result 'cleanup-out))
		(lambda ()
		  (add-result 'thunk-in)
		  (add-result (parm))
		  (raise 2)
		  (add-result 'thunk-out)
		  1))))))
    => '(2 (thunk-in inner-parm
		     guard-test-in outer-parm guard-test-out
		     cleanup-in inner-parm cleanup-out
		     guard-expr-in outer-parm guard-expr-out)))

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
	(guard (E (else
		   (add-result 'guard-else)
		   #t))
	  (unwind-protect
	      (begin
		(add-result 'body-in)
		(error #f "fail!!!")
		(add-result 'body-out)
		1)
	    (add-result 'cleanup))))
    => '(#t (body-in cleanup guard-else)))

  #t)


(parametrise ((check-test-name	'exceptions-from-guards))

  ;;Raising an exception from an exception handler: the unwind handler is called, but
  ;;the original exception is lost.
  ;;
  (check
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (with-exception-handler
	      (lambda (E)
		(add-result 'exception-handler)
		(raise 2))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'unwind-handler))
		(lambda ()
		  (dynamic-wind
		      (lambda ()
			(add-result 'in-guard))
		      (lambda ()
			(add-result 'thunk-in)
			(raise 1)
			(add-result 'thunk-out))
		      (lambda ()
			(add-result 'out-guard)))))))))
    => '(2 (in-guard
	    thunk-in
	    exception-handler
	    out-guard
	    in-guard out-guard unwind-handler
	    guard-else)))

  ;;Raising exception  from GUARD's test:  the unwind handler  is NOT called  and the
  ;;original exception is lost.
  ;;
  (check
      (with-result
	(guard (E (else
		   (add-result 'outer-guard-else)
		   E))
	  (guard (E ((begin
		       (add-result 'inner-guard-test)
		       (raise 2))
		     E))
	    (with-unwind-protection
		(lambda (why)
		  (add-result 'unwind-handler))
	      (lambda ()
		(dynamic-wind
		    (lambda ()
		      (add-result 'in-guard))
		    (lambda ()
		      (add-result 'thunk-in)
		      (raise 1)
		      (add-result 'thunk-out))
		    (lambda ()
		      (add-result 'out-guard))))))))
    => '(2 (in-guard
	    thunk-in
	    out-guard
	    inner-guard-test
	    outer-guard-else)))

  #t)


(parametrise ((check-test-name	'exceptions-from-guards))

;;;What  happens when  we  raise an  exception  from the  in-guard  and out-guard  of
;;;DYNAMIC-WIND?

  (check	;no error in guard thunks
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (dynamic-wind
	      (lambda ()
		(add-result 'in-guard))
	      (lambda ()
		(with-unwind-handler
		    (lambda (E)
		      (add-result 'unwind-handler))
		  (lambda ()
		    (add-result 'thunk-in)
		    (raise 1))))
	      (lambda ()
		(add-result 'out-guard)))))
    => '(1 (in-guard
	    thunk-in
	    out-guard
	    in-guard unwind-handler out-guard
	    guard-else)))

  (check	;error in guard thunks
      (with-result
	(guard (E (else
		   (add-result 'guard-else)
		   E))
	  (dynamic-wind
	      (let ((flag #f))
		(lambda ()
		  (cond (flag
			 (add-result 'in-guard/raise)
			 (raise 2))
			(else
			 (set! flag #t)
			 (add-result 'in-guard)))))
	      (lambda ()
		(with-unwind-handler
		    (lambda (E)
		      (add-result 'unwind-handler))
		  (lambda ()
		    (add-result 'thunk-in)
		    (raise 1))))
	      (lambda ()
		(add-result 'out-guard)))))
    => '(2 (in-guard
	    thunk-in
	    out-guard
	    in-guard/raise
	    guard-else)))

;;; --------------------------------------------------------------------

  (check	;no exceptions raised
    (with-result

      (define (inner)
	(dynamic-wind
	    (lambda ()
	      (add-result 'inner/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'inner/***cleanup***))
		(lambda ()
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk))
		      (lambda ()
			(add-result 'inner/inner-out-guard))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'middle/inner-out-guard))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'outer/inner-out-guard))))))
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
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'inner/inner-out-guard))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'middle/inner-out-guard))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'outer/inner-out-guard))))))
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
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'inner/***cleanup***))
		(lambda ()
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk))
		      (lambda ()
			(add-result 'inner/inner-out-guard))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(raise 2))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'outer/inner-out-guard))))))
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
	      (with-unwind-protection
		  (lambda (why)
		    (add-result 'inner/***cleanup***))
		(lambda ()
		  (dynamic-wind
		      (lambda ()
			(add-result 'inner/inner-in-guard))
		      (lambda ()
			(add-result 'inner/thunk))
		      (lambda ()
			(add-result 'inner/inner-out-guard))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'middle/inner-out-guard))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard/raise)
	      (raise 2))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'outer/inner-out-guard))))))
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
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'inner/inner-out-guard))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(raise 3))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'outer/inner-out-guard))))))
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
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'inner/inner-out-guard))))))
	    (lambda ()
	      (add-result 'inner/outer-out-guard))))

      (define (middle)
	(define counter 0)
	(dynamic-wind
	    (lambda ()
	      (add-result 'middle/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			  (raise 3)))))))
	    (lambda ()
	      (add-result 'middle/outer-out-guard))))

      (define (outer)
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer/outer-in-guard))
	    (lambda ()
	      (with-unwind-protection
		  (lambda (why)
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
			(add-result 'outer/inner-out-guard))))))
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
;; Local Variables:
;; coding: utf-8-unix
;; End:
