;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the dynamic environment
;;;Date: Wed Feb  4, 2015
;;;
;;;Abstract
;;;
;;;	This test  file is  a collection  of code samples  that exercise  the dynamic
;;;	environment facilities.  It is meant for both testing and documentation.
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: dynamic environment\n")


;;;; helpers

(define parm
  (make-parameter #f))

(define-syntax dotimes
  (syntax-rules ()
    ((_ ?count . ?body)
     (do ((i 0 (+ 1 i)))
	 ((= i ?count))
       . ?body))
    ))


(parametrise ((check-test-name	'basics))

  (check	;exiting from the dynamic extent by returning
      (with-result
	(dynamic-wind
	    (lambda ()
	      (add-result 'in-guard))
	    (lambda ()
	      (add-result 'body)
	      1)
	    (lambda ()
	      (add-result 'out-guard))))
    => '(1 (in-guard body out-guard)))

  (check	;exiting from the dynamic extent by escaping
      (with-result
	(call/cc
	    (lambda (escape)
	      (dynamic-wind
		  (lambda ()
		    (add-result 'in-guard))
		  (lambda ()
		    (add-result 'body-in)
		    (escape 2)
		    (add-result 'body-out)
		    1)
		  (lambda ()
		    (add-result 'out-guard))))))
    => '(2 (in-guard body-in out-guard)))

;;; --------------------------------------------------------------------
;;; example of state in the dynamic environment

  (internal-body

    (define var)

    (define (step id)
      (add-result (list id var))
      (++ var))

    (define (doit id init)
      (set! var init)
      (dotimes 5
	(step id)))

    (check
	(with-result
	  (doit 'single 0)
	  1)
      => '(1 ((single 0)
	      (single 1)
	      (single 2)
	      (single 3)
	      (single 4))))

    #f)

  (internal-body
    (define var)

    (define (step id)
      (add-result (list id var))
      (++ var))

    (define (doit id init)
      (define local-var)
      (coroutine
	  (lambda ()
	    (dynamic-wind
		(lambda ()
		  (set! var local-var))
		(lambda ()
		  (set! var init)
		  (dotimes 5
		    (step id)
		    (yield)))
		(lambda ()
		  (set! local-var var))))))

    (check
	(with-result
	  (doit 'one  0)
	  (finish-coroutines)
	  1)
      => '(1 ((one 0)
	      (one 1)
	      (one 2)
	      (one 3)
	      (one 4))))

    (check
	(with-result
	  (doit 'one  0)
	  (doit 'two 10)
	  (finish-coroutines)
	  1)
      => '(1 ((one 0) (two 10)
	      (one 1) (two 11)
	      (one 2) (two 12)
	      (one 3) (two 13)
	      (one 4) (two 14))))

    #f)

;;; --------------------------------------------------------------------
;;; the current exception handler in coroutines

  (internal-body

    (define (doit name init)
      (define X init)
      (coroutine
	  (lambda ()
	    (with-exception-handler
		(lambda (E)
		  (++ X))
	      (lambda ()
		(dotimes 5
		  (add-result (list name (raise-continuable (void))))
		  (yield)))))))

    (check
	(with-result
	  (doit 'one 0)
	  (doit 'two 10)
	  (finish-coroutines)
	  1)
      => '(1 ((one 1) (two 11)
	      (one 2) (two 12)
	      (one 3) (two 13)
	      (one 4) (two 14)
	      (one 5) (two 15))))

    #f)

  #t)


(parametrise ((check-test-name	'dynamic-wind))

  ;;Enter and exit the dynamic extent by simple call and return.
  ;;
  (check
      (with-result
	(dynamic-wind
	    (lambda ()
	      (add-result 'in-guard))
	    (lambda ()
	      (add-result 'thunk)
	      1)
	    (lambda ()
	      (add-result 'out-guard))))
    => '(1 (in-guard thunk out-guard)))

  ;;Exiting by calling an escape function.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (dynamic-wind
		  (lambda ()
		    (add-result 'in-guard))
		  (lambda ()
		    (add-result 'thunk-in)
		    (escape 2)
		    (add-result 'thunk-out)
		    1)
		  (lambda ()
		    (add-result 'out-guard))))))
    => '(2 (in-guard thunk-in out-guard)))

  ;;Aborting a dynamic extent by raising an exception.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result 'handler)
		    (escape E))
		(lambda ()
		  (dynamic-wind
		      (lambda ()
			(add-result 'in-guard))
		      (lambda ()
			(add-result 'thunk-in)
			(raise 2)
			(add-result 'thunk-out)
			1)
		      (lambda ()
			(add-result 'out-guard))))))))
    => '(2 (in-guard thunk-in handler out-guard)))

  ;;Entering and exiting with coroutines.
  ;;
  (check
      (with-result
	(coroutine
	    (lambda ()
	      (dynamic-wind
		  (lambda ()
		    (add-result '(1 in-guard)))
		  (lambda ()
		    (add-result '(1.1 thunk))
		    (yield)
		    (add-result '(1.2 thunk))
		    (yield)
		    (add-result '(1.3 thunk)))
		  (lambda ()
		    (add-result '(1 out-guard))))))
	(coroutine
	    (lambda ()
	      (dynamic-wind
		  (lambda ()
		    (add-result '(2 in-guard)))
		  (lambda ()
		    (add-result '(2.1 thunk))
		    (yield)
		    (add-result '(2.2 thunk))
		    (yield)
		    (add-result '(2.3 thunk)))
		  (lambda ()
		    (add-result '(2 out-guard))))))

	(finish-coroutines)
	1)
    => '(1 ((1 in-guard) (1.1 thunk) (1 out-guard)
	    (2 in-guard) (2.1 thunk) (2 out-guard)
	    (1 in-guard) (1.2 thunk) (1 out-guard)
	    (2 in-guard) (2.2 thunk) (2 out-guard)
	    (1 in-guard) (1.3 thunk) (1 out-guard)
	    (2 in-guard) (2.3 thunk) (2 out-guard))))

  (check
      (with-result
	(dynamic-wind
	    (lambda ()
	      (add-result 'outer-in-guard))
	    (lambda ()
	      (add-result 'outer-thunk-in)
	      (call/cc
		  (lambda (escape)
		    (dynamic-wind
			(lambda ()
			  (add-result 'inner-in-guard))
			(lambda ()
			  (add-result 'inner-thunk-in)
			  (escape)
			  (add-result 'inner-thunk-out))
			(lambda ()
			  (add-result 'inner-out-guard)))))
	      (add-result 'outer-thunk-out)
	      1)
	    (lambda ()
	      (add-result 'outer-out-guard))))
    => '(1 (outer-in-guard outer-thunk-in inner-in-guard
			   inner-thunk-in
			   inner-out-guard outer-thunk-out outer-out-guard)))

;;; --------------------------------------------------------------------
;;; raising exceptions from in-guard and out-guard

  (check	;raise exception from in-guard
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (escape E))
		(lambda ()
		  (dynamic-wind
		      (lambda ()
			(add-result 'in-guard)
			(raise 1))
		      (lambda ()
			(add-result 'thunk))
		      (lambda ()
			(add-result 'out-guard))))))))
    => '(1 (in-guard)))

  (check	;raise exception from out-guard
      (with-result
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (escape E))
		(lambda ()
		  (dynamic-wind
		      (lambda ()
			(add-result 'in-guard))
		      (lambda ()
			(add-result 'thunk))
		      (lambda ()
			(add-result 'out-guard)
			(raise 1))))))))
    => '(1 (in-guard thunk out-guard)))

  (check	;raise exception from thunk then from in-guard
      (with-result
	(define count 0)
	(call/cc
	    (lambda (escape)
	      (with-exception-handler
		  (lambda (E)
		    (add-result (list 'handler E))
		    (escape E))
		(lambda ()
		  (dynamic-wind
		      (lambda ()
			(add-result 'in-guard))
		      (lambda ()
			(add-result 'thunk)
			(raise 1))
		      (lambda ()
			(add-result 'out-guard)
			(raise 2))))))))
    => '(2 (in-guard thunk (handler 1) out-guard (handler 2))))

  #t)


(parametrise ((check-test-name	'exception-handlers))

  ;;The handler  is called in the  dynamic environment of  the thunk, so that  it can
  ;;access the dynamic environment that contributed to cause the exception.
  ;;
  (check
      (parametrise ((parm 'outer-parm))
	(with-exception-handler
	    (lambda (E)
	      (parm))
	  (lambda ()
	    (parametrise ((parm 'inner-parm))
	      (raise-continuable 2)))))
    => 'inner-parm)

;;; --------------------------------------------------------------------
;;; blocking exceptions

  ;;To  block a  raised exception  we do  as follows.   With this  implementation the
  ;;handler returns the raised object.
  ;;
  (check
      (internal-body
	(define-syntax with-blocked-exceptions
	  (syntax-rules ()
	    ((_ ?thunk)
	     (call/cc
		 (lambda (reinstate-with-blocked-exceptions-continuation)
		   (with-exception-handler
		       reinstate-with-blocked-exceptions-continuation
		     ?thunk))))
	    ))
	(with-blocked-exceptions
	    (lambda ()
	      (raise 123))))
    => 123)

  ;;To  block a  raised exception  we do  as follows.   With this  implementation the
  ;;handler evaluate  an appropriate thunk  to compute the  return values in  case of
  ;;exception.
  ;;
  (check
      (internal-body
	(define-syntax with-blocked-exceptions
	  (syntax-rules ()
	    ((_ ?exception-retvals-maker ?thunk)
	     (call/cc
		 (lambda (reinstate-with-blocked-exceptions-continuation)
		   (with-exception-handler
		       (lambda (E)
			 (call-with-values
			     (lambda ()
			       (?exception-retvals-maker E))
			   reinstate-with-blocked-exceptions-continuation))
		     ?thunk))))
	    ))
	(with-blocked-exceptions
	    (lambda (E)
	      (values E 1 2 3))
	  (lambda ()
	    (raise 99))))
    => 99 1 2 3)

  #t)


(parametrise ((check-test-name	'current-environment))

  (define-syntax with-blocked-exceptions
    (syntax-rules ()
      ((_ ?exception-retvals-maker ?thunk)
       (call/cc
	   (lambda (reinstate-with-blocked-exceptions-continuation)
	     (with-exception-handler
		 (lambda (E)
		   (call-with-values
		       (lambda ()
			 (?exception-retvals-maker E))
		     reinstate-with-blocked-exceptions-continuation))
	       ?thunk))))
      ))

  (define-syntax with-current-dynamic-environment
    (syntax-rules ()
      ((_ ?exception-retvals-maker ?thunk)
       (call/cc
	   (lambda (return-thunk-with-packed-environment)
	     ((call/cc
		  (lambda (reinstate-target-environment-continuation)
		    (return-thunk-with-packed-environment
		     (lambda ()
		       (call/cc
			   (lambda (reinstate-thunk-call-continuation)
			     (reinstate-target-environment-continuation
			      (lambda ()
				(call-with-values
				    (lambda ()
				      (with-blocked-exceptions
					  ?exception-retvals-maker
					?thunk))
				  reinstate-thunk-call-continuation)))))))))))))
      ))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(parametrise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parametrise ((parm 'inner))
			    (with-current-dynamic-environment
				values
			      (lambda ()
				(set! counter (+ 1 counter))
				(add-result (list 'inside-thunk (parm))))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner)
	    calling-thunk-2 (inside-thunk inner))))

  (check	;raising exception
      (with-result
	(parametrise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parametrise ((parm 'inner))
			    (with-current-dynamic-environment
				values
			      (lambda ()
				(set! counter (+ 1 counter))
				(add-result (list 'inside-thunk (parm)))
				(add-result 'raise-exception)
				(raise 123))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner) raise-exception
	    calling-thunk-2 (inside-thunk inner) raise-exception)))

  #f)


(parametrise ((check-test-name	'coroutines))

  ;;Parameters and coroutines.
  ;;
  (internal-body
    (define parm
      (make-parameter #f))

    (define (doit name init)
      (parametrise ((parm init))
	(coroutine
	    (lambda ()
	      (dotimes 5
		(add-result (list name (parm)))
		(parm (++ (parm)))
		(yield))))))

    (check
	(with-result
	  (doit 'one  0)
	  (doit 'two 10)
	  (finish-coroutines)
	  1)
      => '(1 ((one 0) (two 10)
	      (one 1) (two 11)
	      (one 2) (two 12)
	      (one 3) (two 13)
	      (one 4) (two 14))))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; eval: (put 'dotimes 'scheme-indent-function 1)
;; End:
