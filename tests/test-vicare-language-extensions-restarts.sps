;;;
;;;Part of: Vicare Scheme
;;;Contents: demo for Scheme-flavoured Common Lisp's restarts
;;;Date: Tue Feb 24, 2015
;;;
;;;Abstract
;;;
;;;	Demo script for Scheme-flavoured Common Lisp's condition handlers and restart
;;;	handlers.  Not everything is implemented:
;;;
;;;     * There is no integration with the debugger.
;;;
;;;     * Some facilities are not  implemented; among the missing ones: RESTART-BIND,
;;;       WITH-SIMPLE-RESTART, MUFFLE-WARNING.
;;;
;;;     To understand what is going on here, we should read Common Lisp's Hyper Spec:
;;;
;;;	9.1 Condition System Concepts -
;;;     <http://www.cs.cmu.edu/Groups/AI/html/hyperspec/HyperSpec/Body/sec_9-1.htm>
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
  (vicare language-extensions conditions-and-restarts)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: dynamic environment, Common Lisp's restarts\n")


(parametrise ((check-test-name	'handler-case))

  (check	;no condition
      (with-result
	(handler-case
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 1))
	     (&warning (lambda (E)
			 (add-result 'warning-handler)
			 2)))
	  (add-result 'body)
	  1))
    => '(1 (body)))

  (check	;no condition, :no-error clause
      (with-result
	(handler-case
	    ((&error    (lambda (E)
			  (add-result 'error-handler)
			  1))
	     (&warning  (lambda (E)
			  (add-result 'warning-handler)
			  2))
	     (:no-error (lambda (X)
			  (add-result 'no-error)
			  (* 10 X))))
	  (add-result 'body)
	  1))
    => '(10 (body no-error)))

;;; --------------------------------------------------------------------

  (internal-body	;signalled condition

    (define (doit C)
      (with-result
	(handler-case
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 1))
	     (&warning (lambda (E)
			 (add-result 'warning-handler)
			 2)))
	  (add-result 'body-begin)
	  (signal C)
	  (add-result 'body-normal-return))))

    (check
	(doit (make-error))
      => '(1 (body-begin error-handler)))

    (check
	(doit (make-warning))
      => '(2 (body-begin warning-handler)))

    #| end of body |# )

  ;;Signalled condition, multiple types in single clause.
  ;;
  (internal-body

    (define (doit C)
      (with-result
	(handler-case
	    (((&error &warning) (lambda (E)
				  (add-result 'handler)
				  1)))
	  (add-result 'body-begin)
	  (signal C)
	  (add-result 'body-normal-return))))

    (check
	(doit (make-error))
      => '(1 (body-begin handler)))

    (check
	(doit (make-warning))
      => '(1 (body-begin handler)))

    #| end of body |# )

  ;;Signalled condition, nested HANDLER-CASE uses.
  ;;
  (internal-body

    (define (doit C)
      (with-result
	(handler-case
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 1)))
	  (handler-case
	      ((&warning (lambda (E)
			   (add-result 'warning-handler)
			   2)))
	    (add-result 'body-begin)
	    (signal C)
	    (add-result 'body-normal-return)))))

    (check
	(doit (make-error))
      => '(1 (body-begin error-handler)))

    (check
	(doit (make-warning))
      => '(2 (body-begin warning-handler)))

    #| end of body |# )

;;; --------------------------------------------------------------------
;;; unwind-protect

  (internal-body

    (define (doit C)
      (with-result
	(handler-case
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 1)))
	  (with-unwind-handler
	      (lambda (why)
		(add-result 'outer-unwind-handler))
	    (lambda ()
	      (handler-case
		  ((&warning (lambda (E)
			       (add-result 'warning-handler)
			       2)))
		(with-unwind-handler
		    (lambda (why)
		      (add-result 'inner-unwind-handler))
		  (lambda ()
		    (add-result 'body-begin)
		    (signal C)
		    (add-result 'body-normal-return)))))))))

    (check
	(doit (make-error))
      => '(1 (body-begin
	      inner-unwind-handler
	      outer-unwind-handler
	      error-handler)))

    (check
	(doit (make-warning))
      => '(2 (body-begin
	      inner-unwind-handler
	      warning-handler
	      outer-unwind-handler)))

    #| end of body |# )

  #t)


(parametrise ((check-test-name	'ignore-errors))

  (check	;no condition
      (with-result
	(ignore-errors
	  (add-result 'body)
	  1))
    => '(1 (body)))

;;; --------------------------------------------------------------------

  (internal-body ;signalled condition

    (define (doit C)
      (with-result
	(handler-case
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 1))
	     (&warning (lambda (E)
			 (add-result 'warning-handler)
			 2)))
	  (receive (A B)
	      (ignore-errors
		(add-result 'body-begin)
		(signal C)
		(add-result 'body-normal-return))
	    (values A (and (error? B) 99))))))

    (check
	(doit (make-error))
      => '(#f 99 (body-begin)))

    (check
	(doit (make-warning))
      => '(2 (body-begin warning-handler)))

    #| end of body |# )

  #f)


(parametrise ((check-test-name	'handler-bind))

  (check	;no condition
      (with-result
	(handler-bind
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 1))
	     (&warning (lambda (E)
			 (add-result 'warning-handler)
			 2)))
	  (add-result 'body)
	  1))
    => '(1 (body)))

;;; --------------------------------------------------------------------

  ;;Escaping  from  handler,  which is  the  normal  way  of  accepting to  handle  a
  ;;condition.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (handler-bind
		  ((&error (lambda (E)
			     (add-result 'error-handler)
			     (escape 2))))
		(add-result 'body-begin)
		(signal (make-error))
		(add-result 'body-return)
		1))))
    => '(2 (body-begin error-handler)))

  ;;The first handler declines to handle a raised exception, the second one accepts.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (handler-bind
		  ((&warning (lambda (E)
			       ;;By returning this handler declines.
			       (add-result 'warning-handler)))
		   (&error   (lambda (E)
			       (add-result 'error-handler)
			       (escape 2))))
		(add-result 'body-begin)
		(raise (condition (make-error)
				  (make-warning)))
		(add-result 'body-return)
		1))))
    => '(2 (body-begin warning-handler error-handler)))

  ;;Multiple condition identifiers in the same clause.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (handler-bind
		  (((&warning &error) (lambda (E)
					(add-result 'handler)
					(escape 2))))
		(add-result 'body-begin)
		(signal (make-error))
		(add-result 'body-return)
		1))))
    => '(2 (body-begin handler)))

  ;;Nested HANDLER-BIND uses, returning from handler.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (handler-bind
		  ((&error (lambda (E)
			     (add-result 'outer-error-handler)
			     (escape 2))))
		(handler-bind
		    ((&error (lambda (E)
			       ;;By returning  this handler  declines to  handle this
			       ;;condition.
			       (add-result 'inner-error-handler))))
		  (add-result 'body-begin)
		  (raise (make-error))
		  (add-result 'body-return)
		  1)))))
    => '(2 (body-begin inner-error-handler outer-error-handler)))

;;; --------------------------------------------------------------------
;;; unwind-protect

  (internal-body

    (define (doit C)
      (with-result
	(returnable
	  (handler-bind
	      ((&error   (lambda (E)
			   (add-result 'error-handler)
			   (return 1))))
	    (with-unwind-handler
		(lambda (why)
		  (add-result 'outer-unwind-handler))
	      (lambda ()
		(handler-bind
		    ((&warning (lambda (E)
				 (add-result 'warning-handler)
				 (return 2))))
		  (with-unwind-handler
		      (lambda (why)
			(add-result 'inner-unwind-handler))
		    (lambda ()
		      (add-result 'body-begin)
		      (signal C)
		      (add-result 'body-normal-return))))))))))

    (check
	(doit (make-error))
      => '(1 (body-begin
	      error-handler
	      inner-unwind-handler
	      outer-unwind-handler)))

    (check
	(doit (make-warning))
      => '(2 (body-begin
	      warning-handler
	      inner-unwind-handler
	      outer-unwind-handler)))

    #| end of body |# )

  #t)


(parametrise ((check-test-name	'restarts-finding-and-invoking))

;;;searching

  ;;Search an UNdefined restart.
  ;;
  (check
      (find-restart 'alpha)
    => #f)

  ;;Search an UNdefined restart.
  ;;
  (check
      (restart-case
	  (find-restart 'beta)
	(alpha (lambda ()
		 (add-result 'restart-alpha))))
    => #f)

  ;;Search an UNdefined restart.
  ;;
  (check
      (restart-case
	  (find-restart 'gamma)
	(alpha (lambda ()
		 (add-result 'restart-alpha)))
	(beta  (lambda ()
		 (add-result 'restart-beta)))
	(delta (lambda ()
		 (add-result 'restart-delta))))
    => #f)

;;; --------------------------------------------------------------------
;;; invoking

  ;;Invoke an UNdefined restart.
  ;;
  (check
      (try
	  (invoke-restart 'alpha)
	(catch E
	  ((&undefined-restart-error)
	   1)
	  (else E)))
    => 1)

  ;;Find then invoke a restart in two steps.  Invoking a restart is a non-local exit.
  ;;
  (check
      (with-result
	(restart-case
	    (begin
	      (add-result 'body-in)
	      (let ((restart (find-restart 'alpha)))
		(add-result 'body-invoking)
		(begin0
		    (invoke-restart restart)
		  (add-result 'body-out)
		  1)))
	  (alpha (lambda ()
		   (add-result 'restart-alpha)
		   2))))
    => '(2 (body-in body-invoking restart-alpha)))

  ;;Find and invoke  a restart in a  single step.  Invoking a restart  is a non-local
  ;;exit.
  ;;
  (check
      (with-result
	(restart-case
	    (begin
	      (add-result 'body-in)
	      (begin0
		  (invoke-restart 'alpha)
		(add-result 'body-out)
		1))
	  (alpha (lambda ()
		   (add-result 'restart-alpha)
		   2))))
    => '(2 (body-in restart-alpha)))

  ;;Find and invoke a restart in a single step.  Return multiple values.
  ;;
  (check
      (with-result
	(restart-case
	    (begin
	      (add-result 'body-in)
	      (begin0
		  (invoke-restart 'alpha 3)
		(add-result 'body-out)
		1))
	  (alpha (lambda (obj)
		   (add-result 'restart-alpha)
		   (values 2 obj 4)))))
    => '(2 3 4 (body-in restart-alpha)))

  #f)


(parametrise ((check-test-name	'restarts-and-handler-bind))

  ;;Nested  RESTART-CASE and  HANDLER-BIND.   Signal a  condition,  call a  handler,
  ;;invoke a restart.
  ;;
  (internal-body

    (define (restarts-outside/handlers-inside C)
      (with-result
	(restart-case
	    (handler-bind
		((&error   (lambda (E)
			     (add-result 'error-handler-begin)
			     (invoke-restart 'alpha)
			     (add-result 'error-handler-return)))
		 (&warning (lambda (E)
			     (add-result 'warning-handler-begin)
			     (invoke-restart 'beta)
			     (add-result 'warning-handler-return))))
	      (begin
		(add-result 'body-begin)
		(signal C)
		(add-result 'body-return)))
	  (alpha (lambda ()
		   (add-result 'restart-alpha)
		   1))
	  (beta  (lambda ()
		   (add-result 'restart-beta)
		   2)))))

    (check
	(restarts-outside/handlers-inside (make-error))
      => '(1 (body-begin error-handler-begin restart-alpha)))

    (check
	(restarts-outside/handlers-inside (make-warning))
      => '(2 (body-begin warning-handler-begin restart-beta)))

    #| end of body |# )

  ;;Nested  RESTART-CASE and  HANDLER-BIND.   Signal a  condition,  call a  handler,
  ;;invoke a restart.
  ;;
  (internal-body

    (define (restarts-inside/handlers-outside C)
      (with-result
	(handler-bind
	    ((&error   (lambda (E)
			 (add-result 'error-handler-begin)
			 (invoke-restart 'alpha)
			 (add-result 'error-handler-return)))
	     (&warning (lambda (E)
			 (add-result 'warning-handler-begin)
			 (invoke-restart 'beta)
			 (add-result 'warning-handler-return))))
	  (restart-case
	      (begin
		(add-result 'body-begin)
		(signal C)
		(add-result 'body-return))
	    (alpha (lambda ()
		     (add-result 'restart-alpha)
		     1))
	    (beta  (lambda ()
		     (add-result 'restart-beta)
		     2))))))

    (check
	(restarts-inside/handlers-outside (make-error))
      => '(1 (body-begin error-handler-begin restart-alpha)))

    (check
	(restarts-inside/handlers-outside (make-warning))
      => '(2 (body-begin warning-handler-begin restart-beta)))

    #| end of body |# )

  ;;Nested  RESTART-CASE and  HANDLER-BIND.   Signal a  condition,  call a  handler,
  ;;invoke a restart.
  ;;
  (internal-body

    (define (restarts-inside/nested-handlers C)
      (with-result
	(handler-bind
	    ((&error   (lambda (E)
			 (add-result 'error-handler-begin)
			 (invoke-restart 'alpha)
			 (add-result 'error-handler-return))))
	  (handler-bind
	      ((&warning (lambda (E)
			   (add-result 'warning-handler-begin)
			   (invoke-restart 'beta)
			   (add-result 'warning-handler-return))))
	    (restart-case
		(begin
		  (add-result 'body-begin)
		  (signal C)
		  (add-result 'body-return))
	      (alpha (lambda ()
		       (add-result 'restart-alpha)
		       1))
	      (beta  (lambda ()
		       (add-result 'restart-beta)
		       2)))))))

    (check
	(restarts-inside/nested-handlers (make-error))
      => '(1 (body-begin error-handler-begin restart-alpha)))

    (check
	(restarts-inside/nested-handlers (make-warning))
      => '(2 (body-begin warning-handler-begin restart-beta)))

    #| end of LET |# )

  ;;Nested  RESTART-CASE and  HANDLER-BIND.   Signal a  condition,  call a  handler,
  ;;invoke a restart.
  ;;
  (internal-body

    (define (nested-restarts/handlers-outside C)
      (with-result
	(handler-bind
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 (invoke-restart 'alpha)))
	     (&warning (lambda (E)
			 (add-result 'warning-handler)
			 (cond ((find-restart 'beta)
				=> (lambda (handler)
				     (invoke-restart handler)))))))

	  (restart-case
	      (restart-case
		  (begin
		    (add-result 'body-begin)
		    (signal C)
		    (add-result 'body-return))
		(alpha (lambda ()
			 (add-result 'restart-alpha)
			 1)))
	    (beta  (lambda ()
		     (add-result 'restart-beta)
		     2))))))

    (check
	(nested-restarts/handlers-outside (make-error))
      => '(1 (body-begin error-handler restart-alpha)))

    (check
	(nested-restarts/handlers-outside (make-warning))
      => '(2 (body-begin warning-handler restart-beta)))

    #| end of body |# )

  ;;Nested  RESTART-CASE  and HANDLER-BIND.   Signal  a  condition, call  the  first
  ;;handler, the first handler declines, call the second handler, invoke a restart.
  ;;
  (check
      (with-result
	(handler-bind
	    ((&message (lambda (E)
			 (add-result 'outer-message-handler-begin)
			 (invoke-restart 'alpha E)
			 (add-result 'outer-message-handler-return))))
	  (handler-bind
	      ((&message (lambda (E)
			   ;;By returning  this handler refuses  to handle
			   ;;the condition.
			   (add-result 'inner-message-handler))))
	    (restart-case
		(begin
		  (add-result 'body-begin)
		  (signal (make-message-condition "ciao"))
		  (add-result 'body-return))
	      (alpha (lambda (E)
		       (add-result 'alpha-restart)
		       (condition-message E)))))))
    => '("ciao" (body-begin
		 inner-message-handler
		 outer-message-handler-begin
		 alpha-restart)))

  #f)


(parametrise ((check-test-name	'restarts-and-declining-handlers))

  ;;No handlers installed.  Signal a condition and cause SIGNAL to return.
  ;;
  (check
      (with-result
	(with-return-to-signal-on-unhandled-exception
	  (add-result 'body-in)
	  (signal (make-error))
	  (add-result 'body-out)
	  1))
    => '(1 (body-in body-out)))

  ;;No handlers installed.  Raise an exception  with the standard RAISE and show that
  ;;it goes through the "return to signal" syntax.
  ;;
  (check
      (with-result
	(try
	    (with-return-to-signal-on-unhandled-exception
	      (add-result 'body-in)
	      (raise (make-error))
	      (add-result 'body-out)
	      1)
	  (catch E
	    ((&error)
	     (add-result 'catch-error)
	     2))))
    => '(2 (body-in catch-error)))

  ;;No handlers  installed.  Raise an  exception with the  standard RAISE-CONTINUABLE
  ;;and show that it goes through the "return to signal" syntax.
  ;;
  (check
      (with-result
	(try
	    (with-return-to-signal-on-unhandled-exception
	      (add-result 'body-in)
	      (raise-continuable (make-error))
	      (add-result 'body-out)
	      1)
	  (catch E
	    ((&error)
	     (add-result 'catch-error)
	     2))))
    => '(2 (body-in catch-error)))

  ;;No handlers  installed.  Raise an  exception with the  standard RAISE-CONTINUABLE
  ;;and show that it goes through the "return to signal" syntax.
  ;;
  (check
      (with-result
	(with-exception-handler
	    (lambda (E)
	      (add-result 'exception-handler))
	  (lambda ()
	    (with-return-to-signal-on-unhandled-exception
	      (add-result 'body-in)
	      (raise-continuable (make-error))
	      (add-result 'body-out)
	      1))))
    => '(1 (body-in exception-handler body-out)))

;;; --------------------------------------------------------------------

  ;;Nested  RESTART-CASE  and HANDLER-BIND.   Signal  a  condition, call  the  first
  ;;handler, the first handler declines, call  the second handler, the second handler
  ;;declines, SIGNAL returns.
  ;;
  (check
      (with-result
	(with-return-to-signal-on-unhandled-exception
	  (handler-bind
	      ((&error (lambda (E)
			 ;;By returning, this handler declines to handle the condition.
			 (add-result 'outer-error-handler))))
	    (handler-bind
		((&error (lambda (E)
			   ;;By  returning,   this  handler  declines  to   handle  the
			   ;;condition.
			   (add-result 'inner-error-handler))))
	      (add-result 'body-in)
	      (signal (make-error))
	      (add-result 'body-out)
	      1))))
    => '(1 (body-in inner-error-handler outer-error-handler body-out)))

  #f)


(parametrise ((check-test-name	'compute-restarts))

  (check
      (compute-restarts)
    => '())

  (check
      (map restart-name
	(restart-case
	    (compute-restarts)
	  (alpha void)
	  (beta  void)))
    => '(alpha beta))

  (check
      (map restart-name
	(restart-case
	    (restart-case
		(compute-restarts)
	      (alpha void)
	      (beta  void))
	  (delta void)
	  (gamma void)))
    => '(alpha beta delta gamma))

  (check
      (map restart-name
	(restart-case
	    (restart-case
		(restart-case
		    (compute-restarts)
		  (alpha void)
		  (beta  void))
	      (delta void)
	      (gamma void))
	  (chi void)
	  (xi  void)))
    => '(alpha beta delta gamma chi xi))

  #t)


(parametrise ((check-test-name	'standard-restart-functions))

;;; USE-VALUE

  ;;Signal condition, call a handler, call a USE-VALUE restart.
  ;;
  (check
      (with-result
	(handler-bind
	    ((&error (lambda (E)
		       (add-result 'error-handler-begin)
		       (use-value "ciao")
		       (add-result 'error-handler-return))))
	  (restart-case
	      (begin
		(add-result 'body-begin)
		(signal (make-error))
		(add-result 'body-return))
	    (use-value (lambda (value)
			 (add-result 'restart-use-value)
			 value)))))
    => '("ciao" (body-begin error-handler-begin restart-use-value)))

  ;;Call a USE-VALUE restart, no USE-VALUE handler is defined.
  ;;
  (check
      (with-result
	(add-result 'body-begin)
	(begin0
	    (use-value 1)
	  (add-result 'body-return)))
    => '(#f (body-begin body-return)))

;;; --------------------------------------------------------------------
;;; STORE-VALUE

  ;;Signal condition, call a handler, call a STORE-VALUE restart.
  ;;
  (check
      (with-result
	(handler-bind
	    ((&error (lambda (E)
		       (add-result 'error-handler-begin)
		       (store-value "ciao")
		       (add-result 'error-handler-return))))
	  (restart-case
	      (begin
		(add-result 'body-begin)
		(signal (make-error))
		(add-result 'body-return))
	    (store-value (lambda (value)
			   (add-result 'restart-store-value)
			   value)))))
    => '("ciao" (body-begin error-handler-begin restart-store-value)))

  ;;Call a STORE-VALUE restart, no STORE-VALUE handler is defined.
  ;;
  (check
      (with-result
	(add-result 'body-begin)
	(begin0
	    (store-value 1)
	  (add-result 'body-return)))
    => '(#f (body-begin body-return)))

;;; --------------------------------------------------------------------
;;; STORE-VALUE/USE-VALUE

  ;;Adapted example from Common Lisp's Hyper Spec.
  ;;
  (internal-body

    (define-condition-type &unbound-symbol-error
	&error
      make-unbound-symbol-error
      unbound-symbol-error?)

    (define* (careful-symbol-value {sym symbol?})
      (restart-case
	  (if (symbol-bound? sym)
	      (symbol-value sym)
	    (signal (make-unbound-symbol-error)))
	(use-value   (lambda (obj) obj))
	(store-value (lambda (obj)
		       (set-symbol-value! sym obj)
		       obj))))

    (check
	(let ((sym (gensym)))
	  (set-symbol-value! sym 1)
	  (careful-symbol-value sym))
      => 1)

    (check
	(let ((sym (gensym)))
	  (handler-bind
	      ((&unbound-symbol-error (lambda (E)
					(use-value 1))))
	    (careful-symbol-value sym)))
      => 1)

    (check
	(let ((sym (gensym)))
	  (handler-bind
	      ((&unbound-symbol-error (lambda (E)
					(store-value 1))))
	    (values (careful-symbol-value sym)
		    (careful-symbol-value sym))))
      => 1 1)

    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------
;;; CONTINUE

  ;;Signal condition, call a handler, call a CONTINUE restart.
  ;;
  (check
      (with-result
	(handler-bind
	    ((&error (lambda (E)
		       (add-result 'error-handler-begin)
		       (continue-restart)
		       (add-result 'error-handler-return))))
	  (restart-case
	      (begin
		(add-result 'body-begin)
		(signal (make-error))
		(add-result 'body-return))
	    (continue (lambda ()
			(add-result 'restart-continue)
			2)))))
    => '(2 (body-begin error-handler-begin restart-continue)))

  ;;CONTINUE example derived from Common Lisp's Hyper Spec.
  ;;
  (check
      (internal-body

	(define (real-sqrt n)
	  (when (negative? n)
	    (set! n (- n))
	    (restart-case
		(signal (make-error))
	      (continue (lambda ()
			  (add-result 'continue-restart)))))
	  (sqrt n))

	(with-result
	  (handler-bind
	      ((&error (lambda (E)
			 (add-result 'error-handler)
			 (continue-restart))))
	    (add-result 'body-enter)
	    (begin0
		(real-sqrt -9)
	      (add-result 'body-return)))))
    => '(3 (body-enter error-handler continue-restart body-return)))

  ;;Call a CONTINUE restart, no CONTINUE handler is defined.
  ;;
  (check
      (with-result
	(try
	    (restart-case
		(begin
		  (add-result 'body-begin)
		  (begin0
		      (continue-restart)
		    (add-result 'body-return))))
	  (catch E
	    ((&restarts-control-error)
	     (add-result 'catch-control-error)
	     2)
	    (else
	     (print-condition E)
	     3))))
    => '(#f (body-begin body-return)))

  ;;Signal condition, call  the inner handler, call an CONTINUE  restart, no CONTINUE
  ;;handler is defined, call the outer handler, return.
  ;;
  (check
      (with-result
	(with-return-to-signal-on-unhandled-exception
	  (handler-bind
	      ((&error (lambda (E)
			 (add-result 'outer-error-handler))))
	    (handler-bind
		((&error (lambda (E)
			   (add-result 'inner-error-handler-begin)
			   (continue-restart)
			   (add-result 'inner-error-handler-return))))
	      (add-result 'body-begin)
	      (signal (make-error))
	      (add-result 'body-return)
	      1))))
    => '(1 (body-begin
	    inner-error-handler-begin
	    inner-error-handler-return
	    outer-error-handler
	    body-return)))

;;; --------------------------------------------------------------------
;;; ABORT

  ;;Signal condition, call a handler, call an ABORT restart.
  ;;
  (check
      (with-result
	(handler-bind
	    ((&error (lambda (E)
		       (add-result 'error-handler-begin)
		       (abort-restart)
		       (add-result 'error-handler-return))))
	  (restart-case
	      (begin
		(add-result 'body-begin)
		(signal (make-error))
		(add-result 'body-return))
	    (abort (lambda ()
		     (add-result 'restart-abort)
		     2)))))
    => '(2 (body-begin error-handler-begin restart-abort)))

  ;;Call an ABORT restart, no ABORT handler is defined.
  ;;
  (check
      (with-result
	(try
	    (begin
	      (add-result 'body-begin)
	      (abort-restart)
	      (add-result 'body-return))
	  (catch E
	    ((&restarts-control-error)
	     (add-result 'catch-control-error)
	     2)
	    (else E))))
    => '(2 (body-begin catch-control-error)))

  ;;Signal condition,  call a  handler, call  an ABORT restart,  no ABORT  handler is
  ;;defined.
  ;;
  (check
      (with-result
	(try
	    (handler-bind
		((&error (lambda (E)
			   (add-result 'error-handler-begin)
			   (abort-restart)
			   (add-result 'error-handler-return))))
	      (add-result 'body-begin)
	      (signal (make-error))
	      (add-result 'body-return))
	  (catch E
	    ((&restarts-control-error)
	     (add-result 'catch-control-error)
	     2)
	    (else E))))
    => '(2 (body-begin error-handler-begin catch-control-error)))

  #f)


(parametrise ((check-test-name	'restarts-conditions-association))

  ;;Install a  restart handler, establish  an association between a  condition object
  ;;and the restart object, call FIND-RESTART with the condition argument, no restart
  ;;objects without association found, FIND-RESTART returns false.
  ;;
  (check
      (let ((C (make-error)))
	(restart-case
	    (with-condition-restarts C
	      (list (find-restart 'alpha))
	      (find-restart 'alpha C))
	  ;;This is associated: it is skipped.
	  (alpha (lambda () 1))))
    => #f)

  ;;Install  nested restart  handlers with  the same  name, establish  an association
  ;;between a  condition object and  the innermost restart object,  call FIND-RESTART
  ;;with the condition argument, invoke restart.
  ;;
  (check
      (let ((C (make-error)))
	(restart-case
	    (restart-case
		(with-condition-restarts C
		  (list (find-restart 'alpha))
		  (invoke-restart (find-restart 'alpha C)))
	      ;;This is associated: it is skipped.
	      (alpha (lambda () 1)))
	  ;;This is not associated: it is selected.
	  (alpha (lambda () 2))))
    => 2)

  ;;Install  a  condition  handler,  install  restart  a  handler  and  establish  an
  ;;association between  the raised condition  object and the restart  object, search
  ;;the restart, no matching restart without association, SIGNAL returns.
  ;;
  (check
      (with-return-to-signal-on-unhandled-exception
	(handler-bind
	    ((&error (lambda (E)
		       (cond ((find-restart 'alpha E)
			      => invoke-restart)
			     (else #f)))))
	  (restart-case
	      (signal (make-error))
	    ;;This is associated: it is skipped.
	    (alpha (lambda ()
		     (add-result 'restart-alpha)
		     1)))
	  123))
    => 123)

  ;;Install a condition handler, install nested  restart handlers with the same name,
  ;;establish an association between a  raised condition object and innermost restart
  ;;object, invoke the restart, the outermost restart is selected.
  ;;
  (check
      (handler-bind
	  ((&error (lambda (E)
		     (invoke-restart (find-restart 'alpha E)))))
	(restart-case
	    (restart-case
		(signal (make-error))
	      ;;This is associated: it is skipped.
	      (alpha (lambda () 1)))
	  ;;This is not associated: it is selected.
	  (alpha (lambda () 2))))
    => 2)

;;; --------------------------------------------------------------------

  ;;Install restart handlers, establish an association between a condition object and
  ;;the restart  handlers, call FIND-RESTART  without condition argument,  invoke the
  ;;restart object.
  ;;
  (check
      (with-result
	(define C
	  (make-error))
	(restart-case
	    (with-condition-restarts C
	      (list (find-restart 'alpha)
		    (find-restart 'beta))
	      (invoke-restart (find-restart 'alpha)))
	  (alpha (lambda ()
		   (add-result 'alpha)
		   1))
	  (beta  (lambda ()
		   (add-result 'beta)
		   2))))
    => '(1 (alpha)))

  ;;FIND-RESTART with condition argument, no restart without association.
  ;;
  (internal-body

    (define (doit restart-name)
      (with-result
	(define C
	  (make-error))
	(restart-case
	    (with-condition-restarts C
	      (list (find-restart 'alpha)
		    (find-restart 'beta))
	      (find-restart restart-name C))
	  (alpha (lambda ()
		   (add-result 'alpha)
		   1))
	  (beta  (lambda ()
		   (add-result 'beta)
		   2)))))

    (check
	(doit 'alpha)
      => '(#f ()))

    (check
	(doit 'beta)
      => '(#f ()))

    #| end of INTERNAL-BODY |# )

  ;;Find a restart with condition object different from the raised one.
  ;;
  (check
      (handler-bind
	  ((&error (lambda (E)
		     (invoke-restart (find-restart 'alpha (make-error))))))
	(restart-case
	    (signal (make-error))
	  (alpha (lambda () 2))))
    => 2)

  ;;Find a restart with condition object different from the raised one.
  ;;
  (check
      (handler-bind
	  ((&error (lambda (E)
		     (invoke-restart (find-restart 'alpha E)))))
	(handler-bind
	    ((&error (lambda (E)
		       (signal (make-error)))))
	  (restart-case
	      (signal (make-error))
	    (alpha (lambda () 2)))))
    => 2)

  ;;FIND-RESTART with condition argument, outer restart without association.
  ;;
  (internal-body

    (define (doit restart-name)
      (with-result
	(define C
	  (make-error))
	(restart-case
	    (restart-case
		(with-condition-restarts C
		  (list (find-restart 'alpha)
			(find-restart 'beta))
		  (invoke-restart (find-restart restart-name C)))
	      (alpha (lambda ()
		       (add-result 'inner-alpha)
		       1))
	      (beta  (lambda ()
		       (add-result 'inner-beta)
		       2)))
	  (alpha (lambda ()
		   (add-result 'outer-alpha)
		   3))
	  (beta  (lambda ()
		   (add-result 'outer-beta)
		   4)))))

    (check
	(doit 'alpha)
      => '(3 (outer-alpha)))

    (check
	(doit 'beta)
      => '(4 (outer-beta)))

    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------

  (check
      (with-result
	(handler-bind
	    ((&error (lambda (E)
		       ;;Invoke the "use-case"  restart that is not  associated to E,
		       ;;which is the signalled object.
		       (add-result 'error-handler)
		       (use-value 1 E))))
	  (restart-case
	      (restart-case
		  (signal (make-error))
		;;This one *is* associated to the signalled object.
		(use-value (lambda (obj)
			     (add-result 'associated-use-value)
			     2)))
	    ;;This is *not* associated to the raised object.
	    (use-value (lambda (obj)
			 (add-result 'not-associated-use-value)
			 3)))))
    => '(3 (error-handler not-associated-use-value)))

  #t)


(parametrise ((check-test-name	'restarts-unwind-protect))

  (check
      (with-result
	(restart-case
	    (with-unwind-handler
		(lambda (why)
		  (add-result 'unwind-handler))
	      (lambda ()
		(add-result 'body-enter)
		(invoke-restart 'alpha)
		(add-result 'body-return)))
	  (alpha (lambda ()
		   (add-result 'restart-alpha)
		   1))))
    => '(1 (body-enter restart-alpha unwind-handler)))

  (internal-body

    (define (doit C)
      (with-result
	(returnable
	  (handler-bind
	      ((&error   (lambda (E)
			   (add-result 'error-handler)
			   (return 1))))
	    (with-unwind-handler
		(lambda (why)
		  (add-result 'outer-unwind-handler))
	      (lambda ()
		(handler-bind
		    ((&warning (lambda (E)
				 (add-result 'warning-handler)
				 (return 2))))
		  (with-unwind-handler
		      (lambda (why)
			(add-result 'inner-unwind-handler))
		    (lambda ()
		      (add-result 'body-begin)
		      (signal C)
		      (add-result 'body-normal-return))))))))))

    (check
	(doit (make-error))
      => '(1 (body-begin
	      error-handler
	      inner-unwind-handler
	      outer-unwind-handler)))

    (check
	(doit (make-warning))
      => '(2 (body-begin
	      warning-handler
	      inner-unwind-handler
	      outer-unwind-handler)))

    #| end of body |# )

  #f)


(parametrise ((check-test-name	'misc))

  ;;Syntax error: non-symbol object as restart name.
  ;;
  ;; (restart-case
  ;;     (void)
  ;;   (alpha  (lambda (E) 1))
  ;;   ("beta" (lambda (E) 2)))

  ;;Syntax error: duplicate restart symbol.
  ;;
  ;; (restart-case
  ;;     (void)
  ;;   (alpha (lambda (E) 1))
  ;;   (alpha (lambda (E) 2)))

  #f)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'handler-case			'scheme-indent-function 1)
;; eval: (put 'ignore-errors			'scheme-indent-function 0)
;; eval: (put 'handler-bind			'scheme-indent-function 1)
;; eval: (put 'restart-case			'scheme-indent-function 1)
;; eval: (put 'with-condition-restarts		'scheme-indent-function 1)
;; eval: (put 'raise-undefined-restart-error	'scheme-indent-function 1)
;; eval: (put 'raise-restart-internal-error	'scheme-indent-function 1)
;; eval: (put 'signal-restarts-control-error	'scheme-indent-function 1)
;; eval: (put 'with-return-to-signal-on-unhandled-exception 'scheme-indent-function 0)
;; End:
