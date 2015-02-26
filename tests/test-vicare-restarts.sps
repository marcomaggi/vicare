;;;
;;;Part of: Vicare Scheme
;;;Contents: demo for Scheme-flavored Common Lisp's restarts
;;;Date: Tue Feb 24, 2015
;;;
;;;Abstract
;;;
;;;	This is officially one more test  file for the dynamic environment.  Actually
;;;	it is a demo script for  Scheme-flavored Common Lisp's condition handlers and
;;;	restart handlers.
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
(check-display "*** testing Vicare: dynamic environment, Common Lisp's restarts\n")


;;;; implementation of HANDLERS-CASE

(define-syntax* (handlers-case stx)
  ;;Evaluate body forms in a dynamic  environment in which new exception handlers are
  ;;installed;   it  is   capable   of  handling   exceptions   raised  with   RAISE,
  ;;RAISE-CONTINUABLE and SIGNAL.  Basically behave like R6RS's GUARD syntax.
  ;;
  ;;The syntax is:
  ;;
  ;;   (handlers-case (?clause ...) ?body0 ?body ...)
  ;;
  ;;   ?clause = (?typespec ?condition-handler)
  ;;           | (:no-error ?no-error-handler)
  ;;
  ;;Every  ?TYPESPEC is  meant to  be a  non-empty proper  list of  identifiers, each
  ;;usable as second argument to CONDITION-IS-A?.
  ;;
  ;;Every  ?HANDLER must  be  an expression  evaluating to  a  procedure accepting  a
  ;;condition  object as  single  argument; the  condition object  can  be simple  or
  ;;compound.
  ;;
  ;;In the no-error clause: ":no-error"  must be the actual symbol; ?NO-ERROR-HANDLER
  ;;must be an  expression evaluating to a  procedure.
  ;;
  ;;If the body performs a normal return:
  ;;
  ;;* If the  ":no-error" clause is missing:  the values returned by  the body become
  ;;  the values returned by HANDLERS-CASE.
  ;;
  ;;*  If the  ":no-error"  clause  is present:  the  procedure ?NO-ERROR-HANDLER  is
  ;;  applied  to the returned values;  the return values of  such application become
  ;;  the return values of HANDLERS-CASE.
  ;;
  ;;If an  exception is raised  (in Common Lisp jargon:  a condition is  signaled): a
  ;;condition  handler matching  the raised  object is  searched in  the sequence  of
  ;;clauses, left-to-right:
  ;;
  ;;* If a clause matches: the dynamic extent of the body is terminated, the ?HANDLER
  ;;  is  applied to  the raised  object and  the return  values of  such application
  ;;  become the return values of HANDLERS-CASE.
  ;;
  ;;* If no clause matches: the raised object is re-raised with RAISE-CONTINUABLE.
  ;;
  (define (main stx)
    (syntax-case stx ()
      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))

      ((_ (?clause0 ?clause ...) ?body0 ?body ...)
       (let ((guarded-expr (with-syntax
			       (((VAR) (generate-temporaries '(E))))
			     (with-syntax
				 (((CLAUSE0 CLAUSE ...)
				   (%process-clauses #'VAR #'(?clause0 ?clause ...))))
			       #'(guard (VAR CLAUSE0 CLAUSE ...)
				   ?body0 ?body ...)))))
	 (if no-error-expr
	     #`(call-with-values
		   (lambda () #,guarded-expr)
		 #,no-error-expr)
	   guarded-expr)))
      ))

  (define no-error-expr #f)

  (define (%process-clauses var clause*)
    (syntax-case clause* ()
      (()
       '())

      (((?no-error ?handler) . ?rest)
       (let ((X #'?no-error))
	 (and (identifier? X)
	      (eq? ':no-error (syntax->datum X))))
       (if no-error-expr
	   (synner "invalid multiple definition of :no-error clause"
		   #'(?no-error ?handler))
	 (begin
	   (set! no-error-expr #'?handler)
	   (%process-clauses var #'?rest))))

      (((?typespec ?handler) . ?rest)
       (cons #`(#,(%process-typespec var #'?typespec) (?handler #,var))
	     (%process-clauses var #'?rest)))

      ((?clause . ?rest)
       (synner "invalid clause" #'?clause))))

  (define (%process-typespec var spec)
    (syntax-case spec ()
      (?condition
       (identifier? #'?condition)
       #`(condition-is-a? #,var ?condition))

      ((?condition0 ?condition ...)
       (all-identifiers? (syntax->list #'(?condition0 ?condition ...)))
       #`(or (condition-is-a? #,var ?condition0)
	     (condition-is-a? #,var ?condition)
	     ...))
      (_
       (synner "invalid typespec syntax" spec))))

  (main stx))


;;;; implementation of HANDLERS-BIND

(define-syntax* (handlers-bind stx)
  ;;Evaluate body forms in a dynamic  environment in which new exception handlers are
  ;;installed;   it  is   capable   of  handling   exceptions   raised  with   RAISE,
  ;;RAISE-CONTINUABLE  and  SIGNAL.   Not quite  like  R6RS's  WITH-EXCEPTION-HANDLER
  ;;syntax.
  ;;
  ;;The syntax is:
  ;;
  ;;   (handlers-bind (?clause ...) ?body0 ?body ...)
  ;;
  ;;   ?clause = (?typespec ?condition-handler)
  ;;
  ;;Every  ?TYPESPEC is  meant to  be a  non-empty proper  list of  identifiers, each
  ;;usable as second argument to CONDITION-IS-A?.
  ;;
  ;;Every  ?HANDLER must  be  an expression  evaluating to  a  procedure accepting  a
  ;;condition  object as  single  argument; the  condition object  can  be simple  or
  ;;compound.
  ;;
  ;;If the body performs a normal return:  the values returned by the body become the
  ;;values returned by HANDLERS-BIND.
  ;;
  ;;If an  exception is raised  (in Common Lisp jargon:  a condition is  signaled): a
  ;;condition  handler matching  the raised  object is  searched in  the sequence  of
  ;;clauses, left-to-right:
  ;;
  ;;* If  a clause  matches: its  ?HANDLER is applied  to the  raised object  and the
  ;;  return values of such application become the return values of HANDLERS-BIND.
  ;;
  ;;* If no clause matches: the raised object is re-raised with RAISE-CONTINUABLE.
  ;;
  ;;The handlers are called with a  continuation whose dynamic environment is that of
  ;;the call to RAISE, RAISE-CONTINUABLE or  SIGNAL that raised the exception; except
  ;;that  the  current  exception  handler  is   the  one  that  was  in  place  when
  ;;HANDLERS-BIND was evaluated.
  ;;
  ;;When a ?HANDLER is applied to the raised condition object:
  ;;
  ;;* If it  accepts to handle the  condition: it must perform a  non-local exit, for
  ;;  example by invoking a restart.
  ;;
  ;;* If  it declines to handle  the condition: it  must perform a normal  return; in
  ;;  this case the raised object is re-raised using RAISE-CONTINUABLE.
  ;;
  (define (main stx)
    (syntax-case stx ()
      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))

      ((_ ((?typespec ?handler) ...) ?body0 ?body ...)
       (with-syntax
	   (((VAR) (generate-temporaries '(E))))
	 (with-syntax
	     (((PRED ...) (%process-typespecs #'VAR #'(?typespec ...))))
	   #'(with-exception-handler
		 (lambda (VAR)
		   (when PRED
		     (?handler VAR))
		   ...
		   ;;If  we are  here either  no ?TYPESPEC  matched E  or a  ?HANDLER
		   ;;returned.   Let's  search for  another  handler  in the  uplevel
		   ;;dynamic environment.
		   (raise-continuable VAR))
	       (lambda () ?body0 ?body ...)))))
      ))

  (define (%process-typespecs var spec*)
    (syntax-case spec* ()
      (()
       '())
      ((?typespec . ?rest)
       (identifier? #'?typespec)
       (cons #`(condition-is-a? #,var ?typespec)
	     (%process-typespecs       var #'?rest)))
      ((?typespec . ?rest)
       (cons (%process-single-typespec var #'?typespec)
	     (%process-typespecs       var #'?rest)))
      ))

  (define (%process-single-typespec var spec)
    (syntax-case spec ()
      ((?condition0 ?condition ...)
       (all-identifiers? (syntax->list #'(?condition0 ?condition ...)))
       #`(or (condition-is-a? #,var ?condition0)
	     (condition-is-a? #,var ?condition)
	     ...))
      (_
       (synner "invalid typespec syntax" spec))
      ))

  (main stx))


;;;; restarts: condition objects and errors

;;Base type of al the error conditions associated to the restarts mechanism.
;;
(define-condition-type &restarts-error
    &error
  make-restarts-condition
  restarts-condition?)

;;; --------------------------------------------------------------------

;;To be used to tag an exception as "internal error in the restarts mechanism".
;;
(define-condition-type &restart-internal-error
    &restarts-error
  make-restart-internal-error
  restart-internal-error?)

(define (raise-restart-internal-error who message . irritants)
  (raise
   (condition (make-restart-internal-error)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))

;;; --------------------------------------------------------------------

;;To be used to tag an exception as "internal error in the restarts mechanism".
;;
(define-condition-type &restarts-control-error
    &restarts-error
  make-restarts-control-error
  restarts-control-error?)

(define (signal-restarts-control-error who message . irritants)
  (signal
   (condition (make-restarts-control-error)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))

;;; --------------------------------------------------------------------

;;To be used to tag an exception as "attempt to use an undefined restart".
;;
(define-condition-type &undefined-restart-error
    &restarts-control-error
  make-undefined-restart-error
  undefined-restart-error?)

(define (raise-undefined-restart-error who message . irritants)
  (raise
   (condition (make-undefined-restart-error)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))


;;;; restarts: internal state

(module RESTARTS-INTERNAL-STATE
  (installed-restart-point
   installed-restart-point.signaled-object
   installed-restart-point.signaled-object-set!
   installed-restart-point.restart-proc
   installed-restart-point.restart-handlers
   make-restart-point)

  (define-record-type <restart-point>
    (nongenerative vicare:restarts:<restart-point>)
    (fields (mutable signaled-object)
		;The condition object signaled by SIGNAL.
	    (immutable restart-proc)
		;The escape procedure to be applied to the return values of a restart
		;handler to jump back to a use of RESTART-CASE.
	    (immutable restart-handlers)
		;List of alists managed as a  stack of alists.  Each alist represents
		;the  restart  handlers  installed  by  the  expansion  of  a  single
		;RESTART-CASE use.
	    #| end of FIELDS |# )
    (protocol
     (lambda (make-record)
       (lambda (restart-proc handlers-alist)
	 (make-record #f restart-proc handlers-alist))))
    #| end of DEFINE-RECORD-TYPE |# )

  (define (default-restart-proc . args)
    (raise-restart-internal-error __who__
      "invalid call to restart procedure outside RESTART-CASE environment"))

  (define installed-restart-point
    (make-parameter (make-<restart-point> default-restart-proc '())))

  (define (make-restart-point restart-proc handlers-alist)
    (make-<restart-point> restart-proc
      (cons handlers-alist (installed-restart-point.restart-handlers))))

  (define-syntax-rule (installed-restart-point.signaled-object)
    (<restart-point>-signaled-object (installed-restart-point)))

  (define-syntax-rule (installed-restart-point.signaled-object-set! ?obj)
    (<restart-point>-signaled-object-set! (installed-restart-point) ?obj))

  (define-syntax-rule (installed-restart-point.restart-proc)
    (<restart-point>-restart-proc (installed-restart-point)))

  (define-syntax-rule (installed-restart-point.restart-handlers)
    (<restart-point>-restart-handlers (installed-restart-point)))

  #| end of module |# )


;;;; restarts: public interface

(module (signal
	 with-return-to-signal-on-unhandled-exception
	 restart-case
	 find-restart
	 invoke-restart
	 ;;
	 use-value
	 continue-restart
	 abort-restart)
  (import RESTARTS-INTERNAL-STATE)

  ;;Condition object to be added to every  condition raised by SIGNAL.  It is used by
  ;;WITH-RETURN-TO-SIGNAL-ON-UNHANDLED-EXCEPTION  to   distinguish  between  signaled
  ;;conditions  and  exceptions  raised  by  a non-SIGNAL  invokation  of  RAISE  and
  ;;RAISE-CONTINUABLE.
  ;;
  (define-condition-type &signal
      &condition
    make-signal-condition
    signal-condition?)

  ;;Condition object of type "&signal" which  is always added to the condition raised
  ;;by  SIGNAL.   Only  one  needs  to  be instantiated.   It  exists  to  allow  the
  ;;implementation  of  WITH-RETURN-TO-SIGNAL-ON-UNHANDLED-EXCEPTION which  needs  to
  ;;distinguish between  signaled conditions  and exceptions  raised by  a non-SIGNAL
  ;;invokation of RAISE and RAISE-CONTINUABLE.
  ;;
  (define-constant SIGNAL-CONDITION
    (make-signal-condition))

  (define* (signal {C condition?})
    ;;Signal  a condition  by raising  it  with RAISE-CONTINUABLE.   Perform what  is
    ;;needed to handle the restarts protocol.
    ;;
    ;;As defined  by Common Lisp:  SIGNAL should return  if no handler  accepting the
    ;;condition  is found.   In this  implementation  this normal  return will  *not*
    ;;happen by default.  To implement such behaviour  we have to wrap the whole code
    ;;in a form like this:
    ;;
    ;;   (with-return-to-signal-on-unhandled-exception
    ;;     ?body0 ?body ...)
    ;;
    (installed-restart-point.signaled-object-set! C)
    (raise-continuable (condition C SIGNAL-CONDITION)))

  (define-syntax with-return-to-signal-on-unhandled-exception
    (syntax-rules ()
      ((_ ?body0 ?body ...)
       (with-exception-handler
	   (lambda (C)
	     (if (signal-condition? C)
		 ;;SIGNAL will return zero values.
		 (values)
	       (raise-continuable C)))
	 (lambda () ?body0 ?body ...)))
      ))

  (define-syntax* (restart-case stx)
    ;;Install restart handlers in the  current dynamic environment, then evaluate the
    ;;?BODY form.
    ;;
    ;;Every ?KEY must be  a symbol representing the name of a  restart; the same ?KEY
    ;;can be used in nested uses of RESTART-CASE.
    ;;
    ;;Every ?HANDLER  must be  an expression  evaluating to  a procedure  accepting a
    ;;single argument.   The return values  of ?HANDLER  become the return  values of
    ;;RESTART-CASE.
    ;;
    (define (main stx)
      (syntax-case stx ()
	((_ ?body)
	 #'(call/cc
	       (lambda (restart-proc)
		 (parametrise
		     ((installed-restart-point (make-restart-point restart-proc '())))
		   ?body))))

	((_ ?body (?key0 ?handler0) (?key ?handler) ...)
	 (let ((keys (syntax->datum #'(?key0 ?key ...))))
	   (cond ((%all-symbols? keys)
		  => (lambda (obj)
		       (synner "expected symbol as restart name" obj)))
		 ((%duplicate-symbols? keys)
		  => (lambda (key)
		       (synner "duplicate restart name" key)))
		 (else
		  #'(call/cc
			(lambda (restart-proc)
			  (parametrise
			      ((installed-restart-point (make-restart-point restart-proc
							  `((?key0 . ,?handler0)
							    (?key  . ,?handler)
							    ...))))
			    ?body)))))))
	))

    (define (%all-symbols? keys)
      (and (pair? keys)
	   (if (symbol? (car keys))
	       (%all-symbols? (cdr keys))
	     (car keys))))

    (define (%duplicate-symbols? keys)
      (and (pair? keys)
	   (let ((tail (cdr keys)))
	     (let loop ((head (car keys))
			(rest tail))
	       (if (pair? rest)
		   (let ((A (car rest))
			 (D (cdr rest)))
		     (if (eq? head A)
			 head
		       (loop head D)))
		 (%duplicate-symbols? tail))))))

    (main stx))

  (define* (find-restart {key symbol?})
    ;;Search  the  current  dynamic  environment   for  the  innest  restart  handler
    ;;associated to  KEY.  If  a handler  is found:  return its  procedure; otherwise
    ;;return #f.
    ;;
    (exists (lambda (alist)
	      (cond ((assq key alist)
		     => cdr)
		    (else #f)))
      (installed-restart-point.restart-handlers)))

  (define (invoke-restart key/handler)
    ;;Given  a  symbol  representing  the  name of  a  restart  handler:  search  the
    ;;associated  handler in  the current  dynamic environment  and apply  it to  the
    ;;signaled condition object.
    ;;
    ;;Given a  procedure being the restart  handler itself: apply it  to the signaled
    ;;condition object
    ;;
    (define (%call-restart-handler handler)
      ((installed-restart-point.restart-proc)
       (handler (installed-restart-point.signaled-object))))
    (cond ((symbol? key/handler)
	   (cond ((find-restart key/handler)
		  => %call-restart-handler)
		 (else
		  (raise-undefined-restart-error __who__
		    "attempt to invoke non-existent restart"
		    key/handler))))
	  ((procedure? key/handler)
	   (%call-restart-handler key/handler))
	  (else
	   (procedure-argument-violation __who__
	     "expected restart name or procedure as argument"
	     key/handler))))

;;; special restart callers

  (define (use-value obj)
    ;;If a  "use-value" restart is  installed in  the dynamic environment:  apply its
    ;;handler to OBJ; otherwise return #f (without performing a non-local exit).
    ;;
    (cond ((find-restart 'use-value)
	   => (lambda (handler)
		((installed-restart-point.restart-proc) (handler obj))))
	  (else #f)))

  (define (continue-restart)
    ;;If a  "continue" restart is  installed in  the dynamic environment:  invoke its
    ;;handler; otherwise return #f (without performing a non-local exit).
    ;;
    ;;NOTE Under Common  Lisp: this is simply called CONTINUE;  under Vicare CONTINUE
    ;;is already bound.
    ;;
    (cond ((find-restart 'continue)
	   => (lambda (handler)
		((installed-restart-point.restart-proc) (handler))))
	  (else #f)))

  (define (abort-restart)
    ;;If  an "abort"  restart is  installed in  the dynamic  environment: invoke  its
    ;;handler; otherwise raise a "&restarts-control-error".
    ;;
    ;;Under Common Lisp: this is simply called  ABORT; this is quite a common action,
    ;;so it is left unbound.
    ;;
    (cond ((find-restart 'abort)
	   => (lambda (handler)
		((installed-restart-point.restart-proc) (handler))))
	  (else
	   (signal-restarts-control-error __who__
	     "call to ABORT restart but no handler is defined"))))

  #| end of module |# )


(parametrise ((check-test-name	'handlers-case))

  (check	;no condition
      (with-result
	(handlers-case
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
	(handlers-case
	    ((&error    (lambda (E)
			  (add-result 'error-handler)
			  1))
	     (:no-error (lambda (X)
			  (add-result 'no-error)
			  (* 10 X)))
	     (&warning  (lambda (E)
			  (add-result 'warning-handler)
			  2)))
	  (add-result 'body)
	  1))
    => '(10 (body no-error)))

;;; --------------------------------------------------------------------

  (internal-body	;signaled condition

    (define (doit C)
      (with-result
	(handlers-case
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

  ;;Signaled condition, multiple types in single clause.
  ;;
  (internal-body

    (define (doit C)
      (with-result
	(handlers-case
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

  ;;Signaled condition, nested HANDLERS-CASE uses.
  ;;
  (internal-body

    (define (doit C)
      (with-result
	(handlers-case
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 1)))
	  (handlers-case
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

  #t)


(parametrise ((check-test-name	'handlers-bind))

  (check	;no condition
      (with-result
	(handlers-case
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
	      (handlers-bind
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
	      (handlers-bind
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
	      (handlers-bind
		  (((&warning &error) (lambda (E)
					(add-result 'handler)
					(escape 2))))
		(add-result 'body-begin)
		(signal (make-error))
		(add-result 'body-return)
		1))))
    => '(2 (body-begin handler)))

  ;;Nested HANDLERS-BIND uses, returning from handler.
  ;;
  (check
      (with-result
	(call/cc
	    (lambda (escape)
	      (handlers-bind
		  ((&error (lambda (E)
			     (add-result 'outer-error-handler)
			     (escape 2))))
		(handlers-bind
		    ((&error (lambda (E)
			       ;;By returning  this handler  declines to  handle this
			       ;;condition.
			       (add-result 'inner-error-handler))))
		  (add-result 'body-begin)
		  (raise (make-error))
		  (add-result 'body-return)
		  1)))))
    => '(2 (body-begin inner-error-handler outer-error-handler)))

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
	(alpha (lambda (E)
		 (add-result 'restart-alpha))))
    => #f)

  ;;Search an UNdefined restart.
  ;;
  (check
      (restart-case
	  (find-restart 'gamma)
	(alpha (lambda (E)
		 (add-result 'restart-alpha)))
	(beta  (lambda (E)
		 (add-result 'restart-beta)))
	(delta (lambda (E)
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
	  (alpha (lambda (E)
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
	  (alpha (lambda (E)
		   (add-result 'restart-alpha)
		   2))))
    => '(2 (body-in restart-alpha)))

  #f)


(parametrise ((check-test-name	'restarts-and-handlers-bind))

  ;;Nested  RESTART-CASE and  HANDLERS-BIND.   Signal a  condition,  call a  handler,
  ;;invoke a restart.
  ;;
  (internal-body

    (define (restarts-outside/handlers-inside C)
      (with-result
	(restart-case
	    (handlers-bind
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
	  (alpha (lambda (E)
		   (add-result 'restart-alpha)
		   1))
	  (beta  (lambda (E)
		   (add-result 'restart-beta)
		   2)))))

    (check
	(restarts-outside/handlers-inside (make-error))
      => '(1 (body-begin error-handler-begin restart-alpha)))

    (check
	(restarts-outside/handlers-inside (make-warning))
      => '(2 (body-begin warning-handler-begin restart-beta)))

    #| end of body |# )

  ;;Nested  RESTART-CASE and  HANDLERS-BIND.   Signal a  condition,  call a  handler,
  ;;invoke a restart.
  ;;
  (internal-body

    (define (restarts-inside/handlers-outside C)
      (with-result
	(handlers-bind
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
	    (alpha (lambda (E)
		     (add-result 'restart-alpha)
		     1))
	    (beta  (lambda (E)
		     (add-result 'restart-beta)
		     2))))))

    (check
	(restarts-inside/handlers-outside (make-error))
      => '(1 (body-begin error-handler-begin restart-alpha)))

    (check
	(restarts-inside/handlers-outside (make-warning))
      => '(2 (body-begin warning-handler-begin restart-beta)))

    #| end of body |# )

  ;;Nested  RESTART-CASE and  HANDLERS-BIND.   Signal a  condition,  call a  handler,
  ;;invoke a restart.
  ;;
  (internal-body

    (define (restarts-inside/nested-handlers C)
      (with-result
	(handlers-bind
	    ((&error   (lambda (E)
			 (add-result 'error-handler-begin)
			 (invoke-restart 'alpha)
			 (add-result 'error-handler-return))))
	  (handlers-bind
	      ((&warning (lambda (E)
			   (add-result 'warning-handler-begin)
			   (invoke-restart 'beta)
			   (add-result 'warning-handler-return))))
	    (restart-case
		(begin
		  (add-result 'body-begin)
		  (signal C)
		  (add-result 'body-return))
	      (alpha (lambda (E)
		       (add-result 'restart-alpha)
		       1))
	      (beta  (lambda (E)
		       (add-result 'restart-beta)
		       2)))))))

    (check
	(restarts-inside/nested-handlers (make-error))
      => '(1 (body-begin error-handler-begin restart-alpha)))

    (check
	(restarts-inside/nested-handlers (make-warning))
      => '(2 (body-begin warning-handler-begin restart-beta)))

    #| end of LET |# )

  ;;Nested  RESTART-CASE and  HANDLERS-BIND.   Signal a  condition,  call a  handler,
  ;;invoke a restart.
  ;;
  (internal-body

    (define (nested-restarts/handlers-outside C)
      (with-result
	(handlers-bind
	    ((&error   (lambda (E)
			 (add-result 'error-handler)
			 (invoke-restart 'alpha)))
	     (&warning (lambda (E)
			 (add-result 'warning-handler)
			 (let ((handler (find-restart 'beta)))
			   (invoke-restart handler)))))

	  (restart-case
	      (restart-case
		  (begin
		    (add-result 'body-begin)
		    (signal C)
		    (add-result 'body-return))
		(alpha (lambda (E)
			 (add-result 'restart-alpha)
			 1)))
	    (beta  (lambda (E)
		     (add-result 'restart-beta)
		     2))))))

    (check
	(nested-restarts/handlers-outside (make-error))
      => '(1 (body-begin error-handler restart-alpha)))

    (check
	(nested-restarts/handlers-outside (make-warning))
      => '(2 (body-begin warning-handler restart-beta)))

    #| end of body |# )

  ;;Nested  RESTART-CASE  and HANDLERS-BIND.   Signal  a  condition, call  the  first
  ;;handler, the first handler declines, call the second handler, invoke a restart.
  ;;
  (check
      (with-result
	(handlers-bind
	    ((&message (lambda (E)
			 (add-result 'outer-message-handler-begin)
			 (invoke-restart 'alpha)
			 (add-result 'outer-message-handler-return))))
	  (handlers-bind
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

  ;;Nested  RESTART-CASE  and HANDLERS-BIND.   Signal  a  condition, call  the  first
  ;;handler, the first handler declines, call  the second handler, the second handler
  ;;declines, SIGNAL returns.
  ;;
  (check
      (with-result
	(with-return-to-signal-on-unhandled-exception
	  (handlers-bind
	      ((&error (lambda (E)
			 ;;By returning, this handler declines to handle the condition.
			 (add-result 'outer-error-handler))))
	    (handlers-bind
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


(parametrise ((check-test-name	'special-restarts))

;;; USE-VALUE

  ;;Signal condition, call a handler, call a USE-VALUE restart.
  ;;
  (check
      (with-result
	(handlers-bind
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

;;; --------------------------------------------------------------------
;;; CONTINUE

  ;;Signal condition, call a handler, call a CONTINUE restart.
  ;;
  (check
      (with-result
	(handlers-bind
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
	  (handlers-bind
	      ((&error (lambda (E)
			 (add-result 'outer-error-handler))))
	    (handlers-bind
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
	(handlers-bind
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
	    (handlers-bind
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
;; eval: (put 'handlers-case			'scheme-indent-function 1)
;; eval: (put 'handlers-bind			'scheme-indent-function 1)
;; eval: (put 'restart-case			'scheme-indent-function 1)
;; eval: (put 'raise-undefined-restart-error	'scheme-indent-function 1)
;; eval: (put 'raise-restart-internal-error	'scheme-indent-function 1)
;; eval: (put 'signal-restarts-control-error	'scheme-indent-function 1)
;; eval: (put 'make-restart-point		'scheme-indent-function 1)
;; eval: (put 'make-<restart-point>		'scheme-indent-function 1)
;; eval: (put 'with-return-to-signal-on-unhandled-exception 'scheme-indent-function 0)
;; End:
