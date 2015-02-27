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
;;;       WITH-SIMPLE-RESTART, IGNORE-ERRORS, MUFFLE-WARNING.
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: dynamic environment, Common Lisp's restarts\n")


;;;; implementation of HANDLER-CASE

(define-syntax* (handler-case stx)
  ;;Evaluate body forms in a dynamic  environment in which new exception handlers are
  ;;installed;   it  is   capable   of  handling   exceptions   raised  with   RAISE,
  ;;RAISE-CONTINUABLE and SIGNAL.  Basically behave like R6RS's GUARD syntax.
  ;;
  ;;The syntax is:
  ;;
  ;;   (handler-case (?clause ...) ?body0 ?body ...)
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
  ;;must be an expression evaluating to a procedure.  The optional ":no-error" clause
  ;;can be present only if it is the last one.
  ;;
  ;;If the body performs a normal return:
  ;;
  ;;* If the  ":no-error" clause is missing:  the values returned by  the body become
  ;;  the values returned by HANDLER-CASE.
  ;;
  ;;*  If the  ":no-error"  clause  is present:  the  procedure ?NO-ERROR-HANDLER  is
  ;;  applied  to the returned values;  the return values of  such application become
  ;;  the return values of HANDLER-CASE.
  ;;
  ;;If an  exception is raised  (in Common Lisp jargon:  a condition is  signaled): a
  ;;condition  handler matching  the raised  object is  searched in  the sequence  of
  ;;clauses, left-to-right:
  ;;
  ;;* If a clause matches: the dynamic extent of the body is terminated, the ?HANDLER
  ;;  is  applied to  the raised  object and  the return  values of  such application
  ;;  become the return values of HANDLER-CASE.
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

  (define (%no-error-spec? X)
    (and (identifier? X)
	 (eq? ':no-error (syntax->datum X))))

  (define (%process-clauses var clause*)
    (syntax-case clause* ()
      (()
       '())

      ;;The no-error clause can be present only if it is the last one.
      (((?no-error ?handler))
       (%no-error-spec? #'?no-error)
       (if no-error-expr
	   (synner "invalid multiple definition of :no-error clause"
		   #'(?no-error ?handler))
	 (begin
	   (set! no-error-expr #'?handler)
	   '())))

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


;;;; implementation of HANDLER-BIND

(define-syntax* (handler-bind stx)
  ;;Evaluate body forms in a dynamic  environment in which new exception handlers are
  ;;installed;   it  is   capable   of  handling   exceptions   raised  with   RAISE,
  ;;RAISE-CONTINUABLE  and  SIGNAL.   Not quite  like  R6RS's  WITH-EXCEPTION-HANDLER
  ;;syntax.
  ;;
  ;;The syntax is:
  ;;
  ;;   (handler-bind (?clause ...) ?body0 ?body ...)
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
  ;;values returned by HANDLER-BIND.
  ;;
  ;;If an  exception is raised  (in Common Lisp jargon:  a condition is  signaled): a
  ;;condition  handler matching  the raised  object is  searched in  the sequence  of
  ;;clauses, left-to-right:
  ;;
  ;;* If  a clause  matches: its  ?HANDLER is applied  to the  raised object  and the
  ;;  return values of such application become the return values of HANDLER-BIND.
  ;;
  ;;* If no clause matches: the raised object is re-raised with RAISE-CONTINUABLE.
  ;;
  ;;The handlers are called with a  continuation whose dynamic environment is that of
  ;;the call to RAISE, RAISE-CONTINUABLE or  SIGNAL that raised the exception; except
  ;;that the current exception handler is the one that was in place when HANDLER-BIND
  ;;was evaluated.
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


;;;; restarts: public interface

(module (signal
	 with-return-to-signal-on-unhandled-exception
	 restart-case
	 with-condition-restarts
	 find-restart
	 invoke-restart
	 restart-name
	 compute-restarts
	 ;;
	 use-value
	 store-value
	 continue-restart
	 abort-restart)
  (import (only (psyntax system $all)
		run-unwind-protection-cleanup-upon-exit?))

  ;;The restart object required by Common Lisp.
  ;;
  (define-record-type <restart>
    (nongenerative vicare:restarts:<restart>)
    (fields (immutable name restart-name)
	    (immutable proc)))

  ;;List  of alists  representing the  currently installed  restart handlers.   Every
  ;;alist represents the restarts installed by a single use of RESTART-CASE.
  ;;
  (define restart-handlers
    (make-parameter '()))

  (define (%make-restarts-alist-entry restart-point-proc restart-name restart-proc)
    (cons restart-name
	  (make-<restart> restart-name
			  (lambda args
			    (call-with-values
				(lambda ()
				  (apply restart-proc args))
			      (lambda retvals
				(run-unwind-protection-cleanup-upon-exit? #t)
				(apply restart-point-proc retvals)))))))

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
    (raise-continuable (condition C SIGNAL-CONDITION)))

  (define-syntax with-return-to-signal-on-unhandled-exception
    (syntax-rules ()
      ((_ ?body0 ?body ...)
       (with-exception-handler
	   (lambda (C)
	     (if (signal-condition? C)
		 ;;SIGNAL will return zero values.
		 ;;
		 ;;NOTE IIUC Common  Lisp wants this to return "nil",  which (in this
		 ;;context) would be #f in Scheme (most likely).
		 (values)
	       (raise-continuable C)))
	 (lambda () ?body0 ?body ...)))
      ))

  (define-syntax* (restart-case stx)
    ;;Install restart handlers in the  current dynamic environment, then evaluate the
    ;;body form.
    ;;
    ;;The syntax is:
    ;;
    ;;   (restart-case ?body (?clause ...))
    ;;
    ;;   ?clause = (?restart-name ?restart-handler)
    ;;
    ;;Every ?RESTART-NAME  must be a symbol  representing the name of  a restart; the
    ;;same ?RESTART-NAME can be used in nested uses of RESTART-CASE.
    ;;
    ;;Every  ?RESTART-HANDLER  must  be  an  expression  evaluating  to  a  procedure
    ;;accepting  a  non-constrained  number  of  arguments.   The  return  values  of
    ;;?RESTART-HANDLER become the return values of RESTART-CASE.
    ;;
    ;;As special case, if ?BODY is:
    ;;
    ;;   (signal ?expr)
    ;;
    ;;the installed restarts  are associated to the condition object  returned by the
    ;;evaluation of ?EXPR.
    ;;
    (define (main stx)
      (syntax-case stx (signal)
	((_ ?body)
	 #' ?body)

	((_ (signal ?obj) (?restart-name ?restart-handler) ...)
	 #'(let ((C ?obj))
	     (restart-case (with-condition-restarts C
			     (list (find-restart (quote ?restart-name))
				   ...)
			     (signal C))
	       (?restart-name ?restart-handler) ...)))

	((_ ?body (?restart-name ?restart-handler) ...)
	 (let ((names (syntax->datum #'(?restart-name ...))))
	   (cond ((%all-symbols? names)
		  => (lambda (obj)
		       (synner "expected symbol as restart name" obj)))
		 ((%duplicate-symbols? names)
		  => (lambda (name)
		       (synner "duplicate restart name" name)))
		 (else
		  #'(begin0
			(call/cc
			    (lambda (restart-point-proc)
			      (parametrise
				  ((restart-handlers
				    (cons `(,(%make-restarts-alist-entry restart-point-proc
									 (quote ?restart-name)
									 ?restart-handler)
					    ...)
					  (restart-handlers))))
				?body)))
		      (run-unwind-protection-cleanup-upon-exit? #f))))))
	))

    (define (%all-symbols? names)
      (and (pair? names)
	   (if (symbol? (car names))
	       (%all-symbols? (cdr names))
	     (car names))))

    (define (%duplicate-symbols? names)
      (and (pair? names)
	   (let ((tail (cdr names)))
	     (let loop ((head (car names))
			(rest tail))
	       (if (pair? rest)
		   (let ((A (car rest))
			 (D (cdr rest)))
		     (if (eq? head A)
			 head
		       (loop head D)))
		 (%duplicate-symbols? tail))))))

    (main stx))

  ;;List  of  alists  representing  the  associations  between  restart  objects  and
  ;;condition objects.
  ;;
  (define restarts.conditions
    (make-parameter '()))

  (define-syntax (with-condition-restarts stx)
    (syntax-case stx ()
      ((_ ?condition-form ?restarts-form ?body0 ?body ...)
       #'(let ((C ?condition-form)
	       (R ?restarts-form))
	   (parametrise ((restarts.conditions (cons (map (lambda (restart)
							   (cons restart C))
						      R)
						    (restarts.conditions))))
	     ?body0 ?body ...)))
      ))

  (define* (associated-condition-match? {C condition?} {R <restart>?})
    ;;Return true if  the condition object C  is associated to the  restart object R;
    ;;otherwise return false.
    ;;
    (exists (lambda (alist)
	      (exists (lambda (restart.condition)
			(and (eq? R (car restart.condition))
			     (let ((K (simple-conditions (cdr restart.condition))))
			       (exists (lambda (C^)
					 (exists (lambda (K^)
						   (eq? C^ K^))
					   K))
				 (simple-conditions C)))))
		alist))
      (restarts.conditions)))

;;;

  (case-define* find-restart
    ;;Search  the  current  dynamic  environment   for  the  innest  restart  handler
    ;;associated  to  NAME.  If  a  handler  is  found:  return its  restart  object;
    ;;otherwise return #f.
    ;;
    (({name symbol?})
     (exists (lambda (alist)
	       (cond ((assq name alist)
		      => cdr)
		     (else #f)))
       (restart-handlers)))
    (({name symbol?} cnd)
     (cond ((not cnd)
	    (find-restart name))
	   ((condition? cnd)
	    (exists (lambda (alist)
		      (exists (lambda (name.restart)
				(and (eq? name (car name.restart))
				     (not (associated-condition-match? cnd (cdr name.restart)))
				     (cdr name.restart)))
			alist))
	      (restart-handlers)))
	   (else
	    (procedure-argument-violation __who__
	      "expected false or condition object as second argument" cnd)))))

  (define (invoke-restart restart-designator . rest)
    ;;Given  a restart  designator:  search  the associated  handler  in the  current
    ;;dynamic  environment and  apply it  to the  given REST  arguments.  Return  the
    ;;return values of such application, if the called function returns.
    ;;
    ;;RESTART-DESIGNATOR can be either a symbol  representing the name of the restart
    ;;or the restart procedure itself.
    ;;
    ;;Given a  procedure being the restart  handler itself: apply it  to the signaled
    ;;condition object
    ;;
    (cond ((symbol? restart-designator)
	   (cond ((find-restart restart-designator)
		  => (lambda (restart)
		       (apply (<restart>-proc restart) rest)))
		 (else
		  (raise-undefined-restart-error __who__
		    "attempt to invoke non-existent restart"
		    restart-designator))))
	  ((is-a? restart-designator <restart>)
	   (apply (<restart>-proc restart-designator) rest))
	  (else
	   (procedure-argument-violation __who__
	     "expected restart name or restart procedure as argument"
	     restart-designator))))

  (define (compute-restarts)
    (fold-right (lambda (alist knil)
		  (append (map cdr alist) knil))
      '()
      (restart-handlers)))

;;; standardised restart functions

  (define-syntax define-restart-function-0
    ;;Define a restart invocation function whose restart handler accepts 0 arguments.
    ;;
    (syntax-rules ()
      ((_ ?who ?restart-name ?not-found-form)
       (case-define ?who
	 (()
	  (?who #f))
	 ((cnd)
	  (cond ((find-restart '?restart-name cnd)
		 => (lambda (restart)
		      ((<restart>-proc restart))))
		(else ?not-found-form)))))
      ))

  (define-syntax define-restart-function-1
    ;;Define a restart invocation function whose restart handler accepts 1 argument.
    ;;
    (syntax-rules ()
      ((_ ?who ?restart-name ?not-found-form)
       (case-define ?who
	 ((obj)
	  (?who obj #f))
	 ((obj cnd)
	  (cond ((find-restart '?restart-name cnd)
		 => (lambda (restart)
		      ((<restart>-proc restart) obj)))
		(else ?not-found-form)))))
      ))

  ;;If a  "use-value" restart is  installed in  the dynamic environment:  apply its
  ;;handler to OBJ; otherwise return #f (without performing a non-local exit).
  ;;
  ;;If  the optional  argument  CND is  a condition  object:  select the  innermost
  ;;matching restart that  is *not* associated with such condition  object.  If CND
  ;;is missing or #f: just select the innermost installed restart.
  ;;
  (define-restart-function-1 use-value use-value #f)

  ;;If a "store-value"  restart is installed in the dynamic  environment: apply its
  ;;handler to OBJ; otherwise return #f (without performing a non-local exit).
  ;;
  ;;If  the optional  argument  CND is  a condition  object:  select the  innermost
  ;;matching restart that  is *not* associated with such condition  object.  If CND
  ;;is missing or #f: just select the innermost installed restart.
  ;;
  (define-restart-function-1 store-value store-value #f)

  ;;If a  "continue" restart is  installed in  the dynamic environment:  invoke its
  ;;handler; otherwise return #f (without performing a non-local exit).
  ;;
  ;;If  the optional  argument  CND is  a condition  object:  select the  innermost
  ;;matching restart that  is *not* associated with such condition  object.  If CND
  ;;is missing or #f: just select the innermost installed restart.
  ;;
  ;;NOTE Under Common  Lisp: this is simply called CONTINUE;  under Vicare CONTINUE
  ;;is already bound.
  ;;
  (define-restart-function-0 continue-restart continue #f)

  ;;If  an "abort"  restart is  installed in  the dynamic  environment: invoke  its
  ;;handler; otherwise raise a "&restarts-control-error".
  ;;
  ;;If  the optional  argument  CND is  a condition  object:  select the  innermost
  ;;matching restart that  is *not* associated with such condition  object.  If CND
  ;;is missing or #f: just select the innermost installed restart.
  ;;
  ;;NOTE Under  Common Lisp: this  is simply called ABORT;  this is quite  a common
  ;;action, so it is left unbound.
  ;;
  (define-restart-function-0 abort-restart abort
    (signal-restarts-control-error __who__
      "call to ABORT restart but no handler is defined"))

  #| end of module |# )


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

  (internal-body	;signaled condition

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

  ;;Signaled condition, multiple types in single clause.
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

  ;;Signaled condition, nested HANDLER-CASE uses.
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

  ;;FIND-RESTART without condition argument.
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
		       ;;which is the signaled object.
		       (add-result 'error-handler)
		       (use-value 1 E))))
	  (restart-case
	      (restart-case
		  (signal (make-error))
		;;This one *is* associated to the signaled object.
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
;; eval: (put 'handler-bind			'scheme-indent-function 1)
;; eval: (put 'restart-case			'scheme-indent-function 1)
;; eval: (put 'with-condition-restarts		'scheme-indent-function 1)
;; eval: (put 'raise-undefined-restart-error	'scheme-indent-function 1)
;; eval: (put 'raise-restart-internal-error	'scheme-indent-function 1)
;; eval: (put 'signal-restarts-control-error	'scheme-indent-function 1)
;; eval: (put '<restart>-associated-condition-set! 'scheme-indent-function 1)
;; eval: (put 'with-return-to-signal-on-unhandled-exception 'scheme-indent-function 0)
;; End:
