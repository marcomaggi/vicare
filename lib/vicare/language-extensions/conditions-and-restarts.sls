;;;
;;;Part of: Vicare Scheme
;;;Contents: Scheme-flavoured Common Lisp's conditions handling and restarts
;;;Date: Fri Feb 27, 2015
;;;
;;;Abstract
;;;
;;;	Scheme-flavoured Common Lisp's condition handlers and restart handlers.
;;;
;;;     Not everything defined by Common Lisp is implemented here:
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


(library (vicare language-extensions conditions-and-restarts)
  (export
    with-return-to-signal-on-unhandled-exception
    signal
    handler-case
    handler-bind
    ignore-errors

    ;; restarts
    restart-case
    with-condition-restarts
    find-restart
    invoke-restart
    restart-name
    compute-restarts

    ;; standardised restart functions
    use-value
    store-value
    continue-restart
    abort-restart

    ;; condition types and error raisers
    &restarts-error make-restarts-error-condition restarts-error-condition?
    &restarts-control-error make-restarts-control-error restarts-control-error?
    signal-restarts-control-error
    &undefined-restart-error make-undefined-restart-error undefined-restart-error?
    raise-undefined-restart-error)
  (import (vicare)
    ;;This is needed for integration with the unwind-protection mechanism.
    (only (psyntax system $all)
	  run-unwind-protection-cleanup-upon-exit?))


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
  ;;Every  ?TYPESPEC is  meant to  be an  identifier or  a non-empty  proper list  of
  ;;identifiers, each usable as second argument to CONDITION-IS-A?.
  ;;
  ;;Every  ?CONDITION-HANDLER  must  be  an  expression  evaluating  to  a  procedure
  ;;accepting a  condition object  as single  argument; the  condition object  can be
  ;;simple or compound.
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
  ;;If an exception  is raised (in Common  Lisp jargon: a condition  is signalled): a
  ;;condition  handler matching  the raised  object is  searched in  the sequence  of
  ;;clauses, left-to-right:
  ;;
  ;;*  If a  clause  matches: the  dynamic  extent  of the  body  is terminated,  the
  ;;  ?CONDITION-HANDLER  is applied to  the raised object  and the return  values of
  ;;  such application become the return values of HANDLER-CASE.
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
      (((?no-error ?condition-handler))
       (%no-error-spec? #'?no-error)
       (if no-error-expr
	   (synner "invalid multiple definition of :no-error clause"
		   #'(?no-error ?condition-handler))
	 (begin
	   (set! no-error-expr #'?condition-handler)
	   '())))

      (((?typespec ?condition-handler) . ?rest)
       (cons #`(#,(%process-typespec var #'?typespec) (?condition-handler #,var))
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


(define-syntax ignore-errors
  ;;Install a handler for conditions of  type "&error", then evaluate the body forms.
  ;;If the body performs a normal return:  its return values become the return values
  ;;of IGNORE-ERRORS.  If  the body signals a condition of  type "&error": two values
  ;;are returned, false and the signalled condition object.
  ;;
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (handler-case
	 ((&error (lambda (E)
		    (values #f E))))
       ?body0 ?body ...))
    ))


(define-syntax* (handler-bind stx)
  ;;Evaluate body forms in a dynamic  environment in which new exception handlers are
  ;;installed;   it  is   capable   of  handling   exceptions   raised  with   RAISE,
  ;;RAISE-CONTINUABLE  and  SIGNAL.   Not quite  like  R6RS's  WITH-EXCEPTION-HANDLER
  ;;syntax, but similar.
  ;;
  ;;The syntax is:
  ;;
  ;;   (handler-bind (?clause ...) ?body0 ?body ...)
  ;;
  ;;   ?clause = (?typespec ?condition-handler)
  ;;
  ;;Every  ?TYPESPEC is  meant to  be an  identifier or  a non-empty  proper list  of
  ;;identifiers, each usable as second argument to CONDITION-IS-A?.
  ;;
  ;;Every  ?CONDITION-HANDLER  must  be  an  expression  evaluating  to  a  procedure
  ;;accepting a  condition object  as single  argument; the  condition object  can be
  ;;simple or compound.
  ;;
  ;;If the body performs a normal return:  the values returned by the body become the
  ;;values returned by HANDLER-BIND.
  ;;
  ;;If an exception  is raised (in Common  Lisp jargon: a condition  is signalled): a
  ;;condition  handler matching  the raised  object is  searched in  the sequence  of
  ;;clauses, left-to-right:
  ;;
  ;;* If a clause matches: its ?CONDITION-HANDLER is applied to the raised object.
  ;;
  ;;* If no clause matches: the raised object is re-raised with RAISE-CONTINUABLE.
  ;;
  ;;The handlers are called with a  continuation whose dynamic environment is that of
  ;;the call to RAISE, RAISE-CONTINUABLE or  SIGNAL that raised the exception; except
  ;;that the current exception handler is the one that was in place when HANDLER-BIND
  ;;was evaluated.
  ;;
  ;;When a ?CONDITION-HANDLER is applied to the raised condition object:
  ;;
  ;;* If it  accepts to handle the  condition: it must perform a  non-local exit, for
  ;;  example by invoking a restart.
  ;;
  ;;* If  it declines to handle  the condition: it  must perform a normal  return; in
  ;;  this case the returned values are discarded and the originally raised object is
  ;;  re-raised using RAISE-CONTINUABLE.
  ;;
  (define (main stx)
    (syntax-case stx ()
      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))

      ((_ ((?typespec ?condition-handler) ...) ?body0 ?body ...)
       (with-syntax
	   (((VAR) (generate-temporaries '(E))))
	 (with-syntax
	     (((PRED ...) (%process-typespecs #'VAR #'(?typespec ...))))
	   #'(with-exception-handler
		 (lambda (VAR)
		   (when PRED
		     (?condition-handler VAR))
		   ...
		   ;;If   we  are   here  either   no  ?TYPESPEC   matched  E   or  a
		   ;;?CONDITION-HANDLER returned.   Let's search for  another handler
		   ;;in the uplevel dynamic environment.
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
  make-restarts-error-condition
  restarts-error-condition?)

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


;;;; restart object

;;The restart object required by Common Lisp.
;;
(define-record-type <restart>
  (nongenerative vicare:restarts:<restart>)
  (fields (immutable name restart-name)
	  (immutable proc)))


;;;; signaling conditions

(module (signal with-return-to-signal-on-unhandled-exception)

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
    (raise-continuable (condition C (make-signal-condition))))

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

  ;;Condition object to be added to every  condition raised by SIGNAL.  It is used by
  ;;WITH-RETURN-TO-SIGNAL-ON-UNHANDLED-EXCEPTION  to  distinguish  between  signalled
  ;;conditions  and  exceptions  raised  by  a non-SIGNAL  invokation  of  RAISE  and
  ;;RAISE-CONTINUABLE.
  ;;
  (define-condition-type &signal
      &condition
    make-signal-condition
    signal-condition?)

  #| end of module |# )


;;;; restart case

;;List of alists representing the  currently installed restart handlers.  Every alist
;;represents the restarts installed by a single use of RESTART-CASE.
;;
(define-constant restart-handlers
  (make-parameter '()))

(define (%make-restarts-alist-entry restart-point-proc restart-name restart-proc)
  ;;Build  and return  an alist  entry  for the  alists  that are  referenced by  the
  ;;parameter RESTART-HANDLERS.
  ;;
  ;;RESTART-POINT-PROC  must   be  the  escape   procedure  used  to   reinstate  the
  ;;continuation of a RESTART-CASE use.
  ;;
  ;;RESTART-NAME must be a symbol representing the name of the restart object.
  ;;
  ;;RESTART-PROC must be the function implementing the restart handler.
  ;;
  (cons restart-name
	(make-<restart> restart-name
			(lambda args
			  (call-with-values
			      (lambda ()
				(apply restart-proc args))
			    (lambda retvals
			      (run-unwind-protection-cleanup-upon-exit? #t)
			      (apply restart-point-proc retvals)))))))

(define-syntax* (restart-case stx)
  ;;Install restart  handlers in the  current dynamic environment, then  evaluate the
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
  ;;Every ?RESTART-HANDLER must be an  expression evaluating to a procedure accepting
  ;;a non-constrained  number of  arguments.  The  return values  of ?RESTART-HANDLER
  ;;become the return  values of RESTART-CASE.  The ?RESTART-HANDLER  is evaluated in
  ;;the dynamic environment of the call to INVOKE-RESTART that invoked the restart.
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


;;;; associating restarts and condition objects

;;List of alists representing the  associations between restart objects and condition
;;objects.   Every alist  represents  the associations  defined by  a  single use  of
;;WITH-CONDITION-RESTARTS.
;;
(define-constant restarts.conditions
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

(define (%associated-condition-match? C R)
  ;;Return true  if the  condition object C  is associated to  the restart  object R;
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


;;;; finding and invoking restarts

(case-define* find-restart
  ;;Search  the  current  dynamic  environment  for  the  innermost  restart  handler
  ;;associated to NAME.  If a handler  is found: return its restart object; otherwise
  ;;return #f.
  ;;
  ;;When the optional argument  CND is used and CND is a  condition object (simple or
  ;;compound): among the installed restarts, return  only the innermost that is *not*
  ;;associated to the condition object CND.
  ;;
  ;;When the optional argument CND is used and CND is false: behave as if CND was not
  ;;used.
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
				   (not (%associated-condition-match? cnd (cdr name.restart)))
				   (cdr name.restart)))
		      alist))
	    (restart-handlers)))
	 (else
	  (procedure-argument-violation __who__
	    "expected false or condition object as second argument" cnd)))))

(define (invoke-restart restart-designator . rest)
  ;;Given a restart designator: search the  associated handler in the current dynamic
  ;;environment and apply  it to the given REST arguments.   Return the return values
  ;;of such application, if the called function returns.
  ;;
  ;;RESTART-DESIGNATOR can be either a symbol representing the name of the restart or
  ;;the restart object itself.
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
  ;;Return  a  list of  symbols  representing  the  names  of the  restars  currently
  ;;installed  in  the dynamic  environment.   The  list  is ordered:  the  innermost
  ;;restarts come first, the outermost restarts come last.
  ;;
  (fold-right (lambda (alist knil)
		(append (map cdr alist) knil))
    '()
    (restart-handlers)))


;;;; standardised restart functions

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

;;If a "use-value" restart is installed in the dynamic environment: apply its handler
;;to OBJ; otherwise return #f (without performing a non-local exit).
;;
;;If the optional  argument CND is a condition object:  select the innermost matching
;;restart that is *not* associated with such  condition object.  If CND is missing or
;;#f: just select the innermost installed restart.
;;
(define-restart-function-1 use-value use-value #f)

;;If  a "store-value"  restart is  installed in  the dynamic  environment: apply  its
;;handler to OBJ; otherwise return #f (without performing a non-local exit).
;;
;;If the optional  argument CND is a condition object:  select the innermost matching
;;restart that is *not* associated with such  condition object.  If CND is missing or
;;#f: just select the innermost installed restart.
;;
(define-restart-function-1 store-value store-value #f)

;;If  a "continue"  restart  is  installed in  the  dynamic  environment: invoke  its
;;handler; otherwise return #f (without performing a non-local exit).
;;
;;If the optional  argument CND is a condition object:  select the innermost matching
;;restart that is *not* associated with such  condition object.  If CND is missing or
;;#f: just select the innermost installed restart.
;;
;;NOTE Under  Common Lisp: this is  simply called CONTINUE; under  Vicare CONTINUE is
;;already bound.
;;
(define-restart-function-0 continue-restart continue #f)

;;If an "abort" restart is installed  in the dynamic environment: invoke its handler;
;;otherwise raise a "&restarts-control-error".
;;
;;If the optional  argument CND is a condition object:  select the innermost matching
;;restart that is *not* associated with such  condition object.  If CND is missing or
;;#f: just select the innermost installed restart.
;;
;;NOTE Under Common Lisp: this is simply called ABORT; this is quite a common action,
;;so it is left unbound.
;;
(define-restart-function-0 abort-restart abort
  (signal-restarts-control-error __who__
    "call to ABORT restart but no handler is defined"))


;;;; done

#| end of LIBRARY |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
