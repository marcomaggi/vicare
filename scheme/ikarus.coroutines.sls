;;;
;;;Part of: Vicare Scheme
;;;Contents: coroutines library
;;;Date: Mon Apr  1, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus coroutines)
  (export
    coroutine yield finish-coroutines
    current-coroutine-uid coroutine-uid?
    suspend-coroutine resume-coroutine suspended-coroutine?
    reset-coroutines! dump-coroutines
    ;;This is for internal use.
    do-monitor)
  (import (except (vicare)
		  coroutine yield finish-coroutines
		  current-coroutine-uid coroutine-uid?
		  suspend-coroutine resume-coroutine suspended-coroutine?
		  reset-coroutines! dump-coroutines)
    (only (ikarus.exceptions)
	  run-unwind-protection-cleanup-upon-exit?)
    (vicare system $pairs))


;;;; helpers

(define-syntax (expand-time-gensym stx)
  (syntax-case stx ()
    ((_ ?template)
     (let* ((tmp (syntax->datum #'?template))
	    (fxs (vector->list (foreign-call "ikrt_current_time_fixnums_2")))
	    (str (apply string-append tmp (map (lambda (N)
						 (string-append "." (number->string N)))
					    fxs)))
	    (sym (gensym str)))
       (with-syntax
	   ((SYM (datum->syntax #'here sym)))
	 (fprintf (current-error-port) "expand-time gensym ~a\n" sym)
	 #'(quote SYM))))))


(module COROUTINE-CONTINUATIONS-QUEUE
  (empty-queue? enqueue! dequeue! reset-coroutines! dump-coroutines)

  ;;The  value of  this parameter  is #f  or a  pair representing  a queue  of escape
  ;;functions.
  ;;
  ;;The car of the queue-pair is the first pair of the list of escape functions.  The
  ;;cdr of the queue-pair is the last pair of the list of escape functions.
  ;;
  ;;NOTE At present Vicare  has no native threads, so this  QUEUE variable could well
  ;;be a simple binding.   To show off: we make it a parameter  because that would be
  ;;needed in a multithreading process.
  ;;
  (define queue
    (make-parameter #f))

  (define (empty-queue?)
    (not (queue)))

  (define (enqueue! escape-func)
    (if (queue)
	(let ((old-last-pair ($cdr (queue)))
	      (new-last-pair (list escape-func)))
	  ($set-cdr! old-last-pair new-last-pair)
	  ($set-cdr! (queue) new-last-pair))
      (let ((Q (list escape-func)))
	(queue (cons Q Q)))))

  (define (dequeue!)
    (let ((Q (queue)))
      (if Q
	  (let ((head ($car Q)))
	    (begin0
		($car head)
	      (let ((head ($cdr head)))
		(if (null? head)
		    (reset-coroutines!)
		  ($set-car! Q head)))))
	(error __who__ "no more coroutines"))))

  (define (reset-coroutines!)
    (queue #f))

  (define (dump-coroutines)
    (debug-print 'enqueued-coroutines
		 (let ((Q (queue)))
		   (if Q
		       (car Q)
		     Q))))

  #| end of module |# )

(module (dump-coroutines reset-coroutines!)
  (import COROUTINE-CONTINUATIONS-QUEUE))


;;;; unique identifier

(define %current-coroutine-uid
  (make-parameter (expand-time-gensym "main-coroutine-uid")))

(define (current-coroutine-uid)
  ;;Return a gensym acting as unique identifier for the current coroutine.
  ;;
  ;;We do not want to expose the parameter in the public API: it must be immutable in
  ;;the user code.
  ;;
  (%current-coroutine-uid))

(define (coroutine-uid? obj)
  (and (symbol? obj)
       (symbol-bound? obj)
       (<coroutine-state>? (symbol-value obj))))


;;;; coroutine state

(define-record-type <coroutine-state>
  (nongenerative vicare:coroutine:<coroutine-state>)
  (fields (mutable reinstate-procedure)
		;False or a procedure that reinstates the coroutine continuation.  It
		;is used when suspending a coroutine.
	  #| end of FIELDS |# )
  (protocol
   (lambda (make-record)
     (lambda ()
       (let ((reinstate-procedure #f))
	 (make-record reinstate-procedure)))))
  #| end of DEFINE-RECORD-TYPE |# )

(define* (coroutine-state {uid symbol?})
  ;;Given a coroutine UID: return the associated state procedure.
  ;;
  (if (symbol-bound? uid)
      (let ((state (%coroutine-state uid)))
	(if (<coroutine-state>? state)
	    state
	  (procedure-argument-violation __who__
	    "expected a coroutine UID symbol as argument" uid)))
    (procedure-argument-violation __who__
      "expected bound symbol as coroutine UID" uid)))

(define-syntax-rule (%coroutine-state ?uid)
  ;;Given a  coroutine UID:  return the  associated state  procedure.  ?UID  is *not*
  ;;validated as UID.
  ;;
  (symbol-value ?uid))


;;;; basic operations

(define (%enqueue-coroutine thunk)
  (import COROUTINE-CONTINUATIONS-QUEUE)
  (call/cc
      (lambda (reenter)
	(enqueue! reenter)
	(thunk)
	((dequeue!)))))

(define (coroutine thunk)
  ;;Create a new coroutine having THUNK as function and enter it.  Return unspecified
  ;;values.
  ;;
  (define uid (gensym "coroutine-uid"))
  (set-symbol-value! uid (make-<coroutine-state>))
  (parametrise
      ((run-unwind-protection-cleanup-upon-exit? #f)
       (%current-coroutine-uid                           uid))
    (%enqueue-coroutine thunk)))

(define (yield)
  ;;Register  the current  continuation as  coroutine, then  run the  next coroutine.
  ;;Return unspecified values.
  ;;
  (%enqueue-coroutine void))

(case-define finish-coroutines
  (()
   ;;Loop running  the next coroutine  until there  are no more.   Return unspecified
   ;;values.
   ;;
   (import COROUTINE-CONTINUATIONS-QUEUE)
   (unless (empty-queue?)
     (yield)
     (finish-coroutines)))
  ((exit-loop?)
   ;;Loop running the next coroutine until there  are no more or the thunk EXIT-LOOP?
   ;;returns true.  Return unspecified values.
   ;;
   (import COROUTINE-CONTINUATIONS-QUEUE)
   (unless (or (empty-queue?)
	       (exit-loop?))
     (yield)
     (finish-coroutines exit-loop?))))


;;;; suspending and resuming

(define* (suspended-coroutine? {uid coroutine-uid?})
  ;;Return true if UID is the unique identifier of a suspended coroutine.
  ;;
  (let ((state (%coroutine-state uid)))
    (and (<coroutine-state>-reinstate-procedure state)
	 #t)))

(define (suspend-coroutine)
  ;;Suspend the current  coroutine.  Yield control to the next  coroutine, but do not
  ;;enqueue the current continuation to be reinstated later.
  ;;
  (import COROUTINE-CONTINUATIONS-QUEUE)
  (let ((state (%coroutine-state (current-coroutine-uid))))
    (cond ((<coroutine-state>-reinstate-procedure state)
	   => (lambda (reinstate)
		(assertion-violation __who__
		  "attempt to suspend an already suspended coroutine"
		  (current-coroutine-uid))))
	  (else
	   (call/cc
	       (lambda (escape)
		 (<coroutine-state>-reinstate-procedure-set! state escape)
		 ((dequeue!))))))))

(define* (resume-coroutine {uid coroutine-uid?})
  ;;Resume a previously suspended coroutine.
  ;;
  (let ((state (%coroutine-state uid)))
    (cond ((<coroutine-state>-reinstate-procedure state)
	   => (lambda (reinstate)
		(<coroutine-state>-reinstate-procedure-set! state #f)
		(%enqueue-coroutine reinstate)))
	  (else
	   (assertion-violation __who__
	     "attempt to resume a non-suspended coroutine" uid)))))


;;;; monitor

(module (do-monitor)

  (define-record-type sem
    (fields (mutable concurrent-coroutines-counter)
		;A non-negative  exact integer representing the  number of coroutines
		;currently inside the critical section.
	    (mutable pending-continuations)
		;Null or a proper list,  representing a FIFO queue, holding coroutine
		;continuation procedures.  Every time a coroutine is denied access to
		;the critial section: its continuation procedure is appended here.
	    (immutable key)
		;A gensym uniquely identifying this monitor.
	    (immutable concurrent-coroutines-maximum))
		;A  positive  exact  integer   representing  the  maximum  number  of
		;coroutines  that  are allowed  to  concurrently  enter the  critical
		;section.
    (protocol
     (lambda (make-record)
       (lambda (key concurrent-coroutines-maximum)
	 (if (symbol-bound? key)
	     (symbol-value key)
	   (receive-and-return (sem)
	       (make-record 0 '() key concurrent-coroutines-maximum)
	     (set-symbol-value! key sem)))))))

  (define-syntax-rule (sem-counter-incr! sem)
    (sem-concurrent-coroutines-counter-set! sem (+ +1 (sem-concurrent-coroutines-counter sem))))

  (define-syntax-rule (sem-counter-decr! sem)
    (sem-concurrent-coroutines-counter-set! sem (+ -1 (sem-concurrent-coroutines-counter sem))))

  (define (sem-enqueue-pending-continuation! sem reenter)
    ;;FIXME  Should this  list be  replaced by  a proper  FIFO queue  object?  (Marco
    ;;Maggi; Mon Jan 19, 2015)
    (sem-pending-continuations-set! sem (append (sem-pending-continuations sem)
						(list reenter))))

  (define (sem-dequeue-pending-continuation! sem)
    (let ((Q (sem-pending-continuations sem)))
      (and (pair? Q)
	   (receive-and-return (reenter)
	       (car Q)
	     (sem-pending-continuations-set! sem (cdr Q))))))

  (define (sem-acquire sem)
    (cond ((< (sem-concurrent-coroutines-counter sem)
	      (sem-concurrent-coroutines-maximum sem))
	   ;;The coroutine is allowed entry in the critical section.
	   (sem-counter-incr! sem))
	  (else
	   ;;The  coroutine is  denied  entry in  the critical  section:  we have  to
	   ;;suspend it.  We enqueue a continuation  function in the queue of pending
	   ;;coroutines, then jump to the next coroutine.
	   (call/cc
	       (lambda (reenter)
		 (import COROUTINE-CONTINUATIONS-QUEUE)
		 (sem-enqueue-pending-continuation! sem reenter)
		 ((dequeue!))))
	   ;;When the  continuation procedure  REENTER is called:  it will  jump back
	   ;;here and return from this function.
	   )))

  (define (sem-release sem)
    (sem-counter-decr! sem)
    (cond ((sem-dequeue-pending-continuation! sem)
	   => coroutine)))

  (define* (do-monitor key {concurrent-coroutines-maximum %concurrent-coroutines-maximum?} body-thunk)
    (let ((sem (make-sem key concurrent-coroutines-maximum)))
      ;;We do *not* want to use DYNAMIC-WIND here!!!
      (sem-acquire sem)
      (unwind-protect
	  (body-thunk)
	(sem-release sem))))

;;; --------------------------------------------------------------------

  (define (%concurrent-coroutines-maximum? obj)
    (and (fixnum?     obj)
	 (fxpositive? obj)))

  #| end of module: MONITOR |# )



;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
