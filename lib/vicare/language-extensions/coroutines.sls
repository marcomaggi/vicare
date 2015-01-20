;;; -*- coding: utf-8-unix -*-
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
(library (vicare language-extensions coroutines)
  (export
    coroutine yield finish-coroutines
    reset-coroutines! dump-coroutines
    parallel monitor)
  (import (vicare)
    (vicare unsafe operations))


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


(define (coroutine thunk)
  ;;Create a new coroutine having THUNK as function and enter it.  Return unspecified
  ;;values.
  ;;
  (import COROUTINE-CONTINUATIONS-QUEUE)
  (call/cc
      (lambda (reenter)
	(enqueue! reenter)
	(thunk)
	((dequeue!)))))

(define (yield)
  ;;Register  the current  continuation as  coroutine, then  run the  next coroutine.
  ;;Return unspecified values.
  ;;
  (coroutine void))

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


;;;; parallel evaluation

(define-syntax parallel
  (syntax-rules ()
    ((_ ?thunk0 ?thunk ...)
     (let ((counter 0))
       (begin
	 (++ counter)
	 (coroutine (lambda () (?thunk0) (-- counter))))
       (begin
	 (++ counter)
	 (coroutine (lambda () (?thunk)  (-- counter))))
       ...
       (finish-coroutines (lambda ()
			    (zero? counter)))))
    ))


;;;; monitor

(module (monitor)

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

  (define-syntax monitor
    ;;Allow only ?CONCURRENT-COROUTINES-MAXIMUM to concurrently enter the monitor.
    ;;
    (lambda (stx)
      (syntax-case stx ()
	((_ ?concurrent-coroutines-maximum ?thunk)
	 (with-syntax (((KEY) (generate-temporaries '(sem-key))))
	   #`(do-monitor (quote KEY) ?concurrent-coroutines-maximum ?thunk)))
	)))

;;; --------------------------------------------------------------------

  (define (%concurrent-coroutines-maximum? obj)
    (and (fixnum?     obj)
	 (fxpositive? obj)))

  #| end of module: MONITOR |# )


;;;; done

#| end of library |# )

;;; end of file
