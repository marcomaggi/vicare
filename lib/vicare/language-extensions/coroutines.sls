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


(module (empty-queue? enqueue! dequeue! reset-coroutines! dump-coroutines)

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


(define (coroutine thunk)
  ;;Create a new coroutine having THUNK as function and enter it.  Return unspecified
  ;;values.
  ;;
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
   (unless (empty-queue?)
     (yield)
     (finish-coroutines)))
  ((exit-loop?)
   ;;Loop running the next coroutine until there  are no more or the thunk EXIT-LOOP?
   ;;returns true.  Return unspecified values.
   ;;
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
    (fields (mutable count)
	    (immutable key)
	    (immutable max-coroutines))
    (protocol
     (lambda (make-record)
       ;;Under Vicare: the protocol function is evaluated only once.
       (define sem-table
	 (make-eq-hashtable))
       (lambda (key max-coroutines)
	 (or (hashtable-ref sem-table key #f)
	     (receive-and-return (sem)
		 (make-record 0 key max-coroutines)
	       (hashtable-set! sem-table key sem)))))))

  (define (sem-acquire sem)
    (define max (sem-max-coroutines sem))
    (let loop ()
      (cond ((<= max (sem-count sem))
	     (yield)
	     (loop))
	    (else
	     (sem-count-set! sem (+ +1 (sem-count sem)))))))

  (define (sem-release sem)
    (sem-count-set! sem (+ -1 (sem-count sem))))

  (define (do-monitor key max-coroutines body-thunk)
    (unless (and (integer? max-coroutines)
		 (positive? max-coroutines))
      (procedure-argument-violation __who__
	"expected positive integer as maximum number of concurrent coroutines"
	max-coroutines))
    (let ((sem (make-sem key max-coroutines)))
      ;;We do *not* want to use DYNAMIC-WIND here!!!
      (sem-acquire sem)
      (unwind-protect
	  (body-thunk)
	(sem-release sem))))

  (define-syntax monitor
    ;;Allow only ?MAX-COROUTINES to concurrently enter the monitor.
    ;;
    (lambda (stx)
      (syntax-case stx ()
	((_ ?max-coroutines ?thunk)
	 (with-syntax (((KEY) (generate-temporaries '(sem-key))))
	   #`(do-monitor (quote KEY) ?max-coroutines ?thunk)))
	)))

  #| end of module: MONITOR |# )


;;;; done

#| end of library |# )

;;; end of file
