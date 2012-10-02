;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: simple event loop
;;;Date: Tue Feb 21, 2012
;;;
;;;Abstract
;;;
;;;	This event  loop implementation is inspired  by the architecture
;;;	of the event loop of Tcl, <http://www.tcl.tk>.
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare simple-event-loop)
  (export

    ;; event loop control
    initialise			finalise
    busy?			do-one-event
    enter			leave-asap
    logging

    ;; interprocess signals
    receive-signal		serve-interprocess-signals

    ;; file descriptor events
    readable
    writable
    exception
    forget-fd
    do-one-fd-event

    ;; fragmented tasks
    task-fragment		do-one-task-event)
  (import (vicare)
    (prefix (vicare posix) px.)
    (prefix (vicare unsafe-operations) unsafe.)
    (vicare syntactic-extensions)
    (vicare platform constants))


;;;; arguments validation

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (port/file-descriptor who obj)
  (or (and (port? obj) (port-fd obj)) (%file-descriptor? obj))
  (assertion-violation who "expected port or fixnum file descriptor as argument" obj))

(define-argument-validation (signum who obj)
  (and (fixnum? obj)
       (unsafe.fx>= obj 0)
       (unsafe.fx<= obj NSIG))
  (assertion-violation who "expected fixnum signal code as argument" obj))


;;;; helpers

(define-inline (%file-descriptor? obj)
  ;;Do  what   is  possible  to  recognise   fixnums  representing  file
  ;;descriptors.
  ;;
  (and (fixnum? obj)
       (unsafe.fx>= obj 0)
       (unsafe.fx<  obj FD_SETSIZE)))

(define-syntax %catch
  (syntax-rules ()
    ((_ . ?body)
     (guard (E (else #f))
       . ?body))))

(define-inline (%fxincr! ?fxvar)
  (set! ?fxvar (unsafe.fxadd1 ?fxvar)))

(define logging
  (make-parameter #f
    (lambda (obj)
      (if obj #t #f))))

(define (%log template . args)
  (when (logging)
    (let ((port (current-error-port)))
      (fprintf port "vicare SEL: ")
      (apply fprintf port template args)
      (fprintf port "\n")
      (flush-output-port port))))


;;;; data structures

(define MAX-CONSECUTIVE-FD-EVENTS 5)

(define-struct event-sources
  (break?
		;Boolean.  True if  a request to leave the  loop as soon
		;as possible was posted.

   signal-handlers
		;Vector   of  null   or  lists.    Each   list  contains
		;interprocess signal  handlers in the form  of thunks to
		;be run once.

   fds-count
		;Non-negative  fixnum.  Count  of consecutive  fd events
		;served.
   fds-watermark
		;Non-negative fixnum.  Maximum  number of consecutive fd
		;events to serve.  When  the count reaches the watermark
		;level: the loop avoids servicing fd events and tries to
		;serve an event from another source.
   fds-rev-head
		;Reverse  list of  fd  entries already  queried for  the
		;current run over fd event sources.
   fds-tail
		;List of  fd entries still  to query in the  current run
		;over fd event sources.

   tasks-rev-head
		;Reverse list  of task  entries already queried  for the
		;current run over task event sources.
   tasks-tail
		;List of task entries still  to query in the current run
		;over task event sources.
   ))

(define SOURCES
  #f)

(define-syntax with-event-sources
  ;;Dot notation for instances of EVENT-SOURCES structures.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ (?src) . ?body)
       (identifier? #'?src)
       (let* ((src-id	#'?src)
	      (src-str	(symbol->string (syntax->datum src-id))))
	 (define (%dot-id field-str)
	   (datum->syntax src-id (string->symbol (string-append src-str field-str))))
	 (with-syntax
	     ((SRC.BREAK?		(%dot-id ".break?"))
	      (SRC.SIGNAL-HANDLERS	(%dot-id ".signal-handlers"))
	      (SRC.FDS.COUNT		(%dot-id ".fds.count"))
	      (SRC.FDS.WATERMARK	(%dot-id ".fds.watermark"))
	      (SRC.FDS.REV-HEAD		(%dot-id ".fds.rev-head"))
	      (SRC.FDS.TAIL		(%dot-id ".fds.tail"))
	      (SRC.TASKS.REV-HEAD	(%dot-id ".tasks.rev-head"))
	      (SRC.TASKS.TAIL		(%dot-id ".tasks.tail")))
	   #'(let-syntax
		 ((SRC.BREAK?
		   (identifier-syntax
		    (_
		     (event-sources-break? ?src))
		    ((set! _ ?val)
		     (set-event-sources-break?! ?src ?val))))
		  (SRC.SIGNAL-HANDLERS
		   (identifier-syntax
		    (_
		     (event-sources-signal-handlers ?src))
		    ((set! _ ?val)
		     (set-event-sources-signal-handlers! ?src ?val))))
		  (SRC.FDS.COUNT
		   (identifier-syntax
		    (_
		     (event-sources-fds-count ?src))
		    ((set! _ ?val)
		     (set-event-sources-fds-count! ?src ?val))))
		  (SRC.FDS.WATERMARK
		   (identifier-syntax
		    (_
		     (event-sources-fds-watermark ?src))
		    ((set! _ ?val)
		     (set-event-sources-fds-watermark! ?src ?val))))
		  (SRC.FDS.REV-HEAD
		   (identifier-syntax
		    (_
		     (event-sources-fds-rev-head ?src))
		    ((set! _ ?val)
		     (set-event-sources-fds-rev-head! ?src ?val))))
		  (SRC.FDS.TAIL
		   (identifier-syntax
		    (_
		     (event-sources-fds-tail ?src))
		    ((set! _ ?val)
		     (set-event-sources-fds-tail! ?src ?val))))
		  (SRC.TASKS.REV-HEAD
		   (identifier-syntax
		    (_
		     (event-sources-tasks-rev-head ?src))
		    ((set! _ ?val)
		     (set-event-sources-tasks-rev-head! ?src ?val))))
		  (SRC.TASKS.TAIL
		   (identifier-syntax
		    (_
		     (event-sources-tasks-tail ?src))
		    ((set! _ ?val)
		     (set-event-sources-tasks-tail! ?src ?val)))))
	       . ?body)))))))


;;;; event loop control

(define (initialise)
  (%log "initialising")
  (set! SOURCES
	(make-event-sources
	 #f			   ;break?
	 (make-vector NSIG '())	   ;signal-handlers
	 0			   ;fds.count
	 MAX-CONSECUTIVE-FD-EVENTS ;fds.watermark
	 '()			   ;fds.rev-head
	 '()			   ;fds.tail
	 '()			   ;tasks.rev-head
	 '()			   ;tasks.tail
	 ))
  (px.signal-bub-init))

(define (finalise)
  (%log "finalising")
  (px.signal-bub-final)
  (set! SOURCES #f))

(define (do-one-event)
  (serve-interprocess-signals)
  (or (do-one-fd-event)
      (do-one-task-event)))

(define (busy?)
  ;;Return true if there is at least one registered event source.
  ;;
  (with-event-sources (SOURCES)
    (or (not (null? SOURCES.fds.rev-head))
	(not (null? SOURCES.fds.tail))
	(not (null? SOURCES.tasks.rev-head))
	(not (null? SOURCES.tasks.tail)))))

(define (enter)
  ;;Enter the event loop and consume all the events.
  ;;
  (%log "enter loop")
  (let loop ()
    (with-event-sources (SOURCES)
      (if SOURCES.break?
	  (set! SOURCES.break? #f)
	(begin
	  (do-one-event)
	  (loop))))))

(define (leave-asap)
  ;;Leave the event loop as soon as possible.
  ;;
  (%log "scheduled loop exit ASAP")
  (with-event-sources (SOURCES)
    (set! SOURCES.break? #t)))


;;;; interprocess signal handlers

(define (serve-interprocess-signals)
  (px.signal-bub-acquire)
  (for-each (lambda (signum)
	      (%log "start evaluation of signal handler for ~a" signum)
	      (with-event-sources (SOURCES)
		(let ((handlers (unsafe.vector-ref SOURCES.signal-handlers signum)))
		  (unsafe.vector-set! SOURCES.signal-handlers signum '())
		  (for-each (lambda (thunk)
			      (%catch (thunk)))
		    handlers)))
	      (%log "finished evaluation of signal handler for ~a" signum))
    (px.signal-bub-all-delivered)))

(define (receive-signal signum handler-thunk)
  (define who 'receive-signal)
  (with-arguments-validation (who)
      ((signum		signum)
       (procedure	handler-thunk))
    (with-event-sources (SOURCES)
      (unsafe.vector-set! SOURCES.signal-handlers signum
			  (cons handler-thunk
				(unsafe.vector-ref SOURCES.signal-handlers signum))))))


;;;; file descriptor events
;;
;;Basic handling of fd events:
;;
;;1. If FDS-TAIL is null replace it with the reverse of FDS-REV-HEAD.
;;2. Extract the next entry from FDS-TAIL.
;;3. Query the fd for the event.
;;4a. If event present: run the handler, discard the entry, return #t.
;;4b. If no event: push the entry on FDS-REV-HEAD.
;;5a. More entries in tail: loop to (1).
;;5b. No more entries in tail: return #f.
;;
;;Event handling for fds takes precedence over other event sources; with
;;the purpose of not  starving other sources:
;;
;;*  Every  FDS.WATERMARK  events served  DO-ONE-FD-EVENT  artificially
;;returns #f as if no event was served, this should let other sources be
;;queried.
;;
;;* The list of registered fd event sources is traversed in "runs", when
;;a run reaches  the end of the list  DO-ONE-FD-EVENT returns #f rather
;;than immediately restart a fresh run.
;;

(define-struct fd-entry
  (fd query handler))

(define (do-one-fd-event)
  ;;Consume one event, if any, and  return.  Return a boolean, #t if one
  ;;event was served.
  ;;
  ;;Exceptions raised while querying an event source or serving an event
  ;;handler are catched and ignored.
  ;;
  (with-event-sources (SOURCES)
    (when (and (null? SOURCES.fds.tail)
	       (not (null? SOURCES.fds.rev-head)))
      (set! SOURCES.fds.tail     (reverse SOURCES.fds.rev-head))
      (set! SOURCES.fds.rev-head '()))
    (if (null? SOURCES.fds.tail)
	#f
      (if (unsafe.fx< SOURCES.fds.count SOURCES.fds.watermark)
	  (let ((E (unsafe.car SOURCES.fds.tail)))
	    (set! SOURCES.fds.tail (unsafe.cdr SOURCES.fds.tail))
	    (if (%catch ((fd-entry-query E)))
		(guard (E (else
			   ;;;(pretty-print E (current-error-port))
			   #f))
		  ((fd-entry-handler E))
		  (%fxincr! SOURCES.fds.count)
		  #t)
	      (begin
		(set! SOURCES.fds.rev-head (cons E SOURCES.fds.rev-head))
		(if (null? SOURCES.fds.tail)
		    (begin
		      (set! SOURCES.fds.count 0)
		      #f)
		  (do-one-fd-event)))))
	(begin
	  (set! SOURCES.fds.count 0)
	  #f)))))

(define (%enqueue-fd-event-source fd query-thunk handler-thunk)
  ;;Enqueue a new entry for a file descriptor event.
  ;;
  (with-event-sources (SOURCES)
    (set! SOURCES.fds.rev-head (cons (make-fd-entry fd query-thunk handler-thunk)
				     SOURCES.fds.rev-head))))

(define (readable port/fd handler-thunk)
  (define who 'readable)
  (with-arguments-validation (who)
      ((port/file-descriptor	port/fd)
       (procedure		handler-thunk))
    (let ((fd (if (port? port/fd)
		  (port-fd port/fd)
		port/fd)))
      (%enqueue-fd-event-source fd
				(lambda ()
				  (px.select-fd-readable? fd 0 0))
				handler-thunk))))

(define (writable port/fd handler-thunk)
  (define who 'writable)
  (with-arguments-validation (who)
      ((port/file-descriptor	port/fd)
       (procedure		handler-thunk))
    (let ((fd (if (port? port/fd)
		  (port-fd port/fd)
		port/fd)))
      (%enqueue-fd-event-source fd (lambda ()
				     (px.select-fd-writable? fd 0 0))
				handler-thunk))))

(define (exception port/fd handler-thunk)
  (define who 'exception)
  (with-arguments-validation (who)
      ((port/file-descriptor	port/fd)
       (procedure		handler-thunk))
    (let ((fd (if (port? port/fd)
		  (port-fd port/fd)
		port/fd)))
      (%enqueue-fd-event-source fd (lambda ()
				     (px.select-fd-exceptional? fd 0 0))
				handler-thunk))))

(define (forget-fd port/fd)
  (define who 'exception)
  (with-arguments-validation (who)
      ((port/file-descriptor	port/fd))
    (let ((fd (if (port? port/fd)
		  (port-fd port/fd)
		port/fd)))
      (with-event-sources (SOURCES)
	(set! SOURCES.fds.tail (remp (lambda (E)
				       (unsafe.fx= fd (fd-entry-fd E)))
				 SOURCES.fds.tail))
	(set! SOURCES.fds.rev-head (remp (lambda (E)
					   (unsafe.fx= fd (fd-entry-fd E)))
				     SOURCES.fds.rev-head))))))


;;;; task fragments handling
;;
;;A "fragmented task" is a thunk  performing a portion of a job.  If the
;;thunk  returns  #f: the  job  is finished.   If  the  thunk returns  a
;;procedure: a portion of job was finished and the returned procedure is
;;a thunk to call to execute the next portion.
;;

(define (task-fragment handler-thunk)
  ;;Enqueue a new entry for a fragmented task.
  ;;
  (with-event-sources (SOURCES)
    (set! SOURCES.tasks.rev-head (cons handler-thunk SOURCES.tasks.rev-head))))

(define (do-one-task-event)
  ;;Consume one event, if any, and  return.  Return a boolean, #t if one
  ;;event was served.
  ;;
  ;;Exceptions  raised while serving  an event  handler are  catched and
  ;;ignored.
  ;;
  (with-event-sources (SOURCES)
    (when (and (null? SOURCES.tasks.tail)
	       (not (null? SOURCES.tasks.rev-head)))
      (set! SOURCES.tasks.tail     (reverse SOURCES.tasks.rev-head))
      (set! SOURCES.tasks.rev-head '()))
    (if (null? SOURCES.tasks.tail)
	#f
      (let ((thunk (unsafe.car SOURCES.tasks.tail)))
	(set! SOURCES.tasks.tail (unsafe.cdr SOURCES.tasks.tail))
	(let ((rv (thunk)))
	  (when rv
	    (set! SOURCES.tasks.rev-head (cons rv SOURCES.tasks.rev-head)))
	  #t)))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-event-sources 'scheme-indent-function 1)
;; End:
