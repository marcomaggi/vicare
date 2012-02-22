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
    busy?
    do-one-event
    enter
    leave-asap

    ;; file descriptor events
    readable
    writable
    exception
    )
  (import (vicare)
    (prefix (vicare posix) px.)
    (prefix (vicare unsafe-operations) unsafe.)
    (vicare syntactic-extensions)
    (vicare platform-constants))


;;;; arguments validation

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (file-descriptor who obj)
  (%file-descriptor? obj)
  (assertion-violation who "expected fixnum file descriptor as argument" obj))


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


;;;; data structures

(define-struct event-sources
  (break? fds-rev-head fds-tail))

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
	      (SRC.FDS-REV-HEAD		(%dot-id ".fds-rev-head"))
	      (SRC.FDS-TAIL		(%dot-id ".fds-tail")))
	   #'(let-syntax
		 ((SRC.BREAK?
		   (identifier-syntax
		    (_
		     (event-sources-break? ?src))
		    ((set! _ ?val)
		     (set-event-sources-break?! ?src ?val))))
		  (SRC.FDS-REV-HEAD
		   (identifier-syntax
		    (_
		     (event-sources-fds-rev-head ?src))
		    ((set! _ ?val)
		     (set-event-sources-fds-rev-head! ?src ?val))))
		  (SRC.FDS-TAIL
		   (identifier-syntax
		    (_
		     (event-sources-fds-tail ?src))
		    ((set! _ ?val)
		     (set-event-sources-fds-tail! ?src ?val)))))
	       . ?body)))))))

(define SOURCES
  (make-event-sources #f '() '()))


;;;; event loop control

(define (do-one-event)
  ;;Consume one event and return.
  ;;
  ;;Exceptions raised while querying an event source or serving an event
  ;;handler are catched and ignored.
  ;;
  ;;Handling of fd events:
  ;;
  ;;1. If FDS-TAIL is null replace it with the reverse of FDS-REV-HEAD.
  ;;2. Extract the next entry from FDS-TAIL.
  ;;3. Query the fd for the event.
  ;;4a. If event present: run the handler and discard the entry.
  ;;4b. If no event: push the entry on FDS-REV-HEAD.
  ;;
  (with-event-sources (SOURCES)
    (when (and (null? SOURCES.fds-tail)
	       (not (null? SOURCES.fds-rev-head)))
      (let ((tail (reverse SOURCES.fds-rev-head)))
	(set! SOURCES.fds-tail     tail)
	(set! SOURCES.fds-rev-head '())))
    (unless (null? SOURCES.fds-tail)
      (let ((P (unsafe.car SOURCES.fds-tail)))
	(set! SOURCES.fds-tail (unsafe.cdr SOURCES.fds-tail))
	(if (%catch ((unsafe.car P)))
	    (%catch ((unsafe.cdr P)))
	  (set! SOURCES.fds-rev-head (cons P SOURCES.fds-rev-head)))))))

(define (busy?)
  ;;Return true if there is at least one registered event source.
  ;;
  (with-event-sources (SOURCES)
    (or (not (null? SOURCES.fds-rev-head))
	(not (null? SOURCES.fds-tail)))))

(define (enter)
  ;;Enter the event loop and consume all the events.
  ;;
  (with-event-sources (SOURCES)
    (if SOURCES.break?
	(set! SOURCES.break? #f)
      (begin
	(do-one-event)
	(enter)))))

(define (leave-asap)
  ;;Leave the event loop as soon as possible.
  ;;
  (with-event-sources (SOURCES)
    (set! SOURCES.break? #t)))


;;;; file descriptor events

(define (%enqueue-fd-event-source query-thunk handler-thunk)
  (with-event-sources (SOURCES)
    (set! SOURCES.fds-rev-head (cons (cons query-thunk handler-thunk)
				     SOURCES.fds-rev-head))))

(define (readable fd handler-thunk)
  (define who 'readable)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (procedure	handler-thunk))
    (%enqueue-fd-event-source (lambda ()
				(px.select-fd-readable? fd 0 0))
			      handler-thunk)))

(define (writable fd handler-thunk)
  (define who 'writable)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (procedure	handler-thunk))
    (%enqueue-fd-event-source (lambda ()
				(px.select-fd-writable? fd 0 0))
			      handler-thunk)))

(define (exception fd handler-thunk)
  (define who 'exception)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (procedure	handler-thunk))
    (%enqueue-fd-event-source (lambda ()
				(px.select-fd-exceptional? fd 0 0))
			      handler-thunk)))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-event-sources 'scheme-indent-function 1)
;; End:
