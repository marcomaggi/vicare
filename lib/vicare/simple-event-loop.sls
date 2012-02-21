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


;;;; data structures

(define-struct event-sources
  (break? fds))

(define SOURCES
  (make-event-sources #f '()))

(define (do-one-event)
  ;;Consume one event and return.
  ;;
  ;;Exceptions raised while querying an event source or serving an event
  ;;handler are catched and ignored.
  ;;
  (let ((fds (event-sources-fds SOURCES)))
    (unless (null? fds)
      (guard (E (else #f))
	(let ((P (unsafe.car fds)))
	  (when ((unsafe.car P))
	    ((unsafe.cdr P)))))
      (set-event-sources-fds! SOURCES (unsafe.cdr fds)))))

(define (busy?)
  ;;Return true if there is at least one registered event source.
  ;;
  (not (null? (event-sources-fds SOURCES))))

(define (enter)
  ;;Enter the event loop and consume all the events.
  ;;
  (if (event-sources-break? SOURCES)
      (set-event-sources-break?! SOURCES #f)
    (begin
      (do-one-event)
      (enter))))

(define (leave-asap)
  ;;Leave the event loop as soon as possible.
  ;;
  (set-event-sources-break?! SOURCES #t))


;;;; file descriptor events

(define (readable fd handler-thunk)
  (define who 'readable)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (procedure	handler-thunk))
    (set-event-sources-fds! SOURCES
			    (cons (cons (lambda ()
					  (let-values (((r w x)
							(px.select-fd fd 0 0)))
					    r))
					handler-thunk)
				  (event-sources-fds SOURCES)))))

(define (writable fd handler-thunk)
  (define who 'writable)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (procedure	handler-thunk))
    (set-event-sources-fds! SOURCES
			    (cons (cons (lambda ()
					  (let-values (((r w x)
							(px.select-fd fd 0 0)))
					    w))
					handler-thunk)
				  (event-sources-fds SOURCES)))))

(define (exception fd handler-thunk)
  (define who 'exception)
  (with-arguments-validation (who)
      ((file-descriptor	fd)
       (procedure	handler-thunk))
    (set-event-sources-fds! SOURCES
			    (cons (cons (lambda ()
					  (let-values (((r w x)
							(px.select-fd fd 0 0)))
					    x))
					handler-thunk)
				  (event-sources-fds SOURCES)))))


;;;; done

)

;;; end of file
