;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: lock pid files facilites
;;;Date: Thu May  9, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare posix lock-pid-files)
  (export with-lock-pid-file)
  (import (vicare)
    (prefix (vicare posix) px.)
    (vicare platform constants)
    (vicare arguments validation))


(define (with-lock-pid-file lock-pathname log thunk)
  (define who 'with-lock-pid-file)

  (define (log-condition-message template cnd)
    (log template (if (message-condition? cnd)
		      (condition-message cnd)
		    "non-described exception")))

  (define-syntax with-logging-handler
    (syntax-rules (condition-message)
      ((_ (condition-message ?template) ?body0 ?body ...)
       (with-exception-handler
	   (lambda (E)
	     (log-condition-message ?template E)
	     (raise-continuable E))
	 (lambda () ?body0 ?body ...)))))

  (with-arguments-validation (who)
      ((non-empty-string	lock-pathname)
       (procedure		log)
       (procedure		thunk))
    (with-compensations
      (define fd
	(compensate
	  (with-logging-handler
	      (condition-message "while creating lock PID file: ~a")
	    (log "creating lock PID file: ~a" lock-pathname)
	    (px.open lock-pathname
		     (fxior O_CREAT O_EXCL O_WRONLY)
		     (fxior S_IRUSR S_IWUSR)))
	  (with
	   (with-logging-handler
	       (condition-message "while removing lock PID file: ~a")
	     (px.close fd)
	     (log "removing lock PID file: ~a" lock-pathname)
	     (delete-file lock-pathname)))))
      (with-logging-handler
	  (condition-message "while locking PID file: ~a")
	;;This locks the file or fail raising an exception.
	(px.lockf fd F_TLOCK 0)
	(px.write fd (string->ascii
		      (string-append (number->string (px.getpid)) "\n"))))
      (thunk))))


;;;; done

)

;;; end of file
