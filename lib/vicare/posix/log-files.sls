;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: logging to file facilities
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
(library (vicare posix log-files)
  (export

    ;; configuration
    logging-enabled?
    log-port
    log-prefix
    log-pathname

    ;; log file
    open-logging
    close-logging
    setup-compensated-log-file-creation

    ;; logging
    log
    log-condition-message
    with-logging-handler)
  (import (except (vicare)
		  log)
    (prefix (vicare posix)
	    px.)
    (vicare arguments validation))


;;;; helpers

(define (%format-and-print port template args)
  ;;Format a  line of text  and display it  to the given  textual output
  ;;port.  We expect the port to have buffer mode set to "line".
  ;;
  (fprintf port (log-prefix))
  (apply fprintf port template args)
  (newline port))


;;;; configuration

(define logging-enabled?
  ;;Boolean; true if logging is enabled, false otherwise.
  ;;
  (make-parameter #f
    (lambda (obj)
      (if obj #t #f))))

(define log-port
  ;;A textual  output port to which  log messages must be  written.  The
  ;;port is expected to have "line" buffering.
  ;;
  (make-parameter (current-error-port)
    (lambda (obj)
      (define who 'log-port)
      (with-arguments-validation (who)
	  ((output-port		obj)
	   (textual-port	obj))
	obj))))

(define log-prefix
  ;;A string representing the prefix for every log message.
  ;;
  (make-parameter ""
    (lambda (obj)
      (define who 'log-prefix)
      (with-arguments-validation (who)
	  ((string	obj))
	obj))))

(define log-pathname
  ;;False or  a Scheme string  representing the log file  pathname.  The
  ;;special string "-" means: log to the current error port.
  ;;
  (make-parameter "-"
    (lambda (obj)
      (define who 'log-pathname)
      (with-arguments-validation (who)
	  ((non-empty-string/false	obj))
	obj))))


;;;; log files

(define (setup-compensated-log-file-creation)
  (compensate
      (open-logging)
    (with
     (close-logging))))

(define (open-logging)
  ;;If logging  is enabled: configure  the log port;  return unspecified
  ;;values.  If  the selected  pathname is "-"  assume the  log messages
  ;;must go to the current error port.  Otherwise open a log file.
  ;;
  (when (logging-enabled?)
    (if (string=? "-" (log-pathname))
	(log-port (current-error-port))
      (let ((size (if (file-exists? (log-pathname))
		      (px.file-size (log-pathname))
		    0)))
	(when (log-pathname)
	  (log-port (open-file-output-port (log-pathname)
					   (file-options no-fail no-truncate)
					   (buffer-mode line)
					   (native-transcoder)))
	  (with-compensations/on-error
	    ;;Close the port if setting the position fails.
	    (push-compensation
	     (close-port (log-port)))
	    (set-port-position! (log-port) size))))))
  (void))

(define (close-logging)
  ;;Close  the log  port unless  it is  the current  error port;  return
  ;;unspecified values.   Notice that the LOGGING-ENABLED?  parameter is
  ;;ignored.
  ;;
  (when (and (log-port)
	     (not (string=? "-" (log-pathname)))
	     (not (equal? (log-port)
			  (current-error-port))))
    (close-port (log-port)))
  (void))


;;;; logging

(define (log template . args)
  ;;If logging  is enabled:  format a  log message and  write it  to the
  ;;current log port.  Return unspecified values.
  ;;
  (when (and (logging-enabled?)
	     (log-port))
    (let* ((date	(px.strftime/string "%FT%T%Z" (px.localtime (px.time))))
	   (template	(string-append (format "~a: " date) template)))
      (%format-and-print (log-port) template args)))
  (void))

(define (log-condition-message template cnd)
  ;;If logging is  enabled: format a log message  extracting the message
  ;;from the condition object CND.   The template is expected to contain
  ;;a "~a" sequence to be replaced by the condition message.
  ;;
  (log template (if (message-condition? cnd)
		    (condition-message cnd)
		  "non-described exception")))

(define-syntax with-logging-handler
  ;;Evaluate the  body forms;  in case  of exception  log a  message and
  ;;raise again.
  ;;
  (syntax-rules (condition-message)
    ((_ (condition-message ?template) ?body0 ?body ...)
     (with-exception-handler
	 (lambda (E)
	   (log-condition-message ?template E)
	   (raise-continuable E))
       (lambda () ?body0 ?body ...)))))


;;;; done

)

;;; end of file
