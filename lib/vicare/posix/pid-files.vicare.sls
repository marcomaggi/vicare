;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: PID file handling
;;;Date: Wed May  8, 2013
;;;
;;;Abstract
;;;
;;;	The PID  file is  a textual  file in  ASCII encoding  created at
;;;	start  time and  deleted  at  exit time;  it  contains a  string
;;;	representing the PID number of the current process.
;;;
;;;	  The  PID  file  is  especially useful  whenever  a  server  is
;;;	executed as  daemon: it  allows for quick  retrieval of  the PID
;;;	number, which  can be used  to send interprocess signals  to the
;;;	daemon, for  example to shut  it down, or  to restart it,  or to
;;;	update its configuration, or whatever.
;;;
;;;	  When a PID file is used:  the creator process should refuse to
;;;	start if the PID file already exists, because it may mean that a
;;;	program instance is already in execution.
;;;
;;;	  The PID file  must be created after  the daemonisation process
;;;	has completed: this way we correctly store the PID of the daemon
;;;	process, rather than the PID of the starting process.
;;;
;;;	  Logging  operations  related  to  the PID  file  rely  on  the
;;;	function referenced by LOG-PROC; the user just need to configure
;;;	this  parameter  with  an appropriate  function.   The  PID-FILE
;;;	module does not import external libraries or modules for logging
;;;	operations.
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
(library (vicare posix pid-files)
  (export

    ;; configuration
    pid-pathname
    file-existence-procedure
    textual-contents-reading-procedure
    textual-contents-writing-procedure
    file-removal-procedure
    log-procedure

    ;; creation and removal
    setup-compensated-pid-file-creation
    create-pid-file
    remove-pid-file

    ;; condition types
    &pid-file-error
    make-pid-file-error-condition
    pid-file-error-condition?

    &pid-file-already-exists
    make-pid-file-already-exists-condition
    pid-file-already-exists-condition?

    &pid-file-creation
    make-pid-file-creation-condition
    pid-file-creation-condition?

    &pid-file-removal
    make-pid-file-removal-condition
    pid-file-removal-condition?

    &pid-file-invalid-contents
    make-pid-file-invalid-contents-condition
    pid-file-invalid-contents-condition?

    &pid-file-missing
    make-pid-file-missing-condition
    pid-file-missing-condition?)
  (import (except (vicare)
		  log)
    (prefix (only (vicare posix)
		  getpid)
	    px.))


;;;; helpers

(define (%make-pid-file-contents)
  (string-append (number->string (px.getpid)) "\n"))


;;;; condition types

(define-condition-type &pid-file-error
    &error
  make-pid-file-error-condition
  pid-file-error-condition?)

(define-condition-type &pid-file-already-exists
    &pid-file-error
  make-pid-file-already-exists-condition
  pid-file-already-exists-condition?)

(define-condition-type &pid-file-creation
    &pid-file-error
  make-pid-file-creation-condition
  pid-file-creation-condition?)

(define-condition-type &pid-file-removal
    &pid-file-error
  make-pid-file-removal-condition
  pid-file-removal-condition?)

(define-condition-type &pid-file-missing
    &pid-file-error
  make-pid-file-missing-condition
  pid-file-missing-condition?)

(define-condition-type &pid-file-invalid-contents
    &pid-file-error
  make-pid-file-invalid-contents-condition
  pid-file-invalid-contents-condition?)


;;;; error functions

(define (%error-pid-file-already-exists who pid-pathname)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition "selected PID file pathname already exists")
	      (make-pid-file-already-exists-condition)
	      (make-irritants-condition (list pid-pathname)))))

(define (%error-pid-file-missing who pid-pathname)
  (raise
   (condition (make-pid-file-missing-condition)
	      (make-who-condition who)
	      (make-message-condition "expected PID file does not exist")
	      (make-irritants-condition (list pid-pathname)))))

(define (%error-corrupted-pid-file-contents who pid-pathname)
  (raise
   (condition (make-pid-file-invalid-contents-condition)
	      (make-who-condition who)
	      (make-message-condition "corrupted PID file contents, avoiding removal")
	      (make-irritants-condition (list pid-pathname)))))


;;;; configuration

(define pid-pathname
  ;;Set  to  false  or  a  string representing  the  PID  file  absolute
  ;;pathname.  When set to false: no PID file is created.
  ;;
  (make-parameter #f
    (lambda (pid-pathname)
      (if (or (not pid-pathname)
	      (string? pid-pathname)
	      (not (zero? (string-length pid-pathname))))
	  pid-pathname
	(assertion-violation 'pid-pathname
	  "expected false or string as PID-PATHNAME value"
	  pid-pathname)))))

;;; --------------------------------------------------------------------

(define textual-contents-reading-procedure
  ;;Hold a function that reads the PID file contents.
  ;;
  (make-parameter
      (lambda (pid-pathname)
	(with-input-from-file pid-pathname
	  (lambda ()
	    ;;A valid PID  file does not contain a lot  of characters; let's
	    ;;say 32 at most.
	    (get-string-n (current-input-port) 32))))
    (lambda (obj)
      (assert (procedure? obj))
      obj)))

;;; --------------------------------------------------------------------

(define textual-contents-writing-procedure
  ;;Hold a function that writes the PID file contents.
  ;;
  (make-parameter
      (lambda (pid-pathname contents)
	(with-output-to-file pid-pathname
	  (lambda ()
	    (display contents))))
    (lambda (obj)
      (assert (procedure? obj))
      obj)))

(define file-existence-procedure
  ;;Hold a function that tests PID file existence.
  ;;
  (make-parameter
      file-exists?
    (lambda (obj)
      (assert (procedure? obj))
      obj)))

;;; --------------------------------------------------------------------

(define file-removal-procedure
  ;;Hold a function that removes the PID file.
  ;;
  (make-parameter
      delete-file
    (lambda (obj)
      (assert (procedure? obj))
      obj)))

;;; --------------------------------------------------------------------

(define log-procedure
  ;;It must  be set  to a function  accepting FORMAT-like  arguments and
  ;;logging the result:
  ;;
  ;;   (log ?template-string ?arg ...)
  ;;
  (make-parameter (lambda args (void))
    (lambda (obj)
      (assert (procedure? obj))
      obj)))


;;;; creation and removal

(define (setup-compensated-pid-file-creation)
  (compensate
      (create-pid-file)
    (with
     (remove-pid-file))))

(define (create-pid-file)
  ;;If requested:  create the PID file  and write the PID  number in it,
  ;;followed a newline.  Return unspecified values.
  ;;
  (define who 'create-pid-file)
  (when (pid-pathname)
    (with-logging-handler
	(condition-message "while creating PID file: ~a")
      (if ((file-existence-procedure) (pid-pathname))
	  (%error-pid-file-already-exists who (pid-pathname))
	(begin
	  (log "creating PID file: ~a" (pid-pathname))
	  (with-condition-handler
	      (make-pid-file-creation-condition)
	    ((textual-contents-writing-procedure) (pid-pathname) (%make-pid-file-contents)))))))
  (void))

(define (remove-pid-file)
  ;;Remove the PID  file registered in the dynamic  environment, if any.
  ;;Return unspecified values.
  ;;
  (define who 'remove-pid-file)
  (when (pid-pathname)
    (with-logging-handler
	(condition-message "while removing PID file: ~a")
      ;;Preliminary check for file existence.  Almost always if the file
      ;;does not exist we can check it before attempting to access it.
      (unless ((file-existence-procedure) (pid-pathname))
	(%error-pid-file-missing who (pid-pathname)))
      ;;Check that the PID file contains the current process' PID number
      ;;followed by a newline.
      (let ((contents (with-condition-handler
			  (make-pid-file-removal-condition)
			((textual-contents-reading-procedure) (pid-pathname)))))
	(unless (string=? (%make-pid-file-contents) contents)
	  (%error-corrupted-pid-file-contents who (pid-pathname))))
      ;;Remove the PID file.
      (log "removing PID file: ~a" (pid-pathname))
      (with-condition-handler
	  (make-pid-file-removal-condition)
	((file-removal-procedure) (pid-pathname)))))
  (void))


;;;; logging

(define-syntax-rule (log ?template . ?args)
  ((log-procedure) ?template . ?args))

(define (log-condition-message template cnd)
  ;;Log a message including the  string in the "&message" condition CND.
  ;;The string TEMPLATE  must contain a "~a" format  specifier that will
  ;;be replaced with the message string.
  ;;
  (log template (if (message-condition? cnd)
		    (condition-message cnd)
		  "unknown error")))

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
       (lambda () ?body0 ?body ...)))
    ))

(define-syntax with-condition-handler
  (syntax-rules ()
    ((_ ?condition ?body0 ?body ...)
     (with-exception-handler
	 (lambda (E)
	   (raise-continuable (condition E ?condition)))
       (lambda () ?body0 ?body ...)))
    ))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-logging-handler 'scheme-indent-function 1)
;; eval: (put 'with-condition-handler 'scheme-indent-function 1)
;; End:

