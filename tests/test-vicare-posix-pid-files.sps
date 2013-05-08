;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the pid-files facilities
;;;Date: Wed May  8, 2013
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
(import (vicare)
  (prefix (vicare posix)
	  px.)
  (prefix (vicare posix pid-files)
	  pidfile.)
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare: PID files\n")


;;; helpers

(define (clean-pid-file)
  (when (and (pidfile.pid-pathname)
	     (file-exists? (pidfile.pid-pathname)))
    (delete-file (pidfile.pid-pathname))))

(define (read-pid-file)
  ((pidfile.textual-contents-reading-procedure)
   (pidfile.pid-pathname)))

(define (pid-file-exists?)
  (file-exists? (pidfile.pid-pathname)))

(define (log-procedure template . args)
  (add-result (apply format template args)))

(define-constant CONTENTS
  (string-append (number->string (px.getpid)) "\n"))


(parametrise ((check-test-name	'basic))

  (check	;creation and removal
      (with-result
       (parametrise
	   ((pidfile.pid-pathname "./pidfile.001"))
	 (clean-pid-file)
	 (begin0
	     (begin
	       (pidfile.create-pid-file)
	       (begin0
		   (read-pid-file)
		 (pidfile.remove-pid-file)))
	   (add-result (pid-file-exists?)))))
    => `(,CONTENTS (#f)))

  (check	;creation and removal with logging
      (with-result
       (parametrise ((pidfile.pid-pathname	"./pidfile.002")
		     (pidfile.log-procedure	log-procedure))
	 (clean-pid-file)
	 (begin0
	     (begin
	       (pidfile.create-pid-file)
	       (begin0
		   ((pidfile.textual-contents-reading-procedure)
		    (pidfile.pid-pathname))
		 (pidfile.remove-pid-file)))
	   (add-result (pid-file-exists?)))))
    => `(,CONTENTS
	 ("creating PID file: ./pidfile.002"
	  "removing PID file: ./pidfile.002"
	  #f)))

  (check	;disabled
      (parametrise ((pidfile.pid-pathname	#f)
		    #;(pidfile.log-procedure	log-procedure))
	(clean-pid-file)
        (pidfile.create-pid-file)
	(pidfile.remove-pid-file))
    => (void))

  (check	;compensated
      (with-result
       (parametrise ((pidfile.pid-pathname	"./pidfile.003")
		     (pidfile.log-procedure	log-procedure))
	 (clean-pid-file)
	 (begin0
	     (with-compensations
	       (pidfile.setup-compensated-pid-file-creation)
	       (read-pid-file))
	   (add-result (pid-file-exists?)))))
    => `(,CONTENTS
	 ("creating PID file: ./pidfile.003"
	  "removing PID file: ./pidfile.003"
	  #f)))

  #t)


(parametrise ((check-test-name	'creation-errors))

  (check	;file already exists
      (with-result
       (guard (E ((pidfile.pid-file-already-exists-condition? E)
		  #t)
		 (else E))
	 (parametrise
	     ((pidfile.pid-pathname	"./pidfile.101")
	      (pidfile.log-procedure	log-procedure)
	      (pidfile.file-existence-procedure (lambda (pathname) #t)))
	   (clean-pid-file)
	   (pidfile.create-pid-file))))
    => `(#t
	 ("while creating PID file: selected PID file pathname already exists")))

  (check	;file already exists
      (with-result
       (guard (E ((and (pidfile.pid-file-creation-condition? E)
		       (i/o-write-error? E))
		  #t)
		 (else E))
	 (parametrise
	     ((pidfile.pid-pathname	"./pidfile.102")
	      (pidfile.log-procedure	log-procedure)
	      (pidfile.textual-contents-writing-procedure
	       (lambda (pathname contents)
		 (raise
		  (condition (make-i/o-write-error)
			     (make-message-condition "error writing file"))))))
	   (clean-pid-file)
	   (pidfile.create-pid-file))))
    => `(#t
	 ("creating PID file: ./pidfile.102"
	  "while creating PID file: error writing file")))

  #t)


(parametrise ((check-test-name	'removal-errors))

  (check	;file already exists
      (with-result
       (parametrise
	   ((pidfile.pid-pathname	"./pidfile.201")
	    (pidfile.log-procedure	log-procedure))
	 (guard (E ((pidfile.pid-file-missing-condition? E)
		    #t)
		   (else E))
	   (clean-pid-file)
	   (pidfile.create-pid-file)
	   (clean-pid-file)
	   (pidfile.remove-pid-file))))
    => `(#t
	 ("creating PID file: ./pidfile.201"
	  "while removing PID file: expected PID file does not exist")))

  (check	;error reading
      (with-result
       (parametrise
	   ((pidfile.pid-pathname	"./pidfile.202")
	    (pidfile.log-procedure	log-procedure)
	    (pidfile.textual-contents-reading-procedure
	     (lambda (pathname)
	       (raise
		(condition (make-i/o-read-error)
			   (make-message-condition "error writing file"))))))
	 (guard (E ((and (pidfile.pid-file-removal-condition? E)
			 (i/o-read-error? E))
		    (clean-pid-file)
		    #t)
		   (else E))
	   (clean-pid-file)
	   (pidfile.create-pid-file)
	   (pidfile.remove-pid-file))))
    => `(#t
	 ("creating PID file: ./pidfile.202"
	  "while removing PID file: error writing file")))

  (check	;invalid contents
      (with-result
       (parametrise
	     ((pidfile.pid-pathname	"./pidfile.203")
	      (pidfile.log-procedure	log-procedure)
	      (pidfile.textual-contents-reading-procedure
	       (lambda (pathname)
		 "ciao\n")))
	 (guard (E ((pidfile.pid-file-invalid-contents-condition? E)
		    (clean-pid-file)
		    #t)
		   (else E))
	   (clean-pid-file)
	   (pidfile.create-pid-file)
	   (pidfile.remove-pid-file))))
    => `(#t
	 ("creating PID file: ./pidfile.203"
	  "while removing PID file: corrupted PID file contents, avoiding removal")))

  (check	;error removing
      (with-result
       (parametrise
	   ((pidfile.pid-pathname	"./pidfile.204")
	    (pidfile.log-procedure	log-procedure)
	    (pidfile.file-removal-procedure
	     (lambda (pathname)
	       (raise
		(condition (make-i/o-filename-error pathname)
			   (make-message-condition "error removing file"))))))
	 (guard (E ((and (pidfile.pid-file-removal-condition? E)
			 (i/o-filename-error? E))
		    (clean-pid-file)
		    #t)
		   (else E))
	   (clean-pid-file)
	   (pidfile.create-pid-file)
	   (pidfile.remove-pid-file))))
    => `(#t
	 ("creating PID file: ./pidfile.204"
	  "removing PID file: ./pidfile.204"
	  "while removing PID file: error removing file")))

  #t)


;;;; done

(check-report)

;;; end of file
