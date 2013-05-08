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
	  pid-file.)
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare: PID files\n")


;;; helpers

(define (clean-pid-file)
  (when (and (pid-file.pid-pathname)
	     (file-exists? (pid-file.pid-pathname)))
    (delete-file (pid-file.pid-pathname))))

(define (read-pid-file)
  ((pid-file.textual-contents-reading-procedure)
   (pid-file.pid-pathname)))

(define (pid-file-exists?)
  (file-exists? (pid-file.pid-pathname)))

(define (log-procedure template . args)
  (add-result (apply format template args)))

(define-constant CONTENTS
  (string-append (number->string (px.getpid)) "\n"))


(parametrise ((check-test-name	'basic))

  (check	;creation and removal
      (with-result
       (parametrise
	   ((pid-file.pid-pathname "./pid-file.001"))
	 (clean-pid-file)
	 (begin0
	     (begin
	       (pid-file.create-pid-file)
	       (begin0
		   (read-pid-file)
		 (pid-file.remove-pid-file)))
	   (add-result (pid-file-exists?)))))
    => `(,CONTENTS (#f)))

  (check	;creation and removal with logging
      (with-result
       (parametrise ((pid-file.pid-pathname	"./pid-file.002")
		     (pid-file.log-procedure	log-procedure))
	 (clean-pid-file)
	 (begin0
	     (begin
	       (pid-file.create-pid-file)
	       (begin0
		   ((pid-file.textual-contents-reading-procedure)
		    (pid-file.pid-pathname))
		 (pid-file.remove-pid-file)))
	   (add-result (pid-file-exists?)))))
    => `(,CONTENTS
	 ("creating PID file: ./pid-file.002"
	  "removing PID file: ./pid-file.002"
	  #f)))

  (check	;disabled
      (parametrise ((pid-file.pid-pathname	#f)
		    #;(pid-file.log-procedure	log-procedure))
	(clean-pid-file)
        (pid-file.create-pid-file)
	(pid-file.remove-pid-file))
    => (void))

  (check	;compensated
      (with-result
       (parametrise ((pid-file.pid-pathname	"./pid-file.003")
		     (pid-file.log-procedure	log-procedure))
	 (clean-pid-file)
	 (begin0
	     (with-compensations
	       (pid-file.setup-compensated-pid-file-creation)
	       (read-pid-file))
	   (add-result (pid-file-exists?)))))
    => `(,CONTENTS
	 ("creating PID file: ./pid-file.003"
	  "removing PID file: ./pid-file.003"
	  #f)))

  #t)


(parametrise ((check-test-name	'creation-errors))

  (check	;file already exists
      (with-result
       (guard (E ((pid-file.pid-file-already-exists-condition? E)
		  #t)
		 (else E))
	 (parametrise
	     ((pid-file.pid-pathname	"./pid-file.101")
	      (pid-file.log-procedure	log-procedure)
	      (pid-file.file-existence-procedure (lambda (pathname) #t)))
	   (clean-pid-file)
	   (pid-file.create-pid-file))))
    => `(#t
	 ("while creating PID file: selected PID file pathname already exists")))

  (check	;file already exists
      (with-result
       (guard (E ((and (pid-file.pid-file-creation-condition? E)
		       (i/o-write-error? E))
		  #t)
		 (else E))
	 (parametrise
	     ((pid-file.pid-pathname	"./pid-file.102")
	      (pid-file.log-procedure	log-procedure)
	      (pid-file.textual-contents-writing-procedure
	       (lambda (pathname contents)
		 (raise
		  (condition (make-i/o-write-error)
			     (make-message-condition "error writing file"))))))
	   (clean-pid-file)
	   (pid-file.create-pid-file))))
    => `(#t
	 ("creating PID file: ./pid-file.102"
	  "while creating PID file: error writing file")))

  #t)


(parametrise ((check-test-name	'removal-errors))

  (check	;file already exists
      (with-result
       (parametrise
	   ((pid-file.pid-pathname	"./pid-file.201")
	    (pid-file.log-procedure	log-procedure))
	 (guard (E ((pid-file.pid-file-missing-condition? E)
		    #t)
		   (else E))
	   (clean-pid-file)
	   (pid-file.create-pid-file)
	   (clean-pid-file)
	   (pid-file.remove-pid-file))))
    => `(#t
	 ("creating PID file: ./pid-file.201"
	  "while removing PID file: expected PID file does not exist")))

  (check	;error reading
      (with-result
       (parametrise
	   ((pid-file.pid-pathname	"./pid-file.202")
	    (pid-file.log-procedure	log-procedure)
	    (pid-file.textual-contents-reading-procedure
	     (lambda (pathname)
	       (raise
		(condition (make-i/o-read-error)
			   (make-message-condition "error writing file"))))))
	 (guard (E ((and (pid-file.pid-file-removal-condition? E)
			 (i/o-read-error? E))
		    (clean-pid-file)
		    #t)
		   (else E))
	   (clean-pid-file)
	   (pid-file.create-pid-file)
	   (pid-file.remove-pid-file))))
    => `(#t
	 ("creating PID file: ./pid-file.202"
	  "while removing PID file: error writing file")))

  (check	;invalid contents
      (with-result
       (parametrise
	     ((pid-file.pid-pathname	"./pid-file.203")
	      (pid-file.log-procedure	log-procedure)
	      (pid-file.textual-contents-reading-procedure
	       (lambda (pathname)
		 "ciao\n")))
	 (guard (E ((pid-file.pid-file-invalid-contents-condition? E)
		    (clean-pid-file)
		    #t)
		   (else E))
	   (clean-pid-file)
	   (pid-file.create-pid-file)
	   (pid-file.remove-pid-file))))
    => `(#t
	 ("creating PID file: ./pid-file.203"
	  "while removing PID file: corrupted PID file contents, avoiding removal")))

  (check	;error removing
      (with-result
       (parametrise
	   ((pid-file.pid-pathname	"./pid-file.204")
	    (pid-file.log-procedure	log-procedure)
	    (pid-file.file-removal-procedure
	     (lambda (pathname)
	       (raise
		(condition (make-i/o-filename-error pathname)
			   (make-message-condition "error removing file"))))))
	 (guard (E ((and (pid-file.pid-file-removal-condition? E)
			 (i/o-filename-error? E))
		    (clean-pid-file)
		    #t)
		   (else E))
	   (clean-pid-file)
	   (pid-file.create-pid-file)
	   (pid-file.remove-pid-file))))
    => `(#t
	 ("creating PID file: ./pid-file.204"
	  "removing PID file: ./pid-file.204"
	  "while removing PID file: error removing file")))

  #t)


;;;; done

(check-report)

;;; end of file
