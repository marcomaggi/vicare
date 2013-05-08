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

(define (log-procedure template . args)
  (add-result (apply format template args)))

(define-constant CONTENTS
  (string-append (number->string (px.getpid)) "\n"))


(parametrise ((check-test-name	'base))

  (check	;creation and removal
      (parametrise ((pid-file.pid-pathname	"./pid-file.001")
		    #;(pid-file.log-procedure	log-procedure))
	(clean-pid-file)
        (pid-file.create-pid-file)
	(begin0
	    ((pid-file.textual-contents-reading-procedure)
	     (pid-file.pid-pathname))
	  (pid-file.remove-pid-file)))
    => CONTENTS)

  (check	;creation and removal with logging
      (with-result
       (parametrise ((pid-file.pid-pathname	"./pid-file.002")
		     (pid-file.log-procedure	log-procedure))
	 (clean-pid-file)
	 (pid-file.create-pid-file)
	 (begin0
	     ((pid-file.textual-contents-reading-procedure)
	      (pid-file.pid-pathname))
	   (pid-file.remove-pid-file))))
    => `(,CONTENTS
	 ("creating PID file: ./pid-file.002"
	  "removing PID file: ./pid-file.002")))

  (check	;disabled
      (parametrise ((pid-file.pid-pathname	#f)
		    #;(pid-file.log-procedure	log-procedure))
	(clean-pid-file)
        (pid-file.create-pid-file)
	(pid-file.remove-pid-file))
    => (void))

  #t)


;;;; done

(check-report)

;;; end of file
