;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test implementation of POSIX functions
;;;Date: Mon Jun  7, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (rename (vicare) #;(ikarus)
		(parameterize	parametrise))
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare POSIX functions\n")


(parametrise ((check-test-name	'system))

  (check
      (system "echo innocuous output from system call ; exit 0")
    => 0)

  (check
      (let ((status (system "echo innocuous output from system call ; exit 0")))
	(WIFEXITED status))
    => #t)

  (check
      (system "echo innocuous output from system call ; exit 4")
    => 1)

  #t)


(parametrise ((check-test-name	'getpid))

  #;(begin
    (display "result of getpid() is " )
    (display (getpid))
    (newline))

  (check
      (fixnum? (getpid))
    => #t)

  (check
      (fixnum? (getppid))
    => #t)

  #t)


(parametrise ((check-test-name	'directory-stream))

  (check	;verify that no error occurs
      (let ((stream (open-directory-stream "..")))
;;;(check-pretty-print stream)
	(let loop ((entry (read-directory-stream stream)))
;;;(check-pretty-print entry)
	  (if entry
	      (loop (read-directory-stream stream))
	    (close-directory-stream stream)))
	(directory-stream? stream))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
