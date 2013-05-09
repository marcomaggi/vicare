;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for logging facilities
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
(import (vicare)
  (prefix (vicare posix log-files) log.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: POSIX log files\n")


(parametrise ((check-test-name	'basic))

  (check
      (receive (port getter)
	  (open-string-output-port)
	(parametrise ((log.logging-enabled?	#t)
		      (log.log-port		port))
	  (log.log "ciao")
	  (let* ((S     (getter))
		 (S.len (string-length S)))
	    (substring S (+ -5 S.len) S.len))))
    => "ciao\n")

  #t)


;;;; done

(check-report)

;;; end of file
