;;;
;;;Part of: Vicare Scheme
;;;Contents: test for general SRFI 6
;;;Date: Fri Dec 21, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (srfi :6)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 6, basic string ports\n")


(parametrise ((check-test-name	'output))

  (check
      (let ((port (open-output-string)))
	(display "ciao" port)
	(flush-output-port port)
	(get-output-string port))
    => "ciao")

  #t)


(parametrise ((check-test-name	'input))

  (check
      (let* ((port (open-input-string "ciao"))
	     (res1 (read port))
	     (res2 (read port)))
	(list res1 (eof-object? res2)))
    => (list 'ciao #t))

  #t)


;;;; done

(check-report)

;;; end of file
