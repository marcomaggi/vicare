;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for lock pid files
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
  (vicare posix lock-pid-files)
  (vicare platform constants)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: POSIX lock PID files\n")


(parametrise ((check-test-name	'basic))

  (check
      (with-lock-pid-file "lock-pid-file.001"
	(lambda ()
	  'ok))
    => 'ok)

  (check
      (guard (E ((errno-condition? E)
		 (condition-errno E))
		(else E))
	(with-lock-pid-file "lock-pid-file.002"
	  (lambda ()
	    (with-lock-pid-file "lock-pid-file.002"
	      (lambda ()
		'error)))))
    => EEXIST)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-lock-pid-file 'scheme-indent-function 1)
;; End:
