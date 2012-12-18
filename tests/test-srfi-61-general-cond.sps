;;;
;;;Part of: Vicare Scheme
;;;Contents: test for general cond
;;;Date: Tue Dec 18, 2012
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
(import (rnrs)
  (prefix (srfi :61) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 61, general cond\n")


(check
    (let ((a 1))
      (srfi.cond ((= a 1)	1)
		 ((= a 2)	2)))
  => 1)

(check
    (let ((a 2))
      (srfi.cond ((= a 1)	1)
		 ((= a 2)	2)))
  => 2)

(check
    (let ((a 3))
      (srfi.cond ((= a 1)	1)
		 ((= a 2)	2)
		 (else		3)))
  => 3)

(check
    (let ((a 1))
      (srfi.cond ((= a 1)	1)
		 ((= a 2)
		  => (lambda (A)
		       A))))
  => 1)

(check
    (let ((a 2))
      (srfi.cond ((= a 1)	1)
		 ((= a 2)
		  => (lambda (A)
		       A))))
  => #t)


;;;; done

(check-report)

;;; end of file
