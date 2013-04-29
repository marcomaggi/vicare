;;;
;;;Part of: Vicare Scheme
;;;Contents: test for general SRFI 8
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
(import (except (vicare)
		receive)
  (srfi :8)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 8, receive\n")


(parametrise ((check-test-name	'basic))

  (check
      (receive (a)
	  (values 1)
	a)
    => 1)

  (check
      (receive (a b)
	  (values 1 2)
	(list a b))
    => '(1 2))

  (check
      (receive (a b c)
	  (values 1 2 3)
	(list a b c))
    => '(1 2 3))

  #t)


;;;; done

(check-report)

;;; end of file
