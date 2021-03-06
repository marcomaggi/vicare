;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for promises
;;;Date: Mon Oct 31, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare promises functions\n")


(parametrise ((check-test-name	'core))

  (check
      (force (delay 123))
    => 123)

  (check
      (force (delay (+ 1 2)))
    => 3)

  (check
      (with-result
       (force (delay (let ((R (+ 1 2)))
		       (add-result R)
		       R))))
    => '(3 (3)))

  (check
      (promise? (delay 123))
    => #t)

  (check
      (with-result
       (let ((p (delay (let ((r (+ 1 2)))
			 (add-result r)
			 r))))
	 (let ((a (force p)))
	   (list a (force p)))))
    => '((3 3) (3)))

  #t)


;;;; done

(check-report)

;;; end of file
