;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 12, timer returning Scheme values
;;;Date: Mon Jun  7, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (rnrs)
  (ikarus)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 12, timer returning values\n")


;;;; code

(check
    (let* ((a #f)
	   (b #f)
	   (c (time-and-gather
	       (lambda (stats-before stats-after)
		 (set! a (stats? stats-before))
		 (set! b (stats? stats-after)))
	       (lambda ()
		 (+ 1 2)))))
      (list a b c))
  => '(#t #t 3))


;;;; done

(check-report)

;;; end of file
