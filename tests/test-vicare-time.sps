;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for basic time functions
;;;Date: Thu Nov 15, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare basic time functions\n")


(parametrise ((check-test-name	'base))

  (when #f
    (let ((T (current-time)))
      (check-pretty-print (list T
				(time-second     T)
				(time-nanosecond T)
				(time-gmt-offset T)))))

  (check
      (time? (current-time))
    => #t)

  (check
      (let ((s (time-second (current-time))))
	(or (fixnum? s) (bignum? s)))
    => #t)

  (check
      (let ((s (time-nanosecond (current-time))))
	(or (fixnum? s) (bignum? s)))
    => #t)

  (check
      (fixnum? (time-gmt-offset (current-time)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
