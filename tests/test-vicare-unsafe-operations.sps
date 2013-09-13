;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for unsafe operations
;;;Date: Fri Sep 13, 2013
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
  (vicare unsafe operations)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: unsafe operations\n")


(parametrise ((check-test-name	'strings))

;;; $string-copy

  (check
      (let* ((src.str "ciao")
	     (dst.str ($string-copy src.str)))
        (string=? src.str dst.str))
    => #t)

  (check
      (let* ((src.str "")
	     (dst.str ($string-copy src.str)))
        (string-length dst.str))
    => 0)

  #t)


;;;; done

(check-report)

;;; end of file
