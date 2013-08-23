;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 111
;;;Date: Fri Aug 23, 2013
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
  (prefix (srfi :111) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI library: 111, boxes\n")


(parametrise ((check-test-name	'base))

  (check
      (let ((B (srfi.box 123)))
        (srfi.box? B))
    => #t)

  (check
      (srfi.box? 123)
    => #f)

  (check
      (let ((B (srfi.box 123)))
	(srfi.unbox B))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (srfi.unbox 123)
    => 123)

  #t)


;;;; done

(check-report)

;;; end of file
