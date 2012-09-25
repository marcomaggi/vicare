;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 13, string-titlecase segfault
;;;Date: Mon Jun 14, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(check-display "*** testing issue 13, string-titlecase segfault\n")


;;;; code

(check
    (string-titlecase "12")
  => "12")

(check
    (string-titlecase "kNock KNoCK")
  => "Knock Knock")

(check
    (string-titlecase "who's there?")
  => "Who's There?")

(check
    (string-titlecase "r6rs")
  => "R6rs")

(check
    (string-titlecase "R6RS")
  => "R6rs")


;;;; done

(check-report)

;;; end of file
