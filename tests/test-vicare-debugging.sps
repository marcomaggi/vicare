;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for debugging utilities
;;;Date: Sun Mar 25, 2012
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare debugging utilities\n")


(parametrise ((check-test-name	'words))

  (check
      (integer->machine-word #b11100)
    => #b111)

  (check
      (integer->machine-word #x3F)
    => #t)

  (check
      (integer->machine-word #x2F)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (machine-word->integer #t)
    => #x3F)

  (check
      (machine-word->integer #f)
    => #x2F)

  #t)


;;;; done

(check-report)

;;; end of file
