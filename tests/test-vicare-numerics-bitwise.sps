;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numeric functions: bitwise operations
;;;Date: Wed Aug 21, 2013
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare numeric functions: bitwise operations\n")


(parametrise ((check-test-name	'logic))

  (check (bitwise-ior)				=> 0)

  (check (bitwise-ior 0)			=> 0)
  (check (bitwise-ior 123)			=> 123)

  (check (bitwise-ior 0 1)			=> 1)
  (check (bitwise-ior 1 0)			=> 1)
  (check (bitwise-ior 1 1)			=> 1)

;;; --------------------------------------------------------------------

  (check (bitwise-xor)				=> 0)

  (check (bitwise-xor 0)			=> 0)
  (check (bitwise-xor 123)			=> 123)

  (check (bitwise-xor 0 1)			=> 1)
  (check (bitwise-xor 1 0)			=> 1)
  (check (bitwise-xor 1 1)			=> 0)

;;; --------------------------------------------------------------------

  (check (bitwise-and)				=> -1)

  (check (bitwise-and 0)			=> 0)
  (check (bitwise-and 123)			=> 123)

  (check (bitwise-and 0 1)			=> 0)
  (check (bitwise-and 1 0)			=> 0)
  (check (bitwise-and 1 1)			=> 1)

  #t)


;;;; done

(check-report)

;;; end of file
