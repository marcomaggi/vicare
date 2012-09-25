;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for low level fixnum operations
;;;Date: Wed Oct 19, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (ikarus system $fx)
  (ikarus system $flonums)
  (vicare checks))

(print-unicode #f)
(check-set-mode! 'report-failed)
(check-display "*** testing Vicare low level fixnum operations\n")


(parametrise ((check-test-name	'fixnums))

  (check
      (fixnum? ($fxinthash 123))
    => #t)

  #t)


(parametrise ((check-test-name	'flonums))

  (check
      (flonum? ($make-flonum))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
