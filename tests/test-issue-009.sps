;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 9, modulo errors
;;;Date: Mon Jun  7, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (only (ikarus) fxmodulo)
  (rnrs r5rs)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 9, modulo errors\n")


;;;; code

(check
    (fxmod -9 10)
  => 1)

(check
    (fxmod -10 10)
  => 0)

(check
    (fxmod -150 10)
  => 0)

(check
    (fxmod -30000 10)
  => 0)

(check
    (fxmod 300 10)
  => 0)

(check
    (fxmod -3 1)
  => 0)

;;; --------------------------------------------------------------------

(check
    (fxmodulo -3 1)
  => 0)

(check
    (fxmodulo 13 -4)
  => -3)

(check
    (fxmodulo -13 -4)
  => -1)

;;; --------------------------------------------------------------------

(check
    (modulo -9 10)
  => 1)

(check
    (modulo -10 10)
  => 0)

(check
    (modulo -150 10)
  => 0)

(check
    (modulo -30000 10)
  => 0)

(check
    (modulo 300 10)
  => 0)

(check
    (modulo -3 1)
  => 0)



;;;; done

(check-report)

;;; end of file
