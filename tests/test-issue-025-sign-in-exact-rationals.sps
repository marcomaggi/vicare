;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for issue 25
;;;Date: Fri Aug 26, 2011
;;;
;;;Abstract
;;;
;;;	Reported by John David Stone as Ikarus bug 831582:
;;;
;;;	I expected to  see (/ -3/8) => -8/3, but 8/-3  is OK even though
;;;	it's  not a valid  R6RS numeric  literal.  But  then (=  8/-3 (/
;;;	-3/8)) should be true.  Other weirdness:
;;;
;;;		(- 8/-3 (/ -3/8)) => 0
;;;		(= -8/3 (/ -3/8)) => #f
;;;		(- -8/3 (/ -3/8)) => 0
;;;		(= -8/3 (/ 1 -3/8)) => #t
;;;		(= (/ -3/8) (/ 1 -3/8)) => #f
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rnrs)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing issue 25, wrong handling of sign in exact rationals\n")


;;;; tests

(check (= -8/3 (/ -3/8))	=> #t)

(check (= -8/3 (/ 1 -3/8))	=> #t)

(check (- -8/3 (/ -3/8))	=> 0)

;;(check (- 8/-3 (/ -3/8))	=> 0)

(check (= (/ -3/8) (/ 1 -3/8))	=> #t)

(check (= -8/3 (/ 1 -3/8))	=> #t)

;;; --------------------------------------------------------------------

(check (/ +8 -3)		=> -8/3)
(check (/ -8 +3)		=> -8/3)
(check (/ -8 -3)		=> +8/3)

;;; --------------------------------------------------------------------

(check (/ +15/4 +12/5)		=> (/ (* +15 +5) (* +4 +12)))
(check (/ +15/4 +12/5)		=> 25/16)

(check (/ -15/4 +12/5)		=> (/ (* -15 +5) (* +4 +12)))
(check (/ -15/4 +12/5)		=> -25/16)

(check (/ +15/4 -12/5)		=> (/ (* +15 +5) (* +4 -12)))
(check (/ +15/4 -12/5)		=> -25/16)


;;;; done

(check-report)

;;; end of file
