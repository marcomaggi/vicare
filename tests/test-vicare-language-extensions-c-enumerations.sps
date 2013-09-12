;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for enumerations utilities
;;;Date: Tue Dec 29, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (vicare)
  (vicare language-extensions c-enumerations)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: C language enumerations\n")


(parametrise ((check-test-name	'constants))

  (define A 1)
  (define B 2)
  (define C 3)

  (define-c-flags things
    (A B C)
    (a b c))

  (check (things->value (things a)) => 1)
  (check (things->value (things b)) => 2)
  (check (things->value (things c)) => 3)

  (check (value->things 1) (=> enum-set=?) (things a))
  (check (value->things 2) (=> enum-set=?) (things b))
  (check (value->things 3) (=> enum-set=?) (things c))

  #t)


(parametrise ((check-test-name	'flags))

  (define A (bitwise-arithmetic-shift-left 1 0))
  (define B (bitwise-arithmetic-shift-left 1 1))
  (define C (bitwise-arithmetic-shift-left 1 2))

  (define-c-ior-flags things
    (A B C)
    (a b c))

  (check (things->value (things a))      => 1)
  (check (things->value (things b))      => 2)
  (check (things->value (things a b))    => 3)
  (check (things->value (things c))      => 4)
  (check (things->value (things a c))    => 5)
  (check (things->value (things b c))    => 6)
  (check (things->value (things a b c))  => 7)

  (check (value->things 1) (=> enum-set=?) (things a))
  (check (value->things 2) (=> enum-set=?) (things b))
  (check (value->things 3) (=> enum-set=?) (things a b))
  (check (value->things 4) (=> enum-set=?) (things c))
  (check (value->things 5) (=> enum-set=?) (things a c))
  (check (value->things 6) (=> enum-set=?) (things b c))
  (check (value->things 7) (=> enum-set=?) (things a b c))

  (check (value->things 10) (=> enum-set=?) (things b))

  #t)


;;;; done

(check-report)

;;; end of file
