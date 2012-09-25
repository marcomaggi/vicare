;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the round function
;;;Date: Thu Nov 24, 2011
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


(import (ikarus)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing round function\n")


;;;; code

(check
    (list (number->string (round (string->number "0.5")))
   (string->number "0.5")
   (round (string->number "0.5")))
  => '("0.0" 0.5 0.0))

(check
    (list (number->string (round (string->number "1.5")))
   (string->number "1.5")
   (round (string->number "1.5")))
  => '("2.0" 1.5 2.0))

(check
    (list (number->string (round (string->number "2.5")))
   (string->number "2.5")
   (round (string->number "2.5")))
  => '("2.0" 2.5 2.0))

(check
    (list (number->string (round (string->number "3.5")))
   (string->number "3.5")
   (round (string->number "3.5")))
  => '("4.0" 3.5 4.0))

(check
    (list (number->string (round (string->number "4.5")))
   (string->number "4.5")
   (round (string->number "4.5")))
  => '("4.0" 4.5 4.0))

(check
    (list (number->string (round (string->number "5.5")))
   (string->number "5.5")
   (round (string->number "5.5")))
  => '("6.0" 5.5 6.0))

(check
    (list (number->string (round (string->number "6.5")))
   (string->number "6.5")
   (round (string->number "6.5")))
  => '("6.0" 6.5 6.0))

(check
    (list (number->string (round (string->number "7.5")))
   (string->number "7.5")
   (round (string->number "7.5")))
  => '("8.0" 7.5 8.0))

(check
    (list (number->string (round (string->number "8.5")))
   (string->number "8.5")
   (round (string->number "8.5")))
  => '("8.0" 8.5 8.0))

(check
    (list (number->string (round (string->number "9.5")))
   (string->number "9.5")
   (round (string->number "9.5")))
  => '("1e1" 9.5 10.0))


;;;; done

(check-report)

;;; end of file
