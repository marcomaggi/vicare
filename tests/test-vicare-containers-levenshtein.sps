;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for Levenshtein Distance
;;;Date: Mon May  9, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2004-2009 Neil Van Dyke
;;;Port to R6RS by Marco Maggi
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
;;;For other  licenses and consulting,  please contact the  author (Neil
;;;Van Dyke).
;;;


#!r6rs
(import (vicare)
  (vicare checks)
  (vicare containers levenshtein))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers: Levenshtein Distance\n")


(parametrise ((check-test-name	'basic))

  (check (vector-levenshtein '#(6 6 6) '#(6 35 6 24 6 32))	=> 3)

  (check (list-levenshtein/eq '(b c e x f y) '(a b c d e f))	=> 4)

  (check (string-levenshtein "adresse" "address")	=> 2)

  #t)


(parametrise ((check-test-name	'coerce))

  (check
      (levenshtein/predicate '#(#\A #\B #\C #\D)
			     "aBXcD"
			     char-ci=?)
    => 1)

  (let ((g '#(#\g #\u #\m #\b #\o)))

    (check (levenshtein g "gambol")  => 2)
    (check (levenshtein g "dumbo")   => 1)
    (check (levenshtein g "umbrage") => 5))

;;; --------------------------------------------------------------------

  (check (levenshtein "adresse" "address")	=> 2)
  (check (levenshtein "adresse" "addressee")	=> 2)
  (check (levenshtein "gambol"  "gumbo")	=> 2)
  (check (levenshtein "gumbo"   "gambol")	=> 2)
  (check (levenshtein "gumbo"  "bumble")	=> 3)
  (check (levenshtein "gumbo"  '#(#\b #\u #\m #\b #\l #\e))	=> 3)

;;; --------------------------------------------------------------------

  (check
    (levenshtein '#(#\g #\u #\m #\b #\o)
		 '#(#\b #\u #\m #\b #\l #\e))
    => 3)

  (check
    (levenshtein '(#\g #\u #\m #\b #\o) '#(#\b #\u #\m #\b #\l #\e))
    => 3)

  (check
    (levenshtein '#(#\g #\u #\m #\b #\o) '(#\b #\u #\m #\b #\l #\e))
    => 3)

  (check
    (levenshtein '#(#\g #\u #\m #\b #\o) '(#\b #\u #\m #\b #\l #\e))
    => 3)

;;; --------------------------------------------------------------------

  (check (levenshtein "a"      "abcde")	=> 4)
  (check (levenshtein "abcde"      "a")	=> 4)

;;; --------------------------------------------------------------------

  (check (levenshtein '#(6 6 6) '(1 2 3 4 5 6))       	=> 5)
  (check (levenshtein '#(6 6 6) '(1 2 3 4 5 6 7))     	=> 6)
  (check (levenshtein '#(6 6 6) '(1 2 3 4 5 6 7 6 6))	=> 6)

  #t)


;;;; done

(check-report)

;;; end of file
