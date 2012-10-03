;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for "ikarus.strings.ss"
;;;Date: Thu Sep 29, 2011
;;;
;;;Abstract
;;;
;;;	Some tests  are from the file  "scheme/tests/strings.ss" file in
;;;	the original Ikarus distribution.
;;;
;;;Copyright (C) 2006-2010 Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (ikarus)
  (ikarus-test-framework)
  (vicare checks))

(define-tests test-strings
  [values
   (string-ci=? "Strasse" "Stra\xDF;e")]
		;[(lambda (x) (string=? x "STRASSE"))
		; (string-upcase "Stra\xDF;e")]
		;[(lambda (x) (string=? x "stra\xDF;e"))
		; (string-downcase "Stra\xDF;e")]
  [(lambda (x) (string=? x "strasse"))
   (string-foldcase "Stra\xDF;e")]
		;[(lambda (x) (string=? x "strasse"))
		; (string-downcase "STRASSE")]
  [values (string-ci=? "Stra\xDF;e" "Strasse")]
  [values (string-ci=? "Stra\xDF;e" "STRASSE")]
  [values (string-ci=? "\xDF;" "SS")]
  [values (string-ci=? "\xDF;\xDF;" "SSSS")]
  )

(check-display "*** testing strings\n")
(test-strings)
(check-display "; *** done\n\n")

;;; end of file
