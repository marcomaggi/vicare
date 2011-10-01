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
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006-2010 Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
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
(import (rename (ikarus)
		(parameterize	parametrise))
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Ikarus bytevector functions\n")


(parametrise ((check-test-name	'latin1))

  (define test-string
    (let* ((str.len 256)
	   (str     (make-string str.len)))
      (do ((i 0 (+ 1 i)))
	  ((= i str.len)
	   str)
	(string-set! str i (integer->char i)))))

  (define test-bytevector
    (let* ((bv.len 256)
	   (bv     (make-bytevector bv.len)))
      (do ((i 0 (+ 1 i)))
	  ((= i bv.len)
	   bv)
	(bytevector-u8-set! bv i i))))

;;; --------------------------------------------------------------------
;;; argument check

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(string->latin1 123))
    => '(123))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(latin1->string 123))
    => '(123))

;;; --------------------------------------------------------------------

  (check
      (string->latin1 test-string)
    => test-bytevector)

  (check
      (latin1->string test-bytevector)
    => test-string)

  #t)


;;;; done

(check-report)

;;; end of file
