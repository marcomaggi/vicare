;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for flonum functions
;;;Date: Sat Oct 20, 2012
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
  (ikarus system $flonums)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare flonum functions\n")


(parametrise ((check-test-name	'debug))

  (when #f
    (check-pretty-print (flonum->bytevector 123.456)))

  (check
      (bytevector->flonum (flonum->bytevector 123.456))
    => 123.456)

;;; --------------------------------------------------------------------

  (let* ((flo		123.456)
	 (octet0	($flonum-u8-ref flo 0))
	 (octet1	($flonum-u8-ref flo 1))
	 (octet2	($flonum-u8-ref flo 2))
	 (octet3	($flonum-u8-ref flo 3))
	 (octet4	($flonum-u8-ref flo 4))
	 (octet5	($flonum-u8-ref flo 5))
	 (octet6	($flonum-u8-ref flo 6))
	 (octet7	($flonum-u8-ref flo 7)))
    (check
	(let* ((bv	(flonum->bytevector flo))
	       (u0	(bytevector-u8-ref bv (+ 8 0)))
	       (u1	(bytevector-u8-ref bv (+ 8 1)))
	       (u2	(bytevector-u8-ref bv (+ 8 2)))
	       (u3	(bytevector-u8-ref bv (+ 8 3)))
	       (u4	(bytevector-u8-ref bv (+ 8 4)))
	       (u5	(bytevector-u8-ref bv (+ 8 5)))
	       (u6	(bytevector-u8-ref bv (+ 8 6)))
	       (u7	(bytevector-u8-ref bv (+ 8 7))))
	  (reverse (list u0 u1 u2 u3 u4 u5 u6 u7)))
      => (list octet0 octet1 octet2 octet3 octet4 octet5 octet6 octet7)))

  #t)


;;;; done

(check-report)

;;; end of file
