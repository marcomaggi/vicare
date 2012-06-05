;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for issue 22, missing single operand / for complexes
;;;Date: Tue Jun 14, 2011
;;;
;;;Abstract
;;;
;;;	$ vicare
;;;	(/ 1.1+2.2i)
;;;	    Unhandled exception
;;;	    Condition components:
;;;	    1. &assertion
;;;	    2. &who: /
;;;	    3. &message: "not a number"
;;;	    4. &irritants: (1.1+2.2i)
;;;
;;;Copyright (c) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 22, missing one-operand division operation for flonum complex numbers\n")


(define (quasi=? a b)
  (< (magnitude (- a b)) 1e-6))

(check
    (/ 1.1+2.2i)
  (=> quasi=?) 0.1818181818181818-0.3636363636363636i)

(check
    (/ 1+2i)
  (=> quasi=?) 1/5-2/5i)

(check
    (/ 2.2)
  (=> quasi=?) 0.45454545454545453)


;;;; done

(check-report)

;;; end of file
