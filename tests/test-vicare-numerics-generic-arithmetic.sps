;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for generic mathematics functions
;;;Date: Sat Nov 17, 2012
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare generic mathematics functions\n")


;;;; helpers

(define epsilon 1e-5)

(define (quasi=? x y)
  (< (magnitude (- x y)) epsilon))


(parametrise ((check-test-name	'sqrt))

  (check
      (sqrt 5)
    (=> quasi=?) 2.23606797749979)

  (check
      (sqrt -5)
    (=> quasi=?) 0.0+2.23606797749979i)

  (check
      (sqrt +inf.0)
    => +inf.0)

  (check
      (sqrt -inf.0)
    => +inf.0i)

  #t)


(parametrise ((check-test-name	'exact-integer-sqrt))

  (check
      (let-values (((S R) (exact-integer-sqrt 0)))
	(list S R))
    => '(0 0))

  (check
      (let-values (((S R) (exact-integer-sqrt 4)))
	(list S R))
    => '(2 0))

  (check
      (let-values (((S R) (exact-integer-sqrt 5)))
	(list S R))
    => '(2 1))

  #t)


;;;; done

(check-report)

;;; end of file
