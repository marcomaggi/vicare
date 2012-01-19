;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for bignums
;;;Date: Thu Jan 19, 2012
;;;
;;;Abstract
;;;
;;;	Test for fixnums and bignums on 32-bit platforms.
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
(import (rename (vicare)
		(parameterize	parametrise))
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare bignums\n")

(define greatest-positive-fx
  (greatest-fixnum))

(define least-negative-fx
  (least-fixnum))

(define least-positive-bn
  (+ 1 greatest-positive-fx))

(define greatest-negative-bn
  (- least-negative-fx 1))


(parametrise ((check-test-name	'inspection))

  (check (even? (* 2 least-positive-bn))	=> #t)
  (check (even? (+ 1 (* 2 least-positive-bn)))	=> #f)

  #t)


(parametrise ((check-test-name	'arithmetics))

  (check
      (- (+ greatest-positive-fx greatest-positive-fx) greatest-positive-fx)
    => greatest-positive-fx)

  (check
      (- greatest-positive-fx (+ greatest-positive-fx greatest-positive-fx))
    => (- greatest-positive-fx))

;;; --------------------------------------------------------------------

  (check
      (- greatest-positive-fx least-positive-bn)
    => -1)

  (check
      (- least-positive-bn greatest-positive-fx)
    => +1)

  ;; (check-pretty-print (list greatest-positive-fx '+ greatest-negative-bn
  ;; 			    '= (+ greatest-positive-fx greatest-negative-bn)))
  (check
      (+ greatest-positive-fx greatest-negative-bn)
    => -2)

  (check
      (+ least-negative-fx least-positive-bn)
    => 0)

  (check
      (+ least-negative-fx (+ 10 least-positive-bn))
    => 10)

  (check
      (+ least-negative-fx (+ least-positive-bn least-positive-bn))
    => least-positive-bn)

  (check
      (+ 1 (+ -1 greatest-negative-bn))
    => greatest-negative-bn)

  (check
      (+ (- least-negative-fx) (+ least-negative-fx greatest-negative-bn))
    => greatest-negative-bn)

;;; --------------------------------------------------------------------

  (check
      (- (+ greatest-positive-fx least-positive-bn) greatest-positive-fx)
    => least-positive-bn)

  (check
      (- (+ greatest-positive-fx least-positive-bn) least-positive-bn)
    => greatest-positive-fx)

  #t)


;;;; done

(check-report)

;;; end of file
