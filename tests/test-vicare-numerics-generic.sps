;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for numeric functions
;;;Date: Sat Aug  3, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(check-display "*** testing Vicare numerics functions: generic stuff\n")

(define greatest-positive-fx
  (greatest-fixnum))

(define least-negative-fx
  (least-fixnum))

(define least-positive-bn
  (+ 1 greatest-positive-fx))

(define greatest-negative-bn
  (- least-negative-fx 1))


(parametrise ((check-test-name	'positivity))

  (check (positive? least-positive-bn)		=> #t)
  (check (negative? least-positive-bn)		=> #f)
  (check (positive? greatest-negative-bn)	=> #f)
  (check (negative? greatest-negative-bn)	=> #t)

  #t)


(parametrise ((check-test-name	'non-positivity))

  (check (non-positive? 0)		=> #t)
  (check (non-negative? 0)		=> #t)

  (check (non-positive? +123)		=> #f)
  (check (non-positive? -123)		=> #t)
  (check (non-negative? +123)		=> #t)
  (check (non-negative? -123)		=> #f)

  (check (non-positive? +0.0)		=> #f)
  (check (non-positive? -0.0)		=> #t)
  (check (non-negative? +0.0)		=> #t)
  (check (non-negative? -0.0)		=> #f)

  (check (non-positive? +123.0)		=> #f)
  (check (non-positive? -123.0)		=> #t)
  (check (non-negative? +123.0)		=> #t)
  (check (non-negative? -123.0)		=> #f)

  (check (non-positive? +1/2)		=> #f)
  (check (non-positive? -1/2)		=> #t)

  (check (non-negative? +1/2)		=> #t)
  (check (non-negative? -1/2)		=> #f)

  (check (non-positive? least-positive-bn)	=> #f)
  (check (non-negative? least-positive-bn)	=> #t)

  (check (non-positive? greatest-negative-bn)	=> #t)
  (check (non-negative? greatest-negative-bn)	=> #f)

  #t)


;;;; done

(check-report)

;;; end of file
