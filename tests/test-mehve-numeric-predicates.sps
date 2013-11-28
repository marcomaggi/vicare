;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for mehve numeric predicates
;;;Date: Sat Jun 18, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa mehve)
  (prefix (nausicaa mehve numeric-predicates) mehve.)
  (prefix (rnrs) rnrs.)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing mehve language, numeric predicates\n")


(parametrise ((check-test-name	'equality))

  (define-method (mehve.equal-predicate-1 (o <vector>))
    (o.for-all =))

  (define-method (mehve.equal-predicate-2 (a <vector>) (b <vector>))
    (a.for-all = b))

  (define-method (mehve.equal-predicate-1 (o <list>))
    (o.for-all =))

  (define-method (mehve.equal-predicate-2 (a <list>) (b <list>))
    (a.for-all = b))

;;; --------------------------------------------------------------------

  (check (=)			=> #t)

  (check (= 1)			=> #t)

  (check (= 1 1)		=> #t)
  (check (= 1 2)		=> #f)
  (check (= 1.1 1.1)		=> #t)
  (check (= 1.1 2.2)		=> #f)
  (check (= 1.1+2.2i 1.1+2.2i)	=> #t)
  (check (= 1.1+2.2i 1.1+3.2i)	=> #f)

  (check (= 1 1 1)		=> #t)
  (check (= 1 2 3)		=> #f)
  (check (= 1 1 2)		=> #f)

;;; --------------------------------------------------------------------

  (check (apply = '())			=> #t)

  (check (apply = '(1))			=> #t)

  (check (apply = 1 '(1))		=> #t)
  (check (apply = '(1 2))		=> #f)
  (check (apply = 1 '(1 1))		=> #t)
  (check (apply = '(1 1 2))		=> #f)
  (check (apply = 1 '(1 1 1))		=> #t)
  (check (apply = '(1 1 1 2))		=> #f)
  (check (apply = 1 '(1 1 1 1))		=> #t)
  (check (apply = '(1 1 1 1 2))		=> #f)

;;; --------------------------------------------------------------------

  (check (= '(1 2 3))				=> #t)
  (check (= '(1 2 3) '(1 2 3))			=> #t)
  (check (= '(1 2 3) '(1 4 3))			=> #f)
  (check (= '(1 2 3) '(1 2 3) '(1 2 3))		=> #t)
  (check (= '(1 2 3) '(1 2 3) '(1 2 4))		=> #f)

  (check (= '#(1 2 3))				=> #t)
  (check (= '#(1 2 3) '#(1 2 3))		=> #t)
  (check (= '#(1 2 3) '#(1 4 3))		=> #f)
  (check (= '#(1 2 3) '#(1 2 3) '#(1 2 3))	=> #t)

  #t)


(parametrise ((check-test-name	'less-than))

  (define-method (mehve.less-than-predicate-1 (o <vector>))
    (o.for-all <))

  (define-method (mehve.less-than-predicate-2 (a <vector>) (b <vector>))
    (a.for-all < b))

  (define-method (mehve.less-than-predicate-1 (o <list>))
    (o.for-all <))

  (define-method (mehve.less-than-predicate-2 (a <list>) (b <list>))
    (a.for-all < b))

;;; --------------------------------------------------------------------

  (check (<)			=> #f)

  (check (< 1)			=> #f)

  (check (< 1 1)		=> #f)
  (check (< 1 2)		=> #t)
  (check (< 2 1)		=> #f)
  (check (< 1.1 1.1)		=> #f)
  (check (< 1.1 2.2)		=> #t)
  (check (< 2.2 1.1)		=> #f)
  (check (< 1/2 1/2)		=> #f)
  (check (< 1/2 2/3)		=> #t)
  (check (< 2/3 1/2)		=> #f)

  (check (< 1 1 1)		=> #f)
  (check (< 1 2 3)		=> #t)
  (check (< 1 2 1)		=> #f)

;;; --------------------------------------------------------------------

  (check (apply < '())			=> #f)

  (check (apply < '(1))			=> #f)

  (check (apply < 1 '(1))		=> #f)
  (check (apply < '(1 2))		=> #t)
  (check (apply < 1 '(2 1))		=> #f)
  (check (apply < '(1 2 3))		=> #t)
  (check (apply < 1 '(2 3 -1))		=> #f)
  (check (apply < '(1 2 3 4))		=> #t)
  (check (apply < 1 '(2 3 4 -1))	=> #f)
  (check (apply < '(1 2 3 4 5))		=> #t)

;;; --------------------------------------------------------------------

  (check (< '(1 2 3) '(1 2 3))		=> #f)
  (check (< '(1 2 3) '(4 5 6))		=> #t)
  (check (< '(1 5 3) '(4 2 6))		=> #f)

  (check (< '#(1 2 3) '#(1 2 3))	=> #f)
  (check (< '#(1 2 3) '#(4 5 6))	=> #t)
  (check (< '#(1 5 3) '#(4 2 6))	=> #f)

  #t)


(parametrise ((check-test-name	'greater-than))

  (define-method (mehve.greater-than-predicate-1 (o <vector>))
    (o.for-all >))

  (define-method (mehve.greater-than-predicate-2 (a <vector>) (b <vector>))
    (a.for-all > b))

  (define-method (mehve.greater-than-predicate-1 (o <list>))
    (o.for-all >))

  (define-method (mehve.greater-than-predicate-2 (a <list>) (b <list>))
    (a.for-all > b))

;;; --------------------------------------------------------------------

  (check (>)			=> #f)

  (check (> 1)			=> #f)

  (check (> 1 1)		=> #f)
  (check (> 1 2)		=> #f)
  (check (> 2 1)		=> #t)
  (check (> 1.1 1.1)		=> #f)
  (check (> 1.1 2.2)		=> #f)
  (check (> 2.2 1.1)		=> #t)
  (check (> 1/2 1/2)		=> #f)
  (check (> 1/2 2/3)		=> #f)
  (check (> 2/3 1/2)		=> #t)

  (check (> 1 1 1)		=> #f)
  (check (> 3 2 1)		=> #t)
  (check (> 2 1 2)		=> #f)

;;; --------------------------------------------------------------------

  (check (apply > '())			=> #f)

  (check (apply > '(1))			=> #f)

  (check (apply > 1 '(1))		=> #f)
  (check (apply > '(2 1))		=> #t)
  (check (apply > 2 '(1 2))		=> #f)
  (check (apply > '(3 2 1))		=> #t)
  (check (apply > 3 '(2 1 4))		=> #f)
  (check (apply > '(4 3 2 1))		=> #t)
  (check (apply > 4 '(3 2 1 5))		=> #f)
  (check (apply > '(5 4 3 2 1))		=> #t)

;;; --------------------------------------------------------------------

  (check (> '(1 2 3) '(1 2 3))		=> #f)
  (check (> '(4 5 6) '(1 2 3))		=> #t)
  (check (> '(4 2 6) '(1 5 3))		=> #f)

  (check (> '#(10 20 3) '#(1 2 3))	=> #f)
  (check (> '#(4 5 6) '#(1 2 3))	=> #t)
  (check (> '#(4 2 6) '#(1 5 3))	=> #f)

  #t)


(parametrise ((check-test-name	'less-or-equal))

  (define-method (mehve.less-than-or-equal-to-predicate-1 (o <vector>))
    (o.for-all <=))

  (define-method (mehve.less-than-or-equal-to-predicate-2 (a <vector>) (b <vector>))
    (a.for-all <= b))

  (define-method (mehve.less-than-or-equal-to-predicate-1 (o <list>))
    (o.for-all <=))

  (define-method (mehve.less-than-or-equal-to-predicate-2 (a <list>) (b <list>))
    (a.for-all <= b))

;;; --------------------------------------------------------------------

  (check (<=)			=> #t)

  (check (<= 1)			=> #t)

  (check (<= 1 1)		=> #t)
  (check (<= 1 2)		=> #t)
  (check (<= 2 1)		=> #f)
  (check (<= 1.1 1.1)		=> #t)
  (check (<= 1.1 2.2)		=> #t)
  (check (<= 2.2 1.1)		=> #f)
  (check (<= 1/2 1/2)		=> #t)
  (check (<= 1/2 2/3)		=> #t)
  (check (<= 2/3 1/2)		=> #f)

  (check (<= 1 1 1)		=> #t)
  (check (<= 1 2 3)		=> #t)
  (check (<= 1 2 1)		=> #f)

;;; --------------------------------------------------------------------

  (check (apply <= '())			=> #t)

  (check (apply <= '(1))		=> #t)

  (check (apply <= 1 '(1))		=> #t)
  (check (apply <= '(1 2))		=> #t)
  (check (apply <= '(2 1))		=> #f)

  (check (apply <= 1 '(2 1))		=> #f)
  (check (apply <= '(1 1 1))		=> #t)
  (check (apply <= '(1 2 3))		=> #t)

  (check (apply <= 1 '(2 3 -1))		=> #f)
  (check (apply <= '(1 1 1 1))		=> #t)
  (check (apply <= '(1 2 3 4))		=> #t)

  (check (apply <= 1 '(2 3 4 -1))	=> #f)
  (check (apply <= '(1 1 1 1 1))	=> #t)
  (check (apply <= '(1 2 3 4 5))	=> #t)

;;; --------------------------------------------------------------------

  (check (<= '(1 2 3) '(1 2 3))		=> #t)
  (check (<= '(1 2 3) '(4 5 6))		=> #t)
  (check (<= '(1 5 3) '(4 2 6))		=> #f)

  (check (<= '#(1 2 3) '#(1 2 3))	=> #t)
  (check (<= '#(1 2 3) '#(4 5 6))	=> #t)
  (check (<= '#(1 5 3) '#(4 2 6))	=> #f)

  #t)


(parametrise ((check-test-name	'greater-or-equal))

  (define-method (mehve.greater-than-or-equal-to-predicate-1 (o <vector>))
    (o.for-all >=))

  (define-method (mehve.greater-than-or-equal-to-predicate-2 (a <vector>) (b <vector>))
    (a.for-all >= b))

  (define-method (mehve.greater-than-or-equal-to-predicate-1 (o <list>))
    (o.for-all >=))

  (define-method (mehve.greater-than-or-equal-to-predicate-2 (a <list>) (b <list>))
    (a.for-all >= b))

;;; --------------------------------------------------------------------

  (check (>=)			=> #t)

  (check (>= 1)			=> #t)

  (check (>= 1 1)		=> #t)
  (check (>= 1 2)		=> #f)
  (check (>= 2 1)		=> #t)
  (check (>= 1.1 1.1)		=> #t)
  (check (>= 1.1 2.2)		=> #f)
  (check (>= 2.2 1.1)		=> #t)
  (check (>= 1/2 1/2)		=> #t)
  (check (>= 1/2 2/3)		=> #f)
  (check (>= 2/3 1/2)		=> #t)

  (check (>= 1 1 1)		=> #t)
  (check (>= 3 2 1)		=> #t)
  (check (>= 2 1 2)		=> #f)

;;; --------------------------------------------------------------------

  (check (apply >= '())			=> #t)

  (check (apply >= '(1))		=> #t)

  (check (apply >= 1 '(1))		=> #t)
  (check (apply >= '(2 1))		=> #t)
  (check (apply >= 2 '(1 2))		=> #f)
  (check (apply >= '(3 2 1))		=> #t)
  (check (apply >= 3 '(2 1 4))		=> #f)
  (check (apply >= '(4 3 2 1))		=> #t)
  (check (apply >= 4 '(3 2 1 5))	=> #f)
  (check (apply >= '(5 4 3 2 1))	=> #t)

;;; --------------------------------------------------------------------

  (check (>= '(1 2 3) '(1 2 3))		=> #t)
  (check (>= '(4 5 6) '(1 2 3))		=> #t)
  (check (>= '(4 2 6) '(1 5 3))		=> #f)

  (check (>= '#(1 2 3) '#(1 2 3))	=> #t)
  (check (>= '#(4 5 6) '#(1 2 3))	=> #t)
  (check (>= '#(4 2 6) '#(1 5 3))	=> #f)

  #t)


(parametrise ((check-test-name	'sign))

  (define-method (mehve.zero? (o <vector>))
    (o.for-all zero?))

  (define-method (mehve.zero? (o <list>))
    (o.for-all zero?))

  (define-method (mehve.positive? (o <vector>))
    (o.for-all positive?))

  (define-method (mehve.positive? (o <list>))
    (o.for-all positive?))

  (define-method (mehve.negative? (o <vector>))
    (o.for-all negative?))

  (define-method (mehve.negative? (o <list>))
    (o.for-all negative?))

  (define-method (mehve.non-positive? (o <vector>))
    (o.for-all non-positive?))

  (define-method (mehve.non-positive? (o <list>))
    (o.for-all non-positive?))

  (define-method (mehve.non-negative? (o <vector>))
    (o.for-all non-negative?))

  (define-method (mehve.non-negative? (o <list>))
    (o.for-all non-negative?))

;;; --------------------------------------------------------------------

  (check (zero? 0)			=> #t)
  (check (zero? 1)			=> #f)
  (check (zero? +0.0)			=> #t)
  (check (zero? -0.0)			=> #t)
  (check (zero? +1.0)			=> #f)
  (check (zero? +0.0+0.0i)		=> #t)
  (check (zero? +0.0-0.0i)		=> #t)
  (check (zero? -0.0+0.0i)		=> #t)
  (check (zero? -0.0-0.0i)		=> #t)
  (check (zero? 1.1+2.2i)		=> #f)
  (check (zero? '(0 0 0))		=> #t)
  (check (zero? '(0 0 1))		=> #f)
  (check (zero? '#(0 0 0))		=> #t)
  (check (zero? '#(0 0 1))		=> #f)

  (check (positive? 0)			=> #f)
  (check (positive? +1)			=> #t)
  (check (positive? -1)			=> #f)
  (check (positive? +0.0)		=> #f)
  (check (positive? -0.0)		=> #f)
  (check (positive? +1.0)		=> #t)
  (check (positive? -1.0)		=> #f)
  (check (positive? '(1 2 3))		=> #t)
  (check (positive? '(1 2 0))		=> #f)
  (check (positive? '#(1 2 3))		=> #t)
  (check (positive? '#(1 2 0))		=> #f)

  (check (negative? 0)			=> #f)
  (check (negative? +1)			=> #f)
  (check (negative? -1)			=> #t)
  (check (negative? +0.0)		=> #f)
  (check (negative? -0.0)		=> #f)
  (check (negative? +1.0)		=> #f)
  (check (negative? -1.0)		=> #t)
  (check (negative? '(-1 -2 -3))	=> #t)
  (check (negative? '(-1 -2 0))		=> #f)
  (check (negative? '#(-1 -2 -3))	=> #t)
  (check (negative? '#(-1 -2 0))	=> #f)

  (check (non-negative? 0)		=> #t)
  (check (non-negative? +1)		=> #t)
  (check (non-negative? -1)		=> #f)
  (check (non-negative? +0.0)		=> #t)
  (check (non-negative? -0.0)		=> #t)
  (check (non-negative? +1.0)		=> #t)
  (check (non-negative? -1.0)		=> #f)
  (check (non-negative? '(1 2 3))	=> #t)
  (check (non-negative? '(1 2 0))	=> #t)
  (check (non-negative? '#(1 2 3))	=> #t)
  (check (non-negative? '#(1 2 0))	=> #t)

  (check (non-positive? 0)		=> #t)
  (check (non-positive? +1)		=> #f)
  (check (non-positive? -1)		=> #t)
  (check (non-positive? +0.0)		=> #t)
  (check (non-positive? -0.0)		=> #t)
  (check (non-positive? +1.0)		=> #f)
  (check (non-positive? -1.0)		=> #t)
  (check (non-positive? '(-1 -2 -3))	=> #t)
  (check (non-positive? '(-1 -2 0))	=> #t)
  (check (non-positive? '#(-1 -2 -3))	=> #t)
  (check (non-positive? '#(-1 -2 0))	=> #t)

  #t)


(parametrise ((check-test-name	'oddness))

  (define-method (mehve.odd? (o <vector>))
    (o.for-all odd?))

  (define-method (mehve.odd? (o <list>))
    (o.for-all odd?))

  (define-method (mehve.even? (o <vector>))
    (o.for-all even?))

  (define-method (mehve.even? (o <list>))
    (o.for-all even?))

;;; --------------------------------------------------------------------

  (check (odd? 1)		=> #t)
  (check (odd? 1.0)		=> #t)
  (check (odd? 2)		=> #f)
  (check (odd? 2.0)		=> #f)
  (check (odd? '(1 3 5))	=> #t)
  (check (odd? '(1 3 6))	=> #f)
  (check (odd? '#(1 3 5))	=> #t)
  (check (odd? '#(1 3 6))	=> #f)

  (check (even? 1)		=> #f)
  (check (even? 1.0)		=> #f)
  (check (even? 2)		=> #t)
  (check (even? 2.0)		=> #t)
  (check (even? '(2 4 5))	=> #f)
  (check (even? '(2 4 6))	=> #t)
  (check (even? '#(2 4 5))	=> #f)
  (check (even? '#(2 4 6))	=> #t)

  #t)


(parametrise ((check-test-name	'non-rational))

  (define-method (mehve.finite? (o <vector>))
    (o.for-all finite?))

  (define-method (mehve.finite? (o <list>))
    (o.for-all finite?))

  (define-method (mehve.infinite? (o <vector>))
    (o.exists infinite?))

  (define-method (mehve.infinite? (o <list>))
    (o.exists infinite?))

  (define-method (mehve.nan? (o <vector>))
    (o.exists nan?))

  (define-method (mehve.nan? (o <list>))
    (o.exists nan?))

;;; --------------------------------------------------------------------

  (check (finite? 1)			=> #t)
  (check (finite? 1.1)			=> #t)
  (check (finite? 1.1+2.2i)		=> #t)
  (check (finite? +inf.0)		=> #f)
  (check (finite? -inf.0)		=> #f)
  (check (finite? 1.1+inf.0i)		=> #f)
  (check (finite? 1.1-inf.0i)		=> #f)
  (check (finite? +inf.0+2.2i)		=> #f)
  (check (finite? -inf.0+2.2i)		=> #f)
  (check (finite? +nan.0)		=> #f)
  (check (finite? '(1 2 3))		=> #t)
  (check (finite? '(1 2 +inf.0))	=> #f)
  (check (finite? '#(1 2 3))		=> #t)
  (check (finite? '#(1 2 +inf.0))	=> #f)

  (check (infinite? 1)			=> #f)
  (check (infinite? 1.1)		=> #f)
  (check (infinite? 1.1+2.2i)		=> #f)
  (check (infinite? +inf.0)		=> #t)
  (check (infinite? -inf.0)		=> #t)
  (check (infinite? 1.1+inf.0i)		=> #t)
  (check (infinite? 1.1-inf.0i)		=> #t)
  (check (infinite? +inf.0+2.2i)	=> #t)
  (check (infinite? -inf.0+2.2i)	=> #t)
  (check (infinite? +nan.0)		=> #f)
  (check (infinite? '(1 2 3))		=> #f)
  (check (infinite? '(1 2 +inf.0))	=> #t)
  (check (infinite? '#(1 2 3))		=> #f)
  (check (infinite? '#(1 2 +inf.0))	=> #t)

  (check (nan? 1)			=> #f)
  (check (nan? 1.1)			=> #f)
  (check (nan? 1.1+2.2i)		=> #f)
  (check (nan? +inf.0)			=> #f)
  (check (nan? -inf.0)			=> #f)
  (check (nan? 1.1+inf.0i)		=> #f)
  (check (nan? 1.1-inf.0i)		=> #f)
  (check (nan? +inf.0+2.2i)		=> #f)
  (check (nan? -inf.0+2.2i)		=> #f)
  (check (nan? 1.1+nan.0i)		=> #t)
  (check (nan? 1.1-nan.0i)		=> #t)
  (check (nan? +nan.0+2.2i)		=> #t)
  (check (nan? -nan.0+2.2i)		=> #t)
  (check (nan? +nan.0)			=> #t)
  (check (nan? '(1 2 3))		=> #f)
  (check (nan? '(1 2 +nan.0))		=> #t)
  (check (nan? '#(1 2 3))		=> #f)
  (check (nan? '#(1 2 +nan.0))		=> #t)

  #t)


;;;; done

(check-report)

;;; end of file
