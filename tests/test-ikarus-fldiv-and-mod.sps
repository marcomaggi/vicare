;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests from the  file "scheme/tests/fldiv-and-mod.ss" file in the
;;;	original Ikarus distribution.
;;;
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

#!ikarus
(import (ikarus))

(define (run-tests)
  (test-fldiv-and-mod)
  (test-fldiv0-and-mod0))


(define (test-fldiv-and-mod)
  (define (test x1 x2 verify?)
    (let-values ([(d m) (fldiv-and-mod x1 x2)])
      (printf "(fldiv-and-mod ~s ~s) = ~s ~s\n" x1 x2 d m)
      (when verify?
	(assert (= d (fldiv x1 x2)))
	(assert (= m (flmod x1 x2)))
	(assert (<= 0.0 m))
	(assert (< m (abs x2)))
	(assert (= x1 (+ (* d x2) m))))))
  (test +17.0 +3.0 #t)
  (test +17.0 -3.0 #t)
  (test -17.0 +3.0 #t)
  (test -17.0 -3.0 #t)
  (test +16.0 +3.0 #t)
  (test +16.0 -3.0 #t)
  (test -16.0 +3.0 #t)
  (test -16.0 -3.0 #t)
  (test +15.0 +3.0 #t)
  (test +15.0 -3.0 #t)
  (test -15.0 +3.0 #t)
  (test -15.0 -3.0 #t)
  (test +17.0 +3.5 #t)
  (test +17.0 -3.5 #t)
  (test -17.0 +3.5 #t)
  (test -17.0 -3.5 #t)
  (test +16.0 +3.5 #t)
  (test +16.0 -3.5 #t)
  (test -16.0 +3.5 #t)
  (test -16.0 -3.5 #t)
  (test +15.0 +3.5 #t)
  (test +15.0 -3.5 #t)
  (test -15.0 +3.5 #t)
  (test -15.0 -3.5 #t)
  (test +17.0 +nan.0 #f)
  (test -17.0 +nan.0 #f)
  (test +17.0 +inf.0 #f)
  (test +17.0 -inf.0 #f)
  (test -17.0 +inf.0 #f)
  (test -17.0 -inf.0 #f))

(define (test-fldiv0-and-mod0)
  (define (test x1 x2 verify?)
    (let-values ([(d m) (fldiv0-and-mod0 x1 x2)])
      (printf "(fldiv0-and-mod0 ~s ~s) = ~s ~s\n" x1 x2 d m)
      (when verify?
	(assert (= d (fldiv0 x1 x2)))
	(assert (= m (flmod0 x1 x2)))
	(assert (<= (fl- (flabs (fl/ x2 2.0))) m))
	(assert (< m (flabs (fl/ x2 2.0))))
	(assert (= x1 (+ (* d x2) m))))))
  (test +17.0 +3.0 #t)
  (test +17.0 -3.0 #t)
  (test -17.0 +3.0 #t)
  (test -17.0 -3.0 #t)
  (test +16.0 +3.0 #t)
  (test +16.0 -3.0 #t)
  (test -16.0 +3.0 #t)
  (test -16.0 -3.0 #t)
  (test +15.0 +3.0 #t)
  (test +15.0 -3.0 #t)
  (test -15.0 +3.0 #t)
  (test -15.0 -3.0 #t)
  (test +17.0 +3.5 #t)
  (test +17.0 -3.5 #t)
  (test -17.0 +3.5 #t)
  (test -17.0 -3.5 #t)
  (test +16.0 +3.5 #t)
  (test +16.0 -3.5 #t)
  (test -16.0 +3.5 #t)
  (test -16.0 -3.5 #t)
  (test +15.0 +3.5 #t)
  (test +15.0 -3.5 #t)
  (test -15.0 +3.5 #t)
  (test -15.0 -3.5 #t)
  (test +10.0 +4.0 #t)
  (test +10.0 -4.0 #t)
  (test -10.0 +4.0 #t)
  (test -10.0 -4.0 #t)
  (test +17.0 +nan.0 #f)
  (test -17.0 +nan.0 #f)
  (test +17.0 +inf.0 #f)
  (test +17.0 -inf.0 #f)
  (test -17.0 +inf.0 #f)
  (test -17.0 -inf.0 #f))

(run-tests)

;;; end of file
