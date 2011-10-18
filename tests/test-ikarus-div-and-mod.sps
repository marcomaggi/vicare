;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests  from the file  "scheme/tests/div-and-mod.ss" file  in the
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
  (test-/)
  (test-div-and-mod)
  (test-div0-and-mod0))


(define (test-/)
  (assert (= (/ 536870912 1/4) 2147483648)))

(define (test-div-and-mod)
  (define (test x1 x2)
    (let-values ([(d m) (div-and-mod x1 x2)])
      (printf "(div-and-mod ~s ~s) = ~s ~s\n" x1 x2 d m)
      (assert (= d (div x1 x2)))
      (assert (= m (mod x1 x2)))
      (assert (<= 0 m))
      (assert (< m (abs x2)))
      (assert (= x1 (+ (* d x2) m)))))

  (test +17 +3)
  (test +17 -3)
  (test -17 +3)
  (test -17 -3)
  (test +16 +3)
  (test +16 -3)
  (test -16 +3)
  (test -16 -3)
  (test +15 +3)
  (test +15 -3)
  (test -15 +3)
  (test -15 -3)
  (test +10 +4)
  (test +10 -4)
  (test -10 +4)
  (test -10 -4)

  (test +3 +5/6)
  (test -3 +5/6)
  (test +3 -5/6)
  (test -3 -5/6)

  (test +3 +7/11)
  (test -3 +7/11)
  (test +3 -7/11)
  (test -3 -7/11)

  (test (least-fixnum) -1)
  (test (least-fixnum) 1)
  (test (greatest-fixnum) -1)
  (test (greatest-fixnum) 1)


  )


(define (test-div0-and-mod0)
  (define (test x1 x2)
    (let-values ([(d m) (div0-and-mod0 x1 x2)])
      (printf "(div0-and-mod0 ~s ~s) = ~s ~s\n" x1 x2 d m)
      (assert (= d (div0 x1 x2)))
      (assert (= m (mod0 x1 x2)))
      (assert (<= (- (abs (/ x2 2))) m))
      (assert (< m (abs (/ x2 2))))
      (assert (= x1 (+ (* d x2) m)))))
  (test +17 +3)
  (test +17 -3)
  (test -17 +3)
  (test -17 -3)
  (test +16 +3)
  (test +16 -3)
  (test -16 +3)
  (test -16 -3)
  (test +15 +3)
  (test +15 -3)
  (test -15 +3)
  (test -15 -3)
  (test +10 +4)
  (test +10 -4)
  (test -10 +4)
  (test -10 -4)

  (test +3 +5/6)
  (test -3 +5/6)
  (test +3 -5/6)
  (test -3 -5/6)

  (test +3 +7/11)
  (test -3 +7/11)
  (test +3 -7/11)
  (test -3 -7/11)


  (test (least-fixnum)    +1)
  (test (least-fixnum)    -1) ;; overflows
  (test (greatest-fixnum) +1)
  (test (greatest-fixnum) -1)
  (test (least-fixnum)    +2)
  (test (least-fixnum)    -2)
  (test (greatest-fixnum) +2)
  (test (greatest-fixnum) -2)

  (test 0 (least-fixnum))
  (test 0 (greatest-fixnum))
  (test +1 (least-fixnum))
  (test +1 (greatest-fixnum))
  (test -1 (least-fixnum))
  (test -1 (greatest-fixnum))
  (test +2 (least-fixnum))
  (test +2 (greatest-fixnum))
  (test -2 (least-fixnum))
  (test -2 (greatest-fixnum))

  (test (least-fixnum) (least-fixnum))
  (test (greatest-fixnum) (least-fixnum))
  (test (least-fixnum) (greatest-fixnum))
  (test (greatest-fixnum) (greatest-fixnum)))

(display "*** testing div and mod\n" (current-error-port))
(flush-output-port (current-error-port))
(run-tests)
(display "; *** done\n" (current-error-port))
(flush-output-port (current-error-port))

;;; end of file
