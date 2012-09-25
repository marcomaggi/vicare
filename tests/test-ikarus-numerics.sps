;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests  from  the  file  "scheme/tests/numerics.ss" file  in  the
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

(define (test-rounding)
  (define (test-round x)
    (let ([rx (round x)])
      (unless (integer? rx)
	(error 'test-round "not an integer result for" x rx))
      (let ([diff (abs (- (abs x) (abs rx)))])
	(cond
	 [(= diff 1/2)
	  (unless (even? rx)
	    (error 'test-round "non-even rounding for" x rx))]
	 [else
	  (unless (< diff 1/2)
	    (error 'test-round "rounding the wrong way for" x rx))]))))
  (test-round -251/100)
  (test-round -250/100)
  (test-round -249/100)
  (test-round +251/100)
  (test-round +250/100)
  (test-round +249/100)

  (test-round -151/100)
  (test-round -150/100)
  (test-round -149/100)
  (test-round +151/100)
  (test-round +150/100)
  (test-round +149/100)

  (test-round -351/100)
  (test-round -350/100)
  (test-round -349/100)
  (test-round +351/100)
  (test-round +350/100)
  (test-round +349/100)

  (test-round +0.00)
  (test-round -0.00)
  (test-round +0.49)
  (test-round -0.49)
  (test-round +1.49)
  (test-round -1.49)
  (test-round +2.49)
  (test-round -2.49)
  (test-round +3.49)
  (test-round -3.49)
  (test-round +0.51)
  (test-round -0.51)
  (test-round +1.51)
  (test-round -1.51)
  (test-round +2.51)
  (test-round -2.51)
  (test-round +3.51)
  (test-round -3.51)
  (test-round +0.50)
  (test-round -0.50)
  (test-round +1.50)
  (test-round -1.50)
  (test-round +2.50)
  (test-round -2.50)
  (test-round +3.50)
  (test-round -3.50)



  )

(define (test-eqv)
  (define (test-eqv x y result)
    (unless (eqv? (eqv? x y) result)
      (error 'test-eqv "failed" x y result)))
  (test-eqv 0 0 #t)
  (test-eqv 0.0 0 #f)
  (test-eqv 0 0.0 #f)
  (test-eqv 0.0 0.0 #t)
  (test-eqv 0.0 -0.0 #f)
  (test-eqv -0.0 0.0 #f)
  (test-eqv -0.0 -0.0 #t)
    ;;; ikarus extensions, not guaranteed by R6RS
  (test-eqv +nan.0 +nan.0 #t)
  (test-eqv (/ +inf.0 +inf.0) (/ -inf.0 +inf.0) #t)
  (test-eqv +nan.0 (string->number "+nan.0") #t))

(define (test-exact-integer-sqrt)
  (define (f i j inc)
    (when (< i j)
      (let-values ([(s r) (exact-integer-sqrt i)])
	(unless (and (= (+ (* s s) r) i)
		     (< i (* (+ s 1) (+ s 1))))
	  (error 'exact-integer-sqrt "wrong result" i))
	(f (+ i inc) j inc))))
  (f 0 10000 1)
  (f 0 536870911 10000)
  (f 0 536870911000 536870911))


(define (run-tests)
  (test-rounding)
  (test-exact-integer-sqrt)
  (test-eqv))

(display "*** testing Ikarus numerics\n\n" (current-error-port))
(run-tests)
(display "; *** done\n" (current-error-port))

;;; end of file
