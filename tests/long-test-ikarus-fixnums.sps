;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests  from  the   file  "scheme/tests/fixnum.ss"  file  in  the
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
  (test-fxdiv-and-mod)
  (test-fxdiv0-and-mod0)
  (test-fxlength)
  (test-fxcarry))


(define (test-fxcarry)
  (define (fx*/carry-reference fx1 fx2 fx3)
    (let* ([s (+ (* fx1 fx2) fx3)]
	   [s0 (mod0 s (expt 2 (fixnum-width)))]
	   [s1 (div0 s (expt 2 (fixnum-width)))])
      (values s0 s1)))
  (define (fx+/carry-reference fx1 fx2 fx3)
    (let* ([s (+ (+ fx1 fx2) fx3)]
	   [s0 (mod0 s (expt 2 (fixnum-width)))]
	   [s1 (div0 s (expt 2 (fixnum-width)))])
      (values s0 s1)))
  (define (fx-/carry-reference fx1 fx2 fx3)
    (let* ([s (- (- fx1 fx2) fx3)]
	   [s0 (mod0 s (expt 2 (fixnum-width)))]
	   [s1 (div0 s (expt 2 (fixnum-width)))])
      (values s0 s1)))
  (define (test name fxop/carry fxop/carry-reference fx1 fx2 fx3)
    (let-values ([(s0 s1) (fxop/carry fx1 fx2 fx3)]
		 [(s2 s3) (fxop/carry-reference fx1 fx2 fx3)])
      (unless (fx= s0 s2)
	(error name "failed (value1) on ~s ~s ~s, got ~s, should be ~s"
	       fx1 fx2 fx3 s0 s2))
      (unless (fx= s1 s3)
	(error name "failed (value2) on ~s ~s ~s, got ~s, should be ~s"
	       fx1 fx2 fx3 s1 s3))))
  (define ls
    (list 0 1 2 -1 -2 38734 -3843 2484598 -348732487 (greatest-fixnum) (least-fixnum)))
  (printf "[~s: test-fxcarry] " (expt (length ls) 3))
  (for-each
      (lambda (fx1)
        (for-each
	    (lambda (fx2)
	      (for-each
		  (lambda (fx3)
		    (test 'fx*/carry fx*/carry fx*/carry-reference fx1 fx2 fx3)
		    (test 'fx+/carry fx+/carry fx+/carry-reference fx1 fx2 fx3)
		    (test 'fx-/carry fx-/carry fx-/carry-reference fx1 fx2 fx3))
		ls))
          ls))
    ls)
  )


(define (test-fxdiv-and-mod)
  (define (test x1 x2)
    (let-values ([(d m) (fxdiv-and-mod x1 x2)])
      (printf "(fxdiv-and-mod ~s ~s) = ~s ~s\n" x1 x2 d m)
      (assert (= d (fxdiv x1 x2)))
      (assert (= m (fxmod x1 x2)))
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
  (test -10 -4))


(define (test-fxdiv0-and-mod0)
  (define (test x1 x2)
    (let-values ([(d m) (fxdiv0-and-mod0 x1 x2)])
      (printf "(fxdiv0-and-mod0 ~s ~s) = ~s ~s\n" x1 x2 d m)
      (assert (= d (fxdiv0 x1 x2)))
      (assert (= m (fxmod0 x1 x2)))
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

  (test (least-fixnum)    +1)
		;(test (least-fixnum)    -1) ;; overflows
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


(define (test-fxlength)
  (define (test x)
    (define (bitlen x)
      (if (zero? x)
	  0
	(+ 1 (bitlen (bitwise-arithmetic-shift-right x 1)))))
    (define (len x)
      (if (< x 0)
	  (bitlen (bitwise-not x))
	(bitlen x)))
    (let ([c0 (len x)]
	  [c1 (fxlength x)])
      (unless (= c0 c1)
	(error 'test-fxlength "failed/expected/got" x c0 c1))))
  (define (test-fx count n inc)
    (when (fixnum? n)
      (when (zero? (fxlogand count #xFFFF))
	(printf "bitwise-bit-count ~s\n" n))
      (test n)
      (test-fx (+ count 1) (+ n inc) inc)))
  (test 0)
  (test 1)
  (test 2)
  (test 3)
  (test -1)
  (test -2)
  (test -3)
  (if (= (fixnum-width) 30)
      (test-fx 0 (least-fixnum) #xFF)
    (test-fx 0 (least-fixnum) #xFF00000000)) )

(display "*** testing fixnums\n" (current-error-port))
(flush-output-port (current-error-port))
(run-tests)
(display "; *** done\n" (current-error-port))
(flush-output-port (current-error-port))

;;; end of file
