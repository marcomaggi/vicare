;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests from the  file "scheme/tests/parse-flonums.ss" file in the
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
(import (ikarus)
  (ikarus-test-framework))

(define file (src-file "rn100"))

(define (read-all)
  (with-input-from-file file
    (lambda ()
      (let f ([ac '()])
	(let ([x (read)])
	  (if (eof-object? x)
	      (reverse ac)
	    (f (cons x ac))))))))

(define (read-flonum)
  (define (decimal x)
    (cond
     [(assv x '([#\0 . 0] [#\1 . 1] [#\2 . 2] [#\3 . 3] [#\4 . 4]
		[#\5 . 5] [#\6 . 6] [#\7 . 7] [#\8 . 8] [#\9 . 9]))
      => cdr]
     [else #f]))
  (define (st)
    (let ([x (read-char)])
      (cond
       [(eof-object? x) x]
       [(char-whitespace? x) (st)]
       [(char=? x #\-) (- (sign))]
       [(decimal x) => num]
       [else (error 'st "invalid char" x)])))
  (define (sign)
    (let ([x (read-char)])
      (cond
       [(eof-object? x) (error 'sign "eof")]
       [(decimal x) => num]
       [else (error 'sign "invalid char" x)])))
  (define (num n)
    (let ([x (read-char)])
      (cond
       [(eof-object? x) (error 'num "eof")]
       [(decimal x) => (lambda (m) (num (+ (* n 10) m)))]
       [(char=? x #\.) (+ n (frac 0 1))]
       [else (error 'num "invalid char" x)])))
  (define (frac num den)
    (let ([x (read-char)])
      (cond
       [(or (eof-object? x) (char-whitespace? x))
	(/ num den)]
       [(decimal x) => (lambda (m)
			 (frac (+ (* num 10) m)
			       (* den 10)))]
       [else (error 'frac "invalid char" x)])))
  (st))

(define smallest-flonum
  (bytevector-ieee-double-ref
   #vu8(1 0 0 0 0 0 0 0)
   0
   'little))

(define (gen-epsilon x)
  (let ([x (flabs x)])
    (let f ([eps smallest-flonum])
      (if (fl=? x (fl- x eps))
	  (f (fl* eps 2.0))
	eps))))

(define (inexact-close-enough? in ex)
    ;;; take the inexact number, and generate two
    ;;; additional numbers: in+epsilon, in-epsilon
    ;;; turn them into exacts: e1=exact(in+epsilon), e2=exact(in-epsilon)
    ;;; ensure that at least e1 < ex < e2
  (let ([eps (gen-epsilon in)])
    (< (exact (fl- in eps)) ex (exact (fl+ in eps)))))

(define (read-exact-all)
  (with-input-from-file file
    (lambda ()
      (let f ([ac '()])
	(let ([x (read-flonum)])
	  (if (eof-object? x)
	      (reverse ac)
	    (f (cons x ac))))))))

(define (run-tests)
  (define who 'test-parse-flonums)
  (define failed #f)
  (define idx 0)
  (let ([ls1 (read-all)]
	[ls2 (read-exact-all)])
    (assert (= (length ls1) (length ls2)))
    (for-each
        (lambda (x1 x2)
          (set! idx (+ idx 1))
          (unless (inexact-close-enough? x1 x2)
            (set! failed #t)
            (printf "test failed in line ~s on read=~s and parsed=~s\n"
                    idx x1 x2)))
      ls1 ls2))
  (when failed (error who "failed"))
  )

(run-tests)

;;; end of file
