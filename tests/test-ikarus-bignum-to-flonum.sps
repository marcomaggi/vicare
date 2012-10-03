;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests
;;;Date: Tue Oct 18, 2011
;;;
;;;Abstract
;;;
;;;	Tests from  the file "scheme/tests/bignum-to-flonum.ss"  file in
;;;	the original Ikarus distribution.
;;;
;;;Copyright (C) 2006-2010 Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (ikarus-test-framework)
  (vicare checks))

(define (%printf . args)
  (when #f
    (apply printf args)))

(define (run-tests)
  (test-bignum-to-flonum)
  (test-bignum->flonum))

(define (t x s)
  (let ([fl (format "~a" (exact->inexact x))])
    (unless (string=? s fl)
      (error 'bignum->flonum
	"incorrect result for ~s\n expected ~a, \n      got ~a" x s fl))))

(define-syntax test*
  (syntax-rules ()
    [(_ name [num str] ...)
     (define-tests name
       [(lambda (x) (string=? str (number->string x))) (exact->inexact num)]
       ...)]))

(define (testnum x)
  (define precision 53)
;;;(assert (bignum? x))
  (let ([fl (inexact x)])
    (let ([n (if (> x 0) x (- x))])
      (let ([bits (bitwise-length n)])
	(%printf "bits(~s) = ~s\n" n bits)
	(cond
	 [(<= bits precision)
	  (unless (= x (exact fl))
	    (error #f "should be exactly equal" x fl (exact fl)))]
	 [else
	  (let ([hi53 (sra n (- bits precision))]
		[lo (bitwise-and n (- (sll 1 (- bits precision)) 1))]
		[breakpoint (sll 1 (- bits precision 1))])
	    (assert (= n (+ lo (sll hi53 (- bits precision)))))
	    (let ([fl2
		   (cond
		    [(or (< lo breakpoint)
			 (and (= lo breakpoint) (even? hi53)))
		     (* (inexact hi53) (sll 1 (- bits precision)))]
		    [else
		     (* (inexact (+ hi53 1)) (sll 1 (- bits precision)))])])
	      (let ([fl2 (if (> x 0) fl2 (* fl2 -1))])
		(%printf "x=~s fl=~s\n" x fl)
		(unless (fl=? fl fl2)
		  (error #f "should be equal" x fl fl2)))))])))))

(define (test-pos-neg x)
  (testnum x)
  (testnum (- x)))

(test* test-bignum-to-flonum
       (1000000000  "1e9")
       (2000000000  "2e9")
       (4000000000  "4e9")
       (-1000000000 "-1e9")
       (-2000000000 "-2e9")
       (-4000000000 "-4e9")
       ( 6000000000   "6e9")
       (12000000000 "1.2e10")
       (25000000000 "2.5e10")
       (50000000000   "5e10")
       (-6000000000  "-6e9")
       (-12000000000 "-1.2e10")
       (-25000000000 "-2.5e10")
       (-50000000000 "-5e10")
       (100000000000 "1e11")
       (200000000000 "2e11")
       (400000000000 "4e11")
       (800000000000 "8e11")
       (-100000000000 "-1e11")
       (-200000000000 "-2e11")
       (-400000000000 "-4e11")
       (-800000000000 "-8e11")
       (#x1FFFFFFFFFFFFF   "9.007199254740991e15")
       (#x3FFFFFFFFFFFFF   "1.8014398509481984e16")
       (#x7FFFFFFFFFFFFF   "3.602879701896397e16")
       (#xFFFFFFFFFFFFFF   "7.205759403792794e16")
       (#x1FFFFFFFFFFFFFF  "1.4411518807585587e17")
       (#x3FFFFFFFFFFFFFF  "2.8823037615171174e17")
       (#x7FFFFFFFFFFFFFF  "5.764607523034235e17")
       (#xFFFFFFFFFFFFFFF  "1.152921504606847e18")
       (#xFFFFFFFFFFFFFFFF "1.8446744073709552e19")
       (1000000000000000000000 "1e21")
       (100000000000000000000000000000 "1e29")
       (100000000000000000000000000000000000000000000000 "1e47")
       (-1000000000000000000000 "-1e21")
       (-100000000000000000000000000000 "-1e29")
       (-100000000000000000000000000000000000000000000000 "-1e47"))

(define (test-bignum->flonum)
  (test-pos-neg 34872389478)
  (test-pos-neg 34872389479)
  (test-pos-neg 3487238948347878)
  (test-pos-neg 3487238948347879)
  (test-pos-neg 5487238948347878)
  (test-pos-neg 5487238948347879)
  (test-pos-neg 543877238948347878)
  (test-pos-neg 543877238948347879)
  (test-pos-neg 5438748878948347878)
  (test-pos-neg 5438748878948347879)
  (test-pos-neg 13874887238948347878)
  (test-pos-neg 13874887238948347879)
  (test-pos-neg 543874887238948347878)
  (test-pos-neg 543874887238948347879)
  (test-pos-neg 5433847834874887238948347878)
  (test-pos-neg 5433847834874887238948347879)
  (test-pos-neg 329847892374892374895433847834874887238948347878)
  (test-pos-neg 329847892374892374895433847834874887238948347879)
  (test-pos-neg
   13407807929942598588139732355608757972494524375225679733981068131349151486565474898751136354405850399729303719974268319295398132445078977825297784408899585)
  (test-pos-neg
   13407807929942598588139732355608757972494524375225679733981068131349151486565474898751136354405850399729303719974268319295398132445078977825297784408899584)
  (test-pos-neg
   13407807929942598588139732355608757972494524375225679733981068131349151486565474898751136354405850399729303719974268319295398132445078977825297784408899586)
  (test-pos-neg
   1340780792994259858813973235560875797249452437522567973398106813134915148656547489875113635440585039972930371997426831929539813244507897782529778440889958413407807929942598588139732355608757972494524375225679733981068131349151486565474898751136354405850399729303719974268319295398132445078977825297784408899584))

(set-port-buffer-mode! (current-output-port) (buffer-mode none))
(check-display "*** testing Ikarus bignum to flonum\n")
(run-tests)
(check-display "; *** done\n\n")

;;; end of file
