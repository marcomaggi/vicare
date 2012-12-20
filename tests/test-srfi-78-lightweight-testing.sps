;;;
;;;Part of: Vicare Scheme
;;;Contents: test for general SRFI 78
;;;Date: Thu Dec 20, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2005-2006 Sebastian Egner.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;``Software''), to deal in the Software without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission notice  shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE  IS PROVIDED  ``AS IS'', WITHOUT  WARRANTY OF  ANY KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN NO  EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
;;;ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(import (rnrs)
  (rnrs r5rs)
  (srfi :78)
  (srfi :42))

(check-set-mode! 'report-failed)
(display "*** testing SRFI libraries: SRFI 78, lightweight testing\n")
(flush-output-port (current-output-port))


(check (+ 1 1) => 2)
;;;(check (+ 1 1) => 3)

;;; -- different equality predicate --

(check (vector 1) => (vector 1))
;;;(check (vector 1) (=> eq?) (vector 1)) ; fails

;;; -- parametric tests --

(check-ec (+ 1 1) => 2)

(check-ec (: x 10) (+ x 1) => (+ x 1) (x))

(check-ec (: e 100) (positive? (expt 2 e)) => #t (e)) ; fails on fixnums

;;(check-ec (: e 100) (:let x (expt 2.0 e)) (= (+ x 1) x) => #f (x)) ; fails

#;(check-ec (: e 100)
    (:let x (expt 2.0 e))
  (= (+ x 1) x)
  => #f)

(check-ec (: x 10) (: y 10) (: z 10)
          (* x (+ y z)) => (+ (* x y) (* x z))
          (x y z)) ; passes with 10^3 cases checked

;;; -- toy examples --

(define (fib n)
  (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(check (fib 1) => 1)
(check (fib 2) => 1)
(check-ec (: n 1 31) (even? (fib n)) => (= (modulo n 3) 0) (n))


;;;; done

(check-report)
(flush-output-port (current-output-port))

;;; end of file
