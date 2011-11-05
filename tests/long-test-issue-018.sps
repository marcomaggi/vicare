;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for issue 18
;;;Date: Thu Oct 21, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (ikarus)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing issue 18\n")
(set-port-buffer-mode! (current-output-port) (buffer-mode none))


(check (string->number "1-1")		=> #f)
(check (string->number "1.2-3.4")	=> #f)
(check (string->number "1/2-3/4")	=> #f)


(let ()

;;; assume reader which loads this file can only read signed integers.

  (define (run-tests)
    (test-string-to-number)
    (generated-tests))

  (define (test string expected)
    (define (equal-results? x y)
      (define (== x y)
        (cond
	 [(nan? x) (nan? y)]
	 [(zero? x) (and (= x y) (= (atan 0.0 x) (atan 0.0 y)))]
	 [else
	  (and (or (and (exact? x) (exact? y))
		   (and (inexact? x) (inexact? y)))
	       (= x y))]))
      (cond
       [(and (number? x) (number? y))
	(and (== (real-part x) (real-part y))
	     (== (imag-part x) (imag-part y)))]
       [else (equal? x y)]))

    (let ([result (string->number string)])
      (if expected
          (unless (number? result)
            (error 'test "did not parse as number" string))
	(when result
	  (error test "incorrectly parse as non-#f" string)))
      (unless (equal-results? result expected)
        (error 'test "failed/expected/got" string expected result))
      (when expected
        (let ([s1 (call-with-string-output-port
		      (lambda (port)
			(write result port)))])
          (unless (string=? s1 string)
            (test s1 expected))))))

  (define inf+ (fl/ (inexact 1) (inexact 0)))
  (define inf- (fl/ (inexact -1) (inexact 0)))


  (define (test-string-to-number)
    (test "10" 10)
    (test "1" 1)
    (test "-17" -17)
    (test "12" 12)
    (test "+12" +12)
    (test "-12" -12)
    (test "+13476238746782364786237846872346782364876238477" 13476238746782364786237846872346782364876238477)
    (test "+inf.0" inf+)
    (test "-inf.0" inf-)
    (test "+i" (make-rectangular 0 +1))
    (test "-i" (make-rectangular 0 -1))
    (test "+15i" (make-rectangular 0 +15))
    (test "-15i" (make-rectangular 0 -15))
    (test "12/7" (/ 12 7))
    (test "-12/7" (/ -12 7))
    (test "+12/7" (/ 12 7))
    (test "12/7i" (make-rectangular 0 (/ 12 7)))
    (test "-12/7i" (make-rectangular 0 (/ -12 7)))
    (test "+12/7i" (make-rectangular 0 (/ 12 7)))
    (test "12/7+7i" (make-rectangular (/ 12 7) (/ 7 1)))
    (test "12/7+7/5i" (make-rectangular (/ 12 7) (/ 7 5)))
    (test "12/7-7/5i" (make-rectangular (/ 12 7) (/ -7 5)))
    (test "12." (inexact 12))
    (test "#e12." 12)
    (test "12.5" (inexact (/ 125 10)))
    (test "#e12.5123" (/ 125123 10000))
    (test "#i125123/10000" (inexact (/ 125123 10000)))
    (test "+inf.0i" (make-rectangular 0 inf+))
    (test "-inf.0i" (make-rectangular 0 inf-))

    (test "1/2" (/ 1 2))
    (test "-1/2" (/ 1 -2))
    (test "#x24" 36)
    (test "#x-24" -36)
    (test "#b+00000110110" 54)
    (test "#b-00000110110/10" -27)
    (test "#e10" 10)
    (test "#e1" 1)
    (test "#e-17" -17)
    (test "#e#x24" 36)
    (test "#e#x-24" -36)
    (test "#e#b+00000110110" 54)
    (test "#e#b-00000110110/10" -27)
    (test "#x#e24" 36)
    (test "#x#e-24" -36)
    (test "#b#e+00000110110" 54)
    (test "#b#e-00000110110/10" -27)
    (test "#e1e1000" (expt 10 1000))
    (test "#e-1e1000" (- (expt 10 1000)))
    (test "#e1e-1000" (expt 10 -1000))
    (test "#e-1e-1000" (- (expt 10 -1000)))
    (test "#i1e100" (inexact (expt 10 100)))
    (test "#i1e1000" (inexact (expt 10 1000)))
    (test "#i-1e1000" (inexact (- (expt 10 1000))))
    (test "1e100" (inexact (expt 10 100)))
    (test "1.0e100" (inexact (expt 10 100)))
    (test "1.e100" (inexact (expt 10 100)))
    (test "0.1e100" (inexact (expt 10 99)))
    (test ".1e100" (inexact (expt 10 99)))
    (test "+1e100" (inexact (expt 10 100)))
    (test "+1.0e100" (inexact (expt 10 100)))
    (test "+1.e100" (inexact (expt 10 100)))
    (test "+0.1e100" (inexact (expt 10 99)))
    (test "+.1e100" (inexact (expt 10 99)))
    (test "-1e100" (inexact   (- (expt 10 100))))
    (test "-1.0e100" (inexact (- (expt 10 100))))
    (test "-1.e100" (inexact  (- (expt 10 100))))
    (test "-0.1e100" (inexact (- (expt 10 99))))
    (test "-.1e100" (inexact  (- (expt 10 99))))

    (test "i" #f)
    (test "/" #f)
    (test "12/0" #f)
    (test "+12/0" #f)
    (test "-12/0" #f)
    (test "12/0000" #f)
    (test "+12/0000" #f)
    (test "-12/0000" #f)
    (test "12+" #f)
    (test "+12+" #f)
    (test "-12+" #f)
    (test "12+" #f)
    (test "+12+" #f)
    (test "-12+" #f)

    (test "8+6.0i" (make-rectangular 8 6.0))
    (test "8.0+6i" (make-rectangular 8.0 6))
    (test "+8+6.0i" (make-rectangular 8 6.0))
    (test "+8.0+6i" (make-rectangular 8.0 6))
    (test "-8+6.0i" (make-rectangular -8 6.0))
    (test "-8.0+6i" (make-rectangular -8.0 6))

    (test "8-6.0i" (make-rectangular 8 -6.0))
    (test "8.0-6i" (make-rectangular 8.0 -6))
    (test "+8-6.0i" (make-rectangular 8 -6.0))
    (test "+8.0-6i" (make-rectangular 8.0 -6))
    (test "-8-6.0i" (make-rectangular -8 -6.0))
    (test "-8.0-6i" (make-rectangular -8.0 -6))

    (test "0i" 0)
    (test "+0i" 0)
    (test "-0i" 0)

    (test "1i" (make-rectangular 0 1))
    (test "+1i" (make-rectangular 0 1))
    (test "-1i" (make-rectangular 0 -1))

    (test "8+nan.0i" (make-rectangular 8 +nan.0))
    (test "8.0+nan.0i" (make-rectangular 8.0 +nan.0))
    (test "+8+nan.0i" (make-rectangular 8 +nan.0))
    (test "+8.0+nan.0i" (make-rectangular 8.0 +nan.0))
    (test "-8+nan.0i" (make-rectangular -8 +nan.0))
    (test "-8.0+nan.0i" (make-rectangular -8.0 +nan.0))
    (test "8-nan.0i" (make-rectangular 8 -nan.0))
    (test "8.0-nan.0i" (make-rectangular 8.0 -nan.0))
    (test "+8-nan.0i" (make-rectangular 8 -nan.0))
    (test "+8.0-nan.0i" (make-rectangular 8.0 -nan.0))
    (test "-8-nan.0i" (make-rectangular -8 -nan.0))
    (test "-8.0-nan.0i" (make-rectangular -8.0 -nan.0))
    (test "+nan.0+6.0i" (make-rectangular +nan.0 6.0))
    (test "+nan.0+6i" (make-rectangular +nan.0 6))
    (test "+nan.0+6.0i" (make-rectangular +nan.0 6.0))
    (test "+nan.0+6i" (make-rectangular +nan.0 6))
    (test "-nan.0+6.0i" (make-rectangular -nan.0 6.0))
    (test "-nan.0+6i" (make-rectangular -nan.0 6))
    (test "+nan.0-6.0i" (make-rectangular +nan.0 -6.0))
    (test "+nan.0-6i" (make-rectangular +nan.0 -6))
    (test "+nan.0-6.0i" (make-rectangular +nan.0 -6.0))
    (test "+nan.0-6i" (make-rectangular +nan.0 -6))
    (test "-nan.0-6.0i" (make-rectangular -nan.0 -6.0))
    (test "-nan.0-6i" (make-rectangular -nan.0 -6))
    (test "+nan.0+nan.0i" (make-rectangular +nan.0 +nan.0))
    (test "+nan.0-nan.0i" (make-rectangular +nan.0 -nan.0))
    (test "-nan.0+nan.0i" (make-rectangular -nan.0 +nan.0))
    (test "-nan.0-nan.0i" (make-rectangular -nan.0 -nan.0))

    (test "+nan.0+i" (make-rectangular +nan.0 +1))
    (test "+nan.0-i" (make-rectangular +nan.0 -1))
    (test "-nan.0+i" (make-rectangular -nan.0 +1))
    (test "-nan.0-i" (make-rectangular -nan.0 -1))

    (test "8+inf.0i" (make-rectangular 8 +inf.0))
    (test "8.0+inf.0i" (make-rectangular 8.0 +inf.0))
    (test "+8+inf.0i" (make-rectangular 8 +inf.0))
    (test "+8.0+inf.0i" (make-rectangular 8.0 +inf.0))
    (test "-8+inf.0i" (make-rectangular -8 +inf.0))
    (test "-8.0+inf.0i" (make-rectangular -8.0 +inf.0))
    (test "8-inf.0i" (make-rectangular 8 -inf.0))
    (test "8.0-inf.0i" (make-rectangular 8.0 -inf.0))
    (test "+8-inf.0i" (make-rectangular 8 -inf.0))
    (test "+8.0-inf.0i" (make-rectangular 8.0 -inf.0))
    (test "-8-inf.0i" (make-rectangular -8 -inf.0))
    (test "-8.0-inf.0i" (make-rectangular -8.0 -inf.0))
    (test "+inf.0+6.0i" (make-rectangular +inf.0 6.0))
    (test "+inf.0+6i" (make-rectangular +inf.0 6))
    (test "+inf.0+6.0i" (make-rectangular +inf.0 6.0))
    (test "+inf.0+6i" (make-rectangular +inf.0 6))
    (test "-inf.0+6.0i" (make-rectangular -inf.0 6.0))
    (test "-inf.0+6i" (make-rectangular -inf.0 6))
    (test "+inf.0-6.0i" (make-rectangular +inf.0 -6.0))
    (test "+inf.0-6i" (make-rectangular +inf.0 -6))
    (test "+inf.0-6.0i" (make-rectangular +inf.0 -6.0))
    (test "+inf.0-6i" (make-rectangular +inf.0 -6))
    (test "-inf.0-6.0i" (make-rectangular -inf.0 -6.0))
    (test "-inf.0-6i" (make-rectangular -inf.0 -6))
    (test "+inf.0+inf.0i" (make-rectangular +inf.0 +inf.0))
    (test "+inf.0-inf.0i" (make-rectangular +inf.0 -inf.0))
    (test "-inf.0+inf.0i" (make-rectangular -inf.0 +inf.0))
    (test "-inf.0-inf.0i" (make-rectangular -inf.0 -inf.0))

    (test "+inf.0+i" (make-rectangular +inf.0 +1))
    (test "+inf.0-i" (make-rectangular +inf.0 -1))
    (test "-inf.0+i" (make-rectangular -inf.0 +1))
    (test "-inf.0-i" (make-rectangular -inf.0 -1))

    (test "8+6e20i" (make-rectangular +8 +6e20))
    (test "8-6e20i" (make-rectangular +8 -6e20))
    (test "8e20+6i" (make-rectangular +8e20 +6))
    (test "8e20-6i" (make-rectangular +8e20 -6))
    (test "+8+6e20i" (make-rectangular +8 +6e20))
    (test "+8-6e20i" (make-rectangular +8 -6e20))
    (test "+8e20+6i" (make-rectangular +8e20 +6))
    (test "+8e20-6i" (make-rectangular +8e20 -6))
    (test "-8+6e20i" (make-rectangular -8 +6e20))
    (test "-8-6e20i" (make-rectangular -8 -6e20))
    (test "-8e20+6i" (make-rectangular -8e20 +6))
    (test "-8e20-6i" (make-rectangular -8e20 -6))

    (test "8e10+6e20i" (make-rectangular +8e10 +6e20))
    (test "8e10-6e20i" (make-rectangular +8e10 -6e20))
    (test "+8e10+6e20i" (make-rectangular +8e10 +6e20))
    (test "+8e10-6e20i" (make-rectangular +8e10 -6e20))
    (test "-8e10+6e20i" (make-rectangular -8e10 +6e20))
    (test "-8e10-6e20i" (make-rectangular -8e10 -6e20))

    (test "-0e-10" -0.0)
    (test "-0e-0" -0.0)
    (test "#d-0e-10-0e-0i" (make-rectangular -0.0 -0.0))
    (test "-0.i" (make-rectangular 0.0 -0.0))
    (test "#d#e-0.0f-0-.0s-0i" 0)

    (test "+.234e4i" (make-rectangular 0 0.234e4))
    (test "+.234e-5i" (make-rectangular 0 0.234e-5))
    (test "+.234i" (make-rectangular 0 0.234))
    )

  (define (generated-tests)

    (define (gen ls1 ls2 comp1 comp2)
      (apply append
	     (map (lambda (x1)
		    (map (lambda (x2)
			   (cons (comp1 (car x1) (car x2))
				 (comp2 (cdr x1) (cdr x2))))
		      ls2))
	       ls1)))

    (define (gensa ls1 ls2 comp)
      (gen ls1 ls2 string-append comp))

    (define suffixed-int
      '(["0" . 0]
        ["1" . 1]
        ["1." . 1.0]
        ["1.0" . 1.0]
        [".5" . 0.5]
        ["0.5" . 0.5]))

    (define exponents
      '(["e0" . 1.0]
        ["e+0" . 1.0]
        ["e-0" . 1.0]
        ["e-1" . 0.1]))

    (define decimal10
      (append
       suffixed-int
       (gensa suffixed-int exponents *)))

    (define naninf
      '(["nan.0" . +nan.0]
        ["inf.0" . +inf.0]))

    (define ureal
      (append
       decimal10
       (gensa decimal10 '(["|53" . #f]) (lambda (x _) (inexact x)))))

    (define sign
      '(["+" . +1]
        ["-" . -1]))

;;; <real> = <sign> <ureal>
;;;        | + <naninf>
;;;        | - <naninf>

    (define sreal
      (append
       (gensa sign ureal *)
       (gensa sign naninf *)))

    (define real
      (append ureal sreal))

;;;<complex> = <real>
;;;          | <real> @ <real>
;;;          | <real> <creal>
;;;          | <creal>
;;; <creal> = <seal> i
;;;         | +i
;;;         | -i


    (define comps
      (append
       (gensa sreal '(["i" . #f]) (lambda (x f) x))
       '(["+i" . 1]
	 ["-i" . -1])))

    (define creal
      (map (lambda (x) (cons (car x) (make-rectangular 0 (cdr x)))) comps))

    (define complex
      (append
       real creal
       (gensa real comps make-rectangular)
       (gen real real (lambda (x y) (string-append x "@" y)) make-polar)
       ))

    (display (string-append "TESTING " (number->string (length complex)) " tests\n"))
    (for-each
	(lambda (x)
	  (test (car x) (cdr x)))
      complex)

    )


  (run-tests))


;;;; done

(check-report)

;;; end of file
