;;;
;;;Part of: Vicare Scheme
;;;Contents: random number generator
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;	This is  a combined  multiple recursive generator.  The sequence
;;;	is,
;;;
;;;	   z_n = (x_n - y_n) mod m1
;;;
;;;	where the two underlying generators x and y are,
;;;
;;;	   x_n = (a_{1} x_{n-1} + a_{2} x_{n-2} + a_{3} x_{n-3}) mod m1
;;;	   y_n = (b_{1} y_{n-1} + b_{2} y_{n-2} + b_{3} y_{n-3}) mod m2
;;;
;;;	with coefficients a11 ... a23,
;;;
;;;		a_{1} = 0,     a_{2} = 63308, a_{3} = -183326
;;;		b_{1} = 86098, b_{2} = 0,     b_{3} = -539608
;;;
;;;	and moduli m1, m2,
;;;
;;;		m1 = 2^31 - 1 = 2147483647
;;;		m2 = 2^31 - 2000169 = 2145483479
;;;
;;;     NOTE: According to the paper the initial values for x_n must lie
;;;     in the range 0  <= x_n < m1 and the initial  values for y_n must
;;;     lie  in the range  0 <=  y_n <  m2, with  at least  one non-zero
;;;     value.
;;;
;;;     The period of this generator is about 2^205.
;;;
;;;     From: P.   L'Ecuyer, "Combined Multiple  Recursive Random Number
;;;     Generators," Operations Research, 44, 5 (1996), 816--822.
;;;
;;;     This is available on the net from L'Ecuyer's home page,
;;;
;;;       <http://www.iro.umontreal.ca/~lecuyer/myftp/papers/combmrg.ps>
;;;     <ftp://ftp.iro.umontreal.ca/pub/simulation/lecuyer/papers/combmrg.ps>
;;;
;;;	The Scheme code  was converted by Marco Maggi  from a C language
;;;	version from the GNU Scientific Library release 1.12.
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 James Theiler, Brian
;;;Gough
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
(library (vicare crypto randomisations cmrg)
  (export make-random-source/cmrg)
  (import (rnrs)
    (vicare crypto randomisations)
    (vicare crypto randomisations low))


(define (make-random-source/cmrg)
  (let ((X1 7) (X2 17) (X3 47)
	(Y1 3) (Y2 13) (Y3 43))

    (define external-state-tag 'random-source-state/cmrg)
    (define M1 2147483647) ; M1 = 2^31 - 1
    (define M2 2145483479) ; M2 = 2^31 - 2000169

    (define A2	  63308) (define QA2 33921) (define RA2 12979)
    (define A3  -183326) (define QA3 11714) (define RA3  2883)
    (define B1    86098) (define QB1 24919) (define RB1  7417)
    (define B3  -539608) (define QB3  3976) (define RB3  2071)

    (define (make-random-bits)
      ;; component 1
      (let* ((H3 (div X3 QA3))
	     (P3 (- (* (- A3) (- X3 (* H3 QA3))) (* H3 RA3)))
		;original: p3 = -a3 * (x3 - h3 * qa3) - h3 * ra3
	     (H2 (div X2 QA2))
	     (P2 (- (*    A2  (- X2 (* H2 QA2))) (* H2 RA2))))
		;originsl: p2 = a2 * (x2 - h2 * qa2) - h2 * ra2
	(when (negative? P3)
	  (set! P3 (+ P3 M1)))
	(when (negative? P2)
	  (set! P2 (+ P2 M1)))
	;;Right-shift the vector X, purging X3.
	(set! X3 X2)
	(set! X2 X1)
	(set! X1 (- P2 P3))
	(when (negative? X1)
	  (set! X1 (+ X1 M1))))
      ;; component 2
      (let* ((H3 (div Y3 QB3))
	     (P3 (- (* (- B3) (- Y3 (* H3 QB3))) (* H3 RB3)))
		;original: p3 = -b3 * (y3 - h3 * qb3) - h3 * rb3

	     (H1 (div Y1 QB1))
	     (P1 (- (* B1 (- Y1 (* H1 QB1))) (* H1 RB1))))
		;original: p1 = b1 * (y1 - h1 * qb1) - h1 * rb1
	(when (negative? P3)
	  (set! P3 (+ P3 M2)))
	(when (negative? P1)
	  (set! P1 (+ P1 M2)))
	;;Right-shift the vector Y, purging Y3.
	(set! Y3 Y2)
	(set! Y2 Y1)
	(set! Y1 (- P1 P3))
	(when (negative? Y1)
	  (set! Y1 (+ Y1 M2))))
      (let ((N (- X1 Y1)))
	(if (< X1 Y1)
	    (+ N M1)
	  N)))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M1 make-random-bits))

    (define (seed! integers-maker)
      (set! X1 (integers-maker M1)) (set! X2 (integers-maker M1)) (set! X3 (integers-maker M1))
      (set! Y1 (integers-maker M2)) (set! Y2 (integers-maker M2)) (set! Y3 (integers-maker M2)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag X1 X2 X3 Y1 Y2 Y3))

    (define (external-state->internal-state external-state)
      (define (check-value idx M)
	(let ((S (vector-ref external-state idx)))
	  (if (and (integer? S) (exact? S) (<= 0 S (- M 1)))
	      S
	    (assertion-violation 'external-state->internal-state
	      "illegal random source CMRG state value" S external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 7 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((s1 (check-value 1 M1))
	    (s2 (check-value 2 M1))
	    (s3 (check-value 3 M1))
	    (r1 (check-value 4 M2))
	    (r2 (check-value 5 M2))
	    (r3 (check-value 6 M2)))
	(when (or (zero? (+ s1 s2 s3))
		  (zero? (+ r1 r2 r3)))
	  (assertion-violation 'external-state->internal-state
	    "illegal random source CMRG degenerate state" external-state))
	(set! X1 s1) (set! X2 s2) (set! X3 s3)
	(set! Y1 r1) (set! Y2 r2) (set! Y3 r3)))

    ;;Initialisation.
    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     6				    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U M1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real M1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real M1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


;;;; done

)

;;; end of file
