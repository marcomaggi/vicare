;;;
;;;Part of: Vicare Scheme
;;;Contents: BBS random number generator
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;	Blum-Blum-Shub is a PRNG proposed in:
;;;
;;;	   Lenore   Blum,  Manuel   Blum,  Michael   Shub.    "A  Simple
;;;	   Unpredictable Pseudo--Random  Number Generator", SIAM Journal
;;;	   on Computing, volume 15, page 364-383, May 1986.
;;;
;;;	when  seeded   with  prime  numbers   satisfying  the  specified
;;;	requirements,  it can be  considered a  cryptographically secure
;;;	PRNG.   The search  of  such  prime numbers  is  a delicate  and
;;;	complex task; we have to turn to specialised literature to learn
;;;	how to do it.
;;;
;;;	   This implementation is derived from:
;;;
;;;		<http://it.wikipedia.org/wiki/Blum_Blum_Shub>
;;;		<http://en.wikipedia.org/wiki/Blum_Blum_Shub>
;;;
;;;	   This library  makes no attempt  to validate the  seed numbers
;;;	for the cryptographic requirements,  it just implements the PRNG
;;;	algorithm; attaining  cryptographic security is  entirely on our
;;;	shoulders.
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare crypto randomisations blum-blum-shub)
  (export make-random-source/blum-blum-shub)
  (import (vicare)
    (vicare crypto randomisations)
    (vicare crypto randomisations low))


(define (make-random-source/blum-blum-shub)
  (let* ((P #f) (Q #f) (PQ #f) (X #f))

    (define c 0)
    (define M const:2^32)

    (define external-state-tag 'random-source-state/blum-blum-shub)

    (define (make-random-bits)
      (set! c (+ 1 c))
      (let* ((N 0)
	     (Xn (do ((i 0 (+ 1 i))
		      (Xn X (mod (* Xn Xn) PQ)))
		     ((= i 32)
		      Xn)
		   (set! N (bitwise-copy-bit N i (mod (bitwise-bit-count Xn) 2))))))
	(set! X Xn)
	N))

    (define make-random-32bits make-random-bits)

    (define (seed! integers-maker)
      (let* ((P? (integers-maker const:2^32))
	     (Q? (integers-maker const:2^32))
	     (PQ?  (* P? Q?))
	     (PQ-1 (- PQ? 1))
	     (S   (do ((S (integers-maker PQ-1) (integers-maker PQ-1)))
		      ((= 1 (gcd S PQ?))
		       S))))
	(set! P  P?)
	(set! Q  Q?)
	(set! PQ PQ?)
	(set! X  (mod (* S S) PQ))))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag P Q PQ X))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source FIB state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 5 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((P0  (check-it 1))
	    (Q0  (check-it 2))
	    (PQ0 (check-it 3))
	    (X0  (check-it 4)))
	(set! P  P0)
	(set! Q  Q0)
	(set! PQ PQ0)
	(set! X  X0)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     +inf.0			    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U M make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real M make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real M make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


;;;; done

)

;;; end of file
