;;;
;;;Part of: Vicare Scheme
;;;Contents: random number generator
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;	This generator is taken from
;;;
;;;	   Donald E. Knuth.  "The Art of Computer Programming".
;;;	   Volume 2.  Third Edition.  Addison-Wesley.  Page 106-108.
;;;
;;;	It  is called  "Borosh  - Niederreiter".   The  Scheme code  was
;;;	converted by Marco Maggi from  a C language version from the GNU
;;;	Scientific Library release 1.12.
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2003 Heiko Bauke.
;;;Copyright (c) 2001 Carlo Perassi
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
(library (vicare crypto randomisations borosh)
  (export make-random-source/borosh)
  (import (rnrs)
    (vicare crypto randomisations)
    (vicare crypto randomisations low))


(define (make-random-source/borosh)
  (let ((N 1))

    (define external-state-tag 'random-source-state/borosh)
    (define A 1812433253)

    ;; Numbers are generated in the range 0 <= N < M.
    (define M   const:2^32)
    (define M-1 const:2^32-1)

    (define (make-random-bits)
      (set! N (mod (* A N) M))
      N)

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (set! N (integers-maker M)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag N))

    (define (external-state->internal-state external-state)
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 2 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((Y (vector-ref external-state 1)))
	(if (and (integer? Y) (exact? Y) (<= 0 Y M-1))
	    (set! N Y)
	  (assertion-violation 'external-state->internal-state
	    "illegal random source Borosh state" external-state))))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     1				    ; required seed values
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
