;;;
;;;Part of: Vicare Scheme
;;;Contents: George Marsaglia's PRNGs
;;;Date: Tue Jul 14, 2009
;;;
;;;Abstract
;;;
;;;	This  is  a  Scheme  port of  George  Marsaglia's  pseudo-random
;;;	numbers generators for  the C language.  The original  is in the
;;;	Public Domain.   The code  was posted by  George Marsaglia  in a
;;;	thread on  sci.stat.math and  sci.crypt starting on  January, 12
;;;	1999.
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 1999 George Marsaglia
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
(library (vicare crypto randomisations marsaglia)
  (export
    make-random-source/marsaglia/cong
    make-random-source/marsaglia/fib
    make-random-source/marsaglia/lfib4
    make-random-source/marsaglia/kiss
    make-random-source/marsaglia/mwc
    make-random-source/marsaglia/shr3
    make-random-source/marsaglia/swb)
  (import (rnrs)
    (vicare crypto randomisations)
    (vicare crypto randomisations low))


;;;; helpers

(define ^		bitwise-xor)
(define &		bitwise-and)

(define-syntax ADD
  (syntax-rules ()
    ((_ ?arg ...)
     (mod (+ ?arg ...) const:2^32))))

(define-syntax ADD256
  (syntax-rules ()
    ((_ ?arg ...)
     (mod (+ ?arg ...) 256))))

(define-syntax SUB
  (syntax-rules ()
    ((_ ?arg ...)
     (mod (- ?arg ...) const:2^32))))

;;The following version is equal to the above.
;;
;; (define-syntax SUB
;;   (syntax-rules ()
;;     ((_ ?a ?b)
;;      (let ((n (- ?a ?b)))
;;        (if (< n 0)
;; 	   (+ n const:2^32)
;; 	 n)))))

(define-syntax MUL
  (syntax-rules ()
    ((_ ?arg ...)
     (mod (* ?arg ...) const:2^32))))

(define-syntax >>
  (syntax-rules ()
    ((_ ?integer ?amount)
     (mod (bitwise-arithmetic-shift-right ?integer ?amount) const:2^32))))

(define-syntax <<
  (syntax-rules ()
    ((_ ?integer ?amount)
     (mod (bitwise-arithmetic-shift-left ?integer ?amount) const:2^32))))

(define-syntax u32-set!
  (syntax-rules ()
    ((_ ?vector ?index ?value)
     (bytevector-u32-native-set! ?vector (* 4 ?index) ?value))))

(define-syntax u32-ref
  (syntax-rules ()
    ((_ ?vector ?index)
     (bytevector-u32-native-ref ?vector (* 4 ?index)))))

(define (bytevector-clone/32bits src)
  (let* ((len (bytevector-length src))
	 (dst (make-bytevector   len)))
    (do ((i 0 (+ 4 i)))
	((= i len)
	 dst)
      (bytevector-u32-native-set! dst i (bytevector-u32-native-ref src i)))))


;;;; generators

;;The arguments to the macros are always in lexicographic (alphabetical)
;;order.

(define-syntax CONG
  ;;Original:
  ;;
  ;;#define CONG	(jcong = 69069 * jcong + 1234567)
  ;;
  (syntax-rules ()
    ((_ ?jcong)
     (begin
       (set! ?jcong (ADD (MUL 69069 ?jcong) 1234567))
       ?jcong))))

(define-syntax FIB
  ;;Original:
  ;;
  ;;#define FIB		((b = a + b), (a = b - a))
  ;;
  (syntax-rules ()
    ((_ ?a ?b)
     (begin
       (set! ?b (ADD ?a ?b))
       (set! ?a (SUB ?b ?a))
       ?a))))

(define-syntax LFIB4
  ;;Original:
  ;;
  ;;#define LFIB4	(c++,				\
  ;;			 t[c] = t[c]			\
  ;;			      + t[(uint8_t)(c + 58)]	\
  ;;			      + t[(uint8_t)(c + 119)]	\
  ;;			      + t[(uint8_t)(c + 178)])
  ;;
  ;;*NOTE* (Marco Maggi; Wed Jul 15, 2009)
  ;;
  ;;  I  dunno if  the first  statement in the  original code  "c++" was
  ;;meant to increment the index  "c" after the full expression has been
  ;;evaluated; the fact is that the  code, at the first invocation (c ==
  ;;0), fills the  slot with "c == 1"  not the slot with "c ==  0" as we
  ;;could expect intuitively.  Marsaglia  disclaims that he has not much
  ;;experience with the  C language.  What I know is  that leaving it as
  ;;it  is now,  and ported  below, Marsaglia's  test program  runs fine
  ;;producing the expected result.
  ;;
  ;;  Notice also that the  post-increment "c++" is ported to "(ADD256 1
  ;;?c)" and not  simply "(+ 1 ?c)", because in  the original C language
  ;;code "c" is of type "uint8_t", so it wraps around automatically when
  ;;overflowing.  This does not happen in Scheme, because Scheme has big
  ;;integers; so with  Scheme we apply "mod c 256" to  keep the value of
  ;;the index small (and it will fit in a fixnum).
  ;;
  (syntax-rules ()
    ((_ ?c ?t)
     (begin
       (set! ?c (ADD256 1 ?c))
       (u32-set! ?t ?c
		 (ADD (u32-ref ?t ?c)
		      (u32-ref ?t (ADD256 ?c  58))
		      (u32-ref ?t (ADD256 ?c 119))
		      (u32-ref ?t (ADD256 ?c 178))))
       (u32-ref ?t ?c)))))

(define-syntax KISS
  ;;Original:
  ;;
  ;;#define KISS	((MWC^CONG) + SHR3)
  ;;
  (syntax-rules ()
    ((_ ?jcong ?jsr ?w ?z)
     (ADD (^ (MWC ?w ?z) (CONG ?jcong)) (SHR3 ?jsr)))))

(define-syntax MWC
  ;;Original:
  ;;
  ;;#define MWC		((znew << 16) + wnew)
  ;;
  (syntax-rules ()
    ((_ ?w ?z)
     (ADD (<< (znew ?z) 16) (wnew ?w)))))

(define-syntax SHR3
  ;;Original:
  ;;
  ;;#define SHR3	(jsr ^= (jsr << 17),	\
  ;;			 jsr ^= (jsr >> 13),	\
  ;;			 jsr ^= (jsr << 5))
  ;;
  (syntax-rules ()
    ((_ ?jsr)
     (begin
       (set! ?jsr (^ ?jsr (<< ?jsr 17)))
       (set! ?jsr (^ ?jsr (>> ?jsr 13)))
       (set! ?jsr (^ ?jsr (<< ?jsr 5)))
       ?jsr))))

(define-syntax SWB
  ;;Original:
  ;;
  ;;#define SWB		(c++,					\
  ;;			 bro  = (x < y),			\
  ;;			 t[c] = (x = t[(uint8_t)(c + 34)])	\
  ;;			      - (y = t[(uint8_t)(c + 19)]	\
  ;;			           + bro))
  ;;
  ;;*NOTE* See the note for the  LFIB4 macro above.  It applies the same
  ;;here.
  ;;
  (syntax-rules ()
    ((_ ?bro ?c ?t ?x ?y)
     (begin
       (set! ?c (ADD256 1 ?c))
       (set! ?bro (if (< ?x ?y) 1 0))
       (u32-set! ?t ?c
		 (SUB (begin
			(set! ?x (u32-ref ?t (ADD256 ?c 34)))
			?x)
		      (ADD (begin
			     (set! ?y (u32-ref ?t (ADD256 ?c 19)))
			     ?y)
			   ?bro)))
       (u32-ref ?t ?c)))))

;;; --------------------------------------------------------------------
;;; The following are helpers.

(define-syntax wnew
  ;;Original:
  ;;
  ;;#define wnew	(w = 18000 * (w & 65535) + (w >> 16))
  ;;
  (syntax-rules ()
    ((_ ?w)
     (begin
       (set! ?w (ADD (MUL 18000 (& ?w 65535)) (>> ?w 16)))
       ?w))))

(define-syntax znew
  ;;Original:
  ;;
  ;;#define znew	(z = 36969 * (z & 65535) + (z >> 16))
  ;;
  (syntax-rules ()
    ((_ ?z)
     (begin
       (set! ?z (ADD (MUL 36969 (& ?z 65535)) (>> ?z 16)))
       ?z))))


(define (make-random-source/marsaglia/cong)
  (let ((jcong 2524969849))

    (define external-state-tag 'random-source-state/marsaglia/cong)
    ;; Numbers are generated in the range 0 <= N < M.
    (define M   const:2^32)
    (define M-1 const:2^32-1)

    (define (make-random-bits)
      (CONG jcong))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (set! jcong (integers-maker M)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag jcong))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 2 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((Y (vector-ref external-state 1)))
	(if (and (integer? Y) (exact? Y) (<= 0 Y M-1))
	    (set! jcong Y)
	  (assertion-violation 'external-state->internal-state
	    "illegal random source CONG state" external-state))))

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


(define (make-random-source/marsaglia/fib)
  (let ((A  9983651)
	(B 95746118))

    ;; Numbers are generated in the range 0 <= N < M.
    (define M   const:2^32)
    (define M-1 const:2^32-1)

    (define external-state-tag 'random-source-state/marsaglia/fib)

    (define (make-random-bits)
      (FIB A B))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (set! A (integers-maker M))
      (set! B (integers-maker M)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag A B))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y M-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source FIB state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 3 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((X (check-it 1))
	    (Y (check-it 2)))
	(set! A X)
	(set! B Y)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     2				    ; required seed values
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


(define (make-random-source/marsaglia/lfib4)
  (let* ((buffer-length/32bits 256)
	 (buffer-length/bytes  (* 4 buffer-length/32bits))
	 (t (bytevector-clone/32bits t-seed/lfib4))
	 (c 0))

    (define external-state-tag 'random-source-state/marsaglia/lfib4)
    ;; Numbers are generated in the range 0 <= N < M.
    (define M   const:2^32)
    (define M-1 const:2^32-1)

    (define (make-random-bits)
      (LFIB4 c t))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (set! c 0)
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(bytevector-u32-native-set! t (* 4 i) (integers-maker M))))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (let ((state (make-vector (+ buffer-length/32bits 1 1))))
	(vector-set! state 0 external-state-tag)
	(vector-set! state 1 c)
	(do ((i 0 (+ 1 i)))
	    ((= i buffer-length/32bits)
	     state)
	  (vector-set! state (+ 2 i) (bytevector-u32-native-ref t (* 4 i))))))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y M-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source LFIB4 state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= (+ buffer-length/32bits 1 1) (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(check-it (+ 2 i)))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(bytevector-u32-native-set! t (* 4 i) (vector-ref external-state (+ 2 i)))))

    ;;Initial seed.
    (let ((source (make-random-source/marsaglia/kiss))
	  (sub-seeds (let ((ell '(12345	   ;; jcong
				  34221	   ;; jsr
				  65435	   ;; w
				  12345))) ;; z
		       (lambda (U)
			 (let ((N (car ell)))
			   (set! ell (cdr ell))
			   N)))))
      (random-source-seed! source sub-seeds)
      (seed! (random-source-integers-maker source)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     buffer-length/32bits	    ; required seed values
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


(define (make-random-source/marsaglia/kiss)
  (let ((jcong 1017008441)
	(jsr   3259917390)
	(w     99545079)
	(z     2247183469))

    (define external-state-tag 'random-source-state/marsaglia/kiss)
    ;; Numbers are generated in the range 0 <= N < M.
    (define M   const:2^32)
    (define M-1 const:2^32-1)

    (define (make-random-bits)
      (KISS jcong jsr w z))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (set! jcong (integers-maker M))
      (set! jsr   (integers-maker M))
      (set! w     (integers-maker M))
      (set! z     (integers-maker M)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag jcong jsr w z))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y M-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source KISS state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 5 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((Y0 (check-it 1))
	    (Y1 (check-it 2))
	    (Y2 (check-it 3))
	    (Y3 (check-it 4)))
	(set! jcong Y0)
	(set! jsr   Y1)
	(set! w     Y2)
	(set! z     Y3)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     4				    ; required seed values
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


(define (make-random-source/marsaglia/mwc)
  (let ((W 1046675282)
	(Z 2374144069))

    (define external-state-tag 'random-source-state/marsaglia/mwc)
    ;; Numbers are generated in the range 0 <= N < M.
    (define M   const:2^32)
    (define M-1 const:2^32-1)

    (define (make-random-bits)
      (MWC W Z))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (set! W (integers-maker M))
      (set! Z (integers-maker M)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag W Z))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y M-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source MWC state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 3 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((X0 (check-it 1))
	    (X1 (check-it 2)))
	(set! W X0)
	(set! Z X1)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     2				    ; required seed values
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


(define (make-random-source/marsaglia/shr3)
  (let ((jsr 4176875757))

    (define external-state-tag 'random-source-state/marsaglia/shr3)
    ;; Numbers are generated in the range 0 <= N < M.
    (define M   const:2^32)
    (define M-1 const:2^32-1)

    (define (make-random-bits)
      (SHR3 jsr))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (set! jsr (integers-maker M)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag jsr))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y M-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source SHR3 state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 2 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (set! jsr (check-it 1)))

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


(define (make-random-source/marsaglia/swb)
  (let* ((buffer-length/32bits 256)
	 (buffer-length/bytes  (* 4 buffer-length/32bits))
	 (t   (bytevector-clone/32bits t-seed/swb))
	 (bro 0)
	 (c   64)
	 (x   0)
	 (y   0))

    (define external-state-tag 'random-source-state/marsaglia/swb)
    ;; Numbers are generated in the range 0 <= N < M.
    (define M   const:2^32)
    (define M-1 const:2^32-1)

    (define (make-random-bits)
      (SWB bro c t x y))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (set! c 0) (set! x 0) (set! y 0) (set! bro 0)
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(bytevector-u32-native-set! t (* 4 i) (integers-maker M))))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (let ((state (make-vector (+ buffer-length/32bits 1 4))))
	(vector-set! state 0 external-state-tag)
	(vector-set! state 1 bro)
	(vector-set! state 2 c)
	(vector-set! state 3 x)
	(vector-set! state 4 y)
	(do ((i 0 (+ 1 i)))
	    ((= i buffer-length/32bits)
	     state)
	  (vector-set! state (+ 5 i) (bytevector-u32-native-ref t (* 4 i))))))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y M-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source SWB state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= (+ buffer-length/32bits 1 4) (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(check-it (+ 5 i)))
      (let ((X0 (check-it 1))
	    (X1 (check-it 2))
	    (X2 (check-it 3))
	    (X3 (check-it 4)))
	(set! bro X0)
	(set! c   X1)
	(set! x   X2)
	(set! y   X3))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(bytevector-u32-native-set! t (* 4 i) (vector-ref external-state (+ 5 i)))))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     buffer-length/32bits	    ; required seed values
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


;;;; seed vectors for LFIB4 and SWB

(define t-seed/lfib4
  (uint-list->bytevector
   '(
     2406566837   3945488823   3217501373   1294085848
     1685356791   1879998683    354155033   1162810413
     2655137063    226356688   3472744942    543082922
     3762916352   1247145318   1960484516   2834937135
     3556413474   4015588494   2433866612   1542681415
     837890377   2156031093    641728713   2071218145
     3128597539   2092573851   1548155322   3718870319
     4167822943   2912646175   2911643863   3362985712
     1886528418    592629127   1204737837   1592327664
     3126732962      1554786   1906632076   2492476552
     111002753   4108454543   3793230690   4226138624
     422644314   2797766512    847647967   1289123979
     1969535464   1902370438    974706952   4222545502
     1130793588   3094143516     86711091   1749721364
     278301801   2154688974   2866252662   1487174760
     119646572   3549236133    302009453    162239274
     1776703449   1318650385   3594113676   4020467310
     941062378   1823539007   3848290724   1275281653
     4013456607   2787872658    770103194   2457950900
     3336084503    657199110   1723679164    798896722
     1705823163   1383115390   2041707688   3804770597
     1136092689   2827291170   1227628224    221271644
     1977255824   1852155101   2301247803   3595646145
     3709922751   2732837192    529978376   1840394256
     850198089   2335427956   3449558694   3023230479
     868614440   1099611446   3334890752   4190589169
     3993307947   3680421264   3143497825    930612474
     3805940850   2814955303   2609714583   3412337804
     1947870458   1173915461   1573012014    927925469
     1957387227   2823082061   3013874876   1144690729
     321153706   3505341913   2118050097   1431882057
     3398744687   2232621844   3712718609   2146053627
     159722353   3930544785   1289237494   3622337738
     132620258   3395020004   3971404583   2672497632
     418556265    375722238   4083798982   3832998408
     4013463868   1778758890   3900234414   3865137622
     7986325   3745270548   1264572959   1918227621
     2293215037   4013539679   1728047317   1329424085
     3875557031   3025492221    424037964   3308379748
     463014440   2869036490   3540996349    378750900
     1776914604   2246544134   1500424097   1031318071
     653625805   3003062951    950363772   1878098560
     1705309242   3066444961   2718370657   2326258461
     3630708903    987445312    187170730    958728662
     4026066647   4126946950   1809706436   2404651296
     2208247683   3582265286   2071240021   2116044308
     2716786358   2335209520   2975196056    245850675
     2274606232    801725457   4131433647    291882028
     173368002    211292240   1606934101     86222388
     2744096089   2594669165   2583936776    267153027
     1270120814   3848285866   3106929809   3457673136
     4059379692   2694564428   3420145872   4000069744
     186853964   1294043757    445382178   1508441456
     975087012   1239667053    827019081   2586195652
     2163669318   1252742899    221573984      9342999
     3375791150    813492546    881353819   1507495170
     4208776213   3297654913   3373356384   2245647143
     3413689312   2468442767   3233866900   2119481006
     3323181420    275426611    267195229   3023950404
     2970782157   3409642083   1313209854   3473951044
     3633718954   3189450566   1981092696    708163582
     488758822   2882757362   3437433893   1453523484
     3414277213   1339141620   3946001167    103943150
     1689000206   2735316516    916302334    823403292)
   (native-endianness) 4))

(define t-seed/swb
  (uint-list->bytevector
   '(
     3444715520    589627959   1852145839   1474265681
     1993127274   2857940909   2250181688   1758786885
     2648200516   3869276561   2131109435   3448185628
     1031138409   1251313198   3212840128   2372025478
     3036010335    804934505   3911956942   2392823437
     1006134078   1365723494   1825619600   2362330954
     3662377205     47556407    927442372   1809363575
     50559844    535744360   2869379169   3507056475
     4028552258   4084264384   3545253694    727759159
     2654534117   3533850408    304476320   1764902585
     3679535483   1403824263    834926528   3713675177
     2170354971   4114394043    130927612    282952029
     2968809523   3602074780   3693234238   3935779525
     4252866265   1395277872    667163033   1147112852
     1587818444    792412337    204831206   1551942735
     2359110429   4196550158   2674773592   4293179457
     1064612766   1513436951   3188416084   3881802103
     1948692362   3490069274   1862523903   1095295803
     351710150   2463825887   3707440418   1512411940
     319358280   1920961757   2779542203   1485468529
     3495985109   1286668255   1351892342   1520856886
     3299209310   3214982838   2677774711   1526350562
     1760324146   2081481100   3799603074    149889485
     3076011032   2859215584   3154421752   1815946240
     347370820    737664179   3237266725   2407857342
     3579310210   2547778315   2049832610   2137532637
     3529594535   3780058177   3589887133   1372294963
     2064299265   4074784224   1313827789   2964267889
     4273698787   3462965074   2664897851   4253389708
     1039607481   3278618941   3090152390   3824433171
     3271035935   2539015026   2046789971    709477519
     433921941    750683020   3531235443   3308066230
     612637020   1706458371   3446844314   3726803551
     553043350   1482744084   1347662155   1305578732
     3594677322   1489645083   1959332212    437229145
     187782346   1781868579     27681368   1400042057
     1118846718   3345622882   2272713889   2978416100
     615822425   4067047814    826286420   2239310652
     3681231216   3105564630    214685676   4045994565
     308613449   1398994572   2030420609   3333349787
     1617601581   3302743289   4041150887   2007052644
     1265002717   1859030124   1386709160   3190496627
     3676023138   3142527399   4136630278   1553025939
     79421320   1550860049   3293858257   1864730241
     742026642    301833874   3979643423   3265481090
     2096184672   2309979351   3241215696   1154856658
     819869785   1858497240    923479652   2598431294
     2065267425   2909206677   1606335486   3334236076
     3934221624   3419762829   3875133691    382578678
     3192117824    505678928   3898267352     95968481
     3699565520    882202361   4293193763   3918914979
     3313751816   3709105074   2794105100   3241694746
     942070142   2832737836    614516447    353963846
     1479714362   2758563050   3289904971   1039663995
     370394114    176879025   3590204376    160913434
     2350559687    873237449   1477310632   3727399863
     1801381754   2548083017   2668966345   1372528365
     3691739586   1279645001   3754587333   2278192398
     1433520522   4115832233   3056969095   2295443016
     1657648077    843630300   2473935165   4018708818
     2158081974    470260008   3001227663    412458272
     1722527672   1861980050   1730243661   1161583444
     776245256   2000435315   2675310267    133667017
     762714965    993364084   2316992894   1904476135)
   (native-endianness) 4))


;;;; done

)

;;; end of file
