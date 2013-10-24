;;;
;;;Part of: Vicare Scheme
;;;Contents: randomness source using Mersenne Twister
;;;Date: Tue Jul 14, 2009
;;;
;;;Abstract
;;;
;;;	The following is the original code by Michael Brundage, slightly
;;;	modified:
;;;
;;;#define MERSENNE_BUFFER_LENGTH       624
;;;
;;;typedef struct mersenne_context_t {
;;;  int			index;
;;;  unsigned long		buffer[MERSENNE_BUFFER_LENGTH];
;;;} mersenne_context_t;
;;;
;;;/* 'buffer'  must  point  to  a MERSENNE_BUFFER_LENGTH  long  buffer  of
;;;   'unsigned long' bytes, filled with seed numbers */
;;;static void	mersenne_init		(mersenne_context_t * ctx,
;;;					 unsigned long * buffer);
;;;
;;;static unsigned long mersenne_random	(mersenne_context_t * ctx);
;;;
;;;#define MT_IA           397
;;;#define MT_IB           (MERSENNE_BUFFER_LENGTH - MT_IA)
;;;#define UPPER_MASK      0x80000000
;;;#define LOWER_MASK      0x7FFFFFFF
;;;#define MATRIX_A        0x9908B0DF
;;;#define TWIST(b,i,j)    ((b)[i] & UPPER_MASK) | ((b)[j] & LOWER_MASK)
;;;#define MAGIC(s)        (((s)&1)*MATRIX_A)
;;;
;;;void
;;;mersenne_init(mersenne_context_t * ctx, unsigned long * buffer)
;;;{
;;;  memcpy(&(ctx->buffer), buffer,
;;;         sizeof(unsigned long)*MERSENNE_BUFFER_LENGTH);
;;;  ctx->index  = 0;
;;;}
;;;unsigned long
;;;mersenne_random (mersenne_context_t * ctx)
;;;{
;;;  unsigned long *	b   = ctx->buffer;
;;;  int			idx = ctx->index;
;;;  unsigned long		s;
;;;  int			i;
;;;
;;;  if (idx == MERSENNE_BUFFER_LENGTH * sizeof(unsigned long))
;;;    {
;;;      idx = 0;
;;;      i = 0;
;;;      for (; i < MT_IB; i++) {
;;;	s = TWIST(b, i, i+1);
;;;	b[i] = b[i + MT_IA] ^ (s >> 1) ^ MAGIC(s);
;;;      }
;;;      for (; i < MERSENNE_BUFFER_LENGTH-1; i++) {
;;;	s = TWIST(b, i, i+1);
;;;	b[i] = b[i - MT_IB] ^ (s >> 1) ^ MAGIC(s);
;;;      }
;;;
;;;      s = TWIST(b, MERSENNE_BUFFER_LENGTH-1, 0);
;;;      b[MERSENNE_BUFFER_LENGTH-1] = b[MT_IA-1] ^ (s >> 1) ^ MAGIC(s);
;;;    }
;;;  ctx->index = idx + sizeof(unsigned long);
;;;  return *(unsigned long *)((unsigned char *)b + idx);
;;;  /*
;;;    Matsumoto and  Nishimura additionally confound  the bits returned
;;;    to the caller but this doesn't increase the randomness, and slows
;;;    down the generator by as much as 25%.  So I omit these operations
;;;    here.
;;;
;;;    r ^= (r >> 11);
;;;    r ^= (r << 7) & 0x9D2C5680;
;;;    r ^= (r << 15) & 0xEFC60000;
;;;    r ^= (r >> 18);
;;;  */
;;;}
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 1990-2005 Michael Brundage
;;;<http://www.qbrundage.com/michaelb/pubs/essays/random_number_generation>
;;;
;;;Modified for Guile-Random by Marco Maggi
;;;Ported to Scheme for Nausicaa/Scheme by Marco Maggi
;;;
;;;CREATIVE COMMONS PUBLIC DOMAIN DEDICATION
;;;
;;;Copyright-Only  Dedication (based  on  United States  law) or  Public
;;;Domain Certification
;;;
;;;The person  or persons  who have associated  work with  this document
;;;(the "Dedicator" or "Certifier") hereby either (a) certifies that, to
;;;the best  of his knowledge, the  work of authorship  identified is in
;;;the public domain of the country from which the work is published, or
;;;(b) hereby  dedicates whatever copyright the dedicators  holds in the
;;;work  of  authorship identified  below  (the  "Work")  to the  public
;;;domain. A  certifier, moreover,  dedicates any copyright  interest he
;;;may have in the associated work, and for these purposes, is described
;;;as a "dedicator" below.
;;;
;;;A certifier has taken reasonable steps to verify the copyright status
;;;of this  work. Certifier recognizes  that his good faith  efforts may
;;;not shield him from liability if in fact the work certified is not in
;;;the public domain.
;;;
;;;Dedicator  makes this  dedication for  the benefit  of the  public at
;;;large   and  to   the  detriment   of  the   Dedicator's   heirs  and
;;;successors. Dedicator intends  this dedication to be an  overt act of
;;;relinquishment in  perpetuity of all present and  future rights under
;;;copyright law,  whether vested or contingent, in  the Work. Dedicator
;;;understands  that  such relinquishment  of  all  rights includes  the
;;;relinquishment  of all rights  to enforce  (by lawsuit  or otherwise)
;;;those copyrights in the Work.
;;;
;;;Dedicator recognizes that, once placed in the public domain, the Work
;;;may be  freely reproduced, distributed,  transmitted, used, modified,
;;;built  upon,  or  otherwise  exploited  by anyone  for  any  purpose,
;;;commercial or  non-commercial, and in  any way, including  by methods
;;;that have not yet been invented or conceived.
;;;


#!r6rs
(library (vicare crypto randomisations mersenne)
  (export make-random-source/mersenne)
  (import (rnrs)
    (vicare crypto randomisations)
    (vicare crypto randomisations low))


(define (make-random-source/mersenne)
  (let* ((buffer-length/32bits	624)
		;length in units of 32bits integers
	 (byte-index		buffer-length/32bits)
		;this makes it run at the first invocation of MAKE-RANDOM-BITS
	 (buffer-length/bytes	(* 4 buffer-length/32bits))
	 (buffer		(make-bytevector buffer-length/bytes)))

    (define external-state-tag 'random-source-state/mersenne)
    (define M   const:2^32)
    (define M-1 const:2^32-1)
    (define MT_IA 397)
    (define MT_IB (- buffer-length/32bits MT_IA))
    (define buffer-length-1 (- buffer-length/32bits 1))

    (define (make-random-bits)
      (when (= byte-index buffer-length/bytes)
	(let-syntax ((%magic (syntax-rules ()
			       ((_ ?s)
				(* (bitwise-and ?s 1) #x9908B0DF))))
		     (%twist (syntax-rules ()
			       ((_ ?i ?j)
				(bitwise-ior
				 (bitwise-and (bytevector-u32-native-ref buffer (* 4 ?i))
					      #x80000000)
				 (bitwise-and (bytevector-u32-native-ref buffer (* 4 ?j))
					      #x7FFFFFFF))))))
	  (let-syntax ((%make (syntax-rules ()
				((_ ?i ?j ?k)
				 (let ((s (%twist ?i ?j)))
				   (bytevector-u32-native-set!
				    buffer (* 4 ?i)
				    (bitwise-xor (bytevector-u32-native-ref buffer (* 4 ?k))
						 (bitwise-arithmetic-shift-right s 1)
						 (%magic s))))))))
	    (set! byte-index 0)
	    (do ((i 0 (+ 1 i)))
		((= i MT_IB))
	      (%make i (+ 1 i) (+ i MT_IA)))
	    (do ((i MT_IB (+ 1 i)))
		((= i buffer-length-1))
	      (%make i (+ 1 i) (- i MT_IB)))
	    (%make buffer-length-1 0 (- MT_IA 1)))))
      (let ((N (bytevector-u32-native-ref buffer byte-index)))
	(let ((N (bitwise-xor N (bitwise-arithmetic-shift-right N 11))))
	  (let ((N (bitwise-xor N (bitwise-and #x9D2C5680 (bitwise-arithmetic-shift-left N 7)))))
	    (let ((N (bitwise-xor N (bitwise-and #xEFC60000 (bitwise-arithmetic-shift-left N 15)))))
	      (let ((N (bitwise-xor N (bitwise-arithmetic-shift-right N 18))))
		(set! byte-index (+ byte-index 4))
		N))))))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M make-random-bits))

    (define (seed! integers-maker)
      (do ((i 0 (+ 4 i)))
	  ((= i buffer-length/bytes)
	   (set! byte-index 0))
	(do ((N (integers-maker const:2^32) (integers-maker const:2^32)))
	    ((< 0 N)
	     (bytevector-u32-native-set! buffer i N)))))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (let ((V (make-vector (+ buffer-length/32bits 1 1) #f)))
	(vector-set! V 0 external-state-tag)
	(vector-set! V 1 byte-index)
	(do ((i 0 (+ 1 i)))
	    ((= i buffer-length/32bits)
	     V)
	  (vector-set! V (+ 2 i) (bytevector-u32-native-ref buffer (* 4 i))))))

    (define (external-state->internal-state external-state)
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= (+ buffer-length/32bits 1 1) (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((I (vector-ref external-state 1)))
	(if (<= 0 I (- buffer-length/bytes 1))
	    (set! byte-index I)
	  (assertion-violation 'external-state->internal-state
	    "illegal random source Mersenne state, bad index" external-state)))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(let ((X (vector-ref external-state (+ 2 i))))
	  (if (and (integer? X) (exact? X) (<= 0 X M-1))
	      (bytevector-u32-native-set! buffer (* 4 i) X)
	    (assertion-violation 'external-state->internal-state
	      "illegal random source Mersenne state" external-state)))))

    ;;Initial seeeding.
    (seed! (random-source-integers-maker (make-random-source/mrg32k3a)))

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


;;;; done

)

;;; end of file
