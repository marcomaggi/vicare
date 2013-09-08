;;;
;;;Part of: Vicare Scheme
;;;Contents: record interface to randomness sources
;;;Date: Sat Jul  4, 2009
;;;
;;;Copyright (c) 2009, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2008 Derick Eddington
;;;Copyright (c) 2002 Sebastian Egner
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.
;;;


#!r6rs
(library (vicare crypto randomisations)
  (export

    ;; random source interface
    random-source-maker			random-source?
    random-source-state-ref		random-source-state-set!
    random-source-seed!			random-source-jumpahead!
    random-source-integers-maker	random-source-reals-maker
    random-source-bytevectors-maker	random-source-bytevectors-filler

    ;; random source constructors
    make-random-source/mrg32k3a		make-random-source/device

    ;; low level API for device-based random sources of numbers
    random-device-cache-length
    %device-read-bytevector!		%device-read-bytevector
    %device-write-bytevector!
    random-device-bytevector!		random-device-bytevector
    urandom-device-bytevector!		urandom-device-bytevector

    ;; no fuss API
    random-integer			random-real
    default-random-source

    ;; utilities
    random-source-integers-maker-from-range
    random-source-reals-maker-from-range
    random-permutations-maker)
  (import (vicare)
    (vicare crypto randomisations low))


;;;; randomness source

(define (random-source-state-ref s)
  ((:random-source-state-ref s)))

(define (random-source-state-set! s state)
  ((:random-source-state-set! s) state))

(define (random-source-seed! s integers-maker)
  ((:random-source-seed! s) integers-maker))

(define (random-source-jumpahead! s steps)
  ((:random-source-jumpahead! s) steps))

(define (random-source-required-seed-values s)
  (:random-source-required-seed-values s))

(define (random-source-integers-maker s)
  (:random-source-integers-maker s))

(define random-source-reals-maker
  (case-lambda
   ((s)
    ((:random-source-reals-maker s)))
   ((s unit)
    ((:random-source-reals-maker s) unit))))

(define (random-source-bytevectors-maker s)
  (lambda (number-of-bytes)
    (let ((bv (make-bytevector number-of-bytes)))
      ((:random-source-bytevectors-filler s) bv)
      bv)))

(define (random-source-bytevectors-filler s)
  (:random-source-bytevectors-filler s))


;;; MRG32k3a pseudo-random numbers generator

(define (make-random-source/mrg32k3a)
  (let ((A1 1062452522) (A2 2961816100) (A3 342112271)
	(B1 2854655037)	(B2 3321940838)	(B3 3542344109))

    (define external-state-tag 'random-source-state/mrg32k3a)
    (define M1 4294967087) ; modulus of component 1
    (define M2 4294944443) ; modulus of component 2

    (define (make-random-bits)
      ;;Compute random bits and advance the state.
      (let ((A0 (mod (- (* 1403580 A2) (*  810728 A3)) M1))
	    (B0 (mod (- (*  527612 B1) (* 1370589 B3)) M2)))
	(set! A3 A2) ; shift the A vector right, purging the old A3
	(set! A2 A1)
	(set! A1 A0)
	(set! B3 B2) ; shift the B vector right, purging the old B3
	(set! B2 B1)
	(set! B1 B0)
	(mod (- A0 B0) M1)))

    (define (make-random-32bits)
      (make-random-integer const:2^32 M1 make-random-bits))

    (define (seed! integers-maker)
      ;; G. Marsaglia's simple 16-bit generator with carry
      (let* ((m 65536)
	     (x (integers-maker m)))
	(define (random n) ; m < n < m^2
	  (define (random-m)
	    (let ((y (mod x m)))
	      (set! x (+ (* 30903 y) (div x m)))
	      y))
	  (mod (+ (* (random-m) m) (random-m)) n))
	(let ((M1-1 (- M1 1))
	      (M2-1 (- M2 1)))
	  (set! A1 (+ 1 (mod (+ A1 (random M1-1)) M1-1)))
	  (set! B1 (+ 1 (mod (+ B1 (random M2-1)) M2-1)))
	  (set! A2 (mod (+ A2 (random M1)) M1))
	  (set! A3 (mod (+ A3 (random M1)) M1))
	  (set! B2 (mod (+ B2 (random M2)) M2))
	  (set! B3 (mod (+ B3 (random M2)) M2)))))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      ;;Package the state to be written in a way that can be read back.
      (vector external-state-tag A1 A2 A3 B1 B2 B3))

    (define (external-state->internal-state external-state)
      ;;Given a packaged state, verifies it and return an internal state.
      (define (check-value idx M)
	(let ((S (vector-ref external-state idx)))
	  (if (and (integer? S) (exact? S) (<= 0 S (- M 1)))
	      S
	    (assertion-violation 'external-state->internal-state
	      "illegal random source MRG32k3a state value" S external-state))))
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
	    "illegal random source MRG32k3a degenerate state" external-state))
	(set! A1 s1) (set! A2 s2) (set! A3 s3)
	(set! B1 r1) (set! B2 r2) (set! B3 r3)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     1				    ; required seed values
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


;;;; low level API for randomness from devices

(define (%device-read-bytevector! device bv)
  ;;Fill the  bytevector BV  with bytes from  DEVICE.  Open  the device,
  ;;then close it.
  (let* ((number-of-bytes (bytevector-length bv))
	 (port (open-file-input-port device
				     (file-options no-create)
				     (buffer-mode block))))
    (dynamic-wind
	(lambda () #f)
	(lambda () (get-bytevector-n! port bv 0 number-of-bytes))
	(lambda () (close-port port))))
  bv)

(define (%device-read-bytevector device number-of-bytes)
  ;;Return a  newly allocated bytevectors filled with  random bytes from
  ;;DEVICE.
  (%device-read-bytevector! device (make-bytevector number-of-bytes)))

(define (%device-write-bytevector! device bv)
  ;;Write the bytevector BV to DEVICE.  Open the device, then close it.
  (let ((port (open-file-output-port device
				     (file-options no-create)
				     (buffer-mode block))))
    (dynamic-wind
	(lambda () #f)
	(lambda () (put-bytevector port bv))
	(lambda () (close-port port)))))

;;; --------------------------------------------------------------------

(define (random-device-bytevector! bv)
  ;;Fill the bytevector BV with bytes from "/dev/random".
  (%device-read-bytevector! "/dev/random" bv))

(define (random-device-bytevector number-of-bytes)
  ;;Return  a   newly  allocated  bytevector  filled   with  bytes  from
  ;;"/dev/random".
  (%device-read-bytevector "/dev/random" number-of-bytes))

(define (urandom-device-bytevector! bv)
  ;;Fill the bytevector BV with bytes from "/dev/urandom".
  (%device-read-bytevector! "/dev/urandom" bv))

(define (urandom-device-bytevector number-of-bytes)
  ;;Return  a   newly  allocated  bytevector  filled   with  bytes  from
  ;;"/dev/urandom".
  (%device-read-bytevector "/dev/urandom" number-of-bytes))


;;;; randomness source from devices

(define random-device-cache-length
  (make-parameter 4096
    (lambda (obj)
      (if (and (integer? obj) (exact? obj) (positive? obj))
	  obj
	(assertion-violation 'random-device-cache-length
	  "invalid device cache length, expected positive, exact, integer"
	  obj)))))

(define make-random-source/device
  (case-lambda
   (()
    (make-random-source/device "/dev/urandom"))
   ((device)
    (let* ((device device)
	   (cache (make-bytevector (random-device-cache-length)))
	   (next  (bytevector-length cache)))
		;This init value  causes the vector to be  filled at the
		;first invocation.

      (define external-state-tag 'random-source-state/device)
      (define M const:2^32)

      (define (make-random-bits)
	;;Extract the next 32bits from the cache and advance the cursor.
	;;Refill the cache when needed.
	(when (<= (bytevector-length cache) next)
	  (%device-read-bytevector! device cache)
	  (set! next 0))
	(let ((n (bytevector-u32-native-ref cache next)))
	  (set! next (+ 4 next))
	  n))

      (define make-random-32bits make-random-bits)

      (define (internal-state->external-state)
	(vector external-state-tag device next cache))

      (define (external-state->internal-state external-state)
	(unless (and (vector? external-state)
		     (= 4 (vector-length external-state))
		     (eq? external-state-tag (vector-ref external-state 0)))
	  (assertion-violation 'external-state->internal-state
	    "invalid device randomness source state" external-state))
	(let ((_device (vector-ref external-state 1))
	      (_next   (vector-ref external-state 2))
	      (_cache  (vector-ref external-state 3)))
	  (unless (and (string? _device) (file-exists? _device))
	    (assertion-violation 'external-state->internal-state
	      "invalid device randomness source state, expected existent file name as device"
	      _device))
	  (unless (and (integer? _next) (exact? _next) (positive? _next))
	    (assertion-violation 'external-state->internal-state
	      "invalid device randomness source state, expected positive integer as next index"
	      _next))
	  (unless (bytevector? _cache)
	    (assertion-violation 'external-state->internal-state
	      "invalid device randomness source state, expected bytevector as randomness pool"
	      _cache))
	  (set! device _device)
	  (set! next   _next)
	  (set! cache  _cache)))

      (define (seed! integers-maker)
	;;Reads random 32bit unsigned integers from INTEGERS-MAKER until
	;;it returns  #f; build a bytevector  with them and  write it to
	;;the DEVICE.
	(let-values (((count numbers)
		      (do ((numbers (list (integers-maker M))
				    (cons (integers-maker M) numbers))
			   (count 0 (+ 1 count)))
			  ((not (car numbers))
			   (values count (cdr numbers))))))
	  (let ((bv (make-bytevector (* 4 count))))
	    (do ((i 0 (+ 4 i))
		 (numbers numbers (cdr numbers)))
		((null? numbers))
	      (bytevector-u32-native-set! bv i (car numbers)))
	    (%device-write-bytevector! device bv))))

      (define (jumpahead! number-of-steps)
	(do ((i 0 (+ 1 i)))
	    ((= i number-of-steps))
	  (make-random-bits)))

      (:random-source-make
       internal-state->external-state ; state-ref
       external-state->internal-state ; state-set!
       seed!			      ; seed!
       jumpahead!		      ; jumpahead!
       #f			      ; required seed values
       (lambda (U)		      ; integers-maker
	 (make-random-integer U M make-random-bits))
       (case-lambda ; reals-maker
	(()
	 (lambda ()
	   (make-random-real M make-random-bits)))
	((unit)
	 (lambda ()
	   (make-random-real M make-random-bits unit))))
       (lambda (bv) ; bytevectors-filler
	 (random-bytevector-fill! bv make-random-32bits)))))))


;;; no fuss API

(define random-source-maker
  (make-parameter make-random-source/mrg32k3a
    (lambda (obj)
      (if (procedure? obj)
	  obj
	(assertion-violation 'random-source-maker
	  "expected procedure as random source maker" obj)))))

(define default-random-source
  (make-random-source/mrg32k3a))

(define random-integer
  (random-source-integers-maker default-random-source))

(define random-real
  (random-source-reals-maker default-random-source))


;;;; utility procedures, numbers from range

(define random-source-integers-maker-from-range
  (case-lambda
   ((source start last)
    (random-source-integers-maker-from-range source start last 1))
   ((source start last step)
    (let ((integer-maker (random-source-integers-maker source))
	  (max		 (div (+ (- last start) 1) step)))
      (if (= 1 step)
	  (lambda ()
	    (+ start (integer-maker max)))
	(lambda ()
	  (+ start (* step (integer-maker max)))))))))

(define random-source-reals-maker-from-range
  (case-lambda
   ((source start past)
    (let ((real-maker (random-source-reals-maker source))
	  (len        (- past start)))
      (lambda ()
	(+ start (* len (real-maker))))))
   ((source start past step)
    (let ((integer-maker	(random-source-integers-maker source))
	  (max			(floor (/ (- past start) step))))
      (lambda ()
	(+ start (* step (integer-maker max))))))))


;;; utility procedures, permutations

(define (random-permutations-maker source)
  ;;For  the   algorithm  refer  to   Knuth's  ``The  Art   of  Computer
  ;;Programming'', Vol. II, 2nd ed., Algorithm P of Section 3.4.2.
  (let ((integer-maker (random-source-integers-maker source)))
    (lambda (n)
      (let ((x (make-vector n 0)))
	(do ((i 0 (+ i 1)))
	    ((= i n))
	  (vector-set! x i i))
	(do ((k n (- k 1)))
	    ((= k 1) x)
	  (let* ((i (- k 1))
		 (j (integer-maker k))
		 (xi (vector-ref x i))
		 (xj (vector-ref x j)))
	    (vector-set! x i xj)
	    (vector-set! x j xi)))))))


;;; done

)

;;; end of file
