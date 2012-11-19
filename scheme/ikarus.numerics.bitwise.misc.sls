;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Implementation  of  FXREVERSE-BIT-FIELD from:
;;;
;;;  Original patch by Göran Weinholt, posted on the Ikarus bug tracker.
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus numerics bitwise misc)
  (export
    bitwise-bit-set?		bitwise-first-bit-set
    bitwise-bit-count
    fxfirst-bit-set		fxbit-count
    fxlength			fxbit-set?
    fxcopy-bit			fxcopy-bit-field
    fxrotate-bit-field		fxreverse-bit-field
    fxbit-field)
  (import (except (ikarus)
		  bitwise-bit-set?		bitwise-first-bit-set
		  bitwise-bit-count
		  fxfirst-bit-set		fxbit-count
		  fxlength			fxbit-set?
		  fxcopy-bit			fxcopy-bit-field
		  fxrotate-bit-field		fxreverse-bit-field
		  fxbit-field)
    (ikarus system $fx)
    (ikarus system $bignums)
    (ikarus system $flonums)
    (vicare syntactic-extensions)
    (vicare arguments validation))


(module (bitwise-first-bit-set
	 fxfirst-bit-set)

  (define (byte-first-bit-set x i)
    (import (ikarus system $bytevectors))
    (define-syntax make-first-bit-set-bytevector
      (lambda (x)
	(define (fst n)
	  (cond
	   ((odd? n) 0)
	   (else (+ 1 (fst (bitwise-arithmetic-shift-right n 1))))))
	(u8-list->bytevector
	 (cons 0 #| not used |#
	       (let f ((i 1))
		 (cond
                  ((= i 256) '())
                  (else (cons (fst i) (f (+ i 1))))))))))
    (define bv (make-first-bit-set-bytevector))
    ($fx+ i ($bytevector-u8-ref bv x)))

  (define ($fxloop x i)
    (let ((y ($fxlogand x 255)))
      (if ($fx= y 0)
	  ($fxloop ($fxsra x 8) ($fx+ i 8))
	(byte-first-bit-set y i))))

  (define ($bnloop x i idx)
    (let ((b ($bignum-byte-ref x idx)))
      (if ($fxzero? b)
	  ($bnloop x ($fx+ i 8) ($fx+ idx 1))
	(byte-first-bit-set b i))))

  (define ($fxfirst-bit-set x)
    (if ($fx> x 0)
	($fxloop x 0)
      (if ($fx= x 0)
	  -1
	(if ($fx> x (least-fixnum))
	    ($fxloop ($fx- 0 x) 0)
	  ($bnloop (- x) 0 0)))))

  (define (fxfirst-bit-set x)
    (define who 'fxfirst-bit-set)
    (with-arguments-validation (who)
	((fixnum	x))
      ($fxfirst-bit-set x)))

  (define (bitwise-first-bit-set x)
    (define who 'bitwise-first-bit-set)
    (with-arguments-validation (who)
	((exact-integer	x))
      (if (fixnum? x)
	  ($fxfirst-bit-set x)
	($bnloop x 0 0))))

  #| end of module: |# )


(module (fxbit-count bitwise-bit-count)

  (define (pos-fxbitcount n)
      ;;; nifty parrallel count from:
      ;;; http://infolab.stanford.edu/~manku/bitcount/bitcount.html
    (case-fixnums (fixnum-width)
      ((30)
       (let ((m0 #x15555555)
	     (m1 #x13333333)
	     (m2 #x0f0f0f0f))
	 (let* ((n ($fx+ ($fxlogand n m0) ($fxlogand ($fxsra n 1) m0)))
		(n ($fx+ ($fxlogand n m1) ($fxlogand ($fxsra n 2) m1)))
		(n ($fx+ ($fxlogand n m2) ($fxlogand ($fxsra n 4) m2))))
	   (fxmodulo n 255))))
      (else
       (let ((m0 #x0555555555555555)
	     (m1 #x0333333333333333)
	     (m2 #x0f0f0f0f0f0f0f0f)
	     (m3 #x00ff00ff00ff00ff))
	 (let* ((n ($fx+ ($fxlogand n m0) ($fxlogand ($fxsra n 1) m0)))
		(n ($fx+ ($fxlogand n m1) ($fxlogand ($fxsra n 2) m1)))
		(n ($fx+ ($fxlogand n m2) ($fxlogand ($fxsra n 4) m2)))
		(n ($fx+ ($fxlogand n m3) ($fxlogand ($fxsra n 8) m3))))
	   (fxmodulo n 255))))))

  (define ($fxbitcount n)
    (if ($fx< n 0)
	(fxlognot (pos-fxbitcount (fxlognot n)))
      (pos-fxbitcount n)))

  (module (bnbitcount)

    (define (bnbitcount n)
      (if ($bignum-positive? n)
	  (%poscount n ($fx- ($bignum-size n) 4) 0)
	(let ((n (bitwise-not n)))
	  (bitwise-not (%poscount n ($fx- ($bignum-size n) 4) 0)))))

    (define (%poscount x idx c)
      (let ((c (+ c ($fx+ (pos-fxbitcount ($fxlogor
					   ($fxsll ($bignum-byte-ref x ($fx+ idx 3)) 8)
					   ($bignum-byte-ref x ($fx+ idx 2))))
			  (pos-fxbitcount ($fxlogor
					   ($fxsll ($bignum-byte-ref x ($fxadd1 idx)) 8)
					   ($bignum-byte-ref x idx)))))))
	(if ($fx= idx 0)
	    c
	  (%poscount x ($fx- idx 4) c))))

    #| end of module: bnbitcount |# )

  (define (fxbit-count n)
    ;;FIXME  To be  checked against  R6RS  errata (Marco  Maggi; Nov  5,
    ;;2011).
    ;;
    (define who 'fxbit-count)
    (with-arguments-validation (who)
	((fixnum	n))
      ($fxbitcount n)))

  (define (bitwise-bit-count n)
    (define who 'bitwise-bit-count)
    (with-arguments-validation (who)
	((exact-integer	n))
      (if (fixnum? n)
	  ($fxbitcount n)
	(bnbitcount n))))

  #| end of module |# )


(define (fxlength x)
  (define who 'fxlength)
  (define (fxlength32 x)
    (let* ((fl  ($fixnum->flonum x))
	   (sbe ($fxlogor ($fxsll ($flonum-u8-ref fl 0) 4)
				($fxsra ($flonum-u8-ref fl 1) 4))))
      (if ($fx= sbe 0)
	  0
	($fx- sbe 1022))))
  (define (fxlength64 x)
    (if ($fx> x #x7FFFFFFF)
	($fx+ 31 (fxlength32 ($fxsra x 31)))
      (fxlength32 x)))
  (with-arguments-validation (who)
      ((fixnum x))
    (if ($fx= 30 (fixnum-width))
	(fxlength32 (if ($fx< x 0) ($fxlognot x) x))
      (fxlength64 (if ($fx< x 0) ($fxlognot x) x)))))

(define (fxbit-set? x i)
  ;;Updated to the R6RS errata (Marco Maggi; Nov 5, 2011).
  ;;
  (define who 'fxbit-set?)
  (with-arguments-validation (who)
      ((fixnum        x)
       (fixnum-index  i))
    (if ($fx>= i ($fxsub1 (fixnum-width)))
	($fx< x 0)
      (not ($fxzero? ($fxlogand ($fxsra x i) 1))))))

(define (bitwise-bit-set? x i)
  (define who 'bitwise-bit-set?)
  (cond ((fixnum? i)
	 (when ($fx< i 0)
	   (assertion-violation who "index must be non-negative" i))
	 (cond ((fixnum? x)
		(if ($fx< i (fixnum-width))
		    ($fx= ($fxlogand ($fxsra x i) 1) 1)
		  ($fx< x 0)))
	       ((bignum? x)
		(let ((n ($bignum-size x)))
		  (let ((m ($fx* n 8)))
		    (cond (($fx< m i)
			   (not ($bignum-positive? x)))
			  (($bignum-positive? x)
			   (let ((b ($bignum-byte-ref x ($fxsra i 3))))
			     ($fx= ($fxlogand ($fxsra b ($fxlogand i 7)) 1) 1)))
			  (else
			   (= 1 (bitwise-and (bitwise-arithmetic-shift-right x i) 1)))))))
	       (else
		(assertion-violation who "not an exact integer" x))))
	((bignum? i)
	 (unless ($bignum-positive? i)
	   (assertion-violation who "index must be non-negative"))
	 (cond ((fixnum? x)
		($fx< x 0))
	       ((bignum? x)
		(= 1 (bitwise-and (bitwise-arithmetic-shift-right x i) 1)))
	       (else
		(assertion-violation who "not an exact integer" x))))
	(else
	 (assertion-violation who "index is not an exact integer" i))))


(define (fxcopy-bit x i b)
  (define who 'fxcopy-bit)
  (if (fixnum? x)
      (if (fixnum? i)
	  (if (and ($fx<= 0 i) ($fx< i (fixnum-width)))
	      (case b
		((0) ($fxlogand x ($fxlognot ($fxsll 1 i))))
		((1) ($fxlogor x ($fxsll 1 i)))
		(else (assertion-violation who "invalid bit value" b)))
	    (assertion-violation who "index out of range" i))
	(assertion-violation who "index is not a fixnum" i))
    (assertion-violation who "not a fixnum" x)))

(define (fxcopy-bit-field x i j b)
  (define who 'fxcopy-bit-field)
  (if (fixnum? x)
      (if (fixnum? i)
	  (if ($fx<= 0 i)
	      (if (fixnum? j)
		  (if ($fx< j (fixnum-width))
		      (if ($fx<= i j)
			  (if (fixnum? b)
			      (let ((m
				     ($fxlogxor
				      ($fxsub1 ($fxsll 1 i))
				      ($fxsub1 ($fxsll 1 j)))))
				($fxlogor
				 ($fxlogand m ($fxsll b i))
				 ($fxlogand ($fxlognot m) x)))
			    (assertion-violation who "not a fixnum" b))
			(if ($fx<= 0 j)
			    (assertion-violation who "index out of range" j)
			  (assertion-violation who "indices not in order" i j)))
		    (assertion-violation who "index out of range" j))
		(assertion-violation who "not a fixnum" j))
	    (assertion-violation who "index out of range" i))
	(assertion-violation who "not a fixnum" i))
    (assertion-violation who "not a fixnum" x)))

(define ($fxrotate-bit-field x i j c w)
  (let ((m ($fxsll ($fxsub1 ($fxsll 1 w)) i)))
    (let ((x0 ($fxlogand x m)))
      (let ((lt ($fxsll x0 c)) (rt ($fxsra x0 ($fx- w c))))
	(let ((x0 ($fxlogand ($fxlogor lt rt) m)))
	  ($fxlogor x0 ($fxlogand x ($fxlognot m))))))))

(define (fxrotate-bit-field x i j c)
  ;;FIXME  This must  be checked  and eventually  updated from  the R6RS
  ;;errata (Marco Maggi; Nov 5, 2011).
  ;;
  (define who 'fxrotate-bit-field)
  (if (fixnum? x)
      (if (fixnum? i)
	  (if ($fx>= i 0)
	      (if (fixnum? j)
		  (if ($fx< j (fixnum-width))
		      (let ((w ($fx- j i)))
			(if ($fx>= w 0)
			    (if (fixnum? c)
				(if (and ($fx>= c 0) ($fx< c w))
				    ($fxrotate-bit-field x i j c w)
				  (assertion-violation who "count is invalid" c))
			      (assertion-violation who "count is not a fixnum" c))
			  (assertion-violation who "field width is negative" i j)))
		    (assertion-violation who "end index is out of range" j))
		(assertion-violation who "end index is not a fixnum" j))
	    (assertion-violation who "start index is out of range" i))
	(assertion-violation who "start index is not a fixnum" i))
    (assertion-violation who "not a fixnum" x)))

(define (fxreverse-bit-field v start end)

  ;;This is from  the original patch by Göran  Weinholt, posted on the
  ;;Ikarus bug tracker.
  ;;
  ;; (define (%fxreverse-bit-field61 v)
  ;;   ;; Based on <http://aggregate.org/MAGIC/#Bit Reversal>.
  ;;   (assert (= (fixnum-width) 61))
  ;;   (let* ( ;; Swap pairs of bits
  ;; 	     (v (fxior (fxarithmetic-shift-right
  ;; 			(fxand v #b101010101010101010101010101010101010101010101010101010101010) 1)
  ;; 		       (fxarithmetic-shift-left
  ;; 			(fxand v #b010101010101010101010101010101010101010101010101010101010101) 1)))
  ;; 	     ;; Swap 2-bit fields
  ;; 	     (v (fxior (fxarithmetic-shift-right
  ;; 			(fxand v #b110011001100110011001100110011001100110011001100110011001100) 2)
  ;; 		       (fxarithmetic-shift-left
  ;; 			(fxand v #b001100110011001100110011001100110011001100110011001100110011) 2)))
  ;; 	     ;; Swap 4-bit fields
  ;; 	     (tmp1     (fxarithmetic-shift-right
  ;; 			(fxand v #b111100000000000000000000000000000000000000000000000000000000) 56))
  ;; 	     (v (fxior (fxarithmetic-shift-right
  ;; 			(fxand v #b000011110000111100001111000011110000111100001111000011110000) 4)
  ;; 		       (fxarithmetic-shift-left
  ;; 			(fxand v #b000000001111000011110000111100001111000011110000111100001111) 4)))
  ;; 	     ;; Swap bytes
  ;; 	     (tmp2     (fxarithmetic-shift-right
  ;; 			(fxand v #b000011111111000000000000000000000000000000000000000000000000) 44))
  ;; 	     (v (fxior (fxarithmetic-shift-right
  ;; 			(fxand v #b111100000000111111110000000011111111000000001111111100000000) 8)
  ;; 		       (fxarithmetic-shift-left
  ;; 			(fxand v #b000000000000000000001111111100000000111111110000000011111111) 8)))
  ;; 	     ;; Swap 16-bit fields
  ;; 	     (tmp3     (fxarithmetic-shift-right
  ;; 			(fxand v #b000000000000111111111111111100000000000000000000000000000000) 20))
  ;; 	     (v (fxior (fxarithmetic-shift-right
  ;; 			(fxand v #b111111111111000000000000000011111111111111110000000000000000) 16)
  ;; 		       (fxarithmetic-shift-left
  ;; 			(fxand v #b000000000000000000000000000000000000000000001111111111111111) 16))))
  ;; 	;; Put together the pieces
  ;; 	(fxior (fxarithmetic-shift-left v 28)
  ;; 	       tmp1 tmp2 tmp3)))

  (define (%fxreverse-bit-field30 v)
    (assert (= (fixnum-width) 30))
    (let* ( ;; Swap pairs of bits
	   (tmp1     (fxarithmetic-shift-right (fxand v #b10000000000000000000000000000) 28))
	   (v (fxior (fxarithmetic-shift-right (fxand v #b01010101010101010101010101010) 1)
		     (fxarithmetic-shift-left  (fxand v #b00101010101010101010101010101) 1)))
	   ;; Swap 2-bit fields
	   (v (fxior (fxarithmetic-shift-right (fxand v #b01100110011001100110011001100) 2)
		     (fxarithmetic-shift-left  (fxand v #b10011001100110011001100110011) 2)))
	   ;; Swap 4-bit fields
	   (tmp2     (fxarithmetic-shift-right (fxand v #b01111000000000000000000000000) 23))
	   (v (fxior (fxarithmetic-shift-right (fxand v #b10000111100001111000011110000) 4)
		     (fxarithmetic-shift-left  (fxand v #b00000000011110000111100001111) 4)))
	   ;; Swap bytes
	   (tmp3     (fxarithmetic-shift-right (fxand v #b00000111111110000000000000000) 11))
	   (v (fxior (fxarithmetic-shift-right (fxand v #b11111000000001111111100000000) 8)
		     (fxarithmetic-shift-left  (fxand v #b00000000000000000000011111111) 8))))
      ;; Put together the pieces
      (fxior (fxarithmetic-shift-left v 13)
	     tmp1 tmp2 tmp3)))

  (define who 'fxreverse-bit-field)
  (unless (fixnum? v)
    (assertion-violation who "expected fixnum as first argument" v))
  (unless (and (integer? start) (exact? start))
    (assertion-violation who "expected exact integer as second argument" start))
  (unless (and (integer? end) (exact? end))
    (assertion-violation who "expected exact integer as third argument" end))
  (unless (< -1 start (fixnum-width))
    (assertion-violation who
      (string-append "expected second argument between zero (included) and fixnum width "
		     (number->string (fixnum-width)))
      start))
  (unless (< -1 end (fixnum-width))
    (assertion-violation who
      (string-append "expected third argument between zero (included) and fixnum width "
		     (number->string (fixnum-width)))
      end))
  (unless (<= 0 start end)
    (assertion-violation who
      "expected second argument between zero (included) and third argument"
      start end))

  ;;This is from  the original patch by Göran  Weinholt, posted on the
  ;;Ikarus bug tracker.
  ;;
  ;; (cond ((= (fixnum-width) 61)
  ;;        (fxior (fxarithmetic-shift-right
  ;;                (%fxreverse-bit-field61 (fxbit-field v start end))
  ;;                (fx- 60 end))
  ;;               (fxcopy-bit-field v start end 0)))
  ;;       ((= (fixnum-width) 30)
  ;;        (fxior (fxarithmetic-shift-right
  ;;                (%fxreverse-bit-field30 (fxbit-field v start end))
  ;;                (fx- 29 end))
  ;;               (fxcopy-bit-field v start end 0)))
  ;;       (else
  ;;        (do ((i start (fx+ i 1))
  ;;             (ret 0 (if (fxbit-set? v i)
  ;;                        (fxior ret (fxarithmetic-shift-left 1 (fx- (fx- end i) 1)))
  ;; 			 ret)))
  ;;            ((fx=? i end)
  ;;             (fxior (fxarithmetic-shift-left ret start)
  ;;                    (fxcopy-bit-field v start end 0))))))

  ;;We have FIXNUM-WIDTH equal to 30.
  (fxior (fxarithmetic-shift-right
	  (%fxreverse-bit-field30 (fxbit-field v start end))
	  (fx- 29 end))
	 (fxcopy-bit-field v start end 0)))


(define (fxbit-field x i j)
  (define who 'fxbit-field)
  (if (fixnum? x)
      (if (fixnum? i)
	  (if ($fx<= 0 i)
	      (if (fixnum? j)
		  (if ($fx< j (fixnum-width))
		      (if ($fx<= i j)
			  ($fxsra
			   ($fxlogand x ($fxsub1 ($fxsll 1 j)))
			   i)
			(if ($fx<= 0 j)
			    (assertion-violation who "index out of range" j)
			  (assertion-violation who "indices not in order" i j)))
		    (assertion-violation who "index out of range" j))
		(assertion-violation who "not a fixnum" j))
	    (assertion-violation who "index out of range" i))
	(assertion-violation who "not a fixnum" i))
    (assertion-violation who "not a fixnum" x)))


;;;; done

)

;;; end of file
