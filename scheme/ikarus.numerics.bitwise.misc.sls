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
    fxbit-field

    $fxcopy-bit			$fxcopy-bit-field
    $fxrotate-bit-field		$fxbit-field)
  (import (except (vicare)
		  fixnum-width
		  greatest-fixnum
		  least-fixnum

		  bitwise-bit-set?		bitwise-first-bit-set
		  bitwise-bit-count
		  fxfirst-bit-set		fxbit-count
		  fxlength			fxbit-set?
		  fxcopy-bit			fxcopy-bit-field
		  fxrotate-bit-field		fxreverse-bit-field
		  fxbit-field)
    (except (vicare system $fx)
	    $fxcopy-bit
	    $fxcopy-bit-field
	    $fxrotate-bit-field
	    $fxbit-field)
    (vicare system $bignums)
    (vicare system $flonums))

  (include "ikarus.wordsize.scm" #t)


;;;; helpers

(define (non-negative-bit-index-in-fixnum-representation? obj)
  (and (fixnum? obj)
       ($fxnonnegative? obj)
       ($fx< obj (fixnum-width))))

(define (bit-value? obj)
  (or (eq? obj 0)
      (eq? obj 1)))


(module (bitwise-first-bit-set
	 fxfirst-bit-set)

  (define (byte-first-bit-set x i)
    (import (vicare system $bytevectors))
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

  (define* (fxfirst-bit-set {x fixnum?})
    ($fxfirst-bit-set x))

  (define* (bitwise-first-bit-set x)
    (cond ((fixnum? x)
	   ($fxfirst-bit-set x))
	  ((bignum? x)
	   ($bnloop x 0 0))
	  (else
	   (procedure-argument-violation __who__ "expected exact integer as argument" x))))

  #| end of module: |# )


(module (fxbit-count bitwise-bit-count)

  (define (pos-fxbitcount n)
      ;;; nifty parrallel count from:
      ;;; http://infolab.stanford.edu/~manku/bitcount/bitcount.html
    (case-word-size
     ((32)
      (let ((m0 #x15555555)
	    (m1 #x13333333)
	    (m2 #x0f0f0f0f))
	(let* ((n ($fx+ ($fxlogand n m0) ($fxlogand ($fxsra n 1) m0)))
	       (n ($fx+ ($fxlogand n m1) ($fxlogand ($fxsra n 2) m1)))
	       (n ($fx+ ($fxlogand n m2) ($fxlogand ($fxsra n 4) m2))))
	  (fxmodulo n 255))))
     ((64)
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

  (define* (fxbit-count {n fixnum?})
    ;;FIXME To be checked against R6RS errata (Marco Maggi; Nov 5, 2011).
    ;;
    ($fxbitcount n))

  (define* (bitwise-bit-count n)
    (cond ((fixnum? n)
	   ($fxbitcount n))
	  ((bignum? n)
	   (bnbitcount n))
	  (else
	   (procedure-argument-violation __who__ "expected exact integer as argument" n))))

  #| end of module |# )


(module (fxlength)

  (define* (fxlength {x fixnum?})
    (let ((x^ (if ($fxnegative? x)
		  ($fxlognot x)
		x)))
      (case-word-size
       ((32)
	(%fxlength32 x^))
       ((64)
	(%fxlength64 x^)))))

  (define (%fxlength32 x)
    (let* ((fl  ($fixnum->flonum x))
	   (sbe ($fxlogor ($fxsll ($flonum-u8-ref fl 0) 4)
			  ($fxsra ($flonum-u8-ref fl 1) 4))))
      (if ($fx= sbe 0)
	  0
	($fx- sbe 1022))))

  (define (%fxlength64 x)
    ;;NOTE The constant #x7FFFFFFF  is a fixnum on 64-bit platforms,  but a bignum on
    ;;32-bit platforms.  For this  reason we use > here to  compare, rather than fx>.
    ;;(Marco Maggi; Tue Nov 11, 2014)
    (if (> x #x7FFFFFFF)
	($fx+ 31 (%fxlength32 ($fxsra x 31)))
      (%fxlength32 x)))

  #| end of module: fxlength |# )


(define* (fxbit-set? {x fixnum?} {i non-negative-bit-index-in-fixnum-representation?})
  ;;Updated to the R6RS errata (Marco Maggi; Nov 5, 2011).
  ;;
  (if ($fx>= i ($fxsub1 (fixnum-width)))
      ($fx< x 0)
    (not ($fxzero? ($fxlogand ($fxsra x i) 1)))))

(define* (bitwise-bit-set? x i)
  (cond ((fixnum? i)
	 (when ($fxnegative? i)
	   (procedure-argument-violation __who__ "index must be non-negative" i))
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
		(procedure-argument-violation __who__ "not an exact integer" x))))
	((bignum? i)
	 (unless ($bignum-positive? i)
	   (procedure-argument-violation __who__ "index must be non-negative" i))
	 (cond ((fixnum? x)
		($fxnegative? x))
	       ((bignum? x)
		(= 1 (bitwise-and (bitwise-arithmetic-shift-right x i) 1)))
	       (else
		(procedure-argument-violation __who__ "not an exact integer" x))))
	(else
	 (procedure-argument-violation __who__ "index is not an exact integer" i))))


(module (fxcopy-bit
	 $fxcopy-bit)

  (define* (fxcopy-bit {x fixnum?} {i non-negative-bit-index-in-fixnum-representation?} {b bit-value?})
    ($fxcopy-bit x i b))

  (define* ($fxcopy-bit x i b)
    (case b
      ((0)
       ($fxlogand x ($fxlognot ($fxsll 1 i))))
      ((1)
       ($fxlogor x ($fxsll 1 i)))
      (else
       (procedure-argument-violation __who__ "invalid bit value" b))))

  #| end of module: fxcopy-bit |# )

;;; --------------------------------------------------------------------

(define* (fxcopy-bit-field {x fixnum?}
			   {i non-negative-bit-index-in-fixnum-representation?}
			   {j non-negative-bit-index-in-fixnum-representation?}
			   {b fixnum?})
  (unless ($fx<= i j)
    (procedure-arguments-consistency-violation __who__ "bit indexes not in order" i j))
  ($fxcopy-bit-field x i j b))

(define ($fxcopy-bit-field x i j b)
  (let ((m ($fxlogxor ($fxsub1 ($fxsll 1 i))
		      ($fxsub1 ($fxsll 1 j)))))
    ($fxlogor ($fxlogand m ($fxsll b i))
	      ($fxlogand ($fxlognot m) x))))

;;; --------------------------------------------------------------------

(define* (fxrotate-bit-field {x fixnum?}
			     {i non-negative-bit-index-in-fixnum-representation?}
			     {j non-negative-bit-index-in-fixnum-representation?}
			     {c non-negative-bit-index-in-fixnum-representation?})
  ;;FIXME This must  be checked and eventually updated from  the R6RS errata (Marco
  ;;Maggi; Nov 5, 2011).
  ;;
  (let ((w ($fx- j i)))
    (unless (and ($fxnonnegative? c)
		 ($fx< c w))
      (procedure-arguments-consistency-violation __who__ "count is invalid" i j c))
    (unless ($fxnonnegative? w)
      (procedure-arguments-consistency-violation __who__ "field width is negative" i j))
    ($fxrotate-bit-field x i j c w)))

(define ($fxrotate-bit-field x i j c w)
  (let* ((m  ($fxsll ($fxsub1 ($fxsll 1 w)) i))
	 (x0 ($fxlogand x m))
	 (lt ($fxsll x0 c))
	 (rt ($fxsra x0 ($fx- w c)))
	 (x0 ($fxlogand ($fxlogor lt rt) m)))
    ($fxlogor x0 ($fxlogand x ($fxlognot m)))))


(module (fxreverse-bit-field)

  (define* (fxreverse-bit-field {v fixnum?}
				{start non-negative-bit-index-in-fixnum-representation?}
				{end   non-negative-bit-index-in-fixnum-representation?})
    (unless ($fx<= start end)
      (procedure-arguments-consistency-violation __who__
	"expected second argument less than, or equal to, third argument" start end))
    (case (fixnum-width)
      ((61)
       (fxior (fxarithmetic-shift-right (%fxreverse-bit-field61 (fxbit-field v start end))
					(fx- 60 end))
	      (fxcopy-bit-field v start end 0)))
      ((30)
       (fxior (fxarithmetic-shift-right (%fxreverse-bit-field30 (fxbit-field v start end))
					(fx- 29 end))
	      (fxcopy-bit-field v start end 0)))
      (else
       (do ((i start (fx+ i 1))
	    (ret 0 (if (fxbit-set? v i)
		       (fxior ret (fxarithmetic-shift-left 1 (fx- (fx- end i) 1)))
		     ret)))
	   ((fx=? i end)
	    (fxior (fxarithmetic-shift-left ret start)
		   (fxcopy-bit-field v start end 0)))))))

  (define (%fxreverse-bit-field30 v)
    (assert (= (fixnum-width) 30))
    (let* ( ;; Swap pairs of bits
;;;                                                               2         1         0
;;;                                                       87654321098765432109876543210
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

  (define (%fxreverse-bit-field61 v)
    ;; Based on <http://aggregate.org/MAGIC/#Bit Reversal>.
    ;;
    ;;NOTE The constants in this function are fixnums on 64-bit platforms and bignums
    ;;on 32-bit  platforms.  For this  reason we  use the BITWISE-*  functions rather
    ;;than the FX* ones.  (Marco Maggi; Tue Nov 11, 2014)
    (assert (= (fixnum-width) 61))
    (let* ( ;; Swap pairs of bits
	   (v (bitwise-ior
	       (bitwise-arithmetic-shift-right
;;;                                       5         4         3         2         1         0
;;;                              987654321098765432109876543210987654321098765432109876543210
                (bitwise-and v #b101010101010101010101010101010101010101010101010101010101010)
                1)
	       (bitwise-arithmetic-shift-left
                (bitwise-and v #b010101010101010101010101010101010101010101010101010101010101)
                1)))
	   ;; Swap 2-bit fields
	   (v (bitwise-ior
	       (bitwise-arithmetic-shift-right
                (bitwise-and v #b110011001100110011001100110011001100110011001100110011001100)
                2)
	       (bitwise-arithmetic-shift-left
                (bitwise-and v #b001100110011001100110011001100110011001100110011001100110011)
                2)))
	   ;; Swap 4-bit fields
	   (tmp1 (bitwise-arithmetic-shift-right
		  (bitwise-and v #b111100000000000000000000000000000000000000000000000000000000)
		  56))
	   (v (bitwise-ior
	       (bitwise-arithmetic-shift-right
                (bitwise-and v #b000011110000111100001111000011110000111100001111000011110000)
                4)
	       (bitwise-arithmetic-shift-left
                (bitwise-and v #b000000001111000011110000111100001111000011110000111100001111)
                4)))
	   ;; Swap bytes
	   (tmp2 (bitwise-arithmetic-shift-right
                  (bitwise-and v #b000011111111000000000000000000000000000000000000000000000000)
                  44))
	   (v (bitwise-ior
               (bitwise-arithmetic-shift-right
		(bitwise-and v #b111100000000111111110000000011111111000000001111111100000000)
		8)
               (bitwise-arithmetic-shift-left
		(bitwise-and v #b000000000000000000001111111100000000111111110000000011111111)
		8)))
	   ;; Swap 16-bit fields
	   (tmp3 (bitwise-arithmetic-shift-right
                  (bitwise-and v #b000000000000111111111111111100000000000000000000000000000000)
                  20))
	   (v (bitwise-ior
               (bitwise-arithmetic-shift-right
		(bitwise-and v #b111111111111000000000000000011111111111111110000000000000000)
		16)
               (bitwise-arithmetic-shift-left
		(bitwise-and v #b000000000000000000000000000000000000000000001111111111111111)
		16))))
      ;; Put together the pieces
      (bitwise-ior (bitwise-arithmetic-shift-left v 28)
		   tmp1 tmp2 tmp3)))

  #| end of module: fxreverse-bit-field |# )


(define* (fxbit-field {x fixnum?}
		      {i non-negative-bit-index-in-fixnum-representation?}
		      {j non-negative-bit-index-in-fixnum-representation?})
  (unless ($fx<= i j)
    (procedure-arguments-consistency-violation __who__
      "expected second argument less than, or equal to, third argument" i j))
  ($fxbit-field x i j))

(define ($fxbit-field x i j)
  ($fxsra ($fxlogand x ($fxsub1 ($fxsll 1 j)))
	  i))


;;;; done

#| end of library |# )

;;; end of file
