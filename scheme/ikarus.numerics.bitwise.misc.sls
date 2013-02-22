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
  (import (except (ikarus)
		  bitwise-bit-set?		bitwise-first-bit-set
		  bitwise-bit-count
		  fxfirst-bit-set		fxbit-count
		  fxlength			fxbit-set?
		  fxcopy-bit			fxcopy-bit-field
		  fxrotate-bit-field		fxreverse-bit-field
		  fxbit-field)
    (except (ikarus system $fx)
	    $fxcopy-bit
	    $fxcopy-bit-field
	    $fxrotate-bit-field
	    $fxbit-field)
    (ikarus system $bignums)
    (ikarus system $flonums)
    (except (vicare syntactic-extensions)
	    case-word-size)
    (vicare arguments validation))

  ;;Remember  that WORDSIZE  is  the  number of  bytes  in a  platform's
  ;;machine word: 4 on 32-bit platforms, 8 on 64-bit platforms.
  (module (wordsize)
    (import (vicare include))
    (include/verbose "ikarus.config.ss"))


;;;; helpers

(define-syntax case-word-size
  ;;We really  need to define  this macro so that  it uses the  value of
  ;;WORDSIZE just defined by the "ikarus.config.ss" file.
  ;;
  (syntax-rules ()
    ((_ ((32) . ?body-32) ((64) . ?body-64))
     (if (= 4 wordsize)
	 (begin . ?body-32)
       (begin . ?body-64)))))


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


(module (fxlength)

  (define who 'fxlength)

  (define (fxlength x)
    (with-arguments-validation (who)
	((fixnum x))
      (let ((x^ (if ($fx< x 0)
		    ($fxlognot x)
		  x)))
	(case-word-size
	 ((32)
	  (%fxlength32 x^))
	 ((64)
	  (%fxlength64 x^))))))

  (define (%fxlength32 x)
    (let* ((fl  ($fixnum->flonum x))
	   (sbe ($fxlogor ($fxsll ($flonum-u8-ref fl 0) 4)
			  ($fxsra ($flonum-u8-ref fl 1) 4))))
      (if ($fx= sbe 0)
	  0
	($fx- sbe 1022))))

  (define (%fxlength64 x)
    (if ($fx> x #x7FFFFFFF)
	($fx+ 31 (%fxlength32 ($fxsra x 31)))
      (%fxlength32 x)))

  #| end of module: fxlength |# )


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


(module (fxcopy-bit
	 $fxcopy-bit)
  (define who 'fxcopy-bit)

  (define (fxcopy-bit x i b)
    (with-arguments-validation (who)
	((fixnum	x)
	 (fixnum	i)
	 (fixnum	b)
	 ($bit-index	i))
      ($fxcopy-bit x i b)))

  (define ($fxcopy-bit x i b)
    (case-fixnums b
      ((0)
       ($fxlogand x ($fxlognot ($fxsll 1 i))))
      ((1)
       ($fxlogor x ($fxsll 1 i)))
      (else
       (assertion-violation who "invalid bit value" b))))

  (define-argument-validation ($bit-index who obj)
    (and ($fx>= obj 0)
	 ($fx<  obj (fixnum-width)))
    (assertion-violation who "bit index out of range" obj))

  #| end of module: fxcopy-bit |# )


(module (fxcopy-bit-field
	 $fxcopy-bit-field)
  (define who 'fxcopy-bit-field)

  (define (fxcopy-bit-field x i j b)
    (with-arguments-validation (who)
	((fixnum		x)
	 (non-negative-fixnum	i)
	 ($bit-index		i)
	 (fixnum		j)
	 ($bit-index		j)
	 ($bit-indexes-in-order i j)
	 (fixnum		b))
      ($fxcopy-bit-field x i j b)))

  (define ($fxcopy-bit-field x i j b)
    (let ((m ($fxlogxor ($fxsub1 ($fxsll 1 i))
			($fxsub1 ($fxsll 1 j)))))
      ($fxlogor ($fxlogand m ($fxsll b i))
		($fxlogand ($fxlognot m) x))))

  (define-argument-validation ($bit-index who obj)
    (and ($fx>= obj 0)
	 ($fx<  obj (fixnum-width)))
    (assertion-violation who "bit index out of range" obj))

  (define-argument-validation ($bit-indexes-in-order who i j)
    ($fx<= i j)
    (assertion-violation who "bit indexes not in order" i j))

  #| end of module: fxcopy-bit-field |# )


(module (fxrotate-bit-field
	 $fxrotate-bit-field)
  (define who 'fxrotate-bit-field)

  (define (fxrotate-bit-field x i j c)
    ;;FIXME  This must  be checked  and eventually  updated from  the R6RS
    ;;errata (Marco Maggi; Nov 5, 2011).
    ;;
    (with-arguments-validation (who)
	((fixnum		x)
	 (non-negative-fixnum	i)
	 (fixnum		j)
	 ($bit-index-positive-limit	j))
      (let ((w ($fx- j i)))
	(with-arguments-validation (who)
	    (($field-width	w i j)
	     (fixnum		c)
	     ($count		c w))
	  ($fxrotate-bit-field x i j c w)))))

  (define ($fxrotate-bit-field x i j c w)
    (let* ((m  ($fxsll ($fxsub1 ($fxsll 1 w)) i))
	   (x0 ($fxlogand x m))
	   (lt ($fxsll x0 c))
	   (rt ($fxsra x0 ($fx- w c)))
	   (x0 ($fxlogand ($fxlogor lt rt) m)))
      ($fxlogor x0 ($fxlogand x ($fxlognot m)))))

  (define-argument-validation ($bit-index-positive-limit who obj)
    ($fx< obj (fixnum-width))
    (assertion-violation who "bit index out of range" obj))

  (define-argument-validation ($field-width who w i j)
    ($fx>= w 0)
    (assertion-violation who "field width is negative" i j))

  (define-argument-validation ($count who c w)
    (and ($fx>= c 0)
	 ($fx<  c w))
    (assertion-violation who "count is invalid" c))

  #| end of module: fxrotate-bit-field |# )


(module (fxreverse-bit-field)
  (define who 'fxreverse-bit-field)

  (define (fxreverse-bit-field v start end)
    (with-arguments-validation (who)
	((fixnum		v)
	 (fixnum		start)
	 (fixnum		end)
	 ($bit-index		start)
	 ($bit-index		end)
	 ($bit-index-order	start end))
      (case-fixnums (fixnum-width)
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
		     (fxcopy-bit-field v start end 0))))))))

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

  (define (%fxreverse-bit-field61 v)
    ;; Based on <http://aggregate.org/MAGIC/#Bit Reversal>.
    (assert (= (fixnum-width) 61))
    (let* ( ;; Swap pairs of bits
	   (v (fxior
	       (fxarithmetic-shift-right
                (fxand v #b101010101010101010101010101010101010101010101010101010101010)
                1)
	       (fxarithmetic-shift-left
                (fxand v #b010101010101010101010101010101010101010101010101010101010101)
                1)))
	   ;; Swap 2-bit fields
	   (v (fxior
	       (fxarithmetic-shift-right
                (fxand v #b110011001100110011001100110011001100110011001100110011001100)
                2)
	       (fxarithmetic-shift-left
                (fxand v #b001100110011001100110011001100110011001100110011001100110011)
                2)))
	   ;; Swap 4-bit fields
	   (tmp1 (fxarithmetic-shift-right
		  (fxand v #b111100000000000000000000000000000000000000000000000000000000)
		  56))
	   (v (fxior
	       (fxarithmetic-shift-right
                (fxand v #b000011110000111100001111000011110000111100001111000011110000)
                4)
	       (fxarithmetic-shift-left
                (fxand v #b000000001111000011110000111100001111000011110000111100001111)
                4)))
	   ;; Swap bytes
	   (tmp2 (fxarithmetic-shift-right
                  (fxand v #b000011111111000000000000000000000000000000000000000000000000)
                  44))
	   (v (fxior
               (fxarithmetic-shift-right
		(fxand v #b111100000000111111110000000011111111000000001111111100000000)
		8)
               (fxarithmetic-shift-left
		(fxand v #b000000000000000000001111111100000000111111110000000011111111)
		8)))
	   ;; Swap 16-bit fields
	   (tmp3 (fxarithmetic-shift-right
                  (fxand v #b000000000000111111111111111100000000000000000000000000000000)
                  20))
	   (v (fxior
               (fxarithmetic-shift-right
		(fxand v #b111111111111000000000000000011111111111111110000000000000000)
		16)
               (fxarithmetic-shift-left
		(fxand v #b000000000000000000000000000000000000000000001111111111111111)
		16))))
      ;; Put together the pieces
      (fxior (fxarithmetic-shift-left v 28)
	     tmp1 tmp2 tmp3)))

;;; --------------------------------------------------------------------

  (define-argument-validation ($bit-index who obj)
    (and ($fx>= obj 0)
	 ($fx<  obj (fixnum-width)))
    (assertion-violation who "bit index out of range" obj))

  (define-argument-validation ($bit-index-order who start end)
    ($fx<= start end)
    (assertion-violation who
      "expected second argument less than, or equal to, third argument" start end))

  #| end of module: fxreverse-bit-field |# )


(module (fxbit-field
	 $fxbit-field)
  (define who 'fxbit-field)

  (define (fxbit-field x i j)
    (with-arguments-validation (who)
	((fixnum		x)
	 (fixnum		i)
	 ($bit-index		i)
	 (fixnum		j)
	 ($bit-index		j)
	 ($bit-index-order	i j))
      ($fxbit-field x i j)))

  (define ($fxbit-field x i j)
    ($fxsra ($fxlogand x ($fxsub1 ($fxsll 1 j)))
	    i))

  (define-argument-validation ($bit-index who obj)
    (and ($fx>= obj 0)
	 ($fx<  obj (fixnum-width)))
    (assertion-violation who "bit index out of range" obj))

  (define-argument-validation ($bit-index-order who i j)
    ($fx<= i j)
    (assertion-violation who
      "expected second argument less than, or equal to, third argument" i j))

  #| end of module: fxbit-field |#)


;;;; done

)

;;; end of file
