;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus transcoders)
  (export string->utf8		utf8->string
	  string->utf16		utf16->string
	  string->utf16le	utf16le->string
	  string->utf16be	utf16be->string
	  string->utf16n	utf16n->string
	  string->utf32		utf32->string
	  string->bytevector	bytevector->string

	  utf8->string-length	string->utf8-length)
  (import (except (vicare)
		  string->utf8		utf8->string
		  string->utf16		utf16->string
		  string->utf16le	utf16le->string
		  string->utf16be	utf16be->string
		  string->utf16n	utf16n->string
		  string->utf32		utf32->string
                  string->bytevector	bytevector->string

		  utf8->string-length	string->utf8-length)
    (vicare system $strings)
    (vicare system $bytevectors)
    (vicare system $fx)
    (vicare system $chars)
    ;;See the documentation of this library for details on Unicode.
    (vicare unsafe unicode))


;;;; helpers

(define (endianness? obj)
  (memq obj '(little big)))

(define ($fxadd2 N)
  ($fx+ N 2))

(define ($fxadd3 N)
  ($fx+ N 3))

(define ($fxadd4 N)
  ($fx+ N 4))


(define (integer->char/invalid n)
  (cond ((not (fixnum? n))	#\xFFFD)
	(($fx<= n #xD7FF)	($fixnum->char n))
	(($fx<  n #xE000)	#\xFFFD)
	(($fx<= n #x10FFFF)	($fixnum->char n))
	(else			#\xFFFD)))


(module (string->utf8 string->utf8-length)

  (define* (string->utf8 {str string?})
    (let loop ((bv       ($make-bytevector (string->utf8-length str)))
	       (str      str)
	       (str.idx  0)
	       (bv.idx   0)
	       (str.len  ($string-length str)))
      (if ($fx= str.idx str.len)
	  bv
	(let ((code-point ($char->fixnum ($string-ref str str.idx))))
	  (cond (($fx<= code-point #x7F)
		 ;;CODE-POINT fits in a 1-octet sequence.
		 ($bytevector-set! bv bv.idx code-point)
		 (loop bv str ($fxadd1 str.idx) ($fxadd1 bv.idx) str.len))

		(($fx<= code-point #x7FF)
		 ;;CODE-POINT fits in a 2-octect sequence.
		 ($bytevector-set! bv bv.idx          ($fxlogor #b11000000 ($fxsra    code-point 6)))
		 ($bytevector-set! bv ($fx+ bv.idx 1) ($fxlogor #b10000000 ($fxlogand code-point #b111111)))
		 (loop bv str ($fxadd1 str.idx) ($fx+ bv.idx 2) str.len))

		;;CODE-POINT fits in a 3-octect sequence.
		(($fx<= code-point #xFFFF)
		 ($bytevector-set! bv bv.idx          ($fxlogor #b11100000 ($fxsra    code-point 12)))
		 ($bytevector-set! bv ($fx+ bv.idx 1) ($fxlogor #b10000000 ($fxlogand ($fxsra code-point 6) #b111111)))
		 ($bytevector-set! bv ($fx+ bv.idx 2) ($fxlogor #b10000000 ($fxlogand code-point #b111111)))
		 (loop bv str ($fxadd1 str.idx) ($fx+ bv.idx 3) str.len))

		;;CODE-POINT fits in a 4-octect sequence.
		(else
		 ($bytevector-set! bv bv.idx          ($fxlogor #b11110000 ($fxsra    code-point 18)))
		 ($bytevector-set! bv ($fx+ bv.idx 1) ($fxlogor #b10000000 ($fxlogand ($fxsra code-point 12) #b111111)))
		 ($bytevector-set! bv ($fx+ bv.idx 2) ($fxlogor #b10000000 ($fxlogand ($fxsra code-point  6) #b111111)))
		 ($bytevector-set! bv ($fx+ bv.idx 3) ($fxlogor #b10000000 ($fxlogand code-point #b111111)))
		 (loop bv str ($fxadd1 str.idx) ($fx+ bv.idx 4) str.len)))))))

  (define (string->utf8-length str)
    (let loop ((str str) (str.len ($string-length str)) (str.idx 0) (bv.len 0))
      (if ($fx= str.idx str.len)
	  bv.len
	(let ((code-point ($char->fixnum ($string-ref str str.idx))))
	  (loop str str.len ($fxadd1 str.idx)
		($fx+ bv.len (cond (($fx<= code-point #x7F)    1)
				   (($fx<= code-point #x7FF)   2)
				   (($fx<= code-point #xFFFF)  3)
				   (else                       4))))))))

  #| end of module |# )


(module (utf8->string utf8->string-length)

  (module (utf8->string)

    (case-define* utf8->string
      (({bv bytevector?})
       (%convert __who__ bv 'replace))
      (({bv bytevector?} {handling-mode error-handling-mode?})
       (%convert __who__ bv handling-mode)))

    (define (%convert who bv mode)
      (let* ((bv.start   (if (%has-bom? bv) 3 0))
	     (bv.end     ($bytevector-length bv))
	     (str        ($make-string (%compute-string-length who bv bv.start bv.end 0 mode)))
	     (str.start  0))
	(%convert-and-fill-string who bv bv.start bv.end str str.start mode)))

    #| end of module |# )

  (module (utf8->string-length)

    (case-define* utf8->string-length
      (({bv bytevector?})
       (%compute __who__ bv 'replace))
      (({bv bytevector?} {handling-mode error-handling-mode?})
       (%compute __who__ bv handling-mode)))

    (define (%compute who bv mode)
      (let ((bv.start   (if (%has-bom? bv) 3 0))
	    (bv.end     ($bytevector-length bv))
	    (accum-len  0))
	(%compute-string-length who bv bv.start bv.end accum-len mode)))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (error-handling-mode? obj)
    (or (eq? obj 'ignore)
	(eq? obj 'replace)
	(eq? obj 'raise)))

  (define (%has-bom? bv)
    (and ($fx>= ($bytevector-length bv) 3)
	 ($fx= ($bytevector-u8-ref bv 0) #xEF)
	 ($fx= ($bytevector-u8-ref bv 1) #xBB)
	 ($fx= ($bytevector-u8-ref bv 2) #xBF)))

;;; --------------------------------------------------------------------

  (define (%compute-string-length who bv bv.idx bv.end accum-len mode)
    (define-syntax-rule (%recurse bv.idx accum-len)
      (%compute-string-length who bv bv.idx bv.end accum-len mode))

    (define (%error-at-end)
      (case mode
	((ignore)
	 ;;When we ignore: we leave the computed string length unchanged.
	 accum-len)
	((replace)
	 ($fxadd1 accum-len))
	(else
	 (error who "invalid byte sequence near end of bytevector" bv))))

    (define (%error-at-index bv.idx bv.next-idx . irritants)
      (case mode
	((ignore)
	 ;;When we ignore: we leave the computed string length unchanged.
	 (%recurse bv.next-idx accum-len))
	((replace)
	 (%recurse bv.next-idx ($fxadd1 accum-len)))
	(else
	 (apply error who
		(string-append "invalid byte sequence at index " (number->string bv.idx) " of bytevector")
		bv irritants))))

    (if ($fx= bv.idx bv.end)
	accum-len
      (let ((octet0 ($bytevector-u8-ref bv bv.idx)))
	(cond ((utf-8-single-octet? octet0)
	       (%recurse ($fxadd1 bv.idx) ($fxadd1 accum-len)))

	      ((utf-8-first-of-two-octets? octet0)
	       (if ($fx< ($fxadd1 bv.idx) bv.end)
		   ;;Good, there is  still one octect which may be  the second in a
		   ;;2-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (bv.next-idx ($fxadd1 i1)))
		     (if (utf-8-second-of-two-octets? octet1)
			 (begin
			   (assert (let ((code-point (utf-8-decode-two-octets octet0 octet1)))
				     (utf-8-valid-code-point-from-2-octets? code-point)))
			   (%recurse bv.next-idx ($fxadd1 accum-len)))
		       (%error-at-index bv.idx bv.next-idx octet0 octet1)))
		 (%error-at-end)))

	      ((utf-8-first-of-three-octets? octet0)
	       (if ($fx< ($fxadd2 bv.idx) bv.end)
		   ;;Good, there are still two octects  which may be the second and
		   ;;third in a 3-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (i2     ($fxadd1 i1))
			  (octet2 ($bytevector-u8-ref bv i2))
			  (bv.next-idx ($fxadd1 i2)))
		     (if (utf-8-second-and-third-of-three-octets? octet1 octet2)
			 (begin
			   (assert (let ((code-point (utf-8-decode-three-octets octet0 octet1 octet2)))
				     (utf-8-valid-code-point-from-3-octets? code-point)))
			   (%recurse bv.next-idx ($fxadd1 accum-len)))
		       (%error-at-index bv.idx bv.next-idx octet0 octet1 octet2)))
		 (%error-at-end)))

	      ((utf-8-first-of-four-octets? octet0)
	       (if ($fx< ($fxadd3 bv.idx) bv.end)
		   ;;Good, there are  still three octects which may  be the second,
		   ;;third and fourth in a 4-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (i2     ($fxadd1 i1))
			  (octet2 ($bytevector-u8-ref bv i2))
			  (i3     ($fxadd1 i2))
			  (octet3 ($bytevector-u8-ref bv i3))
			  (bv.next-idx ($fxadd1 i3)))
		     (if (utf-8-second-third-and-fourth-of-four-octets? octet1 octet2 octet3)
			 (begin
			   (assert (let ((code-point (utf-8-decode-four-octets octet0 octet1 octet2 octet3)))
				     (utf-8-valid-code-point-from-4-octets? code-point)))
			   (%recurse bv.next-idx ($fxadd1 accum-len)))
		       (%error-at-index bv.idx bv.next-idx octet0 octet1 octet2 octet3)))
		 (%error-at-end)))

	      (else
	       (%error-at-end))))))

;;; --------------------------------------------------------------------

  (define (%convert-and-fill-string who bv bv.idx bv.end str str.idx mode)
    (define-syntax-rule (%recurse bv.idx str.idx)
      (%convert-and-fill-string who bv bv.idx bv.end str str.idx mode))

    (define (%error-at-end)
      (case mode
	((ignore)
	 ;;When we ignore: we  skip the invalid char.  This is  coherent with what we
	 ;;did when computing the length.
	 str)
	((replace)
	 ($string-set! str str.idx ($fixnum->char #xFFFD))
	 str)
	(else
	 (error who "invalid byte sequence near end of bytevector" bv))))

    (define (%error-at-index bv.idx bv.next-idx str.idx . irritants)
      (case mode
	((ignore)
	 ;;When we  ignore: we  skip the  invalid char  and leave  STR.IDX unchanged.
	 ;;This is coherent with what we did when computing the length.
	 (%recurse bv.next-idx str.idx))
	((replace)
	 ($string-set! str str.idx ($fixnum->char #xFFFD))
	 (%recurse bv.next-idx ($fxadd1 str.idx)))
	(else
	 (apply error who
		(string-append "invalid byte sequence at index " (number->string bv.idx) " of bytevector")
		bv irritants))))

    (if ($fx= bv.idx bv.end)
	str
      (let ((octet0 ($bytevector-u8-ref bv bv.idx)))
	(cond ((utf-8-single-octet? octet0)
	       ($string-set! str str.idx ($fixnum->char octet0))
	       (%recurse ($fxadd1 bv.idx) ($fxadd1 str.idx)))

	      ((utf-8-first-of-two-octets? octet0)
	       (if ($fx< ($fxadd1 bv.idx) bv.end)
		   ;;Good, there is  still one octect which may be  the second in a
		   ;;2-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (bv.next-idx ($fxadd1 i1)))
		     (if (utf-8-second-of-two-octets? octet1)
			 (begin
			   ($string-set! str str.idx ($fixnum->char (utf-8-decode-two-octets octet0 octet1)))
			   (%recurse bv.next-idx ($fxadd1 str.idx)))
		       (%error-at-index bv.idx bv.next-idx str.idx octet0 octet1)))
		 (%error-at-end)))

	      ((utf-8-first-of-three-octets? octet0)
	       (if ($fx< ($fxadd2 bv.idx) bv.end)
		   ;;Good, there are still two octects  which may be the second and
		   ;;third in a 3-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (i2     ($fxadd1 i1))
			  (octet2 ($bytevector-u8-ref bv i2))
			  (bv.next-idx ($fxadd1 i2)))
		     (if (utf-8-second-and-third-of-three-octets? octet1 octet2)
			 (begin
			   ($string-set! str str.idx ($fixnum->char (utf-8-decode-three-octets octet0 octet1 octet2)))
			   (%recurse bv.next-idx ($fxadd1 str.idx)))
		       (%error-at-index bv.idx bv.next-idx str.idx octet0 octet1 octet2)))
		 (%error-at-end)))

	      ((utf-8-first-of-four-octets? octet0)
	       (if ($fx< ($fxadd3 bv.idx) bv.end)
		   ;;Good, there are  still three octects which may  be the second,
		   ;;third and fourth in a 4-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (i2     ($fxadd1 i1))
			  (octet2 ($bytevector-u8-ref bv i2))
			  (i3     ($fxadd1 i2))
			  (octet3 ($bytevector-u8-ref bv i3))
			  (bv.next-idx ($fxadd1 i3)))
		     (if (utf-8-second-third-and-fourth-of-four-octets? octet1 octet2 octet3)
			 (begin
			   ($string-set! str str.idx ($fixnum->char (utf-8-decode-four-octets octet0 octet1 octet2 octet3)))
			   (%recurse bv.next-idx ($fxadd1 str.idx)))
		       (%error-at-index bv.idx bv.next-idx str.idx octet0 octet1 octet2 octet3)))
		 (%error-at-end)))

	      (else
	       (%error-at-end))))))

  #| end of module |# )


;;; From: http://tools.ietf.org/html/rfc2781
;;;
;;; 2.1 Encoding UTF-16
;;;
;;;   Encoding of a single character from an ISO 10646 character value
;;;   to UTF-16 proceeds as follows. Let U be the character number, no
;;;   greater than 0x10FFFF.
;;;
;;;   1) If U < 0x10000, encode U as a 16-bit unsigned integer and terminate.
;;;
;;;   2) Let U' = U - 0x10000. Because U is less than or equal to 0x10FFFF,
;;;      U' must be less than or equal to 0xFFFFF. That is, U' can be
;;;      represented in 20 bits.
;;;
;;;   3) Initialize two 16-bit unsigned integers, W1 and W2, to 0xD800 and
;;;      0xDC00, respectively. These integers each have 10 bits free to
;;;      encode the character value, for a total of 20 bits.
;;;
;;;   4) Assign the 10 high-order bits of the 20-bit U' to the 10 low-order
;;;      bits of W1 and the 10 low-order bits of U' to the 10 low-order
;;;      bits of W2. Terminate.
;;;
;;;   Graphically, steps 2 through 4 look like:
;;;   U' = yyyyyyyyyyxxxxxxxxxx
;;;   W1 = 110110yyyyyyyyyy
;;;   W2 = 110111xxxxxxxxxx
;;;
;;;   Decoding of a single character from UTF-16 to an ISO 10646 character
;;;   value proceeds as follows. Let W1 be the next 16-bit integer in the
;;;   sequence of integers representing the text. Let W2 be the (eventual)
;;;   next integer following W1.
;;;
;;;   1) If W1 < 0xD800 or W1 > 0xDFFF, the character value U is the value
;;;      of W1. Terminate.
;;;
;;;   2) Determine if W1 is between 0xD800 and 0xDBFF. If not, the sequence
;;;      is in error and no valid character can be obtained using W1.
;;;      Terminate.
;;;
;;;   3) If there is no W2 (that is, the sequence ends with W1), or if W2
;;;      is not between 0xDC00 and 0xDFFF, the sequence is in error.
;;;      Terminate.
;;;
;;;   4) Construct a 20-bit unsigned integer U', taking the 10 low-order
;;;      bits of W1 as its 10 high-order bits and the 10 low-order bits of
;;;      W2 as its 10 low-order bits.
;;;   5) Add 0x10000 to U' to obtain the character value U.
;;;      Terminate.

(define (utf16le->string bv)	(utf16->string bv (endianness little)))
(define (utf16be->string bv)	(utf16->string bv (endianness big)))
(define (utf16n->string  bv)	(utf16->string bv (native-endianness)))

(define (string->utf16le str)	(string->utf16 str (endianness little)))
(define (string->utf16be str)	(string->utf16 str (endianness big)))
(define (string->utf16n  str)	(string->utf16 str (native-endianness)))

(module (string->utf16)
  (define ($string->utf16 str endianness)
    (define (count-surr* str len i n)
      (cond
       ((fx= i len) n)
       (else
	(let ((c (string-ref str i)))
	  (cond
	   ((char<? c #\x10000)
	    (count-surr* str len (fx+ i 1) n))
	   (else
	    (count-surr* str len (fx+ i 1) (fx+ n 1))))))))
    (define (bvfill str bv i j len endianness)
      (cond
       ((fx= i len) bv)
       (else
	(let ((n (char->integer (string-ref str i))))
	  (cond
	   ((fx< n #x10000)
	    (bytevector-u16-set! bv j n endianness)
	    (bvfill str bv (fx+ i 1) (fx+ j 2) len endianness))
	   (else
	    (let ((u^ (fx- n #x10000)))
	      (bytevector-u16-set! bv j
				   (fxlogor (fxsll #b110110 10) (fxsra u^ 10))
				   endianness)
	      (bytevector-u16-set! bv (fx+ j 2)
				   (fxlogor (fxsll #b110111 10) (fxlogand u^ #x3FF))
				   endianness))
	    (bvfill str bv (fx+ i 1) (fx+ j 4) len endianness)))))))
    (let ((len ($string-length str)))
      (let ((n (count-surr* str len 0 0)))
          ;;; FIXME: maybe special case for n=0 later
	(let ((bv (make-bytevector (fxsll (fx+ len n) 1))))
	  (bvfill str bv 0 0 len endianness)))))
  (define string->utf16
    (case-lambda
     ((str)
      (unless (string? str)
	(die 'string->utf16 "not a string" str))
      ($string->utf16 str 'big))
     ((str endianness)
      (unless (string? str)
	(die 'string->utf16 "not a string" str))
      (unless (memv endianness '(big little))
	(die 'string->utf16 "invalid endianness" endianness))
      ($string->utf16 str endianness)))))

(module (utf16->string)
  (define who 'utf16->string)
  (define (count-size bv endianness i len n)
    (cond
     ((fx= i len)
      (if (fx= len (bytevector-length bv))
	  n
	(+ n 1)))
     (else
      (let ((w1 (bytevector-u16-ref bv i endianness)))
	(cond
	 ((or (fx< w1 #xD800) (fx> w1 #xDFFF))
	  (count-size bv endianness (+ i 2) len (+ n 1)))
	 ((not (fx<= #xD800 w1 #xDBFF)) ;;; error sequence
	  (count-size bv endianness (+ i 2) len (+ n 1)))
	 ((<= (+ i 4) (bytevector-length bv))
	  (let ((w2 (bytevector-u16-ref bv (+ i 2) endianness)))
	    (cond
	     ((not (<= #xDC00 w2 #xDFFF))
                   ;;; do we skip w2 also?
                   ;;; I won't.  Just w1 is an error
	      (count-size bv endianness (+ i 2) len (+ n 1)))
	     (else
                   ;;; 4-byte sequence is ok
	      (count-size bv endianness (+ i 4) len (+ n 1))))))
	 (else
              ;;; error again
	  (count-size bv endianness (+ i 2) len (+ n 1))))))))
  (define (fill bv endianness str i len n)
    (cond
     ((fx= i len)
      (unless (fx= len (bytevector-length bv))
	(string-set! str n #\xFFFD))
      str)
     (else
      (let ((w1 (bytevector-u16-ref bv i endianness)))
	(cond
	 ((or (fx< w1 #xD800) (fx> w1 #xDFFF))
	  (string-set! str n (integer->char/invalid w1))
	  (fill bv endianness str (+ i 2) len (+ n 1)))
	 ((not (fx<= #xD800 w1 #xDBFF)) ;;; error sequence
	  (string-set! str n #\xFFFD)
	  (fill bv endianness str (+ i 2) len (+ n 1)))
	 ((<= (+ i 4) (bytevector-length bv))
	  (let ((w2 (bytevector-u16-ref bv (+ i 2) endianness)))
	    (cond
	     ((not (<= #xDC00 w2 #xDFFF))
                   ;;; do we skip w2 also?
                   ;;; I won't.  Just w1 is an error
	      (string-set! str n #\xFFFD)
	      (fill bv endianness str (+ i 2) len (+ n 1)))
	     (else
	      (string-set! str n
			   (integer->char/invalid
			    (+ #x10000
			       (fxlogor (fxsll (fxlogand w1 #x3FF) 10)
					(fxlogand w2 #x3FF)))))
	      (fill bv endianness str (+ i 4) len (+ n 1))))))
	 (else
              ;;; error again
	  (string-set! str n #\xFFFD)
	  (fill bv endianness str (+ i 2) len (+ n 1))))))))
  (define (decode bv endianness start)
    (let ((len (fxand (bytevector-length bv) -2)))
      (let ((n (count-size bv endianness start len 0)))
	(let ((str (make-string n)))
	  (fill bv endianness str start len 0)))))
  (define ($utf16->string bv endianness em?)
    (define (bom-present bv)
      (and (fx>= (bytevector-length bv) 2)
	   (let ((n (bytevector-u16-ref bv 0 'big)))
	     (cond
	      ((fx= n #xFEFF) 'big)
	      ((fx= n #xFFFE) 'little)
	      (else #f)))))
    (unless (bytevector? bv)
      (die who "not a bytevector" bv))
    (unless (memv endianness '(big little))
      (die who "invalid endianness" endianness))
    (cond
     (em?  (decode bv endianness 0))
     ((bom-present bv) =>
      (lambda (endianness)
	(decode bv endianness 2)))
     (else
      (decode bv endianness 0))))
  (define utf16->string
    (case-lambda
     ((bv endianness)
      ($utf16->string bv endianness #f))
     ((bv endianness em?)
      ($utf16->string bv endianness em?)))))


(module (string->utf32)

  (case-define* string->utf32
    (({str string?})
     ($string->utf32 str 'big))

    (({str string?} {endianness endianness?})
     ($string->utf32 str endianness))

    #| end of CASE-DEFINE* |# )

  (define ($string->utf32 str endianness)
    (let ((len (string-length str)))
      (vfill str (make-bytevector (fxsll len 2)) 0 len endianness)))

  (define (vfill str bv i len endianness)
    (if (fx=? i len)
	bv
      (begin
	(bytevector-u32-set! bv
			     (fxsll i 2)
			     (char->integer (string-ref str i))
			     endianness)
	(vfill str bv (fx+ i 1) len endianness))))

  #| end of module: STRING->UTF32 |# )

(module (utf32->string)

  (case-define* utf32->string
    (({bv bytevector?} {endianness endianness?})
     ($utf32->string bv endianness #f))
    (({bv bytevector?} {endianness endianness?} em?)
     ($utf32->string bv endianness em?))
    #| end of CASE-DEFINE* |# )

  (define ($utf32->string bv endianness em?)
    (cond (em?
	   (%decode bv endianness 0))
	  ((%bom-present bv)
	   => (lambda (endianness)
		(%decode bv endianness 4)))
	  (else
	   (%decode bv endianness 0))))

  (define (%bom-present bv)
    (and (fx>= (bytevector-length bv) 4)
	 (let ((n (bytevector-u32-ref bv 0 'big)))
	   (cond ((= n #x0000FEFF)	'big)
		 ((= n #xFFFE0000)	'little)
		 (else			#f)))))

  (define (%decode bv endianness start)
    (let* ((bv.len  ($bytevector-length bv))
	   (str.len ($fxsra ($fx+ ($fx- bv.len start) 3) 2)))
      (%fill bv endianness (make-string str.len)
	     start (fxand bv.len -4)
	     0)))

  (define (%fill bv endianness str i j n)
    (cond ((fx= i j)
	   (unless (fx= n (string-length str))
	     (string-set! str n #\xFFFD))
	   str)
	  (else
	   (string-set! str n (integer->char/invalid (bytevector-u32-ref bv i endianness)))
	   (%fill bv endianness str (fx+ i 4) j (fx+ n 1)))))

  #| end of module: UTF32->STRING |# )


(define* (bytevector->string {bv bytevector?} {tran transcoder?})
  (call-with-port
      (open-bytevector-input-port bv tran)
    (lambda (port)
      (let ((r (get-string-all port)))
	(if (eof-object? r)
	    ""
	  r)))))

(define* (string->bytevector {str string?} {tran transcoder?})
  (call-with-bytevector-output-port
      (lambda (port)
        (put-string port str))
    tran))


;;;; done

#| end of library |# )

;;; end of file
