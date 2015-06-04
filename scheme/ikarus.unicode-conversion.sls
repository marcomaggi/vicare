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

	  utf8->string-length	string->utf8-length
	  utf16->string-length	string->utf16-length
	  utf32->string-length	string->utf32-length)
  (import (except (vicare)
		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Wed Jun 3, 2015)
		  make-utf16-string-decoding-invalid-first-word
		  make-utf16-string-decoding-invalid-second-word
		  make-utf16-string-decoding-missing-second-word
		  make-utf16-string-decoding-standalone-octet
		  ;;

		  string->utf8		utf8->string
		  string->utf16		utf16->string
		  string->utf16le	utf16le->string
		  string->utf16be	utf16be->string
		  string->utf16n	utf16n->string
		  string->utf32		utf32->string
                  string->bytevector	bytevector->string

		  utf8->string-length	string->utf8-length
		  utf16->string-length	string->utf16-length
		  utf32->string-length	string->utf32-length)
    (vicare system $strings)
    (except (vicare system $bytevectors)
	    ;;FIXME This  except is to  be removed at  the next boot  image rotation.
	    ;;(Marco Maggi; Tue Jun 2, 2015)
	    $bytevector-u16-ref
	    $bytevector-u16b-ref
	    $bytevector-u16l-ref
	    $bytevector-u32-set!
	    $bytevector-u32b-set!
	    $bytevector-u32l-set!
	    $bytevector-u32-ref
	    $bytevector-u32b-ref)
    (vicare system $fx)
    (vicare system $chars)
    ;;See the documentation of this library for details on Unicode.
    (prefix (vicare unsafe unicode) unicode.)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Wed Jun 3,
    ;;2015)
    (only (ikarus conditions)
	  make-utf16-string-decoding-invalid-first-word
	  make-utf16-string-decoding-invalid-second-word
	  make-utf16-string-decoding-missing-second-word
	  make-utf16-string-decoding-standalone-octet
	  make-utf32-string-decoding-invalid-word
	  make-utf32-string-decoding-orphan-octets))


;;;; helpers

(define (endianness? obj)
  (memq obj '(little big)))

(define (error-handling-mode? obj)
  (or (eq? obj 'ignore)
      (eq? obj 'replace)
      (eq? obj 'raise)))

(define ($fxadd2 N)
  ($fx+ N 2))

(define ($fxadd3 N)
  ($fx+ N 3))

(define ($fxadd4 N)
  ($fx+ N 4))

(define ($fxsub2 N)
  ($fx- N 2))

(define ($fxsub3 N)
  ($fx- N 3))

(define ($fxsub4 N)
  ($fx- N 4))

(define (%raise-condition who message cnd)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition message)
	      cnd)))

;;; --------------------------------------------------------------------

(define ($bytevector-u16b-ref bv index)
  ;;FIXME To be  removed at the next  boot image rotation.  (Marco Maggi;  Tue Jun 2,
  ;;2015)
  ($fxlogor
   ;; lowest memory location -> most significant byte
   ($fxsll ($bytevector-u8-ref bv index) 8)
   ;; highest memory location -> least significant byte
   ($bytevector-u8-ref bv ($fxadd1 index))))

(define ($bytevector-u16l-ref bv index)
  ;;FIXME To be  removed at the next  boot image rotation.  (Marco Maggi;  Tue Jun 2,
  ;;2015)
  ($fxlogor
   ;; highest memory location -> most significant byte
   ($fxsll ($bytevector-u8-ref bv ($fxadd1 index)) 8)
   ;; lowest memory location -> least significant byte
   ($bytevector-u8-ref bv index)))

(define ($bytevector-u16-ref bv index endianness)
  ;;FIXME To be  removed at the next  boot image rotation.  (Marco Maggi;  Tue Jun 2,
  ;;2015)
  (case endianness
    ((big)		($bytevector-u16b-ref bv index))
    ((little)		($bytevector-u16l-ref bv index))))

;;; --------------------------------------------------------------------

;;FIXME To  be removed at  the next  boot image rotation.   (Marco Maggi; Wed  Jun 3,
;;2015)
(module ($bytevector-u32-set!
	 $bytevector-u32b-set!
	 $bytevector-u32l-set!
	 $bytevector-u32-ref
	 $bytevector-u32b-ref)

  (define ($bytevector-u32-ref bv index endianness)
    (case endianness
      ((big)		($bytevector-u32b-ref bv index))
      ((little)		($bytevector-u32l-ref bv index))))

  (define ($bytevector-u32-set! bv index word endianness)
    (case endianness
      ((big)		($bytevector-u32b-set! bv index word))
      ((little)		($bytevector-u32l-set! bv index word))))

  (define ($bytevector-u32b-ref bv index)
    (+ (sll ($bytevector-u8-ref bv index) 24)
       ($fixnum-ior
	($fxsll ($bytevector-u8-ref bv ($fxadd1 index)) 16)
	($fxsll ($bytevector-u8-ref bv ($fx+ index 2))  8)
	($bytevector-u8-ref bv ($fx+ index 3)))))

  (define ($bytevector-u32b-set! bv index word)
    (let ((b (sra word 16)))
      ($bytevector-set! bv index ($fxsra b 8))
      ($bytevector-set! bv ($fxadd1 index) b))
    (let ((b (bitwise-and word #xFFFF)))
      ($bytevector-set! bv ($fx+ index 2) ($fxsra b 8))
      ($bytevector-set! bv ($fx+ index 3) b)))

  (define ($bytevector-u32l-ref bv index)
    (+ (sll ($bytevector-u8-ref bv ($fx+ index 3)) 24)
       ($fixnum-ior
	($fxsll ($bytevector-u8-ref bv ($fx+ index 2)) 16)
	($fxsll ($bytevector-u8-ref bv ($fxadd1 index)) 8)
	($bytevector-u8-ref bv index))))

  (define ($bytevector-u32l-set! bv index word)
    (let ((b (sra word 16)))
      ($bytevector-set! bv ($fx+ index 3) ($fxsra b 8))
      ($bytevector-set! bv ($fx+ index 2) b))
    (let ((b (bitwise-and word #xFFFF)))
      ($bytevector-set! bv ($fxadd1 index) ($fxsra b 8))
      ($bytevector-set! bv index b)))

  (define-syntax $fixnum-ior
    (syntax-rules ()
      ((_ ?op1)
       ?op1)
      ((_ ?op1 ?op2)
       ($fxlogor ?op1 ?op2))
      ((_ ?op1 ?op2 . ?ops)
       ($fxlogor ?op1 ($fixnum-ior ?op2 . ?ops)))))

  #| end of module |# )


(define (integer->char/invalid n)
  (cond ((not (fixnum? n))	#\xFFFD)
	(($fx<= n #xD7FF)	($fixnum->char n))
	(($fx<  n #xE000)	#\xFFFD)
	(($fx<= n #x10FFFF)	($fixnum->char n))
	(else			#\xFFFD)))


(module (string->utf8 string->utf8-length)

  (define* (string->utf8 {str string?})
    (define str.len
      ($string->utf8-length str))
    (unless (fixnum? str.len)
      (error __who__ "string too long for UTF-8 conversion" str))
    (let loop ((bv       ($make-bytevector str.len))
	       (bv.idx   0)
	       (str      str)
	       (str.idx  0)
	       (str.len  str.len))
      (if ($fx= str.idx str.len)
	  bv
	(let ((code-point ($char->fixnum ($string-ref str str.idx))))
	  (unicode.utf-8-case-code-point code-point
	    ((1)
	     ;;CODE-POINT fits in a 1-octet sequence.
	     ($bytevector-set! bv bv.idx (unicode.utf-8-encode-single-octet code-point))
	     (loop bv ($fxadd1 bv.idx) str ($fxadd1 str.idx) str.len))

	    ((2)
	     ;;CODE-POINT fits in a 2-octect sequence.
	     ($bytevector-set! bv bv.idx           (unicode.utf-8-encode-first-of-two-octets  code-point))
	     ($bytevector-set! bv ($fxadd1 bv.idx) (unicode.utf-8-encode-second-of-two-octets code-point))
	     (loop bv ($fxadd2 bv.idx) str ($fxadd1 str.idx) str.len))

	    ((3)
	     ;;CODE-POINT fits in a 3-octect sequence.
	     ($bytevector-set! bv bv.idx           (unicode.utf-8-encode-first-of-three-octets  code-point))
	     ($bytevector-set! bv ($fxadd1 bv.idx) (unicode.utf-8-encode-second-of-three-octets code-point))
	     ($bytevector-set! bv ($fxadd2 bv.idx) (unicode.utf-8-encode-third-of-three-octets  code-point))
	     (loop bv ($fxadd3 bv.idx) str ($fxadd1 str.idx) str.len))

	    ((4)
	     ;;CODE-POINT fits in a 4-octect sequence.
	     ($bytevector-set! bv bv.idx           (unicode.utf-8-encode-first-of-four-octets  code-point))
	     ($bytevector-set! bv ($fxadd1 bv.idx) (unicode.utf-8-encode-second-of-four-octets code-point))
	     ($bytevector-set! bv ($fxadd2 bv.idx) (unicode.utf-8-encode-third-of-four-octets  code-point))
	     ($bytevector-set! bv ($fxadd3 bv.idx) (unicode.utf-8-encode-fourth-of-four-octets code-point))
	     (loop bv ($fxadd4 bv.idx) str ($fxadd1 str.idx) str.len)))))))

  (define* (string->utf8-length {str string?})
    ($string->utf8-length str))

  (define ($string->utf8-length str)
    (let loop ((str str) (str.len ($string-length str)) (str.idx 0) (bv.len 0))
      (if ($fx= str.idx str.len)
	  bv.len
	(let* ((code-point ($char->fixnum ($string-ref str str.idx)))
	       (bv.len
		(+ bv.len (cond (($fx<= code-point #x7F)    1)
				(($fx<= code-point #x7FF)   2)
				(($fx<= code-point #xFFFF)  3)
				(else                       4)))
		;; (+ bv.len (unicode.utf-8-case-code-point code-point
		;; 	    ((1)	1)
		;; 	    ((2)	2)
		;; 	    ((3)	3)
		;; 	    ((4)	4)))
		))
	  (and (fixnum? bv.len)
	       (loop str str.len ($fxadd1 str.idx) bv.len))))))

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
	(cond ((unicode.utf-8-single-octet? octet0)
	       (%recurse ($fxadd1 bv.idx) ($fxadd1 accum-len)))

	      ((unicode.utf-8-first-of-two-octets? octet0)
	       (if ($fx< ($fxadd1 bv.idx) bv.end)
		   ;;Good, there is  still one octect which may be  the second in a
		   ;;2-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (bv.next-idx ($fxadd1 i1)))
		     (if (unicode.utf-8-second-of-two-octets? octet1)
			 (begin
			   (assert (let ((code-point (unicode.utf-8-decode-two-octets octet0 octet1)))
				     (unicode.utf-8-valid-code-point-from-2-octets? code-point)))
			   (%recurse bv.next-idx ($fxadd1 accum-len)))
		       (%error-at-index bv.idx bv.next-idx octet0 octet1)))
		 (%error-at-end)))

	      ((unicode.utf-8-first-of-three-octets? octet0)
	       (if ($fx< ($fxadd2 bv.idx) bv.end)
		   ;;Good, there are still two octects  which may be the second and
		   ;;third in a 3-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (i2     ($fxadd1 i1))
			  (octet2 ($bytevector-u8-ref bv i2))
			  (bv.next-idx ($fxadd1 i2)))
		     (if (unicode.utf-8-second-and-third-of-three-octets? octet1 octet2)
			 (begin
			   (assert (let ((code-point (unicode.utf-8-decode-three-octets octet0 octet1 octet2)))
				     (unicode.utf-8-valid-code-point-from-3-octets? code-point)))
			   (%recurse bv.next-idx ($fxadd1 accum-len)))
		       (%error-at-index bv.idx bv.next-idx octet0 octet1 octet2)))
		 (%error-at-end)))

	      ((unicode.utf-8-first-of-four-octets? octet0)
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
		     (if (unicode.utf-8-second-third-and-fourth-of-four-octets? octet1 octet2 octet3)
			 (begin
			   (assert (let ((code-point (unicode.utf-8-decode-four-octets octet0 octet1 octet2 octet3)))
				     (unicode.utf-8-valid-code-point-from-4-octets? code-point)))
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
	(cond ((unicode.utf-8-single-octet? octet0)
	       ($string-set! str str.idx ($fixnum->char octet0))
	       (%recurse ($fxadd1 bv.idx) ($fxadd1 str.idx)))

	      ((unicode.utf-8-first-of-two-octets? octet0)
	       (if ($fx< ($fxadd1 bv.idx) bv.end)
		   ;;Good, there is  still one octect which may be  the second in a
		   ;;2-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (bv.next-idx ($fxadd1 i1)))
		     (if (unicode.utf-8-second-of-two-octets? octet1)
			 (begin
			   ($string-set! str str.idx ($fixnum->char (unicode.utf-8-decode-two-octets octet0 octet1)))
			   (%recurse bv.next-idx ($fxadd1 str.idx)))
		       (%error-at-index bv.idx bv.next-idx str.idx octet0 octet1)))
		 (%error-at-end)))

	      ((unicode.utf-8-first-of-three-octets? octet0)
	       (if ($fx< ($fxadd2 bv.idx) bv.end)
		   ;;Good, there are still two octects  which may be the second and
		   ;;third in a 3-octet sequence.
		   (let* ((i1     ($fxadd1 bv.idx))
			  (octet1 ($bytevector-u8-ref bv i1))
			  (i2     ($fxadd1 i1))
			  (octet2 ($bytevector-u8-ref bv i2))
			  (bv.next-idx ($fxadd1 i2)))
		     (if (unicode.utf-8-second-and-third-of-three-octets? octet1 octet2)
			 (begin
			   ($string-set! str str.idx ($fixnum->char (unicode.utf-8-decode-three-octets octet0 octet1 octet2)))
			   (%recurse bv.next-idx ($fxadd1 str.idx)))
		       (%error-at-index bv.idx bv.next-idx str.idx octet0 octet1 octet2)))
		 (%error-at-end)))

	      ((unicode.utf-8-first-of-four-octets? octet0)
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
		     (if (unicode.utf-8-second-third-and-fourth-of-four-octets? octet1 octet2 octet3)
			 (begin
			   ($string-set! str str.idx ($fixnum->char (unicode.utf-8-decode-four-octets octet0 octet1 octet2 octet3)))
			   (%recurse bv.next-idx ($fxadd1 str.idx)))
		       (%error-at-index bv.idx bv.next-idx str.idx octet0 octet1 octet2 octet3)))
		 (%error-at-end)))

	      (else
	       (%error-at-end))))))

  #| end of module |# )


;;;; UTF-16 helpers

(define (utf16le->string bv)	(utf16->string bv (endianness little)))
(define (utf16be->string bv)	(utf16->string bv (endianness big)))
(define (utf16n->string  bv)	(utf16->string bv (native-endianness)))

(define (string->utf16le str)	(string->utf16 str (endianness little)))
(define (string->utf16be str)	(string->utf16 str (endianness big)))
(define (string->utf16n  str)	(string->utf16 str (native-endianness)))


(module (string->utf16 string->utf16-length)

  (case-define* string->utf16
    (({str string?})
     ($string->utf16 str 'big))

    (({str string?} {endianness endianness?})
     ($string->utf16 str endianness))

    #| end of CASE-DEFINE* |# )

  (define* (string->utf16-length {str string?})
    ($string->utf16-length str))

  (define ($string->utf16-length str)
    ;;The number of octets needed is:
    ;;
    ;;   2 * ((number of chars in STR) + (number of surrogate pairs))
    ;;
    (let* ((str.len ($string-length str))
	   ;;We must avoid overflow and check that the returned value is a fixnum!!!
	   (bv.len  (bitwise-arithmetic-shift-left (+ str.len (%count-surrogate-pairs str str.len 0 0)) 1)))
      (and (fixnum? bv.len)
	   bv.len)))

  (define* ($string->utf16 str endianness)
    (let ((str.len ($string-length str)))
      (if ($fxzero? str.len)
	  ;;Let's return a new bytevector.
	  ($make-bytevector 0)
	;;The number  of octets needed  is the number of  characters in STR  plus the
	;;number of surrogate pairs, times 2.
	(let ((bv.len ($string->utf16-length str)))
	  (if (fixnum? bv.len)
	      (let ((bv ($make-bytevector bv.len)))
		(%encode-and-fill-bytevector str 0 str.len bv 0 endianness))
	    (error __who__ "string too long for UTF-16 conversion" str))))))

  (define (%count-surrogate-pairs str str.len i accum-count)
    (if ($fx= i str.len)
	accum-count
      ;;Code points in the range [0, #x10000)  are encoded with a single UTF-16 word;
      ;;code points in the range [#x10000, #x10FFFF] are encoded in a surrogate pair.
      (if (unicode.utf-16-single-word-code-point? ($char->fixnum ($string-ref str i)))
	  (%count-surrogate-pairs str str.len ($fxadd1 i) accum-count)
	(%count-surrogate-pairs str str.len ($fxadd1 i) ($fxadd1 accum-count)))))

  (define (%encode-and-fill-bytevector str str.idx str.len bv bv.idx endianness)
    (if ($fx= str.idx str.len)
	bv
      (let ((code-point ($char->fixnum ($string-ref str str.idx))))
	(if (unicode.utf-16-single-word-code-point? code-point)
	    (begin
	      (bytevector-u16-set! bv bv.idx (unicode.utf-16-encode-single-word code-point) endianness)
	      (%encode-and-fill-bytevector str ($fxadd1 str.idx) str.len bv ($fxadd2 bv.idx) endianness))
	  (begin
	    (bytevector-u16-set! bv bv.idx           (unicode.utf-16-encode-first-of-two-words  code-point) endianness)
	    (bytevector-u16-set! bv ($fxadd2 bv.idx) (unicode.utf-16-encode-second-of-two-words code-point) endianness)
	    (%encode-and-fill-bytevector str ($fxadd1 str.idx) str.len bv ($fxadd4 bv.idx) endianness))))))

  #| end of module |# )


(module (utf16->string utf16->string-length)

  (case-define* utf16->string
    (({bv bytevector?} {endianness endianness?})
     ($utf16->string bv endianness #f 'raise))
    (({bv bytevector?} {endianness endianness?} endianness-mandatory?)
     ($utf16->string bv endianness endianness-mandatory? 'raise))
    (({bv bytevector?} {endianness endianness?} endianness-mandatory? {mode error-handling-mode?})
     ($utf16->string bv endianness endianness-mandatory? mode)))

  (case-define* utf16->string-length
    (({bv bytevector?} {endianness endianness?})
     ($utf16->string-length bv endianness #f 'raise))
    (({bv bytevector?} {endianness endianness?} endianness-mandatory?)
     ($utf16->string-length bv endianness endianness-mandatory? 'raise))
    (({bv bytevector?} {endianness endianness?} endianness-mandatory? {mode error-handling-mode?})
     ($utf16->string-length bv endianness endianness-mandatory? mode)))

;;; --------------------------------------------------------------------

  (module ($utf16->string)

    (define* ($utf16->string bv endianness endianness-mandatory? mode)
      (cond (endianness-mandatory?
	     ;;We accept the argument ENDIANNESS as endianness specification.
	     (%decode __who__ bv 0 endianness mode))
	    ((%bom->endianness bv)
	     => (lambda (endian)
		  (%decode __who__ bv 2 endian mode)))
	    (else
	     ;;There  is  no  BOM,  accept  the  argument  ENDIANNESS  as  endianness
	     ;;specification.
	     (%decode __who__ bv 0 endianness mode))))

    (define (%decode who bv bv.start endianness mode)
      (let* ((bv.len  ($bytevector-length bv))
	     (str.len (%compute-string-length who bv bv.start bv.len 0 endianness mode))
	     (str     ($make-string str.len)))
	(%decode-string-fill-bytevector who bv bv.start bv.len str 0 endianness mode)))

    #| end of module |# )

  (module ($utf16->string-length)

    (define* ($utf16->string-length bv endianness endianness-mandatory? mode)
      (cond (endianness-mandatory?
	     ;;We accept the argument ENDIANNESS as endianness specification.
	     (%compute-length __who__ bv 0 endianness mode))
	    ((%bom->endianness bv)
	     => (lambda (endian)
		  (%compute-length __who__ bv 2 endian mode)))
	    (else
	     ;;There  is  no  BOM,  accept  the  argument  ENDIANNESS  as  endianness
	     ;;specification.
	     (%compute-length __who__ bv 0 endianness mode))))

    (define (%compute-length who bv bv.start endianness mode)
      (%compute-string-length who bv bv.start ($bytevector-length bv) 0 endianness mode))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (%bom->endianness bv)
    (and ($fx>= ($bytevector-length bv) 2)
	 (let ((n ($bytevector-u16b-ref bv 0)))
	   (cond (($fx= n #xFEFF)	'big)
		 (($fx= n #xFFFE)	'little)
		 (else			#f)))))

;;; --------------------------------------------------------------------

  (define (%compute-string-length who bv bv.idx bv.len accum-len endianness mode)
    ;;Compute the  length of the string  object needed to represent  the given UTF-16
    ;;bytevector.
    ;;
    (define-syntax-rule (%recurse bv.idx accum-len)
      (%compute-string-length who bv bv.idx bv.len accum-len endianness mode))

    (define (%error-invalid-first-word bv.idx accum-len)
      ;;At index BV.IDX there should be either  a standalone 16-bit word or the first
      ;;word of  a surrogate pair;  instead, there is  an invalid word.   This should
      ;;never happen.
      ;;
      (case mode
	((ignore)
	 ;;When converting: we will ignore the invalid word.
	 (%recurse ($fxadd2 bv.idx) accum-len))
	((replace)
	 ;;When  converting:  we  will  replace  the  invalid  word  with  a  #\xFFFD
	 ;;character.
	 (%recurse ($fxadd2 bv.idx) ($fxadd1 accum-len)))
	(else	;raise
	 (%raise-condition who "invalid 16-bit first word"
			   (make-utf16-string-decoding-invalid-first-word bv bv.idx (bytevector-u16-ref bv bv.idx endianness))))))

    (define (%error-invalid-second-word bv.idx-1st bv.idx-2nd accum-len)
      ;;At index BV.IDX-SECOND there should be  the second 16-bit word in a surrogate
      ;;pair.  Instead, there is an invalid word.
      (case mode
	((ignore)
	 ;;When converting: we will ignore the invalid pair.
	 (%recurse ($fxadd2 bv.idx) accum-len))
	((replace)
	 ;;When  converting:  we  will  replace  the  invalid  pair  with  a  #\xFFFD
	 ;;character.
	 (%recurse ($fxadd2 bv.idx) ($fxadd1 accum-len)))
	(else	;raise
	 (%raise-condition who "invalid second 16-bit word in surrogate pair"
			   (make-utf16-string-decoding-invalid-second-word bv bv.idx-2nd
									   (bytevector-u16-ref bv bv.idx-1st endianness)
									   (bytevector-u16-ref bv bv.idx-2nd endianness))))))

    (define (%error-standalone-first-word-in-pair bv.idx accum-len)
      ;;At index BV.IDX there is a standalone first word in surrogate pair; we are at
      ;;the end of the bytevector and the second word is missing.
      (case mode
	((ignore)
	 ;;When converting: we will ignore the standalone first word.
	 accum-len)
	((replace)
	 ;;When  converting: we  will  replace  the standalone  word  with a  #\xFFFD
	 ;;character.
	 ($fxadd1 accum-len))
	(else	;raise
	 (%raise-condition who "standalone first word in surrogate pair at end of UTF-16 bytevector"
			   (make-utf16-string-decoding-missing-second-word bv bv.idx (bytevector-u16-ref bv bv.idx endianness))))))

    (define (%error-not-enough-octets bv.idx accum-len)
      ;;At index BV.IDX there  should be a first 16-bit word or the  second word in a
      ;;pair; we are  not yet at the end  of the bytevector, but there  is not enough
      ;;room for a full  16-bit word or a full surrogate pair:  there is a standalone
      ;;octet.
      ;;
      (case mode
	((ignore)
	 ;;When converting: we will ignore the standalone octet.
	 accum-len)
	((replace)
	 ;;When  converting: we  will replace  the  standalone octet  with a  #\xFFFD
	 ;;character.
	 ($fxadd1 accum-len))
	(else	;raise
	 (%raise-condition who "standalone octet at end of UTF-16 bytevector"
			   (make-utf16-string-decoding-standalone-octet bv bv.idx (bytevector-u8-ref bv bv.idx))))))

    (cond (($fx<= bv.idx ($fxsub2 bv.len))
	   ;;Good there is room for at least one more 16-bit word.
	   (let ((word0 ($bytevector-u16-ref bv bv.idx endianness)))
	     (cond ((unicode.utf-16-single-word? word0)
		    (%recurse ($fxadd2 bv.idx) ($fxadd1 accum-len)))
		   ((unicode.utf-16-first-of-two-words? word0)
		    (let ((bv.idx1 ($fxadd2 bv.idx)))
		      (cond (($fx<= bv.idx1 ($fxsub2 bv.len))
			     ;;Good there  is room  for the second  16-bit word  in a
			     ;;surrogate pair.
			     (let ((word1 ($bytevector-u16-ref bv bv.idx1 endianness)))
			       (if (unicode.utf-16-second-of-two-words? word1)
				   ;;Good there is a full surrogate pair.
				   (%recurse ($fxadd2 bv.idx1) ($fxadd1 accum-len))
				 ;;Error: at index BV.IDX1 there should be the second
				 ;;word in a surrogate pair.
				 (%error-invalid-second-word bv.idx bv.idx1 accum-len))))
			    (($fx= bv.idx1 bv.len)
			     ;;Error: we are  at the end of the  bytevector, at index
			     ;;BV.IDX there  is a standalone first  word in surrogate
			     ;;pair.
			     (%error-standalone-first-word-in-pair bv.idx accum-len))
			    (($fx< bv.idx1 bv.len)
			     ;;Error: we  are not yet  at the end of  the bytevector,
			     ;;but there  is not  enough room  for the  second 16-bit
			     ;;word  in the  surrogate pair.   There is  a standalone
			     ;;octet after the first word.
			     (%error-not-enough-octets bv.idx1 accum-len))
			    (else
			     ;;Error we have gone past the end of the bytevector.
			     (assertion-violation who "internal error: overflow while processing UTF-16 bytevector")))))
		   (else
		    ;;Error: at index  BV.IDX there is an invalid  16-bit word.  This
		    ;;should never happen.
		    (%error-invalid-first-word bv.idx accum-len)))))

	  (($fx= bv.idx bv.len)
	   ;;We correctly are at the end of the bytevector.  Done.
	   accum-len)

	  (($fx< bv.idx bv.len)
	   ;;Error: we  are not yet at  the end of  the bytevector, but there  is not
	   ;;enough room for a 16-bit word.
	   (%error-not-enough-octets bv.idx accum-len))

	  (else
	   ;;Error we have gone past the end of the bytevector.
	   (assertion-violation who "internal error: overflow while processing UTF-16 bytevector"))))

;;; --------------------------------------------------------------------

  (define* (%decode-string-fill-bytevector who bv bv.idx bv.len str str.idx endianness mode)
    (define-syntax-rule (%recurse bv.idx str.idx)
      (%decode-string-fill-bytevector who bv bv.idx bv.len str str.idx endianness mode))

    (define (%error-invalid-first-word bv.idx str.idx)
      ;;At index BV.IDX there should be either  a standalone 16-bit word or the first
      ;;word of  a surrogate pair;  instead, there is  an invalid word.   This should
      ;;never happen.
      ;;
      (case mode
	((ignore)
	 ;;We ignore the invalid word.
	 (%recurse ($fxadd2 bv.idx) str.idx))
	((replace)
	 ;;We replace the invalid word with a #\xFFFD character.
	 ($string-set! str str.idx #\xFFFD)
	 (%recurse ($fxadd2 bv.idx) ($fxadd1 str.idx)))
	(else	;raise
	 (%raise-condition who "invalid 16-bit first word"
			   (make-utf16-string-decoding-invalid-first-word bv bv.idx (bytevector-u16-ref bv bv.idx endianness))))))

    (define (%error-invalid-second-word bv.idx-1st bv.idx-2nd str.idx)
      ;;At index  BV.IDX-2ND there should  be the second  16-bit word in  a surrogate
      ;;pair.  Instead, there is an invalid word.
      ;;
      (case mode
	((ignore)
	 ;;We ignore the invalid pair.
	 (%recurse ($fxadd2 bv.idx) str.idx))
	((replace)
	 ;;We replace the invalid pair with a #\xFFFD character.
	 ($string-set! str str.idx #\xFFFD)
	 (%recurse ($fxadd2 bv.idx) ($fxadd1 str.idx)))
	(else	;raise
	 (%raise-condition who "invalid second 16-bit word in surrogate pair"
			   (make-utf16-string-decoding-invalid-second-word bv bv.idx-2nd
									   (bytevector-u16-ref bv bv.idx-1st endianness)
									   (bytevector-u16-ref bv bv.idx-2nd endianness))))))

    (define (%error-standalone-first-word-in-pair bv.idx str.idx)
      ;;At index BV.IDX there is a standalone first word in surrogate pair; we are at
      ;;the end of the bytevector and the second word is missing.
      (case mode
	((ignore)
	 ;;We ignore the standalone first word.
	 str)
	((replace)
	 ;;We replace the standalone word with a #\xFFFD character.
	 ($string-set! str str.idx #\xFFFD)
	 str)
	(else	;raise
	 (%raise-condition who "standalone first word in surrogate pair at end of UTF-16 bytevector"
			   (make-utf16-string-decoding-missing-second-word bv bv.idx (bytevector-u16-ref bv bv.idx endianness))))))

    (define (%error-not-enough-octets bv.idx str.idx)
      ;;At index BV.IDX there  should be a first 16-bit word or the  second word in a
      ;;pair; we are  not yet at the end  of the bytevector, but there  is not enough
      ;;room for a full  16-bit word or a full surrogate pair:  there is a standalone
      ;;octet.
      ;;
      (case mode
	((ignore)
	 ;;We ignore the standalone octet.
	 str)
	((replace)
	 ;;We replace the standalone octet with a #\xFFFD character.
	 ($string-set! str str.idx #\xFFFD)
	 str)
	(else	;raise
	 (%raise-condition who "standalone octet at end of UTF-16 bytevector"
			   (make-utf16-string-decoding-standalone-octet bv bv.idx (bytevector-u8-ref bv bv.idx))))))

    (cond (($fx<= bv.idx ($fxsub2 bv.len))
	   ;;Good there is room for at least one more 16-bit word.
	   (let ((word0 ($bytevector-u16-ref bv bv.idx endianness)))
	     (cond ((unicode.utf-16-single-word? word0)
		    ($string-set! str str.idx ($fixnum->char (unicode.utf-16-decode-single-word word0)))
		    (%recurse ($fxadd2 bv.idx) ($fxadd1 str.idx)))
		   ((unicode.utf-16-first-of-two-words? word0)
		    (let ((bv.idx1 ($fxadd2 bv.idx)))
		      (cond (($fx<= bv.idx1 ($fxsub2 bv.len))
			     ;;Good there  is room  for the second  16-bit word  in a
			     ;;surrogate pair.
			     (let ((word1 ($bytevector-u16-ref bv bv.idx1 endianness)))
			       (if (unicode.utf-16-second-of-two-words? word1)
				   ;;Good there is a full surrogate pair.
				   (begin
				     ($string-set! str str.idx ($fixnum->char (unicode.utf-16-decode-surrogate-pair word0 word1)))
				     (%recurse ($fxadd2 bv.idx1) ($fxadd1 str.idx)))
				 ;;Error: at index BV.IDX1 there should be the second
				 ;;word in a surrogate pair.
				 (%error-invalid-second-word bv.idx bv.idx1 str.idx))))
			    (($fx= bv.idx1 bv.len)
			     ;;Error: we are  at the end of the  bytevector, at index
			     ;;BV.IDX there  is a standalone first  word in surrogate
			     ;;pair.
			     (%error-standalone-first-word-in-pair bv.idx str.idx))
			    (($fx< bv.idx1 bv.len)
			     ;;Error: we  are not yet  at the end of  the bytevector,
			     ;;but there  is not  enough room  for the  second 16-bit
			     ;;word  in the  surrogate pair.   There is  a standalone
			     ;;octet after the first word.
			     (%error-not-enough-octets bv.idx1 str.idx))
			    (else
			     ;;Error we have gone past the end of the bytevector.
			     (assertion-violation who "internal error: overflow while processing UTF-16 bytevector")))))
		   (else
		    ;;Error: at index  BV.IDX there is an invalid  16-bit word.  This
		    ;;should never happen.
		    (%error-invalid-first-word bv.idx str.idx)))))

	  (($fx= bv.idx bv.len)
	   ;;We correctly are at the end of the bytevector.  Done.
	   str)

	  (($fx< bv.idx bv.len)
	   ;;Error: we  are not yet at  the end of  the bytevector, but there  is not
	   ;;enough room for a 16-bit word.
	   (%error-not-enough-octets bv.idx str.idx))

	  (else
	   ;;Error we have gone past the end of the bytevector.
	   (assertion-violation who "internal error: overflow while processing UTF-16 bytevector"))))

  #| end of module: UTF16->STRING |# )


(module (string->utf32 string->utf32-length)

  (case-define* string->utf32
    (({str string?})
     ($string->utf32 str 'big))

    (({str string?} {endianness endianness?})
     ($string->utf32 str endianness))

    #| end of CASE-DEFINE* |# )

  (define* (string->utf32-length {str string?})
    ($string->utf32-length str))

;;; --------------------------------------------------------------------

  (define ($string->utf32-length str)
    (let ((bv.len (bitwise-arithmetic-shift-left ($string-length str) 2)))
      (and (fixnum? bv.len)
	   bv.len)))

  (define* ($string->utf32 str endianness)
    ;;We need to make sure that the length of the bytevector is a fixnum!!!
    (let ((bv.len  ($string->utf32-length str)))
      (if (fixnum? bv.len)
	  (let ((bv      ($make-bytevector bv.len))
		(str.len ($string-length str)))
	    (if (eq? endianness 'big)
		(%encode-and-fill-bytevector/big str 0 str.len bv)
	      (%encode-and-fill-bytevector/little str 0 str.len bv)))
	(error __who__ "string too long for UTF-32 conversion" str))))

;;; --------------------------------------------------------------------

  (define (%encode-and-fill-bytevector/big str str.idx str.len bv)
    (if ($fx= str.idx str.len)
	bv
      (let ((code-point ($char->fixnum ($string-ref str str.idx)))
	    (bv.idx     ($fxsll str.idx 2)))
	($bytevector-u32b-set! bv bv.idx (unicode.utf-32-encode code-point))
	(%encode-and-fill-bytevector/big str ($fxadd1 str.idx) str.len bv))))

  (define (%encode-and-fill-bytevector/little str str.idx str.len bv)
    (if ($fx= str.idx str.len)
	bv
      (let ((code-point ($char->fixnum ($string-ref str str.idx)))
	    (bv.idx     ($fxsll str.idx 2)))
	($bytevector-u32l-set! bv bv.idx (unicode.utf-32-encode code-point))
	(%encode-and-fill-bytevector/little str ($fxadd1 str.idx) str.len bv))))

  #| end of module: STRING->UTF32 |# )


(module (utf32->string utf32->string-length)
  ;;When decoding  a UTF-32 bytevector into  a Scheme string: ideally  the bytevector
  ;;holds only full 32-bit words representing Unicode code points.
  ;;
  ;;We  take  into  account the  case  of  bytevector  starting  with a  32-bit  word
  ;;representing the  Byte Order Mark  (BOM).  For UTF-32, the  BOM is a  32-bit word
  ;;stored in big endian format:
  ;;
  ;;* The BOM #x0000FEFF represents big endianness.
  ;;
  ;;* The BOM #xFFFE0000 represents little endianness.
  ;;
  ;;We also want to consider the cases in  which: after the full 32-bit words, at the
  ;;end of the bytevector there are 1, 2 or 3 orphan octets.
  ;;
  ;;Let's name  BV.START the index of  the first octet  after the BOM and  BV.LEN the
  ;;length of the bytevector.  When the BOM is not present:
  ;;
  ;;
  ;;   |-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
  ;;    ^
  ;; bv.start
  ;;   |.............................................|
  ;;                                                  ^
  ;;                                               bv.len
  ;;
  ;;when the BOM is present:
  ;;
  ;;   |-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
  ;;   |.......|
  ;;            ^
  ;;        bv.start
  ;;   |.............................................|
  ;;                                                  ^
  ;;                                               bv.len
  ;;
  ;;
  ;;Ideal length of string
  ;;----------------------
  ;;
  ;;Ideally, when the  bytevector has full 32-bit words between  BV.START and BV.LEN,
  ;;the string length is:
  ;;
  ;;   (bv.len - bv.start) / 4
  ;;
  ;;which we can compute as:
  ;;
  ;;   ($fxsra ($fx- bv.len bv.start) 2)
  ;;
  ;;
  ;;Non-ideal length of string
  ;;--------------------------
  ;;
  ;;When  1, 2  or  3  orphan octets  are  present and  the  error  handling mode  is
  ;;"replace": we want  to add a slot to the  string.  Leveraging integer arithmetics
  ;;(in  which division  discards  the modulo),  we  add  3 to  the  ideal number  of
  ;;characters:
  ;;
  ;;   (+ 3 (bv.len - bv.start)) / 4
  ;;
  ;;so that  a string slot is  added only if  there are 1,  2 or 3 orphan  octets; if
  ;;there are no  orphan octets the result is  equal to the ideal.  We  can test this
  ;;formula with the program:
  ;;
  ;;   (import (vicare))
  ;;   (define (doit bv.start bv.len)
  ;;     (import (vicare system $fx))
  ;;     ($fxsra ($fx+ ($fx- bv.len bv.start) 3) 2))
  ;;   (debug-print (doit 0  4) (doit 0  5) (doit 0  6) (doit 0  7))
  ;;   (debug-print (doit 0  8) (doit 0  9) (doit 0 10) (doit 0 11))
  ;;   (debug-print (doit 0 12) (doit 0 13) (doit 0 14) (doit 0 15))
  ;;
  ;;which prints:
  ;;
  ;;   (1 2 2 2)
  ;;   (2 3 3 3)
  ;;   (3 4 4 4)
  ;;
  ;;
  ;;Index of first orphan octet
  ;;---------------------------
  ;;
  ;;In the non-ideal  case of orphan octets  in the bytevector, what is  the index of
  ;;the first  orphan octet?  It is  the greatest exact  multiple of 4 which  is less
  ;;than BV.LEN.  Knowing that:
  ;;
  ;;   (number->string  0 2)	=>     "0"
  ;;   (number->string  4 2)	=>   "100"
  ;;   (number->string  8 2)	=>  "1000"
  ;;   (number->string 12 2)	=>  "1100"
  ;;   (number->string 16 2)	=> "10000"
  ;;
  ;;we can compute it by setting to 0 the 2 least significant bits of BV.LEN:
  ;;
  ;;   (number->string (bitwise-and 4 -4))	=> "100"
  ;;   (number->string (bitwise-and 5 -4))	=> "100"
  ;;   (number->string (bitwise-and 6 -4))	=> "100"
  ;;   (number->string (bitwise-and 7 -4))	=> "100"
  ;;
  ;;so we can compute the index with:
  ;;
  ;;   ($fxand bv.len -4)
  ;;
  ;;If there are  *no* orphan octets: the  result of such formula  equals BV.LEN.  If
  ;;there are orphan octets: the result is less than BV.LEN.
  ;;

  (case-define* utf32->string
    (({bv bytevector?} {endianness endianness?})
     ($utf32->string bv endianness #f 'raise))
    (({bv bytevector?} {endianness endianness?} endianness-mandatory?)
     ($utf32->string bv endianness endianness-mandatory? 'raise))
    (({bv bytevector?} {endianness endianness?} endianness-mandatory? {mode error-handling-mode?})
     ($utf32->string bv endianness endianness-mandatory? mode))
    #| end of CASE-DEFINE* |# )

  (case-define* utf32->string-length
    (({bv bytevector?} {endianness endianness?})
     ($utf32->string-length bv endianness #f 'raise))
    (({bv bytevector?} {endianness endianness?} endianness-mandatory?)
     ($utf32->string-length bv endianness endianness-mandatory? 'raise))
    (({bv bytevector?} {endianness endianness?} endianness-mandatory? {mode error-handling-mode?})
     ($utf32->string-length bv endianness endianness-mandatory? mode))
    #| end of CASE-DEFINE* |# )

;;; --------------------------------------------------------------------

  (module ($utf32->string)

    (define* ($utf32->string bv endianness endianness-mandatory? mode)
      (cond (endianness-mandatory?
	     (%decode __who__ bv 0 endianness mode))
	    ((%bom-present bv)
	     => (lambda (endian)
		  (%decode __who__ bv 4 endian mode)))
	    (else
	     (%decode __who__ bv 0 endianness mode))))

    (define (%decode who bv bv.start endianness mode)
      (let* ((bv.len  ($bytevector-length bv))
	     (bv.last ($fxlogand bv.len -4))
	     (str.len (%compute-string-length who bv bv.start bv.last 0 endianness mode))
	     (str     (make-string str.len)))
	(%decode-and-fill-string who bv bv.start bv.last str 0 endianness mode)))

    #| end of module |# )

  (module ($utf32->string-length)

    (define* ($utf32->string-length bv endianness endianness-mandatory? mode)
      (cond (endianness-mandatory?
	     (%compute __who__ bv 0 endianness mode))
	    ((%bom-present bv)
	     => (lambda (endian)
		  (%compute __who__ bv 4 endian mode)))
	    (else
	     (%compute __who__ bv 0 endianness mode))))

    (define (%compute who bv bv.start endianness mode)
      (let* ((bv.len  ($bytevector-length bv))
	     (bv.last ($fxlogand bv.len -4)))
	(%compute-string-length who bv bv.start bv.last 0 endianness mode)))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (define (%bom-present bv)
    (and ($fx>= ($bytevector-length bv) 4)
	 (case ($bytevector-u32b-ref bv 0)
	   ((#x0000FEFF)	'big)
	   ((#xFFFE0000)	'little)
	   (else		#f))))

;;; --------------------------------------------------------------------

  (define (%compute-string-length who bv bv.idx bv.last accum-len endianness mode)
    (define-syntax-rule (%recurse bv.idx accum-len)
      (%compute-string-length who bv bv.idx bv.last accum-len endianness mode))

    (define (%error-orphan-octets-at-end bv.idx accum-len)
      ;;At index BV.IDX of BV there are orphan  octets: 1, 2 or 3 octest that are not
      ;;enough to form a full 32-bit word.
      ;;
      (case mode
	((ignore)
	 ;;When decoding: we will ignore the orphan octets at the end.
	 accum-len)
	((replace)
	 ;;When decoding: we will replace the orphan octets with a #\xFFFD character.
	 ($fxadd1 accum-len))
	(else	;raise
	 (let ((octet* (let loop ((i       bv.idx)
				  (bv.len  (bytevector-length bv))
				  (octet*  '()))
			 (if (fx<? i bv.len)
			     (loop (fxadd1 i) bv.len (cons (bytevector-u8-ref bv i) octet*))
			   (reverse octet*)))))
	   (%raise-condition who "orphan octets at end of UTF-32 bytevector"
			     (make-utf32-string-decoding-orphan-octets bv bv.idx octet*))))))

    (define (%error-invalid-word bv.idx str.idx)
      ;;At index BV.IDX of BV there is a 32-bit word which is invalid as Unicode code
      ;;point.
      ;;
      (case mode
	((ignore)
	 ;;When decoding: we will ignore the invalid word.
	 (%recurse ($fxadd4 bv.idx) accum-len))
	((replace)
	 ;;When decoding: we will replace the invalid word with a #\xFFFD character.
	 (%recurse ($fxadd4 bv.idx) ($fxadd1 accum-len)))
	(else	;raise
	 (%raise-condition who "invalid 32-bit word in UTF-32 bytevector"
			   (make-utf32-string-decoding-invalid-word bv bv.idx (bytevector-u32-ref bv bv.idx endianness))))))

    (cond (($fx= bv.idx bv.last)
	   ;;If there  are no orphan octets:  now BV.IDX, BV.LAST and  BV.LEN are all
	   ;;equal.
	   (if ($fx= bv.last ($bytevector-length bv))
	       accum-len
	     (%error-orphan-octets-at-end bv.idx accum-len)))
	  (else
	   (let ((word ($bytevector-u32-ref bv bv.idx endianness)))
	     (if (unicode.utf-32-word? word)
		 (%recurse ($fxadd4 bv.idx) ($fxadd1 accum-len))
	       (%error-invalid-word bv.idx accum-len))))))

;;; --------------------------------------------------------------------

  (define (%decode-and-fill-string who bv bv.idx bv.last str str.idx endianness mode)
    (define-syntax-rule (%recurse bv.idx str.idx)
      (%decode-and-fill-string who bv bv.idx bv.last str str.idx endianness mode))

    (define (%error-orphan-octets-at-end bv.idx str.idx)
      ;;At index BV.IDX of BV there are orphan  octets: 1, 2 or 3 octest that are not
      ;;enough to form a full 32-bit word.
      ;;
      (case mode
	((ignore)
	 ;;Ignore the orphan octets at the end.
	 str)
	((replace)
	 ;;Replace the orphan octets with a #\xFFFD character.
	 ($string-set! str str.idx #\xFFFD)
	 str)
	(else	;raise
	 (let ((octet* (let loop ((i       bv.idx)
				  (bv.len  (bytevector-length bv))
				  (octet*  '()))
			 (if (fx<? i bv.len)
			     (loop (fxadd1 i) bv.len (cons (bytevector-u8-ref bv i) octet*))
			   (reverse octet*)))))
	   (%raise-condition who "orphan octets at end of UTF-32 bytevector"
			     (make-utf32-string-decoding-orphan-octets bv bv.idx octet*))))))

    (define (%error-invalid-word bv.idx str.idx)
      ;;At index BV.IDX of BV there is a 32-bit word which is invalid as Unicode code
      ;;point.
      ;;
      (case mode
	((ignore)
	 ;;Ignore the invalid word.
	 (%recurse ($fxadd4 bv.idx) str.idx))
	((replace)
	 ;;Replace the invalid word with a #\xFFFD character.
	 ($string-set! str str.idx #\xFFFD)
	 (%recurse ($fxadd4 bv.idx) ($fxadd1 str.idx)))
	(else	;raise
	 (%raise-condition who "invalid 32-bit word in UTF-32 bytevector"
			   (make-utf32-string-decoding-invalid-word bv bv.idx (bytevector-u32-ref bv bv.idx endianness))))))

    (cond (($fx= bv.idx bv.last)
	   ;;If there  are no  orphan octets:  now BV.IDX  equals BV.LEN  and STR.IDX
	   ;;equals the string  length.  If there are orphan  octets: STR.IDX indexes
	   ;;the last slot in STR.
	   (if ($fx= str.idx ($string-length str))
	       str
	     (%error-orphan-octets-at-end bv.idx str.idx)))
	  (else
	   (let ((word ($bytevector-u32-ref bv bv.idx endianness)))

	     (if (unicode.utf-32-word? word)
		 (begin
		   ($string-set! str str.idx ($fixnum->char (unicode.utf-32-decode word)))
		   (%recurse ($fxadd4 bv.idx) ($fxadd1 str.idx)))
	       (%error-invalid-word bv.idx str.idx))))))

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
;; Local Variables:
;; eval: (put 'unicode.utf-8-case-code-point 'scheme-indent-function 1)
;; End:
