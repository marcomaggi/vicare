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
	  utf16->string-length	string->utf16-length)
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
		  utf16->string-length	string->utf16-length)
    (vicare system $strings)
    (except (vicare system $bytevectors)
	    ;;FIXME This  except is to  be removed at  the next boot  image rotation.
	    ;;(Marco Maggi; Tue Jun 2, 2015)
	    $bytevector-u16-ref
	    $bytevector-u16b-ref
	    $bytevector-u16l-ref)
    (vicare system $fx)
    (vicare system $chars)
    ;;See the documentation of this library for details on Unicode.
    (vicare unsafe unicode)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Wed Jun 3,
    ;;2015)
    (only (ikarus conditions)
	  make-utf16-string-decoding-invalid-first-word
	  make-utf16-string-decoding-invalid-second-word
	  make-utf16-string-decoding-missing-second-word
	  make-utf16-string-decoding-standalone-octet))


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
    (let ((str.len ($string-length str)))
      ;;We must avoid overflow and check that the returned value is a fixnum!!!
      (fxsll (fx+ str.len (%count-surrogate-pairs str str.len 0 0)) 1)))

  (define ($string->utf16 str endianness)
    (let ((str.len ($string-length str)))
      (if ($fxzero? str.len)
	  ;;Let's return a new bytevector.
	  ($make-bytevector 0)
	;;The number  of octets needed  is the number of  characters in STR  plus the
	;;number of surrogate pairs, times 2.
	(let ((bv ($make-bytevector ($string->utf16-length str))))
	  (%encode-and-fill-bytevector str 0 str.len bv 0 endianness)))))

  (define (%count-surrogate-pairs str str.len i accum-count)
    (if ($fx= i str.len)
	accum-count
      ;;Code points in the range [0, #x10000)  are encoded with a single UTF-16 word;
      ;;code points in the range [#x10000, #x10FFFF] are encoded in a surrogate pair.
      (if (utf-16-single-word-code-point? ($char->fixnum ($string-ref str i)))
	  (%count-surrogate-pairs str str.len ($fxadd1 i) accum-count)
	(%count-surrogate-pairs str str.len ($fxadd1 i) ($fxadd1 accum-count)))))

  (define (%encode-and-fill-bytevector str str.idx str.len bv bv.idx endianness)
    (if ($fx= str.idx str.len)
	bv
      (let ((code-point ($char->fixnum ($string-ref str str.idx))))
	(if (utf-16-single-word-code-point? code-point)
	    (begin
	      (bytevector-u16-set! bv bv.idx (utf-16-encode-single-word code-point) endianness)
	      (%encode-and-fill-bytevector str ($fxadd1 str.idx) str.len bv ($fxadd2 bv.idx) endianness))
	  (begin
	    (bytevector-u16-set! bv bv.idx           (utf-16-encode-first-of-two-words  code-point) endianness)
	    (bytevector-u16-set! bv ($fxadd2 bv.idx) (utf-16-encode-second-of-two-words code-point) endianness)
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
	     (cond ((utf-16-single-word? word0)
		    (%recurse ($fxadd2 bv.idx) ($fxadd1 accum-len)))
		   ((utf-16-first-of-two-words? word0)
		    (let ((bv.idx1 ($fxadd2 bv.idx)))
		      (cond (($fx<= bv.idx1 ($fxsub2 bv.len))
			     ;;Good there  is room  for the second  16-bit word  in a
			     ;;surrogate pair.
			     (let ((word1 ($bytevector-u16-ref bv bv.idx1 endianness)))
			       (if (utf-16-second-of-two-words? word1)
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
	     (cond ((utf-16-single-word? word0)
		    ($string-set! str str.idx ($fixnum->char (utf-16-decode-single-word word0)))
		    (%recurse ($fxadd2 bv.idx) ($fxadd1 str.idx)))
		   ((utf-16-first-of-two-words? word0)
		    (let ((bv.idx1 ($fxadd2 bv.idx)))
		      (cond (($fx<= bv.idx1 ($fxsub2 bv.len))
			     ;;Good there  is room  for the second  16-bit word  in a
			     ;;surrogate pair.
			     (let ((word1 ($bytevector-u16-ref bv bv.idx1 endianness)))
			       (if (utf-16-second-of-two-words? word1)
				   ;;Good there is a full surrogate pair.
				   (begin
				     ($string-set! str str.idx ($fixnum->char (utf-16-decode-surrogate-pair word0 word1)))
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
