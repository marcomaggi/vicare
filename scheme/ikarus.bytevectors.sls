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
;;;


(library (ikarus bytevectors)
  (export
    make-bytevector		bytevector-length
    bytevector-copy!		bytevector-fill!
    bytevector-copy		bytevector-append
    bytevector=?		native-endianness
    bytevector-reverse-and-concatenate

    bytevector-s8-ref		bytevector-s8-set!
    bytevector-u8-ref		bytevector-u8-set!

    bytevector-u16-native-ref	bytevector-u16-native-set!
    bytevector-s16-native-ref	bytevector-s16-native-set!
    bytevector-u32-native-ref	bytevector-u32-native-set!
    bytevector-s32-native-ref	bytevector-s32-native-set!
    bytevector-u64-native-ref	bytevector-u64-native-set!
    bytevector-s64-native-ref	bytevector-s64-native-set!

    bytevector-u16-ref		bytevector-u16-set!
    bytevector-s16-ref		bytevector-s16-set!
    bytevector-u32-ref		bytevector-u32-set!
    bytevector-s32-ref		bytevector-s32-set!
    bytevector-u64-ref		bytevector-u64-set!
    bytevector-s64-ref		bytevector-s64-set!

    bytevector-uint-ref		bytevector-uint-set!
    bytevector-sint-ref		bytevector-sint-set!

    bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
    bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
    bytevector-ieee-double-ref	bytevector-ieee-double-set!
    bytevector-ieee-single-ref	bytevector-ieee-single-set!

    uint-list->bytevector	bytevector->uint-list
    sint-list->bytevector	bytevector->sint-list

    u8-list->bytevector		bytevector->u8-list
    s8-list->bytevector		bytevector->s8-list

    u16l-list->bytevector	bytevector->u16l-list
    u16b-list->bytevector	bytevector->u16b-list
    u16n-list->bytevector	bytevector->u16n-list
    s16l-list->bytevector	bytevector->s16l-list
    s16b-list->bytevector	bytevector->s16b-list
    s16n-list->bytevector	bytevector->s16n-list

    u32l-list->bytevector	bytevector->u32l-list
    u32b-list->bytevector	bytevector->u32b-list
    u32n-list->bytevector	bytevector->u32n-list
    s32l-list->bytevector	bytevector->s32l-list
    s32b-list->bytevector	bytevector->s32b-list
    s32n-list->bytevector	bytevector->s32n-list

    u64l-list->bytevector	bytevector->u64l-list
    u64b-list->bytevector	bytevector->u64b-list
    u64n-list->bytevector	bytevector->u64n-list
    s64l-list->bytevector	bytevector->s64l-list
    s64b-list->bytevector	bytevector->s64b-list
    s64n-list->bytevector	bytevector->s64n-list

    f4l-list->bytevector	bytevector->f4l-list
    f4b-list->bytevector	bytevector->f4b-list
    f4n-list->bytevector	bytevector->f4n-list
    f8l-list->bytevector	bytevector->f8l-list
    f8b-list->bytevector	bytevector->f8b-list
    f8n-list->bytevector	bytevector->f8n-list

    c4l-list->bytevector	bytevector->c4l-list
    c4b-list->bytevector	bytevector->c4b-list
    c4n-list->bytevector	bytevector->c4n-list
    c8l-list->bytevector	bytevector->c8l-list
    c8b-list->bytevector	bytevector->c8b-list
    c8n-list->bytevector	bytevector->c8n-list

    subbytevector-u8		subbytevector-u8/count
    subbytevector-s8		subbytevector-s8/count

    ;; unsafe bindings, to be exported by (ikarus system $bytevectors)
    $bytevector=		$bytevector-total-length
    $bytevector-concatenate	$bytevector-reverse-and-concatenate)
  (import (except (ikarus)
		  make-bytevector	bytevector-length
		  bytevector-copy!	bytevector-fill!
		  bytevector-copy	bytevector-append
		  bytevector=?		native-endianness
		  bytevector-reverse-and-concatenate

		  bytevector-s8-ref	bytevector-s8-set!
		  bytevector-u8-ref	bytevector-u8-set!

		  bytevector-u16-native-ref	bytevector-u16-native-set!
		  bytevector-s16-native-ref	bytevector-s16-native-set!
		  bytevector-u32-native-ref	bytevector-u32-native-set!
		  bytevector-s32-native-ref	bytevector-s32-native-set!
		  bytevector-u64-native-ref	bytevector-u64-native-set!
		  bytevector-s64-native-ref	bytevector-s64-native-set!

		  bytevector-u16-ref	bytevector-u16-set!
		  bytevector-s16-ref	bytevector-s16-set!
		  bytevector-u32-ref	bytevector-u32-set!
		  bytevector-s32-ref	bytevector-s32-set!
		  bytevector-u64-ref	bytevector-u64-set!
		  bytevector-s64-ref	bytevector-s64-set!

		  bytevector-uint-ref	bytevector-uint-set!
		  bytevector-sint-ref	bytevector-sint-set!

		  bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
		  bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
		  bytevector-ieee-double-ref	bytevector-ieee-double-set!
		  bytevector-ieee-single-ref	bytevector-ieee-single-set!

		  uint-list->bytevector	bytevector->uint-list
		  sint-list->bytevector	bytevector->sint-list

		  u8-list->bytevector	bytevector->u8-list
		  s8-list->bytevector	bytevector->s8-list

		  u16l-list->bytevector	bytevector->u16l-list
		  u16b-list->bytevector	bytevector->u16b-list
		  u16n-list->bytevector	bytevector->u16n-list
		  s16l-list->bytevector	bytevector->s16l-list
		  s16b-list->bytevector	bytevector->s16b-list
		  s16n-list->bytevector	bytevector->s16n-list

		  u32l-list->bytevector	bytevector->u32l-list
		  u32b-list->bytevector	bytevector->u32b-list
		  u32n-list->bytevector	bytevector->u32n-list
		  s32l-list->bytevector	bytevector->s32l-list
		  s32b-list->bytevector	bytevector->s32b-list
		  s32n-list->bytevector	bytevector->s32n-list

		  u64l-list->bytevector	bytevector->u64l-list
		  u64b-list->bytevector	bytevector->u64b-list
		  u64n-list->bytevector	bytevector->u64n-list
		  s64l-list->bytevector	bytevector->s64l-list
		  s64b-list->bytevector	bytevector->s64b-list
		  s64n-list->bytevector	bytevector->s64n-list

		  f4l-list->bytevector	bytevector->f4l-list
		  f4b-list->bytevector	bytevector->f4b-list
		  f4n-list->bytevector	bytevector->f4n-list
		  f8l-list->bytevector	bytevector->f8l-list
		  f8b-list->bytevector	bytevector->f8b-list
		  f8n-list->bytevector	bytevector->f8n-list

		  c4l-list->bytevector	bytevector->c4l-list
		  c4b-list->bytevector	bytevector->c4b-list
		  c4n-list->bytevector	bytevector->c4n-list
		  c8l-list->bytevector	bytevector->c8l-list
		  c8b-list->bytevector	bytevector->c8b-list
		  c8n-list->bytevector	bytevector->c8n-list

		  subbytevector-u8	subbytevector-u8/count
		  subbytevector-s8	subbytevector-s8/count)
    (prefix (vicare platform words) words.)
    (except (vicare unsafe operations)
	    $bytevector=
	    $bytevector-total-length
	    $bytevector-concatenate
	    $bytevector-reverse-and-concatenate)
    (only (vicare language-extensions syntaxes)
	  case-endianness big little)
    (vicare arguments validation))

  (module (platform-endianness)
    (include "ikarus.config.ss" #t))


;;;; helpers

(define (%implementation-violation who msg . irritants)
  (raise (condition
	  (make-assertion-violation)
	  (make-implementation-restriction-violation)
	  (make-who-condition who)
	  (make-message-condition msg)
	  (make-irritants-condition irritants))))

;;; --------------------------------------------------------------------

(define-inline (bytevector-flonum-single-le-set! bv i x)
  (bytevector-ieee-single-set! bv i x (endianness little)))

(define-inline (bytevector-flonum-single-be-set! bv i x)
  (bytevector-ieee-single-set! bv i x (endianness big)))

(define-inline (bytevector-flonum-single-ne-set! bv i x)
  (bytevector-ieee-single-native-set! bv i x))

(define-inline (bytevector-flonum-double-le-set! bv i x)
  (bytevector-ieee-double-set! bv i x (endianness little)))

(define-inline (bytevector-flonum-double-be-set! bv i x)
  (bytevector-ieee-double-set! bv i x (endianness big)))

(define-inline (bytevector-flonum-double-ne-set! bv i x)
  (bytevector-ieee-double-native-set! bv i x))

;;; --------------------------------------------------------------------

(define-inline (bytevector-flonum-single-le-ref bv i)
  (bytevector-ieee-single-ref bv i (endianness little)))

(define-inline (bytevector-flonum-single-be-ref bv i)
  (bytevector-ieee-single-ref bv i (endianness big)))

(define-inline (bytevector-flonum-single-ne-ref bv i)
  (bytevector-ieee-single-native-ref bv i))

(define-inline (bytevector-flonum-double-le-ref bv i)
  (bytevector-ieee-double-ref bv i (endianness little)))

(define-inline (bytevector-flonum-double-be-ref bv i)
  (bytevector-ieee-double-ref bv i (endianness big)))

(define-inline (bytevector-flonum-double-ne-ref bv i)
  (bytevector-ieee-double-native-ref bv i))

;;; --------------------------------------------------------------------

(define-inline (bytevector-cflonum-single-le-set! bv i x)
  (begin
    (bytevector-ieee-single-set! bv i          (real-part x) (endianness little))
    (bytevector-ieee-single-set! bv ($fx+ 4 i) (imag-part x) (endianness little))))

(define-inline (bytevector-cflonum-single-be-set! bv i x)
  (begin
    (bytevector-ieee-single-set! bv i          (real-part x) (endianness big))
    (bytevector-ieee-single-set! bv ($fx+ 4 i) (imag-part x) (endianness big))))

(define-inline (bytevector-cflonum-single-ne-set! bv i x)
  (begin
    (bytevector-ieee-single-native-set! bv i          (real-part x))
    (bytevector-ieee-single-native-set! bv ($fx+ 4 i) (imag-part x))))

(define-inline (bytevector-cflonum-double-le-set! bv i x)
  (begin
    (bytevector-ieee-double-set! bv i          (real-part x) (endianness little))
    (bytevector-ieee-double-set! bv ($fx+ 8 i) (imag-part x) (endianness little))))

(define-inline (bytevector-cflonum-double-be-set! bv i x)
  (begin
    (bytevector-ieee-double-set! bv i          (real-part x) (endianness big))
    (bytevector-ieee-double-set! bv ($fx+ 8 i) (imag-part x) (endianness big))))

(define-inline (bytevector-cflonum-double-ne-set! bv i x)
  (begin
    (bytevector-ieee-double-native-set! bv i          (real-part x))
    (bytevector-ieee-double-native-set! bv ($fx+ 8 i) (imag-part x))))

;;; --------------------------------------------------------------------

(define-inline (bytevector-cflonum-single-le-ref bv i)
  (make-rectangular
    (bytevector-ieee-single-ref bv i          (endianness little))
    (bytevector-ieee-single-ref bv ($fx+ 4 i) (endianness little))))

(define-inline (bytevector-cflonum-single-be-ref bv i)
  (make-rectangular
    (bytevector-ieee-single-ref bv i          (endianness big))
    (bytevector-ieee-single-ref bv ($fx+ 4 i) (endianness big))))

(define-inline (bytevector-cflonum-single-ne-ref bv i)
  (make-rectangular
    (bytevector-ieee-single-native-ref bv i)
    (bytevector-ieee-single-native-ref bv ($fx+ 4 i))))

(define-inline (bytevector-cflonum-double-le-ref bv i)
  (make-rectangular
    (bytevector-ieee-double-ref bv i          (endianness little))
    (bytevector-ieee-double-ref bv ($fx+ 8 i) (endianness little))))

(define-inline (bytevector-cflonum-double-be-ref bv i)
  (make-rectangular
    (bytevector-ieee-double-ref bv i          (endianness big))
    (bytevector-ieee-double-ref bv ($fx+ 8 i) (endianness big))))

(define-inline (bytevector-cflonum-double-ne-ref bv i)
  (make-rectangular
    (bytevector-ieee-double-native-ref bv i)
    (bytevector-ieee-double-native-ref bv ($fx+ 8 i))))


;;;; arguments validation

(define-argument-validation (total-length who len)
  (fixnum? len)
  (%implementation-violation who
    "total bytevector length exceeds the greatest fixnum" len))

(define-argument-validation (byte-filler who fill)
  (and (fixnum? fill) ($fx<= -128 fill) ($fx<= fill 255))
  (assertion-violation who
    "expected fixnum in range [-128, 255] as bytevector fill argument" fill))

;;; --------------------------------------------------------------------

(define-argument-validation (index who idx)
  (and (fixnum? idx) ($fx<= 0 idx))
  (assertion-violation who
    "expected non-negative fixnum as bytevector index argument" idx))

(define-argument-validation (start-index who idx)
  (and (fixnum? idx) ($fx<= 0 idx))
  (assertion-violation who
    "expected non-negative fixnum as bytevector start index argument" idx))

(define-argument-validation (end-index who idx)
  (and (fixnum? idx) ($fx<= 0 idx))
  (assertion-violation who
    "expected non-negative fixnum as bytevector start end argument" idx))

;;; --------------------------------------------------------------------

(define-argument-validation (index-for who idx bv bytes-per-word)
  ;;To be  used after INDEX  validation.  This validation is  for getter
  ;;and setter indexes.  Valid scenarios:
  ;;
  ;;  |...|word
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                      ^index
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                  ^index
  ;;
  ;;the following are invalid scenarios:
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                      ^index = bv.len
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                    ^index = bv.len
  ;;
  ;;  | empty bytevector
  ;;  ^index = bv.len = 0
  ;;
  ($fx<= idx ($fx- ($bytevector-length bv) bytes-per-word))
  (assertion-violation who
    (string-append "index argument "			(number->string idx)
		   " too big for bytevector length "	(number->string ($bytevector-length bv))
		   " and word size "			(number->string bytes-per-word))
    idx))

(define-argument-validation (start-index-for who idx bv bytes-per-word)
  ;;To be used after  START-INDEX validation.  Valid scenarios for start
  ;;indexes:
  ;;
  ;;  |...|word
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                      ^start
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                  ^start
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                      ^start = bv.len
  ;;
  ;;  | empty bytevector
  ;;  ^start = bv.len = 0
  ;;
  ;;the following is an invalid scenario:
  ;;
  ;;  |---|---|---|---|---|---|---|---|---| bytevector
  ;;                                    ^start = bv.len
  ;;
  (let ((bv.len ($bytevector-length bv)))
    (or ($fx=  idx bv.len)
	($fx<= idx ($fx- bv.len bytes-per-word))))
  (assertion-violation who
    (string-append "start index argument "		(number->string idx)
		   " too big for bytevector length "	(number->string ($bytevector-length bv))
		   " and word size "			(number->string bytes-per-word))
    idx))

(define-argument-validation (end-index-for who idx bv bytes-per-word)
  ;;To  be used after  END-INDEX validation.   An end  index can  be any
  ;;index less than or equal to the bytevector size; also in the case of
  ;;empty bytevector.
  ;;
  ($fx<= idx ($bytevector-length bv))
  (assertion-violation who
    (string-append "end index argument "		(number->string idx)
		   " too big for bytevector length "	(number->string ($bytevector-length bv))
		   " and word size "			(number->string bytes-per-word))
    idx))

(define-argument-validation (aligned-index-2 who idx)
  (words.fixnum-aligned-to-2? idx)
  (assertion-violation who
    "expected bytevector index aligned to multiple of 2 as argument" idx))

(define-argument-validation (aligned-index-4 who idx)
  (words.fixnum-aligned-to-4? idx)
  (assertion-violation who
    "expected bytevector index aligned to multiple of 4 as argument" idx))

(define-argument-validation (aligned-index-8 who idx)
  (words.fixnum-aligned-to-8? idx)
  (assertion-violation who
    "expected bytevector index aligned to multiple of 8 as argument" idx))

;;; --------------------------------------------------------------------

(define-argument-validation (count who count)
  (and (fixnum? count) ($fx<= 0 count))
  (assertion-violation who
    "expected non-negative fixnum as bytevector word count argument" count))

(define-argument-validation (count-for who count bv bv.start bytes-per-word)
  (let ((end ($fx+ bv.start ($fx* count bytes-per-word))))
    ($fx<= end ($bytevector-length bv)))
  (assertion-violation who
    (string-append "word count "			(number->string count)
		   " too big for bytevector length "	(number->string ($bytevector-length bv))
		   " start index "			(number->string bv.start)
		   " and word size "			(number->string bytes-per-word))
    count))


;;;; main bytevector handling functions

(define (native-endianness)
  ;;Defined   by  R6RS.    Return  the   endianness   symbol  associated
  ;;implementation's   preferred  endianness   (usually   that  of   the
  ;;underlying  machine  architecture).   This  may  be  any  endianness
  ;;symbol, including a symbol other than "big" and "little".
  ;;
  platform-endianness)

(define make-bytevector
  ;;Defined  by R6RS.   Return a  newly allocated  bytevector  of BV.LEN
  ;;bytes.  If the FILL argument is missing, the initial contents of the
  ;;returned  bytevector  are  unspecified.   If the  FILL  argument  is
  ;;present, it must  be an exact integer object  in the interval [-128,
  ;;255]  that  specifies  the  initial  value  for  the  bytes  of  the
  ;;bytevector: if FILL  is positive, it is interpreted  as an octet; if
  ;;it is negative, it is interpreted as a byte.
  ;;
  (case-lambda
   ((bv.len)
    (define who 'make-bytevector)
    (with-arguments-validation (who)
	((bytevector-length bv.len))
      ($make-bytevector bv.len)))
   ((bv.len fill)
    (define who 'make-bytevector)
    (with-arguments-validation (who)
	((bytevector-length	bv.len)
	 (byte-filler		fill))
      ($bytevector-fill! ($make-bytevector bv.len) 0 bv.len fill)))))

(define (bytevector-fill! bv fill)
  ;;Defined by R6RS.  The FILL argument  is as in the description of the
  ;;MAKE-BYTEVECTOR  procedure.  The BYTEVECTOR-FILL!   procedure stores
  ;;FILL in every element of BV and returns unspecified values.
  ;;
  (define who 'bytevector-fill!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (byte-filler	fill))
    ($bytevector-fill! bv 0 ($bytevector-length bv) fill)))

(define (bytevector-length bv)
  ;;Defined by R6RS.  Return, as  an exact integer object, the number of
  ;;bytes in BV.
  ;;
  (define who 'bytevector-length)
  (with-arguments-validation (who)
      ((bytevector bv))
    ($bytevector-length bv)))

(define (bytevector=? x y)
  ;;Defined by R6RS.  Return  #t if X and Y are equal;  that is, if they
  ;;have  the same  length and  equal bytes  at all  valid  indices.  It
  ;;returns false otherwise.
  ;;
  (define who 'bytevector=?)
  (with-arguments-validation (who)
      ((bytevector x)
       (bytevector y))
    ($bytevector= x y)))

(define ($bytevector= x y)
  (let ((x.len ($bytevector-length x)))
    (and ($fx= x.len ($bytevector-length y))
	 (let loop ((x x) (y y) (i 0) (len x.len))
	   (or ($fx= i len)
	       (and ($fx= ($bytevector-u8-ref x i)
			  ($bytevector-u8-ref y i))
		    (loop x y ($fxadd1 i) len)))))))

(define (bytevector-copy src.bv)
  ;;Defined by R6RS.  Return a newly allocated copy of SRC.BV.
  ;;
  (define who 'bytevector-copy)
  (with-arguments-validation (who)
      ((bytevector src.bv))
    (let ((src.len ($bytevector-length src.bv)))
      (let loop ((src.bv	src.bv)
		 (dst.bv	($make-bytevector src.len))
		 (i		0)
		 (src.len	src.len))
	(if ($fx= i src.len)
	    dst.bv
	  (begin
	    ($bytevector-u8-set! dst.bv i ($bytevector-u8-ref src.bv i))
	    (loop src.bv dst.bv ($fxadd1 i) src.len)))))))

(define (bytevector-copy! src src.start dst dst.start k)
  ;;Defined  by R6RS.   SRC  and DST  must  be bytevectors.   SRC.START,
  ;;DST.START,  and K must  be non-negative  exact integer  objects that
  ;;satisfy:
  ;;
  ;;   0 <= SRC.START <= SRC.START + K <= SRC.LEN
  ;;   0 <= DST.START <= DST.START + K <= DST.LEN
  ;;
  ;;where SRC.LEN is the length of SRC and DST.LEN is the length of DST.
  ;;
  ;;The BYTEVECTOR-COPY! procedure copies the bytes from SRC at indices:
  ;;
  ;;   SRC.START, ..., SRC.START + K - 1
  ;;
  ;;to consecutive indices in DST starting at index DST.START.
  ;;
  ;;This must  work even if  the memory regions  for the source  and the
  ;;target overlap,  i.e., the  bytes at the  target location  after the
  ;;copy must  be equal to the  bytes at the source  location before the
  ;;copy.
  ;;
  ;;Return unspecified values.
  ;;
  (define who 'bytevector-copy!)
  (with-arguments-validation (who)
      ((bytevector	src)
       (bytevector	dst)
       (start-index	src.start)
       (start-index	dst.start)
       (start-index-for	src.start src 1)
       (start-index-for	dst.start dst 1)
       (count		k)
       (count-for	k src src.start 1)
       (count-for	k dst dst.start 1))
    (if (eq? src dst)
	(cond (($fx< dst.start src.start)
	       (let loop ((src		src)
			  (src.index	src.start)
			  (dst.index	dst.start)
			  (src.past	($fx+ src.start k)))
		 (unless ($fx= src.index src.past)
		   ($bytevector-u8-set! src dst.index ($bytevector-u8-ref src src.index))
		   (loop src ($fxadd1 src.index) ($fxadd1 dst.index) src.past))))

	      (($fx> dst.start src.start)
	       (let loop ((src		src)
			  (src.index	($fx+ src.start k))
			  (dst.index	($fx+ dst.start k))
			  (src.past	src.start))
		 (unless ($fx= src.index src.past)
		   (let ((src.index ($fxsub1 src.index))
			 (dst.index ($fxsub1 dst.index)))
		     ($bytevector-u8-set! src dst.index ($bytevector-u8-ref src src.index))
		     (loop src src.index dst.index src.past))))))

      (let loop ((src		src)
		 (src.index	src.start)
		 (dst		dst)
		 (dst.index	dst.start)
		 (src.past	($fx+ src.start k)))
	(unless ($fx= src.index src.past)
	  ($bytevector-u8-set! dst dst.index ($bytevector-u8-ref src src.index))
	  (loop src ($fxadd1 src.index) dst ($fxadd1 dst.index) src.past))))))


;;;; subbytevectors, bytes

(define subbytevector-u8
  ;;Defined by  Vicare.  Build and  return a new bytevector  holding the
  ;;bytes in  SRC.BV from index  SRC.START (inclusive) to  index SRC.END
  ;;(exclusive).  The start and end indexes must be such that:
  ;;
  ;;   0 <= SRC.START <= src.END <= (bytevector-length SRC.BV)
  ;;
  (case-lambda
   ((src.bv src.start)
    (define who 'subbytevector-u8)
    (with-arguments-validation (who)
	((bytevector src.bv))
      (subbytevector-u8 src.bv src.start ($bytevector-length src.bv))))
   ((src.bv src.start src.end)
    (define who 'subbytevector-u8)
    (with-arguments-validation (who)
	((bytevector		src.bv)
	 (start-index		src.start)
	 (end-index		src.end)
	 (start-index-for	src.start src.bv 1)
	 (end-index-for		src.end   src.bv 1))
      (%$subbytevector-u8/count src.bv src.start ($fx- src.end src.start))))))

(define (subbytevector-u8/count src.bv src.start dst.len)
  ;;Defined  by  Vicare.  Build  and  return  a  new bytevector  holding
  ;;DST.LEN bytes in SRC.BV from index SRC.START (inclusive).  The start
  ;;index and the byte count must be such that:
  ;;
  ;;   0 <= SRC.START <= SRC.START + DST.LEN <= (bytevector-length SRC.BV)
  ;;
  (define who 'subbytevector-u8/count)
  (with-arguments-validation (who)
      ((bytevector	src.bv)
       (start-index	src.start)
       (start-index-for	src.start src.bv 1)
       (count		dst.len)
       (count-for	dst.len src.bv src.start 1))
    (%$subbytevector-u8/count src.bv src.start dst.len)))

(define (%$subbytevector-u8/count src.bv src.start dst.len)
  (let ((dst.bv ($make-bytevector dst.len)))
    (do ((dst.index 0         ($fx+ 1 dst.index))
	 (src.index src.start ($fx+ 1 src.index)))
	(($fx= dst.index dst.len)
	 dst.bv)
      ($bytevector-u8-set! dst.bv dst.index ($bytevector-u8-ref src.bv src.index)))))

(define subbytevector-s8
  ;;Defined by  Vicare.  Build and  return a new bytevector  holding the
  ;;bytes in  SRC.BV from index  SRC.START (inclusive) to  index SRC.END
  ;;(exclusive).  The start and end indexes must be such that:
  ;;
  ;;   0 <= SRC.START <= src.END <= (bytevector-length SRC.BV)
  ;;
  (case-lambda
   ((src.bv src.start)
    (define who 'subbytevector-s8)
    (with-arguments-validation (who)
	((bytevector src.bv))
      (subbytevector-s8 src.bv src.start ($bytevector-length src.bv))))
   ((src.bv src.start src.end)
    (define who 'subbytevector-s8)
    (with-arguments-validation (who)
	((bytevector		src.bv)
	 (start-index		src.start)
	 (end-index		src.end)
	 (start-index-for	src.start src.bv 1)
	 (end-index-for		src.end   src.bv 1))
      (%$subbytevector-s8/count src.bv src.start ($fx- src.end src.start))))))

(define (subbytevector-s8/count src.bv src.start dst.len)
  ;;Defined  by  Vicare.  Build  and  return  a  new bytevector  holding
  ;;DST.LEN bytes in SRC.BV from index SRC.START (inclusive).  The start
  ;;index and the byte count must be such that:
  ;;
  ;;   0 <= SRC.START <= SRC.START + DST.LEN <= (bytevector-length SRC.BV)
  ;;
  (define who 'subbytevector-s8/count)
  (with-arguments-validation (who)
      ((bytevector	src.bv)
       (start-index	src.start)
       (start-index-for	src.start src.bv 1)
       (count		dst.len)
       (count-for	dst.len src.bv src.start 1))
    (%$subbytevector-s8/count src.bv src.start dst.len)))

(define (%$subbytevector-s8/count src.bv src.start dst.len)
  (let ((dst.bv ($make-bytevector dst.len)))
    (do ((dst.index 0         ($fx+ 1 dst.index))
	 (src.index src.start ($fx+ 1 src.index)))
	(($fx= dst.index dst.len)
	 dst.bv)
      ($bytevector-s8-set! dst.bv dst.index ($bytevector-s8-ref src.bv src.index)))))


(define (bytevector-append . list-of-bytevectors)
  ;;Defined by Vicare.  Concatenate  the bytevector arguments and return
  ;;the result.  If no arguments are given: return the empty bytevector.
  ;;
  (define who 'bytevector-concatenate)
  (with-arguments-validation (who)
      ((list-of-bytevectors	list-of-bytevectors))
    (let ((total-length ($bytevector-total-length 0 list-of-bytevectors)))
      (with-dangerous-arguments-validation (who)
	  ((total-length	total-length))
	($bytevector-concatenate total-length list-of-bytevectors)))))

(define (bytevector-reverse-and-concatenate list-of-bytevectors)
  ;;Defined  by Vicare.   Reverse  the LIST-OF-BYTEVECTORS,  concatenate
  ;;them and return the resulting bytevector.  It is an error if the sum
  ;;of  the bytevector  lengths  is  not in  the  range  of the  maximum
  ;;bytevector length.
  ;;
  (define who 'bytevector-reverse-and-concatenate)
  (with-arguments-validation (who)
      ((list-of-bytevectors	list-of-bytevectors))
    (let ((total-length ($bytevector-total-length 0 list-of-bytevectors)))
      (with-dangerous-arguments-validation (who)
	  ((total-length	total-length))
	($bytevector-reverse-and-concatenate total-length list-of-bytevectors)))))

(define ($bytevector-total-length total-len list-of-bytevectors)
  ;;Given  the  LIST-OF-BYTEVECTORS: compute  the  total  length of  the
  ;;bytevectors,  add  it  to  TOTAL-LEN  and  return  the  result.   If
  ;;TOTAL-LEN is  zero: the returned  value is  the total length  of the
  ;;bytevectors.  The returned  value may or may not be  in the range of
  ;;the maximum bytevector size.
  ;;
  (if (null? list-of-bytevectors)
      total-len
    ($bytevector-total-length (+ total-len ($bytevector-length ($car list-of-bytevectors)))
			      ($cdr list-of-bytevectors))))

(define ($bytevector-concatenate total-length list-of-bytevectors)
  ;;Concatenate  the  bytevectors  in  LIST-OF-BYTEVECTORS,  return  the
  ;;result.   The resulting  bytevector must  have length  TOTAL-LENGTH.
  ;;Assume the arguments have been already validated.
  ;;
  ;;IMPLEMENTATION RESTRICTION The bytevectors must have a fixnum length
  ;;and the whole bytevector must at maximum have a fixnum length.
  ;;
  (let loop ((dst.bv	($make-bytevector total-length))
	     (dst.start	0)
	     (bvs	list-of-bytevectors))
    (if (null? bvs)
	dst.bv
      (let* ((src.bv   ($car bvs))
	     (src.len  ($bytevector-length src.bv)))
	($bytevector-copy!/count src.bv 0 dst.bv dst.start src.len)
	(loop dst.bv ($fx+ dst.start src.len) ($cdr bvs))))))

(define ($bytevector-reverse-and-concatenate total-length list-of-bytevectors)
  ;;Reverse  LIST-OF-BYTEVECTORS and  concatenate its  bytevector items;
  ;;return  the  result.   The  resulting bytevector  must  have  length
  ;;TOTAL-LENGTH.  Assume the arguments have been already validated.
  ;;
  ;;IMPLEMENTATION RESTRICTION The bytevectors must have a fixnum length
  ;;and the whole bytevector must at maximum have a fixnum length.
  ;;
  (let loop ((dst.bv	($make-bytevector total-length))
	     (dst.start	total-length)
	     (bvs	list-of-bytevectors))
    (if (null? bvs)
	dst.bv
      (let* ((src.bv    ($car bvs))
	     (src.len   ($bytevector-length src.bv))
	     (dst.start ($fx- dst.start src.len)))
	($bytevector-copy!/count src.bv 0 dst.bv dst.start src.len)
	(loop dst.bv dst.start ($cdr bvs))))))


;;;; 8-bit setters and getters

(define (bytevector-s8-ref bv index)
  (define who 'bytevector-s8-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 1))
    ($bytevector-s8-ref bv index)))

(define (bytevector-u8-ref bv index)
  (define who 'bytevector-u8-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 1))
    ($bytevector-u8-ref bv index)))

(define (bytevector-s8-set! bv index byte)
  (define who 'bytevector-s8-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 1)
       (byte		byte))
    ($bytevector-s8-set! bv index byte)))

(define (bytevector-u8-set! bv index octet)
  (define who 'bytevector-u8-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 1)
       (octet		octet))
    ($bytevector-u8-set! bv index octet)))


;;;; 16-bit setters and getters

(define (bytevector-u16-ref bv index endianness)
  (define who 'bytevector-u16-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 2))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-u16b-ref bv index))
      ((little)
       ($bytevector-u16l-ref bv index)))))

(define (bytevector-u16-set! bv index word endianness)
  (define who 'bytevector-u16-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 2)
       (word-u16	word))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-u16b-set! bv index word))
      ((little)
       ($bytevector-u16l-set! bv index word)))))

;;; --------------------------------------------------------------------

(define (bytevector-s16-ref bv index endianness)
  (define who 'bytevector-s16-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 2))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-s16b-ref bv index))
      ((little)
       ($bytevector-s16l-ref bv index)))))

(define (bytevector-s16-set! bv index word endianness)
  (define who 'bytevector-s16-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 2)
       (word-s16	word))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-s16b-set! bv index word))
      ((little)
       ($bytevector-s16l-set! bv index word)))))

;;; --------------------------------------------------------------------

(define (bytevector-u16-native-ref bv index)
  (define who 'bytevector-u16-native-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 2)
       (aligned-index-2	index))
    ($bytevector-u16n-ref bv index)))

(define (bytevector-u16-native-set! bv index word)
  (define who 'bytevector-u16-native-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 2)
       (aligned-index-2	index)
       (word-u16	word))
    ($bytevector-u16n-set! bv index word)))

;;; --------------------------------------------------------------------

(define (bytevector-s16-native-ref bv index)
  (define who 'bytevector-s16-native-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 2)
       (aligned-index-2	index))
    ($bytevector-s16n-ref bv index)))

(define (bytevector-s16-native-set! bv index word)
  (define who 'bytevector-s16-native-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 2)
       (aligned-index-2	index)
       (word-s16	word))
    ($bytevector-s16n-set! bv index word)))


;;;; 32-bit setters and getters

(define (bytevector-u32-ref bv index endianness)
  (define who 'bytevector-u32-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-u32b-ref bv index))
      ((little)
       ($bytevector-u32l-ref bv index)))))

(define (bytevector-u32-set! bv index word endianness)
  (define who 'bytevector-u32-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (word-u32	word))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-u32b-set! bv index word))
      ((little)
       ($bytevector-u32l-set! bv index word)))))

;;; --------------------------------------------------------------------

(define (bytevector-s32-ref bv index endianness)
  (define who 'bytevector-s32-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-s32b-ref bv index))
      ((little)
       ($bytevector-s32l-ref bv index)))))

(define (bytevector-s32-set! bv index word endianness)
  (define who 'bytevector-s32-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (word-s32	word))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-s32b-set! bv index word))
      ((little)
       ($bytevector-s32l-set! bv index word)))))

;;; --------------------------------------------------------------------

(define (bytevector-u32-native-ref bv index)
  (define who 'bytevector-u32-native-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (aligned-index-4	index))
    ($bytevector-u32n-ref bv index)))

(define (bytevector-u32-native-set! bv index word)
  (define who 'bytevector-u32-native-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (aligned-index-4	index)
       (word-u32	word))
    ($bytevector-u32n-set! bv index word)))

;;; --------------------------------------------------------------------

(define (bytevector-s32-native-ref bv index)
  (define who 'bytevector-s32-native-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (aligned-index-4	index))
    ($bytevector-s32n-ref bv index)))

(define (bytevector-s32-native-set! bv index word)
  (define who 'bytevector-s32-native-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (aligned-index-4	index)
       (word-s32	word))
    ($bytevector-s32n-set! bv index word)))


;;;; 64-bit setters and getters

(define (bytevector-u64-ref bv index endianness)
  (define who 'bytevector-u64-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-u64b-ref bv index))
      ((little)
       ($bytevector-u64l-ref bv index)))))

(define (bytevector-u64-set! bv index word endianness)
  (define who 'bytevector-u64-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (word-u64	word))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-u64b-set! bv index word))
      ((little)
       ($bytevector-u64l-set! bv index word)))))

;;; --------------------------------------------------------------------

(define (bytevector-s64-ref bv index endianness)
  (define who 'bytevector-s64-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-s64b-ref bv index))
      ((little)
       ($bytevector-s64l-ref bv index)))))

(define (bytevector-s64-set! bv index word endianness)
  (define who 'bytevector-s64-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (word-s64	word))
    (case-endianness (who endianness)
      ((big)
       ($bytevector-s64b-set! bv index word))
      ((little)
       ($bytevector-s64l-set! bv index word)))))

;;; --------------------------------------------------------------------

(define (bytevector-u64-native-ref bv index)
  (define who 'bytevector-u64-native-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (aligned-index-8	index))
    ($bytevector-u64n-ref bv index)))

(define (bytevector-u64-native-set! bv index word)
  (define who 'bytevector-u64-native-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (aligned-index-8	index)
       (word-u64	word))
    ($bytevector-u64n-set! bv index word)))

;;; --------------------------------------------------------------------

(define (bytevector-s64-native-ref bv index)
  (define who 'bytevector-s64-native-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (aligned-index-8	index))
    ($bytevector-s64n-ref bv index)))

(define (bytevector-s64-native-set! bv index word)
  (define who 'bytevector-s64-native-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (aligned-index-8	index)
       (word-s64	word))
    ($bytevector-s64n-set! bv index word)))


;;;; double-precision flonum bytevector functions

(define (bytevector-ieee-double-ref bv index endianness)
  (define who 'bytevector-ieee-double-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8))
    (if (words.fixnum-aligned-to-8? index)
	(case-endianness (who endianness)
	  ((little)
	   ($bytevector-ieee-double-native-ref bv index))
	  ((big)
	   ($bytevector-ieee-double-nonnative-ref bv index)))
      (case-endianness (who endianness)
	((little)
	 ($bytevector-ieee-double-ref/little bv index))
	((big)
	 ($bytevector-ieee-double-ref/big bv index))))))

(define (bytevector-ieee-double-set! bv index X endianness)
  (define who 'bytevector-ieee-double-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (flonum		X))
    (if (words.fixnum-aligned-to-8? index)
	(case-endianness (who endianness)
	  ((little)
	   ($bytevector-ieee-double-native-set! bv index X))
	  ((big)
	   ($bytevector-ieee-double-nonnative-set! bv index X)))
      (case endianness
	((little)
	 ($bytevector-ieee-double-set!/little bv index X))
	((big)
	 ($bytevector-ieee-double-set!/big bv index X))))))

;;; --------------------------------------------------------------------

(define (bytevector-ieee-double-native-ref bv index)
  (define who 'bytevector-ieee-double-native-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (aligned-index-8	index))
    ($bytevector-ieee-double-native-ref bv index)))

(define (bytevector-ieee-double-native-set! bv index X)
  (define who 'bytevector-ieee-double-native-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 8)
       (aligned-index-8	index)
       (flonum		X))
    ($bytevector-ieee-double-native-set! bv index X)))


;;;; single-precision flonum bytevector functions

(define (bytevector-ieee-single-ref bv index endianness)
  (define who 'bytevector-ieee-single-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4))
    (if (words.fixnum-aligned-to-4? index)
	(case-endianness (who endianness)
	  ((little)
	   ($bytevector-ieee-single-native-ref bv index))
	  ((big)
	   ($bytevector-ieee-single-nonnative-ref bv index)))
      (case-endianness (who endianness)
	((little)
	 ($bytevector-ieee-single-ref/little bv index))
	((big)
	 ($bytevector-ieee-single-ref/big bv index))))))

(define (bytevector-ieee-single-set! bv index X endianness)
  (define who 'bytevector-ieee-single-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (flonum		X))
    (if (words.fixnum-aligned-to-4? index)
	(case-endianness (who endianness)
	  ((little)
	   ($bytevector-ieee-single-native-set! bv index X))
	  ((big)
	   ($bytevector-ieee-single-nonnative-set! bv index X)))
      (case-endianness (who endianness)
	((little)
	 ($bytevector-ieee-single-set!/little bv index X))
	((big)
	 ($bytevector-ieee-single-set!/big bv index X))))))

;;; --------------------------------------------------------------------

(define (bytevector-ieee-single-native-ref bv index)
  (define who 'bytevector-ieee-single-native-ref)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (aligned-index-4	index))
    ($bytevector-ieee-single-native-ref bv index)))

(define (bytevector-ieee-single-native-set! bv index X)
  (define who 'bytevector-ieee-single-native-set!)
  (with-arguments-validation (who)
      ((bytevector	bv)
       (index		index)
       (index-for	index bv 4)
       (aligned-index-4	index)
       (flonum		X))
    ($bytevector-ieee-single-native-set! bv index X)))


;;;; unsafe flonum setters and geters
;;
;;As defined  by IEEE  754: single-precision flonums  are 4  bytes long,
;;double-precision  flonums  are 8  bytes  long.   Vicare's flonums  are
;;double-precision.  On  big-endian platforms the order of  bytes is the
;;same in a flonum data area and the bytevector.
;;

(define ($bytevector-ieee-double-ref/big x i)
  (let ((y ($make-flonum)))
    ($flonum-set! y 0 ($bytevector-u8-ref x i))
    ($flonum-set! y 1 ($bytevector-u8-ref x ($fx+ i 1)))
    ($flonum-set! y 2 ($bytevector-u8-ref x ($fx+ i 2)))
    ($flonum-set! y 3 ($bytevector-u8-ref x ($fx+ i 3)))
    ($flonum-set! y 4 ($bytevector-u8-ref x ($fx+ i 4)))
    ($flonum-set! y 5 ($bytevector-u8-ref x ($fx+ i 5)))
    ($flonum-set! y 6 ($bytevector-u8-ref x ($fx+ i 6)))
    ($flonum-set! y 7 ($bytevector-u8-ref x ($fx+ i 7)))
    y))

(define ($bytevector-ieee-double-set!/big x i y)
  ($bytevector-u8-set! x i                ($flonum-u8-ref y 0))
  ($bytevector-u8-set! x ($fx+ i 1) ($flonum-u8-ref y 1))
  ($bytevector-u8-set! x ($fx+ i 2) ($flonum-u8-ref y 2))
  ($bytevector-u8-set! x ($fx+ i 3) ($flonum-u8-ref y 3))
  ($bytevector-u8-set! x ($fx+ i 4) ($flonum-u8-ref y 4))
  ($bytevector-u8-set! x ($fx+ i 5) ($flonum-u8-ref y 5))
  ($bytevector-u8-set! x ($fx+ i 6) ($flonum-u8-ref y 6))
  ($bytevector-u8-set! x ($fx+ i 7) ($flonum-u8-ref y 7)))

(define ($bytevector-ieee-double-ref/little x i)
  (let ((y ($make-flonum)))
    ($flonum-set! y 7 ($bytevector-u8-ref x i))
    ($flonum-set! y 6 ($bytevector-u8-ref x ($fx+ i 1)))
    ($flonum-set! y 5 ($bytevector-u8-ref x ($fx+ i 2)))
    ($flonum-set! y 4 ($bytevector-u8-ref x ($fx+ i 3)))
    ($flonum-set! y 3 ($bytevector-u8-ref x ($fx+ i 4)))
    ($flonum-set! y 2 ($bytevector-u8-ref x ($fx+ i 5)))
    ($flonum-set! y 1 ($bytevector-u8-ref x ($fx+ i 6)))
    ($flonum-set! y 0 ($bytevector-u8-ref x ($fx+ i 7)))
    y))

(define ($bytevector-ieee-double-set!/little x i y)
  ($bytevector-u8-set! x i                ($flonum-u8-ref y 7))
  ($bytevector-u8-set! x ($fx+ i 1) ($flonum-u8-ref y 6))
  ($bytevector-u8-set! x ($fx+ i 2) ($flonum-u8-ref y 5))
  ($bytevector-u8-set! x ($fx+ i 3) ($flonum-u8-ref y 4))
  ($bytevector-u8-set! x ($fx+ i 4) ($flonum-u8-ref y 3))
  ($bytevector-u8-set! x ($fx+ i 5) ($flonum-u8-ref y 2))
  ($bytevector-u8-set! x ($fx+ i 6) ($flonum-u8-ref y 1))
  ($bytevector-u8-set! x ($fx+ i 7) ($flonum-u8-ref y 0)))

;;; --------------------------------------------------------------------

(define ($bytevector-ieee-single-ref/little x i)
  (let ((bv (make-bytevector 4)))
    ($bytevector-u8-set! bv 0 ($bytevector-u8-ref x i))
    ($bytevector-u8-set! bv 1 ($bytevector-u8-ref x ($fx+ i 1)))
    ($bytevector-u8-set! bv 2 ($bytevector-u8-ref x ($fx+ i 2)))
    ($bytevector-u8-set! bv 3 ($bytevector-u8-ref x ($fx+ i 3)))
    ($bytevector-ieee-single-native-ref bv 0)))

(define ($bytevector-ieee-single-ref/big x i)
  (let ((bv (make-bytevector 4)))
    ($bytevector-u8-set! bv 3 ($bytevector-u8-ref x i))
    ($bytevector-u8-set! bv 2 ($bytevector-u8-ref x ($fx+ i 1)))
    ($bytevector-u8-set! bv 1 ($bytevector-u8-ref x ($fx+ i 2)))
    ($bytevector-u8-set! bv 0 ($bytevector-u8-ref x ($fx+ i 3)))
    ($bytevector-ieee-single-native-ref bv 0)))

(define ($bytevector-ieee-single-set!/little x i v)
  (let ((bv (make-bytevector 4)))
    ($bytevector-ieee-single-native-set! bv 0 v)
    ($bytevector-u8-set! x i                ($bytevector-u8-ref bv 0))
    ($bytevector-u8-set! x ($fx+ i 1) ($bytevector-u8-ref bv 1))
    ($bytevector-u8-set! x ($fx+ i 2) ($bytevector-u8-ref bv 2))
    ($bytevector-u8-set! x ($fx+ i 3) ($bytevector-u8-ref bv 3))))

(define ($bytevector-ieee-single-set!/big x i v)
  (let ((bv (make-bytevector 4)))
    ($bytevector-ieee-single-native-set! bv 0 v)
    ($bytevector-u8-set! x i                ($bytevector-u8-ref bv 3))
    ($bytevector-u8-set! x ($fx+ i 1) ($bytevector-u8-ref bv 2))
    ($bytevector-u8-set! x ($fx+ i 2) ($bytevector-u8-ref bv 1))
    ($bytevector-u8-set! x ($fx+ i 3) ($bytevector-u8-ref bv 0))))


;;;; any integer getters and setters, bytevector to any integer list conversion

(module (bytevector-uint-ref bytevector-sint-ref
			     bytevector->uint-list bytevector->sint-list)
  (define (uref-big x ib il) ;; ib included, il excluded
    (cond
     (($fx= il ib) 0)
     (else
      (let ((b ($bytevector-u8-ref x ib)))
	(cond
	 (($fx= b 0) (uref-big x ($fxadd1 ib) il))
	 (else
	  (case ($fx- il ib)
	    ((1) b)
	    ((2) ($fx+ ($fxsll b 8)
		       ($bytevector-u8-ref x ($fxsub1 il))))
	    ((3)
	     ($fx+ ($fxsll ($fx+ ($fxsll b 8)
				 ($bytevector-u8-ref x ($fxadd1 ib)))
			   8)
		   ($bytevector-u8-ref x ($fxsub1 il))))
	    (else
	     (let ((im ($fxsra ($fx+ il ib) 1)))
	       (+ (uref-big x im il)
		  (* (uref-big x ib im)
		     (expt 256 ($fx- il im)))))))))))))
  (define (uref-little x il ib) ;; il included, ib excluded
    (cond
     (($fx= il ib) 0)
     (else
      (let ((ib^ ($fxsub1 ib)))
	(let ((b ($bytevector-u8-ref x ib^)))
	  (cond
	   (($fx= b 0) (uref-little x il ib^))
	   (else
	    (case ($fx- ib il)
	      ((1) b)
	      ((2) ($fx+ ($fxsll b 8) ($bytevector-u8-ref x il)))
	      ((3)
	       ($fx+ ($fxsll ($fx+ ($fxsll b 8)
				   ($bytevector-u8-ref x ($fxadd1 il)))
			     8)
		     ($bytevector-u8-ref x il)))
	      (else
	       (let ((im ($fxsra ($fx+ il ib) 1)))
		 (+ (uref-little x il im)
		    (* (uref-little x im ib)
		       (expt 256 ($fx- im il))))))))))))))
  (define (sref-big x ib il) ;; ib included, il excluded
    (cond
     (($fx= il ib) -1)
     (else
      (let ((b ($bytevector-u8-ref x ib)))
	(cond
	 (($fx= b 0) (uref-big x ($fxadd1 ib) il))
	 (($fx= b 255) (sref-big-neg x ($fxadd1 ib) il))
	 (($fx< b 128) (uref-big x ib il))
	 (else (- (uref-big x ib il) (expt 256 ($fx- il ib)))))))))
  (define (sref-big-neg x ib il) ;; ib included, il excluded
    (cond
     (($fx= il ib) -1)
     (else
      (let ((b ($bytevector-u8-ref x ib)))
	(cond
	 (($fx= b 255) (sref-big-neg x ($fxadd1 ib) il))
	 (else (- (uref-big x ib il) (expt 256 ($fx- il ib)))))))))
  (define (sref-little x il ib) ;; il included, ib excluded
    (cond
     (($fx= il ib) -1)
     (else
      (let ((ib^ ($fxsub1 ib)))
	(let ((b ($bytevector-u8-ref x ib^)))
	  (cond
	   (($fx= b 0) (uref-little x il ib^))
	   (($fx= b 255) (sref-little-neg x il ib^))
	   (($fx< b 128) (uref-little x il ib))
	   (else (- (uref-little x il ib) (expt 256 ($fx- ib il))))))))))
  (define (sref-little-neg x il ib) ;; il included, ib excluded
    (cond
     (($fx= il ib) -1)
     (else
      (let ((ib^ ($fxsub1 ib)))
	(let ((b ($bytevector-u8-ref x ib^)))
	  (cond
	   (($fx= b 255) (sref-little-neg x il ib^))
	   (else (- (uref-little x il ib) (expt 256 ($fx- ib il))))))))))
  (define bytevector-sint-ref
    (lambda (x k endianness size)
      (define who 'bytevector-sint-ref)
      (unless (bytevector? x) (die who "not a bytevector" x))
      (unless (and (fixnum? k) ($fx>= k 0)) (die who "invalid index" k))
      (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
      (let ((n ($bytevector-length x)))
	(unless ($fx< k n) (die who "index is out of range" k))
	(let ((end ($fx+ k size)))
	  (unless (and ($fx>= end 0) ($fx<= end n))
	    (die who "out of range" k size))
	  (case endianness
	    ((little) (sref-little x k end))
	    ((big)    (sref-big x k end))
	    (else (die who "invalid endianness" endianness)))))))
  (define bytevector-uint-ref
    (lambda (x k endianness size)
      (define who 'bytevector-uint-ref)
      (unless (bytevector? x) (die who "not a bytevector" x))
      (unless (and (fixnum? k) ($fx>= k 0)) (die who "invalid index" k))
      (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
      (let ((n ($bytevector-length x)))
	(unless ($fx< k n) (die who "index is out of range" k))
	(let ((end ($fx+ k size)))
	  (unless (and ($fx>= end 0) ($fx<= end n))
	    (die who "out of range" k size))
	  (case endianness
	    ((little) (uref-little x k end))
	    ((big)    (uref-big x k end))
	    (else (die who "invalid endianness" endianness)))))))
  (define (bytevector->some-list x k n ls proc who)
    (cond
     (($fx= n 0) ls)
     (else
      (let ((i ($fx- n k)))
	(cond
	 (($fx>= i 0)
	  (bytevector->some-list x k i (cons (proc x i n) ls) proc who))
	 (else
	  (die who "invalid size" k)))))))
  (define bytevector->uint-list
    (lambda (x endianness size)
      (define who 'bytevector->uint-list)
      (unless (bytevector? x) (die who "not a bytevector" x))
      (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
      (case endianness
	((little) (bytevector->some-list x size ($bytevector-length x)
					 '() uref-little 'bytevector->uint-list))
	((big)    (bytevector->some-list x size ($bytevector-length x)
					 '() uref-big 'bytevector->uint-list))
	(else (die who "invalid endianness" endianness)))))
  (define bytevector->sint-list
    (lambda (x endianness size)
      (define who 'bytevector->sint-list)
      (unless (bytevector? x) (die who "not a bytevector" x))
      (unless (and (fixnum? size) ($fx>= size 1)) (die who "invalid size" size))
      (case endianness
	((little) (bytevector->some-list x size ($bytevector-length x)
					 '() sref-little 'bytevector->sint-list))
	((big)    (bytevector->some-list x size ($bytevector-length x)
					 '() sref-big 'bytevector->sint-list))
	(else (die who "invalid endianness" endianness))))))


(define (bytevector-uint-set! bv i0 n endianness size)
  (define who 'bytevector-uint-set!)
  (bytevector-uint-set!/who bv i0 n endianness size who))

(define (bytevector-uint-set!/who bv i0 n endianness size who)
  (unless (bytevector? bv)
    (die who "not a bytevector" bv))
  (unless (or (fixnum? n) (bignum? n))
    (die who "not an exact number" n))
  (unless (>= n 0)
    (die who "number must be positive" n))
  (let ((bvsize ($bytevector-length bv)))
    (unless (and (fixnum? i0)
		 ($fx>= i0 0)
		 ($fx< i0 bvsize))
      (die who "invalid index" i0))
    (unless (and (fixnum? size)
		 ($fx> size 0)
		 ($fx<= i0 ($fx- bvsize size)))
      (die who "invalid size" size)))
  (let ((nsize (bitwise-length n)))
    (when (< (* size 8) nsize)
      (die who
	   (format "number does not fit in ~a byte~a" size (if (= size 1) "" "s"))
	   n)))
  (case endianness
    ((little)
     (let f ((bv bv) (i0 i0) (i1 (fx+ i0 size)) (n n))
       (unless ($fx= i0 i1)
	 ($bytevector-u8-set! bv i0 (bitwise-and n 255))
	 (f bv ($fx+ i0 1) i1 (sra n 8)))))
    ((big)
     (let f ((bv bv) (i0 i0) (i1 (fx+ i0 size)) (n n))
       (unless ($fx= i0 i1)
	 (let ((i1 ($fx- i1 1)))
	   ($bytevector-u8-set! bv i1 (bitwise-and n 255))
	   (f bv i0 i1 (sra n 8))))))
    (else (die who "invalid endianness" endianness))))


(define (bytevector-sint-set! bv i0 n endianness size)
  (define who 'bytevector-sint-set!)
  (bytevector-sint-set!/who bv i0 n endianness size who))

(define (bytevector-sint-set!/who bv i0 n endianness size who)
  (unless (bytevector? bv)
    (die who "not a bytevector" bv))
  (unless (or (fixnum? n) (bignum? n))
    (die who "not an exact number" n))
  (let ((bvsize ($bytevector-length bv)))
    (unless (and (fixnum? i0)
		 ($fx>= i0 0)
		 ($fx< i0 bvsize))
      (die who "invalid index" i0))
    (unless (and (fixnum? size)
		 ($fx> size 0)
		 ($fx<= i0 ($fx- bvsize size)))
      (die who "invalid size" size)))
  (let ((nsize (+ (bitwise-length n) 1)))
    (when (< (* size 8) nsize)
      (die who "number does not fit" n)))
  (case endianness
    ((little)
     (let f ((bv bv) (i0 i0) (i1 (fx+ i0 size)) (n n))
       (unless ($fx= i0 i1)
	 ($bytevector-u8-set! bv i0 (bitwise-and n 255))
	 (f bv ($fx+ i0 1) i1 (sra n 8)))))
    ((big)
     (let f ((bv bv) (i0 i0) (i1 (fx+ i0 size)) (n n))
       (unless ($fx= i0 i1)
	 (let ((i1 ($fx- i1 1)))
	   ($bytevector-u8-set! bv i1 (bitwise-and n 255))
	   (f bv i0 i1 (sra n 8))))))
    (else (die who "invalid endianness" endianness))))


;;;; bytevector to fixed-length word list conversion

(define-syntax define-bytevector-to-byte-list
  (syntax-rules ()
    ((_ ?who ?bytevector-ref)
     (define (?who bv)
       (with-arguments-validation (?who)
	   ((bytevector bv))
	 (let loop ((bv	    bv)
		    (index  ($bytevector-length bv))
		    (accum  '()))
	   (if ($fxzero? index)
	       accum
	     (let ((j ($fxsub1 index)))
	       (loop bv j (cons (?bytevector-ref bv j) accum))))))))))

(define-bytevector-to-byte-list bytevector->u8-list $bytevector-u8-ref)
(define-bytevector-to-byte-list bytevector->s8-list $bytevector-s8-ref)

;;; --------------------------------------------------------------------

(define-syntax define-bytevector-to-word-list
  (syntax-rules ()
    ((_ ?who ?tag ?bytes-in-word ?bytevector-ref)
     (define (?who bv)
       (with-arguments-validation (?who)
	   ((bytevector bv))
	 (let* ((bv.len ($bytevector-length bv))
		(rest   (fxmod bv.len ?bytes-in-word)))
	   (unless ($fxzero? rest)
	     (assertion-violation ?who
	       "invalid bytevector size for requested type conversion" '?tag bv.len))
	   (let loop ((bv		bv)
		      (i		bv.len)
		      (accum	'()))
	     (if ($fxzero? i)
		 accum
	       (let ((j ($fx- i ?bytes-in-word)))
		 (loop bv j (cons (?bytevector-ref bv j) accum))))))))
     )))

(define-bytevector-to-word-list bytevector->u16l-list vu16l 2 $bytevector-u16l-ref)
(define-bytevector-to-word-list bytevector->u16b-list vu16b 2 $bytevector-u16b-ref)
(define-bytevector-to-word-list bytevector->u16n-list vu16n 2 $bytevector-u16n-ref)
(define-bytevector-to-word-list bytevector->s16l-list vs16l 2 $bytevector-s16l-ref)
(define-bytevector-to-word-list bytevector->s16b-list vs16b 2 $bytevector-s16b-ref)
(define-bytevector-to-word-list bytevector->s16n-list vs16n 2 $bytevector-s16n-ref)

(define-bytevector-to-word-list bytevector->u32l-list vu32l 4 $bytevector-u32l-ref)
(define-bytevector-to-word-list bytevector->u32b-list vu32b 4 $bytevector-u32b-ref)
(define-bytevector-to-word-list bytevector->u32n-list vu32n 4 $bytevector-u32n-ref)
(define-bytevector-to-word-list bytevector->s32l-list vs32l 4 $bytevector-s32l-ref)
(define-bytevector-to-word-list bytevector->s32b-list vs32b 4 $bytevector-s32b-ref)
(define-bytevector-to-word-list bytevector->s32n-list vs32n 4 $bytevector-s32n-ref)

(define-bytevector-to-word-list bytevector->u64l-list vu64l 8 $bytevector-u64l-ref)
(define-bytevector-to-word-list bytevector->u64b-list vu64b 8 $bytevector-u64b-ref)
(define-bytevector-to-word-list bytevector->u64n-list vu64n 8 $bytevector-u64n-ref)
(define-bytevector-to-word-list bytevector->s64l-list vs64l 8 $bytevector-s64l-ref)
(define-bytevector-to-word-list bytevector->s64b-list vs64b 8 $bytevector-s64b-ref)
(define-bytevector-to-word-list bytevector->s64n-list vs64n 8 $bytevector-s64n-ref)

(define-bytevector-to-word-list bytevector->f4l-list  vf4l  4 bytevector-flonum-single-le-ref)
(define-bytevector-to-word-list bytevector->f4b-list  vf4b  4 bytevector-flonum-single-be-ref)
(define-bytevector-to-word-list bytevector->f4n-list  vf4n  4 bytevector-flonum-single-ne-ref)
(define-bytevector-to-word-list bytevector->f8l-list  vf8l  8 bytevector-flonum-double-le-ref)
(define-bytevector-to-word-list bytevector->f8b-list  vf8b  8 bytevector-flonum-double-be-ref)
(define-bytevector-to-word-list bytevector->f8n-list  vf8n  8 bytevector-flonum-double-ne-ref)

(define-bytevector-to-word-list bytevector->c4l-list  vc4l  8 bytevector-cflonum-single-le-ref)
(define-bytevector-to-word-list bytevector->c4b-list  vc4b  8 bytevector-cflonum-single-be-ref)
(define-bytevector-to-word-list bytevector->c4n-list  vc4n  8 bytevector-cflonum-single-ne-ref)
(define-bytevector-to-word-list bytevector->c8l-list  vc8l 16 bytevector-cflonum-double-le-ref)
(define-bytevector-to-word-list bytevector->c8b-list  vc8b 16 bytevector-cflonum-double-be-ref)
(define-bytevector-to-word-list bytevector->c8n-list  vc8n 16 bytevector-cflonum-double-ne-ref)


;;;; fixed-length word list to bytevector conversion

(define-syntax define-byte-list-to-bytevector
  (syntax-rules ()
    ((_ ?who ?tag ?valid-number-pred ?bytevector-set!)
     (define (?who ls)
       (define (race h t ls n)
	 (if (pair? h)
	     (let ((h ($cdr h)))
	       (if (pair? h)
		   (if (not (eq? h t))
		       (race ($cdr h) ($cdr t) ls ($fx+ n 2))
		     (assertion-violation '?who "circular list" ls))
		 (if (null? h)
		     ($fx+ n 1)
		   (assertion-violation '?who "not a proper list" ls))))
	   (if (null? h)
	       n
	     (assertion-violation '?who "not a proper list" ls))))

       (define (fill s i ls)
	 (if (null? ls)
	     s
	   (let ((c ($car ls)))
	     (unless (?valid-number-pred c)
	       (assertion-violation '?who "not an octet" c))
	     ($bytevector-u8-set! s i c)
	     (fill s ($fxadd1 i) (cdr ls)))))

       (let* ((n (race ls ls ls 0))
	      (s ($make-bytevector n)))
	 (fill s 0 ls))))))

(define-byte-list-to-bytevector u8-list->bytevector
  vu8 words.word-u8? $bytevector-u8-set!)
(define-byte-list-to-bytevector s8-list->bytevector
  vs8 words.word-s8? $bytevector-s8-set!)

;;; --------------------------------------------------------------------

(define-syntax define-word-list-to-bytevector
  (syntax-rules ()
    ((_ ?who ?tag ?valid-number-pred ?bytes-in-word ?bytevector-set!)
     (define (?who ls)
       (define (%race h t ls n)
	 (cond ((pair? h)
		(let ((h ($cdr h)))
		  (if (pair? h)
		      (if (not (eq? h t))
			  (%race ($cdr h) ($cdr t) ls (+ n 2))
			(assertion-violation ?who "circular list" ls))
		    (if (null? h)
			(+ n 1)
		      (assertion-violation ?who "not a proper list" ls)))))
	       ((null? h)
		n)
	       (else
		(assertion-violation ?who "not a proper list" ls))))

       (define (%fill s i ls)
	 (if (null? ls)
	     s
	   (let ((c ($car ls)))
	     (unless (?valid-number-pred c)
	       (assertion-violation ?who "invalid element for requested bytevector type" '?tag c))
	     (?bytevector-set! s i c)
	     (%fill s ($fx+ ?bytes-in-word i) (cdr ls)))))

       (let* ((number-of-words (%race ls ls ls 0))
	      (bv.len	       (* ?bytes-in-word number-of-words)))
	 (unless (fixnum? bv.len)
	   (%implementation-violation ?who "resulting bytevector size must be a fixnum" (list bv.len)))
	 (%fill ($make-bytevector bv.len) 0 ls)))
     )))

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector u16l-list->bytevector
  'vu16l		       ;tag
  words.word-u16?	       ;to validate numbers
  2			       ;number of bytes in word
  $bytevector-u16l-set!) ;setter

(define-word-list-to-bytevector u16b-list->bytevector
  'vu16b		       ;tag
  words.word-u16?	       ;to validate numbers
  2			       ;number of bytes in word
  $bytevector-u16b-set!) ;setter

(define-word-list-to-bytevector u16n-list->bytevector
  'vu16n		       ;tag
  words.word-u16?	       ;to validate numbers
  2			       ;number of bytes in word
  $bytevector-u16n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector s16l-list->bytevector
  'vs16l		       ;tag
  words.word-s16?	      ;to validate numbers
  2			       ;number of bytes in word
  $bytevector-s16l-set!) ;setter

(define-word-list-to-bytevector s16b-list->bytevector
  'vs16b		       ;tag
  words.word-s16?	       ;to validate numbers
  2			       ;number of bytes in word
  $bytevector-s16b-set!) ;setter

(define-word-list-to-bytevector s16n-list->bytevector
  'vs16n		       ;tag
  words.word-s16?	       ;to validate numbers
  2			       ;number of bytes in word
  $bytevector-s16n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector u32l-list->bytevector
  'vu32l		       ;tag
  words.word-u32?	       ;to validate numbers
  4			       ;number of bytes in word
  $bytevector-u32l-set!) ;setter

(define-word-list-to-bytevector u32b-list->bytevector
  'vu32b		       ;tag
  words.word-u32?	       ;to validate numbers
  4			       ;number of bytes in word
  $bytevector-u32b-set!) ;setter

(define-word-list-to-bytevector u32n-list->bytevector
  'vu32n		       ;tag
  words.word-u32?	       ;to validate numbers
  4			       ;number of bytes in word
  $bytevector-u32n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector s32l-list->bytevector
  'vs32l		       ;tag
  words.word-s32?	       ;to validate numbers
  4			       ;number of bytes in word
  $bytevector-s32l-set!) ;setter

(define-word-list-to-bytevector s32b-list->bytevector
  'vs32b		       ;tag
  words.word-s32?	       ;to validate numbers
  4			       ;number of bytes in word
  $bytevector-s32b-set!) ;setter

(define-word-list-to-bytevector s32n-list->bytevector
  'vs32n		       ;tag
  words.word-s32?	       ;to validate numbers
  4			       ;number of bytes in word
  $bytevector-s32n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector u64l-list->bytevector
  'vu64l		       ;tag
  words.word-u64?	       ;to validate numbers
  8			       ;number of bytes in word
  $bytevector-u64l-set!) ;setter

(define-word-list-to-bytevector u64b-list->bytevector
  'vu64b		       ;tag
  words.word-u64?	       ;to validate numbers
  8			       ;number of bytes in word
  $bytevector-u64b-set!) ;setter

(define-word-list-to-bytevector u64n-list->bytevector
  'vu64n		       ;tag
  words.word-u64?	       ;to validate numbers
  8			       ;number of bytes in word
  $bytevector-u64n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector s64l-list->bytevector
  'vs64l		       ;tag
  words.word-s64?	       ;to validate numbers
  8			       ;number of bytes in word
  $bytevector-s64l-set!) ;setter

(define-word-list-to-bytevector s64b-list->bytevector
  'vs64b		       ;tag
  words.word-s64?	       ;to validate numbers
  8			       ;number of bytes in word
  $bytevector-s64b-set!) ;setter

(define-word-list-to-bytevector s64n-list->bytevector
  'vs64n		       ;tag
  words.word-s64?	       ;to validate numbers
  8			       ;number of bytes in word
  $bytevector-s64n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector f4l-list->bytevector
  'vf4l				    ;tag
  flonum?			    ;to validate numbers
  4				    ;number of bytes in word
  bytevector-flonum-single-le-set!) ;setter

(define-word-list-to-bytevector f4b-list->bytevector
  'vf4b				    ;tag
  flonum?			    ;to validate numbers
  4				    ;number of bytes in word
  bytevector-flonum-single-be-set!) ;setter

(define-word-list-to-bytevector f4n-list->bytevector
  'vf4n				    ;tag
  flonum?			    ;to validate numbers
  4				    ;number of bytes in word
  bytevector-flonum-single-ne-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector f8l-list->bytevector
  'vf8l				    ;tag
  flonum?			    ;to validate numbers
  8				    ;number of bytes in word
  bytevector-flonum-double-le-set!) ;setter

(define-word-list-to-bytevector f8b-list->bytevector
  'vf8b				    ;tag
  flonum?			    ;to validate numbers
  8				    ;number of bytes in word
  bytevector-flonum-double-be-set!) ;setter

(define-word-list-to-bytevector f8n-list->bytevector
  'vf8n				    ;tag
  flonum?			    ;to validate numbers
  8				    ;number of bytes in word
  bytevector-flonum-double-ne-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector c4l-list->bytevector
  'vc4l				     ;tag
  cflonum?			     ;to validate numbers
  8				     ;number of bytes in word
  bytevector-cflonum-single-le-set!) ;setter

(define-word-list-to-bytevector c4b-list->bytevector
  'vc4b				     ;tag
  cflonum?			     ;to validate numbers
  8				     ;number of bytes in word
  bytevector-cflonum-single-be-set!) ;setter

(define-word-list-to-bytevector c4n-list->bytevector
  'vc4n				     ;tag
  cflonum?			     ;to validate numbers
  8				     ;number of bytes in word
  bytevector-cflonum-single-ne-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector c8l-list->bytevector
  'vc8l				     ;tag
  cflonum?			     ;to validate numbers
  16				     ;number of bytes in word
  bytevector-cflonum-double-le-set!) ;setter

(define-word-list-to-bytevector c8b-list->bytevector
  'vc8b				     ;tag
  cflonum?			     ;to validate numbers
  16				     ;number of bytes in word
  bytevector-cflonum-double-be-set!) ;setter

(define-word-list-to-bytevector c8n-list->bytevector
  'vc8n				     ;tag
  cflonum?			     ;to validate numbers
  16				     ;number of bytes in word
  bytevector-cflonum-double-ne-set!) ;setter


;;;; any integer list to bytevector functions

(define (%make-xint-list->bytevector who bv-set!)
  (define (race h t ls idx endianness size)
    (if (pair? h)
	(let ((h ($cdr h)) (a ($car h)))
	  (if (pair? h)
	      (if (not (eq? h t))
		  (let ((bv (race ($cdr h) ($cdr t) ls
				  ($fx+ idx ($fx+ size size))
				  endianness size)))
		    (bv-set! bv idx a endianness size who)
		    (bv-set! bv ($fx+ idx size) ($car h) endianness size who)
		    bv)
		(die who "circular list" ls))
	    (if (null? h)
		(let ((bv (make-bytevector ($fx+ idx size))))
		  (bv-set! bv idx a endianness size who)
		  bv)
	      (die who "not a proper list" ls))))
      (if (null? h)
	  (make-bytevector idx)
	(die who "not a proper list" ls))))
  (lambda (ls endianness size)
    (if (and (fixnum? size) (fx> size 0))
	(race ls ls ls 0 endianness size)
      (die who "size must be a positive integer" size))))

(define uint-list->bytevector
  (%make-xint-list->bytevector 'uint-list->bytevector bytevector-uint-set!/who))

(define sint-list->bytevector
  (%make-xint-list->bytevector 'sint-list->bytevector bytevector-sint-set!/who))



#| end of library |#  )


(library (ikarus system bytevectors)
  (export $make-bytevector $bytevector-length
	  $bytevector-u8-ref $bytevector-set!)
  (import (ikarus))
  (define ($bytevector-set! dst.bv dst.index N)
    (if (<= 0 N)
	(bytevector-u8-set! dst.bv dst.index N)
      (bytevector-s8-set! dst.bv dst.index N)))
  (define $bytevector-u8-ref bytevector-u8-ref)
  (define $bytevector-length bytevector-length)
  (define $make-bytevector   make-bytevector))

;;; end of file
;;Local Variables:
;;coding: utf-8-unix
;;eval: (put '%implementation-violation	'scheme-indent-function 1)
;;eval: (put 'case-endianness		'scheme-indent-function 1)
;;End:
