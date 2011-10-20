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
    make-bytevector bytevector-length bytevector-s8-ref
    bytevector-u8-ref bytevector-u8-set! bytevector-s8-set!
    bytevector-copy! u8-list->bytevector bytevector->u8-list
    bytevector-u16-native-ref bytevector-u16-native-set!
    bytevector-s16-native-ref bytevector-s16-native-set!
    bytevector-u32-native-ref bytevector-u32-native-set!
    bytevector-s32-native-ref bytevector-s32-native-set!
    bytevector-u64-native-ref bytevector-u64-native-set!
    bytevector-s64-native-ref bytevector-s64-native-set!
    bytevector-u16-ref bytevector-u16-set!
    bytevector-s16-ref bytevector-s16-set!
    bytevector-u32-ref bytevector-u32-set!
    bytevector-s32-ref bytevector-s32-set!
    bytevector-u64-ref bytevector-u64-set!
    bytevector-s64-ref bytevector-s64-set!
    bytevector-fill! bytevector-copy bytevector=?
    bytevector-uint-ref bytevector-sint-ref
    bytevector-uint-set!  bytevector-sint-set!
    bytevector->uint-list bytevector->sint-list
    uint-list->bytevector sint-list->bytevector
    bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
    bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
    bytevector-ieee-double-ref bytevector-ieee-double-set!
    bytevector-ieee-single-ref bytevector-ieee-single-set!
    native-endianness

    ;; Vicare-specific functions
    subbytevector-u8		subbytevector-u8/count
    subbytevector-s8		subbytevector-s8/count

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

    bytevector-append)
  (import (except (ikarus)
		  make-bytevector bytevector-length bytevector-s8-ref
		  bytevector-u8-ref bytevector-u8-set! bytevector-s8-set!
		  bytevector-copy! u8-list->bytevector bytevector->u8-list
		  bytevector-u16-native-ref bytevector-u16-native-set!
		  bytevector-s16-native-ref bytevector-s16-native-set!
		  bytevector-u32-native-ref bytevector-u32-native-set!
		  bytevector-s32-native-ref bytevector-s32-native-set!
		  bytevector-u64-native-ref bytevector-u64-native-set!
		  bytevector-s64-native-ref bytevector-s64-native-set!
		  bytevector-u16-ref bytevector-u16-set!
		  bytevector-s16-ref bytevector-s16-set!
		  bytevector-u32-ref bytevector-u32-set!
		  bytevector-s32-ref bytevector-s32-set!
		  bytevector-u64-ref bytevector-u64-set!
		  bytevector-s64-ref bytevector-s64-set!
		  bytevector-fill! bytevector-copy bytevector=?
		  bytevector-uint-ref bytevector-sint-ref
		  bytevector-uint-set!  bytevector-sint-set!
		  bytevector->uint-list bytevector->sint-list
		  uint-list->bytevector sint-list->bytevector
		  bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!
		  bytevector-ieee-double-ref bytevector-ieee-double-set!
		  bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
		  bytevector-ieee-single-ref bytevector-ieee-single-set!
		  native-endianness

		  ;; Vicare-specific functions
		  subbytevector-u8		subbytevector-u8/count
		  subbytevector-s8		subbytevector-s8/count

		  s8-list->bytevector		bytevector->s8-list

		  u16l-list->bytevector		bytevector->u16l-list
		  u16b-list->bytevector		bytevector->u16b-list
		  u16n-list->bytevector		bytevector->u16n-list
		  s16l-list->bytevector		bytevector->s16l-list
		  s16b-list->bytevector		bytevector->s16b-list
		  s16n-list->bytevector		bytevector->s16n-list

		  u32l-list->bytevector		bytevector->u32l-list
		  u32b-list->bytevector		bytevector->u32b-list
		  u32n-list->bytevector		bytevector->u32n-list
		  s32l-list->bytevector		bytevector->s32l-list
		  s32b-list->bytevector		bytevector->s32b-list
		  s32n-list->bytevector		bytevector->s32n-list

		  u64l-list->bytevector		bytevector->u64l-list
		  u64b-list->bytevector		bytevector->u64b-list
		  u64n-list->bytevector		bytevector->u64n-list
		  s64l-list->bytevector		bytevector->s64l-list
		  s64b-list->bytevector		bytevector->s64b-list
		  s64n-list->bytevector		bytevector->s64n-list

		  bytevector-append)
    (ikarus system $fx)
    (ikarus system $bignums)
    (ikarus system $pairs)
    (ikarus system $bytevectors)
    (prefix (rename (ikarus system $bytevectors)
		    ($make-bytevector		make-bytevector)
		    ($bytevector-length		bytevector-length)
		    ($bytevector-u8-ref		bytevector-u8-ref)
		    ($bytevector-s8-ref		bytevector-s8-ref)
		    ($bytevector-set!		bytevector-set!)
		    ($bytevector-set!		bytevector-u8-set!)
		    ($bytevector-set!		bytevector-s8-set!)
		    ($bytevector-ieee-double-native-ref		bytevector-ieee-double-native-ref)
		    ($bytevector-ieee-double-nonnative-ref	bytevector-ieee-double-nonnative-ref)
		    ($bytevector-ieee-double-native-set!	bytevector-ieee-double-native-set!)
		    ($bytevector-ieee-single-native-ref		bytevector-ieee-single-native-ref)
		    ($bytevector-ieee-single-native-set!	bytevector-ieee-single-native-set!)
		    ($bytevector-ieee-single-nonnative-ref	bytevector-ieee-single-nonnative-ref)
		    ($bytevector-ieee-double-nonnative-set!	bytevector-ieee-double-nonnative-set!)
		    ($bytevector-ieee-single-nonnative-set!	bytevector-ieee-single-nonnative-set!))
	    unsafe.))


;;;; syntax helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


;;;; helpers

(define (native-endianness) 'little) ;;; HARDCODED

(define (%unsafe.bytevector-fill x i j fill)
  (if ($fx= i j)
      x
    (begin
      (unsafe.bytevector-set! x i fill)
      (%unsafe.bytevector-fill x ($fxadd1 i) j fill))))

(define-inline (%implementation-violation who msg . irritants)
  (raise (condition
	  (make-assertion-violation)
	  (make-implementation-restriction-violation)
	  (make-who-condition who)
	  (make-message-condition msg)
	  (make-irritants-condition irritants))))

;;; --------------------------------------------------------------------

(define-inline (%bytevector-u16l-set! bv index value)
  (bytevector-u16-set! bv index value (endianness little)))

(define-inline (%bytevector-u16b-set! bv index value)
  (bytevector-u16-set! bv index value (endianness big)))

(define-inline (%bytevector-u16n-set! bv index value)
  (bytevector-u16-native-set! bv index value))

(define-inline (%bytevector-u16l-ref bv index)
  (bytevector-u16-ref bv index (endianness little)))

(define-inline (%bytevector-u16b-ref bv index)
  (bytevector-u16-ref bv index (endianness big)))

(define-inline (%bytevector-u16n-ref bv index)
  (bytevector-u16-native-ref bv index))

;;; --------------------------------------------------------------------

(define-inline (%bytevector-s16l-set! bv index value)
  (bytevector-s16-set! bv index value (endianness little)))

(define-inline (%bytevector-s16b-set! bv index value)
  (bytevector-s16-set! bv index value (endianness big)))

(define-inline (%bytevector-s16n-set! bv index value)
  (bytevector-s16-native-set! bv index value))

(define-inline (%bytevector-s16l-ref bv index)
  (bytevector-s16-ref bv index (endianness little)))

(define-inline (%bytevector-s16b-ref bv index)
  (bytevector-s16-ref bv index (endianness big)))

(define-inline (%bytevector-s16n-ref bv index)
  (bytevector-s16-native-ref bv index))

;;; --------------------------------------------------------------------

(define-inline (%bytevector-u32l-set! bv index value)
  (bytevector-u32-set! bv index value (endianness little)))

(define-inline (%bytevector-u32b-set! bv index value)
  (bytevector-u32-set! bv index value (endianness big)))

(define-inline (%bytevector-u32n-set! bv index value)
  (bytevector-u32-native-set! bv index value))

(define-inline (%bytevector-u32l-ref bv index)
  (bytevector-u32-ref bv index (endianness little)))

(define-inline (%bytevector-u32b-ref bv index)
  (bytevector-u32-ref bv index (endianness big)))

(define-inline (%bytevector-u32n-ref bv index)
  (bytevector-u32-native-ref bv index))

;;; --------------------------------------------------------------------

(define-inline (%bytevector-s32l-set! bv index value)
  (bytevector-s32-set! bv index value (endianness little)))

(define-inline (%bytevector-s32b-set! bv index value)
  (bytevector-s32-set! bv index value (endianness big)))

(define-inline (%bytevector-s32n-set! bv index value)
  (bytevector-s32-native-set! bv index value))

(define-inline (%bytevector-s32l-ref bv index)
  (bytevector-s32-ref bv index (endianness little)))

(define-inline (%bytevector-s32b-ref bv index)
  (bytevector-s32-ref bv index (endianness big)))

(define-inline (%bytevector-s32n-ref bv index)
  (bytevector-s32-native-ref bv index))

;;; --------------------------------------------------------------------

(define-inline (%bytevector-u64l-set! bv index value)
  (bytevector-u64-set! bv index value (endianness little)))

(define-inline (%bytevector-u64b-set! bv index value)
  (bytevector-u64-set! bv index value (endianness big)))

(define-inline (%bytevector-u64n-set! bv index value)
  (bytevector-u64-native-set! bv index value))

(define-inline (%bytevector-u64l-ref bv index)
  (bytevector-u64-ref bv index (endianness little)))

(define-inline (%bytevector-u64b-ref bv index)
  (bytevector-u64-ref bv index (endianness big)))

(define-inline (%bytevector-u64n-ref bv index)
  (bytevector-u64-native-ref bv index))

;;; --------------------------------------------------------------------

(define-inline (%bytevector-s64l-set! bv index value)
  (bytevector-s64-set! bv index value (endianness little)))

(define-inline (%bytevector-s64b-set! bv index value)
  (bytevector-s64-set! bv index value (endianness big)))

(define-inline (%bytevector-s64n-set! bv index value)
  (bytevector-s64-native-set! bv index value))

(define-inline (%bytevector-s64l-ref bv index)
  (bytevector-s64-ref bv index (endianness little)))

(define-inline (%bytevector-s64b-ref bv index)
  (bytevector-s64-ref bv index (endianness big)))

(define-inline (%bytevector-s64n-ref bv index)
  (bytevector-s64-native-ref bv index))

;;; --------------------------------------------------------------------

(define-inline (%u16? num)	(and (integer? num) (exact? num)))
(define-inline (%s16? num)	(and (integer? num) (exact? num)))
(define-inline (%u32? num)	(and (integer? num) (exact? num)))
(define-inline (%s32? num)	(and (integer? num) (exact? num)))
(define-inline (%u64? num)	(and (integer? num) (exact? num)))
(define-inline (%s64? num)	(and (integer? num) (exact? num)))

;;; --------------------------------------------------------------------

(define U16MAX		(- (expt 2 16) 1))
(define U16MIN		0)
(define S16MAX		(- (expt 2 15) 1))
(define S16MIN		(- (expt 2 15)))

(define U32MAX		(- (expt 2 32) 1))
(define U32MIN		0)
(define S32MAX		(- (expt 2 31) 1))
(define S32MIN		(- (expt 2 31)))

(define U64MAX		(- (expt 2 64) 1))
(define U64MIN		0)
(define S64MAX		(- (expt 2 63) 1))
(define S64MIN		(- (expt 2 63)))


;;;; assertion helpers

(define-inline (%assert-argument-is-bytevector ?arg ?who)
  (unless (bytevector? ?arg)
    (assertion-violation ?who "expected a bytevector as first argument" ?arg)))

;;; --------------------------------------------------------------------

(let-syntax
    ((%define-assertion
      (syntax-rules ()
	((_ ??who ??size ??message)
	 (define-inline (??who ?arg bv ?who)
	   (unless (and (integer? ?arg) (exact? ?arg))
	     (assertion-violation ?who
	       "expected exact integer as start index argument for bytevector" ?arg))
	   (unless (and (>= ?arg 0)
			(<= ?arg (- (unsafe.bytevector-length bv) ??size)))
	     (assertion-violation ?who ??message ?arg)))))))

  (%define-assertion %assert-argument-is-bytevector-start-index-8 1
		     "start index argument out of range for bytevector")
  (%define-assertion %assert-argument-is-bytevector-start-index-16 2
		     "start index argument out of range for bytevector of 16-bit words")
  (%define-assertion %assert-argument-is-bytevector-start-index-32 4
		     "start index argument out of range for bytevector of 32-bit words")
  (%define-assertion %assert-argument-is-bytevector-start-index-64 8
		     "start index argument out of range for bytevector of 64-bit words"))

;;; --------------------------------------------------------------------

(let-syntax
    ((%define-assertion
      (syntax-rules ()
	((_ ??who ??size ??message)
	 (define-inline (??who ?arg bv ?who)
	   (unless (and (integer? ?arg) (exact? ?arg))
	     (assertion-violation ?who
	       "expected exact integer as end index argument for bytevector" ?arg))
	   (unless (and (>= ?arg 0)
			(<= ?arg (unsafe.bytevector-length bv)))
	     (assertion-violation ?who ??message ?arg)))))))

  (%define-assertion %assert-argument-is-bytevector-end-index-8 1
		     "end index argument out of range for bytevector")
  (%define-assertion %assert-argument-is-bytevector-end-index-16 2
		     "end index argument out of range for bytevector of 16-bit words")
  (%define-assertion %assert-argument-is-bytevector-end-index-32 4
		     "end index argument out of range for bytevector of 32-bit words")
  (%define-assertion %assert-argument-is-bytevector-end-index-64 8
		     "end index argument out of range for bytevector of 64-bit words"))

;;; --------------------------------------------------------------------

(let-syntax
    ((%define-assertion
      (syntax-rules ()
	((_ ??who ??size ??message)
	 (define-inline (??who ?arg ?bv ?start ?who)
	   (unless (and (integer? ?arg) (exact? ?arg))
	     (assertion-violation ?who
	       "expected exact integer as count argument for bytevector" ?arg))
	   (let ((end (+ ?start (* ?arg ??size))))
	     (unless (and (>= end 0)
			  (<= end (unsafe.bytevector-length ?bv)))
	       (assertion-violation ?who ??message ?arg))))))))

  (%define-assertion %assert-argument-is-bytevector-count-8 1
		     "count argument out of range for bytevector and given start index")
  (%define-assertion %assert-argument-is-bytevector-count-16 2
		     "count argument out of range for bytevector of 16-bit words and given start index")
  (%define-assertion %assert-argument-is-bytevector-count-32 4
		     "count argument out of range for bytevector of 32-bit words and given start index")
  (%define-assertion %assert-argument-is-bytevector-count-64 8
		     "count argument out of range for bytevector of 64-bit words and given start index"))


;;;; main bytevector handling functions

(define make-bytevector
  (case-lambda
   ((k)
    (if (and (fixnum? k) ($fx>= k 0))
	($make-bytevector k)
      (die 'make-bytevector "not a valid size" k)))
   ((k fill)
    (if (and (fixnum? fill) ($fx<= -128 fill) ($fx<= fill 255))
	(%unsafe.bytevector-fill (make-bytevector k) 0 k fill)
      (die 'make-bytevector "not a valid fill" fill)))))

(define bytevector-fill!
  (lambda (x fill)
    (unless (bytevector? x)
      (die 'bytevector-fill! "not a bytevector" x))
    (unless (and (fixnum? fill) ($fx<= -128 fill) ($fx<= fill 255))
      (die 'bytevector-fill! "not a valid fill" fill))
    (%unsafe.bytevector-fill x 0 ($bytevector-length x) fill)))

(define bytevector-length
  (lambda (x)
    (if (bytevector? x)
	($bytevector-length x)
      (die 'bytevector-length "not a bytevector" x))))

(define bytevector=?
  (lambda (x y)
    (unless (bytevector? x)
      (die 'bytevector=? "not a bytevector" x))
    (unless (bytevector? y)
      (die 'bytevector=? "not a bytevector" y))
    (let ((n ($bytevector-length x)))
      (and ($fx= n ($bytevector-length y))
	   (let f ((x x) (y y) (i 0) (n n))
	     (or ($fx= i n)
		 (and ($fx= ($bytevector-u8-ref x i)
			    ($bytevector-u8-ref y i))
		      (f x y ($fxadd1 i) n))))))))


;;;; subbytevectors, bytes

(define subbytevector-u8
  (case-lambda
   ((src.bv src.start)
    (define who 'subbytevector-u8)
    (%assert-argument-is-bytevector src.bv who)
    (subbytevector-u8 src.bv src.start (unsafe.bytevector-length src.bv)))
   ((src.bv src.start src.end)
    (define who 'subbytevector-u8)
    (%assert-argument-is-bytevector src.bv 'subbytevector-u8)
    (%assert-argument-is-bytevector-start-index-8 src.start src.bv who)
    (%assert-argument-is-bytevector-end-index-8   src.end   src.bv who)
    (%unsafe.subbytevector-u8/count src.bv src.start (- src.end src.start)))))

(define (subbytevector-u8/count src.bv src.start dst.len)
  (define who 'subbytevector-u8/count)
  (%assert-argument-is-bytevector src.bv 'subbytevector-u8)
  (%assert-argument-is-bytevector-start-index-8 src.start src.bv who)
  (%assert-argument-is-bytevector-count-8 dst.len src.bv src.start who)
  (%unsafe.subbytevector-u8/count src.bv src.start dst.len))

(define (%unsafe.subbytevector-u8/count src.bv src.start dst.len)
  (let ((dst.bv ($make-bytevector dst.len)))
    (do ((dst.index 0         (+ 1 dst.index))
	 (src.index src.start (+ 1 src.index)))
	((= dst.index dst.len)
	 dst.bv)
      (unsafe.bytevector-set! dst.bv dst.index (unsafe.bytevector-u8-ref src.bv src.index)))))

(define subbytevector-s8
  (case-lambda
   ((src.bv src.start)
    (define who 'subbytevector-s8)
    (%assert-argument-is-bytevector src.bv who)
    (subbytevector-s8 src.bv src.start (unsafe.bytevector-length src.bv)))
   ((src.bv src.start src.end)
    (define who 'subbytevector-s8)
    (%assert-argument-is-bytevector src.bv 'subbytevector-s8)
    (%assert-argument-is-bytevector-start-index-8 src.start src.bv who)
    (%assert-argument-is-bytevector-end-index-8   src.end   src.bv who)
    (%unsafe.subbytevector-s8/count src.bv src.start (- src.end src.start)))))

(define (subbytevector-s8/count src.bv src.start dst.len)
  (define who 'subbytevector-s8/count)
  (%assert-argument-is-bytevector src.bv 'subbytevector-s8)
  (%assert-argument-is-bytevector-start-index-8 src.start src.bv who)
  (%assert-argument-is-bytevector-count-8 dst.len src.bv src.start who)
  (%unsafe.subbytevector-s8/count src.bv src.start dst.len))

(define (%unsafe.subbytevector-s8/count src.bv src.start dst.len)
  (let ((dst.bv ($make-bytevector dst.len)))
    (do ((dst.index 0         (+ 1 dst.index))
	 (src.index src.start (+ 1 src.index)))
	((= dst.index dst.len)
	 dst.bv)
      (unsafe.bytevector-set! dst.bv dst.index (unsafe.bytevector-s8-ref src.bv src.index)))))


;;;; setters and getters

(define bytevector-s8-ref
  (lambda (x i)
    (if (bytevector? x)
	(if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
	    ($bytevector-s8-ref x i)
	  (die 'bytevector-s8-ref "invalid index" i x))
      (die 'bytevector-s8-ref "not a bytevector" x))))

(define bytevector-u8-ref
  (lambda (x i)
    (if (bytevector? x)
	(if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
	    ($bytevector-u8-ref x i)
	  (die 'bytevector-u8-ref "invalid index" i x))
      (die 'bytevector-u8-ref "not a bytevector" x))))


(define bytevector-s8-set!
  (lambda (x i v)
    (if (bytevector? x)
	(if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
	    (if (and (fixnum? v) ($fx<= -128 v) ($fx<= v 127))
		($bytevector-set! x i v)
	      (die 'bytevector-s8-set! "not a byte" v))
	  (die 'bytevector-s8-set! "invalid index" i x))
      (die 'bytevector-s8-set! "not a bytevector" x))))

(define bytevector-u8-set!
  (lambda (x i v)
    (if (bytevector? x)
	(if (and (fixnum? i) ($fx<= 0 i) ($fx< i ($bytevector-length x)))
	    (if (and (fixnum? v) ($fx<= 0 v) ($fx<= v 255))
		($bytevector-set! x i v)
	      (die 'bytevector-u8-set! "not an octet" v))
	  (die 'bytevector-u8-set! "invalid index" i x))
      (die 'bytevector-u8-set! "not a bytevector" x))))

(define bytevector-u16-native-ref ;;; HARDCODED
  (lambda (x i)
    (if (bytevector? x)
	(if (and (fixnum? i)
		 ($fx<= 0 i)
		 ($fx< i ($fxsub1 ($bytevector-length x)))
		 ($fxzero? ($fxlogand i 1)))
	    ($fxlogor ;;; little
	     ($bytevector-u8-ref x i)
	     ($fxsll ($bytevector-u8-ref x ($fxadd1 i)) 8))
	  (die 'bytevector-u16-native-ref "invalid index" i))
      (die 'bytevector-u16-native-ref "not a bytevector" x))))


(define bytevector-u16-native-set! ;;; HARDCODED
  (lambda (x i n)
    (if (bytevector? x)
	(if (and (fixnum? n)
		 ($fx<= 0 n)
		 ($fx<= n #xFFFF))
	    (if (and (fixnum? i)
		     ($fx<= 0 i)
		     ($fx< i ($fxsub1 ($bytevector-length x)))
		     ($fxzero? ($fxlogand i 1)))
		(begin ;;; little
		  ($bytevector-set! x i n)
		  ($bytevector-set! x ($fxadd1 i) ($fxsra n 8)))
	      (die 'bytevector-u16-native-set! "invalid index" i))
	  (die 'bytevector-u16-native-set! "invalid value" n))
      (die 'bytevector-u16-native-set! "not a bytevector" x))))

(define bytevector-s16-native-set! ;;; HARDCODED
  (lambda (x i n)
    (if (bytevector? x)
	(if (and (fixnum? n)
		 ($fx<= #x-8000 n)
		 ($fx<= n #x7FFF))
	    (if (and (fixnum? i)
		     ($fx<= 0 i)
		     ($fx< i ($fxsub1 ($bytevector-length x)))
		     ($fxzero? ($fxlogand i 1)))
		(begin ;;; little
		  ($bytevector-set! x i n)
		  ($bytevector-set! x ($fxadd1 i) ($fxsra n 8)))
	      (die 'bytevector-s16-native-set! "invalid index" i))
	  (die 'bytevector-s16-native-set! "invalid value" n))
      (die 'bytevector-s16-native-set! "not a bytevector" x))))

(define bytevector-s16-native-ref ;;; HARDCODED
  (lambda (x i)
    (if (bytevector? x)
	(if (and (fixnum? i)
		 ($fx<= 0 i)
		 ($fx< i ($fxsub1 ($bytevector-length x)))
		 ($fxzero? ($fxlogand i 1)))
	    ($fxlogor ;;; little
	     ($bytevector-u8-ref x i)
	     ($fxsll ($bytevector-s8-ref x ($fxadd1 i)) 8))
	  (die 'bytevector-s16-native-ref "invalid index" i))
      (die 'bytevector-s16-native-ref "not a bytevector" x))))

(define bytevector-u16-ref
  (lambda (x i end)
    (if (bytevector? x)
	(if (and (fixnum? i)
		 ($fx<= 0 i)
		 ($fx< i ($fxsub1 ($bytevector-length x))))
	    (case end
	      ((big)
	       ($fxlogor
		($fxsll ($bytevector-u8-ref x i) 8)
		($bytevector-u8-ref x ($fxadd1 i))))
	      ((little)
	       ($fxlogor
		($fxsll ($bytevector-u8-ref x (fxadd1 i)) 8)
		($bytevector-u8-ref x i)))
	      (else (die 'bytevector-u16-ref "invalid endianness" end)))
	  (die 'bytevector-u16-ref "invalid index" i))
      (die 'bytevector-u16-ref "not a bytevector" x))))

(define bytevector-u32-ref
  (lambda (x i end)
    (if (bytevector? x)
	(if (and (fixnum? i)
		 ($fx<= 0 i)
		 ($fx< i ($fx- ($bytevector-length x) 3)))
	    (case end
	      ((big)
	       (+ (sll ($bytevector-u8-ref x i) 24)
		  ($fxlogor
		   ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 16)
		   ($fxlogor
		    ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 8)
		    ($bytevector-u8-ref x ($fx+ i 3))))))
	      ((little)
	       (+ (sll ($bytevector-u8-ref x ($fx+ i 3)) 24)
		  ($fxlogor
		   ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 16)
		   ($fxlogor
		    ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 8)
		    ($bytevector-u8-ref x i)))))
	      (else (die 'bytevector-u32-ref "invalid endianness" end)))
	  (die 'bytevector-u32-ref "invalid index" i))
      (die 'bytevector-u32-ref "not a bytevector" x))))

(define bytevector-u32-native-ref
  (lambda (x i)
    (if (bytevector? x)
	(if (and (fixnum? i)
		 ($fx<= 0 i)
		 ($fx= 0 ($fxlogand i 3))
		 ($fx< i ($fx- ($bytevector-length x) 3)))
	    (+ (sll ($bytevector-u8-ref x ($fx+ i 3)) 24)
	       ($fxlogor
		($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 16)
		($fxlogor
		 ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 8)
		 ($bytevector-u8-ref x i))))
	  (die 'bytevector-u32-native-ref "invalid index" i))
      (die 'bytevector-u32-native-ref "not a bytevector" x))))

(define bytevector-s32-ref
  (lambda (x i end)
    (if (bytevector? x)
	(if (and (fixnum? i)
		 ($fx<= 0 i)
		 ($fx< i ($fx- ($bytevector-length x) 3)))
	    (case end
	      ((big)
	       (+ (sll ($bytevector-s8-ref x i) 24)
		  ($fxlogor
		   ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 16)
		   ($fxlogor
		    ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 8)
		    ($bytevector-u8-ref x ($fx+ i 3))))))
	      ((little)
	       (+ (sll ($bytevector-s8-ref x ($fx+ i 3)) 24)
		  ($fxlogor
		   ($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 16)
		   ($fxlogor
		    ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 8)
		    ($bytevector-u8-ref x i)))))
	      (else (die 'bytevector-s32-ref "invalid endianness" end)))
	  (die 'bytevector-s32-ref "invalid index" i))
      (die 'bytevector-s32-ref "not a bytevector" x))))

(define bytevector-s32-native-ref
  (lambda (x i)
    (if (bytevector? x)
	(if (and (fixnum? i)
		 ($fx<= 0 i)
		 ($fx= 0 ($fxlogand i 3))
		 ($fx< i ($fx- ($bytevector-length x) 3)))
	    (+ (sll ($bytevector-s8-ref x ($fx+ i 3)) 24)
	       ($fxlogor
		($fxsll ($bytevector-u8-ref x ($fx+ i 2)) 16)
		($fxlogor
		 ($fxsll ($bytevector-u8-ref x ($fx+ i 1)) 8)
		 ($bytevector-u8-ref x i))))
	  (die 'bytevector-s32-native-ref "invalid index" i))
      (die 'bytevector-s32-native-ref "not a bytevector" x))))

(define bytevector-u16-set!
  (lambda (x i n end)
    (if (bytevector? x)
	(if (and (fixnum? n)
		 ($fx<= 0 n)
		 ($fx<= n #xFFFF))
	    (if (and (fixnum? i)
		     ($fx<= 0 i)
		     ($fx< i ($fxsub1 ($bytevector-length x))))
		(case end
		  ((big)
		   ($bytevector-set! x i ($fxsra n 8))
		   ($bytevector-set! x ($fxadd1 i) n))
		  ((little)
		   ($bytevector-set! x i n)
		   ($bytevector-set! x ($fxadd1 i) (fxsra n 8)))
		  (else (die 'bytevector-u16-ref "invalid endianness" end)))
	      (die 'bytevector-u16-set! "invalid index" i))
	  (die 'bytevector-u16-set! "invalid value" n))
      (die 'bytevector-u16-set! "not a bytevector" x))))


(define bytevector-u32-set!
  (lambda (x i n end)
    (if (bytevector? x)
	(if (if (fixnum? n)
		($fx>= n 0)
	      (if (bignum? n)
		  (<= 0 n #xFFFFFFFF)
		#f))
	    (if (and (fixnum? i)
		     ($fx<= 0 i)
		     ($fx< i ($fx- ($bytevector-length x) 3)))
		(case end
		  ((big)
		   (let ((b (sra n 16)))
		     ($bytevector-set! x i ($fxsra b 8))
		     ($bytevector-set! x ($fx+ i 1) b))
		   (let ((b (bitwise-and n #xFFFF)))
		     ($bytevector-set! x ($fx+ i 2) ($fxsra b 8))
		     ($bytevector-set! x ($fx+ i 3) b)))
		  ((little)
		   (let ((b (sra n 16)))
		     ($bytevector-set! x ($fx+ i 3) ($fxsra b 8))
		     ($bytevector-set! x ($fx+ i 2) b))
		   (let ((b (bitwise-and n #xFFFF)))
		     ($bytevector-set! x ($fx+ i 1) ($fxsra b 8))
		     ($bytevector-set! x i b)))
		  (else (die 'bytevector-u32-ref "invalid endianness" end)))
	      (die 'bytevector-u32-set! "invalid index" i))
	  (die 'bytevector-u32-set! "invalid value" n))
      (die 'bytevector-u32-set! "not a bytevector" x))))

(define bytevector-u32-native-set!
  (lambda (x i n)
    (if (bytevector? x)
	(if (if (fixnum? n)
		($fx>= n 0)
	      (if (bignum? n)
		  (<= 0 n #xFFFFFFFF)
		#f))
	    (if (and (fixnum? i)
		     ($fx<= 0 i)
		     ($fx= 0 ($fxlogand i 3))
		     ($fx< i ($fx- ($bytevector-length x) 3)))
		(begin
		  (let ((b (sra n 16)))
		    ($bytevector-set! x ($fx+ i 3) ($fxsra b 8))
		    ($bytevector-set! x ($fx+ i 2) b))
		  (let ((b (bitwise-and n #xFFFF)))
		    ($bytevector-set! x ($fx+ i 1) ($fxsra b 8))
		    ($bytevector-set! x i b)))
	      (die 'bytevector-u32-native-set! "invalid index" i))
	  (die 'bytevector-u32-native-set! "invalid value" n))
      (die 'bytevector-u32-native-set! "not a bytevector" x))))


(define bytevector-s32-native-set!
  (lambda (x i n)
    (if (bytevector? x)
	(if (if (fixnum? n)
		#t
	      (if (bignum? n)
		  (<= #x-80000000 n #x7FFFFFFF)
		#f))
	    (if (and (fixnum? i)
		     ($fx<= 0 i)
		     ($fx= 0 ($fxlogand i 3))
		     ($fx< i ($fx- ($bytevector-length x) 3)))
		(begin
		  (let ((b (sra n 16)))
		    ($bytevector-set! x ($fx+ i 3) ($fxsra b 8))
		    ($bytevector-set! x ($fx+ i 2) b))
		  (let ((b (bitwise-and n #xFFFF)))
		    ($bytevector-set! x ($fx+ i 1) ($fxsra b 8))
		    ($bytevector-set! x i b)))
	      (die 'bytevector-s32-native-set! "invalid index" i))
	  (die 'bytevector-s32-native-set! "invalid value" n))
      (die 'bytevector-s32-native-set! "not a bytevector" x))))

(define bytevector-s32-set!
  (lambda (x i n end)
    (if (bytevector? x)
	(if (if (fixnum? n)
		#t
	      (if (bignum? n)
		  (<= #x-80000000 n #x7FFFFFFF)
		#f))
	    (if (and (fixnum? i)
		     ($fx<= 0 i)
		     ($fx< i ($fx- ($bytevector-length x) 3)))
		(case end
		  ((big)
		   (let ((b (sra n 16)))
		     ($bytevector-set! x i ($fxsra b 8))
		     ($bytevector-set! x ($fx+ i 1) b))
		   (let ((b (bitwise-and n #xFFFF)))
		     ($bytevector-set! x ($fx+ i 2) ($fxsra b 8))
		     ($bytevector-set! x ($fx+ i 3) b)))
		  ((little)
		   (let ((b (sra n 16)))
		     ($bytevector-set! x ($fx+ i 3) ($fxsra b 8))
		     ($bytevector-set! x ($fx+ i 2) b))
		   (let ((b (bitwise-and n #xFFFF)))
		     ($bytevector-set! x ($fx+ i 1) ($fxsra b 8))
		     ($bytevector-set! x i b)))
		  (else (die 'bytevector-s32-ref "invalid endianness" end)))
	      (die 'bytevector-s32-set! "invalid index" i))
	  (die 'bytevector-s32-set! "invalid value" n))
      (die 'bytevector-s32-set! "not a bytevector" x))))

(define bytevector-s16-ref
  (lambda (x i end)
    (if (bytevector? x)
	(if (and (fixnum? i)
		 ($fx<= 0 i)
		 ($fx< i ($fxsub1 ($bytevector-length x))))
	    (case end
	      ((big)
	       ($fxlogor
		($fxsll ($bytevector-s8-ref x i) 8)
		($bytevector-u8-ref x ($fxadd1 i))))
	      ((little)
	       ($fxlogor
		($fxsll ($bytevector-s8-ref x (fxadd1 i)) 8)
		($bytevector-u8-ref x i)))
	      (else (die 'bytevector-s16-ref "invalid endianness" end)))
	  (die 'bytevector-s16-ref "invalid index" i))
      (die 'bytevector-s16-ref "not a bytevector" x))))

(define bytevector-s16-set!
  (lambda (x i n end)
    (if (bytevector? x)
	(if (and (fixnum? n)
		 ($fx<= #x-8000 n)
		 ($fx<= n #x7FFF))
	    (if (and (fixnum? i)
		     ($fx<= 0 i)
		     ($fx< i ($fxsub1 ($bytevector-length x))))
		(case end
		  ((big)
		   ($bytevector-set! x i ($fxsra n 8))
		   ($bytevector-set! x ($fxadd1 i) n))
		  ((little)
		   ($bytevector-set! x i n)
		   ($bytevector-set! x ($fxadd1 i) (fxsra n 8)))
		  (else (die 'bytevector-s16-ref "invalid endianness" end)))
	      (die 'bytevector-s16-set! "invalid index" i))
	  (die 'bytevector-s16-set! "invalid value" n))
      (die 'bytevector-s16-set! "not a bytevector" x))))


;;;; bytevector to list conversion

(define (bytevector->u8-list bv)
  (define who 'bytevector->u8-list)
  (%assert-argument-is-bytevector bv who)
  (let loop ((bv	bv)
	     (i		($bytevector-length bv))
	     (accum	'()))
    (if ($fxzero? i)
	accum
      (let ((j ($fxsub1 i)))
	(loop bv j (cons ($bytevector-u8-ref bv j) accum))))))

(define (bytevector->s8-list bv)
  (define who 'bytevector->s8-list)
  (%assert-argument-is-bytevector bv who)
  (let loop ((bv	bv)
	     (i		($bytevector-length bv))
	     (accum	'()))
    (if ($fxzero? i)
	accum
      (let ((j ($fxsub1 i)))
	(loop bv j (cons ($bytevector-s8-ref bv j) accum))))))

;;; --------------------------------------------------------------------

(define-syntax define-bytevector-to-word-list
  (syntax-rules ()
    ((_ ?who ?tag ?bytes-in-word ?bytevector-ref)
     (define (?who bv)
       (%assert-argument-is-bytevector bv ?who)
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
	       (loop bv j (cons (?bytevector-ref bv j) accum))))))))))

(define-bytevector-to-word-list bytevector->u16l-list vu16l 2 %bytevector-u16l-ref)
(define-bytevector-to-word-list bytevector->u16b-list vu16b 2 %bytevector-u16b-ref)
(define-bytevector-to-word-list bytevector->u16n-list vu16n 2 %bytevector-u16n-ref)
(define-bytevector-to-word-list bytevector->s16l-list vs16l 2 %bytevector-s16l-ref)
(define-bytevector-to-word-list bytevector->s16b-list vs16b 2 %bytevector-s16b-ref)
(define-bytevector-to-word-list bytevector->s16n-list vs16n 2 %bytevector-s16n-ref)

(define-bytevector-to-word-list bytevector->u32l-list vu32l 4 %bytevector-u32l-ref)
(define-bytevector-to-word-list bytevector->u32b-list vu32b 4 %bytevector-u32b-ref)
(define-bytevector-to-word-list bytevector->u32n-list vu32n 4 %bytevector-u32n-ref)
(define-bytevector-to-word-list bytevector->s32l-list vs32l 4 %bytevector-s32l-ref)
(define-bytevector-to-word-list bytevector->s32b-list vs32b 4 %bytevector-s32b-ref)
(define-bytevector-to-word-list bytevector->s32n-list vs32n 4 %bytevector-s32n-ref)

(define-bytevector-to-word-list bytevector->u64l-list vu64l 8 %bytevector-u64l-ref)
(define-bytevector-to-word-list bytevector->u64b-list vu64b 8 %bytevector-u64b-ref)
(define-bytevector-to-word-list bytevector->u64n-list vu64n 8 %bytevector-u64n-ref)
(define-bytevector-to-word-list bytevector->s64l-list vs64l 8 %bytevector-s64l-ref)
(define-bytevector-to-word-list bytevector->s64b-list vs64b 8 %bytevector-s64b-ref)
(define-bytevector-to-word-list bytevector->s64n-list vs64n 8 %bytevector-s64n-ref)


;;;; list to bytevector conversion

(define u8-list->bytevector
  (letrec ((race
	    (lambda (h t ls n)
	      (if (pair? h)
		  (let ((h ($cdr h)))
		    (if (pair? h)
			(if (not (eq? h t))
			    (race ($cdr h) ($cdr t) ls ($fx+ n 2))
			  (die 'u8-list->bytevector "circular list" ls))
		      (if (null? h)
			  ($fx+ n 1)
			(die 'u8-list->bytevector "not a proper list" ls))))
		(if (null? h)
		    n
		  (die 'u8-list->bytevector "not a proper list" ls)))))
	   (fill
	    (lambda (s i ls)
	      (cond
	       ((null? ls) s)
	       (else
		(let ((c ($car ls)))
		  (unless (and (fixnum? c) ($fx<= 0 c) ($fx<= c 255))
		    (die 'u8-list->bytevector "not an octet" c))
		  ($bytevector-set! s i c)
		  (fill s ($fxadd1 i) (cdr ls))))))))
    (lambda (ls)
      (let ((n (race ls ls ls 0)))
	(let ((s ($make-bytevector n)))
	  (fill s 0 ls))))))

;;; --------------------------------------------------------------------

(define s8-list->bytevector
  (letrec ((race
	    (lambda (h t ls n)
	      (if (pair? h)
		  (let ((h ($cdr h)))
		    (if (pair? h)
			(if (not (eq? h t))
			    (race ($cdr h) ($cdr t) ls ($fx+ n 2))
			  (die 's8-list->bytevector "circular list" ls))
		      (if (null? h)
			  ($fx+ n 1)
			(die 's8-list->bytevector "not a proper list" ls))))
		(if (null? h)
		    n
		  (die 's8-list->bytevector "not a proper list" ls)))))
	   (fill
	    (lambda (s i ls)
	      (cond
	       ((null? ls) s)
	       (else
		(let ((c ($car ls)))
		  (unless (and (fixnum? c) ($fx<= -128 c) ($fx<= c 127))
		    (die 's8-list->bytevector "not an octet" c))
		  ($bytevector-set! s i c)
		  (fill s ($fxadd1 i) (cdr ls))))))))
    (lambda (ls)
      (let ((n (race ls ls ls 0)))
	(let ((s ($make-bytevector n)))
	  (fill s 0 ls))))))

;;; --------------------------------------------------------------------

(define-syntax define-word-list-to-bytevector
  (syntax-rules ()
    ((_ ?who ?tag ?number-pred ?<= ?number-min ?number-max ?bytes-in-word ?bytevector-set!)
     (define (?who ls)
       (define-inline (%valid-number? num)
	 (and (?number-pred num)
	      (?<= ?number-min num) (?<= num ?number-max)))

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
	     (unless (%valid-number? c)
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
  'vu16l		 ;tag
  %u16? <= U16MIN U16MAX ;to validate numbers
  2			 ;number of bytes in word
  %bytevector-u16l-set!) ;setter

(define-word-list-to-bytevector u16b-list->bytevector
  'vu16b		 ;tag
  %u16? <= U16MIN U16MAX ;to validate numbers
  2			 ;number of bytes in word
  %bytevector-u16b-set!) ;setter

(define-word-list-to-bytevector u16n-list->bytevector
  'vu16n		 ;tag
  %u16? <= U16MIN U16MAX ;to validate numbers
  2			 ;number of bytes in word
  %bytevector-u16n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector s16l-list->bytevector
  'vs16l		 ;tag
  %s16? <= S16MIN S16MAX ;to validate numbers
  2			 ;number of bytes in word
  %bytevector-s16l-set!) ;setter

(define-word-list-to-bytevector s16b-list->bytevector
  'vs16b		 ;tag
  %s16? <= S16MIN S16MAX ;to validate numbers
  2			 ;number of bytes in word
  %bytevector-s16b-set!) ;setter

(define-word-list-to-bytevector s16n-list->bytevector
  'vs16n		 ;tag
  %s16? <= S16MIN S16MAX ;to validate numbers
  2			 ;number of bytes in word
  %bytevector-s16n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector u32l-list->bytevector
  'vu32l		 ;tag
  %u32? <= U32MIN U32MAX ;to validate numbers
  4			 ;number of bytes in word
  %bytevector-u32l-set!) ;setter

(define-word-list-to-bytevector u32b-list->bytevector
  'vu32b		 ;tag
  %u32? <= U32MIN U32MAX ;to validate numbers
  4			 ;number of bytes in word
  %bytevector-u32b-set!) ;setter

(define-word-list-to-bytevector u32n-list->bytevector
  'vu32n		 ;tag
  %u32? <= U32MIN U32MAX ;to validate numbers
  4			 ;number of bytes in word
  %bytevector-u32n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector s32l-list->bytevector
  'vs32l		 ;tag
  %s32? <= S32MIN S32MAX ;to validate numbers
  4			 ;number of bytes in word
  %bytevector-s32l-set!) ;setter

(define-word-list-to-bytevector s32b-list->bytevector
  'vs32b		 ;tag
  %s32? <= S32MIN S32MAX ;to validate numbers
  4			 ;number of bytes in word
  %bytevector-s32b-set!) ;setter

(define-word-list-to-bytevector s32n-list->bytevector
  'vs32n		 ;tag
  %s32? <= S32MIN S32MAX ;to validate numbers
  4			 ;number of bytes in word
  %bytevector-s32n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector u64l-list->bytevector
  'vu64l		 ;tag
  %u64? <= U64MIN U64MAX ;to validate numbers
  8			 ;number of bytes in word
  %bytevector-u64l-set!) ;setter

(define-word-list-to-bytevector u64b-list->bytevector
  'vu64b		 ;tag
  %u64? <= U64MIN U64MAX ;to validate numbers
  8			 ;number of bytes in word
  %bytevector-u64b-set!) ;setter

(define-word-list-to-bytevector u64n-list->bytevector
  'vu64n		 ;tag
  %u64? <= U64MIN U64MAX ;to validate numbers
  8			 ;number of bytes in word
  %bytevector-u64n-set!) ;setter

;;; --------------------------------------------------------------------

(define-word-list-to-bytevector s64l-list->bytevector
  'vs64l		 ;tag
  %s64? <= S64MIN S64MAX ;to validate numbers
  8			 ;number of bytes in word
  %bytevector-s64l-set!) ;setter

(define-word-list-to-bytevector s64b-list->bytevector
  'vs64b		 ;tag
  %s64? <= S64MIN S64MAX ;to validate numbers
  8			 ;number of bytes in word
  %bytevector-s64b-set!) ;setter

(define-word-list-to-bytevector s64n-list->bytevector
  'vs64n		 ;tag
  %s64? <= S64MIN S64MAX ;to validate numbers
  8			 ;number of bytes in word
  %bytevector-s64n-set!) ;setter


;;;; copying

(define bytevector-copy
  (lambda (src)
    (unless (bytevector? src)
      (die 'bytevector-copy "not a bytevector" src))
    (let ((n ($bytevector-length src)))
      (let f ((src src) (dst ($make-bytevector n)) (i 0) (n n))
	(cond
	 (($fx= i n) dst)
	 (else
	  ($bytevector-set! dst i ($bytevector-u8-ref src i))
	  (f src dst ($fxadd1 i) n)))))))

(define bytevector-copy!
  (lambda (src src-start dst dst-start k)
    (cond
     ((or (not (fixnum? src-start)) ($fx< src-start 0))
      (die 'bytevector-copy! "not a valid starting index" src-start))
     ((or (not (fixnum? dst-start)) ($fx< dst-start 0))
      (die 'bytevector-copy! "not a valid starting index" dst-start))
     ((or (not (fixnum? k)) ($fx< k 0))
      (die 'bytevector-copy! "not a valid length" k))
     ((not (bytevector? src))
      (die 'bytevector-copy! "not a bytevector" src))
     ((not (bytevector? dst))
      (die 'bytevector-copy! "not a bytevector" dst))
     ((let ((n ($fx+ src-start k)))
	(or ($fx< n 0) ($fx> n ($bytevector-length src))))
      (die 'bytevector-copy! "out of range" src-start k))
     ((let ((n ($fx+ dst-start k)))
	(or ($fx< n 0) ($fx> n ($bytevector-length dst))))
      (die 'bytevector-copy! "out of range" dst-start k))
     ((eq? src dst)
      (cond
       (($fx< dst-start src-start)
	(let f ((src src) (si src-start) (di dst-start) (sj ($fx+ src-start k)))
	  (unless ($fx= si sj)
	    ($bytevector-set! src di ($bytevector-u8-ref src si))
	    (f src ($fxadd1 si) ($fxadd1 di) sj))))
       (($fx< src-start dst-start)
	(let f ((src src) (si ($fx+ src-start k)) (di ($fx+ dst-start k)) (sj src-start))
	  (unless ($fx= si sj)
	    (let ((si ($fxsub1 si)) (di ($fxsub1 di)))
	      ($bytevector-set! src di ($bytevector-u8-ref src si))
	      (f src si di sj)))))
       (else (void))))
     (else
      (let f ((src src) (si src-start) (dst dst) (di dst-start) (sj ($fx+ src-start k)))
	(unless ($fx= si sj)
	  ($bytevector-set! dst di ($bytevector-u8-ref src si))
	  (f src ($fxadd1 si) dst ($fxadd1 di) sj)))))))


;;;; platform's integer functions

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
	 ($bytevector-set! bv i0 (bitwise-and n 255))
	 (f bv ($fx+ i0 1) i1 (sra n 8)))))
    ((big)
     (let f ((bv bv) (i0 i0) (i1 (fx+ i0 size)) (n n))
       (unless ($fx= i0 i1)
	 (let ((i1 ($fx- i1 1)))
	   ($bytevector-set! bv i1 (bitwise-and n 255))
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
	 ($bytevector-set! bv i0 (bitwise-and n 255))
	 (f bv ($fx+ i0 1) i1 (sra n 8)))))
    ((big)
     (let f ((bv bv) (i0 i0) (i1 (fx+ i0 size)) (n n))
       (unless ($fx= i0 i1)
	 (let ((i1 ($fx- i1 1)))
	   ($bytevector-set! bv i1 (bitwise-and n 255))
	   (f bv i0 i1 (sra n 8))))))
    (else (die who "invalid endianness" endianness))))


(module (uint-list->bytevector sint-list->bytevector)
  (define (make-xint-list->bytevector who bv-set!)
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
    (make-xint-list->bytevector
     'uint-list->bytevector bytevector-uint-set!/who))
  (define sint-list->bytevector
    (make-xint-list->bytevector
     'sint-list->bytevector bytevector-sint-set!/who)))


;;;; floating point bytevector functions

(define (bytevector-ieee-double-native-ref bv i)
  (if (bytevector? bv)
      (if (and (fixnum? i)
	       ($fx>= i 0)
	       ($fxzero? ($fxlogand i 7))
	       ($fx< i ($bytevector-length bv)))
	  ($bytevector-ieee-double-native-ref bv i)
	(die 'bytevector-ieee-double-native-ref "invalid index" i))
    (die 'bytevector-ieee-double-native-ref "not a bytevector" bv)))

(define (bytevector-ieee-single-native-ref bv i)
  (if (bytevector? bv)
      (if (and (fixnum? i)
	       ($fx>= i 0)
	       ($fxzero? ($fxlogand i 3))
	       ($fx< i ($bytevector-length bv)))
	  ($bytevector-ieee-single-native-ref bv i)
	(die 'bytevector-ieee-single-native-ref "invalid index" i))
    (die 'bytevector-ieee-single-native-ref "not a bytevector" bv)))

(define (bytevector-ieee-double-native-set! bv i x)
  (if (bytevector? bv)
      (if (and (fixnum? i)
	       ($fx>= i 0)
	       ($fxzero? ($fxlogand i 7))
	       ($fx< i ($bytevector-length bv)))
	  (if (flonum? x)
	      ($bytevector-ieee-double-native-set! bv i x)
	    (die 'bytevector-ieee-double-native-set! "not a flonum" x))
	(die 'bytevector-ieee-double-native-set! "invalid index" i))
    (die 'bytevector-ieee-double-native-set! "not a bytevector" bv)))

(define (bytevector-ieee-single-native-set! bv i x)
  (if (bytevector? bv)
      (if (and (fixnum? i)
	       ($fx>= i 0)
	       ($fxzero? ($fxlogand i 3))
	       ($fx< i ($bytevector-length bv)))
	  (if (flonum? x)
	      ($bytevector-ieee-single-native-set! bv i x)
	    (die 'bytevector-ieee-single-native-set! "not a flonum" x))
	(die 'bytevector-ieee-single-native-set! "invalid index" i))
    (die 'bytevector-ieee-single-native-set! "not a bytevector" bv)))

(define ($bytevector-ieee-double-ref/big x i)
  (import (ikarus system $flonums))
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
  (import (ikarus system $flonums))
  ($bytevector-set! x i          ($flonum-u8-ref y 0))
  ($bytevector-set! x ($fx+ i 1) ($flonum-u8-ref y 1))
  ($bytevector-set! x ($fx+ i 2) ($flonum-u8-ref y 2))
  ($bytevector-set! x ($fx+ i 3) ($flonum-u8-ref y 3))
  ($bytevector-set! x ($fx+ i 4) ($flonum-u8-ref y 4))
  ($bytevector-set! x ($fx+ i 5) ($flonum-u8-ref y 5))
  ($bytevector-set! x ($fx+ i 6) ($flonum-u8-ref y 6))
  ($bytevector-set! x ($fx+ i 7) ($flonum-u8-ref y 7)))

(define ($bytevector-ieee-double-ref/little x i)
  (import (ikarus system $flonums))
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
  (import (ikarus system $flonums))
  ($bytevector-set! x i          ($flonum-u8-ref y 7))
  ($bytevector-set! x ($fx+ i 1) ($flonum-u8-ref y 6))
  ($bytevector-set! x ($fx+ i 2) ($flonum-u8-ref y 5))
  ($bytevector-set! x ($fx+ i 3) ($flonum-u8-ref y 4))
  ($bytevector-set! x ($fx+ i 4) ($flonum-u8-ref y 3))
  ($bytevector-set! x ($fx+ i 5) ($flonum-u8-ref y 2))
  ($bytevector-set! x ($fx+ i 6) ($flonum-u8-ref y 1))
  ($bytevector-set! x ($fx+ i 7) ($flonum-u8-ref y 0)))

(define ($bytevector-ieee-single-ref/little x i)
  (let ((bv (make-bytevector 4)))
    ($bytevector-set! bv 0 ($bytevector-u8-ref x i))
    ($bytevector-set! bv 1 ($bytevector-u8-ref x ($fx+ i 1)))
    ($bytevector-set! bv 2 ($bytevector-u8-ref x ($fx+ i 2)))
    ($bytevector-set! bv 3 ($bytevector-u8-ref x ($fx+ i 3)))
    ($bytevector-ieee-single-native-ref bv 0)))

(define ($bytevector-ieee-single-ref/big x i)
  (let ((bv (make-bytevector 4)))
    ($bytevector-set! bv 3 ($bytevector-u8-ref x i))
    ($bytevector-set! bv 2 ($bytevector-u8-ref x ($fx+ i 1)))
    ($bytevector-set! bv 1 ($bytevector-u8-ref x ($fx+ i 2)))
    ($bytevector-set! bv 0 ($bytevector-u8-ref x ($fx+ i 3)))
    ($bytevector-ieee-single-native-ref bv 0)))

(define ($bytevector-ieee-single-set!/little x i v)
  (let ((bv (make-bytevector 4)))
    ($bytevector-ieee-single-native-set! bv 0 v)
    ($bytevector-set! x i          ($bytevector-u8-ref bv 0))
    ($bytevector-set! x ($fx+ i 1) ($bytevector-u8-ref bv 1))
    ($bytevector-set! x ($fx+ i 2) ($bytevector-u8-ref bv 2))
    ($bytevector-set! x ($fx+ i 3) ($bytevector-u8-ref bv 3))))

(define ($bytevector-ieee-single-set!/big x i v)
  (let ((bv (make-bytevector 4)))
    ($bytevector-ieee-single-native-set! bv 0 v)
    ($bytevector-set! x i          ($bytevector-u8-ref bv 3))
    ($bytevector-set! x ($fx+ i 1) ($bytevector-u8-ref bv 2))
    ($bytevector-set! x ($fx+ i 2) ($bytevector-u8-ref bv 1))
    ($bytevector-set! x ($fx+ i 3) ($bytevector-u8-ref bv 0))))

(define (bytevector-ieee-double-ref bv i endianness)
  (define who 'bytevector-ieee-double-ref)
  (if (bytevector? bv)
      (if (and (fixnum? i) ($fx>= i 0))
	  (let ((len ($bytevector-length bv)))
	    (if (and ($fxzero? ($fxlogand i 7)) ($fx< i len))
		(case endianness
		  ((little) ($bytevector-ieee-double-native-ref bv i))
		  ((big) ($bytevector-ieee-double-nonnative-ref bv i))
		  (else (die who "invalid endianness" endianness)))
	      (if ($fx<= i ($fx- len 8))
		  (case endianness
		    ((little)
		     ($bytevector-ieee-double-ref/little bv i))
		    ((big)
		     ($bytevector-ieee-double-ref/big bv i))
		    (else (die who "invalid endianness" endianness)))
		(die who "invalid index" i))))
	(die who "invalid index" i))
    (die who "not a bytevector" bv)))

(define (bytevector-ieee-single-ref bv i endianness)
  (define who 'bytevector-ieee-single-ref)
  (if (bytevector? bv)
      (if (and (fixnum? i) ($fx>= i 0))
	  (let ((len ($bytevector-length bv)))
	    (if (and ($fxzero? ($fxlogand i 3)) ($fx< i len))
		(case endianness
		  ((little) ($bytevector-ieee-single-native-ref bv i))
		  ((big) ($bytevector-ieee-single-nonnative-ref bv i))
		  (else (die who "invalid endianness" endianness)))
	      (if ($fx<= i ($fx- len 4))
		  (case endianness
		    ((little)
		     ($bytevector-ieee-single-ref/little bv i))
		    ((big)
		     ($bytevector-ieee-single-ref/big bv i))
		    (else (die who "invalid endianness" endianness)))
		(die who "invalid index" i))))
	(die who "invalid index" i))
    (die who "not a bytevector" bv)))

(define (bytevector-ieee-double-set! bv i x endianness)
  (define who 'bytevector-ieee-double-set!)
  (if (bytevector? bv)
      (if (flonum? x)
	  (if (and (fixnum? i) ($fx>= i 0))
	      (let ((len ($bytevector-length bv)))
		(if (and ($fxzero? ($fxlogand i 7)) ($fx< i len))
		    (case endianness
		      ((little) ($bytevector-ieee-double-native-set! bv i x))
		      ((big) ($bytevector-ieee-double-nonnative-set! bv i x))
		      (else
		       (die who "invalid endianness" endianness)))
		  (if ($fx<= i ($fx- len 8))
		      (case endianness
			((little)
			 ($bytevector-ieee-double-set!/little bv i x))
			((big)
			 ($bytevector-ieee-double-set!/big bv i x))
			(else
			 (die who "invalid endianness" endianness)))
		    (die who "invalid index" i))))
	    (die who "invalid index" i))
	(die who "not a flonum" x))
    (die who "not a bytevector" bv)))

(define (bytevector-ieee-single-set! bv i x endianness)
  (define who 'bytevector-ieee-single-set!)
  (if (bytevector? bv)
      (if (flonum? x)
	  (if (and (fixnum? i) ($fx>= i 0))
	      (let ((len ($bytevector-length bv)))
		(if (and ($fxzero? ($fxlogand i 3)) ($fx< i len))
		    (case endianness
		      ((little) ($bytevector-ieee-single-native-set! bv i x))
		      ((big) ($bytevector-ieee-single-nonnative-set! bv i x))
		      (else
		       (die who "invalid endianness" endianness)))
		  (if ($fx<= i ($fx- len 4))
		      (case endianness
			((little)
			 ($bytevector-ieee-single-set!/little bv i x))
			((big)
			 ($bytevector-ieee-single-set!/big bv i x))
			(else
			 (die who "invalid endianness" endianness)))
		    (die who "invalid index" i))))
	    (die who "invalid index" i))
	(die who "not a flonum" x))
    (die who "not a bytevector" bv)))


;;;; 64-bit bytevector functions

(define ($bytevector-ref/64/aligned bv i who decoder endianness)
  (if (bytevector? bv)
      (if (and (fixnum? i)
	       ($fx>= i 0)
	       ($fxzero? ($fxlogand i 7))
	       ($fx< i ($bytevector-length bv)))
	  (case endianness
	    ((little big)
	     (decoder bv i endianness 8))
	    (else (die who "invalid endianness" endianness)))
	(die who "invalid index" i))
    (die who "not a bytevector" bv)))

(define ($bytevector-ref/64 bv i who decoder endianness)
  (if (bytevector? bv)
      (if (and (fixnum? i)
	       ($fx>= i 0)
	       ($fx< i ($fx- ($bytevector-length bv) 7)))
	  (case endianness
	    ((little big)
	     (decoder bv i endianness 8))
	    (else (die who "invalid endianness" endianness)))
	(die who "invalid index" i))
    (die who "not a bytevector" bv)))

(define (bytevector-u64-native-ref bv i)
  ($bytevector-ref/64/aligned bv i 'bytevector-u64-native-ref
			      bytevector-uint-ref 'little))
(define (bytevector-s64-native-ref bv i)
  ($bytevector-ref/64/aligned bv i 'bytevector-s64-native-ref
			      bytevector-sint-ref 'little))
(define (bytevector-u64-ref bv i endianness)
  ($bytevector-ref/64 bv i 'bytevector-u64-ref
		      bytevector-uint-ref endianness))
(define (bytevector-s64-ref bv i endianness)
  ($bytevector-ref/64 bv i 'bytevector-s64-ref
		      bytevector-sint-ref endianness))

(define ($bytevector-set/64/align bv i n lo hi who setter endianness)
  (if (bytevector? bv)
      (if (and (fixnum? i)
	       ($fx>= i 0)
	       ($fxzero? ($fxlogand i 7))
	       ($fx< i ($bytevector-length bv)))
	  (case endianness
	    ((little big)
	     (unless (or (fixnum? n) (bignum? n))
	       (die who "number is not an exact number" n))
	     (unless (and (<= lo n) (< n hi))
	       (die who "number out of range" n))
	     (setter bv i n endianness 8))
	    (else (die who "invalid endianness" endianness)))
	(die who "invalid index" i))
    (die who "not a bytevector" bv)))

(define ($bytevector-set/64 bv i n lo hi who setter endianness)
  (if (bytevector? bv)
      (if (and (fixnum? i)
	       ($fx>= i 0)
	       ($fx< i ($fx- ($bytevector-length bv) 7)))
	  (case endianness
	    ((little big)
	     (unless (or (fixnum? n) (bignum? n))
	       (die who "number is not exact number" n))
	     (unless (and (<= lo n) (< n hi))
	       (die who "number out of range" n))
	     (setter bv i n endianness 8))
	    (else (die who "invalid endianness" endianness)))
	(die who "invalid index" i))
    (die who "not a bytevector" bv)))


(define (bytevector-u64-native-set! bv i n)
  ($bytevector-set/64/align bv i n 0 (expt 2 64)
			    'bytevector-u64-native-set! bytevector-uint-set! 'little))
(define (bytevector-s64-native-set! bv i n)
  ($bytevector-set/64/align bv i n (- (expt 2 63)) (expt 2 63)
			    'bytevector-s64-native-set! bytevector-sint-set! 'little))
(define (bytevector-u64-set! bv i n endianness)
  ($bytevector-set/64 bv i n 0 (expt 2 64)
		      'bytevector-u64-set! bytevector-uint-set! endianness))
(define (bytevector-s64-set! bv i n endianness)
  ($bytevector-set/64 bv i n (- (expt 2 63)) (expt 2 63)
		      'bytevector-s64-set! bytevector-sint-set! endianness))


(define (bytevector-append . list-of-bytevectors)
  (define who 'bytevector-append)

  (define-inline (%length list-of-bytevectors)
    (%%length list-of-bytevectors 0))
  (define (%%length list-of-bytevectors accumulated-length)
    ;;Validate LIST-OF-BYTEVECTORS  as a list of  bytevectors and return
    ;;the  total length  of the  bytevectors; if  LIST-OF-BYTEVECTORS is
    ;;null return N.
    ;;
    (if (null? list-of-bytevectors)
	accumulated-length
      (let ((bv ($car list-of-bytevectors)))
	(unless (bytevector? bv)
	  (assertion-violation who "expected list of bytevectors as argument" bv))
	(%%length ($cdr list-of-bytevectors) (+ accumulated-length ($bytevector-length bv))))))

  (define (fill-bytevector dst.bv src.bv dst.index dst.past src.index)
    (unless ($fx= dst.index dst.past)
      ($bytevector-set! dst.bv dst.index ($bytevector-u8-ref src.bv src.index))
      (fill-bytevector dst.bv src.bv ($fxadd1 dst.index) dst.past ($fxadd1 src.index))))

  (define (fill-bytevectors dst.bv list-of-bytevectors dst.start)
    (if (null? list-of-bytevectors)
	dst.bv
      (let ((src.bv ($car list-of-bytevectors)))
	(let ((src.len ($bytevector-length src.bv)))
	  (let ((dst.past ($fx+ dst.start src.len)))
	    (fill-bytevector dst.bv src.bv dst.start dst.past 0)
	    (fill-bytevectors dst.bv ($cdr list-of-bytevectors) dst.past))))))

  (let ((accumulated-length (%length list-of-bytevectors)))
    (unless (fixnum? accumulated-length)
      (%implementation-violation who
	"appending bytevectors would produce a bytevector who length exceeds the supported maximum"
	accumulated-length))
    (fill-bytevectors ($make-bytevector accumulated-length) list-of-bytevectors 0)))



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
