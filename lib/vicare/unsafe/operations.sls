;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: unsafe operations
;;;Date: Sun Oct 23, 2011
;;;
;;;Abstract
;;;
;;;	This library is both  installed and used when expanding Vicare's
;;;	own source code.  For this  reason it must export only: bindings
;;;	imported  by Vicare itself,  syntaxes whose  expansion reference
;;;	only bindings imported by Vicare itself.
;;;
;;;	  In general: all the syntaxes must be used with arguments which
;;;	can be evaluated  multiple times, in practice it  is safe to use
;;;	the syntaxes  only with arguments being  identifiers or constant
;;;	values.
;;;
;;;Endianness handling
;;;
;;;	About endianness, according to R6RS:
;;;
;;;	   Endianness describes the encoding of exact integer objects as
;;;	   several contiguous bytes in a bytevector.
;;;
;;;        The little-endian encoding  places the least significant byte
;;;        of  an  integer first,  with  the  other  bytes following  in
;;;        increasing order of significance.
;;;
;;;        The big-endian  encoding places the most  significant byte of
;;;        an  integer   first,  with  the  other   bytes  following  in
;;;        decreasing order of significance.
;;;
;;;
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare unsafe operations)
  (export
    (rename ($make-struct	make-struct)
	    ($struct		struct)
	    ($struct-rtd	struct-rtd)
	    ($struct/rtd?	struct/rtd?)
	    ($struct-length	struct-length)
	    ($struct-ref	struct-ref)
	    ($struct-set!	struct-set!))

    (rename ($caar		caar)
	    ($cadr		cadr)
	    ($cdar		cdar)
	    ($cddr		cddr)

	    ($caaar		caaar)
	    ($caadr		caadr)
	    ($cadar		cadar)
	    ($cdaar		cdaar)
	    ($cdadr		cdadr)
	    ($cddar		cddar)
	    ($cdddr		cdddr)
	    ($caddr		caddr))

    (rename ($fxzero?		fxzero?)
	    ($fxnegative?	fxnegative?)
	    ($fxpositive?	fxpositive?)
	    ($fxadd1		fxadd1)		;increment
	    ($fxsub1		fxsub1)		;decrement
	    ($fxneg		fxneg)		;negation
	    ($fxsra		fxsra)		;shift right
	    ($fxsll		fxsll)		;shift left
	    ($fxlogor		fxlogor)	;inclusive logic OR
	    ($fxlogxor		fxlogxor)	;exlusive logic OR
	    ($fxlogand		fxlogand)	;logic AND
	    ($fxlognot		fxlognot)	;logic not
	    ($fxlognot		fxnot)		;logic not
	    ($fx+		fx+)
	    ($fx-		fx-)
	    ($fx*		fx*)
	    ($fxdiv		fxdiv)
	    ($fxdiv0		fxdiv0)
	    ($fxmod		fxmod)
	    ($fxmod0		fxmod0)
	    ($fx<		fx<)
	    ($fx>		fx>)
	    ($fx>=		fx>=)
	    ($fx<=		fx<=)
	    ($fx=		fx=))

    (rename ($fxand		fxand)		;multiple arguments AND
	    ($fxior		fxior)		;multiple arguments inclusive OR
	    ($fxxor		fxxor)		;multiple arguments exclusive OR
	    ($fxmax		fxmax)		;multiple arguments max
	    ($fxadd2		fxadd2)
	    ($fxadd3		fxadd3)
	    ($fxadd4		fxadd4)
	    ($fxincr!		fxincr!))

;;; --------------------------------------------------------------------

    (rename ($bignum-positive?		bignum-positive?)
	    ($bignum-negative?		bignum-negative?)
	    ($bignum-byte-ref		bignum-byte-ref)
	    ($bignum-size		bignum-size))

    (rename ($bnbn=			bnbn=)
	    ($bnbn<			bnbn<)
	    ($bnbn>			bnbn>)
	    ($bnbn<=			bnbn<=)
	    ($bnbn>=			bnbn>=))

;;; --------------------------------------------------------------------

    (rename ($make-ratnum		make-ratnum)
	    ($ratnum-n			ratnum-n)
	    ($ratnum-d			ratnum-d))

;;; --------------------------------------------------------------------

    (rename ($make-flonum		make-flonum)
	    ($flonum-u8-ref		flonum-u8-ref)
	    ($flonum-set!		flonum-set!)
	    ($fixnum->flonum		fixnum->flonum)
	    ($fl+			fl+)
	    ($fl-			fl-)
	    ($fl*			fl*)
	    ($fl/			fl/)
	    ($fl=			fl=)
	    ($fl<			fl<)
	    ($fl>			fl>)
	    ($fl<=			fl<=)
	    ($fl>=			fl>=)
	    ($flonum-sbe		flonum-sbe))

;;; --------------------------------------------------------------------

    (rename ($make-cflonum		make-cflonum)
	    ($cflonum-real		cflonum-real)
	    ($cflonum-imag		cflonum-imag)
	    ($make-compnum		make-compnum)
	    ($compnum-real		compnum-real)
	    ($compnum-imag		compnum-imag))

;;; --------------------------------------------------------------------

    (rename ($make-bytevector		make-bytevector)
	    ($bytevector-length		bytevector-length)
	    ($bytevector-u8-ref		bytevector-u8-ref)
	    ($bytevector-s8-ref		bytevector-s8-ref)
	    ($bytevector-u8-set!	bytevector-u8-set!)
	    ($bytevector-s8-set!	bytevector-s8-set!)
	    ($bytevector-ieee-double-native-ref		bytevector-ieee-double-native-ref)
	    ($bytevector-ieee-double-native-set!	bytevector-ieee-double-native-set!)
	    ($bytevector-ieee-single-native-ref		bytevector-ieee-single-native-ref)
	    ($bytevector-ieee-single-native-set!	bytevector-ieee-single-native-set!)
	    ($bytevector-ieee-single-nonnative-ref	bytevector-ieee-single-nonnative-ref)
	    ($bytevector-ieee-single-nonnative-set!	bytevector-ieee-single-nonnative-set!)
	    ($bytevector-ieee-double-nonnative-set!	bytevector-ieee-double-nonnative-set!)
	    ($bytevector-ieee-double-nonnative-ref	bytevector-ieee-double-nonnative-ref))

    (rename ($bytevector-u16b-ref	bytevector-u16b-ref)
	    ($bytevector-u16b-set!	bytevector-u16b-set!)
	    ($bytevector-u16l-ref	bytevector-u16l-ref)
	    ($bytevector-u16l-set!	bytevector-u16l-set!)
	    ($bytevector-s16b-ref	bytevector-s16b-ref)
	    ($bytevector-s16b-set!	bytevector-s16b-set!)
	    ($bytevector-s16l-ref	bytevector-s16l-ref)
	    ($bytevector-s16l-set!	bytevector-s16l-set!)
	    ($bytevector-u16n-ref	bytevector-u16n-ref)
	    ($bytevector-u16n-set!	bytevector-u16n-set!)
	    ($bytevector-s16n-ref	bytevector-s16n-ref)
	    ($bytevector-s16n-set!	bytevector-s16n-set!)

	    ($bytevector-u16-ref	bytevector-u16-ref)
	    ($bytevector-u16-set!	bytevector-u16-set!)

	    ($bytevector-u32b-ref	bytevector-u32b-ref)
	    ($bytevector-u32b-set!	bytevector-u32b-set!)
	    ($bytevector-u32l-ref	bytevector-u32l-ref)
	    ($bytevector-u32l-set!	bytevector-u32l-set!)
	    ($bytevector-s32b-ref	bytevector-s32b-ref)
	    ($bytevector-s32b-set!	bytevector-s32b-set!)
	    ($bytevector-s32l-ref	bytevector-s32l-ref)
	    ($bytevector-s32l-set!	bytevector-s32l-set!)
	    ($bytevector-u32n-ref	bytevector-u32n-ref)
	    ($bytevector-u32n-set!	bytevector-u32n-set!)
	    ($bytevector-s32n-ref	bytevector-s32n-ref)
	    ($bytevector-s32n-set!	bytevector-s32n-set!)

	    ($bytevector-u64b-ref	bytevector-u64b-ref)
	    ($bytevector-u64b-set!	bytevector-u64b-set!)
	    ($bytevector-u64l-ref	bytevector-u64l-ref)
	    ($bytevector-u64l-set!	bytevector-u64l-set!)
	    ($bytevector-s64b-ref	bytevector-s64b-ref)
	    ($bytevector-s64b-set!	bytevector-s64b-set!)
	    ($bytevector-s64l-ref	bytevector-s64l-ref)
	    ($bytevector-s64l-set!	bytevector-s64l-set!)
	    ($bytevector-u64n-ref	bytevector-u64n-ref)
	    ($bytevector-u64n-set!	bytevector-u64n-set!)
	    ($bytevector-s64n-ref	bytevector-s64n-ref)
	    ($bytevector-s64n-set!	bytevector-s64n-set!)

	    ($bytevector-fill!			bytevector-fill!)
	    ($bytevector-copy!			bytevector-copy!)
	    ($bytevector-copy!/count		bytevector-copy!/count)
	    ($bytevector-self-copy-forwards!	bytevector-self-copy-forwards!)
	    ($bytevector-self-copy-backwards!	bytevector-self-copy-backwards!))

;;; --------------------------------------------------------------------

    (rename ($car		car)
	    ($cdr		cdr)
	    ($set-car!		set-car!)
	    ($set-cdr!		set-cdr!))

;;; --------------------------------------------------------------------

    (rename ($make-vector	make-vector)
	    ($vector-length	vector-length)
	    ($vector-ref	vector-ref)
	    ($vector-set!	vector-set!))

    (rename ($vector-copy!			vector-copy!)
	    ($vector-self-copy-forwards!	vector-self-copy-forwards!)
	    ($vector-self-copy-backwards!	vector-self-copy-backwards!)
	    ($vector-fill!			vector-fill!)
	    ($subvector				subvector)
	    ($vector-clean!			vector-clean!)
	    ($make-clean-vector			make-clean-vector))

;;; --------------------------------------------------------------------

    (rename ($char=		char=)
	    ($char<		char<)
	    ($char>		char>)
	    ($char>=		char>=)
	    ($char<=		char<=)
	    ($char->fixnum	char->fixnum)
	    ($fixnum->char	fixnum->char))

    (rename ($char-is-single-char-line-ending?		char-is-single-char-line-ending?)
	    ($char-is-carriage-return?			char-is-carriage-return?)
	    ($char-is-newline-after-carriage-return?	char-is-newline-after-carriage-return?))

;;; --------------------------------------------------------------------

    (rename ($make-string	make-string)
	    ($string-length	string-length)
	    ($string-ref	string-ref)
	    ($string-set!	string-set!)
	    ($string=		string=))

    (rename ($string-copy!			string-copy!)
	    ($string-copy!/count		string-copy!/count)
	    ($string-self-copy-forwards!	string-self-copy-forwards!)
	    ($string-self-copy-backwards!	string-self-copy-backwards!)
	    ($string-fill!			string-fill!)
	    ($substring				substring))

;;; --------------------------------------------------------------------

    (rename ($pointer?				pointer?)
	    ($pointer=				pointer=))

;;; --------------------------------------------------------------------

    (rename ($memory-block-pointer		memory-block-pointer)
	    ($memory-block-size			memory-block-size))

;;; --------------------------------------------------------------------

    (rename ($closure-code			closure-code)
	    ($code->closure			code->closure)
	    ($code-reloc-vector			code-reloc-vector)
	    ($code-freevars			code-freevars)
	    ($code-size				code-size)
	    ($code-annotation			code-annotation)
	    ($code-ref				code-ref)
	    ($code-set!				code-set!)
	    ($set-code-annotation!		set-code-annotation!))

    #| end of export |# )
  (import (ikarus)
    (ikarus system $structs)
    (except (ikarus system $fx)
	    $fxmax
	    $fxmin)
    (ikarus system $bignums)
    (ikarus system $ratnums)
    (ikarus system $flonums)
    (ikarus system $compnums)
    (ikarus system $pairs)
    (ikarus system $vectors)
    (rename (ikarus system $bytevectors)
	    ($bytevector-set!	$bytevector-set!)
	    ($bytevector-set!	$bytevector-u8-set!)
	    ($bytevector-set!	$bytevector-s8-set!))
    (ikarus system $chars)
    (ikarus system $strings)
    (ikarus system $codes)
    (ikarus system $pointers)
    (for (prefix (only (vicare platform configuration)
		       platform-endianness)
		 config.)
	 expand))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


;;;; structures

(define-syntax $struct-length
  (syntax-rules ()
    ((_ ?stru)
     ($struct-ref ($struct-rtd ?stru) 1))))

;;; --------------------------------------------------------------------

(define-syntax $memory-block-pointer
  (syntax-rules ()
    ((_ ?stru)
     ($struct-ref ?stru 0))))

(define-syntax $memory-block-size
  (syntax-rules ()
    ((_ ?stru)
     ($struct-ref ?stru 1))))


;;;; pairs

(define-inline ($caar x)	($car ($car x)))
(define-inline ($cadr x)	($car ($cdr x)))
(define-inline ($cdar x)	($cdr ($car x)))
(define-inline ($cddr x)	($cdr ($cdr x)))

(define-inline ($caaar x)	($car ($car ($car x))))
(define-inline ($caadr x)	($car ($car ($cdr x))))
(define-inline ($cadar x)	($car ($cdr ($car x))))
(define-inline ($cdaar x)	($cdr ($car ($car x))))
(define-inline ($cdadr x)	($cdr ($car ($cdr x))))
(define-inline ($cddar x)	($cdr ($cdr ($car x))))
(define-inline ($cdddr x)	($cdr ($cdr ($cdr x))))
(define-inline ($caddr x)	($car ($cdr ($cdr x))))


;;;; fixnums

;;; arithmetic operations

(define-syntax $fxneg
  (syntax-rules ()
    ((_ ?op)
     ($fx- 0 ?op))))

(define-syntax $fxadd2
  (syntax-rules ()
    ((_ ?op)
     ($fx+ ?op 2))))

(define-syntax $fxadd3
  (syntax-rules ()
    ((_ ?op)
     ($fx+ ?op 3))))

(define-syntax $fxadd4
  (syntax-rules ()
    ((_ ?op)
     ($fx+ ?op 4))))

(define-syntax $fxincr!
  (syntax-rules ()
    ((_ ?op 0)
     ?op)
    ((_ ?op 1)
     (set! ?op ($fxadd1 ?op)))
    ((_ ?op 2)
     (set! ?op ($fxadd2 ?op)))
    ((_ ?op 3)
     (set! ?op ($fxadd3 ?op)))
    ((_ ?op ?N)
     (set! ?op ($fx+ ?op ?N)))
    ))

(define-syntax $fxmax
  (syntax-rules ()
    ((_ ?op)
     ?op)
    ((_ ?op1 ?op2)
     (let ((a ?op1)
	   (b ?op2))
       (if ($fx>= a b) a b)))
    ((_ ?op1 ?op2 . ?ops)
     (let ((X ($fxmax ?op1 ?op2)))
       ($fxmax X . ?ops)))
    ))

(define-syntax $fxmin
  (syntax-rules ()
    ((_ ?op)
     ?op)
    ((_ ?op1 ?op2)
     (let ((a ?op1)
	   (b ?op2))
       (if ($fx<= a b) a b)))
    ((_ ?op1 ?op2 . ?ops)
     (let ((X ($fxmin ?op1 ?op2)))
       ($fxmin X . ?ops)))
    ))

;;; --------------------------------------------------------------------
;;; logic operations

(define-syntax $fxand
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     ($fxlogand ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     ($fxlogand ?op1 ($fxand ?op2 . ?ops)))))

(define-syntax $fxior
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     ($fxlogor ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     ($fxlogor ?op1 ($fxior ?op2 . ?ops)))))

(define-syntax $fxxor
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     ($fxlogxor ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     ($fxlogxor ?op1 ($fxxor ?op2 . ?ops)))))


;;;; bignums

(define-inline (%bnbncmp X Y fxcmp)
  (fxcmp (foreign-call "ikrt_bnbncomp" X Y) 0))

(define-inline ($bnbn= X Y)
  (%bnbncmp X Y $fx=))

(define-inline ($bnbn< X Y)
  (%bnbncmp X Y $fx<))

(define-inline ($bnbn> X Y)
  (%bnbncmp X Y $fx>))

(define-inline ($bnbn<= X Y)
  (%bnbncmp X Y $fx<=))

(define-inline ($bnbn>= X Y)
  (%bnbncmp X Y $fx>=))


;;;; heterogeneous and high-level operations

(define-inline (fx+fx X Y)
  (foreign-call "ikrt_fxfxplus" X Y))

(define-inline (fx+bn X Y)
  (foreign-call "ikrt_fxbnplus" X Y))

(define-inline (bn+bn X Y)
  (foreign-call "ikrt_bnbnplus" X Y))

;;; --------------------------------------------------------------------

(define-inline (fx-fx X Y)
  (foreign-call "ikrt_fxfxminus" X Y))

(define-inline (fx-bn X Y)
  (foreign-call "ikrt_fxbnminus" X Y))

(define-inline (bn-bn X Y)
  (foreign-call "ikrt_bnbnminus" X Y))

;;; --------------------------------------------------------------------

(define-inline (fx-and-bn X Y)
  (foreign-call "ikrt_fxbnlogand" X Y))

(define-inline (bn-and-bn X Y)
  (foreign-call "ikrt_bnbnlogand" X Y))

(define-inline (fx-ior-bn X Y)
  (foreign-call "ikrt_fxbnlogor" X Y))

(define-inline (bn-ior-bn X Y)
  (foreign-call "ikrt_bnbnlogor" X Y))


;;;; unsafe 16-bit setters and getters
;;
;;            |            | lowest memory | highest memory
;; endianness |    word    | location      | location
;; -----------+------------+---------------+--------------
;;   little   |   #xHHLL   |     LL        |     HH
;;    big     |   #xHHLL   |     HH        |      LL
;;
;;
;;NOTE  Remember that  $BYTEVECTOR-SET! takes  care of  storing in
;;memory only the least significant byte of its value argument.
;;

(define-inline ($bytevector-u16l-ref bv index)
  ($fxlogor
   ;; highest memory location -> most significant byte
   ($fxsll ($bytevector-u8-ref bv ($fxadd1 index)) 8)
   ;; lowest memory location -> least significant byte
   ($bytevector-u8-ref bv index)))

(define-inline ($bytevector-u16l-set! bv index word)
  ;; lowest memory location -> least significant byte
  ($bytevector-set! bv index word)
  ;; highest memory location -> most significant byte
  ($bytevector-set! bv ($fxadd1 index) (fxsra word 8)))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-u16b-ref bv index)
  ($fxlogor
   ;; lowest memory location -> most significant byte
   ($fxsll ($bytevector-u8-ref bv index) 8)
   ;; highest memory location -> least significant byte
   ($bytevector-u8-ref bv ($fxadd1 index))))

(define-inline ($bytevector-u16b-set! bv index word)
  ;; lowest memory location -> most significant byte
  ($bytevector-set! bv index ($fxsra word 8))
  ;; highest memory location -> least significant byte
  ($bytevector-set! bv ($fxadd1 index) word))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-s16l-ref bv index)
  ($fxlogor
   ;; highest memory location -> most significant byte
   ($fxsll ($bytevector-s8-ref bv ($fxadd1 index)) 8)
   ;; lowest memory location -> least significant byte
   ($bytevector-u8-ref bv index)))

(define-inline ($bytevector-s16l-set! bv index word)
  ;; lowest memory location -> least significant byte
  ($bytevector-set! bv index word)
  ;; highest memory location -> most significant byte
  ($bytevector-set! bv ($fxadd1 index) (fxsra word 8)))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-s16b-ref bv index)
  ($fxlogor
   ;; lowest memory location -> most significant byte
   ($fxsll ($bytevector-s8-ref bv index) 8)
   ;; highest memory location -> least significant byte
   ($bytevector-u8-ref bv ($fxadd1 index))))

(define-inline ($bytevector-s16b-set! bv index word)
  ;; lowest memory location -> most significant byte
  ($bytevector-set! bv index ($fxsra word 8))
  ;; highest memory location -> least significant byte
  ($bytevector-set! bv ($fxadd1 index) word))

;;; --------------------------------------------------------------------

(define-syntax $bytevector-u16n-ref
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-u16b-ref))
    ((little)
     (identifier-syntax $bytevector-u16l-ref))))

(define-syntax $bytevector-u16n-set!
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-u16b-set!))
    ((little)
     (identifier-syntax $bytevector-u16l-set!))))

;;; --------------------------------------------------------------------

(define-syntax $bytevector-s16n-ref
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-s16b-ref))
    ((little)
     (identifier-syntax $bytevector-s16l-ref))))

(define-syntax $bytevector-s16n-set!
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-s16b-set!))
    ((little)
     (identifier-syntax $bytevector-s16l-set!))))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-u16-ref bv index endianness)
  ;;Like BYTEVECTOR-U16-REF  defined by R6RS.  Assume  all the arguments
  ;;to  have been  already validated;  expect the  index integers  to be
  ;;fixnums.
  ;;
  (if (eq? endianness 'big)
      ($bytevector-u16b-ref bv index)
    ($bytevector-u16l-ref bv index)))

(define-inline ($bytevector-u16-set! bv index word endianness)
  ;;Like BYTEVECTOR-U16-SET!  defined by R6RS.  Assume all the arguments
  ;;to  have been  already validated;  expect the  index integers  to be
  ;;fixnums.
  ;;
  (if (eq? endianness 'big)
      ($bytevector-u16b-set! bv index word)
    ($bytevector-u16l-set! bv index word)))


;;;; unsafe 32-bit setters and getters
;;
;;                           lowest memory ------------> highest memory
;; endianness |    word    | 1st byte | 2nd byte | 3rd byte | 4th byte |
;; -----------+------------+----------+----------+----------+-----------
;;   little   | #xAABBCCDD |   DD     |    CC    |    BB    |    AA
;;    big     | #xAABBCCDD |   AA     |    BB    |    CC    |    DD
;; bit offset |            |    0     |     8    |    16    |    24
;;
;;NOTE  Remember that  $BYTEVECTOR-SET! takes  care of  storing in
;;memory only the least significant byte of its value argument.
;;

(define-inline ($bytevector-u32b-ref bv index)
  (+ (sll ($bytevector-u8-ref bv index) 24)
     ($fxior
      ($fxsll ($bytevector-u8-ref bv ($fxadd1 index)) 16)
      ($fxsll ($bytevector-u8-ref bv ($fx+ index 2))  8)
      ($bytevector-u8-ref bv ($fx+ index 3)))))

(define-inline ($bytevector-u32b-set! bv index word)
  (let ((b (sra word 16)))
    ($bytevector-set! bv index ($fxsra b 8))
    ($bytevector-set! bv ($fxadd1 index) b))
  (let ((b (bitwise-and word #xFFFF)))
    ($bytevector-set! bv ($fx+ index 2) ($fxsra b 8))
    ($bytevector-set! bv ($fx+ index 3) b)))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-u32l-ref bv index)
  (+ (sll ($bytevector-u8-ref bv ($fx+ index 3)) 24)
     ($fxior
      ($fxsll ($bytevector-u8-ref bv ($fx+ index 2)) 16)
      ($fxsll ($bytevector-u8-ref bv ($fxadd1 index)) 8)
      ($bytevector-u8-ref bv index))))

(define-inline ($bytevector-u32l-set! bv index word)
  (let ((b (sra word 16)))
    ($bytevector-set! bv ($fx+ index 3) ($fxsra b 8))
    ($bytevector-set! bv ($fx+ index 2) b))
  (let ((b (bitwise-and word #xFFFF)))
    ($bytevector-set! bv ($fxadd1 index) ($fxsra b 8))
    ($bytevector-set! bv index b)))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-s32b-ref bv index)
  (+ (sll ($bytevector-s8-ref bv index) 24)
     ($fxior
      ($fxsll ($bytevector-u8-ref bv ($fxadd1 index))   16)
      ($fxsll ($bytevector-u8-ref bv ($fx+    index 2))  8)
      ($bytevector-u8-ref bv ($fx+ index 3)))))

(define-inline ($bytevector-s32b-set! bv index word)
  (let ((b (sra word 16)))
    ($bytevector-set! bv index ($fxsra b 8))
    ($bytevector-set! bv ($fxadd1 index) b))
  (let ((b (bitwise-and word #xFFFF)))
    ($bytevector-set! bv ($fx+ index 2) ($fxsra b 8))
    ($bytevector-set! bv ($fx+ index 3) b)))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-s32l-ref bv index)
  (+ (sll ($bytevector-s8-ref bv ($fx+ index 3)) 24)
     ($fxior
      ($fxsll ($bytevector-u8-ref bv ($fx+    index 2)) 16)
      ($fxsll ($bytevector-u8-ref bv ($fxadd1 index))    8)
      ($bytevector-u8-ref bv index))))

(define-inline ($bytevector-s32l-set! bv index word)
  (let ((b (sra word 16)))
    ($bytevector-set! bv ($fx+ index 3) ($fxsra b 8))
    ($bytevector-set! bv ($fx+ index 2) b))
  (let ((b (bitwise-and word #xFFFF)))
    ($bytevector-set! bv ($fxadd1 index) ($fxsra b 8))
    ($bytevector-set! bv index b)))

;;; --------------------------------------------------------------------

(define-syntax $bytevector-u32n-ref
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-u32b-ref))
    ((little)
     (identifier-syntax $bytevector-u32l-ref))))

(define-syntax $bytevector-u32n-set!
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-u32b-set!))
    ((little)
     (identifier-syntax $bytevector-u32l-set!))))

;;; --------------------------------------------------------------------

(define-syntax $bytevector-s32n-ref
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-s32b-ref))
    ((little)
     (identifier-syntax $bytevector-s32l-ref))))

(define-syntax $bytevector-s32n-set!
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-s32b-set!))
    ((little)
     (identifier-syntax $bytevector-s32l-set!))))


;;;; unsafe 64-bit setters and getters
;;
;;                                      lowest memory ------------> highest memory
;; endianness |         word        | 1st | 2nd | 3rd | 4th | 5th | 6th | 7th | 8th |
;; -----------+---------------------+-----+-----+-----+-----+-----+-----+-----+-----|
;;   little   | #xAABBCCDD EEFFGGHH | HH  | GG  | FF  | EE  | DD  | CC  | BB  | AA
;;    big     | #xAABBCCDD EEFFGGHH | AA  | BB  | CC  | DD  | EE  | FF  | GG  | HH
;; bit offset |                     |  0  |  8  | 16  | 24  | 32  | 40  | 48  | 56
;;
;;NOTE  Remember that  $BYTEVECTOR-SET! takes  care of  storing in
;;memory only the least significant byte of its value argument.
;;

(define-inline ($bytevector-u64b-ref ?bv ?index)
  (let ((index ?index))
    (let next-byte ((bv     ?bv)
		    (index  index)
		    (end    ($fx+ index 7))
		    (word   0))
      (let ((word (+ word ($bytevector-u8-ref bv index))))
	(if ($fx= index end)
	    word
	  (next-byte bv ($fxadd1 index) end (sll word 8)))))))

(define-inline ($bytevector-u64b-set! ?bv ?index ?word)
  (let ((index  ?index)
	(word	?word))
    (let next-byte ((bv     ?bv)
		    (index  ($fx+ 7 index))
		    (end    index)
		    (word   word))
      ($bytevector-u8-set! bv index (bitwise-and word #xFF))
      (unless ($fx= index end)
	(next-byte bv ($fxsub1 index) end (sra word 8))))))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-u64l-ref ?bv ?end)
  (let ((end ?end))
    (let next-byte ((bv     ?bv)
		    (index  ($fx+ 7 end))
		    (word   0))
      (let ((word (+ word ($bytevector-u8-ref bv index))))
	(if ($fx= index end)
	    word
	  (next-byte bv ($fxsub1 index) (sll word 8)))))))

(define-inline ($bytevector-u64l-set! ?bv ?index ?word)
  (let ((index	?index)
	(word	?word))
    (let next-byte ((bv     ?bv)
		    (index  index)
		    (end    ($fx+ 7 index))
		    (word   word))
      ($bytevector-u8-set! bv index (bitwise-and word #xFF))
      (unless ($fx= index end)
	(next-byte bv ($fxadd1 index) end (sra word 8))))))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-s64b-ref ?bv ?index)
  (let ((bv	?bv)
	(index	?index))
    (let next-byte ((bv     bv)
		    (index  ($fxadd1 index))
		    (end    ($fx+ index 7))
		    (word   (sll ($bytevector-s8-ref bv index) 8)))
      (let ((word (+ word ($bytevector-u8-ref bv index))))
	(if ($fx= index end)
	    word
	  (next-byte bv ($fxadd1 index) end (sll word 8)))))))

(define-inline ($bytevector-s64b-set! ?bv ?index ?word)
  (let ((index	?index)
	(word	?word))
    (let next-byte ((bv     ?bv)
		    (index  ($fx+ 7 index))
		    (end    index)
		    (word   word))
      (if ($fx= index end)
	  ($bytevector-s8-set! bv index (bitwise-and word #xFF))
	(begin
	  ($bytevector-u8-set! bv index (bitwise-and word #xFF))
	  (next-byte bv ($fxsub1 index) end (sra word 8)))))))

;;; --------------------------------------------------------------------

(define-inline ($bytevector-s64l-ref ?bv ?end)
  (let ((bv	?bv)
	(end	?end))
    (let next-byte ((bv     bv)
		    (index  ($fx+ 6 end))
		    (word   (sll ($bytevector-s8-ref bv ($fx+ 7 end)) 8)))
      (let ((word (+ word ($bytevector-u8-ref bv index))))
	(if ($fx= index end)
	    word
	  (next-byte bv ($fxsub1 index) (sll word 8)))))))

(define-inline ($bytevector-s64l-set! ?bv ?index ?word)
  (let ((index	?index)
	(word	?word))
    (let next-byte ((bv     ?bv)
		    (index  index)
		    (end    ($fx+ 7 index))
		    (word   word))
      (if ($fx= index end)
	  ($bytevector-s8-set! bv index (bitwise-and word #xFF))
	(begin
	  ($bytevector-u8-set! bv index (bitwise-and word #xFF))
	  (next-byte bv ($fxadd1 index) end (sra word 8)))))))

;;; --------------------------------------------------------------------

(define-syntax $bytevector-u64n-ref
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-u64b-ref))
    ((little)
     (identifier-syntax $bytevector-u64l-ref))))

(define-syntax $bytevector-u64n-set!
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-u64b-set!))
    ((little)
     (identifier-syntax $bytevector-u64l-set!))))

;;; --------------------------------------------------------------------

(define-syntax $bytevector-s64n-ref
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-s64b-ref))
    ((little)
     (identifier-syntax $bytevector-s64l-ref))))

(define-syntax $bytevector-s64n-set!
  (case config.platform-endianness
    ((big)
     (identifier-syntax $bytevector-s64b-set!))
    ((little)
     (identifier-syntax $bytevector-s64l-set!))))


;;;; miscellaneous bytevector operations

(define-inline ($bytevector-fill! ?bv ?index ?end ?fill)
  ;;Fill the  positions in ?BV  from ?INDEX inclusive to  ?END exclusive
  ;;with ?FILL.
  ;;
  (let loop ((bv ?bv) (index ?index) (end ?end) (fill ?fill))
    (if ($fx= index end)
	bv
      (begin
	($bytevector-u8-set! bv index fill)
	(loop bv ($fxadd1 index) end fill)))))

(define-inline ($bytevector-copy! ?src.bv ?src.start
				  ?dst.bv ?dst.start
				  ?src.end)
  ;;Copy  the octets of  ?SRC.BV from  ?SRC.START inclusive  to ?SRC.END
  ;;exclusive, to ?DST.BV starting at ?DST.START inclusive.
  ;;
  (let loop ((src.bv ?src.bv) (src.start ?src.start)
	     (dst.bv ?dst.bv) (dst.start ?dst.start)
	     (src.end ?src.end))
    (if ($fx= src.start src.end)
	dst.bv
      (begin
	($bytevector-u8-set! dst.bv dst.start ($bytevector-u8-ref src.bv src.start))
	(loop src.bv ($fxadd1 src.start)
	      dst.bv ($fxadd1 dst.start)
	      src.end)))))

(define-inline ($bytevector-copy!/count ?src.bv ?src.start ?dst.bv ?dst.start ?count)
  ;;Copy ?COUNT octets from  ?SRC.BV starting at ?SRC.START inclusive to
  ;;?DST.BV starting at ?DST.START inclusive.
  ;;
  (let ((src.end ($fx+ ?src.start ?count)))
    ($bytevector-copy! ?src.bv ?src.start
		       ?dst.bv ?dst.start
		       src.end)))

(define-inline ($bytevector-self-copy-forwards! ?bv ?src.start ?dst.start ?count)
  ;;Copy ?COUNT  octets of ?BV  from ?SRC.START inclusive to  ?BV itself
  ;;starting at ?DST.START inclusive.   The copy happens forwards, so it
  ;;is suitable for the case ?SRC.START > ?DST.START.
  ;;
  (let loop ((bv	?bv)
	     (src.start	?src.start)
	     (dst.start	?dst.start)
	     (src.end	($fx+ ?src.start ?count)))
    (unless ($fx= src.start src.end)
      ($bytevector-u8-set! bv dst.start ($bytevector-u8-ref bv src.start))
      (loop bv ($fxadd1 src.start) ($fxadd1 dst.start) src.end))))

(define-inline ($bytevector-self-copy-backwards! ?bv ?src.start ?dst.start ?count)
  ;;Copy ?COUNT  octets of ?BV  from ?SRC.START inclusive to  ?BV itself
  ;;starting at ?DST.START inclusive.  The copy happens backwards, so it
  ;;is suitable for the case ?SRC.START < ?DST.START.
  ;;
  (let loop ((bv	?bv)
	     (src.start	($fx+ ?src.start ?count))
	     (dst.start	($fx+ ?dst.start ?count))
	     (src.end	?src.start))
    (unless ($fx= src.start src.end)
      (let ((src.start ($fxsub1 src.start))
	    (dst.start ($fxsub1 dst.start)))
	($bytevector-u8-set! bv dst.start ($bytevector-u8-ref bv src.start))
	(loop bv src.start dst.start src.end)))))


;;;; miscellaneous string operations

(define-inline ($string-fill! ?str ?index ?end ?fill)
  ;;Fill the positions  in ?STR from ?INDEX inclusive  to ?END exclusive
  ;;with ?FILL.
  ;;
  (let loop ((str ?str) (index ?index) (end ?end) (fill ?fill))
    (if ($fx= index end)
	str
      (begin
	($string-set! str index fill)
	(loop str ($fxadd1 index) end fill)))))

(define-inline ($string-copy! ?src.str ?src.start
			      ?dst.str ?dst.start
			      ?src.end)
  ;;Copy  the  characters  of  ?SRC.STR  from  ?SRC.START  inclusive  to
  ;;?SRC.END exclusive, to ?DST.STR starting at ?DST.START inclusive.
  ;;
  (let loop ((src.str ?src.str) (src.start ?src.start)
	     (dst.str ?dst.str) (dst.start ?dst.start)
	     (src.end ?src.end))
    (if ($fx= src.start src.end)
	dst.str
      (begin
	($string-set! dst.str dst.start ($string-ref src.str src.start))
	(loop src.str ($fxadd1 src.start)
	      dst.str ($fxadd1 dst.start)
	      src.end)))))

(define-inline ($string-copy!/count ?src.str ?src.start ?dst.str ?dst.start ?count)
  ;;Copy  ?COUNT   characters  from  ?SRC.STR   starting  at  ?SRC.START
  ;;inclusive to ?DST.STR starting at ?DST.START inclusive.
  ;;
  (let ((src.end ($fx+ ?src.start ?count)))
    ($string-copy! ?src.str ?src.start
		   ?dst.str ?dst.start
		   src.end)))

(define-inline ($string-self-copy-forwards! ?str ?src.start ?dst.start ?count)
  ;;Copy  ?COUNT characters of  ?STR from  ?SRC.START inclusive  to ?STR
  ;;itself starting at ?DST.START inclusive.  The copy happens forwards,
  ;;so it is suitable for the case ?SRC.START > ?DST.START.
  ;;
  (let loop ((str	?str)
	     (src.start	?src.start)
	     (dst.start	?dst.start)
	     (src.end	($fx+ ?src.start ?count)))
    (unless ($fx= src.start src.end)
      ($string-set! str dst.start ($string-ref str src.start))
      (loop str ($fxadd1 src.start) ($fxadd1 dst.start) src.end))))

(define-inline ($string-self-copy-backwards! ?str ?src.start ?dst.start ?count)
  ;;Copy  ?COUNT characters of  ?STR from  ?SRC.START inclusive  to ?STR
  ;;itself   starting  at  ?DST.START   inclusive.   The   copy  happens
  ;;backwards, so it is suitable for the case ?SRC.START < ?DST.START.
  ;;
  (let loop ((str	?str)
	     (src.start	($fx+ ?src.start ?count))
	     (dst.start	($fx+ ?dst.start ?count))
	     (src.end	?src.start))
    (unless ($fx= src.start src.end)
      (let ((src.start ($fxsub1 src.start))
	    (dst.start ($fxsub1 dst.start)))
	($string-set! str dst.start ($string-ref str src.start))
	(loop str src.start dst.start src.end)))))

(define-inline ($substring ?str ?start ?end)
  ;;Return  a  new  string  holding  characters from  ?STR  from  ?START
  ;;inclusive to ?END exclusive.
  ;;
  (let ((dst.len ($fx- ?end ?start)))
    (if ($fx< 0 dst.len)
	(let ((dst.str ($make-string dst.len)))
	  ($string-copy! ?str ?start dst.str 0 ?end)
	  dst.str)
      "")))


;;;; miscellaneous vector operations

(define-inline ($make-clean-vector ?len)
  (let* ((len ?len)
	 (vec ($make-vector ?len)))
    ($vector-clean! vec)))

(define-inline ($vector-clean! ?vec)
  (foreign-call "ikrt_vector_clean" ?vec))

(define-inline ($vector-fill! ?vec ?index ?end ?fill)
  ;;Fill the positions  in ?VEC from ?INDEX inclusive  to ?END exclusive
  ;;with ?FILL.
  ;;
  (let loop ((vec ?vec) (index ?index) (end ?end) (fill ?fill))
    (if ($fx= index end)
	vec
      (begin
	($vector-set! vec index fill)
	(loop vec ($fxadd1 index) end fill)))))

(define-inline ($vector-copy! ?src.vec ?src.start
			      ?dst.vec ?dst.start
			      ?src.end)
  ;;Copy  the items of  ?SRC.VEC from  ?SRC.START inclusive  to ?SRC.END
  ;;exclusive, to ?DST.VEC starting at ?DST.START inclusive.
  ;;
  (let loop ((src.vec ?src.vec) (src.start ?src.start)
	     (dst.vec ?dst.vec) (dst.start ?dst.start)
	     (src.end ?src.end))
    (if ($fx= src.start src.end)
	dst.vec
      (begin
	($vector-set! dst.vec dst.start ($vector-ref src.vec src.start))
	(loop src.vec ($fxadd1 src.start)
	      dst.vec ($fxadd1 dst.start)
	      src.end)))))

(define-inline ($vector-self-copy-forwards! ?vec ?src.start ?dst.start ?count)
  ;;Copy ?COUNT items  of ?VEC from ?SRC.START inclusive  to ?VEC itself
  ;;starting at ?DST.START inclusive.   The copy happens forwards, so it
  ;;is suitable for the case ?SRC.START > ?DST.START.
  ;;
  (let loop ((vec	?vec)
	     (src.start	?src.start)
	     (dst.start	?dst.start)
	     (src.end	($fx+ ?src.start ?count)))
    (unless ($fx= src.start src.end)
      ($vector-set! vec dst.start ($vector-ref vec src.start))
      (loop vec ($fxadd1 src.start) ($fxadd1 dst.start) src.end))))

(define-inline ($vector-self-copy-backwards! ?vec ?src.start ?dst.start ?count)
  ;;Copy ?COUNT items  of ?VEC from ?SRC.START inclusive  to ?VEC itself
  ;;starting at ?DST.START inclusive.  The copy happens backwards, so it
  ;;is suitable for the case ?SRC.START < ?DST.START.
  ;;
  (let loop ((vec	?vec)
	     (src.start	($fx+ ?src.start ?count))
	     (dst.start	($fx+ ?dst.start ?count))
	     (src.end	?src.start))
    (unless ($fx= src.start src.end)
      (let ((src.start ($fxsub1 src.start))
	    (dst.start ($fxsub1 dst.start)))
	($vector-set! vec dst.start ($vector-ref vec src.start))
	(loop vec src.start dst.start src.end)))))

(define-inline ($subvector ?vec ?start ?end)
  ;;Return a new vector holding items from ?VEC from ?START inclusive to
  ;;?END exclusive.
  ;;
  (let ((dst.len ($fx- ?end ?start)))
    (if ($fx< 0 dst.len)
	(let ((dst.vec ($make-vector dst.len)))
	  ($vector-copy! ?vec ?start dst.vec 0 ?end)
	  dst.vec)
      '#())))


;;;; characters

;;; Line endings
;;
;;R6RS defines  the following standalone and sequences  of characters as
;;line endings:
;;
;;   #\x000A		linefeed
;;   #\x000D		carriage return
;;   #\x0085		next line
;;   #\x2028		line separator
;;   #\x000D #\000A	carriage return + linefeed
;;   #\x000D #\0085	carriage return + next line
;;
;;to  process line  endings  from a  textual  input port,  we should  do
;;something like this:
;;
;; (let ((ch (read-char port)))
;;   (cond ((eof-object? ch)
;;          (handle-eof))
;;         (($char-is-single-char-line-ending? ch)
;;          (handle-ending ch))
;;         (($char-is-carriage-return? ch)
;;          (let ((ch1 (peek-char port)))
;;            (cond ((eof-object? ch1)
;;                   (handle-ending ch))
;;                  (($char-is-newline-after-carriage-return? ch1)
;;                   (read-char port)
;;                   (handle-ending ch ch1))
;;                  (else
;;                   (handle-ending ch))))
;;         (else
;;          (handle-other-char ch)))))
;;

(define-inline ($char-is-single-char-line-ending? ch)
  (or ($fx= ch #\x000A)	  ;linefeed
      ($fx= ch #\x0085)	  ;next line
      ($fx= ch #\x2028))) ;line separator

(define-inline ($char-is-carriage-return? ch)
  ($fx= ch #\xD))

(define-inline ($char-is-newline-after-carriage-return? ch)
  ;;This is used to recognise 2-char newline sequences.
  ;;
  (or ($fx= ch #\x000A)	  ;linefeed
      ($fx= ch #\x0085))) ;next line


;;;; done

)

;;; end of file
