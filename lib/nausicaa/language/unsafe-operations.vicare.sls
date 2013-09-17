;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: unsafe operations
;;;Date: Wed May  2, 2012
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
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language unsafe-operations)
  (export
    $make-struct
    $struct
    $struct-rtd
    $struct/rtd?
    $struct-length
    $struct-ref
    $struct-set!

    $caar
    $cadr
    $cdar
    $cddr

    $caaar
    $caadr
    $cadar
    $cdaar
    $cdadr
    $cddar
    $cdddr
    $caddr

    $fxzero?
    $fxnegative?
    $fxpositive?
    $fxadd1	;increment
    $fxsub1	;decrement
    $fxneg	;negation
    $fxsra	;shift right
    $fxsll	;shift left
    $fxlogor	;inclusive logic OR
    $fxlogxor	;exlusive logic OR
    $fxlogand	;logic AND
    $fxlognot	;logic not
    $fx+
    $fx-
    $fx*
    $fxdiv
    $fxdiv0
    $fxmod
    $fxmod0
    $fx<
    $fx>
    $fx>=
    $fx<=
    $fx=

    $fxand	;multiple arguments AND
    $fxior	;multiple arguments inclusive OR
    $fxxor	;multiple arguments exclusive OR
    $fxmax	;multiple arguments max
    $fxadd2
    $fxadd3
    $fxadd4
    $fxincr!

;;; --------------------------------------------------------------------

    $bignum-positive?
    $bignum-negative?
    $bignum-byte-ref
    $bignum-size

    $bnbn=
    $bnbn<
    $bnbn>
    $bnbn<=
    $bnbn>=

;;; --------------------------------------------------------------------

    $make-ratnum
    $ratnum-n
    $ratnum-d
    $ratnum-num
    $ratnum-den

;;; --------------------------------------------------------------------

    $make-flonum
    $flonum-u8-ref
    $flonum-set!
    $fixnum->flonum
    $fl+
    $fl-
    $fl*
    $fl/
    $fl=
    $fl<
    $fl>
    $fl<=
    $fl>=
    $flonum-sbe

;;; --------------------------------------------------------------------

    $make-cflonum
    $cflonum-real
    $cflonum-imag
    $make-compnum
    $compnum-real
    $compnum-imag

;;; --------------------------------------------------------------------

    $make-bytevector
    $bytevector-length
    $bytevector-empty?
    $bytevector-u8-ref
    $bytevector-s8-ref
    $bytevector-u8-set!
    $bytevector-s8-set!
    $bytevector-ieee-double-native-ref
    $bytevector-ieee-double-native-set!
    $bytevector-ieee-single-native-ref
    $bytevector-ieee-single-native-set!
    $bytevector-ieee-single-nonnative-ref
    $bytevector-ieee-single-nonnative-set!
    $bytevector-ieee-double-nonnative-set!
    $bytevector-ieee-double-nonnative-ref

    $bytevector-u16b-ref
    $bytevector-u16b-set!
    $bytevector-u16l-ref
    $bytevector-u16l-set!
    $bytevector-s16b-ref
    $bytevector-s16b-set!
    $bytevector-s16l-ref
    $bytevector-s16l-set!
    $bytevector-u16n-ref
    $bytevector-u16n-set!
    $bytevector-s16n-ref
    $bytevector-s16n-set!

    $bytevector-u16-ref
    $bytevector-u16-set!

    $bytevector-u32b-ref
    $bytevector-u32b-set!
    $bytevector-u32l-ref
    $bytevector-u32l-set!
    $bytevector-s32b-ref
    $bytevector-s32b-set!
    $bytevector-s32l-ref
    $bytevector-s32l-set!
    $bytevector-u32n-ref
    $bytevector-u32n-set!
    $bytevector-s32n-ref
    $bytevector-s32n-set!

    $bytevector-u64b-ref
    $bytevector-u64b-set!
    $bytevector-u64l-ref
    $bytevector-u64l-set!
    $bytevector-s64b-ref
    $bytevector-s64b-set!
    $bytevector-s64l-ref
    $bytevector-s64l-set!
    $bytevector-u64n-ref
    $bytevector-u64n-set!
    $bytevector-s64n-ref
    $bytevector-s64n-set!

    $bytevector=
    $bytevector-fill!
    $bytevector-copy!
    $bytevector-copy!/count
    $bytevector-self-copy-forwards!
    $bytevector-self-copy-backwards!

    ;;FIXME To be  uncommented at the next boot  image rotation.  (Marco
    ;;Maggi; Fri May 17, 2013)
    #;$bytevector-total-length
    #;$bytevector-concatenate
    #;$bytevector-reverse-and-concatenate

;;; --------------------------------------------------------------------

    $car
    $cdr
    $set-car!
    $set-cdr!

;;; --------------------------------------------------------------------

    $make-vector
    $vector-length
    $vector-ref
    $vector-set!

    $vector-copy!
    $vector-self-copy-forwards!
    $vector-self-copy-backwards!
    $vector-fill!
    $subvector
    $vector-clean!
    $make-clean-vector

;;; --------------------------------------------------------------------

    $char=
    $char<
    $char>
    $char>=
    $char<=
    $char->fixnum
    $fixnum->char

    $char-is-single-char-line-ending?
    $char-is-carriage-return?
    $char-is-newline-after-carriage-return?

;;; --------------------------------------------------------------------

    $make-string
    $string-length
    $string-ref
    $string-set!
    $string=

    $string-copy!
    $string-copy!/count
    $string-self-copy-forwards!
    $string-self-copy-backwards!
    $string-fill!
    $substring

    ;;FIXME To be  uncommented at the next boot  image rotation.  (Marco
    ;;Maggi; Fri May 17, 2013)
    #;$string-total-length
    #;$string-concatenate
    #;$string-reverse-and-concatenate

;;; --------------------------------------------------------------------

    $pointer?
    $pointer=

;;; --------------------------------------------------------------------

    $memory-block-pointer
    $memory-block-size

;;; --------------------------------------------------------------------

    $closure-code
    $code->closure
    $code-reloc-vector
    $code-freevars
    $code-size
    $code-annotation
    $code-ref
    $code-set!
    $set-code-annotation!

    #| end of export |# )
  (import (vicare unsafe operations)))

;;; end of file
