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
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare unsafe-operations)
  (export
    (rename ($fxzero?	fxzero?)
	    ($fxadd1	fxadd1)		;increment
	    ($fxsub1	fxsub1)		;decrement
	    ($fxsra	fxsra)		;shift right
	    ($fxsll	fxsll)		;shift left
	    ($fxlogor	fxlogor)	;inclusive logic OR
	    ($fxlogxor	fxlogxor)	;exlusive logic OR
	    ($fxlogand	fxand)		;logic AND
	    ($fx+	fx+)
	    ($fx-	fx-)
	    ($fx*	fx*)
	    ($fx<	fx<)
	    ($fx>	fx>)
	    ($fx>=	fx>=)
	    ($fx<=	fx<=)
	    ($fx=	fx=))

    (rename ($fxior	fxior)		;multiple arguments inclusive OR
	    ($fxxor	fxxor))		;multiple arguments exclusive OR

    (rename ($bignum-positive?		bignum-positive?)
	    ($bignum-byte-ref		bignum-byte-ref)
	    ($bignum-size		bignum-size))

    (rename ($bnbn=			bnbn=)
	    ($bnbn<			bnbn<)
	    ($bnbn>			bnbn>)
	    ($bnbn<=			bnbn<=)
	    ($bnbn>=			bnbn>=))

    (rename ($make-ratnum		make-ratnum)
	    ($ratnum-n			ratnum-n)
	    ($ratnum-d			ratnum-d))

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

    (rename ($make-cflonum		make-cflonum)
	    ($cflonum-real		cflonum-real)
	    ($cflonum-imag		cflonum-imag)
	    ($make-compnum		make-compnum)
	    ($compnum-real		compnum-real)
	    ($compnum-imag		compnum-imag))

    (rename ($make-bytevector		make-bytevector)
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

    (rename ($car		car)
	    ($cdr		cdr)
	    ($set-car!		set-car!)
	    ($set-cdr!		set-cdr!))

    (rename ($make-vector	make-vector)
	    ($vector-length	vector-length)
	    ($vector-ref	vector-ref)
	    ($vector-set!	vector-set!))

    (rename ($char=		char=)
	    ($char<		char<)
	    ($char>		char>)
	    ($char>=		char>=)
	    ($char<=		char<=)
	    ($char->fixnum	char->fixnum)
	    ($fixnum->char	fixnum->char))

    (rename ($make-string	make-string)
	    ($string-length	string-length)
	    ($string-ref	string-ref)
	    ($string-set!	string-set!))
    )
  (import (ikarus)
    (ikarus system $fx)
    (ikarus system $bignums)
    (ikarus system $ratnums)
    (ikarus system $flonums)
    (ikarus system $compnums)
    (ikarus system $pairs)
    (ikarus system $vectors)
    (ikarus system $bytevectors)
    (ikarus system $chars)
    (ikarus system $strings)
    (only (vicare syntactic-extensions)
	  define-inline))


;;;; fixnums

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


;;;; done

)

;;; end of file
