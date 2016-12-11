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
;;;Copyright (C) 2011-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    $make-struct
    $struct
    $struct-rtd
    $struct/rtd?
    $struct-length
    $struct-ref
    $struct-set!

;;; --------------------------------------------------------------------

    $fxzero?
    $fxnegative?
    $fxpositive?
    $fxnonpositive?
    $fxnonnegative?
    $fxodd?
    $fxeven?
    $fxsign
    $fxabs
    $fxadd1	;increment
    $fxsub1	;decrement
    $fxneg	;negation
    $fxsra	;shift right
    $fxsll	;shift left
    $fx+
    $fx-
    $fx*
    $fxdiv
    $fxdiv0
    $fxmod
    $fxmod0
    $fxdiv-and-mod
    $fxdiv0-and-mod0
    $fx<	;multiple arguments
    $fx>	;multiple arguments
    $fx>=	;multiple arguments
    $fx<=	;multiple arguments
    $fx=	;multiple arguments

    $fxmax	;multiple arguments max
    $fxadd2
    $fxadd3
    $fxadd4
    $fxincr!
    $fxdecr!

;;; --------------------------------------------------------------------

    $bignum-positive?
    $bignum-negative?
    $bignum-non-positive?
    $bignum-non-negative?
    $bignum-byte-ref
    $bignum-size

    $bnbn=
    $bnbn<
    $bnbn>
    $bnbn<=
    $bnbn>=

;;; --------------------------------------------------------------------

    $make-ratnum
    $ratnum-num
    $ratnum-den
    $ratnum-positive?
    $ratnum-negative?
    $ratnum-non-positive?
    $ratnum-non-negative?

;;; --------------------------------------------------------------------

    $make-flonum
    $flonum-u8-ref
    $flonum-set!
    $fixnum->flonum
    $fixnum->string
    $fixnum->char
    $flonum-integer?
    $flonum-rational?
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
    $flzero?
    $flzero?/positive
    $flzero?/negative
    $flpositive?
    $flnegative?
    $flnonpositive?
    $flnonnegative?
    $flodd?
    $fleven?
    $flnan?
    $flfinite?
    $flinfinite?

    $flround
    $flfloor
    $flceiling
    $fltruncate

    $flnumerator
    $fldenominator

    $flmax
    $flmin

    $flabs
    $flsin
    $flcos
    $fltan
    $flasin
    $flacos
    $flatan		$flatan2
    $flsinh
    $flcosh
    $fltanh
    $flasinh
    $flacosh
    $flatanh
    $flexp
    $fllog		$fllog2
    $flexpm1
    $fllog1p
    $flexpt
    $flsquare
    $flcube
    $flsqrt
    $flcbrt
    $flhypot

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
    $bytevector-ieee-double-native-ref
    $bytevector-ieee-double-native-set!
    $bytevector-ieee-single-native-ref
    $bytevector-ieee-single-native-set!
    $bytevector-ieee-single-nonnative-ref
    $bytevector-ieee-single-nonnative-set!
    $bytevector-ieee-double-nonnative-set!
    $bytevector-ieee-double-nonnative-ref

;;; --------------------------------------------------------------------

    $car
    $cdr
    $set-car!
    $set-cdr!

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

;;; --------------------------------------------------------------------

    $length

;;; --------------------------------------------------------------------

    $make-vector
    $vector-ref
    $vector-set!
    $vector-length

    $vector-map1
    $vector-for-each1
    $vector-for-all1
    $vector-exists1

    $vector-copy
    $vector-copy!
    $vector-fill!
    $vector-clean!
    $make-clean-vector

;;; --------------------------------------------------------------------

    $char=
    $char!=
    $char<
    $char>
    $char>=
    $char<=
    $char->fixnum

    $char-is-single-char-line-ending?
    $char-is-carriage-return?
    $char-is-newline-after-carriage-return?

;;; --------------------------------------------------------------------

    $make-string
    $string-length
    $string-empty?
    $string-ref
    $string-set!
    $string=
    $string
    $uri-encoded-string?
    $percent-encoded-string?

    $string-total-length
    $string-concatenate
    $string-reverse-and-concatenate

    $string->octets
    $octets->string
    $octets-encoded-bytevector?

    $string->ascii
    $ascii->string
    $ascii-encoded-bytevector?
    $ascii-encoded-string?

    $string->latin1
    $latin1->string
    $latin1-encoded-bytevector?
    $latin1-encoded-string?

    $string-base64->bytevector
    $bytevector->string-base64

    $uri-encode
    $uri-decode
    $uri-normalise-encoding
    $uri-encoded-bytevector?
    $percent-encode
    $percent-decode
    $percent-normalise-encoding
    $percent-encoded-bytevector?

;;; --------------------------------------------------------------------

    $string-hash
    $string-ci-hash
    $symbol-hash
    $bytevector-hash
    $symbol->string
    $string->symbol
    $putprop
    $getprop
    $remprop
    $property-list

;;; --------------------------------------------------------------------

    $symbol->keyword
    $keyword->symbol
    $keyword-hash
    $keyword=?

;;; --------------------------------------------------------------------

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
  (import (vicare)
    (vicare system $symbols)
    (vicare system $keywords)
    (vicare system $structs)
    (except (vicare system $fx)
	    $fxmax
	    $fxmin
	    $fx<
	    $fx>
	    $fx>=
	    $fx<=
	    $fx=
	    $fxand
	    $fxior
	    $fxxor
	    $fxnot)
    (prefix (only (vicare system $fx)
		  $fx<
		  $fx>
		  $fx>=
		  $fx<=
		  $fx=)
	    sys.)
    (vicare system $bignums)
    (vicare system $ratnums)
    (vicare system $flonums)
    (vicare system $compnums)
    (vicare system $pairs)
    (vicare system $lists)
    (except (vicare system $vectors)
	    $make-clean-vector)
    (vicare system $bytevectors)
    (except (vicare system $chars)
	    $char=
	    $char<
	    $char>
	    $char>=
	    $char<=)
    (prefix (only (vicare system $chars)
		  $char=
		  $char<
		  $char>
		  $char>=
		  $char<=)
	    sys.)
    (vicare system $strings)
    (vicare system $codes)
    (vicare system $pointers)
    (vicare system $hashtables)
    (for (prefix (only (vicare platform configuration)
		       platform-endianness)
		 config.)
	 expand))


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

;;; --------------------------------------------------------------------

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
    ((_ ?op)
     ($fxincr! ?op 1))
    ((_ ?op 0)
     ?op)
    ((_ ?op 1)
     (set! ?op ($fxadd1 ?op)))
    ((_ ?op 2)
     (set! ?op ($fxadd2 ?op)))
    ((_ ?op 3)
     (set! ?op ($fxadd3 ?op)))
    ((_ ?op 4)
     (set! ?op ($fxadd4 ?op)))
    ((_ ?op ?N)
     (set! ?op ($fx+ ?op ?N)))
    ))

;;; --------------------------------------------------------------------

(define-syntax $fxsub2
  (syntax-rules ()
    ((_ ?op)
     ($fx- ?op 2))))

(define-syntax $fxsub3
  (syntax-rules ()
    ((_ ?op)
     ($fx- ?op 3))))

(define-syntax $fxsub4
  (syntax-rules ()
    ((_ ?op)
     ($fx- ?op 4))))

(define-syntax $fxdecr!
  (syntax-rules ()
    ((_ ?op)
     ($fxdecr! ?op 1))
    ((_ ?op 0)
     ?op)
    ((_ ?op 1)
     (set! ?op ($fxsub1 ?op)))
    ((_ ?op 2)
     (set! ?op ($fxsub2 ?op)))
    ((_ ?op 3)
     (set! ?op ($fxsub3 ?op)))
    ((_ ?op 4)
     (set! ?op ($fxsub4 ?op)))
    ((_ ?op ?N)
     (set! ?op ($fx+ ?op ?N)))
    ))

;;; --------------------------------------------------------------------

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

(let-syntax ((define-compar (syntax-rules ()
			      ((_ ?proc sys.?proc)
			       (define-syntax ?proc
				 (syntax-rules ()
				   ((_ ?op1 ?op2)
				    (sys.?proc ?op1 ?op2))
				   ((_ ?op1 ?op2 ?op3 ?op4 (... ...))
				    (let ((op2 ?op2))
				      (and (sys.?proc ?op1 op2)
					   (?proc op2 ?op3 ?op4 (... ...))))))))
			      )))
  (define-compar $fx=  sys.$fx=)
  (define-compar $fx<  sys.$fx<)
  (define-compar $fx<= sys.$fx<=)
  (define-compar $fx>  sys.$fx>)
  (define-compar $fx>= sys.$fx>=))


;;;; bignums

(define-syntax-rule (%bnbncmp X Y fxcmp)
  (fxcmp (foreign-call "ikrt_bnbncomp" X Y) 0))

(define-syntax-rule ($bnbn= X Y)
  (%bnbncmp X Y $fx=))

(define-syntax-rule ($bnbn< X Y)
  (%bnbncmp X Y $fx<))

(define-syntax-rule ($bnbn> X Y)
  (%bnbncmp X Y $fx>))

(define-syntax-rule ($bnbn<= X Y)
  (%bnbncmp X Y $fx<=))

(define-syntax-rule ($bnbn>= X Y)
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


;;;; miscellaneous vector operations

(define-syntax-rule ($make-clean-vector ?len)
  (foreign-call "ikrt_make_vector1" ?len))

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

(define-inline ($vector-copy ?vec)
  (let* ((src.vec ?vec)
	 (src.len ($vector-length src.vec))
	 (dst.vec ($make-vector src.len)))
    ($vector-copy! src.vec 0 dst.vec 0 src.len)
    dst.vec))

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
  (or ($char= ch #\x000A)   ;linefeed
      ($char= ch #\x0085)   ;next line
      ($char= ch #\x2028))) ;line separator

(define-inline ($char-is-carriage-return? ch)
  ($char= ch #\xD))

(define-inline ($char-is-newline-after-carriage-return? ch)
  ;;This is used to recognise 2-char newline sequences.
  ;;
  (or ($char= ch #\x000A)   ;linefeed
      ($char= ch #\x0085))) ;next line

;;; --------------------------------------------------------------------

(let-syntax ((define-compar (syntax-rules ()
			      ((_ ?proc sys.?proc)
			       (define-syntax ?proc
				 (syntax-rules ()
				   ((_ ?op1 ?op2)
				    (sys.?proc ?op1 ?op2))
				   ((_ ?op1 ?op2 ?op3 ?op4 (... ...))
				    (let ((op2 ?op2))
				      (and (sys.?proc ?op1 op2)
					   (?proc op2 ?op3 ?op4 (... ...))))))))
			      )))
  (define-compar $char=  sys.$char=)
  (define-compar $char<  sys.$char<)
  (define-compar $char<= sys.$char<=)
  (define-compar $char>  sys.$char>)
  (define-compar $char>= sys.$char>=))


;;;; done

)

;;; end of file
