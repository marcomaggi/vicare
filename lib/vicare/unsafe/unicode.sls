;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: Unicode characters handling
;;;Date: Fri Nov  4, 2011
;;;
;;;Abstract
;;;
;;;	This library  exports only syntaxes, so  it can be  used also in
;;;	the internals of Vicare's source code.
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; Introduction to Unicode and UCS
;;
;;As  required  by  R6RS,  the  input/output  libraries  must  implement
;;transcoders for textual ports supporting encoding and decoding between
;;Scheme characters and UTF-8 and UTF-16.
;;
;;The  mandatory starting  points  to  learn about  this  stuff are  the
;;following (URLs last verified on Sep 9, 2011):
;;
;;  <http://www.unicode.org/faq/utf_bom.html>
;;  <http://en.wikipedia.org/wiki/Universal_Character_Set>
;;  <http://en.wikipedia.org/wiki/Unicode>
;;  <http://en.wikipedia.org/wiki/Byte_order_mark>
;;  <http://en.wikipedia.org/wiki/UTF-8>
;;  <http://en.wikipedia.org/wiki/UTF-16>
;;  <http://en.wikipedia.org/wiki/UTF-32>
;;
;;here we  give only a brief  overview of the  main definitions, drawing
;;text from those pages.
;;
;;The "Universal  Character Set" (UCS)  is a standard set  of characters
;;upon which  many character encodings  are based; it  contains abstract
;;characters,  each identified  by an  unambiguous name  and  an integer
;;number called its "code point".
;;
;;"Unicode"  is  a  computing   industry  standard  for  the  consistent
;;encoding, representation and handling of text expressed in most of the
;;world's writing systems.
;;
;;UCS and  Unicode have  an identical repertoire  and numbers:  the same
;;characters with  the same numbers exist  in both standards.   UCS is a
;;simple character map, Unicode  adds rules for collation, normalization
;;of forms, and the bidirectional algorithm for scripts.
;;
;;The  Unicode Consortium, the  nonprofit organization  that coordinates
;;Unicode's development,  has the goal of  eventually replacing existing
;;character  encoding schemes  with  Unicode and  its standard  "Unicode
;;Transformation  Format"   alias  "UCS  Transformation   Format"  (UTF)
;;schemes.

;;By  convention a Unicode  code point  is referred  to by  writing "U+"
;;followed by its  hexadecimal number with at least  4 digits (U+0044 is
;;fine, U+12 is not).
;;
;;In practice, Unicode  code points are exact integers  in the range [0,
;;#x10FFFF], but  outside the range  [#xD800, #xDFFF] which  has special
;;meaning in UTF schemes.  A code point can be stored in 21 bits:
;;
;;  (string-length (number->string #x10FFFF 2)) => 21
;;
;;R6RS defines  fixnums to have  at least 24  bits, so a fixnum  is wide
;;enough to hold a code point:
;;
;;  (fixnum? #x10FFFF) => #t
;;
;;and indeed Scheme characters are a disjoint type of value holding such
;;fixnums:
;;
;;  (integer->char #x10FFFF) => #\x10FFFF
;;


#!r6rs
(library (vicare unsafe unicode)
  (export

;;; UTF-8
    utf-8-invalid-octet?

    utf-8-single-octet?
    utf-8-decode-single-octet
    utf-8-valid-code-point-from-1-octet?

    utf-8-first-of-two-octets?
    utf-8-second-of-two-octets?
    utf-8-decode-two-octets
    utf-8-valid-code-point-from-2-octets?

    utf-8-first-of-three-octets?
    utf-8-second-and-third-of-three-octets?
    utf-8-decode-three-octets
    utf-8-valid-code-point-from-3-octets?

    utf-8-first-of-four-octets?
    utf-8-second-third-and-fourth-of-four-octets?
    utf-8-decode-four-octets
    utf-8-valid-code-point-from-4-octets?

    utf-8-single-octet-code-point?
    utf-8-encode-single-octet

    utf-8-two-octets-code-point?
    utf-8-encode-first-of-two-octets
    utf-8-encode-second-of-two-octets

    utf-8-three-octets-code-point?
    utf-8-encode-first-of-three-octets
    utf-8-encode-second-of-three-octets
    utf-8-encode-third-of-three-octets

    utf-8-four-octets-code-point?
    utf-8-encode-first-of-four-octets
    utf-8-encode-second-of-four-octets
    utf-8-encode-third-of-four-octets
    utf-8-encode-fourth-of-four-octets

    utf-8-classify-octet
    utf-8-classify-code-point

;;; UTF-16
    utf-16-single-word?
    utf-16-decode-single-word

    utf-16-first-of-two-words?
    utf-16-second-of-two-words?
    utf-16-decode-surrogate-pair
    utf-16-single-word-code-point?
    utf-16-encode-single-word

    utf-16-two-words-code-point?
    utf-16-encode-first-of-two-words
    utf-16-encode-second-of-two-words
    utf-16-classify-word

;;; Latin-1
    latin-1-octet?
    latin-1-decode
    latin-1-code-point?
    latin-1-encode)
  (import (rnrs)
    (only (vicare language-extensions syntaxes)
	  define-inline)
    (prefix (vicare unsafe operations)
	    unsafe.))


;;;; UTF-8 scheme
;;
;;UTF-8  is  a  multioctet  character  encoding for  Unicode  which  can
;;represent every character in the Unicode set, that is it can represent
;;every code point in the ranges [0, #xD800) and (#xDFFF, #x10FFFF].
;;
;;A stream  of UTF-8 encoded characters  is meant to be  stored octet by
;;octet  in  fixed  order  (and  so  without the  need  to  specify  the
;;endianness of words).
;;
;;The encoding scheme uses sequences of 1,  2, 3 or 4 octets to encode a
;;each code point as shown in  the following table; the first octet in a
;;sequence has a unique bit pattern  in the most significant bits and so
;;it  allows  the determination  of  the  sequence  length; every  octet
;;contains a number of payload  bits which must be concatenated (bitwise
;;inclusive  OR) to  reconstruct the  integer representation  of  a code
;;point:
;;
;; | # of octets | 1st octet | 2nd octet | 3rd octet | 4th octet |
;; |-------------+-----------+-----------+-----------+-----------|
;; |     1        #b0xxxxxxx
;; |     2        #b110xxxxx  #b10xxxxxx
;; |     3        #b1110xxxx  #b10xxxxxx  #b10xxxxxx
;; |     4        #b11110xxx  #b10xxxxxx  #b10xxxxxx  #b10xxxxxx
;;
;; | # of octets | # of payload bits |       hex range     |
;; |-------------+-------------------+---------------------|
;; |     1                         7    [#x0000,   #x007F]
;; |     2                5 + 6 = 11    [#x0080,   #x07FF]
;; |     3            4 + 6 + 6 = 16    [#x0800,   #xFFFF]
;; |     4        3 + 6 + 6 + 6 = 21  [#x010000, #x10FFFF]
;;
;;Note that  octets #xFE  and #xFF  cannot appear in  a valid  stream of
;;UTF-8 encoded  characters.  The sequence of  3 octets is  the one that
;;could encode (but must not) the forbidden range [#xD800, #xDFFF].
;;
;;The  first 128  characters  of the  Unicode  character set  correspond
;;one-to-one with  ASCII and are encoded  using a single  octet with the
;;same binary  value as the corresponding ASCII  character, making valid
;;ASCII text  valid UTF-8  encoded Unicode text  as well.   Such encoded
;;octets have the Most Significant Bit (MSB) set to zero.
;;
;;Although the standard does not  define it, many programs start a UTF-8
;;stream with  a Byte Order Mark  (BOM) composed of the  3 octets: #xEF,
;;#xBB, #xBF.
;;

;;The following  macro definitions assume  that the OCTET  arguments are
;;fixnums representing 1  octet (they are in the  range [0, 255]), while
;;the CODE-POINT arguments are  fixnums representing Unicode code points
;;(they are in  the range [0, #x10FFFF], but  outside the range [#xD800,
;;#xDFFF]).
;;

(define-inline (utf-8-invalid-octet? octet)
  ;;Evaluate to  true if OCTET has a  value that must never  appear in a
  ;;valid UTF-8 stream.
  ;;
  (or (unsafe.fx= octet #xC0)
      (unsafe.fx= octet #xC1)
      (and (unsafe.fx<= #xF5 octet) (unsafe.fx<= octet #xFF))))

;;; -------------------------------------------------------------
;;; decoding 1-octet UTF-8 to code points

(define-inline (utf-8-single-octet? octet)
  ;;Evaluate to  true if OCTET is  valid as 1-octet UTF-8  encoding of a
  ;;Unicode character.
  ;;
  (unsafe.fx< octet 128))

(define-inline (utf-8-decode-single-octet octet)
  ;;Decode the  code point  of a Unicode  character from a  1-octet UTF-8
  ;;encoding.
  ;;
  octet)

(define-inline (utf-8-valid-code-point-from-1-octet? code-point)
  ;;Evaluate to true if CODE-POINT is a valid integer representation for
  ;;a code point decoded from a 2-octets UTF-8 sequence.
  ;;
  (and (unsafe.fx<= 0 code-point) (unsafe.fx<= code-point #x007F)))

;;; -------------------------------------------------------------
;;; decoding 2-octets UTF-8 to code points

(define-inline (utf-8-first-of-two-octets? octet0)
  ;;Evaluate  to  true if  OCTET0  is valid  as  first  of 2-octets  UTF-8
  ;;encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra octet0 5) #b110))

(define-inline (utf-8-second-of-two-octets? octet1)
  ;;Evaluate  to true  if  OCTET1 is  valid  as second  of 2-octets  UTF-8
  ;;encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra octet1 6) #b10))

(define-inline (utf-8-decode-two-octets octet0 octet1)
  ;;Decode the  code point of a  Unicode character from  a 2-octets UTF-8
  ;;encoding.
  ;;
  (unsafe.fxior (unsafe.fxsll (unsafe.fxand octet0 #b11111) 6)
		 (unsafe.fxand octet1 #b111111)))

(define-inline (utf-8-valid-code-point-from-2-octets? code-point)
  ;;Evaluate to true if CODE-POINT is a valid integer representation for
  ;;a code point decoded from a 2-octets UTF-8 sequence.
  ;;
  (and (unsafe.fx<= #x0080 code-point) (unsafe.fx<= code-point #x07FF)))

;;; -------------------------------------------------------------
;;; decoding 3-octets UTF-8 to code points

(define-inline (utf-8-first-of-three-octets? octet0)
  ;;Evaluate  to  true if  OCTET0  is valid  as  first  of 3-octets  UTF-8
  ;;encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra octet0 4) #b1110))

(define-inline (utf-8-second-and-third-of-three-octets? octet1 octet2)
  ;;Evaluate to true if OCTET1 and OCTET2 are valid as second and third of
  ;;3-octets UTF-8 encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra (unsafe.fxior octet1 octet2) 6) #b10))

(define-inline (utf-8-decode-three-octets octet0 octet1 octet2)
  ;;Decode the  code point of a  Unicode character from  a 3-octets UTF-8
  ;;encoding.
  ;;
  (unsafe.fxior (unsafe.fxsll (unsafe.fxand octet0   #b1111) 12)
		 (unsafe.fxsll (unsafe.fxand octet1 #b111111)  6)
		 (unsafe.fxand octet2 #b111111)))

(define-inline (utf-8-valid-code-point-from-3-octets? code-point)
  ;;Evaluate to true if CODE-POINT is a valid integer representation for
  ;;a code point decoded from a 3-octets UTF-8 sequence.
  ;;
  (and (unsafe.fx<= #x0800 code-point) (unsafe.fx<= code-point #xFFFF)
       (or (unsafe.fx< code-point #xD800) (unsafe.fx<  #xDFFF code-point))))

;;; -------------------------------------------------------------
;;; decoding 4-octets UTF-8 to code points

(define-inline (utf-8-first-of-four-octets? octet0)
  ;;Evaluate  to  true if  OCTET0  is valid  as  first  of 4-octets  UTF-8
  ;;encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra octet0 3) #b11110))

(define-inline (utf-8-second-third-and-fourth-of-four-octets? octet1 octet2 octet3)
  ;;Evaluate  to true if  OCTET1, OCTET2  and OCTET3  are valid  as second,
  ;;third and fourth of 4-octets UTF-8 encoding of a Unicode character.
  ;;
  (unsafe.fx= (unsafe.fxsra (unsafe.fxior octet1 octet2 octet3) 6) #b10))

(define-inline (utf-8-decode-four-octets octet0 octet1 octet2 octet3)
  ;;Decode the  code point of a  Unicode character from  a 4-octets UTF-8
  ;;encoding.
  ;;
  (unsafe.fxior (unsafe.fxsll (unsafe.fxand octet0    #b111) 18)
		 (unsafe.fxsll (unsafe.fxand octet1 #b111111) 12)
		 (unsafe.fxsll (unsafe.fxand octet2 #b111111)  6)
		 (unsafe.fxand octet3 #b111111)))

(define-inline (utf-8-valid-code-point-from-4-octets? code-point)
  ;;Evaluate to true if CODE-POINT is a valid integer representation for
  ;;a code point decoded from a 3-octets UTF-8 sequence.
  ;;
  (and (unsafe.fx<= #x010000 code-point) (unsafe.fx<= code-point #x10FFFF)))

;;; -------------------------------------------------------------
;;; encoding code points to 1-octet UTF-8

(define-inline (utf-8-single-octet-code-point? code-point)
  (and (unsafe.fx<= 0 code-point) (unsafe.fx<= code-point 127)))

(define-inline (utf-8-encode-single-octet code-point)
  ;;Encode  the code point  of a  Unicode character  to a  1-octet UTF-8
  ;;encoding.
  ;;
  code-point)

;;; --------------------------------------------------------------------
;;; encoding code points to 2-octet UTF-8

(define-inline (utf-8-two-octets-code-point? code-point)
  (and (unsafe.fx>  code-point 127)
       (unsafe.fx<= code-point #x7FF)))

(define-inline (utf-8-encode-first-of-two-octets code-point)
  (unsafe.fxior #b11000000 (unsafe.fxsra code-point 6)))

(define-inline (utf-8-encode-second-of-two-octets code-point)
  (unsafe.fxior #b10000000 (unsafe.fxand code-point #b111111)))

;;; --------------------------------------------------------------------
;;; encoding code points to 3-octet UTF-8

(define-inline (utf-8-three-octets-code-point? code-point)
  (and (unsafe.fx>  code-point #x7FF)
       (unsafe.fx<= code-point #xFFFF)))

(define-inline (utf-8-encode-first-of-three-octets code-point)
  (unsafe.fxior #b11100000 (unsafe.fxsra code-point 12)))

(define-inline (utf-8-encode-second-of-three-octets code-point)
  (unsafe.fxior #b10000000 (unsafe.fxand (unsafe.fxsra code-point 6) #b111111)))

(define-inline (utf-8-encode-third-of-three-octets code-point)
  (unsafe.fxior #b10000000 (unsafe.fxand code-point #b111111)))

;;; --------------------------------------------------------------------
;;; encoding code points to 4-octet UTF-8

(define-inline (utf-8-four-octets-code-point? code-point)
  (and (unsafe.fx>  code-point #xFFFF)
       (unsafe.fx<= code-point #x10FFFF)))

(define-inline (utf-8-encode-first-of-four-octets code-point)
  (unsafe.fxior #b11110000 (unsafe.fxsra code-point 18)))

(define-inline (utf-8-encode-second-of-four-octets code-point)
  (unsafe.fxior #b10000000 (unsafe.fxand (unsafe.fxsra code-point 12) #b111111)))

(define-inline (utf-8-encode-third-of-four-octets code-point)
  (unsafe.fxior #b10000000 (unsafe.fxand (unsafe.fxsra code-point 6) #b111111)))

(define-inline (utf-8-encode-fourth-of-four-octets code-point)
  (unsafe.fxior #b10000000 (unsafe.fxand code-point #b111111)))

;;; --------------------------------------------------------------------

(define-inline (utf-8-classify-octet octet)
  (cond ((not (fixnum? octet))
	 (list 'invalid-value/not-a-fixnum octet))
	((< octet 0)
	 (list 'invalid-value/negative-fixnum octet))
	(else
	 (let ((str (number->string octet 16)))
	   (cond ((< #xFF octet)
		  (list 'invalid-value/fixnum-too-big str))
		 ((utf-8-invalid-octet? octet)
		  (list 'invalid-byte str))

		 ((utf-8-single-octet? octet)
		  (list 'one-byte-character str))

		 ((utf-8-first-of-two-octets? octet)
		  (list 'first-in-2-byte-char str))
		 ((utf-8-second-of-two-octets? octet)
		  (list 'second-in-2-byte-char str))

		 ((utf-8-first-of-three-octets? octet)
		  (list 'first-in-3-byte-char str))
		 ((utf-8-second-of-three-octets? octet)
		  (list 'second-in-3-byte-char str))
		 ((utf-8-third-of-three-octets? octet)
		  (list 'third-in-3-byte-char str))

		 ((utf-8-first-of-four-octets? octet)
		  (list 'first-in-4-byte-char str))
		 ((utf-8-second-of-four-octets? octet)
		  (list 'second-in-4-byte-char str))
		 ((utf-8-third-of-four-octets? octet)
		  (list 'third-in-4-byte-char str))
		 ((utf-8-fouth-of-four-octets? octet)
		  (list 'third-in-4-byte-char str))

		 (else
		  (list 'internal-error str)))))))

(define-inline (utf-8-classify-code-point code-point)
  (let ((ch (integer->char code-point)))
    (cond ((utf-8-single-octet-code-point? code-point)
	   (list 'single-octet-code-point ch code-point))
	  ((utf-8-two-octets-code-point? code-point)
	   (list 'two-octets-code-point ch code-point))
	  ((utf-8-three-octets-code-point? code-point)
	   (list 'three-octets-code-point ch code-point))
	  ((utf-8-four-octets-code-point? code-point)
	   (list 'four-octets-code-point ch code-point))
	  (else
	   (list 'internal-error ch code-point)))))


;;;; UTF-16 decoding
;;
;;Given a  16-bit word in  a UTF-16 stream,  represented in Scheme  as a
;;fixnum  in the  range  [#x0000, #xFFFF],  we  can classify  it on  the
;;following axis:
;;
;; 0000        D7FF D800    DBFF DC00      DFFF E000       FFFF
;;  |-------------||-----------||-------------||------------|
;;   single word    first in     second in      single word
;;   character      pair         pair           character
;;
;;or the following logic:
;;
;;   word in [#x0000, #xD7FF] => single word character
;;   word in [#xD800, #xDBFF] => first in surrogate pair
;;   word in [#xDC00, #xDFFF] => second in surrogate pair
;;   word in [#xE000, #xFFFF] => single word character

;;The   following  macros   assume  the   WORD  arguments   are  fixnums
;;representing  16-bit words,  that is  they must  be in  the  range [0,
;;#xFFFF].
;;

;;; 1-word encoding

(define-inline (utf-16-single-word? word0)
  ;;Evaluate  to true if  WORD0 is  valid as  single 16-bit  word UTF-16
  ;;encoding of a Unicode character.
  ;;
  (or (unsafe.fx< word0 #xD800) (unsafe.fx< #xDFFF word0)))

(define-inline (utf-16-decode-single-word word0)
  ;;Decode  the integer  representation of  a Unicode  character  from a
  ;;16-bit single word UTF-16 encoding.
  ;;
  word0)

;;; --------------------------------------------------------------------
;;; 2-words encoding

(define-inline (utf-16-first-of-two-words? word0)
  ;;Evaluate  to true  if  WORD0 is  valid  as first  16-bit  word in  a
  ;;surrogate pair UTF-16 encoding of a Unicode character.
  ;;
  (and (unsafe.fx<= #xD800 word0) (unsafe.fx<= word0 #xDBFF)))

(define-inline (utf-16-second-of-two-words? word1)
  ;;Evaluate  to true  if WORD1  is  valid as  second 16-bit  word in  a
  ;;surrogate pair UTF-16 encoding of a Unicode character.
  ;;
  (and (unsafe.fx<= #xDC00 word1) (unsafe.fx<= word1 #xDFFF)))

(define-inline (utf-16-decode-surrogate-pair word0 word1)
  ;;Decode  the integer  representation of  a Unicode  character  from a
  ;;surrogate pair UTF-16 encoding.
  ;;
  (unsafe.fx+ #x10000
	      (unsafe.fxior (unsafe.fxsll (unsafe.fxand word0 #x3FF) 10)
			     (unsafe.fxand word1 #x3FF))))

;;; --------------------------------------------------------------------
;;; 1-word encoding

(define-inline (utf-16-single-word-code-point? code-point)
  (and (unsafe.fx>= code-point 0)
       (unsafe.fx<  code-point #x10000)))

(define-inline (utf-16-encode-single-word code-point)
  code-point)

;;; --------------------------------------------------------------------
;;; 2-word encoding

(define-inline (utf-16-two-words-code-point? code-point)
  (and (unsafe.fx>= code-point #x10000)
       (unsafe.fx<  code-point #x10FFFF)))

(define-inline (utf-16-encode-first-of-two-words code-point)
  (unsafe.fxior #xD800 (unsafe.fxsra (unsafe.fx- code-point #x10000) 10)))

(define-inline (utf-16-encode-second-of-two-words code-point)
  (unsafe.fxior #xDC00 (unsafe.fxand (unsafe.fx- code-point #x10000) (- (unsafe.fxsll 1 10) 1))))

;;; --------------------------------------------------------------------

(define-inline (utf-16-classify-word word)
  (cond ((not (fixnum? word))
	 (list 'invalid-value/not-a-fixnum word))
	((< word 0)
	 (list 'invalid-value/negative-fixnum word))
	(else
	 (let ((str (number->string word 16)))
	   (cond ((< #xFFFF word)
		  (list 'invalid-value/fixnum-too-big str))
		 ((utf-16-single-word? word)
		  (list 'one-word-character str))
		 ((utf-16-first-of-two-words? word)
		  (list 'first-word-in-surrogate-pair str))
		 ((utf-16-second-of-two-words? word)
		  (list 'second-word-in-surrogate-pair str))
		 (else
		  (list 'internal-error str)))))))


;;;; ISO/IEC 8859-1 also known as Latin-1
;;
;;Latin-1 uses 1 octet per character.  The first 256 Unicode code points
;;are identical  to the content of  Latin-1, the first  127 Latin-1 code
;;points are identical to ASCII.  For an itroduction see:
;;
;;  <http://en.wikipedia.org/wiki/ISO/IEC_8859-1>
;;
;;Latin-1 code points are identical to their octet encoding.
;;
;;Latin-1 code  points in the range  [0, 127] are identical  to the same
;;code points encoded in both ASCII and in UTF-8.
;;
;;Latin-1 code points  in the range [128, 255]  are *different* from the
;;same code points encoded in UTF-8.
;;
;;Every  octet (that  is: every  fixnum in  the range  [0, 255])  can be
;;interpreted as a character in Latin-1 encoding.
;;

;;In the  following macros the  argument OCTET is  meant to be  a fixnum
;;representing a octet, while the argument CODE-POINT is meant to be the
;;integer representation of a character.

(define-inline (latin-1-octet? octet)
  #t)

(define-inline (latin-1-decode octet)
  octet)

(define-inline (latin-1-code-point? code-point)
  (and (unsafe.fx<= 0 code-point) (unsafe.fx<= code-point 255)))

(define-inline (latin-1-encode code-point)
  code-point)


;;;; done

)

;;; end of file
