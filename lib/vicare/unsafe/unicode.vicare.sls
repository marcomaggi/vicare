;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: Unicode characters handling
;;;Date: Fri Nov  4, 2011
;;;
;;;Abstract
;;;
;;;	This library exports only  syntaxes, so it can be used  also in the internals
;;;	of Vicare's source code.
;;;
;;;Copyright (C) 2011, 2013, 2015, 2016, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; Introduction to Unicode and UCS
;;
;;As  required by  R6RS, the  input/output libraries  must implement  transcoders for
;;textual ports supporting encoding and  decoding between Scheme characters and UTF-8
;;and UTF-16.
;;
;;The mandatory  starting points to  learn about this  stuff are the  following (URLs
;;last verified on Sep 9, 2011):
;;
;;  <http://www.unicode.org/faq/utf_bom.html>
;;  <http://en.wikipedia.org/wiki/Universal_Character_Set>
;;  <http://en.wikipedia.org/wiki/Unicode>
;;  <http://en.wikipedia.org/wiki/Byte_order_mark>
;;  <http://en.wikipedia.org/wiki/UTF-8>
;;  <http://en.wikipedia.org/wiki/UTF-16>
;;  <http://en.wikipedia.org/wiki/UTF-32>
;;
;;here we give only a brief overview of the main definitions, drawing text from those
;;pages.
;;
;;The "Universal Character Set" (UCS) is a standard set of characters upon which many
;;character encodings are based; it  contains abstract characters, each identified by
;;an unambiguous name and an integer number called its "code point".
;;
;;"Unicode"  is   a  computing  industry   standard  for  the   consistent  encoding,
;;representation  and handling  of  text expressed  in most  of  the world's  writing
;;systems.
;;
;;UCS and Unicode have an identical  repertoire and numbers: the same characters with
;;the same numbers exist  in both standards.  UCS is a  simple character map, Unicode
;;adds rules for  collation, normalization of forms, and  the bidirectional algorithm
;;for scripts.
;;
;;The  Unicode  Consortium, the  nonprofit  organization  that coordinates  Unicode's
;;development,  has the  goal  of eventually  replacing  existing character  encoding
;;schemes with  Unicode and its  standard "Unicode Transformation Format"  alias "UCS
;;Transformation Format" (UTF) schemes.

;;By convention a Unicode  code point is referred to by writing  "U+" followed by its
;;hexadecimal number with at least 4 digits (U+0044 is fine, U+12 is not).
;;
;;In practice, Unicode code points are exact integers in the range [0, #x10FFFF], but
;;outside the  range [#xD800, #xDFFF]  which has special  meaning in UTF  schemes.  A
;;code point can be stored in 21 bits:
;;
;;  (string-length (number->string #x10FFFF 2)) => 21
;;
;;R6RS defines fixnums to have at least 24 bits, so a fixnum is wide enough to hold a
;;code point:
;;
;;  (fixnum? #x10FFFF) => #t
;;
;;and indeed Scheme characters are a disjoint type of value holding such fixnums:
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
    utf-8-case-code-point

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

;;; UTF-32
    utf-32-code-point?
    utf-32-encode

    utf-32-word?
    utf-32-decode

;;; Latin-1
    unicode-code-point-representable-as-latin-1-code-point?
    latin-1-encode
    latin-1-code-point?
    latin-1-decode
    latin-1-C0-control?
    latin-1-C1-control?
    latin-1-control?
    latin-1-graphic?

;;; ASCII
    ascii-code-point?
    ascii-encode
    ascii-octet?
    ascii-decode)
  (import (rnrs)
    (only (vicare)
	  define-inline
	  define-syntax-rule)
    (vicare system $fx))


;;;; helpers

(define-syntax $fx-logand
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     ($fxand ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     ($fxand ?op1 ($fx-logand ?op2 . ?ops)))))

(define-syntax $fx-logor
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     ($fxior ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     ($fxior ?op1 ($fx-logor ?op2 . ?ops)))))


;;;; UTF-8 scheme
;;
;;UTF-8 is  a multioctet  character encoding  for Unicode  which can  represent every
;;character in the Unicode  set: it can represent every code point  in the ranges [0,
;;#xD800) and (#xDFFF, #x10FFFF].
;;
;;A stream of UTF-8 encoded characters is meant  to be stored octet by octet in fixed
;;order (and so without the need to specify the endianness of words).
;;
;;The encoding scheme uses sequences of 1, 2, 3 or 4 octets to encode each code point
;;as shown  in the following table;  the first octet in  a sequence has a  unique bit
;;pattern in  the most  significant bits and  so it allows  the determination  of the
;;sequence  length; every  octet contains  a  number of  payload bits  which must  be
;;concatenated (bitwise inclusive OR) to  reconstruct the integer representation of a
;;code point:
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
;;Note that octets  #xFE (#b11111110) and #xFF (#b11111111) cannot  appear in a valid
;;stream of UTF-8 encoded characters.
;;
;;The sequence of 3 octets is the one  that could encode (but must not) the forbidden
;;range [#xD800, #xDFFF]  which are not Unicode  code points.  So the  table of valid
;;encoded code points is:
;;
;; | # of octets |  # of payload bits |    code point range  |
;; |-------------+--------------------+----------------------|
;; |     1       |                  7 |   [#x0000,   #x007F] |
;; |     2       |        5 + 6 = 11  |   [#x0080,   #x07FF] |
;; |     3       |     4 + 6 + 6 = 16 |   [#x0800,   #xD7FF] |
;; |     3       |     4 + 6 + 6 = 16 |   [#xE000,   #xFFFF] |
;; |     4       | 3 + 6 + 6 + 6 = 21 | [#x010000, #x10FFFF] |
;;
;;The first  128 characters of the  Unicode character set correspond  one-to-one with
;;ASCII  and are  encoded using  a single  octet with  the same  binary value  as the
;;corresponding ASCII character, making valid  ASCII text valid UTF-8 encoded Unicode
;;text as well.  Such encoded octets have the Most Significant Bit (MSB) set to zero.
;;
;;Although the standard does not define it, many programs start a UTF-8 stream with a
;;Byte Order Mark (BOM) composed of the 3 octets: #xEF, #xBB, #xBF.
;;

;;The  following  macro definitions  assume  that  the  OCTET arguments  are  fixnums
;;representing  1 octet  (they  are in  the  range [0,  255]),  while the  CODE-POINT
;;arguments are fixnums  representing Unicode code points (they are  in the range [0,
;;#x10FFFF], but outside the range [#xD800, #xDFFF]).
;;

(define-inline (utf-8-invalid-octet? octet)
  ;;Evaluate to  true if OCTET has  a value that must  never appear in a  valid UTF-8
  ;;stream.
  ;;
  (or ($fx= octet #xC0)
      ($fx= octet #xC1)
      (and ($fx<= #xF5 octet) ($fx<= octet #xFF))))

;;; -------------------------------------------------------------
;;; decoding 1-octet UTF-8 to code points

(define-inline (utf-8-single-octet? octet)
  ;;Evaluate  to true  if OCTET  is  valid as  1-octet  UTF-8 encoding  of a  Unicode
  ;;character.
  ;;
  ($fx< octet 128))

(define-inline (utf-8-decode-single-octet octet)
  ;;Decode the code point of a Unicode character from a 1-octet UTF-8 encoding.
  ;;
  octet)

(define-inline (utf-8-valid-code-point-from-1-octet? code-point)
  ;;Evaluate to true if CODE-POINT is a valid integer representation for a code point
  ;;decoded from a 2-octets UTF-8 sequence.
  ;;
  (and ($fx<= 0 code-point) ($fx<= code-point #x007F)))

;;; -------------------------------------------------------------
;;; decoding 2-octets UTF-8 to code points

(define-inline (utf-8-first-of-two-octets? octet0)
  ;;Evaluate to  true if OCTET0  is valid  as first of  2-octets UTF-8 encoding  of a
  ;;Unicode character.
  ;;
  ($fx= ($fxsra octet0 5) #b110))

(define-inline (utf-8-second-of-two-octets? octet1)
  ;;Evaluate to  true if OCTET1 is  valid as second  of 2-octets UTF-8 encoding  of a
  ;;Unicode character.
  ;;
  ($fx= ($fxsra octet1 6) #b10))

(define-inline (utf-8-decode-two-octets octet0 octet1)
  ;;Decode the code point of a Unicode character from a 2-octets UTF-8 encoding.
  ;;
  ($fx-logor ($fxsll ($fx-logand octet0 #b11111) 6)
	  ($fx-logand octet1 #b111111)))

(define-inline (utf-8-valid-code-point-from-2-octets? code-point)
  ;;Evaluate to true if CODE-POINT is a valid integer representation for a code point
  ;;decoded from a 2-octets UTF-8 sequence.
  ;;
  (and ($fx<= #x0080 code-point) ($fx<= code-point #x07FF)))

;;; -------------------------------------------------------------
;;; decoding 3-octets UTF-8 to code points

(define-inline (utf-8-first-of-three-octets? octet0)
  ;;Evaluate to  true if OCTET0  is valid  as first of  3-octets UTF-8 encoding  of a
  ;;Unicode character.
  ;;
  ($fx= ($fxsra octet0 4) #b1110))

(define-inline (utf-8-second-and-third-of-three-octets? octet1 octet2)
  ;;Evaluate to true if  OCTET1 and OCTET2 are valid as second  and third of 3-octets
  ;;UTF-8 encoding of a Unicode character.
  ;;
  ($fx= ($fxsra ($fx-logor octet1 octet2) 6) #b10))

(define-inline (utf-8-decode-three-octets octet0 octet1 octet2)
  ;;Decode the code point of a Unicode character from a 3-octets UTF-8 encoding.
  ;;
  ($fx-logor ($fxsll ($fx-logand octet0   #b1111) 12)
	  ($fxsll ($fx-logand octet1 #b111111)  6)
	  ($fx-logand octet2 #b111111)))

(define-inline (utf-8-valid-code-point-from-3-octets? code-point)
  ;;Evaluate to true if CODE-POINT is a valid integer representation for a code point
  ;;decoded from a 3-octets UTF-8 sequence.
  ;;
  (and ($fx<= #x0800 code-point)
       ($fx<= code-point #xFFFF)
       ;;This is the forbidden range.
       (or ($fx< code-point #xD800)
	   ($fx> code-point #xDFFF))))

;;; -------------------------------------------------------------
;;; decoding 4-octets UTF-8 to code points

(define-inline (utf-8-first-of-four-octets? octet0)
  ;;Evaluate to  true if OCTET0  is valid  as first of  4-octets UTF-8 encoding  of a
  ;;Unicode character.
  ;;
  ($fx= ($fxsra octet0 3) #b11110))

(define-inline (utf-8-second-third-and-fourth-of-four-octets? octet1 octet2 octet3)
  ;;Evaluate to  true if  OCTET1, OCTET2 and  OCTET3 are valid  as second,  third and
  ;;fourth of 4-octets UTF-8 encoding of a Unicode character.
  ;;
  ($fx= ($fxsra ($fx-logor octet1 octet2 octet3) 6) #b10))

(define-inline (utf-8-decode-four-octets octet0 octet1 octet2 octet3)
  ;;Decode the code point of a Unicode character from a 4-octets UTF-8 encoding.
  ;;
  ($fx-logor ($fxsll ($fx-logand octet0    #b111) 18)
	  ($fxsll ($fx-logand octet1 #b111111) 12)
	  ($fxsll ($fx-logand octet2 #b111111)  6)
	  ($fx-logand octet3 #b111111)))

(define-inline (utf-8-valid-code-point-from-4-octets? code-point)
  ;;Evaluate to true if CODE-POINT is a valid integer representation for a code point
  ;;decoded from a 4-octets UTF-8 sequence.
  ;;
  (and ($fx<= #x010000 code-point) ($fx<= code-point #x10FFFF)))

;;; -------------------------------------------------------------
;;; encoding code points to 1-octet UTF-8

(define-inline (utf-8-single-octet-code-point? code-point)
  (and ($fx<= 0 code-point) ($fx<= code-point 127)))

(define-inline (utf-8-encode-single-octet code-point)
  ;;Encode the code point of a Unicode character to a 1-octet UTF-8 encoding.
  ;;
  code-point)

;;; --------------------------------------------------------------------
;;; encoding code points to 2-octet UTF-8

(define-inline (utf-8-two-octets-code-point? code-point)
  (and ($fx>  code-point 127)
       ($fx<= code-point #x7FF)))

(define-inline (utf-8-encode-first-of-two-octets code-point)
  ($fx-logor #b11000000 ($fxsra code-point 6)))

(define-inline (utf-8-encode-second-of-two-octets code-point)
  ($fx-logor #b10000000 ($fx-logand code-point #b111111)))

;;; --------------------------------------------------------------------
;;; encoding code points to 3-octet UTF-8

(define-inline (utf-8-three-octets-code-point? code-point)
  (and ($fx>  code-point #x7FF)
       ($fx<= code-point #xFFFF)))

(define-inline (utf-8-encode-first-of-three-octets code-point)
  ($fx-logor #b11100000 ($fxsra code-point 12)))

(define-inline (utf-8-encode-second-of-three-octets code-point)
  ($fx-logor #b10000000 ($fx-logand ($fxsra code-point 6) #b111111)))

(define-inline (utf-8-encode-third-of-three-octets code-point)
  ($fx-logor #b10000000 ($fx-logand code-point #b111111)))

;;; --------------------------------------------------------------------
;;; encoding code points to 4-octet UTF-8

(define-inline (utf-8-four-octets-code-point? code-point)
  (and ($fx>  code-point #xFFFF)
       ($fx<= code-point #x10FFFF)))

(define-inline (utf-8-encode-first-of-four-octets code-point)
  ($fx-logor #b11110000 ($fxsra code-point 18)))

(define-inline (utf-8-encode-second-of-four-octets code-point)
  ($fx-logor #b10000000 ($fx-logand ($fxsra code-point 12) #b111111)))

(define-inline (utf-8-encode-third-of-four-octets code-point)
  ($fx-logor #b10000000 ($fx-logand ($fxsra code-point 6) #b111111)))

(define-inline (utf-8-encode-fourth-of-four-octets code-point)
  ($fx-logor #b10000000 ($fx-logand code-point #b111111)))

;;; --------------------------------------------------------------------

(define-inline (utf-8-classify-octet octet)
  (cond ((not (fixnum? octet))				'invalid-value/not-a-fixnum)
	((< octet 0)					'invalid-value/negative-fixnum)
	(else
	 (cond ((< #xFF octet)				'invalid-value/fixnum-too-big)
	       ((utf-8-invalid-octet? octet)		'invalid-byte)

	       ((utf-8-single-octet? octet)		'one-byte-character)

	       ((utf-8-first-of-two-octets?  octet)	'first-in-2-byte-char)
	       ((utf-8-second-of-two-octets? octet)	'second-in-2-byte-char)

	       ((utf-8-first-of-three-octets?  octet)	'first-in-3-byte-char)
	       ((utf-8-second-of-three-octets? octet)	'second-in-3-byte-char)
	       ((utf-8-third-of-three-octets?  octet)	'third-in-3-byte-char)

	       ((utf-8-first-of-four-octets?  octet)	'first-in-4-byte-char)
	       ((utf-8-second-of-four-octets? octet)	'second-in-4-byte-char)
	       ((utf-8-third-of-four-octets?  octet)	'third-in-4-byte-char)
	       ((utf-8-fouth-of-four-octets?  octet)	'third-in-4-byte-char)

	       (else					'internal-error)))))

(define-inline (utf-8-classify-code-point code-point)
  (let ((ch (integer->char code-point)))
    (cond ((utf-8-single-octet-code-point? code-point)		'single-octet-code-point)
	  ((utf-8-two-octets-code-point?   code-point)		'two-octets-code-point)
	  ((utf-8-three-octets-code-point? code-point)		'three-octets-code-point)
	  ((utf-8-four-octets-code-point?  code-point)		'four-octets-code-point)
	  (else							'internal-error))))

(define-syntax utf-8-case-code-point
  (syntax-rules ()
    ((_ ?code-point
	((1) . ?one-octet-encoding)
	((2) . ?two-octets-encoding)
	((3) . ?three-octets-encoding)
	((4) . ?four-octets-encoding))
     (let ((code-point ?code-point))
       (cond (($fx<= code-point #x7F)    . ?one-octet-encoding)
	     (($fx<= code-point #x7FF)   . ?two-octets-encoding)
	     (($fx<= code-point #xFFFF)  . ?three-octets-encoding)
	     (else                       . ?four-octets-encoding))))
    ))


;;;; UTF-16 decoding
;;
;;Code points in the  range [0, #x10000) are encoded with a  single UTF-16 word; code
;;points in the range [#x10000, #x10FFFF] are encoded in a surrogate pair.
;;
;;Given a 16-bit  word in a UTF-16 stream,  represented in Scheme as a  fixnum in the
;;range [#x0000, #xFFFF], we can classify it on the following axis:
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
;;
;;A UTF-16 stream may start with a Byte Order Mark (BOM).  A UTF-16 BOM is either:
;;
;;* A sequence of bytes #xFE, #xFF specifying "big" and UTF-16BE encoding.
;;
;;* A sequence of bytes #xFF, #xFE specifying "little" and UTF-16LE encoding.
;;

;;The  following macros  assume the  WORD arguments  are fixnums  representing 16-bit
;;words: they must be  in the range [0, #xFFFF].  While  the CODE-POINT arguments are
;;fixnums representing Unicode code points (they  are in the range [0, #x10FFFF], but
;;outside the range [#xD800, #xDFFF])
;;

;;; 1-word decoding

(define-inline (utf-16-single-word? word0)
  ;;Evaluate to  true if WORD0 is  valid as single  16-bit word UTF-16 encoding  of a
  ;;Unicode character.
  ;;
  (or ($fx< word0 #xD800) ($fx< #xDFFF word0)))

(define-inline (utf-16-decode-single-word word0)
  ;;Decode the  integer representation of  a Unicode  character from a  16-bit single
  ;;word UTF-16 encoding.
  ;;
  word0)

;;; --------------------------------------------------------------------
;;; 2-words decoding

(define-inline (utf-16-first-of-two-words? word0)
  ;;Evaluate to  true if  WORD0 is  valid as first  16-bit word  in a  surrogate pair
  ;;UTF-16 encoding of a Unicode character.
  ;;
  (and ($fx<= #xD800 word0) ($fx<= word0 #xDBFF)))

(define-inline (utf-16-second-of-two-words? word1)
  ;;Evaluate to  true if WORD1  is valid  as second 16-bit  word in a  surrogate pair
  ;;UTF-16 encoding of a Unicode character.
  ;;
  (and ($fx<= #xDC00 word1) ($fx<= word1 #xDFFF)))

(define-inline (utf-16-decode-surrogate-pair word0 word1)
  ;;Decode the  integer representation of a  Unicode character from a  surrogate pair
  ;;UTF-16 encoding.
  ;;
  ($fx+ #x10000
	($fx-logor ($fxsll ($fx-logand word0 #x3FF) 10)
		($fx-logand word1 #x3FF))))

;;; --------------------------------------------------------------------
;;; 1-word encoding

(define-inline (utf-16-single-word-code-point? code-point)
  ($fx< code-point #x10000))
;; (define-inline (utf-16-single-word-code-point? code-point)
;;   (and ($fx>= code-point 0)
;;        ($fx<  code-point #x10000)))

(define-inline (utf-16-encode-single-word code-point)
  code-point)

;;; --------------------------------------------------------------------
;;; 2-word encoding

(define-inline (utf-16-two-words-code-point? code-point)
  (and ($fx>= code-point #x10000)
       ($fx<= code-point #x10FFFF)))

(define-inline (utf-16-encode-first-of-two-words code-point)
  ($fx-logor #xD800 ($fxsra ($fx- code-point #x10000) 10)))

(define-inline (utf-16-encode-second-of-two-words code-point)
  ($fx-logor #xDC00 ($fx-logand ($fx- code-point #x10000) (- ($fxsll 1 10) 1))))

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


;;;; UTF-32 decoding
;;
;;UTF-32, also called UCS 4, is a multioctet character encoding for Unicode which can
;;represent every character in the Unicode set:  it can represent every code point in
;;the ranges [0, #xD800) and (#xDFFF, #x10FFFF].  It uses exactly 32 bits per Unicode
;;code point.
;;
;;This  makes UTF-32  a  fixed-length  encoding, in  contrast  to  all other  Unicode
;;transformation formats which  are variable-length encodings.  The UTF-32  form of a
;;character is a direct representation of its code point.
;;
;;

;;The  following macros  assume the  WORD arguments  are fixnums  representing 32-bit
;;words: they must  be in the range [0, #xFFFFFFFF].   While the CODE-POINT arguments
;;are fixnums representing Unicode code points  (they are in the range [0, #x10FFFF],
;;but outside the range [#xD800, #xDFFF])
;;

;;; encoding

(define-syntax-rule (utf-32-code-point? code-point)
  #t)

(define-syntax-rule (utf-32-encode code-point)
  code-point)

;;; decoding

(define-inline (utf-32-word? word)
  ;;Evaluate to  true if WORD  is valid  as single 32-bit  word UTF-32 encoding  of a
  ;;Unicode character.
  ;;
  (and ($fx>= word 0)
       ($fx<= word #x10FFFF)
       (or ($fx< word #xD800)
	   ($fx> word #xDFFF))))

(define-syntax-rule (utf-32-decode word)
  word)


;;;; ISO/IEC 8859-1 also known as Latin-1
;;
;;Latin-1 encoding uses 1 octet per character.  For an itroduction to Latin-1 see:
;;
;;  <http://en.wikipedia.org/wiki/ISO/IEC_8859-1>
;;
;;and for Unicode's "C1 Controls and Latin-1 Supplement" see:
;;
;;  <https://en.wikipedia.org/wiki/Latin-1_Supplement_%28Unicode_block%29>
;;  <http://www.unicode.org/charts/PDF/U0080.pdf>
;;
;;Strictly speaking,  the Latin-1  encoding only  defines code  points in  the ranges
;;[#x20, #x7E] and [#xA0, #xFF]; notice that the control characters are excluded.
;;
;;In the range  [#x20, #x7E] the Latin-1  code points are equal  to the corresponding
;;ASCII code points.
;;
;;In both the ranges  [#x20, #x7E] and [#xA0, #xFF]: Latin-1's  code points are equal
;;to Unicode's  code points,  when we  take into account  Unicode's "C1  Controls and
;;Latin-1 Supplement".
;;
;;Notice that:
;;
;;* Unicode's "C0  Controls and Basic Latin"  specifies the code points  in the range
;;  [#x00, #x7F]  so that they  are equal to ASCII's  control characters in  the same
;;  range.  This Unicode block includes the  range of control characters [#x00, #x1F]
;;  which is left undefined by Latin-1.
;;
;;* Unicode's "C1 Controls and Latin-1 Supplement" specifies code points in the range
;;  [#x80, #x9F].  This range is left undefined by Latin-1.
;;
;;This library defines an *extended* Latin-1 encoding spanning the whole [#x00, #xFF]
;;range with the following blocks:
;;
;;   [#x00, #x1F]	C0 Controls
;;   [#x20, #x7E]	Latin-1 code points
;;   #x7F               C0 Controls
;;   [#x80, #x9F]	C1 Controls
;;   [#xA0, #xFF]	Latin-1 code points
;;

;;In the  following macros the  argument LATIN-1-CODE-POINT is  meant to be  a fixnum
;;representing a Latin-1  code point, while the argument  UNICODE-CODE-POINT is meant
;;to be a fixnum representing a Unicode code point.

;;; --------------------------------------------------------------------
;;; encoding Unicode code points as Latin-1 code points

(define-inline (unicode-code-point-representable-as-latin-1-code-point? unicode-code-point)
  ;;Return true if UNICODE-CODE-POINT is a Unicode  code point in a range that can be
  ;;encoded in Latin-1.
  ;;
  (and ($fx>= unicode-code-point #x00)
       ($fx<= unicode-code-point #xFF)))

(define-inline (latin-1-encode unicode-code-point)
  ;;Encode a Unicode code point into a Latin-1 code point.
  ;;
  unicode-code-point)

;;; --------------------------------------------------------------------
;;; decoding Unicode code points from Latin-1 code points

(define-inline (latin-1-code-point? octet)
  ;;Assume OCTET is the fixnum representation of an octet.  Evaluate to true if OCTET
  ;;a valid Latin-1 code point; otherwise evaluate to false.
  ;;
  (and ($fx>= octet #x00)
       ($fx<= octet #xFF)))

(define-inline (latin-1-decode latin-1-code-point)
  ;;Decode a Latin-1 code point to a Unicode code point.
  ;;
  latin-1-code-point)

;;; --------------------------------------------------------------------
;;; classification

(define-inline (latin-1-C0-control? latin-1-code-point)
  (or (and ($fx>= latin-1-code-point #x00)
	   ($fx<= latin-1-code-point #x1F))
      ($fx= latin-1-code-point #x7F)))

(define-inline (latin-1-C1-control? latin-1-code-point)
  (and ($fx>= latin-1-code-point #x80)
       ($fx<= latin-1-code-point #x9F)))

(define-inline (latin-1-control? latin-1-code-point)
  (or (latin-1-C0-control? latin-1-code-point)
      (latin-1-C1-control? latin-1-code-point)))

(define-inline (latin-1-graphic? latin-1-code-point)
  (or (and ($fx>= latin-1-code-point #x20)
	   ($fx<= latin-1-code-point #x7E))
      (and ($fx>= latin-1-code-point #xA0)
	   ($fx<= latin-1-code-point #xFF))))


;;;; ASCII

(define-inline (ascii-code-point? code-point)
  ;;Assume CODE-POINT is  the fixnum representation of a Unicode  code point.  Return
  ;;true if CODE-POINT is in the range accepted by the ASCII encoding.
  ;;
  (and ($fx>= code-point #x00)
       ($fx<= code-point #x7F)))

(define-inline (ascii-encode code-point)
  ;;Assume CODE-POINT  is the fixnum  representation of a  Unicode code point  in the
  ;;range representable with ASCII encoding.  Return its encoding in ASCII format.
  code-point)

;;; --------------------------------------------------------------------

(define-inline (ascii-octet? octet)
  ;;Assume OCTET is a  fixnum representing an octet.  Return true if  OCTET is in the
  ;;range accepted by the ASCII encoding; otherwise return false.
  ;;
  (and ($fx>= octet #x00)
       ($fx<= octet #x7F)))

(define-inline (ascii-decode octet)
  ;;Assume OCTET is the fixnum representation  of an ASCII encoded character.  Return
  ;;its associated Unicode code point.
  octet)


;;;; done

#| end of library |# )

;;; end of file
