;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for bytevector's conversion functions
;;;Date: Tue Jun  2, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (prefix (vicare platform words) words.)
  #;(vicare system $bytevectors)
  (prefix (vicare unsafe unicode) unicode.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare bytevector conversion functions\n")

(printer-integer-radix 16)


;;;; helpers

(define* (code-point->utf16/2-words {code-point unicode.utf-16-two-words-code-point?})
  (values (unicode.utf-16-encode-first-of-two-words  code-point)
	  (unicode.utf-16-encode-second-of-two-words code-point)))

(define* (code-point->utf16/1-word {code-point unicode.utf-16-single-word-code-point?})
  (unicode.utf-16-encode-single-word code-point))

(define* (utf16-string-mirror/little {str string?})
  (utf16->string (string->utf16 str (endianness little)) (endianness little) #t))

(define* (utf16-string-mirror/big {str string?})
  (utf16->string (string->utf16 str (endianness big))    (endianness big) #t))



(parametrise ((check-test-name	'string-utf8))

  (check (utf8->string-length '#vu8())				=> 0)
  (check (utf8->string-length '#vu8(1 2 3))			=> 3)

  ;;BOM!!!
  (check (utf8->string-length '#vu8(#xEF #xBB #xBF))		=> 0)
  (check (utf8->string-length '#vu8(#xEF #xBB #xBF 1 2 3))	=> 3)

  ;;Invalid  sequence starting  with  valid  first byte  in  3-octet sequence.   When
  ;;ignoring or replacing: we process the whole triplet.
  (begin
    (check (utf8->string-length '#vu8(#xe0 #x67 #x0a) 'ignore)	=> 0)
    (check (utf8->string-length '#vu8(#xe0 #x67 #x0a) 'replace)	=> 1)
    (check
	(guard (E ((error? E)
		   #t)
		  (else E))
	  (utf8->string-length '#vu8(#xe0 #x67 #x0a) 'raise))
      => #t))

;;; --------------------------------------------------------------------

  (check (utf8->string '#vu8())				=> "")
  (check (utf8->string '#vu8(1 2 3))			=> "\x01;\x02;\x03;")

  ;;BOM!!!
  (check (utf8->string '#vu8(#xEF #xBB #xBF))		=> "")
  (check (utf8->string '#vu8(#xEF #xBB #xBF 1 2 3))	=> "\x01;\x02;\x03;")

  ;;Invalid  sequence starting  with  valid  first byte  in  3-octet sequence.   When
  ;;ignoring or replacing: we process the whole triplet.
  (begin
    (check (utf8->string '#vu8(#xe0 #x67 #x0a) 'ignore)		=> "")
    (check (utf8->string '#vu8(#xe0 #x67 #x0a) 'replace)	=> "\xFFFD;")
    (check
	(guard (E ((error? E)
		   #t)
		  (else E))
	  (utf8->string '#vu8(#xe0 #x67 #x0a) 'raise))
      => #t))

  #t)


(parametrise ((check-test-name	'string-utf16))

;;; tests for use in the documentation

  (check (utf16->string '#vu8(#xAA #xBB) (endianness big))	=> "\xAABB;")
  (check (utf16->string '#vu8(#xAA #xBB) (endianness little))	=> "\xBBAA;")

;;; --------------------------------------------------------------------

;;Code points in the  range [0, #x10000) are encoded with a  single UTF-16 word; code
;;points in the range [#x10000, #x10FFFF] are encoded in a surrogate pair.

  (check (string->utf16 "\x00;\x01;\x02;\x03;" (endianness big))	=> '#vu8(#x00 #x00   #x00 #x01   #x00 #x02   #x00 #x03))
  (check (string->utf16 "\x00;\x01;\x02;\x03;" (endianness little))	=> '#vu8(#x00 #x00   #x01 #x00   #x02 #x00   #x03 #x00))

  ;; code point that is encoded with 1 16-bit word, and it is equal to the BOM
  (begin
    (check (code-point->utf16/1-word #xFFFE)			=> #xFFFE)
    (check (string->utf16 "\xFFFE;" (endianness big))		=> '#vu8(#xFF #xFE))
    (check (string->utf16 "\xFFFE;" (endianness little))	=> '#vu8(#xFE #xFF))
    (check (utf16->string '#vu8(#xFF #xFE) (endianness big)    #t)	=> "\xFFFE;")
    (check (utf16->string '#vu8(#xFE #xFF) (endianness little) #t)	=> "\xFFFE;")
    (check (utf16-string-mirror/little "\xFFFE;")		=> "\xFFFE;")
    (check (utf16-string-mirror/big    "\xFFFE;")		=> "\xFFFE;"))

  ;;BOM processing:
  ;;
  ;;* #xFEFF is the BOM for big endian.
  ;;
  ;;* #xFFFE is the BOM for little endian.
  ;;
  ;;In all these tests: the endianness argument  is ignored; the BOM is processed; an
  ;;empty string is generated.
  (begin
    ;;Big endian BOM.
    (check (utf16->string '#vu8(#xFE #xFF) (endianness big)    #f)	=> "")
    (check (utf16->string '#vu8(#xFE #xFF) (endianness little) #f)	=> "")
    ;;Little endian BOM.
    (check (utf16->string '#vu8(#xFF #xFE) (endianness big)    #f)	=> "")
    (check (utf16->string '#vu8(#xFF #xFE) (endianness little) #f)	=> ""))
  ;;In all these tests:  the endianness argument is ignored; the  BOM is processed; a
  ;;string of 1 character is generated.
  (begin
    ;;Big endian BOM.
    (check (utf16->string '#vu8(#xFE #xFF #xAA #xBB) (endianness big)    #f)	=> "\xAABB;")
    (check (utf16->string '#vu8(#xFE #xFF #xAA #xBB) (endianness little) #f)	=> "\xAABB;")
    ;;Little endian BOM.
    (check (utf16->string '#vu8(#xFF #xFE #xAA #xBB) (endianness big)    #f)	=> "\xBBAA;")
    (check (utf16->string '#vu8(#xFF #xFE #xAA #xBB) (endianness little) #f)	=> "\xBBAA;"))

  ;; highest code point that is encoded with 1 16-bit word
  (begin
    (check (code-point->utf16/1-word #xFFFF)			=> #xFFFF)
    (check (string->utf16 "\xFFFF;" (endianness big))		=> '#vu8(#xFF #xFF))
    (check (string->utf16 "\xFFFF;" (endianness little))	=> '#vu8(#xFF #xFF))
    (check (utf16-string-mirror/little "\xFFFF;")		=> "\xFFFF;")
    (check (utf16-string-mirror/big    "\xFFFF;")		=> "\xFFFF;"))

;;; --------------------------------------------------------------------

  ;; lowest code point that is encoded with 2 16-bit words
  (begin
    (check (code-point->utf16/2-words #x10000)			=> #xD800 #xDC00)
    (check (string->utf16 "\x10000;" (endianness big))		=> '#vu8(#xD8 #x00 #xDC #x00))
    (check (string->utf16 "\x10000;" (endianness little))	=> '#vu8(#x00 #xD8 #x00 #xDC))
    (check (utf16-string-mirror/little "\x10000;")		=> "\x10000;")
    (check (utf16-string-mirror/big    "\x10000;")		=> "\x10000;"))

  ;; generic code point that is encoded with 2 16-bit words
  (begin
    (check (code-point->utf16/2-words #x12345)			=> #xD808 #xDF45)
    (check (string->utf16 "\x12345;" (endianness big))		=> '#vu8(#xD8 #x08 #xDF #x45))
    (check (string->utf16 "\x12345;" (endianness little))	=> '#vu8(#x08 #xD8 #x45 #xDF))
    (check (utf16-string-mirror/little "\x12345;")		=> "\x12345;")
    (check (utf16-string-mirror/big    "\x12345;")		=> "\x12345;"))

  ;; highest code point that is encoded with 2 16-bit words
  (begin
    (check (code-point->utf16/2-words #x10FFFF)			=> #xDBFF #xDFFF)
    (check (string->utf16 "\x10FFFF;" (endianness big))		=> '#vu8(#xDB #xFF #xDF #xFF))
    (check (string->utf16 "\x10FFFF;" (endianness little))	=> '#vu8(#xFF #xDB #xFF #xDF)))

;;; --------------------------------------------------------------------



  #t)


;;;; done

(printer-integer-radix 10)
(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'with-check-for-procedure-argument-validation 'scheme-indent-function 1)
;;End:

