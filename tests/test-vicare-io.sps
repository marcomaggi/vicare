;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for port related functions
;;;Date: Thu Oct  6, 2011
;;;
;;;Abstract
;;;
;;;
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


#!vicare
(import (rename (ikarus)
		(parameterize	parametrise))
  (except (checks) with-result add-result)
  (prefix (only (checks)
		with-result add-result)
	  check.))

(print-unicode #f)
(check-set-mode! 'report-failed)
(display "*** testing Vicare input/output functions\n")


;;;; syntax helpers

(define-syntax unwind-protect
  (syntax-rules ()
    ((_ ?body ?cleanup0 ?cleanup ...)
     (let ((cleanup (lambda () ?cleanup0 ?cleanup ...)))
       (with-exception-handler
	   (lambda (E)
	     (cleanup)
	     (raise E))
	 (lambda ()
	   (call-with-values
	       (lambda () ?body)
	     (lambda return-values
	       (cleanup)
	       (apply values return-values)))))))))


;;;; generic helpers

(define (%bytevector-u8-compare A.bv B.bv)
  (let ((A.len (bytevector-length A.bv))
	(B.len (bytevector-length B.bv)))
    (if (not (= A.len B.len))
	(begin
	  (printf "different lengths: ~a, ~a\n" A.len B.len)
	  #f)
      (let loop ((i 0))
	(if (= i A.len)
	    #t
	  (let ((A.byte (bytevector-u8-ref A.bv i))
		(B.byte (bytevector-u8-ref B.bv i)))
	    (if (not (= A.byte B.byte))
		(begin
		  (printf "different byte at index ~a: ~a, ~a\n" i A.byte B.byte)
		  #f)
	      (loop (+ 1 i)))))))))

(define (%open-disposable-binary-output-port)
  (let-values (((port getter) (open-bytevector-output-port)))
    port))

(define (%open-disposable-textual-output-port)
  (let-values (((port getter) (open-string-output-port)))
    port))

(define (%open-disposable-binary-input-port)
  (open-bytevector-input-port '#vu8(1)))

(define (%open-disposable-textual-input-port)
  (open-string-input-port "1"))


;;;; file helpers

(define (make-test-pathname filename)
  ;;Build and  return a test file  string pathname to be  used to create
  ;;new test files.
  ;;
  (string-append (or (getenv "VICARE_BUILDDIR") ".") "/" filename))

;;The current test file string pathname.
;;
(define test-pathname
  (make-parameter "test-pathname.bin"
    (lambda (obj)
      (assert (string? obj))
      obj)))

;;Function used to fill the file referenced by (TEST-PATHNAME).
;;
(define test-pathname-data-func
  (make-parameter bindata-hundreds.bv
    (lambda (obj)
      (assert (procedure? obj))
      obj)))

(define (cleanup-test-pathname)
  ;;If the current test file exists: remove it.
  ;;
  (when (file-exists? (test-pathname))
    (delete-file (test-pathname))))

(define (create-binary-test-pathname)
  ;;Create a  new test  file using the  current (TEST-PATHNAME)  and the
  ;;current (TEST-PATHNAME-DATA-FUNC).
  ;;
  (cleanup-test-pathname)
  (let ((port (open-file-output-port (test-pathname) (file-options) (buffer-mode block) #f)))
    (put-bytevector port ((test-pathname-data-func)))
    (close-output-port port)))

(define (create-textual-test-pathname codec)
  ;;Create a  new test  file using the  current (TEST-PATHNAME)  and the
  ;;current (TEST-PATHNAME-DATA-FUNC).
  ;;
  (cleanup-test-pathname)
  (let ((port (open-file-output-port (test-pathname) (file-options) (buffer-mode block)
				     (make-transcoder codec))))
    (put-string port ((test-pathname-data-func)))
    (close-output-port port)))

(define open-test-pathname
  (case-lambda
   (()
    (open-test-pathname #f))
   ((maybe-transcoder)
    ;;Open  the  current test  file  referenced  by (TEST-PATHNAME)  and
    ;;return a binary or textual input port for it.
    ;;
    (open-file-input-port (test-pathname) (file-options)
			  (buffer-mode block) maybe-transcoder))))

(define (binary-read-test-pathname)
  (let ((port (open-test-pathname)))
    (unwind-protect
	(get-bytevector-all port)
      (close-input-port port))))

(define (textual-read-test-pathname transcoder)
  (let ((port (open-test-pathname transcoder)))
    (unwind-protect
	(get-string-all port)
      (close-input-port port))))

(define-syntax with-input-test-pathname
  (syntax-rules ()
    ((_ (?port) . ?body)
     (begin
       (create-binary-test-pathname)
       (let ((?port (open-test-pathname)))
	 (unwind-protect
	     (begin  . ?body)
	   (close-input-port ?port)
	   (cleanup-test-pathname)))))))


;;;; binary data

(define (bindata-empty.len)
  0)

(define (bindata-empty.bv)
  '#vu8())

(define (bindata-zero.bv)
  '#vu8(0))

(define (bindata-zero.len)
  1)

(define (bindata-ten.len)
  10)

(define (bindata-ten.bv)
  ;;Holds  the same  bytes  of the  first  subvector of  length 10  of
  ;;BINDATA-HUNDREDS.
  ;;
  '#vu8(0 1 2 3 4 5 6 7 8 9))

(define (bindata-bytes.len)
  256)

(define bindata-bytes.bv
  ;;Holds  the  same bytes  of  the first  subvector  of  length 256  of
  ;;BINDATA-HUNDREDS.
  ;;
  (let ((result #f))
    (define (%bytevector-u8-fill! bv)
      (do ((i 0 (+ 1 i)))
	  ((= i 256)
	   bv)
	(bytevector-u8-set! bv i i)))
    (lambda ()
      (or result
	  (begin
	    (set! result (%bytevector-u8-fill! (make-bytevector (bindata-bytes.len) 0)))
	    result)))))

(define (bindata-hundreds.len)
  (* 100 256))

(define bindata-hundreds.bv
  ;;A bytevector holding 100 sequences of bytes from 0 included to 255
  ;;included.
  ;;
  (let ((result #f))
    (define (%bytevector-u8-fill! bv)
      (do ((i 0 (+ 1 i)))
	  ((= i 100)
	   bv)
	(let ((base (* 256 i)))
	  (do ((j 0 (+ 1 j)))
	      ((= j 256))
	    (bytevector-u8-set! bv (+ base j) j)))))
    (lambda ()
      (or result
	  (begin
	    (set! result (%bytevector-u8-fill! (make-bytevector (bindata-hundreds.len) 0)))
	    result)))))


;;;; textual data

(define (textdata-empty.len)
  0)

(define (textdata-empty.str)
  '#vu8())

(define (textdata-zero.str)
  "0")

(define (textdata-zero.len)
  1)

(define (textdata-ten.len)
  10)

(define (textdata-ten.str)
  ;;Holds  the  same  bytes of  the  first  substring  of length  10  of
  ;;TEXTDATA-HUNDREDS.
  ;;
  "0123456789")

(define (textdata-bytes.len)
  256)

(define textdata-bytes.str
  ;;Holds  the  same bytes  of  the first  substring  of  length 256  of
  ;;TEXTDATA-HUNDREDS.
  ;;
  (let ((result #f))
    (define (%string-fill! str)
      (do ((i 0 (+ 1 i)))
	  ((= i 256)
	   str)
	(string-set! str i (integer->char i))))
    (lambda ()
      (or result
	  (begin
	    (set! result (%string-fill! (make-string (textdata-bytes.len) #\Z)))
	    result)))))

(define (textdata-hundreds.len)
  (* 100 256))

(define textdata-hundreds.str
  ;;A  string holding  100 sequences  of bytes  from 0  included  to 255
  ;;included.
  ;;
  (let ((result #f))
    (define (%string-fill! bv)
      (do ((i 0 (+ 1 i)))
	  ((= i 100)
	   bv)
	(let ((base (* 256 i)))
	  (do ((j 0 (+ 1 j)))
	      ((= j 256))
	    (string-set! bv (+ base j) (integer->char j))))))
    (lambda ()
      (or result
	  (begin
	    (set! result (%string-fill! (make-string (textdata-hundreds.len) #\Z)))
	    result)))))


;;;; Unicode chars, test bytevectors, test strings
;;
;;UTF-8 encodings available from:
;;
;;   <http://www.utf8-chartable.de/>
;;

(define (utf8->utf16le bv)
  (string->utf16 (utf8->string bv) (endianness little)))

(define (utf8->utf16be bv)
  (string->utf16 (utf8->string bv) (endianness big)))

(define (utf8->latin1 bv)
  (string->latin1 (utf8->string bv)))

(define BYTE-ORDER-MARK-UTF-8		'#vu8(#xEF #xBB #xBF))
(define BYTE-ORDER-MARK-UTF-16-LE	'#vu8(#xFF #xFE))
(define BYTE-ORDER-MARK-UTF-16-BE	'#vu8(#xFE #xFF))

;;; --------------------------------------------------------------------

(define LATIN-SMALL-LETTER-A-WITH-GRAVE			#\x00E0)
(define LATIN-SMALL-LETTER-A-WITH-GRAVE-UTF-8		'#vu8(#xC3 #xA0))
(define LATIN-SMALL-LETTER-A-WITH-GRAVE-UTF-16-LE	'#vu8(224 0))
(define LATIN-SMALL-LETTER-A-WITH-GRAVE-UTF-16-BE	'#vu8(0 224))
(define LATIN-SMALL-LETTER-A-WITH-GRAVE-LATIN-1		'#vu8(224))

(define LATIN-SMALL-LETTER-A-WITH-ACUTE			#\x00E1)
(define LATIN-SMALL-LETTER-A-WITH-ACUTE-UTF-8		'#vu8(#xC3 #xA1))
(define LATIN-SMALL-LETTER-A-WITH-ACUTE-UTF-16-LE	'#vu8(225 0))
(define LATIN-SMALL-LETTER-A-WITH-ACUTE-UTF-16-BE	'#vu8(0 225))
(define LATIN-SMALL-LETTER-A-WITH-ACUTE-LATIN-1		'#vu8(225))

;;; --------------------------------------------------------------------

(define LATIN-SMALL-LETTER-E-WITH-GRAVE			#\x00E8)
(define LATIN-SMALL-LETTER-E-WITH-GRAVE-UTF-8		'#vu8(#xC3 #xA8))
(define LATIN-SMALL-LETTER-E-WITH-GRAVE-UTF-16-LE	'#vu8(232 0))
(define LATIN-SMALL-LETTER-E-WITH-GRAVE-UTF-16-BE	'#vu8(0 232))
(define LATIN-SMALL-LETTER-E-WITH-GRAVE-LATIN-1		'#vu8(232))

(define LATIN-SMALL-LETTER-E-WITH-ACUTE			#\x00E9)
(define LATIN-SMALL-LETTER-E-WITH-ACUTE-UTF-8		'#vu8(#xC3 #xA9))
(define LATIN-SMALL-LETTER-E-WITH-ACUTE-UTF-16-LE	'#vu8(233 0))
(define LATIN-SMALL-LETTER-E-WITH-ACUTE-UTF-16-BE	'#vu8(0 233))
(define LATIN-SMALL-LETTER-E-WITH-ACUTE-LATIN-1		'#vu8(233))

;;; --------------------------------------------------------------------

(define LATIN-SMALL-LETTER-I-WITH-GRAVE			#\x00EC)
(define LATIN-SMALL-LETTER-I-WITH-GRAVE-UTF-8		'#vu8(#xC3 #xAC))
(define LATIN-SMALL-LETTER-I-WITH-GRAVE-UTF-16-LE	'#vu8(236 0))
(define LATIN-SMALL-LETTER-I-WITH-GRAVE-UTF-16-BE	'#vu8(0 236))
(define LATIN-SMALL-LETTER-I-WITH-GRAVE-LATIN-1		'#vu8(236))

(define LATIN-SMALL-LETTER-I-WITH-ACUTE			#\x00ED)
(define LATIN-SMALL-LETTER-I-WITH-ACUTE-UTF-8		'#vu8(#xC3 #xAD))
(define LATIN-SMALL-LETTER-I-WITH-ACUTE-UTF-16-LE	'#vu8(237 0))
(define LATIN-SMALL-LETTER-I-WITH-ACUTE-UTF-16-BE	'#vu8(0 237))
(define LATIN-SMALL-LETTER-I-WITH-ACUTE-LATIN-1		'#vu8(237))

;;; --------------------------------------------------------------------

(define LATIN-SMALL-LETTER-O-WITH-GRAVE			#\x00F2)
(define LATIN-SMALL-LETTER-O-WITH-GRAVE-UTF-8		'#vu8(#xC3 #xB2))
(define LATIN-SMALL-LETTER-O-WITH-GRAVE-UTF-16-LE	'#vu8(242 0))
(define LATIN-SMALL-LETTER-O-WITH-GRAVE-UTF-16-BE	'#vu8(0 242))
(define LATIN-SMALL-LETTER-O-WITH-GRAVE-LATIN-1		'#vu8(242))

(define LATIN-SMALL-LETTER-O-WITH-ACUTE			#\x00F3)
(define LATIN-SMALL-LETTER-O-WITH-ACUTE-UTF-8		'#vu8(#xC3 #xB3))
(define LATIN-SMALL-LETTER-O-WITH-ACUTE-UTF-16-LE	'#vu8(243 0))
(define LATIN-SMALL-LETTER-O-WITH-ACUTE-UTF-16-BE	'#vu8(0 243))
(define LATIN-SMALL-LETTER-O-WITH-ACUTE-LATIN-1		'#vu8(243))

;;; --------------------------------------------------------------------

(define LATIN-SMALL-LETTER-U-WITH-GRAVE			#\x00F9)
(define LATIN-SMALL-LETTER-U-WITH-GRAVE-UTF-8		'#vu8(#xC3 #xB9))
(define LATIN-SMALL-LETTER-U-WITH-GRAVE-UTF-16-LE	'#vu8(249 0))
(define LATIN-SMALL-LETTER-U-WITH-GRAVE-UTF-16-BE	'#vu8(0 249))
(define LATIN-SMALL-LETTER-U-WITH-GRAVE-LATIN-1		'#vu8(249))

(define LATIN-SMALL-LETTER-U-WITH-ACUTE			#\x00FA)
(define LATIN-SMALL-LETTER-U-WITH-ACUTE-UTF-8		'#vu8(#xC3 #xBA))
(define LATIN-SMALL-LETTER-U-WITH-ACUTE-UTF-16-LE	'#vu8(250 0))
(define LATIN-SMALL-LETTER-U-WITH-ACUTE-UTF-16-BE	'#vu8(0 250))
(define LATIN-SMALL-LETTER-U-WITH-ACUTE-LATIN-1		'#vu8(250))

;;; --------------------------------------------------------------------

(define GREEK-SMALL-LETTER-ALPHA			#\x03B1)
(define GREEK-SMALL-LETTER-ALPHA-UTF-8			'#vu8(#xCE #xB1))
(define GREEK-SMALL-LETTER-ALPHA-UTF-16-LE		'#vu8(177 3))
(define GREEK-SMALL-LETTER-ALPHA-UTF-16-BE		'#vu8(3 177))

(define GREEK-SMALL-LETTER-BETA				#\x03B2)
(define GREEK-SMALL-LETTER-BETA-UTF-8			'#vu8(#xCE #xB2))
(define GREEK-SMALL-LETTER-BETA-UTF-16-LE		'#vu8(178 3))
(define GREEK-SMALL-LETTER-BETA-UTF-16-BE		'#vu8(3 178))

(define GREEK-SMALL-LETTER-GAMMA			#\x03B3)
(define GREEK-SMALL-LETTER-GAMMA-UTF-8			'#vu8(#xCE #xB3))
(define GREEK-SMALL-LETTER-GAMMA-UTF-16-LE		'#vu8(179 3))
(define GREEK-SMALL-LETTER-GAMMA-UTF-16-BE		'#vu8(3 179))

(define GREEK-SMALL-LETTER-DELTA			#\x03B4)
(define GREEK-SMALL-LETTER-DELTA-UTF-8			'#vu8(#xCE #xB4))
(define GREEK-SMALL-LETTER-DELTA-UTF-16-LE		'#vu8(180 3))
(define GREEK-SMALL-LETTER-DELTA-UTF-16-BE		'#vu8(3 180))

(define GREEK-SMALL-LETTER-LAMBDA			#\x03BB)
(define GREEK-SMALL-LETTER-LAMBDA-UTF-8			'#vu8(#xCE #xBB))
(define GREEK-SMALL-LETTER-LAMBDA-UTF-16-LE		'#vu8(187 3))
(define GREEK-SMALL-LETTER-LAMBDA-UTF-16-BE		'#vu8(3 187))

;;; --------------------------------------------------------------------

(define BENGALI-SIGN-NUKTA				#\x09BC)
(define BENGALI-SIGN-NUKTA-UTF-8			'#vu8(#xE0 #xA6 #xBC))
(define BENGALI-SIGN-NUKTA-UTF-16-LE			'#vu8(188 9))
(define BENGALI-SIGN-NUKTA-UTF-16-BE			'#vu8(9 188))

(define CJK-COMPATIBILITY-IDEOGRAPH-2F9D1		#\x2F9D1)
(define CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-8		'#vu8(#xF0 #xAF #xA7 #x91))
(define CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-16-LE	'#vu8(126 216 209 221))
(define CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-16-BE	'#vu8(216 126 221 209))

;;; --------------------------------------------------------------------

(define TWO-BYTES-UTF-8-CHAR				GREEK-SMALL-LETTER-LAMBDA)
(define TWO-BYTES-UTF-8-CHAR-UTF-8			GREEK-SMALL-LETTER-LAMBDA-UTF-8)
(define CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8
  (let ((bv (bytevector-copy TWO-BYTES-UTF-8-CHAR-UTF-8)))
    (bytevector-u8-set! bv 1 #xFF)
    bv))

(define THREE-BYTES-UTF-8-CHAR				BENGALI-SIGN-NUKTA)
(define THREE-BYTES-UTF-8-CHAR-UTF-8			BENGALI-SIGN-NUKTA-UTF-8)
(define CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8
  (let ((bv (bytevector-copy THREE-BYTES-UTF-8-CHAR-UTF-8)))
    (bytevector-u8-set! bv 1 #xFF)
    bv))
(define CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8
  (let ((bv (bytevector-copy THREE-BYTES-UTF-8-CHAR-UTF-8)))
    (bytevector-u8-set! bv 2 #xFF)
    bv))

(define FOUR-BYTES-UTF-8-CHAR				CJK-COMPATIBILITY-IDEOGRAPH-2F9D1)
(define FOUR-BYTES-UTF-8-CHAR-UTF-8			CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-8)
(define CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8
  (let ((bv (bytevector-copy FOUR-BYTES-UTF-8-CHAR-UTF-8)))
    (bytevector-u8-set! bv 1 #xFF)
    bv))
(define CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8
  (let ((bv (bytevector-copy FOUR-BYTES-UTF-8-CHAR-UTF-8)))
    (bytevector-u8-set! bv 2 #xFF)
    bv))
(define CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8
  (let ((bv (bytevector-copy FOUR-BYTES-UTF-8-CHAR-UTF-8)))
    (bytevector-u8-set! bv 3 #xFF)
    bv))

;;; --------------------------------------------------------------------

(define ONE-WORD-UTF-16-CHAR				GREEK-SMALL-LETTER-LAMBDA)
(define ONE-WORD-UTF-16-CHAR-UTF-16-LE			GREEK-SMALL-LETTER-LAMBDA-UTF-16-LE)
(define ONE-WORD-UTF-16-CHAR-UTF-16-BE			GREEK-SMALL-LETTER-LAMBDA-UTF-16-BE)
(define ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM
  (bytevector-append BYTE-ORDER-MARK-UTF-16-LE ONE-WORD-UTF-16-CHAR-UTF-16-LE))
(define ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM
  (bytevector-append BYTE-ORDER-MARK-UTF-16-BE ONE-WORD-UTF-16-CHAR-UTF-16-BE))

;;The only way  to make a 1-word UTF-16  character appear "corrupted" is
;;to  select it  in the  range allocated  by UTF-16  to second  words in
;;surrogate pairs [#xDC00, #xDFFF].
;;
(define CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD		#xDDFF)
(define CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE
  (let ((bv (make-bytevector 2)))
    (bytevector-u16-set! bv 0 CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD (endianness little))
    bv))
(define CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE
  (let ((bv (make-bytevector 2)))
    (bytevector-u16-set! bv 0 CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD (endianness big))
    bv))
(define CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM
  (let ((bv (make-bytevector 2)))
    (bytevector-u16-set! bv 0 CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD (endianness little))
    (bytevector-append BYTE-ORDER-MARK-UTF-16-LE bv)))
(define CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM
  (let ((bv (make-bytevector 2)))
    (bytevector-u16-set! bv 0 CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD (endianness big))
    (bytevector-append BYTE-ORDER-MARK-UTF-16-BE bv)))

(define TWO-WORDS-UTF-16-CHAR				CJK-COMPATIBILITY-IDEOGRAPH-2F9D1)
(define TWO-WORDS-UTF-16-CHAR-UTF-16-LE			CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-16-LE)
(define TWO-WORDS-UTF-16-CHAR-UTF-16-BE			CJK-COMPATIBILITY-IDEOGRAPH-2F9D1-UTF-16-BE)
(define TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM
  (bytevector-append BYTE-ORDER-MARK-UTF-16-LE TWO-WORDS-UTF-16-CHAR-UTF-16-LE))
(define TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM
  (bytevector-append BYTE-ORDER-MARK-UTF-16-BE TWO-WORDS-UTF-16-CHAR-UTF-16-BE))

;;To make  a 2-word UTF-16  character appear "corrupted" we  select: the
;;first word in the correct range  allocated by UTF-16 to first words in
;;surrogate  pairs [#xD800,  #xDBFF]; the  second word  in the  range of
;;1-word  characters [#x0000,  #xD7FF] or  again in  the range  of first
;;words in surrogate pairs [#xD800, #xDBFF].
;;
(define CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD	#xDAFF)
(define CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD	#x1234)
(define CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE
  (let ((bv (make-bytevector 4)))
    (bytevector-u16-set! bv 0 CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD (endianness little))
    (bytevector-u16-set! bv 2 CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD (endianness little))
    bv))
(define CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE
  (let ((bv (make-bytevector 4)))
    (bytevector-u16-set! bv 0 CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD (endianness big))
    (bytevector-u16-set! bv 2 CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD (endianness big))
    bv))
(define CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM
  (let ((bv (make-bytevector 4)))
    (bytevector-u16-set! bv 0 CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD (endianness little))
    (bytevector-u16-set! bv 2 CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD (endianness little))
    (bytevector-append BYTE-ORDER-MARK-UTF-16-LE bv)))
(define CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM
  (let ((bv (make-bytevector 4)))
    (bytevector-u16-set! bv 0 CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD (endianness big))
    (bytevector-u16-set! bv 2 CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD (endianness big))
    (bytevector-append BYTE-ORDER-MARK-UTF-16-BE bv)))

(define CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD	#xDAFF)
(define CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD	#xDBFF)
(define CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE
  (let ((bv (make-bytevector 4)))
    (bytevector-u16-set! bv 0 CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD (endianness little))
    (bytevector-u16-set! bv 2 CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD (endianness little))
    bv))
(define CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE
  (let ((bv (make-bytevector 4)))
    (bytevector-u16-set! bv 0 CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD (endianness big))
    (bytevector-u16-set! bv 2 CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD (endianness big))
    bv))
(define CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM
  (let ((bv (make-bytevector 4)))
    (bytevector-u16-set! bv 0 CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD (endianness little))
    (bytevector-u16-set! bv 2 CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD (endianness little))
    (bytevector-append BYTE-ORDER-MARK-UTF-16-LE bv)))
(define CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM
  (let ((bv (make-bytevector 4)))
    (bytevector-u16-set! bv 0 CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD (endianness big))
    (bytevector-u16-set! bv 2 CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD (endianness big))
    (bytevector-append BYTE-ORDER-MARK-UTF-16-BE bv)))

;;; --------------------------------------------------------------------

(define TEST-STRING-FOR-LATIN-1
  (string
   #\c #\i #\a #\o #\space
   LATIN-SMALL-LETTER-A-WITH-GRAVE LATIN-SMALL-LETTER-A-WITH-ACUTE
   LATIN-SMALL-LETTER-E-WITH-GRAVE LATIN-SMALL-LETTER-E-WITH-ACUTE
   LATIN-SMALL-LETTER-I-WITH-GRAVE LATIN-SMALL-LETTER-I-WITH-ACUTE
   LATIN-SMALL-LETTER-O-WITH-GRAVE LATIN-SMALL-LETTER-O-WITH-ACUTE
   LATIN-SMALL-LETTER-U-WITH-GRAVE LATIN-SMALL-LETTER-U-WITH-ACUTE))

(define TEST-BYTEVECTOR-FOR-LATIN-1
  (bytevector-append
   (string->latin1 "ciao ")
   LATIN-SMALL-LETTER-A-WITH-GRAVE-LATIN-1 LATIN-SMALL-LETTER-A-WITH-ACUTE-LATIN-1
   LATIN-SMALL-LETTER-E-WITH-GRAVE-LATIN-1 LATIN-SMALL-LETTER-E-WITH-ACUTE-LATIN-1
   LATIN-SMALL-LETTER-I-WITH-GRAVE-LATIN-1 LATIN-SMALL-LETTER-I-WITH-ACUTE-LATIN-1
   LATIN-SMALL-LETTER-O-WITH-GRAVE-LATIN-1 LATIN-SMALL-LETTER-O-WITH-ACUTE-LATIN-1
   LATIN-SMALL-LETTER-U-WITH-GRAVE-LATIN-1 LATIN-SMALL-LETTER-U-WITH-ACUTE-LATIN-1))

;;; --------------------------------------------------------------------

(define TEST-STRING-FOR-UTF-8
  (string
   #\c #\i #\a #\o #\space
   LATIN-SMALL-LETTER-A-WITH-GRAVE LATIN-SMALL-LETTER-A-WITH-ACUTE
   LATIN-SMALL-LETTER-E-WITH-GRAVE LATIN-SMALL-LETTER-E-WITH-ACUTE
   LATIN-SMALL-LETTER-I-WITH-GRAVE LATIN-SMALL-LETTER-I-WITH-ACUTE
   LATIN-SMALL-LETTER-O-WITH-GRAVE LATIN-SMALL-LETTER-O-WITH-ACUTE
   LATIN-SMALL-LETTER-U-WITH-GRAVE LATIN-SMALL-LETTER-U-WITH-ACUTE
   GREEK-SMALL-LETTER-ALPHA GREEK-SMALL-LETTER-BETA
   GREEK-SMALL-LETTER-GAMMA GREEK-SMALL-LETTER-DELTA
   GREEK-SMALL-LETTER-LAMBDA
   TWO-BYTES-UTF-8-CHAR THREE-BYTES-UTF-8-CHAR FOUR-BYTES-UTF-8-CHAR))

(define TEST-BYTEVECTOR-FOR-UTF-8
  (bytevector-append
   (string->utf8 "ciao ")
   LATIN-SMALL-LETTER-A-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-A-WITH-ACUTE-UTF-8
   LATIN-SMALL-LETTER-E-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-E-WITH-ACUTE-UTF-8
   LATIN-SMALL-LETTER-I-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-I-WITH-ACUTE-UTF-8
   LATIN-SMALL-LETTER-O-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-O-WITH-ACUTE-UTF-8
   LATIN-SMALL-LETTER-U-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-U-WITH-ACUTE-UTF-8
   GREEK-SMALL-LETTER-ALPHA-UTF-8 GREEK-SMALL-LETTER-BETA-UTF-8
   GREEK-SMALL-LETTER-GAMMA-UTF-8 GREEK-SMALL-LETTER-DELTA-UTF-8
   GREEK-SMALL-LETTER-LAMBDA-UTF-8
   TWO-BYTES-UTF-8-CHAR-UTF-8
   THREE-BYTES-UTF-8-CHAR-UTF-8
   FOUR-BYTES-UTF-8-CHAR-UTF-8))

;;The following "special" test strings are here to hunt a specific bug.
;;
(define SPECIAL-TEST-STRING-FOR-UTF-8
  (string
   LATIN-SMALL-LETTER-A-WITH-GRAVE LATIN-SMALL-LETTER-A-WITH-ACUTE
   LATIN-SMALL-LETTER-E-WITH-GRAVE LATIN-SMALL-LETTER-E-WITH-ACUTE
   LATIN-SMALL-LETTER-I-WITH-GRAVE LATIN-SMALL-LETTER-I-WITH-ACUTE
   LATIN-SMALL-LETTER-O-WITH-GRAVE LATIN-SMALL-LETTER-O-WITH-ACUTE
   LATIN-SMALL-LETTER-U-WITH-GRAVE LATIN-SMALL-LETTER-U-WITH-ACUTE))
(define SPECIAL-TEST-BYTEVECTOR-FOR-UTF-8
  (bytevector-append
   LATIN-SMALL-LETTER-A-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-A-WITH-ACUTE-UTF-8
   LATIN-SMALL-LETTER-E-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-E-WITH-ACUTE-UTF-8
   LATIN-SMALL-LETTER-I-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-I-WITH-ACUTE-UTF-8
   LATIN-SMALL-LETTER-O-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-O-WITH-ACUTE-UTF-8
   LATIN-SMALL-LETTER-U-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-U-WITH-ACUTE-UTF-8))

(define SPECIAL1-TEST-STRING-FOR-UTF-8
  (string
   #\c #\i #\a #\o #\space
   LATIN-SMALL-LETTER-A-WITH-GRAVE LATIN-SMALL-LETTER-A-WITH-ACUTE))
(define SPECIAL1-TEST-BYTEVECTOR-FOR-UTF-8
  (bytevector-append
   (string->utf8 "ciao ")
   LATIN-SMALL-LETTER-A-WITH-GRAVE-UTF-8 LATIN-SMALL-LETTER-A-WITH-ACUTE-UTF-8))

;;; --------------------------------------------------------------------

(define TEST-STRING-FOR-UTF-16-LE
  (string
   LATIN-SMALL-LETTER-A-WITH-GRAVE LATIN-SMALL-LETTER-A-WITH-ACUTE
   LATIN-SMALL-LETTER-E-WITH-GRAVE LATIN-SMALL-LETTER-E-WITH-ACUTE
   LATIN-SMALL-LETTER-I-WITH-GRAVE LATIN-SMALL-LETTER-I-WITH-ACUTE
   LATIN-SMALL-LETTER-O-WITH-GRAVE LATIN-SMALL-LETTER-O-WITH-ACUTE
   LATIN-SMALL-LETTER-U-WITH-GRAVE LATIN-SMALL-LETTER-U-WITH-ACUTE
   GREEK-SMALL-LETTER-ALPHA GREEK-SMALL-LETTER-BETA
   GREEK-SMALL-LETTER-GAMMA GREEK-SMALL-LETTER-DELTA
   GREEK-SMALL-LETTER-LAMBDA
   ONE-WORD-UTF-16-CHAR
   TWO-WORDS-UTF-16-CHAR))

(define TEST-BYTEVECTOR-FOR-UTF-16-LE
  (bytevector-append
   LATIN-SMALL-LETTER-A-WITH-GRAVE-UTF-16-LE LATIN-SMALL-LETTER-A-WITH-ACUTE-UTF-16-LE
   LATIN-SMALL-LETTER-E-WITH-GRAVE-UTF-16-LE LATIN-SMALL-LETTER-E-WITH-ACUTE-UTF-16-LE
   LATIN-SMALL-LETTER-I-WITH-GRAVE-UTF-16-LE LATIN-SMALL-LETTER-I-WITH-ACUTE-UTF-16-LE
   LATIN-SMALL-LETTER-O-WITH-GRAVE-UTF-16-LE LATIN-SMALL-LETTER-O-WITH-ACUTE-UTF-16-LE
   LATIN-SMALL-LETTER-U-WITH-GRAVE-UTF-16-LE LATIN-SMALL-LETTER-U-WITH-ACUTE-UTF-16-LE
   GREEK-SMALL-LETTER-ALPHA-UTF-16-LE GREEK-SMALL-LETTER-BETA-UTF-16-LE
   GREEK-SMALL-LETTER-GAMMA-UTF-16-LE GREEK-SMALL-LETTER-DELTA-UTF-16-LE
   GREEK-SMALL-LETTER-LAMBDA-UTF-16-LE
   ONE-WORD-UTF-16-CHAR-UTF-16-LE TWO-WORDS-UTF-16-CHAR-UTF-16-LE))

(define TEST-BYTEVECTOR-FOR-UTF-16-LE/BOM
  (bytevector-append BYTE-ORDER-MARK-UTF-16-LE TEST-BYTEVECTOR-FOR-UTF-16-LE))

;;; --------------------------------------------------------------------

(define TEST-STRING-FOR-UTF-16-BE
  (string
   LATIN-SMALL-LETTER-A-WITH-GRAVE LATIN-SMALL-LETTER-A-WITH-ACUTE
   LATIN-SMALL-LETTER-E-WITH-GRAVE LATIN-SMALL-LETTER-E-WITH-ACUTE
   LATIN-SMALL-LETTER-I-WITH-GRAVE LATIN-SMALL-LETTER-I-WITH-ACUTE
   LATIN-SMALL-LETTER-O-WITH-GRAVE LATIN-SMALL-LETTER-O-WITH-ACUTE
   LATIN-SMALL-LETTER-U-WITH-GRAVE LATIN-SMALL-LETTER-U-WITH-ACUTE
   GREEK-SMALL-LETTER-ALPHA GREEK-SMALL-LETTER-BETA
   GREEK-SMALL-LETTER-GAMMA GREEK-SMALL-LETTER-DELTA
   GREEK-SMALL-LETTER-LAMBDA
   ONE-WORD-UTF-16-CHAR TWO-WORDS-UTF-16-CHAR))

(define TEST-BYTEVECTOR-FOR-UTF-16-BE
  (bytevector-append
   LATIN-SMALL-LETTER-A-WITH-GRAVE-UTF-16-BE LATIN-SMALL-LETTER-A-WITH-ACUTE-UTF-16-BE
   LATIN-SMALL-LETTER-E-WITH-GRAVE-UTF-16-BE LATIN-SMALL-LETTER-E-WITH-ACUTE-UTF-16-BE
   LATIN-SMALL-LETTER-I-WITH-GRAVE-UTF-16-BE LATIN-SMALL-LETTER-I-WITH-ACUTE-UTF-16-BE
   LATIN-SMALL-LETTER-O-WITH-GRAVE-UTF-16-BE LATIN-SMALL-LETTER-O-WITH-ACUTE-UTF-16-BE
   LATIN-SMALL-LETTER-U-WITH-GRAVE-UTF-16-BE LATIN-SMALL-LETTER-U-WITH-ACUTE-UTF-16-BE
   GREEK-SMALL-LETTER-ALPHA-UTF-16-BE GREEK-SMALL-LETTER-BETA-UTF-16-BE
   GREEK-SMALL-LETTER-GAMMA-UTF-16-BE GREEK-SMALL-LETTER-DELTA-UTF-16-BE
   GREEK-SMALL-LETTER-LAMBDA-UTF-16-BE
   ONE-WORD-UTF-16-CHAR-UTF-16-BE TWO-WORDS-UTF-16-CHAR-UTF-16-BE))

(define TEST-BYTEVECTOR-FOR-UTF-16-BE/BOM
  (bytevector-append BYTE-ORDER-MARK-UTF-16-BE TEST-BYTEVECTOR-FOR-UTF-16-BE))


(parametrise ((check-test-name			'make-custom-binary-input-port)
	      (bytevector-port-buffer-size	8))

  (define (make-test-port bv)
    (let ((position	0)
	  (bv.len	(bytevector-length bv))
	  (port		#f))

      (define (read! dst.bv dst.start count)
	(let* ((available	(- bv.len position))
	       (to-read		(min available count)))
	  (unless (zero? to-read)
	    (bytevector-copy! bv position dst.bv dst.start to-read)
	    (set! position (+ to-read position)))
	  to-read))

      (define (get-position)
	position)

      (define (set-position! new-position)
	(if (<= 0 new-position bv.len)
	    (set! position new-position)
	  (raise
	   (condition (make-i/o-invalid-position-error new-position)
		      (make-who-condition 'make-test-port/set-position!)
		      (make-message-condition "invalid port position")
		      (make-irritants-condition (list port))))))

      (define (close)
	(set! bv #f))

      (set! port (make-custom-binary-input-port "*test-binary-input-port*"
						read! get-position set-position! close))
      port))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;ID is not a string
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-input-port 123	     ;id
				       (lambda args #f)   ;read!
				       (lambda args #f)   ;get-position
				       (lambda args #f)   ;set-position!
				       (lambda args #f))) ;close
    => '(123))

  (check	;READ! is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-input-port "test"	     ;id
				       123	     ;read!
				       (lambda args #f)   ;get-position
				       (lambda args #f)   ;set-position!
				       (lambda args #f))) ;close
    => '(123))

  (check	;GET-POSITION is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-input-port "test"	     ;id
				       (lambda args #f)   ;read!
				       123	     ;get-position
				       (lambda args #f)   ;set-position!
				       (lambda args #f))) ;close
    => '(123))

  (check	;SET-POSITION! is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-input-port "test"	     ;id
				       (lambda args #f)   ;read!
				       (lambda args #f)   ;get-position
				       123	     ;set-position!
				       (lambda args #f))) ;close
    => '(123))

  (check	;CLOSE is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-input-port "test"	   ;id
				       (lambda args #f) ;read!
				       (lambda args #f) ;get-position
				       (lambda args #f) ;set-position!
				       123))	   ;close
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (port-has-port-position? (make-test-port '#vu8()))
    => #t)

  (check
      (port-has-set-port-position!? (make-test-port '#vu8()))
    => #t)

;;; --------------------------------------------------------------------
;;; reading bytes

  (check
      (let ((port (make-test-port '#vu8())))
	(lookahead-u8 port))
    => (eof-object))

  (check
      (let ((port (make-test-port '#vu8(12))))
	(lookahead-u8 port))
    => 12)

  (check
      (let ((port (make-test-port '#vu8())))
	(get-u8 port))
    => (eof-object))

  (check
      (let ((port (make-test-port '#vu8(12))))
	(get-u8 port))
    => 12)

  (check
      (let ((port (make-test-port '#vu8(12))))
	(get-u8 port)
	(get-u8 port))
    => (eof-object))

;;; --------------------------------------------------------------------
;;; reading bytevectors

  (check
      (let ((port (make-test-port '#vu8())))
	(get-bytevector-n port 3))
    => (eof-object))

  (check
      (check.with-result
	  (let ((port (make-test-port '#vu8())))
	    (check.add-result (get-bytevector-n port 0))
	    (eof-object? (get-u8 port))))
    => '(#t (#vu8())))

  (check
      (check.with-result
	  (let ((port (make-test-port '#vu8(0 1 2))))
	    (check.add-result (get-bytevector-n port 3))
	    (eof-object? (get-u8 port))))
    => '(#t (#vu8(0 1 2))))

  (check
      (check.with-result
	  (let ((port (make-test-port '#vu8(0 1 2 3 4 5))))
	    (check.add-result (get-bytevector-n port 3))
	    (check.add-result (get-bytevector-n port 3))
	    (eof-object? (get-u8 port))))
    => '(#t (#vu8(0 1 2) #vu8(3 4 5))))

;;; --------------------------------------------------------------------
;;; getting position

  (check
      (let ((port (make-test-port '#vu8())))
	(port-position port))
    => 0)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(port-position port))
    => 0)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(get-bytevector-n port 5)
	(port-position port))
    => 5)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(get-bytevector-n port 10)
	(port-position port))
    => 10)

;;; --------------------------------------------------------------------
;;; setting position

  (check
      (let ((port (make-test-port '#vu8())))
	(set-port-position! port 0)
	(port-position port))
    => 0)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(set-port-position! port 10)
	(port-position port))
    => 10)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(set-port-position! port 5)
	(port-position port))
    => 5)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(set-port-position! port 5)
	(set-port-position! port 1)
	(set-port-position! port 9)
	(port-position port))
    => 9)

  (check
      (check.with-result
	  (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	    (set-port-position! port 5)
	    (check.add-result (get-bytevector-n port 5))
	    (port-position port)))
    => '(10 (#vu8(5 6 7 8 9))))

;;; --------------------------------------------------------------------
;;; closing the port

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(port-closed? port))
    => #f)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(close-port port)
	#t)
    => #t)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(close-port port)
	(close-port port)
	(close-port port)
	#t)
    => #t)

  (check
      (let ((port (make-test-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(close-port port)
	(port-closed? port))
    => #t)

  #t)


(parametrise ((check-test-name			'make-custom-binary-output-port)
	      (bytevector-port-buffer-size	8))

  (define (make-test-port)
    (let ((port #f))
      (let-values (((subport extract) (open-bytevector-output-port)))

	(define (write! src.bv src.start count)
	  (do ((i 0 (+ 1 i)))
	      ((= i count)
	       count)
	    (put-u8 subport (bytevector-u8-ref src.bv (+ i src.start)))))

	(define (get-position)
	  (port-position subport))

	(define (set-position! new-position)
	  (set-port-position! subport new-position))

	(define (close)
	  #f)

	(set! port (make-custom-binary-output-port "*test-binary-output-port*"
						   write! get-position set-position! close))
	(values port (lambda ()
		       (flush-output-port port)
		       (extract))))))

  (define-syntax snapshot-position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx)
	 (with-syntax ((PORT	(datum->syntax #'?ctx 'port))
		       (EXTRACT (datum->syntax #'?ctx 'extract)))
	   #'(let ((pos (port-position PORT)))
	       (list pos (EXTRACT))))))))

  (define-syntax position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx ?port . ?body)
	 #'(let-values (((?port extract) (make-test-port)))
	     (begin . ?body)
	     (let ((pos (port-position ?port)))
	       (list pos (extract))))))))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;ID is not a string
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-output-port 123	      ;id
					(lambda args #f)   ;write!
					(lambda args #f)   ;get-position
					(lambda args #f)   ;set-position!
					(lambda args #f))) ;close
    => '(123))

  (check	;WRITE! is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-output-port "test"	      ;id
					123	      ;write!
					(lambda args #f)   ;get-position
					(lambda args #f)   ;set-position!
					(lambda args #f))) ;close
    => '(123))

  (check	;GET-POSITION is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-output-port "test"	      ;id
					(lambda args #f)   ;write!
					123	      ;get-position
					(lambda args #f)   ;set-position!
					(lambda args #f))) ;close
    => '(123))

  (check	;SET-POSITION! is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-output-port "test"	      ;id
					(lambda args #f)   ;write!
					(lambda args #f)   ;get-position
					123	      ;set-position!
					(lambda args #f))) ;close
    => '(123))

  (check	;CLOSE is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-binary-output-port "test"	    ;id
					(lambda args #f) ;write!
					(lambda args #f) ;get-position
					(lambda args #f) ;set-position!
					123))	    ;close
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (let-values (((port extract) (make-test-port)))
	(port-has-port-position? port))
    => #t)

  (check
      (let-values (((port extract) (make-test-port)))
	(port-has-set-port-position!? port))
    => #t)

;;; --------------------------------------------------------------------
;;; writing data, single extraction, no transcoder

  (check	;single byte
      (let-values (((port extract) (make-test-port)))
	(put-u8 port 65)
	(extract))
    => '#vu8(65))

  (check	;byte by byte until the buffer is full
      (let-values (((port extract) (make-test-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 9 i))
	  (put-u8 port 65))
	(extract))
    => '#vu8(65 65 65  65 65 65  65 65 65))

  (check	;single bytevecor not filling the buffer
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(1 2 3 4 5))
	(extract))
    => '#vu8(1 2 3 4 5))

  (check	;single bytevecor filling the buffer
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(extract))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check	;bytevector by bytevector until the buffer is full
      (let-values (((port extract) (make-test-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 3 i))
	  (put-bytevector port '#vu8(1 2 3)))
	(extract))
    => '#vu8(1 2 3  1 2 3  1 2 3))

  (check	;fill the buffer multiple times
      (let-values (((port extract) (make-test-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 5 i))
	  (put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9)))
	(extract))
    => '#vu8( ;;
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; writing data, multiple extraction, no transcoder

  (check	;empty device, data in buffer
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(0 1 2))
	(let ((result (snapshot-position-and-contents)))
	  (put-bytevector port '#vu8(3 4 5 6))
	  (list result (snapshot-position-and-contents))))
    => '((3 #vu8(0 1 2))
	 (4 #vu8(3 4 5 6))))

  (check	;empty buffer, data in device
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(flush-output-port port)
	(let ((result (snapshot-position-and-contents)))
	  (put-bytevector port '#vu8(10 11 12 13 14 15 16 17 18 19))
	  (flush-output-port port)
	  (list result (snapshot-position-and-contents))))
    => '((10 #vu8(0 1 2 3 4 5 6 7 8 9))
	 (10 #vu8(10 11 12 13 14 15 16 17 18 19))))

  (check	;some data in device, some data in buffer
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(let ((result (snapshot-position-and-contents)))
	  (put-bytevector port '#vu8(10 11 12 13 14 15 16 17 18 19))
	  (list result (snapshot-position-and-contents))))
    => '((10 #vu8(0 1 2 3 4 5 6 7 8 9))
	 (10 #vu8(10 11 12 13 14 15 16 17 18 19))))

;;; --------------------------------------------------------------------
;;; getting port position, no transcoder

  (check	;empty device
      (let-values (((port extract) (make-test-port)))
	(port-position port))
    => 0)

  (check	;some data in buffer, none in device
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(0 1 2))
	(port-position port))
    => 3)

  (check	;buffer full, no data in device
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(port-position port))
    => 8)

  (check	;some data in buffer, some data in device
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(put-bytevector port '#vu8(8 9 10 11))
	(port-position port))
    => 12)

  (check	;buffer empty, data in device
      (let-values (((port extract) (make-test-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(port-position port))
    => 8)

;;; --------------------------------------------------------------------
;;; setting port position, no overwriting, no transcoder

  (check	;empty device
      (position-and-contents port
	(set-port-position! port 0))
    => '(0 #vu8()))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2))
	(set-port-position! port 1))
    => '(1 #vu8(0 1 2)))

  (check   ;Some data  in buffer, none in device.   Move position in the
		;middle, then again at the end.
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2))
	(set-port-position! port 1)
	(set-port-position! port 3))
    => '(3 #vu8(0 1 2)))

  (check ;Buffer full, no data  in device.  Move position in the middle,
		;then again at the end.
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 1)
	(set-port-position! port 8))
    => '(8 #vu8(0 1 2 3 4 5 6 7)))

  (check	;Some  data  in  buffer,  some  data  in  device.   Move
		;position in the middle.
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(put-bytevector port '#vu8(8 9 10 11))
	(set-port-position! port 6))
    => '(6 #vu8(0 1 2 3 4 5 6 7 8 9 10 11)))

  (check	;Buffer  empty, data  in device.   Move position  in the
		;middle.
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(flush-output-port port)
	(set-port-position! port 6))
    => '(6 #vu8(0 1 2 3 4 5 6 7)))

;;; --------------------------------------------------------------------
;;; setting port position, overwriting, no transcoder

  (check	;empty buffer, empty device
      (position-and-contents port
	(set-port-position! port 0)
	(put-bytevector port '#vu8()))
    => '(0 #vu8()))

  (check       ;partial internal overwrite, data in buffer, empty device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 2)
	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7)))

  (check       ;partial internal overwrite, empty buffer, data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(set-port-position! port 2)
	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7)))

  (check ;partial internal overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 2)
	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7 8 9)))

  (check	;overflow overwrite, data in buffer, empty device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 6)
	(put-bytevector port '#vu8(60 70 80 90)))
    => '(10 #vu8(0 1 2 3 4 5 60 70 80 90)))

  (check	;overflow overwrite, empty buffer, empty device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(set-port-position! port 6)
	(put-bytevector port '#vu8(60 70 80 90)))
    => '(10 #vu8(0 1 2 3 4 5 60 70 80 90)))

  (check   ;overflow overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 6)
	(put-bytevector port '#vu8(60 70 80 90 100 110)))
    => '(12 #vu8(0 1 2 3 4 5 60 70 80 90 100 110)))

  (check	;full overwrite, data in buffer, empty device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 0)
	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17)))
    => '(8 #vu8(10 11 12 13 14 15 16 17)))

  (check	;full overwrite, empty buffer, data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(set-port-position! port 0)
	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17)))
    => '(8 #vu8(10 11 12 13 14 15 16 17)))

  (check       ;full overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 0)
	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17 18 19)))
    => '(10 #vu8(10 11 12 13 14 15 16 17 18 19)))

  #t)


(parametrise ((check-test-name		'make-custom-textual-input-port)
	      (string-port-buffer-size	8))

  (define (make-test-port str)
    (let ((position	0)
	  (str.len	(string-length str))
	  (port		#f))

      (define (read! dst.str dst.start count)
	(let* ((available	(- str.len position))
	       (to-read		(min available count)))
	  (unless (zero? to-read)
	    (string-copy! str position dst.str dst.start to-read)
	    (set! position (+ to-read position)))
	  to-read))

      (define (get-position)
	position)

      (define (set-position! new-position)
	(if (<= 0 new-position str.len)
	    (set! position new-position)
	  (raise
	   (condition (make-i/o-invalid-position-error new-position)
		      (make-who-condition 'make-test-port/set-position!)
		      (make-message-condition "invalid port position")
		      (make-irritants-condition (list port))))))

      (define (close)
	(set! str #f))

      (set! port (make-custom-textual-input-port "*test-custom-input-port*"
						 read! get-position set-position! close))
      port))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;ID is not a string
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-input-port 123	     ;id
					(lambda args #f)  ;read!
					(lambda args #f)  ;get-position
					(lambda args #f)  ;set-position!
					(lambda args #f))) ;close
    => '(123))

  (check	;READ! is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-input-port "test"	     ;id
					123	     ;read!
					(lambda args #f)  ;get-position
					(lambda args #f)  ;set-position!
					(lambda args #f))) ;close
    => '(123))

  (check	;GET-POSITION is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-input-port "test"	     ;id
					(lambda args #f)  ;read!
					123	     ;get-position
					(lambda args #f)  ;set-position!
					(lambda args #f))) ;close
    => '(123))

  (check	;SET-POSITION! is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-input-port "test"	     ;id
					(lambda args #f)  ;read!
					(lambda args #f)  ;get-position
					123	     ;set-position!
					(lambda args #f))) ;close
    => '(123))

  (check	;CLOSE is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-input-port "test"	   ;id
					(lambda args #f) ;read!
					(lambda args #f) ;get-position
					(lambda args #f) ;set-position!
					123))	    ;close
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (port-has-port-position? (make-test-port ""))
    => #t)

  (check
      (port-has-set-port-position!? (make-test-port ""))
    => #t)

;;; --------------------------------------------------------------------
;;; reading bytes

  (check
      (let ((port (make-test-port "")))
	(lookahead-char port))
    => (eof-object))

  (check
      (let ((port (make-test-port "12")))
	(lookahead-char port))
    => #\1)

  (check
      (let ((port (make-test-port "")))
	(get-char port))
    => (eof-object))

  (check
      (let ((port (make-test-port "12")))
	(get-char port))
    => #\1)

  (check
      (let ((port (make-test-port "1")))
	(get-char port)
	(get-char port))
    => (eof-object))

;;; --------------------------------------------------------------------
;;; reading strings

  (check
      (let ((port (make-test-port "")))
	(get-string-n port 3))
    => (eof-object))

  (check
      (check.with-result
	  (let ((port (make-test-port "")))
	    (check.add-result (get-string-n port 0))
	    (eof-object? (get-char port))))
    => '(#t ("")))

  (check
      (check.with-result
	  (let ((port (make-test-port "012")))
	    (check.add-result (get-string-n port 3))
	    (eof-object? (get-char port))))
    => '(#t ("012")))

  (check
      (check.with-result
	  (let ((port (make-test-port "012345")))
	    (check.add-result (get-string-n port 3))
	    (check.add-result (get-string-n port 3))
	    (eof-object? (get-char port))))
    => '(#t ("012" "345")))

;;; --------------------------------------------------------------------
;;; getting position

  (check
      (let ((port (make-test-port "")))
	(port-position port))
    => 0)

  (check
      (let ((port (make-test-port "0123456789")))
	(port-position port))
    => 0)

  (check
      (let ((port (make-test-port "0123456789")))
	(get-string-n port 5)
	(port-position port))
    => 5)

  (check
      (let ((port (make-test-port "0123456789")))
	(get-string-n port 10)
	(port-position port))
    => 10)

;;; --------------------------------------------------------------------
;;; setting position

  (check
      (let ((port (make-test-port "")))
	(set-port-position! port 0)
	(port-position port))
    => 0)

  (check
      (let ((port (make-test-port "0123456789")))
	(set-port-position! port 10)
	(port-position port))
    => 10)

  (check
      (let ((port (make-test-port "0123456789")))
	(set-port-position! port 5)
	(port-position port))
    => 5)

  (check
      (let ((port (make-test-port "0123456789")))
	(set-port-position! port 5)
	(set-port-position! port 1)
	(set-port-position! port 9)
	(port-position port))
    => 9)

  (check
      (check.with-result
	  (let ((port (make-test-port "0123456789")))
	    (set-port-position! port 5)
	    (check.add-result (get-string-n port 5))
	    (port-position port)))
    => '(10 ("56789")))

  #t)


(parametrise ((check-test-name		'make-custom-textual-output-port)
	      (string-port-buffer-size	8))

  (define (make-test-port)
    (let ((port #f))
      (let-values (((subport extract) (open-string-output-port)))

	(define (write! src.str src.start count)
	  (do ((i 0 (+ 1 i)))
	      ((= i count)
	       count)
	    (put-char subport (string-ref src.str (+ i src.start)))))

	(define (get-position)
	  (port-position subport))

	(define (set-position! new-position)
	  (set-port-position! subport new-position))

	(define (close)
	  #f)

	(set! port (make-custom-textual-output-port "*test-textual-output-port*"
						    write! get-position set-position! close))
	(values port (lambda ()
		       (flush-output-port port)
		       (extract))))))

  (define-syntax snapshot-position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx)
	 (with-syntax ((PORT	(datum->syntax #'?ctx 'port))
		       (EXTRACT (datum->syntax #'?ctx 'extract)))
	   #'(let ((pos (port-position PORT)))
	       (list pos (EXTRACT))))))))

  (define-syntax position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx ?port . ?body)
	 #'(let-values (((?port extract) (make-test-port)))
	     (begin . ?body)
	     (let ((pos (port-position ?port)))
	       (list pos (extract))))))))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;ID is not a string
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-output-port 123	       ;id
					 (lambda args #f)   ;write!
					 (lambda args #f)   ;get-position
					 (lambda args #f)   ;set-position!
					 (lambda args #f))) ;close
    => '(123))

  (check	;WRITE! is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-output-port "test"	       ;id
					 123	       ;write!
					 (lambda args #f)   ;get-position
					 (lambda args #f)   ;set-position!
					 (lambda args #f))) ;close
    => '(123))

  (check	;GET-POSITION is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-output-port "test"	       ;id
					 (lambda args #f)   ;write!
					 123	       ;get-position
					 (lambda args #f)   ;set-position!
					 (lambda args #f))) ;close
    => '(123))

  (check	;SET-POSITION! is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-output-port "test"	       ;id
					 (lambda args #f)   ;write!
					 (lambda args #f)   ;get-position
					 123	       ;set-position!
					 (lambda args #f))) ;close
    => '(123))

  (check	;CLOSE is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-custom-textual-output-port "test"	     ;id
					 (lambda args #f) ;write!
					 (lambda args #f) ;get-position
					 (lambda args #f) ;set-position!
					 123))	     ;close
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (let-values (((port extract) (make-test-port)))
	(port-has-port-position? port))
    => #t)

  (check
      (let-values (((port extract) (make-test-port)))
	(port-has-set-port-position!? port))
    => #t)

;;; --------------------------------------------------------------------
;;; writing data, single extraction

  (check	;single char
      (let-values (((port extract) (make-test-port)))
	(put-char port #\A)
	(extract))
    => "A")

  (check	;char by char until the buffer is full
      (let-values (((port extract) (make-test-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 9 i)
	     (extract))
	  (put-char port #\A)))
    => "AAAAAAAAA")

  (check	;single string not filling the buffer
      (let-values (((port extract) (make-test-port)))
	(put-string port "12345")
	(extract))
    => "12345")

  (check	;single string filling the buffer
      (let-values (((port extract) (make-test-port)))
	(put-string port "0123456789")
	(extract))
    => "0123456789")

  (check	;string by string until the buffer is full
      (let-values (((port extract) (make-test-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 3 i)
	     (extract))
	  (put-string port "123")))
    => "123123123")

  (check	;fill the buffer multiple times
      (let-values (((port extract) (make-test-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 5 i)
	     (extract))
	  (put-string port "0123456789")))
    => "0123456789\
        0123456789\
        0123456789\
        0123456789\
        0123456789")

;;; --------------------------------------------------------------------
;;; writing data, multiple extraction

  (check	;empty device, data in buffer
      (let-values (((port extract) (make-test-port)))
	(put-string port "012")
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "3456")
	  (list result (snapshot-position-and-contents))))
    => '((3 "012")
	 (4 "3456")))

  (check	;empty buffer, data in device
      (let-values (((port extract) (make-test-port)))
	(put-string port "0123456789")
	(flush-output-port port)
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "abcdefghil")
	  (flush-output-port port)
	  (list result (snapshot-position-and-contents))))
    => '((10 "0123456789")
	 (10 "abcdefghil")))

  (check	;some data in device, some data in buffer
      (let-values (((port extract) (make-test-port)))
	(put-string port "0123456789")
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "abcdefghil")
	  (list result (snapshot-position-and-contents))))
    => '((10 "0123456789")
	 (10 "abcdefghil")))

;;; --------------------------------------------------------------------
;;; getting port position

  (check	;empty device
      (let-values (((port extract) (make-test-port)))
	(port-position port))
    => 0)

  (check	;some data in buffer, none in device
      (let-values (((port extract) (make-test-port)))
	(put-string port "012")
	(port-position port))
    => 3)

  (check	;buffer full, no data in device
      (let-values (((port extract) (make-test-port)))
	(put-string port "01234567")
	(port-position port))
    => 8)

  (check	;some data in buffer, some data in device
      (let-values (((port extract) (make-test-port)))
	(put-string port "01234567")
	(put-string port "89ab")
	(port-position port))
    => 12)

  (check	;buffer empty, data in device
      (let-values (((port extract) (make-test-port)))
	(put-string port "01234567")
	(flush-output-port port)
	(port-position port))
    => 8)

;;; --------------------------------------------------------------------
;;; setting port position, no overwriting

  (check	;empty device
      (position-and-contents port
	(set-port-position! port 0))
    => '(0 ""))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-string port "012")
	(set-port-position! port 1))
    => '(1 "012"))

  (check   ;Some data  in buffer, none in device.   Move position in the
		;middle, then again at the end.
      (position-and-contents port
	(put-string port "012")
	(set-port-position! port 1)
	(set-port-position! port 3))
    => '(3 "012"))

  (check ;Buffer full, no data  in device.  Move position in the middle,
		;then again at the end.
      (position-and-contents port
  	(put-string port "01234567")
	(set-port-position! port 1)
	(set-port-position! port 8))
    => '(8 "01234567"))

  (check	;Some  data  in  buffer,  some  data  in  device.   Move
		;position in the middle.
      (position-and-contents port
  	(put-string port "01234567")
  	(put-string port "89ab")
	(set-port-position! port 6))
    => '(6 "0123456789ab"))

  (check	;Buffer  empty, data  in device.   Move position  in the
		;middle.
      (position-and-contents port
  	(put-string port "01234567")
  	(flush-output-port port)
	(set-port-position! port 6))
    => '(6 "01234567"))

;;; --------------------------------------------------------------------
;;; setting port position, overwriting

  (check	;empty buffer, empty device
      (position-and-contents port
	(set-port-position! port 0)
	(put-string port ""))
    => '(0 ""))

  (check       ;partial internal overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc567"))

  (check       ;partial internal overwrite, empty buffer, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc567"))

  (check ;partial internal overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc56789"))

  (check	;overflow overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 6)
	(put-string port "abcd"))
    => '(10 "012345abcd"))

  (check	;overflow overwrite, empty buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 6)
	(put-string port "abcd"))
    => '(10 "012345abcd"))

  (check   ;overflow overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 6)
	(put-string port "abcdef"))
    => '(12 "012345abcdef"))

  (check	;full overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 0)
	(put-string port "abcdefgh"))
;;;                       01234567
    => '(8 "abcdefgh"))

  (check	;full overwrite, empty buffer, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 0)
	(put-string port "abcdefgh"))
;;;                       01234567
    => '(8 "abcdefgh"))

  (check       ;full overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 0)
	(put-string port "abcdefghil"))
;;;                       0123456789
    => '(10 "abcdefghil"))

  #t)


(parametrise ((check-test-name	'open-bytevector-input-port))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;argument is not a bytevector
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-bytevector-input-port 123))
    => '(123))

  (check	;argument is not a transcoder
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-bytevector-input-port '#vu8(1) 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (port-has-port-position? (open-bytevector-input-port '#vu8()))
    => #t)

  (check
      (port-has-set-port-position!? (open-bytevector-input-port '#vu8()))
    => #t)

;;; --------------------------------------------------------------------
;;; reading bytes

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(lookahead-u8 port))
    => (eof-object))

  (check
      (let ((port (open-bytevector-input-port '#vu8(12))))
	(lookahead-u8 port))
    => 12)

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(get-u8 port))
    => (eof-object))

  (check
      (let ((port (open-bytevector-input-port '#vu8(12))))
	(get-u8 port))
    => 12)

  (check
      (let ((port (open-bytevector-input-port '#vu8(12))))
	(get-u8 port)
	(get-u8 port))
    => (eof-object))

;;; --------------------------------------------------------------------
;;; reading bytevectors

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(get-bytevector-n port 3))
    => (eof-object))

  (check
      (check.with-result
	  (let ((port (open-bytevector-input-port '#vu8())))
	    (check.add-result (get-bytevector-n port 0))
	    (eof-object? (get-u8 port))))
    => '(#t (#vu8())))

  (check
      (check.with-result
	  (let ((port (open-bytevector-input-port '#vu8(0 1 2))))
	    (check.add-result (get-bytevector-n port 3))
	    (eof-object? (get-u8 port))))
    => '(#t (#vu8(0 1 2))))

  (check
      (check.with-result
	  (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5))))
	    (check.add-result (get-bytevector-n port 3))
	    (check.add-result (get-bytevector-n port 3))
	    (eof-object? (get-u8 port))))
    => '(#t (#vu8(0 1 2) #vu8(3 4 5))))

;;; --------------------------------------------------------------------
;;; getting position

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(port-position port))
    => 0)

  (check
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(port-position port))
    => 0)

  (check
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(get-bytevector-n port 5)
	(port-position port))
    => 5)

  (check
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(get-bytevector-n port 10)
	(port-position port))
    => 10)

;;; --------------------------------------------------------------------
;;; setting position

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(set-port-position! port 0)
	(port-position port))
    => 0)

  (check
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(set-port-position! port 10)
	(port-position port))
    => 10)

  (check
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(set-port-position! port 5)
	(port-position port))
    => 5)

  (check
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(set-port-position! port 5)
	(set-port-position! port 1)
	(set-port-position! port 9)
	(port-position port))
    => 9)

  (check
      (check.with-result
	  (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	    (set-port-position! port 5)
	    (check.add-result (get-bytevector-n port 5))
	    (port-position port)))
    => '(10 (#vu8(5 6 7 8 9))))

  #t)


(parametrise ((check-test-name	'open-string-input-port))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;argument is not a string
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-string-input-port 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (port-has-port-position? (open-string-input-port ""))
    => #t)

  (check
      (port-has-set-port-position!? (open-string-input-port ""))
    => #t)

;;; --------------------------------------------------------------------
;;; reading bytes

  (check
      (let ((port (open-string-input-port "")))
	(lookahead-char port))
    => (eof-object))

  (check
      (let ((port (open-string-input-port "12")))
	(lookahead-char port))
    => #\1)

  (check
      (let ((port (open-string-input-port "")))
	(get-char port))
    => (eof-object))

  (check
      (let ((port (open-string-input-port "12")))
	(get-char port))
    => #\1)

  (check
      (let ((port (open-string-input-port "1")))
	(get-char port)
	(get-char port))
    => (eof-object))

;;; --------------------------------------------------------------------
;;; reading strings

  (check
      (let ((port (open-string-input-port "")))
	(get-string-n port 3))
    => (eof-object))

  (check
      (check.with-result
	  (let ((port (open-string-input-port "")))
	    (check.add-result (get-string-n port 0))
	    (eof-object? (get-char port))))
    => '(#t ("")))

  (check
      (check.with-result
	  (let ((port (open-string-input-port "012")))
	    (check.add-result (get-string-n port 3))
	    (eof-object? (get-char port))))
    => '(#t ("012")))

  (check
      (check.with-result
	  (let ((port (open-string-input-port "012345")))
	    (check.add-result (get-string-n port 3))
	    (check.add-result (get-string-n port 3))
	    (eof-object? (get-char port))))
    => '(#t ("012" "345")))

;;; --------------------------------------------------------------------
;;; getting position

  (check
      (let ((port (open-string-input-port "")))
	(port-position port))
    => 0)

  (check
      (let ((port (open-string-input-port "0123456789")))
	(port-position port))
    => 0)

  (check
      (let ((port (open-string-input-port "0123456789")))
	(get-string-n port 5)
	(port-position port))
    => 5)

  (check
      (let ((port (open-string-input-port "0123456789")))
	(get-string-n port 10)
	(port-position port))
    => 10)

;;; --------------------------------------------------------------------
;;; setting position

  (check
      (let ((port (open-string-input-port "")))
	(set-port-position! port 0)
	(port-position port))
    => 0)

  (check
      (let ((port (open-string-input-port "0123456789")))
	(set-port-position! port 10)
	(port-position port))
    => 10)

  (check
      (let ((port (open-string-input-port "0123456789")))
	(set-port-position! port 5)
	(port-position port))
    => 5)

  (check
      (let ((port (open-string-input-port "0123456789")))
	(set-port-position! port 5)
	(set-port-position! port 1)
	(set-port-position! port 9)
	(port-position port))
    => 9)

  (check
      (check.with-result
	  (let ((port (open-string-input-port "0123456789")))
	    (set-port-position! port 5)
	    (check.add-result (get-string-n port 5))
	    (port-position port)))
    => '(10 ("56789")))

  #t)


(parametrise ((check-test-name	'with-input-from-string))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;argument is not a string
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-input-from-string 123 values))
    => '(123))

  (check	;argument is not a thunk
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-input-from-string "ciao" 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (with-input-from-string ""
	(lambda ()
	  (port-has-port-position? (current-input-port))))
    => #t)

  (check
      (with-input-from-string ""
	(lambda ()
	  (port-has-set-port-position!? (current-input-port))))
    => #t)

;;; --------------------------------------------------------------------
;;; reading bytes

  (check
      (with-input-from-string ""
	(lambda ()
	  (lookahead-char (current-input-port))))
    => (eof-object))

  (check
      (with-input-from-string "12"
	(lambda ()
	  (lookahead-char (current-input-port))))
    => #\1)

  (check
      (with-input-from-string ""
	(lambda ()
	  (get-char (current-input-port))))
    => (eof-object))

  (check
      (with-input-from-string "12"
	(lambda ()
	  (get-char (current-input-port))))
    => #\1)

  (check
      (with-input-from-string "1"
	(lambda ()
	  (get-char (current-input-port))
	  (get-char (current-input-port))))
    => (eof-object))

;;; --------------------------------------------------------------------
;;; reading strings

  (check
      (with-input-from-string ""
	(lambda ()
	  (get-string-n (current-input-port) 3)))
    => (eof-object))

  (check
      (check.with-result
	  (with-input-from-string ""
	    (lambda ()
	      (check.add-result (get-string-n (current-input-port) 0))
	      (eof-object? (get-char (current-input-port))))))
    => '(#t ("")))

  (check
      (check.with-result
	  (with-input-from-string "012"
	    (lambda ()
	      (check.add-result (get-string-n (current-input-port) 3))
	      (eof-object? (get-char (current-input-port))))))
    => '(#t ("012")))

  (check
      (check.with-result
	  (with-input-from-string "012345"
	    (lambda ()
	      (check.add-result (get-string-n (current-input-port) 3))
	      (check.add-result (get-string-n (current-input-port) 3))
	      (eof-object? (get-char (current-input-port))))))
    => '(#t ("012" "345")))

;;; --------------------------------------------------------------------
;;; getting position

  (check
      (with-input-from-string ""
	(lambda ()
	  (port-position (current-input-port))))
    => 0)

  (check
      (with-input-from-string "0123456789"
	(lambda ()
	  (port-position (current-input-port))))
    => 0)

  (check
      (with-input-from-string "0123456789"
	(lambda ()
	  (get-string-n (current-input-port) 5)
	  (port-position (current-input-port))))
    => 5)

  (check
      (with-input-from-string "0123456789"
	(lambda ()
	  (get-string-n (current-input-port) 10)
	  (port-position (current-input-port))))
    => 10)

;;; --------------------------------------------------------------------
;;; setting position

  (check
      (with-input-from-string ""
	(lambda ()
	  (set-port-position! (current-input-port) 0)
	  (port-position (current-input-port))))
    => 0)

  (check
      (with-input-from-string "0123456789"
	(lambda ()
	  (set-port-position! (current-input-port) 10)
	  (port-position (current-input-port))))
    => 10)

  (check
      (with-input-from-string "0123456789"
	(lambda ()
	  (set-port-position! (current-input-port) 5)
	  (port-position (current-input-port))))
    => 5)

  (check
      (with-input-from-string "0123456789"
	(lambda ()
	  (set-port-position! (current-input-port) 5)
	  (set-port-position! (current-input-port) 1)
	  (set-port-position! (current-input-port) 9)
	  (port-position (current-input-port))))
    => 9)

  (check
      (check.with-result
	  (with-input-from-string "0123456789"
	    (lambda ()
	      (set-port-position! (current-input-port) 5)
	      (check.add-result (get-string-n (current-input-port) 5))
	      (port-position (current-input-port)))))
    => '(10 ("56789")))

  #t)


(parametrise ((check-test-name			'open-bytevector-output-port)
	      (bytevector-port-buffer-size	8))

  (define-syntax snapshot-position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx)
	 (with-syntax ((PORT	(datum->syntax #'?ctx 'port))
		       (EXTRACT (datum->syntax #'?ctx 'extract)))
	   #'(let ((pos (port-position PORT)))
	       (list pos (EXTRACT))))))))

  (define-syntax position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx ?port . ?body)
	 #'(let-values (((?port extract) (open-bytevector-output-port)))
	     (begin . ?body)
	     (let ((pos (port-position ?port)))
	       (list pos (extract))))))))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;argument is not a transcoder
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let-values (((port extract) (open-bytevector-output-port 123)))
	  #f))
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(port-has-port-position? port))
    => #t)

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(port-has-set-port-position!? port))
    => #t)

;;; --------------------------------------------------------------------
;;; writing data, single extraction, no transcoder

  (check	;single byte
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-u8 port 65)
	(extract))
    => '#vu8(65))

  (check	;byte by byte until the buffer is full
      (let-values (((port extract) (open-bytevector-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 9 i))
	  (put-u8 port 65))
	(extract))
    => '#vu8(65 65 65  65 65 65  65 65 65))

  (check	;single bytevecor not filling the buffer
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3 4 5))
	(extract))
    => '#vu8(1 2 3 4 5))

  (check	;single bytevecor filling the buffer
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(extract))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check	;bytevector by bytevector until the buffer is full
      (let-values (((port extract) (open-bytevector-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 3 i))
	  (put-bytevector port '#vu8(1 2 3)))
	(extract))
    => '#vu8(1 2 3  1 2 3  1 2 3))

  (check	;fill the buffer multiple times
      (let-values (((port extract) (open-bytevector-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 5 i))
	  (put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9)))
	(extract))
    => '#vu8( ;;
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; writing data, multiple extraction, no transcoder

  (check	;empty device, data in buffer
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2))
	(let ((result (snapshot-position-and-contents)))
	  (put-bytevector port '#vu8(3 4 5 6))
	  (list result (snapshot-position-and-contents))))
    => '((3 #vu8(0 1 2))
	 (4 #vu8(3 4 5 6))))

  (check	;empty buffer, data in device
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(flush-output-port port)
	(let ((result (snapshot-position-and-contents)))
	  (put-bytevector port '#vu8(10 11 12 13 14 15 16 17 18 19))
	  (flush-output-port port)
	  (list result (snapshot-position-and-contents))))
    => '((10 #vu8(0 1 2 3 4 5 6 7 8 9))
	 (10 #vu8(10 11 12 13 14 15 16 17 18 19))))

  (check	;some data in device, some data in buffer
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(let ((result (snapshot-position-and-contents)))
	  (put-bytevector port '#vu8(10 11 12 13 14 15 16 17 18 19))
	  (list result (snapshot-position-and-contents))))
    => '((10 #vu8(0 1 2 3 4 5 6 7 8 9))
	 (10 #vu8(10 11 12 13 14 15 16 17 18 19))))

;;; --------------------------------------------------------------------
;;; getting port position, no transcoder

  (check	;empty device
      (let-values (((port extract) (open-bytevector-output-port)))
	(port-position port))
    => 0)

  (check	;some data in buffer, none in device
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2))
	(port-position port))
    => 3)

  (check	;buffer full, no data in device
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(port-position port))
    => 8)

  (check	;some data in buffer, some data in device
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(put-bytevector port '#vu8(8 9 10 11))
	(port-position port))
    => 12)

  (check	;buffer empty, data in device
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(port-position port))
    => 8)

;;; --------------------------------------------------------------------
;;; setting port position, no overwriting, no transcoder

  (check	;empty device
      (position-and-contents port
	(set-port-position! port 0))
    => '(0 #vu8()))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2))
	(set-port-position! port 1))
    => '(1 #vu8(0 1 2)))

  (check   ;Some data  in buffer, none in device.   Move position in the
	   ;middle, then again at the end.
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2))
	(set-port-position! port 1)
	(set-port-position! port 3))
    => '(3 #vu8(0 1 2)))

  (check ;Buffer full, no data  in device.  Move position in the middle,
	 ;then again at the end.
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 1)
	(set-port-position! port 8))
    => '(8 #vu8(0 1 2 3 4 5 6 7)))

  (check	;Some  data  in  buffer,  some  data  in  device.   Move
		;position in the middle.
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(put-bytevector port '#vu8(8 9 10 11))
	(set-port-position! port 6))
    => '(6 #vu8(0 1 2 3 4 5 6 7 8 9 10 11)))

  (check	;Buffer  empty, data  in device.   Move position  in the
		;middle.
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(flush-output-port port)
	(set-port-position! port 6))
    => '(6 #vu8(0 1 2 3 4 5 6 7)))

;;; --------------------------------------------------------------------
;;; setting port position, overwriting, no transcoder

  (check	;empty buffer, empty device
      (position-and-contents port
	(set-port-position! port 0)
	(put-bytevector port '#vu8()))
    => '(0 #vu8()))

  (check	;partial internal overwrite, data in buffer, empty device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 2)
	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7)))

  (check	;partial internal overwrite, empty buffer, data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(set-port-position! port 2)
	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7)))

  (check	;partial internal overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 2)
	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7 8 9)))

  (check	;overflow overwrite, data in buffer, empty device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 6)
	(put-bytevector port '#vu8(60 70 80 90)))
    => '(10 #vu8(0 1 2 3 4 5 60 70 80 90)))

  (check	;overflow overwrite, empty buffer, empty device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(set-port-position! port 6)
	(put-bytevector port '#vu8(60 70 80 90)))
    => '(10 #vu8(0 1 2 3 4 5 60 70 80 90)))

  (check	;overflow overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 6)
	(put-bytevector port '#vu8(60 70 80 90 100 110)))
    => '(12 #vu8(0 1 2 3 4 5 60 70 80 90 100 110)))

  (check	;full overwrite, data in buffer, empty device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 0)
	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17)))
    => '(8 #vu8(10 11 12 13 14 15 16 17)))

  (check	;full overwrite, empty buffer, data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(set-port-position! port 0)
	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17)))
    => '(8 #vu8(10 11 12 13 14 15 16 17)))

  (check	;full overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 0)
	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17 18 19)))
    => '(10 #vu8(10 11 12 13 14 15 16 17 18 19)))

  #t)


(parametrise ((check-test-name			'call-with-bytevector-output-port)
	      (bytevector-port-buffer-size	8))

  (define-syntax position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx ?port . ?body)
	 #'(let ((result (check.with-result
			     (call-with-bytevector-output-port
				 (lambda (?port)
				   (begin . ?body)
				   (check.add-result (port-position ?port)))))))
	     (list (caadr result) (car result)))))))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;argument is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-bytevector-output-port 123 #f))
    => '(123))

  (check	;argument is not a transcoder
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-bytevector-output-port (lambda (port) #f) 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (check.with-result
	  (call-with-bytevector-output-port
	      (lambda (port)
		(check.add-result (port-has-port-position? port)))))
    => '(#vu8() (#t)))

  (check
      (check.with-result
	  (call-with-bytevector-output-port
	      (lambda (port)
		(check.add-result (port-has-set-port-position!? port)))))
    => '(#vu8() (#t)))

;;; --------------------------------------------------------------------
;;; writing data, no transcoder

  (check	;single byte
      (call-with-bytevector-output-port
	  (lambda (port)
	    (put-u8 port 65)))
    => '#vu8(65))

  (check	;byte by byte until the buffer is full
      (call-with-bytevector-output-port
	  (lambda (port)
	    (do ((i 0 (+ 1 i)))
		((= 9 i))
	      (put-u8 port 65))))
    => '#vu8(65 65 65  65 65 65  65 65 65))

  (check	;single bytevecor not filling the buffer
      (call-with-bytevector-output-port
	  (lambda (port)
	    (put-bytevector port '#vu8(1 2 3 4 5))))
    => '#vu8(1 2 3 4 5))

  (check	;single bytevecor filling the buffer
      (call-with-bytevector-output-port
	  (lambda (port)
	    (put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check	;bytevector by bytevector until the buffer is full
      (call-with-bytevector-output-port
	  (lambda (port)
	    (do ((i 0 (+ 1 i)))
		((= 3 i))
	      (put-bytevector port '#vu8(1 2 3)))))
    => '#vu8(1 2 3  1 2 3  1 2 3))

  (check	;fill the buffer multiple times
      (call-with-bytevector-output-port
	  (lambda (port)
	    (do ((i 0 (+ 1 i)))
		((= 5 i))
	      (put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9)))))
    => '#vu8( ;;
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9
	     0 1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; getting port position, no transcoder

  (check	;empty device
      (position-and-contents port
	(values))
    => '(0 #vu8()))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2)))
    => '(3 #vu8(0 1 2)))

  (check	;buffer full, no data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7)))
    => '(8 #vu8(0 1 2 3 4 5 6 7)))

  (check	;some data in buffer, some data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(put-bytevector port '#vu8(8 9 10 11)))
    => '(12 #vu8(0 1 2 3 4 5 6 7 8 9 10 11)))

  (check	;buffer empty, data in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port))
    => '(8 #vu8(0 1 2 3 4 5 6 7)))

;;; --------------------------------------------------------------------
;;; setting port position, no overwriting, no transcoder

  (check	;empty device
      (position-and-contents port
	(set-port-position! port 0))
    => '(0 #vu8()))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2))
	(set-port-position! port 1))
    => '(1 #vu8(0 1 2)))

  (check   ;Some data  in buffer, none in device.   Move position in the
	   ;middle, then again at the end.
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2))
	(set-port-position! port 1)
	(set-port-position! port 3))
    => '(3 #vu8(0 1 2)))

  (check ;Bufffer full, no data in device.  Move position in the middle,
	 ;then again at the end.
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(set-port-position! port 1)
	(set-port-position! port 8))
    => '(8 #vu8(0 1 2 3 4 5 6 7)))

  (check	;Some  data  in  buffer,  some  data  in  device.   Move
		;position in the middle.
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(put-bytevector port '#vu8(8 9 10 11))
	(set-port-position! port 6))
    => '(6 #vu8(0 1 2 3 4 5 6 7 8 9 10 11)))

  (check	;Buffer  empty, data  in device.   Move position  in the
  		;middle.
      (position-and-contents port
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(flush-output-port port)
	(set-port-position! port 6))
    => '(6 #vu8(0 1 2 3 4 5 6 7)))

;;; --------------------------------------------------------------------
;;; setting port position, overwriting, no transcoder

  (check	;empty buffer, empty device
      (position-and-contents port
	(set-port-position! port 0)
  	(put-bytevector port '#vu8()))
    => '(0 #vu8()))

  (check       ;partial internal overwrite, data in buffer, empty device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(set-port-position! port 2)
  	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7)))

  (check       ;partial internal overwrite, empty buffer, data in device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(flush-output-port port)
  	(set-port-position! port 2)
  	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7)))

  (check ;partial internal overwrite, some data in buffer, some data in device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
  	(set-port-position! port 2)
  	(put-bytevector port '#vu8(20 30 40)))
    => '(5 #vu8(0 1 20 30 40 5 6 7 8 9)))

  (check	;overflow overwrite, data in buffer, empty device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(set-port-position! port 6)
  	(put-bytevector port '#vu8(60 70 80 90)))
    => '(10 #vu8(0 1 2 3 4 5 60 70 80 90)))

  (check	;overflow overwrite, empty buffer, empty device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(flush-output-port port)
  	(set-port-position! port 6)
  	(put-bytevector port '#vu8(60 70 80 90)))
    => '(10 #vu8(0 1 2 3 4 5 60 70 80 90)))

  (check   ;overflow overwrite, some data in buffer, some data in device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
  	(set-port-position! port 6)
  	(put-bytevector port '#vu8(60 70 80 90 100 110)))
    => '(12 #vu8(0 1 2 3 4 5 60 70 80 90 100 110)))

  (check	;full overwrite, data in buffer, empty device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(set-port-position! port 0)
  	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17)))
    => '(8 #vu8(10 11 12 13 14 15 16 17)))

  (check	;full overwrite, empty buffer, data in device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
  	(flush-output-port port)
  	(set-port-position! port 0)
  	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17)))
    => '(8 #vu8(10 11 12 13 14 15 16 17)))

  (check       ;full overwrite, some data in buffer, some data in device
      (position-and-contents port
  	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
  	(set-port-position! port 0)
  	(put-bytevector port '#vu8(10 11 12 13 14 15 16 17 18 19)))
    => '(10 #vu8(10 11 12 13 14 15 16 17 18 19)))

  #t)


(parametrise ((check-test-name		'open-string-output-port)
	      (string-port-buffer-size	8))

  (define-syntax snapshot-position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx)
	 (with-syntax ((PORT	(datum->syntax #'?ctx 'port))
		       (EXTRACT (datum->syntax #'?ctx 'extract)))
	   #'(let ((pos (port-position PORT)))
	       (list pos (EXTRACT))))))))

  (define-syntax position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx ?port . ?body)
	 #'(let-values (((?port extract) (open-string-output-port)))
	     (begin . ?body)
	     (let ((pos (port-position ?port)))
	       (list pos (extract))))))))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (let-values (((port extract) (open-string-output-port)))
	(port-has-port-position? port))
    => #t)

  (check
      (let-values (((port extract) (open-string-output-port)))
	(port-has-set-port-position!? port))
    => #t)

;;; --------------------------------------------------------------------
;;; writing data, single extraction

  (check	;single char
      (let-values (((port extract) (open-string-output-port)))
	(put-char port #\A)
	(extract))
    => "A")

  (check	;char by char until the buffer is full
      (let-values (((port extract) (open-string-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 9 i)
	     (extract))
	  (put-char port #\A)))
    => "AAAAAAAAA")

  (check	;single string not filling the buffer
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "12345")
	(extract))
    => "12345")

  (check	;single string filling the buffer
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "0123456789")
	(extract))
    => "0123456789")

  (check	;string by string until the buffer is full
      (let-values (((port extract) (open-string-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 3 i)
	     (extract))
	  (put-string port "123")))
    => "123123123")

  (check	;fill the buffer multiple times
      (let-values (((port extract) (open-string-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 5 i)
	     (extract))
	  (put-string port "0123456789")))
    => "0123456789\
        0123456789\
        0123456789\
        0123456789\
        0123456789")

;;; --------------------------------------------------------------------
;;; writing data, multiple extraction

  (check	;empty device, data in buffer
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "012")
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "3456")
	  (list result (snapshot-position-and-contents))))
    => '((3 "012")
	 (4 "3456")))

  (check	;empty buffer, data in device
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "0123456789")
	(flush-output-port port)
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "abcdefghil")
	  (flush-output-port port)
	  (list result (snapshot-position-and-contents))))
    => '((10 "0123456789")
	 (10 "abcdefghil")))

  (check	;some data in device, some data in buffer
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "0123456789")
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "abcdefghil")
	  (list result (snapshot-position-and-contents))))
    => '((10 "0123456789")
	 (10 "abcdefghil")))

;;; --------------------------------------------------------------------
;;; getting port position

  (check	;empty device
      (let-values (((port extract) (open-string-output-port)))
	(port-position port))
    => 0)

  (check	;some data in buffer, none in device
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "012")
	(port-position port))
    => 3)

  (check	;buffer full, no data in device
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "01234567")
	(port-position port))
    => 8)

  (check	;some data in buffer, some data in device
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "01234567")
	(put-string port "89ab")
	(port-position port))
    => 12)

  (check	;buffer empty, data in device
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "01234567")
	(flush-output-port port)
	(port-position port))
    => 8)

;;; --------------------------------------------------------------------
;;; setting port position, no overwriting

  (check	;empty device
      (position-and-contents port
	(set-port-position! port 0))
    => '(0 ""))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-string port "012")
	(set-port-position! port 1))
    => '(1 "012"))

  (check   ;Some data  in buffer, none in device.   Move position in the
	   ;middle, then again at the end.
      (position-and-contents port
	(put-string port "012")
	(set-port-position! port 1)
	(set-port-position! port 3))
    => '(3 "012"))

  (check ;Buffer full, no data  in device.  Move position in the middle,
	 ;then again at the end.
      (position-and-contents port
  	(put-string port "01234567")
	(set-port-position! port 1)
	(set-port-position! port 8))
    => '(8 "01234567"))

  (check	;Some  data  in  buffer,  some  data  in  device.   Move
		;position in the middle.
      (position-and-contents port
  	(put-string port "01234567")
  	(put-string port "89ab")
	(set-port-position! port 6))
    => '(6 "0123456789ab"))

  (check	;Buffer  empty, data  in device.   Move position  in the
		;middle.
      (position-and-contents port
  	(put-string port "01234567")
  	(flush-output-port port)
	(set-port-position! port 6))
    => '(6 "01234567"))

;;; --------------------------------------------------------------------
;;; setting port position, overwriting

  (check	;empty buffer, empty device
      (position-and-contents port
	(set-port-position! port 0)
	(put-string port ""))
    => '(0 ""))

  (check	;partial internal overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc567"))

  (check	;partial internal overwrite, empty buffer, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc567"))

  (check	;partial internal overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc56789"))

  (check	;overflow overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 6)
	(put-string port "abcd"))
    => '(10 "012345abcd"))

  (check	;overflow overwrite, empty buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 6)
	(put-string port "abcd"))
    => '(10 "012345abcd"))

  (check	;overflow overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 6)
	(put-string port "abcdef"))
    => '(12 "012345abcdef"))

  (check	;full overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 0)
	(put-string port "abcdefgh"))
;;;                       01234567
    => '(8 "abcdefgh"))

  (check	;full overwrite, empty buffer, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 0)
	(put-string port "abcdefgh"))
;;;                       01234567
    => '(8 "abcdefgh"))

  (check	;full overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 0)
	(put-string port "abcdefghil"))
;;;                       0123456789
    => '(10 "abcdefghil"))

;;; --------------------------------------------------------------------

  ;;Commented out because it takes to much memory.
  ;;
  ;; (check
  ;;     (guard (E ((assertion-violation? E)
  ;; 		 (pretty-print (condition-message E))
  ;; 		 (condition-irritants E))
  ;; 		(else E))
  ;; 	(let-values (((port extract) (open-string-output-port)))
  ;; 	  (let ((half (div (greatest-fixnum) 2)))
  ;; 	    (put-string port (make-string half))
  ;; 	    (put-string port (make-string (+ 1 half))))))
  ;;   => (list (+ 1 (greatest-fixnum))))

  #t)


(parametrise ((check-test-name		'get-output-string)
	      (string-port-buffer-size	8))

  (define-syntax snapshot-position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx)
	 (with-syntax ((PORT	(datum->syntax #'?ctx 'port)))
	   #'(let ((pos (port-position PORT)))
	       (list pos (get-output-string PORT))))))))

  (define-syntax position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx ?port . ?body)
	 #'(let-values (((?port extract) (open-string-output-port)))
	     (begin . ?body)
	     (let ((pos (port-position ?port)))
	       (list pos (get-output-string ?port))))))))

;;; --------------------------------------------------------------------
;;; writing data, single extraction

  (check	;single char
      (let-values (((port extract) (open-string-output-port)))
	(put-char port #\A)
	(get-output-string port))
    => "A")

  (check	;char by char until the buffer is full
      (let-values (((port extract) (open-string-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 9 i)
	     (get-output-string port))
	  (put-char port #\A)))
    => "AAAAAAAAA")

  (check	;single string not filling the buffer
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "12345")
	(get-output-string port))
    => "12345")

  (check	;single string filling the buffer
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "0123456789")
	(get-output-string port))
    => "0123456789")

  (check	;string by string until the buffer is full
      (let-values (((port extract) (open-string-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 3 i)
	     (get-output-string port))
	  (put-string port "123")))
    => "123123123")

  (check	;fill the buffer multiple times
      (let-values (((port extract) (open-string-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= 5 i)
	     (get-output-string port))
	  (put-string port "0123456789")))
    => "0123456789\
        0123456789\
        0123456789\
        0123456789\
        0123456789")

;;; --------------------------------------------------------------------
;;; writing data, multiple extraction

  (check	;empty device, data in buffer
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "012")
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "3456")
	  (list result (snapshot-position-and-contents))))
    => '((3 "012")
	 (4 "3456")))

  (check	;empty buffer, data in device
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "0123456789")
	(flush-output-port port)
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "abcdefghil")
	  (flush-output-port port)
	  (list result (snapshot-position-and-contents))))
    => '((10 "0123456789")
	 (10 "abcdefghil")))

  (check	;some data in device, some data in buffer
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "0123456789")
	(let ((result (snapshot-position-and-contents)))
	  (put-string port "abcdefghil")
	  (list result (snapshot-position-and-contents))))
    => '((10 "0123456789")
	 (10 "abcdefghil")))

;;; --------------------------------------------------------------------
;;; setting port position, no overwriting

  (check	;empty device
      (position-and-contents port
	(set-port-position! port 0))
    => '(0 ""))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-string port "012")
	(set-port-position! port 1))
    => '(1 "012"))

  (check   ;Some data  in buffer, none in device.   Move position in the
	   ;middle, then again at the end.
      (position-and-contents port
	(put-string port "012")
	(set-port-position! port 1)
	(set-port-position! port 3))
    => '(3 "012"))

  (check ;Buffer full, no data  in device.  Move position in the middle,
	 ;then again at the end.
      (position-and-contents port
  	(put-string port "01234567")
	(set-port-position! port 1)
	(set-port-position! port 8))
    => '(8 "01234567"))

  (check	;Some  data  in  buffer,  some  data  in  device.   Move
		;position in the middle.
      (position-and-contents port
  	(put-string port "01234567")
  	(put-string port "89ab")
	(set-port-position! port 6))
    => '(6 "0123456789ab"))

  (check	;Buffer  empty, data  in device.   Move position  in the
		;middle.
      (position-and-contents port
  	(put-string port "01234567")
  	(flush-output-port port)
	(set-port-position! port 6))
    => '(6 "01234567"))

;;; --------------------------------------------------------------------
;;; setting port position, overwriting

  (check	;empty buffer, empty device
      (position-and-contents port
	(set-port-position! port 0)
	(put-string port ""))
    => '(0 ""))

  (check	;partial internal overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc567"))

  (check	;partial internal overwrite, empty buffer, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc567"))

  (check	;partial internal overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc56789"))

  (check	;overflow overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 6)
	(put-string port "abcd"))
    => '(10 "012345abcd"))

  (check	;overflow overwrite, empty buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 6)
	(put-string port "abcd"))
    => '(10 "012345abcd"))

  (check	;overflow overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 6)
	(put-string port "abcdef"))
    => '(12 "012345abcdef"))

  (check	;full overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 0)
	(put-string port "abcdefgh"))
;;;                       01234567
    => '(8 "abcdefgh"))

  (check	;full overwrite, empty buffer, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 0)
	(put-string port "abcdefgh"))
;;;                       01234567
    => '(8 "abcdefgh"))

  (check	;full overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 0)
	(put-string port "abcdefghil"))
;;;                       0123456789
    => '(10 "abcdefghil"))

  #t)


(parametrise ((check-test-name		'call-with-string-output-port)
	      (string-port-buffer-size	8))

  (define-syntax position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx ?port . ?body)
	 #'(let ((result (check.with-result
			     (call-with-string-output-port
				 (lambda (?port)
				   (begin . ?body)
				   (check.add-result (port-position ?port)))))))
	     (list (caadr result) (car result)))))))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;argument is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-string-output-port 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (check.with-result
	  (call-with-string-output-port
	      (lambda (port)
		(check.add-result (port-has-port-position? port)))))
    => '("" (#t)))

  (check
      (check.with-result
	  (call-with-string-output-port
	      (lambda (port)
		(check.add-result (port-has-set-port-position!? port)))))
    => '("" (#t)))

;;; --------------------------------------------------------------------
;;; writing data, single extraction

  (check	;single char
      (call-with-string-output-port
	  (lambda (port)
	    (put-char port #\A)))
    => "A")

  (check	;char by char until the buffer is full
      (call-with-string-output-port
	  (lambda (port)
	    (do ((i 0 (+ 1 i)))
		((= 9 i))
	      (put-char port #\A))))
    => "AAAAAAAAA")

  (check	;single string not filling the buffer
      (call-with-string-output-port
	  (lambda (port)
	    (put-string port "12345")))
    => "12345")

  (check	;single string filling the buffer
      (call-with-string-output-port
	  (lambda (port)
	    (put-string port "0123456789")))
    => "0123456789")

  (check	;string by string until the buffer is full
      (call-with-string-output-port
	  (lambda (port)
	    (do ((i 0 (+ 1 i)))
		((= 3 i))
	      (put-string port "123"))))
    => "123123123")

  (check	;fill the buffer multiple times
      (call-with-string-output-port
	  (lambda (port)
	    (do ((i 0 (+ 1 i)))
		((= 5 i))
	      (put-string port "0123456789"))))
    => "0123456789\
        0123456789\
        0123456789\
        0123456789\
        0123456789")

;;; --------------------------------------------------------------------
;;; getting port position

  (check	;empty device
      (position-and-contents port
	(values))
    => '(0 ""))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-string port "012"))
    => '(3 "012"))

  (check	;buffer full, no data in device
      (position-and-contents port
	(put-string port "01234567"))
    => '(8 "01234567"))

  (check	;some data in buffer, some data in device
      (position-and-contents port
	(put-string port "01234567")
	(put-string port "89ab"))
    => '(12 "0123456789ab"))

  (check	;buffer empty, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port))
    => '(8 "01234567"))

;;; --------------------------------------------------------------------
;;; setting port position, no overwriting

  (check	;empty device
      (position-and-contents port
	(set-port-position! port 0))
    => '(0 ""))

  (check	;some data in buffer, none in device
      (position-and-contents port
	(put-string port "012")
	(set-port-position! port 1))
    => '(1 "012"))

  (check   ;Some data  in buffer, none in device.   Move position in the
	   ;middle, then again at the end.
      (position-and-contents port
	(put-string port "012")
	(set-port-position! port 1)
	(set-port-position! port 3))
    => '(3 "012"))

  (check ;Buffer full, no data  in device.  Move position in the middle,
	 ;then again at the end.
      (position-and-contents port
  	(put-string port "01234567")
	(set-port-position! port 1)
	(set-port-position! port 8))
    => '(8 "01234567"))

  (check	;Some  data  in  buffer,  some  data  in  device.   Move
		;position in the middle.
      (position-and-contents port
  	(put-string port "01234567")
  	(put-string port "89ab")
	(set-port-position! port 6))
    => '(6 "0123456789ab"))

  (check	;Buffer  empty, data  in device.   Move position  in the
		;middle.
      (position-and-contents port
  	(put-string port "01234567")
  	(flush-output-port port)
	(set-port-position! port 6))
    => '(6 "01234567"))

;;; --------------------------------------------------------------------
;;; setting port position, overwriting

  (check	;empty buffer, empty device
      (position-and-contents port
	(set-port-position! port 0)
	(put-string port ""))
    => '(0 ""))

  (check	;partial internal overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc567"))

  (check	;partial internal overwrite, empty buffer, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc567"))

  (check	;partial internal overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 2)
	(put-string port "abc"))
    => '(5 "01abc56789"))

  (check	;overflow overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 6)
	(put-string port "abcd"))
    => '(10 "012345abcd"))

  (check	;overflow overwrite, empty buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 6)
	(put-string port "abcd"))
    => '(10 "012345abcd"))

  (check	;overflow overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 6)
	(put-string port "abcdef"))
    => '(12 "012345abcdef"))

  (check	;full overwrite, data in buffer, empty device
      (position-and-contents port
	(put-string port "01234567")
	(set-port-position! port 0)
	(put-string port "abcdefgh"))
;;;                       01234567
    => '(8 "abcdefgh"))

  (check	;full overwrite, empty buffer, data in device
      (position-and-contents port
	(put-string port "01234567")
	(flush-output-port port)
	(set-port-position! port 0)
	(put-string port "abcdefgh"))
;;;                       01234567
    => '(8 "abcdefgh"))

  (check	;full overwrite, some data in buffer, some data in device
      (position-and-contents port
	(put-string port "0123456789")
	(set-port-position! port 0)
	(put-string port "abcdefghil"))
;;;                       0123456789
    => '(10 "abcdefghil"))

  #t)


(parametrise ((check-test-name		'with-output-to-string)
	      (string-port-buffer-size	8))

  (define-syntax position-and-contents
    (lambda (stx)
      (syntax-case stx ()
	((?ctx . ?body)
	 #'(let ((result (check.with-result
			     (with-output-to-string
			       (lambda ()
				 (begin . ?body)
				 (check.add-result (port-position (current-output-port))))))))
	     (list (caadr result) (car result)))))))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;argument is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-output-to-string 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; port operations support

  (check
      (check.with-result
	  (with-output-to-string
	    (lambda ()
	      (check.add-result (port-has-port-position? (current-output-port))))))
    => '("" (#t)))

  (check
      (check.with-result
	  (with-output-to-string
	    (lambda ()
	      (check.add-result (port-has-set-port-position!? (current-output-port))))))
    => '("" (#t)))

;;; --------------------------------------------------------------------
;;; writing data, single extraction

  (check	;single char
      (with-output-to-string
	(lambda ()
	  (display #\A)))
    => "A")

  (check	;char by char until the buffer is full
      (with-output-to-string
	(lambda ()
	  (do ((i 0 (+ 1 i)))
	      ((= 9 i))
	    (display #\A))))
    => "AAAAAAAAA")

  (check	;single string not filling the buffer
      (with-output-to-string
	(lambda ()
	  (display "12345")))
    => "12345")

  (check	;single string filling the buffer
      (with-output-to-string
	(lambda ()
	  (display "0123456789")))
    => "0123456789")

  (check	;string by string until the buffer is full
      (with-output-to-string
	(lambda ()
	  (do ((i 0 (+ 1 i)))
	      ((= 3 i))
	    (display "123"))))
    => "123123123")

  (check	;fill the buffer multiple times
      (with-output-to-string
	(lambda ()
	  (do ((i 0 (+ 1 i)))
	      ((= 5 i))
	    (display "0123456789"))))
    => "0123456789\
        0123456789\
        0123456789\
        0123456789\
        0123456789")

;;; --------------------------------------------------------------------
;;; getting port position

  (check	;empty device
      (position-and-contents
	  (values))
    => '(0 ""))

  (check	;some data in buffer, none in device
      (position-and-contents
	  (display "012"))
    => '(3 "012"))

  (check	;buffer full, no data in device
      (position-and-contents
	  (display "01234567"))
    => '(8 "01234567"))

  (check	;some data in buffer, some data in device
      (position-and-contents
	  (display "01234567")
	(display "89ab"))
    => '(12 "0123456789ab"))

  (check	;buffer empty, data in device
      (position-and-contents
	  (display "01234567")
	(flush-output-port (current-output-port)))
    => '(8 "01234567"))

;;; --------------------------------------------------------------------
;;; setting port position, no overwriting

  (check	;empty device
      (position-and-contents
	  (set-port-position! (current-output-port) 0))
    => '(0 ""))

  (check	;some data in buffer, none in device
      (position-and-contents
	  (display "012")
	(set-port-position! (current-output-port) 1))
    => '(1 "012"))

  (check   ;Some data  in buffer, none in device.   Move position in the
		;middle, then again at the end.
      (position-and-contents
	  (display "012")
	(set-port-position! (current-output-port) 1)
	(set-port-position! (current-output-port) 3))
    => '(3 "012"))

  (check ;Buffer full, no data  in device.  Move position in the middle,
		;then again at the end.
      (position-and-contents
	  (display "01234567")
	(set-port-position! (current-output-port) 1)
	(set-port-position! (current-output-port) 8))
    => '(8 "01234567"))

  (check	;Some  data  in  buffer,  some  data  in  device.   Move
		;position in the middle.
      (position-and-contents
	  (display "01234567")
  	(display "89ab")
	(set-port-position! (current-output-port) 6))
    => '(6 "0123456789ab"))

  (check	;Buffer  empty, data  in device.   Move position  in the
		;middle.
      (position-and-contents
	  (display "01234567")
  	(flush-output-port (current-output-port))
	(set-port-position! (current-output-port) 6))
    => '(6 "01234567"))

;;; --------------------------------------------------------------------
;;; setting port position, overwriting

  (check	;empty buffer, empty device
      (position-and-contents
	  (set-port-position! (current-output-port) 0)
	(display ""))
    => '(0 ""))

  (check       ;partial internal overwrite, data in buffer, empty device
      (position-and-contents
	  (display "01234567")
	(set-port-position! (current-output-port) 2)
	(display "abc"))
    => '(5 "01abc567"))

  (check       ;partial internal overwrite, empty buffer, data in device
      (position-and-contents
	  (display "01234567")
	(flush-output-port (current-output-port))
	(set-port-position! (current-output-port) 2)
	(display "abc"))
    => '(5 "01abc567"))

  (check ;partial internal overwrite, some data in buffer, some data in device
      (position-and-contents
	  (display "0123456789")
	(set-port-position! (current-output-port) 2)
	(display "abc"))
    => '(5 "01abc56789"))

  (check	;overflow overwrite, data in buffer, empty device
      (position-and-contents
	  (display "01234567")
	(set-port-position! (current-output-port) 6)
	(display "abcd"))
    => '(10 "012345abcd"))

  (check	;overflow overwrite, empty buffer, empty device
      (position-and-contents
	  (display "01234567")
	(flush-output-port (current-output-port))
	(set-port-position! (current-output-port) 6)
	(display "abcd"))
    => '(10 "012345abcd"))

  (check   ;overflow overwrite, some data in buffer, some data in device
      (position-and-contents
	  (display "0123456789")
	(set-port-position! (current-output-port) 6)
	(display "abcdef"))
    => '(12 "012345abcdef"))

  (check	;full overwrite, data in buffer, empty device
      (position-and-contents
	  (display "01234567")
	(set-port-position! (current-output-port) 0)
	(display "abcdefgh"))
;;;               01234567
    => '(8 "abcdefgh"))

  (check	;full overwrite, empty buffer, data in device
      (position-and-contents
	  (display "01234567")
	(flush-output-port (current-output-port))
	(set-port-position! (current-output-port) 0)
	(display "abcdefgh"))
;;;               01234567
    => '(8 "abcdefgh"))

  (check       ;full overwrite, some data in buffer, some data in device
      (position-and-contents
	  (display "0123456789")
	(set-port-position! (current-output-port) 0)
	(display "abcdefghil"))
;;;               0123456789
    => '(10 "abcdefghil"))

  #t)


(parametrise ((check-test-name	'with-output-to-port))

;;; arguments validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-output-to-port 123 (lambda () #f)))
    => '(123))

  (check	;argument is not an output port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (car (condition-irritants E)))
		(else E))
	(with-output-to-port (current-input-port) (lambda () #f)))
    (=> eq?) (current-input-port))

  (check	;argument is not a textual port
      (let-values (((port extract) (open-bytevector-output-port)))
	(guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (with-output-to-port port (lambda () #f))))
    => #t)

  (check	;argument is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-output-to-port (current-output-port) 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; textual port

  (check
      (let-values (((port extract) (open-string-output-port)))
	(with-output-to-port port
	  (lambda ()
	    (put-string (current-output-port) "123")))
	(extract))
    => "123")

  #t)


(parametrise ((check-test-name	'call-with-port))

;;; arguments validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-port 123 values))
    => '(123))

  (check	;argument is not an open port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-port port)
	  (call-with-port port values)))
    => #t)

  (check	;argument is not a procedure
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-port (current-output-port) 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; textual port

  (check
      (let-values (((port extract) (open-string-output-port)))
	(call-with-port port
	  (lambda (port)
	    (put-string port "123")))
	(list (port-closed? port) (extract)))
    => '(#t "123"))

  #t)


(parametrise ((check-test-name			'transcoded-port)
	      (bytevector-port-buffer-size	10))

;;; arguments validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(transcoded-port 123 (native-transcoder)))
    => '(123))

  (check	;argument is not a transcoder
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((bin-port (open-bytevector-input-port '#vu8())))
	  (transcoded-port bin-port 123)))
    => '(123))

;;; --------------------------------------------------------------------
;;; operation behaviour

  (check
      (let* ((bin-port	(open-bytevector-input-port '#vu8()))
	     (tran-port	(transcoded-port bin-port (native-transcoder))))
	(port-closed? bin-port))
    => #t)

  (check
      (let* ((bin-port	(open-bytevector-input-port '#vu8()))
	     (tran-port	(transcoded-port bin-port (native-transcoder))))
	(port-transcoder tran-port))
    (=> eq?) (native-transcoder))

;;; --------------------------------------------------------------------
;;; transcoding input ports

  (check
      (let* ((bin-port	(open-bytevector-input-port (string->latin1 TEST-STRING-FOR-LATIN-1)))
	     (tran-port	(transcoded-port bin-port (make-transcoder (latin-1-codec)
								   (eol-style none)
								   (error-handling-mode replace)))))
	(get-string-all tran-port))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (let* ((bin-port	(open-bytevector-input-port (string->utf8 TEST-STRING-FOR-UTF-8)))
	     (tran-port	(transcoded-port bin-port (make-transcoder (utf-8-codec)))))
	(get-string-all tran-port))
    => TEST-STRING-FOR-UTF-8)

  (check
      (let* ((bin-port	(open-bytevector-input-port (string->utf16 TEST-STRING-FOR-UTF-16-BE
								   (endianness big))))
	     (tran-port	(transcoded-port bin-port (make-transcoder (utf-16-codec)))))
	(get-string-all tran-port))
    => TEST-STRING-FOR-UTF-16-BE)

  ;;There is no UTF-32 codec.
  #;(check
      (let* ((bin-port	(open-bytevector-input-port (string->utf32 "ciao mamma     ")))
	     (tran-port	(transcoded-port bin-port (make-transcoder (utf-32-codec)))))
	(get-string-all tran-port))
    => "ciao mamma     ")

;;; --------------------------------------------------------------------
;;; transcoding output ports

  (check
      (let-values (((bin-port extract) (open-bytevector-output-port)))
	(let ((tran-port (transcoded-port bin-port (make-transcoder (latin-1-codec)
								    (eol-style none)
								    (error-handling-mode raise)))))
	  (put-string tran-port TEST-STRING-FOR-LATIN-1)
	  (latin1->string (extract))))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (let-values (((bin-port extract) (open-bytevector-output-port)))
	(let ((tran-port (transcoded-port bin-port (make-transcoder (utf-8-codec)
								    (eol-style none)
								    (error-handling-mode raise)))))
	  (put-string tran-port TEST-STRING-FOR-UTF-8)
	  (let ((bv (extract)))
	    (utf8->string bv))))
    => TEST-STRING-FOR-UTF-8)

  (check
      (let-values (((bin-port extract) (open-bytevector-output-port)))
	(let ((tran-port (transcoded-port bin-port (make-transcoder (utf-8-codec)
								    (eol-style none)
								    (error-handling-mode raise)))))
	  (put-string tran-port SPECIAL-TEST-STRING-FOR-UTF-8)
	  (let ((bv (extract)))
	    (utf8->string bv))))
    => SPECIAL-TEST-STRING-FOR-UTF-8)

  (check
      (let-values (((bin-port extract) (open-bytevector-output-port)))
	(let ((tran-port (transcoded-port bin-port (make-transcoder (utf-16-codec)
								    (eol-style none)
								    (error-handling-mode replace)))))
	  (put-string tran-port TEST-STRING-FOR-UTF-16-BE)
	  (utf16->string (extract) (endianness big))))
    => TEST-STRING-FOR-UTF-16-BE)

  (check
      (let-values (((bin-port extract) (open-bytevector-output-port)))
	(let ((tran-port (transcoded-port bin-port (make-transcoder (utf-16le-codec)
								    (eol-style none)
								    (error-handling-mode replace)))))
	  (put-string tran-port TEST-STRING-FOR-UTF-16-LE)
	  (utf16->string (extract) (endianness little))))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (let-values (((bin-port extract) (open-bytevector-output-port)))
	(let ((tran-port (transcoded-port bin-port (make-transcoder (utf-16be-codec)
								    (eol-style none)
								    (error-handling-mode replace)))))
	  (put-string tran-port TEST-STRING-FOR-UTF-16-BE)
	  (utf16->string (extract) (endianness big))))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name			'transcoded-port))

  (check
      (eqv? (utf-16n-codec) (utf-16n-codec))
    => #t)

  (check
      (eqv? (utf-16n-codec) (utf-8-codec))
    => #f)

  (check
      (eqv? (utf-16n-codec) (case (native-endianness)
			      ((big)	(utf-16be-codec))
			      ((little)	(utf-16le-codec))
			      (else	#f)))
    => #t)

  #t)


(parametrise ((check-test-name	'port-id))

  (check
      (port-id (open-string-input-port ""))
    => "*string-input-port*")

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(port-id 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'port-mode))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(port-mode 123))
    => '(123))

  (check
      (port-mode (open-bytevector-input-port '#vu8()))
    => 'vicare)

  #t)


(parametrise ((check-test-name	'set-port-mode-bang))


  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(set-port-mode! 123 'vicare))
    => '(123))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(set-port-mode! (open-bytevector-input-port '#vu8())
			123))
    => '(123))

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(set-port-mode! port 'r6rs)
	(port-mode port))
    => 'r6rs)

  #t)


(parametrise ((check-test-name	'output-port-buffer-mode))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(output-port-buffer-mode 123))
    => '(123))

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(output-port-buffer-mode port))
    => 'block)

  #t)


(parametrise ((check-test-name	'port-eof?))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(port-eof? 123))
    => '(123))

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (port-eof? port)))
    => #t)

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (port-eof? port)))
    => #t)

  (check
      (port-eof? (open-bytevector-input-port '#vu8()))
    => #t)

  (check
      (port-eof? (open-bytevector-input-port '#vu8(1 2 3)))
    => #f)

  (check
      (port-eof? (open-string-input-port ""))
    => #t)

  (check
      (port-eof? (open-string-input-port "123"))
    => #f)

  #t)


(parametrise ((check-test-name	'flush-output-port))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(flush-output-port 123))
    => '(123))

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (flush-output-port port)))
    => #t)

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-output-port port)
	  (flush-output-port port)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(flush-output-port port)
	(extract))
    => '#vu8(1 2 3))

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(flush-output-port port)
	(extract))
    => '#vu8())

  #t)


(parametrise ((check-test-name	'get-u8))

  (define (make-test-bytevector-input-port bv)
    (let ((position	0)
	  (bv.len	(bytevector-length bv))
	  (port		#f))

      (define (read! dst.bv dst.start count)
	(let* ((available	(- bv.len position))
	       (to-read		(min available count)))
	  (unless (zero? to-read)
	    (bytevector-copy! bv position dst.bv dst.start to-read)
	    (set! position (+ to-read position)))
	  to-read))

      (define (get-position)
	position)

      (define (set-position! new-position)
	(if (<= 0 new-position bv.len)
	    (set! position new-position)
	  (raise
	   (condition (make-i/o-invalid-position-error new-position)
		      (make-who-condition 'make-test-port/set-position!)
		      (make-message-condition "invalid port position")
		      (make-irritants-condition (list port))))))

      (define (close)
	(set! bv #f))

      (set! port (make-custom-binary-input-port "*test-binary-input-port*"
						read! get-position set-position! close))
      port))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(get-u8 123))
    => '(123))

  (check	;not an input port
      (let-values (((port extract) (open-bytevector-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-u8 port)))
    => #t)

  (check	;not a binary port
      (let ((port (open-string-input-port "")))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-u8 port)))
    => #t)

  (check	;not a binary port (with transcoded port)
      (let* ((bin-port  (open-bytevector-input-port '#vu8()))
	     (tran-port (transcoded-port bin-port (native-transcoder))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? tran-port (car (condition-irritants E))))
		  (else E))
	  (get-u8 tran-port)))
    => #t)

  (check	;not an open port (with port fast tagged at creation)
      (let ((port (open-bytevector-input-port '#vu8())))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (get-u8 port)))
    => #t)

  (check	;not an open port (with port not fast tagged at creation)
      (let ((port (make-test-bytevector-input-port '#vu8())))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (get-u8 port)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(get-u8 port))
    => (eof-object))

  (check
      (let ((port (open-bytevector-input-port '#vu8(123))))
	(get-u8 port))
    => 123)

  (check
      (let ((port (open-bytevector-input-port '#vu8(123))))
	(get-u8 port)
	(get-u8 port))
    => (eof-object))

  #t)


(parametrise ((check-test-name	'lookahead-u8))

  (define (make-test-bytevector-input-port bv)
    (let ((position	0)
	  (bv.len	(bytevector-length bv))
	  (port		#f))

      (define (read! dst.bv dst.start count)
	(let* ((available	(- bv.len position))
	       (to-read		(min available count)))
	  (unless (zero? to-read)
	    (bytevector-copy! bv position dst.bv dst.start to-read)
	    (set! position (+ to-read position)))
	  to-read))

      (define (get-position)
	position)

      (define (set-position! new-position)
	(if (<= 0 new-position bv.len)
	    (set! position new-position)
	  (raise
	   (condition (make-i/o-invalid-position-error new-position)
		      (make-who-condition 'make-test-port/set-position!)
		      (make-message-condition "invalid port position")
		      (make-irritants-condition (list port))))))

      (define (close)
	(set! bv #f))

      (set! port (make-custom-binary-input-port "*test-binary-input-port*"
						read! get-position set-position! close))
      port))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(lookahead-u8 123))
    => '(123))

  (check	;not an input port
      (let-values (((port extract) (open-bytevector-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (lookahead-u8 port)))
    => #t)

  (check	;not a binary port
      (let ((port (open-string-input-port "")))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (lookahead-u8 port)))
    => #t)

  (check	;not a binary port (with transcoded port)
      (let* ((bin-port  (open-bytevector-input-port '#vu8()))
	     (tran-port (transcoded-port bin-port (native-transcoder))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? tran-port (car (condition-irritants E))))
		  (else E))
	  (lookahead-u8 tran-port)))
    => #t)

  (check	;not an open port (with port fast tagged at creation)
      (let ((port (open-bytevector-input-port '#vu8())))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (lookahead-u8 port)))
    => #t)

  (check	;not an open port (with port not fast tagged at creation)
      (let ((port (make-test-bytevector-input-port '#vu8())))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (lookahead-u8 port)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(lookahead-u8 port))
    => (eof-object))

  (check
      (let ((port (open-bytevector-input-port '#vu8(123))))
	(lookahead-u8 port))
    => 123)

  (check
      (let ((port (open-bytevector-input-port '#vu8(1 2))))
	(lookahead-u8 port)
	(lookahead-u8 port))
    => 1)

  #t)


(parametrise ((check-test-name			'lookahead-two-u8)
	      (bytevector-port-buffer-size	8))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(lookahead-two-u8 123))
    => '(123))

  (check	;not an input port
      (let-values (((port extract) (open-bytevector-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (lookahead-two-u8 port)))
    => #t)

  (check	;not a binary port
      (let ((port (open-string-input-port "")))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (lookahead-two-u8 port)))
    => #t)

  (check	;not a binary port (with transcoded port)
      (let* ((bin-port  (open-bytevector-input-port '#vu8()))
	     (tran-port (transcoded-port bin-port (native-transcoder))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? tran-port (car (condition-irritants E))))
		  (else E))
	  (lookahead-two-u8 tran-port)))
    => #t)

  (check	;not an open port (with port fast tagged at creation)
      (let ((port (open-bytevector-input-port '#vu8())))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (lookahead-two-u8 port)))
    => #t)

  (check	;not an open port (with port not fast tagged at creation)
      (let ((port (open-bytevector-input-port '#vu8())))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (lookahead-two-u8 port)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((port (open-bytevector-input-port '#vu8())))
	(call-with-values
	    (lambda ()
	      (lookahead-two-u8 port))
	  list))
    => (list (eof-object) (eof-object)))

  (check
      (let ((port (open-bytevector-input-port '#vu8(1))))
	(call-with-values
	    (lambda ()
	      (lookahead-two-u8 port))
	  list))
    => (list 1 (eof-object)))

  (check
      (let ((port (open-bytevector-input-port '#vu8(1 2))))
	(call-with-values
	    (lambda ()
	      (lookahead-two-u8 port))
	  list))
    => '(1 2))

  (check
      (let ((port (open-bytevector-input-port '#vu8(1 2 3))))
	(call-with-values
	    (lambda ()
	      (lookahead-two-u8 port))
	  list))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; filling the buffer

  (check
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9 0))))
	(get-bytevector-n port 8)
	(call-with-values
	    (lambda ()
	      (lookahead-two-u8 port))
	  list))
    => '(8 9))

  (check
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8))))
	(get-bytevector-n port 8)
	(call-with-values
	    (lambda ()
	      (lookahead-two-u8 port))
	  list))
    => `(8 ,(eof-object)))

  #t)


(parametrise ((check-test-name		'get-bytevector-n-plain)
	      (test-pathname		(make-test-pathname "get-bytevector-n.bin"))
	      (input-file-buffer-size	100))

;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(get-bytevector-n 123 1))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-bytevector-n port 1)))
    => #t)

  (check	;argument is not a binary port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-bytevector-n port 1)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (get-bytevector-n port 1)))
    => #t)

;;; --------------------------------------------------------------------
;;; count argument validation

  (check	;count is not an integer
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n port #\a)))
    => '(#\a))

  (check 	;count is not an exact integer
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n port 1.0)))
    => '(1.0))

  (check 	;count is negative
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n port -3)))
    => '(-3))

;;; --------------------------------------------------------------------
;;; input from a bytevector port

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8())))
	(get-bytevector-n port 10))
    => (eof-object))

  (check	;count is zero
      (let ((port (open-bytevector-input-port (bindata-hundreds.bv))))
	(get-bytevector-n port 0))
    => (bindata-empty.bv))

  (check	;count is 1
      (let ((port (open-bytevector-input-port (bindata-hundreds.bv))))
	(get-bytevector-n port 1))
    => (bindata-zero.bv))

  (check	;count is 10
      (let ((port (open-bytevector-input-port (bindata-hundreds.bv))))
	(get-bytevector-n port 10))
    => (bindata-ten.bv))

  (check	;count is big
      (let ((port (open-bytevector-input-port (bindata-hundreds.bv))))
	(get-bytevector-n port (bindata-hundreds.len)))
    => (bindata-hundreds.bv))

;;; --------------------------------------------------------------------
;;; input from a file

  (check	;no data available
      (parametrise ((test-pathname-data-func bindata-empty.bv))
	(with-input-test-pathname (port)
	  (get-bytevector-n port 10)))
    => (eof-object))

  (check	;count is 1, much smaller than buffer size
      (with-input-test-pathname (port)
	(get-bytevector-n port 1))
    => (bindata-zero.bv))

  (check	;count is 10, much smaller than buffer size
      (with-input-test-pathname (port)
	(get-bytevector-n port 10))
    => (bindata-ten.bv))

  (check	;count equals buffer size
      (parametrise ((input-file-buffer-size (bindata-bytes.len)))
	(with-input-test-pathname (port)
	  (get-bytevector-n port (input-file-buffer-size))))
    => (bindata-bytes.bv))

  (check	;count is  equal to buffer  size and equal to  the whole
		;available data size
      (parametrise ((input-file-buffer-size	(bindata-bytes.len))
		    (test-pathname-data-func	bindata-bytes.bv))
	(with-input-test-pathname (port)
	  (get-bytevector-n port (bindata-bytes.len))))
    => (bindata-bytes.bv))

  (check	;count is bigger than buffer size
      (with-input-test-pathname (port)
	(get-bytevector-n port (bindata-bytes.len)))
    => (bindata-bytes.bv))

  (check	;count is much bigger than  buffer size and equal to the
		;whole available data size
      (with-input-test-pathname (port)
	(bytevector-length (get-bytevector-n port (bindata-hundreds.len))))
    => (bindata-hundreds.len))

  (check	;count is much bigger than  buffer size and equal to the
  		;whole available data size
      (with-input-test-pathname (port)
  	(let ((bv (get-bytevector-n port (bindata-hundreds.len)))
	      (lim 4500))
	  (%bytevector-u8-compare bv (bindata-hundreds.bv))))
    => #t)

  (check	;count is bigger than available data
      (let ((port (open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9))))
	(get-bytevector-n port 20))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  #t)


(parametrise ((check-test-name		'get-bytevector-n-bang)
	      (test-pathname		(make-test-pathname "get-bytevector-n-bang.bin"))
	      (input-file-buffer-size	100))

;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(get-bytevector-n! 123 (make-bytevector 1) 0 1))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-bytevector-n! port (make-bytevector 1) 0 1)))
    => #t)

  (check	;argument is not a binary port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-bytevector-n! port  (make-bytevector 1) 0 1)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (get-bytevector-n! port  (make-bytevector 1) 0 1)))
    => #t)

;;; --------------------------------------------------------------------
;;; bytevector argument validation

  (check	;argument is not a bytevector
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port  123 0 1)))
    => '(123))

;;; --------------------------------------------------------------------
;;; start index argument validation

  (check	;argument start index is not an integer
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port  (make-bytevector 1) #\a 1)))
    => '(#\a))

  (check	;argument start index is not an exact integer
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port  (make-bytevector 1) 1.0 1)))
    => '(1.0))

  (check	;argument start index is negative
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port  (make-bytevector 1) -1 1)))
    => '(-1))

  (check	;argument start index is too big
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port  (make-bytevector 1) 2 1)))
    => '(2))

;;; --------------------------------------------------------------------
;;; count argument validation

  (check	;count is not an integer
      (let ((port (open-bytevector-input-port '#vu8(1 2 3))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port (make-bytevector 1) 0 #\a)))
    => '(#\a))

  (check	;count is not an exact integer
      (let ((port (open-bytevector-input-port '#vu8(1 2 3))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port (make-bytevector 1) 0 1.0)))
    => '(1.0))

  (check	;count is negative
      (let ((port (open-bytevector-input-port '#vu8(1 2 3))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port (make-bytevector 1) 0 -1)))
    => '(-1))

  (check	;count is too big
      (let ((port (open-bytevector-input-port '#vu8(1 2 3))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port (make-bytevector 1) 0 123)))
    => '(0 123 1))

  (check	;count is too big
      (let ((port (open-bytevector-input-port '#vu8(1 2 3))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port (make-bytevector 1) 0 2)))
    => '(0 2 1))

;;; --------------------------------------------------------------------
;;; input from a bytevector port

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8())))
	(get-bytevector-n! port (make-bytevector 10) 0 10))
    => (eof-object))

  (check	;count is zero
      (let ((port	(open-bytevector-input-port '#vu8(123 2 3)))
	    (bv		(make-bytevector 1 0))
	    (start	0)
	    (count	0))
	(list (get-bytevector-n! port bv start count) bv))
    => '(0 #vu8(0)))

  (check	;count is 1
      (let ((port	(open-bytevector-input-port (bindata-hundreds.bv)))
	    (bv		(make-bytevector 1))
	    (start	0)
	    (count	1))
	(list (get-bytevector-n! port bv start count) bv))
    => (list 1 (bindata-zero.bv)))

  (check	;count is 10
      (let ((port (open-bytevector-input-port (bindata-hundreds.bv)))
	    (bv		(make-bytevector 10))
	    (start	0)
	    (count	10))
	(list (get-bytevector-n! port bv start count) bv))
    => (list 10 (bindata-ten.bv)))

  (check	;count is big
      (let ((port	(open-bytevector-input-port (bindata-hundreds.bv)))
	    (bv		(make-bytevector (bindata-hundreds.len)))
	    (start	0)
	    (count	(bindata-hundreds.len)))
	(list (get-bytevector-n! port bv start count) bv))
    => (list (bindata-hundreds.len) (bindata-hundreds.bv)))

  (check	;exercise start
      (let ((port	(open-bytevector-input-port '#vu8(1 2 3 4 5 6 7 8 9)))
	    (bv		(make-bytevector 10 0))
	    (start	3)
	    (count	6))
	(list (get-bytevector-n! port bv start count) bv))
    => '(6 #vu8(0 0 0 1 2 3 4 5 6 0)))

  (check	;exercise start
      (let ((port	(open-bytevector-input-port '#vu8(1 2 3 4 5 6 7 8 9)))
	    (bv		(make-bytevector 10 0))
	    (start	8)
	    (count	1))
	(list (get-bytevector-n! port bv start count) bv))
    => '(1 #vu8(0 0 0 0 0 0 0 0 1 0)))

  (check	;count is bigger than available data
      (let ((port	(open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7 8 9)))
	    (bv		(make-bytevector 20 123))
	    (start	0)
	    (count	20))
	(list (get-bytevector-n! port bv start count) bv))
    => '(10 #vu8(0 1 2 3 4 5 6 7 8 9
		   123 123 123
		   123 123 123
		   123 123 123
		   123)))

;;; --------------------------------------------------------------------
;;; input from a file

  (check	;no data available
      (parametrise ((test-pathname-data-func bindata-empty.bv))
	(with-input-test-pathname (port)
	  (let ((bv	(make-bytevector 2 123))
		(start	0)
		(count	2))
	    (list (get-bytevector-n! port bv start count) bv))))
    => (list (eof-object) '#vu8(123 123)))

  (check	;count is 1, much smaller than buffer size
      (with-input-test-pathname (port)
	(let ((bv	(make-bytevector 1 123))
	      (start	0)
	      (count	1))
	  (list (get-bytevector-n! port bv start count) bv)))
    => (list 1 (bindata-zero.bv)))

  (check	;count is 10, much smaller than buffer size
      (with-input-test-pathname (port)
	(let ((bv	(make-bytevector 10))
	      (start	0)
	      (count	10))
	  (list (get-bytevector-n! port bv start count) bv)))
    => (list 10 (bindata-ten.bv)))

  (parametrise ((input-file-buffer-size (bindata-bytes.len)))
    (check	;count equals buffer size
	(with-input-test-pathname (port)

	  (let ((bv	(make-bytevector (input-file-buffer-size)))
		(start	0)
		(count	(input-file-buffer-size)))
	    (list (get-bytevector-n! port bv start count) bv)))
      => (list (input-file-buffer-size) (bindata-bytes.bv))))

  (parametrise ((input-file-buffer-size		(bindata-bytes.len))
		(test-pathname-data-func	bindata-bytes.bv))
    (check	;count is  equal to buffer  size and equal to  the whole
		;available data size
	(with-input-test-pathname (port)
	  (let ((bv	(make-bytevector (bindata-bytes.len)))
		(start	0)
		(count	(bindata-bytes.len)))
	    (list (get-bytevector-n! port bv start count) bv)))
      => (list (bindata-bytes.len) (bindata-bytes.bv))))

  (check	;count is bigger than buffer size
      (with-input-test-pathname (port)
	(let ((bv	(make-bytevector (bindata-bytes.len)))
	      (start	0)
	      (count	(bindata-bytes.len)))
	  (list (get-bytevector-n! port bv start count) bv)))
    => (list (bindata-bytes.len) (bindata-bytes.bv)))

  (check	;count is much bigger than  buffer size and equal to the
		;whole available data size
      (with-input-test-pathname (port)
	(let ((bv	(make-bytevector (bindata-hundreds.len)))
	      (start	0)
	      (count	(bindata-hundreds.len)))
	  (list (get-bytevector-n! port bv start count) bv)))
    => (list (bindata-hundreds.len) (bindata-hundreds.bv)))

  #t)


(parametrise ((check-test-name		'get-bytevector-some)
	      (test-pathname		(make-test-pathname "get-bytevector-some.bin"))
	      (input-file-buffer-size	100))

;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(get-bytevector-some 123))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-bytevector-some port)))
    => #t)

  (check	;argument is not a binary port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-bytevector-some port)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (get-bytevector-some port)))
    => #t)

;;; --------------------------------------------------------------------
;;; input from a bytevector port

;;Remember  that bytevector  input ports  have the  buffer equal  to the
;;input bytevector.

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8())))
	(get-bytevector-some port))
    => (eof-object))

  (check	;data size equal to buffer size
      (let* ((port	(open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7)))
	     (bv	(get-bytevector-some port)))
	(list bv (port-eof? port)))
    => '(#vu8(0 1 2 3 4 5 6 7) #t))

;;; --------------------------------------------------------------------
;;; input from a file

  (check	;no data available
      (parametrise ((test-pathname-data-func bindata-empty.bv))
	(with-input-test-pathname (port)
	  (get-bytevector-some port)))
    => (eof-object))

  (check	;return buffer contents
      (parametrise ((input-file-buffer-size 8))
	(with-input-test-pathname (port)
	  (get-bytevector-some port)))
    => '#vu8(0 1 2 3 4 5 6 7))

  (check	;return buffer contents twice
      (parametrise ((input-file-buffer-size 8))
	(with-input-test-pathname (port)
	  (let ((bv (get-bytevector-some port)))
	    (list bv (get-bytevector-some port)))))
    => '(#vu8(0 1 2 3 4 5 6 7) #vu8(8 9 10 11 12 13 14 15)))

  (check	;return the whole data smaller than buffer
      (parametrise ((input-file-buffer-size	8)
		    (test-pathname-data-func	(lambda ()
						  '#vu8(0 1 2 3))))
	(with-input-test-pathname (port)
	  (get-bytevector-some port)))
    => '#vu8(0 1 2 3))

  (check	;return tail of buffer contents
      (parametrise ((input-file-buffer-size 8))
	(with-input-test-pathname (port)
	  (let ((bv (get-bytevector-n port 4)))
	    (list bv (get-bytevector-some port)))))
    => '(#vu8(0 1 2 3) #vu8(4 5 6 7)))

  #t)


(parametrise ((check-test-name		'get-bytevector-all)
	      (test-pathname		(make-test-pathname "get-bytevector-all.bin"))
	      (input-file-buffer-size	100))

;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(get-bytevector-all 123))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-bytevector-all port)))
    => #t)

  (check	;argument is not a binary port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-bytevector-all port)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (get-bytevector-all port)))
    => #t)

;;; --------------------------------------------------------------------
;;; input from a bytevector port

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8())))
	(get-bytevector-all port))
    => (eof-object))

  (check
      (let* ((port	(open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7)))
	     (bv	(get-bytevector-all port)))
	(list bv (port-eof? port)))
    => '(#vu8(0 1 2 3 4 5 6 7) #t))

  (check	;return tail of data
      (let* ((port	(open-bytevector-input-port '#vu8(0 1 2 3 4 5 6 7)))
	     (bv	(get-bytevector-n port 4)))
	(list bv (get-bytevector-all port)))
    => '(#vu8(0 1 2 3) #vu8(4 5 6 7)))

;;; --------------------------------------------------------------------
;;; input from a file

  (check	;no data available
      (parametrise ((test-pathname-data-func bindata-empty.bv))
	(with-input-test-pathname (port)
	  (get-bytevector-all port)))
    => (eof-object))

  (check	;return the whole data
      (parametrise ((test-pathname-data-func bindata-bytes.bv))
	(with-input-test-pathname (port)
	  (let ((bv (get-bytevector-all port)))
	    (list bv (port-eof? port)))))
    => (list (bindata-bytes.bv) #t))

  (check	;return the whole data
      (with-input-test-pathname (port)
	(let ((bv (get-bytevector-all port)))
	  (list bv (port-eof? port))))
    => (list (bindata-hundreds.bv) #t))

  #t)


(parametrise ((check-test-name	'read-char))

;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(read-char 123))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (read-char port)))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (read-char port)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (read-char port)))
    => #t)

;;; --------------------------------------------------------------------
;;; reading from string input port

;;Remember that a string input port has the string itself as buffer.

  (check	;no input available
      (let ((port (open-string-input-port "")))
	(read-char port))
    => (eof-object))

  (check	;read single char
      (let ((port (open-string-input-port "ciao")))
	(read-char port))
    => #\c)

  (check	;read multiple chars
      (let ((port (open-string-input-port "ciao")))
	(let ((c1 (read-char port))
	      (c2 (read-char port))
	      (c3 (read-char port))
	      (c4 (read-char port)))
	  (list c1 c2 c3 c4 (read-char port))))
    => (list #\c #\i #\a #\o (eof-object)))

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-8

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port
			     bv (make-transcoder (utf-8-codec)))))
		  (let loop ((L '()))
		    (if (port-eof? port)
			(apply string (reverse L))
		      (loop (cons (read-char port) L))))))))

    (check
	(doit TEST-BYTEVECTOR-FOR-UTF-8)
      => TEST-STRING-FOR-UTF-8)

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-8, 2-bytes chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port
			     bv (make-transcoder (utf-8-codec)
						 (eol-style none)
						 (error-handling-mode ignore)))))
		  (read-char port)))))

    (check	;read 2-bytes UTF-8 char, lambda character
	(doit TWO-BYTES-UTF-8-CHAR-UTF-8)
      => TWO-BYTES-UTF-8-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt   to  read   incomplete  2-bytes   UTF-8  char,
		;unexpected EOF, ignore
	(let ((mode (error-handling-mode ignore)))
	  (doit (subbytevector-u8 TWO-BYTES-UTF-8-CHAR-UTF-8 0 1) mode))
      => `(,(eof-object) #t))

    (check	;attempt   to  read   incomplete  2-bytes   UTF-8  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (doit (subbytevector-u8 TWO-BYTES-UTF-8-CHAR-UTF-8 0 1) mode))
      => '(#\xFFFD #t))

    (check	;attempt   to  read   incomplete  2-bytes   UTF-8  char,
		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (gdoit (subbytevector-u8 TWO-BYTES-UTF-8-CHAR-UTF-8 0 1) mode))
      => (list (bytevector-u8-ref TWO-BYTES-UTF-8-CHAR-UTF-8 0)))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-8, 3-bytes chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port
			     bv (make-transcoder (utf-8-codec)
						 (eol-style none)
						 (error-handling-mode ignore)))))
		  (read-char port)))))

    (check	;read 3-bytes UTF-8 char
	(doit THREE-BYTES-UTF-8-CHAR-UTF-8)
      => THREE-BYTES-UTF-8-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
    		;unexpected EOF, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(doit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => `((,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(doit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => '((#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
    		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(gdoit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => (list (bytevector->u8-list (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 2))
	       (bytevector->u8-list (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 1))))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-8, 4-bytes chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-8-codec)))))
		  (read-char port)))))

    (check	;read 4-bytes UTF-8 char
	(doit FOUR-BYTES-UTF-8-CHAR-UTF-8)
      => FOUR-BYTES-UTF-8-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt   to  read   incomplete  4-bytes   UTF-8  char,
    		;unexpected EOF, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 3) mode)
		(doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  4-bytes   UTF-8  char,
    		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 3) mode)
		(doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt   to  read   incomplete  4-bytes   UTF-8  char,
    		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 3) mode)
		(gdoit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(gdoit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => (list (bytevector->u8-list (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 3))
	       (bytevector->u8-list (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 2))
	       (bytevector->u8-list (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 1))))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-8, invalid byte

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to read invalid byte, ignore
	(let ((mode (error-handling-mode ignore)))
	  (doit '#vu8(#xFF) mode))
      => (list (eof-object) #t))

    (check	;attempt to read invalid byte, replace
	(let ((mode (error-handling-mode replace)))
	  (doit '#vu8(#xFF) mode))
      => '(#\xFFFD #t))

    (check	;attempt to read invalid byte, raise
	(let ((mode (error-handling-mode raise)))
	  (gdoit '#vu8(#xFF) mode))
      => '(#xFF))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-8, corrupted 2-bytes chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to read corrupted 2-bytes UTF-8 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (doit CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8 mode))
      => (list (eof-object) #t))

    (check	;attempt to read corrupted 2-bytes UTF-8 char, replace
	(let ((mode (error-handling-mode replace)))
	  (doit CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8 mode))
      => '(#\xFFFD #t))

    (check	;attempt to read corrupted 2-bytes UTF-8 char, raise
	(let ((mode (error-handling-mode raise)))
	  (gdoit CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8 mode))
      => (bytevector->u8-list CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-8, corrupted 3-bytes chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to read corrupted 3-bytes UTF-8 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => `((,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => '((#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
    		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)
		(gdoit CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => (list (bytevector->u8-list CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8)
	       (bytevector->u8-list CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8)))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-8, corrupted 4-bytes chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to read corrupted 4-bytes UTF-8 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt to read corrupted 4-bytes UTF-8 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to read corrupted 4-bytes UTF-8 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(gdoit CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(gdoit CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => (list (bytevector->u8-list CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8)
	       (bytevector->u8-list CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8)
	       (bytevector->u8-list CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8)))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-16

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-16-codec)))))
		  (let loop ((L '()))
		    (if (port-eof? port)
			(apply string (reverse L))
		      (loop (cons (read-char port) L))))))))

    (check
	(doit TEST-BYTEVECTOR-FOR-UTF-16-BE)
      => TEST-STRING-FOR-UTF-16-BE)

    (check
	(doit TEST-BYTEVECTOR-FOR-UTF-16-LE/BOM)
      => TEST-STRING-FOR-UTF-16-LE)

    (check
	(doit TEST-BYTEVECTOR-FOR-UTF-16-BE/BOM)
      => TEST-STRING-FOR-UTF-16-BE)

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-16, 1-word chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-16-codec)))))
		  (read-char port)))))

    (check	;big endian char, default
	(doit ONE-WORD-UTF-16-CHAR-UTF-16-BE)
      => ONE-WORD-UTF-16-CHAR)

    (check	;little endian char, with bom
	(doit ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM)
      => ONE-WORD-UTF-16-CHAR)

    (check	;big endian char
	(doit ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM)
      => ONE-WORD-UTF-16-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-16-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt  to read  incomplete single  word  UTF-16 char,
		;unexpected EOF, ignore
	(doit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-BE 0 1) (error-handling-mode ignore))
      => `(,(eof-object) #t))

    (check	;attempt  to read  incomplete single  word  UTF-16 char,
		;unexpected EOF, replace
	(doit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-BE 0 1) (error-handling-mode replace))
      => '(#\xFFFD #t))

    (check	;attempt  to read  incomplete single  word  UTF-16 char,
		;unexpected EOF, raise
	(gdoit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-BE 0 1) (error-handling-mode raise))
      => (bytevector->u8-list (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-BE 0 1)))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-16, 2-words chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-16-codec)))))
		  (read-char port)))))

    (check	;big endian char, default
	(doit TWO-WORDS-UTF-16-CHAR-UTF-16-BE)
      => TWO-WORDS-UTF-16-CHAR)

    (check	;little endian char, with bom
	(doit TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM)
      => TWO-WORDS-UTF-16-CHAR)

    (check	;big endian char, with bom
	(doit TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM)
      => TWO-WORDS-UTF-16-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-16-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt   to  read   incomplete  2-word   UTF-16  char,
		;unexpected EOF, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 1) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 2) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 3) mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  2-word   UTF-16  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 1) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 2) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 3) mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt  to read  incomplete single  word  UTF-16 char,
		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 1) mode)
		(gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 2) mode)
		(gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 3) mode)))
      => (list (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 1))
	       (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 2))
	       (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 3))))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-16, corrupted 1-word chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-16-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to read corrupted 1-word UTF-16 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt to read corrupted 1-word UTF-16 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to read corrupted 1-word UTF-16 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE     mode)
		(gdoit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(gdoit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD)
	   (,CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD)
	   (,CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD)))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-16, corrupted 2-word chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-16-codec) (eol-style none) mode)))
			 (ch (read-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to read corrupted 2-word UTF-16 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)
	   (,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt to read corrupted 2-word UTF-16 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)
	   (#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to read corrupted 2-word UTF-16 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(gdoit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(gdoit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(gdoit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD)))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded Latin-1

  (let ((doit (lambda (bv mode)
		(let ((port (open-bytevector-input-port
			     bv (make-transcoder (latin-1-codec) (eol-style none) mode))))
		  (let loop ((L '()))
		    (if (port-eof? port)
			(apply string (reverse L))
		      (loop (cons (read-char port) L))))))))

    (check
	(doit TEST-BYTEVECTOR-FOR-LATIN-1 (error-handling-mode replace))
      => TEST-STRING-FOR-LATIN-1)

    #f)

  #t)


(parametrise ((check-test-name	'peek-char))

  (define (%peek-and-consume-char port)
    (let ((ch (peek-char port)))
      (read-char port)
      ch))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(peek-char 123))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (peek-char port)))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (peek-char port)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (peek-char port)))
    => #t)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-8

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port
			     bv (make-transcoder (utf-8-codec)))))
		  (let loop ((L '()))
		    (if (port-eof? port)
			(apply string (reverse L))
		      (loop (cons (%peek-and-consume-char port) L))))))))

    (check
	(doit TEST-BYTEVECTOR-FOR-UTF-8)
      => TEST-STRING-FOR-UTF-8)

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-8, 2-bytes chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port
			     bv (make-transcoder (utf-8-codec)
						 (eol-style none)
						 (error-handling-mode ignore)))))
		  (%peek-and-consume-char port)))))

    (check	;read 2-bytes UTF-8 char, lambda character
	(doit TWO-BYTES-UTF-8-CHAR-UTF-8)
      => TWO-BYTES-UTF-8-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt   to  read   incomplete  2-bytes   UTF-8  char,
		;unexpected EOF, ignore
	(let ((mode (error-handling-mode ignore)))
	  (doit (subbytevector-u8 TWO-BYTES-UTF-8-CHAR-UTF-8 0 1) mode))
      => `(,(eof-object) #t))

    (check	;attempt   to  read   incomplete  2-bytes   UTF-8  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (doit (subbytevector-u8 TWO-BYTES-UTF-8-CHAR-UTF-8 0 1) mode))
      => '(#\xFFFD #t))

    (check	;attempt   to  read   incomplete  2-bytes   UTF-8  char,
		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (gdoit (subbytevector-u8 TWO-BYTES-UTF-8-CHAR-UTF-8 0 1) mode))
      => (list (bytevector-u8-ref TWO-BYTES-UTF-8-CHAR-UTF-8 0)))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-8, 3-bytes chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port
			     bv (make-transcoder (utf-8-codec)
						 (eol-style none)
						 (error-handling-mode ignore)))))
		  (%peek-and-consume-char port)))))

    (check	;read 3-bytes UTF-8 char
	(doit THREE-BYTES-UTF-8-CHAR-UTF-8)
      => THREE-BYTES-UTF-8-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
    		;unexpected EOF, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(doit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => `((,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(doit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => '((#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
    		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(gdoit (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => (list (bytevector->u8-list (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 2))
	       (bytevector->u8-list (subbytevector-u8 THREE-BYTES-UTF-8-CHAR-UTF-8 0 1))))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-8, 4-bytes chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-8-codec)))))
		  (%peek-and-consume-char port)))))

    (check	;read 4-bytes UTF-8 char
	(doit FOUR-BYTES-UTF-8-CHAR-UTF-8)
      => FOUR-BYTES-UTF-8-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt   to  read   incomplete  4-bytes   UTF-8  char,
    		;unexpected EOF, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 3) mode)
		(doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  4-bytes   UTF-8  char,
    		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 3) mode)
		(doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(doit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt   to  read   incomplete  4-bytes   UTF-8  char,
    		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 3) mode)
		(gdoit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 2) mode)
		(gdoit (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 1) mode)))
      => (list (bytevector->u8-list (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 3))
	       (bytevector->u8-list (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 2))
	       (bytevector->u8-list (subbytevector-u8 FOUR-BYTES-UTF-8-CHAR-UTF-8 0 1))))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-8, invalid byte

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to peek invalid byte, ignore
	(let ((mode (error-handling-mode ignore)))
	  (doit '#vu8(#xFF) mode))
      => (list (eof-object) #t))

    (check	;attempt to peek invalid byte, replace
	(let ((mode (error-handling-mode replace)))
	  (doit '#vu8(#xFF) mode))
      => '(#\xFFFD #t))

    (check	;attempt to peek invalid byte, raise
	(let ((mode (error-handling-mode raise)))
	  (gdoit '#vu8(#xFF) mode))
      => '(#xFF))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-8, corrupted 2-bytes chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to peek corrupted 2-bytes UTF-8 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (doit CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8 mode))
      => (list (eof-object) #t))

    (check	;attempt to peek corrupted 2-bytes UTF-8 char, replace
	(let ((mode (error-handling-mode replace)))
	  (doit CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8 mode))
      => '(#\xFFFD #t))

    (check	;attempt to peek corrupted 2-bytes UTF-8 char, raise
	(let ((mode (error-handling-mode raise)))
	  (gdoit CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8 mode))
      => (bytevector->u8-list CORRUPTED-TWO-BYTES-UTF-8-CHAR-UTF-8))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-8, corrupted 3-bytes chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to peek corrupted 3-bytes UTF-8 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => `((,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => '((#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt   to  read   incomplete  3-bytes   UTF-8  char,
    		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)
		(gdoit CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => (list (bytevector->u8-list CORRUPTED1-THREE-BYTES-UTF-8-CHAR-UTF-8)
	       (bytevector->u8-list CORRUPTED2-THREE-BYTES-UTF-8-CHAR-UTF-8)))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-8, corrupted 4-bytes chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-8-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to peek corrupted 4-bytes UTF-8 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt to peek corrupted 4-bytes UTF-8 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(doit CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to peek corrupted 4-bytes UTF-8 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(gdoit CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)
		(gdoit CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8 mode)))
      => (list (bytevector->u8-list CORRUPTED1-FOUR-BYTES-UTF-8-CHAR-UTF-8)
	       (bytevector->u8-list CORRUPTED2-FOUR-BYTES-UTF-8-CHAR-UTF-8)
	       (bytevector->u8-list CORRUPTED3-FOUR-BYTES-UTF-8-CHAR-UTF-8)))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-16

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-16-codec)))))
		  (let loop ((L '()))
		    (if (port-eof? port)
			(apply string (reverse L))
		      (loop (cons (%peek-and-consume-char port) L))))))))

    (check
	(doit TEST-BYTEVECTOR-FOR-UTF-16-BE)
      => TEST-STRING-FOR-UTF-16-BE)

    (check
	(doit TEST-BYTEVECTOR-FOR-UTF-16-LE/BOM)
      => TEST-STRING-FOR-UTF-16-LE)

    (check
	(doit TEST-BYTEVECTOR-FOR-UTF-16-BE/BOM)
      => TEST-STRING-FOR-UTF-16-BE)

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-16, 1-word chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-16-codec)))))
		  (%peek-and-consume-char port)))))

    (check	;big endian char, default
	(doit ONE-WORD-UTF-16-CHAR-UTF-16-BE)
      => ONE-WORD-UTF-16-CHAR)

    (check	;little endian char, with bom
	(doit ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM)
      => ONE-WORD-UTF-16-CHAR)

    (check	;big endian char, with bom
	(doit ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM)
      => ONE-WORD-UTF-16-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-16-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt  to peek  incomplete single  word  UTF-16 char,
		;unexpected EOF, ignore
	(doit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-BE 0 1) (error-handling-mode ignore))
      => `(,(eof-object) #t))

    (check	;attempt  to peek  incomplete single  word  UTF-16 char,
		;unexpected EOF, replace
	(doit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-BE 0 1) (error-handling-mode replace))
      => '(#\xFFFD #t))

    (check	;attempt  to peek  incomplete single  word  UTF-16 char,
		;unexpected EOF, raise
	(gdoit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-BE 0 1) (error-handling-mode raise))
      => (bytevector->u8-list (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-BE 0 1)))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-16, 2-words chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-16-codec)))))
		  (%peek-and-consume-char port)))))

    (check	;big endian char, default
	(doit TWO-WORDS-UTF-16-CHAR-UTF-16-BE)
      => TWO-WORDS-UTF-16-CHAR)

    (check	;little endian char, with bom
	(doit TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM)
      => TWO-WORDS-UTF-16-CHAR)

    (check	;big endian char, with bom
	(doit TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM)
      => TWO-WORDS-UTF-16-CHAR)

    #f)

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-16-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt   to  read   incomplete  2-word   UTF-16  char,
		;unexpected EOF, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 1) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 2) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 3) mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  2-word   UTF-16  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 1) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 2) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 3) mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt  to peek  incomplete single  word  UTF-16 char,
		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 1) mode)
		(gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 2) mode)
		(gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 3) mode)))
      => (list (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 1))
	       (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 2))
	       (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-BE 0 3))))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-16, corrupted 1-word chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-16-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to peek corrupted 1-word UTF-16 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt to peek corrupted 1-word UTF-16 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to peek corrupted 1-word UTF-16 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE     mode)
		(gdoit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(gdoit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD)
	   (,CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD)
	   (,CORRUPTED-ONE-WORD-UTF-16-CHAR-WORD)))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-16, corrupted 2-word chars

  (let* ((doit	(lambda (bv mode)
		  (let* ((port (open-bytevector-input-port
				bv (make-transcoder (utf-16-codec) (eol-style none) mode)))
			 (ch (%peek-and-consume-char port)))
		    (list ch (port-eof? port)))))
	 (gdoit	(lambda (bv mode)
		  (guard (E ((i/o-decoding-error? E)
;;;			     (pretty-print (condition-message E))
			     (condition-irritants E))
			    (else E))
		    (doit bv mode)))))

    (check	;attempt to peek corrupted 2-word UTF-16 char, ignore
	(let ((mode (error-handling-mode ignore)))
	  (list (doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)
	   (,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check 	;attempt to peek corrupted 2-word UTF-16 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)
	   (#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to peek corrupted 2-word UTF-16 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(gdoit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE     mode)
		(gdoit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(gdoit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED1-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD)
	   (,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-1ST-WORD ,CORRUPTED2-TWO-WORDS-UTF-16-CHAR-2ND-WORD)))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded Latin-1

  (let ((doit (lambda (bv mode)
		(let ((port (open-bytevector-input-port
			     bv (make-transcoder (latin-1-codec) (eol-style none) mode))))
		  (let loop ((L '()))
		    (if (port-eof? port)
			(apply string (reverse L))
		      (loop (cons (%peek-and-consume-char port) L))))))))

    (check
	(doit TEST-BYTEVECTOR-FOR-LATIN-1 (error-handling-mode replace))
      => TEST-STRING-FOR-LATIN-1)

    #f)

  #t)


(parametrise ((check-test-name		'get-string-n-plain))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(get-string-n 123 1))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-string-n port 1)))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-string-n port 1)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (get-string-n port 1)))
    => #t)

;;; --------------------------------------------------------------------
;;; count argument validation

  (check	;count is not an integer
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-string-n port #\a)))
    => '(#\a))

  (check 	;count is not an exact integer
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-string-n port 1.0)))
    => '(1.0))

  (check 	;count is negative
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-string-n port -3)))
    => '(-3))

  (check 	;count is not a fixnum
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-string-n port (+ 1 (greatest-fixnum)))))
    => (list (+ 1 (greatest-fixnum))))

;;; --------------------------------------------------------------------
;;; input from a string port

  (check	;no data available
      (let ((port (open-string-input-port "")))
	(get-string-n port 10))
    => (eof-object))

  (check	;count is bigger than available data
      (let ((port (open-string-input-port "01234")))
	(get-string-n port 10))
    => "01234")

  (let* ((src.len 100)
	 (src.str (let ((str (make-string src.len)))
		    (do ((i 0 (+ 1 i)))
			((= i src.len)
			 str)
		      (string-set! str i (integer->char i))))))

    (check	;count is zero
	(let ((port (open-string-input-port src.str)))
	  (get-string-n port 0))
      => "")

    (check	;count is 1
	(let ((port (open-string-input-port src.str)))
	  (get-string-n port 1))
      => "\x0;")

    (check	;count is 10
	(let ((port (open-string-input-port src.str)))
	  (get-string-n port 10))
      => (substring src.str 0 10))

    (check	;count is big
	(let ((port (open-string-input-port src.str)))
	  (get-string-n port src.len))
      => src.str)

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with Latin-1 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (latin-1-codec)))))
	(get-string-n port 10))
    => (eof-object))

  (check	;count is bigger than available data
      (let ((port (open-bytevector-input-port '#vu8(65 66 67 68)
					      (make-transcoder (latin-1-codec)))))
	(get-string-n port 10))
    => "ABCD")

  (let* ((src.len 100)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->latin1 src.str))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (latin-1-codec)))))
		    (get-string-n port count)))))

    (check (doit 0)		=> "")
    (check (doit 1)		=> "\x0;")
    (check (doit 10)		=> (substring src.str 0 10))
    (check (doit src.len)	=> src.str)

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with UTF-8 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (utf-8-codec)))))
	(get-string-n port 10))
    => (eof-object))

  (check	;count is bigger than available data
      (let ((port (open-bytevector-input-port '#vu8(65 66 67 68)
					      (make-transcoder (utf-8-codec)))))
	(get-string-n port 10))
    => "ABCD")

  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf8 src.str))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-8-codec)))))
		    (get-string-n port count)))))

    (check (doit 0)		=> "")
    (check (doit 1)		=> "\x0;")
    (check (doit 10)		=> (substring src.str 0 10))
    (check (doit src.len)	=> src.str)

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with UTF-16 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (utf-16-codec)))))
	(get-string-n port 10))
    => (eof-object))

  (check	;count is bigger than available data
      (let ((port (open-bytevector-input-port (string->utf16 "ABCD" (endianness big))
					      (make-transcoder (utf-16-codec)))))
	(get-string-n port 10))
    => "ABCD")

  ;; big endian, default
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf16 src.str (endianness big)))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (get-string-n port count)))))

    (check (doit 0)		=> "")
    (check (doit 1)		=> "\x0;")
    (check (doit 10)		=> (substring src.str 0 10))
    (check (doit src.len)	=> src.str)

    #f)

  ;; little endian, with bom
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (bytevector-append BYTE-ORDER-MARK-UTF-16-LE
				     (string->utf16 src.str (endianness little))))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (get-string-n port count)))))

    (check (doit 0)		=> "")
    (check (doit 1)		=> "\x0;")
    (check (doit 10)		=> (substring src.str 0 10))
    (check (doit src.len)	=> src.str)
    #f)

  ;; big endian, with bom
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (bytevector-append BYTE-ORDER-MARK-UTF-16-BE
				     (string->utf16 src.str (endianness big))))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (get-string-n port count)))))

    (check (doit 0)		=> "")
    (check (doit 1)		=> "\x0;")
    (check (doit 10)		=> (substring src.str 0 10))
    (check (doit src.len)	=> src.str)
    #f)

  #t)


(parametrise ((check-test-name		'get-string-n-bang))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port		123)
	      (dst.str		(make-string 1))
	      (dst.start	0)
	      (count		1))
	  (get-string-n! port dst.str dst.start count)))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	0)
		(count		1))
	    (get-string-n! port dst.str dst.start count))))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	0)
		(count		1))
	    (get-string-n! port dst.str dst.start count))))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	0)
		(count		1))
	    (get-string-n! port dst.str dst.start count))))
    => #t)

;;; --------------------------------------------------------------------
;;; start argument validation

  (check	;start is not an integer
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	#\a)
		(count		1))
	    (get-string-n! port dst.str dst.start count))))
    => '(#\a))

  (check 	;start is not an exact integer
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	1.0)
		(count		1))
	    (get-string-n! port dst.str dst.start count))))
    => '(1.0))

  (check  	;start is not a fixnum
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	(+ 1 (greatest-fixnum)))
		(count		1))
	    (get-string-n! port dst.str dst.start count))))
    => (list (+ 1 (greatest-fixnum))))

  (check 	;start is negative
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	-3)
		(count		1))
	    (get-string-n! port dst.str dst.start count))))
    => '(-3))

  (check 	;start is too big for string
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 10))
		(dst.start	12)
		(count		1))
	    (get-string-n! port dst.str dst.start count))))
    => '(12))

;;; --------------------------------------------------------------------
;;; count argument validation

  (check	;count is not an integer
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	0)
		(count		#\a))
	    (get-string-n! port dst.str dst.start count))))
    => '(#\a))

  (check 	;count is not an exact integer
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	0)
		(count		1.0))
	    (get-string-n! port dst.str dst.start count))))
    => '(1.0))

  (check 	;count is not a fixnum
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	0)
		(count		(+ 1 (greatest-fixnum))))
	    (get-string-n! port dst.str dst.start count))))
    => (list (+ 1 (greatest-fixnum))))

  (check 	;count is negative
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 1))
		(dst.start	0)
		(count		-3))
	    (get-string-n! port dst.str dst.start count))))
    => '(-3))

;;; --------------------------------------------------------------------
;;; start+count arguments validation

  (check 	;start+count is not a fixnum
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 10))
		(dst.start	1)
		(count		(greatest-fixnum)))
	    (get-string-n! port dst.str dst.start count))))
    => (list 1 (greatest-fixnum) 10))

  (check 	;start+count is too big for string
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (let ((port		port)
		(dst.str	(make-string 10))
		(dst.start	8)
		(count		9))
	    (get-string-n! port dst.str dst.start count))))
    => '(8 9 10))

;;; --------------------------------------------------------------------
;;; input from a string port

  (check	;no data available
      (let ((port (open-string-input-port "")))
	(let ((dst.str		(make-string 10))
	      (dst.start	0)
	      (count		10))
	  (get-string-n! port dst.str dst.start count)))
    => (eof-object))

  (check	;count is bigger than available data
      (let ((port (open-string-input-port "01234")))
	(let ((dst.str		(make-string 10 #\Z))
	      (dst.start	0)
	      (count		10))
	  (list (get-string-n! port dst.str dst.start count) dst.str)))
    => '(5 "01234ZZZZZ"))

  (let* ((src.len 100)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i))))))

    (check	;count is zero
  	(let ((port (open-string-input-port src.str)))
	  (let ((dst.str	(make-string 10 #\Z))
		(dst.start	0)
		(count		0))
	    (list (get-string-n! port dst.str dst.start count) dst.str)))
      => '(0 "ZZZZZZZZZZ"))

    (check	;count is 1
  	(let ((port (open-string-input-port src.str)))
	  (let ((dst.str	(make-string 10 #\Z))
		(dst.start	0)
		(count		1))
	    (list (get-string-n! port dst.str dst.start count) dst.str)))
      => '(1 "\x0;ZZZZZZZZZ"))

    (check	;count is 10
  	(let ((port (open-string-input-port src.str)))
	  (let ((dst.str	(make-string 10 #\Z))
		(dst.start	0)
		(count		10))
	    (list (get-string-n! port dst.str dst.start count) dst.str)))
      => (list 10 (substring src.str 0 10)))

    (check	;count is big

  	(let ((port (open-string-input-port src.str)))
	  (let ((dst.str	(make-string src.len #\Z))
		(dst.start	0)
		(count		src.len))
	    (list (get-string-n! port dst.str dst.start count) dst.str)))
      => (list src.len src.str))

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with Latin-1 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (latin-1-codec)))))
	(let ((dst.str		(make-string 10 #\Z))
	      (dst.start	0)
	      (count		10))
	  (list (get-string-n! port dst.str dst.start count) dst.str)))
    => (list (eof-object) "ZZZZZZZZZZ"))

  (check	;count is bigger than available data
      (let ((port (open-bytevector-input-port '#vu8(65 66 67 68)
					      (make-transcoder (latin-1-codec)))))
	(let ((dst.str		(make-string 10 #\Z))
	      (dst.start	0)
	      (count		10))
	  (list (get-string-n! port dst.str dst.start count) dst.str)))
    => '(4 "ABCDZZZZZZ"))

  (let* ((src.len 100)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->latin1 src.str))
	 (doit	(lambda (count len)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (latin-1-codec)))))
		    (let ((dst.str	(make-string len #\Z))
			  (dst.start	0))
		      (list (get-string-n! port dst.str dst.start count) dst.str))))))

    (check (doit 0 10)			=> '(0 "ZZZZZZZZZZ"))
    (check (doit 1 10)			=> '(1 "\x0;ZZZZZZZZZ"))
    (check (doit 10 10)			=> (list 10 (substring src.str 0 10)))
    (check (doit src.len src.len)	=> (list src.len src.str))

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with UTF-8 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (utf-8-codec)))))
	(let ((dst.str		(make-string 10 #\Z))
	      (dst.start	0)
	      (count		10))
	  (list (get-string-n! port dst.str dst.start count) dst.str)))
    => (list (eof-object) "ZZZZZZZZZZ"))

  (check	;count is bigger than available data
      (let ((port (open-bytevector-input-port '#vu8(65 66 67 68)
					      (make-transcoder (utf-8-codec)))))
	(let ((dst.str		(make-string 10 #\Z))
	      (dst.start	0)
	      (count		10))
	  (list (get-string-n! port dst.str dst.start count) dst.str)))
    => '(4 "ABCDZZZZZZ"))

  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf8 src.str))
	 (doit	(lambda (count len)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-8-codec)))))
		    (let ((dst.str	(make-string len #\Z))
			  (dst.start	0))
		      (list (get-string-n! port dst.str dst.start count) dst.str))))))

    (check (doit 0 10)			=> '(0 "ZZZZZZZZZZ"))
    (check (doit 1 10)			=> '(1 "\x0;ZZZZZZZZZ"))
    (check (doit 10 10)			=> (list 10 (substring src.str 0 10)))
    (check (doit src.len src.len)	=> (list src.len src.str))

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with UTF-16 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (utf-16-codec)))))
	(let ((dst.str		(make-string 10 #\Z))
	      (dst.start	0)
	      (count		10))
	  (list (get-string-n! port dst.str dst.start count) dst.str)))
    => (list (eof-object) "ZZZZZZZZZZ"))

  (check	;count is bigger than available data
      (let ((port (open-bytevector-input-port (string->utf16 "ABCD" (endianness big))
					      (make-transcoder (utf-16-codec)))))
	(let ((dst.str		(make-string 10 #\Z))
	      (dst.start	0)
	      (count		10))
	  (list (get-string-n! port dst.str dst.start count) dst.str)))
    => '(4 "ABCDZZZZZZ"))

  ;; big endian, default
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf16 src.str (endianness big)))
	 (doit	(lambda (count len)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (let ((dst.str	(make-string len #\Z))
			  (dst.start	0))
		      (list (get-string-n! port dst.str dst.start count) dst.str))))))

    (check (doit 0 10)			=> '(0 "ZZZZZZZZZZ"))
    (check (doit 1 10)			=> '(1 "\x0;ZZZZZZZZZ"))
    (check (doit 10 10)			=> (list 10 (substring src.str 0 10)))
    (check (doit src.len src.len)	=> (list src.len src.str))

    #f)

  ;; little endian, with bom
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
 	 (src.bv  (bytevector-append BYTE-ORDER-MARK-UTF-16-LE
				     (string->utf16 src.str (endianness little))))
	 (doit	(lambda (count len)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (let ((dst.str	(make-string len #\Z))
			  (dst.start	0))
		      (list (get-string-n! port dst.str dst.start count) dst.str))))))

    (check (doit 0 10)			=> '(0 "ZZZZZZZZZZ"))
    (check (doit 1 10)			=> '(1 "\x0;ZZZZZZZZZ"))
    (check (doit 10 10)			=> (list 10 (substring src.str 0 10)))
    (check (doit src.len src.len)	=> (list src.len src.str))

    #f)

  ;; big endian, with bom
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
 	 (src.bv  (bytevector-append BYTE-ORDER-MARK-UTF-16-BE
				     (string->utf16 src.str (endianness big))))
	 (doit	(lambda (count len)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (let ((dst.str	(make-string len #\Z))
			  (dst.start	0))
		      (list (get-string-n! port dst.str dst.start count) dst.str))))))

    (check (doit 0 10)			=> '(0 "ZZZZZZZZZZ"))
    (check (doit 1 10)			=> '(1 "\x0;ZZZZZZZZZ"))
    (check (doit 10 10)			=> (list 10 (substring src.str 0 10)))
    (check (doit src.len src.len)	=> (list src.len src.str))

    #f)

  #t)


(parametrise ((check-test-name		'get-string-all))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(get-string-all 123))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-string-all port)))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-string-all port)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (get-string-all port)))
    => #t)

;;; --------------------------------------------------------------------
;;; input from a string port

  (check	;no data available
      (let ((port (open-string-input-port "")))
	(get-string-all port))
    => (eof-object))

  (check	;one char available
      (let ((port (open-string-input-port "A")))
	(get-string-all port))
    => "A")

  (let* ((src.len 100)
	 (src.str (let ((str (make-string src.len)))
		    (do ((i 0 (+ 1 i)))
			((= i src.len)
			 str)
		      (string-set! str i (integer->char i))))))

    (check
	(let ((port (open-string-input-port src.str)))
	  (get-string-all port))
      => src.str)

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with Latin-1 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (latin-1-codec)))))
	(get-string-all port))
    => (eof-object))

  (check	;some data available
      (let ((port (open-bytevector-input-port '#vu8(65 66 67 68)
					      (make-transcoder (latin-1-codec)))))
	(get-string-all port))
    => "ABCD")

  (let* ((src.len 100)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->latin1 src.str))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (latin-1-codec)))))
		    (get-string-all port)))))

    (check (doit src.len)	=> src.str)

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with UTF-8 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (utf-8-codec)))))
	(get-string-all port))
    => (eof-object))

  (check	;some data available
      (let ((port (open-bytevector-input-port '#vu8(65 66 67 68)
					      (make-transcoder (utf-8-codec)))))
	(get-string-all port))
    => "ABCD")

  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf8 src.str))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-8-codec)))))
		    (get-string-all port)))))

    (check (doit src.len)	=> src.str)

    #f)

;;; --------------------------------------------------------------------
;;; input from a bytevector with UTF-16 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (utf-16-codec)))))
	(get-string-all port))
    => (eof-object))

  (check	;some data available
      (let ((port (open-bytevector-input-port (string->utf16 "ABCD" (endianness big))
					      (make-transcoder (utf-16-codec)))))
	(get-string-all port))
    => "ABCD")

  ;; big endian, default
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf16 src.str (endianness big)))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (get-string-all port)))))

    (check (doit src.len)	=> src.str)

    #f)

  ;; little endian, with bom
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (bytevector-append BYTE-ORDER-MARK-UTF-16-LE
				     (string->utf16 src.str (endianness little))))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (get-string-all port)))))

    (check (doit src.len)	=> src.str)
    #f)

  ;; big endian, with bom
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (bytevector-append BYTE-ORDER-MARK-UTF-16-BE
				     (string->utf16 src.str (endianness big))))
	 (doit	(lambda (count)
		  (let ((port (open-bytevector-input-port src.bv (make-transcoder (utf-16-codec)))))
		    (get-string-all port)))))

    (check (doit src.len)	=> src.str)
    #f)

  #t)


(parametrise ((check-test-name	'read-line))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(read-line 123))
    => '(123))

  (check	;argument is not an input port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (read-line port)))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (read-line port)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-input-port port)
	  (read-line port)))
    => #t)

;;; --------------------------------------------------------------------
;;; input from a string port

  (check	;no data available
      (let ((port (open-string-input-port "")))
	(read-line port))
    => (eof-object))

  (check	;one char available
      (let ((port (open-string-input-port "A")))
	(read-line port))
    => "A")

  (check	;newline
      (let ((port (open-string-input-port "\n")))
	(read-line port))
    => "")

  (check	;one char and newline
      (let ((port (open-string-input-port "A\n")))
	(read-line port))
    => "A")

  (check	;string and newline
      (let ((port (open-string-input-port "ciao\n")))
	(read-line port))
    => "ciao")

  ;; long string, no newline
  (let* ((src.len 100)
	 (src.str (let ((str (make-string src.len)))
		    (do ((i 0 (+ 1 i)))
			((= i src.len)
			 str)
		      (string-set! str i (let ((ch (integer->char i)))
					   (if (char=? ch #\newline)
					       #\A
					     ch)))))))

    (check
	(let ((port (open-string-input-port src.str)))
	  (read-line port))
      => src.str)

    #f)

  (check	;multiple lines
      (let ((port (open-string-input-port "ciao\nhello\nsalut\n")))
	(let ((L1 (read-line port))
	      (L2 (read-line port))
	      (L3 (read-line port)))
	  (list L1 L2 L3 (port-eof? port))))
    => '("ciao" "hello" "salut" #t))

;;; --------------------------------------------------------------------
;;; input from a bytevector with Latin-1 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (latin-1-codec)))))
	(read-line port))
    => (eof-object))

  (check	;some data available, no newline
      (let ((port (open-bytevector-input-port '#vu8(65 66 67 68)
					      (make-transcoder (latin-1-codec)))))
	(read-line port))
    => "ABCD")

  (check	;some data available, newline
      (let ((port (open-bytevector-input-port '#vu8(65 66 67 10 68)
					      (make-transcoder (latin-1-codec)))))
	(read-line port))
    => "ABC")

;;; --------------------------------------------------------------------
;;; input from a bytevector with UTF-8 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (utf-8-codec)))))
	(read-line port))
    => (eof-object))

  (check	;some data available, no newline
      (let ((port (open-bytevector-input-port (string->utf8 "ABCD")
					      (make-transcoder (utf-8-codec)))))
	(read-line port))
    => "ABCD")

  (check	;some data available, newline
      (let ((port (open-bytevector-input-port (string->utf8 "ABC\nD")
					      (make-transcoder (utf-8-codec)))))
	(read-line port))
    => "ABC")

;;; --------------------------------------------------------------------
;;; input from a bytevector with UTF-16 encoding

  (check	;no data available
      (let ((port (open-bytevector-input-port '#vu8() (make-transcoder (utf-16-codec)))))
	(read-line port))
    => (eof-object))

  (check	;some data available, no newline
      (let ((port (open-bytevector-input-port (string->utf16 "ABCD" (endianness big))
					      (make-transcoder (utf-16-codec)))))
	(read-line port))
    => "ABCD")

  (check	;some data available, newline
      (let ((port (open-bytevector-input-port (string->utf16 "ABC\nD" (endianness big))
					      (make-transcoder (utf-16-codec)))))
	(read-line port))
    => "ABC")

  (check	;some data available, no newline
      (let ((port (open-bytevector-input-port (bytevector-append
					       BYTE-ORDER-MARK-UTF-16-BE
					       (string->utf16 "ABCD" (endianness big)))
					      (make-transcoder (utf-16-codec)))))
	(read-line port))
    => "ABCD")

  (check	;some data available, newline
      (let ((port (open-bytevector-input-port (bytevector-append
					       BYTE-ORDER-MARK-UTF-16-BE
					       (string->utf16 "ABC\nD" (endianness big)))
					      (make-transcoder (utf-16-codec)))))
	(read-line port))
    => "ABC")

  #t)


(parametrise ((check-test-name			'put-u8)
	      (bytevector-port-buffer-size	8))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(put-u8 123 1))
    => '(123))

  (check	;argument is not an output port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (put-u8 port 1)))
    => #t)

  (check	;argument is not a binary port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (put-u8 port 1)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-output-port port)
	  (put-u8 port 1)))
    => #t)

;;; --------------------------------------------------------------------
;;; octet argument validation

  (check	;octet is not an integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port (%open-disposable-binary-output-port)))
	  (put-u8 port #\a)))
    => '(#\a))

  (check	;octet is not an exact integer
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port (%open-disposable-binary-output-port)))
	  (put-u8 port 1.0)))
    => '(1.0))

  (check	;octet is negative
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port (%open-disposable-binary-output-port)))
	  (put-u8 port -1)))
    => '(-1))

  (check	;octet is too big
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port (%open-disposable-binary-output-port)))
	  (put-u8 port 256)))
    => '(256))

  (check	;octet is not a fixnum
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port (%open-disposable-binary-output-port)))
	  (put-u8 port (+ 1 (greatest-fixnum)))))
    => (list (+ 1 (greatest-fixnum))))

;;; --------------------------------------------------------------------

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-u8 port 100)
	(extract))
    => '#vu8(100))

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-u8 port 0)
	(put-u8 port 255)
	(extract))
    => '#vu8(0 255))

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port)))
	(do ((i 0 (+ 1 i)))
	    ((= i 10)
	     (extract))
	  (put-u8 port i)))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  #t)


(parametrise ((check-test-name			'put-bytevector)
	      (bytevector-port-buffer-size	8))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(put-bytevector 123 '#vu8()))
    => '(123))

  (check	;argument is not an output port
      (let ((port (%open-disposable-binary-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (put-bytevector port '#vu8())))
    => #t)

  (check	;argument is not a binary port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (put-bytevector port '#vu8())))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-output-port port)
	  (put-bytevector port '#vu8())))
    => #t)

;;; --------------------------------------------------------------------
;;; bytevector argument validation

  (check	;argument is not a bytevector
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (put-bytevector port #\a)))
    => '(#\a))

;;; --------------------------------------------------------------------
;;; start argument validation

  (check	;argument is not an integer
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (put-bytevector port '#vu8() #\a)))
    => '(#\a))

  (check	;argument is not an exact integer
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (put-bytevector port '#vu8() 1.0)))
    => '(1.0))

  (check	;argument is not a fixnum
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (put-bytevector port '#vu8() (+ 1 (greatest-fixnum)))))
    => (list (+ 1 (greatest-fixnum))))

  (check	;argument is negative
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (put-bytevector port '#vu8() -1)))
    => '(-1))

  (check	;argument is out of range for bytevector
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (put-bytevector port '#vu8() 1)))
    => '(1))

;;; --------------------------------------------------------------------
;;; count argument validation

  (check	;argument is not an integer
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8() 0 #\a)))
    => '(#\a))

  (check	;argument is not an exact integer
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8() 0 1.0)))
    => '(1.0))

  (check	;argument is not a fixnum
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8() 0 (+ 1 (greatest-fixnum)))))
    => (list (+ 1 (greatest-fixnum))))

  (check	;argument is negative
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8() 0 -1)))
    => '(-1))

  (check	;argument is out of range for bytevector
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8() 0 1)))
    => '(0 1 0))

  (check	;argument is out of range for bytevector
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9) 0 11)))
    => '(0 11 10))

  (check	;argument is out of range for bytevector
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9) 5 6)))
    => '(5 6 10))

;;; --------------------------------------------------------------------
;;; no start, no count

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8())
	(extract))
    => '#vu8())

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0))
	(extract))
    => '#vu8(0))

  (check	;bytevector length equals buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7))
	(extract))
    => '#vu8(0 1 2 3 4 5 6 7))

  (check 	;bytevector length greater than buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(extract))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check	;total length greater than buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5))
	(put-bytevector port '#vu8(6 7 8 9))
	(extract))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; no count

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8() 0)
	(extract))
    => '#vu8())

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0) 0)
	(extract))
    => '#vu8(0))

  (check	;bytevector length equals buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(99 99 0 1 2 3 4 5 6 7) 2)
	(extract))
    => '#vu8(0 1 2 3 4 5 6 7))

  (check	;bytevector length greater than buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(99 99 0 1 2 3 4 5 6 7 8 9) 2)
	(extract))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check	;total length greater than buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(99 99 0 1 2 3 4 5) 2)
	(put-bytevector port '#vu8(99 99 99 6 7 8 9) 3)
	(extract))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; start and count

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
  	(put-bytevector port '#vu8() 0 0)
  	(extract))
    => '#vu8())

  (check
      (let-values (((port extract) (open-bytevector-output-port)))
  	(put-bytevector port '#vu8(0) 0 1)
  	(extract))
    => '#vu8(0))

  (check	;bytevector length equals buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
  	(put-bytevector port '#vu8(99 99 0 1 2 3 4 5 6 7 98 98) 2 8)
  	(extract))
    => '#vu8(0 1 2 3 4 5 6 7))

  (check	;bytevector length greater than buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
  	(put-bytevector port '#vu8(99 99 0 1 2 3 4 5 6 7 8 9 98 98) 2 10)
  	(extract))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check	;total length greater than buffer size
      (let-values (((port extract) (open-bytevector-output-port)))
  	(put-bytevector port '#vu8(99 99 0 1 2 3 4 5 98 98) 2 6)
  	(put-bytevector port '#vu8(99 99 99 6 7 8 9 98 98) 3 4)
  	(extract))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  #t)


(parametrise ((check-test-name			'put-char)
	      (bytevector-port-buffer-size	8))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(put-char 123 #\a))
    => '(123))

  (check	;argument is not an output port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (put-char port #\a)))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (put-char port #\a)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-output-port port)
	  (put-char port #\a)))
    => #t)

;;; --------------------------------------------------------------------
;;; char argument validation

  (check	;argument is not a char
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port (%open-disposable-textual-output-port)))
	  (put-char port 123)))
    => '(123))

;;; --------------------------------------------------------------------
;;; string port

  (check
      (let-values (((port extract) (open-string-output-port)))
	(put-char port #\A)
	(extract))
    => "A")

  (check
      (let-values (((port extract) (open-string-output-port)))
	(put-char port #\A)
	(put-char port #\B)
	(extract))
    => "AB")

  (check
      (let-values (((port extract) (open-string-output-port)))
	(let ((str TEST-STRING-FOR-UTF-8))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (put-char port (string-ref str i)))))
    => TEST-STRING-FOR-UTF-8)

;;; --------------------------------------------------------------------
;;; UTF-8 transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(put-char port #\A)
	(utf8->string (extract)))
    => "A")

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(put-char port #\A)
	(put-char port #\B)
	(utf8->string (extract)))
    => "AB")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(do ((i 0 (+ 1 i)))
	    ((= i 10)
	     (extract))
	  (put-char port (integer->char i))))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(let ((str TEST-STRING-FOR-UTF-8))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (put-char port (string-ref str i)))))
    => TEST-BYTEVECTOR-FOR-UTF-8)

;;; --------------------------------------------------------------------
;;; UTF-16 LE transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16le-codec)))))
	(put-char port #\A)
	(utf16->string (extract) (endianness little)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16le-codec)))))
	(let ((str TEST-STRING-FOR-UTF-16-LE))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (put-char port (string-ref str i)))))
    => TEST-BYTEVECTOR-FOR-UTF-16-LE)

;;; --------------------------------------------------------------------
;;; UTF-16 BE transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16be-codec)))))
	(put-char port #\A)
	(utf16->string (extract) (endianness big)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16be-codec)))))
	(let ((str TEST-STRING-FOR-UTF-16-BE))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (put-char port (string-ref str i)))))
    => TEST-BYTEVECTOR-FOR-UTF-16-BE)

;;; --------------------------------------------------------------------
;;; Latin-1 transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (latin-1-codec)))))
	(put-char port #\A)
	(latin1->string (extract)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (latin-1-codec)))))
	(let ((str TEST-STRING-FOR-LATIN-1))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (put-char port (string-ref str i)))))
    => TEST-BYTEVECTOR-FOR-LATIN-1)

  #t)


(parametrise ((check-test-name			'write-char)
	      (bytevector-port-buffer-size	8))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(write-char #\a 123))
    => '(123))

  (check	;argument is not an output port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (write-char #\a port)))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (write-char #\a port)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-output-port port)
	  (write-char #\a port)))
    => #t)

;;; --------------------------------------------------------------------
;;; char argument validation

  (check	;argument is not a char
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port (%open-disposable-textual-output-port)))
	  (write-char 123 port)))
    => '(123))

;;; --------------------------------------------------------------------
;;; string port

  (check
      (let-values (((port extract) (open-string-output-port)))
	(write-char #\A port)
	(extract))
    => "A")

  (check
      (let-values (((port extract) (open-string-output-port)))
	(write-char #\A port)
	(write-char #\B port)
	(extract))
    => "AB")

  (check
      (let-values (((port extract) (open-string-output-port)))
	(let ((str TEST-STRING-FOR-UTF-8))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (write-char (string-ref str i) port))))
    => TEST-STRING-FOR-UTF-8)

;;; --------------------------------------------------------------------
;;; UTF-8 transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(write-char #\A port)
	(utf8->string (extract)))
    => "A")

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(write-char #\A port)
	(write-char #\B port)
	(utf8->string (extract)))
    => "AB")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(do ((i 0 (+ 1 i)))
	    ((= i 10)
	     (extract))
	  (write-char (integer->char i) port)))
    => '#vu8(0 1 2 3 4 5 6 7 8 9))

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(let ((str TEST-STRING-FOR-UTF-8))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (write-char (string-ref str i) port))))
    => TEST-BYTEVECTOR-FOR-UTF-8)

;;; --------------------------------------------------------------------
;;; UTF-16 LE transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16le-codec)))))
	(write-char #\A port)
	(utf16->string (extract) (endianness little)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16le-codec)))))
	(let ((str TEST-STRING-FOR-UTF-16-LE))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (write-char (string-ref str i) port))))
    => TEST-BYTEVECTOR-FOR-UTF-16-LE)

;;; --------------------------------------------------------------------
;;; UTF-16 BE transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16be-codec)))))
	(write-char #\A port)
	(utf16->string (extract) (endianness big)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16be-codec)))))
	(let ((str TEST-STRING-FOR-UTF-16-BE))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (write-char (string-ref str i) port))))
    => TEST-BYTEVECTOR-FOR-UTF-16-BE)

;;; --------------------------------------------------------------------
;;; Latin-1 transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (latin-1-codec)))))
	(write-char #\A port)
	(latin1->string (extract)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (latin-1-codec)))))
	(let ((str TEST-STRING-FOR-LATIN-1))
	  (do ((i 0 (+ 1 i)))
	      ((= i (string-length str))
	       (extract))
	    (write-char (string-ref str i) port))))
    => TEST-BYTEVECTOR-FOR-LATIN-1)

  #t)


(parametrise ((check-test-name			'put-string)
	      (bytevector-port-buffer-size	8))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(put-string 123 "a"))
    => '(123))

  (check	;argument is not an output port
      (let ((port (%open-disposable-textual-input-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (put-string port "a")))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (put-string port "a")))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-output-port)))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (close-output-port port)
	  (put-string port "a")))
    => #t)

;;; --------------------------------------------------------------------
;;; char argument validation

  (check	;argument is not a char
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(let ((port (%open-disposable-textual-output-port)))
	  (put-string port 123)))
    => '(123))

;;; --------------------------------------------------------------------
;;; string port

  (check
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "A")
	(extract))
    => "A")

  (check
      (let-values (((port extract) (open-string-output-port)))
	(put-string port "A")
	(put-string port "B")
	(extract))
    => "AB")

  (check
      (let-values (((port extract) (open-string-output-port)))
	(put-string port TEST-STRING-FOR-UTF-8)
	(extract))
    => TEST-STRING-FOR-UTF-8)

;;; --------------------------------------------------------------------
;;; UTF-8 transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(put-string port "A")
	(utf8->string (extract)))
    => "A")

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(put-string port "A")
	(put-string port "B")
	(utf8->string (extract)))
    => "AB")

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(put-string port TEST-STRING-FOR-UTF-8)
	(extract))
    => TEST-BYTEVECTOR-FOR-UTF-8)

;;; --------------------------------------------------------------------
;;; UTF-16 LE transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16le-codec)))))
	(put-string port "A")
	(utf16->string (extract) (endianness little)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16le-codec)))))
	(put-string port TEST-STRING-FOR-UTF-16-LE)
	(extract))
    => TEST-BYTEVECTOR-FOR-UTF-16-LE)

;;; --------------------------------------------------------------------
;;; UTF-16 BE transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16be-codec)))))
	(put-string port "A")
	(utf16->string (extract) (endianness big)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16be-codec)))))
	(put-string port TEST-STRING-FOR-UTF-16-BE)
	(extract))
    => TEST-BYTEVECTOR-FOR-UTF-16-BE)

;;; --------------------------------------------------------------------
;;; Latin-1 transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (latin-1-codec)))))
	(put-string port "A")
	(latin1->string (extract)))
    => "A")

  (check	;fill the buffer
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (latin-1-codec)))))
	(put-string port TEST-STRING-FOR-LATIN-1)
	(extract))
    => TEST-BYTEVECTOR-FOR-LATIN-1)

  #t)


(parametrise ((check-test-name	'newline))

;;; --------------------------------------------------------------------
;;; port argument validation

  (check	;argument is not a port
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(newline 123))
    => '(123))

  (check	;argument is not an output port
      (let ((port (%open-disposable-textual-input-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (eq? port (car (condition-irritants E))))
  		  (else E))
  	  (newline port)))
    => #t)

  (check	;argument is not a textual port
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (eq? port (car (condition-irritants E))))
  		  (else E))
  	  (newline port)))
    => #t)

  (check	;argument is not an open port
      (let ((port (%open-disposable-textual-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (eq? port (car (condition-irritants E))))
  		  (else E))
  	  (close-output-port port)
  	  (newline port)))
    => #t)

;;; --------------------------------------------------------------------
;;; string port

  (check
      (let-values (((port extract) (open-string-output-port)))
	(newline port)
	(extract))
    => "\n")

;;; --------------------------------------------------------------------
;;; UTF-8 transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-8-codec)))))
	(newline port)
	(utf8->string (extract)))
    => "\n")

;;; --------------------------------------------------------------------
;;; UTF-16 LE transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16le-codec)))))
	(newline port)
	(utf16->string (extract) (endianness little)))
    => "\n")

;;; --------------------------------------------------------------------
;;; UTF-16 BE transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (utf-16be-codec)))))
	(newline port)
	(utf16->string (extract) (endianness big)))
    => "\n")

;;; --------------------------------------------------------------------
;;; Latin-1 transcoder

  (check
      (let-values (((port extract) (open-bytevector-output-port (make-transcoder (latin-1-codec)))))
	(newline port)
	(latin1->string (extract)))
    => "\n")

  #t)


(parametrise ((check-test-name		'open-file-input-port)
	      (test-pathname		(make-test-pathname "open-file-input-port.bin"))
	      (input-file-buffer-size	9))

  (define-syntax with-binary-input-test-pathname
    (syntax-rules ()
      ((_ ?open-form)
       (begin
	 (create-binary-test-pathname)
	 (let ((port ?open-form))
	   (unwind-protect
	       (get-bytevector-all port)
	     (close-input-port port)
	     (cleanup-test-pathname)))))))

  (define-syntax with-textual-input-test-pathname
    (syntax-rules ()
      ((_ ?open-form)
       (begin
	 (create-binary-test-pathname)
	 (let ((port ?open-form))
	   (unwind-protect
	       (get-string-all port)
	     (close-input-port port)
	     (cleanup-test-pathname)))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-input-port 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; file-options argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-input-port "123" 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; buffer-mode argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-input-port "123" (file-options) 'ciao))
    => '(ciao))

;;; --------------------------------------------------------------------
;;; transcoder argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-input-port "123" (file-options) (buffer-mode block) 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; reading whole binary data

  (check
      (with-binary-input-test-pathname
       (open-file-input-port (test-pathname)))
    => (bindata-hundreds.bv))

  (check
      (with-binary-input-test-pathname
       (open-file-input-port (test-pathname) (file-options)))
    => (bindata-hundreds.bv))

  (check
      (with-binary-input-test-pathname
       (open-file-input-port (test-pathname) (file-options) (buffer-mode block)))
    => (bindata-hundreds.bv))

;;; --------------------------------------------------------------------
;;; reading whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-LATIN-1)))
	(with-textual-input-test-pathname
	 (open-file-input-port (test-pathname) (file-options) (buffer-mode block)
			       (make-transcoder (latin-1-codec)))))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-8)))
	(with-textual-input-test-pathname
	 (open-file-input-port (test-pathname) (file-options) (buffer-mode block)
			       (make-transcoder (utf-8-codec)))))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       SPECIAL1-TEST-BYTEVECTOR-FOR-UTF-8)))
	(with-textual-input-test-pathname
	 (open-file-input-port (test-pathname) (file-options) (buffer-mode block)
			       (make-transcoder (utf-8-codec)))))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-16-LE)))
	(with-textual-input-test-pathname
	 (open-file-input-port (test-pathname) (file-options) (buffer-mode block)
			       (make-transcoder (utf-16le-codec)))))
    => TEST-STRING-FOR-UTF-16-LE)


  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-16-BE)))
	(with-textual-input-test-pathname
	 (open-file-input-port (test-pathname) (file-options) (buffer-mode block)
			       (make-transcoder (utf-16be-codec)))))
    => TEST-STRING-FOR-UTF-16-BE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-16-LE/BOM)))
	(with-textual-input-test-pathname
	 (open-file-input-port (test-pathname) (file-options) (buffer-mode block)
			       (make-transcoder (utf-16-codec)))))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-16-BE/BOM)))
	(with-textual-input-test-pathname
	 (open-file-input-port (test-pathname) (file-options) (buffer-mode block)
			       (make-transcoder (utf-16-codec)))))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name		'open-input-file)
	      (test-pathname		(make-test-pathname "open-input-file.bin"))
	      (input-file-buffer-size	9))

  (define-syntax with-textual-input-test-pathname
    (syntax-rules ()
      ((_ ?open-form)
       (begin
	 (create-binary-test-pathname)
	 (let ((port ?open-form))
	   (unwind-protect
	       (get-string-all port)
	     (close-input-port port)
	     (cleanup-test-pathname)))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-input-file 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; reading whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-LATIN-1))
		    (native-transcoder	(make-transcoder (latin-1-codec))))
  	(with-textual-input-test-pathname
  	 (open-input-file (test-pathname))))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-8)))
  	(with-textual-input-test-pathname
  	 (open-input-file (test-pathname))))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       SPECIAL1-TEST-BYTEVECTOR-FOR-UTF-8)))
  	(with-textual-input-test-pathname
  	 (open-input-file (test-pathname))))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-LE))
		    (native-transcoder	(make-transcoder (utf-16le-codec))))
  	(with-textual-input-test-pathname
  	 (open-input-file (test-pathname))))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-BE))
		    (native-transcoder	(make-transcoder (utf-16be-codec))))
  	(with-textual-input-test-pathname
  	 (open-input-file (test-pathname))))
    => TEST-STRING-FOR-UTF-16-BE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-LE/BOM))
		    (native-transcoder	(make-transcoder (utf-16-codec))))
  	(with-textual-input-test-pathname
  	 (open-input-file (test-pathname))))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-BE/BOM))
		    (native-transcoder	(make-transcoder (utf-16-codec))))
  	(with-textual-input-test-pathname
  	 (open-input-file (test-pathname))))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name		'with-input-from-file)
	      (test-pathname		(make-test-pathname "with-input-from-file.bin"))
	      (input-file-buffer-size	9))

  (define-syntax with-textual-input-test-pathname
    (syntax-rules ()
      ((_)
       (begin
	 (create-binary-test-pathname)
	 (unwind-protect
	     (with-input-from-file (test-pathname)
	       (lambda ()
		 (let ((port (current-input-port)))
		   (get-string-all port))))
	   (cleanup-test-pathname))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-input-from-file 123 values))
    => '(123))

;;; --------------------------------------------------------------------
;;; procedure argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-input-from-file "ciao" 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; reading whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-LATIN-1))
		    (native-transcoder	(make-transcoder (latin-1-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-8)))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       SPECIAL1-TEST-BYTEVECTOR-FOR-UTF-8)))
  	(with-textual-input-test-pathname))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-LE))
		    (native-transcoder	(make-transcoder (utf-16le-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-BE))
		    (native-transcoder	(make-transcoder (utf-16be-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-16-BE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-LE/BOM))
		    (native-transcoder	(make-transcoder (utf-16-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-BE/BOM))
		    (native-transcoder	(make-transcoder (utf-16-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name		'call-with-input-file)
	      (test-pathname		(make-test-pathname "call-with-input-file.bin"))
	      (input-file-buffer-size	9))

  (define-syntax with-textual-input-test-pathname
    (syntax-rules ()
      ((_)
       (begin
	 (create-binary-test-pathname)
	 (unwind-protect
	     (call-with-input-file (test-pathname)
	       (lambda (port)
		 (get-string-all port)))
	   (cleanup-test-pathname))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-input-file 123 values))
    => '(123))

;;; --------------------------------------------------------------------
;;; procedure argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-input-file "ciao" 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; reading whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-LATIN-1))
		    (native-transcoder	(make-transcoder (latin-1-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-8)))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       SPECIAL1-TEST-BYTEVECTOR-FOR-UTF-8)))
  	(with-textual-input-test-pathname))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-LE))
		    (native-transcoder	(make-transcoder (utf-16le-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-BE))
		    (native-transcoder	(make-transcoder (utf-16be-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-16-BE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-LE/BOM))
		    (native-transcoder	(make-transcoder (utf-16-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-BYTEVECTOR-FOR-UTF-16-BE/BOM))
		    (native-transcoder	(make-transcoder (utf-16-codec))))
  	(with-textual-input-test-pathname))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name		'open-file-output-port)
	      (test-pathname		(make-test-pathname "open-file-output-port.bin"))
	      (input-file-buffer-size	9))

  (define-syntax with-binary-output-test-pathname
    (syntax-rules ()
      ((_ ?open-form)
       (begin
	 (cleanup-test-pathname)
	 (unwind-protect
	     (let ((port ?open-form))
	       (unwind-protect
		   (put-bytevector port ((test-pathname-data-func)))
		 (close-output-port port))
	       (binary-read-test-pathname))
	   (cleanup-test-pathname))))))

  (define-syntax with-textual-output-test-pathname
    (syntax-rules ()
      ((_ ?codec ?open-form)
       (begin
	 (cleanup-test-pathname)
	 (unwind-protect
	     (let ((port ?open-form))
	       (unwind-protect
		   (put-string port ((test-pathname-data-func)))
		 (close-output-port port))
	       (textual-read-test-pathname (make-transcoder ?codec)))
	   (cleanup-test-pathname))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-output-port 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; file-options argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-output-port "123" 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; buffer-mode argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-output-port "123" (file-options) 'ciao))
    => '(ciao))

;;; --------------------------------------------------------------------
;;; transcoder argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-output-port "123" (file-options) (buffer-mode block) 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; writing whole binary data

  (check
      (with-binary-output-test-pathname
       (open-file-output-port (test-pathname)))
    => (bindata-hundreds.bv))

  (check
      (with-binary-output-test-pathname
       (open-file-output-port (test-pathname) (file-options)))
    => (bindata-hundreds.bv))

  (check
      (with-binary-output-test-pathname
       (open-file-output-port (test-pathname) (file-options) (buffer-mode block)))
    => (bindata-hundreds.bv))

;;; --------------------------------------------------------------------
;;; writing whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-LATIN-1)))
  	(with-textual-output-test-pathname (latin-1-codec)
  	 (open-file-output-port (test-pathname) (file-options) (buffer-mode block)
  				(make-transcoder (latin-1-codec)))))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-8)))
  	(with-textual-output-test-pathname (utf-8-codec)
  	 (open-file-output-port (test-pathname) (file-options) (buffer-mode block)
  				(make-transcoder (utf-8-codec)))))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       SPECIAL1-TEST-STRING-FOR-UTF-8)))
  	(with-textual-output-test-pathname (utf-8-codec)
  	 (open-file-output-port (test-pathname) (file-options) (buffer-mode block)
  				(make-transcoder (utf-8-codec)))))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check	;default to big endian
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-BE)))
  	(with-textual-output-test-pathname (utf-16be-codec)
  	 (open-file-output-port (test-pathname) (file-options) (buffer-mode block)
  				(make-transcoder (utf-16-codec)))))
    => TEST-STRING-FOR-UTF-16-BE)

  (check	;explicit little endianness selection
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-LE)))
  	(with-textual-output-test-pathname (utf-16le-codec)
  	 (open-file-output-port (test-pathname) (file-options) (buffer-mode block)
  				(make-transcoder (utf-16le-codec)))))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-BE)))
  	(with-textual-output-test-pathname (utf-16be-codec)
  	 (open-file-output-port (test-pathname) (file-options) (buffer-mode block)
  				(make-transcoder (utf-16be-codec)))))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name		'open-output-file)
	      (test-pathname		(make-test-pathname "open-output-file.bin"))
	      (input-file-buffer-size	9))

  (define-syntax with-textual-output-test-pathname
    (syntax-rules ()
      ((_ ?codec)
       (begin
	 (cleanup-test-pathname)
	 (unwind-protect
	     (let ((port (open-output-file (test-pathname))))
	       (unwind-protect
		   (put-string port ((test-pathname-data-func)))
		 (close-output-port port))
	       (textual-read-test-pathname (make-transcoder ?codec)))
	   (cleanup-test-pathname))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-output-file 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; writing whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-LATIN-1))
		    (native-transcoder	(make-transcoder (latin-1-codec))))
  	(with-textual-output-test-pathname (latin-1-codec)))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-8))
		    (native-transcoder	(make-transcoder (utf-8-codec))))
  	(with-textual-output-test-pathname (utf-8-codec)))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       SPECIAL1-TEST-STRING-FOR-UTF-8))
		    (native-transcoder	(make-transcoder (utf-8-codec))))
  	(with-textual-output-test-pathname (utf-8-codec)))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-LE))
		    (native-transcoder	(make-transcoder (utf-16le-codec))))
  	(with-textual-output-test-pathname (utf-16le-codec)))
    => TEST-STRING-FOR-UTF-16-LE)


  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-BE))
		    (native-transcoder	(make-transcoder (utf-16be-codec))))
  	(with-textual-output-test-pathname (utf-16be-codec)))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name		'with-output-to-file)
	      (test-pathname		(make-test-pathname "with-output-to-file.bin"))
	      (input-file-buffer-size	9))

  (define-syntax with-textual-output-test-pathname
    (syntax-rules ()
      ((_ ?codec)
       (begin
	 (cleanup-test-pathname)
	 (unwind-protect
	     (begin
	       (with-output-to-file (test-pathname)
		 (lambda ()
		   (put-string (current-output-port) ((test-pathname-data-func)))))
	       (textual-read-test-pathname (make-transcoder ?codec)))
	   (cleanup-test-pathname))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-output-to-file 123 values))
    => '(123))

;;; --------------------------------------------------------------------
;;; procedure argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(with-output-to-file "ciao" 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; writing whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-LATIN-1))
		    (native-transcoder	(make-transcoder (latin-1-codec))))
  	(with-textual-output-test-pathname (latin-1-codec)))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-8))
		    (native-transcoder	(make-transcoder (utf-8-codec))))
  	(with-textual-output-test-pathname (utf-8-codec)))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       SPECIAL1-TEST-STRING-FOR-UTF-8))
		    (native-transcoder	(make-transcoder (utf-8-codec))))
  	(with-textual-output-test-pathname (utf-8-codec)))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-LE))
		    (native-transcoder	(make-transcoder (utf-16le-codec))))
  	(with-textual-output-test-pathname (utf-16le-codec)))
    => TEST-STRING-FOR-UTF-16-LE)


  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-BE))
		    (native-transcoder	(make-transcoder (utf-16be-codec))))
  	(with-textual-output-test-pathname (utf-16be-codec)))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name		'call-with-output-file)
	      (test-pathname		(make-test-pathname "call-with-output-file.bin"))
	      (input-file-buffer-size	9))

  (define-syntax with-textual-output-test-pathname
    (syntax-rules ()
      ((_ ?codec)
       (begin
	 (cleanup-test-pathname)
	 (unwind-protect
	     (begin
	       (call-with-output-file (test-pathname)
		 (lambda (port)
		   (put-string port ((test-pathname-data-func)))))
	       (textual-read-test-pathname (make-transcoder ?codec)))
	   (cleanup-test-pathname))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-output-file 123 values))
    => '(123))

;;; --------------------------------------------------------------------
;;; procedure argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(call-with-output-file "ciao" 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; writing whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-LATIN-1))
		    (native-transcoder	(make-transcoder (latin-1-codec))))
  	(with-textual-output-test-pathname (latin-1-codec)))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-8))
		    (native-transcoder	(make-transcoder (utf-8-codec))))
  	(with-textual-output-test-pathname (utf-8-codec)))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       SPECIAL1-TEST-STRING-FOR-UTF-8))
		    (native-transcoder	(make-transcoder (utf-8-codec))))
  	(with-textual-output-test-pathname (utf-8-codec)))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-LE))
		    (native-transcoder	(make-transcoder (utf-16le-codec))))
  	(with-textual-output-test-pathname (utf-16le-codec)))
    => TEST-STRING-FOR-UTF-16-LE)


  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-BE))
		    (native-transcoder	(make-transcoder (utf-16be-codec))))
  	(with-textual-output-test-pathname (utf-16be-codec)))
    => TEST-STRING-FOR-UTF-16-BE)

  #t)


(parametrise ((check-test-name		'open-file-input-output-port)
	      (test-pathname		(make-test-pathname "open-file-input-output-port.bin")))

  (define-syntax with-binary-input-test-pathname
    (syntax-rules ()
      ((_ ?open-form)
       (begin
	 (create-binary-test-pathname)
	 (let ((port ?open-form))
	   (unwind-protect
	       (get-bytevector-all port)
	     (close-input-port port)
	     (cleanup-test-pathname)))))))

  (define-syntax with-textual-input-test-pathname
    (syntax-rules ()
      ((_ ?open-form)
       (begin
	 (create-binary-test-pathname)
	 (let ((port ?open-form))
	   (unwind-protect
	       (get-string-all port)
	     (close-input-port port)
	     (cleanup-test-pathname)))))))

;;; --------------------------------------------------------------------

  (define-syntax with-binary-output-test-pathname
    (syntax-rules ()
      ((_ ?open-form)
       (begin
	 (cleanup-test-pathname)
	 (unwind-protect
	     (let ((port ?open-form))
	       (unwind-protect
		   (put-bytevector port ((test-pathname-data-func)))
		 (close-output-port port))
	       (binary-read-test-pathname))
	   (cleanup-test-pathname))))))

  (define-syntax with-textual-output-test-pathname
    (syntax-rules ()
      ((_ ?codec ?open-form)
       (begin
	 (cleanup-test-pathname)
	 (unwind-protect
	     (let ((port ?open-form))
	       (unwind-protect
		   (put-string port ((test-pathname-data-func)))
		 (close-output-port port))
	       (textual-read-test-pathname (make-transcoder ?codec)))
	   (cleanup-test-pathname))))))

;;; --------------------------------------------------------------------

  (define-syntax with-binary-input/output-test-pathname
    (syntax-rules ()
      ((_ ?open-form ?data)
       (check.with-result
	 (create-binary-test-pathname)
	 (let ((port ?open-form))
	   (unwind-protect
	       (begin
		 (check.add-result (get-bytevector-all port))
		 (check.add-result (port-position port))
		 (set-port-position! port 0)
		 (check.add-result (port-position port))
		 (put-bytevector port ?data)
		 (check.add-result (port-position port))
		 (set-port-position! port 0)
		 (check.add-result (port-position port))
		 (check.add-result (get-bytevector-all port))
		 (port-position port))
	     (close-port port)
	     (cleanup-test-pathname)))))))

  (define-syntax with-textual-input/output-test-pathname
    (syntax-rules ()
      ((_ ?open-form ?data)
       (check.with-result
	 (create-textual-test-pathname (utf-8-codec))
	 (let ((port ?open-form))
	   (unwind-protect
	       (begin
		 (check.add-result (get-string-all port))
		 (check.add-result (port-position port))
		 (set-port-position! port 0)
		 (check.add-result (port-position port))
		 (put-string port ?data)
		 (check.add-result (port-position port))
		 (set-port-position! port 0)
		 (check.add-result (port-position port))
		 (check.add-result (get-string-all port))
		 (port-position port))
	     (close-port port)
	     (cleanup-test-pathname)))))))

;;; --------------------------------------------------------------------
;;; filename argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-input/output-port 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; file-options argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-input/output-port "123" 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; buffer-mode argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-input/output-port "123" (file-options) 'ciao))
    => '(ciao))

;;; --------------------------------------------------------------------
;;; transcoder argument validation

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(open-file-input/output-port "123" (file-options) (buffer-mode block) 123))
    => '(123))

;;; --------------------------------------------------------------------
;;; reading whole binary data

  (check
      (with-binary-input-test-pathname
       (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)))
    => (bindata-hundreds.bv))

  (check
      (with-binary-input-test-pathname
       (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)
  				    (buffer-mode block)))
    => (bindata-hundreds.bv))

;;; --------------------------------------------------------------------
;;; reading whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-LATIN-1)))
	(with-textual-input-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)
				      (buffer-mode block)
				      (make-transcoder (latin-1-codec)))))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-8)))
	(with-textual-input-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)
				      (buffer-mode block)
				      (make-transcoder (utf-8-codec)))))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       SPECIAL1-TEST-BYTEVECTOR-FOR-UTF-8)))
	(with-textual-input-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)
				      (buffer-mode block)
				      (make-transcoder (utf-8-codec)))))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-16-BE)))
	(with-textual-input-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)
				      (buffer-mode block)
				      (make-transcoder (utf-16-codec)))))
    => TEST-STRING-FOR-UTF-16-BE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-16-LE)))
	(with-textual-input-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)
				      (buffer-mode block)
				      (make-transcoder (utf-16le-codec)))))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       TEST-BYTEVECTOR-FOR-UTF-16-BE)))
	(with-textual-input-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)
				      (buffer-mode block)
				      (make-transcoder (utf-16be-codec)))))
    => TEST-STRING-FOR-UTF-16-BE)

;;; --------------------------------------------------------------------
;;; writing whole binary data

  (check
      (with-binary-output-test-pathname
       (open-file-input/output-port (test-pathname)))
    => (bindata-hundreds.bv))

  (check
      (with-binary-output-test-pathname
       (open-file-input/output-port (test-pathname) (file-options)))
    => (bindata-hundreds.bv))

  (check
      (with-binary-output-test-pathname
       (open-file-input/output-port (test-pathname) (file-options) (buffer-mode block)))
    => (bindata-hundreds.bv))

;;; --------------------------------------------------------------------
;;; writing whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-LATIN-1)))
  	(with-textual-output-test-pathname (latin-1-codec)
	  (open-file-input/output-port (test-pathname) (file-options) (buffer-mode block)
				       (make-transcoder (latin-1-codec)))))
    => TEST-STRING-FOR-LATIN-1)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-8)))
  	(with-textual-output-test-pathname (utf-8-codec)
	  (open-file-input/output-port (test-pathname) (file-options) (buffer-mode block)
				       (make-transcoder (utf-8-codec)))))
    => TEST-STRING-FOR-UTF-8)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       SPECIAL1-TEST-STRING-FOR-UTF-8)))
  	(with-textual-output-test-pathname (utf-8-codec)
	  (open-file-input/output-port (test-pathname) (file-options) (buffer-mode block)
				       (make-transcoder (utf-8-codec)))))
    => SPECIAL1-TEST-STRING-FOR-UTF-8)

  (check	;default to big endian
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-BE)))
  	(with-textual-output-test-pathname (utf-16be-codec)
	  (open-file-input/output-port (test-pathname) (file-options) (buffer-mode block)
				       (make-transcoder (utf-16-codec)))))
    => TEST-STRING-FOR-UTF-16-BE)

  (check	;explicit little endianness selection
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-LE)))
  	(with-textual-output-test-pathname (utf-16le-codec)
	  (open-file-input/output-port (test-pathname) (file-options) (buffer-mode block)
				       (make-transcoder (utf-16le-codec)))))
    => TEST-STRING-FOR-UTF-16-LE)

  (check
      (parametrise ((test-pathname-data-func (lambda ()
  					       TEST-STRING-FOR-UTF-16-BE)))
  	(with-textual-output-test-pathname (utf-16be-codec)
	  (open-file-input/output-port (test-pathname) (file-options) (buffer-mode block)
				       (make-transcoder (utf-16be-codec)))))
    => TEST-STRING-FOR-UTF-16-BE)

;;; --------------------------------------------------------------------
;;; reading and writing whole binary data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       (bindata-bytes.bv)))
		    (input/output-file-buffer-size 9))
	(with-binary-input/output-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-fail no-create no-truncate))
	 (bindata-bytes.bv)))
    => `(,(bindata-bytes.len)
	 (,(bindata-bytes.bv) ,(bindata-bytes.len) 0 ,(bindata-bytes.len) 0 ,(bindata-bytes.bv))))

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       (bindata-bytes.bv))))
	(with-binary-input/output-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-fail no-create no-truncate))
	 (bindata-bytes.bv)))
    => `(,(bindata-bytes.len)
	 (,(bindata-bytes.bv) ,(bindata-bytes.len) 0 ,(bindata-bytes.len) 0 ,(bindata-bytes.bv))))

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       (bindata-hundreds.bv))))
	(with-binary-input/output-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-create no-truncate))
	 (bindata-hundreds.bv)))
    => `(,(bindata-hundreds.len)
	 (,(bindata-hundreds.bv) ,(bindata-hundreds.len) 0
	  ,(bindata-hundreds.len) 0 ,(bindata-hundreds.bv))))

;;; --------------------------------------------------------------------
;;; reading and writing whole textual data

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       (textdata-bytes.str)))
		    (input/output-file-buffer-size 9))
	(with-textual-input/output-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-fail no-create no-truncate)
				      (buffer-mode block) (make-transcoder (utf-8-codec)))
	 (textdata-bytes.str)))
    => (let ((len (bytevector-length (string->utf8 (textdata-bytes.str)))))
	 `(,len (,(textdata-bytes.str) ,len 0 ,len 0 ,(textdata-bytes.str)))))

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       (textdata-bytes.str))))
	(with-textual-input/output-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-fail no-create no-truncate)
				      (buffer-mode block) (make-transcoder (utf-8-codec)))
	 (textdata-bytes.str)))
    => (let ((len (bytevector-length (string->utf8 (textdata-bytes.str)))))
	 `(,len (,(textdata-bytes.str) ,len 0 ,len 0 ,(textdata-bytes.str)))))

  (check
      (parametrise ((test-pathname-data-func (lambda ()
					       (textdata-hundreds.str))))
	(with-textual-input/output-test-pathname
	 (open-file-input/output-port (test-pathname) (file-options no-create no-truncate)
				      (buffer-mode block) (make-transcoder (utf-8-codec)))
	 (textdata-hundreds.str)))
    => (let ((len (bytevector-length (string->utf8 (textdata-hundreds.str)))))
	 `(,len (,(textdata-hundreds.str) ,len 0 ,len 0 ,(textdata-hundreds.str)))))

  #t)


(parametrise ((check-test-name	'eol-conversion-output-string))

  (define NEWLINE-CODE-POINT		#x000A) ;; U+000A
  (define LINEFEED-CODE-POINT		#x000A) ;; U+000A
  (define CARRIAGE-RETURN-CODE-POINT	#x000D) ;; U+000D
  (define NEXT-LINE-CODE-POINT		#x0085) ;; U+0085
  (define LINE-SEPARATOR-CODE-POINT	#x2028) ;; U+2028

  (define NEWLINE-CHAR			#\x000A) ;; U+000A
  (define LINEFEED-CHAR			#\x000A) ;; U+000A
  (define CARRIAGE-RETURN-CHAR		#\x000D) ;; U+000D
  (define NEXT-LINE-CHAR		#\x0085) ;; U+0085
  (define LINE-SEPARATOR-CHAR		#\x2028) ;; U+2028

;;; --------------------------------------------------------------------

  (let ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end"))
    (check	;no conversion between strings
	(let-values (((port extract) (open-string-output-port)))
	  (put-string port test-string)
	  (extract))
      => test-string))

;;; --------------------------------------------------------------------
;;; output ports, EOL none

  (let* ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1	"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-bv-latin1	(string->latin1 test-string-latin1)))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style none))))
	  (put-string port test-string)
	  (extract))
      => test-string)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style none)))))
	  (put-string port test-string-latin1)
	  (extract))
      => test-bv-latin1)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style none)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-8)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style none)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style none)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL lf

  (let* ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1	"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-bv-latin1	(string->latin1 test-string-latin1)))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style lf))))
	  (put-string port test-string)
	  (extract))
      => test-string)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style lf)))))
	  (put-string port test-string-latin1)
	  (extract))
      => test-bv-latin1)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style lf)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-8)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style lf)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style lf)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL cr

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000D; \x000D; \x0085; \x2028; \x000D;\x000D; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big)))

	 (test-string-latin1		"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-string-latin1-out	"begin \x000D; \x000D; \x000D;\x000D; end")
	 (test-bv-latin1		(string->latin1 test-string-latin1-out)))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style cr))))
	  (put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style cr)))))
	  (put-string port test-string-latin1)
	  (latin1->string (extract)))
      => test-string-latin1-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style cr)))))
	  (put-string port test-string-latin1)
	  (extract))
      => test-bv-latin1)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style cr)))))
	  (put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style cr)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style cr)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL crlf

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000D;\x000A; \x000D; \x0085; \x2028; \x000D;\x000D;\x000A; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big)))

	 (test-string-latin1		"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-string-latin1-out	"begin \x000D;\x000A; \x000D; \x000D;\x000D;\x000A; end")
	 (test-bv-latin1		(string->latin1 test-string-latin1-out)))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style crlf))))
	  (put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style crlf)))))
	  (put-string port test-string-latin1)
	  (latin1->string (extract)))
      => test-string-latin1-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style crlf)))))
	  (put-string port test-string-latin1)
	  (extract))
      => test-bv-latin1)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style crlf)))))
	  (put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style crlf)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style crlf)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL nel

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x0085; \x000D; \x0085; \x2028; \x000D;\x0085; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big))))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style nel))))
	  (put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-output-port (make-transcoder (latin-1-codec) (eol-style nel))))
      => '(nel))

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style nel)))))
	  (put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style nel)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style nel)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL crnel

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000D;\x0085; \x000D; \x0085; \x2028; \x000D;\x000D;\x0085; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big))))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style crnel))))
	  (put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-output-port (make-transcoder (latin-1-codec) (eol-style crnel))))
      => '(crnel))

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style crnel)))))
	  (put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style crnel)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style crnel)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL ls

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x2028; \x000D; \x0085; \x2028; \x000D;\x2028; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big))))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style ls))))
	  (put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-output-port (make-transcoder (latin-1-codec) (eol-style ls))))
      => '(ls))

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style ls)))))
	  (put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style ls)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style ls)))))
	  (put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

  #t)


(parametrise ((check-test-name	'eol-conversion-output-char))

  (define NEWLINE-CODE-POINT		#x000A) ;; U+000A
  (define LINEFEED-CODE-POINT		#x000A) ;; U+000A
  (define CARRIAGE-RETURN-CODE-POINT	#x000D) ;; U+000D
  (define NEXT-LINE-CODE-POINT		#x0085) ;; U+0085
  (define LINE-SEPARATOR-CODE-POINT	#x2028) ;; U+2028

  (define NEWLINE-CHAR			#\x000A) ;; U+000A
  (define LINEFEED-CHAR			#\x000A) ;; U+000A
  (define CARRIAGE-RETURN-CHAR		#\x000D) ;; U+000D
  (define NEXT-LINE-CHAR		#\x0085) ;; U+0085
  (define LINE-SEPARATOR-CHAR		#\x2028) ;; U+2028

  (define (%put-string port str)
    (do ((i 0 (+ 1 i)))
	((= i (string-length str)))
      (put-char port (string-ref str i))))

;;; --------------------------------------------------------------------

  (let ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end"))
    (check	;no conversion between strings
	(let-values (((port extract) (open-string-output-port)))
	  (%put-string port test-string)
	  (extract))
      => test-string))

;;; --------------------------------------------------------------------
;;; output ports, EOL none

  (let* ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1	"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-bv-latin1	(string->latin1 test-string-latin1)))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style none))))
	  (%put-string port test-string)
	  (extract))
      => test-string)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style none)))))
	  (%put-string port test-string-latin1)
	  (extract))
      => test-bv-latin1)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style none)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-8)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style none)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style none)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL lf

  (let* ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1	"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-bv-latin1	(string->latin1 test-string-latin1)))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style lf))))
	  (%put-string port test-string)
	  (extract))
      => test-string)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style lf)))))
	  (%put-string port test-string-latin1)
	  (extract))
      => test-bv-latin1)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style lf)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-8)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style lf)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style lf)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL cr

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000D; \x000D; \x0085; \x2028; \x000D;\x000D; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big)))

	 (test-string-latin1		"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-string-latin1-out	"begin \x000D; \x000D; \x000D;\x000D; end")
	 (test-bv-latin1		(string->latin1 test-string-latin1-out)))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style cr))))
	  (%put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style cr)))))
	  (%put-string port test-string-latin1)
	  (latin1->string (extract)))
      => test-string-latin1-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style cr)))))
	  (%put-string port test-string-latin1)
	  (extract))
      => test-bv-latin1)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style cr)))))
	  (%put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style cr)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style cr)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL crlf

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000D;\x000A; \x000D; \x0085; \x2028; \x000D;\x000D;\x000A; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big)))

	 (test-string-latin1		"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-string-latin1-out	"begin \x000D;\x000A; \x000D; \x000D;\x000D;\x000A; end")
	 (test-bv-latin1		(string->latin1 test-string-latin1-out)))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style crlf))))
	  (%put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style crlf)))))
	  (%put-string port test-string-latin1)
	  (latin1->string (extract)))
      => test-string-latin1-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (latin-1-codec) (eol-style crlf)))))
	  (%put-string port test-string-latin1)
	  (extract))
      => test-bv-latin1)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style crlf)))))
	  (%put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style crlf)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style crlf)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL nel

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x0085; \x000D; \x0085; \x2028; \x000D;\x0085; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big))))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style nel))))
	  (%put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-output-port (make-transcoder (latin-1-codec) (eol-style nel))))
      => '(nel))

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style nel)))))
	  (%put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style nel)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style nel)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL crnel

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000D;\x0085; \x000D; \x0085; \x2028; \x000D;\x000D;\x0085; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big))))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style crnel))))
	  (%put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-output-port (make-transcoder (latin-1-codec) (eol-style crnel))))
      => '(crnel))

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style crnel)))))
	  (%put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style crnel)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style crnel)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

;;; --------------------------------------------------------------------
;;; output ports, EOL ls

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x2028; \x000D; \x0085; \x2028; \x000D;\x2028; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string-out))
	 (test-bv-utf-16le	(string->utf16 test-string-out (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string-out (endianness big))))

    (check
	(let-values (((port extract) (open-string-output-port (eol-style ls))))
	  (%put-string port test-string)
	  (extract))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-output-port (make-transcoder (latin-1-codec) (eol-style ls))))
      => '(ls))

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-8-codec) (eol-style ls)))))
	  (%put-string port test-string)
	  (utf8->string (extract)))
      => test-string-out)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16le-codec) (eol-style ls)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16le)

    (check
	(let-values (((port extract) (open-bytevector-output-port
				      (make-transcoder (utf-16be-codec) (eol-style ls)))))
	  (%put-string port test-string)
	  (extract))
      => test-bv-utf-16be)

    #f)

  #t)


(parametrise ((check-test-name	'eol-conversion-input-string))

  (define NEWLINE-CODE-POINT		#x000A) ;; U+000A
  (define LINEFEED-CODE-POINT		#x000A) ;; U+000A
  (define CARRIAGE-RETURN-CODE-POINT	#x000D) ;; U+000D
  (define NEXT-LINE-CODE-POINT		#x0085) ;; U+0085
  (define LINE-SEPARATOR-CODE-POINT	#x2028) ;; U+2028

  (define NEWLINE-CHAR			#\x000A) ;; U+000A
  (define LINEFEED-CHAR			#\x000A) ;; U+000A
  (define CARRIAGE-RETURN-CHAR		#\x000D) ;; U+000D
  (define NEXT-LINE-CHAR		#\x0085) ;; U+0085
  (define LINE-SEPARATOR-CHAR		#\x2028) ;; U+2028

;;; --------------------------------------------------------------------

  (let ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end"))
    (check	;no conversion between strings
	(let ((port (open-string-input-port test-string)))
	  (get-string-all port))
      => test-string))

;;; --------------------------------------------------------------------
;;; input ports, EOL none

  (let* ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1	"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-bv-latin1	(string->latin1 test-string-latin1)))

    (check
	(let ((port (open-string-input-port test-string (eol-style none))))
	  (get-string-all port))
      => test-string)

    (check
	(let ((port (open-bytevector-input-port test-bv-latin1
						(make-transcoder (latin-1-codec) (eol-style none)))))
	  (get-string-all port))
      => test-string-latin1)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style none)))))
	  (get-string-all port))
      => test-string)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
						(make-transcoder (utf-16le-codec) (eol-style none)))))
	  (get-string-all port))
      => test-string)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
						(make-transcoder (utf-16be-codec) (eol-style none)))))
	  (get-string-all port))
      => test-string)

    #f)

;;; --------------------------------------------------------------------
;;; input ports, EOL lf

  (let* ((test-string     "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out "begin \x000A; \x000A; \x000A; \x000A; \x000A; \x000A; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1		"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-string-latin1-out	"begin \x000A; \x000A; \x000A; end")
	 (test-bv-latin1	(string->latin1 test-string-latin1)))

    (check
	(let ((port (open-string-input-port test-string (eol-style lf))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-latin1
						(make-transcoder (latin-1-codec) (eol-style lf)))))
	  (get-string-all port))
      => test-string-latin1-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style lf)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
						(make-transcoder (utf-16le-codec) (eol-style lf)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
						(make-transcoder (utf-16be-codec) (eol-style lf)))))
	  (get-string-all port))
      => test-string-out)

    #f)

;;; --------------------------------------------------------------------
;;; input ports, EOL cr

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000A; \x000A; \x000A; \x000A; \x000A; \x000A; end")
	 (test-bv-utf-8		(string->utf8  test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1		"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-string-latin1-out	"begin \x000A; \x000A; \x000A; end")
	 (test-bv-latin1		(string->latin1 test-string-latin1)))

    (check
	(let ((port (open-string-input-port test-string (eol-style cr))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-latin1
						(make-transcoder (latin-1-codec) (eol-style cr)))))
	  (get-string-all port))
      => test-string-latin1-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style cr)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
				      (make-transcoder (utf-16le-codec) (eol-style cr)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
				      (make-transcoder (utf-16be-codec) (eol-style cr)))))
	  (get-string-all port))
      => test-string-out)

    #f)

;;; --------------------------------------------------------------------
;;; input ports, EOL crlf

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000A; \x000A; \x000A; \x000A; \x000A; \x000A; end")
	 (test-bv-utf-8		(string->utf8  test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1		"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-string-latin1-out	"begin \x000A; \x000A; \x000A; end")
	 (test-bv-latin1		(string->latin1 test-string-latin1)))

    (check
	(let ((port (open-string-input-port test-string (eol-style crlf))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-latin1
						(make-transcoder (latin-1-codec) (eol-style crlf)))))
	  (get-string-all port))
      => test-string-latin1-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style crlf)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
						(make-transcoder (utf-16le-codec) (eol-style crlf)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
						(make-transcoder (utf-16be-codec) (eol-style crlf)))))
	  (get-string-all port))
      => test-string-out)

    #f)

;;; --------------------------------------------------------------------
;;; input ports, EOL nel

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000A; \x000A; \x000A; \x000A; \x000A; \x000A; end")
	 (test-bv-utf-8		(string->utf8  test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big))))

    (check
	(let ((port (open-string-input-port test-string (eol-style nel))))
	  (get-string-all port))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-input-port '#vu8()
				      (make-transcoder (latin-1-codec) (eol-style nel))))
      => '(nel))

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style nel)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
						(make-transcoder (utf-16le-codec) (eol-style nel)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
						(make-transcoder (utf-16be-codec) (eol-style nel)))))
	  (get-string-all port))
      => test-string-out)

    #f)

;;; --------------------------------------------------------------------
;;; input ports, EOL crnel

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000A; \x000A; \x000A; \x000A; \x000A; \x000A; end")
	 (test-bv-utf-8		(string->utf8  test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big))))

    (check
	(let ((port (open-string-input-port test-string (eol-style crnel))))
	  (get-string-all port))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-input-port '#vu8()
				      (make-transcoder (latin-1-codec) (eol-style crnel))))
      => '(crnel))

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style crnel)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
						(make-transcoder (utf-16le-codec) (eol-style crnel)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
						(make-transcoder (utf-16be-codec) (eol-style crnel)))))
	  (get-string-all port))
      => test-string-out)

    #f)

;;; --------------------------------------------------------------------
;;; input ports, EOL ls

  (let* ((test-string
	  "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out
	  "begin \x000A; \x000A; \x000A; \x000A; \x000A; \x000A; end")
	 (test-bv-utf-8		(string->utf8  test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big))))

    (check
	(let ((port (open-string-input-port test-string (eol-style ls))))
	  (get-string-all port))
      => test-string-out)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (open-bytevector-input-port '#vu8()
				      (make-transcoder (latin-1-codec) (eol-style ls))))
      => '(ls))

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style ls)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
						(make-transcoder (utf-16le-codec) (eol-style ls)))))
	  (get-string-all port))
      => test-string-out)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
						(make-transcoder (utf-16be-codec) (eol-style ls)))))
	  (get-string-all port))
      => test-string-out)

    #f)

  #t)


(parametrise ((check-test-name	'eol-conversion-input-char))

  (define NEWLINE-CODE-POINT		#x000A) ;; U+000A
  (define LINEFEED-CODE-POINT		#x000A) ;; U+000A
  (define CARRIAGE-RETURN-CODE-POINT	#x000D) ;; U+000D
  (define NEXT-LINE-CODE-POINT		#x0085) ;; U+0085
  (define LINE-SEPARATOR-CODE-POINT	#x2028) ;; U+2028

  (define NEWLINE-CHAR			#\x000A) ;; U+000A
  (define LINEFEED-CHAR			#\x000A) ;; U+000A
  (define CARRIAGE-RETURN-CHAR		#\x000D) ;; U+000D
  (define NEXT-LINE-CHAR		#\x0085) ;; U+0085
  (define LINE-SEPARATOR-CHAR		#\x2028) ;; U+2028

  (define (%get-string-all port)
    (let loop ((chars '()))
      (let ((ch (get-char port)))
	(if (eof-object? ch)
	    (apply string (reverse chars))
	  (loop (cons ch chars))))))

;;; --------------------------------------------------------------------

  (let ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end"))
    (check	;no conversion between strings
	(let ((port (open-string-input-port test-string)))
	  (%get-string-all port))
      => test-string))

;;; --------------------------------------------------------------------
;;; input ports, EOL none

  (let* ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1	"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-bv-latin1	(string->latin1 test-string-latin1)))

    (check
	(let ((port (open-string-input-port test-string (eol-style none))))
	  (%get-string-all port))
      => test-string)

    (check
	(let ((port (open-bytevector-input-port test-bv-latin1
						(make-transcoder (latin-1-codec) (eol-style none)))))
	  (%get-string-all port))
      => test-string-latin1)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style none)))))
	  (%get-string-all port))
      => test-string)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
						(make-transcoder (utf-16le-codec) (eol-style none)))))
	  (%get-string-all port))
      => test-string)

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
						(make-transcoder (utf-16be-codec) (eol-style none)))))
	  (%get-string-all port))
      => test-string)

    #f)

;;; --------------------------------------------------------------------
;;; input ports, EOL lf

  (let* ((test-string "begin \x000A; \x000D; \x0085; \x2028; \x000D;\x000A; \x000D;\x0085; end")
	 (test-string-out "begin \x000A; \x000A; \x000A; \x000A; \x000A; \x000A; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1		"begin \x000A; \x000D; \x000D;\x000A; end")
	 (test-string-latin1-out	"begin \x000A; \x000A; \x000A; end")
	 (test-bv-latin1	(string->latin1 test-string-latin1)))

    (for-each
	(lambda (eol-style)
	  (check
	      (let ((port (open-bytevector-input-port test-bv-latin1
						      (make-transcoder (latin-1-codec) eol-style))))
		(%get-string-all port))
	    => test-string-latin1-out))
      '(lf cr crlf))

    (for-each
	(lambda (eol-style)
	  (check
	      (let ((port (open-string-input-port test-string eol-style)))
		(%get-string-all port))
	    => test-string-out)

	  (check
	      (let ((port (open-bytevector-input-port test-bv-utf-8
						      (make-transcoder (utf-8-codec) eol-style))))
		(%get-string-all port))
	    => test-string-out)

	  (check
	      (let ((port (open-bytevector-input-port test-bv-utf-16le
						      (make-transcoder (utf-16le-codec) eol-style))))
		(%get-string-all port))
	    => test-string-out)

	  (check
	      (let ((port (open-bytevector-input-port test-bv-utf-16be
						      (make-transcoder (utf-16be-codec) eol-style))))
		(%get-string-all port))
	    => test-string-out)
	  #f)
      '(lf cr crlf nel crnel ls))

    #f)

  #t)


(parametrise ((check-test-name	'eol-conversion-input-line))

  (define NEWLINE-CODE-POINT		#x000A) ;; U+000A
  (define LINEFEED-CODE-POINT		#x000A) ;; U+000A
  (define CARRIAGE-RETURN-CODE-POINT	#x000D) ;; U+000D
  (define NEXT-LINE-CODE-POINT		#x0085) ;; U+0085
  (define LINE-SEPARATOR-CODE-POINT	#x2028) ;; U+2028

  (define NEWLINE-CHAR			#\x000A) ;; U+000A
  (define LINEFEED-CHAR			#\x000A) ;; U+000A
  (define CARRIAGE-RETURN-CHAR		#\x000D) ;; U+000D
  (define NEXT-LINE-CHAR		#\x0085) ;; U+0085
  (define LINE-SEPARATOR-CHAR		#\x2028) ;; U+2028

;;; --------------------------------------------------------------------

  (let ((test-string      "begin \x000D; \x0085; \x2028; \x000D;\x0085; \x000D;\x000A; \x000A; end")
	(test-string-line "begin \x000D; \x0085; \x2028; \x000D;\x0085; \x000D;")
	(test-string-rest " \x000A; end"))
    (check	;no conversion between strings
	(let ((port (open-string-input-port test-string)))
	  (let ((line (get-line port))
		(rest (get-string-all port)))
	    (list line rest)))
      => (list test-string-line test-string-rest)))

;;; --------------------------------------------------------------------
;;; input ports, EOL none

  (let* ((test-string		"begin \x000D; \x0085; \x2028; \x000D;\x0085; \x000D;\x000A; \x000A; end")
	 (test-string-line	"begin \x000D; \x0085; \x2028; \x000D;\x0085; \x000D;")
	 (test-string-rest	" \x000A; end")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-latin1		"begin \x000D; \x000D;\x000A; \x000A; end")
	 (test-string-latin1-line	"begin \x000D; \x000D;")
	 (test-string-latin1-rest	" \x000A; end")
	 (test-bv-latin1		(string->latin1 test-string-latin1)))

    (check
	(let ((port (open-string-input-port test-string (eol-style none))))
	  (let* ((line (get-line port))
		 (rest (get-string-all port)))
	    (list line rest)))
      => (list test-string-line test-string-rest))

    (check
	(let ((port (open-bytevector-input-port test-bv-latin1
						(make-transcoder (latin-1-codec) (eol-style none)))))
	  (let* ((line (get-line port))
		 (rest (get-string-all port)))
	    (list line rest)))
      => (list test-string-latin1-line test-string-latin1-rest))

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-8
						(make-transcoder (utf-8-codec) (eol-style none)))))
	  (let* ((line (get-line port))
		 (rest (get-string-all port)))
	    (list line rest)))
      => (list test-string-line test-string-rest))

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16le
						(make-transcoder (utf-16le-codec) (eol-style none)))))
	  (let* ((line (get-line port))
		 (rest (get-string-all port)))
	    (list line rest)))
      => (list test-string-line test-string-rest))

    (check
	(let ((port (open-bytevector-input-port test-bv-utf-16be
						(make-transcoder (utf-16be-codec) (eol-style none)))))
	  (let* ((line (get-line port))
		 (rest (get-string-all port)))
	    (list line rest)))
      => (list test-string-line test-string-rest))

    #f)

;;; --------------------------------------------------------------------
;;; input ports, EOL lf

  (let* ((test-string "A_\x000D;_B_\x0085;_C_\x2028;_D_\x000D;\x0085;_E_\x000D;\x000A;_F_\x000A;_G")
	 (test-bv-utf-8		(string->utf8 test-string))
	 (test-bv-utf-16le	(string->utf16 test-string (endianness little)))
	 (test-bv-utf-16be	(string->utf16 test-string (endianness big)))

	 (test-string-result	'("A_" "_B_" "_C_" "_D_" "_E_" "_F_" "_G"))

	 (test-string-latin1		"A_\x000A;_B_\x000D;_C_\x000D;\x000A;_D")
	 (test-string-latin1-result	'("A_" "_B_" "_C_" "_D"))
	 (test-bv-latin1		(string->latin1 test-string-latin1)))

    (define-syntax doit
      (syntax-rules ()
	((_ ?port)
	 (check
	     (let loop ((port ?port) (lines '()))
	       (let ((L (get-line port)))
		 (if (eof-object? L)
		     (reverse lines)
		   (loop port (cons L lines)))))
	   => test-string-result))))

    (define-syntax doit-latin1
      (syntax-rules ()
	((_ ?port)
	 (check
	     (let loop ((port ?port) (lines '()))
	       (let ((L (get-line port)))
		 (if (eof-object? L)
		     (reverse lines)
		   (loop port (cons L lines)))))
	   => test-string-latin1-result))))

    (doit (open-string-input-port test-string (eol-style lf)))
    (doit (open-bytevector-input-port test-bv-utf-8
				      (make-transcoder (utf-8-codec) (eol-style lf))))
    (doit (open-bytevector-input-port test-bv-utf-16le
				      (make-transcoder (utf-16le-codec) (eol-style lf))))
    (doit (open-bytevector-input-port test-bv-utf-16be
				      (make-transcoder (utf-16be-codec) (eol-style lf))))
    (doit-latin1 (open-bytevector-input-port test-bv-latin1
					     (make-transcoder (latin-1-codec) (eol-style lf))))

    #f)


  #t)


(parametrise ((check-test-name	'eol-conversion-input-peek))

  (define LINEFEED-CODE-POINT		#x000A) ;; U+000A
  (define CARRIAGE-RETURN-CODE-POINT	#x000D) ;; U+000D
  (define NEXT-LINE-CODE-POINT		#x0085) ;; U+0085
  (define LINE-SEPARATOR-CODE-POINT	#x2028) ;; U+2028

  (define LINEFEED-CHAR			#\x000A) ;; U+000A
  (define CARRIAGE-RETURN-CHAR		#\x000D) ;; U+000D
  (define NEXT-LINE-CHAR		#\x0085) ;; U+0085
  (define LINE-SEPARATOR-CHAR		#\x2028) ;; U+2028

  (define (%bytevector string->bytevector . chars)
    (string->bytevector (apply string chars)))

  (define (string->utf16le S)
    (string->utf16 S (endianness little)))

  (define (string->utf16be S)
    (string->utf16 S (endianness big)))

;;; --------------------------------------------------------------------
;;; Latin-1

  (check
      (peek-char (open-bytevector-input-port (string->latin1 (string LINEFEED-CHAR))
					     (make-transcoder (latin-1-codec) (eol-style none))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->latin1 (string CARRIAGE-RETURN-CHAR))
					     (make-transcoder (latin-1-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->latin1 (string CARRIAGE-RETURN-CHAR LINEFEED-CHAR))
					     (make-transcoder (latin-1-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)

  (check
      (peek-char (open-bytevector-input-port (string->latin1 (string LINEFEED-CHAR))
					     (make-transcoder (latin-1-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->latin1 (string CARRIAGE-RETURN-CHAR))
					     (make-transcoder (latin-1-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->latin1 (string CARRIAGE-RETURN-CHAR LINEFEED-CHAR))
					     (make-transcoder (latin-1-codec) (eol-style lf))))
    => LINEFEED-CHAR)

;;; --------------------------------------------------------------------
;;; UTF-8

  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string LINEFEED-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style none))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string CARRIAGE-RETURN-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string CARRIAGE-RETURN-CHAR LINEFEED-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string NEXT-LINE-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style none))))
    => NEXT-LINE-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string CARRIAGE-RETURN-CHAR NEXT-LINE-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string LINE-SEPARATOR-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style none))))
    => LINE-SEPARATOR-CHAR)

  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string LINEFEED-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string CARRIAGE-RETURN-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string CARRIAGE-RETURN-CHAR LINEFEED-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string NEXT-LINE-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string CARRIAGE-RETURN-CHAR NEXT-LINE-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf8 (string LINE-SEPARATOR-CHAR))
					     (make-transcoder (utf-8-codec) (eol-style lf))))
    => LINEFEED-CHAR)

;;; --------------------------------------------------------------------
;;; UTF-16-LE

  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string LINEFEED-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style none))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string CARRIAGE-RETURN-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string CARRIAGE-RETURN-CHAR LINEFEED-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string NEXT-LINE-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style none))))
    => NEXT-LINE-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string CARRIAGE-RETURN-CHAR NEXT-LINE-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string LINE-SEPARATOR-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style none))))
    => LINE-SEPARATOR-CHAR)

  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string LINEFEED-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string CARRIAGE-RETURN-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string CARRIAGE-RETURN-CHAR LINEFEED-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string NEXT-LINE-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string CARRIAGE-RETURN-CHAR NEXT-LINE-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16le (string LINE-SEPARATOR-CHAR))
					     (make-transcoder (utf-16le-codec) (eol-style lf))))
    => LINEFEED-CHAR)

;;; --------------------------------------------------------------------
;;; UTF-16-BE

  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string LINEFEED-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style none))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string CARRIAGE-RETURN-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string CARRIAGE-RETURN-CHAR LINEFEED-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string NEXT-LINE-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style none))))
    => NEXT-LINE-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string CARRIAGE-RETURN-CHAR NEXT-LINE-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style none))))
    => CARRIAGE-RETURN-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string LINE-SEPARATOR-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style none))))
    => LINE-SEPARATOR-CHAR)

  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string LINEFEED-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string CARRIAGE-RETURN-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string CARRIAGE-RETURN-CHAR LINEFEED-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string NEXT-LINE-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string CARRIAGE-RETURN-CHAR NEXT-LINE-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style lf))))
    => LINEFEED-CHAR)
  (check
      (peek-char (open-bytevector-input-port (string->utf16be (string LINE-SEPARATOR-CHAR))
					     (make-transcoder (utf-16be-codec) (eol-style lf))))
    => LINEFEED-CHAR)

  #t)


(parametrise ((check-test-name	'utf-bom-codec))

  (define TEST-STRING
    (let* ((str.len	1024)
	   (str		(make-string 1024)))
      (do ((i 0 (+ 1 i)))
	  ((= i 1024)
	   str)
	(string-set! str i (integer->char i)))))

  (define TEST-BV-UTF-8
    (string->utf8 TEST-STRING))
  (define TEST-BV-UTF-8/BOM
    (bytevector-append BYTE-ORDER-MARK-UTF-8 TEST-BV-UTF-8))

  (define TEST-BV-UTF-16-LE
    (string->utf16 TEST-STRING (endianness little)))
  (define TEST-BV-UTF-16-LE/BOM
    (bytevector-append BYTE-ORDER-MARK-UTF-16-LE TEST-BV-UTF-16-LE))

  (define TEST-BV-UTF-16-BE
    (string->utf16 TEST-STRING (endianness big)))
  (define TEST-BV-UTF-16-BE/BOM
    (bytevector-append BYTE-ORDER-MARK-UTF-16-BE TEST-BV-UTF-16-BE))

;;; --------------------------------------------------------------------
;;; reading strings

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-8
					      (make-transcoder (utf-bom-codec)))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-string-all port)))
    => #t)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-8/BOM
					      (make-transcoder (utf-bom-codec)))))
	(get-string-all port))
    => TEST-STRING)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-16-LE/BOM
					      (make-transcoder (utf-bom-codec)))))
	(get-string-all port))
    => TEST-STRING)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-16-BE/BOM
					      (make-transcoder (utf-bom-codec)))))
	(get-string-all port))
    => TEST-STRING)

;;; --------------------------------------------------------------------
;;; reading char

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-8
					      (make-transcoder (utf-bom-codec)))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (get-char port)))
    => #t)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-8/BOM
					      (make-transcoder (utf-bom-codec)))))
	(get-char port))
    => #\x0)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-16-LE/BOM
					      (make-transcoder (utf-bom-codec)))))
	(get-char port))
    => #\x0)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-16-BE/BOM
					      (make-transcoder (utf-bom-codec)))))
	(get-char port))
    => #\x0)

;;; --------------------------------------------------------------------
;;; peeking char

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-8
					      (make-transcoder (utf-bom-codec)))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (eq? port (car (condition-irritants E))))
		  (else E))
	  (lookahead-char port)))
    => #t)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-8/BOM
					      (make-transcoder (utf-bom-codec)))))
	(lookahead-char port))
    => #\x0)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-16-LE/BOM
					      (make-transcoder (utf-bom-codec)))))
	(lookahead-char port))
    => #\x0)

  (check
      (let ((port (open-bytevector-input-port TEST-BV-UTF-16-BE/BOM
					      (make-transcoder (utf-bom-codec)))))
	(lookahead-char port))
    => #\x0)

  #t)


(parametrise ((check-test-name	'wrong-chars-do-not-cause-infinite-loop))

  (check
      (let ((port (open-bytevector-input-port TEST-BYTEVECTOR-FOR-UTF-16-LE
					      (make-transcoder (utf-8-codec)
							       (eol-style none)
							       (error-handling-mode replace)))))
	(let ((c (read-char port))
	      (i (read-char port))
	      (a (read-char port))
	      (o (read-char port))
	      (s (read-char port))
	      (x (read-char port)))
	  (for-all char? (list c i a o s x))))
    => #t)

  (check
      (let ((port (open-bytevector-input-port TEST-BYTEVECTOR-FOR-UTF-16-LE
					      (make-transcoder (utf-8-codec)
							       (eol-style none)
							       (error-handling-mode replace)))))
	(string? (get-string-n port 20)))
    => #t)

  (check
      (let ((port (open-bytevector-input-port TEST-BYTEVECTOR-FOR-UTF-16-LE
					      (make-transcoder (utf-8-codec)
							       (eol-style none)
							       (error-handling-mode replace)))))
	(string? (get-string-all port)))
    => #t)

  #t)


(parametrise ((check-test-name	'reading-wrong-chars))

;;; UTF-8, read

  (check
      (let ((port (open-bytevector-input-port '#vu8(#xFF #xFE #xFF #xFE #x20)
					      (make-transcoder (utf-8-codec)
							       (eol-style none)
							       (error-handling-mode ignore)))))
	(read-char port))
    => #\x20)

  (check
      (let ((port (open-bytevector-input-port '#vu8(#xFF #xFE #xFF #xFE #x20)
					      (make-transcoder (utf-8-codec)
							       (eol-style none)
							       (error-handling-mode replace)))))
	(read-char port))
    => #\xFFFD)

;;; --------------------------------------------------------------------
;;; UTF-8, peek

  (check
      (let ((port (open-bytevector-input-port '#vu8(#xFF #xFE #xFF #xFE #x20)
					      (make-transcoder (utf-8-codec)
							       (eol-style none)
							       (error-handling-mode ignore)))))
	(peek-char port))
    => #\x20)

  (check
      (let ((port (open-bytevector-input-port '#vu8(#xFF #xFE #xFF #xFE #x20)
					      (make-transcoder (utf-8-codec)
							       (eol-style none)
							       (error-handling-mode replace)))))
	(peek-char port))
    => #\xFFFD)

  #t)


;;;; done

(check-report)

;;; end of file
;;; Local Variables:
;;; eval: (put 'with-input-test-pathname	'scheme-indent-function 1)
;;; eval: (put 'check.with-result		'scheme-indent-function 1)
;;; eval: (put 'position-and-contents		'scheme-indent-function 1)
;;; eval: (put 'with-textual-output-test-pathname	'scheme-indent-function 1)
;;; End:
