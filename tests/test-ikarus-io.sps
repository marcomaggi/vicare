;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for "ikarus.io.ss"
;;;Date: Mon Aug 29, 2011
;;;
;;;Abstract
;;;
;;;	Some tests  are from the  file "scheme/tests/io.ss" file  in the
;;;	original Ikarus distribution.
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006-2010 Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
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
(import (rename (ikarus)
		(parameterize	parametrise))
  (except (checks) with-result add-result)
  (prefix (only (checks)
		with-result add-result)
	  check.))

(print-unicode #f)
(check-set-mode! 'report-failed)
(display "*** testing Ikarus input/output functions\n")

;; (bytevector-port-buffer-size	256)
;; (string-port-buffer-size	256)
;; (input-file-buffer-size		256)
;; (output-file-buffer-size	256)
;; (input-socket-buffer-size	256)
;; (output-socket-buffer-size	256)


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

(define-syntax test
  ;;Derived from "scheme/tests/io.ss"
  (syntax-rules ()
    ((_ ?description ?body)
     (test #f ?description ?body))
    ((_ ?name ?description ?body)
     (check ?name
       (if (guard (E (else
		      (when (who-condition? E)
			(printf "who: ~a\n" (condition-who E)))
		      (pretty-print (condition-message E))
		      (when (irritants-condition? E)
			(pretty-print (condition-irritants E)))
		      #f))
	     (begin
	       (printf "running ~s ... "  '?description)
	       ?body #t))
	   (begin
	     (printf "ok\n")
	     #t)
	 (begin (printf "failure\n") #f))
       => #t))))


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

;; (define (%string->latin-1 str)
;;   (let-values (((port extract) (open-bytevector-output-port (make-transcoder (latin-1-codec)))))
;;     (put-string port str)
;;     (extract)))

;; (define (%latin-1->string bv)
;;   (let ((port (open-bytevector-input-port bv (make-transcoder (latin-1-codec)))))
;;     (get-string-all port)))


;;;; file helpers

(define (src-file x)
  ;;Build   and  return   a   test  file   string   pathname  from   the
  ;;"$(srcdir)/tests" directory of the distribution.
  ;;
  (string-append (or (getenv "VICARE_SRC_DIR") ".") "/" x))

;;; --------------------------------------------------------------------

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

(define (create-test-pathname)
  ;;Create a  new test  file using the  current (TEST-PATHNAME)  and the
  ;;current (TEST-PATHNAME-DATA-FUNC).
  ;;
  (cleanup-test-pathname)
  (let ((port (open-file-output-port (test-pathname) (file-options) (buffer-mode block) #f)))
    (put-bytevector port ((test-pathname-data-func)))
    (close-output-port port)))

(define (open-test-pathname)
  ;;Open the current test  file referenced by (TEST-PATHNAME) and return
  ;;a binary input port for it.
  ;;
  (open-file-input-port (test-pathname) (file-options) (buffer-mode block) #f))

(define-syntax with-input-test-pathname
  (syntax-rules ()
    ((_ (?port) . ?body)
     (begin
       (create-test-pathname)
       (let ((?port (open-test-pathname)))
	 (unwind-protect
	     (begin  . ?body)
	   (close-input-port ?port)
	   (cleanup-test-pathname)))))))

;;; --------------------------------------------------------------------

(define (file-size-char-by-char filename)
  ;;Open textual file FILENAME and  compute its size in character units,
  ;;reading characters one by one.
  ;;
  (with-input-from-file filename
    (lambda ()
      (let loop ((i 0))
	(let ((x (get-char (current-input-port))))
	  (if (eof-object? x)
	      i
	    (loop (+ i 1))))))))


(define (file->bytevector filename)
  ;;Open the  binary file  FILENAME read  all the bytes  one by  one and
  ;;return a bytevector holding them.
  ;;
  (let ((port (open-file-input-port filename (file-options) 'block #f)))
    (u8-list->bytevector
     (let loop ()
       (let ((x (get-u8 port)))
	 (if (eof-object? x)
	     (begin
	       (close-input-port port)
	       '())
	   (cons x (loop))))))))

(define (bytevector->binary-port bv port)
  ;;Write to the binary PORT the bytes from the bytevector BV.
  ;;
  (let loop ((i 0))
    (unless (fx= i (bytevector-length bv))
      (put-u8 port (bytevector-u8-ref bv i))
      (loop (fx+ i 1)))))

(define (bytevector->textual-port bv port)
  ;;Write  to  the  textual  PORT  the  bytes  from  the  bytevector  BV
  ;;converting them to characters.
  ;;
  (let loop ((i 0))
    (unless (fx= i (bytevector-length bv))
      (put-char port (integer->char (bytevector-u8-ref bv i)))
      (loop (fx+ i 1)))))


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


(parametrise ((check-test-name	'transcoders))

;;; native transcoder

  (check
      (transcoder-codec (native-transcoder))
    => (utf-8-codec))

  (check
      (transcoder-eol-style (native-transcoder))
    => (eol-style none))

  (check
      (transcoder-error-handling-mode (native-transcoder))
    => (error-handling-mode replace))

;;; --------------------------------------------------------------------
;;; making transcoders

  (check
      (let ((T (make-transcoder (utf-8-codec)
				(eol-style lf)
				(error-handling-mode raise))))
	(list (transcoder-codec T)
	      (transcoder-eol-style T)
	      (transcoder-error-handling-mode T)))
    => (list (utf-8-codec)
	     (eol-style lf)
	     (error-handling-mode raise)))

  (check
      (let ((T (make-transcoder (utf-8-codec)
				(eol-style lf))))
	(list (transcoder-codec T)
	      (transcoder-eol-style T)
	      (transcoder-error-handling-mode T)))
    => (list (utf-8-codec)
	     (eol-style lf)
	     (error-handling-mode replace)))

  (check
      (let ((T (make-transcoder (utf-8-codec))))
	(list (transcoder-codec T)
	      (transcoder-eol-style T)
	      (transcoder-error-handling-mode T)))
    => (list (utf-8-codec)
	     (eol-style none)
	     (error-handling-mode replace)))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-transcoder 123))
    => '(123))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-transcoder (utf-8-codec) 'ciao))
    => '(ciao))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(make-transcoder (utf-8-codec) (eol-style lf) 'ciao))
    => '(ciao))

;;; --------------------------------------------------------------------
;;; buffer mode

  (check
      (buffer-mode? (buffer-mode none))
    => #t)

  (check
      (buffer-mode? (buffer-mode line))
    => #t)

  (check
      (buffer-mode? (buffer-mode block))
    => #t)

  (check
      (buffer-mode? 'ciao)
    => #f)

  #t)


(parametrise ((check-test-name	'output-bytevector))

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
        (getter))
    => '#vu8())

;;; --------------------------------------------------------------------
;;; output bytes

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-u8 port 65)
        (getter))
    => '#vu8(65))

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-u8 port 1)
	(put-u8 port 2)
	(put-u8 port 3)
        (getter))
    => '#vu8(1 2 3))

;;; --------------------------------------------------------------------
;;; output bytevectors

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
        (getter))
    => '#vu8(1 2 3))

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(put-bytevector port '#vu8(4 5 6))
	(put-bytevector port '#vu8(7 8 9))
        (getter))
    => '#vu8(1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------
;;; multiple getter invocation

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
        (let ((result-0 (getter)))
	  (put-bytevector port '#vu8(10 20 30))
	  (list result-0 (getter))))
    => '(#vu8(1 2 3) #vu8(10 20 30)))

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(put-bytevector port '#vu8(4 5 6))
	(put-bytevector port '#vu8(7 8 9))
        (let ((result-0 (getter)))
	  (put-bytevector port '#vu8(10 20 30))
	  (put-bytevector port '#vu8(40 50 60))
	  (put-bytevector port '#vu8(70 80 90))
	  (list result-0 (getter))))
    => '(#vu8(1 2 3 4 5 6 7 8 9) #vu8(10 20 30 40 50 60 70 80 90)))

;;; --------------------------------------------------------------------
;;; getting port position

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(port-position port))
    => 0)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-u8 port 1)
	(port-position port))
    => 1)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-u8 port 1)
	(put-u8 port 2)
	(put-u8 port 3)
	(port-position port))
    => 3)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
        (port-position port))
    => 3)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(put-bytevector port '#vu8(4 5 6))
	(put-bytevector port '#vu8(7 8 9))
	(getter)
        (port-position port))
    => 0)

  (check
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(1 2 3))
	(put-bytevector port '#vu8(4 5 6))
	(put-bytevector port '#vu8(7 8 9))
	(getter)
	(put-bytevector port '#vu8(10 20 30))
	(put-bytevector port '#vu8(40 50 60))
        (port-position port))
    => 6)

;;; --------------------------------------------------------------------
;;; setting port position

  (check	;has position?
      (let-values (((port getter) (open-bytevector-output-port)))
	(port-has-set-port-position!? port))
    => #t)

  (check	;invalid position, empty port
      (let-values (((port getter) (open-bytevector-output-port)))
	(guard (E ((i/o-invalid-position-error? E)
		   (list (condition-who E)
			 (i/o-error-position E)))
		  (else E))
	  (set-port-position! port 3)))
    => '(open-bytevector-output-port/set-position! 3))

  (check	;invalid position, beyond limit
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(guard (E ((i/o-invalid-position-error? E)
		   (list (condition-who E)
			 (i/o-error-position E)))
		  (else E))
	  (set-port-position! port 20)))
    => '(open-bytevector-output-port/set-position! 20))

  (check	;set and get position, no write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 3)
	(port-position port))
    => 3)

  (check	;set and get position, with "short" write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 3)
	(put-bytevector port '#vu8(30 40 50))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(6 #vu8(0 1 2 30 40 50 6 7 8 9)))

  (check	;set and get position, with "short" write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 7)
	(put-bytevector port '#vu8(70 80 90))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(10 #vu8(0 1 2 3 4 5 6 70 80 90)))

  (check	;set and get position, with "long" write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 7)
	(put-bytevector port '#vu8(70 80 90 100))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(11 #vu8(0 1 2 3 4 5 6 70 80 90 100)))

  (check	;set and get position, with "long" write
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 7)
	(put-bytevector port '#vu8(70 80 90 100 110 120))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(13 #vu8(0 1 2 3 4 5 6 70 80 90 100 110 120)))

  (check	;set and get position, with "long" write, then write again
      (let-values (((port getter) (open-bytevector-output-port)))
	(put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9))
	(set-port-position! port 7)
	(put-bytevector port '#vu8(70 80 90 100 110 120))
	(put-bytevector port '#vu8(130 140 150))
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(16 #vu8(0 1 2 3 4 5 6 70 80 90 100 110 120 130 140 150)))

  #t)


(parametrise ((check-test-name	'output-string))

;;;These    tests   OPEN-STRING-OUTPUT-PORT,    OPEN-OUTPUT-STRING   and
;;;GET-OUTPUT-STRING.

  (check
      (let-values (((port getter) (open-string-output-port)))
        (getter))
    => "")

;;; --------------------------------------------------------------------
;;; output chars

  (check
      (let-values (((port getter) (open-string-output-port)))
	(put-char port #\a)
        (getter))
    => "a")

  (check
      (let-values (((port getter) (open-string-output-port)))
  	(put-char port #\a)
  	(put-char port #\b)
  	(put-char port #\c)
        (getter))
    => "abc")

;;; --------------------------------------------------------------------
;;; output strings

  (check
      (let-values (((port getter) (open-string-output-port)))
	(display "abc" port)
        (getter))
    => "abc")

  (check
      (let-values (((port getter) (open-string-output-port)))
	(display "abc" port)
	(display "def" port)
	(display "ghi" port)
        (getter))
    => "abcdefghi")

;;; --------------------------------------------------------------------
;;; multiple getter invocation

  (check
      (let-values (((port getter) (open-string-output-port)))
	(display "abc" port)
        (let ((result-0 (getter)))
	  (display "def" port)
	  (list result-0 (getter))))
    => '("abc" "def"))

  (check
      (let-values (((port getter) (open-string-output-port)))
	(display "abc" port)
	(display "def" port)
	(display "ghi" port)
        (let ((result-0 (getter)))
	  (display "ABC" port)
	  (display "DEF" port)
	  (display "GHI" port)
	  (list result-0 (getter))))
    => '("abcdefghi" "ABCDEFGHI"))

;;; --------------------------------------------------------------------
;;; getting port position

  (check
      (let-values (((port getter) (open-string-output-port)))
	(port-position port))
    => 0)

  (check
      (let-values (((port getter) (open-string-output-port)))
	(put-char port #\a)
	(port-position port))
    => 1)

  (check
      (let-values (((port getter) (open-string-output-port)))
	(put-char port #\a)
	(put-char port #\b)
	(put-char port #\c)
	(port-position port))
    => 3)

  (check
      (let-values (((port getter) (open-string-output-port)))
	(display "abc" port)
        (port-position port))
    => 3)

  (check
      (let-values (((port getter) (open-string-output-port)))
	(display "abc" port)
	(display "def" port)
	(display "ghi" port)
	(getter)
        (port-position port))
    => 0)

  (check
      (let-values (((port getter) (open-string-output-port)))
	(display "abc" port)
	(display "def" port)
	(display "ghi" port)
	(getter)
	(display "ABC" port)
	(display "DEF" port)
        (port-position port))
    => 6)

;;; --------------------------------------------------------------------
;;; setting port position

  (check	;has position?
      (let-values (((port getter) (open-string-output-port)))
	(port-has-set-port-position!? port))
    => #t)

  (check	;invalid position, empty port
      (let-values (((port getter) (open-string-output-port)))
	(guard (E ((i/o-invalid-position-error? E)
		   (list (condition-who E)
			 (i/o-error-position E)))
		  (else E))
	  (set-port-position! port 3)))
    => '(open-string-output-port/set-position! 3))

  (check	;invalid position, beyond limit
      (let-values (((port getter) (open-string-output-port)))
	(display "0123456789" port)
	(guard (E ((i/o-invalid-position-error? E)
		   (list (condition-who E)
			 (i/o-error-position E)))
		  (else E))
	  (set-port-position! port 20)))
    => '(open-string-output-port/set-position! 20))

  (check	;set and get position, no write
      (let-values (((port getter) (open-string-output-port)))
	(display "0123456789" port)
	(set-port-position! port 3)
	(port-position port))
    => 3)

  (check	;set and get position, with "short" write
      (let-values (((port getter) (open-string-output-port)))
	(display "0123456789" port)
	(set-port-position! port 3)
	(display "abc" port)
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(6 "012abc6789"))
;;;         0123456789

  (check	;set and get position, with "short" write
      (let-values (((port getter) (open-string-output-port)))
	(display "0123456789" port)
	(set-port-position! port 7)
	(display "abc" port)
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(10 "0123456abc"))
;;;          0123456789

  (check	;set and get position, with "long" write
      (let-values (((port getter) (open-string-output-port)))
	(display "0123456789" port)
	(set-port-position! port 7)
	(display "abcd" port)
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(11 "0123456abcd"))
;;;          01234567890

  (check	;set and get position, with "long" write
      (let-values (((port getter) (open-string-output-port)))
	(display "0123456789" port)
	(set-port-position! port 7)
	(display "abcdefg" port)
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(14 "0123456abcdefg"))
;;;          01234567890123

  (check	;set and get position, with "long" write, then write again
      (let-values (((port getter) (open-string-output-port)))
	(display "0123456789" port)
	(set-port-position! port 7)
	(display "abcdefg" port)
	(display "hilm" port)
	(let ((pos (port-position port)))
	  (list pos (getter))))
    => '(18 "0123456abcdefghilm"))
;;;          012345678901234567

  #t)


(parametrise ((check-test-name	'ikarus/bytevector-input))

  (define (make-n-byte-custom-binary-input-port number-of-bytes)
    ;;Build and return a custom  binary input port whose device contains
    ;;NUMBER-OF-BYTES bytes from 0 included to NUMBER-OF-BYTES excluded.
    ;;The READ! function read bytes one at a time.
    ;;
    (assert (<= 0 number-of-bytes 256))
    (let ((id			"test0")
	  (read!		(let ((byte 0))
				  (lambda (dst.bv dst.start count)
				    (if (< byte number-of-bytes)
					(begin
					  (bytevector-u8-set! dst.bv dst.start byte)
					  (set! byte (+ 1 byte))
					  ;;read one byte
					  1)
				      ;;read no byte, indicates EOF
				      0))))
	  (get-position		#f)
	  (set-position!	#f)
	  (close		#f))
      (make-custom-binary-input-port id read! get-position set-position! close)))

  (define (make-n2-byte-custom-binary-input-port number-of-bytes)
    ;;Build and return a custom  binary input port whose device contains
    ;;NUMBER-OF-BYTES bytes from 0 included to NUMBER-OF-BYTES excluded.
    ;;The READ! function read bytes two at a time.
    ;;
    (let ((id			"test0")
	  (read!		(let ((byte 0))
				  (lambda (dst.bv dst.start count)
				    (if (< byte number-of-bytes)
					(begin
					  (assert (>= count 2))
					  (bytevector-u8-set! dst.bv dst.start byte)
					  (bytevector-u8-set! dst.bv (+ 1 dst.start) (+ 1 byte))
					  (set! byte (+ byte 2))
					  ;;read two bytes
					  2)
				      ;;read no bytes, indicates EOF
				      0))))
	  (get-position		#f)
	  (set-position!	#f)
	  (close		#f))
      (make-custom-binary-input-port id read! get-position set-position! close)))

  (define (make-n-byte-bytevector-binary-input-port number-of-bytes)
    ;;Build and return a binary  input port whose device is a bytevector
    ;;holding NUMBER-OF-BYTES  bytes from 0  included to NUMBER-OF-BYTES
    ;;excluded.
    ;;
    (assert (<= 0 number-of-bytes 256))
    (let ((bv (make-bytevector number-of-bytes)))
      (let loop ((position 0))
	(unless (= position number-of-bytes)
	  (bytevector-u8-set! bv position position)
	  (loop (+ 1 position))))
      (open-bytevector-input-port bv)))

  (define (make-ascii-range-bytevector)
    ;;Build and return a bytevector holding 128 bytes from 0 included to
    ;;127 included.
    ;;
    (let* ((len 128)
	   (bv  (make-bytevector len)))
      (let loop ((position 0))
	(unless (= len position)
	  (bytevector-u8-set! bv position position)
	  (loop (+ 1 position))))
      bv))

  (define (make-ascii-range-bytevector+utf8-bom)
    ;;Build and return a bytevector holding 128+3 bytes, being the 3 BOM
    ;;bytes for UTF-8 encoding followed  by 128 bytes from 0 included to
    ;;127 included.
    ;;
    (let* ((len 128)
	   (bv  (make-bytevector (+ 3 len))))
      (bytevector-u8-set! bv 0 #xEF)
      (bytevector-u8-set! bv 1 #xBB)
      (bytevector-u8-set! bv 2 #xBF)
      (let loop ((position 0))
	(unless (= position len)
	  (bytevector-u8-set! bv (+ position 3) position)
	  (loop (+ 1 position))))
      bv))

;;; --------------------------------------------------------------------

  (define (test-get-u8-1 port number-of-bytes)
    ;;Attempts  to  read   NUMBER-OF-BYTES  unsigned  bytes  from  PORT.
    ;;Success if NUMBER-OF-BYTES bytes are read and their value is equal
    ;;to their offset from the current position in the underlying device
    ;;port.
    ;;
    (let loop ((offset 0))
      (let ((byte (get-u8 port)))
	(cond ((eof-object? byte)
	       (unless (= offset number-of-bytes)
		 (error 'test0 "premature termination" offset)))
	      ((= byte offset)
	       (loop (+ 1 offset)))
	      (else
	       (error 'test0 "incorrect value returned" byte))))))

  (define (test-get-char-1 port number-of-chars)
    ;;Attempts to read NUMBER-OF-CHARS characters from PORT.  Success if
    ;;NUMBER-OF-CHARS  characters are  read and  their integer  value is
    ;;equal to their offset from  the current position in the underlying
    ;;device.
    ;;
    (let loop ((offset 0))
      (let ((ch (get-char port)))
	(cond ((eof-object? ch)
	       (unless (= offset number-of-chars)
		 (error 'test0 "premature termination" offset)))
	      ((= offset (char->integer ch))
	       (loop (+ 1 offset)))
	      (else
	       (error 'test0 "incorrect value returned" ch))))))

  (define (test-peek-u8-1 port number-of-bytes)
    ;;Attempts  to peek  and  read NUMBER-OF-BYTES  unsigned bytes  from
    ;;PORT.  Success  if NUMBER-OF-BYTES bytes  are peeked and  read and
    ;;their value is equal to  their offset from the current position in
    ;;the underlying device port.
    ;;
    (let loop ((offset 0))
      (let* ((pbyte	(lookahead-u8 port))
	     (byte	(get-u8 port)))
	(cond ((not (eqv? pbyte byte))
	       (error #f "peek invalid" pbyte byte))
	      ((eof-object? byte)
	       (unless (= offset number-of-bytes)
		 (error #f "premature termination" offset)))
	      ((= byte offset)
	       (loop (+ 1 offset)))
	      (else
	       (error #f "incorrect value returned" byte offset))))))

  (define (test-peek-char-1 port number-of-chars)
    ;;Attempts to  peek and  read NUMBER-OF-CHARS characters  from PORT.
    ;;Success  if NUMBER-OF-CHARS  characters  are peeked  and read  and
    ;;their  integer value  is equal  to their  offset from  the current
    ;;position in the underlying device port.
    ;;
    (let loop ((offset 0))
      (let* ((pch (lookahead-char port))
	     (ch  (get-char port)))
	(cond ((not (eqv? pch ch))
	       (error #f "peek invalid" pch ch))
	      ((eof-object? ch)
	       (unless (= offset number-of-chars)
		 (error #f "premature termination" offset)))
	      ((= offset (char->integer ch))
	       (loop (+ 1 offset)))
	      (else
	       (error #f "incorrect value returned" ch offset))))))

  (define (test-binary-port-eof?-1 port number-of-bytes)
    ;;Attempts  to read  NUMBER-OF-BYTES  bytes from  PORT.  Success  if
    ;;NUMBER-OF-BYTES bytes  are read,  their value equals  their offset
    ;;from the current  position in the device and  after that PORT-EOF?
    ;;applied to PORT returns true.
    ;;
    (let loop ((offset 0))
      (cond ((port-eof? port)
	     (unless (= offset number-of-bytes)
	       (error #f "premature termination" offset))
	     (assert (eof-object? (lookahead-u8 port)))
	     (assert (eof-object? (get-u8 port))))
	    ((= offset (get-u8 port))
	     (loop (+ 1 offset)))
	    (else
	     (error #f "incorrect value returned" offset)))))

  (define (test-textual-port-eof?-1 port number-of-chars)
    ;;Attempts to read NUMBER-OF-CHARS characters from PORT.  Success if
    ;;NUMBER-OF-CHARS  characters are read,  their integer  value equals
    ;;their offset fro the current position in the device and after that
    ;;PORT-EOF?  applied to PORT returns true.
    ;;
    (let loop ((offset 0))
      (cond ((port-eof? port)
	     (unless (= offset number-of-chars)
	       (error #f "premature termination" offset))
	     (assert (eof-object? (lookahead-char port)))
	     (assert (eof-object? (get-char port))))
	    ((= offset (char->integer (get-char port)))
	     (loop (+ 1 offset)))
	    (else
	     (error #f "incorrect value returned" offset)))))

;;; --------------------------------------------------------------------

  ;;Custom binary input ports.
  (test "reading 256 bytes in ascending order"
	(test-get-u8-1  (make-n-byte-custom-binary-input-port 256) 256))
  (test "reading 256 bytes in ascending order 2 at a time"
	(test-get-u8-1  (make-n2-byte-custom-binary-input-port 256) 256))
  (test "peeking 256 bytes in ascending order"
	(test-peek-u8-1 (make-n-byte-custom-binary-input-port 256) 256))
  (test "custom-binary-port port-eof?"
	(test-binary-port-eof?-1 (make-n-byte-custom-binary-input-port 256) 256))

  ;;Bytevector binary input ports.
  (test "reading 256 bytes from bytevector-input-port"
	(test-get-u8-1  (make-n-byte-bytevector-binary-input-port 256) 256))
  (test "peeking 256 bytes from bytevector-input-port"
	(test-peek-u8-1 (make-n-byte-bytevector-binary-input-port 256) 256))
  (test "bytevector-binary-port port-eof?"
	(test-binary-port-eof?-1 (make-n-byte-bytevector-binary-input-port 256) 256))

  ;;Latin-1 transcoder on top of bytevector binary input port.
  (let ((make-port (lambda ()
		     (transcoded-port (make-n-byte-bytevector-binary-input-port 256)
				      (make-transcoder (latin-1-codec) 'none 'raise)))))
    (test "reading 256 latin1 chars from bytevector-input-port"
	  (test-get-char-1 (make-port) 256))
    (test "peeking 256 bytes from latin1 transcoded port"
	  (test-peek-char-1 (make-port) 256))
    (test "latin1 transcoded port port-eof?"
	  (test-textual-port-eof?-1 (make-port) 256)))

  ;;UTF-8 transcoder on top of bytevector binary input port.
  (let ((make-port (lambda ()
		     (open-bytevector-input-port (make-ascii-range-bytevector)
						 (make-transcoder (utf-8-codec) 'none 'raise)))))
    (test "reading 128 utf8 chars from bytevector-input-port"
	  (test-get-char-1 (make-port) 128))
    (test "peeking 128 chars from utf8 port"
	  (test-peek-char-1 (make-port) 128))
    (test "utf8 transcoded port port-eof?"
	  (test-textual-port-eof?-1 (make-port) 128)))

  #t)


(parametrise ((check-test-name	'ikarus/bytevector-input))

  (define make-utf8-string-range1
    ;;Build and  return a string  holding characters from 0  included to
    ;;#x7F  (127) included.  This  is the  string representation  of the
    ;;bytevector returned by MAKE-UTF8-BYTEVECTOR-RANGE1.
    ;;
    (let ((result #f))
      (lambda ()
	(or result
	    (let ((R (list->string
		      (let loop ((i 0) (limit #x7F))
			(if (> i limit)
			    '()
			  (cons (integer->char i) (loop (+ i 1) limit)))))))
	      (set! result R)
	      R)))))

  (define make-utf8-bytevector-range1
    ;;Build and  return a  bytevector holding bytes  from 0  included to
    ;;#x7F (127) included.  This  is the bytevector UTF-8 representation
    ;;of the string returned by MAKE-UTF8-STRING-RANGE1.
    ;;
    (let ((result #f))
      (lambda ()
	(or result
	    (let ((R (u8-list->bytevector
		      (let loop ((i 0) (limit #x7F))
			(if (> i limit)
			    '()
			  (cons i (loop (+ i 1) limit)))))))
	      (set! result R)
	      R)))))

;;; --------------------------------------------------------------------

  (define make-utf8-string-range2
    ;;Build and return a string holding characters from #x80 included to
    ;;#x7FF  included.   This  is   the  string  representation  of  the
    ;;bytevector returned by MAKE-UTF8-BYTEVECTOR-RANGE2.
    ;;
    (let ((result #f))
      (lambda ()
	(or result
	    (let ((R (list->string
		      (let loop ((i #x80) (limit #x7FF))
			(if (> i limit)
			    '()
			  (cons (integer->char i) (loop (+ i 1) limit)))))))
	      (set! result R)
	      R)))))

  (define make-utf8-bytevector-range2
    ;;Build  and return  a bytevector  holding 16  bits words  from #x80
    ;;(49792)  included   to  #x7FF  (53695)  included.    This  is  the
    ;;bytevector  UTF-8   representation  of  the   string  returned  by
    ;;MAKE-UTF8-STRING-RANGE2.
    ;;
    ;;For a meaningfull log of this bytevector evaluate the following:
    #|
       (let ((bv (make-utf8-bytevector-range2)))
	 (do ((i 0 (+ 2 i)))
	     ((= i 2048))
	   (pretty-print (bytevector-u16-ref bv i (endianness big)))))
    |#
    (let ((result #f))
      (lambda ()
	(or result
	    (let ((R (u8-list->bytevector
		      (let loop ((i #x80) (limit #x7FF))
			(if (> i limit)
			    '()
			  ;;FXSRA = shift right arithmetic
			  (cons* (fxior #b11000000 (fxsra i 6))
				 (fxior #b10000000 (fxand i #b111111))
;;;                                       76543210            543210
				 (loop (+ i 1) limit)))))))
	      (set! result R)
	      R)))))

;;; --------------------------------------------------------------------

  (define make-utf8-string-range3
    ;;Build and  return a string holding characters  from #x800 included
    ;;to  #xFFFF included.   This is  the string  representation  of the
    ;;bytevector returned by MAKE-UTF8-BYTEVECTOR-RANGE3.
    ;;
    (let ((result #f))
      (lambda ()
	(or result
	    (let ((R (list->string
		      (let loop ((i #x800) (limit #xFFFF))
			(cond ((> i limit)
			       '())
			      ((fx= i #xD800)
			       (loop #xE000 limit))
			      (else
			       (cons (integer->char i) (loop (+ i 1) limit))))))))
	      (set! result R)
	      R)))))

  (define make-utf8-bytevector-range3
    ;;Build  and  return  a  bytevector holding  characters  encoded  as
    ;;3-bytes UTF-8  sequences; the characters  have code points  in the
    ;;range  [#x0800,  #xFFFF], skipping  the  forbidden range  [#xD800,
    ;;#xDFFF]  ==  [#xD800,  #xE000).   This  is  the  bytevector  UTF-8
    ;;representation of the string returned by MAKE-UTF8-STRING-RANGE3.
    ;;
    ;;The bytevector has length:
    ;;
    ;;  (- (+ 1 #xFFFF) #x800 (- #xE000 #xD800)) = 184320
    ;;
    ;;and the number of 24 bits words is 61440.
    ;;
    ;;For a meaningfull log of this bytevector evaluate the following:
    #|
       (let* ((bv   (make-utf8-bytevector-range3))
	      (max  (div (bytevector-length bv) 3))
	      (word (make-bytevector 4 0)))
	 (do ((i 0 (+ 3 i)))
	     ((= i max))
	   (bytevector-u8-set! word 0 (bytevector-u8-ref bv i))
	   (bytevector-u8-set! word 1 (bytevector-u8-ref bv (+ 1 i)))
	   (bytevector-u8-set! word 2 (bytevector-u8-ref bv (+ 2 i)))
	   (pretty-print (number->string (bytevector-u32-ref word 0 (endianness big)) 2))))
    |#
    (let ((result #f))
      (lambda ()
	(or result
	    (let ((R (u8-list->bytevector
		      (let loop ((i #x800) (limit #xFFFF))
			(cond ((> i limit)
			       '())
			      ((fx= i #xD800) ;jump the forbidden range
			       (loop #xE000 limit))
			      (else
			       ;;FXSRA = shift right arithmetic
			       (cons* (fxior #b11100000 (fxsra i 12))
				      (fxior #b10000000 (fxand (fxsra i 6) #b111111))
				      (fxior #b10000000 (fxand i           #b111111))
;;;                                            76543210                      543210
				      (loop (+ i 1) limit))))))))
	      (set! result R)
	      R)))))

;;; --------------------------------------------------------------------

  (define make-utf8-string-range4
    ;;Build and return a string holding characters from #x10000 included
    ;;to #x10FFFF  included.  This is  the string representation  of the
    ;;bytevector returned by MAKE-UTF8-BYTEVECTOR-RANGE4.
    ;;
    (let ((result #f))
      (lambda ()
	(or result
	    (let ((R (list->string
		      (let loop ((i #x10000) (limit #x10FFFF))
			(if (> i limit)
			    '()
			  (cons (integer->char i) (loop (+ i 1) limit)))))))
	      (set! result R)
	      R)))))

  (define make-utf8-bytevector-range4
    ;;Build and return  a bytevector holding 32 bits  words from #x10000
    ;;(65536) included  to #x10FFFF (1114111) included;  #x10FFFF is the
    ;;integer  representation  of  the  character with  biggest  integer
    ;;representation.   This is the  bytevector UTF-8  representation of
    ;;the string returned by MAKE-UTF8-STRING-RANGE4.
    ;;
    ;;The bytevector has length:
    ;;
    ;;  (- #x10ffff #x10000) => 1048575
    ;;
    ;;and the number of 32 bits words is 262143.
    ;;
    ;;For a meaningfull log of this bytevector evaluate the following:
    #|
        (let* ((bv   (make-utf8-bytevector-range4))
	       (max  (div (bytevector-length bv) 4)))
	  (do ((i 0 (+ 4 i)))
	      ((= i max))
	    (pretty-print (number->string (bytevector-u32-ref bv i (endianness big)) 2))))
    |#
    (let ((result #f))
      (lambda ()
	(or result
	    (let ((R (u8-list->bytevector
		      (let loop ((i #x10000) (limit #x10FFFF))
			(if (> i limit)
			    '()
			  (cons* (fxior #b11110000 (fxsra i 18))
				 (fxior #b10000000 (fxand (fxsra i 12) #b111111))
				 (fxior #b10000000 (fxand (fxsra i 6)  #b111111))
				 (fxior #b10000000 (fxand i            #b111111))
;;;                                      76543210                       543210
				 (loop (+ i 1) limit)))))))
	      (set! result R)
	      R)))))

;;; --------------------------------------------------------------------

  (define (test-port-string-output port str)
    ;;Read characters from PORT expecting them to be the same characters
    ;;from the string STR.
    ;;
    (let loop ((i 0))
      (let ((x (get-char port)))
  	(cond ((eof-object? x)
	       (unless (= i (string-length str))
		 (error #f "premature EOF")))
	      ((= i (string-length str))
	       (error #f "too many chars"))
	      ((char=? x (string-ref str i))
	       (loop (+ i 1)))
	      (else
	       (error #f
		 (format "mismatch at index ~a, got char ~a (code #x~x), expected char ~a (code #x~x)"
		   i
		   x
		   (char->integer x)
		   (string-ref str i)
		   (char->integer (string-ref str i)))))))))

  (define (test-port-string-peeking-output port str)
    ;;Peek and read  characters from PORT expecting them  to be the same
    ;;characters from the string STR.
    ;;
    (let loop ((i 0))
      (let ((x (lookahead-char port)))
  	(cond ((eof-object? x)
	       (unless (= i (string-length str))
		 (error #f "premature eof")))
	      ((= i (string-length str))
	       (error #f "too many chars"))
	      ((not (char=? x (get-char port)))
	       (error #f "peek not same as get"))
	      ((char=? x (string-ref str i))
	       (loop (+ i 1)))
	      (else
	       (error #f "mismatch" x (string-ref str i) i))))))

  (define (invalid-code? n)
    (not (valid-code? n)))

  (define (valid-code? n)
    ;;Return  true if  N is  an exact  integer being  a  valid character
    ;;representation
    ;;
    (cond ((< n 0)		#f)
	  ((<= n #xD7FF)	#t)
	  ((<  n #xE000)	#f)
	  ((<= n #x10FFFF)	#t)
	  (else
	   (error 'valid-code? "out of range" n))))

  (define (make-u16le-bv min max)
    ;;Build  and return  a bytevector  holding the  UTF-16 little-endian
    ;;representation  of  characters  whose  integer  representation  is
    ;;between MIN included and MAX included.
    ;;
    (u8-list->bytevector
     (let loop ((i min))
       (cond ((> i max)
	      '())
	     ((invalid-code? i)
	      (loop (+ i 1)))
	     ((< i #x10000)
	      (cons* (fxand i #xFF)
		     (fxsra i 8)
		     (loop (+ i 1))))
	     (else
	      (let ((ii (fx- i #x10000)))
		(let ((w1 (fxior #xD800 (fxand #x3FF (fxsra ii 10))))
		      (w2 (fxior #xDC00 (fxand #x3FF ii))))
		  (cons* (fxand w1 #xFF)
			 (fxsra w1 8)
			 (fxand w2 #xFF)
			 (fxsra w2 8)
			 (loop (+ i 1))))))))))

  (define (make-string-slice min max)
    ;;Build  and  return  a  string  holding  characters  whose  integer
    ;;representation is between MIN included and MAX included.
    ;;
    (list->string
     (let loop ((i min))
       (cond ((> i max)
	      '())
	     ((invalid-code? i)
	      (loop (+ i 1)))
	     (else
	      (cons (integer->char i) (loop (+ i 1))))))))

  (define (make-u16le-range1)
    (make-u16le-bv 0 #x7FFF))
  (define (make-utf16-string-range1)
    (make-string-slice 0 #x7FFF))

  (define (make-u16le-range2)
    (make-u16le-bv #x8000 #x10FFFF))
  (define (make-utf16-string-range2)
    (make-string-slice #x8000 #x10FFFF))

  (define (test-partial-reads)
    ;;Tests UTF-8  and UTF-16 codecs  with underlying binary  input port
    ;;returning one byte at a time.
    ;;
    (define (make-test-string)
      ;;Build   and  return   a  string   holding  chars   with  integer
      ;;representation  from 0 included  to #x110000  excluded, skipping
      ;;the forbidden range.
      ;;
      (list->string (let loop ((i 0))
		      (cond ((fx=? i #x110000)
			     '())
			    ((fx=? i #xD800)
			     (loop #xE000))
			    (else
			     (cons (integer->char i) (loop (+ i 1))))))))
    (define (make-slow-input-port bv transcoder)
      ;;Build and  return a transcoded  port using TRANSCODER,  built on
      ;;top  of  a custom  binary  input  port  drawing bytes  from  the
      ;;bytevector BV.  The READ! function draws one byte at a time.
      ;;
      (let ((identifier		"foo")
	    (read!		(let ((n 0))
				  (lambda (dst.bv dst.start count)
				    (if (fx=? n (bytevector-length bv))
					0
				      (begin
					(let ((u8 (bytevector-u8-ref bv n)))
					  ;; (printf "got (~s) #b~b\n" n u8)
					  (bytevector-u8-set! dst.bv dst.start u8))
					(set! n (+ n 1))
					1)))))
	    (get-position	#f)
	    (set-position!	#f)
	    (close		#f))
  	(transcoded-port (make-custom-binary-input-port identifier read!
							get-position set-position! close)
			 transcoder)))
    (define (test name codec s->bv bv->s)
      (printf "testing partial reads for ~s codec ... " name)
      (let ((s (make-test-string)))
  	(assert (string=? s (bv->s (s->bv s))))
  	(let ((r (call-with-port
  		     (make-slow-input-port (s->bv s) (make-transcoder codec
								      (eol-style none)
								      (error-handling-mode raise)))
  		   get-string-all)))
  	  (unless (string=? r s)
  	    (if (= (string-length r) (string-length s))
  		(error #f "test failed")
              (error #f "length mismatch" (string-length s) (string-length r))))))
      (printf "ok\n")
      #t)
    (check
	(test 'utf8 (utf-8-codec)
	      string->utf8
	      utf8->string)
      => #t)
    (check
	(test 'utf16 (utf-16-codec)
	      (lambda (x) (string->utf16 x 'little))
	      (lambda (x) (utf16->string x 'little)))
      => #t))

;;; --------------------------------------------------------------------

  (check
      (bytevector->string (make-utf8-bytevector-range1)
			  (make-transcoder (utf-8-codec)))
    => (make-utf8-string-range1))

  (check
      (bytevector->string (make-utf8-bytevector-range2)
			  (make-transcoder (utf-8-codec)))
    => (make-utf8-string-range2))

  (check
      (bytevector->string (make-utf8-bytevector-range3)
			  (make-transcoder (utf-8-codec)))
    => (make-utf8-string-range3))

  (check
      (bytevector->string (make-utf8-bytevector-range4)
			  (make-transcoder (utf-8-codec)))
    => (make-utf8-string-range4))


  (let ((make-port (lambda (bv)
		     (open-bytevector-input-port bv (make-transcoder (utf-8-codec) 'none 'raise)))))
    (test "utf8 range 1"
	  (test-port-string-output (make-port (make-utf8-bytevector-range1))
				   (make-utf8-string-range1)))

    (test "utf8 range 2"
  	  (test-port-string-output (make-port (make-utf8-bytevector-range2))
				   (make-utf8-string-range2)))

    (test "utf8 range 3"
  	  (test-port-string-output (make-port (make-utf8-bytevector-range3))
				   (make-utf8-string-range3)))

    (test "utf8 range 4"
  	  (test-port-string-output (make-port (make-utf8-bytevector-range4))
				   (make-utf8-string-range4)))
    #f)

  (let ((make-port (lambda (bv)
		     (open-bytevector-input-port bv (make-transcoder (utf-16-codec) 'none 'raise)))))

    (test "utf16 range 1"
  	  (test-port-string-output (make-port (make-u16le-range1))
				   (make-utf16-string-range1)))

    (test "utf16 range 2"
  	  (test-port-string-output (make-port (make-u16le-range2))
				   (make-utf16-string-range2)))
    #f)

  (let ((make-port (lambda (bv)
		     (open-bytevector-input-port bv (make-transcoder (utf-8-codec) 'none 'raise)))))
    (test "utf8 peek range 1"
  	  (test-port-string-peeking-output (make-port (make-utf8-bytevector-range1))
					   (make-utf8-string-range1)))

    (test "utf8 peek range 2"
  	  (test-port-string-peeking-output (make-port (make-utf8-bytevector-range2))
					   (make-utf8-string-range2)))

    (test "utf8 peek range 3"
  	  (test-port-string-peeking-output (make-port (make-utf8-bytevector-range3))
					   (make-utf8-string-range3)))

    (test "utf8 peek range 4"
  	  (test-port-string-peeking-output (make-port (make-utf8-bytevector-range4))
					   (make-utf8-string-range4)))
    #f)

  (let ((make-port (lambda (bv)
		     (open-bytevector-input-port bv (make-transcoder (utf-16-codec) 'none 'raise)))))
    (test "utf16 peek range 1"
  	  (test-port-string-peeking-output (make-port (make-u16le-range1))
					   (make-utf16-string-range1)))

    (test "utf16 peek range 2"
  	  (test-port-string-peeking-output (make-port (make-u16le-range2))
					   (make-utf16-string-range2)))
    #f)

  (begin	;string input port
    (test "utf8 range 1 string"
  	  (test-port-string-output (open-string-input-port (make-utf8-string-range1))
				   (make-utf8-string-range1)))

    (test "utf8 range 2 string"
  	  (test-port-string-output (open-string-input-port (make-utf8-string-range2))
				   (make-utf8-string-range2)))

    (test "utf8 range 3 string"
  	  (test-port-string-output (open-string-input-port (make-utf8-string-range3))
				   (make-utf8-string-range3)))

    (test "utf8 range 4 string"
  	  (test-port-string-output (open-string-input-port (make-utf8-string-range4))
				   (make-utf8-string-range4)))

    (test "utf8 peek range 2 string"
  	  (test-port-string-peeking-output (open-string-input-port (make-utf8-string-range2))
					   (make-utf8-string-range2)))

    (test "utf8 peek range 3 string"
  	  (test-port-string-peeking-output (open-string-input-port (make-utf8-string-range3))
					   (make-utf8-string-range3)))

    (test "utf8 peek range 4 string"
  	  (test-port-string-peeking-output (open-string-input-port (make-utf8-string-range4))
					   (make-utf8-string-range4)))

    #f)

  (test-partial-reads)

  (check
      (eof-object? (get-line (open-string-input-port "")))
    => #t)

  (check
      ((lambda (x)
	 (equal? x "abcd"))
       (get-line (open-string-input-port "abcd")))
    => #t)

  (check
      ((lambda (x)
	 (equal? x ""))
       (get-line (open-string-input-port "\nabcd")))
    => #t)

  (check
      ((lambda (x)
	 (equal? x "abcd"))
       (get-line (open-string-input-port "abcd\nefg")))
    => #t)

  ;;Test for port position predicates.
  (let-syntax ((check (syntax-rules ()
			((_ e)
			 (begin ;;; evaluating e twice
			   (assert (not (port-has-port-position? e)))
			   (assert
			    (guard (con
				    ((assertion-violation? con) #t)
				    (else                       #f))
			      (begin (port-position e) #f))))))))
    (check (make-custom-binary-input-port "foo" (lambda a 0) #f #f #f))
    (check (make-custom-binary-output-port "foo" (lambda a 0) #f #f #f))
    (check (make-custom-textual-input-port "foo" (lambda a 0) #f #f #f))
    (check (make-custom-textual-output-port "foo" (lambda a 0) #f #f #f)))

  ;; test for GET-BYTEVECTOR-N!
  (check
      (let ((port (open-bytevector-input-port '#vu8(1 2 3 4 5 6 7 8 9)))
	    (buf  (make-bytevector 10 #xff)))
	(printf "going to read 5 bytes: ~s\n" (get-bytevector-n! port buf 0 5))
	(printf "result: ~s\n" buf)
	(printf "going to read 1 byte: ~s\n" (get-bytevector-n! port buf 5 1))
	(printf "result: ~s\n" buf)
	buf)
    => '#vu8(1 2 3 4 5 6 #xff #xff #xff #xff))

  #t)


(parametrise ((check-test-name	'ikarus/bytevector-output))

  (define (test-custom-binary-output-ports)
    (check
	(let* ((ls		'())
	       (identifier		"foo")
	       (write!		(lambda (src.bv src.start count)
				  (let loop ((index src.start) (left count))
				    (if (fx= left 0)
					count
				      (begin
					(set! ls (cons (bytevector-u8-ref src.bv index) ls))
					(loop (fx+ index 1) (fx- left 1)))))))
	       (get-position	#f)
	       (set-position!	#f)
	       (close		#f)
	       (port		(make-custom-binary-output-port identifier write!
								get-position set-position! close)))
	  (let loop ((i 0))
	    (unless (fx= i 10000)
	      (put-u8 port (mod i 37))
	      (loop (+ i 1))))
	  (flush-output-port port)

	  (let loop ((i 0) (ls (reverse ls)))
	    (if (null? ls)
		#t
	      (begin
		(assert (fx= (mod i 37) (car ls)))
		(loop (fx+ i 1) (cdr ls))))))
      => #t))

  (define (test-put-bytevector)
    (check
	(let-values (((port getter) (open-bytevector-output-port)))
	  (do ((i 0 (+ i 1)))
	      ((= i 86)
	       (getter))
	    (put-bytevector port '#vu8(0))
	    (put-u8 port 0)))
      => (make-bytevector (* 86 2) 0))

    (check
	(let-values (((port getter) (open-bytevector-output-port)))
	  (do ((i 0 (+ i 1)))
	      ((= i 86)
	       (getter))
	    (put-u8 port 0)
	    (put-u8 port 0)))
      => (make-bytevector (* 86 2) 0))

    (check
	(let-values (((port getter) (open-bytevector-output-port)))
	  (do ((i 0 (+ i 1)))
	      ((= i 86)
	       (getter))
	    (put-bytevector port '#vu8(0))
	    (put-bytevector port '#vu8(0))))
      => (make-bytevector (* 86 2) 0)))

;;; --------------------------------------------------------------------

  (test-custom-binary-output-ports)
  (test-put-bytevector)

  #t)


(parametrise ((check-test-name	'ikarus/file))

  ;;Preliminary assertions on input data.
  (assert (= (file-size-char-by-char (src-file "TEST-SOURCE-FILE.txt")) 56573))
  (assert (= (file-size (src-file "TEST-SOURCE-FILE.txt")) 56573))
  (let ((bv (file->bytevector (src-file "TEST-SOURCE-FILE.txt"))))
    (let-values (((port extract) (open-bytevector-output-port #f)))
      (bytevector->binary-port bv port)
      (let ((bv2 (extract)))
	(assert (bytevector=? bv bv2))
	(assert (bytevector=? #vu8() (extract))))))

  (check	;bytevector output port, native transcoder
      (let ((bv (file->bytevector (src-file "TEST-SOURCE-FILE.txt"))))
	(let-values (((p extract) (open-bytevector-output-port (native-transcoder))))
	  (bytevector->textual-port bv p)
	  (let ((bv2 (extract)))
	    (list (bytevector=? bv bv2)
		  (bytevector=? #vu8() (extract))))))
    => '(#t #t))

  (check	;bytevector output port, latin-1 codec
      (let ((bv (file->bytevector (src-file "TEST-SOURCE-FILE.txt"))))
	(let-values (((p extract) (open-bytevector-output-port
				   (make-transcoder (latin-1-codec)))))
	  (bytevector->textual-port bv p)
	  (let ((bv2 (extract)))
	    (list (bytevector=? bv bv2)
		  (bytevector=? #vu8() (extract))))))
    => '(#t #t))

  (check	;string output port
      (let ((bv (file->bytevector (src-file "TEST-SOURCE-FILE.txt"))))
	(let-values (((p extract) (open-string-output-port)))
	  (bytevector->textual-port bv p)
	  (let ((str (extract)))
	    (list (bytevector=? bv (string->utf8 str))
		  (string=? "" (extract))))))
    => '(#t #t))

  (check	;standard output port as binary port
      (let ((p (standard-output-port)))
	(bytevector->binary-port (string->utf8 "HELLO THERE\n") p)
	(flush-output-port p)
	#t)
    => #t)

  (check	;current output port as textual
      (let ((p (current-output-port)))
	(bytevector->textual-port (string->utf8 "HELLO THERE\n") p)
	(flush-output-port p)
	#t)
    => #t)

  (check	;current output port as textual
      (let ((p (current-output-port)))
	(put-string p "HELLO THERE\n")
	(flush-output-port p)
	#t)
    => #t)

  #f)


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
      (let* ((bin-port	(open-bytevector-input-port (string->utf16 TEST-STRING-FOR-UTF-16-LE
								   (endianness little))))
	     (tran-port	(transcoded-port bin-port (make-transcoder (utf-16-codec)))))
	(get-string-all tran-port))
    => TEST-STRING-FOR-UTF-16-LE)

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

  (check 'this
      (let-values (((bin-port extract) (open-bytevector-output-port)))
	(let ((tran-port (transcoded-port bin-port (make-transcoder (utf-8-codec)
								    (eol-style none)
								    (error-handling-mode raise)))))
	  (put-string tran-port TEST-STRING-FOR-UTF-8)
	  (let ((bv (extract)))
(pretty-print bv)
(pretty-print (string->utf8 TEST-STRING-FOR-UTF-8))
	    (utf8->string bv))))
    => TEST-STRING-FOR-UTF-8)

  (check
      (let-values (((bin-port extract) (open-bytevector-output-port)))
	(let ((tran-port (transcoded-port bin-port (make-transcoder (utf-16-codec)
								    (eol-style none)
								    (error-handling-mode replace)))))
	  (put-string tran-port TEST-STRING-FOR-UTF-16-LE)
	  (utf16->string (extract) (endianness little))))
    => TEST-STRING-FOR-UTF-16-LE)

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


(parametrise ((check-test-name	'misc))

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

;;; --------------------------------------------------------------------

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(port-mode 123))
    => '(123))

  (check
      (port-mode (open-bytevector-input-port '#vu8()))
    => 'vicare-mode)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(set-port-mode! 123 'vicare-mode))
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
	(set-port-mode! port 'r6rs-mode)
	(port-mode port))
    => 'r6rs-mode)

;;; --------------------------------------------------------------------

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
    => '(123))

  (check	;count is too big
      (let ((port (open-bytevector-input-port '#vu8(1 2 3))))
	(guard (E ((assertion-violation? E)
;;;		   (pretty-print (condition-message E))
		   (condition-irritants E))
		  (else E))
	  (get-bytevector-n! port (make-bytevector 1) 0 2)))
    => '(2))

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
	(doit TEST-BYTEVECTOR-FOR-UTF-16-LE)
      => TEST-STRING-FOR-UTF-16-LE)

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

    (check	;little endian char, default
	(doit ONE-WORD-UTF-16-CHAR-UTF-16-LE)
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
	(doit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-LE 0 1) (error-handling-mode ignore))
      => `(,(eof-object) #t))

    (check	;attempt  to read  incomplete single  word  UTF-16 char,
		;unexpected EOF, replace
	(doit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-LE 0 1) (error-handling-mode replace))
      => '(#\xFFFD #t))

    (check	;attempt  to read  incomplete single  word  UTF-16 char,
		;unexpected EOF, raise
	(gdoit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-LE 0 1) (error-handling-mode raise))
      => (bytevector->u8-list (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-LE 0 1)))

    #f)

;;; --------------------------------------------------------------------
;;; reading from bytevector input port, transcoded UTF-16, 2-words chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-16-codec)))))
		  (read-char port)))))

    (check	;little endian char, default
	(doit TWO-WORDS-UTF-16-CHAR-UTF-16-LE)
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
	  (list (doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 1) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 2) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 3) mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  2-word   UTF-16  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 1) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 2) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 3) mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt  to read  incomplete single  word  UTF-16 char,
		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 1) mode)
		(gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 2) mode)
		(gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 3) mode)))
      => (list (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 1))
	       (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 2))
	       (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 3))))

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
	  (list (doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt to read corrupted 1-word UTF-16 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to read corrupted 1-word UTF-16 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE     mode)
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
	  (list (doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)
	   (,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt to read corrupted 2-word UTF-16 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)
	   (#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to read corrupted 2-word UTF-16 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(gdoit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
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
	(doit TEST-BYTEVECTOR-FOR-UTF-16-LE)
      => TEST-STRING-FOR-UTF-16-LE)

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

    (check	;little endian char, default
	(doit ONE-WORD-UTF-16-CHAR-UTF-16-LE)
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
	(doit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-LE 0 1) (error-handling-mode ignore))
      => `(,(eof-object) #t))

    (check	;attempt  to peek  incomplete single  word  UTF-16 char,
		;unexpected EOF, replace
	(doit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-LE 0 1) (error-handling-mode replace))
      => '(#\xFFFD #t))

    (check	;attempt  to peek  incomplete single  word  UTF-16 char,
		;unexpected EOF, raise
	(gdoit (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-LE 0 1) (error-handling-mode raise))
      => (bytevector->u8-list (subbytevector-u8 ONE-WORD-UTF-16-CHAR-UTF-16-LE 0 1)))

    #f)

;;; --------------------------------------------------------------------
;;; peeking from bytevector input port, transcoded UTF-16, 2-words chars

  (let ((doit (lambda (bv)
		(let ((port (open-bytevector-input-port bv (make-transcoder (utf-16-codec)))))
		  (%peek-and-consume-char port)))))

    (check	;little endian char, default
	(doit TWO-WORDS-UTF-16-CHAR-UTF-16-LE)
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
	  (list (doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 1) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 2) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 3) mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt   to  read   incomplete  2-word   UTF-16  char,
		;unexpected EOF, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 1) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 2) mode)
		(doit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 3) mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt  to peek  incomplete single  word  UTF-16 char,
		;unexpected EOF, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 1) mode)
		(gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 2) mode)
		(gdoit (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 3) mode)))
      => (list (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 1))
	       (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 2))
	       (bytevector->u8-list (subbytevector-u8 TWO-WORDS-UTF-16-CHAR-UTF-16-LE 0 3))))

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
	  (list (doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check	;attempt to peek corrupted 1-word UTF-16 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to peek corrupted 1-word UTF-16 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED-ONE-WORD-UTF-16-CHAR-UTF-16-LE     mode)
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
	  (list (doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => `((,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)
	   (,(eof-object) #t) (,(eof-object) #t) (,(eof-object) #t)))

    (check 	;attempt to peek corrupted 2-word UTF-16 char, replace
	(let ((mode (error-handling-mode replace)))
	  (list (doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(doit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)))
      => '((#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)
	   (#\xFFFD #t) (#\xFFFD #t) (#\xFFFD #t)))

    (check	;attempt to peek corrupted 2-word UTF-16 char, raise
	(let ((mode (error-handling-mode raise)))
	  (list (gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
		(gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-LE/BOM mode)
		(gdoit CORRUPTED1-TWO-WORDS-UTF-16-CHAR-UTF-16-BE/BOM mode)
		(gdoit CORRUPTED2-TWO-WORDS-UTF-16-CHAR-UTF-16-LE     mode)
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
      (let ((port (open-bytevector-input-port (string->utf16 "ABCD" (endianness little))
					      (make-transcoder (utf-16-codec)))))
	(get-string-n port 10))
    => "ABCD")

  ;; little endian, default
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf16 src.str (endianness little)))
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

  (check 	;start is not a fixnum
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
    => (list 1 (greatest-fixnum)))

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
    => '(9))

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
      (let ((port (open-bytevector-input-port (string->utf16 "ABCD" (endianness little))
					      (make-transcoder (utf-16-codec)))))
	(let ((dst.str		(make-string 10 #\Z))
	      (dst.start	0)
	      (count		10))
	  (list (get-string-n! port dst.str dst.start count) dst.str)))
    => '(4 "ABCDZZZZZZ"))

  ;; little endian, default
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf16 src.str (endianness little)))
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
      (let ((port (open-bytevector-input-port (string->utf16 "ABCD" (endianness little))
					      (make-transcoder (utf-16-codec)))))
	(get-string-all port))
    => "ABCD")

  ;; little endian, default
  (let* ((src.len 1024)
  	 (src.str (let ((str (make-string src.len)))
  		    (do ((i 0 (+ 1 i)))
  			((= i src.len)
  			 str)
  		      (string-set! str i (integer->char i)))))
	 (src.bv  (string->utf16 src.str (endianness little)))
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
      (let ((port (open-bytevector-input-port (string->utf16 "ABCD" (endianness little))
					      (make-transcoder (utf-16-codec)))))
	(read-line port))
    => "ABCD")

  (check	;some data available, newline
      (let ((port (open-bytevector-input-port (string->utf16 "ABC\nD" (endianness little))
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
    => '(1))

  (check	;argument is out of range for bytevector
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9) 0 11)))
    => '(11))

  (check	;argument is out of range for bytevector
      (let ((port (%open-disposable-binary-output-port)))
  	(guard (E ((assertion-violation? E)
;;;  		   (pretty-print (condition-message E))
  		   (condition-irritants E))
  		  (else E))
  	  (put-bytevector port '#vu8(0 1 2 3 4 5 6 7 8 9) 5 6)))
    => '(6))

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

  (check	;bytevector length greater than buffer size
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


;;;; done

(check-report)

;;; end of file
;;; Local Variables:
;;; eval: (put 'with-input-test-pathname	'scheme-indent-function 1)
;;; eval: (put 'check.with-result		'scheme-indent-function 1)
;;; eval: (put 'position-and-contents		'scheme-indent-function 1)
;;; End:
