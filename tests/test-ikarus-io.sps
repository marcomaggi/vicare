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
  (checks))

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


;;;; file helpers

(define (src-file x)
  ;;Build   and  return   a   test  file   string   pathname  from   the
  ;;"$(srcdir)/tests" directory of the distribution.
  ;;
  (string-append (or (getenv "VICARE_SRC_DIR") ".") "/" x))

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


;;;; done

(check-report)

;;; end of file
