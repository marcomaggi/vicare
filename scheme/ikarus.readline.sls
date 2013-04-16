;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: interface to readline
;;;Date: Wed Feb  8, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.readline)
  (export
    readline-enabled?
    readline
    make-readline-input-port)
  (import (except (ikarus)
		  readline-enabled?
		  readline
		  make-readline-input-port)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe operations)
	    unsafe.))


;;;; arguments validation

(define-argument-validation (prompt who obj)
  (or (not obj) (bytevector? obj) (string? obj))
  (assertion-violation who "expected false, bytevector or string as prompt argument" obj))

(define-argument-validation (prompt-maker who obj)
  (or (not obj) (procedure? obj))
  (assertion-violation who "expected false or procedure as prompt maker argument" obj))


;;;; access to C API

(define-inline (capi.readline-enabled?)
  (foreign-call "ik_readline_enabled"))

(define-inline (capi.readline prompt)
  (foreign-call "ik_readline_readline" prompt))


;;;; high-level API

(define (readline-enabled?)
  (capi.readline-enabled?))

(define readline
  (case-lambda
   (()
    (readline #f))
   ((prompt)
    (define who 'readline)
    (assert enabled?)
    (with-arguments-validation (who)
	((prompt	prompt))
      (with-bytevectors/or-false ((prompt.bv prompt))
	(let ((rv (capi.readline prompt.bv)))
	  (and rv (ascii->string rv))))))))

(define make-readline-input-port
  (case-lambda
   (()
    (make-readline-input-port #f))
   ((make-prompt)
    (define who 'make-readline-input-port)
    (define buffer (string))
    (define device-position 0)
    (define (read! str start count)
      (define who 'make-readline-input-port/read!)
      (let ((buffer.len (unsafe.string-length buffer)))
	(if (unsafe.fx<= count buffer.len)
	    ;;Enough data in the buffer to satisfy the request.
	    (begin
	      (unsafe.string-copy!/count buffer 0 str start count)
	      (set! buffer (unsafe.substring buffer count buffer.len))
	      count)
	  ;;Read another line.
	  (let ((rv (readline (and make-prompt (make-prompt)))))
	    (if rv
		(let ((rv.len (unsafe.string-length rv)))
		  (set! device-position (+ rv.len device-position))
		  (if (bignum? (+ 1 buffer.len rv.len))
		      (error who "input line from readline too long")
		    (begin
		      (set! buffer (string-append buffer rv "\n"))
		      (let* ((buffer.len	(unsafe.string-length buffer))
			     (count		(fxmin count buffer.len)))
			(unsafe.string-copy!/count buffer 0 str start count)
			(set! buffer (unsafe.substring buffer count buffer.len))
			count))))
	      ;;EOF was found: return the available data.
	      (if (unsafe.fxzero? buffer.len)
		  0
		;;Flush the buffer.
		(begin
		  (unsafe.string-copy!/count buffer 0 str start buffer.len)
		  (set! buffer (string))
		  buffer.len)))))))
    (define (get-position)
      ;;Dummy  function needed  when reading  Scheme code  input  with a
      ;;readline port: the READ function expects the port position to be
      ;;available for debugging purposes.
      device-position)
    (assert enabled?)
    (with-arguments-validation (who)
	((prompt-maker	make-prompt))
      (let ((port (make-custom-textual-input-port "readline input port"
						  read! get-position #f #f)))
	(set-port-buffer-mode! port (buffer-mode line))
	port)))))


;;;; done

(define enabled?
  (capi.readline-enabled?))

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; End:
