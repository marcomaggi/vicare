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
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; arguments validation

(define-argument-validation (prompt who obj)
  (or (not obj) (bytevector? obj) (string? obj))
  (assertion-violation who "expected false, bytevector or string as prompt argument" obj))

(define-argument-validation (prompt-maker who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as prompt maker argument" obj))


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
    (with-arguments-validation (who)
	((prompt	prompt))
      (with-bytevectors ((prompt.bv prompt))
	(let ((rv (capi.readline prompt.bv)))
	  (and rv (ascii->string rv))))))))

(define (make-readline-input-port make-prompt)
  (define who 'make-readline-input-port)
  (define (read! str start count)
    (define who 'make-readline-input-port/read!)
    (let ((rv (readline (make-prompt))))
      (if rv
	  (let ((rv.len (unsafe.string-length rv)))
	    (if (bignum? (+ 1 rv.len))
		(error who "input line from readline too long")
	      (begin
		(unsafe.string-copy!/count rv 0 str start rv.len)
		rv.len)))
	0)))
  (with-arguments-validation (who)
      ((prompt-maker	make-prompt))
    (let ((port (make-custom-textual-input-port "readline input port" read! #f #f #f)))
      (set-port-buffer-mode! port (buffer-mode line))
      port)))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; End:
