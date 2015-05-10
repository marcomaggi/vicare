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
;;;Copyright (C) 2012, 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.readline)
  (export
    readline-enabled?
    readline
    make-readline-input-port)
  (import (except (vicare)
		  readline-enabled?
		  readline
		  make-readline-input-port)
    (vicare system $fx)
    (except (vicare system $strings)
	    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Sun
	    ;;Mar 22, 2015)
	    $string-copy!/count
	    $substring)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Sun Mar 22,
    ;;2015)
    (only (ikarus strings)
	  $string-copy!/count
	  $substring)
    (only (vicare language-extensions syntaxes)
	  with-bytevectors/or-false))


;;;; arguments validation

(define (prompt-object? obj)
  (or (not obj) (bytevector? obj) (string? obj)))

(define (prompt-maker? obj)
  (or (not obj) (procedure? obj)))


;;;; access to C API

(define-inline (capi.readline-enabled?)
  (foreign-call "ik_readline_enabled"))

(define-inline (capi.readline prompt)
  (foreign-call "ik_readline_readline" prompt))


;;;; high-level API

(define (readline-enabled?)
  (capi.readline-enabled?))

(case-define* readline
  (()
   (readline #f))
  (({prompt prompt-object?})
   (assert enabled?)
   (with-bytevectors/or-false ((prompt.bv prompt))
     (let ((rv (capi.readline prompt.bv)))
       (and rv (ascii->string rv))))))

(case-define* make-readline-input-port
  (()
   (make-readline-input-port #f))
  (({make-prompt prompt-maker?})
   (define buffer (string))
   (define device-position 0)
   (define (read! str start count)
     (define who 'make-readline-input-port/read!)
     (let ((buffer.len ($string-length buffer)))
       (if ($fx<= count buffer.len)
	   ;;Enough data in the buffer to satisfy the request.
	   (begin
	     ($string-copy!/count buffer 0 str start count)
	     (set! buffer ($substring buffer count buffer.len))
	     count)
	 ;;Read another line.
	 (let ((rv (readline (and make-prompt (make-prompt)))))
	   (if rv
	       (let ((rv.len ($string-length rv)))
		 (set! device-position (+ rv.len device-position))
		 (if (bignum? (+ 1 buffer.len rv.len))
		     (error who "input line from readline too long")
		   (begin
		     (set! buffer (string-append buffer rv "\n"))
		     (let* ((buffer.len	($string-length buffer))
			    (count		(fxmin count buffer.len)))
		       ($string-copy!/count buffer 0 str start count)
		       (set! buffer ($substring buffer count buffer.len))
		       count))))
	     ;;EOF was found: return the available data.
	     (if ($fxzero? buffer.len)
		 0
	       ;;Flush the buffer.
	       (begin
		 ($string-copy!/count buffer 0 str start buffer.len)
		 (set! buffer (string))
		 buffer.len)))))))
   (define (get-position)
     ;;Dummy function needed when reading Scheme code input with a readline port: the
     ;;READ  function  expects  the  port  position to  be  available  for  debugging
     ;;purposes.
     device-position)
   (assert enabled?)
   (receive-and-return (port)
       (make-custom-textual-input-port "readline input port" read! get-position #f #f)
     (set-port-buffer-mode! port (buffer-mode line)))))


;;;; done

(define enabled?
  (capi.readline-enabled?))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'with-bytevectors/or-false	'scheme-indent-function 1)
;; End:
