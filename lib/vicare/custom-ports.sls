;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: custom ports library
;;;Date: Sat Apr  6, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare custom-ports)
  (export
    open-binary-input-port-pair
    open-binary-output-port-pair
    open-binary-input/output-port-pair

    open-textual-input-port-pair
    open-textual-output-port-pair
    open-textual-input/output-port-pair)
  (import (vicare)
    (prefix (vicare unsafe-operations)
	    $))


;;;; input and output binary port pairs

(define (open-binary-output-port-pair)
  (let-values (((in-port ou-port)
		(open-binary-input-port-pair)))
    (values ou-port in-port)))

(define (open-binary-input-port-pair)
  (let-values (((empty? enqueue! dequeue!)
		(make-queue)))

    (define buf.bv '#vu8())
    (define buf.start 0)
    (define buf.count 0)

    (define (in-port.read! dst.bv dst.start dst.count)
      (let loop ((dst.read  0)
		 (dst.count dst.count))
	(if ($fx< dst.count buf.count)
	    (begin
	      ($bytevector-copy!/count buf.bv buf.start dst.bv dst.start dst.count)
	      (set! buf.start ($fx+ buf.start dst.count))
	      (set! buf.count ($fx- buf.count dst.count))
	      ($fx+ dst.read dst.count))
	  (begin
	    ($bytevector-copy!/count buf.bv buf.start dst.bv dst.start buf.count)
	    (if (empty?)
		(begin
		  (set! buf.bv '#vu8())
		  (set! buf.start 0)
		  (set! buf.count 0)
		  ($fx+ dst.read buf.count))
	      (begin
		(set! buf.bv (dequeue!))
		(set! buf.start 0)
		(set! buf.count ($bytevector-length buf.bv))
		(loop ($fx+ dst.read buf.count) ($fx- dst.count buf.count))))))))

    (define (ou-port.write! src.bv src.start src.count)
      (let ((buffer (make-bytevector src.count)))
	($bytevector-copy!/count src.bv src.start buffer 0 src.count)
	(enqueue! buffer)
	src.count))

    (define in-port
      (make-custom-binary-input-port
       "binary-input-port-pair.input-port"
       in-port.read! #f #f #f))

    (define ou-port
      (make-custom-binary-output-port
       "binary-input-port-pair.output-port"
       ou-port.write! #f #f #f))

    (values in-port ou-port)))


;;;; input/output binary port pairs

(define (open-binary-input/output-port-pair)
  (let-values (((first.empty?  first.enqueue!  first.dequeue!)  (make-queue))
	       ((second.empty? second.enqueue! second.dequeue!) (make-queue)))

    (define first.buf.bv '#vu8())
    (define first.buf.start 0)
    (define first.buf.count 0)

    (define second.buf.bv '#vu8())
    (define second.buf.start 0)
    (define second.buf.count 0)

;;; --------------------------------------------------------------------
;;; acting on FIRST.BUF

    (define (first.read! dst.bv dst.start dst.count)
      (let loop ((dst.read  0)
		 (dst.count dst.count))
	(if ($fx< dst.count first.buf.count)
	    (begin
	      ($bytevector-copy!/count first.buf.bv first.buf.start
				       dst.bv dst.start
				       dst.count)
	      (set! first.buf.start ($fx+ first.buf.start dst.count))
	      (set! first.buf.count ($fx- first.buf.count dst.count))
	      ($fx+ dst.read dst.count))
	  (begin
	    ($bytevector-copy!/count first.buf.bv first.buf.start
				     dst.bv dst.start
				     first.buf.count)
	    (if (first.empty?)
		(begin
		  (set! first.buf.bv '#vu8())
		  (set! first.buf.start 0)
		  (set! first.buf.count 0)
		  ($fx+ dst.read first.buf.count))
	      (begin
		(set! first.buf.bv (first.dequeue!))
		(set! first.buf.start 0)
		(set! first.buf.count ($bytevector-length first.buf.bv))
		(loop ($fx+ dst.read  first.buf.count)
		      ($fx- dst.count first.buf.count))))))))

    (define (second.write! src.bv src.start src.count)
      (let ((buffer (make-bytevector src.count)))
	($bytevector-copy!/count src.bv src.start buffer 0 src.count)
	(first.enqueue! buffer)
	src.count))

;;; --------------------------------------------------------------------
;;; acting on SECOND.BUF

    (define (second.read! dst.bv dst.start dst.count)
      (let loop ((dst.read  0)
		 (dst.count dst.count))
	(if ($fx< dst.count second.buf.count)
	    (begin
	      ($bytevector-copy!/count second.buf.bv second.buf.start
				       dst.bv dst.start
				       dst.count)
	      (set! second.buf.start ($fx+ second.buf.start dst.count))
	      (set! second.buf.count ($fx- second.buf.count dst.count))
	      ($fx+ dst.read dst.count))
	  (begin
	    ($bytevector-copy!/count second.buf.bv second.buf.start
				     dst.bv dst.start
				     second.buf.count)
	    (if (second.empty?)
		(begin
		  (set! second.buf.bv '#vu8())
		  (set! second.buf.start 0)
		  (set! second.buf.count 0)
		  ($fx+ dst.read second.buf.count))
	      (begin
		(set! second.buf.bv (second.dequeue!))
		(set! second.buf.start 0)
		(set! second.buf.count ($bytevector-length second.buf.bv))
		(loop ($fx+ dst.read  second.buf.count)
		      ($fx- dst.count second.buf.count))))))))

    (define (first.write! src.bv src.start src.count)
      (let ((buffer (make-bytevector src.count)))
	($bytevector-copy!/count src.bv src.start buffer 0 src.count)
	(second.enqueue! buffer)
	src.count))

;;; --------------------------------------------------------------------

    (define first.port
      (make-custom-binary-input/output-port
       "binary-input/output-port-pair.first.port"
       first.read! first.write! #f #f #f))

    (define second.port
      (make-custom-binary-input/output-port
       "binary-input/output-port-pair.second.port"
       second.read! second.write! #f #f #f))

    (values first.port second.port)))


;;;; input and output textual port pairs

(define (open-textual-output-port-pair)
  (let-values (((in-port ou-port)
		(open-textual-input-port-pair)))
    (values ou-port in-port)))

(define (open-textual-input-port-pair)
  (let-values (((empty? enqueue! dequeue!)
		(make-queue)))

    (define buf.str '#vu8())
    (define buf.start 0)
    (define buf.count 0)

    (define (in-port.read! dst.str dst.start dst.count)
      (let loop ((dst.read  0)
		 (dst.count dst.count))
	(if ($fx< dst.count buf.count)
	    (begin
	      ($string-copy!/count buf.str buf.start dst.str dst.start dst.count)
	      (set! buf.start ($fx+ buf.start dst.count))
	      (set! buf.count ($fx- buf.count dst.count))
	      ($fx+ dst.read dst.count))
	  (begin
	    ($string-copy!/count buf.str buf.start dst.str dst.start buf.count)
	    (if (empty?)
		(begin
		  (set! buf.str '#vu8())
		  (set! buf.start 0)
		  (set! buf.count 0)
		  ($fx+ dst.read buf.count))
	      (begin
		(set! buf.str (dequeue!))
		(set! buf.start 0)
		(set! buf.count ($string-length buf.str))
		(loop ($fx+ dst.read buf.count) ($fx- dst.count buf.count))))))))

    (define (ou-port.write! src.str src.start src.count)
      (let ((buffer (make-string src.count)))
	($string-copy!/count src.str src.start buffer 0 src.count)
	(enqueue! buffer)
	src.count))

    (define in-port
      (make-custom-textual-input-port
       "textual-input-port-pair.input-port"
       in-port.read! #f #f #f))

    (define ou-port
      (make-custom-textual-output-port
       "textual-input-port-pair.output-port"
       ou-port.write! #f #f #f))

    (values in-port ou-port)))


;;;; input/output textual port pairs

(define (open-textual-input/output-port-pair)
  (let-values (((first.empty?  first.enqueue!  first.dequeue!)  (make-queue))
	       ((second.empty? second.enqueue! second.dequeue!) (make-queue)))

    (define first.buf.str '#vu8())
    (define first.buf.start 0)
    (define first.buf.count 0)

    (define second.buf.str '#vu8())
    (define second.buf.start 0)
    (define second.buf.count 0)

;;; --------------------------------------------------------------------
;;; acting on FIRST.BUF

    (define (first.read! dst.str dst.start dst.count)
      (let loop ((dst.read  0)
		 (dst.count dst.count))
	(if ($fx< dst.count first.buf.count)
	    (begin
	      ($string-copy!/count first.buf.str first.buf.start
				       dst.str dst.start
				       dst.count)
	      (set! first.buf.start ($fx+ first.buf.start dst.count))
	      (set! first.buf.count ($fx- first.buf.count dst.count))
	      ($fx+ dst.read dst.count))
	  (begin
	    ($string-copy!/count first.buf.str first.buf.start
				     dst.str dst.start
				     first.buf.count)
	    (if (first.empty?)
		(begin
		  (set! first.buf.str '#vu8())
		  (set! first.buf.start 0)
		  (set! first.buf.count 0)
		  ($fx+ dst.read first.buf.count))
	      (begin
		(set! first.buf.str (first.dequeue!))
		(set! first.buf.start 0)
		(set! first.buf.count ($string-length first.buf.str))
		(loop ($fx+ dst.read  first.buf.count)
		      ($fx- dst.count first.buf.count))))))))

    (define (second.write! src.str src.start src.count)
      (let ((buffer (make-string src.count)))
	($string-copy!/count src.str src.start buffer 0 src.count)
	(first.enqueue! buffer)
	src.count))

;;; --------------------------------------------------------------------
;;; acting on SECOND.BUF

    (define (second.read! dst.str dst.start dst.count)
      (let loop ((dst.read  0)
		 (dst.count dst.count))
	(if ($fx< dst.count second.buf.count)
	    (begin
	      ($string-copy!/count second.buf.str second.buf.start
				       dst.str dst.start
				       dst.count)
	      (set! second.buf.start ($fx+ second.buf.start dst.count))
	      (set! second.buf.count ($fx- second.buf.count dst.count))
	      ($fx+ dst.read dst.count))
	  (begin
	    ($string-copy!/count second.buf.str second.buf.start
				     dst.str dst.start
				     second.buf.count)
	    (if (second.empty?)
		(begin
		  (set! second.buf.str '#vu8())
		  (set! second.buf.start 0)
		  (set! second.buf.count 0)
		  ($fx+ dst.read second.buf.count))
	      (begin
		(set! second.buf.str (second.dequeue!))
		(set! second.buf.start 0)
		(set! second.buf.count ($string-length second.buf.str))
		(loop ($fx+ dst.read  second.buf.count)
		      ($fx- dst.count second.buf.count))))))))

    (define (first.write! src.str src.start src.count)
      (let ((buffer (make-string src.count)))
	($string-copy!/count src.str src.start buffer 0 src.count)
	(second.enqueue! buffer)
	src.count))

;;; --------------------------------------------------------------------

    (define first.port
      (make-custom-textual-input/output-port
       "textual-input/output-port-pair.first.port"
       first.read! first.write! #f #f #f))

    (define second.port
      (make-custom-textual-input/output-port
       "textual-input/output-port-pair.second.port"
       second.read! second.write! #f #f #f))

    (values first.port second.port)))


;;;; done

)

;;; end of file
