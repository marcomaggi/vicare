;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: demo program for SRFI 106
;;;Date: Thu Aug 22, 2013
;;;
;;;Abstract
;;;
;;;	Download the first page from "http://google.com/".
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
(import (vicare)
  (prefix (srfi :106) srfi.))


;;;; with binary ports
;;
;;Download the first page from "http://reddit.com/r/programming/".
;;

(with-compensations
  (define socket
    (compensate
	(begin
	  (log "connecting...\n")
	  (srfi.make-client-socket "reddit.com" "http"))
      (with
       (srfi.socket-shutdown socket (srfi.shutdown-method read write))
       (srfi.socket-close socket))))
  (define in-port
    (compensate
	(srfi.socket-input-port socket)
      (with
       (close-port in-port))))
  (define ou-port
    (compensate
	(srfi.socket-output-port socket)
      (with
       (close-port ou-port))))

  (define (log template . args)
    (apply fprintf (current-error-port)
	   template args))

  (define (send line ou-port)
    (log "sending: ~s\n" line)
    (put-bytevector ou-port (string->ascii line))
    (flush-output-port ou-port))

  (define (recv in-port)
    (receive (str-port getter)
	(open-string-output-port)
      (let next ((chunk (get-bytevector-some in-port)))
	(if (eof-object? chunk)
	    (getter)
	  (let* ((line.str (ascii->string chunk))
		 (line.len (string-length line.str)))
	    (if (and (>= 3 line.len)
		     (string=? (substring line.str (- line.len 3) line.len) ".\r\n"))
		(getter)
	      (begin
		#;(log "received: ~s\n" line.str)
		(display line.str str-port)
		(next (get-bytevector-some in-port)))))))))

  (send "GET /r/programming/ HTTP/1.0\r\n\r\n" ou-port)
  (display (recv in-port))
  (display (recv in-port))
  (newline)
  (newline)
  (flush-output-port (current-output-port)))


;;;; with transcoded textual ports on top of binary ports
;;
;;Download the first page from "http://reddit.com/r/programming/".
;;

(with-compensations
  (define socket
    (compensate
	(begin
	  (log "connecting...\n")
	  (srfi.make-client-socket "reddit.com" "http"))
      (with
       (srfi.socket-shutdown socket (srfi.shutdown-method read write))
       (srfi.socket-close socket))))
  (define in-port
    (compensate
	(transcoded-port (srfi.socket-input-port socket)
			 (native-transcoder))
      (with
       (close-port in-port))))
  (define ou-port
    (compensate
	(transcoded-port (srfi.socket-output-port socket)
			 (native-transcoder))
      (with
       (close-port ou-port))))

  (define (log template . args)
    (apply fprintf (current-error-port)
	   template args))

  (define (send line ou-port)
    (log "sending: ~s\n" line)
    (display line ou-port)
    (flush-output-port ou-port))

  (define (recv in-port)
    (let-values (((str-port getter) (open-string-output-port)))
      (let next ((line (read-line in-port)))
	(if (or (eof-object? line)
		(string=? line "\r")
		(string=? line ".\r"))
	    (getter)
	  (begin
	    (log "received: ~s\n" line)
	    (display line str-port)
	    (next (read-line in-port)))))))

  (send "GET /r/programming/ HTTP/1.0\r\n\r\n" ou-port)
  (display (recv in-port))
  (display (recv in-port))
  (newline)
  (newline)
  (flush-output-port (current-output-port)))


;;;; with transcoded textual ports on top of binary ports, call with socket
;;
;;Download the first page from "http://reddit.com/r/programming/".
;;

(let ()
  (define (log template . args)
    (apply fprintf (current-error-port)
	   template args))

  (define (main socket)
    (with-compensations
      (define in-port
	(compensate
	    (transcoded-port (srfi.socket-input-port socket)
			     (native-transcoder))
	  (with
	   (close-port in-port))))
      (define ou-port
	(compensate
	    (transcoded-port (srfi.socket-output-port socket)
			     (native-transcoder))
	  (with
	   (close-port ou-port))))

      (define (send line ou-port)
	(log "sending: ~s\n" line)
	(display line ou-port)
	(flush-output-port ou-port))

      (define (recv in-port)
	(let-values (((str-port getter) (open-string-output-port)))
	  (let next ((line (read-line in-port)))
	    (if (or (eof-object? line)
		    (string=? line "\r")
		    (string=? line ".\r"))
		(getter)
	      (begin
		(log "received: ~s\n" line)
		(display line str-port)
		(next (read-line in-port)))))))

      (push-compensation
       (srfi.socket-shutdown socket (srfi.shutdown-method read write)))

      (send "GET /r/programming/ HTTP/1.0\r\n\r\n" ou-port)
      (display (recv in-port))
      (display (recv in-port))
      (newline)
      (newline)
      (flush-output-port (current-output-port))))

  (srfi.call-with-socket (begin
			   (log "connecting...\n")
			   (srfi.make-client-socket "reddit.com" "http"))
			 main)
  #f)


;;; end of file
