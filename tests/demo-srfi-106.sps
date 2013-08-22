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

(with-compensations
  (define socket
    (compensate
	(begin
	  (log "connecting...\n")
	  (srfi.make-client-socket "reddit.com" "http"))
      (with
       (srfi.socket-shutdown socket)
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
  (flush-output-port (current-output-port)))

;;; end of file
