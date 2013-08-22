;;;derived from the reference implementation of SRFI 106
;;;
;;;Copyright (C) 2012 Takashi Kato <ktakashi@ymail.com>
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;``Software''), to deal in the Software without restriction, including
;;;without limitation the  rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission notice  shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE  IS PROVIDED  ``AS IS'', WITHOUT  WARRANTY OF  ANY KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT SHALL  THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
;;;ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

(import (vicare)
  (prefix (srfi :106 socket)
	  srfi.))

(define (server-run master-socket)
  ;;Handle  the first  pending connection.   If an  exception is  raised
  ;;ignore it.
  (guard (E (else
	     (debug-print (condition-message E))))
    (srfi.call-with-socket
	(srfi.socket-accept master-socket)
      (lambda (server-socket)
	(with-compensations
	  (define in
	    (compensate
		(transcoded-port (srfi.socket-input-port  server-socket)
				 (native-transcoder))
	      (with
	       (close-port in))))
	  (define ou
	    (compensate
		(transcoded-port (srfi.socket-output-port server-socket)
				 (native-transcoder))
	      (with
	       (close-port ou))))
	  (push-compensation
	   (srfi.socket-shutdown server-socket)
	   (srfi.socket-close    server-socket))
	  (let loop ((line (read-line in)))
	    (unless (eof-object? line)
	      (put-string ou (string-append line "\r\n"))
	      (flush-output-port ou)
	      (loop (read-line in))))))))
  ;;Handle next pending connection.
  (server-run master-socket))

(define echo-master-socket
  (srfi.make-server-socket "8080"))

(display "echo server listening\n" (current-error-port))
(server-run echo-master-socket)

;;; end of file
;; Local Variables:
;; eval: (put 'srfi.call-with-socket 'scheme-indent-function 1)
;; End:
