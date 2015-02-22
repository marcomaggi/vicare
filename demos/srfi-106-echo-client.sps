;;;derived from the reference implementation of SRFI 106
;;;
;;;Copyright (C) 2012 Takashi Kato ktakashi@ymail.com
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

(import (rnrs)
  (prefix (srfi :106 socket)
	  srfi.))

(define client-socket
  (srfi.make-client-socket "localhost" "8080"
    (srfi.address-family inet)
    (srfi.socket-domain stream)
    (srfi.address-info v4mapped addrconfig)
    (srfi.ip-protocol ip)))

(srfi.socket-send client-socket (string->utf8 "hello\r\n"))
(display (utf8->string (srfi.socket-recv client-socket (string-length "hello\r\n"))))
(flush-output-port (current-output-port))
(srfi.socket-shutdown client-socket
		      (srfi.shutdown-method read write))
(srfi.socket-close client-socket)

;;; end of file
;; Local Variables:
;; eval: (put 'srfi.make-client-socket 'scheme-indent-function 2)
;; End:
