#!/usr/bin/env scheme-script

(import (ikarus))

;;; very simple demo for how to connect to a server, 
;;; send a request and receive a response.

;;; Here, we use an asynchronous IO socket.  We wrap the whole
;;; operation with an exception handler to handle the would-block
;;; conditions.  If we do get a would-block condition, we just
;;; return, causing the read/write operation to be restarted until
;;; it succeeds.  Pretty lame at this point, but it works.

(define (http-cat host)
  (let-values ([(ip op) (tcp-connect-nonblocking host "http")])
    (let ([op (transcoded-port op (native-transcoder))]
          [ip (transcoded-port ip (native-transcoder))])
      (display "GET /\n" op)
      (display (get-string-all ip))
      (newline)
      (close-input-port ip)
      (close-output-port op))))

(http-cat "www.google.com")
;(http-cat "127.0.0.1")

