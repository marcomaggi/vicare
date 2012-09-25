#!/usr/bin/env scheme-script

(import (ikarus))

;;; very simple demo for how to connect to a server, 
;;; send a request and receive a response.

(define (http-cat host)
  (let-values ([(ip op) (tcp-connect host "http")])
    (let ([op (transcoded-port op (native-transcoder))]
          [ip (transcoded-port ip (native-transcoder))])
      (display "GET /\n" op)
      (display (get-string-all ip))
      (close-input-port ip)
      (close-output-port op))))

(http-cat "www.google.com")
(newline)
;(http-cat "127.0.0.1")

