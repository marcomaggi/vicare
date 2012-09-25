#!/usr/bin/env scheme-script

(import (ikarus))

;;; very simple demo for how to connect to a chargen server, 
;;; and print everything that it returns.

(define (chargen host)
  (let-values ([(ip op) (tcp-connect host "chargen")])
    (let ([ip (transcoded-port ip (native-transcoder))])
      (close-output-port op)
      (call/cc
        (lambda (k)
          (with-exception-handler
            k
            (lambda () 
              (let f ()
                (display (get-string-n ip 72))
                (f))))))
      (close-input-port ip))))

(chargen "localhost")
(newline)
