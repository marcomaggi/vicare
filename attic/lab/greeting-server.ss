#!/usr/bin/env ikarus --r6rs-script 

(import (ikarus))

(define (get-name p)
  (list->string 
    (let f ()
      (let ([x (read-char p)])
        (cond
          [(or (eof-object? x) (char-whitespace? x))
           '()]
          [else (cons x (f))])))))

(define serve
  (case-lambda
    [(who port)
     (let ([s (tcp-server-socket 
                (or (string->number port) 
                    (error who "invalid port number" port)))])
       (call/cc
         (lambda (k) 
           (with-exception-handler k
             (lambda () 
               (let f ()
                 (let-values ([(op ip) (accept-connection s)])
                   (printf "Connection from address ~a\n" 
                           op)
                   (let ([op (transcoded-port op (native-transcoder))]
                         [ip (transcoded-port ip (native-transcoder))])
                     (display "What's your name? " op) 
                     (let ([name (get-name ip)])
                       (printf "Connection from ~s\n" name)
                       (fprintf op "Got it, ~a\n" name)
                       (close-input-port ip)
                       (close-output-port op))))
                 (f))))))
       (printf "\nClosing server ...\n")
       (close-tcp-server-socket s))]
    [(who)
     (error who "missing port number")]
    [(who . args)
     (error who "too many arguments")]))

(apply serve (command-line))
