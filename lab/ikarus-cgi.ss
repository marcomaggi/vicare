#!/usr/bin/env ikarus --r6rs-script 

(import (ikarus))

(define (get-headers ip escape)
  (let f ([ls '()])
    (let ([key (get-line ip)])
      (cond
        [(eof-object? key) (escape)]
        [(string=? key "end") ls]
        [else 
         (let ([val (get-line ip)])
           (when (eof-object? val) (escape))
           (f (cons (cons key val) ls)))]))))

(define (put-headers ls op)
  (for-each
    (lambda (p)
      (display (car p) op) 
      (newline op)
      (display (cdr p) op) 
      (newline op))
    ls)
  (display "end\n" op))

(define (alist->string ls)
  (let-values ([(p e) (open-string-output-port)])
    (for-each
      (lambda (x)
        (fprintf p "~s => ~s\n" (car x) (cdr x)))
      ls)
    (e)))
    
(define (serve-client ip op)
  (call/cc
    (lambda (k)
      (with-exception-handler 
        (lambda (con)
          (cond
            [(interrupted-condition? con)
             (raise-continuable con)]
            [else 
             (print-condition con (current-error-port))
             (k)]))
        (lambda ()
          (let loop ()
            (let* ([headers (get-headers ip k)]
                   [response (alist->string headers)])
              (put-headers
                `(("Content-type" . "text/plain")
                  ("Keep-Socket" . "1")
                  ("Content-length" . ,(string-length response)))
                op)
              (display response op)
              (flush-output-port op))
            (loop)))))))

(define cgi-server
  (case-lambda
    [(who port)
     (let ([s (tcp-server-socket-nonblocking
                (or (string->number port)
                    (error who "invalid port number" port)))])
       (printf "Listening on port ~a\n" port)
       (call/cc
         (lambda (k)
           (with-exception-handler 
             (lambda (con)
               (print-condition con)
               (k))
             (lambda ()
               (let f ()
                 (let-values ([(op ip)
                               (accept-connection-nonblocking s)])
                   (printf "got a connection\n")
                   (let ([op (transcoded-port op (native-transcoder))]
                         [ip (transcoded-port ip (native-transcoder))])
                     (register-callback op
                       (lambda () (serve-client ip op)))))
                 (f))))))
       (printf "\nClosing server ...\n")
       (close-tcp-server-socket s))]
    [(who)
     (error who "missing port number")]
    [(who . args)
     (error who "too many arguments")]))

(apply cgi-server (command-line))
