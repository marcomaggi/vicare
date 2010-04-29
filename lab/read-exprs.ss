#!/usr/bin/env scheme-script
(import (ikarus) (ikarus system interrupts))

(define (prompt-and-read-exprs p)
  (define (read-lines ls)
    (let ([p (open-input-string
               (apply string-append (reverse ls)))])
      (let f ([ls '()])
        (let ([x (read p)])
          (cond
            [(eof-object? x) 
             (if (null? ls) 
                 (eof-object) 
                 (reverse ls))]
            [else (f (cons x ls))])))))
  (define (read-lines-maybe ls)
    (call/cc
      (lambda (k)
        (with-exception-handler 
          (lambda (cn) 
            (cond
              [(interrupted-condition? cn) 
               (raise-continuable cn)]
              [else (k #f)]))
           (lambda ()
             (read-lines ls))))))
  (display "> " (console-output-port))
  (let f ([lns '()])
    (let ([x (get-line p)])
      (cond
        [(eof-object? x) 
         (read-lines lns)]
        [else
         (let ([lns (cons x lns)])
           (or (read-lines-maybe lns)
               (f lns)))]))))

(printf "Experimental prompt\n")
(printf "This just echos the output pretty-printed\n\n")
(let f ()
  (define (try k f)
    (with-exception-handler
      (lambda (cn)
        (flush-output-port (current-error-port))
        (flush-output-port (current-output-port))
        (reset-input-port! (current-input-port))
        (newline (console-output-port))
        (unless (interrupted-condition? cn)
          (fprintf (console-output-port) 
            "Error while reading expression\n")
          (print-condition cn (console-output-port)))
        (k))
      f))
  (call/cc
    (lambda (k) 
      (let ([x
             (try k
               (lambda () 
                 (prompt-and-read-exprs
                   (current-input-port))))])
        (cond
          [(eof-object? x)
           (newline (console-output-port))
           (exit)]
          [else
           (for-each 
             (lambda (x)
               (pretty-print x (console-output-port)))
             x)]))))
  (f))

