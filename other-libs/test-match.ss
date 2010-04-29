#!/usr/bin/env scheme-script

(import (ikarus) (match))

(define (constant? x)
  (or (number? x) (char? x) (string? x) (boolean? x)))

(define (ee x)
  (define (ee x env)
    (trace-match foo x
      [,c (guard (constant? c)) c]
      [,x (guard (symbol? x))
       (cond
         ((assq x env) => cdr)
         (else (error 'ee "unbound variable" x)))]
      [(let ([,x* ,[v*]] ...) ,b)
       (ee b (append (map cons x* v*) env))]
      [(+ ,[x] ,[y]) (+ x y)]
      [,others (error 'ee "invalid expression" others)]))
  (ee x '()))

(pretty-print 
  (ee '(let ((x 5)) (let ((y (+ x x))) (+ y x)))))

;(new-cafe ee)

