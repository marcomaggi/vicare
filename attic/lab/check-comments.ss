#!/usr/bin/env ikarus --r6rs-script

(import (ikarus))

(define (suffix? s str)
  (let ([n1 (string-length s)] [n2 (string-length str)])
    (and (>= n2 n1)
         (string=? s (substring str (- n2 n1) n2)))))

(define (prefix? s str)
  (let ([n1 (string-length s)] [n2 (string-length str)])
    (and (>= n2 n1)
         (string=? s (substring str 0 n1)))))





(define (scheme? str)
  (ormap (lambda (s) (suffix? s str)) (library-extensions)))


(define (lsr dir)
  (define (app x) (string-append dir "/" x))
  (let-values ([(dirs others)
                (partition file-directory? 
                  (directory-list dir))])
    (apply append 
      (map app (filter scheme? others))
      (map lsr 
        (map app
          (filter 
            (lambda (x)
              (and (not (string=? x ".")) (not (string=? x ".."))))
            dirs))))))


(define (has-comment? x)
  (with-input-from-file x
    (lambda () 
      (define (S0)
        (let ([x (read-line)])
          (cond
            [(eof-object? x) #f]
            [(prefix? "#!" x) (S1)]
            [(prefix? ";;; Copyright" x) #t]
            [(prefix? ";;;" x) (S1)]
            [else #f])))
      (define (S1)
        (let ([x (read-line)])
          (cond
            [(eof-object? x) #f]
            [(prefix? ";;; Copyright" x) #t]
            [(prefix? ";;;" x) (S1)]
            [else #f])))
      (S0))))

(define (check-comment x)
  (unless (has-comment? x)
    (printf "no comment in ~s\n" x)))

(for-each check-comment (lsr "."))

