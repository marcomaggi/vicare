#!/usr/bin/env ikarus --r6rs-script

(import (ikarus) (objc))

(define-framework Cocoa)
;(load-shared-object "IKFoo.dylib")

(define who (car (command-line)))

(define (println x) (printf "~a\n" x))

(define (print-classes)
  (for-each println 
    (list-sort string<? 
      (map class-name (get-class-list)))))

(define (parents x)
  (let ([p (class-parent x)])
    (if p (cons p (parents p)) '())))

(define (ivar-info x)
  `(ivar name: ,(ivar-name x) 
         type: ,(ivar-type x)
         offset: ,(ivar-offset x)))

(define (print-class-methods x)
  (define (public? x)
    (and (> (string-length x) 0)
         (not (char=? (string-ref x 0) #\_))))
  (let ([x (or (get-class x) (error who "cannot find class" x))])
    (printf "instance size = ~s\n" (class-instance-size x))
    (printf "parents = ~s\n"
            (map (lambda (x) 
                   (cons (class-name x) 
                         (class-instance-size x)))
                 (parents x)))
    (printf "ivars=~s\n" (map ivar-info (class-ivars x)))
    #;
    (for-each println 
      (list-sort string<? 
        (filter public? 
          (map method-name (class-methods x)))))))

(apply
  (case-lambda
    [() (print-classes)]
    [(x) (print-class-methods x)]
    [args 
     (error who "supply either 0 or 1 arguments")])
  (cdr (command-line)))

