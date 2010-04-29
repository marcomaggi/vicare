#!/usr/bin/env ikarus --r6rs-script
(import (SRFI-2)
        (only (ikarus) error parameterize error-handler eval
              environment)
        (r6rs))

(define-syntax expect
  (syntax-rules ()
    [(_ e0 e1)
     (let ([v0 e0] [v1 e1])
       (if (equal? v0 v1)
           'ok
           (error #f "failed in ~s" '(expect e0 e1))))]))

(define-syntax must-be-a-syntax-error
  (syntax-rules ()
    [(_ form)
     ((call/cc
        (lambda (k)
          (parameterize ([error-handler 
                          (lambda args
                            (k (lambda ()
                                (display "failed as expected\n"))))])
             (eval 'form (environment '(r6rs) '(SRFI-2)))
             (lambda ()
               (error #f "did not fail"))))))]))




(display "loaded!\n")



(expect  (land* () 1) 1)
(expect  (land* () 1 2) 2)
(expect  (land* () ) #t)

(expect (let ((x #f)) (land* (x))) #f)
(expect (let ((x 1)) (land* (x))) 1)
(expect (land* ((x #f)) ) #f)
(expect (land* ((x 1)) ) 1)
(must-be-a-syntax-error (land* ( #f (x 1))) )
(expect (land* ( (#f) (x 1)) ) #f)
(must-be-a-syntax-error (land* (2 (x 1))) )
(expect (land* ( (2) (x 1)) ) 1)
(expect (land* ( (x 1) (2)) ) 2)
(expect (let ((x #f)) (land* (x) x)) #f)
(expect (let ((x "")) (land* (x) x)) "")
(expect (let ((x "")) (land* (x)  )) "")
(expect (let ((x 1)) (land* (x) (+ x 1))) 2)
(expect (let ((x #f)) (land* (x) (+ x 1))) #f)
(expect (let ((x 1)) (land* (((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (land* (((positive? x))) )) #t)
(expect (let ((x 0)) (land* (((positive? x))) (+ x 1))) #f)
(expect (let ((x 1)) (land* (((positive? x)) (x (+ x 1))) (+ x 1)))  3)
; this is wrong
; the srfi says: 
;   ``As usual, all VARIABLEs must be unique (like in let*) ''
; but the variables in let* need not be unique; so, it must be 
; a mistake

;(must-be-a-syntax-error
;  (let ((x 1)) 
;    (land* (((positive? x)) 
;            (x (+ x 1))
;            (x (+ x 1)))
;      (+ x 1))))

(expect (let ((x 1)) (land* (x ((positive? x))) (+ x 1))) 2)
(expect (let ((x 1)) (land* ( ((begin x)) ((positive? x))) (+ x 1))) 2)
(expect (let ((x 0)) (land* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (land* (x ((positive? x))) (+ x 1))) #f)
(expect (let ((x #f)) (land* ( ((begin x)) ((positive? x))) (+ x 1))) #f)

;(expect  (let ((x 1)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
;(expect  (let ((x 0)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
;(expect  (let ((x #f)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) #f)
;(expect  (let ((x 3)) (land* (x (y (- x 1)) ((positive? y))) (/ x y))) 3/2)

(display "All tests passed\n")


