
(import (ikarus) (ikarus system $foreign))

(define-syntax check
  (syntax-rules ()
    [(_ pred expr expected)
     (begin 
       (line)
       (printf "TESTING ~s\n" 'expr)
       (let ([v0 expr] [v1 expected])
         (unless (pred v0 v1)
           (error 'pred "failed" v0 v1)))
       (printf "OK\n"))]))

(define (line)
  (printf "=========================================================\n"))


(define self (dlopen))
(define hosym (dlsym self "ho"))

(define ho 
  ((make-c-callout 'signed-int '(pointer signed-int)) hosym))

(define traced-foradd1 
  ((make-c-callback 'signed-int '(signed-int)) 
     (trace-lambda add1 (n) 
       (collect)
       (add1 n))))

(define foradd1
  ((make-c-callback 'signed-int '(signed-int))
     (lambda (n) 
       (collect)
       (add1 n))))

(define foradd1-by-foreign-call
  ((make-c-callback 'signed-int '(signed-int))
     (trace-lambda foradd1-by-foreign-call (n) 
       (/ (ho traced-foradd1 n) 2))))

(check = (ho (dlsym self "cadd1") 17) (+ 18 18))
(check = (ho foradd1 17) (+ 18 18))
(check = (ho traced-foradd1 17) (+ 18 18))
(check = (ho foradd1-by-foreign-call 17) (+ 18 18))


(define test_I_I 
  ((make-c-callout 'signed-int '(pointer signed-int)) (dlsym self "test_I_I")))
(define test_I_II
  ((make-c-callout 'signed-int '(pointer signed-int signed-int)) (dlsym self "test_I_II")))
(define test_I_III
  ((make-c-callout 'signed-int '(pointer signed-int signed-int signed-int)) (dlsym self "test_I_III")))

(define C_add_I_I (dlsym self "add_I_I"))
(define C_add_I_II (dlsym self "add_I_II"))
(define C_add_I_III (dlsym self "add_I_III"))

(check = (test_I_I C_add_I_I 12) (+ 12))
(check = (test_I_II C_add_I_II 12 13) (+ 12 13))
(check = (test_I_III C_add_I_III 12 13 14) (+ 12 13 14))

(define S_add_I_I ((make-c-callback 'signed-int '(signed-int)) +))
(define S_add_I_II ((make-c-callback 'signed-int '(signed-int
                                                  signed-int)) +))
(define S_add_I_III ((make-c-callback 'signed-int '(signed-int
                                                   signed-int
                                                   signed-int)) +))

(check = (test_I_I S_add_I_I 12) (+ 12))
(check = (test_I_II S_add_I_II 12 13) (+ 12 13))
(check = (test_I_III S_add_I_III 12 13 14) (+ 12 13 14))


(define test_D_D 
  ((make-c-callout 'double '(pointer double)) (dlsym self "test_D_D")))
(define test_D_DD
  ((make-c-callout 'double '(pointer double double)) (dlsym self "test_D_DD")))
(define test_D_DDD
  ((make-c-callout 'double '(pointer double double double)) (dlsym self "test_D_DDD")))

(define C_add_D_D (dlsym self "add_D_D"))
(define C_add_D_DD (dlsym self "add_D_DD"))
(define C_add_D_DDD (dlsym self "add_D_DDD"))

(check = (test_D_D C_add_D_D 12.0) (+ 12.0))
(check = (test_D_DD C_add_D_DD 12.0 13.0) (+ 12.0 13.0))
(check = (test_D_DDD C_add_D_DDD 12.0 13.0 14.0) (+ 12.0 13.0 14.0))

(define S_add_D_D ((make-c-callback 'double '(double)) +))
(define S_add_D_DD ((make-c-callback 'double '(double double)) +))
(define S_add_D_DDD ((make-c-callback 'double '(double double double)) +))

(check = (test_D_D S_add_D_D 12.0) (+ 12.0))
(check = (test_D_DD S_add_D_DD 12.0 13.0) (+ 12.0 13.0))
(check = (test_D_DDD S_add_D_DDD 12.0 13.0 14.0) (+ 12.0 13.0 14.0))


(define RectArea 
  ((make-c-callout 'float '(#(#(float float) #(float float))))
   (dlsym self "test_area_F_R")))

(check = (RectArea '#(#(0.0 0.0) #(10.0 10.0))) 100.0)








(line)
(printf "Happy Happy Joy Joy\n")
