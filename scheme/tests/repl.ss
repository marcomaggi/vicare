
(library (tests repl)
  (export run-tests)
  (import (ikarus))





  (define (run-tests)
    (define e (new-interaction-environment))
    (define (test-bound-procedure x)
      (assert (procedure? (eval x e))))
    (define (test-invalid-syntax x)
      (assert
        (guard (con
                 [(syntax-violation? con) #t]
                 [else #f])
          (eval x e))))
    (define-syntax assert-undefined
      (syntax-rules ()
        [(_ expr)
         (assert
           (guard (con
                    [(syntax-violation? con) #f]
                    [(undefined-violation? con) #t]
                    [else #f])
             expr #f))]))
    (define-syntax assert-syntax
      (syntax-rules ()
        [(_ expr)
         (assert
           (guard (con
                    [(syntax-violation? con) #t]
                    [else #f])
             expr #f))]))
    (define-syntax assert-assertion
      (syntax-rules ()
        [(_ expr)
         (assert
           (guard (con
                    [(assertion-violation? con) #t]
                    [else #f])
             expr #f))]))
    ;;;
    (for-each test-bound-procedure '(cons car cdr + -))
    (for-each test-invalid-syntax '(lambda let else))
    (eval '(define x '12) e)
    (assert (eqv? 12 (eval 'x e)))
    (eval '(define y (lambda (x) (+ x x))) e)
    (assert (procedure? (eval 'y e)))
    (assert (eqv? 12 (eval 'x e)))
    (assert (eqv? 24 (eval '(y 12) e)))
    (assert (eqv? 24 (eval '(y x) e)))
    (eval '(define-syntax m (lambda (stx) #'x)) e)
    (assert (eqv? 12 (eval '(m) e)))
    (assert (eqv? 12 (eval 'm e)))
    (assert (eqv? 12 (eval '(let ([x 13]) m) e)))
    (assert (eqv? 12 (eval '(let ([x 13]) (m)) e)))
    (eval '(define z (lambda () q)) e)
    (assert (procedure? (eval 'z e)))
    (assert-undefined (eval '(z) e))
    (eval '(define q 113) e)
    (assert (eqv? 113 (eval '(z) e)))
    (eval '(define + '+) e)
    (assert (eqv? '+ (eval '+ e)))
    (assert-assertion (eval '(+ 1 2) e))
    (eval '(import (only (rnrs) +)) e)
    (assert (eqv? 3 (eval '(+ 1 2) e)))

    (assert-syntax 
      (eval 
        '(let ()
           (define x 1)
           (define x 2)
           x)
        e))

    (assert-syntax 
      (eval 
        '(let ()
           (define-syntax x (identifier-syntax 1))
           (define-syntax x (identifier-syntax 2))
           x)
        e))
    
    (assert-syntax 
      (eval 
        '(let ()
           (define x 1)
           (define-syntax x (identifier-syntax 2))
           x)
        e))


    (assert-syntax 
      (eval 
        '(let ()
           (define-syntax x (identifier-syntax 2))
           (define x 1)
           x)
        e))

    ;;; test from Michele Simionato, reported in
    ;;; http://groups.google.com/group/ikarus-users/msg/218f85234ce82341
    (let ([e (new-interaction-environment)])
      (eval
        '(library (test-sweet-x)
           (export syntax-match def-syntax) 
           (import (rnrs)) 
           (define-syntax syntax-match 
             (lambda (y) 
               (syntax-case y (sub) 
                 ((syntax-match x (literal ...) (sub patt skel) ...) 
                  (for-all identifier? #'(literal ...)) 
                  #'(syntax-case x (literal ...) 
                      (patt skel) 
                      ...)) 
                 ))) 
           (define-syntax def-syntax 
             (lambda (y) 
               (syntax-case y () 
                ((def-syntax name transformer) 
                     #'(define-syntax name 
                         (lambda (x) 
                           (syntax-case x (<source>) 
                             ((name <source>) #''(... (... transformer))) 
                             (x (transformer #'x))))))))))
        e)

      (eval '(import (test-sweet-x)) e)

      (eval '(def-syntax macro 
               (lambda (y) (syntax-match y () (sub (ctx x) #'x))))
            e)
      (assert 
        (equal? (eval '(macro <source>) e) 
           '(lambda (y) (syntax-match y () (sub (ctx x) #'x))))))



    ))
