
(import (ikarus) (ikarus.debugger))

(define (operator? x)
 (and (pair? x)
      (eq? (car x) 'primitive)
      (guard (con [(assertion-violation? con) #t])
        (system-value (cadr x))
        #f)))

(define (get-src/expr ae)
  (if (annotation? ae)
      (cons (annotation-source ae) (annotation-stripped ae))
      (cons #f (syntax->datum ae))))

(define (add-debug-calls expr)
  (define who 'add-debug-calls)
  (define (direct-call? op rands)
    (define n (length rands))
    (define (test cls*)
      (and (pair? cls*)
           (or
             (let ([fmls (caar cls*)])
               (and (list? fmls) (= (length fmls) n)))
             (test (cdr cls*)))))
    (and (pair? op)
         (case (car op)
           [(lambda) (test (list (cdr op)))]
           [(case-lambda) (test (cdr op))]
           [(annotated-case-lambda) (test (cddr op))]
           [else #f])))
  (define (E-call src/expr op rands)
    (cond
      [(or (operator? op) (direct-call? op rands))
       `(,(E op) ,@(map E rands))]
      [else
        `(',debug-call ',src/expr ,(E op) ,@(map E rands))]))
  (define (E expr)
    (cond
      [(symbol? expr) expr]
      [(and (pair? expr) (list? expr))
       (let ([a (car expr)] [d (cdr expr)])
         (case a
           [(quote) expr]
           [(primitive) expr]
           [(set!) `(set! ,(car d) ,(E (cadr d)))]
           [(if) `(if ,(E (car d)) ,(E (cadr d)) ,(E (caddr d)))]
           [(begin) (cons 'begin (map E d))]
           [(lambda) (list 'lambda (car d) (E (cadr d)))]
           [(case-lambda)
            (cons 'case-lambda
              (map (lambda (x) (list (car x) (E (cadr x)))) d))]
           [(annotated-case-lambda)
            (cons* 'annotated-case-lambda (car d)
              (map (lambda (x) (list (car x) (E (cadr x)))) (cdr d)))]
           [(letrec letrec*)
            (list a 
              (map (lambda (x) (list (car x) (E (cadr x)))) (car d))
              (E (cadr d)))]
           [(foreign-call) 
            (cons* 'foreign-call (car d) (map E (cdr d)))]
           [(library-letrec*)
            (list a 
              (map (lambda (x) (list (car x) (cadr x) (E (caddr x)))) (car d))
              (E (cadr d)))]
           [(annotated-call)
            (E-call (get-src/expr (car d)) (cadr d) (cddr d))]
           [else (E-call #f a d)]))]
      [else
       (die who "invalid expression" expr)]))
  (E expr))

(define (start-repl)
  (display "Ikarus Interpreter\n\n")
  (new-cafe
    (lambda (x)
      (guarded-start
        (lambda ()
          (eval x (interaction-environment)))))))

(define (start-script script-name args)
  (command-line-arguments (cons script-name args))
  (guarded-start
    (lambda ()
      (load-r6rs-script script-name #f #t))))
 
(current-core-eval
  (let ([ev (current-core-eval)])
    (lambda (x)
      (let ([x (add-debug-calls x)])
        (ev x)))))

(apply
  (case-lambda
    [(interpreter flag script-name . rest) 
     (if (string=? flag "--r6rs-script") 
         (start-script script-name rest)
         (error interpreter "invalid args" (cons* flag script-name rest)))]
    [(interpreter) (start-repl)]
    [(interpreter . rest)
     (error interpreter "invalid args" rest)])
  (command-line-arguments))
