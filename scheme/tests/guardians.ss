
(library (tests guardians)
  (export run-tests)
  (import (ikarus))

  (define (test1)
    (define (for-each-pair f x)
      (when (pair? x)
        (f x)
        (for-each-pair f (cdr x))))
    
    (define n 100)
    
    (define ls (make-list n))
    
    (define g (make-guardian))
    
    (for-each-pair g ls)
    
    (set! ls (cdr ls))
    
    (let f ([i 1])
      (unless (= i n)
        (collect)
        (cond
          [(g) =>
           (lambda (p)
             (printf " [~s/~s]" i n)
             (assert (eq? (cdr p) ls))
             (set! ls (cdr ls))
             (f (+ i 1)))]
          [else (f i)])))
    (assert (null? ls)))

  (define (run-tests)
    (test1)))

