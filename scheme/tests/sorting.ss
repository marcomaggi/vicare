
(library (tests sorting)
  (export run-tests)
  (import (ikarus))

  (define (permutations ls)
    (define (rem* ls)
      (cond
        [(null? ls) '()]
        [else 
         (cons (cdr ls) 
           (map 
             (lambda (a) (cons (car ls) a))
             (rem* (cdr ls))))]))
    (cond
      [(null? ls) '(())]
      [else
       (apply append
         (map
           (lambda (x a*) 
             (map (lambda (a) (cons x a)) a*))
           ls 
           (map permutations (rem* ls))))]))
  
  
  (define (test-permutations)
    (define (fact n) 
      (if (zero? n)
          1
          (* n (fact (- n 1)))))
    (define (test ls) 
      (let ([p* (permutations ls)])
        (printf "Testing ~s permutations of ~s\n" 
                (length p*) ls)
        (unless (= (length p*) (fact (length ls)))
          (error 'test-permutations "incorrect number of permutations"))
        (let f ([p* p*]) 
          (unless (null? p*) 
            (let ([p (car p*)])
              (when (member p (cdr p*))
                (error 'test-permutations "duplicate" p))
              (f (cdr p*)))))))
    (test '())
    (test '(1))
    (test '(1 2))
    (test '(1 2 3))
    (test '(1 2 3 4))
    (test '(1 2 3 4 5))
    (test '(1 2 3 4 5 6)))
  
  
  
  (define (test-vector-sort)
    (define (test ls) 
      (let ([v1 (list->vector ls)]
            [p* (map list->vector (permutations ls))])
        (printf "Testing vector-sort for all ~s permutations of ~s\n" 
                (length p*) v1)
        (for-each
          (lambda (p) 
            (let* ([copy (list->vector (vector->list p))]
                   [sv (vector-sort < p)])
              (unless (equal? copy p) 
                (error 'test-vector-sort "vector was mutated"))
              (unless (equal? v1 sv)
                (error 'test-vector-sort "failed" p sv))))
          p*)))
    (test '())
    (test '(1))
    (test '(1 2))
    (test '(1 2 3))
    (test '(1 2 3 4))
    (test '(1 2 3 4 5))
    (test '(1 2 3 4 5 6))
    (test '(1 2 3 4 5 6 7))
    (test '(1 2 3 4 5 6 7 8)))
  
  (define (test-list-sort)
    (define (test ls) 
      (let ([p* (permutations ls)])
        (printf "Testing list-sort for all ~s permutations of ~s\n" 
                (length p*) ls)
        (for-each
          (lambda (p) 
            (let* ([copy (map values p)]
                   [sv (list-sort < p)])
              (unless (equal? copy p) 
                (error 'test-list-sort "list was changed"))
              (unless (equal? ls sv)
                (error 'test-list-sort "failed" p sv))))
          p*)))
    (test '())
    (test '(1))
    (test '(1 2))
    (test '(1 2 3))
    (test '(1 2 3 4))
    (test '(1 2 3 4 5))
    (test '(1 2 3 4 5 6))
    (test '(1 2 3 4 5 6 7))
    (test '(1 2 3 4 5 6 7 8)))
  
  (define (run-tests)
    (test-permutations) 
    (test-vector-sort) 
    (test-list-sort)))

