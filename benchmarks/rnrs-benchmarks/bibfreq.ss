 
;;; find the most frequently referenced word in the bible.
;;; aziz ghuloum (Nov 2007)

(library (rnrs-benchmarks bibfreq)
  (export main)
  (import (rnrs) (rnrs-benchmarks))

  (define (fill h)
    (let ([p (open-input-file "bib")])
      (define (put ls) 
        (hashtable-update! h 
          (string->symbol
            (list->string
              (reverse ls)))
          (lambda (x) (+ x 1))
          0))
      (define (alpha ls) 
        (let ([c (read-char p)])
          (cond
            [(eof-object? c) 
             (put ls)]
            [(char-alphabetic? c) 
             (alpha (cons (char-downcase c) ls))]
            [else (put ls) (non-alpha)])))
      (define (non-alpha) 
        (let ([c (read-char p)])
          (cond
            [(eof-object? c) (values)]
            [(char-alphabetic? c) 
             (alpha (list (char-downcase c)))]
            [else (non-alpha)])))
      (non-alpha)
      (close-input-port p)))

  (define (list-head ls n)
    (cond
      [(or (zero? n) (null? ls)) '()]
      [else (cons (car ls) (list-head (cdr ls) (- n 1)))]))

  (define (go)
    (let ([h (make-eq-hashtable)])
      (fill h)
      (let-values ([(keys vals) (hashtable-entries h)])
         (let ([ls (map cons 
                        (vector->list keys)
                        (vector->list vals))])
           (list-head 
             (list-sort (lambda (a b) (> (cdr a) (cdr b))) ls)
             10)))))


  (define (main . args)
    (run-benchmark
     "bibfreq"
     bibfreq-iters
     (lambda (result)
       (equal? result
         '((the . 63922) (and . 51696) (of . 34615) (to . 13562) (that . 12913) 
           (in . 12666) (he . 10420) (shall . 9838) (unto . 8997) (for . 8971))))
     (lambda () (lambda () (go))))))
