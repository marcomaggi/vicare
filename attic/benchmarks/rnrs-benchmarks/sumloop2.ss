;;; SUMLOOP -- One of the Kernighan and Van Wyk benchmarks.
  
(library (rnrs-benchmarks sumloop2)
  (export main)
  (import (rnrs) (rnrs-benchmarks))
 
  (define (do-loop n)
    (define sum 0)
    (set! sum 0)
    (do ((i 0 (+ i 1)))
        ((>= i n) sum)
      (set! sum (+ sum 1))))
  
  (define (main . args)
    (run-benchmark
     "sumloop"
     sumloop-iters
     (lambda (result) (equal? result 100000000))
     (lambda (n) (lambda () (do-loop n)))
     100000000)))
