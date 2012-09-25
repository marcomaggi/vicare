;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point
(library (rnrs-benchmarks sumfp)
  (export main)
  (import (rnrs) (rnrs arithmetic flonums) (rnrs-benchmarks))

  
  (define (run n)
    (let loop ((i n) (sum 0.))
      (if (fl<? i 0.)
          sum
          (loop (fl- i 1.) (fl+ i sum)))))
   
  (define (main . args)
    (run-benchmark
      "sumfp"
      sumfp-iters
      (lambda (result) (equal? result 50005000.))
      (lambda (n) (lambda () (run n)))
      10000.)))
