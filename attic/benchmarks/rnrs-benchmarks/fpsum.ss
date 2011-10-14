;;; FPSUM - Compute sum of integers from 0 to 1e6 using floating point

(library (rnrs-benchmarks fpsum)
  (export main)
  (import (rnrs) (rnrs arithmetic flonums) (rnrs-benchmarks))

  (define (run)
    (let loop ((i 1e6) (n 0.))
      (if (fl<? i 0.)
        n
        (loop (fl- i 1.) (fl+ i n)))))
   
  (define (main . args)
    (run-benchmark
      "fpsum"
      fpsum-iters
      (lambda (result) (equal? result 500000500000.)) 
      (lambda () run))))
