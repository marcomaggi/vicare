;;; FIBFP -- Computes fib(35) using floating point

(library (rnrs-benchmarks fibfp)
  (export main)
  (import (rnrs)
          (rnrs arithmetic flonums)
          (rnrs-benchmarks))
  
  (define (fibfp n)
    (if (fl<? n 2.)
      n
      (fl+ (fibfp (fl- n 1.))
           (fibfp (fl- n 2.)))))
  
  (define (main . args)
    (run-benchmark
      "fibfp"
      fibfp-iters
      (lambda (result) (equal? result 9227465.))
      (lambda (n) (lambda () (fibfp n)))
      35.)))

