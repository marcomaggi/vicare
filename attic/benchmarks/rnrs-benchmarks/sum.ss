;;; SUM -- Compute sum of integers from 0 to 10000

(library (rnrs-benchmarks sum)
  (export main)
  (import (rnrs) (rnrs-benchmarks))

  (define (run n)
    (let loop ((i n) (sum 0))
      (if (< i 0)
        sum
        (loop (- i 1) (+ i sum)))))
   
  (define (main . args)
    (run-benchmark
      "sum"
      sum-iters
      (lambda (result) (equal? result 50005000))
      (lambda (n) (lambda () (run n)))
      10000)))
