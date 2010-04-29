;;; MBROT -- Generation of Mandelbrot set fractal.
  
(library (rnrs-benchmarks mbrot)
  (export main)
  (import (rnrs) (rnrs r5rs) (rnrs arithmetic flonums) (rnrs-benchmarks))

  (define (count r i step x y)
  
    (let ((max-count 64)
          (radius^2  16.0))
  
      (let ((cr (fl+ r (fl* (exact->inexact x) step)))
            (ci (fl+ i (fl* (exact->inexact y) step))))
        
        (let loop ((zr cr)
                   (zi ci)
                   (c 0))
          (if (= c max-count)
            c
            (let ((zr^2 (fl* zr zr))
                  (zi^2 (fl* zi zi)))
              (if (fl>? (fl+ zr^2 zi^2) radius^2)
                c
                (let ((new-zr (fl+ (fl- zr^2 zi^2) cr))
                      (new-zi (fl+ (fl* 2.0 (fl* zr zi)) ci)))
                  (loop new-zr new-zi (+ c 1))))))))))
  
  (define (mbrot matrix r i step n)
    (let loop1 ((y (- n 1)))
      (if (>= y 0)
        (let loop2 ((x (- n 1)))
          (if (>= x 0)
            (begin
              (vector-set! (vector-ref matrix x) y (count r i step x y))
              (loop2 (- x 1)))
            (loop1 (- y 1)))))))
  
  (define (test n)
    (let ((matrix (make-vector n)))
      (let loop ((i (- n 1)))
        (if (>= i 0)
          (begin
            (vector-set! matrix i (make-vector n))
            (loop (- i 1)))))
      (mbrot matrix -1.0 -0.5 0.005 n)
      (vector-ref (vector-ref matrix 0) 0)))
  
  (define (main . args)
    (run-benchmark
      "mbrot"
      mbrot-iters
      (lambda (result) (equal? result 5))
      (lambda (n) (lambda () (test n)))
      75)))
