;;; FFT - Fast Fourier Transform, translated from "Numerical Recipes in C"
  
(library (rnrs-benchmarks fft)
  (export main)
  (import (rnrs) (rnrs r5rs) (rnrs arithmetic flonums) (rnrs-benchmarks))

  ;(define flsin sin)

  (define (four1 data)
    (let ((n (vector-length data))
          (pi*2 6.28318530717959)) ; to compute the inverse, negate this value
  
      ; bit-reversal section
  
      (let loop1 ((i 0) (j 0))
        (if (< i n)
          (begin
            (if (< i j)
              (begin
                (let ((temp (vector-ref data i)))
                  (vector-set! data i (vector-ref data j))
                  (vector-set! data j temp))
                (let ((temp (vector-ref data (+ i 1))))
                  (vector-set! data (+ i 1) (vector-ref data (+ j 1)))
                  (vector-set! data (+ j 1) temp))))
            (let loop2 ((m (quotient n 2)) (j j))
              (if (and (>= m 2) (>= j m))
                (loop2 (quotient m 2) (- j m))
                (loop1 (+ i 2) (+ j m)))))))
  
      ; Danielson-Lanczos section
  
      (let loop3 ((mmax 2))
        (if (< mmax n)
          (let* ((theta
                  (fl/ pi*2 (exact->inexact mmax)))
                 (wpr
                  (let ((x (flsin (fl* 0.5 theta))))
                    (fl* -2.0 (fl* x x))))
                 (wpi
                  (flsin theta)))
            (let loop4 ((wr 1.0) (wi 0.0) (m 0))
              (if (< m mmax)
                (begin
                  (let loop5 ((i m))
                    (if (< i n)
                      (let* ((j
                              (+ i mmax))
                             (tempr
                              (fl-
                                (fl* wr (vector-ref data j))
                                (fl* wi (vector-ref data (+ j 1)))))
                             (tempi
                              (fl+
                                (fl* wr (vector-ref data (+ j 1)))
                                (fl* wi (vector-ref data j)))))
                        (vector-set! data j
                          (fl- (vector-ref data i) tempr))
                        (vector-set! data (+ j 1)
                          (fl- (vector-ref data (+ i 1)) tempi))
                        (vector-set! data i
                          (fl+ (vector-ref data i) tempr))
                        (vector-set! data (+ i 1)
                          (fl+ (vector-ref data (+ i 1)) tempi))
                        (loop5 (+ j mmax)));***))
                  (loop4 (fl+ (fl- (fl* wr wpr) (fl* wi wpi)) wr)
                         (fl+ (fl+ (fl* wi wpr) (fl* wr wpi)) wi)
                         (+ m 2)))))
  ));******
            (loop3 (* mmax 2)))))))
  
  (define data
    (make-vector 1024 0.0))
   
  (define (run data)
    (four1 data)
    (vector-ref data 0))
  
  (define (main . args)
    (run-benchmark
      "fft"
      fft-iters
      (lambda (result) (equal? result 0.0))
      (lambda (data) (lambda () (run data)))
      data)))
