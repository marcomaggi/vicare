#!/usr/bin/env scheme-script 
(import (ikarus))

(define max-bytes 100000000)

(define devrand (open-file-input-port "/dev/urandom"))

(define (rand-length)
  (add1 (mod 
          (bytevector-u16-ref (get-bytevector-n devrand 2) 0 'little)
          1024)))

(define (echo host)
  (printf "Connecting\n")
  (let-values ([(ip op) (tcp-connect host "echo")])
    (printf "Connected\n")
    (let f ([bytes 0])
      (printf "~s " bytes)
      (when (<= bytes max-bytes)
        (let ([n (rand-length)])
          (let ([bv (get-bytevector-n devrand n)])
            (put-bytevector op bv)
            (flush-output-port op)
            (let ([v (get-bytevector-n ip n)])
              (assert (equal? v bv)))
            (f (+ bytes n))))))
    (close-input-port ip)
    (close-output-port op)
    (newline)))

(echo "localhost")
