(library (tests bitwise)
  (export run-tests)
  (import (ikarus) (tests framework))


  (define (test-base-case op i0 i1 r)
    (assert (= (op i0 i1) r)))

  (define (test-base-cases)

    (test-base-case bitwise-and 0 0 0)
    (test-base-case bitwise-and 0 1 0)
    (test-base-case bitwise-and 1 0 0)
    (test-base-case bitwise-and 1 1 1)

    (test-base-case bitwise-ior 0 0 0)
    (test-base-case bitwise-ior 0 1 1)
    (test-base-case bitwise-ior 1 0 1)
    (test-base-case bitwise-ior 1 1 1)

    (test-base-case bitwise-xor 0 0 0)
    (test-base-case bitwise-xor 0 1 1)
    (test-base-case bitwise-xor 1 0 1)
    (test-base-case bitwise-xor 1 1 0))

  (define (generate-numbers)
    (define N 68)
    (define (n* n i)
      (if (zero? i)
          '()
          (cons n (n* (bitwise-arithmetic-shift-left n 1) (- i 1)))))
    (append 
      (n* 1 N)
      (n* -1 N)
      (map sub1 (n* 1 N))
      (map sub1 (n* -1 N))
      (map add1 (n* 1 N))
      (map add1 (n* -1 N))))


  (define (one-bit n)
   (if (even? n) 0 1))

  (define (unit? n)
    (or (= n 0) (= n -1)))

  (define (trusted op n1 n2)
    (if (and (unit? n1) (unit? n2))
        (op n1 n2)
        (+ (one-bit (op (one-bit n1) (one-bit n2)))
           (bitwise-arithmetic-shift-left 
             (trusted op 
               (bitwise-arithmetic-shift-right n1 1)
               (bitwise-arithmetic-shift-right n2 1))
             1))))

  (define (test-case op)
    (define ls (generate-numbers))
    (define id 0)
    (for-each 
      (lambda (n1)
        (for-each
          (lambda (n2) 
            (let ([r0 (op n1 n2)]
                  [r1 (trusted op n1 n2)])
              (unless (= r0 r1)
                (printf "id=~s ~x ~x ~x ~x\n" id n1 n2 r0 r1)
                (error 'test-bitwise-op 
                       "mismatch/op/a0/a1/got/expected" op n1 n2 r0 r1))
              (set! id (+ id 1))))
          ls))
      ls))

  (define (test-other-cases)
    (test-case bitwise-and)
    (test-case bitwise-ior)
    (test-case bitwise-xor)
    )


  (define (run-tests)
    (test-base-cases)
    (test-other-cases)))

 
