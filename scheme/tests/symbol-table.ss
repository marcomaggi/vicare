
(library (tests symbol-table)
  (export run-tests)
  (import
    (ikarus) 
    (only (ikarus system $symbols) $symbol-table-size))

  (define (test-gcable-symbols n)
    (let ([st1 ($symbol-table-size)])
      (do ((i 0 (+ i 1))) 
          ((= i n)) 
        (string->symbol (number->string i)))
      (collect)
      (let ([st2 ($symbol-table-size)])
        (assert (< (- st2 st1) n)))))


  (define (test-reference-after-gc)
    (define random-string
      (lambda (n)
        (list->string
         (map (lambda (n)
                (integer->char (+ (char->integer #\a) (random 26))))
              (make-list n)))))
    (newline)
    (let ([str1 (random-string 70)]
          [str2 (random-string 70)])
      (printf "sym1=~s\n" (string->symbol str1))
      (do ((i 0 (+ i 1))) ((= i 1024)) (collect))
      (let ([sym1 (string->symbol str1)])
        (printf "sym1=~s\n" (string->symbol str1))
        (printf "sym2=~s\n" (string->symbol str2))
        (let ([sym3 (string->symbol str1)])
          (printf "sym3=~s\n" (string->symbol str1))
          (assert (eq? sym1 sym3))))))


  (define (run-tests)
    (test-gcable-symbols 1000000)
    (test-reference-after-gc)))


