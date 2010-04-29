
(library (tests normalization)
  (export run-tests)
  (import (ikarus) (tests framework) (unicode-data))

(define (reset) (error 'reset "yukk"))
(define (enumerate ls)
  (let f ([ls ls] [i 0])
    (cond
      [(null? ls) '()]
      [else (cons i (f (cdr ls) (+ i 1)))])))
(define (list-head ls n)
  (if (zero? n)
      '()
      (cons (car ls) (list-head (cdr ls) (- n 1)))))

(define (split str)
  (remove ""
    (let f ([i 0] [n (string-length str)])
      (cond
        [(= i n) (list (substring str 0 n))]
        [(char=? (string-ref str i) #\space)
         (cons (substring str 0 i) 
               (split (substring str (+ i 1) n)))]
        [else (f (add1 i) n)]))))

(define (conv x)
  (list->string
    (map (lambda (x) (integer->char (string->number x 16)))
         (split x))))

(define (run-tests)
  (let ([data (map (lambda (x) (map conv (list-head x 5)))
                   (filter (lambda (x) (>= (length x) 5))
                     (get-unicode-data
                       (src-file "unicode/UNIDATA/NormalizationTest.txt"))))])
    (define NFD string-normalize-nfd)
    (define NFKD string-normalize-nfkd)
    (define NFC string-normalize-nfc)
    (define NFKC string-normalize-nfkc)

    (define (test1)
      (for-each
        (lambda (x testno)
          (apply
            (lambda (c1 c2 c3 c4 c5)
              (unless (and (string=? c2 (NFC c1) (NFC c2) (NFC c3))
                           (string=? c4 (NFC c4) (NFC c5)))
                (parameterize ([print-unicode #f])
                  (printf "test 1[~s] failed for ~s\n" testno x)
                  (printf "       c2 = ~s\n" c2)
                  (printf "  NFC(c1) = ~s\n" (NFC c1))
                  (printf "  NFC(c2) = ~s\n" (NFC c2))
                  (printf "  NFC(c3) = ~s\n" (NFC c3))
                  (printf "       c4 = ~s\n" c4)
                  (printf "  NFC(c4) = ~s\n" (NFC c4))
                  (printf "  NFC(c5) = ~s\n" (NFC c5))
                  (reset))))
            x))
        data (enumerate data)))

    (define (test2)
      (for-each
        (lambda (x testno)
          (apply
            (lambda (c1 c2 c3 c4 c5)
              (unless (and (string=? c3 (NFD c1) (NFD c2) (NFD c3))
                           (string=? c5 (NFD c4) (NFD c5)))
                (parameterize ([print-unicode #f])
                  (printf "test 2[~s] failed for ~s\n" testno x)
                  (printf "       c3 = ~s\n" c3)
                  (printf "  NFD(c1) = ~s\n" (NFD c1))
                  (printf "  NFD(c2) = ~s\n" (NFD c2))
                  (printf "  NFD(c3) = ~s\n" (NFD c3))
                  (printf "       c5 = ~s\n" c5)
                  (printf "  NFD(c4) = ~s\n" (NFD c4))
                  (printf "  NFD(c5) = ~s\n" (NFD c5))
                  (reset))))
            x))
        data (enumerate data)))

    (define (test3)
      (for-each
        (lambda (x testno)
          (apply
            (lambda (c1 c2 c3 c4 c5)
              (unless (string=? c4 (NFKC c1) (NFKC c2) (NFKC c3) (NFKC c4) (NFKC c5))
                (parameterize ([print-unicode #f])
                  (printf "test 3[~s] failed for ~s\n" testno x)
                  (printf "       c4 = ~s\n" c4)
                  (printf "  NFKC(c1) = ~s\n" (NFKC c1))
                  (printf "  NFKC(c2) = ~s\n" (NFKC c2))
                  (printf "  NFKC(c3) = ~s\n" (NFKC c3))
                  (printf "  NFKC(c4) = ~s\n" (NFKC c4))
                  (printf "  NFKC(c5) = ~s\n" (NFKC c5))
                  (reset))))
            x))
        data (enumerate data)))

    (define (test4)
      (for-each
        (lambda (x testno)
          (apply
            (lambda (c1 c2 c3 c4 c5)
              (unless (string=? c5 (NFKD c1) (NFKD c2) (NFKD c3) (NFKD c4) (NFKD c5))
                (parameterize ([print-unicode #f])
                  (printf "test 4[~s] failed for ~s\n" testno x)
                  (printf "       c5 = ~s\n" c5)
                  (printf "  NFKD(c1) = ~s\n" (NFKD c1))
                  (printf "  NFKD(c2) = ~s\n" (NFKD c2))
                  (printf "  NFKD(c3) = ~s\n" (NFKD c3))
                  (printf "  NFKD(c4) = ~s\n" (NFKD c4))
                  (printf "  NFKD(c5) = ~s\n" (NFKD c5))
                  (reset))))
            x))
        data (enumerate data)))

    (printf " running ~s tests ..." (length data))
    (test1)
    (test2)
    (test3)
    (test4))))
  
