
(library (tests parse-flonums)
  (export run-tests)
  (import (ikarus) (tests framework))
  
  (define file (src-file "tests/rn100"))

  (define (read-all)
    (with-input-from-file file
      (lambda ()
        (let f ([ac '()])
          (let ([x (read)])
            (if (eof-object? x) 
                (reverse ac)
                (f (cons x ac))))))))

  (define (read-flonum) 
    (define (decimal x)
      (cond
        [(assv x '([#\0 . 0] [#\1 . 1] [#\2 . 2] [#\3 . 3] [#\4 . 4]
                   [#\5 . 5] [#\6 . 6] [#\7 . 7] [#\8 . 8] [#\9 . 9]))
         => cdr]
        [else #f]))
    (define (st)
      (let ([x (read-char)])
        (cond
          [(eof-object? x) x]
          [(char-whitespace? x) (st)]
          [(char=? x #\-) (- (sign))]
          [(decimal x) => num]
          [else (error 'st "invalid char" x)])))
    (define (sign)
      (let ([x (read-char)])
        (cond
          [(eof-object? x) (error 'sign "eof")]
          [(decimal x) => num]
          [else (error 'sign "invalid char" x)])))
    (define (num n)
      (let ([x (read-char)])
        (cond
          [(eof-object? x) (error 'num "eof")]
          [(decimal x) => (lambda (m) (num (+ (* n 10) m)))]
          [(char=? x #\.) (+ n (frac 0 1))]
          [else (error 'num "invalid char" x)])))
    (define (frac num den)
      (let ([x (read-char)])
        (cond
          [(or (eof-object? x) (char-whitespace? x))
           (/ num den)]
          [(decimal x) => (lambda (m) 
                            (frac (+ (* num 10) m)
                                  (* den 10)))]
          [else (error 'frac "invalid char" x)])))
    (st))

  (define smallest-flonum
    (bytevector-ieee-double-ref 
      #vu8(1 0 0 0 0 0 0 0)
      0
      'little))

  (define (gen-epsilon x)
    (let ([x (flabs x)])
      (let f ([eps smallest-flonum]) 
        (if (fl=? x (fl- x eps))
            (f (fl* eps 2.0))
            eps))))

  (define (inexact-close-enough? in ex)
    ;;; take the inexact number, and generate two 
    ;;; additional numbers: in+epsilon, in-epsilon
    ;;; turn them into exacts: e1=exact(in+epsilon), e2=exact(in-epsilon)
    ;;; ensure that at least e1 < ex < e2
    (let ([eps (gen-epsilon in)])
      (< (exact (fl- in eps)) ex (exact (fl+ in eps)))))

  (define (read-exact-all)
    (with-input-from-file file
      (lambda () 
        (let f ([ac '()])
          (let ([x (read-flonum)])
            (if (eof-object? x) 
                (reverse ac)
                (f (cons x ac))))))))

  (define (run-tests)
    (define who 'test-parse-flonums)
    (define failed #f)
    (define idx 0)
    (let ([ls1 (read-all)]
          [ls2 (read-exact-all)])
      (assert (= (length ls1) (length ls2)))
      (for-each
        (lambda (x1 x2) 
          (set! idx (+ idx 1))
          (unless (inexact-close-enough? x1 x2)
            (set! failed #t)
            (printf "test failed in line ~s on read=~s and parsed=~s\n" 
                    idx x1 x2)))
        ls1 ls2))
    (when failed (error who "failed"))
    ))
 

