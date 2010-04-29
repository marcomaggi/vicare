
(library (tests set-position)
  (export run-tests)
  (import (ikarus))

  (define fname "temp-test-file")
  (define pos-list '([500 12] [720 34] [12  180] [400 4]))

  (define (write-bytes)
    (when (file-exists? fname) (delete-file fname))
    (let ([p (open-file-output-port fname)])
      (for-each
        (lambda (x)
          (set-port-position! p (car x))
          (assert (= (port-position p) (car x)))
          (put-u8 p (cadr x))
          (assert (= (port-position p) (add1 (car x)))))
        pos-list)
      (close-output-port p)))

  (define (get-bytes)
    (let ([p (open-file-input-port fname)])
      (let ([bv (get-bytevector-all p)])
        (close-input-port p)
        bv)))

  (define (test-setting-position-for-binary-output-files)
    (write-bytes)
    (let ([bv (get-bytes)])
      (assert (= (bytevector-length bv) (add1 (apply max (map car pos-list)))))
      (for-each 
        (lambda (x)
          (assert (= (bytevector-u8-ref bv (car x)) (cadr x))))
        pos-list))
    (delete-file fname))

  (define (test-setting-position-for-binary-input-files)
    (write-bytes)
    (let ([p (open-file-input-port fname)])
      (define (check-pos x)
        (set-port-position! p (car x))
        (assert (= (port-position p) (car x)))
        (assert (= (get-u8 p) (cadr x)))
        (assert (= (port-position p) (add1 (car x)))))
      (for-each check-pos pos-list)
      (for-each check-pos (reverse pos-list))
      (close-input-port p))
    (delete-file fname))

  (define (test-fixed-input-ports)
    (assert (eof-object? 
              (let ([p (open-string-input-port "Hello")])
                (set-port-position! p 5) 
                (get-char p))))
    (assert (char=? #\o 
              (let ([p (open-string-input-port "Hello")])
                (set-port-position! p 4) 
                (get-char p))))
    (assert (eof-object? 
               (let ([p (open-bytevector-input-port #vu8(1 2 3 4 5))])
                 (set-port-position! p 5)
                 (get-u8 p))))
    (assert (= 5
               (let ([p (open-bytevector-input-port #vu8(1 2 3 4 5))])
                 (set-port-position! p 4)
                 (get-u8 p)))))


  (define (run-tests)
    (test-setting-position-for-binary-output-files)
    (test-setting-position-for-binary-input-files)
    (test-fixed-input-ports)))

