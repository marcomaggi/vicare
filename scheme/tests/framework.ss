
(library (tests framework)
  (export define-tests src-file)
  (import (ikarus))

  (define (src-file x)
    (string-append (or (getenv "IKARUS_SRC_DIR") ".") "/" x))
  (define-syntax define-tests
    (syntax-rules ()
      [(_ test-all [p0 e0] ...)
       (define test-all
         (lambda ()
           (let ([p p0] [e e0])
             (unless (p e)
               (error 'test-all "failed" 
                      '(p0 e0) e))) 
           ...
           (printf "[~s: ~s] Happy Happy Joy Joy\n" 
                   (length '(p0 ...))'test-all  )))])))
