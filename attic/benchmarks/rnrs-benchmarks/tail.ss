;;; TAIL -- One of the Kernighan and Van Wyk benchmarks.

(library (rnrs-benchmarks tail)
  (export main)
  (import (rnrs) (rnrs-benchmarks))

  (define inport #f)
  (define outport #f)
  
  (define (readline port line-so-far)
    (let ((x (read-char port)))
      (cond ((eof-object? x)
             x)
            ((char=? x #\newline)
             (list->string (reverse
                            (cons x line-so-far))))
            (#t (readline port (cons x line-so-far))))))
  
  (define (tail-r-aux port file-so-far)
    (let ((x (readline port '())))
      (if (eof-object? x)
          (begin
            (display file-so-far outport)
            (close-output-port outport))
          (tail-r-aux port (cons x file-so-far)))))
  
  (define (tail-r port)
    (tail-r-aux port '()))
  
  (define (go)
    (set! inport (open-input-file "bib"))
    (set! outport 
      (open-file-output-port "foo" 
        (file-options no-fail)
        'block
        (native-transcoder)))
    (tail-r inport)
    (close-input-port inport))
  
  (define (main . args)
    (run-benchmark
     "tail"
     tail-iters
     (lambda (result) #t)
     (lambda () (lambda () (go))))))
