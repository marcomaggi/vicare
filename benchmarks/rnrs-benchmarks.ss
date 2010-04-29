;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (rnrs-benchmarks)
  (export run-benchmark fatal-error include-source 
    call-with-output-file/truncate fast-run
     ack-iters 
     array1-iters
     bibfreq-iters
     boyer-iters
     browse-iters
     cat-iters
     compiler-iters
     conform-iters
     cpstak-iters
     ctak-iters
     dderiv-iters
     deriv-iters
     destruc-iters
     diviter-iters
     divrec-iters
     dynamic-iters
     earley-iters
     fft-iters
     fib-iters
     fibc-iters
     fibfp-iters
     fpsum-iters
     gcbench-iters
     gcold-iters
     graphs-iters
     lattice-iters
     matrix-iters
     maze-iters
     mazefun-iters
     mbrot-iters
     nbody-iters
     nboyer-iters
     nqueens-iters
     ntakl-iters
     nucleic-iters
     takl-iters
     paraffins-iters
     parsing-iters
     perm9-iters
     pnpoly-iters
     peval-iters
     pi-iters
     primes-iters
     puzzle-iters
     quicksort-iters
     ray-iters
     sboyer-iters
     scheme-iters
     simplex-iters
     slatex-iters
     sum-iters
     sum1-iters
     string-iters
     sumfp-iters
     sumloop-iters
     tail-iters
     tak-iters
     trav1-iters
     trav2-iters
     triangl-iters
     wc-iters)

  (import (ikarus))

  (define call-with-output-file/truncate
    (lambda (file-name proc)
      (let ([p (open-file-output-port 
                 file-name 
                 (file-options no-fail)
                 'block
                 (native-transcoder))])
        (call-with-port p proc))))

  (define-syntax include-source
    (lambda (x)
      (syntax-case x ()
        [(ctxt name) 
         (cons #'begin
           (with-input-from-file 
             (format "rnrs-benchmarks/~a" (syntax->datum #'name))
             (lambda ()
               (let f ()
                 (let ([x (read)])
                   (cond
                     [(eof-object? x) '()]
                     [else 
                      (cons (datum->syntax #'ctxt x) (f))]))))))])))

  (define (fatal-error . args)
    (error 'fatal-error "~a"
      (apply (lambda (x) (format "~a" x)) args)))

  (define fast-run (make-parameter #f))
  
  (define (run-bench count run)
    (import (ikarus system $fx))
    (unless ($fx= count 0)
      (let f ([count ($fx- count 1)] [run run])
        (cond
          [($fx= count 0) (run)]
          [else 
           (begin (run) (f ($fx- count 1) run))]))))

  (define (run-benchmark name count ok? run-maker . args)
    (let ([run (apply run-maker args)])
      (let ([result 
             (time-it (format "~a (~a)" name count)
               (if (fast-run) 
                   run
                   (lambda () (run-bench count run))))])
        (unless (ok? result) 
          (error #f "*** wrong result ***")))))


  ; Gabriel benchmarks
  (define boyer-iters        50)
  (define browse-iters      600)
  (define cpstak-iters     1700)
  (define ctak-iters        160)
  (define dderiv-iters  3000000)
  (define deriv-iters   4000000)
  (define destruc-iters     800)
  (define diviter-iters 1200000)
  (define divrec-iters  1200000)
  (define puzzle-iters      180)
  (define tak-iters        3000)
  (define takl-iters        500)
  (define trav1-iters       150)
  (define trav2-iters        40)
  (define triangl-iters      12)
  ; Kernighan and Van Wyk benchmarks
  (define ack-iters           20)
  (define array1-iters        2)
  (define cat-iters           12)
  (define string-iters        4)
  (define sum1-iters          5)
  (define sumloop-iters       2)
  (define tail-iters          4)
  (define wc-iters           15)
  
  ; C benchmarks
  (define fft-iters        4000)
  (define fib-iters           6)
  (define fibfp-iters         2)
  (define mbrot-iters       120)
  (define nucleic-iters      12)
  (define pnpoly-iters   140000)
  (define sum-iters       30000)
  (define sumfp-iters      8000)
  (define tfib-iters         20)
  
  ; Other benchmarks
  (define conform-iters      70)
  (define dynamic-iters      70)
  (define earley-iters      400)
  (define fibc-iters        900)
  (define graphs-iters      500)
  (define lattice-iters       2)
  (define matrix-iters      600)
  (define maze-iters       4000)
  (define mazefun-iters    2500)
  (define nqueens-iters    4000)
  (define ntakl-iters       600)
  (define paraffins-iters  1800)
  (define peval-iters       400)
  (define pi-iters            3)
  (define primes-iters   180000)
  (define ray-iters           5)
  (define scheme-iters    40000)
  (define simplex-iters  160000)
  (define slatex-iters       30)
  (define perm9-iters        12)
  (define nboyer-iters      150)
  (define sboyer-iters      200)
  (define gcbench-iters       2)
  (define compiler-iters    500)

  ; New benchmarks
  (define parsing-iters    360)
  (define gcold-iters      600)

  (define quicksort-iters 60)
  (define fpsum-iters 60)
  (define nbody-iters         1)
  (define bibfreq-iters 2)
)
  
