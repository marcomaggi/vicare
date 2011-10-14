#!../src/ikarus -b ../scheme/ikarus.boot --r6rs-script
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


(import (ikarus))
(optimize-level 2)
;(cp0-effort-limit 100)
;(cp0-size-limit 10)
;(optimizer-output #t)
(pretty-width 200)
(define (run name)
  (let ([proc (time-it (format "compile-~a" name) 
                (lambda ()
                  (eval 'main 
                    (environment
                      (list 'rnrs-benchmarks name)))))])
    (proc)))

(verbose-timer #t)
(apply 
  (case-lambda
    [(script-name bench-name) 
     (run (string->symbol bench-name))]
    [(script-name . args)
     (error script-name 
        (if (null? args)
            "missing benchmark name"
            "too many arguments"))])
  (command-line-arguments))
