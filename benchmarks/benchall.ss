#!/usr/bin/env ikarus --r6rs-script
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

(define all-benchmarks
  '(ack array1 bibfreq boyer browse cat compiler conform cpstak ctak dderiv
    deriv destruc diviter divrec dynamic earley fft fib fibc fibfp
    fpsum gcbench #|gcold|# graphs lattice matrix maze mazefun mbrot
    nbody nboyer nqueens ntakl nucleic paraffins parsing perm9 peval
    pi pnpoly primes puzzle quicksort ray sboyer scheme simplex
    slatex string sum sum1 sumfp sumloop sumloop2 tail tak takl 
    trav1 trav2 triangl wc))

;(define all-benchmarks
;  '(cat tail wc slatex))

        
(define cmd 
  "../src/ikarus -b ../scheme/ikarus.boot --r6rs-script bench.ss ~a")

(for-each 
  (lambda (x) 
    (fprintf (current-error-port) "running ~s\n" x)
    (for-each 
      (lambda (_)
        (unless (zero? (system (format cmd x)))
          (fprintf (current-error-port) "ERROR: ~s failed\n" x)))
      (make-list 5)))
  all-benchmarks)

