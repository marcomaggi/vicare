#!../src/ikarus -b ikarus.boot --r6rs-script

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


(import (ikarus)
        (tests bitwise-op)
        (tests reader)
        (tests lists)
        (tests bytevectors)
        (tests strings)
        (tests hashtables)
        (tests numerics)
        ;(tests numbers)
        (tests bignums)
        (tests fixnums)
        (tests div-and-mod)
        (tests fxcarry)
        (tests bignum-to-flonum)
        (tests string-to-number)
        (tests input-ports)
        (tests fldiv-and-mod)
        (tests parse-flonums)
        (tests io)
        (tests case-folding)
        (tests sorting)
        (tests fasl)
        )

(define (test-exact-integer-sqrt)
  (define (f i j inc)
    (when (< i j)
      (let-values ([(s r) (exact-integer-sqrt i)])
        (unless (and (= (+ (* s s) r) i)
                     (< i (* (+ s 1) (+ s 1))))
          (error 'exact-integer-sqrt "wrong result" i))
        (f (+ i inc) j inc))))
  (f 0 10000 1)
  (f 0 536870911 10000)
  (f 0 536870911000 536870911)
  (printf "[exact-integer-sqrt] Happy Happy Joy Joy\n"))

(test-bitwise-op)
(test-parse-flonums)
(test-case-folding)
(test-reader)
(test-char-syntax)
(test-bytevectors)
(test-strings)
(test-exact-integer-sqrt)
(test-bignum-to-flonum)
(test-bignum->flonum)
(test-string-to-number)
(test-div-and-mod)
(test-bignums)
(test-bignum-length)
(test-fxcarry)
(test-lists)
(test-hashtables)
(test-input-ports)
(test-bignum-conversion)
(test-fldiv-and-mod)
(test-fldiv0-and-mod0)
(test-fxdiv-and-mod)
(test-fxdiv0-and-mod0)
(test-fxlength)
(test-bitwise-bit-count)
(test-io)
(test-sorting)
(test-fasl)
(test-numerics)

(printf "Happy Happy Joy Joy\n")
