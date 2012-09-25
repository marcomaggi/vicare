#!../src/vicare -b vicare.boot --r6rs-script

;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; Modified by Marco Maggi
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

(define test-libraries '(
  lists strings bytevectors hashtables fixnums bignums numerics
  bitwise enums pointers sorting io fasl reader case-folding
  parse-flonums string-to-number bignum-to-flonum div-and-mod
  fldiv-and-mod unicode normalization repl set-position guardians
  symbol-table scribble))

(define (run-test-from-library x)
  (printf "[testing ~a] ..." x)
  (eval '(run-tests) (environment `(tests ,x)))
  (printf " OK\n"))

(apply
  (case-lambda
    [(script) (for-each run-test-from-library test-libraries)]
    [(script . test-name*)
     (let ([test-name* (map string->symbol test-name*)])
       (for-each
         (lambda (x)
           (unless (memq x test-libraries)
             (error script "invalid test name" x)))
         test-name*)
       (for-each run-test-from-library test-name*))])
  (command-line))


(printf "Happy Happy Joy Joy\n")
