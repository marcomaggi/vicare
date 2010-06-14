;;;Copyright (c) 2008 Matthew Flatt
;;;
;;;This library is free software;  you can redistribute it and/or modify
;;;it  under the  terms of  the GNU  Library General  Public  License as
;;;published by  the Free Software  Foundation; either version 2  of the
;;;License, or (at your option) any later version.
;;;
;;;This library is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;Library General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU Library  General Public
;;;License along with  this library; if not, write  to the Free Software
;;;Foundation,  Inc.,  51  Franklin  Street,  Fifth  Floor,  Boston,  MA
;;;02110-1301 USA.

#!r6rs

(library (r6rs r5rs)
  (export run-r5rs-tests)
  (import (rnrs)
          (rnrs r5rs)
          (rnrs eval)
          (r6rs test))

  ;; ----------------------------------------

  (define a-stream
    (letrec ((next
              (lambda (n)
                (cons n (delay (next (+ n 1)))))))
      (next 0)))
  (define head car)
  (define tail
    (lambda (stream) (force (cdr stream))))

  (define count 0)
  (define p
    (delay (begin (set! count (+ count 1))
                  (if (> count x)
                      count
                      (force p)))))
  (define x 5)

  ;; ----------------------------------------

  (define (run-r5rs-tests)

    (test (modulo 13 4)            1)
    (test (remainder 13 4)         1)

    (test (modulo -13 4)           3)
    (test (remainder -13 4)        -1)

    (test (modulo 13 -4)           -3)
    (test (remainder 13 -4)        1)

    (test (modulo -13 -4)          -1)
    (test (remainder -13 -4)       -1)

    (test (remainder -13 -4.0)     -1.0)

    (test (force (delay (+ 1 2)))    3)

    (test (let ((p (delay (+ 1 2))))
            (list (force p) (force p)))
          '(3 3))


    (test (head (tail (tail a-stream))) 2)

    (test/unspec p)
    (test (force p) 6)
    (test/unspec p)
    (test (begin (set! x 10)
                 (force p))
          6)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  quotient, remainder, and modulo tests from Ikarus's
    ;;  "bignums" test suite

    (test (quotient 348972 3434)
          101)
    (test (quotient -348972 3434)
          -101)
    (test (quotient 348972 -3434)
          -101)
    (test (quotient -348972 -3434)
          101)
    (test (quotient 536870912 238)
          2255760)
    (test (quotient -536870912 238)
          -2255760)
    (test (quotient 536870912 -238)
          -2255760)
    (test (quotient -536870912 -238)
          2255760)
    (test (quotient 536870912238479837489374 324873)
          1652556267336712615)
    (test (quotient -536870912238479837489374 324873)
          -1652556267336712615)
    (test (quotient 536870912238479837489374 -324873)
          -1652556267336712615)
    (test (quotient -536870912238479837489374 -324873)
          1652556267336712615)
    (test (quotient 536870912238479837489374 3248732398479823749283)
          165)
    (test (quotient -536870912238479837489374 3248732398479823749283)
          -165)
    (test (quotient 536870912238479837489374 -3248732398479823749283)
          -165)
    (test (quotient -536870912238479837489374 -3248732398479823749283)
          165)
    (test (quotient 5368709122384798374893743894798327498234 3248732398479823749283)
          1652555047284588078)
    (test (quotient -5368709122384798374893743894798327498234 3248732398479823749283)
          -1652555047284588078)
    (test (quotient 5368709122384798374893743894798327498234 -3248732398479823749283)
          -1652555047284588078)
    (test (quotient -5368709122384798374893743894798327498234 -3248732398479823749283)
          1652555047284588078)
    (test (remainder 23 349839489348)
          23)
    (test (remainder -23 349839489348)
          -23)
    (test (remainder 23 -349839489348)
          23)
    (test (remainder -23 -349839489348)
          -23)
    (test (modulo 348972 3434)
          2138)
    (test (modulo -348972 3434)
          1296)
    (test (modulo 348972 -3434)
          -1296)
    (test (modulo -348972 -3434)
          -2138)
    (test (modulo -23 349839489348)
          349839489325)
    (test (modulo -23 -349839489348)
          -23)
    (test (modulo 23 349839489348)
          23)
    (test (modulo 23 -349839489348)
          -349839489325)
    (test (remainder 536870912 238)
          32)
    (test (remainder -536870912 238)
          -32)
    (test (remainder 536870912 -238)
          32)
    (test (remainder -536870912 -238)
          -32)
    (test (modulo 536870912 238)
          32)
    (test (modulo -536870912 238)
          206)
    (test (modulo 536870912 -238)
          -206)
    (test (modulo -536870912 -238)
          -32)
    (test (modulo 536870912238479837489374 324873)
          116479)
    (test (modulo -536870912238479837489374 324873)
          208394)
    (test (modulo 536870912238479837489374 -324873)
          -208394)
    (test (modulo -536870912238479837489374 -324873)
          -116479)
    (test (modulo 536870912238479837489374 3248732398479823749283)
          830066489308918857679)
    (test (modulo 536870912238479837489374 -3248732398479823749283)
          -2418665909170904891604)
    (test (modulo -536870912238479837489374 3248732398479823749283)
          2418665909170904891604)
    (test (modulo -536870912238479837489374 -3248732398479823749283)
          -830066489308918857679)

    ;; ----------------------------------------

    (test (exact->inexact 1) 1.0)
    (test (exact->inexact 1.0) 1.0)
    (test (inexact->exact 1) 1)
    (test (inexact->exact 1.0) 1)

    ;; ----------------------------------------

    (test (eval '(cond [#t 1]) (null-environment 5)) 1)
    (test (eval '(cond [#t => (lambda (x) x)]) (null-environment 5)) #t)


    (test (eval '(cons 1 2) (scheme-report-environment 5)) '(1 . 2))

    ;;
    ))

