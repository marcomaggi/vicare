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

(library (r6rs contrib)
  (export run-contrib-tests)
  (import (rnrs)
          (r6rs test)
          (prefix (r6rs contrib helper1) L:))

  ;; Definitions ----------------------------------------

  ;; from Derick Eddington:
  (define-syntax my-letrec
    (syntax-rules ()
      [(_ ([v e] ...) . b)
       (let ()
         (define t (list e ...))
         (define v (let ([v (car t)]) (set! t (cdr t)) v))
         ...
         . b)]))

  ;; Expressions ----------------------------------------

  (define (run-contrib-tests)

    ;; from Derick Eddington:
    (test (my-letrec ([f (lambda (x) (g x 2))]
                      [g (lambda (x y) (+ x y))])
            (f 1))
          3)

    ;; from Derick Eddington:
    (test (L:s L:x) 'ok)

    ;;;
    ))
