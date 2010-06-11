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

(library (r6rs mutable-pairs)
  (export run-mutable-pairs-tests)
  (import (rnrs)
          (rnrs mutable-pairs)
          (r6rs test))

  (define (f) (list 'not-a-constant-list))
  (define (g) '(constant-list))

  (define (run-mutable-pairs-tests)

    (test/unspec (set-car! (f) 3))
    (test/unspec-or-exn (set-car! (g) 3)
                        &assertion)

    (test (let ((x (list 'a 'b 'c 'a))
                (y (list 'a 'b 'c 'a 'b 'c 'a)))
            (set-cdr! (list-tail x 2) x)
            (set-cdr! (list-tail y 5) y)
            (list
             (equal? x x)
             (equal? x y)
             (equal? (list x y 'a) (list y x 'b))))
          '(#t #t #f))

    ;;
    ))

