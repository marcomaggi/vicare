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

(library (r6rs mutable-strings)
  (export run-mutable-strings-tests)
  (import (rnrs)
          (rnrs mutable-strings)
          (r6rs test))

  (define (f) (make-string 3 #\*))
  (define (g) "***")

  (define (run-mutable-strings-tests)

    (test/unspec (string-set! (f) 0 #\?))
    (test/unspec-or-exn (string-set! (g) 0 #\?)
                        &assertion)
    (test/unspec-or-exn (string-set! (symbol->string 'immutable)
                                     0
                                     #\?)
                        &assertion)

    ;;
    ))

