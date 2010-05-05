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

(library (tests r6rs reader)
  (export run-reader-tests)
  (import (rnrs)
          (tests r6rs test))

  (define-syntax number-test
    (syntax-rules ()
      [(_ str ...)
       (begin
         (test (read (open-string-input-port str))
               (string->number str))
         ...)]))

  (define (run-reader-tests)
    (number-test
     "12"
     "+12"
     "3427384783264876238746784234"
     "0"
     "+0"
     "-12"
     "-3498738947983748939478347834"
     "-0"
     "#x-238973897AAAAAFFFFbb00bbdddcc"
     "#x238973897AAAAA000FFFFbbbbdddcc"
     "#x+07edf387"
     "#x+0"
     "#x-0"
     "#x0"
     "#b01010101010000000111111111110000"
     "#b-01010101010000000111111111110000"
     "#b+01010101010000000111111111110000"
     "#b+0"
     "#b-0"
     "#b0"
     "#d2398128321308912830912830912839"
     "#d-2398128321308912830912830912839"
     "#d+2398128321308912830912830912839"
     "#d+0"
     "#d-0"
     "#d0"
     "#o237612036721631263126371263712"
     "#o-2376120036721631263126371263712"
     "#o+23761236721631263126371263712"
     "#o+0"
     "#o-0"
     "#o0"

     "#X-238973897AAAAAFFFFbb00bbdddcc"
     "#X238973897AAAAA000FFFFbbbbdddcc"
     "#X+07edf387"
     "#X+0"
     "#X-0"
     "#X0"
     "#B01010101010000000111111111110000"
     "#B-01010101010000000111111111110000"
     "#B+01010101010000000111111111110000"
     "#B+0"
     "#B-0"
     "#B0"
     "#D2398128321308912830912830912839"
     "#D-2398128321308912830912830912839"
     "#D+2398128321308912830912830912839"
     "#D+0"
     "#D-0"
     "#D0"
     "#O237612036721631263126371263712"
     "#O-2376120036721631263126371263712"
     "#O+23761236721631263126371263712"
     "#O+0"
     "#O-0"
     "#O0"
     "#i#xf/e"
     "#x#if/e")

    (test (read (open-string-input-port "#\\nul"))
          (integer->char #x0))
    (test (read (open-string-input-port "#\\alarm"))
          (integer->char #x7))
    (test (read (open-string-input-port "#\\backspace"))
          (integer->char #x8))
    (test (read (open-string-input-port "#\\tab"))
          (integer->char #x9))
    (test (read (open-string-input-port "#\\linefeed"))
          (integer->char #xA))
    (test (read (open-string-input-port "#\\newline"))
          (integer->char #xA))
    (test (read (open-string-input-port "#\\vtab"))
          (integer->char #xB))
    (test (read (open-string-input-port "#\\page"))
          (integer->char #xC))
    (test (read (open-string-input-port "#\\return"))
          (integer->char #xD))
    (test (read (open-string-input-port "#\\esc"))
          (integer->char #x1B))
    (test (read (open-string-input-port "#\\space"))
          (integer->char #x20))
    (test (read (open-string-input-port "#\\delete"))
          (integer->char #x7F))

    ;;
    ))
