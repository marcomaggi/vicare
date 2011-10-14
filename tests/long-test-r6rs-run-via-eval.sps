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

(import (rnrs) (rnrs eval) (r6rs test))

(define-syntax test-library
   (syntax-rules ()
     [(_ test-proc library-name)
      (test/unspec (eval '(test-proc) (environment 'library-name)))]))

(test-library run-base-tests               (r6rs base))
(test-library run-reader-tests             (r6rs reader))
(test-library run-unicode-tests            (r6rs unicode))
(test-library run-bytevectors-tests        (r6rs bytevectors))
(test-library run-lists-tests              (r6rs lists))
(test-library run-sorting-tests            (r6rs sorting))
(test-library run-control-tests            (r6rs control))
(test-library run-records-syntactic-tests  (r6rs records syntactic))
(test-library run-records-procedural-tests (r6rs records procedural))
(test-library run-exceptions-tests         (r6rs exceptions))
(test-library run-conditions-tests         (r6rs conditions))
(test-library run-io-ports-tests           (r6rs io ports))
(test-library run-io-simple-tests          (r6rs io simple))
(test-library run-programs-tests           (r6rs programs))
(test-library run-arithmetic-fixnums-tests (r6rs arithmetic fixnums))
(test-library run-arithmetic-flonums-tests (r6rs arithmetic flonums))
(test-library run-arithmetic-bitwise-tests (r6rs arithmetic bitwise))
(test-library run-syntax-case-tests        (r6rs syntax-case))
(test-library run-hashtables-tests         (r6rs hashtables))
(test-library run-enums-tests              (r6rs enums))
(test-library run-eval-tests               (r6rs eval))
(test-library run-mutable-pairs-tests      (r6rs mutable-pairs))
(test-library run-mutable-strings-tests    (r6rs mutable-strings))
(test-library run-r5rs-tests               (r6rs r5rs))
(test-library run-contrib-tests            (r6rs contrib))

(report-test-results)

