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
(import (rnrs)
        (r6rs test)
        (r6rs base)
        (r6rs reader)
        (r6rs unicode)
        (r6rs bytevectors)
        (r6rs lists)
        (r6rs sorting)
        (r6rs control)
        (r6rs records syntactic)
        (r6rs records procedural)
        (r6rs exceptions)
        (r6rs conditions)
        (r6rs io ports)
        (r6rs io simple)
        (r6rs programs)
        (r6rs arithmetic fixnums)
        (r6rs arithmetic flonums)
        (r6rs arithmetic bitwise)
        (r6rs syntax-case)
        (r6rs hashtables)
        (r6rs enums)
        (r6rs eval)
        (r6rs mutable-pairs)
        (r6rs mutable-strings)
        (r6rs r5rs)
        (r6rs contrib))

(run-base-tests)

(run-reader-tests)
(run-unicode-tests)
(run-bytevectors-tests)
(run-lists-tests)
(run-sorting-tests)
(run-control-tests)
(run-records-syntactic-tests)
(run-records-procedural-tests)
(run-exceptions-tests)
(run-conditions-tests)
(run-io-ports-tests)
(run-io-simple-tests)
(run-programs-tests)
(run-arithmetic-fixnums-tests)
(run-arithmetic-flonums-tests)
(run-arithmetic-bitwise-tests)
(run-syntax-case-tests)
(run-hashtables-tests)
(run-enums-tests)
(run-eval-tests)
(run-mutable-pairs-tests)
(run-mutable-strings-tests)
(run-r5rs-tests)
(run-contrib-tests)

(report-test-results)

;;; end of file
