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
        (tests r6rs test)
        (tests r6rs base)
        (tests r6rs reader)
        (tests r6rs unicode)
        (tests r6rs bytevectors)
        (tests r6rs lists)
        (tests r6rs sorting)
        (tests r6rs control)
        (tests r6rs records syntactic)
        (tests r6rs records procedural)
        (tests r6rs exceptions)
        (tests r6rs conditions)
        (tests r6rs io ports)
        (tests r6rs io simple)
        (tests r6rs programs)
        (tests r6rs arithmetic fixnums)
        (tests r6rs arithmetic flonums)
        (tests r6rs arithmetic bitwise)
        (tests r6rs syntax-case)
        (tests r6rs hashtables)
        (tests r6rs enums)
        (tests r6rs eval)
        (tests r6rs mutable-pairs)
        (tests r6rs mutable-strings)
        (tests r6rs r5rs)
        (tests r6rs contrib))

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

