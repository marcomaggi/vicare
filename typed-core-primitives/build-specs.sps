;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: build typed core primitive specifications
;;;Date: Tue Jan  5, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (build-specs)
  (import (vicare)
    (typed-core-primitives annotation-objects)
    (typed-core-primitives bignums)
    (typed-core-primitives booleans)
    (typed-core-primitives bytevectors)
    (typed-core-primitives cflonums)
    (typed-core-primitives characters)
    (typed-core-primitives code-objects)
    (typed-core-primitives compnums)
    (typed-core-primitives condition-objects)
    (typed-core-primitives configuration)
    (typed-core-primitives collect)
    (typed-core-primitives control)
    (typed-core-primitives enum-sets)
    (typed-core-primitives environment-inquiry)
    (typed-core-primitives eval-and-environments)
    (typed-core-primitives expander)
    (typed-core-primitives ffi)
    (typed-core-primitives fixnums)
    (typed-core-primitives flonums)
    (typed-core-primitives generic-primitives)
    (typed-core-primitives hash-tables)
    (typed-core-primitives input-output)
    (typed-core-primitives keywords)
    (typed-core-primitives library-utils)
    (typed-core-primitives numerics)
    (typed-core-primitives object-utilities)
    (typed-core-primitives pairs-and-lists)
    (typed-core-primitives pointers)
    (typed-core-primitives posix)
    (typed-core-primitives ratnums)
    (typed-core-primitives records)
    (typed-core-primitives strings)
    (typed-core-primitives structs)
    (typed-core-primitives symbols)
    (typed-core-primitives times-and-dates)
    (typed-core-primitives transcoders)
    (typed-core-primitives vectors))


;;;; code

(display ";; typed-core-primitives.specs\n\n(define-constant VICARE-TYPED-CORE-PRIMITIVES '(\n")
(typed-core-primitives.annotation-objects)
(typed-core-primitives.bignums)
(typed-core-primitives.booleans)
(typed-core-primitives.bytevectors)
(typed-core-primitives.cflonums)
(typed-core-primitives.characters)
(typed-core-primitives.code-objects)
(typed-core-primitives.compnums)
(typed-core-primitives.condition-objects)
(typed-core-primitives.configuration)
(typed-core-primitives.collect)
(typed-core-primitives.control)
(typed-core-primitives.enum-sets)
(typed-core-primitives.environment-inquiry)
(typed-core-primitives.eval-and-environments)
(typed-core-primitives.expander)
(typed-core-primitives.ffi)
(typed-core-primitives.fixnums)
(typed-core-primitives.flonums)
(typed-core-primitives.generic-primitives)
(typed-core-primitives.hash-tables)
(typed-core-primitives.input-output)
(typed-core-primitives.keywords)
(typed-core-primitives.library-utils)
(typed-core-primitives.numerics)
(typed-core-primitives.object-utilities)
(typed-core-primitives.pairs-and-lists)
(typed-core-primitives.pointers)
(typed-core-primitives.posix)
(typed-core-primitives.ratnums)
(typed-core-primitives.records)
(typed-core-primitives.strings)
(typed-core-primitives.structs)
(typed-core-primitives.symbols)
(typed-core-primitives.times-and-dates)
(typed-core-primitives.transcoders)
(typed-core-primitives.vectors)
(display "))\n\n;;; end of file\n")
(flush-output-port (current-output-port))

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
