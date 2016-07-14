;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for cflonums core primitives
;;Date: Tue Dec 22, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives cflonums)
  (export typed-core-primitives.cflonums)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.cflonums)


;;;; cflonums, unsafe functions

(section

(declare-type-predicate cflonum?	<cflonum>)

/section)


;;;; cflonums, unsafe functions

(section

;;; constructors

(declare-core-primitive $make-cflonum
    (unsafe)
  (signatures
   ((<flonum> <flonum>)		=> (<cflonum>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive $cflonum-real
    (unsafe)
  (signatures
   ((<cflonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $cflonum-imag
    (unsafe)
  (signatures
   ((<cflonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
