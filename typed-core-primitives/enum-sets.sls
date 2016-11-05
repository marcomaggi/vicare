;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for enum-sets core primitives
;;Date: Tue Dec 25, 2015
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
(library (typed-core-primitives enum-sets)
  (export typed-core-primitives.enum-sets)
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.enum-sets)


;;;; enum sets, safe procedure

(section

(declare-type-predicate enum-set?	<enum-set>)

(declare-core-primitive make-enumeration
    (safe)
  (signatures
   ((<list>)		=> (<enum-set>)))
  (attributes
   ((_)			 effect-free result-true)))

(declare-core-primitive enum-set-constructor
    (safe)
  (signatures
   ((<enum-set>)		=> (<enum-set-constructor>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive enum-set-member?
    (safe)
  (signatures
   ((<symbol> <enum-set>)	=> (<boolean>)))
  (attributes
   ((_ _)			 effect-free)))

(declare-core-primitive enum-set-subset?
    (safe)
  (signatures
   ((<enum-set> <enum-set>)	=> (<boolean>)))
  (attributes
   ((_ _)			 effect-free)))

(declare-core-primitive enum-set=?
    (safe)
  (signatures
   ((<symbol> <enum-set>)	=> (<boolean>)))
  (attributes
   ((_ _)			 effect-free)))

;;; --------------------------------------------------------------------
;;; set operations

(declare-core-primitive enum-set-difference
    (safe)
  (signatures
   ((<enum-set> <enum-set>)	=> (<enum-set>)))
  (attributes
   ((_ _)			 effect-free result-true)))

(declare-core-primitive enum-set-intersection
    (safe)
  (signatures
   ((<enum-set> <enum-set>)	=> (<enum-set>)))
  (attributes
   ((_ _)			 effect-free result-true)))

(declare-core-primitive enum-set-union
    (safe)
  (signatures
   ((<enum-set> <enum-set>)	=> (<enum-set>)))
  (attributes
   ((_ _)			 effect-free result-true)))

(declare-core-primitive enum-set-projection
    (safe)
  (signatures
   ((<enum-set> <enum-set>)	=> (<enum-set>)))
  (attributes
   ((_ _)			 effect-free result-true)))

(declare-core-primitive enum-set-complement
    (safe)
  (signatures
   ((<enum-set>)	=> (<enum-set>)))
  (attributes
   ((_)			 effect-free result-true)))

(declare-core-primitive enum-set-universe
    (safe)
  (signatures
   ((<enum-set>)		=> (<enum-set>)))
  (attributes
   ((_)			 effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive enum-set->list
    (safe)
  (signatures
   ((<enum-set>)		=> (<list>)))
  (attributes
   ((_)			 effect-free result-true)))

(declare-core-primitive enum-set-indexer
    (safe)
  (signatures
   ((<enum-set>)		=> (<enum-set-indexer>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive make-file-options
    (safe)
  (signatures
   ((<null>)				=> (<enum-set>))
   (((list-of <symbol>))		=> (<enum-set>)))
  (attributes
   (_			effect-free result-true)))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
