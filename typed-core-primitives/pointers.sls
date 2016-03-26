;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for pointers core primitives
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
(library (typed-core-primitives pointers)
  (export typed-core-primitives.pointers)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.pointers)


;;;; pointers, safe functions

(section

(declare-type-predicate       pointer?		<pointer>)
(declare-type-predicate/false false-or-pointer?	<pointer>)
(declare-type-predicate/maybe maybe-pointer?	<pointer>)
(declare-type-predicate/list  list-of-pointers?	<pointer>)

;;; --------------------------------------------------------------------

(declare-core-primitive pointer-null?
    (safe)
  (signatures
   ((<pointer>)			=> (<boolean>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive null-pointer
    (safe)
  (signatures
   (()				=> (<pointer>)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive set-pointer-null!
    (safe)
  (signatures
   ((<pointer>)			=> (<void>)))
  (attributes
   ((_)				result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive pointer-clone
    (safe)
  (signatures
   ((<pointer>)			=> (<pointer>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive pointer-diff
    (safe)
  (signatures
   ((<pointer> <pointer>)	=> (<exact-integer>)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive pointer-add
    (safe)
  (signatures
   ((<pointer> <exact-integer>)	=> (<pointer>)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive pointer-and-offset?
    (safe)
  (signatures
   ((<pointer> <exact-integer>)	=> (<boolean>)))
  (attributes
   ((_ _)			effect-free)))

;;; --------------------------------------------------------------------

(declare-pointer-unary/multi-comparison pointer=?)
(declare-pointer-unary/multi-comparison pointer!=?)
(declare-pointer-unary/multi-comparison pointer<?)
(declare-pointer-unary/multi-comparison pointer>?)
(declare-pointer-unary/multi-comparison pointer<=?)
(declare-pointer-unary/multi-comparison pointer>=?)

;;; --------------------------------------------------------------------

(declare-core-primitive pointer->integer
    (safe)
  (signatures
   ((<pointer>)			=> (<exact-integer>)))
  (attributes
   ((_)				effect-free result-true)))

;;This is not foldable because we do not want poiner objects to go in fasl files.
;;
(declare-core-primitive integer->pointer
    (safe)
  (signatures
   ((<exact-integer>)		=> (<pointer>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-hash-function pointer-hash <pointer> safe)

/section)


;;;; pointers, unsafe functions

(section

(declare-pointer-binary-comparison $pointer= unsafe)

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
