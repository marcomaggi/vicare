;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for fixnums core primitives
;;Date: Tue Dec 30, 2015
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
(library (typed-core-primitives fixnums)
  (export typed-core-primitives.fixnums)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.fixnums)


;;;; fixnums safe operations

(section

(declare-core-primitive greatest-fixnum
    (unsafe)
  (signatures
   (()				=> (<positive-fixnum>)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive least-fixnum
    (unsafe)
  (signatures
   (()				=> (<fixnum>)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive fixnum-width
    (unsafe)
  (signatures
   (()				=> (<positive-fixnum>)))
  (attributes
   (()				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate fixnum? <fixnum>)
(declare-list-of-type-predicate list-of-fixnums?	<fixnum>)

(declare-fixnum-predicate fxzero?		(replacements $fxzero?))
(declare-fixnum-predicate fxpositive?		(replacements $fxpositive?))
(declare-fixnum-predicate fxnegative?		(replacements $fxnegative?))
(declare-fixnum-predicate fxnonpositive?	(replacements $fxnonpositive?))
(declare-fixnum-predicate fxnonnegative?	(replacements $fxnonnegative?))
(declare-fixnum-predicate fxeven?		(replacements $fxeven?))
(declare-fixnum-predicate fxodd?		(replacements $fxodd?))

(declare-fixnum-predicate fixnum-in-character-range?)

(declare-core-primitive zero-fixnum?
    (safe)
  (signatures
   ((<zero-fixnum>)		=> (<true>))
   ((<positive-fixnum>)		=> (<false>))
   ((<negative-fixnum>)		=> (<false>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free))
  (replacements $fxzero?))

(declare-core-primitive non-zero-fixnum?
    (safe)
  (signatures
   ((<zero-fixnum>)		=> (<false>))
   ((<positive-fixnum>)		=> (<true>))
   ((<negative-fixnum>)		=> (<true>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive positive-fixnum?
    (safe)
  (signatures
   ((<zero-fixnum>)		=> (<false>))
   ((<positive-fixnum>)		=> (<true>))
   ((<negative-fixnum>)		=> (<false>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive negative-fixnum?
    (safe)
  (signatures
   ((<zero-fixnum>)		=> (<false>))
   ((<positive-fixnum>)		=> (<false>))
   ((<negative-fixnum>)		=> (<true>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive non-positive-fixnum?
    (safe)
  (signatures
   ((<zero-fixnum>)		=> (<true>))
   ((<positive-fixnum>)		=> (<false>))
   ((<negative-fixnum>)		=> (<true>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive non-negative-fixnum?
    (safe)
  (signatures
   ((<zero-fixnum>)		=> (<true>))
   ((<positive-fixnum>)		=> (<true>))
   ((<negative-fixnum>)		=> (<false>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; arithmetics

(declare-core-primitive fx+
    (safe)
  (signatures
   ((<positive-fixnum> <positive-fixnum>)		=> (<positive-fixnum>))
   ((<negative-fixnum> <negative-fixnum>)		=> (<negative-fixnum>))
   ((<non-negative-fixnum> <non-negative-fixnum>)	=> (<non-negative-fixnum>))
   ((<fixnum> <fixnum>)					=> (<fixnum>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-fixnum-unary/binary fx-)
(declare-fixnum-binary fx*)
(declare-fixnum-binary fxdiv)
(declare-fixnum-binary fxmod)
(declare-fixnum-binary fxdiv0)
(declare-fixnum-binary fxmod0)
(declare-fixnum-unary fxadd1)
(declare-fixnum-unary fxsub1)

(declare-fixnum-unary fxabs)
(declare-fixnum-unary fxsign)
(declare-fixnum-binary fxremainder)
(declare-fixnum-binary fxquotient)
(declare-fixnum-binary fxmodulo)

(declare-core-primitive fx+/carry
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>) => (<fixnum> <fixnum>)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive fx-/carry
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>) => (<fixnum> <fixnum>)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive fx*/carry
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>) => (<fixnum> <fixnum>)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive fxdiv-and-mod
    (safe)
  (signatures
   ((<fixnum> <fixnum>)		=> (<fixnum> <fixnum>)))
  (attributes
   ((_ _)			effect-free)))

(declare-core-primitive fxdiv0-and-mod0
    (safe)
  (signatures
   ((<fixnum> <fixnum>)		=> (<fixnum> <fixnum>)))
  (attributes
   ((_ _)			effect-free)))

;;; --------------------------------------------------------------------
;;; bitwise operations

(declare-fixnum-unary fxnot		(replacements $fxlognot))
(declare-fixnum-multi fxand		(replacements $fxlogand))
(declare-fixnum-multi fxior		(replacements $fxlogor))
(declare-fixnum-multi fxxor		(replacements $fxlogxor))
(declare-fixnum-unary fxlognot		(replacements $fxlognot))
(declare-fixnum-multi fxlogor		(replacements $fxlogor))
(declare-fixnum-multi fxlogand		(replacements $fxlogand))
(declare-fixnum-multi fxlogxor		(replacements $fxlogxor))

(declare-fixnum-unary fxlength)
(declare-fixnum-binary fxsll)
(declare-fixnum-binary fxsra)
(declare-fixnum-binary fxarithmetic-shift-left)
(declare-fixnum-binary fxarithmetic-shift-right)
(declare-fixnum-binary fxarithmetic-shift)

(declare-fixnum-unary fxbit-count)

(declare-core-primitive fxbit-field
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-core-primitive fxbit-set?
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<boolean>)))
  (attributes
   ((_ _)				foldable effect-free)))

(declare-core-primitive fxreverse-bit-field
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-core-primitive fxcopy-bit
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-core-primitive fxcopy-bit-field
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _ _)					foldable effect-free result-true)))

(declare-core-primitive fxrotate-bit-field
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _ _)				foldable effect-free result-true)))

(declare-core-primitive fxif
    (safe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-fixnum-unary fxfirst-bit-set)

;;; --------------------------------------------------------------------
;;; comparison

(declare-fixnum-unary/multi-comparison fx=?	(replacements $fx=))
(declare-fixnum-unary/multi-comparison fx!=?	(replacements $fx!=))
(declare-fixnum-unary/multi-comparison fx<?	(replacements $fx<))
(declare-fixnum-unary/multi-comparison fx>?	(replacements $fx>))
(declare-fixnum-unary/multi-comparison fx<=?	(replacements $fx<=))
(declare-fixnum-unary/multi-comparison fx>=?	(replacements $fx>=))

;;;

(declare-fixnum-unary/multi-comparison fx=	(replacements $fx=))
(declare-fixnum-unary/multi-comparison fx!=	(replacements $fx!=))
(declare-fixnum-unary/multi-comparison fx<	(replacements $fx<))
(declare-fixnum-unary/multi-comparison fx>	(replacements $fx>))
(declare-fixnum-unary/multi-comparison fx<=	(replacements $fx<=))
(declare-fixnum-unary/multi-comparison fx>=	(replacements $fx>=))

(declare-fixnum-unary/multi-comparison fxmax	(replacements $fxmax))
(declare-fixnum-unary/multi-comparison fxmin	(replacements $fxmin))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive fixnum->flonum
    (safe)
  (signatures
   ((<fixnum>)		=> (<flonum>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive fixnum->string
    (safe)
  (signatures
   ((<fixnum>)		=> (<string>))
   ((<fixnum> <fixnum>)	=> (<string>)))
  (attributes
   ((_)			foldable effect-free result-true)
   ((_ _)		foldable effect-free result-true)))

/section)


;;;; fixnums unsafe operations

(section

;;; predicates

(declare-fixnum-predicate $fxzero? unsafe)
(declare-fixnum-predicate $fxpositive? unsafe)
(declare-fixnum-predicate $fxnegative? unsafe)
(declare-fixnum-predicate $fxnonpositive? unsafe)
(declare-fixnum-predicate $fxnonnegative? unsafe)
(declare-fixnum-predicate $fxeven? unsafe)
(declare-fixnum-predicate $fxodd? unsafe)

;;; --------------------------------------------------------------------
;;; arithmetics

(declare-fixnum-binary $fx+ unsafe)
(declare-fixnum-unary/binary $fx- unsafe)
(declare-fixnum-binary $fx* unsafe)
(declare-fixnum-binary $fxdiv unsafe)
(declare-fixnum-binary $fxmod unsafe)
(declare-fixnum-binary $fxdiv0 unsafe)
(declare-fixnum-binary $fxmod0 unsafe)

(declare-core-primitive $fxdiv-and-mod
    (unsafe)
  (signatures
   ((<fixnum> <fixnum>)		=> (<fixnum> <fixnum>)))
  (attributes
   ((_ _)			effect-free)))

(declare-core-primitive $fxdiv0-and-mod0
    (unsafe)
  (signatures
   ((<fixnum> <fixnum>)		=> (<fixnum> <fixnum>)))
  (attributes
   ((_ _)			effect-free)))

(declare-fixnum-unary $fxadd1 unsafe)
(declare-fixnum-unary $fxsub1 unsafe)
(declare-fixnum-unary $fxabs unsafe)
(declare-fixnum-unary $fxsign unsafe)
(declare-fixnum-binary $fxremainder unsafe)
(declare-fixnum-binary $fxquotient unsafe)
(declare-fixnum-binary $fxmodulo unsafe)

(declare-fixnum-binary $int-quotient unsafe)
(declare-fixnum-binary $int-remainder unsafe)

;;; --------------------------------------------------------------------
;;; bitwise operations

(declare-fixnum-multi $fxlogor unsafe)
(declare-fixnum-multi $fxlogand unsafe)
(declare-fixnum-multi $fxlogxor unsafe)
(declare-fixnum-unary $fxnot unsafe)
(declare-fixnum-binary $fxsll unsafe)
(declare-fixnum-binary $fxsra unsafe)

(declare-core-primitive $fxcopy-bit
    (unsafe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

(declare-core-primitive $fxcopy-bit-field
    (unsafe)
  (signatures
   ((<fixnum> <fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _ _)					foldable effect-free result-true)))

(declare-core-primitive $fxrotate-bit-field
    (unsafe)
  (signatures
   ((<fixnum> <fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _ _)					foldable effect-free result-true)))

(declare-core-primitive $fxbit-field
    (unsafe)
  (signatures
   ((<fixnum> <fixnum> <fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _ _)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-fixnum-binary-comparison $fx= unsafe)
(declare-fixnum-binary-comparison $fx!= unsafe)
(declare-fixnum-binary-comparison $fx< unsafe)
(declare-fixnum-binary-comparison $fx> unsafe)
(declare-fixnum-binary-comparison $fx<= unsafe)
(declare-fixnum-binary-comparison $fx>= unsafe)

(declare-fixnum-binary-comparison $fxmax unsafe)
(declare-fixnum-binary-comparison $fxmin unsafe)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $fixnum->flonum
    (unsafe)
  (signatures
   ((<fixnum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $fixnum->char
    (unsafe)
  (signatures
   ((<fixnum>)			=> (<char>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $fixnum->string
    (unsafe)
  (signatures
   ((<fixnum>)			=> (<string>))
   ((<fixnum> <fixnum>)		=> (<string>)))
  (attributes
   ((_)				foldable effect-free result-true)
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $fxinthash
    (unsafe)
  (signatures
   ((<fixnum>)			=> (<fixnum>)))
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
