;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for numerics core primitives
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
(library (typed-core-primitives numerics)
  (export typed-core-primitives.numerics)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.numerics)


;;;; numerics, safe functions
;;
;;The order of  unsafe replacements *does* matter:  replacements accepting "<number>"
;;operands must come *after* the ones accepting more specific operand types.
;;

(section

;;; predicates

(declare-type-predicate number?			<number>)
(declare-type-predicate complex?		<number>)

(declare-core-primitive real?
    (safe)
  (signatures
   ((<real>)			=> (<true>))
   ((<compnum>)			=> (<false>))
   ((<cflonum>)			=> (<false>))
   ((<number>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive rational?
    (safe)
  (signatures
   ((<fixnum>)			=> (<true>))
   ((<bignum>)			=> (<true>))
   ((<ratnum>)			=> (<true>))
   ;; ((<flonum-finite>)		=> (<true>))
   ;; ((<flonum-infinite>)		=> (<false>))
   ;; ((<flonum-nan>)		=> (<false>))
   ((<compnum>)			=> (<false>))
   ((<cflonum>)			=> (<false>))
   ((<number>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-type-predicate integer?		<integer>)
(declare-type-predicate exact-integer?		<exact-integer>)

(declare-type-predicate real-valued?)
(declare-type-predicate rational-valued?)
(declare-type-predicate integer-valued?)

(declare-number-predicate odd?)
(declare-number-predicate even?)

;;;

(declare-core-primitive zero?
    (safe)
  (signatures
   ((<zero>)			=> (<true>))
   ((<positive>)		=> (<false>))
   ((<negative>)		=> (<false>))
   ((<non-zero-compnum>)	=> (<false>))
   ((<non-positive>)		=> (<boolean>))
   ((<non-negative>)		=> (<boolean>))
   ((<number>)			=> (<boolean>)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

(declare-core-primitive positive?
    (safe)
  (signatures
   ((<positive>)		=> (<true>))
   ((<negative>)		=> (<false>))
   ((<zero>)			=> (<false>))
   ((<non-positive>)		=> (<boolean>))
   ((<non-negative>)		=> (<boolean>))
   ((<real>)			=> (<boolean>)))
  (attributes
   ((0)				foldable effect-free result-false)
   ((_)				foldable effect-free)))

(declare-core-primitive negative?
    (safe)
  (signatures
   ((<negative>)		=> (<true>))
   ((<positive>)		=> (<false>))
   ((<zero>)			=> (<false>))
   ((<non-positive>)		=> (<boolean>))
   ((<non-negative>)		=> (<boolean>))
   ((<real>)			=> (<boolean>)))
  (attributes
   ((0)				foldable effect-free result-false)
   ((_)				foldable effect-free)))

(declare-core-primitive non-negative?
    (safe)
  (signatures
   ((<non-negative>)		=> (<true>))
   ((<positive>)		=> (<false>))
   ((<real>)			=> (<boolean>)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

(declare-core-primitive non-positive?
    (safe)
  (signatures
   ((<non-positive>)		=> (<true>))
   ((<negative>)		=> (<false>))
   ((<real>)			=> (<boolean>)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

;;;

(declare-core-primitive zero-exact-integer?
    (safe)
  (signatures
   ((<zero-fixnum>)	=> (<true>))
   ((<positive-fixnum>)	=> (<false>))
   ((<negative-fixnum>)	=> (<false>))
   ((<fixnum>)		=> (<boolean>))
   ((not <fixnum>)	=> (<false>))
   ((<number>)		=> (<boolean>))
   ((<top>)		=> (<boolean>)))
  (attributes
   ((0)			foldable effect-free result-true)
   ((_)			foldable effect-free)))

(declare-core-primitive positive-exact-integer?
    (safe)
  (signatures
   ((<positive-fixnum>)		=> (<true>))
   ((<positive-bignum>)		=> (<true>))
   ((<zero-fixnum>)		=> (<false>))
   ((<negative-fixnum>)		=> (<false>))
   ((<negative-bignum>)		=> (<false>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive negative-exact-integer?
    (safe)
  (signatures
   ((<negative-fixnum>)		=> (<true>))
   ((<negative-bignum>)		=> (<true>))
   ((<non-negative-fixnum>)	=> (<false>))
   ((<positive-bignum>)		=> (<false>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive non-negative-exact-integer?
    (safe)
  (signatures
   ((<zero-fixnum>)		=> (<true>))
   ((<positive-fixnum>)		=> (<true>))
   ((<positive-bignum>)		=> (<true>))
   ((<negative-fixnum>)		=> (<false>))
   ((<negative-bignum>)		=> (<false>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive non-positive-exact-integer?
    (safe)
  (signatures
   ((<zero-fixnum>)		=> (<true>))
   ((<negative-fixnum>)		=> (<true>))
   ((<negative-bignum>)		=> (<true>))
   ((<positive-fixnum>)		=> (<false>))
   ((<positive-bignum>)		=> (<false>))
   ((<top>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

;;;

(declare-core-primitive exact?
    (safe)
  (signatures
   ((<fixnum>)			=> (<true>))
   ((<bignum>)			=> (<true>))
   ((<ratnum>)			=> (<true>))
   ((<exact-compnum>)		=> (<true>))
   ((<flonum>)			=> (<false>))
   ((<cflonum>)			=> (<false>))
   ((<number>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive inexact?
    (safe)
  (signatures
   ((<fixnum>)			=> (<false>))
   ((<bignum>)			=> (<false>))
   ((<ratnum>)			=> (<false>))
   ((<exact-compnum>)		=> (<false>))
   ((<flonum>)			=> (<true>))
   ((<cflonum>)			=> (<true>))
   ((<number>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive exact-compnum?
    (safe)
  (signatures
   ((<exact-compnum>)		=> (<true>))
   ((<inexact-compnum>)		=> (<false>))
   ((<compnum>)			=> (<boolean>))))

(declare-core-primitive inexact-compnum?
    (safe)
  (signatures
   ((<inexact-compnum>)		=> (<true>))
   ((<exact-compnum>)		=> (<false>))
   ((<compnum>)			=> (<boolean>))))

(declare-core-primitive zero-compnum?
    (safe)
  (signatures
   ((<zero-compnum>)		=> (<true>))
   ((<inexact-compnum>)		=> (<boolean>))
   ((<exact-compnum>)		=> (<false>))
   ((<compnum>)			=> (<boolean>))))

(declare-core-primitive non-zero-compnum?
    (safe)
  (signatures
   ((<non-zero-compnum>)	=> (<true>))
   ((<zero-compnum>)		=> (<false>))
   ((<compnum>)			=> (<boolean>))
   ((<number>)			=> (<boolean>))))

(declare-core-primitive non-zero-inexact-compnum?
    (safe)
  (signatures
   ((<non-zero-inexact-compnum>)	=> (<true>))
   ((<zero-compnum>)			=> (<false>))
   ((<exact-compnum>)			=> (<false>))
   ((<compnum>)				=> (<boolean>))
   ((<number>)				=> (<boolean>))))

;;;

(declare-core-primitive zero-cflonum?
    (safe)
  (signatures
   ((<zero-cflonum>)		=> (<true>))
   ((<cflonum>)			=> (<boolean>))))

(declare-core-primitive non-zero-cflonum?
    (safe)
  (signatures
   ((<non-zero-cflonum>)	=> (<true>))
   ((<zero-cflonum>)		=> (<false>))
   ((<cflonum>)			=> (<boolean>))
   ((<number>)			=> (<boolean>))))

;;;

(declare-core-primitive finite?
    (safe)
  (signatures
   ;; ((<flonum-finite>)		=> (<true>))
   ;; ((<flonum-infinite>)		=> (<false>))
   ;; ((<flonum-nan>)		=> (<false>))
   ((<fixnum>)			=> (<true>))
   ((<bignum>)			=> (<true>))
   ((<ratnum>)			=> (<true>))
   ((<number>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive infinite?
    (safe)
  (signatures
   ;; ((<flonum-finite>)		=> (<false>))
   ;; ((<flonum-infinite>)		=> (<true>))
   ;; ((<flonum-nan>)		=> (<false>))
   ((<fixnum>)			=> (<false>))
   ((<bignum>)			=> (<false>))
   ((<ratnum>)			=> (<false>))
   ((<number>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive nan?
    (safe)
  (signatures
   ;; ((<flonum-nan>)		=> (<false>))
   ;; ((<flonum-infinite>)		=> (<false>))
   ;; ((<flonum-nan>)		=> (<true>))
   ((<fixnum>)			=> (<false>))
   ((<bignum>)			=> (<false>))
   ((<ratnum>)			=> (<false>))
   ((<number>)			=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-core-primitive =
    (safe)
  (signatures
   #;((<positive>     <non-positive> . (list-of <number>))		=> (<false>))
   #;((<non-positive> <positive>     . (list-of <number>))		=> (<false>))
   #;((<negative>     <non-negative> . (list-of <number>))		=> (<false>))
   #;((<non-negative> <negative>     . (list-of <number>))		=> (<false>))
   ((<number> . (list-of <number>))	=> (<boolean>)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive !=
    (safe)
  (signatures
   #;((<positive>     <non-positive> . (list-of <number>))		=> (<true>))
   #;((<non-positive> <positive>     . (list-of <number>))		=> (<true>))
   #;((<negative>     <non-negative> . (list-of <number>))		=> (<true>))
   #;((<non-negative> <negative>     . (list-of <number>))		=> (<true>))
   ((<number> . (list-of <number>))	=> (<boolean>)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive <
    (safe)
  (signatures
   #;((<positive>     <non-positive>)	=> (<false>))
   #;((<non-negative> <negative>)	=> (<false>))
   #;((<non-positive> <positive>)	=> (<true>))
   #;((<negative>     <non-negative>)	=> (<true>))
   ((<real> <real> . (list-of <real>))	=> (<boolean>)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive >
    (safe)
  (signatures
   #;((<positive>     <non-positive>)	=> (<true>))
   #;((<non-negative> <negative>)	=> (<true>))
   #;((<non-positive> <positive>)	=> (<false>))
   #;((<negative>     <non-negative>)	=> (<false>))
   ((<real> <real> . (list-of <real>))	=> (<boolean>)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive <=
    (safe)
  (signatures
   #;((<positive> <negative>)		=> (<false>))
   #;((<negative> <positive>)		=> (<true>))
   ((<real> <real> . (list-of <real>))	=> (<boolean>)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive >=
    (safe)
  (signatures
   #;((<positive> <negative>)		=> (<true>))
   #;((<negative> <positive>)		=> (<false>))
   ((<real> <real> . (list-of <real>))	=> (<boolean>)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

;;;

(declare-core-primitive max
    (safe)
  (signatures
   ((<fixnum>		. (list-of <fixnum>))		=> (<fixnum>))
   ((<bignum>		. (list-of <bignum>))		=> (<bignum>))
   ((<flonum>		. (list-of <flonum>))		=> (<flonum>))
   ((<exact-integer>	. (list-of <exact-integer>))	=> (<exact-integer>))
   ((<exact-rational>	. (list-of <exact-rational>))	=> (<exact-rational>))
   ((<integer>		. (list-of <integer>))		=> (<integer>))
   ((<real>		. (list-of <real>))		=> (<real>)))
  (attributes
   ((_ . _)				foldable effect-free result-true))
  #;(replacements
   $max-fixnum-fixnum $max-fixnum-bignum $max-fixnum-flonum $max-fixnum-ratnum
   $max-bignum-fixnum $max-bignum-bignum $max-bignum-flonum $max-bignum-ratnum
   $max-flonum-fixnum $max-flonum-bignum $max-flonum-flonum $max-flonum-ratnum
   $max-ratnum-fixnum $max-ratnum-bignum $max-ratnum-flonum $max-ratnum-ratnum
   $max-fixnum-number $max-bignum-number $max-flonum-number $max-ratnum-number
   $max-number-fixnum $max-number-bignum $max-number-flonum $max-number-ratnum))

(declare-core-primitive min
    (safe)
  (signatures
   ((<fixnum>		. (list-of <fixnum>))		=> (<fixnum>))
   ((<bignum>		. (list-of <bignum>))		=> (<bignum>))
   ((<flonum>		. (list-of <flonum>))		=> (<flonum>))
   ((<exact-integer>	. (list-of <exact-integer>))	=> (<exact-integer>))
   ((<exact-rational>	. (list-of <exact-rational>))	=> (<exact-rational>))
   ((<integer>		. (list-of <integer>))		=> (<integer>))
   ((<real>		. (list-of <real>))		=> (<real>)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  #;(replacements
   $min-fixnum-fixnum $min-fixnum-bignum $min-fixnum-flonum $min-fixnum-ratnum
   $min-bignum-fixnum $min-bignum-bignum $min-bignum-flonum $min-bignum-ratnum
   $min-flonum-fixnum $min-flonum-bignum $min-flonum-flonum $min-flonum-ratnum
   $min-ratnum-fixnum $min-ratnum-bignum $min-ratnum-flonum $min-ratnum-ratnum
   $min-fixnum-number $min-bignum-number $min-flonum-number $min-ratnum-number
   $min-number-fixnum $min-number-bignum $min-number-flonum $min-number-ratnum))

;;; --------------------------------------------------------------------
;;; arithmetics

(declare-core-primitive +
    (safe)
  (signatures
   ((list-of <flonum>)			=> (<flonum>))
   ((list-of <exact-integer>)		=> (<exact-integer>))
   ((list-of <exact-rational>)		=> (<exact-rational>))
   ((list-of <integer>)			=> (<integer>))
   ((list-of <real>)			=> (<real>))
   ((list-of <cflonum>)			=> (<cflonum>))
   ((list-of <number>)			=> (<number>)))
  (attributes
   (_			foldable effect-free result-true))
  (replacements
   $add-fixnum-fixnum		$add-fixnum-bignum	$add-fixnum-flonum
   $add-fixnum-ratnum		$add-fixnum-compnum	$add-fixnum-cflonum

   $add-bignum-fixnum		$add-bignum-bignum	$add-bignum-flonum
   $add-bignum-ratnum		$add-bignum-compnum	$add-bignum-cflonum

   $add-flonum-fixnum		$add-flonum-bignum	$add-flonum-flonum
   $add-flonum-ratnum		$add-flonum-compnum	$add-flonum-cflonum

   $add-ratnum-fixnum		$add-ratnum-bignum	$add-ratnum-flonum
   $add-ratnum-ratnum		$add-ratnum-compnum	$add-ratnum-cflonum

   $add-compnum-fixnum		$add-compnum-bignum	$add-compnum-ratnum
   $add-compnum-compnum		$add-compnum-flonum	$add-compnum-cflonum

   $add-cflonum-fixnum		$add-cflonum-bignum	$add-cflonum-ratnum
   $add-cflonum-flonum		$add-cflonum-compnum	$add-cflonum-cflonum

   $add-fixnum-number		$add-bignum-number	$add-flonum-number
   $add-ratnum-number		$add-compnum-number	$add-cflonum-number

   $add-number-fixnum		$add-number-bignum	$add-number-flonum
   $add-number-ratnum		$add-number-compnum	$add-number-cflonum

   $add-number-number))

(declare-core-primitive -
    (safe)
  (signatures
   ((<flonum>		. (list-of <flonum>))		=> (<flonum>))
   ((<exact-integer>	. (list-of <exact-integer>))	=> (<exact-integer>))
   ((<exact-rational>	. (list-of <exact-rational>))	=> (<exact-rational>))
   ((<integer>		. (list-of <integer>))		=> (<integer>))
   ((<real>		. (list-of <real>))		=> (<real>))
   ((<cflonum>		. (list-of <cflonum>))		=> (<cflonum>))
   ((<number>		. (list-of <number>))		=> (<number>)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  (replacements
   $sub-fixnum-fixnum		$sub-fixnum-bignum	$sub-fixnum-flonum
   $sub-fixnum-ratnum		$sub-fixnum-compnum	$sub-fixnum-cflonum

   $sub-bignum-fixnum		$sub-bignum-bignum	$sub-bignum-flonum
   $sub-bignum-ratnum		$sub-bignum-compnum	$sub-bignum-cflonum

   $sub-flonum-fixnum		$sub-flonum-bignum	$sub-flonum-flonum
   $sub-flonum-ratnum		$sub-flonum-compnum	$sub-flonum-cflonum

   $sub-ratnum-fixnum		$sub-ratnum-bignum	$sub-ratnum-flonum
   $sub-ratnum-ratnum		$sub-ratnum-compnum	$sub-ratnum-cflonum

   $sub-compnum-fixnum		$sub-compnum-bignum	$sub-compnum-ratnum
   $sub-compnum-compnum		$sub-compnum-flonum	$sub-compnum-cflonum

   $sub-cflonum-fixnum		$sub-cflonum-bignum	$sub-cflonum-ratnum
   $sub-cflonum-flonum		$sub-cflonum-compnum	$sub-cflonum-cflonum

   $sub-fixnum-number		$sub-bignum-number	$sub-flonum-number
   $sub-ratnum-number		$sub-compnum-number	$sub-cflonum-number

   $sub-number-fixnum		$sub-number-bignum	$sub-number-flonum
   $sub-number-ratnum		$sub-number-compnum	$sub-number-cflonum

   $sub-number-number))

(declare-core-primitive *
    (safe)
  (signatures
   ((list-of <flonum>)			=> (<flonum>))
   ((list-of <exact-integer>)		=> (<exact-integer>))
   ((list-of <exact-rational>)		=> (<exact-rational>))
   ((list-of <integer>)			=> (<integer>))
   ((list-of <real>)			=> (<real>))
   ((list-of <cflonum>)			=> (<cflonum>))
   ((list-of <number>)			=> (<number>)))
  (attributes
   (_			foldable effect-free result-true))
  (replacements
   $mul-fixnum-fixnum		$mul-fixnum-bignum	$mul-fixnum-flonum
   $mul-fixnum-ratnum		$mul-fixnum-compnum	$mul-fixnum-cflonum

   $mul-bignum-fixnum		$mul-bignum-bignum	$mul-bignum-flonum
   $mul-bignum-ratnum		$mul-bignum-compnum	$mul-bignum-cflonum

   $mul-flonum-fixnum		$mul-flonum-bignum	$mul-flonum-flonum
   $mul-flonum-ratnum		$mul-flonum-compnum	$mul-flonum-cflonum

   $mul-ratnum-fixnum		$mul-ratnum-bignum	$mul-ratnum-flonum
   $mul-ratnum-ratnum		$mul-ratnum-compnum	$mul-ratnum-cflonum

   $mul-compnum-fixnum		$mul-compnum-bignum	$mul-compnum-ratnum
   $mul-compnum-compnum		$mul-compnum-flonum	$mul-compnum-cflonum

   $mul-cflonum-fixnum		$mul-cflonum-bignum	$mul-cflonum-ratnum
   $mul-cflonum-flonum		$mul-cflonum-compnum	$mul-cflonum-cflonum

   $mul-fixnum-number		$mul-bignum-number	$mul-flonum-number
   $mul-ratnum-number		$mul-compnum-number	$mul-cflonum-number

   $mul-number-fixnum		$mul-number-bignum	$mul-number-flonum
   $mul-number-ratnum		$mul-number-compnum	$mul-number-cflonum

   $mul-number-number))

(declare-core-primitive /
    (safe)
  (signatures
   ((<flonum>		. (list-of <flonum>))		=> (<flonum>))
   ((<exact-rational>	. (list-of <exact-rational>))	=> (<exact-rational>))
   ((<real>		. (list-of <real>))		=> (<real>))
   ((<cflonum>		. (list-of <cflonum>))		=> (<cflonum>))
   ((<number>		. (list-of <number>))		=> (<number>)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  (replacements
   $div-fixnum-fixnum		$div-fixnum-bignum	$div-fixnum-flonum
   $div-fixnum-ratnum		$div-fixnum-compnum	$div-fixnum-cflonum

   $div-bignum-fixnum		$div-bignum-bignum	$div-bignum-flonum
   $div-bignum-ratnum		$div-bignum-compnum	$div-bignum-cflonum

   $div-flonum-fixnum		$div-flonum-bignum	$div-flonum-flonum
   $div-flonum-ratnum		$div-flonum-compnum	$div-flonum-cflonum

   $div-ratnum-fixnum		$div-ratnum-bignum	$div-ratnum-flonum
   $div-ratnum-ratnum		$div-ratnum-compnum	$div-ratnum-cflonum

   $div-compnum-fixnum		$div-compnum-bignum	$div-compnum-ratnum
   $div-compnum-compnum		$div-compnum-flonum	$div-compnum-cflonum

   $div-cflonum-fixnum		$div-cflonum-bignum	$div-cflonum-ratnum
   $div-cflonum-flonum		$div-cflonum-compnum	$div-cflonum-cflonum

   $div-fixnum-number		$div-bignum-number	$div-flonum-number
   $div-ratnum-number		$div-compnum-number	$div-cflonum-number

   $div-number-fixnum		$div-number-bignum	$div-number-flonum
   $div-number-ratnum		$div-number-compnum	$div-number-cflonum

   $div-number-number))

(declare-core-primitive add1
    (safe)
  (signatures
   ((<exact-integer>)		=> (<exact-integer>))
   ((<exact-rational>)		=> (<exact-rational>))
   ((<flonum>)			=> (<flonum>))
   ((<integer>)			=> (<integer>))
   ((<real>)			=> (<real>))
   ((<cflonum>)			=> (<cflonum>))
   ((<compnum>)			=> (<compnum>))
   ((<number>)			=> (<number>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $add1-fixnum
   $add1-bignum
   $add1-integer))

(declare-core-primitive sub1
    (safe)
  (signatures
   ((<exact-integer>)		=> (<exact-integer>))
   ((<exact-rational>)		=> (<exact-rational>))
   ((<flonum>)			=> (<flonum>))
   ((<integer>)			=> (<integer>))
   ((<real>)			=> (<real>))
   ((<cflonum>)			=> (<cflonum>))
   ((<compnum>)			=> (<compnum>))
   ((<number>)			=> (<number>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $sub1-fixnum
   $sub1-bignum
   $sub1-integer))

;;; --------------------------------------------------------------------

(declare-core-primitive div
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational>))
   ((<integer> <integer>)		=> (<integer>))
   ((<real> <real>)			=> (<real>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive mod
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational>))
   ((<integer> <integer>)		=> (<integer>))
   ((<real> <real>)			=> (<real>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive div0
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational>))
   ((<integer> <integer>)		=> (<integer>))
   ((<real> <real>)			=> (<real>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive mod0
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational>))
   ((<integer> <integer>)		=> (<integer>))
   ((<real> <real>)			=> (<real>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive modulo
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational>))
   ((<integer> <integer>)		=> (<integer>))
   ;;It accepts also <fixnum> that are integers.
   ((<real> <real>)			=> (<real>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remainder
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational>))
   ((<integer> <integer>)		=> (<integer>))
   ;;This is needed because integer flonums are valid operands.
   ((<real> <real>)			=> (<real>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive quotient
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<integer> <integer>)		=> (<integer>))
   ;;This is needed because integer flonums are valid operands.
   ((<real> <real>)			=> (<real>)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive quotient+remainder
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum> <fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer> <exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational> <exact-rational>))
   ((<integer> <integer>)		=> (<integer> <integer>))
   ;;This is needed because integer flonums are valid operands.
   ((<real> <real>)			=> (<real> <real>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive div-and-mod
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum> <fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer> <exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational> <exact-rational>))
   ((<real> <real>)			=> (<real> <real>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive div0-and-mod0
    (safe)
  (signatures
   ((<fixnum> <fixnum>)			=> (<fixnum> <fixnum>))
   ((<exact-integer> <exact-integer>)	=> (<exact-integer> <exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational> <exact-rational>))
   ((<real> <real>)			=> (<real> <real>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive factorial
    (safe)
  (signatures
   ((<zero-fixnum>)			=> (<positive-fixnum>)) ;it is 1
   ((<zero-flonum>)			=> (<positive-flonum>)) ;it is 1.0
   ((<positive-fixnum>)			=> (<positive-exact-integer>))
   ((<positive-bignum>)			=> (<positive-bignum>))
   ((<non-negative-exact-integer>)	=> (<positive-exact-integer>))))

;;;

(declare-core-primitive gcd
    (safe)
  (signatures
   (()					=> (<fixnum>))

   ((<fixnum>)				=> (<fixnum>))
   ((<bignum>)				=> (<bignum>))
   ((<flonum>)				=> (<flonum>))
   ((<exact-integer>)			=> (<exact-integer>))
   ((<real>)				=> (<real>))

   ((<fixnum> <fixnum>)			=> (<exact-integer>))
   ((<fixnum> <bignum>)			=> (<exact-integer>))
   ((<fixnum> <flonum>)			=> (<flonum>))

   ((<bignum> <fixnum>)			=> (<exact-integer>))
   ((<bignum> <bignum>)			=> (<exact-integer>))
   ((<bignum> <flonum>)			=> (<flonum>))

   ((<flonum> <fixnum>)			=> (<flonum>))
   ((<flonum> <bignum>)			=> (<flonum>))
   ((<flonum> <flonum>)			=> (<flonum>))

   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational>))
   ((<integer> <integer>)		=> (<integer>))
   ((<real> <real>)			=> (<real>))

   ((<real> <real> <real> . (list-of <real>))	=> (<real>)))
  (attributes
   (()					foldable effect-free result-true)
   ((_)					foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)
   ((_ _ _ . _)				foldable effect-free result-true))
  (replacements
   $gcd-fixnum-fixnum $gcd-fixnum-bignum $gcd-fixnum-flonum
   $gcd-bignum-fixnum $gcd-bignum-bignum $gcd-bignum-flonum
   $gcd-flonum-fixnum $gcd-flonum-bignum $gcd-flonum-flonum
   $gcd-fixnum-number $gcd-bignum-number $gcd-flonum-number
   $gcd-number-fixnum $gcd-number-bignum $gcd-number-flonum
   $gcd-number-number))

(declare-core-primitive lcm
    (safe)
  (signatures
   (()					=> (<fixnum>))

   ((<fixnum>)				=> (<fixnum>))
   ((<bignum>)				=> (<bignum>))
   ((<flonum>)				=> (<flonum>))
   ((<exact-integer>)			=> (<exact-integer>))
   ((<real>)				=> (<real>))

   ((<fixnum> <fixnum>)			=> (<exact-integer>))
   ((<fixnum> <bignum>)			=> (<exact-integer>))
   ((<fixnum> <flonum>)			=> (<flonum>))

   ((<bignum> <fixnum>)			=> (<exact-integer>))
   ((<bignum> <bignum>)			=> (<exact-integer>))
   ((<bignum> <flonum>)			=> (<flonum>))

   ((<flonum> <fixnum>)			=> (<flonum>))
   ((<flonum> <bignum>)			=> (<flonum>))
   ((<flonum> <flonum>)			=> (<flonum>))

   ((<exact-integer> <exact-integer>)	=> (<exact-integer>))
   ((<exact-rational> <exact-rational>)	=> (<exact-rational>))
   ((<integer> <integer>)		=> (<integer>))
   ((<real> <real>)			=> (<real>))

   ((<real> <real> <real> . (list-of <real>))	=> (<real>)))
  (attributes
   (()					foldable effect-free result-true)
   ((_)					foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)
   ((_ _ _ . _)				foldable effect-free result-true))
  (replacements
   $lcm-fixnum-fixnum $lcm-fixnum-bignum $lcm-fixnum-flonum
   $lcm-bignum-fixnum $lcm-bignum-bignum $lcm-bignum-flonum
   $lcm-flonum-fixnum $lcm-flonum-bignum $lcm-flonum-flonum
   $lcm-fixnum-number $lcm-bignum-number $lcm-flonum-number
   $lcm-number-fixnum $lcm-number-bignum $lcm-number-flonum
   $lcm-number-number))

;;; --------------------------------------------------------------------
;;; exactness

(declare-core-primitive exact
    (safe)
  (signatures
   ((<fixnum>)		=> (<fixnum>))
   ((<bignum>)		=> (<bignum>))
   ((<flonum>)		=> (<exact-integer>))
   ((<ratnum>)		=> (<ratnum>))
   ((<compnum>)		=> (<exact-compnum>))
   ((<cflonum>)		=> (<exact-compnum>))
   ((<real>)		=> (<exact-integer>))
   #;((<number>)	=> (<exact>))
   ((<number>)		=> (<number>)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $exact-fixnum $exact-bignum $exact-flonum $exact-ratnum $exact-compnum $exact-cflonum))

(declare-core-primitive inexact
    (safe)
  (signatures
   ((<fixnum>)		=> (<flonum>))
   ((<bignum>)		=> (<flonum>))
   ((<flonum>)		=> (<flonum>))
   ((<ratnum>)		=> (<flonum>))
   ((<compnum>)		=> (<cflonum>))
   ((<cflonum>)		=> (<cflonum>))
   ((<real>)		=> (<flonum>))
   #;((<number>)	=> (<inexact>))
   ((<number>)		=> (<number>)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $inexact-fixnum $inexact-bignum $inexact-flonum $inexact-ratnum $inexact-compnum $inexact-cflonum))

(declare-core-primitive inexact->exact
    (safe)
  (signatures
   ((<fixnum>)		=> (<fixnum>))
   ((<bignum>)		=> (<bignum>))
   ((<flonum>)		=> (<exact-integer>))
   ((<ratnum>)		=> (<ratnum>))
   ((<compnum>)		=> (<exact-compnum>))
   ((<cflonum>)		=> (<exact-compnum>))
   ((<real>)		=> (<exact-integer>))
   #;((<number>)	=> (<exact>))
   ((<number>)		=> (<number>)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $exact-fixnum $exact-bignum $exact-flonum $exact-ratnum $exact-compnum $exact-cflonum))

(declare-core-primitive exact->inexact
    (safe)
  (signatures
   ((<fixnum>)		=> (<flonum>))
   ((<bignum>)		=> (<flonum>))
   ((<flonum>)		=> (<flonum>))
   ((<ratnum>)		=> (<flonum>))
   ((<compnum>)		=> (<cflonum>))
   ((<cflonum>)		=> (<cflonum>))
   ((<real>)		=> (<flonum>))
   #;((<number>)	=> (<inexact>))
   ((<number>)		=> (<number>)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $inexact-fixnum $inexact-bignum $inexact-flonum $inexact-ratnum $inexact-compnum $inexact-cflonum))

;;; --------------------------------------------------------------------
;;; parts

(declare-core-primitive numerator
    (safe)
  (signatures
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<bignum>))
   ((<flonum>)			=> (<flonum>))
   ((<ratnum>)			=> (<exact-integer>))
   ((<real>)			=> (<real>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $numerator-fixnum $numerator-bignum $numerator-flonum $numerator-ratnum))

(declare-core-primitive denominator
    (safe)
  (signatures
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<bignum>))
   ((<flonum>)			=> (<flonum>))
   ((<ratnum>)			=> (<exact-integer>))
   ((<real>)			=> (<real>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $denominator-fixnum $denominator-bignum $denominator-flonum $denominator-ratnum))

(declare-core-primitive rationalize
    (safe)
  (signatures
   ((<real> <real>)		=> (<real>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive make-rectangular
    (safe)
  (signatures
   ((<exact-rational> <exact-rational>)	=> (<exact-compnum>))
   ((<real> <real>)			=> (<complex>)))
  (attributes
   ((_ _)			foldable effect-free result-true))
  (replacements $make-cflonum $make-compnum $make-rectangular))

(declare-core-primitive make-polar
    (safe)
  (signatures
   ((<real> <real>)		=> (<complex>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-number-unary real-part		(replacements $compnum-real $cflonum-real))
(declare-number-unary imag-part		(replacements $compnum-imag $cflonum-imag))

(declare-core-primitive abs
    (safe)
  (signatures
   ((<fixnum>)			=> (<exact-integer>))
   ((<bignum>)			=> (<exact-integer>))
   ((<ratnum>)			=> (<ratnum>))
   ((<flonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $abs-fixnum $abs-bignum $abs-flonum $abs-ratnum))

(declare-core-primitive sign
    (safe)
  (signatures
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<fixnum>))
   ((<ratnum>)			=> (<fixnum>))
   ((<flonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $sign-fixnum $sign-bignum $sign-flonum $sign-ratnum))

(declare-core-primitive angle
    (safe)
  (signatures
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<fixnum>))
   ((<flonum>)			=> (<flonum>))
   ((<ratnum>)			=> (<fixnum>))
   ((<compnum>)			=> (<real>))
   ((<cflonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $angle-fixnum $angle-bignum $angle-ratnum $angle-flonum $angle-compnum $angle-cflonum))

(declare-core-primitive magnitude
    (safe)
  (signatures
   ((<fixnum>)			=> (<exact-integer>))
   ((<bignum>)			=> (<exact-integer>))
   ((<ratnum>)			=> (<ratnum>))
   ((<flonum>)			=> (<flonum>))
   ((<compnum>)			=> (<real>))
   ((<cflonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $magnitude-fixnum $magnitude-bignum $magnitude-ratnum $magnitude-flonum $magnitude-compnum $magnitude-cflonum))

(declare-core-primitive complex-conjugate
    (safe)
  (signatures
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<bignum>))
   ((<ratnum>)			=> (<ratnum>))
   ((<flonum>)			=> (<flonum>))
   ((<compnum>)			=> (<compnum>))
   ((<cflonum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $complex-conjugate-compnum $complex-conjugate-cflonum))

;;; --------------------------------------------------------------------
;;; rounding

(let-syntax
    ((declare-safe-rounding-primitive
      (syntax-rules ()
	((_ ?who . ?replacements)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((<fixnum>)			=> (<fixnum>))
	    ((<bignum>)			=> (<bignum>))
	    ((<flonum>)			=> (<flonum>))
	    ((<ratnum>)			=> (<ratnum>))
	    ((<real>)			=> (<real>)))
	   (attributes
	    ((_)			foldable effect-free result-true))
	   (replacements . ?replacements))))))
  (declare-safe-rounding-primitive floor	   $floor-fixnum    $floor-bignum    $floor-ratnum    $floor-flonum)
  (declare-safe-rounding-primitive ceiling	 $ceiling-fixnum  $ceiling-bignum  $ceiling-ratnum  $ceiling-flonum)
  (declare-safe-rounding-primitive truncate	$truncate-fixnum $truncate-bignum $truncate-ratnum $truncate-flonum)
  (declare-safe-rounding-primitive round	   $round-fixnum    $round-bignum    $round-ratnum    $round-flonum)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; exponentiation, exponentials and logarithms

(declare-number-binary expt
  (replacements
   $expt-fixnum-negative-fixnum $expt-bignum-negative-fixnum $expt-flonum-negative-fixnum $expt-ratnum-negative-fixnum
   $expt-compnum-negative-fixnum $expt-cflonum-negative-fixnum
   $expt-fixnum-positive-fixnum $expt-bignum-positive-fixnum $expt-flonum-positive-fixnum $expt-ratnum-positive-fixnum
   $expt-compnum-positive-fixnum $expt-cflonum-positive-fixnum
   $expt-fixnum-fixnum $expt-bignum-fixnum $expt-flonum-fixnum $expt-ratnum-fixnum $expt-compnum-fixnum $expt-cflonum-fixnum
   $expt-fixnum-bignum $expt-bignum-bignum $expt-flonum-bignum $expt-ratnum-bignum $expt-compnum-bignum $expt-cflonum-bignum
   $expt-fixnum-flonum $expt-bignum-flonum $expt-flonum-flonum $expt-ratnum-flonum $expt-compnum-flonum $expt-cflonum-flonum
   $expt-fixnum-ratnum $expt-bignum-ratnum $expt-flonum-ratnum $expt-ratnum-ratnum $expt-compnum-ratnum $expt-cflonum-ratnum
   $expt-fixnum-cflonum $expt-bignum-cflonum $expt-flonum-cflonum $expt-ratnum-cflonum $expt-compnum-cflonum $expt-cflonum-cflonum
   $expt-fixnum-compnum $expt-bignum-compnum $expt-flonum-compnum $expt-ratnum-compnum $expt-compnum-compnum $expt-cflonum-compnum
   $expt-number-negative-fixnum $expt-number-positive-fixnum
   $expt-number-fixnum $expt-number-bignum $expt-number-flonum $expt-number-ratnum $expt-number-compnum $expt-number-cflonum))

(declare-core-primitive square
    (safe)
  (signatures
   ((<fixnum>)			=> (<exact-integer>))
   ((<bignum>)			=> (<exact-integer>))
   ((<flonum>)			=> (<flonum>))
   ((<ratnum>)			=> (<exact-rational>))
   ((<compnum>)			=> (<number>))
   ((<cflonum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $square-fixnum $square-bignum $flsquare $square-ratnum $square-compnum $square-cflonum))

(declare-core-primitive cube
    (safe)
  (signatures
   ((<fixnum>)			=> (<exact-integer>))
   ((<bignum>)			=> (<exact-integer>))
   ((<flonum>)			=> (<flonum>))
   ((<ratnum>)		=> (<exact-rational>))
   ((<compnum>)			=> (<number>))
   ((<cflonum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $cube-fixnum $cube-bignum $flcube $cube-ratnum $cube-compnum $cube-cflonum))

(declare-core-primitive sqrt
    (safe)
  (signatures
   ((<fixnum>)			=> (<number>))
   ((<bignum>)			=> (<number>))
   ((<flonum>)			=> (<inexact>))
   ((<ratnum>)			=> (<number>))
   ((<compnum>)			=> (<number>))
   ((<cflonum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $sqrt-fixnum $sqrt-bignum $sqrt-flonum $sqrt-ratnum $sqrt-compnum $sqrt-cflonum))

(declare-core-primitive cbrt
    (safe)
  (signatures
   ((<fixnum>)			=> (<number>))
   ((<bignum>)			=> (<number>))
   ((<flonum>)			=> (<inexact>))
   ((<ratnum>)			=> (<number>))
   ((<compnum>)			=> (<number>))
   ((<cflonum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $cbrt-fixnum $cbrt-bignum $cbrt-flonum $cbrt-ratnum $cbrt-compnum $cbrt-cflonum))

(declare-core-primitive exact-integer-sqrt
    (safe)
  (signatures
   ((<fixnum>)			=> (<positive-fixnum> <positive-fixnum>))
   ((<bignum>)			=> (<exact-integer> <exact-integer>))
   ((<exact-integer>)		=> (<exact-integer> <exact-integer>)))
  (attributes
   ((_)				effect-free))
  (replacements $exact-integer-sqrt-fixnum $exact-integer-sqrt-bignum))

(declare-core-primitive exp
    (safe)
  (signatures
   ((<fixnum>)			=> (<real>))
   ((<bignum>)			=> (<flonum>))
   ((<flonum>)			=> (<flonum>))
   ((<ratnum>)			=> (<flonum>))
   ((<compnum>)			=> (<cflonum>))
   ((<cflonum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $exp-fixnum $exp-bignum $exp-flonum $exp-ratnum $exp-compnum $exp-cflonum))

(declare-core-primitive log
    (safe)
  (signatures
   ((<fixnum>)			=> (<number>))
   ((<bignum>)			=> (<inexact>))
   ((<flonum>)			=> (<inexact>))
   ((<ratnum>)			=> (<inexact>))
   ((<compnum>)			=> (<inexact>))
   ((<cflonum>)			=> (<cflonum>))
   ((<number> <number>)		=> (<number>)))
  (attributes
   ((_)				foldable effect-free result-true)
   ((_ _)			foldable effect-free result-true))
  (replacements $log-fixnum $log-bignum $log-flonum $log-ratnum $log-compnum $log-cflonum))

;;; --------------------------------------------------------------------
;;; bitwise

(declare-core-primitive bitwise-and
    (safe)
  (signatures
   (()				=> (<fixnum>))
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<bignum>))
   ((<fixnum> <fixnum>)		=> (<fixnum>))
   ((<bignum> <bignum>)		=> (<bignum>))
   ((<fixnum> <bignum>)		=> (<bignum>))
   ((<bignum> <fixnum>)		=> (<bignum>))
   ((list-of <exact-integer>)	=> (<exact-integer>)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-and-fixnum-fixnum $bitwise-and-fixnum-bignum
   $bitwise-and-bignum-fixnum $bitwise-and-bignum-bignum
   $bitwise-and-fixnum-number $bitwise-and-bignum-number))

(declare-core-primitive bitwise-ior
    (safe)
  (signatures
   (()				=> (<fixnum>))
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<bignum>))
   ((<fixnum> <fixnum>)		=> (<fixnum>))
   ((<bignum> <bignum>)		=> (<bignum>))
   ((<fixnum> <bignum>)		=> (<bignum>))
   ((<bignum> <fixnum>)		=> (<bignum>))
   ((list-of <exact-integer>)	=> (<exact-integer>)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-ior-fixnum-fixnum $bitwise-ior-fixnum-bignum
   $bitwise-ior-bignum-fixnum $bitwise-ior-bignum-bignum
   $bitwise-ior-fixnum-number $bitwise-ior-bignum-number))

(declare-core-primitive bitwise-xor
    (safe)
  (signatures
   (()				=> (<fixnum>))
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<bignum>))
   ((<fixnum> <fixnum>)		=> (<fixnum>))
   ((<bignum> <bignum>)		=> (<bignum>))
   ((<fixnum> <bignum>)		=> (<bignum>))
   ((<bignum> <fixnum>)		=> (<bignum>))
   ((list-of <exact-integer>)	=> (<exact-integer>)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-xor-fixnum-fixnum $bitwise-xor-fixnum-bignum
   $bitwise-xor-bignum-fixnum $bitwise-xor-bignum-bignum
   $bitwise-xor-fixnum-number $bitwise-xor-bignum-number))

(declare-core-primitive bitwise-not
    (safe)
  (signatures
   ((<fixnum>)			=> (<fixnum>))
   ((<bignum>)			=> (<bignum>)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $bitwise-not-fixnum $bitwise-not-bignum))

(declare-core-primitive bitwise-arithmetic-shift
    (safe)
  (signatures
   ((<exact-integer> <fixnum>)	=> (<exact-integer>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-arithmetic-shift-left
    (safe)
  (signatures
   ((<exact-integer> <fixnum>)	=> (<exact-integer>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-arithmetic-shift-right
    (safe)
  (signatures
   ((<exact-integer> <fixnum>)	=> (<exact-integer>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-count
    (safe)
  (signatures
   ((<exact-integer>)		=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-field
    (safe)
  (signatures
   ((<exact-integer> <exact-integer> <exact-integer>)	=> (<exact-integer>)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-set?
    (safe)
  (signatures
   ((<exact-integer> <exact-integer>)	=> (<boolean>)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive bitwise-copy-bit
    (safe)
  (signatures
   ((<exact-integer> <exact-integer> <exact-integer>)	=> (<exact-integer>)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-copy-bit-field
    (safe)
  (signatures
   ((<exact-integer> <exact-integer> <exact-integer> <exact-integer>)	=> (<exact-integer>)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-first-bit-set
    (safe)
  (signatures
   ((<exact-integer>)	=> (<exact-integer>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bitwise-if
    (safe)
  (signatures
   ((<exact-integer> <exact-integer> <exact-integer>)	=> (<exact-integer>)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-length
    (safe)
  (signatures
   ((<exact-integer>)	=> (<exact-integer>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bitwise-reverse-bit-field
    (safe)
  (signatures
   ((<exact-integer> <exact-integer> <exact-integer>)	=> (<exact-integer>)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-rotate-bit-field
    (safe)
  (signatures
   ((<exact-integer> <exact-integer> <exact-integer> <exact-integer>)	=> (<exact-integer>)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive sll
    (safe)
  (signatures
   ((<fixnum> <fixnum>)		=> (<exact-integer>))
   ((<bignum> <fixnum>)		=> (<exact-integer>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive sra
    (safe)
  (signatures
   ((<fixnum> <fixnum>)		=> (<exact-integer>))
   ((<bignum> <fixnum>)		=> (<exact-integer>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; trigonometric

(declare-number-unary sin	(replacements $sin-fixnum $sin-bignum $sin-flonum $sin-ratnum $sin-compnum $sin-cflonum))
(declare-number-unary cos	(replacements $cos-fixnum $cos-bignum $cos-flonum $cos-ratnum $cos-compnum $cos-cflonum))
(declare-number-unary tan	(replacements $tan-fixnum $tan-bignum $tan-flonum $tan-ratnum $tan-compnum $tan-cflonum))
(declare-number-unary asin	(replacements $asin-fixnum $asin-bignum $asin-flonum $asin-ratnum $asin-compnum $asin-cflonum))
(declare-number-unary acos	(replacements $acos-fixnum $acos-bignum $acos-flonum $acos-ratnum $acos-compnum $acos-cflonum))
(declare-number-unary/binary atan (replacements $atan-fixnum $atan-bignum $atan-flonum $atan-ratnum $atan-compnum $atan-cflonum))

;;; --------------------------------------------------------------------
;;; hyperbolic

(declare-number-unary sinh	(replacements $sinh-fixnum $sinh-bignum $sinh-flonum $sinh-ratnum $sinh-compnum $sinh-cflonum))
(declare-number-unary cosh	(replacements $cosh-fixnum $cosh-bignum $cosh-flonum $cosh-ratnum $cosh-compnum $cosh-cflonum))
(declare-number-unary tanh	(replacements $tanh-fixnum $tanh-bignum $tanh-flonum $tanh-ratnum $tanh-compnum $tanh-cflonum))
(declare-number-unary asinh	(replacements $asinh-fixnum $asinh-bignum $asinh-flonum $asinh-ratnum $asinh-compnum $asinh-cflonum))
(declare-number-unary acosh	(replacements $acosh-fixnum $acosh-bignum $acosh-flonum $acosh-ratnum $acosh-compnum $acosh-cflonum))
(declare-number-unary atanh	(replacements $atanh-fixnum $atanh-bignum $atanh-flonum $atanh-ratnum $atanh-compnum $atanh-cflonum))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive integer->char
    (safe)
  (signatures
   ((<non-negative-fixnum>)		=> (<char>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive number->string
    (safe)
  (signatures
   ((<number>)						=> (<string>))
   ((<number> <positive-fixnum>)			=> (<string>))
   ((<number> <positive-fixnum> <positive-fixnum>)	=> (<string>)))
  (attributes
   ((_)			foldable effect-free result-true)
   ((_ _)		foldable effect-free result-true)
   ((_ _ _)		foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; shortcut's interrupt handlers

(declare-core-primitive error@fx+
    (safe)
  (signatures
   ((<top> <top>)		=> (<void>))))

(declare-core-primitive error@fx*
    (safe)
  (signatures
   ((<top> <top>)		=> (<void>))))

(declare-core-primitive error@fxadd1
    (safe)
  (signatures
   ((<top>)			=> (<void>))))

(declare-core-primitive error@fxsub1
    (safe)
  (signatures
   ((<top>)			=> (<void>))))

(declare-core-primitive error@fxarithmetic-shift-left
    (safe)
  (signatures
   ((<top> <top>)		=> (<void>))))

(declare-core-primitive error@fxarithmetic-shift-right
    (safe)
  (signatures
   ((<top> <top>)		=> (<void>))))

;;;

(declare-core-primitive error@add1
    (safe)
  (signatures
   ((<fixnum>)			=> (<exact-integer>))
   ((<bignum>)			=> (<exact-integer>))
   ((<real>)			=> (<real>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive error@sub1
    (safe)
  (signatures
   ((<fixnum>)			=> (<exact-integer>))
   ((<bignum>)			=> (<exact-integer>))
   ((<real>)			=> (<real>)))
  (attributes
   ((_)				foldable effect-free result-true)))

/section)


;;;; numerics, unsafe functions

(section

(declare-core-primitive $make-rectangular
    (unsafe)
  (signatures
   ((<real> <real>)		=> (<complex>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $neg-number	<number>	<number>)
(declare-unsafe-unary-operation $neg-fixnum	<fixnum>	<exact-integer>)
(declare-unsafe-unary-operation $neg-bignum	<bignum>	<exact-integer>)
(declare-unsafe-unary-operation $neg-flonum	<flonum>	<flonum>)
(declare-unsafe-unary-operation $neg-ratnum	<ratnum>	<ratnum>)
(declare-unsafe-unary-operation $neg-compnum	<compnum>	<compnum>)
(declare-unsafe-unary-operation $neg-cflonum	<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $inv-number	<number>	<number>)
(declare-unsafe-unary-operation $inv-fixnum	<fixnum>	<exact-rational>)
(declare-unsafe-unary-operation $inv-bignum	<bignum>	<ratnum>)
(declare-unsafe-unary-operation $inv-flonum	<flonum>	<flonum>)
(declare-unsafe-unary-operation $inv-ratnum	<ratnum>	<exact-rational>)
(declare-unsafe-unary-operation $inv-compnum	<compnum>	<complex>)
(declare-unsafe-unary-operation $inv-cflonum	<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $add1-integer	<exact-integer>	<exact-integer>)

(declare-core-primitive $add1-fixnum
    (unsafe)
  (signatures
   ((<negative-fixnum>)		=> (<fixnum>))
   ((<fixnum>)			=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $add1-bignum
    (unsafe)
  (signatures
   ((<positive-bignum>)		=> (<bignum>))
   ((<bignum>)			=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sub1-integer	<exact-integer>	<exact-integer>)

(declare-core-primitive $sub1-fixnum
    (unsafe)
  (signatures
   ((<non-negative-fixnum>)	=> (<fixnum>))
   ((<fixnum>)			=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $sub1-bignum
    (unsafe)
  (signatures
   ((<negative-bignum>)		=> (<bignum>))
   ((<bignum>)			=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $add-number-number	<number>	<number>	<number>)

(declare-unsafe-binary-operation $add-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $add-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $add-flonum-number	<flonum>	<number>	<number>)
(declare-unsafe-binary-operation $add-ratnum-number	<ratnum>	<number>	<number>)
(declare-unsafe-binary-operation $add-compnum-number	<compnum>	<number>	<number>)
(declare-unsafe-binary-operation $add-cflonum-number	<cflonum>	<number>	<number>)

(declare-unsafe-binary-operation $add-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $add-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $add-number-flonum	<number>	<flonum>	<number>)
(declare-unsafe-binary-operation $add-number-ratnum	<number>	<ratnum>	<number>)
(declare-unsafe-binary-operation $add-number-compnum	<number>	<compnum>	<number>)
(declare-unsafe-binary-operation $add-number-cflonum	<number>	<cflonum>	<number>)

(declare-unsafe-binary-operation $add-fixnum-fixnum	<fixnum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $add-fixnum-bignum	<fixnum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $add-fixnum-flonum	<fixnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $add-fixnum-ratnum	<fixnum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $add-fixnum-compnum	<fixnum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $add-fixnum-cflonum	<fixnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $add-bignum-fixnum	<bignum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $add-bignum-bignum	<bignum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $add-bignum-flonum	<bignum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $add-bignum-ratnum	<bignum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $add-bignum-compnum	<bignum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $add-bignum-cflonum	<bignum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $add-flonum-fixnum	<flonum>	<fixnum>	<flonum>)
(declare-unsafe-binary-operation $add-flonum-bignum	<flonum>	<bignum>	<flonum>)
(declare-unsafe-binary-operation $add-flonum-flonum	<flonum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $add-flonum-ratnum	<flonum>	<ratnum>	<flonum>)
(declare-unsafe-binary-operation $add-flonum-compnum	<flonum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $add-flonum-cflonum	<flonum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $add-ratnum-fixnum	<ratnum>	<fixnum>	<exact-rational>)
(declare-unsafe-binary-operation $add-ratnum-bignum	<ratnum>	<bignum>	<exact-rational>)
(declare-unsafe-binary-operation $add-ratnum-flonum	<ratnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $add-ratnum-ratnum	<ratnum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $add-ratnum-compnum	<ratnum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $add-ratnum-cflonum	<ratnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $add-compnum-fixnum	<compnum>	<fixnum>	<compnum>)
(declare-unsafe-binary-operation $add-compnum-bignum	<compnum>	<bignum>	<compnum>)
(declare-unsafe-binary-operation $add-compnum-flonum	<compnum>	<flonum>	<complex>)
(declare-unsafe-binary-operation $add-compnum-ratnum	<compnum>	<ratnum>	<compnum>)
(declare-unsafe-binary-operation $add-compnum-compnum	<compnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $add-compnum-cflonum	<compnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $add-cflonum-fixnum	<cflonum>	<fixnum>	<cflonum>)
(declare-unsafe-binary-operation $add-cflonum-bignum	<cflonum>	<bignum>	<cflonum>)
(declare-unsafe-binary-operation $add-cflonum-ratnum	<cflonum>	<flonum>	<cflonum>)
(declare-unsafe-binary-operation $add-cflonum-flonum	<cflonum>	<ratnum>	<cflonum>)
(declare-unsafe-binary-operation $add-cflonum-compnum	<cflonum>	<compnum>	<cflonum>)
(declare-unsafe-binary-operation $add-cflonum-cflonum	<cflonum>	<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $sub-number-number	<number>	<number>	<number>)

(declare-unsafe-binary-operation $sub-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $sub-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $sub-flonum-number	<flonum>	<number>	<number>)
(declare-unsafe-binary-operation $sub-ratnum-number	<ratnum>	<number>	<number>)
(declare-unsafe-binary-operation $sub-compnum-number	<compnum>	<number>	<number>)
(declare-unsafe-binary-operation $sub-cflonum-number	<cflonum>	<number>	<number>)

(declare-unsafe-binary-operation $sub-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $sub-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $sub-number-flonum	<number>	<flonum>	<number>)
(declare-unsafe-binary-operation $sub-number-ratnum	<number>	<ratnum>	<number>)
(declare-unsafe-binary-operation $sub-number-compnum	<number>	<compnum>	<number>)
(declare-unsafe-binary-operation $sub-number-cflonum	<number>	<cflonum>	<number>)

(declare-unsafe-binary-operation $sub-fixnum-fixnum	<fixnum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $sub-fixnum-bignum	<fixnum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $sub-fixnum-flonum	<fixnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $sub-fixnum-ratnum	<fixnum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $sub-fixnum-compnum	<fixnum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $sub-fixnum-cflonum	<fixnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $sub-bignum-fixnum	<bignum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $sub-bignum-bignum	<bignum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $sub-bignum-flonum	<bignum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $sub-bignum-ratnum	<bignum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $sub-bignum-compnum	<bignum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $sub-bignum-cflonum	<bignum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $sub-flonum-fixnum	<flonum>	<fixnum>	<flonum>)
(declare-unsafe-binary-operation $sub-flonum-bignum	<flonum>	<bignum>	<flonum>)
(declare-unsafe-binary-operation $sub-flonum-flonum	<flonum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $sub-flonum-ratnum	<flonum>	<ratnum>	<flonum>)
(declare-unsafe-binary-operation $sub-flonum-compnum	<flonum>	<compnum>	<complex>)
(declare-unsafe-binary-operation $sub-flonum-cflonum	<flonum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $sub-ratnum-fixnum	<ratnum>	<fixnum>	<exact-rational>)
(declare-unsafe-binary-operation $sub-ratnum-bignum	<ratnum>	<bignum>	<exact-rational>)
(declare-unsafe-binary-operation $sub-ratnum-flonum	<ratnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $sub-ratnum-ratnum	<ratnum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $sub-ratnum-compnum	<ratnum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $sub-ratnum-cflonum	<ratnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $sub-compnum-fixnum	<compnum>	<fixnum>	<compnum>)
(declare-unsafe-binary-operation $sub-compnum-bignum	<compnum>	<bignum>	<compnum>)
(declare-unsafe-binary-operation $sub-compnum-flonum	<compnum>	<flonum>	<complex>)
(declare-unsafe-binary-operation $sub-compnum-ratnum	<compnum>	<ratnum>	<compnum>)
(declare-unsafe-binary-operation $sub-compnum-compnum	<compnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $sub-compnum-cflonum	<compnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $sub-cflonum-fixnum	<cflonum>	<fixnum>	<cflonum>)
(declare-unsafe-binary-operation $sub-cflonum-bignum	<cflonum>	<bignum>	<cflonum>)
(declare-unsafe-binary-operation $sub-cflonum-flonum	<cflonum>	<flonum>	<cflonum>)
(declare-unsafe-binary-operation $sub-cflonum-ratnum	<cflonum>	<ratnum>	<cflonum>)
(declare-unsafe-binary-operation $sub-cflonum-compnum	<cflonum>	<compnum>	<cflonum>)
(declare-unsafe-binary-operation $sub-cflonum-cflonum	<cflonum>	<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $mul-number-number	<number>	<number>	<number>)

(declare-unsafe-binary-operation $mul-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $mul-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $mul-flonum-number	<flonum>	<number>	<number>)
(declare-unsafe-binary-operation $mul-ratnum-number	<ratnum>	<number>	<number>)
(declare-unsafe-binary-operation $mul-compnum-number	<compnum>	<number>	<number>)
(declare-unsafe-binary-operation $mul-cflonum-number	<cflonum>	<number>	<cflonum>)

(declare-unsafe-binary-operation $mul-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $mul-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $mul-number-flonum	<number>	<flonum>	<number>)
(declare-unsafe-binary-operation $mul-number-ratnum	<number>	<ratnum>	<number>)
(declare-unsafe-binary-operation $mul-number-compnum	<number>	<compnum>	<number>)
(declare-unsafe-binary-operation $mul-number-cflonum	<number>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $mul-fixnum-fixnum	<fixnum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $mul-fixnum-bignum	<fixnum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $mul-fixnum-flonum	<fixnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $mul-fixnum-ratnum	<fixnum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $mul-fixnum-compnum	<fixnum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $mul-fixnum-cflonum	<fixnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $mul-bignum-fixnum	<bignum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $mul-bignum-bignum	<bignum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $mul-bignum-flonum	<bignum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $mul-bignum-ratnum	<bignum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $mul-bignum-compnum	<bignum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $mul-bignum-cflonum	<bignum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $mul-flonum-fixnum	<flonum>	<fixnum>	<flonum>)
(declare-unsafe-binary-operation $mul-flonum-bignum	<flonum>	<bignum>	<flonum>)
(declare-unsafe-binary-operation $mul-flonum-flonum	<flonum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $mul-flonum-ratnum	<flonum>	<ratnum>	<flonum>)
(declare-unsafe-binary-operation $mul-flonum-compnum	<flonum>	<compnum>	<cflonum>)
(declare-unsafe-binary-operation $mul-flonum-cflonum	<flonum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $mul-ratnum-fixnum	<ratnum>	<fixnum>	<exact-rational>)
(declare-unsafe-binary-operation $mul-ratnum-bignum	<ratnum>	<bignum>	<exact-rational>)
(declare-unsafe-binary-operation $mul-ratnum-flonum	<ratnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $mul-ratnum-ratnum	<ratnum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $mul-ratnum-compnum	<ratnum>	<compnum>	<compnum>)
(declare-unsafe-binary-operation $mul-ratnum-cflonum	<ratnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $mul-compnum-fixnum	<compnum>	<fixnum>	<compnum>)
(declare-unsafe-binary-operation $mul-compnum-bignum	<compnum>	<bignum>	<compnum>)
(declare-unsafe-binary-operation $mul-compnum-flonum	<compnum>	<flonum>	<cflonum>)
(declare-unsafe-binary-operation $mul-compnum-ratnum	<compnum>	<ratnum>	<compnum>)
(declare-unsafe-binary-operation $mul-compnum-compnum	<compnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $mul-compnum-cflonum	<compnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $mul-cflonum-fixnum	<cflonum>	<fixnum>	<cflonum>)
(declare-unsafe-binary-operation $mul-cflonum-bignum	<cflonum>	<bignum>	<cflonum>)
(declare-unsafe-binary-operation $mul-cflonum-flonum	<cflonum>	<flonum>	<cflonum>)
(declare-unsafe-binary-operation $mul-cflonum-ratnum	<cflonum>	<ratnum>	<cflonum>)
(declare-unsafe-binary-operation $mul-cflonum-compnum	<cflonum>	<compnum>	<cflonum>)
(declare-unsafe-binary-operation $mul-cflonum-cflonum	<cflonum>	<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $div-number-number	<number>	<number>	<number>)

(declare-unsafe-binary-operation $div-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $div-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $div-flonum-number	<flonum>	<number>	<number>)
(declare-unsafe-binary-operation $div-ratnum-number	<ratnum>	<number>	<number>)
(declare-unsafe-binary-operation $div-compnum-number	<compnum>	<number>	<number>)
(declare-unsafe-binary-operation $div-cflonum-number	<cflonum>	<number>	<number>)

(declare-unsafe-binary-operation $div-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $div-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $div-number-flonum	<number>	<flonum>	<number>)
(declare-unsafe-binary-operation $div-number-ratnum	<number>	<ratnum>	<number>)
(declare-unsafe-binary-operation $div-number-compnum	<number>	<compnum>	<number>)
(declare-unsafe-binary-operation $div-number-cflonum	<number>	<cflonum>	<number>)

(declare-unsafe-binary-operation $div-fixnum-fixnum	<fixnum>	<fixnum>	<exact-rational>)
(declare-unsafe-binary-operation $div-fixnum-bignum	<fixnum>	<bignum>	<exact-rational>)
(declare-unsafe-binary-operation $div-fixnum-flonum	<fixnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $div-fixnum-ratnum	<fixnum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $div-fixnum-compnum	<fixnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $div-fixnum-cflonum	<fixnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $div-bignum-fixnum	<bignum>	<fixnum>	<exact-rational>)
(declare-unsafe-binary-operation $div-bignum-bignum	<bignum>	<bignum>	<exact-rational>)
(declare-unsafe-binary-operation $div-bignum-flonum	<bignum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $div-bignum-ratnum	<bignum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $div-bignum-compnum	<bignum>	<compnum>	<number>)
(declare-unsafe-binary-operation $div-bignum-cflonum	<bignum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $div-ratnum-fixnum	<ratnum>	<fixnum>	<exact-rational>)
(declare-unsafe-binary-operation $div-ratnum-bignum	<ratnum>	<bignum>	<exact-rational>)
(declare-unsafe-binary-operation $div-ratnum-flonum	<ratnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $div-ratnum-ratnum	<ratnum>	<ratnum>	<exact-rational>)
(declare-unsafe-binary-operation $div-ratnum-compnum	<ratnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $div-ratnum-cflonum	<ratnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $div-flonum-fixnum	<flonum>	<fixnum>	<flonum>)
(declare-unsafe-binary-operation $div-flonum-bignum	<flonum>	<bignum>	<flonum>)
(declare-unsafe-binary-operation $div-flonum-flonum	<flonum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $div-flonum-ratnum	<flonum>	<ratnum>	<flonum>)
(declare-unsafe-binary-operation $div-flonum-compnum	<flonum>	<compnum>	<cflonum>)
(declare-unsafe-binary-operation $div-flonum-cflonum	<flonum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $div-compnum-fixnum	<compnum>	<fixnum>	<number>)
(declare-unsafe-binary-operation $div-compnum-bignum	<compnum>	<bignum>	<number>)
(declare-unsafe-binary-operation $div-compnum-flonum	<compnum>	<flonum>	<cflonum>)
(declare-unsafe-binary-operation $div-compnum-ratnum	<compnum>	<ratnum>	<number>)
(declare-unsafe-binary-operation $div-compnum-compnum	<compnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $div-compnum-cflonum	<compnum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $div-cflonum-fixnum	<cflonum>	<fixnum>	<cflonum>)
(declare-unsafe-binary-operation $div-cflonum-bignum	<cflonum>	<bignum>	<cflonum>)
(declare-unsafe-binary-operation $div-cflonum-flonum	<cflonum>	<flonum>	<cflonum>)
(declare-unsafe-binary-operation $div-cflonum-ratnum	<cflonum>	<ratnum>	<cflonum>)
(declare-unsafe-binary-operation $div-cflonum-compnum	<cflonum>	<compnum>	<cflonum>)
(declare-unsafe-binary-operation $div-cflonum-cflonum	<cflonum>	<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $square-fixnum		<fixnum>	<exact-integer>)
(declare-unsafe-unary-operation $square-bignum		<bignum>	<exact-integer>)
(declare-unsafe-unary-operation $square-ratnum		<ratnum>	<exact-rational>)
(declare-unsafe-unary-operation $square-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $square-cflonum		<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $cube-fixnum		<fixnum>	<exact-integer>)
(declare-unsafe-unary-operation $cube-bignum		<bignum>	<exact-integer>)
(declare-unsafe-unary-operation $cube-ratnum		<ratnum>	<exact-rational>)
(declare-unsafe-unary-operation $cube-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $cube-cflonum		<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $gcd-number		<number>	<number>)

(declare-unsafe-binary-operation $gcd-number-number	<number>	<number>	<number>)

(declare-unsafe-binary-operation $gcd-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $gcd-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $gcd-flonum-number	<flonum>	<number>	<number>)

(declare-unsafe-binary-operation $gcd-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $gcd-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $gcd-number-flonum	<number>	<flonum>	<number>)

(declare-unsafe-binary-operation $gcd-fixnum-fixnum	<fixnum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $gcd-fixnum-bignum	<fixnum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $gcd-fixnum-flonum	<fixnum>	<flonum>	<flonum>)

(declare-unsafe-binary-operation $gcd-bignum-fixnum	<bignum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $gcd-bignum-bignum	<bignum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $gcd-bignum-flonum	<bignum>	<flonum>	<flonum>)

(declare-unsafe-binary-operation $gcd-flonum-fixnum	<flonum>	<fixnum>	<flonum>)
(declare-unsafe-binary-operation $gcd-flonum-bignum	<flonum>	<bignum>	<flonum>)
(declare-unsafe-binary-operation $gcd-flonum-flonum	<flonum>	<flonum>	<flonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $lcm-number		<number>	<number>)

(declare-unsafe-binary-operation $lcm-number-number	<number>	<number>	<number>)

(declare-unsafe-binary-operation $lcm-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $lcm-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $lcm-flonum-number	<flonum>	<number>	<number>)

(declare-unsafe-binary-operation $lcm-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $lcm-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $lcm-number-flonum	<number>	<flonum>	<number>)

(declare-unsafe-binary-operation $lcm-fixnum-fixnum	<fixnum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $lcm-fixnum-bignum	<fixnum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $lcm-fixnum-flonum	<fixnum>	<flonum>	<flonum>)

(declare-unsafe-binary-operation $lcm-bignum-fixnum	<bignum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $lcm-bignum-bignum	<bignum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $lcm-bignum-flonum	<bignum>	<flonum>	<flonum>)

(declare-unsafe-binary-operation $lcm-flonum-fixnum	<flonum>	<fixnum>	<flonum>)
(declare-unsafe-binary-operation $lcm-flonum-bignum	<flonum>	<bignum>	<flonum>)
(declare-unsafe-binary-operation $lcm-flonum-flonum	<flonum>	<flonum>	<flonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-number	<fixnum>	<number>      <number>	      <number>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-number	<bignum>	<number>      <number>	      <number>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-number	<flonum>	<number>      <number>	      <number>)

(declare-unsafe-binary-operation/2rv $quotient+remainder-number-fixnum	<number>	<fixnum>      <number>	      <number>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-number-bignum	<number>	<bignum>      <number>	      <number>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-number-flonum	<number>	<flonum>      <number>	      <number>)

(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-fixnum	<fixnum>	<fixnum>      <exact-integer> <exact-integer>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-bignum	<fixnum>	<bignum>      <exact-integer> <exact-integer>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-flonum	<fixnum>	<flonum>      <flonum>	      <flonum>)

(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-fixnum	<bignum>	<fixnum>      <exact-integer> <exact-integer>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-bignum	<bignum>	<bignum>      <exact-integer> <exact-integer>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-flonum	<bignum>	<flonum>      <flonum>	      <flonum>)

(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-fixnum	<flonum>	<fixnum>      <flonum>	      <flonum>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-bignum	<flonum>	<bignum>      <flonum>	      <flonum>)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-flonum	<flonum>	<flonum>      <flonum>	      <flonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $quotient-fixnum-number	<fixnum>	<number>      <number>)
(declare-unsafe-binary-operation $quotient-bignum-number	<bignum>	<number>      <number>)
(declare-unsafe-binary-operation $quotient-flonum-number	<flonum>	<number>      <number>)

(declare-unsafe-binary-operation $quotient-number-fixnum	<number>	<fixnum>      <number>)
(declare-unsafe-binary-operation $quotient-number-bignum	<number>	<bignum>      <number>)
(declare-unsafe-binary-operation $quotient-number-flonum	<number>	<flonum>      <number>)

(declare-unsafe-binary-operation $quotient-fixnum-fixnum	<fixnum>	<fixnum>      <exact-integer>)
(declare-unsafe-binary-operation $quotient-fixnum-bignum	<fixnum>	<bignum>      <exact-integer>)
(declare-unsafe-binary-operation $quotient-fixnum-flonum	<fixnum>	<flonum>      <flonum>)

(declare-unsafe-binary-operation $quotient-bignum-fixnum	<bignum>	<fixnum>      <exact-integer>)
(declare-unsafe-binary-operation $quotient-bignum-bignum	<bignum>	<bignum>      <exact-integer>)
(declare-unsafe-binary-operation $quotient-bignum-flonum	<bignum>	<flonum>      <flonum>)

(declare-unsafe-binary-operation $quotient-flonum-fixnum	<flonum>	<fixnum>      <flonum>)
(declare-unsafe-binary-operation $quotient-flonum-bignum	<flonum>	<bignum>      <flonum>)
(declare-unsafe-binary-operation $quotient-flonum-flonum	<flonum>	<flonum>      <flonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $remainder-fixnum-number	<fixnum>	<number>      <number>)
(declare-unsafe-binary-operation $remainder-bignum-number	<bignum>	<number>      <number>)
(declare-unsafe-binary-operation $remainder-flonum-number	<flonum>	<number>      <number>)

(declare-unsafe-binary-operation $remainder-number-fixnum	<number>	<fixnum>      <number>)
(declare-unsafe-binary-operation $remainder-number-bignum	<number>	<bignum>      <number>)
(declare-unsafe-binary-operation $remainder-number-flonum	<number>	<flonum>      <number>)

(declare-unsafe-binary-operation $remainder-fixnum-fixnum	<fixnum>	<fixnum>      <exact-integer>)
(declare-unsafe-binary-operation $remainder-fixnum-bignum	<fixnum>	<bignum>      <exact-integer>)
(declare-unsafe-binary-operation $remainder-fixnum-flonum	<fixnum>	<flonum>      <flonum>)

(declare-unsafe-binary-operation $remainder-bignum-fixnum	<bignum>	<fixnum>      <exact-integer>)
(declare-unsafe-binary-operation $remainder-bignum-bignum	<bignum>	<bignum>      <exact-integer>)
(declare-unsafe-binary-operation $remainder-bignum-flonum	<bignum>	<flonum>      <flonum>)

(declare-unsafe-binary-operation $remainder-flonum-fixnum	<flonum>	<fixnum>      <flonum>)
(declare-unsafe-binary-operation $remainder-flonum-bignum	<flonum>	<bignum>      <flonum>)
(declare-unsafe-binary-operation $remainder-flonum-flonum	<flonum>	<flonum>      <flonum>)

;;; --------------------------------------------------------------------

(declare-core-primitive $numerator-fixnum
    (safe)
  (signatures
   ((<fixnum>)			=> (<fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $numerator-bignum
    (safe)
  (signatures
   ((<bignum>)			=> (<bignum>)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $numerator-flonum
    (safe)
  (signatures
   ((<flonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $numerator-ratnum
    (safe)
  (signatures
   ((<ratnum>)			=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-fixnum
    (safe)
  (signatures
   ((<fixnum>)			=> (<positive-fixnum>)))
  (attributes
   ;;This always returns 1.
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-bignum
    (safe)
  (signatures
   ((<bignum>)			=> (<positive-bignum>)))
  (attributes
   ;;This always returns 1.
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-flonum
    (safe)
  (signatures
   ((<flonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-ratnum
    (safe)
  (signatures
   ((<ratnum>)			=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $modulo-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $modulo-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $modulo-flonum-number	<flonum>	<number>	<number>)

(declare-unsafe-binary-operation $modulo-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $modulo-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $modulo-number-flonum	<number>	<flonum>	<number>)

(declare-unsafe-binary-operation $modulo-fixnum-fixnum	<fixnum>	<fixnum>	<fixnum>)
(declare-unsafe-binary-operation $modulo-fixnum-bignum	<fixnum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $modulo-fixnum-flonum	<fixnum>	<flonum>	<flonum>)

(declare-unsafe-binary-operation $modulo-bignum-fixnum	<bignum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $modulo-bignum-bignum	<bignum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $modulo-bignum-flonum	<bignum>	<flonum>	<flonum>)

(declare-unsafe-binary-operation $modulo-flonum-fixnum	<flonum>	<fixnum>	<flonum>)
(declare-unsafe-binary-operation $modulo-flonum-bignum	<flonum>	<bignum>	<flonum>)
(declare-unsafe-binary-operation $modulo-flonum-flonum	<flonum>	<flonum>	<flonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $max-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $max-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $max-flonum-number	<flonum>	<number>	<number>)
(declare-unsafe-binary-operation $max-ratnum-number	<ratnum>	<number>	<number>)

(declare-unsafe-binary-operation $max-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $max-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $max-number-flonum	<number>	<flonum>	<number>)
(declare-unsafe-binary-operation $max-number-ratnum	<number>	<ratnum>	<number>)

(declare-unsafe-binary-operation $max-fixnum-fixnum	<fixnum>	<fixnum>	<fixnum>)
(declare-unsafe-binary-operation $max-fixnum-bignum	<fixnum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $max-fixnum-flonum	<fixnum>	<flonum>	<real>)
(declare-unsafe-binary-operation $max-fixnum-ratnum	<fixnum>	<ratnum>	<exact-rational>)

(declare-unsafe-binary-operation $max-bignum-fixnum	<bignum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $max-bignum-bignum	<bignum>	<bignum>	<bignum>)
(declare-unsafe-binary-operation $max-bignum-flonum	<bignum>	<flonum>	<real>)
(declare-unsafe-binary-operation $max-bignum-ratnum	<bignum>	<ratnum>	<exact-rational>)

(declare-unsafe-binary-operation $max-flonum-fixnum	<flonum>	<fixnum>	<real>)
(declare-unsafe-binary-operation $max-flonum-bignum	<flonum>	<bignum>	<real>)
(declare-unsafe-binary-operation $max-flonum-flonum	<flonum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $max-flonum-ratnum	<flonum>	<ratnum>	<real>)

(declare-unsafe-binary-operation $max-ratnum-fixnum	<ratnum>	<fixnum>	<exact-rational>)
(declare-unsafe-binary-operation $max-ratnum-bignum	<ratnum>	<bignum>	<exact-rational>)
(declare-unsafe-binary-operation $max-ratnum-flonum	<ratnum>	<flonum>	<real>)
(declare-unsafe-binary-operation $max-ratnum-ratnum	<ratnum>	<ratnum>	<ratnum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $min-fixnum-number	<fixnum>	<number>	<number>)
(declare-unsafe-binary-operation $min-bignum-number	<bignum>	<number>	<number>)
(declare-unsafe-binary-operation $min-flonum-number	<flonum>	<number>	<number>)
(declare-unsafe-binary-operation $min-ratnum-number	<ratnum>	<number>	<number>)

(declare-unsafe-binary-operation $min-number-fixnum	<number>	<fixnum>	<number>)
(declare-unsafe-binary-operation $min-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $min-number-flonum	<number>	<flonum>	<number>)
(declare-unsafe-binary-operation $min-number-ratnum	<number>	<ratnum>	<number>)

(declare-unsafe-binary-operation $min-fixnum-fixnum	<fixnum>	<fixnum>	<fixnum>)
(declare-unsafe-binary-operation $min-fixnum-bignum	<fixnum>	<bignum>	<exact-integer>)
(declare-unsafe-binary-operation $min-fixnum-flonum	<fixnum>	<flonum>	<real>)
(declare-unsafe-binary-operation $min-fixnum-ratnum	<fixnum>	<ratnum>	<exact-rational>)

(declare-unsafe-binary-operation $min-bignum-fixnum	<bignum>	<fixnum>	<exact-integer>)
(declare-unsafe-binary-operation $min-bignum-bignum	<bignum>	<bignum>	<bignum>)
(declare-unsafe-binary-operation $min-bignum-flonum	<bignum>	<flonum>	<real>)
(declare-unsafe-binary-operation $min-bignum-ratnum	<bignum>	<ratnum>	<exact-rational>)

(declare-unsafe-binary-operation $min-flonum-fixnum	<flonum>	<fixnum>	<real>)
(declare-unsafe-binary-operation $min-flonum-bignum	<flonum>	<bignum>	<real>)
(declare-unsafe-binary-operation $min-flonum-flonum	<flonum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $min-flonum-ratnum	<flonum>	<ratnum>	<real>)

(declare-unsafe-binary-operation $min-ratnum-fixnum	<ratnum>	<fixnum>	<exact-rational>)
(declare-unsafe-binary-operation $min-ratnum-bignum	<ratnum>	<bignum>	<exact-rational>)
(declare-unsafe-binary-operation $min-ratnum-flonum	<ratnum>	<flonum>	<real>)
(declare-unsafe-binary-operation $min-ratnum-ratnum	<ratnum>	<ratnum>	<ratnum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $abs-fixnum		<fixnum>	<exact-integer>)
(declare-unsafe-unary-operation $abs-bignum		<bignum>	<exact-integer>)
(declare-unsafe-unary-operation $abs-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $abs-ratnum		<ratnum>	<ratnum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sign-fixnum		<fixnum>	<fixnum>)
(declare-unsafe-unary-operation $sign-bignum		<bignum>	<fixnum>)
(declare-unsafe-unary-operation $sign-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $sign-ratnum		<ratnum>	<fixnum>)

;;; --------------------------------------------------------------------

(declare-core-primitive $exact-fixnum
    (unsafe)
  (signatures
   ((<positive-fixnum>)		=> (<positive-fixnum>))
   ((<zero-fixnum>)		=> (<zero-fixnum>))
   ((<negative-fixnum>)		=> (<negative-fixnum>))
   ((<fixnum>)			=> (<fixnum>)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-bignum
    (unsafe)
  (signatures
   ((<positive-bignum>)		=> (<positive-bignum>))
   ((<negative-bignum>)		=> (<negative-bignum>))
   ((<bignum>)			=> (<bignum>)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-flonum
    (unsafe)
  (signatures
   ((<flonum>)			=> (<exact-integer>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $exact-ratnum
    (unsafe)
  (signatures
   ((<ratnum>)			=> (<ratnum>)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-compnum
    (unsafe)
  (signatures
   ((<compnum>)			=> (<exact-compnum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $exact-cflonum
    (unsafe)
  (signatures
   ((<cflonum>)			=> (<exact-compnum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;;

(declare-core-primitive $inexact-fixnum
    (unsafe)
  (signatures
   ((<positive-fixnum>)		=> (<positive-flonum>))
   ((<zero-fixnum>)		=> (<zero-flonum>))
   ((<negative-fixnum>)		=> (<negative-flonum>))
   ((<fixnum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-bignum
    (unsafe)
  (signatures
   ((<positive-bignum>)		=> (<positive-flonum>))
   ((<negative-bignum>)		=> (<negative-flonum>))
   ((<bignum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-flonum
    (unsafe)
  (signatures
   ((<flonum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $inexact-ratnum
    (unsafe)
  (signatures
   ((<ratnum>)			=> (<flonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-compnum
    (unsafe)
  (signatures
   ((<compnum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-cflonum
    (unsafe)
  (signatures
   ((<cflonum>)			=> (<cflonum>)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $expt-number-fixnum	<number>	<number>	<number>)

(declare-unsafe-unary-operation $expt-number-zero-fixnum	<number>	<number>)
(declare-unsafe-unary-operation $expt-fixnum-zero-fixnum	<fixnum>	<fixnum>)
(declare-unsafe-unary-operation $expt-flonum-zero-fixnum	<flonum>	<flonum>)
(declare-unsafe-unary-operation $expt-compnum-zero-fixnum	<compnum>	<number>)
(declare-unsafe-unary-operation $expt-cflonum-zero-fixnum	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $expt-number-negative-fixnum	<number>	<negative-fixnum>      <number>)
(declare-unsafe-binary-operation $expt-fixnum-negative-fixnum	<fixnum>	<negative-fixnum>      <exact>)
(declare-unsafe-binary-operation $expt-bignum-negative-fixnum	<bignum>	<negative-fixnum>      <exact>)
(declare-unsafe-binary-operation $expt-flonum-negative-fixnum	<flonum>	<negative-fixnum>      <flonum>)
(declare-unsafe-binary-operation $expt-ratnum-negative-fixnum	<ratnum>	<negative-fixnum>      <exact>)
(declare-unsafe-binary-operation $expt-compnum-negative-fixnum	<compnum>	<negative-fixnum>      <number>)
(declare-unsafe-binary-operation $expt-cflonum-negative-fixnum	<cflonum>	<negative-fixnum>      <cflonum>)

(declare-unsafe-binary-operation $expt-number-positive-fixnum	<number>	<positive-fixnum>      <number>)
(declare-unsafe-binary-operation $expt-fixnum-positive-fixnum	<number>	<positive-fixnum>      <exact>)
(declare-unsafe-binary-operation $expt-bignum-positive-fixnum	<number>	<positive-fixnum>      <exact>)
(declare-unsafe-binary-operation $expt-flonum-positive-fixnum	<number>	<positive-fixnum>      <flonum>)
(declare-unsafe-binary-operation $expt-ratnum-positive-fixnum	<number>	<positive-fixnum>      <exact>)
(declare-unsafe-binary-operation $expt-compnum-positive-fixnum	<number>	<positive-fixnum>      <number>)
(declare-unsafe-binary-operation $expt-cflonum-positive-fixnum	<number>	<positive-fixnum>      <cflonum>)

(declare-unsafe-binary-operation $expt-fixnum-fixnum	<fixnum>	<fixnum>	<exact>)
(declare-unsafe-binary-operation $expt-bignum-fixnum	<bignum>	<fixnum>	<exact>)
(declare-unsafe-binary-operation $expt-flonum-fixnum	<flonum>	<fixnum>	<flonum>)
(declare-unsafe-binary-operation $expt-ratnum-fixnum	<ratnum>	<fixnum>	<exact>)
(declare-unsafe-binary-operation $expt-compnum-fixnum	<compnum>	<fixnum>	<number>)
(declare-unsafe-binary-operation $expt-cflonum-fixnum	<cflonum>	<fixnum>	<cflonum>)

;;;

(declare-unsafe-binary-operation $expt-number-bignum	<number>	<bignum>	<number>)
(declare-unsafe-binary-operation $expt-number-flonum	<number>	<flonum>	<number>)
(declare-unsafe-binary-operation $expt-number-ratnum	<number>	<ratnum>	<number>)
(declare-unsafe-binary-operation $expt-number-compnum	<number>	<compnum>	<number>)
(declare-unsafe-binary-operation $expt-number-cflonum	<number>	<cflonum>	<number>)

(declare-unsafe-binary-operation $expt-fixnum-bignum	<fixnum>	<bignum>	<exact>)
(declare-unsafe-binary-operation $expt-bignum-bignum	<bignum>	<bignum>	<exact>)
(declare-unsafe-binary-operation $expt-flonum-bignum	<flonum>	<bignum>	<flonum>)
(declare-unsafe-binary-operation $expt-ratnum-bignum	<ratnum>	<bignum>	<exact>)
(declare-unsafe-binary-operation $expt-compnum-bignum	<compnum>	<bignum>	<number>)
(declare-unsafe-binary-operation $expt-cflonum-bignum	<cflonum>	<bignum>	<cflonum>)

(declare-unsafe-binary-operation $expt-fixnum-flonum	<fixnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $expt-bignum-flonum	<bignum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $expt-flonum-flonum	<flonum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $expt-ratnum-flonum	<ratnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $expt-compnum-flonum	<compnum>	<flonum>	<flonum>)
(declare-unsafe-binary-operation $expt-cflonum-flonum	<cflonum>	<flonum>	<flonum>)

(declare-unsafe-binary-operation $expt-fixnum-ratnum	<fixnum>	<ratnum>	<exact>)
(declare-unsafe-binary-operation $expt-bignum-ratnum	<bignum>	<ratnum>	<exact>)
(declare-unsafe-binary-operation $expt-flonum-ratnum	<flonum>	<ratnum>	<flonum>)
(declare-unsafe-binary-operation $expt-ratnum-ratnum	<ratnum>	<ratnum>	<exact>)
(declare-unsafe-binary-operation $expt-compnum-ratnum	<compnum>	<ratnum>	<number>)
(declare-unsafe-binary-operation $expt-cflonum-ratnum	<cflonum>	<ratnum>	<cflonum>)

(declare-unsafe-binary-operation $expt-fixnum-cflonum	<fixnum>	<cflonum>	<cflonum>)
(declare-unsafe-binary-operation $expt-bignum-cflonum	<bignum>	<cflonum>	<cflonum>)
(declare-unsafe-binary-operation $expt-flonum-cflonum	<flonum>	<cflonum>	<cflonum>)
(declare-unsafe-binary-operation $expt-ratnum-cflonum	<ratnum>	<cflonum>	<cflonum>)
(declare-unsafe-binary-operation $expt-compnum-cflonum	<compnum>	<cflonum>	<cflonum>)
(declare-unsafe-binary-operation $expt-cflonum-cflonum	<cflonum>	<cflonum>	<cflonum>)

(declare-unsafe-binary-operation $expt-fixnum-compnum	<fixnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $expt-bignum-compnum	<bignum>	<compnum>	<number>)
(declare-unsafe-binary-operation $expt-flonum-compnum	<flonum>	<compnum>	<number>)
(declare-unsafe-binary-operation $expt-ratnum-compnum	<ratnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $expt-compnum-compnum	<compnum>	<compnum>	<number>)
(declare-unsafe-binary-operation $expt-cflonum-compnum	<cflonum>	<compnum>	<number>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sqrt-fixnum		<fixnum>	<number>)
(declare-unsafe-unary-operation $sqrt-bignum		<bignum>	<number>)
(declare-unsafe-unary-operation $sqrt-flonum		<flonum>	<inexact>)
(declare-unsafe-unary-operation $sqrt-ratnum		<ratnum>	<number>)
(declare-unsafe-unary-operation $sqrt-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $sqrt-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation/2rv $exact-integer-sqrt-fixnum	<fixnum>	<exact-integer>	      <exact-integer>)
(declare-unsafe-unary-operation/2rv $exact-integer-sqrt-bignum	<bignum>	<exact-integer>	      <exact-integer>)

(declare-unsafe-unary-operation $cbrt-fixnum		<fixnum>	<number>)
(declare-unsafe-unary-operation $cbrt-bignum		<bignum>	<number>)
(declare-unsafe-unary-operation $cbrt-flonum		<flonum>	<inexact>)
(declare-unsafe-unary-operation $cbrt-ratnum		<ratnum>	<number>)
(declare-unsafe-unary-operation $cbrt-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $cbrt-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $log-fixnum		<fixnum>	<number>)
(declare-unsafe-unary-operation $log-bignum		<bignum>	<inexact>)
(declare-unsafe-unary-operation $log-flonum		<flonum>	<inexact>)
(declare-unsafe-unary-operation $log-ratnum		<ratnum>	<inexact>)
(declare-unsafe-unary-operation $log-compnum		<compnum>	<inexact>)
(declare-unsafe-unary-operation $log-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $exp-fixnum		<fixnum>	<real>)
(declare-unsafe-unary-operation $exp-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $exp-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $exp-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $exp-compnum		<compnum>	<cflonum>)
(declare-unsafe-unary-operation $exp-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $sin-fixnum		<fixnum>	<real>)
(declare-unsafe-unary-operation $sin-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $sin-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $sin-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $sin-compnum		<compnum>	<cflonum>)
(declare-unsafe-unary-operation $sin-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $cos-fixnum		<fixnum>	<real>)
(declare-unsafe-unary-operation $cos-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $cos-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $cos-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $cos-compnum		<compnum>	<cflonum>)
(declare-unsafe-unary-operation $cos-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $tan-fixnum		<fixnum>	<real>)
(declare-unsafe-unary-operation $tan-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $tan-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $tan-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $tan-compnum		<compnum>	<cflonum>)
(declare-unsafe-unary-operation $tan-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $asin-fixnum		<fixnum>	<inexact>)
(declare-unsafe-unary-operation $asin-bignum		<bignum>	<inexact>)
(declare-unsafe-unary-operation $asin-flonum		<flonum>	<inexact>)
(declare-unsafe-unary-operation $asin-ratnum		<ratnum>	<inexact>)
(declare-unsafe-unary-operation $asin-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $asin-cflonum		<cflonum>	<inexact>)

(declare-unsafe-unary-operation $acos-fixnum		<fixnum>	<inexact>)
(declare-unsafe-unary-operation $acos-bignum		<bignum>	<inexact>)
(declare-unsafe-unary-operation $acos-flonum		<flonum>	<inexact>)
(declare-unsafe-unary-operation $acos-ratnum		<ratnum>	<inexact>)
(declare-unsafe-unary-operation $acos-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $acos-cflonum		<cflonum>	<inexact>)

(declare-unsafe-binary-operation $atan2-real-real	<real>		<real>	<flonum>)

(declare-unsafe-unary-operation $atan-fixnum		<fixnum>	<inexact>)
(declare-unsafe-unary-operation $atan-bignum		<bignum>	<inexact>)
(declare-unsafe-unary-operation $atan-flonum		<flonum>	<inexact>)
(declare-unsafe-unary-operation $atan-ratnum		<ratnum>	<inexact>)
(declare-unsafe-unary-operation $atan-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $atan-cflonum		<cflonum>	<inexact>)

(declare-unsafe-unary-operation $sinh-fixnum		<fixnum>	<flonum>)
(declare-unsafe-unary-operation $sinh-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $sinh-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $sinh-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $sinh-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $sinh-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $cosh-fixnum		<fixnum>	<flonum>)
(declare-unsafe-unary-operation $cosh-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $cosh-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $cosh-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $cosh-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $cosh-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $tanh-fixnum		<fixnum>	<flonum>)
(declare-unsafe-unary-operation $tanh-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $tanh-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $tanh-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $tanh-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $tanh-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $asinh-fixnum		<fixnum>	<flonum>)
(declare-unsafe-unary-operation $asinh-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $asinh-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $asinh-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $asinh-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $asinh-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $acosh-fixnum		<fixnum>	<flonum>)
(declare-unsafe-unary-operation $acosh-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $acosh-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $acosh-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $acosh-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $acosh-cflonum		<cflonum>	<cflonum>)

(declare-unsafe-unary-operation $atanh-fixnum		<fixnum>	<flonum>)
(declare-unsafe-unary-operation $atanh-bignum		<bignum>	<flonum>)
(declare-unsafe-unary-operation $atanh-flonum		<flonum>	<flonum>)
(declare-unsafe-unary-operation $atanh-ratnum		<ratnum>	<flonum>)
(declare-unsafe-unary-operation $atanh-compnum		<compnum>	<number>)
(declare-unsafe-unary-operation $atanh-cflonum		<cflonum>	<cflonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $complex-conjugate-cflonum	<cflonum>	<cflonum>)
(declare-unsafe-unary-operation $complex-conjugate-compnum	<compnum>	<compnum>)

(declare-unsafe-unary-operation $magnitude-fixnum		<fixnum>	<non-negative-fixnum>)
(declare-unsafe-unary-operation $magnitude-bignum		<bignum>	<non-negative-exact-integer>)
(declare-unsafe-unary-operation $magnitude-flonum		<flonum>	<positive-flonum>)
(declare-unsafe-unary-operation $magnitude-ratnum		<ratnum>	<positive-ratnum>)
(declare-unsafe-unary-operation $magnitude-compnum		<compnum>	<non-negative>)
(declare-unsafe-unary-operation $magnitude-cflonum		<cflonum>	<positive-flonum>)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $bitwise-not-fixnum		<fixnum>	<fixnum>)
(declare-unsafe-unary-operation $bitwise-not-bignum		<bignum>	<bignum>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-and-fixnum-number	<fixnum>	<exact-integer>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-and-fixnum-fixnum	<fixnum>	<fixnum>	      <fixnum>)
(declare-unsafe-binary-operation $bitwise-and-fixnum-bignum	<fixnum>	<bignum>	      <exact-integer>)

(declare-unsafe-binary-operation $bitwise-and-bignum-number	<bignum>	<exact-integer>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-and-bignum-fixnum	<bignum>	<fixnum>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-and-bignum-bignum	<bignum>	<bignum>	      <exact-integer>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-ior-fixnum-number	<fixnum>	<exact-integer>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-ior-fixnum-fixnum	<fixnum>	<fixnum>	      <fixnum>)
(declare-unsafe-binary-operation $bitwise-ior-fixnum-bignum	<fixnum>	<bignum>	      <exact-integer>)

(declare-unsafe-binary-operation $bitwise-ior-bignum-number	<bignum>	<exact-integer>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-ior-bignum-fixnum	<bignum>	<fixnum>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-ior-bignum-bignum	<bignum>	<bignum>	      <exact-integer>)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-xor-fixnum-number	<fixnum>	<exact-integer>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-xor-fixnum-fixnum	<fixnum>	<fixnum>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-xor-fixnum-bignum	<fixnum>	<bignum>	      <exact-integer>)

(declare-unsafe-binary-operation $bitwise-xor-bignum-number	<bignum>	<exact-integer>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-xor-bignum-fixnum	<bignum>	<fixnum>	      <exact-integer>)
(declare-unsafe-binary-operation $bitwise-xor-bignum-bignum	<bignum>	<bignum>	      <exact-integer>)

;;; --------------------------------------------------------------------

(let-syntax
    ((declare-unsafe-rounding-primitives
       (syntax-rules ()
	 ((_ ?who-fixnum ?who-bignum ?who-flonum ?who-ratnum)
	  (begin
	    (declare-core-primitive ?who-fixnum
		(unsafe)
	      (signatures
	       ((<fixnum>)			=> (<fixnum>)))
	      (attributes
	       ((_)				foldable effect-free result-true identity)))
	    (declare-core-primitive ?who-bignum
		(unsafe)
	      (signatures
	       ((<bignum>)			=> (<bignum>)))
	      (attributes
	       ((_)				foldable effect-free result-true identity)))
	    (declare-core-primitive ?who-flonum
		(unsafe)
	      (signatures
	       ((<flonum>)			=> (<flonum>)))
	      (attributes
	       ((_)				foldable effect-free result-true)))
	    (declare-core-primitive ?who-ratnum
		(unsafe)
	      (signatures
	       ((<ratnum>)			=> (<exact-integer>)))
	      (attributes
	       ((_)				foldable effect-free result-true)))))
	 )))
  (declare-unsafe-rounding-primitives $floor-fixnum $floor-bignum $floor-flonum $floor-ratnum)
  (declare-unsafe-rounding-primitives $ceiling-fixnum $ceiling-bignum $ceiling-flonum $ceiling-ratnum)
  (declare-unsafe-rounding-primitives $truncate-fixnum $truncate-bignum $truncate-flonum $truncate-ratnum)
  (declare-unsafe-rounding-primitives $round-fixnum $round-bignum $round-flonum $round-ratnum)
  #| end of LET-SYNTAX |# )

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
