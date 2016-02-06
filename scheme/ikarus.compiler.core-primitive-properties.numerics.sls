;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: compile-time property definitions for core primitives
;;;Date: Mon Sep 22, 2014
;;;
;;;Abstract
;;;
;;;	The purpose of this module is to  associate values to the public name of core
;;;	primitive.  The values represent core  primitive properties: the arity of the
;;;	primitive; the  number of  returned values;  the core  types of  the expected
;;;	arguments; the  core types of  the returned values;  miscellaneous properties
;;;	used by the source optimiser.
;;;
;;;	  Scheme  object's core  types  are  defined by  the  module "Scheme  objects
;;;	ontology".  This file contains a table  of core primitive properties for both
;;;	primitive functions and primitive operations.
;;;
;;;Copyright (C) 2014, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
(library (ikarus.compiler.core-primitive-properties.numerics)
  (export initialise-core-primitive-properties/numerics)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/numerics)


;;;; numerics, safe functions
;;
;;The order of  unsafe replacements *does* matter:  replacements accepting "T:number"
;;operands must come *after* the ones accepting more specific operand types.
;;

;;; predicates

(declare-type-predicate number?			T:number)
(declare-type-predicate complex?		T:number)

(declare-core-primitive real?
    (safe)
  (signatures
   ((T:real)			=> (T:true))
   ((T:compnum)			=> (T:false))
   ((T:cflonum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive rational?
    (safe)
  (signatures
   ((T:fixnum)			=> (T:true))
   ((T:bignum)			=> (T:true))
   ((T:ratnum)			=> (T:true))
   ((T:flonum-finite)		=> (T:true))
   ((T:flonum-infinite)		=> (T:false))
   ((T:flonum-nan)		=> (T:false))
   ((T:compnum)			=> (T:false))
   ((T:cflonum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-type-predicate integer?		T:integer)
(declare-type-predicate exact-integer?		T:exact-integer)

(declare-type-predicate real-valued?)
(declare-type-predicate rational-valued?)
(declare-type-predicate integer-valued?)

(declare-number-predicate odd?)
(declare-number-predicate even?)

;;;

(declare-core-primitive zero?
    (safe)
  (signatures
   ((T:positive-fixnum)		=> (T:false))
   ((T:negative-fixnum)		=> (T:false))
   ((T:ratnum)			=> (T:false))
   ((T:bignum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

(declare-core-primitive positive?
    (safe)
  (signatures
   ((T:positive-fixnum)		=> (T:true))
   ((T:negative-fixnum)		=> (T:false))
   ((T:positive-bignum)		=> (T:true))
   ((T:negative-bignum)		=> (T:false))
   ((T:real)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-false)
   ((_)				foldable effect-free)))

(declare-core-primitive negative?
    (safe)
  (signatures
   ((T:positive-fixnum)		=> (T:false))
   ((T:negative-fixnum)		=> (T:true))
   ((T:positive-bignum)		=> (T:false))
   ((T:negative-bignum)		=> (T:true))
   ((T:real)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-false)
   ((_)				foldable effect-free)))

(declare-core-primitive non-negative?
    (safe)
  (signatures
   ((T:negative-fixnum)		=> (T:false))
   ((T:non-negative-fixnum)	=> (T:true))
   ((T:positive-bignum)		=> (T:true))
   ((T:negative-bignum)		=> (T:false))
   ((T:real)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

(declare-core-primitive non-positive?
    (safe)
  (signatures
   ((T:positive-fixnum)		=> (T:false))
   ((T:non-positive-fixnum)	=> (T:true))
   ((T:positive-bignum)		=> (T:false))
   ((T:negative-bignum)		=> (T:true))
   ((T:real)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

;;;

(declare-core-primitive zero-exact-integer?
    (safe)
  (signatures
   ((T:object)		=> (T:boolean)))
  (attributes
   ((0)			foldable effect-free result-true)
   ((_)			foldable effect-free)))

(declare-core-primitive positive-exact-integer?
    (safe)
  (signatures
   ((T:positive-fixnum)	=> (T:true))
   ((T:positive-bignum)	=> (T:true))
   ((T:negative)	=> (T:false))
   ((T:object)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive negative-exact-integer?
    (safe)
  (signatures
   ((T:negative-fixnum)	=> (T:true))
   ((T:negative-bignum)	=> (T:true))
   ((T:positive)	=> (T:false))
   ((T:object)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive non-negative-exact-integer?
    (safe)
  (signatures
   ((T:non-negative-fixnum)	=> (T:true))
   ((T:positive-bignum)		=> (T:true))
   ((T:negative)		=> (T:false))
   ((T:object)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive non-positive-exact-integer?
    (safe)
  (signatures
   ((T:non-positive-fixnum)	=> (T:true))
   ((T:negative-bignum)		=> (T:true))
   ((T:positive)		=> (T:false))
   ((T:object)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;;

(declare-core-primitive exact?
    (safe)
  (signatures
   ((T:exact)			=> (T:true))
   ((T:inexact)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive inexact?
    (safe)
  (signatures
   ((T:exact)			=> (T:false))
   ((T:inexact)			=> (T:true))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;;

(declare-core-primitive finite?
    (safe)
  (signatures
   ((T:flonum-finite)		=> (T:true))
   ((T:flonum-infinite)		=> (T:false))
   ((T:flonum-nan)		=> (T:false))
   ((T:fixnum)			=> (T:true))
   ((T:bignum)			=> (T:true))
   ((T:ratnum)			=> (T:true))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive infinite?
    (safe)
  (signatures
   ((T:flonum-finite)		=> (T:false))
   ((T:flonum-infinite)		=> (T:true))
   ((T:flonum-nan)		=> (T:false))
   ((T:fixnum)			=> (T:false))
   ((T:bignum)			=> (T:false))
   ((T:ratnum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive nan?
    (safe)
  (signatures
   ((T:flonum-nan)		=> (T:false))
   ((T:flonum-infinite)		=> (T:false))
   ((T:flonum-nan)		=> (T:true))
   ((T:fixnum)			=> (T:false))
   ((T:bignum)			=> (T:false))
   ((T:ratnum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-core-primitive =
    (safe)
  (signatures
   ((T:positive     T:non-positive . T:number)		=> (T:false))
   ((T:non-positive T:positive     . T:number)		=> (T:false))
   ((T:negative     T:non-negative . T:number)		=> (T:false))
   ((T:non-negative T:negative     . T:number)		=> (T:false))
   ((T:number       T:number       . T:number)		=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive !=
    (safe)
  (signatures
   ((T:positive     T:non-positive . T:number)		=> (T:true))
   ((T:non-positive T:positive     . T:number)		=> (T:true))
   ((T:negative     T:non-negative . T:number)		=> (T:true))
   ((T:non-negative T:negative     . T:number)		=> (T:true))
   ((T:number       T:number       . T:number)		=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive <
    (safe)
  (signatures
   ((T:positive     T:non-positive)	=> (T:false))
   ((T:non-negative T:negative)		=> (T:false))

   ((T:non-positive T:positive)		=> (T:true))
   ((T:negative     T:non-negative)	=> (T:true))

   ((T:number T:number . T:number)	=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive >
    (safe)
  (signatures
   ((T:positive     T:non-positive)	=> (T:true))
   ((T:non-negative T:negative)		=> (T:true))

   ((T:non-positive T:positive)		=> (T:false))
   ((T:negative     T:non-negative)	=> (T:false))

   ((T:number T:number . T:number)	=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive <=
    (safe)
  (signatures
   ((T:positive T:negative)		=> (T:false))
   ((T:negative T:positive)		=> (T:true))

   ((T:number T:number . T:number)	=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive >=
    (safe)
  (signatures
   ((T:positive T:negative)		=> (T:true))
   ((T:negative T:positive)		=> (T:false))

   ((T:number T:number . T:number)	=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

;;;

(declare-core-primitive max
    (safe)
  (signatures
   ((T:fixnum . T:fixnum)		=> (T:fixnum))
   ((T:bignum . T:bignum)		=> (T:bignum))
   ((T:flonum . T:flonum)		=> (T:flonum))
   ((T:exact-integer . T:exact-integer)	=> (T:exact-integer))
   ((T:exact-real . T:exact-real)	=> (T:exact-real))
   ((T:integer . T:integer)		=> (T:integer))
   ((T:real . T:real)			=> (T:real)))
  (attributes
   ((_ . _)				foldable effect-free result-true))
  (replacements
   $max-fixnum-fixnum $max-fixnum-bignum $max-fixnum-flonum $max-fixnum-ratnum
   $max-bignum-fixnum $max-bignum-bignum $max-bignum-flonum $max-bignum-ratnum
   $max-flonum-fixnum $max-flonum-bignum $max-flonum-flonum $max-flonum-ratnum
   $max-ratnum-fixnum $max-ratnum-bignum $max-ratnum-flonum $max-ratnum-ratnum
   $max-fixnum-number $max-bignum-number $max-flonum-number $max-ratnum-number
   $max-number-fixnum $max-number-bignum $max-number-flonum $max-number-ratnum))

(declare-core-primitive min
    (safe)
  (signatures
   ((T:fixnum . T:fixnum)		=> (T:fixnum))
   ((T:bignum . T:bignum)		=> (T:bignum))
   ((T:flonum . T:flonum)		=> (T:flonum))
   ((T:exact-integer . T:exact-integer)	=> (T:exact-integer))
   ((T:exact-real . T:exact-real)	=> (T:exact-real))
   ((T:integer . T:integer)		=> (T:integer))
   ((T:real . T:real)			=> (T:real)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  (replacements
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
   (T:flonum			=> (T:flonum))
   (T:exact-integer		=> (T:exact-integer))
   (T:exact-real		=> (T:exact-real))
   (T:integer			=> (T:integer))
   (T:real			=> (T:real))
   (T:cflonum			=> (T:cflonum))
   (T:number			=> (T:number)))
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
   (T:flonum			=> (T:flonum))
   (T:exact-integer		=> (T:exact-integer))
   (T:exact-real		=> (T:exact-real))
   (T:integer			=> (T:integer))
   (T:real			=> (T:real))
   (T:cflonum			=> (T:cflonum))
   (T:number			=> (T:number)))
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
   (T:flonum			=> (T:flonum))
   (T:exact-integer		=> (T:exact-integer))
   (T:exact-real		=> (T:exact-real))
   (T:integer			=> (T:integer))
   (T:real			=> (T:real))
   (T:cflonum			=> (T:cflonum))
   (T:number			=> (T:number)))
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
   ((T:flonum  . T:flonum)	=> (T:flonum))
   ((T:real    . T:real)	=> (T:real))
   ((T:cflonum . T:cflonum)	=> (T:cflonum))
   ((T:number  . T:number)	=> (T:number)))
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
   ((T:exact-integer)		=> (T:exact-integer))
   ((T:flonum)			=> (T:flonum))
   ((T:integer)			=> (T:integer))
   ((T:real)			=> (T:real))
   ((T:cflonum)			=> (T:cflonum))
   ((T:compnum)			=> (T:compnum))
   ((T:number)			=> (T:number)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $add1-fixnum
   $add1-bignum
   $add1-integer))

(declare-core-primitive sub1
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:exact-integer))
   ((T:flonum)			=> (T:flonum))
   ((T:integer)			=> (T:integer))
   ((T:real)			=> (T:real))
   ((T:cflonum)			=> (T:cflonum))
   ((T:compnum)			=> (T:compnum))
   ((T:number)			=> (T:number)))
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
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive mod
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive div0
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive mod0
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive modulo
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remainder
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive quotient
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive quotient+remainder
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer T:exact-integer))
   ((T:integer T:integer)		=> (T:integer T:integer)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive div-and-mod
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer T:exact-integer))
   ((T:real T:real)			=> (T:real T:real)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive div0-and-mod0
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer T:exact-integer))
   ((T:real T:real)			=> (T:real T:real)))
  (attributes
   ((_ _)				effect-free)))

;;;

(declare-core-primitive gcd
    (safe)
  (signatures
   (()					=> (T:fixnum))

   ((T:fixnum)				=> (T:fixnum))
   ((T:bignum)				=> (T:bignum))
   ((T:flonum)				=> (T:flonum))
   ((T:exact-integer)			=> (T:exact-integer))
   ((T:real)				=> (T:real))

   ((T:fixnum T:fixnum)			=> (T:exact-integer))
   ((T:fixnum T:bignum)			=> (T:exact-integer))
   ((T:fixnum T:flonum)			=> (T:flonum))

   ((T:bignum T:fixnum)			=> (T:exact-integer))
   ((T:bignum T:bignum)			=> (T:exact-integer))
   ((T:bignum T:flonum)			=> (T:flonum))

   ((T:flonum T:fixnum)			=> (T:flonum))
   ((T:flonum T:bignum)			=> (T:flonum))
   ((T:flonum T:flonum)			=> (T:flonum))

   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real))

   ((T:real T:real T:real . T:real)	=> (T:real)))
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
   (()					=> (T:fixnum))

   ((T:fixnum)				=> (T:fixnum))
   ((T:bignum)				=> (T:bignum))
   ((T:flonum)				=> (T:flonum))
   ((T:exact-integer)			=> (T:exact-integer))
   ((T:real)				=> (T:real))

   ((T:fixnum T:fixnum)			=> (T:exact-integer))
   ((T:fixnum T:bignum)			=> (T:exact-integer))
   ((T:fixnum T:flonum)			=> (T:flonum))

   ((T:bignum T:fixnum)			=> (T:exact-integer))
   ((T:bignum T:bignum)			=> (T:exact-integer))
   ((T:bignum T:flonum)			=> (T:flonum))

   ((T:flonum T:fixnum)			=> (T:flonum))
   ((T:flonum T:bignum)			=> (T:flonum))
   ((T:flonum T:flonum)			=> (T:flonum))

   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real))

   ((T:real T:real T:real . T:real)	=> (T:real)))
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
   ((T:fixnum)		=> (T:fixnum))
   ((T:bignum)		=> (T:bignum))
   ((T:flonum)		=> (T:exact-integer))
   ((T:ratnum)		=> (T:ratnum))
   ((T:compnum)		=> (T:exact-compnum))
   ((T:cflonum)		=> (T:exact-compnum))
   ((T:real)		=> (T:exact-integer))
   ((T:number)		=> (T:exact)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $exact-fixnum $exact-bignum $exact-flonum $exact-ratnum $exact-compnum $exact-cflonum))

(declare-core-primitive inexact
    (safe)
  (signatures
   ((T:fixnum)		=> (T:flonum))
   ((T:bignum)		=> (T:flonum))
   ((T:flonum)		=> (T:flonum))
   ((T:ratnum)		=> (T:flonum))
   ((T:compnum)		=> (T:cflonum))
   ((T:cflonum)		=> (T:cflonum))
   ((T:real)		=> (T:flonum))
   ((T:number)		=> (T:inexact)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $inexact-fixnum $inexact-bignum $inexact-flonum $inexact-ratnum $inexact-compnum $inexact-cflonum))

(declare-core-primitive inexact->exact
    (safe)
  (signatures
   ((T:fixnum)		=> (T:fixnum))
   ((T:bignum)		=> (T:bignum))
   ((T:flonum)		=> (T:exact-integer))
   ((T:ratnum)		=> (T:ratnum))
   ((T:compnum)		=> (T:exact-compnum))
   ((T:cflonum)		=> (T:exact-compnum))
   ((T:real)		=> (T:exact-integer))
   ((T:number)		=> (T:exact)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $exact-fixnum $exact-bignum $exact-flonum $exact-ratnum $exact-compnum $exact-cflonum))

(declare-core-primitive exact->inexact
    (safe)
  (signatures
   ((T:fixnum)		=> (T:flonum))
   ((T:bignum)		=> (T:flonum))
   ((T:flonum)		=> (T:flonum))
   ((T:ratnum)		=> (T:flonum))
   ((T:compnum)		=> (T:cflonum))
   ((T:cflonum)		=> (T:cflonum))
   ((T:real)		=> (T:flonum))
   ((T:number)		=> (T:inexact)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $inexact-fixnum $inexact-bignum $inexact-flonum $inexact-ratnum $inexact-compnum $inexact-cflonum))

;;; --------------------------------------------------------------------
;;; parts

(declare-core-primitive numerator
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:exact-integer))
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $numerator-fixnum $numerator-bignum $numerator-flonum $numerator-ratnum))

(declare-core-primitive denominator
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:exact-integer))
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $denominator-fixnum $denominator-bignum $denominator-flonum $denominator-ratnum))

(declare-core-primitive rationalize
    (safe)
  (signatures
   ((T:real T:real)		=> (T:real)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive make-rectangular
    (safe)
  (signatures
   ((T:real T:real)		=> (T:complex)))
  (attributes
   ((_ _)			foldable effect-free result-true))
  (replacements $make-cflonum $make-compnum $make-rectangular))

(declare-core-primitive make-polar
    (safe)
  (signatures
   ((T:real T:real)		=> (T:complex)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-number-unary real-part		(replacements $compnum-real $cflonum-real))
(declare-number-unary imag-part		(replacements $compnum-imag $cflonum-imag))

(declare-core-primitive abs
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:ratnum)			=> (T:ratnum))
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $abs-fixnum $abs-bignum $abs-flonum $abs-ratnum))

(declare-core-primitive sign
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:fixnum))
   ((T:ratnum)			=> (T:fixnum))
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $sign-fixnum $sign-bignum $sign-flonum $sign-ratnum))

(declare-core-primitive angle
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:fixnum))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:fixnum))
   ((T:compnum)			=> (T:real))
   ((T:cflonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $angle-fixnum $angle-bignum $angle-ratnum $angle-flonum $angle-compnum $angle-cflonum))

(declare-core-primitive magnitude
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:ratnum)			=> (T:ratnum))
   ((T:flonum)			=> (T:flonum))
   ((T:compnum)			=> (T:real))
   ((T:cflonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $magnitude-fixnum $magnitude-bignum $magnitude-ratnum $magnitude-flonum $magnitude-compnum $magnitude-cflonum))

(declare-core-primitive complex-conjugate
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:ratnum)			=> (T:ratnum))
   ((T:flonum)			=> (T:flonum))
   ((T:compnum)			=> (T:compnum))
   ((T:cflonum)			=> (T:cflonum)))
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
	    ((T:fixnum)			=> (T:fixnum))
	    ((T:bignum)			=> (T:bignum))
	    ((T:flonum)			=> (T:flonum))
	    ((T:ratnum)			=> (T:ratnum))
	    ((T:real)			=> (T:real)))
	   (attributes
	    ((_)			foldable effect-free result-true))
	   (replacements . ?replacements))))))
  (declare-safe-rounding-primitive floor	   $floor-fixnum    $floor-bignum    $floor-ratnum    $floor-flonum)
  (declare-safe-rounding-primitive ceiling	 $ceiling-fixnum  $ceiling-bignum  $ceiling-ratnum  $ceiling-flonum)
  (declare-safe-rounding-primitive truncate	$truncate-fixnum $truncate-bignum $truncate-ratnum $truncate-flonum)
  (declare-safe-rounding-primitive round	   $round-fixnum    $round-bignum    $round-ratnum    $round-flonum)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; comparison

(let-syntax
    ((declare-real-comparison-unary/multi (syntax-rules ()
					    ((_ ?who)
					     (declare-core-primitive ?who
						 (safe)
					       (signatures
						((T:real . T:real)	=> (T:boolean)))
					       (attributes
						((_ . _)		foldable effect-free)))))))
  (declare-real-comparison-unary/multi =)
  (declare-real-comparison-unary/multi !=)
  (declare-real-comparison-unary/multi <)
  (declare-real-comparison-unary/multi >=)
  (declare-real-comparison-unary/multi <)
  (declare-real-comparison-unary/multi >=)
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
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:exact-real))
   ((T:compnum)			=> (T:number))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $square-fixnum $square-bignum $flsquare $square-ratnum $square-compnum $square-cflonum))

(declare-core-primitive cube
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:exact-real))
   ((T:compnum)			=> (T:number))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $cube-fixnum $cube-bignum $flcube $cube-ratnum $cube-compnum $cube-cflonum))

(declare-core-primitive sqrt
    (safe)
  (signatures
   ((T:fixnum)			=> (T:number))
   ((T:bignum)			=> (T:number))
   ((T:flonum)			=> (T:inexact))
   ((T:ratnum)			=> (T:number))
   ((T:compnum)			=> (T:number))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $sqrt-fixnum $sqrt-bignum $sqrt-flonum $sqrt-ratnum $sqrt-compnum $sqrt-cflonum))

(declare-core-primitive cbrt
    (safe)
  (signatures
   ((T:fixnum)			=> (T:number))
   ((T:bignum)			=> (T:number))
   ((T:flonum)			=> (T:inexact))
   ((T:ratnum)			=> (T:number))
   ((T:compnum)			=> (T:number))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $cbrt-fixnum $cbrt-bignum $cbrt-flonum $cbrt-ratnum $cbrt-compnum $cbrt-cflonum))

(declare-core-primitive exact-integer-sqrt
    (safe)
  (signatures
   ((T:fixnum)			=> (T:positive-fixnum T:positive-fixnum))
   ((T:bignum)			=> (T:exact-integer T:exact-integer))
   ((T:exact-integer)		=> (T:exact-integer T:exact-integer)))
  (attributes
   ((_)				effect-free))
  (replacements $exact-integer-sqrt-fixnum $exact-integer-sqrt-bignum))

(declare-core-primitive exp
    (safe)
  (signatures
   ((T:fixnum)			=> (T:real))
   ((T:bignum)			=> (T:flonum))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:flonum))
   ((T:compnum)			=> (T:cflonum))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $exp-fixnum $exp-bignum $exp-flonum $exp-ratnum $exp-compnum $exp-cflonum))

(declare-core-primitive log
    (safe)
  (signatures
   ((T:fixnum)			=> (T:number))
   ((T:bignum)			=> (T:inexact))
   ((T:flonum)			=> (T:inexact))
   ((T:ratnum)			=> (T:inexact))
   ((T:compnum)			=> (T:inexact))
   ((T:cflonum)			=> (T:cflonum))
   ((T:number T:number)		=> (T:number)))
  (attributes
   ((_)				foldable effect-free result-true)
   ((_ _)			foldable effect-free result-true))
  (replacements $log-fixnum $log-bignum $log-flonum $log-ratnum $log-compnum $log-cflonum))

;;; --------------------------------------------------------------------
;;; bitwise

(declare-core-primitive bitwise-and
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:fixnum T:fixnum)		=> (T:fixnum))
   ((T:bignum T:bignum)		=> (T:bignum))
   ((T:fixnum T:bignum)		=> (T:bignum))
   ((T:bignum T:fixnum)		=> (T:bignum))
   (T:exact-integer		=> (T:exact-integer)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-and-fixnum-fixnum $bitwise-and-fixnum-bignum
   $bitwise-and-bignum-fixnum $bitwise-and-bignum-bignum
   $bitwise-and-fixnum-number $bitwise-and-bignum-number))

(declare-core-primitive bitwise-ior
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:fixnum T:fixnum)		=> (T:fixnum))
   ((T:bignum T:bignum)		=> (T:bignum))
   ((T:fixnum T:bignum)		=> (T:bignum))
   ((T:bignum T:fixnum)		=> (T:bignum))
   (T:exact-integer		=> (T:exact-integer)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-ior-fixnum-fixnum $bitwise-ior-fixnum-bignum
   $bitwise-ior-bignum-fixnum $bitwise-ior-bignum-bignum
   $bitwise-ior-fixnum-number $bitwise-ior-bignum-number))

(declare-core-primitive bitwise-xor
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:fixnum T:fixnum)		=> (T:fixnum))
   ((T:bignum T:bignum)		=> (T:bignum))
   ((T:fixnum T:bignum)		=> (T:bignum))
   ((T:bignum T:fixnum)		=> (T:bignum))
   (T:exact-integer		=> (T:exact-integer)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-xor-fixnum-fixnum $bitwise-xor-fixnum-bignum
   $bitwise-xor-bignum-fixnum $bitwise-xor-bignum-bignum
   $bitwise-xor-fixnum-number $bitwise-xor-bignum-number))

(declare-core-primitive bitwise-not
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $bitwise-not-fixnum $bitwise-not-bignum))

(declare-core-primitive bitwise-arithmetic-shift
    (safe)
  (signatures
   ((T:exact-integer T:fixnum)	=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-arithmetic-shift-left
    (safe)
  (signatures
   ((T:exact-integer T:fixnum)	=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-arithmetic-shift-right
    (safe)
  (signatures
   ((T:exact-integer T:fixnum)	=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-count
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-field
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-set?
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive bitwise-copy-bit
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-copy-bit-field
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-first-bit-set
    (safe)
  (signatures
   ((T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bitwise-if
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-length
    (safe)
  (signatures
   ((T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bitwise-reverse-bit-field
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-rotate-bit-field
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive sll
    (safe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:exact-integer))
   ((T:bignum T:fixnum)		=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive sra
    (safe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:exact-integer))
   ((T:bignum T:fixnum)		=> (T:exact-integer)))
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
   ((T:fixnum)		=> (T:char)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive number->string
    (safe)
  (signatures
   ((T:number)				=> (T:string))
   ((T:number T:positive-fixnum)	=> (T:string))
   ((T:number T:positive-fixnum)	=> (T:string)))
  (attributes
   ((_)			foldable effect-free result-true)
   ((_ _)		foldable effect-free result-true)
   ((_ _ _)		foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; shortcut's interrupt handlers

(declare-core-primitive error@fx+
    (safe)
  (signatures
   ((T:object T:object)		=> (T:void))))

(declare-core-primitive error@fx*
    (safe)
  (signatures
   ((T:object T:object)		=> (T:void))))

(declare-core-primitive error@fxadd1
    (safe)
  (signatures
   ((T:object)			=> (T:void))))

(declare-core-primitive error@fxsub1
    (safe)
  (signatures
   ((T:object)			=> (T:void))))

(declare-core-primitive error@fxarithmetic-shift-left
    (safe)
  (signatures
   ((T:object T:object)		=> (T:void))))

(declare-core-primitive error@fxarithmetic-shift-right
    (safe)
  (signatures
   ((T:object T:object)		=> (T:void))))

;;;

(declare-core-primitive error@add1
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive error@sub1
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; numerics, unsafe functions

(declare-core-primitive $make-rectangular
    (unsafe)
  (signatures
   ((T:real T:real)		=> (T:complex)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $neg-number	T:number	T:number)
(declare-unsafe-unary-operation $neg-fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-unary-operation $neg-bignum	T:bignum	T:exact-integer)
(declare-unsafe-unary-operation $neg-flonum	T:flonum	T:flonum)
(declare-unsafe-unary-operation $neg-ratnum	T:ratnum	T:ratnum)
(declare-unsafe-unary-operation $neg-compnum	T:compnum	T:compnum)
(declare-unsafe-unary-operation $neg-cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $inv-number	T:number	T:number)
(declare-unsafe-unary-operation $inv-fixnum	T:fixnum	T:exact-real)
(declare-unsafe-unary-operation $inv-bignum	T:bignum	T:ratnum)
(declare-unsafe-unary-operation $inv-flonum	T:flonum	T:flonum)
(declare-unsafe-unary-operation $inv-ratnum	T:ratnum	T:exact-real)
(declare-unsafe-unary-operation $inv-compnum	T:compnum	T:complex)
(declare-unsafe-unary-operation $inv-cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $add1-integer	T:exact-integer	T:exact-integer)

(declare-core-primitive $add1-fixnum
    (unsafe)
  (signatures
   ((T:non-positive-fixnum)	=> (T:fixnum))
   ((T:fixnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $add1-bignum
    (unsafe)
  (signatures
   ((T:positive-bignum)		=> (T:bignum))
   ((T:bignum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sub1-integer	T:exact-integer	T:exact-integer)

(declare-core-primitive $sub1-fixnum
    (unsafe)
  (signatures
   ((T:non-negative-fixnum)	=> (T:fixnum))
   ((T:fixnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $sub1-bignum
    (unsafe)
  (signatures
   ((T:negative-bignum)		=> (T:bignum))
   ((T:bignum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $add-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $add-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $add-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $add-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $add-ratnum-number	T:ratnum	T:number	T:number)
(declare-unsafe-binary-operation $add-compnum-number	T:compnum	T:number	T:number)
(declare-unsafe-binary-operation $add-cflonum-number	T:cflonum	T:number	T:number)

(declare-unsafe-binary-operation $add-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $add-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $add-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $add-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $add-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $add-number-cflonum	T:number	T:cflonum	T:number)

(declare-unsafe-binary-operation $add-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $add-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $add-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $add-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $add-fixnum-compnum	T:fixnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $add-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $add-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $add-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $add-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $add-bignum-compnum	T:bignum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $add-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $add-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $add-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $add-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $add-flonum-compnum	T:flonum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $add-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $add-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $add-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $add-ratnum-ratnum	T:ratnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $add-ratnum-compnum	T:ratnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $add-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-compnum-fixnum	T:compnum	T:fixnum	T:compnum)
(declare-unsafe-binary-operation $add-compnum-bignum	T:compnum	T:bignum	T:compnum)
(declare-unsafe-binary-operation $add-compnum-flonum	T:compnum	T:flonum	T:complex)
(declare-unsafe-binary-operation $add-compnum-ratnum	T:compnum	T:ratnum	T:compnum)
(declare-unsafe-binary-operation $add-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $add-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-ratnum	T:cflonum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-flonum	T:cflonum	T:ratnum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-compnum	T:cflonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $sub-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $sub-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $sub-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $sub-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $sub-ratnum-number	T:ratnum	T:number	T:number)
(declare-unsafe-binary-operation $sub-compnum-number	T:compnum	T:number	T:number)
(declare-unsafe-binary-operation $sub-cflonum-number	T:cflonum	T:number	T:number)

(declare-unsafe-binary-operation $sub-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $sub-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $sub-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $sub-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $sub-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $sub-number-cflonum	T:number	T:cflonum	T:number)

(declare-unsafe-binary-operation $sub-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $sub-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $sub-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $sub-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $sub-fixnum-compnum	T:fixnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $sub-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $sub-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $sub-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $sub-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $sub-bignum-compnum	T:bignum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $sub-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $sub-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $sub-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $sub-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $sub-flonum-compnum	T:flonum	T:compnum	T:complex)
(declare-unsafe-binary-operation $sub-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $sub-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $sub-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $sub-ratnum-ratnum	T:ratnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $sub-ratnum-compnum	T:ratnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $sub-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-compnum-fixnum	T:compnum	T:fixnum	T:compnum)
(declare-unsafe-binary-operation $sub-compnum-bignum	T:compnum	T:bignum	T:compnum)
(declare-unsafe-binary-operation $sub-compnum-flonum	T:compnum	T:flonum	T:complex)
(declare-unsafe-binary-operation $sub-compnum-ratnum	T:compnum	T:ratnum	T:compnum)
(declare-unsafe-binary-operation $sub-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $sub-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-flonum	T:cflonum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-ratnum	T:cflonum	T:ratnum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-compnum	T:cflonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $mul-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $mul-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $mul-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $mul-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $mul-ratnum-number	T:ratnum	T:number	T:number)
(declare-unsafe-binary-operation $mul-compnum-number	T:compnum	T:number	T:number)
(declare-unsafe-binary-operation $mul-cflonum-number	T:cflonum	T:number	T:cflonum)

(declare-unsafe-binary-operation $mul-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $mul-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $mul-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $mul-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $mul-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $mul-number-cflonum	T:number	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $mul-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $mul-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $mul-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $mul-fixnum-compnum	T:fixnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $mul-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $mul-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $mul-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $mul-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $mul-bignum-compnum	T:bignum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $mul-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $mul-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $mul-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $mul-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $mul-flonum-compnum	T:flonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $mul-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $mul-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $mul-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $mul-ratnum-ratnum	T:ratnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $mul-ratnum-compnum	T:ratnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $mul-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-compnum-fixnum	T:compnum	T:fixnum	T:compnum)
(declare-unsafe-binary-operation $mul-compnum-bignum	T:compnum	T:bignum	T:compnum)
(declare-unsafe-binary-operation $mul-compnum-flonum	T:compnum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $mul-compnum-ratnum	T:compnum	T:ratnum	T:compnum)
(declare-unsafe-binary-operation $mul-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $mul-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-flonum	T:cflonum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-ratnum	T:cflonum	T:ratnum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-compnum	T:cflonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $div-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $div-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $div-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $div-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $div-ratnum-number	T:ratnum	T:number	T:number)
(declare-unsafe-binary-operation $div-compnum-number	T:compnum	T:number	T:number)
(declare-unsafe-binary-operation $div-cflonum-number	T:cflonum	T:number	T:number)

(declare-unsafe-binary-operation $div-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $div-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $div-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $div-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $div-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $div-number-cflonum	T:number	T:cflonum	T:number)

(declare-unsafe-binary-operation $div-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $div-fixnum-bignum	T:fixnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $div-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $div-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $div-fixnum-compnum	T:fixnum	T:compnum	T:number)
(declare-unsafe-binary-operation $div-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-bignum-fixnum	T:bignum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $div-bignum-bignum	T:bignum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $div-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $div-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $div-bignum-compnum	T:bignum	T:compnum	T:number)
(declare-unsafe-binary-operation $div-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $div-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $div-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $div-ratnum-ratnum	T:ratnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $div-ratnum-compnum	T:ratnum	T:compnum	T:number)
(declare-unsafe-binary-operation $div-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $div-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $div-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $div-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $div-flonum-compnum	T:flonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $div-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-compnum-fixnum	T:compnum	T:fixnum	T:number)
(declare-unsafe-binary-operation $div-compnum-bignum	T:compnum	T:bignum	T:number)
(declare-unsafe-binary-operation $div-compnum-flonum	T:compnum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $div-compnum-ratnum	T:compnum	T:ratnum	T:number)
(declare-unsafe-binary-operation $div-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $div-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-flonum	T:cflonum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-ratnum	T:cflonum	T:ratnum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-compnum	T:cflonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $square-fixnum		T:fixnum	T:exact-integer)
(declare-unsafe-unary-operation $square-bignum		T:bignum	T:exact-integer)
(declare-unsafe-unary-operation $square-ratnum		T:ratnum	T:exact-real)
(declare-unsafe-unary-operation $square-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $square-cflonum		T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $cube-fixnum		T:fixnum	T:exact-integer)
(declare-unsafe-unary-operation $cube-bignum		T:bignum	T:exact-integer)
(declare-unsafe-unary-operation $cube-ratnum		T:ratnum	T:exact-real)
(declare-unsafe-unary-operation $cube-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $cube-cflonum		T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $gcd-number		T:number	T:number)

(declare-unsafe-binary-operation $gcd-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $gcd-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $gcd-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $gcd-flonum-number	T:flonum	T:number	T:number)

(declare-unsafe-binary-operation $gcd-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $gcd-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $gcd-number-flonum	T:number	T:flonum	T:number)

(declare-unsafe-binary-operation $gcd-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $gcd-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $gcd-fixnum-flonum	T:fixnum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $gcd-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $gcd-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $gcd-bignum-flonum	T:bignum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $gcd-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $gcd-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $gcd-flonum-flonum	T:flonum	T:flonum	T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $lcm-number		T:number	T:number)

(declare-unsafe-binary-operation $lcm-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $lcm-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $lcm-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $lcm-flonum-number	T:flonum	T:number	T:number)

(declare-unsafe-binary-operation $lcm-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $lcm-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $lcm-number-flonum	T:number	T:flonum	T:number)

(declare-unsafe-binary-operation $lcm-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $lcm-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $lcm-fixnum-flonum	T:fixnum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $lcm-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $lcm-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $lcm-bignum-flonum	T:bignum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $lcm-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $lcm-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $lcm-flonum-flonum	T:flonum	T:flonum	T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-number	T:fixnum	T:number      T:number	      T:number)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-number	T:bignum	T:number      T:number	      T:number)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-number	T:flonum	T:number      T:number	      T:number)

(declare-unsafe-binary-operation/2rv $quotient+remainder-number-fixnum	T:number	T:fixnum      T:number	      T:number)
(declare-unsafe-binary-operation/2rv $quotient+remainder-number-bignum	T:number	T:bignum      T:number	      T:number)
(declare-unsafe-binary-operation/2rv $quotient+remainder-number-flonum	T:number	T:flonum      T:number	      T:number)

(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-fixnum	T:fixnum	T:fixnum      T:exact-integer T:exact-integer)
(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-bignum	T:fixnum	T:bignum      T:exact-integer T:exact-integer)
(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-flonum	T:fixnum	T:flonum      T:flonum	      T:flonum)

(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-fixnum	T:bignum	T:fixnum      T:exact-integer T:exact-integer)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-bignum	T:bignum	T:bignum      T:exact-integer T:exact-integer)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-flonum	T:bignum	T:flonum      T:flonum	      T:flonum)

(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-fixnum	T:flonum	T:fixnum      T:flonum	      T:flonum)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-bignum	T:flonum	T:bignum      T:flonum	      T:flonum)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-flonum	T:flonum	T:flonum      T:flonum	      T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $quotient-fixnum-number	T:fixnum	T:number      T:number)
(declare-unsafe-binary-operation $quotient-bignum-number	T:bignum	T:number      T:number)
(declare-unsafe-binary-operation $quotient-flonum-number	T:flonum	T:number      T:number)

(declare-unsafe-binary-operation $quotient-number-fixnum	T:number	T:fixnum      T:number)
(declare-unsafe-binary-operation $quotient-number-bignum	T:number	T:bignum      T:number)
(declare-unsafe-binary-operation $quotient-number-flonum	T:number	T:flonum      T:number)

(declare-unsafe-binary-operation $quotient-fixnum-fixnum	T:fixnum	T:fixnum      T:exact-integer)
(declare-unsafe-binary-operation $quotient-fixnum-bignum	T:fixnum	T:bignum      T:exact-integer)
(declare-unsafe-binary-operation $quotient-fixnum-flonum	T:fixnum	T:flonum      T:flonum)

(declare-unsafe-binary-operation $quotient-bignum-fixnum	T:bignum	T:fixnum      T:exact-integer)
(declare-unsafe-binary-operation $quotient-bignum-bignum	T:bignum	T:bignum      T:exact-integer)
(declare-unsafe-binary-operation $quotient-bignum-flonum	T:bignum	T:flonum      T:flonum)

(declare-unsafe-binary-operation $quotient-flonum-fixnum	T:flonum	T:fixnum      T:flonum)
(declare-unsafe-binary-operation $quotient-flonum-bignum	T:flonum	T:bignum      T:flonum)
(declare-unsafe-binary-operation $quotient-flonum-flonum	T:flonum	T:flonum      T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $remainder-fixnum-number	T:fixnum	T:number      T:number)
(declare-unsafe-binary-operation $remainder-bignum-number	T:bignum	T:number      T:number)
(declare-unsafe-binary-operation $remainder-flonum-number	T:flonum	T:number      T:number)

(declare-unsafe-binary-operation $remainder-number-fixnum	T:number	T:fixnum      T:number)
(declare-unsafe-binary-operation $remainder-number-bignum	T:number	T:bignum      T:number)
(declare-unsafe-binary-operation $remainder-number-flonum	T:number	T:flonum      T:number)

(declare-unsafe-binary-operation $remainder-fixnum-fixnum	T:fixnum	T:fixnum      T:exact-integer)
(declare-unsafe-binary-operation $remainder-fixnum-bignum	T:fixnum	T:bignum      T:exact-integer)
(declare-unsafe-binary-operation $remainder-fixnum-flonum	T:fixnum	T:flonum      T:flonum)

(declare-unsafe-binary-operation $remainder-bignum-fixnum	T:bignum	T:fixnum      T:exact-integer)
(declare-unsafe-binary-operation $remainder-bignum-bignum	T:bignum	T:bignum      T:exact-integer)
(declare-unsafe-binary-operation $remainder-bignum-flonum	T:bignum	T:flonum      T:flonum)

(declare-unsafe-binary-operation $remainder-flonum-fixnum	T:flonum	T:fixnum      T:flonum)
(declare-unsafe-binary-operation $remainder-flonum-bignum	T:flonum	T:bignum      T:flonum)
(declare-unsafe-binary-operation $remainder-flonum-flonum	T:flonum	T:flonum      T:flonum)

;;; --------------------------------------------------------------------

(declare-core-primitive $numerator-fixnum
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $numerator-bignum
    (safe)
  (signatures
   ((T:bignum)			=> (T:bignum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $numerator-flonum
    (safe)
  (signatures
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $numerator-ratnum
    (safe)
  (signatures
   ((T:ratnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-fixnum
    (safe)
  (signatures
   ((T:fixnum)			=> (T:positive-fixnum)))
  (attributes
   ;;This always returns 1.
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-bignum
    (safe)
  (signatures
   ((T:bignum)			=> (T:positive-bignum)))
  (attributes
   ;;This always returns 1.
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-flonum
    (safe)
  (signatures
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-ratnum
    (safe)
  (signatures
   ((T:ratnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $modulo-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $modulo-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $modulo-flonum-number	T:flonum	T:number	T:number)

(declare-unsafe-binary-operation $modulo-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $modulo-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $modulo-number-flonum	T:number	T:flonum	T:number)

(declare-unsafe-binary-operation $modulo-fixnum-fixnum	T:fixnum	T:fixnum	T:fixnum)
(declare-unsafe-binary-operation $modulo-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $modulo-fixnum-flonum	T:fixnum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $modulo-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $modulo-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $modulo-bignum-flonum	T:bignum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $modulo-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $modulo-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $modulo-flonum-flonum	T:flonum	T:flonum	T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $max-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $max-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $max-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $max-ratnum-number	T:ratnum	T:number	T:number)

(declare-unsafe-binary-operation $max-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $max-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $max-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $max-number-ratnum	T:number	T:ratnum	T:number)

(declare-unsafe-binary-operation $max-fixnum-fixnum	T:fixnum	T:fixnum	T:fixnum)
(declare-unsafe-binary-operation $max-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $max-fixnum-flonum	T:fixnum	T:flonum	T:real)
(declare-unsafe-binary-operation $max-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)

(declare-unsafe-binary-operation $max-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $max-bignum-bignum	T:bignum	T:bignum	T:bignum)
(declare-unsafe-binary-operation $max-bignum-flonum	T:bignum	T:flonum	T:real)
(declare-unsafe-binary-operation $max-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)

(declare-unsafe-binary-operation $max-flonum-fixnum	T:flonum	T:fixnum	T:real)
(declare-unsafe-binary-operation $max-flonum-bignum	T:flonum	T:bignum	T:real)
(declare-unsafe-binary-operation $max-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $max-flonum-ratnum	T:flonum	T:ratnum	T:real)

(declare-unsafe-binary-operation $max-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $max-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $max-ratnum-flonum	T:ratnum	T:flonum	T:real)
(declare-unsafe-binary-operation $max-ratnum-ratnum	T:ratnum	T:ratnum	T:ratnum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $min-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $min-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $min-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $min-ratnum-number	T:ratnum	T:number	T:number)

(declare-unsafe-binary-operation $min-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $min-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $min-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $min-number-ratnum	T:number	T:ratnum	T:number)

(declare-unsafe-binary-operation $min-fixnum-fixnum	T:fixnum	T:fixnum	T:fixnum)
(declare-unsafe-binary-operation $min-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $min-fixnum-flonum	T:fixnum	T:flonum	T:real)
(declare-unsafe-binary-operation $min-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)

(declare-unsafe-binary-operation $min-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $min-bignum-bignum	T:bignum	T:bignum	T:bignum)
(declare-unsafe-binary-operation $min-bignum-flonum	T:bignum	T:flonum	T:real)
(declare-unsafe-binary-operation $min-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)

(declare-unsafe-binary-operation $min-flonum-fixnum	T:flonum	T:fixnum	T:real)
(declare-unsafe-binary-operation $min-flonum-bignum	T:flonum	T:bignum	T:real)
(declare-unsafe-binary-operation $min-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $min-flonum-ratnum	T:flonum	T:ratnum	T:real)

(declare-unsafe-binary-operation $min-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $min-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $min-ratnum-flonum	T:ratnum	T:flonum	T:real)
(declare-unsafe-binary-operation $min-ratnum-ratnum	T:ratnum	T:ratnum	T:ratnum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $abs-fixnum		T:fixnum	T:exact-integer)
(declare-unsafe-unary-operation $abs-bignum		T:bignum	T:exact-integer)
(declare-unsafe-unary-operation $abs-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $abs-ratnum		T:ratnum	T:ratnum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sign-fixnum		T:fixnum	T:fixnum)
(declare-unsafe-unary-operation $sign-bignum		T:bignum	T:fixnum)
(declare-unsafe-unary-operation $sign-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $sign-ratnum		T:ratnum	T:fixnum)

;;; --------------------------------------------------------------------

(declare-core-primitive $exact-fixnum
    (unsafe)
  (signatures
   ((T:positive-fixnum)		=> (T:positive-fixnum))
   ((T:negative-fixnum)		=> (T:negative-fixnum))
   ((T:non-positive-fixnum)	=> (T:non-positive-fixnum))
   ((T:non-negative-fixnum)	=> (T:non-negative-fixnum))
   ((T:fixnum)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-bignum
    (unsafe)
  (signatures
   ((T:positive-bignum)		=> (T:positive-bignum))
   ((T:negative-bignum)		=> (T:negative-bignum))
   ((T:bignum)			=> (T:bignum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-flonum
    (unsafe)
  (signatures
   ((T:flonum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $exact-ratnum
    (unsafe)
  (signatures
   ((T:ratnum)			=> (T:ratnum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-compnum
    (unsafe)
  (signatures
   ((T:compnum)			=> (T:exact-compnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $exact-cflonum
    (unsafe)
  (signatures
   ((T:cflonum)			=> (T:exact-compnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;;

(declare-core-primitive $inexact-fixnum
    (unsafe)
  (signatures
   ((T:positive-fixnum)		=> (T:positive-flonum))
   ((T:negative-fixnum)		=> (T:negative-flonum))
   ((T:non-positive-fixnum)	=> (T:non-positive-flonum))
   ((T:non-negative-fixnum)	=> (T:non-negative-flonum))
   ((T:fixnum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-bignum
    (unsafe)
  (signatures
   ((T:positive-bignum)		=> (T:positive-flonum))
   ((T:negative-bignum)		=> (T:negative-flonum))
   ((T:bignum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-flonum
    (unsafe)
  (signatures
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $inexact-ratnum
    (unsafe)
  (signatures
   ((T:ratnum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-compnum
    (unsafe)
  (signatures
   ((T:compnum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-cflonum
    (unsafe)
  (signatures
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $expt-number-fixnum	T:number	T:number	T:number)

(declare-unsafe-unary-operation $expt-number-zero-fixnum	T:number	T:number)
(declare-unsafe-unary-operation $expt-fixnum-zero-fixnum	T:fixnum	T:fixnum)
(declare-unsafe-unary-operation $expt-flonum-zero-fixnum	T:flonum	T:flonum)
(declare-unsafe-unary-operation $expt-compnum-zero-fixnum	T:compnum	T:number)
(declare-unsafe-unary-operation $expt-cflonum-zero-fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $expt-number-negative-fixnum	T:number	T:negative-fixnum      T:number)
(declare-unsafe-binary-operation $expt-fixnum-negative-fixnum	T:fixnum	T:negative-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-bignum-negative-fixnum	T:bignum	T:negative-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-flonum-negative-fixnum	T:flonum	T:negative-fixnum      T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-negative-fixnum	T:ratnum	T:negative-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-compnum-negative-fixnum	T:compnum	T:negative-fixnum      T:number)
(declare-unsafe-binary-operation $expt-cflonum-negative-fixnum	T:cflonum	T:negative-fixnum      T:cflonum)

(declare-unsafe-binary-operation $expt-number-positive-fixnum	T:number	T:positive-fixnum      T:number)
(declare-unsafe-binary-operation $expt-fixnum-positive-fixnum	T:number	T:positive-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-bignum-positive-fixnum	T:number	T:positive-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-flonum-positive-fixnum	T:number	T:positive-fixnum      T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-positive-fixnum	T:number	T:positive-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-compnum-positive-fixnum	T:number	T:positive-fixnum      T:number)
(declare-unsafe-binary-operation $expt-cflonum-positive-fixnum	T:number	T:positive-fixnum      T:cflonum)

(declare-unsafe-binary-operation $expt-fixnum-fixnum	T:fixnum	T:fixnum	T:exact)
(declare-unsafe-binary-operation $expt-bignum-fixnum	T:bignum	T:fixnum	T:exact)
(declare-unsafe-binary-operation $expt-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-fixnum	T:ratnum	T:fixnum	T:exact)
(declare-unsafe-binary-operation $expt-compnum-fixnum	T:compnum	T:fixnum	T:number)
(declare-unsafe-binary-operation $expt-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)

;;;

(declare-unsafe-binary-operation $expt-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $expt-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $expt-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $expt-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-number-cflonum	T:number	T:cflonum	T:number)

(declare-unsafe-binary-operation $expt-fixnum-bignum	T:fixnum	T:bignum	T:exact)
(declare-unsafe-binary-operation $expt-bignum-bignum	T:bignum	T:bignum	T:exact)
(declare-unsafe-binary-operation $expt-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-bignum	T:ratnum	T:bignum	T:exact)
(declare-unsafe-binary-operation $expt-compnum-bignum	T:compnum	T:bignum	T:number)
(declare-unsafe-binary-operation $expt-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)

(declare-unsafe-binary-operation $expt-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-compnum-flonum	T:compnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-cflonum-flonum	T:cflonum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $expt-fixnum-ratnum	T:fixnum	T:ratnum	T:exact)
(declare-unsafe-binary-operation $expt-bignum-ratnum	T:bignum	T:ratnum	T:exact)
(declare-unsafe-binary-operation $expt-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-ratnum	T:ratnum	T:ratnum	T:exact)
(declare-unsafe-binary-operation $expt-compnum-ratnum	T:compnum	T:ratnum	T:number)
(declare-unsafe-binary-operation $expt-cflonum-ratnum	T:cflonum	T:ratnum	T:cflonum)

(declare-unsafe-binary-operation $expt-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $expt-fixnum-compnum	T:fixnum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-bignum-compnum	T:bignum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-flonum-compnum	T:flonum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-ratnum-compnum	T:ratnum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-cflonum-compnum	T:cflonum	T:compnum	T:number)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sqrt-fixnum		T:fixnum	T:number)
(declare-unsafe-unary-operation $sqrt-bignum		T:bignum	T:number)
(declare-unsafe-unary-operation $sqrt-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $sqrt-ratnum		T:ratnum	T:number)
(declare-unsafe-unary-operation $sqrt-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $sqrt-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation/2rv $exact-integer-sqrt-fixnum	T:fixnum	T:exact-integer	      T:exact-integer)
(declare-unsafe-unary-operation/2rv $exact-integer-sqrt-bignum	T:bignum	T:exact-integer	      T:exact-integer)

(declare-unsafe-unary-operation $cbrt-fixnum		T:fixnum	T:number)
(declare-unsafe-unary-operation $cbrt-bignum		T:bignum	T:number)
(declare-unsafe-unary-operation $cbrt-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $cbrt-ratnum		T:ratnum	T:number)
(declare-unsafe-unary-operation $cbrt-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $cbrt-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $log-fixnum		T:fixnum	T:number)
(declare-unsafe-unary-operation $log-bignum		T:bignum	T:inexact)
(declare-unsafe-unary-operation $log-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $log-ratnum		T:ratnum	T:inexact)
(declare-unsafe-unary-operation $log-compnum		T:compnum	T:inexact)
(declare-unsafe-unary-operation $log-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $exp-fixnum		T:fixnum	T:real)
(declare-unsafe-unary-operation $exp-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $exp-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $exp-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $exp-compnum		T:compnum	T:cflonum)
(declare-unsafe-unary-operation $exp-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $sin-fixnum		T:fixnum	T:real)
(declare-unsafe-unary-operation $sin-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $sin-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $sin-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $sin-compnum		T:compnum	T:cflonum)
(declare-unsafe-unary-operation $sin-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $cos-fixnum		T:fixnum	T:real)
(declare-unsafe-unary-operation $cos-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $cos-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $cos-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $cos-compnum		T:compnum	T:cflonum)
(declare-unsafe-unary-operation $cos-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $tan-fixnum		T:fixnum	T:real)
(declare-unsafe-unary-operation $tan-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $tan-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $tan-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $tan-compnum		T:compnum	T:cflonum)
(declare-unsafe-unary-operation $tan-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $asin-fixnum		T:fixnum	T:inexact)
(declare-unsafe-unary-operation $asin-bignum		T:bignum	T:inexact)
(declare-unsafe-unary-operation $asin-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $asin-ratnum		T:ratnum	T:inexact)
(declare-unsafe-unary-operation $asin-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $asin-cflonum		T:cflonum	T:inexact)

(declare-unsafe-unary-operation $acos-fixnum		T:fixnum	T:inexact)
(declare-unsafe-unary-operation $acos-bignum		T:bignum	T:inexact)
(declare-unsafe-unary-operation $acos-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $acos-ratnum		T:ratnum	T:inexact)
(declare-unsafe-unary-operation $acos-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $acos-cflonum		T:cflonum	T:inexact)

(declare-unsafe-binary-operation $atan2-real-real	T:real		T:real	T:flonum)

(declare-unsafe-unary-operation $atan-fixnum		T:fixnum	T:inexact)
(declare-unsafe-unary-operation $atan-bignum		T:bignum	T:inexact)
(declare-unsafe-unary-operation $atan-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $atan-ratnum		T:ratnum	T:inexact)
(declare-unsafe-unary-operation $atan-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $atan-cflonum		T:cflonum	T:inexact)

(declare-unsafe-unary-operation $sinh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $sinh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $sinh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $sinh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $sinh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $sinh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $cosh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $cosh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $cosh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $cosh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $cosh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $cosh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $tanh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $tanh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $tanh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $tanh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $tanh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $tanh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $asinh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $asinh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $asinh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $asinh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $asinh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $asinh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $acosh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $acosh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $acosh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $acosh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $acosh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $acosh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $atanh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $atanh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $atanh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $atanh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $atanh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $atanh-cflonum		T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $bitwise-not-fixnum		T:fixnum	T:fixnum)
(declare-unsafe-unary-operation $bitwise-not-bignum		T:bignum	T:bignum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-and-fixnum-number	T:fixnum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-and-fixnum-fixnum	T:fixnum	T:fixnum	      T:fixnum)
(declare-unsafe-binary-operation $bitwise-and-fixnum-bignum	T:fixnum	T:bignum	      T:exact-integer)

(declare-unsafe-binary-operation $bitwise-and-bignum-number	T:bignum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-and-bignum-fixnum	T:bignum	T:fixnum	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-and-bignum-bignum	T:bignum	T:bignum	      T:exact-integer)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-ior-fixnum-number	T:fixnum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-ior-fixnum-fixnum	T:fixnum	T:fixnum	      T:fixnum)
(declare-unsafe-binary-operation $bitwise-ior-fixnum-bignum	T:fixnum	T:bignum	      T:exact-integer)

(declare-unsafe-binary-operation $bitwise-ior-bignum-number	T:bignum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-ior-bignum-fixnum	T:bignum	T:fixnum	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-ior-bignum-bignum	T:bignum	T:bignum	      T:exact-integer)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-xor-fixnum-number	T:fixnum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-xor-fixnum-fixnum	T:fixnum	T:fixnum	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-xor-fixnum-bignum	T:fixnum	T:bignum	      T:exact-integer)

(declare-unsafe-binary-operation $bitwise-xor-bignum-number	T:bignum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-xor-bignum-fixnum	T:bignum	T:fixnum	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-xor-bignum-bignum	T:bignum	T:bignum	      T:exact-integer)

;;; --------------------------------------------------------------------

(let-syntax
    ((declare-unsafe-rounding-primitives
       (syntax-rules ()
	 ((_ ?who-fixnum ?who-bignum ?who-flonum ?who-ratnum)
	  (begin
	    (declare-core-primitive ?who-fixnum
		(unsafe)
	      (signatures
	       ((T:fixnum)			=> (T:fixnum)))
	      (attributes
	       ((_)				foldable effect-free result-true identity)))
	    (declare-core-primitive ?who-bignum
		(unsafe)
	      (signatures
	       ((T:bignum)			=> (T:bignum)))
	      (attributes
	       ((_)				foldable effect-free result-true identity)))
	    (declare-core-primitive ?who-flonum
		(unsafe)
	      (signatures
	       ((T:flonum)			=> (T:flonum)))
	      (attributes
	       ((_)				foldable effect-free result-true)))
	    (declare-core-primitive ?who-ratnum
		(unsafe)
	      (signatures
	       ((T:ratnum)			=> (T:exact-integer)))
	      (attributes
	       ((_)				foldable effect-free result-true)))))
	 )))
  (declare-unsafe-rounding-primitives $floor-fixnum $floor-bignum $floor-flonum $floor-ratnum)
  (declare-unsafe-rounding-primitives $ceiling-fixnum $ceiling-bignum $ceiling-flonum $ceiling-ratnum)
  (declare-unsafe-rounding-primitives $truncate-fixnum $truncate-bignum $truncate-flonum $truncate-ratnum)
  (declare-unsafe-rounding-primitives $round-fixnum $round-bignum $round-flonum $round-ratnum)
  #| end of LET-SYNTAX |# )


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
