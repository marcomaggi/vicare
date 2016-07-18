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
;;;Copyright (C) 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.compiler.core-primitive-properties.ratnums)
  (export initialise-core-primitive-properties/ratnums)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/ratnums)


;;;; ratnums, safe operations

(declare-type-predicate ratnum? T:ratnum)


;;;; ratnums, unsafe operations

(declare-core-primitive $make-ratnum
    (unsafe)
  (signatures
   ((T:exact-integer T:exact-integer)		=> (T:ratnum)))
  (attributes
   ((_ _)					foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive $ratnum-n
    (unsafe)
  (signatures
   ((T:ratnum)					=> (T:exact-integer)))
  (attributes
   ((_)						foldable effect-free result-true)))

(declare-core-primitive $ratnum-d
    (unsafe)
  (signatures
   ((T:ratnum)					=> (T:exact-integer)))
  (attributes
   ((_)						foldable effect-free result-true)))

(declare-core-primitive $ratnum-num
    (unsafe)
  (signatures
   ((T:ratnum)					=> (T:exact-integer)))
  (attributes
   ((_)						foldable effect-free result-true)))

(declare-core-primitive $ratnum-den
    (unsafe)
  (signatures
   ((T:ratnum)					=> (T:exact-integer)))
  (attributes
   ((_)						foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-ratnum-predicate $ratnum-positive? unsafe)
(declare-ratnum-predicate $ratnum-negative? unsafe)
(declare-ratnum-predicate $ratnum-non-positive? unsafe)
(declare-ratnum-predicate $ratnum-non-negative? unsafe)

;;; --------------------------------------------------------------------

(declare-core-primitive $ratnum->flonum
    (unsafe)
  (signatures
   ((T:ratnum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))



;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
