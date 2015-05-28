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
(library (ikarus.compiler.core-primitive-properties.pointers)
  (export initialise-core-primitive-properties/pointers)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/pointers)


;;;; pointers, safe functions

(declare-type-predicate       pointer?		T:pointer)
(declare-type-predicate/maybe maybe-pointer?	T:pointer)
(declare-type-predicate/list  list-of-pointers?	T:pointer)

;;; --------------------------------------------------------------------

(declare-core-primitive pointer-null?
    (safe)
  (signatures
   ((T:pointer)			=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive null-pointer
    (safe)
  (signatures
   (()				=> (T:pointer)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive set-pointer-null!
    (safe)
  (signatures
   ((T:pointer)			=> (T:void)))
  (attributes
   ((_)				result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive pointer-clone
    (safe)
  (signatures
   ((T:pointer)			=> (T:pointer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive pointer-diff
    (safe)
  (signatures
   ((T:pointer T:pointer)	=> (T:exact-integer)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive pointer-add
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:pointer)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive pointer-and-offset?
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:boolean)))
  (attributes
   ((_ _)			effect-free)))

;;; --------------------------------------------------------------------

(declare-pointer-binary-comparison pointer=?)
(declare-pointer-binary-comparison pointer!=?)
(declare-pointer-binary-comparison pointer<?)
(declare-pointer-binary-comparison pointer>?)
(declare-pointer-binary-comparison pointer<=?)
(declare-pointer-binary-comparison pointer>=?)

;;; --------------------------------------------------------------------

(declare-core-primitive pointer->integer
    (safe)
  (signatures
   ((T:pointer)			=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

;;This is not foldable because we do not want poiner objects to go in fasl files.
;;
(declare-core-primitive integer->pointer
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:pointer)))
  (attributes
   ((_)				effect-free result-true)))


;;;; pointers, unsafe functions

(declare-pointer-binary-comparison $pointer= unsafe)



;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
