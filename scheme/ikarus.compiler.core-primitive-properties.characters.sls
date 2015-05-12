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
(library (ikarus.compiler.core-primitive-properties.characters)
  (export initialise-core-primitive-properties/characters)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/characters)


;;;; characters safe operations

;;; predicates

(declare-type-predicate char? T:char)

(declare-char-predicate char-in-ascii-range?)
(declare-char-predicate char-alphabetic?)
(declare-char-predicate char-lower-case?)
(declare-char-predicate char-numeric?)
(declare-char-predicate char-title-case?)
(declare-char-predicate char-upper-case?)
(declare-char-predicate char-whitespace?)
(declare-char-predicate unicode-printable-char?)

;;; --------------------------------------------------------------------
;;; comparison

(declare-char-binary/multi-comparison char=?		(replacements $char=))
(declare-char-binary/multi-comparison char!=?		(replacements $char!=))
(declare-char-binary/multi-comparison char<?		(replacements $char<))
(declare-char-binary/multi-comparison char>?		(replacements $char>))
(declare-char-binary/multi-comparison char<=?		(replacements $char<=))
(declare-char-binary/multi-comparison char>=?		(replacements $char>=))

(declare-char-binary/multi-comparison char-ci=?)
(declare-char-binary/multi-comparison char-ci!=?)
(declare-char-binary/multi-comparison char-ci<?)
(declare-char-binary/multi-comparison char-ci>?)
(declare-char-binary/multi-comparison char-ci<=?)
(declare-char-binary/multi-comparison char-ci>=?)

;;; --------------------------------------------------------------------
;;; transformations

(declare-char-unary char-downcase)
(declare-char-unary char-foldcase)
(declare-char-unary char-titlecase)
(declare-char-unary char-upcase)
(declare-char-unary char-general-category)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive char->integer
    (safe)
  (signatures
   ((T:char)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; characters unsafe operations

;;; --------------------------------------------------------------------
;;; comparison

(declare-char-binary-comparison $char=		unsafe)
(declare-char-binary-comparison $char!=		unsafe)
(declare-char-binary-comparison $char>		unsafe)
(declare-char-binary-comparison $char<		unsafe)
(declare-char-binary-comparison $char>=		unsafe)
(declare-char-binary-comparison $char<=		unsafe)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $char->fixnum
    (unsafe)
  (signatures
   ((T:char)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))



;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
