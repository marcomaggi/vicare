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
(library (ikarus.compiler.core-primitive-properties)
  (export
    initialise-core-primitive-properties

    core-primitive-name->core-type-tag
    core-primitive-name->application-attributes*
    core-primitive-name->core-type-signature*
    core-primitive-name->replacement*
    core-type-tag?
    core-type-tag-is-a?
    core-type-tag-matches-any-object?
    tuple-tags-arity
    tuple-tags-rest-objects-tag
    tuple-tags-ref
    application-attributes-operands-template
    application-attributes-foldable?
    application-attributes-effect-free?
    application-attributes-result-true?
    application-attributes-result-false?
    application-attributes-identity?
    CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES)
  (import (except (vicare) unsafe)
    (ikarus.compiler.scheme-objects-ontology)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.core-primitive-properties.configuration)
    ;;
    (ikarus.compiler.core-primitive-properties.characters)
    (ikarus.compiler.core-primitive-properties.booleans)
    ;;
    (ikarus.compiler.core-primitive-properties.fixnums)
    (ikarus.compiler.core-primitive-properties.bignums)
    (ikarus.compiler.core-primitive-properties.ratnums)
    (ikarus.compiler.core-primitive-properties.flonums)
    (ikarus.compiler.core-primitive-properties.cflonums)
    (ikarus.compiler.core-primitive-properties.compnums)
    ;;
    (ikarus.compiler.core-primitive-properties.code-objects)
    (ikarus.compiler.core-primitive-properties.strings)
    (ikarus.compiler.core-primitive-properties.symbols)
    (ikarus.compiler.core-primitive-properties.keywords)
    (ikarus.compiler.core-primitive-properties.pointers)
    (ikarus.compiler.core-primitive-properties.bytevectors)
    ;;
    (ikarus.compiler.core-primitive-properties.pairs-and-lists)
    (ikarus.compiler.core-primitive-properties.vectors)
    (ikarus.compiler.core-primitive-properties.structs)
    (ikarus.compiler.core-primitive-properties.records)
    (ikarus.compiler.core-primitive-properties.hash-tables)
    ;;
    (ikarus.compiler.core-primitive-properties.annotation-objects)
    (ikarus.compiler.core-primitive-properties.enum-sets)
    (ikarus.compiler.core-primitive-properties.condition-objects)
    (ikarus.compiler.core-primitive-properties.transcoder-objects)
    ;;
    (ikarus.compiler.core-primitive-properties.control)
    (ikarus.compiler.core-primitive-properties.generic-primitives)
    (ikarus.compiler.core-primitive-properties.input-output)
    (ikarus.compiler.core-primitive-properties.environment-inquiry)
    (ikarus.compiler.core-primitive-properties.numerics)
    (ikarus.compiler.core-primitive-properties.times-and-dates)
    (ikarus.compiler.core-primitive-properties.library-utils)
    (ikarus.compiler.core-primitive-properties.expander)
    (ikarus.compiler.core-primitive-properties.eval-and-environments)
    (ikarus.compiler.core-primitive-properties.ffi)
    (ikarus.compiler.core-primitive-properties.posix))

  (import SCHEME-OBJECTS-ONTOLOGY)


;;;; initialisation

(define (initialise-core-primitive-properties)
  (initialise-core-primitive-properties/configuration)
  ;;
  (initialise-core-primitive-properties/booleans)
  (initialise-core-primitive-properties/characters)
  ;;
  (initialise-core-primitive-properties/fixnums)
  (initialise-core-primitive-properties/bignums)
  (initialise-core-primitive-properties/ratnums)
  (initialise-core-primitive-properties/flonums)
  (initialise-core-primitive-properties/cflonums)
  (initialise-core-primitive-properties/compnums)
  ;;
  (initialise-core-primitive-properties/code-objects)
  (initialise-core-primitive-properties/strings)
  (initialise-core-primitive-properties/symbols)
  (initialise-core-primitive-properties/keywords)
  (initialise-core-primitive-properties/pointers)
  (initialise-core-primitive-properties/bytevectors)
  ;;
  (initialise-core-primitive-properties/pairs-and-lists)
  (initialise-core-primitive-properties/vectors)
  (initialise-core-primitive-properties/structs)
  (initialise-core-primitive-properties/records)
  (initialise-core-primitive-properties/hash-tables)
  ;;
  (initialise-core-primitive-properties/annotation-objects)
  (initialise-core-primitive-properties/enum-sets)
  (initialise-core-primitive-properties/condition-objects)
  (initialise-core-primitive-properties/transcoder-objects)
  ;;
  (initialise-core-primitive-properties/control)
  (initialise-core-primitive-properties/generic-primitives)
  (initialise-core-primitive-properties/input-output)
  (initialise-core-primitive-properties/environment-inquiry)
  (initialise-core-primitive-properties/numerics)
  (initialise-core-primitive-properties/times-and-dates)
  (initialise-core-primitive-properties/library-utils)
  (initialise-core-primitive-properties/expander)
  (initialise-core-primitive-properties/eval-and-environments)
  (initialise-core-primitive-properties/ffi)
  (initialise-core-primitive-properties/posix)
  #| end of define |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; eval: (put 'declare-core-primitive		'scheme-indent-function 2)
;; eval: (put 'declare-pair-accessor		'scheme-indent-function 1)
;; eval: (put 'declare-pair-mutator		'scheme-indent-function 1)
;; eval: (put 'declare-number-unary		'scheme-indent-function 1)
;; eval: (put 'declare-number-binary		'scheme-indent-function 1)
;; eval: (put 'declare-number-unary/binary	'scheme-indent-function 1)
;; eval: (put 'declare-number-binary/multi-comparison	'scheme-indent-function 1)
;; End:
