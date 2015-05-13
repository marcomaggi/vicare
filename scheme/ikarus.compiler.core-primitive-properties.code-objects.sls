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
(library (ikarus.compiler.core-primitive-properties.code-objects)
  (export initialise-core-primitive-properties/code-objects)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/code-objects)


;;;; codes and closures, unsafe primitives

(declare-core-primitive $closure-code
    (safe)
  (signatures
   ((T:procedure)	=> (T:code)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $cpref
    (safe)
  (signatures
   ((T:procedure T:non-negative-fixnum)		=> (T:object)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive $make-annotated-procedure
    (safe)
  (signatures
   ((T:object T:procedure)	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive $annotated-procedure-annotation
    (safe)
  (signatures
   ((T:procedure)	=> (T:object)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code->closure
    (safe)
  (signatures
   ((T:code)		=> (T:procedure)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-reloc-vector
    (safe)
  (signatures
   ((T:code)		=> (T:object)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-freevars
    (safe)
  (signatures
   ((T:code)		=> (T:non-negative-fixnum)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-size
    (safe)
  (signatures
   ((T:code)		=> (T:non-negative-exact-integer)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code-annotation
    (safe)
  (signatures
   ((T:code)		=> (T:object)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive $set-code-annotation!
    (safe)
  (signatures
   ((T:code T:object)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code-ref
    (safe)
  (signatures
   ((T:code T:non-negative-exact-integer)	=> (T:octet)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive $code-set!
    (safe)
  (signatures
   ((T:code T:non-negative-exact-integer T:octet)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
