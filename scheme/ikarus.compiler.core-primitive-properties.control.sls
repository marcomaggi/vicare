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
(library (ikarus.compiler.core-primitive-properties.control)
  (export initialise-core-primitive-properties/control)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/control)


;;;; calling procedures and returning

(declare-core-primitive apply
    (safe)
  (signatures
   ((T:procedure . _)		=> T:object)))

(declare-core-primitive values
    (safe)
  (signatures
   (T:object		=> T:object))
  (attributes
   (_			effect-free)))

(declare-core-primitive call-with-current-continuation
    (safe)
  (signatures
   ((T:procedure)	=> T:object)))

(declare-core-primitive call/cc
    (safe)
  (signatures
   ((T:procedure)	=> T:object)))

(declare-core-primitive call-with-values
    (safe)
  (signatures
   ((T:procedure T:procedure)	=> T:object)))


;;;; exceptions and dynamic environment, safe procedures

(declare-core-primitive with-exception-handler
    (safe)
  (signatures
   ((T:procedure T:procedure)	=> T:object)))

(declare-core-primitive dynamic-wind
    (safe)
  (signatures
   ((T:procedure T:procedure T:procedure)	=> T:object)))

;;; --------------------------------------------------------------------

(declare-core-primitive raise
    (safe)
  (signatures
   ((T:object)		=> T:object)))

(declare-core-primitive raise-continuable
    (safe)
  (signatures
   ((T:object)		=> T:object)))


;;;; other primitives

(declare-core-primitive make-guardian
    (safe)
  (signatures
   (()				=> (T:procedure)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive make-parameter
    (safe)
  (signatures
   ((T:object)			=> (T:procedure))
   ((T:object T:procedure)	=> (T:procedure)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))


;;;; compensations, safe primitives

(declare-core-primitive run-compensations
    (safe)
  (signatures
   (()			=> (T:void)))
  (attributes
   (()			result-true)))

(declare-core-primitive push-compensation-thunk
    (safe)
  (signatures
   ((T:procedure)	=> (T:void)))
  (attributes
   ((_)			result-true)))


;;;; invocation and termination procedures

(declare-object-retriever command-line	T:non-empty-proper-list)

(declare-core-primitive exit
    (safe)
  (signatures
   (()				=> (T:void))
   ((T:fixnum)			=> (T:void))))

(declare-parameter exit-hooks	T:proper-list)


;;;; promises, safe primitives

(declare-type-predicate promise?	T:promise)

(declare-core-primitive make-promise
    (safe)
  (signatures
   ((T:procedure)	=> (T:promise)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive force
    (safe)
  (signatures
   ((T:promise)		=> T:object)))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
