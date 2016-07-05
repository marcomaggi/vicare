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
(library (ikarus.compiler.core-primitive-properties.records)
  (export initialise-core-primitive-properties/records)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/records)


;;;; R6RS record type descriptors, safe primitives

(declare-type-predicate record-type-descriptor?	T:record-type-descriptor)

(declare-type-predicate record-constructor-descriptor? T:record-constructor-descriptor)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-record-type-descriptor
    (safe)
  (signatures
   ;;name parent uid sealed? opaque? fields
   ((T:symbol [or T:false T:record-type-descriptor] [or T:false T:symbol] _ _ T:vector)
    => (T:record-type-descriptor)))
  (attributes
   ((_ _ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-record-constructor-descriptor
    (safe)
  (signatures
   ((T:record-type-descriptor [or T:false T:record-constructor-descriptor] [or T:false T:procedure])
    => (T:record-constructor-descriptor)))
  (attributes
   ((_ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; procedures generation

(declare-core-primitive record-constructor
    (safe)
  (signatures
   ((T:record-constructor-descriptor)	=> (T:procedure)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive record-predicate
    (safe)
  (signatures
   ((T:record-type-descriptor)		=> (T:procedure)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive record-accessor
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:false)	=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:symbol)	=> (T:procedure))
   ((T:record-type-descriptor T:symbol)				=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:false)			=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:symbol)		=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive record-mutator
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:false)	=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:symbol)	=> (T:procedure))
   ((T:record-type-descriptor T:symbol)				=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:false)			=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:symbol)		=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive unsafe-record-accessor
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:false)	=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:symbol)	=> (T:procedure))
   ((T:record-type-descriptor T:symbol)				=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:false)			=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:symbol)		=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive unsafe-record-mutator
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:false)	=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:symbol)	=> (T:procedure))
   ((T:record-type-descriptor T:symbol)				=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:false)			=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:symbol)		=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive record-field-mutable?
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:boolean)))
  (attributes
   ((_ _)		effect-free)))

;;;

(declare-core-primitive record-type-generative?
    (safe)
  (signatures
   ((T:record-type-descriptor)		=> (T:boolean)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive record-type-opaque?
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-sealed?
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-field-names
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> (T:vector)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive record-type-name
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> (T:symbol)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive record-type-parent
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> ([or T:false T:record-type-descriptor])))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-uid
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> ([or T:false T:symbol])))
  (attributes
   ((_)				effect-free)))

;;; --------------------------------------------------------------------
;;; destructor

(declare-core-primitive record-destructor
    (safe)
  (signatures
   ((T:record-type-descriptor)		=> ([or T:false T:procedure])))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive record-destructor-set!
    (safe)
  (signatures
   ((T:record-type-descriptor T:procedure)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))


;;;; R6RS records, safe primitives

(declare-type-predicate record? T:record)

(declare-type-predicate record-object? T:record)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive record-and-rtd?
    (safe)
  (signatures
   ((T:record T:record-type-descriptor)		=> (T:boolean)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive record-rtd
    (safe)
  (signatures
   ((T:record)		=> (T:record-type-descriptor)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive record-reset
    (safe)
  (signatures
   ((T:record)		=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive record-guardian-log
    (safe)
  (signatures
   ((T:record _ T:symbol)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))

(declare-parameter record-guardian-logger	[or T:boolean T:procedure])


;;;; R6RS records, unsafe primitives

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $record-guardian
    (unsafe)
  (signatures
   ((T:record)		=> (T:void))))



;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
