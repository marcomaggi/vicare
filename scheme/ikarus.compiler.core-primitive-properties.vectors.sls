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
(library (ikarus.compiler.core-primitive-properties.vectors)
  (export initialise-core-primitive-properties/vectors)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/vectors)


;;;; vectors, safe functions
;;
;;According to R6RS:  VECTOR and MAKE-VECTOR must return a  newly allocated string at
;;every invocation; so they are not foldable.
;;

;;; predicates

(declare-type-predicate vector? T:vector)

(declare-vector-predicate vector-empty?)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive vector
    (safe)
  (signatures
   (()				=> (T:vector))
   (_				=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (()				effect-free result-true)
   (_				effect-free result-true)))

(declare-core-primitive subvector
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive make-vector
    (safe)
  (signatures
   ((T:non-negative-fixnum)	=> (T:vector))
   ((T:non-negative-fixnum _)	=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive vector-resize
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum)		=> (T:vector))
   ((T:vector T:non-negative-fixnum T:object)	=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive vector-append
    (safe)
  (signatures
   (T:vector			=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive vector-copy
    (safe)
  (signatures
   ((T:vector)			=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive vector-length
    (safe)
  (signatures
   ((T:vector)			=> (T:non-negative-fixnum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $vector-length))

;;; --------------------------------------------------------------------
;;; accessors and mutators

;;FIXME  This cannot  have $VECTOR-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-ref
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum)	=> (_)))
  (attributes
   ((_ _)		foldable effect-free)))

;;FIXME This  cannot have  $VECTOR-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-set!
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))

(declare-core-primitive vector-copy!
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum T:vector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:void)))
  ;;Not foldable  because it must return  a newly allocated vector.   Not effect free
  ;;because it mutates the operand.
  (attributes
   ((_ _ _ _ _)			result-true)))

(declare-core-primitive vector-fill!
    (safe)
  (signatures
   ((T:vector T:object)		=> (T:void)))
  ;;Not effect free because it mutates the operand.
  (attributes
   ((_ _)			foldable result-true)))

;;; --------------------------------------------------------------------
;;; sorting

(declare-core-primitive vector-sort
    (safe)
  (signatures
   ((T:procedure T:vector)	=> (T:vector)))
  (attributes
   ;;Not foldable  because it  must return  a new vector  at every  application.  Not
   ;;effect-free because it invokes an unknown procedure.
   ((_ _)			result-true)))

(declare-core-primitive vector-sort!
    (safe)
  (signatures
   ((T:procedure T:vector)	=> (T:void)))
  (attributes
   ;;Not foldable and not effect-free because  it invokes an unknown procedure and it
   ;;mutates the operand.
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive vector-for-each
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  vector at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-map
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:vector)))
  (attributes
   ;;Not foldable  because it  must return  a new vector  at every  application.  Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-for-all
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void))))

(declare-core-primitive vector-exists
    (safe)
  ;;Not foldable and not effect-free becuse it invokes an unknown procedure.
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void))))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive vector->list
    (safe)
  (signatures
   ((T:vector)			=> (T:proper-list)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_)				effect-free result-true)))


;;;; vectors, unsafe functions

;;; constructors

(declare-core-primitive $make-vector
    (unsafe)
  (signatures
   ((T:non-negative-fixnum)	=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-vector-predicate $vector-empty? unsafe)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $vector-length
    (unsafe)
  (signatures
   ((T:vector)			=> (T:non-negative-fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $vector-ref
    (unsafe)
  (signatures
   ((T:vector T:non-negative-fixnum)		=> (_)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $vector-set!
    (unsafe)
  (signatures
   ((T:vector T:non-negative-fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive $vector-map1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:vector)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-each1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-all1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:object))))

(declare-core-primitive $vector-exists1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:object))))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
