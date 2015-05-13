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
(library (ikarus.compiler.core-primitive-properties.hash-tables)
  (export initialise-core-primitive-properties/hash-tables)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/hash-tables)


;;;; hashtables, safe procedures

;;; predicates

(declare-type-predicate hashtable? T:hashtable)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-hashtable
    (safe)
  (signatures
   ((T:procedure T:procedure)			=> (T:hashtable))
   ((T:procedure T:procedure T:exact-integer)	=> (T:hashtable)))
  (attributes
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive make-eq-hashtable
    (safe)
  (signatures
   (()					=> (T:hashtable))
   ((T:exact-integer)			=> (T:hashtable)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive make-eqv-hashtable
    (safe)
  (signatures
   (()					=> (T:hashtable))
   ((T:exact-integer)			=> (T:hashtable)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive hashtable-copy
    (safe)
  (signatures
   ((T:hashtable)			=> (T:hashtable))
   ((T:hashtable T:object)		=> (T:hashtable)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive hashtable-ref
    (safe)
  (signatures
   ((T:hashtable T:object	)	=> (T:object))
   ((T:hashtable T:object T:object)	=> (T:object)))
  (attributes
   ((_ _)			effect-free)
   ((_ _ _)			effect-free)))

(declare-core-primitive hashtable-set!
    (safe)
  (signatures
   ((T:hashtable T:object T:object)	=> (T:void)))
  (attributes
   ((_ _ _)				result-true)))

(declare-core-primitive hashtable-delete!
    (safe)
  (signatures
   ((T:hashtable T:object)		=> (T:void)))
  (attributes
   ((_ _) 				result-true)))

(declare-core-primitive hashtable-clear!
    (safe)
  (signatures
   ((T:hashtable)			=> (T:void))
   ((T:hashtable T:exact-integer)	=> (T:void)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)))

(declare-core-primitive hashtable-update!
    (safe)
  (signatures
   ((T:hashtable T:object T:procedure T:object)	=> (T:void)))
  (attributes
   ((_ _ _ _)				result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive hashtable-contains?
    (safe)
  (signatures
   ((T:hashtable T:object)	=> (T:boolean)))
  (attributes
   ((_ _)			effect-free)))


(declare-core-primitive hashtable-entries
    (safe)
  (signatures
   ((T:hashtable)		=> (T:vector T:vector)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-keys
    (safe)
  (signatures
   ((T:hashtable)		=> (T:vector)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-mutable?
    (safe)
  (signatures
   ((T:hashtable)		=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-size
    (safe)
  (signatures
   ((T:hashtable)		=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive hashtable-hash-function
    (safe)
  (signatures
   ((T:hashtable)		=> (T:object)))
  (attributes
   ;;This returns false for EQ? and EQV? hashtables!!!
   ((_)				effect-free)))

(declare-core-primitive hashtable-equivalence-function
    (safe)
  (signatures
   ((T:hashtable)		=> (T:procedure)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; hash functions

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag . ?replacements)
			       (declare-core-primitive ?who
				   (safe)
				 (signatures
				  ((?obj-tag)		=> (T:fixnum)))
				 (attributes
				  ((_)			foldable effect-free result-true))
				 . ?replacements))
			      )))
  (declare-hash-function equal-hash		T:object)
  (declare-hash-function string-hash		T:string	(replacements $string-hash))
  (declare-hash-function string-ci-hash		T:string	(replacements $string-ci-hash))
  (declare-hash-function symbol-hash		T:symbol	(replacements $symbol-hash))
  (declare-hash-function bytevector-hash	T:bytevector	(replacements $bytevector-hash))
  (declare-hash-function port-hash		T:port)
  #| end of LET-SYNTAX |# )


;;;; hashtables, unsafe procedures

;;; hash functions

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag)
			       (declare-core-primitive ?who
				   (unsafe)
				 (signatures
				  ((?obj-tag)		=> (T:fixnum)))
				 (attributes
				  ((_)			foldable effect-free result-true))))
			      )))
  (declare-hash-function $string-hash		T:string)
  (declare-hash-function $string-ci-hash	T:string)
  (declare-hash-function $symbol-hash		T:symbol)
  (declare-hash-function $bytevector-hash	T:bytevector)
  #| end of LET-SYNTAX |# )


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
