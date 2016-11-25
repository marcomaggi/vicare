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
(library (ikarus.compiler.core-primitive-properties.symbols)
  (export initialise-core-primitive-properties/symbols)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/symbols)


;;;; symbols, safe primitives

(declare-type-predicate symbol? T:symbol)

(declare-core-primitive symbol->string
    (safe)
  (signatures
   ((T:symbol)			=> (T:string)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive symbol=?
    (safe)
  (signatures
   ((T:symbol T:symbol)		=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

;;; --------------------------------------------------------------------
;;; gensyms

(declare-type-predicate gensym?)

(declare-core-primitive gensym
    (safe)
  (signatures
   (()				=> (T:symbol))
   ((T:symbol)			=> (T:symbol))
   ((T:string)			=> (T:symbol)))
  (attributes
   ;;It must return a new gensym every time.
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive gensym->unique-string
    (safe)
  (signatures
   ((T:symbol)			=> (T:string)))
  (attributes
   ;;Once a  gensym has been  created, its unique  string is determined  forever.  So
   ;;this is foldable.
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive symbol-bound?
    (safe)
  (signatures
   ((T:symbol)			=> (T:boolean)))
  (attributes
   ;;Being bound or not is a run-time property; this is *not* foldable.
   ((_)				effect-free)))

(declare-core-primitive top-level-value
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive reset-symbol-proc!
    (safe)
  (signatures
   ((T:symbol)			=> (T:void)))
  (attributes
   ((_)				result-true)))

(declare-core-primitive set-symbol-value!
    (safe)
  (signatures
   ((T:symbol T:object)		=> (T:void)))
  (attributes
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive symbol-value
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive system-value
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive system-label
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive system-id
    (safe)
  (signatures
   ((T:symbol)			=> (T:object)))
  (attributes
   ((_)				effect-free)))

;;FIXME These  are variable  bindings.  We  should convert  them into  functions, for
;;uniformity of syntax.  (Marco Maggi; Thu Nov 20, 2014)
;;
;; (declare-core-primitive system-value-gensym
;;     (safe)
;;   (signatures
;;    (()				=> (T:symbol)))
;;   (attributes
;;    (()				effect-free result-true)))
;;
;; (declare-core-primitive system-label-gensym
;;     (safe)
;;   (signatures
;;    (()				=> (T:symbol)))
;;   (attributes
;;    (()				effect-free result-true)))
;;
;; (declare-core-primitive system-id-gensym
;;     (safe)
;;   (signatures
;;    (()				=> (T:symbol)))
;;   (attributes
;;    (()				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; property lists

(declare-core-primitive putprop
    (safe)
  (signatures
   ((T:symbol T:symbol _) => ()))
  (replacements $putprop))

(declare-core-primitive getprop
    (safe)
  (signatures
   ((T:symbol T:symbol) => (_)))
  (attributes
   ((_ _)		effect-free))
  (replacements $getprop))

(declare-core-primitive remprop
    (safe)
  (signatures
   ((T:symbol T:symbol) => ()))
  (replacements $remprop))

(declare-core-primitive property-list
    (safe)
  ;;The return value can be null or a pair.
  (signatures
   ((T:symbol) => (_)))
  (attributes
   ((_)			effect-free result-true))
  (replacements $property-list))

;;; --------------------------------------------------------------------
;;; printing gensyms

(declare-parameter print-gensym)
(declare-parameter gensym-count		T:exact-integer)
(declare-parameter gensym-prefix	T:string)


;;;; symbols, unsafe primitives

(declare-core-primitive $make-symbol
    (unsafe)
  (signatures
   ((T:false)			=> (T:symbol))
   ((T:string)			=> (T:symbol)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; components

(let-syntax
    ((declare-symbol-accessor (syntax-rules ()
				((_ ?who ?rv-tag)
				 (declare-core-primitive ?who
				     (unsafe)
				   (signatures
				    ((T:symbol)		=> (?rv-tag)))
				   (attributes
				    ((_)		effect-free))))
				)))
  (declare-symbol-accessor $symbol-plist		T:proper-list)
  (declare-symbol-accessor $symbol-proc			T:object)
  (declare-symbol-accessor $symbol-string		T:string)
  (declare-symbol-accessor $symbol-unique-string	T:string)
  (declare-symbol-accessor $symbol-value		T:object)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-symbol-mutator (syntax-rules ()
			       ((_ ?who ?obj-tag)
				(declare-core-primitive ?who
				    (unsafe)
				  (signatures
				   ((T:symbol ?obj-tag)	=> (T:void)))))
			       )))
  (declare-symbol-mutator $set-symbol-value!		T:object)
  (declare-symbol-mutator $set-symbol-proc!		T:object)
  (declare-symbol-mutator $set-symbol-string!		T:string)
  (declare-symbol-mutator $set-symbol-unique-string!	T:string/false)
  (declare-symbol-mutator $set-symbol-plist!		T:proper-list)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; property lists

(declare-core-primitive $putprop
    (unsafe)
  (signatures
   ((T:symbol T:symbol _) => (T:void)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive $getprop
    (unsafe)
  (signatures
   ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive $remprop
    (unsafe)
  (signatures
   ((T:symbol T:symbol) => (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive $property-list
    (unsafe)
  (signatures
   ((T:symbol) => (_)))
  (attributes
   ((_)			effect-free	result-true)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $symbol->string
    (unsafe)
  (signatures
   ((T:symbol)			=> (T:string)))
  (attributes
   ((_)				foldable effect-free result-true)))



;;;; done

#| end of DEFINE |# )

#| end of library |# )

;;; end o file
