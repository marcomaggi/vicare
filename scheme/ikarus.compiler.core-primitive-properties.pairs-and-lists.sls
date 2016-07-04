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
(library (ikarus.compiler.core-primitive-properties.pairs-and-lists)
  (export initialise-core-primitive-properties/pairs-and-lists)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/pairs-and-lists)


;;;; pairs and lists, safe functions

;;; predicates

(declare-core-primitive null?
    (safe)
  (signatures
   ((T:null)		=> (T:true))
   ((T:pair)		=> (T:false))
   ((_)			=> (T:boolean)))
  (attributes
   ((())		foldable effect-free result-true)
   ((_)			foldable effect-free)))

(declare-core-primitive pair?
    (safe)
  (signatures
   ((T:null)		=> (T:false))
   ((T:pair)		=> (T:true))
   ((_)			=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive list?
    (safe)
  (signatures
   ((T:proper-list)	=> (T:true))
   ((T:pair)		=> (T:boolean))
   ((_)			=> (T:false)))
  (attributes
   ((())		foldable effect-free result-true)
   ((_)			foldable effect-free)))

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive cons
    (safe)
  (signatures
   ((_ _)		=> (T:pair)))
  (attributes
   ;;This is not foldable because it must return a newly allocated pair every time.
   ((_ _)		effect-free result-true)))

(declare-core-primitive cons*
    (safe)
  (signatures
   ((_)			=> (_))
   ((_ _ . _)		=> (T:pair)))
  (attributes
   ;;This will return the operand itself, so it is foldable.
   ((_)			foldable effect-free)
   ;;This is not foldable because it must return a newly allocated list every time.
   ((_ _ . _)		effect-free result-true)))

(declare-core-primitive list
    (safe)
  (signatures
   (()			=> (T:null))
   ((_ . _)		=> (T:non-empty-proper-list)))
  (attributes
   ;;Foldable because it returns null.
   (()			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_ . _)		effect-free result-true)))

(declare-core-primitive make-list
    (safe)
  (signatures
   ((T:non-negative-fixnum)		=> (T:proper-list))
   ((T:non-negative-fixnum T:object)	=> (T:proper-list)))
  (attributes
   ;;Foldable because it returns null.
   ((0)				foldable effect-free result-true)
   ((0 _)			foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive reverse
    (safe)
  (signatures
   ((T:null)			=> (T:null))
   ((T:non-empty-proper-list)	=> (T:non-empty-proper-list)))
  (attributes
   ;;This is foldable because it returns null itself.
   ((())		foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated list every time.
   ((_)			effect-free result-true)))

(declare-core-primitive append
    (safe)
  (signatures
   (()				=> (T:null))
   ((T:object . T:object)	=> (T:improper-list)))
  (attributes
   ;;This is foldable because it returns null itself.
   (()				foldable effect-free result-true)
   ;;Not foldable because it must return a newly allocated improper list every time.
   ((_ . _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive length
    (safe)
  (signatures
   ((T:null)			=> (T:zero))
   ((T:non-empty-proper-list)	=> (_)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;;

(declare-list-finder memq T:object)
(declare-list-finder memv T:object)
(declare-list-finder member T:object)

(declare-core-primitive memp
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:false))
   ((T:procedure T:non-empty-proper-list)	=> (_)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

;;;

(declare-core-primitive remp
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:null))
   ((T:procedure T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ _)				result-true)))

(declare-core-primitive remq
    (safe)
  (signatures
   ((T:object T:null)			=> (T:null))
   ((T:object T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remv
    (safe)
  (signatures
   ((T:object T:null)			=> (T:null))
   ((T:object T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remove
    (safe)
  (signatures
   ((T:object T:null)			=> (T:null))
   ((T:object T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ((_ ())				foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)))

;;;

(declare-core-primitive last-pair
    (safe)
  (signatures
   ((T:non-empty-proper-list)		=> (T:pair))
   ((T:standalone-pair)			=> (T:standalone-pair)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive list-tail
    (safe)
  (signatures
   ((T:non-empty-proper-list T:exact-integer)	=> (T:proper-list)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive list-ref
    (safe)
  (signatures
   ((T:non-empty-proper-list T:exact-integer)	=> (T:object)))
  (attributes
   ((_ _)				foldable effect-free)))

;;;

(declare-core-primitive map
    (safe)
  (signatures
   ((T:procedure T:null . T:null)					=> (T:null))
   ((T:procedure T:non-empty-proper-list . T:non-empty-proper-list)	=> (T:non-empty-proper-list)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ _ . _)				result-true)))

(declare-core-primitive for-each
    (safe)
  (signatures
   ((T:procedure T:null . T:null)					=> (T:void))
   ((T:procedure T:non-empty-proper-list . T:non-empty-proper-list)	=> (T:void)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ _ . _)				result-true)))

;;;

(declare-core-primitive find
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:false))
   ((T:procedure T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

(declare-core-primitive exists
    (safe)
  (signatures
   ((T:procedure T:null . T:null)					=> (T:false))
   ((T:procedure T:non-empty-proper-list . T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

(declare-core-primitive for-all
    (safe)
  (signatures
   ((T:procedure T:null . T:null)					=> (T:true))
   ((T:procedure T:non-empty-proper-list . T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)))

(declare-core-primitive filter
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:null))
   ((T:procedure T:non-empty-proper-list)	=> (T:proper-list)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)))

(declare-core-primitive partition
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:null T:null))
   ((T:procedure T:non-empty-proper-list)	=> (T:proper-list T:proper-list)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free)))

(declare-core-primitive fold-left
    (safe)
  (signatures
   ((T:procedure T:object T:proper-list . T:proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ _ ())				foldable effect-free)))

(declare-core-primitive fold-right
    (safe)
  (signatures
   ((T:procedure T:object T:proper-list . T:proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ _ ())			foldable effect-free)))

(declare-core-primitive andmap
    (safe)
  (signatures
   ((T:procedure T:null)						=> (T:true))
   ((T:procedure T:non-empty-proper-list)				=> (T:object))
   ((T:procedure T:null T:null)						=> (T:true))
   ((T:procedure T:non-empty-proper-list T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case:  neither foldable  nor effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-true)
   ((_ () ())				foldable effect-free result-true)))

(declare-core-primitive ormap
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:false))
   ((T:procedure T:non-empty-proper-list)	=> (T:object)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free result-false)))

(declare-core-primitive list-sort
    (safe)
  (signatures
   ((T:procedure T:proper-list)		=> (T:proper-list)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-pair-accessor car
  (replacements $car))

(declare-pair-accessor cdr
  (replacements $cdr))

(declare-pair-accessor caar)
(declare-pair-accessor cadr)
(declare-pair-accessor cdar)
(declare-pair-accessor cddr)
(declare-pair-accessor caaar)
(declare-pair-accessor caadr)
(declare-pair-accessor cadar)
(declare-pair-accessor caddr)
(declare-pair-accessor cdaar)
(declare-pair-accessor cdadr)
(declare-pair-accessor cddar)
(declare-pair-accessor cdddr)
(declare-pair-accessor caaaar)
(declare-pair-accessor caaadr)
(declare-pair-accessor caadar)
(declare-pair-accessor caaddr)
(declare-pair-accessor cadaar)
(declare-pair-accessor cadadr)
(declare-pair-accessor caddar)
(declare-pair-accessor cadddr)
(declare-pair-accessor cdaaar)
(declare-pair-accessor cdaadr)
(declare-pair-accessor cdadar)
(declare-pair-accessor cdaddr)
(declare-pair-accessor cddaar)
(declare-pair-accessor cddadr)
(declare-pair-accessor cdddar)
(declare-pair-accessor cddddr)

;;; --------------------------------------------------------------------
;;; mutators

(declare-pair-mutator set-car!
  (replacements $set-car!))

(declare-pair-mutator set-cdr!
  (replacements $set-cdr!))

;;; --------------------------------------------------------------------
;;; associative lists

(declare-alist-accessor assq T:object)
(declare-alist-accessor assv T:object)
(declare-alist-accessor assoc T:object)

(declare-core-primitive assp
    (safe)
  (signatures
   ((T:procedure T:null)			=> (T:false))
   ((T:procedure T:non-empty-proper-list)	=> (_)))
  (attributes
   ;;In the  general case: neither  foldable nor  effect-free, because it  applies an
   ;;unknown function.
   ((_ ())				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; weak pairs

(declare-core-primitive weak-cons
    (safe)
  (signatures
   ((_ _)		=> (T:pair)))
  (attributes
   ;;This is not foldable because it must return a newly allocated pair every time.
   ((_ _)		effect-free result-true)))

(declare-core-primitive weak-pair?
    (safe)
  (signatures
   ((T:null)		=> (T:false))
   ((T:pair)		=> (T:boolean))
   ((_)			=> (T:boolean)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive list->string
    (safe)
  (signatures
   ((T:proper-list)		=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   ((_)				effect-free result-true)))

(declare-core-primitive list->vector
    (safe)
  (signatures
   ((T:proper-list)		=> (T:vector)))
  (attributes
   ;;Not foldable because it must return a new vector every time.
   ((_)				effect-free result-true)))

(let-syntax
    ((declare-list->bytevector-conversion
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:proper-list)		=> (T:bytevector)))
	   (attributes
	    ;;Not foldable because it must return a new bytevector every time.
	    ((_)				effect-free result-true))))
	)))
  (declare-list->bytevector-conversion c4b-list->bytevector)
  (declare-list->bytevector-conversion c4l-list->bytevector)
  (declare-list->bytevector-conversion c4n-list->bytevector)
  (declare-list->bytevector-conversion c8b-list->bytevector)
  (declare-list->bytevector-conversion c8l-list->bytevector)
  (declare-list->bytevector-conversion c8n-list->bytevector)
  (declare-list->bytevector-conversion f4b-list->bytevector)
  (declare-list->bytevector-conversion f4l-list->bytevector)
  (declare-list->bytevector-conversion f4n-list->bytevector)
  (declare-list->bytevector-conversion f8b-list->bytevector)
  (declare-list->bytevector-conversion f8l-list->bytevector)
  (declare-list->bytevector-conversion f8n-list->bytevector)
  (declare-list->bytevector-conversion s16b-list->bytevector)
  (declare-list->bytevector-conversion s16l-list->bytevector)
  (declare-list->bytevector-conversion s16n-list->bytevector)
  (declare-list->bytevector-conversion s32b-list->bytevector)
  (declare-list->bytevector-conversion s32l-list->bytevector)
  (declare-list->bytevector-conversion s32n-list->bytevector)
  (declare-list->bytevector-conversion s64b-list->bytevector)
  (declare-list->bytevector-conversion s64l-list->bytevector)
  (declare-list->bytevector-conversion s64n-list->bytevector)
  (declare-list->bytevector-conversion s8-list->bytevector)
  (declare-list->bytevector-conversion u16b-list->bytevector)
  (declare-list->bytevector-conversion u16l-list->bytevector)
  (declare-list->bytevector-conversion u16n-list->bytevector)
  (declare-list->bytevector-conversion u32b-list->bytevector)
  (declare-list->bytevector-conversion u32l-list->bytevector)
  (declare-list->bytevector-conversion u32n-list->bytevector)
  (declare-list->bytevector-conversion u64b-list->bytevector)
  (declare-list->bytevector-conversion u64l-list->bytevector)
  (declare-list->bytevector-conversion u64n-list->bytevector)
  (declare-list->bytevector-conversion u8-list->bytevector)
  #| end of LET-SYNTAX |# )

(declare-core-primitive sint-list->bytevector
    (safe)
  (signatures
   ((T:proper-list T:symbol T:positive-fixnum)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)				effect-free result-true)))

(declare-core-primitive uint-list->bytevector
    (safe)
  (signatures
   ((T:proper-list T:symbol T:positive-fixnum)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive make-queue-procs
    (safe)
  (signatures
   (()				=> (T:procedure T:procedure T:procedure))
   ((T:proper-list)		=> (T:procedure T:procedure T:procedure)))
  (attributes
   (()				effect-free)
   ((_)				effect-free)))


;;;; pairs and lists, unsafe functions

(declare-pair-accessor $car unsafe)
(declare-pair-accessor $cdr unsafe)

(declare-pair-mutator $set-car! unsafe)
(declare-pair-mutator $set-cdr! unsafe)

(declare-core-primitive $length
    (unsafe)
  (signatures
   ((T:proper-list)		=> (T:non-negative-fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; done

#| end of define |# )

#| end of library |# )

;;; end o file
