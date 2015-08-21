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
(library (ikarus.compiler.core-primitive-properties.structs)
  (export initialise-core-primitive-properties/structs)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/structs)


;;;; structs, safe primitives

;;; predicates

(declare-core-primitive struct?
    (safe)
  (signatures
   ((T:struct)				=> (T:true))
   ((_)					=> (T:boolean))
   ((T:struct T:struct-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_)					foldable effect-free)
   ((_ _)				foldable effect-free)))

(declare-type-predicate struct-type-descriptor? T:struct-type-descriptor)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-struct-type
    (safe)
  (signatures
   ((T:string T:proper-list)		=> (T:struct-type-descriptor))
   ((T:string T:proper-list T:symbol)	=> (T:struct-type-descriptor)))
  (attributes
   ((_ _)		foldable effect-free result-true)
   ((_ _ _)		foldable effect-free result-true)))


;;; --------------------------------------------------------------------
;;; comparison

(declare-core-primitive struct=?
    (safe)
  (signatures
   ((T:struct T:struct)			=> (T:boolean)))
  (attributes
   ((_ _)		foldable effect-free)))

;;; --------------------------------------------------------------------
;;; struct type descriptor accessors and mutators

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:struct-type-descriptor)		=> (?return-value-tag)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare struct-type-name		T:string)
  (declare struct-type-symbol		T:symbol)
  (declare struct-type-field-names	T:proper-list)
  (declare struct-type-destructor	(or T:false T:procedure))
  (declare struct-constructor		T:procedure)
  (declare struct-predicate		T:procedure)
  #| end of LET-SYNTAX |# )

(declare-core-primitive struct-field-accessor
    (safe)
  (signatures
   ((T:struct-type-descriptor (or T:non-negative-fixnum T:symbol))	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive struct-field-mutator
    (safe)
  (signatures
   ((T:struct-type-descriptor (or T:non-negative-fixnum T:symbol))	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?new-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:struct-type-descriptor ?new-value-tag)	=> (T:void)))
		   (attributes
		    ((_ _)		result-true))))
		)))
  (declare set-rtd-printer!	T:procedure)
  (declare set-rtd-destructor!	T:procedure)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; struct instance accessors and mutators

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:struct)		=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare struct-rtd		T:struct-type-descriptor)
  (declare struct-name		T:string)
  (declare struct-length	T:non-negative-fixnum)
  (declare struct-printer	T:procedure)
  (declare struct-destructor	(or T:false T:procedure))
  #| end of LET-SYNTAX |# )

(declare-core-primitive struct-reset
    (safe)
  (signatures
   ((T:struct)		=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive struct-ref
    (safe)
  (signatures
   ((T:struct T:non-negative-fixnum)	=> (T:object)))
  (attributes
   ;;This cannot be foldable because the referenced field may be mutated at run-time.
   ((_ _)		effect-free)))

(declare-core-primitive struct-set!
    (safe)
  (signatures
   ((T:struct T:non-negative-fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-parameter struct-guardian-logger	(or T:boolean T:procedure))

(declare-core-primitive struct-guardian-log
    (safe)
  (signatures
   ((T:struct _ T:symbol)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))


;;;; structs, unsafe primitives

;;; constructors

;;The base struct type descriptor is a constant created at process boot time.
(declare-core-primitive base-rtd
    (unsafe)
  (signatures
   (()					=> (T:struct-type-descriptor)))
  (attributes
   (()					effect-free result-true)))

(declare-core-primitive $struct
    (unsafe)
  (signatures
   ((T:struct-type-descriptor . _)	=> (T:struct)))
  (attributes
   ;;It must return a new struct every time.
   ((_ . _)				effect-free result-true)))

(declare-core-primitive $make-struct
    (unsafe)
  (signatures
   ((T:struct-type-descriptor T:non-negative-fixnum)	=> (T:struct)))
  (attributes
   ;;Not foldable: it must return a new struct every time.
   ((_ _)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-core-primitive $struct?
    (unsafe)
  (signatures
   ((T:struct)				=> (T:true))
   ((_)					=> (T:boolean)))
  (attributes
   ((_)					foldable effect-free)))

(declare-core-primitive $struct/rtd?
    (unsafe)
  (signatures
   ((_ T:struct-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_ _)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $struct-rtd
    (unsafe)
  (signatures
   ((T:struct)			=> (T:struct-type-descriptor)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $struct-ref
    (unsafe)
  (signatures
   ((T:struct T:fixnum)		=> (_)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $struct-set!
    (unsafe)
  (signatures
   ((T:struct T:fixnum _)	=> (T:void)))
  (attributes
   ((_ _)			foldable result-true)))

;;;

(let-syntax
    ((declare-unsafe-struct-accessor
      (syntax-rules ()
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:struct-type-descriptor)	=> (?return-value-tag)))
	   (attributes
	    ((_)			foldable effect-free))))
	)))
  (declare-unsafe-struct-accessor $std-std		T:struct-type-descriptor)
  (declare-unsafe-struct-accessor $std-name		T:string)
  (declare-unsafe-struct-accessor $std-length		T:non-negative-fixnum)
  (declare-unsafe-struct-accessor $std-fields		T:proper-list)
  (declare-unsafe-struct-accessor $std-printer		T:object)
  (declare-unsafe-struct-accessor $std-symbol		T:object)
  (declare-unsafe-struct-accessor $std-destructor	T:object)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-unsafe-struct-mutator
      (syntax-rules ()
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:struct-type-descriptor ?new-value-tag)	=> (T:void)))))
	)))
  (declare-unsafe-struct-mutator $set-std-std!		T:struct-type-descriptor)
  (declare-unsafe-struct-mutator $set-std-name!		T:string)
  (declare-unsafe-struct-mutator $set-std-length!	T:non-negative-fixnum)
  (declare-unsafe-struct-mutator $set-std-fields!	T:proper-list)
  (declare-unsafe-struct-mutator $set-std-printer!	T:object)
  (declare-unsafe-struct-mutator $set-std-symbol!	T:object)
  (declare-unsafe-struct-mutator $set-std-destructor!	T:object)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $struct-guardian
    (unsafe)
  (signatures
   ((T:struct)		=> (T:void))))



;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
