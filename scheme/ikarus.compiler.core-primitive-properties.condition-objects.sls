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
(library (ikarus.compiler.core-primitive-properties.condition-objects)
  (export initialise-core-primitive-properties/condition-objects)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/condition-objects)


;;;; condition objects, safe procedures

;;; generic condition procedures

(declare-type-predicate condition? T:condition)

(declare-core-primitive condition
    (safe)
  (signatures
   (T:condition		=> (T:condition)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive simple-conditions
    (safe)
  (signatures
   ((T:condition)		=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive condition-predicate
    (safe)
  (signatures
   ((T:record-type-descriptor)		=> (T:procedure)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive condition-accessor
    (safe)
  (signatures
   ((T:record-type-descriptor T:procedure)		=> (T:procedure))
   ((T:record-type-descriptor T:procedure T:symbol)	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-core-primitive print-condition
    (safe)
  (signatures
   ((T:condition T:textual-output-port)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------
;;; constructors

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    (()			=> (T:condition)))
		   (attributes
		    (()			effect-free result-true))))
		)))
  (declare make-assertion-violation)
  (declare make-error)
  (declare make-expression-return-value-violation)
  (declare make-i/o-eagain)
  (declare make-i/o-error)
  (declare make-i/o-read-error)
  (declare make-i/o-write-error)
  (declare make-implementation-restriction-violation)
  (declare make-interrupted-condition)
  (declare make-lexical-violation)
  (declare make-no-infinities-violation)
  (declare make-no-nans-violation)
  (declare make-non-continuable-violation)
  (declare make-procedure-argument-violation)
  (declare make-serious-condition)
  (declare make-undefined-violation)
  (declare make-violation)
  (declare make-warning)
  #| end of LET-SYNTAX |# )

(declare-core-primitive make-who-condition
    (safe)
  (signatures
   (([or T:false T:string T:symbol])	=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-message-condition
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-irritants-condition
    (safe)
  (signatures
   (_			=> (T:condition)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive make-syntax-violation
    (safe)
  (signatures
   ((_ _)		=> (T:condition)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive make-errno-condition
    (safe)
  (signatures
   ((T:fixnum)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-h_errno-condition
    (safe)
  (signatures
   ((T:fixnum)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))


(declare-core-primitive make-i/o-port-error
    (safe)
  (signatures
   ((T:port)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-invalid-position-error
    (safe)
  (signatures
   ((T:object)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-filename-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-protection-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-decoding-error
    (safe)
  (signatures
   ((T:port)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-encoding-error
    (safe)
  (signatures
   ((T:port T:char)	=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-already-exists-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-does-not-exist-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-is-read-only-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-source-position-condition
    (safe)
  (signatures
   ((T:string T:exact-integer T:exact-integer T:exact-integer T:exact-integer)
    => (T:condition)))
  (attributes
   ((_ _ _ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((_)		=> (T:boolean)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare assertion-violation?)
  (declare errno-condition?)
  (declare error?)
  (declare expression-return-value-violation?)
  (declare h_errno-condition?)
  (declare i/o-decoding-error?)
  (declare i/o-eagain-error?)
  (declare i/o-encoding-error?)
  (declare i/o-error?)
  (declare i/o-file-already-exists-error?)
  (declare i/o-file-does-not-exist-error?)
  (declare i/o-file-is-read-only-error?)
  (declare i/o-file-protection-error?)
  (declare i/o-filename-error?)
  (declare i/o-invalid-position-error?)
  (declare i/o-port-error?)
  (declare i/o-read-error?)
  (declare i/o-write-error?)
  (declare implementation-restriction-violation?)
  (declare interrupted-condition?)
  (declare irritants-condition?)
  (declare lexical-violation?)
  (declare message-condition?)
  (declare no-infinities-violation?)
  (declare no-nans-violation?)
  (declare non-continuable-violation?)
  (declare procedure-argument-violation?)
  (declare serious-condition?)
  (declare source-position-condition?)
  (declare syntax-violation?)
  (declare undefined-violation?)
  (declare violation?)
  (declare warning?)
  (declare who-condition?)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ?who T:object))
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:condition)	=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare condition-errno		T:fixnum)
  (declare condition-h_errno		T:fixnum)
  (declare condition-irritants)
  (declare condition-message		T:string)
  (declare condition-who		[or T:false T:string T:symbol])
  (declare i/o-encoding-error-char)
  (declare i/o-error-filename)
  (declare i/o-error-port		T:port)
  (declare i/o-error-position)
  (declare syntax-violation-form)
  (declare syntax-violation-subform)

  (declare source-position-port-id	T:string)
  (declare source-position-byte		T:exact-integer)
  (declare source-position-character	T:exact-integer)
  (declare source-position-line		T:exact-integer)
  (declare source-position-column	T:exact-integer)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

;;These are not effect-free because raising an exception is a "side effect".

(declare-core-primitive error
    (safe)
  (signatures
   (([or T:false T:symbol T:string] T:string . T:object)	=> (T:void))))

(declare-core-primitive assertion-violation
    (safe)
  (signatures
   (([or T:false T:symbol T:string] T:string . T:object)	=> (T:void))))

(declare-core-primitive syntax-violation
    (safe)
  (signatures
   (([or T:false T:symbol T:string] T:string T:object)		=> (T:void))
   (([or T:false T:symbol T:string] T:string T:object T:object)	=> (T:void))))

(declare-core-primitive warning
    (safe)
  (signatures
   ;;We do not know the number of returned values.
   (([or T:false T:symbol T:string] T:string . T:object)	=> T:object)))

;;This is deprecated.
(declare-core-primitive die
    (safe)
  (signatures
   (([or T:false T:symbol T:string] T:string . T:object)	=> (T:void))))

(declare-core-primitive procedure-argument-violation
    (safe)
  (signatures
   (([or T:false T:string T:symbol] T:string . _)	=> (T:void))))

(declare-core-primitive expression-return-value-violation
    (safe)
  (signatures
   ;;FIXME This  is the  correct signature to  use at the  next boot  image rotation.
   ;;(Marco Maggi; Tue Feb 23, 2016)
   ;;
   ;;(([or T:false T:string T:symbol] T:string T:fixnum . _)	=> (T:void))
   (([or T:false T:string T:symbol] T:string . _)	=> (T:void))))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
