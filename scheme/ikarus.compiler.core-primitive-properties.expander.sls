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
(library (ikarus.compiler.core-primitive-properties.expander)
  (export initialise-core-primitive-properties/expander)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/expander)


;;;; syntax-case, safe procedures

(declare-type-predicate syntax-object?	T:syntax-object)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:syntax-object)		=> (T:object)))
		   (attributes
		    ((_)			effect-free))))
		)))
  (declare stx-expr)
  (declare stx-mark*)
  (declare stx-rib*)
  (declare stx-annotated-expr*)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(declare-type-predicate identifier?	T:identifier)

(declare-core-primitive identifier-bound?
    (safe)
  (signatures
   ((T:identifier)	=> (T:boolean)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive bound-identifier=?
    (safe)
  (signatures
   ((T:identifier T:identifier)	=> (T:boolean)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive free-identifier=?
    (safe)
  (signatures
   ((T:identifier T:identifier)	=> (T:boolean)))
  (attributes
   ((_ _)		effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive generate-temporaries
    (safe)
  (signatures
   ((T:object)		=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive datum->syntax
    (safe)
  (signatures
   ((T:identifier T:object)	=> (T:syntax-object)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive syntax->datum
    (safe)
  (signatures
   ((T:object)		=> (T:object)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive make-variable-transformer
    (safe)
  (signatures
   ((T:procedure)	=> (T:object)))
  (attributes
   ((_)			effect-free result-true)))

(declare-object-predicate variable-transformer?)

(declare-core-primitive variable-transformer-procedure
    (safe)
  (signatures
   ((T:object)		=> (T:procedure)))
  (attributes
   ((_)			effect-free result-true)))

;;;

(declare-core-primitive make-synonym-transformer
    (safe)
  (signatures
   ((T:identifier)	=> (T:object)))
  (attributes
   ((_)			effect-free result-true)))

(declare-object-predicate synonym-transformer?)

(declare-core-primitive synonym-transformer-identifier
    (safe)
  (signatures
   ((T:object)		=> (T:identifier)))
  (attributes
   ((_)			effect-free result-true)))

;;;

(declare-core-primitive make-expand-time-value
    (safe)
  (signatures
   ((T:object)		=> (T:object)))
  (attributes
   ((_)			effect-free result-true)))

(declare-object-predicate expand-time-value?)

(declare-core-primitive expand-time-value-object
    (safe)
  (signatures
   ((T:object)		=> (T:object)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive retrieve-expand-time-value
    (safe)
  (signatures
   ((T:identifier)	=> (T:object)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive syntax-parameter-value
    (safe)
  (signatures
   ((T:identifier)	=> (T:object)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive syntactic-binding-putprop
    (safe)
  (signatures
   ((T:identifier T:symbol T:object)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive syntactic-binding-getprop
    (safe)
  (signatures
   ((T:identifier T:symbol)		=> (T:object)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive syntactic-binding-remprop
    (safe)
  (signatures
   ((T:identifier T:symbol)		=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive syntactic-binding-property-list
    (safe)
  (signatures
   ((T:identifier)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; syntax utilities

(declare-core-primitive identifier->string
    (safe)
  (signatures
   ((T:identifier)	=> (T:string)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive string->identifier
    (safe)
  (signatures
   ((T:identifier T:string)	=> (T:identifier)))
  (attributes
   ((_ _)		effect-free result-true)))

;;;

(declare-core-primitive identifier-prefix
    (safe)
  (signatures
   (([or T:string T:symbol T:identifier] T:identifier)		=> (T:identifier)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive identifier-suffix
    (safe)
  (signatures
   ((T:identifier [or T:string T:symbol T:identifier])		=> (T:identifier)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive identifier-append
    (safe)
  (signatures
   ((T:identifier . [or T:string T:symbol T:identifier])	=> (T:identifier)))
  (attributes
   ((_ . _)		effect-free result-true)))

(declare-core-primitive identifier-format
    (safe)
  (signatures
   ((T:identifier T:string . [or T:string T:symbol T:identifier])	=> (T:identifier)))
  (attributes
   ((_ _ . _)		effect-free result-true)))

;;;

(declare-core-primitive duplicate-identifiers?
    (safe)
  (signatures
   ((T:proper-list)			=> ([or T:false T:identifier]))
   ((T:proper-list T:procedure)		=> ([or T:false T:identifier])))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive delete-duplicate-identifiers
    (safe)
  (signatures
   ((T:proper-list)			=> (T:proper-list))
   ((T:proper-list T:procedure)		=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

;;;

(declare-core-primitive identifier-memq
    (safe)
  (signatures
   ((T:identifier T:proper-list)		=> ([or T:false T:proper-list]))
   ((T:identifier T:proper-list T:procedure)	=> ([or T:false T:proper-list])))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

;;;

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:identifier)	=> (T:identifier)))
		   (attributes
		    ((_)		effect-free result-true))))
		)))
  (declare identifier-record-constructor)
  (declare identifier-record-predicate)
  (declare identifier-struct-constructor)
  (declare identifier-struct-predicate)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:identifier [or T:string T:symbol T:identifier])	=> (T:identifier)))
		   (attributes
		    ((_)		effect-free result-true))))
		)))
  (declare identifier-record-field-accessor)
  (declare identifier-record-field-mutator)
  (declare identifier-struct-field-accessor)
  (declare identifier-struct-field-mutator)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(declare-core-primitive syntax-car
    (safe)
  (signatures
   ((T:syntax-object)			=> (T:syntax-object))
   ((T:syntax-object T:procedure)	=> (T:syntax-object)))
  (attributes
   ((_)			effect-free)
   ((_ _)		effect-free)))

(declare-core-primitive syntax-cdr
    (safe)
  (signatures
   ((T:syntax-object)			=> (T:syntax-object))
   ((T:syntax-object T:procedure)	=> (T:syntax-object)))
  (attributes
   ((_)			effect-free)
   ((_ _)		effect-free)))

(declare-core-primitive syntax->list
    (safe)
  (signatures
   ((T:syntax-object)			=> (T:proper-list))
   ((T:syntax-object T:procedure)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax->vector
    (safe)
  (signatures
   ((T:syntax-object)			=> (T:proper-list))
   ((T:syntax-object T:procedure)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive identifiers->list
    (safe)
  (signatures
   ((T:syntax-object)			=> (T:proper-list))
   ((T:syntax-object T:procedure)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax-unwrap
    (safe)
  (signatures
   ((T:object)		=> (T:object)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive all-identifiers?
    (safe)
  (signatures
   ;;The argument  can be  a wrapped  syntax object  or an  unwrapped list  of syntax
   ;;objects.
   ((T:object)		=> (T:boolean)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive syntax=?
    (safe)
  (signatures
   ((T:object T:object)		=> (T:boolean)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive identifier=symbol?
    (safe)
  (signatures
   ((T:identifier T:symbol)	=> (T:boolean)))
  (attributes
   ((_ _)		effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive syntax-clauses-unwrap
    (safe)
  (signatures
   ((T:object)			=> (T:proper-list))
   ((T:object T:procedure)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax-clauses-filter
    (safe)
  (signatures
   ((T:proper-list T:object)	=> (T:proper-list)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax-clauses-remove
    (safe)
  (signatures
   ((T:proper-list T:object)	=> (T:proper-list)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax-clauses-partition
    (safe)
  (signatures
   ((T:proper-list T:proper-list)	=> (T:proper-list T:proper-list)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive syntax-clauses-collapse
    (safe)
  (signatures
   ((T:proper-list)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

;;;

(declare-core-primitive syntax-clauses-verify-at-least-once
    (safe)
  (signatures
   ((T:proper-list T:proper-list)		=> (T:void))
   ((T:proper-list T:proper-list T:procedure)	=> (T:void)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive syntax-clauses-verify-at-most-once
    (safe)
  (signatures
   ((T:proper-list T:proper-list)		=> (T:void))
   ((T:proper-list T:proper-list T:procedure)	=> (T:void)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive syntax-clauses-verify-exactly-once
    (safe)
  (signatures
   ((T:proper-list T:proper-list)		=> (T:void))
   ((T:proper-list T:proper-list T:procedure)	=> (T:void)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive syntax-clauses-verify-mutually-inclusive
    (safe)
  (signatures
   ((T:proper-list T:proper-list)		=> (T:void))
   ((T:proper-list T:proper-list T:procedure)	=> (T:void)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive syntax-clauses-verify-mutually-exclusive
    (safe)
  (signatures
   ((T:proper-list T:proper-list)		=> (T:void))
   ((T:proper-list T:proper-list T:procedure)	=> (T:void)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

;;; --------------------------------------------------------------------
;;; clause specification structs

(declare-core-primitive make-syntax-clause-spec
    (safe)
  (signatures
   ((T:identifier [and T:real T:non-negative] [and T:real T:non-negative] [and T:real T:non-negative] [and T:real T:non-negative] T:proper-list T:proper-list)
    => (T:object))
   ((T:identifier [and T:real T:non-negative] [and T:real T:non-negative] [and T:real T:non-negative] [and T:real T:non-negative] T:proper-list T:proper-list T:object)
    => (T:object)))
  (attributes
   ((_ _ _ _ _ _ _)			effect-free result-true)
   ((_ _ _ _ _ _ _ _)			effect-free result-true)))

(declare-core-primitive syntax-clause-spec?
    (safe)
  (signatures
   ((T:object)		=> (T:boolean)))
  (attributes
   ((_)			effect-free)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:object)		=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare syntax-clause-spec-keyword			T:identifier)
  (declare syntax-clause-spec-min-number-of-occurrences	[and T:real T:non-negative])
  (declare syntax-clause-spec-max-number-of-occurrences	[and T:real T:non-negative])
  (declare syntax-clause-spec-min-number-of-arguments	[and T:real T:non-negative])
  (declare syntax-clause-spec-max-number-of-arguments	[and T:real T:non-negative])
  (declare syntax-clause-spec-mutually-inclusive	T:proper-list)
  (declare syntax-clause-spec-mutually-exclusive	T:proper-list)
  (declare syntax-clause-spec-custom-data		T:object)
  #| end of LET-SYNTAX |# )

(declare-core-primitive syntax-clauses-single-spec
    (safe)
  (signatures
   ((T:object T:proper-list)			=> (T:vector))
   ((T:object T:proper-list T:procedure)	=> (T:vector)))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-core-primitive syntax-clauses-fold-specs
    (safe)
  (signatures
   ((T:procedure T:object T:proper-list T:proper-list)			=> (T:object))
   ((T:procedure T:object T:proper-list T:proper-list T:procedure)	=> (T:object)))
  (attributes
   ((_ _ _)		effect-free)
   ((_ _ _ _)		effect-free)))

(declare-core-primitive syntax-clauses-validate-specs
    (safe)
  (signatures
   ((T:proper-list)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
;; Local Variables:
;; eval: (put 'declare-core-primitive	'scheme-indent-function 2)
;; End:
