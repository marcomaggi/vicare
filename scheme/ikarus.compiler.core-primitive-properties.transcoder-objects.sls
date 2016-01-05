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
(library (ikarus.compiler.core-primitive-properties.transcoder-objects)
  (export initialise-core-primitive-properties/transcoder-objects)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/transcoder-objects)


;;;; transcoders, safe primitives

;;; predicates

(declare-type-predicate transcoder? T:transcoder)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-transcoder
    (safe)
  (signatures
   ((T:symbol)				=> (T:transcoder))
   ((T:symbol T:symbol)			=> (T:transcoder))
   ((T:symbol T:symbol T:symbol)	=> (T:transcoder)))
  (attributes
   ;;Not foldable because transcoders are not representable in FASL files.
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-parameter native-transcoder	T:transcoder)

;;; --------------------------------------------------------------------
;;; accessors

(let-syntax
    ((declare-transcoder-accessor
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:transcoder)	=> (T:symbol)))
	   (attributes
	    ((_)		foldable effect-free result-true))))
	)))
  (declare-transcoder-accessor transcoder-codec)
  (declare-transcoder-accessor transcoder-eol-style)
  (declare-transcoder-accessor transcoder-error-handling-mode)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; codec values

(declare-object-retriever latin-1-codec		foldable	T:symbol)
(declare-object-retriever utf-8-codec		foldable	T:symbol)
(declare-object-retriever utf-16-codec		foldable	T:symbol)
(declare-object-retriever utf-16le-codec	foldable	T:symbol)
(declare-object-retriever utf-16be-codec	foldable	T:symbol)
(declare-object-retriever utf-16n-codec		foldable	T:symbol)
(declare-object-retriever utf-bom-codec		foldable	T:symbol)

(declare-object-retriever native-eol-style	foldable	T:symbol)
(declare-object-retriever native-endianness	foldable	T:symbol)


;;;; transcoders, unsafe primitives

(declare-core-primitive $data->transcoder
    (unsafe)
  (signatures
   ((T:fixnum)			=> (T:transcoder)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive $transcoder->data
    (unsafe)
  (signatures
   ((T:transcoder)		=> (T:fixnum)))
  (attributes
   ((_)				effect-free result-true)))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
