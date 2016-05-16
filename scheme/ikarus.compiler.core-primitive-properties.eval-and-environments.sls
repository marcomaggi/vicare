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
(library (ikarus.compiler.core-primitive-properties.eval-and-environments)
  (export initialise-core-primitive-properties/eval-and-environments)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/eval-and-environments)


;;;; evaluation and lexical environments

(declare-core-primitive eval
    (safe)
  (signatures
   ((_ T:lexical-environment)					=> T:object)
   ((_ T:lexical-environment T:proper-list T:proper-list)	=> T:object)))

;;; --------------------------------------------------------------------

(declare-core-primitive environment
    (safe)
  (signatures
   (T:object			=> (T:lexical-environment)))
  (attributes
   ((_)				result-true)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:fixnum)		=> (T:lexical-environment)))
		   (attributes
		    (()			result-true))))
		)))
  (declare null-environment)
  (declare scheme-report-environment)
  #| end of LET-SYNTAX |# )

(declare-core-primitive interaction-environment
    (safe)
  (signatures
   (()				=> (T:lexical-environment))
   ((T:lexical-environment)	=> (T:void)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

(declare-core-primitive new-interaction-environment
    (safe)
  (signatures
   (()				=> (T:lexical-environment))
   ((T:proper-list)		=> (T:lexical-environment)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive environment?
    (safe)
  (signatures
   ((T:object)			=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

;;; --------------------------------------------------------------------

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:lexical-environment)	=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free result-true))))
		)))
  (declare environment-symbols		T:proper-list)
  (declare environment-libraries	T:library)
  (declare environment-labels		T:proper-list)
  #| end of LET-SYNTAX |# )

(declare-core-primitive environment-binding
    (safe)
  (signatures
   ((T:symbol T:lexical-environment)	=> ([or T:false T:symbol]
					    [or T:false T:proper-list])))
  (attributes
   ((_ _)		effect-free)))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
