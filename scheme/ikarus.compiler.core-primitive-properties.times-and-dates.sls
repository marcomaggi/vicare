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
(library (ikarus.compiler.core-primitive-properties.times-and-dates)
  (export initialise-core-primitive-properties/times-and-dates)
  (import (rnrs)
    ;;NOTE Here we must import only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/times-and-dates)


;;;; time and dates, safe functions

(declare-type-predicate time?	T:time)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-time
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)			=> (T:time))
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:time)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive current-time
    (safe)
  (signatures
   (()			=> (T:time)))
  (attributes
   (()			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive time-seconds
    (safe)
  (signatures
   ((T:time)		=> (T:exact-integer)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive time-nanoseconds
    (safe)
  (signatures
   ((T:time)		=> (T:exact-integer)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive time-gmt-offset
    (safe)
  (signatures
   (()			=> (T:exact-integer)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:time)				=> (T:boolean))
		    ((T:time T:time)			=> (T:boolean))
		    ((T:time T:time T:time . T:time)	=> (T:boolean)))
		   (attributes
		    ((_ _)		effect-free))))
		)))
  (declare time=?)
  (declare time!=?)
  (declare time<?)
  (declare time>?)
  (declare time<=?)
  (declare time>=?)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; operations

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:time)				=> (T:time))
		    ((T:time T:time)			=> (T:time))
		    ((T:time T:time T:time . T:time)	=> (T:time)))
		   (attributes
		    ((_ _)		effect-free))))
		)))
  (declare time-addition)
  (declare time-difference)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive date-string
    (safe)
  (signatures
   (()		=> (T:string)))
  (attributes
   (()		effect-free result-true)))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
