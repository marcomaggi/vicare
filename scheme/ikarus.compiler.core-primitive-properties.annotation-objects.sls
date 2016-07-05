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
(library (ikarus.compiler.core-primitive-properties.annotation-objects)
  (export initialise-core-primitive-properties/annotation-objects)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/annotation-objects)


;;;; annotations

(declare-core-primitive reader-annotation?
    (safe)
  (signatures
   ((T:other-struct)		=> (T:boolean)))
  (attributes
   ((#f)			foldable effect-free result-false)
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive get-annotated-datum
    (safe)
  (signatures
   ((T:input-port)		=> (_))))

(declare-core-primitive reader-annotation-expression
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))

(declare-core-primitive reader-annotation-stripped
    (safe)
  (signatures
   ((T:other-struct)		=> (_)))
  (attributes
   ((#f)			foldable effect-free result-false)))

(declare-core-primitive reader-annotation-textual-position
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))

(declare-core-primitive reader-annotation-source
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
