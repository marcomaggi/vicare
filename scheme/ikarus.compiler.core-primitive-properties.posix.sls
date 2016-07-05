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
(library (ikarus.compiler.core-primitive-properties.posix)
  (export initialise-core-primitive-properties/posix)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/posix)


;;;; POSIX API, safe primitives

(declare-parameter string->filename-func	T:procedure)
(declare-parameter string->pathname-func	T:procedure)

(declare-parameter filename->string-func	T:procedure)
(declare-parameter pathname->string-func	T:procedure)

;;; --------------------------------------------------------------------

(declare-object-predicate file-pathname?)
(declare-object-predicate file-string-pathname?)
(declare-object-predicate file-bytevector-pathname?)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:pathname)	=> (T:boolean)))
		   (attributes
		    ;;Not foldable.
		    ((_)		effect-free))))
		)))
  (declare directory-exists?)
  (declare file-exists?)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:pathname)	=> (T:boolean)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare file-absolute-pathname?)
  (declare file-relative-pathname?)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?argument-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((?argument-tag)	=> (T:boolean)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare file-colon-search-path?		[or T:string T:bytevector])
  (declare file-string-colon-search-path?	T:string)
  (declare file-bytevector-colon-search-path?	T:bytevector)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; file operations

(declare-core-primitive delete-file
    (safe)
  (signatures
   ((T:pathname)	=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive file-modification-time
    (safe)
  (signatures
   ((T:pathname)	=> (T:exact-integer)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive real-pathname
    (safe)
  (signatures
   ((T:pathname)	=> (T:pathname)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive mkdir
    (safe)
  (signatures
   ((T:pathname T:fixnum)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive mkdir/parents
    (safe)
  (signatures
   ((T:pathname T:fixnum)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive search-file-in-environment-path
    (safe)
  (signatures
   ((T:string T:string)		=> ([or T:false T:string])))
  (attributes
   ((_ _)			effect-free)))

(declare-core-primitive search-file-in-list-path
    (safe)
  (signatures
   ((T:string T:proper-list)	=> ([or T:false T:string])))
  (attributes
   ((_ _)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive split-pathname-root-and-tail
    (safe)
  (signatures
   ((T:string)		=> (T:string T:string)))
  (attributes
   ((_ _)		effect-free)))

;;;

(declare-core-primitive split-pathname
    (safe)
  (signatures
   ((T:pathname)	=> (T:boolean T:proper-list)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive split-pathname-bytevector
    (safe)
  (signatures
   ((T:bytevector)	=> (T:boolean T:proper-list)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive split-pathname-string
    (safe)
  (signatures
   ((T:string)		=> (T:boolean T:proper-list)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive split-search-path
    (safe)
  (signatures
   ((T:pathname)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive split-search-path-bytevector
    (safe)
  (signatures
   ((T:bytevector)	=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive split-search-path-string
    (safe)
  (signatures
   ((T:string)		=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; errors

(declare-core-primitive strerror
    (safe)
  (signatures
   (([or T:boolean T:fixnum])	=> (T:string)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive errno->string
    (safe)
  (signatures
   ((T:fixnum)		=> (T:string)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; environment

(declare-core-primitive getenv
    (safe)
  (signatures
   ((T:string)		=> ([or T:false T:string])))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive environ
    (safe)
  (signatures
   (()		=> (T:proper-list)))
  (attributes
   (()		result-true)))


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
