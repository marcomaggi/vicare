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
(library (ikarus.compiler.core-primitive-properties.generic-primitives)
  (export initialise-core-primitive-properties/generic-primitives)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/generic-primitives)


;;;; generic primitives

(declare-core-primitive immediate?
    (safe)
  (signatures
   ((T:fixnum)		=> (T:true))
   ((T:char)		=> (T:true))
   ((T:null)		=> (T:true))
   ((T:boolean)		=> (T:true))
   ((T:eof)		=> (T:true))
   ((T:void)		=> (T:true))
   ((T:transcoder)	=> (T:true))

   ((T:bignum)		=> (T:false))
   ((T:flonum)		=> (T:false))
   ((T:ratnum)		=> (T:false))
   ((T:compnum)		=> (T:false))
   ((T:cflonum)		=> (T:false))
   ((T:pair)		=> (T:false))
   ((T:string)		=> (T:false))
   ((T:vector)		=> (T:false))
   ((T:bytevector)	=> (T:false))
   ((T:struct)		=> (T:false))
   ((T:port)		=> (T:false))
   ((T:symbol)		=> (T:false))
   ((T:keyword)		=> (T:false))
   ((T:hashtable)	=> (T:false))
   ((T:would-block)	=> (T:false))

   ((T:object)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-type-predicate code?		T:code)
(declare-type-predicate procedure?	T:procedure)

(declare-core-primitive procedure-annotation
    (safe)
  (signatures
   ((T:procedure)	=> (T:object)))
  (attributes
   ((_)			effect-free)))

(declare-object-binary-comparison eq?)
(declare-object-binary-comparison neq?)
(declare-object-binary-comparison eqv?)
(declare-object-binary-comparison equal?)

(declare-object-predicate not)

;;; --------------------------------------------------------------------

(declare-core-primitive vicare-argv0
    (safe)
  (signatures
   (()			=> (T:bytevector)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive vicare-argv0-string
    (safe)
  (signatures
   (()			=> (T:string)))
  (attributes
   (()			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive void
    (safe)
  (signatures
   (()				=> (T:void)))
  (attributes
   (()				foldable effect-free result-true)))

(declare-core-primitive load
    (safe)
  (signatures
   ((T:string)			=> T:object)
   ((T:string T:procedure)	=> T:object)))

(declare-core-primitive make-traced-procedure
    (safe)
  (signatures
   ((T:symbol T:procedure)	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive make-traced-macro
    (safe)
  (signatures
   ((T:symbol T:procedure)	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive  random
    (safe)
  (signatures
   ((T:fixnum)		=> (T:fixnum)))
  (attributes
   ;;Not foldable  because the random number  must be generated at  run-time, one for
   ;;each invocation.
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-object-retriever uuid			T:string)

(declare-object-retriever bwp-object)
(declare-object-predicate bwp-object?)

(declare-object-retriever unbound-object)
(declare-object-predicate unbound-object?)

(declare-object-predicate $unbound-object?	unsafe)

;;; --------------------------------------------------------------------

(declare-parameter interrupt-handler	T:procedure)
(declare-parameter engine-handler	T:procedure)

;;; --------------------------------------------------------------------

(declare-core-primitive always-true
    (safe)
  (signatures
   (_				=> (T:true)))
  (attributes
   (_				foldable effect-free result-true)))

(declare-core-primitive always-false
    (safe)
  (signatures
   (_				=> (T:false)))
  (attributes
   (_				foldable effect-free result-false)))

;;; --------------------------------------------------------------------

(declare-core-primitive new-cafe
    (safe)
  (signatures
   (()			=> (T:void))
   ((T:procedure)	=> (T:void)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

(declare-parameter waiter-prompt-string		T:string)
(declare-parameter cafe-input-port		T:textual-input-port)

(declare-core-primitive apropos
    (safe)
  (signatures
   ((T:string)		=> (T:void)))
  (attributes
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive readline-enabled?
    (safe)
  (signatures
   (()			=> (T:boolean)))
  (attributes
   (()			effect-free)))

(declare-core-primitive readline
    (safe)
  (signatures
   (()						=> (T:string))
   (((or T:false T:bytevector T:string))	=> (T:string)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

(declare-core-primitive make-readline-input-port
    (safe)
  (signatures
   (()					=> (T:textual-input-port))
   (((or T:false T:procedure))		=> (T:textual-input-port)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive fasl-write
    (safe)
  (signatures
   ((T:object T:binary-output-port)			=> (T:void))
   ((T:object T:binary-output-port T:proper-list)	=> (T:void)))
  (attributes
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive fasl-read
    (safe)
  (signatures
   ((T:binary-input-port)	=> (T:object))))


;;;; foldable core primitive variants

(declare-core-primitive foldable-cons
    (safe)
  (signatures
   ((_ _)		=> (T:pair)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive foldable-list
    (safe)
  (signatures
   (()			=> (T:null))
   ((_ . _)		=> (T:non-empty-proper-list)))
  (attributes
   (()			foldable effect-free result-true)
   ((_ . _)		foldable effect-free result-true)))

(declare-core-primitive foldable-string
    (safe)
  (signatures
   (()			=> (T:string))
   (T:char		=> (T:string)))
  (attributes
   (()			foldable effect-free result-true)
   (_			foldable effect-free result-true)))

(declare-core-primitive foldable-vector
    (safe)
  (signatures
   (()				=> (T:vector))
   (_				=> (T:vector)))
  (attributes
   (()				foldable effect-free result-true)
   (_				foldable effect-free result-true)))

(declare-core-primitive foldable-list->vector
    (safe)
  (signatures
   ((T:proper-list)		=> (T:vector)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive foldable-append
    (safe)
  (signatures
   (()				=> (T:null))
   ((T:object . T:object)	=> (T:improper-list)))
  (attributes
   (()				foldable effect-free result-true)
   ((_ . _)			foldable effect-free result-true)))


;;;; debugging helpers

(declare-exact-integer-unary integer->machine-word)
(declare-core-primitive machine-word->integer
  (safe)
  (signatures
   ((T:object)			=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive flonum->bytevector
    (safe)
  (signatures
   ((T:flonum)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bytevector->flonum
    (safe)
  (signatures
   ((T:bytevector)	=> (T:flonum)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive bignum->bytevector
    (safe)
  (signatures
   ((T:bignum)		=> (T:bytevector)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bytevector->bignum
    (safe)
  (signatures
   ((T:bytevector)	=> (T:bignum)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive time-it
    (safe)
  (signatures
   ((T:string T:procedure)	=> T:object)))

(declare-core-primitive time-and-gather
    (safe)
  (signatures
   ((T:procedure T:procedure)	=> T:object)))

(declare-parameter verbose-timer)

;;;

(declare-type-predicate stats?		T:stats)

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ?who T:object))
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:stats)		=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare stats-collection-id)
  (declare stats-user-secs	T:exact-integer)
  (declare stats-user-usecs	T:exact-integer)
  (declare stats-sys-secs	T:exact-integer)
  (declare stats-sys-usecs	T:exact-integer)
  (declare stats-real-secs	T:exact-integer)
  (declare stats-real-usecs	T:exact-integer)
  (declare stats-gc-user-secs	T:exact-integer)
  (declare stats-gc-user-usecs	T:exact-integer)
  (declare stats-gc-sys-secs	T:exact-integer)
  (declare stats-gc-sys-usecs	T:exact-integer)
  (declare stats-gc-real-secs	T:exact-integer)
  (declare stats-gc-real-usecs	T:exact-integer)
  (declare stats-bytes-minor	T:exact-integer)
  (declare stats-bytes-major	T:exact-integer)
  #| end of LET-SYNTAX |# )


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
