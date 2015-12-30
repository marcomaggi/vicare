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
(library (ikarus.compiler.core-primitive-properties.ffi)
  (export initialise-core-primitive-properties/ffi)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/ffi)


;;;; system interface and foreign functions interface

(declare-core-primitive errno
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:void)))
  (attributes
   (()			effect-free result-true)
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive free
    (safe)
  (signatures
   ((T:pointer/memory-block)		=> (T:void))))

(declare-core-primitive malloc
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:pointer))))

(declare-core-primitive realloc
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:pointer))))

(declare-core-primitive calloc
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer))))

;;; --------------------------------------------------------------------

(declare-core-primitive guarded-malloc
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:pointer))))

(declare-core-primitive guarded-realloc
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:pointer))))

(declare-core-primitive guarded-calloc
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer))))

;;; --------------------------------------------------------------------

(declare-core-primitive memcpy
    (safe)
  (signatures
   ((T:pointer T:pointer T:exact-integer)	=> (T:void))))

(declare-core-primitive memmove
    (safe)
  (signatures
   ((T:pointer T:pointer T:exact-integer)	=> (T:void))))

(declare-core-primitive memcmp
    (safe)
  (signatures
   ((T:pointer T:pointer T:exact-integer)	=> (T:fixnum))))

(declare-core-primitive memset
    (safe)
  (signatures
   ((T:pointer T:exact-integer T:exact-integer)	=> (T:void))))

;;; --------------------------------------------------------------------

(declare-core-primitive make-memory-block
    (safe)
  (signatures
   ((T:pointer T:exact-integer)			=> (T:memory-block))))

(declare-core-primitive make-memory-block/guarded
    (safe)
  (signatures
   ((T:pointer T:exact-integer)			=> (T:memory-block))))

(declare-core-primitive null-memory-block
    (safe)
  (signatures
   (()						=> (T:memory-block)))
  (attributes
   (()						effect-free result-true)))

(declare-core-primitive memory-block?
    (safe)
  (signatures
   ((T:memory-block)				=> (T:boolean))
   ((T:object)					=> (T:false)))
  (attributes
   ((_)						effect-free)))

(declare-core-primitive memory-block?/not-null
    (safe)
  (signatures
   ((T:memory-block)			=> (T:boolean))
   ((T:object)				=> (T:false)))
  (attributes
   ((_)					effect-free)))

(declare-core-primitive memory-block?/non-null
    (safe)
  (signatures
   ((T:memory-block)			=> (T:boolean))
   ((T:object)				=> (T:false)))
  (attributes
   ((_)					effect-free)))

(declare-core-primitive memory-block-pointer
    (safe)
  (signatures
   ((T:memory-block)			=> (T:pointer)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive memory-block-size
    (safe)
  (signatures
   ((T:memory-block)			=> (T:exact-integer)))
  (attributes
   ((_)					effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive make-out-of-memory-error
    (safe)
  (signatures
   (()					=> (T:record)))
  (attributes
   (()					effect-free result-true)))

(declare-core-primitive out-of-memory-error?
    (safe)
  (signatures
   ((T:record)				=> (T:boolean))
   ((T:object)				=> (T:false)))
  (attributes
   ((_)					effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive memory-copy
    (safe)
  (signatures
   ((T:pointer/bytevector T:fixnum T:pointer/bytevector T:fixnum T:fixnum)	=> (T:void))))

(declare-core-primitive memory->bytevector
    (safe)
  (signatures
   ((T:pointer T:fixnum)		=> (T:bytevector)))
  (attributes
   ((_ _)				effect-free result-true)))

(declare-core-primitive bytevector->memory
    (safe)
  (signatures
   ((T:bytevector)			=> (T:pointer T:fixnum)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->guarded-memory
    (safe)
  (signatures
   ((T:bytevector)			=> (T:pointer T:fixnum)))
  (attributes
   ((_ _)				effect-free)))


;;; --------------------------------------------------------------------
;;; cstrings

(declare-core-primitive bytevector->cstring
    (safe)
  (signatures
   ((T:bytevector)		=> (T:pointer)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive bytevector->guarded-cstring
    (safe)
  (signatures
   ((T:bytevector)		=> (T:pointer)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive string->cstring
    (safe)
  (signatures
   ((T:string)			=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive string->guarded-cstring
    (safe)
  (signatures
   ((T:string)			=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive cstring->bytevector
    (safe)
  (signatures
   ((T:pointer)				=> (T:bytevector))
   ((T:pointer T:non-negative-fixnum)	=> (T:bytevector)))
  (attributes
   ((_)					effect-free result-true)
   ((_ _)				effect-free result-true)))

(declare-core-primitive cstring16->bytevector
    (safe)
  (signatures
   ((T:pointer)				=> (T:bytevector)))
  (attributes
   ((_)					effect-free result-true)))

;;;

(declare-core-primitive cstring->string
    (safe)
  (signatures
   ((T:pointer)				=> (T:string))
   ((T:pointer T:non-negative-fixnum)	=> (T:string)))
  (attributes
   ((_)					effect-free result-true)
   ((_ _)				effect-free result-true)))

(declare-core-primitive cstring16n->string
    (safe)
  (signatures
   ((T:pointer)				=> (T:string)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive cstring16le->string
    (safe)
  (signatures
   ((T:pointer)				=> (T:string)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive cstring16be->string
    (safe)
  (signatures
   ((T:pointer)				=> (T:string)))
  (attributes
   ((_)					effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive strlen
    (safe)
  (signatures
   ((T:pointer)			=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive strcmp
    (safe)
  (signatures
   ((T:pointer T:pointer)	=> (T:fixnum)))
  (attributes
   ((_ _)			effect-free result-true)))

(declare-core-primitive strncmp
    (safe)
  (signatures
   ((T:pointer T:pointer T:non-negative-fixnum)		=> (T:fixnum)))
  (attributes
   ((_ _ _)			effect-free result-true)))

;;;

(declare-core-primitive strdup
    (safe)
  (signatures
   ((T:pointer)			=> (T:pointer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive guarded-strdup
    (safe)
  (signatures
   ((T:pointer)			=> (T:pointer)))
  (attributes
   ((_)				effect-free result-true)))

;;;

(declare-core-primitive strndup
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> (T:pointer)))
  (attributes
   ((_ _)				effect-free result-true)))

(declare-core-primitive guarded-strndup
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> (T:pointer)))
  (attributes
   ((_ _)				effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive bytevectors->argv
    (safe)
  (signatures
   ((T:proper-list)		=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive bytevectors->guarded-argv
    (safe)
  (signatures
   ((T:proper-list)		=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive strings->argv
    (safe)
  (signatures
   ((T:proper-list)		=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive strings->guarded-argv
    (safe)
  (signatures
   ((T:proper-list)		=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive argv->bytevectors
    (safe)
  (signatures
   ((T:pointer)			=> (T:proper-list)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive argv->strings
    (safe)
  (signatures
   ((T:pointer)			=> (T:proper-list)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive argv-length
    (safe)
  (signatures
   ((T:pointer)			=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive dlopen
    (safe)
  (signatures
   (()						=> ([or T:false T:pointer]))
   (([or T:bytevector T:string])		=> ([or T:false T:pointer]))
   (([or T:bytevector T:string] _ _)		=> ([or T:false T:pointer]))))

(declare-core-primitive dlclose
    (safe)
  (signatures
   ((T:pointer)			=> (T:boolean))))

(declare-core-primitive dlerror
    (safe)
  (signatures
   (()				=> ([or T:false T:string]))))

(declare-core-primitive dlsym
    (safe)
  (signatures
   ((T:pointer T:string)	=> ([or T:false T:pointer]))))

;;; --------------------------------------------------------------------

(declare-core-primitive make-c-callout-maker
    (safe)
  (signatures
   ((T:symbol T:proper-list)		=> (T:procedure))))

(declare-core-primitive make-c-callout-maker/with-errno
    (safe)
  (signatures
   ((T:symbol T:proper-list)		=> (T:procedure))))

(declare-core-primitive make-c-callback-maker
    (safe)
  (signatures
   ((T:symbol T:proper-list)		=> (T:procedure))))

(declare-core-primitive free-c-callback
    (safe)
  (signatures
   ((T:pointer)				=> (T:void))))


;;;; foreign functions interface: raw memory accessors and mutators, safe procedures

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:pointer T:non-negative-exact-integer)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free))))
		)))

  (declare pointer-ref-c-uint8		T:uint8)
  (declare pointer-ref-c-sint8		T:sint8)
  (declare pointer-ref-c-uint16		T:uint16)
  (declare pointer-ref-c-sint16		T:sint16)
  (declare pointer-ref-c-uint32		T:uint32)
  (declare pointer-ref-c-sint32		T:sint32)
  (declare pointer-ref-c-uint64		T:uint64)
  (declare pointer-ref-c-sint64		T:sint64)

  (declare pointer-ref-c-signed-char		T:fixnum)
  (declare pointer-ref-c-signed-short		T:fixnum)
  (declare pointer-ref-c-signed-int		T:exact-integer)
  (declare pointer-ref-c-signed-long		T:exact-integer)
  (declare pointer-ref-c-signed-long-long	T:exact-integer)

  (declare pointer-ref-c-unsigned-char		T:non-negative-exact-integer)
  (declare pointer-ref-c-unsigned-short		T:non-negative-exact-integer)
  (declare pointer-ref-c-unsigned-int		T:non-negative-exact-integer)
  (declare pointer-ref-c-unsigned-long		T:non-negative-exact-integer)
  (declare pointer-ref-c-unsigned-long-long	T:non-negative-exact-integer)

  (declare pointer-ref-c-float			T:flonum)
  (declare pointer-ref-c-double			T:flonum)
  (declare pointer-ref-c-pointer		T:pointer)

  (declare pointer-ref-c-size_t			T:non-negative-exact-integer)
  (declare pointer-ref-c-ssize_t		T:exact-integer)
  (declare pointer-ref-c-off_t			T:exact-integer)
  (declare pointer-ref-c-ptrdiff_t		T:exact-integer)

  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?new-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:pointer T:non-negative-exact-integer ?new-value-tag)	=> (T:void)))
		   (attributes
		    ((_ _)		effect-free))))
		)))

  (declare pointer-set-c-uint8!			T:uint8)
  (declare pointer-set-c-sint8!			T:sint8)
  (declare pointer-set-c-uint16!		T:uint16)
  (declare pointer-set-c-sint16!		T:sint16)
  (declare pointer-set-c-uint32!		T:uint32)
  (declare pointer-set-c-sint32!		T:sint32)
  (declare pointer-set-c-uint64!		T:uint64)
  (declare pointer-set-c-sint64!		T:sint64)

  (declare pointer-set-c-signed-char!		T:fixnum)
  (declare pointer-set-c-signed-short!		T:fixnum)
  (declare pointer-set-c-signed-int!		T:exact-integer)
  (declare pointer-set-c-signed-long!		T:exact-integer)
  (declare pointer-set-c-signed-long-long!	T:exact-integer)

  (declare pointer-set-c-unsigned-char!		T:non-negative-exact-integer)
  (declare pointer-set-c-unsigned-short!	T:non-negative-exact-integer)
  (declare pointer-set-c-unsigned-int!		T:non-negative-exact-integer)
  (declare pointer-set-c-unsigned-long!		T:non-negative-exact-integer)
  (declare pointer-set-c-unsigned-long-long!	T:non-negative-exact-integer)

  (declare pointer-set-c-float!			T:flonum)
  (declare pointer-set-c-double!		T:flonum)
  (declare pointer-set-c-pointer!		T:pointer)

  (declare pointer-set-c-size_t!		T:non-negative-exact-integer)
  (declare pointer-set-c-ssize_t!		T:exact-integer)
  (declare pointer-set-c-off_t!			T:exact-integer)
  (declare pointer-set-c-ptrdiff_t!		T:exact-integer)

  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:pointer T:non-negative-exact-integer)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free))))
		)))

  (declare array-ref-c-uint8			T:uint8)
  (declare array-ref-c-sint8			T:sint8)
  (declare array-ref-c-uint16			T:uint16)
  (declare array-ref-c-sint16			T:sint16)
  (declare array-ref-c-uint32			T:uint32)
  (declare array-ref-c-sint32			T:sint32)
  (declare array-ref-c-uint64			T:uint64)
  (declare array-ref-c-sint64			T:sint64)

  (declare array-ref-c-signed-char		T:fixnum)
  (declare array-ref-c-signed-short		T:fixnum)
  (declare array-ref-c-signed-int		T:exact-integer)
  (declare array-ref-c-signed-long		T:exact-integer)
  (declare array-ref-c-signed-long-long		T:exact-integer)

  (declare array-ref-c-unsigned-char		T:non-negative-fixnum)
  (declare array-ref-c-unsigned-short		T:non-negative-fixnum)
  (declare array-ref-c-unsigned-int		T:non-negative-exact-integer)
  (declare array-ref-c-unsigned-long		T:non-negative-exact-integer)
  (declare array-ref-c-unsigned-long-long	T:non-negative-exact-integer)

  (declare array-ref-c-float			T:flonum)
  (declare array-ref-c-double			T:flonum)
  (declare array-ref-c-pointer			T:pointer)

  (declare array-ref-c-size_t			T:non-negative-exact-integer)
  (declare array-ref-c-ssize_t			T:exact-integer)
  (declare array-ref-c-off_t			T:exact-integer)
  (declare array-ref-c-ptrdiff_t		T:exact-integer)

  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?new-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:pointer T:non-negative-exact-integer ?new-value-tag)	=> (T:void)))
		   (attributes
		    ((_ _)		effect-free))))
		)))

  (declare array-set-c-uint8!			T:uint8)
  (declare array-set-c-sint8!			T:sint8)
  (declare array-set-c-uint16!			T:uint16)
  (declare array-set-c-sint16!			T:sint16)
  (declare array-set-c-uint32!			T:uint32)
  (declare array-set-c-sint32!			T:sint32)
  (declare array-set-c-uint64!			T:uint64)
  (declare array-set-c-sint64!			T:sint64)

  (declare array-set-c-signed-char!		T:fixnum)
  (declare array-set-c-signed-short!		T:fixnum)
  (declare array-set-c-signed-int!		T:exact-integer)
  (declare array-set-c-signed-long!		T:exact-integer)
  (declare array-set-c-signed-long-long!	T:exact-integer)

  (declare array-set-c-unsigned-char!		T:non-negative-exact-integer)
  (declare array-set-c-unsigned-short!		T:non-negative-exact-integer)
  (declare array-set-c-unsigned-int!		T:non-negative-exact-integer)
  (declare array-set-c-unsigned-long!		T:non-negative-exact-integer)
  (declare array-set-c-unsigned-long-long!	T:non-negative-exact-integer)

  (declare array-set-c-float!			T:flonum)
  (declare array-set-c-double!			T:flonum)
  (declare array-set-c-pointer!			T:pointer)

  (declare array-set-c-size_t!			T:non-negative-exact-integer)
  (declare array-set-c-ssize_t!			T:exact-integer)
  (declare array-set-c-off_t!			T:exact-integer)
  (declare array-set-c-ptrdiff_t!		T:exact-integer)

  #| end of LET-SYNTAX |# )


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
