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
(library (ikarus.compiler.core-primitive-properties)
  (export
    initialise-core-primitive-properties

    core-primitive-name->core-type-tag
    core-primitive-name->application-attributes*
    core-primitive-name->core-type-signature*
    core-primitive-name->replacement*
    core-type-tag?
    core-type-tag-is-a?
    core-type-tag-matches-any-object?
    tuple-tags-arity
    tuple-tags-rest-objects-tag
    tuple-tags-ref
    application-attributes-operands-template
    application-attributes-foldable?
    application-attributes-effect-free?
    application-attributes-result-true?
    application-attributes-result-false?
    application-attributes-identity?
    CORE-PRIMITIVE-DEFAULT-APPLICATION-ATTRIBUTES)
  (import (except (vicare) unsafe)
    (ikarus.compiler.scheme-objects-ontology)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.core-primitive-properties.configuration)
    ;;
    (ikarus.compiler.core-primitive-properties.characters)
    (ikarus.compiler.core-primitive-properties.booleans)
    ;;
    (ikarus.compiler.core-primitive-properties.fixnums)
    (ikarus.compiler.core-primitive-properties.bignums)
    (ikarus.compiler.core-primitive-properties.ratnums)
    (ikarus.compiler.core-primitive-properties.flonums)
    (ikarus.compiler.core-primitive-properties.cflonums)
    (ikarus.compiler.core-primitive-properties.compnums)
    ;;
    (ikarus.compiler.core-primitive-properties.code-objects)
    (ikarus.compiler.core-primitive-properties.strings)
    (ikarus.compiler.core-primitive-properties.symbols)
    (ikarus.compiler.core-primitive-properties.keywords)
    (ikarus.compiler.core-primitive-properties.pointers)
    (ikarus.compiler.core-primitive-properties.bytevectors)
    ;;
    (ikarus.compiler.core-primitive-properties.pairs-and-lists)
    (ikarus.compiler.core-primitive-properties.vectors)
    (ikarus.compiler.core-primitive-properties.structs)
    (ikarus.compiler.core-primitive-properties.records)
    (ikarus.compiler.core-primitive-properties.hash-tables)
    ;;
    (ikarus.compiler.core-primitive-properties.annotation-objects)
    (ikarus.compiler.core-primitive-properties.enum-sets)
    (ikarus.compiler.core-primitive-properties.condition-objects)
    (ikarus.compiler.core-primitive-properties.transcoder-objects)
    ;;
    (ikarus.compiler.core-primitive-properties.control)
    (ikarus.compiler.core-primitive-properties.generic-primitives)
    (ikarus.compiler.core-primitive-properties.input-output)
    (ikarus.compiler.core-primitive-properties.environment-inquiry)
    (ikarus.compiler.core-primitive-properties.numerics)
    (ikarus.compiler.core-primitive-properties.times-and-dates)
    (ikarus.compiler.core-primitive-properties.library-utils)
    )

  (import SCHEME-OBJECTS-ONTOLOGY)


;;;; initialisation

(define (initialise-core-primitive-properties)
  (initialise-core-primitive-properties/configuration)
  ;;
  (initialise-core-primitive-properties/booleans)
  (initialise-core-primitive-properties/characters)
  ;;
  (initialise-core-primitive-properties/fixnums)
  (initialise-core-primitive-properties/bignums)
  (initialise-core-primitive-properties/ratnums)
  (initialise-core-primitive-properties/flonums)
  (initialise-core-primitive-properties/cflonums)
  (initialise-core-primitive-properties/compnums)
  ;;
  (initialise-core-primitive-properties/code-objects)
  (initialise-core-primitive-properties/strings)
  (initialise-core-primitive-properties/symbols)
  (initialise-core-primitive-properties/keywords)
  (initialise-core-primitive-properties/pointers)
  (initialise-core-primitive-properties/bytevectors)
  ;;
  (initialise-core-primitive-properties/pairs-and-lists)
  (initialise-core-primitive-properties/vectors)
  (initialise-core-primitive-properties/structs)
  (initialise-core-primitive-properties/records)
  (initialise-core-primitive-properties/hash-tables)
  ;;
  (initialise-core-primitive-properties/annotation-objects)
  (initialise-core-primitive-properties/enum-sets)
  (initialise-core-primitive-properties/condition-objects)
  (initialise-core-primitive-properties/transcoder-objects)
  ;;
  (initialise-core-primitive-properties/control)
  (initialise-core-primitive-properties/generic-primitives)
  (initialise-core-primitive-properties/input-output)
  (initialise-core-primitive-properties/environment-inquiry)
  (initialise-core-primitive-properties/numerics)
  (initialise-core-primitive-properties/times-and-dates)
  (initialise-core-primitive-properties/library-utils)
  #| end of define |# )


;;;; evaluation and lexical environments

(declare-core-primitive eval
    (safe)
  (signatures
   ((_ T:lexical-environment)	=> T:object)))

;;; --------------------------------------------------------------------

(declare-core-primitive environment
    (safe)
  (signatures
   (T:object			=> (T:lexical-environment)))
  (attributes
   ((_)				effect-free result-true)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:fixnum)		=> (T:lexical-environment)))
		   (attributes
		    (()			effect-free result-true))))
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
   (()			effect-free result-true)
   ((_)			result-true)))

(declare-core-primitive new-interaction-environment
    (safe)
  (signatures
   (()				=> (T:lexical-environment))
   ((T:proper-list)		=> (T:void)))
  (attributes
   (()			effect-free result-true)
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
		    ((_)		effect-free))))
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

(declare-core-primitive malloc
    (safe)
  (signatures
   ((T:exact-integer)			=> (T:pointer/false))))

(declare-core-primitive realloc
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> (T:pointer/false))))

(declare-core-primitive calloc
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer/false))))

(declare-core-primitive free
    (safe)
  (signatures
   ((T:pointer/memory-block)		=> (T:void))))

;;; --------------------------------------------------------------------

(declare-core-primitive malloc*
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:pointer))))

(declare-core-primitive realloc*
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:pointer))))

(declare-core-primitive calloc*
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer))))

;;; --------------------------------------------------------------------

(declare-core-primitive guarded-malloc
    (safe)
  (signatures
   ((T:exact-integer)			=> (T:pointer/false))))

(declare-core-primitive guarded-realloc
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> (T:pointer/false))))

(declare-core-primitive guarded-calloc
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:pointer/false))))

;;; --------------------------------------------------------------------

(declare-core-primitive guarded-malloc*
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:pointer))))

(declare-core-primitive guarded-realloc*
    (safe)
  (signatures
   ((T:pointer T:exact-integer)	=> (T:pointer))))

(declare-core-primitive guarded-calloc*
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
   ((T:bytevector)			=> (T:pointer/false T:fixnum/false)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->guarded-memory
    (safe)
  (signatures
   ((T:bytevector)			=> (T:pointer/false T:fixnum/false)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->memory*
    (safe)
  (signatures
   ((T:bytevector)			=> (T:pointer T:fixnum)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive bytevector->guarded-memory*
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
   ((T:bytevector)		=> ((or T:false T:pointer))))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive bytevector->guarded-cstring
    (safe)
  (signatures
   ((T:bytevector)		=> ((or T:false T:pointer))))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive bytevector->cstring*
    (safe)
  (signatures
   ((T:bytevector)		=> (T:pointer)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive bytevector->guarded-cstring*
    (safe)
  (signatures
   ((T:bytevector)		=> (T:pointer)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive string->cstring
    (safe)
  (signatures
   ((T:string)			=> ([or T:false T:pointer])))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive string->guarded-cstring
    (safe)
  (signatures
   ((T:string)			=> ([or T:false T:pointer])))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive string->cstring*
    (safe)
  (signatures
   ((T:string)			=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive string->guarded-cstring*
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
   ((T:pointer)			=> ([or T:false T:pointer])))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive guarded-strdup
    (safe)
  (signatures
   ((T:pointer)			=> ([or T:false T:pointer])))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive strdup*
    (safe)
  (signatures
   ((T:pointer)			=> (T:pointer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive guarded-strdup*
    (safe)
  (signatures
   ((T:pointer)			=> (T:pointer)))
  (attributes
   ((_)				effect-free result-true)))

;;;

(declare-core-primitive strndup
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> ([or T:false T:pointer])))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive guarded-strndup
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> ([or T:false T:pointer])))
  (attributes
   ((_ _)				effect-free)))

;;;

(declare-core-primitive strndup*
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> (T:pointer)))
  (attributes
   ((_ _)				effect-free result-true)))

(declare-core-primitive guarded-strndup*
    (safe)
  (signatures
   ((T:pointer T:exact-integer)		=> (T:pointer)))
  (attributes
   ((_ _)				effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive bytevectors->argv
    (safe)
  (signatures
   ((T:proper-list)		=> ([or T:false T:pointer])))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive bytevectors->guarded-argv
    (safe)
  (signatures
   ((T:proper-list)		=> ([or T:false T:pointer])))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive bytevectors->argv*
    (safe)
  (signatures
   ((T:proper-list)		=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive bytevectors->guarded-argv*
    (safe)
  (signatures
   ((T:proper-list)		=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive strings->argv
    (safe)
  (signatures
   ((T:proper-list)		=> ([or T:false T:pointer])))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive strings->guarded-argv
    (safe)
  (signatures
   ((T:proper-list)		=> ([or T:false T:pointer])))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive strings->argv*
    (safe)
  (signatures
   ((T:proper-list)		=> (T:pointer)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive strings->guarded-argv*
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
  (declare syntax-object-expression)
  (declare syntax-object-marks)
  (declare syntax-object-ribs)
  (declare syntax-object-source-objects)
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

(declare-core-primitive make-compile-time-value
    (safe)
  (signatures
   ((T:object)		=> (T:object)))
  (attributes
   ((_)			effect-free result-true)))

(declare-object-predicate compile-time-value?)

(declare-core-primitive compile-time-value-object
    (safe)
  (signatures
   ((T:object)		=> (T:object)))
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
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive syntax-cdr
    (safe)
  (signatures
   ((T:syntax-object)			=> (T:syntax-object))
   ((T:syntax-object T:procedure)	=> (T:syntax-object)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

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
   ((T:proper-list)	=> (T:boolean)))
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


;;;; debugging helpers

(declare-exact-integer-unary integer->machine-word)
(declare-exact-integer-unary machine-word->integer)

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
   ((T:flonum)		=> (T:bytevector)))
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

#| list of core primitives to declare

 do-overflow
 do-overflow-words
 do-vararg-overflow
 collect
 collect-key
 post-gc-hooks
 register-to-avoid-collecting
 forget-to-avoid-collecting
 replace-to-avoid-collecting
 retrieve-to-avoid-collecting
 collection-avoidance-list
 purge-collection-avoidance-list
 do-stack-overflow

;;; --------------------------------------------------------------------

 print-error
 assembler-output
 optimizer-output
 assembler-property-key
 expand-form-to-core-language
 expand-library
 expand-library->sexp
 expand-top-level
 expand-top-level->sexp
;;;
 current-primitive-locations
 boot-library-expand
 current-library-collection
 library-name
 find-library-by-name

;;;
 $make-tcbucket
 $tcbucket-key
 $tcbucket-val
 $tcbucket-next
 $set-tcbucket-val!
 $set-tcbucket-next!
 $set-tcbucket-tconc!

 $arg-list

 $collect-key
 $$apply
 $fp-at-base
 $primitive-call/cc
 $frame->continuation
 $current-frame
 $seal-frame-and-call
 $make-call-with-values-procedure
 $make-values-procedure
 $interrupted?
 $unset-interrupted!
 $swap-engine-counter!
;;;
 $apply-nonprocedure-error-handler
 $incorrect-args-error-handler
 $multiple-values-error
 $debug
 $do-event

;;; --------------------------------------------------------------------

 eval-core
 current-core-eval

;;;
 set-identifier-unsafe-variant!
;;;
 set-predicate-procedure-argument-validation!
 set-predicate-return-value-validation!

;;; --------------------------------------------------------------------
;;; library infrastructure

 library?
 library-uid
 library-imp-lib*
 library-vis-lib*
 library-inv-lib*
 library-export-subst
 library-export-env
 library-visit-state
 library-invoke-state
 library-visit-code
 library-invoke-code
 library-guard-code
 library-guard-lib*
 library-visible?
 library-source-file-name
 library-option*

 library-path
 library-extensions
 fasl-directory
 fasl-search-path
 fasl-path
 fasl-stem+extension

 current-library-locator
 run-time-library-locator
 compile-time-library-locator
 source-library-locator
 current-source-library-file-locator
 current-binary-library-file-locator
 default-source-library-file-locator
 default-binary-library-file-locator
 installed-libraries
 uninstall-library

;;; --------------------------------------------------------------------


 tagged-identifier-syntax?
 list-of-tagged-bindings?
 tagged-lambda-proto-syntax?
 tagged-formals-syntax?
 standard-formals-syntax?
 formals-signature-syntax?
 retvals-signature-syntax?
 parse-tagged-identifier-syntax
 parse-list-of-tagged-bindings
 parse-tagged-lambda-proto-syntax
 parse-tagged-formals-syntax

 make-clambda-compound
 clambda-compound?
 clambda-compound-common-retvals-signature
 clambda-compound-lambda-signatures

 make-lambda-signature
 lambda-signature?
 lambda-signature-formals
 lambda-signature-retvals
 lambda-signature-formals-tags
 lambda-signature-retvals-tags
 lambda-signature=?

 make-formals-signature
 formals-signature?
 formals-signature-tags
 formals-signature=?

 make-retvals-signature
 retvals-signature?
 retvals-signature-tags
 retvals-signature=?
 retvals-signature-common-ancestor

 tag-identifier?
 all-tag-identifiers?
 tag-identifier-callable-signature
 tag-super-and-sub?
 tag-identifier-ancestry
 tag-common-ancestor
 formals-signature-super-and-sub-syntax?

 set-identifier-object-type-spec!
 identifier-object-type-spec
 set-label-object-type-spec!
 label-object-type-spec
 make-object-type-spec
 object-type-spec?
 object-type-spec-uids
 object-type-spec-type-id
 object-type-spec-parent-spec
 object-type-spec-pred-stx
 object-type-spec-constructor-maker
 object-type-spec-accessor-maker
 object-type-spec-mutator-maker
 object-type-spec-getter-maker
 object-type-spec-setter-maker
 object-type-spec-dispatcher
 object-type-spec-ancestry

 tagged-identifier?
 set-identifier-tag!
 identifier-tag
 set-label-tag!
 label-tag

 expand-time-retvals-signature-violation?
 expand-time-retvals-signature-violation-expected-signature
 expand-time-retvals-signature-violation-returned-signature

 top-tag-id
 void-tag-id
 procedure-tag-id
 list-tag-id
 boolean-tag-id

|#


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; mode: vicare
;; eval: (put 'declare-core-primitive		'scheme-indent-function 2)
;; eval: (put 'declare-pair-accessor		'scheme-indent-function 1)
;; eval: (put 'declare-pair-mutator		'scheme-indent-function 1)
;; eval: (put 'declare-number-unary		'scheme-indent-function 1)
;; eval: (put 'declare-number-binary		'scheme-indent-function 1)
;; eval: (put 'declare-number-unary/binary	'scheme-indent-function 1)
;; eval: (put 'declare-number-binary/multi-comparison	'scheme-indent-function 1)
;; End:
