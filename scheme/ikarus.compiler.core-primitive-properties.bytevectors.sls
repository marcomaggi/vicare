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
(library (ikarus.compiler.core-primitive-properties.bytevectors)
  (export initialise-core-primitive-properties/bytevectors)
  (import (except (vicare) unsafe)
    (ikarus.compiler.core-primitive-properties.base)
    (ikarus.compiler.scheme-objects-ontology))

  (import SCHEME-OBJECTS-ONTOLOGY)

  (define (initialise-core-primitive-properties/bytevectors)


;;;; bytevectors, safe functions

(declare-core-primitive make-bytevector
    (safe)
  (signatures
   ((T:non-negative-fixnum)			=> (T:bytevector))
   ((T:non-negative-fixnum T:octet/byte)	=> (T:bytevector)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive bytevector-copy
    (safe)
  (signatures
   ((T:bytevector)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_)				effect-free result-true)))

(declare-core-primitive bytevector-append
    (safe)
  (signatures
   (T:bytevector		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   (_				effect-free result-true)))

(declare-core-primitive bytevector-reverse-and-concatenate
    (safe)
  (signatures
   ((T:proper-list)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_)				effect-free result-true)))

;;;

(declare-core-primitive subbytevector-u8
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum)			=> (T:bytevector))
   ((T:bytevector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive subbytevector-s8
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum)			=> (T:bytevector))
   ((T:bytevector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive subbytevector-u8/count
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive subbytevector-s8/count
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _ _)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate bytevector? T:bytevector)

(declare-bytevector-predicate bytevector-empty?			(replacements $bytevector-empty?))

(declare-bytevector-predicate ascii-encoded-bytevector?		(replacements $ascii-encoded-bytevector?))
(declare-bytevector-predicate latin1-encoded-bytevector?	(replacements $latin1-encoded-bytevector?))
(declare-bytevector-predicate octets-encoded-bytevector?	(replacements $octets-encoded-bytevector?))
(declare-bytevector-predicate uri-encoded-bytevector?		(replacements $uri-encoded-bytevector?))
(declare-bytevector-predicate percent-encoded-bytevector?	(replacements $percent-encoded-bytevector?))

(declare-core-primitive list-of-bytevectors?
    (safe)
  (signatures
   ((T:proper-list)		=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive bytevector-length
    (safe)
  (signatures
   ((T:bytevector)	=> (T:non-negative-fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-bytevector-binary-comparison bytevector=?	(replacements $bytevector=))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive bytevector-fill!
    (safe)
  (signatures
   ((T:bytevector T:octet/byte)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive bytevector-copy!
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:bytevector T:non-negative-fixnum T:non-negative-fixnum)     => (T:void)))
  (attributes
   ((_ _ _ _ _)			result-true)))

;;;

(declare-core-primitive bytevector-s8-ref
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum)	=> (T:byte)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bytevector-u8-ref
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum)	=> (T:octet)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(let-syntax
    ((declare-safe-bytevector-accessor
      (syntax-rules ()
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector T:non-negative-fixnum)	=> (?return-value-tag)))
	   (attributes
	    ((_ _)		foldable effect-free result-true))))
	)))
  (declare-safe-bytevector-accessor bytevector-s16-native-ref		T:sint16)
  (declare-safe-bytevector-accessor bytevector-u16-native-ref		T:uint16)
  (declare-safe-bytevector-accessor bytevector-s32-native-ref		T:sint32)
  (declare-safe-bytevector-accessor bytevector-u32-native-ref		T:uint32)
  (declare-safe-bytevector-accessor bytevector-s64-native-ref		T:sint64)
  (declare-safe-bytevector-accessor bytevector-u64-native-ref		T:uint64)
  (declare-safe-bytevector-accessor bytevector-ieee-double-native-ref	T:flonum)
  (declare-safe-bytevector-accessor bytevector-ieee-single-native-ref	T:flonum)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-safe-bytevector-accessor
      (syntax-rules ()
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector T:non-negative-fixnum T:symbol)	=> (?return-value-tag)))
	   (attributes
	    ((_ _)		foldable effect-free result-true))))
	)))
  (declare-safe-bytevector-accessor bytevector-s16-ref		T:sint16)
  (declare-safe-bytevector-accessor bytevector-u16-ref		T:uint16)
  (declare-safe-bytevector-accessor bytevector-s32-ref		T:sint32)
  (declare-safe-bytevector-accessor bytevector-u32-ref		T:uint32)
  (declare-safe-bytevector-accessor bytevector-s64-ref		T:sint64)
  (declare-safe-bytevector-accessor bytevector-u64-ref		T:uint64)
  (declare-safe-bytevector-accessor bytevector-ieee-double-ref	T:flonum)
  (declare-safe-bytevector-accessor bytevector-ieee-single-ref	T:flonum)
  #| end of LET-SYNTAX |# )

(declare-core-primitive bytevector-sint-ref
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:symbol T:positive-fixnum)	=> (T:exact-integer)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

(declare-core-primitive bytevector-uint-ref
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:symbol T:positive-fixnum)	=> (T:non-negative-exact-integer)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

;;;

(declare-core-primitive bytevector-s8-set!
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:byte)	=> (T:void))))

(declare-core-primitive bytevector-u8-set!
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:octet)	=> (T:void))))

(let-syntax
    ((declare-safe-bytevector-mutator
      (syntax-rules ()
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector T:non-negative-fixnum ?new-value-tag)	=> (T:void)))))
	)))
  (declare-safe-bytevector-mutator bytevector-s16-native-set!		T:sint16)
  (declare-safe-bytevector-mutator bytevector-u16-native-set!		T:uint16)
  (declare-safe-bytevector-mutator bytevector-s32-native-set!		T:sint32)
  (declare-safe-bytevector-mutator bytevector-u32-native-set!		T:uint32)
  (declare-safe-bytevector-mutator bytevector-s64-native-set!		T:sint64)
  (declare-safe-bytevector-mutator bytevector-u64-native-set!		T:uint64)
  (declare-safe-bytevector-mutator bytevector-ieee-double-native-set!	T:flonum)
  (declare-safe-bytevector-mutator bytevector-ieee-single-native-set!	T:flonum)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-safe-bytevector-mutator
      (syntax-rules ()
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector T:non-negative-fixnum ?new-value-tag T:symbol)	=> (T:void)))))
	)))
  (declare-safe-bytevector-mutator bytevector-s16-set!		T:sint16)
  (declare-safe-bytevector-mutator bytevector-u16-set!		T:uint16)
  (declare-safe-bytevector-mutator bytevector-s32-set!		T:sint32)
  (declare-safe-bytevector-mutator bytevector-u32-set!		T:uint32)
  (declare-safe-bytevector-mutator bytevector-s64-set!		T:sint64)
  (declare-safe-bytevector-mutator bytevector-u64-set!		T:uint64)
  (declare-safe-bytevector-mutator bytevector-ieee-double-set!	T:flonum)
  (declare-safe-bytevector-mutator bytevector-ieee-single-set!	T:flonum)
  #| end of LET-SYNTAX |# )

(declare-core-primitive bytevector-sint-set!
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:exact-integer T:symbol T:positive-fixnum)	=> (T:exact-integer))))

(declare-core-primitive bytevector-uint-set!
    (safe)
  (signatures
   ((T:bytevector T:non-negative-fixnum T:exact-integer T:symbol T:positive-fixnum)	=> (T:non-negative-exact-integer))))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive bytevector->string
    (safe)
  (signatures
   ((T:bytevector T:transcoder)	=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_ _)			effect-free result-true)))

(let-syntax
    ((declare-bytevector->string-conversion
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector)	=> (T:string)))
	   (attributes
	    ;;Not foldable because it must return a new string at every application.
	    ((_) 		effect-free result-true))))
	)))
  (declare-bytevector->string-conversion ascii->string)
  (declare-bytevector->string-conversion bytevector->string-base64)
  (declare-bytevector->string-conversion bytevector->string-hex)
  (declare-bytevector->string-conversion latin1->string)
  (declare-bytevector->string-conversion octets->string)
  (declare-bytevector->string-conversion percent-encoding->string)
  (declare-bytevector->string-conversion uri-encoding->string)
  #| end of LET-SYNTAX |# )

(declare-core-primitive utf8->string
    (safe)
  (signatures
   ((T:bytevector)		=> (T:string))
   ((T:bytevector T:symbol)	=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_) 		effect-free result-true)
   ((_ _) 		effect-free result-true)))

(declare-core-primitive utf8->string-length
    (safe)
  (signatures
   ((T:bytevector)		=> (T:fixnum))
   ((T:bytevector T:symbol)	=> (T:fixnum)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_) 		effect-free result-true)
   ((_ _) 		effect-free result-true)))

(declare-core-primitive utf16->string
    (safe)
  (signatures
   ((T:bytevector T:symbol)			=> (T:string))
   ((T:bytevector T:symbol T:object)		=> (T:string))
   ((T:bytevector T:symbol T:object T:symbol)	=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_ _) 		effect-free result-true)
   ((_ _ _) 		effect-free result-true)
   ((_ _ _ _) 		effect-free result-true)))

(let-syntax
    ((declare-bytevector->string-conversion
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector)		=> (T:string))
	    ((T:bytevector T:symbol)	=> (T:string)))
	   (attributes
	    ;;Not foldable because it must return a new string at every application.
	    ((_) 		effect-free result-true)
	    ((_ _) 		effect-free result-true))))
	)))
  (declare-bytevector->string-conversion utf16le->string)
  (declare-bytevector->string-conversion utf16n->string)
  (declare-bytevector->string-conversion utf16be->string)
  #| end of LET-SYNTAX |# )

(declare-core-primitive utf32->string
    (safe)
  (signatures
   ((T:bytevector T:symbol)			=> (T:string))
   ((T:bytevector T:symbol T:object)		=> (T:string))
   ((T:bytevector T:symbol T:object T:symbol)	=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_ _) 		effect-free result-true)
   ((_ _ _) 		effect-free result-true)
   ((_ _ _ _) 		effect-free result-true)))

(let-syntax
    ((declare-bytevector->bytevector-conversion
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector)	=> (T:bytevector)))
	   (attributes
	    ;;Not foldable because it must return a new string at every application.
	    ((_) 		effect-free result-true))))
	)))
  (declare-bytevector->bytevector-conversion uri-decode)
  (declare-bytevector->bytevector-conversion uri-encode)
  (declare-bytevector->bytevector-conversion base64->bytevector)
  (declare-bytevector->bytevector-conversion bytevector->base64)
  (declare-bytevector->bytevector-conversion bytevector->hex)
  (declare-bytevector->bytevector-conversion hex->bytevector)
  (declare-bytevector->bytevector-conversion percent-encode)
  (declare-bytevector->bytevector-conversion percent-decode)
  (declare-bytevector->bytevector-conversion normalise-percent-encoding)
  (declare-bytevector->bytevector-conversion normalise-uri-encoding)
  #| end of LET-SYNTAX |# )

(declare-core-primitive bytevector->sint-list
    (safe)
  (signatures
   ((T:bytevector T:symbol T:positive-fixnum)	=> (T:proper-list)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_ _ _) 		effect-free result-true)))

(declare-core-primitive bytevector->uint-list
    (safe)
  (signatures
   ((T:bytevector T:symbol T:positive-fixnum)	=> (T:proper-list)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_ _ _) 		effect-free result-true)))

(let-syntax
    ((declare-bytevector->list-conversion
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector)	=> (T:proper-list)))
	   (attributes
	    ;;Not foldable because it must return a new list at every application.
	    ((_) 		effect-free result-true))))
	)))
  (declare-bytevector->list-conversion bytevector->s8-list)
  (declare-bytevector->list-conversion bytevector->u8-list)

  (declare-bytevector->list-conversion bytevector->c4b-list)
  (declare-bytevector->list-conversion bytevector->c4l-list)
  (declare-bytevector->list-conversion bytevector->c4n-list)
  (declare-bytevector->list-conversion bytevector->c8b-list)
  (declare-bytevector->list-conversion bytevector->c8l-list)
  (declare-bytevector->list-conversion bytevector->c8n-list)
  (declare-bytevector->list-conversion bytevector->f4b-list)
  (declare-bytevector->list-conversion bytevector->f4l-list)
  (declare-bytevector->list-conversion bytevector->f4n-list)
  (declare-bytevector->list-conversion bytevector->f8b-list)
  (declare-bytevector->list-conversion bytevector->f8l-list)
  (declare-bytevector->list-conversion bytevector->f8n-list)
  (declare-bytevector->list-conversion bytevector->s16b-list)
  (declare-bytevector->list-conversion bytevector->s16l-list)
  (declare-bytevector->list-conversion bytevector->s16n-list)
  (declare-bytevector->list-conversion bytevector->s32b-list)
  (declare-bytevector->list-conversion bytevector->s32l-list)
  (declare-bytevector->list-conversion bytevector->s32n-list)
  (declare-bytevector->list-conversion bytevector->s64b-list)
  (declare-bytevector->list-conversion bytevector->s64l-list)
  (declare-bytevector->list-conversion bytevector->s64n-list)
  (declare-bytevector->list-conversion bytevector->u16b-list)
  (declare-bytevector->list-conversion bytevector->u16l-list)
  (declare-bytevector->list-conversion bytevector->u16n-list)
  (declare-bytevector->list-conversion bytevector->u32b-list)
  (declare-bytevector->list-conversion bytevector->u32l-list)
  (declare-bytevector->list-conversion bytevector->u32n-list)
  (declare-bytevector->list-conversion bytevector->u64b-list)
  (declare-bytevector->list-conversion bytevector->u64l-list)
  (declare-bytevector->list-conversion bytevector->u64n-list)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; bytevector related predicates

(declare-type-predicate bytevector-length?	T:non-negative-fixnum)
(declare-type-predicate bytevector-index?	T:non-negative-fixnum)

(let-syntax
    ((declare-bytevector-releated-fixnum-predicate
      (syntax-rules ()
	((_ ?who ?obj-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((?obj-tag)		=> (T:boolean))
	    ((T:object)		=> (T:false)))
	   (attributes
	    ((_)		foldable effect-free result-true))))
	)))
  (declare-bytevector-releated-fixnum-predicate bytevector-word-size?	T:positive-fixnum)
  (declare-bytevector-releated-fixnum-predicate bytevector-word-count?	T:non-negative-fixnum)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word?	T:non-negative-fixnum)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word8?	T:non-negative-fixnum)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word16?	T:non-negative-fixnum)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word32?	T:non-negative-fixnum)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word64?	T:non-negative-fixnum)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-bytevector-releated-fixnum-predicate
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:bytevector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:boolean)))
	   (attributes
	    ((_ _ _)		foldable effect-free result-true))))
	)))
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word?)
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word8?)
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word16?)
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word32?)
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word64?)
  #| end of LET-SYNTAX |# )


;;;; bytevectors, unsafe functions

;;; constructors

(declare-core-primitive $make-bytevector
    (unsafe)
  (signatures
   ((T:fixnum)		=> (T:bytevector))
   ((T:fixnum T:fixnum)	=> (T:bytevector)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive $bytevector-copy
    (unsafe)
  (signatures
   ((T:bytevector)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_)				effect-free result-true)))

(declare-core-primitive $bytevector-concatenate
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _)			effect-free result-true)))

(declare-core-primitive $bytevector-reverse-and-concatenate
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $bytevector-length
    (unsafe)
  (signatures
   ((T:bytevector)		=> (T:non-negative-fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $bytevector-total-length
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:non-negative-exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-bytevector-predicate $bytevector-empty?		unsafe)
(declare-bytevector-predicate $uri-encoded-bytevector?		unsafe)
(declare-bytevector-predicate $octets-encoded-bytevector?	unsafe)
(declare-bytevector-predicate $ascii-encoded-bytevector?	unsafe)
(declare-bytevector-predicate $latin1-encoded-bytevector?	unsafe)
(declare-bytevector-predicate $percent-encoded-bytevector?	unsafe)

;;; --------------------------------------------------------------------
;;; comparison

(declare-bytevector-binary-comparison $bytevector=	unsafe)

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-unsafe-bytevector-accessor $bytevector-u8-ref				T:octet)
(declare-unsafe-bytevector-accessor $bytevector-s8-ref				T:byte)
(declare-unsafe-bytevector-mutator  $bytevector-set!				T:octet/byte)

(declare-unsafe-bytevector-accessor $bytevector-ieee-double-native-ref		T:flonum)
(declare-unsafe-bytevector-mutator  $bytevector-ieee-double-native-set!		T:flonum)

(declare-unsafe-bytevector-accessor $bytevector-ieee-double-nonnative-ref	T:flonum)
(declare-unsafe-bytevector-mutator  $bytevector-ieee-double-nonnative-set!	T:flonum)

(declare-unsafe-bytevector-accessor $bytevector-ieee-single-native-ref		T:flonum)
(declare-unsafe-bytevector-mutator  $bytevector-ieee-single-native-set!		T:flonum)

(declare-unsafe-bytevector-accessor $bytevector-ieee-single-nonnative-ref	T:flonum)
(declare-unsafe-bytevector-mutator  $bytevector-ieee-single-nonnative-set!	T:flonum)

;;; --------------------------------------------------------------------
;;; conversion

(declare-unsafe-bytevector-conversion $uri-encode			T:bytevector)
(declare-unsafe-bytevector-conversion $uri-decode			T:bytevector)
(declare-unsafe-bytevector-conversion $uri-normalise-encoding		T:bytevector)
(declare-unsafe-bytevector-conversion $percent-encode			T:bytevector)
(declare-unsafe-bytevector-conversion $percent-decode			T:bytevector)
(declare-unsafe-bytevector-conversion $percent-normalise-encoding	T:bytevector)
(declare-unsafe-bytevector-conversion $bytevector->base64		T:bytevector)
(declare-unsafe-bytevector-conversion $base64->bytevector		T:bytevector)
(declare-unsafe-bytevector-conversion $ascii->string			T:string)
(declare-unsafe-bytevector-conversion $octets->string			T:string)
(declare-unsafe-bytevector-conversion $latin1->string			T:string)
(declare-unsafe-bytevector-conversion $bytevector->string-base64	T:string)


;;;; done

 #| end of DEFINE |# )

#| end of library |# )

;;; end o file
;; Local Variables:
;; eval: (put 'declare-core-primitive 'scheme-indent-function 2)
;; End:
