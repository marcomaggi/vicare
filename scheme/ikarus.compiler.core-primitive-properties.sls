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
    (ikarus.compiler.core-primitive-properties.symbols)
    (ikarus.compiler.core-primitive-properties.keywords)
    (ikarus.compiler.core-primitive-properties.pointers)
    ;;
    (ikarus.compiler.core-primitive-properties.pairs-and-lists)
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
  (initialise-core-primitive-properties/symbols)
  (initialise-core-primitive-properties/keywords)
  (initialise-core-primitive-properties/pointers)
  ;;
  (initialise-core-primitive-properties/pairs-and-lists)
  #| end of define |# )


;;;; strings, safe functions
;;
;;According to R6RS:  STRING and MAKE-STRING must return a  newly allocated string at
;;every invocation;  if we want the  same string we  just use the double  quotes.  So
;;STRING and MAKE-STRING are not FOLDABLE.
;;

;;; predicates

(declare-type-predicate string? T:string)

(declare-string-predicate string-empty?			(replacements $string-empty?))

(declare-string-predicate ascii-encoded-string?		(replacements $ascii-encoded-string?))
(declare-string-predicate latin1-encoded-string?	(replacements $latin1-encoded-string?))
(declare-string-predicate octets-encoded-string?	(replacements $octets-encoded-string?))
(declare-string-predicate uri-encoded-string?		(replacements $uri-encoded-string?))
(declare-string-predicate percent-encoded-string?	(replacements $percent-encoded-string?))

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive string
    (safe)
  (signatures
   (()			=> (T:string))
   (T:char		=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   (()			effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive make-string
    (safe)
  (signatures
   ((T:fixnum)		=> (T:string))
   ((T:fixnum T:char)	=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((0)			effect-free result-true)
   ((0 . _)		effect-free result-true)
   (_			effect-free result-true)))

(declare-core-primitive substring
    (safe)
  (signatures
   ((T:string T:fixnum T:fixnum)	=> (T:string)))
  ;;Not  foldable because  it must  return a  newly allocated  string, even  when the
  ;;return value is an empty string.
  (attributes
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive string-copy
    (safe)
  (signatures
   ((T:string)		=> (T:void)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive string-copy!
    (safe)
  (signatures
   ((T:string T:fixnum T:string T:fixnum T:fixnum)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive string-append
    (safe)
  (signatures
   (T:string			=> (T:string)))
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive string-reverse-and-concatenate
    (safe)
  (signatures
   ((T:proper-list)		=> (T:string)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive string-length
    (safe)
  (signatures
   ((T:string)		=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements $string-length))

(declare-core-primitive string-for-each
    (safe)
  (signatures
   ((T:procedure T:string . T:string)		=> (T:void)))
  (attributes
   ;;Not foldable and not effect-free because it applies an unknown procedure.
   ((_ _ . _)					result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

;;FIXME  This cannot  have $STRING-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive string-ref
    (safe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

;;FIXME This  cannot have  $STRING-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the string.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive string-set!
    (safe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))

(declare-core-primitive string-fill!
    (safe)
  (signatures
   ((T:string T:char)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-string-binary/multi-comparison string<=?)
(declare-string-binary/multi-comparison string<?)
(declare-string-binary/multi-comparison string=?)
(declare-string-binary/multi-comparison string>=?)
(declare-string-binary/multi-comparison string>?)

(declare-string-binary/multi-comparison string-ci<=?)
(declare-string-binary/multi-comparison string-ci<?)
(declare-string-binary/multi-comparison string-ci=?)
(declare-string-binary/multi-comparison string-ci>=?)
(declare-string-binary/multi-comparison string-ci>?)

;;; --------------------------------------------------------------------
;;; transformation

(declare-string-unary string-titlecase)
(declare-string-unary string-upcase)
(declare-string-unary string-downcase)
(declare-string-unary string-foldcase)

(declare-string-unary string-normalize-nfc)
(declare-string-unary string-normalize-nfd)
(declare-string-unary string-normalize-nfkc)
(declare-string-unary string-normalize-nfkd)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive string->flonum
    (safe)
  (signatures
   ((T:string)		=> (T:flonum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive string->number
    (safe)
  (signatures
   ((T:string)		=> (T:number/false))
   ((T:string T:fixnum)	=> (T:number/false)))
  (attributes
   ((_)			foldable effect-free)
   ((_ _)		foldable effect-free)))

(declare-core-primitive string->utf8
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string->utf16
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector))
   ((T:string T:symbol)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_ _)			effect-free result-true)))

(declare-core-primitive string->utf32
    (safe)
  (signatures
   ((T:string)			=> (T:bytevector))
   ((T:string T:symbol)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive string->bytevector
    (safe)
  (signatures
   ((T:string T:transcoder)	=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector at every application.
   ((_ _)			effect-free result-true)))

(let-syntax
    ((declare-string->bytevector-conversion
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((T:string)		=> (T:bytevector)))
	   (attributes
	    ;;Not  foldable  because  it  must  return  a  new  bytevector  at  every
	    ;;application.
	    ((_ )		effect-free result-true))))
	)))
  (declare-string->bytevector-conversion string->ascii)
  (declare-string->bytevector-conversion string->latin1)
  (declare-string->bytevector-conversion string->octets)
  (declare-string->bytevector-conversion string->percent-encoding)
  (declare-string->bytevector-conversion string->uri-encoding)
  (declare-string->bytevector-conversion string->utf16be)
  (declare-string->bytevector-conversion string->utf16le)
  (declare-string->bytevector-conversion string->utf16n)
  (declare-string->bytevector-conversion string-base64->bytevector)
  (declare-string->bytevector-conversion string-hex->bytevector)
  #| end of LET-SYNTAX |# )

;;;

(declare-core-primitive string->symbol
    (safe)
  (signatures
   ((T:string)			=> (T:symbol)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive string-or-symbol->string
    (safe)
  (signatures
   ((T:string)			=> (T:string))
   ((T:symbol)			=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_)				effect-free result-true)))

(declare-core-primitive string-or-symbol->symbol
    (safe)
  (signatures
   ((T:string)			=> (T:symbol))
   ((T:symbol)			=> (T:symbol)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive string->keyword
    (safe)
  (signatures
   ((T:string)			=> (T:other-object)))
  (attributes
   ;;Not foldable because keywords cannot be serialised in fasl files.
   ((_)				effect-free result-true)))

(declare-core-primitive string->list
    (safe)
  (signatures
   ((T:string)			=> (T:proper-list)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_)				effect-free result-true)))


;;;; strings, unsafe functions

;;; predicates

(declare-string-predicate $string-empty?)
(declare-string-predicate $octets-encoded-string?)
(declare-string-predicate $ascii-encoded-string?)
(declare-string-predicate $latin1-encoded-string?)
(declare-string-predicate $uri-encoded-string?)
(declare-string-predicate $percent-encoded-string?)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive $make-string
    (unsafe)
  (signatures
   ((T:fixnum)		=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string
    (unsafe)
  (signatures
   (T:char		=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string every time.
   (_			effect-free result-true)))

(declare-core-primitive $string-concatenate
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:string)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

(declare-core-primitive $string-reverse-and-concatenate
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:string)))
  (attributes
   ((_ ())			foldable effect-free result-true)
   ;;Not foldable because it must return a new string every time.
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $string-length
    (unsafe)
  (signatures
   ((T:string)		=> (T:fixnum)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $string-total-length
    (unsafe)
  (signatures
   ((T:exact-integer T:proper-list)	=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $string-ref
    (unsafe)
  (signatures
   ((T:string T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free result-true)))

(declare-core-primitive $string-set!
    (unsafe)
  (signatures
   ((T:string T:fixnum T:char)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-string-binary-comparison $string=)

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive $string->ascii
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->octets
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->latin1
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string-base64->bytevector
    (unsafe)
  (signatures
   ((T:string)		=> (T:bytevector)))
  (attributes
   ;;Not foldable because it must return a new bytevector every time.
   ((_)			effect-free result-true)))

(declare-core-primitive $string->symbol
    (unsafe)
  (signatures
   ((T:string)		=> (T:symbol)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $interned-strings
    (unsafe)
  (signatures
   (()			=> (T:vector)))
  (attributes
   ((_)			effect-free result-true)))


;;;; vectors, safe functions
;;
;;According to R6RS:  VECTOR and MAKE-VECTOR must return a  newly allocated string at
;;every invocation; so they are not foldable.
;;

;;; predicates

(declare-type-predicate vector? T:vector)

(declare-vector-predicate vector-empty?)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive vector
    (safe)
  (signatures
   (()				=> (T:vector))
   (_				=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (()				effect-free result-true)
   (_				effect-free result-true)))

(declare-core-primitive subvector
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive make-vector
    (safe)
  (signatures
   ((T:fixnum)			=> (T:vector))
   ((T:fixnum _)		=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive vector-resize
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum)		=> (T:vector))
   ((T:vector T:non-negative-fixnum T:object)	=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive vector-append
    (safe)
  (signatures
   (T:vector			=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   (_				effect-free result-true)))

(declare-core-primitive vector-copy
    (safe)
  (signatures
   ((T:vector)			=> (T:vector)))
  ;;Not foldable because it must return a newly allocated vector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive vector-length
    (safe)
  (signatures
   ((T:vector)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $vector-length))

;;; --------------------------------------------------------------------
;;; accessors and mutators

;;FIXME  This cannot  have $VECTOR-REF  as  replacement because  there is  no way  to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-ref
    (safe)
  (signatures
   ((T:vector T:fixnum)	=> (T:char)))
  (attributes
   ((_ _)		foldable effect-free)))

;;FIXME This  cannot have  $VECTOR-SET!  as  replacement because there  is no  way to
;;validate the index with respect to the vector.  But in future another primitive can
;;be added that does not validate the  types, but validates the range.  (Marco Maggi;
;;Mon Nov 10, 2014)
(declare-core-primitive vector-set!
    (safe)
  (signatures
   ((T:vector T:fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))

(declare-core-primitive vector-copy!
    (safe)
  (signatures
   ((T:vector T:non-negative-fixnum T:vector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:void)))
  ;;Not foldable  because it must return  a newly allocated vector.   Not effect free
  ;;because it mutates the operand.
  (attributes
   ((_ _ _ _ _)			result-true)))

(declare-core-primitive vector-fill!
    (safe)
  (signatures
   ((T:vector T:object)		=> (T:void)))
  ;;Not effect free because it mutates the operand.
  (attributes
   ((_ _)			foldable result-true)))

;;; --------------------------------------------------------------------
;;; sorting

(declare-core-primitive vector-sort
    (safe)
  (signatures
   ((T:procedure T:vector)	=> (T:vector)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free because it invokes an unknown procedure.
   ((_ _)			result-true)))

(declare-core-primitive vector-sort!
    (safe)
  (signatures
   ((T:procedure T:vector)	=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free because it invokes an unknown procedure and it mutates the operand.
   ((_ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive vector-for-each
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-map
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:vector)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-for-all
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

(declare-core-primitive vector-exists
    (safe)
  (signatures
   ((T:procedure T:vector . T:vector)		=> (T:void)))
  (attributes
   ;;Not  foldable because  it must  return  a new  list at  every application.   Not
   ;;effect-free becuse it invokes an unknown procedure.
   ((_ _ . _)			result-true)))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive vector->list
    (safe)
  (signatures
   ((T:vector)			=> (T:proper-list)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_)				effect-free result-true)))


;;;; vectors, unsafe functions

;;; constructors

(declare-core-primitive $make-vector
    (unsafe)
  (signatures
   ((T:fixnum)			=> (T:vector)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-vector-predicate $vector-empty? unsafe)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $vector-length
    (unsafe)
  (signatures
   ((T:vector)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $vector-ref
    (unsafe)
  (signatures
   ((T:vector T:fixnum)		=> (T:char)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $vector-set!
    (unsafe)
  (signatures
   ((T:vector T:fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))

;;; --------------------------------------------------------------------
;;; iterations

(declare-core-primitive $vector-map1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:vector)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-each1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive $vector-for-all1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:object))))

(declare-core-primitive $vector-exists1
    (unsafe)
  (signatures
   ((T:procedure T:vector)	=> (T:vector))))


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
   ((T:bytevector T:fixnum)	=> (T:byte)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bytevector-u8-ref
    (safe)
  (signatures
   ((T:bytevector T:fixnum)	=> (T:octet)))
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
  (declare-safe-bytevector-accessor bytevector-u64-native-ref		T:sint64)
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
  (declare-safe-bytevector-accessor bytevector-u64-ref		T:sint64)
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
  (declare-bytevector->string-conversion utf8->string)
  (declare-bytevector->string-conversion utf16le->string)
  (declare-bytevector->string-conversion utf16n->string)
  (declare-bytevector->string-conversion utf16be->string)
  (declare-bytevector->string-conversion ascii->string)
  (declare-bytevector->string-conversion bytevector->string-base64)
  (declare-bytevector->string-conversion bytevector->string-hex)
  (declare-bytevector->string-conversion latin1->string)
  (declare-bytevector->string-conversion octets->string)
  (declare-bytevector->string-conversion percent-encoding->string)
  (declare-bytevector->string-conversion uri-encoding->string)
  #| end of LET-SYNTAX |# )

(declare-core-primitive utf16->string
    (safe)
  (signatures
   ((T:bytevector T:symbol)		=> (T:string))
   ((T:bytevector T:symbol T:object)	=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_ _) 		effect-free result-true)
   ((_ _ _) 		effect-free result-true)))

(declare-core-primitive utf32->string
    (safe)
  (signatures
   ((T:bytevector T:symbol)		=> (T:string))
   ((T:bytevector T:symbol T:object)	=> (T:string)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_ _) 		effect-free result-true)
   ((_ _ _) 		effect-free result-true)))

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


;;;; structs, safe primitives

;;; predicates

(declare-core-primitive struct?
    (safe)
  (signatures
   ((T:struct)				=> (T:true))
   ((_)					=> (T:boolean))
   ((T:struct T:struct-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_)					foldable effect-free)
   ((_ _)				foldable effect-free)))

(declare-type-predicate struct-type-descriptor? T:struct-type-descriptor)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-struct-type
    (safe)
  (signatures
   ((T:string T:proper-list)		=> (T:struct-type-descriptor))
   ((T:string T:proper-list T:symbol)	=> (T:struct-type-descriptor)))
  (attributes
   ((_ _)		foldable effect-free result-true)
   ((_ _ _)		foldable effect-free result-true)))


;;; --------------------------------------------------------------------
;;; comparison

(declare-core-primitive struct=?
    (safe)
  (signatures
   ((T:struct T:struct)			=> (T:boolean)))
  (attributes
   ((_ _)		foldable effect-free)))

;;; --------------------------------------------------------------------
;;; struct type descriptor accessors and mutators

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:struct-type-descriptor)		=> (?return-value-tag)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare struct-type-name		T:string)
  (declare struct-type-symbol		T:symbol)
  (declare struct-type-field-names	T:proper-list)
  (declare struct-type-destructor	(or T:false T:procedure))
  (declare struct-constructor		T:procedure)
  (declare struct-predicate		T:procedure)
  #| end of LET-SYNTAX |# )

(declare-core-primitive struct-field-accessor
    (safe)
  (signatures
   ((T:struct-type-descriptor (or T:non-negative-fixnum T:symbol))	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive struct-field-mutator
    (safe)
  (signatures
   ((T:struct-type-descriptor (or T:non-negative-fixnum T:symbol))	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?new-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:struct-type-descriptor ?new-value-tag)	=> (T:void)))
		   (attributes
		    ((_ _)		result-true))))
		)))
  (declare set-rtd-printer!	T:procedure)
  (declare set-rtd-destructor!	T:procedure)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; struct instance accessors and mutators

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:struct)		=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare struct-rtd		T:struct-type-descriptor)
  (declare struct-name		T:string)
  (declare struct-length	T:non-negative-fixnum)
  (declare struct-printer	T:procedure)
  (declare struct-destructor	(or T:false T:procedure))
  #| end of LET-SYNTAX |# )

(declare-core-primitive struct-reset
    (safe)
  (signatures
   ((T:struct)		=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive struct-ref
    (safe)
  (signatures
   ((T:struct T:non-negative-fixnum)	=> (T:object)))
  (attributes
   ;;This cannot be foldable because the referenced field may be mutated at run-time.
   ((_ _)		effect-free)))

(declare-core-primitive struct-set!
    (safe)
  (signatures
   ((T:struct T:non-negative-fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive default-struct-printer
    (safe)
  (signatures
   ((T:struct)			=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-parameter struct-guardian-logger	(or T:boolean T:procedure))

(declare-core-primitive struct-guardian-log
    (safe)
  (signatures
   ((T:struct _ T:symbol)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))


;;;; structs, unsafe primitives

;;; constructors

;;The base struct type descriptor is a constant created at process boot time.
(declare-core-primitive base-rtd
    (unsafe)
  (signatures
   (()					=> (T:struct-type-descriptor)))
  (attributes
   (()					effect-free result-true)))

(declare-core-primitive $struct
    (unsafe)
  (signatures
   ((T:struct-type-descriptor . _)	=> (T:struct)))
  (attributes
   ;;It must return a new struct every time.
   ((_ . _)				effect-free result-true)))

(declare-core-primitive $make-struct
    (unsafe)
  (signatures
   ((T:struct-type-descriptor T:non-negative-fixnum)	=> (T:struct)))
  (attributes
   ;;Not foldable: it must return a new struct every time.
   ((_ _)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-core-primitive $struct?
    (unsafe)
  (signatures
   ((T:struct)				=> (T:true))
   ((_)					=> (T:boolean)))
  (attributes
   ((_)					foldable effect-free)))

(declare-core-primitive $struct/rtd?
    (unsafe)
  (signatures
   ((_ T:struct-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_ _)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive $struct-rtd
    (unsafe)
  (signatures
   ((T:struct)			=> (T:struct-type-descriptor)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $struct-ref
    (unsafe)
  (signatures
   ((T:struct T:fixnum)		=> (_)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive $struct-set!
    (unsafe)
  (signatures
   ((T:struct T:fixnum _)	=> (T:void)))
  (attributes
   ((_ _)			foldable result-true)))

;;;

(let-syntax
    ((declare-unsafe-struct-accessor
      (syntax-rules ()
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:struct-type-descriptor)	=> (?return-value-tag)))
	   (attributes
	    ((_)			foldable effect-free))))
	)))
  (declare-unsafe-struct-accessor $std-std		T:struct-type-descriptor)
  (declare-unsafe-struct-accessor $std-name		T:string)
  (declare-unsafe-struct-accessor $std-length		T:non-negative-fixnum)
  (declare-unsafe-struct-accessor $std-fields		T:proper-list)
  (declare-unsafe-struct-accessor $std-printer		T:object)
  (declare-unsafe-struct-accessor $std-symbol		T:object)
  (declare-unsafe-struct-accessor $std-destructor	T:object)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-unsafe-struct-mutator
      (syntax-rules ()
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:struct-type-descriptor ?new-value-tag)	=> (T:void)))))
	)))
  (declare-unsafe-struct-mutator $set-std-std!		T:struct-type-descriptor)
  (declare-unsafe-struct-mutator $set-std-name!		T:string)
  (declare-unsafe-struct-mutator $set-std-length!	T:non-negative-fixnum)
  (declare-unsafe-struct-mutator $set-std-fields!	T:proper-list)
  (declare-unsafe-struct-mutator $set-std-printer!	T:object)
  (declare-unsafe-struct-mutator $set-std-symbol!	T:object)
  (declare-unsafe-struct-mutator $set-std-destructor!	T:object)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $struct-guardian
    (unsafe)
  (signatures
   ((T:struct)		=> (T:void))))


;;;; annotations

(declare-core-primitive annotation?
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

(declare-core-primitive annotation-expression
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))

(declare-core-primitive annotation-stripped
    (safe)
  (signatures
   ((T:other-struct)		=> (_)))
  (attributes
   ((#f)			foldable effect-free result-false)))

(declare-core-primitive annotation-textual-position
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))

(declare-core-primitive annotation-source
    (safe)
  (signatures
   ((T:other-struct)		=> (_))))


;;;; enum sets, safe procedure

(declare-type-predicate enum-set?	T:enum-set)

(declare-core-primitive make-enumeration
    (safe)
  (signatures
   ((T:proper-list)		=> (T:enum-set)))
  (attributes
   ((_)			 effect-free result-true)))

(declare-core-primitive enum-set-constructor
    (safe)
  (signatures
   ((T:enum-set)		=> (T:procedure)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive enum-set-member?
    (safe)
  (signatures
   ((T:symbol T:enum-set)	=> (T:boolean)))
  (attributes
   ((_ _)			 effect-free)))

(declare-core-primitive enum-set-subset?
    (safe)
  (signatures
   ((T:enum-set T:enum-set)	=> (T:boolean)))
  (attributes
   ((_ _)			 effect-free)))

(declare-core-primitive enum-set=?
    (safe)
  (signatures
   ((T:symbol T:enum-set)	=> (T:boolean)))
  (attributes
   ((_ _)			 effect-free)))

;;; --------------------------------------------------------------------
;;; set operations

(declare-core-primitive enum-set-difference
    (safe)
  (signatures
   ((T:enum-set T:enum-set)	=> (T:enum-set)))
  (attributes
   ((_ _)			 effect-free result-true)))

(declare-core-primitive enum-set-intersection
    (safe)
  (signatures
   ((T:enum-set T:enum-set)	=> (T:enum-set)))
  (attributes
   ((_ _)			 effect-free result-true)))

(declare-core-primitive enum-set-union
    (safe)
  (signatures
   ((T:enum-set T:enum-set)	=> (T:enum-set)))
  (attributes
   ((_ _)			 effect-free result-true)))

(declare-core-primitive enum-set-projection
    (safe)
  (signatures
   ((T:enum-set T:enum-set)	=> (T:enum-set)))
  (attributes
   ((_ _)			 effect-free result-true)))

(declare-core-primitive enum-set-complement
    (safe)
  (signatures
   ((T:enum-set)	=> (T:enum-set)))
  (attributes
   ((_)			 effect-free result-true)))

(declare-core-primitive enum-set-universe
    (safe)
  (signatures
   ((T:enum-set)		=> (T:enum-set)))
  (attributes
   ((_)			 effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive enum-set->list
    (safe)
  (signatures
   ((T:enum-set)		=> (T:proper-list)))
  (attributes
   ((_)			 effect-free result-true)))

(declare-core-primitive enum-set-indexer
    (safe)
  (signatures
   ((T:enum-set)	=> (T:procedure)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive make-file-options
    (safe)
  (signatures
   (T:symbol		=> (T:enum-set)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive make-expander-options
    (safe)
  (signatures
   (T:symbol		=> (T:enum-set)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive make-compiler-options
    (safe)
  (signatures
   (T:symbol		=> (T:enum-set)))
  (attributes
   (_			effect-free result-true)))


;;;; R6RS record type descriptors, safe primitives

(declare-type-predicate record-type-descriptor?	T:record-type-descriptor)

(declare-type-predicate record-constructor-descriptor? T:record-constructor-descriptor)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-record-type-descriptor
    (safe)
  (signatures
   ;;name parent uid sealed? opaque? fields
   ((T:symbol [or T:false T:record-type-descriptor] [or T:false T:symbol] _ _ T:vector)
    => (T:record-type-descriptor)))
  (attributes
   ((_ _ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-record-constructor-descriptor
    (safe)
  (signatures
   ((T:record-type-descriptor [or T:false T:record-constructor-descriptor] [or T:false T:procedure])
    => (T:record-constructor-descriptor)))
  (attributes
   ((_ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; procedures generation

(declare-core-primitive record-constructor
    (safe)
  (signatures
   ((T:record-constructor-descriptor)	=> (T:procedure)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive record-predicate
    (safe)
  (signatures
   ((T:record-type-descriptor)		=> (T:procedure)))
  (attributes
   ((_)					effect-free result-true)))

(declare-core-primitive record-accessor
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:false)	=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:symbol)	=> (T:procedure))
   ((T:record-type-descriptor T:symbol)				=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:false)			=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:symbol)		=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive record-mutator
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:false)	=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:symbol)	=> (T:procedure))
   ((T:record-type-descriptor T:symbol)				=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:false)			=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:symbol)		=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive unsafe-record-accessor
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:false)	=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:symbol)	=> (T:procedure))
   ((T:record-type-descriptor T:symbol)				=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:false)			=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:symbol)		=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

(declare-core-primitive unsafe-record-mutator
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:false)	=> (T:procedure))
   ((T:record-type-descriptor T:non-negative-fixnum T:symbol)	=> (T:procedure))
   ((T:record-type-descriptor T:symbol)				=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:false)			=> (T:procedure))
   ((T:record-type-descriptor T:symbol T:symbol)		=> (T:procedure)))
  (attributes
   ((_ _)			effect-free result-true)
   ((_ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive record-field-mutable?
    (safe)
  (signatures
   ((T:record-type-descriptor T:non-negative-fixnum)		=> (T:boolean)))
  (attributes
   ((_ _)		effect-free)))

;;;

(declare-core-primitive record-type-generative?
    (safe)
  (signatures
   ((T:record-type-descriptor)		=> (T:boolean)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive record-type-opaque?
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-sealed?
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-field-names
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> (T:vector)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive record-type-name
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> (T:symbol)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive record-type-parent
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> ([or T:false T:record-type-descriptor])))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive record-type-uid
    (safe)
  (signatures
   ((T:record-type-descriptor)	=> ([or T:false T:symbol])))
  (attributes
   ((_)				effect-free)))

;;; --------------------------------------------------------------------
;;; destructor

(declare-core-primitive record-destructor
    (safe)
  (signatures
   ((T:record-type-descriptor)		=> ([or T:false T:procedure])))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive record-destructor-set!
    (safe)
  (signatures
   ((T:record-type-descriptor T:procedure)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))


;;;; R6RS records, safe primitives

(declare-type-predicate record? T:record)

(declare-type-predicate record-object? T:record)

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive record-and-rtd?
    (safe)
  (signatures
   ((T:record T:record-type-descriptor)		=> (T:boolean)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive record-rtd
    (safe)
  (signatures
   ((T:record)		=> (T:record-type-descriptor)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive record-reset
    (safe)
  (signatures
   ((T:record)		=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive record-guardian-log
    (safe)
  (signatures
   ((T:record _ T:symbol)	=> (T:void)))
  (attributes
   ((_ _ _)			result-true)))

(declare-parameter record-guardian-logger	[or T:boolean T:procedure])


;;;; R6RS records, unsafe primitives

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive $record-guardian
    (unsafe)
  (signatures
   ((T:record)		=> (T:void))))


;;;; hashtables, safe procedures

;;; predicates

(declare-type-predicate hashtable? T:hashtable)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-hashtable
    (safe)
  (signatures
   ((T:procedure T:procedure)			=> (T:hashtable))
   ((T:procedure T:procedure T:exact-integer)	=> (T:hashtable)))
  (attributes
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive make-eq-hashtable
    (safe)
  (signatures
   (()					=> (T:hashtable))
   ((T:exact-integer)			=> (T:hashtable)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive make-eqv-hashtable
    (safe)
  (signatures
   (()					=> (T:hashtable))
   ((T:exact-integer)			=> (T:hashtable)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive hashtable-copy
    (safe)
  (signatures
   ((T:hashtable)			=> (T:hashtable))
   ((T:hashtable T:object)		=> (T:hashtable)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive hashtable-ref
    (safe)
  (signatures
   ((T:hashtable T:object	)	=> (T:object))
   ((T:hashtable T:object T:object)	=> (T:object)))
  (attributes
   ((_ _)			effect-free)
   ((_ _ _)			effect-free)))

(declare-core-primitive hashtable-set!
    (safe)
  (signatures
   ((T:hashtable T:object T:object)	=> (T:void)))
  (attributes
   ((_ _ _)				result-true)))

(declare-core-primitive hashtable-delete!
    (safe)
  (signatures
   ((T:hashtable T:object)		=> (T:void)))
  (attributes
   ((_ _) 				result-true)))

(declare-core-primitive hashtable-clear!
    (safe)
  (signatures
   ((T:hashtable)			=> (T:void))
   ((T:hashtable T:exact-integer)	=> (T:void)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)))

(declare-core-primitive hashtable-update!
    (safe)
  (signatures
   ((T:hashtable T:object T:procedure T:object)	=> (T:void)))
  (attributes
   ((_ _ _ _)				result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive hashtable-contains?
    (safe)
  (signatures
   ((T:hashtable T:object)	=> (T:boolean)))
  (attributes
   ((_ _)			effect-free)))


(declare-core-primitive hashtable-entries
    (safe)
  (signatures
   ((T:hashtable)		=> (T:vector T:vector)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-keys
    (safe)
  (signatures
   ((T:hashtable)		=> (T:vector)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-mutable?
    (safe)
  (signatures
   ((T:hashtable)		=> (T:boolean)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-size
    (safe)
  (signatures
   ((T:hashtable)		=> (T:exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive hashtable-hash-function
    (safe)
  (signatures
   ((T:hashtable)		=> (T:object)))
  (attributes
   ;;This returns false for EQ? and EQV? hashtables!!!
   ((_)				effect-free)))

(declare-core-primitive hashtable-equivalence-function
    (safe)
  (signatures
   ((T:hashtable)		=> (T:procedure)))
  (attributes
   ((_)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; hash functions

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag . ?replacements)
			       (declare-core-primitive ?who
				   (safe)
				 (signatures
				  ((?obj-tag)		=> (T:fixnum)))
				 (attributes
				  ((_)			foldable effect-free result-true))
				 . ?replacements))
			      )))
  (declare-hash-function equal-hash		T:object)
  (declare-hash-function string-hash		T:string	(replacements $string-hash))
  (declare-hash-function string-ci-hash		T:string	(replacements $string-ci-hash))
  (declare-hash-function symbol-hash		T:symbol	(replacements $symbol-hash))
  (declare-hash-function bytevector-hash	T:bytevector	(replacements $bytevector-hash))
  (declare-hash-function port-hash		T:port)
  #| end of LET-SYNTAX |# )


;;;; hashtables, unsafe procedures

;;; hash functions

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag)
			       (declare-core-primitive ?who
				   (unsafe)
				 (signatures
				  ((?obj-tag)		=> (T:fixnum)))
				 (attributes
				  ((_)			foldable effect-free result-true))))
			      )))
  (declare-hash-function $string-hash		T:string)
  (declare-hash-function $string-ci-hash	T:string)
  (declare-hash-function $symbol-hash		T:symbol)
  (declare-hash-function $bytevector-hash	T:bytevector)
  #| end of LET-SYNTAX |# )


;;;; condition objects, safe procedures

;;; generic condition procedures

(declare-type-predicate condition? T:condition)

(declare-core-primitive condition
    (safe)
  (signatures
   (T:condition		=> (T:condition)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive simple-conditions
    (safe)
  (signatures
   ((T:condition)		=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive condition-predicate
    (safe)
  (signatures
   ((T:object)		=> (T:boolean)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive condition-accessor
    (safe)
  (signatures
   ((T:record-type-descriptor T:procedure)	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive print-condition
    (safe)
  (signatures
   ((T:condition T:textual-output-port)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------
;;; constructors

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    (()			=> (T:condition)))
		   (attributes
		    (()			effect-free result-true))))
		)))
  (declare make-assertion-violation)
  (declare make-error)
  (declare make-expression-return-value-violation)
  (declare make-i/o-eagain)
  (declare make-i/o-error)
  (declare make-i/o-read-error)
  (declare make-i/o-write-error)
  (declare make-implementation-restriction-violation)
  (declare make-interrupted-condition)
  (declare make-lexical-violation)
  (declare make-no-infinities-violation)
  (declare make-no-nans-violation)
  (declare make-non-continuable-violation)
  (declare make-procedure-argument-violation)
  (declare make-serious-condition)
  (declare make-undefined-violation)
  (declare make-violation)
  (declare make-warning)
  #| end of LET-SYNTAX |# )

(declare-core-primitive make-who-condition
    (safe)
  (signatures
   (([or T:false T:string T:symbol])	=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-message-condition
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-irritants-condition
    (safe)
  (signatures
   (_			=> (T:condition)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive make-syntax-violation
    (safe)
  (signatures
   ((_ _)		=> (T:condition)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive make-errno-condition
    (safe)
  (signatures
   ((T:fixnum)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-h_errno-condition
    (safe)
  (signatures
   ((T:fixnum)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))


(declare-core-primitive make-i/o-port-error
    (safe)
  (signatures
   ((T:port)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-invalid-position-error
    (safe)
  (signatures
   ((T:object)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-filename-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-protection-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-decoding-error
    (safe)
  (signatures
   ((T:port)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-encoding-error
    (safe)
  (signatures
   ((T:port T:char)	=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-already-exists-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-does-not-exist-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-i/o-file-is-read-only-error
    (safe)
  (signatures
   ((T:string)		=> (T:condition)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive make-source-position-condition
    (safe)
  (signatures
   ((T:string T:exact-integer T:exact-integer T:exact-integer T:exact-integer)
    => (T:condition)))
  (attributes
   ((_ _ _ _ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((_)		=> (T:boolean)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare assertion-violation?)
  (declare errno-condition?)
  (declare error?)
  (declare expression-return-value-violation?)
  (declare h_errno-condition?)
  (declare i/o-decoding-error?)
  (declare i/o-eagain-error?)
  (declare i/o-encoding-error?)
  (declare i/o-error?)
  (declare i/o-file-already-exists-error?)
  (declare i/o-file-does-not-exist-error?)
  (declare i/o-file-is-read-only-error?)
  (declare i/o-file-protection-error?)
  (declare i/o-filename-error?)
  (declare i/o-invalid-position-error?)
  (declare i/o-port-error?)
  (declare i/o-read-error?)
  (declare i/o-write-error?)
  (declare implementation-restriction-violation?)
  (declare interrupted-condition?)
  (declare irritants-condition?)
  (declare lexical-violation?)
  (declare message-condition?)
  (declare no-infinities-violation?)
  (declare no-nans-violation?)
  (declare non-continuable-violation?)
  (declare procedure-argument-violation?)
  (declare serious-condition?)
  (declare source-position-condition?)
  (declare syntax-violation?)
  (declare undefined-violation?)
  (declare violation?)
  (declare warning?)
  (declare who-condition?)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(letrec-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare ?who T:object))
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:condition)	=> (?return-value-tag)))
		   (attributes
		    ((_)		effect-free))))
		)))
  (declare condition-errno		T:fixnum)
  (declare condition-h_errno		T:fixnum)
  (declare condition-irritants)
  (declare condition-message		T:string)
  (declare condition-who		[or T:false T:string T:symbol])
  (declare i/o-encoding-error-char)
  (declare i/o-error-filename)
  (declare i/o-error-port		T:port)
  (declare i/o-error-position)
  (declare syntax-violation-form)
  (declare syntax-violation-subform)

  (declare source-position-port-id	T:string)
  (declare source-position-byte		T:exact-integer)
  (declare source-position-character	T:exact-integer)
  (declare source-position-line		T:exact-integer)
  (declare source-position-column	T:exact-integer)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

;;These are not effect-free because raising an exception is a "side effect".

(declare-core-primitive error
    (safe)
  (signatures
   (([or T:false T:symbol T:string] T:string . T:object)	=> (T:void))))

(declare-core-primitive assertion-violation
    (safe)
  (signatures
   (([or T:false T:symbol T:string] T:string . T:object)	=> (T:void))))

(declare-core-primitive syntax-violation
    (safe)
  (signatures
   (([or T:false T:symbol T:string] T:string T:object)		=> (T:void))
   (([or T:false T:symbol T:string] T:string T:object T:object)	=> (T:void))))

(declare-core-primitive warning
    (safe)
  (signatures
   ;;We do not know the number of returned values.
   (([or T:false T:symbol T:string] T:string . T:object)	=> T:object)))

;;This is deprecated.
(declare-core-primitive die
    (safe)
  (signatures
   (([or T:false T:symbol T:string] T:string . T:object)	=> (T:void))))

(declare-core-primitive procedure-argument-violation
    (safe)
  (signatures
   (([or T:false T:string T:symbol] T:string . _)	=> (T:void))))

(declare-core-primitive expression-return-value-violation
    (safe)
  (signatures
   (([or T:false T:string T:symbol] T:string . _)	=> (T:void))))


;;;; codes and closures, unsafe primitives

(declare-core-primitive $closure-code
    (safe)
  (signatures
   ((T:procedure)	=> (T:code)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $cpref
    (safe)
  (signatures
   ((T:procedure T:non-negative-fixnum)		=> (T:object)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive $make-annotated-procedure
    (safe)
  (signatures
   ((T:object T:procedure)	=> (T:procedure)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive $annotated-procedure-annotation
    (safe)
  (signatures
   ((T:procedure)	=> (T:object)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code->closure
    (safe)
  (signatures
   ((T:code)		=> (T:procedure)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-reloc-vector
    (safe)
  (signatures
   ((T:code)		=> (T:object)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-freevars
    (safe)
  (signatures
   ((T:code)		=> (T:non-negative-fixnum)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive $code-size
    (safe)
  (signatures
   ((T:code)		=> (T:non-negative-exact-integer)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code-annotation
    (safe)
  (signatures
   ((T:code)		=> (T:object)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive $set-code-annotation!
    (safe)
  (signatures
   ((T:code T:object)	=> (T:void)))
  (attributes
   ((_ _)		result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive $code-ref
    (safe)
  (signatures
   ((T:code T:non-negative-exact-integer)	=> (T:octet)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive $code-set!
    (safe)
  (signatures
   ((T:code T:non-negative-exact-integer T:octet)	=> (T:void)))
  (attributes
   ((_ _ _)		result-true)))


;;;; input/output, safe primitives

(declare-parameter current-input-port	T:textual-input-port)
(declare-parameter current-output-port	T:textual-output-port)
(declare-parameter current-error-port	T:textual-output-port)

(declare-object-retriever standard-input-port	T:binary-input-port)
(declare-object-retriever standard-output-port	T:binary-output-port)
(declare-object-retriever standard-error-port	T:binary-output-port)

(declare-parameter console-input-port	T:textual-input-port)
(declare-parameter console-output-port	T:textual-output-port)
(declare-parameter console-error-port	T:textual-output-port)

(declare-parameter print-graph			T:boolean)
(declare-parameter print-unicode		T:boolean)
(declare-parameter printer-integer-radix	T:non-negative-fixnum)

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate port?				T:port)
(declare-type-predicate binary-port?			T:binary-port)
(declare-type-predicate textual-port?			T:textual-port)

(declare-type-predicate input-port?			T:input-port)
(declare-type-predicate output-port?			T:output-port)
(declare-type-predicate input/output-port?		T:input/output-port)

(declare-type-predicate binary-input-port?		T:binary-input-port)
(declare-type-predicate binary-output-port?		T:binary-output-port)
(declare-type-predicate binary-input/output-port?	T:binary-input/output-port)

(declare-type-predicate textual-input-port?		T:textual-input-port)
(declare-type-predicate textual-output-port?		T:textual-output-port)
(declare-type-predicate textual-input/output-port?	T:textual-input/output-port)

(declare-core-primitive port-eof?
    (safe)
  (signatures
   ((T:input-port)	=> (T:boolean)))
  ;;Not foldable because EOF is an internal, run-time state.  Not effect-free because
  ;;it requires a lookahead, which consumes data from the underlying device.
  (attributes))

(declare-core-primitive port-closed?
    (safe)
  (signatures
   ((T:port)		=> (T:boolean)))
  (attributes
   ;;Not foldable because "port closed" is an internal, run-time state.
   ((_)			effect-free)))

(declare-port-predicate port-has-port-position?)
(declare-port-predicate port-has-set-port-position!?)
(declare-port-predicate port-in-non-blocking-mode?)

;;; --------------------------------------------------------------------
;;; constructors

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:file-descriptor T:string)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free result-true))))
		)))
  (declare make-binary-file-descriptor-input-port		T:binary-input-port)
  (declare make-binary-file-descriptor-input-port*		T:binary-input-port)
  (declare make-binary-file-descriptor-output-port		T:binary-output-port)
  (declare make-binary-file-descriptor-output-port*		T:binary-output-port)
  (declare make-binary-file-descriptor-input/output-port	T:binary-input/output-port)
  (declare make-binary-file-descriptor-input/output-port*	T:binary-input/output-port)

  (declare make-binary-socket-input-port			T:binary-input-port)
  (declare make-binary-socket-input-port*			T:binary-input-port)
  (declare make-binary-socket-output-port			T:binary-output-port)
  (declare make-binary-socket-output-port*			T:binary-output-port)
  (declare make-binary-socket-input/output-port			T:binary-input/output-port)
  (declare make-binary-socket-input/output-port*		T:binary-input/output-port)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:file-descriptor T:string T:transcoder)	=> (?return-value-tag)))
		   (attributes
		    ((_ _)		effect-free result-true))))
		)))
  (declare make-textual-file-descriptor-input-port		T:textual-input-port)
  (declare make-textual-file-descriptor-input-port*		T:textual-input-port)
  (declare make-textual-file-descriptor-output-port		T:textual-output-port)
  (declare make-textual-file-descriptor-output-port*		T:textual-output-port)
  (declare make-textual-file-descriptor-input/output-port	T:textual-input/output-port)
  (declare make-textual-file-descriptor-input/output-port*	T:textual-input/output-port)

  (declare make-textual-socket-input-port			T:textual-input-port)
  (declare make-textual-socket-input-port*			T:textual-input-port)
  (declare make-textual-socket-output-port			T:textual-output-port)
  (declare make-textual-socket-output-port*			T:textual-output-port)
  (declare make-textual-socket-input/output-port		T:textual-input/output-port)
  (declare make-textual-socket-input/output-port*		T:textual-input/output-port)
  #| end of LET-SYNTAX |# )

;;;

(declare-core-primitive make-custom-binary-input-port
    (safe)
  (signatures
   ;;id read! get-position set-position close
   ((T:string T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))	=> (T:binary-input-port)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-custom-textual-input-port
    (safe)
  (signatures
   ;;id read! get-position set-position close
   ((T:string T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))	=> (T:textual-input-port)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

;;;

(declare-core-primitive make-custom-binary-output-port
    (safe)
  (signatures
   ;;id write! get-position set-position close
   ((T:string T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))	=> (T:binary-output-port)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

(declare-core-primitive make-custom-textual-output-port
    (safe)
  (signatures
   ;;id write! get-position set-position close
   ((T:string T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))	=> (T:textual-output-port)))
  (attributes
   ((_ _ _ _ _)		effect-free result-true)))

;;;

(declare-core-primitive make-custom-binary-input/output-port
    (safe)
  (signatures
   ;;id read! write! get-position set-position close
   ((T:string T:procedure T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))
    => (T:binary-input/output-port)))
  (attributes
   ((_ _ _ _ _ _)	effect-free result-true)))

(declare-core-primitive make-custom-textual-input/output-port
    (safe)
  (signatures
   ;;id read! write! get-position set-position close
   ((T:string T:procedure T:procedure (or T:false T:procedure) (or T:false T:procedure) (or T:false T:procedure))
    => (T:textual-input/output-port)))
  (attributes
   ((_ _ _ _ _ _)	effect-free result-true)))

;;;

(declare-core-primitive open-input-file
    (safe)
  (signatures
   ((T:string)		=> (T:textual-input-port)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive open-output-file
    (safe)
  (signatures
   ((T:string)		=> (T:textual-output-port)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive open-file-input-port
    (safe)
  (signatures
   ((T:string)					=> (T:binary-input-port))
   ((T:string T:enum-set)			=> (T:binary-input-port))
   ((T:string T:enum-set T:symbol)		=> (T:binary-input-port))
   ((T:string T:enum-set T:symbol T:false)	=> (T:binary-input-port))
   ((T:string T:enum-set T:symbol T:transcoder)	=> (T:textual-input-port)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

(declare-core-primitive open-file-output-port
    (safe)
  (signatures
   ((T:string)					=> (T:binary-output-port))
   ((T:string T:enum-set)			=> (T:binary-output-port))
   ((T:string T:enum-set T:symbol)		=> (T:binary-output-port))
   ((T:string T:enum-set T:symbol T:false)	=> (T:binary-output-port))
   ((T:string T:enum-set T:symbol T:transcoder)	=> (T:textual-output-port)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

(declare-core-primitive open-file-input/output-port
    (safe)
  (signatures
   ((T:string)					=> (T:binary-input/output-port))
   ((T:string T:enum-set)			=> (T:binary-input/output-port))
   ((T:string T:enum-set T:symbol)		=> (T:binary-input/output-port))
   ((T:string T:enum-set T:symbol T:false)	=> (T:binary-input/output-port))
   ((T:string T:enum-set T:symbol T:transcoder)	=> (T:textual-input/output-port)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)
   ((_ _ _)				result-true)
   ((_ _ _ _)				result-true)))

;;;

(declare-core-primitive open-string-input-port
    (safe)
  (signatures
   ((T:string)		=> (T:textual-input-port))
   ((T:string T:symbol)	=> (T:textual-input-port)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive open-string-input-port/id
    (safe)
  (signatures
   ((T:string T:string)			=> (T:textual-input-port))
   ((T:string T:string T:symbol)	=> (T:textual-input-port)))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-core-primitive open-string-output-port
    (safe)
  (signatures
   (()			=> (T:textual-output-port T:procedure))
   ((T:symbol)		=> (T:textual-output-port T:procedure)))
  (attributes
   (()			effect-free)))

;;;

(declare-core-primitive open-bytevector-input-port
    (safe)
  (signatures
   ((T:bytevector)		=> (T:binary-input-port))
   ((T:bytevector T:false)	=> (T:binary-input-port))
   ((T:bytevector T:transcoder)	=> (T:textual-input-port)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive open-bytevector-output-port
    (safe)
  (signatures
   (()			=> (T:binary-output-port T:procedure))
   ((T:false)		=> (T:binary-output-port T:procedure))
   ((T:transcoder)	=> (T:textual-output-port T:procedure)))
  (attributes
   (()			effect-free)
   ((_)			effect-free)))

;;;

(declare-core-primitive call-with-bytevector-output-port
    (safe)
  (signatures
   ((T:procedure)			=> T:object)
   ((T:procedure T:false)		=> T:object)
   ((T:procedure T:transcoder)		=> T:object)))

(declare-core-primitive call-with-string-output-port
    (safe)
  (signatures
   ((T:procedure)			=> T:object)))

(declare-core-primitive call-with-input-file
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive call-with-output-file
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive call-with-port
    (safe)
  (signatures
   ((T:port T:procedure)		=> T:object)))

(declare-core-primitive with-input-from-file
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive with-output-to-file
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive with-input-from-string
    (safe)
  (signatures
   ((T:string T:procedure)		=> T:object)))

(declare-core-primitive with-output-to-string
    (safe)
  (signatures
   ((T:procedure)			=> T:object)))

(declare-core-primitive transcoded-port
    (safe)
  (signatures
   ((T:binary-port T:transcoder)	=> (T:textual-port)))
  (attributes
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; closing

(declare-core-primitive close-port
    (safe)
  (signatures
   ((T:port)		=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive close-input-port
    (safe)
  (signatures
   ((T:input-port)	=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive close-output-port
    (safe)
  (signatures
   ((T:output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive port-id
    (safe)
  (signatures
   ((T:port)		=> (T:string)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-uid
    (safe)
  (signatures
   ((T:port)		=> (T:symbol)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-fd
    (safe)
  (signatures
   ((T:port)		=> ((or T:false T:file-descriptor))))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive port-transcoder
    (safe)
  (signatures
   ((T:port)		=> ((or T:false T:transcoder))))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive output-port-buffer-mode
    (safe)
  (signatures
   ((T:output-port)		=> (T:symbol)))
  (attributes
   ((_)				effect-free result-true)))

;;;

(declare-core-primitive port-set-non-blocking-mode!
    (safe)
  (signatures
   ((T:port)		=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive port-unset-non-blocking-mode!
    (safe)
  (signatures
   ((T:port)		=> (T:void)))
  (attributes
   ((_)			result-true)))

;;;

(declare-core-primitive port-mode
    (safe)
  (signatures
   ((T:port)		=> (T:symbol)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive set-port-mode!
    (safe)
  (signatures
   ((T:port T:symbol)		=> (T:void)))
  (attributes
   ((_ _)			result-true)))

;;;

(declare-core-primitive set-port-buffer-mode!
    (safe)
  (signatures
   ((T:port T:symbol)		=> (T:void)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive port-dump-status
    (safe)
  (signatures
   ((T:port)			=> (T:void)))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive port-position
    (safe)
  (signatures
   ((T:port)			=> (T:non-negative-exact-integer)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive port-textual-position
    (safe)
  (signatures
   ((T:textual-port)		=> (T:record)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive set-port-position!
    (safe)
  (signatures
   ((T:port T:exact-integer)	=> (T:void)))
  (attributes
   ((_ _)			result-true)))

;;;

(declare-core-primitive port-putprop
    (safe)
  (signatures
   ((T:port T:symbol T:object)		=> (T:void)))
  (attributes
   ((_ _ _)				result-true)))

(declare-core-primitive port-getprop
    (safe)
  (signatures
   ((T:port T:symbol)			=> (T:object)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive port-remprop
    (safe)
  (signatures
   ((T:port T:symbol)			=> (T:void)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive port-property-list
    (safe)
  (signatures
   ((T:port)		=> (T:proper-list)))
  (attributes
   ((_)			effect-free result-true)))

;;;

(declare-core-primitive reset-input-port!
    (safe)
  (signatures
   ((T:input-port)	=> (T:void)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive reset-output-port!
    (safe)
  (signatures
   ((T:input-port)	=> (T:void)))
  (attributes
   ((_)			result-true)))

;;;

(declare-core-primitive get-output-string
    (safe)
  (signatures
   ((T:textual-input-port)	=> (T:string)))
  (attributes
   ((_)				result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-parameter bytevector-port-buffer-size		T:non-negative-fixnum)
(declare-parameter string-port-buffer-size		T:non-negative-fixnum)
(declare-parameter input-file-buffer-size		T:non-negative-fixnum)
(declare-parameter output-file-buffer-size		T:non-negative-fixnum)
(declare-parameter input/output-file-buffer-size	T:non-negative-fixnum)
(declare-parameter input/output-socket-buffer-size	T:non-negative-fixnum)

;;; --------------------------------------------------------------------
;;; input procedures

(declare-core-primitive get-bytevector-all
    (safe)
  (signatures
   ((T:binary-input-port)		=> ((or T:eof T:would-block T:bytevector))))
  (attributes
   ((_)					result-true)))

(declare-core-primitive get-bytevector-n
    (safe)
  (signatures
   ((T:binary-input-port T:non-negative-fixnum)		=> ((or T:eof T:would-block T:bytevector))))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive get-bytevector-n!
    (safe)
  (signatures
   ((T:binary-input-port T:bytevector T:non-negative-fixnum T:non-negative-fixnum)
    => ((or T:eof T:would-block T:bytevector))))
  (attributes
   ((_ _ _ _)			result-true)))

(declare-core-primitive get-bytevector-some
    (safe)
  (signatures
   ((T:binary-input-port)	=> ((or T:eof T:would-block T:bytevector))))
  (attributes
   ((_ _ _ _)			result-true)))

;;;

(declare-core-primitive get-string-all
    (safe)
  (signatures
   ((T:textual-input-port)		=> ((or T:eof T:would-block T:string))))
  (attributes
   ((_)					result-true)))

(declare-core-primitive get-string-n
    (safe)
  (signatures
   ((T:textual-input-port T:non-negative-fixnum)	=> ((or T:eof T:would-block T:string))))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive get-string-n!
    (safe)
  (signatures
   ((T:textual-input-port T:string T:non-negative-fixnum T:non-negative-fixnum)
    => ((or T:eof T:would-block T:string))))
  (attributes
   ((_ _ _ _)			result-true)))

(declare-core-primitive get-string-some
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:string))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive get-u8
    (safe)
  (signatures
   ((T:binary-input-port)	=> ((or T:eof T:would-block T:octet))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-char
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-char-and-track-textual-position
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-datum
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:object))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive get-line
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:string))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive lookahead-u8
    (safe)
  (signatures
   ((T:binary-input-port)	=> ((or T:eof T:would-block T:octet))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive lookahead-two-u8
    (safe)
  (signatures
   ((T:binary-input-port)	=> ((or T:eof T:would-block T:octet)
				    (or T:eof T:would-block T:octet))))
  (attributes
   ((_)				result-true)))

(declare-core-primitive lookahead-char
    (safe)
  (signatures
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive read
    (safe)
  (signatures
   (()				=> (T:object))
   ((T:textual-input-port)	=> (T:object))))

(declare-core-primitive read-char
    (safe)
  (signatures
   (()				=> ((or T:eof T:would-block T:char)))
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

(declare-core-primitive peek-char
    (safe)
  (signatures
   (()				=> ((or T:eof T:would-block T:char)))
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:char))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

(declare-core-primitive read-line
    (safe)
  (signatures
   (()				=> ((or T:eof T:would-block T:string)))
   ((T:textual-input-port)	=> ((or T:eof T:would-block T:string))))
  (attributes
   (()				result-true)
   ((_)				result-true)))

;;; --------------------------------------------------------------------
;;; output procedures

(declare-core-primitive put-bytevector
    (safe)
  (signatures
   ((T:binary-output-port T:bytevector)							=> (T:void))
   ((T:binary-output-port T:bytevector T:non-negative-fixnum)				=> (T:void))
   ((T:binary-output-port T:bytevector T:non-negative-fixnum T:non-negative-fixnum)	=> (T:void)))
  (attributes
   ((_ _)			result-true)
   ((_ _ _)			result-true)
   ((_ _ _ _)			result-true)))

(declare-core-primitive put-string
    (safe)
  (signatures
   ((T:textual-output-port T:string)							=> (T:void))
   ((T:textual-output-port T:string T:non-negative-fixnum)				=> (T:void))
   ((T:textual-output-port T:string T:non-negative-fixnum T:non-negative-fixnum)	=> (T:void)))
  (attributes
   ((_ _)			result-true)
   ((_ _ _)			result-true)
   ((_ _ _ _)			result-true)))

(declare-core-primitive put-u8
    (safe)
  (signatures
   ((T:binary-output-port T:octet)		=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive put-char
    (safe)
  (signatures
   ((T:textual-output-port T:char)		=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive put-datum
    (safe)
  (signatures
   ((T:textual-output-port T:object)		=> (T:void)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive write-char
    (safe)
  (signatures
   ((T:char)				=> (T:void))
   ((T:char T:textual-output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive write
    (safe)
  (signatures
   ((T:object)				=> (T:void))
   ((T:object T:textual-output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive display
    (safe)
  (signatures
   ((T:object)				=> (T:void))
   ((T:object T:textual-output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive newline
    (safe)
  (signatures
   (()				=> (T:void))
   (( T:textual-output-port)	=> (T:void)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

;;;

(declare-core-primitive flush-output-port
    (safe)
  (signatures
   ((T:output-port)		=> (T:void)))
  (attributes
   ((_)				result-true)))

;;;

(declare-core-primitive format
    (safe)
  (signatures
   ((T:string . T:object)	=> (T:void)))
  (attributes
   ((_ . _)		result-true)))

(declare-core-primitive printf
    (safe)
  (signatures
   ((T:string . T:object)	=> (T:void)))
  (attributes
   ((_ . _)		result-true)))

(declare-core-primitive fprintf
    (safe)
  (signatures
   ((T:textual-output-port T:string . T:object)	=> (T:void)))
  (attributes
   ((_ _ . _)		result-true)))

;;; --------------------------------------------------------------------
;;; special values

(declare-object-retriever eof-object)
(declare-object-predicate eof-object?)

(declare-object-retriever would-block-object)
(declare-object-predicate would-block-object?)

(declare-core-primitive buffer-mode?
    (safe)
  (signatures
   ((T:symbol)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; pretty printing

(declare-core-primitive pretty-print
    (safe)
  (signatures
   ((T:object)				=> (T:void))
   ((T:object T:textual-output-port)	=> (T:void)))
  (attributes
   ((_)			result-true)
   ((_ _)		result-true)))

(declare-core-primitive pretty-print*
    (safe)
  (signatures
   ((T:object T:textual-output-port T:non-negative-fixnum _)	=> (T:void)))
  (attributes
   ((_ _ _ _)		result-true)))

(declare-parameter pretty-width		T:non-negative-exact-integer)

(declare-core-primitive pretty-format
    (safe)
  (signatures
   ((T:object)		=> (T:procedure)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive debug-print
    (safe)
  (signatures
   (T:object		=> (T:void)))
  (attributes
   (_			result-true)))

(declare-core-primitive debug-print*
    (safe)
  (signatures
   (T:object		=> (T:void)))
  (attributes
   (_			result-true)))

(declare-parameter debug-print-enabled?)


;;;; input/output, unsafe primitives

;;; constructors

(declare-core-primitive $make-port
    (unsafe)
  (signatures
   ;;attrs                 idx                   buffer-size           buffer
   ((T:non-negative-fixnum T:non-negative-fixnum T:non-negative-fixnum T:object
			   ;;transcoder
			   (or T:transcoder T:boolean)
			   ;;id     read     write    getp     setp     close    cookie
			   T:object T:object T:object T:object T:object T:object T:object)	=> (T:port)))
  (attributes
   ;;FIXME Should this be foldable?  (Marco Maggi; Wed Nov 26, 2014)
   ((_ _ _  _ _ _  _ _ _  _ _ _)		effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(letrec-syntax
    ((declare-unsafe-port-accessor
      (syntax-rules ()
	((_ ?who)
	 (declare-unsafe-port-accessor ?who T:object))
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:port)			=> (T:object)))
	   (attributes
	    ((_)			effect-free))))
	)))
  (declare-unsafe-port-accessor $port-tag		T:fixnum)
  (declare-unsafe-port-accessor $port-attrs		T:non-negative-fixnum)
  (declare-unsafe-port-accessor $port-index		T:non-negative-fixnum)
  (declare-unsafe-port-accessor $port-size		T:non-negative-fixnum)
  (declare-unsafe-port-accessor $port-buffer)
  (declare-unsafe-port-accessor $port-transcoder	T:transcoder)
  (declare-unsafe-port-accessor $port-cookie)
  (declare-unsafe-port-accessor $port-get-position)
  (declare-unsafe-port-accessor $port-close)
  (declare-unsafe-port-accessor $port-id)
  (declare-unsafe-port-accessor $port-read!)
  (declare-unsafe-port-accessor $port-write!)
  (declare-unsafe-port-accessor $port-set-position!)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-unsafe-port-mutator
      (syntax-rules ()
	((_ ?who)
	 (declare-unsafe-port-mutator ?who T:object))
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:port ?new-value-tag)	=> (T:void)))
	   (attributes
	    ((_ _)			result-true))))
	)))
  (declare-unsafe-port-mutator $set-port-index!		T:non-negative-fixnum)
  (declare-unsafe-port-mutator $set-port-size!		T:non-negative-fixnum)
  (declare-unsafe-port-mutator $set-port-attrs!		T:non-negative-fixnum)
  #| end of LET-SYNTAX |# )


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


;;;; exceptions and dynamic environment, safe procedures

(declare-core-primitive with-exception-handler
    (safe)
  (signatures
   ((T:procedure T:procedure)	=> T:object)))

(declare-core-primitive dynamic-wind
    (safe)
  (signatures
   ((T:procedure T:procedure T:procedure)	=> T:object)))

;;; --------------------------------------------------------------------

(declare-core-primitive raise
    (safe)
  (signatures
   ((T:object)		=> T:object)))

(declare-core-primitive raise-continuable
    (safe)
  (signatures
   ((T:object)		=> T:object)))


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

(declare-core-primitive pointer-value
    (safe)
  (signatures
   ((T:object)		=> (T:non-false)))
  (attributes
   ((_)			effect-free result-true)))

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

(declare-core-primitive native-endianness
    (safe)
  (signatures
   (()				=> (T:symbol)))
  (attributes
   (()				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive values
    (safe)
  (signatures
   (T:object		=> T:object))
  (attributes
   (_			effect-free)))

(declare-core-primitive call-with-current-continuation
    (safe)
  (signatures
   ((T:procedure)	=> T:object)))

(declare-core-primitive call/cc
    (safe)
  (signatures
   ((T:procedure)	=> T:object)))

(declare-core-primitive call-with-values
    (safe)
  (signatures
   ((T:procedure T:procedure)	=> T:object)))

(declare-core-primitive apply
    (safe)
  (signatures
   ((T:procedure . _)		=> T:object)))

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

(declare-core-primitive make-guardian
    (safe)
  (signatures
   (()				=> (T:procedure)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive make-parameter
    (safe)
  (signatures
   ((T:object)			=> (T:procedure))
   ((T:object T:procedure)	=> (T:procedure)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

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


;;;; compensations, safe primitives

(declare-core-primitive run-compensations
    (safe)
  (signatures
   (()			=> (T:void)))
  (attributes
   (()			result-true)))

(declare-core-primitive push-compensation-thunk
    (safe)
  (signatures
   ((T:procedure)	=> (T:void)))
  (attributes
   ((_)			result-true)))


;;;; environment inquiry, safe primitives

(declare-core-primitive host-info
    (safe)
  (signatures
   (()			=> (T:string)))
  (attributes
   (()			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive uname
    (safe)
  (signatures
   (()			=> (T:utsname)))
  (attributes
   (()			effect-free result-true)))

(declare-type-predicate utsname?	T:utsname)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:utsname)	=> (T:string)))
		   (attributes
		    ((_)		effect-free result-true))))
		)))
  (declare utsname-sysname)
  (declare utsname-nodename)
  (declare utsname-release)
  (declare utsname-version)
  (declare utsname-machine)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(declare-core-primitive implementation-name
    (safe)
  (signatures
   (()			=> (T:string)))
  (attributes
   (()			foldable effect-free result-true)))

(declare-core-primitive implementation-version
    (safe)
  (signatures
   (()			=> (T:string)))
  (attributes
   ;;Not foldable.
   (()			effect-free result-true)))

(declare-core-primitive cpu-architecture
    (safe)
  (signatures
   (()			=> (T:string)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive machine-name
    (safe)
  (signatures
   (()			=> (T:string)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive os-name
    (safe)
  (signatures
   (()			=> (T:string)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive os-version
    (safe)
  (signatures
   (()			=> (T:string)))
  (attributes
   (()			effect-free result-true)))


;;;; invocation and termination procedures

(declare-object-retriever command-line	T:non-empty-proper-list)

(declare-core-primitive exit
    (safe)
  (signatures
   (()				=> (T:void))
   ((T:fixnum)			=> (T:void))))

(declare-parameter exit-hooks	T:proper-list)


;;;; numerics, safe functions
;;
;;The order of  unsafe replacements *does* matter:  replacements accepting "T:number"
;;operands must come *after* the ones accepting more specific operand types.
;;

;;; predicates

(declare-type-predicate number?			T:number)
(declare-type-predicate complex?		T:number)

(declare-core-primitive real?
    (safe)
  (signatures
   ((T:real)			=> (T:true))
   ((T:compnum)			=> (T:false))
   ((T:cflonum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive rational?
    (safe)
  (signatures
   ((T:fixnum)			=> (T:true))
   ((T:bignum)			=> (T:true))
   ((T:ratnum)			=> (T:true))
   ((T:flonum-finite)		=> (T:true))
   ((T:flonum-infinite)		=> (T:false))
   ((T:flonum-nan)		=> (T:false))
   ((T:compnum)			=> (T:false))
   ((T:cflonum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-type-predicate integer?		T:integer)
(declare-type-predicate exact-integer?		T:exact-integer)

(declare-type-predicate real-valued?)
(declare-type-predicate rational-valued?)
(declare-type-predicate integer-valued?)

(declare-number-predicate odd?)
(declare-number-predicate even?)

;;;

(declare-core-primitive zero?
    (safe)
  (signatures
   ((T:positive-fixnum)		=> (T:false))
   ((T:negative-fixnum)		=> (T:false))
   ((T:ratnum)			=> (T:false))
   ((T:bignum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

(declare-core-primitive positive?
    (safe)
  (signatures
   ((T:positive-fixnum)		=> (T:true))
   ((T:negative-fixnum)		=> (T:false))
   ((T:positive-bignum)		=> (T:true))
   ((T:negative-bignum)		=> (T:false))
   ((T:real)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-false)
   ((_)				foldable effect-free)))

(declare-core-primitive negative?
    (safe)
  (signatures
   ((T:positive-fixnum)		=> (T:false))
   ((T:negative-fixnum)		=> (T:true))
   ((T:positive-bignum)		=> (T:false))
   ((T:negative-bignum)		=> (T:true))
   ((T:real)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-false)
   ((_)				foldable effect-free)))

(declare-core-primitive non-negative?
    (safe)
  (signatures
   ((T:negative-fixnum)		=> (T:false))
   ((T:non-negative-fixnum)	=> (T:true))
   ((T:positive-bignum)		=> (T:true))
   ((T:negative-bignum)		=> (T:false))
   ((T:real)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

(declare-core-primitive non-positive?
    (safe)
  (signatures
   ((T:positive-fixnum)		=> (T:false))
   ((T:non-positive-fixnum)	=> (T:true))
   ((T:positive-bignum)		=> (T:false))
   ((T:negative-bignum)		=> (T:true))
   ((T:real)			=> (T:boolean)))
  (attributes
   ((0)				foldable effect-free result-true)
   ((_)				foldable effect-free)))

;;;

(declare-core-primitive zero-exact-integer?
    (safe)
  (signatures
   ((T:object)		=> (T:boolean)))
  (attributes
   ((0)			foldable effect-free result-true)
   ((_)			foldable effect-free)))

(declare-core-primitive positive-exact-integer?
    (safe)
  (signatures
   ((T:positive-fixnum)	=> (T:true))
   ((T:positive-bignum)	=> (T:true))
   ((T:negative)	=> (T:false))
   ((T:object)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive negative-exact-integer?
    (safe)
  (signatures
   ((T:negative-fixnum)	=> (T:true))
   ((T:negative-bignum)	=> (T:true))
   ((T:positive)	=> (T:false))
   ((T:object)		=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive non-negative-exact-integer?
    (safe)
  (signatures
   ((T:non-negative-fixnum)	=> (T:true))
   ((T:positive-bignum)		=> (T:true))
   ((T:negative)		=> (T:false))
   ((T:object)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive non-positive-exact-integer?
    (safe)
  (signatures
   ((T:non-positive-fixnum)	=> (T:true))
   ((T:negative-bignum)		=> (T:true))
   ((T:positive)		=> (T:false))
   ((T:object)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;;

(declare-core-primitive exact?
    (safe)
  (signatures
   ((T:exact)			=> (T:true))
   ((T:inexact)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive inexact?
    (safe)
  (signatures
   ((T:exact)			=> (T:false))
   ((T:inexact)			=> (T:true))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;;

(declare-core-primitive finite?
    (safe)
  (signatures
   ((T:flonum-finite)		=> (T:true))
   ((T:flonum-infinite)		=> (T:false))
   ((T:flonum-nan)		=> (T:false))
   ((T:fixnum)			=> (T:true))
   ((T:bignum)			=> (T:true))
   ((T:ratnum)			=> (T:true))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive infinite?
    (safe)
  (signatures
   ((T:flonum-finite)		=> (T:false))
   ((T:flonum-infinite)		=> (T:true))
   ((T:flonum-nan)		=> (T:false))
   ((T:fixnum)			=> (T:false))
   ((T:bignum)			=> (T:false))
   ((T:ratnum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive nan?
    (safe)
  (signatures
   ((T:flonum-nan)		=> (T:false))
   ((T:flonum-infinite)		=> (T:false))
   ((T:flonum-nan)		=> (T:true))
   ((T:fixnum)			=> (T:false))
   ((T:bignum)			=> (T:false))
   ((T:ratnum)			=> (T:false))
   ((T:number)			=> (T:boolean)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; comparison

(declare-core-primitive =
    (safe)
  (signatures
   ((T:positive     T:non-positive . T:number)		=> (T:false))
   ((T:non-positive T:positive     . T:number)		=> (T:false))
   ((T:negative     T:non-negative . T:number)		=> (T:false))
   ((T:non-negative T:negative     . T:number)		=> (T:false))
   ((T:number       T:number       . T:number)		=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive !=
    (safe)
  (signatures
   ((T:positive     T:non-positive . T:number)		=> (T:true))
   ((T:non-positive T:positive     . T:number)		=> (T:true))
   ((T:negative     T:non-negative . T:number)		=> (T:true))
   ((T:non-negative T:negative     . T:number)		=> (T:true))
   ((T:number       T:number       . T:number)		=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive <
    (safe)
  (signatures
   ((T:positive     T:non-positive)	=> (T:false))
   ((T:non-negative T:negative)		=> (T:false))

   ((T:non-positive T:positive)		=> (T:true))
   ((T:negative     T:non-negative)	=> (T:true))

   ((T:number T:number . T:number)	=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive >
    (safe)
  (signatures
   ((T:positive     T:non-positive)	=> (T:true))
   ((T:non-negative T:negative)		=> (T:true))

   ((T:non-positive T:positive)		=> (T:false))
   ((T:negative     T:non-negative)	=> (T:false))

   ((T:number T:number . T:number)	=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive <=
    (safe)
  (signatures
   ((T:positive T:negative)		=> (T:false))
   ((T:negative T:positive)		=> (T:true))

   ((T:number T:number . T:number)	=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

(declare-core-primitive >=
    (safe)
  (signatures
   ((T:positive T:negative)		=> (T:true))
   ((T:negative T:positive)		=> (T:false))

   ((T:number T:number . T:number)	=> (T:boolean)))
  (attributes
   ((_ _ . _)			foldable effect-free)))

;;;

(declare-core-primitive max
    (safe)
  (signatures
   ((T:fixnum . T:fixnum)		=> (T:fixnum))
   ((T:bignum . T:bignum)		=> (T:bignum))
   ((T:flonum . T:flonum)		=> (T:flonum))
   ((T:exact-integer . T:exact-integer)	=> (T:exact-integer))
   ((T:exact-real . T:exact-real)	=> (T:exact-real))
   ((T:integer . T:integer)		=> (T:integer))
   ((T:real . T:real)			=> (T:real)))
  (attributes
   ((_ . _)				foldable effect-free result-true))
  (replacements
   $max-fixnum-fixnum $max-fixnum-bignum $max-fixnum-flonum $max-fixnum-ratnum
   $max-bignum-fixnum $max-bignum-bignum $max-bignum-flonum $max-bignum-ratnum
   $max-flonum-fixnum $max-flonum-bignum $max-flonum-flonum $max-flonum-ratnum
   $max-ratnum-fixnum $max-ratnum-bignum $max-ratnum-flonum $max-ratnum-ratnum
   $max-fixnum-number $max-bignum-number $max-flonum-number $max-ratnum-number
   $max-number-fixnum $max-number-bignum $max-number-flonum $max-number-ratnum))

(declare-core-primitive min
    (safe)
  (signatures
   ((T:fixnum . T:fixnum)		=> (T:fixnum))
   ((T:bignum . T:bignum)		=> (T:bignum))
   ((T:flonum . T:flonum)		=> (T:flonum))
   ((T:exact-integer . T:exact-integer)	=> (T:exact-integer))
   ((T:exact-real . T:exact-real)	=> (T:exact-real))
   ((T:integer . T:integer)		=> (T:integer))
   ((T:real . T:real)			=> (T:real)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  (replacements
   $min-fixnum-fixnum $min-fixnum-bignum $min-fixnum-flonum $min-fixnum-ratnum
   $min-bignum-fixnum $min-bignum-bignum $min-bignum-flonum $min-bignum-ratnum
   $min-flonum-fixnum $min-flonum-bignum $min-flonum-flonum $min-flonum-ratnum
   $min-ratnum-fixnum $min-ratnum-bignum $min-ratnum-flonum $min-ratnum-ratnum
   $min-fixnum-number $min-bignum-number $min-flonum-number $min-ratnum-number
   $min-number-fixnum $min-number-bignum $min-number-flonum $min-number-ratnum))

;;; --------------------------------------------------------------------
;;; arithmetics

(declare-core-primitive +
    (safe)
  (signatures
   (T:flonum			=> (T:flonum))
   (T:exact-integer		=> (T:exact-integer))
   (T:exact-real		=> (T:exact-real))
   (T:integer			=> (T:integer))
   (T:real			=> (T:real))
   (T:cflonum			=> (T:cflonum))
   (T:number			=> (T:number)))
  (attributes
   (_			foldable effect-free result-true))
  (replacements
   $add-fixnum-fixnum		$add-fixnum-bignum	$add-fixnum-flonum
   $add-fixnum-ratnum		$add-fixnum-compnum	$add-fixnum-cflonum

   $add-bignum-fixnum		$add-bignum-bignum	$add-bignum-flonum
   $add-bignum-ratnum		$add-bignum-compnum	$add-bignum-cflonum

   $add-flonum-fixnum		$add-flonum-bignum	$add-flonum-flonum
   $add-flonum-ratnum		$add-flonum-compnum	$add-flonum-cflonum

   $add-ratnum-fixnum		$add-ratnum-bignum	$add-ratnum-flonum
   $add-ratnum-ratnum		$add-ratnum-compnum	$add-ratnum-cflonum

   $add-compnum-fixnum		$add-compnum-bignum	$add-compnum-ratnum
   $add-compnum-compnum		$add-compnum-flonum	$add-compnum-cflonum

   $add-cflonum-fixnum		$add-cflonum-bignum	$add-cflonum-ratnum
   $add-cflonum-flonum		$add-cflonum-compnum	$add-cflonum-cflonum

   $add-fixnum-number		$add-bignum-number	$add-flonum-number
   $add-ratnum-number		$add-compnum-number	$add-cflonum-number

   $add-number-fixnum		$add-number-bignum	$add-number-flonum
   $add-number-ratnum		$add-number-compnum	$add-number-cflonum

   $add-number-number))

(declare-core-primitive -
    (safe)
  (signatures
   (T:flonum			=> (T:flonum))
   (T:exact-integer		=> (T:exact-integer))
   (T:exact-real		=> (T:exact-real))
   (T:integer			=> (T:integer))
   (T:real			=> (T:real))
   (T:cflonum			=> (T:cflonum))
   (T:number			=> (T:number)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  (replacements
   $sub-fixnum-fixnum		$sub-fixnum-bignum	$sub-fixnum-flonum
   $sub-fixnum-ratnum		$sub-fixnum-compnum	$sub-fixnum-cflonum

   $sub-bignum-fixnum		$sub-bignum-bignum	$sub-bignum-flonum
   $sub-bignum-ratnum		$sub-bignum-compnum	$sub-bignum-cflonum

   $sub-flonum-fixnum		$sub-flonum-bignum	$sub-flonum-flonum
   $sub-flonum-ratnum		$sub-flonum-compnum	$sub-flonum-cflonum

   $sub-ratnum-fixnum		$sub-ratnum-bignum	$sub-ratnum-flonum
   $sub-ratnum-ratnum		$sub-ratnum-compnum	$sub-ratnum-cflonum

   $sub-compnum-fixnum		$sub-compnum-bignum	$sub-compnum-ratnum
   $sub-compnum-compnum		$sub-compnum-flonum	$sub-compnum-cflonum

   $sub-cflonum-fixnum		$sub-cflonum-bignum	$sub-cflonum-ratnum
   $sub-cflonum-flonum		$sub-cflonum-compnum	$sub-cflonum-cflonum

   $sub-fixnum-number		$sub-bignum-number	$sub-flonum-number
   $sub-ratnum-number		$sub-compnum-number	$sub-cflonum-number

   $sub-number-fixnum		$sub-number-bignum	$sub-number-flonum
   $sub-number-ratnum		$sub-number-compnum	$sub-number-cflonum

   $sub-number-number))

(declare-core-primitive *
    (safe)
  (signatures
   (T:flonum			=> (T:flonum))
   (T:exact-integer		=> (T:exact-integer))
   (T:exact-real		=> (T:exact-real))
   (T:integer			=> (T:integer))
   (T:real			=> (T:real))
   (T:cflonum			=> (T:cflonum))
   (T:number			=> (T:number)))
  (attributes
   (_			foldable effect-free result-true))
  (replacements
   $mul-fixnum-fixnum		$mul-fixnum-bignum	$mul-fixnum-flonum
   $mul-fixnum-ratnum		$mul-fixnum-compnum	$mul-fixnum-cflonum

   $mul-bignum-fixnum		$mul-bignum-bignum	$mul-bignum-flonum
   $mul-bignum-ratnum		$mul-bignum-compnum	$mul-bignum-cflonum

   $mul-flonum-fixnum		$mul-flonum-bignum	$mul-flonum-flonum
   $mul-flonum-ratnum		$mul-flonum-compnum	$mul-flonum-cflonum

   $mul-ratnum-fixnum		$mul-ratnum-bignum	$mul-ratnum-flonum
   $mul-ratnum-ratnum		$mul-ratnum-compnum	$mul-ratnum-cflonum

   $mul-compnum-fixnum		$mul-compnum-bignum	$mul-compnum-ratnum
   $mul-compnum-compnum		$mul-compnum-flonum	$mul-compnum-cflonum

   $mul-cflonum-fixnum		$mul-cflonum-bignum	$mul-cflonum-ratnum
   $mul-cflonum-flonum		$mul-cflonum-compnum	$mul-cflonum-cflonum

   $mul-fixnum-number		$mul-bignum-number	$mul-flonum-number
   $mul-ratnum-number		$mul-compnum-number	$mul-cflonum-number

   $mul-number-fixnum		$mul-number-bignum	$mul-number-flonum
   $mul-number-ratnum		$mul-number-compnum	$mul-number-cflonum

   $mul-number-number))

(declare-core-primitive /
    (safe)
  (signatures
   ((T:flonum  . T:flonum)	=> (T:flonum))
   ((T:real    . T:real)	=> (T:real))
   ((T:cflonum . T:cflonum)	=> (T:cflonum))
   ((T:number  . T:number)	=> (T:number)))
  (attributes
   ((_ . _)			foldable effect-free result-true))
  (replacements
   $div-fixnum-fixnum		$div-fixnum-bignum	$div-fixnum-flonum
   $div-fixnum-ratnum		$div-fixnum-compnum	$div-fixnum-cflonum

   $div-bignum-fixnum		$div-bignum-bignum	$div-bignum-flonum
   $div-bignum-ratnum		$div-bignum-compnum	$div-bignum-cflonum

   $div-flonum-fixnum		$div-flonum-bignum	$div-flonum-flonum
   $div-flonum-ratnum		$div-flonum-compnum	$div-flonum-cflonum

   $div-ratnum-fixnum		$div-ratnum-bignum	$div-ratnum-flonum
   $div-ratnum-ratnum		$div-ratnum-compnum	$div-ratnum-cflonum

   $div-compnum-fixnum		$div-compnum-bignum	$div-compnum-ratnum
   $div-compnum-compnum		$div-compnum-flonum	$div-compnum-cflonum

   $div-cflonum-fixnum		$div-cflonum-bignum	$div-cflonum-ratnum
   $div-cflonum-flonum		$div-cflonum-compnum	$div-cflonum-cflonum

   $div-fixnum-number		$div-bignum-number	$div-flonum-number
   $div-ratnum-number		$div-compnum-number	$div-cflonum-number

   $div-number-fixnum		$div-number-bignum	$div-number-flonum
   $div-number-ratnum		$div-number-compnum	$div-number-cflonum

   $div-number-number))

(declare-core-primitive add1
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:exact-integer))
   ((T:flonum)			=> (T:flonum))
   ((T:integer)			=> (T:integer))
   ((T:real)			=> (T:real))
   ((T:cflonum)			=> (T:cflonum))
   ((T:compnum)			=> (T:compnum))
   ((T:number)			=> (T:number)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $add1-fixnum
   $add1-bignum
   $add1-integer))

(declare-core-primitive sub1
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:exact-integer))
   ((T:flonum)			=> (T:flonum))
   ((T:integer)			=> (T:integer))
   ((T:real)			=> (T:real))
   ((T:cflonum)			=> (T:cflonum))
   ((T:compnum)			=> (T:compnum))
   ((T:number)			=> (T:number)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements
   $sub1-fixnum
   $sub1-bignum
   $sub1-integer))

;;; --------------------------------------------------------------------

(declare-core-primitive div
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive mod
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive div0
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive mod0
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive modulo
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive remainder
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive quotient
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer)))
  (attributes
   ((_ _)				foldable effect-free result-true)))

(declare-core-primitive quotient+remainder
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer T:exact-integer))
   ((T:integer T:integer)		=> (T:integer T:integer)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive div-and-mod
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer T:exact-integer))
   ((T:real T:real)			=> (T:real T:real)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive div0-and-mod0
    (safe)
  (signatures
   ((T:fixnum T:fixnum)			=> (T:fixnum T:fixnum))
   ((T:exact-integer T:exact-integer)	=> (T:exact-integer T:exact-integer))
   ((T:real T:real)			=> (T:real T:real)))
  (attributes
   ((_ _)				effect-free)))

;;;

(declare-core-primitive gcd
    (safe)
  (signatures
   (()					=> (T:fixnum))

   ((T:fixnum)				=> (T:fixnum))
   ((T:bignum)				=> (T:bignum))
   ((T:flonum)				=> (T:flonum))
   ((T:exact-integer)			=> (T:exact-integer))
   ((T:real)				=> (T:real))

   ((T:fixnum T:fixnum)			=> (T:exact-integer))
   ((T:fixnum T:bignum)			=> (T:exact-integer))
   ((T:fixnum T:flonum)			=> (T:flonum))

   ((T:bignum T:fixnum)			=> (T:exact-integer))
   ((T:bignum T:bignum)			=> (T:exact-integer))
   ((T:bignum T:flonum)			=> (T:flonum))

   ((T:flonum T:fixnum)			=> (T:flonum))
   ((T:flonum T:bignum)			=> (T:flonum))
   ((T:flonum T:flonum)			=> (T:flonum))

   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real))

   ((T:real T:real T:real . T:real)	=> (T:real)))
  (attributes
   (()					foldable effect-free result-true)
   ((_)					foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)
   ((_ _ _ . _)				foldable effect-free result-true))
  (replacements
   $gcd-fixnum-fixnum $gcd-fixnum-bignum $gcd-fixnum-flonum
   $gcd-bignum-fixnum $gcd-bignum-bignum $gcd-bignum-flonum
   $gcd-flonum-fixnum $gcd-flonum-bignum $gcd-flonum-flonum
   $gcd-fixnum-number $gcd-bignum-number $gcd-flonum-number
   $gcd-number-fixnum $gcd-number-bignum $gcd-number-flonum
   $gcd-number-number))

(declare-core-primitive lcm
    (safe)
  (signatures
   (()					=> (T:fixnum))

   ((T:fixnum)				=> (T:fixnum))
   ((T:bignum)				=> (T:bignum))
   ((T:flonum)				=> (T:flonum))
   ((T:exact-integer)			=> (T:exact-integer))
   ((T:real)				=> (T:real))

   ((T:fixnum T:fixnum)			=> (T:exact-integer))
   ((T:fixnum T:bignum)			=> (T:exact-integer))
   ((T:fixnum T:flonum)			=> (T:flonum))

   ((T:bignum T:fixnum)			=> (T:exact-integer))
   ((T:bignum T:bignum)			=> (T:exact-integer))
   ((T:bignum T:flonum)			=> (T:flonum))

   ((T:flonum T:fixnum)			=> (T:flonum))
   ((T:flonum T:bignum)			=> (T:flonum))
   ((T:flonum T:flonum)			=> (T:flonum))

   ((T:exact-integer T:exact-integer)	=> (T:exact-integer))
   ((T:integer T:integer)		=> (T:integer))
   ((T:real T:real)			=> (T:real))

   ((T:real T:real T:real . T:real)	=> (T:real)))
  (attributes
   (()					foldable effect-free result-true)
   ((_)					foldable effect-free result-true)
   ((_ _)				foldable effect-free result-true)
   ((_ _ _ . _)				foldable effect-free result-true))
  (replacements
   $lcm-fixnum-fixnum $lcm-fixnum-bignum $lcm-fixnum-flonum
   $lcm-bignum-fixnum $lcm-bignum-bignum $lcm-bignum-flonum
   $lcm-flonum-fixnum $lcm-flonum-bignum $lcm-flonum-flonum
   $lcm-fixnum-number $lcm-bignum-number $lcm-flonum-number
   $lcm-number-fixnum $lcm-number-bignum $lcm-number-flonum
   $lcm-number-number))

;;; --------------------------------------------------------------------
;;; exactness

(declare-core-primitive exact
    (safe)
  (signatures
   ((T:fixnum)		=> (T:fixnum))
   ((T:bignum)		=> (T:bignum))
   ((T:flonum)		=> (T:exact-integer))
   ((T:ratnum)		=> (T:ratnum))
   ((T:compnum)		=> (T:exact-compnum))
   ((T:cflonum)		=> (T:exact-compnum))
   ((T:real)		=> (T:exact-integer))
   ((T:number)		=> (T:exact)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $exact-fixnum $exact-bignum $exact-flonum $exact-ratnum $exact-compnum $exact-cflonum))

(declare-core-primitive inexact
    (safe)
  (signatures
   ((T:fixnum)		=> (T:flonum))
   ((T:bignum)		=> (T:flonum))
   ((T:flonum)		=> (T:flonum))
   ((T:ratnum)		=> (T:flonum))
   ((T:compnum)		=> (T:cflonum))
   ((T:cflonum)		=> (T:cflonum))
   ((T:real)		=> (T:flonum))
   ((T:number)		=> (T:inexact)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $inexact-fixnum $inexact-bignum $inexact-flonum $inexact-ratnum $inexact-compnum $inexact-cflonum))

(declare-core-primitive inexact->exact
    (safe)
  (signatures
   ((T:fixnum)		=> (T:fixnum))
   ((T:bignum)		=> (T:bignum))
   ((T:flonum)		=> (T:exact-integer))
   ((T:ratnum)		=> (T:ratnum))
   ((T:compnum)		=> (T:exact-compnum))
   ((T:cflonum)		=> (T:exact-compnum))
   ((T:real)		=> (T:exact-integer))
   ((T:number)		=> (T:exact)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $exact-fixnum $exact-bignum $exact-flonum $exact-ratnum $exact-compnum $exact-cflonum))

(declare-core-primitive exact->inexact
    (safe)
  (signatures
   ((T:fixnum)		=> (T:flonum))
   ((T:bignum)		=> (T:flonum))
   ((T:flonum)		=> (T:flonum))
   ((T:ratnum)		=> (T:flonum))
   ((T:compnum)		=> (T:cflonum))
   ((T:cflonum)		=> (T:cflonum))
   ((T:real)		=> (T:flonum))
   ((T:number)		=> (T:inexact)))
  (attributes
   ((_)			foldable effect-free result-true))
  (replacements
   $inexact-fixnum $inexact-bignum $inexact-flonum $inexact-ratnum $inexact-compnum $inexact-cflonum))

;;; --------------------------------------------------------------------
;;; parts

(declare-core-primitive numerator
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:exact-integer))
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $numerator-fixnum $numerator-bignum $numerator-flonum $numerator-ratnum))

(declare-core-primitive denominator
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:exact-integer))
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $denominator-fixnum $denominator-bignum $denominator-flonum $denominator-ratnum))

(declare-core-primitive rationalize
    (safe)
  (signatures
   ((T:real T:real)		=> (T:real)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive make-rectangular
    (safe)
  (signatures
   ((T:real T:real)		=> (T:non-real)))
  (attributes
   ((_ _)			foldable effect-free result-true))
  (replacements $make-cflonum $make-compnum $make-rectangular))

(declare-core-primitive make-polar
    (safe)
  (signatures
   ((T:real T:real)		=> (T:non-real)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-number-unary real-part		(replacements $compnum-real $cflonum-real))
(declare-number-unary imag-part		(replacements $compnum-imag $cflonum-imag))

(declare-core-primitive abs
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:ratnum)			=> (T:ratnum))
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $abs-fixnum $abs-bignum $abs-flonum $abs-ratnum))

(declare-core-primitive sign
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:fixnum))
   ((T:ratnum)			=> (T:fixnum))
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $sign-fixnum $sign-bignum $sign-flonum $sign-ratnum))

(declare-core-primitive angle
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:fixnum))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:fixnum))
   ((T:compnum)			=> (T:real))
   ((T:cflonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $angle-fixnum $angle-bignum $angle-ratnum $angle-flonum $angle-compnum $angle-cflonum))

(declare-core-primitive magnitude
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:ratnum)			=> (T:ratnum))
   ((T:flonum)			=> (T:flonum))
   ((T:compnum)			=> (T:real))
   ((T:cflonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $magnitude-fixnum $magnitude-bignum $magnitude-ratnum $magnitude-flonum $magnitude-compnum $magnitude-cflonum))

(declare-core-primitive complex-conjugate
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:ratnum)			=> (T:ratnum))
   ((T:flonum)			=> (T:flonum))
   ((T:compnum)			=> (T:compnum))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $complex-conjugate-compnum $complex-conjugate-cflonum))

;;; --------------------------------------------------------------------
;;; rounding

(let-syntax
    ((declare-safe-rounding-primitive
      (syntax-rules ()
	((_ ?who . ?replacements)
	 (declare-core-primitive ?who
	     (unsafe)
	   (signatures
	    ((T:fixnum)			=> (T:fixnum))
	    ((T:bignum)			=> (T:bignum))
	    ((T:flonum)			=> (T:flonum))
	    ((T:ratnum)			=> (T:ratnum))
	    ((T:real)			=> (T:real)))
	   (attributes
	    ((_)			foldable effect-free result-true))
	   (replacements . ?replacements))))))
  (declare-safe-rounding-primitive floor	   $floor-fixnum    $floor-bignum    $floor-ratnum    $floor-flonum)
  (declare-safe-rounding-primitive ceiling	 $ceiling-fixnum  $ceiling-bignum  $ceiling-ratnum  $ceiling-flonum)
  (declare-safe-rounding-primitive truncate	$truncate-fixnum $truncate-bignum $truncate-ratnum $truncate-flonum)
  (declare-safe-rounding-primitive round	   $round-fixnum    $round-bignum    $round-ratnum    $round-flonum)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; comparison

(let-syntax
    ((declare-real-comparison-unary/multi (syntax-rules ()
					    ((_ ?who)
					     (declare-core-primitive ?who
						 (safe)
					       (signatures
						((T:real . T:real)	=> (T:boolean)))
					       (attributes
						((_ . _)		foldable effect-free)))))))
  (declare-real-comparison-unary/multi =)
  (declare-real-comparison-unary/multi !=)
  (declare-real-comparison-unary/multi <)
  (declare-real-comparison-unary/multi >=)
  (declare-real-comparison-unary/multi <)
  (declare-real-comparison-unary/multi >=)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; exponentiation, exponentials and logarithms

(declare-number-binary expt
  (replacements
   $expt-fixnum-negative-fixnum $expt-bignum-negative-fixnum $expt-flonum-negative-fixnum $expt-ratnum-negative-fixnum
   $expt-compnum-negative-fixnum $expt-cflonum-negative-fixnum
   $expt-fixnum-positive-fixnum $expt-bignum-positive-fixnum $expt-flonum-positive-fixnum $expt-ratnum-positive-fixnum
   $expt-compnum-positive-fixnum $expt-cflonum-positive-fixnum
   $expt-fixnum-fixnum $expt-bignum-fixnum $expt-flonum-fixnum $expt-ratnum-fixnum $expt-compnum-fixnum $expt-cflonum-fixnum
   $expt-fixnum-bignum $expt-bignum-bignum $expt-flonum-bignum $expt-ratnum-bignum $expt-compnum-bignum $expt-cflonum-bignum
   $expt-fixnum-flonum $expt-bignum-flonum $expt-flonum-flonum $expt-ratnum-flonum $expt-compnum-flonum $expt-cflonum-flonum
   $expt-fixnum-ratnum $expt-bignum-ratnum $expt-flonum-ratnum $expt-ratnum-ratnum $expt-compnum-ratnum $expt-cflonum-ratnum
   $expt-fixnum-cflonum $expt-bignum-cflonum $expt-flonum-cflonum $expt-ratnum-cflonum $expt-compnum-cflonum $expt-cflonum-cflonum
   $expt-fixnum-compnum $expt-bignum-compnum $expt-flonum-compnum $expt-ratnum-compnum $expt-compnum-compnum $expt-cflonum-compnum
   $expt-number-negative-fixnum $expt-number-positive-fixnum
   $expt-number-fixnum $expt-number-bignum $expt-number-flonum $expt-number-ratnum $expt-number-compnum $expt-number-cflonum))

(declare-core-primitive square
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:exact-real))
   ((T:compnum)			=> (T:number))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $square-fixnum $square-bignum $square-flonum $square-ratnum $square-compnum $square-cflonum))

(declare-core-primitive cube
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:exact-real))
   ((T:compnum)			=> (T:number))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $cube-fixnum $cube-bignum $cube-flonum $cube-ratnum $cube-compnum $cube-cflonum))

(declare-core-primitive sqrt
    (safe)
  (signatures
   ((T:fixnum)			=> (T:number))
   ((T:bignum)			=> (T:number))
   ((T:flonum)			=> (T:inexact))
   ((T:ratnum)			=> (T:number))
   ((T:compnum)			=> (T:number))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $sqrt-fixnum $sqrt-bignum $sqrt-flonum $sqrt-ratnum $sqrt-compnum $sqrt-cflonum))

(declare-core-primitive cbrt
    (safe)
  (signatures
   ((T:fixnum)			=> (T:number))
   ((T:bignum)			=> (T:number))
   ((T:flonum)			=> (T:inexact))
   ((T:ratnum)			=> (T:number))
   ((T:compnum)			=> (T:number))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $cbrt-fixnum $cbrt-bignum $cbrt-flonum $cbrt-ratnum $cbrt-compnum $cbrt-cflonum))

(declare-core-primitive exact-integer-sqrt
    (safe)
  (signatures
   ((T:fixnum)			=> (T:positive-fixnum T:positive-fixnum))
   ((T:bignum)			=> (T:exact-integer T:exact-integer))
   ((T:exact-integer)		=> (T:exact-integer T:exact-integer)))
  (attributes
   ((_)				effect-free))
  (replacements $exact-integer-sqrt-fixnum $exact-integer-sqrt-bignum))

(declare-core-primitive exp
    (safe)
  (signatures
   ((T:fixnum)			=> (T:real))
   ((T:bignum)			=> (T:flonum))
   ((T:flonum)			=> (T:flonum))
   ((T:ratnum)			=> (T:flonum))
   ((T:compnum)			=> (T:cflonum))
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $exp-fixnum $exp-bignum $exp-flonum $exp-ratnum $exp-compnum $exp-cflonum))

(declare-core-primitive log
    (safe)
  (signatures
   ((T:fixnum)			=> (T:number))
   ((T:bignum)			=> (T:inexact))
   ((T:flonum)			=> (T:inexact))
   ((T:ratnum)			=> (T:inexact))
   ((T:compnum)			=> (T:inexact))
   ((T:cflonum)			=> (T:cflonum))
   ((T:number T:number)		=> (T:number)))
  (attributes
   ((_)				foldable effect-free result-true)
   ((_ _)			foldable effect-free result-true))
  (replacements $log-fixnum $log-bignum $log-flonum $log-ratnum $log-compnum $log-cflonum))

;;; --------------------------------------------------------------------
;;; bitwise

(declare-core-primitive bitwise-and
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:fixnum T:fixnum)		=> (T:fixnum))
   ((T:bignum T:bignum)		=> (T:bignum))
   ((T:fixnum T:bignum)		=> (T:bignum))
   ((T:bignum T:fixnum)		=> (T:bignum))
   (T:exact-integer		=> (T:exact-integer)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-and-fixnum-fixnum $bitwise-and-fixnum-bignum
   $bitwise-and-bignum-fixnum $bitwise-and-bignum-bignum
   $bitwise-and-fixnum-number $bitwise-and-bignum-number))

(declare-core-primitive bitwise-ior
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:fixnum T:fixnum)		=> (T:fixnum))
   ((T:bignum T:bignum)		=> (T:bignum))
   ((T:fixnum T:bignum)		=> (T:bignum))
   ((T:bignum T:fixnum)		=> (T:bignum))
   (T:exact-integer		=> (T:exact-integer)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-ior-fixnum-fixnum $bitwise-ior-fixnum-bignum
   $bitwise-ior-bignum-fixnum $bitwise-ior-bignum-bignum
   $bitwise-ior-fixnum-number $bitwise-ior-bignum-number))

(declare-core-primitive bitwise-xor
    (safe)
  (signatures
   (()				=> (T:fixnum))
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum))
   ((T:fixnum T:fixnum)		=> (T:fixnum))
   ((T:bignum T:bignum)		=> (T:bignum))
   ((T:fixnum T:bignum)		=> (T:bignum))
   ((T:bignum T:fixnum)		=> (T:bignum))
   (T:exact-integer		=> (T:exact-integer)))
  (attributes
   (_				foldable effect-free result-true))
  (replacements
   $bitwise-xor-fixnum-fixnum $bitwise-xor-fixnum-bignum
   $bitwise-xor-bignum-fixnum $bitwise-xor-bignum-bignum
   $bitwise-xor-fixnum-number $bitwise-xor-bignum-number))

(declare-core-primitive bitwise-not
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum))
   ((T:bignum)			=> (T:bignum)))
  (attributes
   ((_)				foldable effect-free result-true))
  (replacements $bitwise-not-fixnum $bitwise-not-bignum))

(declare-core-primitive bitwise-arithmetic-shift
    (safe)
  (signatures
   ((T:exact-integer T:fixnum)	=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-arithmetic-shift-left
    (safe)
  (signatures
   ((T:exact-integer T:fixnum)	=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-arithmetic-shift-right
    (safe)
  (signatures
   ((T:exact-integer T:fixnum)	=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-count
    (safe)
  (signatures
   ((T:exact-integer)		=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-field
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-bit-set?
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:boolean)))
  (attributes
   ((_ _)			foldable effect-free)))

(declare-core-primitive bitwise-copy-bit
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-copy-bit-field
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-first-bit-set
    (safe)
  (signatures
   ((T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bitwise-if
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-length
    (safe)
  (signatures
   ((T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive bitwise-reverse-bit-field
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _)			foldable effect-free result-true)))

(declare-core-primitive bitwise-rotate-bit-field
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer T:exact-integer T:exact-integer)	=> (T:exact-integer)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive sll
    (safe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:exact-integer))
   ((T:bignum T:fixnum)		=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive sra
    (safe)
  (signatures
   ((T:fixnum T:fixnum)		=> (T:exact-integer))
   ((T:bignum T:fixnum)		=> (T:exact-integer)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; trigonometric

(declare-number-unary sin	(replacements $sin-fixnum $sin-bignum $sin-flonum $sin-ratnum $sin-compnum $sin-cflonum))
(declare-number-unary cos	(replacements $cos-fixnum $cos-bignum $cos-flonum $cos-ratnum $cos-compnum $cos-cflonum))
(declare-number-unary tan	(replacements $tan-fixnum $tan-bignum $tan-flonum $tan-ratnum $tan-compnum $tan-cflonum))
(declare-number-unary asin	(replacements $asin-fixnum $asin-bignum $asin-flonum $asin-ratnum $asin-compnum $asin-cflonum))
(declare-number-unary acos	(replacements $acos-fixnum $acos-bignum $acos-flonum $acos-ratnum $acos-compnum $acos-cflonum))
(declare-number-unary/binary atan (replacements $atan-fixnum $atan-bignum $atan-flonum $atan-ratnum $atan-compnum $atan-cflonum))

;;; --------------------------------------------------------------------
;;; hyperbolic

(declare-number-unary sinh	(replacements $sinh-fixnum $sinh-bignum $sinh-flonum $sinh-ratnum $sinh-compnum $sinh-cflonum))
(declare-number-unary cosh	(replacements $cosh-fixnum $cosh-bignum $cosh-flonum $cosh-ratnum $cosh-compnum $cosh-cflonum))
(declare-number-unary tanh	(replacements $tanh-fixnum $tanh-bignum $tanh-flonum $tanh-ratnum $tanh-compnum $tanh-cflonum))
(declare-number-unary asinh	(replacements $asinh-fixnum $asinh-bignum $asinh-flonum $asinh-ratnum $asinh-compnum $asinh-cflonum))
(declare-number-unary acosh	(replacements $acosh-fixnum $acosh-bignum $acosh-flonum $acosh-ratnum $acosh-compnum $acosh-cflonum))
(declare-number-unary atanh	(replacements $atanh-fixnum $atanh-bignum $atanh-flonum $atanh-ratnum $atanh-compnum $atanh-cflonum))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive integer->char
    (safe)
  (signatures
   ((T:fixnum)		=> (T:char)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive number->string
    (safe)
  (signatures
   ((T:number)				=> (T:string))
   ((T:number T:positive-fixnum)	=> (T:string))
   ((T:number T:positive-fixnum)	=> (T:string)))
  (attributes
   ((_)			foldable effect-free result-true)
   ((_ _)		foldable effect-free result-true)
   ((_ _ _)		foldable effect-free result-true)))

;;; --------------------------------------------------------------------
;;; shortcut's interrupt handlers

(declare-core-primitive error@fx+
    (safe)
  (signatures
   ((T:object T:object)		=> (T:void))))

(declare-core-primitive error@fx*
    (safe)
  (signatures
   ((T:object T:object)		=> (T:void))))

(declare-core-primitive error@fxadd1
    (safe)
  (signatures
   ((T:object)			=> (T:void))))

(declare-core-primitive error@fxsub1
    (safe)
  (signatures
   ((T:object)			=> (T:void))))

(declare-core-primitive error@fxarithmetic-shift-left
    (safe)
  (signatures
   ((T:object T:object)		=> (T:void))))

(declare-core-primitive error@fxarithmetic-shift-right
    (safe)
  (signatures
   ((T:object T:object)		=> (T:void))))

;;;

(declare-core-primitive error@add1
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive error@sub1
    (safe)
  (signatures
   ((T:fixnum)			=> (T:exact-integer))
   ((T:bignum)			=> (T:exact-integer))
   ((T:real)			=> (T:real)))
  (attributes
   ((_)				foldable effect-free result-true)))


;;;; numerics, unsafe functions

(declare-core-primitive $make-rectangular
    (unsafe)
  (signatures
   ((T:real T:real)		=> (T:non-real)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $neg-number	T:number	T:number)
(declare-unsafe-unary-operation $neg-fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-unary-operation $neg-bignum	T:bignum	T:exact-integer)
(declare-unsafe-unary-operation $neg-flonum	T:flonum	T:flonum)
(declare-unsafe-unary-operation $neg-ratnum	T:ratnum	T:ratnum)
(declare-unsafe-unary-operation $neg-compnum	T:compnum	T:compnum)
(declare-unsafe-unary-operation $neg-cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $inv-number	T:number	T:number)
(declare-unsafe-unary-operation $inv-fixnum	T:fixnum	T:exact-real)
(declare-unsafe-unary-operation $inv-bignum	T:bignum	T:ratnum)
(declare-unsafe-unary-operation $inv-flonum	T:flonum	T:flonum)
(declare-unsafe-unary-operation $inv-ratnum	T:ratnum	T:exact-real)
(declare-unsafe-unary-operation $inv-compnum	T:compnum	T:non-real)
(declare-unsafe-unary-operation $inv-cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $add1-integer	T:exact-integer	T:exact-integer)

(declare-core-primitive $add1-fixnum
    (unsafe)
  (signatures
   ((T:non-positive-fixnum)	=> (T:fixnum))
   ((T:fixnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $add1-bignum
    (unsafe)
  (signatures
   ((T:positive-bignum)		=> (T:bignum))
   ((T:bignum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sub1-integer	T:exact-integer	T:exact-integer)

(declare-core-primitive $sub1-fixnum
    (unsafe)
  (signatures
   ((T:non-negative-fixnum)	=> (T:fixnum))
   ((T:fixnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $sub1-bignum
    (unsafe)
  (signatures
   ((T:negative-bignum)		=> (T:bignum))
   ((T:bignum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $add-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $add-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $add-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $add-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $add-ratnum-number	T:ratnum	T:number	T:number)
(declare-unsafe-binary-operation $add-compnum-number	T:compnum	T:number	T:number)
(declare-unsafe-binary-operation $add-cflonum-number	T:cflonum	T:number	T:number)

(declare-unsafe-binary-operation $add-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $add-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $add-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $add-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $add-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $add-number-cflonum	T:number	T:cflonum	T:number)

(declare-unsafe-binary-operation $add-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $add-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $add-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $add-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $add-fixnum-compnum	T:fixnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $add-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $add-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $add-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $add-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $add-bignum-compnum	T:bignum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $add-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $add-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $add-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $add-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $add-flonum-compnum	T:flonum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $add-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $add-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $add-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $add-ratnum-ratnum	T:ratnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $add-ratnum-compnum	T:ratnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $add-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-compnum-fixnum	T:compnum	T:fixnum	T:compnum)
(declare-unsafe-binary-operation $add-compnum-bignum	T:compnum	T:bignum	T:compnum)
(declare-unsafe-binary-operation $add-compnum-flonum	T:compnum	T:flonum	T:non-real)
(declare-unsafe-binary-operation $add-compnum-ratnum	T:compnum	T:ratnum	T:compnum)
(declare-unsafe-binary-operation $add-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $add-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $add-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-ratnum	T:cflonum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-flonum	T:cflonum	T:ratnum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-compnum	T:cflonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $add-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $sub-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $sub-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $sub-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $sub-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $sub-ratnum-number	T:ratnum	T:number	T:number)
(declare-unsafe-binary-operation $sub-compnum-number	T:compnum	T:number	T:number)
(declare-unsafe-binary-operation $sub-cflonum-number	T:cflonum	T:number	T:number)

(declare-unsafe-binary-operation $sub-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $sub-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $sub-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $sub-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $sub-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $sub-number-cflonum	T:number	T:cflonum	T:number)

(declare-unsafe-binary-operation $sub-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $sub-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $sub-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $sub-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $sub-fixnum-compnum	T:fixnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $sub-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $sub-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $sub-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $sub-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $sub-bignum-compnum	T:bignum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $sub-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $sub-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $sub-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $sub-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $sub-flonum-compnum	T:flonum	T:compnum	T:non-real)
(declare-unsafe-binary-operation $sub-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $sub-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $sub-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $sub-ratnum-ratnum	T:ratnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $sub-ratnum-compnum	T:ratnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $sub-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-compnum-fixnum	T:compnum	T:fixnum	T:compnum)
(declare-unsafe-binary-operation $sub-compnum-bignum	T:compnum	T:bignum	T:compnum)
(declare-unsafe-binary-operation $sub-compnum-flonum	T:compnum	T:flonum	T:non-real)
(declare-unsafe-binary-operation $sub-compnum-ratnum	T:compnum	T:ratnum	T:compnum)
(declare-unsafe-binary-operation $sub-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $sub-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $sub-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-flonum	T:cflonum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-ratnum	T:cflonum	T:ratnum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-compnum	T:cflonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $sub-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $mul-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $mul-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $mul-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $mul-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $mul-ratnum-number	T:ratnum	T:number	T:number)
(declare-unsafe-binary-operation $mul-compnum-number	T:compnum	T:number	T:number)
(declare-unsafe-binary-operation $mul-cflonum-number	T:cflonum	T:number	T:cflonum)

(declare-unsafe-binary-operation $mul-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $mul-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $mul-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $mul-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $mul-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $mul-number-cflonum	T:number	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $mul-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $mul-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $mul-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $mul-fixnum-compnum	T:fixnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $mul-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $mul-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $mul-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $mul-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $mul-bignum-compnum	T:bignum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $mul-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $mul-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $mul-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $mul-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $mul-flonum-compnum	T:flonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $mul-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $mul-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $mul-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $mul-ratnum-ratnum	T:ratnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $mul-ratnum-compnum	T:ratnum	T:compnum	T:compnum)
(declare-unsafe-binary-operation $mul-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-compnum-fixnum	T:compnum	T:fixnum	T:compnum)
(declare-unsafe-binary-operation $mul-compnum-bignum	T:compnum	T:bignum	T:compnum)
(declare-unsafe-binary-operation $mul-compnum-flonum	T:compnum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $mul-compnum-ratnum	T:compnum	T:ratnum	T:compnum)
(declare-unsafe-binary-operation $mul-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $mul-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $mul-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-flonum	T:cflonum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-ratnum	T:cflonum	T:ratnum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-compnum	T:cflonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $mul-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $div-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $div-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $div-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $div-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $div-ratnum-number	T:ratnum	T:number	T:number)
(declare-unsafe-binary-operation $div-compnum-number	T:compnum	T:number	T:number)
(declare-unsafe-binary-operation $div-cflonum-number	T:cflonum	T:number	T:number)

(declare-unsafe-binary-operation $div-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $div-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $div-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $div-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $div-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $div-number-cflonum	T:number	T:cflonum	T:number)

(declare-unsafe-binary-operation $div-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $div-fixnum-bignum	T:fixnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $div-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $div-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $div-fixnum-compnum	T:fixnum	T:compnum	T:number)
(declare-unsafe-binary-operation $div-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-bignum-fixnum	T:bignum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $div-bignum-bignum	T:bignum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $div-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $div-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $div-bignum-compnum	T:bignum	T:compnum	T:number)
(declare-unsafe-binary-operation $div-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $div-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $div-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $div-ratnum-ratnum	T:ratnum	T:ratnum	T:exact-real)
(declare-unsafe-binary-operation $div-ratnum-compnum	T:ratnum	T:compnum	T:number)
(declare-unsafe-binary-operation $div-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $div-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $div-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $div-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $div-flonum-compnum	T:flonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $div-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-compnum-fixnum	T:compnum	T:fixnum	T:number)
(declare-unsafe-binary-operation $div-compnum-bignum	T:compnum	T:bignum	T:number)
(declare-unsafe-binary-operation $div-compnum-flonum	T:compnum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $div-compnum-ratnum	T:compnum	T:ratnum	T:number)
(declare-unsafe-binary-operation $div-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $div-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $div-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-flonum	T:cflonum	T:flonum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-ratnum	T:cflonum	T:ratnum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-compnum	T:cflonum	T:compnum	T:cflonum)
(declare-unsafe-binary-operation $div-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $square-fixnum		T:fixnum	T:exact-integer)
(declare-unsafe-unary-operation $square-bignum		T:bignum	T:exact-integer)
(declare-unsafe-unary-operation $square-ratnum		T:ratnum	T:exact-real)
(declare-unsafe-unary-operation $square-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $square-cflonum		T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $cube-fixnum		T:fixnum	T:exact-integer)
(declare-unsafe-unary-operation $cube-bignum		T:bignum	T:exact-integer)
(declare-unsafe-unary-operation $cube-ratnum		T:ratnum	T:exact-real)
(declare-unsafe-unary-operation $cube-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $cube-cflonum		T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $gcd-number		T:number	T:number)

(declare-unsafe-binary-operation $gcd-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $gcd-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $gcd-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $gcd-flonum-number	T:flonum	T:number	T:number)

(declare-unsafe-binary-operation $gcd-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $gcd-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $gcd-number-flonum	T:number	T:flonum	T:number)

(declare-unsafe-binary-operation $gcd-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $gcd-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $gcd-fixnum-flonum	T:fixnum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $gcd-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $gcd-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $gcd-bignum-flonum	T:bignum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $gcd-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $gcd-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $gcd-flonum-flonum	T:flonum	T:flonum	T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $lcm-number		T:number	T:number)

(declare-unsafe-binary-operation $lcm-number-number	T:number	T:number	T:number)

(declare-unsafe-binary-operation $lcm-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $lcm-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $lcm-flonum-number	T:flonum	T:number	T:number)

(declare-unsafe-binary-operation $lcm-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $lcm-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $lcm-number-flonum	T:number	T:flonum	T:number)

(declare-unsafe-binary-operation $lcm-fixnum-fixnum	T:fixnum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $lcm-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $lcm-fixnum-flonum	T:fixnum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $lcm-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $lcm-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $lcm-bignum-flonum	T:bignum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $lcm-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $lcm-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $lcm-flonum-flonum	T:flonum	T:flonum	T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-number	T:fixnum	T:number      T:number	      T:number)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-number	T:bignum	T:number      T:number	      T:number)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-number	T:flonum	T:number      T:number	      T:number)

(declare-unsafe-binary-operation/2rv $quotient+remainder-number-fixnum	T:number	T:fixnum      T:number	      T:number)
(declare-unsafe-binary-operation/2rv $quotient+remainder-number-bignum	T:number	T:bignum      T:number	      T:number)
(declare-unsafe-binary-operation/2rv $quotient+remainder-number-flonum	T:number	T:flonum      T:number	      T:number)

(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-fixnum	T:fixnum	T:fixnum      T:exact-integer T:exact-integer)
(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-bignum	T:fixnum	T:bignum      T:exact-integer T:exact-integer)
(declare-unsafe-binary-operation/2rv $quotient+remainder-fixnum-flonum	T:fixnum	T:flonum      T:flonum	      T:flonum)

(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-fixnum	T:bignum	T:fixnum      T:exact-integer T:exact-integer)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-bignum	T:bignum	T:bignum      T:exact-integer T:exact-integer)
(declare-unsafe-binary-operation/2rv $quotient+remainder-bignum-flonum	T:bignum	T:flonum      T:flonum	      T:flonum)

(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-fixnum	T:flonum	T:fixnum      T:flonum	      T:flonum)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-bignum	T:flonum	T:bignum      T:flonum	      T:flonum)
(declare-unsafe-binary-operation/2rv $quotient+remainder-flonum-flonum	T:flonum	T:flonum      T:flonum	      T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $quotient-fixnum-number	T:fixnum	T:number      T:number)
(declare-unsafe-binary-operation $quotient-bignum-number	T:bignum	T:number      T:number)
(declare-unsafe-binary-operation $quotient-flonum-number	T:flonum	T:number      T:number)

(declare-unsafe-binary-operation $quotient-number-fixnum	T:number	T:fixnum      T:number)
(declare-unsafe-binary-operation $quotient-number-bignum	T:number	T:bignum      T:number)
(declare-unsafe-binary-operation $quotient-number-flonum	T:number	T:flonum      T:number)

(declare-unsafe-binary-operation $quotient-fixnum-fixnum	T:fixnum	T:fixnum      T:exact-integer)
(declare-unsafe-binary-operation $quotient-fixnum-bignum	T:fixnum	T:bignum      T:exact-integer)
(declare-unsafe-binary-operation $quotient-fixnum-flonum	T:fixnum	T:flonum      T:flonum)

(declare-unsafe-binary-operation $quotient-bignum-fixnum	T:bignum	T:fixnum      T:exact-integer)
(declare-unsafe-binary-operation $quotient-bignum-bignum	T:bignum	T:bignum      T:exact-integer)
(declare-unsafe-binary-operation $quotient-bignum-flonum	T:bignum	T:flonum      T:flonum)

(declare-unsafe-binary-operation $quotient-flonum-fixnum	T:flonum	T:fixnum      T:flonum)
(declare-unsafe-binary-operation $quotient-flonum-bignum	T:flonum	T:bignum      T:flonum)
(declare-unsafe-binary-operation $quotient-flonum-flonum	T:flonum	T:flonum      T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $remainder-fixnum-number	T:fixnum	T:number      T:number)
(declare-unsafe-binary-operation $remainder-bignum-number	T:bignum	T:number      T:number)
(declare-unsafe-binary-operation $remainder-flonum-number	T:flonum	T:number      T:number)

(declare-unsafe-binary-operation $remainder-number-fixnum	T:number	T:fixnum      T:number)
(declare-unsafe-binary-operation $remainder-number-bignum	T:number	T:bignum      T:number)
(declare-unsafe-binary-operation $remainder-number-flonum	T:number	T:flonum      T:number)

(declare-unsafe-binary-operation $remainder-fixnum-fixnum	T:fixnum	T:fixnum      T:exact-integer)
(declare-unsafe-binary-operation $remainder-fixnum-bignum	T:fixnum	T:bignum      T:exact-integer)
(declare-unsafe-binary-operation $remainder-fixnum-flonum	T:fixnum	T:flonum      T:flonum)

(declare-unsafe-binary-operation $remainder-bignum-fixnum	T:bignum	T:fixnum      T:exact-integer)
(declare-unsafe-binary-operation $remainder-bignum-bignum	T:bignum	T:bignum      T:exact-integer)
(declare-unsafe-binary-operation $remainder-bignum-flonum	T:bignum	T:flonum      T:flonum)

(declare-unsafe-binary-operation $remainder-flonum-fixnum	T:flonum	T:fixnum      T:flonum)
(declare-unsafe-binary-operation $remainder-flonum-bignum	T:flonum	T:bignum      T:flonum)
(declare-unsafe-binary-operation $remainder-flonum-flonum	T:flonum	T:flonum      T:flonum)

;;; --------------------------------------------------------------------

(declare-core-primitive $numerator-fixnum
    (safe)
  (signatures
   ((T:fixnum)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $numerator-bignum
    (safe)
  (signatures
   ((T:bignum)			=> (T:bignum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $numerator-flonum
    (safe)
  (signatures
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $numerator-ratnum
    (safe)
  (signatures
   ((T:ratnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-fixnum
    (safe)
  (signatures
   ((T:fixnum)			=> (T:positive-fixnum)))
  (attributes
   ;;This always returns 1.
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-bignum
    (safe)
  (signatures
   ((T:bignum)			=> (T:positive-bignum)))
  (attributes
   ;;This always returns 1.
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-flonum
    (safe)
  (signatures
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $denominator-ratnum
    (safe)
  (signatures
   ((T:ratnum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $modulo-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $modulo-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $modulo-flonum-number	T:flonum	T:number	T:number)

(declare-unsafe-binary-operation $modulo-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $modulo-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $modulo-number-flonum	T:number	T:flonum	T:number)

(declare-unsafe-binary-operation $modulo-fixnum-fixnum	T:fixnum	T:fixnum	T:fixnum)
(declare-unsafe-binary-operation $modulo-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $modulo-fixnum-flonum	T:fixnum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $modulo-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $modulo-bignum-bignum	T:bignum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $modulo-bignum-flonum	T:bignum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $modulo-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $modulo-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $modulo-flonum-flonum	T:flonum	T:flonum	T:flonum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $max-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $max-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $max-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $max-ratnum-number	T:ratnum	T:number	T:number)

(declare-unsafe-binary-operation $max-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $max-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $max-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $max-number-ratnum	T:number	T:ratnum	T:number)

(declare-unsafe-binary-operation $max-fixnum-fixnum	T:fixnum	T:fixnum	T:fixnum)
(declare-unsafe-binary-operation $max-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $max-fixnum-flonum	T:fixnum	T:flonum	T:real)
(declare-unsafe-binary-operation $max-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)

(declare-unsafe-binary-operation $max-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $max-bignum-bignum	T:bignum	T:bignum	T:bignum)
(declare-unsafe-binary-operation $max-bignum-flonum	T:bignum	T:flonum	T:real)
(declare-unsafe-binary-operation $max-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)

(declare-unsafe-binary-operation $max-flonum-fixnum	T:flonum	T:fixnum	T:real)
(declare-unsafe-binary-operation $max-flonum-bignum	T:flonum	T:bignum	T:real)
(declare-unsafe-binary-operation $max-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $max-flonum-ratnum	T:flonum	T:ratnum	T:real)

(declare-unsafe-binary-operation $max-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $max-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $max-ratnum-flonum	T:ratnum	T:flonum	T:real)
(declare-unsafe-binary-operation $max-ratnum-ratnum	T:ratnum	T:ratnum	T:ratnum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $min-fixnum-number	T:fixnum	T:number	T:number)
(declare-unsafe-binary-operation $min-bignum-number	T:bignum	T:number	T:number)
(declare-unsafe-binary-operation $min-flonum-number	T:flonum	T:number	T:number)
(declare-unsafe-binary-operation $min-ratnum-number	T:ratnum	T:number	T:number)

(declare-unsafe-binary-operation $min-number-fixnum	T:number	T:fixnum	T:number)
(declare-unsafe-binary-operation $min-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $min-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $min-number-ratnum	T:number	T:ratnum	T:number)

(declare-unsafe-binary-operation $min-fixnum-fixnum	T:fixnum	T:fixnum	T:fixnum)
(declare-unsafe-binary-operation $min-fixnum-bignum	T:fixnum	T:bignum	T:exact-integer)
(declare-unsafe-binary-operation $min-fixnum-flonum	T:fixnum	T:flonum	T:real)
(declare-unsafe-binary-operation $min-fixnum-ratnum	T:fixnum	T:ratnum	T:exact-real)

(declare-unsafe-binary-operation $min-bignum-fixnum	T:bignum	T:fixnum	T:exact-integer)
(declare-unsafe-binary-operation $min-bignum-bignum	T:bignum	T:bignum	T:bignum)
(declare-unsafe-binary-operation $min-bignum-flonum	T:bignum	T:flonum	T:real)
(declare-unsafe-binary-operation $min-bignum-ratnum	T:bignum	T:ratnum	T:exact-real)

(declare-unsafe-binary-operation $min-flonum-fixnum	T:flonum	T:fixnum	T:real)
(declare-unsafe-binary-operation $min-flonum-bignum	T:flonum	T:bignum	T:real)
(declare-unsafe-binary-operation $min-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $min-flonum-ratnum	T:flonum	T:ratnum	T:real)

(declare-unsafe-binary-operation $min-ratnum-fixnum	T:ratnum	T:fixnum	T:exact-real)
(declare-unsafe-binary-operation $min-ratnum-bignum	T:ratnum	T:bignum	T:exact-real)
(declare-unsafe-binary-operation $min-ratnum-flonum	T:ratnum	T:flonum	T:real)
(declare-unsafe-binary-operation $min-ratnum-ratnum	T:ratnum	T:ratnum	T:ratnum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $abs-fixnum		T:fixnum	T:exact-integer)
(declare-unsafe-unary-operation $abs-bignum		T:bignum	T:exact-integer)
(declare-unsafe-unary-operation $abs-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $abs-ratnum		T:ratnum	T:ratnum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sign-fixnum		T:fixnum	T:fixnum)
(declare-unsafe-unary-operation $sign-bignum		T:bignum	T:fixnum)
(declare-unsafe-unary-operation $sign-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $sign-ratnum		T:ratnum	T:fixnum)

;;; --------------------------------------------------------------------

(declare-core-primitive $exact-fixnum
    (unsafe)
  (signatures
   ((T:positive-fixnum)		=> (T:positive-fixnum))
   ((T:negative-fixnum)		=> (T:negative-fixnum))
   ((T:non-positive-fixnum)	=> (T:non-positive-fixnum))
   ((T:non-negative-fixnum)	=> (T:non-negative-fixnum))
   ((T:fixnum)			=> (T:fixnum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-bignum
    (unsafe)
  (signatures
   ((T:positive-bignum)		=> (T:positive-bignum))
   ((T:negative-bignum)		=> (T:negative-bignum))
   ((T:bignum)			=> (T:bignum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-flonum
    (unsafe)
  (signatures
   ((T:flonum)			=> (T:exact-integer)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $exact-ratnum
    (unsafe)
  (signatures
   ((T:ratnum)			=> (T:ratnum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $exact-compnum
    (unsafe)
  (signatures
   ((T:compnum)			=> (T:exact-compnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $exact-cflonum
    (unsafe)
  (signatures
   ((T:cflonum)			=> (T:exact-compnum)))
  (attributes
   ((_)				foldable effect-free result-true)))

;;;

(declare-core-primitive $inexact-fixnum
    (unsafe)
  (signatures
   ((T:positive-fixnum)		=> (T:positive-flonum))
   ((T:negative-fixnum)		=> (T:negative-flonum))
   ((T:non-positive-fixnum)	=> (T:non-positive-flonum))
   ((T:non-negative-fixnum)	=> (T:non-negative-flonum))
   ((T:fixnum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-bignum
    (unsafe)
  (signatures
   ((T:positive-bignum)		=> (T:positive-flonum))
   ((T:negative-bignum)		=> (T:negative-flonum))
   ((T:bignum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-flonum
    (unsafe)
  (signatures
   ((T:flonum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

(declare-core-primitive $inexact-ratnum
    (unsafe)
  (signatures
   ((T:ratnum)			=> (T:flonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-compnum
    (unsafe)
  (signatures
   ((T:compnum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true)))

(declare-core-primitive $inexact-cflonum
    (unsafe)
  (signatures
   ((T:cflonum)			=> (T:cflonum)))
  (attributes
   ((_)				foldable effect-free result-true identity)))

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $expt-number-fixnum	T:number	T:number	T:number)

(declare-unsafe-unary-operation $expt-number-zero-fixnum	T:number	T:number)
(declare-unsafe-unary-operation $expt-fixnum-zero-fixnum	T:fixnum	T:fixnum)
(declare-unsafe-unary-operation $expt-flonum-zero-fixnum	T:flonum	T:flonum)
(declare-unsafe-unary-operation $expt-compnum-zero-fixnum	T:compnum	T:number)
(declare-unsafe-unary-operation $expt-cflonum-zero-fixnum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $expt-number-negative-fixnum	T:number	T:negative-fixnum      T:number)
(declare-unsafe-binary-operation $expt-fixnum-negative-fixnum	T:fixnum	T:negative-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-bignum-negative-fixnum	T:bignum	T:negative-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-flonum-negative-fixnum	T:flonum	T:negative-fixnum      T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-negative-fixnum	T:ratnum	T:negative-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-compnum-negative-fixnum	T:compnum	T:negative-fixnum      T:number)
(declare-unsafe-binary-operation $expt-cflonum-negative-fixnum	T:cflonum	T:negative-fixnum      T:cflonum)

(declare-unsafe-binary-operation $expt-number-positive-fixnum	T:number	T:positive-fixnum      T:number)
(declare-unsafe-binary-operation $expt-fixnum-positive-fixnum	T:number	T:positive-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-bignum-positive-fixnum	T:number	T:positive-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-flonum-positive-fixnum	T:number	T:positive-fixnum      T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-positive-fixnum	T:number	T:positive-fixnum      T:exact)
(declare-unsafe-binary-operation $expt-compnum-positive-fixnum	T:number	T:positive-fixnum      T:number)
(declare-unsafe-binary-operation $expt-cflonum-positive-fixnum	T:number	T:positive-fixnum      T:cflonum)

(declare-unsafe-binary-operation $expt-fixnum-fixnum	T:fixnum	T:fixnum	T:exact)
(declare-unsafe-binary-operation $expt-bignum-fixnum	T:bignum	T:fixnum	T:exact)
(declare-unsafe-binary-operation $expt-flonum-fixnum	T:flonum	T:fixnum	T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-fixnum	T:ratnum	T:fixnum	T:exact)
(declare-unsafe-binary-operation $expt-compnum-fixnum	T:compnum	T:fixnum	T:number)
(declare-unsafe-binary-operation $expt-cflonum-fixnum	T:cflonum	T:fixnum	T:cflonum)

;;;

(declare-unsafe-binary-operation $expt-number-bignum	T:number	T:bignum	T:number)
(declare-unsafe-binary-operation $expt-number-flonum	T:number	T:flonum	T:number)
(declare-unsafe-binary-operation $expt-number-ratnum	T:number	T:ratnum	T:number)
(declare-unsafe-binary-operation $expt-number-compnum	T:number	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-number-cflonum	T:number	T:cflonum	T:number)

(declare-unsafe-binary-operation $expt-fixnum-bignum	T:fixnum	T:bignum	T:exact)
(declare-unsafe-binary-operation $expt-bignum-bignum	T:bignum	T:bignum	T:exact)
(declare-unsafe-binary-operation $expt-flonum-bignum	T:flonum	T:bignum	T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-bignum	T:ratnum	T:bignum	T:exact)
(declare-unsafe-binary-operation $expt-compnum-bignum	T:compnum	T:bignum	T:number)
(declare-unsafe-binary-operation $expt-cflonum-bignum	T:cflonum	T:bignum	T:cflonum)

(declare-unsafe-binary-operation $expt-fixnum-flonum	T:fixnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-bignum-flonum	T:bignum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-flonum-flonum	T:flonum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-flonum	T:ratnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-compnum-flonum	T:compnum	T:flonum	T:flonum)
(declare-unsafe-binary-operation $expt-cflonum-flonum	T:cflonum	T:flonum	T:flonum)

(declare-unsafe-binary-operation $expt-fixnum-ratnum	T:fixnum	T:ratnum	T:exact)
(declare-unsafe-binary-operation $expt-bignum-ratnum	T:bignum	T:ratnum	T:exact)
(declare-unsafe-binary-operation $expt-flonum-ratnum	T:flonum	T:ratnum	T:flonum)
(declare-unsafe-binary-operation $expt-ratnum-ratnum	T:ratnum	T:ratnum	T:exact)
(declare-unsafe-binary-operation $expt-compnum-ratnum	T:compnum	T:ratnum	T:number)
(declare-unsafe-binary-operation $expt-cflonum-ratnum	T:cflonum	T:ratnum	T:cflonum)

(declare-unsafe-binary-operation $expt-fixnum-cflonum	T:fixnum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-bignum-cflonum	T:bignum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-flonum-cflonum	T:flonum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-ratnum-cflonum	T:ratnum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-compnum-cflonum	T:compnum	T:cflonum	T:cflonum)
(declare-unsafe-binary-operation $expt-cflonum-cflonum	T:cflonum	T:cflonum	T:cflonum)

(declare-unsafe-binary-operation $expt-fixnum-compnum	T:fixnum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-bignum-compnum	T:bignum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-flonum-compnum	T:flonum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-ratnum-compnum	T:ratnum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-compnum-compnum	T:compnum	T:compnum	T:number)
(declare-unsafe-binary-operation $expt-cflonum-compnum	T:cflonum	T:compnum	T:number)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $sqrt-fixnum		T:fixnum	T:number)
(declare-unsafe-unary-operation $sqrt-bignum		T:bignum	T:number)
(declare-unsafe-unary-operation $sqrt-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $sqrt-ratnum		T:ratnum	T:number)
(declare-unsafe-unary-operation $sqrt-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $sqrt-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation/2rv $exact-integer-sqrt-fixnum	T:fixnum	T:exact-integer	      T:exact-integer)
(declare-unsafe-unary-operation/2rv $exact-integer-sqrt-bignum	T:bignum	T:exact-integer	      T:exact-integer)

(declare-unsafe-unary-operation $cbrt-fixnum		T:fixnum	T:number)
(declare-unsafe-unary-operation $cbrt-bignum		T:bignum	T:number)
(declare-unsafe-unary-operation $cbrt-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $cbrt-ratnum		T:ratnum	T:number)
(declare-unsafe-unary-operation $cbrt-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $cbrt-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $log-fixnum		T:fixnum	T:number)
(declare-unsafe-unary-operation $log-bignum		T:bignum	T:inexact)
(declare-unsafe-unary-operation $log-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $log-ratnum		T:ratnum	T:inexact)
(declare-unsafe-unary-operation $log-compnum		T:compnum	T:inexact)
(declare-unsafe-unary-operation $log-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $exp-fixnum		T:fixnum	T:real)
(declare-unsafe-unary-operation $exp-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $exp-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $exp-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $exp-compnum		T:compnum	T:cflonum)
(declare-unsafe-unary-operation $exp-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $sin-fixnum		T:fixnum	T:real)
(declare-unsafe-unary-operation $sin-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $sin-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $sin-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $sin-compnum		T:compnum	T:cflonum)
(declare-unsafe-unary-operation $sin-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $cos-fixnum		T:fixnum	T:real)
(declare-unsafe-unary-operation $cos-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $cos-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $cos-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $cos-compnum		T:compnum	T:cflonum)
(declare-unsafe-unary-operation $cos-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $tan-fixnum		T:fixnum	T:real)
(declare-unsafe-unary-operation $tan-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $tan-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $tan-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $tan-compnum		T:compnum	T:cflonum)
(declare-unsafe-unary-operation $tan-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $asin-fixnum		T:fixnum	T:inexact)
(declare-unsafe-unary-operation $asin-bignum		T:bignum	T:inexact)
(declare-unsafe-unary-operation $asin-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $asin-ratnum		T:ratnum	T:inexact)
(declare-unsafe-unary-operation $asin-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $asin-cflonum		T:cflonum	T:inexact)

(declare-unsafe-unary-operation $acos-fixnum		T:fixnum	T:inexact)
(declare-unsafe-unary-operation $acos-bignum		T:bignum	T:inexact)
(declare-unsafe-unary-operation $acos-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $acos-ratnum		T:ratnum	T:inexact)
(declare-unsafe-unary-operation $acos-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $acos-cflonum		T:cflonum	T:inexact)

(declare-unsafe-binary-operation $atan2-real-real	T:real		T:real	T:flonum)

(declare-unsafe-unary-operation $atan-fixnum		T:fixnum	T:inexact)
(declare-unsafe-unary-operation $atan-bignum		T:bignum	T:inexact)
(declare-unsafe-unary-operation $atan-flonum		T:flonum	T:inexact)
(declare-unsafe-unary-operation $atan-ratnum		T:ratnum	T:inexact)
(declare-unsafe-unary-operation $atan-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $atan-cflonum		T:cflonum	T:inexact)

(declare-unsafe-unary-operation $sinh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $sinh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $sinh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $sinh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $sinh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $sinh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $cosh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $cosh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $cosh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $cosh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $cosh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $cosh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $tanh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $tanh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $tanh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $tanh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $tanh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $tanh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $asinh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $asinh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $asinh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $asinh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $asinh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $asinh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $acosh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $acosh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $acosh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $acosh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $acosh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $acosh-cflonum		T:cflonum	T:cflonum)

(declare-unsafe-unary-operation $atanh-fixnum		T:fixnum	T:flonum)
(declare-unsafe-unary-operation $atanh-bignum		T:bignum	T:flonum)
(declare-unsafe-unary-operation $atanh-flonum		T:flonum	T:flonum)
(declare-unsafe-unary-operation $atanh-ratnum		T:ratnum	T:flonum)
(declare-unsafe-unary-operation $atanh-compnum		T:compnum	T:number)
(declare-unsafe-unary-operation $atanh-cflonum		T:cflonum	T:cflonum)

;;; --------------------------------------------------------------------

(declare-unsafe-unary-operation $bitwise-not-fixnum		T:fixnum	T:fixnum)
(declare-unsafe-unary-operation $bitwise-not-bignum		T:bignum	T:bignum)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-and-fixnum-number	T:fixnum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-and-fixnum-fixnum	T:fixnum	T:fixnum	      T:fixnum)
(declare-unsafe-binary-operation $bitwise-and-fixnum-bignum	T:fixnum	T:bignum	      T:exact-integer)

(declare-unsafe-binary-operation $bitwise-and-bignum-number	T:bignum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-and-bignum-fixnum	T:bignum	T:fixnum	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-and-bignum-bignum	T:bignum	T:bignum	      T:exact-integer)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-ior-fixnum-number	T:fixnum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-ior-fixnum-fixnum	T:fixnum	T:fixnum	      T:fixnum)
(declare-unsafe-binary-operation $bitwise-ior-fixnum-bignum	T:fixnum	T:bignum	      T:exact-integer)

(declare-unsafe-binary-operation $bitwise-ior-bignum-number	T:bignum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-ior-bignum-fixnum	T:bignum	T:fixnum	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-ior-bignum-bignum	T:bignum	T:bignum	      T:exact-integer)

;;; --------------------------------------------------------------------

(declare-unsafe-binary-operation $bitwise-xor-fixnum-number	T:fixnum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-xor-fixnum-fixnum	T:fixnum	T:fixnum	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-xor-fixnum-bignum	T:fixnum	T:bignum	      T:exact-integer)

(declare-unsafe-binary-operation $bitwise-xor-bignum-number	T:bignum	T:exact-integer	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-xor-bignum-fixnum	T:bignum	T:fixnum	      T:exact-integer)
(declare-unsafe-binary-operation $bitwise-xor-bignum-bignum	T:bignum	T:bignum	      T:exact-integer)

;;; --------------------------------------------------------------------

(let-syntax
    ((declare-unsafe-rounding-primitives
       (syntax-rules ()
	 ((_ ?who-fixnum ?who-bignum ?who-flonum ?who-ratnum)
	  (begin
	    (declare-core-primitive ?who-fixnum
		(unsafe)
	      (signatures
	       ((T:fixnum)			=> (T:fixnum)))
	      (attributes
	       ((_)				foldable effect-free result-true identity)))
	    (declare-core-primitive ?who-bignum
		(unsafe)
	      (signatures
	       ((T:bignum)			=> (T:bignum)))
	      (attributes
	       ((_)				foldable effect-free result-true identity)))
	    (declare-core-primitive ?who-flonum
		(unsafe)
	      (signatures
	       ((T:flonum)			=> (T:flonum)))
	      (attributes
	       ((_)				foldable effect-free result-true)))
	    (declare-core-primitive ?who-ratnum
		(unsafe)
	      (signatures
	       ((T:ratnum)			=> (T:exact-integer)))
	      (attributes
	       ((_)				foldable effect-free result-true)))))
	 )))
  (declare-unsafe-rounding-primitives $floor-fixnum $floor-bignum $floor-flonum $floor-ratnum)
  (declare-unsafe-rounding-primitives $ceiling-fixnum $ceiling-bignum $ceiling-flonum $ceiling-ratnum)
  (declare-unsafe-rounding-primitives $truncate-fixnum $truncate-bignum $truncate-flonum $truncate-ratnum)
  (declare-unsafe-rounding-primitives $round-fixnum $round-bignum $round-flonum $round-ratnum)
  #| end of LET-SYNTAX |# )


;;;; time and dates, safe functions

(declare-type-predicate time?	T:time)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-time
    (safe)
  (signatures
   ((T:exact-integer T:exact-integer)	=> (T:time)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive current-time
    (safe)
  (signatures
   (()			=> (T:time)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive time-from-now
    (safe)
  (signatures
   ((T:time)		=> (T:time)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive time-second
    (safe)
  (signatures
   ((T:time)		=> (T:exact-integer)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive time-nanosecond
    (safe)
  (signatures
   ((T:time)		=> (T:exact-integer)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive time-gmt-offset
    (safe)
  (signatures
   ((T:time)		=> (T:exact-integer)))
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
		    ((T:time T:time)	=> (T:boolean)))
		   (attributes
		    ((_ _)		effect-free))))
		)))
  (declare time=?)
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
		    ((T:time T:time)	=> (T:time)))
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


;;;; promises, safe primitives

(declare-type-predicate promise?	T:promise)

(declare-core-primitive make-promise
    (safe)
  (signatures
   ((T:procedure)	=> (T:promise)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive force
    (safe)
  (signatures
   ((T:promise)		=> T:object)))


;;;; library names, safe primitives

(declare-core-primitive library-name?
    (safe)
  (signatures
   ((T:object)			=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-version-numbers?
    (safe)
  (signatures
   ((T:object)			=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-version-number?
    (safe)
  (signatures
   ((T:object)			=> (T:boolean)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-name-decompose
    (safe)
  (signatures
   ((T:object)		=> (T:proper-list T:proper-list)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-name->identifiers
    (safe)
  (signatures
   ((T:proper-list)		=> (T:proper-list)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive library-name->version
    (safe)
  (signatures
   ((T:proper-list)		=> (T:proper-list)))
  (attributes
   ((_)			foldable effect-free result-true)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:proper-list T:proper-list)	=> (T:boolean)))
		   (attributes
		    ((_ _)		foldable effect-free))))
		)))
  (declare library-name-identifiers=?)
  (declare library-name=?)
  (declare library-name<?)
  (declare library-name<=?)
  (declare library-version=?)
  (declare library-version<?)
  (declare library-version<=?)
  #| end of LET-SYNTAX |#)


;;;; library references and conformity, safe procedures

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((T:object)		=> (T:boolean)))
		   (attributes
		    ((_)		foldable effect-free))))
		)))
  (declare library-reference?)
  (declare library-version-reference?)
  (declare library-sub-version-reference?)
  (declare library-sub-version?)
  #| end of LET-SYNTAX |# )

(declare-core-primitive library-reference-decompose
    (safe)
  (signatures
   ((T:object)		=> (T:proper-list T:proper-list)))
  (attributes
   ((_)			foldable effect-free)))

(declare-core-primitive library-reference->identifiers
    (safe)
  (signatures
   ((T:proper-list)		=> (T:proper-list)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive library-reference->version-reference
    (safe)
  (signatures
   ((T:proper-list)		=> (T:proper-list)))
  (attributes
   ((_)			foldable effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive library-reference-identifiers=?
    (safe)
  (signatures
   ((T:proper-list T:proper-list)	=> (T:boolean)))
  (attributes
   ((_ _)		foldable effect-free)))

(declare-core-primitive conforming-sub-version-and-sub-version-reference?
    (safe)
  (signatures
   ((T:proper-list T:proper-list)	=> (T:boolean)))
  (attributes
   ((_ _)		foldable effect-free)))

(declare-core-primitive conforming-version-and-version-reference?
    (safe)
  (signatures
   ((T:proper-list T:proper-list)	=> (T:boolean)))
  (attributes
   ((_ _)		foldable effect-free)))

(declare-core-primitive conforming-library-name-and-library-reference?
    (safe)
  (signatures
   ((T:proper-list T:proper-list)	=> (T:boolean)))
  (attributes
   ((_ _)		foldable effect-free)))


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
