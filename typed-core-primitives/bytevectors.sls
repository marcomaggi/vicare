;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for bytevectors core primitives
;;Date: Tue Dec 23, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives bytevectors)
  (export typed-core-primitives.bytevectors)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.bytevectors)


;;;; bytevectors, safe functions

(section

(declare-core-primitive make-bytevector
    (safe)
  (signatures
   ((<non-negative-fixnum>)			=> (<bytevector>))
   ((<non-negative-fixnum> <fixnum>)		=> (<bytevector>)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive bytevector-copy
    (safe)
  (signatures
   ((<bytevector>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_)				effect-free result-true)))

(declare-core-primitive bytevector-append
    (safe)
  (signatures
   (<bytevector>		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   (_				effect-free result-true)))

(declare-core-primitive bytevector-reverse-and-concatenate
    (safe)
  (signatures
   ((<list>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_)				effect-free result-true)))

;;;

(declare-core-primitive subbytevector-u8
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum>)			=> (<bytevector>))
   ((<bytevector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive subbytevector-s8
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum>)			=> (<bytevector>))
   ((<bytevector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive subbytevector-u8/count
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive subbytevector-s8/count
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _ _)				effect-free result-true)))

;;; --------------------------------------------------------------------
;;; predicates

(declare-type-predicate bytevector? <bytevector>)

(declare-bytevector-predicate bytevector-empty?			(replacements $bytevector-empty?))

(declare-bytevector-predicate ascii-encoded-bytevector?		(replacements $ascii-encoded-bytevector?))
(declare-bytevector-predicate latin1-encoded-bytevector?	(replacements $latin1-encoded-bytevector?))
(declare-bytevector-predicate octets-encoded-bytevector?	(replacements $octets-encoded-bytevector?))
(declare-bytevector-predicate uri-encoded-bytevector?		(replacements $uri-encoded-bytevector?))
(declare-bytevector-predicate percent-encoded-bytevector?	(replacements $percent-encoded-bytevector?))

(declare-core-primitive list-of-bytevectors?
    (safe)
  (signatures
   ((<list>)		=> (<boolean>)))
  (attributes
   ((_)				foldable effect-free)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive bytevector-length
    (safe)
  (signatures
   ((<bytevector>)	=> (<non-negative-fixnum>)))
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
   ((<bytevector> <non-negative-fixnum>)	=> (<void>))
   ((<bytevector> <fixnum>)	=> (<void>)))
  (attributes
   ((_ _)			result-true)))

(declare-core-primitive bytevector-copy!
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <bytevector> <non-negative-fixnum> <non-negative-fixnum>)     => (<void>)))
  (attributes
   ((_ _ _ _ _)			result-true)))

;;;

(declare-core-primitive bytevector-s8-ref
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum>)	=> (<fixnum>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(declare-core-primitive bytevector-u8-ref
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum>)	=> (<non-negative-fixnum>)))
  (attributes
   ((_ _)			foldable effect-free result-true)))

(let-syntax
    ((declare-safe-bytevector-accessor
      (syntax-rules ()
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((<bytevector> <non-negative-fixnum>)	=> (?return-value-tag)))
	   (attributes
	    ((_ _)		foldable effect-free result-true))))
	)))
  (declare-safe-bytevector-accessor bytevector-s16-native-ref		<fixnum>)
  (declare-safe-bytevector-accessor bytevector-u16-native-ref		<non-negative-fixnum>)
  (declare-safe-bytevector-accessor bytevector-s32-native-ref		<exact-integer>)
  (declare-safe-bytevector-accessor bytevector-u32-native-ref		<exact-integer>)
  (declare-safe-bytevector-accessor bytevector-s64-native-ref		<exact-integer>)
  (declare-safe-bytevector-accessor bytevector-u64-native-ref		<exact-integer>)
  (declare-safe-bytevector-accessor bytevector-ieee-double-native-ref	<flonum>)
  (declare-safe-bytevector-accessor bytevector-ieee-single-native-ref	<flonum>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-safe-bytevector-accessor
      (syntax-rules ()
	((_ ?who ?return-value-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((<bytevector> <non-negative-fixnum> <symbol>)	=> (?return-value-tag)))
	   (attributes
	    ((_ _)		foldable effect-free result-true))))
	)))
  (declare-safe-bytevector-accessor bytevector-s16-ref		<fixnum>)
  (declare-safe-bytevector-accessor bytevector-u16-ref		<non-negative-fixnum>)
  (declare-safe-bytevector-accessor bytevector-s32-ref		<exact-integer>)
  (declare-safe-bytevector-accessor bytevector-u32-ref		<exact-integer>)
  (declare-safe-bytevector-accessor bytevector-s64-ref		<exact-integer>)
  (declare-safe-bytevector-accessor bytevector-u64-ref		<exact-integer>)
  (declare-safe-bytevector-accessor bytevector-ieee-double-ref	<flonum>)
  (declare-safe-bytevector-accessor bytevector-ieee-single-ref	<flonum>)
  #| end of LET-SYNTAX |# )

(declare-core-primitive bytevector-sint-ref
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <symbol> <positive-fixnum>)	=> (<exact-integer>)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

(declare-core-primitive bytevector-uint-ref
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <symbol> <positive-fixnum>)	=> (<exact-integer>)))
  (attributes
   ((_ _ _ _)			foldable effect-free result-true)))

;;;

(declare-core-primitive bytevector-s8-set!
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <fixnum>)	=> (<void>))))

(declare-core-primitive bytevector-u8-set!
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<void>))))

(let-syntax
    ((declare-safe-bytevector-mutator
      (syntax-rules ()
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((<bytevector> <non-negative-fixnum> ?new-value-tag)	=> (<void>)))))
	)))
  (declare-safe-bytevector-mutator bytevector-s16-native-set!		<fixnum>)
  (declare-safe-bytevector-mutator bytevector-u16-native-set!		<non-negative-fixnum>)
  (declare-safe-bytevector-mutator bytevector-s32-native-set!		<exact-integer>)
  (declare-safe-bytevector-mutator bytevector-u32-native-set!		<exact-integer>)
  (declare-safe-bytevector-mutator bytevector-s64-native-set!		<exact-integer>)
  (declare-safe-bytevector-mutator bytevector-u64-native-set!		<exact-integer>)
  (declare-safe-bytevector-mutator bytevector-ieee-double-native-set!	<flonum>)
  (declare-safe-bytevector-mutator bytevector-ieee-single-native-set!	<flonum>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-safe-bytevector-mutator
      (syntax-rules ()
	((_ ?who ?new-value-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((<bytevector> <non-negative-fixnum> ?new-value-tag <symbol>)	=> (<void>)))))
	)))
  (declare-safe-bytevector-mutator bytevector-s16-set!		<fixnum>)
  (declare-safe-bytevector-mutator bytevector-u16-set!		<non-negative-fixnum>)
  (declare-safe-bytevector-mutator bytevector-s32-set!		<exact-integer>)
  (declare-safe-bytevector-mutator bytevector-u32-set!		<exact-integer>)
  (declare-safe-bytevector-mutator bytevector-s64-set!		<exact-integer>)
  (declare-safe-bytevector-mutator bytevector-u64-set!		<exact-integer>)
  (declare-safe-bytevector-mutator bytevector-ieee-double-set!	<flonum>)
  (declare-safe-bytevector-mutator bytevector-ieee-single-set!	<flonum>)
  #| end of LET-SYNTAX |# )

(declare-core-primitive bytevector-sint-set!
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <exact-integer> <symbol> <positive-fixnum>)	=> (<exact-integer>))))

(declare-core-primitive bytevector-uint-set!
    (safe)
  (signatures
   ((<bytevector> <non-negative-fixnum> <exact-integer> <symbol> <positive-fixnum>)	=> (<exact-integer>))))

;;; --------------------------------------------------------------------
;;; conversion

(declare-core-primitive bytevector->string
    (safe)
  (signatures
   ((<bytevector> <transcoder>)	=> (<string>)))
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
	    ((<bytevector>)	=> (<string>)))
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
   ((<bytevector>)		=> (<string>))
   ((<bytevector> <symbol>)	=> (<string>)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_) 		effect-free result-true)
   ((_ _) 		effect-free result-true)))

(declare-core-primitive utf8->string-length
    (safe)
  (signatures
   ((<bytevector>)		=> (<fixnum>))
   ((<bytevector> <symbol>)	=> (<fixnum>)))
  (attributes
   ;;Not foldable because it must return a new string at every application.
   ((_) 		effect-free result-true)
   ((_ _) 		effect-free result-true)))

(declare-core-primitive utf16->string
    (safe)
  (signatures
   ((<bytevector> <symbol>)			=> (<string>))
   ((<bytevector> <symbol> <top>)		=> (<string>))
   ((<bytevector> <symbol> <top> <symbol>)	=> (<string>)))
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
	    ((<bytevector>)		=> (<string>))
	    ((<bytevector> <symbol>)	=> (<string>)))
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
   ((<bytevector> <symbol>)			=> (<string>))
   ((<bytevector> <symbol> <top>)		=> (<string>))
   ((<bytevector> <symbol> <top> <symbol>)	=> (<string>)))
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
	    ((<bytevector>)	=> (<bytevector>)))
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
   ((<bytevector> <symbol> <positive-fixnum>)	=> (<list>)))
  (attributes
   ;;Not foldable because it must return a new list at every application.
   ((_ _ _) 		effect-free result-true)))

(declare-core-primitive bytevector->uint-list
    (safe)
  (signatures
   ((<bytevector> <symbol> <positive-fixnum>)	=> (<list>)))
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
	    ((<bytevector>)	=> (<list>)))
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

(declare-type-predicate bytevector-length?	<non-negative-fixnum>)
(declare-type-predicate bytevector-index?	<non-negative-fixnum>)

(let-syntax
    ((declare-bytevector-releated-fixnum-predicate
      (syntax-rules ()
	((_ ?who ?obj-tag)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((?obj-tag)		=> (<boolean>))
	    ((<top>)		=> (<false>)))
	   (attributes
	    ((_)		foldable effect-free result-true))))
	)))
  (declare-bytevector-releated-fixnum-predicate bytevector-word-size?	<positive-fixnum>)
  (declare-bytevector-releated-fixnum-predicate bytevector-word-count?	<non-negative-fixnum>)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word?	<non-negative-fixnum>)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word8?	<non-negative-fixnum>)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word16?	<non-negative-fixnum>)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word32?	<non-negative-fixnum>)
  (declare-bytevector-releated-fixnum-predicate bytevector-index-for-word64?	<non-negative-fixnum>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-bytevector-releated-fixnum-predicate
      (syntax-rules ()
	((_ ?who)
	 (declare-core-primitive ?who
	     (safe)
	   (signatures
	    ((<bytevector> <non-negative-fixnum> <non-negative-fixnum>)	=> (<boolean>)))
	   (attributes
	    ((_ _ _)		foldable effect-free result-true))))
	)))
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word?)
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word8?)
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word16?)
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word32?)
  (declare-bytevector-releated-fixnum-predicate bytevector-start-index-and-count-for-word64?)
  #| end of LET-SYNTAX |# )

/section)


;;;; bytevectors, unsafe functions

(section

;;; constructors

(declare-core-primitive $make-bytevector
    (unsafe)
  (signatures
   ((<fixnum>)		=> (<bytevector>))
   ((<fixnum> <fixnum>)	=> (<bytevector>)))
  ;;Not foldable because it must return a newly allocated bytevector.
  (attributes
   ((0)				effect-free result-true)
   ((0 _)			effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive $bytevector-copy
    (unsafe)
  (signatures
   ((<bytevector>)		=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_)				effect-free result-true)))

(declare-core-primitive $bytevector-concatenate
    (unsafe)
  (signatures
   ((<exact-integer> <list>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _)			effect-free result-true)))

(declare-core-primitive $bytevector-reverse-and-concatenate
    (unsafe)
  (signatures
   ((<exact-integer> <list>)	=> (<bytevector>)))
  (attributes
   ;;Not foldable because it must return a newly allocated bytevector.
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive $bytevector-length
    (unsafe)
  (signatures
   ((<bytevector>)		=> (<non-negative-fixnum>)))
  (attributes
   ((_)			foldable effect-free result-true)))

(declare-core-primitive $bytevector-total-length
    (unsafe)
  (signatures
   ((<exact-integer> <list>)	=> (<exact-integer>)))
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

(declare-unsafe-bytevector-accessor $bytevector-u8-ref				<non-negative-fixnum>)
(declare-unsafe-bytevector-accessor $bytevector-s8-ref				<fixnum>)
(declare-core-primitive $bytevector-set!
    (unsafe)
  (signatures
   ((<bytevector> <fixnum> <non-negative-fixnum>)	=> (<void>))
   ((<bytevector> <fixnum> <fixnum>)	=> (<void>))))

(declare-unsafe-bytevector-accessor $bytevector-ieee-double-native-ref		<flonum>)
(declare-unsafe-bytevector-mutator  $bytevector-ieee-double-native-set!		<flonum>)

(declare-unsafe-bytevector-accessor $bytevector-ieee-double-nonnative-ref	<flonum>)
(declare-unsafe-bytevector-mutator  $bytevector-ieee-double-nonnative-set!	<flonum>)

(declare-unsafe-bytevector-accessor $bytevector-ieee-single-native-ref		<flonum>)
(declare-unsafe-bytevector-mutator  $bytevector-ieee-single-native-set!		<flonum>)

(declare-unsafe-bytevector-accessor $bytevector-ieee-single-nonnative-ref	<flonum>)
(declare-unsafe-bytevector-mutator  $bytevector-ieee-single-nonnative-set!	<flonum>)

;;; --------------------------------------------------------------------
;;; conversion

(declare-unsafe-bytevector-conversion $uri-encode			<bytevector>)
(declare-unsafe-bytevector-conversion $uri-decode			<bytevector>)
(declare-unsafe-bytevector-conversion $uri-normalise-encoding		<bytevector>)
(declare-unsafe-bytevector-conversion $percent-encode			<bytevector>)
(declare-unsafe-bytevector-conversion $percent-decode			<bytevector>)
(declare-unsafe-bytevector-conversion $percent-normalise-encoding	<bytevector>)
(declare-unsafe-bytevector-conversion $bytevector->base64		<bytevector>)
(declare-unsafe-bytevector-conversion $base64->bytevector		<bytevector>)
(declare-unsafe-bytevector-conversion $ascii->string			<string>)
(declare-unsafe-bytevector-conversion $octets->string			<string>)
(declare-unsafe-bytevector-conversion $latin1->string			<string>)
(declare-unsafe-bytevector-conversion $bytevector->string-base64	<string>)

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 2)
;; End:
