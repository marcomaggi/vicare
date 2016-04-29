;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for hashtables core primitives
;;Date: Fri Jan  1, 2016
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (typed-core-primitives hash-tables)
  (export typed-core-primitives.hash-tables)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.hash-tables)


;;;; hashtables, safe procedures

(section

;;; predicates

(declare-type-predicate hashtable? <hashtable>)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-hashtable
    (safe)
  (signatures
   ((<procedure> <procedure>)			=> (<hashtable>))
   ((<procedure> <procedure> <exact-integer>)	=> (<hashtable>)))
  (attributes
   ((_ _)				effect-free result-true)
   ((_ _ _)				effect-free result-true)))

(declare-core-primitive make-eq-hashtable
    (safe)
  (signatures
   (()					=> (<hashtable>))
   ((<exact-integer>)			=> (<hashtable>)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive make-eqv-hashtable
    (safe)
  (signatures
   (()					=> (<hashtable>))
   ((<exact-integer>)			=> (<hashtable>)))
  (attributes
   (()				effect-free result-true)
   ((_)				effect-free result-true)))

(declare-core-primitive hashtable-copy
    (safe)
  (signatures
   ((<hashtable>)			=> (<hashtable>))
   ((<hashtable> <top>)		=> (<hashtable>)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors and mutators

(declare-core-primitive hashtable-ref
    (safe)
  (signatures
   ((<hashtable> <top>	)	=> (<top>))
   ((<hashtable> <top> <top>)	=> (<top>)))
  (attributes
   ((_ _)			effect-free)
   ((_ _ _)			effect-free)))

(declare-core-primitive hashtable-set!
    (safe)
  (signatures
   ((<hashtable> <top> <top>)	=> (<void>)))
  (attributes
   ((_ _ _)				result-true)))

(declare-core-primitive hashtable-delete!
    (safe)
  (signatures
   ((<hashtable> <top>)		=> (<void>)))
  (attributes
   ((_ _) 				result-true)))

(declare-core-primitive hashtable-clear!
    (safe)
  (signatures
   ((<hashtable>)			=> (<void>))
   ((<hashtable> <exact-integer>)	=> (<void>)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)))

(declare-core-primitive hashtable-update!
    (safe)
  (signatures
   ((<hashtable> <top> <procedure> <top>)	=> (<void>)))
  (attributes
   ((_ _ _ _)				result-true)))

;;; --------------------------------------------------------------------
;;; inspection

(declare-core-primitive hashtable-contains?
    (safe)
  (signatures
   ((<hashtable> <top>)	=> (<boolean>)))
  (attributes
   ((_ _)			effect-free)))


(declare-core-primitive hashtable-entries
    (safe)
  (signatures
   ((<hashtable>)		=> (<vector> <vector>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-keys
    (safe)
  (signatures
   ((<hashtable>)		=> (<vector>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive hashtable-mutable?
    (safe)
  (signatures
   ((<hashtable>)		=> (<boolean>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive mutable-hashtable?
    (safe)
  (signatures
   ((<hashtable>)				=> (<boolean>))
   (((and (not <hashtable>)
	  (not (ancestor-of <hashtable>))))	=> (<false>))
   (((not <hashtable>))				=> (<false>)))
  (attributes
   ((_)				foldable effect-free)))

(declare-core-primitive hashtable-size
    (safe)
  (signatures
   ((<hashtable>)		=> (<exact-integer>)))
  (attributes
   ((_)				effect-free result-true)))

(declare-core-primitive hashtable-hash-function
    (safe)
  (signatures
   ((<hashtable>)		=> (<top>)))
  (attributes
   ;;This returns false for EQ? and EQV? hashtables!!!
   ((_)				effect-free)))

(declare-core-primitive hashtable-equivalence-function
    (safe)
  (signatures
   ((<hashtable>)		=> (<procedure>)))
  (attributes
   ((_)				effect-free result-true)))

/section)


;;;; safe hashtable iterators
;;
;;NOTE All the procedures that apply operand functions might generate side effects!!!
;;

(section

(declare-core-primitive hashtable-map-keys
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<hashtable>)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive hashtable-map-entries
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<hashtable>)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive hashtable-for-each-key
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<void>)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive hashtable-for-each-entry
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<void>)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive hashtable-for-all-keys
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<top>)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive hashtable-for-all-entries
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<top>)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive hashtable-exists-key
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<top>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive hashtable-exists-entry
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<top>)))
  (attributes
   ((_ _)				effect-free)))

(declare-core-primitive hashtable-find-key
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<top>))))

(declare-core-primitive hashtable-find-entry
    (safe)
  (signatures
   ((<procedure> <hashtable>)		=> (<top>))))

(declare-core-primitive hashtable-fold-keys
    (safe)
  (signatures
   ((<procedure> <top> <hashtable>)		=> (<top>))))

(declare-core-primitive hashtable-fold-entries
    (safe)
  (signatures
   ((<procedure> <top> <hashtable>)		=> (<top>))))

(declare-core-primitive alist->hashtable!
    (safe)
  (signatures
   ((<hashtable> <list>)		=> (<hashtable>)))
  (attributes
   ((_ _)				result-true)))

(declare-core-primitive hashtable->alist
    (safe)
  (signatures
   ((<hashtable>)				=> (<list>))
   ((<hashtable> <false>)			=> (<list>))
   ((<hashtable> <procedure>)			=> (<list>)))
  (attributes
   ((_)					result-true)
   ((_ _)				result-true)))

/section)


;;; safe hash functions

(section

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag . ?replacements)
			       (declare-core-primitive ?who
				   (safe)
				 (signatures
				  ((?obj-tag)				=> (<fixnum>))
				  ((?obj-tag <non-negative-fixnum>)	=> (<fixnum>))
				  ((?obj-tag <boolean>)			=> (<fixnum>)))
				 (attributes
				  ((_)			foldable effect-free result-true)
				  ((_ _)		foldable effect-free result-true))
				 . ?replacements))
			      )))
  (declare-hash-function string-hash		<string>	(replacements $string-hash))
  (declare-hash-function string-ci-hash		<string>	(replacements $string-ci-hash))
  (declare-hash-function bytevector-hash	<bytevector>	(replacements $bytevector-hash))
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag . ?replacements)
			       (declare-core-primitive ?who
				   (safe)
				 (signatures
				  ((?obj-tag)		=> (<fixnum>)))
				 (attributes
				  ((_)			foldable effect-free result-true))
				 . ?replacements))
			      )))
  (declare-hash-function equal-hash		<top>)
  (declare-hash-function symbol-hash		<symbol>	(replacements $symbol-hash))
  (declare-hash-function port-hash		<port>)
  (declare-hash-function char-hash		<char>)
  (declare-hash-function char-ci-hash		<char>)
  (declare-hash-function boolean-hash		<boolean>)
  (declare-hash-function fixnum-hash		<fixnum>)
  (declare-hash-function exact-integer-hash	<exact-integer>)
  (declare-hash-function flonum-hash		<flonum>)
  (declare-hash-function number-hash		<number>)
  (declare-hash-function struct-hash		<struct>)
  (declare-hash-function record-hash		<record>)
  ;;Notice that "<void>" is forbidden as type in arguments signatures.
  (declare-hash-function void-hash		<top>)
  (declare-hash-function eof-object-hash	<eof>)
  (declare-hash-function would-block-hash	<would-block>)
  (declare-hash-function object-hash		<top>)
  #| end of LET-SYNTAX |# )

(declare-core-primitive pointer-value
    (safe)
  (signatures
   ((<top>)		=> (<fixnum>)))
  (attributes
   ((_)			effect-free result-true)))

/section)


;;;; hashtables, unsafe procedures

(section

;;; hash functions

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag)
			       (declare-core-primitive ?who
				   (unsafe)
				 (signatures
				  ((?obj-tag)				=> (<fixnum>))
				  ((?obj-tag <non-negative-fixnum>)	=> (<fixnum>))
				  ((?obj-tag <boolean>)			=> (<fixnum>)))
				 (attributes
				  ((_)			foldable effect-free result-true)
				  ((_ _)		foldable effect-free result-true))))
			      )))
  (declare-hash-function $string-hash		<string>)
  (declare-hash-function $string-ci-hash	<string>)
  (declare-hash-function $bytevector-hash	<bytevector>)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag)
			       (declare-core-primitive ?who
				   (unsafe)
				 (signatures
				  ((?obj-tag)		=> (<fixnum>)))
				 (attributes
				  ((_)			foldable effect-free result-true))))
			      )))
  (declare-hash-function $symbol-hash		<symbol>)
  #| end of LET-SYNTAX |# )

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
