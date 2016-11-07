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
  (import (rnrs)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.hash-tables)


;;;; hashtables, safe procedures

(section

;;; predicates

(declare-type-predicate hashtable? <hashtable>)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<hashtable>)			=> (<boolean>))
		    (((ancestor-of <hashtable>))	=> (<boolean>))
		    (((not <hashtable>))		=> (<false>)))))
		)))
  (declare hashtable-eq?)
  (declare hashtable-eqv?)
  (declare hashtable-equiv?)
  #| end of LET-SYNTAX |# )

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
   ((<hashtable> <top>)		=> (<top> <top>)))
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
				  ((?obj-tag)				=> (<non-negative-fixnum>))
				  ((?obj-tag <non-negative-fixnum>)	=> (<non-negative-fixnum>))
				  ((?obj-tag <boolean>)			=> (<non-negative-fixnum>)))
				 (attributes
				  ((_)			foldable effect-free result-true)
				  ((_ _)		foldable effect-free result-true))
				 . ?replacements))
			      )))
  (declare-hash-function string-hash		<string>	(replacements $string-hash))
  (declare-hash-function string-ci-hash		<string>	(replacements $string-ci-hash))
  (declare-hash-function bytevector-hash	<bytevector>	(replacements $bytevector-hash))
  (declare-hash-function vector-hash		<vector>	(replacements $vector-hash))
  (declare-hash-function list-hash		<list>		(replacements $list-hash))
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare-hash-function (syntax-rules ()
			      ((_ ?who ?obj-tag . ?replacements)
			       (declare-core-primitive ?who
				   (safe)
				 (signatures
				  ((?obj-tag)		=> (<non-negative-fixnum>)))
				 (attributes
				  ((_)			foldable effect-free result-true))
				 . ?replacements))
			      )))
  (declare-hash-function pair-hash		<pair>		(replacements $pair-hash))
  (declare-hash-function ipair-hash		<ipair>		(replacements $ipair-hash))
  (declare-hash-function equal-hash		<top>)
  (declare-hash-function symbol-hash		<symbol>	(replacements $symbol-hash))
  (declare-hash-function port-hash		<port>)
  (declare-hash-function char-hash		<char>		(replacements $char-hash))
  (declare-hash-function char-ci-hash		<char>		(replacements $char-hash))
  (declare-hash-function boolean-hash		<boolean>	(replacements $boolean-hash))
  (declare-hash-function fixnum-hash		<fixnum>	(replacements $fixnum-hash))
  (declare-hash-function bignum-hash		<bignum>	(replacements $bignum-hash))
  (declare-hash-function exact-integer-hash	<exact-integer>	(replacements $exact-integer-hash))
  (declare-hash-function flonum-hash		<flonum>	(replacements $flonum-hash))
  (declare-hash-function ratnum-hash		<ratnum>	(replacements $ratnum-hash))
  (declare-hash-function cflonum-hash		<cflonum>	(replacements $cflonum-hash))
  (declare-hash-function compnum-hash		<compnum>	(replacements $compnum-hash))
  (declare-hash-function number-hash		<number>)
  (declare-hash-function struct-hash		<struct>	(replacements $struct-hash))
  (declare-hash-function record-hash		<record>	(replacements $record-hash))
  ;;Notice that "<void>" is forbidden as type in arguments signatures.
  (declare-hash-function void-hash		<top>)
  (declare-hash-function eof-object-hash	<eof>)
  (declare-hash-function would-block-hash	<would-block>)
  (declare-hash-function sentinel-hash		<sentinel>)
  (declare-hash-function enum-set-hash		<enum-set>	(replacements $enum-set-hash))
  (declare-hash-function promise-hash		<promise>	(replacements $promise-hash))
  (declare-hash-function transcoder-hash	<transcoder>	(replacements $transcoder-hash))
  (declare-hash-function pointer-hash		<pointer>	(replacements $pointer-hash))
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
  (declare-hash-function $vector-hash		<vector>)
  (declare-hash-function $list-hash		<list>)
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
  (declare-hash-function $pair-hash		<pair>)
  (declare-hash-function $ipair-hash		<ipair>)
  (declare-hash-function $boolean-hash		<boolean>)
  (declare-hash-function $char-ci-hash		<char>)
  (declare-hash-function $char-hash		<char>)
  (declare-hash-function $exact-integer-hash	<exact-integer>)
  (declare-hash-function $fixnum-hash		<fixnum>)
  (declare-hash-function $bignum-hash		<bignum>)
  (declare-hash-function $flonum-hash		<flonum>)
  (declare-hash-function $ratnum-hash		<ratnum>)
  (declare-hash-function $cflonum-hash		<cflonum>)
  (declare-hash-function $compnum-hash		<compnum>)
  (declare-hash-function $record-hash		<record>)
  (declare-hash-function $struct-hash		<struct>)
  (declare-hash-function $symbol-hash		<symbol>)
  (declare-hash-function $enum-set-hash		<enum-set>)
  (declare-hash-function $promise-hash		<promise>)
  (declare-hash-function $transcoder-hash	<transcoder>)
  (declare-hash-function $pointer-hash		<pointer>)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(declare-core-primitive $hashtable-type-descriptor-set!
    (unsafe)
  (signatures
   ((<hashtable> <hashtable-type-descr>)	=> (<void>))))

(declare-core-primitive $hashtable-type-descriptor
    (unsafe)
  (signatures
   ((<hashtable>)				=> (<hashtable-type-descr>))))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
