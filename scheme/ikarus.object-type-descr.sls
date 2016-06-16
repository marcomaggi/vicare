;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: object type descriptors
;;;Date: Thu Jun  2, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.object-type-descr)
  (export

    <lambda-descriptors>-rtd			<lambda-descriptors>-rcd
    make-lambda-descriptors			lambda-descriptors?
    lambda-descriptors.retvals			lambda-descriptors.argvals

    <case-lambda-descriptors>-rtd		<case-lambda-descriptors>-rcd
    make-case-lambda-descriptors		case-lambda-descriptors?
    case-lambda-descriptors.clause-signature*

    <descriptors-signature>-rtd			<descriptors-signature>-rcd
    make-descriptors-signature			descriptors-signature?
    descriptors-signature.object-type-descrs

;;; --------------------------------------------------------------------

    <compound-condition-type-descr>-rtd		<compound-condition-type-descr>-rcd
    make-compound-condition-type-descr		compound-condition-type-descr?
    compound-condition-type-descr.component-des*

    <hashtable-type-descr>-rtd			<hashtable-type-descr>-rcd
    make-hashtable-type-descr			hashtable-type-descr?
    hashtable-type-descr.key-des		hashtable-type-descr.val-des

    <alist-type-descr>-rtd			<alist-type-descr>-rcd
    make-alist-type-descr			alist-type-descr?
    alist-type-descr.key-des			alist-type-descr.val-des

    <enumeration-type-descr>-rtd		<enumeration-type-descr>-rcd
    make-enumeration-type-descr			enumeration-type-descr?
    enumeration-type-descr.symbol*

    <closure-type-descr>-rtd			<closure-type-descr>-rcd
    make-closure-type-descr			closure-type-descr?
    closure-type-descr.signature

;;; --------------------------------------------------------------------

    <pair-type-descr>-rtd			<pair-type-descr>-rcd
    make-pair-type-descr			pair-type-descr?
    pair-type-descr.car-des			pair-type-descr.cdr-des

    <pair-of-type-descr>-rtd			<pair-of-type-descr>-rcd
    make-pair-of-type-descr			pair-of-type-descr?
    pair-of-type-descr.item-des

    <list-type-descr>-rtd			<list-type-descr>-rcd
    make-list-type-descr			list-type-descr?
    list-type-descr.item-des*

    <list-of-type-descr>-rtd			<list-of-type-descr>-rcd
    make-list-of-type-descr			list-of-type-descr?
    list-of-type-descr.item-des

    <vector-type-descr>-rtd			<vector-type-descr>-rcd
    make-vector-type-descr			vector-type-descr?
    vector-type-descr.item-des*

    <vector-of-type-descr>-rtd			<vector-of-type-descr>-rcd
    make-vector-of-type-descr			vector-of-type-descr?
    vector-of-type-descr.item-des

;;; --------------------------------------------------------------------

    <ancestor-of-type-descr>-rtd		<ancestor-of-type-descr>-rcd
    make-ancestor-of-type-descr			ancestor-of-type-descr?
    ancestor-of-type-descr.item-des		ancestor-of-type-descr.ancestor-des*

;;; --------------------------------------------------------------------

    <union-type-descr>-rtd			<union-type-descr>-rcd
    make-union-type-descr			union-type-descr?
    union-type-descr.item-des*

    <intersection-type-descr>-rtd		<intersection-type-descr>-rcd
    make-intersection-type-descr		intersection-type-descr?
    intersection-type-descr.item-des*

    <complement-type-descr>-rtd			<complement-type-descr>-rcd
    make-complement-type-descr			complement-type-descr?
    complement-type-descr.item-des

;;; --------------------------------------------------------------------

    type-descriptor-of
    object-type-descr=?
    object-type-descr.parent
    object-type-descr.ancestor-des*
    object-type-descr.matching-super-and-sub?
    object-type-descr.compatible-super-and-sub?
    object-type-descr.matching-formal-and-operand

    descriptors-signature=?
    descriptors-signature.matching-super-and-sub?
    descriptors-signature.compatible-super-and-sub?
    descriptors-signature.match-formals-against-operands

    #| end of EXPORTS |# )
  (import (except (vicare)
		  ratnum-positive?
		  ratnum-negative?
		  exact-compnum?
		  zero-compnum?
		  zero-cflonum?)
    (only (ikarus.object-utilities)
	  core-type-descriptor?
	  core-type-descriptor=?
	  core-type-descriptor.uids-list
	  core-type-descriptor.parent
	  core-type-descriptor.ancestor-des*
	  ;;;
	  <void>-ctd				<top>-ctd			<no-return>-ctd
	  <true>-ctd				<false>-ctd
	  <char>-ctd				<keyword>-ctd			<symbol>-ctd
	  <procedure>-ctd			<struct>-ctd			<record>-ctd
	  <positive-fixnum>-ctd			<negative-fixnum>-ctd		<zero-fixnum>-ctd     <fixnum>-ctd
	  <positive-flonum>-ctd			<negative-flonum>-ctd
	  <positive-zero-flonum>-ctd		<negative-zero-flonum>-ctd	<flonum>-ctd
	  <positive-ratnum>-ctd			<negative-ratnum>-ctd		<ratnum>-ctd
	  <positive-bignum>-ctd			<negative-bignum>-ctd		<bignum>-ctd
	  <exact-compnum>-ctd			<zero-compnum>-ctd		<non-zero-inexact-compnum>-ctd
	  <zero-cflonum>-ctd			<non-zero-cflonum>-ctd
	  <empty-string>-ctd			<nestring>-ctd
	  <null>-ctd				<nelist>-ctd			<list>-ctd	      <pair>-ctd
	  <empty-vector>-ctd			<nevector>-ctd			<vector>-ctd
	  <empty-bytevector>-ctd		<nebytevector>-ctd
	  <condition>-ctd			<compound-condition>-ctd
	  <hashtable>-ctd)
    (only (ikarus records procedural)
	  $rtd-subtype?)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Fri Jun 3,
    ;;2016)
    (only (ikarus ratnums)
	  ratnum-positive?
	  ratnum-negative?)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Fri Jun 3,
    ;;2016)
    (only (ikarus numerics complex-numbers)
	  exact-compnum?
	  zero-compnum?
	  zero-cflonum?))


;;;; helpers

(define (%list-of-single-item? obj)
  (and (pair? obj)
       (null? (cdr obj))))

(define (simple-condition-type-descr? des)
  (and (record-type-descriptor? des)
       ($simple-condition-type-descr? des)))

(define ($simple-condition-type-descr? des)
  ($rtd-subtype? des (record-type-descriptor &condition)))

(define (core-type-descriptor.super-and-sub? super.des sub.des)
  (memq (car (core-type-descriptor.uids-list super.des))
	(core-type-descriptor.uids-list sub.des)))

;;; --------------------------------------------------------------------

(define (object-type-descr.list-type-descr? object.des)
  (or (<list>-ctd? object.des)
      (<null>-ctd? object.des)
      (<nelist>-ctd? object.des)
      (list-type-descr? object.des)
      (list-of-type-descr? object.des)
      (and (pair-type-descr? object.des)
	   (object-type-descr.list-type-descr? (pair-type-descr.cdr-des object.des)))
      (and (pair-of-type-descr? object.des)
	   (object-type-descr.list-type-descr? (pair-of-type-descr.item-des object.des)))))

(define (pair-type-descr?/list des)
  (and (pair-type-descr? des)
       (object-type-descr.list-type-descr? (pair-type-descr.cdr-des des))))

;;; --------------------------------------------------------------------

(define-syntax cond-with-predicates
  (syntax-rules (else)
    ((_ ?expr (?pred . ?body) ... (else . ?else-body))
     (let ((X ?expr))
       (cond ((?pred X) . ?body)
	     ...
	     (else . ?else-body))))
    ((_ ?expr (?pred . ?body) ...)
     (let ((X ?expr))
       (cond ((?pred X) . ?body)
	     ...)))
    ))

(define-syntax or-with-predicates
  (syntax-rules ()
    ((_ ?expr ?pred ...)
     (let ((X ?expr))
       (or (?pred X) ...)))
    ))

(define-syntax define-type-descriptor-predicate
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?descr)
       (identifier? #'?descr)
       (with-syntax ((WHO (identifier-suffix #'?descr "?")))
	 #'(define (WHO obj)
	     (and (core-type-descriptor? obj)
		  (eq? (car (core-type-descriptor.uids-list obj))
		       (car (core-type-descriptor.uids-list ?descr)))))))
      )))

(define-syntax case-descriptor
  (syntax-rules (else
		 core-type-descriptor? record-type-descriptor? struct-type-descriptor?
		 list-type-descr? list-of-type-descr? vector-type-descr? vector-of-type-descr?
		 pair-type-descr? pair-of-type-descr? compound-condition-type-descr? enumeration-type-descr?
		 closure-type-descr?
		 hashtable-type-descr? alist-type-descr? union-type-descr? intersection-type-descr? complement-type-descr?
		 ancestor-of-type-descr?)
    ((_ ?expr
	(core-type-descriptor?		. ?body-core)
	(record-type-descriptor?	. ?body-record)
	(struct-type-descriptor?	. ?body-struct)
	(list-type-descr?		. ?body-list)
	(list-of-type-descr?		. ?body-list-of)
	(vector-type-descr?		. ?body-vector)
	(vector-of-type-descr?		. ?body-vector-of)
	(pair-type-descr?		. ?body-pair)
	(pair-of-type-descr?		. ?body-pair-of)
	(compound-condition-type-descr?	. ?body-compound-condition)
	(enumeration-type-descr?	. ?body-enumeration)
	(closure-type-descr?		. ?body-closure)
	(hashtable-type-descr?		. ?body-hashtable)
	(alist-type-descr?		. ?body-alist)
	(union-type-descr?		. ?body-union)
	(intersection-type-descr?	. ?body-intersection)
	(complement-type-descr?		. ?body-complement)
	(ancestor-of-type-descr?	. ?body-ancestor-of)
	(else				. ?body-else))
     (let ((des ?expr))
       (cond
	((core-type-descriptor? des)		. ?body-core)
	((record-type-descriptor? des)		. ?body-record)
	((struct-type-descriptor? des)		. ?body-struct)
	((list-type-descr? des)			. ?body-list)
	((list-of-type-descr? des)		. ?body-list-of)
	((vector-type-descr? des)		. ?body-vector)
	((vector-of-type-descr? des)		. ?body-vector-of)
	((pair-type-descr? des)			. ?body-pair)
	((pair-of-type-descr? des)		. ?body-pair-of)
	((compound-condition-type-descr? des)	. ?body-compound-condition)
	((enumeration-type-descr? des)		. ?body-enumeration)
	((closure-type-descr? des)		. ?body-closure)
	((hashtable-type-descr? des)		. ?body-hashtable)
	((alist-type-descr? des)		. ?body-alist)
	((union-type-descr? des)		. ?body-union)
	((intersection-type-descr? des)		. ?body-intersection)
	((complement-type-descr? des)		. ?body-complement)
	((ancestor-of-type-descr? des)		. ?body-ancestor-of)
	(else					. ?body-else))))
    ))

;;; --------------------------------------------------------------------

(define-type-descriptor-predicate <no-return>-ctd)
(define-type-descriptor-predicate <void>-ctd)
(define-type-descriptor-predicate <top>-ctd)
(define-type-descriptor-predicate <symbol>-ctd)

(define-type-descriptor-predicate <list>-ctd)
(define-type-descriptor-predicate <nelist>-ctd)
(define-type-descriptor-predicate <null>-ctd)

(define-type-descriptor-predicate <vector>-ctd)
(define-type-descriptor-predicate <nevector>-ctd)
(define-type-descriptor-predicate <empty-vector>-ctd)

(define-type-descriptor-predicate <pair>-ctd)
(define-type-descriptor-predicate <procedure>-ctd)

(define-type-descriptor-predicate <struct>-ctd)
(define-type-descriptor-predicate <record>-ctd)

(define-type-descriptor-predicate <hashtable>-ctd)
(define-type-descriptor-predicate <condition>-ctd)
(define-type-descriptor-predicate <compound-condition>-ctd)


;;;; include files

(include "ikarus.object-type-descr.descriptors-signatures.sls"	#t)


;;;; compound type descriptors: compound condition-object types

(define-record-type (<compound-condition-type-descr> make-compound-condition-type-descr compound-condition-type-descr?)
  (nongenerative vicare:descriptors:<compound-condition-type-descr>)
  (sealed #t)
  (fields
    (immutable component-des*		compound-condition-type-descr.component-des*)
		;A list of instances of "<record-type-descr>" describing the types of
		;component condition objects.
    #| end of FIELDS |# )
  #| end of DEFINE-RECORD-TYPE |# )

(define <compound-condition-type-descr>-rtd
  (record-type-descriptor <compound-condition-type-descr>))

(define <compound-condition-type-descr>-rcd
  (record-constructor-descriptor <compound-condition-type-descr>))

;;; --------------------------------------------------------------------

(define* (compound-condition-type-descr.exists {compound-condition.des compound-condition-type-descr?} {proc procedure?})
  (exists proc (compound-condition-type-descr.component-des* compound-condition.des)))

(define* (compound-condition-type-descr.for-all {compound-condition.des compound-condition-type-descr?} {proc procedure?})
  (for-all proc (compound-condition-type-descr.component-des* compound-condition.des)))


;;;; compound type descriptors: symbols enumeration

(define-record-type (<enumeration-type-descr> make-enumeration-type-descr enumeration-type-descr?)
  (nongenerative vicare:descriptors:<enumeration-type-descr>)
  (sealed #t)
  (fields
    (immutable symbol*	enumeration-type-descr.symbol*)
    (mutable memoised-length)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (lambda (symbol*)
	(make-record symbol* #f)))))

(define <enumeration-type-descr>-rtd
  (record-type-descriptor <enumeration-type-descr>))

(define <enumeration-type-descr>-rcd
  (record-constructor-descriptor <enumeration-type-descr>))

;;; --------------------------------------------------------------------

(define* (enumeration-type-descr.member? {des enumeration-type-descr?} {sym symbol?})
  (memq sym (enumeration-type-descr.symbol* des)))

(define* (enumeration-type-descr.length {des enumeration-type-descr?})
  (or (<enumeration-type-descr>-memoised-length des)
      (receive-and-return (len)
	  (length (enumeration-type-descr.symbol* des))
	(<enumeration-type-descr>-memoised-length-set! des len))))


;;;; compound type descriptors: closure

(define-record-type (<closure-type-descr> make-closure-type-descr closure-type-descr?)
  (nongenerative vicare:descriptors:<closure-type-descr>)
  (sealed #t)
  (fields
    (immutable signature	closure-type-descr.signature)
		;An  instance of  "<case-lambda-descriptors>"  representing the  type
		;signatures of the clauses.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (lambda* ({signature case-lambda-descriptors?})
	(make-record signature)))))

(define <closure-type-descr>-rtd
  (record-type-descriptor <closure-type-descr>))

(define <closure-type-descr>-rcd
  (record-constructor-descriptor <closure-type-descr>))

;;; --------------------------------------------------------------------

(define* (closure-type-descr=? {D1 closure-type-descr?} {D2 closure-type-descr?})
  (case-lambda-descriptors=? (closure-type-descr.signature D1)
			     (closure-type-descr.signature D2)))

(define* (closure-type-descr.match-super-and-sub {D1 closure-type-descr?} {D2 closure-type-descr?})
  (case-lambda-descriptors.match-super-and-sub (closure-type-descr.signature D1)
					       (closure-type-descr.signature D2)))


;;;; compound type descriptors: hashtables

(define-record-type (<hashtable-type-descr> make-hashtable-type-descr hashtable-type-descr?)
  (nongenerative vicare:descriptors:<hashtable-type-descr>)
  (sealed #t)
  (fields
    (immutable key-des			hashtable-type-descr.key-des)
		;An instance of "<object-type-descr>" describing the type of keys.
    (immutable val-des			hashtable-type-descr.val-des)
		;An instance of "<object-type-descr>" describing the type of values.
    #| end of FIELDS |# ))

(define <hashtable-type-descr>-rtd
  (record-type-descriptor <hashtable-type-descr>))

(define <hashtable-type-descr>-rcd
  (record-constructor-descriptor <hashtable-type-descr>))


;;;; compound type descriptors: alists

(define-record-type (<alist-type-descr> make-alist-type-descr alist-type-descr?)
  (nongenerative vicare:descriptors:<alist-type-descr>)
  (sealed #t)
  (fields
    (immutable key-des			alist-type-descr.key-des)
		;An instance of "<object-type-descr>" describing the type of keys.
    (immutable val-des			alist-type-descr.val-des)
		;An instance of "<object-type-descr>" describing the type of values.
    #| end of FIELDS |# ))

(define <alist-type-descr>-rtd
  (record-type-descriptor <alist-type-descr>))

(define <alist-type-descr>-rcd
  (record-constructor-descriptor <alist-type-descr>))


;;;; compound type descriptors: pairs

(define-record-type (<pair-type-descr> make-pair-type-descr pair-type-descr?)
  (nongenerative vicare:descriptors:<pair-type-descr>)
  (sealed #t)
  (fields
    (immutable car-des		pair-type-descr.car-des)
    (immutable cdr-des		pair-type-descr.cdr-des)
    #| end of FIELDS |# ))

(define <pair-type-descr>-rtd
  (record-type-descriptor <pair-type-descr>))

(define <pair-type-descr>-rcd
  (record-constructor-descriptor <pair-type-descr>))


;;;; compound type descriptors: pairs of

(define-record-type (<pair-of-type-descr> make-pair-of-type-descr pair-of-type-descr?)
  (nongenerative vicare:descriptors:<pair-of-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des		pair-of-type-descr.item-des)
    #| end of FIELDS |# ))

(define <pair-of-type-descr>-rtd
  (record-type-descriptor <pair-of-type-descr>))

(define <pair-of-type-descr>-rcd
  (record-constructor-descriptor <pair-of-type-descr>))


;;;; compound type descriptors: lists

(define-record-type (<list-type-descr> %make-list-type-descr list-type-descr?)
  (nongenerative vicare:descriptors:<list-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des*		list-type-descr.item-des*)
    (mutable memoised-length)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (lambda (item*.des)
	(make-record item*.des #f)))))

(define* (make-list-type-descr {item*.des list?})
  (if (null? item*.des)
      <null>-ctd
    (%make-list-type-descr item*.des)))

(define <list-type-descr>-rtd
  (record-type-descriptor <list-type-descr>))

(define <list-type-descr>-rcd
  (record-constructor-descriptor <list-type-descr>))

;;; --------------------------------------------------------------------

(define* (list-type-descr.length {des list-type-descr?})
  (or (<list-type-descr>-memoised-length des)
      (receive-and-return (len)
	  (length (list-type-descr.item-des* des))
	(<list-type-descr>-memoised-length-set! des len))))

(define* (list-type-descr.list-of-single-item? {list.des list-type-descr?})
  (= 1 (list-type-descr.length list.des)))

(define* (list-type-descr.exists {des list-type-descr?} {proc procedure?})
  (exists proc (list-type-descr.item-des* des)))

(define* (list-type-descr.for-all {des list-type-descr?} {proc procedure?})
  (for-all proc (list-type-descr.item-des* des)))


;;;; compound type descriptors: lists of

(define-record-type (<list-of-type-descr> make-list-of-type-descr list-of-type-descr?)
  (nongenerative vicare:descriptors:<list-of-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des		list-of-type-descr.item-des)
    #| end of FIELDS |# ))

(define <list-of-type-descr>-rtd
  (record-type-descriptor <list-of-type-descr>))

(define <list-of-type-descr>-rcd
  (record-constructor-descriptor <list-of-type-descr>))


;;;; compound type descriptors: vectors

(define-record-type (<vector-type-descr> %make-vector-type-descr vector-type-descr?)
  (nongenerative vicare:descriptors:<vector-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des*		vector-type-descr.item-des*)
    (mutable memoised-length)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (lambda (item*.des)
	(make-record item*.des #f)))))

(define (make-vector-type-descr item*.des)
  (if (null? item*.des)
      <empty-vector>-ctd
    (%make-vector-type-descr item*.des)))

(define <vector-type-descr>-rtd
  (record-type-descriptor <vector-type-descr>))

(define <vector-type-descr>-rcd
  (record-constructor-descriptor <vector-type-descr>))

;;; --------------------------------------------------------------------

(define* (vector-type-descr.length {des vector-type-descr?})
  (or (<vector-type-descr>-memoised-length des)
      (receive-and-return (len)
	  (length (vector-type-descr.item-des* des))
	(<vector-type-descr>-memoised-length-set! des len))))

(define* (vector-type-descr.exists {des vector-type-descr?} {proc procedure?})
  (exists proc (vector-type-descr.item-des* des)))

(define* (vector-type-descr.for-all {des vector-type-descr?} {proc procedure?})
  (for-all proc (vector-type-descr.item-des* des)))


;;;; compound type descriptors: vectors of

(define-record-type (<vector-of-type-descr> make-vector-of-type-descr vector-of-type-descr?)
  (nongenerative vicare:descriptors:<vector-of-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des		vector-of-type-descr.item-des)
    #| end of FIELDS |# ))

(define <vector-of-type-descr>-rtd
  (record-type-descriptor <vector-of-type-descr>))

(define <vector-of-type-descr>-rcd
  (record-constructor-descriptor <vector-of-type-descr>))


;;;; compound type descriptors: union

(define-record-type (<union-type-descr> %make-union-type-descr union-type-descr?)
  (nongenerative vicare:descriptors:<union-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des*	union-type-descr.item-des*)
    #| end of FIELDS |# ))

(define (make-union-type-descr item*.des)
  (if (%list-of-single-item? item*.des)
      (car item*.des)
    (%make-union-type-descr item*.des)))

(define <union-type-descr>-rtd
  (record-type-descriptor <union-type-descr>))

(define <union-type-descr>-rcd
  (record-constructor-descriptor <union-type-descr>))

;;; --------------------------------------------------------------------

(define (union-type-descr.exists union.des proc)
  (exists proc (union-type-descr.item-des* union.des)))

(define (union-type-descr.for-all union.des proc)
  (for-all proc (union-type-descr.item-des* union.des)))


;;;; compound type descriptors: intersection

(define-record-type (<intersection-type-descr> %make-intersection-type-descr intersection-type-descr?)
  (nongenerative vicare:descriptors:<intersection-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des*	intersection-type-descr.item-des*)
    #| end of FIELDS |# ))

(define (make-intersection-type-descr item*.des)
  (if (%list-of-single-item? item*.des)
      (car item*.des)
    (%make-intersection-type-descr item*.des)))

(define <intersection-type-descr>-rtd
  (record-type-descriptor <intersection-type-descr>))

(define <intersection-type-descr>-rcd
  (record-constructor-descriptor <intersection-type-descr>))

;;; --------------------------------------------------------------------

(define (intersection-type-descr.exists intersection.des proc)
  (exists proc (intersection-type-descr.item-des* intersection.des)))

(define (intersection-type-descr.for-all intersection.des proc)
  (for-all proc (intersection-type-descr.item-des* intersection.des)))


;;;; compound type descriptors: complement

(define-record-type (<complement-type-descr> make-complement-type-descr complement-type-descr?)
  (nongenerative vicare:descriptors:<complement-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des		complement-type-descr.item-des)
    #| end of FIELDS |# ))

(define <complement-type-descr>-rtd
  (record-type-descriptor <complement-type-descr>))

(define <complement-type-descr>-rcd
  (record-constructor-descriptor <complement-type-descr>))

;;; --------------------------------------------------------------------

(define* (complement-type-descr.not {complement.des complement-type-descr?} {proc procedure?})
  (not (proc (complement-type-descr.item-des complement.des))))


;;;; compound type descriptors: ancestor-of

(define-record-type (<ancestor-of-type-descr> make-ancestor-of-type-descr ancestor-of-type-descr?)
  (nongenerative vicare:descriptors:<ancestor-of-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des		ancestor-of-type-descr.item-des)
    (immutable ancestor-des*	ancestor-of-type-descr.ancestor-des*)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (lambda (object.des)
	(make-record object.des (object-type-descr.ancestor-des* object.des))))))

(define <ancestor-of-type-descr>-rtd
  (record-type-descriptor <ancestor-of-type-descr>))

(define <ancestor-of-type-descr>-rcd
  (record-constructor-descriptor <ancestor-of-type-descr>))

;;; --------------------------------------------------------------------

(define* (ancestor-of-type-descr.exists {ancestor-of.des ancestor-of-type-descr?} {proc procedure?})
  (exists  proc (ancestor-of-type-descr.ancestor-des* ancestor-of.des)))

(define* (ancestor-of-type-descr.for-all {ancestor-of.des ancestor-of-type-descr?} {proc procedure?})
  ;;We want to return false if the item in the ANCESTOR-OF is "<top>".
  ;;
  (let ((ancestor*.des (ancestor-of-type-descr.ancestor-des* ancestor-of.des)))
    (and (pair? ancestor*.des)
	 (for-all proc ancestor*.des))))


;;;; object-type descriptors: parent

(define (object-type-descr.parent object.des)
  ;;Return false or an object-type descriptor which is the parent of OBJECT.DES.
  ;;
  (case-descriptor object.des
    (core-type-descriptor?			(core-type-descriptor.parent object.des))
    (record-type-descriptor?			(or (record-type-parent object.des) <record>-ctd))
    (struct-type-descriptor?			<struct>-ctd)
    (list-type-descr?				<nelist>-ctd)
    (list-of-type-descr?			<list>-ctd)
    (vector-type-descr?				<nevector>-ctd)
    (vector-of-type-descr?			<vector>-ctd)
    (pair-type-descr?				<pair>-ctd)
    (pair-of-type-descr?			<pair>-ctd)
    (compound-condition-type-descr?		<compound-condition>-ctd)
    (enumeration-type-descr?			<symbol>-ctd)
    (closure-type-descr?			<procedure>-ctd)
    (hashtable-type-descr?			<hashtable>-ctd)
    (alist-type-descr?				<list>-ctd)
    (union-type-descr?				<top>-ctd)
    (intersection-type-descr?			<top>-ctd)
    (complement-type-descr?			<top>-ctd)
    (ancestor-of-type-descr?			<top>-ctd)
    (else					<top>-ctd)))


;;;; object-type descriptors: ancestry

(define object-type-descr.ancestor-des*
  (let* ((TOP*				(list <top>-ctd))
	 (STRUCT-ANCESTORS		(cons <struct>-ctd TOP*))
	 (RECORD-ANCESTORS		(cons <record>-ctd STRUCT-ANCESTORS))
	 (SIMPLE-CONDITION-ANCESTORS	(cons <condition>-ctd RECORD-ANCESTORS))
	 (LIST-OF-ANCESTORS		(cons <list>-ctd TOP*))
	 (LIST-ANCESTORS		(cons <nelist>-ctd LIST-OF-ANCESTORS))
	 (VECTOR-OF-ANCESTORS		(cons <vector>-ctd TOP*))
	 (VECTOR-ANCESTORS		(cons <nevector>-ctd VECTOR-OF-ANCESTORS))
	 (PAIR-ANCESTORS		(cons <pair>-ctd TOP*))
	 (PAIR-OF-ANCESTORS		PAIR-ANCESTORS)
	 (COMPOUND-CONDITION-ANCESTORS	(cons <compound-condition>-ctd SIMPLE-CONDITION-ANCESTORS))
	 (ENUMERATION-ANCESTORS		(cons <symbol>-ctd TOP*))
	 (CLOSURE-ANCESTORS		(cons <procedure>-ctd TOP*))
	 (HASHTABLE-ANCESTORS		(cons <hashtable>-ctd STRUCT-ANCESTORS))
	 (ALIST-ANCESTORS		LIST-OF-ANCESTORS)
	 (UNION-ANCESTORS		TOP*)
	 (INTERSECTION-ANCESTORS	TOP*)
	 (COMPLEMENT-ANCESTORS		TOP*)
	 (ANCESTOR-OF-ANCESTORS		TOP*))
    (lambda (object.des)
      ;;Return a (possibly empty) list  of type descriptors representing the ancestry
      ;;of OBJECT.DES.  OBJECT.DES is *not* included in the list.
      ;;
      (case-descriptor object.des
	(core-type-descriptor?
	 (core-type-descriptor.ancestor-des* object.des))

	(record-type-descriptor?
	 (let recur ((obj.des object.des))
	   (cond ((record-type-parent obj.des)
		  => (lambda (parent.des)
		       (cons parent.des (recur parent.des))))
		 (else
		  (if ($simple-condition-type-descr? object.des)
		      SIMPLE-CONDITION-ANCESTORS
		    RECORD-ANCESTORS)))))

	(struct-type-descriptor?		STRUCT-ANCESTORS)
	(list-type-descr?			LIST-ANCESTORS)
	(list-of-type-descr?			LIST-OF-ANCESTORS)
	(vector-type-descr?			VECTOR-ANCESTORS)
	(vector-of-type-descr?			VECTOR-OF-ANCESTORS)
	(pair-type-descr?			PAIR-ANCESTORS)
	(pair-of-type-descr?			PAIR-OF-ANCESTORS)
	(compound-condition-type-descr?		COMPOUND-CONDITION-ANCESTORS)
	(enumeration-type-descr?		ENUMERATION-ANCESTORS)
	(closure-type-descr?			CLOSURE-ANCESTORS)
	(hashtable-type-descr?			HASHTABLE-ANCESTORS)
	(alist-type-descr?			ALIST-ANCESTORS)
	(union-type-descr?			UNION-ANCESTORS)
	(intersection-type-descr?		INTERSECTION-ANCESTORS)
	(complement-type-descr?			COMPLEMENT-ANCESTORS)
	(ancestor-of-type-descr?		ANCESTOR-OF-ANCESTORS)
	(else					TOP*)))))


;;;; object-type descriptors: parent and child

(define (object-type-descr.parent-and-child? super.des sub.des)
  ;;Recursive function.   Search SUPER.DES in  the hierarchy of SUB.DES:  return true
  ;;when found, false otherwise.
  ;;
  (cond ((eq? super.des sub.des))
	((core-type-descriptor? super.des)
	 (and (core-type-descriptor? sub.des)
	      (core-type-descriptor.super-and-sub? super.des sub.des)))
	((core-type-descriptor? sub.des)
	 #f)
	((object-type-descr=? super.des sub.des))
	((object-type-descr.parent sub.des)
	 => (lambda (sub-parent.des)
	      (object-type-descr.parent-and-child? super.des sub-parent.des)))
	(else #f)))


;;;; type descriptors: equality between super-types and sub-types

(define (object-type-descr=? super.des sub.des)
  (cond
   ((eq? super.des sub.des))

   ((core-type-descriptor? super.des)
    (and (core-type-descriptor? sub.des)
	 (core-type-descriptor=? super.des sub.des)))

   ((core-type-descriptor? sub.des)
    #f)

   ((record-type-descriptor? super.des)
    (and (record-type-descriptor? sub.des)
	 (eq? (record-type-uid super.des)
	      (record-type-uid   sub.des))))

   ((struct-type-descriptor? super.des)
    (and (struct-type-descriptor? sub.des)
	 (eq? (struct-type-symbol super.des)
	      (struct-type-symbol   sub.des))))

;;; --------------------------------------------------------------------

   ((pair-type-descr? super.des)
    (and (pair-type-descr? sub.des)
	 (object-type-descr=? (pair-type-descr.car-des super.des)
			      (pair-type-descr.car-des   sub.des))
	 (object-type-descr=? (pair-type-descr.cdr-des super.des)
			      (pair-type-descr.cdr-des   sub.des))))

   ((pair-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" or "<pair>" has been handled above.
    #f)

;;; --------------------------------------------------------------------

   ((pair-of-type-descr? super.des)
    (and (pair-of-type-descr? sub.des)
	 (object-type-descr=? (pair-of-type-descr.item-des super.des)
			      (pair-of-type-descr.item-des   sub.des))))

   ((pair-of-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" or "<pair>" has been handled above.
    #f)

;;; --------------------------------------------------------------------

   ((list-type-descr? super.des)
    (and (list-type-descr? sub.des)
	 (= (list-type-descr.length super.des)
	    (list-type-descr.length   sub.des))
	 (for-all object-type-descr=?
	   (list-type-descr.item-des* super.des)
	   (list-type-descr.item-des*   sub.des))))

   ((list-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((list-of-type-descr? super.des)
    (and (list-of-type-descr? sub.des)
	 (object-type-descr=? (list-of-type-descr.item-des super.des)
			      (list-of-type-descr.item-des   sub.des))))

   ((list-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((vector-type-descr? super.des)
    (and (vector-type-descr? sub.des)
	 (= (vector-type-descr.length super.des)
	    (vector-type-descr.length   sub.des))
	 (for-all object-type-descr=?
	   (vector-type-descr.item-des* super.des)
	   (vector-type-descr.item-des*   sub.des))))

   ((vector-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((vector-of-type-descr? super.des)
    (and (vector-of-type-descr? sub.des)
	 (object-type-descr=? (vector-of-type-descr.item-des super.des)
			      (vector-of-type-descr.item-des   sub.des))))

   ((vector-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((compound-condition-type-descr? super.des)
    (let ((super-component*.des (compound-condition-type-descr.component-des* super.des))
	  (sub-component*.des   (compound-condition-type-descr.component-des*   sub.des)))
      (and (for-all (lambda (super-component.des)
		      (exists (lambda (sub-component.des)
				(object-type-descr=? super-component.des sub-component.des))
			sub-component*.des))
	     super-component*.des)
	   (for-all (lambda (super-component.des)
		      (exists (lambda (sub-component.des)
				(object-type-descr=? super-component.des sub-component.des))
			sub-component*.des))
	     super-component*.des))))

   ((compound-condition-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((enumeration-type-descr? super.des)
    (and (enumeration-type-descr? sub.des)
	 (= (enumeration-type-descr.length super.des)
	    (enumeration-type-descr.length   sub.des))
	 (let ((super*.sym	(enumeration-type-descr.symbol* super.des))
	       (sub*.sym	(enumeration-type-descr.symbol* sub.des)))
	   (and (for-all (lambda (super.sym)
			   (memq super.sym sub*.sym))
		  super*.sym)
		(for-all (lambda (sub.sym)
			   (memq sub.sym super*.sym))
		  sub*.sym)))))

   ((enumeration-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((closure-type-descr? super.des)
    (cond ((closure-type-descr? sub.des)
	   (closure-type-descr=? super.des sub.des))
	  (else #f)))

   ((closure-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((hashtable-type-descr? super.des)
    (cond ((hashtable-type-descr? sub.des)
	   (and (object-type-descr=? (hashtable-type-descr.key-des super.des)
				     (hashtable-type-descr.key-des sub.des))
		(object-type-descr=? (hashtable-type-descr.val-des super.des)
				     (hashtable-type-descr.val-des sub.des))))
	  (else #f)))

   ((hashtable-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((alist-type-descr? super.des)
    (cond ((alist-type-descr? sub.des)
	   (and (object-type-descr=? (alist-type-descr.key-des super.des)
				     (alist-type-descr.key-des sub.des))
		(object-type-descr=? (alist-type-descr.val-des super.des)
				     (alist-type-descr.val-des sub.des))))
	  (else #f)))

   ((alist-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((union-type-descr? super.des)
    (and (union-type-descr? sub.des)
	 (for-all (lambda (super-item.des)
		    (exists (lambda (sub-item.des)
			      (object-type-descr=? super-item.des sub-item.des))
		      (union-type-descr.item-des* sub.des)))
	   (union-type-descr.item-des* super.des))))

   ((union-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((intersection-type-descr? super.des)
    (and (intersection-type-descr? sub.des)
	 (for-all (lambda (super-item.des)
		    (exists (lambda (sub-item.des)
			      (object-type-descr=? super-item.des sub-item.des))
		      (intersection-type-descr.item-des* sub.des)))
	   (intersection-type-descr.item-des* super.des))))

   ((intersection-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((complement-type-descr? super.des)
    (and (complement-type-descr? sub.des)
	 (object-type-descr=? (complement-type-descr.item-des super.des)
			      (complement-type-descr.item-des   sub.des))))

   ((complement-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((ancestor-of-type-descr? super.des)
    (cond ((ancestor-of-type-descr? sub.des)
	   (object-type-descr=? (ancestor-of-type-descr.item-des super.des)
				(ancestor-of-type-descr.item-des   sub.des)))
	  (else #f)))

   ((ancestor-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   (else #f)))


;;;; type descriptors: matching super-types and sub-types

(module (object-type-descr.matching-super-and-sub?)

  (define-syntax-rule (super-and-sub? A B)
    (object-type-descr.matching-super-and-sub? A B))

  (define* (object-type-descr.matching-super-and-sub? super.des sub.des)
    (cond
     ((eq? super.des sub.des))
     ((core-type-descriptor? sub.des)		(%matching-sub/core-type-descr   super.des sub.des))
     (else
      (case-descriptor super.des
	(core-type-descriptor?			(%matching-super/core-type-descr super.des sub.des))
	(record-type-descriptor?		(%matching-super/record-type-descriptor	      super.des sub.des))
	(struct-type-descriptor?		(%matching-super/struct-type-descriptor	      super.des sub.des))
	(list-type-descr?			(%matching-super/list-type-descriptor	      super.des sub.des))
	(list-of-type-descr?			(%matching-super/list-of-type-descriptor      super.des sub.des))
	(vector-type-descr?			(%matching-super/vector-type-descriptor	      super.des sub.des))
	(vector-of-type-descr?			(%matching-super/vector-of-type-descriptor    super.des sub.des))
	(pair-type-descr?			(%matching-super/pair-type-descriptor	      super.des sub.des))
	(pair-of-type-descr?			(%matching-super/pair-of-type-descriptor      super.des sub.des))
	(compound-condition-type-descr?		(%matching-super/compound-condition-type-descriptor super.des sub.des))
	(enumeration-type-descr?		(%matching-super/enumeration-type-descriptor  super.des sub.des))
	(closure-type-descr?			(%matching-super/closure-type-descriptor      super.des sub.des))
	(hashtable-type-descr?			(%matching-super/hashtable-type-descriptor    super.des sub.des))
	(alist-type-descr?			(%matching-super/alist-type-descriptor	      super.des sub.des))
	(union-type-descr?			(%matching-super/union-type-descriptor	      super.des sub.des))
	(intersection-type-descr?		(%matching-super/intersection-type-descriptor super.des sub.des))
	(complement-type-descr?			(%matching-super/complement-type-descriptor   super.des sub.des))
	(ancestor-of-type-descr?		(%matching-super/ancestor-of-type-descriptor  super.des sub.des))
	(else					#f)))))

;;; --------------------------------------------------------------------

  (module (%matching-sub/core-type-descr)

    (define (%matching-sub/core-type-descr super.des sub.des)
      (cond-with-predicates super.des
	(core-type-descriptor?
	 ;;Both  the descriptors  are instances  of "<core-type-descriptor>".   We do
	 ;;this case first because it is the most likely.
	 (cond ((<top>-ctd? super.des)
		(cond-with-predicates sub.des
		  (<void>-ctd?		#f)
		  (<no-return>-ctd?	#f)
		  (else			#t)))
	       ((core-type-descriptor=? super.des sub.des))
	       ((core-type-descriptor.super-and-sub? super.des sub.des))
	       ((<pair>-ctd? super.des)
		(<nelist>-ctd? sub.des))
	       (else #f)))

	(union-type-descr?
	 ;;(super-and-sub? (or <fixnum> <string>) <fixnum>)	=> #t
	 ;;(super-and-sub? (or <fixnum> <string>) <string>)	=> #t
	 (union-type-descr.exists super.des (lambda (super-item.des)
					      (super-and-sub? super-item.des sub.des))))

	(intersection-type-descr?
	 ;;(super-and-sub? (and <zero> <fixnum>) <zero-fixnum>) => #t
	 (intersection-type-descr.for-all super.des (lambda (super-item.des)
						      (super-and-sub? super-item.des sub.des))))

	(complement-type-descr?
	 (%matching/super-complement/sub-core super.des sub.des))

	(ancestor-of-type-descr?
	 ;;(super-and-sub? (ancestor-of <fixnum>) <exact-integer>) => #t
	 (ancestor-of-type-descr.exists super.des (lambda (super-ancestor.des)
						    (object-type-descr=? super-ancestor.des sub.des))))

	;;Special case: (super-and-sub? (list <top>) <pair>)
	(list-type-descr?
	 (and (<pair>-ctd? sub.des)
	      (= 1 (list-type-descr.length super.des))
	      (<top>-ctd? (car (list-type-descr.item-des* super.des)))))

	;;Special case: (super-and-sub? (list-of <top>) <list>)
	;;Special case: (super-and-sub? (list-of <top>) <nelist>)
	;;Special case: (super-and-sub? (list-of ?type) <null>)
	(list-of-type-descr?
	 (or (and (or-with-predicates sub.des <list>-ctd? <nelist>-ctd?)
		  (<top>-ctd? (list-of-type-descr.item-des super.des)))
	     (<null>-ctd? sub.des)))

	;;Special case: (super-and-sub? (vector-of <top>) <vector>)
	;;Special case: (super-and-sub? (vector-of <top>) <nevector>)
	;;Special case: (super-and-sub? (vector-of ?type) <empty-vector>)
	(vector-of-type-descr?
	 (or (and (or-with-predicates sub.des <vector>-ctd? <nevector>-ctd?)
		  (<top>-ctd? (vector-of-type-descr.item-des super.des)))
	     (<empty-vector>-ctd? sub.des)))

	;;Special case: (super-and-sub? (pair <top> <top>) <pair>)
	;;Special case: (super-and-sub? (pair <top> <list>) <nelist>)
	;;Special case: (super-and-sub? (pair <top> <list>) <list>)
	(pair-type-descr?
	 (or (and (<pair>-ctd? sub.des)
		  (<top>-ctd? (pair-type-descr.car-des super.des))
		  (<top>-ctd? (pair-type-descr.cdr-des super.des)))
	     (and (or-with-predicates sub.des <nelist>-ctd? <list>-ctd?)
		  (<top>-ctd?  (pair-type-descr.car-des super.des))
		  (<list>-ctd? (pair-type-descr.cdr-des super.des)))))

	;;Special case: (super-and-sub? (pair-of <top>) <pair>)
	(pair-of-type-descr?
	 (and (<pair>-ctd? sub.des)
	      (<top>-ctd? (pair-of-type-descr.item-des super.des))))

	(alist-type-descr?
	 ;;(super-and-sub? (alist <top> <top>) <null>)	=> #t
	 (<null>-ctd? sub.des))

	(else #f)))

    (define (%matching/super-complement/sub-core super.des sub.des)
      (let ((super-item.des (complement-type-descr.item-des super.des)))
	(cond-with-predicates super-item.des
	  ;;(super-and-sub? (not <top>) <fixnum>)		=> #f
	  ;;(super-and-sub? (not <top>) <top>)			=> #f
	  ;;(super-and-sub? (not <top>) <void>)			=> #f
	  ;;(super-and-sub? (not <top>) <no-return>)		=> #f
	  ;;(matching (not <top>) <fixnum>)			=> possible-match
	  ;;(matching (not <top>) <top>)			=> no-match
	  ;;(matching (not <top>) <void>)			=> possible-match
	  ;;(matching (not <top>) <no-return>)			=> possible-match
	  (<top>-ctd?			#f)

	  ;;(super-and-sub? (not <void>) <fixnum>)		=> #f
	  ;;(super-and-sub? (not <void>) <top>)			=> #f
	  ;;(super-and-sub? (not <void>) <void>			=> #f
	  ;;(super-and-sub? (not <void>) <no-return>)		=> #f
	  ;;(matching (not <void>) <fixnum>)			=> possible-match
	  ;;(matching (not <void>) <top>)			=> possible-match
	  ;;(matching (not <void>) <void>)			=> no-match
	  ;;(matching (not <void>) <no-return>)			=> possible-match
	  (<void>-ctd?			#f)

	  ;;(super-and-sub? (not <no-return>) <fixnum>)		=> #f
	  ;;(super-and-sub? (not <no-return>) <top>)		=> #f
	  ;;(super-and-sub? (not <no-return>) <void>)		=> #f
	  ;;(super-and-sub? (not <no-return>) <no-return>)	=> #f
	  ;;(matching (not <no-return>) <fixnum>)		=> possible-match
	  ;;(matching (not <no-return>) <top>)			=> possible-match
	  ;;(matching (not <no-return>) <void>)			=> possible-match
	  ;;(matching (not <no-return>) <no-return>)		=> no-match
	  (<no-return>-ctd?		#f)

	  (ancestor-of-type-descr?
	   ;;(super-and-sub? (not (ancestor-of <fixnum>)) <top>)		=> #f
	   ;;(super-and-sub? (not (ancestor-of <fixnum>)) <positive-fixnum>)	=> #t
	   ;;(super-and-sub? (not (ancestor-of <fixnum>)) <flonum>)		=> #t
	   (ancestor-of-type-descr.for-all super-item.des (lambda (super-item-ancestor.des)
							    (not (object-type-descr=? super-item-ancestor.des sub.des)))))

	  (else
	   ;;For this  case we never  return true.  Matching  is never exact,  it can
	   ;;only be possible.
	   ;;
	   ;;(super-and-sub? (not <string>) <top>)		=> #f
	   ;;(super-and-sub? (not <fixnum>) <flonum>)		=> #f
	   ;;(super-and-sub? (not <struct>) <record>)		=> #f
	   ;;(matching (not <string>) <top>)			=> possible-match
	   ;;(matching (not <fixnum>) <flonum>)			=> possible-match
 	   ;;(matching (not <struct>) <record>)			=> possible-match
	   #f))))

    #| end of module: %MATCHING-SUB/CORE-TYPE-DESCR |# )

;;; --------------------------------------------------------------------

  (define* (%matching-super/core-type-descr super.des sub.des)
    ;;In   this  continuation   we   know   that:  SUPER.DES   is   an  instance   of
    ;;"<core-type-descriptor>";     SUB.DES    is     *not*     an    instance     of
    ;;"<core-type-descriptor>".
    ;;
    (cond-with-predicates super.des
      (<top>-ctd?
       (cond-with-predicates sub.des
	 (ancestor-of-type-descr?	(ancestor-of-type-descr.for-all sub.des (lambda (sub-ancestor.des)
										  (object-type-descr=? <top>-ctd sub-ancestor.des))))
	 (complement-type-descr?	#f)
	 (else				#t)))
      (<pair>-ctd?			(or-with-predicates sub.des pair-type-descr? pair-of-type-descr? list-type-descr?))
      (<list>-ctd?			(or-with-predicates sub.des
					  list-type-descr? list-of-type-descr? alist-type-descr? pair-type-descr?/list))
      (<nelist>-ctd?			(or-with-predicates sub.des list-type-descr? pair-type-descr?/list))
      (<vector>-ctd?			(or-with-predicates sub.des vector-type-descr? vector-of-type-descr?))
      (<nevector>-ctd?			(vector-type-descr? sub.des))
      (<symbol>-ctd?			(enumeration-type-descr? sub.des))
      (<hashtable>-ctd?			(hashtable-type-descr? sub.des))
      (<condition>-ctd?			(or-with-predicates sub.des compound-condition-type-descr? simple-condition-type-descr?))
      (<compound-condition>-ctd?	(or-with-predicates sub.des compound-condition-type-descr?))
      ;;The record-type case includes the condition object-type case.
      (<record>-ctd?			(or-with-predicates sub.des record-type-descriptor? compound-condition-type-descr?))
      ;;The record-type case includes the condition object-type case.
      (<struct>-ctd?			(or-with-predicates sub.des
					  record-type-descriptor? compound-condition-type-descr? hashtable-type-descr? struct-type-descriptor?))
      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/record-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      ;;This branch includes the case of SUPER.DES and SUB.DES being simple condition
      ;;object types.
      (record-type-descriptor?		($rtd-subtype? sub.des super.des))

      (compound-condition-type-descr?
       ;;(super-and-sub? &who (condition &who &irritants)) => #t
       (and (simple-condition-type-descr? super.des)
	    (compound-condition-type-descr.exists sub.des
						  (lambda (sub-item.des)
						    (super-and-sub? super.des sub-item.des)))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/struct-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (struct-type-descriptor?
       (eq? (struct-type-symbol super.des)
	    (struct-type-symbol sub.des)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/pair-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (pair-type-descr?
       (and (super-and-sub? (pair-type-descr.car-des super.des) (pair-type-descr.car-des sub.des))
	    (super-and-sub? (pair-type-descr.cdr-des super.des) (pair-type-descr.cdr-des sub.des))))

      (pair-of-type-descr?
       (let ((sub-item.des (pair-of-type-descr.item-des sub.des)))
	 (and (super-and-sub? (pair-type-descr.car-des super.des) sub-item.des)
	      (super-and-sub? (pair-type-descr.cdr-des super.des) sub-item.des))))

      (list-type-descr?
       (let ((sub-item*.des (list-type-descr.item-des* sub.des)))
	 (and (super-and-sub? (pair-type-descr.car-des super.des) (car sub-item*.des))
	      (super-and-sub? (pair-type-descr.cdr-des super.des) (if (pair? (cdr sub-item*.des))
								      (make-list-type-descr (cdr sub-item*.des))
								    <null>-ctd)))))

      ;;No list-of because a list-of may be empty.

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/pair-of-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (pair-type-descr?
       (let ((super-item.des (pair-of-type-descr.item-des super.des)))
	 (and (super-and-sub? super-item.des (pair-type-descr.car-des sub.des))
	      (super-and-sub? super-item.des (pair-type-descr.cdr-des sub.des)))))

      (pair-of-type-descr?
       (super-and-sub? (pair-of-type-descr.item-des super.des)
		       (pair-of-type-descr.item-des sub.des)))

      (list-type-descr?
       (let ((super-item.des (pair-of-type-descr.item-des super.des)))
	 (list-type-descr.for-all sub.des (lambda (sub-item.des)
					    (super-and-sub? super-item.des sub-item.des)))))

      (list-of-type-descr?
       ;;(super-and-sub? (pair-of <fixnum>) (list-of <fixnum>))	=> #f
       (let ((super-item.des (pair-of-type-descr.item-des super.des)))
	 (and
	  ;;for the car
	  (super-and-sub? super-item.des (list-of-type-descr.item-des sub.des))
	  ;;for the cdr
	  (super-and-sub? super-item.des sub.des))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/list-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (pair-type-descr?
       (let ((super-item*.des (list-type-descr.item-des* super.des)))
	 (and (super-and-sub? (car super-item*.des) (pair-type-descr.car-des sub.des))
	      (super-and-sub? (make-list-type-descr (cdr super-item*.des))
			      (pair-type-descr.cdr-des sub.des)))))

      (pair-of-type-descr?
       (let ((super-item*.des (list-type-descr.item-des* super.des)))
	 (let ((sub-item.des (pair-of-type-descr.item-des sub.des)))
	   (and (super-and-sub? (car super-item*.des) sub-item.des)
		(super-and-sub? (if (pair? (cdr super-item*.des))
				    (make-list-type-descr (cdr super-item*.des))
				  <null>-ctd)
				sub-item.des)))))

      (list-type-descr?
       (and (= (list-type-descr.length super.des)
	       (list-type-descr.length sub.des))
	    (for-all object-type-descr.matching-super-and-sub?
	      (list-type-descr.item-des* super.des)
	      (list-type-descr.item-des* sub.des))))

      ;;No list-of because a list-of may be empty.

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/list-of-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (pair-type-descr?
       (let ((super-item.des (list-of-type-descr.item-des super.des)))
	 (and (super-and-sub? super-item.des (pair-type-descr.car-des sub.des))
	      (super-and-sub? super.des      (pair-type-descr.cdr-des sub.des)))))

      (pair-of-type-descr?
       (let ((super-item.des	(list-of-type-descr.item-des super.des))
	     (sub-item.des	(pair-of-type-descr.item-des sub.des)))
	 (and (super-and-sub? super-item.des sub-item.des)
	      (object-type-descr.list-type-descr? sub-item.des))))

      (list-type-descr?
       (let ((super-item.des (list-of-type-descr.item-des super.des)))
	 (list-type-descr.for-all sub.des (lambda (sub-item.des)
					    (super-and-sub? super-item.des sub-item.des)))))

      (list-of-type-descr?
       (super-and-sub? (list-of-type-descr.item-des super.des) (list-of-type-descr.item-des sub.des)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/vector-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (vector-type-descr?
       (let ((super-item*.des (vector-type-descr.item-des* super.des)))
	 (and (= (vector-type-descr.length super.des)
		 (vector-type-descr.length   sub.des))
	      (for-all (lambda (super-item.des sub-item.des)
			 (super-and-sub? super-item.des sub-item.des))
		super-item*.des
		(vector-type-descr.item-des* sub.des)))))

      ;;No vector-of because a vector-of may be empty.

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/vector-of-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (vector-type-descr?
       (let ((super-item.des (vector-of-type-descr.item-des super.des)))
	 (for-all (lambda (sub-item.des)
		    (super-and-sub? super-item.des sub-item.des))
	   (vector-type-descr.item-des* sub.des))))

      (vector-of-type-descr?
       (super-and-sub? (vector-of-type-descr.item-des super.des)
		       (vector-of-type-descr.item-des sub.des)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/compound-condition-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (compound-condition-type-descr?
       (let ((super-component*.des (compound-condition-type-descr.component-des* super.des))
	     (sub-component*.des   (compound-condition-type-descr.component-des*   sub.des)))
	 (for-all (lambda (super-component.des)
		    (exists (lambda (sub-component.des)
			      (object-type-descr.matching-super-and-sub? super-component.des sub-component.des))
		      sub-component*.des))
	   super-component*.des)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/enumeration-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (enumeration-type-descr?
       ;; (super-and-sub? (enumeration ciao hello) (enumeration ciao)) => #t
       (let ((super*.sym (enumeration-type-descr.symbol* super.des)))
	 (for-all (lambda (sub.sym)
		    (memq sub.sym super*.sym))
	   (enumeration-type-descr.symbol* sub.des))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/closure-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (closure-type-descr?
       (eq? 'exact-match (closure-type-descr.match-super-and-sub super.des sub.des)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/hashtable-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (hashtable-type-descr?
       (and (object-type-descr.matching-super-and-sub? (hashtable-type-descr.key-des super.des)
						       (hashtable-type-descr.key-des sub.des))
	    (object-type-descr.matching-super-and-sub? (hashtable-type-descr.val-des super.des)
						       (hashtable-type-descr.val-des sub.des))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/alist-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (alist-type-descr?
       (and (object-type-descr.matching-super-and-sub? (alist-type-descr.key-des super.des)
						       (alist-type-descr.key-des sub.des))
	    (object-type-descr.matching-super-and-sub? (alist-type-descr.val-des super.des)
						       (alist-type-descr.val-des sub.des))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%matching-super/union-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (union-type-descr?
       (let ((super-item*.des (union-type-descr.item-des* super.des))
	     (sub-item*.des   (union-type-descr.item-des* super.des)))
	 (for-all (lambda (super-item.des)
		    (exists (lambda (sub-item.des)
			      (object-type-descr.matching-super-and-sub? super-item.des sub-item.des))
		      sub-item*.des))
	   super-item*.des)))

      (else
       (union-type-descr.exists super.des (lambda (super-item.des)
					    (super-and-sub? super-item.des sub.des))))))

;;; --------------------------------------------------------------------

  (define (%matching-super/intersection-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (intersection-type-descr?
       (let ((super-item*.des (intersection-type-descr.item-des* super.des))
	     (sub-item*.des   (intersection-type-descr.item-des* super.des)))
	 (for-all (lambda (super-item.des)
		    (for-all (lambda (sub-item.des)
			       (super-and-sub? super-item.des sub-item.des))
		      sub-item*.des))
	   super-item*.des)))

      (else
       (intersection-type-descr.for-all super.des (lambda (super-item.des)
						    (super-and-sub? super-item.des sub.des))))))

;;; --------------------------------------------------------------------

  (define (%matching-super/complement-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (complement-type-descr?
       ;;(super-and-sub? (not <struct>) (not <record>)) => #t
       ;;(super-and-sub? (not <number>) (not <fixnum>)) => #t
       ;;(super-and-sub? (not <fixnum>) (not <number>)) => #f
       ;;(super-and-sub? (not <fixnum>) (not <string>)) => #f
       (super-and-sub? (complement-type-descr.item-des super.des)
		       (complement-type-descr.item-des sub.des)))

      (else
       ;;We must remember  that here SUB.DES is never a  core type descriptor.  Let's
       ;;assume the following definitions:
       ;;
       ;;   (define-record-type <duo>)
       ;;   (define-record-type <alpha>)
       ;;   (define-record-type <beta>  (parent <alpha>))
       ;;   (define-record-type <delta> (parent <beta>))
       ;;
       ;;then:
       ;;
       ;;   (matching (not <alpha>)	<duo>)		=> exact-match
       ;;   (matching (not <alpha>)	<alpha>)	=> no-match
       ;;   (matching (not <alpha>)	<beta>)		=> no-match
       ;;   (matching (not <alpha>)	<delta>)	=> no-match
       ;;   (matching (not <beta>)	<duo>)		=> exact-match
       ;;   (matching (not <beta>)	<alpha>)	=> possible-match
       ;;   (matching (not <beta>)	<beta>)		=> no-match
       ;;   (matching (not <beta>)	<delta>)	=> no-match
       ;;   (matching (not <delta>)	<duo>)		=> exact-match
       ;;   (matching (not <delta>)	<alpha>)	=> possible-match
       ;;   (matching (not <delta>)	<beta>)		=> possible-match
       ;;   (matching (not <delta>)	<delta>)	=> no-match
       ;;
       (let ((super-item.des (complement-type-descr.item-des super.des)))
	 (cond ((super-and-sub? sub.des super-item.des)		#f)
	       ((super-and-sub? super-item.des sub.des)		#f)
	       (else						#t))))))

;;; --------------------------------------------------------------------

  (define (%matching-super/ancestor-of-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (ancestor-of-type-descr?
       ;;(define-record-type <alpha>)
       ;;(define-record-type <beta> (parent <alpha>))
       ;;
       ;;(super-and-sub? (ancestor-of <alpha>) (ancestor-of <alpha>))	=> #t
       ;;(super-and-sub? (ancestor-of <alpha>) (ancestor-of <beta>))	=> #f
       ;;(super-and-sub? (ancestor-of <beta>)  (ancestor-of <alpha>))	=> #t
       ;;(matching (ancestor-of <alpha>) (ancestor-of <alpha>))		=> exact-match
       ;;(matching (ancestor-of <alpha>) (ancestor-of <beta>))		=> possible-match
       ;;(matching (ancestor-of <beta>)  (ancestor-of <alpha>))		=> exact-match
       (object-type-descr.matching-super-and-sub? (ancestor-of-type-descr.item-des sub.des)
						  (ancestor-of-type-descr.item-des super.des)))

      (complement-type-descr?
       ;;(define-record-type <alpha>)
       ;;(define-record-type <beta> (parent <alpha>))
       ;;
       ;;(super-and-sub? (ancestor-of <alpha>) (not <alpha>))	=> #f
       ;;(super-and-sub? (ancestor-of <alpha>) (not <beta>))	=> #f
       ;;(super-and-sub? (ancestor-of <beta>)  (not <alpha>))	=> #f
       ;;(matching (ancestor-of <alpha>) (not <alpha>))	=> possible-match
       ;;(matching (ancestor-of <alpha>) (not <beta>))	=> possible-match
       ;;(matching (ancestor-of <beta>)  (not <alpha>))	=> possible-match
       #f)

      (else
       (ancestor-of-type-descr.exists super.des (lambda (ancestor-super.des)
						  (object-type-descr=? ancestor-super.des sub.des))))))

;;; --------------------------------------------------------------------

  (define (%matching-sub/union/intersection/complement/ancestor-of super.des sub.des)
    (cond-with-predicates sub.des
      (union-type-descr?
       (union-type-descr.for-all sub.des
				 (lambda (sub-item.des)
				   (super-and-sub? super.des sub-item.des))))

      (intersection-type-descr?
       (intersection-type-descr.for-all sub.des
					(lambda (sub-item.des)
					  (super-and-sub? super.des sub-item.des))))

      (complement-type-descr?
       #f)

      (ancestor-of-type-descr?
       (ancestor-of-type-descr.for-all sub.des
				       (lambda (sub-ancestor.des)
					 (object-type-descr=? super.des sub-ancestor.des))))

      (else #f)))

  #| end of module: OBJECT-TYPE-DESCR.MATCHING-SUPER-AND-SUB? |# )


;;;; type descriptors: compatible super-types and sub-types

(module (object-type-descr.compatible-super-and-sub?)

  (define (%matching-or-compatible? super.des sub.des)
    (or (object-type-descr.matching-super-and-sub?   super.des sub.des)
	(object-type-descr.compatible-super-and-sub? super.des sub.des)))

  (define (object-type-descr.compatible-super-and-sub? super.des sub.des)
    ;;This  function is  used to  check  for non-matching  compatibility between  two
    ;;object-type    descriptors.     It    is    meant    to    be    called    when
    ;;OBJECT-TYPE-DESCR.MATCHING-SUPER-AND-SUB? has already  returned #f when applied
    ;;to the same operands.
    ;;
    ;;As example:  a "<number>" argument  matches a "<fixnum>" operand;  a "<fixnum>"
    ;;argument is compatible with a "<number>" operand.
    ;;
    (cond
     ((core-type-descriptor? sub.des)		(%compatible-sub/core-type-descr   super.des sub.des))
     (else
      (case-descriptor super.des
	(core-type-descriptor?			(%compatible-super/core-type-descr super.des sub.des))
	(record-type-descriptor?		(%compatible-super/record-type-descriptor	super.des sub.des))
	(struct-type-descriptor?		(%compatible-super/struct-type-descriptor	super.des sub.des))
	(list-type-descr?			(%compatible-super/list-type-descriptor	        super.des sub.des))
	(list-of-type-descr?			(%compatible-super/list-of-type-descriptor      super.des sub.des))
	(vector-type-descr?			(%compatible-super/vector-type-descriptor	super.des sub.des))
	(vector-of-type-descr?			(%compatible-super/vector-of-type-descriptor    super.des sub.des))
	(pair-type-descr?			(%compatible-super/pair-type-descriptor		super.des sub.des))
	(pair-of-type-descr?			(%compatible-super/pair-of-type-descriptor      super.des sub.des))
	(compound-condition-type-descr?		(%compatible-super/compound-condition-type-descriptor super.des sub.des))
	(enumeration-type-descr?		(%compatible-super/enumeration-type-descriptor  super.des sub.des))
	(closure-type-descr?			(%compatible-super/closure-type-descriptor      super.des sub.des))
	(hashtable-type-descr?			(%compatible-super/hashtable-type-descriptor    super.des sub.des))
	(alist-type-descr?			(%compatible-super/alist-type-descriptor	super.des sub.des))
	(union-type-descr?			(%compatible-super/union-type-descriptor	super.des sub.des))
	(intersection-type-descr?		(%compatible-super/intersection-type-descriptor super.des sub.des))
	(complement-type-descr?			(%compatible-super/complement-type-descriptor   super.des sub.des))
	(ancestor-of-type-descr?		(%compatible-super/ancestor-of-type-descriptor  super.des sub.des))
	(else
	 (object-type-descr.matching-super-and-sub? sub.des super.des))))))

;;; --------------------------------------------------------------------

  (module (%compatible-sub/core-type-descr)

    (define (%compatible-sub/core-type-descr super.des sub.des)
      (cond-with-predicates super.des
	(core-type-descriptor?
	 ;;Both the descriptors are core.
	 (%compatible-super/core-type-descr super.des sub.des))

	(union-type-descr?
	 (union-type-descr.exists super.des (lambda (super-item.des)
					      (%matching-or-compatible? super-item.des sub.des))))
	(intersection-type-descr?
	 (intersection-type-descr.for-all super.des (lambda (super-item.des)
						      (%matching-or-compatible? super-item.des sub.des))))
	(complement-type-descr?
	 (%compatible/super-complement/sub-core super.des sub.des))

	(ancestor-of-type-descr?
	 ;;The ANCESTOR-OF super-type matches if one of the ancestors is equal to the
	 ;;sub-type.  There is no compatibility  condition, only exact matching or no
	 ;;matching.
	 #f)

	(else
	 (cond-with-predicates sub.des
	   (<top>-ctd?			#t)

	   (<list>-ctd?
	    ;;(matching (list <fixnum>)     <list>)		=> possible-match
	    ;;(matching (list-of <fixnum>)  <list>)		=> possible-match
	    ;;(matching (alist <top> <top>) <list>)		=> possible-match
	    (cond-with-predicates super.des
	      (list-type-descr?		#t)
	      (list-of-type-descr?	#t)
	      (alist-type-descr?	#t)
	      (else
	       (object-type-descr.matching-super-and-sub? sub.des super.des))))

	   (<nelist>-ctd?
	    ;;(matching (list <fixnum>)     <nelist>)		=> possible-match
	    ;;(matching (list-of <fixnum>)  <nelist>)		=> possible-match
	    ;;(matching (alist <top> <top>) <nelist>)		=> possible-match
	    (cond-with-predicates super.des
	      (list-type-descr?		#t)
	      (list-of-type-descr?	#t)
	      (alist-type-descr?	#t)
	      (else
	       (object-type-descr.matching-super-and-sub? sub.des super.des))))

	   (<null>-ctd?
	    ;;(matching (list <fixnum>)     <null>)		=> no-match
	    ;;(matching (list-of <fixnum>)  <null>)		=> possible-match
	    ;;(matching (alist <top> <top>) <null>)		=> possible-match
	    (or-with-predicates super.des
	      list-of-type-descr? alist-type-descr?))

	   (<pair>-ctd?
	    ;;(matching (list <fixnum>)     <pair>)		=> possible-match
	    ;;(matching (list-of <fixnum>)  <pair>)		=> possible-match
	    ;;(matching (alist <top> <top>) <pair>)		=> possible-match
	    (or-with-predicates super.des
	      list-type-descr? list-of-type-descr? alist-type-descr?))

	   (<vector>-ctd?
	    ;;(matching (vector <fixnum>)     <vector>)		=> possible-match
	    ;;(matching (vector-of <fixnum>)  <vector>)		=> possible-match
	    (or-with-predicates super.des
	      vector-type-descr? vector-of-type-descr?))

	   (<nevector>-ctd?
	    ;;(matching (vector <fixnum>)     <nevector>)	=> possible-match
	    ;;(matching (vector-of <fixnum>)  <nevector>)	=> possible-match
	    (or-with-predicates super.des
	      vector-type-descr? vector-of-type-descr?))

	   (<empty-vector>-ctd?
	    ;;(matching (vector <fixnum>)     <empty-vector>)	=> no-match
	    ;;(matching (vector-of <fixnum>)  <empty-vector>)	=> possible-match
	    (or-with-predicates super.des
	      vector-of-type-descr?))

	   (<struct>-ctd?
	    ;;(matching (hashtable <top> <top>) <struct>)	=> possible-match
	    (or-with-predicates super.des
	      record-type-descriptor? struct-type-descriptor? hashtable-type-descr?))

	   (else
	    (object-type-descr.matching-super-and-sub? sub.des super.des))))))

    (define (%compatible/super-complement/sub-core super.des sub.des)
      (let ((super-item.des (complement-type-descr.item-des super.des)))
	(cond-with-predicates sub.des
	  (<top>-ctd?
	   ;;(matching (not <fixnum>)    <top>)		=> possible-match
	   ;;(matching (not <top>)       <top>)		=> no-match
	   ;;(matching (not <void>)      <top>)		=> possible-match
	   ;;(matching (not <no-return>) <top>)		=> possible-match
	   (cond-with-predicates super-item.des
	     (<top>-ctd?		#f)
	     (<void>-ctd?		#t)
	     (<no-return>-ctd?		#t)
	     (else
	      (not (object-type-descr.matching-super-and-sub? super-item.des sub.des)))))

	  (<void>-ctd?
	   ;;(matching (not <fixnum>)    <void>)	=> possible-match
	   ;;(matching (not <top>)       <void>)	=> possible-match
	   ;;(matching (not <void>)      <void>)	=> no-match
	   ;;(matching (not <no-return>) <void>)	=> possible-match
	   (cond-with-predicates super-item.des
	     (<top>-ctd?		#t)
	     (<void>-ctd?		#f)
	     (<no-return>-ctd?		#t)
	     (else			#t)))

	  (<no-return>-ctd?
	   ;;(matching (not <fixnum>)    <no-return>)	=> possible-match
	   ;;(matching (not <top>)       <no-return>)	=> possible-match
	   ;;(matching (not <void>)      <no-return>)	=> possible-match
	   ;;(matching (not <no-return>) <no-return>)	=> possible-match
	   (cond-with-predicates super-item.des
	     (<top>-ctd?		#t)
	     (<void>-ctd?		#t)
	     (<no-return>-ctd?		#f)
	     (else			#t)))

	  (else
	   (cond ((object-type-descr=? super-item.des sub.des)
		  ;;(matching (not <fixnum>) <fixnum>)		=> no-match
		  #f)
		 ((object-type-descr.matching-super-and-sub? super-item.des sub.des)
		  ;;(matching (not <fixnum>) <positive-fixnum>) => no-match
		  #f)
		 ((object-type-descr.matching-super-and-sub? sub.des super-item.des)
		  ;;(matching (not <fixnum>) <number>)		=> possible-match
		  #t)
		 (else
		  ;;(matching (not <fixnum>) <flonum>)		=> possible-match
		  ;;(matching (not <flonum>) <fixnum>)		=> possible-match
		  #t))))))

    #| end of module: %COMPATIBLE-SUB/CORE-TYPE-DESCR |# )

;;; --------------------------------------------------------------------

  (module (%compatible-super/core-type-descr)

    (define (%compatible-super/core-type-descr super.des sub.des)
      ;;Here it is possible that both the descriptors are core.
      ;;
      (cond-with-predicates super.des
	(<list>-ctd?
	 ;;(matching <list> <pair>)			=> possible-match
	 ;;(matching <list> (pair <top> <list>))	=> possible-match
	 ;;(matching <list> (pair-of <list>))		=> possible-match
	 ;;(matching <list> <top>)			=> possible-match
	 (cond-with-predicates sub.des
	   (<pair>-ctd?		#t)
	   (pair-type-descr?
	    (object-type-descr.compatible-super-and-sub? <list>-ctd (pair-type-descr.cdr-des sub.des)))
	   (pair-of-type-descr?
	    (object-type-descr.compatible-super-and-sub? <list>-ctd (pair-of-type-descr.item-des sub.des)))
	   (else
	    (object-type-descr.matching-super-and-sub? sub.des super.des))))

	(<nelist>-ctd?
	 ;;(matching <nelist> <top>)			=> possible-match
	 ;;(matching <nelist> <list>)			=> possible-match
	 ;;(matching <nelist> <pair>)			=> possible-match
	 ;;(matching <nelist> (pair <top> <list>))	=> possible-match
	 ;;(matching <nelist> (pair-of <list>))		=> possible-match
	 ;;(matching <nelist> (list ...))		=> possible-match
	 ;;(matching <nelist> (list-of ...))		=> possible-match
	 (cond-with-predicates sub.des
	   (<list>-ctd?		#t)
	   (<pair>-ctd?		#t)
	   (pair-type-descr?
	    (object-type-descr.compatible-super-and-sub? <list>-ctd (pair-type-descr.cdr-des sub.des)))
	   (pair-of-type-descr?
	    (object-type-descr.compatible-super-and-sub? <list>-ctd (pair-of-type-descr.item-des sub.des)))
	   (list-of-type-descr?	#t)
	   (alist-type-descr?	#t)
	   (else
	    (object-type-descr.matching-super-and-sub? sub.des super.des))))

	(<null>-ctd?
	 ;;(matching <null> <list>)			=> possible-match
	 ;;(matching <null> (list-of <fixnum>))		=> possible-match
	 ;;(matching <null> (alist <top> <top>))	=> possible-match
	 (or-with-predicates sub.des <list>-ctd? list-of-type-descr? alist-type-descr?))

	(<nevector>-ctd?
	 ;;(matching <nevector> <vector>)		=> possible-match
	 ;;(matching <nevector> (vector-of <fixnum>))	=> possible-match
	 (or-with-predicates sub.des vector-of-type-descr? <vector>-ctd?))

	(<empty-vector>-ctd?
	 ;;(matching <empty-vector> <vector>)		=> possible-match
	 ;;(matching <empty-vector> (vector-of <fixnum>)) => possible-match
	 (or-with-predicates sub.des vector-of-type-descr? <vector>-ctd?))

	(<pair>-ctd?
	 ;;(matching <pair> <list>)			=> possible-match
	 ;;(matching <pair> (list-of <fixnum>))		=> possible-match
	 ;;(matching <pair> (alist <top> <top>))	=> possible-match
	 (or-with-predicates sub.des
	   <list>-ctd? list-of-type-descr? alist-type-descr?))

	(else
	 (cond-with-predicates sub.des
	   (union-type-descr?
	    ;;(matching <fixnum> (or <fixnum> <string>))	=> possible-match
	    (union-type-descr.exists sub.des (lambda (sub-item.des)
					       (object-type-descr.matching-super-and-sub? super.des sub-item.des))))
	   (intersection-type-descr?
	    ;;(matching <fixnum> (and <positive> <exact-integer>))
	    (intersection-type-descr.for-all sub.des (lambda (sub-item.des)
						       (object-type-descr.matching-super-and-sub? super.des sub-item.des))))
	   (complement-type-descr?
	    (%compatible/super-core/sub-complement super.des sub.des))
	   (else
	    ;;(matching <fixnum> <exact-integer>)
	    (object-type-descr.matching-super-and-sub? sub.des super.des))))))

    (define (%compatible/super-core/sub-complement super.des sub.des)
      (let ((sub-item.des (complement-type-descr.item-des sub.des)))
	(cond ((<top>-ctd? super.des)
	       ;;(matching <top> (not <fixnum>))		=> possible-match
	       ;;(matching <top> (not <top>))			=> no-match
	       ;;(matching <top> (not <void>))			=> possible-match
	       ;;(matching <top> (not <no-return>))		=> possible-match
	       (cond-with-predicates sub-item.des
		 (<top>-ctd?		#f)
		 (<void>-ctd?		#t)
		 (<no-return>-ctd?	#t)
		 (else
		  (object-type-descr.matching-super-and-sub? <top>-ctd sub-item.des))))

	      ((<void>-ctd? super.des)
	       ;;(matching <void> (not <fixnum>))		=> possible-match
	       ;;(matching <void> (not <top>))			=> possible-match
	       ;;(matching <void> (not <void>))			=> no-match
	       ;;(matching <void> (not <no-return>))		=> possible-match
	       (cond-with-predicates sub-item.des
		 (<no-return>-ctd?	#t)
		 (<void>-ctd?		#f)
		 (<top>-ctd?		#t)
		 (else
		  (object-type-descr.matching-super-and-sub? <top>-ctd sub-item.des))))

	      ((<no-return>-ctd? super.des)
	       ;;(matching <no-return> (not <fixnum>))		=> possible-match
	       ;;(matching <no-return> (not <top>))		=> possible-match
	       ;;(matching <no-return> (not <void>))		=> possible-match
	       ;;(matching <no-return> (not <no-return>))	=> no-match
	       (cond-with-predicates sub-item.des
		 (<top>-ctd?		#t)
		 (<void>-ctd?		#t)
		 (<no-return>-ctd?	#f)
		 (else
		  (object-type-descr.matching-super-and-sub? <top>-ctd sub-item.des))))

	      ((object-type-descr=? super.des sub-item.des)
	       ;;(matching <top> (not <top>))			=> no-match
	       #f)

	      ((object-type-descr.matching-super-and-sub? super.des sub-item.des)
	       ;;(matching <top>           (not <fixnum>))	=> possible-match
	       ;;(matching <exact-integer> (not <fixnum>))	=> possible-match
	       #t)

	      ((object-type-descr.matching-super-and-sub? sub-item.des super.des)
	       ;;(matching <fixnum>          (not <top>))	=> possible-match
	       ;;(matching <positive-fixnum> (not <fixnum>))	=> possible-match
	       #t)

	      (else
	       ;;(matching <fixnum> (not <flonum>))	=> possible-match
	       #t))))

    #| end of module: %COMPATIBLE-SUPER/CORE-TYPE-DESCR |# )

;;; --------------------------------------------------------------------

  (module (%compatible-super/record-type-descriptor)

    (define (%compatible-super/record-type-descriptor super.des sub.des)
      (cond-with-predicates sub.des
	(complement-type-descr?
	 (%compatible/super-record/sub-complement super.des sub.des))
      (else
       (object-type-descr.matching-super-and-sub? sub.des super.des))))

    (define (%compatible/super-record/sub-complement super.des sub.des)
      ;;In the comments, let's assume the following definitions:
      ;;
      ;;   (define-record-type <duo>)
      ;;   (define-record-type <alpha>)
      ;;   (define-record-type <beta>  (parent <alpha>))
      ;;
      (let ((sub-item.des (complement-type-descr.item-des sub.des)))
	(cond ((object-type-descr=? super.des sub-item.des)
	       ;;(matching <alpha> (not <alpha>))	=> no-match
	       #f)
	      ((object-type-descr.matching-super-and-sub? super.des sub-item.des)
	       ;;(matching <alpha> (not <beta>))	=> possible-match
	       #t)
	      ((object-type-descr.matching-super-and-sub? sub-item.des super.des)
	       ;;(matching <beta>  (not <alpha>))	=> possible-match
	       #t)
	      (else
	       ;;(matching <alpha> (not <duo>))		=> possible-match
	       #t))))

  #| end of module: %COMPATIBLE-SUPER/RECORD-TYPE-DESCRIPTOR |# )

;;; --------------------------------------------------------------------

  (define (%compatible-super/struct-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (complement-type-descr?
       (let ((sub-item.des (complement-type-descr.item-des sub.des)))
	 (cond
	  ((object-type-descr.matching-super-and-sub? super.des sub-item.des)
	   #f)
	  (else #t))))
      (else
       (object-type-descr.matching-super-and-sub? sub.des super.des))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/pair-type-descriptor super.des sub.des)
    (object-type-descr.matching-super-and-sub? sub.des super.des))

;;; --------------------------------------------------------------------

  (define (%compatible-super/pair-of-type-descriptor super.des sub.des)
    (object-type-descr.matching-super-and-sub? sub.des super.des))

;;; --------------------------------------------------------------------

  (define (%compatible-super/list-type-descriptor super.des sub.des)
    (object-type-descr.matching-super-and-sub? sub.des super.des))

;;; --------------------------------------------------------------------

  (define (%compatible-super/list-of-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (list-type-descr?
       ;;(matching (list-of <fixnum>) (list <number>))		=> possible-match
       (let ((super-item.des (list-of-type-descr.item-des super.des)))
	 (list-type-descr.for-all sub.des (lambda (sub-item.des)
					    (object-type-descr.compatible-super-and-sub? super-item.des sub-item.des)))))

      (list-of-type-descr?
       ;;(matching (list-of <fixnum>) (list-of <number>))	=> possible-match
       (object-type-descr.compatible-super-and-sub? (list-of-type-descr.item-des super.des)
						    (list-of-type-descr.item-des sub.des)))

      (else
       (object-type-descr.matching-super-and-sub? sub.des super.des))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/vector-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (vector-type-descr?
       ;;(matching (vector <fixnum>) (vector <number>))	=> possible-match
       (and (= (vector-type-descr.length super.des)
	       (vector-type-descr.length   sub.des))
	    (for-all (lambda (super-item.des sub-item.des)
		       (object-type-descr.compatible-super-and-sub? super-item.des sub-item.des))
	      (vector-type-descr.item-des* super.des)
	      (vector-type-descr.item-des* sub.des))))

      (vector-of-type-descr?
       ;;(matching (vector <fixnum>) (vector-of <number>))	=> possible-match
       (let ((sub-item.des (vector-of-type-descr.item-des sub.des)))
	 (vector-type-descr.for-all super.des (lambda (super-item.des)
						(object-type-descr.compatible-super-and-sub? super-item.des sub-item.des)))))

      (else
       (object-type-descr.matching-super-and-sub? sub.des super.des))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/vector-of-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (vector-type-descr?
       ;;(matching (vector-of <fixnum>) (vector <number>))	=> possible-match
       (let ((super-item.des (vector-of-type-descr.item-des super.des)))
	 (vector-type-descr.for-all sub.des (lambda (sub-item.des)
					      (object-type-descr.compatible-super-and-sub? super-item.des sub-item.des)))))

      (vector-of-type-descr?
       ;;(matching (vector-of <fixnum>) (vector-of <number>))	=> possible-match
       (object-type-descr.compatible-super-and-sub? (vector-of-type-descr.item-des super.des)
						    (vector-of-type-descr.item-des sub.des)))

      (else
       (object-type-descr.matching-super-and-sub? sub.des super.des))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/compound-condition-type-descriptor super.des sub.des)
    (object-type-descr.matching-super-and-sub? sub.des super.des))

;;; --------------------------------------------------------------------

  (define (%compatible-super/enumeration-type-descriptor super.des sub.des)
    (object-type-descr.matching-super-and-sub? sub.des super.des))

;;; --------------------------------------------------------------------

  (define (%compatible-super/closure-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (closure-type-descr?
       ;;For every super-type signature there  must be a matching sub-type signature;
       ;;so that the sub  can be used everywhere the super can be  used.  It does not
       ;;matter if the sub has clauses with non-matching signatures.
       (for-all (lambda (super.clause-signature)
		  (exists (lambda (sub.clause-signature)
			    (and (descriptors-signature.compatible-super-and-sub?
				  (lambda-descriptors.argvals super.clause-signature)
				  (lambda-descriptors.argvals sub.clause-signature))
				 (descriptors-signature.compatible-super-and-sub?
				  (lambda-descriptors.retvals super.clause-signature)
				  (lambda-descriptors.retvals sub.clause-signature))))
		    (case-lambda-descriptors.clause-signature* (closure-type-descr.signature sub.des))))
	 (case-lambda-descriptors.clause-signature* (closure-type-descr.signature super.des))))

      (else
       (object-type-descr.matching-super-and-sub? sub.des super.des))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/hashtable-type-descriptor super.des sub.des)
    (object-type-descr.matching-super-and-sub? sub.des super.des))

;;; --------------------------------------------------------------------

  (define (%compatible-super/alist-type-descriptor super.des sub.des)
    (object-type-descr.matching-super-and-sub? sub.des super.des))

;;; --------------------------------------------------------------------

  (define (%compatible-super/union-type-descriptor super.des sub.des)
    (union-type-descr.exists super.des (lambda (super-item.des)
					 (object-type-descr.compatible-super-and-sub? super-item.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/intersection-type-descriptor super.des sub.des)
    (intersection-type-descr.for-all super.des (lambda (super-item.des)
						 (object-type-descr.compatible-super-and-sub? super-item.des sub.des))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/complement-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (complement-type-descr?
       ;;(matching (not <number>) (not <fixnum>))	=> exact-match
       ;;(matching (not <fixnum>) (not <number>))	=> possible-match
       ;;(matching (not <fixnum>) (not <string>))	=> possible-match
       #t)
      (else
       ;;We must remember  that here SUB.DES is never a  core type descriptor.  Let's
       ;;assume the following definitions:
       ;;
       ;;   (define-record-type <duo>)
       ;;   (define-record-type <alpha>)
       ;;   (define-record-type <beta>  (parent <alpha>))
       ;;   (define-record-type <delta> (parent <beta>))
       ;;
       ;;then:
       ;;
       ;;   (matching (not <alpha>)	<duo>)		=> exact-match
       ;;   (matching (not <alpha>)	<alpha>)	=> no-match
       ;;   (matching (not <alpha>)	<beta>)		=> no-match
       ;;   (matching (not <alpha>)	<delta>)	=> no-match
       ;;   (matching (not <beta>)	<duo>)		=> exact-match
       ;;   (matching (not <beta>)	<alpha>)	=> possible-match
       ;;   (matching (not <beta>)	<beta>)		=> no-match
       ;;   (matching (not <beta>)	<delta>)	=> no-match
       ;;   (matching (not <delta>)	<duo>)		=> exact-match
       ;;   (matching (not <delta>)	<alpha>)	=> possible-match
       ;;   (matching (not <delta>)	<beta>)		=> possible-match
       ;;   (matching (not <delta>)	<delta>)	=> no-match
       ;;
       (let ((super-item.des (complement-type-descr.item-des super.des)))
	 (not (object-type-descr.matching-super-and-sub? super-item.des sub.des))))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/ancestor-of-type-descriptor super.des sub.des)
    (cond-with-predicates sub.des
      (ancestor-of-type-descr?
       ;;(define-record-type <duo>)
       ;;(define-record-type <alpha>)
       ;;(define-record-type <beta> (parent <alpha>))
       ;;
       ;;(matching (ancestor-of <alpha>) (ancestor-of <alpha>))		=> exact-match
       ;;(matching (ancestor-of <alpha>) (ancestor-of <beta>))		=> possible-match
       ;;(matching (ancestor-of <beta>)  (ancestor-of <alpha>))		=> exact-match
       ;;(matching (ancestor-of <alpha>) (ancestor-of <duo>))		=> no-match
       (object-type-descr.matching-super-and-sub? (ancestor-of-type-descr.item-des super.des)
						  (ancestor-of-type-descr.item-des sub.des)))
      (else #f)))

  #| end of module: OBJECT-TYPE-DESCR.COMPATIBLE-SUPER-AND-SUB? |# )


;;;; type descriptors: matching formal arguments and operands

(define (object-type-descr.matching-formal-and-operand formal.des operand.des)
  (cond ((object-type-descr.matching-super-and-sub? formal.des operand.des)
	 'exact-match)
	((object-type-descr.compatible-super-and-sub? formal.des operand.des)
	 'possible-match)
	(else
	 'no-match)))


;;;; descriptor of objects

(define (type-descriptor-of datum)
  ;;Recursive function.  Build and return  an object type descriptor representing the
  ;;type of datum.
  ;;
  ;;We use a hashtable  to detect circular structures in DATUM; we  put in here pairs
  ;;and vectors.
  (define table (make-eq-hashtable))
  (let recur ((datum datum))
    (cond ((boolean? datum)		(cond (datum
					       <true>-ctd)
					      (else
					       <false>-ctd)))
	  ((char?    datum)		<char>-ctd)
	  ((symbol?  datum)		(make-enumeration-type-descr (list datum)))
	  ((keyword? datum)		<keyword>-ctd)
	  ((procedure? datum)		<procedure>-ctd)

	  ((fixnum?  datum)		(cond ((fxpositive? datum)
					       <positive-fixnum>-ctd)
					      ((fxnegative? datum)
					       <negative-fixnum>-ctd)
					      ((fxzero? datum)
					       <zero-fixnum>-ctd)
					      (else
					       ;;This should never happen.
					       <fixnum>-ctd)))

	  ((flonum?  datum)		(cond ((flpositive? datum)
					       <positive-flonum>-ctd)
					      ((flnegative? datum)
					       <negative-flonum>-ctd)
					      ((flzero?/positive datum)
					       <positive-zero-flonum>-ctd)
					      ((flzero?/negative datum)
					       <negative-zero-flonum>-ctd)
					      (else
					       ;;This  happens  when  the  flonum  is
					       ;;not-a-number.
					       <flonum>-ctd)))

	  ((ratnum?  datum)		(cond ((ratnum-positive? datum)
					       <positive-ratnum>-ctd)
					      ((ratnum-negative? datum)
					       <negative-ratnum>-ctd)
					      (else
					       ;;This should never happen.
					       <ratnum>-ctd)))
	  ((bignum?  datum)		(cond ((bignum-positive? datum)
					       <positive-bignum>-ctd)
					      ((bignum-negative? datum)
					       <negative-bignum>-ctd)
					      (else
					       ;;This should never happen.
					       <bignum>-ctd)))
	  ((compnum? datum)		(cond ((exact-compnum? datum)
					       <exact-compnum>-ctd)
					      ((zero-compnum? datum)
					       <zero-compnum>-ctd)
					      (else
					       <non-zero-inexact-compnum>-ctd)))
	  ((cflonum? datum)		(cond ((zero-cflonum? datum)
					       <zero-cflonum>-ctd)
					      (else
					       <non-zero-cflonum>-ctd)))

	  ((string?  datum)		(cond ((string-empty? datum)
					       <empty-string>-ctd)
					      (else
					       <nestring>-ctd)))

	  ((null? datum)		<null>-ctd)

	  ((list? datum)		(if (hashtable-ref table datum #f)
					    <nelist>-ctd
					  (begin
					    (let pair-recur ((P datum))
					      (when (pair? P)
						(hashtable-set! table P #t)
						(pair-recur (cdr P))))
					    (make-list-type-descr (map recur datum)))))

	  ((pair? datum)		(if (hashtable-ref table datum #f)
					    <pair>-ctd
					  (begin
					    (hashtable-set! table datum #t)
					    (make-pair-type-descr (recur (car datum))
								  (recur (cdr datum))))))

	  ((vector?  datum)		(cond ((vector-empty? datum)
					       <empty-vector>-ctd)
					      (else
					       (if (hashtable-ref table datum #f)
						   <nevector>-ctd
						 (begin
						   (hashtable-set! table datum #t)
						   (make-vector-type-descr (map recur (vector->list datum))))))))

	  ((bytevector? datum)		(cond ((bytevector-empty? datum)
					       <empty-bytevector>-ctd)
					      (else
					       <nebytevector>-ctd)))

	  ((hashtable? datum)		<hashtable>-ctd)

	  ((eq? datum (void))		<void>-ctd)
	  (else				<top>-ctd))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
