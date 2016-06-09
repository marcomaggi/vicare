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
    ancestor-of-type-descr.item-des		ancestor-of-type-descr.ancestors-des*

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

    object-type-descr-of
    object-type-descr=?
    object-type-descr.ancestry-super-and-sub?
    object-type-descr.matching-super-and-sub?
    object-type-descr.compatible-super-and-sub?
    object-type-descr.parent
    object-type-descr.ancestors-des*

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
	  core-type-descriptor.uids-list
	  core-type-descriptor.parent
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

(define (same-type-descriptor? des1 des2)
  (or (eq? des1 des2)
      (and (core-type-descriptor? des1)
	   (core-type-descriptor? des2)
	   (eq? (car (core-type-descriptor.uids-list des1))
		(car (core-type-descriptor.uids-list des2))))))

(define (simple-condition-type-descr? des)
  (and (record-type-descriptor? des)
       ($rtd-subtype? des (record-type-descriptor &condition))))

(define (core-uids-list.super-and-sub? super.des sub.des)
  (memq (car (core-type-descriptor.uids-list super.des))
	(core-type-descriptor.uids-list sub.des)))

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
  (syntax-rules ()
    ((_ ?who ?descr)
     (define (?who obj)
       (and (core-type-descriptor? obj)
	    (eq? (car (core-type-descriptor.uids-list obj))
		 (car (core-type-descriptor.uids-list ?descr))))))
    ))

;;; --------------------------------------------------------------------

(define-type-descriptor-predicate <no-return>-ctd?		<no-return>-ctd)
(define-type-descriptor-predicate <void>-ctd?			<void>-ctd)
(define-type-descriptor-predicate <top>-ctd?			<top>-ctd)
(define-type-descriptor-predicate <symbol>-ctd?			<symbol>-ctd)

(define-type-descriptor-predicate <list>-ctd?			<list>-ctd)
(define-type-descriptor-predicate <nelist>-ctd?			<nelist>-ctd)
(define-type-descriptor-predicate <null>-ctd?			<null>-ctd)

(define-type-descriptor-predicate <vector>-ctd?			<vector>-ctd)
(define-type-descriptor-predicate <nevector>-ctd?		<nevector>-ctd)
(define-type-descriptor-predicate <empty-vector>-ctd?		<empty-vector>-ctd)

(define-type-descriptor-predicate <pair>-ctd?			<pair>-ctd)
(define-type-descriptor-predicate <procedure>-ctd?		<procedure>-ctd)

(define-type-descriptor-predicate <struct>-ctd?			<struct>-ctd)
(define-type-descriptor-predicate <record>-ctd?			<record>-ctd)

(define-type-descriptor-predicate <hashtable>-ctd?		<hashtable>-ctd)
(define-type-descriptor-predicate <condition>-ctd?		<condition>-ctd)
(define-type-descriptor-predicate <compound-condition>-ctd?	<compound-condition>-ctd)


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

(define (compound-condition-type-descr.exists compound-condition.des proc)
  (exists proc (compound-condition-type-descr.component-des* compound-condition.des)))

(define (compound-condition-type-descr.for-all compound-condition.des proc)
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

(define (enumeration-type-descr.length otd)
  (or (<enumeration-type-descr>-memoised-length otd)
      (receive-and-return (len)
	  (length (enumeration-type-descr.symbol* otd))
	(<enumeration-type-descr>-memoised-length-set! otd len))))


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

(define (list-type-descr.length otd)
  (or (<list-type-descr>-memoised-length otd)
      (receive-and-return (len)
	  (length (list-type-descr.item-des* otd))
	(<list-type-descr>-memoised-length-set! otd len))))


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

(define (vector-type-descr.length otd)
  (or (<vector-type-descr>-memoised-length otd)
      (receive-and-return (len)
	  (length (vector-type-descr.item-des* otd))
	(<vector-type-descr>-memoised-length-set! otd len))))


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

(define (complement-type-descr.not complement.des proc)
  (not (proc (complement-type-descr.item-des complement.des))))


;;;; compound type descriptors: ancestor-of

(define-record-type (<ancestor-of-type-descr> make-ancestor-of-type-descr ancestor-of-type-descr?)
  (nongenerative vicare:descriptors:<ancestor-of-type-descr>)
  (sealed #t)
  (fields
    (immutable item-des		ancestor-of-type-descr.item-des)
    (immutable ancestors-des*	ancestor-of-type-descr.ancestors-des*)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-record)
      (lambda (object.des)
	(make-record object.des (object-type-descr.ancestors-des* object.des))))))

(define <ancestor-of-type-descr>-rtd
  (record-type-descriptor <ancestor-of-type-descr>))

(define <ancestor-of-type-descr>-rcd
  (record-constructor-descriptor <ancestor-of-type-descr>))

;;; --------------------------------------------------------------------

(define (ancestor-of-type-descr.exists ancestor-of.des item.des)
  (exists (lambda (ancestor.des)
	    (object-type-descr=? ancestor.des item.des))
    (ancestor-of-type-descr.ancestors-des* ancestor-of.des)))


;;;; utilities

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


;;;; type descriptors: equality between super-types and sub-types

(define (object-type-descr=? super.des sub.des)
  (cond
   ((eq? super.des sub.des))

   ((core-type-descriptor? super.des)
    (same-type-descriptor? super.des sub.des))

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
    ;;The case of SUPER.DES being "<top>" has been handled above.
    (same-type-descriptor? <pair>-ctd super.des))

;;; --------------------------------------------------------------------

   ((pair-of-type-descr? super.des)
    (and (pair-of-type-descr? sub.des)
	 (object-type-descr=? (pair-of-type-descr.item-des super.des)
			      (pair-of-type-descr.item-des   sub.des))))

   ((pair-of-type-descr? sub.des)
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

   ((intersection-type-descr? super.des)
    (and (intersection-type-descr? sub.des)
	 (for-all (lambda (super-item.des)
		    (exists (lambda (sub-item.des)
			      (object-type-descr=? super-item.des sub-item.des))
		      (intersection-type-descr.item-des* sub.des)))
	   (intersection-type-descr.item-des* super.des))))

   ((complement-type-descr? super.des)
    (and (complement-type-descr? sub.des)
	 (object-type-descr=? (complement-type-descr.item-des super.des)
			      (complement-type-descr.item-des   sub.des))))

   ;;The cases in which SUB.DES is a compound type union, intersection or complement:
   ;;the result is true only is the parent is "<top>".

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


;;;; type descriptors: hierarchy super-types and sub-types

(define (object-type-descr.ancestry-super-and-sub? super.des sub.des)
  (define-syntax-rule (super-and-sub? A B)
    (object-type-descr.ancestry-super-and-sub? A B))
  (cond
   ((eq? super.des sub.des))

   ((core-type-descriptor? super.des)
    (cond ((<top>-ctd? super.des)
	   ;;"<top>" is the super-type of all the types, but "<void>".
	   (not (<void>-ctd? sub.des)))

	  ;;SUPER.DES and SUB.DES are both values of built-in Scheme types.
	  ((core-type-descriptor? sub.des)
	   (memq (car (core-type-descriptor.uids-list super.des))
		 (core-type-descriptor.uids-list sub.des)))

	  ((record-type-descriptor? sub.des)
	   (or (<record>-ctd? super.des)
	       (<struct>-ctd? super.des)))

	  ((struct-type-descriptor? sub.des)
	   (<struct>-ctd? super.des))

	  ((<pair>-ctd? super.des)
	   (or (pair-type-descr?    sub.des)
	       (pair-of-type-descr? sub.des)))

	  ((<list>-ctd? super.des)
	   (or (list-type-descr?    sub.des)
	       (list-of-type-descr? sub.des)
	       (alist-type-descr?   sub.des)))

	  ((<nelist>-ctd? super.des)
	   (list-type-descr? sub.des))

	  ((<vector>-ctd? super.des)
	   (or (vector-type-descr?    sub.des)
	       (vector-of-type-descr? sub.des)))

	  ((<nevector>-ctd? super.des)
	   (vector-type-descr? sub.des))

	  ((<symbol>-ctd? super.des)
	   (enumeration-type-descr? sub.des))

	  ((<procedure>-ctd? super.des)
	   (closure-type-descr? sub.des))

	  ((<hashtable>-ctd? super.des)
	   (hashtable-type-descr? sub.des))

	  (else #f)))

   ((record-type-descriptor? super.des)
    (cond ((record-type-descriptor? sub.des)
	   ($rtd-subtype? sub.des super.des))
	  (else #f)))

   ((struct-type-descriptor? super.des)
    (cond ((struct-type-descriptor? sub.des)
	   (eq? (struct-type-symbol super.des)
		(struct-type-symbol   sub.des)))
	  (else #f)))

;;; --------------------------------------------------------------------

   ((pair-type-descr? super.des)
    (and (pair-type-descr? sub.des)
	 (super-and-sub? (pair-type-descr.car-des super.des)
			 (pair-type-descr.car-des   sub.des))
	 (super-and-sub? (pair-type-descr.cdr-des super.des)
			 (pair-type-descr.cdr-des   sub.des))))

   ((pair-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((pair-of-type-descr? super.des)
    (and (pair-of-type-descr? sub.des)
	 (super-and-sub? (pair-of-type-descr.item-des super.des)
			 (pair-of-type-descr.item-des   sub.des))))

   ((pair-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((list-type-descr? super.des)
    (and (list-type-descr? sub.des)
	 (= (list-type-descr.length super.des)
	    (list-type-descr.length   sub.des))
	 (for-all object-type-descr.ancestry-super-and-sub?
	   (list-type-descr.item-des* super.des)
	   (list-type-descr.item-des*   sub.des))))

   ((list-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((list-of-type-descr? super.des)
    (or (and (list-of-type-descr? sub.des)
	     (super-and-sub? (list-of-type-descr.item-des super.des)
			     (list-of-type-descr.item-des   sub.des)))
	(<null>-ctd? sub.des)))

   ((list-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((vector-type-descr? super.des)
    (and (vector-type-descr? sub.des)
	 (= (vector-type-descr.length super.des)
	    (vector-type-descr.length   sub.des))
	 (for-all object-type-descr.ancestry-super-and-sub?
	   (vector-type-descr.item-des* super.des)
	   (vector-type-descr.item-des*   sub.des))))

   ((vector-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((vector-of-type-descr? super.des)
    (or (and (vector-of-type-descr? sub.des)
	     (super-and-sub? (vector-of-type-descr.item-des super.des)
			     (vector-of-type-descr.item-des   sub.des)))
	(<empty-vector>-ctd? sub.des)))

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
	   (and (object-type-descr.ancestry-super-and-sub? (hashtable-type-descr.key-des super.des)
							   (hashtable-type-descr.key-des sub.des))
		(object-type-descr.ancestry-super-and-sub? (hashtable-type-descr.val-des super.des)
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
			      (super-and-sub? super-item.des sub-item.des))
		      (union-type-descr.item-des* sub.des)))
	   (union-type-descr.item-des* super.des))))

   ((intersection-type-descr? super.des)
    (and (intersection-type-descr? sub.des)
	 (for-all (lambda (super-item.des)
		    (exists (lambda (sub-item.des)
			      (super-and-sub? super-item.des sub-item.des))
		      (intersection-type-descr.item-des* sub.des)))
	   (intersection-type-descr.item-des* super.des))))

   ((complement-type-descr? super.des)
    (and (complement-type-descr? sub.des)
	 (super-and-sub? (complement-type-descr.item-des super.des)
			 (complement-type-descr.item-des   sub.des))))

   ;;The cases in which SUB.DES is a compound type union, intersection or complement:
   ;;the result is true only is the parent is "<top>".

;;; --------------------------------------------------------------------

   ((ancestor-of-type-descr? super.des)
    (cond ((ancestor-of-type-descr? sub.des)
	   (object-type-descr=? (ancestor-of-type-descr.item-des super.des)
				(ancestor-of-type-descr.item-des   sub.des)))
	  (else
	   (exists (lambda (ancestor.des)
		     (object-type-descr=? ancestor.des sub.des))
	     (ancestor-of-type-descr.ancestors-des* super.des)))))

   ((ancestor-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   (else #f)))


;;;; type descriptors: matching super-types and sub-types

(define (object-type-descr.matching-super-and-sub? super.des sub.des)
  (define-syntax-rule (super-and-sub? A B)
    (object-type-descr.matching-super-and-sub? A B))
  (cond
   ((eq? super.des sub.des))

   ((core-type-descriptor? super.des)
    (if (core-type-descriptor? sub.des)
	;;We do this case first because it is the most likely.
	(core-uids-list.super-and-sub? super.des sub.des)
      ;;In this continuation we know that SUB.DES is *not* a core type descriptor.
      (cond-with-predicates super.des
	;;"<top>" is the super-type of all the types, but "<void>" and "<no-return>".
	(<top>-ctd?			(and (not (<void>-ctd?      sub.des))
					     (not (<no-return>-ctd? sub.des))))
	(<pair>-ctd?			(or-with-predicates sub.des pair-type-descr? pair-of-type-descr?))
	(<list>-ctd?			(or-with-predicates sub.des list-type-descr? list-of-type-descr? alist-type-descr?))
	(<nelist>-ctd?			(list-type-descr? sub.des))
	(<vector>-ctd?			(or-with-predicates sub.des vector-type-descr? vector-of-type-descr?))
	(<nevector>-ctd?		(vector-type-descr? sub.des))
	(<symbol>-ctd?			(enumeration-type-descr? sub.des))
	(<hashtable>-ctd?		(hashtable-type-descr? sub.des))
	(<condition>-ctd?		(or-with-predicates sub.des
					  compound-condition-type-descr?
					  simple-condition-type-descr?
					  <condition>-ctd?))
	(<compound-condition>-ctd?	(or-with-predicates sub.des
					  compound-condition-type-descr?
					  <compound-condition>-ctd?))
	(<record>-ctd?			(or-with-predicates sub.des
					  ;;The   record-type   case   includes   the
					  ;;condition object-type case.
					  record-type-descriptor?
					  compound-condition-type-descr?))
	(<struct>-ctd?			(or-with-predicates sub.des
					  ;;The   record-type   case   includes   the
					  ;;condition object-type case.
					  record-type-descriptor?
					  compound-condition-type-descr?
					  struct-type-descriptor?))
	(<void>-ctd?			#f)
	(<no-return>-ctd?		#f)
	(else
	 (cond-with-predicates sub.des
	   ;;For an  exact match:  every descriptor  in the SUB.DES  union must  be a
	   ;;sub-type of the super-type SUPER.DES.
	   (union-type-descr?		(union-type-descr.for-all sub.des (lambda (sub-item.des)
									    (super-and-sub? super.des sub-item.des))))
	   (intersection-type-descr?	(intersection-type-descr.for-all sub.des (lambda (sub-item.des)
										   (super-and-sub? super.des sub-item.des))))
	   (complement-type-descr?	#f)
	   (ancestor-of-type-descr?	(for-all (lambda (ancestor.des)
						   (super-and-sub? super.des ancestor.des))
					  (ancestor-of-type-descr.ancestors-des* sub.des)))
	   (else			#f))))))

   ((core-type-descriptor? sub.des)
    (cond-with-predicates super.des
      (ancestor-of-type-descr?		(ancestor-of-type-descr.exists super.des sub.des))
      (union-type-descr?		(union-type-descr.exists super.des (lambda (super-item.des)
									     (super-and-sub? super-item.des sub.des))))

      (intersection-type-descr?		(intersection-type-descr.for-all super.des (lambda (super-item.des)
										     (super-and-sub? super-item.des sub.des))))
      (complement-type-descr?		(let ((super-item.des (complement-type-descr.item-des super.des)))
					  (cond
					   ;;(super-and-sub? (not <string>) <top>) => #f
					   ((super-and-sub? sub.des super-item.des)
					    #f)
					   (else
					    ;;(super-and-sub? (not <fixnum>) <flonum>) => #t
					    ;;(super-and-sub? (not <struct>) <record>) => #t
					    (not (super-and-sub? super-item.des sub.des))))))
      ;;Special case: (super-and-sub? (list <top> <null>) <pair>)
      (list-type-descr?			(and (<list>-ctd? sub.des)
					     (= 2 (list-type-descr.length super.des))
					     (let ((item*.des (list-type-descr.item-des* super.des)))
					       (<top>-ctd?  (car  item*.des))
					       (<null>-ctd? (cadr item*.des)))))
      ;;Special case: (super-and-sub? (list-of <top>) <list>)
      ;;Special case: (super-and-sub? (list-of <top>) <nelist>)
      ;;Special case: (super-and-sub? (list-of ?type) <null>)
      (list-of-type-descr?		(or (and (or-with-predicates sub.des <list>-ctd? <nelist>-ctd?)
						 (<top>-ctd? (list-of-type-descr.item-des super.des)))
					    (<null>-ctd? sub.des)))
      ;;Special case: (super-and-sub? (vector-of <top>) <vector>)
      ;;Special case: (super-and-sub? (vector-of <top>) <nevector>)
      ;;Special case: (super-and-sub? (vector-of ?type) <empty-vector>)
      (vector-of-type-descr?		(or (and (or-with-predicates sub.des <vector>-ctd? <nevector>-ctd?)
						 (<top>-ctd? (vector-of-type-descr.item-des super.des)))
					    (<empty-vector>-ctd? sub.des)))
      ;;Special case: (super-and-sub? (pair <top> <top>) <pair>)
      ;;Special case: (super-and-sub? (pair <top> <list>) <nelist>)
      (pair-type-descr?			(or (and (<pair>-ctd? sub.des)
						 (<top>-ctd? (pair-type-descr.car-des super.des))
						 (<top>-ctd? (pair-type-descr.cdr-des super.des)))
					    (and (<nelist>-ctd? sub.des)
						 (<top>-ctd?  (pair-type-descr.car-des super.des))
						 (<list>-ctd? (pair-type-descr.cdr-des super.des)))))
      ;;Special case: (super-and-sub? (pair-of <top>) <pair>)
      (pair-of-type-descr?		(and (<pair>-ctd? sub.des)
					     (<top>-ctd? (pair-of-type-descr.item-des super.des))))
      (else				#f)))

;;; --------------------------------------------------------------------

   ((record-type-descriptor? super.des)
    (cond-with-predicates sub.des
      ;;This branch includes the case of SUPER.DES and SUB.DES being simple condition
      ;;object types.
      (record-type-descriptor?			($rtd-subtype? sub.des super.des))
      ;;(super-and-sub? &who (condition &who &irritants)) => #t
      (compound-condition-type-descr?		(and (simple-condition-type-descr? super.des)
						     (compound-condition-type-descr.exists sub.des
											   (lambda (sub-item.des)
											     (super-and-sub? super.des sub-item.des)))))
      (union-type-descr?			(union-type-descr.for-all sub.des (lambda (sub-item.des)
										    (super-and-sub? super.des sub-item.des))))
      (intersection-type-descr?			(intersection-type-descr.for-all sub.des (lambda (sub-item.des)
											   (super-and-sub? super.des sub-item.des))))
      (complement-type-descr?			#f)
      (ancestor-of-type-descr?			(ancestor-of-type-descr.exists sub.des super.des))
      (else					#f)))

;;; --------------------------------------------------------------------

   ((struct-type-descriptor? super.des)
    (cond-with-predicates sub.des
      (struct-type-descriptor?			(eq? (struct-type-symbol super.des) (struct-type-symbol sub.des)))
      (union-type-descr?			(union-type-descr.for-all sub.des (lambda (sub-item.des)
										    (super-and-sub? super.des sub-item.des))))
      (intersection-type-descr?			(intersection-type-descr.for-all sub.des (lambda (sub-item.des)
											   (super-and-sub? super.des sub-item.des))))
      (complement-type-descr?			#f)
      (ancestor-of-type-descr?			(ancestor-of-type-descr.exists sub.des super.des))
      (else					#f)))

;;; --------------------------------------------------------------------

   ((pair-type-descr? super.des)
    (let ((super-car.des (pair-type-descr.car-des super.des))
	  (super-cdr.des (pair-type-descr.cdr-des super.des)))
      (cond ((pair-type-descr? sub.des)
	     (and (super-and-sub? super-car.des (pair-type-descr.car-des sub.des))
		  (super-and-sub? super-cdr.des (pair-type-descr.cdr-des sub.des))))

	    ((pair-of-type-descr? sub.des)
	     (let ((sub-item.des (pair-of-type-descr.item-des sub.des)))
	       (and (super-and-sub? super-car.des sub-item.des)
		    (super-and-sub? super-cdr.des sub-item.des))))

	    ((list-type-descr? sub.des)
	     (let ((sub-item*.des (list-type-descr.item-des* sub.des)))
	       (and (super-and-sub? super-car.des (car sub-item*.des))
		    (super-and-sub? super-cdr.des (if (pair? (cdr sub-item*.des))
						      (make-list-type-descr (cdr sub-item*.des))
						    <null>-ctd)))))

	    ;;No list-of because a list-of may be empty.

	    ((same-type-descriptor? sub.des <nelist>-ctd)
	     (and (same-type-descriptor? super-car.des <top>-ctd)
		  (object-type-descr.list-type-descr? super-cdr.des)))

	    ((union-type-descr? sub.des)
	     (exists (lambda (sub-item.des)
		       (super-and-sub? super.des sub-item.des))
	       (union-type-descr.item-des* sub.des)))

	    ((intersection-type-descr? sub.des)
	     (for-all (lambda (sub-item.des)
			(super-and-sub? super.des sub-item.des))
	       (intersection-type-descr.item-des* sub.des)))

	    ((complement-type-descr? sub.des)
	     (not (super-and-sub? super.des (complement-type-descr.item-des sub.des))))

	    (else #f))))

;;; --------------------------------------------------------------------

   ((pair-of-type-descr? super.des)
    (let ((super-item.des (pair-of-type-descr.item-des super.des)))
      (cond ((pair-type-descr? sub.des)
	     (and (super-and-sub? super-item.des (pair-type-descr.car-des sub.des))
		  (super-and-sub? super-item.des (pair-type-descr.cdr-des sub.des))))

	    ((pair-of-type-descr? sub.des)
	     (super-and-sub? super-item.des (pair-of-type-descr.item-des sub.des)))

	    ((list-type-descr? sub.des)
	     (for-all (lambda (sub-item.des)
			(super-and-sub? super-item.des sub-item.des))
	       (list-type-descr.item-des* sub.des)))

	    ((list-of-type-descr? sub.des)
	     (super-and-sub? super-item.des (list-of-type-descr.item-des sub.des)))

	    ((or (same-type-descriptor? sub.des   <list>-ctd)
		 (same-type-descriptor? sub.des <nelist>-ctd))
	     (object-type-descr.list-type-descr? super-item.des))

	    ((union-type-descr? sub.des)
	     (exists (lambda (sub-item.des)
		       (super-and-sub? super.des sub-item.des))
	       (union-type-descr.item-des* sub.des)))

	    ((intersection-type-descr? sub.des)
	     (for-all (lambda (sub-item.des)
			(super-and-sub? super.des sub-item.des))
	       (intersection-type-descr.item-des* sub.des)))

	    ((complement-type-descr? sub.des)
	     (not (super-and-sub? super.des (complement-type-descr.item-des sub.des))))

	    (else #f))))

;;; --------------------------------------------------------------------

   ((list-type-descr? super.des)
    (let ((super-item*.des (list-type-descr.item-des* super.des)))
      (cond ((pair-type-descr? sub.des)
	     (and (super-and-sub? (car super-item*.des) (pair-type-descr.car-des sub.des))
		  (super-and-sub? (make-list-type-descr (cdr super-item*.des))
				  (pair-type-descr.cdr-des sub.des))))

	    ((pair-of-type-descr? sub.des)
	     (let ((sub-item.des (pair-of-type-descr.item-des sub.des)))
	       (and (super-and-sub? (car super-item*.des) sub-item.des)
		    (super-and-sub? (if (pair? (cdr super-item*.des))
					(make-list-type-descr (cdr super-item*.des))
				      <null>-ctd)
				    sub-item.des))))

	    ((list-type-descr? sub.des)
	     (for-all (lambda (super-item.des sub-item.des)
			(super-and-sub? super-item.des sub-item.des))
	       super-item*.des
	       (list-type-descr.item-des* sub.des)))

	    ;;No list-of because a list-of may be empty.

	    ;; (list <top>)    <nelist> --> match
	    ;; (list <fixnum>) <nelist> --> no match
	    ((same-type-descriptor? sub.des <nelist>-ctd)
	     (and (%list-of-single-item? super-item*.des)
		  (same-type-descriptor? (car super-item*.des) <top>-ctd)))

	    ((union-type-descr? sub.des)
	     (exists (lambda (sub-item.des)
		       (super-and-sub? super.des sub-item.des))
	       (union-type-descr.item-des* sub.des)))

	    ((intersection-type-descr? sub.des)
	     (for-all (lambda (sub-item.des)
			(super-and-sub? super.des sub-item.des))
	       (intersection-type-descr.item-des* sub.des)))

	    ((complement-type-descr? sub.des)
	     (not (super-and-sub? super.des (complement-type-descr.item-des sub.des))))

	    (else #f))))

;;; --------------------------------------------------------------------

   ((list-of-type-descr? super.des)
    (or (same-type-descriptor? sub.des <null>-ctd)
	(let ((super-item.des (list-of-type-descr.item-des super.des)))
	  (cond ((pair-type-descr? sub.des)
		 (and (super-and-sub? super-item.des (pair-type-descr.car-des sub.des))
		      (super-and-sub? super.des      (pair-type-descr.cdr-des sub.des))))

		((pair-of-type-descr? sub.des)
		 (let ((sub-item.des (pair-of-type-descr.item-des sub.des)))
		   (and (super-and-sub? super-item.des sub-item.des)
			(object-type-descr.list-type-descr? sub-item.des))))

		((list-type-descr? sub.des)
		 (for-all (lambda (sub-item.des)
			    (super-and-sub? super-item.des sub-item.des))
		   (list-type-descr.item-des* sub.des)))

		((list-of-type-descr? sub.des)
		 (super-and-sub? super-item.des (list-of-type-descr.item-des sub.des)))

		((or (same-type-descriptor? sub.des <nelist>-ctd)
		     (same-type-descriptor? sub.des   <list>-ctd))
		 (same-type-descriptor? (list-of-type-descr.item-des super.des)
					<top>-ctd))

		((union-type-descr? sub.des)
		 (exists (lambda (sub-item.des)
			   (super-and-sub? super.des sub-item.des))
		   (union-type-descr.item-des* sub.des)))

		((intersection-type-descr? sub.des)
		 (for-all (lambda (sub-item.des)
			    (super-and-sub? super.des sub-item.des))
		   (intersection-type-descr.item-des* sub.des)))

		((complement-type-descr? sub.des)
		 (not (super-and-sub? super.des (complement-type-descr.item-des sub.des))))

		(else #f)))))

;;; --------------------------------------------------------------------

   ((vector-type-descr? super.des)
    (let ((super-item*.des (vector-type-descr.item-des* super.des)))
      (cond ((vector-type-descr? sub.des)
	     (and (= (vector-type-descr.length super.des)
		     (vector-type-descr.length   sub.des))
		  (for-all (lambda (super-item.des sub-item.des)
			     (super-and-sub? super-item.des sub-item.des))
		    super-item*.des
		    (vector-type-descr.item-des* sub.des))))

	    ;;No vector-of because a vector-of may be empty.

	    ;; (vector <top>)    <nevector> --> match
	    ;; (vector <fixnum>) <nevector> --> no match
	    ((same-type-descriptor? sub.des <nevector>-ctd)
	     (and (%list-of-single-item? super-item*.des)
		  (same-type-descriptor? (car super-item*.des) <top>-ctd)))

	    ((union-type-descr? sub.des)
	     (exists (lambda (sub-item.des)
		       (super-and-sub? super.des sub-item.des))
	       (union-type-descr.item-des* sub.des)))

	    ((intersection-type-descr? sub.des)
	     (for-all (lambda (sub-item.des)
			(super-and-sub? super.des sub-item.des))
	       (intersection-type-descr.item-des* sub.des)))

	    ((complement-type-descr? sub.des)
	     (not (super-and-sub? super.des (complement-type-descr.item-des sub.des))))

	    (else #f))))

;;; --------------------------------------------------------------------

   ((vector-of-type-descr? super.des)
    (or (same-type-descriptor? sub.des <empty-vector>-ctd)
	(let ((super-item.des (vector-of-type-descr.item-des super.des)))
	  (cond ((vector-type-descr? sub.des)
		 (for-all (lambda (sub-item.des)
			    (super-and-sub? super-item.des sub-item.des))
		   (vector-type-descr.item-des* sub.des)))

		((vector-of-type-descr? sub.des)
		 (super-and-sub? super-item.des (vector-of-type-descr.item-des sub.des)))

		((or (same-type-descriptor? sub.des <nevector>-ctd)
		     (same-type-descriptor? sub.des   <vector>-ctd))
		 (same-type-descriptor? (vector-of-type-descr.item-des super.des)
					<top>-ctd))

		((union-type-descr? sub.des)
		 (exists (lambda (sub-item.des)
			   (super-and-sub? super.des sub-item.des))
		   (union-type-descr.item-des* sub.des)))

		((intersection-type-descr? sub.des)
		 (for-all (lambda (sub-item.des)
			    (super-and-sub? super.des sub-item.des))
		   (intersection-type-descr.item-des* sub.des)))

		((complement-type-descr? sub.des)
		 (not (super-and-sub? super.des (complement-type-descr.item-des sub.des))))

		(else #f)))))

;;; --------------------------------------------------------------------

   ((compound-condition-type-descr? super.des)
    (cond ((compound-condition-type-descr? sub.des)
	   (let ((super-component*.des (compound-condition-type-descr.component-des* super.des))
		 (sub-component*.des   (compound-condition-type-descr.component-des*   sub.des)))
	     (for-all (lambda (super-component.des)
			(exists (lambda (sub-component.des)
				  (object-type-descr.matching-super-and-sub? super-component.des sub-component.des))
			  sub-component*.des))
	       super-component*.des)))
	  (else #f)))

   ((compound-condition-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((enumeration-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (enumeration-type-descr? sub.des)
	     (let ((sub*.sym (enumeration-type-descr.symbol* sub.des)))
	       (for-all (lambda (super.sym)
			  (memq super.sym sub*.sym))
		 (enumeration-type-descr.symbol* super.des))))))

   ((enumeration-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((closure-type-descr? super.des)
    (cond ((closure-type-descr? sub.des)
	   (eq? 'exact-match (closure-type-descr.match-super-and-sub super.des sub.des)))
	  (else #f)))

   ((closure-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((hashtable-type-descr? super.des)
    (cond ((hashtable-type-descr? sub.des)
	   (and (object-type-descr.matching-super-and-sub? (hashtable-type-descr.key-des super.des)
							   (hashtable-type-descr.key-des sub.des))
		(object-type-descr.matching-super-and-sub? (hashtable-type-descr.val-des super.des)
							   (hashtable-type-descr.val-des sub.des))))
	  (else #f)))

   ((hashtable-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((alist-type-descr? super.des)
    (cond ((alist-type-descr? sub.des)
	   (and (object-type-descr.matching-super-and-sub? (alist-type-descr.key-des super.des)
							   (alist-type-descr.key-des sub.des))
		(object-type-descr.matching-super-and-sub? (alist-type-descr.val-des super.des)
							   (alist-type-descr.val-des sub.des))))
	  (else #f)))

   ((alist-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((union-type-descr? super.des)
    (cond ((union-type-descr? sub.des)
	   (let ((super-item*.des (union-type-descr.item-des* super.des))
		 (sub-item*.des   (union-type-descr.item-des* super.des)))
	     (for-all (lambda (super-item.des)
			(exists (lambda (sub-item.des)
				  (object-type-descr.matching-super-and-sub? super-item.des sub-item.des))
			  sub-item*.des))
	       super-item*.des)))
	  (else
	   (exists (lambda (super-item.des)
		     (super-and-sub? super-item.des sub.des))
	     (union-type-descr.item-des* super.des)))))

   ((union-type-descr? sub.des)
    (cond ((ancestor-of-type-descr? super.des)
	   (let ((super-component*.des	(ancestor-of-type-descr.ancestors-des* super.des))
		 (sub-component*.des	(union-type-descr.item-des* sub.des)))
	     (exists (lambda (super-component.des)
		       (for-all (lambda (sub-component.des)
				  (object-type-descr=? super-component.des sub-component.des))
			 sub-component*.des))
	       super-component*.des)))
	  (else #f)))

;;; --------------------------------------------------------------------

   ((intersection-type-descr? super.des)
    (cond ((intersection-type-descr? sub.des)
	   (let ((super-item*.des (intersection-type-descr.item-des* super.des))
		 (sub-item*.des   (intersection-type-descr.item-des* super.des)))
	     (for-all (lambda (super-item.des)
			(for-all (lambda (sub-item.des)
				   (object-type-descr.matching-super-and-sub? super-item.des sub-item.des))
			  sub-item*.des))
	       super-item*.des)))
	  (else
	   (for-all (lambda (super-item.des)
		      (super-and-sub? super-item.des sub.des))
	     (intersection-type-descr.item-des* super.des)))))

   ((intersection-type-descr? sub.des)
    (cond ((ancestor-of-type-descr? super.des)
	   (let ((super-component*.des	(ancestor-of-type-descr.ancestors-des* super.des))
		 (sub-component*.des	(intersection-type-descr.item-des* sub.des)))
	     (exists (lambda (super-component.des)
		       (for-all (lambda (sub-component.des)
				  (object-type-descr=? super-component.des sub-component.des))
			 sub-component*.des))
	       super-component*.des)))
	  (else #f)))

;;; --------------------------------------------------------------------

   ((complement-type-descr? super.des)
    (let ((super-item.des (complement-type-descr.item-des super.des)))
      (cond
       ;;(super-and-sub? (not <struct>) (not <record>)) => #t
       ((complement-type-descr? sub.des)
	(super-and-sub? super-item.des (complement-type-descr.item-des sub.des)))
       ;;(super-and-sub? (not <string>) <top>) => #f
       ((super-and-sub? sub.des super-item.des)
	#f)
       (else
	;;(super-and-sub? (not <fixnum>) <flonum>) => #t
	;;(super-and-sub? (not <struct>) <record>) => #t
	(not (super-and-sub? super-item.des sub.des))))))

   ((complement-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((ancestor-of-type-descr? super.des)
    (cond ((ancestor-of-type-descr? sub.des)
	   ;;(super-and-sub? (ancestor-of <record>) (ancestor-of <struct>)) => #t
	   ;;(super-and-sub? (ancestor-of <struct>) (ancestor-of <record>)) => #f
	   (object-type-descr=? (ancestor-of-type-descr.item-des sub.des)
				(ancestor-of-type-descr.item-des super.des)))
	  (else
	   (exists (lambda (ancestor.des)
		     (object-type-descr=? ancestor.des sub.des))
	     (ancestor-of-type-descr.ancestors-des* super.des)))))

   ((ancestor-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   (else #f)))


;;;; type descriptors: compatible super-types and sub-types

(define (object-type-descr.compatible-super-and-sub? super.des sub.des)
  ;;This  function  is used  to  check  for  non-matching compatibility  between  two
  ;;object-type    descriptors.      It    is    meant    to     be    called    when
  ;;OBJECT-TYPE-DESCR.MATCHING-SUPER-AND-SUB? has already returned #f when applied to
  ;;the same operands.
  ;;
  ;;As  example: a  "<number>" argument  matches a  "<fixnum>" operand;  a "<fixnum>"
  ;;argument is compatible with a "<number>" operand.
  ;;
  (define (%matching-or-compatible? super.des sub.des)
    (or (object-type-descr.matching-super-and-sub?   super.des sub.des)
	(object-type-descr.compatible-super-and-sub? super.des sub.des)))
  (cond

   ((and (core-type-descriptor? super.des)
	 (core-type-descriptor?   sub.des))
    (object-type-descr.matching-super-and-sub? sub.des super.des))

;;; --------------------------------------------------------------------

   ((union-type-descr? sub.des)
    (exists (lambda (item-sub.des)
	      (%matching-or-compatible? super.des item-sub.des))
      (union-type-descr.item-des* sub.des)))

;;; --------------------------------------------------------------------

   ((intersection-type-descr? super.des)
    (for-all (lambda (super-item.des)
	       (%matching-or-compatible? super-item.des sub.des))
      (intersection-type-descr.item-des* super.des)))

;;; --------------------------------------------------------------------

   ((complement-type-descr? super.des)
    (cond ((complement-type-descr? sub.des)
	   #f)
	  (else
	   (let ((super-item.des (complement-type-descr.item-des super.des)))
	     (cond
	      ;; (type-signature-matching ((not (ancestor-of &condition)))
	      ;;                          (<condition>))
	      ;; => no-match
	      ((ancestor-of-type-descr? super-item.des)
	       #f)
	      ((object-type-descr=? super-item.des sub.des)
	       #f)
	      (else
	       ;; (type-signature-matching ((not <fixnum>))
	       ;;                          (<exact-integer>))
	       ;; => possible-match
	       (object-type-descr.matching-super-and-sub? sub.des super-item.des)))))))

   ((complement-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------
;;; lists

   ((and (list-of-type-descr? super.des)
	 (list-type-descr?    sub.des))
    (let ((super-item.des (list-of-type-descr.item-des super.des)))
      (for-all (lambda (sub-item.des)
		 (object-type-descr.compatible-super-and-sub? super-item.des sub-item.des))
	(list-type-descr.item-des* sub.des))))

   ((list-of-type-descr? super.des)
    (or (<nelist>-ctd? sub.des)
	(object-type-descr.matching-super-and-sub? sub.des super.des)))

   ((list-type-descr? super.des)
    (or (<list>-ctd?   sub.des)
	(object-type-descr.matching-super-and-sub? sub.des super.des)))

   ((<nelist>-ctd? super.des)
    ;; (type-annotation-matching <nelist> (list-of <top>)) => possible-match
    (or (list-of-type-descr? sub.des)
	(<pair>-ctd?        sub.des)
	(object-type-descr.matching-super-and-sub? sub.des super.des)))

   ((and (%compare-super-with-sub-and-its-parents <list>-ctd super.des)
	 (%compare-super-with-sub-and-its-parents <pair>-ctd sub.des))
    (cond ((pair-type-descr? sub.des)
	   (object-type-descr.compatible-super-and-sub? <list>-ctd (pair-type-descr.cdr-des sub.des)))
	  ((pair-of-type-descr? sub.des)
	   (object-type-descr.compatible-super-and-sub? <list>-ctd (pair-of-type-descr.item-des sub.des)))
	  (else #t)))

;;; --------------------------------------------------------------------

   ((and (vector-of-type-descr? super.des)
	 (vector-type-descr?    sub.des))
    (let ((super-item.des (vector-of-type-descr.item-des super.des)))
      (for-all (lambda (sub-item.des)
		 (object-type-descr.compatible-super-and-sub? super-item.des sub-item.des))
	(vector-type-descr.item-des* sub.des))))

;;; --------------------------------------------------------------------
;;; closure type specifications

   ((closure-type-descr? super.des)
    (cond ((closure-type-descr? sub.des)
	   ;;For every  super-type signature there  must be a  matching sub-type
	   ;;signature; so that the sub can  be used everywhere the super can be
	   ;;used.  It does not matter if  the sub has clauses with non-matching
	   ;;signatures.
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
	   (or (<procedure>-ctd? sub.des)
	       (<top>-ctd?       sub.des)))))

;;; --------------------------------------------------------------------

   ((object-type-descr.matching-super-and-sub? sub.des super.des))

   (else #f)))

(define (%compare-super-with-sub-and-its-parents super.des sub.des)
  ;;Recursive function.   Search SUPER.DES in  the hierarchy of SUB.DES:  return true
  ;;when found, false otherwise.
  ;;
  (cond ((object-type-descr=? super.des sub.des))
	((object-type-descr.parent sub.des)
	 => (lambda (sub-parent.des)
	      (%compare-super-with-sub-and-its-parents super.des sub-parent.des)))
	(else #f)))


;;;; object-type descriptors: parent

(define (object-type-descr.parent object.des)
  ;;Return false or an object-type descriptor which is the parent of OBJECT.DES.
  ;;
  (cond ((core-type-descriptor? object.des)
	 (core-type-descriptor.parent object.des))

	((record-type-descriptor? object.des)
	 (or (record-type-parent object.des)
	     <record>-ctd))

	((struct-type-descriptor? object.des)
	 <struct>-ctd)

	((closure-type-descr? object.des)
	 <procedure>-ctd)

	((enumeration-type-descr? object.des)
	 <symbol>-ctd)

	((list-of-type-descr? object.des)
	 <list>-ctd)

	((list-type-descr? object.des)
	 <nelist>-ctd)

	((vector-of-type-descr? object.des)
	 <vector>-ctd)

	((vector-type-descr? object.des)
	 <nevector>-ctd)

	((or (pair-type-descr?    object.des)
	     (pair-of-type-descr? object.des))
	 <pair>-ctd)

	((compound-condition-type-descr? object.des)
	 <compound-condition>-ctd)

	((alist-type-descr? object.des)
	 <list>-ctd)

	((hashtable-type-descr? object.des)
	 <hashtable>-ctd)

	(else
	 <top>-ctd)))


;;;; object-type descriptors: ancestry

(define (object-type-descr.ancestors-des* object.des)
  ;;Return a (possibly  empty) list of type descriptors representing  the ancestry of
  ;;OBJECT.DES.  OBJECT.DES is *not* included in the list.
  ;;
  (cond ((core-type-descriptor? object.des)
	 (let recur ((object.des object.des))
	   (cond ((core-type-descriptor.parent object.des)
		  => (lambda (parent.des)
		       (cons parent.des (recur parent.des))))
		 (else '()))))

	((record-type-descriptor? object.des)
	 (let recur ((obj.des object.des))
	   (cond ((record-type-parent obj.des)
		  => (lambda (parent.des)
		       (cons parent.des (recur parent.des))))
		 (else
		  (let ((c* (list <record>-ctd <struct>-ctd <top>-ctd)))
		    (if ($rtd-subtype? object.des (record-type-descriptor &condition))
			(cons <condition>-ctd c*)
		      c*))))))

	((struct-type-descriptor? object.des)
	 (list <struct>-ctd
	       <top>-ctd))

	((closure-type-descr? object.des)
	 (list <procedure>-ctd
	       <top>-ctd))

	((enumeration-type-descr? object.des)
	 (list <symbol>-ctd
	       <top>-ctd))

	((list-of-type-descr? object.des)
	 (list <list>-ctd <top>-ctd))

	((list-type-descr? object.des)
	 (list <nelist>-ctd <list>-ctd <top>-ctd))

	((vector-of-type-descr? object.des)
	 (list <vector>-ctd <top>-ctd))

	((vector-type-descr? object.des)
	 (list <nevector>-ctd <vector>-ctd <top>-ctd))

	((or (pair-type-descr?    object.des)
	     (pair-of-type-descr? object.des))
	 (list <pair>-ctd <top>-ctd))

	((alist-type-descr? object.des)
	 (list <list>-ctd
	       <top>-ctd))

	((hashtable-type-descr? object.des)
	 (list <hashtable>-ctd
	       <top>-ctd))

	((compound-condition-type-descr? object.des)
	 (list <compound-condition>-ctd
	       <condition>-ctd
	       <record>-ctd
	       <struct>-ctd
	       <top>-ctd))

	(else
	 (list <top>-ctd))))


;;;; descriptor of objects

(define (object-type-descr-of datum)
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
