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
    <pair-type-descr>-rtd
    <pair-type-descr>-rcd
    make-pair-type-descr
    pair-type-descr?
    pair-type-descr.car-des
    pair-type-descr.cdr-des

    <pair-of-type-descr>-rtd
    <pair-of-type-descr>-rcd
    make-pair-of-type-descr
    pair-of-type-descr?
    pair-of-type-descr.item-des

    <list-type-descr>-rtd
    <list-type-descr>-rcd
    make-list-type-descr
    list-type-descr?
    list-type-descr.item-des*

    <list-of-type-descr>-rtd
    <list-of-type-descr>-rcd
    make-list-of-type-descr
    list-of-type-descr?
    list-of-type-descr.item-des

    <vector-type-descr>-rtd
    <vector-type-descr>-rcd
    make-vector-type-descr
    vector-type-descr?
    vector-type-descr.item-des*

    <vector-of-type-descr>-rtd
    <vector-of-type-descr>-rcd
    make-vector-of-type-descr
    vector-of-type-descr?
    vector-of-type-descr.item-des

    <enumeration-type-descr>-rtd
    <enumeration-type-descr>-rcd
    make-enumeration-type-descr
    enumeration-type-descr?
    enumeration-type-descr.symbol*

;;; --------------------------------------------------------------------

    <union-type-descr>-rtd
    <union-type-descr>-rcd
    make-union-type-descr
    union-type-descr?
    union-type-descr.item-des*

    <intersection-type-descr>-rtd
    <intersection-type-descr>-rcd
    make-intersection-type-descr
    intersection-type-descr?
    intersection-type-descr.item-des*

    <complement-type-descr>-rtd
    <complement-type-descr>-rcd
    make-complement-type-descr
    complement-type-descr?
    complement-type-descr.item-des

;;; --------------------------------------------------------------------

    object-type-descr-of
    object-type-descr=?
    object-type-descr.ancestry-super-and-sub?
    object-type-descr.matching-super-and-sub?

    #| end of EXPORTS |# )
  (import (except (vicare)
		  ratnum-positive?
		  ratnum-negative?
		  exact-compnum?
		  zero-compnum?
		  zero-cflonum?)
    (only (ikarus.object-utilities)
	  scheme-type-descriptor?
	  scheme-type-descriptor-uids-list
	  <bignum>-type-descriptor
	  <binary-input-port>-type-descriptor
	  <binary-input/output-port>-type-descriptor
	  <binary-output-port>-type-descriptor
	  <boolean>-type-descriptor
	  <bytevector>-type-descriptor
	  <empty-bytevector>-type-descriptor
	  <nebytevector>-type-descriptor
	  <cflonum>-type-descriptor
	  <char>-type-descriptor
	  <code>-type-descriptor
	  <complex>-type-descriptor
	  <compnum>-type-descriptor
	  <compound-condition>-type-descriptor
	  <condition>-type-descriptor
	  <empty-string>-type-descriptor
	  <empty-vector>-type-descriptor
	  <enum-set>-type-descriptor
	  <eof>-type-descriptor
	  <exact-compnum>-type-descriptor
	  <exact-integer>-type-descriptor
	  <false>-type-descriptor
	  <fixnum>-type-descriptor
	  <flonum>-type-descriptor
	  <gensym>-type-descriptor
	  <hashtable-eq>-type-descriptor
	  <hashtable-equal>-type-descriptor
	  <hashtable-eqv>-type-descriptor
	  <hashtable>-type-descriptor
	  <inexact-compnum>-type-descriptor
	  <input-port>-type-descriptor
	  <input/output-port>-type-descriptor
	  <integer-valued>-type-descriptor
	  <integer>-type-descriptor
	  <ipair>-type-descriptor
	  <keyword>-type-descriptor
	  <list>-type-descriptor
	  <memory-block>-type-descriptor
	  <negative-bignum>-type-descriptor
	  <negative-fixnum>-type-descriptor
	  <negative-flonum>-type-descriptor
	  <negative-ratnum>-type-descriptor
	  <negative-zero-flonum>-type-descriptor
	  <nelist>-type-descriptor
	  <nestring>-type-descriptor
	  <nevector>-type-descriptor
	  <no-return>-type-descriptor
	  <non-zero-cflonum>-type-descriptor
	  <non-zero-inexact-compnum>-type-descriptor
	  <null>-type-descriptor
	  <number>-type-descriptor
	  <opaque-record>-type-descriptor
	  <output-port>-type-descriptor
	  <pair>-type-descriptor
	  <pointer>-type-descriptor
	  <port>-type-descriptor
	  <positive-bignum>-type-descriptor
	  <positive-fixnum>-type-descriptor
	  <positive-flonum>-type-descriptor
	  <positive-ratnum>-type-descriptor
	  <positive-zero-flonum>-type-descriptor
	  <procedure>-type-descriptor
	  <promise>-type-descriptor
	  <rational-valued>-type-descriptor
	  <rational>-type-descriptor
	  <ratnum>-type-descriptor
	  <reader-annotation>-type-descriptor
	  <real-valued>-type-descriptor
	  <real>-type-descriptor
	  <record-constructor-descriptor>-type-descriptor
	  <record-type-descriptor>-type-descriptor
	  <record>-type-descriptor
	  <scheme-type-descriptor>-type-descriptor
	  <sentinel>-type-descriptor
	  <stats>-type-descriptor
	  <string>-type-descriptor
	  <struct-type-descriptor>-type-descriptor
	  <struct>-type-descriptor
	  <symbol>-type-descriptor
	  <textual-input-port>-type-descriptor
	  <textual-input/output-port>-type-descriptor
	  <textual-output-port>-type-descriptor
	  <time>-type-descriptor
	  <untyped>-type-descriptor
	  <top>-type-descriptor
	  <transcoder>-type-descriptor
	  <true>-type-descriptor
	  <utsname>-type-descriptor
	  <vector>-type-descriptor
	  <void>-type-descriptor
	  <would-block>-type-descriptor
	  <zero-cflonum>-type-descriptor
	  <zero-compnum>-type-descriptor
	  <zero-fixnum>-type-descriptor
	  <zero-flonum>-type-descriptor)
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
      (and (scheme-type-descriptor? des1)
	   (scheme-type-descriptor? des2)
	   (eq? (car (scheme-type-descriptor-uids-list des1))
		(car (scheme-type-descriptor-uids-list des2))))))


;;;; compound type descriptors: pairs

(define-record-type (<pair-type-descr> make-pair-type-descr pair-type-descr?)
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
  (fields
    (immutable item-des		pair-of-type-descr.item-des)
    #| end of FIELDS |# ))

(define <pair-of-type-descr>-rtd
  (record-type-descriptor <pair-of-type-descr>))

(define <pair-of-type-descr>-rcd
  (record-constructor-descriptor <pair-of-type-descr>))


;;;; compound type descriptors: lists

(define-record-type (<list-type-descr> %make-list-type-descr list-type-descr?)
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
      <null>-type-descriptor
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
  (fields
    (immutable item-des		list-of-type-descr.item-des)
    #| end of FIELDS |# ))

(define <list-of-type-descr>-rtd
  (record-type-descriptor <list-of-type-descr>))

(define <list-of-type-descr>-rcd
  (record-constructor-descriptor <list-of-type-descr>))


;;;; compound type descriptors: vectors

(define-record-type (<vector-type-descr> %make-vector-type-descr vector-type-descr?)
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
      <empty-vector>-type-descriptor
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
  (fields
    (immutable item-des		vector-of-type-descr.item-des)
    #| end of FIELDS |# ))

(define <vector-of-type-descr>-rtd
  (record-type-descriptor <vector-of-type-descr>))

(define <vector-of-type-descr>-rcd
  (record-constructor-descriptor <vector-of-type-descr>))


;;;; compound type descriptors: symbols enumeration

(define-record-type (<enumeration-type-descr> make-enumeration-type-descr enumeration-type-descr?)
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


;;;; compound type descriptors: union

(define-record-type (<union-type-descr> %make-union-type-descr union-type-descr?)
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


;;;; compound type descriptors: intersection

(define-record-type (<intersection-type-descr> %make-intersection-type-descr intersection-type-descr?)
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


;;;; compound type descriptors: complement

(define-record-type (<complement-type-descr> make-complement-type-descr complement-type-descr?)
  (fields
    (immutable item-des		complement-type-descr.item-des)
    #| end of FIELDS |# ))

(define <complement-type-descr>-rtd
  (record-type-descriptor <complement-type-descr>))

(define <complement-type-descr>-rcd
  (record-constructor-descriptor <complement-type-descr>))


;;;; utilities

(define (object-type-descr.pair-type-descr? object.des)
  (or (same-type-descriptor? object.des <pair>-type-descriptor)
      (pair-type-descr? object.des)
      (pair-of-type-descr? object.des)
      (list-type-descr? object.des)))

;;; --------------------------------------------------------------------

(define (object-type-descr.list-type-descr? object.des)
  (or (same-type-descriptor? object.des   <list>-type-descriptor)
      (same-type-descriptor? object.des   <null>-type-descriptor)
      (same-type-descriptor? object.des <nelist>-type-descriptor)
      (list-type-descr? object.des)
      (list-of-type-descr? object.des)
      (and (pair-type-descr? object.des)
	   (object-type-descr.list-type-descr? (pair-type-descr.cdr-des object.des)))
      (and (pair-of-type-descr? object.des)
	   (object-type-descr.list-type-descr? (pair-of-type-descr.item-des object.des)))))

(define (object-type-descr.nelist-type-descr? object.des)
  (or (same-type-descriptor? object.des <nelist>-type-descriptor)
      (list-type-descr? object.des)
      (and (pair-type-descr? object.des)
	   (object-type-descr.list-type-descr? (pair-type-descr.cdr-des object.des)))
      (and (pair-of-type-descr? object.des)
	   (object-type-descr.list-type-descr? (pair-of-type-descr.item-des object.des)))))

;;; --------------------------------------------------------------------

(define (object-type-descr.vector-type-descr? object.des)
  (or (same-type-descriptor? object.des   <vector>-type-descriptor)
      (same-type-descriptor? object.des <nevector>-type-descriptor)
      (vector-type-descr? object.des)
      (vector-of-type-descr? object.des)))

(define (object-type-descr.nevector-type-descr? object.des)
  (or (same-type-descriptor? object.des <nevector>-type-descriptor)
      (vector-type-descr? object.des)))


;;;; type descriptors: equality between super-types and sub-types

(define (object-type-descr=? super.des sub.des)
  (cond
   ((scheme-type-descriptor? super.des)
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
    (or (eq? super.des sub.des)
	(and (pair-type-descr? sub.des)
	     (object-type-descr=? (pair-type-descr.car-des super.des)
				  (pair-type-descr.car-des   sub.des))
	     (object-type-descr=? (pair-type-descr.cdr-des super.des)
				  (pair-type-descr.cdr-des   sub.des)))))

   ((pair-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" has been handled above.
    (same-type-descriptor? <pair>-type-descriptor super.des))

;;; --------------------------------------------------------------------

   ((pair-of-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (pair-of-type-descr? sub.des)
	     (object-type-descr=? (pair-of-type-descr.item-des super.des)
				  (pair-of-type-descr.item-des   sub.des)))))

   ((pair-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((list-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (list-type-descr? sub.des)
	     (= (list-type-descr.length super.des)
		(list-type-descr.length   sub.des))
	     (for-all object-type-descr=?
	       (list-type-descr.item-des* super.des)
	       (list-type-descr.item-des*   sub.des)))))

   ((list-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((list-of-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (list-of-type-descr? sub.des)
	     (object-type-descr=? (list-of-type-descr.item-des super.des)
				  (list-of-type-descr.item-des   sub.des)))))

   ((list-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((vector-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (vector-type-descr? sub.des)
	     (= (vector-type-descr.length super.des)
		(vector-type-descr.length   sub.des))
	     (for-all object-type-descr=?
	       (vector-type-descr.item-des* super.des)
	       (vector-type-descr.item-des*   sub.des)))))

   ((vector-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((vector-of-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (vector-of-type-descr? sub.des)
	     (object-type-descr=? (vector-of-type-descr.item-des super.des)
				  (vector-of-type-descr.item-des   sub.des)))))

   ((vector-of-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((enumeration-type-descr? super.des)
    (or (eq? super.des sub.des)
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
		      sub*.sym))))))

   ((enumeration-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((union-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (union-type-descr? sub.des)
	     (for-all (lambda (super-item.des)
			(exists (lambda (sub-item.des)
				  (object-type-descr=? super-item.des sub-item.des))
			  (union-type-descr.item-des* sub.des)))
	       (union-type-descr.item-des* super.des)))))

   ((intersection-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (intersection-type-descr? sub.des)
	     (for-all (lambda (super-item.des)
			(exists (lambda (sub-item.des)
				  (object-type-descr=? super-item.des sub-item.des))
			  (intersection-type-descr.item-des* sub.des)))
	       (intersection-type-descr.item-des* super.des)))))

   ((complement-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (complement-type-descr? sub.des)
	     (object-type-descr=? (complement-type-descr.item-des super.des)
				  (complement-type-descr.item-des   sub.des)))))

   ;;The cases in which SUB.DES is a compound type union, intersection or complement:
   ;;the result is true only is the parent is "<top>".

;;; --------------------------------------------------------------------

   (else #f)))


;;;; type descriptors: hierarchy super-types and sub-types

(define (object-type-descr.ancestry-super-and-sub? super.des sub.des)
  (define-syntax-rule (super-and-sub? A B)
    (object-type-descr.ancestry-super-and-sub? A B))
  (cond
   ((scheme-type-descriptor? super.des)
    ;;"<top>" is the super-type of all the types.
    (cond ((same-type-descriptor? super.des <top>-type-descriptor)
	   (not (same-type-descriptor? sub.des <void>-type-descriptor)))

	  ;;SUPER.DES and SUB.DES are both values of built-in Scheme types.
	  ((scheme-type-descriptor? sub.des)
	   (memq (car (scheme-type-descriptor-uids-list super.des))
		 (scheme-type-descriptor-uids-list sub.des)))

	  ((record-type-descriptor? sub.des)
	   (or (same-type-descriptor? super.des <record>-type-descriptor)
	       (same-type-descriptor? super.des <struct>-type-descriptor)
	       (same-type-descriptor? super.des    <top>-type-descriptor)))

	  ((struct-type-descriptor? sub.des)
	   (or (same-type-descriptor? super.des <struct>-type-descriptor)
	       (same-type-descriptor? super.des    <top>-type-descriptor)))

	  ((same-type-descriptor? super.des <pair>-type-descriptor)
	   (or (pair-type-descr? sub.des)
	       (pair-of-type-descr? sub.des)))

	  ((same-type-descriptor? super.des <list>-type-descriptor)
	   (or (list-type-descr? sub.des)
	       (list-of-type-descr? sub.des)
	       (same-type-descriptor? sub.des <null>-type-descriptor)))

	  ((same-type-descriptor? super.des <nelist>-type-descriptor)
	   (list-type-descr? sub.des))

	  ((same-type-descriptor? super.des <vector>-type-descriptor)
	   (or (vector-type-descr? sub.des)
	       (vector-of-type-descr? sub.des)
	       (same-type-descriptor? sub.des <empty-vector>-type-descriptor)))

	  ((same-type-descriptor? super.des <nevector>-type-descriptor)
	   (vector-type-descr? sub.des))

	  ((same-type-descriptor? super.des <symbol>-type-descriptor)
	   (enumeration-type-descr? sub.des))

	  (else #f)))

   ;;SUPER.DES is a record-type descriptor.
   ((record-type-descriptor? super.des)
    (cond ((record-type-descriptor? sub.des)
	   ($rtd-subtype? sub.des super.des))
	  (else #f)))

   ;;SUPER.DES is a  struct-type descriptor.  SUB.DES matches only if  it is equal to
   ;;SUPER.DES.
   ((struct-type-descriptor? super.des)
    (cond ((struct-type-descriptor? sub.des)
	   (or (eq? super.des sub.des)
	       (eq? (struct-type-symbol super.des)
		    (struct-type-symbol   sub.des))))
	  (else #f)))

;;; --------------------------------------------------------------------

   ((pair-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (pair-type-descr? sub.des)
	     (super-and-sub? (pair-type-descr.car-des super.des)
			     (pair-type-descr.car-des   sub.des))
	     (super-and-sub? (pair-type-descr.cdr-des super.des)
			     (pair-type-descr.cdr-des   sub.des)))))

   ((pair-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" has been handled above.
    (same-type-descriptor? <pair>-type-descriptor super.des))

;;; --------------------------------------------------------------------

   ((pair-of-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (pair-of-type-descr? sub.des)
	     (super-and-sub? (pair-of-type-descr.item-des super.des)
			     (pair-of-type-descr.item-des   sub.des)))))

   ((pair-of-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" has been handled above.
    (same-type-descriptor? <pair>-type-descriptor super.des))

;;; --------------------------------------------------------------------

   ((list-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (list-type-descr? sub.des)
	     (= (list-type-descr.length super.des)
		(list-type-descr.length   sub.des))
	     (for-all object-type-descr.ancestry-super-and-sub?
	       (list-type-descr.item-des* super.des)
	       (list-type-descr.item-des*   sub.des)))))

   ((list-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" has been handled above.
    (or (same-type-descriptor? super.des   <list>-type-descriptor)
	(same-type-descriptor? super.des <nelist>-type-descriptor)))

;;; --------------------------------------------------------------------

   ((list-of-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (list-of-type-descr? sub.des)
	     (super-and-sub? (list-of-type-descr.item-des super.des)
			     (list-of-type-descr.item-des   sub.des)))
	(same-type-descriptor? sub.des <null>-type-descriptor)))

   ((list-of-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" has been handled above.
    (same-type-descriptor? <list>-type-descriptor super.des))

;;; --------------------------------------------------------------------

   ((vector-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (vector-type-descr? sub.des)
	     (= (vector-type-descr.length super.des)
		(vector-type-descr.length   sub.des))
	     (for-all object-type-descr.ancestry-super-and-sub?
	       (vector-type-descr.item-des* super.des)
	       (vector-type-descr.item-des*   sub.des)))))

   ((vector-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" has been handled above.
    (or (same-type-descriptor? super.des   <vector>-type-descriptor)
	(same-type-descriptor? super.des <nevector>-type-descriptor)))

;;; --------------------------------------------------------------------

   ((vector-of-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (vector-of-type-descr? sub.des)
	     (super-and-sub? (vector-of-type-descr.item-des super.des)
			     (vector-of-type-descr.item-des   sub.des)))
	(same-type-descriptor? sub.des <empty-vector>-type-descriptor)))

   ((vector-of-type-descr? sub.des)
    ;;The case of SUPER.DES being "<top>" has been handled above.
    (same-type-descriptor? <vector>-type-descriptor super.des))

;;; --------------------------------------------------------------------

   ((enumeration-type-descr? super.des)
    (or (eq? super.des sub.des)
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
		      sub*.sym))))))

   ((enumeration-type-descr? sub.des)
    #f)

;;; --------------------------------------------------------------------

   ((union-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (union-type-descr? sub.des)
	     (for-all (lambda (super-item.des)
			(exists (lambda (sub-item.des)
				  (super-and-sub? super-item.des sub-item.des))
			  (union-type-descr.item-des* sub.des)))
	       (union-type-descr.item-des* super.des)))))

   ((intersection-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (intersection-type-descr? sub.des)
	     (for-all (lambda (super-item.des)
			(exists (lambda (sub-item.des)
				  (super-and-sub? super-item.des sub-item.des))
			  (intersection-type-descr.item-des* sub.des)))
	       (intersection-type-descr.item-des* super.des)))))

   ((complement-type-descr? super.des)
    (or (eq? super.des sub.des)
	(and (complement-type-descr? sub.des)
	     (super-and-sub? (complement-type-descr.item-des super.des)
			     (complement-type-descr.item-des   sub.des)))))

   ;;The cases in which SUB.DES is a compound type union, intersection or complement:
   ;;the result is true only is the parent is "<top>".

;;; --------------------------------------------------------------------

   (else #f)))


;;;; type descriptors: matching super-types and sub-types

(define (object-type-descr.matching-super-and-sub? super.des sub.des)
  (define-syntax-rule (super-and-sub? A B)
    (object-type-descr.matching-super-and-sub? A B))
  (cond
   ((scheme-type-descriptor? super.des)
    ;;"<top>" is the super-type of all the types.
    (cond ((same-type-descriptor? super.des <top>-type-descriptor))

	  ;;SUPER.DES and SUB.DES are both values of built-in Scheme types.
	  ((scheme-type-descriptor? sub.des)
	   (memq (car (scheme-type-descriptor-uids-list super.des))
		 (scheme-type-descriptor-uids-list sub.des)))

	  ((record-type-descriptor? sub.des)
	   (or (same-type-descriptor? super.des <record>-type-descriptor)
	       (same-type-descriptor? super.des <struct>-type-descriptor)
	       (same-type-descriptor? super.des    <top>-type-descriptor)))

	  ((struct-type-descriptor? sub.des)
	   (or (same-type-descriptor? super.des <struct>-type-descriptor)
	       (same-type-descriptor? super.des    <top>-type-descriptor)))

	  ((same-type-descriptor? super.des <pair>-type-descriptor)
	   (object-type-descr.pair-type-descr? sub.des))

	  ((same-type-descriptor? super.des <list>-type-descriptor)
	   (object-type-descr.list-type-descr? sub.des))

	  ((same-type-descriptor? super.des <nelist>-type-descriptor)
	   (object-type-descr.nelist-type-descr? sub.des))

	  ((same-type-descriptor? super.des <vector>-type-descriptor)
	   (object-type-descr.vector-type-descr? sub.des))

	  ((same-type-descriptor? super.des <nevector>-type-descriptor)
	   (object-type-descr.nevector-type-descr? sub.des))

	  ((same-type-descriptor? super.des <symbol>-type-descriptor)
	   (enumeration-type-descr? sub.des))

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

	  (else #f)))

;;; --------------------------------------------------------------------

   ;;SUPER.DES is a record-type descriptor.
   ((record-type-descriptor? super.des)
    (cond ((record-type-descriptor? sub.des)
	   ($rtd-subtype? sub.des super.des))

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

	  (else #f)))

;;; --------------------------------------------------------------------

   ;;SUPER.DES is a  struct-type descriptor.  SUB.DES matches only if  it is equal to
   ;;SUPER.DES.
   ((struct-type-descriptor? super.des)
    (cond ((struct-type-descriptor? sub.des)
	   (or (eq? super.des sub.des)
	       (eq? (struct-type-symbol super.des)
		    (struct-type-symbol   sub.des))))

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

	  (else #f)))

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
						    <null>-type-descriptor)))))

	    ;;No list-of because a list-of may be empty.

	    ((same-type-descriptor? sub.des <nelist>-type-descriptor)
	     (and (same-type-descriptor? super-car.des <top>-type-descriptor)
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

	    ((or (same-type-descriptor? sub.des   <list>-type-descriptor)
		 (same-type-descriptor? sub.des <nelist>-type-descriptor))
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
				      <null>-type-descriptor)
				    sub-item.des))))

	    ((list-type-descr? sub.des)
	     (for-all (lambda (super-item.des sub-item.des)
			(super-and-sub? super-item.des sub-item.des))
	       super-item*.des
	       (list-type-descr.item-des* sub.des)))

	    ;;No list-of because a list-of may be empty.

	    ;; (list <top>)    <nelist> --> match
	    ;; (list <fixnum>) <nelist> --> no match
	    ((same-type-descriptor? sub.des <nelist>-type-descriptor)
	     (and (%list-of-single-item? super-item*.des)
		  (same-type-descriptor? (car super-item*.des) <top>-type-descriptor)))

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
    (or (same-type-descriptor? sub.des <null>-type-descriptor)
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

		((or (same-type-descriptor? sub.des <nelist>-type-descriptor)
		     (same-type-descriptor? sub.des   <list>-type-descriptor))
		 (same-type-descriptor? (list-of-type-descr.item-des super.des)
					<top>-type-descriptor))

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
	     (for-all (lambda (super-item.des sub-item.des)
			(super-and-sub? super-item.des sub-item.des))
	       super-item*.des
	       (vector-type-descr.item-des* sub.des)))

	    ;;No vector-of because a vector-of may be empty.

	    ;; (vector <top>)    <nevector> --> match
	    ;; (vector <fixnum>) <nevector> --> no match
	    ((same-type-descriptor? sub.des <nevector>-type-descriptor)
	     (and (%list-of-single-item? super-item*.des)
		  (same-type-descriptor? (car super-item*.des) <top>-type-descriptor)))

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
    (or (same-type-descriptor? sub.des <empty-vector>-type-descriptor)
	(let ((super-item.des (vector-of-type-descr.item-des super.des)))
	  (cond ((vector-type-descr? sub.des)
		 (for-all (lambda (sub-item.des)
			    (super-and-sub? super-item.des sub-item.des))
		   (vector-type-descr.item-des* sub.des)))

		((vector-of-type-descr? sub.des)
		 (super-and-sub? super-item.des (vector-of-type-descr.item-des sub.des)))

		((or (same-type-descriptor? sub.des <nevector>-type-descriptor)
		     (same-type-descriptor? sub.des   <vector>-type-descriptor))
		 (same-type-descriptor? (vector-of-type-descr.item-des super.des)
					<top>-type-descriptor))

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

   ((union-type-descr? super.des)
    (exists (lambda (super-item.des)
	      (super-and-sub? super-item.des sub.des))
      (union-type-descr.item-des* super.des)))

;;; --------------------------------------------------------------------

   ((intersection-type-descr? super.des)
    (for-all (lambda (super-item.des)
	       (super-and-sub? super-item.des sub.des))
      (intersection-type-descr.item-des* super.des)))

;;; --------------------------------------------------------------------

   ((complement-type-descr? super.des)
    (not (super-and-sub? (complement-type-descr.item-des super.des) sub.des)))

;;; --------------------------------------------------------------------

   (else #f)))


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
					       <true>-type-descriptor)
					      (else
					       <false>-type-descriptor)))
	  ((char?    datum)		<char>-type-descriptor)
	  ((symbol?  datum)		(make-enumeration-type-descr (list datum)))
	  ((keyword? datum)		<keyword>-type-descriptor)

	  ((fixnum?  datum)		(cond ((fxpositive? datum)
					       <positive-fixnum>-type-descriptor)
					      ((fxnegative? datum)
					       <negative-fixnum>-type-descriptor)
					      ((fxzero? datum)
					       <zero-fixnum>-type-descriptor)
					      (else
					       ;;This should never happen.
					       <fixnum>-type-descriptor)))

	  ((flonum?  datum)		(cond ((flpositive? datum)
					       <positive-flonum>-type-descriptor)
					      ((flnegative? datum)
					       <negative-flonum>-type-descriptor)
					      ((flzero?/positive datum)
					       <positive-zero-flonum>-type-descriptor)
					      ((flzero?/negative datum)
					       <negative-zero-flonum>-type-descriptor)
					      (else
					       ;;This  happens  when  the  flonum  is
					       ;;not-a-number.
					       <flonum>-type-descriptor)))

	  ((ratnum?  datum)		(cond ((ratnum-positive? datum)
					       <positive-ratnum>-type-descriptor)
					      ((ratnum-negative? datum)
					       <negative-ratnum>-type-descriptor)
					      (else
					       ;;This should never happen.
					       <ratnum>-type-descriptor)))
	  ((bignum?  datum)		(cond ((bignum-positive? datum)
					       <positive-bignum>-type-descriptor)
					      ((bignum-negative? datum)
					       <negative-bignum>-type-descriptor)
					      (else
					       ;;This should never happen.
					       <bignum>-type-descriptor)))
	  ((compnum? datum)		(cond ((exact-compnum? datum)
					       <exact-compnum>-type-descriptor)
					      ((zero-compnum? datum)
					       <zero-compnum>-type-descriptor)
					      (else
					       <non-zero-inexact-compnum>-type-descriptor)))
	  ((cflonum? datum)		(cond ((zero-cflonum? datum)
					       <zero-cflonum>-type-descriptor)
					      (else
					       <non-zero-cflonum>-type-descriptor)))

	  ((string?  datum)		(cond ((string-empty? datum)
					       <empty-string>-type-descriptor)
					      (else
					       <nestring>-type-descriptor)))

	  ((null? datum)		<null>-type-descriptor)

	  ((list? datum)		(if (hashtable-ref table datum #f)
					    <nelist>-type-descriptor
					  (begin
					    (let pair-recur ((P datum))
					      (when (pair? P)
						(hashtable-set! table P #t)
						(pair-recur (cdr P))))
					    (make-list-type-descr (map recur datum)))))

	  ((pair? datum)		(if (hashtable-ref table datum #f)
					    <pair>-type-descriptor
					  (begin
					    (hashtable-set! table datum #t)
					    (make-pair-type-descr (recur (car datum))
								  (recur (cdr datum))))))

	  ((vector?  datum)		(cond ((vector-empty? datum)
					       <empty-vector>-type-descriptor)
					      (else
					       (if (hashtable-ref table datum #f)
						   <nevector>-type-descriptor
						 (begin
						   (hashtable-set! table datum #t)
						   (make-vector-type-descr (map recur (vector->list datum))))))))

	  ((bytevector? datum)		(cond ((bytevector-empty? datum)
					       <empty-bytevector>-type-descriptor)
					      (else
					       <nebytevector>-type-descriptor)))

	  ((eq? datum (void))		<void>-type-descriptor)
	  (else				<top>-type-descriptor))))


;;;; type descriptor signatures

(define-record-type (<descriptors-signature> make-descriptors-signature descriptors-signature?)
  (fields
    (immutable descriptors	descriptors-signature.descriptors)
    #| end of FIELDS |# ))

(define (descriptor-signature.matching-super-and-sub? super.sig sub.sig)
  (void))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
