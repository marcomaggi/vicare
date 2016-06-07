;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for object utilities core primitives
;;Date: Thu Mar 24, 2016
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
(library (typed-core-primitives object-utilities)
  (export typed-core-primitives.object-utilities)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.object-utilities)


;;;; <core-type-descriptor> record, safe procedures

(section

(declare-core-rtd <core-type-descriptor>-rtd)
(declare-core-rcd <core-type-descriptor>-rcd)

(declare-type-predicate core-type-descriptor?		<core-type-descriptor>)

(declare-core-primitive core-type-descriptor.name
    (safe)
  (signatures
   ((<core-type-descriptor>)		=> (<symbol>))))

(declare-core-primitive core-type-descriptor.parent
    (safe)
  (signatures
   ((<core-type-descriptor>)		=> ((or <false> <core-type-descriptor>)))))

(declare-core-primitive core-type-descriptor.uids-list
    (safe)
  (signatures
   ((<core-type-descriptor>)		=> ((list-of <symbol>)))))

(declare-core-primitive core-type-descriptor.method-retriever
    (safe)
  (signatures
   ((<core-type-descriptor>)		=> (<procedure>))))

/section)


;;;; stuff, safe procedures

(section

(declare-core-primitive method-call-late-binding
    (safe)
  (signatures
   ((<symbol> <top> . <list>)		=> <list>)))

(declare-core-primitive internal-delete
    (safe)
  (signatures
   ((<top>)				=> (<void>))))

/section)


;;;; Scheme build-in object-type descriptors

(declare-core-type-descriptor <bignum>-ctd)
(declare-core-type-descriptor <binary-input/output-port>-ctd)
(declare-core-type-descriptor <binary-input-port>-ctd)
(declare-core-type-descriptor <binary-output-port>-ctd)
(declare-core-type-descriptor <binary-port>-ctd)
(declare-core-type-descriptor <boolean>-ctd)
(declare-core-type-descriptor <bytevector>-ctd)
(declare-core-type-descriptor <nebytevector>-ctd)
(declare-core-type-descriptor <empty-bytevector>-ctd)
(declare-core-type-descriptor <cflonum>-ctd)
(declare-core-type-descriptor <zero-cflonum>-ctd)
(declare-core-type-descriptor <non-zero-cflonum>-ctd)
(declare-core-type-descriptor <char>-ctd)
(declare-core-type-descriptor <code>-ctd)
(declare-core-type-descriptor <complex>-ctd)
(declare-core-type-descriptor <compnum>-ctd)
(declare-core-type-descriptor <zero-compnum>-ctd)
(declare-core-type-descriptor <non-zero-inexact-compnum>-ctd)
(declare-core-type-descriptor <compound-condition>-ctd)
(declare-core-type-descriptor <condition>-ctd)
(declare-core-type-descriptor <empty-vector>-ctd)
(declare-core-type-descriptor <enum-set>-ctd)
(declare-core-type-descriptor <eof>-ctd)
(declare-core-type-descriptor <exact-compnum>-ctd)
(declare-core-type-descriptor <inexact-compnum>-ctd)
(declare-core-type-descriptor <exact-integer>-ctd)
(declare-core-type-descriptor <false>-ctd)
(declare-core-type-descriptor <fixnum>-ctd)
(declare-core-type-descriptor <flonum>-ctd)
(declare-core-type-descriptor <hashtable-eq>-ctd)
(declare-core-type-descriptor <hashtable-equal>-ctd)
(declare-core-type-descriptor <hashtable-eqv>-ctd)
(declare-core-type-descriptor <hashtable>-ctd)
(declare-core-type-descriptor <input/output-port>-ctd)
(declare-core-type-descriptor <input-port>-ctd)
(declare-core-type-descriptor <integer>-ctd)
(declare-core-type-descriptor <integer-valued>-ctd)
(declare-core-type-descriptor <ipair>-ctd)
(declare-core-type-descriptor <keyword>-ctd)
(declare-core-type-descriptor <list>-ctd)
(declare-core-type-descriptor <nelist>-ctd)
;;
(declare-core-type-descriptor <negative-bignum>-ctd)
(declare-core-type-descriptor <negative-fixnum>-ctd)
(declare-core-type-descriptor <negative-flonum>-ctd)
(declare-core-type-descriptor <negative-ratnum>-ctd)
(declare-core-type-descriptor <negative-zero-flonum>-ctd)
(declare-core-type-descriptor <no-return>-ctd)
(declare-core-type-descriptor <null>-ctd)
(declare-core-type-descriptor <number>-ctd)
(declare-core-type-descriptor <opaque-record>-ctd)
(declare-core-type-descriptor <output-port>-ctd)
(declare-core-type-descriptor <pair>-ctd)
(declare-core-type-descriptor <pointer>-ctd)
(declare-core-type-descriptor <port>-ctd)
(declare-core-type-descriptor <positive-bignum>-ctd)
(declare-core-type-descriptor <positive-fixnum>-ctd)
(declare-core-type-descriptor <positive-flonum>-ctd)
(declare-core-type-descriptor <positive-ratnum>-ctd)
(declare-core-type-descriptor <positive-zero-flonum>-ctd)
(declare-core-type-descriptor <procedure>-ctd)
(declare-core-type-descriptor <promise>-ctd)
(declare-core-type-descriptor <rational>-ctd)
(declare-core-type-descriptor <rational-valued>-ctd)
(declare-core-type-descriptor <ratnum>-ctd)
(declare-core-type-descriptor <real>-ctd)
(declare-core-type-descriptor <real-valued>-ctd)
(declare-core-type-descriptor <record-constructor-descriptor>-ctd)
(declare-core-type-descriptor <record>-ctd)
(declare-core-type-descriptor <record-type-descriptor>-ctd)
(declare-core-type-descriptor <string>-ctd)
(declare-core-type-descriptor <nestring>-ctd)
(declare-core-type-descriptor <empty-string>-ctd)
(declare-core-type-descriptor <struct>-ctd)
(declare-core-type-descriptor <struct-type-descriptor>-ctd)
(declare-core-type-descriptor <symbol>-ctd)
(declare-core-type-descriptor <gensym>-ctd)
(declare-core-type-descriptor <textual-input/output-port>-ctd)
(declare-core-type-descriptor <textual-input-port>-ctd)
(declare-core-type-descriptor <textual-output-port>-ctd)
(declare-core-type-descriptor <textual-port>-ctd)
(declare-core-type-descriptor <top>-ctd)
(declare-core-type-descriptor <transcoder>-ctd)
(declare-core-type-descriptor <true>-ctd)
(declare-core-type-descriptor <utsname>-ctd)
(declare-core-type-descriptor <vector>-ctd)
(declare-core-type-descriptor <nevector>-ctd)
(declare-core-type-descriptor <void>-ctd)
(declare-core-type-descriptor <would-block>-ctd)
(declare-core-type-descriptor <zero-fixnum>-ctd)
;;
(declare-core-type-descriptor <memory-block>-ctd)
(declare-core-type-descriptor <time>-ctd)
(declare-core-type-descriptor <reader-annotation>-ctd)
(declare-core-type-descriptor <core-type-descriptor>-ctd)
(declare-core-type-descriptor <stats>-ctd)


;;;; object-type lambda signatures

(declare-core-rtd <descriptors-signature>-rtd)
(declare-core-rcd <descriptors-signature>-rcd)

(declare-core-primitive make-descriptors-signature
    (safe)
  (signatures
   ((<top>)				=> (<descriptors-signature>))))

(declare-type-predicate descriptors-signature?	<descriptors-signature>)

(declare-core-primitive descriptors-signature.object-type-descrs
    (safe)
  (signatures
   ((<descriptors-signature>)		=> (<top>))))

(declare-core-primitive descriptors-signature=?
    (safe)
  (signatures
   ((<descriptors-signature> <descriptors-signature>)	=> (<boolean>))))

(declare-core-primitive descriptors-signature.matching-super-and-sub?
    (safe)
  (signatures
   ((<descriptors-signature> <descriptors-signature>)	=> (<boolean>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <lambda-descriptors>-rtd)
(declare-core-rcd <lambda-descriptors>-rcd)

(declare-core-primitive make-lambda-descriptors
    (safe)
  (signatures
   ((<descriptors-signature> <descriptors-signature>)	=> (<lambda-descriptors>))))

(declare-type-predicate lambda-descriptors?	<lambda-descriptors>)

(declare-core-primitive lambda-descriptors.retvals
    (safe)
  (signatures
   ((<lambda-descriptors>)		=> (<descriptors-signature>))))

(declare-core-primitive lambda-descriptors.argvals
    (safe)
  (signatures
   ((<lambda-descriptors>)		=> (<descriptors-signature>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <case-lambda-descriptors>-rtd)
(declare-core-rcd <case-lambda-descriptors>-rcd)

(declare-core-primitive make-case-lambda-descriptors
    (safe)
  (signatures
   (((pair <lambda-descriptors> (list-of <lambda-descriptors>)))
    => (<case-lambda-descriptors>))))

(declare-type-predicate case-lambda-descriptors?	<case-lambda-descriptors>)

(declare-core-primitive case-lambda-descriptors.clause-signature*
    (safe)
  (signatures
   ((<case-lambda-descriptors>)		=> ((pair <lambda-descriptors> (list-of <lambda-descriptors>))))))


;;;; object type descriptors

(declare-core-rtd <pair-type-descr>-rtd)
(declare-core-rcd <pair-type-descr>-rcd)

(declare-core-primitive make-pair-type-descr
    (safe)
  (signatures
   ((<top> <top>)		=> (<pair-type-descr>))))

(declare-type-predicate pair-type-descr?	<pair-type-descr>)

(declare-core-primitive pair-type-descr.car-des
    (safe)
  (signatures
   ((<pair-type-descr>)		=> (<top>))))

(declare-core-primitive pair-type-descr.cdr-des
    (safe)
  (signatures
   ((<pair-type-descr>)		=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <pair-of-type-descr>-rtd)
(declare-core-rcd <pair-of-type-descr>-rcd)

(declare-core-primitive make-pair-of-type-descr
    (safe)
  (signatures
   ((<top>)			=> (<pair-of-type-descr>))))

(declare-type-predicate pair-of-type-descr?	<pair-of-type-descr>)

(declare-core-primitive pair-of-type-descr.item-des
    (safe)
  (signatures
   ((<pair-of-type-descr>)	=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <list-type-descr>-rtd)
(declare-core-rcd <list-type-descr>-rcd)

(declare-core-primitive make-list-type-descr
    (safe)
  (signatures
   ((<null>)			=> (<core-type-descriptor>))
   ((<list>)			=> (<list-type-descr>))))

(declare-type-predicate list-type-descr?	<list-type-descr>)

(declare-core-primitive list-type-descr.item-des*
    (safe)
  (signatures
   ((<list-type-descr>)		=> (<list>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <list-of-type-descr>-rtd)
(declare-core-rcd <list-of-type-descr>-rcd)

(declare-core-primitive make-list-of-type-descr
    (safe)
  (signatures
   ((<top>)			=> (<list-of-type-descr>))))

(declare-type-predicate list-of-type-descr?	<list-of-type-descr>)

(declare-core-primitive list-of-type-descr.item-des
    (safe)
  (signatures
   ((<list-of-type-descr>)	=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <vector-type-descr>-rtd)
(declare-core-rcd <vector-type-descr>-rcd)

(declare-core-primitive make-vector-type-descr
    (safe)
  (signatures
   ((<null>)			=> (<core-type-descriptor>))
   ((<list>)			=> (<vector-type-descr>))))

(declare-type-predicate vector-type-descr?	<vector-type-descr>)

(declare-core-primitive vector-type-descr.item-des*
    (safe)
  (signatures
   ((<vector-type-descr>)	=> (<list>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <vector-of-type-descr>-rtd)
(declare-core-rcd <vector-of-type-descr>-rcd)

(declare-core-primitive make-vector-of-type-descr
    (safe)
  (signatures
   ((<top>)			=> (<vector-of-type-descr>))))

(declare-type-predicate vector-of-type-descr?	<vector-of-type-descr>)

(declare-core-primitive vector-of-type-descr.item-des
    (safe)
  (signatures
   ((<vector-of-type-descr>)	=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <enumeration-type-descr>-rtd)
(declare-core-rcd <enumeration-type-descr>-rcd)

(declare-core-primitive make-enumeration-type-descr
    (safe)
  (signatures
   (((list-of <symbol>))		=> (<enumeration-type-descr>))))

(declare-type-predicate enumeration-type-descr?	<enumeration-type-descr>)

(declare-core-primitive enumeration-type-descr.symbol*
    (safe)
  (signatures
   ((<enumeration-type-descr>)		=> ((list-of <symbol>)))))

(declare-core-primitive enumeration-type-descr.length
    (safe)
  (signatures
   ((<enumeration-type-descr>)		=> (<non-negative-exact-integer>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <closure-type-descr>-rtd)
(declare-core-rcd <closure-type-descr>-rcd)

(declare-core-primitive make-closure-type-descr
    (safe)
  (signatures
   ((<case-lambda-descriptors>)		=> (<closure-type-descr>))))

(declare-type-predicate closure-type-descr?	<closure-type-descr>)

(declare-core-primitive closure-type-descr.signature
    (safe)
  (signatures
   ((<closure-type-descr>)		=> (<case-lambda-descriptors>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <union-type-descr>-rtd)
(declare-core-rcd <union-type-descr>-rcd)

(declare-core-primitive make-union-type-descr
    (safe)
  (signatures
   (((list <top>))		=> (<top>))
   ((<list>)			=> (<union-type-descr>))))

(declare-type-predicate union-type-descr?	<union-type-descr>)

(declare-core-primitive union-type-descr.item-des*
    (safe)
  (signatures
   ((<union-type-descr>)	=> (<list>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <intersection-type-descr>-rtd)
(declare-core-rcd <intersection-type-descr>-rcd)

(declare-core-primitive make-intersection-type-descr
    (safe)
  (signatures
   (((list <top>))		=> (<top>))
   ((<list>)			=> (<intersection-type-descr>))))

(declare-type-predicate intersection-type-descr?	<intersection-type-descr>)

(declare-core-primitive intersection-type-descr.item-des*
    (safe)
  (signatures
   ((<intersection-type-descr>)		=> (<list>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <complement-type-descr>-rtd)
(declare-core-rcd <complement-type-descr>-rcd)

(declare-core-primitive make-complement-type-descr
    (safe)
  (signatures
   ((<top>)				=> (<complement-type-descr>))))

(declare-type-predicate complement-type-descr?	<complement-type-descr>)

(declare-core-primitive complement-type-descr.item-des
    (safe)
  (signatures
   ((<complement-type-descr>)		=> (<top>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <ancestor-of-type-descr>-rtd)
(declare-core-rcd <ancestor-of-type-descr>-rcd)

(declare-core-primitive make-ancestor-of-type-descr
    (safe)
  (signatures
   ((<top>)				=> (<ancestor-of-type-descr>))))

(declare-type-predicate ancestor-of-type-descr?		<ancestor-of-type-descr>)

(declare-core-primitive ancestor-of-type-descr.item-des
    (safe)
  (signatures
   ((<ancestor-of-type-descr>)		=> (<top>))))

(declare-core-primitive ancestor-of-type-descr.ancestors-des*
    (safe)
  (signatures
   ((<ancestor-of-type-descr>)		=> (<list>))))


;;;; object type descriptors: utilities

(declare-core-primitive object-type-descr-of
    (safe)
  (signatures
   ((<top>)			=> (<top>))))

(declare-core-primitive object-type-descr=?
    (safe)
  (signatures
   ((<top> <top>)		=> (<boolean>))))

(declare-core-primitive object-type-descr.ancestry-super-and-sub?
    (safe)
  (signatures
   ((<top> <top>)		=> (<boolean>))))

(declare-core-primitive object-type-descr.matching-super-and-sub?
    (safe)
  (signatures
   ((<top> <top>)		=> (<boolean>))))

(declare-core-primitive object-type-descr.ancestors-des*
    (safe)
  (signatures
   ((<top>)			=> (<list>))))

(declare-core-primitive object-type-descr.parent
    (safe)
  (signatures
   ((<top>)			=> (<top>))))


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
