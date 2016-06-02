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


;;;; <scheme-type-descriptor> record, safe procedures

(section

(declare-type-predicate scheme-type-descriptor?		<scheme-type-descriptor>)

(declare-core-primitive scheme-type-descriptor-name
    (safe)
  (signatures
   ((<scheme-type-descriptor>)		=> (<symbol>))))

(declare-core-primitive scheme-type-descriptor-parent
    (safe)
  (signatures
   ((<scheme-type-descriptor>)		=> ((or <false> <scheme-type-descriptor>)))))

(declare-core-primitive scheme-type-descriptor-uids-list
    (safe)
  (signatures
   ((<scheme-type-descriptor>)		=> ((list-of <symbol>)))))

(declare-core-primitive scheme-type-descriptor-method-retriever
    (safe)
  (signatures
   ((<scheme-type-descriptor>)		=> (<procedure>))))

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


;;;; Scheme build-tin object-type descriptors

(declare-core-scheme-type-descriptor <bignum>-type-descriptor)
(declare-core-scheme-type-descriptor <binary-input/output-port>-type-descriptor)
(declare-core-scheme-type-descriptor <binary-input-port>-type-descriptor)
(declare-core-scheme-type-descriptor <binary-output-port>-type-descriptor)
(declare-core-scheme-type-descriptor <binary-port>-type-descriptor)
(declare-core-scheme-type-descriptor <boolean>-type-descriptor)
(declare-core-scheme-type-descriptor <bytevector>-type-descriptor)
(declare-core-scheme-type-descriptor <cflonum>-type-descriptor)
(declare-core-scheme-type-descriptor <char>-type-descriptor)
(declare-core-scheme-type-descriptor <code>-type-descriptor)
(declare-core-scheme-type-descriptor <complex>-type-descriptor)
(declare-core-scheme-type-descriptor <compnum>-type-descriptor)
(declare-core-scheme-type-descriptor <compound-condition>-type-descriptor)
(declare-core-scheme-type-descriptor <condition>-type-descriptor)
(declare-core-scheme-type-descriptor <empty-vector>-type-descriptor)
(declare-core-scheme-type-descriptor <enum-set>-type-descriptor)
(declare-core-scheme-type-descriptor <eof>-type-descriptor)
(declare-core-scheme-type-descriptor <exact-compnum>-type-descriptor)
(declare-core-scheme-type-descriptor <inexact-compnum>-type-descriptor)
(declare-core-scheme-type-descriptor <exact-integer>-type-descriptor)
(declare-core-scheme-type-descriptor <false>-type-descriptor)
(declare-core-scheme-type-descriptor <fixnum>-type-descriptor)
(declare-core-scheme-type-descriptor <flonum>-type-descriptor)
(declare-core-scheme-type-descriptor <hashtable-eq>-type-descriptor)
(declare-core-scheme-type-descriptor <hashtable-equal>-type-descriptor)
(declare-core-scheme-type-descriptor <hashtable-eqv>-type-descriptor)
(declare-core-scheme-type-descriptor <hashtable>-type-descriptor)
(declare-core-scheme-type-descriptor <input/output-port>-type-descriptor)
(declare-core-scheme-type-descriptor <input-port>-type-descriptor)
(declare-core-scheme-type-descriptor <integer>-type-descriptor)
(declare-core-scheme-type-descriptor <integer-valued>-type-descriptor)
(declare-core-scheme-type-descriptor <ipair>-type-descriptor)
(declare-core-scheme-type-descriptor <keyword>-type-descriptor)
(declare-core-scheme-type-descriptor <list>-type-descriptor)
(declare-core-scheme-type-descriptor <nelist>-type-descriptor)
;;
(declare-core-scheme-type-descriptor <negative-bignum>-type-descriptor)
(declare-core-scheme-type-descriptor <negative-fixnum>-type-descriptor)
(declare-core-scheme-type-descriptor <negative-flonum>-type-descriptor)
(declare-core-scheme-type-descriptor <negative-ratnum>-type-descriptor)
(declare-core-scheme-type-descriptor <negative-zero-flonum>-type-descriptor)
(declare-core-scheme-type-descriptor <no-return>-type-descriptor)
(declare-core-scheme-type-descriptor <null>-type-descriptor)
(declare-core-scheme-type-descriptor <number>-type-descriptor)
(declare-core-scheme-type-descriptor <opaque-record>-type-descriptor)
(declare-core-scheme-type-descriptor <output-port>-type-descriptor)
(declare-core-scheme-type-descriptor <pair>-type-descriptor)
(declare-core-scheme-type-descriptor <pointer>-type-descriptor)
(declare-core-scheme-type-descriptor <port>-type-descriptor)
(declare-core-scheme-type-descriptor <positive-bignum>-type-descriptor)
(declare-core-scheme-type-descriptor <positive-fixnum>-type-descriptor)
(declare-core-scheme-type-descriptor <positive-flonum>-type-descriptor)
(declare-core-scheme-type-descriptor <positive-ratnum>-type-descriptor)
(declare-core-scheme-type-descriptor <positive-zero-flonum>-type-descriptor)
(declare-core-scheme-type-descriptor <procedure>-type-descriptor)
(declare-core-scheme-type-descriptor <promise>-type-descriptor)
(declare-core-scheme-type-descriptor <rational>-type-descriptor)
(declare-core-scheme-type-descriptor <rational-valued>-type-descriptor)
(declare-core-scheme-type-descriptor <ratnum>-type-descriptor)
(declare-core-scheme-type-descriptor <real>-type-descriptor)
(declare-core-scheme-type-descriptor <real-valued>-type-descriptor)
(declare-core-scheme-type-descriptor <record-constructor-descriptor>-type-descriptor)
(declare-core-scheme-type-descriptor <record>-type-descriptor)
(declare-core-scheme-type-descriptor <record-type-descriptor>-type-descriptor)
(declare-core-scheme-type-descriptor <string>-type-descriptor)
(declare-core-scheme-type-descriptor <struct>-type-descriptor)
(declare-core-scheme-type-descriptor <struct-type-descriptor>-type-descriptor)
(declare-core-scheme-type-descriptor <symbol>-type-descriptor)
(declare-core-scheme-type-descriptor <gensym>-type-descriptor)
(declare-core-scheme-type-descriptor <textual-input/output-port>-type-descriptor)
(declare-core-scheme-type-descriptor <textual-input-port>-type-descriptor)
(declare-core-scheme-type-descriptor <textual-output-port>-type-descriptor)
(declare-core-scheme-type-descriptor <textual-port>-type-descriptor)
(declare-core-scheme-type-descriptor <top>-type-descriptor)
(declare-core-scheme-type-descriptor <transcoder>-type-descriptor)
(declare-core-scheme-type-descriptor <true>-type-descriptor)
(declare-core-scheme-type-descriptor <utsname>-type-descriptor)
(declare-core-scheme-type-descriptor <vector>-type-descriptor)
(declare-core-scheme-type-descriptor <nevector>-type-descriptor)
(declare-core-scheme-type-descriptor <void>-type-descriptor)
(declare-core-scheme-type-descriptor <would-block>-type-descriptor)
(declare-core-scheme-type-descriptor <zero-fixnum>-type-descriptor)
;;
(declare-core-scheme-type-descriptor <memory-block>-type-descriptor)
(declare-core-scheme-type-descriptor <time>-type-descriptor)
(declare-core-scheme-type-descriptor <reader-annotation>-type-descriptor)
(declare-core-scheme-type-descriptor <scheme-type-descriptor>-type-descriptor)
(declare-core-scheme-type-descriptor <stats>-type-descriptor)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
