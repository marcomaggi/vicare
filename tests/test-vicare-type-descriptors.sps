;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for object type descriptors
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
(program (test-vicare-type-descriptors)
  (options typed-language)
  (import (vicare)
    (vicare checks)
    (only (psyntax system $all)
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
	  <zero-flonum>-type-descriptor))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: object type descriptors\n")


;;;; helper definitions

(define-record-type <alpha>
  (define-type-descriptors)
  (fields a))

(define-record-type <beta>
  (define-type-descriptors)
  (parent <alpha>)
  (fields b))

(define-record-type <delta>
  (define-type-descriptors)
  (parent <beta>)
  (fields d))

(define-record-type <duo>
  (define-type-descriptors)
  (fields one two))

(define-struct alpha
  (a))

(define-struct beta
  (b))

(define alpha-rtd	(struct-type-descriptor alpha))
(define beta-rtd	(struct-type-descriptor beta))


(parametrise ((check-test-name	'type-of))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   (object-type-descr-of ?expr)
	 (=> object-type-descr=?)
	 ?expected))
      ))

;;; --------------------------------------------------------------------
;;; numbers

  (doit +1			=> <positive-fixnum>-type-descriptor)
  (doit -1			=> <negative-fixnum>-type-descriptor)
  (doit  0			=>     <zero-fixnum>-type-descriptor)

  (doit +0.0			=> <positive-zero-flonum>-type-descriptor)
  (doit -0.0			=> <negative-zero-flonum>-type-descriptor)
  (doit +1.0			=> <positive-flonum>-type-descriptor)
  (doit -1.0			=> <negative-flonum>-type-descriptor)
  (doit +inf.0			=> <positive-flonum>-type-descriptor)
  (doit -inf.0			=> <negative-flonum>-type-descriptor)

  (doit +1/2			=> <positive-ratnum>-type-descriptor)
  (doit -1/2			=> <negative-ratnum>-type-descriptor)

  (doit (least-positive-bignum)		=> <positive-bignum>-type-descriptor)
  (doit (greatest-negative-bignum)	=> <negative-bignum>-type-descriptor)

  (doit 1+2i			=> <exact-compnum>-type-descriptor)
  (doit 0+0.0i			=> <zero-compnum>-type-descriptor)
  (doit 1.2+2i			=> <non-zero-inexact-compnum>-type-descriptor)

  (doit 0.0+0.0i		=> <zero-cflonum>-type-descriptor)
  (doit 1.2+2.3i		=> <non-zero-cflonum>-type-descriptor)

;;; --------------------------------------------------------------------
;;; characters

  (doit #\c			=> <char>-type-descriptor)

;;; --------------------------------------------------------------------
;;; booleans

  (doit #t			=> <true>-type-descriptor)
  (doit #f			=> <false>-type-descriptor)

;;; --------------------------------------------------------------------
;;; strings

  (doit "ciao"			=> <nestring>-type-descriptor)
  (doit ""			=> <empty-string>-type-descriptor)

;;; --------------------------------------------------------------------
;;; enumerations

  (doit 'ciao			=> (make-enumeration-type-descr '(ciao)))

;;; --------------------------------------------------------------------
;;; keywords

  (doit #:ciao			=> <keyword>-type-descriptor)

;;; --------------------------------------------------------------------
;;; pairs and lists

  (doit '()			=> <null>-type-descriptor)

  (doit '(1 2)			=> (make-list-type-descr (list <positive-fixnum>-type-descriptor
							       <positive-fixnum>-type-descriptor)))

  (doit '(1 . 2)		=> (make-pair-type-descr <positive-fixnum>-type-descriptor
							 <positive-fixnum>-type-descriptor))

  (doit '(1 2 . 3)		=> (make-pair-type-descr <positive-fixnum>-type-descriptor
							 (make-pair-type-descr <positive-fixnum>-type-descriptor
									       <positive-fixnum>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; vectors

  (doit '#(1 2 3)		=> (make-vector-type-descr (list <positive-fixnum>-type-descriptor
								 <positive-fixnum>-type-descriptor
								 <positive-fixnum>-type-descriptor)))
  (doit '#()			=> <empty-vector>-type-descriptor)


;;; --------------------------------------------------------------------
;;; bytevectors

  (doit '#vu8(1 2 3)		=> <nebytevector>-type-descriptor)
  (doit '#vu8()			=> <empty-bytevector>-type-descriptor)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'equality-super-and-sub))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(object-type-descr=? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-false
	(object-type-descr=? ?super ?sub)))
      ))

;;; --------------------------------------------------------------------
;;; built-in types

  (doit-true	<top>-type-descriptor		<top>-type-descriptor)
  (doit-false	<top>-type-descriptor		<number>-type-descriptor)
  (doit-false	<number>-type-descriptor	<top>-type-descriptor)

  (doit-false	<top>-type-descriptor		<struct>-type-descriptor)
  (doit-false	<top>-type-descriptor		<record>-type-descriptor)
  (doit-false	<struct>-type-descriptor	<record>-type-descriptor)

  (doit-true	<struct>-type-descriptor	<struct>-type-descriptor)
  (doit-true	<record>-type-descriptor	<record>-type-descriptor)

  (doit-false	<list>-type-descriptor		<null>-type-descriptor)
  (doit-false	<nelist>-type-descriptor	<null>-type-descriptor)

  (doit-true	<list>-type-descriptor		<list>-type-descriptor)
  (doit-true	<null>-type-descriptor		<null>-type-descriptor)
  (doit-true	<nelist>-type-descriptor	<nelist>-type-descriptor)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	alpha-rtd			alpha-rtd)
  (doit-true	beta-rtd			beta-rtd)

  (doit-false	<top>-type-descriptor		alpha-rtd)
  (doit-false	<struct>-type-descriptor	alpha-rtd)
  (doit-false	alpha-rtd			<top>-type-descriptor)
  (doit-false	alpha-rtd			<struct>-type-descriptor)
  (doit-false	alpha-rtd			beta-rtd)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-false	<top>-type-descriptor		<duo>-rtd)
  (doit-false	<struct>-type-descriptor	<duo>-rtd)
  (doit-false	<record>-type-descriptor	<duo>-rtd)
  (doit-false	<duo>-rtd			<top>-type-descriptor)
  (doit-false	<duo>-rtd			<struct>-type-descriptor)
  (doit-false	<duo>-rtd			<record>-type-descriptor)

  (doit-true	<alpha>-rtd			<alpha>-rtd)
  (doit-true	<beta>-rtd			<beta>-rtd)
  (doit-true	<delta>-rtd			<delta>-rtd)

  (doit-false	<alpha>-rtd			<beta>-rtd)
  (doit-false	<beta>-rtd			<delta>-rtd)
  (doit-false	<alpha>-rtd			<delta>-rtd)
  (doit-false	<beta>-rtd			<alpha>-rtd)
  (doit-false	<delta>-rtd			<beta>-rtd)
  (doit-false	<delta>-rtd			<alpha>-rtd)
  (doit-false	<alpha>-rtd			<duo>-rtd)
  (doit-false	<duo>-rtd			<alpha>-rtd)

;;; --------------------------------------------------------------------
;;; pair

  (doit-false	<pair>-type-descriptor
		(make-pair-type-descr <fixnum>-type-descriptor <flonum>-type-descriptor))

  (doit-false	(make-pair-type-descr <top>-type-descriptor <null>-type-descriptor)
		<pair>-type-descriptor)

  (doit-true	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-pair-type-descr <number>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-pair-type-descr <string>-type-descriptor <number>-type-descriptor)
		(make-pair-type-descr <string>-type-descriptor <fixnum>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <number>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-pair-type-descr <string>-type-descriptor <fixnum>-type-descriptor)
		(make-pair-type-descr <string>-type-descriptor <number>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <symbol>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <symbol>-type-descriptor))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-false	<pair>-type-descriptor
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <fixnum>-type-descriptor)
		<pair>-type-descriptor)

  (doit-true	(make-pair-of-type-descr <fixnum>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <number>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <fixnum>-type-descriptor)
		(make-pair-of-type-descr <number>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <fixnum>-type-descriptor)
		(make-pair-of-type-descr <string>-type-descriptor))

;;; --------------------------------------------------------------------
;;; list

  (doit-false	<list>-type-descriptor
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	<nelist>-type-descriptor
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		<nelist>-type-descriptor)

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		<null>-type-descriptor)

;;; list/list

  (doit-true	(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor <number>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <string>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-false	<list>-type-descriptor
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	<nelist>-type-descriptor
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-list-of-type-descr <fixnum>-type-descriptor)
		<null>-type-descriptor)

  (doit-false	(make-list-of-type-descr <top>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-false	(make-list-of-type-descr <top>-type-descriptor)
		<list>-type-descriptor)

;;; list-of/list-of

  (doit-true	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-list-of-type-descr <number>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-list-of-type-descr <number>-type-descriptor))

  (doit-false	(make-list-of-type-descr <string>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; vector

  (doit-false	<vector>-type-descriptor
		(make-vector-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	<nevector>-type-descriptor
		(make-vector-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	(make-vector-type-descr (list <top>-type-descriptor))
		<nevector>-type-descriptor)

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor))
		<nevector>-type-descriptor)

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor))
		<null>-type-descriptor)

;;; vector/vector

  (doit-true	(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor <number>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <string>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-false	<vector>-type-descriptor
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-false	<nevector>-type-descriptor
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-vector-of-type-descr <fixnum>-type-descriptor)
		<empty-vector>-type-descriptor)

  (doit-false	(make-vector-of-type-descr <top>-type-descriptor)
		<nevector>-type-descriptor)

  (doit-false	(make-vector-of-type-descr <top>-type-descriptor)
		<vector>-type-descriptor)

;;; vector-of/vector-of

  (doit-true	(make-vector-of-type-descr <fixnum>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-vector-of-type-descr <number>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-vector-of-type-descr <fixnum>-type-descriptor)
		(make-vector-of-type-descr <number>-type-descriptor))

  (doit-false	(make-vector-of-type-descr <string>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(ciao)))

  (doit-false	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello)))

  (doit-false	(make-enumeration-type-descr '(ciao hello))
		(make-enumeration-type-descr '(hello)))

  (doit-false	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello ciao)))

;;; --------------------------------------------------------------------
;;; union

  (doit-false	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<fixnum>-type-descriptor)

  (doit-false	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<flonum>-type-descriptor)

  (doit-false	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<string>-type-descriptor)

  (doit-true	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-false	(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor))
		<flonum>-type-descriptor)

  (doit-false	(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor))
		<string>-type-descriptor)

  (doit-true	(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor))
		(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; complement

  (doit-false	(make-complement-type-descr <flonum>-type-descriptor)
		<fixnum>-type-descriptor)

  (doit-false	<fixnum>-type-descriptor
		(make-complement-type-descr <flonum>-type-descriptor))

  (doit-false	<fixnum>-type-descriptor
		(make-complement-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-complement-type-descr <fixnum>-type-descriptor)
		(make-complement-type-descr <fixnum>-type-descriptor))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'ancestry-super-and-sub))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(object-type-descr.ancestry-super-and-sub? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-false
	(object-type-descr.ancestry-super-and-sub? ?super ?sub)))
      ))

;;; --------------------------------------------------------------------
;;; built-in types

  (doit-true	<top>-type-descriptor		<top>-type-descriptor)
  (doit-true	<top>-type-descriptor		<number>-type-descriptor)
  (doit-false	<number>-type-descriptor	<top>-type-descriptor)

  (doit-true	<top>-type-descriptor		<struct>-type-descriptor)
  (doit-true	<top>-type-descriptor		<record>-type-descriptor)
  (doit-true	<struct>-type-descriptor	<record>-type-descriptor)

  (doit-true	<list>-type-descriptor		<null>-type-descriptor)
  (doit-false	<nelist>-type-descriptor	<null>-type-descriptor)

  (doit-true	<vector>-type-descriptor	<empty-vector>-type-descriptor)
  (doit-false	<nevector>-type-descriptor	<empty-vector>-type-descriptor)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	<top>-type-descriptor		alpha-rtd)
  (doit-true	<struct>-type-descriptor	alpha-rtd)
  (doit-false	alpha-rtd			<top>-type-descriptor)
  (doit-false	alpha-rtd			<struct>-type-descriptor)
  (doit-false	alpha-rtd			beta-rtd)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-true	<top>-type-descriptor		<duo>-rtd)
  (doit-true	<struct>-type-descriptor	<duo>-rtd)
  (doit-true	<record>-type-descriptor	<duo>-rtd)
  (doit-false	<duo>-rtd			<top>-type-descriptor)
  (doit-false	<duo>-rtd			<struct>-type-descriptor)
  (doit-false	<duo>-rtd			<record>-type-descriptor)

  (doit-true	<alpha>-rtd			<beta>-rtd)
  (doit-true	<beta>-rtd			<delta>-rtd)
  (doit-true	<alpha>-rtd			<delta>-rtd)
  (doit-false	<beta>-rtd			<alpha>-rtd)
  (doit-false	<delta>-rtd			<beta>-rtd)
  (doit-false	<delta>-rtd			<alpha>-rtd)
  (doit-false	<alpha>-rtd			<duo>-rtd)
  (doit-false	<duo>-rtd			<alpha>-rtd)

;;; --------------------------------------------------------------------
;;; pair

  (doit-true	<pair>-type-descriptor
		(make-pair-type-descr <fixnum>-type-descriptor <flonum>-type-descriptor))

  (doit-false	(make-pair-type-descr <top>-type-descriptor <null>-type-descriptor)
		<pair>-type-descriptor)

  (doit-true	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor))

  (doit-true	(make-pair-type-descr <number>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor))

  (doit-true	(make-pair-type-descr <string>-type-descriptor <number>-type-descriptor)
		(make-pair-type-descr <string>-type-descriptor <fixnum>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <number>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-pair-type-descr <string>-type-descriptor <fixnum>-type-descriptor)
		(make-pair-type-descr <string>-type-descriptor <number>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <symbol>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <symbol>-type-descriptor))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-true	<pair>-type-descriptor
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <fixnum>-type-descriptor)
		<pair>-type-descriptor)

  (doit-true	(make-pair-of-type-descr <fixnum>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-pair-of-type-descr <number>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <fixnum>-type-descriptor)
		(make-pair-of-type-descr <number>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <fixnum>-type-descriptor)
		(make-pair-of-type-descr <string>-type-descriptor))

;;; --------------------------------------------------------------------
;;; list

  (doit-true	<list>-type-descriptor
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	<nelist>-type-descriptor
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		<nelist>-type-descriptor)

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		<null>-type-descriptor)

;;; list/list

  (doit-true	(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-true	(make-list-type-descr (list <fixnum>-type-descriptor <number>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <string>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-true	<list>-type-descriptor
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	<nelist>-type-descriptor
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-list-of-type-descr <fixnum>-type-descriptor)
		<null>-type-descriptor)

  (doit-false	(make-list-of-type-descr <top>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-false	(make-list-of-type-descr <top>-type-descriptor)
		<list>-type-descriptor)

;;; list-of/list-of

  (doit-true	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-list-of-type-descr <number>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-list-of-type-descr <number>-type-descriptor))

  (doit-false	(make-list-of-type-descr <string>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; vector

  (doit-true	<vector>-type-descriptor
		(make-vector-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	<nevector>-type-descriptor
		(make-vector-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	(make-vector-type-descr (list <top>-type-descriptor))
		<nevector>-type-descriptor)

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor))
		<nevector>-type-descriptor)

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor))
		<null>-type-descriptor)

;;; vector/vector

  (doit-true	(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-true	(make-vector-type-descr (list <fixnum>-type-descriptor <number>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <string>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-true	<vector>-type-descriptor
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-false	<nevector>-type-descriptor
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-vector-of-type-descr <fixnum>-type-descriptor)
		<empty-vector>-type-descriptor)

  (doit-false	(make-vector-of-type-descr <top>-type-descriptor)
		<nevector>-type-descriptor)

  (doit-false	(make-vector-of-type-descr <top>-type-descriptor)
		<vector>-type-descriptor)

;;; vector-of/vector-of

  (doit-true	(make-vector-of-type-descr <fixnum>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-vector-of-type-descr <number>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-vector-of-type-descr <fixnum>-type-descriptor)
		(make-vector-of-type-descr <number>-type-descriptor))

  (doit-false	(make-vector-of-type-descr <string>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(ciao)))

  (doit-true	<symbol>-type-descriptor
		(make-enumeration-type-descr '(ciao)))

  (doit-false	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello)))

  (doit-false	(make-enumeration-type-descr '(ciao hello))
		(make-enumeration-type-descr '(hello)))

  (doit-false	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello ciao)))

;;; --------------------------------------------------------------------
;;; union

  (doit-false	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<fixnum>-type-descriptor)

  (doit-false	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<flonum>-type-descriptor)

  (doit-false	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<string>-type-descriptor)

  (doit-true	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-false	(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor))
		<flonum>-type-descriptor)

  (doit-false	(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor))
		<string>-type-descriptor)

  (doit-true	(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor))
		(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; complement

  (doit-false	(make-complement-type-descr <flonum>-type-descriptor)
		<fixnum>-type-descriptor)

  (doit-false	<fixnum>-type-descriptor
		(make-complement-type-descr <flonum>-type-descriptor))

  (doit-false	<fixnum>-type-descriptor
		(make-complement-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-complement-type-descr <fixnum>-type-descriptor)
		(make-complement-type-descr <fixnum>-type-descriptor))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'matching-super-and-sub))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(object-type-descr.matching-super-and-sub? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-false
	(object-type-descr.matching-super-and-sub? ?super ?sub)))
      ))

;;; --------------------------------------------------------------------
;;; built-in types

  (doit-true	<top>-type-descriptor		<top>-type-descriptor)
  (doit-true	<top>-type-descriptor		<number>-type-descriptor)
  (doit-false	<number>-type-descriptor	<top>-type-descriptor)

  (doit-true	<top>-type-descriptor		<struct>-type-descriptor)
  (doit-true	<top>-type-descriptor		<record>-type-descriptor)
  (doit-true	<struct>-type-descriptor	<record>-type-descriptor)

  (doit-true	<list>-type-descriptor		<null>-type-descriptor)
  (doit-false	<nelist>-type-descriptor	<null>-type-descriptor)

  (doit-true	<vector>-type-descriptor	<empty-vector>-type-descriptor)
  (doit-false	<nevector>-type-descriptor	<empty-vector>-type-descriptor)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	<top>-type-descriptor		alpha-rtd)
  (doit-true	<struct>-type-descriptor	alpha-rtd)
  (doit-false	alpha-rtd			<top>-type-descriptor)
  (doit-false	alpha-rtd			<struct>-type-descriptor)
  (doit-false	alpha-rtd			beta-rtd)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-true	<top>-type-descriptor		<duo>-rtd)
  (doit-true	<struct>-type-descriptor	<duo>-rtd)
  (doit-true	<record>-type-descriptor	<duo>-rtd)
  (doit-false	<duo>-rtd			<top>-type-descriptor)
  (doit-false	<duo>-rtd			<struct>-type-descriptor)
  (doit-false	<duo>-rtd			<record>-type-descriptor)

  (doit-true	<alpha>-rtd			<beta>-rtd)
  (doit-true	<beta>-rtd			<delta>-rtd)
  (doit-true	<alpha>-rtd			<delta>-rtd)
  (doit-false	<beta>-rtd			<alpha>-rtd)
  (doit-false	<delta>-rtd			<beta>-rtd)
  (doit-false	<delta>-rtd			<alpha>-rtd)
  (doit-false	<alpha>-rtd			<duo>-rtd)
  (doit-false	<duo>-rtd			<alpha>-rtd)

;;; --------------------------------------------------------------------
;;; pair

  (doit-true	<pair>-type-descriptor
		(make-pair-type-descr <fixnum>-type-descriptor <flonum>-type-descriptor))

  (doit-true	(make-pair-type-descr <top>-type-descriptor <null>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-true	(make-pair-type-descr <top>-type-descriptor <null>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-true	(make-pair-type-descr <top>-type-descriptor <null>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-true	(make-pair-type-descr <top>-type-descriptor <list>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <list>-type-descriptor)
		<nelist>-type-descriptor)

;;; pair/pair

  (doit-true	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor))

  (doit-true	(make-pair-type-descr <number>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor))

  (doit-true	(make-pair-type-descr <string>-type-descriptor <number>-type-descriptor)
		(make-pair-type-descr <string>-type-descriptor <fixnum>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <number>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-pair-type-descr <string>-type-descriptor <fixnum>-type-descriptor)
		(make-pair-type-descr <string>-type-descriptor <number>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <symbol>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <symbol>-type-descriptor))

;;; pair/pair-of

  (doit-true	(make-pair-type-descr    <fixnum>-type-descriptor <number>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-type-descr    <string>-type-descriptor <number>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-type-descr    <fixnum>-type-descriptor <string>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

;;; pair/list

  (doit-true	(make-pair-type-descr <fixnum>-type-descriptor <null>-type-descriptor)
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor <flonum>-type-descriptor)
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	(make-pair-type-descr <fixnum>-type-descriptor (make-list-of-type-descr <fixnum>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor (make-list-type-descr (list <fixnum>-type-descriptor)))
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	(make-pair-type-descr <fixnum>-type-descriptor (make-list-type-descr (list <fixnum>-type-descriptor)))
		(make-list-type-descr (list <fixnum>-type-descriptor <fixnum>-type-descriptor)))

;;; pair/list-of

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor (make-list-of-type-descr <fixnum>-type-descriptor))
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-type-descr <fixnum>-type-descriptor (make-list-type-descr (list <fixnum>-type-descriptor)))
		(make-list-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-true	<pair>-type-descriptor
		(make-pair-of-type-descr <fixnum>-type-descriptor))

;;;

  (doit-true	(make-pair-of-type-descr <null>-type-descriptor)
		<list>-type-descriptor)

  (doit-true	(make-pair-of-type-descr <list>-type-descriptor)
		<list>-type-descriptor)

  (doit-false	(make-pair-of-type-descr <fixnum>-type-descriptor)
		<list>-type-descriptor)

;;;

  (doit-true	(make-pair-of-type-descr <null>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-true	(make-pair-of-type-descr <list>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-false	(make-pair-of-type-descr <fixnum>-type-descriptor)
		<nelist>-type-descriptor)

;;; pair-of/pair

  (doit-true	(make-pair-of-type-descr <list>-type-descriptor)
		(make-pair-type-descr <list>-type-descriptor <list>-type-descriptor))

  (doit-true	(make-pair-of-type-descr (make-union-type-descr (list <number>-type-descriptor <list>-type-descriptor)))
		(make-pair-type-descr <fixnum>-type-descriptor <list>-type-descriptor))

;;; pair-of/pair-of

  (doit-true	(make-pair-of-type-descr <number>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <number>-type-descriptor)
		(make-pair-of-type-descr <string>-type-descriptor))

;;; pair-of/list

  (doit-true	(make-pair-of-type-descr <list>-type-descriptor)
		(make-list-type-descr (list <list>-type-descriptor <list>-type-descriptor)))

  (doit-true	(make-pair-of-type-descr (make-union-type-descr (list <number>-type-descriptor <list>-type-descriptor)))
		(make-list-type-descr (list <fixnum>-type-descriptor <list>-type-descriptor)))

;;; pair-of/list-of

  (doit-true	(make-pair-of-type-descr <number>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-pair-of-type-descr <number>-type-descriptor)
		(make-list-of-type-descr <string>-type-descriptor))

;;; --------------------------------------------------------------------
;;; list

  (doit-true	<list>-type-descriptor
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	<nelist>-type-descriptor
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	(make-list-type-descr (list <top>-type-descriptor))
		<nelist>-type-descriptor)

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		<nelist>-type-descriptor)

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		<null>-type-descriptor)

;;; list/pair

  (doit-true	(make-list-type-descr (list <fixnum>-type-descriptor))
		(make-pair-type-descr <fixnum>-type-descriptor <null>-type-descriptor))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		(make-pair-type-descr <fixnum>-type-descriptor <string>-type-descriptor))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-pair-type-descr <fixnum>-type-descriptor <null>-type-descriptor))

;;; list/pair-of

  (doit-true	(make-list-type-descr (list <null>-type-descriptor))
		(make-pair-of-type-descr <null>-type-descriptor))

  (doit-true	(make-list-type-descr (list <list>-type-descriptor))
		(make-pair-of-type-descr <null>-type-descriptor))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		(make-pair-of-type-descr <list>-type-descriptor))

  (doit-true	(make-list-type-descr (list (make-list-type-descr (list <flonum>-type-descriptor)) <flonum>-type-descriptor))
		(make-pair-of-type-descr (make-list-type-descr (list <flonum>-type-descriptor))))

;;; list/list

  (doit-true	(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-true	(make-list-type-descr (list <fixnum>-type-descriptor <number>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-list-type-descr (list <fixnum>-type-descriptor <string>-type-descriptor)))

;;; list/list-of

  (doit-false	(make-list-type-descr (list <fixnum>-type-descriptor))
		(make-list-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-true	<list>-type-descriptor
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	<nelist>-type-descriptor
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-list-of-type-descr <fixnum>-type-descriptor)
		<null>-type-descriptor)

  (doit-true	(make-list-of-type-descr <top>-type-descriptor)
		<nelist>-type-descriptor)

  (doit-true	(make-list-of-type-descr <top>-type-descriptor)
		<list>-type-descriptor)

;;; list-of/pair

  (doit-true	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor <null>-type-descriptor))

  (doit-true	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor (make-list-of-type-descr <fixnum>-type-descriptor)))

  (doit-true	(make-list-of-type-descr <number>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor (make-list-type-descr (list <flonum>-type-descriptor))))

  (doit-false	(make-list-of-type-descr <number>-type-descriptor)
		(make-pair-type-descr <fixnum>-type-descriptor (make-list-type-descr (list <string>-type-descriptor))))

  (doit-false	(make-list-of-type-descr <number>-type-descriptor)
		(make-pair-type-descr <string>-type-descriptor <null>-type-descriptor))

;;; list-of/pair-of

  (doit-true	(make-list-of-type-descr <null>-type-descriptor)
		(make-pair-of-type-descr <null>-type-descriptor))

  (doit-true	(make-list-of-type-descr <list>-type-descriptor)
		(make-pair-of-type-descr <list>-type-descriptor))

  (doit-false	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-pair-of-type-descr <fixnum>-type-descriptor))

;;; list-of/list

  (doit-true	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	(make-list-of-type-descr <number>-type-descriptor)
		(make-list-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-list-type-descr (list <number>-type-descriptor)))

;;; list-of/list-of

  (doit-true	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-list-of-type-descr <number>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-list-of-type-descr <fixnum>-type-descriptor)
		(make-list-of-type-descr <number>-type-descriptor))

  (doit-false	(make-list-of-type-descr <string>-type-descriptor)
		(make-list-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; vector

  (doit-true	<vector>-type-descriptor
		(make-vector-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	<nevector>-type-descriptor
		(make-vector-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	(make-vector-type-descr (list <top>-type-descriptor))
		<nevector>-type-descriptor)

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor))
		<nevector>-type-descriptor)

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor))
		<null>-type-descriptor)

;;; vector/vector

  (doit-true	(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-true	(make-vector-type-descr (list <fixnum>-type-descriptor <number>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-vector-type-descr (list <fixnum>-type-descriptor <string>-type-descriptor)))

;;; vector/vector-of

  (doit-false	(make-vector-type-descr (list <fixnum>-type-descriptor))
		(make-vector-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-true	<vector>-type-descriptor
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-false	<nevector>-type-descriptor
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-vector-of-type-descr <fixnum>-type-descriptor)
		<empty-vector>-type-descriptor)

  (doit-true	(make-vector-of-type-descr <top>-type-descriptor)
		<nevector>-type-descriptor)

  (doit-true	(make-vector-of-type-descr <top>-type-descriptor)
		<vector>-type-descriptor)

;;; vector-of/vector

  (doit-true	(make-vector-of-type-descr <fixnum>-type-descriptor)
		(make-vector-type-descr (list <fixnum>-type-descriptor)))

  (doit-true	(make-vector-of-type-descr <number>-type-descriptor)
		(make-vector-type-descr (list <fixnum>-type-descriptor)))

  (doit-false	(make-vector-of-type-descr <fixnum>-type-descriptor)
		(make-vector-type-descr (list <number>-type-descriptor)))

;;; vector-of/vector-of

  (doit-true	(make-vector-of-type-descr <fixnum>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-true	(make-vector-of-type-descr <number>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

  (doit-false	(make-vector-of-type-descr <fixnum>-type-descriptor)
		(make-vector-of-type-descr <number>-type-descriptor))

  (doit-false	(make-vector-of-type-descr <string>-type-descriptor)
		(make-vector-of-type-descr <fixnum>-type-descriptor))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(ciao)))

  (doit-true	<symbol>-type-descriptor
		(make-enumeration-type-descr '(ciao)))

  (doit-false	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello)))

  (doit-false	(make-enumeration-type-descr '(ciao hello))
		(make-enumeration-type-descr '(hello)))

  (doit-true	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello ciao)))

;;; --------------------------------------------------------------------
;;; union

  (doit-true	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<fixnum>-type-descriptor)

  (doit-true	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<flonum>-type-descriptor)

  (doit-false	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		<string>-type-descriptor)

  (doit-true	(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor))
		(make-union-type-descr (list <fixnum>-type-descriptor <flonum>-type-descriptor)))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-true	(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor))
		<flonum>-type-descriptor)

  (doit-false	(make-intersection-type-descr (list <number>-type-descriptor <flonum>-type-descriptor))
		<string>-type-descriptor)

;;; --------------------------------------------------------------------
;;; complement

  (doit-true	(make-complement-type-descr <flonum>-type-descriptor)
		<fixnum>-type-descriptor)

  (doit-true	<fixnum>-type-descriptor
		(make-complement-type-descr <flonum>-type-descriptor))

  (doit-false	<fixnum>-type-descriptor
		(make-complement-type-descr <fixnum>-type-descriptor))

  #| end of PARAMETRISE |# )


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
