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
    (vicare system type-descriptors))

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

  (doit +1			=> <positive-fixnum>-ctd)
  (doit -1			=> <negative-fixnum>-ctd)
  (doit  0			=>     <zero-fixnum>-ctd)

  (doit +0.0			=> <positive-zero-flonum>-ctd)
  (doit -0.0			=> <negative-zero-flonum>-ctd)
  (doit +1.0			=> <positive-flonum>-ctd)
  (doit -1.0			=> <negative-flonum>-ctd)
  (doit +inf.0			=> <positive-flonum>-ctd)
  (doit -inf.0			=> <negative-flonum>-ctd)

  (doit +1/2			=> <positive-ratnum>-ctd)
  (doit -1/2			=> <negative-ratnum>-ctd)

  (doit (least-positive-bignum)		=> <positive-bignum>-ctd)
  (doit (greatest-negative-bignum)	=> <negative-bignum>-ctd)

  (doit 1+2i			=> <exact-compnum>-ctd)
  (doit 0+0.0i			=> <zero-compnum>-ctd)
  (doit 1.2+2i			=> <non-zero-inexact-compnum>-ctd)

  (doit 0.0+0.0i		=> <zero-cflonum>-ctd)
  (doit 1.2+2.3i		=> <non-zero-cflonum>-ctd)

;;; --------------------------------------------------------------------
;;; characters

  (doit #\c			=> <char>-ctd)

;;; --------------------------------------------------------------------
;;; booleans

  (doit #t			=> <true>-ctd)
  (doit #f			=> <false>-ctd)

;;; --------------------------------------------------------------------
;;; strings

  (doit "ciao"			=> <nestring>-ctd)
  (doit ""			=> <empty-string>-ctd)

;;; --------------------------------------------------------------------
;;; enumerations

  (doit 'ciao			=> (make-enumeration-type-descr '(ciao)))

;;; --------------------------------------------------------------------
;;; keywords

  (doit #:ciao			=> <keyword>-ctd)

;;; --------------------------------------------------------------------
;;; pairs and lists

  (doit '()			=> <null>-ctd)

  (doit '(1 2)			=> (make-list-type-descr (list <positive-fixnum>-ctd
							       <positive-fixnum>-ctd)))

  (doit '(1 . 2)		=> (make-pair-type-descr <positive-fixnum>-ctd
							 <positive-fixnum>-ctd))

  (doit '(1 2 . 3)		=> (make-pair-type-descr <positive-fixnum>-ctd
							 (make-pair-type-descr <positive-fixnum>-ctd
									       <positive-fixnum>-ctd)))

;;; --------------------------------------------------------------------
;;; vectors

  (doit '#(1 2 3)		=> (make-vector-type-descr (list <positive-fixnum>-ctd
								 <positive-fixnum>-ctd
								 <positive-fixnum>-ctd)))
  (doit '#()			=> <empty-vector>-ctd)


;;; --------------------------------------------------------------------
;;; bytevectors

  (doit '#vu8(1 2 3)		=> <nebytevector>-ctd)
  (doit '#vu8()			=> <empty-bytevector>-ctd)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'type-descriptor-syntax))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?type-annotation => ?expected)
       (check
	   (type-descriptor ?type-annotation)
	 (=> object-type-descr=?)
	 ?expected))
      ))

;;; --------------------------------------------------------------------
;;; core

  (doit <fixnum>		=> <fixnum>-ctd)
  (doit <string>		=> <string>-ctd)

  (doit <vector>		=> <vector>-ctd)
  (doit <list>			=> <list>-ctd)

;;; --------------------------------------------------------------------
;;; records

  (doit <alpha>
	=> (record-type-descriptor <alpha>))

  (doit &who
	=> (record-type-descriptor &who))

;;; --------------------------------------------------------------------
;;; structs

  (doit alpha
	=> (struct-type-descriptor alpha))

;;; --------------------------------------------------------------------
;;; pairs

  (doit (pair <fixnum> <flonum>)
	=> (make-pair-type-descr <fixnum>-ctd <flonum>-ctd))

  (doit (pair-of <fixnum>)
	=> (make-pair-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; lists

  #;(debug-print (expansion-of (type-descriptor (list <fixnum> <flonum>))))

  (doit (list <fixnum> <flonum>)
	=> (make-list-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit (list-of <fixnum>)
	=> (make-list-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; vectors

  (doit (vector <fixnum> <flonum>)
	=> (make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit (vector-of <fixnum>)
	=> (make-vector-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; hashtables

  (doit (hashtable <fixnum> <flonum>)
	=> (make-hashtable-type-descr <fixnum>-ctd <flonum>-ctd))

;;; --------------------------------------------------------------------
;;; alists

  (doit (alist <fixnum> <flonum>)
	=> (make-alist-type-descr <fixnum>-ctd <flonum>-ctd))

;;; --------------------------------------------------------------------
;;; enumerations

  (doit (enumeration ciao hello)
	=> (make-enumeration-type-descr '(ciao hello)))

;;; --------------------------------------------------------------------
;;; compound-condition objects

  (doit (condition &who)
	=> (record-type-descriptor &who))

  (doit (condition &who &message)
	=> (make-compound-condition-type-descr (list (record-type-descriptor &who)
						     (record-type-descriptor &message))))

  (doit (condition &who (condition &message &irritants))
	=> (make-compound-condition-type-descr (list (record-type-descriptor &who)
						     (record-type-descriptor &message)
						     (record-type-descriptor &irritants))))

;;; --------------------------------------------------------------------
;;; closure

  #;(debug-print (expansion-of (type-descriptor (lambda (<fixnum>) => (<string>)))))

  (doit (lambda (<fixnum>) => (<string>))
	=> (make-closure-type-descr
	    (make-case-lambda-descriptors
	     (list (make-lambda-descriptors (make-descriptors-signature (list <string>-ctd))
					    (make-descriptors-signature (list <fixnum>-ctd)))))))

  (doit (lambda (<fixnum> <string> . <list>) => (<top>))
	=> (make-closure-type-descr
	    (make-case-lambda-descriptors
	     (list (make-lambda-descriptors (make-descriptors-signature (list <top>-ctd))
					    (make-descriptors-signature (cons* <fixnum>-ctd <string>-ctd <list>-ctd)))))))

  (doit (case-lambda
	  ((<number>) => (<string>))
	  ((<number> <non-negative-fixnum>) => (<string>)))
	=> (make-closure-type-descr
	    (make-case-lambda-descriptors
	     (list (make-lambda-descriptors (make-descriptors-signature (list <string>-ctd))
					    (make-descriptors-signature (list <number>-ctd)))
		   (make-lambda-descriptors (make-descriptors-signature (list <string>-ctd))
					    (make-descriptors-signature
					     (list <number>-ctd (make-union-type-descr (list <zero-fixnum>-ctd
											     <positive-fixnum>-ctd)))))))))

;;; --------------------------------------------------------------------
;;; union

  (doit (or <fixnum> <flonum>)
	=> (make-union-type-descr (list <fixnum>-ctd <flonum>-ctd)))

;;; --------------------------------------------------------------------
;;; intersection

  (doit (and <fixnum> <flonum>)
	=> (make-intersection-type-descr (list <fixnum>-ctd <flonum>-ctd)))

;;; --------------------------------------------------------------------
;;; complement

  (doit (not <fixnum>)
	=> (make-complement-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit (ancestor-of <fixnum>)
	=> (make-ancestor-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit (ancestor-of <fixnum>)
	=> (make-ancestor-of-type-descr <fixnum>-ctd))

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

  (doit-true	<top>-ctd		<top>-ctd)
  (doit-false	<top>-ctd		<number>-ctd)
  (doit-false	<number>-ctd		<top>-ctd)

  (doit-false	<top>-ctd		<struct>-ctd)
  (doit-false	<top>-ctd		<record>-ctd)
  (doit-false	<struct>-ctd		<record>-ctd)

  (doit-true	<struct>-ctd		<struct>-ctd)
  (doit-true	<record>-ctd		<record>-ctd)

  (doit-false	<list>-ctd		<null>-ctd)
  (doit-false	<nelist>-ctd		<null>-ctd)

  (doit-true	<list>-ctd		<list>-ctd)
  (doit-true	<null>-ctd		<null>-ctd)
  (doit-true	<nelist>-ctd		<nelist>-ctd)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	alpha-rtd			alpha-rtd)
  (doit-true	beta-rtd			beta-rtd)

  (doit-false	<top>-ctd			alpha-rtd)
  (doit-false	<struct>-ctd			alpha-rtd)
  (doit-false	alpha-rtd			<top>-ctd)
  (doit-false	alpha-rtd			<struct>-ctd)
  (doit-false	alpha-rtd			beta-rtd)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-false	<top>-ctd			<duo>-rtd)
  (doit-false	<struct>-ctd			<duo>-rtd)
  (doit-false	<record>-ctd			<duo>-rtd)
  (doit-false	<duo>-rtd			<top>-ctd)
  (doit-false	<duo>-rtd			<struct>-ctd)
  (doit-false	<duo>-rtd			<record>-ctd)

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

  (doit-false	<pair>-ctd
		(make-pair-type-descr <fixnum>-ctd <flonum>-ctd))

  (doit-false	(make-pair-type-descr <top>-ctd <null>-ctd)
		<pair>-ctd)

  (doit-true	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <string>-ctd))

  (doit-false	(make-pair-type-descr <number>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <string>-ctd))

  (doit-false	(make-pair-type-descr <string>-ctd <number>-ctd)
		(make-pair-type-descr <string>-ctd <fixnum>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <number>-ctd <string>-ctd))

  (doit-false	(make-pair-type-descr <string>-ctd <fixnum>-ctd)
		(make-pair-type-descr <string>-ctd <number>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <symbol>-ctd <string>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <symbol>-ctd))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-false	<pair>-ctd
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)
		<pair>-ctd)

  (doit-true	(make-pair-of-type-descr <fixnum>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-of-type-descr <number>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)
		(make-pair-of-type-descr <number>-ctd))

  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)
		(make-pair-of-type-descr <string>-ctd))

;;; --------------------------------------------------------------------
;;; list

  (doit-false	<list>-ctd
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-false	<nelist>-ctd
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		<nelist>-ctd)

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		<null>-ctd)

;;; list/list

  (doit-true	(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd <number>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <string>-ctd)))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-false	<list>-ctd
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	<nelist>-ctd
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	(make-list-of-type-descr <fixnum>-ctd)
		<null>-ctd)

  (doit-false	(make-list-of-type-descr <top>-ctd)
		<nelist>-ctd)

  (doit-false	(make-list-of-type-descr <top>-ctd)
		<list>-ctd)

;;; list-of/list-of

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	(make-list-of-type-descr <number>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	(make-list-of-type-descr <fixnum>-ctd)
		(make-list-of-type-descr <number>-ctd))

  (doit-false	(make-list-of-type-descr <string>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; vector

  (doit-false	<vector>-ctd
		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-false	<nevector>-ctd
		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-vector-type-descr (list <top>-ctd))
		<nevector>-ctd)

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))
		<nevector>-ctd)

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))
		<null>-ctd)

;;; vector/vector

  (doit-true	(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd <number>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <string>-ctd)))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-false	<vector>-ctd
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-false	<nevector>-ctd
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-false	(make-vector-of-type-descr <fixnum>-ctd)
		<empty-vector>-ctd)

  (doit-false	(make-vector-of-type-descr <top>-ctd)
		<nevector>-ctd)

  (doit-false	(make-vector-of-type-descr <top>-ctd)
		<vector>-ctd)

;;; vector-of/vector-of

  (doit-true	(make-vector-of-type-descr <fixnum>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-false	(make-vector-of-type-descr <number>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-false	(make-vector-of-type-descr <fixnum>-ctd)
		(make-vector-of-type-descr <number>-ctd))

  (doit-false	(make-vector-of-type-descr <string>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

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

  (doit-false	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<fixnum>-ctd)

  (doit-false	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<flonum>-ctd)

  (doit-false	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<string>-ctd)

  (doit-true	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd)))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-false	(make-intersection-type-descr (list <number>-ctd <flonum>-ctd))
		<flonum>-ctd)

  (doit-false	(make-intersection-type-descr (list <number>-ctd <flonum>-ctd))
		<string>-ctd)

  (doit-true	(make-intersection-type-descr (list <number>-ctd <flonum>-ctd))
		(make-intersection-type-descr (list <number>-ctd <flonum>-ctd)))

;;; --------------------------------------------------------------------
;;; complement

  (doit-false	(make-complement-type-descr <flonum>-ctd)
		<fixnum>-ctd)

  (doit-false	<fixnum>-ctd
		(make-complement-type-descr <flonum>-ctd))

  (doit-false	<fixnum>-ctd
		(make-complement-type-descr <fixnum>-ctd))

  (doit-true	(make-complement-type-descr <fixnum>-ctd)
		(make-complement-type-descr <fixnum>-ctd))

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

  (doit-true	<top>-ctd		<top>-ctd)
  (doit-true	<top>-ctd		<number>-ctd)
  (doit-false	<number>-ctd		<top>-ctd)

  (doit-true	<top>-ctd		<struct>-ctd)
  (doit-true	<top>-ctd		<record>-ctd)
  (doit-true	<struct>-ctd		<record>-ctd)

  (doit-true	<list>-ctd		<null>-ctd)
  (doit-false	<nelist>-ctd		<null>-ctd)

  (doit-true	<vector>-ctd		<empty-vector>-ctd)
  (doit-false	<nevector>-ctd		<empty-vector>-ctd)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	<top>-ctd		alpha-rtd)
  (doit-true	<struct>-ctd		alpha-rtd)
  (doit-false	alpha-rtd		<top>-ctd)
  (doit-false	alpha-rtd		<struct>-ctd)
  (doit-false	alpha-rtd		beta-rtd)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-true	<top>-ctd		<duo>-rtd)
  (doit-true	<struct>-ctd		<duo>-rtd)
  (doit-true	<record>-ctd		<duo>-rtd)
  (doit-false	<duo>-rtd		<top>-ctd)
  (doit-false	<duo>-rtd		<struct>-ctd)
  (doit-false	<duo>-rtd		<record>-ctd)

  (doit-true	<alpha>-rtd		<beta>-rtd)
  (doit-true	<beta>-rtd		<delta>-rtd)
  (doit-true	<alpha>-rtd		<delta>-rtd)
  (doit-false	<beta>-rtd		<alpha>-rtd)
  (doit-false	<delta>-rtd		<beta>-rtd)
  (doit-false	<delta>-rtd		<alpha>-rtd)
  (doit-false	<alpha>-rtd		<duo>-rtd)
  (doit-false	<duo>-rtd		<alpha>-rtd)

;;; --------------------------------------------------------------------
;;; pair

  (doit-true	<pair>-ctd
		(make-pair-type-descr <fixnum>-ctd <flonum>-ctd))

  (doit-false	(make-pair-type-descr <top>-ctd <null>-ctd)
		<pair>-ctd)

  (doit-true	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <string>-ctd))

  (doit-true	(make-pair-type-descr <number>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <string>-ctd))

  (doit-true	(make-pair-type-descr <string>-ctd <number>-ctd)
		(make-pair-type-descr <string>-ctd <fixnum>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <number>-ctd <string>-ctd))

  (doit-false	(make-pair-type-descr <string>-ctd <fixnum>-ctd)
		(make-pair-type-descr <string>-ctd <number>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <symbol>-ctd <string>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <symbol>-ctd))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-true	<pair>-ctd
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)
		<pair>-ctd)

  (doit-true	(make-pair-of-type-descr <fixnum>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-true	(make-pair-of-type-descr <number>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)
		(make-pair-of-type-descr <number>-ctd))

  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)
		(make-pair-of-type-descr <string>-ctd))

;;; --------------------------------------------------------------------
;;; list

  (doit-true	<list>-ctd
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-true	<nelist>-ctd
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		<nelist>-ctd)

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		<null>-ctd)

;;; list/list

  (doit-true	(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-true	(make-list-type-descr (list <fixnum>-ctd <number>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <string>-ctd)))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-true	<list>-ctd
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	<nelist>-ctd
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)
		<null>-ctd)

  (doit-false	(make-list-of-type-descr <top>-ctd)
		<nelist>-ctd)

  (doit-false	(make-list-of-type-descr <top>-ctd)
		<list>-ctd)

;;; list-of/list-of

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-true	(make-list-of-type-descr <number>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	(make-list-of-type-descr <fixnum>-ctd)
		(make-list-of-type-descr <number>-ctd))

  (doit-false	(make-list-of-type-descr <string>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; vector

  (doit-true	<vector>-ctd
		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-true	<nevector>-ctd
		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-vector-type-descr (list <top>-ctd))
		<nevector>-ctd)

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))
		<nevector>-ctd)

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))
		<null>-ctd)

;;; vector/vector

  (doit-true	(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-true	(make-vector-type-descr (list <fixnum>-ctd <number>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <string>-ctd)))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-true	<vector>-ctd
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-false	<nevector>-ctd
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-true	(make-vector-of-type-descr <fixnum>-ctd)
		<empty-vector>-ctd)

  (doit-false	(make-vector-of-type-descr <top>-ctd)
		<nevector>-ctd)

  (doit-false	(make-vector-of-type-descr <top>-ctd)
		<vector>-ctd)

;;; vector-of/vector-of

  (doit-true	(make-vector-of-type-descr <fixnum>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-true	(make-vector-of-type-descr <number>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-false	(make-vector-of-type-descr <fixnum>-ctd)
		(make-vector-of-type-descr <number>-ctd))

  (doit-false	(make-vector-of-type-descr <string>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(ciao)))

  (doit-true	<symbol>-ctd
		(make-enumeration-type-descr '(ciao)))

  (doit-false	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello)))

  (doit-false	(make-enumeration-type-descr '(ciao hello))
		(make-enumeration-type-descr '(hello)))

  (doit-false	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello ciao)))

;;; --------------------------------------------------------------------
;;; union

  (doit-false	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<fixnum>-ctd)

  (doit-false	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<flonum>-ctd)

  (doit-false	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<string>-ctd)

  (doit-true	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd)))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-false	(make-intersection-type-descr (list <number>-ctd <flonum>-ctd))
		<flonum>-ctd)

  (doit-false	(make-intersection-type-descr (list <number>-ctd <flonum>-ctd))
		<string>-ctd)

  (doit-true	(make-intersection-type-descr (list <number>-ctd <flonum>-ctd))
		(make-intersection-type-descr (list <number>-ctd <flonum>-ctd)))

;;; --------------------------------------------------------------------
;;; complement

  (doit-false	(make-complement-type-descr <flonum>-ctd)
		<fixnum>-ctd)

  (doit-false	<fixnum>-ctd
		(make-complement-type-descr <flonum>-ctd))

  (doit-false	<fixnum>-ctd
		(make-complement-type-descr <fixnum>-ctd))

  (doit-true	(make-complement-type-descr <fixnum>-ctd)
		(make-complement-type-descr <fixnum>-ctd))

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

  (doit-true	<top>-ctd		<top>-ctd)
  (doit-true	<top>-ctd		<number>-ctd)
  (doit-false	<number>-ctd		<top>-ctd)

  (doit-true	<top>-ctd		<struct>-ctd)
  (doit-true	<top>-ctd		<record>-ctd)
  (doit-true	<struct>-ctd		<record>-ctd)

  (doit-true	<list>-ctd		<null>-ctd)
  (doit-false	<nelist>-ctd		<null>-ctd)

  (doit-true	<vector>-ctd		<empty-vector>-ctd)
  (doit-false	<nevector>-ctd		<empty-vector>-ctd)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	<top>-ctd		alpha-rtd)
  (doit-true	<struct>-ctd		alpha-rtd)
  (doit-false	alpha-rtd		<top>-ctd)
  (doit-false	alpha-rtd		<struct>-ctd)
  (doit-false	alpha-rtd		beta-rtd)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-true	<top>-ctd		<duo>-rtd)
  (doit-true	<struct>-ctd		<duo>-rtd)
  (doit-true	<record>-ctd		<duo>-rtd)
  (doit-false	<duo>-rtd		<top>-ctd)
  (doit-false	<duo>-rtd		<struct>-ctd)
  (doit-false	<duo>-rtd		<record>-ctd)

  (doit-true	<alpha>-rtd		<beta>-rtd)
  (doit-true	<beta>-rtd		<delta>-rtd)
  (doit-true	<alpha>-rtd		<delta>-rtd)
  (doit-false	<beta>-rtd		<alpha>-rtd)
  (doit-false	<delta>-rtd		<beta>-rtd)
  (doit-false	<delta>-rtd		<alpha>-rtd)
  (doit-false	<alpha>-rtd		<duo>-rtd)
  (doit-false	<duo>-rtd		<alpha>-rtd)

;;; --------------------------------------------------------------------
;;; pair

  (doit-true	<pair>-ctd
		(make-pair-type-descr <fixnum>-ctd <flonum>-ctd))

  (doit-true	(make-pair-type-descr <top>-ctd <null>-ctd)
		<nelist>-ctd)

  (doit-true	(make-pair-type-descr <top>-ctd <null>-ctd)
		<nelist>-ctd)

  (doit-true	(make-pair-type-descr <top>-ctd <null>-ctd)
		<nelist>-ctd)

  (doit-true	(make-pair-type-descr <top>-ctd <list>-ctd)
		<nelist>-ctd)

  (doit-false	(make-pair-type-descr <fixnum>-ctd <list>-ctd)
		<nelist>-ctd)

;;; pair/pair

  (doit-true	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <string>-ctd))

  (doit-true	(make-pair-type-descr <number>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <string>-ctd))

  (doit-true	(make-pair-type-descr <string>-ctd <number>-ctd)
		(make-pair-type-descr <string>-ctd <fixnum>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <number>-ctd <string>-ctd))

  (doit-false	(make-pair-type-descr <string>-ctd <fixnum>-ctd)
		(make-pair-type-descr <string>-ctd <number>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <symbol>-ctd <string>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <string>-ctd)
		(make-pair-type-descr <fixnum>-ctd <symbol>-ctd))

;;; pair/pair-of

  (doit-true	(make-pair-type-descr    <fixnum>-ctd <number>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-type-descr    <string>-ctd <number>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-type-descr    <fixnum>-ctd <string>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

;;; pair/list

  (doit-true	(make-pair-type-descr <fixnum>-ctd <null>-ctd)
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-pair-type-descr <fixnum>-ctd <flonum>-ctd)
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-true	(make-pair-type-descr <fixnum>-ctd (make-list-of-type-descr <fixnum>-ctd))
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-pair-type-descr <fixnum>-ctd (make-list-type-descr (list <fixnum>-ctd)))
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-true	(make-pair-type-descr <fixnum>-ctd (make-list-type-descr (list <fixnum>-ctd)))
		(make-list-type-descr (list <fixnum>-ctd <fixnum>-ctd)))

;;; pair/list-of

  (doit-false	(make-pair-type-descr <fixnum>-ctd (make-list-of-type-descr <fixnum>-ctd))
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-type-descr <fixnum>-ctd (make-list-type-descr (list <fixnum>-ctd)))
		(make-list-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-true	<pair>-ctd
		(make-pair-of-type-descr <fixnum>-ctd))

;;;

  (doit-true	(make-pair-of-type-descr <null>-ctd)
		<list>-ctd)

  (doit-true	(make-pair-of-type-descr <list>-ctd)
		<list>-ctd)

  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)
		<list>-ctd)

;;;

  (doit-true	(make-pair-of-type-descr <null>-ctd)
		<nelist>-ctd)

  (doit-true	(make-pair-of-type-descr <list>-ctd)
		<nelist>-ctd)

  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)
		<nelist>-ctd)

;;; pair-of/pair

  (doit-true	(make-pair-of-type-descr <list>-ctd)
		(make-pair-type-descr <list>-ctd <list>-ctd))

  (doit-true	(make-pair-of-type-descr (make-union-type-descr (list <number>-ctd <list>-ctd)))
		(make-pair-type-descr <fixnum>-ctd <list>-ctd))

;;; pair-of/pair-of

  (doit-true	(make-pair-of-type-descr <number>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-of-type-descr <number>-ctd)
		(make-pair-of-type-descr <string>-ctd))

;;; pair-of/list

  (doit-true	(make-pair-of-type-descr <list>-ctd)
		(make-list-type-descr (list <list>-ctd <list>-ctd)))

  (doit-true	(make-pair-of-type-descr (make-union-type-descr (list <number>-ctd <list>-ctd)))
		(make-list-type-descr (list <fixnum>-ctd <list>-ctd)))

;;; pair-of/list-of

  (doit-true	(make-pair-of-type-descr <number>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	(make-pair-of-type-descr <number>-ctd)
		(make-list-of-type-descr <string>-ctd))

;;; --------------------------------------------------------------------
;;; list

  (doit-true	<list>-ctd
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-true	<nelist>-ctd
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-true	(make-list-type-descr (list <top>-ctd))
		<nelist>-ctd)

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		<nelist>-ctd)

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		<null>-ctd)

;;; list/pair

  (doit-true	(make-list-type-descr (list <fixnum>-ctd))
		(make-pair-type-descr <fixnum>-ctd <null>-ctd))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		(make-pair-type-descr <fixnum>-ctd <string>-ctd))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-pair-type-descr <fixnum>-ctd <null>-ctd))

;;; list/pair-of

  (doit-true	(make-list-type-descr (list <null>-ctd))
		(make-pair-of-type-descr <null>-ctd))

  (doit-true	(make-list-type-descr (list <list>-ctd))
		(make-pair-of-type-descr <null>-ctd))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		(make-pair-of-type-descr <list>-ctd))

  (doit-true	(make-list-type-descr (list (make-list-type-descr (list <flonum>-ctd)) <flonum>-ctd))
		(make-pair-of-type-descr (make-list-type-descr (list <flonum>-ctd))))

;;; list/list

  (doit-true	(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-true	(make-list-type-descr (list <fixnum>-ctd <number>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-false	(make-list-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-list-type-descr (list <fixnum>-ctd <string>-ctd)))

;;; list/list-of

  (doit-false	(make-list-type-descr (list <fixnum>-ctd))
		(make-list-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-true	<list>-ctd
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	<nelist>-ctd
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)
		<null>-ctd)

  (doit-true	(make-list-of-type-descr <top>-ctd)
		<nelist>-ctd)

  (doit-true	(make-list-of-type-descr <top>-ctd)
		<list>-ctd)

;;; list-of/pair

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)
		(make-pair-type-descr <fixnum>-ctd <null>-ctd))

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)
		(make-pair-type-descr <fixnum>-ctd (make-list-of-type-descr <fixnum>-ctd)))

  (doit-true	(make-list-of-type-descr <number>-ctd)
		(make-pair-type-descr <fixnum>-ctd (make-list-type-descr (list <flonum>-ctd))))

  (doit-false	(make-list-of-type-descr <number>-ctd)
		(make-pair-type-descr <fixnum>-ctd (make-list-type-descr (list <string>-ctd))))

  (doit-false	(make-list-of-type-descr <number>-ctd)
		(make-pair-type-descr <string>-ctd <null>-ctd))

;;; list-of/pair-of

  (doit-true	(make-list-of-type-descr <null>-ctd)
		(make-pair-of-type-descr <null>-ctd))

  (doit-true	(make-list-of-type-descr <list>-ctd)
		(make-pair-of-type-descr <list>-ctd))

  (doit-false	(make-list-of-type-descr <fixnum>-ctd)
		(make-pair-of-type-descr <fixnum>-ctd))

;;; list-of/list

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-true	(make-list-of-type-descr <number>-ctd)
		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-list-of-type-descr <fixnum>-ctd)
		(make-list-type-descr (list <number>-ctd)))

;;; list-of/list-of

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-true	(make-list-of-type-descr <number>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

  (doit-false	(make-list-of-type-descr <fixnum>-ctd)
		(make-list-of-type-descr <number>-ctd))

  (doit-false	(make-list-of-type-descr <string>-ctd)
		(make-list-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; vector

  (doit-true	<vector>-ctd
		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-true	<nevector>-ctd
		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-true	(make-vector-type-descr (list <top>-ctd))
		<nevector>-ctd)

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))
		<nevector>-ctd)

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))
		<null>-ctd)

;;; vector/vector

  (doit-true	(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-true	(make-vector-type-descr (list <fixnum>-ctd <number>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd)))

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-vector-type-descr (list <fixnum>-ctd <string>-ctd)))

;;; vector/vector-of

  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))
		(make-vector-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-true	<vector>-ctd
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-false	<nevector>-ctd
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-true	(make-vector-of-type-descr <fixnum>-ctd)
		<empty-vector>-ctd)

  (doit-true	(make-vector-of-type-descr <top>-ctd)
		<nevector>-ctd)

  (doit-true	(make-vector-of-type-descr <top>-ctd)
		<vector>-ctd)

;;; vector-of/vector

  (doit-true	(make-vector-of-type-descr <fixnum>-ctd)
		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-true	(make-vector-of-type-descr <number>-ctd)
		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-vector-of-type-descr <fixnum>-ctd)
		(make-vector-type-descr (list <number>-ctd)))

;;; vector-of/vector-of

  (doit-true	(make-vector-of-type-descr <fixnum>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-true	(make-vector-of-type-descr <number>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-false	(make-vector-of-type-descr <fixnum>-ctd)
		(make-vector-of-type-descr <number>-ctd))

  (doit-false	(make-vector-of-type-descr <string>-ctd)
		(make-vector-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(ciao)))

  (doit-true	<symbol>-ctd
		(make-enumeration-type-descr '(ciao)))

  (doit-false	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello)))

  (doit-false	(make-enumeration-type-descr '(ciao hello))
		(make-enumeration-type-descr '(hello)))

  (doit-true	(make-enumeration-type-descr '(ciao))
		(make-enumeration-type-descr '(hello ciao)))

;;; --------------------------------------------------------------------
;;; union

  (doit-true	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<fixnum>-ctd)

  (doit-true	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<flonum>-ctd)

  (doit-false	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		<string>-ctd)

  (doit-true	(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd))
		(make-union-type-descr (list <fixnum>-ctd <flonum>-ctd)))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-true	(make-intersection-type-descr (list <number>-ctd <flonum>-ctd))
		<flonum>-ctd)

  (doit-false	(make-intersection-type-descr (list <number>-ctd <flonum>-ctd))
		<string>-ctd)

;;; --------------------------------------------------------------------
;;; complement

  (doit-true	(make-complement-type-descr <flonum>-ctd)
		<fixnum>-ctd)

  (doit-true	<fixnum>-ctd
		(make-complement-type-descr <flonum>-ctd))

  (doit-false	<fixnum>-ctd
		(make-complement-type-descr <fixnum>-ctd))

  #| end of PARAMETRISE |# )


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
