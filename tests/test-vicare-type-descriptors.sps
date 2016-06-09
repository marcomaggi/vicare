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


(parametrise ((check-test-name	'type-descriptor-parent))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?type-annotation => ?expected)
       (check
	   (type-descriptor-parent ?type-annotation)
	 (=> object-type-descr=?)
	 ?expected))
      ))

;;; --------------------------------------------------------------------
;;; core

  (doit <top>			=> #f)
  (doit <fixnum>		=> <exact-integer>-ctd)
  (doit <string>		=> <top>-ctd)

  (doit <vector>		=> <top>-ctd)
  (doit <nevector>		=> <vector>-ctd)
  (doit <empty-vector>		=> <vector>-ctd)

  (doit <list>			=> <top>-ctd)
  (doit <nelist>		=> <list>-ctd)
  (doit <null>			=> <list>-ctd)

  (doit <bytevector>		=> <top>-ctd)
  (doit <nebytevector>		=> <bytevector>-ctd)
  (doit <empty-bytevector>	=> <bytevector>-ctd)

;;; --------------------------------------------------------------------
;;; records

  (doit <alpha>			=> <record>-ctd)
  (doit <beta>			=> (record-type-descriptor <alpha>))

  (doit &who			=> (record-type-descriptor &condition))

;;; --------------------------------------------------------------------
;;; structs

  (doit alpha			=> <struct>-ctd)
  (doit beta			=> <struct>-ctd)

;;; --------------------------------------------------------------------
;;; pairs

  (doit (pair <fixnum> <flonum>)	=> <pair>-ctd)
  (doit (pair-of <fixnum>)		=> <pair>-ctd)

;;; --------------------------------------------------------------------
;;; lists

  (doit (list <fixnum> <flonum>)	=> <nelist>-ctd)
  (doit (list-of <fixnum>)		=> <list>-ctd)

;;; --------------------------------------------------------------------
;;; vectors

  (doit (vector <fixnum> <flonum>)	=> <nevector>-ctd)
  (doit (vector-of <fixnum>)		=> <vector>-ctd)

;;; --------------------------------------------------------------------
;;; compound condition-objects

  (doit (condition &who)		=> (record-type-descriptor &condition))
  (doit (condition &who &message)	=> <compound-condition>-ctd)

;;; --------------------------------------------------------------------
;;; misc

  (doit (enumeration ciao)		=> <symbol>-ctd)
  (doit (hashtable <fixnum> <flonum>)	=> <hashtable>-ctd)
  (doit (alist <fixnum> <flonum>)	=> <list>-ctd)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'type-descriptor-ancestors))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?type-annotation => ?expected)
       (check
	   (type-descriptor-ancestors ?type-annotation)
	 (=> result=?)
	 ?expected))
      ))

  (define (result=? A B)
    (cond ((list? A)
	   (and (list? B)
		(= (length A) (length B))
		(for-all result=? A B)))
	  ((vector? A)
	   (and (vector? B)
		(= (vector-length A) (vector-length B))
		(vector-for-all result=? A B)))
	  ((pair? A)
	   (and (pair? B)
		(result=? (car A) (car B))
		(result=? (cdr A) (cdr B))))
	  (else
	   (object-type-descr=? A B))))

;;; --------------------------------------------------------------------
;;; core

  (doit <top>			=> '())
  (doit <fixnum>		=> (list <exact-integer>-ctd
					 <integer>-ctd
					 <rational>-ctd
					 <rational-valued>-ctd
					 <real>-ctd
					 <real-valued>-ctd
					 <complex>-ctd
					 <number>-ctd
					 <top>-ctd))
  (doit <string>		=> (list <top>-ctd))

  (doit <vector>		=> (list <top>-ctd))
  (doit <nevector>		=> (list <vector>-ctd <top>-ctd))
  (doit <empty-vector>		=> (list <vector>-ctd <top>-ctd))

  (doit <list>			=> (list <top>-ctd))
  (doit <nelist>		=> (list <list>-ctd <top>-ctd))
  (doit <null>			=> (list <list>-ctd <top>-ctd))

  (doit <bytevector>		=> (list <top>-ctd))
  (doit <nebytevector>		=> (list <bytevector>-ctd <top>-ctd))
  (doit <empty-bytevector>	=> (list <bytevector>-ctd <top>-ctd))

;;; --------------------------------------------------------------------
;;; records

  (doit <alpha>			=> (list <record>-ctd <struct>-ctd <top>-ctd))
  (doit <beta>			=> (list (record-type-descriptor <alpha>) <record>-ctd <struct>-ctd <top>-ctd))

  (doit &who			=> (list (record-type-descriptor &condition)
					 <condition>-ctd <record>-ctd <struct>-ctd <top>-ctd))

;;; --------------------------------------------------------------------
;;; structs

  (doit alpha			=> (list <struct>-ctd <top>-ctd))
  (doit beta			=> (list <struct>-ctd <top>-ctd))

;;; --------------------------------------------------------------------
;;; pairs

  (doit (pair <fixnum> <flonum>)	=> (list <pair>-ctd <top>-ctd))
  (doit (pair-of <fixnum>)		=> (list <pair>-ctd <top>-ctd))

;;; --------------------------------------------------------------------
;;; lists

  (doit (list <fixnum> <flonum>)	=> (list <nelist>-ctd <list>-ctd <top>-ctd))
  (doit (list-of <fixnum>)		=> (list <list>-ctd <top>-ctd))

;;; --------------------------------------------------------------------
;;; vectors

  (doit (vector <fixnum> <flonum>)	=> (list <nevector>-ctd <vector>-ctd <top>-ctd))
  (doit (vector-of <fixnum>)		=> (list <vector>-ctd <top>-ctd))

;;; --------------------------------------------------------------------
;;; compound condition-objects

  (doit (condition &who)		=> (list (record-type-descriptor &condition)
						 <condition>-ctd <record>-ctd <struct>-ctd <top>-ctd))
  (doit (condition &who &message)	=> (list <compound-condition>-ctd <condition>-ctd <record>-ctd <struct>-ctd <top>-ctd))

;;; --------------------------------------------------------------------
;;; misc

  (doit (enumeration ciao)		=> (list <symbol>-ctd <top>-ctd))
  (doit (hashtable <fixnum> <flonum>)	=> (list <hashtable>-ctd <top>-ctd))
  (doit (alist <fixnum> <flonum>)	=> (list <list>-ctd <top>-ctd))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'descriptors-equality-proc))

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


(parametrise ((check-test-name	'descriptors-equality-syntax))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(type-descriptor=? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-false
	(type-descriptor=? ?super ?sub)))
      ))

;;; --------------------------------------------------------------------
;;; built-in types

  (doit-true	<top>			<top>)
  (doit-false	<top>			<number>)
  (doit-false	<number>		<top>)

  (doit-false	<top>			<struct>)
  (doit-false	<top>			<record>)
  (doit-false	<struct>		<record>)

  (doit-true	<struct>		<struct>)
  (doit-true	<record>		<record>)

  (doit-false	<list>			<null>)
  (doit-false	<nelist>		<null>)

  (doit-true	<list>			<list>)
  (doit-true	<null>			<null>)
  (doit-true	<nelist>		<nelist>)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	alpha			alpha)
  (doit-true	beta			beta)

  (doit-false	<top>			alpha)
  (doit-false	<struct>		alpha)
  (doit-false	alpha			<top>)
  (doit-false	alpha			<struct>)
  (doit-false	alpha			beta)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-false	<top>			<duo>)
  (doit-false	<struct>		<duo>)
  (doit-false	<record>		<duo>)
  (doit-false	<duo>			<top>)
  (doit-false	<duo>			<struct>)
  (doit-false	<duo>			<record>)

  (doit-true	<alpha>			<alpha>)
  (doit-true	<beta>			<beta>)
  (doit-true	<delta>			<delta>)

  (doit-false	<alpha>			<beta>)
  (doit-false	<beta>			<delta>)
  (doit-false	<alpha>			<delta>)
  (doit-false	<beta>			<alpha>)
  (doit-false	<delta>			<beta>)
  (doit-false	<delta>			<alpha>)
  (doit-false	<alpha>			<duo>)
  (doit-false	<duo>			<alpha>)

;;; --------------------------------------------------------------------
;;; pair

  (doit-false	<pair>
		(pair <fixnum> <flonum>))

  (doit-false	(pair <top> <null>)
		<pair>)

  (doit-true	(pair <fixnum> <string>)
		(pair <fixnum> <string>))

  (doit-false	(pair <number> <string>)
		(pair <fixnum> <string>))

  (doit-false	(pair <string> <number>)
		(pair <string> <fixnum>))

  (doit-false	(pair <fixnum> <string>)
		(pair <number> <string>))

  (doit-false	(pair <string> <fixnum>)
		(pair <string> <number>))

  (doit-false	(pair <fixnum> <string>)
		(pair <symbol> <string>))

  (doit-false	(pair <fixnum> <string>)
		(pair <fixnum> <symbol>))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-false	<pair>
		(pair-of <fixnum>))

  (doit-false	(pair-of <fixnum>)
		<pair>)

  (doit-true	(pair-of <fixnum>)
		(pair-of <fixnum>))

  (doit-false	(pair-of <number>)
		(pair-of <fixnum>))

  (doit-false	(pair-of <fixnum>)
		(pair-of <number>))

  (doit-false	(pair-of <fixnum>)
		(pair-of <string>))

;;; --------------------------------------------------------------------
;;; list

  (doit-false	<list>
		(list <fixnum>))

  (doit-false	<nelist>
		(list <fixnum>))

  (doit-false	(list <fixnum>)
		<nelist>)

  (doit-false	(list <fixnum>)
		<null>)

;;; list/list

  (doit-true	(list <fixnum> <flonum>)
		(list <fixnum> <flonum>))

  (doit-false	(list <fixnum> <number>)
		(list <fixnum> <flonum>))

  (doit-false	(list <fixnum> <flonum>)
		(list <fixnum> <string>))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-false	<list>
		(list-of <fixnum>))

  (doit-false	<nelist>
		(list-of <fixnum>))

  (doit-false	(list-of <fixnum>)
		<null>)

  (doit-false	(list-of <top>)
		<nelist>)

  (doit-false	(list-of <top>)
		<list>)

;;; list-of/list-of

  (doit-true	(list-of <fixnum>)
		(list-of <fixnum>))

  (doit-false	(list-of <number>)
		(list-of <fixnum>))

  (doit-false	(list-of <fixnum>)
		(list-of <number>))

  (doit-false	(list-of <string>)
		(list-of <fixnum>))

;;; --------------------------------------------------------------------
;;; vector

  (doit-false	<vector>
		(vector (list <fixnum>)))

  (doit-false	<nevector>
		(vector (list <fixnum>)))

  (doit-false	(vector (list <top>))
		<nevector>)

  (doit-false	(vector (list <fixnum>))
		<nevector>)

  (doit-false	(vector (list <fixnum>))
		<null>)

;;; vector/vector

  (doit-true	(vector (list <fixnum> <flonum>))
		(vector (list <fixnum> <flonum>)))

  (doit-false	(vector (list <fixnum> <number>))
		(vector (list <fixnum> <flonum>)))

  (doit-false	(vector (list <fixnum> <flonum>))
		(vector (list <fixnum> <string>)))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-false	<vector>
		(vector-of <fixnum>))

  (doit-false	<nevector>
		(vector-of <fixnum>))

  (doit-false	(vector-of <fixnum>)
		<empty-vector>)

  (doit-false	(vector-of <top>)
		<nevector>)

  (doit-false	(vector-of <top>)
		<vector>)

;;; vector-of/vector-of

  (doit-true	(vector-of <fixnum>)
		(vector-of <fixnum>))

  (doit-false	(vector-of <number>)
		(vector-of <fixnum>))

  (doit-false	(vector-of <fixnum>)
		(vector-of <number>))

  (doit-false	(vector-of <string>)
		(vector-of <fixnum>))

;;; --------------------------------------------------------------------
;;; compound condition-objects

  (doit-true	(condition &who)
		(condition &who))

  (doit-false	(condition &condition)
		(condition &who))

  (doit-false	(condition &who)
		(condition &who &irritants))

  (doit-true	(condition &who &irritants)
		(condition &who &irritants))

  (doit-true	(condition &irritants &who)
		(condition &who &irritants))

  (doit-false	(condition &who)
		(condition &who (condition &irritants)))

  (doit-true	(condition &who (condition &irritants))
		(condition &who (condition &irritants)))

  (doit-true	(condition &irritants (condition &who))
		(condition (condition &who) &irritants))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	(enumeration ciao)
		(enumeration ciao))

  (doit-false	(enumeration ciao)
		(enumeration hello))

  (doit-false	(enumeration ciao hello)
		(enumeration hello))

  (doit-false	(enumeration ciao)
		(enumeration hello ciao))

;;; --------------------------------------------------------------------
;;; hashtable

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <fixnum>))

  (doit-false	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <string>))

  (doit-false	(hashtable <fixnum> <fixnum>)
		(hashtable <string> <fixnum>))

;;; --------------------------------------------------------------------
;;; alist

  (doit-true	(alist <fixnum> <fixnum>)
		(alist <fixnum> <fixnum>))

  (doit-false	(alist <fixnum> <fixnum>)
		(alist <fixnum> <string>))

  (doit-false	(alist <fixnum> <fixnum>)
		(alist <string> <fixnum>))

;;; --------------------------------------------------------------------
;;; union

  (doit-false	(or (list <fixnum> <flonum>))
		<fixnum>)

  (doit-false	(or (list <fixnum> <flonum>))
		<flonum>)

  (doit-false	(or (list <fixnum> <flonum>))
		<string>)

  (doit-true	(or (list <fixnum> <flonum>))
		(or (list <fixnum> <flonum>)))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-false	(and (list <number> <flonum>))
		<flonum>)

  (doit-false	(and (list <number> <flonum>))
		<string>)

  (doit-true	(and (list <number> <flonum>))
		(and (list <number> <flonum>)))

;;; --------------------------------------------------------------------
;;; complement

  (doit-false	(not <flonum>)
		<fixnum>)

  (doit-false	<fixnum>
		(not <flonum>))

  (doit-false	<fixnum>
		(not <fixnum>))

  (doit-true	(not <fixnum>)
		(not <fixnum>))

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit-true	(ancestor-of <flonum>)		(ancestor-of <flonum>))
  (doit-false	(ancestor-of <flonum>)		(ancestor-of <fixnum>))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'ancestry-super-and-sub-proc))

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


(parametrise ((check-test-name	'ancestry-super-and-sub-syntax))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(type-descriptor-ancestry-super-and-sub? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-false
	(type-descriptor-ancestry-super-and-sub? ?super ?sub)))
      ))

;;; --------------------------------------------------------------------
;;; built-in types

  (doit-true	<top>			<top>)
  (doit-true	<top>			<number>)
  (doit-false	<number>		<top>)

  (doit-true	<top>			<struct>)
  (doit-true	<top>			<record>)
  (doit-true	<struct>		<record>)

  (doit-true	<list>			<null>)
  (doit-false	<nelist>		<null>)

  (doit-true	<vector>		<empty-vector>)
  (doit-false	<nevector>		<empty-vector>)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	<top>			alpha)
  (doit-true	<struct>		alpha)
  (doit-false	alpha			<top>)
  (doit-false	alpha			<struct>)
  (doit-false	alpha			beta)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-true	<top>			<duo>)
  (doit-true	<struct>		<duo>)
  (doit-true	<record>		<duo>)
  (doit-false	<duo>			<top>)
  (doit-false	<duo>			<struct>)
  (doit-false	<duo>			<record>)

  (doit-true	<alpha>			<beta>)
  (doit-true	<beta>			<delta>)
  (doit-true	<alpha>			<delta>)
  (doit-false	<beta>			<alpha>)
  (doit-false	<delta>			<beta>)
  (doit-false	<delta>			<alpha>)
  (doit-false	<alpha>			<duo>)
  (doit-false	<duo>			<alpha>)

;;; --------------------------------------------------------------------
;;; pair

  (doit-true	<pair>
		(pair <fixnum> <flonum>))

  (doit-false	(pair <top> <null>)
		<pair>)

  (doit-true	(pair <fixnum> <string>)
		(pair <fixnum> <string>))

  (doit-true	(pair <number> <string>)
		(pair <fixnum> <string>))

  (doit-true	(pair <string> <number>)
		(pair <string> <fixnum>))

  (doit-false	(pair <fixnum> <string>)
		(pair <number> <string>))

  (doit-false	(pair <string> <fixnum>)
		(pair <string> <number>))

  (doit-false	(pair <fixnum> <string>)
		(pair <symbol> <string>))

  (doit-false	(pair <fixnum> <string>)
		(pair <fixnum> <symbol>))

  (doit-false	(pair <fixnum> <null>)
		(list <fixnum>))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-true	<pair>
		(pair-of <fixnum>))

  (doit-false	(pair-of <fixnum>)
		<pair>)

  (doit-true	(pair-of <fixnum>)
		(pair-of <fixnum>))

  (doit-true	(pair-of <number>)
		(pair-of <fixnum>))

  (doit-false	(pair-of <fixnum>)
		(pair-of <number>))

  (doit-false	(pair-of <fixnum>)
		(pair-of <string>))

;;; --------------------------------------------------------------------
;;; list

  (doit-true	<list>
		(list <fixnum>))

  (doit-true	<nelist>
		(list <fixnum>))

  (doit-false	(list <fixnum>)
		<nelist>)

  (doit-false	(list <fixnum>)
		<null>)

;;; list/list

  (doit-true	(list <fixnum> <flonum>)
		(list <fixnum> <flonum>))

  (doit-true	(list <fixnum> <number>)
		(list <fixnum> <flonum>))

  (doit-false	(list <fixnum> <flonum>)
		(list <fixnum> <string>))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-true	<list>
		(list-of <fixnum>))

  (doit-false	<nelist>
		(list-of <fixnum>))

  (doit-true	(list-of <fixnum>)
		<null>)

  (doit-false	(list-of <top>)
		<nelist>)

  (doit-false	(list-of <top>)
		<list>)

;;; list-of/list-of

  (doit-true	(list-of <fixnum>)
		(list-of <fixnum>))

  (doit-true	(list-of <number>)
		(list-of <fixnum>))

  (doit-false	(list-of <fixnum>)
		(list-of <number>))

  (doit-false	(list-of <string>)
		(list-of <fixnum>))

;;; --------------------------------------------------------------------
;;; vector

  (doit-true	<vector>
		(vector <fixnum>))

  (doit-true	<nevector>
		(vector <fixnum>))

  (doit-false	(vector <top>)
		<nevector>)

  (doit-false	(vector <fixnum>)
		<nevector>)

  (doit-false	(vector <fixnum>)
		<null>)

;;; vector/vector

  (doit-true	(vector <fixnum> <flonum>)
		(vector <fixnum> <flonum>))

  (doit-true	(vector <fixnum> <number>)
		(vector <fixnum> <flonum>))

  (doit-false	(vector <fixnum> <flonum>)
		(vector <fixnum> <string>))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-true	<vector>
		(vector-of <fixnum>))

  (doit-false	<nevector>
		(vector-of <fixnum>))

  (doit-true	(vector-of <fixnum>)
		<empty-vector>)

  (doit-false	(vector-of <top>)
		<nevector>)

  (doit-false	(vector-of <top>)
		<vector>)

;;; vector-of/vector-of

  (doit-true	(vector-of <fixnum>)
		(vector-of <fixnum>))

  (doit-true	(vector-of <number>)
		(vector-of <fixnum>))

  (doit-false	(vector-of <fixnum>)
		(vector-of <number>))

  (doit-false	(vector-of <string>)
		(vector-of <fixnum>))

;;; --------------------------------------------------------------------
;;; compound condition-objects

  (doit-true	(condition &who)
		(condition &who))

  (doit-true	(condition &condition)
		(condition &who))

  (doit-false	(condition &who)
		(condition &who &irritants))

  (doit-true	(condition &who &irritants)
		(condition &who &irritants))

  (doit-true	(condition &irritants &who)
		(condition &who &irritants))

  (doit-false	(condition &who)
		(condition &who (condition &irritants)))

  (doit-true	(condition &who (condition &irritants))
		(condition &who (condition &irritants)))

  (doit-true	(condition &irritants (condition &who))
		(condition (condition &who) &irritants))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	(enumeration ciao)
		(enumeration ciao))

  (doit-true	<symbol>
		(enumeration ciao))

  (doit-false	(enumeration ciao)
		(enumeration hello))

  (doit-false	(enumeration ciao hello)
		(enumeration hello))

  (doit-false	(enumeration ciao)
		(enumeration hello ciao))

;;; --------------------------------------------------------------------
;;; hashtable

  (doit-true	<top>
		(hashtable <fixnum> <fixnum>))

  (doit-true	<hashtable>
		(hashtable <fixnum> <fixnum>))

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <fixnum>))

  (doit-false	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <string>))

  (doit-false	(hashtable <fixnum> <fixnum>)
		(hashtable <string> <fixnum>))

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <positive-fixnum> <fixnum>))

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <positive-fixnum>))

;;; --------------------------------------------------------------------
;;; alist

  (doit-true	(alist <fixnum> <fixnum>)
		(alist <fixnum> <fixnum>))

  (doit-false	(alist <fixnum> <fixnum>)
		(alist <fixnum> <string>))

  (doit-false	(alist <fixnum> <fixnum>)
		(alist <string> <fixnum>))

;;; --------------------------------------------------------------------
;;; union

  (doit-false	(or <fixnum> <flonum>)
		<fixnum>)

  (doit-false	(or <fixnum> <flonum>)
		<flonum>)

  (doit-false	(or <fixnum> <flonum>)
		<string>)

  (doit-true	(or <fixnum> <flonum>)
		(or <fixnum> <flonum>))

;;; --------------------------------------------------------------------
;;; intersection

  ;;This  results into  true because  the intersection  is simplified  into a  single
  ;;"<flonum>".
  ;;
  (doit-true	(and <number> <flonum>)
		<flonum>)

  (doit-false	(and <number> <flonum>)
		<string>)

  (doit-true	(and <number> <flonum>)
		(and <number> <flonum>))

;;; --------------------------------------------------------------------
;;; complement

  (doit-false	(not <flonum>)
		<fixnum>)

  (doit-false	<fixnum>
		(not <flonum>))

  (doit-false	<fixnum>
		(not <fixnum>))

  (doit-true	(not <fixnum>)
		(not <fixnum>))

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit-true	(ancestor-of <fixnum>)
		(ancestor-of <fixnum>))

  (doit-false	(ancestor-of <fixnum>)
		(ancestor-of <positive-fixnum>))

  (doit-false	(ancestor-of <fixnum>)			<fixnum>)
  (doit-true	(ancestor-of <fixnum>)			<exact-integer>)
  (doit-true	(ancestor-of <fixnum>)			<integer>)
  (doit-true	(ancestor-of <fixnum>)			<real>)
  (doit-true	(ancestor-of <fixnum>)			<complex>)
  (doit-true	(ancestor-of <fixnum>)			<number>)
  (doit-true	(ancestor-of <fixnum>)			<top>)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'matching-super-and-sub-proc))

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

  (doit-false	(make-pair-type-descr <top>-ctd <null>-ctd)	<nelist>-ctd)
  (doit-true	(make-pair-type-descr <top>-ctd <list>-ctd)	<nelist>-ctd)
  (doit-false	(make-pair-type-descr <fixnum>-ctd <list>-ctd)	<nelist>-ctd)

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

  (doit-false	(make-pair-of-type-descr <null>-ctd)		<list>-ctd)
  (doit-false	(make-pair-of-type-descr <list>-ctd)		<list>-ctd)
  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)		<list>-ctd)

;;;

  (doit-false	(make-pair-of-type-descr <null>-ctd)		<nelist>-ctd)
  (doit-false	(make-pair-of-type-descr <list>-ctd)		<nelist>-ctd)
  (doit-false	(make-pair-of-type-descr <fixnum>-ctd)		<nelist>-ctd)

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

  (doit-true	<list>-ctd		(make-list-type-descr (list <fixnum>-ctd)))
  (doit-true	<nelist>-ctd		(make-list-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-list-type-descr (list <top>-ctd))		<nelist>-ctd)
  (doit-false	(make-list-type-descr (list <fixnum>-ctd))	<nelist>-ctd)
  (doit-false	(make-list-type-descr (list <fixnum>-ctd))	<null>-ctd)

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

  (doit-true	<list>-ctd		(make-list-of-type-descr <fixnum>-ctd))
  (doit-false	<nelist>-ctd		(make-list-of-type-descr <fixnum>-ctd))

  (doit-true	(make-list-of-type-descr <top>-ctd)		<null>-ctd)
  (doit-true	(make-list-of-type-descr <top>-ctd)		<list>-ctd)
  (doit-true	(make-list-of-type-descr <top>-ctd)		<nelist>-ctd)
  (doit-false	(make-list-of-type-descr <fixnum>-ctd)		<list>-ctd)
  (doit-false	(make-list-of-type-descr <fixnum>-ctd)		<nelist>-ctd)
  (doit-true	(make-list-of-type-descr <fixnum>-ctd)		<null>-ctd)

;;; list-of/pair

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)	(make-pair-type-descr <fixnum>-ctd <null>-ctd))
  (doit-true	(make-list-of-type-descr <fixnum>-ctd)	(make-pair-type-descr <fixnum>-ctd (make-list-of-type-descr <fixnum>-ctd)))
  (doit-true	(make-list-of-type-descr <number>-ctd)	(make-pair-type-descr <fixnum>-ctd (make-list-type-descr (list <flonum>-ctd))))
  (doit-false	(make-list-of-type-descr <number>-ctd)	(make-pair-type-descr <fixnum>-ctd (make-list-type-descr (list <string>-ctd))))
  (doit-false	(make-list-of-type-descr <number>-ctd)	(make-pair-type-descr <string>-ctd <null>-ctd))

;;; list-of/pair-of

  (doit-true	(make-list-of-type-descr <null>-ctd)	(make-pair-of-type-descr <null>-ctd))
  (doit-true	(make-list-of-type-descr <list>-ctd)	(make-pair-of-type-descr <list>-ctd))
  (doit-false	(make-list-of-type-descr <fixnum>-ctd)	(make-pair-of-type-descr <fixnum>-ctd))

;;; list-of/list

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)	(make-list-type-descr (list <fixnum>-ctd)))
  (doit-true	(make-list-of-type-descr <number>-ctd)	(make-list-type-descr (list <fixnum>-ctd)))
  (doit-false	(make-list-of-type-descr <fixnum>-ctd)	(make-list-type-descr (list <number>-ctd)))

;;; list-of/list-of

  (doit-true	(make-list-of-type-descr <fixnum>-ctd)	(make-list-of-type-descr <fixnum>-ctd))
  (doit-true	(make-list-of-type-descr <number>-ctd)	(make-list-of-type-descr <fixnum>-ctd))
  (doit-false	(make-list-of-type-descr <fixnum>-ctd)	(make-list-of-type-descr <number>-ctd))
  (doit-false	(make-list-of-type-descr <string>-ctd)	(make-list-of-type-descr <fixnum>-ctd))

;;; --------------------------------------------------------------------
;;; vector

  (doit-true	<vector>-ctd		(make-vector-type-descr (list <fixnum>-ctd)))
  (doit-true	<nevector>-ctd		(make-vector-type-descr (list <fixnum>-ctd)))

  (doit-false	(make-vector-type-descr (list <top>-ctd))	<vector>-ctd)
  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))	<vector>-ctd)
  (doit-false	(make-vector-type-descr (list <top>-ctd))	<nevector>-ctd)
  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))	<nevector>-ctd)
  (doit-false	(make-vector-type-descr (list <fixnum>-ctd))	<empty-vector>-ctd)

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

  (doit-true	<vector>-ctd		(make-vector-of-type-descr <fixnum>-ctd))
  (doit-false	<nevector>-ctd		(make-vector-of-type-descr <fixnum>-ctd))

  (doit-true	(make-vector-of-type-descr <top>-ctd)		<vector>-ctd)
  (doit-true	(make-vector-of-type-descr <top>-ctd)		<nevector>-ctd)
  (doit-true	(make-vector-of-type-descr <top>-ctd)		<empty-vector>-ctd)
  (doit-false	(make-vector-of-type-descr <fixnum>-ctd)	<vector>-ctd)
  (doit-false	(make-vector-of-type-descr <fixnum>-ctd)	<nevector>-ctd)
  (doit-true	(make-vector-of-type-descr <fixnum>-ctd)	<empty-vector>-ctd)

;;; vector-of/vector

  (doit-true	(make-vector-of-type-descr <fixnum>-ctd)	(make-vector-type-descr (list <fixnum>-ctd)))
  (doit-true	(make-vector-of-type-descr <number>-ctd)	(make-vector-type-descr (list <fixnum>-ctd)))
  (doit-false	(make-vector-of-type-descr <fixnum>-ctd)	(make-vector-type-descr (list <number>-ctd)))

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

  (doit-true	(make-complement-type-descr <flonum>-ctd)		<fixnum>-ctd)
  (doit-false	(make-complement-type-descr <exact-integer>-ctd)	<fixnum>-ctd)
  (doit-true	(make-complement-type-descr <exact-integer>-ctd)	<flonum>-ctd)

  (doit-false	<fixnum>-ctd		(make-complement-type-descr <flonum>-ctd))
  (doit-false	<fixnum>-ctd		(make-complement-type-descr <fixnum>-ctd))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'matching-super-and-sub-syntax))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(type-descriptor-matching-super-and-sub? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-false
	(type-descriptor-matching-super-and-sub? ?super ?sub)))
      ))

;;; --------------------------------------------------------------------
;;; built-in types

  (doit-true	<top>			<top>)
  (doit-true	<top>			<number>)
  (doit-false	<number>		<top>)

  (doit-false	<top>			<void>)
  (doit-false	<top>			<no-return>)
  (doit-true	<void>			<void>)
  (doit-true	<no-return>		<no-return>)
  (doit-false	<void>			<top>)
  (doit-false	<no-return>		<top>)

  (doit-true	<top>			<struct>)
  (doit-true	<top>			<record>)
  (doit-true	<struct>		<record>)

  (doit-true	<list>			<null>)
  (doit-false	<nelist>		<null>)

  (doit-true	<vector>		<empty-vector>)
  (doit-false	<nevector>		<empty-vector>)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	<top>			alpha)
  (doit-true	<struct>		alpha)
  (doit-false	alpha			<top>)
  (doit-false	alpha			<struct>)
  (doit-false	alpha			beta)

  (doit-false	alpha			(or alpha <struct>))
  (doit-false	alpha			(or alpha beta))

  (doit-true	alpha			(and alpha <struct>))
  (doit-false	alpha			(and alpha beta))

  (doit-false	alpha			(not <top>))
  (doit-false	alpha			(not <fixnum>))
  (doit-false	alpha			(not alpha))
  (doit-false	alpha			(not beta))

  (doit-false	alpha			(ancestor-of <top>))
  (doit-false	alpha			(ancestor-of <struct>))
  (doit-false	alpha			(ancestor-of alpha))
  (doit-false	alpha			(ancestor-of beta))

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-true	<top>			<duo>)
  (doit-true	<struct>		<duo>)
  (doit-true	<record>		<duo>)
  (doit-false	<duo>			<top>)
  (doit-false	<duo>			<struct>)
  (doit-false	<duo>			<record>)

  (doit-true	<alpha>			<beta>)
  (doit-true	<beta>			<delta>)
  (doit-true	<alpha>			<delta>)
  (doit-false	<beta>			<alpha>)
  (doit-false	<delta>			<beta>)
  (doit-false	<delta>			<alpha>)
  (doit-false	<alpha>			<duo>)
  (doit-false	<duo>			<alpha>)

;;; --------------------------------------------------------------------
;;; simple condition type descriptors

  (doit-true	<top>			&condition)
  (doit-true	<struct>		&condition)
  (doit-true	<record>		&condition)
  (doit-true	&condition		&condition)

  (doit-true	<top>			&who)
  (doit-true	<struct>		&who)
  (doit-true	<record>		&who)
  (doit-true	&condition		&who)

;;; --------------------------------------------------------------------
;;; pair

  (doit-true	<pair>			(pair <fixnum> <flonum>))

  (doit-false	(pair <top> <null>)	<list>)
  (doit-false	(pair <top> <list>)	<list>)
  (doit-false	(pair <top> <null>)	<nelist>)
  (doit-true	(pair <top> <list>)	<nelist>)

  (doit-false	(pair <fixnum> <list>)	<list>)
  (doit-false	(pair <fixnum> <list>)	<nelist>)

;;; pair/pair

  (doit-true	(pair <fixnum> <string>)
		(pair <fixnum> <string>))

  (doit-true	(pair <number> <string>)
		(pair <fixnum> <string>))

  (doit-true	(pair <string> <number>)
		(pair <string> <fixnum>))

  (doit-false	(pair <fixnum> <string>)
		(pair <number> <string>))

  (doit-false	(pair <string> <fixnum>)
		(pair <string> <number>))

  (doit-false	(pair <fixnum> <string>)
		(pair <symbol> <string>))

  (doit-false	(pair <fixnum> <string>)
		(pair <fixnum> <symbol>))

;;; pair/pair-of

  (doit-true	(pair    <fixnum> <number>)
		(pair-of <fixnum>))

  (doit-false	(pair    <string> <number>)
		(pair-of <fixnum>))

  (doit-false	(pair    <fixnum> <string>)
		(pair-of <fixnum>))

;;; pair/list

  (doit-true	(pair <fixnum> <null>)			(list <fixnum>))
  (doit-false	(pair <fixnum> <flonum>)		(list <fixnum>))
  (doit-true	(pair <fixnum> (list-of <fixnum>))	(list <fixnum>))
  (doit-false	(pair <fixnum> (list <fixnum>))		(list <fixnum>))
  (doit-true	(pair <fixnum> (list <fixnum>))		(list <fixnum> <fixnum>))

;;; pair/list-of

  (doit-false	(pair <fixnum> (list-of <fixnum>))
		(list-of <fixnum>))

  (doit-false	(pair <fixnum> (list <fixnum>))
		(list-of <fixnum>))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-true	<pair>			(pair-of <fixnum>))

;;;

  (doit-false	(pair-of <null>)	<list>)
  (doit-false	(pair-of <list>)	<list>)
  (doit-false	(pair-of <fixnum>)	<list>)

  (doit-false	(pair-of <null>)	<nelist>)
  (doit-false	(pair-of <list>)	<nelist>)
  (doit-false	(pair-of <fixnum>)	<nelist>)

  (doit-false	(pair-of <null>)	<null>)
  (doit-false	(pair-of <list>)	<null>)
  (doit-false	(pair-of <fixnum>)	<null>)

;;; pair-of/pair

  (doit-true	(pair-of <list>)		(pair <list> <list>))
  (doit-true	(pair-of (or <number> <list>))	(pair <fixnum> <list>))

;;; pair-of/pair-of

  (doit-true	(pair-of <number>)		(pair-of <fixnum>))
  (doit-false	(pair-of <number>)		(pair-of <string>))

;;; pair-of/list

  (doit-true	(pair-of <list>)		(list <list> <list>))
  (doit-true	(pair-of (or <number> <list>))	(list <fixnum> <list>))

;;; pair-of/list-of

  (doit-true	(pair-of <number>)		(list-of <fixnum>))
  (doit-false	(pair-of <number>)		(list-of <string>))

;;; --------------------------------------------------------------------
;;; list

  (doit-true	<list>			(list <fixnum>))
  (doit-true	<nelist>		(list <fixnum>))

  (doit-false	(list <top>)		<list>)
  (doit-false	(list <top>)		<nelist>)
  (doit-false	(list <top>)		<null>)

  (doit-false	(list <fixnum>)		<list>)
  (doit-false	(list <fixnum>)		<nelist>)
  (doit-false	(list <fixnum>)		<null>)

;;; list/pair

  (doit-true	(list <fixnum>)			(pair <fixnum> <null>))
  (doit-false	(list <fixnum>)			(pair <fixnum> <string>))
  (doit-false	(list <fixnum> <flonum>)	(pair <fixnum> <null>))

;;; list/pair-of

  (doit-true	(list <null>)			(pair-of <null>))
  (doit-true	(list <list>)			(pair-of <null>))
  (doit-false	(list <fixnum>)			(pair-of <list>))

  (doit-true	(list (list <flonum>) <flonum>)	(pair-of (list <flonum>)))

;;; list/list

  (doit-true	(list <fixnum> <flonum>)	(list <fixnum> <flonum>))
  (doit-true	(list <fixnum> <number>)	(list <fixnum> <flonum>))
  (doit-false	(list <fixnum> <flonum>)	(list <fixnum> <string>))

;;; list/list-of

  (doit-false	(list <fixnum>)			(list-of <fixnum>))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-true	<list>			(list-of <fixnum>))
  (doit-false	<nelist>		(list-of <fixnum>))

  (doit-true	(list-of <top>)		<list>)
  (doit-true	(list-of <top>)		<nelist>)
  (doit-true	(list-of <top>)		<null>)

  (doit-false	(list-of <fixnum>)	<list>)
  (doit-false	(list-of <fixnum>)	<nelist>)
  (doit-true	(list-of <fixnum>)	<null>)

;;; list-of/pair

  (doit-true	(list-of <fixnum>)		(pair <fixnum> <null>))
  (doit-true	(list-of <fixnum>)		(pair <fixnum> (list-of <fixnum>)))
  (doit-true	(list-of <number>)		(pair <fixnum> (list <flonum>)))
  (doit-false	(list-of <number>)		(pair <fixnum> (list <string>)))
  (doit-false	(list-of <number>)		(pair <string> <null>))

;;; list-of/pair-of

  (doit-true	(list-of <null>)		(pair-of <null>))
  (doit-true	(list-of <list>)		(pair-of <list>))
  (doit-false	(list-of <fixnum>)		(pair-of <fixnum>))

;;; list-of/list

  (doit-true	(list-of <fixnum>)		(list <fixnum>))
  (doit-true	(list-of <number>)		(list <fixnum>))
  (doit-false	(list-of <fixnum>)		(list <number>))

;;; list-of/list-of

  (doit-true	(list-of <fixnum>)		(list-of <fixnum>))
  (doit-true	(list-of <number>)		(list-of <fixnum>))
  (doit-false	(list-of <fixnum>)		(list-of <number>))
  (doit-false	(list-of <string>)		(list-of <fixnum>))

;;; --------------------------------------------------------------------
;;; vector

  (doit-true	<vector>			(vector <fixnum>))
  (doit-true	<nevector>			(vector <fixnum>))
  (doit-false	(vector <top>)			<nevector>)
  (doit-false	(vector <fixnum>)		<nevector>)
  (doit-false	(vector <fixnum>)		<null>)

;;; vector/vector

  (doit-true	(vector <fixnum> <flonum>)	(vector <fixnum> <flonum>))
  (doit-true	(vector <fixnum> <number>)	(vector <fixnum> <flonum>))
  (doit-false	(vector <fixnum> <flonum>)	(vector <fixnum> <string>))

;;; vector/vector-of

  (doit-false	(vector <fixnum>)		(vector-of <fixnum>))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-true	<vector>			(vector-of <fixnum>))
  (doit-false	<nevector>			(vector-of <fixnum>))

  (doit-true	(vector-of <top>)		<vector>)
  (doit-true	(vector-of <top>)		<nevector>)
  (doit-true	(vector-of <true>)		<empty-vector>)

  (doit-false	(vector-of <fixnum>)		<vector>)
  (doit-false	(vector-of <fixnum>)		<nevector>)
  (doit-true	(vector-of <fixnum>)		<empty-vector>)

;;; vector-of/vector

  (doit-true	(vector-of <fixnum>)		(vector <fixnum>))
  (doit-true	(vector-of <number>)		(vector <fixnum>))
  (doit-false	(vector-of <fixnum>)		(vector <number>))

;;; vector-of/vector-of

  (doit-true	(vector-of <fixnum>)		(vector-of <fixnum>))
  (doit-true	(vector-of <number>)		(vector-of <fixnum>))
  (doit-false	(vector-of <fixnum>)		(vector-of <number>))
  (doit-false	(vector-of <string>)		(vector-of <fixnum>))

;;; --------------------------------------------------------------------
;;; compound condition-objects

  (doit-true	(condition &who)		(condition &who))
  (doit-true	(condition &condition)		(condition &who))
  (doit-true	(condition &condition)		(condition &who &irritants))
  (doit-false	(condition &who)		(condition &condition))
  (doit-true	(condition &who)		(condition &who &irritants))
  (doit-false	(condition &who &irritants)	(condition &who))
  (doit-true	(condition &who &irritants)	(condition &who &irritants))
  (doit-true	(condition &irritants &who)	(condition &who &irritants))

  (doit-true	(condition &who (condition &irritants))		(condition &who (condition &irritants)))
  (doit-true	(condition &irritants (condition &who))		(condition (condition &who) &irritants))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	<symbol>			(enumeration ciao))

  (doit-true	(enumeration ciao)		(enumeration ciao))
  (doit-false	(enumeration ciao)		(enumeration hello))
  (doit-false	(enumeration ciao hello)	(enumeration hello))
  (doit-true	(enumeration ciao)		(enumeration hello ciao))

;;; --------------------------------------------------------------------
;;; hashtable

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <fixnum>))

  (doit-false	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <string>))

  (doit-false	(hashtable <fixnum> <fixnum>)
		(hashtable <string> <fixnum>))

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <positive-fixnum> <fixnum>))

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <positive-fixnum>))

;;; --------------------------------------------------------------------
;;; alist

  (doit-true	(alist <fixnum> <fixnum>)
		(alist <fixnum> <fixnum>))

  (doit-false	(alist <fixnum> <fixnum>)
		(alist <fixnum> <string>))

  (doit-false	(alist <fixnum> <fixnum>)
		(alist <string> <fixnum>))

;;; --------------------------------------------------------------------
;;; union

  (doit-true	(or <fixnum> <flonum>)
		<fixnum>)

  (doit-true	(or <fixnum> <flonum>)
		<flonum>)

  (doit-false	(or <fixnum> <flonum>)
		<string>)

  (doit-true	(or <fixnum> <flonum>)
		(or <fixnum> <flonum>))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-true	(and <number> <flonum>)
		<flonum>)

  (doit-false	(and <number> <flonum>)
		<string>)

;;; --------------------------------------------------------------------
;;; complement

  (doit-false	(not <string>)		<top>)

  (doit-true	(not <flonum>)		<fixnum>)
  (doit-false	(not <flonum>)		<flonum>)
  (doit-false	(not <flonum>)		<positive-flonum>)

  (doit-false	<fixnum>		(not <flonum>))
  (doit-false	<fixnum>		(not <fixnum>))

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit-true	(ancestor-of <fixnum>)
		(ancestor-of <fixnum>))

  (doit-false	(ancestor-of <fixnum>)
		(ancestor-of <positive-fixnum>))

  (doit-false	(ancestor-of <fixnum>)			<fixnum>)
  (doit-true	(ancestor-of <fixnum>)			<exact-integer>)
  (doit-true	(ancestor-of <fixnum>)			<integer>)
  (doit-true	(ancestor-of <fixnum>)			<real>)
  (doit-true	(ancestor-of <fixnum>)			<complex>)
  (doit-true	(ancestor-of <fixnum>)			<number>)
  (doit-true	(ancestor-of <fixnum>)			<top>)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'descriptors-signature-matching))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?one ?two => ?expected)
       (check
	   (descriptors-signature-matching ?one ?two)
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------

  (doit (<top>) (<void>)	=> no-match)
  (doit (<void>) (<top>)	=> no-match)

  (doit (<top>) (<fixnum>)	=> exact-match)
  (doit (<fixnum>) (<top>)	=> possible-match)

;;; --------------------------------------------------------------------
;;; type unions

  (doit ((or <fixnum> <string>))
	(<fixnum>)
	=> exact-match)

  (doit ((or <fixnum> <string>))
	(<string>)
	=> exact-match)

  (doit (<fixnum>)
	((or <fixnum> <string>))
	=> possible-match)

  (doit (<string>)
	((or <fixnum> <string>))
	=> possible-match)

;;; --------------------------------------------------------------------
;;; type complement

  ;;"<top>" is an ancestor of "<string>".
  ;;
  (doit ((not <string>))		(<top>)			=> possible-match)
  (doit ((not <string>))		(<string>)		=> no-match)

;;;

  (doit ((not <fixnum>))
	(<positive-fixnum>)
	=> no-match)

  (doit ((not <fixnum>))
	(<fixnum>)
	=> no-match)

  ;;"<exact-integer>" is an ancestor of "<fixnum>".
  ;;
  (doit ((not <fixnum>))
	(<exact-integer>)
	=> possible-match)

  (doit ((not <fixnum>))
	(<bignum>)
	=> exact-match)

;;; --------------------------------------------------------------------
;;; type intersection

  (doit ((not <fixnum>))
	(<vector>)
	=> exact-match)

  (doit ((not <fixnum>))
	(<exact-integer>)
	=> possible-match)

  (doit ((not <string>))
	(<vector>)
	=> exact-match)

  (doit ((not <string>))
	(<exact-integer>)
	=> exact-match)

  (begin
    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<vector>)
	  => exact-match)

    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<fixnum>)
	  => no-match)

    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<string>)
	  => no-match)

    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<positive-fixnum>)
	  => no-match)

    ;;"<exact-integer>" is an ancestor of "<fixnum>".
    ;;
    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<exact-integer>)
	  => possible-match)
    #| end of BEGIN |# )

  (internal-body
    (define-type <it>
      (and (not <fixnum>)
	   (not <string>)))

    (doit (<it>)
	  (<vector>)
	  => exact-match)

    (doit (<it>)
	  (<fixnum>)
	  => no-match)

    (doit (<it>)
	  (<string>)
	  => no-match)

    (doit (<it>)
	  (<positive-fixnum>)
	  => no-match)

    ;;"<exact-integer>" is an ancestor of "<fixnum>".
    ;;
    (doit (<it>)
	  (<exact-integer>)
	  => possible-match)
    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit ((ancestor-of &condition))
	(<condition>)
	=> exact-match)

  (doit ((ancestor-of &condition))
	(<record>)
	=> exact-match)

  (doit ((ancestor-of &condition))
	(<struct>)
	=> exact-match)

  (doit ((ancestor-of &condition))
	(<top>)
	=> exact-match)

  (doit ((ancestor-of &condition))
	(<fixnum>)
	=> no-match)

  (doit ((ancestor-of &condition))
	(&condition)
	=> no-match)

  (doit ((ancestor-of &condition))
	(&who)
	=> no-match)

  (doit ((ancestor-of &condition))
	((condition &who &message))
	=> no-match)

  (doit ((ancestor-of &who))
	((condition &who &message))
	=> no-match)

;;; complement

  (doit ((not (ancestor-of &condition)))
	(&condition)
	=> exact-match)

  (doit ((not (ancestor-of &condition)))
	(<condition>)
	=> no-match)

  (doit ((not (ancestor-of &condition)))
	(<record>)
	=> no-match)

  (doit ((not (ancestor-of &condition)))
	(<struct>)
	=> no-match)

  (doit ((not (ancestor-of &condition)))
	(<top>)
	=> no-match)

;;;

  (doit (<fixnum>)
	(<zero>)
	=> possible-match)
  (doit ((ancestor-of <fixnum>))
	(<zero>)
	=> no-match)

  (doit ((not (ancestor-of <fixnum>)))
	(<zero>)
	=> possible-match)

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
