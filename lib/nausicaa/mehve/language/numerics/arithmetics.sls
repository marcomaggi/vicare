;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: arithmetics functions for Mehve
;;;Date: Fri Nov 29, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa mehve language numerics arithmetics)
  (export
    initialise-mehve-numerics-arithmetics

    + - * / abs
    div div0 mod mod0
    div-and-mod div0-and-mod0

    addition-0			subtraction-0
    addition-1			subtraction-1
    addition-2			subtraction-2

    multiplication-0		division-0
    multiplication-1		division-1
    multiplication-2		division-2)
  (import (except (nausicaa)
		  + - * / abs
		  div div0 mod mod0
		  div-and-mod div0-and-mod0)
    (prefix (only (nausicaa)
		  + - * / abs
		  div div0 mod mod0
		  div-and-mod div0-and-mod0)
	    nau.)
    (vicare unsafe operations))


;;;; helpers

(define-syntax (define-arithmetic-operator stx)
  (define (main stx)
    (syntax-case stx ()
      ((_ ?operator ?stem)
       (identifier? #'?who)
       (with-syntax
	   ((OPERATOR-0	(identifier-suffix #'?stem "-0"))
	    (OPERATOR-1	(identifier-suffix #'?stem "-1"))
	    (OPERATOR-2	(identifier-suffix #'?stem "-2"))
	    (OPERATOR	#'?stem))
	 #'(begin
	     (define-syntax (?operator stx)
	       (syntax-case stx ()
		 (??id
		  (identifier? #'??id)
		  #'OPERATOR)
		 ((_)
		  #'(OPERATOR-0))
		 ((_ ??a)
		  #'(OPERATOR-1 ??a))
		 ((_ ??a ??b)
		  #'(OPERATOR-2 ??a ??b))
		 ((_ ??a ??b ??c)
		  #'(OPERATOR-2 (OPERATOR-2 ??a ??b) ??c))
		 ((_ . ??args)
		  #'(OPERATOR . ??args))))

	     (define OPERATOR
	       (case-lambda
		(()
		 (OPERATOR-0))
		((arg)
		 (OPERATOR-1 arg))
		((A B)
		 (OPERATOR-2 A B))
		((A B C)
		 (OPERATOR-2 (OPERATOR-2 A B) C))
		((A B C . args)
		 (fold-left OPERATOR-2 (OPERATOR-2 (OPERATOR-2 A B) C) args))
		))

	     (define-generic OPERATOR-0 ())
	     (define-generic OPERATOR-1 (a))
	     (define-generic OPERATOR-2 (a b))
	     )))
      ))

  (main stx))


;;;; arithmetics operators

(define-arithmetic-operator +	addition)
(define-arithmetic-operator -	subtraction)
(define-arithmetic-operator *	multiplication)
(define-arithmetic-operator /	division)

;;; --------------------------------------------------------------------

(define-method (addition-0)
  ;;Weird but compliant with (rnrs base (6)).
  ;;
  0)

(define-method (addition-1 (a <number>))
  a)

;;; --------------------------------------------------------------------

(define-method (subtraction-0)
  ;;Weird, not compliant with (rnrs base (6)).
  ;;
  0)

;;; --------------------------------------------------------------------

(define-method (multiplication-0)
  ;;Weird but compliant with (rnrs base (6)).
  ;;
  1)

(define-method (multiplication-1 (a <number>))
  a)

;;; --------------------------------------------------------------------

(define-method (division-0)
  ;;Weird, not compliant with (rnrs base (6)).
  ;;
  +nan.0)


;;;; arithmetics: integer operations

(define-generic div		(a b))
(define-generic mod		(a b))
(define-generic div0		(a b))
(define-generic mod0		(a b))
(define-generic div-and-mod	(a b))
(define-generic div0-and-mod0	(a b))


;;;; arithmetics: absolute

(define-generic abs (x))


;;;; arithmetics: method additions

(define (initialise-mehve-numerics-arithmetics)
  (add-method addition-2	(<fixnum>	<fixnum>)	(lambda (x y) ($fx+ x y)))
  (add-method addition-2	(<flonum>	<flonum>)	(lambda (x y) ($fl+ x y)))
  (add-method addition-2	(<number>	<number>)	nau.+)

  (add-method subtraction-1	(<fixnum>)			(lambda (x) ($fx- x)))
  (add-method subtraction-1	(<flonum>)			(lambda (x) ($fl- x)))
  (add-method subtraction-1	(<number>)			nau.-)

  (add-method subtraction-2	(<fixnum>	<fixnum>)	(lambda (x y) ($fx- x y)))
  (add-method subtraction-2	(<flonum>	<flonum>)	(lambda (x y) ($fl- x y)))
  (add-method subtraction-2	(<number>	<number>)	nau.-)

  (add-method multiplication-2	(<fixnum>	<fixnum>)	(lambda (x y) ($fx* x y)))
  (add-method multiplication-2	(<flonum>	<flonum>)	(lambda (x y) ($fl* x y)))
  (add-method multiplication-2	(<number>	<number>)	nau.*)

  ;;We want  to return exact rationals,  so we use the  following rather
  ;;than FXDIV.
  (add-method division-1	(<fixnum>)			nau./)
  (add-method division-1	(<flonum>)			(lambda (x) ($fl/ 1.0 x)))
  (add-method division-1	(<number>)			nau./)

  ;;We want  to return exact rationals,  so we use the  following rather
  ;;than FXDIV.
  (add-method division-2	(<fixnum>	<fixnum>)	nau./)
  (add-method division-2	(<flonum>	<flonum>)	(lambda (x y) ($fl/ x y)))
  (add-method division-2	(<number>	<number>)	nau./)

  (add-method div		(<fixnum>	<fixnum>)	$fxdiv)
  (add-method div		(<integer>	<integer>)	nau.div)
  (add-method div		(<real>		<real>)		nau.div)
  (add-method div0		(<fixnum>	<fixnum>)	$fxdiv0)
  (add-method div0		(<integer>	<integer>)	nau.div0)
  (add-method div0		(<real>		<real>)		nau.div0)

  (add-method mod		(<fixnum>	<fixnum>)	$fxmod)
  (add-method mod		(<integer>	<integer>)	nau.mod)
  (add-method mod		(<real>		<real>)		nau.mod)
  (add-method mod0		(<fixnum>	<fixnum>)	$fxmod0)
  (add-method mod0		(<integer>	<integer>)	nau.mod0)
  (add-method mod0		(<real>		<real>)		nau.mod0)

  (add-method div-and-mod	(<fixnum>	<fixnum>)	$fxdiv-and-mod)
  (add-method div-and-mod	(<integer>	<integer>)	nau.div-and-mod)
  (add-method div-and-mod	(<real>		<real>)		nau.div-and-mod)
  (add-method div0-and-mod0	(<fixnum>	<fixnum>)	$fxdiv0-and-mod0)
  (add-method div0-and-mod0	(<integer>	<integer>)	nau.div0-and-mod0)
  (add-method div0-and-mod0	(<real>		<real>)		nau.div0-and-mod0)

  (add-method abs		(<fixnum>)			$fxabs)
  (add-method abs		(<flonum>)			$flabs)
  (add-method abs		(<real>)			nau.abs)

  #| end of initialisation function |# )


;;;; done

)

;;; end of file
