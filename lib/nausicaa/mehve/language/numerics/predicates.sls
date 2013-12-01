;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: numerics predicates for Mehve
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
(library (nausicaa mehve language numerics predicates)
  (export
    initialise-mehve-numerics-predicates

    = < > <= >=
    zero? positive? negative? non-negative? non-positive?
    odd? even?
    finite? infinite? nan?

    equal-predicate-0
    equal-predicate-1
    equal-predicate-2

    less-than-predicate-0		greater-than-predicate-0
    less-than-predicate-1		greater-than-predicate-1
    less-than-predicate-2		greater-than-predicate-2

    less-than-or-equal-to-predicate-0	greater-than-or-equal-to-predicate-0
    less-than-or-equal-to-predicate-1	greater-than-or-equal-to-predicate-1
    less-than-or-equal-to-predicate-2	greater-than-or-equal-to-predicate-2)
  (import (except (nausicaa)
		  = < > <= >=
		  zero? positive? negative? non-negative? non-positive?
		  odd? even?
		  finite? infinite? nan?)
    (prefix (only (nausicaa)
		  = < > <= >=
		  zero? positive? negative? non-negative? non-positive?
		  odd? even?
		  finite? infinite? nan?)
	    nau.)
    (vicare unsafe operations))


;;;; helpers

(define-syntax (define-comparison-operator stx)
  (define (main stx)
    (syntax-case stx ()
      ((_ ?operator ?stem)
       (identifier? #'?who)
       (with-syntax
	   ((PREDICATE-0	(identifier-suffix #'?stem "-0"))
	    (PREDICATE-1	(identifier-suffix #'?stem "-1"))
	    (PREDICATE-2	(identifier-suffix #'?stem "-2"))
	    (PREDICATE-N	#'?stem))
	 #'(begin
	     (define-syntax (?operator stx)
	       (syntax-case stx ()
		 (??id
		  (identifier? #'??id)
		  #'PREDICATE)
		 ((_)
		  #'(PREDICATE-0))
		 ((_ ??a)
		  #'(PREDICATE-1 ??a))
		 ((_ ??a ??b)
		  #'(PREDICATE-2 ??a ??b))
		 ((_ ??a ??b ??c)
		  #'(and (PREDICATE-2 ??a ??b)
			 (PREDICATE-2 ??b ??c)))
		 ((_ . ??args)
		  #'(PREDICATE . ??args))))

	     (define PREDICATE
	       (case-lambda
		(()
		 (PREDICATE-0))
		((arg)
		 (PREDICATE-1 arg))
		((A B . args)
		 (let loop ((A		A)
			    (B		B)
			    (args	args))
		   (and (PREDICATE-2 A B)
			(or (null? args)
			    (loop B ($car args) ($cdr args))))))))

	     (define-generic PREDICATE-0 ())
	     (define-generic PREDICATE-1 (a))
	     (define-generic PREDICATE-2 (a b))
	     )))
      ))

  (main stx))


;;;; numeric predicates: comparison operators

(define-comparison-operator =	equal-predicate)
(define-comparison-operator <	less-than-predicate)
(define-comparison-operator >	greater-than-predicate)
(define-comparison-operator <=	less-than-or-equal-to-predicate)
(define-comparison-operator >=	greater-than-or-equal-to-predicate)

;;; --------------------------------------------------------------------

(define-method (equal-predicate-0)
  #t)

(define-method (equal-predicate-1 x)
  #t)

;;; --------------------------------------------------------------------

(define-method (less-than-predicate-0)
  #f)

(define-method (less-than-predicate-1 x)
  #f)

;;; --------------------------------------------------------------------

(define-method (greater-than-predicate-0)
  #f)

(define-method (greater-than-predicate-1 x)
  #f)

;;; --------------------------------------------------------------------

(define-method (less-than-or-equal-to-predicate-0)
  #t)

(define-method (less-than-or-equal-to-predicate-1 x)
  #t)

;;; --------------------------------------------------------------------

(define-method (greater-than-or-equal-to-predicate-0)
  #t)

(define-method (greater-than-or-equal-to-predicate-1 x)
  #t)


;;;; numeric predicates: other predicates

(define-generic zero?		(x))
(define-generic positive?	(x))
(define-generic negative?	(x))
(define-generic non-negative?	(x))
(define-generic non-positive?	(x))
(define-generic odd?		(x))
(define-generic even?		(x))
(define-generic finite?		(x))
(define-generic infinite?	(x))
(define-generic nan?		(x))

(define-method (finite? (x <fixnum>))
  #t)

(define-method (infinite? (x <fixnum>))
  #f)

(define-method (nan? (x <fixnum>))
  #f)


(define (initialise-mehve-numerics-predicates)

  (add-method equal-predicate-2				(<fixnum> <fixnum>)	(lambda (a b) ($fx= a b)))
  (add-method equal-predicate-2				(<flonum> <flonum>)	(lambda (a b) ($fl= a b)))
  (add-method equal-predicate-2				(<complex> <complex>)	nau.=)

  (add-method less-than-predicate-2			(<fixnum> <fixnum>)	(lambda (a b) ($fx< a b)))
  (add-method less-than-predicate-2			(<flonum> <flonum>)	(lambda (a b) ($fl< a b)))
  (add-method less-than-predicate-2			(<real>   <real>)	nau.<)

  (add-method less-than-or-equal-to-predicate-2		(<fixnum> <fixnum>)	(lambda (a b) ($fx<= a b)))
  (add-method less-than-or-equal-to-predicate-2		(<flonum> <flonum>)	(lambda (a b) ($fl<= a b)))
  (add-method less-than-or-equal-to-predicate-2		(<real>   <real>)	nau.<=)

  (add-method greater-than-predicate-2			(<fixnum> <fixnum>)	(lambda (a b) ($fx> a b)))
  (add-method greater-than-predicate-2			(<flonum> <flonum>)	(lambda (a b) ($fl> a b)))
  (add-method greater-than-predicate-2			(<real>   <real>)	nau.>)

  (add-method greater-than-or-equal-to-predicate-2	(<fixnum> <fixnum>)	(lambda (a b) ($fx>= a b)))
  (add-method greater-than-or-equal-to-predicate-2	(<flonum> <flonum>)	(lambda (a b) ($fl>= a b)))
  (add-method greater-than-or-equal-to-predicate-2	(<real>	  <real>)	nau.>=)

  (add-method zero?		(<fixnum>)	$fxzero?)
  (add-method positive?		(<fixnum>)	$fxpositive?)
  (add-method negative?		(<fixnum>)	$fxnegative?)
  (add-method odd?		(<fixnum>)	$fxodd?)
  (add-method even?		(<fixnum>)	$fxeven?)

  (add-method non-negative?	(<fixnum>)	$fxnonnegative?)
  (add-method non-positive?	(<fixnum>)	$fxnonpositive?)

  (add-method zero?		(<flonum>)	$flzero?)
  (add-method positive?		(<flonum>)	$flpositive?)
  (add-method negative?		(<flonum>)	$flnegative?)
  (add-method odd?		(<flonum>)	$flodd?)
  (add-method even?		(<flonum>)	$fleven?)
  (add-method finite?		(<flonum>)	$flfinite?)
  (add-method infinite?		(<flonum>)	$flinfinite?)
  (add-method nan?		(<flonum>)	$flnan?)

  (add-method zero?		(<number>)	nau.zero?)
  (add-method positive?		(<real>)	nau.positive?)
  (add-method negative?		(<real>)	nau.negative?)
  (add-method non-negative?	(<real>)	nau.non-negative?)
  (add-method non-positive?	(<real>)	nau.non-positive?)
  (add-method odd?		(<real>)	nau.odd?)
  (add-method even?		(<real>)	nau.even?)
  (add-method finite?		(<complex>)	nau.finite?)
  (add-method infinite?		(<complex>)	nau.infinite?)
  (add-method nan?		(<complex>)	nau.nan?)

  #| end of initialisation function |# )


;;;; done

)

;;; end of file
