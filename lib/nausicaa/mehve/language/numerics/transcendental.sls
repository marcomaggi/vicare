;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: numeric transcendental functions for Mehve
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
(library (nausicaa mehve language numerics transcendental)
  (export
    initialise-mehve-numerics-transcendental

    expt sqrt cbrt square cube exp log
    sin cos tan asin acos atan
    sinh cosh tanh asinh acosh atanh

    log-1	log-2
    atan-1	atan-2)
  (import (except (nausicaa)
		  expt sqrt cbrt square cube exp log
		  sin cos tan asin acos atan
		  sinh cosh tanh asinh acosh atanh)
    (prefix (only (nausicaa)
		  expt sqrt cbrt square cube exp log
		  sin cos tan asin acos atan
		  sinh cosh tanh asinh acosh atanh)
	    nau.)
    (vicare unsafe operations)
    (vicare system $numerics))


;;;; transcendental: exponentials and logarithms

(define-generic expt	(x y))
(define-generic sqrt	(x))
(define-generic cbrt	(x))
(define-generic square	(x))
(define-generic cube	(x))
(define-generic exp	(x))

;;; --------------------------------------------------------------------

(define-syntax (log stx)
  (syntax-case stx ()
    (?id
     (identifier? #'?id)
     #'log-N)
    ((_ ?x)
     #'(log-1 ?x))
    ((_ ?x ?base)
     #'(log-2 ?x ?base))
    ))

(define log-N
  (case-lambda
   ((X)
    (log-1 X))
   ((X Y)
    (log-2 X Y))))

(define-generic log-1	(x))
(define-generic log-2	(x base))


;;;; transcendental: trigonometric

(define-generic sin	(x))
(define-generic cos	(x))
(define-generic tan	(x))

(define-generic asin	(x))
(define-generic acos	(x))

;;; --------------------------------------------------------------------

(define-syntax (atan stx)
  (syntax-case stx ()
    (?id
     (identifier? #'?id)
     #'atan-N)
    ((_ ?x)
     #'(atan-1 ?x))
    ((_ ?x ?y)
     #'(atan-2 ?x ?y))
    ))

(define atan-N
  (case-lambda
   ((X)
    (atan-1 X))
   ((X Y)
    (atan-2 X Y))))

(define-generic atan-1	(x))
(define-generic atan-2	(x y))


;;;; transcendental: hyperbolic

(define-generic sinh	(x))
(define-generic cosh	(x))
(define-generic tanh	(x))

(define-generic asinh	(x))
(define-generic acosh	(x))
(define-generic atanh	(x))


(define (initialise-mehve-numerics-transcendental)
  ;; exponentials and logarithms
  (add-method square (<flonum>)			$square-fixnum)
  (add-method square (<bignum>)			$square-bignum)
  (add-method square (<ratnum>)			$square-ratnum)
  (add-method square (<flonum>)			$flsquare)
  (add-method square (<cflonum>)		$square-cflonum)
  (add-method square (<compnum>)		$square-compnum)
  (add-method square (<complex>)		nau.square)

  (add-method cube (<flonum>)			$cube-fixnum)
  (add-method cube (<bignum>)			$cube-bignum)
  (add-method cube (<ratnum>)			$cube-ratnum)
  (add-method cube (<flonum>)			$flcube)
  (add-method cube (<cflonum>)			$cube-cflonum)
  (add-method cube (<compnum>)			$cube-compnum)
  (add-method cube (<number>)			nau.cube)

  (add-method expt (<flonum>	<flonum>)	$expt-flonum-flonum)
  (add-method expt (<complex>	<complex>)	nau.expt)

  (add-method sqrt (<flonum>)			$flsqrt)
  (add-method sqrt (<complex>)			nau.sqrt)

  (add-method cbrt (<flonum>)			$flcbrt)
  (add-method cbrt (<complex>)			nau.cbrt)

  (add-method exp (<flonum>)			$flexp)
  (add-method exp (<complex>)			nau.exp)

  ;;We use $LOG-FLONUM, rather than $FLLOG,  because the result can be a
  ;;real or a complex.
  (add-method log-1 (<flonum>)			$log-flonum)
  (add-method log-1 (<complex>)			nau.log)
  (add-method log-2 (<flonum>	<flonum>)	$fllog2)
  (add-method log-2 (<complex>	<complex>)	nau.log)

  ;; trigonometric
  (add-method sin (<fixnum>)			$sin-fixnum)
  (add-method sin (<bignum>)			$sin-bignum)
  (add-method sin (<ratnum>)			$sin-ratnum)
  (add-method sin (<flonum>)			$sin-flonum)
  (add-method sin (<cflonum>)			$sin-cflonum)
  (add-method sin (<compnum>)			$sin-compnum)
  (add-method sin (<complex>)			nau.sin)

  (add-method cos (<fixnum>)			$cos-fixnum)
  (add-method cos (<bignum>)			$cos-bignum)
  (add-method cos (<ratnum>)			$cos-ratnum)
  (add-method cos (<flonum>)			$cos-flonum)
  (add-method cos (<cflonum>)			$cos-cflonum)
  (add-method cos (<compnum>)			$cos-compnum)
  (add-method cos (<complex>)			nau.cos)

  (add-method tan (<fixnum>)			$tan-fixnum)
  (add-method tan (<bignum>)			$tan-bignum)
  (add-method tan (<ratnum>)			$tan-ratnum)
  (add-method tan (<flonum>)			$tan-flonum)
  (add-method tan (<cflonum>)			$tan-cflonum)
  (add-method tan (<compnum>)			$tan-compnum)
  (add-method tan (<complex>)			nau.tan)

  ;;We use $ASIN-FLONUM, rather than  $FLASIN, because the result can be
  ;;a real or a complex.
  (add-method asin (<fixnum>)			$asin-fixnum)
  (add-method asin (<bignum>)			$asin-bignum)
  (add-method asin (<ratnum>)			$asin-ratnum)
  (add-method asin (<flonum>)			$asin-flonum)
  (add-method asin (<cflonum>)			$asin-cflonum)
  (add-method asin (<compnum>)			$asin-compnum)
  (add-method asin (<complex>)			nau.asin)

  ;;We use $ACOS-FLONUM, rather than  $FLACOS, because the result can be
  ;;a real or a complex.
  (add-method acos (<fixnum>)			$acos-fixnum)
  (add-method acos (<bignum>)			$acos-bignum)
  (add-method acos (<ratnum>)			$acos-ratnum)
  (add-method acos (<flonum>)			$acos-flonum)
  (add-method acos (<cflonum>)			$acos-cflonum)
  (add-method acos (<compnum>)			$acos-compnum)
  (add-method acos (<complex>)			nau.acos)

  (add-method atan-1 (<fixnum>)			$atan-fixnum)
  (add-method atan-1 (<bignum>)			$atan-bignum)
  (add-method atan-1 (<ratnum>)			$atan-ratnum)
  (add-method atan-1 (<flonum>)			$atan-flonum)
  (add-method atan-1 (<cflonum>)		$atan-cflonum)
  (add-method atan-1 (<compnum>)		$atan-compnum)
  (add-method atan-1 (<complex>)		nau.atan)

  (add-method atan-2 (<flonum> <flonum>)	$flatan2)
  (add-method atan-2 (<real> <real>)		nau.atan)

  ;; hyperbolic
  (add-method sinh (<fixnum>)			$sinh-fixnum)
  (add-method sinh (<bignum>)			$sinh-bignum)
  (add-method sinh (<ratnum>)			$sinh-ratnum)
  (add-method sinh (<flonum>)			$sinh-flonum)
  (add-method sinh (<cflonum>)			$sinh-cflonum)
  (add-method sinh (<compnum>)			$sinh-compnum)
  (add-method sinh (<complex>)			nau.sinh)

  (add-method cosh (<fixnum>)			$cosh-fixnum)
  (add-method cosh (<bignum>)			$cosh-bignum)
  (add-method cosh (<ratnum>)			$cosh-ratnum)
  (add-method cosh (<flonum>)			$cosh-flonum)
  (add-method cosh (<cflonum>)			$cosh-cflonum)
  (add-method cosh (<compnum>)			$cosh-compnum)
  (add-method cosh (<complex>)			nau.cosh)

  (add-method tanh (<fixnum>)			$tanh-fixnum)
  (add-method tanh (<bignum>)			$tanh-bignum)
  (add-method tanh (<ratnum>)			$tanh-ratnum)
  (add-method tanh (<flonum>)			$tanh-flonum)
  (add-method tanh (<cflonum>)			$tanh-cflonum)
  (add-method tanh (<compnum>)			$tanh-compnum)
  (add-method tanh (<complex>)			nau.tanh)

  (add-method asinh (<fixnum>)			$asinh-fixnum)
  (add-method asinh (<bignum>)			$asinh-bignum)
  (add-method asinh (<ratnum>)			$asinh-ratnum)
  (add-method asinh (<flonum>)			$asinh-flonum)
  (add-method asinh (<cflonum>)			$asinh-cflonum)
  (add-method asinh (<compnum>)			$asinh-compnum)
  (add-method asinh (<complex>)			nau.asinh)

  (add-method acosh (<fixnum>)			$acosh-fixnum)
  (add-method acosh (<bignum>)			$acosh-bignum)
  (add-method acosh (<ratnum>)			$acosh-ratnum)
  (add-method acosh (<flonum>)			$acosh-flonum)
  (add-method acosh (<cflonum>)			$acosh-cflonum)
  (add-method acosh (<compnum>)			$acosh-compnum)
  (add-method acosh (<complex>)			nau.acosh)

  (add-method atanh (<fixnum>)			$atanh-fixnum)
  (add-method atanh (<bignum>)			$atanh-bignum)
  (add-method atanh (<ratnum>)			$atanh-ratnum)
  (add-method atanh (<flonum>)			$atanh-flonum)
  (add-method atanh (<cflonum>)			$atanh-cflonum)
  (add-method atanh (<compnum>)			$atanh-compnum)
  (add-method atanh (<complex>)			nau.atanh)

  #| end of initialisation function |# )


;;;; done

)

;;; end of file
