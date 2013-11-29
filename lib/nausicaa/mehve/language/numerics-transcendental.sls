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
(library (nausicaa mehve language numerics-transcendental)
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
    (vicare unsafe operations))




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
  (add-method square (<flonum>)			$flsquare)
  (add-method square (<number>)			nau.square)

  (add-method cube (<flonum>)			$flcube)
  (add-method cube (<number>)			nau.cube)

  (add-method expt (<flonum>	<flonum>)	$flexpt)
  (add-method expt (<complex>	<complex>)	nau.expt)

  (add-method sqrt (<flonum>)			$flsqrt)
  (add-method sqrt (<complex>)			nau.sqrt)

  (add-method cbrt (<flonum>)			$flcbrt)
  (add-method cbrt (<complex>)			nau.cbrt)

  (add-method exp (<flonum>)			$flexp)
  (add-method exp (<complex>)			nau.exp)

  (add-method log-1 (<flonum>)			$fllog)
  (add-method log-1 (<complex>)			nau.log)
  (add-method log-2 (<flonum>	<flonum>)	$fllog2)
  (add-method log-2 (<complex>	<complex>)	nau.log)

  ;; trigonometric
  (add-method sin (<flonum>)			$flsin)
  (add-method sin (<real>)			nau.sin)
  (add-method sin (<complex>)			nau.sin)

  (add-method cos (<flonum>)			$flcos)
  (add-method cos (<real>)			nau.cos)
  (add-method cos (<complex>)			nau.cos)

  (add-method tan (<flonum>)			$fltan)
  (add-method tan (<real>)			nau.tan)
  (add-method tan (<complex>)			nau.tan)

  (add-method asin (<flonum>)			$flasin)
  (add-method asin (<real>)			nau.asin)
  (add-method asin (<complex>)			nau.asin)

  (add-method acos (<flonum>)			$flacos)
  (add-method acos (<real>)			nau.acos)
  (add-method acos (<complex>)			nau.acos)

  (add-method atan-1 (<flonum>)			$flatan)
  (add-method atan-1 (<real>)			nau.atan)
  (add-method atan-1 (<complex>)		nau.atan)

  (add-method atan-2 (<flonum> <flonum>)	$flatan2)
  (add-method atan-2 (<real> <real>)		nau.atan)

  ;; hyperbolic
  (add-method sinh (<flonum>)			$flsinh)
  (add-method sinh (<real>)			nau.sinh)
  (add-method sinh (<complex>)			nau.sinh)

  (add-method cosh (<flonum>)			$flcosh)
  (add-method cosh (<real>)			nau.cosh)
  (add-method cosh (<complex>)			nau.cosh)

  (add-method tanh (<flonum>)			$fltanh)
  (add-method tanh (<real>)			nau.tanh)
  (add-method tanh (<complex>)			nau.tanh)

  (add-method asinh (<flonum>)			$flasinh)
  (add-method asinh (<real>)			nau.asinh)
  (add-method asinh (<complex>)			nau.asinh)

  (add-method acosh (<flonum>)			$flacosh)
  (add-method acosh (<real>)			nau.acosh)
  (add-method acosh (<complex>)			nau.acosh)

  (add-method atanh (<flonum>)			$flatanh)
  (add-method atanh (<real>)			nau.atanh)
  (add-method atanh (<complex>)			nau.atanh)

  #| end of initialisation function |# )


;;;; done

)

;;; end of file
