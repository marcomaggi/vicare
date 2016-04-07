;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: helper functions
;;;Date: Thu Apr  7, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.helpers)
  (export
    ;; conversion
    any->symbol				any->string

    ;; predicates
    always-true				always-false
    expect-single-argument-and-return-it
    expect-single-argument-and-return-true
    expect-single-argument-and-return-false)
  (import (except (vicare)
		  any->symbol		any->string
		  always-true		always-false
		  expect-single-argument-and-return-it
		  expect-single-argument-and-return-true
		  expect-single-argument-and-return-false))


;;;; conversion

(define* (any->symbol obj)
  (cond ((string? obj)
	 (string->symbol obj))
	((symbol? obj)
	 obj)
	(else
	 (procedure-argument-violation __who__
	   "invalid source object type for conversion" obj))))

(define* (any->string obj)
  (cond ((symbol? obj)
	 (symbol->string obj))
	((number? obj)
	 (number->string obj))
	(else
	 (procedure-argument-violation __who__
	   "invalid source object type for conversion" obj))))


;;;; predicates

(define (always-true . args)
  #t)

(define (always-false . args)
  #f)

(define (expect-single-argument-and-return-true arg)
  ;;This function is  used by some syntaxes  to check at run-time  that an expression
  ;;returns a single value.   If zero, two or more values  are returned: the built-in
  ;;validation mechanism raises  an exception at run-time; otherwise  the argument is
  ;;discarded and the return value is #t.
  ;;
  #t)

(define (expect-single-argument-and-return-false arg)
  ;;This function is  used by some syntaxes  to check at run-time  that an expression
  ;;returns a single value.   If zero, two or more values  are returned: the built-in
  ;;validation mechanism raises  an exception at run-time; otherwise  the argument is
  ;;discarded and the return value is #f.
  ;;
  #f)

(define (expect-single-argument-and-return-it arg)
  ;;This function is  used by some syntaxes  to check at run-time  that an expression
  ;;returns a single value.   If zero, two or more values  are returned: the built-in
  ;;validation  mechanism raises  an exception  at run-time;  otherwise the  argument
  ;;itself is returned.
  ;;
  arg)


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.helpers")))

#| end of library |# )

;;; end of file
