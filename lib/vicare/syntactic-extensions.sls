;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: utility syntaxes
;;;Date: Fri Oct 21, 2011
;;;
;;;Abstract
;;;
;;;	This library is both  installed and used when expanding Vicare's
;;;	own source code.  For this  reason it must export only: bindings
;;;	imported  by Vicare itself,  syntaxes whose  expansion reference
;;;	only bindings imported by Vicare itself.
;;;
;;;Copyright (C) 2011, 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;;copyright notice for RECEIVE
;;;
;;;Copyright (C) John David Stone (1999). All Rights Reserved.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(library (vicare syntactic-extensions)
  (export
    ;; miscellaneous extensions
    define-inline		define-inline-constant
    define-constant
    define-syntax*		define-auxiliary-syntaxes
    let-inline			let*-inline
    debug-assert		unwind-protect
    begin0			begin0-let
    with-pathnames
    with-bytevectors		with-bytevectors/or-false
    callet			callet*
    receive

    ;; arguments validation
    define-argument-validation
    with-arguments-validation
    with-dangerous-arguments-validation
    arguments-validation-forms

    ;; miscellaneous dispatching
    case-word-size		case-endianness
    case-fixnums		case-integers
    case-symbols
    define-exact-integer->symbol-function
    cond-numeric-operand	cond-real-numeric-operand
    cond-exact-integer-operand	cond-inexact-integer-operand
    cond-exact-real-numeric-operand

    ;; auxiliary syntaxes
    big				little)
  (import (ikarus)
    (for (prefix (vicare installation-configuration)
		 config.)
	 expand)
    (only (vicare arguments validation)
	  define-argument-validation
	  with-arguments-validation
	  with-dangerous-arguments-validation
	  arguments-validation-forms))


;;;; some defining syntaxes

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

(define-syntax define-constant
  (syntax-rules ()
    ((_ ?name ?expr)
     (begin
       (define ghost ?expr)
       (define-syntax ?name
	 (identifier-syntax ghost))))))

#;(define-syntax define-inline-constant
  (syntax-rules ()
    ((_ ?name ?value)
     (define-syntax ?name (identifier-syntax ?value)))))
(define-syntax define-inline-constant
  ;;We want to allow a generic expression to generate the constant value
  ;;at expand time.
  ;;
  (syntax-rules ()
    ((_ ?name ?expr)
     (define-syntax ?name
       (let ((const ?expr))
	 (lambda (stx)
	   (syntax-case stx ()
	     (?id
	      (identifier? #'?id)
	      (with-syntax ((VALUE const))
		#'(quote VALUE))))))))))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (?who ?stx) . ?body)
     (define-syntax ?who (lambda (?stx) . ?body)))))

(define-syntax define-auxiliary-syntaxes
  (syntax-rules ()
    ((_ ?name)
     (define-syntax ?name (syntax-rules ())))
    ((_ ?name0 ?name ...)
     (begin
       (define-syntax ?name0 (syntax-rules ()))
       (define-auxiliary-syntaxes ?name ...)))
    ((_)	;allows this  syntax to be called with  no arguments and
		;still expand to a definition
     (define-syntax dummy (syntax-rules ())))
    ))

;;; --------------------------------------------------------------------

(define-syntax let-inline
  (syntax-rules ()
    ((_ ((?var ?expr) ...) ?body0 . ?body)
     (let-syntax ((?var (identifier-syntax ?expr)) ...)
       ?body0 . ?body))))

(define-syntax let*-inline
  (syntax-rules ()
    ((_ () ?body0 . ?body)
     (begin ?body0 . ?body))
    ((_ ((?var0 ?expr0) (?var ?expr) ...) ?body0 . ?body)
     (let-syntax ((?var0 (identifier-syntax ?expr0)))
       (let*-inline ((?var ?expr) ...)
	 ?body0 . ?body)))))

(define-syntax callet
  ;;Transforms:
  ;;
  ;;   (callet printf (string "ciao ~a") (arg 123))
  ;;
  ;;into:
  ;;
  ;;   (printf "ciao ~a" 123)
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?func ?arg ...)
       (let loop ((args		#'(?arg ...))
		  (keys		'())
		  (exprs	'()))
	 (syntax-case args ()
	   (()
	    #`(?func . #,(reverse exprs)))
	   (((?key ?expr) . ?args)
	    (identifier? #'?key)
	    (loop #'?args
		  (cons #'?key  keys)
		  (cons #'?expr exprs)))
	   (((?key ?expr) . ?args)
	    (syntax-violation 'callet
	      "expected identifier as argument key" stx #'(?key ?expr)))
	   ((?arg . ?args)
	    (loop #'?args
		  (cons (car (generate-temporaries '(#f))) keys)
		  (cons #'?arg exprs)))
	   ))))))

(define-syntax callet*
  ;;Transforms:
  ;;
  ;;   (callet printf (string "ciao ~a") (arg 123))
  ;;
  ;;into:
  ;;
  ;;   (let* ((string "ciao ~a")
  ;;          (arg    123))
  ;;     (printf string arg))
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?func ?arg ...)
       (let loop ((args		#'(?arg ...))
		  (keys		'())
		  (exprs	'()))
	 (syntax-case args ()
	   (()
	    (with-syntax (((KEY  ...) (reverse keys))
			  ((EXPR ...) (reverse exprs)))
	      #`(let* ((KEY EXPR) ...)
		  (?func KEY ...))))
	   (((?key ?expr) . ?args)
	    (identifier? #'?key)
	    (loop #'?args
		  (cons #'?key  keys)
		  (cons #'?expr exprs)))
	   (((?key ?expr) . ?args)
	    (syntax-violation 'callet
	      "expected identifier as argument key" stx #'(?key ?expr)))
	   ((?arg . ?args)
	    (loop #'?args
		  (cons (car (generate-temporaries '(#f))) keys)
		  (cons #'?arg exprs)))
	   ))))))


;;;; other syntaxes

(define-syntax begin0
  ;;This  syntax  comes from  the  R6RS  original  document, Appendix  A
  ;;``Formal semantics''.
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda args
	 ?expr ...
	 (apply values args))))))

(define-syntax begin0-let
  (syntax-rules ()
    ((_ ((?var0 ?init0) (?var ?init) ...) ?form0 ?form ...)
     (let ((?var0 ?init0)
	   (?var  ?init)
	   ...)
       ?form0 ?form ...
       ?var0))))

(define-syntax unwind-protect
  ;;Not a  general UNWIND-PROTECT for Scheme,  but fine where  we do not
  ;;use continuations to escape from the body.
  ;;
  (syntax-rules ()
    ((_ ?body ?cleanup0 ?cleanup ...)
     (let ((cleanup (lambda () ?cleanup0 ?cleanup ...)))
       (with-exception-handler
	   (lambda (E)
	     (cleanup)
	     (raise E))
	 (lambda ()
	   (begin0
	       ?body
	     (cleanup))
	   #;(call-with-values
	       (lambda () ?body)
	     (lambda return-values
	       (cleanup)
	       (apply values return-values)))
	   ))))))

(define-syntax debug-assert
  ;;This is meant to expand to nothing when debugging is turned off.
  ;;
  (if #t
      (syntax-rules ()
  	((_ ?pred)
  	 (assert ?pred)))
    (syntax-rules ()
      ((_ ?pred)
       (values)))))

(define-syntax with-pathnames
  (syntax-rules ()
    ((_ ((?pathname.bv ?pathname) ...) . ?body)
     (let ((?pathname.bv (let ((pathname ?pathname))
			   (if (bytevector? pathname)
			       pathname
			     ((string->filename-func) pathname))))
	   ...)
       . ?body))))

(define-syntax with-bytevectors
  ;;Used to  preprocess function arguments which must  be bytevectors or
  ;;strings;  the  strings are  converted  to  bytevectors.  This  macro
  ;;assumes that the arguments have already been validated.
  ;;
  ;;The ?VALUE.BV and ?VALUE input forms must be identifiers.
  ;;
  (syntax-rules ()
    ((_ ((?value.bv ?value) ...) . ?body)
     (let ((?value.bv (let ((V ?value))
			(if (bytevector? V)
			    V
			  (string->latin1 V))))
	   ...)
       . ?body))))

(define-syntax with-bytevectors/or-false
  ;;Used  to preprocess  function arguments  which must  be bytevectors,
  ;;strings or  false; the strings  are converted to  bytevectors.  This
  ;;macro assumes that the arguments have already been validated.
  ;;
  ;;The ?VALUE.BV and ?VALUE input forms must be identifiers.
  ;;
  (syntax-rules ()
    ((_ ((?value.bv ?value) ...) . ?body)
     (let ((?value.bv (let ((V ?value))
			(cond ((bytevector? V)
			       V)
			      ((string? V)
			       (string->latin1 V))
			      (else V))))
	   ...)
       . ?body))))

(define-syntax receive
  (syntax-rules ()
    ((_ ?formals ?expression ?form0 ?form ...)
     (call-with-values
	 (lambda () ?expression)
       (lambda ?formals ?form0 ?form ...)))))


(define-syntax case-word-size
  (if (= 4 config.wordsize)
      (syntax-rules ()
	((_ ((32) . ?body-32) ((64) . ?body-64))
	 (begin . ?body-32)))
    (syntax-rules ()
      ((_ ((32) . ?body-32) ((64) . ?body-64))
       (begin . ?body-64)))))

(define-syntax big	(syntax-rules ()))
(define-syntax little	(syntax-rules ()))

(define-syntax case-endianness
  (lambda (stx)
    (syntax-case stx (big little)
      ((case-endianness (?who ?endianness)
	 ((little)	. ?lit-body)
	 ((big)		. ?big-body))
       (and (identifier? #'?who)
	    (identifier? #'?endianness))
       #'(case-endianness (?who ?endianness)
	   ((big)	. ?big-body)
	   ((little)	. ?lit-body)))

      ((case-endianness (?who ?endianness)
	 ((big)		. ?big-body)
	 ((little)	. ?lit-body))
       (and (identifier? #'?who)
	    (identifier? #'?endianness))
       #'(case ?endianness
	   ((big)	. ?big-body)
	   ((little)	. ?lit-body)
	   (else
	    (assertion-violation ?who "expected endianness symbol as argument" ?endianness)))))))

(define-syntax case-fixnums
  (syntax-rules (else)
    ((_ ?expr
	((?fixnum0 ?fixnum ...)
	 ?fx-body0 ?fx-body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((fx ?expr))
       (import (only (ikarus system $fx)
		     $fx=))
       (cond ((or ($fx= ?fixnum0 fx)
		  ($fx= ?fixnum  fx)
		  ...)
	      ?fx-body0 ?fx-body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?fixnum0 ?fixnum ...)
	 ?fx-body0 ?fx-body ...)
	...)
     (let ((fx ?expr))
       (import (only (ikarus system $fx)
		     $fx=))
       (cond ((or ($fx= ?fixnum0 fx)
		  ($fx= ?fixnum  fx)
		  ...)
	      ?fx-body0 ?fx-body ...)
	     ...)))
    ))

(define-syntax case-integers
  (syntax-rules (else)
    ((_ ?expr
	((?integer0 ?integer ...)
	 ?body0 ?body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((int ?expr))
       (cond ((or (= ?integer0 int)
		  (= ?integer  int)
		  ...)
	      ?body0 ?body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?integer0 ?integer ...)
	 ?body0 ?body ...)
	...)
     (let ((int ?expr))
       (cond ((or (= ?integer0 int)
		  (= ?integer  int)
		  ...)
	      ?body0 ?body ...)
	     ...)))
    ))

(define-syntax case-symbols
  (syntax-rules (else)
    ((_ ?expr
	((?symbol0 ?symbol ...)
	 ?sym-body0 ?sym-body ...)
	...
	(else
	 ?else-body0 ?else-body ...))
     (let ((sym ?expr))
       (cond ((or (eq? (quote ?symbol0) sym)
		  (eq? (quote ?symbol)  sym)
		  ...)
	      ?sym-body0 ?sym-body ...)
	     ...
	     (else
	      ?else-body0 ?else-body ...))))
    ((_ ?expr
	((?symbol0 ?symbol ...)
	 ?sym-body0 ?sym-body ...)
	...)
     (let ((sym ?expr))
       (cond ((or (eq? (quote ?symbol0) sym)
		  (eq? (quote ?symbol)  sym)
		  ...)
	      ?sym-body0 ?sym-body ...)
	     ...)))
    ))

(define-argument-validation (exact-integer who obj)
  (and (integer? obj) (exact? obj))
  (assertion-violation who "expected exact integer as argument" obj))

(define-syntax define-exact-integer->symbol-function
  (syntax-rules ()
    ((_ ?who (?code ...))
     (define (?who code)
       (define who '?who)
       (with-arguments-validation (who)
	   ((exact-integer	code))
	 (case-integers code
	   ((?code)	'?code)
	   ...
	   (else #f)))))))


;;;; math functions dispatching

(define-syntax cond-exact-integer-operand
  (syntax-rules (else fixnum? bignum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     (else		?body-el0 ?body-el ...))))))

(define-syntax cond-inexact-integer-operand
  (syntax-rules (else fixnum? bignum? flonum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))))

(define-syntax cond-exact-real-numeric-operand
  (syntax-rules (else fixnum? bignum? ratnum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rn0 ?body-rn ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rn0 ?body-rn ...)
	     (else		?body-el0 ?body-el ...))))))

;;; --------------------------------------------------------------------

(define-syntax cond-real-numeric-operand
  (syntax-rules (else fixnum? bignum? ratnum? flonum?)
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))

    ;;As above but with flonums before ratnums.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     (else		?body-el0 ?body-el ...))))

    ;;As above but with flonums first.
    ;;
    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     (else		?body-el0 ?body-el ...))))
    ))

;;; --------------------------------------------------------------------

(define-syntax cond-numeric-operand
  (syntax-rules (else
		 zero? exact? inexact?
		 fixnum? bignum? ratnum? flonum? compnum? cflonum? real? complex?)

    ;;Dispatch for all the numeric types.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;;Dispatch for  all the  numeric types,  but flonums  before ratnums
    ;;because they are more likely.
    ;;
    ((_ ?num
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ;;Dispatch for all the numeric types, but flonums first because they
    ;;are most likely.
    ;;
    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum?  num)	?body-fl0 ?body-fl ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((real?)	?body-re0 ?body-re ...)
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((or (fixnum?  num)
		  (bignum?  num)
		  (ratnum?  num)
		  (flonum?  num))
	      ?body-re0 ?body-re ...)
	     ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     (else		?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((zero?)	?body-zr0 ?body-zr ...)
	((real? exact?)	?body-re0 ?body-re ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (import (only (ikarus system $fx)
		     $fxzero?))
       (cond ((flonum? num)
	      ?body-fl0 ?body-fl ...)
	     ((and (fixnum? num) ($fxzero? num))
	      ?body-zr0 ?body-zr ...)
	     ((or (fixnum? num) (bignum? num) (ratnum? num))
	      ?body-re0 ?body-re ...)
	     (else
	      ?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ((_ ?num
	((flonum?)	?body-fl0 ?body-fl ...)
	((real? exact?)	?body-re0 ?body-re ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((flonum? num)
	      ?body-fl0 ?body-fl ...)
	     ((or (fixnum? num) (bignum? num) (ratnum? num))
	      ?body-re0 ?body-re ...)
	     (else
	      ?body-el0 ?body-el ...))))

    ;; --------------------------------------------------

    ;;Dispatch for all the numeric types, complex first.
    ;;
    ((_ ?num
	((compnum?)	?body-cn0 ?body-cn ...)
	((cflonum?)	?body-cf0 ?body-cf ...)
	((fixnum?)	?body-fx0 ?body-fx ...)
	((bignum?)	?body-bg0 ?body-bg ...)
	((ratnum?)	?body-rt0 ?body-rt ...)
	((flonum?)	?body-fl0 ?body-fl ...)
	(else		?body-el0 ?body-el ...))
     (let ((num ?num))
       (cond ((compnum? num)	?body-cn0 ?body-cn ...)
	     ((cflonum? num)	?body-cf0 ?body-cf ...)
	     ((fixnum?  num)	?body-fx0 ?body-fx ...)
	     ((bignum?  num)	?body-bg0 ?body-bg ...)
	     ((ratnum?  num)	?body-rt0 ?body-rt ...)
	     ((flonum?  num)	?body-fl0 ?body-fl ...)
	     (else		?body-el0 ?body-el ...))))
    ))

;;; --------------------------------------------------------------------

;; (define-syntax fixnum	(syntax-rules ()))
;; (define-syntax bignum	(syntax-rules ()))
;; (define-syntax flonum	(syntax-rules ()))
;; (define-syntax cflonum	(syntax-rules ()))
;; (define-syntax compnum	(syntax-rules ()))

;; (define-syntax case-one-operand
;;   (syntax-rules (fixnum bignum flonum cflonum compnum)
;;     ((case-one-operand (?who ?op)
;;        ((fixnum)	. ?fixnum-body)
;;        ((bignum)	. ?bignum-body)
;;        ((flonum)	. ?flonum-body)
;;        ((cflonum)	. ?cflonum-body)
;;        ((compnum)	. ?compnum-body))
;;      (let ((op ?op))
;;        (cond ((fixnum?  op)	. ?fixnum-body)
;; 	     ((bignum?  op)	. ?bignum-body)
;; 	     ((flonum?  op)	. ?flonum-body)
;; 	     ((cflonum? op)	. ?cflonum-body)
;; 	     ((compnum? op)	. ?compnum-body)
;; 	     (else
;; 	      (assertion-violation ?who "invalid numeric operand" op)))))))

;; (define-syntax case-two-operands
;;   (syntax-rules (fixnum bignum flonum cflonum compnum)
;;     ((case-two-operands (?who ?op1 ?op2)
;;        ((fixnum)
;; 	((fixnum)	. ?fixnum/fixnum-body)
;; 	((bignum)	. ?fixnum/bignum-body)
;; 	((flonum)	. ?fixnum/flonum-body)
;; 	((cflonum)	. ?fixnum/cflonum-body)
;; 	((compnum)	. ?fixnum/compnum-body))
;;        ((bignum)
;; 	((fixnum)	. ?bignum/fixnum-body)
;; 	((bignum)	. ?bignum/bignum-body)
;; 	((flonum)	. ?bignum/flonum-body)
;; 	((cflonum)	. ?bignum/cflonum-body)
;; 	((compnum)	. ?bignum/compnum-body))
;;        ((flonum)
;; 	((fixnum)	. ?flonum/fixnum-body)
;; 	((bignum)	. ?flonum/bignum-body)
;; 	((flonum)	. ?flonum/flonum-body)
;; 	((cflonum)	. ?flonum/cflonum-body)
;; 	((compnum)	. ?flonum/compnum-body))
;;        ((cflonum)
;; 	((fixnum)	. ?cflonum/fixnum-body)
;; 	((bignum)	. ?cflonum/bignum-body)
;; 	((flonum)	. ?cflonum/flonum-body)
;; 	((cflonum)	. ?cflonum/cflonum-body)
;; 	((compnum)	. ?cflonum/compnum-body))
;;        ((compnum)
;; 	((fixnum)	. ?compnum/fixnum-body)
;; 	((bignum)	. ?compnum/bignum-body)
;; 	((flonum)	. ?compnum/flonum-body)
;; 	((cflonum)	. ?compnum/cflonum-body)
;; 	((compnum)	. ?compnum/compnum-body)))
;;      (case-one-operand (?who ?op1)
;;        ((fixnum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?fixnum/fixnum-body)
;; 	  ((bignum)	. ?fixnum/bignum-body)
;; 	  ((flonum)	. ?fixnum/flonum-body)
;; 	  ((cflonum)	. ?fixnum/cflonum-body)
;; 	  ((compnum)	. ?fixnum/compnum-body)))
;;        ((bignum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?bignum/fixnum-body)
;; 	  ((bignum)	. ?bignum/bignum-body)
;; 	  ((flonum)	. ?bignum/flonum-body)
;; 	  ((cflonum)	. ?bignum/cflonum-body)
;; 	  ((compnum)	. ?bignum/compnum-body)))
;;        ((flonum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?flonum/fixnum-body)
;; 	  ((bignum)	. ?flonum/bignum-body)
;; 	  ((flonum)	. ?flonum/flonum-body)
;; 	  ((cflonum)	. ?flonum/cflonum-body)
;; 	  ((compnum)	. ?flonum/compnum-body)))
;;        ((cflonum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?cflonum/fixnum-body)
;; 	  ((bignum)	. ?cflonum/bignum-body)
;; 	  ((flonum)	. ?cflonum/flonum-body)
;; 	  ((cflonum)	. ?cflonum/cflonum-body)
;; 	  ((compnum)	. ?cflonum/compnum-body)))
;;        ((compnum)
;; 	(case-one-operand (?who ?op2)
;; 	  ((fixnum)	. ?compnum/fixnum-body)
;; 	  ((bignum)	. ?compnum/bignum-body)
;; 	  ((flonum)	. ?compnum/flonum-body)
;; 	  ((cflonum)	. ?compnum/cflonum-body)
;; 	  ((compnum)	. ?compnum/compnum-body)))))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'case-one-operand 'scheme-indent-function 1)
;;eval: (put 'case-two-operands 'scheme-indent-function 1)
;;eval: (put 'case-integers 'scheme-indent-function 1)
;;End:
