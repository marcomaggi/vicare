;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: arguments validation syntaxes
;;;Date: Mon Oct  1, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare arguments validation)
  (export
    define-argument-validation
    with-arguments-validation
    with-dangerous-arguments-validation
    arguments-validation-forms

    ;; fixnums
    vicare.argument-validation-for-fixnum
    vicare.argument-validation-for-fixnum/false
    vicare.argument-validation-for-positive-fixnum
    vicare.argument-validation-for-negative-fixnum
    vicare.argument-validation-for-non-positive-fixnum
    vicare.argument-validation-for-non-negative-fixnum
    vicare.argument-validation-for-fixnum-in-inclusive-range
    vicare.argument-validation-for-fixnum-in-exclusive-range
    vicare.argument-validation-for-even-fixnum
    vicare.argument-validation-for-odd-fixnum

    ;; exact integers
    vicare.argument-validation-for-exact-integer
    vicare.argument-validation-for-exact-integer/false
    vicare.argument-validation-for-positive-exact-integer
    vicare.argument-validation-for-negative-exact-integer
    vicare.argument-validation-for-non-positive-exact-integer
    vicare.argument-validation-for-non-negative-exact-integer
    vicare.argument-validation-for-exact-integer-in-inclusive-range
    vicare.argument-validation-for-exact-integer-in-exclusive-range
    vicare.argument-validation-for-even-exact-integer
    vicare.argument-validation-for-odd-exact-integer

    ;; bit sized integers
    vicare.argument-validation-for-word-u8
    vicare.argument-validation-for-word-u8/false
    vicare.argument-validation-for-word-s8
    vicare.argument-validation-for-word-s8/false
    vicare.argument-validation-for-word-u16
    vicare.argument-validation-for-word-u16/false
    vicare.argument-validation-for-word-s16
    vicare.argument-validation-for-word-s16/false
    vicare.argument-validation-for-word-u32
    vicare.argument-validation-for-word-u32/false
    vicare.argument-validation-for-word-s32
    vicare.argument-validation-for-word-s32/false
    vicare.argument-validation-for-word-u64
    vicare.argument-validation-for-word-u64/false
    vicare.argument-validation-for-word-s64
    vicare.argument-validation-for-word-s64/false
    vicare.argument-validation-for-word-u128
    vicare.argument-validation-for-word-u128/false
    vicare.argument-validation-for-word-s128
    vicare.argument-validation-for-word-s128/false
    vicare.argument-validation-for-word-u256
    vicare.argument-validation-for-word-u256/false
    vicare.argument-validation-for-word-s256
    vicare.argument-validation-for-word-s256/false
    vicare.argument-validation-for-machine-word
    vicare.argument-validation-for-machine-word/false

    ;; C language "int" type
    vicare.argument-validation-for-signed-int
    vicare.argument-validation-for-signed-int/false
    vicare.argument-validation-for-positive-signed-int
    vicare.argument-validation-for-negative-signed-int
    vicare.argument-validation-for-non-positive-signed-int
    vicare.argument-validation-for-non-negative-signed-int
    vicare.argument-validation-for-signed-int-in-inclusive-range
    vicare.argument-validation-for-signed-int-in-exclusive-range
    vicare.argument-validation-for-even-signed-int
    vicare.argument-validation-for-odd-signed-int

    ;; other C language types
    vicare.argument-validation-for-unsigned-char
    vicare.argument-validation-for-signed-char
    vicare.argument-validation-for-unsigned-short
    vicare.argument-validation-for-signed-short
    vicare.argument-validation-for-unsigned-int
    vicare.argument-validation-for-unsigned-long
    vicare.argument-validation-for-signed-long
    vicare.argument-validation-for-unsigned-long-long
    vicare.argument-validation-for-signed-long-long
    vicare.argument-validation-for-pointer-integer
    vicare.argument-validation-for-size_t
    vicare.argument-validation-for-ssize_t
    vicare.argument-validation-for-off_t
    vicare.argument-validation-for-ptrdiff_t

    vicare.argument-validation-for-unsigned-char/false
    vicare.argument-validation-for-signed-char/false
    vicare.argument-validation-for-unsigned-short/false
    vicare.argument-validation-for-signed-short/false
    vicare.argument-validation-for-unsigned-int/false
    vicare.argument-validation-for-unsigned-long/false
    vicare.argument-validation-for-signed-long/false
    vicare.argument-validation-for-unsigned-long-long/false
    vicare.argument-validation-for-signed-long-long/false
    vicare.argument-validation-for-pointer-integer/false
    vicare.argument-validation-for-size_t/false
    vicare.argument-validation-for-ssize_t/false
    vicare.argument-validation-for-off_t/false
    vicare.argument-validation-for-ptrdiff_t/false
    )
  (import (ikarus)
    (for (prefix (vicare installation-configuration)
		 config.)
	 expand)
    (prefix (vicare words)
	    words.)
    (prefix (vicare unsafe-operations)
	    $))


;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


(define-syntax define-argument-validation
  ;;Define a set of macros to  validate arguments, to be used along with
  ;;WITH-ARGUMENTS-VALIDATION.  Transform:
  ;;
  ;;  (define-argument-validation (bytevector who bv)
  ;;    (bytevector? bv)
  ;;    (assertion-violation who "expected a bytevector as argument" bv))
  ;;
  ;;into:
  ;;
  ;;  (define-inline (vicare.argument-validation-for-bytevector who bv . body)
  ;;    (if (vicare.argument-validation-predicate-for-bytevector bv)
  ;;        (begin . body)
  ;;      (vicare.argument-validation-error-for-bytevector who bv)))
  ;;
  ;;  (define-inline (vicare.argument-validation-predicate-for-bytevector bv)
  ;;    (bytevector? bv))
  ;;
  ;;  (define-inline (vicare.argument-validation-error-for-bytevector who bv))
  ;;    (assertion-violation who "expected a bytevector as argument" bv))
  ;;
  ;;If we need to export a validator  from a library: we can export just
  ;;the    identifier   VICARE.ARGUMENT-VALIDATION-FOR-?NAME,    without
  ;;prefixing it.
  ;;
  (lambda (stx)
    (define who 'define-argument-validation)
    (define (main stx)
      (syntax-case stx ()
	((_ (?name ?who ?arg ...) ?predicate ?error-handler)
	 (and (identifier? #'?name)
	      (identifier? #'?who))
	 (let ((ctx  #'?name)
	       (name (symbol->string (syntax->datum #'?name))))
	   (with-syntax
	       ((VALIDATE	(%name ctx name "argument-validation-for-"))
		(PREDICATE	(%name ctx name "argument-validation-predicate-for-"))
		(ERROR	(%name ctx name "argument-validation-error-for-")))
	     #'(begin
		 (define-inline (PREDICATE ?arg ...) ?predicate)
		 (define-inline (ERROR ?who ?arg ...) ?error-handler)
		 (define-inline (VALIDATE ?who ?arg ... . body)
		   (if (PREDICATE ?arg ...)
		       (begin . body)
		     (ERROR ?who ?arg ...)))))))
	(_
	 (%synner "invalid input form" #f))))

    (define (%name ctx name prefix-string)
      (let ((str (string-append "vicare." prefix-string name)))
	(datum->syntax ctx (string->symbol str))))

    (define (%synner msg subform)
      (syntax-violation who msg (syntax->datum stx) (syntax->datum subform)))

    (main stx)))


(define-syntax arguments-validation-forms
  (if config.arguments-validation
      (syntax-rules ()
	((_)
	 (values))
	((_ ?body0 . ?body)
	 (begin ?body0 . ?body)))
    (syntax-rules ()
      ((_)
       (values))
      ((_ ?body0 . ?body)
       (values)))))


(define-syntax with-arguments-validation
  ;;Perform the validation only if enabled at configure time.
  ;;
  (syntax-rules ()
    ((_ . ?args)
     (%with-arguments-validation #f . ?args))))

(define-syntax with-dangerous-arguments-validation
  ;;Dangerous validations are always performed.
  ;;
  (syntax-rules ()
    ((_ . ?args)
     (%with-arguments-validation #t . ?args))))

(define-syntax %with-arguments-validation
  ;;Transform:
  ;;
  ;;  (with-arguments-validation (who)
  ;;       ((fixnum  X)
  ;;        (integer Y))
  ;;    (do-this)
  ;;    (do-that))
  ;;
  ;;into:
  ;;
  ;;  (vicare.argument-validation-for-fixnum who X
  ;;   (vicare.argument-validation-for-integer who Y
  ;;    (do-this)
  ;;    (do-that)))
  ;;
  ;;As a special case:
  ;;
  ;;  (with-arguments-validation (who)
  ;;       ((#t  X))
  ;;    (do-this)
  ;;    (do-that))
  ;;
  ;;expands to:
  ;;
  ;;  (begin
  ;;    (do-this)
  ;;    (do-that))
  ;;
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?always-include (?who) ((?validator ?arg ...) ...) . ?body)
	 (and (identifier? #'?who)
	      (for-all identifier? (syntax->list #'(?validator ...))))
	 ;;Whether we  include the arguments  checking or not,  we build
	 ;;the output form validating the input form.
	 (let* ((include?	(syntax->datum #'?always-include))
		(body		#'(begin . ?body))
		(output-form	(%build-output-form #'?who
						    #'(?validator ...)
						    #'((?arg ...) ...)
						    body)))
	   (if (or include? config.arguments-validation)
	       output-form body)))
	(_
	 (%synner "invalid input form" #f))))

    (define (%build-output-form who validators list-of-args body)
      (syntax-case validators ()
	(()
	 #`(let () #,body))
	;;Accept #t as special validator meaning "always valid"; this is
	;;sometimes useful when composing syntax output forms.
	((#t . ?other-validators)
	 (%build-output-form who #'?other-validators list-of-args body))
	((?validator . ?other-validators)
	 (identifier? #'?validator)
	 (let ((str (symbol->string (syntax->datum #'?validator))))
	   (with-syntax
	       ((VALIDATE (%name #'?validator str "argument-validation-for-"))
		(((ARG ...) . OTHER-ARGS) list-of-args))
	     #`(VALIDATE #,who ARG ...
			 #,(%build-output-form who #'?other-validators #'OTHER-ARGS body)))))
	((?validator . ?others)
	 (%synner "invalid argument-validator selector" #'?validator))))

    (define (%name ctx name prefix-string)
      (let ((str (string-append "vicare." prefix-string name)))
	(datum->syntax ctx (string->symbol str))))

    (define syntax->list
      (case-lambda
       ((stx)
	(syntax->list stx '()))
       ((stx tail)
	(syntax-case stx ()
	  ((?car . ?cdr)
	   (cons #'?car (syntax->list #'?cdr tail)))
	  (()
	   tail)))))

    (define (%synner msg subform)
      (syntax-violation 'with-arguments-validation
	msg (syntax->datum stx) (syntax->datum subform)))

    (main stx)))


;;;; fixnums validation

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (%invalid-fixnum who obj))

(define (%invalid-fixnum who obj)
  (assertion-violation who "expected fixnum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (fixnum/false who obj)
  (or (not obj) (fixnum? obj))
  (%invalid-fixnum/false who obj))

(define (%invalid-fixnum/false who obj)
  (assertion-violation who "expected false or fixnum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (positive-fixnum who obj)
  (and (fixnum? obj)
       ($fx< 0 obj))
  (%invalid-positive-fixnum who obj))

(define (%invalid-positive-fixnum who obj)
  (assertion-violation who "expected positive fixnum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (negative-fixnum who obj)
  (and (fixnum? obj)
       ($fx> 0 obj))
  (%invalid-negative-fixnum who obj))

(define (%invalid-negative-fixnum who obj)
  (assertion-violation who "expected negative fixnum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (non-positive-fixnum who obj)
  (and (fixnum? obj)
       ($fx>= 0 obj))
  (%invalid-non-positive-fixnum who obj))

(define (%invalid-non-positive-fixnum who obj)
  (assertion-violation who "expected non-positive fixnum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (non-negative-fixnum who obj)
  (and (fixnum? obj)
       ($fx<= 0 obj))
  (%invalid-non-negative-fixnum who obj))

(define (%invalid-non-negative-fixnum who obj)
  (assertion-violation who "expected non-negative fixnum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (fixnum-in-inclusive-range ?who ?obj ?min ?max)
  (and (fixnum? ?obj)
       ($fx>= ?obj ?min)
       ($fx<= ?obj ?max))
  (%invalid-fixnum-in-inclusive-range ?who ?obj ?min ?max))

(define (%invalid-fixnum-in-inclusive-range who obj min max)
  (assertion-violation who
    (string-append "expected fixnum in inclusive range ["
		   (number->string min) ", " (number->string max)
		   "] as argument")
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (fixnum-in-exclusive-range ?who ?obj ?min ?max)
  (and (fixnum? ?obj)
       ($fx> ?obj ?min)
       ($fx< ?obj ?max))
  (%invalid-fixnum-in-exclusive-range ?who ?obj ?min ?max))

(define (%invalid-fixnum-in-exclusive-range who obj min max)
  (assertion-violation who
    (string-append "expected fixnum in exclusive range ("
		   (number->string min) ", " (number->string max)
		   ") as argument")
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (even-fixnum who obj)
  (and (fixnum? obj)
       (fxeven? obj))
  (%invalid-even-fixnum who obj))

(define (%invalid-even-fixnum who obj)
  (assertion-violation who "expected even fixnum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (odd-fixnum who obj)
  (and (fixnum? obj)
       (fxodd? obj))
  (%invalid-odd-fixnum who obj))

(define (%invalid-odd-fixnum who obj)
  (assertion-violation who "expected odd fixnum as argument" obj))


;;;; exact integers validation

(define-inline (exact-integer? obj)
  (and (integer? obj)
       (exact?   obj)))

(define-argument-validation (exact-integer who obj)
  (exact-integer? obj)
  (%invalid-exact-integer who obj))

(define (%invalid-exact-integer who obj)
  (assertion-violation who "expected exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (exact-integer/false who obj)
  (or (not obj) (exact-integer? obj))
  (%invalid-exact-integer/false who obj))

(define (%invalid-exact-integer/false who obj)
  (assertion-violation who "expected false or exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (positive-exact-integer who obj)
  (and (exact-integer? obj)
       (< 0 obj))
  (%invalid-positive-exact-integer who obj))

(define (%invalid-positive-exact-integer who obj)
  (assertion-violation who "expected positive exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (negative-exact-integer who obj)
  (and (exact-integer? obj)
       (> 0 obj))
  (%invalid-negative-exact-integer who obj))

(define (%invalid-negative-exact-integer who obj)
  (assertion-violation who "expected negative exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (non-positive-exact-integer who obj)
  (and (exact-integer? obj)
       (>= 0 obj))
  (%invalid-non-positive-exact-integer who obj))

(define (%invalid-non-positive-exact-integer who obj)
  (assertion-violation who "expected non-positive exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (non-negative-exact-integer who obj)
  (and (exact-integer? obj)
       (<= 0 obj))
  (%invalid-non-negative-exact-integer who obj))

(define (%invalid-non-negative-exact-integer who obj)
  (assertion-violation who "expected non-negative exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (exact-integer-in-inclusive-range ?who ?obj ?min ?max)
  (and (exact-integer? ?obj)
       (>= ?obj ?min)
       (<= ?obj ?max))
  (%invalid-exact-integer-in-inclusive-range ?who ?obj ?min ?max))

(define (%invalid-exact-integer-in-inclusive-range who obj min max)
  (assertion-violation who
    (string-append "expected exact integer in inclusive range ["
		   (number->string min) ", " (number->string max)
		   "] as argument")
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (exact-integer-in-exclusive-range ?who ?obj ?min ?max)
  (and (exact-integer? ?obj)
       (> ?obj ?min)
       (< ?obj ?max))
  (%invalid-exact-integer-in-exclusive-range ?who ?obj ?min ?max))

(define (%invalid-exact-integer-in-exclusive-range who obj min max)
  (assertion-violation who
    (string-append "expected exact integer in exclusive range ("
		   (number->string min) ", " (number->string max)
		   ") as argument")
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (even-exact-integer who obj)
  (and (exact-integer? obj)
       (even? obj))
  (%invalid-even-exact-integer who obj))

(define (%invalid-even-exact-integer who obj)
  (assertion-violation who "expected even exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (odd-exact-integer who obj)
  (and (exact-integer? obj)
       (odd? obj))
  (%invalid-odd-exact-integer who obj))

(define (%invalid-odd-exact-integer who obj)
  (assertion-violation who "expected odd exact integer as argument" obj))


;;;; C language "int" type

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (%invalid-signed-int who obj))

(define (%invalid-signed-int who obj)
  (assertion-violation who "expected exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-int/false who obj)
  (or (not obj) (words.signed-int? obj))
  (%invalid-signed-int/false who obj))

(define (%invalid-signed-int/false who obj)
  (assertion-violation who "expected false or exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (positive-signed-int who obj)
  (and (words.signed-int? obj)
       (< 0 obj))
  (%invalid-positive-signed-int who obj))

(define (%invalid-positive-signed-int who obj)
  (assertion-violation who "expected positive exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (negative-signed-int who obj)
  (and (words.signed-int? obj)
       (> 0 obj))
  (%invalid-negative-signed-int who obj))

(define (%invalid-negative-signed-int who obj)
  (assertion-violation who "expected negative exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (non-positive-signed-int who obj)
  (and (words.signed-int? obj)
       (>= 0 obj))
  (%invalid-non-positive-signed-int who obj))

(define (%invalid-non-positive-signed-int who obj)
  (assertion-violation who "expected non-positive exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (non-negative-signed-int who obj)
  (and (words.signed-int? obj)
       (<= 0 obj))
  (%invalid-non-negative-signed-int who obj))

(define (%invalid-non-negative-signed-int who obj)
  (assertion-violation who "expected non-negative exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-int-in-inclusive-range ?who ?obj ?min ?max)
  (and (words.signed-int? ?obj)
       (>= ?obj ?min)
       (<= ?obj ?max))
  (%invalid-signed-int-in-inclusive-range ?who ?obj ?min ?max))

(define (%invalid-signed-int-in-inclusive-range who obj min max)
  (assertion-violation who
    (string-append "expected exact integer in inclusive range ["
		   (number->string min) ", " (number->string max)
		   "] as argument")
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-int-in-exclusive-range ?who ?obj ?min ?max)
  (and (words.signed-int? ?obj)
       (> ?obj ?min)
       (< ?obj ?max))
  (%invalid-signed-int-in-exclusive-range ?who ?obj ?min ?max))

(define (%invalid-signed-int-in-exclusive-range who obj min max)
  (assertion-violation who
    (string-append "expected exact integer in exclusive range ("
		   (number->string min) ", " (number->string max)
		   ") as argument")
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (even-signed-int who obj)
  (and (words.signed-int? obj)
       (even? obj))
  (%invalid-even-signed-int who obj))

(define (%invalid-even-signed-int who obj)
  (assertion-violation who "expected even exact integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (odd-signed-int who obj)
  (and (words.signed-int? obj)
       (odd? obj))
  (%invalid-odd-signed-int who obj))

(define (%invalid-odd-signed-int who obj)
  (assertion-violation who "expected odd exact integer as argument" obj))


;;;; bit sized integers

(define-argument-validation (word-u8 who obj)
  (words.word-u8? obj)
  (%invalid-word-u8 who obj))

(define (%invalid-word-u8 who obj)
  (assertion-violation who
    "expected exact integer representing an octet as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s8 who obj)
  (words.word-s8? obj)
  (%invalid-word-s8 who obj))

(define (%invalid-word-s8 who obj)
  (assertion-violation who
    "expected exact integer representing a byte as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u16 who obj)
  (words.word-u16? obj)
  (%invalid-word-u16 who obj))

(define (%invalid-word-u16 who obj)
  (assertion-violation who
    "expected exact integer representing a 16-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s16 who obj)
  (words.word-s16? obj)
  (%invalid-word-s16 who obj))

(define (%invalid-word-s16 who obj)
  (assertion-violation who
    "expected exact integer representing a 16-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u32 who obj)
  (words.word-u32? obj)
  (%invalid-word-u32 who obj))

(define (%invalid-word-u32 who obj)
  (assertion-violation who
    "expected exact integer representing a 32-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s32 who obj)
  (words.word-s32? obj)
  (%invalid-word-s32 who obj))

(define (%invalid-word-s32 who obj)
  (assertion-violation who
    "expected exact integer representing a 32-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u64 who obj)
  (words.word-u64? obj)
  (%invalid-word-u64 who obj))

(define (%invalid-word-u64 who obj)
  (assertion-violation who
    "expected exact integer representing a 64-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s64 who obj)
  (words.word-s64? obj)
  (%invalid-word-s64 who obj))

(define (%invalid-word-s64 who obj)
  (assertion-violation who
    "expected exact integer representing a 64-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u128 who obj)
  (words.word-u128? obj)
  (%invalid-word-u128 who obj))

(define (%invalid-word-u128 who obj)
  (assertion-violation who
    "expected exact integer representing a 128-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s128 who obj)
  (words.word-s128? obj)
  (%invalid-word-s128 who obj))

(define (%invalid-word-s128 who obj)
  (assertion-violation who
    "expected exact integer representing a 128-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u256 who obj)
  (words.word-u256? obj)
  (%invalid-word-u256 who obj))

(define (%invalid-word-u256 who obj)
  (assertion-violation who
    "expected exact integer representing a 256-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s256 who obj)
  (words.word-s256? obj)
  (%invalid-word-s256 who obj))

(define (%invalid-word-s256 who obj)
  (assertion-violation who
    "expected exact integer representing a 256-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (machine-word who obj)
  (words.machine-word? obj)
  (%invalid-machine-word who obj))

(define (%invalid-machine-word who obj)
  (assertion-violation who
    "expected exact integer representing a machine word as argument" obj))


;;;; false or bit sized integers

(define-argument-validation (word-u8/false who obj)
  (or (not obj) (words.word-u8? obj))
  (%invalid-word-u8/false who obj))

(define (%invalid-word-u8/false who obj)
  (assertion-violation who
    "expected false or exact integer representing an octet as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s8/false who obj)
  (or (not obj) (words.word-s8? obj))
  (%invalid-word-s8/false who obj))

(define (%invalid-word-s8/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a byte as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u16/false who obj)
  (or (not obj) (words.word-u16? obj))
  (%invalid-word-u16/false who obj))

(define (%invalid-word-u16/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 16-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s16/false who obj)
  (or (not obj) (words.word-s16? obj))
  (%invalid-word-s16/false who obj))

(define (%invalid-word-s16/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 16-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u32/false who obj)
  (or (not obj) (words.word-u32? obj))
  (%invalid-word-u32/false who obj))

(define (%invalid-word-u32/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 32-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s32/false who obj)
  (or (not obj) (words.word-s32? obj))
  (%invalid-word-s32/false who obj))

(define (%invalid-word-s32/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 32-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u64/false who obj)
  (or (not obj) (words.word-u64? obj))
  (%invalid-word-u64/false who obj))

(define (%invalid-word-u64/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 64-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s64/false who obj)
  (or (not obj) (words.word-s64? obj))
  (%invalid-word-s64/false who obj))

(define (%invalid-word-s64/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 64-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u128/false who obj)
  (or (not obj) (words.word-u128? obj))
  (%invalid-word-u128/false who obj))

(define (%invalid-word-u128/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 128-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s128/false who obj)
  (or (not obj) (words.word-s128? obj))
  (%invalid-word-s128/false who obj))

(define (%invalid-word-s128/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 128-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-u256/false who obj)
  (or (not obj) (words.word-u256? obj))
  (%invalid-word-u256/false who obj))

(define (%invalid-word-u256/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 256-bit unsigned integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (word-s256/false who obj)
  (or (not obj) (words.word-s256? obj))
  (%invalid-word-s256/false who obj))

(define (%invalid-word-s256/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a 256-bit signed integer as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (machine-word/false who obj)
  (or (not obj) (words.machine-word? obj))
  (%invalid-machine-word/false who obj))

(define (%invalid-machine-word/false who obj)
  (assertion-violation who
    "expected false or exact integer representing a machine word as argument" obj))


;;;; C language values

(define-argument-validation (unsigned-char who obj)
  (words.unsigned-char? obj)
  (%invalid-unsigned-char who obj))

(define (%invalid-unsigned-char who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned char\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-char who obj)
  (words.signed-char? obj)
  (%invalid-signed-char who obj))

(define (%invalid-signed-char who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"signed char\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-short who obj)
  (words.unsigned-short? obj)
  (%invalid-unsigned-short who obj))

(define (%invalid-unsigned-short who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned short int\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-short who obj)
  (words.signed-short? obj)
  (%invalid-signed-short who obj))

(define (%invalid-signed-short who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"signed short int\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-int who obj)
  (words.unsigned-int? obj)
  (%invalid-unsigned-int who obj))

(define (%invalid-unsigned-int who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned int\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-long who obj)
  (words.unsigned-long? obj)
  (%invalid-unsigned-long who obj))

(define (%invalid-unsigned-long who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned long\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-long who obj)
  (words.signed-long? obj)
  (%invalid-signed-long who obj))

(define (%invalid-signed-long who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"signed long\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-long-long who obj)
  (words.unsigned-long-long? obj)
  (%invalid-unsigned-long-long who obj))

(define (%invalid-unsigned-long-long who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned long long\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-long-long who obj)
  (words.signed-long-long? obj)
  (%invalid-signed-long-long who obj))

(define (%invalid-signed-long-long who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"signed long long\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pointer-integer who obj)
  (words.pointer-integer? obj)
  (%invalid-pointer-integer who obj))

(define (%invalid-pointer-integer who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"void *\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (size_t who obj)
  (words.size_t? obj)
  (%invalid-size_t who obj))

(define (%invalid-size_t who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"size_t\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (ssize_t who obj)
  (words.ssize_t? obj)
  (%invalid-ssize_t who obj))

(define (%invalid-ssize_t who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"ssize_t\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (off_t who obj)
  (words.off_t? obj)
  (%invalid-off_t who obj))

(define (%invalid-off_t who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"off_t\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (ptrdiff_t who obj)
  (words.ptrdiff_t? obj)
  (%invalid-ptrdiff_t who obj))

(define (%invalid-ptrdiff_t who obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"ptrdiff_t\"" obj))


;;;; false or C language values

(define-argument-validation (unsigned-char/false who obj)
  (or (not obj) (words.unsigned-char? obj))
  (%invalid-unsigned-char/false who obj))

(define (%invalid-unsigned-char/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned char\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-char/false who obj)
  (or (not obj) (words.signed-char? obj))
  (%invalid-signed-char/false who obj))

(define (%invalid-signed-char/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"signed char\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-short/false who obj)
  (or (not obj) (words.unsigned-short? obj))
  (%invalid-unsigned-short/false who obj))

(define (%invalid-unsigned-short/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned short int\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-short/false who obj)
  (or (not obj) (words.signed-short? obj))
  (%invalid-signed-short/false who obj))

(define (%invalid-signed-short/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"signed short int\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-int/false who obj)
  (or (not obj) (words.unsigned-int? obj))
  (%invalid-unsigned-int/false who obj))

(define (%invalid-unsigned-int/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned int\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-long/false who obj)
  (or (not obj) (words.unsigned-long? obj))
  (%invalid-unsigned-long/false who obj))

(define (%invalid-unsigned-long/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned long\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-long/false who obj)
  (or (not obj) (words.signed-long? obj))
  (%invalid-signed-long/false who obj))

(define (%invalid-signed-long/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"signed long\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (unsigned-long-long/false who obj)
  (or (not obj) (words.unsigned-long-long? obj))
  (%invalid-unsigned-long-long/false who obj))

(define (%invalid-unsigned-long-long/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned long long\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (signed-long-long/false who obj)
  (or (not obj) (words.signed-long-long? obj))
  (%invalid-signed-long-long/false who obj))

(define (%invalid-signed-long-long/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"signed long long\""
    obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pointer-integer/false who obj)
  (or (not obj) (words.pointer-integer? obj))
  (%invalid-pointer-integer/false who obj))

(define (%invalid-pointer-integer/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"void *\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (size_t/false who obj)
  (or (not obj) (words.size_t? obj))
  (%invalid-size_t/false who obj))

(define (%invalid-size_t/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"size_t\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (ssize_t/false who obj)
  (or (not obj) (words.ssize_t? obj))
  (%invalid-ssize_t/false who obj))

(define (%invalid-ssize_t/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"ssize_t\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (off_t/false who obj)
  (or (not obj) (words.off_t? obj))
  (%invalid-off_t/false who obj))

(define (%invalid-off_t/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"off_t\"" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (ptrdiff_t/false who obj)
  (or (not obj) (words.ptrdiff_t? obj))
  (%invalid-ptrdiff_t/false who obj))

(define (%invalid-ptrdiff_t/false who obj)
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"ptrdiff_t\"" obj))


;;;; done

)

;;; end of file
