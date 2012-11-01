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

    ;; booleans
    boolean.vicare-arguments-validation

    ;; fixnums
    fixnum.vicare-arguments-validation
    fixnum/false.vicare-arguments-validation
    positive-fixnum.vicare-arguments-validation
    negative-fixnum.vicare-arguments-validation
    non-positive-fixnum.vicare-arguments-validation
    non-negative-fixnum.vicare-arguments-validation
    fixnum-in-inclusive-range.vicare-arguments-validation
    fixnum-in-exclusive-range.vicare-arguments-validation
    even-fixnum.vicare-arguments-validation
    odd-fixnum.vicare-arguments-validation

    (rename (non-negative-fixnum.vicare-arguments-validation
	     fixnum-index.vicare-arguments-validation))

    ;; exact integers
    exact-integer.vicare-arguments-validation
    exact-integer/false.vicare-arguments-validation
    positive-exact-integer.vicare-arguments-validation
    negative-exact-integer.vicare-arguments-validation
    non-positive-exact-integer.vicare-arguments-validation
    non-negative-exact-integer.vicare-arguments-validation
    exact-integer-in-inclusive-range.vicare-arguments-validation
    exact-integer-in-exclusive-range.vicare-arguments-validation
    even-exact-integer.vicare-arguments-validation
    odd-exact-integer.vicare-arguments-validation

    ;; bit sized integers
    word-u8.vicare-arguments-validation
    word-u8/false.vicare-arguments-validation
    word-s8.vicare-arguments-validation
    word-s8/false.vicare-arguments-validation
    word-u16.vicare-arguments-validation
    word-u16/false.vicare-arguments-validation
    word-s16.vicare-arguments-validation
    word-s16/false.vicare-arguments-validation
    word-u32.vicare-arguments-validation
    word-u32/false.vicare-arguments-validation
    word-s32.vicare-arguments-validation
    word-s32/false.vicare-arguments-validation
    word-u64.vicare-arguments-validation
    word-u64/false.vicare-arguments-validation
    word-s64.vicare-arguments-validation
    word-s64/false.vicare-arguments-validation
    word-u128.vicare-arguments-validation
    word-u128/false.vicare-arguments-validation
    word-s128.vicare-arguments-validation
    word-s128/false.vicare-arguments-validation
    word-u256.vicare-arguments-validation
    word-u256/false.vicare-arguments-validation
    word-s256.vicare-arguments-validation
    word-s256/false.vicare-arguments-validation
    machine-word.vicare-arguments-validation
    machine-word/false.vicare-arguments-validation

    ;; C language "int" type
    signed-int.vicare-arguments-validation
    signed-int/false.vicare-arguments-validation
    positive-signed-int.vicare-arguments-validation
    negative-signed-int.vicare-arguments-validation
    non-positive-signed-int.vicare-arguments-validation
    non-negative-signed-int.vicare-arguments-validation
    signed-int-in-inclusive-range.vicare-arguments-validation
    signed-int-in-exclusive-range.vicare-arguments-validation
    even-signed-int.vicare-arguments-validation
    odd-signed-int.vicare-arguments-validation

    ;; other C language types
    unsigned-char.vicare-arguments-validation
    signed-char.vicare-arguments-validation
    unsigned-short.vicare-arguments-validation
    signed-short.vicare-arguments-validation
    unsigned-int.vicare-arguments-validation
    unsigned-long.vicare-arguments-validation
    signed-long.vicare-arguments-validation
    unsigned-long-long.vicare-arguments-validation
    signed-long-long.vicare-arguments-validation
    pointer-integer.vicare-arguments-validation
    size_t.vicare-arguments-validation
    ssize_t.vicare-arguments-validation
    off_t.vicare-arguments-validation
    ptrdiff_t.vicare-arguments-validation

    unsigned-char/false.vicare-arguments-validation
    signed-char/false.vicare-arguments-validation
    unsigned-short/false.vicare-arguments-validation
    signed-short/false.vicare-arguments-validation
    unsigned-int/false.vicare-arguments-validation
    unsigned-long/false.vicare-arguments-validation
    signed-long/false.vicare-arguments-validation
    unsigned-long-long/false.vicare-arguments-validation
    signed-long-long/false.vicare-arguments-validation
    pointer-integer/false.vicare-arguments-validation
    size_t/false.vicare-arguments-validation
    ssize_t/false.vicare-arguments-validation
    off_t/false.vicare-arguments-validation
    ptrdiff_t/false.vicare-arguments-validation

    ;; strings
    string.vicare-arguments-validation
    string/false.vicare-arguments-validation
    non-empty-string.vicare-arguments-validation
    non-empty-string/false.vicare-arguments-validation
    index-for-string.vicare-arguments-validation
    index-and-count-for-string.vicare-arguments-validation
    start-and-end-for-string.vicare-arguments-validation
    start-and-past-for-string.vicare-arguments-validation

    ;; vectors
    vector.vicare-arguments-validation
    vector/false.vicare-arguments-validation
    non-empty-vector.vicare-arguments-validation
    non-empty-vector/false.vicare-arguments-validation
    index-for-vector.vicare-arguments-validation
    index-and-count-for-vector.vicare-arguments-validation
    start-and-end-for-vector.vicare-arguments-validation
    start-and-past-for-vector.vicare-arguments-validation

    ;; bytevectors
    bytevector.vicare-arguments-validation
    bytevector/false.vicare-arguments-validation
    non-empty-bytevector.vicare-arguments-validation
    non-empty-bytevector/false.vicare-arguments-validation
    index-for-bytevector.vicare-arguments-validation
    index-and-count-for-bytevector.vicare-arguments-validation
    start-and-end-for-bytevector.vicare-arguments-validation
    start-and-past-for-bytevector.vicare-arguments-validation

    ;; symbols
    symbol.vicare-arguments-validation
    symbol/false.vicare-arguments-validation

    ;; enum-sets
    enum-set.vicare-arguments-validation
    enum-set/false.vicare-arguments-validation

    ;; pointers
    pointer.vicare-arguments-validation
    non-null-pointer.vicare-arguments-validation
    pointer/false.vicare-arguments-validation

    ;; memory-blocks
    memory-block.vicare-arguments-validation
    memory-block/false.vicare-arguments-validation

    ;; flonums
    flonum.vicare-arguments-validation

    ;; bignums
    bignum.vicare-arguments-validation

    ;; input/output ports
    port.vicare-arguments-validation
    port/false.vicare-arguments-validation
    input-port.vicare-arguments-validation
    input-port/false.vicare-arguments-validation
    output-port.vicare-arguments-validation
    output-port/false.vicare-arguments-validation
    input/output-port.vicare-arguments-validation
    input/output-port/false.vicare-arguments-validation
    textual-port.vicare-arguments-validation
    textual-port/false.vicare-arguments-validation
    binary-port.vicare-arguments-validation
    binary-port/false.vicare-arguments-validation

    ;; procedures
    procedure.vicare-arguments-validation
    procedure/false.vicare-arguments-validation

    ;; generalised C strings and buffers
    general-c-string?
    general-c-string.vicare-arguments-validation
    general-c-string/false.vicare-arguments-validation

    general-c-buffer?
    general-c-buffer.vicare-arguments-validation
    general-c-buffer/false.vicare-arguments-validation

    general-c-sticky-buffer?
    general-c-sticky-buffer.vicare-arguments-validation
    general-c-sticky-buffer/false.vicare-arguments-validation
    )
  (import (ikarus)
    (for (prefix (vicare installation-configuration)
		 config.)
	 expand)
    (only (vicare platform constants)
	  FD_SETSIZE)
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
  ;;  (define-inline (bytevector.vicare-arguments-validation who bv . body)
  ;;    (if (the-predicate bv)
  ;;        (begin . body)
  ;;      (the-error who bv)))
  ;;
  ;;  (define-inline (the-predicate bv)
  ;;    (bytevector? bv))
  ;;
  ;;  (define-inline (the-error who bv))
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
	       ((VALIDATE (%name ctx name ".vicare-arguments-validation")))
	     #'(begin
		 (define-inline (the-predicate ?arg ...) ?predicate)
		 (define-inline (the-error ?who ?arg ...) ?error-handler)
		 (define-inline (VALIDATE ?who ?arg ... . body)
		   (if (the-predicate ?arg ...)
		       (begin . body)
		     (the-error ?who ?arg ...)))))))
	(_
	 (%synner "invalid input form" #f))))

    (define (%name ctx name suffix-string)
      (let ((str (string-append name suffix-string)))
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
      (syntax-case validators (void)
	(()
	 #`(let () #,body))
	;;Accept VOID as special  validator meaning "always valid"; this
	;;is sometimes useful when composing syntax output forms.
	((void . ?other-validators)
	 (%build-output-form who #'?other-validators list-of-args body))
	((?validator . ?other-validators)
	 (identifier? #'?validator)
	 (let ((str (symbol->string (syntax->datum #'?validator))))
	   (with-syntax
	       ((VALIDATE (%name #'?validator str ".vicare-arguments-validation"))
		(((ARG ...) . OTHER-ARGS) list-of-args))
	     #`(VALIDATE #,who ARG ...
			 #,(%build-output-form who #'?other-validators #'OTHER-ARGS body)))))
	((?validator . ?others)
	 (%synner "invalid argument-validator selector" #'?validator))))

    (define (%name ctx name suffix-string)
      (let ((str (string-append name suffix-string)))
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


;;;; booleans

(define-argument-validation (boolean who obj)
  (boolean? obj)
  (assertion-violation who "expected boolean as argument" obj))


;;;; fixnums validation

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (fixnum/false who obj)
  (or (not obj) (fixnum? obj))
  (assertion-violation who "expected false or fixnum as argument" obj))

(define-argument-validation (positive-fixnum who obj)
  (and (fixnum? obj)
       ($fx< 0 obj))
  (assertion-violation who "expected positive fixnum as argument" obj))

(define-argument-validation (negative-fixnum who obj)
  (and (fixnum? obj)
       ($fx> 0 obj))
  (assertion-violation who "expected negative fixnum as argument" obj))

(define-argument-validation (non-positive-fixnum who obj)
  (and (fixnum? obj)
       ($fx>= 0 obj))
  (assertion-violation who "expected non-positive fixnum as argument" obj))

(define-argument-validation (non-negative-fixnum who obj)
  (and (fixnum? obj)
       ($fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as argument" obj))

(define-argument-validation (fixnum-in-inclusive-range who obj min max)
  (and (fixnum? obj)
       ($fx>= obj min)
       ($fx<= obj max))
  (assertion-violation who
    (string-append "expected fixnum in inclusive range ["
		   (number->string min) ", " (number->string max)
		   "] as argument")
    obj))

(define-argument-validation (fixnum-in-exclusive-range who obj min max)
  (and (fixnum? obj)
       ($fx> obj min)
       ($fx< obj max))
  (assertion-violation who
    (string-append "expected fixnum in exclusive range ("
		   (number->string min) ", " (number->string max)
		   ") as argument")
    obj))

(define-argument-validation (even-fixnum who obj)
  (and (fixnum? obj)
       (fxeven? obj))
  (assertion-violation who "expected even fixnum as argument" obj))

(define-argument-validation (odd-fixnum who obj)
  (and (fixnum? obj)
       (fxodd? obj))
  (assertion-violation who "expected odd fixnum as argument" obj))


;;;; exact integers validation

(define-inline (exact-integer? obj)
  (or (fixnum? obj)
      (bignum? obj)))

;;; --------------------------------------------------------------------

(define-argument-validation (exact-integer who obj)
  (exact-integer? obj)
  (assertion-violation who "expected exact integer as argument" obj))

(define-argument-validation (exact-integer/false who obj)
  (or (not obj) (exact-integer? obj))
  (assertion-violation who "expected false or exact integer as argument" obj))

(define-argument-validation (positive-exact-integer who obj)
  (and (exact-integer? obj)
       (< 0 obj))
  (assertion-violation who "expected positive exact integer as argument" obj))

(define-argument-validation (negative-exact-integer who obj)
  (and (exact-integer? obj)
       (> 0 obj))
  (assertion-violation who "expected negative exact integer as argument" obj))

(define-argument-validation (non-positive-exact-integer who obj)
  (and (exact-integer? obj)
       (>= 0 obj))
  (assertion-violation who "expected non-positive exact integer as argument" obj))

(define-argument-validation (non-negative-exact-integer who obj)
  (and (exact-integer? obj)
       (<= 0 obj))
  (assertion-violation who "expected non-negative exact integer as argument" obj))

(define-argument-validation (exact-integer-in-inclusive-range who obj min max)
  (and (exact-integer? obj)
       (>= obj min)
       (<= obj max))
  (assertion-violation who
    (string-append "expected exact integer in inclusive range ["
		   (number->string min) ", " (number->string max)
		   "] as argument")
    obj))

(define-argument-validation (exact-integer-in-exclusive-range who obj min max)
  (and (exact-integer? obj)
       (> obj min)
       (< obj max))
  (assertion-violation who
    (string-append "expected exact integer in exclusive range ("
		   (number->string min) ", " (number->string max)
		   ") as argument")
    obj))

(define-argument-validation (even-exact-integer who obj)
  (and (exact-integer? obj)
       (even? obj))
  (assertion-violation who "expected even exact integer as argument" obj))

(define-argument-validation (odd-exact-integer who obj)
  (and (exact-integer? obj)
       (odd? obj))
  (assertion-violation who "expected odd exact integer as argument" obj))


;;;; C language "int" type

(define-argument-validation (signed-int who obj)
  (words.signed-int? obj)
  (assertion-violation who "expected exact integer as argument" obj))

(define-argument-validation (signed-int/false who obj)
  (or (not obj) (words.signed-int? obj))
  (assertion-violation who "expected false or exact integer as argument" obj))

(define-argument-validation (positive-signed-int who obj)
  (and (words.signed-int? obj)
       (< 0 obj))
  (assertion-violation who "expected positive exact integer as argument" obj))

(define-argument-validation (negative-signed-int who obj)
  (and (words.signed-int? obj)
       (> 0 obj))
  (assertion-violation who "expected negative exact integer as argument" obj))

(define-argument-validation (non-positive-signed-int who obj)
  (and (words.signed-int? obj)
       (>= 0 obj))
  (assertion-violation who "expected non-positive exact integer as argument" obj))

(define-argument-validation (non-negative-signed-int who obj)
  (and (words.signed-int? obj)
       (<= 0 obj))
  (assertion-violation who "expected non-negative exact integer as argument" obj))

(define-argument-validation (signed-int-in-inclusive-range who obj min max)
  (and (words.signed-int? obj)
       (>= obj min)
       (<= obj max))
  (assertion-violation who
    (string-append "expected exact integer in inclusive range ["
		   (number->string min) ", " (number->string max)
		   "] as argument")
    obj))

(define-argument-validation (signed-int-in-exclusive-range who obj min max)
  (and (words.signed-int? obj)
       (> obj min)
       (< obj max))
  (assertion-violation who
    (string-append "expected exact integer in exclusive range ("
		   (number->string min) ", " (number->string max)
		   ") as argument")
    obj))

(define-argument-validation (even-signed-int who obj)
  (and (words.signed-int? obj)
       (even? obj))
  (assertion-violation who "expected even exact integer as argument" obj))

(define-argument-validation (odd-signed-int who obj)
  (and (words.signed-int? obj)
       (odd? obj))
  (assertion-violation who "expected odd exact integer as argument" obj))


;;;; bit sized integers

(define-argument-validation (word-u8 who obj)
  (words.word-u8? obj)
  (assertion-violation who
    "expected exact integer representing an octet as argument" obj))

(define-argument-validation (word-s8 who obj)
  (words.word-s8? obj)
  (assertion-violation who
    "expected exact integer representing a byte as argument" obj))

(define-argument-validation (word-u16 who obj)
  (words.word-u16? obj)
  (assertion-violation who
    "expected exact integer representing a 16-bit unsigned integer as argument" obj))

(define-argument-validation (word-s16 who obj)
  (words.word-s16? obj)
  (assertion-violation who
    "expected exact integer representing a 16-bit signed integer as argument" obj))

(define-argument-validation (word-u32 who obj)
  (words.word-u32? obj)
  (assertion-violation who
    "expected exact integer representing a 32-bit unsigned integer as argument" obj))

(define-argument-validation (word-s32 who obj)
  (words.word-s32? obj)
  (assertion-violation who
    "expected exact integer representing a 32-bit signed integer as argument" obj))

(define-argument-validation (word-u64 who obj)
  (words.word-u64? obj)
  (assertion-violation who
    "expected exact integer representing a 64-bit unsigned integer as argument" obj))

(define-argument-validation (word-s64 who obj)
  (words.word-s64? obj)
  (assertion-violation who
    "expected exact integer representing a 64-bit signed integer as argument" obj))

(define-argument-validation (word-u128 who obj)
  (words.word-u128? obj)
  (assertion-violation who
    "expected exact integer representing a 128-bit unsigned integer as argument" obj))

(define-argument-validation (word-s128 who obj)
  (words.word-s128? obj)
  (assertion-violation who
    "expected exact integer representing a 128-bit signed integer as argument" obj))

(define-argument-validation (word-u256 who obj)
  (words.word-u256? obj)
  (assertion-violation who
    "expected exact integer representing a 256-bit unsigned integer as argument" obj))

(define-argument-validation (word-s256 who obj)
  (words.word-s256? obj)
  (assertion-violation who
    "expected exact integer representing a 256-bit signed integer as argument" obj))

(define-argument-validation (machine-word who obj)
  (words.machine-word? obj)
  (assertion-violation who
    "expected exact integer representing a machine word as argument" obj))


;;;; false or bit sized integers

(define-argument-validation (word-u8/false who obj)
  (or (not obj) (words.word-u8? obj))
  (assertion-violation who
    "expected false or exact integer representing an octet as argument" obj))

(define-argument-validation (word-s8/false who obj)
  (or (not obj) (words.word-s8? obj))
  (assertion-violation who
    "expected false or exact integer representing a byte as argument" obj))

(define-argument-validation (word-u16/false who obj)
  (or (not obj) (words.word-u16? obj))
  (assertion-violation who
    "expected false or exact integer representing a 16-bit unsigned integer as argument"
    obj))

(define-argument-validation (word-s16/false who obj)
  (or (not obj) (words.word-s16? obj))
  (assertion-violation who
    "expected false or exact integer representing a 16-bit signed integer as argument"
    obj))

(define-argument-validation (word-u32/false who obj)
  (or (not obj) (words.word-u32? obj))
  (assertion-violation who
    "expected false or exact integer representing a 32-bit unsigned integer as argument"
    obj))

(define-argument-validation (word-s32/false who obj)
  (or (not obj) (words.word-s32? obj))
  (assertion-violation who
    "expected false or exact integer representing a 32-bit signed integer as argument"
    obj))

(define-argument-validation (word-u64/false who obj)
  (or (not obj) (words.word-u64? obj))
  (assertion-violation who
    "expected false or exact integer representing a 64-bit unsigned integer as argument"
    obj))

(define-argument-validation (word-s64/false who obj)
  (or (not obj) (words.word-s64? obj))
  (assertion-violation who
    "expected false or exact integer representing a 64-bit signed integer as argument"
    obj))

(define-argument-validation (word-u128/false who obj)
  (or (not obj) (words.word-u128? obj))
  (assertion-violation who
    "expected false or exact integer representing a 128-bit unsigned integer as argument"
    obj))

(define-argument-validation (word-s128/false who obj)
  (or (not obj) (words.word-s128? obj))
  (assertion-violation who
    "expected false or exact integer representing a 128-bit signed integer as argument"
    obj))

(define-argument-validation (word-u256/false who obj)
  (or (not obj) (words.word-u256? obj))
  (assertion-violation who
    "expected false or exact integer representing a 256-bit unsigned integer as argument"
    obj))

(define-argument-validation (word-s256/false who obj)
  (or (not obj) (words.word-s256? obj))
  (assertion-violation who
    "expected false or exact integer representing a 256-bit signed integer as argument"
    obj))

(define-argument-validation (machine-word/false who obj)
  (or (not obj) (words.machine-word? obj))
  (assertion-violation who
    "expected false or exact integer representing a machine word as argument"
    obj))


;;;; C language values

(define-argument-validation (unsigned-char who obj)
  (words.unsigned-char? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned char\""
    obj))

(define-argument-validation (signed-char who obj)
  (words.signed-char? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"signed char\""
    obj))

(define-argument-validation (unsigned-short who obj)
  (words.unsigned-short? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned short int\""
    obj))

(define-argument-validation (signed-short who obj)
  (words.signed-short? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"signed short int\""
    obj))

(define-argument-validation (unsigned-int who obj)
  (words.unsigned-int? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned int\""
    obj))

(define-argument-validation (unsigned-long who obj)
  (words.unsigned-long? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned long\""
    obj))

(define-argument-validation (signed-long who obj)
  (words.signed-long? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"signed long\""
    obj))

(define-argument-validation (unsigned-long-long who obj)
  (words.unsigned-long-long? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"unsigned long long\""
    obj))

(define-argument-validation (signed-long-long who obj)
  (words.signed-long-long? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"signed long long\""
    obj))

(define-argument-validation (pointer-integer who obj)
  (words.pointer-integer? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"void *\"" obj))

(define-argument-validation (size_t who obj)
  (words.size_t? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"size_t\""
    obj))

(define-argument-validation (ssize_t who obj)
  (words.ssize_t? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"ssize_t\""
    obj))

(define-argument-validation (off_t who obj)
  (words.off_t? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"off_t\""
    obj))

(define-argument-validation (ptrdiff_t who obj)
  (words.ptrdiff_t? obj)
  (assertion-violation who
    "expected exact integer in the range of the C language type \"ptrdiff_t\""
    obj))


;;;; false or C language values

(define-argument-validation (unsigned-char/false who obj)
  (or (not obj) (words.unsigned-char? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned char\""
    obj))

(define-argument-validation (signed-char/false who obj)
  (or (not obj) (words.signed-char? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"signed char\""
    obj))

(define-argument-validation (unsigned-short/false who obj)
  (or (not obj) (words.unsigned-short? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned short int\""
    obj))

(define-argument-validation (signed-short/false who obj)
  (or (not obj) (words.signed-short? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"signed short int\""
    obj))

(define-argument-validation (unsigned-int/false who obj)
  (or (not obj) (words.unsigned-int? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned int\""
    obj))

(define-argument-validation (unsigned-long/false who obj)
  (or (not obj) (words.unsigned-long? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned long\""
    obj))

(define-argument-validation (signed-long/false who obj)
  (or (not obj) (words.signed-long? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"signed long\""
    obj))

(define-argument-validation (unsigned-long-long/false who obj)
  (or (not obj) (words.unsigned-long-long? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"unsigned long long\""
    obj))

(define-argument-validation (signed-long-long/false who obj)
  (or (not obj) (words.signed-long-long? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"signed long long\""
    obj))

(define-argument-validation (pointer-integer/false who obj)
  (or (not obj) (words.pointer-integer? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"void *\""
    obj))

(define-argument-validation (size_t/false who obj)
  (or (not obj) (words.size_t? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"size_t\""
    obj))

(define-argument-validation (ssize_t/false who obj)
  (or (not obj) (words.ssize_t? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"ssize_t\""
    obj))

(define-argument-validation (off_t/false who obj)
  (or (not obj) (words.off_t? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"off_t\""
    obj))

(define-argument-validation (ptrdiff_t/false who obj)
  (or (not obj) (words.ptrdiff_t? obj))
  (assertion-violation who
    "expected false or exact integer in the range of the C language type \"ptrdiff_t\""
    obj))


;;;; strings

(define-inline (index-for-string? str idx)
  (and (fixnum? idx)
       ($fx>= idx 0)
       ($fx<  idx ($string-length str))))

(define-inline (one-off-index-for-string? str idx)
  (and (fixnum? idx)
       ($fx>= idx 0)
       ($fx<= idx ($string-length str))))

;;; --------------------------------------------------------------------

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (string/false who obj)
  (or (not obj) (string? obj))
  (assertion-violation who "expected false or string as argument" obj))

(define-argument-validation (non-empty-string who obj)
  (and (string? obj)
       ($fx< 0 ($string-length obj)))
  (assertion-violation who "expected non-empty string as argument" obj))

(define-argument-validation (non-empty-string/false who obj)
  (or (not obj)
      (and (string? obj)
	   ($fx< 0 ($string-length obj))))
  (assertion-violation who "expected false or non-empty string as argument" obj))

(define-argument-validation (index-for-string who str idx)
  ;;We assume that STR has already been validated as string.
  (index-for-string? str idx)
  (assertion-violation who "expected valid fixnum as index for string argument" idx str))

(define-argument-validation (index-and-count-for-string who str idx count)
  ;;We assume that STR has already been validated as string.
  (and (index-for-string? str idx)
       (fixnum? count)
       (let ((end (+ idx count)))
	 (one-off-index-for-string? str end)))
  (assertion-violation who
    "expected valid fixnums as arguments for string index and character count"
    idx count str))

(define-argument-validation (start-and-end-for-string who str start end)
  ;;We assume that STR has already been validated as string.
  (and (index-for-string? str start)
       (index-for-string? str end)
       ($fx<= start end))
  (assertion-violation who
    "expected valid fixnums as arguments for start and end string indexes"
    start end str))

(define-argument-validation (start-and-past-for-string who str start past)
  ;;We assume that STR has already been validated as string.
  (and (index-for-string? str start)
       (one-off-index-for-string? str past)
       ($fx<= start past))
  (assertion-violation who
    "expected valid fixnums as arguments for start and past string indexes"
    start past str))


;;;; vectors

(define-inline (index-for-vector? vec idx)
  (and (fixnum? idx)
       ($fx>= idx 0)
       ($fx<  idx ($vector-length vec))))

(define-inline (one-off-index-for-vector? vec idx)
  (and (fixnum? idx)
       ($fx>= idx 0)
       ($fx<= idx ($vector-length vec))))

;;; --------------------------------------------------------------------

(define-argument-validation (vector who obj)
  (vector? obj)
  (assertion-violation who "expected vector as argument" obj))

(define-argument-validation (vector/false who obj)
  (or (not obj) (vector? obj))
  (assertion-violation who "expected false or vector as argument" obj))

(define-argument-validation (non-empty-vector who obj)
  (and (vector? obj)
       ($fx< 0 ($vector-length obj)))
  (assertion-violation who "expected non-empty vector as argument" obj))

(define-argument-validation (non-empty-vector/false who obj)
  (or (not obj)
      (and (vector? obj)
	   ($fx< 0 ($vector-length obj))))
  (assertion-violation who "expected false or non-empty vector as argument" obj))

(define-argument-validation (index-for-vector who vec idx)
  ;;We assume that VEC has already been validated as vector.
  (index-for-vector? vec idx)
  (assertion-violation who "expected valid fixnum as index for vector argument" idx vec))

(define-argument-validation (index-and-count-for-vector who vec idx count)
  ;;We assume that VEC has already been validated as vector.
  (and (index-for-vector? vec idx)
       (fixnum? count)
       (let ((end (+ idx count)))
	 (one-off-index-for-vector? vec end)))
  (assertion-violation who
    "expected valid fixnums as arguments for vector index and character count"
    idx count vec))

(define-argument-validation (start-and-end-for-vector who vec start end)
  ;;We assume that VEC has already been validated as vector.
  (and (index-for-vector? vec start)
       (index-for-vector? vec end)
       ($fx<= start end))
  (assertion-violation who
    "expected valid fixnums as arguments for start and end vector indexes"
    start end vec))

(define-argument-validation (start-and-past-for-vector who vec start past)
  ;;We assume that VEC has already been validated as vector.
  (and (index-for-vector? vec start)
       (one-off-index-for-vector? vec past)
       ($fx<= start past))
  (assertion-violation who
    "expected valid fixnums as arguments for start and past vector indexes"
    start past vec))


;;;; bytevectors

(define-inline (index-for-bytevector? vec idx)
  (and (fixnum? idx)
       ($fx>= idx 0)
       ($fx<  idx ($bytevector-length vec))))

(define-inline (one-off-index-for-bytevector? vec idx)
  (and (fixnum? idx)
       ($fx>= idx 0)
       ($fx<= idx ($bytevector-length vec))))

;;; --------------------------------------------------------------------

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

(define-argument-validation (bytevector/false who obj)
  (or (not obj) (bytevector? obj))
  (assertion-violation who "expected false or bytevector as argument" obj))

(define-argument-validation (non-empty-bytevector who obj)
  (and (bytevector? obj)
       ($fx< 0 ($bytevector-length obj)))
  (assertion-violation who "expected non-empty bytevector as argument" obj))

(define-argument-validation (non-empty-bytevector/false who obj)
  (or (not obj)
      (and (bytevector? obj)
	   ($fx< 0 ($bytevector-length obj))))
  (assertion-violation who "expected false or non-empty bytevector as argument" obj))

(define-argument-validation (index-for-bytevector who vec idx)
  ;;We assume that VEC has already been validated as bytevector.
  (index-for-bytevector? vec idx)
  (assertion-violation who "expected valid fixnum as index for bytevector argument" idx vec))

(define-argument-validation (index-and-count-for-bytevector who vec idx count)
  ;;We assume that VEC has already been validated as bytevector.
  (and (index-for-bytevector? vec idx)
       (fixnum? count)
       (let ((end (+ idx count)))
	 (one-off-index-for-bytevector? vec end)))
  (assertion-violation who
    "expected valid fixnums as arguments for bytevector index and character count"
    idx count vec))

(define-argument-validation (start-and-end-for-bytevector who vec start end)
  ;;We assume that VEC has already been validated as bytevector.
  (and (index-for-bytevector? vec start)
       (index-for-bytevector? vec end)
       ($fx<= start end))
  (assertion-violation who
    "expected valid fixnums as arguments for start and end bytevector indexes"
    start end vec))

(define-argument-validation (start-and-past-for-bytevector who vec start past)
  ;;We assume that VEC has already been validated as bytevector.
  (and (index-for-bytevector? vec start)
       (one-off-index-for-bytevector? vec past)
       ($fx<= start past))
  (assertion-violation who
    "expected valid fixnums as arguments for start and past bytevector indexes"
    start past vec))


;;;; symbols

(define-argument-validation (symbol who obj)
  (symbol? obj)
  (assertion-violation who "expected symbol as argument" obj))

(define-argument-validation (symbol/false who obj)
  (or (not obj) (symbol? obj))
  (assertion-violation who "expected false or symbol as argument" obj))


;;;; enum-sets

(define-argument-validation (enum-set who obj)
  (enum-set? obj)
  (assertion-violation who "expected enum-set as argument" obj))

(define-argument-validation (enum-set/false who obj)
  (or (not obj) (enum-set? obj))
  (assertion-violation who "expected false or enum-set as argument" obj))


;;;; pointers

(define-argument-validation (pointer who obj)
  (pointer? obj)
  (assertion-violation who "expected pointer as argument" obj))

(define-argument-validation (pointer/false who obj)
  (or (not obj) (pointer? obj))
  (assertion-violation who "expected false or pointer as argument" obj))

(define-argument-validation (non-null-pointer who obj)
  (and (pointer? obj)
       (not (pointer-null? obj)))
  (assertion-violation who "expected non NULL pointer as argument" obj))


;;;; memory-blocks

(define-argument-validation (memory-block who obj)
  (memory-block? obj)
  (assertion-violation who "expected memory-block as argument" obj))

(define-argument-validation (memory-block/false who obj)
  (or (not obj) (memory-block? obj))
  (assertion-violation who "expected false or memory-block as argument" obj))


;;;; flonums validation

(define-argument-validation (flonum who obj)
  (flonum? obj)
  (assertion-violation who "expected flonum as argument" obj))


;;;; bignums validation

(define-argument-validation (bignum who obj)
  (bignum? obj)
  (assertion-violation who "expected bignum as argument" obj))


;;;; ports

(define-argument-validation (port who obj)
  (port? obj)
  (assertion-violation who "expected port as argument" obj))

(define-argument-validation (port/false who obj)
  (or (not obj) (port? obj))
  (assertion-violation who "expected false or port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (input-port who obj)
  (input-port? obj)
  (assertion-violation who "expected input port as argument" obj))

(define-argument-validation (input-port/false who obj)
  (or (not obj) (input-port? obj))
  (assertion-violation who "expected false or input port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (output-port who obj)
  (output-port? obj)
  (assertion-violation who "expected output port as argument" obj))

(define-argument-validation (output-port/false who obj)
  (or (not obj) (output-port? obj))
  (assertion-violation who "expected false or output port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (input/output-port who obj)
  (input/output-port? obj)
  (assertion-violation who "expected input/output port as argument" obj))

(define-argument-validation (input/output-port/false who obj)
  (or (not obj) (input/output-port? obj))
  (assertion-violation who "expected false or input/output port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (textual-port who obj)
  (textual-port? obj)
  (assertion-violation who "expected textual port as argument" obj))

(define-argument-validation (textual-port/false who obj)
  (or (not obj) (textual-port? obj))
  (assertion-violation who "expected false or textual port as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (binary-port who obj)
  (binary-port? obj)
  (assertion-violation who "expected binary port as argument" obj))

(define-argument-validation (binary-port/false who obj)
  (or (not obj) (binary-port? obj))
  (assertion-violation who "expected false or binary port as argument" obj))


;;;; procedures

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (procedure/false who obj)
  (or (not obj) (procedure? obj))
  (assertion-violation who "expected false or procedure as argument" obj))


;;;; generalised C strings

(define-inline (general-c-string? ?obj)
  (let ((obj ?obj))
    (or (string?	obj)
	(bytevector?	obj)
	(pointer?	obj)
	(memory-block?	obj))))

(define-argument-validation (general-c-string who obj)
  (general-c-string? obj)
  (assertion-violation who "expected general C string as argument" obj))

(define-argument-validation (general-c-string/false who obj)
  (or (not obj)
      (string? obj)
      (bytevector? obj)
      (pointer? obj)
      (memory-block? obj))
  (assertion-violation who "expected false or general C string as argument" obj))

;;; --------------------------------------------------------------------

(define-inline (general-c-buffer? ?obj)
  (let ((obj ?obj))
    (or (bytevector?	obj)
	(pointer?	obj)
	(memory-block?	obj))))

(define-argument-validation (general-c-buffer who obj)
  (general-c-buffer? obj)
  (assertion-violation who "expected general C buffer as argument" obj))

(define-argument-validation (general-c-buffer/false who obj)
  (or (not obj) (general-c-buffer? obj))
  (assertion-violation who "expected false or general C buffer as argument" obj))

;;; --------------------------------------------------------------------

(define-inline (general-c-sticky-buffer? ?obj)
  (let ((obj ?obj))
    (or (pointer?	obj)
	(memory-block?	obj))))

(define-argument-validation (general-c-sticky-buffer who obj)
  (general-c-sticky-buffer? obj)
  (assertion-violation who "expected general C sticky buffer as argument" obj))

(define-argument-validation (general-c-sticky-buffer/false who obj)
  (or (not obj) (general-c-sticky-buffer? obj))
  (assertion-violation who "expected general C sticky buffer as argument" obj))


;;;; done

)

;;; end of file
