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

    ;; arguments validation
    define-argument-validation
    with-arguments-validation
    with-dangerous-arguments-validation
    arguments-validation-forms

    ;; miscellaneous dispatching
    case-word-size		case-endianness
    case-one-operand		case-two-operands
    case-fixnums		case-integers
    define-exact-integer->symbol-function

    ;; auxiliary syntaxes
    big			little
    fixnum		bignum
    flonum		cflonum
    compnum)
  (import (ikarus)
    (for (prefix (vicare installation-configuration)
		 config.)
	 expand)
    (only (ikarus system $fx)
	  $fx=))


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

(define-syntax define-inline-constant
  (syntax-rules ()
    ((_ ?name ?value)
     (define-syntax ?name (identifier-syntax ?value)))))

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
	((_ . ?body)
	 (begin . ?body)))
    (syntax-rules ()
      ((_ . ?body)
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
	 ;;Whether we  include the arguments  checking or not,  we build
	 ;;the output form validating the input form.
	 (let* ((include?	(syntax->datum #'?always-include))
		(body		#'(begin . ?body))
		(output-form	(%build-output-form #'?who
						    #'(?validator ...)
						    #'((?arg ...) ...)
						    body)))
	   (if (or include? config.arguments-validation
		   (let ((S (getenv "VICARE_ARGUMENTS_VALIDATION")))
		     (and (string? S) (string=? S "yes"))))
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

    (define (%synner msg subform)
      (syntax-violation 'with-arguments-validation
	msg (syntax->datum stx) (syntax->datum subform)))

    (main stx)))


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

(define-syntax fixnum	(syntax-rules ()))
(define-syntax bignum	(syntax-rules ()))
(define-syntax flonum	(syntax-rules ()))
(define-syntax cflonum	(syntax-rules ()))
(define-syntax compnum	(syntax-rules ()))

(define-syntax case-one-operand
  (syntax-rules (fixnum bignum flonum cflonum compnum)
    ((case-one-operand (?who ?op)
       ((fixnum)	. ?fixnum-body)
       ((bignum)	. ?bignum-body)
       ((flonum)	. ?flonum-body)
       ((cflonum)	. ?cflonum-body)
       ((compnum)	. ?compnum-body))
     (let ((op ?op))
       (cond ((fixnum?  op)	. ?fixnum-body)
	     ((bignum?  op)	. ?bignum-body)
	     ((flonum?  op)	. ?flonum-body)
	     ((cflonum? op)	. ?cflonum-body)
	     ((compnum? op)	. ?compnum-body)
	     (else
	      (assertion-violation ?who "invalid numeric operand" op)))))))

(define-syntax case-two-operands
  (syntax-rules (fixnum bignum flonum cflonum compnum)
    ((case-two-operands (?who ?op1 ?op2)
       ((fixnum)
	((fixnum)	. ?fixnum/fixnum-body)
	((bignum)	. ?fixnum/bignum-body)
	((flonum)	. ?fixnum/flonum-body)
	((cflonum)	. ?fixnum/cflonum-body)
	((compnum)	. ?fixnum/compnum-body))
       ((bignum)
	((fixnum)	. ?bignum/fixnum-body)
	((bignum)	. ?bignum/bignum-body)
	((flonum)	. ?bignum/flonum-body)
	((cflonum)	. ?bignum/cflonum-body)
	((compnum)	. ?bignum/compnum-body))
       ((flonum)
	((fixnum)	. ?flonum/fixnum-body)
	((bignum)	. ?flonum/bignum-body)
	((flonum)	. ?flonum/flonum-body)
	((cflonum)	. ?flonum/cflonum-body)
	((compnum)	. ?flonum/compnum-body))
       ((cflonum)
	((fixnum)	. ?cflonum/fixnum-body)
	((bignum)	. ?cflonum/bignum-body)
	((flonum)	. ?cflonum/flonum-body)
	((cflonum)	. ?cflonum/cflonum-body)
	((compnum)	. ?cflonum/compnum-body))
       ((compnum)
	((fixnum)	. ?compnum/fixnum-body)
	((bignum)	. ?compnum/bignum-body)
	((flonum)	. ?compnum/flonum-body)
	((cflonum)	. ?compnum/cflonum-body)
	((compnum)	. ?compnum/compnum-body)))
     (case-one-operand (?who ?op1)
       ((fixnum)
	(case-one-operand (?who ?op2)
	  ((fixnum)	. ?fixnum/fixnum-body)
	  ((bignum)	. ?fixnum/bignum-body)
	  ((flonum)	. ?fixnum/flonum-body)
	  ((cflonum)	. ?fixnum/cflonum-body)
	  ((compnum)	. ?fixnum/compnum-body)))
       ((bignum)
	(case-one-operand (?who ?op2)
	  ((fixnum)	. ?bignum/fixnum-body)
	  ((bignum)	. ?bignum/bignum-body)
	  ((flonum)	. ?bignum/flonum-body)
	  ((cflonum)	. ?bignum/cflonum-body)
	  ((compnum)	. ?bignum/compnum-body)))
       ((flonum)
	(case-one-operand (?who ?op2)
	  ((fixnum)	. ?flonum/fixnum-body)
	  ((bignum)	. ?flonum/bignum-body)
	  ((flonum)	. ?flonum/flonum-body)
	  ((cflonum)	. ?flonum/cflonum-body)
	  ((compnum)	. ?flonum/compnum-body)))
       ((cflonum)
	(case-one-operand (?who ?op2)
	  ((fixnum)	. ?cflonum/fixnum-body)
	  ((bignum)	. ?cflonum/bignum-body)
	  ((flonum)	. ?cflonum/flonum-body)
	  ((cflonum)	. ?cflonum/cflonum-body)
	  ((compnum)	. ?cflonum/compnum-body)))
       ((compnum)
	(case-one-operand (?who ?op2)
	  ((fixnum)	. ?compnum/fixnum-body)
	  ((bignum)	. ?compnum/bignum-body)
	  ((flonum)	. ?compnum/flonum-body)
	  ((cflonum)	. ?compnum/cflonum-body)
	  ((compnum)	. ?compnum/compnum-body)))))))


;;;; done

)

;;; end of file
;;Local Variables:
;;eval: (put 'case-one-operand 'scheme-indent-function 1)
;;eval: (put 'case-two-operands 'scheme-indent-function 1)
;;eval: (put 'case-integers 'scheme-indent-function 1)
;;End:
