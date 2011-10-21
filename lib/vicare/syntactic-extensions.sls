;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: utility syntaxes
;;;Date: Fri Oct 21, 2011
;;;
;;;Abstract
;;;
;;;	Utility syntaxes used also in the source code of Vicare.
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    define-inline
    define-syntax*
    unwind-protect
    define-argument-validation
    with-arguments-validation)
  (import (rnrs)
    (for (prefix (vicare installation-configuration)
		 config.)
	 expand))


(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (?who ?stx) . ?body)
     (define-syntax ?who (lambda (?stx) . ?body)))))

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
	   (call-with-values
	       (lambda () ?body)
	     (lambda return-values
	       (cleanup)
	       (apply values return-values)))))))))


(define-syntax define-argument-validation
  ;;Transform:
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


(define-syntax with-arguments-validation
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
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ (?who) ((?validator ?arg ...) ...) . ?body)
	 ;;Whether we  include the arguments  checking or not,  we build
	 ;;the output form validating the input form.
	 (let* ((body		#'(begin . ?body))
		(output-form	(%build-output-form #'?who
						    #'(?validator ...)
						    #'((?arg ...) ...)
						    body)))
	   (if config.arguments-validation output-form body)))
	(_
	 (%synner "invalid input form" #f))))

    (define (%build-output-form who validators list-of-args body)
      (syntax-case validators ()
	(()
	 body)
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


;;;; done

)

;;; end of file
