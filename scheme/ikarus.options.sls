;;;
;;;Part of: Vicare Scheme
;;;Contents: configuration options
;;;Date: Mon Jun  4, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (ikarus.options)
  (export
    print-verbose-messages?
    print-debug-messages?
    print-library-debug-messages?
    print-loaded-libraries?

    debug-mode-enabled?
    drop-assertions?
    strict-r6rs
    ;; tagged language parameter options
    typed-language?
    ;; vicare configuration options
    vicare-built-with-arguments-validation-enabled
    vicare-built-with-srfi-enabled
    vicare-built-with-ffi-enabled
    vicare-built-with-iconv-enabled
    vicare-built-with-posix-enabled
    vicare-built-with-glibc-enabled
    vicare-built-with-linux-enabled)
  (import (except (vicare)
		  vicare-built-with-arguments-validation-enabled
		  vicare-built-with-srfi-enabled
		  vicare-built-with-ffi-enabled
		  vicare-built-with-iconv-enabled
		  vicare-built-with-posix-enabled
		  vicare-built-with-glibc-enabled
		  vicare-built-with-linux-enabled))


;;;; some boolean options

(define-syntax define-boolean-option
  (syntax-rules ()
    ((_ ?who)
     (define-boolean-option ?who #f))
    ((_ ?who ?default)
     (define ?who
       (let ((bool ?default))
	 (case-lambda
	  (()
	   bool)
	  ((value)
	   (set! bool (and value #t)))))))
    ))

(define-boolean-option debug-mode-enabled?)
(define-boolean-option print-verbose-messages?)
(define-boolean-option print-debug-messages?)
(define-boolean-option print-library-debug-messages?)
(define-boolean-option print-loaded-libraries?)


;;;; some parameter boolean options

(define-syntax define-parameter-boolean-option
  (syntax-rules ()
    ((_ ?who)
     (define-parameter-boolean-option ?who #f))
    ((_ ?who ?default)
     (define ?who
       (make-parameter ?default
	 (lambda (value)
	   (and value #t)))))
    ))

(define-parameter-boolean-option strict-r6rs)

;;Turn on typed language extensions.
;;
(define-parameter-boolean-option typed-language?)

;;When  set to  true: expand  every ASSERT  macro into  its expression,  dropping the
;;assertions.  Specifically:
;;
;;   (assert ?expr)
;;
;;is expanded into:
;;
;;   ?expr
;;
;;so that side effects in ?EXPR are performed and the resulting value is returned.
(define-parameter-boolean-option drop-assertions?)


;;;; vicare build configuration options

(module (vicare-built-with-arguments-validation-enabled
	 vicare-built-with-srfi-enabled
	 vicare-built-with-ffi-enabled
	 vicare-built-with-iconv-enabled
	 vicare-built-with-posix-enabled
	 vicare-built-with-glibc-enabled
	 vicare-built-with-linux-enabled)
  (module (arguments-validation
	   VICARE_BUILT_WITH_SRFI_ENABLED
	   VICARE_BUILT_WITH_ICONV_ENABLED
	   VICARE_BUILT_WITH_FFI_ENABLED
	   VICARE_BUILT_WITH_NAUSICAA_ENABLED
	   VICARE_BUILT_WITH_POSIX_ENABLED
	   VICARE_BUILT_WITH_GLIBC_ENABLED
	   VICARE_BUILT_WITH_LINUX_ENABLED)
    (include "ikarus.config.scm" #t))
  (define (vicare-built-with-arguments-validation-enabled)
    arguments-validation)
  (define (vicare-built-with-srfi-enabled)	VICARE_BUILT_WITH_SRFI_ENABLED)
  (define (vicare-built-with-iconv-enabled)	VICARE_BUILT_WITH_ICONV_ENABLED)
  (define (vicare-built-with-ffi-enabled)	VICARE_BUILT_WITH_FFI_ENABLED)
  (define (vicare-built-with-nausicaa-enabled)	VICARE_BUILT_WITH_NAUSICAA_ENABLED)
  (define (vicare-built-with-posix-enabled)	VICARE_BUILT_WITH_POSIX_ENABLED)
  (define (vicare-built-with-glibc-enabled)	VICARE_BUILT_WITH_GLIBC_ENABLED)
  (define (vicare-built-with-linux-enabled)	VICARE_BUILT_WITH_LINUX_ENABLED)
  #| end of module |# )


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.options")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
