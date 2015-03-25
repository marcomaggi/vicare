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
(library (ikarus.options)
  (export
    verbose?
    print-debug-messages?
    print-loaded-libraries?

    debug-mode-enabled?
    report-errors-at-runtime
    strict-r6rs
    descriptive-labels
    ;; tagged language parameter options
    tagged-language.rhs-tag-propagation?
    tagged-language.datums-as-operators?
    tagged-language.setter-forms?
    tagged-language?
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

(define-boolean-option verbose?)
(define-boolean-option debug-mode-enabled?)
(define-boolean-option print-debug-messages?)
(define-boolean-option print-loaded-libraries?)
(define-boolean-option report-errors-at-runtime)
(define-boolean-option strict-r6rs)
(define-boolean-option descriptive-labels)


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

;;This option is about the tagged language.  When we write:
;;
;;   (let ((a 1)) . ?body)
;;
;;the  identifier  A is  the  left-hand  side  of  the binding  and  the
;;expression 1  is the right-hand side  of the binding; since  A is left
;;untagged in  the source code,  the expander  will tag it,  by default,
;;with "<untagged>".
;;
;;* If this option  is turned OFF: the identifier A  is left tagged with
;;  "<untagged>".  We are  free to assign any object to  A, mutating the
;;  bound value  multiple times with  objects of different tag;  this is
;;  standard Scheme behaviour.
;;
;;* When this option is turned ON:  the expander infers that the RHS has
;;  signature "(<fixnum>)", so it propagates the tag from the RHS to the
;;  LHS  overriding "<untagged>"  with "<fixnum>".   This will  cause an
;;  error to be raised if we mutate the binding assigning to A an object
;;  whose tag is not "<fixnum>".
;;
(define-parameter-boolean-option tagged-language.rhs-tag-propagation?)

;;The option "datums as operators" allows us to use a non-closure object
;;as operator  in call  forms.  When  this option  is on,  the following
;;example evaluations are possible:
;;
;;(123 positive?)	=> #t
;;
;;   The operator is the fixnum 123 and the expander determines that its
;;   tag is  "<fixnum>"; this  form matches  the syntax  of a  method or
;;   accessor application.
;;
;;("ciao" [1])		=> #\i
;;
;;   The operator is the string  "ciao" and the expander determines that
;;   its tag  is "<string>"; this  form matches  the syntax of  a getter
;;   application.
;;
(define-parameter-boolean-option tagged-language.datums-as-operators?)

;;The option "setter forms" allows us to use a non-identifier expression
;;as  left-hand side  in a  SET! syntax.   When this  option is  on, the
;;following evaluations are possible:
;;
;;(set! (?expr (?key00 ?key0* ...) (?key11* ?key1** ...) ...) ?new-value)
;;(set!  ?expr (?key00 ?key0* ...) (?key11* ?key1** ...) ...  ?new-value)
;;
;;   This is the  setter syntax.  ?EXPR can be any  expression for which
;;   the  expander can  determine  the retvals  signature.  This  syntax
;;   mutates  a property  of  the  result of  ?EXPR  to ?NEW-VALUE;  the
;;   property is selected by the set of specified keys.  Example:
;;
;;      (define V (vector 1 2 3))
;;      (set! V [1] 9)
;;      V => #(1 9 3)
;;
;;(set! (?expr ?field-name) ?new-value)
;;
;;   This is the mutator syntax.  ?EXPR  can be any expression for which
;;   the  expander can  determine  the retvals  signature.  This  syntax
;;   mutates the (concrete or virtual)  field selected by ?FIELD-NAME to
;;   ?NEW-VALUE.
;;
(define-parameter-boolean-option tagged-language.setter-forms?)

;;Turn on  tagged language  extensions.  When this  parameter is  set to
;;true: we must also set to true all the tagged language sub-parameters.
;;
(define-parameter-boolean-option tagged-language?)


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
    (include "ikarus.config.ss" #t))
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
