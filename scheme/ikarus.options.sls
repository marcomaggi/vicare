;;;
;;;Part of: Vicare Scheme
;;;Contents: configuration options
;;;Date: Mon Jun  4, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare options)
  (export
    verbose?
    print-loaded-libraries
    report-errors-at-runtime
    strict-r6rs
    descriptive-labels
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

  (define-syntax define-boolean-option
    (syntax-rules ()
      ((_ ?who ?default)
       (define ?who
	 (let ((bool ?default))
	   (case-lambda
	    (()
	     bool)
	    ((value)
	     (set! bool (and value #t)))))))
      ))

  (define-boolean-option verbose?		  #f)
  (define-boolean-option print-loaded-libraries   #f)
  (define-boolean-option report-errors-at-runtime #f)
  (define-boolean-option strict-r6rs              #f)
  (define-boolean-option descriptive-labels       #f)

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
    (define (vicare-built-with-srfi-enabled)		VICARE_BUILT_WITH_SRFI_ENABLED)
    (define (vicare-built-with-iconv-enabled)		VICARE_BUILT_WITH_ICONV_ENABLED)
    (define (vicare-built-with-ffi-enabled)		VICARE_BUILT_WITH_FFI_ENABLED)
    (define (vicare-built-with-nausicaa-enabled)	VICARE_BUILT_WITH_NAUSICAA_ENABLED)
    (define (vicare-built-with-posix-enabled)		VICARE_BUILT_WITH_POSIX_ENABLED)
    (define (vicare-built-with-glibc-enabled)		VICARE_BUILT_WITH_GLIBC_ENABLED)
    (define (vicare-built-with-linux-enabled)		VICARE_BUILT_WITH_LINUX_ENABLED)
    #| end of module |# )

  #| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
