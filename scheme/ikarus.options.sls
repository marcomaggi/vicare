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
    ((_ ?who ?default)
     (define ?who
       (let ((bool ?default))
	 (case-lambda
	  (()
	   bool)
	  ((value)
	   (set! bool (and value #t)))))))
    ))

(define-boolean-option verbose?			#f)
(define-boolean-option print-loaded-libraries   #f)
(define-boolean-option report-errors-at-runtime #f)
(define-boolean-option strict-r6rs              #f)
(define-boolean-option descriptive-labels       #f)


;;;; vicare build configuration options

(module (vicare-built-with-arguments-validation-enabled)
  (module (arguments-validation)
    (include "ikarus.config.ss" #t))
  (define (vicare-built-with-arguments-validation-enabled)
    arguments-validation)
  #| end of module |# )

(define (vicare-built-with-srfi-enabled)
  (foreign-call "ikrt_vicare_built_with_srfi_enabled"))

(define (vicare-built-with-ffi-enabled)
  (foreign-call "ikrt_vicare_built_with_ffi_enabled"))

(define (vicare-built-with-iconv-enabled)
  (foreign-call "ikrt_vicare_built_with_iconv_enabled"))

(define (vicare-built-with-posix-enabled)
  (foreign-call "ikrt_vicare_built_with_posix_enabled"))

(define (vicare-built-with-glibc-enabled)
  (foreign-call "ikrt_vicare_built_with_glibc_enabled"))

(define (vicare-built-with-linux-enabled)
  (foreign-call "ikrt_vicare_built_with_linux_enabled"))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
