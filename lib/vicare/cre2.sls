;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: binding to CRE2
;;;Date: Fri Jan  6, 2012
;;;
;;;Abstract
;;;
;;;	Built in  binding to the CRE2  library: a C wrapper  for the RE2
;;;	regular expressions library from Google.
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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare cre2)
  (export
    ;; version functions
    enabled?			version-interface-current
    version-interface-revision	version-interface-age

    ;; precompiled regular expressions
    (rename (%make-regexp	make-regexp))
    delete-regexp
    regexp?

    ;; precompiled configuration options
    (rename (%make-options	make-options))
    delete-options
    options?

    )
  (import (vicare)
    (only (vicare syntactic-extensions)
	  define-argument-validation
	  with-arguments-validation
	  define-inline)
    (prefix (only (vicare ffi)
		  pointer?
		  pointer-null?)
	    ffi.))


;;;; arguments validation

(define-argument-validation (string/bytevector who obj)
  (or (string? obj) (bytevector? obj))
  (assertion-violation who "expected string or bytevector as argument" obj))

(define-argument-validation (false/options who obj)
  (or (not obj) (options? obj))
  (assertion-violation who "expected false or a RE2 options object as argument" obj))

(define-argument-validation (rex who obj)
  (regexp? obj)
  (assertion-violation who "expected a RE2 regular expression object as argument" obj))

(define-argument-validation (options who obj)
  (options? obj)
  (assertion-violation who "expected a RE2 options object as argument" obj))


;;;; C API

(define-inline (capi.cre2-enabled?)
  (foreign-call "ikrt_cre2_enabled"))

(define-inline (capi.cre2-version-interface-current)
  (foreign-call "ikrt_cre2_version_interface_current"))

(define-inline (capi.cre2-version-interface-revision)
  (foreign-call "ikrt_cre2_version_interface_revision"))

(define-inline (capi.cre2-version-interface-age)
  (foreign-call "ikrt_cre2_version_interface_age"))

;;; --------------------------------------------------------------------

(define-inline (capi.cre2-new pattern options)
  (foreign-call "ikrt_cre2_new" pattern options))

(define-inline (capi.cre2-delete pointer)
  (foreign-call "ikrt_cre2_delete" pointer))

;;; --------------------------------------------------------------------

(define-inline (capi.cre2-opt-new)
  (foreign-call "ikrt_cre2_opt_new"))

(define-inline (capi.cre2-opt-delete pointer)
  (foreign-call "ikrt_cre2_opt_delete" pointer))


;;;; version functions

(define (enabled?)
  (capi.cre2-enabled?))

(define (version-interface-current)
  (capi.cre2-version-interface-current))

(define (version-interface-revision)
  (capi.cre2-version-interface-revision))

(define (version-interface-age)
  (capi.cre2-version-interface-age))


;;;; precompiled regular expressions

(define-struct regexp
  (pointer))

(define (%struct-regexp-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[re2-regexp")
  (%display " pointer=")	(%display (regexp-pointer S))
  (%display "]"))

(define regexp-guardian
  (make-guardian))

(define (%free-allocated-regexp)
  (do ((S (regexp-guardian) (regexp-guardian)))
      ((not S))
    (capi.cre2-delete (regexp-pointer S))))

(define (%make-regexp pattern opts)
  (define who 'cre2.make-regexp)
  (with-arguments-validation (who)
      ((string/bytevector	pattern)
       (false/options		opts))
    (let* ((pattern.bv	(cond ((bytevector? pattern)
			       pattern)
			      ((options? opts)
			       #f)
			      (else
			       (string->utf8 pattern))))
	   (rv		(capi.cre2-new pattern.bv (options-pointer opts))))
      (cond ((ffi.pointer?)
	     (regexp-guardian (make-regexp rv)))
	    ((not rv)
	     (error who
	       "memory allocation error while building RE2 regular expression"
	       pattern opts))
	    (else
	     (error who
	       (latin1->string (cdr rv))
	       (car rv) pattern opts))))))

(define (delete-regexp rex)
  (define who 'cre2.delete-regexp)
  (with-arguments-validation (who)
      ((rex	rex))
    (capi.cre2-delete (regexp-pointer rex))))


;;;; configuration options

(define-struct options
  (pointer))

(define (%struct-options-printer S port sub-printer)
  (define-inline (%display thing)
    (display thing port))
  (%display "#[re2-options")
  (%display " pointer=")	(%display (options-pointer S))
  (%display "]"))

(define options-guardian
  (make-guardian))

(define (%free-allocated-options)
  (do ((S (options-guardian) (options-guardian)))
      ((not S))
    (capi.cre2-opt-delete (options-pointer S))))

(define (%make-options)
  (define who 'cre2.make-options)
  (let ((rv (capi.cre2-opt-new)))
    (if (ffi.pointer-null? rv)
	(error who
	  "memory allocation error while building RE2 options object")
      (options-guardian (make-options rv)))))

(define (delete-options opts)
  (define who 'cre2.delete-options)
  (with-arguments-validation (who)
      ((options	opts))
    (capi.cre2-opt-delete (options-pointer opts))))


;;;; done

(post-gc-hooks (cons* %free-allocated-regexp
		      %free-allocated-options
		      (post-gc-hooks)))

(set-rtd-printer! (type-descriptor regexp)  %struct-regexp-printer)
(set-rtd-printer! (type-descriptor options) %struct-options-printer)

)

;;; end of file
