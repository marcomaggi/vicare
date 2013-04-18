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
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
#!(load-shared-library "vicarecre2")
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
    set-posix-syntax!		posix-syntax?
    set-longest-match!		longest-match?
    set-log-errors!		log-errors?
    set-literal!		literal?
    set-never-nl!		never-nl?
    set-case-sensitive!		case-sensitive?
    set-perl-classes!		perl-classes?
    set-word-boundary!		word-boundary?
    set-one-line!		one-line?
    set-max-mem!		max-mem

    ;; matching regular expressions
    match
    )
  (import (vicare)
    (only (vicare language-extensions syntaxes)
	  define-argument-validation
	  with-arguments-validation
	  define-inline)
    (prefix (vicare unsafe operations)
	    unsafe.)
    (prefix (only (vicare ffi)
		  dlopen
		  pointer?
		  pointer-null?
		  null-pointer)
	    ffi.)
    (prefix (only (vicare platform words)
		  signed-int?)
	    words.))


;;;; helpers

(define-syntax with-bytevectors
  (syntax-rules ()
    ((_ ((?value.bv ?value) ...) . ?body)
     (let ((?value.bv (if (bytevector? ?value)
			  ?value
			(string->utf8 ?value)))
	   ...)
       . ?body))))


;;;; arguments validation

;; (define-argument-validation (boolean who obj)
;;   (boolean? obj)
;;   (assertion-violation who "expected boolean as argument" obj))

(define-argument-validation (symbol who obj)
  (symbol? obj)
  (assertion-violation who "expected symbol as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (string/bytevector who obj)
  (or (string? obj) (bytevector? obj))
  (assertion-violation who "expected string or bytevector as argument" obj))

(define-argument-validation (positive-signed-int who obj)
  (and (words.signed-int? obj) (positive? obj))
  (assertion-violation who "expected a positive signed int as argument" obj))

(define-argument-validation (index who obj)
  (and (fixnum? obj) (unsafe.fx<= 0 obj))
  (assertion-violation who "expected non-negative fixnum as argument" obj))

(define-argument-validation (false/index who obj)
  (or (not obj) (and (fixnum? obj) (unsafe.fx<= 0 obj)))
  (assertion-violation who "expected false or non-negative fixnum as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (rex who obj)
  (regexp? obj)
  (assertion-violation who "expected a RE2 regular expression object as argument" obj))

(define-argument-validation (options who obj)
  (options? obj)
  (assertion-violation who "expected a RE2 options object as argument" obj))

(define-argument-validation (false/options who obj)
  (or (not obj) (options? obj))
  (assertion-violation who "expected false or a RE2 options object as argument" obj))


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

(define-inline (capi.cre2-opt-set-posix-syntax opt bool)
  (foreign-call "ikrt_cre2_opt_set_posix_syntax" opt bool))
(define-inline (capi.cre2-opt-posix-syntax opt)
  (foreign-call "ikrt_cre2_opt_posix_syntax" opt))

(define-inline (capi.cre2-opt-set-longest-match opt bool)
  (foreign-call "ikrt_cre2_opt_set_longest_match" opt bool))
(define-inline (capi.cre2-opt-longest-match opt)
  (foreign-call "ikrt_cre2_opt_longest_match" opt))

(define-inline (capi.cre2-opt-set-log-errors opt bool)
  (foreign-call "ikrt_cre2_opt_set_log_errors" opt bool))
(define-inline (capi.cre2-opt-log-errors opt)
  (foreign-call "ikrt_cre2_opt_log_errors" opt))

(define-inline (capi.cre2-opt-set-literal opt bool)
  (foreign-call "ikrt_cre2_opt_set_literal" opt bool))
(define-inline (capi.cre2-opt-literal opt)
  (foreign-call "ikrt_cre2_opt_literal" opt))

(define-inline (capi.cre2-opt-set-never-nl opt bool)
  (foreign-call "ikrt_cre2_opt_set_never_nl" opt bool))
(define-inline (capi.cre2-opt-never-nl opt)
  (foreign-call "ikrt_cre2_opt_never_nl" opt))

(define-inline (capi.cre2-opt-set-case-sensitive opt bool)
  (foreign-call "ikrt_cre2_opt_set_case_sensitive" opt bool))
(define-inline (capi.cre2-opt-case-sensitive opt)
  (foreign-call "ikrt_cre2_opt_case_sensitive" opt))

(define-inline (capi.cre2-opt-set-perl-classes opt bool)
  (foreign-call "ikrt_cre2_opt_set_perl_classes" opt bool))
(define-inline (capi.cre2-opt-perl-classes opt)
  (foreign-call "ikrt_cre2_opt_perl_classes" opt))

(define-inline (capi.cre2-opt-set-word-boundary opt bool)
  (foreign-call "ikrt_cre2_opt_set_word_boundary" opt bool))
(define-inline (capi.cre2-opt-word-boundary opt)
  (foreign-call "ikrt_cre2_opt_word_boundary" opt))

(define-inline (capi.cre2-opt-set-one-line opt bool)
  (foreign-call "ikrt_cre2_opt_set_one_line" opt bool))
(define-inline (capi.cre2-opt-one-line opt)
  (foreign-call "ikrt_cre2_opt_one_line" opt))

(define-inline (capi.cre2-opt-set-max-mem opt dim)
  (foreign-call "ikrt_cre2_opt_set_max_mem" opt dim))
(define-inline (capi.cre2-opt-max-mem opt)
  (foreign-call "ikrt_cre2_opt_max_mem" opt))

;;; --------------------------------------------------------------------

(define-inline (capi.cre2-match rex text start end anchor)
  (foreign-call "ikrt_cre2_match" rex text start end anchor))


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

(define %make-regexp
  (case-lambda
   ((pattern)
    (%make-regexp pattern #f))
   ((pattern opts)
    (define who 'cre2.make-regexp)
    (with-arguments-validation (who)
	((string/bytevector	pattern)
	 (false/options		opts))
      (with-bytevectors ((pattern.bv pattern))
	(let ((rv (capi.cre2-new pattern.bv (if opts
						(options-pointer opts)
					      (ffi.null-pointer)))))
	  (cond ((ffi.pointer? rv)
		 (regexp-guardian (make-regexp rv)))
		((not rv)
		 (error who
		   "memory allocation error while building RE2 regular expression"
		   pattern opts))
		(else
		 (error who
		   (latin1->string (cdr rv))
		   (car rv) pattern opts)))))))))

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
  (let ((P (options-pointer S)))
    (%display "#[re2-options")
    (%display " pointer=")		(%display P)
    (%display " posix-syntax?=")	(%display (capi.cre2-opt-posix-syntax P))
    (%display " longest-match?=")	(%display (capi.cre2-opt-longest-match P))
    (%display " log-errors?=")		(%display (capi.cre2-opt-log-errors P))
    (%display " literal?=")		(%display (capi.cre2-opt-literal P))
    (%display " never-nl?=")		(%display (capi.cre2-opt-never-nl P))
    (%display " case-sensitive?=")	(%display (capi.cre2-opt-case-sensitive P))
    (%display " perl-classes?=")	(%display (capi.cre2-opt-perl-classes P))
    (%display " word-boundary?=")	(%display (capi.cre2-opt-word-boundary P))
    (%display " one-line?=")		(%display (capi.cre2-opt-one-line P))
    (%display " max-mem=")		(%display (capi.cre2-opt-max-mem P))
    (%display "]")))

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

;;; --------------------------------------------------------------------

(let-syntax ((define-option-setter (syntax-rules ()
				     ((_ ?name ?who ?func)
				      (define (?name opt bool)
					(define who '?who)
					(with-arguments-validation (who)
					    ((options	opt))
					  (?func (options-pointer opt) bool)))))))
  (define-option-setter set-posix-syntax!
    cre2.set-posix-syntax!
    capi.cre2-opt-set-posix-syntax)
  (define-option-setter set-longest-match!
    cre2.set-longest-match!
    capi.cre2-opt-set-longest-match)
  (define-option-setter set-log-errors!
    cre2.set-log-errors!
    capi.cre2-opt-set-log-errors)
  (define-option-setter set-literal!
    cre2.set-literal!
    capi.cre2-opt-set-literal)
  (define-option-setter set-never-nl!
    cre2.set-never-nl!
    capi.cre2-opt-set-never-nl)
  (define-option-setter set-case-sensitive!
    cre2.set-case-sensitive!
    capi.cre2-opt-set-case-sensitive)
  (define-option-setter set-perl-classes!
    cre2.set-perl-classes!
    capi.cre2-opt-set-perl-classes)
  (define-option-setter set-word-boundary!
    cre2.set-word-boundary!
    capi.cre2-opt-set-word-boundary)
  (define-option-setter set-one-line!
    cre2.set-one-line!
    capi.cre2-opt-set-one-line))

(let-syntax ((define-option-getter (syntax-rules ()
				     ((_ ?name ?who ?func)
				      (define (?name opt)
					(define who '?who)
					(with-arguments-validation (who)
					    ((options	opt))
					  (?func (options-pointer opt))))))))
  (define-option-getter posix-syntax?
    cre2.posix-syntax?
    capi.cre2-opt-posix-syntax)
  (define-option-getter longest-match?
    cre2.longest-match?
    capi.cre2-opt-longest-match)
  (define-option-getter log-errors?
    cre2.log-errors?
    capi.cre2-opt-log-errors)
  (define-option-getter literal?
    cre2.literal?
    capi.cre2-opt-literal)
  (define-option-getter never-nl?
    cre2.never-nl?
    capi.cre2-opt-never-nl)
  (define-option-getter case-sensitive?
    cre2.case-sensitive?
    capi.cre2-opt-case-sensitive)
  (define-option-getter perl-classes?
    cre2.perl-classes?
    capi.cre2-opt-perl-classes)
  (define-option-getter word-boundary?
    cre2.word-boundary?
    capi.cre2-opt-word-boundary)
  (define-option-getter one-line?
    cre2.one-line?
    capi.cre2-opt-one-line))

;;; --------------------------------------------------------------------

(define (set-max-mem! opt dim)
  (define who 'cre2.set-max-mem!)
  (with-arguments-validation (who)
      ((options			opt)
       (positive-signed-int	dim))
    (capi.cre2-opt-set-max-mem (options-pointer opt) dim)))

(define (max-mem opt)
  (define who 'cre2.max-mem)
  (with-arguments-validation (who)
      ((options	opt))
    (capi.cre2-opt-max-mem (options-pointer opt))))


;;;; matching regular expressions

(define (match rex text start end anchor)
  (define who 'cre2.match)
  (with-arguments-validation (who)
      ((rex			rex)
       (string/bytevector	text)
       (false/index		start)
       (false/index		end)
       (symbol			anchor))
    (with-bytevectors ((text.bv text))
      (let ((start	(or start 0))
	    (end	(or end   (bytevector-length text.bv)))
	    (anchor	(case anchor
			  ((unanchored)	0)
			  ((start)		1)
			  ((both)		2)
			  (else
			   (assertion-violation who "invalid anchor argument" anchor)))))
	(capi.cre2-match (regexp-pointer rex) text.bv start end anchor)))))


;;;; done

(post-gc-hooks (cons* %free-allocated-regexp
		      %free-allocated-options
		      (post-gc-hooks)))

(set-rtd-printer! (type-descriptor regexp)  %struct-regexp-printer)
(set-rtd-printer! (type-descriptor options) %struct-options-printer)

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; End:
