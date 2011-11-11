;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: GNU C Library platform API
;;;Date: Wed Nov  9, 2011
;;;
;;;Abstract
;;;
;;;
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


(library (ikarus.glibc)
  (export
    ;; operating system environment variables
    clearenv

    ;; file system directories
    dirfd

    ;; temporary files and directories
    mkstemp		mkdtemp
    )
  (import (except (ikarus)
		  ;; operating system environment variables
		  clearenv

		  ;; file system directories
		  dirfd
		  )
    (prefix (only (ikarus.posix)
		  directory-stream?
		  directory-stream-closed?
		  directory-stream-pointer)
	    posix.)
    (vicare syntactic-extensions)
    (vicare platform-constants)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare unsafe-operations)
	    unsafe.))


;;;; helpers

(define-syntax define-for-glibc
  (if #t
      (syntax-rules ()
	((_ (?who . ?args) . ?body)
	 (define (?who . ?args) . ?body)))
    (syntax-rules ()
      ((_ (?who . ?args) . ?body)
       (define (?who . ?args)
	 (assertion-violation '?who
	   "attempt to call unimplemented GNU C Library function"))))))

(define (raise-errno-error who errno . irritants)
  (raise (condition
	  (make-error)
	  (make-who-condition who)
	  (make-message-condition (strerror errno))
	  (make-irritants-condition irritants))))


;;;; arguments validation

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (boolean who obj)
  (boolean? obj)
  (assertion-violation who "expected boolean as argument" obj))

(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))

(define-argument-validation (string who obj)
  (string? obj)
  (assertion-violation who "expected string as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))

;;; --------------------------------------------------------------------

(define-argument-validation (pid who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum pid as argument" obj))

(define-argument-validation (signal who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum signal code as argument" obj))

(define-argument-validation (directory-stream who obj)
  (posix.directory-stream? obj)
  (assertion-violation who "expected directory stream as argument" obj))

(define-argument-validation (open-directory-stream who obj)
  (not (posix.directory-stream-closed? obj))
  (assertion-violation who "expected open directory stream as argument" obj))


;;;; operating system environment variables

(define-for-glibc (clearenv)
  (capi.glibc-clearenv))


;;;; file system directories

(define-for-glibc (dirfd stream)
  (define who 'dirfd)
  (with-arguments-validation (who)
      ((directory-stream       stream)
       (open-directory-stream  stream))
    (let ((rv (capi.glibc-dirfd (posix.directory-stream-pointer stream))))
      (if (unsafe.fx<= 0 rv)
	  rv
	(raise-errno-error who rv stream)))))


;;;; temporary files and directories

(define (mkstemp template)
  (define who 'mkstemp)
  (with-arguments-validation (who)
      ((bytevector  template))
    (let ((rv (capi.glibc-mkstemp template)))
      (if (unsafe.fx<= 0 rv)
	  rv
	(raise-errno-error who rv template)))))

(define (mkdtemp template)
  (define who 'mkdtemp)
  (with-arguments-validation (who)
      ((bytevector  template))
    (let ((rv (capi.glibc-mkdtemp template)))
      (if (fixnum? rv)
	  (raise-errno-error who rv template)
	rv))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'define-for-glibc 'scheme-indent-function 1)
;; End:
