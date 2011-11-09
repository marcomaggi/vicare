;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: Linux platform API
;;;Date: Mon Nov  7, 2011
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


(library (ikarus.linux)
  (export
    ;; operative system environment variables
    clearenv

    ;; process termination status
    waitid
    make-siginfo_t		siginfo_t?
    siginfo_t-si_pid		siginfo_t-si_uid
    siginfo_t-si_signo		siginfo_t-si_status
    siginfo_t-si_code
    WIFCONTINUED
    )
  (import (except (ikarus)
		  ;; operative system environment variables
		  clearenv

		  ;; process termination status
		  waitid
		  make-siginfo_t		siginfo_t?
		  siginfo_t-si_pid		siginfo_t-si_uid
		  siginfo_t-si_signo		siginfo_t-si_status
		  siginfo_t-si_code
		  WIFCONTINUED

		  )
    (vicare syntactic-extensions)
    (vicare platform-constants)
    (prefix (vicare unsafe-capi)
	    capi.)
    (prefix (vicare unsafe-operations)
	    unsafe.))


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

;;; --------------------------------------------------------------------

(define-argument-validation (pid who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum pid as argument" obj))

(define-argument-validation (signal who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum signal code as argument" obj))


;;;; operative system environment variables

(define (clearenv)
  (capi.linux-clearenv))


;;;; process termination status

(define-struct siginfo_t
  (si_pid si_uid si_signo si_status si_code))

(define (waitid idtype id options)
  (define who 'waitid)
  (with-arguments-validation (who)
      ((fixnum  idtype)
       (fixnum	id)
       (fixnum	options))
    (capi.linux-waitid idtype id (make-siginfo_t #f #f #f #f #f) options)))

(define (WIFCONTINUED status)
  (define who 'WIFCONTINUED)
  (with-arguments-validation (who)
      ((fixnum  status))
    (capi.linux-WIFCONTINUED status)))


;;;; done

)

;;; end of file
