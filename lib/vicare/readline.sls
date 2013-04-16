;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: bindings for GNU Readline
;;;Date: Tue Feb  7, 2012
;;;
;;;Abstract
;;;
;;;
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
(library (vicare readline)
  (export

    ;; readline compatible API
    readline-enabled?
    readline
    make-readline-input-port

;;; --------------------------------------------------------------------
;;; GNU Readline specific API

    rl-version)
  (import (vicare)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe operations)
	    unsafe.))


;;;; arguments validation

;; (define-argument-validation (prompt who obj)
;;   (or (not obj) (bytevector? obj) (string? obj))
;;   (assertion-violation who "expected false, bytevector or string as prompt argument" obj))

;; (define-argument-validation (prompt-maker who obj)
;;   (procedure? obj)
;;   (assertion-violation who "expected procedure as prompt maker argument" obj))


;;;; access to C API

(define-inline (capi.rl-version)
  (foreign-call "ik_readline_rl_version"))


;;;; high-level API

(define (rl-version)
  (capi.rl-version))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-bytevectors 'scheme-indent-function 1)
;; End:
