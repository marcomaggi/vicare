;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: condition type definitions
;;;Date: Mon Nov  3, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.compiler.condition-types)
  (export
    make-library-condition library-condition? library-condition-name
    make-module-condition module-condition? module-condition-name
    make-compiler-internal-error compiler-internal-error?

    ;; these go in (vicare compiler)
    make-compile-time-error			compile-time-error?
    make-compile-time-arity-error		compile-time-arity-error?
    make-compile-time-core-type-error		compile-time-core-type-error?
    make-compile-time-operand-core-type-error	compile-time-operand-core-type-error?
    make-compile-time-retval-core-type-error	compile-time-retval-core-type-error?
;;;
    compile-time-error
    compile-time-arity-error
    compile-time-operand-core-type-error
    compile-time-retval-core-type-error
    compiler-internal-error)
  (import (vicare))


(define-condition-type &library
    &condition
  make-library-condition library-condition?
  (name		library-condition-name))

(define-condition-type &module
    &condition
  make-module-condition module-condition?
  (name		module-condition-name))

;;; --------------------------------------------------------------------

(define-condition-type &compile-time-error
    &assertion
  make-compile-time-error compile-time-error?)

(define-condition-type &compile-time-arity-error
    &compile-time-error
  make-compile-time-arity-error compile-time-arity-error?)

(define-condition-type &compile-time-core-type-error
    &compile-time-error
  make-compile-time-core-type-error compile-time-core-type-error?)

(define-condition-type &compile-time-operand-core-type-error
    &compile-time-error
  make-compile-time-operand-core-type-error compile-time-operand-core-type-error?)

(define-condition-type &compile-time-retval-core-type-error
    &compile-time-error
  make-compile-time-retval-core-type-error compile-time-retval-core-type-error?)

(define (compile-time-error module-who who message . irritants)
  (%raise-error module-who who message (make-compile-time-error) irritants))

(define (compile-time-arity-error module-who who message . irritants)
  (%raise-error module-who who message (make-compile-time-arity-error) irritants))

(define (compile-time-operand-core-type-error module-who who message . irritants)
  (%raise-error module-who who message (make-compile-time-operand-core-type-error) irritants))

(define (compile-time-retval-core-type-error module-who who message . irritants)
  (%raise-error module-who who message (make-compile-time-retval-core-type-error) irritants))

;;; --------------------------------------------------------------------

(define-condition-type &compiler-internal-error &compile-time-error
  make-compiler-internal-error compiler-internal-error?)

(define (compiler-internal-error module-who who message . irritants)
  (%raise-error module-who who message (make-compiler-internal-error) irritants))

;;; --------------------------------------------------------------------

(define (%raise-error module-who who message cnd irritants)
  (raise
   (condition cnd
	      (make-library-condition 'compiler)
	      (make-module-condition module-who)
	      (make-who-condition who)
	      (make-message-condition message)
	      (make-irritants-condition irritants))))


;;;; done

)

;;; end of file
;; Local Variables:
;; End:
