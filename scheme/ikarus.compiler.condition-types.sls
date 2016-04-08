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
;;;Copyright (C) 2014, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus.compiler.condition-types)
  (export
    &library-rtd				&library-rcd
    &module-rtd					&module-rcd

    make-library-condition			library-condition?
    library-condition-name
    make-module-condition			module-condition?
    module-condition-name
    make-compiler-internal-error		compiler-internal-error?

    ;; these go in (vicare compiler)
    &compile-time-error-rtd			&compile-time-error-rcd
    &compile-time-arity-error-rtd		&compile-time-arity-error-rcd
    &compile-time-core-type-error-rtd		&compile-time-core-type-error-rcd
    &compile-time-operand-core-type-error-rtd	&compile-time-operand-core-type-error-rcd
    &compile-time-retval-core-type-error-rtd	&compile-time-retval-core-type-error-rcd
    &compiler-internal-error-rtd		&compiler-internal-error-rcd
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

;; (define begin-end-of-file
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.compiler.condition-types.sls begin")))


;;;; helpers

(define-syntax (declare-rtd/rcd stx)
  (define (mkname type.id suffix)
    (datum->syntax type.id (string->symbol (string-append (symbol->string (syntax->datum type.id)) suffix))))
  (syntax-case stx ()
    ((_ ?type)
     (identifier? #'?type)
     (with-syntax
	 ((RTD-ID (mkname #'?type "-rtd"))
	  (RCD-ID (mkname #'?type "-rcd")))
       #'(begin
	   (define RTD-ID (record-type-descriptor ?type))
	   (define RCD-ID (record-constructor-descriptor ?type)))))
    ))


(define-condition-type &library
    &condition
  make-library-condition library-condition?
  (name		library-condition-name))
(declare-rtd/rcd &library)

(define-condition-type &module
    &condition
  make-module-condition module-condition?
  (name		module-condition-name))
(declare-rtd/rcd &module)

;;; --------------------------------------------------------------------

(define-condition-type &compile-time-error
    &assertion
  make-compile-time-error compile-time-error?)
(declare-rtd/rcd &compile-time-error)

(define-condition-type &compile-time-arity-error
    &compile-time-error
  make-compile-time-arity-error compile-time-arity-error?)
(declare-rtd/rcd &compile-time-arity-error)

(define-condition-type &compile-time-core-type-error
    &compile-time-error
  make-compile-time-core-type-error compile-time-core-type-error?)
(declare-rtd/rcd &compile-time-core-type-error)

(define-condition-type &compile-time-operand-core-type-error
    &compile-time-error
  make-compile-time-operand-core-type-error compile-time-operand-core-type-error?)
(declare-rtd/rcd &compile-time-operand-core-type-error)

(define-condition-type &compile-time-retval-core-type-error
    &compile-time-error
  make-compile-time-retval-core-type-error compile-time-retval-core-type-error?)
(declare-rtd/rcd &compile-time-retval-core-type-error)

(define (compile-time-error module-who who message . irritants)
  (%raise-error module-who who message (make-compile-time-error) irritants))

(define (compile-time-arity-error module-who who message . irritants)
  (%raise-error module-who who message (make-compile-time-arity-error) irritants))

(define (compile-time-operand-core-type-error module-who who message . irritants)
  (%raise-error module-who who message (make-compile-time-operand-core-type-error) irritants))

(define (compile-time-retval-core-type-error module-who who message . irritants)
  (%raise-error module-who who message (make-compile-time-retval-core-type-error) irritants))

;;; --------------------------------------------------------------------

(define-condition-type &compiler-internal-error
    &compile-time-error
  make-compiler-internal-error compiler-internal-error?)
(declare-rtd/rcd &compiler-internal-error)

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

;;(foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.compiler.condition-types end"))

#| end of library |# )

;;; end of file
;; Local Variables:
;; End:
