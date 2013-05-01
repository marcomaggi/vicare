;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus.code-objects)
  (export
    make-code code-reloc-vector code-freevars
    code-size code-ref code-set! set-code-reloc-vector!
    set-code-annotation! procedure-annotation
    make-annotation-indirect annotation-indirect?
    code->thunk)
  (import (except (ikarus)
		  make-code code-reloc-vector code-freevars
		  code-size code-ref code-set! set-code-reloc-vector!
		  set-code-annotation! procedure-annotation)
    (ikarus system $fx)
    (ikarus system $codes)
    (vicare arguments validation))


;;;; helpers

(define-inline ($fx-non-negative? X)
  (and (fixnum? X)
       ($fx>= X 0)))


;;;; arguments validation

(define-argument-validation (octet who obj)
  (and (fixnum? obj)
       ($fx>= obj 0)
       ($fx<  obj 256))
  (assertion-violation who "expected fixnum in range [0, 255] as octet argument" obj))

(define-argument-validation (code-size who obj)
  ($fx-non-negative? obj)
  (assertion-violation who "expected non-negative fixnum as code object size argument" obj))

(define-argument-validation (number-of-freevars who obj)
  ($fx-non-negative? obj)
  (assertion-violation who
    "expected non-negative fixnum as number of free variables argument"
    obj))

(define-argument-validation (code who obj)
  (code? obj)
  (assertion-violation who "expected code object as argument" obj))

(define-argument-validation (code-index who code idx)
  (and (fixnum? idx)
       ($fx>= idx 0)
       ($fx<  idx ($code-size code)))
  (assertion-violation who "expected valid fixnum code index argument" idx code))

(define-argument-validation (no-freevars who code)
  ;;We expect CODE to have been already validated as code object.
  ;;
  ($fxzero? ($code-freevars code))
  (assertion-violation who
    "expected code object without free variables as argument"
    code ($code-freevars code)))


(define (make-code code-size freevars)
  (define who 'make-code)
  (with-arguments-validation (who)
      ((code-size		code-size)
       (number-of-freevars	freevars))
    (foreign-call "ikrt_make_code" code-size freevars '#())))

(define (code-reloc-vector x)
  (define who 'code-reloc-vector)
  (with-arguments-validation (who)
      ((code	x))
    ($code-reloc-vector x)))

(define (code-freevars x)
  (define who 'code-freevars)
  (with-arguments-validation (who)
      ((code	x))
    ($code-freevars x)))

(define (code-size x)
  (define who 'code-size)
  (with-arguments-validation (who)
      ((code	x))
    ($code-size x)))

(define (code-set! code idx octet)
  (define who 'code-set!)
  (with-arguments-validation (who)
      ((code		code)
       (code-index	code idx)
       (octet		octet))
    ($code-set! code idx octet)))

(define (code-ref code idx)
  (define who 'code-ref)
  (with-arguments-validation (who)
      ((code		code)
       (code-index	code idx))
    ($code-ref code idx)))

(define (set-code-reloc-vector! code vec)
  (define who 'set-code-reloc-vector!)
  (with-arguments-validation (who)
      ((code	code)
       (vector	vec))
    (foreign-call "ikrt_set_code_reloc_vector" code vec)))

(define (set-code-annotation! code v)
  (define who 'set-code-annotation!)
  (with-arguments-validation (who)
      ((code	code))
    (foreign-call "ikrt_set_code_annotation" code v)))

(define (code->thunk code)
  (define who 'code->thunk)
  (with-arguments-validation (who)
      ((code		code)
       (no-freevars	code))
    ($code->closure code)))

(define-struct annotation-indirect
  ())

(define (procedure-annotation x)
  (define who 'procedure-annotation)
  (with-arguments-validation (who)
      ((procedure	x))
    (let ((ae ($code-annotation ($closure-code x))))
      (if (annotation-indirect? ae)
	  ($annotated-procedure-annotation x)
	ae))))


;;;; done

)

;;; end of file
