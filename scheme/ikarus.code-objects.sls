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


#!vicare
(library (ikarus.code-objects)
  (export
    make-code
    code-reloc-vector		code-freevars
    code-size			code-ref
    code-set!			set-code-reloc-vector!
    set-code-annotation!	procedure-annotation
    make-annotation-indirect	annotation-indirect?
    code->thunk)
  (import (except (vicare)
		  make-code
		  code-reloc-vector		code-freevars
		  code-size			code-ref
		  code-set!			set-code-reloc-vector!
		  set-code-annotation!		procedure-annotation)
    ;;NOTE  This library  is needed  to build  a  new boot  image.  Let's  try to  do
    ;;everything here using the system  libraries and not loading external libraries.
    ;;(Marco Maggi; Fri May 23, 2014)
    (vicare system $fx)
    (vicare system $codes)
    (prefix (vicare platform words)
	    words.))


;;;; helpers

(define ($fx-non-negative? X)
  (and (fixnum? X)
       ($fx>= X 0)))

(define (code-size? obj)
  ($fx-non-negative? obj))

(define (number-of-freevars? obj)
  ($fx-non-negative? obj))

(define (code-with-no-freevars? code)
  (and (code? code)
       ($fxzero? ($code-freevars code))))


(define* (make-code {code-size code-size?} {freevars number-of-freevars?})
  (foreign-call "ikrt_make_code" code-size freevars '#()))

(define* (code-reloc-vector {x code?})
  ($code-reloc-vector x))

(define* (code-freevars {x code?})
  ($code-freevars x))

(define* (code-size {x code?})
  ($code-size x))

(define* (code-set! {code code?} {idx fixnum?} {octet words.word-u8?})
  (unless (and ($fx>= idx 0)
	       ($fx<  idx ($code-size code)))
    (procedure-argument-violation __who__
      "expected valid fixnum code index argument" idx code))
  ($code-set! code idx octet))

(define* (code-ref {code code?} {idx fixnum?})
  (unless (and ($fx>= idx 0)
	       ($fx<  idx ($code-size code)))
    (procedure-argument-violation __who__
      "expected valid fixnum code index argument" idx code))
  ($code-ref code idx))

(define* (set-code-reloc-vector! {code code?} {vec vector?})
  (foreign-call "ikrt_set_code_reloc_vector" code vec))

(define* (set-code-annotation! {code code?} v)
  (foreign-call "ikrt_set_code_annotation" code v))

(define* (code->thunk {code code-with-no-freevars?})
  ($code->closure code))

(define-struct annotation-indirect
  ())

(define* (procedure-annotation {x procedure?})
  (let ((ae ($code-annotation ($closure-code x))))
    (if (annotation-indirect? ae)
	($annotated-procedure-annotation x)
      ae)))


;;;; done

)

;;; end of file
