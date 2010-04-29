;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus.code-objects)
  (export
    make-code code-reloc-vector code-freevars
    code-size code-ref code-set! set-code-reloc-vector!
    set-code-annotation! procedure-annotation
    make-annotation-indirect annotation-indirect?
    code->thunk)
  (import
    (ikarus system $fx)
    (ikarus system $codes)
    (except (ikarus) make-code code-reloc-vector code-freevars
            code-size code-ref code-set! set-code-reloc-vector!
            procedure-annotation
            set-code-annotation!))

  (define make-code
    (lambda (code-size freevars)
      (unless (and (fixnum? code-size) ($fx>= code-size 0))
        (die 'make-code "not a valid code size" code-size))
      (unless (and (fixnum? freevars) ($fx>= freevars 0))
        (die 'make-code "not a valid number of free vars" freevars))
      (foreign-call "ikrt_make_code" code-size freevars '#())))

  (define code-reloc-vector
    (lambda (x)
      (unless (code? x) (die 'code-reloc-vector "not a code" x))
      ($code-reloc-vector x)))

  (define code-freevars
    (lambda (x)
      (unless (code? x) (die 'code-closure-size "not a code" x))
      ($code-freevars x)))

  (define code-size
    (lambda (x)
      (unless (code? x) (die 'code-size "not a code" x))
      ($code-size x)))

  (define code-set!
    (lambda (x i v)
      (unless (code? x) (die 'code-set! "not a code" x))
      (unless (and (fixnum? i)
                   ($fx>= i 0)
                   ($fx< i ($code-size x)))
        (die 'code-set! "not a valid index" i))
      (unless (and (fixnum? v)
                   ($fx>= v 0)
                   ($fx< v 256))
        (die 'code-set! "not a valid byte" v))
      ($code-set! x i v)))

  (define code-ref
    (lambda (x i)
      (unless (code? x) (die 'code-ref "not a code" x))
      (unless (and (fixnum? i)
                   ($fx>= i 0)
                   ($fx< i ($code-size x)))
        (die 'code-ref "not a valid index" i))
      ($code-ref x i)))

  (define set-code-reloc-vector!
    (lambda (x v)
      (unless (code? x) 
        (die 'set-code-reloc-vector! "not a code" x))
      (unless (vector? v)
        (die 'set-code-reloc-vector! "not a vector" v))
      (foreign-call "ikrt_set_code_reloc_vector" x v)))


  (define set-code-annotation!
    (lambda (x v)
      (unless (code? x) 
        (die 'set-code-annotation! "not a code" x))
      (foreign-call "ikrt_set_code_annotation" x v)))

  (define code->thunk
    (lambda (x)
      (unless (code? x)
        (die 'code->thunk "not a a code object" x))
      (unless ($fxzero? ($code-freevars x))
        (die 'code->thunk "has free variables" x))
      ($code->closure x)))

  (define-struct annotation-indirect ())
  (define (procedure-annotation x)
    (if (procedure? x) 
        (let ([ae ($code-annotation ($closure-code x))])
          (if (annotation-indirect? ae)
              ($annotated-procedure-annotation x)
              ae))
        (die 'procedure-annotation "not a procedure" x)))
  
  )

