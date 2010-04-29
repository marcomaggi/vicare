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

(library (ikarus system parameters)
  (export make-parameter)
  (import (except (ikarus) make-parameter))
  (define make-parameter
    (let ()
      (import (ikarus))
      (case-lambda
        [(x guard) (make-parameter x guard)]
        [(x) (make-parameter x)]))))

(library (ikarus.pointer-value)
  (export pointer-value)
  (import (only (ikarus) define import))
  (define (pointer-value x)
    (import (ikarus))
    (pointer-value x)))


(library (ikarus system handlers)
  (export
    interrupt-handler $apply-nonprocedure-error-handler
    $incorrect-args-error-handler $multiple-values-error $debug
    $underflow-misaligned-error top-level-value-error car-error
    cdr-error fxadd1-error fxsub1-error cadr-error fx+-type-error
    fx+-types-error fx+-overflow-error $do-event engine-handler)
  (import (except (ikarus) interrupt-handler engine-handler)
          (only (ikarus system $interrupts) $interrupted? $unset-interrupted!))

  (define interrupt-handler
    (make-parameter
      (lambda ()
        ; FIXME
        ;(set-port-output-index! (console-output-port) 0)
        (raise-continuable
          (condition
            (make-interrupted-condition)
            (make-message-condition "received an interrupt signal"))))
      (lambda (x)
        (if (procedure? x)
            x
            (die 'interrupt-handler "not a procedure" x)))))

  (define engine-handler
    (make-parameter
      void
      (lambda (x)
        (if (procedure? x)
            x
            (die 'engine-handler "not a procedure" x)))))

  (define $apply-nonprocedure-error-handler
    (lambda (x)
      (die 'apply "not a procedure" x)))
  
  (define $incorrect-args-error-handler
    (lambda (p . ls)
      (apply die 'apply "incorrect number of arguments" p ls)))
  
  (define $multiple-values-error
    (lambda args
      (die 'apply 
             "incorrect number of values returned to single value context" 
             args)))
  
  (define $debug
    (lambda (x)
      (foreign-call "ik_error" (cons "DEBUG" x))))
  
  (define $underflow-misaligned-error
    (lambda ()
      (foreign-call "ik_error" "misaligned")))
  
  (define top-level-value-error
    (lambda (x)
      (cond
        [(symbol? x)
         (if (symbol-bound? x)
             (die 'top-level-value-error "BUG: should not happen" x)
             (die #f "unbound" (string->symbol (symbol->string x))))]
        [else
         (die 'top-level-value "not a symbol" x)])))
  
  (define car-error
    (lambda (x)
      (die 'car "not a pair" x)))
  
  (define cdr-error
    (lambda (x)
      (die 'cdr "not a pair" x)))
  
  (define fxadd1-error
    (lambda (x)
      (if (fixnum? x)
          (die 'fxadd1 "overflow")
          (die 'fxadd1 "not a fixnum" x))))
  
  (define fxsub1-error
    (lambda (x)
      (if (fixnum? x)
          (die 'fxsub1 "underflow")
          (die 'fxsub1 "not a fixnum" x))))
  
  (define cadr-error
    (lambda (x)
      (die 'cadr "invalid list structure" x)))
  
  (define fx+-type-error
    (lambda (x)
      (die 'fx+ "not a fixnum" x)))
  
  (define fx+-types-error
    (lambda (x y)
      (die 'fx+ "not a fixnum"
             (if (fixnum? x) y x))))
  
  (define fx+-overflow-error
    (lambda (x y)
      (die 'fx+ "overflow")))
  
  (define $do-event
    (lambda ()
      (cond
        [($interrupted?)
         ($unset-interrupted!)
         ((interrupt-handler))]
        [else
         ((engine-handler))])))

  )
