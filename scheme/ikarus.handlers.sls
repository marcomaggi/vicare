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
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (vicare system parameters)
  (export make-parameter)
  (import (except (vicare) make-parameter))
  (define make-parameter
    ;;The  actual  implementation of  MAKE-PARAMETER  is  weirdly integrated  in  the
    ;;compiler.  The forms below with format:
    ;;
    ;;   (make-parameter ?arg ...)
    ;;
    ;;are expanded by the compiler with the actual implementation.
    ;;
    (internal-body
      (import (vicare))
      (case-lambda
        ((x guard)
	 (make-parameter x guard))
        ((x)
	 (make-parameter x))))))

(library (ikarus.pointer-value)
  (export pointer-value)
  (import (only (vicare) define import))
  (define (pointer-value x)
    (import (vicare))
    (pointer-value x)))


(library (vicare system handlers)
  (export
    interrupt-handler engine-handler
    $apply-nonprocedure-error-handler
    $incorrect-args-error-handler
    $multiple-values-error
    $do-event)
  (import (except (vicare)
		  interrupt-handler
		  engine-handler)
          (only (vicare system $interrupts)
		$interrupted?
		$unset-interrupted!))


(define interrupt-handler
  (make-parameter
      (lambda ()
        (raise-continuable
	 (condition (make-interrupted-condition)
		    (make-message-condition "received an interrupt signal"))))
    (lambda (x)
      (if (procedure? x)
	  x
	(assertion-violation 'interrupt-handler "not a procedure" x)))))

(define ($apply-nonprocedure-error-handler x)
  (assertion-violation 'apply "not a procedure" x))

(define ($incorrect-args-error-handler proc wrong-num-of-args list-of-args)
  ;;Whenever an attempt to apply a closure object to the wrong number of arguments is
  ;;performed: the assembly subroutine SL_INVALID_ARGS  calls this function to handle
  ;;the error.
  ;;
  ;;P is a reference to closure object, being the offended one.  WRONG-NUM-OF-ARGS is
  ;;fixnum representing the  wrong number of arguments.  LIST-OF-ARGS is  the list of
  ;;wrong arguments.
  ;;
  (define-condition-type &affected-procedure
      &condition
    make-affected-procedure-condition
    affected-procedure-condition?
    (proc	condition-affected-procedure))
  (define-condition-type &wrong-num-args
      &condition
    make-wrong-num-args-condition
    wrong-num-args-condition?
    (n		condition-wrong-num-args))
  (raise
   (condition (make-assertion-violation)
	      (make-who-condition 'apply)
	      (make-message-condition "incorrect number of arguments")
	      (make-affected-procedure-condition proc)
	      (make-wrong-num-args-condition wrong-num-of-args)
	      (make-irritants-condition (cons proc list-of-args)))))

(define ($multiple-values-error . args)
  ;;Whenever an attempt to return zero  or multiple, but not one, values
  ;;to a single value contest is performed, as in:
  ;;
  ;;   (let ((x (values 1 2)))
  ;;     x)
  ;;
  ;;or:
  ;;
  ;;   (let ((x (values)))
  ;;     x)
  ;;
  ;;this function is called to handle the error.
  ;;
  ;;ARGS is the list of invalid arguments.
  ;;
  (assertion-violation 'apply
    "incorrect number of values returned to single value context" args))

(define ($do-event)
  (cond (($interrupted?)
	 ($unset-interrupted!)
	 ((interrupt-handler)))
	(else
	 ((engine-handler)))))

(define engine-handler
  (make-parameter
      void
    (lambda (x)
      (if (procedure? x)
	  x
	(assertion-violation 'engine-handler "not a procedure" x)))))


;;;; done

#| end of library (vicare system handlers) |# )

;;; end of file
