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

(library (ikarus system parameters)
  (export make-parameter)
  (import (except (ikarus) make-parameter))
  (define make-parameter
    (let ()
      (import (ikarus))
      (case-lambda
        ((x guard) (make-parameter x guard))
        ((x) (make-parameter x))))))

(library (ikarus.pointer-value)
  (export pointer-value)
  (import (only (ikarus) define import))
  (define (pointer-value x)
    (import (ikarus))
    (pointer-value x)))


(library (ikarus system handlers)
  (export
    interrupt-handler engine-handler
    $apply-nonprocedure-error-handler
    $incorrect-args-error-handler
    $multiple-values-error
    $do-event)
  (import (except (ikarus)
		  interrupt-handler
		  engine-handler)
          (only (ikarus system $interrupts)
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

(define ($incorrect-args-error-handler p . ls)
  ;;Whenever an attempt to apply a closure object to the wrong number of
  ;;arguments  is  performed:  the assembly  subroutine  SL_INVALID_ARGS
  ;;calls this function to handle the error.
  ;;
  ;;P is a reference to closure object, being the offended one.
  ;;
  ;;LS is  a list containing  a single non-negative  fixnum representing
  ;;the incorrect number of arguments.
  ;;
  (apply assertion-violation 'apply "incorrect number of arguments" p ls))

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


;;;; commented out because unused
;;
;;These where  defined and exported  by the original Ikarus  code.  They
;;appear to be unused.  (Marco Maggi; Nov  3, 2011)
;;

;; (define $debug
;;   (lambda (x)
;;     (foreign-call "ik_error" (cons "DEBUG" x))))

;; (define $underflow-misaligned-error
;;   (lambda ()
;;     (foreign-call "ik_error" "misaligned")))

;; (define car-error
;;   (lambda (x)
;;     (assertion-violation 'car "not a pair" x)))

;; (define cdr-error
;;   (lambda (x)
;;     (assertion-violation 'cdr "not a pair" x)))

;; (define fxadd1-error
;;   (lambda (x)
;;     (if (fixnum? x)
;; 	(assertion-violation 'fxadd1 "overflow")
;;       (assertion-violation 'fxadd1 "not a fixnum" x))))

;; (define fxsub1-error
;;   (lambda (x)
;;     (if (fixnum? x)
;; 	(assertion-violation 'fxsub1 "underflow")
;;       (assertion-violation 'fxsub1 "not a fixnum" x))))

;; (define cadr-error
;;   (lambda (x)
;;     (assertion-violation 'cadr "invalid list structure" x)))

;; (define fx+-type-error
;;   (lambda (x)
;;     (assertion-violation 'fx+ "not a fixnum" x)))

;; (define fx+-types-error
;;   (lambda (x y)
;;     (assertion-violation 'fx+ "not a fixnum"
;; 	 (if (fixnum? x) y x))))

;; (define fx+-overflow-error
;;   (lambda (x y)
;;     (assertion-violation 'fx+ "overflow")))


;;;; done

#| end of library (ikarus system handlers) |# )

;;; end of file
