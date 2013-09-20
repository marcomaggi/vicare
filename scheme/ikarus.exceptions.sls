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


(library (ikarus exceptions)
  (export
    with-exception-handler
    raise		raise-continuable
    error		warning
    assertion-violation	die)
  (import (except (ikarus)
		  with-exception-handler
		  raise			raise-continuable
		  error			warning
		  assertion-violation	die)
    (vicare arguments validation))


(define handlers
  (make-parameter (list (lambda (x)
			  (let ((port (console-error-port)))
			    (display "*** Vicare: unhandled exception:\n" port)
			    (print-condition x (console-error-port)))
			  (when (serious-condition? x)
			    (exit -1)))
			(lambda args
			  (exit -1)))))

(define (with-exception-handler handler proc2)
  (define who 'with-exception-handler)
  (with-arguments-validation (who)
      ((procedure	handler)
       (procedure	proc2))
    (parameterize ((handlers (cons handler (handlers))))
      (proc2))))

(define (raise-continuable x)
  (let* ((h* (handlers))
	 (h  (car h*))
	 (h* (cdr h*)))
    (parameterize ((handlers h*))
      (h x))))

(define (raise x)
  (let* ((h* (handlers))
	 (h  (car h*))
	 (h* (cdr h*)))
    (parameterize ((handlers h*))
      (h x)
      (raise (condition
	      (make-non-continuable-violation)
	      (make-message-condition "handler returned from non-continuable exception"))))))


(module (error assertion-violation warning die)

  (let-syntax ((define-raiser
		 (syntax-rules ()
		   ((_ ?who ?raiser ?make-main-condition)
		    (define (?who who msg . irritants)
		      (%raise-exception (quote ?who)
					?raiser ?make-main-condition
					who msg irritants))))))
    (define-raiser error		raise			make-error)
    (define-raiser assertion-violation	raise			make-assertion-violation)
    (define-raiser warning		raise-continuable	make-warning)
    (define-raiser die			raise			make-assertion-violation))

  (define (%raise-exception caller-who raise* cond* who msg irritants)
    (with-arguments-validation (caller-who)
	((who-spec	who)
	 (string	msg))
      (raise* (condition (cond*)
			 (if who
			     (make-who-condition who)
			   (condition))
			 (make-message-condition   msg)
			 (make-irritants-condition irritants)))))

  (define-argument-validation (who-spec who obj)
    (or (not obj)
	(symbol? obj)
	(string? obj))
    (procedure-argument-violation who "expected false, symbol or string as value for &who" obj))

  #| end of module |# )


;;;; done

)

;;; end of file
