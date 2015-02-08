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


(library (ikarus.exceptions)
  (export
    with-exception-handler
    raise		raise-continuable
    error		warning
    assertion-violation	die
    run-escape-handler-thunks
    run-unwind-protection-cleanup-upon-exit?)
  (import (except (vicare)
		  with-exception-handler
		  raise			raise-continuable
		  error			warning
		  assertion-violation	die))


(define current-handlers
  (make-parameter
      (list (lambda (x)
	      (let ((port (console-error-port)))
		(display "*** Vicare: unhandled exception:\n" port)
		(print-condition x (console-error-port)))
	      (when (serious-condition? x)
		(exit -1)))
	    (lambda args
	      (exit -1)))))

(define* (with-exception-handler {handler procedure?} {proc2 procedure?})
  (parameterize ((current-handlers (cons handler (current-handlers))))
    (proc2)))

(let-syntax
    ((raise-machinery (syntax-rules ()
			((_ ?exc . ?tail)
			 (let* ((handler*      (current-handlers))
				(head-handler  (car handler*))
				(tail-handler* (cdr handler*)))
			   (parameterize ((current-handlers tail-handler*))
			     (head-handler ?exc)
			     . ?tail)))
			)))
  (define (raise-continuable exc)
    (raise-machinery exc))
  (define (raise exc)
    (raise-machinery exc
		     (raise (condition
			     (make-non-continuable-violation)
			     (make-message-condition "handler returned from non-continuable exception")))))
  #| end of LET-SYNTAX |# )


(module (error assertion-violation warning die)

  (let-syntax ((define-raiser
		 (syntax-rules ()
		   ((_ ?who ?raiser ?make-main-condition)
		    (define* (?who {who false-or-string-or-symbol?} {msg string?} . irritants)
		      (%raise-exception (quote ?who)
					?raiser ?make-main-condition
					who msg irritants))))))
    (define-raiser error		raise			make-error)
    (define-raiser assertion-violation	raise			make-assertion-violation)
    (define-raiser warning		raise-continuable	make-warning)
    (define-raiser die			raise			make-assertion-violation))

  (define (%raise-exception caller-who raise* cond* who msg irritants)
    (raise* (condition (cond*)
		       (if who
			   (make-who-condition who)
			 (condition))
		       (make-message-condition   msg)
		       (make-irritants-condition irritants))))

  (define (false-or-string-or-symbol? obj)
    (or (not obj)
	(symbol? obj)
	(string? obj)))

  #| end of module |# )


(define (run-escape-handler-thunks handlers)
  (for-each (lambda (handler)
	      (call/cc
		  (lambda (escape)
		    (with-exception-handler
			escape
		      handler))))
    handlers))

(define run-unwind-protection-cleanup-upon-exit?
  ;;This is used  in the interaction between the unwind-protection  mechanism and the
  ;;GUARD syntax.
  ;;
  (make-parameter #f))


;;;; done

#| end of library |# )

;;; end of file
