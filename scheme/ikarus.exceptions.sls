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
  (options typed-language)
  (export
    with-exception-handler
    raise		raise-continuable
    error		warning
    assertion-violation)
  (import (except (vicare)
		  with-exception-handler
		  raise			raise-continuable
		  error			warning
		  assertion-violation))


(define current-handlers
  (make-parameter
      ;;There  must  be   always  at  least  two  handlers  in   list  referenced  by
      ;;CURRENT-HANDLERS.
      (list
       (lambda (x)
	 (cond ((warning? x)
		;;When a "&warning"  is raised with RAISE-CONTINUABLE: we  want to go
		;;on with the execution.
		(let ((port (console-error-port)))
		  (display "*** Vicare: warning:\n" port)
		  (print-condition x port))
		(values))
	       (else
		(let ((port (console-error-port)))
		  (display "*** Vicare: unhandled exception:\n" port)
		  (print-condition x port))
		(when (serious-condition? x)
		  (exit -1))
		(values))))
       (lambda args
	 (exit -1))
       #| end of LIST |# )))

(define-type <handler>
  (lambda (<wildcard>) => <list>))

(define (with-exception-handler {handler <handler>} {proc2 <thunk>})
  (parameterize ((current-handlers (cons handler (current-handlers))))
    (proc2)))

(let-syntax
    ((raise-machinery (syntax-rules ()
			((_ ?exc . ?tail)
			 ;;There  are  always  at  least two  handlers  in  the  list
			 ;;returned by CURRENT-HANDLERS.
			 (let* (({handler*	(nelist-of <handler>)}	(current-handlers))
				({head-handler	<handler>}		(car handler*))
				({tail-handler*	(nelist-of <handler>)}	(cdr handler*)))
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


(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?raiser ?make-main-condition)
		 (define ({?who . <bottom>} {who <&who-value>} {msg <string>} . {irritants (list-of <top>)})
		   (?raiser (condition (?make-main-condition)
				       (if who
					   (make-who-condition who)
					 (condition))
				       (make-message-condition   msg)
				       (make-irritants-condition irritants)))))
		)))
  (declare error		raise			make-error)
  (declare assertion-violation	raise			make-assertion-violation)
  (declare warning		raise-continuable	make-warning)
  ;;(declare die		raise			make-assertion-violation)
  #| end of LET-SYNTAX |# )


;;;; done

;; #!vicare
;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.exceptions after"))

#| end of library |# )

;;; end of file
