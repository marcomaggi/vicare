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

(library (ikarus cafe)
  (export new-cafe waiter-prompt-string cafe-input-port)
  (import (except (ikarus)
		  new-cafe
		  waiter-prompt-string
		  cafe-input-port)
    (vicare language-extensions syntaxes))


;;;; arguments validation

(define-argument-validation (textual-input-port who obj)
  (and (textual-port? obj) (input-port? obj))
  (assertion-violation who "expected textual input port as argument" obj))


(define eval-depth 0)

(define waiter-prompt-string
  (make-parameter "vicare"
    (lambda (x)
      (if (string? x)
	  x
	(assertion-violation 'waiter-prompt-string "not a string" x)))))

(define cafe-input-port
  (make-parameter (console-input-port)
    (lambda (x)
      (define who 'cafe-input-port)
      (with-arguments-validation (who)
	  ((textual-input-port x))
	x))))

(define NESTED-PROMPT-CHAR #\>)

(define (display-prompt)
  (define port (console-output-port))
  (display (waiter-prompt-string) port)
  (let next-level ((i    0)
		   (port port))
    (if (= i eval-depth)
	(begin
	  (display " " port)
	  (flush-output-port port))
      (begin
	(display NESTED-PROMPT-CHAR port)
	(next-level (+ 1 i) port)))))

(define (print-ex ex)
  (flush-output-port (console-output-port))
  (display "Unhandled exception\n" (console-error-port))
  (print-condition ex (console-error-port)))

(define (reset k)
  (reset-input-port! (cafe-input-port))
  (k))


(define (default-cafe-eval x)
  (eval x (interaction-environment)))

(define new-cafe
  ;;Create a REPL, possibly nested.
  ;;
  (case-lambda
   (()
    (do-new-cafe default-cafe-eval))
   ((proc)
    (if (procedure? proc)
	(do-new-cafe proc)
      (assertion-violation 'new-cafe "not a procedure" proc)))))

(define (do-new-cafe eval-proc)
  ;;Implement the  LOOP, handling  recovery from exceptions  and keeping
  ;;track of the  nesting level.  The READ, EVAL,  PRINT is delegated to
  ;;the function WAIT1.
  ;;
  (dynamic-wind
      (lambda ()
	(set! eval-depth (fxadd1 eval-depth)))
      (lambda ()
	(call/cc
	    (lambda (exit-k)
	      (let loop ()
		(call/cc
		    (lambda (loop-k)
		      (with-exception-handler
			  ;;If an exception occurs doing the READ, EVAL,
			  ;;PRINT: flush and reset the output port, then
			  ;;jump to the LOOP by invoking LOOP-K.
			  (lambda (ex)
			    (with-exception-handler
				;;If  an exception  occurs  flushing and
				;;resetting  the output port,  just jump
				;;to the LOOP by invoking LOOP-K.
				loop-k
			      (lambda ()
				(flush-output-port (console-output-port))
				(newline (console-output-port))
				(reset loop-k))))
			(lambda ()
			  (wait1 eval-proc loop-k exit-k)))))
		(loop)))))
      (lambda ()
	(set! eval-depth (fxsub1 eval-depth)))))

(define (wait1 eval-proc loop-k exit-repl-k)
  ;;Perform the READ, EVAL, PRINT.
  ;;
  ;;LOOP-K must be an escape procedure to be called to perform the LOOP.
  ;;
  ;;EXIT-REPL-K must  be an escape procedure  to be called  to exit this
  ;;REPL, for example when the  EOF object is read.
  ;;
  (display-prompt)
  (let ((x (with-exception-handler
	       (lambda (ex)
		 ;;If an error occurs  while READing, reset the port and
		 ;;LOOP.
		 (cond ((lexical-violation? ex)
			(print-ex ex)
			(reset loop-k))
		       ((interrupted-condition? ex)
			(flush-output-port (console-output-port))
			(newline (console-output-port))
			(reset loop-k))
		       (else
			(raise-continuable ex))))
	     ;;Perform the READ.
	     (lambda ()
	       (read (cafe-input-port))))))
    (if (eof-object? x)
	;;Exit the REPL.
	(begin
	  (newline (console-output-port))
	  (exit-repl-k (void)))
      (call-with-values
	  ;;Perform the EVAL.
	  (lambda ()
	    (with-exception-handler
		;;If  an error  occurs  recovering from  an error  while
		;;EVALuating, LOOP if needed.
		(lambda (ex)
		  (if (non-continuable-violation? ex)
		      (reset loop-k)
		    (raise-continuable ex)))
	      (lambda ()
		(with-exception-handler
		    ;;If an error occurs  doing the EVAL, reset the port
		    ;;and do the LOOP.
		    (lambda (ex)
		      (print-ex ex)
		      (when (serious-condition? ex)
			(reset loop-k)))
		  ;;Really perform the EVAL.
		  (lambda ()
		    (eval-proc x))))))
	;;Perform the PRINT.   Do nothing if all the  values returned by
	;;EVAL are void.
	(lambda v*
	  (unless (andmap (lambda (v)
			    (eq? v (void)))
			  v*)
	    (with-exception-handler
		(lambda (ex)
		  ;;If an error occurs while PRINTing, LOOP if needed.
		  (if (interrupted-condition? ex)
		      (begin
			(flush-output-port (console-output-port))
			(newline (console-output-port))
			(reset loop-k))
		    (raise-continuable ex)))
	      (lambda ()
		(let ((port (console-output-port)))
		  (do ((i 1 (+ 1 i))
		       (v* v* (cdr v*)))
		      ((null? v*))
		    ;;Print:
		    ;;
		    ;;  $i = <value>\n
		    ;;
		    (write-char #\$ port)
		    (parameterize ((printer-integer-radix 10))
		      (display i port))
		    (write-char #\space port)
		    (write-char #\= port)
		    (write-char #\space port)
		    (pretty-print (car v*) port)))))))))))


;;;; done

)

;;; end of file
