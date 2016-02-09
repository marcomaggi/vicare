;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the expander
;;;Date: Tue Sep 25, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (test-vicare-expander-dynamic-environment)
  (import (vicare)
    (vicare language-extensions callables)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expander syntaxes for dynamic environment\n")


;;;; helpers

(define (%eval sexp)
  (eval sexp (environment '(vicare))))

(define-syntax check-argument-violation
  (syntax-rules (=>)
    ((_ ?body => ?result)
     (check
	 (guard (E ((procedure-signature-argument-violation? E)
		    #;(print-condition E)
		    (list (condition-who E)
			  (procedure-signature-argument-violation.one-based-argument-index E)
			  (procedure-signature-argument-violation.failed-expression E)
			  (procedure-signature-argument-violation.offending-value E)))
		   ((procedure-signature-return-value-violation? E)
		    #;(print-condition E)
		    (list (condition-who E)
			  (procedure-signature-return-value-violation.one-based-return-value-index E)
			  (procedure-signature-return-value-violation.failed-expression E)
			  (procedure-signature-return-value-violation.offending-value E)))
		   ((procedure-arguments-consistency-violation? E)
		    #;(print-condition E)
		    (condition-irritants E))
		   ((procedure-argument-violation? E)
		    (when #f
		      (debug-print (condition-message E)))
		    (let ((D (cdr (condition-irritants E))))
		      (if (pair? D)
			  (car D)
			(condition-irritants E))))
		   ((assertion-violation? E)
		    (condition-irritants E))
		   (else
		    (print-condition E)
		    E))
	   ?body)
       => ?result))))


(parametrise ((check-test-name	'blocking-exceptions))

  (check
      (with-blocked-exceptions
	  (lambda (E)
	    (values E 1 2 3))
	(lambda ()
	  (raise 99)))
    => 99 1 2 3)

  (check	;exceptions fromt he handler are not blocked
      (guard (E (else
		 E))
	(with-blocked-exceptions
	    (lambda (E)
	      (raise (list E 1 2 3)))
	  (lambda ()
	    (raise 99))))
    => '(99 1 2 3))

  #t)


(parametrise ((check-test-name	'current-dynamic-environment))

  (define parm
    (make-parameter #f))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(parametrise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parametrise ((parm 'inner))
			    (with-current-dynamic-environment
				values
			      (lambda ()
				(set! counter (+ 1 counter))
				(add-result (list 'inside-thunk (parm))))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner)
	    calling-thunk-2 (inside-thunk inner))))

  (check	;raising exception
      (with-result
	(parametrise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parametrise ((parm 'inner))
			    (with-current-dynamic-environment
				values
			      (lambda ()
				(set! counter (+ 1 counter))
				(add-result (list 'inside-thunk (parm)))
				(add-result 'raise-exception)
				(raise 123))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner) raise-exception
	    calling-thunk-2 (inside-thunk inner) raise-exception)))

  #t)


(parametrise ((check-test-name	'try))

  (let ()	;with else clause
    (define-condition-type &this
	&error
      make-this-condition
      condition-this?
      (a condition-this.a)
      (b condition-this.b)
      (c condition-this.c))

    (define (doit thunk)
      (try
	  (thunk)
	(catch E
	  ((&this)
	   (list (condition-this.a E)
		 (condition-this.b E)
		 (condition-this.c E)))
	  ((&message)
	   (condition-message E))
	  (else E))))

    (check
	(doit (lambda ()
		(raise (make-this-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 123)

    (check
	(try
	    (raise 123)
	  (catch E
	    ((&this)
	     (list (condition-this.a E)
		   (condition-this.b E)
		   (condition-this.c E)))
	    ((&message)
	     (condition-message E))
	    (else E)))
      => 123)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;with else clause
    (define-condition-type &that
	&error
      make-that-condition
      condition-that?
      (a condition-that.a)
      (b condition-that.b)
      (c condition-that.c))

    (define (doit thunk)
      (try
	  (thunk)
	(catch T
	  ((&that)
	   (list (condition-that.a T)
		 (condition-that.b T)
		 (condition-that.c T)))
	  ((&message)
	   (condition-message T))
	  (else T))))

    (check
	(doit (lambda ()
		(raise (make-that-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 123)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;without else clause
    (define-condition-type &those
	&error
      make-those-condition
      condition-those?
      (a condition-those.a)
      (b condition-those.b)
      (c condition-those.c))

    (define (doit thunk)
      (guard (E (else
		 (values 'reraised E)))
	(try
	    (thunk)
	  (catch E
	    ((&those)
	     (list (condition-those.a E)
		   (condition-those.b E)
		   (condition-those.c E)))
	    ((&message)
	     (condition-message E))))))

    (check
	(doit (lambda ()
		(raise (make-those-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 'reraised 123)

    #f)

;;; --------------------------------------------------------------------
;;; finally

  (check	;no exception
      (let ((a 1))
	(try
	    (set! a (+ a 10))
	  (catch E
	    ((&error)	E)
	    ((&warning)	E)
	    (else	E))
	  (finally
	   (set! a (+ a 100))))
	a)
    => 111)

  (check	;no exception
      (with-result
	(try
	    (add-result 'body)
	  (catch E
	    ((&error)	(add-result 'catch-error))
	    ((&warning)	(add-result 'catch-warning))
	    (else	(add-result 'catch-else)))
	  (finally
	   (add-result 'finally))))
    => '(body (body finally)))

  (check	;with exception
      (let ((a 1))
	(try
	    (raise (make-warning))
	  (catch E
	    ((&error)	E)
	    ((&warning)	(set! a (+ a 10)))
	    (else	E))
	  (finally
	   (set! a (+ a 100))))
	a)
    => 111)

  (check	;with exception
      (with-result
	(try
	    (begin
	      (add-result 'body)
	      (raise (make-warning)))
	  (catch E
	    ((&error)	(add-result 'catch-error))
	    ((&warning)	(add-result 'catch-warning))
	    (else	(add-result 'catch-else)))
	  (finally
	   (add-result 'finally))))
    => '(catch-warning (body catch-warning finally)))

  #t)


(parametrise ((check-test-name	'shift-and-reset))

  (check
      (+ 1 (reset 2))
    => 3)

  (check
      (reset (* 2 (shift K (K 3))))
    => 6)

  (check
      (+ 1 (reset (* 2 (shift K (K 3)))))
    => 7)

  (check
      (reset (* 2 (shift K (K (K 2)))))
    => 8)

  (check
      (reset (* 2 (shift K (K (K (K 2))))))
    => 16)

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'typ.set-identifier-tag-type-spec!	'scheme-indent-function 1)
;; eval: (put 'catch-syntax-violation			'scheme-indent-function 1)
;; eval: (put 'case-identifiers				'scheme-indent-function 1)
;; End:
