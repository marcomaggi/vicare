;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the expander
;;;Date: Sun Feb  7, 2016
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
(program (test-vicare-expander-lambdas)
  (options strict-r6rs)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expander syntaxes extensions for LAMBDA, DEFINE and similar\n")


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


(parametrise ((check-test-name	'case-define))

  (check
      (let ()
	(case-define doit
	  (()
	   123))
	(doit))
    => 123)

  (check
      (let ()
	(case-define doit
	  ((a)
	   (list a))
	  ((a b)
	   (vector a b)))
	(list (doit 1) (doit 1 2)))
    => '((1) #(1 2)))

  #t)


(parametrise ((check-test-name	'named-lambda-core))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define doit
	  (named-lambda the-name ()
	    123))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (named-lambda the-name (a)
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (named-lambda the-name (a b c)
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (named-lambda the-name rest
	    (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (named-lambda the-name (a . rest)
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (named-lambda the-name (a b . rest)
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (named-lambda the-name (a b c . rest)
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define doit
	  (named-lambda the-name ()
	    __who__))
	(doit))
    => 'the-name)

  #t)


(parametrise ((check-test-name	'named-case-lambda-core))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define doit
	  (named-case-lambda the-name
	    (()
	     123)))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (named-case-lambda the-name
	    ((a)
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (named-case-lambda the-name
	    ((a b c)
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (named-case-lambda the-name
	    (rest
	     (vector 123 rest))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda the-name
	    ((a . rest)
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda the-name
	    ((a b . rest)
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda the-name
	    ((a b c . rest)
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define doit
	  (named-case-lambda the-name
	    (()
	     __who__)))
	(doit))
    => 'the-name)

  #t)


(parametrise ((check-test-name	'test-define-integrable))

  (define-syntax define-integrable
    ;;Posted  by "leppie"  on the  Ikarus mailing  list; subject  "Macro
    ;;Challenge of Last Year [Difficulty: *****]", 20 Oct 2009.
    ;;
    (lambda (x)
      (define (make-residual-name name)
	(datum->syntax name
		       (string->symbol
			(string-append "residual-"
				       (symbol->string (syntax->datum name))))))
      (syntax-case x (lambda)
        ((_ (?name . ?formals) ?form1 ?form2 ...)
	 (identifier? #'?name)
	 #'(define-integrable ?name (lambda ?formals ?form1 ?form2 ...)))

        ((_ ?name (lambda ?formals ?form1 ?form2 ...))
         (identifier? #'?name)
         (with-syntax ((XNAME (make-residual-name #'?name)))
           #'(begin
               (define-fluid-syntax ?name
                 (lambda (x)
                   (syntax-case x ()
                     (_
		      (identifier? x)
		      #'XNAME)

                     ((_ arg (... ...))
                      #'((fluid-let-syntax
			     ((?name (identifier-syntax XNAME)))
                           (lambda ?formals ?form1 ?form2 ...))
                         arg (... ...))))))

               (define XNAME
                 (fluid-let-syntax ((?name (identifier-syntax XNAME)))
                   (lambda ?formals ?form1 ?form2 ...))))))
	)))

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define-integrable (fact n)
	  (let ((residual-fact (lambda (x)
				 (error 'fact "captured residual-fact"))))
	    (if (< n 2)
		1
	      (* n (fact (- n 1))))))
	(fact 5))
    => 120)

  (check
      (let ()
	(define-integrable (f x) (+ x 1))
	(eq? f f))
    => #t)

  (check
      (let ()
	(define-integrable (even? n) (or (zero? n) (odd? (- n 1))))
	(define-integrable (odd? n) (not (even? n)))
	(even? 5))
    => #f)

  #t)


(parametrise ((check-test-name	'define-integrable))

  (check
      (let ()
	(define-integrable (fact n)
	  (if (< n 2)
	      1
	    (* n (fact (- n 1)))))
	(fact 5))
    => 120)

  (check
      (let ()
	(define-integrable (f x) (+ x 1))
	(eq? f f))
    => #t)

  (check
      (let ()
	(define-integrable (even? n) (or (zero? n) (odd? (- n 1))))
	(define-integrable (odd? n) (not (even? n)))
	(even? 5))
    => #f)

  (check
      (let ()
	(define-integrable (incr x)
	  (+ x 1))
	(map incr '(10 20 30)))
    => '(11 21 31))

  #t)


(parametrise ((check-test-name	'define-values))

  (check
      (let ()
	(define-values (a)
	  1)
	a)
    => 1)

  (check
      (with-result
       (let ()
	 (define-values (a)
	   (add-result 2)
	   1)
	 a))
    => '(1 (2)))

  (check
      (let ()
  	(define-values (a b c)
  	  #t
  	  (values 1 2 3))
  	(list a b c))
    => '(1 2 3))

  (check
      (let ((a 2))
  	(define-values (a)
  	  (values 1))
  	a)
    => 1)

  (check
      (let ()
  	(define-values (a b c . rest)
  	  #t
  	  (values 1 2 3 4 5 6))
  	(list a b c rest))
    => '(1 2 3 (4 5 6)))

  (check
      (let ()
  	(define-values args
  	  #t
  	  (values 1 2 3))
  	args)
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'define-constant-values))

  (check
      (let ()
	(define-constant-values (a b c)
	  #t
	  (values 1 2 3))
	(list a b c))
    => '(1 2 3))

  (check
      (let ()
	(define-constant-values (a)
	  #t
	  (values 1))
	a)
    => 1)

  (check
      (let ()
	(define-constant-values (a)
	  #t
	  1)
	a)
    => 1)

  (check
      (let ()
  	(define-constant-values (a b c . rest)
  	  #t
  	  (values 1 2 3 4 5 6))
  	(list a b c rest))
    => '(1 2 3 (4 5 6)))

  (check
      (let ()
  	(define-constant-values args
  	  #t
  	  (values 1 2 3))
  	args)
    => '(1 2 3))
  #t)


(parametrise ((check-test-name	'define-constant-values))

  (define-syntax-rule (check-syntax-error ?form)
    (check
	(guard (E ((syntax-violation? E)
		   #t)
		  (else E))
	  (eval '?form
		(environment '(vicare))))
      => #t))

;;; --------------------------------------------------------------------

  (check
      (let-constants ()
	1)
    => 1)

  (check
      (let-constants ((a 1))
	a)
    => 1)

  (check
      (let-constants ((a 1)
		      (b 2)
		      (c 3))
	(values a b c))
    => 1 2 3)

  (check-syntax-error
   (let-constants ((a 1))
     (set! a 2)))

;;; --------------------------------------------------------------------

  (check
      (let*-constants ()
	1)
    => 1)

  (check
      (let*-constants ((a 1))
	a)
    => 1)

  (check
      (let*-constants ((a 1)
		       (b 2)
		       (c 3))
	(values a b c))
    => 1 2 3)

  (check
      (let*-constants ((a 1)
		       (b (cons a 2))
		       (c 3))
	(values a b c))
    => 1 '(1 . 2) 3)

  (check-syntax-error
   (let*-constants ((a 1))
     (set! a 2)))

;;; --------------------------------------------------------------------

  (check
      (letrec-constants ()
	1)
    => 1)

  (check
      (letrec-constants ((a 1))
	a)
    => 1)

  (check
      (letrec-constants ((a 1)
			 (b 2)
			 (c 3))
	(values a b c))
    => 1 2 3)

  (check
      (letrec-constants ((a 1)
			 (b (lambda () (cons a 2)))
			 (c 3))
	(values a (b) c))
    => 1 '(1 . 2) 3)

  (check
      (letrec-constants ((a (lambda () (b)))
			 (b (lambda () 1))
			 (c 3))
	(values (a) (b) c))
    => 1 1 3)

  (check-syntax-error
   (letrec-constants ((a 1))
     (set! a 2)))

;;; --------------------------------------------------------------------

  (check
      (letrec*-constants ()
	1)
    => 1)

  (check
      (letrec*-constants ((a 1))
	a)
    => 1)

  (check
      (letrec*-constants ((a 1)
			  (b 2)
			  (c 3))
	(values a b c))
    => 1 2 3)

  (check
      (letrec*-constants ((a 1)
			  (b (lambda () (cons a 2)))
			  (c 3))
	(values a (b) c))
    => 1 '(1 . 2) 3)

  (check-syntax-error
   (letrec*-constants ((a 1))
     (set! a 2)))

  #t)


(parametrise ((check-test-name	'define-inline))

  (check
      (internal-body
  	(define-inline (ciao a b)
  	  (+ a b))
  	(ciao 1 2))
    => 3)

  (check
      (internal-body
  	(define-inline (ciao)
  	  (+ 1 2))
  	(ciao))
    => 3)

  (check
      (internal-body
  	(define-inline (ciao . rest)
  	  (apply + rest))
  	(ciao 1 2))
    => 3)

  (check
      (internal-body
	(define-inline (ciao a . rest)
	  (apply + a rest))
	(ciao 1 2))
    => 3)

  #t)


(parametrise ((check-test-name	'define-constant))

  (check
      (let ()
	(define-constant a 123)
	a)
    => 123)

  #t)


(parametrise ((check-test-name	'define-inline-constant))

  (check
      (let ()
	(define-inline-constant a (+ 1 2 3))
	a)
    => 6)

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'catch-syntax-violation			'scheme-indent-function 1)
;; eval: (put 'case-identifiers				'scheme-indent-function 1)
;; End:
