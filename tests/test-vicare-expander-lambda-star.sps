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
(program (test-vicare-expander-lambda-star)
  (options strict-r6rs)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expander syntaxes LAMBDA*, DEFINE* and similar\n")


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


(parametrise ((check-test-name	'lambda-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define doit
	  (lambda* ()
	    123))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (lambda* (a)
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (lambda* (a b c)
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (lambda* rest
	    (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (lambda* (a . rest)
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (lambda* (a b . rest)
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (lambda* (a b c . rest)
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?})
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (lambda* {args fixnum?}
	    (vector 123 args)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?} . {rest fixnum?})
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} . {rest fixnum?})
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(_ (#\5)))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(define doit
	  (lambda* ({_ fixnum?})
	    123))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?})
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} . {rest fixnum?})
	    (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?} . {rest fixnum?})
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?} {b fixnum?} . {rest fixnum?})
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(_ (#\5)))

  (check-argument-violation
      (let ()
	(define doit
	  (lambda* ({_ list?} {a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '(_ 1 list? #(123 4 5 6)))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(define doit
	  (lambda* ({_ fixnum? string? char?})
	    (values 1 "2" #\3)))
	(doit))
    => 1 "2" #\3)

  (check-for-procedure-signature-return-value-violation
      (let ()
	(define doit
	  (lambda* ({_ fixnum? string? char?})
	    (values 1 'a #\3)))
	(doit))
    => '(_ 2 string? a))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define doit
	  (lambda* ()
	    __who__))
	(doit))
    => '_)

  (check
      (let ()
	(define doit
	  (lambda* ({_ symbol?})
	    __who__))
	(doit))
    => '_)

;;; --------------------------------------------------------------------
;;; compound predicates

;;; argument

  (check
      (internal-body
	(define doit
	  (lambda* ({a (or integer? string?)})
	    a))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-for-procedure-signature-argument-violation
      (internal-body
	(define doit
	  (lambda* ({a (or integer? string?)})
	    a))
	(doit 1.2))
    => '(_ 1 (or integer? string?) 1.2))

;;; rest argument

  (check
      (internal-body
	(define doit
	  (lambda* (a {b (or integer? string?)})
	    b))
	(values (doit 1 123) (doit 1 "B")))
    => 123 "B")

  (check-for-procedure-signature-argument-violation
      (internal-body
	(define doit
	  (lambda* (a {b (or integer? string?)})
	    b))
	(doit 1 1.2))
    => '(_ 2 (or integer? string?) 1.2))

;;; single return value

  (check
      (internal-body
	(define doit
	  (lambda* ({_ (or integer? string?)} a)
	    a))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-for-procedure-signature-return-value-violation
      (internal-body
	(define doit
	  (lambda* ({_ (or integer? string?)} a)
	    a))
	(doit 1.2))
    => '(_ 1 (or integer? string?) 1.2))

;;; multiple return values

  (check
      (internal-body
	(define doit
	  (lambda* ({_ (or integer? string?) (or symbol? string?)} a b)
	    (values a b)))
	(let-values (((a b) (doit 123 'ciao))
		     ((c d) (doit "B" "C")))
	  (values a b c d)))
    => 123 'ciao "B" "C")

  (check-for-procedure-signature-return-value-violation
      (internal-body
	(define doit
	  (lambda* ({_ (or integer? string?) (or symbol? string?)} a b)
	    (values a b)))
	(doit 1.2 'ciao))
    => '(_ 1 (or integer? string?) 1.2))

  (check-for-procedure-signature-return-value-violation
      (internal-body
	(define doit
	  (lambda* ({_ (or integer? string?) (or symbol? string?)} a b)
	    (values a b)))
	(doit 12 4))
    => '(_ 2 (or symbol? string?) 4))

  #t)


(parametrise ((check-test-name	'named-lambda-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ()
	    123))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (named-lambda* the-name (a)
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name (a b c)
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name rest
	    (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name (a . rest)
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name (a b . rest)
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name (a b c . rest)
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({a fixnum?})
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name {args fixnum?}
	    (vector 123 args)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({a fixnum?} . {rest fixnum?})
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({a fixnum?} {b fixnum?} . {rest fixnum?})
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define doit
	  (named-lambda* the-name ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(the-name (#\5)))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ fixnum?})
	    123))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ vector?} {a fixnum?})
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ vector?} . {rest fixnum?})
	    (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ vector?} {a fixnum?} . {rest fixnum?})
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ vector?} {a fixnum?} {b fixnum?} . {rest fixnum?})
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define doit
	  (named-lambda* the-name ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(the-name (#\5)))

  (check-for-procedure-signature-return-value-violation
      (let ()
	(define doit
	  (named-lambda* the-name ({_ list?} {a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '(the-name 1 list? #(123 4 5 6)))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ fixnum? string? char?})
	    (values 1 "2" #\3)))
	(doit))
    => 1 "2" #\3)

  (check-for-procedure-signature-return-value-violation
      (let ()
	(define doit
	  (named-lambda* the-name ({_ fixnum? string? char?})
	    (values 1 'a #\3)))
	(doit))
    => '(the-name 2 string? a))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ()
	    __who__))
	(doit))
    => 'the-name)

  (check
      (let ()
	(define doit
	  (named-lambda* the-name ({_ symbol?})
	    __who__))
	(doit))
    => 'the-name)

;;; --------------------------------------------------------------------
;;; compound predicates

;;; argument

  (check
      (internal-body
	(define doit
	  (named-lambda* the-name ({a (or integer? string?)})
	    a))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-for-procedure-signature-argument-violation
      (internal-body
	(define doit
	  (named-lambda* the-name ({a (or integer? string?)})
	    a))
	(doit 1.2))
    => '(the-name 1 (or integer? string?) 1.2))

;;; rest argument

  (check
      (internal-body
	(define doit
	  (named-lambda* the-name (a {b (or integer? string?)})
	    b))
	(values (doit 1 123) (doit 1 "B")))
    => 123 "B")

  (check-for-procedure-signature-argument-violation
      (internal-body
	(define doit
	  (named-lambda* the-name (a {b (or integer? string?)})
	    b))
	(doit 1 1.2))
    => '(the-name 2 (or integer? string?) 1.2))

;;; single return value

  (check
      (internal-body
	(define doit
	  (named-lambda* the-name ({_ (or integer? string?)} a)
	    a))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-for-procedure-signature-return-value-violation
      (internal-body
	(define doit
	  (named-lambda* the-name ({_ (or integer? string?)} a)
	    a))
	(doit 1.2))
    => '(the-name 1 (or integer? string?) 1.2))

;;; multiple return values

  (check
      (internal-body
	(define doit
	  (named-lambda* the-name ({_ (or integer? string?) (or symbol? string?)} a b)
	    (values a b)))
	(let-values (((a b) (doit 123 'ciao))
		     ((c d) (doit "B" "C")))
	  (values a b c d)))
    => 123 'ciao "B" "C")

  (check-for-procedure-signature-return-value-violation
      (internal-body
	(define doit
	  (named-lambda* the-name ({_ (or integer? string?) (or symbol? string?)} a b)
	    (values a b)))
	(doit 1.2 'ciao))
    => '(the-name 1 (or integer? string?) 1.2))

  (check-for-procedure-signature-return-value-violation
      (internal-body
	(define doit
	  (named-lambda* the-name ({_ (or integer? string?) (or symbol? string?)} a b)
	    (values a b)))
	(doit 12 4))
    => '(the-name 2 (or symbol? string?) 4))

  #t)


(parametrise ((check-test-name	'case-lambda-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (()
	     123)))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a)
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a b c)
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (rest
	     (vector 123 rest))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a . rest)
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a b . rest)
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a b c . rest)
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?})
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ({args fixnum?}
	     (vector 123 args))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} . {rest fixnum?})
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} . {rest fixnum?})
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-argument-violation
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 #\5 6))
    => '(_ 2 fixnum? #\5))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ fixnum?})
	     123)))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?})
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} . {rest fixnum?})
	     (vector 123 rest))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?} . {rest fixnum?})
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?} {b fixnum?} . {rest fixnum?})
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-argument-violation
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 #\5 6))
    => '(_ 2 fixnum? #\5))

  (check-argument-violation
      (let ()
	(define doit
	  (case-lambda*
	    (({_ list?} {a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '(_ 1 list? #(123 4 5 6)))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ fixnum? string? char?})
	     (values 1 "2" #\3))))
	(doit))
    => 1 "2" #\3)

  (check-argument-violation
      (let ()
	(define doit
	  (case-lambda*
	    (({_ fixnum? string? char?})
	     (values 1 'a #\3))))
	(doit))
    => '(_ 2 string? a))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (()
	     __who__)))
	(doit))
    => '_)

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ symbol?})
	     __who__)))
	(doit))
    => '_)

;;; --------------------------------------------------------------------
;;; compound predicates

;;; argument

  (check
      (internal-body
	(define doit
	  (case-lambda*
	    (({a (or integer? string?)})
	     a)))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define doit
	  (case-lambda*
	    (({a (or integer? string?)})
	     a)))
	(doit 1.2))
    => '(_ 1 (or integer? string?) 1.2))

;;; rest argument

  (check
      (internal-body
	(define doit
	  (case-lambda*
	    ((a {b (or integer? string?)})
	     b)))
	(values (doit 1 123) (doit 1 "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define doit
	  (case-lambda*
	    ((a {b (or integer? string?)})
	     b)))
	(doit 1 1.2))
    => '(_ 2 (or integer? string?) 1.2))

;;; single return value

  (check
      (internal-body
	(define doit
	  (case-lambda*
	    (({_ (or integer? string?)} a)
	     a)))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define doit
	  (case-lambda*
	    (({_ (or integer? string?)} a)
	     a)))
	(doit 1.2))
    => '(_ 1 (or integer? string?) 1.2))

;;; multiple return values

  (check
      (internal-body
	(define doit
	  (case-lambda*
	    (({_ (or integer? string?) (or symbol? string?)} a b)
	     (values a b))))
	(let-values (((a b) (doit 123 'ciao))
		     ((c d) (doit "B" "C")))
	  (values a b c d)))
    => 123 'ciao "B" "C")

  (check-argument-violation
      (internal-body
	(define doit
	  (case-lambda*
	    (({_ (or integer? string?) (or symbol? string?)} a b)
	     (values a b))))
	(doit 1.2 'ciao))
    => '(_ 1 (or integer? string?) 1.2))

  (check-argument-violation
      (internal-body
	(define doit
	  (case-lambda*
	    (({_ (or integer? string?) (or symbol? string?)} a b)
	     (values a b))))
	(doit 12 4))
    => '(_ 2 (or symbol? string?) 4))

  #t)


(parametrise ((check-test-name	'named-case-lambda-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (()
	     123)))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    ((a)
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    ((a b c)
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (rest
	     (vector 123 rest))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    ((a . rest)
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    ((a b . rest)
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    ((a b c . rest)
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({a fixnum?})
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    ({args fixnum?}
	     (vector 123 args))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({a fixnum?} . {rest fixnum?})
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({a fixnum?} {b fixnum?} . {rest fixnum?})
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-argument-violation
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 #\5 6))
    => '(the-name 2 fixnum? #\5))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ fixnum?})
	     123)))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ vector?} {a fixnum?})
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ vector?} . {rest fixnum?})
	     (vector 123 rest))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ vector?} {a fixnum?} . {rest fixnum?})
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ vector?} {a fixnum?} {b fixnum?} . {rest fixnum?})
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-argument-violation
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 #\5 6))
    => '(the-name 2 fixnum? #\5))

  (check-argument-violation
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ list?} {a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '(the-name 1 list? #(123 4 5 6)))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ fixnum? string? char?})
	     (values 1 "2" #\3))))
	(doit))
    => 1 "2" #\3)

  (check-argument-violation
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ fixnum? string? char?})
	     (values 1 'a #\3))))
	(doit))
    => '(the-name 2 string? a))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (()
	     __who__)))
	(doit))
    => 'the-name)

  (check
      (let ()
	(define doit
	  (named-case-lambda* the-name
	    (({_ symbol?})
	     __who__)))
	(doit))
    => 'the-name)

;;; --------------------------------------------------------------------
;;; compound predicates

;;; argument

  (check
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    (({a (or integer? string?)})
	     a)))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    (({a (or integer? string?)})
	     a)))
	(doit 1.2))
    => '(the-name 1 (or integer? string?) 1.2))

;;; rest argument

  (check
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    ((a {b (or integer? string?)})
	     b)))
	(values (doit 1 123) (doit 1 "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    ((a {b (or integer? string?)})
	     b)))
	(doit 1 1.2))
    => '(the-name 2 (or integer? string?) 1.2))

;;; single return value

  (check
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    (({_ (or integer? string?)} a)
	     a)))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    (({_ (or integer? string?)} a)
	     a)))
	(doit 1.2))
    => '(the-name 1 (or integer? string?) 1.2))

;;; multiple return values

  (check
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    (({_ (or integer? string?) (or symbol? string?)} a b)
	     (values a b))))
	(let-values (((a b) (doit 123 'ciao))
		     ((c d) (doit "B" "C")))
	  (values a b c d)))
    => 123 'ciao "B" "C")

  (check-argument-violation
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    (({_ (or integer? string?) (or symbol? string?)} a b)
	     (values a b))))
	(doit 1.2 'ciao))
    => '(the-name 1 (or integer? string?) 1.2))

  (check-argument-violation
      (internal-body
	(define doit
	  (named-case-lambda* the-name
	    (({_ (or integer? string?) (or symbol? string?)} a b)
	     (values a b))))
	(doit 12 4))
    => '(the-name 2 (or symbol? string?) 4))

  #t)


(parametrise ((check-test-name	'define-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define* (doit)
	  123)
	(doit))
    => 123)

  (check
      (let ()
	(define* (doit a)
	  (vector 123 a))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define* (doit a b c)
	  (vector 123 a b c))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define* (doit . rest)
	  (vector 123 rest))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define* (doit a . rest)
	  (vector 123 a rest))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define* (doit a b . rest)
	  (vector 123 a b rest))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define* (doit a b c . rest)
	  (vector 123 a b c rest))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(define* (doit {a fixnum?})
	  (vector 123 a))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define* (doit {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define* (doit . {rest fixnum?})
	  (vector 123 rest))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define* (doit {a fixnum?} . {rest fixnum?})
	  (vector 123 a rest))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define* (doit {a fixnum?} {b fixnum?} . {rest fixnum?})
	  (vector 123 a b rest))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define* (doit {a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	  (vector 123 a b c rest))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-argument-violation
      (let ()
	(define* (doit {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 #\5 6))
    => '(doit 2 fixnum? #\5))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(define* ({doit fixnum?})
	  123)
	(doit))
    => 123)

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?})
	  (vector 123 a))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define* ({doit vector?} . {rest fixnum?})
	  (vector 123 rest))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?} . {rest fixnum?})
	  (vector 123 a rest))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?} {b fixnum?} . {rest fixnum?})
	  (vector 123 a b rest))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	  (vector 123 a b c rest))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-argument-violation
      (let ()
	(define* ({doit vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 #\5 6))
    => '(doit 2 fixnum? #\5))

  (check-argument-violation

      (let ()
	(define* ({doit list?} {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 5 6))
    => '(doit 1 list? #(123 4 5 6)))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(define* ({doit fixnum? string? char?})
	  (values 1 "2" #\3))
	(doit))
    => 1 "2" #\3)

  (check-argument-violation

      (let ()
	(define* ({doit fixnum? string? char?})
	  (values 1 'a #\3))
	(doit))
    => '(doit 2 string? a))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define* (doit)
	  __who__)
	(doit))
    => 'doit)

  (check
      (let ()
	(define* ({doit symbol?})
	  __who__)
	(doit))
    => 'doit)

;;; --------------------------------------------------------------------
;;; compound predicates

;;; argument

  (check
      (internal-body
	(define* (doit {a (or integer? string?)})
	  a)
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define* (doit {a (or integer? string?)})
	  a)
	(doit 1.2))
    => '(doit 1 (or integer? string?) 1.2))

;;; rest argument

  (check
      (internal-body
	(define* (doit a {b (or integer? string?)})
	  b)
	(values (doit 1 123) (doit 1 "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define* (doit a {b (or integer? string?)})
	  b)
	(doit 1 1.2))
    => '(doit 2 (or integer? string?) 1.2))

;;; single return value

  (check
      (internal-body
	(define* ({doit (or integer? string?)} a)
	  a)
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(define* ({doit (or integer? string?)} a)
	  a)
	(doit 1.2))
    => '(doit 1 (or integer? string?) 1.2))

;;; multiple return values

  (check
      (internal-body
	(define* ({doit (or integer? string?) (or symbol? string?)} a b)
	  (values a b))
	(let-values (((a b) (doit 123 'ciao))
		     ((c d) (doit "B" "C")))
	  (values a b c d)))
    => 123 'ciao "B" "C")

  (check-argument-violation
      (internal-body
	(define* ({doit (or integer? string?) (or symbol? string?)} a b)
	  (values a b))
	(doit 1.2 'ciao))
    => '(doit 1 (or integer? string?) 1.2))

  (check-argument-violation
      (internal-body
	(define* ({doit (or integer? string?) (or symbol? string?)} a b)
	  (values a b))
	(doit 12 4))
    => '(doit 2 (or symbol? string?) 4))

  #t)


(parametrise ((check-test-name	'case-define-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(case-define* doit
	  (()
	   123))
	(doit))
    => 123)

  (check
      (let ()
	(case-define* doit
	  ((a)
	   (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(case-define* doit
	  ((a b c)
	   (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(case-define* doit
	  (rest
	   (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(case-define* doit
	  ((a . rest)
	   (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(case-define* doit
	  ((a b . rest)
	   (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(case-define* doit
	  ((a b c . rest)
	   (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check
      (let ()
	(case-define* doit
	  (()		0)
	  ((a)		1)
	  ((a b c)	3))
	(list (doit)
	      (doit 1)
	      (doit 1 2 3)))
    => '(0 1 3))

  (check
      (let ()
	(case-define* doit
	  (()		0)
	  ((a)		(list a))
	  ((a b c)	(list a b c)))
	(vector (doit)
		(doit 1)
		(doit 1 2 3)))
    => '#(0 (1) (1 2 3)))

  (check
      (let ()
	(case-define* doit
	  (()			0)
	  ((a)			(list a))
	  ((a b c)		(list a b c))
	  ((a b c . rest)	(cons* a b c rest)))
	(vector (doit)
		(doit 1)
		(doit 1 2 3)
		(doit 1 2 3 4 5)))
    => '#(0 (1) (1 2 3) (1 2 3 4 5)))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?})
	   (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(case-define* doit
	  ({rest fixnum?}
	   (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?} . {rest fixnum?})
	   (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} . {rest fixnum?})
	   (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	   (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check
      (let ()
	(case-define* doit
	  (()
	   0)
	  (({a fixnum?})
	   (list a))
	  (({a fixnum?} {b fixnum?} {c fixnum?})
	   (list a b c))
	  (({a fixnum?} {b fixnum?} {c fixnum?} . rest)
	   (cons* a b c rest)))
	(vector (doit)
		(doit 1)
		(doit 1 2 3)
		(doit 1 2 3 4 5)))
    => '#(0 (1) (1 2 3) (1 2 3 4 5)))

  (check-argument-violation
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(doit 2 fixnum? #\5))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(case-define* doit
	  (({_ fixnum?})
	   123))
	(doit))
    => 123)

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?})
	   (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} . {rest fixnum?})
	   (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?} . {rest fixnum?})
	   (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?} {b fixnum?} . {rest fixnum?})
	   (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest fixnum?})
	   (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-argument-violation
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(doit 2 fixnum? #\5))

  (check-argument-violation
      (let ()
	(case-define* doit
	  (({_ list?} {a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 5 6))
    => '(doit 1 list? #(123 4 5 6)))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(case-define* doit
	  (({_ fixnum? string? char?})
	   (values 1 "2" #\3)))
	(doit))
    => 1 "2" #\3)

  (check-argument-violation
      (let ()
	(case-define* doit
	  (({_ fixnum? string? char?})
	   (values 1 'a #\3)))
	(doit))
    => '(doit 2 string? a))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(case-define* doit
	  (()
	   __who__))
	(doit))
    => 'doit)

  (check
      (let ()
	(case-define* doit
	  (({_ symbol?})
	   __who__))
	(doit))
    => 'doit)

;;; --------------------------------------------------------------------
;;; compound predicates

;;; argument

  (check
      (internal-body
	(case-define* doit
	  (({a (or integer? string?)})
	   a))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(case-define* doit
	  (({a (or integer? string?)})
	   a))
	(doit 1.2))
    => '(doit 1 (or integer? string?) 1.2))

;;; rest argument

  (check
      (internal-body
	(case-define* doit
	  ((a {b (or integer? string?)})
	   b))
	(values (doit 1 123) (doit 1 "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(case-define* doit
	  ((a {b (or integer? string?)})
	   b))
	(doit 1 1.2))
    => '(doit 2 (or integer? string?) 1.2))

;;; single return value

  (check
      (internal-body
	(case-define* doit
	  (({_ (or integer? string?)} a)
	   a))
	(values (doit 123) (doit "B")))
    => 123 "B")

  (check-argument-violation
      (internal-body
	(case-define* doit
	  (({_ (or integer? string?)} a)
	   a))
	(doit 1.2))
    => '(doit 1 (or integer? string?) 1.2))

;;; multiple return values

  (check
      (internal-body
	(case-define* doit
	  (({_ (or integer? string?) (or symbol? string?)} a b)
	   (values a b)))
	(let-values (((a b) (doit 123 'ciao))
		     ((c d) (doit "B" "C")))
	  (values a b c d)))
    => 123 'ciao "B" "C")

  (check-argument-violation
      (internal-body
	(case-define* doit
	  (({_ (or integer? string?) (or symbol? string?)} a b)
	   (values a b)))
	(doit 1.2 'ciao))
    => '(doit 1 (or integer? string?) 1.2))

  (check-argument-violation
      (internal-body
	(case-define* doit
	  (({_ (or integer? string?) (or symbol? string?)} a b)
	   (values a b)))
	(doit 12 4))
    => '(doit 2 (or symbol? string?) 4))

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
