;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for tags helpers for expand
;;;Date: Wed May  2, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (prefix (nausicaa language oopp helpers) help.)
  (for (prefix (nausicaa language auxiliary-syntaxes) aux.)
       (meta -1))
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing tags helpers for expand\n")


;;;; helpers

(define synner
  (case-lambda
   ((message)
    (synner message #f))
   ((message subform)
    (syntax-violation '<the-tag> message #'test-clauses subform))))

(define (debug-pretty-print thing)
  (pretty-print thing (current-error-port))
  (flush-output-port (current-error-port)))

(define (debug-write . args)
  (for-each (lambda (thing)
	      (write thing (current-error-port))
	      (display #\space (current-error-port)))
    args)
  (newline (current-error-port))
  (flush-output-port (current-error-port)))

(define (debug-newline thing)
  (newline (current-error-port))
  (flush-output-port (current-error-port)))

(define-syntax catch-syntax-violation
  (syntax-rules ()
    ((_ ?verbose . ?body)
     (guard (E ((syntax-violation? E)
		(when ?verbose
		  (debug-write (condition-message E)
			       (syntax-violation-subform E)))
		(syntax-violation-subform E))
	       (else E))
       . ?body))))

(define (%parse-class-clauses clauses)
  (help.parse-class-clauses clauses #'<the-class> #'<top> #'lambda/tags synner))

(define (%parse-label-clauses clauses)
  (help.parse-label-clauses clauses #'<the-label> #'<top> #'lambda/tags synner))

(define (%filter-and-validate-mixins-clauses clauses)
  (help.filter-and-validate-mixins-clauses clauses synner))


(parametrise ((check-test-name	'class-parent))

  (check
      (let ((spec (%parse-class-clauses #'((aux.parent <alpha>)))))
	(and (help.<class-spec>? spec)
	     (free-identifier=? #'<alpha> (help.<parsed-spec>-parent-id spec))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.parent <alpha>)
				 (aux.parent <alpha>))))
    (=> syntax=?) #'(aux.parent <alpha>))

;;; --------------------------------------------------------------------
;;; errors: argument is not an identifier

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.parent 123))))
    (=> syntax=?) #'(aux.parent 123))

  #t)


(parametrise ((check-test-name	'class-opaque))

  (check
      (let ((spec (%parse-class-clauses #'((aux.opaque #t)))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-opaque? spec)))
    => #t)

  (check
      (let ((spec (%parse-class-clauses #'((aux.opaque #f)))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-opaque? spec)))
    => #f)

  (check
      (let ((spec (%parse-class-clauses '())))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-opaque? spec)))
    => #f)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.opaque #t)
				 (aux.opaque #t))))
    (=> syntax=?) #'(aux.opaque #t))

;;; --------------------------------------------------------------------
;;; errors: argument is not a boolean

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.opaque 123))))
    (=> syntax=?) #'(aux.opaque 123))

  #t)


(parametrise ((check-test-name	'class-sealed))

  (check
      (let ((spec (%parse-class-clauses #'((aux.sealed #t)))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-sealed? spec)))
    => #t)

  (check
      (let ((spec (%parse-class-clauses #'((aux.sealed #f)))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-sealed? spec)))
    => #f)

  (check
      (let ((spec (%parse-class-clauses '())))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-sealed? spec)))
    => #f)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.sealed #t)
				 (aux.sealed #t))))
    (=> syntax=?) #'(aux.sealed #t))

;;; --------------------------------------------------------------------
;;; errors: argument is not a boolean

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.sealed 123))))
    (=> syntax=?) #'(aux.sealed 123))

  #t)


(parametrise ((check-test-name	'class-nongenerative))

  (check
      (let ((spec (%parse-class-clauses #'((aux.nongenerative ciao)))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-nongenerative-uid spec)))
    (=> free-identifier=?) #'ciao)

  (check
      (let ((spec (%parse-class-clauses #'((aux.nongenerative)))))
	(and (help.<class-spec>? spec)
	     (identifier? (help.<parsed-spec>-nongenerative-uid spec))))
    => #t)

  (check
      (let ((spec (%parse-class-clauses '())))
	(and (help.<class-spec>? spec)
	     (identifier? (help.<parsed-spec>-nongenerative-uid spec))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.nongenerative ciao)
				 (aux.nongenerative hello))))
    (=> syntax=?) #'(aux.nongenerative hello))

;;; --------------------------------------------------------------------
;;; errors: argument is not a symbol

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.nongenerative 123))))
    (=> syntax=?) #'(aux.nongenerative 123))

  #t)


(parametrise ((check-test-name	'class-common-protocol))

  (check
      (let ((spec (%parse-class-clauses #'((aux.protocol (lambda (x) (lambda () (x))))))))
	(and (help.<class-spec>? spec)
	     (syntax=? #'(lambda (x) (lambda () (x)))
			  (help.<parsed-spec>-common-protocol spec))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.protocol <alpha>)
				 (aux.protocol <alpha>))))
    (=> syntax=?) #'(aux.protocol <alpha>))

  #t)


(parametrise ((check-test-name	'class-public-protocol))

  (check
      (let ((spec (%parse-class-clauses #'((aux.public-protocol (lambda (x) (lambda () (x))))))))
	(and (help.<class-spec>? spec)
	     (syntax=? #'(lambda (x) (lambda () (x)))
			  (help.<parsed-spec>-public-protocol spec))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.public-protocol <alpha>)
				 (aux.public-protocol <alpha>))))
    (=> syntax=?) #'(aux.public-protocol <alpha>))

  #t)


(parametrise ((check-test-name	'class-super-protocol))

  (check
      (let ((spec (%parse-class-clauses #'((aux.super-protocol (lambda (x) (lambda () (x))))))))
	(and (help.<class-spec>? spec)
	     (syntax=? #'(lambda (x) (lambda () (x)))
			  (help.<parsed-spec>-super-protocol spec))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.super-protocol <alpha>)
				 (aux.super-protocol <alpha>))))
    (=> syntax=?) #'(aux.super-protocol <alpha>))

  #t)


(parametrise ((check-test-name	'class-abstract))

  (check
      (let ((spec (%parse-class-clauses #'((aux.abstract)))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-abstract? spec)))
    => #t)

  (check
      (let ((spec (%parse-class-clauses #'())))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-abstract? spec)))
    => #f)

  (check
      (let ((spec (%parse-class-clauses #'((aux.abstract)
					   (aux.super-protocol (lambda (x) x))))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-abstract? spec)))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.abstract)
				 (aux.abstract))))
    (=> syntax=?) #'(aux.abstract))

;;; --------------------------------------------------------------------
;;; abstract and protocol clauses used together

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.abstract)
				 (aux.protocol (lambda (x) x)))))
    => #f)

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.abstract)
				 (aux.public-protocol (lambda (x) x)))))
    => #f)

  #t)


(parametrise ((check-test-name	'label-predicate))

  (check	;predicate identifier
      (let ((spec (%parse-label-clauses #'((aux.predicate list?)))))
	(and (help.<label-spec>? spec)
	     (list (free-identifier=? #'list? (help.<parsed-spec>-private-predicate-id spec))
		   (help.<parsed-spec>-definitions spec))))
    => '(#t ()))

  (check	;predicate expression
      (let ((spec (%parse-label-clauses #'((aux.predicate (lambda (x) (list? x)))))))
	(and (help.<label-spec>? spec)
	     (free-identifier=? #'<the-label>-private-predicate
				(help.<parsed-spec>-private-predicate-id spec))
	     (syntax=? #'((define <the-label>-private-predicate (lambda (x) (list? x))))
		       (help.<parsed-spec>-definitions spec))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.predicate list?)
				 (aux.predicate list?))))
    (=> syntax=?) #'(aux.predicate list?))

  #t)


(parametrise ((check-test-name	'label-method))

;;; single clause

  (check	;single clause, expr body
      (let ((spec (%parse-label-clauses #'((aux.method alpha (lambda (x) x))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (syntax=? (list-ref defs 0)
				  #'(define <the-label>-alpha (lambda (x) x)))
		     ))))
    => '(1 1 #t #t #t))

;;; --------------------------------------------------------------------

  (check	;single clause, integrated body, single method argument
      (let ((spec (%parse-label-clauses #'((aux.method (alpha x) x (list x))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (syntax=? (list-ref defs 0)
				  #'(define <the-label>-alpha (lambda/tags (x) x (list x))))
		     ))))
    => '(1 1 #t #t #t))

  (check      ;single clause, integrated body, multiple method arguments
      (let ((spec (%parse-label-clauses #'((aux.method (alpha x y) x (list y))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (syntax=? (list-ref defs 0)
				  #'(define <the-label>-alpha (lambda/tags (x y) x (list y))))
		     ))))
    => '(1 1 #t #t #t))

  (check	;single clause, integrated body, rest method arguments
      (let ((spec (%parse-label-clauses #'((aux.method (alpha x . y) x (list y))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (syntax=? (list-ref defs 0)
				  #'(define <the-label>-alpha (lambda/tags (x . y) x (list y))))
		     ))))
    => '(1 1 #t #t #t))

  (check    ;single clause, integrated body, identifier method arguments
      (let ((spec (%parse-label-clauses #'((aux.method (alpha . x) x (list x))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (syntax=? (list-ref defs 0)
				  #'(define <the-label>-alpha (lambda/tags x x (list x))))
		     ))))
    => '(1 1 #t #t #t))

;;; --------------------------------------------------------------------
;;; multiple clauses

  (check	;multiple clauses, expr body
      (let ((spec (%parse-label-clauses #'((aux.method alpha (lambda (x) x))
					   (aux.method beta  (lambda (y) y))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 1)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'beta)
			    (free-identifier=? (cdr P) #'<the-label>-beta)))
		     (syntax=? (list-ref defs 0)
				  #'(define <the-label>-alpha (lambda (x) x)))
		     (syntax=? (list-ref defs 1)
				  #'(define <the-label>-beta (lambda (y) y)))
		     ))))
    => '(2 2 #t  #t #t #t #t))

;;; --------------------------------------------------------------------
;;; errors: duplicate member name

  (check	;method name and method name
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.method alpha (lambda (x) x))
				 (aux.method alpha (lambda (x) x)))))
    (=> free-identifier=?) #'alpha)

  (check	;method name and field name
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.method alpha (lambda (x) x))
				 (aux.virtual-fields alpha))))
    (=> free-identifier=?) #'alpha)

  #t)


(parametrise ((check-test-name	'label-method-syntax))

;;; single clause

  (check	;single clause, expr body
      (let ((spec (%parse-label-clauses #'((aux.method-syntax alpha (lambda (x) x))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (syntax=? (list-ref defs 0)
			       #'(define-syntax <the-label>-alpha (lambda (x) x)))
		     ))))
    => '(1 1 #t #t #t))

;;; --------------------------------------------------------------------
;;; multiple clauses

  (check	;multiple clauses, expr body
      (let ((spec (%parse-label-clauses #'((aux.method-syntax alpha (lambda (x) x))
					   (aux.method-syntax beta  (lambda (y) y))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 1)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'beta)
			    (free-identifier=? (cdr P) #'<the-label>-beta)))
		     (syntax=? (list-ref defs 0)
			       #'(define-syntax <the-label>-alpha (lambda (x) x)))
		     (syntax=? (list-ref defs 1)
			       #'(define-syntax <the-label>-beta (lambda (y) y)))
		     ))))
    => '(2 2 #t  #t #t #t #t))

;;; --------------------------------------------------------------------
;;; errors: duplicate member name

  (check	;method name and method name
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.method-syntax alpha (lambda (x) x))
				 (aux.method-syntax alpha (lambda (x) x)))))
    (=> free-identifier=?) #'alpha)

  (check	;method name and field name
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.method-syntax alpha (lambda (x) x))
				 (aux.virtual-fields alpha))))
    (=> free-identifier=?) #'alpha)

  #t)


(parametrise ((check-test-name	'label-methods))

;;; single clause

  (check	;single clause, single method, identifier spec
      (let ((spec (%parse-label-clauses #'((aux.methods alpha)))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     ))))
    => '(1 0 #t #t))

  (check	;single clause, single method, explicit spec
      (let ((spec (%parse-label-clauses #'((aux.methods (alpha alpha-method))))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'alpha-method)))
		     ))))
    => '(1 0 #t #t))

  (check	;single clause, multiple methods, identifier spec
      (let ((spec (%parse-label-clauses #'((aux.methods alpha beta)))))
	(and (help.<label-spec>? spec)
	     (let ((table	(help.<parsed-spec>-methods-table spec))
		   (defs	(help.<parsed-spec>-definitions   spec)))
	       (list (length table)
		     (length defs)
		     (for-all (lambda (pair)
				(identifier? (car pair))
				(identifier? (cdr pair)))
		       table)
		     (let ((P (list-ref table 1)))
		       (and (free-identifier=? (car P) #'alpha)
			    (free-identifier=? (cdr P) #'<the-label>-alpha)))
		     (let ((P (list-ref table 0)))
		       (and (free-identifier=? (car P) #'beta)
			    (free-identifier=? (cdr P) #'<the-label>-beta)))
		     ))))
    => '(2 0 #t #t #t))

;;; --------------------------------------------------------------------
;;; errors: duplicate member name

  (check	;single clause
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.methods alpha alpha))))
    (=> free-identifier=?) #'alpha)

  (check	;multiple clauses
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.methods alpha)
				 (aux.methods alpha))))
    (=> free-identifier=?) #'alpha)

  #t)


(parametrise ((check-test-name	'class-concrete-fields))

  (check ;single clause, single field, immutable field, no accessor, no tags
      (let ((spec (%parse-class-clauses #'((aux.fields)))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-concrete-fields spec)))
    => '())

;;; --------------------------------------------------------------------
;;; immutable fields

  (check ;single clause, single field, immutable field, no accessor, no tags
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable alpha))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-class>-alpha))
				  M
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 (#t #t #f #t)))

  (check ;single clause, single field, immutable field, accessor, no tags
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable alpha alpha-accessor))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'alpha-accessor))
				  M
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 (#t #t #f #t)))

  (check ;single clause, single field, immutable field, no accessor, single tag
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable (alpha <pair>)))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-class>-alpha))
				  M
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 (#t #t #f #t)))

  (check    ;single clause, single field, immutable field, accessor, tag
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable (alpha <pair>)
								      alpha-accessor))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'alpha-accessor))
				  M
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 (#t #t #f #t)))

;;; --------------------------------------------------------------------
;;; mutable fields

  (check ;single clause, single field, mutable field, no accessor+mutator, no tags
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.mutable alpha))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-class>-alpha))
				  (and (identifier? M)
				       (free-identifier=? M #'<the-class>-alpha-set!))
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 (#t #t #t #t)))

  (check ;single clause, single field, mutable field, accessor and mutator, no tags
      (let ((spec (%parse-class-clauses
		      #'((aux.fields (aux.mutable alpha alpha-accessor alpha-mutator))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'alpha-accessor))
				  (and (identifier? M)
				       (free-identifier=? M #'alpha-mutator))
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 (#t #t #t #t)))

  (check ;single clause, single field, mutable field, no accessor+mutator, tag
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.mutable (alpha <pair>)))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-class>-alpha))
				  (and (identifier? M)
				       (free-identifier=? M #'<the-class>-alpha-set!))
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 (#t #t #t #t)))

  (check ;single clause, single field, mutable field, accessor+mutator, multiple tags
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.mutable (alpha <pair>)
								    alpha-accessor alpha-mutator))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'alpha-accessor))
				  (and (identifier? M)
				       (free-identifier=? M #'alpha-mutator))
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 (#t #t #t #t)))

;;; --------------------------------------------------------------------
;;; immutable fields without IMMUTABLE keyword

  (check	;single clause, single field, immutable field, no tags
      (let ((spec (%parse-class-clauses #'((aux.fields alpha)))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-class>-alpha))
				  M
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 (#t #t #f #t)))

  (check       ;single clause, single field, immutable field, single tag
      (let ((spec (%parse-class-clauses #'((aux.fields (alpha <pair>))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-class>-alpha))
				  M
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 (#t #t #f #t)))

;;; --------------------------------------------------------------------
;;; multiple fields

  (check	;single clause, multiple fields
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable alpha)
						       (aux.immutable beta))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (list-ref fields 0))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'alpha)))
			  (let* ((F (list-ref fields 1))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'beta)))
			  )))))
    => '(2 #t #t))

  (check	;single clause, multiple fields
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable alpha)
						       (aux.mutable beta)
						       gamma)))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (list-ref fields 0))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'alpha)))
			  (let* ((F (list-ref fields 1))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'beta)))
			  (let* ((F (list-ref fields 2))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'gamma)))
			  )))))
    => '(3 #t #t #t))

  (check	;single clause, multiple fields
      (let ((spec (%parse-class-clauses #'((aux.fields alpha beta gamma)))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (list-ref fields 0))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'alpha)))
			  (let* ((F (list-ref fields 1))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'beta)))
			  (let* ((F (list-ref fields 2))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'gamma)))
			  )))))
    => '(3 #t #t #t))

  (check	;multiple clauses, multiple fields
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable alpha))
					   (aux.fields (aux.immutable beta))))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (list-ref fields 0))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'alpha)))
			  (let* ((F (list-ref fields 1))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'beta)))
			  )))))
    => '(2 #t #t))

  (check	;multiple clauses, multiple fields
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable alpha)
						       (aux.mutable beta))
					   (aux.fields gamma)))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (list-ref fields 0))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'alpha)))
			  (let* ((F (list-ref fields 1))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'beta)))
			  (let* ((F (list-ref fields 2))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'gamma)))
			  )))))
    => '(3 #t #t #t))

  (check	;multiple clauses, multiple fields
      (let ((spec (%parse-class-clauses #'((aux.fields (aux.immutable alpha))
					   (aux.fields (aux.mutable beta)
						       gamma)))))
	(and (help.<class-spec>? spec)
	     (let ((fields (help.<parsed-spec>-concrete-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<concrete-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (list-ref fields 0))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'alpha)))
			  (let* ((F (list-ref fields 1))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'beta)))
			  (let* ((F (list-ref fields 2))
				 (N (help.<field-spec>-name-id F)))
			    (and (identifier? N)
				 (free-identifier=? N #'gamma)))
			  )))))
    => '(3 #t #t #t))

;;; --------------------------------------------------------------------
;;; errors: duplicate tag identifiers

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.fields (alpha <pair> <pair>)))))
    (=> syntax=?) #'(alpha <pair> <pair>))

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.fields (aux.mutable (alpha <pair> <pair>))))))
    (=> syntax=?) #'(aux.mutable (alpha <pair> <pair>)))

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.fields (aux.immutable (alpha <pair> <pair>))))))
    (=> syntax=?) #'(aux.immutable (alpha <pair> <pair>)))

;;; --------------------------------------------------------------------
;;; errors: field name conflicts with other member name

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.fields alpha alpha))))
    (=> free-identifier=?) #'alpha)

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.fields (aux.mutable alpha)
					     (aux.immutable alpha)))))
    (=> free-identifier=?) #'alpha)

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.fields (aux.mutable alpha))
				 (aux.fields (aux.immutable alpha)))))
    (=> free-identifier=?) #'alpha)

  (check
      (catch-syntax-violation #f
	(%parse-class-clauses #'((aux.fields (aux.mutable alpha)
					     (aux.immutable beta))
				 (aux.fields (aux.immutable alpha)))))
    (=> free-identifier=?) #'alpha)

  #t)


(parametrise ((check-test-name	'label-virtual-fields))

  (check ;single clause, single field, immutable field, no accessor, no tags
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields)))))
	(and (help.<label-spec>? spec)
	     (help.<parsed-spec>-virtual-fields spec)))
    => '())

;;; --------------------------------------------------------------------
;;; immutable fields

  (check ;single clause, single field, immutable field, no accessor, no tags
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable alpha))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  M
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 () (#t #t #f #t)))

  (check ;single clause, single field, immutable field, accessor id, no tags
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable alpha alpha-accessor))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'alpha-accessor))
				  M
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 () (#t #t #f #t)))

  (check ;single clause, single field, immutable field, accessor expr, no tags
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable alpha (lambda (x) x)))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (syntax=? #'((define <the-label>-alpha (lambda (x) x)))
				    (help.<parsed-spec>-definitions spec))
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  M
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 #t (#t #t #f #t)))

  (check ;single clause, single field, immutable field, no accessor, single tag
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable (alpha <pair>)))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  M
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 () (#t #t #f #t)))

  (check ;single clause, single field, immutable field, accessor id, tag
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable (alpha <pair>)
									      alpha-accessor))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'alpha-accessor))
				  M
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 () (#t #t #f #t)))

  (check ;single clause, single field, immutable field, accessor expr, tag
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable (alpha <pair>)
									      (lambda (x) x)))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (syntax=? (help.<parsed-spec>-definitions spec)
				    #'((define <the-label>-alpha (lambda (x) x))))
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  M
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 #t (#t #t #f #t)))

;;; --------------------------------------------------------------------
;;; mutable fields

  (check ;single clause, single field, mutable field, no accessor+mutator, no tag
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.mutable alpha))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  (and (identifier? M)
				       (free-identifier=? M #'<the-label>-alpha-set!))
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 () (#t #t #t #t)))

  (check ;single clause, single field, mutable field, accessor id and mutator id, no tag
      (let ((spec (%parse-label-clauses
		      #'((aux.virtual-fields (aux.mutable alpha alpha-accessor alpha-mutator))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'alpha-accessor))
				  (and (identifier? M)
				       (free-identifier=? M #'alpha-mutator))
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 () (#t #t #t #t)))

  (check ;single clause, single field, mutable field, accessor expr and mutator expr, no tags
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.mutable alpha
									    (lambda (x) x)
									    (lambda (y v) v)))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (syntax=? (help.<parsed-spec>-definitions spec)
				    #'((define <the-label>-alpha (lambda (x) x))
				       (define <the-label>-alpha-set! (lambda (y v) v))))
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  (and (identifier? M)
				       (free-identifier=? M #'<the-label>-alpha-set!))
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 #t (#t #t #t #t)))

  (check ;single clause, single field, mutable field, no accessor+mutator, tag
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.mutable (alpha <pair>)))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  (and (identifier? M)
				       (free-identifier=? M #'<the-label>-alpha-set!))
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 (#t #t #t #t)))

  (check ;single clause, single field, mutable field, accessor+mutator, tag
      (let ((spec (%parse-label-clauses
		      #'((aux.virtual-fields (aux.mutable (alpha <pair>)
							  alpha-accessor alpha-mutator))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'alpha-accessor))
				  (and (identifier? M)
				       (free-identifier=? M #'alpha-mutator))
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 () (#t #t #t #t)))

;;; --------------------------------------------------------------------
;;; immutable fields without IMMUTABLE keyword

  (check	;single clause, single field, immutable field, no tags
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields alpha)))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  M
				  (free-identifier=? T #'<top>)))
			  )))))
    => '(1 () (#t #t #f #t)))

  (check       ;single clause, single field, immutable field, single tag
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (alpha <pair>))))))
	(and (help.<label-spec>? spec)
	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
	       (and (and (list? fields)
			 (for-all help.<virtual-field-spec>? fields))
		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
			  (let* ((F (car fields))
				 (N (help.<field-spec>-name-id		F))
				 (A (help.<field-spec>-accessor-id	F))
				 (M (help.<field-spec>-mutator-id	F))
				 (T (help.<field-spec>-tag-id		F)))
			    (list (and (identifier? N)
				       (free-identifier=? N #'alpha))
				  (and (identifier? A)
				       (free-identifier=? A #'<the-label>-alpha))
				  M
				  (and T
				       (identifier? T)
				       (free-identifier=? T #'<pair>))))
			  )))))
    => '(1 () (#t #t #f #t)))

;;; --------------------------------------------------------------------
;;; multiple fields

  (check	;single clause, multiple fields
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable alpha)
							       (aux.immutable beta))))))
  	(and (help.<label-spec>? spec)
  	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
  	       (and (and (list? fields)
  			 (for-all help.<virtual-field-spec>? fields))
  		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
  			  (let* ((F (list-ref fields 1))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'alpha)))
  			  (let* ((F (list-ref fields 0))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'beta)))
  			  )))))
    => '(2 () #t #t))

  (check	;single clause, multiple fields
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable alpha)
							       (aux.mutable beta)
							       gamma)))))
  	(and (help.<label-spec>? spec)
  	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
  	       (and (and (list? fields)
  			 (for-all help.<virtual-field-spec>? fields))
  		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
  			  (let* ((F (list-ref fields 2))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'alpha)))
  			  (let* ((F (list-ref fields 1))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'beta)))
  			  (let* ((F (list-ref fields 0))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'gamma)))
  			  )))))
    => '(3 () #t #t #t))

  (check	;single clause, multiple fields
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields alpha beta gamma)))))
  	(and (help.<label-spec>? spec)
  	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
  	       (and (and (list? fields)
  			 (for-all help.<virtual-field-spec>? fields))
  		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
  			  (let* ((F (list-ref fields 2))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'alpha)))
  			  (let* ((F (list-ref fields 1))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'beta)))
  			  (let* ((F (list-ref fields 0))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'gamma)))
  			  )))))
    => '(3 () #t #t #t))

  (check	;multiple clauses, multiple fields
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable alpha))
					   (aux.virtual-fields (aux.immutable beta))))))
  	(and (help.<label-spec>? spec)
  	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
  	       (and (and (list? fields)
  			 (for-all help.<virtual-field-spec>? fields))
  		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
  			  (let* ((F (list-ref fields 1))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'alpha)))
  			  (let* ((F (list-ref fields 0))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'beta)))
  			  )))))
    => '(2 () #t #t))

  (check	;multiple clauses, multiple fields
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable alpha)
							       (aux.mutable beta))
					   (aux.virtual-fields gamma)))))
  	(and (help.<label-spec>? spec)
  	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
  	       (and (and (list? fields)
  			 (for-all help.<virtual-field-spec>? fields))
  		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
  			  (let* ((F (list-ref fields 2))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'alpha)))
  			  (let* ((F (list-ref fields 1))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'beta)))
  			  (let* ((F (list-ref fields 0))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'gamma)))
  			  )))))
    => '(3 () #t #t #t))

  (check	;multiple clauses, multiple fields
      (let ((spec (%parse-label-clauses #'((aux.virtual-fields (aux.immutable alpha))
					   (aux.virtual-fields (aux.mutable beta)
							       gamma)))))
  	(and (help.<label-spec>? spec)
  	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
  	       (and (and (list? fields)
  			 (for-all help.<virtual-field-spec>? fields))
  		    (list (length fields)
			  (help.<parsed-spec>-definitions spec)
  			  (let* ((F (list-ref fields 2))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'alpha)))
  			  (let* ((F (list-ref fields 1))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'beta)))
  			  (let* ((F (list-ref fields 0))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'gamma)))
  			  )))))
    => '(3 () #t #t #t))

  (check	;multiple clauses, multiple fields, definitions
      (let ((spec (%parse-label-clauses
		      #'((aux.virtual-fields (aux.immutable alpha (lambda (x) x)))
			 (aux.virtual-fields (aux.mutable beta (lambda (y) y) (lambda (z) z))
					     gamma)))))
  	(and (help.<label-spec>? spec)
  	     (let ((fields (help.<parsed-spec>-virtual-fields spec)))
  	       (and (and (list? fields)
  			 (for-all help.<virtual-field-spec>? fields))
  		    (list (length fields)
			  (syntax=? (help.<parsed-spec>-definitions spec)
				    #'((define <the-label>-alpha (lambda (x) x))
				       (define <the-label>-beta (lambda (y) y))
				       (define <the-label>-beta-set! (lambda (z) z))))
  			  (let* ((F (list-ref fields 2))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'alpha)))
  			  (let* ((F (list-ref fields 1))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'beta)))
  			  (let* ((F (list-ref fields 0))
  				 (N (help.<field-spec>-name-id F)))
  			    (and (identifier? N)
  				 (free-identifier=? N #'gamma)))
  			  )))))
    => '(3 #t #t #t #t))

;;; --------------------------------------------------------------------
;;; errors: field name conflicts with other member name

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.virtual-fields alpha alpha))))
    (=> free-identifier=?) #'alpha)

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.virtual-fields (aux.mutable alpha)
						     (aux.immutable alpha)))))
    (=> free-identifier=?) #'alpha)

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.virtual-fields (aux.mutable alpha))
				 (aux.virtual-fields (aux.immutable alpha)))))
    (=> free-identifier=?) #'alpha)

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.virtual-fields (aux.mutable alpha)
						     (aux.immutable beta))
				 (aux.virtual-fields (aux.immutable alpha)))))
    (=> free-identifier=?) #'alpha)

  #t)


(parametrise ((check-test-name	'label-getter))

  (check	;identifier argument, matching getter identifier
      (let ((spec (%parse-label-clauses #'((aux.getter <the-label>-getter)))))
	(list (help.<label-spec>? spec)
	      (free-identifier=? #'<the-label>-getter (help.<parsed-spec>-getter spec))))
    => '(#t #t))

  (check	;identifier argument, custom getter identifier
      (let ((spec (%parse-label-clauses #'((aux.getter the-getter)))))
	(list (help.<label-spec>? spec)
	      (free-identifier=? #'the-getter
				 (help.<parsed-spec>-getter spec))))
    => '(#t #t))

  (check	;identifier argument, getter expression
      (let ((spec (%parse-label-clauses #'((aux.getter (lambda (x) x))))))
	(list (help.<label-spec>? spec)
	      (syntax=? #'(lambda (x) x)
			(help.<parsed-spec>-getter spec))))
    => '(#t #t))

;;; --------------------------------------------------------------------
;;; errors

  (check	;clause used multiple times
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.getter alpha)
				 (aux.getter beta))))
    (=> syntax=?) #'(aux.getter beta))

  (check	;clause has multiple arguments
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.getter alpha beta))))
    (=> syntax=?) #'(aux.getter alpha beta))

  (check	;unknown clauses after parsing
      (catch-syntax-violation #f
	(%parse-label-clauses #'((ciao alpha)
				 (aux.getter alpha)
				 (ciao beta))))
    (=> syntax=?) #'((ciao alpha) (ciao beta)))

  #t)


(parametrise ((check-test-name	'label-setter))

  (check	;identifier argument, matching setter identifier
      (let ((spec (%parse-label-clauses #'((aux.setter <the-label>-setter)))))
	(list (help.<label-spec>? spec)
	      (free-identifier=? #'<the-label>-setter (help.<parsed-spec>-setter spec))))
    => '(#t #t))

  (check	;identifier argument, custom setter identifier
      (let ((spec (%parse-label-clauses #'((aux.setter the-setter)))))
	(list (help.<label-spec>? spec)
	      (free-identifier=? #'the-setter
				 (help.<parsed-spec>-setter spec))))
    => '(#t #t))

  (check	;identifier argument, setter expression
      (let ((spec (%parse-label-clauses #'((aux.setter (lambda (x) x))))))
	(list (help.<label-spec>? spec)
	      (syntax=? #'(lambda (x) x)
			(help.<parsed-spec>-setter spec))))
    => '(#t #t))

;;; --------------------------------------------------------------------
;;; errors

  (check	;clause used multiple times
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.setter alpha)
				 (aux.setter beta))))
    (=> syntax=?) #'(aux.setter beta))

  (check	;clause has multiple arguments
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.setter alpha beta))))
    (=> syntax=?) #'(aux.setter alpha beta))

  (check	;unknown clauses after parsing
      (catch-syntax-violation #f
	(%parse-label-clauses #'((ciao alpha)
				 (aux.setter alpha)
				 (ciao beta))))
    (=> syntax=?) #'((ciao alpha) (ciao beta)))

  #t)


(parametrise ((check-test-name	'class-satisfactions))

  (check	;no clause
      (let ((spec (%parse-class-clauses #'())))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-satisfactions spec)))
    => '())

  (check	;empty clause
      (let ((spec (%parse-class-clauses #'((aux.satisfies)))))
	(and (help.<class-spec>? spec)
	     (help.<parsed-spec>-satisfactions spec)))
    => '())

;;; --------------------------------------------------------------------

  (check	;single clause, single identifier argument
      (let ((spec (%parse-class-clauses #'((aux.satisfies alpha)))))
	(and (help.<class-spec>? spec)
	     (list (syntax=? #'(alpha)
			     (help.<parsed-spec>-satisfactions spec))
		   (help.<parsed-spec>-definitions spec))))
    => '(#t ()))

  (check	;single clause, multiple identifier arguments
      (let ((spec (%parse-class-clauses #'((aux.satisfies alpha beta)))))
	(and (help.<class-spec>? spec)
	     (list (syntax=? #'(beta alpha)
			     (help.<parsed-spec>-satisfactions spec))
		   (help.<parsed-spec>-definitions spec))))
    => '(#t ()))

  (check	;multiple clause, identifier arguments
      (let ((spec (%parse-class-clauses #'((aux.satisfies alpha)
					   (aux.satisfies beta)))))
	(and (help.<class-spec>? spec)
	     (list (syntax=? #'(beta alpha)
			     (help.<parsed-spec>-satisfactions spec))
		   (help.<parsed-spec>-definitions spec))))
    => '(#t ()))

;;; --------------------------------------------------------------------

  (check	;single clause, single expression argument
      (let ((spec (%parse-class-clauses #'((aux.satisfies (lambda (stx) #f))))))
	(and (help.<class-spec>? spec)
	     (list (syntax-case (help.<parsed-spec>-satisfactions spec) ()
		     ((?id)
		      (identifier? #'?id)
		      #t)
		     (_ #f))
		   (syntax-case (help.<parsed-spec>-definitions spec) (define-syntax lambda)
		     (((define-syntax ?name (lambda (stx) #f)))
		      (identifier? #'?name)
		      #t)
		     (_ #f)))))
    => '(#t #t))

  #t)


(parametrise ((check-test-name	'label-shadows))

  (check	;no clause
      (let ((spec (%parse-label-clauses #'())))
	(and (help.<label-spec>? spec)
	     (help.<parsed-spec>-shadowed-identifier spec)))
    => #f)

  (check
      (let ((spec (%parse-label-clauses #'((aux.shadows alpha)))))
	(and (help.<label-spec>? spec)
	     (free-identifier=? #'alpha (help.<parsed-spec>-shadowed-identifier spec))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: empty clause

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.shadows))))
    (=> syntax=?) #'(aux.shadows))

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.shadows a)
				 (aux.shadows b))))
    (=> syntax=?) #'(aux.shadows b))

  #t)


(parametrise ((check-test-name	'label-maker))

  (check	;no clause
      (let ((spec (%parse-label-clauses #'())))
	(and (help.<label-spec>? spec)
	     (help.<parsed-spec>-maker-transformer spec)))
    => #f)

  (check
      (let ((spec (%parse-label-clauses #'((aux.maker alpha)))))
	(and (help.<label-spec>? spec)
	     (free-identifier=? #'alpha (help.<parsed-spec>-maker-transformer spec))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors: empty clause

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.maker))))
    (=> syntax=?) #'(aux.maker))

;;; --------------------------------------------------------------------
;;; errors: clause used multiple times

  (check
      (catch-syntax-violation #f
	(%parse-label-clauses #'((aux.maker a)
				 (aux.maker b))))
    (=> syntax=?) #'(aux.maker b))

  #t)


(parametrise ((check-test-name	'filter-mixins))

  (check
      (let-values (((mixins clauses)
		    (%filter-and-validate-mixins-clauses #'())))
	(vector mixins clauses))
    => '#(() ()))

;;; --------------------------------------------------------------------
;;; mixins clauses, without substitution map

  (check
      (let-values (((mixins clauses)
		    (%filter-and-validate-mixins-clauses #'((aux.mixins <a> <b> <c>)))))
	(vector mixins clauses))
    (=> syntax=?) (syntax #(((<a>) (<b>) (<c>)) ())))

  (check
      (let-values (((mixins clauses)
		    (%filter-and-validate-mixins-clauses #'((aux.mixins <a>)
							    (aux.mixins <b> <c>)))))
	(vector mixins clauses))
    (=> syntax=?) (syntax #(((<a>) (<b>) (<c>)) ())))

;;; --------------------------------------------------------------------
;;; mixins clauses, with substitution map

  (check
      (let-values (((mixins clauses)
		    (%filter-and-validate-mixins-clauses #'((aux.mixins <a>
									(<b>
									 (<sequence> <string>)
									 (<array>    <vector>))
									<c>)))))
	(vector mixins clauses))
    (=> syntax=?) (syntax #(((<a>)
			     (<b>
			      (<sequence> <string>)
			      (<array>    <vector>))
			     (<c>)) ())))

;;; --------------------------------------------------------------------
;;; non-mixins clauses

  (check
      (let-values (((mixins clauses)
		    (%filter-and-validate-mixins-clauses #'((aux.fields a b c)))))
	(vector mixins clauses))
    (=> syntax=?) (syntax #(() ((aux.fields a b c)))))

  (check
      (let-values (((mixins clauses)
		    (%filter-and-validate-mixins-clauses #'((aux.fields a b c)
							    (aux.methods (e f)
									 (g h))))))
	(vector mixins clauses))
    (=> syntax=?) (syntax #(() ((aux.fields a b c)
				(aux.methods (e f)
					     (g h))))))

;;; --------------------------------------------------------------------
;;; mixed clauses

  (check
      (let-values (((mixins clauses)
		    (%filter-and-validate-mixins-clauses #'((aux.fields a b c)
							    (aux.mixins <a> <b>)))))
	(vector mixins clauses))
    (=> syntax=?) (syntax #(((<a>) (<b>)) ((aux.fields a b c)))))

  (check
      (let-values (((mixins clauses)
		    (%filter-and-validate-mixins-clauses #'((aux.fields a b c)
							    (aux.mixins <a> <b>)
							    (aux.methods (e f)
									 (g h))
							    (aux.mixins <c> <d>)))))
	(vector mixins clauses))
    (=> syntax=?) (syntax #(((<a>) (<b>) (<c>) (<d>))
			    ((aux.fields a b c)
			     (aux.methods (e f)
					  (g h))))))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'catch-syntax-violation 'scheme-indent-function 1)
;; eval: (put '%parse-label-clauses 'scheme-indent-function 1)
;; eval: (put '%parse-class-clauses 'scheme-indent-function 1)
;; eval: (put 'help.parse-mixin-clauses 'scheme-indent-function 1)
;; coding: utf-8-unix
;; End:
