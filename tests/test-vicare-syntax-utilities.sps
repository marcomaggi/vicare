;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the syntax utility functions
;;;Date: Thu Aug 29, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (vicare unsafe operations)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare functions: syntax utilities\n")


;;;; helpers

(define-syntax %guard-syntax-error
  (syntax-rules (=>)
    ((_ ?body => (?message ?form ?subform))
     (check
	 (guard (E ((syntax-violation? E)
		    (list (or (string=? (condition-message E)
					?message)
			      (condition-message E))
			  (or (syntax=? (syntax-violation-form E)
					?form)
			      (syntax-violation-form E))
			  (or (syntax=? (syntax-violation-subform E)
					?subform)
			      (syntax-violation-subform E))))
		   (else E))
	   ?body)
       => '(#t #t #t)))))


(parametrise ((check-test-name	'ids))

  (check
      (identifier-prefix "this-" #'that)
    (=> bound-identifier=?)
    #'this-that)

  (check
      (identifier-prefix 'this- #'that)
    (=> bound-identifier=?)
    #'this-that)

  (check
      (identifier-prefix #'this- #'that)
    (=> bound-identifier=?)
    #'this-that)

;;; --------------------------------------------------------------------

  (check
      (identifier-suffix #'this "-that")
    (=> bound-identifier=?)
    #'this-that)

  (check
      (identifier-suffix #'this- 'that)
    (=> bound-identifier=?)
    #'this-that)

  (check
      (identifier-suffix #'this- #'that)
    (=> bound-identifier=?)
    #'this-that)

;;; --------------------------------------------------------------------

  (check
      (identifier-append #'here #'this "-that-" 'those)
    (=> bound-identifier=?)
    #'this-that-those)

;;; --------------------------------------------------------------------

  (check
      (identifier-format #'here "~a-~a-~a" #'this "that" 'those)
    (=> bound-identifier=?)
    #'this-that-those)

;;; --------------------------------------------------------------------

  (check
      (duplicate-identifiers? (list #'a #'b #'c))
    => #f)

  (check
      (duplicate-identifiers? (list #'a #'b #'c #'b))
    (=> bound-identifier=?)
    #'b)

;;; --------------------------------------------------------------------

  (check
      (for-all (lambda (obj1 obj2)
		 (if (identifier? obj2)
		     (bound-identifier=? obj1 obj2)
		   (equal? obj1 obj2)))
	(delete-duplicate-identifiers (list #'a #'b #'c #'b))
	(list #'a #'b #'c))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (for-all bound-identifier=?
	(identifier-memq #'b (list #'a #'b #'c))
	(list #'b #'c))
    => #t)

  #t)


(parametrise ((check-test-name	'records))

  (check
      (identifier-record-constructor #'alpha)
    (=> bound-identifier=?)
    #'make-alpha)

;;; --------------------------------------------------------------------

  (check
      (identifier-record-predicate #'alpha)
    (=> bound-identifier=?)
    #'alpha?)

;;; --------------------------------------------------------------------

  (check
      (identifier-record-field-accessor #'alpha "one")
    (=> bound-identifier=?)
    #'alpha-one)

;;; --------------------------------------------------------------------

  (check
      (identifier-record-field-mutator #'alpha "one")
    (=> bound-identifier=?)
    #'alpha-one-set!)

  #t)


(parametrise ((check-test-name	'structs))

  (check
      (identifier-struct-constructor #'alpha)
    (=> bound-identifier=?)
    #'make-alpha)

;;; --------------------------------------------------------------------

  (check
      (identifier-struct-predicate #'alpha)
    (=> bound-identifier=?)
    #'alpha?)

;;; --------------------------------------------------------------------

  (check
      (identifier-struct-field-accessor #'alpha "one")
    (=> bound-identifier=?)
    #'alpha-one)

;;; --------------------------------------------------------------------

  (check
      (identifier-struct-field-mutator #'alpha "one")
    (=> bound-identifier=?)
    #'set-alpha-one!)

  #t)


(parametrise ((check-test-name	'pairs))

  (check
      (syntax-car #'(display . 1))
    (=> bound-identifier=?)
    #'display)

;;; --------------------------------------------------------------------

  (check
      (syntax-cdr #'(1 . display))
    (=> bound-identifier=?)
    #'display)

;;; --------------------------------------------------------------------

  (check
      (for-all (lambda (obj1 obj2)
		 (if (identifier? obj2)
		     (bound-identifier=? obj1 obj2)
		   (equal? obj1 obj2)))
	(syntax->list #'(display 123 write))
	(list #'display 123 #'write))
    => #t)

  (%guard-syntax-error
      (syntax->list #'ciao)
    => ("expected syntax object holding proper list as argument" #'ciao #f))

;;; --------------------------------------------------------------------

  (check
      (for-all bound-identifier=?
	(identifiers->list #'(display write))
	(list #'display #'write))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (all-identifiers? #'(a b c))
    => #t)

  (check
      (all-identifiers? #'(a 1 c))
    => #f)

  #t)


(parametrise ((check-test-name	'vectors))

  (check
      (vector-for-all
       (lambda (obj1 obj2)
	 (if (identifier? obj2)
	     (bound-identifier=? obj1 obj2)
	   (equal? obj1 obj2)))
       (syntax->vector #'#(display 123 write))
       (vector #'display 123 #'write))
    => #t)

  #f)


(parametrise ((check-test-name	'unwrap))

  (check
      (syntax-unwrap #'(1 (2 3) 4))
    => '(1 (2 3) 4))

  (check
      (syntax-unwrap #'(1 #(2 3) 4))
    => '(1 #(2 3) 4))

;;; --------------------------------------------------------------------

  (check
      (vector-for-all
       (lambda (obj1 obj2)
	 (if (identifier? obj2)
	     (bound-identifier=? obj1 obj2)
	   (equal? obj1 obj2)))
       (syntax-unwrap #'#(display 123 write))
       (vector #'display 123 #'write))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (for-all (lambda (obj1 obj2)
		 (if (identifier? obj2)
		     (bound-identifier=? obj1 obj2)
		   (equal? obj1 obj2)))
	(syntax-unwrap #'(display 123 write))
	(list #'display 123 #'write))
    => #t)

  #t)


(parametrise ((check-test-name	'inspection))

  ;; (check
  ;;     (quoted-syntax-object? #'(quote alpha))
  ;;   => #t)

  ;; (check
  ;;     (quoted-syntax-object? #'(alpha))
  ;;   => #f)

  #t)


(parametrise ((check-test-name	'clauses))

;;; unwrapping and format validation

  (check
      (syntax-clauses-unwrap #'())
    (=> syntax=?)
    '())

  (check
      (syntax-clauses-unwrap #'((alpha 123)
				(beta  456)))
    (=> syntax=?)
    (list (list #'alpha 123)
	  (list #'beta  456)))

  (%guard-syntax-error
      (syntax-clauses-unwrap #'#(123 123))
    => ("expected list of elements as syntax clauses" #'#(123 123) #f))

  (%guard-syntax-error
      (syntax-clauses-unwrap #'(#(123 123)))
    => ("invalid clause syntax" #'#(123 123) #f))

  (%guard-syntax-error
      (syntax-clauses-unwrap #'((123 123)))
    => ("expected identifier as syntax clause first element" 123 #f))

;;; --------------------------------------------------------------------
;;; selecting clauses

  (check
      (syntax-clauses-filter (list #'alpha #'gamma)
			     (syntax-clauses-unwrap #'((alpha 123)
						       (beta  456))))
    (=> syntax=?)
    (list (list #'alpha 123)))

  (check
      (syntax-clauses-filter (list #'gamma)
			     (syntax-clauses-unwrap #'((alpha 123)
						       (beta  456))))
    (=> syntax=?)
    '())

;;; --------------------------------------------------------------------
;;; discarding clauses

  (check
      (syntax-clauses-remove (list #'alpha #'gamma)
			     (syntax-clauses-unwrap #'((alpha 123)
						       (beta  456))))
    (=> syntax=?)
    (list (list #'beta 456)))

  (check
      (syntax-clauses-remove (list #'gamma)
			     (syntax-clauses-unwrap #'((alpha 123)
						       (beta  456))))
    (=> syntax=?)
    (list (list #'alpha 123)
	  (list #'beta  456)))

;;; --------------------------------------------------------------------
;;; partitioning clauses

  (check
      (receive (match no-match)
	  (syntax-clauses-partition (list #'alpha #'gamma)
				    (syntax-clauses-unwrap #'((alpha 123)
							      (beta  456))))
	(vector match no-match))
    (=> syntax=?)
    (vector (list (list #'alpha 123))
	    (list (list #'beta  456))))

;;; --------------------------------------------------------------------
;;; collapsing clauses

  (check
      (syntax-clauses-collapse (syntax-clauses-unwrap #'((alpha 123)
							 (beta  456))))
    (=> syntax=?)
    (list (list #'alpha 123)
	  (list #'beta  456)))

  (check
      (syntax-clauses-collapse (syntax-clauses-unwrap #'((fields a b c)
							 (fields d e f))))
    (=> syntax=?)
    (list (list #'fields #'a #'b #'c #'d #'e #'f)))

  (check
      (syntax-clauses-collapse (syntax-clauses-unwrap #'((fields a b)
							 (fields c d)
							 (fields e f))))
    (=> syntax=?)
    (list (list #'fields #'a #'b #'c #'d #'e #'f)))

  (check
      (syntax-clauses-collapse (syntax-clauses-unwrap #'((fields (mutable a) (immutable b))
							 (fields (mutable c) (immutable d))
							 (fields (mutable e) (immutable f)))))
    (=> syntax=?)
    (list (list #'fields
		(list #'mutable #'a) (list #'immutable #'b)
		(list #'mutable #'c) (list #'immutable #'d)
		(list #'mutable #'e) (list #'immutable #'f))))

;;; --------------------------------------------------------------------
;;; verify at least once

  (check
      (guard (E (else E))
	(syntax-clauses-verify-at-least-once (list #'a #'b)
					     (syntax-clauses-unwrap #'((a 123)
								       (b 456)
								       (d 789))))
	#t)
    => #t)

  (%guard-syntax-error
      (syntax-clauses-verify-at-least-once (list #'a #'b)
					   (syntax-clauses-unwrap #'((a 123)
								     (d 789))))

    => ("missing mandatory clause" #'b #f))

;;; --------------------------------------------------------------------
;;; verify at most once

  (check	;present
      (guard (E (else E))
	(syntax-clauses-verify-at-most-once (list #'a #'b)
					    (syntax-clauses-unwrap #'((a 123)
								      (b 456)
								      (d 789))))
	#t)
    => #t)

  (check	;missing
      (guard (E (else E))
	(syntax-clauses-verify-at-most-once (list #'a #'b)
					    (syntax-clauses-unwrap #'((d 789))))
	#t)
    => #t)

  (%guard-syntax-error
      (syntax-clauses-verify-at-most-once (list #'a #'b)
					  (syntax-clauses-unwrap #'((a 123)
								    (a 456)
								    (d 789))))

    => ("clause must be present at most once"
	(list (list #'a 123)
	      (list #'a 456))
	#f))

;;; --------------------------------------------------------------------
;;; verify exactly once

  (check	;present
      (guard (E (else E))
	(syntax-clauses-verify-exactly-once (list #'a #'b)
					    (syntax-clauses-unwrap #'((a 123)
								      (b 456)
								      (d 789))))
	#t)
    => #t)

  (%guard-syntax-error
      (syntax-clauses-verify-exactly-once (list #'a #'b)
					  (syntax-clauses-unwrap #'((d 789)
								    (a 123))))
    => ("clause must be present exactly once" #'b #f))

  (%guard-syntax-error
      (syntax-clauses-verify-exactly-once (list #'a #'b)
					  (syntax-clauses-unwrap #'((a 123)
								    (a 456)
								    (d 789))))

    => ("clause must be present exactly once"
	(list (list #'a 123)
	      (list #'a 456))
	#f))

;;; --------------------------------------------------------------------
;;; verify mutually inclusive

  (check	;present
      (guard (E (else E))
	(syntax-clauses-verify-mutually-inclusive (list #'a #'b)
						  (syntax-clauses-unwrap #'((a 123)
									    (b 456)
									    (d 789))))
	#t)
    => #t)

  (check	;all missing
      (guard (E (else E))
	(syntax-clauses-verify-mutually-inclusive (list #'a #'b)
						  (syntax-clauses-unwrap #'((d 789)
									    (e 0))))
	#t)
    => #t)

  ;;One missing.
  ;;
  (%guard-syntax-error
      (syntax-clauses-verify-mutually-inclusive (list #'a #'b)
						(syntax-clauses-unwrap #'((a 123)
									  (d 789))))
    => ("mutually inclusive clauses are missing" (list #'b) #f))

;;; --------------------------------------------------------------------
;;; verify mutually exclusive

  (check	;first present
      (guard (E (else E))
	(syntax-clauses-verify-mutually-exclusive (list #'a #'b #'c)
						  (syntax-clauses-unwrap #'((a 123)
									    (d 789))))
	#t)
    => #t)

  (check	;second present
      (guard (E (else E))
	(syntax-clauses-verify-mutually-exclusive (list #'a #'b #'c)
						  (syntax-clauses-unwrap #'((b 456)
									    (d 789))))
	#t)
    => #t)

  (check	;third present
      (guard (E (else E))
	(syntax-clauses-verify-mutually-exclusive (list #'a #'b #'c)
						  (syntax-clauses-unwrap #'((c 456)
									    (d 789))))
	#t)
    => #t)

  (check	;all missing
      (guard (E (else E))
	(syntax-clauses-verify-mutually-exclusive (list #'a #'b #'c)
						  (syntax-clauses-unwrap #'((d 789)
									    (e 0))))
	#t)
    => #t)

  ;;First and second present.
  ;;
  (%guard-syntax-error
      (syntax-clauses-verify-mutually-exclusive (list #'a #'b #'c)
						(syntax-clauses-unwrap #'((a 1)
									  (b 2)
									  (d 4))))
    => ("mutually exclusive clauses are present"
	#'((a 1)
	   (b 2)) #f))

  ;;First and third present.
  ;;
  (%guard-syntax-error
      (syntax-clauses-verify-mutually-exclusive (list #'a #'b #'c)
						(syntax-clauses-unwrap #'((a 1)
									  (c 3)
									  (d 4))))
    => ("mutually exclusive clauses are present"
	#'((a 1)
	   (c 3)) #f))

  ;;Second and third present.
  ;;
  (%guard-syntax-error
      (syntax-clauses-verify-mutually-exclusive (list #'a #'b #'c)
						(syntax-clauses-unwrap #'((b 2)
									  (c 3)
									  (d 4))))
    => ("mutually exclusive clauses are present"
	#'((b 2)
	   (c 3)) #f))

  ;;All present.
  ;;
  (%guard-syntax-error
      (syntax-clauses-verify-mutually-exclusive (list #'a #'b #'c)
						(syntax-clauses-unwrap #'((a 1)
									  (b 2)
									  (c 3)
									  (d 4))))
    => ("mutually exclusive clauses are present"
	#'((a 1)
	   (b 2)
	   (c 3)) #f))

  #t)


(parametrise ((check-test-name	'clauses-structs))

;;; single struct clauses validation

  ;;Single occurrence, single value.
  ;;
  (check
      (let ((spec (make-syntax-clause-spec #'b 1 1 1 1 (list #'a #'d) (list #'W))))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b 456)
								   (d 789)))))
    (=> syntax=?)
    '((456)))

  ;;Single occurrence, multiple values.
  ;;
  (check
      (let ((spec (make-syntax-clause-spec #'b 1 1 1 +inf.0 (list #'a #'d) (list #'W))))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b 4 5 6)
								   (d 789)))))
    (=> syntax=?)
    '((4 5 6)))

  ;;Multiple occurrences, single value.
  ;;
  (check
      (let ((spec (make-syntax-clause-spec #'b 1 +inf.0 1 1 (list #'a #'d) (list #'W))))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b 4)
								   (b 5)
								   (b 6)
								   (d 789)))))
    (=> syntax=?)
    '((4) (5) (6)))

  ;;Multiple occurrences, multiple values.
  ;;
  (check
      (let ((spec (make-syntax-clause-spec #'b 1 +inf.0 1 +inf.0 (list #'a #'d) (list #'W))))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b 4 5 6)
								   (d 789)
								   (b x y z)
								   (b l m n)
								   ))))
    (=> syntax=?)
    #'((4 5 6) (x y z) (l m n)))

  ;;Not present.
  ;;
  (%guard-syntax-error
      (let ((spec (make-syntax-clause-spec #'b 1 1 1 1 '() '())))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (d 789)))))
    => ("clause must be present at least 1 times and at most 1 times"
	#'b #f))

  ;;Too many clauses.
  ;;
  (%guard-syntax-error
      (let ((spec (make-syntax-clause-spec #'b 1 1 1 1 '() '())))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b 8)
								   (b 9)
								   (d 789)))))
    => ("clause must be present at least 1 times and at most 1 times"
	#'((b 8) (b 9)) #f))

  ;;No argument.
  ;;
  (%guard-syntax-error
      (let ((spec (make-syntax-clause-spec #'b 1 1 1 1 '() '())))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b)
								   (d 789)))))
    => ("clause must have at least 1 arguments and at most 1 arguments"
	#'(b) #f))

  ;;Too many arguments.
  ;;
  (%guard-syntax-error
      (let ((spec (make-syntax-clause-spec #'b 1 1 1 1 '() '())))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b 8 9)
								   (d 789)))))
    => ("clause must have at least 1 arguments and at most 1 arguments"
	#'(b 8 9) #f))

  ;;Missing mutually inclusive.
  ;;
  (%guard-syntax-error
      (let ((spec (make-syntax-clause-spec #'b 1 1 1 1 (list #'W) '())))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b 456)
								   (d 789)))))
    => ("missing mutually inclusive clause" (list #'b #'W) #f))

  ;;Present mutually exclusive.
  ;;
  (%guard-syntax-error
      (let ((spec (make-syntax-clause-spec #'b 1 1 1 1 '() (list #'d))))
	(syntax-clauses-single-spec spec (syntax-clauses-unwrap #'((a 123)
								   (b 456)
								   (d 789)))))
    => ("mutually exclusive clauses are present"
	#'((b 456) (d 789))
	#f))

;;; --------------------------------------------------------------------
;;; clauses specs folding

  (check	;single spec
      (guard (E (else E))
	(let ((specs    (list (make-syntax-clause-spec #'a 1 1 1 1 '() '())))
	      (clauses  (syntax-clauses-unwrap #'((a 123)
						  (b 456)
						  (d 789)))))
	  (syntax-clauses-fold-specs (lambda (knil spec args)
				       (cons args knil))
				     '() specs clauses)))
    (=> syntax=?)
    #'(((123))))

  (check	;multiple specs
      (guard (E (else E))
	(let ((specs    (list (make-syntax-clause-spec #'a 1 1 1 1 '() '())
			      (make-syntax-clause-spec #'b 1 1 1 1 '() '())
			      (make-syntax-clause-spec #'d 1 1 1 1 '() '())))
	      (clauses  (syntax-clauses-unwrap #'((a 123)
						  (b 456)
						  (d 789)))))
	  (syntax-clauses-fold-specs (lambda (knil spec args)
				       (cons (cons (identifier->string (syntax-clause-spec-keyword spec))
						   args)
					     knil))
				     '() specs clauses)))
    (=> syntax=?)
    (reverse '(("a" (123)) ("b" (456)) ("d" (789)))))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put '%guard-syntax-error 'scheme-indent-function 1)
;; End:
