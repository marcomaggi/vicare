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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare functions: syntax utilities\n")


;;;; helpers

(define-syntax %guard-syntax-error
  (syntax-rules (=>)
    ((_ ?body => (?message ?form ?subform))
     (check
	 (guard (E ((syntax-violation? E)
		    (list (string=? (condition-message E)
				    ?message)
			  (syntax=? (syntax-violation-form E)
				    ?form)
			  (syntax=? (syntax-violation-subform E)
				    ?subform)))
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

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put '%guard-syntax-error 'scheme-indent-function 1)
;; End:
