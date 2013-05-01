;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for library utility functions
;;;Date: Mon Apr 22, 2013
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
  (vicare language-extensions simple-match)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: library utility functions\n")


(parametrise ((check-test-name	'names-decomposition))

  (check
      (library-name->version '(alpha))
    => '())

  (check
      (library-name->version '(alpha beta))
    => '())

  (check
      (library-name->version '(alpha beta gamma))
    => '())

  (check
      (library-name->version '(alpha beta gamma ()))
    => '())

  (check
      (library-name->version '(alpha beta gamma (1)))
    => '(1))

  (check
      (library-name->version '(alpha beta gamma (1 2 3)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (library-name->identifiers '(alpha))
    => '(alpha))

  (check
      (library-name->identifiers '(alpha beta))
    => '(alpha beta))

  (check
      (library-name->identifiers '(alpha beta gamma))
    => '(alpha beta gamma))

  (check
      (library-name->identifiers '(alpha beta gamma ()))
    => '(alpha beta gamma))

  (check
      (library-name->identifiers '(alpha beta gamma (1)))
    => '(alpha beta gamma))

  (check
      (library-name->identifiers '(alpha beta gamma (1 2 3)))
    => '(alpha beta gamma))

;;; --------------------------------------------------------------------

  (let-syntax ((check-decomposition
		(syntax-rules ()
		  ((_ ?sexp ?result)
		   (check
		       (receive (identifiers version)
			   (library-name-decompose ?sexp)
			 (vector identifiers version))
		     => ?result)))))

    (check-decomposition '(alpha)
			 '#( (alpha) () ))

    (check-decomposition '(alpha beta)
			 '#( (alpha beta) () ))

    (check-decomposition '(alpha beta gamma)
			 '#( (alpha beta gamma) () ))

    (check-decomposition '(alpha beta gamma ())
			 '#( (alpha beta gamma) () ))

    (check-decomposition '(alpha beta gamma (1))
			 '#( (alpha beta gamma) (1) ))

    (check-decomposition '(alpha beta gamma (1 2 3))
			 '#( (alpha beta gamma) (1 2 3) ))

    (check-decomposition '(alpha beta gamma (1 A 3))
			 '#( #f #f ))

    (check-decomposition '(alpha 1 gamma (1 2 3))
			 '#( #f #f ))

    #f)

  #f)


(parametrise ((check-test-name	'names-predicates))

  (check
      (library-name? '())
    => #f)

  (check
      (library-name? '(alpha))
    => #t)

  (check
      (library-name? '(alpha beta))
    => #t)

  (check
      (library-name? '(alpha beta gamma))
    => #t)

  (check
      (library-name? '(alpha beta gamma ()))
    => #t)

  (check
      (library-name? '(alpha beta gamma (1)))
    => #t)

  (check
      (library-name? '(alpha beta gamma (1 2 3)))
    => #t)

  (check
      (library-name? '(alpha 123 gamma))
    => #f)

  (check
      (library-name? '(alpha beta gamma (1 ciao)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (library-version-numbers? '(1))
    => #t)

  (check
      (library-version-numbers? '(1 2 3))
    => #t)

  (check
      (library-version-numbers? '(1 -2 3))
    => #f)

  (check
      (library-version-numbers? '(1 A 3))
    => #f)

  #t)


(parametrise ((check-test-name	'names-comparison))

  (check (library-version=? '() '()) => #t)
  (check (library-version=? '(1) '(1)) => #t)
  (check (library-version=? '(1 2) '(1 2)) => #t)
  (check (library-version=? '(1 2 3) '(1 2 3)) => #t)

  (check (library-version=? '(1) '(2)) => #f)
  (check (library-version=? '(1 2) '(1 3)) => #f)
  (check (library-version=? '(1 2 3) '(1 2 4)) => #f)

  (check (library-version=? '(1 2 0 0 0) '(1 2)) => #t)
  (check (library-version=? '(1 2) '(1 2 0 0 0)) => #t)

  (check (library-version=? '(1 2 0 0 1) '(1 2)) => #f)
  (check (library-version=? '(1 2) '(1 2 0 0 1)) => #f)

;;; --------------------------------------------------------------------

  (check (library-version<? '() '()) => #f)
  (check (library-version<? '(1) '(1)) => #f)
  (check (library-version<? '(1 2) '(1 2)) => #f)
  (check (library-version<? '(1 2 3) '(1 2 3)) => #f)

  (check (library-version<? '(1) '(2)) => #t)
  (check (library-version<? '(2) '(1)) => #f)
  (check (library-version<? '(1 2) '(1 3)) => #t)
  (check (library-version<? '(1 3) '(1 2)) => #f)
  (check (library-version<? '(1 2 3) '(1 2 4)) => #t)
  (check (library-version<? '(1 2 4) '(1 2 3)) => #f)

  (check (library-version<? '(1 2 0 0 0) '(1 2)) => #f)
  (check (library-version<? '(1 2) '(1 2 0 0 0)) => #f)

  (check (library-version<? '(1 2 0 0 1) '(1 2)) => #f)
  (check (library-version<? '(1 2) '(1 2 0 0 1)) => #t)

;;; --------------------------------------------------------------------

  (check (library-version<=? '() '()) => #t)
  (check (library-version<=? '(1) '(1)) => #t)
  (check (library-version<=? '(1 2) '(1 2)) => #t)
  (check (library-version<=? '(1 2 3) '(1 2 3)) => #t)

  (check (library-version<=? '(1) '(2)) => #t)
  (check (library-version<=? '(2) '(1)) => #f)
  (check (library-version<=? '(1 2) '(1 3)) => #t)
  (check (library-version<=? '(1 3) '(1 2)) => #f)
  (check (library-version<=? '(1 2 3) '(1 2 4)) => #t)
  (check (library-version<=? '(1 2 4) '(1 2 3)) => #f)

  (check (library-version<=? '(1 2 0 0 0) '(1 2)) => #t)
  (check (library-version<=? '(1 2) '(1 2 0 0 0)) => #t)

  (check (library-version<=? '(1 2 0 0 1) '(1 2)) => #f)
  (check (library-version<=? '(1 2) '(1 2 0 0 1)) => #t)

;;; --------------------------------------------------------------------

  (check (library-name-identifiers=? '(a) '(a)) => #t)
  (check (library-name-identifiers=? '(a b) '(a b)) => #t)
  (check (library-name-identifiers=? '(a b c) '(a b c)) => #t)

  (check (library-name-identifiers=? '(a) '(b)) => #f)
  (check (library-name-identifiers=? '(a b) '(b a)) => #f)
  (check (library-name-identifiers=? '(a b c) '(a b z)) => #f)

  (check (library-name-identifiers=? '(a b c (2)) '(a b c (1))) => #t)
  (check (library-name-identifiers=? '(a b c (2)) '(a b d (1))) => #f)

;;; --------------------------------------------------------------------

  (check (library-name=? '(a) '(a)) => #t)
  (check (library-name=? '(a b) '(a b)) => #t)
  (check (library-name=? '(a b c) '(a b c)) => #t)

  (check (library-name=? '(a) '(b)) => #f)
  (check (library-name=? '(a b) '(b a)) => #f)
  (check (library-name=? '(a b c) '(a b z)) => #f)

  (check (library-name=? '(a b c (2)) '(a b c (1))) => #f)
  (check (library-name=? '(a b c (2)) '(a b d (1))) => #f)

  (check (library-name=? '(a ()) '(a ())) => #t)
  (check (library-name=? '(a (1)) '(a (1))) => #t)
  (check (library-name=? '(a (1 2)) '(a (1 2))) => #t)
  (check (library-name=? '(a (1 2 3)) '(a (1 2 3))) => #t)

  (check (library-name=? '(a (1)) '(a (2))) => #f)
  (check (library-name=? '(a (1 2)) '(a (1 3))) => #f)
  (check (library-name=? '(a (1 2 3)) '(a (1 2 4))) => #f)

  (check (library-name=? '(a (1 2 0 0 0)) '(a (1 2))) => #t)
  (check (library-name=? '(a (1 2)) '(a (1 2 0 0 0))) => #t)

  (check (library-name=? '(a (1 2 0 0 1)) '(a (1 2))) => #f)
  (check (library-name=? '(a (1 2)) '(a (1 2 0 0 1))) => #f)

;;; --------------------------------------------------------------------

  (check (library-name<? '(a ()) '(a ())) => #f)
  (check (library-name<? '(a (1)) '(a (1))) => #f)
  (check (library-name<? '(a (1 2)) '(a (1 2))) => #f)
  (check (library-name<? '(a (1 2 3)) '(a (1 2 3))) => #f)

  (check (library-name<? '(a (1)) '(a (2))) => #t)
  (check (library-name<? '(a (2)) '(a (1))) => #f)
  (check (library-name<? '(a (1 2)) '(a (1 3))) => #t)
  (check (library-name<? '(a (1 3)) '(a (1 2))) => #f)
  (check (library-name<? '(a (1 2 3)) '(a (1 2 4))) => #t)
  (check (library-name<? '(a (1 2 4)) '(a (1 2 3))) => #f)

  (check (library-name<? '(a (1 2 0 0 0)) '(a (1 2))) => #f)
  (check (library-name<? '(a (1 2)) '(a (1 2 0 0 0))) => #f)

  (check (library-name<? '(a (1 2 0 0 1)) '(a (1 2))) => #f)
  (check (library-name<? '(a (1 2)) '(a (1 2 0 0 1))) => #t)

;;; --------------------------------------------------------------------

  (check (library-name<=? '(a ()) '(a ())) => #t)
  (check (library-name<=? '(a (1)) '(a (1))) => #t)
  (check (library-name<=? '(a (1 2)) '(a (1 2))) => #t)
  (check (library-name<=? '(a (1 2 3)) '(a (1 2 3))) => #t)

  (check (library-name<=? '(a (1)) '(a (2))) => #t)
  (check (library-name<=? '(a (2)) '(a (1))) => #f)
  (check (library-name<=? '(a (1 2)) '(a (1 3))) => #t)
  (check (library-name<=? '(a (1 3)) '(a (1 2))) => #f)
  (check (library-name<=? '(a (1 2 3)) '(a (1 2 4))) => #t)
  (check (library-name<=? '(a (1 2 4)) '(a (1 2 3))) => #f)

  (check (library-name<=? '(a (1 2 0 0 0)) '(a (1 2))) => #t)
  (check (library-name<=? '(a (1 2)) '(a (1 2 0 0 0))) => #t)

  (check (library-name<=? '(a (1 2 0 0 1)) '(a (1 2))) => #f)
  (check (library-name<=? '(a (1 2)) '(a (1 2 0 0 1))) => #t)

  #t)


(parametrise ((check-test-name	'names-sorting))

  (define A '(x y (1 2)))
  (define B '(x y (1 3)))
  (define C '(x y (1 4)))
  (define D '(x y (2 1)))

  (check
      (list-sort library-name<? (list A B))
    => (list A B))

  (check
      (list-sort library-name<? (list A D C B))
    => (list A B C D))

  #t)


(parametrise ((check-test-name	'refs-decomposition))

  (check
      (receive (identifiers version-reference)
	  (library-reference-decompose '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (>= 2))))))
	(vector identifiers version-reference))
    => '#( (alpha beta)  (1 (<= 2) (or 10 (and (>= 4) (>= 2)))) ))

;;; --------------------------------------------------------------------

  (check
      (library-reference->identifiers '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (>= 2))))))
    => '(alpha beta))

  (check
      (library-reference->version-reference '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (>= 2))))))
    => '(1 (<= 2) (or 10 (and (>= 4) (>= 2)))))

  #t)


(parametrise ((check-test-name	'refs-predicates))

  (check
      (library-version-reference? '())
    => #t)

  (check
      (library-version-reference? '(1))
    => #t)

  (check
      (library-version-reference? '((1)))
    => #f)

  (check
      (library-version-reference? '(1 2 3))
    => #t)

  (check
      (library-version-reference? '((1) (2) (3)))
    => #f)

  (check
      (library-version-reference? '(and))
    => #t)

  (check
      (library-version-reference? '(and (1)))
    => #t)

  (check
      (library-version-reference? '(and (1) (2)))
    => #t)

  (check
      (library-version-reference? '((and 1)))
    => #t)

  (check
      (library-version-reference? '((and 1 2)))
    => #t)

  (check
      (library-version-reference? '(or))
    => #t)

  (check
      (library-version-reference? '(or (1)))
    => #t)

  (check
      (library-version-reference? '(or (1) (2)))
    => #t)

  (check
      (library-version-reference? '((or 1)))
    => #t)

  (check
      (library-version-reference? '((or 1 2)))
    => #t)

  (check
      (library-version-reference? '(not))
    => #f)

  (check
      (library-version-reference? '(not (1)))
    => #t)

  (check
      (library-version-reference? '(not 1 2))
    => #f)

  (check
      (library-version-reference? '(<= 1))
    => #f)

  (check
      (library-version-reference? '((<=)))
    => #f)

  (check
      (library-version-reference? '((<= 1)))
    => #t)

  (check
      (library-version-reference? '((<= 1 2)))
    => #f)

  (check
      (library-version-reference? '(>= 1))
    => #f)

  (check
      (library-version-reference? '((>=)))
    => #f)

  (check
      (library-version-reference? '((>= 1)))
    => #t)

  (check
      (library-version-reference? '((>= 1 2)))
    => #f)

  (check
      (library-version-reference? '((>= 1) (<= 2) (or 3 4)))
    => #t)

  (check
      (library-version-reference? '(1 (<= 2) (or 10 (and (>= 4) (>= 2)))))
    => #t)

  (check
      (library-version-reference? '(or (1 (<= 1) (not 1))
					   (1 (>= 1) (not 1))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (library-reference? '())
    => #f)

  (check
      (library-reference? '(alpha))
    => #t)

  (check
      (library-reference? '(alpha beta))
    => #t)

  (check
      (library-reference? '(alpha beta gamma))
    => #t)

  (check
      (library-reference? '(alpha beta gamma ()))
    => #t)

  (check
      (library-reference? '(alpha beta gamma (1)))
    => #t)

  (check
      (library-reference? '(alpha beta gamma (1 2 3)))
    => #t)

  (check
      (library-reference? '(alpha 123 gamma))
    => #f)

  (check
      (library-reference? '(alpha beta gamma (1 ciao)))
    => #f)

  (check
      (library-reference? '(alpha ((>= 1) (<= 2) (or 3 4))))
    => #t)

  (check
      (library-reference? '(alpha beta (1 (<= 2) (or 10 (and (>= 4) (<= 2))))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (library-reference-identifiers=? '(alpha beta (1 2))
					   '(alpha beta (1 2)))
    => #t)

  (check
      (library-reference-identifiers=? '(alpha beta (1 2))
					   '(alpha gamma (1 2)))
    => #f)

  (check
      (library-reference-identifiers=? '(alpha beta (1 2))
					   '(alpha beta gamma (1 2)))
    => #f)

  #f)


(parametrise ((check-test-name	'conformance-sub-version))

  (check
      (conforming-sub-version-and-sub-version-reference? 1 1)
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 0)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(<= 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 2 '(<= 1))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 0 '(<= 1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(>= 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 2 '(>= 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 0 '(>= 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(not 1))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(not 0))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 3 '(or))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or 1 2))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or 2 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or 2 3))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or 2 3 4))
    => #f)

  (check
      (conforming-sub-version-and-sub-version-reference? 4 '(or 2 3 4))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 5 '(or 2 (or 3 (or 4 (or 5)))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 3 '(and))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and 1 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and 1 1 1))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and 1 (and 1 (and 1 (and 1)))))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 0 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 2 '(and (>= 0) (<= 2)))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and (>= 0) (<= 2) (not 3)))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(and (>= 0) (<= 2) (not 1)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-sub-version-and-sub-version-reference? 1 '(or (and (>= 0) (<= 2))
								4))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 4 '(or (and (>= 0) (<= 2))
								4))
    => #t)

  (check
      (conforming-sub-version-and-sub-version-reference? 3 '(or (and (>= 0) (<= 2))
								4))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(>=)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(<=)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(not)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(not ciao)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(not)))
    => #t)

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(conforming-sub-version-and-sub-version-reference? 3 '(ciao)))
    => #t)

  #t)


(parametrise ((check-test-name	'conformance-version))

  (check
      (conforming-version-and-version-reference? '() '())
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '())
    => #t)

  (check
      (conforming-version-and-version-reference? '() '(1))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1) '(1))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '(0))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((<= 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(2) '((<= 1)))
    => #f)

  (check
      (conforming-version-and-version-reference? '(0) '((<= 1)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((>= 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(2) '((>= 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(0) '((>= 1)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((not 1)))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1) '((not 0)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((or 1 2)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((or 2 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((or 2 3)))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1) '((or 2 3 4)))
    => #f)

  (check
      (conforming-version-and-version-reference? '(4) '((or 2 3 4)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(5) '((or 2 (or 3 (or 4 (or 5))))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((and 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and 1 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and 1 1 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and 1 (and 1 (and 1 (and 1))))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and (>= 0) (<= 2))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(0) '((and (>= 0) (<= 2))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(2) '((and (>= 0) (<= 2))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and (>= 0) (<= 2) (not 3))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1) '((and (>= 0) (<= 2) (not 1))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1) '((or (and (>= 0) (<= 2))
								4)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(4) '((or (and (>= 0) (<= 2))
								4)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(3) '((or (and (>= 0) (<= 2))
								4)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(1 2 3))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(1 2 4))
    => #f)

  (check
      (conforming-version-and-version-reference? '(1 2 0) '(1 2))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 1) '(1 2))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(1 (>= 1) (not 1)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(1 (<= 1) (not 1)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(or (1 (<= 1) (not 1))
								   (1 (>= 1) (not 1))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(or (1 2 5) (1 2 10)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(and (1 (>= 1) (not 1))
								    (1 2 (not 5))))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(and (1 (<= 1) (not 1))
								    (1 2 (not 5))))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(not (1 5 4)))
    => #t)

  (check
      (conforming-version-and-version-reference? '(1 2 3) '(not (1 2 3)))
    => #f)

  #t)


(parametrise ((check-test-name	'conformance-reference))

  (check
      (conforming-library-name-and-library-reference? '(a (1)) '(a (1)))
    => #t)

  (check
      (conforming-library-name-and-library-reference? '(a (1)) '(a (0)))
    => #f)

  (check
      (conforming-library-name-and-library-reference? '(a b c (1)) '(a b c (1)))
    => #t)

  (check
      (conforming-library-name-and-library-reference? '(a b c (1)) '(a b c (0)))
    => #f)

  (check
      (conforming-library-name-and-library-reference? '(a b c (1)) '(a z c (1)))
    => #f)

  #f)


;;;; done

(check-report)

;;; end of file
