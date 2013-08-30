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


;;;; done

(check-report)

;;; end of file
