;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for enumerations
;;;Date: Sat Mar 24, 2012
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare enumerations\n")


;;;; helpers

(define environment-for-syntax-errors
  (environment '(rnrs)))

(define environment-for-assertion-errors
  environment-for-syntax-errors)

(define-syntax check-syntax-violation
  (syntax-rules (=>)
    ((_ ?body => ?result)
     (check
	 (guard (E ((syntax-violation? E)
		    (list (condition-message E)
;;;			  (syntax-violation-form E)
			  (syntax->datum (syntax-violation-subform E))))
		   (else E))
	   (eval (quote ?body) environment-for-syntax-errors))
       (=> syntax=?) ?result))))

(define-syntax check-assertion-violation
  (syntax-rules (=>)
    ((_ ?body => ?result)
     (check
	 (guard (E ((assertion-violation? E)
		    (cons (condition-message E)
			  (condition-irritants E)))
		   (else E))
	   (eval (quote ?body) environment-for-assertion-errors))
       => ?result))))


(parametrise ((check-test-name	'syntax-violations))

  (check-syntax-violation ;invalid type name
   (define-enumeration 123 (alpha beta delta) woppa)
   => '("expected identifier as enumeration type name" 123))

  (check-syntax-violation ;invalid constructor nam
   (define-enumeration enum-woppa (alpha beta delta) 123)
   => '("expected identifier as enumeration constructor syntax name" 123))

  (check-syntax-violation ;invalid list of symbols
   (define-enumeration enum-woppa 123 woppa)
   => '("invalid syntax" #f))
;;;   => '("expected list of symbols as enumeration elements" 123))

  (check-syntax-violation ;invalid list of symbols
   (define-enumeration enum-woppa (123) woppa)
   => '("expected list of symbols as enumeration elements" (123)))

  (check-syntax-violation ;invalid list of symbols
   (define-enumeration enum-woppa (alpha beta 123 gamma) woppa)
   => '("expected list of symbols as enumeration elements" (alpha beta 123 gamma)))

;;; --------------------------------------------------------------------

  (check-syntax-violation ;wrong argument to validator
   (let ()
     (define-enumeration enum-woppa (alpha beta gamma) woppa)
     (enum-woppa 123))
   => '("expected symbol as argument to enumeration validator" 123))

  (check-syntax-violation ;invalid symbol to validator
   (let ()
     (define-enumeration enum-woppa (alpha beta gamma) woppa)
     (enum-woppa delta))
   => '("expected symbol in enumeration as argument to enumeration validator" delta))

  (check-syntax-violation ;wrong argument to constructor
   (let ()
     (define-enumeration enum-woppa (alpha beta gamma) woppa)
     (woppa 123))
   => '("expected symbols as arguments to enumeration constructor syntax" 123))

  (check-syntax-violation ;wrong argument to constructor
   (let ()
     (define-enumeration enum-woppa (alpha beta gamma) woppa)
     (woppa alpha beta 123))
   => '("expected symbols as arguments to enumeration constructor syntax" 123))

  (check-syntax-violation ;invalid symbol to constructor
   (let ()
     (define-enumeration enum-woppa (alpha beta gamma) woppa)
     (woppa delta))
   => '("expected symbols in enumeration as arguments to enumeration constructor syntax" (delta)))

  (check-syntax-violation ;invalid symbols to constructor
   (let ()
     (define-enumeration enum-woppa (alpha beta gamma) woppa)
     (woppa delta zeta))
   => '("expected symbols in enumeration as arguments to enumeration constructor syntax" (delta zeta)))

  (check-syntax-violation ;invalid symbol to constructor
   (let ()
     (define-enumeration enum-woppa (alpha beta gamma) woppa)
     (woppa alpha beta delta))
   => '("expected symbols in enumeration as arguments to enumeration constructor syntax"
	(delta)))

  #t)


(parametrise ((check-test-name	'assertion-violations))

  (check-assertion-violation
   (make-enumeration 123)
   => '("expected list of symbols as argument" 123))

  (check-assertion-violation
   (make-enumeration '(123))
   => '("expected list of symbols as argument" (123)))

  (check-assertion-violation
   (make-enumeration '(alpha beta 123 gamma))
   => '("expected list of symbols as argument" (alpha beta 123 gamma)))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-universe 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-indexer 123)
   => '("expected enumeration set as argument" 123))

  (check-assertion-violation
   (let* ((S (make-enumeration '(a b c)))
	  (I (enum-set-indexer S)))
     (I 123))
   => '("expected symbol as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-constructor 123)
   => '("expected enumeration set as argument" 123))

  (check-assertion-violation
   (let* ((S (make-enumeration '(a b c)))
	  (C (enum-set-constructor S)))
     (C 123))
   => '("expected list of symbols as argument" 123))

  (check-assertion-violation
   (let* ((S (make-enumeration '(a b c)))
	  (C (enum-set-constructor S)))
     (C '(a b 123)))
   => '("expected list of symbols as argument" (a b 123)))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set->list 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (let ((A (make-enumeration '(a b c))))
     (enum-set-member? 123 A))
   => '("expected symbol as argument" 123))

  (check-assertion-violation
   (enum-set-member? 'ciao 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-subset? 123 (make-enumeration '(a b c)))
   => '("expected enumeration set as argument" 123))

  (check-assertion-violation
   (enum-set-subset? (make-enumeration '(a b c)) 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set=? 123 (make-enumeration '(a b c)))
   => '("expected enumeration set as argument" 123))

  (check-assertion-violation
   (enum-set=? (make-enumeration '(a b c)) 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-union 123 (make-enumeration '(a b c)))
   => '("expected enumeration set as argument" 123))

  (check-assertion-violation
   (enum-set-union (make-enumeration '(a b c)) 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-intersection 123 (make-enumeration '(a b c)))
   => '("expected enumeration set as argument" 123))

  (check-assertion-violation
   (enum-set-intersection (make-enumeration '(a b c)) 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-difference 123 (make-enumeration '(a b c)))
   => '("expected enumeration set as argument" 123))

  (check-assertion-violation
   (enum-set-difference (make-enumeration '(a b c)) 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-complement 123)
   => '("expected enumeration set as argument" 123))

;;; --------------------------------------------------------------------

  (check-assertion-violation
   (enum-set-projection 123 (make-enumeration '(a b c)))
   => '("expected enumeration set as argument" 123))

  (check-assertion-violation
   (enum-set-projection (make-enumeration '(a b c)) 123)
   => '("expected enumeration set as argument" 123))

  #t)


;;;; done

(check-report)

;;; end of file
