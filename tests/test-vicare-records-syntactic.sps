;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for R6RS records, syntactic layer
;;;Date: Thu Mar 22, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(program (test-vicare-records-syntactic)
  (import (vicare)
    (vicare language-extensions syntaxes)
    (vicare system $structs)
    (libtest records-lib)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare R6RS records, syntactic layer\n")


(parametrise ((check-test-name	'definition))

  (check	;safe accessors
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	(list (color-red   X)
	      (color-green X)
	      (color-blue  X)))
    => '(1 2 3))

  (check	;safe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	(color-red-set!   X 10)
	(color-green-set! X 20)
	(color-blue-set!  X 30)
	(list (color-red   X)
	      (color-green X)
	      (color-blue  X)))
    => '(10 20 30))

  (check	;safe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red   the-red   set-the-red!)
		  (mutable green the-green set-the-green!)
		  (mutable blue  the-blue  set-the-blue!)))
	(define X
	  (make-color 1 2 3))
	(set-the-red!   X 10)
	(set-the-green! X 20)
	(set-the-blue!  X 30)
	(list (the-red   X)
	      (the-green X)
	      (the-blue  X)))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'protocol))

  ;;Record-type without parent.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b c)
	  (protocol
	   (lambda (alpha-initialiser)
	     (lambda (a b c)
	       (receive-and-return (R)
		   (alpha-initialiser a b c)
		 (assert (alpha? R)))))))

	(let ((R (make-alpha 1 2 3)))
	  (values (alpha-a R)
		  (alpha-b R)
		  (alpha-c R))))
    => 1 2 3)

  ;;Record-type with single parent.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b c)
	  (protocol
	   (lambda (alpha-initialiser)
	     (lambda (a b c)
	       (receive-and-return (R)
		   (alpha-initialiser a b c)
		 (assert (alpha? R)))))))

	(define-record-type beta
	  (parent alpha)
	  (fields d e f)
	  (protocol
	   (lambda (alpha-maker)
	     (lambda (a b c d e f)
	       (receive-and-return (R)
		   ((alpha-maker a b c) d e f)
		 (assert (beta? R)))))))

	(let ((R (make-beta 1 2 3 4 5 6)))
	  (values (alpha-a R)
		  (alpha-b R)
		  (alpha-c R)
		  (beta-d R)
		  (beta-e R)
		  (beta-f R))))
    => 1 2 3 4 5 6)

  ;;Record-type with double parent.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b c)
	  (protocol
	   (lambda (alpha-initialiser)
	     (lambda (a b c)
	       (receive-and-return (R)
		   (alpha-initialiser a b c)
		 (assert (alpha? R)))))))

	(define-record-type beta
	  (parent alpha)
	  (fields d e f)
	  (protocol
	   (lambda (alpha-maker)
	     (lambda (a b c d e f)
	       (receive-and-return (R)
		   ((alpha-maker a b c) d e f)
		 (assert (beta? R)))))))

	(define-record-type gamma
	  (parent beta)
	  (fields g h i)
	  (protocol
	   (lambda (beta-maker)
	     (lambda (a b c d e f g h i)
	       (receive-and-return (R)
		   ((beta-maker a b c d e f) g h i)
		 (assert (gamma? R)))))))

	(let ((R (make-gamma 1 2 3 4 5 6 7 8 9)))
	  (values (alpha-a R)
		  (alpha-b R)
		  (alpha-c R)
		  (beta-d R)
		  (beta-e R)
		  (beta-f R)
		  (gamma-g R)
		  (gamma-h R)
		  (gamma-i R))))
    => 1 2 3 4 5 6 7 8 9)

  #t)


(parametrise ((check-test-name	'unsafe-accessors))

  (check	;unsafe accessors
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	(list ($color-red   X)
	      ($color-green X)
	      ($color-blue  X)))
    => '(1 2 3))

  (check	;unsafe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	($color-red-set!   X 10)
	($color-green-set! X 20)
	($color-blue-set!  X 30)
	(list ($color-red   X)
	      ($color-green X)
	      ($color-blue  X)))
    => '(10 20 30))

  (check	;unsafe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red   the-red   set-the-red!)
		  (mutable green the-green set-the-green!)
		  (mutable blue  the-blue  set-the-blue!)))
	(define X
	  (make-color 1 2 3))
	($color-red-set!   X 10)
	($color-green-set! X 20)
	($color-blue-set!  X 30)
	(list ($color-red   X)
	      ($color-green X)
	      ($color-blue  X)))
    => '(10 20 30))

;;; --------------------------------------------------------------------

  (check	;unsafe accessors, with inheritance
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(list ($alpha-a O)
	      ($alpha-b O)
	      ($alpha-c O)
	      ($beta-a O)
	      ($beta-b O)
	      ($beta-c O)))
    => '(1 2 3 4 5 6))

  (check	;unsafe accessors and mutators, with inheritance
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define O
	  (make-beta 1 2 3 4 5 6))
	($alpha-a-set! O 10)
	($alpha-b-set! O 20)
	($alpha-c-set! O 30)
	($beta-a-set! O 40)
	($beta-b-set! O 50)
	($beta-c-set! O 60)
	(list ($alpha-a O)
	      ($alpha-b O)
	      ($alpha-c O)
	      ($beta-a O)
	      ($beta-b O)
	      ($beta-c O)))
    => '(10 20 30 40 50 60))

  (check	;unsafe accessors and mutators, with inheritance
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define O
	  (make-beta 1 2 3 4 5 6))
	($alpha-a-set! O 10)
	#;($alpha-b-set! O 20)
	($alpha-c-set! O 30)
	($beta-a-set! O 40)
	#;($beta-b-set! O 50)
	($beta-c-set! O 60)
	(list ($alpha-a O)
	      ($alpha-b O)
	      ($alpha-c O)
	      ($beta-a O)
	      ($beta-b O)
	      ($beta-c O)))
    => '(10 2 30 40 5 60))

  (check	;unsafe accessors and mutators, with inheritance
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define-record-type gamma
	  (parent beta)
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define O
	  (make-gamma 1 2 3 4 5 6 7 8 9))
	($alpha-a-set! O 10)
	#;($alpha-b-set! O 20)
	($alpha-c-set! O 30)
	($beta-a-set! O 40)
	#;($beta-b-set! O 50)
	($beta-c-set! O 60)
	($gamma-a-set! O 70)
	#;($gamma-b-set! O 80)
	($gamma-c-set! O 90)
	(list ($alpha-a O)
	      ($alpha-b O)
	      ($alpha-c O)
	      ($beta-a O)
	      ($beta-b O)
	      ($beta-c O)
	      ($gamma-a O)
	      ($gamma-b O)
	      ($gamma-c O)))
    => '(10 2 30 40 5 60 70 8 90))

  #t)


(parametrise ((check-test-name	'record-type-field))

;;; record-type-field-names

  (check
      (internal-body
	(define-record-type alpha)
	(record-type-field-names (record-type-descriptor alpha)))
    => '#())

  (check
      (internal-body
	(define-record-type alpha
	  (fields a b c))
	(record-type-field-names (record-type-descriptor alpha)))
    => '#(a b c))

  (check
      (internal-body
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(record-type-field-names (record-type-descriptor beta)))
    => '#(d e f))

  (check
      (internal-body
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(define-record-type gamma
	  (parent beta)
	  (fields g h i))
	(record-type-field-names (record-type-descriptor gamma)))
    => '#(g h i))

;;; --------------------------------------------------------------------
;;; record-type-all-field-names

  (check
      (internal-body
	(define-record-type alpha)
	(record-type-all-field-names (record-type-descriptor alpha)))
    => '#())

  (check
      (internal-body
	(define-record-type alpha
	  (fields a b c))
	(record-type-all-field-names (record-type-descriptor alpha)))
    => '#(a b c))

  (check
      (internal-body
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(record-type-all-field-names (record-type-descriptor beta)))
    => '#(a b c d e f))

  (check
      (internal-body
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(define-record-type gamma
	  (parent beta)
	  (fields g h i))
	(record-type-all-field-names (record-type-descriptor gamma)))
    => '#(a b c d e f g h i))

  (check
      (internal-body
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(define-record-type gamma
	  (parent beta)
	  (fields g h i))
	(define fields
	  (record-type-all-field-names (record-type-descriptor gamma)))
	(define O
	  (make-gamma 1 2 3 4 5 6 7 8 9))
	(let recur ((len (vector-length fields))
		    (i   0))
	  (if (fx<? i len)
	      (cons (list (vector-ref fields i)
			  (struct-ref O i))
		    (recur len (fxadd1 i)))
	    '())))
    => '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9)))

;;; --------------------------------------------------------------------

  (check	;safe accessors
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	(list (slot-ref X red   color)
	      (slot-ref X green color)
	      (slot-ref X blue  color)))
    => '(1 2 3))

  (check	;safe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	(slot-set! X red   color 10)
	(slot-set! X green color 20)
	(slot-set! X blue  color 30)
	(list (slot-ref X red   color)
	      (slot-ref X green color)
	      (slot-ref X blue  color)))
    => '(10 20 30))

  (check	;safe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red   the-red   set-the-red!)
		  (mutable green the-green set-the-green!)
		  (mutable blue  the-blue  set-the-blue!)))
	(define X
	  (make-color 1 2 3))
	(slot-set! X red   color 10)
	(slot-set! X green color 20)
	(slot-set! X blue  color 30)
	(list (slot-ref X red   color)
	      (slot-ref X green color)
	      (slot-ref X blue  color)))
    => '(10 20 30))

;;; --------------------------------------------------------------------

  (check	;safe accessors, with inheritance
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(list (slot-ref O a alpha)
	      (slot-ref O b alpha)
	      (slot-ref O c alpha)
	      (slot-ref O a beta)
	      (slot-ref O b beta)
	      (slot-ref O c beta)))
    => '(1 2 3 4 5 6))

  (check	;safe accessors, with inheritance, sub-rtd access
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(list (slot-ref O a beta)
	      (slot-ref O b beta)
	      (slot-ref O c beta)
	      (slot-ref O d beta)
	      (slot-ref O e beta)
	      (slot-ref O f beta)))
    => '(1 2 3 4 5 6))

  (check	;safe accessors and mutators, with inheritance, sub-rtd access
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(slot-set! O a beta 19)
	(slot-set! O b beta 29)
	(slot-set! O c beta 39)
	(slot-set! O d beta 49)
	(slot-set! O e beta 59)
	(slot-set! O f beta 69)
	(list (slot-ref O a beta)
	      (slot-ref O b beta)
	      (slot-ref O c beta)
	      (slot-ref O d beta)
	      (slot-ref O e beta)
	      (slot-ref O f beta)))
    => '(19 29 39 49 59 69))

  (check	;safe accessors and mutators, with inheritance
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(slot-set! O a alpha 10)
	(slot-set! O b alpha 20)
	(slot-set! O c alpha 30)
	(slot-set! O a beta 40)
	(slot-set! O b beta 50)
	(slot-set! O c beta 60)
	(list (slot-ref O a alpha)
	      (slot-ref O b alpha)
	      (slot-ref O c alpha)
	      (slot-ref O a beta)
	      (slot-ref O b beta)
	      (slot-ref O c beta)))
    => '(10 20 30 40 50 60))

  (check	;safe accessors and mutators, with inheritance
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(slot-set! O a alpha 10)
	#;(slot-set! O b alpha 20)
	(slot-set! O c alpha 30)
	(slot-set! O a beta 40)
	#;(slot-set! O b beta 50)
	(slot-set! O c beta 60)
	(list (slot-ref O a alpha)
	      (slot-ref O b alpha)
	      (slot-ref O c alpha)
	      (slot-ref O a beta)
	      (slot-ref O b beta)
	      (slot-ref O c beta)))
    => '(10 2 30 40 5 60))

  (check	;safe accessors and mutators, with inheritance
      (let ()
	(define-record-type alpha
	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
	(define-record-type gamma
	  (parent beta)
	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
	(define O
	  (make-gamma 1 2 3 4 5 6 7 8 9))
	(slot-set! O a alpha 10)
	#;(slot-set! O b alpha 20)
	(slot-set! O c alpha 30)
	;;
	(slot-set! O a beta 40)
	#;(slot-set! O b beta 50)
	(slot-set! O c beta 60)
	;;
	(slot-set! O a gamma 70)
	#;(slot-set! O b gamma 80)
	(slot-set! O c gamma 90)
	(list (slot-ref O a alpha)
	      (slot-ref O b alpha)
	      (slot-ref O c alpha)
	      (slot-ref O a beta)
	      (slot-ref O b beta)
	      (slot-ref O c beta)
	      (slot-ref O a gamma)
	      (slot-ref O b gamma)
	      (slot-ref O c gamma)))
    => '(10 2 30 40 5 60 70 8 90))

;;; --------------------------------------------------------------------
;;; here we use records from the library (libtest records-lib)

  (check	;safe accessors
      (let ((X (make-<alpha> 1 2)))
	(list (slot-ref X one <alpha>)
	      (slot-ref X two <alpha>)))
    => '(1 2))

  (check	;safe accessors and mutators
      (let ((X (make-<alpha> 1 2)))
	(slot-set! X one <alpha> 10)
	(list (slot-ref X one <alpha>)
	      (slot-ref X two <alpha>)))
    => '(10 2))

  (check	;safe accessors
      (let ((X (make-<gamma> 1 2 3 4)))
	(list (slot-ref X one <beta>)
	      (slot-ref X two <beta>)
	      (slot-ref X three <gamma>)
	      (slot-ref X four  <gamma>)))
    => '(1 2 3 4))

  (check	;safe accessors and mutators
      (let ((X (make-<gamma> 1 2 3 4)))
	(slot-set! X one   <beta>  10)
	(slot-set! X three <gamma> 30)
	(list (slot-ref X one <beta>)
	      (slot-ref X two <beta>)
	      (slot-ref X three <gamma>)
	      (slot-ref X four  <gamma>)))
    => '(10 2 30 4))

  #t)


(parametrise ((check-test-name	'record-accessor-constructor))

  (check	;record accessor constructor with symbol argument
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define alpha-rtd
	  (record-type-descriptor alpha))
	(define R
	  (make-alpha 1 2 3))
	(list ((record-accessor alpha-rtd 'a) R)
	      ((record-accessor alpha-rtd 'b) R)
	      ((record-accessor alpha-rtd 'c) R)))
    => '(1 2 3))

  (check	;Record  accessor constructor  with  symbol argument;  a
		;field in ALPHA has the same name of a field in BETA.
      (let ()
	(define-record-type alpha
	  (fields a b C))
	(define-record-type beta
	  (parent alpha)
	  (fields C d e))
	(define beta-rtd
	  (record-type-descriptor beta))
	(define R
	  (make-beta 1 2 3 4 5 6))
	(list ((record-accessor beta-rtd 'a) R)
	      ((record-accessor beta-rtd 'b) R)
	      ((record-accessor beta-rtd 'C) R)
	      ((record-accessor beta-rtd 'd) R)
	      ((record-accessor beta-rtd 'e) R)))
    => '(1 2 4 5 6))

  #t)


(parametrise ((check-test-name	'record-mutator-constructor))

  (check	;record mutator constructor with symbol argument
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define alpha-rtd
	  (record-type-descriptor alpha))
	(define R
	  (make-alpha 1 2 3))
	((record-mutator alpha-rtd 'a) R 19)
	((record-mutator alpha-rtd 'b) R 29)
	((record-mutator alpha-rtd 'c) R 39)
	(list ((record-accessor alpha-rtd 'a) R)
	      ((record-accessor alpha-rtd 'b) R)
	      ((record-accessor alpha-rtd 'c) R)))
    => '(19 29 39))

  (check	;Record  accessor constructor  with  symbol argument;  a
		;field in ALPHA has the same name of a field in BETA.
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)
		  (mutable C)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable C)
		  (mutable d)
		  (mutable e)))
	(define beta-rtd
	  (record-type-descriptor beta))
	(define R
	  (make-beta 1 2 3 4 5 6))
	((record-mutator beta-rtd 'a) R 19)
	((record-mutator beta-rtd 'b) R 29)
	((record-mutator beta-rtd 'C) R 49)
	((record-mutator beta-rtd 'd) R 59)
	((record-mutator beta-rtd 'e) R 69)
	(list ((record-accessor beta-rtd 'a) R)
	      ((record-accessor beta-rtd 'b) R)
	      ((record-accessor beta-rtd 'C) R)
	      ((record-accessor beta-rtd 'd) R)
	      ((record-accessor beta-rtd 'e) R)))
    => '(19 29 49 59 69))

  #t)


(parametrise ((check-test-name	'predicates))

  (check
      (let ()
	(define-record-type alpha
	  (fields a))
	(define A
	  (make-alpha 1))
	(record-and-rtd? A (record-type-descriptor alpha)))
    => #t)

  (check
      (let ()
	(define-record-type alpha
	  (fields a))
	(define-record-type beta
	  (parent alpha)
	  (fields b))
	(define B
	  (make-beta 1 2))
	(list (record-and-rtd? B (record-type-descriptor alpha))
	      (record-and-rtd? B (record-type-descriptor beta))))
    => '(#t #t))

  (check
      (let ()
	(define-record-type alpha
	  (fields a))
	(define-record-type beta
	  (parent alpha)
	  (fields b))
	(define A
	  (make-alpha 1))
	(list (record-and-rtd? A (record-type-descriptor alpha))
	      (record-and-rtd? A (record-type-descriptor beta))))
    => '(#t #f))

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define-record-type alpha
	  (fields a))
	(define A
	  (make-alpha 1))
	(record-type-and-record? alpha A))
    => #t)

  (check
      (let ()
	(define-record-type alpha
	  (fields a))
	(define-record-type beta
	  (parent alpha)
	  (fields b))
	(define B
	  (make-beta 1 2))
	(list (record-type-and-record? alpha B)
	      (record-type-and-record? beta  B)))
    => '(#t #t))

  (check
      (let ()
	(define-record-type alpha
	  (fields a))
	(define-record-type beta
	  (parent alpha)
	  (fields b))
	(define A
	  (make-alpha 1))
	(list (record-type-and-record? alpha A)
	      (record-type-and-record? beta  A)))
    => '(#t #f))

  ;;Opaque record type.
  ;;
  (check
      (let ()
	(define-record-type alpha
	  (opaque #t)
	  (fields a))
	(define A
	  (make-alpha 1))
	(record-type-and-record? alpha A))
    => #t)

  #t)


(parametrise ((check-test-name	'generic-rtd-syntax))

  (let ()	;application syntax
    (define-record-type alpha
      (fields a b c))

    (check
	(eq? (record-type-descriptor alpha)
	     (type-descriptor alpha))
      => #t)

    (void))

  #t)


(parametrise ((check-test-name	'generic-maker-syntax))

  (let ()	;application syntax
    (define-record-type alpha
      (fields a b c))

    (define-record-type beta
      (fields a b))

    (check
	(let ((reco (alpha (1 2 3))))
	  (alpha? reco))
      => #t)

    (check
	(let ((reco (beta (1 2))))
	  (beta? reco))
      => #t)

    (void))

  (let ()	;reference syntax
    (define-record-type alpha
      (fields a b c))

    (define-record-type beta
      (fields a b))

    (check
	(let ((reco (apply (alpha (...)) 1 '(2 3))))
	  (alpha? reco))
      => #t)

    (check
	(let ((reco (apply (beta (...)) '(1 2))))
	  (beta? reco))
      => #t)

    (void))

  #t)


(parametrise ((check-test-name	'generic-predicate-syntax))

  (let ()
    (define-record-type alpha
      (fields a b c))

    (define-record-type beta
      (fields a b c))

    (check
	(let ((stru (make-alpha 1 2 3)))
	  (is-a? stru alpha))
      => #t)

    (check
	(let ((stru (make-alpha 1 2 3)))
	  (is-a? stru beta))
      => #f)

    (check
	(let ((stru (make-alpha 1 2 3)))
	  ((is-a? _ alpha) stru))
      => #t)

    (check
	(is-a? 123 alpha)
      => #f)

    (check
	(is-a? 123 beta)
      => #f)

    (void))

  #t)


(parametrise ((check-test-name	'generic-slots-syntax))

  (define-record-type alpha
    (fields (mutable a)
	    (mutable b)
	    (mutable c)))

  (define-record-type beta
    (parent alpha)
    (fields (mutable d)
	    (mutable e)
	    (mutable f)))

  (define-record-type gamma
    (parent beta)
    (fields (mutable g)
	    (mutable h)
	    (mutable i)))

;;; --------------------------------------------------------------------
;;; accessors and mutators, no parent

  (check
      (let ((stru (new alpha 1 2 3)))
	(list (slot-ref stru a alpha)
	      (slot-ref stru b alpha)
	      (slot-ref stru c alpha)))
    => '(1 2 3))

  (check
      (let ((stru (new alpha 1 2 3)))
	(slot-set! stru a alpha 19)
	(slot-set! stru b alpha 29)
	(slot-set! stru c alpha 39)
	(list (slot-ref stru a alpha)
	      (slot-ref stru b alpha)
	      (slot-ref stru c alpha)))
    => '(19 29 39))

  (check
      (let ((stru (new alpha 1 2 3)))
	(list ((slot-ref <> a alpha) stru)
	      ((slot-ref <> b alpha) stru)
	      ((slot-ref <> c alpha) stru)))
    => '(1 2 3))

  (check
      (let ((stru (new alpha 1 2 3)))
	((slot-set! <> a alpha <>) stru 19)
	((slot-set! <> b alpha <>) stru 29)
	((slot-set! <> c alpha <>) stru 39)
	(list ((slot-ref <> a alpha) stru)
	      ((slot-ref <> b alpha) stru)
	      ((slot-ref <> c alpha) stru)))
    => '(19 29 39))

  (check
      (let ((stru (new alpha 1 2 3)))
	(list ((slot-ref _ a alpha) stru)
	      ((slot-ref _ b alpha) stru)
	      ((slot-ref _ c alpha) stru)))
    => '(1 2 3))

  (check
      (let ((stru (new alpha 1 2 3)))
	((slot-set! _ a alpha _) stru 19)
	((slot-set! _ b alpha _) stru 29)
	((slot-set! _ c alpha _) stru 39)
	(list ((slot-ref _ a alpha) stru)
	      ((slot-ref _ b alpha) stru)
	      ((slot-ref _ c alpha) stru)))
    => '(19 29 39))

;;; --------------------------------------------------------------------
;;; accessors and mutators, parent

  (check
      (let ((stru (new beta 1 2 3 4 5 6)))
	(values (slot-ref stru a alpha)
		(slot-ref stru b alpha)
		(slot-ref stru c alpha)
		(slot-ref stru a beta)
		(slot-ref stru b beta)
		(slot-ref stru c beta)
		(slot-ref stru d beta)
		(slot-ref stru e beta)
		(slot-ref stru f beta)))
    => 1 2 3 1 2 3 4 5 6)

  (check
      (let ((stru (new beta 1 2 3 4 5 6)))
	(slot-set! stru a beta 10)
	(slot-set! stru b beta 20)
	(slot-set! stru c beta 30)
	(slot-set! stru d beta 40)
	(slot-set! stru e beta 50)
	(slot-set! stru f beta 60)
	(values (slot-ref stru a beta)
		(slot-ref stru b beta)
		(slot-ref stru c beta)
		(slot-ref stru d beta)
		(slot-ref stru e beta)
		(slot-ref stru f beta)))
    => 10 20 30 40 50 60)

;;; --------------------------------------------------------------------
;;; accessors and mutators, parent and grand-parent

  (check
      (let ((stru (new gamma 1 2 3 4 5 6 7 8 9)))
	(values (slot-ref stru a alpha)
		(slot-ref stru b alpha)
		(slot-ref stru c alpha)
		;;
		(slot-ref stru a beta)
		(slot-ref stru b beta)
		(slot-ref stru c beta)
		(slot-ref stru d beta)
		(slot-ref stru e beta)
		(slot-ref stru f beta)
		;;
		(slot-ref stru a gamma)
		(slot-ref stru b gamma)
		(slot-ref stru c gamma)
		(slot-ref stru d gamma)
		(slot-ref stru e gamma)
		(slot-ref stru f gamma)
		(slot-ref stru g gamma)
		(slot-ref stru h gamma)
		(slot-ref stru i gamma)))
    => 1 2 3
    1 2 3 4 5 6
    1 2 3 4 5 6 7 8 9)

;;; QUALCOSA QUI PROVOCA UN CRASH!!!

  (check
      (let ((stru (new gamma 1 2 3 4 5 6 7 8 9)))
	(slot-set! stru a gamma 10)
	(slot-set! stru b gamma 20)
	(slot-set! stru c gamma 30)
	(slot-set! stru d gamma 40)
	(slot-set! stru e gamma 50)
	(slot-set! stru f gamma 60)
	(slot-set! stru g gamma 70)
	(slot-set! stru h gamma 80)
	(slot-set! stru i gamma 90)
	(values (slot-ref stru a gamma)
		(slot-ref stru b gamma)
		(slot-ref stru c gamma)
		(slot-ref stru d gamma)
		(slot-ref stru e gamma)
		(slot-ref stru f gamma)
		(slot-ref stru g gamma)
		(slot-ref stru h gamma)
		(slot-ref stru i gamma)))
    => 10 20 30 40 50 60 70 80 90)

  (check
      (let ((stru (new gamma 1 2 3 4 5 6 7 8 9)))
	(slot-set! stru a alpha 10)
	(slot-set! stru b alpha 20)
	(slot-set! stru c alpha 30)
	(slot-set! stru d beta 40)
	(slot-set! stru e beta 50)
	(slot-set! stru f beta 60)
	(slot-set! stru g gamma 70)
	(slot-set! stru h gamma 80)
	(slot-set! stru i gamma 90)
	(values (slot-ref stru a alpha)
		(slot-ref stru b alpha)
		(slot-ref stru c alpha)
		(slot-ref stru d beta)
		(slot-ref stru e beta)
		(slot-ref stru f beta)
		(slot-ref stru g gamma)
		(slot-ref stru h gamma)
		(slot-ref stru i gamma)))
    => 10 20 30 40 50 60 70 80 90)

  #t)


(parametrise ((check-test-name	'equality))

  (define-record-type <alpha>
    (fields a b c))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((P (make-<alpha> 1 2 3)))
     (record=? P P)))

  (check-for-true
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 3)))
     (record=? P Q)))

  (check-for-false
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 9)))
     (record=? P Q)))

;;; --------------------------------------------------------------------
;;; STRUCT=? works on records

  (check-for-true
   (let ((P (make-<alpha> 1 2 3)))
     (struct=? P P)))

  (check-for-true
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 3)))
     (struct=? P Q)))

  (check-for-false
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 9)))
     (struct=? P Q)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((P (make-<alpha> 1 2 3)))
     (equal? P P)))

  (check-for-true
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 3)))
     (equal? P Q)))

  (check-for-false
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 9)))
     (equal? P Q)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((P (make-<alpha> 1 2 3)))
     (eqv? P P)))

  (check-for-false
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 3)))
     (eqv? P Q)))

  (check-for-false
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 9)))
     (eqv? P Q)))

  #t)


(parametrise ((check-test-name	'reset))

  (define-record-type <alpha>
    (fields a b c))

  (check
      (let ((R (make-<alpha> 1 2 3)))
	(record-reset R)
	(list (<alpha>-a R)
	      (<alpha>-b R)
	      (<alpha>-c R)))
    => (list (void) (void) (void)))

  (check
      (guard (E ((assertion-violation? E)
		 (condition-who E))
		(else E))
	(record-reset 123))
    => 'record-reset)

  #t)


(parametrise ((check-test-name		'destructor)
	      (record-guardian-logger	(lambda (S E action)
					  (check-pretty-print (list S E action)))))

  (module ()	;example for the documentation

    (define-record-type <alpha>
      (fields a b c))

    (record-type-destructor-set! (record-type-descriptor <alpha>)
				 (lambda (S)
				   (pretty-print (list 'finalising S)
						 (current-error-port))))

    (parametrise ((record-guardian-logger #f))
      (pretty-print (make-<alpha> 1 2 3) (current-error-port))
      (collect))

    #f)

;;; --------------------------------------------------------------------

  (internal-body

    (define-record-type <alpha>
      (fields a b c))

    (record-type-destructor-set! (record-type-descriptor <alpha>)
				 (lambda (S)
				   (void)))

    (check
	(procedure? (record-type-destructor (record-type-descriptor <alpha>)))
      => #t)

    (check
	(parametrise ((record-guardian-logger #t))
	  (let ((S (make-<alpha> 1 2 3)))
	    (check-pretty-print S)
	    (collect)))
      => (void))

    (check
	(let ((S (make-<alpha> 1 2 3)))
	  (check-pretty-print S)
	  (collect))
      => (void))

    (check
	(let ((S (make-<alpha> 1 2 3)))
	  (check-pretty-print S)
	  (collect))
      => (void))

    (void))

  (collect))


(parametrise ((check-test-name		'custom-printer))

  (check
      (internal-body
	(define-record-type duo
	  (fields one two)
	  (custom-printer
	    (lambda (record port sub-printer)
	      (display "#{record duo one=" port)
	      (display (duo-one record) port)
	      (display " two=" port)
	      (display (duo-two record) port)
	      (display "}" port))))

	(receive (port extract)
	    (open-string-output-port)
	  (display (make-duo 1 2) port)
	  (extract)))
    => "#{record duo one=1 two=2}")

  (check-for-assertion-violation
      (internal-body
	(define-record-type duo
	  (fields one two)
	  (custom-printer
	    123))
	(void))
    => '(duo (123)))

  (void))


(parametrise ((check-test-name		'destructor-protocol)
	      (record-guardian-logger	(lambda (S E action)
					  (check-pretty-print (list S E action)))))

  ;;No parent, no destructor.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields one two))
	(define O
	  (make-duo 1 2))
	(delete O))
    => '#!void)

  ;;No parent, this type with destructor.
  ;;
  (check
      (with-result
	(internal-body
	  (define-record-type duo
	    (fields one two)
	    (destructor-protocol
	      (lambda ()
		(lambda (record)
		  (add-result 'duo-destructor)))))
	  (define O
	    (make-duo 1 2))
	  (delete O)))
    => '(duo-destructor (duo-destructor)))

  ;;No parent, this type with destructor.
  ;;
  (check
      (with-result
	(internal-body
	  (define-record-type duo
	    (fields one two)
	    (destructor-protocol
	      (lambda ()
		(lambda (record)
		  (add-result 'duo-destructor)))))
	  (delete (make-duo 1 2))))
    => '(duo-destructor (duo-destructor)))

;;; --------------------------------------------------------------------
;;; parent specified with PARENT clause

  ;;Parent with no destructor, this type with destructor.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b))

	  (define-record-type beta
	    (parent alpha)
	    (fields c d)
	    (destructor-protocol
	      (lambda (destroy-alpha)
		(lambda (record)
		  (destroy-alpha record)
		  (add-result 'beta-destructor)))))

	  (define O
	    (make-beta 1 2 3 4))

	  (delete O)))
    => '(beta-destructor (beta-destructor)))

  ;;Both this type and its parent with destructor.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b)
	    (destructor-protocol
	      (lambda ()
		(lambda (record)
		  (add-result 'alpha-destructor)))))

	  (define-record-type beta
	    (parent alpha)
	    (fields c d)
	    (destructor-protocol
	      (lambda (destroy-alpha)
		(lambda (record)
		  (destroy-alpha record)
		  (add-result 'beta-destructor)))))

	  (define O
	    (make-beta 1 2 3 4))

	  (delete O)))
    => '(beta-destructor (alpha-destructor beta-destructor)))

  ;;Parent with destructor, this type with no destructor.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b)
	    (destructor-protocol
	      (lambda ()
		(lambda (record)
		  (add-result 'alpha-destructor)))))

	  (define-record-type beta
	    (parent alpha)
	    (fields c d))

	  (define O
	    (make-beta 1 2 3 4))

	  (delete O)))
    => '(alpha-destructor (alpha-destructor)))

;;; --------------------------------------------------------------------
;;; parent specified with PARENT-RTD clause

  ;;Parent with no destructor, this type with destructor.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b))

	  (define-record-type beta
	    (parent-rtd (record-type-descriptor alpha)
			(record-constructor-descriptor alpha))
	    (fields c d)
	    (destructor-protocol
	      (lambda (destroy-alpha)
		(lambda (record)
		  (destroy-alpha record)
		  (add-result 'beta-destructor)))))

	  (define O
	    (make-beta 1 2 3 4))

	  (delete O)))
    => '(beta-destructor (beta-destructor)))

  ;;Both this type and its parent with destructor.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b)
	    (destructor-protocol
	      (lambda ()
		(lambda (record)
		  (add-result 'alpha-destructor)))))

	  (define-record-type beta
	    (parent-rtd (record-type-descriptor alpha)
			(record-constructor-descriptor alpha))
	    (fields c d)
	    (destructor-protocol
	      (lambda (destroy-alpha)
		(lambda (record)
		  (destroy-alpha record)
		  (add-result 'beta-destructor)))))

	  (define O
	    (make-beta 1 2 3 4))

	  (delete O)))
    => '(beta-destructor (alpha-destructor beta-destructor)))

  ;;Parent with destructor, this type with no destructor.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b)
	    (destructor-protocol
	      (lambda ()
		(lambda (record)
		  (add-result 'alpha-destructor)))))

	  (define-record-type beta
	    (parent-rtd (record-type-descriptor alpha)
			(record-constructor-descriptor alpha))
	    (fields c d))

	  (define O
	    (make-beta 1 2 3 4))

	  (delete O)))
    => '(alpha-destructor (alpha-destructor)))

  (collect))


(parametrise ((check-test-name	'misc))

  (let ()
    (define-record-type <alpha>
      (nongenerative ciao-hello-ciao-1)
      (fields a))

    (check
	(record-rtd (make-<alpha> 1))
      => (record-type-descriptor <alpha>))

    #f)

  #t)


(parametrise ((check-test-name	'bugs))

  (check-for-expression-return-value-violation
      (internal-body
	(define-record-type alpha
	  (fields a)
	  (protocol (lambda (maker)
		      (void))))
	(make-alpha 1))
    => '(make-record-constructor-descriptor (#!void)))

  #t)


;;;; done

(collect 4)
(check-report)

#| end of program |# )

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
