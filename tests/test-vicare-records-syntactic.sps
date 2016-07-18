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
;;;Copyright (C) 2012-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    (vicare system $structs)
    (libtest records-lib)
    (prefix (vicare expander) expander::)
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

  #t)


(parametrise ((check-test-name	'multiple-fields-clauses))

  (check
      (internal-body
	(define-record-type alpha
	  (fields a)
	  (fields b)
	  (fields c))
	(define O
	  (make-alpha 1 2 3))
	(values (alpha-a O)
		(alpha-b O)
		(alpha-c O)))
    => 1 2 3)

  (check
      (internal-body
	(define-record-type alpha
	  (fields a)
	  (fields b)
	  (fields c))
	(define-record-type beta
	  (parent alpha)
	  (fields d)
	  (fields e)
	  (fields f))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(values (alpha-a O)
		(alpha-b O)
		(alpha-c O)
		(beta-d  O)
		(beta-e  O)
		(beta-f  O)))
    => 1 2 3 4 5 6)

  (void))


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

  (check
      (internal-body
	(define-record-type alpha
	  (fields a b c))

	(let ((reco (new alpha 1 2 3)))
	  (alpha? reco)))
    => #t)

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


(parametrise ((check-test-name	'equality))

  (define-record-type <alpha>
    (fields a b c))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((P (make-<alpha> 1 2 3)))
     (record=? P P)))

  (check-for-true
   (let ((P (make-<alpha> 1 2 3)))
     (record=? P P P P)))

  (check-for-true
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 3)))
     (record=? P Q)))

  (check-for-true
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 3))
	 (R (make-<alpha> 1 2 3)))
     (record=? P Q R)))

  (check-for-false
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 9)))
     (record=? P Q)))

  (check-for-false
   (let ((P (make-<alpha> 1 2 3))
	 (Q (make-<alpha> 1 2 3))
	 (R (make-<alpha> 1 2 9)))
     (record=? P Q R)))

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
;;; equal?

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

;;;

  (check
      (internal-body
	(define-record-type <duo>
	  (fields one two)
	  (equality-predicate
	    (lambda ()
	      (lambda (A B)
		(and (= (<duo>-one A)
			(<duo>-one B))
		     (= (<duo>-two A)
			(<duo>-two B)))))))

	(values (equal? (make-<duo> 1 2)
			(make-<duo> 1 2))
		(equal? (make-<duo> 1 2)
			(make-<duo> 1 99))))
    => #t #f)

  ;;Custom equality predicate that compares only one field.
  ;;
  (check
      (internal-body
	(define-record-type <duo>
	  (fields one two)
	  (equality-predicate
	    (lambda ()
	      (lambda (A B)
		(= (<duo>-one A)
		   (<duo>-one B))))))

	(values (equal? (make-<duo> 1 2)
			(make-<duo> 1 99))
		(equal? (make-<duo> 1 2)
			(make-<duo> 99 2))))
    => #t #f)

;;; --------------------------------------------------------------------
;;; eqv?

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

;;;

  ;;EQV? does *not* use the equality predicate.
  ;;
  (check
      (internal-body
	(define-record-type <duo>
	  (fields one two)
	  (equality-predicate
	    (lambda ()
	      (lambda (A B)
		(and (= (<duo>-one A)
			(<duo>-one B))
		     (= (<duo>-two A)
			(<duo>-two B)))))))

	(values (let ((X (make-<duo> 1 2)))
		  (eqv? X X))
		(eqv? (make-<duo> 1 2)
		      (make-<duo> 1 2))
		(eqv? (make-<duo> 1 2)
		      (make-<duo> 1 99))))
    => #t #f #f)

  ;;EQV? does *not* use the equality predicate.
  ;;
  (check
      (internal-body
	(define-record-type <duo>
	  (fields one two)
	  (equality-predicate
	    (lambda ()
	      (lambda (A B)
		(= (<duo>-one A)
		   (<duo>-one B))))))

	(values (let ((X (make-<duo> 1 2)))
		  (eqv? X X))
		(eqv? (make-<duo> 1 2)
		      (make-<duo> 1 2))
		(eqv? (make-<duo> 1 2)
		      (make-<duo> 1 99))
		(eqv? (make-<duo> 1 2)
		      (make-<duo> 99 2))))
    => #t #f #f #f)

  #t)


(parametrise ((check-test-name	'reset))

  (define-record-type <alpha>
    (fields a b c))

  (check
      (let ((R (make-<alpha> 1 2 3)))
	(record-reset R)
	(list (void-object? (<alpha>-a R))
	      (void-object? (<alpha>-b R))
	      (void-object? (<alpha>-c R))))
    => '(#t #t #t))

  (check
      (try
	  (eval '(record-reset 123)
		(environment '(vicare))
		(expander-options typed-language)
		(compiler-options))
	(catch E
	  ((expander::&expand-time-type-signature-violation)
	   #t)
	  (else E)))
    => #t)

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
	    (void-object? (collect))))
      => #t)

    (check
	(let ((S (make-<alpha> 1 2 3)))
	  (check-pretty-print S)
	  (void-object? (collect)))
      => #t)

    (check
	(let ((S (make-<alpha> 1 2 3)))
	  (check-pretty-print S)
	  (void-object? (collect)))
      => #t)

    (void))

  (collect))


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
	(void-object? (delete O)))
    => #t)

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


(parametrise ((check-test-name		'super-protocol))

  ;;Single record with  no PROTOCOL and SUPER-PROTOCOL.  The normal  protocol is used
  ;;to build.
  ;;
  (check
      (internal-body
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b))
	  (super-protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record (+ 100 a) (+ 200 b))))))

	(let ((R (make-alpha 1 2)))
	  (values (alpha-a R)
		  (alpha-b R))))
    => 1 2)

  ;;Single record with  PROTOCOL and SUPER-PROTOCOL.  The normal protocol  is used to
  ;;build.
  ;;
  (check
      (internal-body
	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b))
	  (protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record (+ 10 a) (+ 20 b)))))
	  (super-protocol
	    (lambda (make-record)
	      (lambda (a b)
		(make-record (+ 100 a) (+ 200 b))))))

	(let ((R (make-alpha 1 2)))
	  (values (alpha-a R)
		  (alpha-b R))))
    => 11 22)

;;; --------------------------------------------------------------------
;;; recort-type with parent

  (internal-body
    (define-record-type alpha
      (fields (mutable a)
	      (mutable b))
      (super-protocol
	(lambda (make-record)
	  (lambda (a b)
	    (make-record (+ 10 a) (+ 20 b))))))

    (define-record-type beta
      (parent alpha)
      (fields (mutable c)
	      (mutable d)))

    (check
	(let ((R (make-alpha 1 2)))
	  (values (alpha-a R)
		  (alpha-b R)))
      => 1 2)

    (check
	(let ((R (make-beta 1 2 3 4)))
	  (values (alpha-a R)
		  (alpha-b R)
		  (beta-c  R)
		  (beta-d  R)))
      => 11 22 3 4)

    (void))

;;; --------------------------------------------------------------------
;;; record-type with parent and grandparent

  (internal-body
    (define-record-type alpha
      (fields (mutable a)
	      (mutable b))
      (super-protocol
	(lambda (make-record)
	  (lambda (a b)
	    (make-record (+ 10 a) (+ 20 b))))))

    (define-record-type beta
      (parent alpha)
      (fields (mutable c)
	      (mutable d))
      (super-protocol
	(lambda (make-alpha)
	  (lambda (a b c d)
	    ((make-alpha a b) (+ 30 c) (+ 40 d))))))

    (define-record-type gamma
      (parent beta)
      (fields (mutable e)
	      (mutable f))
      (super-protocol
	(lambda (make-beta)
	  (lambda (a b c d e f)
	    ((make-beta a b c d) (+ 50 e) (+ 60 f))))))

    (check
	(let ((R (make-alpha 1 2)))
	  (values (alpha-a R)
		  (alpha-b R)))
      => 1 2)

    (check
	(let ((R (make-beta 1 2 3 4)))
	  (values (alpha-a R)
		  (alpha-b R)
		  (beta-c  R)
		  (beta-d  R)))
      => 11 22 3 4)

    (check
	(let ((R (make-gamma 1 2 3 4 5 6)))
	  (values (alpha-a R)
		  (alpha-b R)
		  (beta-c  R)
		  (beta-d  R)
		  (gamma-e R)
		  (gamma-f R)))
      => 11 22 33 44 5 6)

    (void))

  ;;Parent record-type with no SUPER-PROTOCOL.
  ;;
  (internal-body
    (define-record-type alpha
      (fields (mutable a)
	      (mutable b))
      (super-protocol
	(lambda (make-record)
	  (lambda (a b)
	    (make-record (+ 10 a) (+ 20 b))))))

    (define-record-type beta
      (parent alpha)
      (fields (mutable c)
	      (mutable d)))

    (define-record-type gamma
      (parent beta)
      (fields (mutable e)
	      (mutable f))
      (super-protocol
	(lambda (make-beta)
	  (lambda (a b c d e f)
	    ((make-beta a b c d) (+ 50 e) (+ 60 f))))))

    (check
	(let ((R (make-alpha 1 2)))
	  (values (alpha-a R)
		  (alpha-b R)))
      => 1 2)

    (check
	(let ((R (make-beta 1 2 3 4)))
	  (values (alpha-a R)
		  (alpha-b R)
		  (beta-c  R)
		  (beta-d  R)))
      => 11 22 3 4)

    (check
	(let ((R (make-gamma 1 2 3 4 5 6)))
	  (values (alpha-a R)
		  (alpha-b R)
		  (beta-c  R)
		  (beta-d  R)
		  (gamma-e R)
		  (gamma-f R)))
      => 11 22 3 4 5 6)

    (void))

  (void))


(parametrise ((check-test-name		'type-predicate))

  ;;Custom predicate that just uses the default type predicate.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields one two)
	  (type-predicate
	    (lambda (duo?)
	      duo?)))

	(define O
	  (make-duo 1 2))

	(values (duo? O)
		(duo? 123)
		;;Remember that condition objects are records.
		(duo? (make-warning))))
    => #t #f #f)

  ;;Custom predicate that accepts records wrapped into lists and vectors.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields one two)
	  (type-predicate
	    (lambda (duo?)
	      (lambda (obj)
		(or (duo? obj)
		    (and (list? obj)
			 (duo?  (car obj))
			 (null? (cdr obj)))
		    (and (vector? obj)
			 (= 1 (vector-length obj))
			 (duo? (vector-ref obj 0))))))))

	(define O
	  (make-duo 1 2))

	(values (duo? O)
		(duo? (list O))
		(duo? (vector O))
		(duo? 123)
		;;Remember that condition objects are records.
		(duo? (make-warning))))
    => #t #t #t #f #f)

  (void))


(parametrise ((check-test-name	'hash-function))

  (check
      (internal-body
	(define-record-type duo
	  (fields one two)
	  (hash-function
	    (lambda ()
	      (lambda (O)
		(fx+ (fixnum-hash (duo-one O))
		     (fixnum-hash (duo-two O)))))))
	(hash (make-duo 1 2)))
    => (fx+ (fixnum-hash 1)
	    (fixnum-hash 2)))

  (void))


(parametrise ((check-test-name	'misc))

  (let ()
    (define-record-type <alpha>
      (nongenerative ciao-hello-ciao-1)
      (fields a))

    (check
	(record-rtd (make-<alpha> 1))
      => (record-type-descriptor <alpha>))

    #f)

;;; --------------------------------------------------------------------
;;; record-ref

  (check
      (internal-body
	(define-record-type duo
	  (fields one two))

	(define O
	  (make-duo 10 20))

	(values (record-ref O 0)
		(record-ref O 1)))
    => 10 20)

  (check
      (internal-body
	(define-record-type low
	  (fields a b))
	(define-record-type high
	  (parent low)
	  (fields c d))

	(define O
	  (make-high 10 20 30 40))

	(values (record-ref O 0)
		(record-ref O 1)
		(record-ref O 2)
		(record-ref O 3)))
    => 10 20 30 40)

  (check
      (internal-body
	(define-record-type low
	  (fields a b))
	(define-record-type middle
	  (parent low)
	  (fields c d))
	(define-record-type high
	  (parent middle)
	  (fields e f))

	(define O
	  (make-high 10 20 30 40 50 60))

	(values (record-ref O 0)
		(record-ref O 1)
		(record-ref O 2)
		(record-ref O 3)
		(record-ref O 4)
		(record-ref O 5)))
    => 10 20 30 40 50 60)

  #t)


(parametrise ((check-test-name	'bugs))

  (check-for-expression-return-value-violation
      (internal-body
	(define-record-type alpha
	  (fields a)
	  (protocol
	    (lambda (maker)
	      (void))))
	(make-alpha 1))
    => '(make-record-constructor-descriptor (#!void)))

  #t)


(parametrise ((check-test-name	'define-type-descriptors))

  (check
      (internal-body
	(define-record-type alpha
	  (define-type-descriptors))
	(values (record-type-descriptor? alpha-rtd)
		(record-constructor-descriptor? alpha-rcd)))
    => #t #t)

  (void))


(parametrise ((check-test-name	'strip-angular-parentheses))

  (check
      (internal-body
	(define-record-type <alpha>)
	(<alpha>? (make-<alpha>)))
    => #t)

  (check
      (internal-body
	(define-record-type <alpha>
	  (strip-angular-parentheses))
	(alpha? (make-alpha)))
    => #t)

;;; --------------------------------------------------------------------
;;; immutable fields

  (check
      (internal-body
	(define-record-type <alpha>
	  (fields a b c))
	(let ((O (make-<alpha> 1 2 3)))
	  (values (<alpha>-a O)
		  (<alpha>-b O)
		  (<alpha>-c O))))
    => 1 2 3)

  (check
      (internal-body
	(define-record-type <alpha>
	  (strip-angular-parentheses)
	  (fields a b c))
	(let ((O (make-alpha 1 2 3)))
	  (values (alpha-a O)
		  (alpha-b O)
		  (alpha-c O))))
    => 1 2 3)

;;; --------------------------------------------------------------------
;;; mutable fields

  (check
      (internal-body
	(define-record-type <alpha>
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(let ((O (make-<alpha> 1 2 3)))
	  (<alpha>-a-set! O 10)
	  (<alpha>-b-set! O 20)
	  (<alpha>-c-set! O 30)
	  (values (<alpha>-a O)
		  (<alpha>-b O)
		  (<alpha>-c O))))
    => 10 20 30)

  (check
      (internal-body
	(define-record-type <alpha>
	  (strip-angular-parentheses)
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(let ((O (make-alpha 1 2 3)))
	  (alpha-a-set! O 10)
	  (alpha-b-set! O 20)
	  (alpha-c-set! O 30)
	  (values (alpha-a O)
		  (alpha-b O)
		  (alpha-c O))))
    => 10 20 30)

  (void))


;;;; done

(collect 4)
(check-report)

#| end of program |# )

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
