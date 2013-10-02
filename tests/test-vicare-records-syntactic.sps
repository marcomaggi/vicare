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
  (vicare language-extensions syntaxes)
  (vicare system $structs)
  (libtest records-lib)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare R6RS records, syntactic layer\n")


;;;; syntax helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))


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

  (check	;safe accessors
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	(list (record-type-field-ref color red   X)
	      (record-type-field-ref color green X)
	      (record-type-field-ref color blue  X)))
    => '(1 2 3))

  (check	;safe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	(record-type-field-set! color red   X 10)
	(record-type-field-set! color green X 20)
	(record-type-field-set! color blue  X 30)
	(list (record-type-field-ref color red   X)
	      (record-type-field-ref color green X)
	      (record-type-field-ref color blue  X)))
    => '(10 20 30))

  (check	;safe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red   the-red   set-the-red!)
		  (mutable green the-green set-the-green!)
		  (mutable blue  the-blue  set-the-blue!)))
	(define X
	  (make-color 1 2 3))
	(record-type-field-set! color red   X 10)
	(record-type-field-set! color green X 20)
	(record-type-field-set! color blue  X 30)
	(list (record-type-field-ref color red   X)
	      (record-type-field-ref color green X)
	      (record-type-field-ref color blue  X)))
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
	(list (record-type-field-ref alpha a O)
	      (record-type-field-ref alpha b O)
	      (record-type-field-ref alpha c O)
	      (record-type-field-ref beta a O)
	      (record-type-field-ref beta b O)
	      (record-type-field-ref beta c O)))
    => '(1 2 3 4 5 6))

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
	(record-type-field-set! alpha a O 10)
	(record-type-field-set! alpha b O 20)
	(record-type-field-set! alpha c O 30)
	(record-type-field-set! beta a O 40)
	(record-type-field-set! beta b O 50)
	(record-type-field-set! beta c O 60)
	(list (record-type-field-ref alpha a O)
	      (record-type-field-ref alpha b O)
	      (record-type-field-ref alpha c O)
	      (record-type-field-ref beta a O)
	      (record-type-field-ref beta b O)
	      (record-type-field-ref beta c O)))
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
	(record-type-field-set! alpha a O 10)
	#;(record-type-field-set! alpha b O 20)
	(record-type-field-set! alpha c O 30)
	(record-type-field-set! beta a O 40)
	#;(record-type-field-set! beta b O 50)
	(record-type-field-set! beta c O 60)
	(list (record-type-field-ref alpha a O)
	      (record-type-field-ref alpha b O)
	      (record-type-field-ref alpha c O)
	      (record-type-field-ref beta a O)
	      (record-type-field-ref beta b O)
	      (record-type-field-ref beta c O)))
    => '(10 2 30 40 5 60))

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
	(define-record-type gamma
	  (parent beta)
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define O
	  (make-gamma 1 2 3 4 5 6 7 8 9))
	(record-type-field-set! alpha a O 10)
	#;(record-type-field-set! alpha b O 20)
	(record-type-field-set! alpha c O 30)
	(record-type-field-set! beta a O 40)
	#;(record-type-field-set! beta b O 50)
	(record-type-field-set! beta c O 60)
	(record-type-field-set! gamma a O 70)
	#;(record-type-field-set! gamma b O 80)
	(record-type-field-set! gamma c O 90)
	(list (record-type-field-ref alpha a O)
	      (record-type-field-ref alpha b O)
	      (record-type-field-ref alpha c O)
	      (record-type-field-ref beta a O)
	      (record-type-field-ref beta b O)
	      (record-type-field-ref beta c O)
	      (record-type-field-ref gamma a O)
	      (record-type-field-ref gamma b O)
	      (record-type-field-ref gamma c O)))
    => '(10 2 30 40 5 60 70 8 90))

;;; --------------------------------------------------------------------
;;; here we use records from the library (libtest records-lib)

  (check	;safe accessors
      (let ((X (make-<alpha> 1 2)))
	(list (record-type-field-ref <alpha> one X)
	      (record-type-field-ref <alpha> two X)))
    => '(1 2))

  (check	;safe accessors and mutators
      (let ((X (make-<alpha> 1 2)))
	(record-type-field-set! <alpha> one X 10)
	(list (record-type-field-ref <alpha> one X)
	      (record-type-field-ref <alpha> two X)))
    => '(10 2))

  (check	;safe accessors
      (let ((X (make-<gamma> 1 2 3 4)))
	(list (record-type-field-ref <beta>  one   X)
	      (record-type-field-ref <beta>  two   X)
	      (record-type-field-ref <gamma> three X)
	      (record-type-field-ref <gamma> four  X)
	      ))
    => '(1 2 3 4))

  (check	;safe accessors and mutators
      (let ((X (make-<gamma> 1 2 3 4)))
	(record-type-field-set! <beta>  one   X 10)
	(record-type-field-set! <gamma> three X 30)
	(list (record-type-field-ref <beta>  one   X)
	      (record-type-field-ref <beta>  two   X)
	      (record-type-field-ref <gamma> three X)
	      (record-type-field-ref <gamma> four  X)
	      ))
    => '(10 2 30 4))

  #t)


(parametrise ((check-test-name	'unsafe-record-type-field))

  (check	;unsafe accessors
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	(list ($record-type-field-ref color red   X)
	      ($record-type-field-ref color green X)
	      ($record-type-field-ref color blue  X)))
    => '(1 2 3))

  (check	;unsafe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red)
		  (mutable green)
		  (mutable blue)))
	(define X
	  (make-color 1 2 3))
	($record-type-field-set! color red   X 10)
	($record-type-field-set! color green X 20)
	($record-type-field-set! color blue  X 30)
	(list ($record-type-field-ref color red   X)
	      ($record-type-field-ref color green X)
	      ($record-type-field-ref color blue  X)))
    => '(10 20 30))

  (check	;unsafe accessors and mutators
      (let ()
	(define-record-type color
	  (fields (mutable red   the-red   set-the-red!)
		  (mutable green the-green set-the-green!)
		  (mutable blue  the-blue  set-the-blue!)))
	(define X
	  (make-color 1 2 3))
	($record-type-field-set! color red   X 10)
	($record-type-field-set! color green X 20)
	($record-type-field-set! color blue  X 30)
	(list ($record-type-field-ref color red   X)
	      ($record-type-field-ref color green X)
	      ($record-type-field-ref color blue  X)))
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
	(list ($record-type-field-ref alpha a O)
	      ($record-type-field-ref alpha b O)
	      ($record-type-field-ref alpha c O)
	      ($record-type-field-ref beta a O)
	      ($record-type-field-ref beta b O)
	      ($record-type-field-ref beta c O)))
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
	($record-type-field-set! alpha a O 10)
	($record-type-field-set! alpha b O 20)
	($record-type-field-set! alpha c O 30)
	($record-type-field-set! beta a O 40)
	($record-type-field-set! beta b O 50)
	($record-type-field-set! beta c O 60)
	(list ($record-type-field-ref alpha a O)
	      ($record-type-field-ref alpha b O)
	      ($record-type-field-ref alpha c O)
	      ($record-type-field-ref beta a O)
	      ($record-type-field-ref beta b O)
	      ($record-type-field-ref beta c O)))
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
	($record-type-field-set! alpha a O 10)
	#;($record-type-field-set! alpha b O 20)
	($record-type-field-set! alpha c O 30)
	($record-type-field-set! beta a O 40)
	#;($record-type-field-set! beta b O 50)
	($record-type-field-set! beta c O 60)
	(list ($record-type-field-ref alpha a O)
	      ($record-type-field-ref alpha b O)
	      ($record-type-field-ref alpha c O)
	      ($record-type-field-ref beta a O)
	      ($record-type-field-ref beta b O)
	      ($record-type-field-ref beta c O)))
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
	($record-type-field-set! alpha a O 10)
	#;($record-type-field-set! alpha b O 20)
	($record-type-field-set! alpha c O 30)
	($record-type-field-set! beta a O 40)
	#;($record-type-field-set! beta b O 50)
	($record-type-field-set! beta c O 60)
	($record-type-field-set! gamma a O 70)
	#;($record-type-field-set! gamma b O 80)
	($record-type-field-set! gamma c O 90)
	(list ($record-type-field-ref alpha a O)
	      ($record-type-field-ref alpha b O)
	      ($record-type-field-ref alpha c O)
	      ($record-type-field-ref beta a O)
	      ($record-type-field-ref beta b O)
	      ($record-type-field-ref beta c O)
	      ($record-type-field-ref gamma a O)
	      ($record-type-field-ref gamma b O)
	      ($record-type-field-ref gamma c O)))
    => '(10 2 30 40 5 60 70 8 90))

;;; --------------------------------------------------------------------
;;; here we use records from the library (libtest records-lib)

  (check	;safe accessors
      (let ((X (make-<alpha> 1 2)))
	(list ($record-type-field-ref <alpha> one X)
	      ($record-type-field-ref <alpha> two X)))
    => '(1 2))

  (check	;safe accessors and mutators
      (let ((X (make-<alpha> 1 2)))
	($record-type-field-set! <alpha> one X 10)
	(list ($record-type-field-ref <alpha> one X)
	      ($record-type-field-ref <alpha> two X)))
    => '(10 2))

  (check	;safe accessors
      (let ((X (make-<gamma> 1 2 3 4)))
	(list ($record-type-field-ref <beta>  one   X)
	      ($record-type-field-ref <beta>  two   X)
	      ($record-type-field-ref <gamma> three X)
	      ($record-type-field-ref <gamma> four  X)
	      ))
    => '(1 2 3 4))

  (check	;safe accessors and mutators
      (let ((X (make-<gamma> 1 2 3 4)))
	($record-type-field-set! <beta>  one   X 10)
	($record-type-field-set! <gamma> three X 30)
	(list ($record-type-field-ref <beta>  one   X)
	      ($record-type-field-ref <beta>  two   X)
	      ($record-type-field-ref <gamma> three X)
	      ($record-type-field-ref <gamma> four  X)
	      ))
    => '(10 2 30 4))

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

    (record-destructor-set! (record-type-descriptor <alpha>)
			    (lambda (S)
			      (pretty-print (list 'finalising S)
					    (current-error-port))))

    (parametrise ((record-guardian-logger #f))
      (pretty-print (make-<alpha> 1 2 3) (current-error-port))
      (collect))

    #f)

  (define-record-type <alpha>
    (fields a b c))

  (record-destructor-set! (record-type-descriptor <alpha>)
			  (lambda (S)
			    (void)))

  (check
      (procedure? (record-destructor (record-type-descriptor <alpha>)))
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

  (check
      (catch #f
	(let ()
	  (define-record-type alpha
	    (fields a)
	    (protocol (lambda (maker)
			(newline))))
	  (make-alpha 1)))
    => (list (void)))

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
