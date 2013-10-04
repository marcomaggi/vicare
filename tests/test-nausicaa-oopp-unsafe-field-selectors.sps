;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for unsafe field selectors in Nausicaa
;;;Date: Fri Oct  4, 2013
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
(import (nausicaa)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa libraries: unsafe field selectors\n")


(parametrise ((check-test-name	'base-accessors))

  (check	;accessors for immutable fields
      (let ()
	(define-class <alpha>
	  (fields a b c))
	(<alpha> A (<> (1 2 3)))
	(list (A $a) (A $b) (A $c)))
    => '(1 2 3))

  (check	;accessors for mutable fields
      (let ()
	(define-class <alpha>
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(<alpha> A (<> (1 2 3)))
	(list (A $a) (A $b) (A $c)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;accessors for immutable tagged fields
      (let ()
	(define-class <alpha>
	  (fields (immutable (a <fixnum>))
		  (immutable (b <fixnum>))
		  (immutable (c <fixnum>))))
	(<alpha> A (<> (1 2 3)))
	(list (A $a) (A $b) (A $c)))
    => '(1 2 3))

  (check	;accessors for mutable tagged fields
      (let ()
	(define-class <alpha>
	  (fields (mutable (a <fixnum>))
		  (mutable (b <fixnum>))
		  (mutable (c <fixnum>))))
	(<alpha> A (<> (1 2 3)))
	(list (A $a) (A $b) (A $c)))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'base-mutators))

  (check	;mutators for mutable fields
      (let ()
	(define-class <alpha>
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(<alpha> A (<> (1 2 3)))
	(set! (A $a) 11)
	(set! (A $b) 22)
	(set! (A $c) 33)
	(list (A $a) (A $b) (A $c)))
    => '(11 22 33))

;;; --------------------------------------------------------------------

  (check	;mutators for mutable tagged fields
      (let ()
	(define-class <alpha>
	  (fields (mutable (a <fixnum>))
		  (mutable (b <fixnum>))
		  (mutable (c <fixnum>))))
	(<alpha> A (<> (1 2 3)))
	(set! (A $a) 11)
	(set! (A $b) 22)
	(set! (A $c) 33)
	(list (A $a) (A $b) (A $c)))
    => '(11 22 33))

  #t)


(parametrise ((check-test-name	'super-accessors))

  (check	;accessors for immutable fields
      (let ()
	(define-class <alpha>
	  (fields a b c))
	(define-class <beta>
	  (parent <alpha>)
	  (fields d e f))
	(<beta> B (<> (1 2 3 4 5 6)))
	(list (B $a) (B $b) (B $c)
	      (B $d) (B $e) (B $f)))
    => '(1 2 3 4 5 6))

  (check	;accessors for mutable fields
      (let ()
	(define-class <alpha>
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
	(<beta> B (<> (1 2 3 4 5 6)))
	(list (B $a) (B $b) (B $c)
	      (B $d) (B $e) (B $f)))
    => '(1 2 3 4 5 6))

  (check	;accessors for mutable fields
      (let ()
	(define-class <alpha>
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
	(define-class <gamma>
	  (parent <beta>)
	  (fields (mutable g)
		  (mutable h)
		  (mutable i)))
	(<gamma> G (<> (1 2 3 4 5 6 7 8 9)))
	(list (G $a) (G $b) (G $c)
	      (G $d) (G $e) (G $f)
	      (G $g) (G $h) (G $i)))
    => '(1 2 3 4 5 6 7 8 9))

;;; --------------------------------------------------------------------

  (check	;accessors for immutable tagged fields
      (let ()
	(define-class <alpha>
	  (fields (immutable (a <fixnum>))
		  (immutable (b <fixnum>))
		  (immutable (c <fixnum>))))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (immutable (d <fixnum>))
		  (immutable (e <fixnum>))
		  (immutable (f <fixnum>))))
	(<beta> B (<> (1 2 3 4 5 6)))
	(list (B $a) (B $b) (B $c)
	      (B $d) (B $e) (B $f)))
    => '(1 2 3 4 5 6))

  (check	;accessors for mutable tagged fields
      (let ()
	(define-class <alpha>
	  (fields (mutable (a <fixnum>))
		  (mutable (b <fixnum>))
		  (mutable (c <fixnum>))))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (mutable (d <fixnum>))
		  (mutable (e <fixnum>))
		  (mutable (f <fixnum>))))
	(<beta> B (<> (1 2 3 4 5 6)))
	(list (B $a) (B $b) (B $c)
	      (B $d) (B $e) (B $f)))
    => '(1 2 3 4 5 6))

  (check	;accessors for mutable tagged fields
      (let ()
	(define-class <alpha>
	  (fields (mutable (a <fixnum>))
		  (mutable (b <fixnum>))
		  (mutable (c <fixnum>))))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (mutable (d <fixnum>))
		  (mutable (e <fixnum>))
		  (mutable (f <fixnum>))))
	(define-class <gamma>
	  (parent <beta>)
	  (fields (mutable (g <fixnum>))
		  (mutable (h <fixnum>))
		  (mutable (i <fixnum>))))
	(<gamma> G (<> (1 2 3 4 5 6 7 8 9)))
	(list (G $a) (G $b) (G $c)
	      (G $d) (G $e) (G $f)
	      (G $g) (G $h) (G $i)))
    => '(1 2 3 4 5 6 7 8 9))

  #t)


(parametrise ((check-test-name	'super-mutators))

  (check	;mutators for mutable fields
      (let ()
	(define-class <alpha>
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
	(<beta> B (<> (1 2 3 4 5 6)))
	(set! (B $a) 11)
	(set! (B $b) 22)
	(set! (B $c) 33)
	(set! (B $d) 44)
	(set! (B $e) 55)
	(set! (B $f) 66)
	(list (B $a) (B $b) (B $c)
	      (B $d) (B $e) (B $f)))
    => '(11 22 33 44 55 66))

  (check	;mutators for mutable fields
      (let ()
	(define-class <alpha>
	  (fields (mutable a)
		  (mutable b)
		  (mutable c)))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
	(define-class <gamma>
	  (parent <beta>)
	  (fields (mutable g)
		  (mutable h)
		  (mutable i)))
	(<gamma> G (<> (1 2 3 4 5 6 7 8 9)))
	(set! (G $a) 11)
	(set! (G $b) 22)
	(set! (G $c) 33)
	(set! (G $d) 44)
	(set! (G $e) 55)
	(set! (G $f) 66)
	(set! (G $g) 77)
	(set! (G $h) 88)
	(set! (G $i) 99)
	(list (G $a) (G $b) (G $c)
	      (G $d) (G $e) (G $f)
	      (G $g) (G $h) (G $i)))
    => '(11 22 33 44 55 66 77 88 99))

;;; --------------------------------------------------------------------

  (check	;mutators for mutable tagged fields
      (let ()
	(define-class <alpha>
	  (fields (mutable (a <fixnum>))
		  (mutable (b <fixnum>))
		  (mutable (c <fixnum>))))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (mutable (d <fixnum>))
		  (mutable (e <fixnum>))
		  (mutable (f <fixnum>))))
	(<beta> B (<> (1 2 3 4 5 6)))
	(set! (B $a) 11)
	(set! (B $b) 22)
	(set! (B $c) 33)
	(set! (B $d) 44)
	(set! (B $e) 55)
	(set! (B $f) 66)
	(list (B $a) (B $b) (B $c)
	      (B $d) (B $e) (B $f)))
    => '(11 22 33 44 55 66))

  (check	;mutators for mutable tagged fields
      (let ()
	(define-class <alpha>
	  (fields (mutable (a <fixnum>))
		  (mutable (b <fixnum>))
		  (mutable (c <fixnum>))))
	(define-class <beta>
	  (parent <alpha>)
	  (fields (mutable (d <fixnum>))
		  (mutable (e <fixnum>))
		  (mutable (f <fixnum>))))
	(define-class <gamma>
	  (parent <beta>)
	  (fields (mutable (g <fixnum>))
		  (mutable (h <fixnum>))
		  (mutable (i <fixnum>))))
	(<gamma> G (<> (1 2 3 4 5 6 7 8 9)))
	(set! (G $a) 11)
	(set! (G $b) 22)
	(set! (G $c) 33)
	(set! (G $d) 44)
	(set! (G $e) 55)
	(set! (G $f) 66)
	(set! (G $g) 77)
	(set! (G $h) 88)
	(set! (G $i) 99)
	(list (G $a) (G $b) (G $c)
	      (G $d) (G $e) (G $f)
	      (G $g) (G $h) (G $i)))
    => '(11 22 33 44 55 66 77 88 99))
  #t)


(parametrise ((check-test-name	'field-accessors))

  (define-class <alpha>
    (fields a))
  (define-class <beta>
    (fields b))
  (define-class <gamma>
    (fields c))

;;; --------------------------------------------------------------------

  (check	;immutable field accessors
      (let ()
	(define-class <class>
	  (fields (immutable (A <alpha>))
		  (immutable (B <beta>))
		  (immutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(list (C $A a) (C A $a) (C $A $a)
	      (C $B b) (C B $b) (C $B $b)
	      (C $C c) (C C $c) (C $C $c)))
    => '(1 1 1 2 2 2 3 3 3))

;;; --------------------------------------------------------------------

  (check	;mutable field accessors
      (let ()
	(define-class <class>
	  (fields (mutable (A <alpha>))
		  (mutable (B <beta>))
		  (mutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(list (C $A a) (C A $a) (C $A $a)
	      (C $B b) (C B $b) (C $B $b)
	      (C $C c) (C C $c) (C $C $c)))
    => '(1 1 1 2 2 2 3 3 3))

  #t)


(parametrise ((check-test-name	'field-mutators))

  (define-class <alpha>
    (fields (mutable a)))
  (define-class <beta>
    (fields (mutable b)))
  (define-class <gamma>
    (fields (mutable c)))

;;; --------------------------------------------------------------------

  (check	;mutable field mutators, unsafe unsafe
      (let ()
	(define-class <class>
	  (fields (mutable (A <alpha>))
		  (mutable (B <beta>))
		  (mutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(set! (C $A $a) 11)
	(set! (C $B $b) 22)
	(set! (C $C $c) 33)
	(list (C $A a) (C A $a) (C $A $a)
	      (C $B b) (C B $b) (C $B $b)
	      (C $C c) (C C $c) (C $C $c)))
    => '(11 11 11 22 22 22 33 33 33))

  (check	;mutable field mutators, safe unsafe
      (let ()
	(define-class <class>
	  (fields (mutable (A <alpha>))
		  (mutable (B <beta>))
		  (mutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(set! (C A $a) 11)
	(set! (C B $b) 22)
	(set! (C C $c) 33)
	(list (C $A a) (C A $a) (C $A $a)
	      (C $B b) (C B $b) (C $B $b)
	      (C $C c) (C C $c) (C $C $c)))
    => '(11 11 11 22 22 22 33 33 33))

  (check	;mutable field mutators, unsafe safe
      (let ()
	(define-class <class>
	  (fields (mutable (A <alpha>))
		  (mutable (B <beta>))
		  (mutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(set! (C $A a) 11)
	(set! (C $B b) 22)
	(set! (C $C c) 33)
	(list (C $A a) (C A $a) (C $A $a)
	      (C $B b) (C B $b) (C $B $b)
	      (C $C c) (C C $c) (C $C $c)))
    => '(11 11 11 22 22 22 33 33 33))

  #t)


(parametrise ((check-test-name	'field-methods))

  (define-class <alpha>
    (fields a)
    (method (doit-a (A <alpha>))
      (A $a)))
  (define-class <beta>
    (fields b)
    (method (doit-b (B <beta>))
      (B $b)))
  (define-class <gamma>
    (fields c)
    (method (doit-c (C <gamma>))
      (C $c)))

;;; --------------------------------------------------------------------

  (check	;immutable field methods
      (let ()
	(define-class <class>
	  (fields (immutable (A <alpha>))
		  (immutable (B <beta>))
		  (immutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(list (C $A doit-a)
	      (C $B doit-b)
	      (C $C doit-c)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;mutable field methods
      (let ()
	(define-class <class>
	  (fields (mutable (A <alpha>))
		  (mutable (B <beta>))
		  (mutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(list (C $A doit-a)
	      (C $B doit-b)
	      (C $C doit-c)))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'field-getters))

  (define-class <alpha>
    (fields a)
    (getter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)))
		 #'(?var ?key))))))
  (define-class <beta>
    (fields b)
    (getter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)))
		 #'(?var ?key))))))
  (define-class <gamma>
    (fields c)
    (getter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)))
		 #'(?var ?key))))))

;;; --------------------------------------------------------------------

  (check	;immutable field getters
      (let ()
	(define-class <class>
	  (fields (immutable (A <alpha>))
		  (immutable (B <beta>))
		  (immutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(list (C $A [a])
	      (C $B [b])
	      (C $C [c])))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;mutable field getters
      (let ()
	(define-class <class>
	  (fields (mutable (A <alpha>))
		  (mutable (B <beta>))
		  (mutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(list (C $A [a])
	      (C $B [b])
	      (C $C [c])))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'field-setters))

  (define-class <alpha>
    (fields (mutable a))
    (getter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)))
		 #'(?var ?key)))))
    (setter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)) ?val)
		 #'(set! (?var ?key) ?val))))))

  (define-class <beta>
    (fields (mutable b))
    (getter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)))
		 #'(?var ?key)))))
    (setter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)) ?val)
		 #'(set! (?var ?key) ?val))))))

  (define-class <gamma>
    (fields (mutable c))
    (getter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)))
		 #'(?var ?key)))))
    (setter (lambda (stx)
	      (syntax-case stx ()
		((?var ((?key)) ?val)
		 #'(set! (?var ?key) ?val))))))

;;; --------------------------------------------------------------------

  (check	;immutable field setters
      (let ()
	(define-class <class>
	  (fields (immutable (A <alpha>))
		  (immutable (B <beta>))
		  (immutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(set! (C $A [a]) 11)
	(set! (C $B [b]) 22)
	(set! (C $C [c]) 33)
	(list (C $A [a])
	      (C $B [b])
	      (C $C [c])))
    => '(11 22 33))

;;; --------------------------------------------------------------------

  (check	;mutable field setters
      (let ()
	(define-class <class>
	  (fields (mutable (A <alpha>))
		  (mutable (B <beta>))
		  (mutable (C <gamma>))))
	(<class> C (<> [(<alpha> (1))
			(<beta>  (2))
			(<gamma> (3))]))
	(set! (C $A [a]) 11)
	(set! (C $B [b]) 22)
	(set! (C $C [c]) 33)
	(list (C $A [a])
	      (C $B [b])
	      (C $C [c])))
    => '(11 22 33))

  #t)


;;;; done

(check-report)

;;; end of file
