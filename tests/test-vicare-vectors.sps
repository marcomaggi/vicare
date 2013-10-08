;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for "ikarus.vectors.ss"
;;;Date: Mon Oct 31, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (ikarus)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare vector functions\n")


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


(parametrise ((check-test-name	'vector-length))

  (check
      (vector-length '#())
    => 0)

  (check
      (vector-length '#(a))
    => 1)

  (check
      (vector-length '#(a b c))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (catch #f
	(vector-length 123))
    => '(123))

;;; --------------------------------------------------------------------

  (check
      (vector-empty? '#())
    => #t)

  (check
      (vector-empty? '#(1 2 3))
    => #f)

  #t)


(parametrise ((check-test-name	'vector-ref))

  (check
      (vector-ref '#(a) 0)
    => 'a)

  (check
      (vector-ref '#(a b c) 2)
    => 'c)

;;; --------------------------------------------------------------------
;;; arguments validation: vector

  (check
      (catch #f
	(vector-ref 123 1))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check
      (catch #f
  	(vector-ref '#(c i a o) #\d))
    => '(#\d #(c i a o)))

  (check
      (catch #f
  	(vector-ref '#(c i a o) 'd))
    => '(d #(c i a o)))

  (check
      (catch #f
	(vector-ref '#() -1))
    => '(-1 #()))

  (check
      (catch #f
	(vector-ref '#() (+ 1 (greatest-fixnum))))
    => (list (+ 1 (greatest-fixnum)) '#()))

  (check
      (catch #f
	(vector-ref '#() 0))
    => '(0 #()))

  (check
      (catch #f
	(vector-ref '#(a b c) 10))
    => '(10 #(a b c)))

  #t)


(parametrise ((check-test-name	'vector-set-bang))

  (check
      (let ((vec (vector 'a 'b 'c)))
	(vector-set! vec 0 '9)
	vec)
    => '#(9 b c))

  (check
      (let ((vec (vector 'a 'b 'c)))
	(vector-set! vec 1 '9)
	vec)
    => '#(a 9 c))

  (check
      (let ((vec (vector 'a 'b 'c)))
	(vector-set! vec 2 '9)
	vec)
    => '#(a b 9))

;;; --------------------------------------------------------------------
;;; arguments validation: vector

  (check
      (catch #f
	(vector-set! 123 1 'a))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check
      (catch #f
	(vector-set! (vector 'a 'b 'c) 'a 'b))
    => '(a #(a b c)))

  (check
      (catch #f
	(vector-set! (vector 'a 'b 'c) -1 'a))
    => '(-1 #(a b c)))

  (check
      (catch #f
	(vector-set! (vector 'a 'b 'c) (+ 1 (greatest-fixnum)) 'a))
    => (list (+ 1 (greatest-fixnum)) '#(a b c)))

  (check
      (catch #f
	(vector-set! (vector) 0 'a))
    => '(0 #()))

  (check
      (catch #f
	(vector-set! (vector 'a 'b 'c) 10 '9))
    => `(10 #(a b c)))

  #t)


(parametrise ((check-test-name	'make-vector))

  (check
      (make-vector 0)
    => '#())

  (check
      (make-vector 1)
    => '#(0))

  (check
      (make-vector 3)
    => '#(0 0 0))

;;; --------------------------------------------------------------------

  (check
      (make-vector 0 'A)
    => '#())

  (check
      (make-vector 1 'A)
    => '#(A))

  (check
      (make-vector 3 'A)
    => '#(A A A))

;;; --------------------------------------------------------------------
;;; arguments validation: length

  (check	;not a number
      (catch #f
	(make-vector #\t 'A))
    => '(#\t))

  (check	;negative
      (catch #f
	(make-vector -1 'A))
    => '(-1))

  (check	;not a fixnum
      (catch #f
	(make-vector (+ 1 (greatest-fixnum)) 'A))
    => (list (+ 1 (greatest-fixnum))))

  #t)


(parametrise ((check-test-name	'vector))

  (check
      (vector)
    => '#())

  (check
      (vector 'a)
    => '#(a))

  (check
      (vector 'a 'b 'c)
    => '#(a b c))

  (check
      (vector 'a 'b 'c 'd)
    => '#(a b c d))

  (check
      (vector 'a 'b 'c 'd 'e 'f 'g)
    => '#(a b c d e f g))

  #t)


(parametrise ((check-test-name	'subvector))

  (check
      (subvector '#() 0 0)
    => '#())

  (check
      (subvector '#(a) 0 0)
    => '#())

  (check
      (subvector '#(a b c) 0 0)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (subvector '#(a) 1 1)
    => '#())

  (check
      (subvector '#(a b c) 1 1)
    => '#())

  (check
      (subvector '#(a b c) 3 3)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (subvector '#(a) 0 1)
    => '#(a))

  (check
      (subvector '#(a b c) 1 2)
    => '#(b))

  (check
      (subvector '#(a b c) 2 3)
    => '#(c))

;;; --------------------------------------------------------------------

  (check
      (subvector '#(a b c) 0 3)
    => '#(a b c))

;;; --------------------------------------------------------------------
;;; arguments validation: vector

  (check
      (catch #f
	(subvector 123 1 1))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: start index

  (check	;not a number
      (catch #f
	(subvector '#(a b c d) #t 1))
    => '(#t))

  (check	;negative
      (catch #f
	(subvector '#(a b c d) -1 1))
    => '(-1))

  (check	;not a fixnum
      (catch #f
	(subvector '#(a b c d) (+ 1 (greatest-fixnum)) 1))
    => (list (+ 1 (greatest-fixnum))))

  (check	;too big for vector
      (catch #f
	(subvector '#(a b c d) 5 6))
    => '(5 4))

;;; --------------------------------------------------------------------
;;; arguments validation: end index

  (check	;not a number
      (catch #f
	(subvector '#(a b c d) 1 #t))
    => '(#t))

  (check	;negative
      (catch #f
	(subvector '#(a b c d) 1 -1))
    => '(-1))

  (check	;not a fixnum
      (catch #f
	(subvector '#(a b c d) 1 (+ 1 (greatest-fixnum))))
    => (list (+ 1 (greatest-fixnum))))

  (check	;too big for vector
      (catch #f
	(subvector '#(a b c d) 2 6))
    => '(6 4))

;;; --------------------------------------------------------------------
;;; arguments validation: indexes

  (check	;incorrect order
      (catch #f
	(subvector '#(a b c d) 2 1))
    => '(2 1))

  #t)


(parametrise ((check-test-name	'vector-copy))

  (check
      (vector-copy '#())
    => '#())

  (check
      (vector-copy '#(a))
    => '#(a))

  (check
      (vector-copy '#(a b c))
    => '#(a b c))

;;; --------------------------------------------------------------------
;;; arguments validation: vector

  (check
      (catch #f
	(vector-copy 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'vector-resize))

  (check
      (vector-resize '#() 3)
    => '#(#f #f #f))

  (check
      (vector-resize '#() 0)
    => '#())

  (check
      (vector-resize '#(a b c) 0)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-resize '#(a) 1)
    => '#(a))

  (check
      (vector-resize '#(a b c) 5)
    => '#(a b c #f #f))

  (check
      (vector-resize '#(a b c) 5 123)
    => '#(a b c 123 123))

  (check
      (vector-resize '#(a b c d e) 3)
    => '#(a b c))

;;; --------------------------------------------------------------------
;;; arguments validation: vector

  (check
      (catch #f
	(vector-resize 123 1))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: new-len

  (check
      (catch #f
	(vector-resize '#(1 2 3) -123))
    => '(-123))

  #t)


(parametrise ((check-test-name	'vector-to-list))

  (check
      (vector->list '#())
    => '())

  (check
      (vector->list '#(a))
    => '(a))

  (check
      (vector->list '#(a b c))
    => '(a b c))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check
      (catch #f
	(vector->list 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'list-to-vector))

  (check
      (list->vector '())
    => '#())

  (check
      (list->vector '(a))
    => '#(a))

  (check
      (list->vector '(a b c))
    => '#(a b c))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;not a list
      (catch #f
	(list->vector 123))
    => '(123))

  (check	;not a proper list
      (catch #f
	(list->vector '(a b . c)))
    => '((a b . c)))

  (check	;not a proper list
      (catch #f
	(list->vector '(a . c)))
    => '((a . c)))

  (let ((circ '#0=(a b c . #0#)))
    (check	;circular list
	(catch #f
	  (list->vector circ))
      => (list circ)))

  #t)


(parametrise ((check-test-name	'vector-append))

  (check
      (vector-append)
    => '#())

  (check
      (vector-append '#())
    => '#())

  (check
      (vector-append '#() '#() '#())
    => '#())

  (check
      (vector-append '#() '#() '#() '#())
    => '#())

  (check
      (vector-append '#() '#() '#() '#() '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-append '#(a))
    => '#(a))

  (check
      (vector-append '#(a) '#(b))
    => '#(a b))

  (check
      (vector-append '#(a) '#(b) '#(c))
    => '#(a b c))

  (check
      (vector-append '#(a) '#(b) '#(c) '#(d))
    => '#(a b c d))

  (check
      (vector-append '#(a) '#(b) '#(c) '#(d) '#(e))
    => '#(a b c d e))

;;; --------------------------------------------------------------------

  (check
      (vector-append '#(a b c))
    => '#(a b c))

  (check
      (vector-append '#(a b c) '#(d e f))
    => '#(a b c d e f))

  (check
      (vector-append '#(a b c) '#(d e f) '#(g h i))
    => '#(a b c d e f g h i))

  (check
      (vector-append '#(a b c) '#(d e f) '#(g h i) '#(l m n))
    => '#(a b c d e f g h i l m n))

  (check
      (vector-append '#(a b c) '#(d e f) '#(g h i) '#(l m n) '#(o p q))
    => '#(a b c d e f g h i l m n o p q))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check
      (catch #f
	(vector-append 123))
    => '(123))

  (check
      (catch #f
	(vector-append '#(a) 123))
    => '(123))

  (check
      (catch #f
	(vector-append '#(a) '#(b) 123))
    => '(123))

  (check
      (catch #f
	(vector-append '#(a) '#(b) '#(c) 123))
    => '(123))

  (check
      (catch #f
	(vector-append '#(a) '#(b) '#(c) '#(d) 123))
    => '(123))

  (check
      (catch #f
	(vector-append '#(a) '#(b) '#(c) '#(d) '#(e) 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'vector-for-each))

  (check
      (with-result
       (vector-for-each (lambda (ch)
			  (add-result ch))
	 '#()))
    => `(,(void) ()))

  (check
      (with-result
       (vector-for-each (lambda (ch)
			  (add-result ch))
	 '#(a)))
    => `(,(void) (a)))

  (check
      (with-result
       (vector-for-each (lambda (ch)
			  (add-result ch))
	 '#(a b c)))
    => `(,(void) (a b c)))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (vector-for-each (lambda (ch1 ch2)
			  (add-result (cons ch1 ch2)))
	 '#() '#()))
    => `(,(void) ()))

  (check
      (with-result
       (vector-for-each (lambda (ch1 ch2)
			  (add-result (cons ch1 ch2)))
	 '#(a) '#(b)))
    => `(,(void) ((a . b))))

  (check
      (with-result
       (vector-for-each (lambda (ch1 ch2)
			  (add-result (cons ch1 ch2)))
	 '#(a b c) '#(d e f )))
    => `(,(void) ((a . d)
		  (b . e)
		  (c . f))))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (vector-for-each (lambda (ch1 ch2 ch3)
			  (add-result (list ch1 ch2 ch3)))
	 '#() '#() '#()))
    => `(,(void) ()))

  (check
      (with-result
       (vector-for-each (lambda (ch1 ch2 ch3)
			  (add-result (list ch1 ch2 ch3)))
	 '#(a) '#(b) '#(c)))
    => `(,(void) ((a b c))))

  (check
      (with-result
       (vector-for-each (lambda (ch1 ch2 ch3)
			  (add-result (list ch1 ch2 ch3)))
	 '#(a b c) '#(d e f ) '#(g h i)))
    => `(,(void) ((a d g)
		  (b e h)
		  (c f i))))

;;; --------------------------------------------------------------------
;;; arguments validation: procedure

  (check
      (catch #f
	(vector-for-each 123 '#()))
    => '(123))

  (check
      (catch #f
	(vector-for-each 123 '#() '#()))
    => '(123))

  (check
      (catch #f
	(vector-for-each 123 '#() '#() '#()))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: vectors

  (check
      (catch #f
	(vector-for-each values 123))
    => '(123))

  (check
      (catch #f
	(vector-for-each values '#() 123))
    => '(123))

  (check
      (catch #f
	(vector-for-each values '#() '#() 123))
    => '(123))

  (check
      (catch #f
	(vector-for-each values '#() '#() '#() 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'vector-fill-bang))

  (check
      (let ((vec '#()))
	(vector-fill! vec 'a)
	vec)
    => '#())

  (check
      (let ((vec (vector 'b)))
	(vector-fill! vec 'a)
	vec)
    => '#(a))

  (check
      (let ((vec (vector 'b 'c 'd)))
	(vector-fill! vec 'a)
	vec)
    => '#(a a a))

;;; --------------------------------------------------------------------
;;; arguments validation: vector

  (check
      (catch #f
	(vector-fill! 123 'a))
    => '(123))

  #t)


(parametrise ((check-test-name	'vector-copy-bang))

  (check
      (let ((dst '#()))
	(vector-copy! '#() 0 dst 0 0)
	dst)
    => '#())

  (check
      (let ((dst (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! '#() 0 dst 0 0)
	dst)
    => '#(a b c d e f g h i l m))

  (check
      (let ((dst (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! '#(A B C) 0 dst 0 3)
	dst)
    => '#(A B C d e f g h i l m))

  (check
      (let ((dst (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! '#(A B C) 0 dst 1 3)
	dst)
    => '#(a A B C e f g h i l m))

  (check
      (let ((dst (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! '#(A B C) 0 dst 7 3)
	dst)
    => '#(a b c d e f g A B C m))

  (check
      (let ((dst (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! '#(A B C) 0 dst 8 3)
	dst)
    => '#(a b c d e f g h A B C))

  (check
      (let ((dst (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! '#(0 1 2 A B C) 3 dst 8 3)
	dst)
    => '#(a b c d e f g h A B C))

;;; --------------------------------------------------------------------
;;; overlapping source and destination, forwards copy

  (check
      (let ((vec (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! vec 0 vec 3 3)
	vec)
    => '#(a b c a b c g h i l m))

  (check
      (let ((vec (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! vec 0 vec 2 3)
	vec)
    => '#(a b a b c f g h i l m))

  (check
      (let ((vec (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! vec 0 vec 1 3)
	vec)
    => '#(a a b c e f g h i l m))

  (check
      (let ((vec (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! vec 0 vec 0 3)
	vec)
    => '#(a b c d e f g h i l m))

;;; --------------------------------------------------------------------
;;; overlapping source and destination, backwards copy

  (check
      (let ((vec (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! vec 3 vec 0 3)
	vec)
    => '#(d e f d e f g h i l m))

  (check
      (let ((vec (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! vec 2 vec 0 3)
	vec)
    => '#(c d e d e f g h i l m))

  (check
      (let ((vec (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! vec 1 vec 0 3)
	vec)
    => '#(b c d d e f g h i l m))

  (check
      (let ((vec (vector-copy '#(a b c d e f g h i l m))))
	(vector-copy! vec 0 vec 0 3)
	vec)
    => '#(a b c d e f g h i l m))

  #t)


(parametrise ((check-test-name	'for-all))

  (check
      (vector-for-all even? '#())
    => #t)

  (check
      (vector-for-all even? '#(3 1 4 1 5 9))
    => #f)

  (check
      (vector-for-all even? '#(2 4 14))
    => #t)

  (check
      (vector-for-all (lambda (n) (and (even? n) n))
		      '#(2 4 14))
    => 14)

  (check
      (vector-for-all < '#(1 2 3) '#(2 3 4))
    => #t)

  (check
      (vector-for-all < '#(1 2 4) '#(2 3 4))
    => #f)

  (check
      (vector-exists even? '#(3 1 4 1 5 9))
    => #t)

  (check
      (vector-exists even? '#(3 1 1 5 9))
    => #f)

  (check
      (vector-exists even? '#())
    => #f)

  (check
      (vector-exists (lambda (n) (and (even? n) n))
		     '#(2 1 4 14))
    => 2)

  (check
      (vector-exists < '#(1 2 4) '#(2 3 4))
    => #t)

  (check
      (vector-exists > '#(1 2 3) '#(2 3 4))
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
