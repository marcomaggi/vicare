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
;;;Copyright (C) 2011-2013, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-vectors)
  (options strict-r6rs)
  (import (vicare)
    (except (vicare checks)
	    check-for-assertion-violation)
    (vicare system $vectors))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare vector functions\n")


;;;; helpers

(define (A arg)
  (receive (port extract)
      (open-string-output-port)
    (write arg)
    (read (open-string-input-port (extract)))))

;;; --------------------------------------------------------------------

(define-syntax check-for-assertion-violation
  (syntax-rules (=>)
    ((_ ?body => ?expected-who/irritants)
     (check
	 (guard (E ((assertion-violation? E)
		    (cdr (condition-irritants E)))
		   (else E))
	   ?body)
       => ?expected-who/irritants))
    ))

(define-syntax check-for-assertion-irritants
  (syntax-rules (=>)
    ((_ ?body => ?expected-who/irritants)
     (check
	 (guard (E ((assertion-violation? E)
		    (condition-irritants E))
		   (else E))
	   ?body)
       => ?expected-who/irritants))
    ))

(define-syntax check-argument-violation
  (syntax-rules (=>)
    ((_ ?body => ?result)
     (check
	 (guard (E ((procedure-signature-argument-violation? E)
		    #;(print-condition E)
		    (procedure-signature-argument-violation.offending-value E))
		   ((procedure-arguments-consistency-violation? E)
		    #;(print-condition E)
		    (condition-irritants E))
		   ((procedure-argument-violation? E)
		    (when #f
		      (debug-print (condition-message E)))
		    (let ((D (cdr (condition-irritants E))))
		      (if (pair? D)
			  (car D)
			(condition-irritants E))))
		   ((assertion-violation? E)
		    (condition-irritants E))
		   (else
		    (print-condition E)
		    E))
	   ?body)
       => ?result))))


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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-length (read port)))
    => 123)

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-ref (read port) 1))
    => 123)

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check-argument-violation
      (apply vector-ref '#(c i a o) #\d '())
    => #\d)

  (check-argument-violation
      (apply vector-ref '#(c i a o) 'd '())
    => 'd)

  (check-argument-violation
      (apply vector-ref '#() -1 '())
    => -1)

  (check-argument-violation
      (apply vector-ref '#() (+ 1 (greatest-fixnum)) '())
    => (+ 1 (greatest-fixnum)))

  (check-argument-violation
      (vector-ref '#() 0)
    => '(#() 0))

  (check-argument-violation
      (vector-ref '#(a b c) 10)
    => '(#(a b c) 10))

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-set! (read port) 1 'a))
    => 123)

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check-argument-violation
      (let ((port (open-string-input-port "a")))
	(vector-set! (vector 'a 'b 'c) (read port) 'b))
    => 'a)

  (check-argument-violation
      (let ((port (open-string-input-port "-1")))
	(vector-set! (vector 'a 'b 'c) (read port) 'a))
    => -1)

  (check-argument-violation
      (vector-set! (vector 'a 'b 'c) (+ 1 (greatest-fixnum)) 'a)
    => (+ 1 (greatest-fixnum)))

  (check-argument-violation
      (vector-set! (vector) 0 'a)
    => '(#() 0))

  (check-argument-violation
      (vector-set! (vector 'a 'b 'c) 10 '9)
    => '(#(a b c) 10))

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

  ;;not a number
  (check-argument-violation
      (let ((port (open-string-input-port "#\\t")))
	(make-vector (read port) 'A))
    => #\t)

  ;;negative
  (check-argument-violation
      (let ((port (open-string-input-port "-1")))
	(make-vector (read port) 'A))
    => -1)

  ;;not a fixnum
  (check-argument-violation
      (make-vector (+ 1 (greatest-fixnum)) 'A)
    => (+ 1 (greatest-fixnum)))

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(subvector (read port) 1 1))
    => 123)

;;; --------------------------------------------------------------------
;;; arguments validation: start index

  ;;not a number
  (check-argument-violation
      (let ((port (open-string-input-port "#t")))
	(subvector '#(a b c d) (read port) 1))
    => #t)

  ;;negative
  (check-argument-violation
      (let ((port (open-string-input-port "-1")))
	(subvector '#(a b c d) (read port) 1))
    => -1)

  ;;not a fixnum
  (check-argument-violation
      (subvector '#(a b c d) (+ 1 (greatest-fixnum)) 1)
    => (+ 1 (greatest-fixnum)))

  ;;too big for vector
  (check-argument-violation
      (subvector '#(a b c d) 5 6)
    => '(#(a b c d) 5))

;;; --------------------------------------------------------------------
;;; arguments validation: end index

  ;;not a number
  (check-argument-violation
      (let ((port (open-string-input-port "#t")))
	(subvector '#(a b c d) 1 (read port)))
    => #t)

  ;;negative
  (check-argument-violation
      (let ((port (open-string-input-port "-1")))
	(subvector '#(a b c d) 1 (read port)))
    => -1)

  ;;not a fixnum
  (check-argument-violation
      (subvector '#(a b c d) 1 (+ 1 (greatest-fixnum)))
    => (+ 1 (greatest-fixnum)))

  ;;too big for vector
  (check-argument-violation
      (subvector '#(a b c d) 2 6)
    => '(#(a b c d) 6))

;;; --------------------------------------------------------------------
;;; arguments validation: indexes

  ;;incorrect order
  (check-argument-violation
      (subvector '#(a b c d) 2 1)
    => '(2 1))

  #t)


(parametrise ((check-test-name	'vector-copy-base))

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-copy (read port)))
    => 123)

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-resize (read port) 1))
    => 123)

;;; --------------------------------------------------------------------
;;; arguments validation: new-len

  (check-argument-violation
      (let ((port (open-string-input-port "-123")))
	(vector-resize '#(1 2 3) (read port)))
    => -123)

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector->list (read port)))
    => 123)

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

  ;;not a list
  (check-argument-violation
      (list->vector 123)
    => 123)

  ;;not a proper list
  (check-argument-violation
      (list->vector '(a b . c))
    => '(a b . c))

  ;;not a proper list
  (check-argument-violation
      (list->vector '(a . c))
    => '(a . c))

  ;;circular list
  (let ((circ '#0=(a b c . #0#)))
    (check-argument-violation
	(let ((circ '#1=(a b c . #1#)))
	  (list->vector circ))
      => circ))

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-append (read port)))
    => 123)

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-append '#(a) (read port)))
    => 123)

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-append '#(a) '#(b) (read port)))
    => 123)

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-append '#(a) '#(b) '#(c) (read port)))
    => 123)

  (check-for-procedure-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-append '#(a) '#(b) '#(c) '#(d) (read port)))
    => '(vector-append (123)))

  (check-for-procedure-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-append '#(a) '#(b) '#(c) '#(d) '#(e) (read port)))
    => '(vector-append (123)))

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-for-each (read port) '#()))
    => 123)

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-for-each (read port) '#() '#()))
    => 123)

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-for-each (read port) '#() '#() '#()))
    => 123)

;;; --------------------------------------------------------------------
;;; arguments validation: vectors

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-for-each values (read port)))
    => 123)

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-for-each values '#() (read port)))
    => 123)

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-for-each values '#() '#() (read port)))
    => 123)

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-for-each values '#() '#() '#() (read port)))
    => 123)

;;; --------------------------------------------------------------------
;;; unsafe operation

  (check
      (with-result
       ($vector-for-each1 (lambda (ch)
			    (add-result ch))
			  '#()))
    => '(#t ()))

  (check
      (with-result
       ($vector-for-each1 (lambda (ch)
			    (add-result ch))
			  '#(a)))
    => '(#t (a)))

  (check
      (with-result
       ($vector-for-each1 (lambda (ch)
			    (add-result ch))
			  '#(a b c)))
    => '(#t (a b c)))

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

  (check-argument-violation
      (let ((port (open-string-input-port "123")))
	(vector-fill! (read port) 'a))
    => 123)

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

;;;

  (check
      (vector-for-all < '#() '#())
    => #t)

  (check
      (vector-for-all < '#(1 2 3) '#(2 3 4))
    => #t)

  (check
      (vector-for-all < '#(1 2 4) '#(2 3 4))
    => #f)

;;; --------------------------------------------------------------------
;;; unsafe for-all

  (check
      ($vector-for-all1 even? '#())
    => #t)

  (check
      ($vector-for-all1 even? '#(3 1 4 1 5 9))
    => #f)

  (check
      ($vector-for-all1 even? '#(2 4 14))
    => #t)

  (check
      ($vector-for-all1 (lambda (n) (and (even? n) n))
			'#(2 4 14))
    => 14)

  #t)


(parametrise ((check-test-name	'exists))

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

;;;

  (check
      (vector-exists < '#() '#())
    => #f)

  (check
      (vector-exists < '#(1 2 4) '#(2 3 4))
    => #t)

  (check
      (vector-exists > '#(1 2 3) '#(2 3 4))
    => #f)

;;; --------------------------------------------------------------------
;;; unsafe exists

  (check
      ($vector-exists1 even? '#(3 1 4 1 5 9))
    => #t)

  (check
      ($vector-exists1 even? '#(3 1 1 5 9))
    => #f)

  (check
      ($vector-exists1 even? '#())
    => #f)

  (check
      ($vector-exists1 (lambda (n) (and (even? n) n))
		       '#(2 1 4 14))
    => 2)

  #t)


(parametrise ((check-test-name	'find))

  (check
      (vector-find even? '#(3 1 4 1 5 9))
    => 4)

  (check
      (vector-find even? '#(3 1 1 5 9))
    => #f)

  (check
      (vector-find even? '#())
    => #f)

  #t)


(parametrise ((check-test-name	'fold-left))

  (check
      (vector-fold-left (lambda (nil x) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold-left (lambda (nil x y) (cons (cons x y) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D))
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (vector-fold-left (lambda (nil x y z)
			  (cons (list x y z) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D)
	'#(0 1 2 3))
    => '((#\d #\D 3)
	 (#\c #\C 2)
	 (#\b #\B 1)
	 (#\a #\A 0)))

  (check
      (vector-fold-left (lambda (nil x) (cons x nil))
	'()
	'#())
    => '())

  (check
      (vector-fold-left (lambda (count c)
			  (if (char-upper-case? c)
			      (+ count 1)
			    count))
	0
	'#(#\A #\B #\C #\d #\e #\f #\G #\H #\i))
    => 5)

  #t)


(parametrise ((check-test-name	'fold-right))

  (check
      (vector-fold-right (lambda (x nil) (cons x nil))
	'()
	'#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right (lambda (x y nil) (cons (cons x y) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D))
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (vector-fold-right (lambda (x y z nil)
			   (cons (list x y z) nil))
	'()
	'#(#\a #\b #\c #\d)
	'#(#\A #\B #\C #\D)
	'#(0 1 2 3))
    => '((#\a #\A 0)
	 (#\b #\B 1)
	 (#\c #\C 2)
	 (#\d #\D 3)))

  (check
      (vector-fold-right (lambda (x nil) (cons x nil))
	'()
	'#())
    => '())

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;;Local Variables:
;;eval: (put 'check-argument-violation	'scheme-indent-function 1)
;;End:
