;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (nausicaa language conditions)
;;;Date: Sun Nov 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (rnrs eval)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing condition objects\n")


(parametrise ((check-test-name	'definition))

  (let ()

    (define-condition-type &beta)

    (check
	(let ((E (make-beta-condition)))
	  (beta-condition? E))
      => #t)

    (check
	(let ((E (make-beta-condition)))
	  (condition? E))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (let ()
    (define-condition-type &alpha
      (parent &assertion)
      (fields a b))

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (list (condition-alpha.a E) (condition-alpha.b E)))
      => '(1 2))

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (alpha-condition? E))
      => #t)

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (list (error? E)
		(assertion-violation? E)))
      => '(#f #t))

    #f)

;;; --------------------------------------------------------------------

  (let ()
    (define-condition-type &alpha
      (parent &assertion)
      (fields (a get-a)
	      (b get-b)))

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (list (get-a E) (get-b E)))
      => '(1 2))

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (alpha-condition? E))
      => #t)

    (check
	(let ((E (make-alpha-condition 1 2)))
	  (list (error? E)
		(assertion-violation? E)))
      => '(#f #t))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;tagged fields with accessors
    (define-condition-type &alpha
      (parent &assertion)
      (fields ((a <string>)  get-a)
	      ((b <integer>) get-b)))

    (check
	(let ((E (make-alpha-condition "ciao" 2)))
	  (list (get-a E) (get-b E)))
      => '("ciao" 2))

    (check
	(let ((E (make-alpha-condition "ciao" 2)))
	  (alpha-condition? E))
      => #t)

    (check
	(let ((E (make-alpha-condition "ciao" 2)))
	  (list (error? E)
		(assertion-violation? E)))
      => '(#f #t))

    (check
	(let (((E &alpha) (make-alpha-condition "ciao" 2)))
	  (list (E a) (E b)))
      => '("ciao" 2))

    (check
	(let (((E &alpha) (make-alpha-condition "ciao" 2)))
	  (list (E a length) (E b positive?)))
      => '(4 #t))

    #f)

  (let ()	;tagged fields without accessors
    (define-condition-type &alpha
      (parent &assertion)
      (fields ((a <string>))
	      ((b <integer>))))

    (check
	(let ((E (make-alpha-condition "ciao" 2)))
	  (list (condition-alpha.a E) (condition-alpha.b E)))
      => '("ciao" 2))

    (check
	(let ((E (make-alpha-condition "ciao" 2)))
	  (alpha-condition? E))
      => #t)

    (check
	(let ((E (make-alpha-condition "ciao" 2)))
	  (list (error? E)
		(assertion-violation? E)))
      => '(#f #t))

    (check
	(let (((E &alpha) (make-alpha-condition "ciao" 2)))
	  (list (E a) (E b)))
      => '("ciao" 2))

    (check
	(let (((E &alpha) (make-alpha-condition "ciao" 2)))
	  (list (E a length) (E b positive?)))
      => '(4 #t))

    #f)

  #t)


(parametrise ((check-test-name	'definition-errors))

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-message E) (syntax-violation-subform E)))
		(else #f))
	(eval '(define-condition-type &alpha
		 (parent a) (parent b))
	      (environment '(nausicaa))))
    => '("invalid syntax for condition type definition" #f))

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-message E) (syntax-violation-subform E)))
		(else #f))
	(eval '(define-condition-type &alpha
		 (fields a) (fields b))
	      (environment '(nausicaa))))
    => '("invalid syntax for condition type definition" #f))

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-message E) (syntax->datum (syntax-violation-subform E))))
		(else #f))
	(eval '(define-condition-type 123
		 (fields a b))
	      (environment '(nausicaa))))
    => '("expected identifier as condition type name" 123))

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-message E) (syntax->datum (syntax-violation-subform E))))
		(else #f))
	(eval '(define-condition-type &alpha
		 (parent 123)
		 (fields a b))
	      (environment '(nausicaa))))
    => '("expected identifier as condition supertype name" (parent 123)))

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-message E) (syntax->datum (syntax-violation-subform E))))
		(else #f))
	(eval '(define-condition-type &alpha
		 (fields a 123 b))
	      (environment '(nausicaa))))
    => '("invalid condition type field specification" 123))

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-message E) (syntax->datum (syntax-violation-subform E))))
		(else #f))
	(eval '(define-condition-type &alpha
		 (fields a (c 123) b))
	      (environment '(nausicaa))))
    => '("invalid condition type field specification" (c 123)))

  #t)


(parametrise ((check-test-name	'try-catch))

  (let ()
    (define-condition-type &this
	&error
      make-this-condition
      condition-this?
      (a condition-this.a)
      (b condition-this.b)
      (c condition-this.c))

    (define (doit thunk)
      (try
	  (thunk)
	(catch E
	  (&this
	   (list (E a) (E b) (E c)))
	  (&message
	   (E message))
	  (else E))))

    (check
	(doit (lambda ()
		(raise (make-this-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 123)

    #f)

;;; --------------------------------------------------------------------

  (let ()
    (define-condition-type &that
      (parent &error)
      (fields a b c))

    (define (doit thunk)
      (try
	  (thunk)
	(catch T
	  (&that
	   (list (T a) (T b) (T c)))
	  (&message
	   (T message))
	  (else T))))

    (check
	(doit (lambda ()
		(raise (make-that-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 123)

    #f)

;;; --------------------------------------------------------------------

  (check
      (try
	  (raise (<common-conditions> ('ciao "bau" '(miao))))
	(catch E
	  (<common-conditions>
	   (list (E who) (E message) (E irritants)))
	  (else E)))
    => '(ciao "bau" (miao)))

;;; --------------------------------------------------------------------
;;; finally

  (check	;no exception
      (let ((a 1))
	(try
	    (set! a (+ a 10))
	  (catch E
	    (&error	E)
	    (&warning	E)
	    (else	E))
	  (finally
	   (set! a (+ a 100))))
	a)
    => 111)

  (check	;with exception
      (let ((a 1))
	(try
	    (raise (&warning ()))
	  (catch E
	    (&error	E)
	    (&warning	(set! a (+ a 10)))
	    (else	E))
	  (finally
	   (set! a (+ a 100))))
	a)
    => 111)

  #t)


(parametrise ((check-test-name	'unimplemented))

  (check
      (guard (E ((unimplemented-condition? E)
		 (list (who-condition? E)
		       (condition-who E)))
		(else #f))
	(raise-unimplemented-error 'woppa))
    => '(#t woppa))

  #t)


(parametrise ((check-test-name	'wrong-num-args))

  (check
      (guard (E ((wrong-num-args-condition? E)
		 (list (condition-who E)
		       (condition-message E)
		       (condition-wrong-num-args.procname E)
		       (condition-wrong-num-args.expected E)
		       (condition-wrong-num-args.given E)))
		(else #f))
	(raise-wrong-num-args-error 'woppa "hey!"
				    'the-proc 5 10))
    => '(woppa "hey!" the-proc 5 10))

  #t)


(parametrise ((check-test-name	'common))

  (check
      (let ()
	(<common-conditions> C (<> ('ciao "ciao" '(ciao))))
	(vector ((<common-conditions>) C)
		(is-a? C <common-conditions>)
		(C who)
		(C message)
		(C irritants)))
    => '#(#t #t ciao "ciao" (ciao)))

;;; --------------------------------------------------------------------

  (let ()

    (define-condition-type &whatever
	(parent &condition)
      (fields a b c))

    (define-label &tuple
      (parent <common-conditions>)
      (protocol
       (lambda ()
	 (lambda (who message irritants a b c)
	   (condition (<common-conditions> (who message irritants))
		      (&whatever (a b c))))))
      (predicate (&whatever))
      (virtual-fields (immutable a (slot-ref <> a &whatever))
		      (immutable b (slot-ref <> b &whatever))
		      (immutable c (slot-ref <> c &whatever))))

    (check
	(let ()
	  (&tuple T (<> ('ciao "ciao" '(ciao) 1 2 3)))
	  (vector (T who) (T message) (T irritants)
		  (T a) (T b) (T c)))
      => '#(ciao "ciao" (ciao) 1 2 3))

    (check
	(try
	    (raise (&tuple ('ciao "ciao" '(ciao) 1 2 3)))
	  (catch E
	    (&tuple
	     (vector (E who) (E message) (E irritants)
		     (E a) (E b) (E c)))))
      => '#(ciao "ciao" (ciao) 1 2 3))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
