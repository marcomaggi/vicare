;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for stack containers
;;;Date: Wed Sep 25, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (vicare containers stacks)
  (vicare containers iteration-thunks)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: stack containers\n")


;;;; helpers

(define (make-list-20)
  ;;Build and return a new list object holding fixnums from 0 to 99 included.
  ;;
  (let loop ((i   19)
	     (ell '()))
    (if (fxzero? i)
	(cons i ell)
      (loop (fxsub1 i) (cons i ell)))))

(define (make-vector-20)
  ;;Build and return a new vector object holding fixnums from 0 to 99 included.
  ;;
  (do ((vec (make-vector 20))
       (i 0 (fxadd1 i)))
      ((fx=? i 20)
       vec)
    (vector-set! vec i i)))

;;; --------------------------------------------------------------------

(define (make-list-100)
  ;;Build and return a new list object holding fixnums from 0 to 99 included.
  ;;
  (let loop ((i   99)
	     (ell '()))
    (if (fxzero? i)
	(cons i ell)
      (loop (fxsub1 i) (cons i ell)))))

(define (make-vector-100)
  ;;Build and return a new vector object holding fixnums from 0 to 99 included.
  ;;
  (do ((vec (make-vector 100))
       (i 0 (fxadd1 i)))
      ((fx=? i 100)
       vec)
    (vector-set! vec i i)))

;;; --------------------------------------------------------------------

;;A list of 5 objects, enstackd in a stack, is fully stored in a single buffer.
;;
(define-constant LIST-5			'(0 1 2 3 4))
(define-constant LIST-5-REVERSED	(reverse LIST-5))
(define-constant LIST-5-NEGATED		(map - LIST-5))

;;A list of 20 objects, enstackd in a stack, is stored in two buffers.
;;
(define-constant LIST-20		(make-list-20))
(define-constant LIST-20-REVERSED	(reverse LIST-20))
(define-constant LIST-20-NEGATED	(map - LIST-20))

;;A list of 100 objects, enstackd in a stack, is stored in multiple buffers.
;;
(define-constant LIST-100		(make-list-100))
(define-constant LIST-100-REVERSED	(reverse LIST-100))
(define-constant LIST-100-NEGATED	(map - LIST-100))

;;A vector of 5 objects, enstackd in a stack, is fully stored in a single buffer.
;;
(define-constant VECTOR-5		(list->vector LIST-5))
(define-constant VECTOR-5-REVERSED	(list->vector LIST-5-REVERSED))

;;A vector of 20 objects, enstackd in a stack, is stored in two buffers.
;;
(define-constant VECTOR-20		(list->vector LIST-20))
(define-constant VECTOR-20-REVERSED	(list->vector LIST-20-REVERSED))

;;A vector of 100 objects, enstackd in a stack, is stored in multiple buffers.
;;
(define-constant VECTOR-100		(list->vector LIST-100))
(define-constant VECTOR-100-REVERSED	(list->vector LIST-100-REVERSED))


(parametrise ((check-test-name	'making))

  (check
      (stack? (make-stack))
    => #t)

  (check
      (stack->list (make-stack))
    => '())

  (check
      (stack->list (stack 1))
    => '(1))

  (check
      (stack->list (stack 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name 'pred))

  (check-for-true  (stack-empty? (make-stack)))
  (check-for-false (stack-empty? (stack 1)))
  (check-for-false (stack-empty? (stack 1 2 3)))

;;; --------------------------------------------------------------------

  (check-for-false (stack-not-empty? (make-stack)))
  (check-for-true  (stack-not-empty? (stack 1)))
  (check-for-true  (stack-not-empty? (stack 1 2 3)))

  #t)


(parametrise ((check-test-name	'object))

  (define who 'test)

;;; hash

  (check-for-true
   (integer? (stack-hash (stack 1 2 3))))

  (check
      (let ((A (stack 1 2 3))
	    (B (stack 1 2 3))
	    (T (make-hashtable stack-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (stack 1 2 3)))
	(stack-property-list S))
    => '())

  (check
      (let ((S (stack 1 2 3)))
	(stack-putprop S 'ciao 'salut)
	(stack-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (stack 1 2 3)))
	(stack-getprop S 'ciao))
    => #f)

  (check
      (let ((S (stack 1 2 3)))
	(stack-putprop S 'ciao 'salut)
	(stack-remprop S 'ciao)
	(stack-getprop S 'ciao))
    => #f)

  (check
      (let ((S (stack 1 2 3)))
	(stack-putprop S 'ciao 'salut)
	(stack-putprop S 'hello 'ohayo)
	(list (stack-getprop S 'ciao)
	      (stack-getprop S 'hello)))
    => '(salut ohayo))

  #f)


(parametrise ((check-test-name	'inspect))

  (check
      (stack-size (make-stack))
    => 0)

  (check
      (stack-size (stack 1))
    => 1)

  (check
      (stack-size (stack 1 2 3))
    => 3)

  (check
      (let ((D (make-stack)))
	(for-each (lambda (obj)
		    (stack-push! D obj))
	  LIST-5)
	(stack-size D))
    => 5)

  (check
      (let ((D (make-stack)))
	(for-each (lambda (obj)
		    (stack-push! D obj))
	  LIST-20)
	(stack-size D))
    => 20)

  (check
      (let ((D (make-stack)))
	(for-each (lambda (obj)
		    (stack-push! D obj))
	  LIST-100)
	(stack-size D))
    => 100)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(stack-top (make-stack)))
    => "the container is empty")

  (check
      (stack-top (stack 1))
    => 1)

  (check
      (stack-top (stack 1 2 3))
    => 1)

  #t)


(parametrise ((check-test-name 'access))

  (check
      (let ((D (make-stack)))
	(stack-push! D 1)
	(stack-top D))
    => 1)

  (check
      (let ((D (make-stack)))
	(stack-push! D 1)
	(stack-push! D 2)
	(let* ((A (stack-pop! D))
	       (B (stack-top D)))
	  (values A B)))
    => 2 1)

  (check
      (let ((D (make-stack)))
	(stack-push! D 1)
	(let ((A (stack-pop! D)))
	  (stack-push! D 2)
	  (let ((B (stack-top D)))
	    (values A B))))
    => 1 2)

  (check
      (let ((D (make-stack 5)))
	(stack-push! D 5)
	(stack-push! D 4)
	(stack-push! D 3)
	(stack-push! D 2)
	(stack-push! D 1)
	(stack-top D))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (make-stack)))
	  (stack-pop! q)))
    => "the container is empty")

  (check
      (let ((q (stack 1 2 3)))
	(stack-pop! q))
    => 1)

  (check
      (let ((q (stack 1 2 3)))
	(stack-pop! q)
	(stack-pop! q)
	(stack-pop! q))
    => 3)

  (check
      (let ((q (stack 1 2 3)))
	(stack-pop! q)
	(stack-pop! q)
	(stack-pop! q)
	(stack-empty? q))
    => #t)

;;; --------------------------------------------------------------------

  ;;Pushing then popping 5 objects.
  ;;
  (check
      (let ((D (make-stack)))
	(for-each (lambda (obj)
		    (stack-push! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (stack-pop! D))
	  LIST-5))
    => LIST-5-REVERSED)

  ;;pushing then popping 20 objects.
  ;;
  (check
      (let ((D (make-stack)))
	(for-each (lambda (obj)
		    (stack-push! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (stack-pop! D))
	  LIST-20))
    => LIST-20-REVERSED)

  ;;pushing then popping 100 objects.
  ;;
  (check
      (let ((D (make-stack)))
	(for-each (lambda (obj)
		    (stack-push! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (stack-pop! D))
	  LIST-100))
    => LIST-100-REVERSED)

  #t)


(parametrise ((check-test-name	'folding))

;;; front folding

  (check
      (let ((D (make-stack)))
	(stack-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => '())

  (check
      (let* ((L LIST-5)
	     (D (list->stack L)))
	(stack-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-5-REVERSED)

  (check
      (let ((D (list->stack LIST-20)))
	(stack-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-20-REVERSED)

  (check
      (let ((D (list->stack LIST-100)))
	(stack-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-100-REVERSED)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D
	  (stack 0 1 2 3 4 5))
	(stack-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => '(5 4 3 2 1 0))

;;; --------------------------------------------------------------------
;;; rear folding

  (check
      (let ((D (make-stack)))
	(stack-fold-right (lambda (knil obj)
			   (cons obj knil))
	  '() D))
    => '())

  (check
      (let* ((L LIST-5)
	     (D (list->stack L)))
	(stack-fold-right cons '() D))
    => LIST-5)

  (check
      (let ((D (list->stack LIST-20)))
	(stack-fold-right cons '() D))
    => LIST-20)

  (check
      (let ((D (list->stack LIST-100)))
	(stack-fold-right cons '() D))
    => LIST-100)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D
	  (stack 0 1 2 3 4 5))
	(stack-fold-right (lambda (obj knil)
			   (cons obj knil))
	  '() D))
    => '(0 1 2 3 4 5))

  (collect))


(parametrise ((check-test-name	'copy))

  (check
      (stack->list (stack-copy! (make-stack) (stack)))
    => '())

  (check
      (stack->list (stack-copy! (make-stack) (list->stack LIST-5)))
    => LIST-5)

  (check
      (stack->list (stack-copy! (make-stack) (list->stack LIST-20)))
    => LIST-20)

  (check
      (stack->list (stack-copy! (make-stack) (list->stack LIST-100)))
    => LIST-100)

  #t)


(parametrise ((check-test-name	'reverse))

  (check
      (stack->list (stack-reverse! (stack) (stack)))
    => '())

  (check
      (stack->list (stack-reverse! (stack) (stack 0)))
    => '(0))

  (check
      (stack->list (stack-reverse! (stack) (stack 0 1 2 3 4)))
    => '(4 3 2 1 0))

  #t)





(parametrise ((check-test-name	'mapping))

  (check (stack->list (stack-map-left (stack) - (stack)))			=> '())
  (check (stack->list (stack-map-left (stack) - (list->stack LIST-5)))		=> LIST-5-NEGATED)
  (check (stack->list (stack-map-left (stack) - (list->stack LIST-20)))	=> LIST-20-NEGATED)
  (check (stack->list (stack-map-left (stack) - (list->stack LIST-100)))	=> LIST-100-NEGATED)

  (check (stack->list (stack-map-right (stack) - (stack)))			=> '())
  (check (stack->list (stack-map-right (stack) - (list->stack LIST-5)))		=> LIST-5-NEGATED)
  (check (stack->list (stack-map-right (stack) - (list->stack LIST-20)))		=> LIST-20-NEGATED)
  (check (stack->list (stack-map-right (stack) - (list->stack LIST-100)))	=> LIST-100-NEGATED)

  (check (stack->list (stack-map (stack) - (stack)))			=> '())
  (check (stack->list (stack-map (stack) - (list->stack LIST-5)))		=> LIST-5-NEGATED)
  (check (stack->list (stack-map (stack) - (list->stack LIST-20)))	=> LIST-20-NEGATED)
  (check (stack->list (stack-map (stack) - (list->stack LIST-100)))	=> LIST-100-NEGATED)

  #t)


(parametrise ((check-test-name	'for-each))

;;; for-each-left

  (check
      (with-result
	(void-object? (stack-for-each-left add-result (stack))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (stack-for-each-left add-result (list->stack LIST-5))))
    => `(#t ,LIST-5))

  (check
      (with-result
	(void-object? (stack-for-each-left add-result (list->stack LIST-20))))
    => `(#t ,LIST-20))

  (check
      (with-result
	(void-object? (stack-for-each-left add-result (list->stack LIST-100))))
    => `(#t ,LIST-100))

;;; --------------------------------------------------------------------
;;; for-each-right

  (check
      (with-result
	(void-object? (stack-for-each-right add-result (stack))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (stack-for-each-right add-result (list->stack LIST-5))))
    => `(#t ,LIST-5-REVERSED))

  (check
      (with-result
	(void-object? (stack-for-each-right add-result (list->stack LIST-20))))
    => `(#t ,LIST-20-REVERSED))

  (check
      (with-result
	(void-object? (stack-for-each-right add-result (list->stack LIST-100))))
    => `(#t ,LIST-100-REVERSED))

;;; --------------------------------------------------------------------
;;; for-each-left aliases

  (check
      (with-result
	(void-object? (stack-for-each add-result (stack))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (stack-for-each add-result (list->stack LIST-5))))
    => `(#t ,LIST-5))

  (check
      (with-result
	(void-object? (stack-for-each add-result (list->stack LIST-20))))
    => `(#t ,LIST-20))

  (check
      (with-result
	(void-object? (stack-for-each add-result (list->stack LIST-100))))
    => `(#t ,LIST-100))

  #t)


(parametrise ((check-test-name	'purge))

  (check
      (let ((D (list->stack LIST-100)))
	(stack-purge! D)
	(stack-size D))
    => 0)

  (check
      (let ((D (list->stack LIST-100)))
	(stack-purge! D)
	(for-each (lambda (obj)
		    (stack-push! D obj))
	  LIST-20)
	(stack-size D))
    => 20)

  (check
      (let ((D (list->stack LIST-100)))
	(stack-purge! D)
	(for-each (lambda (obj)
		    (stack-push! D obj))
	  LIST-20)
	(stack->list D))
    => LIST-20-REVERSED)

  #t)


(parametrise ((check-test-name	'for-all))

  (define (fun . obj*)
    (add-result (list->vector obj*))
    ;;Remember that FOR-ALL returns the result of the last application.
    (for-all values obj*))

;;; --------------------------------------------------------------------
;;; for-all'ing one argument

  (check
      (with-result
	(stack-for-all
	    add-result
	  (stack)))
    => '(#t ()))


  (check
      (with-result
	(stack-for-all
	    add-result
	  (stack 10)))
    => '(10 (10)))


  (check
      (with-result
	(stack-for-all
	    add-result
	  (stack 10 20 30)))
    => '(30 (10 20 30)))

  (check
      (with-result
	(stack-for-all
	    add-result
	  (stack 10 #f 30)))
    => '(#f (10 #f)))

  #t)


(parametrise ((check-test-name	'exists))

  (define (fun . obj*)
    (add-result (list->vector obj*))
    (find (lambda (obj)
	    (<= 30 obj))
      obj*))

;;; --------------------------------------------------------------------
;;; from left

  (check
      (with-result
	(stack-exists-left
	    fun
	  (stack)))
    => '(#f ()))

  (check
      (with-result
	(stack-exists-left
	    fun
	  (stack 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(stack-exists-left
	    fun
	  (stack 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(stack-exists-left
	    fun
	  (stack 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(stack-exists-left
	    fun
	  (stack 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

;;; --------------------------------------------------------------------
;;; from right

  (check
      (with-result
	(stack-exists-right
	    fun
	  (stack)))
    => '(#f ()))

  (check
      (with-result
	(stack-exists-right
	    fun
	  (stack 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(stack-exists-right
	    fun
	  (stack 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(stack-exists-right
	    fun
	  (stack 30 20 10)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(stack-exists-right
	    fun
	  (stack 25 20 10)))
    => '(#f (#(10) #(20) #(25))))

;;; --------------------------------------------------------------------
;;; left/right difference

  (check
      (stack-exists-left
	  (lambda (obj)
	    (and (even? obj)
		 obj))
	(stack 1 2 3 5 6 7))
    => 2)

  (check
      (stack-exists-right
	  (lambda (obj)
	    (and (even? obj)
		 obj))
	(stack 1 2 3 5 6 7))
    => 6)

;;; --------------------------------------------------------------------
;;; from left aliases

  (check
      (with-result
	(stack-exists
	    fun
	  (stack)))
    => '(#f ()))

  (check
      (with-result
	(stack-exists
	    fun
	  (stack 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(stack-exists
	    fun
	  (stack 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(stack-exists
	    fun
	  (stack 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(stack-exists
	    fun
	  (stack 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

  #t)


(parametrise ((check-test-name	'find))

  (check (stack-find-left even? (stack))				=> #f)
  (check (stack-find-left even? (stack) 'not-found)			=> 'not-found)

  (check (stack-find-left even? (stack 1 3 5 7))			=> #f)
  (check (stack-find-left even? (stack 1 3 5 7) 'not-found)		=> 'not-found)

  (check (stack-find-left even? (stack 1 3 5 6 7))			=> 6)
  (check (stack-find-left even? (stack 1 3 5 6 7) 'not-found)		=> 6)

;;; --------------------------------------------------------------------

  (check (stack-find-right even? (stack))				=> #f)
  (check (stack-find-right even? (stack) 'not-found)			=> 'not-found)

  (check (stack-find-right even? (stack 1 3 5 7))			=> #f)
  (check (stack-find-right even? (stack 1 3 5 7) 'not-found)		=> 'not-found)

  (check (stack-find-right even? (stack 1 3 5 6 7))			=> 6)
  (check (stack-find-right even? (stack 1 3 5 6 7) 'not-found)		=> 6)

;;; --------------------------------------------------------------------
;;; left/rigt difference

  (check (stack-find-left  even? (stack 1 2 3 5 6 7))			=> 2)
  (check (stack-find-right even? (stack 1 2 3 5 6 7))			=> 6)

;;; --------------------------------------------------------------------
;;; left aliases

  (check (stack-find even? (stack))					=> #f)
  (check (stack-find even? (stack) 'not-found)				=> 'not-found)

  (check (stack-find even? (stack 1 3 5 7))				=> #f)
  (check (stack-find even? (stack 1 3 5 7) 'not-found)			=> 'not-found)

  (check (stack-find even? (stack 1 3 5 6 7))				=> 6)
  (check (stack-find even? (stack 1 3 5 6 7) 'not-found)		=> 6)

  #t)


(parametrise ((check-test-name	'filter))

  (check
      (stack->list (stack-filter (stack) even? (stack)))
    => '())

  (check
      (stack->list (stack-filter (stack) even? (stack 1 3 5 7)))
    => '())

  (check
      (stack->list (stack-filter (stack) even? (stack 1 3 5 6 7)))
    => '(6))

  (check
      (stack->list (stack-filter (stack) even? (stack 1 2 3 4 5 6 7)))
    => '(2 4 6))

  (check
      (stack->list (stack-filter (stack) even? (stack 1 2 3 4 5 6 7 8)))
    => '(2 4 6 8))

  (check
      (stack->list (stack-filter (stack) even? (stack 2 4 6 8)))
    => '(2 4 6 8))

  #t)


(parametrise ((check-test-name	'partition))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?body ?expected-in ?expected-ou)
       (check
	   (receive (in ou)
	       ?body
	     (values (stack->list in) (stack->list ou)))
	 => ?expected-in ?expected-ou))
      ))

;;; --------------------------------------------------------------------

  (doit (stack-partition (stack) (stack) even? (stack))				'() '())

  (doit (stack-partition (stack) (stack) even? (stack 1 3 5 7))			'() '(1 3 5 7))
  (doit (stack-partition (stack) (stack) even? (stack 2 4 6 8))			'(2 4 6 8) '())

  (doit (stack-partition (stack) (stack) even? (stack 1 3 5 6 7))		'(6) '(1 3 5 7))

  (doit (stack-partition (stack) (stack) even? (stack 1 2 3 4 5 6 7))		'(2 4 6) '(1 3 5 7))

  (doit (stack-partition (stack) (stack) even? (stack 1 2 3 4 5 6 7 8))		'(2 4 6 8) '(1 3 5 7))

  #t)



(parametrise ((check-test-name 'conversion))

  (check
      (stack->list (make-stack))
    => '())

  (check
      (stack->list (stack 1))
    => '(1))

  (check
      (stack->list (stack 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (stack->vector (make-stack))
    => '#())

  (check
      (stack->vector (stack 1))
    => '#(1))

  (check
      (stack->vector (stack 1 2 3))
    => '#(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (stack->list (list->stack '()))
    => '())

  (check
      (stack->list (list->stack '(1)))
    => '(1))

  (check
      (stack->list (list->stack '(1 2 3)))
    => '(1 2 3))

  (check
      (stack->list (list->stack (make-list-100)))
    => (make-list-100))

  (check
      (stack->list (list->stack LIST-100))
    => LIST-100)

;;; --------------------------------------------------------------------

  (check
      (stack->vector (vector->stack '#()))
    => '#())

  (check
      (stack->vector (vector->stack '#(1)))
    => '#(1))

  (check
      (stack->vector (vector->stack '#(1 2 3)))
    => '#(1 2 3))

  (check
      (stack->vector (vector->stack VECTOR-20))
    => VECTOR-20)

  (check
      (stack->vector (vector->stack VECTOR-5))
    => VECTOR-5)

  (check
      (stack->vector (vector->stack VECTOR-100))
    => VECTOR-100)

  #t)


(parametrise ((check-test-name	'iteration-thunks))

  (define (xcons a b)
    (cons b a))

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-stack-iteration-thunk (stack)))
    => '())

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-stack-iteration-thunk (stack 0 1 2 3 4 5)))
    => '(5 4 3 2 1 0))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'stack-fold-left		'scheme-indent-function 1)
;; eval: (put 'stack-fold-right		'scheme-indent-function 1)
;; eval: (put 'stack-map-left		'scheme-indent-function 1)
;; eval: (put 'stack-map-right		'scheme-indent-function 1)
;; eval: (put 'stack-for-each-left	'scheme-indent-function 1)
;; eval: (put 'stack-for-each-right	'scheme-indent-function 1)
;; eval: (put 'stack-for-all		'scheme-indent-function 1)
;; eval: (put 'stack-find		'scheme-indent-function 1)
;; eval: (put 'stack-find-left		'scheme-indent-function 1)
;; eval: (put 'stack-find-right		'scheme-indent-function 1)
;; eval: (put 'stack-exists		'scheme-indent-function 1)
;; eval: (put 'stack-exists-left	'scheme-indent-function 1)
;; eval: (put 'stack-exists-right	'scheme-indent-function 1)
;; End:
