;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for queue containers
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
  (vicare containers queues)
  (vicare containers iteration-thunks)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: queue containers\n")


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

;;A list of 5 objects, enqueued in a queue, is fully stored in a single buffer.
;;
(define-constant LIST-5			'(0 1 2 3 4))
(define-constant LIST-5-REVERSED	(reverse LIST-5))
(define-constant LIST-5-NEGATED		(map - LIST-5))

;;A list of 20 objects, enqueued in a queue, is stored in two buffers.
;;
(define-constant LIST-20		(make-list-20))
(define-constant LIST-20-REVERSED	(reverse LIST-20))
(define-constant LIST-20-NEGATED	(map - LIST-20))

;;A list of 100 objects, enqueued in a queue, is stored in multiple buffers.
;;
(define-constant LIST-100		(make-list-100))
(define-constant LIST-100-REVERSED	(reverse LIST-100))
(define-constant LIST-100-NEGATED	(map - LIST-100))

;;A vector of 5 objects, enqueued in a queue, is fully stored in a single buffer.
;;
(define-constant VECTOR-5		(list->vector LIST-5))
(define-constant VECTOR-5-REVERSED	(list->vector LIST-5-REVERSED))

;;A vector of 20 objects, enqueued in a queue, is stored in two buffers.
;;
(define-constant VECTOR-20		(list->vector LIST-20))
(define-constant VECTOR-20-REVERSED	(list->vector LIST-20-REVERSED))

;;A vector of 100 objects, enqueued in a queue, is stored in multiple buffers.
;;
(define-constant VECTOR-100		(list->vector LIST-100))
(define-constant VECTOR-100-REVERSED	(list->vector LIST-100-REVERSED))


(parametrise ((check-test-name	'making))

  (check
      (queue? (make-queue))
    => #t)

  (check
      (queue->list (make-queue))
    => '())

  (check
      (queue->list (queue 1))
    => '(1))

  (check
      (queue->list (queue 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name 'pred))

  (check-for-true  (queue-empty? (make-queue)))
  (check-for-false (queue-empty? (queue 1)))
  (check-for-false (queue-empty? (queue 1 2 3)))

;;; --------------------------------------------------------------------

  (check-for-false (queue-not-empty? (make-queue)))
  (check-for-true  (queue-not-empty? (queue 1)))
  (check-for-true  (queue-not-empty? (queue 1 2 3)))

  #t)


(parametrise ((check-test-name	'object))

  (define who 'test)

;;; hash

  (check-for-true
   (integer? (queue-hash (queue 1 2 3))))

  (check
      (let ((A (queue 1 2 3))
	    (B (queue 1 2 3))
	    (T (make-hashtable queue-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (queue 1 2 3)))
	(queue-property-list S))
    => '())

  (check
      (let ((S (queue 1 2 3)))
	(queue-putprop S 'ciao 'salut)
	(queue-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (queue 1 2 3)))
	(queue-getprop S 'ciao))
    => #f)

  (check
      (let ((S (queue 1 2 3)))
	(queue-putprop S 'ciao 'salut)
	(queue-remprop S 'ciao)
	(queue-getprop S 'ciao))
    => #f)

  (check
      (let ((S (queue 1 2 3)))
	(queue-putprop S 'ciao 'salut)
	(queue-putprop S 'hello 'ohayo)
	(list (queue-getprop S 'ciao)
	      (queue-getprop S 'hello)))
    => '(salut ohayo))

  #f)


(parametrise ((check-test-name	'inspect))

  (check
      (queue-size (make-queue))
    => 0)

  (check
      (queue-size (queue 1))
    => 1)

  (check
      (queue-size (queue 1 2 3))
    => 3)

  (check
      (let ((D (make-queue)))
	(map (lambda (obj)
	       (queue-push! D obj))
	  LIST-5)
	(queue-size D))
    => 5)

  (check
      (let ((D (make-queue)))
	(map (lambda (obj)
	       (queue-push! D obj))
	  LIST-20)
	(queue-size D))
    => 20)

  (check
      (let ((D (make-queue)))
	(map (lambda (obj)
	       (queue-push! D obj))
	  LIST-100)
	(queue-size D))
    => 100)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(queue-front (make-queue)))
    => "the container is empty")

  (check
      (queue-front (queue 1))
    => 1)

  (check
      (queue-front (queue 1 2 3))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(queue-rear (make-queue)))
    => "the container is empty")

  (check
      (queue-rear (queue 1))
    => 1)

  (check
      (queue-rear (queue 1 2 3))
    => 3)

  #t)


(parametrise ((check-test-name 'access))

  (check
      (let ((D (make-queue)))
	(queue-push! D 1)
	(queue-front D))
    => 1)

  (check
      (let ((D (make-queue)))
	(queue-push! D 1)
	(queue-push! D 2)
	(let* ((A (queue-pop! D))
	       (B (queue-front D)))
	  (values A B)))
    => 1 2)

  (check
      (let ((D (make-queue)))
	(queue-push! D 1)
	(let ((A (queue-pop! D)))
	  (queue-push! D 2)
	  (let ((B (queue-front D)))
	    (values A B))))
    => 1 2)

  (check
      (let ((D (make-queue 5)))
	(queue-push! D 5)
	(queue-push! D 4)
	(queue-push! D 3)
	(queue-push! D 2)
	(queue-push! D 1)
	(queue-front D))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (make-queue)))
	  (queue-pop! q)))
    => "the container is empty")

  (check
      (let ((q (queue 1 2 3)))
	(queue-pop! q))
    => 1)

  (check
      (let ((q (queue 1 2 3)))
	(queue-pop! q)
	(queue-pop! q)
	(queue-pop! q))
    => 3)

  (check
      (let ((q (queue 1 2 3)))
	(queue-pop! q)
	(queue-pop! q)
	(queue-pop! q)
	(queue-empty? q))
    => #t)

;;; --------------------------------------------------------------------

  ;;Pushing then popping 5 objects.
  ;;
  (check
      (let ((D (make-queue)))
	(map (lambda (obj)
	       (queue-push! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (queue-pop! D))
	  LIST-5))
    => LIST-5)

  ;;pushing then popping 20 objects.
  ;;
  (check
      (let ((D (make-queue)))
	(map (lambda (obj)
	       (queue-push! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (queue-pop! D))
	  LIST-20))
    => LIST-20)

  ;;pushing then popping 100 objects.
  ;;
  (check
      (let ((D (make-queue)))
	(map (lambda (obj)
	       (queue-push! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (queue-pop! D))
	  LIST-100))
    => LIST-100)

  #t)


(parametrise ((check-test-name	'folding))

;;; front folding

  (check
      (let ((D (make-queue)))
	(queue-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => '())

  (check
      (let* ((L LIST-5)
	     (D (list->queue L)))
	(queue-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-5-REVERSED)

  (check
      (let ((D (list->queue LIST-20)))
	(queue-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-20-REVERSED)

  (check
      (let ((D (list->queue LIST-100)))
	(queue-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-100-REVERSED)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D
	  (queue 0 1 2 3 4 5))
	(queue-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => '(5 4 3 2 1 0))

;;; --------------------------------------------------------------------
;;; rear folding

  (check
      (let ((D (make-queue)))
	(queue-fold-right (lambda (knil obj)
			   (cons obj knil))
	  '() D))
    => '())

  (check
      (let* ((L LIST-5)
	     (D (list->queue L)))
	(queue-fold-right cons '() D))
    => LIST-5)

  (check
      (let ((D (list->queue LIST-20)))
	(queue-fold-right cons '() D))
    => LIST-20)

  (check
      (let ((D (list->queue LIST-100)))
	(queue-fold-right cons '() D))
    => LIST-100)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D
	  (queue 0 1 2 3 4 5))
	(queue-fold-right (lambda (obj knil)
			   (cons obj knil))
	  '() D))
    => '(0 1 2 3 4 5))

  (collect))


(parametrise ((check-test-name	'copy))

  (check
      (queue->list (queue-copy! (make-queue) (queue)))
    => '())

  (check
      (queue->list (queue-copy! (make-queue) (list->queue LIST-5)))
    => LIST-5)

  (check
      (queue->list (queue-copy! (make-queue) (list->queue LIST-20)))
    => LIST-20)

  (check
      (queue->list (queue-copy! (make-queue) (list->queue LIST-100)))
    => LIST-100)

  #t)


(parametrise ((check-test-name	'reverse))

  (check
      (queue->list (queue-reverse! (queue) (queue)))
    => '())

  (check
      (queue->list (queue-reverse! (queue) (queue 0)))
    => '(0))

  (check
      (queue->list (queue-reverse! (queue) (queue 0 1 2 3 4)))
    => '(4 3 2 1 0))

  #t)





(parametrise ((check-test-name	'mapping))

  (check (queue->list (queue-map-left (queue) - (queue)))			=> '())
  (check (queue->list (queue-map-left (queue) - (list->queue LIST-5)))		=> LIST-5-NEGATED)
  (check (queue->list (queue-map-left (queue) - (list->queue LIST-20)))	=> LIST-20-NEGATED)
  (check (queue->list (queue-map-left (queue) - (list->queue LIST-100)))	=> LIST-100-NEGATED)

  (check (queue->list (queue-map-right (queue) - (queue)))			=> '())
  (check (queue->list (queue-map-right (queue) - (list->queue LIST-5)))		=> LIST-5-NEGATED)
  (check (queue->list (queue-map-right (queue) - (list->queue LIST-20)))		=> LIST-20-NEGATED)
  (check (queue->list (queue-map-right (queue) - (list->queue LIST-100)))	=> LIST-100-NEGATED)

  (check (queue->list (queue-map (queue) - (queue)))			=> '())
  (check (queue->list (queue-map (queue) - (list->queue LIST-5)))		=> LIST-5-NEGATED)
  (check (queue->list (queue-map (queue) - (list->queue LIST-20)))	=> LIST-20-NEGATED)
  (check (queue->list (queue-map (queue) - (list->queue LIST-100)))	=> LIST-100-NEGATED)

  #t)


(parametrise ((check-test-name	'for-each))

;;; for-each-left

  (check
      (with-result
	(void-object? (queue-for-each-left add-result (queue))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (queue-for-each-left add-result (list->queue LIST-5))))
    => `(#t ,LIST-5))

  (check
      (with-result
	(void-object? (queue-for-each-left add-result (list->queue LIST-20))))
    => `(#t ,LIST-20))

  (check
      (with-result
	(void-object? (queue-for-each-left add-result (list->queue LIST-100))))
    => `(#t ,LIST-100))

;;; --------------------------------------------------------------------
;;; for-each-right

  (check
      (with-result
	(void-object? (queue-for-each-right add-result (queue))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (queue-for-each-right add-result (list->queue LIST-5))))
    => `(#t ,LIST-5-REVERSED))

  (check
      (with-result
	(void-object? (queue-for-each-right add-result (list->queue LIST-20))))
    => `(#t ,LIST-20-REVERSED))

  (check
      (with-result
	(void-object? (queue-for-each-right add-result (list->queue LIST-100))))
    => `(#t ,LIST-100-REVERSED))

;;; --------------------------------------------------------------------
;;; for-each-left aliases

  (check
      (with-result
	(void-object? (queue-for-each add-result (queue))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (queue-for-each add-result (list->queue LIST-5))))
    => `(#t ,LIST-5))

  (check
      (with-result
	(void-object? (queue-for-each add-result (list->queue LIST-20))))
    => `(#t ,LIST-20))

  (check
      (with-result
	(void-object? (queue-for-each add-result (list->queue LIST-100))))
    => `(#t ,LIST-100))

  #t)


(parametrise ((check-test-name	'purge))

  (check
      (let ((D (list->queue LIST-100)))
	(queue-purge! D)
	(queue-size D))
    => 0)

  (check
      (let ((D (list->queue LIST-100)))
	(queue-purge! D)
	(map (lambda (obj)
	       (queue-push! D obj))
	  LIST-20)
	(queue-size D))
    => 20)

  (check
      (let ((D (list->queue LIST-100)))
	(queue-purge! D)
	(map (lambda (obj)
	       (queue-push! D obj))
	  LIST-20)
	(queue->list D))
    => LIST-20)

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
	(queue-for-all
	    add-result
	  (queue)))
    => '(#t ()))


  (check
      (with-result
	(queue-for-all
	    add-result
	  (queue 10)))
    => '(10 (10)))


  (check
      (with-result
	(queue-for-all
	    add-result
	  (queue 10 20 30)))
    => '(30 (10 20 30)))

  (check
      (with-result
	(queue-for-all
	    add-result
	  (queue 10 #f 30)))
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
	(queue-exists-left
	    fun
	  (queue)))
    => '(#f ()))

  (check
      (with-result
	(queue-exists-left
	    fun
	  (queue 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(queue-exists-left
	    fun
	  (queue 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(queue-exists-left
	    fun
	  (queue 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(queue-exists-left
	    fun
	  (queue 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

;;; --------------------------------------------------------------------
;;; from right

  (check
      (with-result
	(queue-exists-right
	    fun
	  (queue)))
    => '(#f ()))

  (check
      (with-result
	(queue-exists-right
	    fun
	  (queue 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(queue-exists-right
	    fun
	  (queue 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(queue-exists-right
	    fun
	  (queue 30 20 10)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(queue-exists-right
	    fun
	  (queue 25 20 10)))
    => '(#f (#(10) #(20) #(25))))

;;; --------------------------------------------------------------------
;;; left/right difference

  (check
      (queue-exists-left
	  (lambda (obj)
	    (and (even? obj)
		 obj))
	(queue 1 2 3 5 6 7))
    => 2)

  (check
      (queue-exists-right
	  (lambda (obj)
	    (and (even? obj)
		 obj))
	(queue 1 2 3 5 6 7))
    => 6)

;;; --------------------------------------------------------------------
;;; from left aliases

  (check
      (with-result
	(queue-exists
	    fun
	  (queue)))
    => '(#f ()))

  (check
      (with-result
	(queue-exists
	    fun
	  (queue 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(queue-exists
	    fun
	  (queue 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(queue-exists
	    fun
	  (queue 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(queue-exists
	    fun
	  (queue 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

  #t)


(parametrise ((check-test-name	'find))

  (check (queue-find-left even? (queue))				=> #f)
  (check (queue-find-left even? (queue) 'not-found)			=> 'not-found)

  (check (queue-find-left even? (queue 1 3 5 7))			=> #f)
  (check (queue-find-left even? (queue 1 3 5 7) 'not-found)		=> 'not-found)

  (check (queue-find-left even? (queue 1 3 5 6 7))			=> 6)
  (check (queue-find-left even? (queue 1 3 5 6 7) 'not-found)		=> 6)

;;; --------------------------------------------------------------------

  (check (queue-find-right even? (queue))				=> #f)
  (check (queue-find-right even? (queue) 'not-found)			=> 'not-found)

  (check (queue-find-right even? (queue 1 3 5 7))			=> #f)
  (check (queue-find-right even? (queue 1 3 5 7) 'not-found)		=> 'not-found)

  (check (queue-find-right even? (queue 1 3 5 6 7))			=> 6)
  (check (queue-find-right even? (queue 1 3 5 6 7) 'not-found)		=> 6)

;;; --------------------------------------------------------------------
;;; left/rigt difference

  (check (queue-find-left  even? (queue 1 2 3 5 6 7))			=> 2)
  (check (queue-find-right even? (queue 1 2 3 5 6 7))			=> 6)

;;; --------------------------------------------------------------------
;;; left aliases

  (check (queue-find even? (queue))					=> #f)
  (check (queue-find even? (queue) 'not-found)				=> 'not-found)

  (check (queue-find even? (queue 1 3 5 7))				=> #f)
  (check (queue-find even? (queue 1 3 5 7) 'not-found)			=> 'not-found)

  (check (queue-find even? (queue 1 3 5 6 7))				=> 6)
  (check (queue-find even? (queue 1 3 5 6 7) 'not-found)		=> 6)

  #t)


(parametrise ((check-test-name	'filter))

  (check
      (queue->list (queue-filter (queue) even? (queue)))
    => '())

  (check
      (queue->list (queue-filter (queue) even? (queue 1 3 5 7)))
    => '())

  (check
      (queue->list (queue-filter (queue) even? (queue 1 3 5 6 7)))
    => '(6))

  (check
      (queue->list (queue-filter (queue) even? (queue 1 2 3 4 5 6 7)))
    => '(2 4 6))

  (check
      (queue->list (queue-filter (queue) even? (queue 1 2 3 4 5 6 7 8)))
    => '(2 4 6 8))

  (check
      (queue->list (queue-filter (queue) even? (queue 2 4 6 8)))
    => '(2 4 6 8))

  #t)


(parametrise ((check-test-name	'partition))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?body ?expected-in ?expected-ou)
       (check
	   (receive (in ou)
	       ?body
	     (values (queue->list in) (queue->list ou)))
	 => ?expected-in ?expected-ou))
      ))

;;; --------------------------------------------------------------------

  (doit (queue-partition (queue) (queue) even? (queue))				'() '())

  (doit (queue-partition (queue) (queue) even? (queue 1 3 5 7))			'() '(1 3 5 7))
  (doit (queue-partition (queue) (queue) even? (queue 2 4 6 8))			'(2 4 6 8) '())

  (doit (queue-partition (queue) (queue) even? (queue 1 3 5 6 7))		'(6) '(1 3 5 7))

  (doit (queue-partition (queue) (queue) even? (queue 1 2 3 4 5 6 7))		'(2 4 6) '(1 3 5 7))

  (doit (queue-partition (queue) (queue) even? (queue 1 2 3 4 5 6 7 8))		'(2 4 6 8) '(1 3 5 7))

  #t)



(parametrise ((check-test-name 'conversion))

  (check
      (queue->list (make-queue))
    => '())

  (check
      (queue->list (queue 1))
    => '(1))

  (check
      (queue->list (queue 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (queue->vector (make-queue))
    => '#())

  (check
      (queue->vector (queue 1))
    => '#(1))

  (check
      (queue->vector (queue 1 2 3))
    => '#(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (queue->list (list->queue '()))
    => '())

  (check
      (queue->list (list->queue '(1)))
    => '(1))

  (check
      (queue->list (list->queue '(1 2 3)))
    => '(1 2 3))

  (check
      (queue->list (list->queue (make-list-100)))
    => (make-list-100))

  (check
      (queue->list (list->queue LIST-100))
    => LIST-100)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D (list->queue '(0 1 2)))
	(values (queue-front D)
		(queue-rear  D)))
    => 0 2)

;;; --------------------------------------------------------------------

  (check
      (queue->vector (vector->queue '#()))
    => '#())

  (check
      (queue->vector (vector->queue '#(1)))
    => '#(1))

  (check
      (queue->vector (vector->queue '#(1 2 3)))
    => '#(1 2 3))

  (check
      (queue->vector (vector->queue VECTOR-20))
    => VECTOR-20)

  (check
      (queue->vector (vector->queue VECTOR-5))
    => VECTOR-5)

  (check
      (queue->vector (vector->queue VECTOR-100))
    => VECTOR-100)

  (check
      (internal-body
	(define D (vector->queue '#(0 1 2)))
	(values (queue-front D)
		(queue-rear  D)
		(queue->vector D)))
    => 0 2 '#(0 1 2))

  #t)


(parametrise ((check-test-name	'iteration-thunks))

  (define (xcons a b)
    (cons b a))

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-queue-iteration-thunk (queue)))
    => '())

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-queue-iteration-thunk (queue 0 1 2 3 4 5)))
    => '(5 4 3 2 1 0))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'queue-fold-left		'scheme-indent-function 1)
;; eval: (put 'queue-fold-right		'scheme-indent-function 1)
;; eval: (put 'queue-map-left		'scheme-indent-function 1)
;; eval: (put 'queue-map-right		'scheme-indent-function 1)
;; eval: (put 'queue-for-each-left	'scheme-indent-function 1)
;; eval: (put 'queue-for-each-right	'scheme-indent-function 1)
;; eval: (put 'queue-for-all		'scheme-indent-function 1)
;; eval: (put 'queue-find		'scheme-indent-function 1)
;; eval: (put 'queue-find-left		'scheme-indent-function 1)
;; eval: (put 'queue-find-right		'scheme-indent-function 1)
;; eval: (put 'queue-exists		'scheme-indent-function 1)
;; eval: (put 'queue-exists-left	'scheme-indent-function 1)
;; eval: (put 'queue-exists-right	'scheme-indent-function 1)
;; End:
