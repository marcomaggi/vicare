;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for dynamic vector containers
;;;Date: Wed Aug 12, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (vicare containers dynamic-arrays)
  (vicare containers dynamic-arrays sort)
  (vicare containers iteration-thunks)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: dynamic array containers\n")


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

;;A list of 5 objects, enqueued in a dynamic-array, is fully stored in a single buffer.
;;
(define-constant LIST-5			'(0 1 2 3 4))
(define-constant LIST-5-REVERSED	(reverse LIST-5))
(define-constant LIST-5-NEGATED		(map - LIST-5))

;;A list of 20 objects, enqueued in a dynamic-array, is stored in two buffers.
;;
(define-constant LIST-20		(make-list-20))
(define-constant LIST-20-REVERSED	(reverse LIST-20))
(define-constant LIST-20-NEGATED	(map - LIST-20))

;;A list of 100 objects, enqueued in a dynamic-array, is stored in multiple buffers.
;;
(define-constant LIST-100		(make-list-100))
(define-constant LIST-100-REVERSED	(reverse LIST-100))
(define-constant LIST-100-NEGATED	(map - LIST-100))

;;A vector of 5 objects, enqueued in a dynamic-array, is fully stored in a single buffer.
;;
(define-constant VECTOR-5		(list->vector LIST-5))
(define-constant VECTOR-5-REVERSED	(list->vector LIST-5-REVERSED))

;;A vector of 20 objects, enqueued in a dynamic-array, is stored in two buffers.
;;
(define-constant VECTOR-20		(list->vector LIST-20))
(define-constant VECTOR-20-REVERSED	(list->vector LIST-20-REVERSED))

;;A vector of 100 objects, enqueued in a dynamic-array, is stored in multiple buffers.
;;
(define-constant VECTOR-100		(list->vector LIST-100))
(define-constant VECTOR-100-REVERSED	(list->vector LIST-100-REVERSED))


(parametrise ((check-test-name	'making))

  (check
      (dynamic-array? (make-dynamic-array))
    => #t)

  (check
      (dynamic-array->list (make-dynamic-array))
    => '())

  (check
      (let* ((V (dynamic-array 1))
	     (A (dynamic-array-ref V 0)))
	A)
    => 1)

  (check
      (dynamic-array->list (dynamic-array 1))
    => '(1))

  (check
      (let* ((V (dynamic-array 1 2 3))
	     (A (dynamic-array-ref V 0))
	     (B (dynamic-array-ref V 1))
	     (C (dynamic-array-ref V 2)))
	(values A B C))
    => 1 2 3)

  (check
      (dynamic-array->list (dynamic-array 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'object))

  (define who 'test)

;;; hash

  (check-for-true
   (integer? (dynamic-array-hash (dynamic-array 1 2 3))))

  (check
      (let ((A (dynamic-array 1 2 3))
	    (B (dynamic-array 1 2 3))
	    (T (make-hashtable dynamic-array-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (dynamic-array 1 2 3)))
	(dynamic-array-property-list S))
    => '())

  (check
      (let ((S (dynamic-array 1 2 3)))
	(dynamic-array-putprop S 'ciao 'salut)
	(dynamic-array-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (dynamic-array 1 2 3)))
	(dynamic-array-getprop S 'ciao))
    => #f)

  (check
      (let ((S (dynamic-array 1 2 3)))
	(dynamic-array-putprop S 'ciao 'salut)
	(dynamic-array-remprop S 'ciao)
	(dynamic-array-getprop S 'ciao))
    => #f)

  (check
      (let ((S (dynamic-array 1 2 3)))
	(dynamic-array-putprop S 'ciao 'salut)
	(dynamic-array-putprop S 'hello 'ohayo)
	(list (dynamic-array-getprop S 'ciao)
	      (dynamic-array-getprop S 'hello)))
    => '(salut ohayo))

  #f)


(parametrise ((check-test-name	'inspect))

  (check-for-true  (dynamic-array-empty? (make-dynamic-array)))
  (check-for-false (dynamic-array-empty? (dynamic-array 1)))
  (check-for-false (dynamic-array-empty? (dynamic-array 1 2 3)))

;;; --------------------------------------------------------------------

  (check-for-false (dynamic-array-not-empty? (make-dynamic-array)))
  (check-for-true  (dynamic-array-not-empty? (dynamic-array 1)))
  (check-for-true  (dynamic-array-not-empty? (dynamic-array 1 2 3)))

;;; --------------------------------------------------------------------

  (check
      (dynamic-array-size (make-dynamic-array))
    => 0)

  (check
      (dynamic-array-size (dynamic-array 1))
    => 1)

  (check
      (dynamic-array-size (dynamic-array 1 2 3))
    => 3)

  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-5)
	(dynamic-array-size D))
    => 5)

  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-20)
	(dynamic-array-size D))
    => 20)

  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-100)
	(dynamic-array-size D))
    => 100)

  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-5)
	(dynamic-array-size D))
    => 5)

  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-20)
	(dynamic-array-size D))
    => 20)

  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-100)
	(dynamic-array-size D))
    => 100)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(dynamic-array-front (make-dynamic-array)))
    => "the container is empty")

  (check
      (dynamic-array-front (dynamic-array 1))
    => 1)

  (check
      (dynamic-array-front (dynamic-array 1 2 3))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(dynamic-array-rear (make-dynamic-array)))
    => "the container is empty")

  (check
      (dynamic-array-rear (dynamic-array 1))
    => 1)

  (check
      (dynamic-array-rear (dynamic-array 1 2 3))
    => 3)

  #t)


(parametrise ((check-test-name 'access))

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-push-front! D 1)
	(dynamic-array-front D))
    => 1)

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-push-front! D 1)
	(dynamic-array-push-front! D 2)
	(let* ((A (dynamic-array-pop-front! D))
	       (B (dynamic-array-front D)))
	  (values A B)))
    => 2 1)

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-push-front! D 1)
	(let ((A (dynamic-array-pop-front! D)))
	  (dynamic-array-push-front! D 2)
	  (let ((B (dynamic-array-front D)))
	    (values A B))))
    => 1 2)

  (check
      (let ((D (make-dynamic-array 5)))
	(dynamic-array-push-front! D 5)
	(dynamic-array-push-front! D 4)
	(dynamic-array-push-front! D 3)
	(dynamic-array-push-front! D 2)
	(dynamic-array-push-front! D 1)
	(dynamic-array-front D))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-push-rear! D 1)
	(dynamic-array-rear D))
    => 1)

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-push-rear! D 1)
	(dynamic-array-push-rear! D 2)
	(let* ((A (dynamic-array-pop-rear! D))
	       (B (dynamic-array-rear D)))
	  (values A B)))
    => 2 1)

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-push-rear! D 1)
	(let ((A (dynamic-array-pop-rear! D)))
	  (dynamic-array-push-rear! D 2)
	  (let ((B (dynamic-array-rear D)))
	    (values A B))))
    => 1 2)


  (check
      (let ((q (make-dynamic-array)))
	(dynamic-array-push-rear! q 1)
	(dynamic-array-push-rear! q 2)
	(dynamic-array-push-rear! q 3)
	(dynamic-array->list q))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (make-dynamic-array)))
	  (dynamic-array-pop-front! q)))
    => "the container is empty")

  (check
      (let ((q (dynamic-array 1 2 3)))
	(dynamic-array-pop-front! q))
    => 1)

  (check
      (let ((q (dynamic-array 1 2 3)))
	(dynamic-array-pop-front! q)
	(dynamic-array-pop-front! q)
	(dynamic-array-pop-front! q))
    => 3)

  (check
      (let ((q (dynamic-array 1 2 3)))
	(dynamic-array-pop-front! q)
	(dynamic-array-pop-front! q)
	(dynamic-array-pop-front! q)
	(dynamic-array-empty? q))
    => #t)

;;; --------------------------------------------------------------------

  ;;Front pushing then front popping 5 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (dynamic-array-pop-front! D))
	  LIST-5))
    => LIST-5-REVERSED)

  ;;Front pushing then front popping 20 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (dynamic-array-pop-front! D))
	  LIST-20))
    => LIST-20-REVERSED)

  ;;Front pushing then front popping 100 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (dynamic-array-pop-front! D))
	  LIST-100))
    => LIST-100-REVERSED)

;;; --------------------------------------------------------------------

  ;;Rear pushing then rear popping 5 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (dynamic-array-pop-rear! D))
	  LIST-5))
    => LIST-5-REVERSED)

  ;;Rear pushing then rear popping 20 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (dynamic-array-pop-rear! D))
	  LIST-20))
    => LIST-20-REVERSED)

  ;;Rear pushing then rear popping 100 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (dynamic-array-pop-rear! D))
	  LIST-100))
    => LIST-100-REVERSED)

;;; --------------------------------------------------------------------

  ;;Front pushing then rear popping 5 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (dynamic-array-pop-rear! D))
	  LIST-5))
    => LIST-5)

  ;;Front pushing then rear popping 20 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (dynamic-array-pop-rear! D))
	  LIST-20))
    => LIST-20)

  ;;Front pushing then rear popping 100 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (dynamic-array-pop-rear! D))
	  LIST-100))
    => LIST-100)

;;; --------------------------------------------------------------------

  ;;Rear pushing then front popping 5 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (dynamic-array-pop-front! D))
	  LIST-5))
    => LIST-5)

  ;;Rear pushing then front popping 20 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (dynamic-array-pop-front! D))
	  LIST-20))
    => LIST-20)

  ;;Rear pushing then front popping 100 objects.
  ;;
  (check
      (let ((D (make-dynamic-array)))
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (dynamic-array-pop-front! D))
	  LIST-100))
    => LIST-100)

  #t)


(parametrise ((check-test-name	'insertion))

;;; insertion at index -1

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-insert! D -1 0)
	(dynamic-array->vector D))
    => '#(0))

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-insert! D -1 3)
	(dynamic-array-insert! D -1 2)
	(dynamic-array-insert! D -1 1)
	(dynamic-array-insert! D -1 0)
	(dynamic-array->vector D))
    => '#(0 1 2 3))

  (check
      (let ((D (make-dynamic-array)))
	(for-each-in-order (lambda (obj)
			     (dynamic-array-insert! D -1 obj))
	  LIST-5-REVERSED)
	(dynamic-array->vector D))
    => VECTOR-5)

  (check
      (let ((D (make-dynamic-array)))
	(for-each-in-order (lambda (obj)
			     (dynamic-array-insert! D -1 obj))
	  LIST-20-REVERSED)
	(dynamic-array->vector D))
    => VECTOR-20)

  (check
      (let ((D (make-dynamic-array)))
	(for-each-in-order (lambda (obj)
			     (dynamic-array-insert! D -1 obj))
	  LIST-100-REVERSED)
	(dynamic-array->vector D))
    => VECTOR-100)

;;; --------------------------------------------------------------------
;;; insertion at index past end

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-insert! D (dynamic-array-size D) 0)
	(dynamic-array->vector D))
    => '#(0))

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-insert! D (dynamic-array-size D) 0)
	(dynamic-array-insert! D (dynamic-array-size D) 1)
	(dynamic-array-insert! D (dynamic-array-size D) 2)
	(dynamic-array-insert! D (dynamic-array-size D) 3)
	(dynamic-array->vector D))
    => '#(0 1 2 3))

  (check
      (let ((D (make-dynamic-array)))
	(for-each-in-order (lambda (obj)
			     (dynamic-array-insert! D (dynamic-array-size D) obj))
	  LIST-5)
	(dynamic-array->vector D))
    => VECTOR-5)

  (check
      (let ((D (make-dynamic-array)))
	(for-each-in-order (lambda (obj)
			     (dynamic-array-insert! D (dynamic-array-size D) obj))
	  LIST-20)
	(dynamic-array->vector D))
    => VECTOR-20)

  (check
      (let ((D (make-dynamic-array)))
	(for-each-in-order (lambda (obj)
			     (dynamic-array-insert! D (dynamic-array-size D) obj))
	  LIST-100)
	(dynamic-array->vector D))
    => VECTOR-100)

;;; --------------------------------------------------------------------
;;; insertion at index

  (check	;more room at the left
      (let ((D (dynamic-array 0 1 2 3)))
	(dynamic-array-insert! D 0 -1)
	(dynamic-array->vector D))
    => '#(-1 0 1 2 3))

  (check	;more room at the left
      (let ((D (dynamic-array 0 1 2 3)))
	(dynamic-array-insert! D 1 -1)
	(dynamic-array->vector D))
    => '#(0 -1 1 2 3))

  (check	;more room at the right
      (let ((D (make-dynamic-array)))
	(for-each-in-order
	    (lambda (obj)
	      (dynamic-array-push-front! D obj))
	  (reverse '(0 1 2 3)))
	(dynamic-array-insert! D 3 -1)
	(dynamic-array->vector D))
    => '#(0 1 2 -1 3))

  ;;Full array before insertion.
  ;;
  (check
      (let ((D (make-dynamic-array 5)))
	#;(debug-print D)
	(for-each-in-order
	    (lambda (obj)
	      (dynamic-array-push-front! D obj))
	  (reverse '(0 1 2 3 4)))
	#;(debug-print D)
	(dynamic-array-insert! D 1 -1)
	(dynamic-array->vector D))
    => '#(0 -1 1 2 3 4))

  ;;Full array before insertion.
  ;;
  (check
      (let ((D (make-dynamic-array 5)))
	#;(debug-print D)
	(for-each-in-order
	    (lambda (obj)
	      (dynamic-array-push-front! D obj))
	  (reverse '(0 1 2 3 4)))
	#;(debug-print D)
	(dynamic-array-insert! D 0 -1)
	(dynamic-array->vector D))
    => '#(-1 0 1 2 3 4))

  ;;Full array before insertion.
  ;;
  (check
      (let ((D (make-dynamic-array 5)))
	#;(debug-print D)
	(for-each-in-order
	    (lambda (obj)
	      (dynamic-array-push-front! D obj))
	  (reverse '(0 1 2 3 4)))
	#;(debug-print D)
	(dynamic-array-insert! D 3 -1)
	(dynamic-array->vector D))
    => '#(0 1 2 -1 3 4))

  ;;Full array before insertion.
  ;;
  (check
      (let ((D (make-dynamic-array 5)))
	#;(debug-print D)
	(for-each-in-order
	    (lambda (obj)
	      (dynamic-array-push-front! D obj))
	  (reverse '(0 1 2 3 4)))
	#;(debug-print D)
	(dynamic-array-insert! D 4 -1)
	(dynamic-array->vector D))
    => '#(0 1 2 3 -1 4))

  #t)


(parametrise ((check-test-name	'removal))

;;; remove from the left side

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 0)
	(dynamic-array->list D))
    => '(1 2 3 4))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array->list D))
    => '(2 3 4))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array->list D))
    => '(3 4))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array->list D))
    => '(4))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array-remove! D 0)
	(dynamic-array->list D))
    => '())

;;; --------------------------------------------------------------------
;;; remove from the right

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 4)
	(dynamic-array->list D))
    => '(0 1 2 3))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 4)
	(dynamic-array-remove! D 3)
	(dynamic-array->list D))
    => '(0 1 2))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 4)
	(dynamic-array-remove! D 3)
	(dynamic-array-remove! D 2)
	(dynamic-array->list D))
    => '(0 1))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 4)
	(dynamic-array-remove! D 3)
	(dynamic-array-remove! D 2)
	(dynamic-array-remove! D 1)
	(dynamic-array->list D))
    => '(0))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 4)
	(dynamic-array-remove! D 3)
	(dynamic-array-remove! D 2)
	(dynamic-array-remove! D 1)
	(dynamic-array-remove! D 0)
	(dynamic-array->list D))
    => '())

;;; --------------------------------------------------------------------
;;; remove from the middle

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 2)
	(dynamic-array->list D))
    => '(0 1 3 4))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 1)
	(dynamic-array->list D))
    => '(0 2 3 4))

  (check
      (let ((D (dynamic-array 0 1 2 3 4 5 6 7 8 9)))
	(dynamic-array-remove! D 3)
	(dynamic-array-remove! D 2)
	(dynamic-array-remove! D 1)
	(dynamic-array-remove! D 0)
	(dynamic-array->list D))
    => '(4 5 6 7 8 9))

  (check
      (let ((D (dynamic-array 0 1 2 3 4)))
	(dynamic-array-remove! D 3)
	(dynamic-array->list D))
    => '(0 1 2 4))

  (check
      (let ((D (dynamic-array 0 1 2 3 4 5 6 7 8 9)))
	(dynamic-array-remove! D 6)
	(dynamic-array-remove! D 6)
	(dynamic-array-remove! D 6)
	(dynamic-array-remove! D 6)
	(dynamic-array->list D))
    => '(0 1 2 3 4 5))

  #t)


(parametrise ((check-test-name	'folding))

;;; front folding

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => '())

  (check
      (let* ((L LIST-5)
	     (D (list->dynamic-array L)))
	(dynamic-array-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-5-REVERSED)

  (check
      (let ((D (list->dynamic-array LIST-20)))
	(dynamic-array-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-20-REVERSED)

  (check
      (let ((D (list->dynamic-array LIST-100)))
	(dynamic-array-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-100-REVERSED)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D
	  (dynamic-array 0 1 2 3 4 5))
	(dynamic-array-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => '(5 4 3 2 1 0))

;;; --------------------------------------------------------------------
;;; rear folding

  (check
      (let ((D (make-dynamic-array)))
	(dynamic-array-fold-right (lambda (knil obj)
			   (cons obj knil))
	  '() D))
    => '())

  (check
      (let* ((L LIST-5)
	     (D (list->dynamic-array L)))
	(dynamic-array-fold-right cons '() D))
    => LIST-5)

  (check
      (let ((D (list->dynamic-array LIST-20)))
	(dynamic-array-fold-right cons '() D))
    => LIST-20)

  (check
      (let ((D (list->dynamic-array LIST-100)))
	(dynamic-array-fold-right cons '() D))
    => LIST-100)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D
	  (dynamic-array 0 1 2 3 4 5))
	(dynamic-array-fold-right (lambda (obj knil)
			   (cons obj knil))
	  '() D))
    => '(0 1 2 3 4 5))

  (collect))


(parametrise ((check-test-name	'copy))

  (check
      (dynamic-array->list (dynamic-array-copy! (make-dynamic-array) (dynamic-array)))
    => '())

  (check
      (dynamic-array->list (dynamic-array-copy! (make-dynamic-array) (list->dynamic-array LIST-5)))
    => LIST-5)

  (check
      (dynamic-array->list (dynamic-array-copy! (make-dynamic-array) (list->dynamic-array LIST-20)))
    => LIST-20)

  (check
      (dynamic-array->list (dynamic-array-copy! (make-dynamic-array) (list->dynamic-array LIST-100)))
    => LIST-100)

  #t)


(parametrise ((check-test-name	'reverse))

  (check
      (dynamic-array->list (dynamic-array-reverse! (dynamic-array) (dynamic-array)))
    => '())

  (check
      (dynamic-array->list (dynamic-array-reverse! (dynamic-array) (dynamic-array 0)))
    => '(0))

  (check
      (dynamic-array->list (dynamic-array-reverse! (dynamic-array) (dynamic-array 0 1 2 3 4)))
    => '(4 3 2 1 0))

  #t)


(parametrise ((check-test-name	'mapping))

  (check (dynamic-array->list (dynamic-array-map-left (dynamic-array) - (dynamic-array)))			=> '())
  (check (dynamic-array->list (dynamic-array-map-left (dynamic-array) - (list->dynamic-array LIST-5)))	=> LIST-5-NEGATED)
  (check (dynamic-array->list (dynamic-array-map-left (dynamic-array) - (list->dynamic-array LIST-20)))	=> LIST-20-NEGATED)
  (check (dynamic-array->list (dynamic-array-map-left (dynamic-array) - (list->dynamic-array LIST-100)))	=> LIST-100-NEGATED)

  (check (dynamic-array->list (dynamic-array-map-right (dynamic-array) - (dynamic-array)))			=> '())
  (check (dynamic-array->list (dynamic-array-map-right (dynamic-array) - (list->dynamic-array LIST-5)))	=> LIST-5-NEGATED)
  (check (dynamic-array->list (dynamic-array-map-right (dynamic-array) - (list->dynamic-array LIST-20)))	=> LIST-20-NEGATED)
  (check (dynamic-array->list (dynamic-array-map-right (dynamic-array) - (list->dynamic-array LIST-100)))	=> LIST-100-NEGATED)

  (check (dynamic-array->list (dynamic-array-map (dynamic-array) - (dynamic-array)))			=> '())
  (check (dynamic-array->list (dynamic-array-map (dynamic-array) - (list->dynamic-array LIST-5)))		=> LIST-5-NEGATED)
  (check (dynamic-array->list (dynamic-array-map (dynamic-array) - (list->dynamic-array LIST-20)))		=> LIST-20-NEGATED)
  (check (dynamic-array->list (dynamic-array-map (dynamic-array) - (list->dynamic-array LIST-100)))	        => LIST-100-NEGATED)

  #t)


(parametrise ((check-test-name	'for-each))

;;; for-each-left

  (check
      (with-result
	(void-object? (dynamic-array-for-each-left add-result (dynamic-array))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (dynamic-array-for-each-left add-result (list->dynamic-array LIST-5))))
    => `(#t ,LIST-5))

  (check
      (with-result
	(void-object? (dynamic-array-for-each-left add-result (list->dynamic-array LIST-20))))
    => `(#t ,LIST-20))

  (check
      (with-result
	(void-object? (dynamic-array-for-each-left add-result (list->dynamic-array LIST-100))))
    => `(#t ,LIST-100))

;;; --------------------------------------------------------------------
;;; for-each-right

  (check
      (with-result
	(void-object? (dynamic-array-for-each-right add-result (dynamic-array))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (dynamic-array-for-each-right add-result (list->dynamic-array LIST-5))))
    => `(#t ,LIST-5-REVERSED))

  (check
      (with-result
	(void-object? (dynamic-array-for-each-right add-result (list->dynamic-array LIST-20))))
    => `(#t ,LIST-20-REVERSED))

  (check
      (with-result
	(void-object? (dynamic-array-for-each-right add-result (list->dynamic-array LIST-100))))
    => `(#t ,LIST-100-REVERSED))

;;; --------------------------------------------------------------------
;;; for-each-left aliases

  (check
      (with-result
	(void-object? (dynamic-array-for-each add-result (dynamic-array))))
    => '(#t ()))

  (check
      (with-result
	(void-object? (dynamic-array-for-each add-result (list->dynamic-array LIST-5))))
    => `(#t ,LIST-5))

  (check
      (with-result
	(void-object? (dynamic-array-for-each add-result (list->dynamic-array LIST-20))))
    => `(#t ,LIST-20))

  (check
      (with-result
	(void-object? (dynamic-array-for-each add-result (list->dynamic-array LIST-100))))
    => `(#t ,LIST-100))

  #t)


(parametrise ((check-test-name	'purge))

  (check
      (let ((D (list->dynamic-array LIST-100)))
	(dynamic-array-purge! D)
	(dynamic-array-size D))
    => 0)

  (check
      (let ((D (list->dynamic-array LIST-100)))
	(dynamic-array-purge! D)
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-20)
	(dynamic-array-size D))
    => 20)

  (check
      (let ((D (list->dynamic-array LIST-100)))
	(dynamic-array-purge! D)
	(map (lambda (obj)
	       (dynamic-array-push-front! D obj))
	  LIST-20)
	(dynamic-array->list D))
    => LIST-20-REVERSED)

  (check
      (let ((D (list->dynamic-array LIST-100)))
	(dynamic-array-purge! D)
	(map (lambda (obj)
	       (dynamic-array-push-rear! D obj))
	  LIST-20)
	(dynamic-array->list D))
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
	(dynamic-array-for-all
	    add-result
	  (dynamic-array)))
    => '(#t ()))


  (check
      (with-result
	(dynamic-array-for-all
	    add-result
	  (dynamic-array 10)))
    => '(10 (10)))


  (check
      (with-result
	(dynamic-array-for-all
	    add-result
	  (dynamic-array 10 20 30)))
    => '(30 (10 20 30)))

  (check
      (with-result
	(dynamic-array-for-all
	    add-result
	  (dynamic-array 10 #f 30)))
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
	(dynamic-array-exists-left
	    fun
	  (dynamic-array)))
    => '(#f ()))

  (check
      (with-result
	(dynamic-array-exists-left
	    fun
	  (dynamic-array 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(dynamic-array-exists-left
	    fun
	  (dynamic-array 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(dynamic-array-exists-left
	    fun
	  (dynamic-array 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(dynamic-array-exists-left
	    fun
	  (dynamic-array 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

;;; --------------------------------------------------------------------
;;; from right

  (check
      (with-result
	(dynamic-array-exists-right
	    fun
	  (dynamic-array)))
    => '(#f ()))

  (check
      (with-result
	(dynamic-array-exists-right
	    fun
	  (dynamic-array 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(dynamic-array-exists-right
	    fun
	  (dynamic-array 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(dynamic-array-exists-right
	    fun
	  (dynamic-array 30 20 10)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(dynamic-array-exists-right
	    fun
	  (dynamic-array 25 20 10)))
    => '(#f (#(10) #(20) #(25))))

;;; --------------------------------------------------------------------
;;; left/right difference

  (check
      (dynamic-array-exists-left
	  (lambda (obj)
	    (and (even? obj)
		 obj))
	(dynamic-array 1 2 3 5 6 7))
    => 2)

  (check
      (dynamic-array-exists-right
	  (lambda (obj)
	    (and (even? obj)
		 obj))
	(dynamic-array 1 2 3 5 6 7))
    => 6)

;;; --------------------------------------------------------------------
;;; from left aliases

  (check
      (with-result
	(dynamic-array-exists
	    fun
	  (dynamic-array)))
    => '(#f ()))

  (check
      (with-result
	(dynamic-array-exists
	    fun
	  (dynamic-array 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(dynamic-array-exists
	    fun
	  (dynamic-array 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(dynamic-array-exists
	    fun
	  (dynamic-array 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(dynamic-array-exists
	    fun
	  (dynamic-array 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

  #t)


(parametrise ((check-test-name	'find))

  (check (dynamic-array-find-left even? (dynamic-array))				=> #f)
  (check (dynamic-array-find-left even? (dynamic-array) 'not-found)			=> 'not-found)

  (check (dynamic-array-find-left even? (dynamic-array 1 3 5 7))			=> #f)
  (check (dynamic-array-find-left even? (dynamic-array 1 3 5 7) 'not-found)		=> 'not-found)

  (check (dynamic-array-find-left even? (dynamic-array 1 3 5 6 7))			=> 6)
  (check (dynamic-array-find-left even? (dynamic-array 1 3 5 6 7) 'not-found)		=> 6)

;;; --------------------------------------------------------------------

  (check (dynamic-array-find-right even? (dynamic-array))				=> #f)
  (check (dynamic-array-find-right even? (dynamic-array) 'not-found)			=> 'not-found)

  (check (dynamic-array-find-right even? (dynamic-array 1 3 5 7))			=> #f)
  (check (dynamic-array-find-right even? (dynamic-array 1 3 5 7) 'not-found)		=> 'not-found)

  (check (dynamic-array-find-right even? (dynamic-array 1 3 5 6 7))			=> 6)
  (check (dynamic-array-find-right even? (dynamic-array 1 3 5 6 7) 'not-found)		=> 6)

;;; --------------------------------------------------------------------
;;; left/rigt difference

  (check (dynamic-array-find-left  even? (dynamic-array 1 2 3 5 6 7))			=> 2)
  (check (dynamic-array-find-right even? (dynamic-array 1 2 3 5 6 7))			=> 6)

;;; --------------------------------------------------------------------
;;; left aliases

  (check (dynamic-array-find even? (dynamic-array))					=> #f)
  (check (dynamic-array-find even? (dynamic-array) 'not-found)				=> 'not-found)

  (check (dynamic-array-find even? (dynamic-array 1 3 5 7))				=> #f)
  (check (dynamic-array-find even? (dynamic-array 1 3 5 7) 'not-found)			=> 'not-found)

  (check (dynamic-array-find even? (dynamic-array 1 3 5 6 7))				=> 6)
  (check (dynamic-array-find even? (dynamic-array 1 3 5 6 7) 'not-found)		=> 6)

  #t)


(parametrise ((check-test-name	'filter))

  (check
      (dynamic-array->list (dynamic-array-filter (dynamic-array) even? (dynamic-array)))
    => '())

  (check
      (dynamic-array->list (dynamic-array-filter (dynamic-array) even? (dynamic-array 1 3 5 7)))
    => '())

  (check
      (dynamic-array->list (dynamic-array-filter (dynamic-array) even? (dynamic-array 1 3 5 6 7)))
    => '(6))

  (check
      (dynamic-array->list (dynamic-array-filter (dynamic-array) even? (dynamic-array 1 2 3 4 5 6 7)))
    => '(2 4 6))

  (check
      (dynamic-array->list (dynamic-array-filter (dynamic-array) even? (dynamic-array 1 2 3 4 5 6 7 8)))
    => '(2 4 6 8))

  (check
      (dynamic-array->list (dynamic-array-filter (dynamic-array) even? (dynamic-array 2 4 6 8)))
    => '(2 4 6 8))

  #t)


(parametrise ((check-test-name	'partition))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?body ?expected-in ?expected-ou)
       (check
	   (receive (in ou)
	       ?body
	     (values (dynamic-array->list in) (dynamic-array->list ou)))
	 => ?expected-in ?expected-ou))
      ))

;;; --------------------------------------------------------------------

  (doit (dynamic-array-partition (dynamic-array) (dynamic-array) even? (dynamic-array))				'() '())

  (doit (dynamic-array-partition (dynamic-array) (dynamic-array) even? (dynamic-array 1 3 5 7))			'() '(1 3 5 7))
  (doit (dynamic-array-partition (dynamic-array) (dynamic-array) even? (dynamic-array 2 4 6 8))			'(2 4 6 8) '())

  (doit (dynamic-array-partition (dynamic-array) (dynamic-array) even? (dynamic-array 1 3 5 6 7))		'(6) '(1 3 5 7))

  (doit (dynamic-array-partition (dynamic-array) (dynamic-array) even? (dynamic-array 1 2 3 4 5 6 7))		'(2 4 6) '(1 3 5 7))

  (doit (dynamic-array-partition (dynamic-array) (dynamic-array) even? (dynamic-array 1 2 3 4 5 6 7 8))		'(2 4 6 8) '(1 3 5 7))

  #t)



(parametrise ((check-test-name 'conversion))

  (check
      (dynamic-array->list (make-dynamic-array))
    => '())

  (check
      (dynamic-array->list (dynamic-array 1))
    => '(1))

  (check
      (dynamic-array->list (dynamic-array 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (dynamic-array->vector (make-dynamic-array))
    => '#())

  (check
      (dynamic-array->vector (dynamic-array 1))
    => '#(1))

  (check
      (dynamic-array->vector (dynamic-array 1 2 3))
    => '#(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (dynamic-array->list (list->dynamic-array '()))
    => '())

  (check
      (dynamic-array->list (list->dynamic-array '(1)))
    => '(1))

  (check
      (dynamic-array->list (list->dynamic-array '(1 2 3)))
    => '(1 2 3))

  (check
      (dynamic-array->list (list->dynamic-array (make-list-20)))
    => (make-list-20))

  (check
      (dynamic-array->list (list->dynamic-array (make-list-100)))
    => (make-list-100))

  (check
      (dynamic-array->list (list->dynamic-array LIST-100))
    => LIST-100)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D (list->dynamic-array '(0 1 2)))
	(values (dynamic-array-front D)
		(dynamic-array-rear  D)))
    => 0 2)

;;; --------------------------------------------------------------------

  (check
      (dynamic-array->vector (vector->dynamic-array '#()))
    => '#())

  (check
      (dynamic-array->vector (vector->dynamic-array '#(1)))
    => '#(1))

  (check
      (dynamic-array->vector (vector->dynamic-array '#(1 2 3)))
    => '#(1 2 3))

  (check
      (dynamic-array->vector (vector->dynamic-array VECTOR-20))
    => VECTOR-20)

  (check
      (dynamic-array->vector (vector->dynamic-array VECTOR-5))
    => VECTOR-5)

  (check
      (dynamic-array->vector (vector->dynamic-array VECTOR-100))
    => VECTOR-100)

  (check
      (internal-body
	(define D (vector->dynamic-array '#(0 1 2)))
	(values (dynamic-array-front D)
		(dynamic-array-rear  D)
		(dynamic-array->vector D)))
    => 0 2 '#(0 1 2))

  #t)


(parametrise ((check-test-name	'sorting))

  (define (permutations ls)
    (define (rem* ls)
      (if (null? ls)
	  '()
	(begin
	  (cons (cdr ls)
		(map (lambda (a)
		       (cons (car ls) a))
		  (rem* (cdr ls)))))))
    (if (null? ls)
	'(())
      (begin
	(apply append (map (lambda (x a*)
			     (map (lambda (a) (cons x a)) a*))
			ls
			(map permutations (rem* ls)))))))

;;; --------------------------------------------------------------------

  (check
      (let* ((C1 (dynamic-array 0 1 2 3 4 5))
	     (C2 (dynamic-array-sort < C1)))
	(dynamic-array->list C2))
    => '(0 1 2 3 4 5))

  (check
      (let* ((C1 (dynamic-array 5 4 3 2 1 0))
	     (C2 (dynamic-array-sort < C1)))
	(dynamic-array->list C2))
    => '(0 1 2 3 4 5))

  (check
      (let* ((C1 (dynamic-array 0 4 3 1 2 5))
	     (C2 (dynamic-array-sort < C1)))
	(dynamic-array->list C2))
    => '(0 1 2 3 4 5))

;;; --------------------------------------------------------------------

  (check
      (for-all (lambda (ell)
		 #;(debug-print ell)
		 (equal? LIST-5 (dynamic-array->list (dynamic-array-sort < (list->dynamic-array ell)))))
	(permutations LIST-5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((C (dynamic-array 0 4 3 1 2 5)))
	(dynamic-array->list (dynamic-array-sort! < C)))
    => '(0 1 2 3 4 5))

  #t)


(parametrise ((check-test-name	'iteration-thunks))

  (define (xcons a b)
    (cons b a))

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-dynamic-array-front-iteration-thunk (dynamic-array)))
    => '())

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-dynamic-array-front-iteration-thunk (dynamic-array 0 1 2 3 4 5)))
    => '(5 4 3 2 1 0))

;;; --------------------------------------------------------------------

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-dynamic-array-rear-iteration-thunk (dynamic-array)))
    => '())

  (check
      (iteration-thunk-fold
	  xcons
	'()
	(make-dynamic-array-rear-iteration-thunk (dynamic-array 0 1 2 3 4 5)))
    => '(0 1 2 3 4 5))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'dynamic-array-fold-left		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-fold-right	'scheme-indent-function 1)
;; eval: (put 'dynamic-array-map-left		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-map-right		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-for-each-left	'scheme-indent-function 1)
;; eval: (put 'dynamic-array-for-each-right	'scheme-indent-function 1)
;; eval: (put 'dynamic-array-for-all		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-find		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-find-left		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-find-right		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-exists		'scheme-indent-function 1)
;; eval: (put 'dynamic-array-exists-left	'scheme-indent-function 1)
;; eval: (put 'dynamic-array-exists-right	'scheme-indent-function 1)
;; End:
