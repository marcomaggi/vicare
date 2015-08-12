;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for deque containers
;;;Date: Thu Aug  6, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare containers deques)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: deque containers\n")


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

;;A list of 5 objects, enqueued in a deque, is fully stored in a single buffer.
;;
(define-constant LIST-5			'(0 1 2 3 4))
(define-constant LIST-5-REVERSED	(reverse LIST-5))
(define-constant LIST-5-NEGATED		(map - LIST-5))

;;A list of 20 objects, enqueued in a deque, is stored in two buffers.
;;
(define-constant LIST-20		(make-list-20))
(define-constant LIST-20-REVERSED	(reverse LIST-20))
(define-constant LIST-20-NEGATED	(map - LIST-20))

;;A list of 100 objects, enqueued in a deque, is stored in multiple buffers.
;;
(define-constant LIST-100		(make-list-100))
(define-constant LIST-100-REVERSED	(reverse LIST-100))
(define-constant LIST-100-NEGATED	(map - LIST-100))

;;A vector of 5 objects, enqueued in a deque, is fully stored in a single buffer.
;;
(define-constant VECTOR-5		(list->vector LIST-5))
(define-constant VECTOR-5-REVERSED	(list->vector LIST-5-REVERSED))

;;A vector of 20 objects, enqueued in a deque, is stored in two buffers.
;;
(define-constant VECTOR-20		(list->vector LIST-20))
(define-constant VECTOR-20-REVERSED	(list->vector LIST-20-REVERSED))

;;A vector of 100 objects, enqueued in a deque, is stored in multiple buffers.
;;
(define-constant VECTOR-100		(list->vector LIST-100))
(define-constant VECTOR-100-REVERSED	(list->vector LIST-100-REVERSED))


(parametrise ((check-test-name	'making))

  (check
      (deque? (make-deque))
    => #t)

  (check
      (deque->list (make-deque))
    => '())

  (check
      (deque->list (deque 1))
    => '(1))

  (check
      (deque->list (deque 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name 'pred))

  (check-for-true  (deque-empty? (make-deque)))
  (check-for-false (deque-empty? (deque 1)))
  (check-for-false (deque-empty? (deque 1 2 3)))

;;; --------------------------------------------------------------------

  (check-for-false (deque-not-empty? (make-deque)))
  (check-for-true  (deque-not-empty? (deque 1)))
  (check-for-true  (deque-not-empty? (deque 1 2 3)))

  #t)


(parametrise ((check-test-name	'object))

  (define who 'test)

;;; hash

  (check-for-true
   (integer? (deque-hash (deque 1 2 3))))

  (check
      (let ((A (deque 1 2 3))
	    (B (deque 1 2 3))
	    (T (make-hashtable deque-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (deque 1 2 3)))
	(deque-property-list S))
    => '())

  (check
      (let ((S (deque 1 2 3)))
	(deque-putprop S 'ciao 'salut)
	(deque-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (deque 1 2 3)))
	(deque-getprop S 'ciao))
    => #f)

  (check
      (let ((S (deque 1 2 3)))
	(deque-putprop S 'ciao 'salut)
	(deque-remprop S 'ciao)
	(deque-getprop S 'ciao))
    => #f)

  (check
      (let ((S (deque 1 2 3)))
	(deque-putprop S 'ciao 'salut)
	(deque-putprop S 'hello 'ohayo)
	(list (deque-getprop S 'ciao)
	      (deque-getprop S 'hello)))
    => '(salut ohayo))

  #f)


(parametrise ((check-test-name	'inspect))

  (check
      (deque-size (make-deque))
    => 0)

  (check
      (deque-size (deque 1))
    => 1)

  (check
      (deque-size (deque 1 2 3))
    => 3)

  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-5)
	(deque-size D))
    => 5)

  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-20)
	(deque-size D))
    => 20)

  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-100)
	(deque-size D))
    => 100)

  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-5)
	(deque-size D))
    => 5)

  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-20)
	(deque-size D))
    => 20)

  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-100)
	(deque-size D))
    => 100)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(deque-front (make-deque)))
    => "the container is empty")

  (check
      (deque-front (deque 1))
    => 1)

  (check
      (deque-front (deque 1 2 3))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(deque-rear (make-deque)))
    => "the container is empty")

  (check
      (deque-rear (deque 1))
    => 1)

  (check
      (deque-rear (deque 1 2 3))
    => 3)

  #t)


(parametrise ((check-test-name 'access))

  (check
      (let ((D (make-deque)))
	(deque-push-front! D 1)
	(deque-front D))
    => 1)

  (check
      (let ((D (make-deque)))
	(deque-push-front! D 1)
	(deque-push-front! D 2)
	(let* ((A (deque-pop-front! D))
	       (B (deque-front D)))
	  (values A B)))
    => 2 1)

  (check
      (let ((D (make-deque)))
	(deque-push-front! D 1)
	(let ((A (deque-pop-front! D)))
	  (deque-push-front! D 2)
	  (let ((B (deque-front D)))
	    (values A B))))
    => 1 2)

  (check
      (let ((D (make-deque 5)))
	(deque-push-front! D 5)
	(deque-push-front! D 4)
	(deque-push-front! D 3)
	(deque-push-front! D 2)
	(deque-push-front! D 1)
	(deque-front D))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let ((D (make-deque)))
	(deque-push-rear! D 1)
	(deque-rear D))
    => 1)

  (check 'this
      (let ((D (make-deque)))
	(deque-push-rear! D 1)
	(deque-push-rear! D 2)
	(let* ((A (deque-pop-rear! D))
	       (B (deque-rear D)))
	  (values A B)))
    => 2 1)

  (check
      (let ((D (make-deque)))
	(deque-push-rear! D 1)
	(let ((A (deque-pop-rear! D)))
	  (deque-push-rear! D 2)
	  (let ((B (deque-rear D)))
	    (values A B))))
    => 1 2)


  (check
      (let ((q (make-deque)))
	(deque-push-rear! q 1)
	(deque-push-rear! q 2)
	(deque-push-rear! q 3)
	(deque->list q))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (make-deque)))
	  (deque-pop-front! q)))
    => "the container is empty")

  (check
      (let ((q (deque 1 2 3)))
	(deque-pop-front! q))
    => 1)

  (check
      (let ((q (deque 1 2 3)))
	(deque-pop-front! q)
	(deque-pop-front! q)
	(deque-pop-front! q))
    => 3)

  (check
      (let ((q (deque 1 2 3)))
	(deque-pop-front! q)
	(deque-pop-front! q)
	(deque-pop-front! q)
	(deque-empty? q))
    => #t)

;;; --------------------------------------------------------------------

  ;;Front pushing then front popping 5 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (deque-pop-front! D))
	  LIST-5))
    => LIST-5-REVERSED)

  ;;Front pushing then front popping 20 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (deque-pop-front! D))
	  LIST-20))
    => LIST-20-REVERSED)

  ;;Front pushing then front popping 100 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (deque-pop-front! D))
	  LIST-100))
    => LIST-100-REVERSED)

;;; --------------------------------------------------------------------

  ;;Rear pushing then rear popping 5 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (deque-pop-rear! D))
	  LIST-5))
    => LIST-5-REVERSED)

  ;;Rear pushing then rear popping 20 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (deque-pop-rear! D))
	  LIST-20))
    => LIST-20-REVERSED)

  ;;Rear pushing then rear popping 100 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (deque-pop-rear! D))
	  LIST-100))
    => LIST-100-REVERSED)

;;; --------------------------------------------------------------------

  ;;Front pushing then rear popping 5 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (deque-pop-rear! D))
	  LIST-5))
    => LIST-5)

  ;;Front pushing then rear popping 20 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (deque-pop-rear! D))
	  LIST-20))
    => LIST-20)

  ;;Front pushing then rear popping 100 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (deque-pop-rear! D))
	  LIST-100))
    => LIST-100)

;;; --------------------------------------------------------------------

  ;;Rear pushing then front popping 5 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-5)
	(map (lambda (dummy)
	       (deque-pop-front! D))
	  LIST-5))
    => LIST-5)

  ;;Rear pushing then front popping 20 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-20)
	(map (lambda (dummy)
	       (deque-pop-front! D))
	  LIST-20))
    => LIST-20)

  ;;Rear pushing then front popping 100 objects.
  ;;
  (check
      (let ((D (make-deque)))
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-100)
	(map (lambda (dummy)
	       (deque-pop-front! D))
	  LIST-100))
    => LIST-100)

  #t)


(parametrise ((check-test-name	'folding))

;;; front folding

  (check
      (let ((D (make-deque)))
	(deque-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => '())

  (check
      (let* ((L LIST-5)
	     (D (list->deque L)))
	(deque-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-5-REVERSED)

  (check
      (let ((D (list->deque LIST-20)))
	(deque-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-20-REVERSED)

  (check
      (let ((D (list->deque LIST-100)))
	(deque-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => LIST-100-REVERSED)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D
	  (deque 0 1 2 3 4 5))
	(deque-fold-left (lambda (knil obj)
			    (cons obj knil))
	  '() D))
    => '(5 4 3 2 1 0))

;;; --------------------------------------------------------------------
;;; rear folding

  (check
      (let ((D (make-deque)))
	(deque-fold-right (lambda (knil obj)
			   (cons obj knil))
	  '() D))
    => '())

  (check
      (let* ((L LIST-5)
	     (D (list->deque L)))
	(deque-fold-right cons '() D))
    => LIST-5)

  (check
      (let ((D (list->deque LIST-20)))
	(deque-fold-right cons '() D))
    => LIST-20)

  (check
      (let ((D (list->deque LIST-100)))
	(deque-fold-right cons '() D))
    => LIST-100)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D
	  (deque 0 1 2 3 4 5))
	(deque-fold-right (lambda (obj knil)
			   (cons obj knil))
	  '() D))
    => '(0 1 2 3 4 5))

  (collect))


(parametrise ((check-test-name	'copy))

  (check
      (deque->list (deque-copy! (make-deque) (deque)))
    => '())

  (check
      (deque->list (deque-copy! (make-deque) (list->deque LIST-5)))
    => LIST-5)

  (check
      (deque->list (deque-copy! (make-deque) (list->deque LIST-20)))
    => LIST-20)

  (check
      (deque->list (deque-copy! (make-deque) (list->deque LIST-100)))
    => LIST-100)

  #t)


(parametrise ((check-test-name	'reverse))

  (check
      (deque->list (deque-reverse! (deque) (deque)))
    => '())

  (check
      (deque->list (deque-reverse! (deque) (deque 0)))
    => '(0))

  (check
      (deque->list (deque-reverse! (deque) (deque 0 1 2 3 4)))
    => '(4 3 2 1 0))

  #t)





(parametrise ((check-test-name	'mapping))

  (check (deque->list (deque-map-left (deque) - (deque)))			=> '())
  (check (deque->list (deque-map-left (deque) - (list->deque LIST-5)))		=> LIST-5-NEGATED)
  (check (deque->list (deque-map-left (deque) - (list->deque LIST-20)))	=> LIST-20-NEGATED)
  (check (deque->list (deque-map-left (deque) - (list->deque LIST-100)))	=> LIST-100-NEGATED)

  (check (deque->list (deque-map-right (deque) - (deque)))			=> '())
  (check (deque->list (deque-map-right (deque) - (list->deque LIST-5)))		=> LIST-5-NEGATED)
  (check (deque->list (deque-map-right (deque) - (list->deque LIST-20)))		=> LIST-20-NEGATED)
  (check (deque->list (deque-map-right (deque) - (list->deque LIST-100)))	=> LIST-100-NEGATED)

  (check (deque->list (deque-map (deque) - (deque)))			=> '())
  (check (deque->list (deque-map (deque) - (list->deque LIST-5)))		=> LIST-5-NEGATED)
  (check (deque->list (deque-map (deque) - (list->deque LIST-20)))	=> LIST-20-NEGATED)
  (check (deque->list (deque-map (deque) - (list->deque LIST-100)))	=> LIST-100-NEGATED)

  #t)


(parametrise ((check-test-name	'for-each))

;;; for-each-left

  (check
      (with-result
	(deque-for-each-left add-result (deque)))
    => '(#!void ()))

  (check
      (with-result
	(deque-for-each-left add-result (list->deque LIST-5)))
    => `(#!void ,LIST-5))

  (check
      (with-result
	(deque-for-each-left add-result (list->deque LIST-20)))
    => `(#!void ,LIST-20))

  (check
      (with-result
	(deque-for-each-left add-result (list->deque LIST-100)))
    => `(#!void ,LIST-100))

;;; --------------------------------------------------------------------
;;; for-each-right

  (check
      (with-result
	(deque-for-each-right add-result (deque)))
    => '(#!void ()))

  (check
      (with-result
	(deque-for-each-right add-result (list->deque LIST-5)))
    => `(#!void ,LIST-5-REVERSED))

  (check
      (with-result
	(deque-for-each-right add-result (list->deque LIST-20)))
    => `(#!void ,LIST-20-REVERSED))

  (check
      (with-result
	(deque-for-each-right add-result (list->deque LIST-100)))
    => `(#!void ,LIST-100-REVERSED))

;;; --------------------------------------------------------------------
;;; for-each-left aliases

  (check
      (with-result
	(deque-for-each add-result (deque)))
    => '(#!void ()))

  (check
      (with-result
	(deque-for-each add-result (list->deque LIST-5)))
    => `(#!void ,LIST-5))

  (check
      (with-result
	(deque-for-each add-result (list->deque LIST-20)))
    => `(#!void ,LIST-20))

  (check
      (with-result
	(deque-for-each add-result (list->deque LIST-100)))
    => `(#!void ,LIST-100))

  #t)


(parametrise ((check-test-name	'purge))

  (check
      (let ((D (list->deque LIST-100)))
	(deque-purge! D)
	(deque-size D))
    => 0)

  (check
      (let ((D (list->deque LIST-100)))
	(deque-purge! D)
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-20)
	(deque-size D))
    => 20)

  (check
      (let ((D (list->deque LIST-100)))
	(deque-purge! D)
	(map (lambda (obj)
	       (deque-push-front! D obj))
	  LIST-20)
	(deque->list D))
    => LIST-20-REVERSED)

  (check
      (let ((D (list->deque LIST-100)))
	(deque-purge! D)
	(map (lambda (obj)
	       (deque-push-rear! D obj))
	  LIST-20)
	(deque->list D))
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
	(deque-for-all
	    add-result
	  (deque)))
    => '(#t ()))


  (check
      (with-result
	(deque-for-all
	    add-result
	  (deque 10)))
    => '(10 (10)))


  (check
      (with-result
	(deque-for-all
	    add-result
	  (deque 10 20 30)))
    => '(30 (10 20 30)))

  (check
      (with-result
	(deque-for-all
	    add-result
	  (deque 10 #f 30)))
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
	(deque-exists-left
	    fun
	  (deque)))
    => '(#f ()))

  (check
      (with-result
	(deque-exists-left
	    fun
	  (deque 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(deque-exists-left
	    fun
	  (deque 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(deque-exists-left
	    fun
	  (deque 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(deque-exists-left
	    fun
	  (deque 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

;;; --------------------------------------------------------------------
;;; from right

  (check
      (with-result
	(deque-exists-right
	    fun
	  (deque)))
    => '(#f ()))

  (check
      (with-result
	(deque-exists-right
	    fun
	  (deque 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(deque-exists-right
	    fun
	  (deque 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(deque-exists-right
	    fun
	  (deque 30 20 10)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(deque-exists-right
	    fun
	  (deque 25 20 10)))
    => '(#f (#(10) #(20) #(25))))

;;; --------------------------------------------------------------------
;;; left/right difference

  (check
      (deque-exists-left
	  (lambda (obj)
	    (and (even? obj)
		 obj))
	(deque 1 2 3 5 6 7))
    => 2)

  (check
      (deque-exists-right
	  (lambda (obj)
	    (and (even? obj)
		 obj))
	(deque 1 2 3 5 6 7))
    => 6)

;;; --------------------------------------------------------------------
;;; from left aliases

  (check
      (with-result
	(deque-exists
	    fun
	  (deque)))
    => '(#f ()))

  (check
      (with-result
	(deque-exists
	    fun
	  (deque 10)))
    => '(#f (#(10))))

  (check
      (with-result
	(deque-exists
	    fun
	  (deque 30)))
    => '(30 (#(30))))

  (check
      (with-result
	(deque-exists
	    fun
	  (deque 10 20 30)))
    => '(30 (#(10) #(20) #(30))))

  (check
      (with-result
	(deque-exists
	    fun
	  (deque 10 20 25)))
    => '(#f (#(10) #(20) #(25))))

  #t)


(parametrise ((check-test-name	'find))

  (check (deque-find-left even? (deque))				=> #f)
  (check (deque-find-left even? (deque) 'not-found)			=> 'not-found)

  (check (deque-find-left even? (deque 1 3 5 7))			=> #f)
  (check (deque-find-left even? (deque 1 3 5 7) 'not-found)		=> 'not-found)

  (check (deque-find-left even? (deque 1 3 5 6 7))			=> 6)
  (check (deque-find-left even? (deque 1 3 5 6 7) 'not-found)		=> 6)

;;; --------------------------------------------------------------------

  (check (deque-find-right even? (deque))				=> #f)
  (check (deque-find-right even? (deque) 'not-found)			=> 'not-found)

  (check (deque-find-right even? (deque 1 3 5 7))			=> #f)
  (check (deque-find-right even? (deque 1 3 5 7) 'not-found)		=> 'not-found)

  (check (deque-find-right even? (deque 1 3 5 6 7))			=> 6)
  (check (deque-find-right even? (deque 1 3 5 6 7) 'not-found)		=> 6)

;;; --------------------------------------------------------------------
;;; left/rigt difference

  (check (deque-find-left  even? (deque 1 2 3 5 6 7))			=> 2)
  (check (deque-find-right even? (deque 1 2 3 5 6 7))			=> 6)

;;; --------------------------------------------------------------------
;;; left aliases

  (check (deque-find even? (deque))					=> #f)
  (check (deque-find even? (deque) 'not-found)				=> 'not-found)

  (check (deque-find even? (deque 1 3 5 7))				=> #f)
  (check (deque-find even? (deque 1 3 5 7) 'not-found)			=> 'not-found)

  (check (deque-find even? (deque 1 3 5 6 7))				=> 6)
  (check (deque-find even? (deque 1 3 5 6 7) 'not-found)		=> 6)

  #t)


(parametrise ((check-test-name	'filter))

  (check
      (deque->list (deque-filter (deque) even? (deque)))
    => '())

  (check
      (deque->list (deque-filter (deque) even? (deque 1 3 5 7)))
    => '())

  (check
      (deque->list (deque-filter (deque) even? (deque 1 3 5 6 7)))
    => '(6))

  (check
      (deque->list (deque-filter (deque) even? (deque 1 2 3 4 5 6 7)))
    => '(2 4 6))

  (check
      (deque->list (deque-filter (deque) even? (deque 1 2 3 4 5 6 7 8)))
    => '(2 4 6 8))

  (check
      (deque->list (deque-filter (deque) even? (deque 2 4 6 8)))
    => '(2 4 6 8))

  #t)


(parametrise ((check-test-name	'partition))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?body ?expected-in ?expected-ou)
       (check
	   (receive (in ou)
	       ?body
	     (values (deque->list in) (deque->list ou)))
	 => ?expected-in ?expected-ou))
      ))

;;; --------------------------------------------------------------------

  (doit (deque-partition (deque) (deque) even? (deque))				'() '())

  (doit (deque-partition (deque) (deque) even? (deque 1 3 5 7))			'() '(1 3 5 7))
  (doit (deque-partition (deque) (deque) even? (deque 2 4 6 8))			'(2 4 6 8) '())

  (doit (deque-partition (deque) (deque) even? (deque 1 3 5 6 7))		'(6) '(1 3 5 7))

  (doit (deque-partition (deque) (deque) even? (deque 1 2 3 4 5 6 7))		'(2 4 6) '(1 3 5 7))

  (doit (deque-partition (deque) (deque) even? (deque 1 2 3 4 5 6 7 8))		'(2 4 6 8) '(1 3 5 7))

  #t)



(parametrise ((check-test-name 'conversion))

  (check
      (deque->list (make-deque))
    => '())

  (check
      (deque->list (deque 1))
    => '(1))

  (check
      (deque->list (deque 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (deque->vector (make-deque))
    => '#())

  (check
      (deque->vector (deque 1))
    => '#(1))

  (check
      (deque->vector (deque 1 2 3))
    => '#(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (deque->list (list->deque '()))
    => '())

  (check
      (deque->list (list->deque '(1)))
    => '(1))

  (check
      (deque->list (list->deque '(1 2 3)))
    => '(1 2 3))

  (check
      (deque->list (list->deque (make-list-100)))
    => (make-list-100))

  (check
      (deque->list (list->deque LIST-100))
    => LIST-100)

  ;;Documentation example.
  ;;
  (check
      (internal-body
	(define D (list->deque '(0 1 2)))
	(values (deque-front D)
		(deque-rear  D)))
    => 0 2)

;;; --------------------------------------------------------------------

  (check
      (deque->vector (vector->deque '#()))
    => '#())

  (check
      (deque->vector (vector->deque '#(1)))
    => '#(1))

  (check
      (deque->vector (vector->deque '#(1 2 3)))
    => '#(1 2 3))

  (check
      (deque->vector (vector->deque VECTOR-20))
    => VECTOR-20)

  (check
      (deque->vector (vector->deque VECTOR-5))
    => VECTOR-5)

  (check
      (deque->vector (vector->deque VECTOR-100))
    => VECTOR-100)

  (check
      (internal-body
	(define D (vector->deque '#(0 1 2)))
	(values (deque-front D)
		(deque-rear  D)
		(deque->vector D)))
    => 0 2 '#(0 1 2))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'deque-fold-left		'scheme-indent-function 1)
;; eval: (put 'deque-fold-right		'scheme-indent-function 1)
;; eval: (put 'deque-map-left		'scheme-indent-function 1)
;; eval: (put 'deque-map-right		'scheme-indent-function 1)
;; eval: (put 'deque-for-each-left	'scheme-indent-function 1)
;; eval: (put 'deque-for-each-right	'scheme-indent-function 1)
;; eval: (put 'deque-for-all		'scheme-indent-function 1)
;; eval: (put 'deque-find		'scheme-indent-function 1)
;; eval: (put 'deque-find-left		'scheme-indent-function 1)
;; eval: (put 'deque-find-right		'scheme-indent-function 1)
;; eval: (put 'deque-exists		'scheme-indent-function 1)
;; eval: (put 'deque-exists-left	'scheme-indent-function 1)
;; eval: (put 'deque-exists-right	'scheme-indent-function 1)
;; End:
