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
(import (vicare)
  (vicare containers queues)
  (vicare arguments validation)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: queue containers\n")


(parametrise ((check-test-name	'making))

  (check
      (queue? (make-queue))
    => #t)

  (check
      (queue->list (make-queue))
    => '())

  (check
      (queue->list (make-queue 1))
    => '(1))

  (check
      (queue->list (make-queue 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name 'pred))

  (check
      (queue-empty? (make-queue))
    => #t)

  (check
      (queue-empty? (make-queue 1))
    => #f)

  (check
      (queue-empty? (make-queue 1 2 3))
    => #f)

  #t)


(parametrise ((check-test-name		'object))

  (define who 'test)

;;; hash

  (check-for-true
   (integer? (queue-hash (make-queue 1 2 3))))

  (check
      (let ((A (make-queue 1 2 3))
	    (B (make-queue 1 2 3))
	    (T (make-hashtable queue-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (make-queue 1 2 3)))
	(queue-property-list S))
    => '())

  (check
      (let ((S (make-queue 1 2 3)))
	(queue-putprop S 'ciao 'salut)
	(queue-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (make-queue 1 2 3)))
	(queue-getprop S 'ciao))
    => #f)

  (check
      (let ((S (make-queue 1 2 3)))
	(queue-putprop S 'ciao 'salut)
	(queue-remprop S 'ciao)
	(queue-getprop S 'ciao))
    => #f)

  (check
      (let ((S (make-queue 1 2 3)))
	(queue-putprop S 'ciao 'salut)
	(queue-putprop S 'hello 'ohayo)
	(list (queue-getprop S 'ciao)
	      (queue-getprop S 'hello)))
    => '(salut ohayo))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check-for-true
   (let ((S (make-queue 1 2 3)))
     (with-arguments-validation (who)
	 ((queue	S))
       #t)))

;;;

  (check-for-procedure-argument-violation
   (let ((S 123))
     (with-arguments-validation (who)
	 ((queue	S))
       #t))
   '(123))

  #f)


(parametrise ((check-test-name 'inspect))

  (check
      (queue-size (make-queue))
    => 0)

  (check
      (queue-size (make-queue 1))
    => 1)

  (check
      (queue-size (make-queue 1 2 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(queue-front (make-queue)))
    => "queue is empty")

  (check
      (queue-front (make-queue 1))
    => 1)

  (check
      (queue-front (make-queue 1 2 3))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(queue-rear (make-queue)))
    => "queue is empty")

  (check
      (queue-rear (make-queue 1))
    => 1)

  (check
      (queue-rear (make-queue 1 2 3))
    => 3)

  #t)


(parametrise ((check-test-name 'operations))

  (check
      (let ((q (make-queue)))
	(queue-push! q 1)
	(queue-push! q 2)
	(queue-push! q 3)
	(queue->list q))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (make-queue)))
	  (queue-pop! q)))
    => "queue is empty")

  (check
      (let ((q (make-queue 1 2 3)))
	(queue-pop! q))
    => 1)

  (check
      (let ((q (make-queue 1 2 3)))
	(queue-pop! q)
	(queue-pop! q)
	(queue-pop! q))
    => 3)

  (check
      (let ((q (make-queue 1 2 3)))
	(queue-pop! q)
	(queue-pop! q)
	(queue-pop! q)
	(queue-empty? q))
    => #t)

  #t)


(parametrise ((check-test-name 'conversion))

  (check
      (queue->list (make-queue))
    => '())

  (check
      (queue->list (make-queue 1))
    => '(1))

  (check
      (queue->list (make-queue 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (queue->vector (make-queue))
    => '#())

  (check
      (queue->vector (make-queue 1))
    => '#(1))

  (check
      (queue->vector (make-queue 1 2 3))
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

  #t)


;;;; done

(check-report)

;;; end of file
