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


#!r6rs
(import (vicare)
  (vicare containers deques)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: deque containers\n")


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

  (check
      (deque-empty? (make-deque))
    => #t)

  (check
      (deque-empty? (deque 1))
    => #f)

  (check
      (deque-empty? (deque 1 2 3))
    => #f)

  #t)


(parametrise ((check-test-name		'object))

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


(parametrise ((check-test-name 'inspect))

  (check
      (deque-size (make-deque))
    => 0)

  (check
      (deque-size (deque 1))
    => 1)

  (check
      (deque-size (deque 1 2 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(deque-front (make-deque)))
    => "deque is empty")

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
    => "deque is empty")

  (check
      (deque-rear (deque 1))
    => 1)

  (check
      (deque-rear (deque 1 2 3))
    => 3)

  #t)


(parametrise ((check-test-name 'operations))

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
    => "deque is empty")

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

  #t)


;;;; done

(check-report)

;;; end of file
