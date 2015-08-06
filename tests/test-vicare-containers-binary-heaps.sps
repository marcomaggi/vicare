;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for binary heap containers
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
  (vicare containers binary-heaps)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: binary heap containers\n")


(parametrise ((check-test-name	'making))

  (check
      (binary-heap? (make-binary-heap <))
    => #t)

  (check
      (binary-heap? (make-binary-heap < 123))
    => #t)

  #t)


(parametrise ((check-test-name 'pred))

  (check
      (let ((H (make-binary-heap <)))
	(binary-heap-empty? H))
    => #t)

  (check
      (let ((H (make-binary-heap <)))
	(binary-heap-push! H 123)
	(binary-heap-empty? H))
    => #f)

  (check
      (let ((H (make-binary-heap <)))
	(binary-heap-push! H 123)
	(binary-heap-not-empty? H))
    => #t)

  #t)


(parametrise ((check-test-name	'object))

  (define who 'test)

;;; hash

  (check-for-true
   (integer? (binary-heap-hash (make-binary-heap <))))

  (check
      (let ((A (make-binary-heap <))
	    (B (make-binary-heap <))
	    (T (make-hashtable binary-heap-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (make-binary-heap <)))
	(binary-heap-property-list S))
    => '())

  (check
      (let ((S (make-binary-heap <)))
	(binary-heap-putprop S 'ciao 'salut)
	(binary-heap-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (make-binary-heap <)))
	(binary-heap-getprop S 'ciao))
    => #f)

  (check
      (let ((S (make-binary-heap <)))
	(binary-heap-putprop S 'ciao 'salut)
	(binary-heap-remprop S 'ciao)
	(binary-heap-getprop S 'ciao))
    => #f)

  (check
      (let ((S (make-binary-heap <)))
	(binary-heap-putprop S 'ciao 'salut)
	(binary-heap-putprop S 'hello 'ohayo)
	(list (binary-heap-getprop S 'ciao)
	      (binary-heap-getprop S 'hello)))
    => '(salut ohayo))

  #f)


(parametrise ((check-test-name 'inspect))

  (check
      (binary-heap-size (make-binary-heap <))
    => 0)

  (check
      (let ((H (make-binary-heap <)))
	(binary-heap-push! H 123)
	(binary-heap-size H))
    => 1)

  (check
      (let ((H (make-binary-heap <)))
	(binary-heap-push! H 1)
	(binary-heap-push! H 2)
	(binary-heap-push! H 3)
	(binary-heap-size H))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(binary-heap-top (make-binary-heap <)))
    => "binary heap is empty")

  (check
      (let ((H (make-binary-heap <)))
	(binary-heap-push! H 123)
	(binary-heap-top H))
    => 123)

  #t)


(parametrise ((check-test-name 'operations))

  (check
      (let ((q (make-binary-heap <)))
	(binary-heap-push! q 1)
	(binary-heap-push! q 2)
	(binary-heap-push! q 3)
	(binary-heap-sort-to-list! q))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (make-binary-heap <)))
	  (binary-heap-pop! q)))
    => "binary heap is empty")

  #t)


(parametrise ((check-test-name 'sorting))

  (check
      (let ((H (make-binary-heap <)))
	(binary-heap-fill! H '(3 5 7 0 6 5 34 3 6 9 67 5 4 4 3 1 2 3))
	(binary-heap-sort-to-list! H))
    => '(0 1 2 3 3 3 3 4 4 5 5 5 6 6 7 9 34 67))

  (check
      (let ((H (make-binary-heap < 16)))
	(binary-heap-fill! H '(3 5 7 0 6 5 34 3 6 9 67 5 4 4 3 1 2 3))
	(binary-heap-sort-to-list! H))
    => '(0 1 2 3 3 3 3 4 4 5 5 5 6 6 7 9 34 67))

  #t)


;;;; done

(check-report)

;;; end of file
