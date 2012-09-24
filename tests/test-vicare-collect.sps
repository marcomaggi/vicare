;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for garbage collector API
;;;Date: Sat Aug 25, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare garbage collection\n")


(parametrise ((check-test-name	'avoid))

  (check
      (pointer? (register-to-avoid-collecting #t))
    => #t)

  (check
      (pointer? (register-to-avoid-collecting '(1 2 3 4)))
    => #t)

  (check
      (pointer-null? (register-to-avoid-collecting (void)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (forget-to-avoid-collecting (register-to-avoid-collecting #t))
    => #t)

  (check
      (forget-to-avoid-collecting (register-to-avoid-collecting 123))
    => 123)

  (check
      (forget-to-avoid-collecting (null-pointer))
    => (void))

  (check
      (let ((obj '(1 2 3 4)))
	(eq? obj (forget-to-avoid-collecting (register-to-avoid-collecting obj))))
    => #t)

  (check
      (let* ((obj1 '(1 2 3 4))
	     (obj2 '(5 6 7 8))
	     (ptr  (register-to-avoid-collecting obj1))
	     (rv1  (eq? obj1 (replace-to-avoid-collecting ptr obj2)))
	     (rv2  (eq? obj2 (forget-to-avoid-collecting ptr))))
	(cons rv1 rv2))
    => '(#t . #t))

;;; --------------------------------------------------------------------

  (check
      (retrieve-to-avoid-collecting (register-to-avoid-collecting #t))
    => #t)

  (check
      (retrieve-to-avoid-collecting (register-to-avoid-collecting 123))
    => 123)

  (check
      (retrieve-to-avoid-collecting (null-pointer))
    => (void))

  (check
      (let ((obj '(1 2 3 4)))
	(eq? obj (retrieve-to-avoid-collecting (register-to-avoid-collecting obj))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (begin
	(purge-collection-avoidance-list)
	(collection-avoidance-list))
    => '())

  (check
      (begin
	(purge-collection-avoidance-list)
	(register-to-avoid-collecting '(1 . 2))
	(register-to-avoid-collecting '(3 . 4))
	(register-to-avoid-collecting "ciao ciao")
	(do ((i 0 (+ 1 i)))
	    ((= i 1000)
	     (collection-avoidance-list))
;;;	  (check-pretty-print (list 'gc-run i))
	  (collect)))
    => '((1 . 2)
	 (3 . 4)
	 "ciao ciao"))

;;; --------------------------------------------------------------------

  (check
      (begin
  	(purge-collection-avoidance-list)
  	(let ((A (register-to-avoid-collecting '(1 . 2)))
  	      (B (register-to-avoid-collecting '(3 . 4)))
  	      (C (register-to-avoid-collecting "ciao ciao")))
  	  (forget-to-avoid-collecting A)
  	  (collection-avoidance-list)))
    => '((3 . 4)
	 "ciao ciao"))

  (check
      (begin
  	(purge-collection-avoidance-list)
  	(let ((A (register-to-avoid-collecting '(1 . 2)))
  	      (B (register-to-avoid-collecting '(3 . 4)))
  	      (C (register-to-avoid-collecting "ciao ciao")))
  	  (forget-to-avoid-collecting B)
  	  (collection-avoidance-list)))
    => '((1 . 2)
	 "ciao ciao"))

  (check
      (begin
  	(purge-collection-avoidance-list)
  	(let ((A (register-to-avoid-collecting '(1 . 2)))
  	      (B (register-to-avoid-collecting '(3 . 4)))
  	      (C (register-to-avoid-collecting "ciao ciao")))
  	  (forget-to-avoid-collecting C)
  	  (collection-avoidance-list)))
    => '((1 . 2)
	 (3 . 4)))

  (check
      (begin
  	(purge-collection-avoidance-list)
  	(let ((A (register-to-avoid-collecting '(1 . 2))))
  	  (forget-to-avoid-collecting A)
  	  (collection-avoidance-list)))
    => '())

;;; --------------------------------------------------------------------

  (check
      (let ((ptrs '()))
	(purge-collection-avoidance-list)
	(do ((i 0 (+ 1 i)))
	    ((= i #e2e4))
	  (set! ptrs (cons (register-to-avoid-collecting i)
			   ptrs)))
	(for-all fixnum? (map forget-to-avoid-collecting ptrs)))
    => #t)

  (check
      (let ((ptrs '()))
	(purge-collection-avoidance-list)
	(do ((i 0 (+ 1 i)))
	    ((= i #e2e4))
	  (set! ptrs (cons (register-to-avoid-collecting i)
			   ptrs)))
;;;	(check-pretty-print (collection-avoidance-list))
	(purge-collection-avoidance-list)
	(collection-avoidance-list))
    => '())

  #t)


;;;; done

(check-report)

;;; end of file
