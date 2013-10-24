;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for weak hashtables
;;;Date: Fri Feb  3, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare containers weak-hashtables)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare weak hashtables\n")


(parametrise ((check-test-name	'basic))

  (check
      (weak-hashtable? (make-weak-hashtable string-hash string=?))
    => #t)

  (check
      (weak-hashtable-size (make-weak-hashtable string-hash string=?))
    => 0)

  (check
      (weak-hashtable-size (make-weak-hashtable values =))
    => 0)

  #t)


(parametrise ((check-test-name	'setref))

  (check
      (let ((T (make-weak-hashtable string-hash string=?)))
        (weak-hashtable-ref T "ciao" #f))
    => #f)

  (check
      (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
	(list (weak-hashtable-ref T (vector-ref K 0) #f)
	      (weak-hashtable-ref T (vector-ref K 1) #f)
	      (weak-hashtable-ref T (vector-ref K 2) #f)
	      (weak-hashtable-ref T (vector-ref K 3) #f)
	      (weak-hashtable-ref T (vector-ref K 4) #f)
	      (weak-hashtable-ref T (vector-ref K 5) #f)
	      K))
    => '(1 2 3 4 5 6 #("a" "b" "c" "d" "e" "f")))

  (check
      (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
        (weak-hashtable-set! T (vector-ref K 0) 10) ;!!!
	(list (weak-hashtable-ref T (vector-ref K 0) #f)
	      (weak-hashtable-ref T (vector-ref K 1) #f)
	      (weak-hashtable-ref T (vector-ref K 2) #f)
	      (weak-hashtable-ref T (vector-ref K 3) #f)
	      (weak-hashtable-ref T (vector-ref K 4) #f)
	      (weak-hashtable-ref T (vector-ref K 5) #f)
	      K))
    => '(10 2 3 4 5 6 #("a" "b" "c" "d" "e" "f")))

;;; --------------------------------------------------------------------
;;; contains

  (check
      (let ((T (make-weak-hashtable string-hash string=? 5)))
	(weak-hashtable-contains? T "ciao"))
    => #f)

  (check
      (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
	(list (weak-hashtable-contains? T (vector-ref K 0))
	      K))
    => '(#t #("a" "b" "c" "d" "e" "f")))

  (check
      (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
	(list (weak-hashtable-contains? T "Z")
	      K))
    => '(#f #("a" "b" "c" "d" "e" "f")))

;;; --------------------------------------------------------------------
;;; delete

  (check
      (let ((T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-delete! T "ciao")
	(weak-hashtable-size T))
    => 0)

  (check
      (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
        (weak-hashtable-delete! T (vector-ref K 3))
	(list (weak-hashtable-contains? T (vector-ref K 3))
	      (weak-hashtable-size T)
	      K))
    => '(#f 5 #("a" "b" "c" "d" "e" "f")))

;;; --------------------------------------------------------------------

  (check	;update existing
      (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
	(weak-hashtable-update! T (vector-ref K 3)
				(lambda (x) (+ 1 x))
				#t)
	(list (weak-hashtable-ref T (vector-ref K 3) #f) K))
    => '(5 #("a" "b" "c" "d" "e" "f")))

  (check	;update non-existing
      (let ((K '#("a" "b" "c" "d" "e" "f" "g")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
	(weak-hashtable-update! T (vector-ref K 6)
				(lambda (x) (+ 1 x))
				100)
	(list (weak-hashtable-ref T (vector-ref K 6) #f) K))
    => '(101 #("a" "b" "c" "d" "e" "f" "g")))

;;; --------------------------------------------------------------------

  (check	;clear
      (let ((T (make-weak-hashtable string-hash string=? 5)))
	(weak-hashtable-clear! T)
	(weak-hashtable-size T))
    => 0)

  (check	;clear
      (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=? 5)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
	(weak-hashtable-clear! T)
	(list (weak-hashtable-size T) K))
    => '(0 #("a" "b" "c" "d" "e" "f")))

  #t)


(parametrise ((check-test-name	'inspection))

  (check	;size
      (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	    (T (make-weak-hashtable string-hash string=?)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
	(list K (weak-hashtable-size T)))
    => '(#("a" "b" "c" "d" "e" "f") 6))

  (check
      (let ((T (make-weak-hashtable string-hash string=?)))
	(weak-hashtable-keys T))
    => '#())

  (when #f	;keys
    (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	  (T (make-weak-hashtable string-hash string=?)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
	(check-pretty-print (weak-hashtable-keys T))))

  (check
      (let ((T (make-weak-hashtable string-hash string=?)))
	(call-with-values
	    (lambda ()
	      (weak-hashtable-entries T))
	  list))
    => '(#() #()))

  (when #f	;entries
    (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	  (T (make-weak-hashtable string-hash string=?)))
        (weak-hashtable-set! T (vector-ref K 0) 1)
        (weak-hashtable-set! T (vector-ref K 1) 2)
        (weak-hashtable-set! T (vector-ref K 2) 3)
        (weak-hashtable-set! T (vector-ref K 3) 4)
        (weak-hashtable-set! T (vector-ref K 4) 5)
        (weak-hashtable-set! T (vector-ref K 5) 6)
      (let-values (((keys vals) (weak-hashtable-entries T)))
	(check-pretty-print (list keys vals)))))

  #t)


(parametrise ((check-test-name	'stress))

  (check
      (let ((T (make-weak-hashtable values =))
	    (N 4096))
	(do ((i 0 (+ 1 i)))
	    ((= i N))
	  (weak-hashtable-set! T i i))
	(do ((i 0 (+ 1 i)))
	    ((= i N))
	  (assert (= i (weak-hashtable-ref T i #f))))
	(do ((i 0 (+ 1 i)))
	    ((= i N))
	  (assert (weak-hashtable-contains? T i)))
	(do ((i 0 (+ 1 i)))
	    ((= i N))
	  (weak-hashtable-update! T i (lambda (x) (+ 1 i)) -100))
	(do ((i 0 (+ 1 i)))
	    ((= i N))
	  (assert (= (+ 1 i) (weak-hashtable-ref T i #f))))
	(do ((i 0 (+ 1 i)))
	    ((= i N))
	  (weak-hashtable-delete! T i))
	(weak-hashtable-size T))
    => 0)

  #t)


(parametrise ((check-test-name	'misc))

  ;; printer
  (when #f
    (let ((K '#("a" "b" "c" "d" "e" "f")) ;attempt to prevent GC
	  (T (make-weak-hashtable string-hash string=? 5)))
      (weak-hashtable-set! T (vector-ref K 0) 1)
      (weak-hashtable-set! T (vector-ref K 1) 2)
      (weak-hashtable-set! T (vector-ref K 2) 3)
      (weak-hashtable-set! T (vector-ref K 3) 4)
      (weak-hashtable-set! T (vector-ref K 4) 5)
      (weak-hashtable-set! T (vector-ref K 5) 6)
      (check-pretty-print T)
      K))

  (check
      (let ((T (make-weak-hashtable string-hash string=? 5)))
	(weak-hashtable-set! T "ciao" 123)
	(weak-hashtable-set! T "ciao" 456)
	(list (weak-hashtable-size T)
	      (weak-hashtable-ref T "ciao" #f)))
    => '(1 456))

  #t)


;;;; done

(check-report)

;;; end of file
