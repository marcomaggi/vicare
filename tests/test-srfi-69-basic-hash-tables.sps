;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 69, basic hash tables
;;;Date: Tue Feb  5, 2013
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
  (prefix (srfi :69) srfi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI 69: basic hash tables\n")


(parametrise ((check-test-name	'constructors))

  (check
      (srfi.hash-table? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table? H))
    => #t)

  (check
      (let ((H (srfi.make-hash-table string=?)))
        (srfi.hash-table? H))
    => #t)

  (check
      (let ((H (srfi.make-hash-table string=? string-hash)))
        (srfi.hash-table? H))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((H (srfi.alist->hash-table '((a . 1) (b . 2)))))
        (srfi.hash-table? H))
    => #t)

  (check
      (let ((H (srfi.alist->hash-table '(("a" . 1) ("b" . 2))
				       string=?)))
        (srfi.hash-table? H))
    => #t)

  (check
      (let ((H (srfi.alist->hash-table '(("a" . 1) ("b" . 2))
				       string=? string-hash)))
        (srfi.hash-table? H))
    => #t)

  #t)


(parametrise ((check-test-name	'inspection))

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-equivalence-function H))
    => equal?)

  (check
      (let ((H (srfi.make-hash-table eqv?)))
        (srfi.hash-table-equivalence-function H))
    => eqv?)

;;; --------------------------------------------------------------------

  (check
      (let ((H (srfi.make-hash-table string=? string-hash)))
        (srfi.hash-table-hash-function H))
    => string-hash)

  #t)


(parametrise ((check-test-name	'getters-setters))

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
	(srfi.hash-table-set! H "hello" 2)
        (list (srfi.hash-table-ref H "ciao")
	      (srfi.hash-table-ref H "hello")))
    => '(1 2))

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
	(srfi.hash-table-set! H "hello" 2)
	(srfi.hash-table-ref H "salut" (lambda () 123)))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
	(srfi.hash-table-set! H "hello" 2)
	(list (srfi.hash-table-ref/default H "ciao" 3)
	      (srfi.hash-table-ref/default H "salut" 4)))
    => '(1 4))

;;; --------------------------------------------------------------------

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
	(srfi.hash-table-delete! H "ciao")
	(srfi.hash-table-ref H "ciao" (lambda () 123)))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
	(srfi.hash-table-exists? H "ciao"))
    => #t)

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
	(srfi.hash-table-exists? H "salut"))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
        (srfi.hash-table-update! H "ciao" (lambda (v) (+ 10 v)))
	(srfi.hash-table-ref H "ciao"))
    => 11)

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
        (srfi.hash-table-update! H "hello"
				 (lambda (v) (+ 10 v))
				 (lambda () 123))
	(srfi.hash-table-ref H "hello"))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
        (srfi.hash-table-update!/default H "ciao" (lambda (v) (+ 10 v)) 99)
	(srfi.hash-table-ref H "ciao"))
    => 11)

  (check
      (let ((H (srfi.make-hash-table)))
        (srfi.hash-table-set! H "ciao" 1)
        (srfi.hash-table-update!/default H "hello"
					 (lambda (v) (+ 10 v))
					 123)
	(srfi.hash-table-ref H "hello"))
    => 133)

  #t)


;;;; done

(check-report)

;;; end of file
