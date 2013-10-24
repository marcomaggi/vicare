;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for callable objects
;;;Date: Thu Sep 12, 2013
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
  (vicare language-extensions callables)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: callable objects\n")


(parametrise ((check-test-name	'base))

;;; no additional arguments

  (check
      (let ((C (callable 123 (lambda (self) (+ 9000 self)))))
        (list (callable? C)
	      (callable-object C)
	      (C)))
    => '(#t 123 9123))

  (check
      (let ((C (callable 123 (lambda (self) (+ 9000 self)))))
        (list (callable? C)
	      ($callable-object C)
	      (C)))
    => '(#t 123 9123))

;;; --------------------------------------------------------------------
;;; with additional arguments

  (check
      (let ((C (callable 123 (lambda (self delta) (+ delta self)))))
        (list (callable? C)
  	      (callable-object C)
  	      (C 9000)))
    => '(#t 123 9123))

  (check
      (let ((C (callable 123 (lambda (self delta) (+ delta self)))))
        (list (callable? C)
  	      ($callable-object C)
  	      (C 9000)))
    => '(#t 123 9123))

;;; --------------------------------------------------------------------

  (check
      (callable? (lambda (obj) obj))
    => #f)

  (check
      (let ((self 123))
	(callable? (lambda (obj) (list self obj))))
    => #f)

  (check
      (guard (E ((assertion-violation? E)
		 (condition-message E))
		(else E))
	(let ((self 123))
	  (callable-object (lambda (obj) (list self obj)))))
    => "expected callable object as argument")

  #t)


;;;; done

(check-report)

;;; end of file
