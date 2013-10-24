;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: callable objects
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
(library (vicare language-extensions callables)
  (export
    callable
    callable?
    callable.vicare-arguments-validation
    callable/false.vicare-arguments-validation
    callable-object
    $callable-object)
  (import (vicare)
    (vicare arguments validation)
    (vicare system $codes)
    (vicare system $fx))


(define-struct :callable-data
  (object function))

(define (callable? obj)
  (and (procedure? obj)
       ($fx= 1 ($code-freevars ($closure-code obj)))
       (:callable-data? ($cpref obj 0))))

(define-argument-validation (callable who obj)
  (callable? obj)
  (assertion-violation who "expected callable object as argument" obj))

(define-argument-validation (callable/false who obj)
  (or (not obj) (callable? obj))
  (assertion-violation who "expected false or callable object as argument" obj))

(define-syntax-rule (callable ?object ?function)
  (let ((data (let ((proc ?function))
		(assert (procedure? proc))
		(make-:callable-data ?object proc))))
    (lambda args
      (apply ($:callable-data-function data)
	     ($:callable-data-object   data)
	     args))))

(define (callable-object obj)
  (define who 'callable-object)
  (with-arguments-validation (who)
      ((callable	obj))
    ($callable-object obj)))

(define ($callable-object clbl)
  ($:callable-data-object ($cpref clbl 0)))


;;;; done

)

;;; end of file
