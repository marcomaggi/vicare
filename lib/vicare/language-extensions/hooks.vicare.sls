;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: hooks definition
;;;Date: Wed Jun 15, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare language-extensions hooks)
  (export
    make-hook
    hook.vicare-arguments-validation
    hook?		hook-empty?
    add-hook!		remove-hook!
    reset-hook!
    hook->list		run-hook)
  (import (vicare)
    (vicare arguments validation)
    (vicare system $pairs))


(define-record-type hook
  (nongenerative vicare:hooks:<hook>)
  (opaque #t)
  (sealed #t)
  (fields (mutable functions))
  (protocol
   (lambda (make-record)
     (lambda ()
       (make-record '())))))

(define-argument-validation (hook who obj)
  (hook? obj)
  (assertion-violation who "expected hook object as argument" obj))

(define add-hook!
  (case-lambda
   ((hook func)
    (add-hook! hook func #f))
   ((hook func last?)
    (define who 'add-hook!)
    (with-arguments-validation (who)
	((hook		hook)
	 (procedure	func))
      (let ((ell ($hook-functions hook)))
	(cond ((null? ell)
	       ($hook-functions-set! hook (list func)))
	      (last?
	       ($set-cdr! (let last-pair ((ell ell))
			    (if (null? ($cdr ell))
				ell
			      (last-pair ($cdr ell))))
			  (list func)))
	      (else
	       ($hook-functions-set! hook (cons func ell)))))))
   ))

(define (remove-hook! hook func)
  (define who 'remove-hook!)
  (with-arguments-validation (who)
      ((hook		hook)
       (procedure	func))
    ($hook-functions-set! hook (let remq ((ell ($hook-functions hook)))
				 (cond ((null? ell)
					'())
				       ((eq? func ($car ell))
					($cdr ell))
				       (else
					(cons ($car ell) (remq ($cdr ell)))))))))

(define (reset-hook! hook)
  (define who 'reset-hook!)
  (with-arguments-validation (who)
      ((hook	hook))
    ($hook-functions-set! hook '())))

(define (hook-empty? hook)
  (define who 'hook-empty?)
  (with-arguments-validation (who)
      ((hook	hook))
    (null? ($hook-functions hook))))

(define (hook->list hook)
  (define who 'hook-empty?)
  (with-arguments-validation (who)
      ((hook	hook))
    (let list-copy ((ell ($hook-functions hook)))
      (if (pair? ell)
	  (cons ($car ell) (list-copy ($cdr ell)))
	ell))))

(define (run-hook hook . args)
  (define who 'run-hook)
  (with-arguments-validation (who)
      ((hook	hook))
    (for-each (lambda (func)
		(apply func args))
      ($hook-functions hook))))


;;;; done

)

;;; end of file
