;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for labels
;;;Date: Wed Dec 15, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (rnrs eval)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing class labels\n")


;;;; helpers

(define test-environ
  (environment '(except (rnrs)
			define
			lambda		case-lambda
			let		let*
			letrec		letrec*
			let-values	let*-values
			do		set!)
	       '(rename (nausicaa language oopp)
			(define/tags			define)
			(lambda/tags			lambda)
			(case-lambda/tags		case-lambda)
			(let/tags			let)
			(let*/tags			let*)
			(letrec/tags			letrec)
			(letrec*/tags			letrec*)
			(let-values/tags		let-values)
			(let*-values/tags		let*-values)
			(receive/tags			receive)
			(do/tags			do)
			(do*/tags			do*)
			(set!/tags			set!))))

(define-syntax %eval
  (syntax-rules ()
    ((_ ?form)
     (eval ?form test-environ))))

(define (debug-pretty-print thing)
  (pretty-print thing (current-error-port))
  (flush-output-port (current-error-port)))

(define (debug-write . args)
  (for-each (lambda (thing)
	      (write thing (current-error-port))
	      (display #\space (current-error-port)))
    args)
  (newline (current-error-port))
  (flush-output-port (current-error-port)))

(define (debug-newline thing)
  (newline (current-error-port))
  (flush-output-port (current-error-port)))

(define-syntax catch-syntax-violation
  (syntax-rules ()
    ((_ ?verbose . ?body)
     (guard (E ((syntax-violation? E)
		(when ?verbose
		  (debug-write (condition-message E)
			       (syntax-violation-subform E)))
		(syntax->datum (syntax-violation-subform E)))
	       (else E))
       . ?body))))

(define-syntax catch-assertion
  (syntax-rules ()
    ((_ ?verbose . ?body)
     (guard (E ((assertion-violation? E)
		(when ?verbose
		  (debug-write (condition-message E)
			       (condition-irritants E)))
		(condition-irritants E))
	       (else E))
       . ?body))))


(parametrise ((check-test-name	'virtual-fields))

  (let ()	;virtual fields

    (define-label <alpha>
      (virtual-fields a b c))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-c o) 3)

    (check
	(let (((o <alpha>) #t))
	  (list o (o a) (o b) (o c)))
      => '(#t 1 2 3))

    #f)

  (let ()	;virtual fields

    (define-label <alpha>
      (virtual-fields (mutable a)
		      (immutable b)
		      (immutable c)))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-c o) 3)

    (check
	(let (((o <alpha>) #t))
	  (list o (o a) (o b) (o c)))
      => '(#t 1 2 3))

    #f)

  #t)


(parametrise ((check-test-name	'methods))

  (let ()	;methods

    (define-label <alpha>
      (methods a b c))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-c o) 3)

    (check
	(let (((o <alpha>) #t))
	  (list o (o a) (o b) (o c)))
      => '(#t 1 2 3))

    #f)

  (let ()	;in-definition methods

    (define-label <alpha>
      (method (a o) 1)
      (method (b o) 2)
      (method (c o) 3))

    (check
	(let (((o <alpha>) #t))
	  (list o (o a) (o b) (o c)))
      => '(#t 1 2 3))

    #f)

  (let ()	;syntax methods

    (define-label <alpha>
      (method-syntax a
	(syntax-rules ()
	  ((_ ?obj) 1)))
      (method-syntax b
	(syntax-rules ()
	  ((_ ?obj) 2)))
      (method-syntax c
	(syntax-rules ()
	  ((_ ?obj) 3))))

    (check
	(let (((o <alpha>) #t))
	  (list o (o a) (o b) (o c)))
      => '(#t 1 2 3))

    #f)

  #t)


(parametrise ((check-test-name	'predicate))

  (let ()	;predicate, identifier

    (define-label <alpha>
      (predicate integer?))

    (check
	(is-a? 123 <alpha>)
      => #t)

    (check
	(is-a? "ciao" <alpha>)
      => #f)

    #f)

  (let ()	;predicate, inline expression

    (define-label <alpha>
      (predicate (lambda (o)
		   (and (integer? o) (exact? o)))))

    (check
	(is-a? 123 <alpha>)
      => #t)

    (check
    	(is-a? 123.4 <alpha>)
      => #f)

    (check
    	(is-a? "ciao" <alpha>)
      => #f)

    #f)

  #t)


(parametrise ((check-test-name	'setter-getter))

  (let ()	;setter

    (define-label <alpha>
      (setter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)) ?val)
		   #'(<alpha>-setf ?var ?key ?val))
		  ))))

    (define (<alpha>-setf o key val)
      (list o key val))

    (check
	(let (((o <alpha>) #t))
	  (set! (o (1)) 2))
      => '(#t 1 2))

    #f)

  (let ()	;getter

    (define-label <alpha>
      (getter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)))
		   #'(<alpha>-getf ?var ?key))
		  ))))

    (define (<alpha>-getf o key)
      (list o key))

    (check
	(let (((o <alpha>) #t))
	  (o (1)))
      => '(#t 1))

    #f)

  #t)


(parametrise ((check-test-name	'inheritance))

  (let ()	;virtual fields

    (define-label <alpha>
      (virtual-fields a b z))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-z o) 88)

    (define-label <beta>
      (parent <alpha>)
      (virtual-fields c d z))

    (define (<beta>-c o) 3)
    (define (<beta>-d o) 4)
    (define (<beta>-z o) 99)

    (check
	(let (((o <alpha>) #t))
	  (list o (o a) (o b) (o z)))
      => '(#t 1 2 88))

    (check
	(let (((o <beta>) #t))
	  (list o (o a) (o b) (o c) (o d) (o z)))
      => '(#t 1 2 3 4 99))

    #f)

  (let ()	;methods

    (define-label <alpha>
      (methods a b z))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-z o) 88)

    (define-label <beta>
      (parent <alpha>)
      (methods c d z))

    (define (<beta>-c o) 3)
    (define (<beta>-d o) 4)
    (define (<beta>-z o) 99)

    (check
	(let (((o <alpha>) #t))
	  (list o (o a) (o b) (o z)))
      => '(#t 1 2 88))

    (check
	(let (((o <beta>) #t))
	  (list o (o a) (o b) (o c) (o d) (o z)))
      => '(#t 1 2 3 4 99))

    #f)

  (let ()	;parent <top>

       (define-label <alpha>
	 (parent <top>)
	 (virtual-fields a b c))

       (define (<alpha>-a o) 1)
       (define (<alpha>-b o) 2)
       (define (<alpha>-c o) 3)

       (check
	   (let (((o <alpha>) #t))
	     (list o (o a) (o b) (o c)))
	 => '(#t 1 2 3))

       #f)


  (let ()	;parent is a class

    (define-class <alpha>
      (fields a b z))

    (define-label <beta>
      (parent <alpha>)
      (virtual-fields c d z))

    (define (<beta>-c o) 3)
    (define (<beta>-d o) 4)
    (define (<beta>-z o) 99)

    (check
	(let (((o <alpha>) (<alpha> (1 2 3))))
	  (list (o a) (o b) (o z)))
      => '(1 2 3))

    (check
	(let* (((p <alpha>)	(<alpha> (1 2 -3)))
	       ((o <beta>)	p))
	  (list (o a) (o b) (o c) (o d) (o z)))
      => '(1 2 3 4 99))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
