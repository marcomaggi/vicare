;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for generic functions as class methods
;;;Date: Tue Mar 22, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa language multimethods)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing generic functions as class methods\n")


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


(parametrise ((check-test-name	'method-redefinition))

  (define-class <alpha>
    (fields a)
    (methods (doit doit)))

  (define-class <beta>
    (parent <alpha>)
    (fields b)
    (methods (doit doit)))

  (define-generic doit (o arg))

  (define-method (doit (o <alpha>) arg)
    (list 'doit-alpha arg))

  (define-method (doit (o <beta>) arg)
    (list 'doit-beta arg))

  (check
      (let (((o <alpha>) (<alpha> (1))))
	(o doit 10))
    => '(doit-alpha 10))

  (check
      (let (((o <beta>) (<beta> (1 2))))
	(o doit 20))
    => '(doit-beta 20))

  (check
      (let (((o <alpha>) (<beta> (1 2))))
	(o doit 30))
    => '(doit-beta 30))

  #t)


(parametrise ((check-test-name	'pre-post-conditions))

  (define-class <alpha>
    (fields a)
    (methods (doit doit)))

  (define-class <beta>
    (parent <alpha>)
    (fields b)
    (methods (doit doit)))

  (define-generic*-definer define-generic*
    (argument-type-inspector tag-unique-identifiers-of)
    (reverse-before-methods? #t))

  (define-generic* doit (o arg))

  (define-method (doit (o <alpha>) arg)
    (list 'doit-alpha arg))

  (define-method (doit (o <beta>) arg)
    (list 'doit-beta arg))

  (define-method doit :before ((o <alpha>) arg)
    (add-result 'before-alpha))

  (define-method doit :after ((o <alpha>) arg)
    (add-result 'after-alpha))

  (define-method doit :before ((o <beta>) arg)
    (add-result 'before-beta))

  (define-method doit :after ((o <beta>) arg)
    (add-result 'after-beta))

  (check
      (with-result
       (let (((o <alpha>) (<beta> (1 2))))
	 (o doit 30)))
    => '((doit-beta 30)
	 (before-alpha before-beta after-alpha after-beta)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
