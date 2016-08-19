;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for recursive type definitions
;;;Date: Fri Aug 19, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test)
  (options typed-language)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: recursive types definition\n")


;;;; helpers

(define-constant THE-ENVIRONMENT
  (environment '(vicare)))

(define (%eval sexp)
  (eval sexp
	THE-ENVIRONMENT
	(expander-options typed-language)
	(compiler-options)))

(define (%print-message print? message)
  (when print?
    (fprintf (current-error-port) "~s\n" message)))


(parametrise ((check-test-name	'forward-definitions))

  (check
      (internal-body
	(define-type <it>)
	(define-type <it>)
	(define-type <it>
	  <fixnum>)
        (values (is-a? 123 <it>)
		(is-a? 1.2 <it>)))
    => #t #f)

  (check
      (internal-body
	(define-type <it>)
	(define-type <it>
	  (list-of <fixnum>))

	(define {O <it>}
	  '(1 2 3))

	O)
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'simple-recursion))

  (check
      (internal-body
	(define-type <it>
	  (or (list-of <fixnum>)
	      (vector-of <it>)))

	(values (is-a? '(1 2 3) <it>)
		(is-a? '#((1 2) (3 4)) <it>)
		(is-a? '("1" 2 3) <it>)
		))
    => #t #t #f)

;;; --------------------------------------------------------------------
;;; errors

  (check
      (try
	  (%eval '(internal-body
		    (define-type <damn>
		      (not <damn>))
		    #f))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(not <damn>))

  (check
      (try
	  (%eval '(internal-body
		    (define-type <damn>
		      (or <fixnum> <damn>))
		    #f))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '<damn>)

  (check
      (try
	  (%eval '(internal-body
		    (define-type <damn>
		      (and <fixnum> <damn>))
		    #f))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '<damn>)

  (void))


(parametrise ((check-test-name	'record-types))

  (check
      (internal-body
	(define-record-type <node>
	  (fields {lx (or <node> <false>)}
		  {rx (or <node> <false>)})
	  (protocol
	    (lambda (make-record)
	      (case-lambda
		(()
		 (make-record #f #f))
		((lx rx)
		 (make-record lx rx)))))
	  (constructor-signature
	    (case-lambda
	      (()		=> (<node>))
	      ((<node> <node>)	=> (<node>)))))

	(values (is-a? (new <node>) <node>)
		(is-a? (new <node> (new <node>) (new <node>)) <node>)))
    => #t #t)

  (void))


(parametrise ((check-test-name	'struct-types))

  (check
      (internal-body
	(define-struct <node>
	  ({lx (or <node> <false>)}
	   {rx (or <node> <false>)}))

	(values (is-a? (new <node>) <node>)
		(is-a? (new <node> (new <node>) (new <node>)) <node>)))
    => #t #t)

  (void))




;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
