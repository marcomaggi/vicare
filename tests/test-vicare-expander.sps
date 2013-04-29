;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for the expander
;;;Date: Tue Sep 25, 2012
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


#!vicare
(import (vicare)
  (rnrs eval)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare expander\n")


(parametrise ((check-test-name	'import))

  (check	;import separately a named module and a library, library
		;first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (vicare language-extensions syntaxes))
	(import ciao)
	(list (hello) (salut)))
    => '(hello salut))

  (check	;import separately a named  module and a library, module
		;first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import ciao)
	(import (vicare language-extensions syntaxes))
	(list (hello) (salut)))
    => '(hello salut))

  (check	;import both a named module and a library, library first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (vicare language-extensions syntaxes)
	  ciao)
	(list (hello) (salut)))
    => '(hello salut))

  (check	;import both a named module and a library, module first
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import ciao
	  (vicare language-extensions syntaxes))
	(list (hello) (salut)))
    => '(hello salut))

  #;(check	;import a named module with some name mangling
      (let ()
	(module ciao
	  (hello salut)
	  (define (hello) 'hello)
	  (define (salut) 'salut))
	(import (prefix ciao ciao.))
	(list (ciao.hello) (ciao.salut)))
    => '(hello salut))

  #t)


(parametrise ((check-test-name	'export))

  (check
      (let ()
	(module (green)
	  (define (green) 'green)
	  (define (yellow) 'yellow)
	  (export yellow))
	(list (green) (yellow)))
    => '(green yellow))

  #t)


(parametrise ((check-test-name	'deprefix))

  (check
      (eval '(str.length "ciao")
	    (environment
	     '(prefix
	       (deprefix (only (rnrs)
			       string-length
			       string-append)
			 string-)
	       str.)))
    => 4)

  #t)


(parametrise ((check-test-name	'fluid-let-syntax))

  (define-syntax define-integrable
    ;;Posted  by "leppie"  on the  Ikarus mailing  list; subject  "Macro
    ;;Challenge of Last Year [Difficulty: *****]", 20 Oct 2009.
    ;;
    (lambda (x)
      (define (make-residual-name name)
	(datum->syntax name
		       (string->symbol
			(string-append "residual-"
				       (symbol->string (syntax->datum name))))))
      (syntax-case x (lambda)
        ((_ (?name . ?formals) ?form1 ?form2 ...)
	 (identifier? #'?name)
	 #'(define-integrable ?name (lambda ?formals ?form1 ?form2 ...)))

        ((_ ?name (lambda ?formals ?form1 ?form2 ...))
         (identifier? #'?name)
         (with-syntax ((XNAME (make-residual-name #'?name)))
           #'(begin
               (define-fluid-syntax ?name
                 (lambda (x)
                   (syntax-case x ()
                     (_
		      (identifier? x)
		      #'XNAME)

                     ((_ arg (... ...))
                      #'((fluid-let-syntax
			     ((?name (identifier-syntax XNAME)))
                           (lambda ?formals ?form1 ?form2 ...))
                         arg (... ...))))))

               (define XNAME
                 (fluid-let-syntax ((?name (identifier-syntax XNAME)))
                   (lambda ?formals ?form1 ?form2 ...))))))
	)))

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define-integrable (fact n)
	  (let ((residual-fact (lambda (x)
				 (error 'fact "captured residual-fact"))))
	    (if (< n 2)
		1
	      (* n (fact (- n 1))))))
	(fact 5))
    => 120)

  (check
      (let ()
	(define-integrable (f x) (+ x 1))
	(eq? f f))
    => #t)

  (check
      (let ()
	(define-integrable (even? n) (or (zero? n) (odd? (- n 1))))
	(define-integrable (odd? n) (not (even? n)))
	(even? 5))
    => #f)

  #t)


(parametrise ((check-test-name	'define-values))

  (check
      (let ()
	(define-values (a)
	  1)
	a)
    => 1)

  (check
      (with-result
       (let ()
	 (define-values (a)
	   (add-result 2)
	   1)
	 a))
    => '(1 (2)))

  (check
      (let ()
  	(define-values (a b c)
  	  #t
  	  (values 1 2 3))
  	(list a b c))
    => '(1 2 3))

  (check
      (let ((a 2))
  	(define-values (a)
  	  (values 1))
  	a)
    => 1)

  #t)


(parametrise ((check-test-name	'define-constant-values))

  (check
      (let ()
	(define-constant-values (a b c)
	  #t
	  (values 1 2 3))
	(list a b c))
    => '(1 2 3))

  (check
      (let ()
	(define-constant-values (a)
	  #t
	  (values 1))
	a)
    => 1)

  (check
      (let ()
	(define-constant-values (a)
	  #t
	  1)
	a)
    => 1)

  #t)


(parametrise ((check-test-name	'receive))

  (check
      (receive* (a b c)
		(values 1 2 3)
		(list a b c))
    => '(1 2 3))

  (check
      (receive* (a)
		1
		a)
    => 1)

  #t)


;;;; done

(check-report)

;;; end of file
