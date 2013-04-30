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


(parametrise ((check-test-name	'test-define-integrable))

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
      (receive (a b c)
		(values 1 2 3)
		(list a b c))
    => '(1 2 3))

  (check
      (receive (a)
		1
		a)
    => 1)

  #t)


(parametrise ((check-test-name	'begin0))

  (check
      (begin0
       1)
    => 1)

  (check
      (call-with-values
	  (lambda ()
	    (begin0
	     (values 1 2 3)))
	list)
    => '(1 2 3))

  (check
      (with-result
       (begin0
	1
	(add-result 2)
	(add-result 3)))
    => '(1 (2 3)))

  (check
      (with-result
       (call-with-values
	   (lambda ()
	     (begin0
	      (values 1 10)
	      (add-result 2)
	      (add-result 3)))
	 list))
    => '((1 10) (2 3)))


  #t)


(parametrise ((check-test-name	'define-inline))

  (check
      (let ()
	(define-inline (ciao a b)
	  (+ a b))
	(ciao 1 2))
    => 3)

  (check
      (let ()
	(define-inline (ciao)
	  (+ 1 2))
	(ciao))
    => 3)

  (check
      (let ()
	(define-inline (ciao . rest)
	  (apply + rest))
	(ciao 1 2))
    => 3)

  (check
      (let ()
	(define-inline (ciao a . rest)
	  (apply + a rest))
	(ciao 1 2))
    => 3)

  #t)


(parametrise ((check-test-name	'define-constant))

  (check
      (let ()
	(define-constant a 123)
	a)
    => 123)

  #t)


(parametrise ((check-test-name	'define-integrable))

  (check
      (let ()
	(define-integrable (fact n)
	  (if (< n 2)
	      1
	    (* n (fact (- n 1)))))
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

  (check
      (let ()
	(define-integrable (incr x)
	  (+ x 1))
	(map incr '(10 20 30)))
    => '(11 21 31))

  #t)


(parametrise ((check-test-name	'define-syntax-rule))

  (check
      (let ()
	(define-syntax-rule (ciao a b)
	  (+ a b))
	(ciao 1 2))
    => 3)

  (check
      (let ()
	(define-syntax-rule (ciao)
	  (+ 1 2))
	(ciao))
    => 3)

  (check
      (let ()
	(define-syntax-rule (ciao . ?rest)
	  (+ . ?rest))
	(ciao 1 2))
    => 3)

  (check
      (let ()
	(define-syntax-rule (ciao a . ?rest)
	  (+ a . ?rest))
	(ciao 1 2))
    => 3)

  #t)


(parametrise ((check-test-name	'test-while))

  (define-fluid-syntax continue
    (lambda (stx)
      (syntax-error 'continue "syntax \"continue\" out of any loop")))

  (define-fluid-syntax break
    (lambda (stx)
      (syntax-error 'continue "syntax \"break\" out of any loop")))

  (define-syntax while
    (syntax-rules ()
      ((_ ?test ?body ...)
       (call/cc
	   (lambda (escape)
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ?test
		     (begin
		       ?body ...
		       (loop))
		   (escape)))))))
      ))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let ((i 5))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i)))
	 i))
    => '(0 (5 4 3 2 1)))

  (check
      (with-result
       (let ((i 0))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i)))
	 i))
    => '(0 ()))

  (check
      (with-result	;continue
       (let ((i 5))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (continue)
	   (add-result "post"))
	 i))
    => '(0 (5 4 3 2 1)))

  (check
      (with-result	;break
       (let ((i 5))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (break)
	   (add-result "post"))
	 i))
    => '(4 (5)))

  (check		;break with single value
      (with-result
       (let ((i 5))
	 (while (positive? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (break 'ciao)
	   (add-result "post"))))
    => '(ciao (5)))

  (check		;break with multiple values
      (with-result
       (let ((i 5))
	 (receive (a b)
	     (while (positive? i)
	       (add-result i)
	       (set! i (+ -1 i))
	       (break 'ciao 'hello)
	       (add-result "post"))
	   (list a b))))
    => '((ciao hello) (5)))

  #t)


(parametrise ((check-test-name	'test-until))

  (define-fluid-syntax continue
    (lambda (stx)
      (syntax-error 'continue "syntax \"continue\" out of any loop")))

  (define-fluid-syntax break
    (lambda (stx)
      (syntax-error 'continue "syntax \"break\" out of any loop")))

  (define-syntax until
    (syntax-rules ()
      ((_ ?test ?body ...)
       (call/cc
	   (lambda (escape)
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ?test
		     (escape)
		   (begin
		     ?body ...
		     (loop))))))))
      ))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (let ((i 5))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i)))
	 i))
    => '(0 (5 4 3 2 1)))

  (check
      (with-result
       (let ((i 0))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i)))
	 i))
    => '(0 ()))

  (check	;continue
      (with-result
       (let ((i 5))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (continue)
	   (add-result "post"))
	 i))
    => '(0 (5 4 3 2 1)))

  (check	;break with no values
      (with-result
       (let ((i 5))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (break)
	   (add-result "post"))
	 i))
    => '(4 (5)))

  (check	;break with single value
      (with-result
       (let ((i 5))
	 (until (zero? i)
	   (add-result i)
	   (set! i (+ -1 i))
	   (break 'ciao)
	   (add-result "post"))))
    => '(ciao (5)))

  (check	;break with multiple values
      (with-result
       (let ((i 5))
	 (receive (a b)
	     (until (zero? i)
	       (add-result i)
	       (set! i (+ -1 i))
	       (break 'ciao 'hello)
	       (add-result "post"))
	   (list a b))))
    => '((ciao hello) (5)))

  #t)


(parametrise ((check-test-name	'test-for))

  (define-fluid-syntax continue
    (lambda (stx)
      (syntax-error 'continue "syntax \"continue\" out of any loop")))

  (define-fluid-syntax break
    (lambda (stx)
      (syntax-error 'continue "syntax \"break\" out of any loop")))

  (define-syntax for
    (syntax-rules ()
      ((_ (?init ?test ?incr) ?body ...)
       (call/cc
	   (lambda (escape)
	     ?init
	     (let loop ()
	       (fluid-let-syntax ((break    (syntax-rules ()
					      ((_ . ?args)
					       (escape . ?args))))
				  (continue (lambda (stx) #'(loop))))
		 (if ?test
		     (begin
		       ?body ... ?incr
		       (loop))
		   (escape)))))))
      ))

;;; --------------------------------------------------------------------

  (check	;test true
      (with-result
       (for ((define i 5) (positive? i) (set! i (+ -1 i)))
	 (add-result i))
       #t)
    => '(#t (5 4 3 2 1)))

  (check	;test immediately false
      (with-result
       (for ((define i 0) (positive? i) (set! i (+ -1 i)))
	 (add-result i))
       #t)
    => '(#t ()))

  (check	;continue
      (with-result
       (for ((define i 5) (positive? i) (set! i (+ -1 i)))
	 (add-result i)
	 (set! i (+ -1 i))
	 (continue)
	 (add-result "post"))
       #t)
    => '(#t (5 4 3 2 1)))

  (check	;break with no values
      (with-result
       (for ((define i 5) (positive? i) (set! i (+ -1 i)))
	 (add-result i)
	 (break)
	 (add-result "post"))
       #t)
    => '(#t (5)))

  (check	;break with single value
      (with-result
       (for ((define i 5) (positive? i) (set! i (+ -1 i)))
	 (add-result i)
	 (break 'ciao)
	 (add-result "post")))
    => '(ciao (5)))

  (check	;break with multiple values
      (with-result
       (receive (a b)
	   (for ((define i 5) (positive? i) (set! i (+ -1 i)))
	     (add-result i)
	     (break 'ciao 'hello)
	     (add-result "post"))
	 (list a b)))
    => '((ciao hello) (5)))

  (check	;multiple bindings
      (with-result
       (for ((begin
	       (define i 5)
	       (define j 10))
	     (positive? i)
	     (begin
	       (set! i (+ -1 i))
	       (set! j (+ -1 j))))
	 (add-result i)
	 (add-result j))
       #t)
    => '(#t (5 10 4 9 3 8 2 7 1 6)))

  (check	;no bindings
      (with-result
       (let ((i #f))
	 (for ((set! i 5) (positive? i) (set! i (+ -1 i)))
	   (add-result i))
	 i))
    => '(0 (5 4 3 2 1)))

  #t)


(parametrise ((check-test-name	'unwind-protect))

  (define-syntax unwind-protect
    ;;Not a general UNWIND-PROTECT for Scheme,  but fine where we do not
    ;;use continuations to escape from the body.
    ;;
    (syntax-rules ()
      ((_ ?body ?cleanup0 ?cleanup ...)
       (let ((cleanup (lambda () ?cleanup0 ?cleanup ...)))
	 (with-exception-handler
	     (lambda (E)
	       (cleanup)
	       (raise E))
	   (lambda ()
	     (begin0
		 ?body
	       (cleanup))))))))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (unwind-protect
	   (begin
	     (add-result 'in)
	     1)
	 (add-result 'out)))
    => '(1 (in out)))

  (check
      (with-result
       (unwind-protect
	   (begin
	     (add-result 'in)
	     1)
	 (add-result 'out1)
	 (add-result 'out2)))
    => '(1 (in out1 out2)))

  (check	;multiple return values
      (with-result
       (receive (a b)
	   (unwind-protect
	       (begin
		 (add-result 'in)
		 (values 1 2))
	     (add-result 'out1)
	     (add-result 'out2))
	 (list a b)))
    => '((1 2) (in out1 out2)))

  (check	;zero return values
      (with-result
       (unwind-protect
  	   (begin
  	     (add-result 'in)
  	     (values))
  	 (add-result 'out1)
  	 (add-result 'out2))
       #t)
    => `(#t (in out1 out2)))

  (check	;exception in body
      (with-result
       (guard (E (else #t))
	 (unwind-protect
	     (begin
	       (add-result 'in)
	       (error #f "fail!!!")
	       (add-result 'after)
	       1)
	   (add-result 'out))))
    => '(#t (in out)))

  #t)


;;;; done

(check-report)

;;; end of file
