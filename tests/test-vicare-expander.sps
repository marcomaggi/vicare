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
;;;Copyright (C) 2012, 2013, 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; copyright notice for the XOR macro
;;;
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!vicare
(import (vicare)
  (prefix (vicare expander object-type-specs) typ.)
  (vicare language-extensions callables)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: expander syntaxes\n")


;;;; helpers

(define-syntax catch-expand-time-signature-violation
  (syntax-rules ()
    ((_ ?verbose . ?body)
     (guard (E ((typ.expand-time-retvals-signature-violation? E)
		(when ?verbose
		  (debug-print (condition-message E)
			       (syntax-violation-form E)))
		(values (syntax->datum (typ.retvals-signature-tags (typ.expand-time-retvals-signature-violation-expected-signature E)))
			(syntax->datum (typ.retvals-signature-tags (typ.expand-time-retvals-signature-violation-returned-signature E)))))
	       (else E))
       . ?body))))

(define (%eval sexp)
  (eval sexp (environment '(vicare))))


(parametrise ((check-test-name	'syntax-objects))

  (define-syntax (check-it stx)
    (syntax-case stx ()
      ((_ ?pattern ?syntax (_ . ?input) ?output)
       (let ((out #'(check
			(let ()
			  (define-syntax doit
			    (lambda (stx)
			      (syntax-case stx ()
				(?pattern ?syntax))))
			  (doit . ?input))
		      => ?output)))
	 #;(check-pretty-print (syntax->datum out))
	 out))))

;;; --------------------------------------------------------------------
;;; lists and pattern variables

  (check-it
      (_ ())
    (syntax 123)
    (_ ())
    123)

  (check-it
      (_ ?val)
    (syntax ?val)
    (_ 123)
    123)

  (check-it
      (_ ?a ?b ?c)
    (syntax (quote (?a ?b ?c)))
    (_ 1 2 3)
    '(1 2 3))

  (check-it
      (_ ?a ?b ?c)
    (syntax (list ?a ?b ?c))
    (_ 1 2 3)
    '(1 2 3))

  (check-it
      (_ (((?a ?b ?c))))
    (syntax (quote (?a ?b ?c)))
    (_ (((1 2 3))))
    '(1 2 3))

;;; --------------------------------------------------------------------
;;; improper lists and pattern variables

  (check-it
      (_ (?a ?b . ?c))
    (syntax (quote (?a ?b ?c)))
    (_ (1 2 . 3))
    '(1 2 3))

;;; --------------------------------------------------------------------
;;; pairs and pattern variables

  (check-it
      (_ (?a . ?b))
    (syntax (quote (?a ?b)))
    (_ (1 . 2))
    '(1 2))

  (check-it
      (_ ((?a . ?b) ?c))
    (syntax (quote (?a ?b ?c)))
    (_ ((1 . 2) 3))
    '(1 2 3))

;;; --------------------------------------------------------------------
;;; vectors and pattern variables

  (check-it
      (_ #())
    (syntax 123)
    (_ #())
    123)

  (check-it
      (_ #(?a ?b ?c))
    (syntax (quote (?a ?b ?c)))
    (_ #(1 2 3))
    '(1 2 3))

  (check-it
      (_ #(#(#(?a ?b ?c))))
    (syntax (quote (?a ?b ?c)))
    (_ #(#(#(1 2 3))))
    '(1 2 3))

;;; --------------------------------------------------------------------
;;; lists and ellipses

  (check-it
      (_ ?a ...)
    (syntax (quote (?a ...)))
    (_ 1 2 3)
    '(1 2 3))

  (check-it
      (_ ?a ?b ...)
    (syntax (quote (?a ?b ...)))
    (_ 1 2 3)
    '(1 2 3))

  (check-it
      (_ (?a ...) ...)
    (syntax (quote ((?a ...) ...)))
    (_ (1 2 3) (4 5 6) (7 8 9))
    '((1 2 3) (4 5 6) (7 8 9)))

  (check-it
      (_ (?a ?b ...) ...)
    (syntax (quote ((?a ?b ...) ...)))
    (_ (1 2 3) (4 5 6) (7 8 9))
    '((1 2 3) (4 5 6) (7 8 9)))

  (check-it
      (_ ?a ... ?b)
    (syntax (quote ((?a ...) ?b)))
    (_ 1 2 3)
    '((1 2) 3))

;;; --------------------------------------------------------------------
;;; vectors and ellipses

  (check-it
      (_ #(?a ...))
    (syntax (quote (?a ...)))
    (_ #(1 2 3))
    '(1 2 3))

  (check-it
      (_ #(?a ?b ...))
    (syntax (quote (?a ?b ...)))
    (_ #(1 2 3))
    '(1 2 3))

  (check-it
      (_ #(?a ...) ...)
    (syntax (quote ((?a ...) ...)))
    (_ #(1 2 3) #(4 5 6) #(7 8 9))
    '((1 2 3) (4 5 6) (7 8 9)))

  (check-it
      (_ #(?a ?b ...) ...)
    (syntax (quote ((?a ?b ...) ...)))
    (_ #(1 2 3) #(4 5 6) #(7 8 9))
    '((1 2 3) (4 5 6) (7 8 9)))

  (check-it
      (_ #(?a ... ?b))
    (syntax (quote ((?a ...) ?b)))
    (_ #(1 2 3))
    '((1 2) 3))

  (check-it
      (_ #(?a ... (?b ...)))
    (syntax (quote ((?a ...) ?b ...)))
    (_ #(1 2 (3 4)))
    '((1 2) 3 4))

  #t)


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

  (check
      (let ()
  	(define-values (a b c . rest)
  	  #t
  	  (values 1 2 3 4 5 6))
  	(list a b c rest))
    => '(1 2 3 (4 5 6)))

  (check
      (let ()
  	(define-values args
  	  #t
  	  (values 1 2 3))
  	args)
    => '(1 2 3))

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

  (check
      (let ()
  	(define-constant-values (a b c . rest)
  	  #t
  	  (values 1 2 3 4 5 6))
  	(list a b c rest))
    => '(1 2 3 (4 5 6)))

  (check
      (let ()
  	(define-constant-values args
  	  #t
  	  (values 1 2 3))
  	args)
    => '(1 2 3))
  #t)


(parametrise ((check-test-name	'define-constant-values))

  (define-syntax-rule (check-syntax-error ?form)
    (check
	(guard (E ((syntax-violation? E)
		   #t)
		  (else E))
	  (eval '?form
		(environment '(vicare))))
      => #t))

;;; --------------------------------------------------------------------

  (check
      (let-constants ()
	1)
    => 1)

  (check
      (let-constants ((a 1))
	a)
    => 1)

  (check
      (let-constants ((a 1)
		      (b 2)
		      (c 3))
	(values a b c))
    => 1 2 3)

  (check-syntax-error
   (let-constants ((a 1))
     (set! a 2)))

;;; --------------------------------------------------------------------

  (check
      (let*-constants ()
	1)
    => 1)

  (check
      (let*-constants ((a 1))
	a)
    => 1)

  (check
      (let*-constants ((a 1)
		       (b 2)
		       (c 3))
	(values a b c))
    => 1 2 3)

  (check
      (let*-constants ((a 1)
		       (b (cons a 2))
		       (c 3))
	(values a b c))
    => 1 '(1 . 2) 3)

  (check-syntax-error
   (let*-constants ((a 1))
     (set! a 2)))

;;; --------------------------------------------------------------------

  (check
      (letrec-constants ()
	1)
    => 1)

  (check
      (letrec-constants ((a 1))
	a)
    => 1)

  (check
      (letrec-constants ((a 1)
			 (b 2)
			 (c 3))
	(values a b c))
    => 1 2 3)

  (check
      (letrec-constants ((a 1)
			 (b (lambda () (cons a 2)))
			 (c 3))
	(values a (b) c))
    => 1 '(1 . 2) 3)

  (check
      (letrec-constants ((a (lambda () (b)))
			 (b (lambda () 1))
			 (c 3))
	(values (a) (b) c))
    => 1 1 3)

  (check-syntax-error
   (letrec-constants ((a 1))
     (set! a 2)))

;;; --------------------------------------------------------------------

  (check
      (letrec*-constants ()
	1)
    => 1)

  (check
      (letrec*-constants ((a 1))
	a)
    => 1)

  (check
      (letrec*-constants ((a 1)
			  (b 2)
			  (c 3))
	(values a b c))
    => 1 2 3)

  (check
      (letrec*-constants ((a 1)
			  (b (lambda () (cons a 2)))
			  (c 3))
	(values a (b) c))
    => 1 '(1 . 2) 3)

  (check-syntax-error
   (letrec*-constants ((a 1))
     (set! a 2)))

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


(parametrise ((check-test-name	'receive-and-return))

  (check
      (receive (a b c)
	  (receive-and-return (a b c)
	      (values 1 2 3)
	    (vector a b c))
	(list a b c))
    => '(1 2 3))

  (check
      (with-result
       (receive (a)
	   (receive-and-return (a)
	       1
	     (add-result a))
	 a))
    => '(1 (1)))

  (check
      (with-result
       (receive-and-return ()
	   (values)
	 (add-result 1))
       #t)
    => '(#t (1)))

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


(parametrise ((check-test-name	'define-inline-constant))

  (check
      (let ()
	(define-inline-constant a (+ 1 2 3))
	a)
    => 6)

  #t)


(parametrise ((check-test-name	'fluid-syntaxes))

  (check
      (with-result
       (let ()
	 (define-fluid-syntax ciao
	   (identifier-syntax "ciao"))
	 (add-result ciao)
	 (fluid-let-syntax ((ciao (identifier-syntax "hello")))
	   (add-result ciao))
	 (fluid-let-syntax ((ciao (identifier-syntax "ohayo")))
	   (add-result ciao))
	 ciao))
    => '("ciao" ("ciao" "hello" "ohayo")))

  (check
      (with-result
       (let ()
	 (define-fluid-syntax ciao
	   (identifier-syntax "ciao"))
	 (add-result ciao)
	 (fluid-let-syntax ((ciao (identifier-syntax "hello")))
	   (fluid-let-syntax ((ciao (identifier-syntax "ohayo")))
	     (add-result ciao))
	   (add-result ciao))
	 ciao))
    => '("ciao" ("ciao" "ohayo" "hello")))

  (check
      (with-result
       (let ()
	 (define-fluid-syntax ciao
	   (identifier-syntax "ciao"))
	 (add-result ciao)
	 (let ()
	   (define-fluid-override ciao
	     (identifier-syntax "hello"))
	   (add-result ciao))
	 ciao))
    => '("ciao" ("ciao" "hello")))

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


(parametrise ((check-test-name	'while))

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

  #t)


(parametrise ((check-test-name	'until))

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

  #t)


(parametrise ((check-test-name	'for))

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


(parametrise ((check-test-name	'do))

;;; standard do

  (check
      (with-result
       (do ((i 5 (+ -1 i)))
	   ((zero? i)
	    'done)
	 (add-result i)))
    => '(done (5 4 3 2 1)))

  (check	;binding with no step
      (with-result
       (do ((j 123)
	    (i 5 (+ -1 i)))
	   ((zero? i)
	    j)
	 (add-result i)))
    => '(123 (5 4 3 2 1)))

  (check	;break
      (with-result
       (do ((i 0 (+ 1 i)))
	   ((= i 5)
	    i)
	 (add-result i)
	 (when (= i 3)
	   (break 123))))
    => '(123 (0 1 2 3)))

  (check	;break
      (with-result
       (do ((i 0 (+ 1 i)))
	   ((= i 5)
	    i)
	 (add-result i)
	 (when (= i 3)
	   (break 123))))
    => '(123 (0 1 2 3)))

  (check	;continue
      (with-result
       (do ((i 0 (+ 1 i)))
	   ((= i 5)
	    i)
	 (when (= i 3)
	   (continue))
	 (add-result i)))
    => '(5 (0 1 2 4)))

;;; --------------------------------------------------------------------
;;; do ... while

  (check
      (with-result
       (define i 5)
       (do
	   (begin
	     (add-result i)
	     (set! i (+ -1 i)))
	   (while (positive? i))))
    => `(,(void) (5 4 3 2 1)))

  (check 	;continue
      (with-result
       (define i 5)
       (do
	   (begin
	     (set! i (+ -1 i))
	     (when (= i 3)
	       (continue))
	     (add-result i))
	   (while (positive? i))))
    => `(,(void) (4 2 1 0)))

  (check	;break
      (with-result
       (define i 5)
       (do
	   (begin
	     (set! i (+ -1 i))
	     (when (= i 2)
	       (break))
	     (add-result i))
	   (while (positive? i))))
    => '((4 3)))

;;; --------------------------------------------------------------------
;;; do ... until

  (check
      (with-result
       (define i 5)
       (do
	   (begin
	     (add-result i)
	     (set! i (+ -1 i)))
	   (until (zero? i))))
    => `(,(void) (5 4 3 2 1)))

  (check	;continue
      (with-result
       (define i 5)
       (do
	   (begin
	     (set! i (+ -1 i))
	     (when (= i 3)
	       (continue))
	     (add-result i))
	   (until (zero? i))))
    => `(,(void) (4 2 1 0)))

  (check	;break
      (with-result
       (define i 5)
       (do
	   (begin
	     (set! i (+ -1 i))
	     (when (= i 2)
	       (break))
	     (add-result i))
	   (until (zero? i))))
    => '((4 3)))

  #t)


(parametrise ((check-test-name	'named-let))

  (check
      (with-result
       (let loop ((i 0))
	 (if (= i 5)
	     i
	   (begin
	     (add-result i)
	     (loop (+ 1 i))))))
    => '(5 (0 1 2 3 4)))

  #t)


(parametrise ((check-test-name	'return))

  (define-syntax define-returnable
    (syntax-rules ()
      ((_ (?name . ?formals) ?body0 ?body ...)
       (define (?name . ?formals)
	 (call/cc
	     (lambda (escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (escape . ?args)))))
		 ?body0 ?body ...)))))
      ))

  (define-syntax lambda-returnable
    (syntax-rules ()
      ((_ ?formals ?body0 ?body ...)
       (lambda ?formals
	 (call/cc
	     (lambda (escape)
	       (fluid-let-syntax ((return (syntax-rules ()
					    ((_ . ?args)
					     (escape . ?args)))))
		 ?body0 ?body ...)))))
      ))

  (define-syntax begin-returnable
    (syntax-rules ()
      ((_ ?body0 ?body ...)
       (call/cc
	   (lambda (escape)
	     (fluid-let-syntax ((return (syntax-rules ()
					  ((_ . ?args)
					   (escape . ?args)))))
	       ?body0 ?body ...))))
      ))

;;; --------------------------------------------------------------------
;;; define-returnable

  (check	;no return, no arguments
      (with-result
       (let ()
	 (define-returnable (ciao)
	   (add-result 'in)
	   (add-result 'out)
	   1)
	 (ciao)))
    => '(1 (in out)))

  (check	;no return, arguments
      (with-result
       (let ()
	 (define-returnable (ciao a b)
	   (add-result 'in)
	   (add-result 'out)
	   (list a b))
	 (ciao 1 2)))
    => '((1 2) (in out)))

  (check	;return no values
      (with-result
       (let ()
	 (define-returnable (ciao)
	   (add-result 'in)
	   (return)
	   (add-result 'out)
	   1)
	 (ciao)
	 #t))
    => '(#t (in)))

  (check	;return single value
      (with-result
       (let ()
	 (define-returnable (ciao)
	   (add-result 'in)
	   (return 2)
	   (add-result 'out)
	   1)
	 (ciao)))
    => '(2 (in)))

  (check	;return multiple values
      (with-result
       (let ()
	 (define-returnable (ciao)
	   (add-result 'in)
	   (return 2 3 4)
	   (add-result 'out)
	   (values 1 2 3))
	 (receive (a b c)
	     (ciao)
	   (list a b c))))
    => '((2 3 4) (in)))

;;; --------------------------------------------------------------------
;;; lambda-returnable

  (check	;no return, no arguments
      (with-result
       (let ()
	 (define ciao
	   (lambda-returnable ()
	     (add-result 'in)
	     (add-result 'out)
	     1))
	 (ciao)))
    => '(1 (in out)))

  (check	;no return, arguments
      (with-result
       (let ()
	 (define ciao
	   (lambda-returnable (a b)
	     (add-result 'in)
	     (add-result 'out)
	     (list a b)))
	 (ciao 1 2)))
    => '((1 2) (in out)))

  (check	;return no values
      (with-result
       (let ()
	 (define ciao
	   (lambda-returnable ()
	     (add-result 'in)
	     (return)
	     (add-result 'out)
	     1))
	 (ciao)
	 #t))
    => '(#t (in)))

  (check	;return single value
      (with-result
       (let ()
	 (define ciao
	   (lambda-returnable ()
	     (add-result 'in)
	     (return 2)
	     (add-result 'out)
	     1))
	 (ciao)))
    => '(2 (in)))

  (check	;return multiple values
      (with-result
       (let ()
	 (define ciao
	   (lambda-returnable ()
	     (add-result 'in)
	     (return 2 3 4)
	     (add-result 'out)
	     (values 1 2 3)))
	 (receive (a b c)
	     (ciao)
	   (list a b c))))
    => '((2 3 4) (in)))

;;; --------------------------------------------------------------------
;;; begin-returnable

  (check	;no return, no arguments
      (with-result
       (begin-returnable
	(add-result 'in)
	(add-result 'out)
	1))
    => '(1 (in out)))

  (check	;no return, arguments
      (with-result
       (begin-returnable
	(add-result 'in)
	(add-result 'out)
	(list 1 2)))
    => '((1 2) (in out)))

  (check	;return no values
      (with-result
       (begin-returnable
	(add-result 'in)
	(return)
	(add-result 'out)
	1)
       #t)
    => '(#t (in)))

  (check	;return single value
      (with-result
       (begin-returnable
	(add-result 'in)
	(return 2)
	(add-result 'out)
	1))
    => '(2 (in)))

  (check	;return multiple values
      (with-result
       (receive (a b c)
	   (begin-returnable
	    (add-result 'in)
	    (return 2 3 4)
	    (add-result 'out)
	    (values 1 2 3))
	 (list a b c)))
    => '((2 3 4) (in)))

;;; --------------------------------------------------------------------
;;; returnable

  (check	;no return, no arguments
      (with-result
       (let ()
	 (define (ciao)
	   (returnable
	     (add-result 'in)
	     (add-result 'out)
	     1))
	 (ciao)))
    => '(1 (in out)))

  (check	;no return, arguments
      (with-result
       (let ()
	 (define (ciao a b)
	   (returnable
	     (add-result 'in)
	     (add-result 'out)
	     (list a b)))
	 (ciao 1 2)))
    => '((1 2) (in out)))

  (check	;return no values
      (with-result
       (let ()
	 (define (ciao)
	   (returnable
	     (add-result 'in)
	     (return)
	     (add-result 'out)
	     1))
	 (ciao)
	 #t))
    => '(#t (in)))

  (check	;return single value
      (with-result
       (let ()
	 (define (ciao)
	   (returnable
	     (add-result 'in)
	     (return 2)
	     (add-result 'out)
	     1))
	 (ciao)))
    => '(2 (in)))

  (check	;return multiple values
      (with-result
       (let ()
	 (define (ciao)
	   (returnable
	     (add-result 'in)
	     (return 2 3 4)
	     (add-result 'out)
	     (values 1 2 3)))
	 (receive (a b c)
	     (ciao)
	   (list a b c))))
    => '((2 3 4) (in)))

  #f)


(parametrise ((check-test-name	'define-auxiliary-syntaxes))

  (define-auxiliary-syntaxes)
  (define-auxiliary-syntaxes ciao)
  (define-auxiliary-syntaxes blu red)

  (define-syntax doit
    (syntax-rules (blu red)
      ((_ (blu ?blu) (red ?red))
       (list ?blu ?red))))

;;; --------------------------------------------------------------------

  (check
      (doit (blu 1) (red 2))
    => '(1 2))

  #t)


(parametrise ((check-test-name	'test-xor))

  (define-syntax xor
    (syntax-rules ()
      ((_ expr ...)
       (xor-aux #F expr ...))))

  (define-syntax xor-aux
    (syntax-rules ()
      ((_ r)
       r)
      ((_ r expr)
       (let ((x expr))
	 (if r
	     (and (not x) r)
	   x)))
      ((_ r expr0 expr ...)
       (let ((x expr0))
	 (and (or (not r) (not x))
	      (let ((n (or r x)))
		(xor-aux n expr ...)))))))

;;; --------------------------------------------------------------------

  (check (xor) => #f)
  (check (xor (number? 1)) => #T)
  (check (xor (null? 1)) => #f)
  (check (xor (string->symbol "foo")) => 'foo)
  (check (xor (string? "a") (symbol? 1)) => #T)
  (check (xor (string? 1) (symbol? 'a)) => #T)
  (check (xor (string? 1) (symbol? 2)) => #f)
  (check (xor (pair? '(a)) (list? '(b))) => #f)
  (check (xor (- 42) (not 42)) => -42)
  (check (xor (null? 1) (/ 42)) => 1/42)
  (check (xor (integer? 1.2) (positive? -2) (exact? 3)) => #T)
  (check (xor (integer? 1.2) (positive? 2) (exact? 3.4)) => #T)
  (check (xor (integer? 1) (positive? -2) (exact? 3.4)) => #T)
  (check (xor (integer? 1.2) (positive? -2) (exact? 3.4)) => #f)
  (check (xor (integer? 1.2) (positive? 2) (exact? 3)) => #f)
  (check (xor (integer? 1) (positive? -2) (exact? 3)) => #f)
  (check (xor (integer? 1) (positive? 2) (exact? 3.4)) => #f)
  (check (xor (integer? 1) (positive? 2) (exact? 3)) => #f)
  (check (xor "foo" (not 'foo) (eq? 'a 'b)) => "foo")
  (check (xor (not 'foo) (+ 1 2) (eq? 'a 'b)) => 3)
  (check (xor (not 'foo) (eq? 'a 'b) (- 1 2)) => -1)
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) #f)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #f)
		(begin (set! x (cons 'd x)) #f))
      => #f)
    (check x => '(d c b a)))
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) 'R)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #f)
		(begin (set! x (cons 'd x)) #f))
      => 'R)
    (check x => '(d c b a)))
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) #T)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #T)
		(begin (set! x (cons 'd x)) #f))
      => #f)
    (check x => '(c b a)))
  (let-syntax ((macro
		   (let ((count 0))
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_) (begin (set! count (+ 1 count)) #''foo))
			 ((_ _) count))))))
    (check (xor #f (macro) #f) => 'foo)
    (check (macro 'count) => 1))

  #t)


(parametrise ((check-test-name	'xor))

  (check (xor) => #f)
  (check (xor (number? 1)) => #T)
  (check (xor (null? 1)) => #f)
  (check (xor (string->symbol "foo")) => 'foo)
  (check (xor (string? "a") (symbol? 1)) => #T)
  (check (xor (string? 1) (symbol? 'a)) => #T)
  (check (xor (string? 1) (symbol? 2)) => #f)
  (check (xor (pair? '(a)) (list? '(b))) => #f)
  (check (xor (- 42) (not 42)) => -42)
  (check (xor (null? 1) (/ 42)) => 1/42)
  (check (xor (integer? 1.2) (positive? -2) (exact? 3)) => #T)
  (check (xor (integer? 1.2) (positive? 2) (exact? 3.4)) => #T)
  (check (xor (integer? 1) (positive? -2) (exact? 3.4)) => #T)
  (check (xor (integer? 1.2) (positive? -2) (exact? 3.4)) => #f)
  (check (xor (integer? 1.2) (positive? 2) (exact? 3)) => #f)
  (check (xor (integer? 1) (positive? -2) (exact? 3)) => #f)
  (check (xor (integer? 1) (positive? 2) (exact? 3.4)) => #f)
  (check (xor (integer? 1) (positive? 2) (exact? 3)) => #f)
  (check (xor "foo" (not 'foo) (eq? 'a 'b)) => "foo")
  (check (xor (not 'foo) (+ 1 2) (eq? 'a 'b)) => 3)
  (check (xor (not 'foo) (eq? 'a 'b) (- 1 2)) => -1)
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) #f)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #f)
		(begin (set! x (cons 'd x)) #f))
      => #f)
    (check x => '(d c b a)))
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) 'R)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #f)
		(begin (set! x (cons 'd x)) #f))
      => 'R)
    (check x => '(d c b a)))
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) #T)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #T)
		(begin (set! x (cons 'd x)) #f))
      => #f)
    (check x => '(c b a)))
  (let-syntax ((macro
		   (let ((count 0))
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_) (begin (set! count (+ 1 count)) #''foo))
			 ((_ _) count))))))
    (check (xor #f (macro) #f) => 'foo)
    (check (macro 'count) => 1))

  #t)


(parametrise ((check-test-name	'extended-define-syntax))

  (define-syntax (doit stx)
    (syntax-case stx ()
      ((_ a b)
       #'(list a b))))

  (check
      (doit 1 2)
    => '(1 2))

  #t)


(parametrise ((check-test-name	'endianness))

  (check (endianness little)		=> 'little)
  (check (endianness big)		=> 'big)
  (check (endianness network)		=> 'big)
  (check (endianness native)		=> (native-endianness))

  #t)


(parametrise ((check-test-name	'with-implicits))

  (check
      (let-syntax ((doit (lambda (stx)
			   (syntax-case stx ()
			     ((?id)
			      (identifier? #'?id)
			      (with-implicits ((#'?id x y z)
					       (#'?id p q r))
				#'(list x y z p q r)))))))

	(let ((x 1) (y 2) (z 3)
	      (p 10) (q 20) (r 30))
	  (doit)))
    => '(1 2 3 10 20 30))

  (check
      (let-syntax ((doit (lambda (stx)
			   (syntax-case stx ()
			     ((?ctx)
			      (with-implicits ()
				123))))))
	(doit))
    => 123)

  #t)


(parametrise ((check-test-name	'define-syntax-star))

;;;auxiliary syntax definition

  (check
      (let ()
	(define-syntax* ciao)
	(define-syntax (doit stx)
	  (syntax-case stx ()
	    ((_ ?id)
	     (free-identifier=? #'?id #'ciao))))
	(doit ciao))
    => #t)

  (check
      (let ()
	(define-syntax* ciao)
	(define-syntax (doit stx)
	  (syntax-case stx ()
	    ((_ ?id)
	     (free-identifier=? #'?id #'ciao))))
	(doit hello))
    => #f)

;;; --------------------------------------------------------------------
;;;common syntax definition

  (check
      (let ()
	(define-syntax* ciao)
	(define-syntax* doit
	  (lambda (stx)
	    (syntax-case stx ()
	      ((_ ?id)
	       (free-identifier=? #'?id #'ciao)))))
	(doit hello))
    => #f)

;;; --------------------------------------------------------------------
;;; special syntax definitions

  (check
      (let ()
	(define-syntax* ciao)
	(define-syntax* (doit stx)
	  (syntax-case stx ()
	    ((_ ?id)
	     (free-identifier=? #'?id #'ciao))))
	(doit hello))
    => #f)

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-who E)
		       (syntax->datum (syntax-violation-form E))
		       (syntax->datum (syntax-violation-subform E))))
		(else E))
	(eval '(let ()
		 (define-syntax* (doit stx)
		   (syntax-case stx ()
		     ((_ ?id)
		      (synner "bad syntax"))))
		 (doit hello))
	      (environment '(vicare))))
    => '(doit (doit hello) #f))

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-who E)
		       (syntax->datum (syntax-violation-form E))
		       (syntax->datum (syntax-violation-subform E))))
		(else E))
	(eval '(let ()
		 (define-syntax* (doit stx)
		   (syntax-case stx ()
		     ((_ ?id)
		      (synner "bad syntax" #'?id))))
		 (doit hello))
	      (environment '(vicare))))
    => '(doit (doit hello) hello))

  (check	;redefinition  of __WHO__,  SYNNER  still  bound to  the
		;automatically generated one
      (guard (E ((syntax-violation? E)
		 (list (condition-who E)
		       (syntax->datum (syntax-violation-form E))
		       (syntax->datum (syntax-violation-subform E))))
		(else E))
	(eval '(let ()
		 (define-syntax* (doit stx)
		   (define __who__ 'other)
		   (syntax-case stx ()
		     ((_ ?id)
		      (synner "bad syntax" #'?id))))
		 (doit hello))
	      (environment '(vicare))))
    => '(doit (doit hello) hello))

  (check	;redefinition of WHO and SYNNER
      (guard (E ((syntax-violation? E)
		 (list (condition-who E)
		       (syntax->datum (syntax-violation-form E))
		       (syntax->datum (syntax-violation-subform E))))
		(else E))
	(eval '(let ()
		 (define-syntax* (doit stx)
		   (define __who__ 'other)
		   (define (synner message subform)
		     (syntax-violation __who__ message subform #f))
		   (syntax-case stx ()
		     ((_ ?id)
		      (synner "bad syntax" #'?id))))
		 (doit hello))
	      (environment '(vicare))))
    => '(other hello #f))

  #t)


(parametrise ((check-test-name	'define-alias))

  (check
      (let ((a 1))
	(define-alias b a)
	b)
    => 1)

  (check
      (let ((a 1))
	(define-alias b a)
	(define-alias c b)
	(define-alias d c)
	(define-alias e d)
	e)
    => 1)

  (check
      (let ((a 1))
	(define-alias b a)
	(+ a b))
    => 2)

  (check
      (let ()
	(define a 1)
	(define-alias b a)
	b)
    => 1)

  (check
      (let ()
	(define-syntax a
	  (identifier-syntax 1))
	(define-alias b a)
	b)
    => 1)

  (check
      (let ()
	(define-syntax (a stx)
	  1)
	(define-alias b a)
	(b))
    => 1)

;;; --------------------------------------------------------------------
;;; free-identifier=?

  (check-for-true
   (let ()
     (define a 1)
     (define-alias b a)
     (define-syntax (doit stx)
       (syntax-case stx ()
	 ((_ ?id1 ?id2)
	  (free-identifier=? #'?id1 #'?id2))))
     ;; (eval-for-expand
     ;;   (debug-print 'equal (free-identifier=? #'a #'b)))
     (doit a b)))

  (check-for-true
   (let ()
     (define a 1)
     (define-alias b a)
     (define-alias c b)
     (define-syntax (doit stx)
       (syntax-case stx ()
	 ((_ ?id1 ?id2)
	  (free-identifier=? #'?id1 #'?id2))))
     (doit b c)))

  #t)


(parametrise ((check-test-name	'let-star-syntax))

  (check
      (let*-syntax () 1 2)
    => 2)

  (check
      (let*-syntax ((id (identifier-syntax 123)))
	1 id)
    => 123)

  (check
      (let*-syntax ((id1 (identifier-syntax 123))
		    (id2 (identifier-syntax id1)))
	(list id1 id2))
    => '(123 123))

  (check
      (let*-syntax ((id1 (identifier-syntax 123))
		    (id2 (identifier-syntax (+ 9000 id1))))
	(list id1 id2))
    => '(123 9123))

  (check
      (let*-syntax ((id (identifier-syntax 123))
		    (id (identifier-syntax id)))
	1 id)
    => 123)

  #t)


(parametrise ((check-test-name	'values-to-list))

  (check
      (values->list 123)
    => '(123))

  (check
      (values->list (values 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'case-define))

  (check
      (let ()
	(case-define doit
	  (()
	   123))
	(doit))
    => 123)

  (check
      (let ()
	(case-define doit
	  ((a)
	   (list a))
	  ((a b)
	   (vector a b)))
	(list (doit 1) (doit 1 2)))
    => '((1) #(1 2)))

  #t)


(parametrise ((check-test-name	'lambda-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define doit
	  (lambda* ()
	    123))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (lambda* (a)
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (lambda* (a b c)
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (lambda* rest
	    (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (lambda* (a . rest)
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (lambda* (a b . rest)
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (lambda* (a b c . rest)
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?})
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (lambda* {args list-of-fixnums?}
	    (vector 123 args)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?} . {rest list-of-fixnums?})
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} . {rest list-of-fixnums?})
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} {c fixnum?} . {rest list-of-fixnums?})
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(_ ((fixnum? b) #\5)))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(define doit
	  (lambda* ({_ fixnum?})
	    123))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?})
	    (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} . {rest list-of-fixnums?})
	    (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?} . {rest list-of-fixnums?})
	    (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?} {b fixnum?} . {rest list-of-fixnums?})
	    (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (lambda* ({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest list-of-fixnums?})
	    (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define doit
	  (lambda* ({a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(_ ((fixnum? b) #\5)))

  (check-for-expression-return-value-violation
      (let ()
	(define doit
	  (lambda* ({_ list?} {a fixnum?} {b fixnum?} {c fixnum?})
	    (vector 123 a b c)))
	(doit 4 5 6))
    => '(_ ((list? #(123 4 5 6)))))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(define doit
	  (lambda* ({_ fixnum? string? char?})
	    (values 1 "2" #\3)))
	(doit))
    => 1 "2" #\3)

  (check-for-expression-return-value-violation
      (let ()
	(define doit
	  (lambda* ({_ fixnum? string? char?})
	    (values 1 'a #\3)))
	(doit))
    => '(_ ((string? a))))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define doit
	  (lambda* ()
	    __who__))
	(doit))
    => '_)

  (check
      (let ()
	(define doit
	  (lambda* ({_ symbol?})
	    __who__))
	(doit))
    => '_)

  #t)


(parametrise ((check-test-name	'case-lambda-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (()
	     123)))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a)
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a b c)
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (rest
	     (vector 123 rest))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a . rest)
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a b . rest)
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ((a b c . rest)
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?})
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    ({args list-of-fixnums?}
	     (vector 123 args))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} . {rest list-of-fixnums?})
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} . {rest list-of-fixnums?})
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} {c fixnum?} . {rest list-of-fixnums?})
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 #\5 6))
    => '(_ ((fixnum? b) #\5)))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ fixnum?})
	     123)))
	(doit))
    => 123)

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?})
	     (vector 123 a))))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} . {rest list-of-fixnums?})
	     (vector 123 rest))))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?} . {rest list-of-fixnums?})
	     (vector 123 a rest))))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?} {b fixnum?} . {rest list-of-fixnums?})
	     (vector 123 a b rest))))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest list-of-fixnums?})
	     (vector 123 a b c rest))))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define doit
	  (case-lambda*
	    (({a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 #\5 6))
    => '(_ ((fixnum? b) #\5)))

  (check-for-expression-return-value-violation
      (let ()
	(define doit
	  (case-lambda*
	    (({_ list?} {a fixnum?} {b fixnum?} {c fixnum?})
	     (vector 123 a b c))))
	(doit 4 5 6))
    => '(_ ((list? #(123 4 5 6)))))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ fixnum? string? char?})
	     (values 1 "2" #\3))))
	(doit))
    => 1 "2" #\3)

  (check-for-expression-return-value-violation
      (let ()
	(define doit
	  (case-lambda*
	    (({_ fixnum? string? char?})
	     (values 1 'a #\3))))
	(doit))
    => '(_ ((string? a))))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (()
	     __who__)))
	(doit))
    => '_)

  (check
      (let ()
	(define doit
	  (case-lambda*
	    (({_ symbol?})
	     __who__)))
	(doit))
    => '_)

  #t)


(parametrise ((check-test-name	'define-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(define* (doit)
	  123)
	(doit))
    => 123)

  (check
      (let ()
	(define* (doit a)
	  (vector 123 a))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define* (doit a b c)
	  (vector 123 a b c))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define* (doit . rest)
	  (vector 123 rest))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define* (doit a . rest)
	  (vector 123 a rest))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define* (doit a b . rest)
	  (vector 123 a b rest))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define* (doit a b c . rest)
	  (vector 123 a b c rest))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(define* (doit {a fixnum?})
	  (vector 123 a))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define* (doit {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define* (doit . {rest list-of-fixnums?})
	  (vector 123 rest))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define* (doit {a fixnum?} . {rest list-of-fixnums?})
	  (vector 123 a rest))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define* (doit {a fixnum?} {b fixnum?} . {rest list-of-fixnums?})
	  (vector 123 a b rest))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define* (doit {a fixnum?} {b fixnum?} {c fixnum?} . {rest list-of-fixnums?})
	  (vector 123 a b c rest))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define* (doit {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 #\5 6))
    => '(doit ((fixnum? b) #\5)))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(define* ({doit fixnum?})
	  123)
	(doit))
    => 123)

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?})
	  (vector 123 a))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(define* ({doit vector?} . {rest list-of-fixnums?})
	  (vector 123 rest))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?} . {rest list-of-fixnums?})
	  (vector 123 a rest))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?} {b fixnum?} . {rest list-of-fixnums?})
	  (vector 123 a b rest))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(define* ({doit vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest list-of-fixnums?})
	  (vector 123 a b c rest))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(define* ({doit vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 #\5 6))
    => '(doit ((fixnum? b) #\5)))

  (check-for-expression-return-value-violation
      (let ()
	(define* ({doit list?} {a fixnum?} {b fixnum?} {c fixnum?})
	  (vector 123 a b c))
	(doit 4 5 6))
    => '(doit ((list? #(123 4 5 6)))))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(define* ({doit fixnum? string? char?})
	  (values 1 "2" #\3))
	(doit))
    => 1 "2" #\3)

  (check-for-expression-return-value-violation
      (let ()
	(define* ({doit fixnum? string? char?})
	  (values 1 'a #\3))
	(doit))
    => '(doit ((string? a))))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(define* (doit)
	  __who__)
	(doit))
    => 'doit)

  (check
      (let ()
	(define* ({doit symbol?})
	  __who__)
	(doit))
    => 'doit)

  #t)


(parametrise ((check-test-name	'case-define-star))

  (define (list-of-fixnums? obj)
    (and (list? obj)
	 (for-all fixnum? obj)))

;;; --------------------------------------------------------------------
;;; without predicates

  (check
      (let ()
	(case-define* doit
	  (()
	   123))
	(doit))
    => 123)

  (check
      (let ()
	(case-define* doit
	  ((a)
	   (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(case-define* doit
	  ((a b c)
	   (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(case-define* doit
	  (rest
	   (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(case-define* doit
	  ((a . rest)
	   (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(case-define* doit
	  ((a b . rest)
	   (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(case-define* doit
	  ((a b c . rest)
	   (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check
      (let ()
	(case-define* doit
	  (()		0)
	  ((a)		1)
	  ((a b c)	3))
	(list (doit)
	      (doit 1)
	      (doit 1 2 3)))
    => '(0 1 3))

  (check
      (let ()
	(case-define* doit
	  (()		0)
	  ((a)		(list a))
	  ((a b c)	(list a b c)))
	(vector (doit)
		(doit 1)
		(doit 1 2 3)))
    => '#(0 (1) (1 2 3)))

  (check
      (let ()
	(case-define* doit
	  (()			0)
	  ((a)			(list a))
	  ((a b c)		(list a b c))
	  ((a b c . rest)	(cons* a b c rest)))
	(vector (doit)
		(doit 1)
		(doit 1 2 3)
		(doit 1 2 3 4 5)))
    => '#(0 (1) (1 2 3) (1 2 3 4 5)))

;;; --------------------------------------------------------------------
;;; with arg predicates, list spec, without retval predicate

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?})
	   (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(case-define* doit
	  ({rest list-of-fixnums?}
	   (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?} . {rest list-of-fixnums?})
	   (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} . {rest list-of-fixnums?})
	   (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} {c fixnum?} . {rest list-of-fixnums?})
	   (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check
      (let ()
	(case-define* doit
	  (()
	   0)
	  (({a fixnum?})
	   (list a))
	  (({a fixnum?} {b fixnum?} {c fixnum?})
	   (list a b c))
	  (({a fixnum?} {b fixnum?} {c fixnum?} . rest)
	   (cons* a b c rest)))
	(vector (doit)
		(doit 1)
		(doit 1 2 3)
		(doit 1 2 3 4 5)))
    => '#(0 (1) (1 2 3) (1 2 3 4 5)))

  (check-for-procedure-argument-violation
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(doit ((fixnum? b) #\5)))

;;; --------------------------------------------------------------------
;;; with arg predicates, with retval predicate

  (check
      (let ()
	(case-define* doit
	  (({_ fixnum?})
	   123))
	(doit))
    => 123)

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?})
	   (vector 123 a)))
	(doit 456))
    => '#(123 456))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 5 6))
    => '#(123 4 5 6))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} . {rest list-of-fixnums?})
	   (vector 123 rest)))
	(doit 4 5 6))
    => '#(123 (4 5 6)))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?} . {rest list-of-fixnums?})
	   (vector 123 a rest)))
	(doit 4 5 6))
    => '#(123 4 (5 6)))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?} {b fixnum?} . {rest list-of-fixnums?})
	   (vector 123 a b rest)))
	(doit 4 5 6))
    => '#(123 4 5 (6)))

  (check
      (let ()
	(case-define* doit
	  (({_ vector?} {a fixnum?} {b fixnum?} {c fixnum?} . {rest list-of-fixnums?})
	   (vector 123 a b c rest)))
	(doit 4 5 6))
    => '#(123 4 5 6 ()))

  (check-for-procedure-argument-violation
      (let ()
	(case-define* doit
	  (({a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 #\5 6))
    => '(doit ((fixnum? b) #\5)))

  (check-for-expression-return-value-violation
      (let ()
	(case-define* doit
	  (({_ list?} {a fixnum?} {b fixnum?} {c fixnum?})
	   (vector 123 a b c)))
	(doit 4 5 6))
    => '(doit ((list? #(123 4 5 6)))))

;;; --------------------------------------------------------------------
;;; multiple retval predicates

  (check
      (let ()
	(case-define* doit
	  (({_ fixnum? string? char?})
	   (values 1 "2" #\3)))
	(doit))
    => 1 "2" #\3)

  (check-for-expression-return-value-violation
      (let ()
	(case-define* doit
	  (({_ fixnum? string? char?})
	   (values 1 'a #\3)))
	(doit))
    => '(doit ((string? a))))

;;; --------------------------------------------------------------------
;;; non-hygienic bindings

  (check
      (let ()
	(case-define* doit
	  (()
	   __who__))
	(doit))
    => 'doit)

  (check
      (let ()
	(case-define* doit
	  (({_ symbol?})
	   __who__))
	(doit))
    => 'doit)

  #t)


(parametrise ((check-test-name	'blocking-exceptions))

  (check
      (with-blocked-exceptions
	  (lambda (E)
	    (values E 1 2 3))
	(lambda ()
	  (raise 99)))
    => 99 1 2 3)

  (check	;exceptions fromt he handler are not blocked
      (guard (E (else
		 E))
	(with-blocked-exceptions
	    (lambda (E)
	      (raise (list E 1 2 3)))
	  (lambda ()
	    (raise 99))))
    => '(99 1 2 3))

  #t)


(parametrise ((check-test-name	'current-dynamic-environment))

  (define parm
    (make-parameter #f))

;;; --------------------------------------------------------------------

  (check
      (with-result
	(parametrise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parametrise ((parm 'inner))
			    (with-current-dynamic-environment
				values
			      (lambda ()
				(set! counter (+ 1 counter))
				(add-result (list 'inside-thunk (parm))))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner)
	    calling-thunk-2 (inside-thunk inner))))

  (check	;raising exception
      (with-result
	(parametrise ((parm 'outer))
	  (let* ((counter 0)
		 (thunk   (parametrise ((parm 'inner))
			    (with-current-dynamic-environment
				values
			      (lambda ()
				(set! counter (+ 1 counter))
				(add-result (list 'inside-thunk (parm)))
				(add-result 'raise-exception)
				(raise 123))))))
	    (add-result (parm))
	    (add-result 'calling-thunk-1)
	    (thunk)
	    (add-result 'calling-thunk-2)
	    (thunk)
	    counter)))
    => '(2 (outer
	    calling-thunk-1 (inside-thunk inner) raise-exception
	    calling-thunk-2 (inside-thunk inner) raise-exception)))

  #t)


(parametrise ((check-test-name	'try))

  (let ()	;with else clause
    (define-condition-type &this
	&error
      make-this-condition
      condition-this?
      (a condition-this.a)
      (b condition-this.b)
      (c condition-this.c))

    (define (doit thunk)
      (try
	  (thunk)
	(catch E
	  ((&this)
	   (list (condition-this.a E)
		 (condition-this.b E)
		 (condition-this.c E)))
	  ((&message)
	   (condition-message E))
	  (else E))))

    (check
	(doit (lambda ()
		(raise (make-this-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 123)

    (check
	(try
	    (raise 123)
	  (catch E
	    ((&this)
	     (list (condition-this.a E)
		   (condition-this.b E)
		   (condition-this.c E)))
	    ((&message)
	     (condition-message E))
	    (else E)))
      => 123)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;with else clause
    (define-condition-type &that
	&error
      make-that-condition
      condition-that?
      (a condition-that.a)
      (b condition-that.b)
      (c condition-that.c))

    (define (doit thunk)
      (try
	  (thunk)
	(catch T
	  ((&that)
	   (list (condition-that.a T)
		 (condition-that.b T)
		 (condition-that.c T)))
	  ((&message)
	   (condition-message T))
	  (else T))))

    (check
	(doit (lambda ()
		(raise (make-that-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 123)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;without else clause
    (define-condition-type &those
	&error
      make-those-condition
      condition-those?
      (a condition-those.a)
      (b condition-those.b)
      (c condition-those.c))

    (define (doit thunk)
      (guard (E (else
		 (values 'reraised E)))
	(try
	    (thunk)
	  (catch E
	    ((&those)
	     (list (condition-those.a E)
		   (condition-those.b E)
		   (condition-those.c E)))
	    ((&message)
	     (condition-message E))))))

    (check
	(doit (lambda ()
		(raise (make-those-condition 1 2 3))))
      => '(1 2 3))

    (check
	(doit (lambda ()
		(raise (make-message-condition "ciao"))))
      => "ciao")

    (check
	(doit (lambda ()
		(raise 123)))
      => 'reraised 123)

    #f)

;;; --------------------------------------------------------------------
;;; finally

  (check	;no exception
      (let ((a 1))
	(try
	    (set! a (+ a 10))
	  (catch E
	    ((&error)	E)
	    ((&warning)	E)
	    (else	E))
	  (finally
	   (set! a (+ a 100))))
	a)
    => 111)

  (check	;no exception
      (with-result
	(try
	    (add-result 'body)
	  (catch E
	    ((&error)	(add-result 'catch-error))
	    ((&warning)	(add-result 'catch-warning))
	    (else	(add-result 'catch-else)))
	  (finally
	   (add-result 'finally))))
    => '(body (body finally)))

  (check	;with exception
      (let ((a 1))
	(try
	    (raise (make-warning))
	  (catch E
	    ((&error)	E)
	    ((&warning)	(set! a (+ a 10)))
	    (else	E))
	  (finally
	   (set! a (+ a 100))))
	a)
    => 111)

  (check	;with exception
      (with-result
	(try
	    (begin
	      (add-result 'body)
	      (raise (make-warning)))
	  (catch E
	    ((&error)	(add-result 'catch-error))
	    ((&warning)	(add-result 'catch-warning))
	    (else	(add-result 'catch-else)))
	  (finally
	   (add-result 'finally))))
    => '(catch-warning (body catch-warning finally)))

  #t)


(parametrise ((check-test-name	'case-with-arrow))

  (check	;no arrow
      (case 2
	((a b c)	'symbol)
	((1 2 3)	'fixnum)
	(else		'else))
    => 'fixnum)

  (check	;no arrow
      (case 'c
	((a b c)	'symbol)
	((1 2 3)	'fixnum)
	(else		'else))
    => 'symbol)

  (check	;no arrow
      (case "c"
	((a b c)	'symbol)
	((1 2 3)	'fixnum)
	(else		'else))
    => 'else)

  (check	;no arrow, multiple values
      (case 2
	((a b c)	'symbol)
	((1 2 3)	(values 7 8 9))
	(else		'else))
    => 7 8 9)

;;; --------------------------------------------------------------------

  (check	;with arrow
      (case 2
	((a b c)	'symbol)
	((1 2 3)	=> (lambda (N) (vector N)))
	(else		'else))
    => '#(2))

  (check	;with arrow
      (case 'a
	((a b c)	=> (lambda (N) (list N)))
	((1 2 3)	=> (lambda (N) (vector N)))
	(else		'else))
    => '(a))

  (check	;with arrow multiple values
      (case 2
	((a b c)	'symbol)
	((1 2 3)	=> (lambda (N) (values N N N)))
	(else		'else))
    => 2 2 2)

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid arrow in ELSE clause
      (guard (E ((syntax-violation? E)
		 (condition-message E))
		(else E))
	(eval '(case 2
		 ((a b c)	'symbol)
		 ((1 2 3)	=> (lambda (N) (vector N)))
		 (else		=> 'else))
	      (environment '(vicare))))
    => "incorrect usage of auxiliary keyword")

  (check	;receiver form does not evaluate to function
      (catch-expand-time-signature-violation #f
	(%eval '(case 2
		  ((a b c)	'symbol)
		  ((1 2 3)	=> 123)
		  (else		'else))))
    => '(<procedure>) '(<fixnum>))

  #t)


(parametrise ((check-test-name	'case-identifiers))

  (check	;no arrow
      (case-identifiers #'two
	((a b c)		'symbol)
	((one two three)	'fixnum)
	(else			'else))
    => 'fixnum)

  (check	;no arrow
      (case-identifiers #'c
  	((a b c)		'symbol)
  	((one two three)	'fixnum)
  	(else			'else))
    => 'symbol)

  (check	;no arrow
      (case-identifiers #'other
  	((a b c)		'symbol)
  	((one two three)	'fixnum)
  	(else			'else))
    => 'else)

  (check	;no arrow, multiple values
      (case-identifiers #'two
  	((a b c)		'symbol)
  	((one two three)	(values 7 8 9))
  	(else			'else))
    => 7 8 9)

  (check	;expr is not an identifier
      (case-identifiers 123
  	((a b c)		'symbol)
  	((one two three)	(values 7 8 9))
  	(else			'else))
    => 'else)

;;; --------------------------------------------------------------------

  (check	;with arrow
      (case-identifiers #'two
  	((a b c)		'symbol)
  	((one two three)	=> (lambda (N) (vector N)))
  	(else			'else))
    (=> syntax=?)
    (vector #'two))

  (check	;with arrow
      (case-identifiers #'a
  	((a b c)		=> (lambda (N) (list N)))
  	((one two three)	=> (lambda (N) (vector N)))
  	(else			'else))
    (=> syntax=?)
    (list #'a))

  (check	;with arrow multiple values
      (case-identifiers #'two
  	((a b c)		'symbol)
  	((one two three)	=> (lambda (N) (values N N N)))
  	(else			'else))
    (=> syntax=?)
    #'two #'two #'two)

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid arrow in ELSE clause
      (guard (E ((syntax-violation? E)
  		 (condition-message E))
  		(else E))
  	(eval '(case-identifiers #'two
  		 ((a b c)		'symbol)
  		 ((one two three)	=> (lambda (N) (vector N)))
  		 (else			=> 'else))
  	      (environment '(vicare))))
    => "incorrect usage of auxiliary keyword")

  (check	;receiver form does not evaluate to function
      (catch-expand-time-signature-violation #f
	(%eval '(case-identifiers #'two
		  ((a b c)		'symbol)
		  ((one two three)	=> 'one-two-three)
		  (else			'else))))
    => '(<procedure>) '(<symbol>))

  (check	;datum is not an identifier
      (guard (E ((syntax-violation? E)
  		 (condition-message E))
  		(else E))
  	(eval '(case-identifiers #'two
  		 ((a b c)		'symbol)
  		 ((one 123 three)	=> (lambda (N) (vector N)))
  		 (else			=> 'else))
  	      (environment '(vicare))))
    => "expected identifiers as datums")

  #t)


(parametrise ((check-test-name	'splice-first-expand))

  (check
      (splice-first-expand 123)
    => 123)

  (check
      (splice-first-expand (+ 1 2))
    => 3)

  (check
      (list 8 (splice-first-expand (+ 1 2)) 9)
    => '(8 3 9))

  (check
      (with-result
       (begin
  	 (add-result 1)
  	 (splice-first-expand (add-result 2))
  	 (add-result 3)
  	 4))
    => '(4 (1 2 3)))

  (check
      (if (splice-first-expand 1)
  	  (splice-first-expand 2)
  	(splice-first-expand 3))
    => 2)

  (check
      (if (splice-first-expand #f)
  	  (splice-first-expand 2)
  	(splice-first-expand 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      ((splice-first-expand (+)) 1 2)
    => 3)

  (check
      ((splice-first-expand (+ 1 2)) 3 4)
    => (+ 1 2 3 4))

  (check
      (with-result
       ((splice-first-expand (begin (add-result 1) (add-result 2)))
  	(add-result 3) 4))
    => '(4 (1 2 3)))

  (check
      (let-syntax ((doit (syntax-rules ()
  			   ((_ ?arg ...)
  			    (+ (square ?arg) ...)))))
  	((splice-first-expand (doit 1 2)) 3 4))
    => (+ (square 1) (square 2) (square 3) (square 4)))

  (check
      (let-syntax ((arg1 (identifier-syntax 1))
  		   (arg2 (identifier-syntax 2))
  		   (doit (syntax-rules ()
  			   ((_ ?arg ...)
  			    (+ (square ?arg) ...)))))
  	((splice-first-expand (doit arg1 arg2)) 3 4))
    => (+ (square 1) (square 2) (square 3) (square 4)))

  (check
      (let-syntax ((doit (syntax-rules ()
  			   ((_)
  			    (splice-first-expand 123)))))
  	(doit))
    => 123)

  (check
      (let-syntax ((doit (syntax-rules ()
  			   ((_)
  			    (splice-first-expand (+ 1 2))))))
  	(doit))
    => 3)

  (check
      (let-syntax ((doit (identifier-syntax
			  (splice-first-expand (+ 1 2)))))
  	doit)
    => 3)

  (check
      (let-syntax ((doit (syntax-rules ()
  			   ((_)
  			    (splice-first-expand (+ 1 2))))))
  	((doit) 10))
    => 13)

  (check
      (let-syntax ((doit (identifier-syntax
			  (splice-first-expand (+ 1 2)))))
  	(doit 10))
    => 13)

  (check
      (let*-syntax ((arg1 (identifier-syntax 1))
  		    (arg2 (identifier-syntax 2))
  		    (doit (syntax-rules ()
  			    ((_ ?arg ...)
  			     (+ (square ?arg) ...))))
  		    (flop (syntax-rules ()
  			    ((_ ?arg ...)
  			     (splice-first-expand
  			      (doit arg1 ?arg ...))))))
  	((flop arg2) 3 4))
    => (+ (square 1) (square 2) (square 3) (square 4)))

  (check
      (let*-syntax ((arg1 (identifier-syntax 1))
  		    (arg2 (identifier-syntax 2))
  		    (doit (syntax-rules ()
  			    ((_ ?arg ...)
  			     (+ (square ?arg) ...))))
  		    (flop (syntax-rules ()
  			    ((_ ?arg ...)
  			     (splice-first-expand
  			      (doit arg1 ?arg ...)))))
		    (flip (identifier-syntax
			   (flop arg2))))
  	(flip 3 4))
    => (+ (square 1) (square 2) (square 3) (square 4)))

  (check
      (let*-syntax ((arg1 (identifier-syntax 1))
  		    (arg2 (identifier-syntax 2))
  		    (doit (syntax-rules ()
  			    ((_ ?arg ...)
  			     (+ (square ?arg) ...))))
  		    (flop (syntax-rules ()
  			    ((_ ?arg ...)
  			     (splice-first-expand
  			      (doit arg1 ?arg ...)))))
  		    (flip (syntax-rules ()
  			    ((_ ?arg ...)
  			     (flop ?arg ...)))))
  	((flip arg2) 3 4))
    => (+ (square 1) (square 2) (square 3) (square 4)))

  (check	;nested intermixed splicing, 2 times
      (let*-syntax ((one	(syntax-rules ()
				  ((_ ?arg ...)
				   (+ (square ?arg) ...))))
  		    (two	(syntax-rules ()
				  ((_ ?arg ...)
				   (splice-first-expand
				    (one 1 ?arg ...)))))
  		    (three	(syntax-rules ()
				  ((_ ?arg ...)
				   (two 2 ?arg ...))))
  		    (four	(syntax-rules ()
				  ((_ ?arg ...)
				   (splice-first-expand
				    (three 3 ?arg ...))))))
  	((four 4) 5))
    => (+ (square 1) (square 2) (square 3) (square 4) (square 5)))

;;; --------------------------------------------------------------------
;;; syntaxes expanding into each other

  (check
      (let ()
	(define (the-alpha x)   (* 10 x))
	(define (the-beta  x y) (+  x y))
	(define (the-gamma x y) (/  x y))

	(define-syntax gamma
	  (syntax-rules ()
	    ((_ ?arg ...)
	     (the-gamma ?arg ...))))

	(define-syntax beta
	  (syntax-rules ()
	    ((_ ?expr)
	     ?expr)
	    ((_ ?expr ?arg ...)
	     (splice-first-expand (gamma (the-beta ?expr ?arg ...) 3)))))

	(define-syntax alpha
	  (syntax-rules ()
	    ((_ ?arg)
	     (splice-first-expand (beta (the-alpha ?arg) 3)))))

	(gamma 23 3)
	(beta  20 3)
	(alpha 2))
    => 23/3)

  (check
      (let ()
	(define (the-alpha x)   (* 10 x))
	(define (the-beta  x y) (+  x y))
	(define (the-gamma x y) (/  x y))

	(define-syntax gamma
	  (syntax-rules ()
	    ((_ #:splice ?expr)
	     (quote ?expr))
	    ((_ #:splice ?expr ?arg ...)
	     (gamma #:doit ?expr ?arg ...))
	    ((_ #:doit ?expr ?arg ...)
	     (the-gamma ?expr ?arg ...))
	    ))

	(define-syntax beta
	  (syntax-rules ()
	    ((_ #:splice ?expr)
	     (quote ?expr))
	    ((_ #:splice ?expr ?arg ...)
	     (beta #:doit ?expr ?arg ...))
	    ((_ #:doit ?expr ?arg ...)
	     (splice-first-expand (gamma #:splice (the-beta ?expr ?arg ...) 3)))
	    ))

	(define-syntax alpha
	  (syntax-rules ()
	    ((_ ?arg)
	     (splice-first-expand (beta #:splice (the-alpha ?arg) 3)))))

	(gamma #:splice 23 3)
	(beta  #:splice 20 3)
	(alpha 2))
    => 23/3)

;;; --------------------------------------------------------------------
;;; errors

  (check
      (guard (E ((syntax-violation? E)
  		 (condition-message E))
  		(else E))
  	(eval '((splice-first-expand 123))
  	      (environment '(vicare))))
    => "expected list as argument of splice-first-expand")

  #t)


(parametrise ((check-test-name	'compile-time-values))

  (check
      (let ()
	(define-syntax obj1
	  (make-compile-time-value (+ 1 2 3)))

	(define-syntax get-obj1
	  (lambda (stx)
	    (lambda (ctv-retriever)
	      (ctv-retriever #'obj1))))

	(get-obj1))
    => 6)

  (check
      (let ()
	(define-syntax obj2
	  (make-compile-time-value (vector 1 2 3)))

	(define-syntax get-obj2
	  (lambda (stx)
	    (lambda (ctv-retriever)
	      #`(quote #,(ctv-retriever #'obj2)))))

	(get-obj2))
    => '#(1 2 3))

  (void))


(parametrise ((check-test-name	'synonym-transformers))

;;; define-syntax

  (check	;reference
      (let ()
	(define a 1)
	(define-syntax b
	  (make-synonym-transformer #'a))
	(list a b))
    => '(1 1))

  (check	;mutation
      (let ()
	(define a 1)
	(define-syntax b
	  (make-synonym-transformer #'a))
	(set! b 2)
	(list a b))
    => '(2 2))

  (check	;nested mutation
      (let ()
	(define a 1)
	(define-syntax b
	  (make-synonym-transformer #'a))
	(define-syntax c
	  (make-synonym-transformer #'b))
	(set! c 2)
	(list a b c))
    => '(2 2 2))

  (check	;nested nested mutation
      (let ()
	(define a 1)
	(define-syntax b
	  (make-synonym-transformer #'a))
	(define-syntax c
	  (make-synonym-transformer #'b))
	(define-syntax d
	  (make-synonym-transformer #'c))
	(set! c 2)
	(list a b c d))
    => '(2 2 2 2))

;;; --------------------------------------------------------------------
;;; let-syntax

  (check	;reference
      (let ()
	(define a 1)
	(let-syntax ((b (make-synonym-transformer #'a)))
	  (list a b)))
    => '(1 1))

  (check	;mutation
      (let ()
	(define a 1)
	(let-syntax ((b (make-synonym-transformer #'a)))
	  (set! b 2)
	  (list a b)))
    => '(2 2))

  (check	;nested reference
      (let ()
	(define a 1)
	(let-syntax ((b (make-synonym-transformer #'a)))
	  (let-syntax ((c (make-synonym-transformer #'b)))
	    (list a b c))))
    => '(1 1 1))

  (check	;nested mutation
      (let ()
	(define a 1)
	(let-syntax ((b (make-synonym-transformer #'a)))
	  (let-syntax ((c (make-synonym-transformer #'b)))
	    (set! c 2)
	    (list a b c))))
    => '(2 2 2))

  (check	;nested nested mutation
      (let ()
	(define a 1)
	(let-syntax ((b (make-synonym-transformer #'a)))
	  (let-syntax ((c (make-synonym-transformer #'b)))
	    (let-syntax ((d (make-synonym-transformer #'c)))
	      (set! c 2) ;!!!
	      (list a b c d)))))
    => '(2 2 2 2))

;;; --------------------------------------------------------------------
;;; let*-syntax

  (check	;reference
      (let ()
	(define a 1)
	(let*-syntax ((b (make-synonym-transformer #'a))
		      (c (make-synonym-transformer #'b))
		      (d (make-synonym-transformer #'c)))
	  (list a b c d)))
    => '(1 1 1 1))

  (check	;mutation
      (let ()
	(define a 1)
	(let*-syntax ((b (make-synonym-transformer #'a))
		      (c (make-synonym-transformer #'b))
		      (d (make-synonym-transformer #'c)))
	  (set! c 2)
	  (list a b c d)))
    => '(2 2 2 2))

;;; --------------------------------------------------------------------
;;; letrec-syntax

  (check	;reference
      (let ()
	(define a 1)
	(letrec-syntax ((b (make-synonym-transformer #'a))
			(c (make-synonym-transformer #'b))
			(d (make-synonym-transformer #'c)))
	  (list a b c d)))
    => '(1 1 1 1))

  (check	;mutation
      (let ()
	(define a 1)
	(letrec-syntax ((b (make-synonym-transformer #'a))
			(c (make-synonym-transformer #'b))
			(d (make-synonym-transformer #'c)))
	  (set! c 2)
	  (list a b c d)))
    => '(2 2 2 2))

;;; --------------------------------------------------------------------
;;; fluid syntax

  (check
      (let ()
	(define a 1)
	(define-fluid-syntax b
	  (make-synonym-transformer #'a))
	(list a b))
    => '(1 1))

  (check
      (let ()
	(define a 1)
	(define-fluid-syntax b
	  (make-synonym-transformer #'a))
	(set! a 2)
	(list a b))
    => '(2 2))

  (check
      (let ()
	(define a 1)
	(define-fluid-syntax b
	  (lambda (stx) #f))
	(fluid-let-syntax ((b (make-synonym-transformer #'a)))
	  (list a b)))
    => '(1 1))

  (check
      (let ()
	(define a 1)
	(define-fluid-syntax b
	  (lambda (stx) #f))
	(fluid-let-syntax ((b (make-synonym-transformer #'a)))
	  (set! b 2)
	  (list a b)))
    => '(2 2))

;;; --------------------------------------------------------------------
;;; free-identifier=?

  (check
      (let ()
	(define a 1)
	(define-syntax b
	  (make-synonym-transformer #'a))
	(define-syntax (doit stx)
	  (syntax-case stx ()
	    ((_ ?id1 ?id2)
	     (free-identifier=? #'?id1 #'?id2))))
	(doit a b))
    => #f)

  (check
      (let ()
	(define a 1)
	(define-syntax b
	  (make-synonym-transformer #'a))
	(define-syntax c
	  (make-synonym-transformer #'b))
	(define-syntax (doit stx)
	  (syntax-case stx ()
	    ((_ ?id1 ?id2)
	     (free-identifier=? #'?id1 #'?id2))))
	(doit b c))
    => #f)

;;; --------------------------------------------------------------------
;;; circular reference

  (check
      (guard (E ((syntax-violation? E)
		 #t)
		(else E))
	(eval '(letrec-syntax ((b (make-synonym-transformer #'c))
			       (c (make-synonym-transformer #'b)))
		 (list b c))
	      (environment '(vicare))))
    => #t)

  #t)


(parametrise ((check-test-name	'interaction-environment))

  (check	;check persistence of bindings
      (begin
	(eval '(begin
		 (define a 1)
		 (define b 2))
	      (interaction-environment))
	(eval '(list a b)
	      (interaction-environment)))
    => '(1 2))

  (check	;check persistence of bindings
      (begin
	(eval '(define c 3)
	      (interaction-environment))
	(eval 'c
	      (interaction-environment)))
    => 3)

;;; --------------------------------------------------------------------

  (check	;check persistence of bindings
      (let ((env (new-interaction-environment)))
	(eval '(begin
		 (define a 1)
		 (define b 2))
	      env)
	(eval '(list a b)
	      env))
    => '(1 2))

  (check	;check persistence of bindings
      (let ((env (new-interaction-environment '(rnrs base))))
	(eval '(begin
		 (define a 1)
		 (define b 2))
	      env)
	(eval '(list a b)
	      env))
    => '(1 2))

  #t)


(parametrise ((check-test-name	'begin-for-syntax))

  (check	;define then reference
      (let ()
	(begin-for-syntax
	  (define a 1)
	  (define b 2))
	(define-syntax (doit stx)
	  #`(quote (#,a #,b)))
	(doit))
    => '(1 2))

  (check	;define functions then reference
      (let ()
	(begin-for-syntax
	  (define (a) 1)
	  (define (b) 2))
	(define-syntax (doit stx)
	  #`(quote (#,(a) #,(b))))
	(doit))
    => '(1 2))

  (check	;multiple define then reference
      (let ()
	(begin-for-syntax
	  (define a 1))
	(begin-for-syntax
	  (define b 2))
	(begin-for-syntax
	  (define c (+ a b)))
	(define-syntax (doit stx)
	  #`(quote (#,a #,b #,c)))
	(doit))
    => '(1 2 3))

  (check	;multiple define functions then reference
      (let ()
  	(begin-for-syntax
  	  (define (a) 1))
  	(begin-for-syntax
  	  (define (b) 2))
  	(begin-for-syntax
  	  (define c (+ (a) (b))))
  	(define-syntax (doit stx)
  	  #`(quote (#,(a) #,(b) #,c)))
  	(doit))
    => '(1 2 3))

  (check	;define-syntax, then reference
      (let ()
  	;;A  DEFINE-SYNTAX  alone in  the  body  of BEGIN-FOR-SYNTAX  is
  	;;special because it expands to nothing, so we have to test it.
  	(begin-for-syntax
  	  (define-syntax (a stx)
  	    1))
  	(define-syntax (doit stx)
  	  #`(quote #,(a)))
  	(doit))
    => 1)

  (check	;define, define-syntax, then reference
      (let ()
  	(begin-for-syntax
  	  (define-syntax (a stx)
  	    1)
  	  (define b 2))
  	(define-syntax (doit stx)
  	  #`(quote (#,(a) #,b)))
  	(doit))
    => '(1 2))

  (check	;mix defininitions and expressions, then reference
      (let ()
  	(begin-for-syntax
  	  (define a 1)
  	  (set! a 11)
  	  (define b 2))
  	(define-syntax (doit stx)
  	  #`(quote (#,a #,b)))
  	(doit))
    => '(11 2))

  (check	;mix defininitions and expressions, then reference
      (let ()
  	(begin-for-syntax
  	  (define a 1)
  	  (set! a (lambda () 11))
  	  (define b 2))
  	(define-syntax (doit stx)
  	  #`(quote (#,(a) #,b)))
  	(doit))
    => '(11 2))

  (check	;define, mutate, then reference
      (let ()
  	(begin-for-syntax
  	  (define a 1))
  	(begin-for-syntax
  	  (set! a 11))
  	(define-syntax (doit stx)
  	  #`(quote (#,a)))
  	(doit))
    => '(11))

;;; --------------------------------------------------------------------
;;; imported bindings

  (check	;test of import in the rhs of a define-syntax
      (let ()
  	(define-syntax doit
  	  (let ()
  	    (import (vicare language-extensions callables))
  	    (define C
  	      (callable 1 (lambda (self delta)
  			    (+ self delta))))
  	    (lambda (stx)
  	      (C 2))))
  	(doit))
    => 3)

  (check
      (let ()
	(begin-for-syntax
	  (import (vicare language-extensions callables)))
  	(begin-for-syntax
  	  (define C
  	    (callable 1 (lambda (self delta)
  			  (+ self delta)))))
  	(define-syntax (doit stx)
  	  (C 2))
  	(doit))
    => 3)

  (check
      (let ()
  	(begin-for-syntax
  	  (define C
  	    (callable 1 (lambda (self delta)
  			  (+ self delta)))))
  	(define-syntax (doit stx)
  	  (C 2))
  	(doit))
    => 3)

;;; --------------------------------------------------------------------

  (check	;attempt to define the same variable multiple times
      (guard (E ((syntax-violation? E)
		 (values (condition-message E)
			 (syntax->datum (syntax-violation-form E))))
		(else E))
	(eval '(let ()
		 (begin-for-syntax
		   (define a 1)
		   (define a 2))
		 (define-syntax (doit stx)
		   #`(quote (#,a)))
		 (doit))
	      (environment '(vicare))))
    => "multiple definitions of identifier" 'a)

  (check	;attempt to define the same variable multiple times
      (guard (E ((syntax-violation? E)
		 (values (condition-message E)
			 (syntax->datum (syntax-violation-form E))))
		(else E))
	(eval '(let ()
		 (begin-for-syntax
		   (define a 1))
		 (begin-for-syntax
		   (define a 2))
		 (define-syntax (doit stx)
		   #`(quote (#,a)))
		 (doit))
	      (environment '(vicare))))
    => "multiple definitions of identifier" 'a)

  (check	;attempt to define the same syntax multiple times
      (guard (E ((syntax-violation? E)
		 (values (condition-message E)
			 (syntax->datum (syntax-violation-form E))))
		(else E))
	(eval '(let ()
		 (begin-for-syntax
		   (define-syntax (a stx)
		     1)
		   (define-syntax (a stx)
		     2))
		 (define-syntax (doit stx)
		   #`(quote (#,(a))))
		 (doit))
	      (environment '(vicare))))
    => "multiple definitions of identifier" 'a)

  (check	;attempt to define the same syntax multiple times
      (guard (E ((syntax-violation? E)
		 (values (condition-message E)
			 (syntax->datum (syntax-violation-form E))))
		(else E))
	(eval '(let ()
		 (begin-for-syntax
		   (define-syntax (a stx)
		     1))
		 (begin-for-syntax
		   (define-syntax (a stx)
		     2))
		 (define-syntax (doit stx)
		   #`(quote (#,(a))))
		 (doit))
	      (environment '(vicare))))
    => "multiple definitions of identifier" 'a)

  #t)


(parametrise ((check-test-name	'identifier-bound))

  (check-for-false
   (identifier-bound? #'woppa-woppa-woppa))

  (check-for-true
   (let ((ciao 123))
     (define-syntax (doit stx)
       (identifier-bound? #'ciao))
     (doit)))

  (check-for-true
   (let ((ciao 123))
     (define-syntax (doit stx)
       (syntax-case stx ()
	 ((_ ?id)
	  (identifier-bound? #'?id))))
     (doit ciao)))

  (check-for-true
   (let ()
     (define ciao 123)
     (define-syntax (doit stx)
       (syntax-case stx ()
	 ((_ ?id)
	  (identifier-bound? #'?id))))
     (doit ciao)))

  (check-for-true
   (let ()
     (let-syntax ((ciao (identifier-syntax 123)))
       (define-syntax (doit stx)
	 (syntax-case stx ()
	   ((_ ?id)
	    (identifier-bound? #'?id))))
       (doit ciao))))

  #t)


(parametrise ((check-test-name	'binding-properties))

;;; DEFINE-SYNTAX bindings

  (check
      (let ()
	(define-syntax ciao
	  (let ()
	    (syntactic-binding-putprop #'ciao 'a 123)
	    (lambda (stx) #t)))
	(define-syntax (doit stx)
	  (syntactic-binding-getprop #'ciao 'a))
	(doit))
    => 123)

  (check
      (let ()
	(define-syntax (ciao stx)
	  #t)
	(define-syntax (doit stx)
	  (syntactic-binding-getprop #'ciao 'a))
	(eval-for-expand
	  (syntactic-binding-putprop #'ciao 'a 123))
	(doit))
    => 123)

;;; --------------------------------------------------------------------
;;; LETREC-SYNTAX bindings

  (check
      (letrec-syntax ((ciao (let ()
			      (syntactic-binding-putprop #'ciao 'a 123)
			      (lambda (stx) #t))))
	(define-syntax (doit stx)
	  (syntactic-binding-getprop #'ciao 'a))
	(doit))
    => 123)

;;; --------------------------------------------------------------------
;;; LET-SYNTAX bindings

  (check
      (let-syntax ((ciao (lambda (stx) #t)))
	(define-syntax (doit stx)
	  (syntactic-binding-getprop #'ciao 'a))
	(eval-for-expand
	  (syntactic-binding-putprop #'ciao 'a 123))
	(doit))
    => 123)

;;; --------------------------------------------------------------------
;;; DEFINE bindings

  (check
      (let ()
	(define ciao "ciao")
	(define-syntax (doit stx)
	  (syntactic-binding-getprop #'ciao 'a))
	(eval-for-expand
	  (syntactic-binding-putprop #'ciao 'a 123))
	(doit))
    => 123)

;;; --------------------------------------------------------------------
;;; LET bindings

  (check
      (let ((ciao "ciao"))
	(define-syntax (doit stx)
	  (syntactic-binding-getprop #'ciao 'a))
	(eval-for-expand
	  (syntactic-binding-putprop #'ciao 'a 123))
	(doit))
    => 123)

  #t)


(parametrise ((check-test-name	'syntax-parameters))

  (check	;no parametrise, ctv-retriever
      (let ()
	(define-syntax-parameter parm 1)
	(define-syntax (show-it stx)
	  (lambda (ctv-retriever)
	    (ctv-retriever #'parm)))
	(show-it))
    => 1)

  (check	;one parametrise, ctv-retriever
      (let ()
	(define-syntax-parameter parm 1)
	(define-syntax (show-it stx)
	  (lambda (ctv-retriever)
	    (ctv-retriever #'parm)))
	(syntax-parametrise ((parm 2))
	  (show-it)))
    => 2)

  (check	;two parametrise, ctv-retriever
      (let ()
	(define-syntax-parameter parm 1)
	(define-syntax (show-it stx)
	  (lambda (ctv-retriever)
	    (ctv-retriever #'parm)))
	(syntax-parametrise ((parm 2))
	  (show-it)))
    => 2)

;;; --------------------------------------------------------------------

  (check	;no parametrise
      (let ()
  	(define-syntax-parameter parm 1)
  	(define-syntax (show-it stx)
	  (syntax-parameter-value #'parm))
  	(show-it))
    => 1)

  (check	;one parametrise
      (let ()
  	(define-syntax-parameter parm 1)
  	(define-syntax (show-it stx)
  	  (syntax-parameter-value #'parm))
  	(syntax-parametrise ((parm 2))
  	  (show-it)))
    => 2)

  (check	;two parametrise
      (let ()
  	(define-syntax-parameter parm 1)
  	(define-syntax (show-it stx)
  	  (syntax-parameter-value #'parm))
  	(syntax-parametrise ((parm 2))
  	  (show-it)))
    => 2)

;;; --------------------------------------------------------------------

  (check	;alternative spelling: syntax-parameterise
      (let ()
  	(define-syntax-parameter parm 1)
  	(define-syntax (show-it stx)
  	  (syntax-parameter-value #'parm))
  	(syntax-parameterise ((parm 2))
  	  (show-it)))
    => 2)

  (check	;alternative spelling: syntax-parameterize
      (let ()
  	(define-syntax-parameter parm 1)
  	(define-syntax (show-it stx)
  	  (syntax-parameter-value #'parm))
  	(syntax-parameterize ((parm 2))
  	  (show-it)))
    => 2)

;;; --------------------------------------------------------------------

  (check	;documentation example
      (with-result
       (let ()
	 (define-syntax (show-it stx)
	   (syntax-parameter-value #'parm))
	 (define-syntax-parameter parm #f)
	 (add-result (cons 1 (show-it)))
	 (syntax-parametrise ((parm #t))
	   (add-result (cons 2 (show-it)))
	   (syntax-parametrise ((parm #f))
	     (add-result (cons 3 (show-it))))
	   (add-result (cons 4 (show-it))))
	 (add-result (cons 5 (show-it)))
	 #t))
    => '(#t ((1 . #f)
	     (2 . #t)
	     (3 . #f)
	     (4 . #t)
	     (5 . #f))))

  #t)


(parametrise ((check-test-name	'predicate-procedure-argument-validation))

  (define (list-procedure-argument? who obj)
    (if (list? obj)
	obj
      (procedure-argument-violation who
	"expected list object as argument" obj)))

  (begin-for-syntax
    (set-predicate-procedure-argument-validation! #'list?
      #'list-procedure-argument?))

  (check
      ((predicate-procedure-argument-validation list?) 'hey '(1 2 3))
    => '(1 2 3))

  (check
      (guard (E ((procedure-argument-violation? E)
		 (vector (condition-who E)
			 (condition-irritants E)))
		(else E))
	((predicate-procedure-argument-validation list?) 'hey '#(1 2 3)))
    => '#(hey (#(1 2 3))))

  #t)


(parametrise ((check-test-name	'predicate-return-value-validation))

  (define (list-return-value? who obj)
    (if (list? obj)
	obj
      (expression-return-value-violation who
	"expected list object as return value" obj)))

  (begin-for-syntax
    (set-predicate-return-value-validation! #'list?
      #'list-return-value?))

  (check
      ((predicate-return-value-validation list?) 'hey '(1 2 3))
    => '(1 2 3))

  (check
      (guard (E ((expression-return-value-violation? E)
		 (vector (condition-who E)
			 (condition-irritants E)))
		(else E))
	((predicate-return-value-validation list?) 'hey '#(1 2 3)))
    => '#(hey (#(1 2 3))))

  #t)


(parametrise ((check-test-name	'non-hygienic-identifier-syntaxes))

  (check-for-true
   (string? __file__))

  (check
      (let ((len (string-length "test-vicare-expander.sps"))
	    (S   __file__))
	(substring S
		   (- (string-length S) len)
		   (string-length S)))
    => "test-vicare-expander.sps")

;;; --------------------------------------------------------------------

  (check-for-true
   (number? __line__))

  (when #f
    (fprintf stderr "line number ~a\n" __line__))

  #t)


(parametrise ((check-test-name	'non-hygienic-identifier-syntaxes))

  (check
      (with-result
       (let ()
	 (define a 2)
	 (add-result 1)
	 (add-result a)
	 (internal-body
	  (define b 4)
	  (define c 5)
	  (add-result b)
	  (add-result c)
	  (+ a b c))))
    => `(,(+ 2 4 5) (1 2 4 5)))

  (check
      (with-result
       (internal-body
	(define a 1)
	(define b 2)
	(add-result a)
	(add-result b)
	(+ a b)))
    => `(,(+ 1 2) (1 2)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'typ.set-identifier-object-type-spec! 'scheme-indent-function 1)
;; eval: (put 'catch-syntax-violation 'scheme-indent-function 1)
;; eval: (put 'catch-expand-time-signature-violation 'scheme-indent-function 1)
;; End:
