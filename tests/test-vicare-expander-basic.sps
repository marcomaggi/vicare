;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the expander
;;;Date: Tue Sep 25, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; copyright notice for the XOR macro
;;;
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;Except as  contained in this  notice, the name(s)  of the above  copyright holders
;;;shall not be  used in advertising or  otherwise to promote the sale,  use or other
;;;dealings in this Software without prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#!vicare
(program (test-vicare-expander-basic)
  (import (vicare)
    (vicare language-extensions callables)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: basic expander syntaxes\n")


;;;; helpers

(case-define %eval
  ((sexp)
   (%eval sexp (environment '(vicare))))
  ((sexp env)
   (with-exception-handler
       (lambda (E)
	 (unless (warning? E)
	   (raise E)))
     (lambda ()
       (eval sexp env)))))

(define-syntax check-argument-violation
  (syntax-rules (=>)
    ((_ ?body => ?result)
     (check
	 (guard (E ((procedure-signature-argument-violation? E)
		    #;(print-condition E)
		    (list (condition-who E)
			  (procedure-signature-argument-violation.one-based-argument-index E)
			  (procedure-signature-argument-violation.failed-expression E)
			  (procedure-signature-argument-violation.offending-value E)))
		   ((procedure-signature-return-value-violation? E)
		    #;(print-condition E)
		    (list (condition-who E)
			  (procedure-signature-return-value-violation.one-based-return-value-index E)
			  (procedure-signature-return-value-violation.failed-expression E)
			  (procedure-signature-return-value-violation.offending-value E)))
		   ((procedure-arguments-consistency-violation? E)
		    #;(print-condition E)
		    (condition-irritants E))
		   ((procedure-argument-violation? E)
		    (when #f
		      (debug-print (condition-message E)))
		    (let ((D (cdr (condition-irritants E))))
		      (if (pair? D)
			  (car D)
			(condition-irritants E))))
		   ((assertion-violation? E)
		    (condition-irritants E))
		   (else
		    (print-condition E)
		    E))
	   (with-exception-handler
	       (lambda (E)
		 (unless (warning? E)
		   (raise E)))
	     (lambda () ?body)))
       => ?result))))


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
      (with-exception-handler
	  (lambda (E)
	    (unless (warning? E)
	      (raise E)))
	(lambda ()
	  (eval '(str.length "ciao")
		(environment
		 '(prefix
		   (deprefix (only (rnrs)
				   string-length
				   string-append)
			     string-)
		   str.)))))
    => 4)

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


(parametrise ((check-test-name	'define-syntax-extended))

;;;auxiliary syntax definition

  (check
      (let ()
	(define-syntax ciao)
	(define-syntax (doit stx)
	  (syntax-case stx ()
	    ((_ ?id)
	     (free-identifier=? #'?id #'ciao))))
	(doit ciao))
    => #t)

  (check
      (let ()
	(define-syntax ciao)
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
	(define-syntax ciao)
	(define-syntax doit
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
	(define-syntax ciao)
	(define-syntax (doit stx)
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
	(%eval '(let ()
		  (define-syntax (doit stx)
		    (syntax-case stx ()
		      ((_ ?id)
		       (__synner__ "bad syntax"))))
		  (doit hello))
	       (environment '(vicare))))
    => '(doit (doit hello) #f))

  (check
      (guard (E ((syntax-violation? E)
		 (list (condition-who E)
		       (syntax->datum (syntax-violation-form E))
		       (syntax->datum (syntax-violation-subform E))))
		(else E))
	(%eval '(let ()
		  (define-syntax (doit stx)
		    (syntax-case stx ()
		      ((_ ?id)
		       (__synner__ "bad syntax" #'?id))))
		  (doit hello))
	       (environment '(vicare))))
    => '(doit (doit hello) hello))

  (check	;redefinition  of __WHO__,  __SYNNER__  still  bound to  the
		;automatically generated one
      (guard (E ((syntax-violation? E)
		 (list (condition-who E)
		       (syntax->datum (syntax-violation-form E))
		       (syntax->datum (syntax-violation-subform E))))
		(else E))
	(%eval '(let ()
		  (define-syntax (doit stx)
		    (define __who__ 'other)
		    (syntax-case stx ()
		      ((_ ?id)
		       (__synner__ "bad syntax" #'?id))))
		  (doit hello))
	       (environment '(vicare))))
    => '(doit (doit hello) hello))

  (check	;redefinition of WHO and __SYNNER__
      (guard (E ((syntax-violation? E)
		 (list (condition-who E)
		       (syntax->datum (syntax-violation-form E))
		       (syntax->datum (syntax-violation-subform E))))
		(else E))
	(%eval '(let ()
		  (define-syntax (doit stx)
		    (define __who__ 'other)
		    (define (__synner__ message subform)
		      (syntax-violation __who__ message subform #f))
		    (syntax-case stx ()
		      ((_ ?id)
		       (__synner__ "bad syntax" #'?id))))
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
  (check (xor (number? (cast-signature (<top>) 1))) => #T)
  (check (xor (null? (cast-signature (<top>) 1))) => #f)
  (check (xor (string->symbol "foo")) => 'foo)
  (check (xor (string? (cast-signature (<top>) "a")) (symbol? (cast-signature (<top>) 1))) => #T)
  (check (xor (string? (cast-signature (<top>) 1)) (symbol? (cast-signature (<top>) 'a))) => #T)
  (check (xor (string? (cast-signature (<top>) 1)) (symbol? (cast-signature (<top>) 2))) => #f)
  (check (xor (pair? (cast-signature (<top>) '(a))) (list? (cast-signature (<top>) '(b)))) => #f)
  (check (xor (cast-signature (<top>) (- 42)) (cast-signature (<top>) (not 42))) => -42)
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


(parametrise ((check-test-name	'and))

  (define-syntax expand
    (syntax-rules (=>)
      ((_ ?form => ?expected)
       (check
	   (expansion-of ?form)
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------

  (expand (and (real? (read))
	       (nan?  (read)))
	  => (if ((primitive real?) ((primitive read)))
		 ((primitive nan?) ((primitive read)))
	       (quote #f)))

;;; --------------------------------------------------------------------
;;; expressions typed as always non-false

  (expand (and 1 2 3)
	  => (begin
	       (quote 1)
	       (begin
		 (quote 2)
		 (quote 3))))

;;; --------------------------------------------------------------------
;;; expressions typed as "<false>"

  (expand (and #f 2 3)
	  => (quote #f))

  (expand (and 1 #f 3)
	  => (begin
	       (quote 1)
	       (quote #f)))

  (expand (and 1 2 #f)
	  => (begin
	       (quote 1)
	       (begin
		 (quote 2)
		 (quote #f))))

;;; --------------------------------------------------------------------
;;; expressions typed as "<no-return>"

;;This file does not use the typed language.

  (expand (and 1 (raise 2) 3)
	  => (begin
	       (quote 1)
	       ((primitive raise) (quote 2))))

  (expand (and 1 (error #f "ciao") 3)
	  => (begin
	       (quote 1)
	       ((primitive error) (quote #f) (quote "ciao"))))

  (expand (and 1 (raise-continuable 2) 3)
	  => (begin
	       (quote 1)
	       (if ((primitive raise-continuable) (quote 2))
		   (quote 3)
		 (quote #f))))

  (void))


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


(parametrise ((check-test-name	'endianness))

  (check (endianness little)		=> 'little)
  (check (endianness big)		=> 'big)
  (check (endianness network)		=> 'big)
  (check (endianness native)		=> (native-endianness))

  #t)


(parametrise ((check-test-name	'values-to-list))

  (check
      (values->list 123)
    => '(123))

  (check
      (values->list (values 1 2 3))
    => '(1 2 3))

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
	((a b c)	(values 'symbol #f #f))
	((1 2 3)	(values 7 8 9))
	(else		(values 'else #f #f)))
    => 7 8 9)

  (check
      (case '()
	((#f 0 ())	'here)
	(else		'there))
    => 'here)

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
	((a b c)	(values 'symbol #f #f))
	((1 2 3)	=> (lambda (N) (values N N N)))
	(else		(values 'else #f #f)))
    => 2 2 2)

;;; --------------------------------------------------------------------
;;; arrow in ELSE clause

  (check
      (case 9
	((1)	1)
	((2)	2)
	(else
	 => (lambda (obj)
	      (list 'else obj))))
    => '(else 9))

;;; --------------------------------------------------------------------
;;; errors

  (check	;receiver form does not evaluate to function
      (try
	  (%eval '(case 2
		    ((a b c)	'symbol)
		    ((1 2 3)	=> 123)
		    (else	'else)))
	(catch E
	  ((&expression-return-value-violation)
	   #t)
	  (else #f)))
    => #t)

  (check	;receiver form does not evaluate to function
      (try
	  (%eval '(case 99
		    ((a b c)	'symbol)
		    ((1 2 3)	123)
		    (else	=> 'else)))
	(catch E
	  ((&expression-return-value-violation)
	   #t)
	  (else #f)))
    => #t)

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
  	((one two three)	123)
  	(else			'else))
    => 123)

  (check	;expr is not an identifier
      (case-identifiers 123
  	((a b c)		'symbol)
  	((one two three)	123)
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
  	((one two three)	=> (lambda (N) N))
  	(else			'else))
    (=> syntax=?)
    #'two)

;;; --------------------------------------------------------------------
;;; arrow in ELSE clause

  (check
      (case-identifiers #'nine
	((a b c)		'symbol)
	((one two three)	'id)
	(else			=> (lambda (N) (list #'else N))))
    (=> syntax=?)
    #'(else nine))

;;; --------------------------------------------------------------------
;;; errors

  (check	;receiver form does not evaluate to function
      (try
	  (%eval '(case-identifiers #'two
		    ((a b c)		'symbol)
		    ((one two three)	=> 'one-two-three)
		    (else		'else)))
	(catch E
	  ((&expression-return-value-violation)
	   #t)
	  (else #f)))
    => #t)

  (check	;receiver form does not evaluate to function
      (try
	  (%eval '(case-identifiers #'nine
		    ((a b c)		'symbol)
		    ((one two three)	'one-two-three)
		    (else		=> 'else)))
	(catch E
	  ((&expression-return-value-violation)
	   #t)
	  (else #f)))
    => #t)

  (check	;datum is not an identifier
      (guard (E ((syntax-violation? E)
  		 (condition-message E))
  		(else E))
  	(%eval '(case-identifiers #'two
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
  	(%eval '((splice-first-expand 123))
  	      (environment '(vicare))))
    => "expected list as argument of splice-first-expand")

  #t)


(parametrise ((check-test-name	'expand-time-values))

  (check
      (let ()
	(define-syntax obj1
	  (make-expand-time-value (+ 1 2 3)))

	(define-syntax get-obj1
	  (lambda (stx)
	    (retrieve-expand-time-value #'obj1)))

	(get-obj1))
    => 6)

  (check
      (let ()
	(define-syntax obj2
	  (make-expand-time-value (vector 1 2 3)))

	(define-syntax get-obj2
	  (lambda (stx)
	    #`(quote #,(retrieve-expand-time-value #'obj2))))

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
	(%eval '(letrec-syntax ((b (make-synonym-transformer #'c))
			       (c (make-synonym-transformer #'b)))
		 (list b c))
	      (environment '(vicare))))
    => #t)

  #t)


(parametrise ((check-test-name	'interaction-environment))

;;; interaction-environment

  (check	;check persistence of bindings
      (begin
	(%eval '(begin
		 (define a 1)
		 (define b 2))
	      (interaction-environment))
	(%eval '(list a b)
	      (interaction-environment)))
    => '(1 2))

  (check	;check persistence of bindings
      (begin
	(%eval '(define c 3)
	      (interaction-environment))
	(%eval 'c
	      (interaction-environment)))
    => 3)

  (check	;check binding redefinition
      (begin
	(%eval '(define d 3)
	      (interaction-environment))
	(%eval '(define d 4)
	      (interaction-environment))
	(%eval 'd
	      (interaction-environment)))
    => 4)

  (check	;check binding redefinition
      (begin
	(%eval '(begin
		 (define x 1)
		 (define y 2)
		 (define z 3))
	      (interaction-environment))
	(%eval '(begin
		 (define x 10)
		 (define y 20)
		 (define z 30))
	      (interaction-environment))
	(%eval '(list x y z)
	      (interaction-environment)))
    => '(10 20 30))

;;; --------------------------------------------------------------------
;;; new-interaction-environment

  (check	;check persistence of bindings
      (let ((env (new-interaction-environment)))
	(%eval '(begin
		 (define a 1)
		 (define b 2))
	      env)
	(%eval '(list a b)
	      env))
    => '(1 2))

  (check	;check persistence of bindings
      (let ((env (new-interaction-environment '(rnrs base))))
	(%eval '(begin
		 (define a 1)
		 (define b 2))
	      env)
	(%eval '(list a b)
	      env))
    => '(1 2))

  (check	;check binding redefinition
      (let ((env (new-interaction-environment '(rnrs base))))
	(%eval '(define d 3) env)
	(%eval '(define d 4) env)
	(%eval 'd env))
    => 4)

  (check	;check binding redefinition
      (let ((env (new-interaction-environment '(rnrs base))))
	(%eval '(begin
		 (define x 1)
		 (define y 2)
		 (define z 3))
	      env)
	(%eval '(begin
		 (define x 10)
		 (define y 20)
		 (define z 30))
	      env)
	(%eval '(list x y z)
	      env))
    => '(10 20 30))

  (check	;check binding redefinition
      (let ((env (new-interaction-environment '(rnrs base))))
	(%eval '(begin
		 (define-syntax x (identifier-syntax 1))
		 (define-syntax y (identifier-syntax 2))
		 (define-syntax z (identifier-syntax 3)))
	      env)
	(%eval '(begin
		 (define-syntax x (identifier-syntax 10))
		 (define-syntax y (identifier-syntax 20))
		 (define-syntax z (identifier-syntax 30)))
	      env)
	(%eval '(list x y z)
	      env))
    => '(10 20 30))

  (check	;check binding redefinition
      (let ((env (new-interaction-environment '(vicare))))
	(%eval '(begin
		 (define-fluid-syntax x (identifier-syntax 1))
		 (define-fluid-syntax y (identifier-syntax 2))
		 (define-fluid-syntax z (identifier-syntax 3)))
	      env)
	(%eval '(begin
		 (define-fluid-syntax x (identifier-syntax 10))
		 (define-fluid-syntax y (identifier-syntax 20))
		 (define-fluid-syntax z (identifier-syntax 30)))
	      env)
	(%eval '(list x y z)
	      env))
    => '(10 20 30))

  ;;In an interaction  environment the top-level definitions can  shadow the bindings
  ;;imported by the environment.
  ;;
  (check
      (let ((env (new-interaction-environment '(vicare))))
	(%eval '(define display 123) env)
	(%eval 'display env))
    => 123)

;;; --------------------------------------------------------------------

  ;;In  an  interaction  environment  the   top-level  definitions  in  the  body  of
  ;;BEGIN-FOR-SYNTAX can be redefined.
  ;;
  (check
      (let ((env (new-interaction-environment '(vicare))))
	(%eval '(begin
		 (begin-for-syntax
		   (define a 1)
		   (define a 2))
		 (define-syntax doit
		   (lambda (stx) a)))
	      env)
	(%eval '(doit) env))
    => 2)

  ;;In  an  interaction  environment  the   top-level  definitions  in  the  body  of
  ;;BEGIN-FOR-SYNTAX can shadow the bindings imported by the environment.
  ;;
  (check
      (let ((env (new-interaction-environment '(vicare))))
	(%eval '(begin
		 (begin-for-syntax
		   (define display 123))
		 (define-syntax doit
		   (lambda (stx) display)))
	      env)
	(%eval '(doit) env))
    => 123)

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
		(else
		 (values E #f)))
	(%eval '(let ()
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
		(else
		 (values E #f)))
	(%eval '(let ()
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
		(else
		 (values E #f)))
	(%eval '(let ()
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
		(else
		 (values E #f)))
	(%eval '(let ()
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
	(begin-for-syntax
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
	(begin-for-syntax
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
	(begin-for-syntax
	  (syntactic-binding-putprop #'ciao 'a 123))
	(doit))
    => 123)

;;; --------------------------------------------------------------------
;;; LET bindings

  (check
      (let ((ciao "ciao"))
	(define-syntax (doit stx)
	  (syntactic-binding-getprop #'ciao 'a))
	(begin-for-syntax
	  (syntactic-binding-putprop #'ciao 'a 123))
	(doit))
    => 123)

  #t)


(parametrise ((check-test-name	'syntax-parameters))

  (check	;no parametrise, retrieve-expand-time-value
      (let ()
	(define-syntax-parameter parm 1)
	(define-syntax (show-it stx)
	  (retrieve-expand-time-value #'parm))
	(show-it))
    => 1)

  (check	;one parametrise, retrieve-expand-time-value
      (let ()
	(define-syntax-parameter parm 1)
	(define-syntax (show-it stx)
	  (retrieve-expand-time-value #'parm))
	(syntax-parametrise ((parm 2))
	  (show-it)))
    => 2)

  (check	;two parametrise, retrieve-expand-time-value
      (let ()
	(define-syntax-parameter parm 1)
	(define-syntax (show-it stx)
	  (retrieve-expand-time-value #'parm))
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


(parametrise ((check-test-name	'non-hygienic-identifier-syntaxes))

  (check-for-true
   (string? __file__))

  (check
      (let ((len (string-length "test-vicare-expander-basic.sps"))
	    (S   __file__))
	(substring S
		   (- (string-length S) len)
		   (string-length S)))
    => "test-vicare-expander-basic.sps")

;;; --------------------------------------------------------------------

  (check-for-true
   (number? __line__))

  (when #f
    (fprintf stderr "line number ~a\n" __line__))

  #t)


(parametrise ((check-test-name	'internal-body))

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


(parametrise ((check-test-name	'new-and-delete))

;;; structs

  (check
      (with-result
	(internal-body

	  (define-struct duo (one two))

	  (define (duo-destructor stru)
	    (receive (port extract)
		(open-string-output-port)
	      (display stru port)
	      (add-result (extract)))
	    1234)

	  (module ()
	    (set-struct-type-destructor! (type-descriptor duo)
	      duo-destructor))

	  (define O
	    (new duo 1 2))

	  (delete O)))
    => '(1234 ("#[struct duo one=1 two=2]")))

;;; --------------------------------------------------------------------
;;; records

  (check
      (with-result
	(internal-body

	  (define-record-type duo
	    (fields one two))

	  (define (duo-destructor stru)
	    (receive (port extract)
		(open-string-output-port)
	      (display stru port)
	      (add-result (extract)))
	    1234)

	  (module ()
	    (record-type-destructor-set! (type-descriptor duo)
	      duo-destructor))

	  (define O
	    (new duo 1 2))

	  (delete O)))
    => '(1234 ("#[record duo one=1 two=2]")))

  ;;Record with parent, no protocols.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b))

	  (define-record-type beta
	    (parent alpha)
	    (fields c d))

	  (define (beta-destructor stru)
	    (receive (port extract)
		(open-string-output-port)
	      (display stru port)
	      (add-result (extract)))
	    1234)

	  (module ()
	    (record-type-destructor-set! (type-descriptor beta)
	      beta-destructor))

	  (define O
	    (new beta 1 2 3 4))

	  (delete O)))
    => '(1234 ("#[record beta a=1 b=2 c=3 d=4]")))

  ;;Record with parent, parent with protocol.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b)
	    (protocol
	     (lambda (make-record)
	       (lambda (a)
		 (make-record a 2)))))

	  (define-record-type beta
	    (parent alpha)
	    (fields c d)
	    (protocol
	     (lambda (make-alpha)
	       (lambda (a c)
		 ((make-alpha a) c 4)))))

	  (define (beta-destructor stru)
	    (receive (port extract)
		(open-string-output-port)
	      (display stru port)
	      (add-result (extract)))
	    1234)

	  (module ()
	    (record-type-destructor-set! (type-descriptor beta)
	      beta-destructor))

	  (define O
	    (new beta 1 3))

	  (delete O)))
    => '(1234 ("#[record beta a=1 b=2 c=3 d=4]")))

  ;;Record with parent, child with protocol.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b))

	  (define-record-type beta
	    (parent alpha)
	    (fields c d)
	    (protocol
	     (lambda (make-alpha)
	       (lambda (a b c)
		 ((make-alpha a b) c 4)))))

	  (define (beta-destructor stru)
	    (receive (port extract)
		(open-string-output-port)
	      (display stru port)
	      (add-result (extract)))
	    1234)

	  (module ()
	    (record-type-destructor-set! (type-descriptor beta)
	      beta-destructor))

	  (define O
	    (new beta 1 2 3))

	  (delete O)))
    => '(1234 ("#[record beta a=1 b=2 c=3 d=4]")))

  ;;Record with parent, parent and child with protocols.
  ;;
  (check
      (with-result
	(internal-body

	  (define-record-type alpha
	    (fields a b)
	    (protocol
	     (lambda (make-record)
	       (lambda (a)
		 (make-record a 2)))))

	  (define-record-type beta
	    (parent alpha)
	    (fields c d)
	    (protocol
	     (lambda (make-alpha)
	       (lambda (a c)
		 ((make-alpha a) c 4)))))

	  (define (beta-destructor stru)
	    (receive (port extract)
		(open-string-output-port)
	      (display stru port)
	      (add-result (extract)))
	    1234)

	  (module ()
	    (record-type-destructor-set! (type-descriptor beta)
	      beta-destructor))

	  (define O
	    (new beta 1 3))

	  (delete O)))
    => '(1234 ("#[record beta a=1 b=2 c=3 d=4]")))

;;; --------------------------------------------------------------------
;;; compensations

  (check
      (with-result
	(internal-body

	  (define-struct duo (one two))

	  (define (duo-destructor stru)
	    (receive (port extract)
		(open-string-output-port)
	      (display stru port)
	      (add-result (extract)))
	    1234)

	  (module ()
	    (set-struct-type-destructor! (type-descriptor duo)
	      duo-destructor))

	  (with-compensations
	    (duo-two (compensate
			 (new duo 1 2)
		       (with
			(delete <>)))))))
    => '(2 ("#[struct duo one=1 two=2]")))

  (check
      (with-result
	(internal-body

	  (define-struct duo (one two))

	  (define (duo-destructor stru)
	    (receive (port extract)
		(open-string-output-port)
	      (display stru port)
	      (add-result (extract)))
	    1234)

	  (module ()
	    (set-struct-type-destructor! (type-descriptor duo)
	      duo-destructor))

	  (define (make-compensated-duo one two)
	    (compensate
		(new duo one two)
	      (with
	       (delete <>))))

	  (with-compensations
	    (duo-two (make-compensated-duo 1 2)))))
    => '(2 ("#[struct duo one=1 two=2]")))

  #t)


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'typ.set-identifier-tag-type-spec!	'scheme-indent-function 1)
;; eval: (put 'catch-syntax-violation			'scheme-indent-function 1)
;; eval: (put 'case-identifiers				'scheme-indent-function 1)
;; End:
