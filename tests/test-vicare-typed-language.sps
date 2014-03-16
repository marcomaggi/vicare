;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for typed language extensions
;;;Date: Wed Mar 12, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (prefix (vicare expander type-spec) typ.)
  (vicare language-extensions tags)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: expand-time types\n")


;;;; helpers

(define-auxiliary-syntaxes <fixnums>)

(define (fixnums? obj)
  (and (list? obj)
       (for-all fixnum? obj)))

(begin-for-syntax
  (define-syntax-rule (tag=tagging? ?tag ?var)
    (let ((id (typ.identifier-type-tagging #'?var)))
      (if (identifier? id)
	  (free-identifier=? #'?tag id)
	"no-id")))

  (define-syntax-rule (top-tagged? ?var)
    (tag=tagging? <top> ?var))

  (typ.set-identifier-type-spec! #'<fixnums>
    (typ.make-type-spec #'<fixnums> #'fixnums?))

  #| end of begin-for-syntax |# )

(define-syntax %eval
  (syntax-rules ()
    ((_ ?form)
     (eval ?form (environment '(vicare)
			      '(vicare language-extensions tags))))))

(define-syntax catch-syntax-violation
  (syntax-rules ()
    ((_ ?verbose . ?body)
     (guard (E ((syntax-violation? E)
		(when ?verbose
		  (debug-print (condition-message E)
			       (syntax-violation-form E)
			       (syntax-violation-subform E)))
		(syntax->datum (syntax-violation-subform E)))
	       (else E))
       . ?body))))


(parametrise ((check-test-name	'parsing-tagged-bindings))

  (check
      (typ.tagged-identifier? #'(brace X <fixnum>))
    => #t)

  (check
      (typ.tagged-identifier? #'X)
    => #t)

  (check
      (typ.tagged-identifier? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (typ.parse-tagged-identifier #'(brace X <fixnum>))
    (=> syntax=?)
    #'X #'<fixnum>)

  (check
      (typ.parse-tagged-identifier #'X)
    (=> syntax=?)
    #'X #'<top>)

;;; --------------------------------------------------------------------
;;; list of tagged identifiers

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}))
    (=> syntax=?)
    #'(a) #'(<fixnum>))

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}
				    {b <string>}))
    (=> syntax=?)
    #'(a b) #'(<fixnum> <string>))

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}
				    {b <string>}
				    {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <string> <vector>))

;;;

  (check
      (typ.parse-tagged-bindings #'(a))
    (=> syntax=?)
    #'(a) #'(<top>))

  (check
      (typ.parse-tagged-bindings #'(a b))
    (=> syntax=?)
    #'(a b) #'(<top> <top>))

  (check
      (typ.parse-tagged-bindings #'(a b c))
    (=> syntax=?)
    #'(a b c) #'(<top> <top> <top>))

;;;

  (check
      (typ.parse-tagged-bindings #'(a
				    {b <string>}))
    (=> syntax=?)
    #'(a b) #'(<top> <string>))

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}
				    b))
    (=> syntax=?)
    #'(a b) #'(<fixnum> <top>))

  (check
      (typ.parse-tagged-bindings #'(a
				    {b <string>}
				    {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<top> <string> <vector>))

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}
				    b
				    {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <top> <vector>))

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}
				    {b <string>}
				    c))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <string> <top>))

;;; --------------------------------------------------------------------
;;; tagged formals

;;; tagged

  (check
      (typ.parse-tagged-lambda-formals #'({a <fixnum>}
					  {b <string>}))
    (=> syntax=?)
    #'(a b) #'(() <fixnum> <string>))

  (check
      (typ.parse-tagged-lambda-formals #'({a <fixnum>}
					  {b <string>}
					  {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(() <fixnum> <string> <vector>))

;;; untagged

  (check
      (typ.parse-tagged-lambda-formals #'(a))
    (=> syntax=?)
    #'(a) #'(() <top>))

  (check
      (typ.parse-tagged-lambda-formals #'(a b))
    (=> syntax=?)
    #'(a b) #'(() <top> <top>))

  (check
      (typ.parse-tagged-lambda-formals #'(a b c))
    (=> syntax=?)
    #'(a b c) #'(() <top> <top> <top>))

;;; mixed tagged and untagged

  (check
      (typ.parse-tagged-lambda-formals #'(a
					  {b <string>}))
    (=> syntax=?)
    #'(a b) #'(() <top> <string>))

  (check
      (typ.parse-tagged-lambda-formals #'({a <fixnum>}
					  b))
    (=> syntax=?)
    #'(a b) #'(() <fixnum> <top>))

  (check
      (typ.parse-tagged-lambda-formals #'(a
					  {b <string>}
					  {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(() <top> <string> <vector>))

  (check
      (typ.parse-tagged-lambda-formals #'({a <fixnum>}
					  b
					  {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(() <fixnum> <top> <vector>))

  (check
      (typ.parse-tagged-lambda-formals #'({a <fixnum>}
					  {b <string>}
					  c))
    (=> syntax=?)
    #'(a b c) #'(() <fixnum> <string> <top>))

;;; args argument

  (check	;tagged args argument
      (typ.parse-tagged-lambda-formals #'{args <fixnums>})
    (=> syntax=?)
    #'args #'(() . <fixnums>))

  (check	;UNtagged args argument
      (typ.parse-tagged-lambda-formals #'args)
    (=> syntax=?)
    #'args #'(() . <top>))

;;; rest argument

  (check	;tagged rest
      (typ.parse-tagged-lambda-formals #'({a <fixnum>} . {rest <fixnums>}))
    (=> syntax=?)
    #'(a . rest) #'(() <fixnum> . <fixnums>))

  (check	;UNtagged rest
      (typ.parse-tagged-lambda-formals #'({a <fixnum>} . rest))
    (=> syntax=?)
    #'(a . rest) #'(() <fixnum> . <top>))

  (check	;tagged rest
      (typ.parse-tagged-lambda-formals #'({a <fixnum>} {b <string>} . {rest <fixnums>}))
    (=> syntax=?)
    #'(a b . rest) #'(() <fixnum> <string> . <fixnums>))

  (check	;UNtagged rest
      (typ.parse-tagged-lambda-formals #'({a <fixnum>} {b <string>} . rest))
    (=> syntax=?)
    #'(a b . rest) #'(() <fixnum> <string> . <top>))

;;; return values tagging

  (check
      (typ.parse-tagged-lambda-formals #'({_ <fixnum>} a b))
    (=> syntax=?)
    #'(a b) #'((<fixnum>) <top> <top>))

  (check
      (typ.parse-tagged-lambda-formals #'({_ <fixnum>} {a <flonum>} {b <string>}))
    (=> syntax=?)
    #'(a b) #'((<fixnum>) <flonum> <string>))

  (check
      (typ.parse-tagged-lambda-formals #'({_ <fixnum> <flonum>} {a <vector>} {b <string>}))
    (=> syntax=?)
    #'(a b) #'((<fixnum> <flonum>) <vector> <string>))

  (check
      (typ.parse-tagged-lambda-formals #'({_ <fixnum> <flonum>} . {args <fixnums>}))
    (=> syntax=?)
    #'args #'((<fixnum> <flonum>) . <fixnums>))


;;; --------------------------------------------------------------------
;;; tagged formals predicate

  (check-for-true
   (typ.tagged-lambda-formals? #'({a <fixnum>} {b <string>})))

  #t)


(parametrise ((check-test-name	'tagged-bindings-lambda))

;;;untagged bindings

  (check
      ((lambda args
	 (define-syntax (inspect stx)
	   (top-tagged? args))
	 (values args (inspect)))
       1)
    (=> syntax=?) '(1) #t)

  (check
      ((lambda (a)
	 (define-syntax (inspect stx)
	   (top-tagged? a))
	 (values a (inspect)))
       1)
    (=> syntax=?) 1 #t)

  (check
      ((lambda (a b)
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b))))
	 (values a b (inspect)))
       1 2)
    (=> syntax=?) 1 2 '(#t #t))

  (check
      ((lambda (a b . rest)
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b)
			    (top-tagged? rest))))
	 (values a b rest (inspect)))
       1 2 3 4)
    (=> syntax=?) 1 2 '(3 4) '(#t #t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      ((lambda {args <fixnums>}
	 (define-syntax (inspect stx)
	   (tag=tagging? <fixnums> args))
	 (values args (inspect)))
       1 2 3)
    (=> syntax=?) '(1 2 3) #t)

  (check
      ((lambda ({a <flonum>})
	 (define-syntax (inspect stx)
	   (tag=tagging? <flonum> a))
	 (values a (inspect)))
       1.1)
    (=> syntax=?) 1.1 #t)

  (check
      ((lambda ({a <flonum>} {b <ratnum>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (tag=tagging? <flonum> a)
			    (tag=tagging? <ratnum> b))))
	 (values a b (inspect)))
       1.1 2/3)
    (=> syntax=?) 1.1 2/3 '(#t #t))

  (check
      ((lambda ({a <flonum>} {b <ratnum>} . {rest <fixnums>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (tag=tagging? <flonum>  a)
			    (tag=tagging? <ratnum>  b)
			    (tag=tagging? <fixnums> rest))))
	 (values a b rest (inspect)))
       1.1 2/3 3 4)
    (=> syntax=?) 1.1 2/3 '(3 4) '(#t #t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings and return values

  (check
      ((lambda ({_ <list> <boolean> <boolean>} . {args <fixnums>})
	 (define-syntax (inspect stx)
	   (tag=tagging? <fixnums> args))
	 (values args (inspect)))
       1 2 3)
    (=> syntax=?) '(1 2 3) #t)

  (check
      ((lambda ({_ <flonum> <boolean>} {a <flonum>})
	 (define-syntax (inspect stx)
	   (tag=tagging? <flonum> a))
	 (values a (inspect)))
       1.1)
    (=> syntax=?) 1.1 #t)

  (check
      ((lambda ({_ <flonum> <ratnum> <list>} {a <flonum>} {b <ratnum>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (tag=tagging? <flonum> a)
			    (tag=tagging? <ratnum> b))))
	 (values a b (inspect)))
       1.1 2/3)
    (=> syntax=?) 1.1 2/3 '(#t #t))

  (check
      ((lambda ({_ <flonum> <ratnum> <list> <list>} {a <flonum>} {b <ratnum>} . {rest <fixnums>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (tag=tagging? <flonum>  a)
			    (tag=tagging? <ratnum>  b)
			    (tag=tagging? <fixnums> rest))))
	 (values a b rest (inspect)))
       1.1 2/3 3 4)
    (=> syntax=?) 1.1 2/3 '(3 4) '(#t #t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-define))

;;;untagged bindings

  (check
      (let ()
	(define (fun . args)
	  (define-syntax (inspect stx)
	    #`(quote #,(list (tag=tagging? <procedure> fun)
			     (top-tagged? args))))
	  (define-syntax (signature stx)
	    (syntax=? #'(() . <top>) (typ.identifier-function-signature #'fun)))
	  (values args (inspect) (signature)))
	(fun 1))
    => '(1) '(#t #t) #t)

  (check
      (let ()
	(define (fun)
	  (define-syntax (inspect stx)
	    (tag=tagging? <procedure> fun))
	  (define-syntax (signature stx)
	    (syntax=? #'(() . ()) (typ.identifier-function-signature #'fun)))
	  (values (inspect) (signature)))
	(fun))
    => #t #t)

  (check
      (let ()
	(define (fun a)
	  (define-syntax (inspect stx)
	    #`(quote #,(list (tag=tagging? <procedure> fun)
			     (top-tagged? a))))
	  (values a (inspect)))
	(fun 1))
    => 1 '(#t #t))

  (check
      (let ()
	(define (fun a b)
	  (define-syntax (inspect stx)
	    #`(quote #,(list (top-tagged? a)
			     (top-tagged? b))))
	  (values a b (inspect)))
	(fun 1 2))
    => 1 2 '(#t #t))

  (check
      (let ()
	(define (fun a b . rest)
	  (define-syntax (inspect stx)
	    #`(quote #,(list (top-tagged? a)
			     (top-tagged? b)
			     (top-tagged? rest))))
	  (values a b rest (inspect)))
	(fun 1 2 3 4))
    => 1 2 '(3 4) '(#t #t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (let ()
	(define (fun . {args <fixnums>})
	  (define-syntax (inspect stx)
	    (tag=tagging? <fixnums> args))
	  (values args (inspect)))
	(fun 1 2 3))
    => '(1 2 3) #t)

  (check
      (let ()
	(define (fun {a <flonum>})
	  (define-syntax (inspect stx)
	    (tag=tagging? <flonum> a))
	  (values a (inspect)))
	(fun 1.1))
    => 1.1 #t)

  (check
      (let ()
	(define (fun {a <flonum>} {b <ratnum>})
	  (define-syntax (inspect stx)
	    #`(quote #,(list (tag=tagging? <flonum> a)
			     (tag=tagging? <ratnum> b))))
	  (values a b (inspect)))
	(fun 1.1 2/3))
    => 1.1 2/3 '(#t #t))

  (check
      (let ()
	(define (fun {a <flonum>} {b <ratnum>} . {rest <fixnums>})
	  (define-syntax (inspect stx)
	    #`(quote #,(list (tag=tagging? <flonum>  a)
			     (tag=tagging? <ratnum>  b)
			     (tag=tagging? <fixnums> rest))))
	  (values a b rest (inspect)))
	(fun 1.1 2/3 3 4))
    => 1.1 2/3 '(3 4) '(#t #t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings and tagged return values

  (check	;tagged args, UNtagged return values
      (let ()
	(define (fun . {args <fixnums>})
	  (define-syntax (inspect stx)
	    (tag=tagging? <fixnums> args))
	  (define-syntax (signature stx)
	    (syntax=? (typ.identifier-function-signature #'fun)
		      #'(() . <fixnums>)))
	  (values args (inspect) (signature)))
	(fun 1 2 3))
    => '(1 2 3) #t #t)

  (check	;tagged args, tagged return value
      (with-result
       (let ()
	 (define ({fun <fixnum>} . {args <fixnums>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <procedure> fun)
			      (tag=tagging? <fixnums> args))))
	   (define-syntax (signature stx)
	     (syntax=? (typ.identifier-function-signature #'fun)
		       #'((<fixnum>) . <fixnums>)))
	   (add-result (inspect))
	   (add-result (signature))
	   (apply + args))
	 (fun 1 10 100)))
    => '(111 ((#t #t) #t)))

  (check	;tagged args, tagged return value
      (with-result
       (let ()
	 (define ({fun <fixnum> <flonum>} . {args <fixnums>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <procedure> fun)
			      (tag=tagging? <fixnums> args))))
	   (define-syntax (signature stx)
	     (syntax=? (typ.identifier-function-signature #'fun)
		       #'((<fixnum> <flonum>) . <fixnums>)))
	   (add-result (inspect))
	   (add-result (signature))
           (list (apply + args) 2.2))
	 (fun 1 10 100)))
    => '((111 2.2) ((#t #t) #t)))

  (check	;tagged args, single tagged return values
      (with-result
       (let ()
	 (define ({fun <fixnum>} {a <fixnum>} {b <fixnum>} {c <fixnum>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <procedure> fun)
			      (tag=tagging? <fixnum> a)
			      (tag=tagging? <fixnum> b)
			      (tag=tagging? <fixnum> c))))
	   (define-syntax (signature stx)
	     (syntax=? (typ.identifier-function-signature #'fun)
		       #'((<fixnum>) . (<fixnum> <fixnum> <fixnum>))))
	   (add-result (inspect))
	   (add-result (signature))
	   (+ a b c))
	 (fun 1 10 100)))
    => '(111 ((#t #t #t #t) #t)))

  #t)


(parametrise ((check-test-name	'tagged-bindings-case-lambda))

;;;untagged bindings

  (check
      ((case-lambda
	(args
	 (define-syntax (inspect stx)
	   (top-tagged? args))
	 (values args (inspect))))
       1)
    => '(1) #t)

  (check
      ((case-lambda
	((a)
	 (define-syntax (inspect stx)
	   (top-tagged? a))
	 (values a (inspect))))
       1)
    => 1 #t)

  (check
      ((case-lambda
	((a b)
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b))))
	 (values a b (inspect))))
       1 2)
    => 1 2 '(#t #t))

  (check
      ((case-lambda
	((a b . rest)
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b)
			    (top-tagged? rest))))
	 (values a b rest (inspect))))
       1 2 3 4)
    => 1 2 '(3 4) '(#t #t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      ((case-lambda
	({args <fixnums>}
	 (define-syntax (inspect stx)
	   (tag=tagging? <fixnums> args))
	 (values args (inspect))))
       1 2 3)
    => '(1 2 3) #t)

  (check
      ((case-lambda
	(({a <flonum>})
	 (define-syntax (inspect stx)
	   (tag=tagging? <flonum> a))
	 (values a (inspect))))
       1.1)
    => 1.1 #t)

  (check
      ((case-lambda
	(({a <flonum>} {b <ratnum>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (tag=tagging? <flonum> a)
			    (tag=tagging? <ratnum> b))))
	 (values a b (inspect))))
       1.1 2/3)
    => 1.1 2/3 '(#t #t))

  (check
      ((case-lambda
	(({a <flonum>} {b <ratnum>} . {rest <fixnums>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (tag=tagging? <flonum>  a)
			    (tag=tagging? <ratnum>  b)
			    (tag=tagging? <fixnums> rest))))
	 (values a b rest (inspect))))
       1.1 2/3 3 4)
    => 1.1 2/3 '(3 4) '(#t #t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-let))

;;; untagged bindings

  (check
      (let ((a 1))
	(define-syntax (inspect stx)
	  (top-tagged? a))
	(values a (inspect)))
    => 1 #t)

  (check
      (let ((a 1)
	    (b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (top-tagged? a)
			   (top-tagged? b))))
	(values a (inspect)))
    => 1 '(#t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (let (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check
      (let (({a <fixnum>} 1)
	    ({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <flonum> b))))
	(values a (inspect)))
    => 1 '(#t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-trace-let))

;;; untagged bindings

  (check
      (trace-let name ((a 1))
	(define-syntax (inspect stx)
	  (top-tagged? a))
	(values a (inspect)))
    => 1 #t)

  (check
      (trace-let name ((a 1)
		       (b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (top-tagged? a)
			   (top-tagged? b))))
	(values a (inspect)))
    => 1 '(#t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (trace-let name (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check
      (trace-let name (({a <fixnum>} 1)
		       ({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <flonum> b))))
	(values a (inspect)))
    => 1 '(#t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-let-star))

;;; untagged bindings

  (check
      (let* ((a 1))
	(define-syntax (inspect stx)
	  (top-tagged? a))
	(values a (inspect)))
    => 1 #t)

  (check
      (let* ((a 1)
	     (b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (top-tagged? a)
			   (top-tagged? b))))
	(values a (inspect)))
    => 1 '(#t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (let* (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check
      (let* (({a <fixnum>} 1)
	     ({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <flonum> b))))
	(values a (inspect)))
    => 1 '(#t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-letrec))

;;; untagged bindings

  (check
      (letrec ((a 1))
	(define-syntax (inspect stx)
	  (top-tagged? a))
	(values a (inspect)))
    => 1 #t)

  (check
      (letrec ((a 1)
	       (b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (top-tagged? a)
			   (top-tagged? b))))
	(values a (inspect)))
    => 1 '(#t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (letrec (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check
      (letrec (({a <fixnum>} 1)
	       ({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <flonum> b))))
	(values a (inspect)))
    => 1 '(#t #t))

  (check
      (letrec (({a <procedure>} (lambda (x)
				  (define-syntax (inspect stx)
				    (tag=tagging? <procedure> a))
				  (values x (inspect)))))
	(a 1))
    => 1 #t)

  #t)


(parametrise ((check-test-name	'tagged-bindings-letrec-star))

;;; untagged bindings

  (check
      (letrec* ((a 1))
	(define-syntax (inspect stx)
	  (top-tagged? a))
	(values a (inspect)))
    => 1 #t)

  (check
      (letrec* ((a 1)
		(b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (top-tagged? a)
			   (top-tagged? b))))
	(values a (inspect)))
    => 1 '(#t #t))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (letrec* (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check
      (letrec* (({a <fixnum>} 1)
		({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <flonum> b))))
	(values a (inspect)))
    => 1 '(#t #t))

  (check
      (letrec* (({a <procedure>} (lambda (x)
				   (define-syntax (inspect stx)
				     (tag=tagging? <procedure> a))
				   (values x (inspect)))))
	(a 1))
    => 1 #t)

  #t)


(parametrise ((check-test-name	'tagged-bindings-define-values))

;;; untagged bindings

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

  (check	;recursive binding
      (with-result
       (let ()
	 (define-values (f)
	   (lambda (arg)
	     (if (positive? arg)
		 (begin
		   (add-result arg)
		   (f (- arg 1)))
	       arg)))
	 (f 2)))
    => '(0 (2 1)))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (with-result
       (let ()
  	 (define-values ({a <fixnum>})
  	   (define-syntax (inspect stx)
  	     (tag=tagging? <fixnum> a))
  	   (add-result (inspect))
  	   1)
  	 a))
    => '(1 (#t)))

  (check
      (with-result
       (let ()
	 (define-values ({a <fixnum>} {b <flonum>} {c <ratnum>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <flonum> b)
			      (tag=tagging? <ratnum> c))))
	   (add-result (inspect))
	   (values 1 2.2 3/4))
	 (vector a b c)))
    => '(#(1 2.2 3/4) ((#t #t #t))))

  (check
      (with-result
       (let (({a <flonum>} 2.2))
	 (define-values ({a <fixnum>})
	   (define-syntax (inspect stx)
	     (tag=tagging? <fixnum> a))
	   (add-result (inspect))
	   1)
	 a))
    => '(1 (#t)))

  #t)


(parametrise ((check-test-name	'tagged-bindings-do))

;;; untagged bindings

  (check
      (with-result
       (do ((a 1 (+ 1 a)))
	   ((= 2 a)
	    a)
	 (let ()
	   (define-syntax (inspect stx)
	     (top-tagged? a))
	   (add-result (inspect)))))
    => '(2 (#t)))

  (check
      (with-result
       (do ((a 1 (+ 1 a))
	    (b 9))
	   ((= 2 a)
	    (vector a b))
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? b))))
	   (add-result (inspect)))))
    => '(#(2 9) ((#t #t))))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (with-result
       (do (({a <fixnum>} 1 (+ 1 a)))
	   ((= 2 a)
	    a)
	 (let ()
	   (define-syntax (inspect stx)
	     (tag=tagging? <fixnum> a))
	   (add-result (inspect)))))
    => '(2 (#t)))

  (check
      (with-result
       (do (({a <fixnum>} 1 (+ 1 a))
	    ({b <flonum>} 2.2))
	   ((= 2 a)
	    (vector a b))
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <flonum> b))))
	   (add-result (inspect)))))
    => '(#(2 2.2) ((#t #t))))

  #t)


(parametrise ((check-test-name	'tagged-bindings-receive))

  (check
      (with-result
       (receive (a)
	   1
	 (let ()
	   (define-syntax (inspect stx)
	     (top-tagged? a))
	   (add-result (inspect))
	   a)))
    => '(1 (#t)))

  (check
      (with-result
       (receive (a b c)
	   (values 1 2 3)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? b)
			      (top-tagged? c))))
	   (add-result (inspect))
	   (list a b c))))
    => '((1 2 3) ((#t #t #t))))

  (check
      (with-result
       (receive args
	   (values 1 2 3)
	 (let ()
	   (define-syntax (inspect stx)
	     (top-tagged? args))
	   (add-result (inspect))
	   args)))
    => '((1 2 3) (#t)))

  (check
      (with-result
       (receive (a b c . args)
	   (values 1 2 3 4 5)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? b)
			      (top-tagged? c)
			      (top-tagged? args))))
	   (add-result (inspect))
	   (list a b c args))))
    => '((1 2 3 (4 5)) ((#t #t #t #t))))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (receive ({a <fixnum>})
	   1
	 (let ()
	   (define-syntax (inspect stx)
	     (tag=tagging? <fixnum> a))
	   (add-result (inspect))
	   a)))
    => '(1 (#t)))

  (check
      (with-result
       (receive ({a <fixnum>} {b <flonum>} {c <ratnum>})
	   (values 1 2.2 3/4)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <flonum> b)
			      (tag=tagging? <ratnum> c))))
	   (add-result (inspect))
	   (list a b c))))
    => '((1 2.2 3/4) ((#t #t #t))))

  (check
      (with-result
       (receive {args <list>}
	   (values 1 2 3)
	 (let ()
	   (define-syntax (inspect stx)
	     (tag=tagging? <list> args))
	   (add-result (inspect))
	   args)))
    => '((1 2 3) (#t)))

  (check
      (with-result
       (receive ({a <fixnum>} {b <flonum>} {c <ratnum>} . {args <fixnums>})
	   (values 1 2.2 3/4 4 5)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <flonum> b)
			      (tag=tagging? <ratnum> c)
			      (tag=tagging? <fixnums> args))))
	   (add-result (inspect))
	   (list a b c args))))
    => '((1 2.2 3/4 (4 5)) ((#t #t #t #t))))

  #t)


(parametrise ((check-test-name	'tagged-bindings-receive-and-return))

  (check
      (with-result
       (receive-and-return (a)
	   1
	 (let ()
	   (define-syntax (inspect stx)
	     (top-tagged? a))
	   (add-result (inspect)))))
    => '(1 (#t)))

  (check
      (with-result
       (receive-and-return (a b c)
	   (values 1 2 3)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? b)
			      (top-tagged? c))))
	   (add-result (inspect)))))
    => '(1 2 3 ((#t #t #t))))

  (check
      (with-result
       (receive-and-return args
	   (values 1 2 3)
	 (let ()
	   (define-syntax (inspect stx)
	     (top-tagged? args))
	   (add-result (inspect)))))
    => '((1 2 3) (#t)))

  (check
      (with-result
       (receive-and-return (a b c . args)
	   (values 1 2 3 4 5)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? b)
			      (top-tagged? c)
			      (top-tagged? args))))
	   (add-result (inspect)))))
    => '(1 2 3 (4 5) ((#t #t #t #t))))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (receive-and-return ({a <fixnum>})
	   1
	 (let ()
	   (define-syntax (inspect stx)
	     (tag=tagging? <fixnum> a))
	   (add-result (inspect)))))
    => '(1 (#t)))

  (check
      (with-result
       (receive-and-return ({a <fixnum>} {b <flonum>} {c <ratnum>})
	   (values 1 2.2 3/4)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <flonum> b)
			      (tag=tagging? <ratnum> c))))
	   (add-result (inspect)))))
    => '(1 2.2 3/4 ((#t #t #t))))

  (check
      (with-result
       (receive-and-return {args <list>}
	   (values 1 2 3)
	 (let ()
	   (define-syntax (inspect stx)
	     (tag=tagging? <list> args))
	   (add-result (inspect)))))
    => '((1 2 3) (#t)))

  (check
      (with-result
       (receive-and-return ({a <fixnum>} {b <flonum>} {c <ratnum>} . {args <fixnums>})
	   (values 1 2.2 3/4 4 5)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <flonum> b)
			      (tag=tagging? <ratnum> c)
			      (tag=tagging? <fixnums> args))))
	   (add-result (inspect)))))
    => '(1 2.2 3/4 (4 5) ((#t #t #t #t))))

  #t)


(parametrise ((check-test-name	'tagged-bindings-define-inline))

;;; untyped

  (check
      (with-result
       (let ()
	 (define-inline (ciao a b)
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? b))))
	   (add-result (inspect))
	   (+ a b))
	 (ciao 1 2)))
    => '(3 ((#t #t))))

  (check
      (let ()
	(define-inline (ciao)
	  (+ 1 2))
	(ciao))
    => 3)

  (check
      (with-result
       (let ()
	 (define-inline (ciao . rest)
	   (define-syntax (inspect stx)
	     (top-tagged? rest))
	   (add-result (inspect))
	   (apply + rest))
	 (ciao 1 2)))
    => '(3 (#t)))

  (check
      (with-result
       (let ()
	 (define-inline (ciao a . rest)
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? rest))))
	   (add-result (inspect))
	   (apply + a rest))
	 (ciao 1 2)))
    => '(3 ((#t #t))))

;;; --------------------------------------------------------------------
;;; typed

  (check
      (with-result
       (let ()
	 (define-inline (ciao {a <fixnum>} {b <flonum>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <flonum> b))))
	   (add-result (inspect))
	   (+ a b))
	 (ciao 1 2.2)))
    => '(3.2 ((#t #t))))

  (check
      (with-result
       (let ()
	 (define-inline (ciao . {rest <fixnums>})
	   (define-syntax (inspect stx)
	     (tag=tagging? <fixnums> rest))
	   (add-result (inspect))
	   (apply + rest))
	 (ciao 1 2)))
    => '(3 (#t)))

  (check
      (with-result
       (let ()
	 (define-inline (ciao {a <fixnum>} . {rest <fixnums>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <fixnums> rest))))
	   (add-result (inspect))
	   (apply + a rest))
	 (ciao 1 2)))
    => '(3 ((#t #t))))

  #t)


(parametrise ((check-test-name	'tagged-bindings-define-integrable))

  (check
      (with-result
       (let ()
	 (define-integrable (fact n)
	   (define-syntax (inspect stx)
	     (top-tagged? n))
	   (add-result (inspect))
	   (if (< n 2)
	       1
	     (* n (fact (- n 1)))))
	 (fact 5)))
    => '(120 (#t #t #t #t #t)))

  (check	;here the integrable is never called
      (let ()
	(define-integrable (f x)
	  (+ x 1))
	(eq? f f))
    => #t)

  (check
      (with-result
       (let ()
	 (define-integrable (even? n)
	   (define-syntax (inspect stx)
	     (top-tagged? n))
	   (add-result (inspect))
	   (or (zero? n) (odd? (- n 1))))
	 (define-integrable (odd? n)
	   (define-syntax (inspect stx)
	     (top-tagged? n))
	   (add-result (inspect))
	   (not (even? n)))
	 (even? 5)))
    => '(#f (#t #t #t #t #t #t #t #t #t #t #t)))

  (check
      (with-result
       (let ()
	 (define-integrable (incr x)
	   (define-syntax (inspect stx)
	     (top-tagged? x))
	   (add-result (inspect))
	   (+ x 1))
	 (map incr '(10 20 30))))
    => '((11 21 31) (#t #t #t)))

  #t)


(parametrise ((check-test-name	'tagged-bindings-let-values))

  (check	;no bindings
      (let-values ()
	1)
    => 1)

  (check	;special case, var specification is a symbol
      (with-result
       (let-values ((a (values 1 2 3)))
	 (define-syntax (inspect stx)
	   (top-tagged? a))
	 (add-result (inspect))
	 a))
    => '((1 2 3) (#t)))

;;; --------------------------------------------------------------------

  (check	;single binding, single id, no tags
      (with-result
       (let-values (((a) 1))
	 (define-syntax (inspect stx)
	   (top-tagged? a))
	 (add-result (inspect))
	 a))
    => '(1 (#t)))

  (check	;single binding, multiple ids, no tags
      (with-result
       (let-values (((a b c) (values 1 2 3)))
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b)
			    (top-tagged? c))))
	 (add-result (inspect))
	 (list a b c)))
    => '((1 2 3) ((#t #t #t))))

  (check	;multiple bindings, single id, no tags
      (with-result
       (let-values (((a) 1)
		    ((b) 2)
		    ((c) 3))
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b)
			    (top-tagged? c))))
	 (add-result (inspect))
	 (list a b c)))
    => '((1 2 3) ((#t #t #t))))

  (check	;multiple bindings, multiple ids, no tags
      (with-result
       (let-values (((a b c) (values 1 2 3))
		    ((d e f) (values 4 5 6))
		    ((g h i) (values 7 8 9)))
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b)
			    (top-tagged? c)
			    (top-tagged? d)
			    (top-tagged? e)
			    (top-tagged? f)
			    (top-tagged? g)
			    (top-tagged? h)
			    (top-tagged? i))))
	 (add-result (inspect))
	 (list a b c d e f g h i)))
    => '((1 2 3 4 5 6 7 8 9) ((#t #t #t  #t #t #t  #t #t #t))))

  (check	;mixed bindings, no tags
      (with-result
       (let-values (((a)	1)
		    ((d)	4)
		    ((g h i)	(values 7 8 9)))
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? d)
			    (top-tagged? g)
			    (top-tagged? h)
			    (top-tagged? i))))
	 (add-result (inspect))
	 (list a d g h i)))
    => '((1 4 7 8 9) ((#t #t #t #t #t))))

  (check	;mixed bindings, no tags
      (with-result
       (let-values (((a b c)	(values 1 2 3))
		    ((d)	4)
		    ((g h i)	(values 7 8 9)))
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b)
			    (top-tagged? c)
			    (top-tagged? d)
			    (top-tagged? g)
			    (top-tagged? h)
			    (top-tagged? i))))
	 (add-result (inspect))
	 (list a b c  d  g h i)))
    => '((1 2 3 4 7 8 9) ((#t #t #t  #t  #t #t #t))))

  (check	;mixed bindings, no tags
      (with-result
       (let-values (((a b c)	(values 1 2 3))
		    ((d)	4)
		    ((g)	7))
	 (define-syntax (inspect stx)
	   #`(quote #,(list (top-tagged? a)
			    (top-tagged? b)
			    (top-tagged? c)
			    (top-tagged? d)
			    (top-tagged? g))))
	 (add-result (inspect))
	 (list a b c d g)))
    => '((1 2 3 4 7) ((#t #t #t #t #t))))

;;; --------------------------------------------------------------------
;;; tagged

  (check	;single binding, single id, single tag
      (with-result
       (let-values ((({a <fixnum>}) 1))
	 (define-syntax (inspect stx)
	   (tag=tagging? <fixnum> a))
	 (add-result (inspect))
	 a))
    => '(1 (#t)))

  (check	;single binding, multiple ids, single tag
      (with-result
       (let-values ((({a <fixnum>} {b <fixnum>} {c <fixnum>}) (values 1 2 3)))
	 (define-syntax (inspect stx)
	   #`(quote #,(list (tag=tagging? <fixnum> a)
			    (tag=tagging? <fixnum> b)
			    (tag=tagging? <fixnum> c))))
	 (add-result (inspect))
	 (values a b c)))
    => '(1 2 3 ((#t #t #t))))

  (check	;multiple bindings, single id, single tags
      (let-values ((({a <fixnum>}) 1)
		   (({b <fixnum>}) 2)
		   (({c <fixnum>}) 3))
  	(values a b c))
    => 1 2 3)

  (check	;multiple bindings, multiple ids, with tags
      (let-values ((({a <fixnum>} {b <fixnum>} {c <fixnum>}) (values 1 2 3))
		   (({d <fixnum>} {e <fixnum>} {f <fixnum>}) (values 4 5 6))
		   (({g <fixnum>} {h <fixnum>} {i <fixnum>}) (values 7 8 9)))
  	(values a b c d e f g h i))
    => 1 2 3 4 5 6 7 8 9)

  (check	;mixed bindings, with tags
      (let-values ((({a <fixnum>})	1)
		   ((d)			4)
		   ((g {h <fixnum>} i)	(values 7 8 9)))
  	(values a d g h i))
    => 1 4 7 8 9)

  (check	;mixed bindings, with tags
      (let-values ((({a <fixnum>})			1)
		   ((d)					4)
		   (({g <fixnum>} {h <fixnum>} i)	(values 7 8 9)))
  	(values a d g h i))
    => 1 4 7 8 9)

;;; --------------------------------------------------------------------

  (check	;multiple bindings, scope rules
      (let ((a 4) (b 5) (c 6))
	(let-values ((({a <fixnum>} {b <fixnum>} {c <fixnum>}) (values 1 2 3))
		     (({d <fixnum>} {e <fixnum>} {f <fixnum>}) (values a b c))
		     (({g <fixnum>} {h <fixnum>} {i <fixnum>}) (values 7 8 9)))
	  (values a b c d e f g h i)))
    => 1 2 3 4 5 6 7 8 9)

;;; --------------------------------------------------------------------

  (check	;error, invalid syntax for list of bindings
      (catch-syntax-violation #f
	(%eval '(let-values #(((a) 1))
		  a)))
    => #f)

  (check	;error, invalid syntax in single binding
      (catch-syntax-violation #f
	(%eval '(let-values (((a) 1) #(b 2))
		  a)))
    => #f)

  (check	;error, duplicate identifiers
      (catch-syntax-violation #f
	(%eval '(let-values (((a {a <fixnum>}) (values 1 2)))
		  a)))
    => 'a)

  (check	;error, duplicate identifiers
      (catch-syntax-violation #f
	(%eval '(let-values (((a) 1) ((a) 2))
		  a)))
    => 'a)

  #t)


(parametrise ((check-test-name	'tagged-bindings-let*-values))

  (check	;no bindings
      (let*-values ()
	1)
    => 1)

  (check	;special case, var specification is a symbol
      (with-result
       (let*-values ((a (values 1 2 3)))
	 (define-syntax (inspect stx)
	   (top-tagged? a))
	 (add-result (inspect))
	 a))
    => '((1 2 3) (#t)))

;;; --------------------------------------------------------------------

  (check	;single binding, single id, no tags
      (let*-values (((a) 1))
	a)
    => 1)

  (check	;single binding, multiple ids, no tags
      (let*-values (((a b c) (values 1 2 3)))
	(list a b c))
    => '(1 2 3))

  (check	;multiple bindings, single id, no tags
      (let*-values (((a) 1)
		    ((b) 2)
		    ((c) 3))
	(list a b c))
    => '(1 2 3))

  (check	;multiple bindings, multiple ids, no tags
      (let*-values (((a b c) (values 1 2 3))
		    ((d e f) (values 4 5 6))
		    ((g h i) (values 7 8 9)))
	(list a b c d e f g h i))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;mixed bindings, no tags
      (let*-values (((a)	1)
		    ((d)	4)
		    ((g h i)	(values 7 8 9)))
	(list a d g h i))
    => '(1 4 7 8 9))

  (check	;mixed bindings, no tags
      (let*-values (((a b c)	(values 1 2 3))
		    ((d)	4)
		    ((g h i)	(values 7 8 9)))
	(list a b c  d  g h i))
    => '(1 2 3 4 7 8 9))

  (check	;mixed bindings, no tags
      (let*-values (((a b c)	(values 1 2 3))
		    ((d)	4)
		    ((g)	7))
	(list a b c d g))
    => '(1 2 3 4 7))

  (check	;correct duplicate identifiers, no tags
      (let*-values (((a) 1)
		    ((a) 2))
	a)
    => 2)

;;; --------------------------------------------------------------------

  (check	;single binding, single id, single tag
      (with-result
       (let*-values ((({a <fixnum>}) 1))
	 (define-syntax (inspect stx)
	   (tag=tagging? <fixnum> a))
	 (add-result (inspect))
	 a))
    => '(1 (#t)))

  (check	;single binding, multiple ids, single tag
      (let*-values ((({a <fixnum>} {b <fixnum>} {c <fixnum>}) (values 1 2 3)))
	(values a b c))
    => 1 2 3)

  (check	;multiple bindings, single id, single tags
      (let*-values ((({a <fixnum>}) 1)
		    (({b <fixnum>}) 2)
		    (({c <fixnum>}) 3))
	(values a b c))
    => 1 2 3)

  (check	;multiple bindings, multiple ids, with tags
      (let*-values ((({a <fixnum>} {b <fixnum>} {c <fixnum>}) (values 1 2 3))
		    (({d <fixnum>} {e <fixnum>} {f <fixnum>}) (values 4 5 6))
		    (({g <fixnum>} {h <fixnum>} {i <fixnum>}) (values 7 8 9)))
	(values a b c d e f g h i))
    => 1 2 3 4 5 6 7 8 9)

  (check	;mixed bindings, with tags
      (let*-values ((({a <fixnum>})	1)
		    ((d)		4)
		    ((g {h <fixnum>} i)	(values 7 8 9)))
	(values a d g h i))
    => 1 4 7 8 9)

  (check	;mixed bindings, with tags
      (let*-values ((({a <fixnum>})			1)
		    ((d)				4)
		    (({g <fixnum>} {h <fixnum>} i)	(values 7 8 9)))
	(values a d g h i))
    => 1 4 7 8 9)

  (check	;mixed bindings, with tags
      (let*-values ((({a <fixnum>})			1)
		    ((d)				4)
		    ((g {h <fixnum>} {i <fixnum>})	(values 7 8 9)))
	(values a d g h i))
    => 1 4 7 8 9)

;;; --------------------------------------------------------------------

  (check	;multiple bindings, scope rules
      (let ((a 4) (b 5) (c 6))
	(let*-values ((({a <fixnum>} {b <fixnum>} {c <fixnum>}) (values 1 2 3))
		      (({d <fixnum>} {e <fixnum>} {f <fixnum>}) (values a b c))
		      (({g <fixnum>} {h <fixnum>} {i <fixnum>}) (values 7 8 9)))
	  (values  a b c d e f g h i)))
    => 1 2 3 1 2 3 7 8 9)

  (check	;multiple bindings, scope rules
      (let ((a 4) (b 5) (c 6))
	(let*-values ((({a <fixnum>} {b <fixnum>} {c <fixnum>}) (values 1 2 3))
		      (({d <fixnum>} {e <fixnum>} {f <fixnum>}) (values (- a) (- b) (- c)))
		      (({g <fixnum>} {h <fixnum>} {i <fixnum>}) (values 7 8 9)))
	  (values a b c d e f g h i)))
    => 1 2 3 -1 -2 -3 7 8 9)

  (check	;correct duplicate identifiers, no tags
      (let*-values ((({a <fixnum>}) 1)
		    (({a <fixnum>}) 2))
	a)
    => 2)

;;; --------------------------------------------------------------------

  (check	;error, invalid syntax for list of bindings
      (catch-syntax-violation #f
	(%eval '(let*-values #(((a) 1))
		  a)))
    => #f)

  (check	;error, invalid syntax in single binding
      (catch-syntax-violation #f
	(%eval '(let*-values (((a) 1) #(b 2))
		  a)))
    => #f)

  (check	;error, duplicate identifiers
      (catch-syntax-violation #f
	(%eval '(let*-values (((a {a <fixnum>}) (values 1 2)))
		  a)))
    => 'a)

  #t)


(parametrise ((check-test-name	'slot-ref-set-structs))

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(list (slot-ref O a)
	      (slot-ref O b)
	      (slot-ref O c)))
    => '(1 2 3))

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(slot-set! O a 11)
	(slot-set! O b 22)
	(slot-set! O c 33)
	(list (slot-ref O a)
	      (slot-ref O b)
	      (slot-ref O c)))
    => '(11 22 33))

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(list ($slot-ref O a)
	      ($slot-ref O b)
	      ($slot-ref O c)))
    => '(1 2 3))

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	($slot-set! O a 11)
	($slot-set! O b 22)
	($slot-set! O c 33)
	(list ($slot-ref O a)
	      ($slot-ref O b)
	      ($slot-ref O c)))
    => '(11 22 33))

  #t)


(parametrise ((check-test-name	'slot-ref-set-records))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(list (slot-ref O a)
	      (slot-ref O b)
	      (slot-ref O c)))
    => '(1 2 3))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(slot-set! O a 11)
	(slot-set! O b 22)
	(slot-set! O c 33)
	(list (slot-ref O a)
	      (slot-ref O b)
	      (slot-ref O c)))
    => '(11 22 33))

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(list ($slot-ref O a)
	      ($slot-ref O b)
	      ($slot-ref O c)))
    => '(1 2 3))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	($slot-set! O a 11)
	($slot-set! O b 22)
	($slot-set! O c 33)
	(list ($slot-ref O a)
	      ($slot-ref O b)
	      ($slot-ref O c)))
    => '(11 22 33))

;;; --------------------------------------------------------------------
;;; some immutable fields below

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(list (slot-ref O a)
	      (slot-ref O b)
	      (slot-ref O c)))
    => '(1 2 3))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a)
		  (immutable b)
		  (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(slot-set! O a 11)
	#;(slot-set! O b 22)
	(slot-set! O c 33)
	(list (slot-ref O a)
	      (slot-ref O b)
	      (slot-ref O c)))
    => '(11 2 33))

;;; --------------------------------------------------------------------
;;; searching the slot of the parent

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable   a1)
		  (immutable a2)
		  (mutable   a3)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable   b1)
		  (immutable b2)
		  (mutable   b3)))
	(define {O beta}
	  (make-beta 1 2 3 4 5 6))
	(list (slot-ref O a1) (slot-ref O a2) (slot-ref O a3)
	      (slot-ref O b1) (slot-ref O b2) (slot-ref O b3)))
    => '(1 2 3 4 5 6))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable   a1)
		  (immutable a2)
		  (mutable   a3)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable   b1)
		  (immutable b2)
		  (mutable   b3)))
	(define {O beta}
	  (make-beta 1 2 3 4 5 6))
	(slot-set! O a1 11)
	#;(slot-set! O a2 22)
	(slot-set! O a3 33)
	(slot-set! O b1 44)
	#;(slot-set! O b2 55)
	(slot-set! O b3 66)
	(list (slot-ref O a1) (slot-ref O a2) (slot-ref O a3)
	      (slot-ref O b1) (slot-ref O b2) (slot-ref O b3)))
    => '(11 2 33 44 5 66))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable   a1)
		  (immutable a2)
		  (mutable   a3)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable   b1)
		  (immutable b2)
		  (mutable   b3)))
	(define {O beta}
	  (make-beta 1 2 3 4 5 6))
	(list ($slot-ref O a1) ($slot-ref O a2) ($slot-ref O a3)
	      ($slot-ref O b1) ($slot-ref O b2) ($slot-ref O b3)))
    => '(1 2 3 4 5 6))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable   a1)
		  (immutable a2)
		  (mutable   a3)))
	(define-record-type beta
	  (parent alpha)
	  (fields (mutable   b1)
		  (immutable b2)
		  (mutable   b3)))
	(define {O beta}
	  (make-beta 1 2 3 4 5 6))
	($slot-set! O a1 11)
	#;($slot-set! O a2 22)
	($slot-set! O a3 33)
	($slot-set! O b1 44)
	#;($slot-set! O b2 55)
	($slot-set! O b3 66)
	(list ($slot-ref O a1) ($slot-ref O a2) ($slot-ref O a3)
	      ($slot-ref O b1) ($slot-ref O b2) ($slot-ref O b3)))
    => '(11 2 33 44 5 66))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'typ.set-identifier-type-spec! 'scheme-indent-function 1)
;; eval: (put 'catch-syntax-violation 'scheme-indent-function 1)
;; End:
