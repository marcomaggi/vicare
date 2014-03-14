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
  (prefix (vicare expander object-spec) typ.)
  (vicare expander tags)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: expand-time types\n")


;;;; helpers

(begin-for-syntax
  (define-syntax-rule (tag=tagging? ?tag ?var)
    (free-identifier=? #'?tag (typ.identifier-type-tagging #'?var)))
  (define-syntax-rule (top-tagged? ?var)
    (tag=tagging? <top> ?var)))


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


(parametrise ((check-test-name	'tagged-bindings-let-values))

  #t)


(parametrise ((check-test-name	'tagged-bindings-let*-values))



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


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; End:
