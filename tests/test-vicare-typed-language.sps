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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: expand-time types\n")


;;;; helpers

(define-auxiliary-syntaxes
  <fixnum> <flonum> <ratnum> <string> <list>
  <fixnums>)


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
    #'X #f)

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
    #'(a) #'(#f))

  (check
      (typ.parse-tagged-bindings #'(a b))
    (=> syntax=?)
    #'(a b) #'(#f #f))

  (check
      (typ.parse-tagged-bindings #'(a b c))
    (=> syntax=?)
    #'(a b c) #'(#f #f #f))

;;;

  (check
      (typ.parse-tagged-bindings #'(a
				    {b <string>}))
    (=> syntax=?)
    #'(a b) #'(#f <string>))

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}
				    b))
    (=> syntax=?)
    #'(a b) #'(<fixnum> #f))

  (check
      (typ.parse-tagged-bindings #'(a
				    {b <string>}
				    {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(#f <string> <vector>))

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}
				    b
				    {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> #f <vector>))

  (check
      (typ.parse-tagged-bindings #'({a <fixnum>}
				    {b <string>}
				    c))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <string> #f))

;;; --------------------------------------------------------------------
;;; tagged formals

;;; tagged

  (check
      (typ.parse-tagged-formals #'({a <fixnum>}
				   {b <string>}))
    (=> syntax=?)
    #'(a b) #'(<fixnum> <string>))

  (check
      (typ.parse-tagged-formals #'({a <fixnum>}
				   {b <string>}
				   {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <string> <vector>))

;;; untagged

  (check
      (typ.parse-tagged-formals #'(a))
    (=> syntax=?)
    #'(a) #'(#f))

  (check
      (typ.parse-tagged-formals #'(a b))
    (=> syntax=?)
    #'(a b) #'(#f #f))

  (check
      (typ.parse-tagged-formals #'(a b c))
    (=> syntax=?)
    #'(a b c) #'(#f #f #f))

;;; mixed tagged and untagged

  (check
      (typ.parse-tagged-formals #'(a
				   {b <string>}))
    (=> syntax=?)
    #'(a b) #'(#f <string>))

  (check
      (typ.parse-tagged-formals #'({a <fixnum>}
				   b))
    (=> syntax=?)
    #'(a b) #'(<fixnum> #f))

  (check
      (typ.parse-tagged-formals #'(a
				   {b <string>}
				   {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(#f <string> <vector>))

  (check
      (typ.parse-tagged-formals #'({a <fixnum>}
				   b
				   {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> #f <vector>))

  (check
      (typ.parse-tagged-formals #'({a <fixnum>}
				   {b <string>}
				   c))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <string> #f))

;;; args argument

  (check	;tagged args argument
      (typ.parse-tagged-formals #'{args <fixnums>})
    (=> syntax=?)
    #'args #'<fixnums>)

  (check	;UNtagged args argument
      (typ.parse-tagged-formals #'args)
    (=> syntax=?)
    #'args #f)

;;; rest argument

  (check	;tagged rest
      (typ.parse-tagged-formals #'({a <fixnum>} . {rest <fixnums>}))
    (=> syntax=?)
    #'(a . rest) #'(<fixnum> . <fixnums>))

  (check	;UNtagged rest
      (typ.parse-tagged-formals #'({a <fixnum>} . rest))
    (=> syntax=?)
    #'(a . rest) #'(<fixnum> . #f))

  (check	;tagged rest
      (typ.parse-tagged-formals #'({a <fixnum>} {b <string>} . {rest <fixnums>}))
    (=> syntax=?)
    #'(a b . rest) #'(<fixnum> <string> . <fixnums>))

  (check	;UNtagged rest
      (typ.parse-tagged-formals #'({a <fixnum>} {b <string>} . rest))
    (=> syntax=?)
    #'(a b . rest) #'(<fixnum> <string> . #f))

;;; --------------------------------------------------------------------
;;; tagged formals predicate

  (check-for-true
   (typ.tagged-formals? #'({a <fixnum>} {b <string>})))

  #t)


(parametrise ((check-test-name	'tagged-bindings-lambda))

;;;untagged bindings

  (check
      ((lambda args
	 (define-syntax (inspect stx)
	   (typ.identifier-type-tagging #'args))
	 (values args (inspect)))
       1)
    => '(1) #f)

  (check
      ((lambda (a)
	 (define-syntax (inspect stx)
	   (typ.identifier-type-tagging #'a))
	 (values a (inspect)))
       1)
    => 1 #f)

  (check
      ((lambda (a b)
	 (define-syntax (inspect stx)
	   #`(quote #,(list (typ.identifier-type-tagging #'a)
			    (typ.identifier-type-tagging #'b))))
	 (values a b (inspect)))
       1 2)
    => 1 2 '(#f #f))

  (check
      ((lambda (a b . rest)
	 (define-syntax (inspect stx)
	   #`(quote #,(list (typ.identifier-type-tagging #'a)
			    (typ.identifier-type-tagging #'b)
			    (typ.identifier-type-tagging #'rest))))
	 (values a b rest (inspect)))
       1 2 3 4)
    => 1 2 '(3 4) '(#f #f #f))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      ((lambda {args <fixnums>}
	 (define-syntax (inspect stx)
	   (free-identifier=? #'<fixnums> (typ.identifier-type-tagging #'args)))
	 (values args (inspect)))
       1 2 3)
    => '(1 2 3) #t)

  (check
      ((lambda ({a <flonum>})
	 (define-syntax (inspect stx)
	   (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'a)))
	 (values a (inspect)))
       1.1)
    => 1.1 #t)

  (check
      ((lambda ({a <flonum>} {b <ratnum>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'a))
			    (free-identifier=? #'<ratnum> (typ.identifier-type-tagging #'b)))))
	 (values a b (inspect)))
       1.1 2/3)
    => 1.1 2/3 '(#t #t))

  (check
      ((lambda ({a <flonum>} {b <ratnum>} . {rest <fixnums>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (free-identifier=? #'<flonum>  (typ.identifier-type-tagging #'a))
			    (free-identifier=? #'<ratnum>  (typ.identifier-type-tagging #'b))
			    (free-identifier=? #'<fixnums> (typ.identifier-type-tagging #'rest)))))
	 (values a b rest (inspect)))
       1.1 2/3 3 4)
    => 1.1 2/3 '(3 4) '(#t #t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-define))

;;;untagged bindings

  (check
      (let ()
	(define (fun . args)
	  (define-syntax (inspect stx)
	    (typ.identifier-type-tagging #'args))
	  (values args (inspect)))
	(fun 1))
    => '(1) #f)

  (check
      (let ()
	(define (fun a)
	  (define-syntax (inspect stx)
	    (typ.identifier-type-tagging #'a))
	  (values a (inspect)))
	(fun 1))
    => 1 #f)

  (check
      (let ()
	(define (fun a b)
	  (define-syntax (inspect stx)
	    #`(quote #,(list (typ.identifier-type-tagging #'a)
			     (typ.identifier-type-tagging #'b))))
	  (values a b (inspect)))
	(fun 1 2))
    => 1 2 '(#f #f))

  (check
      (let ()
	(define (fun a b . rest)
	  (define-syntax (inspect stx)
	    #`(quote #,(list (typ.identifier-type-tagging #'a)
			     (typ.identifier-type-tagging #'b)
			     (typ.identifier-type-tagging #'rest))))
	  (values a b rest (inspect)))
	(fun 1 2 3 4))
    => 1 2 '(3 4) '(#f #f #f))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (let ()
	(define (fun . {args <fixnums>})
	  (define-syntax (inspect stx)
	    (free-identifier=? #'<fixnums> (typ.identifier-type-tagging #'args)))
	  (values args (inspect)))
	(fun 1 2 3))
    => '(1 2 3) #t)

  (check
      (let ()
	(define (fun {a <flonum>})
	  (define-syntax (inspect stx)
	    (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'a)))
	  (values a (inspect)))
	(fun 1.1))
    => 1.1 #t)

  (check
      (let ()
	(define (fun {a <flonum>} {b <ratnum>})
	  (define-syntax (inspect stx)
	    #`(quote #,(list (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'a))
			     (free-identifier=? #'<ratnum> (typ.identifier-type-tagging #'b)))))
	  (values a b (inspect)))
	(fun 1.1 2/3))
    => 1.1 2/3 '(#t #t))

  (check
      (let ()
	(define (fun {a <flonum>} {b <ratnum>} . {rest <fixnums>})
	  (define-syntax (inspect stx)
	    #`(quote #,(list (free-identifier=? #'<flonum>  (typ.identifier-type-tagging #'a))
			     (free-identifier=? #'<ratnum>  (typ.identifier-type-tagging #'b))
			     (free-identifier=? #'<fixnums> (typ.identifier-type-tagging #'rest)))))
	  (values a b rest (inspect)))
	(fun 1.1 2/3 3 4))
    => 1.1 2/3 '(3 4) '(#t #t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-case-lambda))

;;;untagged bindings

  (check
      ((case-lambda
	(args
	 (define-syntax (inspect stx)
	   (typ.identifier-type-tagging #'args))
	 (values args (inspect))))
       1)
    => '(1) #f)

  (check
      ((case-lambda
	((a)
	 (define-syntax (inspect stx)
	   (typ.identifier-type-tagging #'a))
	 (values a (inspect))))
       1)
    => 1 #f)

  (check
      ((case-lambda
	((a b)
	 (define-syntax (inspect stx)
	   #`(quote #,(list (typ.identifier-type-tagging #'a)
			    (typ.identifier-type-tagging #'b))))
	 (values a b (inspect))))
       1 2)
    => 1 2 '(#f #f))

  (check
      ((case-lambda
	((a b . rest)
	 (define-syntax (inspect stx)
	   #`(quote #,(list (typ.identifier-type-tagging #'a)
			    (typ.identifier-type-tagging #'b)
			    (typ.identifier-type-tagging #'rest))))
	 (values a b rest (inspect))))
       1 2 3 4)
    => 1 2 '(3 4) '(#f #f #f))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      ((case-lambda
	({args <fixnums>}
	 (define-syntax (inspect stx)
	   (free-identifier=? #'<fixnums> (typ.identifier-type-tagging #'args)))
	 (values args (inspect))))
       1 2 3)
    => '(1 2 3) #t)

  (check
      ((case-lambda
	(({a <flonum>})
	 (define-syntax (inspect stx)
	   (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'a)))
	 (values a (inspect))))
       1.1)
    => 1.1 #t)

  (check
      ((case-lambda
	(({a <flonum>} {b <ratnum>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'a))
			    (free-identifier=? #'<ratnum> (typ.identifier-type-tagging #'b)))))
	 (values a b (inspect))))
       1.1 2/3)
    => 1.1 2/3 '(#t #t))

  (check
      ((case-lambda
	(({a <flonum>} {b <ratnum>} . {rest <fixnums>})
	 (define-syntax (inspect stx)
	   #`(quote #,(list (free-identifier=? #'<flonum>  (typ.identifier-type-tagging #'a))
			    (free-identifier=? #'<ratnum>  (typ.identifier-type-tagging #'b))
			    (free-identifier=? #'<fixnums> (typ.identifier-type-tagging #'rest)))))
	 (values a b rest (inspect))))
       1.1 2/3 3 4)
    => 1.1 2/3 '(3 4) '(#t #t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-let))

;;; untagged bindings

  (check
      (let ((a 1))
	(define-syntax (inspect stx)
	  (typ.identifier-type-tagging #'a))
	(values a (inspect)))
    => 1 #f)

  (check
      (let ((a 1)
	    (b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (typ.identifier-type-tagging #'a)
			   (typ.identifier-type-tagging #'b))))
	(values a (inspect)))
    => 1 '(#f #f))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (let (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a)))
	(values a (inspect)))
    => 1 #t)

  (check
      (let (({a <fixnum>} 1)
	    ({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a))
			   (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'b)))))
	(values a (inspect)))
    => 1 '(#t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-trace-let))

;;; untagged bindings

  (check
      (trace-let name ((a 1))
	(define-syntax (inspect stx)
	  (typ.identifier-type-tagging #'a))
	(values a (inspect)))
    => 1 #f)

  (check
      (trace-let name ((a 1)
		       (b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (typ.identifier-type-tagging #'a)
			   (typ.identifier-type-tagging #'b))))
	(values a (inspect)))
    => 1 '(#f #f))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (trace-let name (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a)))
	(values a (inspect)))
    => 1 #t)

  (check
      (trace-let name (({a <fixnum>} 1)
		       ({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a))
			   (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'b)))))
	(values a (inspect)))
    => 1 '(#t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-let-star))

;;; untagged bindings

  (check
      (let* ((a 1))
	(define-syntax (inspect stx)
	  (typ.identifier-type-tagging #'a))
	(values a (inspect)))
    => 1 #f)

  (check
      (let* ((a 1)
	     (b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (typ.identifier-type-tagging #'a)
			   (typ.identifier-type-tagging #'b))))
	(values a (inspect)))
    => 1 '(#f #f))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (let* (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a)))
	(values a (inspect)))
    => 1 #t)

  (check
      (let* (({a <fixnum>} 1)
	     ({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a))
			   (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'b)))))
	(values a (inspect)))
    => 1 '(#t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-letrec))

;;; untagged bindings

  (check
      (letrec ((a 1))
	(define-syntax (inspect stx)
	  (typ.identifier-type-tagging #'a))
	(values a (inspect)))
    => 1 #f)

  (check
      (letrec ((a 1)
	       (b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (typ.identifier-type-tagging #'a)
			   (typ.identifier-type-tagging #'b))))
	(values a (inspect)))
    => 1 '(#f #f))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (letrec (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a)))
	(values a (inspect)))
    => 1 #t)

  (check
      (letrec (({a <fixnum>} 1)
	       ({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a))
			   (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'b)))))
	(values a (inspect)))
    => 1 '(#t #t))

  #t)


(parametrise ((check-test-name	'tagged-bindings-letrec-star))

;;; untagged bindings

  (check
      (letrec* ((a 1))
	(define-syntax (inspect stx)
	  (typ.identifier-type-tagging #'a))
	(values a (inspect)))
    => 1 #f)

  (check
      (letrec* ((a 1)
		(b 2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (typ.identifier-type-tagging #'a)
			   (typ.identifier-type-tagging #'b))))
	(values a (inspect)))
    => 1 '(#f #f))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (letrec* (({a <fixnum>} 1))
	(define-syntax (inspect stx)
	  (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a)))
	(values a (inspect)))
    => 1 #t)

  (check
      (letrec* (({a <fixnum>} 1)
		({b <flonum>} 2.2))
	(define-syntax (inspect stx)
	  #`(quote #,(list (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a))
			   (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'b)))))
	(values a (inspect)))
    => 1 '(#t #t))

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
  	     (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a)))
  	   (add-result (inspect))
  	   1)
  	 a))
    => '(1 (#t)))

  (check
      (with-result
       (let ()
	 (define-values ({a <fixnum>} {b <flonum>} {c <ratnum>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a))
			      (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'b))
			      (free-identifier=? #'<ratnum> (typ.identifier-type-tagging #'c)))))
	   (add-result (inspect))
	   (values 1 2.2 3/4))
	 (vector a b c)))
    => '(#(1 2.2 3/4) ((#t #t #t))))

  (check
      (with-result
       (let (({a <flonum>} 2.2))
	 (define-values ({a <fixnum>})
	   (define-syntax (inspect stx)
	     (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a)))
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
	     (typ.identifier-type-tagging #'a))
	   (add-result (inspect)))))
    => '(2 (#f)))

  (check
      (with-result
       (do ((a 1 (+ 1 a))
	    (b 9))
	   ((= 2 a)
	    (vector a b))
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (typ.identifier-type-tagging #'a)
			      (typ.identifier-type-tagging #'b))))
	   (add-result (inspect)))))
    => '(#(2 9) ((#f #f))))

;;; --------------------------------------------------------------------
;;; tagged bindings

  (check
      (with-result
       (do (({a <fixnum>} 1 (+ 1 a)))
	   ((= 2 a)
	    a)
	 (let ()
	   (define-syntax (inspect stx)
	     (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a)))
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
	     #`(quote #,(list (free-identifier=? #'<fixnum> (typ.identifier-type-tagging #'a))
			      (free-identifier=? #'<flonum> (typ.identifier-type-tagging #'b)))))
	   (add-result (inspect)))))
    => '(#(2 2.2) ((#t #t))))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; End:
