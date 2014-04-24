;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for tagged language extensions
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
  (for (prefix (vicare expander object-type-specs) typ.)
    run expand)
  (vicare expander tags)
  (vicare checks))
(options tagged-language)

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: expand-time types\n")


;;;; helpers

(define-auxiliary-syntaxes <fixnums> <numbers> <complexes>)

(define (fixnums? obj)
  (and (list? obj)
       (for-all fixnum? obj)))

(define (numbers? obj)
  (and (list? obj)
       (for-all number? obj)))

(define (complexes? obj)
  (and (list? obj)
       (for-all complex? obj)))

(begin-for-syntax
  (define-syntax-rule (tag=tagging? ?tag ?var)
    (let ((id (typ.identifier-tag #'?var)))
      (if (identifier? id)
	  (free-identifier=? #'?tag id)
	"no-id")))

  (define-syntax-rule (procedure-subtag? ?var)
    (let ((id (typ.identifier-tag #'?var)))
      (if (identifier? id)
	  (typ.tag-super-and-sub? #'<procedure> id)
	"no-id")))

  (define-syntax-rule (top-tagged? ?var)
    (tag=tagging? <top> ?var))

  (typ.set-identifier-object-type-spec! #'<numbers>
    (typ.make-object-type-spec #'<numbers> #'<list> #'numbers?))

  (typ.set-identifier-object-type-spec! #'<complexes>
    (typ.make-object-type-spec #'<complexes> #'<numbers> #'complexes?))

  (typ.set-identifier-object-type-spec! #'<fixnums>
    (typ.make-object-type-spec #'<fixnums> #'<complexes> #'fixnums?))

  #| end of begin-for-syntax |# )

(define-syntax %eval
  (syntax-rules ()
    ((_ ?form)
     (eval ?form (environment '(vicare)
			      '(vicare expander tags))))))

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

(define-syntax catch-expression-return-value-violation
  (syntax-rules ()
    ((_ ?verbose . ?body)
     (guard (E ((expression-return-value-violation? E)
		(when ?verbose
		  (debug-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       . ?body))))

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


(parametrise ((check-test-name	'built-in-tags/super-and-sub))

  (define-syntax-rule (TRUE ?super ?sub)
    (check-for-true
     (typ.tag-super-and-sub? ?super ?sub)))

  (define-syntax-rule (FALS ?super ?sub)
    (check-for-false
     (typ.tag-super-and-sub? ?super ?sub)))

;;; --------------------------------------------------------------------

  (TRUE #'<top>			#'<number>)
  (TRUE #'<number>		#'<complex>)
  (TRUE #'<complex>		#'<real>)
  (TRUE #'<real>		#'<integer>)
  (TRUE #'<integer>		#'<exact-integer>)
  (TRUE #'<exact-integer>	#'<fixnum>)
  (TRUE #'<exact-integer>	#'<bignum>)
  (TRUE #'<real>		#'<flonum>)
  (TRUE #'<complex>		#'<compnum>)
  (TRUE #'<complex>		#'<cflonum>)

  #t)


(parametrise ((check-test-name	'formals-signatures/super-and-sub))

  (define-syntax-rule (TRUE ?super ?sub)
    (check-for-true
     (typ.formals-signature-super-and-sub-syntax? ?super ?sub)))

  (define-syntax-rule (FALS ?super ?sub)
    (check-for-false
     (typ.formals-signature-super-and-sub-syntax? ?super ?sub)))

;;; --------------------------------------------------------------------
;;; standalone identifier formals signatures

  (FALS #'<fixnums> #'<list>)
  (TRUE #'<list> #'<fixnums>)

;;; --------------------------------------------------------------------
;;; proper list formals signatures

  (TRUE #'(<number>) #'(<complex>))
  (TRUE #'(<fixnum> <fixnum> <fixnum>)
	#'(<fixnum> <fixnum> <fixnum>))
  (TRUE #'(<number> <fixnum> <fixnum>)
	#'(<fixnum> <fixnum> <fixnum>))
  (TRUE #'(<fixnum> <number> <fixnum>)
	#'(<fixnum> <fixnum> <fixnum>))
  (TRUE #'(<fixnum> <fixnum> <number>)
	#'(<fixnum> <fixnum> <fixnum>))
  (FALS #'(<fixnum> <fixnum> <fixnum>)
	#'(<number> <fixnum> <fixnum>))
  (FALS #'(<fixnum> <fixnum> <fixnum>)
	#'(<fixnum> <number> <fixnum>))
  (FALS #'(<fixnum> <fixnum> <fixnum>)
	#'(<fixnum> <fixnum> <number>))

;;; --------------------------------------------------------------------
;;; improper list formals signatures

  (TRUE #'(<number> . <numbers>) #'(<complex> . <complexes>))
  (FALS #'(<complex> . <complexes>) #'(<number> . <numbers>))

  (TRUE #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))
  (TRUE #'(<number> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))
  (TRUE #'(<fixnum> <number> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))
  (TRUE #'(<fixnum> <fixnum> <number> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))
  (TRUE #'(<fixnum> <fixnum> <fixnum> . <numbers>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))

  (FALS #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<number> <fixnum> <fixnum> . <fixnums>))
  (FALS #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <number> <fixnum> . <fixnums>))
  (FALS #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <number> . <fixnums>))
  (FALS #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <numbers>))

  (TRUE #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))
  (TRUE #'(<top>    <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))
  (TRUE #'(<fixnum> <top>    <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))
  (TRUE #'(<fixnum> <fixnum> <top>    . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <fixnums>))
  (TRUE #'(<fixnum> <fixnum> <fixnum> . <list>)    #'(<fixnum> <fixnum> <fixnum> . <fixnums>))

  (FALS #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<top>    <fixnum> <fixnum> . <fixnums>))
  (FALS #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <top>    <fixnum> . <fixnums>))
  (FALS #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <top>    . <fixnums>))
  (FALS #'(<fixnum> <fixnum> <fixnum> . <fixnums>) #'(<fixnum> <fixnum> <fixnum> . <list>))

;;; --------------------------------------------------------------------
;;; special mismatchings

  (TRUE #'(<number> . <list>)	#'(<complex> <fixnum> <fixnum>))
  (TRUE #'(<number> . <list>)	#'(<complex> <fixnum> <fixnum>))

  (TRUE #'(<number> <exact-integer> . <list>)	#'(<complex> <fixnum> <fixnum>))
  (TRUE #'(<number> <exact-integer> . <list>)	#'(<complex> <fixnum> <fixnum>))

  (TRUE #'(<number> <exact-integer> <fixnum> . <list>)	#'(<complex> <fixnum> <fixnum>))
  (TRUE #'(<number> <exact-integer> <fixnum> . <list>)	#'(<complex> <fixnum> <fixnum>))

  #t)


(parametrise ((check-test-name	'tag-ancestry))

  (check
      (typ.tag-identifier-ancestry #'<top>)
    (=> syntax=?)
    #'(<top>))

  (check
      (typ.tag-identifier-ancestry #'<top>)
    (=> syntax=?)
    #'(<top>))

  (check
      (typ.tag-identifier-ancestry #'<string>)
    (=> syntax=?)
    #'(<string> <top>))

  (check
      (typ.tag-identifier-ancestry #'<fixnum>)
    (=> syntax=?)
    #'(<fixnum> <exact-integer> <integer> <rational-valued>
		<real> <real-valued> <complex> <number> <top>))

;;; --------------------------------------------------------------------

  (check
      (typ.tag-common-ancestor #'<fixnum> #'<real>)
    (=> syntax=?)
    #'<real>)

  (check
      (typ.tag-common-ancestor #'<fixnum> #'<bignum>)
    (=> syntax=?)
    #'<exact-integer>)

  (check
      (typ.tag-common-ancestor #'<fixnum> #'<ratnum>)
    (=> syntax=?)
    #'<rational-valued>)

  #t)


(parametrise ((check-test-name	'retvals-ancestry))

  (define-syntax-rule (compare ?sig1 ?sig2 => ?result)
    (check
	(typ.retvals-signature-tags
	 (typ.retvals-signature-common-ancestor (typ.make-retvals-signature ?sig1)
						(typ.make-retvals-signature ?sig2)))
      (=> syntax=?)
      ?result))

;;; --------------------------------------------------------------------
;;; signatures, standalone identifiers

  (compare #'<fixnums> #'<list>			=> #'<list>)
  (compare #'<fixnums> #'<numbers>		=> #'<numbers>)

;;; --------------------------------------------------------------------
;;; signatures, proper lists

  (compare #'(<fixnum>) #'()			=> #'<list>)
  (compare #'() #'(<fixnum>)			=> #'<list>)

  (compare #'(<fixnum>) #'(<real>)		=> #'(<real>))
  (compare #'(<real>) #'(<fixnum>)		=> #'(<real>))

  (compare #'(<fixnum> <ratnum> <bignum>)
	   #'(<real> <complex> <exact-integer>)
	   => #'(<real> <complex> <exact-integer>))
  (compare #'(<real> <complex> <exact-integer>)
	   #'(<fixnum> <ratnum> <bignum>)
	   => #'(<real> <complex> <exact-integer>))

  (compare #'(<fixnum> <ratnum> <bignum>)
	   #'(<real> <complex>)
	   => #'(<real> <complex> . <list>))

  (compare #'(<real> <complex>)
	   #'(<fixnum> <ratnum> <bignum>)
	   => #'(<real> <complex> . <list>))

;;; --------------------------------------------------------------------
;;; signatures, improper lists

  (compare #'(<fixnum> <ratnum> . <list>)
	   #'(<real> <complex> <exact-integer>)
	   => #'(<real> <complex> . <list>))
  (compare #'(<real> <complex> <exact-integer>)
	   #'(<fixnum> <ratnum> . <list>)
	   => #'(<real> <complex> . <list>))

  (compare #'(<fixnum> <ratnum> . <list>)
	   #'(<real> <complex> . <list>)
	   => #'(<real> <complex> . <list>))

  (compare #'()
	   #'(<real> <complex> . <list>)
	   => #'<list>)
  (compare #'(<real> <complex> . <list>)
	   #'()
	   => #'<list>)

  #t)


(parametrise ((check-test-name	'parsing-tagged-bindings/identifiers))

  (check
      (typ.tagged-identifier-syntax? #'(brace X <fixnum>))
    => #t)

  (check
      (typ.tagged-identifier-syntax? #'{X <fixnum>})
    => #t)

  (check
      (typ.tagged-identifier-syntax? #'X)
    => #t)

  (check
      (typ.tagged-identifier-syntax? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (typ.parse-tagged-identifier-syntax #'(brace X <fixnum>))
    (=> syntax=?)
    #'X #'<fixnum>)

  (check
      (typ.parse-tagged-identifier-syntax #'X)
    (=> syntax=?)
    #'X #'<top>)

  #t)


(parametrise ((check-test-name	'parsing-tagged-bindings/bindings))

;;; list of tagged identifiers

  (check
      (typ.parse-list-of-tagged-bindings #'({a <fixnum>}))
    (=> syntax=?)
    #'(a) #'(<fixnum>))

  (check
      (typ.parse-list-of-tagged-bindings #'({a <fixnum>}
					    {b <string>}))
    (=> syntax=?)
    #'(a b) #'(<fixnum> <string>))

  (check
      (typ.parse-list-of-tagged-bindings #'({a <fixnum>}
					    {b <string>}
					    {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <string> <vector>))

;;;

  (check
      (typ.parse-list-of-tagged-bindings #'(a))
    (=> syntax=?)
    #'(a) #'(<top>))

  (check
      (typ.parse-list-of-tagged-bindings #'(a b))
    (=> syntax=?)
    #'(a b) #'(<top> <top>))

  (check
      (typ.parse-list-of-tagged-bindings #'(a b c))
    (=> syntax=?)
    #'(a b c) #'(<top> <top> <top>))

;;;

  (check
      (typ.parse-list-of-tagged-bindings #'(a
					    {b <string>}))
    (=> syntax=?)
    #'(a b) #'(<top> <string>))

  (check
      (typ.parse-list-of-tagged-bindings #'({a <fixnum>}
					    b))
    (=> syntax=?)
    #'(a b) #'(<fixnum> <top>))

  (check
      (typ.parse-list-of-tagged-bindings #'(a
					    {b <string>}
					    {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<top> <string> <vector>))

  (check
      (typ.parse-list-of-tagged-bindings #'({a <fixnum>}
					    b
					    {c <vector>}))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <top> <vector>))

  (check
      (typ.parse-list-of-tagged-bindings #'({a <fixnum>}
					    {b <string>}
					    c))
    (=> syntax=?)
    #'(a b c) #'(<fixnum> <string> <top>))

  #t)


(parametrise ((check-test-name	'parsing-tagged-bindings/callables))

  (define-syntax-rule (split ?input)
    (receive (standard-formals-stx callable)
	(typ.parse-tagged-lambda-proto-syntax ?input)
      (let ((formals-tags (typ.lambda-signature-formals-tags callable))
	    (rv-tags      (typ.lambda-signature-retvals-tags callable)))
	(values standard-formals-stx rv-tags formals-tags))))

;;; tagged

  (check
      (split #'({a <fixnum>}
		{b <string>}))
    (=> syntax=?)
    #'(a b) #'<list> #'(<fixnum> <string>))

  (check
      (split #'({a <fixnum>}
		{b <string>}
		{c <vector>}))
    (=> syntax=?)
    #'(a b c) #'<list> #'(<fixnum> <string> <vector>))

;;; untagged

  (check
      (split #'(a))
    (=> syntax=?)
    #'(a) #'<list> #'(<top>))

  (check
      (split #'(a b))
    (=> syntax=?)
    #'(a b) #'<list> #'(<top> <top>))

  (check
      (split #'(a b c))
    (=> syntax=?)
    #'(a b c) #'<list> #'(<top> <top> <top>))

;;; mixed tagged and untagged

  (check
      (split #'(a
		{b <string>}))
    (=> syntax=?)
    #'(a b) #'<list> #'(<top> <string>))

  (check
      (split #'({a <fixnum>}
		b))
    (=> syntax=?)
    #'(a b) #'<list> #'(<fixnum> <top>))

  (check
      (split #'(a
		{b <string>}
		{c <vector>}))
    (=> syntax=?)
    #'(a b c) #'<list> #'(<top> <string> <vector>))

  (check
      (split #'({a <fixnum>}
		b
		{c <vector>}))
    (=> syntax=?)
    #'(a b c) #'<list> #'(<fixnum> <top> <vector>))

  (check
      (split #'({a <fixnum>}
		{b <string>}
		c))
    (=> syntax=?)
    #'(a b c) #'<list> #'(<fixnum> <string> <top>))

;;; args argument

  (check	;tagged args argument
      (split #'{args <fixnums>})
    (=> syntax=?)
    #'args #'<list> #'<fixnums>)

  (check	;UNtagged args argument
      (split #'args)
    (=> syntax=?)
    #'args #'<list> #'<list>)

;;; rest argument

  (check	;tagged rest
      (split #'({a <fixnum>} . {rest <fixnums>}))
    (=> syntax=?)
    #'(a . rest) #'<list> #'(<fixnum> . <fixnums>))

  (check	;UNtagged rest
      (split #'({a <fixnum>} . rest))
    (=> syntax=?)
    #'(a . rest) #'<list> #'(<fixnum> . <list>))

  (check	;tagged rest
      (split #'({a <fixnum>} {b <string>} . {rest <fixnums>}))
    (=> syntax=?)
    #'(a b . rest) #'<list> #'(<fixnum> <string> . <fixnums>))

  (check	;UNtagged rest
      (split #'({a <fixnum>} {b <string>} . rest))
    (=> syntax=?)
    #'(a b . rest) #'<list> #'(<fixnum> <string> . <list>))

;;; return values tagging

  (check
      (split #'({_} a b))
    (=> syntax=?)
    #'(a b) '() #'(<top> <top>))

  (check
      (split #'({_ <fixnum>} a b))
    (=> syntax=?)
    #'(a b) #'(<fixnum>) #'(<top> <top>))

  (check
      (split #'({_ <fixnum>} {a <flonum>} {b <string>}))
    (=> syntax=?)
    #'(a b) #'(<fixnum>) #'(<flonum> <string>))

  (check
      (split #'({_ <fixnum> <flonum>} {a <vector>} {b <string>}))
    (=> syntax=?)
    #'(a b) #'(<fixnum> <flonum>) #'(<vector> <string>))

  (check
      (split #'({_ <fixnum> <flonum>} . {args <fixnums>}))
    (=> syntax=?)
    #'args #'(<fixnum> <flonum>) #'<fixnums>)


;;; --------------------------------------------------------------------
;;; tagged formals predicate

  (check-for-true
   (typ.tagged-lambda-proto-syntax? #'({a <fixnum>} {b <string>})))

  #t)


(parametrise ((check-test-name	'tagged-bindings-lambda))

;;;untagged bindings

  (check
      ((lambda args
	 (define-syntax (inspect stx)
	   (tag=tagging? <list> args))
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
			    (tag=tagging? <list> rest))))
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
      (with-result
       ((lambda ({_ <fixnum> <string> <symbol>} . {args <fixnums>})
	  (define-syntax (inspect stx)
	    (tag=tagging? <fixnums> args))
	  (add-result args)
	  (add-result (inspect))
	  (values 1 "2" 'c))
	1 2 3))
    => '(1 "2" c ((1 2 3) #t)))

  (check
      (with-result
       ((lambda ({_ <flonum> <boolean>} {a <flonum>})
	  (define-syntax (inspect stx)
	    (tag=tagging? <flonum> a))
	  (add-result a)
	  (add-result (inspect))
	  (values 2.2 #f))
	1.1))
    => '(2.2 #f (1.1 #t)))

  (check
      (with-result
       ((lambda ({_ <flonum> <ratnum> <list>} {a <flonum>} {b <ratnum>})
	  (define-syntax (inspect stx)
	    #`(quote #,(list (tag=tagging? <flonum> a)
			     (tag=tagging? <ratnum> b))))
	  (add-result a)
	  (add-result b)
	  (add-result (inspect))
	  (values 2.2 5/6 '(7)))
	1.1 2/3))
    => '(2.2 5/6 (7) (1.1 2/3 (#t #t))))

  (check
      (with-result
       ((lambda ({_ <flonum> <ratnum> <list> <list>} {a <flonum>} {b <ratnum>} . {rest <fixnums>})
	  (define-syntax (inspect stx)
	    #`(quote #,(list (tag=tagging? <flonum>  a)
			     (tag=tagging? <ratnum>  b)
			     (tag=tagging? <fixnums> rest))))
	  (add-result a)
	  (add-result b)
	  (add-result rest)
	  (add-result (inspect))
	  (values 2.2 5/6 '(7) '(8)))
	1.1 2/3 3 4))
    => '(2.2 5/6 (7) (8) (1.1 2/3 (3 4) (#t #t #t))))

  #t)


(parametrise ((check-test-name	'tagged-bindings-define))

  (begin-for-syntax
    (define* (lambda-signature->stx {fun-id identifier?})
      (let* ((signature    (typ.tag-identifier-callable-signature (typ.identifier-tag fun-id)))
	     (formals.tags (typ.lambda-signature-formals-tags signature))
	     (retvals.tags (typ.lambda-signature-retvals-tags signature)))
	(cons retvals.tags formals.tags))))

;;; --------------------------------------------------------------------
;;;untagged bindings

  (check
      (let ()
	(define (fun . args)
	  (define-syntax (inspect stx)
	    #`(quote #,(list (procedure-subtag? fun)
			     (tag=tagging? <list> args))))
	  (define-syntax (signature stx)
	    (syntax=? #'(<list> . <list>) (lambda-signature->stx #'fun)))
	  (values args (inspect) (signature)))
	(fun 1))
    => '(1) '(#t #t) #t)

  (check
      (let ()
	(define (fun)
	  (define-syntax (inspect stx)
	    (procedure-subtag? fun))
	  (define-syntax (signature stx)
	    (syntax=? #'(<list> . ()) (lambda-signature->stx #'fun)))
	  (values (inspect) (signature)))
	(fun))
    => #t #t)

  (check
      (let ()
	(define (fun a)
	  (define-syntax (inspect stx)
	    #`(quote #,(list (procedure-subtag? fun)
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
			     (tag=tagging? <list> rest))))
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
	    (syntax=? (lambda-signature->stx #'fun)
		      #'(<list> . <fixnums>)))
	  (values args (inspect) (signature)))
	(fun 1 2 3))
    => '(1 2 3) #t #t)

  (check	;tagged args, tagged return value
      (with-result
       (let ()
	 (define ({fun <fixnum>} . {args <fixnums>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (procedure-subtag? fun)
			      (tag=tagging? <fixnums> args))))
	   (define-syntax (signature stx)
	     (syntax=? (lambda-signature->stx #'fun)
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
	     #`(quote #,(list (procedure-subtag? fun)
			      (tag=tagging? <fixnums> args))))
	   (define-syntax (signature stx)
	     (syntax=? (lambda-signature->stx #'fun)
		       #'((<fixnum> <flonum>) . <fixnums>)))
	   (add-result (inspect))
	   (add-result (signature))
           (values (apply + args) 2.2))
	 (fun 1 10 100)))
    => '(111 2.2 ((#t #t) #t)))

  (check	;tagged args, single tagged return values
      (with-result
       (let ()
	 (define ({fun <fixnum>} {a <fixnum>} {b <fixnum>} {c <fixnum>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (procedure-subtag? fun)
			      (tag=tagging? <fixnum> a)
			      (tag=tagging? <fixnum> b)
			      (tag=tagging? <fixnum> c))))
	   (define-syntax (signature stx)
	     (syntax=? (lambda-signature->stx #'fun)
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
	   (tag=tagging? <list> args))
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
			    (tag=tagging? <list> rest))))
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

  (check	;the tag of LHS is inferred from the RHS
      (let ((a 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check	;the tag of LHS is inferred from the RHS
      (let ((a 1)
	    (b "2"))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <string> b))))
	(values a b (inspect)))
    => 1 "2" '(#t #t))

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

  (check	;the tag of LHS is inferred from the RHS
      (let* ((a 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check	;the tag of LHS is inferred from the RHS
      (let* ((a 1)
	     (b "2"))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <string> b))))
	(values a b (inspect)))
    => 1 "2" '(#t #t))

  (check	;the tag of LHS is inferred from the RHS
      (let* ((a 1)
	     (b a))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <fixnum> b))))
	(values a b (inspect)))
    => 1 1 '(#t #t))

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

  (check	;the tag of LHS is inferred from the RHS
      (letrec ((a 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check	;the tag of LHS is inferred from the RHS
      (letrec ((a 1)
	       (b "2"))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <string> b))))
	(values a b (inspect)))
    => 1 "2" '(#t #t))

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
				    (procedure-subtag? a))
				  (values x (inspect)))))
	(a 1))
    => 1 #t)

  #t)


(parametrise ((check-test-name	'tagged-bindings-letrec-star))

;;; untagged bindings

  (check	;the tag of LHS is inferred from the RHS
      (letrec* ((a 1))
	(define-syntax (inspect stx)
	  (tag=tagging? <fixnum> a))
	(values a (inspect)))
    => 1 #t)

  (check	;the tag of LHS is inferred from the RHS
      (letrec* ((a 1)
		(b "2"))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <string> b))))
	(values a b (inspect)))
    => 1 "2" '(#t #t))

  (check	;the tag of LHS is inferred from the RHS
      (letrec* ((a 1)
		(b a))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum> a)
			   (tag=tagging? <fixnum> b))))
	(values a b (inspect)))
    => 1 1 '(#t #t))

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

  (check	;B gets its value from evaluating an expression in which
		;A is tagged.
      (letrec* (({a <fixnum>}  1)
		({b <boolean>} (a positive?)))
	(define-syntax (inspect stx)
	  #`(quote #,(list (tag=tagging? <fixnum>  a)
			   (tag=tagging? <boolean> b))))
	(values a b (inspect)))
    => 1 #t '(#t #t))

  (check
      (letrec* (({a <procedure>} (lambda (x)
				   (define-syntax (inspect stx)
				     (procedure-subtag? a))
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
       (let ()
	 (define-values ({a <fixnum>} . {b <list>})
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> a)
			      (tag=tagging? <list>   b))))
	   (add-result (inspect))
	   (values 1 2.2 3/4))
	 (vector a b)))
    => '(#(1 (2.2 3/4)) ((#t #t))))

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


(parametrise ((check-test-name	'tagged-bindings-define-constant-values))

;;; untagged bindings

  (check
      (let ()
	(define-constant-values (a)
	  1)
	a)
    => 1)

  (check
      (with-result
       (let ()
	 (define-constant-values (a)
	   (add-result 2)
	   1)
	 a))
    => '(1 (2)))

  (check
      (let ()
  	(define-constant-values (a b c)
  	  #t
  	  (values 1 2 3))
  	(list a b c))
    => '(1 2 3))

  (check
      (let ((a 2))
  	(define-constant-values (a)
  	  (values 1))
  	a)
    => 1)

  (check	;recursive binding
      (with-result
       (let ()
	 (define-constant-values (f)
	   (lambda (arg)
	     (if (positive? arg)
		 (begin
		   (add-result arg)
		   (f (- arg 1)))
	       arg)))
	 (f 2)))
    => '(0 (2 1)))

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
	     (tag=tagging? <list> args))
	   (add-result (inspect))
	   args)))
    => '((1 2 3) (#t)))

  (check
      (with-result
       (receive (a b c . rest)
	   (values 1 2 3 4 5)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? b)
			      (top-tagged? c)
			      (tag=tagging? <list> rest))))
	   (add-result (inspect))
	   (list a b c rest))))
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
	     (tag=tagging? <list> args))
	   (add-result (inspect)))))
    => '((1 2 3) (#t)))

  (check
      (with-result
       (receive-and-return (a b c . rest)
	   (values 1 2 3 4 5)
	 (let ()
	   (define-syntax (inspect stx)
	     #`(quote #,(list (top-tagged? a)
			      (top-tagged? b)
			      (top-tagged? c)
			      (tag=tagging? <list> rest))))
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
	 (define-inline (ciao ?a ?b)
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> ?a)
			      (tag=tagging? <fixnum> ?b))))
	   (add-result (inspect))
	   (+ ?a ?b))
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
	 (define-inline (ciao . ?args)
	   (define-syntax (inspect stx)
	     (tag=tagging? <list> ?args))
	   (add-result (inspect))
	   (apply + ?args))
	 (ciao 1 2)))
    => '(3 (#t)))

  (check
      (with-result
       (let ()
	 (define-inline (ciao ?a . ?rest)
	   (define-syntax (inspect stx)
	     #`(quote #,(list (tag=tagging? <fixnum> ?a)
			      (tag=tagging? <list> ?rest))))
	   (add-result (inspect))
	   (apply + ?a ?rest))
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
	   (tag=tagging? <list> a))
	 (add-result (inspect))
	 a))
    => '((1 2 3) (#t)))

  (check
      (with-result
       (let-values (((a . rest) (values 1 2 3)))
	 (define-syntax (inspect stx)
	   (tag=tagging? <list> rest))
	 (add-result (inspect))
	 (values a rest)))
    => '(1 (2 3) (#t)))

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
	   (tag=tagging? <list> a))
	 (add-result (inspect))
	 a))
    => '((1 2 3) (#t)))

  (check
      (with-result
       (let*-values (((a . rest) (values 1 2 3)))
	 (define-syntax (inspect stx)
	   (tag=tagging? <list> rest))
	 (add-result (inspect))
	 (values a rest)))
    => '(1 (2 3) (#t)))

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

  #t)


(parametrise ((check-test-name	'tag-predicate))

  (check
      ((tag-predicate <fixnum>) 123)
    => #t)

  (check
      ((tag-predicate <fixnum>) "123")
    => #f)

  (check
      (is-a? 123 <top>)
    => #t)

  (check
      ((tag-predicate <top>) 123)
    => #t)

  #t)


(parametrise ((check-test-name	'tag-validator))

  (check
      (tag-procedure-argument-validator <fixnum> 123)
    => 123)

  (check
      ((tag-procedure-argument-validator <fixnum> <>) 123)
    => 123)

  (check-for-procedure-argument-violation
      (tag-procedure-argument-validator <fixnum> "123")
    => '(<fixnum> ("123")))

  (check
      (tag-procedure-argument-validator <top> 123)
    => 123)

;;; --------------------------------------------------------------------

  (check
      (tag-return-value-validator <fixnum> 123)
    => 123)

  (check
      ((tag-return-value-validator <fixnum> <>) 123)
    => 123)

  (check-for-expression-return-value-violation
      (tag-return-value-validator <fixnum> "123")
    => '(<fixnum> ("123")))

  (check
      (tag-return-value-validator <top> 123)
    => 123)

  #t)


(parametrise ((check-test-name	'tag-assert))

  (check
      (tag-assert (<fixnum>) 123)
    => (void))

  (check	;check  at expand-time  that  the  expression returns  a
		;single value
      (tag-assert (<top>) 123)
    => (void))

  (check	;check  at expand-time  that  the  expression returns  a
		;single value
      (tag-assert (<top>) ((<top>)123))
    => (void))

  (check	;check at run-time that  the expression returns a single
		;value
      (let ()
	;;The signature  of this function  has an unspecified  number of
	;;return values.
	(define ({doit . <list>})
	  123)
	(tag-assert (<top>) (doit)))
    => (void))

;;; --------------------------------------------------------------------
;;; any tuple of returned values is of type <top>

  (check
      (tag-assert <list> 123)
    => (void))

  (check
      (with-result
       (tag-assert <list> (receive-and-return (V)
			      (+ 1 2 3)
			    (add-result V))))
    => `(,(void) (6)))

  (check
      (with-result
       (tag-assert <list> (receive-and-return (a b c)
			      (values 1 2 3)
			    (add-result (vector a b c)))))
    => `(,(void) (#(1 2 3))))

;;; --------------------------------------------------------------------
;;; a proper list of returned values is of type <list>

  (check
      (with-result
       (tag-assert <list> (receive-and-return (a b c)
  			      (values 1 2 3)
  			    (add-result (vector a b c)))))
    => `(,(void) (#(1 2 3))))

  (check
      (with-result
       (tag-assert (<fixnum> . <list>)
  		   (receive-and-return (a b c)
  		       (values 1 2 3)
  		     (add-result (vector a b c)))))
    => `(,(void) (#(1 2 3))))

  (check
      (with-result
       (tag-assert (<fixnum> <fixnum> <exact-integer> . <list>)
  		   (receive-and-return (a b c)
  		       (values 1 2 3)
  		     (add-result (vector a b c)))))
    => `(,(void) (#(1 2 3))))

;;; --------------------------------------------------------------------
;;; records

  (check	;the ?EXPR is not explicitly tagged, run-time test
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define O
	  (make-alpha 1 2 3))
	(tag-assert (alpha) O))
    => (void))

  (check	;the ?EXPR is expliticly tagged, expand-time test
      (let ()
  	(define-record-type alpha
  	  (fields a b c))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
  	(tag-assert (alpha) O))
    => (void))

  (check
      (catch-expand-time-signature-violation #f
  	(%eval '(let ()
  		  (define-record-type alpha
  		    (fields a b c))
  		  (define {O alpha}
  		    (make-alpha 1 2 3))
  		  (tag-assert (<fixnum>) O))))
    => '(<fixnum>) '(alpha))

;;; --------------------------------------------------------------------
;;; record with parent

  (check	;the ?EXPR is not explicitly tagged, run-time check
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(tag-assert (beta) O))
    => (void))

  (check	;the ?EXPR is expliticly tagged, expand-time check
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(define {O beta}
	  (make-beta 1 2 3 4 5 6))
	(values (tag-assert (alpha) O)
		(tag-assert (beta)  O)))
    => (void) (void))

  #t)


(parametrise ((check-test-name	'tag-assert-and-return))

  (check
      (tag-assert-and-return (<fixnum>) 123)
    => 123)

  (check
      (tag-assert-and-return (<top>) 123)
    => 123)

  (check	;check  at expand-time  that  the  expression returns  a
		;single value
      (tag-assert-and-return (<top>) 123)
    => 123)

  (check	;check at run-time that  the expression returns a single
		;value
      (tag-assert-and-return (<top>) ((<top>)123))
    => 123)

;;; --------------------------------------------------------------------
;;; any tuple of returned values is of type <top>

  (check
      (tag-assert-and-return <list> 123)
    => 123)

  (check
      (with-result
       (tag-assert-and-return <list> (receive-and-return (V)
					 (+ 1 2 3)
				       (add-result V))))
    => '(6 (6)))

  (check
      (with-result
       (tag-assert-and-return <list> (receive-and-return (a b c)
					 (values 1 2 3)
				       (add-result (vector a b c)))))
    => '(1 2 3 (#(1 2 3))))

;;; --------------------------------------------------------------------
;;; a proper list of returned values is of type <list>

  (check
      (with-result
       (tag-assert-and-return <list> (receive-and-return (a b c)
					 (values 1 2 3)
				       (add-result (vector a b c)))))
    => '(1 2 3 (#(1 2 3))))

  (check
      (with-result
       (tag-assert-and-return (<fixnum> . <list>)
			      (receive-and-return (a b c)
				  (values 1 2 3)
				(add-result (vector a b c)))))
    => '(1 2 3 (#(1 2 3))))

  (check
      (with-result
       (tag-assert-and-return (<fixnum> <fixnum> <exact-integer> . <list>)
			      (receive-and-return (a b c)
				  (values 1 2 3)
				(add-result (vector a b c)))))
    => '(1 2 3 (#(1 2 3))))

;;; --------------------------------------------------------------------
;;; records

  (check	;the ?EXPR is not explicitly tagged, run-time check
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define O
	  (make-alpha 1 2 3))
	(alpha? (tag-assert-and-return (alpha) O)))
    => #t)

  (check	;the ?EXPR is expliticly tagged, expand-time check
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(alpha? (tag-assert-and-return (alpha) O)))
    => #t)

  (check
      (catch-expand-time-signature-violation #f
  	(%eval '(let ()
  		  (define-record-type alpha
  		    (fields a b c))
  		  (define {O alpha}
  		    (make-alpha 1 2 3))
  		  (tag-assert-and-return (<fixnum>) O))))
    => '(<fixnum>) '(alpha))

;;; --------------------------------------------------------------------
;;; record with parent

  (check	;the ?EXPR is not explicitly tagged, run-time check
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(beta? (tag-assert-and-return (beta) O)))
    => #t)

  (check	;the ?EXPR is expliticly tagged, expand-time check
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(define {O beta}
	  (make-beta 1 2 3 4 5 6))
	(values (alpha? (tag-assert-and-return (alpha) O))
		(beta?  (tag-assert-and-return (beta)  O))))
    => #t #t)

  #t)


(parametrise ((check-test-name	'values))

  (check
      (tag-assert-and-return (<fixnum>) (values 123))
    => 123)

  (check
      (tag-assert-and-return (<fixnum> <flonum> <ratnum>) (values 1 2.2 3/4))
    => 1 2.2 3/4)

  (check	;expected rest
      (tag-assert-and-return (<fixnum> . <list>) (values 1 2.2 3/4))
    => 1 2.2 3/4)

  (check	;wrong number of returned values
      (catch-expand-time-signature-violation #f
	(%eval '(tag-assert-and-return (<fixnum> <flonum>) (values 1 2.2 3/4))))
    => '(<fixnum> <flonum>) '(<fixnum> <flonum> <ratnum>))

  (check
      (catch-expand-time-signature-violation #f
	(%eval '(tag-assert-and-return (<fixnum> <flonum> <ratnum>) (values "1" 2.2 3/4))))
    => '(<fixnum> <flonum> <ratnum>) '(<string> <flonum> <ratnum>))

  (check
      (catch-expand-time-signature-violation #f
	(%eval '(tag-assert-and-return (<fixnum> <flonum> <ratnum>) (values 1 "2.2" 3/4))))
    => '(<fixnum> <flonum> <ratnum>) '(<fixnum> <string> <ratnum>))

  (check
      (catch-expand-time-signature-violation #f
	(%eval '(tag-assert-and-return (<fixnum> <flonum> <ratnum>) (values 1 2.2 "3/4"))))
    => '(<fixnum> <flonum> <ratnum>) '(<fixnum> <flonum> <string>))

  #t)


(parametrise ((check-test-name	'tag-cast))

  (check	;demo
      (let ((O "ciao"))
	(tag-assert-and-return (<string>) O))
    => "ciao")

  (check	;demo
      (let ((O "ciao"))
	((splice-first-expand (tag-assert-and-return (<string>))) O))
    => "ciao")

  (check	;demo
      (catch-expand-time-signature-violation #f
	(%eval '(let (({O <fixnum>} 123))
		  ((splice-first-expand (tag-assert-and-return (<string>))) O))))
    => '(<string>) '(<fixnum>))

  (check	;demo
      (let ((O "ciao"))
	(((splice-first-expand (tag-assert-and-return (<string>))) O) [1]))
    => #\i)

  (check	;demo
      (let ((O "ciao"))
	(let-syntax ((type (syntax-rules ()
			     ((_)
			      (splice-first-expand (tag-assert-and-return (<string>)))))))
	  (((type)O) [1])))
    => #\i)

;;; --------------------------------------------------------------------

  (check	;tag of expr known
      (tag-cast <string> 123)
    => "123")

  (check	;tag of expr known
      (tag-cast <string> 'ciao)
    => "ciao")

  (check	;tag of expr known
      ((tag-cast <string> 'ciao) length)
    => 4)

  (check	;tag of expr is <top>
      (let ((O 123))
  	(tag-cast <string> O))
    => "123")

;;; --------------------------------------------------------------------

  (check	;tag of expr known
      ((<string>) 123)
    => "123")

  (check	;tag of expr known
      ((<string>) 'ciao)
    => "ciao")

  (check	;tag of expr known
      (((<string>) 'ciao) length)
    => 4)

  (check	;tag of expr is <top>
      (let ((O 123))
  	((<string>) O))
    => "123")

  (check	;tag of expr is <top>
      (let ((O 123))
  	(((<string>) O) length))
    => 3)

  #t)


(parametrise ((check-test-name	'tag-accessor))

;;; built-ins

  (check-for-false (tag-accessor +123 even?))
  (check-for-true  (tag-accessor +123 odd?))
  (check-for-true  (tag-accessor +123 positive?))
  (check-for-false (tag-accessor +123 negative?))
  (check-for-false (tag-accessor +123 non-positive?))
  (check-for-true  (tag-accessor +123 non-negative?))

  (check-for-false (tag-accessor -123 even?))
  (check-for-true  (tag-accessor -123 odd?))
  (check-for-false (tag-accessor -123 positive?))
  (check-for-true  (tag-accessor -123 negative?))
  (check-for-true  (tag-accessor -123 non-positive?))
  (check-for-false (tag-accessor -123 non-negative?))

  (check-for-false (tag-accessor +123 zero?))
  (check-for-false (tag-accessor -123 zero?))
  (check-for-true  (tag-accessor 0 zero?))

;;; --------------------------------------------------------------------
;;; structs

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(tag-accessor O a))
    => 1)

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(list (tag-accessor O a)
	      (tag-accessor O b)
	      (tag-accessor O c)))
    => '(1 2 3))

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(let (({O alpha} (make-alpha 1 2 3)))
	  (list (tag-accessor O a)
		(tag-accessor O b)
		(tag-accessor O c))))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; records

  (check
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(tag-accessor O a))
    => 1)

  (check
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(list (tag-accessor O a)
	      (tag-accessor O b)
	      (tag-accessor O c)))
    => '(1 2 3))

  (check	;record with parent
      (let ()
	(define-record-type alpha
	  (fields a b c))
	(define-record-type beta
	  (parent alpha)
	  (fields d e f))
	(define O
	  (make-beta 1 2 3 4 5 6))
	(list (tag-accessor O a)
	      (tag-accessor O b)
	      (tag-accessor O c)
	      (tag-accessor O d)
	      (tag-accessor O e)
	      (tag-accessor O f)))
    => '(1 2 3 4 5 6))

  #t)


(parametrise ((check-test-name	'tag-mutator))

;;; structs

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(tag-mutator O a 11)
	(tag-accessor O a))
    => 11)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(tag-mutator O a 11)
	(tag-mutator O b 22)
	(tag-mutator O c 33)
  	(values (tag-accessor O a)
  		(tag-accessor O b)
  		(tag-accessor O c)))
    => 11 22 33)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(let (({O alpha} (make-alpha 1 2 3)))
	  (tag-mutator O a 11)
	  (tag-mutator O b 22)
	  (tag-mutator O c 33)
	  (values (tag-accessor O a)
		  (tag-accessor O b)
		  (tag-accessor O c))))
    => 11 22 33)

;;; --------------------------------------------------------------------
;;; records

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
  	(tag-mutator O a 11)
  	(tag-accessor O a))
    => 11)

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(tag-mutator O a 11)
	#;(tag-mutator O b 22)
	(tag-mutator O c 33)
  	(values (tag-accessor O a)
  		(tag-accessor O b)
  		(tag-accessor O c)))
    => 11 2 33)

  (check	;record with parent
      (let ()
  	(define-record-type alpha
  	  (fields (mutable a)
		  (immutable b)
		  (mutable   c)))
  	(define-record-type beta
  	  (parent alpha)
  	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
  	(define {O beta}
  	  (make-beta 1 2 3 4 5 6))
	(tag-mutator O a 11)
	#;(tag-mutator O b 22)
	(tag-mutator O c 33)
	(tag-mutator O d 44)
	(tag-mutator O e 55)
	(tag-mutator O f 66)
  	(list (tag-accessor O a)
	      (tag-accessor O b)
	      (tag-accessor O c)
	      (tag-accessor O d)
	      (tag-accessor O e)
	      (tag-accessor O f)))
    => '(11 2 33 44 55 66))

  #t)


(parametrise ((check-test-name	'tag-getter))

;;; built-in tags

  (check
      (tag-getter "ciao" [1])
    => #\i)

  (check
      (tag-getter "ciao" [3])
    => #\o)

;;; --------------------------------------------------------------------
;;; structs

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(tag-getter O [a]))
    => 1)

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(values (tag-getter O [a])
		(tag-getter O [b])
		(tag-getter O [c])))
    => 1 2 3)

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(let (({O alpha} (make-alpha 1 2 3)))
	  (values (tag-getter O [a])
		  (tag-getter O [b])
		  (tag-getter O [c]))))
    => 1 2 3)

;;; --------------------------------------------------------------------
;;; records

  (check
      (let ()
  	(define-record-type alpha
  	  (fields a b c))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
  	(tag-getter O [a]))
    => 1)

  (check
      (let ()
  	(define-record-type alpha
  	  (fields a b c))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 1 2 3)

  (check	;record with parent
      (let ()
  	(define-record-type alpha
  	  (fields a b c))
  	(define-record-type beta
  	  (parent alpha)
  	  (fields d e f))
  	(define {O beta}
  	  (make-beta 1 2 3 4 5 6))
  	(list (tag-getter O [a])
	      (tag-getter O [b])
	      (tag-getter O [c])
	      (tag-getter O [d])
	      (tag-getter O [e])
	      (tag-getter O [f])))
    => '(1 2 3 4 5 6))

  #t)


(parametrise ((check-test-name	'tag-setter))

;;; built-in tags

  (check
      (let (({S <string>} (string-copy "ciao")))
	(tag-setter S [1] #\I)
	(tag-getter S [1]))
    => #\I)

  (check
      (let (({S <string>} (string-copy "ciao")))
	(tag-setter S [3] #\O)
	S)
    => "ciaO")

  (check
      (receive-and-return ({S <string>})
	  (string-copy "ciao")
	(tag-setter S [3] #\O))
    => "ciaO")

;;; --------------------------------------------------------------------
;;; structs

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(tag-setter O [a] 11)
	(tag-getter O [a]))
    => 11)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(tag-setter O [a] 11)
	(tag-setter O [b] 22)
	(tag-setter O [c] 33)
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 11 22 33)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(let (({O alpha} (make-alpha 1 2 3)))
	  (tag-setter O [a] 11)
	  (tag-setter O [b] 22)
	  (tag-setter O [c] 33)
	  (values (tag-getter O [a])
		  (tag-getter O [b])
		  (tag-getter O [c]))))
    => 11 22 33)

;;; --------------------------------------------------------------------
;;; records

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
  	(tag-setter O [a] 11)
  	(tag-getter O [a]))
    => 11)

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(tag-setter O [a] 11)
	#;(tag-setter O [b] 22)
	(tag-setter O [c] 33)
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 11 2 33)

  (check	;record with parent
      (let ()
  	(define-record-type alpha
  	  (fields (mutable a)
		  (immutable b)
		  (mutable   c)))
  	(define-record-type beta
  	  (parent alpha)
  	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
  	(define {O beta}
  	  (make-beta 1 2 3 4 5 6))
	(tag-setter O [a] 11)
	#;(tag-setter O [b] 22)
	(tag-setter O [c] 33)
	(tag-setter O [d] 44)
	(tag-setter O [e] 55)
	(tag-setter O [f] 66)
  	(list (tag-getter O [a])
	      (tag-getter O [b])
	      (tag-getter O [c])
	      (tag-getter O [d])
	      (tag-getter O [e])
	      (tag-getter O [f])))
    => '(11 2 33 44 55 66))

  #t)


(parametrise ((check-test-name	'tag-dispatch/builtins))

;;; strings

  (check
      (tag-dispatch "ciao" = "hello")
    => #f)

  (check
      (tag-dispatch "ciao" = "ciao")
    => #t)

  (check
      (tag-dispatch "ciao" substring 1 3)
    => "ia")

  (check
      (tag-dispatch "ciao" list)
    => '(#\c #\i #\a #\o))

  #t)


(parametrise ((check-test-name	'tag-dispatch/structs))

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(tag-dispatch O a))
    => 1)

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(values (tag-dispatch O a)
		(tag-dispatch O b)
		(tag-dispatch O c)))
    => 1 2 3)

  #t)


(parametrise ((check-test-name	'tag-dispatch/records))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(alpha? O))
    => #t)

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(tag-dispatch O a))
    => 1)

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(values (tag-dispatch O a) (tag-dispatch O b) (tag-dispatch O c)))
    => 1 2 3)

;;; --------------------------------------------------------------------
;;; record types with parents

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
	(list (tag-dispatch O a1) (tag-dispatch O a2) (tag-dispatch O a3)
	      (tag-dispatch O b1) (tag-dispatch O b2) (tag-dispatch O b3)))
    => '(1 2 3 4 5 6))

  #t)


(parametrise ((check-test-name	'implicit-dispatching/structs))

  (check
      (internal-body
	(define-struct alpha
	  (a b c))
	(define {O alpha} ;explicit tagging
	  (make-alpha 1 2 3))
	;;In this syntax the dispatching form is evaluated by CHI-BODY.
	(O a))
    => 1)

  (check
      (internal-body
	(define-struct alpha
	  (a b c))
	(define O	;implicit tagging
	  (make-alpha 1 2 3))
	;;In  this  syntax  the   dispatching  forms  are  evaluated  by
	;;CHI-EXPR.
	(values (O a) (O b) (O c)))
    => 1 2 3)

;;; --------------------------------------------------------------------
;;; typed fields

  (check
      (internal-body
	(define-struct alpha
	  ({a <fixnum>} {b <ratnum>} {c <flonum>}))
	(define O
	  (make-alpha 1 2/3 4.5))
	(values (O a) (O b) (O c)))
    => 1 2/3 4.5)

  (check
      (internal-body
	(define-struct alpha
	  ({a <fixnum>} {b <ratnum>} {c <flonum>}))
	(define O
	  (make-alpha 1 2/3 4.5))
	(values ((O a) positive?)
		((O b) numerator)
		((O c) square)))
    => #t 2 (square 4.5))

  (check	;maker syntax causes rhs tag propagation
      (internal-body
  	(define-struct alpha
  	  ({a <fixnum>} {b <ratnum>} {c <flonum>}))
  	(define O
  	  (alpha (1 2/3 4.5)))
  	(values ((O a) positive?)
  		((O b) numerator)
  		((O c) square)))
    => #t 2 (square 4.5))

  #t)


(parametrise ((check-test-name	'implicit-dispatching/records))

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(alpha? O))
    => #t)

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	;;In this syntax  the dispatching form is  evaluated by CHI-EXPR
	;;after being processed by CHI-BODY*.
	(O a))
    => 1)

  (check
      (let ()
	(define-record-type alpha
	  (fields (mutable a) (mutable b) (mutable c)))
	(define {O alpha}
	  (make-alpha 1 2 3))
	;;In  this  syntax  the   dispatching  forms  are  evaluated  by
	;;CHI-EXPR*.
	(values (O a) (O b) (O c)))
    => 1 2 3)

;;; --------------------------------------------------------------------
;;; record types with parents

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
	(list (O a1) (O a2) (O a3)
	      (O b1) (O b2) (O b3)))
    => '(1 2 3 4 5 6))

;;; --------------------------------------------------------------------
;;; typed fields

  (check
      (internal-body
	(define-record-type alpha
	  (fields {a <fixnum>} {b <ratnum>} {c <flonum>}))
	(define O
	  (make-alpha 1 2/3 4.5))
	(values (O a) (O b) (O c)))
    => 1 2/3 4.5)

  (check
      (internal-body
	(define-record-type alpha
	  (fields {a <fixnum>} {b <ratnum>} {c <flonum>}))
	(define O
	  (make-alpha 1 2/3 4.5))
	(values ((O a) positive?)
		((O b) numerator)
		((O c) square)))
    => #t 2 (square 4.5))

  (check	;maker syntax causes rhs tag propagation
      (internal-body
	(define-record-type alpha
	  (fields {a <fixnum>} {b <ratnum>} {c <flonum>}))
	(define O
	  (alpha (1 2/3 4.5)))
	(values ((O a) positive?)
		((O b) numerator)
		((O c) square)))
    => #t 2 (square 4.5))

  #t)


(parametrise ((check-test-name	'implicit-dispatching/datums))

  (check
      (let ()
	;;The expander  will automatically assign the  tag "<fixnum>" to
	;;the identifier O.
	(define O 123)
	(O positive?))
    => #t)

  (check
      (let ()
	;;The expander  will automatically assign the  tag "<string>" to
	;;the identifier O.
	(define O "ciao")
	(O [1]))
    => #\i)


  #t)


(parametrise ((check-test-name	'set-bang))

;;; structs and setter, syntax 1

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(set! O [a] 11)
	(tag-getter O [a]))
    => 11)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(set! O [a] 11)
	(set! O [b] 22)
	(set! O [c] 33)
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 11 22 33)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(let (({O alpha} (make-alpha 1 2 3)))
	  (set! O [a] 11)
	  (set! O [b] 22)
	  (set! O [c] 33)
	  (values (tag-getter O [a])
		  (tag-getter O [b])
		  (tag-getter O [c]))))
    => 11 22 33)

;;; --------------------------------------------------------------------
;;; structs and setter, syntax 2

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(set! (O [a]) 11)
	(tag-getter O [a]))
    => 11)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(set! (O [a]) 11)
	(set! (O [b]) 22)
	(set! (O [c]) 33)
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 11 22 33)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(let (({O alpha} (make-alpha 1 2 3)))
	  (set! (O [a]) 11)
	  (set! (O [b]) 22)
	  (set! (O [c]) 33)
	  (values (tag-getter O [a])
		  (tag-getter O [b])
		  (tag-getter O [c]))))
    => 11 22 33)

;;; --------------------------------------------------------------------
;;; structs and mutator

  (check
      (let ()
	(define-struct alpha
	  (a b c))
	(define {O alpha}
	  (make-alpha 1 2 3))
	(set! (O a) 11)
	(tag-getter O [a]))
    => 11)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(set! (O a) 11)
	(set! (O b) 22)
	(set! (O c) 33)
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 11 22 33)

  (check
      (let ()
  	(define-struct alpha
  	  (a b c))
  	(let (({O alpha} (make-alpha 1 2 3)))
	  (set! (O a) 11)
	  (set! (O b) 22)
	  (set! (O c) 33)
	  (values (tag-getter O [a])
		  (tag-getter O [b])
		  (tag-getter O [c]))))
    => 11 22 33)

;;; --------------------------------------------------------------------
;;; records and setter, syntax 1

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
  	(set! O [a] 11)
  	(tag-getter O [a]))
    => 11)

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(set! O [a] 11)
	#;(set! O [b] 22)
	(set! O [c] 33)
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 11 2 33)

  (check	;record with parent
      (let ()
  	(define-record-type alpha
  	  (fields (mutable a)
		  (immutable b)
		  (mutable   c)))
  	(define-record-type beta
  	  (parent alpha)
  	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
  	(define {O beta}
  	  (make-beta 1 2 3 4 5 6))
	(set! O [a] 11)
	#;(set! O [b] 22)
	(set! O [c] 33)
	(set! O [d] 44)
	(set! O [e] 55)
	(set! O [f] 66)
  	(list (tag-getter O [a])
	      (tag-getter O [b])
	      (tag-getter O [c])
	      (tag-getter O [d])
	      (tag-getter O [e])
	      (tag-getter O [f])))
    => '(11 2 33 44 55 66))

;;; --------------------------------------------------------------------
;;; records and setter, syntax 2

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
  	(set! (O [a]) 11)
  	(tag-getter O [a]))
    => 11)

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(set! (O [a]) 11)
	#;(set! (O [b]) 22)
	(set! (O [c]) 33)
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 11 2 33)

  (check	;record with parent
      (let ()
  	(define-record-type alpha
  	  (fields (mutable a)
		  (immutable b)
		  (mutable   c)))
  	(define-record-type beta
  	  (parent alpha)
  	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
  	(define {O beta}
  	  (make-beta 1 2 3 4 5 6))
	(set! (O [a]) 11)
	#;(set! (O [b]) 22)
	(set! (O [c]) 33)
	(set! (O [d]) 44)
	(set! (O [e]) 55)
	(set! (O [f]) 66)
  	(list (tag-getter O [a])
	      (tag-getter O [b])
	      (tag-getter O [c])
	      (tag-getter O [d])
	      (tag-getter O [e])
	      (tag-getter O [f])))
    => '(11 2 33 44 55 66))

;;; --------------------------------------------------------------------
;;; records and mutator

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
  	(set! (O a) 11)
  	(tag-getter O [a]))
    => 11)

  (check
      (let ()
  	(define-record-type alpha
  	  (fields (mutable   a)
		  (immutable b)
		  (mutable   c)))
  	(define {O alpha}
  	  (make-alpha 1 2 3))
	(set! (O a) 11)
	#;(set! (O b) 22)
	(set! (O c) 33)
  	(values (tag-getter O [a])
  		(tag-getter O [b])
  		(tag-getter O [c])))
    => 11 2 33)

  (check	;record with parent
      (let ()
  	(define-record-type alpha
  	  (fields (mutable a)
		  (immutable b)
		  (mutable   c)))
  	(define-record-type beta
  	  (parent alpha)
  	  (fields (mutable d)
		  (mutable e)
		  (mutable f)))
  	(define {O beta}
  	  (make-beta 1 2 3 4 5 6))
	(set! (O a) 11)
	#;(set! (O b) 22)
	(set! (O c) 33)
	(set! (O d) 44)
	(set! (O e) 55)
	(set! (O f) 66)
  	(list (tag-getter O [a])
	      (tag-getter O [b])
	      (tag-getter O [c])
	      (tag-getter O [d])
	      (tag-getter O [e])
	      (tag-getter O [f])))
    => '(11 2 33 44 55 66))

  #t)


(parametrise ((check-test-name	'lambda-retvals))

  (check	;demo
      (3 positive?)
    => #t)

  (check	;demo
      (((<fixnum>)(+ 1 2)) positive?)
    => #t)

;;; --------------------------------------------------------------------
;;; nested lambda calls

  (check	;nested field accessor
      (((lambda ({_ <fixnum>} {a <fixnum>} {b <fixnum>})
  	  (+ a b))
  	1 2) positive?)
    => #t)

  (check	;nested getter accessor
      (((lambda ({_ <string>} {a <fixnum>})
  	  (number->string a))
  	123) [1])
    => #\2)

  #t)


(parametrise ((check-test-name	'define-retvals))

  (check	;nested field accessor
      (let ()
	(define ({fun <fixnum>} {a <fixnum>} {b <fixnum>})
  	  (+ a b))
	((fun 1 2) positive?))
    => #t)

  (check	;nested getter
      (let ()
	(define ({fun <string>} {a <fixnum>})
  	  (number->string a))
	((fun 123) [1]))
    => #\2)

  (check
      ;;Function application nested in getter nested in field accessor.
      (let ()
	(define ({fun <string>} {a <fixnum>})
  	  (number->string a))
	(((fun 123) [1]) numeric?))
    => #t)

  #t)


(parametrise ((check-test-name	'let-retvals))

  (check
      (let ((O 123))
	(O positive?))
    => #t)

;;; --------------------------------------------------------------------

  (check	;nested field accessor
      (let ((fun (lambda ({_ <fixnum>} {a <fixnum>} {b <fixnum>})
		   (+ a b))))
	((fun 1 2) positive?))
    => #t)

  (check	;nested getter
      (let ((fun (lambda ({_ <string>} {a <fixnum>})
		   (number->string a))))
	((fun 123) [1]))
    => #\2)

  (check
      ;;Function application nested in getter nested in field accessor.
      (let ((fun (lambda ({_ <string>} {a <fixnum>})
		   (number->string a))))
	(((fun 123) [1]) numeric?))
    => #t)

  #t)


(parametrise ((check-test-name	'expansion-of))

  (print-gensym #f)

  (check
      (expansion-of 123)
    => '(quote 123))

  (check
      (expansion-of (+ 1 2))
    => '((primitive +) (quote 1) (quote 2)))

  ;;These are not  executed because the output  of EXPANSION-OF contains
  ;;gensyms which do not match the sexp we write here.
  (when #f
    (check
	(expansion-of (lambda (a b) (+ a b)))
      => '(case-lambda
	   ((a b)
	    ((primitive +) a b))))

    (check
	(internal-body
	  (define-syntax (doit stx)
	    (syntax-case stx ()
	      ((_ ?a ?b)
	       #'(vector ?a ?b))))
	  (expansion-of (doit 1 2)))
      => '((primitive vector) (quote 1) (quote 2)))

    (check
	(internal-body
	  (define-struct alpha
	    (a b c))
	  (define O
	    (alpha (1 2 3)))
	  (expansion-of (slot-ref O a)))
      => '(alpha-a O))
    (void))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'typ.set-identifier-object-type-spec! 'scheme-indent-function 1)
;; eval: (put 'catch-syntax-violation 'scheme-indent-function 1)
;; eval: (put 'catch-expand-time-signature-violation 'scheme-indent-function 1)
;; End:
