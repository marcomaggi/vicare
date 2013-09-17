;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (nausicaa language oopp)
;;;Date: Thu Apr  1, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (rnrs mutable-pairs)
  (vicare checks)
  (prefix (libtest records-lib) test.))

(check-set-mode! 'report-failed)
(check-display "*** testing classes basics\n")


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


(parametrise ((check-test-name	'top-internal-definition-bindings))

  (check	;access to binding
      (let ()
  	(<top> b '(1 2 3))
        b)
    => '(1 2 3))

  (check	;mutation of binding
      (let ()
  	(<top> b '(1 2 3))
  	(set! b '(4 5 6))
        b)
    => '(4 5 6))

  (check	;maker
      (catch-syntax-violation #f
	(%eval '(<top> (1 2 3))))
    => #f)

  (check	;predicate
      (let ()
	(define b '(1 2 3))
	((<top>) b))
    => #t)

  (check	;dispatch request
      (let ()
  	(<top> b (lambda (x) (vector x)))
        (b 123))
    => '#(123))

  (check	;field mutation
      (catch-syntax-violation #f
	(%eval '(let ()
		  (<top> b '(1 2 3))
		  (set! (b car) 99))))
    => #f)

  (check	;getter syntax -> function application
      (let ()
	(<top> b (lambda (x) (vector x)))
	(b[list]))
    => '#(()))

  (check	;setter, syntax 1
      (catch-syntax-violation #f
	(%eval '(let ()
		  (<top> b '(1 2 3))
		  (set! (b[0]) 99)
		  b)))
    => #f)

  (check	;setter, syntax 2
      (catch-syntax-violation #f
	(%eval '(let ()
		  (<top> b '(1 2 3))
		  (set! b[0] 99)
		  b)))
    => #f)

  #t)


(parametrise ((check-test-name	'top-let-style-bindings))

  (check	;access to binding
      (let (((b <top>) '(1 2 3)))
	b)
    => '(1 2 3))

  (check	;mutation of binding
      (let (((a <top>) '(1 2 3)))
	(set! a '(4 5 6))
	a)
    => '(4 5 6))

  (check	;predicate
      (let (((b <top>) '(1 2 3)))
        ((<top>) b))
    => #t)

  (check	;dispatch request
      (let (((b <top>) (lambda (x) (vector x))))
        (b 123))
    => '#(123))

  (check	;field mutation
      (catch-syntax-violation #f
	(%eval '(let (((b <top>) '(1 2 3)))
		  (set! (b car) 99))))
    => #f)

  (check	;getter syntax -> function application
      (let (((b <top>) (lambda (x) (vector x))))
	(b[list]))
    => '#(()))

  (check	;setter, syntax 1
      (catch-syntax-violation #f
	(%eval '(let (((b <top>) '(1 2 3)))
		  (set! (b[0]) 99)
		  b)))
    => #f)

  (check	;setter, syntax 2
      (catch-syntax-violation #f
	(%eval '(let (((b <top>) '(1 2 3)))
		  (set! b[0] 99)
		  b)))
    => #f)

  #t)



(parametrise ((check-test-name	'definition-simple))

  (let ()

    (define-class <alpha>
      (fields (mutable a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (<alpha>-a o))
      => 123)

    #f)

  (let ()

    (define-class (<alpha> make-<alpha> <alpha>?)
      (fields (mutable a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (<alpha>-a o))
      => 123)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid name
      (catch-syntax-violation #f
	(%eval '(define-class 123
		  (fields a b c))))
    => 123)

  (check	;invalid constructor
      (catch-syntax-violation #f
	(%eval '(define-class (<alpha> 123 <alpha>?)
		  (fields a b c))))
    => '(<alpha> 123 <alpha>?))

  (check 	;unknown clause
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (woppa 123))))
    => '((woppa 123)))

  #t)


(parametrise ((check-test-name	'definition-parent-clause))

  (let ()	;inherit with PARENT

    (define-class <alpha>
      (parent <top>)
      (nongenerative alpha)
      (fields (mutable a)))

    (define-class <beta>
      (parent <alpha>)
      (protocol (lambda (alpha-maker)
      		  (lambda (a b)
      		    (let ((beta-maker (alpha-maker a)))
    		      (beta-maker b)))))
      (sealed #t)
      (opaque #t)
      (nongenerative test:beta)
      (fields (immutable b)))

    (check
    	(let ((o (make-<beta> 1 2)))
    	  (list (<alpha>-a o)
    		(<beta>-b o)))
      => '(1 2))

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid value
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (parent 123)
		  (fields a b c))))
    => '(parent 123))

  (check 	;multiple PARENT is bad
      (catch-syntax-violation #f
	(%eval '(let ()
		  (define-class <alpha>
		    (parent ciao)
		    (parent hello))
		  #f)))
    => '(parent hello))

  #t)


(parametrise ((check-test-name	'definition-common-protocol-clause))

  (let ()	;no default constructor arguments

    (define-class <alpha>
      (protocol (lambda (make-top)
		  (lambda (a b)
		    ((make-top) a b))))
      (fields a b))

    (check
	(let (((o <alpha>) (<alpha> (1 2))))
	  ((<alpha>) o))
      => #t)

    #f)

  (let ()	;one default constructor argument

    (define-class <alpha>
      (protocol (lambda (make-top)
		  (lambda (a)
		    ((make-top) a 2))))
      (fields a b))

    (check
	(let (((o <alpha>) (<alpha> (1))))
	  ((<alpha>) o))
      => #t)

    #f)

  (let ()	;all default constructor arguments

    (define-class <alpha>
      (fields a b c)
      (protocol (lambda (make-top)
		  (lambda ()
		    ((make-top) 1 2 3)))))

    (check
	(let ((o (make-<alpha>)))
	  (list (<alpha>-a o)
		(<alpha>-b o)
		(<alpha>-c o)))
      => '(1 2 3))

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple PROTOCOL is bad
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (protocol ciao)
		  (protocol hello))))
    => '(protocol hello))

  (check   ;PROTOCOL expression argument does not evaluate to a function
      (catch-assertion #f
	(let ()
	  (define-class <beta>
	    (fields a b c)
	    (protocol 123))
	  #f))
    => '(123))

  (check	;PROTOCOL function does not return a function
      (catch-assertion #f
	(let ()
	  (define-class <beta>
	    (fields a b c)
	    (protocol (lambda (make-top)
			123)))
	  #f))
    => '(123))

  #t)


(parametrise ((check-test-name	'definition-public-protocol-clause))

  (let ()	;public protocol

    (define-class <alpha>
      (fields a b)
      (public-protocol (lambda (make-top)
			 (lambda (a b)
			   ((make-top) a b))))
      (super-protocol (lambda (make-top)
			(lambda (a b)
			  #f))))

    (check
	((<alpha>) (<alpha> (1 2)))
      => #t)

    (check
	((<alpha>) (<alpha> (1 2)))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (check	;multiple PUBLIC-ROTOCOL is bad
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (public-protocol ciao)
		  (public-protocol hello))))
    => '(public-protocol hello))

  #t)


(parametrise ((check-test-name	'definition-super-protocol-clause))

  (let ()	;superclass protocol

    (define-class <alpha>
      (fields a b)
      (public-protocol (lambda (make-top)
			 (lambda (a b)
			   #f)))
      (super-protocol (lambda (make-top)
			(lambda (a b)
			  ((make-top) a b)))))

    (define-class <beta>
      (parent <alpha>)
      (fields c d)
      (public-protocol (lambda (make-alpha)
			 (lambda (a b c d)
			   ((make-alpha a b) c d))))
      (super-protocol (lambda (make-alpha)
			(lambda (a b c d)
			  #f))))

    (check
	((<beta>) (<beta> (1 2 3 4)))
      => #t)

    (check
	((<beta>) (<beta> (1 2 3 4)))
      => #t)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple SUPERCLASS-ROTOCOL is bad
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (super-protocol ciao)
		  (super-protocol hello))))
    => '(super-protocol hello))

  #t)


(parametrise ((check-test-name	'definition-from-fields-constructor))

  (let ()	;from-fields constructor

    (define-class <alpha>
      (fields a b)
      (protocol (lambda (make-top)
		  (lambda ()
		    ((make-top) 1 2)))))

    (define-class <beta>
      (parent <alpha>)
      (fields c d)
      (protocol (lambda (make-alpha)
		  (lambda ()
		    ((make-alpha) 3 4)))))

    (check
	(let ((o (<beta> ())))
	  (list (<alpha>-a o) (<alpha>-b o)
		(<beta>-c o) (<beta>-d o)))
      => '(1 2 3 4))

    (check
	(let ((o (make-from-fields <beta> #\a #\b #\c #\d)))
	  (list (<alpha>-a o) (<alpha>-b o)
		(<beta>-c o) (<beta>-d o)))
      => '(#\a #\b #\c #\d))

    #f)

  #t)


(parametrise ((check-test-name	'definition-sealed-clause))

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid sealed
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (sealed 123)
		  (fields a b c))))
    => '(sealed 123))

  (check	;multiple SEALED is bad
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (sealed #t)
		  (sealed #f))))
    => '(sealed #f))

  #t)


(parametrise ((check-test-name	'definition-opaque-clause))

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid opaque
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (opaque 123)
		  (fields a b c))))
    => '(opaque 123))

  (check	;multiple OPAQUE is bad
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (opaque #t)
		  (opaque #f))))
    => '(opaque #f))

  #t)


(parametrise ((check-test-name	'definition-nongenerative-clause))

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid value
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (nongenerative 123)
		  (fields a b c))))
    => '(nongenerative 123))

  (check	;multiple non-empty NONGENERATIVE is bad
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (nongenerative ciao)
		  (nongenerative hello))))
    => '(nongenerative hello))

  (check	;multiple empty NONGENERATIVE is bad
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (nongenerative)
		  (nongenerative))))
    => '(nongenerative))

  #t)


(parametrise ((check-test-name	'definitions-fields-clause))

  (let ()

    (define-class <alpha>
      (nongenerative a1)
      (fields (mutable a)
	      (immutable b)
	      c))

    (check
	(let ((o (make-<alpha> 1 2 3)))
	  (list (<alpha>-a o)
		(<alpha>-b o)
		(<alpha>-c o)))
      => '(1 2 3))

    (check
	(let ((o (make-<alpha> 1 2 3)))
	  (<alpha>-a-set! o 10)
	  (list (<alpha>-a o)
		(<alpha>-b o)
		(<alpha>-c o)))
      => '(10 2 3))

    #f)

  (check
      (letrec ()
	(define-class <alpha>
	  (nongenerative a2)
	  (fields (mutable a)
		  (immutable b)
		  c))
	(define o (make-<alpha> 1 2 3))
	(<alpha>-a-set! o #t)
	(<alpha>-a o))
    => #t)

;;; --------------------------------------------------------------------
;;; accessor and mutator names

  (let ()

    (define-class <alpha>
      (nongenerative a3)
      (fields (mutable a access-a mutate-a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (access-a o))
      => 123)

    (check
	(let ((o (make-<alpha> 123)))
	  (mutate-a o 456)
	  (access-a o))
      => 456)

    #f)

  (let ()

    (define-class <alpha>
      (nongenerative a4)
      (fields (immutable a access-a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (access-a o))
      => 123)

    #f)

;;; --------------------------------------------------------------------
;;; LET/TAGS macro

  (let ()	;one field

    (define-class <alpha>
      (nongenerative a5)
      (fields (mutable a)))

    (define r (make-<alpha> 123))
    (define s (make-<alpha> #\a))
    (define t (make-<alpha> 1.0))

    (with-tags ((r <alpha>)
		(s <alpha>)
		(t <alpha>))
      (check
	  (list (r a) (s a) (t a))
	=> '(123 #\a 1.0))

      (set! (r a) 456)
      (set! (s a) #\b)
      (set! (t a) 2.0)

      (check
	  (list (r a) (s a) (t a))
	=> '(456 #\b 2.0))

      #f))

  (let ()	;more fields
    (define-class <alpha>
      (nongenerative a6)
      (fields (mutable a)
	      (mutable b)))

    (define r (make-<alpha> 1 2))
    (define s (make-<alpha> #\a #\b))
    (define t (make-<alpha> 1.0 2.0))
    (with-tags ((r <alpha>)
		 (s <alpha>)
		 (t <alpha>))
      (check
	  (list (r a) (s a) (t a)
		(r b) (s b) (t b))
	=> '(1 #\a 1.0  2 #\b 2.0))

      (set! (r a) 3)
      (set! (s a) #\c)
      (set! (t a) 3.0)

      (check
	  (list (r a) (s a) (t a)
		(r b) (s b) (t b))
	=> '(3 #\c 3.0  2 #\b 2.0))

      (set! (r b) 4)
      (set! (s b) #\d)
      (set! (t b) 4.0)

      (check
	  (list (r a) (s a) (t a)
		(r b) (s b) (t b))
	=> '(3 #\c 3.0  4 #\d 4.0))

      #f)
    #f)

;;; these tests use the record definitions from (libtest records-lib)

  (check
      (let ((r (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(with-tags ((r test.<gamma>))
	  (list (r a) (r b) (r c)
		(r d) (r e) (r f)
		(r g) (r h) (r i))))
    => '(1 2 3 4 5 6 7 8 9))

  (check
      (let ((r (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(with-tags ((r test.<gamma>))
	  (list (r a) (r b) (r c)
		(r g) (r h) (r i))))
    => '(1 2 3 7 8 9))

;;; common inheritance

  (let ()	;fields

    (define-class <alpha>
      (nongenerative a7)
      (fields a b z))

    (define-class <beta>
      (nongenerative b1)
      (parent <alpha>)
      (fields c d z))

    (check	;accessing fields of both class and superclass
    	(let ((p (<beta> (1 2 3 4 5 6))))
    	  (with-tags ((p <beta>))
    	    (list (p a) (p b) (p c) (p d))))
      => '(1 2 4 5))

    (check	;precedence of subclass fields
    	(let ((p (<beta> (1 2 3 4 5 6))))
    	  (with-tags ((p <beta>))
    	    (p z)))
      => 6)

    (check	;custom precedence of superclass fields
    	(let ((p (<beta> (1 2 3 4 5 6))))
    	  (with-tags ((p <alpha>))
    	    (p z)))
      => 3)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple FIELDS is fine
      (let ()
	(define-class <alpha>
	  (nongenerative a8)
	  (fields a b)
	  (fields c d))
	#t)
    => #t)

  (check	;attempt to mutate immutable field
      (catch-syntax-violation #f
  	(%eval '(letrec ()
		  (define-class <alpha>
		    (fields (mutable a)
			    (immutable b)
			    c))
		  (define o (make-<alpha> 1 2 3))
		  (with-tags ((o <alpha>))
		    (set! (o b) #f)))))
    => 'b)

  (check	;attempt to mutate immutable field
      (catch-syntax-violation #f
  	(%eval '(letrec ()
		  (define-class <alpha>
		    (fields (mutable a)
			    (immutable b)
			    c))
		  (define o (make-<alpha> 1 2 3))
		  (with-tags ((o <alpha>))
		    (set! (o c) #f)))))
    => 'c)

  #t)


(parametrise ((check-test-name	'definitions-virtual-fields-clause))

  (let ()	;immutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;mutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named immutable virtual fields accessor

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator the-numerator)
		      (immutable denominator the-denominator)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named virtual fields accessor and mutator

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator the-numerator the-numerator-set!)
		      (mutable denominator the-denominator the-denominator-set!)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (define (the-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named virtual fields accessor and mutator
		;mixed mutable and immutable

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator the-numerator)
		      (mutable denominator the-denominator the-denominator-set!)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (define (the-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------
;;; inline expressions

  (let ()	;immutable virtual fields with inline expression

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator
				 (lambda (o)
				   (numerator (<fraction>-number o))))
		      (immutable denominator
				 (lambda (o)
				   (denominator (<fraction>-number o))))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    #f)

  (let () ;mutable virtual fields with accessor and mutator inline expressions

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator
			       (lambda (o)
				 (numerator (<fraction>-number o)))
			       (lambda (o v)
				 (let ((n (<fraction>-number o)))
				   (<fraction>-number-set! o (/ v (denominator n))))))
		      (mutable denominator)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

  (let ()	;mutable virtual fields with accessor inline expressions

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator
			       (lambda (o)
				 (numerator (<fraction>-number o)))
			       <fraction>-numerator-set!)
		      (mutable denominator)))

    (define <fraction>-numerator-set!
      (lambda (o v)
	(let ((n (<fraction>-number o)))
	  (<fraction>-number-set! o (/ v (denominator n))))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

  (let ()	;mutable virtual fields with mutator inline expressions

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator
			       <fraction>-numerator
			       (lambda (o v)
				 (let ((n (<fraction>-number o)))
				   (<fraction>-number-set! o (/ v (denominator n))))))
		      (mutable denominator)))

    (define <fraction>-numerator
      (lambda (o)
	(numerator (<fraction>-number o))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------
;;; WITH-TAGS usage

  (let ()

    (define-class <alpha>
      (fields a b z)
      (virtual-fields (immutable x <alpha>-a)
		      (immutable y <alpha>-b)))

    (define-class <beta>
      (parent <alpha>)
      (fields c d z)
      (virtual-fields (immutable m <beta>-c)
		      (immutable y <beta>-d)))

    (check	;accessing fields of both class and superclass
    	(let ((p (<beta> (1 2 3  4 5 6))))
    	  (with-tags ((p <beta>))
    	    (list (p x) (p m))))
      => '(1 4))

    (check	;precedence of subclass fields
    	(let ((p (<beta> (1 2 3  4 5 6))))
    	  (with-tags ((p <beta>))
    	    (p y)))
      => 5)

    (check	;custom precedence of superclass fields
    	(let ((p (<beta> (1 2 3
			    4 5 6))))
    	  (with-tags ((p <alpha>))
    	    (p y)))
      => 2)

    #f)

  (let ()	;immutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o numerator)))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o denominator)))
      => 3)

    #f)

  (let ()	;mutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o numerator)))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o denominator)))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (set! (o numerator) 5)
	    (o number)))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (set! (o denominator) 5)
	    (o number)))
      => 2/5)

    #f)

;;; the following tests use the records from (libtest records-lib)

  (let ()
    (define r (test.<alpha> (123 #\a 1.0)))

    (with-tags ((r test.<alpha>))

      (check
      	  (list (r a) (r b) (r c))
      	=> '(123 #\a 1.0))

      (set! (r a) 456)
      (set! (r c) 2.0)

      (check
	  (list (r a) (r b) (r c))
	=> '(456 #\a 2.0))

      #f)
    #f)

;;; --------------------------------------------------------------------
;;; virtual field name collision

  (let ()	;concrete fields name collision

    (define-record-type <alpha>
      (fields a))

    (define-record-type <beta>
      (parent <alpha>)
      (fields a))

    (let ((o (make-<beta> 1 2)))

      (check
	  (<alpha>-a o)
	=> 1)

      (check
	  (<beta>-a o)
	=> 2)

      #f)

    #f)

  (let ()	;virtual fields

    (define-class <alpha>
      (virtual-fields a))

    (define-class <beta>
      (parent <alpha>)
      (virtual-fields a))

    (define (<alpha>-a o)
      1)

    (define (<beta>-a o)
      2)

    (let ((o (make-<beta>)))

      (check
	  (with-tags ((o <alpha>))
	    (o a))
	=> 1)

      (check
	  (with-tags ((o <beta>))
	    (o a))
	=> 2)

      #f)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple VIRTUAL-FIELDS is fine
      (let ()
	(define-class <alpha>
	  (virtual-fields a b)
	  (virtual-fields c d))
	#t)
    => #t)

  (check	;attempt to mutate immutable virtual-field
      (catch-syntax-violation #f
  	(%eval '(letrec ()
		  (define-class <alpha>
		    (fields (mutable a)
			    c)
		    (virtual-fields (immutable b)))
		  (define (<alpha>-b o)
		    #t)
		  (define o (make-<alpha> 1 2 3))
		  (with-tags ((o <alpha>))
		    (set! (o b) #f)))))
    => 'b)

  (check	;attempt to mutate immutable virtual field
      (catch-syntax-violation #f
  	(%eval '(letrec ()
		  (define-class <alpha>
		    (fields (mutable a)
			    (immutable b))
		    (virtual-fields c))
		  (define (<alpha>-c o)
		    #t)
		  (define o (make-<alpha> 1 2 3))
		  (with-tags ((o <alpha>))
		    (set! (o c) #f)))))
    => 'c)

  #t)


(parametrise ((check-test-name	'definitions-methods-clause))

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (methods numerator denominator))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (methods (numerator)
	       (denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (methods (numerator   fraction-numerator)
	       (denominator fraction-denominator)))

    (define (fraction-numerator o)
      (numerator (<fraction>-number o)))

    (define (fraction-denominator o)
      (denominator (<fraction>-number o)))

    #f)

;;; --------------------------------------------------------------------
;;; WITH-TAGS usage

  (let ()	;methods

    (define-class <alpha>
      (fields a b z)
      (methods (x <alpha>-a)
	       (y <alpha>-b)))

    (define-class <beta>
      (parent <alpha>)
      (fields c d z)
      (methods (m <beta>-c)
	       (y <beta>-d)))

    (check	;accessing methods of both class and superclass
    	(let ((p (<beta>
		  (1 2 3
		     4 5 6))))
    	  (with-tags ((p <beta>))
    	    (list (p x) (p m))))
      => '(1 4))

    (check	;precedence of subclass methods
    	(let ((p (<beta>
		  (1 2 3
		   4 5 6))))
    	  (with-tags ((p <beta>))
    	    (p y)))
      => 5)

    (check	;custom precedence of superclass methods
    	(let ((p (<beta>
		  (1 2 3
		   4 5 6))))
    	  (with-tags ((p <alpha>))
    	    (p y)))
      => 2)

    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator))
      (methods (denominator)
	       product
	       (the-list the-list-function)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-product (o <fraction>) lambda)
      (set! (o numerator) (* (o numerator) lambda)))

    (define (the-list-function (o <fraction>) . ell)
      (cons (o numerator) ell))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o numerator)))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o denominator)))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o product 10)
	    (o numerator)))
      => 20)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o the-list 10 11 12 13)))
      => '(2 10 11 12 13))

    #f)

;;; --------------------------------------------------------------------
;;; method name collision

  (let ()

    (define-class <alpha>
      (methods a))

    (define-class <beta>
      (parent <alpha>)
      (methods a))

    (define (<alpha>-a o)
      1)

    (define (<beta>-a o)
      2)

    (let ((o (make-<beta>)))

      (check
	  (with-tags ((o <alpha>))
	    (o a))
	=> 1)

      (check
	  (with-tags ((o <beta>))
	    (o a))
	=> 2)

      #f)
    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple METHODS is fine
      (let ()
	(define-class <alpha>
	  (methods a b)
	  (methods c d))
	#t)
    => #t)

  #t)


(parametrise ((check-test-name	'definition-method-clause))

  (let ()	; methods

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator))
      (method (denominator o)
	(denominator (<fraction>-number o)))
      (method (product (o <fraction>) lambda)
	(set! (o numerator) (* (o numerator) lambda)))
      (method the-list
	(lambda ((o <fraction>) . ell)
	  (cons (o numerator) ell))))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o numerator)))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o denominator)))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o product 10)
	    (o numerator)))
      => 20)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o the-list 10 11 12 13)))
      => '(2 10 11 12 13))

    #f)

  (let ()	; methods syntaxes

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator))
      (method-syntax denominator
	(syntax-rules ()
	  ((_ ?obj)
	   (let ((o ?obj))
	     (denominator (<fraction>-number o))))))
      (method (product (o <fraction>) lambda)
	(set! (o numerator) (* (o numerator) lambda)))
      (method the-list
	(lambda ((o <fraction>) . ell)
	  (cons (o numerator) ell))))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o numerator)))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o denominator)))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o product 10)
	    (o numerator)))
      => 20)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o the-list 10 11 12 13)))
      => '(2 10 11 12 13))

    #f)

  (let ()	; method syntax with ellipsis in the body, FIXME!!!

    (define-class <stuff>
      (fields (mutable a))
      (method-syntax doit
	(syntax-rules ()
	  ((_ ?obj ?item ...)
	   (let (((o <stuff>) ?obj))
	     (list (o a) ?item ...))))))

    (check
	(let (((o <stuff>) (<stuff> (#\a))))
	  (o doit 1 2 3))
      => '(#\a 1 2 3))

    #f)

;;; --------------------------------------------------------------------
;;; method name collision

  (let ()

    (define-class <alpha>
      (method (a o)
	1))

    (define-class <beta>
      (parent <alpha>)
      (method (a o)
	2))

    (check
	(let ((o (make-<beta>)))
	  (with-tags ((o <alpha>))
	    (o a)))
      => 1)

    (check
	(let ((o (make-<beta>)))
	  (with-tags ((o <beta>))
	    (o a)))
      => 2)

    #f)
  #t)


(parametrise ((check-test-name	'definition-duplicated-id-errors))

  (check	;duplicated field name, FIELDS/VIRTUAL-FIELDS
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (fields a)
		  (virtual-fields a))))
    => 'a)

  (check	;duplicated field name, FIELDS/METHODS
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (fields a)
		  (methods a))))
    => 'a)

  (check	;duplicated field name, FIELDS/METHOD
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (fields a)
		  (method (a o)
		    #t))))
    => 'a)

  (check	;duplicated field name, VIRTUAL-FIELDS/METHODS
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (virtual-fields a)
		  (methods a))))
    => 'a)

  (check	;duplicated field name, VIRTUAL-FIELDS/METHOD
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (virtual-fields a)
		  (method (a o)
		    #t))))
    => 'a)

  (check	;duplicated field name, METHODS/METHOD
      (catch-syntax-violation #f
	(%eval '(define-class <alpha>
		  (methods a)
		  (method (a o)
		    #t))))
    => 'a)

  #t)


(parametrise ((check-test-name	'definition-maker))

  (let ()	;only mandatory arguments

    (define-class <alpha>
      (fields a b)
      (maker (lambda (stx)
	       (syntax-case stx ()
		 ((_ (?a ?b))
		  #'(make-<alpha> ?a ?b))))))

    (check
	(let ((o (<alpha> (1 2))))
	  (with-tags ((o <alpha>))
	    (list (o a) (o b))))
      => '(1 2))

    #f)

  (let ()	;only optional arguments

    (define-class <alpha>
      (fields a b)
      (maker (lambda (stx)
	       (syntax-case stx ()
		 ((_ ())
		  #'(make-<alpha> 1 2))
		 ((_ ((a: ?a)))
		  (eq? 'a (syntax->datum #'a:))
		  #'(make-<alpha> ?a 2))
		 ((_ ((b: ?b)))
		  (eq? 'b (syntax->datum #'b:))
		  #'(make-<alpha> 1 ?b))
		 ((_ ((b: ?b) (a: ?a)))
		  (and (eq? 'b (syntax->datum #'b:))
		       (eq? 'a (syntax->datum #'a:)))
		  #'(make-<alpha> ?a ?b))
		 ))))

    (check
	(let ((o (<alpha> ())))
	  (with-tags ((o <alpha>))
	    (list (o a) (o b))))
      => '(1 2))

    (check
	(let ((o (<alpha> ((a 10)))))
	  (with-tags ((o <alpha>))
	    (list (o a) (o b))))
      => '(10 2))

    (check
	(let ((o (<alpha> ((b 20)))))
	  (with-tags ((o <alpha>))
	    (list (o a) (o b))))
      => '(1 20))

    (check
	(let ((o (<alpha> ((b 20) (a 10)))))
	  (with-tags ((o <alpha>))
	    (list (o a) (o b))))
      => '(10 20))

    #f)

;;; --------------------------------------------------------------------
;;; no maker, defaults to the public protocol

  (let ()
    (define-class <alpha>
      (fields a b))

    (check	;when no MAKER is defined default to public constructor
	(is-a? (<alpha> (1 2)) <alpha>)
      => #t)

    #f)

  #t)


(parametrise ((check-test-name		'definition-finaliser))

  (let ()

    (define-class <alpha>
      (fields a b c)
      (finaliser (lambda (S)
		   (add-result 'finalising))))

    (check
	(with-result
	 (parametrise ((record-guardian-logger (lambda (S E action)
						 (add-result action))))
	   (pretty-print (make-<alpha> 1 2 3) (current-error-port))
	   (collect)))
      => `(,(void) (registration
		    before-destruction
		    finalising
		    after-destruction)))

    #f)

  #t)


(parametrise ((check-test-name	'definition-abstract-class))

  (let ()

    (define-class <alpha>
      (abstract)
      (fields a))

    (define-class <beta>
      (parent <alpha>)
      (fields b))

    (check
	(let ((o (<beta> (1 2))))
	  (with-tags ((o <beta>))
	    (list (o a) (o b))))
      => '(1 2))

    #f)

  (let ()

    (define-class <alpha>
      (abstract)
      (fields a))

    (check
	(catch-assertion #f
	  (<alpha> (1)))
      => '())

    #f)

  #t)


(parametrise ((check-test-name	'with-tags))

  (let ()

    (define-class <alpha>
      (fields a))

    (check
	(let ((o (<alpha> (1))))
	  (with-tags ((o <alpha>))
	    (o a)))
      => 1)

    (check
	(let ((o (<alpha> (1))))
	  (with-tags ((o <alpha>))
	    (define b 2)
	    (list b (o a))))
      => '(2 1))

    #f)

  #t)


(parametrise ((check-test-name	'lambda-with))

;;; untyped

  (let ((f (lambda (a)
	     a)))
    (check (f 123) => 123)
    #f)

  (let ((f (lambda (a b)
	     (list a b))))
    (check (f 1 2) => '(1 2))
    #f)

  (let ((f (lambda (a b c)
	     (list a b c))))
    (check (f 1 2 3) => '(1 2 3))
    #f)

  (let ((f (lambda args
	     (list->vector args))))
    (check (f) => '#())
    (check (f 1) => '#(1))
    (check (f 1 2) => '#(1 2))
    (check (f 1 2 3) => '#(1 2 3))
    #f)

  (let ((f (lambda (a . rest)
	     (vector a rest))))
    (check (f 1) => '#(1 ()))
    (check (f 1 2) => '#(1 (2)))
    (check (f 1 2 3 4) => '#(1 (2 3 4)))
    #f)

  (let ((f (lambda (a b . rest)
	     (vector a b rest))))
    (check (f 1 2) => '#(1 2 ()))
    (check (f 1 2 3) => '#(1 2 (3)))
    (check (f 1 2 3 4) => '#(1 2 (3 4)))
    #f)

;;; --------------------------------------------------------------------
;;; typed

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define <fraction>-numerator
      (lambda ((o <fraction>))
	(numerator (o number))))

    (define <fraction>-numerator-set!
      (lambda ((o <fraction>) v)
	(set! (o number) (/ v (denominator (o number))))))

    (define <fraction>-denominator
      (lambda ((o <fraction>))
	(denominator (o number))))

    (define <fraction>-denominator-set!
      (lambda ((o <fraction>) (v <top>))
	(set! (o number) (/ (numerator (o number)) v))))

    (let ((f (lambda ((a <fraction>))
	       (a numerator))))
      (check (f (make-<fraction> 2/3)) => 2)
      #f)

    (let ((f (lambda ((a <fraction>) (b <complex>))
	       (list (a numerator) (b magnitude)))))
      (check (f (make-<fraction> 2/3) -4) => '(2 4))
      #f)

    (let ((f (lambda ((a <fraction>) b (c <fraction>))
	       (list (a numerator) b (c denominator)))))
      (check (f (make-<fraction> 2/3) 4 (make-<fraction> 5/6)) => '(2 4 6))
      #f)

    (let ((f (lambda ((a <fraction>) . rest)
	       (vector (a numerator) rest))))
      (check (f (make-<fraction> 11/12)) => '#(11 ()))
      (check (f (make-<fraction> 11/12) 2) => '#(11 (2)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 (2 3 4)))
      #f)

    (let ((f (lambda ((a <fraction>) b . rest)
	       (vector (a numerator) b rest))))
      (check (f (make-<fraction> 11/12) 2) => '#(11 2 ()))
      (check (f (make-<fraction> 11/12) 2 3) => '#(11 2 (3)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 2 (3 4)))
      #f)

    ;;With definition in the body.
    (let ((f (lambda ((a <fraction>) b . rest)
	       (define r rest)
	       (vector (a numerator) b r))))
      (check (f (make-<fraction> 11/12) 2) => '#(11 2 ()))
      (check (f (make-<fraction> 11/12) 2 3) => '#(11 2 (3)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 2 (3 4)))
      #f)

;;; --------------------------------------------------------------------

    (check
    	(let (((o <fraction>) (make-<fraction> 2/3)))
    	  (o numerator))
      => 2)

    (check
    	(let (((o <fraction>) (make-<fraction> 2/3)))
    	  (o numerator))
      => 2)

    (check
    	(let ((o (make-<fraction> 2/3)))
    	  (with-tags ((o <fraction>))
    	    (o denominator)))
      => 3)

    (check
    	(let ((o (make-<fraction> 2/3)))
    	  (with-tags ((o <fraction>))
    	    (set! (o numerator) 5)
    	    (o number)))
      => 5/3)

    (check
    	(let ((o (make-<fraction> 2/3)))
    	  (with-tags ((o <fraction>))
    	    (set! (o denominator) 5)
    	    (o number)))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------
;;; use the records from (libtest records-lib)

  (check
      (let ((r (test.<gamma> (1 2 3 4 5 6 7 8 9)))
	    (f (lambda ((r test.<gamma>))
		 (list (r a) (r b) (r c)
		       (r d) (r e) (r f)
		       (r g) (r h) (r i)))))
	(f r))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (libtest records-lib)
      (let ((r (test.<gamma> (1 2 3 4 5 6 7 8 9)))
	    (s (test.<beta>  (10 20 30 40 50 60)))
	    (f (lambda ((r test.<gamma>) (s test.<beta>))
		 (list (r a) (r g) (s a) (s d)))))
	(f r s))
    => '(1 7 10 40))

;;; --------------------------------------------------------------------

  (let ()	;single args, tagged

    (define func
      (lambda/tags #(args <list>)
	(args length)))

    (check
	(func 1 2 3)
      => 3)

    (check
    	((lambda/tags #(args <list>)
    	   (args car))
	 1 2 3)
      => 1)

    (check
    	((lambda/tags #(args <list>)
    	   (args cdr))
	 1 2 3)
      => '(2 3))

    #f)

  #t)


(parametrise ((check-test-name	'define-with))

  (let ()
    (define (f a)
      a)
    (check (f 123) => 123)
    #f)

  (let ()
    (define (f a b)
      (list a b))
    (check (f 1 2) => '(1 2))
    #f)

  (let ()
    (define (f a b c)
      (list a b c))
    (check (f 1 2 3) => '(1 2 3))
    #f)

  (let ()
    (define (f . args)
      (list->vector args))
    (check (f) => '#())
    (check (f 1) => '#(1))
    (check (f 1 2) => '#(1 2))
    (check (f 1 2 3) => '#(1 2 3))
    #f)

  (let ()
    (define (f a . rest)
      (vector a rest))
    (check (f 1) => '#(1 ()))
    (check (f 1 2) => '#(1 (2)))
    (check (f 1 2 3 4) => '#(1 (2 3 4)))
    #f)

  (let ()
    (define (f a b . rest)
      (vector a b rest))
    (check (f 1 2) => '#(1 2 ()))
    (check (f 1 2 3) => '#(1 2 (3)))
    (check (f 1 2 3 4) => '#(1 2 (3 4)))
    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define (<fraction>-numerator (o <fraction>))
      (numerator (o number)))

    (define (<fraction>-numerator-set! (o <fraction>) (v <top>))
      (set! (o number) (/ v (denominator (o number)))))

    (define (<fraction>-denominator (o <fraction>))
      (denominator (o number)))

    (define (<fraction>-denominator-set! (o <fraction>) (v <top>))
      (set! (o number) (/ (numerator (o number)) v)))

    (check
	(let (((o <fraction>) (make-<fraction> 2/3)))
	  (o numerator))
      => 2)

    (check
	(let (((o <fraction>) (make-<fraction> 2/3)))
	  (o numerator))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (o denominator)))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (set! (o numerator) 5)
	    (o number)))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-tags ((o <fraction>))
	    (set! (o denominator) 5)
	    (o number)))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------
;;; use the records from (libtest records-lib)

  (check
      (let ((r (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(define (f (r test.<gamma>))
	  (list (r a) (r b) (r c)
		(r d) (r e) (r f)
		(r g) (r h) (r i)))
	(f r))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (libtest records-lib)
      (let ((r (test.<gamma> (1 2 3 4 5 6 7 8 9)))
	    (s (test.<beta>  (10 20 30 40 50 60))))
	(define (f (r test.<gamma>) (s test.<beta>))
	  (list (r a) (r g) (s a) (s d)))
	(f r s))
    => '(1 7 10 40))

  #t)


(parametrise ((check-test-name	'case-lambda-with))

;;; untyped

  (let ((f (case-lambda
	    ((a)
	     a))))
    (check (f 123) => 123)
    #f)

  (let ((f (case-lambda
	    ((a b)
	     (list a b)))))
    (check (f 1 2) => '(1 2))
    #f)

  (let ((f (case-lambda
	    ((a b c)
	     (list a b c)))))
    (check (f 1 2 3) => '(1 2 3))
    #f)

  (let ((f (case-lambda
	    (args
	     (list->vector args)))))
    (check (f) => '#())
    (check (f 1) => '#(1))
    (check (f 1 2) => '#(1 2))
    (check (f 1 2 3) => '#(1 2 3))
    #f)

  (let ((f (case-lambda
	    ((a . rest)
	     (vector a rest)))))
    (check (f 1) => '#(1 ()))
    (check (f 1 2) => '#(1 (2)))
    (check (f 1 2 3 4) => '#(1 (2 3 4)))
    #f)

  (let ((f (case-lambda
	    ((a b . rest)
	     (vector a b rest)))))
    (check (f 1 2) => '#(1 2 ()))
    (check (f 1 2 3) => '#(1 2 (3)))
    (check (f 1 2 3 4) => '#(1 2 (3 4)))
    #f)

;;; --------------------------------------------------------------------
;;; typed

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define <fraction>-numerator
      (case-lambda
       (((o <fraction>))
	(numerator (o number)))))

    (define <fraction>-numerator-set!
      (case-lambda
       (((o <fraction>) v)
	(set! (o number) (/ v (denominator (o number)))))))

    (define <fraction>-denominator
      (case-lambda
       (((o <fraction>))
	(denominator (o number)))))

    (define <fraction>-denominator-set!
      (case-lambda
       (((o <fraction>) (v <top>))
	(set! (o number) (/ (numerator (o number)) v)))))

    (let ((f (case-lambda
	      (((a <fraction>))
	       (a numerator)))))
      (check (f (make-<fraction> 2/3)) => 2)
      #f)

    (let ((f (case-lambda
	      (((a <fraction>) (b <complex>))
	       (list (a numerator) (b magnitude))))))
      (check (f (make-<fraction> 2/3) -4) => '(2 4))
      #f)

    (let ((f (case-lambda
	      (((a <fraction>) b (c <fraction>))
	       (list (a numerator) b (c denominator))))))
      (check (f (make-<fraction> 2/3) 4 (make-<fraction> 5/6)) => '(2 4 6))
      #f)

    (let ((f (case-lambda
	      (((a <fraction>) . rest)
	       (vector (a numerator) rest)))))
      (check (f (make-<fraction> 11/12)) => '#(11 ()))
      (check (f (make-<fraction> 11/12) 2) => '#(11 (2)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 (2 3 4)))
      #f)

    (let ((f (case-lambda
	      (((a <fraction>) b . rest)
	       (vector (a numerator) b rest)))))
      (check (f (make-<fraction> 11/12) 2) => '#(11 2 ()))
      (check (f (make-<fraction> 11/12) 2 3) => '#(11 2 (3)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 2 (3 4)))
      #f)

    #f)

;;; --------------------------------------------------------------------
;;; multiple clauses

  (let ((f (case-lambda
	    ((a) a)
	    ((a b) (list a b)))))
    (check (f 1) => 1)
    (check (f 1 2) => '(1 2))
    #f)

  (let ((f (case-lambda
	    ((a)	a)
	    ((a b)	(list a b))
	    ((a b c)	(list a b c))
	    )))
    (check (f 1) => 1)
    (check (f 1 2) => '(1 2))
    (check (f 1 2 3) => '(1 2 3))
    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define <fraction>-numerator
      (case-lambda
       (((o <fraction>))
	(numerator (o number)))))

    (define <fraction>-denominator
      (case-lambda
       (((o <fraction>))
	(denominator (o number)))))

    (let ((f (case-lambda
	      (((a <fraction>))
	       (a numerator))
	      (((a <fraction>) (b <string>))
	       (list (a numerator) (b length))))))
      (check (f (make-<fraction> 2/3)) => 2)
      (check (f (make-<fraction> 2/3) "ciao") => '(2 4))
      #f)

    (let ((f (case-lambda
	      (((a <fraction>))
	       (a numerator))
	      (((a <fraction>) (b <string>))
	       (list (a numerator) (b length)))
	      (((a <fraction>) (b <string>) (c <char>))
	       (list (a numerator) (b length) (c upcase))))))
      (check (f (make-<fraction> 2/3)) => 2)
      (check (f (make-<fraction> 2/3) "ciao") => '(2 4))
      (check (f (make-<fraction> 2/3) "ciao" #\a) => '(2 4 #\A))
      #f)

    #f)

;;; --------------------------------------------------------------------
;;; use the records from (libtest records-lib)

  (check
      (let ((r (test.<gamma> (1 2 3 4 5 6 7 8 9)))
	    (f (case-lambda
		(((r test.<gamma>))
		 (list (r a) (r b) (r c)
		       (r d) (r e) (r f)
		       (r g) (r h) (r i))))))
	(f r))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (libtest records-lib)
      (let ((r (test.<gamma> (1 2 3 4 5 6 7 8 9)))
	    (s (test.<beta>  (10 20 30 40 50 60)))
	    (f (case-lambda
		(((r test.<gamma>) (s test.<beta>))
		 (list (r a) (r g) (s a) (s d))))))
	(f r s))
    => '(1 7 10 40))

  #t)


(parametrise ((check-test-name 'list-of-uids))

  (check
      (tag-unique-identifiers <top>)
    => '(nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers <pair>)
    => '(nausicaa:builtin:<pair> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers <textual-port>)
    => '(nausicaa:builtin:<textual-port> nausicaa:builtin:<port> nausicaa:builtin:<top>))

  (check
      (eq? (tag-unique-identifiers <list>)
	   (tag-unique-identifiers <list>))
    => #t)

  #t)


(parametrise ((check-test-name	'list-of-uids-of))

  (check
      (tag-unique-identifiers-of '(1 . 2))
    => '(nausicaa:builtin:<pair> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of '(1 2))
    => '(nausicaa:builtin:<list> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of #\a)
    => '(nausicaa:builtin:<char> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of "c")
    => '(nausicaa:builtin:<string> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of 'a)
    => '(nausicaa:builtin:<symbol> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of '#(1))
    => '(nausicaa:builtin:<vector> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of '#vu8(1))
    => '(nausicaa:builtin:<bytevector> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of (make-eq-hashtable))
    => '(nausicaa:builtin:<hashtable> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of (condition))
    => '(nausicaa:builtin:<condition>
	 nausicaa:builtin:<record>
	 nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of (current-output-port))
    => '(nausicaa:builtin:<output-port> nausicaa:builtin:<port> nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of 123)
    => '(nausicaa:builtin:<fixnum>
	 nausicaa:builtin:<integer>
	 nausicaa:builtin:<integer-valued>
	 nausicaa:builtin:<rational-valued>
	 nausicaa:builtin:<real>
	 nausicaa:builtin:<real-valued>
	 nausicaa:builtin:<complex>
	 nausicaa:builtin:<number>
	 nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of 1.2)
    => '(nausicaa:builtin:<rational>
	 nausicaa:builtin:<rational-valued>
	 nausicaa:builtin:<real>
	 nausicaa:builtin:<real-valued>
	 nausicaa:builtin:<complex>
	 nausicaa:builtin:<number>
	 nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of +1.2i)
    => '(nausicaa:builtin:<complex>
	 nausicaa:builtin:<number>
	 nausicaa:builtin:<top>))

  (check
      (tag-unique-identifiers-of values)
    => '(nausicaa:builtin:<procedure> nausicaa:builtin:<top>))

  (check
      (let ()
	(define-class <alpha>
	  (nongenerative test:<alpha>))
	(tag-unique-identifiers-of (<alpha> ())))
    => '(test:<alpha> nausicaa:builtin:<top>))

  #t)


(parametrise ((check-test-name 'predicates))

;;; These tests make use of the record types exported by (libtest records-lib).

  (let ((a (test.<alpha> (1 2 3)))
	(b (test.<beta>  (1 2 3  4 5 6))))

    (check
    	(is-a? a test.<alpha>)
      => #t)

    (check
    	(is-a? b test.<beta>)
      => #t)

    #f)

;;; --------------------------------------------------------------------
;;; Special syntax.

  (let ()

    (define-class <alpha>
      (fields a))

    (define-class <beta>
      (fields b))

    (define a (<alpha> (1)))
    (define b (<beta>  (2)))

    (check	;special syntax
    	((is-a? <> <beta>) b)
      => #t)

    (check	;special syntax
    	((is-a? <> <beta>) a)
      => #f)

    (check	;special syntax
    	(for-all (is-a? <> <alpha>) (list a a a))
      => #t)

    (check	;special syntax
    	(for-all (is-a? <> <alpha>) (list a b a))
      => #f)

    #f)

;;; --------------------------------------------------------------------
;;; Built-in types.

  (check-for-true	(is-a? 123 <fixnum>))
  (check-for-false	(is-a? #\a <fixnum>))

  (check-for-true	(is-a? 1 <integer>))
  (check-for-false	(is-a? 1.2 <integer>))

  (check-for-true	(is-a? 1/2 <rational>))
  (check-for-false	(is-a? 1+2i <rational>))

  (check-for-true	(is-a? 1.0 <integer-valued>))
  (check-for-false	(is-a? 1.1 <integer-valued>))

  (check-for-true	(is-a? 1/2 <rational-valued>))
  (check-for-false	(is-a? #\a <rational-valued>))

  (check-for-true	(is-a? 1.1 <flonum>))
  (check-for-false	(is-a? #\a <flonum>))

  (check-for-true	(is-a? 1.1 <real>))
  (check-for-false	(is-a? #\a <real>))

  (check-for-true	(is-a? 1.1 <real-valued>))
  (check-for-false	(is-a? #\a <real-valued>))

  (check-for-true	(is-a? 1.1+2i <complex>))
  (check-for-false	(is-a? #\a <complex>))

  (check-for-true	(is-a? 1 <number>))
  (check-for-false	(is-a? #\a <number>))

  (check-for-true	(is-a? #\a <char>))
  (check-for-false	(is-a? 1 <char>))

  (check-for-true	(is-a? "ciao" <string>))
  (check-for-false	(is-a? 123 <string>))

  (check-for-true	(is-a? '#(1 2 3) <vector>))
  (check-for-false	(is-a? "ciao" <vector>))

  (check-for-true	(is-a? '#vu8(1 2 3) <bytevector>))
  (check-for-false	(is-a? "ciao" <bytevector>))

  (check-for-true	(is-a? (make-eq-hashtable) <hashtable>))
  (check-for-false	(is-a? "ciao" <hashtable>))

  (check-for-true	(is-a? (open-string-input-port "ciao") <input-port>))
  (check-for-false	(is-a? 123 <input-port>))

  (check-for-true	(let-values (((port getter) (open-string-output-port)))
  			  (is-a? port <output-port>)))
  (check-for-false	(is-a? 123 <output-port>))

  ;; (check-for-true	(let-values (((port getter) (open-string-output-port)))
  ;; 			  (is-a? port <output-port>)))
  (check-for-false	(is-a? 123 <binary-port>))

  (check-for-true	(let-values (((port getter) (open-string-output-port)))
  			  (is-a? port <textual-port>)))
  (check-for-false	(is-a? 123 <textual-port>))

  (check-for-true	(is-a? (open-string-input-port "ciao") <port>))
  (check-for-false	(is-a? 123 <port>))

  (check-for-true	(is-a? (make-message-condition "ciao") <condition>))
  (check-for-false	(is-a? 123 <condition>))

  (check-for-true	(is-a? '(1 . 2) <pair>))
  (check-for-false	(is-a? 1 <pair>))

  (check-for-true	(is-a? '(1 2) <list>))
  (check-for-false	(is-a? '(1 . 2) <list>))

  #t)


(parametrise ((check-test-name 'slot-set-ref))

  (let ()	;mutable fields

    (define-class <alpha>
      (fields (mutable a)
	      (mutable b)))

    (check
	(let ((o (<alpha> (1 2))))
	  (list (slot-ref o a <alpha>)
		(slot-ref o b <alpha>)))
      => '(1 2))

    (check
	(let ((o (<alpha> (1 2))))
	  (slot-set! o a <alpha> 11)
	  (slot-set! o b <alpha> 22)
	  (list (slot-ref o a <alpha>)
		(slot-ref o b <alpha>)))
      => '(11 22))

    (check	;special syntax
	(let ((o (<alpha> (1 2))))
	  ((slot-set! <> a <alpha> <>) o 11)
	  ((slot-set! <> b <alpha> <>) o 22)
	  (list ((slot-ref <> a <alpha>) o)
		((slot-ref <> b <alpha>) o)))
      => '(11 22))

    #f)

  (let ()	;immutable fields

    (define-class <alpha>
      (fields (immutable a)
	      (immutable b)))

    (check
	(let ((o (<alpha> (1 2))))
	  (list (slot-ref o a <alpha>)
		(slot-ref o b <alpha>)))
      => '(1 2))

    #f)

  (check	;slot accessor as syntax
      (let ()
	(define-label <alpha>
	  (virtual-fields (immutable a)))
	(define-syntax <alpha>-a
	  (syntax-rules ()
	    ((_ ?o)
	     (car ?o))))
	(slot-ref '(1 2) a <alpha>))
    => 1)

  (check	;slot mutator as syntax
      (let ()
	(define-label <alpha>
	  (virtual-fields (mutable a)))
	(define-syntax <alpha>-a-set!
	  (syntax-rules ()
	    ((_ ?o ?v)
	     (set-car! ?o ?v))))
	(let ((o '(1 2)))
	  (slot-set! o a <alpha> 99)
	  o))
    => '(99 2))

;;; --------------------------------------------------------------------
;;; labels

  (let ()	;mutable fields

    (define-label <p>
      (protocol (lambda ()
		  cons))
      (virtual-fields (mutable a car set-car!)
		      (mutable b cdr set-cdr!)))

    (check
	(let ((o (<p> (1 2))))
	  (list (slot-ref o a <p>)
		(slot-ref o b <p>)))
      => '(1 2))

    (check
	(let ((o (<p> (1 2))))
	  (slot-set! o a <p> 11)
	  (slot-set! o b <p> 22)
	  (list (slot-ref o a <p>)
		(slot-ref o b <p>)))
      => '(11 22))

    (check	;special syntax
	(let ((o (<p> (1 2))))
	  ((slot-set! <> a <p> <>) o 11)
	  ((slot-set! <> b <p> <>) o 22)
	  (list ((slot-ref <> a <p>) o)
		((slot-ref <> b <p>) o)))
      => '(11 22))

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;no mutable fields
      (catch-syntax-violation #f
	(%eval '(let ()
		  (define-class <alpha>
		    (fields (immutable a)
			    (immutable b)))
		  (define o
		    (<alpha> (1 2)))
		  (slot-set! o a <alpha> 11))))
    => 'a)

  (check	;unknown field for slot-ref
      (catch-syntax-violation #f
	(%eval '(let ()
		  (define-class <alpha>
		    (fields (mutable a)
			    (mutable b)))
		  (define o
		    (<alpha> (1 2)))
		  (slot-ref o c <alpha>))))
    => 'c)

  (check	;unknown field for slot-set!
      (catch-syntax-violation #f
	(%eval '(let ()
		  (define-class <alpha>
		    (fields (mutable a)
			    (mutable b)))
		  (define o
		    (<alpha> (1 2)))
		  (slot-set! o c <alpha> 11))))
    => 'c)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'catch-assertion 'scheme-indent-function 1)
;; eval: (put 'catch-syntax-violation 'scheme-indent-function 1)
;; End:
