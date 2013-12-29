;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: basic tests for labels
;;;Date: Tue May 22, 2012
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
(import (nausicaa)
  (rnrs mutable-pairs)
  (rnrs mutable-strings)
  (vicare numerics constants)
  (only (vicare language-extensions)
	define-inline
	module)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing tags core features: labels\n")


;;;; helpers

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
		(syntax-violation-subform E))
	       (else E))
       . ?body))))


(define-label <the-list>
  (parent <top>)
  (protocol (lambda ()
	      list))
  (predicate list?)
  (virtual-fields (mutable car car set-car!)
		  (mutable cdr cdr set-cdr!)
		  (immutable length length))
  (methods (reverse reverse)
	   (append append))
  (getter (lambda (stx tag)
	    (syntax-case stx ()
	      ((?var ((?index)))
	       #'(list-ref ?var ?index)))))
  (setter (lambda (stx tag)
	    (syntax-case stx ()
	      ((?var ((?index)) ?val)
	       #'(%list-set! ?var ?index ?val))))))

  (define (%list-set! (ell <the-list>) idx val)
    (let loop (((L <the-list>) ell) (i 0) (j idx))
      (if (= i j)
	  (set! (L car) val)
	(loop (L cdr) (+ 1 i) j))))

;;; --------------------------------------------------------------------

(define-label <the-pair>
  (protocol (lambda ()
	      cons))
  (virtual-fields (mutable (car <the-pair>) car set-car!)
		  (mutable (cdr <the-pair>) cdr set-cdr!)))


(parametrise ((check-test-name	'internal-definition-bindings))

;;; label <the-list>

  (check	;access to binding
      (let ()
  	(<the-list> b '(1 2 3))
        b)
    => '(1 2 3))

  (check	;mutation of binding
      (let ()
  	(<the-list> b '(1 2 3))
  	(set! b '(4 5 6))
        b)
    => '(4 5 6))

  (check	;maker
      (let ()
  	(<the-list> b (<the-list> (1 2 3)))
        b)
    => '(1 2 3))

  (check	;predicate
      (let ()
  	(<the-list> b (<the-list> (1 2 3)))
        ((<the-list> #:predicate) b))
    => #t)

  (check	;predicate application
      (let ()
  	(<the-list> b (<the-list> (1 2 3)))
        (<the-list> #:is-a? b))
    => #t)

  (check	;predicate
      (for-all (<the-list> #:predicate)
	'((1 2) (3 4) (5 6)))
    => #t)

  (check	;internal definition syntax
      (let ()
  	(<the-list> b (<> (1 2 3)))
        ((<the-list> #:predicate) b))
    => #t)

  (check	;access to fields
      (let ()
  	(<the-list> b '(1 2 3))
        (vector (b car) (b cdr) (b length)))
    => '#(1 (2 3) 3))

  (check	;field mutation
      (let ()
  	(<the-list> b (list 1 2 3))
  	(set! (b car) 99)
  	(set! (b cdr) 88)
        b)
    => '(99 . 88))

  (check	;method call
      (let ()
  	(<the-list> b '(1 2 3))
  	(b reverse))
    => '(3 2 1))

  (check	;method call
      (let ((a '(1 2 3)))
  	(<the-list> b a)
  	(b append '(4 5 6)))
    => '(1 2 3 4 5 6))

  (check	;getter
      (let ()
  	(<the-list> b '(1 2 3))
  	(vector (b[0]) (b[1]) (b[2])))
    => '#(1 2 3))

  (check	;setter, syntax 1
      (let ()
  	(<the-list> b (list 1 2 3))
  	(set! (b[0]) 99)
  	b)
    => '(99 2 3))

  (check	;setter, syntax 1
      (let ()
  	(<the-list> b (list 1 2 3))
  	(set! (b[0]) 99)
  	(b[0]))
    => 99)

  (check	;setter, syntax 1
      (let ()
  	(<the-list> b (list 1 2 3))
  	(set! (b[0]) 99)
  	(set! (b[1]) 98)
  	(set! (b[2]) 97)
  	(vector (b[0]) (b[1]) (b[2])))
    => '#(99 98 97))

  (check	;setter, syntax 1
      (let ()
  	(<the-list> b (list 1 2 3))
  	(set! (b[0]) 99)
  	(set! (b[1]) 98)
  	(set! (b[2]) 97)
  	b)
    => '(99 98 97))

  (check	;setter, syntax 2
      (let ()
  	(<the-list> b (list 1 2 3))
  	(set! b[0] 99)
  	b)
    => '(99 2 3))

  (check	;setter, syntax 2
      (let ()
  	(<the-list> b (list 1 2 3))
  	(set! b[0] 99)
  	(b[0]))
    => 99)

  (check	;setter, syntax 2
      (let ()
  	(<the-list> b (list 1 2 3))
  	(set! b[0] 99)
  	(set! b[1] 98)
  	(set! b[2] 97)
  	(vector (b[0]) (b[1]) (b[2])))
    => '#(99 98 97))

;;; --------------------------------------------------------------------
;;; label <the-pair>

  (check	;access to binding
      (let ()
  	(<the-pair> b '(1 . 2))
        b)
    => '(1 . 2))

  (check	;mutation of binding
      (let ()
  	(<the-pair> b '(1 . 2))
  	(set! b '(4 . 5))
        b)
    => '(4 . 5))

  (check	;maker
      (let ()
  	(<the-pair> b (<the-pair> (1 2)))
        (pair? b))
    => #t)

  (check	;access to fields
      (let ()
  	(<the-pair> b '(1 . 2))
        (vector (b car) (b cdr)))
    => '#(1 2))

  (check	;field mutation
      (let ()
  	(<the-pair> b (cons 1 2))
  	(set! (b car) 99)
  	(set! (b cdr) 88)
        b)
    => '(99 . 88))

  (check	;field mutation
      (let ()
  	(<the-pair> b (cons 1000 2000))
  	(set! (b car) 99)
  	(set! (b cdr) 88)
	(vector (b car) (b cdr)))
    => '#(99 88))

  (check	;recursive access to fields
      (let ()
  	(<the-pair> b '((1 . 2) . (3 . 4)))
	(vector (b car car) (b car cdr)
		(b cdr car) (b cdr cdr)))
    => '#(1 2 3 4))

  (check	;recursive access to fields
      (let ()
  	(<the-pair> b '(((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8))))
	(vector (b car car car)
		(b car car cdr)

		(b car cdr car)
		(b car cdr cdr)

		(b cdr car car)
		(b cdr car cdr)

		(b cdr cdr car)
		(b cdr cdr cdr)))
    => '#(1 2 3 4 5 6 7 8))

  (check	;recursive mutation of fields
      (let ()
  	(<the-pair> b (cons (cons 1 2) (cons 3 4)))
	(set! (b car car) 19)
	(set! (b car cdr) 29)
	(set! (b cdr car) 39)
	(set! (b cdr cdr) 49)
	(vector (b car car) (b car cdr)
		(b cdr car) (b cdr cdr)))
    => '#(19 29 39 49))

  (check	;recursive mutation of fields
      (let ()
  	(<the-pair> b (cons (cons (cons 1 2)
				  (cons 3 4))
			    (cons (cons 5 6)
				  (cons 7 8))))

	(set! (b car car car) 19)
	(set! (b car car cdr) 29)

	(set! (b car cdr car) 39)
	(set! (b car cdr cdr) 49)

	(set! (b cdr car car) 59)
	(set! (b cdr car cdr) 69)

	(set! (b cdr cdr car) 79)
	(set! (b cdr cdr cdr) 89)

	b)
    => '(((19 . 29) . (39 . 49)) . ((59 . 69) . (79 . 89))))

  #t)


(parametrise ((check-test-name	'let-style-bindings))

  (check	;access to binding
      (let (((b <the-list>) '(1 2 3)))
	b)
    => '(1 2 3))

  (check	;mutation of binding
      (let (((a <the-list>) '(1 2 3)))
	(set! a '(4 5 6))
	a)
    => '(4 5 6))

  (check	;maker
      (let (((b <the-list>) (<the-list> (1 2 3))))
        b)
    => '(1 2 3))

  (check	;predicate
      (let (((b <the-list>) (<the-list> (1 2 3))))
        ((<the-list> #:predicate) b))
    => #t)

  (check	;access to fields
      (let (((b <the-list>) '(1 2 3)))
	(vector (b car) (b cdr) (b length)))
    => '#(1 (2 3) 3))

  (check	;field mutation
      (let (((b <the-list>) (list 1 2 3)))
	(set! (b car) 99)
	(set! (b cdr) 88)
	b)
    => '(99 . 88))

  (check	;method call
      (let (((b <the-list>) '(1 2 3)))
	(b reverse))
    => '(3 2 1))

  (check	;method call
      (let (((b <the-list>) '(1 2 3)))
	(b append '(4 5 6)))
    => '(1 2 3 4 5 6))

  (check	;getter
      (let (((b <the-list>) '(1 2 3)))
	(vector (b[0]) (b[1]) (b[2])))
    => '#(1 2 3))

  (check	;setter, syntax 1
      (let (((b <the-list>) (list 1 2 3)))
	(set! (b[0]) 1099)
	b)
    => '(1099 2 3))

  (check	;setter, syntax 2
      (let (((b <the-list>) (list 1 2 3)))
	(set! b[0] 99)
	b)
    => '(99 2 3))

  #t)


(parametrise ((check-test-name	'vector-labels))

  (define-label <a-vector>
    (predicate vector?)
    (virtual-fields (immutable length vector-length))
    (getter (lambda (stx tag)
	      (syntax-case stx ()
		((?var ((?index)))
		 #'(vector-ref ?var ?index)))))
    (setter (lambda (stx tag)
	      (syntax-case stx ()
		((?var ((?index)) ?val)
		 #'(vector-set! ?var ?index ?val))))))

  (define-label <a-vector-of-numbers>
    (parent <a-vector>)
    (predicate (lambda ((V <a-vector>))
		 (let loop ((i 0))
		   (or (= i (V length))
		       (and (number? (V (i)))
			    (loop (+ 1 i))))))))

  (define-label <a-vector-of-integers>
    (parent <a-vector-of-numbers>)
    (predicate (lambda ((V <a-vector>))
		 (let loop ((i 0))
		   (or (= i (V length))
		       (and (integer? (V (i)))
			    (loop (+ 1 i))))))))

;;; --------------------------------------------------------------------
;;; predicate

  (check (is-a? '(1 2 3) <a-vector>)			=> #f)
  (check (is-a? '#(1 2 3) <a-vector>)			=> #t)
  (check ((<a-vector> #:predicate) '(1 2 3))			=> #f)
  (check ((<a-vector> #:predicate) '#(1 2 3))			=> #t)

  (check (is-a? '(1 2 3) <a-vector-of-numbers>)		=> #f)
  (check (is-a? '#(1 #\2 3) <a-vector-of-numbers>)	=> #f)
  (check (is-a? '#(1 2 3) <a-vector-of-numbers>)	=> #t)
  (check ((<a-vector-of-numbers> #:predicate) '(1 2 3))		=> #f)
  (check ((<a-vector-of-numbers> #:predicate) '#(1 #\2 3))		=> #f)
  (check ((<a-vector-of-numbers> #:predicate) '#(1 2 3))		=> #t)

  (check (is-a? '(1 2 3) <a-vector-of-integers>)	=> #f)
  (check (is-a? '#(1 #\2 3) <a-vector-of-integers>)	=> #f)
  (check (is-a? '#(1 2.2 3) <a-vector-of-integers>)	=> #f)
  (check (is-a? '#(1 2 3) <a-vector-of-integers>)	=> #t)

;;; --------------------------------------------------------------------
;;; getters and setters

  (check
      (let (((o <a-vector>) (vector 1 2 3)))
	(set! o[1] #\a)
	(list (o[0]) (o[1]) (o[2])))
    => '(1 #\a 3))

  (check
      (let (((o <a-vector-of-numbers>) (vector 1 2 3)))
	(set! o[1] #\a)
	(list (o [0]) (o [1]) (o [2])))
    => '(1 #\a 3))

  (check
      (let (((o <a-vector-of-integers>) (vector 1 2 3)))
	(set! o[1] #\a)
	(list (o [0]) (o [1]) (o [2])))
    => '(1 #\a 3))

  #t)


(parametrise ((check-test-name	'shadowing))

  (define-syntax my-define-condition-type
    (syntax-rules ()
      ((_ ?type ?supertype ?constructor ?predicate (?field ?accessor) ...)
       (begin
	 (with-label-shadowing (?supertype)
	   (define-condition-type the-type
	       ?supertype ?constructor ?predicate
	       (?field ?accessor) ...))
	 (define-label ?type
	   (shadows the-type)
	   (maker (syntax-rules ()
		    ((_ (?arg (... ...)))
		     (?constructor ?arg (... ...)))))
	   (predicate ?predicate)
	   (virtual-fields (immutable ?field ?accessor) ...))))))

  (define-label &my-warning
    (maker (syntax-rules ()
	     ((_ ())
	      (make-warning))))
    (predicate warning?)
    (shadows &warning))

  (my-define-condition-type &warning-with-fields
    &my-warning make-warning-with-fields warning-with-fields?
    (a warning-with-fields-a)
    (b warning-with-fields-b))

  (check
      (let ((E (&my-warning ())))
	(list (warning? E)
	      ((&my-warning #:predicate) E)
	      (is-a? E &my-warning)
	      ))
    => '(#t #t #t))

  (check
      (let (((E &warning-with-fields) (&warning-with-fields (1 2))))
	(list (warning-with-fields? E)
	      (&warning-with-fields #:is-a? E)
	      (is-a? E &warning-with-fields)
	      (warning? E)
	      (warning-with-fields-a E)
	      (warning-with-fields-b E)
	      (E a)
	      (E b)))
    => '(#t #t #t #t 1 2 1 2))

  #t)


(parametrise ((check-test-name	'mixins))

  (let ()	;single label mixin

    (define-mixin <pair-stuff>
      (virtual-fields (mutable car car set-car!)
		      (mutable cdr cdr set-cdr!)))

    (define-label <a-list>
      (virtual-fields (immutable length length))
      (mixins <pair-stuff>))

    (check
	(let (((o <a-list>) '(1 2 3)))
	  (vector (o car) (o cdr) (o length)))
      => '#(1 (2 3) 3))

    #f)

  (let ()	;multiple label mixins

    (define-mixin <car-stuff>
      (virtual-fields (mutable car car set-car!)))

    (define-mixin <cdr-stuff>
      (virtual-fields (mutable cdr cdr set-cdr!)))

    (define-label <a-list>
      (virtual-fields (immutable length length))
      (mixins <car-stuff> <cdr-stuff>))

    (check
	(let (((o <a-list>) '(1 2 3)))
	  (vector (o car) (o cdr) (o length)))
      => '#(1 (2 3) 3))

    #f)

  (let ()	;single label mixin, single mixin mixin

    (define-mixin <car-stuff>
      (virtual-fields (mutable car car set-car!)))

    (define-mixin <pair-stuff>
      (virtual-fields (mutable cdr cdr set-cdr!))
      (mixins <car-stuff>))

    (define-label <a-list>
      (virtual-fields (immutable length length))
      (mixins <pair-stuff>))

    (check
	(let (((o <a-list>) '(1 2 3)))
	  (vector (o car) (o cdr) (o length)))
      => '#(1 (2 3) 3))

    #f)

  (let ()	;single label mixin, multiple mixin mixin

    (define-mixin <car-stuff>
      (virtual-fields (mutable car car set-car!)))

    (define-mixin <cdr-stuff>
      (virtual-fields (mutable cdr cdr set-cdr!)))

    (define-mixin <pair-stuff>
      (mixins <car-stuff>)
      (mixins <cdr-stuff>))

    (define-label <a-list>
      (virtual-fields (immutable length length))
      (mixins <pair-stuff>))

    (check
	(let (((o <a-list>) '(1 2 3)))
	  (vector (o car) (o cdr) (o length)))
      => '#(1 (2 3) 3))

    #f)

  #t)


(parametrise ((check-test-name	'nested-accesor))

  (let ()

    (define-label <alpha>
      (virtual-fields a)
      (getter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)))
		   #'123))))
      (method (doit O n)
	(cons 456 n)))

    (define-label <beta>
      (virtual-fields (b <alpha>)))

    (define-label <gamma>
      (virtual-fields (c <beta>)))

    (define (<alpha>-a o) 'a)
    (define (<beta>-b  o) 'b)
    (define (<gamma>-c o) 'c)

    (check
	(let (((O <gamma>) #f))
	  (list (O c b a) (O c b) (O c)))
      => '(a b c))

    (check
	(let (((O <gamma>) #f))
	  (O c b[99]))
      => 123)

    (check
	(let (((O <gamma>) #f))
	  (O c b doit 99))
      => '(456 . 99))

    #f)

  (let ()	;redirect to parent

    (define-label <base>
      (virtual-fields a)
      (getter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)))
		   #'123))))
      (method (doit O n)
	(cons 456 n)))

    (define-label <alpha>
      (parent <base>))

    (define-label <beta>
      (virtual-fields (b <alpha>)))

    (define-label <gamma>
      (virtual-fields (c <beta>)))

    (define (<base>-a  o) 'a)
    (define (<beta>-b  o) 'b)
    (define (<gamma>-c o) 'c)

    (check
	(let (((O <gamma>) #f))
	  (list (O c b a) (O c b) (O c)))
      => '(a b c))

    (check
	(let (((O <gamma>) #f))
	  (O c b[99]))
      => 123)

    (check
	(let (((O <gamma>) #f))
	  (O c b doit 99))
      => '(456 . 99))

    #f)

  #t)


(parametrise ((check-test-name	'nested-mutator))

  (let ()

    (define-label <alpha>
      (virtual-fields (mutable a))
      (setter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)) ?val)
		   #'(list 123 ?key ?val))))))

    (define-label <beta>
      (virtual-fields (mutable (b <alpha>))))

    (define-label <gamma>
      (virtual-fields (mutable (c <beta>))))

    (define (<alpha>-a o) 'a)
    (define (<beta>-b  o) 'b)
    (define (<gamma>-c o) 'c)
    (define (<alpha>-a-set! o v) 'A)
    (define (<beta>-b-set!  o v) 'B)
    (define (<gamma>-c-set! o v) 'C)

    (check
	(let (((O <gamma>) #f))
	  (list (set! (O c b a) 1)
		(set! (O c b) 2)
		(set! (O c) 3)))
      => '(A B C))

    (check
	(let (((O <gamma>) #f))
	  (set! (O c b[777]) 999))
      => '(123 777 999))

    #f)

  (let ()	;redirect to parent

    (define-label <base>
      (virtual-fields (mutable a))
      (setter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)) ?val)
		   #'(list 123 ?key ?val))))))

    (define-label <alpha>
      (parent <base>))

    (define-label <beta>
      (virtual-fields (mutable (b <alpha>))))

    (define-label <gamma>
      (virtual-fields (mutable (c <beta>))))

    (define (<base>-a o) 'a)
    (define (<beta>-b  o) 'b)
    (define (<gamma>-c o) 'c)
    (define (<base>-a-set!  o v) 'A)
    (define (<beta>-b-set!  o v) 'B)
    (define (<gamma>-c-set! o v) 'C)

    (check
	(let (((O <gamma>) #f))
	  (list (set! (O c b a) 1)
		(set! (O c b) 2)
		(set! (O c) 3)))
      => '(A B C))

    (check
	(let (((O <gamma>) #f))
	  (set! (O c b[777]) 999))
      => '(123 777 999))

    #f)

  #t)


(parametrise ((check-test-name	'protocols))

  (let ()	;common protocol

    (define-label <the-pair>
      (protocol (lambda ()
		  cons))
      (virtual-fields (immutable (car <the-pair>) car)
		      (immutable (cdr <the-pair>) cdr)))

    (check
	(let ()
	  (<the-pair> P (<> (1 2)))
	  (vector (P car) (P cdr)))
      => '#(1 2))

    #f)

  (let ()	;public protocol

    (define-label <the-pair>
      (public-protocol (lambda ()
			 cons))
      (virtual-fields (immutable (car <the-pair>) car)
		      (immutable (cdr <the-pair>) cdr)))

    (check
	(let ()
	  (<the-pair> P (<> (1 2)))
	  (vector (P car) (P cdr)))
      => '#(1 2))

    #f)

  (let ()	;common and public protocol

    (define-label <the-pair>
      (public-protocol (lambda ()
			 cons))
      (protocol (lambda ()
		  (lambda args
		    '(3 . 4))))
      (virtual-fields (immutable (car <the-pair>) car)
		      (immutable (cdr <the-pair>) cdr)))

    (check
	(let ()
	  (<the-pair> P (<> (1 2)))
	  (vector (P car) (P cdr)))
      => '#(1 2))

    #f)

  #t)


(parametrise ((check-test-name	'maker))

  (define-label <a-pair>
    (maker (lambda (stx)
	     (syntax-case stx ()
	       ((_ (?a ?d))
		#'(cons ?a ?d)))))
    (virtual-fields (immutable car car)
		    (immutable cdr cdr)))


  (check
      (let (((o <a-pair>) (<a-pair> (1 2))))
	o)
    => '(1 . 2))

  (check
      (let (((o <a-pair>) (<a-pair> (1 2))))
	(o car))
    => 1)

  (check
      (let (((o <a-pair>) (<a-pair> (1 2))))
	(o cdr))
    => 2)

  #t)


(parametrise ((check-test-name	'predicates))

  (let ()

    (define-label <list>
      (predicate list?)
      (virtual-fields (immutable car car)
		      (immutable cdr cdr)))

    (define-label <list-of-numbers>
      (parent <list>)
      (predicate (lambda (ell)
		   (for-all number? ell))))

    (define L '(1 2 3))
    (define M '(a b c))
    (define N "abc")

    (check ((<list> #:predicate) L)		=> #t)
    (check ((<list-of-numbers> #:predicate) L)	=> #t)
    (check (<list>? L)			=> #t)
    (check (<list-of-numbers>? L)	=> #t)

    (check ((<list> #:predicate) M)			=> #t)
    (check ((<list-of-numbers> #:predicate) M)	=> #f)

    (check ((<list> #:predicate) N)			=> #f)
    (check ((<list-of-numbers> #:predicate) N)	=> #f)

    #f)

  #t)


(parametrise ((check-test-name	'satisfactions))

  (define-syntax general-label-constraint
    (lambda (stx)
      (syntax-case stx (parent virtual-fields mutable immutable
			       methods getter setter nongenerative
			       shadows)
	((_ (?label-id ?constructor ?predicate)
	    (parent ?parent)
	    (virtual-fields (mutable   (?vm-field ?vm-tag) ?vm-accessor ?vm-mutator) ...)
	    (virtual-fields (immutable (?vi-field ?vi-tag) ?vi-accessor #f)          ...)
	    (methods (?method-name . ?method-callable) ...)
	    (getter ?getter)
	    (setter ?setter)
	    (nongenerative ?uid)
	    (shadows ?shadows))
	 #f))))

  (module ()

    (define-syntax constraint
      (syntax-rules (parent virtual-fields mutable immutable methods
			    getter setter nongenerative shadows)
	((_ (?name ?constructor ?predicate)
	    (parent ?parent)
	    (virtual-fields (mutable (?m-field ?m-tag) ?m-accessor #f) ...)
	    (virtual-fields (immutable (?i-field ?i-tag) ?i-accessor #f) ...)
	    (methods)
	    (getter #f)
	    (setter #f)
	    (nongenerative ?uid)
	    (shadows #f))
	 #f)))

    (define-label <alpha>
      (virtual-fields a b c)
      (satisfies constraint general-label-constraint))

    (define (<alpha>-a o) #f)
    (define (<alpha>-b o) #f)
    (define (<alpha>-c o) #f)

    #f)

  (define-label <label-with-everything>
    (satisfies general-label-constraint)
    (parent <top>)
    (nongenerative the-uid-of-<label-with-everything>)
    (virtual-fields (mutable (c <a-tag>) c-accessor c-mutator)
		    (immutable (d <d-tag>) d-accessor))
    (setter (lambda args #f))
    (getter (lambda args #f))
    (method (doit obj)
      #f))

    (define (c-accessor obj) #f)
    (define (d-accessor obj) #f)
    (define (c-mutator obj val) #f)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'my-define-condition-type 'scheme-indent-function 1)
;; coding: utf-8-unix
;; End:
