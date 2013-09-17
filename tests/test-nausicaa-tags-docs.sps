;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: examples used in the documentation
;;;Date: Mon May 28, 2012
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


#!r6rs
(import (vicare)
  (nausicaa language oopp)
  (nausicaa language builtins)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing tags features: documentation examples\n")


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


(parametrise ((check-test-name	'identifier-syntax))

  (let ()

    (define-syntax pair
      (syntax-rules ()))

    (define-label <pair>
      (maker (lambda (stx)
	       (syntax-case stx (pair car cdr)
		 ((?type (pair (car ?a) (cdr ?d)))
		  #'(?type (?a ?d)))
		 ((?type (?a ?d))
		  #'(cons ?a ?d)))))
      (virtual-fields (immutable car car)
		      (immutable cdr cdr)))

    (check
	(<pair> (pair
		 (car 1)
		 (cdr 2)))
      => '(1 . 2))

    (check
	(<pair> (1 2))
      => '(1 . 2))

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-label <list>
      (method (append3 a b c)
	(append a b c)))

    (<list> A '(1 2))
    (define B '(3 4))

    (check
	(A append3 B '(5 6))
      => '(1 2 3 4 5 6))

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-class <alpha>
      (fields (mutable v)))

    (define-class <beta>
      (fields (immutable (a <alpha>))))

    (define-class <gamma>
      (fields (immutable (b <beta>))))

    (<alpha> A (<> (1)))
    (<beta>  B (<> (A)))
    (<gamma> O (<> (B)))

    (check
	((<gamma>) O)
      => #t)

    (check
    	((<beta>) (O b))
      => #t)

    (check
    	((<alpha>) (O b a))
      => #t)

    (check
    	(O b a v)
      => 1)

    (set!/tags (O b a v) 2)

    (check
    	(O b a v)
      => 2)

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-label <vector>
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?idx)))
		   #'(vector-ref ?var ?idx)))))
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?idx)) ?expr)
		   #'(vector-set! ?var ?idx ?expr))))))

    (define-class <alpha>
      (fields (immutable (v <vector>))))

    (define-class <beta>
      (fields (immutable (a <alpha>))))

    (define-class <gamma>
      (fields (immutable (b <beta>))))

    (<alpha> A (<> ((vector 1 2 3))))
    (<beta>  B (<> (A)))
    (<gamma> O (<> (B)))

    (check
	((<gamma>) O)
      => #t)
    (check
	((<beta>) (O b))
      => #t)
    (check
	((<alpha>) (O b a))
      => #t)
    (check
	(O b a v)
      => '#(1 2 3))
    (check
	(O b a v[0])
      => 1)
    (check
	(O b a v[1])
      => 2)
    (check
	(O b a v[2])
      => 3)

    (set!/tags (O b a v[0]) 10)
    (set!/tags (O b a v[1]) 20)
    (set!/tags (O b a v[2]) 30)
    (check
	(O b a v)
      => '#(10 20 30))

    #f)

  #t)


(parametrise ((check-test-name	'parent))

  (let ()

    (define-label <car>
      (virtual-fields (immutable car car)))

    (define-label <car-and-cdr>
      (parent <car>)
      (virtual-fields (immutable cdr cdr)))

    (define-label <pair>
      (parent <car-and-cdr>)
      (protocol (lambda () cons))
      (predicate pair?))

    (<pair> O (<> (1 2)))

    (check ((<pair>) O)            => #t)
    (check ((<car-and-cdr>)  O)    => #t)
    (check ((<car>)  O)            => #t)
    (check ((<top>)  O)            => #t)

    (check (O car)                 => 1)
    (check (O cdr)                 => 2)

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-class <alpha>
      (fields a b))

    (define-label <beta>
      (parent <alpha>)
      (predicate <alpha>?)
      (protocol (lambda () make-<alpha>))
      (virtual-fields (immutable sum
				 (lambda/tags ((O <beta>))
				   (+ (O a) (O b))))))

    (<beta> O (<> (1 2)))

    (check ((<beta>)  O)           => #t)
    (check ((<alpha>) O)           => #t)
    (check ((<top>)   O)           => #t)

    (check (O a)                   => 1)
    (check (O b)                   => 2)
    (check (O sum)                 => 3)

    (let/tags (((O <beta>) (make-<beta> 1 2)))

      (check ((<beta>)  O)           => #t)
      (check ((<alpha>) O)           => #t)
      (check ((<top>)   O)           => #t)

      (check (O a)                   => 1)
      (check (O b)                   => 2)
      (check (O sum)                 => 3)

      #f)

    #f)

  #t)


(parametrise ((check-test-name	'setter-and-getter))

  (let ()	;getter

    (define-label <vector>
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?idx)))
		   #'(vector-ref ?var ?idx))))))

    (define-label <matrix>
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?row) (?col)))
		   #'(vector-ref
		      (vector-ref ?var ?row)
		      ?col))))))

    (check
	(let ()
	  (<vector> V '#(1 2 3))
	  (list (V[0])
		(V[1])
		(V[2])))
      => '(1 2 3))

    (check
	(let ()
	  (<matrix> M '#(#(1 2 3)
			 #(4 5 6)))
	  (list (M[0][2])
		(M[1][1])))
      => '(3 5))

    #f)

  (let ()	;setter

    (define-label <vector>
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?idx)) ?expr)
		   #'(vector-set! ?var ?idx ?expr))))))

    (define-label <matrix>
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?row) (?col)) ?expr)
		   #'(vector-set!
		      (vector-ref ?var ?row)
		      ?col ?expr))))))

    (check
	(let ()
	  ;;We can mutate only dynamically built objects.
	  (<vector> V (vector 1 2 3))
	  (set!/tags V[1] 77)
	  (set!/tags (V[2]) 99)
	  V)
      => '#(1 77 99))

    (check
	(let ()
	  (<matrix> M (vector (vector 1 2 3)
			      (vector 4 5 6)))
	  (set!/tags M[0][2] 77)
	  (set!/tags (M[1][1]) 99)
	  M)
      => '#(#(1 2 77) #(4 99 6)))

    #f)

  #t)


(parametrise ((check-test-name	'slots))

  (let ()

    (define-class <alpha>
      (fields (mutable a)
	      (mutable b)))

    (check
	(let ()
	  (<alpha> A (<> (1 2)))
	  (list (slot-ref A a <alpha>)
		(slot-ref A b <alpha>)))
      => '(1 2))

    (check
	(let ()
	  (<alpha> A (<> (1 2)))
	  (slot-set! A a <alpha> 10)
	  (slot-set! A b <alpha> 20)
	  (list (slot-ref A a <alpha>)
		(slot-ref A b <alpha>)))
      => '(10 20))

    (check
	(let ()
	  (<alpha> A (<> (1 2)))
	  (list ((slot-ref <> a <alpha>) A)
		((slot-ref <> b <alpha>) A)))
      => '(1 2))

    (check
	(let ()
	  (<alpha> A (<> (1 2)))
	  ((slot-set! <> a <alpha> <>) A 10)
	  ((slot-set! <> b <alpha> <>) A 20)
	  (list ((slot-ref <> a <alpha>) A)
		((slot-ref <> b <alpha>) A)))
      => '(10 20))

    #f)

  #t)


(parametrise ((check-test-name	'bindings))

  (check	;with-tags
      (let ()
	(define-class <alpha>
	  (fields a b))
	(define A (<alpha> (1 2)))
	(define V 123)
	(with-tags ((A <alpha>) V)
	  (vector (A a) V)))
    => '#(1 123))

  (check
      (let*/tags (((a <number>) 123)
		  ((b <string>) (a string)))
	b)
    => "123")

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define f
	  (case-lambda/tags
	   (((a <number>))
	    (a string))))
	(f 123))
    => "123")

  (check
      (let ()
	(define g
	  (case-lambda/tags
	   (#(args <list>)
	    (args length))))
	(g 1 2 3))
    => 3)

;;; --------------------------------------------------------------------
;;; lambda/tags

  (check
      (let ()
	(define f
	  (lambda/tags ((a <number>))
	    (a string)))
	(f 123))
    => "123")

  (check
      (let ()
	(define f
	  (lambda/tags ((a <number>) (b <number>))
	    (list (a string) (b string))))
	(f 1 2))
    => '("1" "2"))

  (check
      (let ()
	(define g
	  (lambda/tags #(args <list>)
	    (args length)))
	(g 1 2 3))
    => 3)

  (check	;untagged rest argument
      (let ()
	(define g
	  (lambda/tags ((a <number>) (b <number>) . args)
	    (list (a string)
		  (b string)
		  (length args))))
	(g 1 2 3 4 5))
    => '("1" "2" 3))

  (check	;rest argument
      (let ()
	(define g
	  (lambda/tags ((a <number>) (b <number>) . #(args <list>))
	    (list (a string)
		  (b string)
		  (args length))))
	(g 1 2 3 4 5))
    => '("1" "2" 3))

;;; --------------------------------------------------------------------
;;; define/tags

  (check
      (let ()
	(define/tags (f (a <number>))
	  (a string))
	(f 123))
    => "123")

  (check
      (let ()
	(define/tags (f (a <number>) (b <number>))
	  (list (a string) (b string)))
	(f 1 2))
    => '("1" "2"))

  (check
      (let ()
	(define/tags (g . #(args <list>))
	  (args length))
	(g 1 2 3))
    => 3)

  (check	;untagged rest argument
      (let ()
	(define/tags (g (a <number>) (b <number>) . args)
	  (list (a string)
		(b string)
		(length args)))
	(g 1 2 3 4 5))
    => '("1" "2" 3))

  (check	;rest argument
      (let ()
	(define/tags (g (a <number>) (b <number>) . #(args <list>))
	  (list (a string)
		(b string)
		(args length)))
	(g 1 2 3 4 5))
    => '("1" "2" 3))

  (check
      (let ()
	(define/tags #(a <number>) 123)
	(a string))
    => "123")

  (check
      (let ()
	(define/tags #(a <number>))
	(set! a 123)
	(a string))
    => "123")

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
