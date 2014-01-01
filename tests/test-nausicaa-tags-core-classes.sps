;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: basic tests for class definitions
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
  (vicare numerics constants)
  (only (vicare language-extensions)
	define-inline
	module)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing classes core features\n")


;;;; helpers

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


(define-class <alpha>
  (fields (mutable a)
	  (mutable b)
	  (immutable c))

  (getter (lambda (stx tag)
	    (syntax-case stx ()
	      ((?var (()))
	       #'(?var a)))))

  (setter (lambda (stx tag)
	    (syntax-case stx ()
	      ((?var (()) ?value)
	       #'(set! (?var a) ?value)))))

  (method (incr-a (O <alpha>))
    (+ 1 (O a)))

  (method (incr-b (O <alpha>))
    (+ 1 (O b))))


(parametrise ((check-test-name	'internal-definition-bindings))

  (check	;access to binding
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
        ((<alpha> #:predicate) O))
    => #t)

  (check	;access to binding
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
        ((<alpha> #:predicate) O))
    => #t)

  (check	;internal definition syntax
      (let ()
  	(<alpha> O (<> (1 2 3)))
        ((<alpha> #:predicate) O))
    => #t)

  (check	;mutation of binding
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
  	(set! O (<alpha> (4 5 6)))
	((<alpha> #:predicate) O))
    => #t)

  (check	;mutation of binding
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
  	(set! O (<alpha> (4 5 6)))
	((<alpha> #:predicate) O))
    => #t)

  (check	;access to fields
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
        (vector (O a) (O b) (O c)))
    => '#(1 2 3))

  (check	;field mutation
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
  	(set! (O a) 99)
  	(set! (O b) 88)
        (list (O a) (O b)))
    => '(99 88))

  (check	;method call
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
  	(O incr-b))
    => 3)

  (check	;method call
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
  	(O incr-a))
    => 2)

  (check	;getter
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
  	(O[]))
    => 1)

  (check	;setter, syntax 1
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
  	(set! (O[]) 99)
  	(O[]))
    => 99)

  (check	;setter, syntax 2
      (let ()
  	(<alpha> O (<alpha> (1 2 3)))
  	(set! O[] 99)
  	(O[]))
    => 99)

  #;(check	;invalid expression result
      (let ()
	(<alpha> O (+ 1 2 3))
	O)
    => #f)

  #t)


(parametrise ((check-test-name	'let-style-bindings))

  (check	;access to binding
      (let (((O <alpha>) (<alpha> (1 2 3))))
	((<alpha> #:predicate) O))
    => #t)

  (check	;mutation of binding
      (let (((O <alpha>) (<alpha> (1 2 3))))
	(set! O (<alpha> (4 5 6)))
	((<alpha> #:predicate) O))
    => #t)

  (check	;access to fields
      (let (((O <alpha>) (<alpha> (1 2 3))))
	(vector (O a) (O b) (O c)))
    => '#(1 2 3))

  (check	;field mutation
      (let (((O <alpha>) (<alpha> (1 2 3))))
	(set! (O a) 99)
	(set! (O b) 88)
	(list (O a) (O b)))
    => '(99 88))

  (check	;method call
      (let (((O <alpha>) (<alpha> (1 2 3))))
	(O incr-b))
    => 3)

  (check	;method call
      (let (((O <alpha>) (<alpha> (1 2 3))))
	(O incr-a))
    => 2)

  (check	;getter
      (let (((O <alpha>) (<alpha> (1 2 3))))
	(O[]))
    => 1)

  (check	;setter, syntax 1
      (let (((O <alpha>) (<alpha> (1 2 3))))
	(set! (O[]) 99)
	(O[]))
    => 99)

  (check	;setter, syntax 2
      (let (((O <alpha>) (<alpha> (1 2 3))))
	(set! O[] 99)
	(O[]))
    => 99)

  #t)


(parametrise ((check-test-name	'inheritance))

  (let ()	;inherit from <top>, no protocol clauses

    (define-class <alpha>
      (fields a b))

    (check
	(let (((o <alpha>) (<alpha> (1 2))))
	  ((<alpha> #:predicate) o))
      => #t)

    #f)

  (let ()	;inherit from <top>, protocol clauses

    (define-class <alpha>
      (protocol (lambda (make-top)
		  (lambda (a b)
		    ((make-top) a b))))
      (fields a b))

    (check
	(let (((o <alpha>) (<alpha> (1 2))))
	  ((<alpha> #:predicate) o))
      => #t)

    #f)

  (let ()	;no protocol clauses

    (define-class <alpha>
      (fields a b))

    (define-class <beta>
      (parent <alpha>)
      (fields c d))

    (check
	(let (((o <beta>) (<beta> (1 2 3 4))))
	  (<beta> #:is-a? o))
      => #t)

    #f)

  (let ()	;with protocol clauses

    (define-class <alpha>
      (protocol (lambda (make-top)
		  (lambda (a b)
		    ((make-top) a b))))
      (fields a b))

    (define-class <beta>
      (parent <alpha>)
      (protocol (lambda (make-alpha)
		  (lambda (a b c d)
		    ((make-alpha a b) c d))))
      (fields c d))

    (check
	(let (((o <beta>) (<beta> (1 2 3 4))))
	  (<beta> #:is-a? o))
      => #t)

    #f)

;;; --------------------------------------------------------------------
;;; examples for the documentation

  (let ()	;no protocol clauses

    (define-class <alpha>
      (fields a b))

    (define-class <beta>
      (parent <alpha>)
      (fields c d))

    #;(check-pretty-print (<alpha> (1 2)))

    (check
	(let ()
	  (<alpha> A (<> (1 2)))
	  (list (A a) (A b)))
      => '(1 2))

    (check
	(let ()
	  (<beta> B (<> (1 2 3 4)))
	  (list (B a) (B b) (B c) (B d)))
      => '(1 2 3 4))

    #f)

  (let ()	;common protocols equivalent to the default ones

    (define-class <alpha>
      (fields a b)
      (protocol (lambda (make-top)
		  (lambda (a b)
		    ((make-top) a b)))))

    (define-class <beta>
      (parent <alpha>)
      (fields c d)
      (protocol (lambda (make-alpha)
		  (lambda (a b c d)
		    ((make-alpha a b) c d)))))

    (check
	(let ()
	  (<alpha> A (<> (1 2)))
	  (list (A a) (A b)))
      => '(1 2))

    (check
	(let ()
	  (<beta> B (<> (1 2 3 4)))
	  (list (B a) (B b) (B c) (B d)))
      => '(1 2 3 4))

    #f)

  (let ()	;from fields protocols

    (define-class <alpha>
      (fields a b))

    (define-class <beta>
      (parent <alpha>)
      (fields c d))

    (check
	(let ()
	  (<alpha> A (make-from-fields <alpha> 1 2))
	  (list (A a) (A b)))
      => '(1 2))

    (check
	(let ()
	  (<beta> B (make-from-fields <beta> 1 2 3 4))
	  (list (B a) (B b) (B c) (B d)))
      => '(1 2 3 4))

    #f)

  (let ()	;common protocol clauses

    (define-class <alpha>
      (fields a b)
      (protocol (lambda (make-top)
		  (lambda (a)
		    ((make-top) a 2)))))

    (define-class <beta>
      (parent <alpha>)
      (fields c d)
      (protocol (lambda (make-alpha)
		  (lambda (a c)
		    ((make-alpha a) c 4)))))

    (check
	(let ()
	  (<alpha> A (<> (1)))
	  (list (A a) (A b)))
      => '(1 2))

    (check
	(let ()
	  (<beta> B (<> (1 3)))
	  (list (B a) (B b) (B c) (B d)))
      => '(1 2 3 4))

    #f)

  (let ()	;super and public protocol clauses

    (define-class <alpha>
      (fields a b)
      (public-protocol (lambda (make-top)
			 (lambda ()
			   ((make-top) 1 2))))
      (super-protocol  (lambda (make-top)
			 (lambda (a b)
			   ((make-top) (* 10 a) (* 10 b))))))

    (define-class <beta>
      (parent <alpha>)
      (fields c d)
      (public-protocol (lambda (make-alpha)
			 (lambda (a b)
			   ((make-alpha a b) 3 4)))))

    (check
	(let ()
	  (<alpha> A (<> ()))
	  (list (A a) (A b)))
      => '(1 2))

    (check
	(let ()
	  (<beta> B (<> (1 2)))
	  (list (B a) (B b) (B c) (B d)))
      => '(10 20 3 4))

    #f)

  #t)


(parametrise ((check-test-name	'mixins))

  (let ()	;single class mixin

    (define-mixin <ab-stuff>
      (fields (mutable a)
	      (mutable b)))

    (define-class <abc>
      (fields (immutable c))
      (mixins <ab-stuff>))

    (check
	(let (((o <abc>) (<abc> (1 #;c 2 #;a 3 #;b))))
	  (vector (o a) (o b) (o c)))
      => '#(2 3 1))

    #f)

  (let ()	;multiple class mixins

    (define-mixin <a-stuff>
      (fields (mutable a)))

    (define-mixin <b-stuff>
      (fields (mutable b)))

    (define-class <abc>
      (fields (immutable c))
      (mixins <a-stuff> <b-stuff>))

    (check
	(let (((o <abc>) (<abc> (1 #;c 2 #;a 3 #;b))))
	  (vector (o a) (o b) (o c)))
      => '#(2 3 1))

    #f)

  (let ()	;single class mixin, single mixin mixin

    (define-mixin <a-stuff>
      (fields (mutable a)))

    (define-mixin <ab-stuff>
      (fields (mutable b))
      (mixins <a-stuff>))

    (define-class <abc>
      (fields (immutable c))
      (mixins <ab-stuff>))

    (check
	(let (((o <abc>) (<abc> (1 #;c 2 #;b 3 #;a))))
	  (vector (o a) (o b) (o c)))
      => '#(3 2 1))

    #f)

  (let ()	;single class mixin, multiple mixin mixin

    (define-mixin <a-stuff>
      (fields (mutable a)))

    (define-mixin <b-stuff>
      (fields (mutable b)))

    (define-mixin <ab-stuff>
      (mixins <a-stuff>)
      (mixins <b-stuff>))

    (define-class <abc>
      (fields (immutable c))
      (mixins <ab-stuff>))

    (check
	(let (((o <abc>) (<abc> (1 #;c 2 #;a 3 #;b))))
	  (vector (o a) (o b) (o c)))
      => '#(2 3 1))

    #f)

  #t)


(parametrise ((check-test-name	'nested-accessor))

  (let ()

    (define-class <alpha>
      (virtual-fields a)
      (getter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)))
		   #'123))))
      (method (doit O n)
	(cons 456 n)))

    (define-class <beta>
      (virtual-fields (b <alpha>)))

    (define-class <gamma>
      (virtual-fields (c <beta>)))

    (define (<alpha>-a o) 'a)
    (define (<beta>-b  o) 'b)
    (define (<gamma>-c o) 'c)

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (list (O c b a) (O c b) (O c)))
      => '(a b c))

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (O c b[99]))
      => 123)

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (O c b doit 99))
      => '(456 . 99))

    #f)

  (let ()	;redirect to parent

    (define-class <base>
      (virtual-fields a)
      (getter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)))
		   #'123))))
      (method (doit O n)
	(cons 456 n)))

    (define-class <alpha>
      (parent <base>))

    (define-class <beta>
      (virtual-fields (b <alpha>)))

    (define-class <gamma>
      (virtual-fields (c <beta>)))

    (define (<base>-a  o) 'a)
    (define (<beta>-b  o) 'b)
    (define (<gamma>-c o) 'c)

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (list (O c b a) (O c b) (O c)))
      => '(a b c))

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (O c b[99]))
      => 123)

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (O c b doit 99))
      => '(456 . 99))

    #f)

  #t)


(parametrise ((check-test-name	'nested-mutator))

  (let ()

    (define-class <alpha>
      (virtual-fields (mutable a))
      (setter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)) ?val)
		   #'(list 123 ?key ?val))))))

    (define-class <beta>
      (virtual-fields (mutable (b <alpha>))))

    (define-class <gamma>
      (virtual-fields (mutable (c <beta>))))

    (define (<alpha>-a o) 'a)
    (define (<beta>-b  o) 'b)
    (define (<gamma>-c o) 'c)
    (define (<alpha>-a-set! o v) 'A)
    (define (<beta>-b-set!  o v) 'B)
    (define (<gamma>-c-set! o v) 'C)

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (list (set! (O c b a) 1)
		(set! (O c b)   (<alpha> ()))
		(set! (O c)     (<beta>  ()))))
      => '(A B C))

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (set! (O c b[777]) 999))
      => '(123 777 999))

    #f)

  (let ()	;redirect to parent

    (define-class <base>
      (virtual-fields (mutable a))
      (setter (lambda (stx tag)
		(syntax-case stx ()
		  ((?var ((?key)) ?val)
		   #'(list 123 ?key ?val))))))

    (define-class <alpha>
      (parent <base>))

    (define-class <beta>
      (virtual-fields (mutable (b <alpha>))))

    (define-class <gamma>
      (virtual-fields (mutable (c <beta>))))

    (define (<base>-a o) 'a)
    (define (<beta>-b  o) 'b)
    (define (<gamma>-c o) 'c)
    (define (<base>-a-set!  o v) 'A)
    (define (<beta>-b-set!  o v) 'B)
    (define (<gamma>-c-set! o v) 'C)

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (list (set! (O c b a) 1)
		(set! (O c b)   (<alpha> ()))
		(set! (O c)     (<beta>  ()))))
      => '(A B C))

    (check
	(let (((O <gamma>) (<gamma> ())))
	  (set! (O c b[777]) 999))
      => '(123 777 999))

    #f)

  #t)


(parametrise ((check-test-name	'maker))

  (let ()

    (define-class <alpha>
      (maker (lambda (stx)
	       (syntax-case stx ()
		 ((_ (?a ?b))
		  #'(make-<alpha> ?a ?b)))))
      (fields a b))

    (check
	(let (((o <alpha>) (<alpha> (1 2))))
	  (o a))
      => 1)

    (check
	(let (((o <alpha>) (<alpha> (1 2))))
	  (o b))
      => 2)

    (check	;nested
	((<alpha> (1 2)) b)
      => 2)

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-class <rect-coords>
      (fields x y)
      (maker (lambda (stx)
	       (syntax-case stx (rec pol)
		 ((_ (pol ?rho ?theta))
		  #'(let ((rho   ?rho)
			  (theta ?theta))
		      (make-<rect-coords> (* rho (cos theta))
					  (* rho (sin theta)))))
		 ((_ (rec ?x ?y))
		  #'(make-<rect-coords> ?x ?y))
		 ))))

    (check
	(let ()
	  (<rect-coords> R (<> (rec 1.0 2.0)))
	  (list (R x) (R y)))
      => '(1.0 2.0))

    (check
	(let ()
	  (<rect-coords> R (<> (pol 1.0 2.0)))
	  #;(check-pretty-print (list (R x) (R y)))
	  (list (R x) (R y)))
      => (list (* 1.0 (cos 2.0))
	       (* 1.0 (sin 2.0))))

    #f)

  #t)


(parametrise ((check-test-name	'fields))

;;;examples of fields and virtual-fields for documentation

  (let ()	;with inline accessor and mutator expressions

    (define-class <angle>
      (fields (mutable (radians <real>)))
      (virtual-fields (mutable (degrees <real>)
			       (lambda ((A <angle>))
				 (* (/ 180.0 greek-pi) (A radians)))
			       (lambda ((A <angle>) (deg <real>))
				 (set! (A radians) (* (/ greek-pi 180.0) deg))))))

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (A radians))
      => greek-pi)

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (A degrees))
      => 180.0)

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (set! (A degrees) 90.0)
	  (A radians))
      => greek-pi/2)

    #f)

  (let ()	;with accessor and mutator functions

    (define-class <angle>
      (fields (mutable (radians <real>)))
      (virtual-fields (mutable (degrees <real>))))

    (define (<angle>-degrees (A <angle>))
      (* (/ 180.0 greek-pi) (A radians)))

    (define (<angle>-degrees-set! (A <angle>) (deg <real>))
      (set! (A radians) (* (/ greek-pi 180.0) deg)))

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (A radians))
      => greek-pi)

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (A degrees))
      => 180.0)

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (set! (A degrees) 90.0)
	  (A radians))
      => greek-pi/2)

    #f)

  (let ()	;with accessor and mutator syntaxes

    (define-class <angle>
      (fields (mutable (radians <real>)))
      (virtual-fields (mutable (degrees <real>))))

    (define-inline (<angle>-degrees A)
      (let (((A <angle>) A))
	(* (/ 180.0 greek-pi) (A radians))))

    (define-inline (<angle>-degrees-set! A deg)
      (let (((A <angle>)  A)
	    ((deg <real>) deg))
	(set! (A radians) (* (/ greek-pi 180.0) deg))))

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (A radians))
      => greek-pi)

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (A degrees))
      => 180.0)

    (check
	(let ()
	  (<angle> A (<> (greek-pi)))
	  (set! (A degrees) 90.0)
	  (A radians))
      => greek-pi/2)

    #f)

  #t)


(parametrise ((check-test-name	'methods))

  (let ()	;embedded method definition, form 1

    (define-class <stuff>
      (fields a b)
      (method (sum (S <stuff>))
	(+ (S a) (S b))))

    (check
	(let ()
	  (<stuff> S (<> (1 2)))
	  (S sum))
      => 3)

    #f)

  (let ()	;embedded method definition, form 2

    (define-class <stuff>
      (fields a b)
      (method sum
	(lambda ((S <stuff>))
	  (+ (S a) (S b)))))

    (check
	(let ()
	  (<stuff> S (<> (1 2)))
	  (S sum))
      => 3)

    #f)

  (let ()	;embedded method syntax

    (define-class <stuff>
      (fields a b)
      (method-syntax sum
	(lambda (stx)
	  (syntax-case stx ()
	    ((_ ?instance)
	     #'(let (((S <stuff>) ?instance))
		 (+ (S a) (S b))))))))

    (check
	(let ()
	  (<stuff> S (<> (1 2)))
	  (S sum))
      => 3)

    #f)

  (let ()	;outside method definition, method function

    (define-class <stuff>
      (fields a b)
      (methods (sum <stuff>-sum)))

    (define (<stuff>-sum (S <stuff>))
      (+ (S a) (S b)))

    (check
	(let ()
	  (<stuff> S (<> (1 2)))
	  (S sum))
      => 3)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;METHODS form 1

    (define-class <stuff>
      (fields a b)
      (methods sum))

    (define/tags (<stuff>-sum (S <stuff>))
      (+ (S a) (S b)))

    (check
	(let ()
	  (<stuff> S (<> (1 2)))
	  (S sum))
      => 3)

    #f)

  (let ()	;METHODS form 3, function

    (define-class <stuff>
      (fields a b)
      (methods (sum <stuff>-sum)))

    (define/tags (<stuff>-sum (S <stuff>))
      (+ (S a) (S b)))

    (check
	(let ()
	  (<stuff> S (<> (1 2)))
	  (S sum))
      => 3)

    #f)

  (let ()	;METHODS form 3, syntax

    (define-class <stuff>
      (fields a b)
      (methods (sum <stuff>-sum)))

    (define-syntax <stuff>-sum
      (lambda (stx)
	(syntax-case stx ()
	  ((_ ?instance)
	   #'(let (((S <stuff>) ?instance))
	       (+ (S a) (S b)))))))

    (check
	(let ()
	  (<stuff> S (<> (1 2)))
	  (S sum))
      => 3)

    #f)

  #t)


(parametrise ((check-test-name	'satisfactions))

  (define-syntax general-class-constraint
    (lambda (stx)
      (syntax-case stx (parent fields virtual-fields mutable immutable
			       methods getter setter nongenerative
			       sealed opaque abstract)
	((_ (?class-id ?constructor ?predicate ?record-type)
	    (parent ?parent)
	    (fields (mutable   (?cm-field ?cm-tag) ?cm-accessor ?cm-mutator) ...)
	    (fields (immutable (?ci-field ?ci-tag) ?ci-accessor #f)          ...)
	    (virtual-fields (mutable   (?vm-field ?vm-tag) ?vm-accessor ?vm-mutator) ...)
	    (virtual-fields (immutable (?vi-field ?vi-tag) ?vi-accessor #f)          ...)
	    (methods (?method-name . ?method-callable) ...)
	    (getter ?getter)
	    (setter ?setter)
	    (nongenerative ?uid)
	    (sealed ?sealed)
	    (opaque ?opaque)
	    (abstract ?abstract))
	 #f))))

;;; --------------------------------------------------------------------

  (module ()

    (define-syntax constraint
      (syntax-rules (parent fields virtual-fields mutable immutable methods
			    getter setter nongenerative shadows)
	((_ (?name ?constructor ?predicate ?record-type)
	    (parent ?parent)
	    (fields)
	    (fields (immutable (?c-field ?c-tag) ?c-accessor #f) ...)
	    (virtual-fields)
	    (virtual-fields (?v-mutability (?v-field ?v-tag) ?v-accessor #f) ...)
	    (methods)
	    (getter #f)
	    (setter #f)
	    (nongenerative ?uid)
	    (sealed #f)
	    (opaque #f)
	    (abstract #f))
	 #f)))

    (define-class <alpha>
      (fields a b c)
      (virtual-fields d e f)
      (satisfies constraint general-class-constraint))

    (define (<alpha>-d o) #f)
    (define (<alpha>-e o) #f)
    (define (<alpha>-f o) #f)

    #f)

;;; --------------------------------------------------------------------

  (define-class <class-with-everything>
    (satisfies general-class-constraint)
    (parent <top>)
    (abstract)
    (opaque #t)
    (sealed #t)
    (nongenerative the-uid-of-<class-with-everything>)
    (fields (mutable (a <a-tag>) a-accessor a-mutator)
	    (immutable (b <b-tag>) b-accessor))
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
;; coding: utf-8-unix
;; End:
