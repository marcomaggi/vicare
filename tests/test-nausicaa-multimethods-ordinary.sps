;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: usage tests for ordinary generic functions
;;;Date: Mon Jul  5, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing ordinary generic functions\n")


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
			(set!/tags			set!))
	       '(nausicaa language multimethods)
	       '(nausicaa language builtins)))

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


(parametrise ((check-test-name 'errors))

  (check
      (catch-assertion #f
	(%eval '(let ()
		  (define-generic a (b))
		  (define-method (a b) #f)
		  (define-method (a b c) #f)
		  #f)))
    => '(((nausicaa:builtin:<top>)
	  (nausicaa:builtin:<top>))))

  (check
      (catch-assertion #f
	(%eval '(let ()
		 (define-generic a (p))
		 (define-generic b (p q))
		 (define-generic c (p)
		   (merge-with-multimethods a b))
                 #f)))
    => '())

  #t)


(parametrise ((check-test-name 'simple-inheritance))

  (let ()
    (define-class <one>
      (nongenerative generics-test:simple-inheritance:<one>)
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (nongenerative generics-test:simple-inheritance:<two>)
      (parent <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))
    (define-class <three>
      (nongenerative generics-test:simple-inheritance:<three>)
      (parent <two>)
      (fields (mutable g)
	      (mutable h)
	      (mutable i)))

    (define-generic alpha (o))

    (define-method alpha ((o <one>))
      (<one>-a o))

    (define-method alpha ((o <two>))
      (o d))

    (define-method alpha ((o <three>))
      (<three>-g o))

    ;;Direct public constructor call.
    (let ((a (make-<one> 1 10 100))
    	  (b (make-<two> 0 0 0 2 20 200))
    	  (c (make-<three> 0 0 0 0 0 0 3 30 300)))
      (check (alpha a) => 1)
      (check (alpha b) => 2)
      (check (alpha c) => 3)
      #t)

    ;;Constructor call from tag identifier.
    (let ((a (<one> (1 10 100)))
    	  (b (<two> (0 0 0 2 20 200)))
    	  (c (<three> (0 0 0 0 0 0 3 30 300))))
      (check (alpha a) => 1)
      (check (alpha b) => 2)
      (check (alpha c) => 3)
      #t)
    #t)

;;; --------------------------------------------------------------------

  (let ()
    ;;This tests overwriting an existing method function.

    (define-class <one>
      (nongenerative generics-test:simple-inheritance:<one>1)
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (nongenerative generics-test:simple-inheritance:<two>1)
      (parent <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))

    (define-generic alpha (o))

    (define-method alpha ((o <one>))
      (<one>-a o))

    (define-method alpha ((o <one>))
      (<one>-b o))

    (let ((o (make-<two> 1 2 3 4 5 6)))
      (check (alpha o) => 2)
      (check (alpha o) => 2)) ;this exercises the cache
    #t)

;;; --------------------------------------------------------------------

  (let ()
    ;;Built in types.

    (define-generic alpha (o))

    (define-method alpha ((o <fixnum>))		'<fixnum>)
    (define-method alpha ((o <flonum>))		'<flonum>)
    (define-method alpha ((o <integer>))	'<integer>)
    (define-method alpha ((o <real>))		'<real>)
    (define-method alpha ((o <complex>))	'<complex>)
    (define-method alpha ((o <number>))		'<number>)

    (check (alpha 12)		=> '<fixnum>)
    (check (alpha (expt 12 65)) => '<integer>)
    (check (alpha 2/3)		=> '<real>)
    (check (alpha 1.2+3.4i)	=> '<complex>)

    #t)

  #t)


(parametrise ((check-test-name 'mapping))

  (let ()
    (define-class <one>
      (nongenerative generics-test:mapping:<one>)
      (fields (mutable a)))
    (define-class <two>
      (nongenerative generics-test:mapping:<two>)
      (fields (mutable b)))
    (define-class <three>
      (nongenerative generics-test:mapping:<three>)
      (fields (mutable c)))

    (define-generic alpha (o))

    (define-method alpha ((o <one>))
      (<one>-a o))

    (define-method alpha ((o <two>))
      (<two>-b o))

    (define-method alpha ((o <three>))
      (<three>-c o))

    (check
	(map alpha (list (<one> (1))
			 (<two> (2))
			 (<three> (3))))
      => '(1 2 3))

    #f)

  #t)


(parametrise ((check-test-name 'next-method))

  (define-class <one>
    (nongenerative generics-test:next-method:<one>)
    (fields (mutable a)
	    (mutable b)
	    (mutable c)))
  (define-class <two>
    (nongenerative generics-test:next-method:<two>)
    (parent <one>)
    (fields (mutable d)
	    (mutable e)
	    (mutable f)))
  (define-class <three>
    (nongenerative generics-test:next-method:<three>)
    (parent <two>)
    (fields (mutable g)
	    (mutable h)
	    (mutable i)))

  (define-generic alpha (o))

  (define-method (alpha (o <one>))
    (<one>-a o))

  (define-method alpha ((o <two>))
    (cons (<two>-d o)
	  (call-next-method)))

  (define-method alpha ((o <three>))
    (cons (<three>-g o)
	  (call-next-method)))

  (let ((a (make-<one> 1 2 3))
	(b (make-<two> 2.1 2.2 2.3 2.4 2.5 2.6))
	(c (make-<three> 3.1 3.2 3.3 3.4 3.5 3.6 3.7 3.8 3.9)))

    (check (alpha a) => 1)
    (check (alpha b) => '(2.4 . 2.1))
    (check (alpha c) => '(3.7 3.4 . 3.1))

    #t)

  #t)


(parametrise ((check-test-name 'specificity))

  (define-class <a>
    (nongenerative generics-test:specificity:<a>)
    (fields (mutable a)))
  (define-class <b>
    (nongenerative generics-test:specificity:<b>)
    (fields (mutable b)))
  (define-class <c>
    (nongenerative generics-test:specificity:<c>)
    (fields (mutable c)))
  (define-class <d>
    (nongenerative generics-test:specificity:<d>)
    (fields (mutable d)))

  (define-class <1>
    (nongenerative generics-test:specificity:<1>)
    (parent <a>))
  (define-class <2>
    (nongenerative generics-test:specificity:<2>)
    (parent <b>))
  (define-class <3>
    (nongenerative generics-test:specificity:<3>)
    (parent <c>))
  (define-class <4>
    (nongenerative generics-test:specificity:<4>)
    (parent <d>))

  (define a (make-<a> 1))
  (define b (make-<b> 2))
  (define c (make-<c> 3))
  (define d (make-<d> 4))

  (define n1 (make-<1> 1))
  (define n2 (make-<2> 2))
  (define n3 (make-<3> 3))
  (define n4 (make-<4> 4))

;;; --------------------------------------------------------------------
;;; Two levels specificity.
  (let ()
    (define-generic alpha (p q r))
    (define-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method (alpha (p <a>) (q <b>) (r <c>)) 2)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 2)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 2)
    (check (alpha  a  b  c) => 2)
    )

;;; --------------------------------------------------------------------
;;; Mixed levels specificity.
  (let ()
    (define-generic alpha (p q r))
    (define-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method (alpha (p <1>) (q <b>) (r <3>)) 2)
    (define-method (alpha (p <a>) (q <b>) (r <c>)) 3)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 3)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 3)
    (check (alpha  a  b  c) => 3)
    )
  (let ()
    (define-generic alpha (p q r))
    (define-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method (alpha (p <1>) (q <b>) (r <c>)) 2)
    (define-method (alpha (p <a>) (q <b>) (r <c>)) 3)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 3)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 2)
    (check (alpha  a  b  c) => 3)
    )

;;; --------------------------------------------------------------------
;;; Overwriting existing method.
  (let ()
    (define-generic alpha (p))
    (define-method (alpha (p <1>)) 123)
    (define-method (alpha (p <1>)) 456)
    (check (alpha n1) => 456))

  #t)


(parametrise ((check-test-name 'merge))

  (let ()	;merge without signature conflict
    (define-class <one>
      (nongenerative generics-test:merge:<one>)
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (nongenerative generics-test:merge:<two>)
      (parent <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))
    (define-class <three>
      (nongenerative generics-test:merge:<three>)
      (parent <two>)
      (fields (mutable g)
	      (mutable h)
	      (mutable i)))

    (define-generic alpha (o))
    (define-generic beta  (o))

    (define-method (alpha (o <one>))
      'alpha-one)

    (define-method (alpha (o <two>))
      'alpha-two)

    (define-method (beta (o <three>))
      'beta-three)

    (define-generic gamma (o)
      (merge-with-multimethods alpha beta))

    (define a (<one> (1 10 100)))
    (define b (<two> (0 0 0 2 20 200)))
    (define c (<three> (0 0 0 0 0 0 3 30 300)))

    (check (gamma a) => 'alpha-one)
    (check (gamma b) => 'alpha-two)
    (check (gamma c) => 'beta-three)

    #t)

;;; --------------------------------------------------------------------

  (let ()	;merge with signature conflict
    (define-class <one>
      (nongenerative generics-test:merge:<one>1)
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (nongenerative generics-test:merge:<two>1)
      (parent <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))
    (define-class <three>
      (nongenerative generics-test:merge:<three>2)
      (parent <two>)
      (fields (mutable g)
	      (mutable h)
	      (mutable i)))

    (define-generic alpha (o))
    (define-generic beta  (o))

    (define-method (alpha (o <one>))
      'alpha-one)

    (define-method (alpha (o <two>))
      'alpha-two)

    (define-method (beta (o <three>))
      'beta-three)

    (define-method (beta (o <two>))	;this is discarded when merging
      'beta-two)

    (define-generic gamma (o)
      (merge-with-multimethods alpha beta))

    (define a (make-<one> 1 10 100))
    (define b (make-<two> 0 0 0 2 20 200))
    (define c (make-<three> 0 0 0 0 0 0 3 30 300))

    (check (gamma a) => 'alpha-one)
    (check (gamma b) => 'alpha-two)
    (check (gamma c) => 'beta-three)

    #t)

  #t)


(parametrise ((check-test-name 'cache))

  (let ()	;merge without signature conflict
    (define-class <one>
      (nongenerative generics-test:cache:<one>)
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (nongenerative generics-test:cache:<two>)
      (parent <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))

    (define-generic alpha (o))
    (define-method (alpha (o <one>))
      1)
    (define-method (alpha (o <two>))
      2)

    (check	;fills the cache
	(alpha (<one> (1 2 3)))
      => 1)

    (check	;fills the cache
	(alpha (<two> (1 2 3 4 5 6)))
      => 2)

    (check	;uses the cache
	(alpha (<one> (1 2 3)))
      => 1)

    (check	;uses the cache
	(alpha (<two> (1 2 3 4 5 6)))
      => 2)

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-class <a> (nongenerative generics-test:cache:<a>))
    (define-class <b> (nongenerative generics-test:cache:<b>))
    (define-class <c> (nongenerative generics-test:cache:<c>))
    (define-class <d> (nongenerative generics-test:cache:<d>))
    (define-class <e> (nongenerative generics-test:cache:<e>))
    (define-class <f> (nongenerative generics-test:cache:<f>))
    (define-class <g> (nongenerative generics-test:cache:<g>))
    (define-class <h> (nongenerative generics-test:cache:<h>))
    (define-class <i> (nongenerative generics-test:cache:<i>))
    (define-class <l> (nongenerative generics-test:cache:<l>))

    (define-generic alpha (p q r))

    (define-method (alpha (p <a>) (q <b>) (r <c>)) 'abc)
    (define-method (alpha (p <d>) (q <e>) (r <f>)) 'def)
    (define-method (alpha (p <g>) (q <h>) (r <i>)) 'ghi)
    (define-method (alpha (p <l>) (q <a>) (r <b>)) 'lab)
    (define-method (alpha (p <c>) (q <d>) (r <e>)) 'cde)
    (define-method (alpha (p <f>) (q <g>) (r <h>)) 'fgh)
    (define-method (alpha (p <i>) (q <l>) (r <a>)) 'ila)
    (define-method (alpha (p <b>) (q <c>) (r <d>)) 'bcd)
    (define-method (alpha (p <e>) (q <f>) (r <g>)) 'efg)
    (define-method (alpha (p <h>) (q <i>) (r <l>)) 'hil)

    (define a (<a> ()))
    (define b (<b> ()))
    (define c (<c> ()))
    (define d (<d> ()))
    (define e (<e> ()))
    (define f (<f> ()))
    (define g (<g> ()))
    (define h (<h> ()))
    (define i (<i> ()))
    (define l (<l> ()))

    (do ((count 0 (+ 1 count)))
	((= count 10))
      (check (alpha a b c) => 'abc)
      (check (alpha d e f) => 'def)
      (check (alpha g h i) => 'ghi)
      (check (alpha l a b) => 'lab))

    (do ((count 0 (+ 1 count)))
	((= count 3))
      (check (alpha c d e) => 'cde)
      (check (alpha f g h) => 'fgh)
      (check (alpha i l a) => 'ila)
      (check (alpha b c d) => 'bcd)
      (check (alpha e f g) => 'efg)
      (check (alpha h i l) => 'hil))

    (check (alpha a b c) => 'abc)
    (check (alpha d e f) => 'def)
    (check (alpha g h i) => 'ghi)
    (check (alpha l a b) => 'lab)

    #f)

  #t)


(parametrise ((check-test-name 'predefined))

  (define-generic object->string (o))

  (define-method (object->string o)
    (call-with-string-output-port
	(lambda (port)
	  (display o port))))

  (define-class <alpha>
    (nongenerative generics-test:predefined:<alpha>)
    (fields (immutable the-string)))

  (define-class <beta>
    (nongenerative generics-test:predefined:<beta>)
    (fields (immutable the-string)))

  (check
      (object->string 123)
    => "123")

  (let ((a (make-<alpha> "alpha"))
	(b (make-<beta>  "beta")))

    (define-method (object->string (o <alpha>))
      (<alpha>-the-string o))

    (define-method object->string ((o <beta>))
      (<beta>-the-string o))

    (check
	(object->string a)
      => "alpha")

    (check
	(object->string b)
      => "beta")

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
