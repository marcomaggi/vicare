;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for binding constructs with tagging
;;;Date: Fri Jul 22, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks)
  (prefix (libtest classes-lib) test.))

(check-set-mode! 'report-failed)
(check-display "*** testing classes, tagged binding constructs\n")


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


(parametrise ((check-test-name	'let))

  (define-label <n>
    (virtual-fields (immutable neg)))

  (define (<n>-neg n)
    (- n))

  (define-label <t>
    (virtual-fields (immutable ten)
		    (immutable neg)))

  (define (<t>-ten n)
    (+ 10 n))

  (define (<t>-neg n)
    (- (+ 10 n)))

;;; --------------------------------------------------------------------

  (check	;single binding, no tags
      (let ((a 1))
	a)
    => '1)

  (check	;multiple bindings, no tags
      (let ((a 1) (b 2) (c 3))
	(list a b c))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;single binding, with single tag
      (let (((a <n>) 1))
	(list a (a neg)))
    => '(1 -1))

  (check	;multiple bindings, with single tag
      (let (((a <n>) 1) ((b <n>) 2) ((c <n>) 3))
	(list a (a neg) b (b neg) c (c neg)))
    => '(1 -1 2 -2 3 -3))

;;; --------------------------------------------------------------------
;;; errors

  (check	;duplicate identifier in untagged bindings
      (catch-syntax-violation #f
	(%eval '(let (((a <n>) 1)
		      (a 2))
		  a)))
    => 'a)

  (check	;duplicate identifier in tagged bindings
      (catch-syntax-violation #f
	(%eval '(let (((a <n>) 1)
		      ((a <t>) 2))
		  a)))
    => 'a)

;;; --------------------------------------------------------------------
;;; show off examples

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let (((a <fraction>) (make-<fraction> 2/3))
	      ((b <fraction>) (make-<fraction> 4/5)))
	  (list (a numerator) (b denominator)))
      => '(2 5))

    #f)

  (check	;use the records from (libtest classes-lib)
      (let (((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(list (r a) (r b) (r c)
	      (r d) (r e) (r f)
	      (r g) (r h) (r i)))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (libtest classes-lib)
      (let (((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9)))
	    ((s test.<beta>)  (test.<beta>  (10 20 30 40 50 60))))
	(list (r a) (r g) (s a) (s d)))
    => '(1 7 10 40))

  #t)


(parametrise ((check-test-name	'named-let))

  (check
      (let loop ((a 0) (b 3))
  	(if (zero? b)
  	    a
  	  (loop (+ 1 a) (- b 1))))
    => 3)

  (check
      (let loop ((a 0)
  		 ((b <integer>) 3))
  	(if (b zero?)
  	    a
  	  (loop (+ 1 a) (- b 1))))
    => 3)

  (check
      (let loop (((A <integer>) 0)
		 ((b <integer>) 3))
	(if (b zero?)
	    (A odd?)
	  (loop (+ 1 A) (- b 1))))
    => #t)

;;; --------------------------------------------------------------------
;;; errors

  (check	;duplicate identifier in untagged bindings
      (catch-syntax-violation #f
	(%eval '(let loop (((a <n>) 1)
			   (a 2))
		  a)))
    => 'a)

  (check	;duplicate identifier in tagged bindings
      (catch-syntax-violation #f
	(%eval '(let loop (((a <n>) 1)
			   ((a <t>) 2))
		  a)))
    => 'a)

;;; --------------------------------------------------------------------
;;; let, type violation

    (let ()

      (define-class <alpha>
        (fields a))

      (define-class <beta>
        (parent <alpha>)
        (fields b))

      (check
	  (catch-assertion #f
	    (let (((o <alpha>) 1))
	      (o a)))
        => '((expression: 1)
	     (result: 1)))

      (check
	  (catch-assertion #f
	    (let (((o <alpha>) 1))
	      (o a)))
        => '((expression: 1)
	     (result: 1)))

      (check
	  (catch-assertion #f
	    (let (((o <alpha>) (<alpha> (1))))
	      (set! o 123)
	      (o a)))
        => '((expression: 123)
	     (result: 123)))

      #f)

  #t)


(parametrise ((check-test-name	'let-star))

  (define-label <n>
    (virtual-fields (immutable neg)))

  (define (<n>-neg n)
    (- n))

  (define-label <t>
    (virtual-fields (immutable ten)
		    (immutable neg)))

  (define (<t>-ten n)
    (+ 10 n))

  (define (<t>-neg n)
    (- (+ 10 n)))

;;; --------------------------------------------------------------------

  (check	;single binding, no tags
      (let* ((a 1))
	a)
    => '1)

  (check	;multiple bindings, no tags
      (let* ((a 1) (b 2) (c 3))
	(list a b c))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;single binding, with single tag
      (let* (((a <n>) 1))
	(list a (a neg)))
    => '(1 -1))

  (check	;multiple bindings, with single tag
      (let* (((a <n>) 1)
	     ((b <n>) 2)
	     ((c <n>) 3))
	(list a (a neg) b (b neg) c (c neg)))
    => '(1 -1 2 -2 3 -3))

;;; --------------------------------------------------------------------
;;; scope

  (check
      (let* ((a 1) (b a))
	(list a b))
    => '(1 1))

;;; --------------------------------------------------------------------
;;; examples to show off

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let* (((a <fraction>) (make-<fraction> 2/3))
	       ((b <fraction>) (make-<fraction> 4/5)))
	  (list (a numerator) (b denominator)))
      => '(2 5))

    (check
	(let* (((a <fraction>) (make-<fraction> 2/3))
	       ((b <fraction>) (make-<fraction> (/ (a numerator) 5))))
	  (b number))
      => 2/5)

    #f)

  (check	;use the records from (libtest classes-lib)
      (let* (((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(list (r a) (r b) (r c)
	      (r d) (r e) (r f)
	      (r g) (r h) (r i)))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (libtest classes-lib)
      (let* (((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9)))
	     ((s test.<beta>)  (test.<beta>  (10 20 30 40 50 60))))
	(list (r a) (r g) (s a) (s d)))
    => '(1 7 10 40))

  (check
      (let* ((a 1) (b 2))
	(list a b))
    => '(1 2))

  (check
      (let* ((a 1)
	     ((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(list a (r b)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; let*, type violation

  (let ()

    (define-class <alpha>
      (fields a))

    (define-class <beta>
      (parent <alpha>)
      (fields b))

    (check
	(catch-assertion #f
  	  (let* (((o <alpha>) 1))
  	    (o a)))
      => '((expression: 1)
	   (result: 1)))

    (check
	(catch-assertion #f
  	  (let* (((o <alpha>) (<alpha> (1))))
  	    (set! o (+ 1 2 3))
  	    (o a)))
      => '((expression: (+ 1 2 3))
	   (result: 6)))

    #f)

  #t)


(parametrise ((check-test-name	'letrec))

  (define-label <n>
    (virtual-fields (immutable neg)))

  (define (<n>-neg n)
    (- n))

  (define-label <t>
    (virtual-fields (immutable ten)
		    (immutable neg)))

  (define (<t>-ten n)
    (+ 10 n))

  (define (<t>-neg n)
    (- (+ 10 n)))

;;; --------------------------------------------------------------------

  (check	;single binding, no tags
      (letrec ((a 1))
	a)
    => '1)

  (check	;multiple bindings, no tags
      (letrec ((a 1) (b 2) (c 3))
	(list a b c))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;single binding, with single tag
      (letrec (((a <n>) 1))
	(list a (a neg)))
    => '(1 -1))

  (check	;multiple bindings, with single tag
      (letrec (((a <n>) 1)
	       ((b <n>) 2)
	       ((c <n>) 3))
	(list a (a neg) b (b neg) c (c neg)))
    => '(1 -1 2 -2 3 -3))

;;; --------------------------------------------------------------------
;;; scope

  (check
      (letrec ((a 1)
	       (b (lambda () a)))
	(list a (b)))
    => '(1 1))

;;; --------------------------------------------------------------------
;;; examples to show off

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(letrec (((a <fraction>) (make-<fraction> 2/3))
		 ((b <fraction>) (make-<fraction> 4/5)))
	  (list (a numerator) (b denominator)))
      => '(2 5))

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)))

    (define-class <beta>
      (fields (immutable proc)))

    (check
	(letrec (((a <alpha>) (make-<alpha> 123))
		 ((b <beta>)  (make-<beta> (lambda () (a value)))))
	  ((b proc)))
      => 123)

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)
	      (immutable proc)))

    (define-class <beta>
      (fields (immutable value)
	      (immutable proc)))

    (check
	(letrec (((a <alpha>) (make-<alpha>
			       1 (lambda ()
				   (cons (a value) (b value)))))
		 ((b <beta>)  (make-<beta>
			       2 (lambda ()
				   (cons (a value) (b value))))))
	  (list ((a proc)) ((b proc))))
      => '((1 . 2) (1 . 2)))

    #f)

  (check	;use the records from (libtest classes-lib)
      (letrec (((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(list (r a) (r b) (r c)
	      (r d) (r e) (r f)
	      (r g) (r h) (r i)))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (libtest classes-lib)
      (letrec (((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9)))
	       ((s test.<beta>)  (test.<beta>  (10 20 30 40 50 60))))
	(list (r a) (r g) (s a) (s d)))
    => '(1 7 10 40))

  (check
      (letrec ((a 1) (b 2))
	(list a b))
    => '(1 2))

  (check
      (letrec ((a 1)
	       ((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(list a (r b)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; letrec, type violation

  (let ()

    (define-class <alpha>
      (fields a))

    (define-class <beta>
      (parent <alpha>)
      (fields b))

    (check
	(catch-assertion #f
  	  (letrec (((o <alpha>) 1))
  	    (o a)))
      => '((expression: 1)
	   (result: 1)))

    (check
	(catch-assertion #f
  	  (letrec (((o <alpha>) (<alpha> (1))))
  	    (set! o (+ 1 2 3))
  	    (o a)))
      => '((expression: (+ 1 2 3))
	   (result: 6)))

    #f)

  #t)


(parametrise ((check-test-name	'letrec-star))

  (define-label <n>
    (virtual-fields (immutable neg)))

  (define (<n>-neg n)
    (- n))

  (define-label <t>
    (virtual-fields (immutable ten)
		    (immutable neg)))

  (define (<t>-ten n)
    (+ 10 n))

  (define (<t>-neg n)
    (- (+ 10 n)))

;;; --------------------------------------------------------------------

  (check	;single binding, no tags
      (letrec* ((a 1))
	a)
    => '1)

  (check	;multiple bindings, no tags
      (letrec* ((a 1) (b 2) (c 3))
	(list a b c))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;single binding, with single tag
      (letrec* (((a <n>) 1))
	(list a (a neg)))
    => '(1 -1))

  (check	;multiple bindings, with single tag
      (letrec* (((a <n>) 1) ((b <n>) 2) ((c <n>) 3))
	(list a (a neg) b (b neg) c (c neg)))
    => '(1 -1 2 -2 3 -3))

;;; --------------------------------------------------------------------
;;; scope

  (check
      (letrec* ((a 1)
		(b (lambda () a)))
	(list a (b)))
    => '(1 1))

;;; --------------------------------------------------------------------
;;; examples to show off

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(letrec* (((a <fraction>) (make-<fraction> 2/3))
		  ((b <fraction>) (make-<fraction> 4/5)))
	  (list (a numerator) (b denominator)))
      => '(2 5))

    (check
	(letrec* (((a <fraction>) (make-<fraction> 2/3))
		  ((b <fraction>) (make-<fraction> (/ (a numerator) 5))))
	  (b number))
      => 2/5)

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)
	      (immutable proc)))

    (define-class <beta>
      (fields (immutable value)
	      (immutable proc)))

    (check
	(letrec* (((a <alpha>) (make-<alpha>
				1 (lambda () (b value))))
		  ((b <beta>)  (make-<beta>
				2 (lambda () (a value)))))
	  (list ((a proc)) ((b proc))))
      => '(2 1))

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)
	      (immutable proc)))

    (define-class <beta>
      (fields (immutable value)
	      (immutable proc)))

    (check
	(letrec* (((a <alpha>) (make-<alpha>
				1 (lambda ()
				    (cons (a value) (b value)))))
		  ((b <beta>)  (make-<beta>
				2 (lambda ()
				    (cons (a value) (b value))))))
	  (list ((a proc)) ((b proc))))
      => '((1 . 2) (1 . 2)))

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)
	      (immutable proc)))

    (define-class <beta>
      (fields (immutable value)
	      (immutable proc)))

    (check
	(letrec* (((a <alpha>) (make-<alpha>
				1 (lambda ()
				    (cons (a value) (b value)))))
		  ((b <beta>)  (make-<beta>
				2 (lambda ()
				    (cons (a value) (b value)))))
		  ((c <top>)   (list ((a proc)) ((b proc)))))
	  c)
      => '((1 . 2) (1 . 2)))

    #f)

  (check	;use the records from (libtest classes-lib)
      (letrec* (((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(list (r a) (r b) (r c)
	      (r d) (r e) (r f)
	      (r g) (r h) (r i)))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (libtest classes-lib)
      (letrec* (((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9)))
		((s test.<beta>)  (test.<beta>  (10 20 30 40 50 60))))
	(list (r a) (r g) (s a) (s d)))
    => '(1 7 10 40))

  (check
      (letrec* ((a 1) (b 2))
	(list a b))
    => '(1 2))

  (check
      (letrec* ((a 1)
		((r test.<gamma>) (test.<gamma> (1 2 3 4 5 6 7 8 9))))
	(list a (r b)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; letrec*, type violation

  (let ()

    (define-class <alpha>
      (fields a))

    (define-class <beta>
      (parent <alpha>)
      (fields b))

    (check
	(catch-assertion #f
  	  (letrec* (((o <alpha>) 1))
  	    (o a)))
      => '((expression: 1)
	   (result: 1)))

    (check
	(catch-assertion #f
  	  (letrec* (((o <alpha>) (<alpha> (1))))
  	    (set! o (+ 1 2 3))
  	    (o a)))
      => '((expression: (+ 1 2 3))
	   (result: 6)))

    #f)

  #t)


(parametrise ((check-test-name	'let-values))

  (define-label <n>
    (virtual-fields (immutable neg)))

  (define (<n>-neg n)
    (- n))

  (define-label <t>
    (virtual-fields (immutable ten)
		    (immutable neg)))

  (define (<t>-ten n)
    (+ 10 n))

  (define (<t>-neg n)
    (- (+ 10 n)))

;;; --------------------------------------------------------------------

  (check	;no bindings
      (let-values ()
	1)
    => 1)

  (check	;special case, var specification is a symbol
      (let-values ((a (values 1 2 3)))
	a)
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;single binding, single id, no tags
      (let-values (((a) 1))
	a)
    => 1)

  (check	;single binding, multiple ids, no tags
      (let-values (((a b c) (values 1 2 3)))
	(list a b c))
    => '(1 2 3))

  (check	;multiple bindings, single id, no tags
      (let-values (((a) 1)
		   ((b) 2)
		   ((c) 3))
	(list a b c))
    => '(1 2 3))

  (check	;multiple bindings, multiple ids, no tags
      (let-values (((a b c) (values 1 2 3))
		   ((d e f) (values 4 5 6))
		   ((g h i) (values 7 8 9)))
	(list a b c d e f g h i))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;mixed bindings, no tags
      (let-values (((a)	1)
		   ((d)	4)
		   ((g h i)	(values 7 8 9)))
	(list a d g h i))
    => '(1 4 7 8 9))

  (check	;mixed bindings, no tags
      (let-values (((a b c)	(values 1 2 3))
		   ((d)	4)
		   ((g h i)	(values 7 8 9)))
	(list a b c  d  g h i))
    => '(1 2 3 4 7 8 9))

  (check	;mixed bindings, no tags
      (let-values (((a b c)	(values 1 2 3))
		   ((d)	4)
		   ((g)	7))
	(list a b c d g))
    => '(1 2 3 4 7))

;;; --------------------------------------------------------------------

  (check	;single binding, single id in parens, no tags
      (let-values ((((a)) 1))
	(list a))
    => '(1))

  (check	;single binding, multiple ids in parens, no tags
      (let-values ((((a) (b) (c)) (values 1 2 3)))
	(list a b c))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;single binding, single id, single tag
      (let-values ((((a <n>)) 1))
	(list a (a neg)))
    => '(1 -1))

  (check	;single binding, multiple ids, single tag
      (let-values ((((a <n>) (b <n>) (c <n>)) (values 1 2 3)))
  	(list a (a neg)
	      b (b neg)
	      c (c neg)))
    => '(1 -1 2 -2 3 -3))

  (check	;multiple bindings, single id, single tags
      (let-values ((((a <n>)) 1)
		   (((b <n>)) 2)
		   (((c <n>)) 3))
  	(list a (a neg)
	      b (b neg)
	      c (c neg)))
    => '(1 -1 2 -2 3 -3))

  (check	;multiple bindings, multiple ids, with tags
      (let-values ((((a <n>) (b <n>) (c <n>)) (values 1 2 3))
		   (((d <n>) (e <n>) (f <n>)) (values 4 5 6))
		   (((g <n>) (h <n>) (i <n>)) (values 7 8 9)))
  	(list a b c d e f g h i
	      (a neg) (b neg) (c neg) (d neg) (e neg) (f neg) (g neg) (h neg) (i neg)))
    => '(1 2 3 4 5 6 7 8 9
	   -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check	;mixed bindings, with tags
      (let-values ((((a <n>))	1)
		   ((d)		4)
		   ((g (h <n>) i)	(values 7 8 9)))
  	(list a (a neg) d g h (h neg) i))
    => '(1 -1 4 7 8 -8 9))

  (check	;mixed bindings, with tags
      (let-values ((((a <n>))		1)
		   ((d)			4)
		   (((g <n>) (h <n>) i)	(values 7 8 9)))
  	(list a (a neg) d g (g neg) h (h neg) i))
    => '(1 -1 4 7 -7 8 -8 9))

;;; --------------------------------------------------------------------

  (check	;multiple bindings, scope rules
      (let ((a 4) (b 5) (c 6))
	(let-values ((((a <n>) (b <n>) (c <n>)) (values 1 2 3))
		     (((d <n>) (e <n>) (f <n>)) (values a b c))
		     (((g <n>) (h <n>) (i <n>)) (values 7 8 9)))
	  (list a b c d e f g h i
		(a neg) (b neg) (c neg) (d neg) (e neg) (f neg) (g neg) (h neg) (i neg))))
    => '(1 2 3 4 5 6 7 8 9
	   -1 -2 -3 -4 -5 -6 -7 -8 -9))

;;; --------------------------------------------------------------------

  (check	;error, invalid syntax for list of bindings
      (catch-syntax-violation #f
	(%eval '(let-values #(((a) 1))
		  a)))
    => '#(((a) 1)))

  (check	;error, invalid syntax in single binding
      (catch-syntax-violation #f
	(%eval '(let-values (((a) 1) #(b 2))
		  a)))
    => '(((a) 1) #(b 2)))

  (check	;error, duplicate identifiers
      (catch-syntax-violation #f
	(%eval '(let-values (((a (a <n>)) (values 1 2)))
		  a)))
    => 'a)

  (check	;error, duplicate identifiers
      (catch-syntax-violation #f
	(%eval '(let-values (((a) 1) ((a) 2))
		  a)))
    => 'a)

  #t)


(parametrise ((check-test-name	'let-star-values))

  (define-label <n>
    (virtual-fields (immutable neg)))

  (define (<n>-neg n)
    (- n))

  (define-label <t>
    (virtual-fields (immutable ten)
		    (immutable neg)))

  (define (<t>-ten n)
    (+ 10 n))

  (define (<t>-neg n)
    (- (+ 10 n)))

;;; --------------------------------------------------------------------

  (check	;no bindings
      (let*-values ()
	1)
    => 1)

  (check	;special case, var specification is a symbol
      (let*-values ((a (values 1 2 3)))
	a)
    => '(1 2 3))

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

  (check	;single binding, single id in parens, no tags
      (let*-values ((((a)) 1))
	(list a))
    => '(1))

  (check	;single binding, multiple ids in parens, no tags
      (let*-values ((((a) (b) (c)) (values 1 2 3)))
	(list a b c))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check	;single binding, single id, single tag
      (let*-values ((((a <n>)) 1))
	(list a (a neg)))
    => '(1 -1))

  (check	;single binding, multiple ids, single tag
      (let*-values ((((a <n>) (b <n>) (c <n>)) (values 1 2 3)))
	(list a (a neg)
	      b (b neg)
	      c (c neg)))
    => '(1 -1 2 -2 3 -3))

  (check	;multiple bindings, single id, single tags
      (let*-values ((((a <n>)) 1)
		    (((b <n>)) 2)
		    (((c <n>)) 3))
	(list a (a neg)
	      b (b neg)
	      c (c neg)))
    => '(1 -1 2 -2 3 -3))

  (check	;multiple bindings, multiple ids, with tags
      (let*-values ((((a <n>) (b <n>) (c <n>)) (values 1 2 3))
		    (((d <n>) (e <n>) (f <n>)) (values 4 5 6))
		    (((g <n>) (h <n>) (i <n>)) (values 7 8 9)))
	(list a b c d e f g h i
	      (a neg) (b neg) (c neg) (d neg) (e neg) (f neg) (g neg) (h neg) (i neg)))
    => '(1 2 3 4 5 6 7 8 9
	   -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check	;mixed bindings, with tags
      (let*-values ((((a <n>))	1)
		    ((d)		4)
		    ((g (h <n>) i)	(values 7 8 9)))
	(list a (a neg) d g h (h neg) i))
    => '(1 -1 4 7 8 -8 9))

  (check	;mixed bindings, with tags
      (let*-values ((((a <n>))			1)
		    ((d)			4)
		    (((g <n>) (h <n>) i)	(values 7 8 9)))
	(list a (a neg) d g (g neg) h (h neg) i))
    => '(1 -1 4 7 -7 8 -8 9))

  (check	;mixed bindings, with tags
      (let*-values ((((a <n>))			1)
		    ((d)			4)
		    ((g (h <n>) (i <n>))	(values 7 8 9)))
	(list a (a neg) d g h (h neg) i (i neg)))
    => '(1 -1 4 7 8 -8 9 -9))

;;; --------------------------------------------------------------------

  (check	;multiple bindings, scope rules
      (let ((a 4) (b 5) (c 6))
	(let*-values ((((a <n>) (b <n>) (c <n>)) (values 1 2 3))
		      (((d <n>) (e <n>) (f <n>)) (values a b c))
		      (((g <n>) (h <n>) (i <n>)) (values 7 8 9)))
	  (list a b c d e f g h i
		(a neg) (b neg) (c neg) (d neg) (e neg) (f neg) (g neg) (h neg) (i neg))))
    => '(1 2 3 1 2 3 7 8 9
	   -1 -2 -3 -1 -2 -3 -7 -8 -9))

  (check	;multiple bindings, scope rules
      (let ((a 4) (b 5) (c 6))
	(let*-values ((((a <n>) (b <n>) (c <n>)) (values 1 2 3))
		      (((d <n>) (e <n>) (f <n>)) (values (a neg) (b neg) (c neg)))
		      (((g <n>) (h <n>) (i <n>)) (values 7 8 9)))
	  (list a b c d e f g h i
		(a neg) (b neg) (c neg) (d neg) (e neg) (f neg) (g neg) (h neg) (i neg))))
    => '(1 2 3 -1 -2 -3 7 8 9
	   -1 -2 -3 1 2 3 -7 -8 -9))

  (check	;correct duplicate identifiers, no tags
      (let*-values ((((a <n>)) 1)
		    (((a <n>)) 2))
	a)
    => 2)

;;; --------------------------------------------------------------------

  (check	;error, invalid syntax for list of bindings
      (catch-syntax-violation #f
	(%eval '(let*-values #(((a) 1))
		  a)))
    => '#(((a) 1)))

  (check	;error, invalid syntax in single binding
      (catch-syntax-violation #f
	(%eval '(let*-values (((a) 1) #(b 2))
		  a)))
    => '(((a) 1) #(b 2)))

  (check	;error, duplicate identifiers
      (catch-syntax-violation #f
	(%eval '(let*-values (((a (a <n>)) (values 1 2)))
		  a)))
    => 'a)

  #t)


(parametrise ((check-test-name	'do))

  (check
      (do ((a 0 (+ 1 a))
	   (b 3 (- b 1)))
	  ((zero? b) a))
    => 3)

  (check
      (do ((a 0 (+ 1 a))
	   ((b <integer>) 3 (- b 1)))
	  ((b zero?) a))
    => 3)

  (check
      (do (((a <integer>) 0 (+ 1 a))
	   ((b <integer>) 3 (- b 1)))
	  ((b zero?) (a odd?)))
    => #t)

  #t)


(parametrise ((check-test-name	'do-star))

  (check
      (do* ((a 0 (+ 1 a))
	    (b 3 (- b 1)))
	  ((zero? b) a))
    => 3)

  (check
      (do* ((a 0 (+ 1 a))
	    ((b <integer>) 3 (- b 1)))
	  ((b zero?)
	   a))
    => 3)

  (check
      (do* (((a <integer>) 0 (+ 1 a))
	    ((b <integer>) 3 (- b 1)))
	  ((b zero?)
	   (a odd?)))
    => #t)

  #t)


(parametrise ((check-test-name	'receive))

;;; untagged

  (check
      (receive (a b c)
	  (values 1 2 3)
	(list a b c))
    => '(1 2 3))

  (check
      (receive args
	  (values 1 2 3)
	args)
    => '(1 2 3))

  (check
      (receive (a)
	  1
	a)
    => 1)

;;; tagged

  (check
      (receive ((a <number>) (b <number>) (c <number>))
	  (values 1 2 3)
	(list (a string)
	      (b string)
	      (c string)))
    => '("1" "2" "3"))

  (check
      (receive #(args <list>)
	  (values 1 2 3)
	(args length))
    => 3)

  (check
      (receive ((a <number>))
	  1
	(a string))
    => "1")

;;; --------------------------------------------------------------------
;;; related tests

  (check
      (call-with-values
	  (lambda ()
	    (values 1 2 3))
	(lambda args
	  (length args)))
    => 3)

  (check
      (call-with-values
	  (lambda ()
	    (values 1 2 3))
	(lambda/tags #(args <list>)
	  (args length)))
    => 3)

  #t)


(parametrise ((check-test-name	'receive-and-return))

;;; untagged

  (check
      (with-result
       (let-values (((A B C) (receive-and-return (a b c)
				 (values 1 2 3)
			       (add-result (list a b c)))))
	 (vector A B C)))
    => '(#(1 2 3) ((1 2 3))))

  (check
      (with-result
       (let-values (((A B C) (receive-and-return args
				 (values 1 2 3)
			       (add-result args))))
	 (vector A B C)))
    => '(#(1 2 3) ((1 2 3))))

  (check
      (with-result
       (let ((A (receive-and-return (a)
		    1
		  (add-result a))))
	 A))
    => '(1 (1)))

;;; tagged

  (check
      (with-result
       (let-values (((A B C) (receive-and-return ((a <number>) (b <number>) (c <number>))
				 (values 1 2 3)
			       (add-result (list (a string)
						 (b string)
						 (c string))))))
	 (vector A B C)))
    => '(#(1 2 3) (("1" "2" "3"))))

  (check
      (with-result
       (let-values (((A B C) (receive-and-return #(args <list>)
				 (values 1 2 3)
			       (add-result (args length)))))
	 (vector A B C)))
    => '(#(1 2 3) (3)))

  (check
      (with-result
       (let ((A (receive-and-return ((a <number>))
		    1
		  (add-result (a string)))))
	 A))
    => '(1 ("1")))

  #t)


(parametrise ((check-test-name	'define-values))

;;;; untagged

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

;;; --------------------------------------------------------------------
;;; tagged

  (check
      (let ()
	(define-values ((a <integer>))
	  123)
	(list a (a positive?) (a negative?)))
    => '(123 #t #f))

  (check
      (let ()
  	(define-values ((a <integer>) (b <string>) (c <pair>))
  	  (values 123 "ciao" '(1 . 2)))
  	(list a (a odd?)
  	      b (b length)
  	      c (c cdr)))
    => '(123 #t  "ciao" 4 (1 . 2) 2))


  #f)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
