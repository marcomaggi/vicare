;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for class mixins
;;;Date: Sat Jan  8, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(check-display "*** testing class mixins\n")


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


(parametrise ((check-test-name	'class))

  (let ()
    (define-mixin <stuff>
      (fields c)
      (method (doit (o <stuff>))
	(+ 1 (o c))))

    (define-class <base>
      (fields a))

    (define-class <one>
      (parent <base>)
      (fields p)
      (mixins <stuff>))

    (define-class <two>
      (parent <base>)
      (fields q)
      (mixins <stuff>))

    (check
	(let (((o <one>) (<one> (#;a 10 #;p 20 #;c 30))))
	  (o doit))
      => 31)

    (check
	(let (((o <two>) (<two> (#;a 10 #;q 20 #;c 30))))
	  (o doit))
      => 31)

    #f)

;;; --------------------------------------------------------------------

  #;(let ()	;mixin accessing the fields of the receiving class

    (define-mixin <stuff1>
      (fields c)
      (method (doit (o <stuff1>))
	(+ (o a) (o c))))

    (define-class <alpha1>
      (fields a)
      (mixins <stuff1>))

    (check
	(let (((o <alpha1>) (<alpha1> (#;c 1 #;a 2))))
	  (o doit))
      => 3)

    #f)

  #t)


(parametrise ((check-test-name	'label))

  (define-mixin <stuff2>
    (virtual-fields (immutable c car))
    (method (doit (o <stuff2>))
      (+ 1 (o c))))

  (define-label <one2>
    (virtual-fields (immutable p cdr))
    (mixins <stuff2>))

  (define-label <two2>
    (virtual-fields (immutable q cdr))
    (mixins <stuff2>))

  (check
      (let (((o <one2>) '(10 . 20)))
        (o doit))
    => 11)

  (check
      (let (((o <two2>) '(10 . 20)))
        (o doit))
    => 11)

  #t)


(parametrise ((check-test-name	'multiple-mixins))

  (define-mixin <stuff3>
    (fields second)
    (method (doit (o <stuff3>))
      (+ 1 (o second))))

  (define-mixin <other-stuff3>
    (fields third))

  (define-class <base3>
    (fields first))

  (define-class <one3>
    (parent <base3>)
    (mixins <stuff3> <other-stuff3>))

  (check
      (let (((o <one3>) (<one3> (#;first 10 #;second 20 #;third 30))))
        (o doit))
    => 21)

  (check
      (let (((o <one3>) (<one3> (#;first 10 #;second 20 #;third 30))))
        (list (o first) (o second) (o third)))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'composite-mixins))

  (define-mixin <stuff4>
    (fields c)
    (method (doit (o <stuff4>))
      (+ 1 (o c))))

  (define-mixin <other-stuff4>
    (fields p)
    (mixins <stuff4>))

  (define-class <base4>
    (fields a))

  (define-class <one4>
    (parent <base4>)
    (mixins <other-stuff4>))

  (check
      (let (((o <one4>) (<one4> (#;a 10 #;p 20 #;c 30))))
        (o doit))
    => 31)

  (check
      (let (((o <one4>) (<one4> (#;a 10 #;p 20 #;c 30))))
        (list (o a) (o p) (o c)))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'substitutions))

  (let ()

    (define-syntax <sequence> (syntax-rules ()))

    (define-mixin <the-sequence>
      (fields (subject <sequence>))
      (method (get-first (o <the-sequence>))
	(o subject[0])))

    (define-class <the-string>
      (mixins (<the-sequence>
	       (<sequence>       <string>))))

    (define-class <the-vector>
      (mixins (<the-sequence>
	       (<sequence>       <vector>))))

    (check
	(let (((o <the-string>) (<the-string> ("ciao"))))
	  (list (o subject length) (o get-first)))
      => '(4 #\c))

    (check
	(let (((o <the-vector>) (<the-vector> ((vector 'c 'i 'a 'o)))))
	  (list (o subject length) (o get-first)))
      => '(4 c))

    #f)
  #t)


(parametrise ((check-test-name	'errors))

  (check 	;empty mixin definition
      (catch-syntax-violation #f
	(%eval '(let ()
		  (define-mixin <alpha10>))))
    => #f)

;;; --------------------------------------------------------------------

  (check	;duplicated field names in class and mixin
      (catch-syntax-violation #f
	(%eval '(let ()
		  (define-mixin <alpha11>
		    (fields A))
		  (define-class <beta11>
		    (fields A)
		    (mixins <alpha11>)))))
    => 'A)

  (check	;duplicated field names in class and composite mixin
      (catch-syntax-violation #f
	(%eval '(let ()
		  (define-mixin <alpha12>
		    (fields A))
		  (define-mixin <beta12>
		    (fields B)
		    (mixins <alpha12>))
		  (define-class <delta12>
		    (fields A)
		    (mixins <beta12>)))))
    => 'A)

  #;(check	;recursive mixin composition
      (catch-syntax-violation #t
	(%eval '(let ()
		  (define-mixin <beta13>
		    (fields A)
		    (mixins <beta13>)))))
    => '<beta13>)

;;; --------------------------------------------------------------------

  #;(check	;same mixin multiple selection in class
      (catch-syntax-violation #t
	(%eval '(let ()
		  (define-mixin <beta14>
		    (fields A))
		  (define-class <alpha14>
		    (mixins <beta14> <beta14>)))))
    => '<beta14>)

  #;(check	;same mixin multiple selection in label
      (catch-syntax-violation #t
	(%eval '(let ()
		  (define-mixin <beta15>
		    (fields A))
		  (define-label <alpha15>
		    (mixins <beta15> <beta15>)))))
    => '<beta15>)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
