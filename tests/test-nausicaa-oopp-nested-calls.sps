;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (nausicaa language oopp)
;;;Date: Thu Nov 28, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa: OOPP syntax, nested members usage\n")


;;;; helpers

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


(parametrise ((check-test-name	'methods/embedded-definition/nested-application))

  (let ()	;list tag specification
    (define-label <beta>
      (nongenerative nested-method-application.<beta>)
      (parent <list>)
      (method ((map <beta>) self func)
	(map func self)))

    (define-label <alpha>
      (nongenerative nested-method-application.<alpha>)
      (method ((one <beta>) self a b)
	(list self a b)))

    ;; ----------

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1))
      => '(1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => fold-left 0 +))
      => (+ 1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => map -
	     => fold-left 0 +))
      => (+ -1 -11 -21))

    (void))

  (let ()	;vector tag specification
    (define-label <beta>
      (nongenerative nested-method-application.<beta>)
      (parent <list>)
      (method (#(map <beta>) self func)
	(map func self)))

    (define-label <alpha>
      (nongenerative nested-method-application.<alpha>)
      (method (#(one <beta>) self a b)
	(list self a b)))

    ;; ----------

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1))
      => '(1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => fold-left 0 +))
      => (+ 1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => map -
	     => fold-left 0 +))
      => (+ -1 -11 -21))

    (void))

  #t)


(parametrise ((check-test-name	'methods/lambda-definition/nested-application))

  (let ()
    (define-label <beta>
      (nongenerative nested-method-application.<beta>)
      (parent <list>)
      (method #(map <beta>)
	(lambda (self func)
	  (map func self))))

    (define-label <alpha>
      (nongenerative nested-method-application.<alpha>)
      (method #(one <beta>)
	(lambda (self a b)
	  (list self a b))))

    ;; ----------

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1))
      => '(1 11 21))

    (check
    	(let (((O <alpha>) 0))
    	  (O one 10 20
    	     => map add1
    	     => fold-left 0 +))
      => (+ 1 11 21))

    (check
    	(let (((O <alpha>) 0))
    	  (O one 10 20
    	     => map add1
    	     => map -
    	     => fold-left 0 +))
      => (+ -1 -11 -21))

    (void))

  #t)


(parametrise ((check-test-name	'methods/syntax-definition/nested-application))

  (let ()	;list tag specification.
    (define-label <beta>
      (nongenerative nested-method-application.<beta>)
      (parent <list>)
      (method-syntax (map <beta>)
	(syntax-rules ()
	  ((_ self func)
	   (map func self)))))

    (define-label <alpha>
      (nongenerative nested-method-application.<alpha>)
      (method-syntax (one <beta>)
	(syntax-rules ()
	  ((_ self a b)
	   (list self a b)))))

    ;; ----------

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1))
      => '(1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => fold-left 0 +))
      => (+ 1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => map -
	     => fold-left 0 +))
      => (+ -1 -11 -21))

    (void))

  (let ()	;vector tag specification.
    (define-label <beta>
      (nongenerative nested-method-application.<beta>)
      (parent <list>)
      (method-syntax #(map <beta>)
	(syntax-rules ()
	  ((_ self func)
	   (map func self)))))

    (define-label <alpha>
      (nongenerative nested-method-application.<alpha>)
      (method-syntax #(one <beta>)
	(syntax-rules ()
	  ((_ self a b)
	   (list self a b)))))

    ;; ----------

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1))
      => '(1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => fold-left 0 +))
      => (+ 1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => map -
	     => fold-left 0 +))
      => (+ -1 -11 -21))

    (void))

  #t)


(parametrise ((check-test-name	'methods/extern-definition/nested-application))

  (let ()	;list tag specification
    (define-label <beta>
      (nongenerative nested-method-application.<beta>)
      (parent <list>)
      (methods ((map <beta>) <beta>-map)))

    (define (<beta>-map self func)
      (map func self))

    (define-label <alpha>
      (nongenerative nested-method-application.<alpha>)
      (methods ((one <beta>) <alpha>-one)))

    (define (<alpha>-one self a b)
      (list self a b))

    ;; ----------

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1))
      => '(1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => fold-left 0 +))
      => (+ 1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => map -
	     => fold-left 0 +))
      => (+ -1 -11 -21))

    (void))

  (let ()	;vector tag specification
    (define-label <beta>
      (nongenerative nested-method-application.<beta>)
      (parent <list>)
      (methods (#(map <beta>) <beta>-map)))

    (define (<beta>-map self func)
      (map func self))

    (define-label <alpha>
      (nongenerative nested-method-application.<alpha>)
      (methods (#(one <beta>) <alpha>-one)))

    (define (<alpha>-one self a b)
      (list self a b))

    ;; ----------

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1))
      => '(1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => fold-left 0 +))
      => (+ 1 11 21))

    (check
	(let (((O <alpha>) 0))
	  (O one 10 20
	     => map add1
	     => map -
	     => fold-left 0 +))
      => (+ -1 -11 -21))

    (void))

  #t)


(parametrise ((check-test-name	'getter/nested-application))

  (let ()

    (define-label <fixnum-vector>
      (parent <vector>)
      (getter
       (lambda (stx)
	 (syntax-case stx (=>)
	   ((?var ((?index)))
	    #'(vector-ref ?var ?index))
	   ((?var ((?index)) => ?form0 ?form ...)
	    #'(let (((fx <fixnum>) (vector-ref ?var ?index)))
		(fx ?form0 ?form ...)))
	   ))))

    (check
	(let (((O <fixnum-vector>) '#(0 1 2 3)))
	  (O[1] => string))
      => "1")

    (check
	(let (((O <fixnum-vector>) '#(0 1 2 3)))
	  (O[1] => odd?))
      => #t)

    (check
	(let (((O <fixnum-vector>) '#(0 1 2 3)))
	  (O[2] => * 10))
      => 20)

    (void))

  (let ()	;nester ?var usage

    (define-label <fixnum-vector>
      (parent <vector>)
      (getter
       (lambda (stx)
	 (syntax-case stx (=>)
	   ((?var ((?index)))
	    #'(vector-ref ?var ?index))
	   ((?var ((?index)) => ?form0 ?form ...)
	    #'(let (((fx <fixnum>) (?var[?index])))
		(fx ?form0 ?form ...)))
	   ))))

    (check
	(let (((O <fixnum-vector>) '#(0 1 2 3)))
	  (O[1] => string))
      => "1")

    (check
	(let (((O <fixnum-vector>) '#(0 1 2 3)))
	  (O[1] => odd?))
      => #t)

    (check
	(let (((O <fixnum-vector>) '#(0 1 2 3)))
	  (O[2] => * 10))
      => 20)

    (void))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'catch-assertion 'scheme-indent-function 1)
;; eval: (put 'catch-syntax-violation 'scheme-indent-function 1)
;; End:
