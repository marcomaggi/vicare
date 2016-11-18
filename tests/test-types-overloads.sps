;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for overloaded functions
;;;Date: Sun May 29, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (test-types-overloads)
  (options typed-language)
  (import (vicare)
    (only (vicare expander)
	  type-annotation=?
	  type-annotation-super-and-sub?
	  type-annotation-common-ancestor
	  type-annotation-ancestors
	  type-annotation-syntax
	  type-annotation-matching
	  type-signature-super-and-sub?
	  type-signature-common-ancestor
	  type-signature-matching
	  type-signature-union)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: tests for overloaded functions\n")


(parametrise ((check-test-name	'base))

  (check
      (internal-body
	(define/overload (fun {O <fixnum>})
	  (list 'fixnum O))

	(define/overload (fun {O <string>})
	  (list 'string O))

	(define/overload (fun {A <vector>} {B <vector>})
	  (list 'vectors (vector-append A B)))

	(values (fun 123)
		(fun "ciao")
		(fun '#(1) '#(2))))
    => '(fixnum 123) '(string "ciao") '(vectors #(1 2)))

  ;;No arguments.
  ;;
  (check
      (internal-body
	(define/overload (fun)
	  1)

	(define/overload (fun {A <fixnum>})
	  A)

	(values (fun) (fun 2)))
    => 1 2)

  ;;Specialisations ranking.
  ;;
  (check
      (internal-body
	(define/overload (fun {O <number>})
	  `(number ,O))

	(define/overload (fun {O <real>})
	  `(real ,O))

	(define/overload (fun {O <fixnum>})
	  `(fixnum ,O))

	(values (fun (cast-signature (<number>) 123))
		(fun (cast-signature (<real>)   123))
		(fun (cast-signature (<fixnum>) 123))))
    => '(number 123) '(real 123) '(fixnum 123))

  ;;Specialisations ranking.
  ;;
  (check
      (internal-body
	(define/overload (fun {O <number>})
	  `(number ,O))

	(define/overload (fun {O <real>})
	  `(real ,O))

	(define/overload (fun {O <fixnum>})
	  `(fixnum ,O))

	(values (fun 1+2i)
		(fun 3.4)
		(fun 5)))
    => '(number 1+2i) '(real 3.4) '(fixnum 5))

  #t)


(parametrise ((check-test-name	'records))

;;; no parent

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method (get-a)
	    (alpha-a this))
	  (method (get-b)
	    (alpha-b this)))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (method-call get-a O)
		(method-call get-b O))
	(values 1 2))
    => 1 2)

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method (get-a)
	    (alpha-a this))
	  (method (get-b)
	    (alpha-b this))
	  (method (set-a v)
	    (alpha-a-set! this v))
	  (method (set-b v)
	    (alpha-b-set! this v)))

	(define {O alpha}
	  (make-alpha 1 2))

	(method-call set-a O 10)
	(method-call set-b O 20)
	(values (method-call get-a O)
		(method-call get-b O)))
    => 10 20)

;;; --------------------------------------------------------------------
;;; calling parent's methods

  ;;Record-type with parent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields one two)
	  (method (sum-them)
	    (+ (duo-one this)
	       (duo-two this))))

	(define-record-type trio
	  (parent duo)
	  (fields three)
	  (method (mul-them)
	    (* (duo-one this)
	       (duo-two this)
	       (trio-three this))))

	(define {O trio}
	  (make-trio 3 5 7))

	(values (method-call sum-them O)
		(method-call mul-them O)))
    => (+ 3 5) (* 3 5 7))

  ;;Record-type with parent and grandparent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields one two)
	  (method (sum-them)
	    (+ (duo-one this)
	       (duo-two this))))

	(define-record-type trio
	  (parent duo)
	  (fields three)
	  (method (mul-them)
	    (* (duo-one this)
	       (duo-two this)
	       (trio-three this))))

	(define-record-type quater
	  (parent trio)
	  (fields four)
	  (method (list-them)
	    (list (duo-one this)
		  (duo-two this)
		  (trio-three this)
		  (quater-four this))))

	(define {O quater}
	  (make-quater 3 5 7 11))

	(values (method-call sum-them O)
		(method-call mul-them O)
		(method-call list-them O)))
    => (+ 3 5) (* 3 5 7) (list 3 5 7 11))

;;; --------------------------------------------------------------------
;;; dot notation

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method (get-a)
	    (alpha-a this))
	  (method (get-b)
	    (alpha-b this)))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (.get-a O)
		(.get-b O)))
    => 1 2)

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method (get-a)
	    (alpha-a this))
	  (method (get-b)
	    (alpha-b this))
	  (method (set-a v)
	    (alpha-a-set! this v))
	  (method (set-b v)
	    (alpha-b-set! this v)))

	(define {O alpha}
	  (make-alpha 1 2))

	(.set-a O 10)
	(.set-b O 20)
	(values (.get-a O)
		(.get-b O)))
    => 10 20)

;;; --------------------------------------------------------------------
;;; actual overloadgin with multiple implementations

  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (method (doit {A <fixnum>})
	    (list (.a this) (.b this) 'fixnum A))
	  (method (doit {A <symbol>})
	    (list (.a this) (.b this) 'symbol A))
	  (method (doit {A <number>} {B <number>})
	    (list (.a this) (.b this) 'numbers A B)))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (.doit O 123)
		(.doit O 'ciao)
		(.doit O 3 4)))
    => '(1 2 fixnum 123) '(1 2 symbol ciao) '(1 2 numbers 3 4))

  (check
      (internal-body
	(define-record-type <duo>
	  (fields one two)
	  (method (doit)
	    (+ (.one this) (.two this)))
	  (method (doit {C <number>})
	    (* C (+ (.one this) (.two this)))))

	(define O
	  (new <duo> 1 2))

	(values (.doit O)
		(.doit O 3)))
    => 3 9)

  #t)


(parametrise ((check-test-name	'late-binding-raw))

  (import (only (vicare system type-descriptors)
		make-overloaded-function-descriptor
		overloaded-function-descriptor.register!
		overloaded-function-late-binding
		case-lambda-descriptors.clause-signature*
		closure-type-descr.signature))

  (define (doit-string {O <string>})
    (list 'string O))

  (define (doit-fixnum {O <fixnum>})
    (list 'fixnum O))

  (define doit-string.des
    (car (case-lambda-descriptors.clause-signature*
	  (closure-type-descr.signature (type-descriptor (lambda (<string>) => (<list>)))))))

  (define doit-fixnum.des
    (car (case-lambda-descriptors.clause-signature*
	  (closure-type-descr.signature (type-descriptor (lambda (<fixnum>) => (<list>)))))))

  (define ofd
    (receive-and-return (ofd)
	(make-overloaded-function-descriptor 'doit (list (cons doit-string.des doit-string)))
      (overloaded-function-descriptor.register! ofd doit-fixnum.des doit-fixnum)))

;;; --------------------------------------------------------------------

  (check
      (overloaded-function-late-binding ofd "ciao")
    => '(string "ciao"))

  (check
      (overloaded-function-late-binding ofd 123)
    => '(fixnum 123))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'late-binding-syntax-application))

  (check
      (internal-body
	(define/overload (doit {O <string>})
	  (list 'string O))

	(define/overload (doit {O <fixnum>})
	  (list 'fixnum O))

	(values (doit (cast-signature (<top>) "ciao"))
		(doit (cast-signature (<top>) 123))))
    => '(string "ciao") '(fixnum 123))

  (check
      (internal-body
	(define/overload (doit {O <string>})
	  (list 'string O))

	(define/overload (doit {O <fixnum>})
	  (list 'fixnum O))

	(define (call-it {obj <top>})
	  (doit obj))

	(values (call-it "ciao")
		(call-it 123)))
    => '(string "ciao") '(fixnum 123))

;;; --------------------------------------------------------------------

  (check-for-true
   (internal-body
     (define/overload (doit {O <string>})
       (list 'string O))

     (define/overload (doit {O <fixnum>})
       (list 'fixnum O))

     (try
	 (doit 'hello)
       (catch E
	 ((&overloaded-function-late-binding-error)
	  (when #f
	    (debug-print (condition-message E)))
	  #t)
	 (else E)))))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'late-binding-syntax-reference))

  (check
      (internal-body
	(define/overload (doit {O <string>})
	  (list 'string O))

	(define/overload (doit {O <fixnum>})
	  (list 'fixnum O))

	(values (apply doit '("ciao"))
		(apply doit '(123))))
    => '(string "ciao") '(fixnum 123))

  (check
      (internal-body
	(define/overload (doit {O <string>})
	  (list 'string O))

	(define/overload (doit {O <fixnum>})
	  (list 'fixnum O))

	(define (those func)
	  (func "ciao"))

	(define (that func)
	  (func 123))

	(values (those doit)
		(that doit)))
    => '(string "ciao") '(fixnum 123))

  (check
      (internal-body
	(define/overload ({doit <top>} {O <string>})
	  (list 'string O))

	(define/overload ({doit <top>} {O <fixnum>})
	  (list 'fixnum O))

	(map doit '("ciao" 123)))
    => '((string "ciao") (fixnum 123)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'overloaded-methods))

  (check
      (internal-body
	(define-record-type <alpha>
	  (fields a)
	  (method (doit)
	    (.a this))
	  (method (doit {S <symbol>})
	    (list S (.a this))))

	(define-record-type <beta>
	  (fields b)
	  (method (doit)
	    (.b this))
	  (method (doit {S <symbol>})
	    (list S (.b this))))

	(define (those O)
	  (.doit O))

	(define (that O)
	  (.doit O 'ciao))

	(define A
	  (new <alpha> 1))

	(define B
	  (new <beta> 2))

	(values (those A) (those B)
		(that A) (that B)))
    => 1 2 '(ciao 1) '(ciao 2))

  #| end of PARAMETRISE |# )


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
