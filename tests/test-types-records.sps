;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for records, typed language
;;;Date: Sat Sep 12, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-records-typed)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: records with typed language\n")


;;;; helpers

(define (%eval sexp)
  (with-exception-handler
      (lambda (E)
	(unless (warning? E)
	  (raise-continuable E)))
    (lambda ()
      (eval sexp THE-ENVIRONMENT
	    (expander-options typed-language)
	    (compiler-options)))))

(define THE-ENVIRONMENT
  (environment '(vicare)
	       '(vicare language-extensions interfaces)))

(define (%print-message bool str)
  (when bool
    (fprintf (current-error-port) "~a\n" str)))


(parametrise ((check-test-name	'descriptor-syntax))

  (internal-body	;application syntax
    (define-record-type alpha
      (fields a b c))

    (check
	(eq? (record-type-descriptor alpha)
	     (type-descriptor alpha))
      => #t)

    (void))

  #t)


(parametrise ((check-test-name	'maker-syntax))

  (internal-body ;application syntax
    (define-record-type alpha
      (fields a b c))

    (define-record-type beta
      (fields a b))

    (check
	(let ((reco (new alpha 1 2 3)))
	  (alpha? reco))
      => #t)

    (check
	(let ((reco (new beta 1 2)))
	  (beta? reco))
      => #t)

    (check
	(expander::type-signature.syntax-object (type-of (new alpha 1 2 3)))
      (=> expander::syntax=?)
      (list #'alpha))

    (check
	(expander::type-signature.syntax-object (type-of (make-alpha 1 2 3)))
      (=> expander::syntax=?)
      (list #'alpha))

    (void))

  #t)


(parametrise ((check-test-name	'constructor-signature))

  (check
      (internal-body
	(define-record-type <duo>
	  (fields {one <fixnum>} {two <string>})
	  (constructor-signature
	    (lambda (<fixnum> <string>) => (<duo>))))

	(is-a? (new <duo> 1 "ciao") <duo>))
    => #t)

  (check
      (internal-body
	(define-record-type <duo>
	  (fields {one <fixnum>} {two <string>})
	  (constructor-signature
	    (lambda (<fixnum> <string>) => (<duo>))))

	(expander::syntax=? (type-annotation-syntax (type-of (new <duo> 1 "ciao")))
		  #'<duo>))
    => #t)

  (check
      (internal-body
	(define-record-type <alpha>
	  (fields {A <fixnum>}
		  {B <string>})
	  (protocol
	    (lambda (make-record)
	      (lambda (A)
		(make-record A (number->string A)))))
	  (constructor-signature
	    (lambda (<fixnum>) => (<alpha>))))

	(is-a? (new <alpha> 1) <alpha>))
    => #t)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'predicate-syntax))

  (internal-body

    (define-record-type alpha
      (fields a b c))

    (define-record-type beta
      (fields a b c))

    (check
	(let ((reco (make-alpha 1 2 3)))
	  (is-a? reco alpha))
      => #t)

    (check
	(let ((reco (make-alpha 1 2 3)))
	  (is-a? reco beta))
      => #f)

;;;

    (check
	(let ((reco (make-alpha 1 2 3)))
	  ((is-a? _ alpha) reco))
      => #t)

;;;

    (check
	(is-a? 123 alpha)
      => #f)

    (check
	(is-a? 123 beta)
      => #f)

;;;

    (check
	(let ((reco (make-alpha 1 2 3)))
	  (is-a? reco <record>))
      => #t)

    (check
	(let ((reco (make-alpha 1 2 3)))
	  ((is-a? _ <record>) reco))
      => #t)

    (void))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-record-type duo
	  (fields one two))

	(is-a? (new duo 1 2) duo))
    => #t)

  (check
      (internal-body
	(define-record-type duo
	  (fields one two))
	(expander::type-signature.syntax-object (type-of (is-a? (read) duo))))
    (=> expander::syntax=?)
    (list #'<boolean>))

  (check
      (internal-body
	(define-record-type duo
	  (fields one two))
	(expander::type-signature.syntax-object (type-of (duo? (read)))))
    (=> expander::syntax=?)
    (list #'<boolean>))

  (void))


(parametrise ((check-test-name	'method-syntax))

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
		(method-call get-b O)))
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

  ;;Field accessors.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (method-call a O)
		(method-call b O)))
    => 1 2)

  ;;Field accessors and mutators.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a)
		  (mutable b)))

	(define {O alpha}
	  (make-alpha 1 2))

	(method-call a O 10)
	(method-call b O 20)
	(values (method-call a O)
		(method-call b O)))
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

  ;;Accessing fields of record-type with parent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields one two))

	(define-record-type trio
	  (parent duo)
	  (fields three))

	(define {O trio}
	  (make-trio 3 5 7))

	(values (method-call one O)
		(method-call two O)
		(method-call three O)))
    => 3 5 7)

  ;;Accessing and mutating fields of record-type with parent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields (mutable one)
		  (mutable two)))

	(define-record-type trio
	  (parent duo)
	  (fields (mutable three)))

	(define {O trio}
	  (make-trio 3 5 7))

	(method-call one O 30)
	(method-call two O 50)
	(method-call three O 70)
	(values (method-call one O)
		(method-call two O)
		(method-call three O)))
    => 30 50 70)

  ;;Accessing fields of record-type with parent and grandparent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields one two))

	(define-record-type trio
	  (parent duo)
	  (fields three))

	(define-record-type quater
	  (parent trio)
	  (fields four))

	(define {O quater}
	  (make-quater 3 5 7 11))

	(values (method-call one O)
		(method-call two O)
		(method-call three O)
		(method-call four O)))
    => 3 5 7 11)

  ;;Accessing and mutating fields of record-type with parent and grandparent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields (mutable one)
		  (mutable two)))

	(define-record-type trio
	  (parent duo)
	  (fields (mutable three)))

	(define-record-type quater
	  (parent trio)
	  (fields (mutable four)))

	(define {O quater}
	  (make-quater 3 5 7 11))

	(method-call one O 1)
	(method-call two O 2)
	(method-call three O 3)
	(method-call four O 4)
	(values (method-call one O)
		(method-call two O)
		(method-call three O)
		(method-call four O)))
    => 1 2 3 4)

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

  ;;Accessing and mutating fields of record-type with parent and grandparent.
  ;;
  (check
      (internal-body

	(define-record-type duo
	  (fields (mutable one)
		  (mutable two)))

	(define-record-type trio
	  (parent duo)
	  (fields (mutable three)))

	(define-record-type quater
	  (parent trio)
	  (fields (mutable four)))

	(define {O quater}
	  (make-quater 3 5 7 11))

	(.one O 1)
	(.two O 2)
	(.three O 3)
	(.four O 4)
	(values (.one O)
		(.two O)
		(.three O)
		(.four O)))
    => 1 2 3 4)

;;; --------------------------------------------------------------------
;;; misc method examples

  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (method (doit c d)
	    (+ (alpha-a this) (alpha-b this) c d)))

	(define {O alpha}
	  (make-alpha 1 2))

	(.doit O 3 4))
    => (+ 1 2 3 4))

  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (method (doit . arg*)
	    (apply + (alpha-a this) (alpha-b this) arg*)))

	(define {O alpha}
	  (make-alpha 1 2))

	(.doit O 3 4))
    => (+ 1 2 3 4))

;;; --------------------------------------------------------------------
;;; documentation examples

  (check
      (internal-body

	(define-record-type duo
	  (fields one two)
	  (method (sum-them)
	    (+ (duo-one this)
	       (duo-two this)))
	  (method (mul-them)
	    (* (duo-one this)
	       (duo-two this))))

	(define {O duo}
	  (new duo 3 5))

	(values (method-call sum-them O)
		(method-call mul-them O)))
    => 8 15)

  (check
      (internal-body

	(define-record-type duo
	  (fields one two)
	  (method (sum-them)
	    (+ (.one this)
	       (.two this)))
	  (method (mul-them)
	    (* (.one this)
	       (.two this))))

	(define {O duo}
	  (new duo 3 5))

	(values (.sum-them O)
		(.mul-them O)))
    => 8 15)

  (check
      (internal-body
	(define-record-type alpha
	  (fields (mutable a)))

	(define {O alpha}
	  (new alpha 1))

	(method-call a O 2)
	(method-call a O))
    => 2)

;;; --------------------------------------------------------------------
;;; errors

  ;;Field and concrete method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (fields it)
		      (method (it)
			2))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'it)

  #t)


(parametrise ((check-test-name	'methods-late-binding))

;;; METHOD-CALL, late binding, calling methods

  ;;Calling methods.
  ;;
  (check
      (internal-body
	(import (only (psyntax system $all)
		      record-type-method-retriever))

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (method (get-a)
	    (alpha-a this))
	  (method (get-b)
	    (alpha-b this)))

	(define {O alpha}
	  (make-alpha 1 2))

	(define ({the-record <top>})
	  O)

	(values (.get-a (the-record))
		(.get-b (the-record))))
    => 1 2)

  ;;Calling methods and parent's methods.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (method (add-them)
	    (+ (.a this) (.b this))))

	(define-record-type beta
	  (parent alpha)
	  (method (mul-them)
	    (* (.a this) (.b this))))

	(define {O beta}
	  (make-beta 3 5))

	(define ({the-record <top>})
	  O)

	(values (.add-them (the-record))
		(.mul-them (the-record))))
    => 8 15)

  ;;Calling methods, parent's methods, grandparent's methods.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (method (add-them)
	    (+ (.a this) (.b this))))

	(define-record-type beta
	  (parent alpha)
	  (method (mul-them)
	    (* (.a this) (.b this))))

	(define-record-type gamma
	  (parent beta)
	  (method (list-them)
	    (list (.a this) (.b this))))

	(define {O gamma}
	  (make-gamma 3 5))

	(define ({the-record <top>})
	  O)

	(values (.add-them (the-record))
		(.mul-them (the-record))
		(.list-them (the-record))))
    => 8 15 '(3 5))

;;; --------------------------------------------------------------------
;;; METHOD-CALL-LATE-BINDING, calling methods

  ;;Calling methods.
  ;;
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

	(define ({the-record <top>})
	  O)

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))

	(values (method-call-late-binding 'get-a #f (the-record))
		(method-call-late-binding 'get-b #f (the-record))))
    => 1 2)

  ;;Calling methods and parent's methods.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (method (add-them)
	    (+ (.a this) (.b this))))

	(define-record-type beta
	  (parent alpha)
	  (method (mul-them)
	    (* (.a this) (.b this))))

	(define {O beta}
	  (make-beta 3 5))

	(define ({the-record <top>})
	  O)

	(values (method-call-late-binding 'add-them #f (the-record))
		(method-call-late-binding 'mul-them #f (the-record))))
    => 8 15)

  ;;Calling methods, parent's methods, grandparent's methods.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (method (add-them)
	    (+ (.a this) (.b this))))

	(define-record-type beta
	  (parent alpha)
	  (method (mul-them)
	    (* (.a this) (.b this))))

	(define-record-type gamma
	  (parent beta)
	  (method (list-them)
	    (list (.a this) (.b this))))

	(define {O gamma}
	  (make-gamma 3 5))

	(define ({the-record <top>})
	  O)

	(values (method-call-late-binding 'add-them #f (the-record))
		(method-call-late-binding 'mul-them #f (the-record))
		(method-call-late-binding 'list-them #f (the-record))))
    => 8 15 '(3 5))

;;; --------------------------------------------------------------------
;;; METHOD-CALL-LATE-BINDING, accessing fields

  ;;Accessing fields.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b))

	(define O
	  (make-alpha 1 2))

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a #f O)
		(method-call-late-binding 'b #f O)))
    => 1 2)

  ;;Accessing parent's fields.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b))

	(define-record-type beta
	  (parent alpha)
	  (fields c d))

	(define O
	  (make-beta 1 2 3 4))

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a #f O)
		(method-call-late-binding 'b #f O)
		(method-call-late-binding 'c #f O)
		(method-call-late-binding 'd #f O)))
    => 1 2 3 4)

  ;;Accessing grandparent's fields.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a b))

	(define-record-type beta
	  (parent alpha)
	  (fields c d))

	(define-record-type gamma
	  (parent beta)
	  (fields e f))

	(define O
	  (make-gamma 1 2 3 4 5 6))

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a #f O)
		(method-call-late-binding 'b #f O)
		(method-call-late-binding 'c #f O)
		(method-call-late-binding 'd #f O)
		(method-call-late-binding 'e #f O)
		(method-call-late-binding 'f #f O)))
    => 1 2 3 4 5 6)

;;; --------------------------------------------------------------------
;;; METHOD-CALL-LATE-BINDING, accessing and mutating fields

  ;;Accessing fields.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b)))

	(define O
	  (make-alpha 1 2))

	(method-call-late-binding 'a #f O 11)
	(method-call-late-binding 'b #f O 22)

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a #f O)
		(method-call-late-binding 'b #f O)))
    => 11 22)

  ;;Accessing parent's fields.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b)))

	(define-record-type beta
	  (parent alpha)
	  (fields (mutable c) (mutable d)))

	(define O
	  (make-beta 1 2 3 4))

	(method-call-late-binding 'a #f O 11)
	(method-call-late-binding 'b #f O 22)
	(method-call-late-binding 'c #f O 33)
	(method-call-late-binding 'd #f O 44)

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a #f O)
		(method-call-late-binding 'b #f O)
		(method-call-late-binding 'c #f O)
		(method-call-late-binding 'd #f O)))
    => 11 22 33 44)

  ;;Accessing grandparent's fields.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b)))

	(define-record-type beta
	  (parent alpha)
	  (fields (mutable c) (mutable d)))

	(define-record-type gamma
	  (parent beta)
	  (fields (mutable e) (mutable f)))

	(define O
	  (make-gamma 1 2 3 4 5 6))

	(method-call-late-binding 'a #f O 11)
	(method-call-late-binding 'b #f O 22)
	(method-call-late-binding 'c #f O 33)
	(method-call-late-binding 'd #f O 44)
	(method-call-late-binding 'e #f O 55)
	(method-call-late-binding 'f #f O 66)

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a #f O)
		(method-call-late-binding 'b #f O)
		(method-call-late-binding 'c #f O)
		(method-call-late-binding 'd #f O)
		(method-call-late-binding 'e #f O)
		(method-call-late-binding 'f #f O)))
    => 11 22 33 44 55 66)

  (void))


(parametrise ((check-test-name	'overloaded-methods))

  ;;Early binding.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a)
	  (method (doit)
	    (.a this))
	  (method (doit {O <fixnum>})
	    (+ O (.doit this))))

	(define {O alpha}
	  (new alpha 1))

	(values (.doit O)
		(.doit O 10)))
    => 1 11)

  ;;Late binding.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a)
	  (method (doit)
	    (.a this))
	  (method (doit {O <fixnum>})
	    (+ O (.doit this))))

	(define {O <top>}
	  (new alpha 1))

	(values (.doit O)
		(.doit O 10)))
    => 1 11)

  ;;Late binding.
  ;;
  (check
      (internal-body

	(define-record-type alpha
	  (fields a)
	  (method (doit)
	    (.a this))
	  (method (doit {O <fixnum>})
	    (+ O (.doit this))))

	(define {O <top>}
	  (new alpha 1))

	(define (fun {O <top>})
	  (.doit O))

	(fun O))
    => 1)

  (void))


(parametrise ((check-test-name	'virtual-methods))

  ;;Hierarchy of two record-types.  No virtual  methods, keep it as reference to show
  ;;how virtual methods work.
  ;;
  (check
      (internal-body
	(define-record-type <super>
	  (method (doit)
	    1))

	(define-record-type <sub>
	  (parent <super>)
	  (method (doit)
	    2))

	(define up	(new <super>))
	(define down	(new <sub>))

	(define (fun {O <super>})
	  (.doit O))

	(values (.doit up)
		(.doit down)
		(fun up)
		(fun down)))
    => 1 2 1 1)

;;; --------------------------------------------------------------------
;;; hierarchy of two record-types

  ;;VIRTUAL-METHOD and METHOD clauses.
  ;;
  (check
      (internal-body
	(define-record-type <super>
	  (fields value)
	  (virtual-method (doit)
	    (.value this)))

	(define-record-type <sub>
	  (parent <super>)
	  (method (doit)
	    (.value this)))

	(define up	(new <super> 1))
	(define down	(new <sub>   2))

	(define (fun {O <super>})
	  (.doit O))

	(values (.doit up)
		(.doit down)
		(fun up)
		(fun down)))
    => 1 2 1 2)

  ;;VIRTUAL-METHOD and VIRTUAL-METHOD clauses.  Complex signatures.
  ;;
  (check
      (internal-body
	(define-record-type <super>
	  (fields value)
	  (virtual-method ({doit <number>} {S <nestring>})
	    (.value this)))

	(define-record-type <sub>
	  (parent <super>)
	  (virtual-method ({doit <fixnum>} {S <string>})
	    (.value this)))

	(define up	(new <super> 1))
	(define down	(new <sub>   2))

	(define (fun {O <super>})
	  (.doit O "ciao"))

	(values (.doit up "ciao")
		(.doit down "ciao")
		(fun up)
		(fun down)))
    => 1 2 1 2)

  ;;VIRTUAL-METHOD and SEAL-METHOD clauses.  Complex signatures.
  ;;
  (check
      (internal-body
	(define-record-type <super>
	  (fields value)
	  (virtual-method ({doit <number>} {S <nestring>})
	    (.value this)))

	(define-record-type <sub>
	  (parent <super>)
	  (seal-method ({doit <fixnum>} {S <string>})
	    (.value this)))

	(define up	(new <super> 1))
	(define down	(new <sub>   2))

	(define (fun {O <super>})
	  (.doit O "ciao"))

	(values (.doit up "ciao")
		(.doit down "ciao")
		(fun up)
		(fun down)))
    => 1 2 1 2)

;;; --------------------------------------------------------------------

  ;;Hierarchy of three record-types.
  ;;
  (check
      (internal-body
	(define-record-type <super>
	  (virtual-method (doit)
	    1))

	(define-record-type <middle>
	  (parent <super>)
	  (virtual-method (doit)
	    2))

	(define-record-type <sub>
	  (parent <middle>)
	  (method (doit)
	    3))

	(define up	(new <super>))
	(define middle	(new <middle>))
	(define down	(new <sub>))

	(define (fun-super {O <super>})
	  (.doit O))

	(define (fun-middle {O <middle>})
	  (.doit O))

	(values (.doit up)
		(.doit middle)
		(.doit down)
		(fun-super up)
		(fun-super middle)
		(fun-super down)
		(fun-middle middle)
		(fun-middle down)))
    => 1 2 3
    1 2 3
    2 3)

  ;;Hierarchy of four record-types.
  ;;
  (check
      (internal-body
	(define-record-type <one>
	  (virtual-method (doit)
	    1))

	(define-record-type <two>
	  (parent <one>)
	  (virtual-method (doit)
	    2))

	(define-record-type <three>
	  (parent <two>)
	  (virtual-method (doit)
	    3))

	(define-record-type <four>
	  (parent <three>)
	  (virtual-method (doit)
	    4))

	(define one	(new <one>))
	(define two	(new <two>))
	(define three	(new <three>))
	(define four	(new <four>))

	(define (fun {O <one>})
	  (.doit O))

	(values (.doit one) (.doit two) (.doit three) (.doit four)
		(fun one) (fun two) (fun three) (fun four)))
    => 1 2 3 4
    1 2 3 4)

;;; --------------------------------------------------------------------
;;; errors

  ;;Virtual method and field with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (fields it)
		      (virtual-method (it)
			2))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'it)

  ;;Virtual method and concrete method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (method         (doit)	1)
		      (virtual-method (doit)	2))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'doit)


  ;;Field and parent's virtual method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <super>
		      (virtual-method (it)
			1))
		    (define-record-type <sub>
		      (parent <super>)
		      (fields it))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'it)

;;;

  ;;The sub-type concrete method has signature incompatible with the parent's virtual
  ;;method.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <super>
		      (fields value)
		      (virtual-method ({doit <fixnum>} {S <string>})
			(.value this)))

		    (define-record-type <sub>
		      (parent <super>)
		      (method ({doit <number>} {S <flonum>})
			(.value this)))

		    (void)))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'doit)

  (void))


(parametrise ((check-test-name	'seal-methods))

  ;;Overloaded seal method.
  ;;
  (check
      (internal-body
	(define-record-type <it>
	  (method (doit)
	    1)
	  (method (doit a)
	    2))

	(define O (new <it>))

	(values (.doit O)
		(.doit O 99)))
    => 1 2)

  ;;Hierarchy of record-types.
  ;;
  (check
      (internal-body
	(define-record-type <super>
	  (method (doit)
	    1))

	(define-record-type <sub>
	  (parent <super>)
	  (seal-method (doit)
	    2))

	(define up	(new <super>))
	(define down	(new <sub>))

	(define (fun {O <super>})
	  (.doit O))

	(values (.doit up)
		(.doit down)
		(fun up)
		(fun down)))
    => 1 2 1 1)

;;; --------------------------------------------------------------------
;;; errors

  ;;Method and seal method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (method      (doit)	1)
		      (seal-method (doit)	2))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'doit)

  ;;Field and seal method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (fields it)
		      (seal-method (it)
			2))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'it)

;;; seal method in the parent

  ;;Field and parent's seal method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <super>
		      (seal-method (it)
			1))
		    (define-record-type <sub>
		      (parent <super>)
		      (fields it))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'it)

  ;;Concrete method and parent's seal method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <super>
		      (seal-method (it)
			1))
		    (define-record-type <sub>
		      (parent <super>)
		      (method (it)
			2))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'it)

  ;;Virtual method and parent's seal method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <super>
		      (seal-method (it)
			1))
		    (define-record-type <sub>
		      (parent <super>)
		      (virtual-method (it)
			2))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'it)

  ;;Seal method and parent's seal method with the same name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <super>
		      (seal-method (it)
			1))
		    (define-record-type <sub>
		      (parent <super>)
		      (seal-method (it)
			2))
		    #t))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'it)

  (void))


(parametrise ((check-test-name	'fields-protection))

  ;;Accessing public field from public method.
  ;;
  (begin
    (check
	(internal-body
	  (define-record-type <it>
	    (fields public inner)
	    (method (outer)
	      (.inner this)))

	  (.outer (new <it> 1)))
      => 1)
    (check
	(internal-body
	  (define-record-type <it>
	    (public
	      (fields inner))
	    (method (outer)
	      (.inner this)))

	  (.outer (new <it> 1)))
      => 1)
    #| end of BEGIN |# )

  ;;Accessing protected field from public method.
  ;;
  (check
      (internal-body
	(define-record-type <it>
	  (fields protected inner)
	  (method (outer)
	    (.inner this)))

	(.outer (new <it> 1)))
    => 1)

  ;;Accessing private field from public method.
  ;;
  (check
      (internal-body
	(define-record-type <it>
	  (fields private inner)
	  (method (outer)
	    (.inner this)))

	(.outer (new <it> 1)))
    => 1)

  ;;Accessing super-type's public field.
  ;;
  (check
      (internal-body
	(define-record-type <blue>
	  (public
	    (fields light)))

	(define-record-type <dark-blue>
	  (parent <blue>)
	  (method (doit)
	    (.light this)))

	(.doit (new <dark-blue> 1)))
    => 1)

  ;;Accessing super-type's protected field.
  ;;
  (check
      (internal-body
	(define-record-type <blue>
	  (protected
	    (fields light)))

	(define-record-type <dark-blue>
	  (parent <blue>)
	  (method (doit)
	    (.light this)))

	(.doit (new <dark-blue> 1)))
    => 1)

;;; --------------------------------------------------------------------
;;; definition errors

  ;;Field and method with equal name.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (fields private doit)
		      (method private (doit) 1))
		    (void)))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'doit)

;;; --------------------------------------------------------------------
;;; access errors

  ;;Attempt to access private field.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <blue>
		      (private
			(fields light)))

		    (.light (new <blue> 1))))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'light)

  ;;Attempt to access protected field.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <blue>
		      (protected
			(fields light)))

		    (.light (new <blue> 1))))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'light)

  ;;Attempt to access super-type's private field.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <blue>
		      (private
			(fields light)))

		    (define-record-type <dark-blue>
		      (parent <blue>)
		      (method (doit)
			(.light this)))

		    (.doit (new <dark-blue> 1))))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'light)

  (void))


(parametrise ((check-test-name	'methods-protection))

  ;;Calling public method from public method.
  ;;
  (begin
    (check
	(internal-body
	  (define-record-type <it>
	    (method public (inner)
		    1)
	    (method (outer)
	      (.inner this)))

	  (.outer (new <it>)))
      => 1)
    (check
	(internal-body
	  (define-record-type <it>
	    (public
	      (method (inner)
		1))
	    (method (outer)
	      (.inner this)))

	  (.outer (new <it>)))
      => 1)
    #| end of BEGIN |# )

  ;;Calling protected method from public method.
  ;;
  (check
      (internal-body
	(define-record-type <it>
	  (method protected (inner)
		  1)
	  (method (outer)
	    (.inner this)))

	(.outer (new <it>)))
    => 1)

  ;;Calling private method from public method.
  ;;
  (check
      (internal-body
	(define-record-type <it>
	  (method private (inner)
		  1)
	  (method (outer)
	    (.inner this)))

	(.outer (new <it>)))
    => 1)

  ;;Calling super-type's public method.
  ;;
  (check
      (internal-body
	(define-record-type <blue>
	  (public
	    (method (light)
	      1)))

	(define-record-type <dark-blue>
	  (parent <blue>)
	  (method (doit)
	    (.light this)))

	(.doit (new <dark-blue>)))
    => 1)

  ;;Calling super-type's protected method.
  ;;
  (check
      (internal-body
	(define-record-type <blue>
	  (protected
	    (method (light)
	      1)))

	(define-record-type <dark-blue>
	  (parent <blue>)
	  (method (doit)
	    (.light this)))

	(.doit (new <dark-blue>)))
    => 1)

;;; --------------------------------------------------------------------
;;; definition errors

  ;;Methods with equal name but different protection level: public, private.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (method public (doit) 1)
		      (method private (doit {O <string>}) 2))
		    (void)))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'doit)

  ;;Methods with equal name but different protection level: public, protected.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (method public (doit) 1)
		      (method protected (doit {O <string>}) 2))
		    (void)))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'doit)

  ;;Methods with equal name but different protection level: protected, private.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <it>
		      (method protected (doit) 1)
		      (method private (doit {O <string>}) 2))
		    (void)))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'doit)

;;; --------------------------------------------------------------------
;;; calling errors

  ;;Attempt to call private method.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <blue>
		      (private
			(method (light)
			  1)))

		    (.light (new <blue>))))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'light)

  ;;Attempt to call protected method.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <blue>
		      (protected
			(method (light)
			  1)))

		    (.light (new <blue>))))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'light)

  ;;Attempt to call super-type's private method.
  ;;
  (check
      (try
	  (%eval '(internal-body
		    (define-record-type <blue>
		      (private
			(method (light)
			  1)))

		    (define-record-type <dark-blue>
		      (parent <blue>)
		      (method (doit)
			(.light this)))

		    (.doit (new <dark-blue>))))
	(catch E
	  ((&syntax)
	   (%print-message #f (condition-message E))
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => 'light)

  (void))


(parametrise ((check-test-name	'typed-fields))

  (check
      (internal-body
	(define-record-type duo
	  (fields {one <fixnum>} {two <flonum>}))
	(define {O duo}
	  (new duo 1 2.3))
	(values (.one O)
		(.two O)))
    => 1 2.3)

  (check
      (internal-body
	(define-record-type duo
	  (fields {one <fixnum>} {two <flonum>}))
	(define {O duo}
	  (new duo 1 2.3))
	(values (expander::type-signature.syntax-object (type-of (.one O)))
		(expander::type-signature.syntax-object (type-of (.two O)))))
    (=> expander::syntax=?)
    (list #'<fixnum>)
    (list #'<flonum>))

  (check
      (internal-body
	(define-record-type duo
	  (fields {one <fixnum>} {two <flonum>}))
	(define {O duo}
	  (new duo 1 2.3))
	(values (expander::type-signature.syntax-object (type-of (duo-one O)))
		(expander::type-signature.syntax-object (type-of (duo-two O)))))
    (=> expander::syntax=?)
    (list #'<fixnum>)
    (list #'<flonum>))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-record-type alpha
	  (fields {a <fixnum>}
		  (mutable   {b <flonum>})
		  (immutable {c <flonum>})
		  (mutable   {d <flonum>} get-alpha-d set-alpha-d)
		  (immutable {e <flonum>} get-alpha-e)))
	(define {O alpha}
	  (new alpha 1 2. 3. 4. 5.))
	(values (.a O) (.b O) (.c O) (.d O) (.e O)))
    => 1 2. 3. 4. 5.)

  (void))


(parametrise ((check-test-name	'hash-function))

  ;;No parent.  Early binding method call
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields one two)
	  (hash-function
	    (lambda ()
	      (lambda ({O duo})
		(fx+ (fixnum-hash (.one O))
		     (fixnum-hash (.two O)))))))
	(hash (new duo 1 2)))
    => (fx+ (fixnum-hash 1)
	    (fixnum-hash 2)))

  ;;No parent.  Late binding method call.
  ;;
  (check
      (internal-body
	(define-record-type duo
	  (fields one two)
	  (hash-function
	    (lambda ()
	      (lambda ({O duo})
		(fx+ (fixnum-hash (.one O))
		     (fixnum-hash (.two O)))))))
	(let (({O <top>} (new duo 1 2)))
	  (hash O)))
    => (fx+ (fixnum-hash 1)
	    (fixnum-hash 2)))

;;; --------------------------------------------------------------------

  ;;Parent without hash function.  Early binding method call.
  ;;
  (check
      (internal-body
	(define-record-type alpha
	  (fields {a <non-negative-fixnum>}))
	(define-record-type beta
	  (parent alpha)
	  (fields {b <non-negative-fixnum>})
	  (hash-function
	    (lambda (alpha-hash)
	      (assert (not alpha-hash))
	      (lambda ({O beta})
		(fx+ (.a O) (.b O))))))
	(hash (new beta 1 2)))
    => 3)

  ;;Parent with hash function.  Early binding method call.
  ;;
  (check
      (internal-body
	(define-record-type alpha
	  (fields {a <non-negative-fixnum>})
	  (hash-function
	    (lambda ()
	      (lambda ({O alpha})
		(.a O)))))
	(define-record-type beta
	  (parent alpha)
	  (fields {b <non-negative-fixnum>})
	  (hash-function
	    (lambda ({alpha-hash (hash-function alpha)})
	      (lambda ({O beta})
		(fx+ (alpha-hash O) (.b O))))))
	(hash (new beta 1 2)))
    => 3)

  ;;Parent with hash function.  Early binding method call.  Call through methods.
  ;;
  (check
      (internal-body
	(define-record-type alpha
	  (fields {a <non-negative-fixnum>})
	  (hash-function
	    (lambda ()
	      (lambda ({O alpha})
		(.a O)))))
	(define-record-type beta
	  (parent alpha)
	  (fields {b <non-negative-fixnum>})
	  (hash-function
	    (lambda ({alpha-hash (hash-function alpha)})
	      (lambda ({O beta})
		(let (({A alpha} O))
		  (fx+ (hash A) (.b O)))))))
	(hash (new beta 1 2)))
    => 3)

  (void))


;;;; done

(collect 'fullest)
(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
