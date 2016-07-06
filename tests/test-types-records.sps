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

  #t)


(parametrise ((check-test-name	'case-method-syntax))

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (case-method get-a
	    (()
	     (alpha-a this)))
	  (case-method get-b
	    (()
	     (alpha-b this))))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (method-call get-a O)
		(method-call get-b O)))
    => 1 2)

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (case-method on-a
	    (()
	     (alpha-a this))
	    ((v)
	     (alpha-a-set! this v)))
	  (case-method on-b
	    (()
	     (alpha-b this))
	    ((v)
	     (alpha-b-set! this v))))

	(define {O alpha}
	  (make-alpha 1 2))

	(method-call on-a O 10)
	(method-call on-b O 20)
	(values (method-call on-a O)
		(method-call on-b O)))
    => 10 20)

;;; --------------------------------------------------------------------
;;; dot notation

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (case-method on-a
	    (()
	     (alpha-a this)))
	  (case-method on-b
	    (()
	     (alpha-b this))))

	(define {O alpha}
	  (make-alpha 1 2))

	(values (.on-a O)
		(.on-b O)))
    => 1 2)

  (check
      (internal-body

	(define-record-type alpha
	  (fields (mutable a) (mutable b))
	  (case-method on-a
	    (()
	     (alpha-a this))
	    ((v)
	     (alpha-a-set! this v)))
	  (case-method on-b
	    (()
	     (alpha-b this))
	    ((v)
	     (alpha-b-set! this v))))

	(define {O alpha}
	  (make-alpha 1 2))

	(.on-a O 10)
	(.on-b O 20)
	(values (.on-a O)
		(.on-b O)))
    => 10 20)

;;; --------------------------------------------------------------------
;;; misc method examples

  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (case-method doit
	    ((c d)
	     (+ (alpha-a this) (alpha-b this) c d))))

	(define {O alpha}
	  (make-alpha 1 2))

	(.doit O 3 4))
    => (+ 1 2 3 4))

  (check
      (internal-body

	(define-record-type alpha
	  (fields a b)
	  (case-method doit
	    (arg*
	     (apply + (alpha-a this) (alpha-b this) arg*))))

	(define {O alpha}
	  (make-alpha 1 2))

	(.doit O 3 4))
    => (+ 1 2 3 4))

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

	(values (method-call-late-binding 'get-a (the-record))
		(method-call-late-binding 'get-b (the-record))))
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

	(values (method-call-late-binding 'add-them (the-record))
		(method-call-late-binding 'mul-them (the-record))))
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

	(values (method-call-late-binding 'add-them (the-record))
		(method-call-late-binding 'mul-them (the-record))
		(method-call-late-binding 'list-them (the-record))))
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
	(values (method-call-late-binding 'a O)
		(method-call-late-binding 'b O)))
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
	(values (method-call-late-binding 'a O)
		(method-call-late-binding 'b O)
		(method-call-late-binding 'c O)
		(method-call-late-binding 'd O)))
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
	(values (method-call-late-binding 'a O)
		(method-call-late-binding 'b O)
		(method-call-late-binding 'c O)
		(method-call-late-binding 'd O)
		(method-call-late-binding 'e O)
		(method-call-late-binding 'f O)))
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

	(method-call-late-binding 'a O 11)
	(method-call-late-binding 'b O 22)

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a O)
		(method-call-late-binding 'b O)))
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

	(method-call-late-binding 'a O 11)
	(method-call-late-binding 'b O 22)
	(method-call-late-binding 'c O 33)
	(method-call-late-binding 'd O 44)

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a O)
		(method-call-late-binding 'b O)
		(method-call-late-binding 'c O)
		(method-call-late-binding 'd O)))
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

	(method-call-late-binding 'a O 11)
	(method-call-late-binding 'b O 22)
	(method-call-late-binding 'c O 33)
	(method-call-late-binding 'd O 44)
	(method-call-late-binding 'e O 55)
	(method-call-late-binding 'f O 66)

	#;(debug-print (property-list (record-type-uid (record-type-descriptor alpha))))
	(values (method-call-late-binding 'a O)
		(method-call-late-binding 'b O)
		(method-call-late-binding 'c O)
		(method-call-late-binding 'd O)
		(method-call-late-binding 'e O)
		(method-call-late-binding 'f O)))
    => 11 22 33 44 55 66)

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
