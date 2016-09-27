;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for type propagation through built-in syntaxes
;;;Date: Sat Apr 30, 2016
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
(program (test-types-rhs-type-propagation)
  (options typed-language predicate-type-propagation)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (only (vicare expander)
	  &expand-time-type-signature-violation)
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
(check-display "*** testing Vicare typed language: type propagation through built-in syntaxes\n")


;;;; helpers

(define-syntax doit
  (syntax-rules (=>)
    ((_ ?expr => ?expected)
     (check
	 (.syntax-object (type-of ?expr))
       (=> expander::syntax=?)
       (syntax ?expected)))
    ))

(define-syntax doit-result
  (syntax-rules (=>)
    ((_ ?expr => ?expected)
     (check
	 (with-result
	   (.syntax-object (type-of ?expr)))
       (=> expander::syntax=?)
       (syntax ?expected)))
    ))

(define-constant THE-ENVIRONMENT
  (environment '(vicare)))

(define (%eval expr)
  (with-exception-handler
      (lambda (E)
	(unless (warning? E)
	  (raise E)))
    (lambda ()
      (eval expr
	    THE-ENVIRONMENT
	    (expander-options typed-language)
	    (compiler-options)))))

(define-syntax type-signature-violation-for-message
  (syntax-rules (=>)
    ((_ ?expr => ?message)
     (check
	 (try
	     (%eval (quote ?expr))
	   (catch E
	     ((&expand-time-type-signature-violation)
	      (condition-message E))
	     (else E)))
       => ?message))
    ))

(define-syntax assertion-violation-for-message
  (syntax-rules (=>)
    ((_ ?expr => ?message)
     (check
	 (try
	     ?expr
	   (catch E
	     ((&assertion)
	      (condition-message E))
	     (else E)))
       => ?message))
    ))


(parametrise ((check-test-name	'let-plain))

  (doit (let ((A 1))
	  A)
	=> (<positive-fixnum>))

  (doit (let ((A "ciao"))
	  A)
	=> (<nestring>))

  (doit (let ((A "ciao"))
	  (let ((B A))
	    B))
	=> (<nestring>))

  (doit (let ((A (fxadd1 1)))
	  A)
	=> (<positive-fixnum>))

  (doit (let ((A (fx+ 1 -2)))
	  A)
	=> (<fixnum>))

  (doit (let ((A (cast-signature (<top>) 1)))
	  A)
	=> (<top>))

;;; --------------------------------------------------------------------

  (doit (let (({a <fixnum>} 1)
	      ({b <fixnum>} 2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (let (({a <positive-fixnum>} 1)
	      ({b <positive-fixnum>} 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (doit (let (({a <positive-fixnum>} 1)
	      ({b <negative-fixnum>} -2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (let ((a 1)
	      (b 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (void))


(parametrise ((check-test-name	'let-star))

  (doit (let* ((A 1))
	  A)
	=> (<positive-fixnum>))

  (doit (let* ((A "ciao"))
	  A)
	=> (<nestring>))

  (doit (let* ((A "ciao"))
	  (let* ((B A))
	    B))
	=> (<nestring>))

  (doit (let* ((A (fxadd1 1)))
	  A)
	=> (<positive-fixnum>))

  (doit (let* ((A (fx+ 1 -2)))
	  A)
	=> (<fixnum>))

  (doit (let* ((A (cast-signature (<top>) 1)))
	  A)
	=> (<top>))

;;; --------------------------------------------------------------------

  (doit (let* (({a <fixnum>} 1)
	       ({b <fixnum>} 2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (let* (({a <positive-fixnum>} 1)
	       ({b <positive-fixnum>} 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (doit (let* (({a <positive-fixnum>} 1)
	       ({b <negative-fixnum>} -2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (let* ((a 1)
	       (b 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (void))


(parametrise ((check-test-name	'letrec))

  (doit (letrec ((A 1))
	  A)
	=> (<positive-fixnum>))

  (doit (letrec ((A "ciao"))
	  A)
	=> (<nestring>))

  (doit (letrec ((A "ciao"))
	  (letrec ((B A))
	    B))
	=> (<nestring>))

  (doit (letrec ((A (fxadd1 1)))
	  A)
	=> (<positive-fixnum>))

  (doit (letrec ((A (fx+ 1 -2)))
	  A)
	=> (<fixnum>))

  (doit (letrec ((A (cast-signature (<top>) 1)))
	  A)
	=> (<top>))

;;; --------------------------------------------------------------------

  (doit (letrec (({a <fixnum>} 1)
		 ({b <fixnum>} 2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (letrec (({a <positive-fixnum>} 1)
		 ({b <positive-fixnum>} 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (doit (letrec (({a <positive-fixnum>} 1)
		 ({b <negative-fixnum>} -2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (letrec ((a 1)
		 (b 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (void))


(parametrise ((check-test-name	'letrec*))

  (doit (letrec* ((A 1))
	  A)
	=> (<positive-fixnum>))

  (doit (letrec* ((A "ciao"))
	  A)
	=> (<nestring>))

  (doit (letrec* ((A "ciao"))
	  (letrec* ((B A))
	    B))
	=> (<nestring>))

  (doit (letrec* ((A (fxadd1 1)))
	  A)
	=> (<positive-fixnum>))

  (doit (letrec* ((A (fx+ 1 -2)))
	  A)
	=> (<fixnum>))

  (doit (letrec* ((A (cast-signature (<top>) 1)))
	  A)
	=> (<top>))

;;; --------------------------------------------------------------------

  (doit (letrec* (({a <fixnum>} 1)
		  ({b <fixnum>} 2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (letrec* (({a <positive-fixnum>} 1)
		  ({b <positive-fixnum>} 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (doit (letrec* (({a <positive-fixnum>} 1)
		  ({b <negative-fixnum>} -2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (letrec* ((a 1)
		  (b 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (void))


(parametrise ((check-test-name	'call-with-values))

  (doit (call-with-values
	    (lambda () 1)
	  (lambda ({_ <fixnum>} {a <fixnum>})
	    (add1 a)))
	=> (<fixnum>))

  (doit (call-with-values
	    (lambda () 1)
	  (lambda (a)
	    (add1 a)))
	=> (<exact-integer>))

;;; --------------------------------------------------------------------
;;; no return values

  (doit (call-with-values
	    (lambda () (values))
	  (lambda () 123))
	=> (<positive-fixnum>))
  (check
      (call-with-values
	  (lambda () (values))
	(lambda () 123))
    => 123)

  (doit (call-with-values
	    (lambda ()
	      (cast-signature <list> (values)))
	  (lambda () 123))
	=> (<positive-fixnum>))
  (check
      (call-with-values
	  (lambda () (cast-signature <list> (values)))
       	(lambda () 123))
    => 123)

;;; special cases

  (type-signature-violation-for-message
   (call-with-values
       (lambda () (cast-signature <nelist> (values 1)))
     (lambda () 123))
   => "type signature mismatch between producer return values and consumer expected arguments")

  (type-signature-violation-for-message
   (call-with-values
       (lambda ()
	 (cast-signature (list <fixnum> <fixnum>) (values 1 2)))
     (lambda () 123))
   => "type signature mismatch between producer return values and consumer expected arguments")

  (doit (call-with-values
	    (lambda ()
	      (cast-signature (list-of <fixnum>) (values)))
	  (lambda () 123))
	=> (<positive-fixnum>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature (list-of <fixnum>) (values)))
	(lambda () 123))
    => 123)

  (doit (call-with-values
	    (lambda ()
	      (cast-signature (list-of <fixnum>) (values 1 2)))
	  (lambda () 123))
	=> (<positive-fixnum>))
  (assertion-violation-for-message
   (call-with-values
       (lambda ()
	 (cast-signature (list-of <fixnum>) (values 1 2)))
     (lambda () 123))
   => "incorrect number of arguments")

  ;;Producer not returning.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (error #f "wrong"))
	  (lambda () 123))
	=> <bottom>)
  (check
      (try
	  (call-with-values
	      (lambda ()
		(error #f "wrong"))
	    (lambda () 123))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")
  (when #f
    (debug-print
     (expansion-of
      (call-with-values
	  (lambda ()
	    (error #f "wrong"))
	(lambda () 123)))))

  ;;Producer returns values.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda ()
	 (values 1))
     (lambda () 123))
   => "type signature mismatch between producer return values and consumer expected arguments")

;;; --------------------------------------------------------------------
;;; single return value

  ;;Typed single argument, expand-time validation.
  ;;
  (doit (call-with-values
	    (lambda () 1)
	  (lambda ({a <fixnum>})
	    a))
	=> (<fixnum>))
  (check
      (call-with-values
	  (lambda () 1)
	(lambda ({a <fixnum>})
	  a))
    => 1)

  ;;Typed single argument, run-time validation.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature (<top>) 1))
	  (lambda ({a <fixnum>})
	    a))
	=> (<fixnum>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature (<top>) 1))
	(lambda ({a <fixnum>})
	  a))
    => 1)

  ;;UNtyped single value ignored in body.  Type propagation.
  ;;
  (doit (call-with-values
	    (lambda () 1)
	  (lambda (a) 123))
	=> (<positive-fixnum>))
  (check
      (call-with-values
	  (lambda () 1)
	(lambda (a) 123))
    => 123)

  ;;UNtyped single value used in body.  Type propagation.
  ;;
  (doit (call-with-values
	    (lambda () 1)
	  (lambda (a) a))
	=> (<positive-fixnum>))
  (check
      (call-with-values
	  (lambda () 1)
	(lambda (a) a))
    => 1)

  ;;UNtyped single value used in body.  Type propagation.  More body forms
  ;;
  (doit (call-with-values
	    (lambda () 1)
	  (lambda (a)
	    (+ 1 2)
	    (list a)))
	=> ((list <positive-fixnum>)))
  (check
      (call-with-values
	  (lambda () 1)
	(lambda (a)
	  (+ 1 2)
	  (list a)))
    => '(1))

  ;;Producer returns a LIST-OF.  Untyped syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature (list-of <fixnum>) 1))
	  (lambda (a) a))
	=> (<fixnum>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature (list-of <fixnum>) 1))
	(lambda (a) a))
    => 1)

  ;;Producer returns a LIST-OF.  Typed syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature (list-of <fixnum>) 1))
	  (lambda ({a <fixnum>})
	    a))
	=> (<fixnum>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature (list-of <fixnum>) 1))
	(lambda ({a <fixnum>})
	  a))
    => 1)

  ;;Producer returns a LIST-OF with too many values.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature (list-of <fixnum>) (values 1 2)))
	  (lambda (a) a))
	=> (<fixnum>))
  (assertion-violation-for-message
   (call-with-values
       (lambda ()
	 (cast-signature (list-of <fixnum>) (values 1 2)))
     (lambda (a) a))
   => "incorrect number of values returned to single value context")

;;;

  ;;Producer returns a LIST.  Untyped syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature (list <fixnum>) 1))
	  (lambda (a) a))
	=> (<fixnum>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature (list <fixnum>) 1))
	(lambda (a) a))
    => 1)

  ;;Producer returns a LIST.  Typed syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature (list <positive-fixnum>) 1))
	  (lambda ({a <fixnum>})
	    a))
	=> (<fixnum>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature (list <positive-fixnum>) 1))
	(lambda ({a <fixnum>})
	  a))
    => 1)

  ;;Producer returns a LIST with too many values.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda ()
	 (cast-signature (list <fixnum> <flonum>) (values 1 2.3)))
     (lambda (a) a))
   => "type signature mismatch between producer return values and consumer expected arguments")

  ;;Producer returns a <nelist>.  Untyped syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature <nelist> 1))
	  (lambda (a) a))
	=> (<top>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature <nelist> 1))
	(lambda (a) a))
    => 1)

  ;;Producer returns a <nelist> with too many values.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature <nelist> (values 1 2)))
	  (lambda (a) a))
	=> (<top>))
  (assertion-violation-for-message
   (call-with-values
       (lambda ()
	 (cast-signature <nelist> (values 1 2)))
     (lambda (a) a))
   => "incorrect number of values returned to single value context")

;;;

  ;;Producer returns a "<list>".  Untyped syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature <list> 1))
	  (lambda (a) a))
	=> (<top>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature <list> 1))
	(lambda (a) a))
    => 1)

  ;;Producer returns a "<list>".  Typed syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature <list> 1))
	  (lambda ({a <fixnum>})
	    a))
	=> (<fixnum>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature <list> 1))
	(lambda ({a <fixnum>})
	  a))
    => 1)

  ;;Producer returns a <list> with multiple values.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature <list> (values 1 2 3)))
	  (lambda ({a <fixnum>})
	    a))
	=> (<fixnum>))
  (assertion-violation-for-message
   (call-with-values
       (lambda ()
	 (cast-signature <list> (values 1 2 3)))
     (lambda ({a <fixnum>})
       a))
   => "incorrect number of values returned to single value context")

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (error #f "wrong"))
	  (lambda (a) a))
	=> <bottom>)
  (check
      (try
	  (call-with-values
	      (lambda ()
		(error #f "wrong"))
	    (lambda (a) 123))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns zero values.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda () (values))
     (lambda (a) a))
   => "type signature mismatch between producer return values and consumer expected arguments")

  ;;Producer returns multiple values.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda () (values 1 2 3))
     (lambda (a) a))
   => "type signature mismatch between producer return values and consumer expected arguments")

  ;;Producer returning void.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda () (void))
     (lambda (a) a))
   => "type signature mismatch between producer return values and consumer expected arguments")

;;; --------------------------------------------------------------------
;;; fixed number of two or more mandatory arguments

  ;;Two values, typed syntactic bindings.
  ;;
  (doit (call-with-values
	    (lambda () (values 123 "ciao"))
	  (lambda ({a <positive-fixnum>} {b <nestring>})
	    (values a b)))
	=> (<positive-fixnum> <nestring>))
  (check
      (call-with-values
	  (lambda () (values 123 "ciao"))
	(lambda ({a <positive-fixnum>} {b <nestring>})
	  (values a b)))
    => 123 "ciao")

  ;;Two values, typed syntactic bindings.  Run-time validation.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (values (cast-signature (<top>) 1)
		      (cast-signature (<top>) 2.3)))
	  (lambda ({a <fixnum>} {b <flonum>})
	    (values a b)))
	=> (<fixnum> <flonum>))
  (check
      (call-with-values
	  (lambda ()
	    (values (cast-signature (<top>) 1)
		    (cast-signature (<top>) 2.3)))
	(lambda ({a <fixnum>} {b <flonum>})
	  (values a b)))
    => 1 2.3)

  ;;Two values, UNtyped syntactic bindings.  Unused bindings.  Type propagation.
  ;;
  (doit (call-with-values
	    (lambda () (values 1 2))
	  (lambda (a b)
	    (values "hello" "world")))
	=> (<nestring> <nestring>))
  (check
      (call-with-values
	  (lambda () (values 1 2))
	(lambda (a b)
	  (values "hello" "world")))
    => "hello" "world")

  ;;Two values, UNtyped syntactic bindings.  Type propagation.
  ;;
  (doit (call-with-values
	    (lambda () (values 1 2.3))
	  (lambda (a b)
	    (values a b)))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (call-with-values
	  (lambda () (values 1 2.3))
	(lambda (a b)
	  (values a b)))
    => 1 2.3)

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (call-with-values
	    (lambda () (error #f "wrong"))
	  (lambda (a b)
	    (values a b)))
	=> <bottom>)
  (check
      (try
	  (call-with-values
	      (lambda () (error #f "wrong"))
	    (lambda (a b)
	      (values a b)))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns zero values.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda () (values))
     (lambda (a b)
       (list a b)))
   => "type signature mismatch between producer return values and consumer expected arguments")

  ;;Producer returns one value.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda () (values 1))
     (lambda (a b)
       (list a b)))
   => "type signature mismatch between producer return values and consumer expected arguments")

;;;

  ;;Two Untyped bindings.  Producer returns an UNtyped list of two values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <list> (values 1 2.3)))
	  (lambda (a b)
	    (values a b)))
	=> (<top> <top>))
  (check
      (call-with-values
	  (lambda () (cast-signature <list> (values 1 2.3)))
	(lambda (a b)
	  (values a b)))
    => 1 2.3)

  ;;Two typed  bindings.  Producer returns an  UNtyped list of two  values.  Run-time
  ;;validation.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <list> (values 1 2.3)))
	  (lambda ({a <fixnum>} {b <flonum>})
	    (values a b)))
	=> (<fixnum> <flonum>))
  (check
      (call-with-values
	  (lambda () (cast-signature <list> (values 1 2.3)))
	(lambda ({a <fixnum>} {b <flonum>})
	  (values a b)))
    => 1 2.3)

  ;;The producer returns a <list> with too many values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <list> (values 1 2 3)))
	  (lambda (a b)
	    (values a b)))
	=> (<top> <top>))
  (assertion-violation-for-message
   (call-with-values
       (lambda () (cast-signature <list> (values 1 2 3)))
     (lambda (a b)
       (values a b)))
   => "incorrect number of arguments")

  ;;The producer returns a <list> with too few values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <list> (values 1)))
	  (lambda (a b)
	    (values a b)))
	=> (<top> <top>))
  (assertion-violation-for-message
   (call-with-values
       (lambda () (cast-signature <list> (values 1)))
     (lambda (a b)
       (values a b)))
   => "incorrect number of arguments")

;;;

  ;;Two Untyped bindings.  Producer returns a <nelist>.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <nelist> (values 1 2.3)))
	  (lambda (a b)
	    (values a b)))
	=> (<top> <top>))
  (check
      (call-with-values
	  (lambda () (cast-signature <nelist> (values 1 2.3)))
	(lambda (a b)
	  (values a b)))
    => 1 2.3)

  ;;Two typed bindings.  Producer returns a <nelist>.  Run-time validation.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <nelist> (values 1 2.3)))
	  (lambda ({a <fixnum>} {b <flonum>})
	    (values a b)))
	=> (<fixnum> <flonum>))
  (check
      (call-with-values
	  (lambda () (cast-signature <nelist> (values 1 2.3)))
	(lambda ({a <fixnum>} {b <flonum>})
	  (values a b)))
    => 1 2.3)

  ;;The producer returns a <nelist> with too many values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <nelist> (values 1 2 3)))
	  (lambda (a b)
	    (values a b)))
	=> (<top> <top>))
  (assertion-violation-for-message
   (call-with-values
       (lambda () (cast-signature <nelist> (values 1 2 3)))
     (lambda (a b)
       (values a b)))
   => "incorrect number of arguments")

  ;;The producer returns a <nelist> with too few values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <nelist> (values 1)))
	  (lambda (a b)
	    (values a b)))
	=> (<top> <top>))
  (assertion-violation-for-message
   (call-with-values
       (lambda () (cast-signature <nelist> (values 1)))
     (lambda (a b)
       (values a b)))
   => "incorrect number of arguments")

;;;

  ;;Two Untyped bindings.  Producer returns a typed list of two values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature (list-of <number>) (values 1 2.3)))
	  (lambda (a b)
	    (values a b)))
	=> (<number> <number>))
  (check
      (call-with-values
	  (lambda () (cast-signature (list-of <number>) (values 1 2.3)))
	(lambda (a b)
	  (values a b)))
    => 1 2.3)

  ;;Two  typed bindings.   Producer returns  a typed  list of  two values.   Run-time
  ;;validation.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature (list-of <number>) (values 1 2.3)))
	  (lambda ({a <fixnum>} {b <flonum>})
	    (values a b)))
	=> (<fixnum> <flonum>))
  (check
      (call-with-values
	  (lambda () (cast-signature (list-of <number>) (values 1 2.3)))
	(lambda ({a <fixnum>} {b <flonum>})
	  (values a b)))
    => 1 2.3)

  ;;The producer returns a LIST-OF with too many values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature (list-of <fixnum>) (values 1 2 3)))
	  (lambda (a b)
	    (values a b)))
	=> (<fixnum> <fixnum>))
  (assertion-violation-for-message
   (call-with-values
       (lambda () (cast-signature (list-of <fixnum>) (values 1 2 3)))
     (lambda (a b)
       (values a b)))
   => "incorrect number of arguments")

  ;;The producer returns a LIST-OF with too few values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature (list-of <fixnum>) (values 1)))
	  (lambda (a b)
	    (values a b)))
	=> (<fixnum> <fixnum>))
  (assertion-violation-for-message
   (call-with-values
       (lambda () (cast-signature (list-of <fixnum>) (values 1)))
     (lambda (a b)
       (values a b)))
   => "incorrect number of arguments")

;;;

  ;;Two Untyped bindings.  Producer returns a typed list of two values.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature (list <number> <number>) (values 1 2.3)))
	  (lambda (a b)
	    (values a b)))
	=> (<number> <number>))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature (list <number> <number>) (values 1 2.3)))
	(lambda (a b)
	  (values a b)))
    => 1 2.3)

  ;;Two  typed bindings.   Producer returns  a typed  list of  two values.   Run-time
  ;;validation.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature (list <number> <number>) (values 1 2.3)))
	  (lambda ({a <fixnum>} {b <flonum>})
	    (values a b)))
	=> (<fixnum> <flonum>))
  (check
      (call-with-values
	  (lambda () (cast-signature (list <number> <number>) (values 1 2.3)))
	(lambda ({a <fixnum>} {b <flonum>})
	  (values a b)))
    => 1 2.3)

  ;;The producer returns a LIST with too many values.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda () (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3)))
     (lambda (a b)
       (values a b)))
   => "type signature mismatch between producer return values and consumer expected arguments")

  ;;The producer returns a LIST with too few values.
  ;;
  (type-signature-violation-for-message
   (call-with-values
       (lambda () (cast-signature (list <fixnum>) (values 1)))
     (lambda (a b)
       (values a b)))
   => "type signature mismatch between producer return values and consumer expected arguments")

;;; --------------------------------------------------------------------
;;; unspecified number of values, args arguments

  ;;Typed catch-all syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda () (values 1 2 3))
	  (lambda {vals (list-of <fixnum>)}
	    vals))
	=> ((list-of <fixnum>)))
  (check
      (call-with-values
	  (lambda () (values 1 2 3))
	(lambda {vals (list-of <fixnum>)}
	  vals))
    => '(1 2 3))

  ;;Untyped catch-all syntactic binding.
  ;;
  (doit (call-with-values
	    (lambda () (values 1 2 3))
	  (lambda vals vals))
	=> ((list <positive-fixnum> <positive-fixnum> <positive-fixnum>)))
  (check
      (call-with-values
	  (lambda () (values 1 2 3))
	(lambda vals vals))
    => '(1 2 3))

  ;;Untyped catch-all syntactic bindings.  The producer returns a single value.
  ;;
  (doit (call-with-values
	    (lambda () 1)
	  (lambda vals vals))
	=> ((list <positive-fixnum>)))
  (check
      (call-with-values
	  (lambda () 1)
	(lambda vals vals))
    => '(1))

;;;

  ;;Untyped catch-all syntactic binding.  The  producer returns an unspecified number
  ;;of unspecified values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <list> (values 1 2 3)))
	  (lambda vals vals))
	=> (<list>))
  (check
      (call-with-values
	  (lambda () (cast-signature <list> (values 1 2 3)))
	(lambda vals vals))
    => '(1 2 3))

  ;;Untyped catch-all syntactic binding.  The producer returns a <nelist>.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <nelist> (values 1 2 3)))
	  (lambda vals vals))
	=> (<nelist>))
  (check
      (call-with-values
	  (lambda () (cast-signature <nelist> (values 1 2 3)))
	(lambda vals vals))
    => '(1 2 3))

  ;;Untyped catch-all syntactic binding.  The producer returns a LIST-OF.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature (list-of <fixnum>) (values 1 2 3)))
	  (lambda vals vals))
	=> ((list-of <fixnum>)))
  (check
      (call-with-values
	  (lambda () (cast-signature (list-of <fixnum>) (values 1 2 3)))
	(lambda vals vals))
    => '(1 2 3))

  ;;Untyped catch-all syntactic binding.  The producer returns a LIST.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3)))
	  (lambda vals vals))
	=> ((list <fixnum> <fixnum> <fixnum>)))
  (check
      (call-with-values
	  (lambda () (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3)))
	(lambda vals vals))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; unspecified number of values, rest arguments

  ;;Untyped syntactic bindings with rest.  The producer returns an unspecified number
  ;;of unspecified values.
  ;;
  (doit (call-with-values
	    (lambda () (cast-signature <list> (values 1 2 3)))
	  (lambda (a b . vals)
	    (values a b vals)))
	=> (<top> <top> <list>))
  (check
      (call-with-values
	  (lambda () (cast-signature <list> (values 1 2 3)))
	(lambda (a b . vals)
	  (values a b vals)))
    => 1 2 '(3))

  ;;Typed syntactic bindings with rest.
  ;;
  (doit (call-with-values
	    (lambda () (values 1 2.3 "C" "D"))
	  (lambda ({a <fixnum>} {b <flonum>} . {vals (list-of <nestring>)})
	    (values a b vals)))
	=> (<fixnum> <flonum> (list-of <nestring>)))
  (check
      (call-with-values
	  (lambda () (values 1 2.3 "C" "D"))
	(lambda ({a <fixnum>} {b <flonum>} . {vals (list-of <nestring>)})
	  (values a b vals)))
    => 1 2.3 '("C" "D"))

  ;;Untyped syntactic bindings with rest.
  ;;
  (doit (call-with-values
	    (lambda () (values 1 2.3 "C" "D"))
	  (lambda (a b . vals)
	    (values a b vals)))
	=> (<positive-fixnum> <positive-flonum> (list <nestring> <nestring>)))
  (check
      (call-with-values
	  (lambda () (values 1 2.3 "C" "D"))
	(lambda (a b . vals)
	  (values a b vals)))
    => 1 2.3 '("C" "D"))

  ;;Untyped syntactic bindings with rest.  Special case of type propagation.
  ;;
  (doit (call-with-values
	    (lambda ()
	      (cast-signature (<fixnum> <flonum> . (list-of <nestring>))
			      (values 1 2.3 "C" "D")))
	  (lambda (a b . vals)
	    (values a b vals)))
	=> (<fixnum> <flonum> (list-of <nestring>)))
  (check
      (call-with-values
	  (lambda ()
	    (cast-signature (<fixnum> <flonum> . (list-of <nestring>))
			    (values 1 2.3 "C" "D")))
	(lambda (a b . vals)
	  (values a b vals)))
    => 1 2.3 '("C" "D"))

  ;;Untyped syntactic bindings with rest.  The producer returns a single value.
  ;;
  (doit (call-with-values
	    (lambda () 1)
	  (lambda (a . vals)
	    (values a vals)))
	=> (<positive-fixnum> <null>))
  (check
      (call-with-values
	  (lambda () 1)
	(lambda (a . vals)
	  (values a vals)))
    => 1 '())

  ;;Producer not returning.
  ;;
  (doit (call-with-values
	    (lambda () (error #f "wrong"))
	  (lambda (a b . rest)
	    (values a b rest)))
	=> <bottom>)
  (check
      (try
	  (call-with-values
	      (lambda () (error #f "wrong"))
	    (lambda (a b . rest)
	      (values a b rest)))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  (void))


(parametrise ((check-test-name	'lambda))

  (doit ((lambda ({_ <fixnum>} {a <fixnum>} {b <fixnum>})
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

  (doit ((lambda ({_ <fixnum>} {a <positive-fixnum>} {b <negative-fixnum>})
	   (fx+ a b))
	 1 -2)
	=> (<fixnum>))

  (doit ((lambda ({_ <fixnum>} a b)
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

;;; --------------------------------------------------------------------
;;; type propagation: automatic inference of return values's type signature

  (doit ((lambda ({a <fixnum>} {b <fixnum>})
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

  (void))


(parametrise ((check-test-name	'named-lambda))

  (doit ((named-lambda me ({_ <fixnum>} {a <fixnum>} {b <fixnum>})
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

  (doit ((named-lambda me ({_ <fixnum>} {a <positive-fixnum>} {b <negative-fixnum>})
	   (fx+ a b))
	 1 -2)
	=> (<fixnum>))

  (doit ((named-lambda me ({_ <fixnum>} a b)
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

;;; --------------------------------------------------------------------
;;; type propagation: automatic inference of return values's type signature

  (doit ((named-lambda me ({a <fixnum>} {b <fixnum>})
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

  (void))


(parametrise ((check-test-name	'case-lambda))

  (doit ((case-lambda
	   (({_ <fixnum>} {a <fixnum>} {b <fixnum>})
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <fixnum>} {a <positive-fixnum>} {b <negative-fixnum>})
	    (fx+ a b)))
	 1 -2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} a b c)
	    (fl+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <flonum>} a b c)
	    (fl+ a b))
	   (({_ <fixnum>} a b)
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} a b c)
	    (fl+ a b)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} {a <flonum>} {b <flonum>} {c <flonum>})
	    (fl+ a b)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} {a <flonum>} . {b* (list-of <flonum>)})
	    (apply fl+ a b*)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} . {fl* (list-of <flonum>)})
	    (apply fl+ fl*)))
	 1.0 2.0 3.0)
	=> (<flonum>))

;;; --------------------------------------------------------------------
;;; type propagation: automatic inference of return values's type signature

  (doit ((case-lambda
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b))
	   (({a <nestring>} {b <nestring>} {c <nestring>})
	    (string-append a b c)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b))
	   (({a <nestring>} {b <nestring>} {c <nestring>})
	    (string-append a b c)))
	 "1" "2" "3")
	=> (<nestring>))

  (void))


(parametrise ((check-test-name	'named-case-lambda))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} {a <fixnum>} {b <fixnum>})
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} {a <positive-fixnum>} {b <negative-fixnum>})
	    (fx+ a b)))
	 1 -2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} a b c)
	    (fl+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <flonum>} a b c)
	    (fl+ a b))
	   (({_ <fixnum>} a b)
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} a b c)
	    (fl+ a b)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} {a <flonum>} {b <flonum>} {c <flonum>})
	    (fl+ a b)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} {a <flonum>} . {b* (list-of <flonum>)})
	    (apply fl+ a b*)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} . {fl* (list-of <flonum>)})
	    (apply fl+ fl*)))
	 1.0 2.0 3.0)
	=> (<flonum>))

;;; --------------------------------------------------------------------
;;; type propagation: automatic inference of return values's type signature

  (doit ((named-case-lambda me
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b))
	   (({a <nestring>} {b <nestring>} {c <nestring>})
	    (string-append a b c)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b))
	   (({a <nestring>} {b <nestring>} {c <nestring>})
	    (string-append a b c)))
	 "1" "2" "3")
	=> (<nestring>))

  (void))


(parametrise ((check-test-name	'begin0))

  (doit (begin0 1 "ciao")
	=> (<positive-fixnum>))

  (doit (begin0 (read) 1 2)
	=> (<top>))

  (doit (begin0 (values "ciao" 'hey) 1 2)
	=> (<nestring> (enumeration hey)))

;;; --------------------------------------------------------------------
;;; special cases

  ;;The first expression returns zero values.
  ;;
  (doit (begin0 (values) (read) (read))
	=> ())

  ;;The first expression returns void.
  ;;
  (doit (begin0 (void) (read) (read))
	=> (<void>))

  ;;The first expression does not return.
  ;;
  (doit (begin0 (error #f "ciao") (read) (read))
	=> <bottom>)

  ;;The  first expression  returns an  unspecified number  of values,  of unspecified
  ;;type.
  ;;
  (doit (letrec ((fun (lambda ({_ . <list>}) (fun))))
	  (begin0 (fun) (read) (read)))
	=> <list>)

  ;;The first expression returns an unspecified number of values, of known type.
  ;;
  (doit (letrec ((fun (lambda ({_ . (list-of <fixnum>)}) (fun))))
	  (begin0 (fun) (read) (read)))
	=> (list-of <fixnum>))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'receive-std))

;;; no return values

  (doit (receive/std ()
	    (values)
	  123)
	=> (<positive-fixnum>))
  (check
      (receive/std ()
	  (values)
	123)
    => 123)

  (doit (receive/std ()
	    (cast-signature <list> (values))
	  123)
	=> (<positive-fixnum>))
  (check
      (receive/std ()
	  (cast-signature <list> (values))
	123)
    => 123)

  (doit (receive/std ()
	    (cast-signature (list-of <fixnum>) (values))
	  123)
	=> (<positive-fixnum>))
  (check
      (receive/std ()
	  (cast-signature (list-of <fixnum>) (values))
	123)
    => 123)

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (receive/std ()
	    (error #f "wrong")
	  123)
	=> <bottom>)
  (check
      (try
	  (receive/std ()
	      (error #f "wrong")
	    123)
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns values.
  ;;
  (type-signature-violation-for-message
   (receive/std ()
       (values 1)
     123)
   => "zero return values are expected from the producer expression")

;;; --------------------------------------------------------------------
;;; single return value

  ;;UNtyped single value ignored in body.  Type propagation.
  ;;
  (doit (receive/std (a)
	    1
	  123)
	=> (<positive-fixnum>))
  (check
      (receive/std (a)
	  1
	123)
    => 123)

  ;;UNtyped single value used in body.  Type propagation.
  ;;
  (doit (receive/std (a)
	    1
	  a)
	=> (<top>))
  (check
      (receive/std (a)
	  1
	a)
    => 1)

  ;;UNtyped single value used in body.  Type propagation.  More body forms
  ;;
  (doit (receive/std (a)
	    1
	  (+ 1 2)
	  (list a))
	=> ((list <top>)))
  (check
      (receive/std (a)
	  1
	(+ 1 2)
	(list a))
    => '(1))

  ;;Producer returns a <list>.
  ;;
  (doit (receive/std (a)
	    (cast-signature <list> 1)
	  a)
	=> (<top>))
  (check
      (receive/std (a)
	  (cast-signature <list> 1)
	a)
    => 1)

  ;;Producer returns a <nelist>.
  ;;
  (doit (receive/std (a)
	    (cast-signature <nelist> 1)
	  a)
	=> (<top>))
  (check
      (receive/std (a)
	  (cast-signature <nelist> 1)
	a)
    => 1)

  ;;Producer returns a LIST-OF.
  ;;
  (doit (receive/std (a)
	    (cast-signature (list-of <fixnum>) 1)
	  a)
	=> (<top>))
  (check
      (receive/std (a)
	  (cast-signature (list-of <fixnum>) 1)
	a)
    => 1)

  ;;Producer returns a LIST.
  ;;
  (doit (receive/std (a)
	    (cast-signature (list <fixnum>) 1)
	  a)
	=> (<top>))
  (check
      (receive/std (a)
	  (cast-signature (list <fixnum>) 1)
	a)
    => 1)

  ;;Producer returns a <list> with multiple values.  Run-time violaion.
  ;;
  (assertion-violation-for-message
   (receive/std (a)
       (cast-signature <list> (values 1 2 3))
     a)
   => "incorrect number of values returned to single value context")

  ;;Producer returns a <nelist> with multiple values.  Run-time violation.
  ;;
  (assertion-violation-for-message
   (receive/std (a)
       (cast-signature <nelist> (values 1 2 3))
     a)
   => "incorrect number of values returned to single value context")

  ;;Producer returns a LIST with multiple values.  Expand-time violation.
  ;;
  (type-signature-violation-for-message
   (receive/std (a)
       (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3))
     a)
   => "one value is expected but the producer expression is typed as returning zero, two or more values")

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (receive/std (a)
	    (error #f "wrong")
	  a)
	=> <bottom>)
  (check
      (try
	  (receive/std (a)
	      (error #f "wrong")
	    123)
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns zero values.
  ;;
  (type-signature-violation-for-message
   (receive/std (a)
       (values)
     a)
   => "one value is expected but the producer expression is typed as returning zero, two or more values")

  ;;Producer returns multiple values.
  ;;
  (type-signature-violation-for-message
   (receive/std (a)
       (values 1 2 3)
     a)
   => "one value is expected but the producer expression is typed as returning zero, two or more values")

  ;;Producer returning void.
  ;;
  (type-signature-violation-for-message
   (receive/std (a)
       (void)
     a)
   => "the producer expression is typed as returning void")

;;; --------------------------------------------------------------------
;;; fixed number of two or more mandatory arguments

  ;;Two values.  Unused bindings.  Type propagation of return values.
  ;;
  (doit (receive/std (a b)
	    (values 1 2)
	  (values "hello" "world"))
	=> (<nestring> <nestring>))
  (check
      (receive/std (a b)
	  (values 1 2)
	(values "hello" "world"))
    => "hello" "world")

  ;;Two values, UNtyped syntactic bindings.  Type propagation.
  ;;
  (doit (receive/std (a b)
	    (values 1 2.3)
	  (values a b))
	=> (<top> <top>))
  (check
      (receive/std (a b)
	  (values 1 2.3)
	(values a b))
    => 1 2.3)

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (receive/std (a b)
	    (error #f "wrong")
	  (values a b))
	=> <bottom>)
  (check
      (try
	  (receive/std (a b)
	      (error #f "wrong")
	    (values a b))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns zero values.
  ;;
  (type-signature-violation-for-message
   (receive/std (a b)
       (values)
     (list a b))
   => "two or more values are expected from the producer expression, but it returns zero values")

  ;;Producer returns one value.
  ;;
  (type-signature-violation-for-message
   (receive/std (a b)
       (values 1)
     (list a b))
   => "two or more values are expected from the producer expression, but it returns one value")

  ;;Two bindings.  Producer returns an UNtyped list of two values.
  ;;
  (doit (receive/std (a b)
	    (cast-signature <list> (values 1 2.3))
	  (values a b))
	=> (<top> <top>))
  (check
      (receive/std (a b)
	  (cast-signature <list> (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;Two bindings.  Producer returns a typed list of two values.
  ;;
  (doit (receive/std (a b)
	    (cast-signature (list-of <number>) (values 1 2.3))
	  (values a b))
	=> (<top> <top>))
  (check
      (receive/std (a b)
	  (cast-signature (list-of <number>) (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;Two bindings.  Producer returns a typed list of two values.
  ;;
  (doit (receive/std (a b)
	    (cast-signature (list-of <number>) (values 1 2.3))
	  (values a b))
	=> (<top> <top>))
  (check
      (receive/std (a b)
	  (cast-signature (list-of <number>) (values 1 2.3))
	(values a b))
    => 1 2.3)

;;; -------------------------------------------------------------------- ;
;;; unspecified number of values

  ;;Args catch-all syntactic binding.
  ;;
  (doit (receive/std vals
	    (values 1 2 3)
	  vals)
	=> (<top>))
  (check
      (receive/std vals
	  (values 1 2 3)
	vals)
    => '(1 2 3))

  ;;Args catch-all syntactic bindings.  The producer returns a single value.
  ;;
  (doit (receive/std vals
	    1
	  vals)
	=> (<top>))
  (check
      (receive/std vals
	  1
	vals)
    => '(1))

  ;;Args catch-all syntactic bindings.  The producer returns no values.
  ;;
  (doit (receive/std vals
	    (values)
	  vals)
	=> (<top>))
  (check
      (receive/std vals
	  (values)
	vals)
    => '())

  ;;Args catch-all syntactic binding.  The producer returns a LIST-OF.
  ;;
  (doit (receive/std vals
	    (cast-signature (list-of <fixnum>) (values 1 2 3))
	  vals)
	=> (<top>))
  (check
      (receive/std vals
	  (cast-signature (list-of <fixnum>) (values 1 2 3))
	vals)
    => '(1 2 3))

  ;;Args catch-all syntactic binding.  The producer returns a LIST.
  ;;
  (doit (receive/std vals
	    (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3))
	  vals)
	=> (<top>))
  (check
      (receive/std vals
	  (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3))
	vals)
    => '(1 2 3))

  ;;Args catch-all syntactic binding.  The producer returns a <nelist>.
  ;;
  (doit (receive/std vals
	    (cast-signature <nelist> (values 1 2 3))
	  vals)
	=> (<top>))
  (check
      (receive/std vals
	  (cast-signature <nelist> (values 1 2 3))
	vals)
    => '(1 2 3))

  ;;Args catch-all syntactic binding.  The producer returns a <list>.
  ;;
  (doit (receive/std vals
	    (cast-signature <list> (values 1 2 3))
	  vals)
	=> (<top>))
  (check
      (receive/std vals
	  (cast-signature <list> (values 1 2 3))
	vals)
    => '(1 2 3))

;;;

  ;;Formals with rest argument.  The producer returns a <list>.
  ;;
  (doit (receive/std (a b . vals)
	    (cast-signature <list> (values 1 2 3))
	  (values a b vals))
	=> (<top> <top> <top>))
  (check
      (receive/std (a b . vals)
	  (cast-signature <list> (values 1 2 3))
	(values a b vals))
    => 1 2 '(3))

  ;;Formals with rest argument.  The producer returns a <nelist>.
  ;;
  (doit (receive/std (a b . vals)
	    (cast-signature <nelist> (values 1 2 3))
	  (values a b vals))
	=> (<top> <top> <top>))
  (check
      (receive/std (a b . vals)
	  (cast-signature <nelist> (values 1 2 3))
	(values a b vals))
    => 1 2 '(3))

  ;;Formals with rest argument.  The producer returns a LIST-OF.
  ;;
  (doit (receive/std (a b . vals)
	    (cast-signature (list-of <fixnum>) (values 1 2 3))
	  (values a b vals))
	=> (<top> <top> <top>))
  (check
      (receive/std (a b . vals)
	  (cast-signature (list-of <fixnum>) (values 1 2 3))
	(values a b vals))
    => 1 2 '(3))

  ;;Formals with rest argument.  The producer returns a LIST.
  ;;
  (doit (receive/std (a b . vals)
	    (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3))
	  (values a b vals))
	=> (<top> <top> <top>))
  (check
      (receive/std (a b . vals)
	  (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3))
	(values a b vals))
    => 1 2 '(3))

  ;;Formals with  rest argument.   The producer  returns a  LIST with  exactly enough
  ;;values.
  ;;
  (doit (receive/std (a b . vals)
	    (cast-signature (list <fixnum> <fixnum>) (values 1 2))
	  (values a b vals))
	=> (<top> <top> <top>))
  (check
      (receive/std (a b . vals)
	  (cast-signature (list <fixnum> <fixnum>) (values 1 2))
	(values a b vals))
    => 1 2 '())

  ;;Formals with rest argument.  The producer returns a LIST with not enough values.
  ;;
  (type-signature-violation-for-message
   (receive/std (a b . vals)
       (cast-signature (list <fixnum>) 1)
     (values a b vals))
   => "mismatching number of arguments in type signatures")

  ;;Formals with rest argument.
  ;;
  (doit (receive/std (a b . vals)
	    (values 1 2.3 "C" "D")
	  (values a b vals))
	=> (<top> <top> <top>))
  (check
      (receive/std (a b . vals)
	  (values 1 2.3 "C" "D")
	(values a b vals))
    => 1 2.3 '("C" "D"))

  ;;Formals with rest argument.
  ;;
  (doit (receive/std (a b . vals)
	    (cast-signature (<fixnum> <flonum> . (list-of <nestring>))
			    (values 1 2.3 "C" "D"))
	  (values a b vals))
     => (<top> <top> <top>))
  (check
      (receive/std (a b . vals)
	  (cast-signature (<fixnum> <flonum> . (list-of <nestring>))
			  (values 1 2.3 "C" "D"))
	(values a b vals))
    => 1 2.3 '("C" "D"))

  ;;Formals with rest argument.  The producer returns a single value.
  ;;
  (doit (receive/std (a . vals)
	    1
	  (values a vals))
	=> (<top> <top>))
  (check
      (receive/std (a . vals)
	  1
	(values a vals))
    => 1 '())

  ;;Formals with rest argument.  Producer not returning.
  ;;
  (doit (receive/std (a b . rest)
	    (error #f "wrong")
	  (values a b rest))
	=> <bottom>)
  (check
      (try
	  (receive/std (a b . rest)
	      (error #f "wrong")
	    (values a b rest))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  (void))


(parametrise ((check-test-name	'receive-checked))

;;; no return values

  (doit (receive ()
	    (values)
	  123)
	=> (<positive-fixnum>))
  (check
      (receive ()
	  (values)
	123)
    => 123)

  (doit (receive ()
	    (cast-signature <list> (values))
	  123)
	=> (<positive-fixnum>))
  (check
      (receive ()
	  (cast-signature <list> (values))
	123)
    => 123)

;;; special cases

  (type-signature-violation-for-message
   (receive ()
       (cast-signature <nelist> (values 1))
     123)
   => "zero return values are expected from the producer expression")

  (type-signature-violation-for-message
   (receive ()
       (cast-signature (list <fixnum> <fixnum>) (values 1 2))
     123)
   => "zero return values are expected from the producer expression")

  (doit (receive ()
	    (cast-signature (list-of <fixnum>) (values))
	  123)
	=> (<positive-fixnum>))
  (check
      (receive ()
	  (cast-signature (list-of <fixnum>) (values))
	123)
    => 123)

  (doit (receive ()
	    (cast-signature (list-of <fixnum>) (values 1 2))
	  123)
	=> (<positive-fixnum>))
  (assertion-violation-for-message
      (receive ()
	  (cast-signature (list-of <fixnum>) (values 1 2))
	123)
    => "incorrect number of arguments")

  ;;Producer not returning.
  ;;
  (doit (receive ()
	    (error #f "wrong")
	  123)
	=> <bottom>)
  (check
      (try
	  (receive ()
	      (error #f "wrong")
	    123)
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns values.
  ;;
  (type-signature-violation-for-message
   (receive ()
       (values 1)
     123)
   => "zero return values are expected from the producer expression")

;;; --------------------------------------------------------------------
;;; single return value

  ;;Typed single argument, expand-time validation.
  ;;
  (doit (receive ({a <fixnum>})
	    1
	  a)
	=> (<fixnum>))
  (check
      (receive ({a <fixnum>})
	  1
	a)
    => 1)

  ;;Typed single argument, run-time validation.
  ;;
  (doit (receive ({a <fixnum>})
	    (cast-signature (<top>) 1)
	  a)
	=> (<fixnum>))
  (check
      (receive ({a <fixnum>})
	  (cast-signature (<top>) 1)
	a)
    => 1)

  ;;UNtyped single value ignored in body.  Type propagation.
  ;;
  (doit (receive (a)
	    1
	  123)
	=> (<positive-fixnum>))
  (check
      (receive (a)
	  1
	123)
    => 123)

  ;;UNtyped single value used in body.  Type propagation.
  ;;
  (doit (receive (a)
	    1
	  a)
	=> (<positive-fixnum>))
  (check
      (receive (a)
	  1
	a)
    => 1)

  ;;UNtyped single value used in body.  Type propagation.  More body forms
  ;;
  (doit (receive (a)
	    1
	  (+ 1 2)
	  (list a))
	=> ((list <positive-fixnum>)))
  (check
      (receive (a)
	  1
	(+ 1 2)
	(list a))
    => '(1))

  ;;Producer returns a LIST-OF.  Untyped syntactic binding.
  ;;
  (doit (receive (a)
	    (cast-signature (list-of <fixnum>) 1)
	  a)
	=> (<fixnum>))
  (check
      (receive (a)
	  (cast-signature (list-of <fixnum>) 1)
	a)
    => 1)

  ;;Producer returns a LIST-OF.  Typed syntactic binding.
  ;;
  (doit (receive ({a <fixnum>})
	    (cast-signature (list-of <fixnum>) 1)
	  a)
	=> (<fixnum>))
  (check
      (receive ({a <fixnum>})
	  (cast-signature (list-of <fixnum>) 1)
	a)
    => 1)

  ;;Producer returns a LIST-OF with too many values.
  ;;
  (doit (receive (a)
	    (cast-signature (list-of <fixnum>) (values 1 2))
	  a)
	=> (<fixnum>))
  (assertion-violation-for-message
   (receive (a)
       (cast-signature (list-of <fixnum>) (values 1 2))
     a)
   => "incorrect number of values returned to single value context")

;;;

  ;;Producer returns a LIST.  Untyped syntactic binding.
  ;;
  (doit (receive (a)
	    (cast-signature (list <fixnum>) 1)
	  a)
	=> (<fixnum>))
  (check
      (receive (a)
	  (cast-signature (list <fixnum>) 1)
	a)
    => 1)

  ;;Producer returns a LIST.  Typed syntactic binding.
  ;;
  (doit (receive ({a <fixnum>})
	    (cast-signature (list <positive-fixnum>) 1)
	  a)
	=> (<fixnum>))
  (check
      (receive ({a <fixnum>})
	  (cast-signature (list <positive-fixnum>) 1)
	a)
    => 1)

  ;;Producer returns a LIST with too many values.
  ;;
  (type-signature-violation-for-message
   (receive (a)
       (cast-signature (list <fixnum> <flonum>) (values 1 2.3))
     a)
   => "mismatching number of arguments in type signatures")

  ;;Producer returns a <nelist>.  Untyped syntactic binding.
  ;;
  (doit (receive (a)
	    (cast-signature <nelist> 1)
	  a)
	=> (<top>))
  (check
      (receive (a)
	  (cast-signature <nelist> 1)
	a)
    => 1)

  ;;Producer returns a <nelist> with too many values.
  ;;
  (doit (receive (a)
	    (cast-signature <nelist> (values 1 2))
	  a)
	=> (<top>))
  (assertion-violation-for-message
   (receive (a)
       (cast-signature <nelist> (values 1 2))
     a)
   => "incorrect number of values returned to single value context")

;;;

  ;;Producer returns a "<list>".  Untyped syntactic binding.
  ;;
  (doit (receive (a)
	    (cast-signature <list> 1)
	  a)
	=> (<top>))
  (check
      (receive (a)
	  (cast-signature <list> 1)
	a)
    => 1)

  ;;Producer returns a "<list>".  Typed syntactic binding.
  ;;
  (doit (receive ({a <fixnum>})
	    (cast-signature <list> 1)
	  a)
	=> (<fixnum>))
  (check
      (receive ({a <fixnum>})
	  (cast-signature <list> 1)
	a)
    => 1)

  ;;Producer returns a <list> with multiple values.
  ;;
  (doit (receive ({a <fixnum>})
	    (cast-signature <list> (values 1 2 3))
	  a)
	=> (<fixnum>))
  (assertion-violation-for-message
   (receive ({a <fixnum>})
       (cast-signature <list> (values 1 2 3))
     a)
   => "incorrect number of values returned to single value context")

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (receive (a)
	    (error #f "wrong")
	  a)
	=> <bottom>)
  (check
      (try
	  (receive (a)
	      (error #f "wrong")
	    123)
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns zero values.
  ;;
  (type-signature-violation-for-message
   (receive (a)
       (values)
     a)
   => "one value is expected but the producer expression is typed as returning zero, two or more values")

  ;;Producer returns multiple values.
  ;;
  (type-signature-violation-for-message
   (receive (a)
       (values 1 2 3)
     a)
   => "one value is expected but the producer expression is typed as returning zero, two or more values")

  ;;Producer returning void.
  ;;
  (type-signature-violation-for-message
   (receive (a)
       (void)
     a)
   => "the producer expression is typed as returning void")

;;; --------------------------------------------------------------------
;;; fixed number of two or more mandatory arguments

  ;;Two values, typed syntactic bindings.
  ;;
  (doit (receive ({a <positive-fixnum>} {b <nestring>})
	    (values 123 "ciao")
	  (values a b))
	=> (<positive-fixnum> <nestring>))
  (check
      (receive ({a <positive-fixnum>} {b <nestring>})
	  (values 123 "ciao")
	(values a b))
    => 123 "ciao")

  ;;Two values, typed syntactic bindings.  Run-time validation.
  ;;
  (doit (receive ({a <fixnum>} {b <flonum>})
	    (values (cast-signature (<top>) 1)
		    (cast-signature (<top>) 2.3))
	  (values a b))
	=> (<fixnum> <flonum>))
  (check
      (receive ({a <fixnum>} {b <flonum>})
	  (values (cast-signature (<top>) 1)
		  (cast-signature (<top>) 2.3))
	(values a b))
    => 1 2.3)

  ;;Two values, UNtyped syntactic bindings.  Unused bindings.  Type propagation.
  ;;
  (doit (receive (a b)
	    (values 1 2)
	  (values "hello" "world"))
	=> (<nestring> <nestring>))
  (check
      (receive (a b)
	  (values 1 2)
	(values "hello" "world"))
    => "hello" "world")

  ;;Two values, UNtyped syntactic bindings.  Type propagation.
  ;;
  (doit (receive (a b)
	    (values 1 2.3)
	  (values a b))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (receive (a b)
	  (values 1 2.3)
	(values a b))
    => 1 2.3)

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (receive (a b)
	    (error #f "wrong")
	  (values a b))
	=> <bottom>)
  (check
      (try
	  (receive (a b)
	      (error #f "wrong")
	    (values a b))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns zero values.
  ;;
  (type-signature-violation-for-message
   (receive (a b)
       (values)
     (list a b))
   => "two or more values are expected from the producer expression, but it returns zero values")

  ;;Producer returns one value.
  ;;
  (type-signature-violation-for-message
   (receive (a b)
       (values 1)
     (list a b))
   => "two or more values are expected from the producer expression, but it returns one value")

;;;

  ;;Two Untyped bindings.  Producer returns an UNtyped list of two values.
  ;;
  (doit (receive (a b)
	    (cast-signature <list> (values 1 2.3))
	  (values a b))
	=> (<top> <top>))
  (check
      (receive (a b)
	  (cast-signature <list> (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;Two typed  bindings.  Producer returns an  UNtyped list of two  values.  Run-time
  ;;validation.
  ;;
  (doit (receive ({a <fixnum>} {b <flonum>})
	    (cast-signature <list> (values 1 2.3))
	  (values a b))
	=> (<fixnum> <flonum>))
  (check
      (receive ({a <fixnum>} {b <flonum>})
	  (cast-signature <list> (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;The producer returns a <list> with too many values.
  ;;
  (doit (receive (a b)
	    (cast-signature <list> (values 1 2 3))
	  (values a b))
	=> (<top> <top>))
  (assertion-violation-for-message
   (receive (a b)
       (cast-signature <list> (values 1 2 3))
     (values a b))
   => "incorrect number of arguments")

  ;;The producer returns a <list> with too few values.
  ;;
  (doit (receive (a b)
	    (cast-signature <list> (values 1))
	  (values a b))
	=> (<top> <top>))
  (assertion-violation-for-message
   (receive (a b)
       (cast-signature <list> (values 1))
     (values a b))
   => "incorrect number of arguments")

;;;

  ;;Two Untyped bindings.  Producer returns a <nelist>.
  ;;
  (doit (receive (a b)
	    (cast-signature <nelist> (values 1 2.3))
	  (values a b))
	=> (<top> <top>))
  (check
      (receive (a b)
	  (cast-signature <nelist> (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;Two typed bindings.  Producer returns a <nelist>.  Run-time validation.
  ;;
  (doit (receive ({a <fixnum>} {b <flonum>})
	    (cast-signature <nelist> (values 1 2.3))
	  (values a b))
	=> (<fixnum> <flonum>))
  (check
      (receive ({a <fixnum>} {b <flonum>})
	  (cast-signature <nelist> (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;The producer returns a <nelist> with too many values.
  ;;
  (doit (receive (a b)
	    (cast-signature <nelist> (values 1 2 3))
	  (values a b))
	=> (<top> <top>))
  (assertion-violation-for-message
   (receive (a b)
       (cast-signature <nelist> (values 1 2 3))
     (values a b))
   => "incorrect number of arguments")

  ;;The producer returns a <nelist> with too few values.
  ;;
  (doit (receive (a b)
	    (cast-signature <nelist> (values 1))
	  (values a b))
	=> (<top> <top>))
  (assertion-violation-for-message
   (receive (a b)
       (cast-signature <nelist> (values 1))
     (values a b))
   => "incorrect number of arguments")

;;;

  ;;Two Untyped bindings.  Producer returns a typed list of two values.
  ;;
  (doit (receive (a b)
	    (cast-signature (list-of <number>) (values 1 2.3))
	  (values a b))
	=> (<number> <number>))
  (check
      (receive (a b)
	  (cast-signature (list-of <number>) (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;Two  typed bindings.   Producer returns  a typed  list of  two values.   Run-time
  ;;validation.
  ;;
  (doit (receive ({a <fixnum>} {b <flonum>})
	    (cast-signature (list-of <number>) (values 1 2.3))
	  (values a b))
	=> (<fixnum> <flonum>))
  (check
      (receive ({a <fixnum>} {b <flonum>})
	  (cast-signature (list-of <number>) (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;The producer returns a LIST-OF with too many values.
  ;;
  (doit (receive (a b)
	    (cast-signature (list-of <fixnum>) (values 1 2 3))
	  (values a b))
	=> (<fixnum> <fixnum>))
  (assertion-violation-for-message
   (receive (a b)
       (cast-signature (list-of <fixnum>) (values 1 2 3))
     (values a b))
   => "incorrect number of arguments")

  ;;The producer returns a LIST-OF with too few values.
  ;;
  (doit (receive (a b)
	    (cast-signature (list-of <fixnum>) (values 1))
	  (values a b))
	=> (<fixnum> <fixnum>))
  (assertion-violation-for-message
   (receive (a b)
       (cast-signature (list-of <fixnum>) (values 1))
     (values a b))
   => "incorrect number of arguments")

;;;

  ;;Two Untyped bindings.  Producer returns a typed list of two values.
  ;;
  (doit (receive (a b)
	    (cast-signature (list <number> <number>) (values 1 2.3))
	  (values a b))
	=> (<number> <number>))
  (check
      (receive (a b)
	  (cast-signature (list <number> <number>) (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;Two  typed bindings.   Producer returns  a typed  list of  two values.   Run-time
  ;;validation.
  ;;
  (doit (receive ({a <fixnum>} {b <flonum>})
	    (cast-signature (list <number> <number>) (values 1 2.3))
	  (values a b))
	=> (<fixnum> <flonum>))
  (check
      (receive ({a <fixnum>} {b <flonum>})
	  (cast-signature (list <number> <number>) (values 1 2.3))
	(values a b))
    => 1 2.3)

  ;;The producer returns a LIST with too many values.
  ;;
  (type-signature-violation-for-message
   (receive (a b)
       (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3))
     (values a b))
   => "mismatching number of arguments in type signatures")

  ;;The producer returns a LIST with too few values.
  ;;
  (type-signature-violation-for-message
   (receive (a b)
       (cast-signature (list <fixnum>) (values 1))
     (values a b))
   => "mismatching number of arguments in type signatures")

;;; --------------------------------------------------------------------
;;; unspecified number of values, args arguments

  ;;Typed catch-all syntactic binding.
  ;;
  (doit (receive {vals (list-of <fixnum>)}
	    (values 1 2 3)
	  vals)
	=> ((list-of <fixnum>)))
  (check
      (receive {vals (list-of <fixnum>)}
	  (values 1 2 3)
	vals)
    => '(1 2 3))

  ;;Untyped catch-all syntactic binding.
  ;;
  (doit (receive vals
	    (values 1 2 3)
	  vals)
	=> ((list <positive-fixnum> <positive-fixnum> <positive-fixnum>)))
  (check
      (receive vals
	  (values 1 2 3)
	vals)
    => '(1 2 3))

  ;;Untyped catch-all syntactic bindings.  The producer returns a single value.
  ;;
  (doit (receive vals
	    1
	  vals)
	=> ((list <positive-fixnum>)))
  (check
      (receive vals
	  1
	vals)
    => '(1))

;;;

  ;;Untyped catch-all syntactic binding.  The  producer returns an unspecified number
  ;;of unspecified values.
  ;;
  (doit (receive vals
	    (cast-signature <list> (values 1 2 3))
	  vals)
	=> (<list>))
  (check
      (receive vals
	  (cast-signature <list> (values 1 2 3))
	vals)
    => '(1 2 3))

  ;;Untyped catch-all syntactic binding.  The producer returns a <nelist>.
  ;;
  (doit (receive vals
	    (cast-signature <nelist> (values 1 2 3))
	  vals)
	=> (<nelist>))
  (check
      (receive vals
	  (cast-signature <nelist> (values 1 2 3))
	vals)
    => '(1 2 3))

  ;;Untyped catch-all syntactic binding.  The producer returns a LIST-OF.
  ;;
  (doit (receive vals
	    (cast-signature (list-of <fixnum>) (values 1 2 3))
	  vals)
	=> ((list-of <fixnum>)))
  (check
      (receive vals
	  (cast-signature (list-of <fixnum>) (values 1 2 3))
	vals)
    => '(1 2 3))

  ;;Untyped catch-all syntactic binding.  The producer returns a LIST.
  ;;
  (doit (receive vals
	    (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3))
	  vals)
	=> ((list <fixnum> <fixnum> <fixnum>)))
  (check
      (receive vals
	  (cast-signature (list <fixnum> <fixnum> <fixnum>) (values 1 2 3))
	vals)
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; unspecified number of values, rest arguments

  ;;Untyped syntactic bindings with rest.  The producer returns an unspecified number
  ;;of unspecified values.
  ;;
  (doit (receive (a b . vals)
	    (cast-signature <list> (values 1 2 3))
	  (values a b vals))
	=> (<top> <top> <list>))
  (check
      (receive (a b . vals)
	  (cast-signature <list> (values 1 2 3))
	(values a b vals))
    => 1 2 '(3))

  ;;Typed syntactic bindings with rest.
  ;;
  (doit (receive ({a <fixnum>} {b <flonum>} . {vals (list-of <nestring>)})
	    (values 1 2.3 "C" "D")
	  (values a b vals))
	=> (<fixnum> <flonum> (list-of <nestring>)))
  (check
      (receive ({a <fixnum>} {b <flonum>} . {vals (list-of <nestring>)})
	  (values 1 2.3 "C" "D")
	(values a b vals))
    => 1 2.3 '("C" "D"))

  ;;Untyped syntactic bindings with rest.
  ;;
  (doit (receive (a b . vals)
	    (values 1 2.3 "C" "D")
	  (values a b vals))
	=> (<positive-fixnum> <positive-flonum> (list <nestring> <nestring>)))
  (check
      (receive (a b . vals)
	  (values 1 2.3 "C" "D")
	(values a b vals))
    => 1 2.3 '("C" "D"))

  ;;Untyped syntactic bindings with rest.  Special case of type propagation.
  ;;
  (doit (receive (a b . vals)
	    (cast-signature (<fixnum> <flonum> . (list-of <nestring>))
			    (values 1 2.3 "C" "D"))
	  (values a b vals))
	=> (<fixnum> <flonum> (list-of <nestring>)))
  (check
      (receive (a b . vals)
	  (cast-signature (<fixnum> <flonum> . (list-of <nestring>))
			  (values 1 2.3 "C" "D"))
	(values a b vals))
    => 1 2.3 '("C" "D"))

  ;;Untyped syntactic bindings with rest.  The producer returns a single value.
  ;;
  (doit (receive (a . vals)
	    1
	  (values a vals))
	=> (<positive-fixnum> <null>))
  (check
      (receive (a . vals)
	  1
	(values a vals))
    => 1 '())

  ;;Producer not returning.
  ;;
  (doit (receive (a b . rest)
	    (error #f "wrong")
	  (values a b rest))
	=> <bottom>)
  (check
      (try
	  (receive (a b . rest)
	      (error #f "wrong")
	    (values a b rest))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  (void))


(parametrise ((check-test-name	'receive-and-return))

  (define (one)
    (add-result 'one))

  (case-define two
    (()
     (add-result 'two))
    (args
     (add-result (cons 'two args))))

;;; --------------------------------------------------------------------
;;; no return values

  (doit (receive-and-return ()
	    (values)
	  (one))
	=> ())
  (check
      (with-result
	(call-with-values
	    (lambda ()
	      (receive-and-return ()
		  (values)
		(one)))
	  (lambda args
	    (two)
	    args)))
    => '(() (one two)))

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (receive-and-return ()
	    (error #f "wrong")
	  (one)
	  (two))
	=> <bottom>)
  (check
      (try
	  (receive-and-return ()
	      (error #f "wrong")
	    (one)
	    (two))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns values.
  ;;
  (type-signature-violation-for-message
   (receive-and-return ()
       1
     (one)
     (two))
   => "zero return values are expected from the producer expression")

;;; --------------------------------------------------------------------
;;; single return value

  ;;Typed single argument, expand-time validation.
  ;;
  (doit (receive-and-return ({a <fixnum>})
	    1
	  (one)
	  (two a))
	=> (<fixnum>))
  (check
      (with-result
	(receive-and-return ({a <fixnum>})
	    1
	  (one)
	  (two a)))
    => '(1 (one (two 1))))

  ;;Typed single argument, run-time validation.
  ;;
  (doit (receive-and-return ({a <fixnum>})
	    (cast-signature (<top>) 1)
	  (one)
	  (two a))
	=> (<fixnum>))
  (check
      (with-result
	(receive-and-return ({a <fixnum>})
	    (cast-signature (<top>) 1)
	  (one)
	  (two a)))
    => '(1 (one (two 1))))

  ;;UNtyped single value ignored in body.  Type propagation.
  ;;
  (doit (receive-and-return (a)
	    1
	  (one)
	  (two a))
	=> (<positive-fixnum>))
  (check
      (with-result
	(receive-and-return (a)
	    1
	  (one)
	  (two a)))
    => '(1 (one (two 1))))

  ;;UNtyped single value used in body.  Type propagation.
  ;;
  (doit (receive-and-return (a)
	    1
	  (two a))
	=> (<positive-fixnum>))
  (check
      (with-result
	(receive-and-return (a)
	    1
	  (two a)))
    => '(1 ((two 1))))

  ;;UNtyped single value used in body.  Type propagation.  More body forms
  ;;
  (doit (receive-and-return (a)
	    1
	  (one)
	  (two a))
	=> (<positive-fixnum>))
  (check
      (with-result
	(receive-and-return (a)
	    1
	  (one)
	  (two a)))
    => '(1 (one (two 1))))

  ;;Producer returns a LIST-OF.  Untyped syntactic binding.
  ;;
  (doit (receive-and-return (a)
	    (cast-signature (list-of <fixnum>) 1)
	  (one)
	  (two a))
	=> (<fixnum>))
  (check
      (with-result
	(receive-and-return (a)
	    (cast-signature (list-of <fixnum>) 1)
	  (one)
	  (two a)))
    => '(1 (one (two 1))))

  ;;Producer returns a LIST-OF.  Typed syntactic binding.
  ;;
  (doit (receive-and-return ({a <fixnum>})
	    (cast-signature (list-of <fixnum>) 1)
	  (one)
	  (two a))
	=> (<fixnum>))
  (check
      (with-result
	(receive-and-return ({a <fixnum>})
	    (cast-signature (list-of <fixnum>) 1)
	  (one)
	  (two a)))
    => '(1 (one (two 1))))

  ;;Producer returns a LIST-OF with multiple values.
  ;;
  (assertion-violation-for-message
   (receive-and-return ({a <fixnum>})
       (cast-signature <list> (values 1 2 3))
     (one)
     (two a))
   => "incorrect number of values returned to single value context")

  ;;Producer returns a "<list>".  Untyped syntactic binding.
  ;;
  (doit (receive-and-return (a)
	    (cast-signature <list> 1)
	  (one)
	  (two a))
	=> (<top>))
  (check
      (with-result
	(receive-and-return (a)
	    (cast-signature <list> 1)
	  (one)
	  (two a)))
    => '(1 (one (two 1))))

  ;;Producer returns a "<list>".  Typed syntactic binding.
  ;;
  (doit (receive-and-return ({a <fixnum>})
	    (cast-signature <list> 1)
	  (one)
	  (two a))
	=> (<fixnum>))
  (check
      (with-result
	(receive-and-return ({a <fixnum>})
	    (cast-signature <list> 1)
	  (one)
	  (two a)))
    => '(1 (one (two 1))))

  ;;Producer returns a "<list>" with multiple values.
  ;;
  (check
      (try
	  (receive-and-return ({a <fixnum>})
	      (cast-signature <list> (values 1 2 3))
	    (one)
	    (two a))
	(catch E
	  ((&assertion)
	   (condition-message E))
	  (else E)))
    => "incorrect number of values returned to single value context")

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (receive-and-return (a)
	    (error #f "wrong")
	  (one)
	  (two a))
	=> <bottom>)
  (check
      (try
	  (receive-and-return (a)
	      (error #f "wrong")
	    (one)
	    (two a))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns zero values.
  ;;
  (type-signature-violation-for-message
   (receive-and-return (a)
       (values)
     (one)
     (two a))
   => "one value is expected but the producer expression is typed as returning zero, two or more values")

  ;;Producer returns multiple values.
  ;;
  (type-signature-violation-for-message
   (receive-and-return (a)
       (values 1 2 3)
     (one)
     (two a))
   => "one value is expected but the producer expression is typed as returning zero, two or more values")

  ;;Producer returning void.
  ;;
  (type-signature-violation-for-message
   (receive-and-return (a)
       (void)
     (one)
     (two a))
   => "the producer expression is typed as returning void")

;;; --------------------------------------------------------------------
;;; fixed number of two or more mandatory arguments

  ;;Two values, typed syntactic bindings.
  ;;
  (doit (receive-and-return ({a <positive-fixnum>} {b <nestring>})
	    (values 123 "ciao")
	  (one)
	  (two a b))
	=> (<positive-fixnum> <nestring>))
  (check
      (with-result
	(receive-and-return ({a <positive-fixnum>} {b <nestring>})
	    (values 123 "ciao")
	  (one)
	  (two a b)))
    => '(123 "ciao" (one (two 123 "ciao"))))

  ;;Two values, typed syntactic bindings.  Run-time validation.
  ;;
  (doit (receive-and-return ({a <fixnum>} {b <flonum>})
	    (values (cast-signature (<top>) 1)
		    (cast-signature (<top>) 2.3))
	  (one)
	  (two a b))
	=> (<fixnum> <flonum>))
  (check
      (with-result
	(receive-and-return ({a <fixnum>} {b <flonum>})
	    (values (cast-signature (<top>) 1)
		    (cast-signature (<top>) 2.3))
	  (one)
	  (two a b)))
    => '(1 2.3 (one (two 1 2.3))))

  ;;Two values, UNtyped syntactic bindings.  Unused bindings.  Type propagation.
  ;;
  (doit (receive-and-return (a b)
	    (values 1 2)
	  (one)
	  (two "hello" "world"))
	=> (<positive-fixnum> <positive-fixnum>))
  (check
      (with-result
	(receive-and-return (a b)
	    (values 1 2)
	  (one)
	  (two "hello" "world")))
    => '(1 2 (one (two "hello" "world"))))

  ;;Two values, UNtyped syntactic bindings.  Type propagation.
  ;;
  (doit (receive-and-return (a b)
	    (values 1 2.3)
	  (one)
	  (two a b))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (with-result
	(receive-and-return (a b)
	    (values 1 2.3)
	  (one)
	  (two a b)))
    => '(1 2.3 (one (two 1 2.3))))

;;; special cases

  ;;Producer not returning.
  ;;
  (doit (receive-and-return (a b)
	    (error #f "wrong")
	  (one)
	  (two a b))
	=> <bottom>)
  (check
      (try
	  (receive-and-return (a b)
	      (error #f "wrong")
	    (one)
	    (two a b))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  ;;Producer returns zero values.
  ;;
  (type-signature-violation-for-message
   (receive-and-return (a b)
       (values)
     (one)
     (two a b))
   => "two or more values are expected from the producer expression, but it returns zero values")

  ;;Producer returns one value.
  ;;
  (type-signature-violation-for-message
   (receive-and-return (a b)
       (values 1)
     (one)
     (two a b))
   => "two or more values are expected from the producer expression, but it returns one value")

  ;;Two Untyped bindings.  Producer returns an UNtyped list of two values.
  ;;
  (doit (receive-and-return (a b)
	    (cast-signature <list> (values 1 2.3))
	  (one)
	  (two a b))
	=> (<top> <top>))
  (check
      (with-result
	(receive-and-return (a b)
	    (cast-signature <list> (values 1 2.3))
	  (one)
	  (two a b)))
    => '(1 2.3 (one (two 1 2.3))))

  ;;Two typed  bindings.  Producer returns an  UNtyped list of two  values.  Run-time
  ;;validation.
  ;;
  (doit (receive-and-return ({a <fixnum>} {b <flonum>})
	    (cast-signature <list> (values 1 2.3))
	  (one)
	  (two a b))
	=> (<fixnum> <flonum>))
  (check
      (with-result
	(receive-and-return ({a <fixnum>} {b <flonum>})
	    (cast-signature <list> (values 1 2.3))
	  (one)
	  (two a b)))
    => '(1 2.3 (one (two 1 2.3))))

  ;;Two Untyped bindings.  Producer returns a typed list of two values.
  ;;
  (doit (receive-and-return (a b)
	    (cast-signature (list-of <number>) (values 1 2.3))
	  (one)
	  (two a b))
	=> (<number> <number>))
  (check
      (with-result
	(receive-and-return (a b)
	    (cast-signature (list-of <number>) (values 1 2.3))
	  (one)
	  (two a b)))
    => '(1 2.3 (one (two 1 2.3))))

  ;;Two  typed bindings.   Producer returns  a typed  list of  two values.   Run-time
  ;;validation.
  ;;
  (doit (receive-and-return ({a <fixnum>} {b <flonum>})
	    (cast-signature (list-of <number>) (values 1 2.3))
	  (one)
	  (two a b))
	=> (<fixnum> <flonum>))
  (check
      (with-result
	(receive-and-return ({a <fixnum>} {b <flonum>})
	    (cast-signature (list-of <number>) (values 1 2.3))
	  (one)
	  (two a b)))
    => '(1 2.3 (one (two 1 2.3))))

;;; -------------------------------------------------------------------- ;
;;; unspecified number of values

  ;;Typed catch-all syntactic binding.
  ;;
  (doit (receive-and-return {vals (list-of <fixnum>)}
	    (values 1 2 3)
	  (one)
	  (two))
	=> (list-of <fixnum>))
  (check
      (with-result
	(receive-and-return {vals (list-of <fixnum>)}
	    (values 1 2 3)
	  (one)
	  (two vals)))
    => '(1 2 3 (one (two (1 2 3)))))

  ;;Untyped catch-all syntactic binding.
  ;;
  (doit (receive-and-return vals
	    (values 1 2 3)
	  (one)
	  (two))
	=> (list <positive-fixnum> <positive-fixnum> <positive-fixnum>))
  (check
      (with-result
	(receive-and-return vals
	    (values 1 2 3)
	  (one)
	  (two vals)))
    => '(1 2 3 (one (two (1 2 3)))))

  ;;Untyped catch-all syntactic bindings.  The producer returns a single value.
  ;;
  (doit (receive-and-return vals
	    1
	  (one)
	  (two))
	=> (list <positive-fixnum>))
  (check
      (with-result
	(receive-and-return vals
	    1
	  (one)
	  (two vals)))
    => '(1 (one (two (1)))))

  ;;Untyped catch-all syntactic binding.  Special case of type propagation.
  ;;
  (doit (receive-and-return vals
	    (cast-signature (list-of <fixnum>) (values 1 2 3))
	  (one)
	  (two))
	=> (list-of <fixnum>))
  (check
      (with-result
	(receive-and-return vals
	    (cast-signature (list-of <fixnum>) (values 1 2 3))
	  (one)
	  (two vals)))
    => '(1 2 3 (one (two (1 2 3)))))

  ;;Untyped catch-all syntactic binding.  The  producer returns an unspecified number
  ;;of unspecified values.
  ;;
  (doit (receive-and-return vals
	    (cast-signature <list> (values 1 2 3))
	  (one)
	  (two))
	=> <list>)
  (check
      (with-result
	(receive-and-return vals
	    (cast-signature <list> (values 1 2 3))
	  (one)
	  (two vals)))
    => '(1 2 3 (one (two (1 2 3)))))

  ;;Untyped syntactic bindings with rest.  The producer returns an unspecified number
  ;;of unspecified values.
  ;;
  (doit (receive-and-return (a b . vals)
	    (cast-signature <list> (values 1 2 3))
	  (one)
	  (two a b vals))
	=> (<top> <top> . <list>))
  (check
      (with-result
	(receive-and-return (a b . vals)
	    (cast-signature <list> (values 1 2 3))
	  (one)
	  (two a b vals)))
    => '(1 2 3 (one (two 1 2 (3)))))

  ;;Typed syntactic bindings with rest.
  ;;
  (doit (receive-and-return ({a <fixnum>} {b <flonum>} . {vals (list-of <nestring>)})
	    (values 1 2.3 "C" "D")
	  (one)
	  (two))
	=> (<fixnum> <flonum> . (list-of <nestring>)))
  (check
      (with-result
	(receive-and-return ({a <fixnum>} {b <flonum>} . {vals (list-of <nestring>)})
	    (values 1 2.3 "C" "D")
	  (one)
	  (two a b vals)))
    => '(1 2.3 "C" "D" (one (two 1 2.3 ("C" "D")))))

  ;;Untyped syntactic bindings with rest.
  ;;
  (doit (receive-and-return (a b . vals)
	    (values 1 2.3 "C" "D")
	  (one)
	  (two a b vals))
	=> (<positive-fixnum> <positive-flonum> . (list <nestring> <nestring>)))
  (check
      (with-result
	(receive-and-return (a b . vals)
	    (values 1 2.3 "C" "D")
	  (one)
	  (two a b vals)))
    => '(1 2.3 "C" "D" (one (two 1 2.3 ("C" "D")))))

  ;;Untyped syntactic bindings with rest.  Special case of type propagation.
  ;;
  (doit (receive-and-return (a b . vals)
	    (cast-signature (<fixnum> <flonum> . (list-of <nestring>))
			    (values 1 2.3 "C" "D"))
	  (one)
	  (two a b vals))
	=> (<fixnum> <flonum> . (list-of <nestring>)))
  (check
      (with-result
	(receive-and-return (a b . vals)
	    (cast-signature (<fixnum> <flonum> . (list-of <nestring>))
			    (values 1 2.3 "C" "D"))
	  (one)
	  (two a b vals)))
    => '(1 2.3 "C" "D" (one (two 1 2.3 ("C" "D")))))

  ;;Untyped syntactic bindings with rest.  The producer returns a single value.
  ;;
  (doit (receive-and-return (a . vals)
	    1
	  (one)
	  (two a vals))
	=> (<positive-fixnum> . <null>))
  (check
      (with-result
	(receive-and-return (a . vals)
	    1
	  (one)
	  (two a vals)))
    => '(1 (one (two 1 ()))))

  ;;Producer not returning.
  ;;
  (doit (receive-and-return (a b . rest)
	    (error #f "wrong")
	  (one)
	  (two a b rest))
	=> <bottom>)
  (check
      (try
	  (receive-and-return (a b . rest)
	      (error #f "wrong")
	    (one)
	    (two a b rest))
	(catch E
	  ((&error)
	   (condition-message E))
	  (else E)))
    => "wrong")

  (void))


(parametrise ((check-test-name	'let-values-std))

  (define (one)
    (add-result 'one))

  (case-define two
    (()
     (add-result 'two))
    (args
     (add-result (cons 'two args))))

;;; --------------------------------------------------------------------
;;; no bindings, no return values

  (doit (let-values/std ()
	  (one)
	  (two)
	  (values))
	=> ())
  (check
      (with-result
	(let-values/std ()
	  (one)
	  (two)
	  (values)))
    => '((one two)))

;;; --------------------------------------------------------------------
;;; empty formals

  (doit (let-values/std ((() (values)))
	  (one)
	  (two)
	  (values))
	=> ())
  (check
      (with-result
	(let-values/std ((() (values)))
	  (one)
	  (two)
	  (values)))
    => '((one two)))

  (doit (let-values/std ((() (values)))
	  (one)
	  (two)
	  (values 1 2.3))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (with-result
	(let-values/std ((() (values)))
	  (one)
	  (two)
	  (values 1 2.3)))
    => '(1 2.3 (one two)))

;;; --------------------------------------------------------------------
;;; single clause, untyped syntactic bindings combinations

  (doit (let-values/std (((a) (values 1)))
	  (one)
	  (values a))
	=> (<top>))
  (check
      (with-result
	(let-values/std (((a) (values 1)))
	  (one)
	  (values a)))
    => '(1 (one)))

  (doit (let-values/std (((a b) (values 1 2.3)))
	  (one)
	  (values a b))
	=> (<top> <top>))
  (check
      (with-result
	(let-values/std (((a b) (values 1 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let-values/std (((a b c) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c))
	=> (<top> <top> <top>))
  (check
      (with-result
	(let-values/std (((a b c) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c)))
    => '(1 2.3 "ciao" (one)))

  (doit (let-values/std (((a b . rest) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest))
	=> (<top> <top> <top>))
  (check
      (with-result
	(let-values/std (((a b . rest) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest)))
    => '(1 2.3 ("C" "D") (one)))

;;; --------------------------------------------------------------------
;;; multiple clauses, untyped syntactic bindings combinations

  (doit (let-values/std (((a) (values 1))
			 ((b) (values 2.3)))
	  (one)
	  (values a b))
	=> (<top> <top>))
  (check
      (with-result
	(let-values/std (((a) (values 1))
			 ((b) (values 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let-values/std (((a b) (values 1 2.3))
			 ((c d) (values 1+i 2/3)))
	  (one)
	  (values a b c d))
	=> (<top> <top> <top> <top>))
  (check
      (with-result
	(let-values/std (((a b) (values 1 2.3))
			 ((c d) (values 1+i 2/3)))
	  (one)
	  (values a b c d)))
    => '(1 2.3 1+i 2/3(one)))

  (doit (let-values/std (((a b c) (values 1 2.3 "ciao"))
			 ((d e) (values 1+i 2/3)))
	  (one)
	  (values a b c d e))
	=> (<top> <top> <top> <top> <top>))
  (check
      (with-result
	(let-values/std (((a b c) (values 1 2.3 "ciao"))
			 ((d e) (values 1+i 2/3)))
	  (one)
	  (values a b c d e)))
    => '(1 2.3 "ciao" 1+i 2/3 (one)))

  (doit (let-values/std (((a b . rest) (values 1 2.3 "C" "D"))
			 ((c . stuff) (values 1+i 'x 'y)))
	  (one)
	  (values a b rest c stuff))
	=> (<top> <top> <top> <top> <top>))
  (check
      (with-result
	(let-values/std (((a b . rest) (values 1 2.3 "C" "D"))
			 ((c . stuff) (values 1+i 'x 'y)))
	  (one)
	  (values a b rest c stuff)))
    => '(1 2.3 ("C" "D") 1+i (x y) (one)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'let-values-checked))

  (define (one)
    (add-result 'one))

  (case-define two
    (()
     (add-result 'two))
    (args
     (add-result (cons 'two args))))

;;; --------------------------------------------------------------------
;;; no bindings, no return values

  (doit (let-values ()
	  (one)
	  (two)
	  (values))
	=> ())
  (check
      (with-result
	(let-values ()
	  (one)
	  (two)
	  (values)))
    => '((one two)))

;;; --------------------------------------------------------------------
;;; empty formals

  (doit (let-values ((() (values)))
	  (one)
	  (two)
	  (values))
	=> ())
  (check
      (with-result
	(let-values ((() (values)))
	  (one)
	  (two)
	  (values)))
    => '((one two)))

;;; --------------------------------------------------------------------
;;; single clause, typed syntactic bindings combinations

  (doit (let-values ((({a <fixnum>}) (values 1)))
	  (one)
	  (values a))
	=> (<fixnum>))
  (check
      (with-result
	(let-values ((({a <fixnum>}) (values 1)))
	  (one)
	  (values a)))
    => '(1 (one)))

  (doit (let-values ((({a <fixnum>} {b <flonum>}) (values 1 2.3)))
	  (one)
	  (values a b))
	=> (<fixnum> <flonum>))
  (check
      (with-result
	(let-values ((({a <fixnum>} {b <flonum>}) (values 1 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let-values ((({a <fixnum>} {b <flonum>} {c <nestring>}) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c))
	=> (<fixnum> <flonum> <nestring>))
  (check
      (with-result
	(let-values ((({a <fixnum>} {b <flonum>} {c <nestring>}) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c)))
    => '(1 2.3 "ciao" (one)))

  (doit (let-values ((({a <fixnum>} {b <flonum>} . {rest (list-of <nestring>)}) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest))
	=> (<fixnum> <flonum> (list-of <nestring>)))

  (check
      (with-result
	(let-values ((({a <fixnum>} {b <flonum>} . {rest (list-of <nestring>)}) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest)))
    => '(1 2.3 ("C" "D") (one)))

;;; --------------------------------------------------------------------
;;; single clause, untyped syntactic bindings combinations

  (doit (let-values (((a) (values 1)))
	  (one)
	  (values a))
	=> (<positive-fixnum>))
  (check
      (with-result
	(let-values (((a) (values 1)))
	  (one)
	  (values a)))
    => '(1 (one)))

  (doit (let-values (((a b) (values 1 2.3)))
	  (one)
	  (values a b))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (with-result
	(let-values (((a b) (values 1 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let-values (((a b c) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c))
	=> (<positive-fixnum> <positive-flonum> <nestring>))
  (check
      (with-result
	(let-values (((a b c) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c)))
    => '(1 2.3 "ciao" (one)))

  (doit (let-values (((a b . rest) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest))
	=> (<positive-fixnum> <positive-flonum> (list <nestring> <nestring>)))
  (check
      (with-result
	(let-values (((a b . rest) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest)))
    => '(1 2.3 ("C" "D") (one)))

;;; --------------------------------------------------------------------
;;; multiple clauses, untyped syntactic bindings combinations

  (doit (let-values (((a) (values 1))
		     ((b) (values 2.3)))
	  (one)
	  (values a b))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (with-result
	(let-values (((a) (values 1))
		     ((b) (values 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let-values (((a b) (values 1 2.3))
		     ((c d) (values 1+i 2/3)))
	  (one)
	  (values a b c d))
	=> (<positive-fixnum> <positive-flonum> <exact-compnum> <positive-ratnum>))
  (check
      (with-result
	(let-values (((a b) (values 1 2.3))
		     ((c d) (values 1+i 2/3)))
	  (one)
	  (values a b c d)))
    => '(1 2.3 1+i 2/3(one)))

  (doit (let-values (((a b c) (values 1 2.3 "ciao"))
		     ((d e) (values 1+i 2/3)))
	  (one)
	  (values a b c d e))
	=> (<positive-fixnum> <positive-flonum> <nestring> <exact-compnum> <positive-ratnum>))
  (check
      (with-result
	(let-values (((a b c) (values 1 2.3 "ciao"))
		     ((d e) (values 1+i 2/3)))
	  (one)
	  (values a b c d e)))
    => '(1 2.3 "ciao" 1+i 2/3 (one)))

  (doit (let-values (((a b . rest) (values 1 2.3 "C" "D"))
		     ((c . stuff) (values 1+i 'x 'y)))
	  (one)
	  (values a b rest c stuff))
	=> (<positive-fixnum> <positive-flonum> (list <nestring> <nestring>) <exact-compnum> (list (enumeration x) (enumeration y))))
  (check
      (with-result
	(let-values (((a b . rest) (values 1 2.3 "C" "D"))
		     ((c . stuff) (values 1+i 'x 'y)))
	  (one)
	  (values a b rest c stuff)))
    => '(1 2.3 ("C" "D") 1+i (x y) (one)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'let*-values-std))

  (define (one)
    (add-result 'one))

  (case-define two
    (()
     (add-result 'two))
    (args
     (add-result (cons 'two args))))

;;; --------------------------------------------------------------------
;;; no bindings, no return values

  (doit (let*-values/std ()
	  (one)
	  (two)
	  (values))
	=> ())
  (check
      (with-result
	(let*-values/std ()
	  (one)
	  (two)
	  (values)))
    => '((one two)))

;;; --------------------------------------------------------------------
;;; empty formals

  (doit (let*-values/std ((() (values)))
	  (one)
	  (two)
	  (values))
	=> ())
  (check
      (with-result
	(let*-values/std ((() (values)))
	  (one)
	  (two)
	  (values)))
    => '((one two)))

;;; --------------------------------------------------------------------
;;; single clause, untyped syntactic bindings combinations

  (doit (let*-values/std (((a) (values 1)))
	  (one)
	  (values a))
	=> (<top>))
  (check
      (with-result
	(let*-values/std (((a) (values 1)))
	  (one)
	  (values a)))
    => '(1 (one)))

  (doit (let*-values/std (((a b) (values 1 2.3)))
	  (one)
	  (values a b))
	=> (<top> <top>))
  (check
      (with-result
	(let*-values/std (((a b) (values 1 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let*-values/std (((a b c) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c))
	=> (<top> <top> <top>))
  (check
      (with-result
	(let*-values/std (((a b c) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c)))
    => '(1 2.3 "ciao" (one)))

  (doit (let*-values/std (((a b . rest) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest))
	=> (<top> <top> <top>))
  (check
      (with-result
	(let*-values/std (((a b . rest) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest)))
    => '(1 2.3 ("C" "D") (one)))

;;; --------------------------------------------------------------------
;;; multiple clauses, untyped syntactic bindings combinations

  (doit (let*-values/std (((a) (values 1))
			  ((b) (values 2.3)))
	  (one)
	  (values a b))
	=> (<top> <top>))
  (check
      (with-result
	(let*-values/std (((a) (values 1))
			  ((b) (values 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let*-values/std (((a b) (values 1 2.3))
			  ((c d) (values 1+i 2/3)))
	  (one)
	  (values a b c d))
	=> (<top> <top> <top> <top>))
  (check
      (with-result
	(let*-values/std (((a b) (values 1 2.3))
			  ((c d) (values 1+i 2/3)))
	  (one)
	  (values a b c d)))
    => '(1 2.3 1+i 2/3(one)))

  (doit (let*-values/std (((a b c) (values 1 2.3 "ciao"))
			  ((d e) (values 1+i 2/3)))
	  (one)
	  (values a b c d e))
	=> (<top> <top> <top> <top> <top>))
  (check
      (with-result
	(let*-values/std (((a b c) (values 1 2.3 "ciao"))
			  ((d e) (values 1+i 2/3)))
	  (one)
	  (values a b c d e)))
    => '(1 2.3 "ciao" 1+i 2/3 (one)))

  (doit (let*-values/std (((a b . rest) (values 1 2.3 "C" "D"))
			  ((c . stuff) (values 1+i 'x 'y)))
	  (one)
	  (values a b rest c stuff))
	=> (<top> <top> <top> <top> <top>))
  (check
      (with-result
	(let*-values/std (((a b . rest) (values 1 2.3 "C" "D"))
			  ((c . stuff) (values 1+i 'x 'y)))
	  (one)
	  (values a b rest c stuff)))
    => '(1 2.3 ("C" "D") 1+i (x y) (one)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'let*-values-checked))

  (define (one)
    (add-result 'one))

  (case-define two
    (()
     (add-result 'two))
    (args
     (add-result (cons 'two args))))

;;; --------------------------------------------------------------------
;;; no bindings, no return values

  (doit (let*-values ()
	  (one)
	  (two)
	  (values))
	=> ())
  (check
      (with-result
	(let*-values ()
	  (one)
	  (two)
	  (values)))
    => '((one two)))

;;; --------------------------------------------------------------------
;;; empty formals

  (doit (let*-values ((() (values)))
	  (one)
	  (two)
	  (values))
	=> ())
  (check
      (with-result
	(let*-values ((() (values)))
	  (one)
	  (two)
	  (values)))
    => '((one two)))

;;; --------------------------------------------------------------------
;;; single clause, typed syntactic bindings combinations

  (doit (let*-values ((({a <fixnum>}) (values 1)))
	  (one)
	  (values a))
	=> (<fixnum>))
  (check
      (with-result
	(let*-values ((({a <fixnum>}) (values 1)))
	  (one)
	  (values a)))
    => '(1 (one)))

  (doit (let*-values ((({a <fixnum>} {b <flonum>}) (values 1 2.3)))
	  (one)
	  (values a b))
	=> (<fixnum> <flonum>))
  (check
      (with-result
	(let*-values ((({a <fixnum>} {b <flonum>}) (values 1 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let*-values ((({a <fixnum>} {b <flonum>} {c <nestring>}) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c))
	=> (<fixnum> <flonum> <nestring>))
  (check
      (with-result
	(let*-values ((({a <fixnum>} {b <flonum>} {c <nestring>}) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c)))
    => '(1 2.3 "ciao" (one)))

  (doit (let*-values ((({a <fixnum>} {b <flonum>} . {rest (list-of <nestring>)}) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest))
	=> (<fixnum> <flonum> (list-of <nestring>)))

  (check
      (with-result
	(let*-values ((({a <fixnum>} {b <flonum>} . {rest (list-of <nestring>)}) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest)))
    => '(1 2.3 ("C" "D") (one)))

;;; --------------------------------------------------------------------
;;; single clause, untyped syntactic bindings combinations

  (doit (let*-values (((a) (values 1)))
	  (one)
	  (values a))
	=> (<positive-fixnum>))
  (check
      (with-result
	(let*-values (((a) (values 1)))
	  (one)
	  (values a)))
    => '(1 (one)))

  (doit (let*-values (((a b) (values 1 2.3)))
	  (one)
	  (values a b))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (with-result
	(let*-values (((a b) (values 1 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let*-values (((a b c) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c))
	=> (<positive-fixnum> <positive-flonum> <nestring>))
  (check
      (with-result
	(let*-values (((a b c) (values 1 2.3 "ciao")))
	  (one)
	  (values a b c)))
    => '(1 2.3 "ciao" (one)))

  (doit (let*-values (((a b . rest) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest))
	=> (<positive-fixnum> <positive-flonum> (list <nestring> <nestring>)))
  (check
      (with-result
	(let*-values (((a b . rest) (values 1 2.3 "C" "D")))
	  (one)
	  (values a b rest)))
    => '(1 2.3 ("C" "D") (one)))

;;; --------------------------------------------------------------------
;;; multiple clauses, untyped syntactic bindings combinations

  (doit (let*-values (((a) (values 1))
		      ((b) (values 2.3)))
	  (one)
	  (values a b))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (with-result
	(let*-values (((a) (values 1))
		      ((b) (values 2.3)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  (doit (let*-values (((a b) (values 1 2.3))
		      ((c d) (values 1+i 2/3)))
	  (one)
	  (values a b c d))
	=> (<positive-fixnum> <positive-flonum> <exact-compnum> <positive-ratnum>))
  (check
      (with-result
	(let*-values (((a b) (values 1 2.3))
		      ((c d) (values 1+i 2/3)))
	  (one)
	  (values a b c d)))
    => '(1 2.3 1+i 2/3(one)))

  (doit (let*-values (((a b c) (values 1 2.3 "ciao"))
		      ((d e) (values 1+i 2/3)))
	  (one)
	  (values a b c d e))
	=> (<positive-fixnum> <positive-flonum> <nestring> <exact-compnum> <positive-ratnum>))
  (check
      (with-result
	(let*-values (((a b c) (values 1 2.3 "ciao"))
		      ((d e) (values 1+i 2/3)))
	  (one)
	  (values a b c d e)))
    => '(1 2.3 "ciao" 1+i 2/3 (one)))

  (doit (let*-values (((a b . rest) (values 1 2.3 "C" "D"))
		      ((c . stuff) (values 1+i 'x 'y)))
	  (one)
	  (values a b rest c stuff))
	=> (<positive-fixnum> <positive-flonum> (list <nestring> <nestring>) <exact-compnum> (list (enumeration x) (enumeration y))))
  (check
      (with-result
	(let*-values (((a b . rest) (values 1 2.3 "C" "D"))
		      ((c . stuff) (values 1+i 'x 'y)))
	  (one)
	  (values a b rest c stuff)))
    => '(1 2.3 ("C" "D") 1+i (x y) (one)))

;;; --------------------------------------------------------------------
;;; variables references

  (doit (let*-values (((a b) (values 1 2.3))
		      ((a b) (values a b)))
	  (one)
	  (values a b))
	=> (<positive-fixnum> <positive-flonum>))
  (check
      (with-result
	(let*-values (((a b) (values 1 2.3))
		      ((a b) (values a b)))
	  (one)
	  (values a b)))
    => '(1 2.3 (one)))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'if))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   (.syntax-object (type-of ?expr))
	 (=> expander::syntax=?)
	 (syntax (?expected))))
      ))

;;; --------------------------------------------------------------------
;;; PAIR? in test

  (doit (let (({var <list>} '(1 2 3)))
	  (if (pair? var)
	      var
	    #f))
	=> (or <nelist> <false>))

  (doit (let ((var '(1 2 3)))
	  (if (pair? var)
	      var
	    #f))
	=> (list <positive-fixnum> <positive-fixnum> <positive-fixnum>))

  (doit (let (({var (list-of <fixnum>)} '(1 2 3)))
	  (if (pair? var)
	      var
	    #f))
	=> (or (pair <fixnum> (list-of <fixnum>)) <false>))

  (doit (let (({var <pair>} '(1 2 3)))
	  (if (pair? var)
	      var
	    #f))
	=> <pair>)

  (doit (let (({var <top>} '(1 2 3)))
	  (if (pair? var)
	      var
	    #f))
	=> (or <pair> <false>))

;;; --------------------------------------------------------------------
;;; NULL? in test

  (doit (let (({var <list>} '(1 2 3)))
	  (if (null? var)
	      var
	    #f))
	=> (or <null> <false>))

  (void))


(parametrise ((check-test-name	'cond))

  (doit (cond ((read)	1)
	      ((read)	2)
	      (else	3))
	=> (<positive-fixnum>))

  (doit (cond ((read)	1)
	      ((read)	2)
	      (else	#f))
	=> ((or <positive-fixnum> <false>)))

  (doit (cond ((read)	1)
	      ((read)	2.0)
	      (else	#f))
	=> ((or <positive-fixnum> <positive-flonum> <false>)))

  (void))


(parametrise ((check-test-name	'case))

  (doit (case (read)
	  ((1)		1)
	  ((ciao)	2)
	  (else		3))
	=> (<positive-fixnum>))

  (doit (case (read)
	  ((1)		1)
	  ((ciao)	'ciao)
	  (else		#f))
	=> ((or <positive-fixnum> <false> (enumeration ciao))))

  ;;Without ELSE clause: if no datum matches, CASE returns void.
  ;;
  (doit (case (read)
	  ((1)	1)
	  ((2)	"ciao"))
	=> (<void>))

  ;;Special case of type propagation from the datum to the lambda.
  ;;
  (doit (case (read)
	  ((1)	=> (lambda (x) (list 'hey x)))
	  ((2)	"ciao")
	  (else	#f))
	=> ((or <nestring>
		(list (enumeration hey) <positive-fixnum>)
		<false>)))

  (doit (case (read)
	  ((1)	=> (lambda ({x <positive-fixnum>}) x))
	  ((2)	"ciao")
	  (else	#f))
	=> ((or <nestring> <positive-fixnum> <false>)))

  (doit (case (read)
	  ((1)	=> (lambda (x) 999))
	  ((2)	"ciao")
	  (else	#f))
	=> ((or <nestring> <positive-fixnum> <false>)))

  (void))


(parametrise ((check-test-name	'and))

  (doit (and)
	=> (<true>))

  (doit (and 1 2 3)
	=> (<positive-fixnum>))

  (doit (and 1 "2" 3)
	=> (<positive-fixnum>))

  (doit (and 1 2 "3")
	=> (<nestring>))

  (doit (and #f 2 3)
	=> (<false>))

  (doit (and 1 #f 3)
	=> (<false>))

  (doit (and 1 2 #f)
	=> (<false>))

  (doit (and 1 (and 2.1 2.2) 3)
	=> (<positive-fixnum>))

  (doit (and 1 2 (and 3.1 3.2))
	=> (<positive-flonum>))

  (void))


(parametrise ((check-test-name	'or))

  (doit (or)
	=> (<false>))

  ;;This is expanded to:
  ;;
  ;;   (quote 1)
  ;;
  (doit (or 1 2 "3")
	=> (<positive-fixnum>))

  ;;This is expanded to:
  ;;
  ;;   (begin
  ;;     #f
  ;;     (quote 2))
  ;;
  (doit (or #f 2 "3")
	=> (<positive-fixnum>))

  (doit (or #f #f "3")
	=> (<nestring>))

  ;;This is expanded to:
  ;;
  ;;   (begin
  ;;     #f
  ;;     (quote #t)
  ;;     (quote "ciao"))
  ;;
  (doit (or #f (and #t "ciao") 3.4)
	=> (<nestring>))

  (doit (or (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<top>) (read)))
	=> (<top>))

  (doit (or (unsafe-cast-signature (<boolean>) (read))
	    (unsafe-cast-signature (<boolean>) (read))
	    (unsafe-cast-signature (<boolean>) (read)))
	=> (<boolean>))

  (doit (or (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<fixnum>) (read))
	    (unsafe-cast-signature (<top>) (read)))
	=> (<top>))

  (doit (or (unsafe-cast-signature (<fixnum>) (read))
	    (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<top>) (read)))
	=> (<fixnum>))

  (doit (or (unsafe-cast-signature ((or <false> <nestring>)) (read))
	    (unsafe-cast-signature (<fixnum>) (read))
	    (unsafe-cast-signature (<top>) (read)))
	=> ((or <nestring> <fixnum>)))

  (doit (or (unsafe-cast-signature ((or <false> <nestring>)) (read))
	    (unsafe-cast-signature ((or <false> <vector>)) (read))
	    (unsafe-cast-signature (<fixnum>) (read)))
	=> ((or <nestring> <vector> <fixnum>)))

  (doit (or (unsafe-cast-signature ((or <false> <fixnum>)) (read))
	    (unsafe-cast-signature ((or <false> <nestring>)) (read))
	    (unsafe-cast-signature ((or <false> <vector>)) (read)))
	=> ((or <fixnum> <nestring> <false> <vector>)))

  (doit (or (unsafe-cast-signature ((or <boolean> <fixnum>)) (read))
	    (unsafe-cast-signature ((or <boolean> <nestring>)) (read))
	    (unsafe-cast-signature ((or <boolean> <vector>)) (read)))
	=> ((or <fixnum> <nestring> <boolean> <vector>)))

  (doit (or (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<top>) (read))
	    (error #f "ciao"))
	=> (<top>))

  ;;This raises an expand-time exception because void is forbidden as argument to OR.
  ;;
  ;; (doit (or (unsafe-cast-signature (<top>) (read))
  ;; 	    (void)
  ;; 	    (unsafe-cast-signature (<top>) (read)))
  ;; 	=> (<void>))

  (void))


(parametrise ((check-test-name	'not))

  (doit (not #t)
	=> (<false>))

  (doit (not #f)
	=> (<true>))

  (doit (not (read))
	=> (<boolean>))

  (void))


(parametrise ((check-test-name	'xor))

;;; XOR returns non-false if only one sub-expression is non-false.

  (doit (xor #f)		=> (<false>))
  (doit (xor 123)		=> (<positive-fixnum>))

  (doit (xor 123 #f)		=> (<positive-fixnum>))
  (doit (xor #f 123)		=> (<positive-fixnum>))

  (doit (xor #f #f)		=> (<false>))
  (doit (xor #f #f)		=> (<false>))

  (doit (xor #f 2 3)		=> (<false>))
  (doit (xor 1 #f 3)		=> (<false>))
  (doit (xor 1 2 #f)		=> (<false>))

  (doit (xor 1 #f #f)		=> (<positive-fixnum>))
  (doit (xor #f 2 #f)		=> (<positive-fixnum>))
  (doit (xor #f #f 3)		=> (<positive-fixnum>))

  (doit (xor "1" #f #f)		=> (<nestring>))
  (doit (xor #f "2" #f)		=> (<nestring>))
  (doit (xor #f #f "3")		=> (<nestring>))

;;; --------------------------------------------------------------------

  (doit (xor (read) #f)		=> (<top>))
  (doit (xor #f     (read))	=> (<top>))
  (doit (xor (read) (read))	=> (<top>))

  (doit (xor (read) #f #f)	=> (<top>))
  (doit (xor #f (read) #f)	=> (<top>))
  (doit (xor #f #f (read))	=> (<top>))

  (doit (xor (read) (read) (read))	=> (<top>))

  (doit (xor (cast-signature (<boolean>) (read))
	     (cast-signature (<boolean>) (read))
	     (cast-signature (<boolean>) (read)))
	=> (<boolean>))

  (void))


(parametrise ((check-test-name	'application))

  (doit (let ((fun (lambda ({a <fixnum>}) a)))
	  (fun 1))
	=> (<fixnum>))

  ;;Exact match between operands and clauses.
  ;;
  (let ((fun (case-lambda
	       (({a <fixnum>}) a)
	       (({a <fixnum>} {b <fixnum>}) (fx+ a b)))))
    (doit (fun 1)	=> (<fixnum>))
    (doit (fun 1 -2)	=> (<fixnum>))
    #| end of LET |# )

  ;;Exact match between operands and clauses.
  ;;
  (let ((fun (case-lambda
	       (({a <fixnum>}) a)
	       (({a <nestring>} {b <nestring>}) (string-append a b)))))
    (doit (fun 1)			=> (<fixnum>))
    (doit (fun "ciao" " mamma")		=> (<nestring>))
    #| end of LET |# )

  ;;Possible match between operands and clauses: only one clause is possible.
  ;;
  (let ((fun (case-lambda
	       (({a <fixnum>}) a)
	       (({a <nestring>} {b <nestring>}) (string-append a b)))))
    (doit (fun (cast-signature (<number>) 1))		=> (<fixnum>))
    (doit (fun (cast-signature (<top>) "ciao")
	       (cast-signature (<top>) " mamma"))	=> (<nestring>))
    #| end of LET |# )

  ;;Possible match between operands and clauses: multiple clauses are possible.
  ;;
  (let ((fun (case-lambda
	       (({a <fixnum>}) a)
	       ((a b c d)	'no-match)
	       (({a <nestring>} . {b* (list-of <nestring>)}) (apply string-append a b*)))))
    (doit (fun (cast-signature (<top>) 1))		=> ((or <nestring> <fixnum>)))
    (doit (fun (cast-signature (<top>) "ciao"))		=> ((or <nestring> <fixnum>)))
    (doit (fun (cast-signature (<top>) "ciao")
	       (cast-signature (<top>) " mamma"))	=> (<nestring>))
    #| end of LET |# )

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'apply-values))

  (doit (apply values '())
	=> ())

  (doit (apply values 1 2.3 '())
	=> (<positive-fixnum> <positive-flonum>))

  (doit (apply values 1 2.3 '(a "b"))
	=> (<positive-fixnum> <positive-flonum> (enumeration a) <nestring>))

  (doit (apply values '(a "b"))
	=> ((enumeration a) <nestring>))

;;; --------------------------------------------------------------------

  (doit (apply values 1 2.3 (unsafe-cast-signature (<list>) '(a "b")))
	=> (<positive-fixnum> <positive-flonum> . <list>))

  (doit (apply values 1 2.3 (unsafe-cast-signature (<nelist>) '(a "b")))
	=> (<positive-fixnum> <positive-flonum> . <nelist>))

  (doit (apply values 1 2.3 (unsafe-cast-signature ((list-of <symbol>)) '(a b)))
	=> (<positive-fixnum> <positive-flonum> . (list-of <symbol>)))

  #| end of parametrise |# )


(parametrise ((check-test-name	'dynamic-wind))

  (doit (dynamic-wind
	    (lambda () 'in)
	    (lambda () 1)
	    (lambda () 'out))
	=> (<positive-fixnum>))

  (doit (dynamic-wind
	    (lambda () 'in)
	    (lambda () (values 1 2.3))
	    (lambda () 'out))
	=> (<positive-fixnum> <positive-flonum>))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'define-in-body))

  (doit (internal-body
	  (define a 1)
	  a)
	=> (<positive-fixnum>))

  (doit (internal-body
	  (define a 1)
	  (define b a)
	  b)
	=> (<positive-fixnum>))

  (doit (internal-body
	  (define a 1)
	  (define b a)
	  (define c b)
	  c)
	=> (<positive-fixnum>))

;;; --------------------------------------------------------------------
;;; standard variables

  (doit (internal-body
	  (define/std a 1)
	  a)
	=> (<top>))

  (doit (internal-body
	  (define/std a)
	  a)
	=> (<top>))

;;; --------------------------------------------------------------------
;;; typed variables

  (doit (internal-body
	  (define/typed {a <fixnum>} 1)
	  a)
	=> (<fixnum>))

  (doit (internal-body
	  (define/typed a)
	  a)
	=> (<void>))

  (doit (internal-body
	  (define/typed a 1)
	  a)
	=> (<positive-fixnum>))

  (doit (internal-body
	  (define/typed a (cast-signature <list> 1))
	  a)
	=> (<top>))

  (doit (internal-body
	  (define/typed a (cast-signature (list-of <fixnum>) 1))
	  a)
	=> (<fixnum>))

;;; --------------------------------------------------------------------
;;; checked variables

  (doit (internal-body
	  (define/checked {a <fixnum>} 1)
	  a)
	=> (<fixnum>))

  (doit (internal-body
	  (define/checked a)
	  a)
	=> (<void>))

  (doit (internal-body
	  (define/checked a 1)
	  a)
	=> (<positive-fixnum>))

  (doit (internal-body
	  (define/checked a (cast-signature <list> 1))
	  a)
	=> (<top>))

  (doit (internal-body
	  (define/checked a (cast-signature (list-of <fixnum>) 1))
	  a)
	=> (<fixnum>))

;;;

  (doit (internal-body
	  (define {a <fixnum>} 1)
	  a)
	=> (<fixnum>))

  (doit (internal-body
	  (define a)
	  a)
	=> (<void>))

  (doit (internal-body
	  (define a 1)
	  a)
	=> (<positive-fixnum>))

  (doit (internal-body
	  (define a (cast-signature <list> 1))
	  a)
	=> (<top>))

  (doit (internal-body
	  (define a (cast-signature (list-of <fixnum>) 1))
	  a)
	=> (<fixnum>))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'define-values-in-body))

;;; standard

  (doit (internal-body
	  (define-values/std (a)
	    1)
	  a)
	=> (<top>))

  (doit (internal-body
	  (define-values/std (a b)
	    (values 1 2.3))
	  (values a b))
	=> (<top> <top>))

  (doit (internal-body
	  (define-values/std (a b . rest)
	    (values 1 2.3 'X 'Y))
	  (values a b rest))
	=> (<top> <top> <top>))

  (doit (internal-body
	  (define-values/std args
	    (values 'X 'Y))
	  args)
	=> (<top>))

;;; --------------------------------------------------------------------
;;; checked

  (doit (internal-body
	  (define-values/checked (a)
	    1)
	  a)
	=> (<positive-fixnum>))
  (check
      (internal-body
	(define-values/checked (a)
	  1)
	a)
    => 1)
  ;; (debug-print
  ;;  (expansion-of
  ;;   (internal-body
  ;;     (define-values/checked (a)
  ;; 	1)
  ;;     a)))

  (doit (internal-body
	  (define-values/checked (a b)
	    (values 1 2.3))
	  (values a b))
	=> (<positive-fixnum> <positive-flonum>))

  (doit (internal-body
	  (define-values/checked ({a <fixnum>} {b <flonum>})
	    (values 1 2.3))
	  (values a b))
	=> (<fixnum> <flonum>))

  (doit (internal-body
	  (define-values/checked ({a <fixnum>} b)
	    (values 1 2.3))
	  (values a b))
	=> (<fixnum> <positive-flonum>))

  (doit (internal-body
	  (define-values/checked (a {b <flonum>})
	    (values 1 2.3))
	  (values a b))
	=> (<positive-fixnum> <flonum>))

  (doit (internal-body
	  (define-values/checked ({a <fixnum>} {b <flonum>} . rest)
	    (values 1 2.3 'X 'Y))
	  (values a b rest))
	=> (<fixnum> <flonum> <list>))

  (doit (internal-body
	  (define-values/checked ({a <fixnum>} {b <flonum>} . {rest (list-of (enumeration X Y))})
	    (values 1 2.3 'X 'Y))
	  (values a b rest))
	=> (<fixnum> <flonum> (list-of (enumeration X Y))))

  (doit (internal-body
	  (define-values/checked args
	    (values 'X 'Y))
	  args)
	=> (<list>))
  (doit (internal-body
	  (define-values/checked {args (list-of (enumeration X Y))}
	    (values 'X 'Y))
	  args)
	=> ((list-of (enumeration X Y))))

;;;

  (doit (internal-body
	  (define-values (a)
	    1)
	  a)
	=> (<positive-fixnum>))

  (doit (internal-body
	  (define-values (a b)
	    (values 1 2.3))
	  (values a b))
	=> (<positive-fixnum> <positive-flonum>))

  (doit (internal-body
	  (define-values ({a <fixnum>} {b <flonum>})
	    (values 1 2.3))
	  (values a b))
	=> (<fixnum> <flonum>))

  (doit (internal-body
	  (define-values ({a <fixnum>} b)
	    (values 1 2.3))
	  (values a b))
	=> (<fixnum> <positive-flonum>))

  (doit (internal-body
	  (define-values (a {b <flonum>})
	    (values 1 2.3))
	  (values a b))
	=> (<positive-fixnum> <flonum>))

  (doit (internal-body
	  (define-values ({a <fixnum>} {b <flonum>} . rest)
	    (values 1 2.3 'X 'Y))
	  (values a b rest))
	=> (<fixnum> <flonum> <list>))

  (doit (internal-body
	  (define-values ({a <fixnum>} {b <flonum>} . {rest (list-of (enumeration X Y))})
	    (values 1 2.3 'X 'Y))
	  (values a b rest))
	=> (<fixnum> <flonum> (list-of (enumeration X Y))))
  (check
      (internal-body
	(define-values ({a <fixnum>} {b <flonum>} . {rest (list-of (enumeration X Y))})
	  (values 1 2.3 'X 'Y))
	(values a b rest))
    => 1 2.3 '(X Y))

  (doit (internal-body
	  (define-values args
	    (values 'X 'Y))
	  args)
	=> (<list>))

  (doit (internal-body
	  (define-values {args (list-of (enumeration X Y))}
	    (values 'X 'Y))
	  args)
	=> ((list-of (enumeration X Y))))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'doc-examples))

  #;(debug-print
   (type-of (let ((A 1))
	      (let* ((B A)
		     (C B))
		(letrec ((D C))
		  (letrec* ((E D)
			    (F E))
		    (define (G)
		      F)
		    ((lambda () (G)))))))))

  (doit (let ((A 1))
	  (let* ((B A)
		 (C B))
	    (letrec ((D C))
	      (letrec* ((E D)
			(F E))
		(define (G)
		  F)
		(define (H)
		  2.3)
		(define-values (I J)
		  (values (G) (H)))
		((lambda () I))))))
	=> (<positive-fixnum>))

;;; --------------------------------------------------------------------

  (begin
    #;(debug-print
     (type-of (let ((A 1))
		(let* ((B A)
		       (C B))
		  (letrec ((D C))
		    (letrec* ((E D)
			      (F E))
		      (define (G)
			F)
		      (define H
			(G))
		      ((lambda () H))))))))

    (doit (let ((A 1))
	    (let* ((B A)
		   (C B))
	      (letrec ((D C))
		(letrec* ((E D)
			  (F E))
		  (define (G)
		    F)
		  (define H
		    (G))
		  ((lambda () H))))))
	  => (<positive-fixnum>))

    #| end of BEGIN |#  )

;;; --------------------------------------------------------------------
;;; mutated variables

  (doit (let ((A "ciao"))
	  (set! A "hello")
	  A)
	=> (<nestring>))

  #;(doit (let ((A 1))
	  (set! A -1)
	  A)
	=> (<fixnum>))

  (doit (let (({A <fixnum>} 1))
	  (set! A -1)
	  A)
	=> (<fixnum>))

;;; --------------------------------------------------------------------
;;; CASE

  (doit (case (read)
	  ((1 "2" 'ciao) => (lambda (x) x))
	  (else #f))
	=> ((or <positive-fixnum> <nestring> (enumeration ciao) <false>)))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
