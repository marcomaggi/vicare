;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for typed language syntaxes
;;;Date: Sat Oct 10, 2015
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
(program (test-typed-language-syntaxes)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
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
    (prefix (vicare system type-descriptors)
	    td::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: type-related syntaxes\n")


;;;; helpers

(define-syntax-rule (print-expansion ?stx)
  (debug-print (quote expansion-example:) (quote ?stx) (quote ==>) (expansion-of ?stx)))

(library (some-type-annotations)
  (options strict-r6rs)
  (export
    <list-of-fixnums>		<vector-of-fixnums>	<pair-of-fixnums>
    <list-of-flonums>		<vector-of-flonums>	<pair-of-flonums>
    <list-of-reals>		<vector-of-reals>	<pair-of-reals>
    <list-of-numbers>		<vector-of-numbers>	<pair-of-numbers>
    <list-of-strings>		<vector-of-strings>	<pair-of-strings>)
  (import (vicare))
  (define-type <list-of-fixnums>	(list-of <fixnum>))
  (define-type <list-of-flonums>	(list-of <flonum>))
  (define-type <list-of-numbers>	(list-of <number>))
  (define-type <list-of-reals>		(list-of <real>))
  (define-type <list-of-strings>	(list-of <string>))
  (define-type <vector-of-fixnums>	(vector-of <fixnum>))
  (define-type <vector-of-flonums>	(vector-of <flonum>))
  (define-type <vector-of-numbers>	(vector-of <number>))
  (define-type <vector-of-reals>	(vector-of <real>))
  (define-type <vector-of-strings>	(vector-of <string>))
  (define-type <pair-of-fixnums>	(pair-of <fixnum>))
  (define-type <pair-of-flonums>	(pair-of <flonum>))
  (define-type <pair-of-numbers>	(pair-of <number>))
  (define-type <pair-of-reals>		(pair-of <real>))
  (define-type <pair-of-strings>	(pair-of <string>))
  #| end of LIBRARY |# )

(import (some-type-annotations))

;;; --------------------------------------------------------------------

(define-constant EVAL-ENVIRONMENT
  (environment '(vicare) '(some-type-annotations)))

(define (%type-signature->sexp sig)
  (syntax->datum (expander::type-signature.syntax-object sig)))

(define-syntax check-expand-time-signature-violation
  (syntax-rules (=>)
    ((_ ?input-form => ?expected-signature-sexp ?returned-signature-sexp)
     (check
	 (try
	     (eval (quote ?input-form)
		   EVAL-ENVIRONMENT
		   (expander-options typed-language)
		   (compiler-options))
	   (catch E
	     ((expander::&expand-time-type-signature-violation)
	      #;(print-condition E)
	      (values (syntax->datum (expander::type-signature.syntax-object (expander::condition-expected-type-signature E)))
		      (syntax->datum (expander::type-signature.syntax-object (expander::condition-returned-type-signature E)))))
	     (else
	      (values E #f))))
       => (quote ?expected-signature-sexp) (quote ?returned-signature-sexp)))
    ))


(parametrise ((check-test-name	'type-descriptor))

  (check
      (td::core-type-descriptor? (type-descriptor <string>))
    => #t)

  (check
      (td::core-type-descriptor.name (type-descriptor <string>))
    => '<string>)

  (let ((btd (type-descriptor <string>)))

    (check
	(td::core-type-descriptor.name (td::core-type-descriptor.parent btd))
      => '<top>)

    (check
	(td::core-type-descriptor.uids-list btd)
      => '(vicare:core-type:<string> vicare:core-type:<top>))

    (check-for-true	(procedure? (td::core-type-descriptor.method-retriever btd)))
    (check-for-true	((td::core-type-descriptor.method-retriever btd) 'length))

    (check
	(let* ((retriever (td::core-type-descriptor.method-retriever btd))
	       ({strlen <procedure>} (retriever 'length)))
	  (strlen "ciao"))
      => 4)

    (void))

;;; --------------------------------------------------------------------

  (internal-body
    (define {string-btd td::<core-type-descriptor>}
      (type-descriptor <string>))

    (check
	(.name string-btd)
      => '<string>)

    (check
	(td::core-type-descriptor.name (.parent string-btd))
      => '<top>)

    (check
	(.uids-list string-btd)
      => '(vicare:core-type:<string> vicare:core-type:<top>))

    (check-for-true	(procedure? (.method-retriever string-btd)))
    (check-for-true	((.method-retriever string-btd) 'length))

    (check
	(let* ((retriever (td::core-type-descriptor.method-retriever string-btd))
	       ({strlen <procedure>} (retriever 'length)))
	  (strlen "ciao"))
      => 4)

    (void))

  (void))


(parametrise ((check-test-name	'type-unique-identifiers))

  (check
      (type-unique-identifiers <top>)
    => '(vicare:core-type:<top>))

  (check
      (type-unique-identifiers <string>)
    => '(vicare:core-type:<string>
	 vicare:core-type:<top>))

  (check
      (type-unique-identifiers <condition>)
    => '(vicare:core-type:<condition>
	 vicare:core-type:<record>
	 vicare:core-type:<struct>
	 vicare:core-type:<top>))

  (check
      (type-unique-identifiers <compound-condition>)
    => '(vicare:core-type:<compound-condition>
	 vicare:core-type:<condition>
	 vicare:core-type:<record>
	 vicare:core-type:<struct>
	 vicare:core-type:<top>))

  (check
      (type-unique-identifiers &condition)
    => '(vicare:core-type:&condition
	 vicare:core-type:<condition>
	 vicare:core-type:<record>
	 vicare:core-type:<struct>
	 vicare:core-type:<top>))

  (check
      (type-unique-identifiers &message)
    => '(vicare:core-type:&message
	 vicare:core-type:&condition
	 vicare:core-type:<condition>
	 vicare:core-type:<record>
	 vicare:core-type:<struct>
	 vicare:core-type:<top>))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-struct duo
	  (one two)
	  (nongenerative yeah))
	(type-unique-identifiers duo))
    => '(yeah
	 vicare:core-type:<struct>
	 vicare:core-type:<top>))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define-record-type duo
	  (nongenerative duo-1)
	  (fields one two))
	(type-unique-identifiers duo))
    => '(duo-1
	 vicare:core-type:<record>
	 vicare:core-type:<struct>
	 vicare:core-type:<top>))

  (check
      (internal-body
	(define-record-type alpha
	  (nongenerative alpha-1))
	(define-record-type beta
	  (parent alpha)
	  (nongenerative beta-1))
	(type-unique-identifiers beta))
    => '(beta-1
	 alpha-1
	 vicare:core-type:<record>
	 vicare:core-type:<struct>
	 vicare:core-type:<top>))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'is-a))

  (check-for-true	(is-a? "string" <string>))
  (check-for-false	(is-a? 123      <string>))

  (check-for-true	(is-a? '(1 2 3) (list-of <fixnum>)))
  (check-for-false	(is-a? "ciao"   (list-of <fixnum>)))

  (check-for-true	(is-a? '#(1 2 3) (vector-of <fixnum>)))
  (check-for-false	(is-a? "ciao"    (vector-of <fixnum>)))

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? error			(lambda (<&who-value> <string> . <list>) => <bottom>)))
  (check-for-true	(is-a? assertion-violation	(lambda (<&who-value> <string> . <list>) => <bottom>)))

  (void))


(parametrise ((check-test-name	'type-annotation-syntax))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   ?expr
	 (=> expander::syntax=?)
	 (syntax ?expected)))
      ))

;;; --------------------------------------------------------------------

  (doit (type-annotation-syntax (or <fixnum> <bignum>))
	=> (or <fixnum> <bignum>))

  (doit (type-annotation-syntax (or (enumeration hello salut)
				    (enumeration ciao)
				    (enumeration ohayo ciao)))
	=> (enumeration hello salut ohayo ciao))

;;; --------------------------------------------------------------------
;;; handling of void

  (doit (type-annotation-syntax (or <fixnum> <void>))
	=> <void>)

  (doit (type-annotation-syntax (and <fixnum> <void>))
	=> <void>)

  (doit (type-annotation-syntax (not <void>))
	=> (not <void>))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'make-type-specification))

  (check-for-true
   (let ((ots (expander::make-type-specification #'<string>)))
     (is-a? ots expander::<object-type-spec>)))

  (void))


(parametrise ((check-test-name	'type-annotation=?))

  (check-for-true	(type-annotation=? <top> <top>))
  (check-for-true	(type-annotation=? <fixnum> <fixnum>))

  (check-for-false	(type-annotation=? <fixnum> <flonum>))
  (check-for-false	(type-annotation=? <fixnum> <positive-fixnum>))
  (check-for-false	(type-annotation=? <positive-fixnum> <fixnum>))

;;; --------------------------------------------------------------------

  (internal-body
    (define-type <my-fixnum>	<fixnum>)
    (define-type <my-fixnum1>	<fixnum>)

    (check-for-true	(type-annotation=? <fixnum> <my-fixnum>))
    (check-for-true	(type-annotation=? <my-fixnum> <fixnum>))
    (check-for-true	(type-annotation=? <my-fixnum1> <my-fixnum>))
    (check-for-true	(type-annotation=? <my-fixnum> <my-fixnum1>))

    (void))

;;; --------------------------------------------------------------------
;;; LAMBDA

  (check-for-true	(type-annotation=? (lambda (<fixnum>) => (<fixnum>))
					   (lambda (<fixnum>) => (<fixnum>))))

  (check-for-false	(type-annotation=? (lambda (<flonum>) => (<fixnum>))
					   (lambda (<fixnum>) => (<fixnum>))))

  (check-for-false	(type-annotation=? (lambda (<fixnum>) => (<flonum>))
					   (lambda (<fixnum>) => (<fixnum>))))

  (check-for-false	(type-annotation=? (lambda (<fixnum>) => (<fixnum>))
					   (lambda (<flonum>) => (<fixnum>))))

  (check-for-false	(type-annotation=? (lambda (<fixnum>) => (<fixnum>))
					   (lambda (<fixnum>) => (<flonum>))))

;;;

  (check-for-true	(type-annotation=? (equality-predicate <fixnum>)
					   (lambda (<fixnum> <fixnum>) => (<boolean>))))
  (check-for-true	(type-annotation=? (equality-predicate <string>)
					   (lambda (<string> <string>) => (<boolean>))))

  (check-for-true	(type-annotation=? (comparison-procedure <fixnum>)
					   (lambda (<fixnum> <fixnum>) => (<fixnum>))))
  (check-for-true	(type-annotation=? (comparison-procedure <string>)
					   (lambda (<string> <string>) => (<fixnum>))))

  (check-for-true	(type-annotation=? (hash-function <fixnum>)
					   (lambda (<fixnum>) => (<non-negative-fixnum>))))
  (check-for-true	(type-annotation=? (hash-function <string>)
					   (lambda (<string>) => (<non-negative-fixnum>))))

;;; --------------------------------------------------------------------
;;; case-lambda

  (check-for-true	(type-annotation=? (case-lambda
					     ((<fixnum>) => (<fixnum>))
					     ((<string>) => (<symbol>)))
					   (case-lambda
					     ((<fixnum>) => (<fixnum>))
					     ((<string>) => (<symbol>)))))

  (check-for-true	(type-annotation=? (case-lambda
					     ((<fixnum>) => (<fixnum>))
					     ((<string>) => (<symbol>)))
					   (case-lambda
					     ((<string>) => (<symbol>))
					     ((<fixnum>) => (<fixnum>)))))

  (check-for-false	(type-annotation=? (case-lambda
					     ((<fixnum>) => (<fixnum>))
					     ((<boolean>) => (<number>))
					     ((<string>) => (<symbol>)))
					   (case-lambda
					     ((<fixnum>) => (<fixnum>))
					     ((<string>) => (<symbol>)))))

  (check-for-false	(type-annotation=? (case-lambda
					     ((<fixnum>) => (<fixnum>))
					     ((<string>) => (<symbol>)))
					   (case-lambda
					     ((<fixnum>) => (<fixnum>))
					     ((<boolean>) => (<number>))
					     ((<string>) => (<symbol>)))))

;;; --------------------------------------------------------------------
;;; union

  (check-for-true	(type-annotation=? (or <fixnum> <string>)
					   (or <fixnum> <string>)))
  (check-for-true	(type-annotation=? (or <fixnum> <string>)
					   (or <string> <fixnum>)))

  (check-for-false	(type-annotation=? (or <fixnum> <string>)
					   (or <fixnum> <symbol>)))
  (check-for-false	(type-annotation=? (or <fixnum> <string>)
					   (or <symbol> <fixnum>)))
  (check-for-false	(type-annotation=? (or <fixnum> <string>)
					   (or <fixnum> <string> <boolean>)))
  (check-for-false	(type-annotation=? (or <fixnum> <string> <boolean>)
					   (or <fixnum> <string>)))

;;; --------------------------------------------------------------------
;;; intersection

  (check-for-true	(type-annotation=? (and <fixnum> <string>)
					   (and <fixnum> <string>)))
  (check-for-true	(type-annotation=? (and <fixnum> <string>)
					   (and <string> <fixnum>)))

  (check-for-false	(type-annotation=? (and <fixnum> <string>)
					   (and <fixnum> <symbol>)))
  (check-for-false	(type-annotation=? (and <fixnum> <string>)
					   (and <symbol> <fixnum>)))
  (check-for-false	(type-annotation=? (and <fixnum> <string>)
					   (and <fixnum> <string> <boolean>)))
  (check-for-false	(type-annotation=? (and <fixnum> <string> <boolean>)
					   (and <fixnum> <string>)))

  (check-for-true	(type-annotation=? (and <fixnum> <exact-integer>)
					   <fixnum>))
  (check-for-true	(type-annotation=? (and <number> <positive>)
					   (or <positive-fixnum> <positive-bignum> <positive-ratnum> <positive-flonum>)))
  (check-for-true	(type-annotation=? (and <number> <negative>)
					   (or <negative-fixnum> <negative-bignum> <negative-ratnum> <negative-flonum>)))

;;; --------------------------------------------------------------------
;;; complement

  (check-for-true	(type-annotation=? (not <fixnum>)
					   (not <fixnum>)))

  (check-for-false	(type-annotation=? (not <fixnum>)
					   (not <flonum>)))

;;; --------------------------------------------------------------------
;;; parent-of

  (check-for-true	(type-annotation=? <struct> (parent-of <record>)))
  (check-for-true	(type-annotation=? <fixnum> (parent-of <positive-fixnum>)))

  (check-for-false	(type-annotation=? <positive-fixnum> (parent-of <fixnum>)))

  (void))


(parametrise ((check-test-name	'type-annotation-super-and-sub-1))

  (check-for-true	(type-annotation-super-and-sub? <number> <fixnum>))
  (check-for-false	(type-annotation-super-and-sub? <number> <string>))

  (check
      (expansion-of (type-annotation-super-and-sub? <number> <fixnum>))
    => '(quote #t))

  (check
      (expansion-of (type-annotation-super-and-sub? <number> <string>))
    => '(quote #f))

  (check-for-true	(type-annotation-super-and-sub? <top> <number>))
  (check-for-false	(type-annotation-super-and-sub? <number> <top>))

  (internal-body
    (define-record-type alpha)

    (define-record-type beta
      (parent alpha))

    (define-record-type gamma
      (parent beta))

    (check-for-true	(type-annotation-super-and-sub? alpha beta))
    (check-for-false	(type-annotation-super-and-sub? beta alpha))

    (check-for-true	(type-annotation-super-and-sub? alpha gamma))
    (check-for-false	(type-annotation-super-and-sub? gamma alpha))

    (check-for-true	(type-annotation-super-and-sub? beta gamma))
    (check-for-false	(type-annotation-super-and-sub? gamma beta))

    (check-for-true	(type-annotation-super-and-sub? <top> alpha))
    (check-for-false	(type-annotation-super-and-sub? alpha <top>))

    (check-for-true	(type-annotation-super-and-sub? <top> beta))
    (check-for-false	(type-annotation-super-and-sub? beta <top>))

    (check-for-true	(type-annotation-super-and-sub? <top> gamma))
    (check-for-false	(type-annotation-super-and-sub? gamma <top>))

    (void))

;;; --------------------------------------------------------------------
;;; lists

  (check-for-true	(type-annotation-super-and-sub? <list-of-numbers> <list-of-reals>))
  (check-for-false	(type-annotation-super-and-sub? <list-of-numbers> <list-of-strings>))

  (check-for-true	(type-annotation-super-and-sub? (list <number>) (list <fixnum>)))
  (check-for-false	(type-annotation-super-and-sub? (list <symbol>) (list <string>)))

  (check-for-true	(type-annotation-super-and-sub? (list-of <number>) (list-of <fixnum>)))
  (check-for-false	(type-annotation-super-and-sub? (list-of <symbol>) (list-of <string>)))

  (check-for-true	(type-annotation-super-and-sub? (list-of <number>) (list <fixnum>)))
  (check-for-true	(type-annotation-super-and-sub? (list-of <number>) (list <fixnum> <flonum> <number>)))

  ;;This is false  because a LIST annotation  specifies the number of  items, while a
  ;;LIST-OF annotation does not specify it.
  (check-for-false	(type-annotation-super-and-sub? (list <number>) (list-of <number>)))

  (check-for-false	(type-annotation-super-and-sub? (list <number>) (list-of <string>)))

  ;;Tests for "<nelist>".
  (check-for-true	(type-annotation-super-and-sub? <nelist> <nelist>))
  (check-for-true	(type-annotation-super-and-sub? <list> <nelist>))
  (check-for-true	(type-annotation-super-and-sub? <nelist> (list <top>)))
  (check-for-false	(type-annotation-super-and-sub? <nelist> (list-of <top>)))
  (check-for-true	(type-annotation-super-and-sub? <nelist> (pair-of <null>)))
  (check-for-true	(type-annotation-super-and-sub? <nelist> (pair-of <list>)))
  (check-for-true	(type-annotation-super-and-sub? <nelist> (pair-of <nelist>)))
  (check-for-true	(type-annotation-super-and-sub? <nelist> (pair <top> <null>)))
  (check-for-true	(type-annotation-super-and-sub? <nelist> (pair <top> <list>)))
  (check-for-true	(type-annotation-super-and-sub? <nelist> (pair <top> (list-of <string>))))

;;; --------------------------------------------------------------------
;;; vectors

  (check-for-true	(type-annotation-super-and-sub? <vector-of-numbers> <vector-of-reals>))
  (check-for-false	(type-annotation-super-and-sub? <vector-of-numbers> <vector-of-strings>))

  (check-for-true	(type-annotation-super-and-sub? (vector <number>) (vector <fixnum>)))
  (check-for-false	(type-annotation-super-and-sub? (vector <symbol>) (vector <string>)))

  (check-for-true	(type-annotation-super-and-sub? (vector-of <number>) (vector-of <fixnum>)))
  (check-for-false	(type-annotation-super-and-sub? (vector-of <symbol>) (vector-of <string>)))

  (check-for-true	(type-annotation-super-and-sub? (vector-of <number>) (vector <fixnum>)))
  (check-for-true	(type-annotation-super-and-sub? (vector-of <number>) (vector <fixnum> <flonum> <number>)))

  ;;This is false  because a VECTOR annotation  specifies the number of  items, while a
  ;;VECTOR-OF annotation does not specify it.
  (check-for-false	(type-annotation-super-and-sub? (vector <number>) (vector-of <number>)))

  (check-for-false	(type-annotation-super-and-sub? (vector <number>) (vector-of <string>)))

  (check-for-true (type-annotation-super-and-sub? <vector> <nevector>))
  (check-for-true (type-annotation-super-and-sub? <vector> (vector <fixnum> <fixnum>)))
  (check-for-true (type-annotation-super-and-sub? <nevector> (vector <fixnum> <fixnum>)))

;;; --------------------------------------------------------------------
;;; pairs

  (check-for-true	(type-annotation-super-and-sub? <pair-of-numbers> <pair-of-reals>))
  (check-for-false	(type-annotation-super-and-sub? <pair-of-numbers> <pair-of-strings>))

  (check-for-true	(type-annotation-super-and-sub? (pair <number> <integer>) (pair <fixnum> <fixnum>)))
  (check-for-false	(type-annotation-super-and-sub? (pair <symbol> <keyword>) (pair <string> <keyword>)))
  (check-for-false	(type-annotation-super-and-sub? (pair <symbol> <keyword>) (pair <keyword> <string>)))
  (check-for-false	(type-annotation-super-and-sub? (pair <symbol> <keyword>) (pair <string> <string>)))

  (check-for-true	(type-annotation-super-and-sub? (pair-of <number>) (pair-of <fixnum>)))
  (check-for-false	(type-annotation-super-and-sub? (pair-of <symbol>) (pair-of <string>)))

  (check-for-true	(type-annotation-super-and-sub? (pair-of <number>) (pair <fixnum> <flonum>)))

  ;;This is true  because both a PAIR  annotation and a PAIR-OF  annotation specify a
  ;;pair, which holds two values.
  (check-for-true	(type-annotation-super-and-sub? (pair <number> <number>) (pair-of <number>)))

  (check-for-false	(type-annotation-super-and-sub? (pair <number> <number>) (pair-of <string>)))

  (check-for-false	(type-annotation-super-and-sub? (list <fixnum>) (pair-of (or <fixnum> <null>))))

  (check-for-true	(type-annotation-super-and-sub? (pair <fixnum> <null>)
							(list <fixnum>)))
  (check-for-true	(type-annotation-super-and-sub? (pair-of (or <fixnum> <null>))
							(list <fixnum>)))

  (check-for-true	(type-annotation-super-and-sub? (list-of <fixnum>) (list <fixnum>)))
  (check-for-true	(type-annotation-super-and-sub? (list-of <number>) (list <fixnum> <flonum>)))

  ;;Does not match because  a LIST annotation specifies the number  of items, while a
  ;;LIST-OF annotation does not specify it.
  (check-for-false	(type-annotation-super-and-sub? (list <fixnum> <fixnum>) (list-of <fixnum>)))

  ;;Does not match because a LIST-OF annotation does not specify the number of items,
  ;;while a PAIR annotation implies at least one item.
  (check-for-false	(type-annotation-super-and-sub? (pair <fixnum> (list-of <fixnum>))
							(list-of <fixnum>)))

  (check-for-true	(type-annotation-super-and-sub? (list-of <fixnum>) (pair <fixnum> <null>)))

  ;;Does not  match because the PAIR-OF  annotation implies at least  one item, while
  ;;the LIST-OF annotation implies nothing.
  (check-for-false	(type-annotation-super-and-sub? (list-of <fixnum>) (pair-of (or <fixnum> <null>))))

;;; --------------------------------------------------------------------
;;; unions

  (check-for-true	(type-annotation-super-and-sub? (or <number>) (or <number>)))
  (check-for-true	(type-annotation-super-and-sub? (or <number>) (or <fixnum>)))
  (check-for-false	(type-annotation-super-and-sub? (or <fixnum>) (or <number>)))

  (check-for-true	(type-annotation-super-and-sub? (or <number> <string>) (or <number> <string>)))
  (check-for-true	(type-annotation-super-and-sub? (or <number> <string>) (or <fixnum> <string>)))
  (check-for-true	(type-annotation-super-and-sub? (or <number> <vector>) (or <fixnum> (vector-of <symbol>))))

  (check-for-true	(type-annotation-super-and-sub? (or <number> <string>) (or <string> <number>)))
  (check-for-true	(type-annotation-super-and-sub? (or <number> <string>) (or <string> <fixnum>)))
  (check-for-true	(type-annotation-super-and-sub? (or <number> <vector>) (or (vector-of <symbol>) <fixnum>)))

  (check-for-true	(type-annotation-super-and-sub? (or <number> <string>)	<number>))
  (check-for-true	(type-annotation-super-and-sub? (or <number> <string>)	<fixnum>))
  (check-for-true	(type-annotation-super-and-sub? (or <number> <string>)	<string>))

  ;;True because all the types in the union are sub-types of the super-type.
  (check-for-true	(type-annotation-super-and-sub? <number>	(or <fixnum> <flonum>)))

  (check-for-false	(type-annotation-super-and-sub? <number>	(or <number> <string>)))
  (check-for-false	(type-annotation-super-and-sub? <string>	(or <number> <string>)))

;;; --------------------------------------------------------------------
;;; intersections

  (check-for-true	(type-annotation-super-and-sub? (and <fixnum> <exact>) <positive-fixnum>))
  (check-for-true	(type-annotation-super-and-sub? (and <fixnum> <exact>) <fixnum>))
  (check-for-false	(type-annotation-super-and-sub? (and <fixnum> <exact>) <exact>))
  (check-for-false	(type-annotation-super-and-sub? (and <fixnum> <exact>) <string>))

  (check-for-true	(type-annotation-super-and-sub? (and <positive> <exact>) <positive-fixnum>))
  (check-for-false	(type-annotation-super-and-sub? (and <positive> <exact>) <negative-fixnum>))
  (check-for-false	(type-annotation-super-and-sub? (and <positive> <exact>) <fixnum>))
  (check-for-false	(type-annotation-super-and-sub? (and <positive> <exact>) <string>))

  (check-for-true	(type-annotation-super-and-sub? (and <exact-integer> <positive>)
							(and <positive-fixnum> <positive-bignum>)))
  (check-for-false	(type-annotation-super-and-sub? (and <number> <positive>)
							(and <positive-fixnum> <negative-flonum>)))
  (check-for-false	(type-annotation-super-and-sub? (and <number> <positive>)
							(and <negative-fixnum> <positive-flonum>)))

;;; --------------------------------------------------------------------
;;; complement

  ;;If something is not a "<number>", for sure it is not a "<fixnum>".
  (check-for-true	(type-annotation-super-and-sub? (not <fixnum>) (not <number>)))

  ;;If something is not a "<fixnum>"
  (check-for-false	(type-annotation-super-and-sub? (not <number>) (not <fixnum>)))
  (check-for-false	(type-annotation-super-and-sub? (not <number>) (not <string>)))

  (check-for-true	(type-annotation-super-and-sub? (not <string>) <number>))
  (check-for-false	(type-annotation-super-and-sub? (not <number>) <number>))
  (check-for-false	(type-annotation-super-and-sub? (not <number>) <fixnum>))

  (check-for-false	(type-annotation-super-and-sub? <number> (not <string>)))

;;; --------------------------------------------------------------------
;;; ancestor-of

  (check-for-true	(type-annotation-super-and-sub? (ancestor-of <fixnum>) <exact-integer>))
  (check-for-false	(type-annotation-super-and-sub? (ancestor-of <fixnum>) <positive-fixnum>))
  (check-for-false	(type-annotation-super-and-sub? (ancestor-of <fixnum>) <fixnum>))

  (check-for-true	(type-annotation-super-and-sub? (ancestor-of &condition)
							<condition>))
  (check-for-true	(type-annotation-super-and-sub? (ancestor-of &condition)
							<record>))
  (check-for-true	(type-annotation-super-and-sub? (ancestor-of &condition)
							<struct>))
  (check-for-true	(type-annotation-super-and-sub? (ancestor-of &condition)
							<top>))
  (check-for-false	(type-annotation-super-and-sub? (ancestor-of &condition)
							<string>))

  (check-for-true	(type-annotation-super-and-sub? (not (ancestor-of &condition))
							<string>))

  (check-for-true	(type-annotation-super-and-sub? (not (ancestor-of <false>))
							<fixnum>))

  (check-for-false	(type-annotation-super-and-sub? (ancestor-of <false>)
							(or <false> <string>)))
  (check-for-true	(type-annotation-super-and-sub? (not (ancestor-of <false>))
							(or <false> <string>)))

;;; --------------------------------------------------------------------
;;; misc

  (check-for-true	(type-annotation-super-and-sub? (or <exact> <inexact>)	<fixnum>))

  (void))


(parametrise ((check-test-name	'type-annotation-super-and-sub-2))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(type-annotation-super-and-sub? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-false
	(type-annotation-super-and-sub? ?super ?sub)))
      ))

;;; --------------------------------------------------------------------

  (define-record-type <alpha>
    (define-type-descriptors)
    (fields a))

  (define-record-type <beta>
    (define-type-descriptors)
    (parent <alpha>)
    (fields b))

  (define-record-type <delta>
    (define-type-descriptors)
    (parent <beta>)
    (fields d))

  (define-record-type <duo>
    (define-type-descriptors)
    (fields one two))

  (define-struct alpha
    (a))

  (define-struct beta
    (b))

  (define alpha-rtd	(struct-type-descriptor alpha))
  (define beta-rtd	(struct-type-descriptor beta))

;;; --------------------------------------------------------------------
;;; core types as super-types

  (doit-true	<top>			<top>)
  (doit-true	<top>			<number>)
  (doit-false	<number>		<top>)

  (doit-false	<top>			<void>)
  (doit-true	<void>			<void>)
  (doit-false	<void>			<top>)

  (doit-true	<top>			<struct>)
  (doit-true	<top>			<record>)
  (doit-true	<struct>		<record>)

  (doit-true	<pair>			<pair>)
  (doit-false	<pair>			<list>)
  (doit-false	<pair>			<null>)
  (doit-true	<pair>			<nelist>)
  (doit-true	<pair>			(pair <fixnum> <flonum>))
  (doit-true	<pair>			(pair-of <fixnum>))
  (doit-true	<pair>			(list <fixnum> <flonum>))
  (doit-false	<pair>			(list-of <fixnum>))
  (doit-true	<pair>			(nelist-of <fixnum>))
  (doit-false	<pair>			(alist <fixnum> <flonum>))

  (doit-false	<list>			<pair>)
  (doit-true	<list>			<list>)
  (doit-true	<list>			<null>)
  (doit-true	<list>			<nelist>)
  (doit-false	<list>			(pair <fixnum> <flonum>))
  (doit-true	<list>			(pair <fixnum> <null>))
  (doit-false	<list>			(pair-of <fixnum>))
  (doit-true	<list>			(pair-of <null>))
  (doit-true	<list>			(list <fixnum> <flonum>))
  (doit-true	<list>			(list-of <fixnum>))
  (doit-true	<list>			(nelist-of <fixnum>))
  (doit-true	<list>			(alist <fixnum> <flonum>))

  (doit-false	<nelist>		<pair>)
  (doit-false	<nelist>		<list>)
  (doit-false	<nelist>		<null>)
  (doit-true	<nelist>		<nelist>)
  (doit-false	<nelist>		(pair <fixnum> <flonum>))
  (doit-true	<nelist>		(pair <fixnum> <null>))
  (doit-false	<nelist>		(pair-of <fixnum>))
  (doit-true	<nelist>		(pair-of <null>))
  (doit-true	<nelist>		(list <fixnum> <flonum>))
  (doit-false	<nelist>		(list-of <fixnum>))
  (doit-true	<nelist>		(nelist-of <fixnum>))
  (doit-false	<nelist>		(alist <fixnum> <flonum>))

  (doit-false	<null>			<pair>)
  (doit-false	<null>			<list>)
  (doit-true	<null>			<null>)
  (doit-false	<null>			<nelist>)
  (doit-false	<null>			(pair <fixnum> <flonum>))
  (doit-false	<null>			(pair <fixnum> <null>))
  (doit-false	<null>			(pair-of <fixnum>))
  (doit-false	<null>			(pair-of <null>))
  (doit-false	<null>			(list <fixnum> <flonum>))
  (doit-false	<null>			(list-of <fixnum>))
  (doit-false	<null>			(nelist-of <fixnum>))
  (doit-false	<null>			(alist <fixnum> <flonum>))

  (doit-true	<vector>		<vector>)
  (doit-true	<vector>		<nevector>)
  (doit-true	<vector>		<empty-vector>)
  (doit-true	<vector>		(vector-of <fixnum>))
  (doit-true	<vector>		(vector <fixnum> <flonum>))
  #;(doit-true	<vector>		(nevector-of <fixnum>))

  (doit-false	<nevector>		<vector>)
  (doit-true	<nevector>		<nevector>)
  (doit-false	<nevector>		<empty-vector>)
  (doit-false	<nevector>		(vector-of <fixnum>))
  (doit-true	<nevector>		(vector <fixnum> <flonum>))
  #;(doit-true	<nevector>		(nevector-of <fixnum>))

  (doit-false	<empty-vector>		<vector>)
  (doit-false	<empty-vector>		<nevector>)
  (doit-true	<empty-vector>		<empty-vector>)
  (doit-false	<empty-vector>		(vector-of <fixnum>))
  (doit-false	<empty-vector>		(vector <fixnum> <flonum>))
  #;(doit-false	<empty-vector>		(nevector-of <fixnum>))

  (doit-true	<symbol>		<symbol>)
  (doit-true	<symbol>		<gensym>)
  (doit-true	<symbol>		(enumeration ciao))
  (doit-true	<symbol>		(enumeration ciao hello))
  (doit-false	<symbol>		<string>)

  (doit-false	<gensym>		<symbol>)
  (doit-true	<gensym>		<gensym>)
  (doit-false	<gensym>		(enumeration ciao))
  (doit-false	<gensym>		(enumeration ciao hello))
  (doit-false	<symbol>		<string>)

  (doit-false	<struct>		<top>)
  (doit-true	<struct>		<struct>)
  (doit-true	<struct>		<record>)
  (doit-true	<struct>		alpha)
  (doit-true	<struct>		beta)
  (doit-true	<struct>		<alpha>)
  (doit-true	<struct>		<beta>)
  (doit-false	<struct>		<string>)
  (doit-true	<struct>		<condition>)
  (doit-true	<struct>		<compound-condition>)
  (doit-true	<struct>		&condition)
  (doit-true	<struct>		&who)
  (doit-true	<struct>		(condition &who))
  (doit-true	<struct>		(condition &who &irritants))
  (doit-true	<struct>		(condition &who (condition &irritants)))

  (doit-true	<hashtable>		<hashtable>)
  (doit-true	<hashtable>		(hashtable <fixnum> <flonum>))
  (doit-false	<hashtable>		<record>)
  (doit-false	<hashtable>		<struct>)

  (doit-false	<record>		<top>)
  (doit-false	<record>		<struct>)
  (doit-true	<record>		<record>)
  (doit-false	<record>		alpha)
  (doit-false	<record>		beta)
  (doit-true	<record>		<alpha>)
  (doit-true	<record>		<beta>)
  (doit-false	<record>		<string>)
  (doit-true	<record>		<condition>)
  (doit-true	<record>		<compound-condition>)
  (doit-true	<record>		&condition)
  (doit-true	<record>		&who)
  (doit-true	<record>		(condition &who))
  (doit-true	<record>		(condition &who &irritants))
  (doit-true	<record>		(condition &who (condition &irritants)))

  (doit-true	<condition>		<condition>)
  (doit-true	<condition>		<compound-condition>)
  (doit-true	<condition>		&condition)
  (doit-true	<condition>		&who)
  (doit-false	<condition>		<record>)
  (doit-false	<condition>		<struct>)
  (doit-true	<condition>		(condition &who))
  (doit-true	<condition>		(condition &who &irritants))
  (doit-true	<condition>		(condition &who (condition &irritants)))

  (doit-false	<compound-condition>	<condition>)
  (doit-true	<compound-condition>	<compound-condition>)
  (doit-false	<compound-condition>	&condition)
  (doit-false	<compound-condition>	&who)
  (doit-false	<compound-condition>	<record>)
  (doit-false	<compound-condition>	<struct>)
  (doit-false	<compound-condition>	(condition &who))
  (doit-true	<compound-condition>	(condition &who &irritants))
  (doit-true	<compound-condition>	(condition &who (condition &irritants)))

  (doit-true	<top>			(or <fixnum> <flonum>))
  (doit-false	<fixnum>		(or <fixnum> <flonum>))
  (doit-false	<flonum>		(or <fixnum> <flonum>))

  (doit-true	<top>			(and <positive-fixnum> <zero-fixnum>))
  (doit-true	<fixnum>		(and <positive-fixnum> <zero-fixnum>))
  (doit-false	<positive-fixnum>	(and <positive-fixnum> <zero-fixnum>))
  (doit-false	<zero-fixnum>		(and <positive-fixnum> <zero-fixnum>))
  (doit-false	<flonum>		(and <positive-fixnum> <zero-fixnum>))

  (doit-false	<top>			(not <top>))
  (doit-false	<top>			(not <fixnum>))
  (doit-false	<top>			(not <void>))
  ;;
  (doit-false	<void>			(not <top>))
  (doit-false	<void>			(not <void>))
  (doit-false	<void>			(not <fixnum>))
  ;;
  (doit-false	<fixnum>		(not <string>))

  (doit-false	<top>			(ancestor-of <top>))
  (begin
    (doit-true	<top>			(ancestor-of <fixnum>))
    (doit-true	<number>		(ancestor-of <fixnum>))
    (doit-true	<integer>		(ancestor-of <fixnum>))
    (doit-false	<fixnum>		(ancestor-of <fixnum>))
      ;;
    (doit-true	(ancestor-of <fixnum>)	<top>)
    (doit-true	(ancestor-of <fixnum>)	<number>)
    (doit-true	(ancestor-of <fixnum>)	<integer>)
    (doit-false	(ancestor-of <fixnum>)	<fixnum>))
  (doit-true	<fixnum>		(ancestor-of <positive-fixnum>))
  (doit-false	(ancestor-of <fixnum>)	<positive-fixnum>)
  (doit-false	<positive-fixnum>	(ancestor-of <fixnum>))

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit-true	<top>			alpha)
  (doit-true	<struct>		alpha)
  (doit-false	alpha			<top>)
  (doit-false	alpha			<struct>)
  (doit-false	alpha			beta)

  (doit-false	alpha			(or alpha <struct>))
  (doit-false	alpha			(or alpha beta))

  (doit-true	alpha			(and alpha <struct>))
  (doit-false	alpha			(and alpha beta))

  (doit-false	alpha			(not <top>))
  (doit-false	alpha			(not <fixnum>))
  (doit-false	alpha			(not alpha))
  (doit-false	alpha			(not beta))

  (doit-false	alpha			(ancestor-of <top>))
  (doit-false	alpha			(ancestor-of <struct>))
  (doit-false	alpha			(ancestor-of alpha))
  (doit-false	alpha			(ancestor-of beta))

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit-true	<top>			<duo>)
  (doit-true	<struct>		<duo>)
  (doit-true	<record>		<duo>)
  (doit-false	<duo>			<top>)
  (doit-false	<duo>			<struct>)
  (doit-false	<duo>			<record>)

  (doit-true	<alpha>			<beta>)
  (doit-true	<beta>			<delta>)
  (doit-true	<alpha>			<delta>)
  (doit-false	<beta>			<alpha>)
  (doit-false	<delta>			<beta>)
  (doit-false	<delta>			<alpha>)
  (doit-false	<alpha>			<duo>)
  (doit-false	<duo>			<alpha>)

;;; --------------------------------------------------------------------
;;; simple condition type descriptors

  (doit-true	<top>			&condition)
  (doit-true	<struct>		&condition)
  (doit-true	<record>		&condition)
  (doit-true	&condition		&condition)

  (doit-true	<top>			&who)
  (doit-true	<struct>		&who)
  (doit-true	<record>		&who)
  (doit-true	&condition		&who)

;;; --------------------------------------------------------------------
;;; pair

  (doit-true	<pair>			(pair <fixnum> <flonum>))

  (doit-false	(pair <top> <null>)	<list>)
  (doit-false	(pair <top> <list>)	<list>)
  (doit-false	(pair <top> <null>)	<nelist>)
  (doit-true	(pair <top> <list>)	<nelist>)

  (doit-false	(pair <fixnum> <list>)	<list>)
  (doit-false	(pair <fixnum> <list>)	<nelist>)

;;; pair/pair

  (doit-true	(pair <fixnum> <string>)
		(pair <fixnum> <string>))

  (doit-true	(pair <number> <string>)
		(pair <fixnum> <string>))

  (doit-true	(pair <string> <number>)
		(pair <string> <fixnum>))

  (doit-false	(pair <fixnum> <string>)
		(pair <number> <string>))

  (doit-false	(pair <string> <fixnum>)
		(pair <string> <number>))

  (doit-false	(pair <fixnum> <string>)
		(pair <symbol> <string>))

  (doit-false	(pair <fixnum> <string>)
		(pair <fixnum> <symbol>))

;;; pair/pair-of

  (doit-true	(pair    <fixnum> <number>)
		(pair-of <fixnum>))

  (doit-false	(pair    <string> <number>)
		(pair-of <fixnum>))

  (doit-false	(pair    <fixnum> <string>)
		(pair-of <fixnum>))

;;; pair/list

  (doit-true	(pair <fixnum> <null>)			(list <fixnum>))
  (doit-false	(pair <fixnum> <flonum>)		(list <fixnum>))
  (doit-true	(pair <fixnum> (list-of <fixnum>))	(list <fixnum>))
  (doit-false	(pair <fixnum> (list <fixnum>))		(list <fixnum>))
  (doit-true	(pair <fixnum> (list <fixnum>))		(list <fixnum> <fixnum>))

;;; pair/list-of

  (doit-false	(pair <fixnum> (list-of <fixnum>))
		(list-of <fixnum>))

  (doit-false	(pair <fixnum> (list <fixnum>))
		(list-of <fixnum>))

;;; --------------------------------------------------------------------
;;; pair-of

  (doit-true	<pair>			(pair-of <fixnum>))

;;;

  (doit-false	(pair-of <null>)	<list>)
  (doit-false	(pair-of <list>)	<list>)
  (doit-false	(pair-of <fixnum>)	<list>)

  (doit-false	(pair-of <null>)	<nelist>)
  (doit-false	(pair-of <list>)	<nelist>)
  (doit-false	(pair-of <fixnum>)	<nelist>)

  (doit-false	(pair-of <null>)	<null>)
  (doit-false	(pair-of <list>)	<null>)
  (doit-false	(pair-of <fixnum>)	<null>)

;;; pair-of/pair

  (doit-true	(pair-of <list>)		(pair <list> <list>))
  (doit-true	(pair-of (or <number> <list>))	(pair <fixnum> <list>))

;;; pair-of/pair-of

  (doit-true	(pair-of <number>)		(pair-of <fixnum>))
  (doit-false	(pair-of <number>)		(pair-of <string>))

;;; pair-of/list

  (doit-true	(pair-of <list>)		(list <list> <list>))
  (doit-true	(pair-of (or <number> <list>))	(list <fixnum> <list>))

;;; pair-of/list-of

  (doit-false	(pair-of <number>)		(list-of <fixnum>))
  (doit-false	(pair-of <number>)		(list-of <string>))

;;; --------------------------------------------------------------------
;;; list

  (doit-true	<list>			(list <fixnum>))
  (doit-true	<nelist>		(list <fixnum>))

  (doit-false	(list <top>)		<list>)
  (doit-false	(list <top>)		<nelist>)
  (doit-false	(list <top>)		<null>)

  (doit-false	(list <fixnum>)		<list>)
  (doit-false	(list <fixnum>)		<nelist>)
  (doit-false	(list <fixnum>)		<null>)

;;; list/pair

  (doit-true	(list <fixnum>)			(pair <fixnum> <null>))
  (doit-false	(list <fixnum>)			(pair <fixnum> <string>))
  (doit-false	(list <fixnum> <flonum>)	(pair <fixnum> <null>))

;;; list/pair-of

  (doit-true	(list <null>)			(pair-of <null>))
  (doit-true	(list <list>)			(pair-of <null>))
  (doit-false	(list <fixnum>)			(pair-of <list>))

  (doit-true	(list (list <flonum>) <flonum>)	(pair-of (list <flonum>)))

;;; list/list

  (doit-true	(list <fixnum> <flonum>)	(list <fixnum> <flonum>))
  (doit-true	(list <fixnum> <number>)	(list <fixnum> <flonum>))
  (doit-false	(list <fixnum> <flonum>)	(list <fixnum> <string>))

;;; list/list-of

  (doit-false	(list <fixnum>)			(list-of <fixnum>))

;;; --------------------------------------------------------------------
;;; list-of

  (doit-true	<list>			(list-of <fixnum>))
  (doit-false	<nelist>		(list-of <fixnum>))

  (doit-true	(list-of <top>)		<list>)
  (doit-true	(list-of <top>)		<nelist>)
  (doit-true	(list-of <top>)		<null>)

  (doit-false	(list-of <fixnum>)	<list>)
  (doit-false	(list-of <fixnum>)	<nelist>)
  (doit-true	(list-of <fixnum>)	<null>)

;;; list-of/pair

  (doit-true	(list-of <fixnum>)		(pair <fixnum> <null>))
  (doit-true	(list-of <fixnum>)		(pair <fixnum> (list-of <fixnum>)))
  (doit-true	(list-of <number>)		(pair <fixnum> (list <flonum>)))
  (doit-false	(list-of <number>)		(pair <fixnum> (list <string>)))
  (doit-false	(list-of <number>)		(pair <string> <null>))

;;; list-of/pair-of

  (doit-true	(list-of <null>)		(pair-of <null>))
  (doit-true	(list-of <list>)		(pair-of <list>))
  (doit-false	(list-of <fixnum>)		(pair-of <fixnum>))

;;; list-of/list

  (doit-true	(list-of <fixnum>)		(list <fixnum>))
  (doit-true	(list-of <number>)		(list <fixnum>))
  (doit-false	(list-of <fixnum>)		(list <number>))

;;; list-of/list-of

  (doit-true	(list-of <fixnum>)		(list-of <fixnum>))
  (doit-true	(list-of <number>)		(list-of <fixnum>))
  (doit-false	(list-of <fixnum>)		(list-of <number>))
  (doit-false	(list-of <string>)		(list-of <fixnum>))

;;; --------------------------------------------------------------------
;;; vector

  (doit-true	<vector>			(vector <fixnum>))
  (doit-true	<nevector>			(vector <fixnum>))
  (doit-false	(vector <top>)			<nevector>)
  (doit-false	(vector <fixnum>)		<nevector>)
  (doit-false	(vector <fixnum>)		<empty-vector>)

;;; vector/vector

  (doit-true	(vector <fixnum> <flonum>)	(vector <fixnum> <flonum>))
  (doit-true	(vector <fixnum> <number>)	(vector <fixnum> <flonum>))
  (doit-false	(vector <fixnum> <flonum>)	(vector <fixnum> <string>))

;;; vector/vector-of

  (doit-false	(vector <fixnum>)		(vector-of <fixnum>))

;;; --------------------------------------------------------------------
;;; vector-of

  (doit-true	<vector>			(vector-of <fixnum>))
  (doit-false	<nevector>			(vector-of <fixnum>))

  (doit-true	(vector-of <top>)		<vector>)
  (doit-true	(vector-of <top>)		<nevector>)
  (doit-true	(vector-of <true>)		<empty-vector>)

  (doit-false	(vector-of <fixnum>)		<vector>)
  (doit-false	(vector-of <fixnum>)		<nevector>)
  (doit-true	(vector-of <fixnum>)		<empty-vector>)

;;; vector-of/vector

  (doit-true	(vector-of <fixnum>)		(vector <fixnum>))
  (doit-true	(vector-of <number>)		(vector <fixnum>))
  (doit-false	(vector-of <fixnum>)		(vector <number>))

;;; vector-of/vector-of

  (doit-true	(vector-of <fixnum>)		(vector-of <fixnum>))
  (doit-true	(vector-of <number>)		(vector-of <fixnum>))
  (doit-false	(vector-of <fixnum>)		(vector-of <number>))
  (doit-false	(vector-of <string>)		(vector-of <fixnum>))

;;; --------------------------------------------------------------------
;;; compound condition-objects

  (doit-true	(condition &who)		(condition &who))
  (doit-true	(condition &condition)		(condition &who))
  (doit-true	(condition &condition)		(condition &who &irritants))
  (doit-false	(condition &who)		(condition &condition))
  (doit-true	(condition &who)		(condition &who &irritants))
  (doit-false	(condition &who &irritants)	(condition &who))
  (doit-true	(condition &who &irritants)	(condition &who &irritants))
  (doit-true	(condition &irritants &who)	(condition &who &irritants))

  (doit-true	(condition &who (condition &irritants))		(condition &who (condition &irritants)))
  (doit-true	(condition &irritants (condition &who))		(condition (condition &who) &irritants))

;;; --------------------------------------------------------------------
;;; enumeration

  (doit-true	<symbol>			(enumeration ciao))

  (doit-true	(enumeration ciao)		(enumeration ciao))
  (doit-false	(enumeration ciao)		(enumeration hello))
  (doit-true	(enumeration ciao hello)	(enumeration hello))
  (doit-false	(enumeration ciao)		(enumeration hello ciao))

;;; --------------------------------------------------------------------
;;; hashtable

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <fixnum>))

  (doit-false	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <string>))

  (doit-false	(hashtable <fixnum> <fixnum>)
		(hashtable <string> <fixnum>))

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <positive-fixnum> <fixnum>))

  (doit-true	(hashtable <fixnum> <fixnum>)
		(hashtable <fixnum> <positive-fixnum>))

;;; --------------------------------------------------------------------
;;; alist

  (doit-true	<list>					(alist <fixnum> <string>))
  (doit-true	(list-of <pair>)			(alist <fixnum> <string>))
  (doit-true	(list-of (pair <fixnum> <string>))	(alist <fixnum> <string>))

  (doit-true	(alist <fixnum> <fixnum>)	(alist <fixnum> <fixnum>))
  (doit-false	(alist <fixnum> <fixnum>)	(alist <fixnum> <string>))
  (doit-false	(alist <fixnum> <fixnum>)	(alist <string> <fixnum>))

;;; --------------------------------------------------------------------
;;; union

  (doit-true	(or <fixnum> <flonum>)		<fixnum>)
  (doit-true	(or <fixnum> <flonum>)		<flonum>)
  (doit-false	(or <fixnum> <flonum>)		<string>)

  (doit-true	(or <fixnum> <flonum>)		(or <fixnum> <flonum>))

;;; --------------------------------------------------------------------
;;; intersection

  (doit-true	(and <number> <flonum>)
		<flonum>)

  (doit-false	(and <number> <flonum>)
		<string>)

;;; --------------------------------------------------------------------
;;; complement

  (doit-false	(not <string>)		<top>)
  (doit-true	(not <flonum>)		<fixnum>)
  (doit-false	(not <flonum>)		<flonum>)
  (doit-false	(not <flonum>)		<positive-flonum>)

  (doit-false	<fixnum>		(not <flonum>))
  (doit-false	<fixnum>		(not <fixnum>))

  (begin
    (doit-false	(not <struct>)		(not <record>))
    (doit-true	(not <record>)		(not <struct>)))
  (begin
    (doit-false	(not <number>)		(not <fixnum>))
    (doit-true	(not <fixnum>)		(not <number>)))
  (begin
    (doit-false	(not <fixnum>)		(not <string>))
    (doit-false	(not <string>)		(not <fixnum>)))

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit-true	(ancestor-of <fixnum>)	(ancestor-of <exact-integer>))
  (doit-true	(ancestor-of <fixnum>)	(ancestor-of <fixnum>))
  (doit-false	(ancestor-of <fixnum>)	(ancestor-of <positive-fixnum>))

  (doit-false	(ancestor-of <fixnum>)			<fixnum>)
  (doit-true	(ancestor-of <fixnum>)			<exact-integer>)
  (doit-true	(ancestor-of <fixnum>)			<integer>)
  (doit-true	(ancestor-of <fixnum>)			<real>)
  (doit-true	(ancestor-of <fixnum>)			<complex>)
  (doit-true	(ancestor-of <fixnum>)			<number>)
  (doit-true	(ancestor-of <fixnum>)			<top>)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'type-annotation-super-and-sub-procedure))

  (define-syntax doit-true
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-true
	(type-annotation-super-and-sub? ?super ?sub)))
      ))

  (define-syntax doit-false
    (syntax-rules ()
      ((_ ?super ?sub)
       (check-for-false
	(type-annotation-super-and-sub? ?super ?sub)))
      ))

;;; --------------------------------------------------------------------

  (doit-true	<procedure>				<procedure>)
  (doit-true	<procedure>				(lambda <list> => (<fixnum>)))

  (doit-false	(lambda <list> => (<fixnum>))		<procedure>)
  (doit-false	(lambda <list> => (<fixnum>))		<fixnum>)

;;; --------------------------------------------------------------------

  (doit-true	(lambda <list> => (<fixnum>))		(lambda <list> => (<fixnum>)))
  (doit-true	(lambda <list> => (<number>))		(lambda <list> => (<fixnum>)))
  (doit-false	(lambda <list> => (<fixnum>))		(lambda <list> => (<number>)))
;;;

  (doit-true	(lambda (<fixnum>) => <list>)		(lambda (<fixnum>) => <list>))
  (doit-false	(lambda (<number>) => <list>)		(lambda (<fixnum>) => <list>))
  (doit-true	(lambda (<fixnum>) => <list>)		(lambda (<number>) => <list>))
;;;
  (doit-true	(lambda (<fixnum>) => (<fixnum>))		(lambda (<fixnum>) => (<fixnum>)))
  (doit-false	(lambda (<number>) => (<number>))		(lambda (<fixnum>) => (<fixnum>)))
  (doit-true	(lambda (<fixnum>) => (<number>))		(lambda (<number>) => (<fixnum>)))
  (doit-false	(lambda (<fixnum>) => (<fixnum>))		(lambda (<number>) => (<number>)))
  (doit-true	(lambda (<fixnum>) => (<number>))		(lambda (<number>) => (<number>)))
  (doit-false	(lambda (<number>) => (<fixnum>))		(lambda (<number>) => (<number>)))
;;;
  (doit-false	(lambda (<fixnum>) => (<string>))		(lambda (<fixnum>) => (<fixnum>)))
  (doit-false	(lambda (<string>) => (<fixnum>))		(lambda (<fixnum>) => (<fixnum>)))
  (doit-false	(lambda (<top>) => (<top>))			(lambda (<fixnum>) => (<pair>)))
  (doit-false	(lambda (<fixnum>) => (<pair>))		(lambda (<top>) => (<top>)))

;;; --------------------------------------------------------------------

  (doit-true <procedure>
	     (case-lambda
	       ((<fixnum>) => (<fixnum>))))

  (doit-false (case-lambda
		((<fixnum>) => (<fixnum>)))
	      <procedure>)

  (doit-true (lambda (<fixnum>) => (<string>))
	     (case-lambda
	       ((<fixnum>) => (<string>))
	       ((<flonum>) => (<string>))))

  (doit-true (lambda (<flonum>) => (<string>))
	     (case-lambda
	       ((<fixnum>) => (<string>))
	       ((<flonum>) => (<string>))))

;;;

  (doit-false (case-lambda
		((<fixnum>) => (<string>))
		((<flonum>) => (<string>)))
	      (lambda (<fixnum>) => (<string>)))

  (doit-false (case-lambda
		((<fixnum>) => (<string>))
		((<flonum>) => (<string>)))
	      (lambda (<flonum>) => (<string>)))

  (doit-false (case-lambda
		((<fixnum>) => (<string>))
		((<flonum>) => (<string>)))
	      (lambda (<string>) => (<fixnum>)))

  (doit-true (case-lambda
	       ((<fixnum>)	=> (<boolean>))
	       ((<vector>)	=> (<boolean>)))
	     (case-lambda
	       ((<fixnum>)	=> (<boolean>))
	       ((<flonum>)	=> (<boolean>))
	       ((<vector>)	=> (<boolean>))))

  (doit-false (case-lambda
		((<top>)		=> (<pair>))
		((<top>)		=> (<real>))
		((<top>)		=> (<vector>)))
	      (case-lambda
		((<top>)		=> (<fixnum>))
		((<top>)		=> (<flonum>))
		((<top>)		=> ((vector-of <true>)))))

  (doit-false (case-lambda
		((<top>)		=> (<pair>))
		((<top>)		=> (<real>))
		((<top>)		=> (<vector>)))
	      (case-lambda
		((<top>)		=> (<fixnum>))
		((<top>)		=> (<flonum>))
		((<top>)		=> (<transcoder>)) ;;this does not match
		((<top>)		=> ((vector-of <true>)))))

;;; --------------------------------------------------------------------
;;; special procedures types: <thunk>

  (doit-true <thunk>
	     (lambda () => <list>))

  (doit-true <thunk>
	     (case-lambda
	       (() => <list>)))

  (doit-true <thunk>
	     (case-lambda
	       (()		=> (<string>))
	       ((<fixnum>)	=> (<string>))))

  (doit-true <thunk>
	     (case-lambda
	       ((<fixnum>)	=> (<string>))
	       (()		=> (<string>))))

;;; --------------------------------------------------------------------
;;; special procedures types: <type-predicate>

  ;; <type-predicate> == (lambda (<top>) => (<boolean>))
  (doit-true	<type-predicate>		(lambda (<top>) => (<boolean>)))
  (doit-false	<type-predicate>		(lambda (<string>) => (<boolean>)))
  (doit-true	(lambda (<string>) => (<boolean>))	<type-predicate>)

  (doit-false	<type-predicate>		(lambda (<top>) => (<string>)))
  (doit-false	<type-predicate>		(lambda (<top>) => (<top>)))

  ;;Ugly but what can I do?
  (doit-true	<type-predicate>		(lambda (<top>) => (<true>)))
  (doit-true	<type-predicate>		(lambda (<top>) => (<false>)))

  (doit-true	<type-predicate>		(case-lambda
						  ((<top>)	=> (<boolean>))
						  ((<string>)	=> (<string>))))

  (doit-true	<type-predicate>		(case-lambda
						  ((<string> <number>)	=> (<string>))
						  ((<top>)		=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; special procedures types: <type-destructor>

  (doit-true <type-destructor>		(lambda (<top>) => (<boolean>)))
  (doit-true <type-destructor>		(lambda (<top>) => <list>))
  (doit-true <type-destructor>		(case-lambda
					  ((<top> <top>) => (<top>))
					  ((<top>)	=> <list>)))

;;; --------------------------------------------------------------------
;;; special procedures types: <type-printer>

  (doit-true	<type-printer>	(lambda (<top>    <textual-output-port> <procedure>) => <list>))
  (doit-true	<type-printer>	(lambda (<fixnum> <textual-output-port> <procedure>) => (<void>)))
  (doit-false	(lambda (<fixnum> <textual-output-port> <procedure>) => <list>)	<type-printer>)

;;; --------------------------------------------------------------------
;;; special procedures types: <equality-predicate>

  (doit-true	<equality-predicate>	(lambda (<top>    <top>)		=> (<boolean>)))
  (doit-true	<equality-predicate>	(lambda (<string> <string>)		=> (<boolean>)))
  (doit-false	<equality-predicate>	(lambda (<top>    <top>)		=> (<top>)))
  (doit-false	<equality-predicate>	(lambda (<top>    <top> <top>)	=> (<boolean>)))

  (doit-false	(lambda (<string> <string>) => (<boolean>))	<equality-predicate>)

;;; --------------------------------------------------------------------
;;; special procedures types: <comparison-procedure>

  (doit-true	<comparison-procedure>	(lambda (<top>    <top>)	=> (<fixnum>)))
  (doit-true	<comparison-procedure>	(lambda (<string> <string>)	=> (<fixnum>)))
  (doit-false	<comparison-procedure>	(lambda (<top>    <top>)	=> (<top>)))
  (doit-false	<comparison-procedure>	(lambda (<top> <top> <top>)	=> (<fixnum>)))

  (doit-false	(lambda (<string> <string>) => (<fixnum>))	<comparison-procedure>)

;;; --------------------------------------------------------------------
;;; special procedures types: <hash-function>

  (doit-true	<hash-function>		(lambda (<top>)		=> (<non-negative-fixnum>)))
  (doit-true	<hash-function>		(lambda (<string>)		=> (<non-negative-fixnum>)))
  (doit-false	<hash-function>		(lambda (<top>)		=> (<fixnum>)))
  (doit-false	<hash-function>		(lambda (<top> <top>)	=> (<non-negative-fixnum>)))

  (doit-false	(lambda (<string>) => (<non-negative-fixnum>))	<hash-function>)

;;; --------------------------------------------------------------------
;;; special procedures types: <type-method-retriever>

  (doit-true	<type-method-retriever>		(lambda (<symbol>)	=> (<procedure>)))
  (doit-true	<type-method-retriever>		(lambda (<top>)	=> (<procedure>)))

  (doit-false	<type-method-retriever>		(lambda (<string>)	=> (<procedure>)))
  (doit-false	<type-method-retriever>		(lambda (<symbol>)	=> (<number>)))

;;; --------------------------------------------------------------------
;;; <bottom> as not-returning type

  (doit-true	(lambda (<top>) => <bottom>)		(lambda (<top>) => <bottom>))
  (doit-false	(lambda (<top>) => <bottom>)		(lambda (<top>) => (<bottom>)))
  (doit-true	(lambda (<top>) => (<bottom>))	(lambda (<top>) => <bottom>))

  (begin
    (doit-false	(lambda (<top>) => <bottom>)			(lambda (<top>) => <list>))
    (doit-false	(lambda (<top>) => <bottom>)			(lambda (<top>) => <nelist>))
    (doit-false	(lambda (<top>) => <bottom>)			(lambda (<top>) => <null>))
    (doit-false	(lambda (<top>) => <bottom>)			(lambda (<top>) => (list <fixnum>)))
    (doit-false	(lambda (<top>) => <bottom>)			(lambda (<top>) => (list-of <fixnum>)))
    (doit-false	(lambda (<top>) => <bottom>)			(lambda (<top>) => (pair <fixnum> <list>)))
    (doit-false	(lambda (<top>) => <bottom>)			(lambda (<top>) => (pair-of <list>)))
    ;;
    (doit-true	(lambda (<top>) => <list>)			(lambda (<top>) => <bottom>))
    (doit-true	(lambda (<top>) => <nelist>)			(lambda (<top>) => <bottom>))
    (doit-true	(lambda (<top>) => <null>)			(lambda (<top>) => <bottom>))
    (doit-true	(lambda (<top>) => (list <fixnum>))		(lambda (<top>) => <bottom>))
    (doit-true	(lambda (<top>) => (list-of <fixnum>))	(lambda (<top>) => <bottom>))
    (doit-true	(lambda (<top>) => (pair <fixnum> <list>))	(lambda (<top>) => <bottom>))
    (doit-true	(lambda (<top>) => (pair-of <list>))		(lambda (<top>) => <bottom>)))

  (doit-false	(lambda (<top>) => <bottom>)		(lambda (<top>) => (<string>)))
  (doit-true	(lambda (<top>) => (<string>))	(lambda (<top>) => <bottom>))

  (doit-true	(lambda (<bottom>) => (<char>))	(lambda (<top> . <list>) => <bottom>))

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'type-signature-super-and-sub))

  (check-for-false	(type-signature-super-and-sub? (<top>) (<void>)))
  (check-for-false	(type-signature-super-and-sub? (<void>) (<top>)))

;;; --------------------------------------------------------------------

  (check-for-true	(type-signature-super-and-sub? (<number>) (<fixnum>)))
  (check-for-false	(type-signature-super-and-sub? (<number>) (<string>)))

  (check
      (expansion-of (type-signature-super-and-sub? (<number>) (<fixnum>)))
    => '(quote #t))

  (check
      (expansion-of (type-signature-super-and-sub? (<number>) (<string>)))
    => '(quote #f))

  (check-for-true	(type-signature-super-and-sub? (<top>) (<number>)))
  (check-for-false	(type-signature-super-and-sub? (<number>) (<top>)))

  (internal-body
    (define-record-type alpha)

    (define-record-type beta
      (parent alpha))

    (define-record-type gamma
      (parent beta))

    (check-for-true	(type-signature-super-and-sub? (alpha) (beta)))
    (check-for-false	(type-signature-super-and-sub? (beta) (alpha)))

    (check-for-true	(type-signature-super-and-sub? (alpha) (gamma)))
    (check-for-false	(type-signature-super-and-sub? (gamma) (alpha)))

    (check-for-true	(type-signature-super-and-sub? (beta) (gamma)))
    (check-for-false	(type-signature-super-and-sub? (gamma) (beta)))

    (check-for-true	(type-signature-super-and-sub? (<top>) (alpha)))
    (check-for-false	(type-signature-super-and-sub? (alpha) (<top>)))

    (check-for-true	(type-signature-super-and-sub? (<top>) (beta)))
    (check-for-false	(type-signature-super-and-sub? (beta) (<top>)))

    (check-for-true	(type-signature-super-and-sub? (<top>) (gamma)))
    (check-for-false	(type-signature-super-and-sub? (gamma) (<top>)))

    (void))

;;; --------------------------------------------------------------------
;;; proper lists

  (check-for-true	(type-signature-super-and-sub? (<number> <number>) (<fixnum> <fixnum>)))
  (check-for-false	(type-signature-super-and-sub? (<fixnum> <fixnum>) (<number> <number>)))
  (check-for-false	(type-signature-super-and-sub? (<number> <number>) (<string> <string>)))
  (check-for-false	(type-signature-super-and-sub? (<number> <fixnum>) (<fixnum> <number>)))
  (check-for-false	(type-signature-super-and-sub? (<fixnum> <number>) (<number> <fixnum>)))

  (check-for-true	(type-signature-super-and-sub? (<top> <top>) (<number> <number>)))
  (check-for-false	(type-signature-super-and-sub? (<top> <number>) (<number> <top>)))
  (check-for-false	(type-signature-super-and-sub? (<number> <top>) (<top> <number>)))
  (check-for-false	(type-signature-super-and-sub? (<number> <number>) (<top> <top>)))

  (check-for-false	(type-signature-super-and-sub? () (<top>)))
  (check-for-false	(type-signature-super-and-sub? (<top>) ()))
  (check-for-false	(type-signature-super-and-sub? () (<top> <top>)))
  (check-for-false	(type-signature-super-and-sub? (<top> <top>) ()))

  (check-for-false	(type-signature-super-and-sub? (<number> <number>) (<fixnum>)))
  (check-for-false	(type-signature-super-and-sub? (<number>) (<fixnum> <fixnum>)))

;;; --------------------------------------------------------------------
;;; standalone list identifiers

  (check-for-true	(type-signature-super-and-sub? <list-of-numbers> <list-of-reals>))
  (check-for-false	(type-signature-super-and-sub? <list-of-numbers> <list-of-strings>))

  (check-for-true	(type-signature-super-and-sub? <list> ()))
  (check-for-true	(type-signature-super-and-sub? <list> (<top> <top>)))
  (check-for-true	(type-signature-super-and-sub? <list> (<top> <top> . <list>)))

  (check-for-false	(type-signature-super-and-sub? <nelist> ()))
  (check-for-true	(type-signature-super-and-sub? <nelist> (<top>)))
  (check-for-true	(type-signature-super-and-sub? <nelist> (<top> <top>)))
  (check-for-true	(type-signature-super-and-sub? <nelist> (<top> <top> . <list>)))

;;; --------------------------------------------------------------------
;;; improper lists

  (check-for-true	(type-signature-super-and-sub? (<number> . <list-of-numbers>) (<real> . <list-of-reals>)))
  (check-for-false	(type-signature-super-and-sub? (<number> . <list-of-numbers>) (<number> . <list-of-strings>)))
  (check-for-false	(type-signature-super-and-sub? (<number> . <list-of-numbers>) (<string> . <list-of-numbers>)))
  (check-for-false	(type-signature-super-and-sub? (<fixnum> . <list-of-numbers>) (<number> . <list-of-numbers>)))

  (check-for-true	(type-signature-super-and-sub? (<number> <number> . <list>) (<fixnum> <real> . <list>)))

  (check-for-false	(type-signature-super-and-sub? (<number> <number> . <list>) (<fixnum>)))
  (check-for-true	(type-signature-super-and-sub? (<number> <number> . <list>) (<fixnum> <fixnum>)))

;;; --------------------------------------------------------------------
;;; list specs

  (check-for-true	(type-signature-super-and-sub? <list>		(list <top>)))
  (check-for-true	(type-signature-super-and-sub? <nelist>		(list <top>)))
  (check-for-true	(type-signature-super-and-sub? (list <top>)	(list <top>)))

  (check-for-true	(type-signature-super-and-sub? <list>		(list <fixnum>)))
  (check-for-true	(type-signature-super-and-sub? <nelist>		(list <fixnum>)))
  (check-for-true	(type-signature-super-and-sub? (list <fixnum>)	(list <fixnum>)))

  (check-for-true	(type-signature-super-and-sub? (list <top>)	(list <fixnum>)))
  (check-for-false	(type-signature-super-and-sub? (list <fixnum>)	(list <top>)))

  (check-for-true	(type-signature-super-and-sub? (list <number> <number>)
						       (list <fixnum> <flonum>)))

  (check-for-true	(type-signature-super-and-sub? (list-of <number>)
						       (list <fixnum> <flonum>)))

  (check-for-true	(type-signature-super-and-sub? (<top> <top> <top>)
						       (list <top> <top> <top>)))
  (check-for-true	(type-signature-super-and-sub? (<fixnum> <fixnum> <fixnum>)
						       (list <fixnum> <fixnum> <fixnum>)))

  (check-for-true	(type-signature-super-and-sub? (list <top> <top> <top>)
						       (<top> <top> <top>)))
  (check-for-true	(type-signature-super-and-sub? (list <fixnum> <fixnum> <fixnum>)
						       (<fixnum> <fixnum> <fixnum>)))

  (check-for-true	(type-signature-super-and-sub? (list <number> <number> <number>)
						       (<number> <number> <number>)))
  (check-for-true	(type-signature-super-and-sub? (list <fixnum> <fixnum> <fixnum>)
						       (<fixnum> <fixnum> <fixnum>)))

  (void))


(parametrise ((check-test-name	'type-annotation-matching-1))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?one ?two => ?expected)
       (check
	   (type-annotation-matching ?one ?two)
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------

  (doit <top> <void>	=> no-match)
  (doit <void> <top>	=> no-match)

  (doit <top> <fixnum>	=> exact-match)
  (doit <fixnum> <top>	=> possible-match)

;;; --------------------------------------------------------------------
;;; lists

  ;;Tests for "<list>".
  (doit <list>			<list>				=> exact-match)
  (doit <list>			<null>				=> exact-match)
  (doit <list>			<nelist>			=> exact-match)
  (doit <list>			(list <top>)			=> exact-match)
  (doit <list>			(list-of <top>)			=> exact-match)
  (doit <list>			(pair-of <null>)		=> exact-match)
  (doit	<list>			(pair-of <list>)		=> exact-match)
  (doit <list>			(pair-of <nelist>)		=> exact-match)
  (doit <list>			(pair <top> <null>)		=> exact-match)
  (doit <list>			(pair <top> <list>)		=> exact-match)
  (doit <list>			(pair <top> <nelist>)		=> exact-match)
  (doit <list>			(pair <top> (list <top>))	=> exact-match)
  (doit <list>			(pair <top> (list-of <string>))	=> exact-match)

  (doit <null>				<list>			=> possible-match)
  (doit <nelist>			<list>			=> possible-match)
  (doit (list <top>)			<list>			=> possible-match)
  (doit (list-of <top>)			<list>			=> exact-match)
  (doit (pair-of <null>)		<list>			=> possible-match)
  (doit (pair-of <list>)		<list>			=> possible-match)
  (doit (pair-of <nelist>)		<list>			=> possible-match)
  (doit (pair <top> <null>)		<list>			=> possible-match)
  (doit (pair <top> <list>)		<list>			=> possible-match)
  (doit (pair <top> <nelist>)		<list>			=> possible-match)
  (doit (pair <top> (list <top>))	<list>			=> possible-match)
  (doit (pair <top> (list-of <string>))	<list>			=> possible-match)

  ;;Tests for "<nelist>".
  (doit <nelist>		<nelist>			=> exact-match)
  (doit <nelist>		<list>				=> possible-match)
  (doit <nelist>		<null>				=> no-match)
  (doit <nelist>		(list <top>)			=> exact-match)
  (doit <nelist>		(list-of <top>)			=> possible-match)
  (doit <nelist>		(pair-of <null>)		=> exact-match)
  (doit	<nelist>		(pair-of <list>)		=> exact-match)
  (doit <nelist>		(pair-of <nelist>)		=> exact-match)
  (doit <nelist>		(pair <top> <null>)		=> exact-match)
  (doit <nelist>		(pair <top> <list>)		=> exact-match)
  (doit <nelist>		(pair <top> (list-of <string>))	=> exact-match)

  (doit <list>				<nelist>		=> exact-match)
  (doit <null>				<nelist>		=> no-match)
  (doit (list <top>)			<nelist>		=> possible-match)
  (doit (list-of <top>)			<nelist>		=> exact-match)
  (doit (pair-of <null>)		<nelist>		=> possible-match)
  (doit (pair-of <list>)		<nelist>		=> possible-match)
  (doit (pair-of <nelist>)		<nelist>		=> possible-match)
  (doit (pair <top> <null>)		<nelist>		=> possible-match)
  (doit (pair <top> <list>)		<nelist>		=> exact-match)
  (doit (pair <top> (list-of <string>))	<nelist>		=> possible-match)

  ;;tests for list specs
  (doit <list>				(list <top>)		=> exact-match)
  (doit <nelist>			(list <top>)		=> exact-match)
  (doit (list <top>)			(list <top>)		=> exact-match)

  (doit <list>				(list <fixnum>)		=> exact-match)
  (doit <nelist>			(list <fixnum>)		=> exact-match)
  (doit (list <fixnum>)			(list <fixnum>)		=> exact-match)

  (doit (list <top>)			(list <fixnum>)		=> exact-match)
  (doit (list <fixnum>)			(list <top>)		=> possible-match)
  (doit (list <fixnum>)			(list <number>)		=> possible-match)
  (doit (list <fixnum>)			(list <string>)		=> no-match)

  (doit (list <number> <number>)
	(list <fixnum> <flonum>)				=> exact-match)

  (doit (list-of <number>)
	(list <fixnum> <flonum>)				=> exact-match)

;;; --------------------------------------------------------------------
;;; type unions

  (doit (or <fixnum> <string>)	<fixnum>		=> exact-match)
  (doit (or <fixnum> <string>)	<string>		=> exact-match)
  (doit <fixnum>		(or <fixnum> <string>)	=> possible-match)
  (doit <string>		(or <fixnum> <string>)	=> possible-match)

  (doit <exact-integer> <fixnum>		=> exact-match)
  (doit <exact-integer> <bignum>		=> exact-match)
  (doit <exact-integer> <exact-integer>		=> exact-match)
  (doit <exact-integer> <ratnum>		=> no-match)
  (doit <fixnum>	<exact-integer>		=> possible-match)
  (doit <bignum>	<exact-integer>		=> possible-match)
  (doit <exact-integer> <exact-integer>		=> exact-match)
  (doit <ratnum>	<exact-integer>		=> no-match)

  (doit <exact>		<fixnum>		=> exact-match)
  (doit <exact>		<bignum>		=> exact-match)
  (doit <exact>		<exact-integer>		=> exact-match)
  (doit <exact>		<ratnum>		=> exact-match)
  (doit <fixnum>	<exact>			=> possible-match)
  (doit <bignum>	<exact>			=> possible-match)
  (doit <exact-integer> <exact>			=> possible-match)
  (doit <ratnum>	<exact>			=> possible-match)

;;; --------------------------------------------------------------------
;;; type complement

  ;;"<top>" is an ancestor of "<string>".
  (doit (not <string>)	<top>		=> possible-match)
  (doit (not <string>)	<string>	=> no-match)
;;;
  (doit (not <fixnum>)	<positive-fixnum>	=> no-match)
  (doit (not <fixnum>)	<fixnum>		=> no-match)
  ;;"<exact-integer>" is an ancestor of "<fixnum>".
  (doit (not <fixnum>)	<exact-integer>		=> possible-match)
  (doit (not <fixnum>)	<bignum>		=> exact-match)

;;; --------------------------------------------------------------------
;;; type intersection

  (doit (not <fixnum>)	<vector>		=> exact-match)
  (doit (not <fixnum>)	<exact-integer>		=> possible-match)
  (doit (not <string>)	<vector>		=> exact-match)
  (doit (not <string>)	<exact-integer>		=> exact-match)

  (begin
    (doit (and (not <fixnum>)
	       (not <string>))
	  <vector>
	  => exact-match)

    (doit (and (not <fixnum>)
	       (not <string>))
	  <fixnum>
	  => no-match)

    (doit (and (not <fixnum>)
	       (not <string>))
	  <string>
	  => no-match)

    (doit (and (not <fixnum>)
	       (not <string>))
	  <positive-fixnum>
	  => no-match)

    ;;"<exact-integer>" is an ancestor of "<fixnum>".
    ;;
    (doit (and (not <fixnum>)
	       (not <string>))
	  <exact-integer>
	  => possible-match)

    #| end of BEGIN |# )

  (internal-body
    (define-type <it>
      (and (not <fixnum>)
	   (not <string>)))
    (doit <it> <vector>			=> exact-match)
    (doit <it> <fixnum>			=> no-match)
    (doit <it> <string>			=> no-match)
    (doit <it> <positive-fixnum>	=> no-match)
    ;;"<exact-integer>" is an ancestor of "<fixnum>".
    (doit <it> <exact-integer>		=> possible-match)
    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit (ancestor-of &condition)	<condition>	=> exact-match)
  (doit (ancestor-of &condition)	<record>	=> exact-match)
  (doit (ancestor-of &condition)	<struct>	=> exact-match)
  (doit (ancestor-of &condition)	<top>		=> exact-match)
  (doit (ancestor-of &condition)	<fixnum>	=> no-match)
  (doit (ancestor-of &condition)	&condition	=> no-match)
  (doit (ancestor-of &condition)	&who		=> no-match)
  (doit (ancestor-of &condition)	(condition &who &message)	=> no-match)
  (doit (ancestor-of &who)		(condition &who &message)	=> no-match)

;;; complement of ancestor

  (doit (not (ancestor-of &condition))		&condition	=> exact-match)
  (doit (not (ancestor-of &condition))		<condition>	=> no-match)
  (doit (not (ancestor-of &condition))		<record>	=> no-match)
  (doit (not (ancestor-of &condition))		<struct>	=> no-match)
  (doit (not (ancestor-of &condition))		<top>		=> no-match)

;;;

  (doit <fixnum>			<zero>		=> possible-match)
  (doit (ancestor-of <fixnum>)		<zero>		=> no-match)
  (doit (not (ancestor-of <fixnum>))	<zero>		=> exact-match)

  (void))


(parametrise ((check-test-name	'type-annotation-matching-2))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?super ?sub => ?expected)
       (check
	   (type-annotation-matching ?super ?sub)
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------

  (define-record-type <alpha>
    (define-type-descriptors)
    (fields a))

  (define-record-type <beta>
    (define-type-descriptors)
    (parent <alpha>)
    (fields b))

  (define-record-type <delta>
    (define-type-descriptors)
    (parent <beta>)
    (fields d))

  (define-record-type <duo>
    (define-type-descriptors)
    (fields one two))

  (define-struct alpha
    (a))

  (define-struct beta
    (b))

  (define alpha-rtd	(struct-type-descriptor alpha))
  (define beta-rtd	(struct-type-descriptor beta))

;;; --------------------------------------------------------------------
;;; core types as super-types

  (doit	<top>			<top>				=> exact-match)
  (doit	<top>			<number>			=> exact-match)
  (doit	<number>		<top>				=> possible-match)

  (doit	<top>			<void>				=> no-match)
  (doit	<top>			<bottom>			=> exact-match)
  (doit	<void>			<void>				=> exact-match)
  (doit	<bottom>		<bottom>			=> exact-match)
  (doit	<void>			<top>				=> no-match)
  (doit	<bottom>		<top>				=> no-match)

  (doit	<top>			<struct>			=> exact-match)
  (doit	<top>			<record>			=> exact-match)
  (doit	<struct>		<record>			=> exact-match)

  (doit	<pair>			<pair>				=> exact-match)
  (doit	<pair>			<list>				=> possible-match)
  (doit	<pair>			<null>				=> no-match)
  (doit	<pair>			<nelist>			=> exact-match)
  (doit	<pair>			(pair <fixnum> <flonum>)	=> exact-match)
  (doit	<pair>			(pair-of <fixnum>)		=> exact-match)
  (doit	<pair>			(list <fixnum> <flonum>)	=> exact-match)
  (doit	<pair>			(list-of <fixnum>)		=> possible-match)
  (doit	<pair>			(nelist-of <fixnum>)		=> exact-match)
  (doit	<pair>			(alist <fixnum> <flonum>)	=> possible-match)

  (doit	<list>			<pair>				=> possible-match)
  (doit	<list>			<list>				=> exact-match)
  (doit	<list>			<null>				=> exact-match)
  (doit	<list>			<nelist>			=> exact-match)
  (doit	<list>			(pair <fixnum> <flonum>)	=> no-match)
  (doit	<list>			(pair <fixnum> <null>)		=> exact-match)
  (doit	<list>			(pair-of <fixnum>)		=> no-match)
  (doit	<list>			(pair-of <list>)		=> exact-match)
  (doit	<list>			(pair-of <null>)		=> exact-match)
  (doit	<list>			(pair-of <nelist>)		=> exact-match)
  (doit	<list>			(list <fixnum> <flonum>)	=> exact-match)
  (doit	<list>			(list-of <fixnum>)		=> exact-match)
  (doit	<list>			(nelist-of <fixnum>)		=> exact-match)
  (doit	<list>			(alist <fixnum> <flonum>)	=> exact-match)

  (doit	<nelist>		<pair>				=> possible-match)
  (doit	<nelist>		<list>				=> possible-match)
  (doit	<nelist>		<null>				=> no-match)
  (doit	<nelist>		<nelist>			=> exact-match)
  (doit	<nelist>		(pair <fixnum> <flonum>)	=> no-match)
  (doit	<nelist>		(pair <fixnum> <null>)		=> exact-match)
  (doit	<nelist>		(pair-of <fixnum>)		=> no-match)
  (doit	<nelist>		(pair-of <list>)		=> exact-match)
  (doit	<nelist>		(pair-of <null>)		=> exact-match)
  (doit	<nelist>		(pair-of <nelist>)		=> exact-match)
  (doit	<nelist>		(list <fixnum> <flonum>)	=> exact-match)
  (doit	<nelist>		(list-of <fixnum>)		=> possible-match)
  (doit	<nelist>		(nelist-of <fixnum>)		=> exact-match)
  (doit	<nelist>		(alist <fixnum> <flonum>)	=> possible-match)

  (doit	<null>			<pair>				=> no-match)
  (doit	<null>			<list>				=> possible-match)
  (doit	<null>			<null>				=> exact-match)
  (doit	<null>			<nelist>			=> no-match)
  (doit	<null>			(pair <fixnum> <flonum>)	=> no-match)
  (doit	<null>			(pair <fixnum> <null>)		=> no-match)
  (doit	<null>			(pair-of <fixnum>)		=> no-match)
  (doit	<null>			(pair-of <null>)		=> no-match)
  (doit	<null>			(list <fixnum> <flonum>)	=> no-match)
  (doit	<null>			(list-of <fixnum>)		=> possible-match)
  (doit	<null>			(nelist-of <fixnum>)		=> no-match)
  (doit	<null>			(alist <fixnum> <flonum>)	=> possible-match)

  (doit	<vector>		<vector>			=> exact-match)
  (doit	<vector>		<nevector>			=> exact-match)
  (doit	<vector>		<empty-vector>			=> exact-match)
  (doit	<vector>		(vector-of <fixnum>)		=> exact-match)
  (doit	<vector>		(vector <fixnum> <flonum>)	=> exact-match)
  #;(doit-true	<vector>		(nevector-of <fixnum>))

  (doit	<nevector>		<vector>			=> possible-match)
  (doit	<nevector>		<nevector>			=> exact-match)
  (doit	<nevector>		<empty-vector>			=> no-match)
  (doit	<nevector>		(vector-of <fixnum>)		=> possible-match)
  (doit	<nevector>		(vector <fixnum> <flonum>)	=> exact-match)
  #;(doit-true	<nevector>		(nevector-of <fixnum>))

  (doit	<empty-vector>		<vector>			=> possible-match)
  (doit	<empty-vector>		<nevector>			=> no-match)
  (doit	<empty-vector>		<empty-vector>			=> exact-match)
  (doit	<empty-vector>		(vector-of <fixnum>)		=> possible-match)
  (doit	<empty-vector>		(vector <fixnum> <flonum>)	=> no-match)
  #;(doit-false	<empty-vector>		(nevector-of <fixnum>))

  (doit	<symbol>		<symbol>			=> exact-match)
  (doit	<symbol>		<gensym>			=> exact-match)
  (doit	<symbol>		(enumeration ciao)		=> exact-match)
  (doit	<symbol>		(enumeration ciao hello)	=> exact-match)
  (doit	<symbol>		<string>			=> no-match)

  (doit	<gensym>		<symbol>			=> possible-match)
  (doit	<gensym>		<gensym>			=> exact-match)
  (doit	<gensym>		(enumeration ciao)		=> no-match)
  (doit	<gensym>		(enumeration ciao hello)	=> no-match)
  (doit	<symbol>		<string>			=> no-match)

  (doit	<struct>		<top>				=> possible-match)
  (doit	<struct>		<struct>			=> exact-match)
  (doit	<struct>		<record>			=> exact-match)
  (doit	<struct>		alpha				=> exact-match)
  (doit	<struct>		beta				=> exact-match)
  (doit	<struct>		<alpha>				=> exact-match)
  (doit	<struct>		<beta>				=> exact-match)
  (doit	<struct>		<string>			=> no-match)
  (doit	<struct>		<condition>			=> exact-match)
  (doit	<struct>		<compound-condition>		=> exact-match)
  (doit	<struct>		&condition			=> exact-match)
  (doit	<struct>		&who				=> exact-match)
  (doit	<struct>		(condition &who)		=> exact-match)
  (doit	<struct>		(condition &who &irritants)	=> exact-match)
  (doit	<struct>		(condition &who (condition &irritants))	=> exact-match)

  (doit	<hashtable>		<top>				=> possible-match)
  (doit	<hashtable>		<struct>			=> possible-match)
  (doit	<hashtable>		<record>			=> no-match)
  (doit	<hashtable>		<hashtable>			=> exact-match)
  (doit	<hashtable>		(hashtable <fixnum> <flonum>)	=> exact-match)

  (doit	<record>		<top>				=> possible-match)
  (doit	<record>		<struct>			=> possible-match)
  (doit	<record>		<record>			=> exact-match)
  (doit	<record>		alpha				=> no-match)
  (doit	<record>		beta				=> no-match)
  (doit	<record>		<alpha>				=> exact-match)
  (doit	<record>		<beta>				=> exact-match)
  (doit	<record>		<string>			=> no-match)
  (doit	<record>		<condition>			=> exact-match)
  (doit	<record>		<compound-condition>		=> exact-match)
  (doit	<record>		&condition			=> exact-match)
  (doit	<record>		&who				=> exact-match)
  (doit	<record>		(condition &who)		=> exact-match)
  (doit	<record>		(condition &who &irritants)	=> exact-match)
  (doit	<record>		(condition &who (condition &irritants))	=> exact-match)

  (doit	<condition>		<top>				=> possible-match)
  (doit	<condition>		<struct>			=> possible-match)
  (doit	<condition>		<record>			=> possible-match)
  (doit	<condition>		<condition>			=> exact-match)
  (doit	<condition>		<compound-condition>		=> exact-match)
  (doit	<condition>		&condition			=> exact-match)
  (doit	<condition>		&who				=> exact-match)
  (doit	<condition>		(condition &who)		=> exact-match)
  (doit	<condition>		(condition &who &irritants)	=> exact-match)
  (doit	<condition>		(condition &who (condition &irritants))	=> exact-match)

  (doit	<compound-condition>	<top>				=> possible-match)
  (doit	<compound-condition>	<struct>			=> possible-match)
  (doit	<compound-condition>	<record>			=> possible-match)
  (doit	<compound-condition>	<condition>			=> possible-match)
  (doit	<compound-condition>	<compound-condition>		=> exact-match)
  (doit	<compound-condition>	&condition			=> no-match)
  (doit	<compound-condition>	&who				=> no-match)
  (doit	<compound-condition>	(condition &who)		=> no-match)
  (doit	<compound-condition>	(condition &who &irritants)	=> exact-match)
  (doit	<compound-condition>	(condition &who (condition &irritants))	=> exact-match)

  (doit	<top>			(or <fixnum> <flonum>)		=> exact-match)
  (doit	<fixnum>		(or <fixnum> <flonum>)		=> possible-match)
  (doit	<flonum>		(or <fixnum> <flonum>)		=> possible-match)

  (doit	<top>			(and <positive-fixnum> <zero-fixnum>)	=> exact-match)
  (doit	<fixnum>		(and <positive-fixnum> <zero-fixnum>)	=> exact-match)
  (doit	<positive-fixnum>	(and <positive-fixnum> <zero-fixnum>)	=> no-match)
  (doit	<zero-fixnum>		(and <positive-fixnum> <zero-fixnum>)	=> no-match)
  (doit	<flonum>		(and <positive-fixnum> <zero-fixnum>)	=> no-match)

  (doit	<top>			(not <top>)		=> no-match)
  (doit	<top>			(not <fixnum>)		=> possible-match)
  (doit	<top>			(not <void>)		=> possible-match)
  (doit	<top>			(not <bottom>)		=> possible-match)
  (doit (not <fixnum>)		<top>			=> possible-match)
  (doit (not <top>)		<top>			=> no-match)
  (doit (not <void>)		<top>			=> possible-match)
  (doit (not <bottom>)		<top>			=> possible-match)
  ;;
  (doit	<void>			(not <bottom>)		=> possible-match)
  (doit	<void>			(not <top>)		=> possible-match)
  (doit	<void>			(not <void>)		=> no-match)
  (doit	<void>			(not <fixnum>)		=> possible-match)
  (doit (not <fixnum>)		<void>			=> exact-match)
  (doit (not <top>)		<void>			=> possible-match)
  (doit (not <void>)		<void>			=> no-match)
  (doit (not <bottom>)		<void>			=> possible-match)
  ;;
  (doit	<bottom>		(not <bottom>)		=> no-match)
  (doit	<bottom>		(not <void>)		=> no-match)
  (doit	<bottom>		(not <top>)		=> no-match)
  (doit	<bottom>		(not <fixnum>)		=> no-match)
  (doit (not <fixnum>)		<bottom>		=> exact-match)
  (doit (not <top>)		<bottom>		=> exact-match)
  (doit (not <void>)		<bottom>		=> exact-match)
  (doit (not <bottom>)		<bottom>		=> exact-match)
  ;;
  (doit	<top>			(not <fixnum>)		=> possible-match)
  (doit	<exact-integer>		(not <fixnum>)		=> possible-match)
  (doit	<fixnum>		(not <fixnum>)		=> no-match)
  (doit	<positive-fixnum>	(not <fixnum>)		=> possible-match)
  (doit	<bignum>		(not <fixnum>)		=> possible-match)
  (doit	<string>		(not <string>)		=> no-match)
  (doit	<fixnum>		(not <string>)		=> possible-match)
  (doit (not <fixnum>)		<fixnum>		=> no-match)
  (doit (not <fixnum>)		<positive-fixnum>	=> no-match)
  (doit (not <fixnum>)		<exact-integer>		=> possible-match)
  (doit (not <fixnum>)		<string>		=> exact-match)

  (doit	<top>			(ancestor-of <top>)		=> no-match)
  (begin
    (doit <top>			(ancestor-of <fixnum>)		=> exact-match)
    (doit <number>		(ancestor-of <fixnum>)		=> exact-match)
    (doit <integer>		(ancestor-of <fixnum>)		=> exact-match)
    (doit <exact-integer>	(ancestor-of <fixnum>)		=> exact-match)
    (doit <fixnum>		(ancestor-of <fixnum>)		=> no-match)
    (doit <positive-fixnum>	(ancestor-of <fixnum>)		=> no-match))
  (doit	<fixnum>		(ancestor-of <positive-fixnum>)	=> exact-match)
  (doit	<string>		(ancestor-of <positive-fixnum>)	=> no-match)
  (doit	<fixnum>		(ancestor-of <string>)		=> no-match)

;;; --------------------------------------------------------------------
;;; struct-type descriptors

  (doit	<top>			alpha			=> exact-match)
  (doit	<struct>		alpha			=> exact-match)
  (doit	alpha			<top>			=> possible-match)
  (doit	alpha			<struct>		=> possible-match)
  (doit	alpha			beta			=> no-match)

  (doit	alpha			(or alpha <struct>)	=> possible-match)
  (doit	alpha			(or alpha beta)		=> possible-match)
  (doit	(or alpha <struct>)	alpha			=> exact-match)
  (doit	(or alpha beta)		alpha			=> exact-match)

  (doit	alpha			(and alpha <struct>)	=> exact-match)
  (doit	alpha			(and alpha beta)	=> no-match)
  (doit (and alpha <struct>)	alpha			=> exact-match)
  (doit (and alpha beta)	alpha			=> no-match)

  (doit	alpha			(not <top>)		=> possible-match)
  (doit	alpha			(not <fixnum>)		=> possible-match)
  (doit	alpha			(not alpha)		=> no-match)
  (doit	alpha			(not beta)		=> possible-match)

  (doit (not <top>)		alpha			=> no-match)
  (doit (not alpha)		alpha			=> no-match)
  (doit (not <fixnum>)		alpha			=> exact-match)
  (doit (not beta)		alpha			=> exact-match)

  (doit	alpha			(ancestor-of <top>)	=> no-match)
  (doit	alpha			(ancestor-of <struct>)	=> no-match)
  (doit	alpha			(ancestor-of alpha)	=> no-match)
  (doit	alpha			(ancestor-of beta)	=> no-match)

  (doit (ancestor-of <top>)	alpha			=> no-match)
  (doit (ancestor-of <struct>)	alpha			=> no-match)
  (doit (ancestor-of alpha)	alpha			=> no-match)
  (doit (ancestor-of beta)	alpha			=> no-match)

;;; --------------------------------------------------------------------
;;; record-type descriptors

  (doit	<duo>			<top>			=> possible-match)
  (doit	<duo>			<struct>		=> possible-match)
  (doit	<duo>			<record>		=> possible-match)
  (doit	<top>			<duo>			=> exact-match)
  (doit	<struct>		<duo>			=> exact-match)
  (doit	<record>		<duo>			=> exact-match)

  (doit	<alpha>			<beta>			=> exact-match)
  (doit	<beta>			<delta>			=> exact-match)
  (doit	<alpha>			<delta>			=> exact-match)
  (doit	<beta>			<alpha>			=> possible-match)
  (doit	<delta>			<beta>			=> possible-match)
  (doit	<delta>			<alpha>			=> possible-match)
  (doit	<alpha>			<duo>			=> no-match)
  (doit	<duo>			<alpha>			=> no-match)

  (doit (not <top>)		<alpha>			=> no-match)
  (doit (not <alpha>)		<alpha>			=> no-match)
  (doit (not <beta>)		<alpha>			=> possible-match)
  (doit (not <duo>)		<alpha>			=> exact-match)
  (doit (not <fixnum>)		<alpha>			=> exact-match)

  (doit <alpha>			(not <top>)		=> possible-match)
  (doit <alpha>			(not <alpha>)		=> no-match)
  (doit <alpha>			(not <beta>)		=> possible-match)
  (doit <alpha>			(not <fixnum>)		=> possible-match)
  (doit <alpha>			(not <duo>)		=> possible-match)

  (begin
    (doit (not <alpha>)		<duo>			=> exact-match)
    (doit (not <alpha>)		<alpha>			=> no-match)
    (doit (not <alpha>)		<beta>			=> no-match)
    (doit (not <alpha>)		<delta>			=> no-match)
    ;;
    (doit (not <beta>)		<duo>			=> exact-match)
    (doit (not <beta>)		<alpha>			=> possible-match)
    (doit (not <beta>)		<beta>			=> no-match)
    (doit (not <beta>)		<delta>			=> no-match)
    ;;
    (doit (not <delta>)		<duo>			=> exact-match)
    (doit (not <delta>)		<alpha>			=> possible-match)
    (doit (not <delta>)		<beta>			=> possible-match)
    (doit (not <delta>)		<delta>			=> no-match))

  (begin
    (doit (ancestor-of <top>)		<alpha>			=> no-match)
    (doit (ancestor-of <struct>)	<alpha>			=> no-match)
    (doit (ancestor-of <record>)	<alpha>			=> no-match)
    (doit (ancestor-of <alpha>)		<alpha>			=> no-match)
    (doit (ancestor-of <beta>)		<alpha>			=> exact-match)
    (doit (ancestor-of <fixnum>)	<alpha>			=> no-match)
    ;;
    (doit <alpha>			(ancestor-of <top>)	=> no-match)
    (doit <alpha>			(ancestor-of <struct>)	=> no-match)
    (doit <alpha>			(ancestor-of <record>)	=> no-match)
    (doit <alpha>			(ancestor-of <alpha>)	=> no-match)
    (doit <alpha>			(ancestor-of <beta>)	=> exact-match)
    (doit <alpha>			(ancestor-of <fixnum>)	=> no-match))

  (begin
    (doit (ancestor-of <alpha>)	(ancestor-of <alpha>)	=> exact-match)
    (doit (ancestor-of <alpha>)	(ancestor-of <beta>)	=> possible-match)
    (doit (ancestor-of <alpha>)	(ancestor-of <delta>)	=> possible-match)
    (doit (ancestor-of <alpha>)	(ancestor-of <duo>)	=> no-match)
    ;;
    (doit (ancestor-of <beta>)	(ancestor-of <alpha>)	=> exact-match)
    (doit (ancestor-of <beta>)	(ancestor-of <beta>)	=> exact-match)
    (doit (ancestor-of <beta>)	(ancestor-of <delta>)	=> possible-match)
    (doit (ancestor-of <beta>)	(ancestor-of <duo>)	=> no-match)
    ;;
    (doit (ancestor-of <delta>)	(ancestor-of <alpha>)	=> exact-match)
    (doit (ancestor-of <delta>)	(ancestor-of <beta>)	=> exact-match)
    (doit (ancestor-of <delta>)	(ancestor-of <delta>)	=> exact-match)
    (doit (ancestor-of <delta>)	(ancestor-of <duo>)	=> no-match)
    ;;
    (doit (ancestor-of <duo>)	(ancestor-of <alpha>)	=> no-match)
    (doit (ancestor-of <duo>)	(ancestor-of <beta>)	=> no-match)
    (doit (ancestor-of <duo>)	(ancestor-of <delta>)	=> no-match)
    (doit (ancestor-of <duo>)	(ancestor-of <duo>)	=> exact-match))

;;; --------------------------------------------------------------------
;;; simple condition type descriptors

  (doit	&condition		<top>			=> possible-match)
  (doit	&condition		<struct>		=> possible-match)
  (doit	&condition		<record>		=> possible-match)
  (doit	&condition		<condition>		=> possible-match)
  (doit	&condition		<compound-condition>	=> no-match)

  (doit	<top>			&condition		=> exact-match)
  (doit	<struct>		&condition		=> exact-match)
  (doit	<record>		&condition		=> exact-match)
  (doit	<condition>		&condition		=> exact-match)
  (doit	<compound-condition>	&condition		=> no-match)
  (doit	&condition		&condition		=> exact-match)

  (doit	<top>			&who			=> exact-match)
  (doit	<struct>		&who			=> exact-match)
  (doit	<record>		&who			=> exact-match)
  (doit	<condition>		&who			=> exact-match)
  (doit	&condition		&who			=> exact-match)
  (doit	<compound-condition>	&who			=> no-match)

;;; --------------------------------------------------------------------
;;; pair

  (doit	<pair>			(pair <fixnum> <flonum>)	=> exact-match)

  (doit	(pair <top> <null>)	<list>			=> possible-match)
  (doit	(pair <top> <list>)	<list>			=> possible-match)
  (doit	(pair <top> <null>)	<nelist>		=> possible-match)
  (doit	(pair <top> <list>)	<nelist>		=> exact-match)

  (doit	(pair <fixnum> <list>)	<list>			=> possible-match)
  (doit	(pair <fixnum> <list>)	<nelist>		=> possible-match)

;;; pair/pair

  (doit	(pair <fixnum> <string>) (pair <fixnum> <string>)	=> exact-match)
  (doit	(pair <number> <string>) (pair <fixnum> <string>)	=> exact-match)

  (doit	(pair <string> <number>) (pair <string> <fixnum>)	=> exact-match)

  (doit	(pair <fixnum> <string>) (pair <number> <string>)	=> possible-match)

  (doit	(pair <string> <fixnum>) (pair <string> <number>)	=> possible-match)
  (doit	(pair <fixnum> <string>) (pair <symbol> <string>)	=> no-match)

  (doit	(pair <fixnum> <string>) (pair <fixnum> <symbol>)	=> no-match)

;;; pair/pair-of

  (doit	(pair <fixnum> <number>) (pair-of <fixnum>)		=> exact-match)
  (doit	(pair <string> <number>) (pair-of <fixnum>)		=> no-match)
  (doit	(pair <fixnum> <string>) (pair-of <fixnum>)		=> no-match)

;;; pair/list

  (doit	(pair <fixnum> <null>)			(list <fixnum>)		=> exact-match)
  (doit	(pair <fixnum> <flonum>)		(list <fixnum>)		=> no-match)
  (doit	(pair <fixnum> (list-of <fixnum>))	(list <fixnum>)		=> exact-match)
  (doit	(pair <fixnum> (list <fixnum>))		(list <fixnum>)		=> no-match)
  (doit	(pair <fixnum> (list <fixnum>))		(list <fixnum> <fixnum>) => exact-match)

;;; pair/list-of

  (doit	(pair <fixnum> (list-of <fixnum>))	(list-of <fixnum>)	=> possible-match)
  (doit	(pair <fixnum> (list <fixnum>))		(list-of <fixnum>)	=> possible-match)

;;; --------------------------------------------------------------------
;;; pair-of

  (doit	<pair>			(pair-of <fixnum>)	=> exact-match)

;;;

  (doit	(pair-of <null>)	<list>			=> possible-match)
  (doit	(pair-of <list>)	<list>			=> possible-match)
  (doit	(pair-of <fixnum>)	<list>			=> no-match)

  (doit	(pair-of <null>)	<nelist>		=> possible-match)
  (doit	(pair-of <list>)	<nelist>		=> possible-match)
  (doit	(pair-of <fixnum>)	<nelist>		=> no-match)

  (doit	(pair-of <null>)	<null>			=> no-match)
  (doit	(pair-of <list>)	<null>			=> no-match)
  (doit	(pair-of <fixnum>)	<null>			=> no-match)

;;; pair-of/pair

  (doit	(pair-of <list>)		(pair <list> <list>)	=> exact-match)
  (doit	(pair-of (or <number> <list>))	(pair <fixnum> <list>)	=> exact-match)

;;; pair-of/pair-of

  (doit	(pair-of <number>)		(pair-of <fixnum>)	=> exact-match)
  (doit	(pair-of <number>)		(pair-of <string>)	=> no-match)

;;; pair-of/list

  (doit	(pair-of <list>)		(list <list> <list>)	=> exact-match)
  (doit	(pair-of <list>)		(list <list> <fixnum>)	=> exact-match)
  (doit	(pair-of (or <number> <list>))	(list <fixnum> <list>)	=> exact-match)

;;; pair-of/list-of

  (doit	(pair-of <number>)		(list-of <fixnum>)	=> no-match)
  (doit	(pair-of <number>)		(list-of <string>)	=> no-match)
  (doit (pair-of <list>)		(list-of <list>)	=> possible-match)
  (doit (pair-of (or <number> <list>))	(list-of <fixnum>)	=> possible-match)

;;; --------------------------------------------------------------------
;;; list

  (doit	<list>			(list <fixnum>)		=> exact-match)
  (doit	<nelist>		(list <fixnum>)		=> exact-match)

  (doit	(list <top>)		<pair>			=> exact-match)
  (doit	(list <top>)		<list>			=> possible-match)
  (doit	(list <top>)		<nelist>		=> possible-match)
  (doit	(list <top>)		<null>			=> no-match)

  (doit	(list <fixnum>)		<list>			=> possible-match)
  (doit	(list <fixnum>)		<nelist>		=> possible-match)
  (doit	(list <fixnum>)		<null>			=> no-match)

;;; list/pair

  (doit	(list <fixnum>)			(pair <fixnum> <null>)		=> exact-match)
  (doit	(list <fixnum>)			(pair <fixnum> <string>)	=> no-match)
  (doit	(list <fixnum> <flonum>)	(pair <fixnum> <null>)		=> no-match)

;;; list/pair-of

  (doit	(list <null>)			(pair-of <null>)		=> exact-match)
  (doit	(list <list>)			(pair-of <null>)		=> exact-match)
  (doit	(list <fixnum>)			(pair-of <list>)		=> no-match)

  (doit	(list (list <flonum>) <flonum>)	(pair-of (list <flonum>))	=> exact-match)

;;; list/list

  (doit	(list <fixnum> <flonum>)	(list <fixnum> <flonum>)	=> exact-match)
  (doit	(list <fixnum> <number>)	(list <fixnum> <flonum>)	=> exact-match)
  (doit	(list <fixnum> <flonum>)	(list <fixnum> <string>)	=> no-match)

;;; list/list-of

  (doit	(list <fixnum>)			(list-of <top>)			=> possible-match)
  (doit	(list <fixnum>)			(list-of <exact-integer>)	=> possible-match)
  (doit	(list <fixnum>)			(list-of <fixnum>)		=> possible-match)
  (doit	(list <fixnum>)			(list-of <string>)		=> no-match)

;;; --------------------------------------------------------------------
;;; list-of

  (doit	<list>			(list-of <fixnum>)		=> exact-match)
  (doit	<nelist>		(list-of <fixnum>)		=> possible-match)

  (doit	(list-of <top>)		<list>				=> exact-match)
  (doit	(list-of <top>)		<nelist>			=> exact-match)
  (doit	(list-of <top>)		<null>				=> exact-match)

  (doit	(list-of <fixnum>)	<list>				=> possible-match)
  (doit	(list-of <fixnum>)	<nelist>			=> possible-match)
  (doit	(list-of <fixnum>)	<null>				=> exact-match)

;;; list-of/pair

  (doit	(list-of <fixnum>)		(pair <fixnum> <null>)			=> exact-match)
  (doit	(list-of <fixnum>)		(pair <fixnum> (list-of <fixnum>))	=> exact-match)
  (doit	(list-of <number>)		(pair <fixnum> (list <flonum>))		=> exact-match)
  (doit	(list-of <number>)		(pair <fixnum> (list <string>))		=> no-match)
  (doit	(list-of <number>)		(pair <string> <null>)			=> no-match)

;;; list-of/pair-of

  (doit	(list-of <null>)		(pair-of <null>)	=> exact-match)
  (doit	(list-of <list>)		(pair-of <list>)	=> exact-match)
  (doit	(list-of <fixnum>)		(pair-of <fixnum>)	=> no-match)

;;; list-of/list

  (doit	(list-of <fixnum>)		(list <fixnum>)		=> exact-match)
  (doit	(list-of <number>)		(list <fixnum>)		=> exact-match)
  (doit	(list-of <fixnum>)		(list <number>)		=> possible-match)

;;; list-of/list-of

  (doit	(list-of <fixnum>)		(list-of <fixnum>)	=> exact-match)
  (doit	(list-of <number>)		(list-of <fixnum>)	=> exact-match)
  (doit	(list-of <fixnum>)		(list-of <number>)	=> possible-match)
  (doit	(list-of <string>)		(list-of <fixnum>)	=> no-match)

;;; --------------------------------------------------------------------
;;; vector

  (doit	<vector>			(vector <fixnum>)	=> exact-match)
  (doit	<nevector>			(vector <fixnum>)	=> exact-match)
  (doit	(vector <top>)			<nevector>		=> possible-match)
  (doit	(vector <fixnum>)		<nevector>		=> possible-match)
  (doit	(vector <fixnum>)		<empty-vector>		=> no-match)

;;; vector/vector

  (doit	(vector <fixnum> <flonum>)	(vector <fixnum> <flonum>)	=> exact-match)
  (doit	(vector <fixnum> <number>)	(vector <fixnum> <flonum>)	=> exact-match)
  (doit	(vector <fixnum> <flonum>)	(vector <number> <number>)	=> possible-match)
  (doit	(vector <fixnum> <flonum>)	(vector <fixnum> <string>)	=> no-match)

;;; vector/vector-of

  (doit	(vector <fixnum>)		(vector-of <top>)		=> possible-match)
  (doit	(vector <fixnum>)		(vector-of <integer>)		=> possible-match)
  (doit	(vector <fixnum>)		(vector-of <fixnum>)		=> possible-match)
  (doit	(vector <fixnum>)		(vector-of <string>)		=> no-match)

;;; --------------------------------------------------------------------
;;; vector-of

  (doit	<vector>			(vector-of <fixnum>)	=> exact-match)
  (doit	<nevector>			(vector-of <fixnum>)	=> possible-match)
  (doit	<empty-vector>			(vector-of <fixnum>)	=> possible-match)

  (doit	(vector-of <top>)		<vector>		=> exact-match)
  (doit	(vector-of <top>)		<nevector>		=> exact-match)
  (doit	(vector-of <top>)		<empty-vector>		=> exact-match)

  (doit	(vector-of <fixnum>)		<vector>		=> possible-match)
  (doit	(vector-of <fixnum>)		<nevector>		=> possible-match)
  (doit	(vector-of <fixnum>)		<empty-vector>		=> exact-match)

;;; vector-of/vector

  (doit	(vector-of <fixnum>)		(vector <fixnum>)	=> exact-match)
  (doit	(vector-of <number>)		(vector <fixnum>)	=> exact-match)
  (doit	(vector-of <fixnum>)		(vector <number>)	=> possible-match)
  (doit	(vector-of <fixnum>)		(vector <string>)	=> no-match)

;;; vector-of/vector-of

  (doit	(vector-of <fixnum>)		(vector-of <fixnum>)	=> exact-match)
  (doit	(vector-of <number>)		(vector-of <fixnum>)	=> exact-match)
  (doit	(vector-of <fixnum>)		(vector-of <number>)	=> possible-match)
  (doit	(vector-of <string>)		(vector-of <fixnum>)	=> no-match)

;;; --------------------------------------------------------------------
;;; compound condition-objects

  (doit &who	(condition &who)					=> exact-match)
  (doit &who	(condition &who &message)				=> exact-match)
  (doit &who	(condition (condition &who) &message)			=> exact-match)
  (doit &who	(condition (condition (condition &who)) &message)	=> exact-match)
  (doit &who	(condition (condition (condition &who)))		=> exact-match)

  (doit	(condition &condition)		(condition &who)		=> exact-match)
  (doit	(condition &condition)		(condition &who &irritants)	=> exact-match)
  (doit	(condition &who)		(condition &who)		=> exact-match)
  (doit	(condition &who)		(condition &condition)		=> possible-match)
  (doit	(condition &who)		(condition &who &irritants)	=> exact-match)
  (doit	(condition &who &irritants)	(condition &who)		=> possible-match)
  (doit	(condition &who &irritants)	(condition &who &irritants)	=> exact-match)
  (doit	(condition &irritants &who)	(condition &who &irritants)	=> exact-match)
  (doit	(condition &who &irritants)	(condition &who &message)	=> no-match)

  (doit	(condition &who (condition &irritants))		(condition &who (condition &irritants))	      => exact-match)
  (doit	(condition &irritants (condition &who))		(condition (condition &who) &irritants)	      => exact-match)

;;; --------------------------------------------------------------------
;;; enumeration

  (doit	<symbol>			(enumeration ciao)		=> exact-match)
  (doit	(enumeration ciao)		<symbol>			=> possible-match)

  (doit	(enumeration ciao)		(enumeration ciao)		=> exact-match)
  (doit	(enumeration ciao)		(enumeration hello)		=> no-match)
  (doit	(enumeration ciao hello)	(enumeration hello)		=> exact-match)
  (doit	(enumeration ciao)		(enumeration hello ciao)	=> possible-match)

;;; --------------------------------------------------------------------
;;; hashtable

  (doit <top>		(hashtable <fixnum> <flonum>)		=> exact-match)
  (doit <struct>	(hashtable <fixnum> <flonum>)		=> exact-match)
  (doit <record>	(hashtable <fixnum> <flonum>)		=> no-match)
  (doit (hashtable <fixnum> <flonum>)		<top>		=> possible-match)
  (doit (hashtable <fixnum> <flonum>)		<struct>	=> possible-match)
  (doit (hashtable <fixnum> <flonum>)		<record>	=> no-match)

  (doit	(hashtable <fixnum> <fixnum>) (hashtable <fixnum> <fixnum>)		=> exact-match)
  (doit	(hashtable <fixnum> <fixnum>) (hashtable <fixnum> <string>)		=> no-match)
  (doit	(hashtable <fixnum> <fixnum>) (hashtable <string> <fixnum>)		=> no-match)
  (doit	(hashtable <fixnum> <fixnum>) (hashtable <positive-fixnum> <fixnum>)	=> exact-match)
  (doit	(hashtable <fixnum> <fixnum>) (hashtable <fixnum> <positive-fixnum>)	=> exact-match)

;;; --------------------------------------------------------------------
;;; procedures

  (begin
    (doit <top>			(lambda (<fixnum>) => (<string>))	=> exact-match)
    (doit <procedure>		(lambda (<fixnum>) => (<string>))	=> exact-match)
    ;;
    (doit (lambda (<fixnum>) => (<string>))		<top>		=> possible-match)
    (doit (lambda (<fixnum>) => (<string>))		<procedure>	=> possible-match))

  (begin
    (doit (lambda (<number>) => (<string>))	(lambda (<fixnum>) => (<string>))	=> no-match)
    (doit (lambda (<fixnum>) => (<string>))	(lambda (<number>) => (<string>))	=> exact-match)
    (doit (lambda (<symbol>) => (<string>))	(lambda (<number>) => (<string>))	=> no-match)
    ;;
    (doit (lambda (<string>) => (<number>))	(lambda (<string>) => (<fixnum>))	=> exact-match)
    (doit (lambda (<string>) => (<fixnum>))	(lambda (<string>) => (<number>))	=> no-match)
    (doit (lambda (<string>) => (<symbol>))	(lambda (<string>) => (<number>))	=> no-match))

  (doit (lambda (<number>) => (<string>))
	(case-lambda
	  ((<number>) => (<string>))
	  ((<symbol>) => (<string>)))
	=> exact-match)

  (doit (lambda (<fixnum>) => (<string>))
	(case-lambda
	  ((<number>) => (<string>))
	  ((<symbol>) => (<string>)))
	=> exact-match)

  (doit (lambda (<number>) => (<string>))
	(case-lambda
	  ((<fixnum>) => (<string>))
	  ((<symbol>) => (<string>)))
	=> no-match)

  (doit (case-lambda
	  ((<number>) => (<string>))
	  ((<symbol>) => (<string>)))
	(lambda (<number>) => (<string>))
	=> no-match)

  (doit <type-predicate>	(lambda (<bottom>)	 => (<boolean>))	=> no-match)
  (doit <type-predicate>	(lambda (<top>)	 => (<boolean>))	=> exact-match)
  (doit <type-predicate>	(lambda (<string>)	 => (<boolean>))	=> no-match)

  (doit (lambda (<bottom>)	=> (<boolean>))		<type-predicate>	=> exact-match)
  (doit (lambda (<top>)	=> (<boolean>))		<type-predicate>	=> exact-match)
  (doit (lambda (<string>)	=> (<boolean>))		<type-predicate>	=> exact-match)

  (doit (or <false> (lambda (<symbol>) => ((or <false> <procedure>))))
	(lambda (<symbol>) => ((or <false> <procedure>)))		=> exact-match)

  (doit (lambda (<symbol>) => ((or <false> <procedure>)))
	(lambda (<symbol>) => ((or <false> <procedure>)))		=> exact-match)

  (doit (or <false> <procedure>)	(or <false> <procedure>)	=> exact-match)

  (doit  (lambda (<fixnum> <fixnum>) => (<boolean>))
	 (case-lambda
	   ((<fixnum>)					=> (<boolean>))
	   ((<fixnum> <fixnum>)				=> (<boolean>))
	   ((<fixnum> <fixnum> . (list-of <fixnum>))	=> (<boolean>)))
	 => exact-match)

;;; --------------------------------------------------------------------
;;; alist

  (doit	<top>		(alist <fixnum> <fixnum>)	=> exact-match)
  (doit	<list>		(alist <fixnum> <fixnum>)	=> exact-match)
  (doit	<null>		(alist <fixnum> <fixnum>)	=> possible-match)
  (doit	<nelist>	(alist <fixnum> <fixnum>)	=> possible-match)

  (begin
    (doit <list>				(alist <fixnum> <string>)	=> exact-match)
    (doit (list-of <pair>)			(alist <fixnum> <string>)	=> exact-match)
    (doit (list-of (pair <fixnum> <string>))	(alist <fixnum> <string>)	=> exact-match)
    (doit (list <pair>)				(alist <fixnum> <string>)	=> possible-match)
    (doit (list (pair <fixnum> <string>))	(alist <fixnum> <string>)	=> possible-match)
    (doit (pair <pair> <list>)			(alist <fixnum> <string>)	=> possible-match)
    (doit (pair-of <pair>)			(alist <fixnum> <string>)	=> possible-match)
    ;;
    (doit (alist <fixnum> <string>)	<list>					=> possible-match)
    (doit (alist <fixnum> <string>)	(list-of <pair>)			=> possible-match)
    (doit (alist <fixnum> <string>)	(list-of (pair <fixnum> <string>))	=> exact-match)
    (doit (alist <fixnum> <string>)	(list (pair <fixnum> <string>))		=> exact-match)
    (doit (alist <fixnum> <string>)	(list <pair>)				=> possible-match)
    (doit (alist <fixnum> <string>)	(pair <pair> <list>)			=> possible-match)
    (doit (alist <fixnum> <string>)	(pair-of <pair>)			=> possible-match)
    #| end of BEGIN |# )

  (doit	(alist <fixnum> <fixnum>)	<top>		=> possible-match)
  (doit	(alist <fixnum> <fixnum>)	<list>		=> possible-match)
  (doit	(alist <fixnum> <fixnum>)	<null>		=> exact-match)
  (doit	(alist <fixnum> <fixnum>)	<nelist>	=> possible-match)

  (doit	(alist <fixnum> <fixnum>) (alist <fixnum> <fixnum>)	=> exact-match)
  (doit	(alist <fixnum> <fixnum>) (alist <fixnum> <string>)	=> no-match)
  (doit	(alist <fixnum> <fixnum>) (alist <string> <fixnum>)	=> no-match)

;;; --------------------------------------------------------------------
;;; union

  (doit	(or <fixnum> <flonum>)	<fixnum>	=> exact-match)
  (doit	(or <fixnum> <flonum>)	<flonum>	=> exact-match)
  (doit	(or <fixnum> <flonum>)	<string>	=> no-match)

  (doit	(or <fixnum> <flonum>)	(or <fixnum> <flonum>)		=> exact-match)
  (doit	(or <number> <string>)	(and <fixnum> <positive>)	=> exact-match)

  (doit <zero>			<non-positive>			=> possible-match)
  (doit <positive>		<non-positive>			=> no-match)
  (doit <negative>		<non-positive>			=> possible-match)
  (doit <non-positive>		<non-positive>			=> exact-match)
  (doit <non-negative>		<non-positive>			=> possible-match)
  (doit <number>		<non-positive>			=> exact-match)

;;; --------------------------------------------------------------------
;;; intersection

  (doit	(and <number> <flonum>)	<flonum>	=> exact-match)
  (doit	(and <number> <flonum>)	<string>	=> no-match)

  (doit (and (not <fixnum>) (not <string>))	<vector>	=> exact-match)

;;; --------------------------------------------------------------------
;;; complement

  (doit	(not <string>)		<top>			=> possible-match)

  (doit	(not <flonum>)		<fixnum>		=> exact-match)
  (doit	(not <flonum>)		<flonum>		=> no-match)
  (doit	(not <flonum>)		<positive-flonum>	=> no-match)

  (doit	<fixnum>		(not <flonum>)		=> possible-match)
  (doit	<fixnum>		(not <fixnum>)		=> no-match)

  (doit (not <number>)		(not <fixnum>)		=> possible-match)
  (doit (not <fixnum>)		(not <number>)		=> exact-match)
  (doit (not <string>)		(not <number>)		=> no-match)

  (doit	(not (ancestor-of <fixnum>))	<fixnum>		=> exact-match)
  (doit	(not (ancestor-of <fixnum>))	<positive-fixnum>	=> exact-match)
  (doit	(not (ancestor-of <fixnum>))	<bignum>		=> exact-match)
  (doit	(not (ancestor-of <fixnum>))	<exact-integer>		=> no-match)
  (doit	(not (ancestor-of <fixnum>))	<number>		=> no-match)
  (doit	(not (ancestor-of <fixnum>))	<top>			=> no-match)

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit	(ancestor-of <fixnum>)	(ancestor-of <fixnum>)		=> exact-match)
  (doit	(ancestor-of <fixnum>)	(ancestor-of <positive-fixnum>)	=> possible-match)

  (doit	(ancestor-of <fixnum>)			<fixnum>	=> no-match)
  (doit	(ancestor-of <fixnum>)			<exact-integer>	=> exact-match)
  (doit	(ancestor-of <fixnum>)			<integer>	=> exact-match)
  (doit	(ancestor-of <fixnum>)			<real>		=> exact-match)
  (doit	(ancestor-of <fixnum>)			<complex>	=> exact-match)
  (doit	(ancestor-of <fixnum>)			<number>	=> exact-match)
  (doit	(ancestor-of <fixnum>)			<top>		=> exact-match)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'type-signature-matching))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?one ?two => ?expected)
       (check
	   (type-signature-matching ?one ?two)
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------

  (doit (<top>) (<void>)	=> no-match)
  (doit (<void>) (<top>)	=> no-match)

  (doit (<top>) (<fixnum>)	=> exact-match)
  (doit (<fixnum>) (<top>)	=> possible-match)

;;; --------------------------------------------------------------------
;;; type unions

  (doit ((or <fixnum> <string>))
	(<fixnum>)
	=> exact-match)

  (doit ((or <fixnum> <string>))
	(<string>)
	=> exact-match)

  (doit (<fixnum>)
	((or <fixnum> <string>))
	=> possible-match)

  (doit (<string>)
	((or <fixnum> <string>))
	=> possible-match)

;;; --------------------------------------------------------------------
;;; type complement

  ;;"<top>" is an ancestor of "<string>".
  ;;
  (doit ((not <string>))
	(<top>)
	=> possible-match)

  (doit ((not <string>))
	(<string>)
	=> no-match)

;;;

  (doit ((not <fixnum>))
	(<positive-fixnum>)
	=> no-match)

  (doit ((not <fixnum>))
	(<fixnum>)
	=> no-match)

  ;;"<exact-integer>" is an ancestor of "<fixnum>".
  ;;
  (doit ((not <fixnum>))
	(<exact-integer>)
	=> possible-match)

  (doit ((not <fixnum>))
	(<bignum>)
	=> exact-match)

;;; --------------------------------------------------------------------
;;; type intersection

  (doit ((not <fixnum>))
	(<vector>)
	=> exact-match)

  (doit ((not <fixnum>))
	(<exact-integer>)
	=> possible-match)

  (doit ((not <string>))
	(<vector>)
	=> exact-match)

  (doit ((not <string>))
	(<exact-integer>)
	=> exact-match)

  (begin
    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<vector>)
	  => exact-match)

    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<fixnum>)
	  => no-match)

    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<string>)
	  => no-match)

    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<positive-fixnum>)
	  => no-match)

    ;;"<exact-integer>" is an ancestor of "<fixnum>".
    ;;
    (doit ((and (not <fixnum>)
		(not <string>)))
	  (<exact-integer>)
	  => possible-match)
    #| end of BEGIN |# )

  (internal-body
    (define-type <it>
      (and (not <fixnum>)
	   (not <string>)))

    (doit (<it>)
	  (<vector>)
	  => exact-match)

    (doit (<it>)
	  (<fixnum>)
	  => no-match)

    (doit (<it>)
	  (<string>)
	  => no-match)

    (doit (<it>)
	  (<positive-fixnum>)
	  => no-match)

    ;;"<exact-integer>" is an ancestor of "<fixnum>".
    ;;
    (doit (<it>)
	  (<exact-integer>)
	  => possible-match)
    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------
;;; ancestor-of

  (doit ((ancestor-of &condition))
	(<condition>)
	=> exact-match)

  (doit ((ancestor-of &condition))
	(<record>)
	=> exact-match)

  (doit ((ancestor-of &condition))
	(<struct>)
	=> exact-match)

  (doit ((ancestor-of &condition))
	(<top>)
	=> exact-match)

  (doit ((ancestor-of &condition))
	(<fixnum>)
	=> no-match)

  (doit ((ancestor-of &condition))
	(&condition)
	=> no-match)

  (doit ((ancestor-of &condition))
	(&who)
	=> no-match)

  (doit ((ancestor-of &condition))
	((condition &who &message))
	=> no-match)

  (doit ((ancestor-of &who))
	((condition &who &message))
	=> no-match)

;;; complement

  (doit ((not (ancestor-of &condition)))
	(&condition)
	=> exact-match)

  (doit ((not (ancestor-of &condition)))
	(<condition>)
	=> no-match)

  (doit ((not (ancestor-of &condition)))
	(<record>)
	=> no-match)

  (doit ((not (ancestor-of &condition)))
	(<struct>)
	=> no-match)

  (doit ((not (ancestor-of &condition)))
	(<top>)
	=> no-match)

;;;

  (doit (<fixnum>)
	(<zero>)
	=> possible-match)
  (doit ((ancestor-of <fixnum>))
	(<zero>)
	=> no-match)

  (doit ((not (ancestor-of <fixnum>)))
	(<zero>)
	=> exact-match)

  (void))


(parametrise ((check-test-name	'type-annotation-common-ancestor))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?one ?two => ?expected)
       (check
	   (type-annotation-common-ancestor ?one ?two)
	 (=> expander::syntax=?)
	 (syntax ?expected)))
      ))

;;; --------------------------------------------------------------------

  (doit <top> <top>		=> <top>)

  (doit <fixnum> <fixnum>	=> <fixnum>)
  (doit <fixnum> <top>		=> <top>)
  (doit <top> <fixnum>		=> <top>)

  (doit <fixnum> <flonum>	=> <real>)
  (doit <flonum> <fixnum>	=> <real>)

  (doit <positive-fixnum> <fixnum>	=> <fixnum>)
  (doit <fixnum> <positive-fixnum>	=> <fixnum>)

  (void))


(parametrise ((check-test-name	'type-signature-common-ancestor))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?one ?two => ?expected)
       (check
	   (type-signature-common-ancestor ?one ?two)
	 (=> expander::syntax=?)
	 (syntax ?expected)))
      ))

;;; --------------------------------------------------------------------

  (doit (<top>) (<top>)		=> (<top>))

  (doit (<fixnum>) (<fixnum>)	=> (<fixnum>))
  (doit (<fixnum>) (<top>)		=> (<top>))
  (doit (<top>) (<fixnum>)		=> (<top>))

  (doit (<fixnum>) (<flonum>)	=> (<real>))
  (doit (<flonum>) (<fixnum>)	=> (<real>))

  (doit (<positive-fixnum>) (<fixnum>)	=> (<fixnum>))
  (doit (<fixnum>) (<positive-fixnum>)	=> (<fixnum>))

  (doit (<fixnum> <fixnum>)
	(<flonum> <bignum>)
	=> (<real> <exact-integer>))

  (doit (<fixnum> <fixnum> <string>)
	(<flonum> <bignum>)
	=> (<real> <exact-integer> . <list>))

  (doit (<fixnum> <fixnum>)
	(<flonum> <bignum> <string>)
	=> (<real> <exact-integer> . <list>))

  (void))


(parametrise ((check-test-name	'type-signature-union))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   ?expr
	 (=> expander::syntax=?)
	 (syntax ?expected)))
      ))

;;; --------------------------------------------------------------------
;;; some special cases

  (doit (type-signature-union)
	=> <list>)

  (doit (type-signature-union (<fixnum>) (<void>))
	=> (<void>))

  (doit (type-signature-union (<fixnum>) <bottom>)
	=> (<fixnum>))

;;; --------------------------------------------------------------------
;;; special booleans handling

  (doit (type-signature-union (<true>) (<false>))
	=> (<boolean>))

  (doit (type-signature-union (<true>) (<false>) (<boolean>))
	=> (<boolean>))

  (doit (type-signature-union (<true>) (<boolean>))
	=> (<boolean>))

  (doit (type-signature-union (<false>) (<boolean>))
	=> (<boolean>))

;;; --------------------------------------------------------------------

  (doit (type-signature-union (<fixnum>) (<string>))
	=> ((or <fixnum> <string>)))

  (doit (type-signature-union (<fixnum> <string> <vector>)
			      (<fixnum> <string> <vector>))
	=> (<fixnum> <string> <vector>))

  (doit (type-signature-union (<fixnum> <string> <vector>)
			      (<positive-fixnum> <string> <vector>))
	=> (<fixnum> <string> <vector>))

;;; --------------------------------------------------------------------

  (doit (type-signature-union <list> <list>)
	=> <list>)

  (doit (type-signature-union <list> (list-of <fixnum>))
	=> <list>)

  (doit (type-signature-union (<list>) ((list <string> <fixnum>)))
	=> (<list>))

  (doit (type-signature-union (list-of <string>) (list-of <fixnum>))
	=> (list-of (or <string> <fixnum>)))

  (void))


(parametrise ((check-test-name	'type-annotation-ancestors))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?one => ?expected)
       (check
	   (type-annotation-ancestors ?one)
	 (=> expander::syntax=?)
	 (syntax ?expected)))
      ))

;;; --------------------------------------------------------------------

  (doit <top>			=> ())
  (doit <void>			=> ())
  (doit <bottom>		=> ())

  (doit <condition>		=> (<record> <struct> <top>))

  (doit <positive-fixnum>	=> (<fixnum>
				    <exact-integer> <integer> <rational> <rational-valued>
				    <real> <real-valued> <complex> <number> <top>))

  (doit (list-of <fixnum>)		=> (<list> <top>))
  (doit (list <fixnum> <string>)	=> (<nelist> <list> <top>))

  (doit (pair-of <fixnum>)		=> (<pair> <top>))
  (doit (pair <fixnum> <string>)	=> (<pair> <top>))

  (doit (vector-of <fixnum>)		=> (<vector> <top>))
  (doit (vector <fixnum> <string>)	=> (<nevector> <vector> <top>))

  (doit (alist <fixnum> <string>)	=> (<list> <top>))

  (doit &who				=> (&condition <condition> <record> <struct> <top>))
  (doit (condition &who &message)	=> (<compound-condition> <condition> <record> <struct> <top>))

;;; --------------------------------------------------------------------

  (internal-body
    (define-type <my-condition>
      <condition>)
    (doit <my-condition>	=> (<record> <struct> <top>)))

;;; --------------------------------------------------------------------

  (doit <zero>			=> (<top>))

  (void))


(parametrise ((check-test-name	'type-of))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   ?expr
	 (=> expander::syntax=?)
	 (syntax ?expected)))
      ))

  (define-syntax doit-error
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check-for-true
	(try
	    (eval (quote ?expr) (environment '(vicare)))
	  (catch E
	    ((?expected)	#t)
	    (else		E)))))
      ))

;;; --------------------------------------------------------------------

  (doit (type-annotation-syntax (type-of 123))
	=> <positive-fixnum>)

  (doit (type-annotation-syntax (type-of (void)))
	=> <void>)

  (doit (let ((fun (lambda () 123)))
	  (type-annotation-syntax (type-of (fun))))
	=> <positive-fixnum>)

  (doit (type-annotation-syntax (or (type-of 1)
				    (type-of "ciao")
				    (type-of 'hey)))
	=> (or <positive-fixnum> <nestring> (enumeration hey)))

;;; --------------------------------------------------------------------
;;; invalid expressions

  ;;This raises an assertion: the expression is not allowed not to return.
  ;;
  (doit-error (type-annotation-syntax (type-of (error #f "error")))
	      => &syntax)

  ;;This raises an assertion:  the expression is not allowed not  return zero, two or
  ;;more values.
  ;;
  (doit-error (type-annotation-syntax (type-of (values)))	=> &syntax)
  (doit-error (type-annotation-syntax (type-of (values 1 2)))	=> &syntax)
  (doit-error (type-annotation-syntax (type-of (values 1 2 3)))	=> &syntax)

  ;;This raises  an assertion: the  expression is  not allowed to  return unspecified
  ;;values.
  ;;
  (doit-error (letrec ((fun (lambda ({_ . <list>}) (fun))))
		(type-annotation-syntax (type-of (fun))))
	      => &error)

  ;;The expression is expanded in the current lexical environment for phase zero, but
  ;;with empty lexical environment for the other phases:
  ;;
  (doit-error (let-syntax
		  ((outer (lambda (stx) 123)))
		(type-annotation-syntax
		 (type-of (let-syntax
			      ((inner (lambda (stx) (outer))))
			    (inner)))))
	      => &syntax)

  (doit-error (let-syntax
		  ((outer (lambda (stx) 123)))
		(type-annotation-syntax (outer)))
	      => &syntax)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'assert-signature))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?signature ?expr => ?expected0 ?expected ...)
       (begin
	 (check-for-true
	  (begin
	    (assert-signature ?signature ?expr)
	    #t))
	 (check
	     (assert-signature-and-return ?signature ?expr)
	   => ?expected0 ?expected ...)
	 (check
	     (cast-signature ?signature ?expr)
	   => ?expected0 ?expected ...)
	 ))
      ))

;;; --------------------------------------------------------------------
;;; single return value

  (doit (<fixnum>)	123	=> 123)
  (doit (<string>)	"ciao"	=> "ciao")

  (check-expand-time-signature-violation
      (assert-signature (<fixnum>) "ciao")
    => (<fixnum>) (<nestring>))

;;; --------------------------------------------------------------------
;;; multiple return values

  (doit (<fixnum> <flonum>)	(values 1 2.0)			=> 1 2.0)
  (doit (<string> <pair>)	(values "ciao" '(1 . 2))	=> "ciao" '(1 . 2))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum>) (values "A" "B"))
    => (<fixnum> <flonum>) (<nestring> <nestring>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum>) (values 1 "B"))
    => (<fixnum> <flonum>) (<positive-fixnum> <nestring>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum>) (values "A" 2.0))
    => (<fixnum> <flonum>) (<nestring> <positive-flonum>))

  ;;Any tuple of values matches.
  (doit <list>			1				=> 1)
  (doit <list>			(values 1 2 3)			=> 1 2 3)

  ;;Zero values, empty signature.
  (check-for-true
   (begin
     (assert-signature () (values))
     #t))

  (check-for-true
   (begin
     (assert-signature-and-return () (values))
     #t))

  (check-for-true
   (call-with-values
       (lambda ()
	 (assert-signature-and-return () (values)))
     (lambda () #t)))

  (check-expand-time-signature-violation
      (assert-signature () 1)
    => () (<positive-fixnum>))

  (check-expand-time-signature-violation
      (assert-signature () (values 1 2.0))
    => () (<positive-fixnum> <positive-flonum>))

  ;;Zero values, list signature.
  (check-for-true
   (begin
     (assert-signature <list> (values))
     #t))
  (check-for-true
   (call-with-values
       (lambda ()
	 (assert-signature-and-return <list> (values)))
     (lambda () #t)))

  (doit (<fixnum> . <list-of-flonums>)		(values 1 2.0)		=> 1 2.0)
  (doit (<fixnum> <flonum> . <list-of-strings>)	(values 1 2.0 "a")	=> 1 2.0 "a")
  (doit (<fixnum> <flonum> . <list-of-strings>)	(values 1 2.0 "a" "b")	=> 1 2.0 "a" "b")

;;; --------------------------------------------------------------------

  (check-for-true
   (begin
     (assert-signature <list-of-fixnums> (values))
     #t))

  (check-for-true
   (call-with-values
       (lambda ()
	 (assert-signature-and-return <list-of-fixnums> (values)))
     (lambda () #t)))

  (doit (<fixnum> <flonum> . <list-of-strings>) (values 1 2.0)	=> 1 2.0)

  (check-expand-time-signature-violation
      (assert-signature <list-of-flonums> (values 1 "A"))
    => <list-of-flonums> (<positive-fixnum> <nestring>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> . <list-of-flonums>) (values 1 "A"))
    => (<fixnum> . <list-of-flonums>) (<positive-fixnum> <nestring>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum> . <list-of-strings>) (values 1))
    => (<fixnum> <flonum> . <list-of-strings>) (<positive-fixnum>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum> . <list-of-strings>) (values 1.0))
    => (<fixnum> <flonum> . <list-of-strings>) (<positive-flonum>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum> . <list-of-strings>) (values 1 2))
    => (<fixnum> <flonum> . <list-of-strings>) (<positive-fixnum> <positive-fixnum>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum> . <list-of-strings>) (values 1.0 2.0))
    => (<fixnum> <flonum> . <list-of-strings>) (<positive-flonum> <positive-flonum>))

;;; --------------------------------------------------------------------
;;; number of values validation

  (doit (<top>)			1			=> 1)
  (doit (<top> <top>)		(values 1 2)		=> 1 2)
  (doit (<top> <top> <top>)	(values 1 2 3)		=> 1 2 3)

  (doit (<top> <top> . <list>)	(values 1 2)		=> 1 2)
  (doit (<top> <top> . <list>)	(values 1 2 3)		=> 1 2 3)
  (doit (<top> <top> . <list>)	(values 1 2 3 4)	=> 1 2 3 4)

;;; --------------------------------------------------------------------

  ;;Any tuple of values matches.
  ;;
  (check
      (expansion-of
       (assert-signature <list> 123))
    => '(begin (quote 123) (quote #!void)))

  (check
      (expansion-of
       (assert-signature (<fixnum>) 123))
    => '(begin (quote 123) (quote #!void)))

  (check
      (expansion-of
       (assert-signature-and-return (<fixnum>) 123))
    => '(quote 123))

  (check
      (expansion-of
       (assert-signature-and-return (<fixnum>) (unsafe-cast-signature (<fixnum>) (read))))
    => '((primitive read)))

  (when #f
    (expansion-of
     (assert-signature-and-return (<fixnum>) (read))))

;;; --------------------------------------------------------------------

  #;#;(parametrise ((print-gensym #f))
    (begin-for-syntax
      (expander::generate-descriptive-gensyms? #t))

    (define (f)
      (values 1 2 3))

    ;; (when #f
    ;;   (print-expansion
    ;;    (assert-signature-and-return (<fixnum> <flonum> <string>) (f))))

    ;; (when #f
    ;;   (print-expansion
    ;;    (assert-signature (<fixnum> <flonum> <string>) (f))))

    ;; (when #f
    ;;   (print-expansion
    ;;    (assert-signature-and-return <list-of-fixnums> (f))))

    ;; (when #f
    ;;   (print-expansion
    ;;    (assert-signature-and-return (<fixnum> <flonum> . <list-of-fixnums>) (f))))

    ;; (when #f
    ;;   (print-expansion
    ;;    (assert-signature (<fixnum> <flonum> . <list-of-fixnums>) (f))))

    (void))

  (void))


(parametrise ((check-test-name	'case-type))

  (check
      (case-type 123
	((<fixnum>)		'fixnum))
    => 'fixnum)

  (check
      (case-type "123"
	((<fixnum>)		'fixnum)
	((<string>)		'string))
    => 'string)

  (check
      (case-type 123
	((<vector>)		'vector)
	((<fixnum>)		'fixnum)
	((<string>)		'string))
    => 'fixnum)

  (check
      (void-object? (case-type #t
		      ((<vector>)		'vector)
		      ((<fixnum>)		'fixnum)
		      ((<string>)		'string)))
    => #t)

;;; --------------------------------------------------------------------
;;; with ELSE clause

  (check
      (case-type 123
	((<fixnum>)		'fixnum)
	(else			'else))
    => 'fixnum)

  (check
      (case-type "123"
	((<fixnum>)		'fixnum)
	((<string>)		'string)
	(else			'else))
    => 'string)

  (check
      (case-type 123
	((<vector>)		'vector)
	((<fixnum>)		'fixnum)
	((<string>)		'string)
	(else			'else))
    => 'fixnum)

  (check
      (case-type #t
	((<vector>)		'vector)
	((<fixnum>)		'fixnum)
	((<string>)		'string)
	(else			'else))
    => 'else)

;;; --------------------------------------------------------------------
;;; with receiver

  (check
      (case-type 123
	((<fixnum>)		=> (lambda (arg) (list arg 'fixnum)))
	(else			'else))
    => '(123 fixnum))

  (check
      (fluid-let-syntax
	  ((__who__ (identifier-syntax '_)))
	(case-type "123"
	  ((<string>)		=> (lambda ({arg <string>}) (list (.length arg) 'string)))
	  (else			'else)))
    => '(3 string))

  (check
      (case-type "123"
	((<fixnum>)		=> (lambda (arg) (list arg 'fixnum)))
	((<string>)		'string)
	(else			'else))
    => 'string)

  (check
      (case-type 123
	((<vector>)		'vector)
	((<fixnum>)		=> (lambda (arg) (list arg 'fixnum)))
	((<string>)		'string)
	(else			'else))
    => '(123 fixnum))

  (check
      (case-type #t
	((<vector>)		'vector)
	((<fixnum>)		=> (lambda (arg) (list arg 'fixnum)))
	((<string>)		'string)
	(else			'else))
    => 'else)

  (void))


(parametrise ((check-test-name	'type-of-predicates))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   (%type-signature->sexp (type-of ?expr))
	 => (quote ?expected)))
      ))

  (define-syntax doit*
    (syntax-rules (=>)
      ((_ (?pred ?type) => ?expected-type)
       (doit (?pred (unsafe-cast-signature (?type) (read)))
	     => (?expected-type)))
      ))

;;; --------------------------------------------------------------------
;;; fixnum?

  (doit* (fixnum? <fixnum>)		=> <true>)
  (doit* (fixnum? <exact-integer>)	=> <boolean>)
  (doit* (fixnum? <number>)		=> <boolean>)
  (doit* (fixnum? <top>)		=> <boolean>)
  (doit* (fixnum? <string>)		=> <false>)

;;;

  (doit* (fixnum? <positive-fixnum>)	=> <true>)
  (doit* (fixnum? <negative-fixnum>)	=> <true>)
  (doit* (fixnum? <zero-fixnum>)	=> <true>)
  (doit* (fixnum? <zero>)		=> <boolean>)
  (doit* (fixnum? <exact-integer>)	=> <boolean>)
  (doit* (fixnum? <number>)		=> <boolean>)
  (doit* (fixnum? <bignum>)		=> <false>)

;;; --------------------------------------------------------------------
;;; pair?

  (doit* (pair? <pair>)		=> <true>)
  (doit* (pair? <list>)		=> <boolean>)
  (doit* (pair? <top>)		=> <boolean>)
  (doit* (pair? <fixnum>)	=> <false>)

;;; --------------------------------------------------------------------
;;; list?

  (doit* (list? <null>)				=> <true>)
  (doit* (list? <list>)				=> <true>)
  (doit* (list? <pair>)				=> <boolean>)
  (doit* (list? <top>)				=> <boolean>)
  (doit* (list? <fixnum>)			=> <false>)

  (doit* (list? (list-of <fixnum>))		=> <true>)
  (doit* (list? (list <fixnum> <string>))	=> <true>)
  (doit* (list? (pair <fixnum> <string>))	=> <boolean>)
  (doit* (list? (pair <fixnum> <null>))		=> <true>)

;;; --------------------------------------------------------------------
;;; circular-list?

  (doit* (circular-list? <pair>)			=> <boolean>)
  (doit* (circular-list? <top>)				=> <boolean>)
  (doit* (circular-list? <null>)			=> <false>)
  (doit* (circular-list? <list>)			=> <false>)
  (doit* (circular-list? (list-of <fixnum>))		=> <false>)
  (doit* (circular-list? (list <fixnum> <string>))	=> <false>)
  (doit* (circular-list? <string>)			=> <false>)

;;; --------------------------------------------------------------------
;;; list-of-fixnums?

  (doit* (list-of-fixnums? (list-of <fixnum>))			=> <true>)
  (doit* (list-of-fixnums? (list <fixnum> <fixnum>))		=> <true>)

  (doit* (list-of-fixnums? (list-of <exact-integer>))		=> <boolean>)
  (doit* (list-of-fixnums? (list <fixnum> <exact-integer>))	=> <boolean>)
  (doit* (list-of-fixnums? <list>)				=> <boolean>)
  (doit* (list-of-fixnums? (pair <fixnum> <string>))		=> <boolean>)
  (doit* (list-of-fixnums? (pair <fixnum> <null>))		=> <true>)
  (doit* (list-of-fixnums? <pair>)				=> <boolean>)
  (doit* (list-of-fixnums? <top>)				=> <boolean>)

  (doit* (list-of-fixnums? (list <fixnum> <string>))		=> <boolean>)
  (doit* (list-of-fixnums? (list <fixnum> <bignum>))		=> <boolean>)
  (doit* (list-of-fixnums? (list-of <string>))			=> <false>)
  (doit* (list-of-fixnums? <string>)				=> <false>)

;;; --------------------------------------------------------------------
;;; zero?

  (doit* (zero? <zero>)			=> <true>)
  (doit* (zero? <positive>)		=> <false>)
  (doit* (zero? <negative>)		=> <false>)
  (doit* (zero? <non-positive>)		=> <boolean>)
  (doit* (zero? <non-negative>)		=> <boolean>)
  (doit* (zero? <real>)			=> <boolean>)
  (doit* (zero? <complex>)		=> <boolean>)
  (doit* (zero? <number>)		=> <boolean>)

;;; compnums

  (doit* (zero? <zero-compnum>)			=> <true>)
  (doit* (zero? <non-zero-inexact-compnum>)	=> <false>)
  (doit* (zero? <non-zero-compnum>)		=> <false>)
  (doit* (zero? <exact-compnum>)		=> <false>)
  (doit* (zero? <inexact-compnum>)		=> <boolean>)

;;; cflonums

  (doit* (zero? <zero-cflonum>)		=> <true>)
  (doit* (zero? <non-zero-cflonum>)	=> <boolean>)

  (void))


(parametrise ((check-test-name	'type-of-special-functions))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   (%type-signature->sexp (type-of ?expr))
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------
;;; not

  (doit (not #t)		=> (<false>))
  (doit (not #f)		=> (<true>))
  (doit (not 1)			=> (<false>))
  (doit (not (read))		=> (<boolean>))

  (doit (not (unsafe-cast-signature (<boolean>) (read)))
	=> (<boolean>))

  (doit (not (unsafe-cast-signature (<string>) (read)))
	=> (<false>))

  (doit (not (unsafe-cast-signature ((or <true> <string>)) (read)))
	=> (<false>))

  (doit (not (unsafe-cast-signature ((or <false> <string>)) (read)))
	=> (<boolean>))

  (doit (not (unsafe-cast-signature ((or <false> <true>)) (read)))
	=> (<boolean>))

  (doit (not (unsafe-cast-signature ((or <fixnum> <string>)) (read)))
	=> (<false>))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; eval: (put 'check-expand-time-signature-violation 'scheme-indent-function 1)
;; End:
