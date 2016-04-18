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
	     (eval (quote ?input-form) EVAL-ENVIRONMENT)
	   (catch E
	     ((expander::&expand-time-type-signature-violation)
	      #;(print-condition E)
	      (values (syntax->datum (expander::type-signature.syntax-object (expander::condition-expected-type-signature E)))
		      (syntax->datum (expander::type-signature.syntax-object (expander::condition-returned-type-signature E)))))
	     (else E)))
       => (quote ?expected-signature-sexp) (quote ?returned-signature-sexp)))
    ))


(parametrise ((check-test-name	'type-descriptor))

  (check
      (scheme-type-descriptor? (type-descriptor <string>))
    => #t)

  (check
      (scheme-type-descriptor-name (type-descriptor <string>))
    => '<string>)

  (let ((btd (type-descriptor <string>)))

    (check
	(scheme-type-descriptor-name (scheme-type-descriptor-parent btd))
      => '<top>)

    (check
	(scheme-type-descriptor-uids-list btd)
      => '(vicare:scheme-type:<string> vicare:scheme-type:<top>))

    (check-for-true	(procedure? (scheme-type-descriptor-method-retriever btd)))
    (check-for-true	((scheme-type-descriptor-method-retriever btd) 'length))

    (check
	(((scheme-type-descriptor-method-retriever btd) 'length) "ciao")
      => 4)

    (void))

;;; --------------------------------------------------------------------

  (internal-body
    (define {string-btd <scheme-type-descriptor>}
      (type-descriptor <string>))

    (check
	(.name string-btd)
      => '<string>)

    (check
	(scheme-type-descriptor-name (.parent string-btd))
      => '<top>)

    (check
	(.uids-list string-btd)
      => '(vicare:scheme-type:<string> vicare:scheme-type:<top>))

    (check-for-true	(procedure? (.method-retriever string-btd)))
    (check-for-true	((.method-retriever string-btd) 'length))

    (check
	(((.method-retriever string-btd) 'length) "ciao")
      => 4)

    (void))

  (void))


(parametrise ((check-test-name	'is-a))

  (check-for-true	(is-a? "string" <string>))
  (check-for-false	(is-a? 123      <string>))

  (check-for-true	(is-a? '(1 2 3) (list-of <fixnum>)))
  (check-for-false	(is-a? "ciao"   (list-of <fixnum>)))

  (check-for-true	(is-a? '#(1 2 3) (vector-of <fixnum>)))
  (check-for-false	(is-a? "ciao"    (vector-of <fixnum>)))

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
					   (or <positive-fixnum> <positive-bignum> <positive-ratnum>
					       <positive-flonum> <positive-zero-flonum>)))

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


(parametrise ((check-test-name	'type-annotation-super-and-sub))

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
;;; ancestors-of

  (check-for-true	(type-annotation-super-and-sub? (ancestors-of <fixnum>) <exact-integer>))
  (check-for-false	(type-annotation-super-and-sub? (ancestors-of <fixnum>) <positive-fixnum>))
  (check-for-false	(type-annotation-super-and-sub? (ancestors-of <fixnum>) <fixnum>))

  (check-for-true	(type-annotation-super-and-sub? (ancestors-of &condition)
							<condition>))
  (check-for-true	(type-annotation-super-and-sub? (ancestors-of &condition)
							<record>))
  (check-for-true	(type-annotation-super-and-sub? (ancestors-of &condition)
							<struct>))
  (check-for-true	(type-annotation-super-and-sub? (ancestors-of &condition)
							<top>))
  (check-for-false	(type-annotation-super-and-sub? (ancestors-of &condition)
							<string>))

  (check-for-true	(type-annotation-super-and-sub? (not (ancestors-of &condition))
							<string>))

  (check-for-true	(type-annotation-super-and-sub? (not (ancestors-of <false>))
							<fixnum>))

  (check-for-false	(type-annotation-super-and-sub? (ancestors-of <false>)
							(or <false> <string>)))
  (check-for-true	(type-annotation-super-and-sub? (not (ancestors-of <false>))
							(or <false> <string>)))

;;; --------------------------------------------------------------------
;;; misc

  (check-for-true	(type-annotation-super-and-sub? (or <exact> <inexact>)	<fixnum>))

  (void))


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

;;; --------------------------------------------------------------------
;;; improper lists

  (check-for-true	(type-signature-super-and-sub? (<number> . <list-of-numbers>) (<real> . <list-of-reals>)))
  (check-for-false	(type-signature-super-and-sub? (<number> . <list-of-numbers>) (<number> . <list-of-strings>)))
  (check-for-false	(type-signature-super-and-sub? (<number> . <list-of-numbers>) (<string> . <list-of-numbers>)))
  (check-for-false	(type-signature-super-and-sub? (<fixnum> . <list-of-numbers>) (<number> . <list-of-numbers>)))

  (check-for-true	(type-signature-super-and-sub? (<number> <number> . <list>) (<fixnum> <real> . <list>)))

  (check-for-false	(type-signature-super-and-sub? (<number> <number> . <list>) (<fixnum>)))
  (check-for-true	(type-signature-super-and-sub? (<number> <number> . <list>) (<fixnum> <fixnum>)))

  (void))


(parametrise ((check-test-name	'type-signature-super-and-sub))

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
;;; ancestors-of

  (doit ((ancestors-of &condition))
	(<condition>)
	=> exact-match)

  (doit ((ancestors-of &condition))
	(<record>)
	=> exact-match)

  (doit ((ancestors-of &condition))
	(<struct>)
	=> exact-match)

  (doit ((ancestors-of &condition))
	(<top>)
	=> exact-match)

  (doit ((ancestors-of &condition))
	(<fixnum>)
	=> no-match)

  (doit ((ancestors-of &condition))
	(&condition)
	=> no-match)

  (doit ((ancestors-of &condition))
	(&who)
	=> no-match)

  (doit ((ancestors-of &condition))
	((condition &who &message))
	=> no-match)

  (doit ((ancestors-of &who))
	((condition &who &message))
	=> no-match)

;;; complement

  (doit ((not (ancestors-of &condition)))
	(&condition)
	=> exact-match)

  (doit ((not (ancestors-of &condition)))
	(<condition>)
	=> no-match)

  (doit ((not (ancestors-of &condition)))
	(<record>)
	=> no-match)

  (doit ((not (ancestors-of &condition)))
	(<struct>)
	=> no-match)

  (doit ((not (ancestors-of &condition)))
	(<top>)
	=> no-match)

  (void))


(parametrise ((check-test-name	'type-annotation-common-ancestor))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?one ?two => ?expected)
       (check
	   (type-annotation-common-ancestor ?one ?two)
	 (=> syntax=?)
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
	 (=> syntax=?)
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


(parametrise ((check-test-name	'type-annotation-ancestors))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?one => ?expected)
       (check
	   (type-annotation-ancestors ?one)
	 (=> syntax=?)
	 (syntax ?expected)))
      ))

;;; --------------------------------------------------------------------

  (doit <top>			=> ())
  (doit <void>			=> ())
  (doit <no-return>		=> ())

  (doit <condition>		=> (<record> <struct> <top>))

  (doit <positive-fixnum>	=> (<fixnum>
				    <exact-integer> <integer> <rational> <rational-valued>
				    <real> <real-valued> <complex> <number> <top>))

  (doit (list-of <fixnum>)		=> (<list> <top>))
  (doit (list <fixnum> <string>)	=> (<list> <top>))

  (doit (pair-of <fixnum>)		=> (<pair> <top>))
  (doit (pair <fixnum> <string>)	=> (<pair> <top>))

  (doit (vector-of <fixnum>)		=> (<vector> <top>))
  (doit (vector <fixnum> <string>)	=> (<vector> <top>))

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


(parametrise ((check-test-name	'assert-signature))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?signature ?expr => ?expected0 ?expected ...)
       (begin
	 (check-for-true
	  (void-object? (assert-signature ?signature ?expr)))
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
    => (<fixnum>) (<string>))

;;; --------------------------------------------------------------------
;;; multiple return values

  (doit (<fixnum> <flonum>)	(values 1 2.0)			=> 1 2.0)
  (doit (<string> <pair>)	(values "ciao" '(1 . 2))	=> "ciao" '(1 . 2))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum>) (values "A" "B"))
    => (<fixnum> <flonum>) (<string> <string>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum>) (values 1 "B"))
    => (<fixnum> <flonum>) (<positive-fixnum> <string>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum>) (values "A" 2.0))
    => (<fixnum> <flonum>) (<string> <positive-flonum>))

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
    => <list-of-flonums> (<positive-fixnum> <string>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> . <list-of-flonums>) (values 1 "A"))
    => (<fixnum> . <list-of-flonums>) (<positive-fixnum> <string>))

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

  (parametrise ((print-gensym #f))
    (begin-for-syntax
      (expander::generate-descriptive-gensyms? #t))

    (define (f)
      (values 1 2 3))

    (when #f
      (print-expansion
       (assert-signature-and-return (<fixnum> <flonum> <string>) (f))))

    (when #f
      (print-expansion
       (assert-signature (<fixnum> <flonum> <string>) (f))))

    (when #f
      (print-expansion
       (assert-signature-and-return <list-of-fixnums> (f))))

    (when #f
      (print-expansion
       (assert-signature-and-return (<fixnum> <flonum> . <list-of-fixnums>) (f))))

    (when #f
      (print-expansion
       (assert-signature (<fixnum> <flonum> . <list-of-fixnums>) (f))))

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


(parametrise ((check-test-name	'type-of-common-syntaxes))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   (%type-signature->sexp (type-of ?expr))
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------
;;; LET type propagation

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
	=> ((or <positive-fixnum> <negative-fixnum> <zero-fixnum> <fixnum>)))

;;; --------------------------------------------------------------------
;;; LET* type propagation

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
	=> ((or <positive-fixnum> <negative-fixnum> <zero-fixnum> <fixnum>)))

;;; --------------------------------------------------------------------
;;; LETREC type propagation

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
	=> ((or <positive-fixnum> <negative-fixnum> <zero-fixnum> <fixnum>)))

;;; --------------------------------------------------------------------
;;; LETREC* type propagation

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
	=> ((or <positive-fixnum> <negative-fixnum> <zero-fixnum> <fixnum>)))

;;; --------------------------------------------------------------------
;;; CALL-WITH-VALUES type propagation

  (doit (call-with-values
	    (lambda () 1)
	  (lambda ({_ <fixnum>} {a <fixnum>})
	    (add1 a)))
	=> (<fixnum>))

  (doit (call-with-values
	    (lambda () 1)
	  (lambda (a)
	    (add1 a)))
	=> <list>)

;;; --------------------------------------------------------------------
;;; LAMBDA application type propagation

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
;;; CASE-LAMBDA application type propagation

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
;;; RECEIVE type propagation

  #;(doit (receive ({a <fixnum>})
	    1
	  a)
	=> (<fixnum>))

  #;(doit (receive (a)
	    1
	  a)
	=> (<top>))

;;; --------------------------------------------------------------------
;;; RECEIVE-AND-RETURN type propagation

  #;(doit (receive-and-return ({a <fixnum>})
	    1
	  (void))
	=> (<fixnum>))

  #;(doit (receive-and-return (a)
	    1
	  a)
	=> (<top>))

;;; --------------------------------------------------------------------
;;; COND type propagation

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

;;; --------------------------------------------------------------------
;;; CASE type propagation

  #;(doit (case (read)
	  ((1)		1)
	  ((ciao)	2)
	  (else		3))
	=> (<positive-fixnum>))

  #;(doit (case (read)
	  ((1)		1)
	  ((ciao)	'ciao)
	  (else		#f))
	=> ((or <positive-fixnum> <symbol> <false>)))

;;; --------------------------------------------------------------------
;;; AND type propagation

  (doit (and 1 2 3)
	=> (<positive-fixnum>))

  (doit (and 1 "2" 3)
	=> (<positive-fixnum>))

  (doit (and 1 2 "3")
	=> (<string>))

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

;;; --------------------------------------------------------------------
;;; OR type propagation
#|
  (doit (or 1 2 "3")
	=> (<positive-fixnum>))

  (doit (or #f 2 "3")
	=> (<positive-fixnum>))

  (doit (or #f #f "3")
	=> (<string>))

  (doit (or #f (and #t "ciao") 3.4)
	=> (<string>))
|#
;;; --------------------------------------------------------------------
;;; XOR type propagation
#|
  (doit (xor 1 2 "3")
	=> ((or <positive-fixnum> <string>)))

  (doit (xor 1 2.2 "3")
	=> ((or <positive-fixnum> <positive-flonum> <string>)))
|#
  (void))


(parametrise ((check-test-name	'type-of-predicates))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?expr => ?expected)
       (check
	   (%type-signature->sexp (type-of ?expr))
	 => (quote ?expected)))
      ))

;;; --------------------------------------------------------------------
;;; fixnum?

  (doit (fixnum? (unsafe-cast-signature (<fixnum>) (read)))
	=> (<true>))

  (doit (fixnum? (unsafe-cast-signature (<exact-integer>) (read)))
	=> (<boolean>))

  (doit (fixnum? (unsafe-cast-signature (<number>) (read)))
	=> (<boolean>))

  (doit (fixnum? (unsafe-cast-signature (<top>) (read)))
	=> (<boolean>))

  (doit (fixnum? (unsafe-cast-signature (<string>) (read)))
	=> (<false>))

;;;

  (doit (fixnum? (unsafe-cast-signature (<positive-fixnum>) (read)))
	=> (<true>))

  (doit (fixnum? (unsafe-cast-signature (<negative-fixnum>) (read)))
	=> (<true>))

  (doit (fixnum? (unsafe-cast-signature (<zero-fixnum>) (read)))
	=> (<true>))

  (doit (fixnum? (unsafe-cast-signature (<zero>) (read)))
	=> (<boolean>))

  (doit (fixnum? (unsafe-cast-signature (<exact-integer>) (read)))
	=> (<boolean>))

  (doit (fixnum? (unsafe-cast-signature (<number>) (read)))
	=> (<boolean>))

  (doit (fixnum? (unsafe-cast-signature (<bignum>) (read)))
	=> (<false>))

;;; --------------------------------------------------------------------
;;; pair?

  (doit (pair? (unsafe-cast-signature (<pair>) (read)))
	=> (<true>))

  (doit (pair? (unsafe-cast-signature (<list>) (read)))
	=> (<boolean>))

  (doit (pair? (unsafe-cast-signature (<top>) (read)))
	=> (<boolean>))

  (doit (pair? (unsafe-cast-signature (<fixnum>) (read)))
	=> (<false>))

;;; --------------------------------------------------------------------
;;; list?

  (doit (list? (unsafe-cast-signature (<pair>) (read)))
	=> (<boolean>))

  (doit (list? (unsafe-cast-signature (<list>) (read)))
	=> (<true>))

  (doit (list? (unsafe-cast-signature (<top>) (read)))
	=> (<boolean>))

  (doit (list? (unsafe-cast-signature (<fixnum>) (read)))
	=> (<false>))

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

  (doit (not (unsafe-cast-signature ((or <true> <string>)) (read)))
	=> (<true>))

  (doit (not (unsafe-cast-signature ((or <false> <string>)) (read)))
	=> (<boolean>))

  (doit (not (unsafe-cast-signature ((or <false> <true>)) (read)))
	=> (<boolean>))

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
