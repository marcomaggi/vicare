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
  (import (vicare)
    (prefix (vicare expander) expander::))
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
      (scheme-type-descriptor.name (type-descriptor <string>))
    => '<string>)

  (let ((btd (type-descriptor <string>)))

    (check
	(scheme-type-descriptor.name (scheme-type-descriptor.parent btd))
      => '<top>)

    (check
	(scheme-type-descriptor.uids-list btd)
      => '(vicare:scheme-type:<string> vicare:scheme-type:<top>))

    (check-for-true	(procedure? (scheme-type-descriptor.method-retriever btd)))
    (check-for-true	((scheme-type-descriptor.method-retriever btd) 'length))

    (check
	(((scheme-type-descriptor.method-retriever btd) 'length) "ciao")
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
	(scheme-type-descriptor.name (.parent string-btd))
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


(parametrise ((check-test-name	'type-super-and-sub))

  (check-for-true	(type-super-and-sub? <number> <fixnum>))
  (check-for-false	(type-super-and-sub? <number> <string>))

  (check
      (expansion-of (type-super-and-sub? <number> <fixnum>))
    => '(quote #t))

  (check
      (expansion-of (type-super-and-sub? <number> <string>))
    => '(quote #f))

  (check-for-true	(type-super-and-sub? <top> <number>))
  (check-for-false	(type-super-and-sub? <number> <top>))

  (internal-body
    (define-record-type alpha)

    (define-record-type beta
      (parent alpha))

    (define-record-type gamma
      (parent beta))

    (check-for-true	(type-super-and-sub? alpha beta))
    (check-for-false	(type-super-and-sub? beta alpha))

    (check-for-true	(type-super-and-sub? alpha gamma))
    (check-for-false	(type-super-and-sub? gamma alpha))

    (check-for-true	(type-super-and-sub? beta gamma))
    (check-for-false	(type-super-and-sub? gamma beta))

    (check-for-true	(type-super-and-sub? <top> alpha))
    (check-for-false	(type-super-and-sub? alpha <top>))

    (check-for-true	(type-super-and-sub? <top> beta))
    (check-for-false	(type-super-and-sub? beta <top>))

    (check-for-true	(type-super-and-sub? <top> gamma))
    (check-for-false	(type-super-and-sub? gamma <top>))

    (void))

;;; --------------------------------------------------------------------
;;; lists

  (check-for-true	(type-super-and-sub? <list-of-numbers> <list-of-reals>))
  (check-for-false	(type-super-and-sub? <list-of-numbers> <list-of-strings>))

  (check-for-true	(type-super-and-sub? (list <number>) (list <fixnum>)))
  (check-for-false	(type-super-and-sub? (list <symbol>) (list <string>)))

  (check-for-true	(type-super-and-sub? (list-of <number>) (list-of <fixnum>)))
  (check-for-false	(type-super-and-sub? (list-of <symbol>) (list-of <string>)))

  (check-for-true	(type-super-and-sub? (list-of <number>) (list <fixnum>)))
  (check-for-true	(type-super-and-sub? (list-of <number>) (list <fixnum> <flonum> <number>)))

  ;;This is false  because a LIST annotation  specifies the number of  items, while a
  ;;LIST-OF annotation does not specify it.
  (check-for-false	(type-super-and-sub? (list <number>) (list-of <number>)))

  (check-for-false	(type-super-and-sub? (list <number>) (list-of <string>)))

;;; --------------------------------------------------------------------
;;; vectors

  (check-for-true	(type-super-and-sub? <vector-of-numbers> <vector-of-reals>))
  (check-for-false	(type-super-and-sub? <vector-of-numbers> <vector-of-strings>))

  (check-for-true	(type-super-and-sub? (vector <number>) (vector <fixnum>)))
  (check-for-false	(type-super-and-sub? (vector <symbol>) (vector <string>)))

  (check-for-true	(type-super-and-sub? (vector-of <number>) (vector-of <fixnum>)))
  (check-for-false	(type-super-and-sub? (vector-of <symbol>) (vector-of <string>)))

  (check-for-true	(type-super-and-sub? (vector-of <number>) (vector <fixnum>)))
  (check-for-true	(type-super-and-sub? (vector-of <number>) (vector <fixnum> <flonum> <number>)))

  ;;This is false  because a VECTOR annotation  specifies the number of  items, while a
  ;;VECTOR-OF annotation does not specify it.
  (check-for-false	(type-super-and-sub? (vector <number>) (vector-of <number>)))

  (check-for-false	(type-super-and-sub? (vector <number>) (vector-of <string>)))

;;; --------------------------------------------------------------------
;;; pairs

  (check-for-true	(type-super-and-sub? <pair-of-numbers> <pair-of-reals>))
  (check-for-false	(type-super-and-sub? <pair-of-numbers> <pair-of-strings>))

  (check-for-true	(type-super-and-sub? (pair <number> <integer>) (pair <fixnum> <fixnum>)))
  (check-for-false	(type-super-and-sub? (pair <symbol> <keyword>) (pair <string> <keyword>)))
  (check-for-false	(type-super-and-sub? (pair <symbol> <keyword>) (pair <keyword> <string>)))
  (check-for-false	(type-super-and-sub? (pair <symbol> <keyword>) (pair <string> <string>)))

  (check-for-true	(type-super-and-sub? (pair-of <number>) (pair-of <fixnum>)))
  (check-for-false	(type-super-and-sub? (pair-of <symbol>) (pair-of <string>)))

  (check-for-true	(type-super-and-sub? (pair-of <number>) (pair <fixnum> <flonum>)))

  ;;This is true  because both a PAIR  annotation and a PAIR-OF  annotation specify a
  ;;pair, which holds two values.
  (check-for-true	(type-super-and-sub? (pair <number> <number>) (pair-of <number>)))

  (check-for-false	(type-super-and-sub? (pair <number> <number>) (pair-of <string>)))

  (check-for-false	(type-super-and-sub? (list <fixnum>) (pair-of (or <fixnum> <null>))))

  (check-for-true	(type-super-and-sub? (pair <fixnum> <null>)
					     (list <fixnum>)))
  (check-for-true	(type-super-and-sub? (pair-of (or <fixnum> <null>))
					     (list <fixnum>)))

  (check-for-true	(type-super-and-sub? (list-of <fixnum>) (list <fixnum>)))
  (check-for-true	(type-super-and-sub? (list-of <number>) (list <fixnum> <flonum>)))

  ;;Does not match because  a LIST annotation specifies the number  of items, while a
  ;;LIST-OF annotation does not specify it.
  (check-for-false	(type-super-and-sub? (list <fixnum> <fixnum>) (list-of <fixnum>)))

  ;;Does not match because a LIST-OF annotation does not specify the number of items,
  ;;while a PAIR annotation implies at least one item.
  (check-for-false	(type-super-and-sub? (pair <fixnum> (list-of <fixnum>))
					     (list-of <fixnum>)))

  (check-for-true	(type-super-and-sub? (list-of <fixnum>) (pair <fixnum> <null>)))

  ;;Does not  match because the PAIR-OF  annotation implies at least  one item, while
  ;;the LIST-OF annotation implies nothing.
  (check-for-false	(type-super-and-sub? (list-of <fixnum>) (pair-of (or <fixnum> <null>))))

;;; --------------------------------------------------------------------
;;; unions

  (check-for-true	(type-super-and-sub? (or <number>) (or <number>)))
  (check-for-true	(type-super-and-sub? (or <number>) (or <fixnum>)))
  (check-for-false	(type-super-and-sub? (or <fixnum>) (or <number>)))

  (check-for-true	(type-super-and-sub? (or <number> <string>) (or <number> <string>)))
  (check-for-true	(type-super-and-sub? (or <number> <string>) (or <fixnum> <string>)))
  (check-for-true	(type-super-and-sub? (or <number> <vector>) (or <fixnum> (vector-of <symbol>))))

  (check-for-true	(type-super-and-sub? (or <number> <string>) (or <string> <number>)))
  (check-for-true	(type-super-and-sub? (or <number> <string>) (or <string> <fixnum>)))
  (check-for-true	(type-super-and-sub? (or <number> <vector>) (or (vector-of <symbol>) <fixnum>)))

  (check-for-true	(type-super-and-sub? (or <number> <string>)	<number>))
  (check-for-true	(type-super-and-sub? (or <number> <string>)	<fixnum>))
  (check-for-true	(type-super-and-sub? (or <number> <string>)	<string>))

  ;;True because all the types in the union are sub-types of the super-type.
  (check-for-true	(type-super-and-sub? <number>	(or <fixnum> <flonum>)))

  (check-for-false	(type-super-and-sub? <number>	(or <number> <string>)))
  (check-for-false	(type-super-and-sub? <string>	(or <number> <string>)))

;;; --------------------------------------------------------------------
;;; intersections

  (check-for-true	(type-super-and-sub? (and <fixnum> <exact>) <positive-fixnum>))
  (check-for-true	(type-super-and-sub? (and <fixnum> <exact>) <fixnum>))
  (check-for-false	(type-super-and-sub? (and <fixnum> <exact>) <exact>))
  (check-for-false	(type-super-and-sub? (and <fixnum> <exact>) <string>))

  (check-for-true	(type-super-and-sub? (and <positive> <exact>) <positive-fixnum>))
  (check-for-false	(type-super-and-sub? (and <positive> <exact>) <negative-fixnum>))
  (check-for-false	(type-super-and-sub? (and <positive> <exact>) <fixnum>))
  (check-for-false	(type-super-and-sub? (and <positive> <exact>) <string>))

  (check-for-true	(type-super-and-sub? (and <number> <positive>)
					     (and <positive-fixnum> <positive-flonum>)))
  (check-for-false	(type-super-and-sub? (and <number> <positive>)
					     (and <positive-fixnum> <negative-flonum>)))
  (check-for-false	(type-super-and-sub? (and <number> <positive>)
					     (and <negative-fixnum> <positive-flonum>)))

;;; --------------------------------------------------------------------
;;; complement

  ;;If something is not a "<number>", for sure it is not a "<fixnum>".
  (check-for-true	(type-super-and-sub? (not <fixnum>) (not <number>)))

  ;;If something is not a "<fixnum>"
  (check-for-false	(type-super-and-sub? (not <number>) (not <fixnum>)))
  (check-for-false	(type-super-and-sub? (not <number>) (not <string>)))

  (check-for-true	(type-super-and-sub? (not <string>) <number>))
  (check-for-false	(type-super-and-sub? (not <number>) <number>))
  (check-for-false	(type-super-and-sub? (not <number>) <fixnum>))

  (check-for-false	(type-super-and-sub? <number> (not <string>)))

;;; --------------------------------------------------------------------
;;; misc

  (check-for-true	(type-super-and-sub? (or <exact> <inexact>)	<fixnum>))

  (void))


(parametrise ((check-test-name	'signature-super-and-sub))

  (check-for-true	(signature-super-and-sub? (<number>) (<fixnum>)))
  (check-for-false	(signature-super-and-sub? (<number>) (<string>)))

  (check
      (expansion-of (signature-super-and-sub? (<number>) (<fixnum>)))
    => '(quote #t))

  (check
      (expansion-of (signature-super-and-sub? (<number>) (<string>)))
    => '(quote #f))

  (check-for-true	(signature-super-and-sub? (<top>) (<number>)))
  (check-for-false	(signature-super-and-sub? (<number>) (<top>)))

  (internal-body
    (define-record-type alpha)

    (define-record-type beta
      (parent alpha))

    (define-record-type gamma
      (parent beta))

    (check-for-true	(signature-super-and-sub? (alpha) (beta)))
    (check-for-false	(signature-super-and-sub? (beta) (alpha)))

    (check-for-true	(signature-super-and-sub? (alpha) (gamma)))
    (check-for-false	(signature-super-and-sub? (gamma) (alpha)))

    (check-for-true	(signature-super-and-sub? (beta) (gamma)))
    (check-for-false	(signature-super-and-sub? (gamma) (beta)))

    (check-for-true	(signature-super-and-sub? (<top>) (alpha)))
    (check-for-false	(signature-super-and-sub? (alpha) (<top>)))

    (check-for-true	(signature-super-and-sub? (<top>) (beta)))
    (check-for-false	(signature-super-and-sub? (beta) (<top>)))

    (check-for-true	(signature-super-and-sub? (<top>) (gamma)))
    (check-for-false	(signature-super-and-sub? (gamma) (<top>)))

    (void))

;;; --------------------------------------------------------------------
;;; proper lists

  (check-for-true	(signature-super-and-sub? (<number> <number>) (<fixnum> <fixnum>)))
  (check-for-false	(signature-super-and-sub? (<fixnum> <fixnum>) (<number> <number>)))
  (check-for-false	(signature-super-and-sub? (<number> <number>) (<string> <string>)))
  (check-for-false	(signature-super-and-sub? (<number> <fixnum>) (<fixnum> <number>)))
  (check-for-false	(signature-super-and-sub? (<fixnum> <number>) (<number> <fixnum>)))

  (check-for-true	(signature-super-and-sub? (<top> <top>) (<number> <number>)))
  (check-for-false	(signature-super-and-sub? (<top> <number>) (<number> <top>)))
  (check-for-false	(signature-super-and-sub? (<number> <top>) (<top> <number>)))
  (check-for-false	(signature-super-and-sub? (<number> <number>) (<top> <top>)))

  (check-for-false	(signature-super-and-sub? () (<top>)))
  (check-for-false	(signature-super-and-sub? (<top>) ()))
  (check-for-false	(signature-super-and-sub? () (<top> <top>)))
  (check-for-false	(signature-super-and-sub? (<top> <top>) ()))

  (check-for-false	(signature-super-and-sub? (<number> <number>) (<fixnum>)))
  (check-for-false	(signature-super-and-sub? (<number>) (<fixnum> <fixnum>)))

;;; --------------------------------------------------------------------
;;; standalone list identifiers

  (check-for-true	(signature-super-and-sub? <list-of-numbers> <list-of-reals>))
  (check-for-false	(signature-super-and-sub? <list-of-numbers> <list-of-strings>))

  (check-for-true	(signature-super-and-sub? <list> ()))
  (check-for-true	(signature-super-and-sub? <list> (<top> <top>)))
  (check-for-true	(signature-super-and-sub? <list> (<top> <top> . <list>)))

;;; --------------------------------------------------------------------
;;; improper lists

  (check-for-true	(signature-super-and-sub? (<number> . <list-of-numbers>) (<real> . <list-of-reals>)))
  (check-for-false	(signature-super-and-sub? (<number> . <list-of-numbers>) (<number> . <list-of-strings>)))
  (check-for-false	(signature-super-and-sub? (<number> . <list-of-numbers>) (<string> . <list-of-numbers>)))
  (check-for-false	(signature-super-and-sub? (<fixnum> . <list-of-numbers>) (<number> . <list-of-numbers>)))

  (check-for-true	(signature-super-and-sub? (<number> <number> . <list>) (<fixnum> <real> . <list>)))

  (check-for-false	(signature-super-and-sub? (<number> <number> . <list>) (<fixnum>)))
  (check-for-true	(signature-super-and-sub? (<number> <number> . <list>) (<fixnum> <fixnum>)))

  #t)


(parametrise ((check-test-name	'assert-signature))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?signature ?expr => ?expected0 ?expected ...)
       (begin
	 (check
	     (assert-signature ?signature ?expr)
	   => (void))
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
      (case-type #t
	((<vector>)		'vector)
	((<fixnum>)		'fixnum)
	((<string>)		'string))
    => (void))

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


(parametrise ((check-test-name	'closure-type-spec))
#|
  (check
      (expander::type-signature.syntax-object (type-of (lambda ({a <fixnum>}) a)))
    (=> syntax=?)
    #'((lambda (<fixnum>) => <list>)))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ <fixnum>} {a <fixnum>}) a)))
    (=> syntax=?)
    #'((lambda (<fixnum>) => (<fixnum>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ <fixnum> <string>} {a <fixnum>} {b <string>})
  							 (values a b))))
    (=> syntax=?)
    #'((lambda (<fixnum> <string>) => (<fixnum> <string>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
  							 (list a b))))
    (=> syntax=?)
    #'((lambda (<fixnum> <fixnum>) => ((list-of <fixnum>)))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda ({_ . (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
  							 (values a b))))
    (=> syntax=?)
    #'((lambda (<fixnum> <fixnum>) => (list-of <fixnum>))))

  (check
      (expander::type-signature.syntax-object (type-of (lambda (_ {a <fixnum>} . {rest (list-of <fixnum>)})
  							 (values a rest))))
    (=> syntax=?)
    #'((lambda (<fixnum> . (list-of <fixnum>)) => <list>)))
|#
;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define (fun {a <fixnum>}) a)
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum>) => <list>)))

  (check
      (internal-body
	(define ({fun <fixnum>} {a <fixnum>}) a)
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum>) => (<fixnum>))))

  (check
      (internal-body
	(define ({fun <fixnum> <string>} {a <fixnum>} {b <string>})
	  (values a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum> <string>) => (<fixnum> <string>))))

  (check
      (internal-body
	(define ({fun (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
	  (list a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum> <fixnum>) => ((list-of <fixnum>)))))

  (check
      (internal-body
	(define ({fun . (list-of <fixnum>)} {a <fixnum>} {b <fixnum>})
	  (values a b))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum> <fixnum>) => (list-of <fixnum>))))

  (check
      (internal-body
	(define (fun {a <fixnum>} . {rest (list-of <fixnum>)})
	  (values a rest))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((lambda (<fixnum> . (list-of <fixnum>)) => <list>)))

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(case-define fun
	  (({a <fixnum>})
	   a)
	  (({a <fixnum>} {b <string>})
	   (list a b)))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> <list>)
	 ((<fixnum> <string>)	=> <list>))))

  (check
      (internal-body
	(case-define fun
	  (({_ <fixnum>} {a <fixnum>})
	   a)
	  (({_ (list <fixnum> <string>)} {a <fixnum>} {b <string>})
	   (list a b)))
	(expander::type-signature.syntax-object (type-of fun)))
    (=> syntax=?)
    #'((case-lambda
	 ((<fixnum>)		=> (<fixnum>))
	 ((<fixnum> <string>)	=> ((list <fixnum> <string>))))))

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
