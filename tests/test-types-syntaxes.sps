;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for typed language syntaxes
;;;Date: Sat Oct 10, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    (prefix (vicare expander) xp.)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: type-related syntaxes\n")


;;;; helpers

(define-syntax-rule (print-expansion ?stx)
  (debug-print (quote expansion-example:) (quote ?stx) (quote ==>) (expansion-of ?stx)))

(library (types-of-lists)
  (options strict-r6rs)
  (export
    <list-of-fixnums>
    <list-of-flonums>
    <list-of-reals>
    <list-of-numbers>
    <list-of-strings>)
  (import (vicare)
    (prefix (vicare expander) xp.))
  (define-syntax <list-of-fixnums>	(xp.make-list-type-spec #'<fixnum>))
  (define-syntax <list-of-flonums>	(xp.make-list-type-spec #'<flonum>))
  (define-syntax <list-of-numbers>	(xp.make-list-type-spec #'<number>))
  (define-syntax <list-of-reals>	(xp.make-list-type-spec #'<real>))
  (define-syntax <list-of-strings>	(xp.make-list-type-spec #'<string>))
  #| end of LIBRARY |# )

(import (types-of-lists))

;;; --------------------------------------------------------------------

(define-constant EVAL-ENVIRONMENT
  (environment '(vicare) '(types-of-lists)))

(define (%type-signature->sexp sig)
  (syntax->datum (xp.type-signature-tags sig)))

(define-syntax check-expand-time-signature-violation
  (syntax-rules (=>)
    ((_ ?input-form => ?expected-signature-sexp ?returned-signature-sexp)
     (check
	 (try
	     (eval (quote ?input-form) EVAL-ENVIRONMENT)
	   (catch E
	     ((xp.&expand-time-retvals-signature-violation)
	      #;(print-condition E)
	      (values (%type-signature->sexp (xp.expand-time-retvals-signature-violation-expected-signature E))
		      (%type-signature->sexp (xp.expand-time-retvals-signature-violation-returned-signature E))))
	     (else E)))
       => (quote ?expected-signature-sexp) (quote ?returned-signature-sexp)))
    ))


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

  #t)


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
    => (<fixnum> <flonum>) (<fixnum> <string>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum>) (values "A" 2.0))
    => (<fixnum> <flonum>) (<string> <flonum>))

  ;;Any tuple of values matches.
  (doit <list>			1				=> 1)
  (doit <list>			(values 1 2 3)			=> 1 2 3)

  ;;Zero values, empty signature.
  (check-for-true
   (begin
     (assert-signature () (values))
     #t))
  (check-for-true
   (call-with-values
       (lambda ()
	 (assert-signature-and-return () (values)))
     (lambda () #t)))

  (check-expand-time-signature-violation
      (assert-signature () 1)
    => () (<fixnum>))

  (check-expand-time-signature-violation
      (assert-signature () (values 1 2.0))
    => () (<fixnum> <flonum>))

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
    => <list-of-flonums> (<fixnum> <string>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> . <list-of-flonums>) (values 1 "A"))
    => (<fixnum> . <list-of-flonums>) (<fixnum> <string>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum> . <list-of-strings>) (values 1))
    => (<fixnum> <flonum> . <list-of-strings>) (<fixnum>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum> . <list-of-strings>) (values 1.0))
    => (<fixnum> <flonum> . <list-of-strings>) (<flonum>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum> . <list-of-strings>) (values 1 2))
    => (<fixnum> <flonum> . <list-of-strings>) (<fixnum> <fixnum>))

  (check-expand-time-signature-violation
      (assert-signature (<fixnum> <flonum> . <list-of-strings>) (values 1.0 2.0))
    => (<fixnum> <flonum> . <list-of-strings>) (<flonum> <flonum>))

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
       (assert-signature-and-return (<fixnum>) (unsafe-cast <fixnum> (read))))
    => '((primitive read)))

  (when #f
    (expansion-of
     (assert-signature-and-return (<fixnum>) (read))))

;;; --------------------------------------------------------------------

  (parametrise ((print-gensym #f))
    (begin-for-syntax
      (xp.generate-descriptive-gensyms? #t))

    (when #f
      (print-expansion
       (assert-signature-and-return (<fixnum> <flonum> <string>) (read))))

    (when #f
      (print-expansion
       (assert-signature (<fixnum> <flonum> <string>) (read))))

    (when #f
      (print-expansion
       (assert-signature-and-return <list-of-fixnums> (read))))

    (when #f
      (print-expansion
       (assert-signature-and-return (<fixnum> <flonum> . <list-of-fixnums>) (read))))

    (when #f
      (print-expansion
       (assert-signature (<fixnum> <flonum> . <list-of-fixnums>) (read))))

    (void))

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
