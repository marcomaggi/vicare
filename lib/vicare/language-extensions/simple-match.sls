;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: destructuring match syntax
;;;Date: Sat Apr 20, 2013
;;;
;;;Abstract
;;;
;;;	By putting  everything in  the body of  a macro  transformer, we
;;;	have no problems in using the MATCH syntax in the boot image.
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare language-extensions simple-match)
  (export match else)
  (import (vicare)
    (prefix (except (vicare unsafe operations)
		    bytevector=)
	    $)
    (only (ikarus system $bytevectors)
	  $bytevector=))


#|
The macro use:

   (match 3
     (1
      (display 1))
     (2
      (display 2)))

is expanded to:

   (letrec ((clause0 (lambda (expr)
		       (if (and (fixnum? expr)
				(fx=? expr 1))
			   (display 1)
			 (clause1 expr))))
            (clause1 (lambda (expr)
		       (if (and (fixnum? expr)
				(fx=? expr 2))
			   (display 2)
			 (failed-match-error expr))))
            (failed-match-error
             (lambda (expr)
	       (assertion-violation 'match
		 "failed destructuring match: no matching clause"
		 expr))))
     (clause0 expr clause2))

|#


(define-syntax match
  (lambda (stx)
    (define (main stx)
      (syntax-case stx (else)
	((_)
	 #'(values))

	((_ ?expr)
	 #'(begin ?expr))

	((_ ?expr ?clause0 ?clause ... (else ?else-body0 ?else-body ...))
	 (parse-clauses-with-else #'?expr
				  (syntax->list #'(?clause0 ?clause ...))
				  #'(?else-body0 ?else-body ...)))

	((_ ?expr ?clause0 ?clause ...)
	 (parse-clauses-without-else #'?expr
				     (syntax->list #'(?clause0 ?clause ...))))

	))

    (define (parse-clauses-without-else expr-stx clauses)
      (parse-clauses expr-stx clauses
		     #'(lambda (expr)
			 (assertion-violation 'match
			   "failed destructuring match: no matching clause"
			   expr))))

    (define (parse-clauses-with-else expr-stx clauses else-body)
      (parse-clauses expr-stx clauses
		     #`(lambda (expr) . #,else-body)))

    (define (parse-clauses expr-stx clauses fail-thunk-stx)
      (with-syntax (((CLAUSE-ID0 CLAUSE-ID ...) (generate-temporaries clauses)))
	(define funcs-stx
	  (let recur ((clauses     clauses)
		      (clause-ids  (syntax->list #'(CLAUSE-ID ... failed-match-error))))
	    (if (null? clauses)
		'()
	      (let ((fail-form-stx #`(#,(car clause-ids) expr)))
		(cons #`(lambda (expr)
			  #,(parse-single-clause (car clauses) #'expr fail-form-stx))
		      (recur (cdr clauses)
			     (cdr clause-ids)))))))
	(with-syntax
	    (((CLAUSE-THUNK0 CLAUSE-THUNK ...)
	      funcs-stx))
	  #`(letrec ((CLAUSE-ID0 CLAUSE-THUNK0)
		     (CLAUSE-ID  CLAUSE-THUNK)
		     ...
		     (failed-match-error #,fail-thunk-stx))
	      (CLAUSE-ID0 #,expr-stx)))))


;;;; clause parsing

(define (parse-single-clause clause expr-id fail-form-stx)
  ;;Parse a  match CLAUSE  and return a  syntax object  representing the
  ;;code to match an expression against the pattern.  Assume the initial
  ;;expression is bound  to the identifier EXPR-ID.   If matching fails:
  ;;the generated code will evaluate  the result of expanding the syntax
  ;;object FAIL-FORM-STX.
  ;;
  (syntax-case clause ()
    ;;Pattern without body.
    ((?pattern)
     (parse-pattern #'pattern expr-id #'(values) fail-form-stx))

    ;;Pattern with body.
    ((?pattern ?body0 ?body ...)
     (parse-pattern #'?pattern expr-id #'(begin ?body0 ?body ...) fail-form-stx))

    ;;Syntax error.
    (_
     (synner "invalid syntax in match clause" clause))))


;;;; pattern parsing

(define (parse-pattern pattern-stx in-expr-stx success-form-stx fail-form-stx)
  (define (recurse pattern-stx in-expr-stx)
    (parse-pattern pattern-stx in-expr-stx success-form-stx fail-form-stx))
  (with-syntax (((expr)        (generate-temporaries #'(#f)))
		(IN-EXPR       in-expr-stx)
		(SUCCESS-FORM  success-form-stx)
		(FAIL-FORM     fail-form-stx))
    (syntax-case pattern-stx (let quote quasiquote)

      ;;This matches  the end of  a clause when  the clause is  a proper
      ;;list.
      ;;
      (()
       #'(let ((expr IN-EXPR))
	   (if (null? expr)
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a variable assignment.
      ;;
      ((let ?id)
       (identifier? #'?id)
       #`(let ((?id IN-EXPR))
	   SUCCESS-FORM))

      ;;Match a quoted datum.
      ;;
      ((quote ?datum)
       (cond ((symbol? (syntax->datum #'?datum))
	      #`(let ((expr IN-EXPR))
		  (if (eq? expr (quote ?datum))
		      SUCCESS-FORM
		    FAIL-FORM)))
	     (else
	      #`(let ((expr IN-EXPR))
		  (if (equal? expr (quote ?datum))
		      SUCCESS-FORM
		    FAIL-FORM)))))

      ;;Match a quasiquoted datum.
      ;;
      ((quasiquote ?datum)
       #`(let ((expr IN-EXPR))
	   (if (equal? expr (quasiquote ?datum))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a pair.
      ;;
      ((?car . ?cdr)
       #`(let ((expr IN-EXPR))
	   (if (pair? expr)
	       #,(parse-pattern #'?car #'(car expr)
				(recurse #'?cdr #'(cdr expr))
				fail-form-stx)
	     FAIL-FORM)))

      ;;Match an empty vector.
      ;;
      (#()
       #'(let ((expr IN-EXPR))
	   (if (and (vector? expr) ($fxzero? ($vector-length expr)))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a non-empty vector.
      ;;
      (#(?item0 ?item ...)
       #`(let ((expr IN-EXPR))
	   (if (vector? expr)
	       #,(recurse #'?item0 #'(?item ...))
	     FAIL-FORM)))

      ;;Match anything and ignore.
      ;;
      (?id
       (and (identifier? #'?id)
	    (eq? '_ (syntax->datum #'?id)))
       success-form-stx)

      ;;Match a variable reference.
      ;;
      (?id
       (identifier? #'?id)
       #`(let ((expr IN-EXPR))
	   (if (equal? expr ?id)
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match the boolean true.
      ;;
      (#t
       #`(let ((expr IN-EXPR))
	   (if (and (boolean? expr) expr) SUCCESS-FORM FAIL-FORM)))

      ;;Match the boolean false.
      ;;
      (#f
       #`(let ((expr IN-EXPR))
	   (if (and (boolean? expr) (not expr)) SUCCESS-FORM FAIL-FORM)))

      ;;Match a character datum.
      ;;
      (?datum
       (char? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (char? expr) ($char= expr ?datum))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a fixnum datum.
      ;;
      (?datum
       (fixnum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (fixnum? expr) ($fx= expr ?datum))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a bignum datum.
      ;;
      (?datum
       (bignum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (bignum? expr) (= expr ?datum))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a  NaN datum.  This  clause must  come before the  one for
      ;;flonums.
      ;;
      (?datum
       (let ((D (syntax->datum #'?datum)))
	 (and (number? D)
	      (nan?    D)))
       #`(if (nan? IN-EXPR)
	     SUCCESS-FORM
	   FAIL-FORM))

      ;;Match an infinite  datum.  This clause must come  before the one
      ;;for flonums.
      ;;
      (?datum
       (let ((D (syntax->datum #'?datum)))
	 (and (number?   D)
	      (infinite? D)))
       #`(let ((expr IN-EXPR))
	   (if (and (infinite? expr) (= expr ?datum))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a flonum datum.
      ;;
      (?datum
       (flonum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (flonum? expr) ($fl= expr ?datum))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a ratnum datum.
      ;;
      (?datum
       (ratnum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (ratnum? expr)
		    (= ($ratnum-num expr) ($ratnum-num ?datum))
		    (= ($ratnum-den expr) ($ratnum-den ?datum)))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a cflonum datum.
      ;;
      (?datum
       (cflonum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (cflonum? expr)
		    ($fl= ($cflonum-real expr) ($cflonum-real ?datum))
		    ($fl= ($cflonum-imag expr) ($cflonum-imag ?datum)))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a compnum datum.
      ;;
      (?datum
       (compnum? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (compnum? expr)
		    (= ($compnum-real expr) ($compnum-real ?datum))
		    (= ($compnum-imag expr) ($compnum-imag ?datum)))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match  a number  datum.  This  clause should  never be  included
      ;;because the clauses  above should have matches all  the kinds of
      ;;numbers.
      ;;
      (?datum
       (number? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (number? expr) (= expr ?datum))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a string datum.
      ;;
      (?datum
       (string? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (string? expr) (string=? expr ?datum))
	       SUCCESS-FORM
	     FAIL-FORM)))

      ;;Match a bytevector datum.
      ;;
      (?datum
       (bytevector? (syntax->datum #'?datum))
       #`(let ((expr IN-EXPR))
	   (if (and (bytevector? expr)
		    ($bytevector= expr (quote ?datum)))
	       SUCCESS-FORM
	     FAIL-FORM)))

      (_
       (synner "invalid syntax in match pattern" pattern-stx))

      )))


;;;; helpers

(define (syntax->list stx)
  (syntax-case stx ()
    (()
     '())
    ((?car . ?cdr)
     (cons #'?car (syntax->list #'?cdr)))
    (_
     (synner "expected syntax object representing proper list" stx))))

(define (synner message subform)
  (syntax-violation 'match message stx subform))


;;;; end of transformer

(let ((out (main stx)))
  #;(pretty-print (syntax->datum out) (current-error-port))
  out)))


;;;; done

)

;;; end of file
