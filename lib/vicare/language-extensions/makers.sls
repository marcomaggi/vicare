;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: helper macros for constructors
;;;Date: Sat May 22, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare language-extensions makers)
  (export
    define-maker
    ;; auxiliary syntaxes
    mandatory optional with without)
  (import (except (vicare) with)
    (for (vicare language-extensions case-identifiers) expand))


(define-auxiliary-syntaxes
  mandatory
  optional
  with
  without)


(define-syntax define-maker
  (let*-constants
      ((who		'define-maker)
       (clause-specs	(syntax-clauses-validate-specs
			 (list (make-syntax-clause-spec #'mandatory 0 1 0 0      '() (list #'optional))
			       (make-syntax-clause-spec #'optional  0 1 0 0      '() (list #'mandatory))
			       (make-syntax-clause-spec #'with      0 1 0 +inf.0 '() '())
			       (make-syntax-clause-spec #'without   0 1 0 +inf.0 '() '())))))
    (define-struct maker-clause-spec
      (keyword default mandatory? with without))
    (lambda (stx)
      (define (main stx)
	#;(debug-print (syntax->datum stx))
	(syntax-case stx ()
	  ((_ ?maker-name ?maker-sexp ?definition-clauses)
	   (receive-and-return (output-form)
	       (receive (maker-who-id variable-args-stx)
		   (parse-maker-name #'?maker-name)
		 (let ((maker-sexp-stx (parse-maker-sexp #'?maker-sexp)))
		   (make-output-form maker-who-id variable-args-stx maker-sexp-stx
				     (parse-definition-clauses #'?definition-clauses))))
	     #;(debug-print (syntax->datum output-form))
	     (void)))
	  (_
	   (synner "invalid maker definition"))))

      (define (parse-maker-name maker-name-stx)
	(syntax-case maker-name-stx ()
	  ;;Name and variable arguments
	  ((?maker-who ?variable-arg-expr ...)
	   (let ((maker-who-id		#'?maker-who)
		 (variable-args-stx	#'(?variable-arg-expr ...)))
	     (cond ((not (identifier? maker-who-id))
		    (synner "expected identifier as maker name in maker definition" maker-who-id))
		   ((not (all-identifiers? variable-args-stx))
		    (synner "expected identifiers as positional argument names" variable-args-stx))
		   (else
		    (values maker-who-id variable-args-stx)))))
	  ;;Name only, no variable arguments.
	  (?maker-who
	   (let ((maker-who-id		#'?maker-who))
	     (if (identifier? maker-who-id)
		 (values maker-who-id '())
	       (synner "expected identifier as maker name in maker definition" maker-who-id))))))

      (define (parse-maker-sexp maker-sexp-stx)
	(syntax-case maker-sexp-stx ()
	  ;;The  actual  maker expression  is  a  single identifier,  no
	  ;;mandatory arguments.
	  (?maker-id
	   (identifier? #'?maker-id)
	   #'(?maker-id))
	  ;;The actual maker  expression is a list  of expressions, with
	  ;;possible mandatory arguments.
	  ((?maker-first ?mandatory-arg-expr ...)
	   maker-sexp-stx)
	  (_
	   (synner "invalid maker sexp specification" maker-sexp-stx))))

      (define (parse-definition-clauses definition-clauses-stx)
	;;Recursive function parsing the clauses of DEFINE-MAKER.
	;;
	(syntax-case definition-clauses-stx ()
	  (()
	   '())
	  (((?keyword ?default . ?keyword-clauses) . ?other-clauses)
	   (identifier? #'?keyword)
	   (let ((knil    (make-maker-clause-spec #'?keyword #'?default #f '() '()))
		 (clauses (syntax-clauses-unwrap #'?keyword-clauses)))
	     (cons (syntax-clauses-fold-specs combine knil clause-specs clauses synner)
		   (parse-definition-clauses #'?other-clauses))))
	  (_
	   (synner "invalid maker clauses" definition-clauses-stx))))

      (define (combine maker-clause-spec definer-clause-spec args)
	;;Remember that ARGS  is a vector of vectors  of syntax objects.
	;;We know that:
	;;
	;;* All the clauses must appear 0 or 1 time; so ARGS is always a
	;;  vector of a single argument.
	;;
	(define (%with-and-without setter)
	  (let ((list-of-ids (vector->list (vector-ref args 0))))
	    (cond ((not (all-identifiers? list-of-ids))
		   (synner "expected identifiers as values for \"with\" clause" list-of-ids))
		  (else
		   (setter maker-clause-spec list-of-ids)))))
	(case-identifiers (syntax-clause-spec-keyword definer-clause-spec)
	  ((mandatory)
	   (set-maker-clause-spec-mandatory?! maker-clause-spec #t))
	  ((optional)
	   (set-maker-clause-spec-mandatory?! maker-clause-spec #f))
	  ((with)
	   (%with-and-without set-maker-clause-spec-with!))
	  ((without)
	   (%with-and-without set-maker-clause-spec-without!)))
	maker-clause-spec)

      (define (make-output-form maker-who-id variable-args-stx maker-sexp-stx keyword-specs)
	(with-syntax
	    (((CLAUSE-VAR ...)			(generate-temporaries keyword-specs))
	     ((CLAUSE-KEYWORD ...)		(map maker-clause-spec-keyword keyword-specs))
	     ((CLAUSE-MIN-OCCUR ...)		(map (lambda (spec)
						       (if (maker-clause-spec-mandatory? spec)
							   1
							 0))
						  keyword-specs))
	     (((CLAUSE-WITH ...) ...)		(map maker-clause-spec-with    keyword-specs))
	     (((CLAUSE-WITHOUT ...) ...)	(map maker-clause-spec-without keyword-specs))
	     ((CLAUSE-FIELD-DEFAULT ...)	(map maker-clause-spec-default keyword-specs))
	     (OPTIONS				(datum->syntax maker-who-id 'options))
	     (MAKE-OPTIONS			(datum->syntax maker-who-id 'make-options))
	     ((CLAUSE-FIELD ...)		(map maker-clause-spec-keyword keyword-specs))
	     ((CLAUSE-FIELD-ACCESSOR ...)	(map (lambda (spec)
						       (let ((keyword (maker-clause-spec-keyword spec)))
							 (identifier-append keyword "options-" keyword)))
						  keyword-specs))
	     ((CLAUSE-FIELD-MUTATOR ...)	(map (lambda (spec)
						       (let ((keyword (maker-clause-spec-keyword spec)))
							 (identifier-append keyword "set-options-" keyword "!")))
						  keyword-specs)))
	  #`(define-syntax #,maker-who-id
	      (let-constants
		  ((who          (quote #,maker-who-id))
		   (clause-specs (syntax-clauses-validate-specs
				  (list (make-syntax-clause-spec #'CLAUSE-KEYWORD
								 CLAUSE-MIN-OCCUR 1 1 (greatest-fixnum)
								 (list #'CLAUSE-WITH ...)
								 (list #'CLAUSE-WITHOUT ...))
					...))))
		(define-struct OPTIONS
		  (CLAUSE-FIELD ...))
		(lambda (stx)
		  (define (main stx)
		    (syntax-case stx ()
		      ((_ #,@variable-args-stx . ?clauses)
		       (let ((options (MAKE-OPTIONS #'CLAUSE-FIELD-DEFAULT ...))
			     (clauses (syntax-clauses-unwrap #'?clauses)))
			 (syntax-clauses-fold-specs combine options clause-specs clauses synner)
			 (with-syntax
			     ((CLAUSE-FIELD (CLAUSE-FIELD-ACCESSOR options))
			      ...)
			   #'(#,@maker-sexp-stx #,@variable-args-stx CLAUSE-FIELD ...))))
		      (_
		       (synner "invalid syntax in maker invocation"))))
		  (define (combine options clause-spec args)
		    ;;Remember  that  ARGS is  a  vector  of vectors  of
		    ;;syntax objects.  We know that:
		    ;;
		    ;;* All the  clauses must appear at most  1 time; so
		    ;;ARGS is always a vector of a single argument.
		    ;;
		    ;;* All  the clauses  accept 1  to (greatest-fixnum)
		    ;;arguments, so each nested vector will have zero to
		    ;;(greatest-fixnum) items.
		    ;;
		    (let ((value (syntax-case args ()
				   (#(#(?expr))
				    #'?expr)
				   (#(#(?expr (... ...)))
				    #'(list ?expr (... ...)))
				   (_
				    (synner "invalid maker clause values" args)))))
		      (case-identifiers (syntax-clause-spec-keyword clause-spec)
			((CLAUSE-KEYWORD)	(CLAUSE-FIELD-MUTATOR options value))
			...))
		    options)
		  (define synner
		    (case-lambda
		     ((message)
		      (synner message #f))
		     ((message subform)
		      (syntax-violation who message stx subform))))
		  (main stx))))))

      (define synner
	(case-lambda
	 ((message)
	  (synner message #f))
	 ((message subform)
	  (syntax-violation who message stx subform))))

      (main stx))))


;;;; done

)

;;; end of file
