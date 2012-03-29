;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: parser logic constructor
;;;Date: Thu Mar 29, 2012
;;;
;;;Abstract
;;;
;;;	This library defines a parser constructor to be used to generate
;;;	lexers, parsers,  reader components accepting  Scheme characters
;;;	as input and producing whatever token is needed.  Characters are
;;;	drawn from an unspecified source: the parser generator defines a
;;;	macro  embedding the parser  logic, and  later we  specialise it
;;;	with a device logic macro specifying how to draw characters.
;;;
;;;	  This library is  derived from the original Ikarus  code and it
;;;	is used by Vicare to generate both the parser for STRING->NUMBER
;;;	and the  "string to number"  parser embedded in the  source code
;;;	reader.
;;;
;;;Copyright (C) 2008,2009  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare parser-logic)
  (export
    define-parser-logic
    :introduce-device-arguments
    :generate-eof-then-chars-tests
    :generate-delimiter-test
    :unexpected-eof-error
    :fail)
  (import (rnrs)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (only (vicare syntactic-extensions)
	  define-auxiliary-syntaxes))


(define-auxiliary-syntaxes
  :introduce-device-arguments
  :generate-eof-then-chars-tests
  :generate-delimiter-test
  :unexpected-eof-error
  :fail)


(define-syntax define-parser-logic
  ;;Define the  parser logic through  a list of  symbolic subexpressions
  ;;representing operator function  specifications.  The parser logic is
  ;;an "abstract parser" which must be specialised with the input device
  ;;logic.
  ;;
  ;;?PARSER-DEFINER must be the  identifier that will become the keyword
  ;;of the concrete parser-definition macro.
  ;;
  ;;?NEXT must be  the identifier that will become  the keyword bound to
  ;;the macro  used to call  the "next" operator function  inserting the
  ;;input   device   arguments.   ?NEXT   is   used   in  the   operator
  ;;subexpressions.
  ;;
  ;;?FAIL must be  the identifier that will become  the keyword bound to
  ;;the  macro used  to return  an error  to the  caller of  an operator
  ;;function.  ?FAIL is used in the operator subexpressions.
  ;;
  ;;?OPERATOR-NAME must be the  identifier to which an operator function
  ;;will be  bound.  This  identifier is in  the lexical context  of the
  ;;DEFINE-PARSER-LOGIC use  form, which  is different from  the lexical
  ;;context of a use of the ?PARSER-DEFINER macro.
  ;;
  ;;?OPERATOR-SPECIFIC-ARG  must be  an  identifier that  will become  a
  ;;formal argument  for an operator function.  These  arguments are the
  ;;operator-specific ones.
  ;;
  ;;?OPERATOR-CLAUSE must be a symbolic expression representing accepted
  ;;input conditions for an operator function.
  ;;
  (lambda (x)
    (syntax-case x ()
      ((_ ?parser-definer ?next ?fail
	  (?operator-name (?operator-specific-arg ...) ?operator-clause ...)
	  ...)
       (with-syntax
	   ;;Insert:
	   ;;
	   ;;  #'(?operator-name ...)
	   ;;
	   ;;in a syntax object so that it can be embedded in the output
	   ;;form  of this  macro and  later extracted  intact  from the
	   ;;input form of DEFINE-PARSER.
	   ((ORIGINAL-OPERATOR-NAMES (datum->syntax #'foo #'(?operator-name ...))))
	 #'(define-syntax ?parser-definer
	     (syntax-rules ()
	       ((_ ??device-logic (??public-operator-name (... ...)))
		(define-parser (??public-operator-name (... ...))
		  ??device-logic ?next ?fail
		  ORIGINAL-OPERATOR-NAMES
		  (?operator-name (?operator-specific-arg ...) ?operator-clause ...) ...))))
	 ))
      )))

(define-syntax define-parser
  ;;This is the true parser logic generator.
  ;;
  (lambda (x)
    (define (lookup original-operator-names actual-operator-names)
      (lambda (public-operator-name)
	;;Check that a requested public operator name is indeed the name
	;;of  an operator  function in  the abstract  parser  logic.  If
	;;successful:  return  an  identifier  representing  the  actual
	;;parser operator name.
	;;
	(let loop ((ls1 original-operator-names)
		   (ls2 actual-operator-names))
	  (cond ((null? ls1)
		 (assertion-violation 'define-parser
		   "unknown requested public parser operator name"
		   public-operator-name))
		((bound-identifier=? public-operator-name (unsafe.car ls1))
		 (unsafe.car ls2))
		(else
		 (loop (unsafe.cdr ls1) (unsafe.cdr ls2)))))))
    (define (syntax->list stx)
      ;;Given a syntax object STX holding a list, unwrap it and return a
      ;;proper list holding the component syntax objects.
      ;;
      (syntax-case stx ()
	((?car . ?cdr)
	 (cons #'?car (syntax->list #'?cdr)))
	(() '())))
    (syntax-case x ()
      ((_ (?public-operator-name ...) ?device-logic ?next ?fail
	  ?original-operator-names
	  (?operator-name (?operator-arg ...) ?operator-clause ...) ...)
       (with-syntax (((OPERATOR-NAME ...)
		      (map (lookup (syntax->list (syntax->datum #'?original-operator-names))
				   (syntax->list #'(?operator-name ...)))
			(syntax->list #'(?public-operator-name ...)))))
	 #'(begin
	     ;;This form expands to an operator function bound to a name
	     ;;in the lexical scope of the DEFINE-PARSER-LOGIC use.
	     (?device-logic :introduce-device-arguments
			    %generate-operator ?device-logic ?next ?fail ?operator-name
			    (?operator-arg ...)
			    (?operator-clause ...))
	     ...

	     ;;This form  aliases an operator name in  the lexical scope
	     ;;of the DEFINE-PARSER-LOGIC use to an operator name in the
	     ;;lexical scope of the concrete parser definition.
	     (define ?public-operator-name OPERATOR-NAME)
	     ...))))))

(define-syntax %generate-operator
  ;;Define an operator function.
  ;;
  (syntax-rules ()
    ((_ (?device-arg ...) ?device-logic ?next ?fail
	?operator-name (?operator-arg ...) (?operator-clause ...))
     (define (?operator-name ?device-arg ... ?operator-arg ...)
       ;;Introduce the identifier  "ch" used to bind the  next char from
       ;;the input device.
       (?device-logic :generate-eof-then-chars-tests ch
		      ?next ?fail (?device-arg ...)
		      (%generate-end-of-input-form
		       ?device-logic (?device-arg ...)
		       ?operator-clause ...)
		      (%generate-parse-more-chars-form
		       ?device-logic (?device-arg ...)
		       ch (%generate-delimiter-test
			   ?device-logic (?device-arg ...)
			   ch ?operator-clause ...)
		       ?operator-clause ...)
		      )))))


(define-syntax %generate-end-of-input-form
  ;;Iterate through the clauses of an operator looking for the EOF one.
  ;;
  ;;The literal EOF must be a free identifier.
  ;;
  (syntax-rules (eof)
    ;;End-of-input was found from the  input device, but there is no EOF
    ;;clause  specified for  this  parser operator:  end-of-input is  an
    ;;error here.
    ((_ ?device-logic ?device-arg-list)
     (?device-logic :unexpected-eof-error ?device-arg-list))
    ;;End-of-input was found  from the input device and  there is an EOF
    ;;clause specified  for this  parser operator: end-of-input  is fine
    ;;here, so execute the clause.
    ((_ ?device-logic ?device-arg-list ((eof) ?then-form) . ?other-clauses)
     ?then-form)
    ;;Discard ?NOT-EOF-CLAUSE and recurse.
    ((_ ?device-logic ?device-arg-list ?not-eof-clause . ?other-clauses)
     (%generate-end-of-input-form ?device-logic ?device-arg-list . ?other-clauses))))

(define-syntax %generate-delimiter-test
  ;;Expand to a single form testing if the character bound to ?CH-VAR is
  ;;an end-of-number delimiter.  Both the test and the responsibility to
  ;;perfom  an action  are delegated  to  the device  logic; the  parser
  ;;merely   suggests   a   is-delimiter   continuation   form   and   a
  ;;is-not-delimiter continuation form.
  ;;
  (syntax-rules (eof)
    ;;There  is  no  EOF  clause  specified  for  the  current  operator
    ;;function: expand to the error form.
    ((_ ?device-logic ?device-arg-list ?ch-var)
     (?device-logic :generate-delimiter-test ?ch-var
		    (?device-logic :fail ?device-arg-list ?ch-var)
		    (?device-logic :fail ?device-arg-list ?ch-var)))
    ;;There  is  an  EOF  clause  specified  for  the  current  operator
    ;;function; such clause is used to process both the end-of-input and
    ;;end-of-number conditions.
    ((_ ?device-logic ?device-arg-list ?ch-var ((eof) ?then-clause) . ?other-clauses)
     (?device-logic :generate-delimiter-test ?ch-var
		    ?then-clause
		    (?device-logic :fail ?device-arg-list ?ch-var)))
    ;;Discard ?NOT-EOF-CLAUSE and recurse.
    ((_ ?device-logic ?device-arg-list ?ch-var ?not-eof-clause . ?other-clauses)
     (%generate-delimiter-test ?device-logic ?device-arg-list ?ch-var . ?other-clauses))))

(define-syntax %generate-parse-more-chars-form
  ;;Recursively  iterate over the  clauses of  an operator  function and
  ;;expand  to code  that checks  them  against the  character bound  to
  ;;?CH-VAR.
  ;;
  (syntax-rules (eof =>)

    ;;No more  operator clauses.  Expand to  the end-of-number delimiter
    ;;characters test.
    ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form)
     ?test-delimiter-form)

    ;;The operator clause specifies  that end-of-input is acceptable for
    ;;this  operator function.   Skip  this clause  here because  (being
    ;;checked  first among  all the  clauses) it  is handled  by another
    ;;syntax.
    ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	((eof) ?then-form) . ?other-operator-clauses)
     (%generate-parse-more-chars-form ?device-logic ?device-arg-list ?ch-var
				      ?test-delimiter-form . ?other-operator-clauses))

    ;;The  clause specifies  a test  function to  be applied  to ?CH-VAR
    ;;along with some additional arguments.
    ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	((?test . ?test-args) => ?test-result ?then-form) . ?other-operator-clauses)
     (cond ((?test ?ch-var . ?test-args)
	    => (lambda (?test-result) ?then-form))
	   (else
	    (%generate-parse-more-chars-form ?device-logic ?device-arg-list ?ch-var
					     ?test-delimiter-form . ?other-operator-clauses))))

    ;;The  operator  clause specifies  a  list  of  characters to  match
    ;;against ?CH-VAR.
    ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	((?char ...) ?then-form) . ?other-operator-clauses)
     (if (or (unsafe.char= ?char ?ch-var)
	     ...)
	 ?then-form
       (%generate-parse-more-chars-form ?device-logic ?device-arg-list ?ch-var
					?test-delimiter-form . ?other-operator-clauses)))))


;;;; done

)

;;; end of file
