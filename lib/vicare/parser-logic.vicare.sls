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
    string->token-or-false
    :introduce-device-arguments
    :generate-end-of-input-or-char-tests
    :generate-delimiter-test
    :unexpected-end-of-input
    :invalid-input-char
    :end-of-input)
  (import (rnrs)
    (vicare unsafe operations)
    (only (vicare)
	  define-auxiliary-syntaxes))


(define-auxiliary-syntaxes
  :introduce-device-arguments
  :generate-end-of-input-or-char-tests
  :unexpected-end-of-input
  :generate-delimiter-test
  :invalid-input-char
  :end-of-input)


(define-syntax define-parser-logic
  ;;Define the  parser logic through  a list of  symbolic subexpressions
  ;;representing operator function  specifications.  The parser logic is
  ;;an "abstract parser" which must be specialised with the input device
  ;;logic.
  ;;
  ;;See the documentation in Texinfo format.
  ;;
  (lambda (x)
    (define (syntax->list stx)
      ;;Given a syntax object STX holding a list, unwrap it and return a
      ;;proper list holding the component syntax objects.
      ;;
      (syntax-case stx ()
	((?car . ?cdr)
	 (cons #'?car (syntax->list #'?cdr)))
	(() '())))
    (syntax-case x ()
      ((_ ?parser-definer ?ch-var ?next ?fail
	  (?operator-name (?operator-specific-arg ...) ?operator-clause ...)
	  ...)
       (and (identifier? #'?parser-definer)
	    (identifier? #'?ch-var)
	    (identifier? #'?next)
	    (identifier? #'?fail)
	    (for-all identifier? (syntax->list #'(?operator-name ...))))
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
		  ??device-logic ?ch-var ?next ?fail
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
		((bound-identifier=? public-operator-name ($car ls1))
		 ($car ls2))
		(else
		 (loop ($cdr ls1) ($cdr ls2)))))))
    (define (syntax->list stx)
      ;;Given a syntax object STX holding a list, unwrap it and return a
      ;;proper list holding the component syntax objects.
      ;;
      (syntax-case stx ()
	((?car . ?cdr)
	 (cons #'?car (syntax->list #'?cdr)))
	(() '())))
    (syntax-case x ()
      ((_ (?public-operator-name ...) ?device-logic ?ch-var ?next ?fail
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
			    %generate-operator ?device-logic ?ch-var ?next ?fail ?operator-name
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
    ((_ (?device-arg ...) ?device-logic ?ch-var ?next ?fail
	?operator-name (?operator-arg ...) (?operator-clause ...))
     (define (?operator-name ?device-arg ... ?operator-arg ...)
       ;;Introduce the identifier  "ch" used to bind the  next char from
       ;;the input device.
       (?device-logic :generate-end-of-input-or-char-tests
		      ?ch-var ?next ?fail (?device-arg ...)
		      (%generate-end-of-input-form
		       ?device-logic (?device-arg ...)
		       ?operator-clause ...)
		      (%generate-parse-input-char-form
		       ?device-logic (?device-arg ...)
		       ?ch-var (%generate-delimiter-test
				?device-logic (?device-arg ...)
				?ch-var ?operator-clause ...)
		       ?operator-clause ...)
		      )))))

(define-syntax %generate-end-of-input-form
  ;;Recursively iterate  through the clauses of an  operator looking for
  ;;the end-of-input one.
  ;;
  (syntax-rules (:end-of-input)
    ;;No more  operator clauses.  End-of-input was found  from the input
    ;;device,  but there is  no end-of-input  clause specified  for this
    ;;parser operator: end-of-input is an error here.
    ((_ ?device-logic ?device-arg-list)
     (?device-logic :unexpected-end-of-input ?device-arg-list))
    ;;End-of-input  was found  from the  input  device and  there is  an
    ;;end-of-input   clause   specified   for  this   parser   operator:
    ;;end-of-input is fine here, so execute the clause.
    ((_ ?device-logic ?device-arg-list ((:end-of-input) ?then-form) . ?other-clauses)
     ?then-form)
    ;;Discard ?NOT-EOF-CLAUSE and recurse.
    ((_ ?device-logic ?device-arg-list ?not-eof-clause . ?other-clauses)
     (%generate-end-of-input-form ?device-logic ?device-arg-list . ?other-clauses))))

(define-syntax %generate-delimiter-test
  ;;Recursively iterate  through the clauses of an  operator looking for
  ;;an  end-of-input  one.  Expand  to  a  single  form testing  if  the
  ;;character bound to ?CH-VAR  is an end-of-lexeme delimiter.  Both the
  ;;test and the responsibility to perfom an action are delegated to the
  ;;device logic; the parser merely suggests a is-delimiter continuation
  ;;form and a is-not-delimiter continuation form.
  ;;
  (syntax-rules (:end-of-input)
    ;;No  more  operator  clauses.   There  is  no  end-of-input  clause
    ;;specified for this operator function:  in both cases expand to the
    ;;error form.
    ((_ ?device-logic ?device-arg-list ?ch-var)
     (?device-logic :generate-delimiter-test ?ch-var
		    ;;Continuation when ?CH-VAR is a delimiter.
		    (?device-logic :invalid-input-char ?device-arg-list ?ch-var)
		    ;;Continuation when ?CH-VAR is not a delimiter.
		    (?device-logic :invalid-input-char ?device-arg-list ?ch-var)))
    ;;There  is  an  end-of-input  clause specified  for  this  operator
    ;;function; such clause is used to process both the end-of-input and
    ;;end-of-lexeme conditions.
    ((_ ?device-logic ?device-arg-list ?ch-var ((:end-of-input) ?then-clause) . ?other-clauses)
     (?device-logic :generate-delimiter-test ?ch-var
		    ;;Continuation when ?CH-VAR is a delimiter.
		    ?then-clause
		    ;;Continuation when ?CH-VAR is not a delimiter.
		    (?device-logic :invalid-input-char ?device-arg-list ?ch-var)))
    ;;Discard ?NOT-EOF-CLAUSE and recurse.
    ((_ ?device-logic ?device-arg-list ?ch-var ?not-eof-clause . ?other-clauses)
     (%generate-delimiter-test ?device-logic ?device-arg-list ?ch-var . ?other-clauses))))

(define-syntax %generate-parse-input-char-form
  ;;Recursively  iterate over the  clauses of  an operator  function and
  ;;expand  to code  that checks  them  against the  character bound  to
  ;;?CH-VAR.
  ;;
  (syntax-rules (:end-of-input =>)

    ;;No more  operator clauses.  Expand to  the end-of-lexeme delimiter
    ;;characters test.
    ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form)
     ?test-delimiter-form)

    ;;The operator clause specifies  that end-of-input is acceptable for
    ;;this  operator  function.   Skip  this clause  here  and  recurse,
    ;;because (being checked first among  all the clauses) it is handled
    ;;by another syntax.
    ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	((:end-of-input) ?then-form) . ?other-operator-clauses)
     (%generate-parse-input-char-form ?device-logic ?device-arg-list ?ch-var
				      ?test-delimiter-form . ?other-operator-clauses))

    ;;The  clause specifies  a test  function to  be applied  to ?CH-VAR
    ;;along with some additional arguments.
    ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	((?test . ?test-args) => ?test-result ?then-form) . ?other-operator-clauses)
     (cond ((?test ?ch-var . ?test-args)
	    => (lambda (?test-result) ?then-form))
	   (else
	    (%generate-parse-input-char-form ?device-logic ?device-arg-list ?ch-var
					     ?test-delimiter-form . ?other-operator-clauses))))

    ;;The  operator  clause specifies  a  list  of  characters to  match
    ;;against ?CH-VAR.
    ((_ ?device-logic ?device-arg-list ?ch-var ?test-delimiter-form
	((?char ...) ?then-form) . ?other-operator-clauses)
     (if (or ($char= ?char ?ch-var)
	     ...)
	 ?then-form
       (%generate-parse-input-char-form ?device-logic ?device-arg-list ?ch-var
					?test-delimiter-form . ?other-operator-clauses)))))


;;;; device logic for full string input

(define-syntax string->token-or-false
  ;;Define the device logic to parse  a lexeme from a full Scheme string
  ;;object.
  ;;
  (syntax-rules (:introduce-device-arguments
		 :generate-end-of-input-or-char-tests
		 :unexpected-end-of-input
		 :generate-delimiter-test
		 :invalid-input-char)

    ;;Introduce a list of identifiers used as device-specific arguments;
    ;;they  will  be  the  first  arguments  for  each  parser  operator
    ;;function.
    ((_ :introduce-device-arguments ?kont . ?rest)
     (?kont (input.string input.length input.index) . ?rest))

    ;;Whenever  an  input  character  is  not accepted  by  an  operator
    ;;function  this   rule  is  used   to  decide  what  to   do.   For
    ;;STRING->NUMBER the action is to return false.
    ((_ :invalid-input-char (?input.string ?input.length ?input.index) ?ch-var)
     #f)

    ;;Whenever the  end-of-input is found  by an operator that  does not
    ;;accept it as  valid, this rule is used to decide  what to do.  For
    ;;STRING->NUMBER the action is to return false.
    ((_ :unexpected-end-of-input (?input.string ?input.length ?input.index))
     #f)

    ;;This rule is used for input devices for which the lexeme string is
    ;;embedded into  a sequence of  other characters, so there  exists a
    ;;set  of characters  that  delimit the  end-of-lexeme.  The  parser
    ;;delegates  to  the  device  the responsibility  of  knowing  which
    ;;characters are delimiters, if any.
    ;;
    ;;When the input  device is a string containing  only the lexeme, as
    ;;is  the case  for  STRING->NUMBER: there  are  no delimiters,  the
    ;;end-of-lexeme  is the  end of  the  string.  We  avoid looking  at
    ;;?CH-VAR and just expand to the not-delimiter continuation form.
    ((_ :generate-delimiter-test ?ch-var ?ch-is-delimiter-kont ?ch-is-not-delimiter-kont)
     ?ch-is-not-delimiter-kont)

    ;;This  rule is  used  to generate  the  input device  tests for  an
    ;;operator  function.  First  of all  the end-of-input  condition is
    ;;checked; then the continuation form to parse an input character is
    ;;expanded.
    ((_ :generate-end-of-input-or-char-tests ?ch-var ?next ?fail
	(?input.string ?input.length ?input.index)
	?end-of-input-kont ?parse-input-char-kont)
     (let-syntax
	 ((?fail (syntax-rules ()
		   ((_) #f)))
	  (?next (syntax-rules ()
		   ((_ ?operator-name ?operator-arg (... ...))
		    (?operator-name ?input.string ?input.length ($fxadd1 ?input.index)
				    ?operator-arg (... ...))))))
       (if ($fx= ?input.index ?input.length) ;end-of-input
	   ?end-of-input-kont
	 (let ((?ch-var ($string-ref ?input.string ?input.index)))
	   ?parse-input-char-kont))))
    ))


;;;; done

)

;;; end of file
