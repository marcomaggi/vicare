;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for LALR, miscellaneous stuff
;;;Date: Thu Aug  6, 2009
;;;
;;;Abstract
;;;
;;;	Miscellaneous tests for LALR, GLR driver.
;;;
;;;Copyright (c) 2009-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (prefix (nausicaa parser-tools lalr) lalr.)
  (prefix (nausicaa parser-tools lexical-tokens) lt.)
  (prefix (nausicaa parser-tools source-locations) sl.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa parser tools: LALR GLR driver \n")


;;;; helpers

(define debugging
  (make-parameter #f))

(define-constant EOI-TOKEN
  (lt.<end-of-input> ()))

(define (make-lexer list-of-tokens)
  ;;Return a lexer closure  drawing tokens from the list LIST-OF-TOKENS.
  ;;When the list is empty, return the EOI-TOKEN.
  ;;
  (lambda ()
    (if (null? list-of-tokens)
	EOI-TOKEN
      (begin0
	  (car list-of-tokens)
	(set! list-of-tokens (cdr list-of-tokens))))))

(define (make-token category value)
  (lt.<lexical-token> ((lt.category: category)
		       (lt.value:    value)
		       (lt.length:   0))))

(define (make-error-handler yycustom)
  ;;Return  an error  handler closure  that calls  YYCUSTOM with  a pair
  ;;describing the offending token.  To just return the pair invoke as:
  ;;
  ;;	(make-error-handler (lambda x x))
  ;;
  (lambda (message (token lt.<lexical-token>))
    (yycustom `(error-handler . ,(token value)))))

(define (debug:print-tables doit? terminals non-terminals)
  (when doit?
    (let ((port (current-output-port)))
      (lalr.lalr-parser (lalr.output-port:	port)
			(lalr.expect:		#f)
			(lalr.terminals:	terminals)
			(lalr.rules:		non-terminals))
      (newline port)
      (newline port))))


(parameterise ((check-test-name 'basics))

;;;Test very basic grammars.

  (define (error-handler message (token lt.<lexical-token>))
    (cons message (token value)))

  (define (doit-1 . tokens)
    ;;A grammar that only accept a single terminal as input.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr.lalr-parser (lalr.output-value: #t)
						  (lalr.parser-type: 'glr)
						  (lalr.expect: #f)
						  (lalr.terminals: '(A))
						  (lalr.rules: '((e (A) : $1)))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-2 . tokens)
    ;;A grammar that only accept a single terminal or the EOI.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr.lalr-parser (lalr.output-value: #t)
						  (lalr.parser-type: 'glr)
						  (lalr.expect: 0)
						  (lalr.terminals: '(A))
						  (lalr.rules: '((e (A) : $1
								    ()  : 0)))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-3 . tokens)
    ;;A grammar that accepts fixed sequences of a single terminal or the
    ;;EOI.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr.lalr-parser (lalr.output-value: #t)
						  (lalr.parser-type: 'glr)
						  (lalr.expect: #f)
						  (lalr.terminals: '(A))
						  (lalr.rules: '((e (A)     : (list $1)
								    (A A)   : (list $1 $2)
								    (A A A) : (list $1 $2 $3)
								    ()      : 0)))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-4 . tokens)
    ;;A grammar accepting a sequence  of equal tokens.  The return value
    ;;is the value of the last parsed token.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr.lalr-parser (lalr.output-value: #t)
						  (lalr.parser-type: 'glr)
						  (lalr.expect: #f)
						  (lalr.terminals: '(A))
						  (lalr.rules: '((e (e A) : $2
								    (A)   : $1
								    ()    : 0)))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-5 . tokens)
    ;;A grammar accepting a sequence  of equal tokens.  The return value
    ;;is the list of values.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr.lalr-parser (lalr.output-value: #t)
						  (lalr.parser-type: 'glr)
						  (lalr.expect: #f)
						  (lalr.terminals: '(A))
						  (lalr.rules: '((e (e A) : (cons $2 $1)
								    (A)   : (list $1)
								    ()    : (list 0))))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (debug:print-tables #f
		      '(A)
		      '((e (A)     : (list $1)
			   (A A)   : (list $1 $2)
			   (A A A) : (list $1 $2 $3)
			   ()      : 0)))

;;; --------------------------------------------------------------------

  (check
      (doit-1 (make-token 'A 1))
    => '(1))

  (check
      (doit-1)
    => `())

  (check
    ;;Parse correctly the first A  and reduce it.  The second A triggers
    ;;an  error which  empties  the  stack and  consumes  all the  input
    ;;tokens.  Finally, an unexpected end-of-input error is returned.
      (doit-1 (make-token 'A 1)
	      (make-token 'A 2)
	      (make-token 'A 3))
    => `())

;;; --------------------------------------------------------------------

  (check
      (parameterise ((debugging #f))
	(doit-2))
    => '(0))

  (check
      (doit-2 (make-token 'A 1))
    => '(1))

  (check
    ;;Parse correctly the first A  and reduce it.  The second A triggers
    ;;an  error which  empties  the  stack and  consumes  all the  input
    ;;tokens.  Finally, an unexpected end-of-input error is returned.
      (parameterise ((debugging #f))
	(doit-2 (make-token 'A 1)
		(make-token 'A 2)
		(make-token 'A 3)))
    => `())

;;; --------------------------------------------------------------------

  (check
    (parameterise ((debugging #f))
      (doit-3 (make-token 'A 1)))
    => '((1)))

  (check
      (doit-3 (make-token 'A 1)
	      (make-token 'A 2))
    => '((1 2)))

  (check
      (doit-3 (make-token 'A 1)
	      (make-token 'A 2)
	      (make-token 'A 3))
    => '((1 2 3)))

  (check
      (doit-3)
    => '(0))

;;; --------------------------------------------------------------------

  (check
      (doit-4)
    => '(0))

  (check
      ;;Two  results because there  is a  shift/reduce conflict,  so two
      ;;processes are generated.
      (doit-4 (make-token 'A 1))
    => '(1 1))

  (check
      ;;Two  results because there  is a  shift/reduce conflict,  so two
      ;;processes are generated.  Notice that the rules:
      ;;
      ;;  (e A) (A)
      ;;
      ;;generate only one conflict when the second "A" comes.  The third
      ;;"A" comes when the state is inside the rule "(e A)", so there is
      ;;no conflict.
      ;;
      (doit-4 (make-token 'A 1)
	      (make-token 'A 2)
	      (make-token 'A 3))
    => '(3 3))

;;; --------------------------------------------------------------------

  (check
      (doit-5)
    => '((0)))

  (check
      (doit-5 (make-token 'A 1))
    => '((1 0)
	 (1)))

  (check
      (doit-5 (make-token 'A 1)
	      (make-token 'A 2))
    => '((2 1 0)
	 (2 1)))

  (check
      (doit-5 (make-token 'A 1)
	      (make-token 'A 2)
	      (make-token 'A 3))
    => '((3 2 1 0)
	 (3 2 1)))

  #t)


(parameterise ((check-test-name 'script-expression))

;;;This is the grammar of the (lalr) documentation in Texinfo format.

  (define terminals
    '(N O C T
	(left: A)
	(left: M)
	(nonassoc: U)))

  (define non-terminals
    '((script	(lines)		: (reverse $1))

      (lines	(lines line)	: (cons $2 $1)
		(line)		: (list $1))

      (line	(T)		: #\newline
		(E T)		: $1
		(error T)	: (list 'error-clause $2))

      (E	(N)		: $1
		(E A E)		: ($2 $1 $3)
		(E M E)		: ($2 $1 $3)
		(A E (prec: U))	: ($1 $2)
		(O E C)		: $2)))

  (define (doit . tokens)
    (let* ((lexer	(make-lexer tokens))
	   (make-parser (lalr.lalr-parser (lalr.output-value: #t)
					  (lalr.expect: #f)
					  (lalr.parser-type: 'glr)
					  (lalr.terminals: terminals)
					  (lalr.rules: non-terminals)))
           (parser	(make-parser)))
      (parser lexer (make-error-handler (lambda x x)))))

  (debug:print-tables #f terminals non-terminals)

;;; --------------------------------------------------------------------
;;; Correct input.

  (check
      (parameterise ((debugging #f))
	(doit (make-token 'T #\newline)))
    => '((#\newline)))

  (check	;correct input
      (doit (make-token 'N 1)
	    (make-token 'T #\newline))
    => '((1)))

  (check	;correct input
      (doit  (make-token 'N 1)
	     (make-token 'A +)
	     (make-token 'N 2)
	     (make-token 'T #\newline))
    => '((3)))

  (check	 ;correct input
      (doit (make-token 'N 1)
	    (make-token 'A +)
	    (make-token 'N 2)
	    (make-token 'M *)
	    (make-token 'N 3)
	    (make-token 'T #\newline))
    => '((9)
	 (7)))

  (check
      (doit (make-token 'N 10)
	    (make-token 'M *)
	    (make-token 'N 2)
	    (make-token 'A +)
	    (make-token 'N 3)
	    (make-token 'T #\newline))
    => '((23)))

  (check	;correct input
      (doit  (make-token 'O #\()
	     (make-token 'N 1)
	     (make-token 'A +)
	     (make-token 'N 2)
	     (make-token 'C #\))
	     (make-token 'M *)
	     (make-token 'N 3)
	     (make-token 'T #\newline))
    => '((9)))

  (check  	;correct input
    (parameterise ((debugging #f))
      (doit (make-token 'O #\()
	    (make-token 'N 1)
	    (make-token 'A +)
	    (make-token 'N 2)
	    (make-token 'C #\))
	    (make-token 'M *)
	    (make-token 'N 3)
	    (make-token 'T #\newline)
	    (make-token 'N 4)
	    (make-token 'M /)
	    (make-token 'N 5)
	    (make-token 'T #\newline)))
    => '((9 4/5)))

  #t)


;;;; done

(check-report)

;;; end of file
