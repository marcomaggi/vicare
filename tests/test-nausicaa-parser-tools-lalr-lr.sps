;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for LALR, miscellaneous stuff
;;;Date: Thu Aug  6, 2009
;;;
;;;Abstract
;;;
;;;	Miscellaneous tests for LALR, LR driver.
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(check-display "*** testing Nausicaa parser tools: LALR LR driver\n")


;;;; helpers

(define debugging
  (make-parameter #f))

(define-constant EOI-TOKEN
  (lt.<end-of-input> ()))

(define (make-lexer list-of-tokens)
  ;;Return a lexer closure drawing  tokens from the list LIST-OF-TOKENS.
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

(define make-error-handler
  ;;Return  an error  handler closure  that calls  YYCUSTOM with  a pair
  ;;describing the offending token.  To just return the pair invoke as:
  ;;
  ;;	(make-error-handler (lambda x x))
  ;;
  (case-lambda
   (()
    (make-error-handler (lambda x x)))
   ((yycustom)
    (lambda ((message <string>) (token lt.<lexical-token>))
      (yycustom `(error-handler . ,(token value)))))))

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
	   (make-parser		(lalr.make-lalr-parser (lalr.output-value: #t)
						       (lalr.expect: #f)
						       (lalr.terminals: '(A))
						       (lalr.rules: '((e (A) : $1)))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-2 . tokens)
    ;;A grammar that only accept a single terminal or the EOI.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr.lalr-parser (lalr.output-value: #t)
						  (lalr.expect: #f)
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
						  (lalr.expect: #f)
						  (lalr.terminals: '(A))
						  (lalr.rules: '((e (e A) : (cons $2 $1)
								    (A)   : (list $1)
								    ()    : 0)))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------

  (check
      (doit-1 (make-token 'A 1))
    => 1)

  (check
      (doit-1)
    => `("unexpected end of input" . ,(eof-object)))

  (check
      ;;Parse correctly the first A  and reduce it.  The second A triggers
      ;;an  error which  empties  the  stack and  consumes  all the  input
      ;;tokens.  Finally, an unexpected end-of-input error is returned.
      (parameterise ((debugging #f))
	(doit-1 (make-token 'A 1)
		(make-token 'A 2)
		(make-token 'A 3)))
    => `("unexpected end of input" . ,(eof-object)))

;;; --------------------------------------------------------------------

  (check
      (parameterise ((debugging #f))
	(doit-2))
    => 0)

  (check
      (doit-2 (make-token 'A 1))
    => 1)

  (check
      ;;Parse correctly the first A  and reduce it.  The second A triggers
      ;;an  error which  empties  the  stack and  consumes  all the  input
      ;;tokens.  Finally, an unexpected end-of-input error is returned.
      (parameterise ((debugging #f))
	(doit-1 (make-token 'A 1)
		(make-token 'A 2)
		(make-token 'A 3)))
    => `("unexpected end of input" . ,(eof-object)))

;;; --------------------------------------------------------------------

  (check
      (doit-3 (make-token 'A 1))
    => '(1))

  (check
      (doit-3 (make-token 'A 1)
	      (make-token 'A 2))
    => '(1 2))

  (check
      (doit-3 (make-token 'A 1)
	      (make-token 'A 2)
	      (make-token 'A 3))
    => '(1 2 3))

  (check
      (doit-3)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (doit-4)
    => 0)

  (check
      (doit-4 (make-token 'A 1))
    => 1)

  (check
      (doit-4 (make-token 'A 1)
	      (make-token 'A 2)
	      (make-token 'A 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (doit-5)
    => 0)

  (check
      (doit-5 (make-token 'A 1))
    => '(1))

  (check
      (doit-5 (make-token 'A 1)
	      (make-token 'A 2))
    => '(2 1))

  (check
      (doit-5 (make-token 'A 1)
	      (make-token 'A 2)
	      (make-token 'A 3))
    => '(3 2 1))

  #t)


(parameterise ((check-test-name 'error-recovery-1))

;;;Test error recovery with a terminator terminal.

  (define terminals
    '(NUMBER BAD NEWLINE))

  (define non-terminals
    '((script	(lines)			: (reverse $1)
		()			: '())
      (lines	(lines line)		: (cons $2 $1)
		(line)			: (list $1))
      (line	(NEWLINE)		: (list 'line $1)
		(NUMBER NEWLINE)	: (list 'line $1 $2)
		(NUMBER NUMBER NEWLINE)	: (list 'line $1 $2 $3)

		;;This semantic  action will cause "(recover  $1 $2)" to
		;;be the result of the offending line.
		(error NEWLINE)		: (list 'recover $1 $2))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler))
	   (make-parser		(lalr.lalr-parser (lalr.output-value: #t)
						  (lalr.expect: #f)
						  (lalr.terminals: terminals)
						  (lalr.rules: non-terminals)))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (debug:print-tables #f terminals non-terminals)

;;; --------------------------------------------------------------------
;;; No errors, grammar tests.

  (check
      (doit)
    => '())

  (check
      (doit (make-token 'NEWLINE #\newline))
    => '((line #\newline)))

  (check
      (doit (make-token 'NUMBER  1)
	    (make-token 'NEWLINE #\newline))
    => '((line 1 #\newline)))

  (check
      (doit (make-token 'NUMBER  1)
	    (make-token 'NUMBER  2)
	    (make-token 'NEWLINE #\newline))
    => '((line 1 2 #\newline)))

  (check
      (doit (make-token 'NUMBER  1)
	    (make-token 'NEWLINE #\newline)
	    (make-token 'NUMBER  2)
	    (make-token 'NEWLINE #\newline))
    => '((line 1 #\newline)
	 (line 2 #\newline)))

  (check
      (doit (make-token 'NUMBER  1)
	    (make-token 'NEWLINE #\newline)
	    (make-token 'NUMBER  2)
	    (make-token 'NEWLINE #\newline)
	    (make-token 'NUMBER  3)
	    (make-token 'NEWLINE #\newline))
    => '((line 1 #\newline)
	 (line 2 #\newline)
	 (line 3 #\newline)))

  (check
      (doit (make-token 'NUMBER  1)
	    (make-token 'NEWLINE #\newline)
	    (make-token 'NUMBER  2)
	    (make-token 'NEWLINE #\newline)
	    (make-token 'NUMBER  3)
	    (make-token 'NEWLINE #\newline)
	    (make-token 'NUMBER  41)
	    (make-token 'NUMBER  42)
	    (make-token 'NEWLINE #\newline))
    => '((line 1 #\newline)
	 (line 2 #\newline)
	 (line 3 #\newline)
	 (line 41 42 #\newline)))

;;; --------------------------------------------------------------------
;;; Successful error recovery.

  (check
      ;;The BAD  triggers an error, recovery happens,  the first NEWLINE
      ;;is  correctly  parsed as  recovery  token;  the  second line  is
      ;;correct.
      (doit (make-token 'NUMBER  1)
	    (make-token 'BAD      'alpha)
	    (make-token 'NEWLINE #\newline)
	    (make-token 'NUMBER  2)
	    (make-token 'NEWLINE #\newline))
    => '((recover error #\newline)
	 (line 2 #\newline)))

  (check
      ;;The first  BAD triggers an error, recovery  happens skipping the
      ;;second  and  third  BADs,  the  first  NEWLINE  is  detected  as
      ;;synchronisation token; the second line is correct.
      (doit (make-token 'NUMBER  1)
	    (make-token 'BAD     'alpha)
	    (make-token 'BAD     'beta)
	    (make-token 'BAD     'delta)
	    (make-token 'NEWLINE #\newline)
	    (make-token 'NUMBER  2)
	    (make-token 'NEWLINE #\newline))
    => '((recover error #\newline)
	 (line 2 #\newline)))

;;; --------------------------------------------------------------------
;;; Failed error recovery.

  (check
      ;;End-of-input is found after NUMBER.
      (doit (make-token 'NUMBER  1))
    => `((error-handler . ,(eof-object))))

  (check
      ;;The BAD triggers the error, the stack is rewind up to the start,
      ;;then end-of-input happens while  trying to skip tokens until the
      ;;synchronisation  one is  found.  End-of-input  is  an acceptable
      ;;token after the start.
      (parameterise ((debugging #f))
	(doit (make-token 'NUMBER  1)
	      (make-token 'BAD     'alpha)
	      (make-token 'BAD     'beta)
	      (make-token 'BAD     'delta)))
    => '())

  (check
      ;;The BAD triggers the error, the stack is rewind up to the start,
      ;;then end-of-input happens while  trying to skip tokens until the
      ;;synchronisation  one is  found.  End-of-input  is  an acceptable
      ;;token after the start.
      (parameterise ((debugging #f))
	(doit (make-token 'BAD 'alpha)))
    => '())

  #t)


(parameterise ((check-test-name 'error-recovery-2))

;;;Test error recovery policy  when the synchronisation terminal has the
;;;same category of the lookahead that raises the error.

  (define make-parser
    (lalr.lalr-parser (lalr.output-value: #t)
		      (lalr.expect: #f)
		      (lalr.terminals: '(A B C))
		      (lalr.rules: '((alphas (alpha)		: $1
					     (alphas alpha)	: $2)
				     (alpha  (A B)		: (list $1 $2)
					     (C)		: $1
					     (error C)		: (list $1 $2))))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
           (result		'())
	   (error-handler	(make-error-handler))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------
;;; No error, just grammar tests.

  (check
      (doit (make-token 'A 1)
	    (make-token 'B 2))
    => '(1 2))

  (check
      (doit (make-token 'C 3))
    => '3)

;;; --------------------------------------------------------------------
;;; Successful error recovery.

  (check
      ;;Error, recovery, eoi.
      (parameterise ((debugging #f))
	(doit (make-token 'A 1)
	      (make-token 'C 3)))
    => '(error 3))

  (check
    ;;Error, recovery, correct parse.
    (parameterise ((debugging #f))
      (doit (make-token 'A 1)
	    (make-token 'C 3)
	    (make-token 'A 1)
	    (make-token 'B 2)))
    => '(1 2))

  #t)


(parameterise ((check-test-name 'lexer-error))

;;;Test the lexer returning a non-token value.

  (define make-parser
    (lalr.lalr-parser (lalr.output-value: #t)
		      (lalr.expect: #f)
		      (lalr.terminals: '(A B C))
		      (lalr.rules: '((alpha (A B)	: (list $1 $2)
					    (C)		: $1
					    (error C)	: (list $1 $2))))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
           (result		'())
	   (yycustom		(lambda args args))
	   (error-handler	(lambda (message token)
				  (error #f message token)))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------
;;; No error, just grammar tests.

  (check
      (doit (make-token 'A 1)
	    (make-token 'B 2))
    => '(1 2))

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else (cons (condition-message exc)
			      (condition-irritants exc))))
	(doit (make-token 'A 1)
	      (make-token 'B 2)
	      123))
    => '("expected lexical token from lexer" 123))

  #t)


(parameterise ((check-test-name 'no-semantic-clause))

  (define terminals
    '(NUMBER COMMA NEWLINE))

  (define non-terminals
    '((lines (lines line)		: (yycustom $2)
	     (line)			: (yycustom $1))
      (line (NEWLINE)			: #\newline
            (NUMBER NEWLINE)		: $1
            (COMMA NUMBER NEWLINE))
                ;this is a rule with no semantic action
      ))

  (define make-parser
    (lalr.lalr-parser (lalr.output-value: #t)
		      (lalr.expect: #f)
		      (lalr.terminals: terminals)
		      (lalr.rules: non-terminals)))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
           (result		'())
           (yycustom		(lambda (value)
                                  (set! result (cons value result))))
	   (error-handler	(make-error-handler yycustom))
           (parser		(make-parser)))
  	(parser lexer error-handler yycustom)
        result))

  (check	;correct input
      (doit (make-token 'NUMBER  1)
	    (make-token 'NEWLINE #\newline))
    => '(1))

  (check  ;correct input with comma, which is a rule with no client form
      (doit (make-token 'COMMA   #\,)
	    (make-token 'NUMBER  1)
	    (make-token 'NEWLINE #\newline))
    => (list sentinel))

  #t)


(parameterise ((check-test-name 'single-expressions))

  ;;This is the grammar of the (lalr) documentation in Texinfo format.

  (define terminals
    '(N O C (left: A) (left: M) (nonassoc: U)))

  (define non-terminals
    '((E	(N)		: $1
		(E A E)		: ($2 $1 $3)
		(E M E)		: ($2 $1 $3)
		(A E (prec: U))	: ($1 $2)
		(O E C)		: $2)))

  (define make-parser
    (lalr.lalr-parser (lalr.output-value: #t)
		      (lalr.expect: #f)
		      (lalr.terminals: terminals)
		      (lalr.rules: non-terminals)))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (debug:print-tables #f terminals non-terminals)

  (check	;correct input
      (doit (make-token 'N 1))
    => 1)

  (check	;correct input
      (doit (make-token 'A -)
	    (make-token 'N 1))
    => -1)

  (check	;correct input
      (doit (make-token 'A +)
	    (make-token 'N 1))
    => 1)

  (check	;correct input
      (doit (make-token 'N 1)
	    (make-token 'A +)
	    (make-token 'N 2))
    => 3)

  (check	;correct input
      (doit (make-token 'N 1)
	    (make-token 'A +)
	    (make-token 'N 2)
	    (make-token 'M *)
	    (make-token 'N 3))
    => 7)

  (check	;correct input
      (doit (make-token 'O #\()
	    (make-token 'N 1)
	    (make-token 'A +)
	    (make-token 'N 2)
	    (make-token 'C #\))
	    (make-token 'M *)
	    (make-token 'N 3))
    => 9)

  #t)


(parameterise ((check-test-name 'associativity-1))

;;;Expression language with no associativity attributes.
;;;
;;;When no  associativity specification is  present for a symbol  in the
;;;"terminals:"  clause:  a  terminal  has precedence  index  zero  and,
;;;according to the conflict resolution rules, it is right-associative.
;;;
;;;The conflict resolution protocol rules:
;;;
;;; Shift/Reduce	-> Shift
;;; Reduce/Reduce	-> the Reduce that comes first in the grammar
;;;

  (define make-parser
    (lalr.lalr-parser
     (lalr.output-value: #t)
     (lalr.expect: 0)
     (lalr.terminals: '(NUM	;precedence = 0
			ADD	;precedence = 0
			SUB	;precedence = 0
			MUL	;precedence = 0
			DIV))	;precedence = 0

     (lalr.rules: '((EXPR (EXPR ADD EXPR)	: (list $2 $1 $3)
			  (EXPR SUB EXPR)	: (list $2 $1 $3)
			  (EXPR MUL EXPR)	: (list $2 $1 $3)
			  (EXPR DIV EXPR)	: (list $2 $1 $3)
			  (ADD NUM)		: (list $1 $2)
			  (SUB NUM)		: (list $1 $2)
			  (NUM)			: $1)))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define-constant ADD		(make-token 'ADD '+))
  (define-constant SUB		(make-token 'SUB '-))
  (define-constant MUL		(make-token 'MUL '*))
  (define-constant DIV		(make-token 'DIV '/))

  (define-constant ONE		(make-token 'NUM 1))
  (define-constant TWO		(make-token 'NUM 2))
  (define-constant THREE	(make-token 'NUM 3))
  (define-constant FOUR		(make-token 'NUM 4))
  (define-constant FIVE		(make-token 'NUM 5))
  (define-constant SIX		(make-token 'NUM 6))

;;; --------------------------------------------------------------------
;;; single operator

  (check (doit ONE)			=> 1)
  (check (doit ADD ONE)			=> '(+ 1))
  (check (doit SUB ONE)			=> '(- 1))
  (check (doit ONE ADD TWO)		=> '(+ 1 2))
  (check (doit ONE SUB TWO)		=> '(- 1 2))
  (check (doit ONE MUL TWO)		=> '(* 1 2))
  (check (doit ONE DIV TWO)		=> '(/ 1 2))

;;; --------------------------------------------------------------------
;;; different operators precedence, shift/reduce conflict: pick shift

  ;; add first
  (check (doit ONE ADD TWO SUB THREE)	=> '(+ 1 (- 2 3)))
  (check (doit ONE ADD TWO MUL THREE)	=> '(+ 1 (* 2 3)))
  (check (doit ONE ADD TWO DIV THREE)	=> '(+ 1 (/ 2 3)))

  ;;sub first
  (check (doit ONE SUB TWO ADD THREE)	=> '(- 1 (+ 2 3)))
  (check (doit ONE SUB TWO MUL THREE)	=> '(- 1 (* 2 3)))
  (check (doit ONE SUB TWO DIV THREE)	=> '(- 1 (/ 2 3)))

  ;;mul first
  (check (doit ONE MUL TWO ADD THREE)	=> '(* 1 (+ 2 3)))
  (check (doit ONE MUL TWO SUB THREE)	=> '(* 1 (- 2 3)))
  (check (doit ONE MUL TWO DIV THREE)	=> '(* 1 (/ 2 3)))

  ;;div first
  (check (doit ONE DIV TWO ADD THREE)	=> '(/ 1 (+ 2 3)))
  (check (doit ONE DIV TWO SUB THREE)	=> '(/ 1 (- 2 3)))
  (check (doit ONE DIV TWO MUL THREE)	=> '(/ 1 (* 2 3)))

;;; --------------------------------------------------------------------
;;; operator associativity, shift/reduce conflict: pick shift

  ;;add operators
  (check (doit ONE ADD TWO ADD THREE)	=> '(+ 1 (+ 2 3)))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR)		=> '(+ 1 (+ 2 (+ 3 4))))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR ADD FIVE ADD
	       SIX)			=> '(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6))))))

  ;;sub operators
  (check (doit ONE SUB TWO SUB THREE)	=> '(- 1 (- 2 3)))
  (check (doit ONE SUB TWO SUB THREE
	       SUB FOUR)		=> '(- 1 (- 2 (- 3 4))))
  (check (doit ONE SUB TWO SUB THREE
	       SUB FOUR SUB FIVE SUB
	       SIX)			=> '(- 1 (- 2 (- 3 (- 4 (- 5 6))))))

  ;;mul operators
  (check (doit ONE MUL TWO MUL THREE)	=> '(* 1 (* 2 3)))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR)		=> '(* 1 (* 2 (* 3 4))))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR MUL FIVE MUL
	       SIX)			=> '(* 1 (* 2 (* 3 (* 4 (* 5 6))))))

  ;;div operators
  (check (doit ONE DIV TWO DIV THREE)	=> '(/ 1 (/ 2 3)))
  (check (doit ONE DIV TWO DIV THREE
	       DIV FOUR)		=> '(/ 1 (/ 2 (/ 3 4))))
  (check (doit ONE DIV TWO DIV THREE
	       DIV FOUR DIV FIVE DIV
	       SIX)			=> '(/ 1 (/ 2 (/ 3 (/ 4 (/ 5 6))))))

;;; --------------------------------------------------------------------
;;; associativity of unary operators

  (check (doit ONE ADD TWO ADD ADD THREE)	=> '(+ 1 (+ 2 (+ 3))))
  (check (doit ONE ADD TWO ADD SUB THREE)	=> '(+ 1 (+ 2 (- 3))))
  (check (doit ONE ADD TWO SUB ADD THREE)	=> '(+ 1 (- 2 (+ 3))))
  (check (doit ONE ADD TWO SUB SUB THREE)	=> '(+ 1 (- 2 (- 3))))

  #t)


(parameterise ((check-test-name 'associativity-2))

;;;Expression language with all non-associative operators.
;;;
;;;When  a  symbol  in  the "terminals:"  clause  has  an  associativity
;;;specification:  its  precedence  index  is 1  higher  than  its  last
;;;predecessor in the list having  an associativity specification; if it
;;;is the first: its precedence index is 1.
;;;
;;;When the associativity specification  is "nonassoc:": the terminal is
;;;right associative.
;;;
;;;The conflict resolution protocol rules:
;;;
;;; Shift/Reduce	-> Shift
;;; Reduce/Reduce	-> the Reduce that comes first in the grammar
;;;

  (define make-parser
    (lalr.lalr-parser
     (lalr.output-value: #t)
     (lalr.expect: 0)
     (lalr.terminals: '(NUM		  ;precedence 0
			(nonassoc: ADD)	  ;precedence 1
			(nonassoc: SUB)	  ;precedence 2
			(nonassoc: MUL)	  ;precedence 3
			(nonassoc: DIV))) ;precedence 4

     (lalr.rules: '((EXPR (EXPR ADD EXPR)	: (list $2 $1 $3)
			  (EXPR SUB EXPR)	: (list $2 $1 $3)
			  (EXPR MUL EXPR)	: (list $2 $1 $3)
			  (EXPR DIV EXPR)	: (list $2 $1 $3)
			  (ADD NUM)		: (list $1 $2)
			  (SUB NUM)		: (list $1 $2)
			  (NUM)			: $1)))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define-constant ADD		(make-token 'ADD '+))
  (define-constant SUB		(make-token 'SUB '-))
  (define-constant MUL		(make-token 'MUL '*))
  (define-constant DIV		(make-token 'DIV '/))

  (define-constant ONE		(make-token 'NUM 1))
  (define-constant TWO		(make-token 'NUM 2))
  (define-constant THREE	(make-token 'NUM 3))
  (define-constant FOUR		(make-token 'NUM 4))
  (define-constant FIVE		(make-token 'NUM 5))
  (define-constant SIX		(make-token 'NUM 6))

;;; --------------------------------------------------------------------
;;; single operator

  (check (doit ONE)			=> 1)
  (check (doit ADD ONE)			=> '(+ 1))
  (check (doit SUB ONE)			=> '(- 1))
  (check (doit ONE ADD TWO)		=> '(+ 1 2))
  (check (doit ONE SUB TWO)		=> '(- 1 2))
  (check (doit ONE MUL TWO)		=> '(* 1 2))
  (check (doit ONE DIV TWO)		=> '(/ 1 2))

;;; --------------------------------------------------------------------
;;; operator precedence

  ;; add first
  (check (doit ONE ADD TWO SUB THREE)	=> '(+ 1 (- 2 3)))
  (check (doit ONE ADD TWO MUL THREE)	=> '(+ 1 (* 2 3)))
  (check (doit ONE ADD TWO DIV THREE)	=> '(+ 1 (/ 2 3)))

  ;;sub first
  (check (doit ONE SUB TWO ADD THREE)	=> '(+ (- 1 2) 3))
  (check (doit ONE SUB TWO MUL THREE)	=> '(- 1 (* 2 3)))
  (check (doit ONE SUB TWO DIV THREE)	=> '(- 1 (/ 2 3)))

  ;;mul first
  (check (doit ONE MUL TWO ADD THREE)	=> '(+ (* 1 2) 3))
  (check (doit ONE MUL TWO SUB THREE)	=> '(- (* 1 2) 3))
  (check (doit ONE MUL TWO DIV THREE)	=> '(* 1 (/ 2 3)))

  ;;div first
  (check (doit ONE DIV TWO ADD THREE)	=> '(+ (/ 1 2) 3))
  (check (doit ONE DIV TWO SUB THREE)	=> '(- (/ 1 2) 3))
  (check (doit ONE DIV TWO MUL THREE)	=> '(* (/ 1 2) 3))

;;; --------------------------------------------------------------------
;;; operator associativity

  ;;add operators
  (check (doit ONE ADD TWO ADD THREE)	=> '(+ 1 (+ 2 3)))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR)		=> '(+ 1 (+ 2 (+ 3 4))))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR ADD FIVE ADD
	       SIX)			=> '(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6))))))

  ;;sub operators
  (check (doit ONE SUB TWO SUB THREE)	=> '(- 1 (- 2 3)))
  (check (doit ONE SUB TWO SUB THREE
	       SUB FOUR)		=> '(- 1 (- 2 (- 3 4))))
  (check (doit ONE SUB TWO SUB THREE
	       SUB FOUR SUB FIVE SUB
	       SIX)			=> '(- 1 (- 2 (- 3 (- 4 (- 5 6))))))

  ;;mul operators
  (check (doit ONE MUL TWO MUL THREE)	=> '(* 1 (* 2 3)))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR)		=> '(* 1 (* 2 (* 3 4))))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR MUL FIVE MUL
	       SIX)			=> '(* 1 (* 2 (* 3 (* 4 (* 5 6))))))

  ;;div operators
  (check (doit ONE DIV TWO DIV THREE)	=> '(/ 1 (/ 2 3)))
  (check (doit ONE DIV TWO DIV THREE
	       DIV FOUR)		=> '(/ 1 (/ 2 (/ 3 4))))
  (check (doit ONE DIV TWO DIV THREE
	       DIV FOUR DIV FIVE DIV
	       SIX)			=> '(/ 1 (/ 2 (/ 3 (/ 4 (/ 5 6))))))

;;; --------------------------------------------------------------------
;;; associativity of unary operators

  (check (doit ONE ADD TWO ADD ADD THREE)	=> '(+ 1 (+ 2 (+ 3))))
  (check (doit ONE ADD TWO ADD SUB THREE)	=> '(+ 1 (+ 2 (- 3))))
  (check (doit ONE ADD TWO SUB ADD THREE)	=> '(+ 1 (- 2 (+ 3))))
  (check (doit ONE ADD TWO SUB SUB THREE)	=> '(+ 1 (- 2 (- 3))))

  #t)


(parameterise ((check-test-name 'associativity-3))

;;;Fully working  test of  precedence and  associativity.  Just  to show
;;;off:  addition and  subtraction are  left-associative, multiplication
;;;and division are right-associative; they  could have been all left or
;;;all right with no problems.
;;;
;;;When  a  symbol  in  the "terminals:"  clause  has  an  associativity
;;;specification:  its  precedence  index  is 1  higher  than  its  last
;;;predecessor in the list having  an associativity specification; if it
;;;is the first: its precedence index is 1.
;;;
;;;When multiple symbols are present  in the same associativity selector
;;;in the "terminals:" clause: they have the same precedence.
;;;
;;;When the associativity specification  is "nonassoc:": the terminal is
;;;right associative.
;;;

  (define make-parser
    (lalr.lalr-parser
     (lalr.output-value: #t)
     (lalr.expect: 0)
     (lalr.terminals: '(NUM		    ;precedence 0
			(left:     ADD SUB) ;precedence 1
			(right:    MUL DIV) ;precedence 2
			(nonassoc: UNARY))) ;precedence 3

     (lalr.rules: '((EXPR (EXPR ADD EXPR)		: (list $2 $1 $3)
			  (EXPR SUB EXPR)		: (list $2 $1 $3)
			  (EXPR MUL EXPR)		: (list $2 $1 $3)
			  (EXPR DIV EXPR)		: (list $2 $1 $3)
			  (NUM)				: $1
			  (ADD NUM (prec: UNARY))	: (list $1 $2)
			  (SUB NUM (prec: UNARY))	: (list $1 $2))))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define-constant ADD		(make-token 'ADD '+))
  (define-constant SUB		(make-token 'SUB '-))
  (define-constant MUL		(make-token 'MUL '*))
  (define-constant DIV		(make-token 'DIV '/))

  (define-constant ONE		(make-token 'NUM 1))
  (define-constant TWO		(make-token 'NUM 2))
  (define-constant THREE	(make-token 'NUM 3))
  (define-constant FOUR		(make-token 'NUM 4))
  (define-constant FIVE		(make-token 'NUM 5))
  (define-constant SIX		(make-token 'NUM 6))

;;; --------------------------------------------------------------------
;;; single operator

  (check (doit ONE)			=> 1)
  (check (doit ADD ONE)			=> '(+ 1))
  (check (doit SUB ONE)			=> '(- 1))
  (check (doit ONE ADD TWO)		=> '(+ 1 2))
  (check (doit ONE SUB TWO)		=> '(- 1 2))
  (check (doit ONE MUL TWO)		=> '(* 1 2))
  (check (doit ONE DIV TWO)		=> '(/ 1 2))

;;; --------------------------------------------------------------------
;;; operator precedence

  ;; add first
  (check (doit ONE ADD TWO SUB THREE)	=> '(- (+ 1 2) 3)) ;same precedence, left-associativity
  (check (doit ONE ADD TWO MUL THREE)	=> '(+ 1 (* 2 3)))
  (check (doit ONE ADD TWO DIV THREE)	=> '(+ 1 (/ 2 3)))

  ;;sub first
  (check (doit ONE SUB TWO ADD THREE)	=> '(+ (- 1 2) 3)) ;same precedence, left-associativity
  (check (doit ONE SUB TWO MUL THREE)	=> '(- 1 (* 2 3)))
  (check (doit ONE SUB TWO DIV THREE)	=> '(- 1 (/ 2 3)))

  ;;mul first
  (check (doit ONE MUL TWO ADD THREE)	=> '(+ (* 1 2) 3))
  (check (doit ONE MUL TWO SUB THREE)	=> '(- (* 1 2) 3))
  (check (doit ONE MUL TWO DIV THREE)	=> '(* 1 (/ 2 3))) ;same precedence, right-associativity

  ;;div first
  (check (doit ONE DIV TWO ADD THREE)	=> '(+ (/ 1 2) 3))
  (check (doit ONE DIV TWO SUB THREE)	=> '(- (/ 1 2) 3))
  (check (doit ONE DIV TWO MUL THREE)	=> '(/ 1 (* 2 3))) ;same precedence, right-associativity

;;; --------------------------------------------------------------------
;;; operator associativity

  ;;add operators, left associative
  (check (doit ONE ADD TWO ADD THREE)	=> '(+ (+ 1 2) 3))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR)		=> '(+ (+ (+ 1 2) 3) 4))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR ADD FIVE ADD
	       SIX)			=> '(+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6))

  ;;sub operators, left associative
  (check (doit ONE SUB TWO SUB THREE)	=> '(- (- 1 2) 3))
  (check (doit ONE SUB TWO SUB THREE
	       SUB FOUR)		=> '(- (- (- 1 2) 3) 4))
  (check (doit ONE SUB TWO SUB THREE
	       SUB FOUR SUB FIVE SUB
	       SIX)			=> '(- (- (- (- (- 1 2) 3) 4) 5) 6))

  ;;mul operators, right associative
  (check (doit ONE MUL TWO MUL THREE)	=> '(* 1 (* 2 3)))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR)		=> '(* 1 (* 2 (* 3 4))))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR MUL FIVE MUL
	       SIX)			=> '(* 1 (* 2 (* 3 (* 4 (* 5 6))))))

  ;;div operators, right associative
  (check (doit ONE DIV TWO DIV THREE)	=> '(/ 1 (/ 2 3)))
  (check (doit ONE DIV TWO DIV THREE
	       DIV FOUR)		=> '(/ 1 (/ 2 (/ 3 4))))
  (check (doit ONE DIV TWO DIV THREE
	       DIV FOUR DIV FIVE DIV
	       SIX)			=> '(/ 1 (/ 2 (/ 3 (/ 4 (/ 5 6))))))

;;; --------------------------------------------------------------------
;;; associativity of unary operators

  (check (doit ONE ADD TWO ADD ADD THREE)	=> '(+ (+ 1 2) (+ 3))) ;same precedence, left-associative
  (check (doit ONE ADD TWO ADD SUB THREE)	=> '(+ (+ 1 2) (- 3))) ;same precedence, left-associative
  (check (doit ONE ADD TWO SUB ADD THREE)	=> '(- (+ 1 2) (+ 3))) ;same precedence, left-associative
  (check (doit ONE ADD TWO SUB SUB THREE)	=> '(- (+ 1 2) (- 3))) ;same precedence, left-associative

  #t)


(parameterise ((check-test-name 'associativity-4))

;;;Tests   for   precedence   and  associativity   between   conflicting
;;;non-terminals.
;;;
;;;When no  associativity specification is  present for a symbol  in the
;;;"terminals:"  clause:  a  terminal  has precedence  index  zero  and,
;;;according to the conflict resolution rules, it is right-associative.
;;;
;;;The conflict resolution protocol rules:
;;;
;;; Shift/Reduce	-> choose Shift
;;; Reduce/Reduce	-> choose the rule listed first in the grammar
;;;

  (define make-parser
    (lalr.lalr-parser
     (lalr.output-value: #t)
     (lalr.expect: 0)
     (lalr.terminals: '(NUM		    ;precedence 0
			ADD		    ;precedence 0
			SUB		    ;precedence 0
			MUL		    ;precedence 0
			DIV		    ;precedence 0
			(right: POW)	    ;precedence 1
			(nonassoc: UNARY))) ;precedence 2

     (lalr.rules:
      '((EXPR

	 ;; conflicting non-terminals
	 (EXPR ADD EXPR)          : (list $2 $1 $3) ;in coflict: choose this
	 (EXPR ADD EXPR ADD EXPR) : (list $2 $1 $3 $5)

	 ;; conflicting non-terminals
	 (EXPR SUB EXPR)          : (list $2 $1 $3) ;in coflict: choose this
	 (EXPR SUB EXPR SUB EXPR) : (list $2 $1 $3 $5)

	 ;; conflicting non-terminals
	 (EXPR MUL EXPR)          : (list $2 $1 $3) ;in coflict: choose this
	 (EXPR MUL EXPR MUL EXPR) : (list $2 $1 $3 $5)

	 ;; conflicting non-terminals
	 (EXPR DIV EXPR)          : (list $2 $1 $3) ;in coflict: choose this
	 (EXPR DIV EXPR DIV EXPR) : (list $2 $1 $3 $5)

	 (EXPR POW EXPR)          : (list $2 $1 $3)

	 (NUM)                    : $1
	 (ADD NUM (prec: UNARY))  : (list $1 $2)
	 (SUB NUM (prec: UNARY))  : (list $1 $2))))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define-constant ADD		(make-token 'ADD '+))
  (define-constant SUB		(make-token 'SUB '-))
  (define-constant MUL		(make-token 'MUL '*))
  (define-constant DIV		(make-token 'DIV '/))
  (define-constant POW		(make-token 'POW '^))

  (define-constant ONE		(make-token 'NUM 1))
  (define-constant TWO		(make-token 'NUM 2))
  (define-constant THREE	(make-token 'NUM 3))
  (define-constant FOUR		(make-token 'NUM 4))
  (define-constant FIVE		(make-token 'NUM 5))
  (define-constant SIX		(make-token 'NUM 6))

;;; --------------------------------------------------------------------
;;; single operator

  (check (doit ONE)			=> 1)
  (check (doit ADD ONE)			=> '(+ 1))
  (check (doit SUB ONE)			=> '(- 1))
  (check (doit ONE ADD TWO)		=> '(+ 1 2))
  (check (doit ONE SUB TWO)		=> '(- 1 2))
  (check (doit ONE MUL TWO)		=> '(* 1 2))
  (check (doit ONE DIV TWO)		=> '(/ 1 2))

;;; --------------------------------------------------------------------
;;; precedence between different operators, shift/reduce conflict: pick shift

  ;;add first
  (check (doit ONE ADD TWO SUB THREE)		=> '(+ 1 (- 2 3)))
  (check (doit ONE ADD TWO MUL THREE)		=> '(+ 1 (* 2 3)))
  (check (doit ONE ADD TWO DIV THREE)		=> '(+ 1 (/ 2 3)))

  ;;sub first
  (check (doit ONE SUB TWO ADD THREE)		=> '(- 1 (+ 2 3)))
  (check (doit ONE SUB TWO MUL THREE)		=> '(- 1 (* 2 3)))
  (check (doit ONE SUB TWO DIV THREE)		=> '(- 1 (/ 2 3)))

  ;;mul first
  (check (doit ONE MUL TWO ADD THREE)		=> '(* 1 (+ 2 3)))
  (check (doit ONE MUL TWO SUB THREE)		=> '(* 1 (- 2 3)))
  (check (doit ONE MUL TWO DIV THREE)		=> '(* 1 (/ 2 3)))

  ;;div first
  (check (doit ONE DIV TWO ADD THREE)		=> '(/ 1 (+ 2 3)))
  (check (doit ONE DIV TWO SUB THREE)		=> '(/ 1 (- 2 3)))
  (check (doit ONE DIV TWO MUL THREE)		=> '(/ 1 (* 2 3)))

;;; --------------------------------------------------------------------
;;; precedence between same operators, shift/reduce conflict: pick shift

  ;;add operators
  (check (doit ONE ADD TWO ADD THREE)		=> '(+ 1 (+ 2 3)))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR)			=> '(+ 1 (+ 2 (+ 3 4))))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR ADD FIVE ADD SIX)	=> '(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6))))))

  ;;sub operators
  (check (doit ONE SUB TWO SUB THREE)		=> '(- 1 (- 2 3)))

  ;;mul operators
  (check (doit ONE MUL TWO MUL THREE)		=> '(* 1 (* 2 3)))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR)			=> '(* 1 (* 2 (* 3 4))))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR MUL FIVE MUL SIX)	=> '(* 1 (* 2 (* 3 (* 4 (* 5 6))))))

  ;;div operators
  (check (doit ONE DIV TWO DIV THREE)		=> '(/ 1 (/ 2 3)))

;;; --------------------------------------------------------------------
;;; associativity of unary operators

  (check (doit ONE ADD TWO ADD ADD THREE)	=> '(+ 1 (+ 2 (+ 3))))
  (check (doit ONE ADD TWO ADD SUB THREE)	=> '(+ 1 (+ 2 (- 3))))
  (check (doit ONE ADD TWO SUB ADD THREE)	=> '(+ 1 (- 2 (+ 3))))
  (check (doit ONE ADD TWO SUB SUB THREE)	=> '(+ 1 (- 2 (- 3))))

;;; --------------------------------------------------------------------

  ;;Start:		ONE ADD TWO POW THREE ADD FOUR
  ;;Reduce POW:		ONE ADD (^ 2 3) ADD FOUR
  ;;Shift:		ONE ADD (+ (^ 2 3) 4)
  ;;Reduce:		(+ 1 (+ (^ 2 3) 4))
  ;;
  (check (doit ONE ADD TWO POW THREE ADD FOUR)	=> '(+ 1 (+ (^ 2 3) 4)))

  #t)


(parameterise ((check-test-name 'associativity-5))

;;;Tests   for   precedence   and  associativity   between   conflicting
;;;non-terminals.
;;;
;;;When no  associativity specification is  present for a symbol  in the
;;;"terminals:"  clause:  a  terminal  has precedence  index  zero  and,
;;;according to the conflict resolution rules, it is right-associative.
;;;
;;;The conflict resolution protocol rules:
;;;
;;; Shift/Reduce	-> choose Shift
;;; Reduce/Reduce	-> choose the rule listed first in the grammar
;;;

  (define make-parser
    (lalr.lalr-parser
     (lalr.output-value: #t)
     (lalr.expect: 0)
     (lalr.terminals: '(NUM		    ;precedence 0
			ADD		    ;precedence 0
			SUB		    ;precedence 0
			MUL		    ;precedence 0
			DIV		    ;precedence 0
			(right: POW)	    ;precedence 1
			(nonassoc: UNARY))) ;precedence 2

     (lalr.rules:
      '((EXPR

	 ;; conflicting non-terminals
	 (EXPR ADD EXPR ADD EXPR) : (list $2 $1 $3 $5) ;in coflict: choose this
	 (EXPR ADD EXPR)          : (list $2 $1 $3)

	 ;; conflicting non-terminals
	 (EXPR SUB EXPR SUB EXPR) : (list $2 $1 $3 $5) ;in coflict: choose this
	 (EXPR SUB EXPR)          : (list $2 $1 $3)

	 ;; conflicting non-terminals
	 (EXPR MUL EXPR MUL EXPR) : (list $2 $1 $3 $5) ;in coflict: choose this
	 (EXPR MUL EXPR)          : (list $2 $1 $3)

	 ;; conflicting non-terminals
	 (EXPR DIV EXPR DIV EXPR) : (list $2 $1 $3 $5) ;in coflict: choose this
	 (EXPR DIV EXPR)          : (list $2 $1 $3)

	 (EXPR POW EXPR)          : (list $2 $1 $3)

	 (NUM)                    : $1
	 (ADD NUM (prec: UNARY))  : (list $1 $2)
	 (SUB NUM (prec: UNARY))  : (list $1 $2))))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define-constant ADD		(make-token 'ADD '+))
  (define-constant SUB		(make-token 'SUB '-))
  (define-constant MUL		(make-token 'MUL '*))
  (define-constant DIV		(make-token 'DIV '/))
  (define-constant POW		(make-token 'POW '^))

  (define-constant ONE		(make-token 'NUM 1))
  (define-constant TWO		(make-token 'NUM 2))
  (define-constant THREE	(make-token 'NUM 3))
  (define-constant FOUR		(make-token 'NUM 4))
  (define-constant FIVE		(make-token 'NUM 5))
  (define-constant SIX		(make-token 'NUM 6))

;;; --------------------------------------------------------------------
;;; single operator

  (check (doit ONE)			=> 1)
  (check (doit ADD ONE)			=> '(+ 1))
  (check (doit SUB ONE)			=> '(- 1))
  (check (doit ONE ADD TWO)		=> '(+ 1 2))
  (check (doit ONE SUB TWO)		=> '(- 1 2))
  (check (doit ONE MUL TWO)		=> '(* 1 2))
  (check (doit ONE DIV TWO)		=> '(/ 1 2))

;;; --------------------------------------------------------------------
;;; precedence between different operators, shift/reduce conflict: pick shift

  ;;add first
  (check (doit ONE ADD TWO SUB THREE)		=> '(+ 1 (- 2 3)))
  (check (doit ONE ADD TWO MUL THREE)		=> '(+ 1 (* 2 3)))
  (check (doit ONE ADD TWO DIV THREE)		=> '(+ 1 (/ 2 3)))

  ;;sub first
  (check (doit ONE SUB TWO ADD THREE)		=> '(- 1 (+ 2 3)))
  (check (doit ONE SUB TWO MUL THREE)		=> '(- 1 (* 2 3)))
  (check (doit ONE SUB TWO DIV THREE)		=> '(- 1 (/ 2 3)))

  ;;mul first
  (check (doit ONE MUL TWO ADD THREE)		=> '(* 1 (+ 2 3)))
  (check (doit ONE MUL TWO SUB THREE)		=> '(* 1 (- 2 3)))
  (check (doit ONE MUL TWO DIV THREE)		=> '(* 1 (/ 2 3)))

  ;;div first
  (check (doit ONE DIV TWO ADD THREE)		=> '(/ 1 (+ 2 3)))
  (check (doit ONE DIV TWO SUB THREE)		=> '(/ 1 (- 2 3)))
  (check (doit ONE DIV TWO MUL THREE)		=> '(/ 1 (* 2 3)))

;;; --------------------------------------------------------------------
;;; precedence between same operators, shift/reduce conflict: pick shift

  ;;add operators
  (check (doit ONE ADD TWO ADD THREE)		=> '(+ 1 2 3))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR)			=> '(+ 1 (+ 2 3 4)))
  (check (doit ONE ADD TWO ADD THREE
	       ADD FOUR ADD FIVE ADD SIX)	=> '(+ 1 (+ 2 3 (+ 4 5 6))))

  ;;sub operators
  (check (doit ONE SUB TWO SUB THREE)		=> '(- 1 2 3))

  ;;mul operators
  (check (doit ONE MUL TWO MUL THREE)		=> '(* 1 2 3))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR)			=> '(* 1 (* 2 3 4)))
  (check (doit ONE MUL TWO MUL THREE
	       MUL FOUR MUL FIVE MUL SIX)	=> '(* 1 (* 2 3 (* 4 5 6))))

  ;;div operators
  (check (doit ONE DIV TWO DIV THREE)		=> '(/ 1 2 3))

;;; --------------------------------------------------------------------
;;; associativity of unary operators

  (check (doit ONE ADD TWO ADD ADD THREE)	=> '(+ 1 2 (+ 3)))
  (check (doit ONE ADD TWO ADD SUB THREE)	=> '(+ 1 2 (- 3)))
  (check (doit ONE ADD TWO SUB ADD THREE)	=> '(+ 1 (- 2 (+ 3))))
  (check (doit ONE ADD TWO SUB SUB THREE)	=> '(+ 1 (- 2 (- 3))))

;;; --------------------------------------------------------------------

  ;;Start:		ONE ADD TWO POW THREE ADD FOUR
  ;;Reduce POW:		ONE ADD (^ 2 3) ADD FOUR
  ;;Reduce:		(+ 1 (^ 2 3) 4)
  ;;
  (check (doit ONE ADD TWO POW THREE ADD FOUR)	=> '(+ 1 (^ 2 3) 4))

  #t)


(parameterise ((check-test-name 'doc-examples-1))

  ;;Let's define two operators ADD and MUL and the grammar:
  ;;
  ;;   ((EXPR (EXPR ADD EXPR)
  ;;          (EXPR MUL EXPR)
  ;;          (NUM)))
  ;;

  ;;Operator precedence  matters when  parsing input sequences  in which
  ;;both appear:
  ;;
  ;;   NUM ADD NUM MUL NUM
  ;;
  ;;If ADD  and MUL have  the same  precedence, there is  a Shift/Reduce
  ;;conflict; the sequence can be parsed by shifting first as in:
  ;;
  ;;   NUM ADD NUM MUL NUM => NUM ADD EXPR => EXPR
  ;;
  ;;or by reducing first as in:
  ;;
  ;;   NUM ADD NUM MUL NUM => EXPR MUL NUM => EXPR
  ;;
  ;;If MUL has higher precedence than ADD, subexpressions containing MUL
  ;;are reduced first no matter if they come before or after:
  ;;
  ;;   NUM ADD NUM MUL NUM => NUM ADD EXPR => EXPR
  ;;   NUM MUL NUM ADD NUM => EXPR ADD NUM => EXPR
  ;;

  ;;Operator associativity matters when parsing input sequences in which
  ;;only one operator appears:
  ;;
  ;;   NUM ADD NUM ADD NUM
  ;;
  ;;If  ADD  is left-associative,  the  first  subexpression is  reduced
  ;;first:
  ;;
  ;;   NUM ADD NUM ADD NUM => EXPR ADD NUM => EXPR
  ;;
  ;;If  ADD is  right-associative, the  second subexpression  is reduced
  ;;first:
  ;;
  ;;   NUM ADD NUM ADD NUM => NUM ADD EXPR => EXPR
  ;;
  ;;If ADD  is non-associative  or has  no specified  associativity: the
  ;;expression is parsed as if ADD is right-associative.
  ;;

  ;;If the grammar  contains two rules, one of which  is a subexpression
  ;;of the other:
  ;;
  ;;   ((EXPR (EXPR ADD EXPR)
  ;;          (EXPR ADD EXPR ADD EXPR)
  ;;          (NUM)))
  ;;
  ;;we have a Reduce/Reduce conflict.  A sequence like:
  ;;
  ;;   NUM1 ADD NUM2 ADD NUM2
  ;;
  ;;can be parsed by reducing the first subexpression:
  ;;
  ;;   NUM1 ADD NUM2 ADD NUM2 => EXPR ADD NUM2 => EXPR
  ;;
  ;;or by reducing the whole expression:
  ;;
  ;;   NUM1 ADD NUM2 ADD NUM2 => EXPR
  ;;
  ;;LALR selects the rule that comes first in the grammar definition.
  ;;

;;; --------------------------------------------------------------------
;;; precedence

  ;;On the meaning  of terminal precedence: two operators  with the same
  ;;precedence.   Show  that  Shift/Reduce  conflicts  are  resolved  by
  ;;choosing Shift.
  (let ()

    (define make-parser
      (lalr.lalr-parser
       (lalr.output-value: #t)
       (lalr.expect: 0)
       (lalr.terminals: '(NUM	;precedence 0
			  ADD	;precedence 0
			  MUL))	;precedence 0
       (lalr.rules:
	'((EXPR (EXPR ADD EXPR)	: (list $2 $1 $3)
		(EXPR MUL EXPR)	: (list $2 $1 $3)
		(NUM)		: $1)))))

    (define (doit . tokens)
      (let* ((lexer		(make-lexer tokens))
	     (error-handler	(make-error-handler))
	     (parser		(make-parser)))
	(parser lexer error-handler)))

    (define-constant ADD	(make-token 'ADD '+))
    (define-constant MUL	(make-token 'MUL '*))
    (define-constant ONE	(make-token 'NUM 1))
    (define-constant TWO	(make-token 'NUM 2))
    (define-constant THREE	(make-token 'NUM 3))

    (check (doit ONE ADD TWO)	=> '(+ 1 2))
    (check (doit ONE MUL TWO)	=> '(* 1 2))

    ;;Selecting shift: the second subexpression is reduced first and the
    ;;result is: (+ 1 (* 2 3)).
    ;;
    ;;Selecting reduce: the first subexpression is reduced first and the
    ;;result is: (* (+ 1 2) 3).
    ;;
    (check (doit ONE ADD TWO MUL THREE)		=> '(+ 1 (* 2 3)))
    (check (doit ONE MUL TWO ADD THREE)		=> '(* 1 (+ 2 3)))

    #f)

  ;;On the meaning of terminal precedence: two non-associative operators
  ;;with different precedence.  Show that precedence always wins.
  (let ()

    (define make-parser
      (lalr.lalr-parser
       (lalr.output-value: #t)
       (lalr.expect: 0)
       (lalr.terminals: '(NUM		    ;precedence 0
			  (nonassoc: ADD)   ;precedence 1
			  (nonassoc: MUL))) ;precedence 2
       (lalr.rules:
	'((EXPR (EXPR ADD EXPR)	: (list $2 $1 $3)
		(EXPR MUL EXPR)	: (list $2 $1 $3)
		(NUM)		: $1)))))

    (define (doit . tokens)
      (let* ((lexer		(make-lexer tokens))
	     (error-handler	(make-error-handler))
	     (parser		(make-parser)))
	(parser lexer error-handler)))

    (define-constant ADD	(make-token 'ADD '+))
    (define-constant MUL	(make-token 'MUL '*))
    (define-constant ONE	(make-token 'NUM 1))
    (define-constant TWO	(make-token 'NUM 2))
    (define-constant THREE	(make-token 'NUM 3))

    (check (doit ONE ADD TWO)	=> '(+ 1 2))
    (check (doit ONE MUL TWO)	=> '(* 1 2))

    ;;Precedence wins.
    (check (doit ONE ADD TWO MUL THREE)		=> '(+ 1 (* 2 3)))
    (check (doit ONE MUL TWO ADD THREE)		=> '(+ (* 1 2) 3))

    #f)

;;; --------------------------------------------------------------------
;;; associativity

  ;;On  the  meaning  of   terminal  associativity:  a  left-associative
  ;;operator and a right-associative operator.
  (let ()

    (define make-parser
      (lalr.lalr-parser
       (lalr.output-value: #t)
       (lalr.expect: 0)
       (lalr.terminals: '(NUM		 ;precedence 0
			  (left:  ADD)   ;precedence 1
			  (right: MUL))) ;precedence 2
       (lalr.rules:
	'((EXPR (EXPR ADD EXPR)	: (list $2 $1 $3)
		(EXPR MUL EXPR)	: (list $2 $1 $3)
		(NUM)		: $1)))))

    (define (doit . tokens)
      (let* ((lexer		(make-lexer tokens))
	     (error-handler	(make-error-handler))
	     (parser		(make-parser)))
	(parser lexer error-handler)))

    (define-constant ADD	(make-token 'ADD '+))
    (define-constant MUL	(make-token 'MUL '*))
    (define-constant ONE	(make-token 'NUM 1))
    (define-constant TWO	(make-token 'NUM 2))
    (define-constant THREE	(make-token 'NUM 3))

    (check (doit ONE ADD TWO)	=> '(+ 1 2))
    (check (doit ONE MUL TWO)	=> '(* 1 2))

    ;;left-associative
    (check (doit ONE ADD TWO ADD THREE)		=> '(+ (+ 1 2) 3))
    ;;right-associative
    (check (doit ONE MUL TWO MUL THREE)		=> '(* 1 (* 2 3)))

    #f)

  ;;On the meaning of  terminal associativity: non-associative operators
  ;;and operators for which the associativity is unspecified are handled
  ;;as right-associative.
  (let ()

    (define make-parser
      (lalr.lalr-parser
       (lalr.output-value: #t)
       (lalr.expect: 0)
       (lalr.terminals: '(NUM		  ;precedence 0
			  (nonassoc: ADD) ;precedence 1
			  MUL))		  ;precedence 0
       (lalr.rules:
	'((EXPR (EXPR ADD EXPR)	: (list $2 $1 $3)
		(EXPR MUL EXPR)	: (list $2 $1 $3)
		(NUM)		: $1)))))

    (define (doit . tokens)
      (let* ((lexer		(make-lexer tokens))
	     (error-handler	(make-error-handler))
	     (parser		(make-parser)))
	(parser lexer error-handler)))

    (define-constant ADD	(make-token 'ADD '+))
    (define-constant MUL	(make-token 'MUL '*))
    (define-constant ONE	(make-token 'NUM 1))
    (define-constant TWO	(make-token 'NUM 2))
    (define-constant THREE	(make-token 'NUM 3))

    (check (doit ONE ADD TWO)	=> '(+ 1 2))
    (check (doit ONE MUL TWO)	=> '(* 1 2))

    (check (doit ONE ADD TWO ADD THREE)		=> '(+ 1 (+ 2 3)))
    (check (doit ONE MUL TWO MUL THREE)		=> '(* 1 (* 2 3)))

    #f)

;;; --------------------------------------------------------------------
;;; Reduce/Reduce conflicts

  ;;On the meaning of Reduce/Reduce conflicts: the longest rule is given
  ;;first.  Operator precedence does matter.
  (let ()

    (define make-parser
      (lalr.lalr-parser
       (lalr.output-value: #t)
       (lalr.expect: 0)
       (lalr.terminals: '(NUM		    ;precedence 0
			  ADD		    ;precedence 0
			  (nonassoc: SUB)   ;precedence 1
			  (left:     MUL)   ;precedence 2
			  (right:    DIV))) ;precedence 3
       (lalr.rules:
	'((EXPR
	   (EXPR ADD EXPR ADD EXPR)	: (list $2 $1 $3 $5)
	   (EXPR SUB EXPR SUB EXPR)	: (list $2 $1 $3 $5)
	   (EXPR MUL EXPR MUL EXPR)	: (list $2 $1 $3 $5)
	   (EXPR DIV EXPR DIV EXPR)	: (list $2 $1 $3 $5)
	   (EXPR ADD EXPR)		: (list $2 $1 $3)
	   (EXPR SUB EXPR)		: (list $2 $1 $3)
	   (EXPR MUL EXPR)		: (list $2 $1 $3)
	   (EXPR DIV EXPR)		: (list $2 $1 $3)
	   (NUM)			: $1)))))

    (define (doit . tokens)
      (let* ((lexer		(make-lexer tokens))
	     (error-handler	(make-error-handler))
	     (parser		(make-parser)))
	(parser lexer error-handler)))

    (define-constant ADD	(make-token 'ADD '+))
    (define-constant SUB	(make-token 'SUB '-))
    (define-constant MUL	(make-token 'MUL '*))
    (define-constant DIV	(make-token 'DIV '/))
    (define-constant ONE	(make-token 'NUM 1))
    (define-constant TWO	(make-token 'NUM 2))
    (define-constant THREE	(make-token 'NUM 3))
    (define-constant FOUR	(make-token 'NUM 4))

    (check (doit ONE ADD TWO)	=> '(+ 1 2))
    (check (doit ONE SUB TWO)	=> '(- 1 2))
    (check (doit ONE MUL TWO)	=> '(* 1 2))
    (check (doit ONE DIV TWO)	=> '(/ 1 2))

    ;;No associativity specified.
    (check (doit ONE ADD TWO
		 ADD THREE)	=> '(+ 1 2 3))
    (check (doit ONE ADD TWO
		 ADD THREE
		 ADD FOUR)	=> '(+ 1 (+ 2 3 4)))

    ;;Non-associative operator.
    (check (doit ONE SUB TWO
		 SUB THREE)	=> '(- 1 2 3))
    (check (doit ONE SUB TWO
		 SUB THREE
		 SUB FOUR)	=> '(- 1 (- 2 3 4)))

    ;;Left-associative operator.
    (check (doit ONE MUL TWO
		 MUL THREE)	=> '(* (* 1 2) 3))
    (check (doit ONE MUL TWO
		 MUL THREE
		 MUL FOUR)	=> '(* (* (* 1 2) 3) 4))

    ;;Right-associative operator.
    (check (doit ONE DIV TWO
		 DIV THREE)	=> '(/ 1 2 3))
    (check (doit ONE DIV TWO
		 DIV THREE
		 DIV FOUR)	=> '(/ 1 (/ 2 3 4)))

    #f)

  ;;On the meaning of Reduce/Reduce conflicts: the longest rule is given
  ;;last.  Operator precedence does matter.
  (let ()

    (define make-parser
      (lalr.lalr-parser
       (lalr.output-value: #t)
       (lalr.expect: 0)
       (lalr.terminals: '(NUM		    ;precedence 0
			  ADD		    ;precedence 0
			  (nonassoc: SUB)   ;precedence 1
			  (left:     MUL)   ;precedence 2
			  (right:    DIV))) ;precedence 3
       (lalr.rules:
	'((EXPR
	   (EXPR ADD EXPR)		: (list $2 $1 $3)
	   (EXPR SUB EXPR)		: (list $2 $1 $3)
	   (EXPR MUL EXPR)		: (list $2 $1 $3)
	   (EXPR DIV EXPR)		: (list $2 $1 $3)
	   (EXPR ADD EXPR ADD EXPR)	: (list $2 $1 $3 $5)
	   (EXPR SUB EXPR SUB EXPR)	: (list $2 $1 $3 $5)
	   (EXPR MUL EXPR MUL EXPR)	: (list $2 $1 $3 $5)
	   (EXPR DIV EXPR DIV EXPR)	: (list $2 $1 $3 $5)
	   (NUM)			: $1)))))

    (define (doit . tokens)
      (let* ((lexer		(make-lexer tokens))
	     (error-handler	(make-error-handler))
	     (parser		(make-parser)))
	(parser lexer error-handler)))

    (define-constant ADD	(make-token 'ADD '+))
    (define-constant SUB	(make-token 'SUB '-))
    (define-constant MUL	(make-token 'MUL '*))
    (define-constant DIV	(make-token 'DIV '/))
    (define-constant ONE	(make-token 'NUM 1))
    (define-constant TWO	(make-token 'NUM 2))
    (define-constant THREE	(make-token 'NUM 3))
    (define-constant FOUR	(make-token 'NUM 4))

    (check (doit ONE ADD TWO)	=> '(+ 1 2))
    (check (doit ONE SUB TWO)	=> '(- 1 2))
    (check (doit ONE MUL TWO)	=> '(* 1 2))
    (check (doit ONE DIV TWO)	=> '(/ 1 2))

    ;;No   associativity   specified.   Handled   as   right-associative
    ;;operator.
    (check (doit ONE ADD TWO
		 ADD THREE)	=> '(+ 1 (+ 2 3)))
    (check (doit ONE ADD TWO
		 ADD THREE
		 ADD FOUR)	=> '(+ 1 (+ 2 (+ 3 4))))

    ;;Non-associative operator.  Handled as right-associative operator.
    (check (doit ONE SUB TWO
		 SUB THREE)	=> '(- 1 (- 2 3)))
    (check (doit ONE SUB TWO
		 SUB THREE
		 SUB FOUR)	=> '(- 1 (- 2 (- 3 4))))

    ;;Left-associative operator.
    (check (doit ONE MUL TWO
		 MUL THREE)	=> '(* (* 1 2) 3))
    (check (doit ONE MUL TWO
		 MUL THREE
		 MUL FOUR)	=> '(* (* (* 1 2) 3) 4))

    ;;Right-associative operator.
    (check (doit ONE DIV TWO
		 DIV THREE)	=> '(/ 1 (/ 2 3)))
    (check (doit ONE DIV TWO
		 DIV THREE
		 DIV FOUR)	=> '(/ 1 (/ 2 (/ 3 4))))

    #f)

;;; --------------------------------------------------------------------
;;; Reduce/Reduce conflicts with unary operators

  ;;On   the  meaning   of  Reduce/Reduce   conflicts:  left-associative
  ;;operators with unary rule without precedence.
  (let ()

    (define make-parser
      (lalr.lalr-parser
       (lalr.output-value: #t)
       (lalr.expect: 0)
       (lalr.terminals: '(NUM		    ;precedence 0
			  (left: ADD SUB)   ;precedence 1
			  (left: MUL DIV))) ;precedence 2
       (lalr.rules:
	'((EXPR (ADD EXPR)		: (list $1 $2)
		(SUB EXPR)		: (list $1 $2)
		(EXPR ADD EXPR)		: (list $2 $1 $3)
		(EXPR SUB EXPR)		: (list $2 $1 $3)
		(EXPR MUL EXPR)		: (list $2 $1 $3)
		(EXPR DIV EXPR)		: (list $2 $1 $3)
		(NUM)			: $1)))))

    (define (doit . tokens)
      (let* ((lexer		(make-lexer tokens))
	     (error-handler	(make-error-handler))
	     (parser		(make-parser)))
	(parser lexer error-handler)))

    (define-constant ADD	(make-token 'ADD '+))
    (define-constant SUB	(make-token 'SUB '-))
    (define-constant MUL	(make-token 'MUL '*))
    (define-constant DIV	(make-token 'DIV '/))
    (define-constant ONE	(make-token 'NUM 1))
    (define-constant TWO	(make-token 'NUM 2))

    (check (doit ADD ONE)		=> '(+ 1))
    (check (doit SUB ONE)		=> '(- 1))

    (check (doit ADD ADD ONE)		=> '(+ (+ 1)))
    (check (doit SUB SUB ONE)		=> '(- (- 1)))
    (check (doit ADD SUB ONE)		=> '(+ (- 1)))
    (check (doit SUB ADD ONE)		=> '(- (+ 1)))

    ;;ADD is left-associative.
    (check (doit ADD ONE ADD TWO)	=> '(+ (+ 1) 2))
    ;;ADD and SUB have equal preceence, but ADD is left-associative.
    (check (doit ADD ONE SUB TWO)	=> '(- (+ 1) 2))
    ;;precedence(MUL) > precedence(ADD).
    (check (doit ADD ONE MUL TWO)	=> '(+ (* 1 2)))
    ;;precedence(DIV) > precedence(ADD).
    (check (doit ADD ONE DIV TWO)	=> '(+ (/ 1 2)))

    ;;ADD and SUB have equal preceence, but SUB is left-associative.
    (check (doit SUB ONE ADD TWO)	=> '(+ (- 1) 2))
    ;;SUB is left-associative.
    (check (doit SUB ONE SUB TWO)	=> '(- (- 1) 2))
    ;;precedence(MUL) > precedence(SUB).
    (check (doit SUB ONE MUL TWO)	=> '(- (* 1 2)))
    ;;precedence(DIV) > precedence(SUB).
    (check (doit SUB ONE DIV TWO)	=> '(- (/ 1 2)))

    #f)

  ;;On   the  meaning   of  Reduce/Reduce   conflicts:  left-associative
  ;;operators with unary rule having precedence.
  (let ()

    (define make-parser
      (lalr.lalr-parser
       (lalr.output-value: #t)
       (lalr.expect: 0)
       (lalr.terminals: '(NUM		     ;precedence 0
			  (left: ADD SUB)    ;precedence 1
			  (left: MUL DIV)    ;precedence 2
			  (nonassoc: UADD)   ;precedence 3
			  (nonassoc: USUB))) ;precedence 4
       (lalr.rules:
	'((EXPR (ADD EXPR (prec: UADD)) : (list $1 $2)
		(SUB EXPR (prec: USUB)) : (list $1 $2)
		(EXPR ADD EXPR)		: (list $2 $1 $3)
		(EXPR SUB EXPR)		: (list $2 $1 $3)
		(EXPR MUL EXPR)		: (list $2 $1 $3)
		(EXPR DIV EXPR)		: (list $2 $1 $3)
		(NUM)			: $1)))))

    (define (doit . tokens)
      (let* ((lexer		(make-lexer tokens))
	     (error-handler	(make-error-handler))
	     (parser		(make-parser)))
	(parser lexer error-handler)))

    (define-constant ADD	(make-token 'ADD '+))
    (define-constant SUB	(make-token 'SUB '-))
    (define-constant MUL	(make-token 'MUL '*))
    (define-constant DIV	(make-token 'DIV '/))
    (define-constant ONE	(make-token 'NUM 1))
    (define-constant TWO	(make-token 'NUM 2))

    (check (doit ADD ONE)		=> '(+ 1))
    (check (doit SUB ONE)		=> '(- 1))

    (check (doit ADD ADD ONE)		=> '(+ (+ 1)))
    (check (doit SUB SUB ONE)		=> '(- (- 1)))
    (check (doit ADD SUB ONE)		=> '(+ (- 1)))
    (check (doit SUB ADD ONE)		=> '(- (+ 1)))

    ;;precedence(UNARY) > precedence(ADD).
    (check (doit ADD ONE ADD TWO)	=> '(+ (+ 1) 2))
    ;;precedence(UNARY) > precedence(SUB).
    (check (doit ADD ONE SUB TWO)	=> '(- (+ 1) 2))
    ;;precedence(UNARY) > precedence(MUL).
    (check (doit ADD ONE MUL TWO)	=> '(* (+ 1) 2))
    ;;precedence(UNARY) > precedence(DIV).
    (check (doit ADD ONE DIV TWO)	=> '(/ (+ 1) 2))

    ;;precedence(UNARY) > precedence(ADD).
    (check (doit SUB ONE ADD TWO)	=> '(+ (- 1) 2))
    ;;precedence(UNARY) > precedence(SUB).
    (check (doit SUB ONE SUB TWO)	=> '(- (- 1) 2))

    ;;FIXME Commented  out because they  fail.  I dunno why  because the
    ;;same thing works in the calc example in the other test file.  This
    ;;is Issue #58,  see the relative test file.  (Marco  Maggi; Wed Oct
    ;;23, 2013)
    ;;
    ;; ;;precedence(UNARY) > precedence(MUL).
    ;; (check (doit SUB ONE MUL TWO)	=> '(* (- 1) 2))
    ;; ;;precedence(UNARY) > precedence(DIV).
    ;; (check (doit SUB ONE DIV TWO)	=> '(/ (- 1) 2))

    #f)

  #t)


(parameterise ((check-test-name 'script-expression))

  ;;This is the grammar of the (lalr) documentation in Texinfo format.

  (define terminals
    '(N O C T (left: A) (left: M) (nonassoc: U)))

  (define non-terminals
    '((script	(lines)		: #f)

      (lines	(lines line)	: (yycustom $2)
		(line)		: (yycustom $1))

      (line	(T)		: #\newline
		(E T)		: $1
		(error T)	: #f)

      (E	(N)		: $1
		(E A E)		: ($2 $1 $3)
		(E M E)		: ($2 $1 $3)
		(A E (prec: U))	: ($1 $2)
		(O E C)		: $2)))

  (define make-parser
    (lalr.lalr-parser (lalr.output-value: #t)
		      (lalr.expect: #f)
		      (lalr.terminals: terminals)
		      (lalr.rules: non-terminals)))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
           (result		'())
           (yycustom		(lambda (value)
                                  (set! result (cons value result))
				  'yycustom))
	   (error-handler	(make-error-handler yycustom))
           (parser		(make-parser)))
      (parser lexer error-handler yycustom)
      result))

  (debug:print-tables #f terminals non-terminals)

  (check	;correct input
      (doit (make-token 'T #\newline))
    => '(#\newline))

  (check	;correct input
      (doit (make-token 'N 1)
	    (make-token 'T #\newline))
    => '(1))

  (check	;correct input
      (doit (make-token 'N 1)
	    (make-token 'A +)
	    (make-token 'N 2)
	    (make-token 'T #\newline))
    => '(3))

  (check	;correct input
      (doit (make-token 'N 1)
	    (make-token 'A +)
	    (make-token 'N 2)
	    (make-token 'M *)
	    (make-token 'N 3)
	    (make-token 'T #\newline))
    => '(7))

  (check	;correct input
      (doit (make-token 'O #\()
	    (make-token 'N 1)
	    (make-token 'A +)
	    (make-token 'N 2)
	    (make-token 'C #\))
	    (make-token 'M *)
	    (make-token 'N 3)
	    (make-token 'T #\newline))
    => '(9))

  (check	;correct input
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
	    (make-token 'T #\newline))
    => '(4/5 9))

  #t)


;;;; done

(check-report)

;;; end of file
