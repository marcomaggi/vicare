;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: implementation of infix to prefix macro
;;;Date: Tue May 20, 2014
;;;
;;;Abstract
;;;
;;;	This file implements the non-core macro transformer for the INFIX syntax.  It
;;;	is  an  infix  to  prefix   notation  transformer  supporting  the  tradition
;;;	mathematical expressions infix  syntax.  The transformer is based  on a Pratt
;;;	parser as exposed in:
;;;
;;;        Pratt, Vaughan.  "Top Down  Operator Precedence".  Massachussets Institute
;;;        of Technology.  Proceedings of the 1st Annual ACM SIGACT-SIGPLAN Symposium
;;;        on Principles of Programming Languages (1973).
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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

(define (infix-macro input-form.stx)
  ;;Transformer  function used  to expand  Vicare's INFIX  macros from  the top-level
  ;;built in  environment.  Expand  the contents of  INPUT-FORM.STX; return  a syntax
  ;;object that must be further expanded.
  ;;
  (define (synner message subform)
    (syntax-violation 'infix message input-form.stx subform))
  (define (immediate-end-of-input-handler)
    ;;Special case: no input tokens.
    (bless
     '(values)))
  (syntax-match input-form.stx ()
    ((_ . ?expr)
     (let* ((obj*   (reverse (flatten ?expr synner)))
	    (token* (map (lambda (obj)
			   (tokenise obj synner))
		      obj*))
	    (lexer  (make-lexer token*))
	    (expr   (parse lexer synner MINIMUM-BINDING-POWER
			   immediate-end-of-input-handler)))
       expr))
    ))


;;;; helpers

(define (syntax->list stx)
  (syntax-match stx ()
    (() '())
    ((?car . ?cdr)
     (cons ?car (syntax->list ?cdr)))))

(define-syntax-rule (call-nud ?token ?lexer ?synner)
  ;;Call the null-denotator procedure of ?TOKEN.
  ;;
  (let ((token ?token))
    ((<token>-null-denotator token) token ?lexer ?synner)))

(define-syntax-rule (call-led ?token ?lexer ?synner ?left-semantic-value)
  ;;Call the left-denotator procedure of ?TOKEN.
  ;;
  (let ((token ?token))
    ((<token>-left-denotator token) token ?lexer ?synner ?left-semantic-value)))

(define-syntax define-memoized
  (syntax-rules ()
    ((_ ?who ?form)
     (begin
       (define func
	 (let ((memoized-val #f))
	   (lambda ()
	     (or memoized-val
		 (receive-and-return (val)
		     ?form
		   (set! memoized-val val))))))
       (define-syntax ?who
	 (identifier-syntax (func)))))
    ))


;;;; flattening the input expression

(define (atom-stx? stx)
  (atom? (syntax->datum stx)))

(define (atom? obj)
  (or (number?     obj)
      (boolean?    obj)
      (char?       obj)
      (string?     obj)
      (bytevector? obj)
      (null?       obj)))

(define (flatten stx synner)
  ;;Given a syntax  object STX representing an input expression:  decompose it into a
  ;;flat list  of raw tokens and  return the list  holding the raw tokens  in reverse
  ;;order.  We expect  the list to contain:  atoms as defined by  the function ATOM?;
  ;;identifiers; the characters open parenthesis, close parenthesis and comma.
  ;;
  (syntax-match stx (_ unquote quote quasiquote syntax quasisyntax)
    (()	'())
    ((quote . ?stuff)
     stx)
    ((quasiquote . ?stuff)
     stx)
    ((syntax . ?stuff)
     stx)
    ((quasisyntax . ?stuff)
     stx)
    ((unquote . ?stuff)
     (append (flatten ?stuff synner)
	     (list #\,)))
    ((?item* ...)
     (cons #\)
	   (fold-left
	       (lambda (knil item)
		 (syntax-match item ()
		   ;;This clause is needed to  correctly handle the case of procedure
		   ;;application with no arguments.  Examples:
		   ;;
		   ;;   (infix noargs())
		   ;;   (infix 2 + noargs() + 3)
		   ;;
		   ;;in which the "()" must be lexed as the two tokens #\( and #\).
		   (()
		    (append '(#\) #\() knil))
		   (_
		    (let ((R (flatten item synner)))
		      (if (list? R)
			  (append R knil)
			(cons R knil))))))
	     (list #\()
	     (syntax->list ?item*))))
    (?id
     (identifier? ?id)
     ?id)
    (?atom
     (atom-stx? ?atom)
     (syntax->datum ?atom))
    (_
     (synner "invalid expression syntax" stx))))


(define (tokenise obj synner)
  ;;Given a raw object from the flattened  list of input tokens return an instance of
  ;;"<token>" describing it.
  ;;
  (cond ((identifier? obj)
	 (case (syntax->datum obj)
	   ((==)	EQUAL-TO-TOKEN)
	   ((<>)	NOT-EQUAL-TO-TOKEN)
	   ((≠)		NOT-EQUAL-TO-TOKEN)
					;Unicode symbol "\x2260;".
	   ((!)		BANG-TOKEN)
	   ((**)	EXPT-TOKEN)
	   ((%)		IMOD-TOKEN)
	   ((×)		MUL-TOKEN)
					;Unicode symbol "\xD7;".
	   ((⋅)		MUL-TOKEN)
					;Unicode symbol "\x22C5;".

	   ((&&)	AND-TOKEN)
	   ((⏐⏐)	OR-TOKEN)
					;Symbol "\x23D0;\x23D0;".
	   ((∧)	AND-TOKEN)
					;Unicode symbol "\x2227;".
	   ((∨)	OR-TOKEN)
					;Unicode symbol "\x2228;".
	   ((⊻)	XOR-TOKEN)
					;Unicode symbol "\x22BB;".
	   ((¬)		NOT-TOKEN)
					;Unicode symbol "\xAC;".

	   ((⏐)		BITWISE-IOR-TOKEN)
					;Unicode symbol "\x23D0;".

	   ((&)		BITWISE-AND-TOKEN)
	   ((^)		BITWISE-XOR-TOKEN)
	   ((~)		BITWISE-NOT-TOKEN)
	   ((<<)	BITWISE-SHIFT-LEFT-TOKEN)
	   ((>>)	BITWISE-SHIFT-RIGHT-TOKEN)

	   ((?)		QUESTION-MARK-TOKEN)
	   ((:)		COLON-TOKEN)
	   (else
	    (cond
	     ((free-id=? obj ID:+)			PLUS-TOKEN)
	     ((free-id=? obj ID:-)			MINUS-TOKEN)
	     ((free-id=? obj ID:*)			MUL-TOKEN)
	     ((free-id=? obj ID:/)			DIV-TOKEN)
	     ((free-id=? obj ID:++)			INCR-TOKEN)
	     ((free-id=? obj ID:--)			DECR-TOKEN)
	     ((free-id=? obj ID:div)			IDIV-TOKEN)
	     ((free-id=? obj ID:mod)			IMOD-TOKEN)
	     ((free-id=? obj ID:div0)			IDIV0-TOKEN)
	     ((free-id=? obj ID:mod0)			IMOD0-TOKEN)
	     ((free-id=? obj ID:expt)			EXPT-TOKEN)

	     ((free-id=? obj ID:fx+)			FXPLUS-TOKEN)
	     ((free-id=? obj ID:fx-)			FXMINUS-TOKEN)
	     ((free-id=? obj ID:fx*)			FXMUL-TOKEN)
	     ((free-id=? obj ID:fxdiv)			FXDIV-TOKEN)
	     ((free-id=? obj ID:fxmod)			FXMOD-TOKEN)
	     ((free-id=? obj ID:fxdiv0)			FXDIV0-TOKEN)
	     ((free-id=? obj ID:fxmod0)			FXMOD0-TOKEN)

	     ((free-id=? obj ID:fl+)			FLPLUS-TOKEN)
	     ((free-id=? obj ID:fl-)			FLMINUS-TOKEN)
	     ((free-id=? obj ID:fl*)			FLMUL-TOKEN)
	     ((free-id=? obj ID:fl/)			FLDIV-TOKEN)
	     ((free-id=? obj ID:flexpt)			FLEXPT-TOKEN)

	     ((free-id=? obj ID:and)			AND-TOKEN)
	     ((free-id=? obj ID:or)			OR-TOKEN)
	     ((free-id=? obj ID:xor)			XOR-TOKEN)
	     ((free-id=? obj ID:not)			NOT-TOKEN)

	     ((free-id=? obj ID:<)			LESS-THAN-TOKEN)
	     ((free-id=? obj ID:>)			GREATER-THAN-TOKEN)
	     ((free-id=? obj ID:<=)			LESS-THAN-EQUAL-TO-TOKEN)
	     ((free-id=? obj ID:>=)			GREATER-THAN-EQUAL-TO-TOKEN)
	     ((free-id=? obj ID:=)			EQUAL-TO-TOKEN)
	     ((free-id=? obj ID:!=)			NOT-EQUAL-TO-TOKEN)

	     ((free-id=? obj ID:fx<?)			FXLESS-THAN-TOKEN)
	     ((free-id=? obj ID:fx>?)			FXGREATER-THAN-TOKEN)
	     ((free-id=? obj ID:fx<=?)			FXLESS-THAN-EQUAL-TO-TOKEN)
	     ((free-id=? obj ID:fx>=?)			FXGREATER-THAN-EQUAL-TO-TOKEN)
	     ((free-id=? obj ID:fx=?)			FXEQUAL-TO-TOKEN)

	     ((free-id=? obj ID:fl<?)			FLLESS-THAN-TOKEN)
	     ((free-id=? obj ID:fl>?)			FLGREATER-THAN-TOKEN)
	     ((free-id=? obj ID:fl<=?)			FLLESS-THAN-EQUAL-TO-TOKEN)
	     ((free-id=? obj ID:fl>=?)			FLGREATER-THAN-EQUAL-TO-TOKEN)
	     ((free-id=? obj ID:fl=?)			FLEQUAL-TO-TOKEN)

	     ((free-id=? obj ID:eq?)			EQ-PRED-TOKEN)
	     ((free-id=? obj ID:eqv?)			EQV-PRED-TOKEN)
	     ((free-id=? obj ID:equal?)			EQUAL-PRED-TOKEN)

	     ;; bitwise operators
	     ((free-id=? obj ID:bitwise-and)		BITWISE-AND-TOKEN)
	     ((free-id=? obj ID:bitwise-ior)		BITWISE-IOR-TOKEN)
	     ((free-id=? obj ID:bitwise-xor)		BITWISE-XOR-TOKEN)
	     ((free-id=? obj ID:bitwise-not)		BITWISE-NOT-TOKEN)
	     ((free-id=? obj ID:bitwise-arithmetic-shift-left)	BITWISE-SHIFT-LEFT-TOKEN)
	     ((free-id=? obj ID:bitwise-arithmetic-shift-right)	BITWISE-SHIFT-RIGHT-TOKEN)

	     ;; bitwise operators
	     ((free-id=? obj ID:fxand)			FXAND-TOKEN)
	     ((free-id=? obj ID:fxior)			FXIOR-TOKEN)
	     ((free-id=? obj ID:fxxor)			FXXOR-TOKEN)
	     ((free-id=? obj ID:fxnot)			FXNOT-TOKEN)
	     ((free-id=? obj ID:fxarithmetic-shift-left)	FXSHIFT-LEFT-TOKEN)
	     ((free-id=? obj ID:fxarithmetic-shift-right)	FXSHIFT-RIGHT-TOKEN)

	     (else
	      ;;It is a variable.
	      (make-<operand> obj))))))
	((char? obj)
	 (cond ((char=? obj #\()		LEFT-PAREN-TOKEN)
	       ((char=? obj #\))		RIGHT-PAREN-TOKEN)
	       ((char=? obj #\,)		COMMA-TOKEN)
	       (else
		(synner "invalid character object from input" obj))))
	(else
	 (make-<operand> obj))))


(define (make-lexer token*)
  ;;Given a list of "<token>" records return a closure implementing the lexer.
  ;;
  ;;* When the lexer  is called without arguments: it pops the  next token from the
  ;;  list and returns it.
  ;;
  ;;* When the  lexer is called with  a single argument: it returns  the next token
  ;;  from the list without popping it.  The argument itself is ignored.
  ;;
  ;;* When there are no more tokens: the lexer returns the end-of-input object.
  ;;
  (assert (and (list? token*)
	       (for-all <token>? token*)))
  (case-lambda
   ((obj)	;peek
    (if (pair? token*)
	(car token*)
      (end-of-input-object)))
   (()	;get
    (if (pair? token*)
	(receive-and-return (T)
	    (car token*)
	  (set! token* (cdr token*)))
      (end-of-input-object)))))

(define-syntax-rule (end-of-input? ?object-from-lexer)
  (eof-object? ?object-from-lexer))

(define-syntax-rule (end-of-input-object)
  (eof-object))


(define (parse lexer synner caller-right-binding-power
	       immediate-end-of-input-handler)
  ;;This  procedure  is  the core  of  the  Pratt  parser.   It composes  the  next
  ;;sub-expression's semantic value parsing tokens from the lexer; it can be called
  ;;by the operator's left-denotators to produce a right-hand argument.
  ;;
  ;;Let's examine  what happens when parsing the expression:
  ;;
  ;;   1 + 2 - 3
  ;;
  ;;which we represent with the sequence of tokens:
  ;;
  ;;   OPERAND[1] PLUS OPERAND[2] MINUS OPERAND[3]
  ;;
  ;;where PLUS  and MINUS are operator  tokens.  We use the  abbreviations: NUD for
  ;;token's null-denotator procedure; LED for token's left-denotator procedure.
  ;;
  ;;PARSE is called with minimum right-binding power:
  ;;
  ;;01.PARSE consumes  the token OPERAND[1] and  calls its NUD, which  just returns
  ;;the semantic value: fixnum 1.  PARSE tail-calls its sub-procedure LOOP.
  ;;
  ;;02.LOOP consumes the  token PLUS, whose left-binding power is  greater than the
  ;;minimum right-binding power; LOOP calls the LED of PLUS with 1 as left semantic
  ;;value.
  ;;
  ;;03.,,The LED of PLUS calls PARSE with its right-binding power.
  ;;
  ;;04.,,..PARSE  consumes the  token  OPERAND[2]  and calls  its  NUD, which  just
  ;;returns the semantic value: fixnum 2.  PARSE tail-calls LOOP.
  ;;
  ;;05.,,..LOOP looks ahead  the token MINUS, whose left-binding power  is equal to
  ;;the right-binding power or PLUS; LOOP returns the semantic value 2.
  ;;
  ;;06.,,The LED of PLUS composes the semantic value (#'+ 1 2) and returns it.
  ;;
  ;;07.LOOP consumes the token MINUS, whose  left-binding power is greater than the
  ;;minimum right-binding power; LOOP calls the LED of MINUS with (#'+ 1 2) as left
  ;;semantic value.
  ;;
  ;;08.,,The LED of MINUS calls PARSE with its right-binding power.
  ;;
  ;;09.,,..PARSE  consumes the  token  OPERAND[3]  and calls  its  NUD, which  just
  ;;returns the semantic value: fixnum 3.  PARSE tail-calls LOOP.
  ;;
  ;;10.,,..LOOP looks ahead the end-of-input and returns the semantic value 3.
  ;;
  ;;11.,,The LED of MINUS composes the semantic value (#'- (#'+ 1 2) 3) and returns
  ;;it.
  ;;
  ;;12.LOOP looks ahead the end-of-input and returns the semantic value (#'- (#'+ 1
  ;;2) 3).
  ;;
  (define (loop left-semantic-value lexer synner caller-right-binding-power)
    ;;We have acquired a semantic value  from parsing previous tokens.  Now we loop
    ;;parsing tokens until a full sub-expression's semantic value has been formed.
    ;;
    ;;While  the  left-binding  power  of  the  next  token  is  greater  than  the
    ;;right-binding  power  of  the  caller  denotator:  we  continue  calling  the
    ;;left-denotator of the  next token.  It is this very  mechanism that allows us
    ;;to implement operator precedence, left and right associativity.
    ;;
    (define-syntax-rule (recurse ?new-semantic-value)
      (loop ?new-semantic-value lexer synner caller-right-binding-power))
    (let ((token (lexer 'lookahead)))
      (cond ((<token>? token)
	     ;;By using FX>=?  we make  the binary infix operators left-associative
	     ;;by default; example:
	     ;;
	     ;;   (1 + 2 + 3) => (+ (+ 1 2) 3)
	     ;;
	     ;;by using FX>? they would have been right-associative by default.
	     (if (fx>=? caller-right-binding-power (<token>-left-binding-power token))
		 left-semantic-value
	       (begin
		 (lexer) ;consume the token
		 (recurse (call-led token lexer synner left-semantic-value)))))
	    ((end-of-input? token)
	     left-semantic-value)
	    (else
	     (synner "internal error: invalid object from lexer" token)))))

  ;;When correct  input is given:  we expect at least  a sub-expression.  It  is an
  ;;exception if we immediately find end-of-input.
  (let ((token (lexer)))
    (cond ((<token>? token)
	   (loop (call-nud token lexer synner)
		 lexer synner caller-right-binding-power))
	  ((end-of-input? token)
	   ;;We got an EOI before composing a full expression.
	   (immediate-end-of-input-handler))
	  (else
	   (synner "internal error: invalid object from lexer" token)))))


(define-record-type <token>
  ;;This is the base type of tokens for Pratt parsing.
  ;;
  (fields (immutable semantic-value)
		;Scheme object representing  the semantic value of  this token.  This
		;object can be anything.

	  (immutable left-binding-power)
		;A  fixnum  representing  the   left-binding  power  of  this  token.
		;Operands must  have the  minimum left-binding power;  operators must
		;have  a   left-binding  power  which  defines   precedence;  passive
		;syntactic tokens must have minimum left-binding power.

	  (immutable null-denotator)
		;The null-denotator  closure object  (NUD).  It  is called  when this
		;token  does *not*  have  a left  semantic value  to  which it  might
		;left-bind, or it has already been decided that this token does *not*
		;left-bind to the left semantic  value.  This denotator is allowed to
		;select its right-binding power depending on which tokens come next.
		;
		;Both operands  and operators  might have a  NUD that  does something
		;(rather than raising a syntax error exception).
		;
		;A null-denotator has the following signature:
		;
		;   (nud ?this-token ?lexer ?synner)
		;
		;where:  ?THIS-TOKEN  is the  token  instance;  ?LEXER is  the  lexer
		;procedure; ?SYNNER is used to raise errors.

	  (immutable left-denotator))
		;The left-denotator  closure object  (LED).  It  is called  when this
		;token *does*  have a  left semantic  value and  it has  already been
		;decided that this  token *does* left-bind to it.   This denotator is
		;allowed to select its right-binding  power depending on which tokens
		;come next.
		;
		;A left-denotator has the following signature:
		;
		;   (led ?this-token ?lexer ?synner ?left-semantic-value)
		;
		;where:  ?THIS-TOKEN is  this  token instance;  ?LEXER  is the  lexer
		;procedure; ?SYNNER is used  to raise errors; ?LEFT-SEMANTIC-VALUE is
		;the semantic value composed by parsing previous tokens.
  (protocol
   (lambda (make-record)
     (lambda (semantic-value left-binding-power null-denotator left-denotator)
       (assert (fixnum?    left-binding-power))
       (assert (procedure? null-denotator))
       (assert (procedure? left-denotator))
       (make-record semantic-value left-binding-power null-denotator left-denotator)))))


(define-record-type <operand>
  ;;This is the type of operand tokens for Pratt parsing.  Conceptually, an operand
  ;;has both  left and right  binding powers set to  the minimum; the  previous and
  ;;next tokens decide whether an operand will bind to the left or right.
  ;;
  (parent <token>)
  (protocol
   (lambda (make-token)
     (lambda (semantic-value)
       ((make-token semantic-value MINIMUM-BINDING-POWER <operand>-nud <operand>-led))))))

(define (<operand>-nud this-token lexer synner)
  ;;The token is an  operand and it starts a sequence of tokens  which are meant to
  ;;compose a sub-expression.  Return the semantic  value and let the caller decide
  ;;if this  token binds  to the  left or right;  the caller  is usually  the PARSE
  ;;procedure.
  ;;
  (<token>-semantic-value this-token))

(define (<operand>-led this-token lexer synner left-semantic-value)
  (synner "operand found when an operator was expected" this-token))


(define-record-type <operator>
  ;;This is  the type of  operator tokens for Pratt  parsing.  We decide  that only
  ;;identifiers are operators.
  ;;
  (parent <token>)
  (protocol
   (lambda (make-token)
     (lambda (id token-left-binding-power null-denotator left-denotator)
       (identifier? id)
       ((make-token id token-left-binding-power null-denotator left-denotator))))))


(define-record-type <fixed-right-binding-power-operator>
  ;;Record  type of  operators of  which  we know  the right-binding  power of  the
  ;;null-denotator  and left-denotator  procedures is  always the  same, no  matter
  ;;which next tokens come out of the lexer.
  ;;
  (parent <operator>)
  (fields (immutable right-binding-power))
		;A  fixnum  representing  the  right-binding power  of  this  token's
		;null-denotator and left-denotator procedures.
  (protocol
   (lambda (make-operator)
     (lambda (id token-left-binding-power nud/led-right-binding-power
		 null-denotator left-denotator)
       (assert (fixnum? nud/led-right-binding-power))
       ((make-operator id token-left-binding-power null-denotator left-denotator)
	nud/led-right-binding-power)))))


(define-record-type <infix-operator>
  ;;This is the record type of binary infix operators.
  ;;
  (parent <fixed-right-binding-power-operator>)
  (protocol
   (lambda (make-fixed-right-binding-power-operator)
     (lambda (id token-left-binding-power nud/led-right-binding-power)
       ((make-fixed-right-binding-power-operator
	 id token-left-binding-power nud/led-right-binding-power
	 <infix-operator>-nud <infix-operator>-led))))))

(define (<infix-operator>-nud this-token lexer synner)
  ;;This token is  a binary infix operator  and it has *no* left  semantic value it
  ;;might left-bind to: this is a syntax error.
  ;;
  (synner "binary infix operator without left operand" this-token))

(define (<infix-operator>-led this-token lexer synner left-semantic-value)
  ;;This token is a binary infix operator, it  has a left semantic value and it has
  ;;already been decided that this token binds to it.
  ;;
  (define-constant THIS-DENOTATOR-RIGHT-BINDING-POWER
    (<fixed-right-binding-power-operator>-right-binding-power this-token))
  (define (immediate-end-of-input-handler)
    (synner "infix operator without right operand" this-token))
  (let* ((this-semantic-value (<token>-semantic-value this-token))
	 (right-semantic-value (parse lexer synner THIS-DENOTATOR-RIGHT-BINDING-POWER
				      immediate-end-of-input-handler)))
    (list this-semantic-value
	  left-semantic-value
	  right-semantic-value)))


(define-record-type <left-assoc-infix-operator>
  ;;This is the record type of  left-associative binary infix operators like *.  To
  ;;be left-associative the operator X must act as follows:
  ;;
  ;;   A x B x C => (x (x A B) C)
  ;;
  ;;the compared binding powers are the following:
  ;;
  ;;                       A x B x C
  ;;                         ^   ^
  ;;   LED right-binding power   token left-binding power
  ;;
  ;;and the LED right-binding power must be higher than or equal to the other.
  ;;
  (parent <infix-operator>)
  (protocol
   (lambda (make-infix-operator)
     (lambda (id token-left-binding-power nud/led-right-binding-power)
       (assert (fx<=? token-left-binding-power nud/led-right-binding-power))
       ((make-infix-operator id token-left-binding-power
			     nud/led-right-binding-power))))))


(define-record-type <right-assoc-infix-operator>
  ;;This is the record type of  right-associative binary infix operators like EXPT.
  ;;To be right-associative the operator X must act as follows:
  ;;
  ;;   A x B x C => (x A (x B C))
  ;;
  ;;the compared binding powers are the following:
  ;;
  ;;                       A x B x C
  ;;                         ^   ^
  ;;   LED right-binding power   token left-binding power
  ;;
  ;;and the token left-binding power must be higher than the other.
  ;;
  (parent <infix-operator>)
  (protocol
   (lambda (make-infix-operator)
     (lambda (id token-left-binding-power nud/led-right-binding-power)
       (assert (fx>? token-left-binding-power nud/led-right-binding-power))
       ((make-infix-operator id token-left-binding-power
			     nud/led-right-binding-power))))))


(define-record-type <symmetric-left-assoc-infix-operator>
  ;;Record type  of left-associative binary  infix operators  of which we  know the
  ;;left-binding  power   of  the  token   and  the  right-binding  power   of  the
  ;;left-denotator procedure is  always the same, no matter which  next tokens come
  ;;out of the lexer.
  ;;
  ;;Examples are  the common arithmetic operators  +, -, *, /,  the logic operators
  ;;AND, OR, XOR, NOT and the comparison operators <, >, <=, >=, =, !=; notice that
  ;;EXPT is right-associative, so it is not of this type.
  ;;
  (parent <left-assoc-infix-operator>)
  (protocol
   (lambda (make-left-assoc-infix-operator)
     (lambda (id binding-power)
       ((make-left-assoc-infix-operator id binding-power binding-power))))))


(define-record-type <prefix-operator>
  ;;This is the record type of unary prefix operators like NOT.
  ;;
  (parent <fixed-right-binding-power-operator>)
  (protocol
   (lambda (make-fixed-right-binding-power-operator)
     (lambda (id nud/led-right-binding-power)
       ((make-fixed-right-binding-power-operator
	 id MINIMUM-BINDING-POWER nud/led-right-binding-power
	 <prefix-operator>-nud <prefix-operator>-led))))))

(define (<prefix-operator>-nud this-token lexer synner)
  ;;This token is  a unary prefix operator  and it has *no* left  semantic value it
  ;;might left-bind to.
  ;;
  ;;The scenario of tokens from the lexer is this:
  ;;
  ;;   this-token next-token
  ;;
  ;;where NEXT-TOKEN is still in the lexer.
  ;;
  ;;Examples:
  ;;
  ;;* This token is the operator "not" in the following expression:
  ;;
  ;;     not 4
  ;;
  ;;  the  next token is "4".   The next token is  an operand: we just  acquire the
  ;;  next token and call its left-denotator to obtain the full right operand.
  ;;
  ;;* This token is the leftmost operator "not" in the following expression:
  ;;
  ;;     not not 4
  ;;      ^
  ;;     this one
  ;;
  ;;  the next token is "not".  The next  token is an operator: we just acquire the
  ;;  next token and call its null-denotator to obtain the full right operand.
  ;;
  (define-constant THIS-DENOTATOR-RIGHT-BINDING-POWER
    (<fixed-right-binding-power-operator>-right-binding-power this-token))
  (define (immediate-end-of-input-handler)
    (synner "unexpected end of input while parsing operand for unary prefix operator"
	    this-token))
  (let ((this-semantic-value  (<token>-semantic-value this-token))
	(right-semantic-value (parse lexer synner THIS-DENOTATOR-RIGHT-BINDING-POWER
				     immediate-end-of-input-handler)))
    (list this-semantic-value
	  right-semantic-value)))

(define (<prefix-operator>-led this-token lexer synner left-semantic-value)
  ;;This token is a unary prefix operator, it  has a left semantic value and it has
  ;;already been decided that it left-binds to it: this is a syntax error.
  ;;
  (synner "unary prefix operator has no left operand" this-token))


(define-record-type <postfix-operator>
  ;;This  is  the  record type  of  unary  postfix  operators  like "!"  (which  is
  ;;factorial).
  ;;
  (parent <fixed-right-binding-power-operator>)
  (protocol
   (lambda (make-fixed-right-binding-power-operator)
     (lambda (id token-left-binding-power)
       ((make-fixed-right-binding-power-operator
	 id token-left-binding-power MINIMUM-BINDING-POWER
	 <postfix-operator>-nud <postfix-operator>-led))))))

(define (<postfix-operator>-nud this-token lexer synner)
  ;;This token is a  unary postfix operator and it has *no*  left semantic value it
  ;;might left-bind to: this is a syntax error.
  ;;
  (synner "unary postfix operator without left operand" this-token))

(define (<postfix-operator>-led this-token lexer synner left-semantic-value)
  ;;This token is a unary postfix operator, it has a left semantic value and it has
  ;;already been been decided that it left-binds to it.
  ;;
  (list (<token>-semantic-value this-token) left-semantic-value))


(define-record-type <left-assoc-infix/prefix-operator>
  ;;This is the record type of operators that  can be used both as binary infix and
  ;;unary prefix.  Examples are the arithmetic operators + and -.
  ;;
  (parent <fixed-right-binding-power-operator>)
  (protocol
   (lambda (make-fixed-right-binding-power-operator)
     (lambda (id token-left-binding-power nud/led-right-binding-power)
       ((make-fixed-right-binding-power-operator
	 id token-left-binding-power nud/led-right-binding-power
	 <prefix-operator>-nud <infix-operator>-led))))))


(define-record-type <prefix/postfix-operator>
  ;;This is the base  record type for operators that can appear  in both prefix and
  ;;postfix position, for example the increment and decrement operators.
  ;;
  (parent <fixed-right-binding-power-operator>)
  (fields (immutable postfix-semantic-value))
		;A  semantic  value to  be  used  when  the  operator is  in  postfix
		;position.
  (protocol
   (lambda (make-fixed-right-binding-power-operator)
     (lambda (prefix-id postfix-id token-left-binding-power nud/led-right-binding-power)
       (assert (identifier? postfix-id))
       ((make-fixed-right-binding-power-operator
	 prefix-id token-left-binding-power nud/led-right-binding-power
	 <prefix-operator>-nud <prefix/postfix-operator>-led)
	postfix-id)))))

(define (<prefix/postfix-operator>-led this-token lexer synner left-semantic-value)
  ;;This token is a unary prefix/postfix operator, it has a left semantic value and
  ;;it has  already been  been decided  that it  left-binds to  it; this  means the
  ;;operator is in postfix position.
  ;;
  (list (<prefix/postfix-operator>-postfix-semantic-value this-token)
	left-semantic-value))


(define-record-type <symmetric-prefix/postfix-operator>
  ;;This  is the  record type  for operators  that can  appear in  both prefix  and
  ;;postfix position, for example the  increment and decrement operators, for which
  ;;we know  the token left-binding  power equals the  left-denotator right-binding
  ;;power.
  ;;
  (parent <prefix/postfix-operator>)
  (protocol
   (lambda (make-prefix/postfix-operator)
     (lambda (prefix-id postfix-id binding-power)
       ((make-prefix/postfix-operator prefix-id postfix-id
				      binding-power binding-power))))))


(define-record-type <passive-syntactic-token>
  ;;This is the base  type of passive syntactic tokens.  These  tokens are meant to
  ;;be consumed by the NUD or LED of  operators; the NUD and LED of a passive token
  ;;are never called when correct input is parsed.
  ;;
  (parent <token>)
  (fields (immutable description))
  (protocol
   (lambda (make-token)
     (lambda (description)
       ((make-token #f MINIMUM-BINDING-POWER
		    <passive-syntactic-token>-nud
		    <passive-syntactic-token>-led)
	description)))))

(define (<passive-syntactic-token>-syntax-error-message token)
  (string-append "unexpected " (<passive-syntactic-token>-description token)))

(define (<passive-syntactic-token>-nud this-token lexer synner)
  (synner (<passive-syntactic-token>-syntax-error-message this-token)
	  this-token))

(define (<passive-syntactic-token>-led this-token lexer synner left-semantic-value)
  (synner (<passive-syntactic-token>-syntax-error-message this-token)
	  this-token))


(define-record-type <right-paren>
  ;;The right parenthesis  is meant to be  passively consumed by the NUD  or LED of
  ;;the left parenthesis.
  ;;
  (parent <passive-syntactic-token>)
  (protocol
   (lambda (make-passive-syntactic-token)
     (let ((memoised #f))
       (lambda ()
	 (or memoised
	     (receive-and-return (V)
		 ((make-passive-syntactic-token "right parenthesis"))
	       (set! memoised V))))))))


(define-record-type <comma>
  ;;The comma separator  is meant to be  passively consumed by the LED  of the left
  ;;parenthesis.
  ;;
  (parent <passive-syntactic-token>)
  (protocol
   (lambda (make-passive-syntactic-token)
     (let ((memoised #f))
       (lambda ()
	 (or memoised
	     (receive-and-return (V)
		 ((make-passive-syntactic-token "comma separator"))
	       (set! memoised V))))))))


(define-record-type <left-paren>
  ;;The left parenthesis is an operator.
  ;;
  (parent <fixed-right-binding-power-operator>)
  (protocol
   (lambda (make-fixed-right-binding-power-operator)
     (let ((memoised #f))
       (lambda ()
	 (or memoised
	     (receive-and-return (V)
		 ((make-fixed-right-binding-power-operator
		   #\(
		   LEFT-PAREN-LEFT-BINDING-POWER
		   LEFT-PAREN-RIGHT-BINDING-POWER
		   <left-paren>-nud <left-paren>-led))
	       (set! memoised V))))))))

(define (<left-paren>-nud this-token lexer synner)
  ;;This token is a  left parenthesis and there is no left  semantic value it might
  ;;left-bind to.  Open  a parenthetical sub-expression: read  a sub-expression and
  ;;consume a right parenthesis token.
  ;;
  ;;Example, when this procedure is called the left-paren has just been consumed:
  ;;
  ;;   a * (b - c)
  ;;        ^
  ;;    we are here
  ;;
  (define-constant THIS-DENOTATOR-RIGHT-BINDING-POWER
    (<fixed-right-binding-power-operator>-right-binding-power this-token))
  (define (end-of-input-handler)
    (synner "end of input while looking for matching sub-expression right parenthesis"
	    this-token))
  (let ((left-semantic-value (parse lexer synner
				    THIS-DENOTATOR-RIGHT-BINDING-POWER
				    end-of-input-handler)))
    (let ((token (lexer)))
      (cond ((<token>? token)
	     (if (<right-paren>? token)
		 left-semantic-value
	       (synner "expected matching right parenthesis" token)))
	    ((end-of-input? token)
	     (end-of-input-handler))
	    (else
	     (synner "internal error: invalid object from lexer" token))))))

(define (<left-paren>-led this-token lexer synner left-semantic-value)
  ;;This token  is a left parenthesis,  there is a  left semantic value and  it has
  ;;already  been decided  that this  token left-binds  to it.   Start the  list of
  ;;arguments for a procedure  application sub-expression: read arguments separated
  ;;by comma tokens and finally consume the right parenthesis token.
  ;;
  ;;Example, when this procedure is called the left-paren has just been consumed:
  ;;
  ;;   func ( arg1 , arg , ... )
  ;;          ^
  ;;    we are here
  ;;
  ;;We expect  the LEFT-SEMANTIC-VALUE to  represent an expression evaluating  to a
  ;;procedure and the next tokens to form an arguments list.
  ;;
  ;;NOTE We use the comma as  arguments separator; the Scheme reader transforms the
  ;;sequence:
  ;;
  ;;   , ?form
  ;;
  ;;into:
  ;;
  ;;  (unsyntax ?form)
  ;;
  ;;so the list of arguments is:
  ;;
  ;;  ( arg1 (unsyntax arg) ... )
  ;;
  ;;NOTE R6RS does not define the comma to be a delimiter, so writing:
  ;;
  ;;   func (arg1, arg, ...)
  ;;
  ;;with no  space between the ARG  and the comma is  a syntax error; it  should be
  ;;trivial to change the Scheme reader to handle the comma as a delimiter.
  ;;
  (define-constant THIS-DENOTATOR-RIGHT-BINDING-POWER
    (<fixed-right-binding-power-operator>-right-binding-power this-token))

  (define (end-of-input-handler)
    (synner "unexpected end of input while reading list of procedure application arguments" this-token))

  (define (unexpected-token token)
    (synner "unexpected token while reading list of procedure application arguments" token))

  (define (loop reversed-list-of-arguments)
    (let ((token (lexer 'lookahead)))
      (cond ((<token>? token)
	     (cond ((<right-paren>? token)
		    (lexer) ;consume the token
		    ;;Return a  semantic value representing a  function application
		    ;;with arguments.
		    (cons left-semantic-value (reverse reversed-list-of-arguments)))
		   ((<comma>? token)
		    (lexer) ;consume the token
		    (let ((next-semantic-value (parse lexer synner
						      THIS-DENOTATOR-RIGHT-BINDING-POWER
						      end-of-input-handler)))
		      (loop (cons next-semantic-value reversed-list-of-arguments))))
		   (else
		    (unexpected-token token))))
	    ((end-of-input? token)
	     (end-of-input-handler))
	    (else
	     (synner "internal error: invalid object from lexer" token)))))

  (unless (identifier? left-semantic-value)
    (synner "expected identifier as left operand in procedure application"
	    left-semantic-value))

  ;;First we expect a closed parenthesis or an argument.
  (let ((token (lexer 'lookahead)))
    (cond ((<token>? token)
	   (if (<right-paren>? token)
	       (begin
		 (lexer) ;consume the token
		 ;;Return a semantic value representing a function application with
		 ;;no arguments.
		 (list left-semantic-value))
	     (loop (list (parse lexer synner THIS-DENOTATOR-RIGHT-BINDING-POWER
				end-of-input-handler)))))
	  ((end-of-input? token)
	   (end-of-input-handler))
	  (else
	   (synner "internal error: invalid object from lexer" token)))))


(define-record-type <colon>
  ;;The colon is the separator in the syntax:
  ;;
  ;;   ?test ? ?consequent : ?alternate
  ;;
  ;;it is  meant to be  passively consumed by  the left-denotator procedure  of the
  ;;question mark.
  ;;
  (parent <passive-syntactic-token>)
  (protocol
   (lambda (make-passive-syntactic-token)
     (let ((memoised #f))
       (lambda ()
	 (or memoised
	     (receive-and-return (V)
		 ((make-passive-syntactic-token "colon"))
	       (set! memoised V))))))))


(define-record-type <question-mark>
  ;;The question mark is the operator in the ternary conditional expression syntax:
  ;;
  ;;   ?test ? ?consequent : ?alternate
  ;;
  (parent <fixed-right-binding-power-operator>)
  (protocol
   (lambda (make-fixed-right-binding-power-operator)
     (let ((memoised #f))
       (lambda ()
	 (or memoised
	     (receive-and-return (V)
		 ((make-fixed-right-binding-power-operator
		   #\?
		   QUESTION-MARK-LEFT-BINDING-POWER
		   QUESTION-MARK-RIGHT-BINDING-POWER
		   <question-mark>-nud <question-mark>-led))
	       (set! memoised V))))))))

(define (<question-mark>-nud this-token lexer synner)
  ;;This token  is a question  mark and  there is no  left semantic value  it might
  ;;left-bind to: this is a syntax error.
  ;;
  (synner "question mark operator without test operand" this-token))

(define (<question-mark>-led this-token lexer synner left-semantic-value)
  ;;This  token is  a question  mark, there  is a  left semantic  value and  it has
  ;;already been decided that this token left-binds to it.
  ;;
  ;;We expect  the LEFT-SEMANTIC-VALUE to  represent the test expression.   When we
  ;;enter this procedure the question mark has been already parsed:
  ;;
  ;;   ?test ? ?consequent : ?alternate
  ;;           ^
  ;;           we are here
  ;;
  ;;so we read a sub-expression as  consequent, consume the colon passive syntactic
  ;;token, and finally read a sub-expression as alternate.
  ;;
  (define-constant THIS-DENOTATOR-RIGHT-BINDING-POWER
    (<fixed-right-binding-power-operator>-right-binding-power this-token))

  (define (end-of-input-handler)
    (synner "unexpected end of input while reading ternary conditional expression" this-token))

  (define (unexpected-token token)
    (synner "unexpected token while reading ternary conditional expression" token))

  (let ((consequent (parse lexer synner THIS-DENOTATOR-RIGHT-BINDING-POWER
			   end-of-input-handler)))
    (let ((token (lexer)))
      (cond ((<token>? token)
	     (if (<colon>? token)
		 (let ((alternate (parse lexer synner THIS-DENOTATOR-RIGHT-BINDING-POWER
					 end-of-input-handler)))
		   (bless
		    `(if ,left-semantic-value
			 ,consequent
		       ,alternate)))
	       (unexpected-token token)))
	    ((end-of-input? token)
	     (end-of-input-handler))
	    (else
	     (synner "internal error: invalid object from lexer" token))))))


;;;; binding powers

;;The fixnum  zero can  be used  as minimum binding  power (but  such value  is not
;;enforced as minimum, negative fixnums will work just fine).
(define-constant MINIMUM-BINDING-POWER			0)
(define-constant INFIX-LOGIC-BINDING-POWER		300)
(define-constant PREFIX-LOGIC-BINDING-POWER		350)
(define-constant BANG-RIGHT-BINDING-POWER		PREFIX-LOGIC-BINDING-POWER)
(define-constant COMPARISON-BINDING-POWER		400)
(define-constant PLUS/MINUS-BINDING-POWER		500)
(define-constant MUL/DIV-BINDING-POWER			600)
(define-constant MOD-BINDING-POWER			650)
(define-constant EXPT-RIGHT-BINDING-POWER		700)
(define-constant EXPT-LEFT-BINDING-POWER		800)
(define-constant FACTORIAL-LEFT-BINDING-POWER		900)
(define-constant INCR/DECR-BINDING-POWER		1000)
(define-constant INFIX-BITWISE-BINDING-POWER		1300)
(define-constant PREFIX-BITWISE-BINDING-POWER		1350)
(define-constant INFIX-BITSHIFT-BINDING-POWER		1400)
(define-constant LEFT-PAREN-LEFT-BINDING-POWER		2000)
(define-constant LEFT-PAREN-RIGHT-BINDING-POWER		MINIMUM-BINDING-POWER)

(define-constant QUESTION-MARK-LEFT-BINDING-POWER	400)
(define-constant QUESTION-MARK-RIGHT-BINDING-POWER	MINIMUM-BINDING-POWER)


;;;; identifiers
;;
;;We need to make sure that when CORE-PRIM-ID is applied to the symbols: the required
;;initialisation has  been already performed.   For this we  cannot put the  calls to
;;CORE-PRIM-ID  in the  initialisation invoke  code of  this library,  rather we  use
;;memoizing procedures.

(define-memoized ID:+				(core-prim-id '+))
(define-memoized ID:-				(core-prim-id '-))
(define-memoized ID:*				(core-prim-id '*))
(define-memoized ID:/				(core-prim-id '/))
(define-memoized ID:div				(core-prim-id 'div))
(define-memoized ID:mod				(core-prim-id 'mod))
(define-memoized ID:div0			(core-prim-id 'div0))
(define-memoized ID:mod0			(core-prim-id 'mod0))
(define-memoized ID:factorial			(core-prim-id 'factorial))
(define-memoized ID:expt			(core-prim-id 'expt))
(define-memoized ID:++				(core-prim-id '++))
(define-memoized ID:--				(core-prim-id '--))
(define-memoized ID:pre-incr!			(core-prim-id 'pre-incr!))
(define-memoized ID:pre-decr!			(core-prim-id 'pre-decr!))
(define-memoized ID:post-incr!			(core-prim-id 'post-incr!))
(define-memoized ID:post-decr!			(core-prim-id 'post-decr!))
(define-memoized ID:fx+				(core-prim-id 'fx+))
(define-memoized ID:fx-				(core-prim-id 'fx-))
(define-memoized ID:fx*				(core-prim-id 'fx*))
(define-memoized ID:fxdiv			(core-prim-id 'fxdiv))
(define-memoized ID:fxmod			(core-prim-id 'fxmod))
(define-memoized ID:fxdiv0			(core-prim-id 'fxdiv0))
(define-memoized ID:fxmod0			(core-prim-id 'fxmod0))
(define-memoized ID:fl+				(core-prim-id 'fl+))
(define-memoized ID:fl-				(core-prim-id 'fl-))
(define-memoized ID:fl*				(core-prim-id 'fl*))
(define-memoized ID:fl/				(core-prim-id 'fl/))
(define-memoized ID:flexpt			(core-prim-id 'flexpt))
(define-memoized ID:and				(core-prim-id 'and))
(define-memoized ID:or				(core-prim-id 'or))
(define-memoized ID:xor				(core-prim-id 'xor))
(define-memoized ID:not				(core-prim-id 'not))
(define-memoized ID:<				(core-prim-id '<))
(define-memoized ID:>				(core-prim-id '>))
(define-memoized ID:<=				(core-prim-id '<=))
(define-memoized ID:>=				(core-prim-id '>=))
(define-memoized ID:=				(core-prim-id '=))
(define-memoized ID:!=				(core-prim-id '!=))
(define-memoized ID:fx<?			(core-prim-id 'fx<?))
(define-memoized ID:fx>?			(core-prim-id 'fx>?))
(define-memoized ID:fx<=?			(core-prim-id 'fx<=?))
(define-memoized ID:fx>=?			(core-prim-id 'fx>=?))
(define-memoized ID:fx=?			(core-prim-id 'fx=?))
(define-memoized ID:fl<?			(core-prim-id 'fl<?))
(define-memoized ID:fl>?			(core-prim-id 'fl>?))
(define-memoized ID:fl<=?			(core-prim-id 'fl<=?))
(define-memoized ID:fl>=?			(core-prim-id 'fl>=?))
(define-memoized ID:fl=?			(core-prim-id 'fl=?))
(define-memoized ID:eq?				(core-prim-id 'eq?))
(define-memoized ID:eqv?			(core-prim-id 'eqv?))
(define-memoized ID:equal?			(core-prim-id 'equal?))
(define-memoized ID:bitwise-and			(core-prim-id 'bitwise-and))
(define-memoized ID:bitwise-ior			(core-prim-id 'bitwise-ior))
(define-memoized ID:bitwise-xor			(core-prim-id 'bitwise-xor))
(define-memoized ID:bitwise-not			(core-prim-id 'bitwise-not))
(define-memoized ID:bitwise-arithmetic-shift-left	(core-prim-id 'bitwise-arithmetic-shift-left))
(define-memoized ID:bitwise-arithmetic-shift-right	(core-prim-id 'bitwise-arithmetic-shift-right))
(define-memoized ID:fxand			(core-prim-id 'fxand))
(define-memoized ID:fxior			(core-prim-id 'fxior))
(define-memoized ID:fxxor			(core-prim-id 'fxxor))
(define-memoized ID:fxnot			(core-prim-id 'fxnot))
(define-memoized ID:fxarithmetic-shift-left	(core-prim-id 'fxarithmetic-shift-left))
(define-memoized ID:fxarithmetic-shift-right	(core-prim-id 'fxarithmetic-shift-right))


;;;; tokens

(define-memoized PLUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> ID:+ PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define-memoized MINUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> ID:- PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define-memoized MUL-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:* MUL/DIV-BINDING-POWER))

(define-memoized DIV-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:/ MUL/DIV-BINDING-POWER))

(define-memoized IDIV-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:div MUL/DIV-BINDING-POWER))

(define-memoized IMOD-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:mod MOD-BINDING-POWER))

(define-memoized IDIV0-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:div0 MUL/DIV-BINDING-POWER))

(define-memoized IMOD0-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:mod0 MOD-BINDING-POWER))

(define-memoized EXPT-TOKEN
  (make-<right-assoc-infix-operator> ID:expt EXPT-LEFT-BINDING-POWER EXPT-RIGHT-BINDING-POWER))

(define-memoized BANG-TOKEN
  (make-<prefix/postfix-operator> ID:not ID:factorial FACTORIAL-LEFT-BINDING-POWER PREFIX-LOGIC-BINDING-POWER))

(define-memoized INCR-TOKEN
  (make-<symmetric-prefix/postfix-operator> ID:pre-incr! ID:post-incr! INCR/DECR-BINDING-POWER))

(define-memoized DECR-TOKEN
  (make-<symmetric-prefix/postfix-operator> ID:pre-decr! ID:post-decr! INCR/DECR-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized FXPLUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> ID:fx+ PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define-memoized FXMINUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> ID:fx- PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define-memoized FXMUL-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fx* MUL/DIV-BINDING-POWER))

(define-memoized FXDIV-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxdiv MUL/DIV-BINDING-POWER))

(define-memoized FXMOD-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxmod MOD-BINDING-POWER))

(define-memoized FXDIV0-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxdiv0 MUL/DIV-BINDING-POWER))

(define-memoized FXMOD0-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxmod0 MOD-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized FLPLUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> ID:fl+ PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define-memoized FLMINUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> ID:fl- PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define-memoized FLMUL-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fl* MUL/DIV-BINDING-POWER))

(define-memoized FLDIV-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fl/ MUL/DIV-BINDING-POWER))

(define-memoized FLEXPT-TOKEN
  (make-<right-assoc-infix-operator> ID:flexpt EXPT-LEFT-BINDING-POWER EXPT-RIGHT-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized AND-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:and INFIX-LOGIC-BINDING-POWER))

(define-memoized OR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:or INFIX-LOGIC-BINDING-POWER))

(define-memoized XOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:xor INFIX-LOGIC-BINDING-POWER))

(define-memoized NOT-TOKEN
  (make-<prefix-operator> ID:not PREFIX-LOGIC-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized LESS-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:< COMPARISON-BINDING-POWER))

(define-memoized GREATER-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:> COMPARISON-BINDING-POWER))

(define-memoized LESS-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:<= COMPARISON-BINDING-POWER))

(define-memoized GREATER-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:>= COMPARISON-BINDING-POWER))

(define-memoized EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:= COMPARISON-BINDING-POWER))

(define-memoized NOT-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:!= COMPARISON-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized FXLESS-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fx<? COMPARISON-BINDING-POWER))

(define-memoized FXGREATER-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fx>? COMPARISON-BINDING-POWER))

(define-memoized FXLESS-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fx<=? COMPARISON-BINDING-POWER))

(define-memoized FXGREATER-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fx>=? COMPARISON-BINDING-POWER))

(define-memoized FXEQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fx=? COMPARISON-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized FLLESS-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fl<? COMPARISON-BINDING-POWER))

(define-memoized FLGREATER-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fl>? COMPARISON-BINDING-POWER))

(define-memoized FLLESS-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fl<=? COMPARISON-BINDING-POWER))

(define-memoized FLGREATER-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fl>=? COMPARISON-BINDING-POWER))

(define-memoized FLEQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fl=? COMPARISON-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized EQ-PRED-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:eq? COMPARISON-BINDING-POWER))

(define-memoized EQV-PRED-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:eqv? COMPARISON-BINDING-POWER))

(define-memoized EQUAL-PRED-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:equal? COMPARISON-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized BITWISE-AND-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:bitwise-and INFIX-BITWISE-BINDING-POWER))

(define-memoized BITWISE-IOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:bitwise-ior INFIX-BITWISE-BINDING-POWER))

(define-memoized BITWISE-XOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:bitwise-xor INFIX-BITWISE-BINDING-POWER))

(define-memoized BITWISE-NOT-TOKEN
  (make-<prefix-operator> ID:bitwise-not PREFIX-BITWISE-BINDING-POWER))

(define-memoized BITWISE-SHIFT-LEFT-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:bitwise-arithmetic-shift-left INFIX-BITSHIFT-BINDING-POWER))

(define-memoized BITWISE-SHIFT-RIGHT-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:bitwise-arithmetic-shift-right INFIX-BITSHIFT-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized FXAND-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxand INFIX-BITWISE-BINDING-POWER))

(define-memoized FXIOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxior INFIX-BITWISE-BINDING-POWER))

(define-memoized FXXOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxxor INFIX-BITWISE-BINDING-POWER))

(define-memoized FXNOT-TOKEN
  (make-<prefix-operator> ID:fxnot PREFIX-BITWISE-BINDING-POWER))

(define-memoized FXSHIFT-LEFT-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxarithmetic-shift-left INFIX-BITSHIFT-BINDING-POWER))

(define-memoized FXSHIFT-RIGHT-TOKEN
  (make-<symmetric-left-assoc-infix-operator> ID:fxarithmetic-shift-right INFIX-BITSHIFT-BINDING-POWER))

;;; --------------------------------------------------------------------

(define-memoized LEFT-PAREN-TOKEN
  (make-<left-paren>))

(define-memoized RIGHT-PAREN-TOKEN
  (make-<right-paren>))

(define-memoized COMMA-TOKEN
  (make-<comma>))

(define-memoized QUESTION-MARK-TOKEN
  (make-<question-mark>))

(define-memoized COLON-TOKEN
  (make-<colon>))

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; mode: vicare
;; fill-column: 85
;; End:
