;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for LALR
;;;Date: Thu Jul 16, 2009
;;;
;;;Abstract
;;;
;;;	Simple calculator in Scheme
;;;
;;;	  This  program  illustrates  the  use of  the  lalr-scm  parser
;;;	generator for Scheme. It is NOT robust, since calling a function
;;;	with the  wrong number of  arguments may generate an  error that
;;;	will cause the calculator to crash.
;;;
;;;Copyright (c) 2009-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2004 Dominique Boucher
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
  (prefix (nausicaa parser-tools lalr)  lalr.)
  (prefix (vicare parser-tools silex) lex.))

(define (%log file-name)
  (display (string-append "generating " file-name "\n")
	   (current-error-port)))


;;;; lexer

(define testdir		(getenv "testdir"))

(%log (string-append testdir "/libtest/calc-parser-lexer.sls"))
(lex.lex (lex.output-file:	(string-append testdir "/libtest/calc-parser-lexer.sls"))
	 (lex.counters:		'all)
	 (lex.library-spec:	"(libtest calc-parser-lexer)")
	 (lex.library-language:	'(nausicaa))
	 (lex.library-imports:	'((prefix (nausicaa parser-tools lexical-tokens)   lt.)
				  (prefix (nausicaa parser-tools source-locations) sl.)
				  (nausicaa parser-tools silex default-error-handler)))
	 (lex.table-name:	'calc-parser-lexer-table)
	 (lex.input-string: "
blanks		[ \\9]+
newline		[\\10\\13]+

decint          [0-9]+
binint          #[bB][01]+
octint          #[oO][0-7]+
hexint          #[xX][0-9A-Fa-f]+
integer		{decint}|{binint}|{octint}|{hexint}

exponent        ([eE][+\\-]?[0-9]+)
truereal	[0-9]+\\.|[0-9]*\\.[0-9]+{exponent}?|[0-9]+{exponent}
real		{truereal}|{integer}

imag		({decint}|{real})i

nan             \\-nan\\.0|\\+nan\\.0|nan\\.0
inf             \\inf\\.0

initial         [a-zA-Z_]
subsequent      {initial}|[0-9.@]
symbol          {initial}{subsequent}*

operator	(<=|>=|==|[\\+*/%\\^\\\\<>\\-])
assign		=

comma		,

oparen		\\(
cparen		\\)

%%
{blanks}	;; skip spaced and tabs
{imag}		(lt.<lexical-token> ((lt.category: 'NUM)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    (string->number (string-append \"+\" yytext)))
				     (lt.length:   (string-length yytext))))

{real}		(lt.<lexical-token> ((lt.category: 'NUM)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    (string->number yytext))
				     (lt.length:   (string-length yytext))))

{nan}		(lt.<lexical-token> ((lt.category: 'NUM)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    +nan.0)
				     (lt.length:   (string-length yytext))))

{inf}		(lt.<lexical-token> ((lt.category: 'NUM)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    +inf.0)
				     (lt.length:   (string-length yytext))))

{operator}	(let ((position (sl.<source-location> ((sl.line:   yyline)
						       (sl.column: yycolumn)
						       (sl.offset: yyoffset))))
		      (len	(string-length yytext)))
		  (define (%make-lt category value len)
		    (lt.<lexical-token> ((lt.category: category)
					 (lt.location: position)
					 (lt.value:    value)
					 (lt.length:   len))))
		  (case (string->symbol yytext)
		    ((+)	(%make-lt '+ '+ len))
		    ((-)	(%make-lt '- '- len))
		    ((*)	(%make-lt '* '* len))
		    ((/)	(%make-lt '/ '/ len))
		    ((%)	(%make-lt 'MOD mod len))
		    ((^)	(%make-lt 'EXPT expt len))
		    ((\\x5C;)	(%make-lt 'DIV div len))
		    ((<)	(%make-lt 'LESS < len))
		    ((>)	(%make-lt 'GREAT > len))
		    ((<=)	(%make-lt 'LESSEQ <= len))
		    ((>=)	(%make-lt 'GREATEQ >= len))
		    ((==)	(%make-lt 'EQUAL = len))
		    (else       (error #f \"unknown operator\" yytext len))))

{symbol}	(lt.<lexical-token> ((lt.category: 'ID)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    (string->symbol yytext))
				     (lt.length:   (string-length yytext))))

{assign}	(lt.<lexical-token> ((lt.category: 'ASSIGN)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'ASSIGN)
				     (lt.length:   (string-length yytext))))


{comma}		(lt.<lexical-token> ((lt.category: 'COMMA)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'COMMA)
				     (lt.length:    (string-length yytext))))

{newline}	(lt.<lexical-token> ((lt.category: 'NEWLINE)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'NEWLINE)
				     (lt.length:   (string-length yytext))))

{oparen}	(lt.<lexical-token> ((lt.category: 'LPAREN)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'LPAREN)
				     (lt.length:   (string-length yytext))))

{cparen}	(lt.<lexical-token> ((lt.category: 'RPAREN)
				     (lt.location: (sl.<source-location> ((sl.line:   yyline)
									  (sl.column: yycolumn)
									  (sl.offset: yyoffset))))
				     (lt.value:    'RPAREN)
				     (lt.length:   (string-length yytext))))

<<EOF>>		(silex-default-eof-handler)
<<ERROR>>	(assertion-violation #f \"invalid lexer token\")
"))


;;;; parser

(%log "libtest/calc-parser.sls")
(lalr.lalr-parser

 (lalr.output-file:		(string-append testdir "/libtest/calc-parser.sls"))
		;output a parser, called calc-parser, in a separate file

 (lalr.parser-name:		'make-calc-parser)
 (lalr.library-spec:		'(libtest calc-parser))
 (lalr.library-imports:		'((libtest calc-parser-helper)
				  (rnrs eval)))

 (lalr.dump-table:		"calc-parser-tables.txt")
		;output to a file the human readable LALR table

;;; (lalr.expect:		5)
		;there should be no conflicts

 (lalr.terminals:	'(ID NUM ASSIGN LPAREN RPAREN NEWLINE COMMA
			     (left: + -)
			     (left: * / DIV MOD EXPT LESS GREAT LESSEQ GREATEQ EQUAL)
			     (nonassoc: uminus)
			     (nonassoc: uplus)))

 (lalr.rules:
  '((script	(lines)			: $1)

    (lines	(lines line)		: (let ((result $2))
					    (when result
					      (evaluated-expressions
					       (cons result (evaluated-expressions))))
					    result)
		;this reports the result of the last line
		(line)		: (let ((result $1))
				    (when result
				      (evaluated-expressions
				       (cons result (evaluated-expressions))))
				    result))
		;this reports the result of all the lines but the last

    (line	(assign NEWLINE)	: $1
		(expr   NEWLINE)	: $1

		(error  NEWLINE)	: #f)
		;either a line starts  with an expression or assignment,
		;or it  is an  error; in case  of error discard  all the
		;tokens up until the first newline

    (assign   (ID ASSIGN expr)	: (begin
				    (hashtable-set! (table-of-variables) $1 $3)
				    #f))

    (expr     (expr + expr)	: (+ $1 $3)
	      (expr - expr)	: (- $1 $3)
	      (expr * expr)	: (* $1 $3)
	      (expr / expr)	: (/ $1 $3)
	      (+ expr (prec: uplus))
	      : $2
	      (- expr (prec: uminus))
	      : (- $2)
	      (expr DIV expr)	: (div $1 $3)
	      (expr MOD expr)	: (mod $1 $3)
	      (expr EXPT expr)	: (expt $1 $3)
	      (expr LESS expr)	: (< $1 $3)
	      (expr GREAT expr)	: (> $1 $3)
	      (expr LESSEQ expr)	: (<= $1 $3)
	      (expr GREATEQ expr)	: (>= $1 $3)
	      (expr EQUAL expr)	: (= $1 $3)
	      (ID)		: (hashtable-ref (table-of-variables) $1 #f)
	      (ID LPAREN args RPAREN)
	      : (apply (eval $1 (environment '(rnrs))) $3)
	      (NUM)		: $1
	      (LPAREN expr RPAREN)
	      : $2)

    (args     ()			: '()
	      (expr arg-rest)	: (cons $1 $2))

    (arg-rest (COMMA expr arg-rest)
	      : (cons $2 $3)
	      ()			: '()))))

;;; end of file
