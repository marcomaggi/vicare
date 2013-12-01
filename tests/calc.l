;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: calculator lexer specification for SILex
;;;Date: Tue Jul  3, 2007
;;;
;;;Abstract
;;;
;;;	This is a  test lexer specification for a  calculator.  It seems
;;;	to  work well.  To  produce the  lexer table  we can  process it
;;;	using the following program:
;;;
;;;		(import (rnrs) (nausicaa parser-tools silex))
;;;		(lex	input-file:  "calc.l"
;;;			output-file: "calc-lib.sls"
;;;			library-spec: "(calc-lib)"
;;;			table-name: 'calc-lexer-table)
;;;
;;;	this  will produce a  new file  "calc-lib.sls" holding  a proper
;;;	R6RS Scheme library having  import spec "(calc-lib)".  The table
;;;	will  be  exported  by  the  library, bound  to  the  identifier
;;;	"calc-lexer-table".  See the documentation for more details.
;;;
;;;	NOTES
;;;
;;; 	The definition of the  "symbol" regular expression must not hold
;;; 	any of the  chars in [+-*\/^%,] or it  will result in tokenising
;;; 	errors when splitting "1*2", "1/2", "a-b" and such.
;;;
;;;Copyright (C) 2007, 2009, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2000 The Free Software Foundation
;;;
;;;Originally from Guile-Arith was written by Ian Grant <iang@pobox.com>
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

blanks		[ \9\10\13]+

decint          [0-9]+
binint          #[bB][01]+
octint          #[oO][0-7]+
hexint          #[xX][0-9A-Fa-f]+
integer		{decint}|{binint}|{octint}|{hexint}

exponent        ([eE][+\-]?[0-9]+)
truereal	[0-9]+\.|[0-9]*\.[0-9]+{exponent}?|[0-9]+{exponent}
real		{truereal}|{integer}

imag		({decint}|{real})i

nan             \-nan\.0|\+nan\.0|nan\.0
pinf            \+inf\.0|inf\.0
minf            \-inf\.0

initial         [a-zA-Z_]
subsequent      {initial}|[0-9\.@!$&:<=>?~\-]
symbol          {initial}{subsequent}*

operator	<=|>=|//|[\+\-*/%\^<>=]

comma		,

oparen		\(
cparen		\)

%%
{blanks}	;; skip blanks, tabs and newlines
{imag}		(string->number (string-append "+" yytext))
{real}		(string->number yytext)
{nan}		+nan.0
{pinf}		+inf.0
{minf}		-inf.0
{operator}	(case (string->symbol yytext)
		  ((+) '+)
		  ((-) '-)
		  ((*) '*)
		  ((/) '/)
		  ((%) 'mod)
		  ((^) 'expt)
		  ((//) 'div)
		  ((=) '=)
		  ((<) '<)
		  ((>) '>)
		  ((<=) '<=)
		  ((>=) '>=))
{symbol}	(string->symbol yytext)
{comma}		'cons

{oparen}	#\(
{cparen}	#\)

<<EOF>>		(eof-object)
<<ERROR>>	(assertion-violation #f
                  "invalid lexer token")

