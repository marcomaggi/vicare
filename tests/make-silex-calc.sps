;;;
;;;Part of: Vicare Scheme
;;;Contents: test program for SILex
;;;Date: Fri Jul 17, 2009
;;;
;;;Abstract
;;;
;;;	Run this program with:
;;;
;;;	   $ VICARE_LIBRARY_PATH=./path/to/libs \
;;;		vicare make-calc-test.sps
;;;
;;;	it will produce the "libtest/calc-*-lexer.sls" output files.
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
(import (vicare)
  (vicare parser-tools silex))


(define l "
blanks		[ \\9\\10\\13]+

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
pinf            \\+inf\\.0|inf\\.0
minf            \\-inf\\.0

initial         [a-zA-Z_]
subsequent      {initial}|[0-9\\.@!$&:<=>?~\\-]
symbol          {initial}{subsequent}*

operator	<=|>=|//|[\\+\\-*/%\\^<>=]

comma		,

oparen		\\(
cparen		\\)

%%
{blanks}	;; skip blanks, tabs and newlines
{imag}		(string->number (string-append \"+\" yytext))
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

{oparen}	#\\(
{cparen}	#\\)

<<EOF>>		(eof-object)
<<ERROR>>	(assertion-violation #f
                  \"invalid lexer token\")
")


(define testdir		(getenv "testdir"))
(define lexer-file	(string-append testdir "/calc.l"))
(define tree-file	(string-append testdir "/libtest/calc-tree-lexer.sls"))
(define code-file	(string-append testdir "/libtest/calc-code-lexer.sls"))
(define portable-file	(string-append testdir "/libtest/calc-portable-lexer.sls"))

(define lp (open-string-input-port l))

(display (string-append "generating " tree-file "\n") (current-error-port))
(lex (input-file: lexer-file) (output-file: tree-file)
     (library-spec: "(libtest calc-tree-lexer)")
     (table-name: 'calc-lexer-table/tree))

(display (string-append "generating " code-file "\n") (current-error-port))
(lex (input-string: l) (output-file: code-file)
     (lexer-format: 'code)
     (library-spec: "(libtest calc-code-lexer)")
     (table-name: 'calc-lexer-table/code))

(display (string-append "generating " portable-file "\n") (current-error-port))
(lex (input-port: lp) (output-file: portable-file)
     (lexer-format: 'portable)
     (library-spec: "(libtest calc-portable-lexer)")
     (table-name: 'calc-lexer-table/portable))

;;; end of file
