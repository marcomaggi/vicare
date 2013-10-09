;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;SILex - Scheme Implementation of Lex
;;;Copyright (C) 2001  Danny Dube'
;;;
;;;This program is free software;  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation; either version 2 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along  with  this  program;  if  not,  write  to  the  Free  Software
;;;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;;;USA.

;;Table to  parse the  internal of classes  of characters:  "[...]".  It
;;must be used after the opening bracket has alraedy been parsed.

hexradix	(#x)|(#X)
hexdigit	[0123456789abcdefABCDEF]
digit		[0123456789]

%%

"]"			(make-tok rbrack-tok yytext yyline yycolumn)
"-"			(make-tok minus-tok  yytext yyline yycolumn)
"\\n"			(parse-spec-char     yytext yyline yycolumn)
"\\"{digit}+		(parse-digits-char   yytext yyline yycolumn)
"\\-"{digit}+		(parse-digits-char   yytext yyline yycolumn)
{hexradix}{hexdigit}+	(parse-hex-digits-char yytext yyline yycolumn)
"\\"[^]			(parse-quoted-char   yytext yyline yycolumn)
[^]			(parse-ordinary-char yytext yyline yycolumn)
<<EOF>>			(make-tok eof-tok    yytext yyline yycolumn)

;;; end of file
