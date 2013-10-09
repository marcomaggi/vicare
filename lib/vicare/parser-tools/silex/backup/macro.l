;;; -*- mode: scheme; coding: utf-8-unix; page-delimiter: "^;;page" -*-
;;;
;;;SILex - Scheme Implementation of Lex
;;;
;;;Copyright (C) 2011 by Marco Maggi <marco.maggi-ipsu@poste.it>
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

%[unicode.macros.l]
%[whitespaces-and-comments.macros.l]
%[identifier.macros.l]

hblank			{WHITESPACE-NO-NEWLINE}|{LINECOMMENT-NOEND}
vblank			"\n"
id			{IDENTIFIER}|{PECULIAR_IDENTIFIER}
open-include		"%["
close-include		"]"
percent-include		{open-include}[^\]]+{close-include}
percent-percent		"%%"

%%

{hblank}		(make-tok hblank-tok		yytext yyline yycolumn)
{ONESTEDCOMMENT}	(make-tok open-comment-tok	yytext yyline yycolumn)
{vblank}		(make-tok vblank-tok		yytext yyline yycolumn)
{percent-include}	(parse-percent-include		yytext yyline yycolumn)
{percent-percent}	(make-tok percent-percent-tok	yytext yyline yycolumn)
{id}			(parse-id			yytext yyline yycolumn)
#| everything else is invalid |#
[^]			(make-tok illegal-tok		yytext yyline yycolumn)
<<EOF>>			(make-tok eof-tok		yytext yyline yycolumn)

;;; end of file
