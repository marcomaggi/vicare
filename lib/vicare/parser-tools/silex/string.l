;;; -*- mode: scheme; coding: utf-8-unix-*-
;;;
;;;SILex - Scheme Implementation of Lex
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;; this is the same string specification of the R6RS lexer
%[string.macros.l]

%%

{DOUBLEQUOTE}		(make-tok doublequote-tok yytext yyline yycolumn)
{ESCAPED-A}		(parse-escaped-char	yytext yyline yycolumn #\x7)
{ESCAPED-B}		(parse-escaped-char	yytext yyline yycolumn #\x8)
{ESCAPED-T}		(parse-escaped-char	yytext yyline yycolumn #\x9)
;;newline is special for SILex
{ESCAPED-N}		(parse-spec-char	yytext yyline yycolumn)
{ESCAPED-V}		(parse-escaped-char	yytext yyline yycolumn #\xB)
{ESCAPED-F}		(parse-escaped-char	yytext yyline yycolumn #\xC)
{ESCAPED-R}		(parse-escaped-char	yytext yyline yycolumn #\xD)
{ESCAPED-DOUBLEQUOTE}	(parse-escaped-char	yytext yyline yycolumn #\x22)
{ESCAPED-BACKSLASH}	(parse-escaped-char	yytext yyline yycolumn #\x5C)
{LINE-WRAP-TO-IGNORE}	;;ignored
{INLINE-HEX-ESCAPE}	(parse-inline-hex-escape yytext yyline yycolumn)
[^]			(parse-ordinary-char	yytext yyline yycolumn)
<<EOF>>			(make-tok eof-tok	yytext yyline yycolumn)

;;; end of file
