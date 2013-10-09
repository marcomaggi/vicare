;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: nested comment lexer table
;;;Date: Tue Dec 28, 2010
;;;
;;;Abstract
;;;
;;;	This table defines a sublexer  for nested comments: "#| ... |#";
;;;	nested comments  can be nested.   We must call this  lexer after
;;;	having consumed a "#|" token  from the input stream and continue
;;;	until we consume the matching "|#" token.  This sublexer must be
;;;	called recursively to consume nested comments.
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

OPEN			"#|"
CLOSE			"|#"

;;Note that we  cannot define the following pattern  as ".+" because its
;;match would  be longer  than the two-characters  "#|" and  "|#"; SILex
;;gives precedence to the longest match.  We have to match one character
;;at a time.
;;
;;Note that the dot matches  all the characters except the newline; also
;;the dot cannot be used in the [...] regex.
;;
CHAR			.|"\n"

%%

{OPEN}		(make-tok open-comment-tok	yytext yyline yycolumn)
{CLOSE}		(make-tok close-comment-tok	yytext yyline yycolumn)
{CHAR}		(parse-ordinary-char		yytext yyline yycolumn)
<<EOF>>		(make-tok eof-tok		yytext yyline yycolumn)

;;; end of file
