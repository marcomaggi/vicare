;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;SILex - Scheme Implementation of Lex
;;;
;;;Copyright (C) 2011  Marco Maggi <marco.maggi-ipsu@poste.it>
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
digit			[0123456789]
hexdigit		[0123456789abcdefABCDEF]
id			{IDENTIFIER}|{PECULIAR_IDENTIFIER}

%%

{hblank}			(make-tok hblank-tok        yytext yyline yycolumn)
{ONESTEDCOMMENT}		(make-tok open-comment-tok  yytext yyline yycolumn)
{vblank}                  (make-tok vblank-tok        yytext yyline yycolumn)
"|"                       (make-tok pipe-tok          yytext yyline yycolumn)
"?"                       (make-tok question-tok      yytext yyline yycolumn)
"+"                       (make-tok plus-tok          yytext yyline yycolumn)
"*"                       (make-tok star-tok          yytext yyline yycolumn)
"("                       (make-tok lpar-tok          yytext yyline yycolumn)
")"                       (make-tok rpar-tok          yytext yyline yycolumn)
"."                       (make-tok dot-tok           yytext yyline yycolumn)
"["                       (make-tok lbrack-tok        yytext yyline yycolumn)
"[]"                      (make-tok lbrack-rbrack-tok yytext yyline yycolumn)
"[^"                      (make-tok lbrack-caret-tok  yytext yyline yycolumn)
"[-"                      (make-tok lbrack-minus-tok  yytext yyline yycolumn)
"{"{id}"}"                (parse-id-ref               yytext yyline yycolumn)
"{"{digit}+"}"            (parse-power-m              yytext yyline yycolumn)
"{"{digit}+",}"           (parse-power-m-inf          yytext yyline yycolumn)
"{"{digit}+","{digit}+"}" (parse-power-m-n            yytext yyline yycolumn)
"{"                       (make-tok illegal-tok       yytext yyline yycolumn)
"\""                      (make-tok doublequote-tok   yytext yyline yycolumn)
"\\n"                     (parse-spec-char            yytext yyline yycolumn)
"\\"{digit}+              (parse-digits-char          yytext yyline yycolumn)
"\\-"{digit}+             (parse-digits-char          yytext yyline yycolumn)
"#x"{hexdigit}+";"	  (parse-hex-digits-char      yytext yyline yycolumn)
"\\"[^]                   (parse-quoted-char          yytext yyline yycolumn)
"^"                       (make-tok caret-tok         yytext yyline yycolumn)
"$"                       (make-tok dollar-tok        yytext yyline yycolumn)
.                         (parse-ordinary-char        yytext yyline yycolumn)
"<<EOF>>"                 (make-tok <<EOF>>-tok       yytext yyline yycolumn)
"<<ERROR>>"               (make-tok <<ERROR>>-tok     yytext yyline yycolumn)
<<EOF>>                   (make-tok eof-tok           yytext yyline yycolumn)

;;; end of file
