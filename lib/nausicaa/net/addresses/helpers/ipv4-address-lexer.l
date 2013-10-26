;;; -*- mode: scheme; coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: lexer for Ipv4 address textual representation
;;;Date: Fri Jun 11, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;; structural characters

decdig			[0-9]
;; hexdig		[0-9A-Fa-f]
;; octdig		[0-7]

decint5			25[0-5]
decint4			2[0-4]{decdig}
decint3			1{decdig}{decdig}
decint2			[1-9]{decdig}
decint1			{decdig}
decint			{decint1}|{decint2}|{decint3}|{decint4}|{decint5}

lenint4			3[0-2]
lenint3			2[0-9]
lenint2			1[0-9]
lenint1			[0-9]
lenint			{lenint1}|{lenint2}|{lenint3}|{lenint4}

;; hexint			0x{hexdig}{hexdig}?

;; octint			0{octdig}({octdig}({octdig})?)?

prefix-length		/{lenint}

DOT			\.

%%

{DOT}			(lt.<lexical-token> ((lt.category: 'DOT)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    #\.)
					     (lt.length:   1)))

{decint}		(lt.<lexical-token> ((lt.category: 'NUMBER)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    (string->number yytext))
					     (lt.length:   (string-length yytext))))

;; {hexint}		(lt.<lexical-token> ((lt.category: 'NUMBER)
;; 					 (lt.location: (sl.<source-location> ((sl.line:   yyline)
;; 									(sl.column: yycolumn)
;; 									(sl.offset: yyoffset))))
;; 					 (lt.value:    (string->number (substring yytext 2 (string-length yytext)) 16))
;; 					 (lt.length:   (string-length yytext))))

;; {octint}		(lt.<lexical-token> ((lt.cateory: 'NUMBER)
;; 					 (lt.location: (sl.<source-location> ((sl.line:   yyline)
;; 									(sl.column: yycolumn)
;; 									(sl.offset: yyoffset))))
;; 					 (lt.value:    (string->number (substring yytext 1 (string-length yytext)) 8))
;; 					 (lt.category: (string-length yytext))))

{prefix-length}		(lt.<lexical-token> ((lt.category: 'PREFIX-LENGTH)
					     (lt.location: (sl.<source-location> ((sl.line:   yyline)
										  (sl.column: yycolumn)
										  (sl.offset: yyoffset))))
					     (lt.value:    (string->number (substring yytext 1 (string-length yytext))))
					     (lt.length:   (string-length yytext))))

<<EOF>>			(silex-default-eof-handler)

<<ERROR>>		(silex-default-error-handler)

;;; end of file
