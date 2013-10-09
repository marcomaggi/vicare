;;; -*- mode: scheme; coding: utf-8; page-delimiter: "^;;page" -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: SILex macros used by the string lexer
;;;Date: Tue Jan 18, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

DOUBLEQUOTE		\"

ESCAPED-A		\\a
ESCAPED-B		\\b
ESCAPED-T		\\t
ESCAPED-N		\\n
ESCAPED-V		\\v
ESCAPED-F		\\f
ESCAPED-R		\\r
ESCAPED-DOUBLEQUOTE	\\\"
ESCAPED-BACKSLASH	\\\\

;;<line ending> is:
;;
;;	\10		line feed
;;	\13		carriage return
;;	\133		next line
;;	\10\13		carriage return, line feed
;;	\13\133		carriage return, next line
;;	\8232		line separator
;;
LINEENDING		[\10\13\133\8232]|(\13\10)|(\13\133)

;;<intraline whitespace> is: char tabulation \9, any char whose Unicode
;;category is Zs.
;;
;;(http://www.fileformat.info/info/unicode/category/index.htm)
;;
INTRALINE-WHITESPACE	[\9#x0020#x00A0#x1680#x180E#x2000#x2001#x2002#x2003#x2004#x2005#x2006#x2007#x2008#x2009#x200A#x202F#x205F#x3000]

LINE-WRAP-TO-IGNORE	\\{INTRALINE-WHITESPACE}*{LINEENDING}{INTRALINE-WHITESPACE}*

DIGIT			[0-9]
HEXDIGIT		{DIGIT}|[a-fA-F]
HEX-SCALAR-VALUE	{HEXDIGIT}+
INLINE-HEX-ESCAPE	\\x{HEX-SCALAR-VALUE}\;

;;; end of file
