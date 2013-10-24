;;; -*- mode: scheme; coding: utf-8; page-delimiter: "^;;page" -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: SILex macros for  whitespace and comment in SILex tables
;;;Date: Tue Jan 18, 2011
;;;
;;;Abstract
;;;
;;;	This file  needs the macro file "unicode.macros.l"  to be loaded
;;;	first.
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

;;A whitespace character is one among:
;;
;;		\9	char tabulation
;;		\11	line tabulation
;;		\12	form feed
;;		\13	carriage return
;;		\133	next line
;;
;;and  any char  whose Unicode  category is  Zs, Zl  or Zp.   Notice the
;;absence of newline (\10), which is special for SILex.
;;
WHITESPACE-NO-NEWLINE	([\9\11\12\13\133]|{UNICODE-Zs}|{UNICODE-Zl}|{UNICODE-Zp})+

;;A simple comment is everything between a semicolon and a LINEENDING or
;;a paragraph separator (#x2029 = \8233).
;;
LINECOMMENT-NOEND	";"[^\10\13\133\8232\8233]*

;;Multiline comments can contain nested multiline comments, so we need a
;;sublexer.
;;
ONESTEDCOMMENT		"#|"

;;; end of file
