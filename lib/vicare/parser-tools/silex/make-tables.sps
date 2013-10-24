;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: rebuild internal tables for SILex
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  wilnl be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (vicare)
  (prefix (vicare parser-tools silex) lex.))

(define (%log file-name)
  (display (string-append "generating " file-name "\n")
	   (current-error-port)))

(define silex-srcdir
  (string-append (getenv "SILEX_SRCDIR") "/"))

(%log (string-append silex-srcdir "action-l.sls"))
(lex.lex (lex.input-file:	(string-append silex-srcdir "action.l"))
	 (lex.output-file:	(string-append silex-srcdir "action-l.sls"))
	 (lex.counters:		'all)
	 (lex.library-spec:	"(vicare parser-tools silex action-l)")
	 (lex.library-imports:	'((vicare parser-tools silex semantic)))
	 (lex.table-name:	'action-tables)
	 (lex.lexer-format:	'code))

(%log (string-append silex-srcdir "class-l.sls"))
(lex.lex (lex.input-file:	(string-append silex-srcdir "class.l"))
	 (lex.output-file:	(string-append silex-srcdir "class-l.sls"))
	 (lex.counters:		'all)
	 (lex.library-spec:	"(vicare parser-tools silex class-l)")
	 (lex.library-imports:	'((vicare parser-tools silex semantic)))
	 (lex.table-name:	'class-tables)
	 (lex.lexer-format:	'code))

(%log (string-append silex-srcdir "macro-l.sls"))
(lex.lex (lex.input-file:	(string-append silex-srcdir "macro.l"))
	 (lex.output-file:	(string-append silex-srcdir "macro-l.sls"))
	 (lex.counters:		'all)
	 (lex.library-spec:	"(vicare parser-tools silex macro-l)")
	 (lex.library-imports:	'((vicare parser-tools silex semantic)))
	 (lex.table-name:	'macro-tables)
	 (lex.lexer-format:	'code))

(%log (string-append silex-srcdir "regexp-l.sls"))
(lex.lex (lex.input-file:	(string-append silex-srcdir "regexp.l"))
	 (lex.output-file:	(string-append silex-srcdir "regexp-l.sls"))
	 (lex.counters:		'all)
	 (lex.library-spec:	"(vicare parser-tools silex regexp-l)")
	 (lex.library-imports:	'((vicare parser-tools silex semantic)))
	 (lex.table-name:	'regexp-tables)
	 (lex.lexer-format:	'code))

(%log (string-append silex-srcdir "string-l.sls"))
(lex.lex (lex.input-file:	(string-append silex-srcdir "string.l"))
	 (lex.output-file:	(string-append silex-srcdir "string-l.sls"))
	 (lex.counters:		'all)
	 (lex.library-spec:	"(vicare parser-tools silex string-l)")
	 (lex.library-imports:	'((vicare parser-tools silex semantic)))
	 (lex.table-name:	'string-tables)
	 (lex.lexer-format:	'code))

(%log (string-append silex-srcdir "nested-comment-l.sls"))
(lex.lex (lex.input-file:	(string-append silex-srcdir "nested-comment.l"))
	 (lex.output-file:	(string-append silex-srcdir "nested-comment-l.sls"))
	 (lex.counters:		'all)
	 (lex.library-spec:	"(vicare parser-tools silex nested-comment-l)")
	 (lex.library-imports:	'((vicare parser-tools silex semantic)))
	 (lex.table-name:	'nested-comment-tables)
	 (lex.lexer-format:	'code))

;;; end of file
