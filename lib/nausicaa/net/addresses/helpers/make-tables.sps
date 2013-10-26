;;;
;;;Part of: Vicare Scheme
;;;Contents: rebuild the lexer and parser tables for net libraries
;;;Date: Wed Jun  9, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (prefix (vicare parser-tools silex) lex.)
  (prefix (nausicaa parser-tools lalr)  lalr.))

(define (%log file-name)
  (display (string-append "generating " (%prefix file-name) "\n")
	   (current-error-port)))

(define (%prefix file-name)
  (string-append srcdir "/" file-name))

(define-constant srcdir
  (getenv "NAUNET_SRCDIR"))


(%log "ipv6-address-lexer.sls")
(lex.lex (lex.input-file:	(%prefix "ipv6-address-lexer.l"))
	 (lex.output-file:	(%prefix "ipv6-address-lexer.sls"))
	 (lex.library-spec:	'(nausicaa net addresses helpers ipv6-address-lexer))
	 (lex.library-language:	'(nausicaa))
	 (lex.library-imports:	'((nausicaa parser-tools silex default-error-handler)
				  (prefix (nausicaa parser-tools lexical-tokens) lt.)
				  (prefix (nausicaa parser-tools source-locations) sl.)))
	 (lex.table-name:	'ipv6-address-lexer-table)
	 (lex.counters:		'all))

(%log "ipv4-address-lexer.sls")
(lex.lex (lex.input-file:	(%prefix "ipv4-address-lexer.l"))
	 (lex.output-file:	(%prefix "ipv4-address-lexer.sls"))
	 (lex.library-spec:	'(nausicaa net addresses helpers ipv4-address-lexer))
	 (lex.library-language:	'(nausicaa))
	 (lex.library-imports:	'((nausicaa parser-tools silex default-error-handler)
				  (prefix (nausicaa parser-tools lexical-tokens) lt.)
				  (prefix (nausicaa parser-tools source-locations) sl.)))
	 (lex.table-name:	'ipv4-address-lexer-table)
	 (lex.counters:		'all))


(%log "ipv4-address-parser.sls")
(lalr.lalr-parser

 (lalr.output-file:		(%prefix "ipv4-address-parser.sls"))
 (lalr.parser-name:		'make-ipv4-address-parser)
 (lalr.library-spec:		'(nausicaa net addresses helpers ipv4-address-parser))

 (lalr.terminals:		'(DOT NUMBER PREFIX-LENGTH))

 (lalr.rules:
  '((ipv4-address
     (NUMBER DOT NUMBER DOT NUMBER DOT NUMBER)
		: (list $1 $3 $5 $7)
     (NUMBER DOT NUMBER DOT NUMBER DOT NUMBER PREFIX-LENGTH)
		: (list $1 $3 $5 $7 (list $8)))
    )))


(%log "ipv6-address-parser.sls")
(lalr.lalr-parser

 (lalr.output-file:		(%prefix "ipv6-address-parser.sls"))
 (lalr.parser-name:		'make-ipv6-address-parser)
 (lalr.library-spec:		'(nausicaa net addresses helpers ipv6-address-parser))

 (lalr.terminals:		'(COLON DOT SLASH NUMBER))

 (lalr.rules:
  '((ipv6-address
     (NUMBER double-colon-tail)		: (cons (string->number $1 16) $2)
     (NUMBER colon-tail)		: (cons (string->number $1 16) $2)
     (double-colon-tail)		: $1)

    (colon-tail
     (COLON ipv4-address)		: $2
     (COLON NUMBER colon-tail)		: (cons (string->number $2 16) $3)
     (COLON NUMBER double-colon-tail)	: (cons (string->number $2 16) $3)
     (prefix-length)			: $1
     ()					: '())

    (double-colon-tail
     (COLON COLON after-double-colon)	: (cons #f $3))

    (after-double-colon
     (NUMBER no-double-colon-tail)	: (cons (string->number $1 16) $2)
     (ipv4-address)			: $1
     (prefix-length)			: $1
     ()					: '())

    (no-double-colon-tail
     (COLON NUMBER no-double-colon-tail): (cons (string->number $2 16) $3)
     (COLON ipv4-address)		: $2
     (prefix-length)			: $1
     ()					: '())

    (prefix-length
     (SLASH NUMBER)			: `((,(string->number $2))))

    (ipv4-address
     (dot-couple DOT dot-couple ipv4-address-tail)
					: (cons* $1 $3 $4))

    (ipv4-address-tail
     (prefix-length)			: $1
     ()					: '())

    (dot-couple
     (NUMBER DOT NUMBER)		: (+ (bitwise-arithmetic-shift-left (string->number $1 10) 8)
					     (string->number $3 10)))
    )))

;;; end of file
