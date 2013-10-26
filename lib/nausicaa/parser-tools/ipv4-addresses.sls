;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: lexer and parser for IPv4 addresses
;;;Date: Sat Oct 26, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa parser-tools ipv4-addresses)
  (export
    ;; conditions
    &ipv4-address-parser-error
    make-ipv4-address-parser-error-handler

    ;; lexer and parser utilities
    (rename (lexer.ipv4-address-lexer-table	ipv4-address-lexer-table))
    make-ipv4-address-lexer
    (rename (lex.string:	string:)
	    (lex.port:		port:)
	    (lex.procedure:	procedure:))

    (rename (parser.make-ipv4-address-parser	make-ipv4-address-parser))
    parse-ipv4-address
    parse-ipv4-address-only
    parse-ipv4-address-prefix)
  (import (nausicaa)
    (prefix (nausicaa parser-tools ip-addresses ipv4-address-lexer)  lexer.)
    (prefix (nausicaa parser-tools ip-addresses ipv4-address-parser) parser.)
    (prefix (vicare language-extensions makers) mk.)
    (prefix (vicare parser-tools silex lexer) lex.)
    (prefix (nausicaa parser-tools lexical-tokens) lt.)
    (prefix (nausicaa parser-tools source-locations) sl.))


;;;; conditions and error handlers

(define-condition-type &ipv4-address-parser-error
  (parent &condition))

(define make-ipv4-address-parser-error-handler
  (case-lambda
   ((who irritants)
    (make-ipv4-address-parser-error-handler who irritants make-error))
   ((who irritants condition-maker)
    (lambda (message (token lt.<lexical-token>))
      (raise
       (condition (make-ipv4-address-parser-error-condition)
		  (condition-maker)
		  (make-who-condition who)
		  (make-message-condition (let (((pos sl.<source-location>) (token location)))
					    (string-append "invalid Ipv4 address input at column "
							   (pos column string) ": " message)))
		  (make-irritants-condition (cons (token value) irritants))))))))


;;;; high-level lexer functions

(mk.define-maker make-ipv4-address-lexer
    %make-ipv4-address-lexer
  ;;These  auxiliary   syntaxes  are   the  ones  exported   by  (vicare
  ;;parser-tools silex lexer).
  ((lex.string:		sentinel	(mk.without lex.port:   lex.procedure:))
   (lex.port:		sentinel	(mk.without lex.string: lex.procedure:))
   (lex.procedure:	sentinel	(mk.without lex.port:   lex.string:))))

(define-syntax* (%make-ipv4-address-lexer stx)
  (syntax-case stx (sentinel)

    ((_ ?string sentinel sentinel)
     #'(lex.make-lexer lexer.ipv4-address-lexer-table
		       (lex.make-IS (lex.string: ?string) (lex.counters: 'all))))

    ((_ sentinel ?port sentinel)
     #'(lex.make-lexer lexer.ipv4-address-lexer-table
		       (lex.make-IS (lex.port: ?port) (lex.counters: 'all))))

    ((_ sentinel sentinel ?procedure)
     #'(lex.make-lexer lexer.ipv4-address-lexer-table
		       (lex.make-IS (lex.procedure: ?procedure) (lex.counters: 'all))))

    ((_ sentinel sentinel ?procedure)
     (synner "invalid or missing selection of input method"))
    ))


;;;; low-level and high-level parser functions

(module (parse-ipv4-address-only
	 parse-ipv4-address-prefix
	 parse-ipv4-address)

  (define (parse-ipv4-address-only the-string)
    (define who 'parse-ipv4-address-only)
    (define irritants (list the-string))
    (let* ((lexer	(make-ipv4-address-lexer (lex.string: the-string)))
	   (parser	(%make-ipv4-address-parser who lexer the-string))
	   (ell		(parser)))
      (if (= 4 (length ell))
	  ell
	(%raise-parser-error who the-string))))

  (define (parse-ipv4-address-prefix the-string)
    (define who 'parse-ipv4-address-prefix)
    (let* ((lexer	(make-ipv4-address-lexer (lex.string: the-string)))
	   (parser	(%make-ipv4-address-parser who lexer the-string))
	   (ell		(parser)))
      (if (= 5 (length ell))
	  (let ((rell (reverse ell)))
	    (values (reverse (cdr rell)) (caar rell)))
	(%raise-parser-error who the-string))))

  (define (parse-ipv4-address the-string)
    (define who 'parse-ipv4-address)
    (let* ((lexer	(make-ipv4-address-lexer (lex.string: the-string)))
	   (parser	(%make-ipv4-address-parser who lexer the-string)))
      (parser)))

;;; --------------------------------------------------------------------

  (define (%make-ipv4-address-parser who lexer the-string)
    (lambda ()
      ((parser.make-ipv4-address-parser)
       lexer
       (make-ipv4-address-parser-error-handler who (list the-string)))))

  (define (%raise-parser-error who the-string)
    (raise
     (condition (make-ipv4-address-parser-error-condition)
		(make-who-condition who)
		(make-message-condition "invalid IPv4 address string")
		(make-irritants-condition (list the-string)))))

  #| end of module |# )


;;;; done

)

;;; end of file
