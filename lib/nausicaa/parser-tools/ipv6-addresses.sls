;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: lexer and parser for IPv6 addresses
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
(library (nausicaa parser-tools ipv6-addresses)
  (export
    ;; conditions
    &ipv6-address-parser-error
    make-ipv6-address-parser-error-handler

    ;; lexer and parser utilities
    (rename (lexer.ipv6-address-lexer-table	ipv6-address-lexer-table))
    make-ipv6-address-lexer
    (rename (lex.string:	string:)
	    (lex.port:		port:)
	    (lex.procedure:	procedure:))

    (rename (parser.make-ipv6-address-parser	make-ipv6-address-parser))

    parse-ipv6-address-only
    parse-ipv6-address-prefix
    parse-ipv6-address

    ;; validation utilities
    ipv6-address-parsed-list-split
    ipv6-address-parsed-list-expand
    ipv6-address-parsed-list-validate-prefix)
  (import (nausicaa)
    (prefix (nausicaa parser-tools ip-addresses ipv6-address-lexer)  lexer.)
    (prefix (nausicaa parser-tools ip-addresses ipv6-address-parser) parser.)
    (prefix (vicare language-extensions makers) mk.)
    (prefix (vicare parser-tools silex lexer) lex.)
    (prefix (nausicaa parser-tools lexical-tokens)   lt.)
    (prefix (nausicaa parser-tools source-locations) sl.)
    (vicare unsafe operations))


;;;; conditions and error handlers

(define-condition-type &ipv6-address-parser-error
  (parent &condition))

(define make-ipv6-address-parser-error-handler
  (case-lambda
   ((who irritants)
    (make-ipv6-address-parser-error-handler who irritants make-error))
   ((who irritants condition-maker)
    (lambda ((message <string>) (token lt.<lexical-token>))
      (raise
       (condition (make-ipv6-address-parser-error-condition)
		  (condition-maker)
		  (make-who-condition who)
		  (make-message-condition (string-append "invalid IPv6 address input at column "
							 (token location column string) ": " message))
		  (make-irritants-condition (cons (token value) irritants))))))))


;;;; high-level lexer functions

(mk.define-maker make-ipv6-address-lexer
    %make-ipv6-address-lexer
  ;;These  auxiliary   syntaxes  are   the  ones  exported   by  (vicare
  ;;parser-tools silex lexer).
  ((lex.string:		sentinel	(mk.without lex.port:   lex.procedure:))
   (lex.port:		sentinel	(mk.without lex.string: lex.procedure:))
   (lex.procedure:	sentinel	(mk.without lex.port:   lex.string:))))

(define-syntax* (%make-ipv6-address-lexer stx)
  (syntax-case stx (sentinel)

    ((_ ?string sentinel sentinel)
     #'(lex.make-lexer lexer.ipv6-address-lexer-table
		       (lex.make-IS (lex.string: ?string) (lex.counters: 'all))))

    ((_ sentinel ?port sentinel)
     #'(lex.make-lexer lexer.ipv6-address-lexer-table
		       (lex.make-IS (lex.port: ?port) (lex.counters: 'all))))

    ((_ sentinel sentinel ?procedure)
     #'(lex.make-lexer lexer.ipv6-address-lexer-table
		       (lex.make-IS (lex.procedure: ?procedure) (lex.counters: 'all))))

    ((_ sentinel sentinel ?procedure)
     (synner "invalid or missing selection of input method"))
    ))


;;;; low-level and high-level parser functions

(module (parse-ipv6-address-only
	 parse-ipv6-address-prefix
	 parse-ipv6-address)

  (define (parse-ipv6-address the-string)
    (define who 'parse-ipv6-address)
    (let* ((lexer	(make-ipv6-address-lexer (lex.string: the-string)))
	   (parser	(%make-ipv6-address-parser who lexer the-string))
	   (ell		(parser)))
      (receive (addr-ell number-of-bits-in-prefix)
	  (ipv6-address-parsed-list-split ell)
	(receive-and-return (addr-ell^)
	    (ipv6-address-parsed-list-expand addr-ell)
	  (if addr-ell^
	      ($set-last-pair! addr-ell^ (list (list number-of-bits-in-prefix)))
	    (%raise-parser-error who the-string))))))

  (define (parse-ipv6-address-only the-string)
    (define who 'parse-ipv6-address-only)
    (let* ((lexer	(make-ipv6-address-lexer (lex.string: the-string)))
	   (parser	(%make-ipv6-address-parser who lexer the-string))
	   (ell		(parser)))
      (receive (addr-ell number-of-bits-in-prefix)
	  (ipv6-address-parsed-list-split ell)
	(when number-of-bits-in-prefix
	  (%raise-parser-error who the-string))
	(let ((addr-ell (ipv6-address-parsed-list-expand addr-ell)))
	  (or addr-ell (%raise-parser-error who the-string))))))

  (define (parse-ipv6-address-prefix the-string)
    (define who 'parse-ipv6-address-prefix)
    (let* ((lexer	(make-ipv6-address-lexer (lex.string: the-string)))
	   (parser	(%make-ipv6-address-parser who lexer the-string))
	   (ell		(parser)))
      (receive (addr-ell number-of-bits-in-prefix)
	  (ipv6-address-parsed-list-split ell)
	(unless number-of-bits-in-prefix
	  (%raise-parser-error who the-string))
	(let ((addr-ell (ipv6-address-parsed-list-expand addr-ell)))
	  (if addr-ell
	      (values addr-ell number-of-bits-in-prefix)
	    (%raise-parser-error who the-string))))))

;;; --------------------------------------------------------------------

  (define (%make-ipv6-address-parser who lexer the-string)
    (lambda ()
      ((parser.make-ipv6-address-parser)
       lexer
       (make-ipv6-address-parser-error-handler who (list the-string)))))

  (define (%raise-parser-error who the-string)
    (raise
     (condition (make-ipv6-address-parser-error-condition)
		(make-who-condition who)
		(make-message-condition "invalid IPv6 address or address-prefix string")
		(make-irritants-condition (list the-string)))))

  (define ($set-last-pair! ell pair)
    (if (pair? ($cdr ell))
	($set-last-pair! ($cdr ell) pair)
      ($set-cdr! ell pair)))

  #| end of module |# )


;;;; validation utilities

(define (ipv6-address-parsed-list-split ell)
  ;;Given a list returned by the  parser, return two values: the list of
  ;;address components, the number of bits in the prefix length or #f if
  ;;there is no prefix length.
  ;;
  (let ((rell (reverse ell)))
    (if (pair? (car rell))
	(values (reverse (cdr rell)) (caar rell))
      (values ell #f))))

(define (ipv6-address-parsed-list-expand ell)
  ;;Given a list returned by  the parser, with the prefix length element
  ;;stripped, expand the #f value if present.  Return the resulting list
  ;;or #f if the list is invalid.
  ;;
  (define (make-list len fill)
    (do ((i 0 (+ 1 i))
	 (l '() (cons fill l)))
	((= i len)
	 l)))
  (let ((len		(length ell))
	(present?	(memv #f ell)))
    (if present?
	(if (< 8 len)
	    #f
	  (let split ((ell ell) (flag #f) (pre '()) (post '()))
	    (if (null? ell)
		(let ((pre-len  (length pre))
		      (post-len (length post)))
		  (append (reverse pre) (make-list (- 8 pre-len post-len) 0) (reverse post)))
	      (cond (flag
		     (split (cdr ell) #t pre (cons (car ell) post)))
		    ((car ell)
		     (split (cdr ell) #f (cons (car ell) pre) post))
		    (else
		     (split (cdr ell) #t pre post))))))
      (if (= 8 len)
	  ell
	#f))))

(define (ipv6-address-parsed-list-validate-prefix number-of-bits-in-prefix ell)
  ;;Given  the  number of  bits  in the  prefix  and  the expanded  list
  ;;returned by the parser (of length  8): return true if all the unused
  ;;bits are set to zero; else return false.
  ;;
  (let loop ((ell ell) (n number-of-bits-in-prefix))
    (if (null? ell)
	#t
      (if (or (not (zero? n)) (zero? (car ell)))
	  (loop (cdr ell) (let ((n (- n 16)))
			    (if (positive? n) n 0)))
	#f))))


;;;; done

)

;;; end of file
