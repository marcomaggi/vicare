;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: IPv4 address object type
;;;Date: Fri Jun 11, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa net ipv4-addresses)
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
    parse-ipv4-address-prefix

    ;; classes
    <ipv4-address>			<ipv4-address-prefix>
    <ipv4-address-fixnum>		<list-of-ipv4-address-fixnums>)
  (import (nausicaa)
    (vicare unsafe operations)
    (prefix (nausicaa net helpers ipv4-address-lexer) lexer.)
    (prefix (nausicaa net helpers ipv4-address-parser) parser.)
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


;;;; auxiliary labels

(define-label <ipv4-address-fixnum>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (N)
	       ($fx<= N 255))))

(define-label <list-of-ipv4-address-fixnums>
  (parent <list>)
  (predicate (lambda ((obj <list>))
	       (and (= 4 (obj length))
		    (for-all (<ipv4-address-fixnum>) obj)))))


(define-class <ipv4-address>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address>)

  (protocol (lambda (make-top)
	      (lambda ((addr-ell <list-of-ipv4-address-fixnums>))
		(apply (make-top) #f #f addr-ell))))

  (fields (mutable cached-bignum)
	  (mutable cached-string)
	  (immutable (third	<ipv4-address-fixnum>))
	  (immutable (second	<ipv4-address-fixnum>))
	  (immutable (first	<ipv4-address-fixnum>))
	  (immutable (zeroth	<ipv4-address-fixnum>)))

  (virtual-fields
   bignum
   string
   private?
   loopback?
   localhost?
   link-local?
   reserved?
   test-net-1?
   six-to-four-relay-anycast?
   benchmark-tests?
   test-net-2?
   test-net-3?
   multicast?
   limited-broadcast?
   #| end of virtual-fields |# )

  #| end of class |# )

(define (<ipv4-address>-bignum (o <ipv4-address>))
  (or (o cached-bignum)
      (receive-and-return (bn)
	  (+ (o zeroth)
	     (bitwise-arithmetic-shift-left (o first)    8)
	     (bitwise-arithmetic-shift-left (o second)   16)
	     (bitwise-arithmetic-shift-left (o third)    24))
	(set! (o cached-bignum) bn))))

(define (<ipv4-address>-string (o <ipv4-address>))
  (receive-and-return (S)
      (string-append (number->string (o third))  "."
		     (number->string (o second)) "."
		     (number->string (o first))  "."
		     (number->string (o zeroth)))
    (set! (o cached-string) S)))

;;; --------------------------------------------------------------------

(define (<ipv4-address>-private? (o <ipv4-address>))
  (or (= 10 (o third))
      (and (= 172 (o third)) (= #b00010000 (bitwise-and #b11110000 (o second))))
      (and (= 192 (o third)) (= 168 (o second)))))

(define (<ipv4-address>-loopback? (o <ipv4-address>))
  (= 127 (o third)))

(define (<ipv4-address>-localhost? (o <ipv4-address>))
  (and (= 127 (o third))
       (=   0 (o second))
       (=   0 (o first))
       (=   1 (o zeroth))))

(define (<ipv4-address>-link-local? (o <ipv4-address>))
  (and (= 169 (o third))
       (= 254 (o second))))

(define (<ipv4-address>-reserved? (o <ipv4-address>))
  (or (and (= 192 (o third))
	   (=   0 (o second))
	   (=   0 (o first)))
      (= 240 (bitwise-and #b11110000 (o third)))))

(define (<ipv4-address>-test-net-1? (o <ipv4-address>))
  (and (= 192 (o third))
       (=   0 (o second))
       (=   2 (o first))))

(define (<ipv4-address>-six-to-four-relay-anycast? (o <ipv4-address>))
  (and (= 192 (o third))
       (=  88 (o second))
       (=  99 (o first))))

(define (<ipv4-address>-benchmark-tests? (o <ipv4-address>))
  (and (= 198 (o third))
       (=  18 (bitwise-and #b11111110 (o second)))))

(define (<ipv4-address>-test-net-2? (o <ipv4-address>))
  (and (= 198 (o third))
       (=  51 (o second))
       (= 100 (o first))))

(define (<ipv4-address>-test-net-3? (o <ipv4-address>))
  (and (= 203 (o third))
       (=   0 (o second))
       (= 113 (o first))))

(define (<ipv4-address>-multicast? (o <ipv4-address>))
  (= 224 (bitwise-and #b11110000 (o third))))

(define (<ipv4-address>-limited-broadcast? (o <ipv4-address>))
  (and (= 255 (o third))
       (= 255 (o second))
       (= 255 (o first))
       (= 255 (o zeroth))))


(define-class <ipv4-address-prefix>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address-prefix>)
  (parent <ipv4-address>)

  (protocol (lambda (make-address)
	      (lambda (addr-ell number-of-bits)
		((make-address addr-ell) number-of-bits #f))))

  (fields prefix-length
	  (mutable cached-string))

  (virtual-fields (immutable (string <string>)
			     (lambda ((o <ipv4-address-prefix>))
			       (or (o $cached-string)
				   (receive-and-return (S)
				       (string-append (slot-ref o string <ipv4-address>)
						      "/"
						      (number->string (o $prefix-length)))
				     (set! (o $cached-string) S))))))

  #| end of class |# )


;;;; done

)

;;; end of file
