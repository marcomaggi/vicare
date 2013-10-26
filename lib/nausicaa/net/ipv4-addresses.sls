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
    <ipv4-address-fixnum>		<list-of-ipv4-address-fixnums>
    <ipv4-address-prefix-length>)
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

(define-label <ipv4-address-prefix-length>
  (parent <nonnegative-fixnum>)
  (predicate (lambda (N)
	       ($fx<= N 32))))


(define-class <ipv4-address>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address>)

  (protocol
   (lambda (make-top)
     (define (%make-address third second first zeroth)
       ((make-top) #f #f third second first zeroth))
     (case-lambda
      (((third <ipv4-address-fixnum>) (second <ipv4-address-fixnum>)
	(first <ipv4-address-fixnum>) (zeroth <ipv4-address-fixnum>))
       (%make-address third second first zeroth))
      (((addr-ell <list-of-ipv4-address-fixnums>))
       (apply %make-address addr-ell))
      )))

  (fields (mutable memoized-bignum-rep)
	  (mutable memoized-string-rep)
	  (immutable (third	<ipv4-address-fixnum>))
	  (immutable (second	<ipv4-address-fixnum>))
	  (immutable (first	<ipv4-address-fixnum>))
	  (immutable (zeroth	<ipv4-address-fixnum>)))

  (virtual-fields
   (immutable (bignum <exact-integer>)
	      (lambda ((o <ipv4-address>))
		(or (o $memoized-bignum-rep)
		    (receive-and-return (bn)
			(+ (o $zeroth)
			   (fxarithmetic-shift-left (o $first)   8)
			   (fxarithmetic-shift-left (o $second) 16)
			   (fxarithmetic-shift-left (o $third)  24))
		      (set! (o $memoized-bignum-rep) bn)))))

   (immutable (string <string>)
	      (lambda ((o <ipv4-address>))
		(or (o $memoized-string-rep)
		    (receive-and-return (S)
			(string-append (o $third  $string) "."
				       (o $second $string) "."
				       (o $first  $string) "."
				       (o $zeroth $string))
		      (set! (o $memoized-string-rep) S)))))

;;;

   (immutable (private?		<boolean>)
	      (lambda ((O <ipv4-address>))
		(or ($fx= 10 (O $third))
		    (and ($fx= 172 (O $third)) ($fx= #b00010000 ($fxlogand #b11110000 (O $second))))
		    (and ($fx= 192 (O $third)) ($fx= 168 (O $second))))))

   (immutable (loopback?	<boolean>)
	      (lambda ((O <ipv4-address>))
		($fx= 127 (O $third))))

   (immutable (localhost?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 127 (O $third))
		     ($fx=   0 (O $second))
		     ($fx=   0 (O $first))
		     ($fx=   1 (O $zeroth)))))

   (immutable (link-local?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 169 (O $third))
		     ($fx= 254 (O $second)))))

   (immutable (reserved?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(or (and ($fx= 192 (O $third))
			 ($fx=   0 (O $second))
			 ($fx=   0 (O $first)))
		    ($fx= 240 ($fxlogand #b11110000 (O $third))))))

   (immutable (test-net-1?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 192 (O $third))
		     ($fx=   0 (O $second))
		     ($fx=   2 (O $first)))))

   (immutable (six-to-four-relay-anycast?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 192 (O $third))
		     ($fx=  88 (O $second))
		     ($fx=  99 (O $first)))))

   (immutable (benchmark-tests?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 198 (O $third))
		     ($fx=  18 ($fxlogand #b11111110 (O $second))))))

   (immutable (test-net-2?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 198 (O $third))
		     ($fx=  51 (O $second))
		     ($fx= 100 (O $first)))))

   (immutable (test-net-3?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 203 (O $third))
		     ($fx=   0 (O $second))
		     ($fx= 113 (O $first)))))

   (immutable (multicast?	<boolean>)
	      (lambda ((O <ipv4-address>))
		($fx= 224 ($fxlogand #b11110000 (O $third)))))

   (immutable (limited-broadcast?	<boolean>)
	      (lambda ((O <ipv4-address>))
		(and ($fx= 255 (O $third))
		     ($fx= 255 (O $second))
		     ($fx= 255 (O $first))
		     ($fx= 255 (O $zeroth)))))

   #| end of virtual-fields |# )

  #| end of class |# )


(define-class <ipv4-address-prefix>
  (nongenerative nausicaa:net:ipv4-address:<ipv4-address-prefix>)
  (parent <ipv4-address>)

  (protocol (lambda (make-address)
	      (lambda ((addr-ell <list-of-ipv4-address-fixnums>)
		  (number-of-bits <ipv4-address-prefix-length>))
		((make-address addr-ell) number-of-bits #f))))

  (fields (immutable (prefix-length <ipv4-address-prefix-length>))
	  (mutable   memoized-string-rep))

  (virtual-fields (immutable (string <string>)
			     (lambda ((o <ipv4-address-prefix>))
			       (or (o $memoized-string-rep)
				   (receive-and-return (S)
				       (string-append (slot-ref o string <ipv4-address>)
						      "/"
						      (o $prefix-length $string))
				     (set! (o $memoized-string-rep) S))))))

  #| end of class |# )


;;;; done

)

;;; end of file
