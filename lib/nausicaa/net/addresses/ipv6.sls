;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: IPv6 address object type
;;;Date: Wed Jun  9, 2010
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
(library (nausicaa net addresses ipv6)
  (export

    ;; conditions
    &ipv6-address-parser-error
    make-ipv6-address-parser-error-condition
    ipv6-address-parser-error-condition?

    make-ipv6-address-parser-error-handler

    ;; validation utilities
    ipv6-address-parsed-list-split
    ipv6-address-parsed-list-expand
    ipv6-address-parsed-list-validate-prefix

    ;; lexer and parser utilities
    make-ipv6-address-lexer
    make-ipv6-address-parser
    ipv6-address-parse
    ipv6-address-prefix-parse

    ;; class
    <ipv6-address>		<ipv6-address>?
    make-<ipv6-address>
    <ipv6-address>-zeroth
    <ipv6-address>-first
    <ipv6-address>-second
    <ipv6-address>-third
    <ipv6-address>-fourth
    <ipv6-address>-fifth
    <ipv6-address>-sixth
    <ipv6-address>-seventh

    <ipv6-address>-bignum
    <ipv6-address>-string
    <ipv6-address>-unspecified?
    <ipv6-address>-loopback?
    <ipv6-address>-multicast?
    <ipv6-address>-link-local-unicast?
    <ipv6-address>-global-unicast?

    <ipv6-address-prefix>	<ipv6-address-prefix>?
    <ipv6-address-prefix>-prefix-length
    <ipv6-address-prefix>-string
    )
  (import (nausicaa)
    (nausicaa parser-tools ip-addresses ipv6-address-lexer)
    (prefix (nausicaa parser-tools ip-addresses ipv6-address-parser) parser.)
    (prefix (vicare language-extensions makers) mk.)
    (prefix (vicare parser-tools silex lexer) lex.)
    (prefix (nausicaa parser-tools lexical-tokens)   lt.)
    (prefix (nausicaa parser-tools source-locations) sl.))


;;;; conditions and error handlers

(define-condition-type &ipv6-address-parser-error
  (parent &condition))

(define make-ipv6-address-parser-error-handler
  (case-lambda
   ((who irritants)
    (make-ipv6-address-parser-error-handler who irritants make-error))
   ((who irritants condition-maker)
    (lambda (message (token lt.<lexical-token>))
      (raise
       (condition (make-ipv6-address-parser-error-condition)
		  (condition-maker)
		  (make-who-condition who)
		  (make-message-condition (string-append "invalid IPv6 address input at column "
							 (token location column string) ": " message))
		  (make-irritants-condition (cons (token value) irritants))))))))


;;;; lexer and parser utilities

(mk.define-maker make-ipv6-address-lexer
    %make-ipv6-address-lexer
  ;;These  auxiliary  keywords  are   the  ones  exported  by  (nausicaa
  ;;parser-tools silex lexer).
  ((lex.string:		sentinel)
   (lex.port:		sentinel)
   (lex.procedure:	sentinel)))

(define-syntax* (%make-ipv6-address-lexer stx)
  (syntax-case stx (sentinel)

    ((_ ?string sentinel sentinel)
     #'(lex.make-lexer ipv6-address-lexer-table
		       (lex.make-IS (lex.string: ?string) (lex.counters: 'all))))

    ((_ sentinel ?port sentinel)
     #'(lex.make-lexer ipv6-address-lexer-table
		       (lex.make-IS (lex.port: ?port) (lex.counters: 'all))))

    ((_ sentinel sentinel ?procedure)
     #'(lex.make-lexer ipv6-address-lexer-table
		       (lex.make-IS (lex.procedure: ?procedure) (lex.counters: 'all))))

    ((_ sentinel sentinel ?procedure)
     (synner "invalid or missing selection of input method"))
    ))

(define (make-ipv6-address-parser who lexer irritants)
  (lambda ()
    ((parser.make-ipv6-address-parser) lexer
     (make-ipv6-address-parser-error-handler who irritants))))

(define (ipv6-address-parse the-string)
  (let* ((who		'ipv6-address-parse)
	 (irritants	(list the-string))
	 (%error	(lambda ()
			  (raise
			   (condition (make-ipv6-address-parser-error-condition)
				      (make-who-condition who)
				      (make-message-condition "invalid IPv6 address string")
				      (make-irritants-condition irritants))))))
    (let* ((lexer	(make-ipv6-address-lexer  (lex.string: the-string)))
	   (parser	(make-ipv6-address-parser who lexer irritants))
	   (ell		(parser)))
      (receive (addr-ell number-of-bits-in-prefix)
	  (ipv6-address-parsed-list-split ell)
	(when number-of-bits-in-prefix
	  (%error))
	(let ((addr-ell (ipv6-address-parsed-list-expand addr-ell)))
	  (or addr-ell (%error)))))))

(define (ipv6-address-prefix-parse the-string)
  (let* ((who		'ipv6-address-prefix-parse)
	 (irritants	(list the-string))
	 (%error	(lambda ()
			  (raise
			   (condition (make-ipv6-address-parser-error-condition)
				      (make-who-condition who)
				      (make-message-condition "invalid IPv6 address prefix string")
				      (make-irritants-condition irritants))))))
    (let* ((lexer	(make-ipv6-address-lexer  (lex.string: the-string)))
	   (parser	(make-ipv6-address-parser who lexer irritants))
	   (ell		(parser)))
      (receive (addr-ell number-of-bits-in-prefix)
	  (ipv6-address-parsed-list-split ell)
	(unless number-of-bits-in-prefix
	  (%error))
	(let ((addr-ell (ipv6-address-parsed-list-expand addr-ell)))
	  (unless addr-ell
	    (%error))
	  (values addr-ell number-of-bits-in-prefix))))))


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


(define-class <ipv6-address>
  (nongenerative nausicaa:net:ipv6-address:<ipv6-address>)

  (protocol (lambda (make-top)
	      (lambda (addr-ell)
		(apply (make-top) #f #f #f #f #f addr-ell))))

  (fields (mutable cached-bignum)
	  (mutable cached-string)
	  (mutable cached-unspecified?)
	  (mutable cached-loopback?)
	  (mutable cached-global-unicast?)
	  seventh  sixth   fifth  fourth
	  third    second  first  zeroth)

  (virtual-fields bignum string
		  unspecified?
		  loopback?
		  multicast?
		  link-local-unicast?
		  global-unicast?)

  #| end of class |# )

(define (<ipv6-address>-bignum (o <ipv6-address>))
  (or (o cached-bignum)
      (receive-and-return (bn)
	  (+ (o zeroth)
	     (bitwise-arithmetic-shift-left (o first)    16)
	     (bitwise-arithmetic-shift-left (o second)   32)
	     (bitwise-arithmetic-shift-left (o third)    48)
	     (bitwise-arithmetic-shift-left (o fourth)   64)
	     (bitwise-arithmetic-shift-left (o fifth)    80)
	     (bitwise-arithmetic-shift-left (o sixth)    96)
	     (bitwise-arithmetic-shift-left (o seventh) 112))
	(set! (o cached-bignum) bn))))

(define (<ipv6-address>-string (o <ipv6-address>))
  (or (o cached-string)
      (receive-and-return (S)
	  (string-append (number->string (o seventh)  16) ":"
			 (number->string (o sixth)    16) ":"
			 (number->string (o fifth)    16) ":"
			 (number->string (o fourth)   16) ":"
			 (number->string (o third)    16) ":"
			 (number->string (o second)   16) ":"
			 (number->string (o first)    16) ":"
			 (number->string (o zeroth)   16))
	(set! (o cached-string) S))))

;;; --------------------------------------------------------------------

(define (<ipv6-address>-unspecified? (o <ipv6-address>))
  (or (o cached-unspecified?)
      (receive-and-return (B)
	  (and (zero? (o zeroth))
	       (zero? (o first))
	       (zero? (o second))
	       (zero? (o third))
	       (zero? (o fourth))
	       (zero? (o fifth))
	       (zero? (o sixth))
	       (zero? (o seventh)))
	(set! (o cached-unspecified?) B))))

(define (<ipv6-address>-loopback? (o <ipv6-address>))
  (or (o cached-unspecified?)
      (receive-and-return (B)
	  (and (= 1 (o zeroth))
	       (zero? (o first))
	       (zero? (o second))
	       (zero? (o third))
	       (zero? (o fourth))
	       (zero? (o fifth))
	       (zero? (o sixth))
	       (zero? (o seventh)))
	(set! (o cached-unspecified?) B))))

(define (<ipv6-address>-multicast? (o <ipv6-address>))
;;;                        012345678
  (= #xFF00 (bitwise-and #b11111111100000000 (o seventh))))

(define (<ipv6-address>-link-local-unicast? (o <ipv6-address>))
;;;                        0123456789
  (= #xFE80 (bitwise-and #b11111111110000000 (o seventh))))

(define (<ipv6-address>-global-unicast? (o <ipv6-address>))
  (or (o cached-global-unicast?)
      (not (or (o unspecified?)
	       (o loopback?)
	       (o multicast?)
	       (o link-local-unicast?)))))


(define-class <ipv6-address-prefix>
  (nongenerative nausicaa:net:ipv6-address:<ipv6-address-prefix>)
  (parent <ipv6-address>)

  (protocol (lambda (make-address)
	      (lambda (addr-ell number-of-bits)
		((make-address addr-ell) number-of-bits #f))))

  (fields prefix-length
	  (mutable cached-string))

  (virtual-fields (immutable string
			     (lambda ((o <ipv6-address-prefix>))
			       (or (o $cached-string)
				   (receive-and-return (S)
				       (string-append (slot-ref o string <ipv6-address>)
						      "/"
						      (number->string (o prefix-length)))
				     (set! (o $cached-string) S))))))

  #| end of class |# )


;;;; done

)

;;; end of file
