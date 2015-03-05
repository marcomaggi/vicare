;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: byte as ASCII characters, syntaxes
;;;Date: Sat Nov 16, 2013
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


#!r6rs
(library (vicare language-extensions ascii-chars syntaxes)
  (export
    $fixnum-in-ascii-range?
    $fixnum-in-base10-range?
    $fixnum-in-base16-range?

    $ascii-cased?
    $ascii-upper-case?
    $ascii-lower-case?
    (rename ($ascii-upper-case?	$ascii-title-case?)
	    ($ascii-cased?	$ascii-alphabetic?))

    $ascii-upcase
    $ascii-downcase
    (rename ($ascii-upcase	$ascii-titlecase))

    $ascii-dec-digit?
    $ascii-hex-digit?
    $ascii-alpha-digit?

    $ascii-dec->fixnum
    $ascii-hex->fixnum
    $fixnum->ascii-dec
    $fixnum->ascii-hex

    $ascii-chi-V?
    $ascii-chi-ampersand?
    $ascii-chi-at-sign?
    $ascii-chi-bang?
    $ascii-chi-close-bracket?
    $ascii-chi-close-paren?
    $ascii-chi-colon?
    $ascii-chi-comma?
    $ascii-chi-dollar?
    $ascii-chi-dot?
    $ascii-chi-dash?
    $ascii-chi-equal?
    $ascii-chi-minus?
    $ascii-chi-number-sign?
    $ascii-chi-open-bracket?
    $ascii-chi-open-paren?
    $ascii-chi-percent?
    $ascii-chi-plus?
    $ascii-chi-question-mark?
    $ascii-chi-quote?
    $ascii-chi-semicolon?
    $ascii-chi-slash?
    $ascii-chi-star?
    $ascii-chi-v?

    $ascii-uri-gen-delim?
    $ascii-uri-sub-delim?
    $ascii-uri-reserved?
    $ascii-uri-unreserved?
    $ascii-uri-pct-encoded?
    $ascii-uri-pchar?
    $ascii-uri-pchar-not-percent-encoded?)
  (import (vicare)
    (vicare arguments validation)
    (vicare unsafe operations))


;;;; constants

(define-inline-constant FIXNUM-0		(char->integer #\0))
(define-inline-constant FIXNUM-9		(char->integer #\9))
(define-inline-constant FIXNUM-A		(char->integer #\A))
(define-inline-constant FIXNUM-F		(char->integer #\F))
(define-inline-constant FIXNUM-V		(char->integer #\V))
(define-inline-constant FIXNUM-Z		(char->integer #\Z))
(define-inline-constant FIXNUM-a		(char->integer #\a))
(define-inline-constant FIXNUM-f		(char->integer #\f))
(define-inline-constant FIXNUM-v		(char->integer #\v))
(define-inline-constant FIXNUM-z		(char->integer #\z))

;;; --------------------------------------------------------------------

(define-inline-constant FIXNUM-PERCENT		(char->integer #\%))
(define-inline-constant FIXNUM-MINUS		(char->integer #\-))

;;In the URI RFC these are "gen-delims".
(define-inline-constant FIXNUM-COLON		(char->integer #\:))
(define-inline-constant FIXNUM-SLASH		(char->integer #\/))
(define-inline-constant FIXNUM-QUESTION-MARK	(char->integer #\?))
(define-inline-constant FIXNUM-NUMBER-SIGN	(char->integer #\#))
(define-inline-constant FIXNUM-OPEN-BRACKET	(char->integer #\[))
(define-inline-constant FIXNUM-CLOSE-BRACKET	(char->integer #\]))
(define-inline-constant FIXNUM-AT-SIGN		(char->integer #\@))

;;In the URI RFC these are "sub-delims".
(define-inline-constant FIXNUM-BANG		(char->integer #\!))
(define-inline-constant FIXNUM-DOLLAR		(char->integer #\$))
(define-inline-constant FIXNUM-AMPERSAND	(char->integer #\&))
(define-inline-constant FIXNUM-QUOTE		(char->integer #\'))
(define-inline-constant FIXNUM-OPEN-PAREN	(char->integer #\())
(define-inline-constant FIXNUM-CLOSE-PAREN	(char->integer #\)))
(define-inline-constant FIXNUM-STAR		(char->integer #\*))
(define-inline-constant FIXNUM-PLUS		(char->integer #\+))
(define-inline-constant FIXNUM-COMMA		(char->integer #\,))
(define-inline-constant FIXNUM-SEMICOLON	(char->integer #\;))
(define-inline-constant FIXNUM-EQUAL		(char->integer #\=))

;;In the URI RFC these are "unreserved".
(define-inline-constant FIXNUM-DASH		(char->integer #\-))
(define-inline-constant FIXNUM-DOT		(char->integer #\.))
(define-inline-constant FIXNUM-UNDERSCORE	(char->integer #\_))
(define-inline-constant FIXNUM-TILDE		(char->integer #\~))


;;;; generic utilities for ASCII encoded characters

(define-inline ($fixnum-in-ascii-range? obj)
  ($fx<= 0 obj 127))

;;; --------------------------------------------------------------------

(define-inline ($fixnum-in-base10-range? obj)
  (fx<=? 0 obj 9))

;;; --------------------------------------------------------------------

(define-inline ($fixnum-in-base16-range? obj)
  (fx<=? 0 obj 15))

;;; --------------------------------------------------------------------

(define-inline ($ascii-upper-case? fx)
  ($fx<= FIXNUM-A fx FIXNUM-Z))

;;; --------------------------------------------------------------------

(define-inline ($ascii-lower-case? fx)
  ($fx<= FIXNUM-a fx FIXNUM-z))

;;; --------------------------------------------------------------------

(define-inline ($ascii-cased? fx)
  (or ($ascii-upper-case? fx)
      ($ascii-lower-case? fx)))

;;; --------------------------------------------------------------------

(define-inline ($ascii-dec-digit? fx)
  ($fx<= FIXNUM-0 fx FIXNUM-9))

;;; --------------------------------------------------------------------

(define-inline ($ascii-hex-digit? chi)
  (or ($ascii-dec-digit? chi)
      ($fx<= FIXNUM-a chi FIXNUM-f)
      ($fx<= FIXNUM-A chi FIXNUM-F)))

;;; --------------------------------------------------------------------

(define-inline ($ascii-upcase fx)
  (if ($ascii-lower-case? fx)
      ($fx- fx 32)
    fx))

;;; --------------------------------------------------------------------

(define-inline ($ascii-downcase fx)
  (if ($ascii-upper-case? fx)
      ($fx+ 32 fx)
    fx))

;;; --------------------------------------------------------------------

(define-inline ($ascii-alpha-digit? fx)
  (or ($ascii-cased? fx)
      ($ascii-dec-digit? fx)))


;;;; conversion

(define-inline ($ascii-dec->fixnum chi)
  ($fx- chi FIXNUM-0))

;;; --------------------------------------------------------------------

(define-inline ($fixnum->ascii-dec n)
  ($fx+ FIXNUM-0 n))

;;; --------------------------------------------------------------------

(define-inline ($ascii-hex->fixnum chi)
  ;;This must be used only after "$ascii-hex-digit?" has validated CHI.
  ;;
  (cond (($fx<= FIXNUM-0 chi FIXNUM-9)
	 ($fx- chi FIXNUM-0))
	(($fx<= FIXNUM-a chi FIXNUM-f)
	 ($fx+ 10 ($fx- chi FIXNUM-a)))
	(else
	 #;(assert (<= FIXNUM-A chi FIXNUM-F))
	 ($fx+ 10 ($fx- chi FIXNUM-A)))))

;;; --------------------------------------------------------------------

(define-inline ($fixnum->ascii-hex n)
  (if ($fx<= 0 n 9)
      ($fx+ FIXNUM-0 n)
    ($fx+ FIXNUM-A ($fx- n 10))))


;;;; char integer predicates

(let-syntax ((define-chi-predicate (syntax-rules ()
				     ((_ ?who ?const)
				      (define-inline (?who chi)
					($fx= chi ?const))))))
  ;;lexicographically sorted, please
  (define-chi-predicate $ascii-chi-V?			FIXNUM-V)
  (define-chi-predicate $ascii-chi-ampersand?		FIXNUM-AMPERSAND)
  (define-chi-predicate $ascii-chi-at-sign?		FIXNUM-AT-SIGN)
  (define-chi-predicate $ascii-chi-bang?		FIXNUM-BANG)
  (define-chi-predicate $ascii-chi-close-bracket?	FIXNUM-CLOSE-BRACKET)
  (define-chi-predicate $ascii-chi-close-paren?		FIXNUM-CLOSE-PAREN)
  (define-chi-predicate $ascii-chi-colon?		FIXNUM-COLON)
  (define-chi-predicate $ascii-chi-comma?		FIXNUM-COMMA)
  (define-chi-predicate $ascii-chi-dash?		FIXNUM-DASH)
  (define-chi-predicate $ascii-chi-dollar?		FIXNUM-DOLLAR)
  (define-chi-predicate $ascii-chi-dot?			FIXNUM-DOT)
  (define-chi-predicate $ascii-chi-equal?		FIXNUM-EQUAL)
  (define-chi-predicate $ascii-chi-minus?		FIXNUM-MINUS)
  (define-chi-predicate $ascii-chi-number-sign?		FIXNUM-NUMBER-SIGN)
  (define-chi-predicate $ascii-chi-open-bracket?	FIXNUM-OPEN-BRACKET)
  (define-chi-predicate $ascii-chi-open-paren?		FIXNUM-OPEN-PAREN)
  (define-chi-predicate $ascii-chi-percent?		FIXNUM-PERCENT)
  (define-chi-predicate $ascii-chi-plus?		FIXNUM-PLUS)
  (define-chi-predicate $ascii-chi-question-mark?	FIXNUM-QUESTION-MARK)
  (define-chi-predicate $ascii-chi-quote?		FIXNUM-QUOTE)
  (define-chi-predicate $ascii-chi-semicolon?		FIXNUM-SEMICOLON)
  (define-chi-predicate $ascii-chi-slash?		FIXNUM-SLASH)
  (define-chi-predicate $ascii-chi-star?		FIXNUM-STAR)
  (define-chi-predicate $ascii-chi-tilde?		FIXNUM-TILDE)
  (define-chi-predicate $ascii-chi-underscore?		FIXNUM-UNDERSCORE)
  (define-chi-predicate $ascii-chi-v?			FIXNUM-v)
  #| end of let-syntax |# )


;;;; character predicates related to RFC 3986, Uniform Resource Identifiers

(define-inline ($ascii-uri-gen-delim? chi)
  (or ($ascii-chi-colon?		chi)
      ($ascii-chi-slash?		chi)
      ($ascii-chi-question-mark?	chi)
      ($ascii-chi-number-sign?		chi)
      ($ascii-chi-open-bracket?		chi)
      ($ascii-chi-close-bracket?	chi)
      ($ascii-chi-at-sign?		chi)))

(define-inline ($ascii-uri-sub-delim? chi)
  (or ($ascii-chi-bang?			chi)
      ($ascii-chi-dollar?		chi)
      ($ascii-chi-ampersand?		chi)
      ($ascii-chi-quote?		chi)
      ($ascii-chi-open-paren?		chi)
      ($ascii-chi-close-paren?		chi)
      ($ascii-chi-star?			chi)
      ($ascii-chi-plus?			chi)
      ($ascii-chi-comma?		chi)
      ($ascii-chi-semicolon?		chi)
      ($ascii-chi-equal?		chi)))

(define-inline ($ascii-uri-reserved? chi)
  (or ($ascii-gen-delim? chi)
      ($ascii-uri-sub-delim? chi)))

(define-inline ($ascii-uri-unreserved? chi)
  (or ($ascii-alpha-digit?		chi)
      ($ascii-chi-dash?			chi)
      ($ascii-chi-dot?			chi)
      ($ascii-chi-underscore?		chi)
      ($ascii-chi-tilde?		chi)))

(define-syntax ($ascii-uri-pct-encoded? stx)
  (syntax-case stx ()
    ;;CHI must  be a  fixnum representing  the octet at  index I  in the
    ;;bytevector BV.   Check that the  3 bytes  at offset I  represent a
    ;;percent-encoded sequence  and increment  I to reference  the third
    ;;octet.
    ;;
    ((_ ?chi ?bv ?i)
     (and (identifier? #'?bv)
	  (identifier? #'?i))
     #'(and ($ascii-chi-percent? ?chi)
	    ;;There must  be at  least 2 more  octets after  the percent
	    ;;character.
	    (< (+ 2 ?i) ($bytevector-length ?bv))
	    (begin
	      ($fxincr! ?i)
	      (and ($ascii-hex-digit? ($bytevector-u8-ref ?bv ?i))
		   (begin
		     ($fxincr! ?i)
		     ($ascii-hex-digit? ($bytevector-u8-ref ?bv ?i)))))))
    ))

;;; --------------------------------------------------------------------

(define-inline ($ascii-uri-pchar-not-percent-encoded? chi)
  ;;Evaluate  to true  if CHI  matches  the "pchar"  component with  the
  ;;exception of the percent-encoded sequence.
  ;;
  (or ($ascii-uri-unreserved? chi)
      ($ascii-uri-sub-delim? chi)
      ($ascii-chi-colon? chi)
      ($ascii-chi-at-sign? chi)))

(define-syntax ($ascii-uri-pchar? stx)
  (syntax-case stx ()
    ;;CHI must  be a  fixnum representing  the octet at  index I  in the
    ;;bytevector  BV.   Evaluate to  true  if  CHI matches  the  "pchar"
    ;;component.
    ;;
    ((_ ?chi ?bv ?i)
     (and (identifier? #'?bv)
	  (identifier? #'?i))
     #'(or ($ascii-uri-pchar-not-percent-encoded? ?chi)
	   ($ascii-uri-pct-encoded? ?chi ?bv ?i)))))


;;;; done

)

;;; end of file
