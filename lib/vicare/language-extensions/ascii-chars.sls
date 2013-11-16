;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: byte as ASCII characters
;;;Date: Tue Jun 29, 2010
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
(library (vicare language-extensions ascii-chars)
  (export
    fixnum-in-ascii-range?	$fixnum-in-ascii-range?
    fixnum-in-base10-range?	$fixnum-in-base10-range?
    fixnum-in-base16-range?	$fixnum-in-base16-range?

    ascii-cased?		$ascii-cased?
    ascii-upper-case?		$ascii-upper-case?
    ascii-lower-case?		$ascii-lower-case?
    (rename (ascii-upper-case?	ascii-title-case?)
	    (ascii-cased?	ascii-alphabetic?)
	    ($ascii-upper-case?	$ascii-title-case?)
	    ($ascii-cased?	$ascii-alphabetic?))

    ascii-upcase		$ascii-upcase
    ascii-downcase		$ascii-downcase
    (rename (ascii-upcase	ascii-titlecase)
	    ($ascii-upcase	$ascii-titlecase))

    ascii-dec-digit?		$ascii-dec-digit?
    ascii-hex-digit?		$ascii-hex-digit?
    ascii-alpha-digit?		$ascii-alpha-digit?

    ascii-dec->fixnum		$ascii-dec->fixnum
    ascii-hex->fixnum		$ascii-hex->fixnum
    fixnum->ascii-dec		$fixnum->ascii-dec
    fixnum->ascii-hex		$fixnum->ascii-hex

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
    (vicare unsafe operations)
    (vicare arguments validation)
    (vicare language-extensions ascii-chars syntaxes))


;;;; generic utilities for ASCII encoded characters

(define (fixnum-in-ascii-range? obj)
  (and (fixnum? obj)
       ($fixnum-in-ascii-range? obj)))

;;; --------------------------------------------------------------------

(define (fixnum-in-base10-range? obj)
  (and (fixnum? obj)
       ($fixnum-in-base10-range? obj)))

;;; --------------------------------------------------------------------

(define (fixnum-in-base16-range? obj)
  (and (fixnum? obj)
       ($fixnum-in-base16-range? obj)))

;;; --------------------------------------------------------------------

(define (ascii-upper-case? fx)
  (and (fixnum? fx)
       ($ascii-upper-case? fx)))

;;; --------------------------------------------------------------------

(define (ascii-lower-case? fx)
  (and (fixnum? fx)
       ($ascii-lower-case? fx)))

;;; --------------------------------------------------------------------

(define (ascii-cased? fx)
  (and (fixnum? fx)
       ($ascii-cased? fx)))

;;; --------------------------------------------------------------------

(define (ascii-dec-digit? fx)
  (and (fixnum? fx)
       ($ascii-dec-digit? fx)))

;;; --------------------------------------------------------------------

(define (ascii-hex-digit? fx)
  (and (fixnum? fx)
       ($ascii-hex-digit? fx)))

;;; --------------------------------------------------------------------

(define* (ascii-upcase (fx fixnum-in-ascii-range?))
  ($ascii-upcase fx))

;;; --------------------------------------------------------------------

(define* (ascii-downcase (fx fixnum-in-ascii-range?))
  ($ascii-downcase fx))

;;; --------------------------------------------------------------------

(define (ascii-alpha-digit? fx)
  (and (fixnum? fx)
       ($ascii-alpha-digit? fx)))


;;;; conversion

(define* (ascii-dec->fixnum (chi ascii-dec-digit?))
  ($ascii-dec->fixnum chi))

;;; --------------------------------------------------------------------

(define* (fixnum->ascii-dec (chi fixnum-in-base10-range?))
  ($ascii-dec->fixnum chi))

;;; --------------------------------------------------------------------

(define* (ascii-hex->fixnum (chi ascii-hex-digit?))
  ($ascii-hex->fixnum chi))

;;; --------------------------------------------------------------------

(define* (fixnum->ascii-hex (chi fixnum-in-base16-range?))
  ($ascii-hex->fixnum chi))


;;;; done

)

;;; end of file
