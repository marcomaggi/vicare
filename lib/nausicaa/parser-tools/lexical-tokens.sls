;;;
;;;Part of: Vicare Scheme
;;;Contents: lexical token record
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	The <LEXICAL-TOKEN>  record type describes tokens  produced by a
;;;	lexer and consumed  by a parser.  It is meant to  be used by all
;;;	the parser libraries distributed with Vicare.
;;;
;;;	The <SOURCE-LOCATION>  record type describes the  position in an
;;;	input stream  of a token produced  by a lexer and  consumed by a
;;;	parser.   It is meant  to be  used by  all the  parser libraries
;;;	distributed with Vicare.
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;
;;;Original  code  by  Dominique  Boucher.   Port  to  R6RS  and  Vicare
;;;integration by Marco Maggi.
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
(library (nausicaa parser-tools lexical-tokens)
  (export
    <lexical-token>
    <lexer-error>
    <source-location>

    ;; auxiliary syntaxes
    category:
    column:
    error-message:
    input:
    length:
    line:
    location:
    offset:
    value:

    ;; traditional records API
    make-<lexical-token>
    make-<lexical-token>/end-of-input
    make-<lexical-token>/lexer-error

    <lexical-token>?

    <lexical-token>-value
    <lexical-token>-category
    <lexical-token>-location
    <lexical-token>-length

    <lexical-token>?/end-of-input
    <lexical-token>?/lexer-error
    <lexical-token>?/special

    make-<lexer-error>
    <lexer-error>?
    <lexer-error>-message

    make-<source-location>	make-<source-location>/start
    <source-location>?		<source-location>?/start
    <source-location>?/or-false	<source-location>?/start/or-false

    <source-location>-line
    <source-location>-input
    <source-location>-column
    <source-location>-offset

    source-location-update
    source-location->string

    source-location=?

    source-location-point=?
    source-location-point>?  source-location-point<?
    source-location-point>=? source-location-point<=?

    source-location-tab-function
    source-location-tab-function/8chars
    source-location-tab-function/tab-table
    source-location-tab-table

    source-location-honor-return)
  (import (nausicaa)
    (prefix (vicare language-extensions makers) mk.))


(define-auxiliary-syntaxes
  category:
  column:
  error-message:
  input:
  length:
  line:
  location:
  offset:
  value:
  )


(define-class <lexical-token>
  (nongenerative vicare:parser-tools:<lexical-token>)
  (maker
   (lambda (stx)
     (syntax-case stx ()
       ((_ (?clause ...))
	#'(%make-lexical-token ?clause ...)))))
  (protocol
   (lambda (make-top)
     (lambda (category location value length)
       ((make-top) category location value length))))
  (fields (immutable category)
	  (immutable (location <source-location>))
	  (immutable value)
	  (immutable length))
  (virtual-fields (immutable end-of-input?	<lexical-token>?/end-of-input)
		  (immutable lexer-error?	<lexical-token>?/lexer-error)
		  (immutable special?		<lexical-token>?/special)))

(mk.define-maker %make-lexical-token
    make-<lexical-token>
  ((category:	#f (mk.mandatory))
   (location:	#f)
   (value:	#f)
   (length:	0)))

(define (make-<lexical-token>/end-of-input location)
  (make-<lexical-token> '*eoi* location (eof-object) 0))

(define (make-<lexical-token>/lexer-error location value length)
  (make-<lexical-token> '*lexer-error* location (eof-object) 0))

(define (<lexical-token>?/end-of-input obj)
  (and (<lexical-token>? obj)
       (eq? '*eoi* (<lexical-token>-category obj))
       #t))

(define (<lexical-token>?/lexer-error obj)
  (and (<lexical-token>? obj)
       (eq? '*lexer-error* (<lexical-token>-category obj))
       #t))

(define (<lexical-token>?/special obj)
  (and (<lexical-token>? obj)
       (memq (<lexical-token>-category obj) '(*eoi* *lexer-error*))
       #t))


(define-class <lexer-error>
  (nongenerative vicare:parser-tools:lexical-tokens:<lexer-error>)
  (parent <lexical-token>)
  (fields (immutable (message <string>)))
  (maker
   (lambda (stx)
     (syntax-case stx (password)
       ((_ (?expr ...))
	#'(%make-lexer-error ?expr ...)))))
  (protocol
   (lambda (make-lexical-token)
     (lambda (location value length error-message)
       ((make-lexical-token '*lexer-error* location value length) error-message)))))

(mk.define-maker %make-lexer-error
    (make-<lexer-error>)
  ((location:		#f)
   (value:		#f)
   (length:		0)
   (error-message:	#f (mk.mandatory))))


(define-class <source-location>
  (nongenerative vicare:parser-tools:lexical-tokens:<source-location>)
  (fields (immutable input)
	  (immutable (line	<integer>))
	  (immutable (column	<integer>))
	  (immutable (offset	<integer>)))
  (maker
   (lambda (stx)
     (syntax-case stx (password)
       ((_ (?expr ...))
	#'(%make-source-location ?expr ...))))))

(mk.define-maker %make-source-location
    (make-<source-location>)
  ((input:	#f)
   (line:	#f)
   (column:	#f)
   (offset:	#f)))

(define-inline (make-<source-location>/start input-spec)
  (make-<source-location> input-spec 1 1 0))

(define (<source-location>?/or-false obj)
  (or (not obj)
      (<source-location>? obj)))

(define (<source-location>?/start obj)
  (and (<source-location>? obj)
       (= 1 (<source-location>-line   obj))
       (= 1 (<source-location>-column obj))))

(define (<source-location>?/start/or-false obj)
  (or (not obj)
      (<source-location>?/start obj)))


(define (source-location=? a b)
  (cond ((not a) #f)
	((not b) #f)
	(else
	 (with-tags ((a <source-location>)
		     (b <source-location>))
	   (and (= (a line) (b line))
		(= (a column) (b column))
		(= (a offset) (b offset)))))))

(define (source-location-point=? a b)
  (cond ((not a) #f)
	((not b) #f)
	(else
	 (with-tags ((a <source-location>)
		     (b <source-location>))
	   (and (= (a line) (b line))
		(= (a column) (b column)))))))

(define (source-location-point>? a b)
  (cond ((not a) #f)
	((not b) #t)
	(else
	 (with-tags ((a <source-location>)
		     (b <source-location>))
	   (or (> (a line) (b line))
	       (and (= (a line) (b line))
		    (> (a column) (b column))))))))

(define (source-location-point<? a b)
  (source-location-point>? b a))

(define (source-location-point>=? a b)
  (cond ((not a) #f)
	((not b) #t)
	(else
	 (with-tags ((a <source-location>)
		     (b <source-location>))
	   (or (> (a line) (b line))
	       (and (= (a line) (b line))
		    (>= (a column) (b column))))))))

(define (source-location-point<=? a b)
  (source-location-point>=? b a))


(define (source-location-update (location <source-location>) char/token-length)
  (and location
       (cond ((integer? char/token-length)
	      (make-<source-location> (location input) (location line)
				      (+ char/token-length (location column))
				      (+ char/token-length (location offset))))

	     ((char? char/token-length)
	      (let ((new-offset (+ 1 (location offset))))
		(case char/token-length
		  ((#\newline)
		   (make-<source-location> (location input) (+ (location line) 1) 1 new-offset))
		  ((#\return)
		   (make-<source-location> (location input) (location line)
					   (if (source-location-honor-return)
					       1
					     (+ 1 (location column)))
					   new-offset))
		  ((#\tab)
		   (make-<source-location> (location input) (location line)
					   ((source-location-tab-function) (location column))
					   new-offset))
		  (else
		   (make-<source-location> (location input) (location line)
					   (+ 1 (location column)) new-offset)))))
	     (else
	      (assertion-violation 'source-location-update
		"expected character or lexical token length"
		char/token-length)))))

(define (source-location-tab-function/8chars column)
  (+ 8 (- column (mod column 8))))

(define (source-location-tab-function/tab-table column)
  (let ((table (source-location-tab-table)))
    (let loop ((table table))
      (cond ((null? table)
	     (source-location-tab-function/8chars column))
	    ((< column (car table))
	     (car table))
	    (else
	     (loop (cdr table)))))))

(define source-location-tab-function
  (make-parameter source-location-tab-function/8chars
    (lambda (obj)
      (assert (procedure? obj))
      obj)))

(define source-location-tab-table
  (make-parameter '()
    (lambda (obj)
      (assert (list? obj))
      obj)))

(define source-location-honor-return
  (make-parameter #f))


(define (source-location->string (location <source-location>))
  (if location
      (string-append (object->string (location input))
		     ":"
		     (number->string (location line))
		     ":"
		     (number->string (location column)))
    "<??>"))

(define-method (object->string (o <source-location>))
  (source-location->string o))


;;;; done

)

;;; end of file
