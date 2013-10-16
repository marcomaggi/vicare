;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: source location objects
;;;Date: Wed Oct 16, 2013
;;;
;;;Abstract
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa parser-tools source-locations)
  (export
    <source-location>

    ;; auxiliary syntaxes
    line:
    column:
    offset:

    unspecified-source-location
    unspecified-source-location-string

    source-location-tab-function
    source-location-tab-function/8chars
    source-location-tab-function/tab-table
    source-location-tab-table
    source-location-honor-return)
  (import (nausicaa)
    (prefix (vicare language-extensions makers) mk.)
    (vicare arguments validation)
    (vicare system $fx))


(define-auxiliary-syntaxes
  column:
  line:
  offset:)


(define-class <source-location>
  (nongenerative nausicaa:parser-tools:lexical-tokens:<source-location>)

  (fields (immutable (specified?	<boolean>))
	  (immutable (line		<positive-fixnum>))
	  (immutable (column		<positive-fixnum>))
	  (immutable (offset		<nonnegative-fixnum>)))

  (virtual-fields
   (immutable (unspecified?	<boolean>)
	      (lambda ((O <source-location>))
		(not (O $specified?)))))

  (protocol (lambda (make-top)
	      (lambda ((specified? <boolean>)
		  (line <positive-fixnum>) (column <positive-fixnum>)
		  (offset <nonnegative-fixnum>))
		(<- <source-location>)
		(if specified?
		    ((make-top) specified? line column offset)
		  (unspecified-source-location)))))

  (maker (lambda (stx)
	   (syntax-case stx ()
	     ((_ (?unspecified))
	      (and (identifier? #'?unspecified)
		   (identifier=symbol? #'?unspecified 'unspecified))
	      #'(make-<source-location> #t 1 1 0))
	     ((_ (?expr ...))
	      #'(%make-source-location ?expr ...)))))

;;; --------------------------------------------------------------------

  (method (start? (L <source-location>))
    (and (L $specified?)
	 ($fx= 1 (L $line))
	 ($fx= 1 (L $column))))

  (method (string (L <source-location>))
    (<- <string>)
    ;;If  the line  and  column numbers  are  non-positive: this  object
    ;;represents an unspecified source location.
    ;;
    (if (L $specified?)
	(string-append (number->string (L $line))
		       ":"
		       (number->string (L $column)))
      (unspecified-source-location-string)))

;;; --------------------------------------------------------------------

  (method (= (A <source-location>) (B <source-location>))
    (and (A $specified?)
	 (B $specified?)
	 ($fx= (A $offset) (B $offset))))

  (method (< (A <source-location>) (B <source-location>))
    (and (A $specified?)
	 (B $specified?)
	 ($fx< (A $offset) (B $offset))))

  (method (> (A <source-location>) (B <source-location>))
    (and (A $specified?)
	 (B $specified?)
	 ($fx> (A $offset) (B $offset))))

  (method (<= (A <source-location>) (B <source-location>))
    (and (A $specified?)
	 (B $specified?)
	 ($fx<= (A $offset) (B $offset))))

  (method (>= (A <source-location>) (B <source-location>))
    (and (A $specified?)
	 (B $specified?)
	 ($fx>= (A $offset) (B $offset))))

;;; --------------------------------------------------------------------

  (methods (update source-location-update))

  #| end of class definition |# )

(mk.define-maker %make-source-location
    (make-<source-location> #t)
  ;;These default  values represent  the start location  of a  source of
  ;;characters.
  ((line:	1)
   (column:	1)
   (offset:	0)))

;;; --------------------------------------------------------------------

(module ()
  (add-method object->string (<source-location>) <source-location>-string))


(define-generic source-location-update (location delta))

(define-method ((source-location-update <source-location>) (L <source-location>) (offset-delta <fixnum>))
  (if (L $specified?)
      (<source-location> ((line:   (L $line))
			  (column: (+ offset-delta (L $column)))
			  (offset: (+ offset-delta (L $offset)))))
    (unspecified-source-location)))

(define-method ((source-location-update <source-location>) (L <source-location>) (char <char>))
  (if (L $specified?)
      (let ((new-offset (fx+ 1 (L $offset))))
	(case char
	  ((#\newline)
	   (<source-location> ((line:   (fx+ 1 (L $line)))
			       (column: 1)
			       (offset: new-offset))))
	  ((#\return)
	   (<source-location> ((line:   (L $line))
			       (column: (if (source-location-honor-return)
					    1
					  (fx+ 1 (L $column))))
			       (offset: new-offset))))
	  ((#\tab)
	   (<source-location> ((line:   (L $line))
			       (column: ((source-location-tab-function) (L $column)))
			       (offset: new-offset))))
	  (else
	   (<source-location> ((line:   (L $line))
			       (column: (+ 1 (L column)))
			       (offset: new-offset))))))
    (unspecified-source-location)))

;;; --------------------------------------------------------------------

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


(define unspecified-source-location
  (let ((L (<source-location> (unspecified))))
    (lambda () L)))

(define unspecified-source-location-string
  (make-parameter "no-source"
    (lambda (obj)
      (define who 'unspecified-source-location-string)
      (with-arguments-validation (who)
	  ((string	obj))
	obj))))


;;;; done

)

;;; end of file
