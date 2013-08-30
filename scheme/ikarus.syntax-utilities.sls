;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: utility functions for macro-writing
;;;Date: Thu Aug 29, 2013
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
(library (ikarus.syntax-utilities)
  (export

    ;; pairs processing
    syntax-car		syntax-cdr
    syntax->list	identifiers->list
    all-identifiers?

    ;; identifier processing: generic functions
    identifier-prefix	identifier-suffix
    identifier-append	identifier-format

    ;; identifier processing: records API
    identifier-record-constructor
    identifier-record-predicate
    identifier-record-field-accessor
    identifier-record-field-mutator

    ;; identifier processing: structs API
    identifier-struct-constructor
    identifier-struct-predicate
    identifier-struct-field-accessor
    identifier-struct-field-mutator

    )
  (import (vicare))


;;;; helpers

(define (%make-synner who)
  (lambda (message subform)
    (syntax-violation who message subform #f)))


;;;; pairs processing

(define syntax-car
  (case-lambda
   ((stx)
    (syntax-car stx (%make-synner 'syntax-car)))
   ((stx synner)
    (syntax-case stx ()
      ((?car . ?cdr)
       #'?car)
      (_
       (synner "expected syntax object holding pair as argument" stx))))))

(define syntax-cdr
  (case-lambda
   ((stx)
    (syntax-cdr stx (%make-synner 'syntax-cdr)))
   ((stx synner)
    (syntax-case stx ()
      ((?car . ?cdr)
       #'?cdr)
      (_
       (synner "expected syntax object holding pair as argument" stx))))))

(define syntax->list
  (case-lambda
   ((stx)
    (syntax->list stx (%make-synner 'syntax->list)))
   ((stx synner)
    (syntax-case stx ()
      (() '())
      ((?car . ?cdr)
       (identifier? #'?car)
       (cons #'?car (syntax->list #'?cdr synner)))
      ((?car . ?cdr)
       (cons (syntax->datum #'?car) (syntax->list #'?cdr synner)))
      (_
       (synner "expected syntax object holding proper list as argument" stx))))))

(define identifiers->list
  (case-lambda
   ((stx)
    (identifiers->list stx (%make-synner 'identifiers->list)))
   ((stx synner)
    (syntax-case stx ()
      (() '())
      ((?car . ?cdr)
       (identifier? #'?car)
       (cons #'?car (identifiers->list #'?cdr synner)))
      (_
       (synner "expected syntax object holding proper list of identifiers as argument" stx))))))

(define (all-identifiers? stx)
  (syntax-case stx ()
    (() #t)
    ((?car . ?cdr)
     (identifier? #'?car)
     (all-identifiers? #'?cdr))
    (_ #f)))


;;;; identifiers processing: generic functions

(define (identifier-prefix prefix id)
  (define who 'identifier-prefix)
  (assert (identifier? id))
  (datum->syntax id (string->symbol
		     (string-append
		      (cond ((string? prefix)
			     prefix)
			    ((symbol? prefix)
			     (symbol->string prefix))
			    ((identifier? prefix)
			     (symbol->string (syntax->datum prefix)))
			    (else
			     (assertion-violation who
			       "expected string, symbol or identifier as prefix argument" prefix)))
		      (symbol->string (syntax->datum id))))))

(define (identifier-suffix id suffix)
  (define who 'identifier-suffix)
  (assert (identifier? id))
  (datum->syntax id (string->symbol
		     (string-append
		      (symbol->string (syntax->datum id))
		      (cond ((string? suffix)
			     suffix)
			    ((symbol? suffix)
			     (symbol->string suffix))
			    ((identifier? suffix)
			     (symbol->string (syntax->datum suffix)))
			    (else
			     (assertion-violation who
			       "expected string, symbol or identifier as suffix argument" suffix)))))))

(define (identifier-append ctx . items)
  (define who 'identifier-append)
  (identifier? ctx)
  (receive (port getter)
      (open-string-output-port)
    (for-each (lambda (item)
		(display (cond ((string? item)
				item)
			       ((symbol? item)
				(symbol->string item))
			       ((identifier? item)
				(symbol->string (syntax->datum item)))
			       (else
				(assertion-violation who
				  "expected string, symbol or identifier as item argument" item)))
			 port))
      items)
    (datum->syntax ctx (string->symbol (getter)))))

(define (identifier-format ctx template . items)
  (define who 'identifier-format)
  (datum->syntax ctx (string->symbol
		      (apply format template
			     (map (lambda (item)
				    (cond ((string? item)
					   item)
					  ((symbol? item)
					   (symbol->string item))
					  ((identifier? item)
					   (symbol->string (syntax->datum item)))
					  (else
					   (assertion-violation who
					     "expected string, symbol or identifier as item argument" item))))
			       items)))))


;;;; identifiers processing: records API

(define (identifier-record-constructor type-id)
  (identifier-prefix "make-" type-id))

(define (identifier-record-predicate type-id)
  (identifier-suffix type-id "?"))

(define (identifier-record-field-accessor type-id field-name)
  (identifier-append type-id type-id "-" field-name))

(define (identifier-record-field-mutator type-id field-name)
  (identifier-append type-id type-id "-" field-name "-set!"))


;;;; identifiers processing: structs API

(define (identifier-struct-constructor type-id)
  (identifier-prefix "make-" type-id))

(define (identifier-struct-predicate type-id)
  (identifier-suffix type-id "?"))

(define (identifier-struct-field-accessor type-id field-name)
  (identifier-append type-id type-id "-" field-name))

(define (identifier-struct-field-mutator type-id field-name)
  (identifier-append type-id "set-" type-id "-" field-name "!"))


;;;; done

)

;;; end of file
