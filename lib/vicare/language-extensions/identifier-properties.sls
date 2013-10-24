;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Scheme
;;;Contents: expand-time identifier properties
;;;Date: Mon Nov  8, 2010
;;;
;;;Abstract
;;;
;;;	The idea is taken from the compile-time values and properties of
;;;	Chez  Scheme  (see  Chez's  manual, Section  11.4  "Compile-time
;;;	Values and Properties").  No code comes from Chez.
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
(library (vicare language-extensions identifier-properties)
  (export
    register-identifier-property
    forget-identifier-property
    identifier-property-exists?
    identifier-property-set!
    identifier-property-ref
    ;;This is for debugging purposes.
    #;identifier-property-table)
  (import (vicare)
    (vicare arguments validation)
    (vicare language-extensions identifier-alists)
    (vicare language-extensions sentinels)
    (vicare language-extensions variables))


;;;; helpers

(define-constant THE-SENTINEL
  (make-sentinel))


;;;; table of property tables

;;Identifier properties are stored in an alist and they are alists.
;;
;;An alist of alists sucks in  performance, but with identifiers as keys
;;we have no other choice.
;;
(define TABLE-OF-PROPERTY-TABLES '())

(define-syntax (with-identifier-property-table stx)
  (syntax-case stx ()
    ((_ (?who ?subject ?table) ?body0 ?body ...)
     (all-identifiers? #'(?who ?subject ?table))
     #'(cond ((identifier-alist-ref TABLE-OF-PROPERTY-TABLES ?subject #f)
	      => (lambda (box)
		   (define-variable-alias ?table box)
		   ?body0 ?body ...))
	     (else
	      (assertion-violation ?who "request to access undefined identifier property" ?subject))))))

(define (register-identifier-property subject)
  (define who 'register-identifier-property)
  (with-arguments-validation (who)
      ((identifier	subject))
    (cond ((identifier-alist-exists TABLE-OF-PROPERTY-TABLES subject)
	   => (lambda (entry)
		(assertion-violation who "identifier property already exists" subject)))
	  (else
	   (set! TABLE-OF-PROPERTY-TABLES (identifier-alist-new subject (make-variable '()) TABLE-OF-PROPERTY-TABLES))))))

(define (forget-identifier-property subject)
  (define who 'forget-identifier-property)
  (with-arguments-validation (who)
      ((identifier	subject))
    (set! TABLE-OF-PROPERTY-TABLES (identifier-alist-remove TABLE-OF-PROPERTY-TABLES subject))))

(define (identifier-property-exists? subject)
  (define who 'identifier-property-exists?)
  (with-arguments-validation (who)
      ((identifier	subject))
    (cond ((identifier-alist-exists TABLE-OF-PROPERTY-TABLES subject)
	   => (lambda (entry) #t))
	  (else #f))))


;;;; access to properties

(define (identifier-property-table subject)
  ;;Return an alist  representing the property table  or SUBJECT.  Raise
  ;;an assertion violation if SUBJECT is not a previously defined object
  ;;property.
  ;;
  ;;This is for debugging purposes only.
  ;;
  (define who 'identifier-property-table)
  (with-arguments-validation (who)
      ((identifier	subject))
    (with-identifier-property-table (who subject property-table)
      property-table)))

(define (identifier-property-set! subject key value)
  ;;Associate  KEY to  VALUE  in SUBJECT's  table.   Raise an  assertion
  ;;violation if  SUBJECT is not  a previously defined  object property.
  ;;If  KEY already  has an  identifier property  for SUBJECT:  silently
  ;;overwrite the old value.
  ;;
  (define who 'identifier-property-set!)
  (with-arguments-validation (who)
      ((identifier	subject)
       (identifier	key))
    (with-identifier-property-table (who subject property-table)
      (set! property-table (identifier-alist-cons-and-replace key value property-table)))))

(define (identifier-property-ref subject key default)
  ;;Return the  value associated  to KEY in  SUBJECT's table.   Raise an
  ;;assertion violation  if SUBJECT is  not a previously  defined object
  ;;property.  If KEY does not  have an identifier property for SUBJECT:
  ;;return DEFAULT.
  ;;
  (define who 'identifier-property-ref)
  (with-arguments-validation (who)
      ((identifier	subject)
       (identifier	key))
    (with-identifier-property-table (who subject property-table)
      (let ((value (identifier-alist-ref property-table key THE-SENTINEL)))
	(if (eq? value THE-SENTINEL)
	    default
	  value)))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'with-identifier-property-table 'scheme-indent-function 1)
;; End:
