;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: C language enumeration utilities
;;;Date: Tue Dec 29, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare language-extensions c-enumerations)
  (export define-c-flags define-c-ior-flags)
  (import (vicare))


(define-syntax (define-c-flags stx)
  (define (%name->enum-name sym)
    (string->symbol (string-append "enum-" (symbol->string sym))))
  (define (%name->tovalue-name sym)
    (string->symbol (string-append (symbol->string sym) "->value")))
  (define (%name->fromvalue-name sym)
    (string->symbol (string-append "value->" (symbol->string sym))))
  (define (%name->stx-name sym)
    (string->symbol (string-append "%" (symbol->string sym))))
  (syntax-case stx ()
    ((_ ?name (?flag0 ?flag ...) (?symbol0 ?symbol ...))
     (with-syntax
	 ((ENUM-NAME	(datum->syntax #'?name (%name->enum-name      (syntax->datum #'?name))))
	  (NAME->VALUE	(datum->syntax #'?name (%name->tovalue-name   (syntax->datum #'?name))))
	  (VALUE->NAME	(datum->syntax #'?name (%name->fromvalue-name (syntax->datum #'?name))))
	  (STX-NAME	(datum->syntax #'?name (%name->stx-name       (syntax->datum #'?name)))))
       #'(begin
	   (define-enumeration ENUM-NAME (?symbol0 ?symbol ...) STX-NAME)
	   (define-syntax ?name
	     (syntax-rules () ((_ flag) (STX-NAME flag))))
	   (define NAME->VALUE
	     (let* ((universe		(enum-set-universe (STX-NAME)))
		    (symbol->index	(enum-set-indexer universe))
		    (flags		(vector ?flag0 ?flag ...)))
	       (lambda (set)
		 (assert (enum-set-subset? set universe))
		 (vector-ref flags (symbol->index (car (enum-set->list set)))))))
	   (define VALUE->NAME
	     (let ((make     (enum-set-constructor (enum-set-universe (STX-NAME))))
		   (flags    (list ?flag0 ?flag ...))
		   (symbols  (vector '(?symbol0) '(?symbol) ...)))
	       (lambda (value)
		 (let loop ((i 0) (flags flags))
		   (cond ((null? flags)
			  #f)
			 ((= value (car flags))
			  (make (vector-ref symbols i)))
			 (else
			  (loop (+ 1 i) (cdr flags))))))))
	   )))))


(define-syntax (define-c-ior-flags stx)
  (define (%name->enum-name sym)
    (string->symbol (string-append "enum-" (symbol->string sym))))
  (define (%name->tovalue-name sym)
    (string->symbol (string-append (symbol->string sym) "->value")))
  (define (%name->fromvalue-name sym)
    (string->symbol (string-append "value->" (symbol->string sym))))
  (syntax-case stx ()
    ((_ ?name (?flag0 ?flag ...) (?symbol0 ?symbol ...))
     (with-syntax
	 ((ENUM-NAME	(datum->syntax #'?name (%name->enum-name      (syntax->datum #'?name))))
	  (NAME->VALUE	(datum->syntax #'?name (%name->tovalue-name   (syntax->datum #'?name))))
	  (VALUE->NAME	(datum->syntax #'?name (%name->fromvalue-name (syntax->datum #'?name)))))
       #'(begin
	   (define-enumeration ENUM-NAME
	     (?symbol0 ?symbol ...)
	     ?name)
	   (define NAME->VALUE
	     (let* ((universe		(enum-set-universe (?name)))
		    (symbol->index	(enum-set-indexer universe))
		    (flags		(vector ?flag0 ?flag ...)))
	       (lambda (set)
		 (assert (enum-set-subset? set universe))
		 (fold-left (lambda (knil symbol)
			      (bitwise-ior knil (vector-ref flags (symbol->index symbol))))
		   0
		   (enum-set->list set)))))
	   (define VALUE->NAME
	     (let ((list->set	(enum-set-constructor (enum-set-universe (?name))))
		   (alist	`((,?flag0		. ?symbol0)
				  (,?flag		. ?symbol)
				  ...)))
	       (lambda (value)
		 (let loop ((alist	alist)
			    (ell	'()))
		   (cond ((null? alist)
			  (list->set ell))
			 ((not (zero? (bitwise-and value (caar alist))))
			  (loop (cdr alist) (cons (cdar alist) ell)))
			 (else
			  (loop (cdr alist) ell)))))))
	   )))))


;;;; done

)

;;; end of file
