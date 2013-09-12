;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: extended struct and R6RS records types
;;;Date: Thu Sep 12, 2013
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
(library (vicare language-extensions define-record-extended)
  (export
    define-struct-extended
    define-record-type-extended)
  (import (vicare)
    (vicare arguments validation))


;;;; extended R6RS record type definition

(define-syntax define-record-type-extended
  ;;Like DEFINE-RECORD-TYPE but define also argument validators.
  ;;
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((_ ?type-id . ?body)
	 (identifier? #'?type-id)
	 (output #'?type-id #'?type-id #'?body))

	((_ (?name ?constructor ?predicate) . ?body)
	 (and (identifier? #'?name)
	      (identifier? #'?constructor)
	      (identifier? #'?predicate))
	 (output #'?name #'(?name ?constructor ?predicate) #'?body))))

    (define (output type-id type-spec-stx body-stx)
      (let ((type-str (%id->string type-id)))
	(with-syntax
	    ((TYPE-PRED
	      (%id->id type-id (lambda (type-str)
				 (string-append type-str "?"))))
	     (TYPE-VALIDATOR
	      type-id)
	     (TYPE-VALIDATOR-MESSAGE
	      (string-append "expected struct instance of type \""
			     type-str
			     "\" as argument"))
	     (FALSE-OR-TYPE-VALIDATOR
	      (%id->id type-id (lambda (type-str)
				 (string-append "false-or-" type-str))))
	     (FALSE-OR-TYPE-VALIDATOR-MESSAGE
	      (string-append "expected false or struct instance of type \""
			     type-str
			     "\" as argument")))
	  #`(begin
	      (define-record-type #,type-spec-stx
		. #,body-stx)
	      (define-argument-validation (TYPE-VALIDATOR who obj)
		(TYPE-PRED obj)
		(assertion-violation who TYPE-VALIDATOR-MESSAGE obj))
	      (define-argument-validation (FALSE-OR-TYPE-VALIDATOR who obj)
		(or (not obj) (TYPE-PRED obj))
		(assertion-violation who FALSE-OR-TYPE-VALIDATOR-MESSAGE obj))
	      ))))


    (define (%id->id src-id string-maker)
      (datum->syntax src-id (string->symbol (string-maker (%id->string src-id)))))

    (define (%id->string id)
      (symbol->string (syntax->datum id)))

    (define (syntax->list stx)
      (syntax-case stx ()
	((?car . ?cdr)
	 (cons #'?car (syntax->list #'?cdr)))
	(() '())))

    (let ((out (main stx)))
      #;(pretty-print (syntax->datum out) (current-error-port))
      out)))


;;;; extended struct definition

(define-syntax define-struct-extended
  ;;Like DEFINE-STRUCT but define  also argument validators, an optional
  ;;printer and an optional destructor.
  ;;
  (lambda (stx)
    (define (main stx)
      (syntax-case stx ()
	((?kwd ?type-id (?field-id ...))
	 #'(?kwd ?type-id (?field-id ...) #f #f))
	((_ ?type-id (?field-id ...) ?printer ?destructor)
	 (and (identifier? #'?type-id)
	      (for-all identifier? (syntax->list #'(?field-id ...))))
	 (let* ((type-id	#'?type-id)
		(type-str	(%id->string type-id)))
	   (with-syntax
	       ((TYPE-PRED
		 (%id->id type-id (lambda (type-str)
				    (string-append type-str "?"))))
		(TYPE-VALIDATOR
		 type-id)
		(TYPE-VALIDATOR-MESSAGE
		 (string-append "expected struct instance of type \""
				type-str
				"\" as argument"))
		(FALSE-OR-TYPE-VALIDATOR
		 (%id->id type-id (lambda (type-str)
				    (string-append "false-or-" type-str))))
		(FALSE-OR-TYPE-VALIDATOR-MESSAGE
		 (string-append "expected false or struct instance of type \""
				type-str
				"\" as argument"))
		((PRINTER-REGISTRATION ...)
		 (if (identifier? #'?printer)
		     #'((module ()
			  (set-rtd-printer! (type-descriptor ?type-id)
					    ?printer)))
		   #'()))
		((DESTRUCTOR-REGISTRATION ...)
		 (if (identifier? #'?destructor)
		     #'((module ()
			  (set-rtd-destructor! (type-descriptor ?type-id)
					       ?destructor)))
		   #'())))
	     #'(begin
		 (define-struct ?type-id
		   (?field-id ...))

		 (define-argument-validation (TYPE-VALIDATOR who obj)
		   (TYPE-PRED obj)
		   (assertion-violation who TYPE-VALIDATOR-MESSAGE obj))

		 (define-argument-validation (FALSE-OR-TYPE-VALIDATOR who obj)
		   (or (not obj) (TYPE-PRED obj))
		   (assertion-violation who FALSE-OR-TYPE-VALIDATOR-MESSAGE obj))
		 PRINTER-REGISTRATION ...
		 DESTRUCTOR-REGISTRATION ...
		 ))))
	))

    (define (%id->id src-id string-maker)
      (datum->syntax src-id (string->symbol (string-maker (%id->string src-id)))))

    (define (%id->string id)
      (symbol->string (syntax->datum id)))

    (define (syntax->list stx)
      (syntax-case stx ()
	((?car . ?cdr)
	 (cons #'?car (syntax->list #'?cdr)))
	(() '())))

    (let ((out (main stx)))
      #;(pretty-print (syntax->datum out) (current-error-port))
      out)))


;;;; done

)

;;; end of file
