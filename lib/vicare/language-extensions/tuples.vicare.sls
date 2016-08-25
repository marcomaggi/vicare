;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tuples on top of lists and vectors
;;;Date: Thu Aug 25, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare language-extensions tuples (0 4 2016 08 25))
  (options typed-language)
  (export
    define-list-tuple-type
    define-vector-tuple-type
    fields
    brace)
  (import (vicare)
    (prefix (vicare expander) xp::)
    (vicare language-extensions labels))


(define-syntax (define-list-tuple-type stx)
  (define (main stx)
    (syntax-case stx (fields)
      ((_ ?type-name (fields ?field-spec0 ?field-spec ...))
       (let ((field-spec* (xp::syntax-unwrap #'(?field-spec0 ?field-spec ...))))
	 (receive (field-name* field-type*)
	     (%parse-field-specs field-spec*)
	   (with-syntax
	       (((FIELD-NAME0 FIELD-NAME ...)	field-name*)
		((FIELD-TYPE0 FIELD-TYPE ...)	field-type*)
		((IDX ...)		(%iota 1 (length field-spec*))))
	     #'(define-label-type ?type-name
		 (parent (list FIELD-TYPE0 FIELD-TYPE ...))
		 (constructor ({FIELD-NAME0 FIELD-TYPE0} {FIELD-NAME FIELD-TYPE} ...)
		   (list FIELD-NAME0 FIELD-NAME ...))
		 (method ({FIELD-NAME0 FIELD-TYPE0})	(cast-signature (FIELD-TYPE0) (car this)))
		 (method ({FIELD-NAME  FIELD-TYPE})	(cast-signature (FIELD-TYPE)  (list-ref this IDX)))
		 ...
		 )))))
      (_
       (__synner__ "invalid syntax in input form"))))

  (define (%parse-field-specs field-spec*)
    (if (pair? field-spec*)
	(syntax-case (car field-spec*) (brace)
	  ((brace ?field-name ?field-type)
	   (identifier? #'?field-name)
	   (receive (field-name* field-type*)
	       (%parse-field-specs (cdr field-spec*))
	     (values (cons #'?field-name field-name*)
		     (cons #'?field-type field-type*))))

	  (?field-name
	   (identifier? #'?field-name)
	   (receive (field-name* field-type*)
	       (%parse-field-specs (cdr field-spec*))
	     (values (cons #'?field-name field-name*)
		     (cons #'<top>       field-type*))))

	  (?stuff
	   (__synner__ "invalid field specification" #'?stuff)))
      (values '() '())))

  (define (%iota start end)
    (if (fx<? start end)
	(cons start (%iota (fxadd1 start) end))
      '()))

  (main stx))


(define-syntax (define-vector-tuple-type stx)
  (define (main stx)
    (syntax-case stx (fields)
      ((_ ?type-name (fields ?field-spec0 ?field-spec ...))
       (let ((field-spec* (xp::syntax-unwrap #'(?field-spec0 ?field-spec ...))))
	 (receive (field-name* field-type*)
	     (%parse-field-specs field-spec*)
	   (with-syntax
	       (((FIELD-NAME0 FIELD-NAME ...)	field-name*)
		((FIELD-TYPE0 FIELD-TYPE ...)	field-type*)
		((IDX ...)		(%iota 1 (length field-spec*))))
	     #'(define-label-type ?type-name
		 (parent (vector FIELD-TYPE0 FIELD-TYPE ...))
		 (constructor ({FIELD-NAME0 FIELD-TYPE0} {FIELD-NAME FIELD-TYPE} ...)
		   (vector FIELD-NAME0 FIELD-NAME ...))
		 (method ({FIELD-NAME0 FIELD-TYPE0})	(cast-signature (FIELD-TYPE0) (vector-ref this 0)))
		 (method ({FIELD-NAME FIELD-TYPE})	(cast-signature (FIELD-TYPE)  (vector-ref this IDX)))
		 ...
		 )))))
      (_
       (__synner__ "invalid syntax in input form"))))

  (define (%parse-field-specs field-spec*)
    (if (pair? field-spec*)
	(syntax-case (car field-spec*) (brace)
	  ((brace ?field-name ?field-type)
	   (identifier? #'?field-name)
	   (receive (field-name* field-type*)
	       (%parse-field-specs (cdr field-spec*))
	     (values (cons #'?field-name field-name*)
		     (cons #'?field-type field-type*))))

	  (?field-name
	   (identifier? #'?field-name)
	   (receive (field-name* field-type*)
	       (%parse-field-specs (cdr field-spec*))
	     (values (cons #'?field-name field-name*)
		     (cons #'<top>       field-type*))))

	  (?stuff
	   (__synner__ "invalid field specification" #'?stuff)))
      (values '() '())))

  (define (%iota start end)
    (if (fx<? start end)
	(cons start (%iota (fxadd1 start) end))
      '()))

  (main stx))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
