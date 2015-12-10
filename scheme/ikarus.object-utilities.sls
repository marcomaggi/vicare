;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: utility functions for built-in Scheme objects
;;;Date: Sat Mar 29, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus.object-utilities)
  (export
    ;; type methods
    method-call-late-binding

    ;; delete fallback
    internal-delete

    ;; conversion
    any->symbol			any->string

    ;; predicates
    always-true			always-false

    ;; built-in object-type specification utilities, for internal use
    <top>-constructor
    <top>-type-predicate)
  (import (except (vicare)
		  method-call-late-binding
		  any->symbol		any->string
		  always-true		always-false
		  procedure-and-error
		  procedure-argument-validation-with-predicate
		  return-value-validation-with-predicate)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Fri May 8,
    ;;2015)
    (prefix (only (ikarus conditions)
		  procedure-signature-argument-violation)
	    conditions.)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Sat Sep 5,
    ;;2015)
    (prefix (only (ikarus records procedural)
		  record-object?
		  record-destructor
		  internal-applicable-record-destructor)
	    records.)
    (only (vicare system $fx)
	  $fxadd1))


;;;; helpers for object-type method calls

(define* (method-call-late-binding method-name.sym subject . args)
  ;;This function is called by the expansion of METHOD-CALL:
  ;;
  ;;   (method-call ?method-name ?subject-expr ?arg ...)
  ;;
  ;;when it  is impossible to  determine type signature of  the return values  of the
  ;;subject expression.   SUBJECT is the  result of  the subject expression;  ARGS is
  ;;null or a list of method arguments.
  ;;
  (define (%error message)
    (error __who__ message method-name.sym subject args))
  (define (%error-object-type-has-no-methods-table)
    (%error "object type has no methods table"))
  (define (%error-record-type-has-no-matching-method)
    (%error "record type has no matching method"))
  (cond ((records.record-object? subject)
	 ;;Here we  expect the record-type  descriptor to have  a symbol as  UID: the
	 ;;property  list   of  the   symbol  should  contain   an  entry   with  key
	 ;;"late-binding-methods-table"; the value  of the entry must  be a hashtable
	 ;;associating the method name to the implementation procedure.
	 (let loop ((rtd (record-rtd subject)))
	   (if rtd
	       (cond ((record-type-uid rtd)
		      => (lambda (uid)
			   (cond ((getprop uid 'late-binding-methods-table)
				  => (lambda (table)
				       (cond ((hashtable-ref table method-name.sym #f)
					      => (lambda (proc)
						   (apply proc subject args)))
					     (else
					      (loop (record-type-parent rtd))))))
				 (else
				  (loop (record-type-parent rtd))))))
		     (else
		      (loop (record-type-parent rtd))))
	     (%error-record-type-has-no-matching-method))))
	(else
	 (%error-object-type-has-no-methods-table))))


;;;; delete fallback implementation

(define (internal-delete obj)
  ;;This is  the fallback  implementation of  the DELETE  syntax.  When  the expander
  ;;cannot determine at expand-time the type of an object: this function is called to
  ;;recognise it and apply to it the appropriate destructor function.
  ;;
  ;;This  is not  a public  syntactic binding:  it is  exported only  by the  library
  ;;"(psyntax system $all)".
  ;;
  (cond ((records.record-object? obj)
	 ((records.internal-applicable-record-destructor obj) obj))
	((struct? obj)
	 ((struct-destructor obj) obj))
	(else
	 (assertion-violation 'delete
	   "unknown method to destroy object" obj))))


;;;; conversion

(define* (any->symbol obj)
  (cond ((string? obj)
	 (string->symbol obj))
	((symbol? obj)
	 obj)
	(else
	 (procedure-argument-violation __who__
	   "invalid source object type for conversion" obj))))

(define* (any->string obj)
  (cond ((symbol? obj)
	 (symbol->string obj))
	((number? obj)
	 (number->string obj))
	(else
	 (procedure-argument-violation __who__
	   "invalid source object type for conversion" obj))))


;;;; predicates

(define (always-true . args)
  #t)

(define (always-false . args)
  #f)


;;;; built-in object-type specification utilities

(define (<top>-constructor . args)
  (apply assertion-violation '<top> "no constructor defined for this object-type" args))

(define (<top>-type-predicate obj)
  #t)

;;; --------------------------------------------------------------------


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.object-utilities")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'conditions.procedure-signature-argument-violation 'scheme-indent-function 1)
;; End:
