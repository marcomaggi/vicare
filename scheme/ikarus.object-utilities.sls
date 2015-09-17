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

    ;; validation
    procedure-argument-validation-with-predicate
    return-value-validation-with-predicate
    signature-rest-argument-validation-with-predicate

    ;; built-in object-type specification utilities, for internal use
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
  (define (%error-scheme-type-has-no-matching-method)
    (%error "scheme type has no matching method"))

  (define (%built-in-scheme-object-call btd)
    (define (%recurse)
      (%built-in-scheme-object-call (scheme-type-parent btd)))
    (if btd
	(cond ((scheme-type-method-retriever btd)
	       => (lambda (method-retriever)
		    (cond ((method-retriever method-name.sym)
			   => (lambda (proc)
				(apply proc subject args)))
			  (else
			   (%recurse)))))
	      (else
	       (%recurse)))
      (%error-scheme-type-has-no-matching-method)))

  (define (%record-object-call rtd)
    ;;Here we expect the record-type descriptor to have a symbol as UID: the property
    ;;list    of    the    symbol    should     contain    an    entry    with    key
    ;;"late-binding-methods-table";  the  value of  the  entry  must be  a  hashtable
    ;;associating the method name to the implementation procedure.
    ;;
    (define (%recurse)
      (%record-object-call (record-type-parent rtd)))
    (if rtd
	(cond ((record-type-uid rtd)
	       => (lambda (uid)
		    (cond ((getprop uid 'late-binding-methods-table)
			   => (lambda (table)
				(cond ((hashtable-ref table method-name.sym #f)
				       => (lambda (proc)
					    (apply proc subject args)))
				      (else
				       (%recurse)))))
			  (else
			   (%recurse)))))
	      (else
	       (%recurse)))
      (%error-record-type-has-no-matching-method)))

  (cond ((records.record-object? subject)
	 (%record-object-call (record-rtd subject)))

	((pair? subject)
	 (%built-in-scheme-object-call <pair>-type-descriptor))

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
	((port? obj)
	 (close-port obj))
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

(define (signature-rest-argument-validation-with-predicate who arg-counter pred pred-sexp arg*)
  ;;This is used by LAMBDA* and similar syntaxes to validate rest and args arguments.
  ;;
  ;;Let's say we have:
  ;;
  ;;   (define* (doit a . {rest fixnum?})
  ;;     ---)
  ;;
  ;;   (doit 1 2 3 4)
  ;;
  ;;we know that REST will be a proper list (possibly null), so we only need to check
  ;;that every item  in REST is a fixnum,  this is what this function  does; for this
  ;;example this function must be called as:
  ;;
  ;;   (signature-rest-argument-validation-with-predicate 'doit 2 fixnum? 'fixnum? rest)
  ;;
  ;;notice that the argument counter is 1-based.
  ;;
  ;;Let's say we have:
  ;;
  ;;   (define* (doit . {args fixnum?})
  ;;     ---)
  ;;
  ;;   (doit 1 2 3 4)
  ;;
  ;;we know that ARGS will be a proper list (possibly null), so we only need to check
  ;;that every item  in ARGS is a fixnum,  this is what this function  does; for this
  ;;example this function must be called as:
  ;;
  ;;   (signature-rest-argument-validation-with-predicate 'doit 1 fixnum? 'fixnum? args)
  ;;
  ;;notice that the argument counter is 1-based.
  ;;
  (when (pair? arg*)
    (if (pred (car arg*))
	(signature-rest-argument-validation-with-predicate who ($fxadd1 arg-counter) pred pred-sexp (cdr arg*))
      (conditions.procedure-signature-argument-violation who
	"failed argument validation"
	arg-counter pred-sexp (car arg*)))))


;;;; object type validation

(define* (procedure-argument-validation-with-predicate {type-name symbol?} {pred procedure?} obj)
  ;;Validate  OBJ as  Scheme object  satisfying  the predicate  PRED.  If  successful
  ;;return OBJ  itself, otherwise raise  an exception with compound  condition object
  ;;type "&procedure-argument-violation".
  ;;
  ;;This function is used  as tag validator by the tagged  language.  It validates an
  ;;object type as  belonging to a tag  specification; it must raise  an exception or
  ;;just return the object  itself.  TYPE-NAME is typically the symbol  name of a tag
  ;;identifier.  PRED is typically the predicate function from the "tag-type-spec" of
  ;;a tag identifier.
  ;;
  (if (pred obj)
      obj
    (procedure-argument-violation type-name "invalid object type" obj)))

(define* (return-value-validation-with-predicate {type-name symbol?} {pred procedure?} obj)
  ;;Validate  OBJ as  Scheme object  satisfying  the predicate  PRED.  If  successful
  ;;return OBJ  itself, otherwise raise  an exception with compound  condition object
  ;;type "&expression-return-value-violation".
  ;;
  ;;This function is used  as tag validator by the tagged  language.  It validates an
  ;;object type as  belonging to a tag  specification; it must raise  an exception or
  ;;just return the object  itself.  TYPE-NAME is typically the symbol  name of a tag
  ;;identifier.  PRED is typically the predicate function from the "tag-type-spec" of
  ;;a tag identifier.
  ;;
  (if (pred obj)
      obj
    (expression-return-value-violation type-name "invalid object type" obj)))


;;;; built-in object-type specification utilities

;;Instances of this type are used at  run-time to describe the built-in Scheme object
;;types: pairs, fixnums, strings, et cetera.  Lexical variables bound to instances of
;;this type should be called BTD (as in "built-in type descriptor").
;;
(define-struct scheme-type
  (parent
		;False  if  this  type  has  no  parent;  otherwise  an  instance  of
		;"scheme-type" representing the parent of this type.
   uid
		;A symbol representing the unique identifier for this type.
   uids-list
		;A list of symbols representing the  hierarchy of UIDs for this type.
		;The  first item  in the  list  is the  UID  of this  type, then  the
		;parent's UID, then the grandparent's UID, et cetera.
   method-retriever
		;If this type has methods: the  property list of the UID must contain
		;an  entry with  key "late-binding-methods-table";  the value  of the
		;entry must be  an EQ?  hashtable associating the method  name to the
		;implementation procedure.
   ))

(define-syntax define-scheme-type
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?type-name ?parent-name (?method-name ?method-implementation-procedure) ...)
       (let ((type-name.str (symbol->string (syntax->datum #'?type-name))))
	 (define (%datum->syntax obj)
	   (datum->syntax #'?type-name obj))
	 (define (%mk-btd-name type.id)
	   (%datum->syntax (string->symbol (string-append (symbol->string (syntax->datum type.id))
							  "-type-descriptor"))))
	 (with-syntax
	     ((BTD-NAME		(%mk-btd-name #'?type-name))
	      (PARENT-NAME	(%mk-btd-name #'?parent-name))
	      (UID		(%datum->syntax (string->symbol (string-append "vicare:scheme-type:" type-name.str)))))
	   #'(define BTD-NAME
	       (make-scheme-type PARENT-NAME (quote UID)
				 (%build-scheme-type-uids-list (quote UID) PARENT-NAME)
				 (lambda (method-name.sym)
				   (case method-name.sym
				     ((?method-name) ?method-implementation-procedure)
				     ...))))
	   )))
      )))

(define (%build-scheme-type-uids-list this-uid parent-btd)
  (cons this-uid
	(if parent-btd
	    (%build-scheme-type-uids-list (scheme-type-uid    parent-btd)
					  (scheme-type-parent parent-btd))
	  '())))

;;; --------------------------------------------------------------------
;;; <top> helpers

(define (<top>-type-predicate obj)
  #t)

;;; --------------------------------------------------------------------
;;; built-in Scheme objects type descriptors

(define-constant <top>-type-descriptor
  (make-scheme-type #f 'vicare:scheme-type:<top> '() #f))

(define-scheme-type <pair>
    <top>
  (car car)
  (cdr cdr))


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.object-utilities")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'conditions.procedure-signature-argument-violation 'scheme-indent-function 1)
;; eval: (put 'define-scheme-type		'scheme-indent-function 2)
;; End:
