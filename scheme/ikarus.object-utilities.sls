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
    any->symbol				any->string

    ;; predicates
    always-true				always-false

    ;; validation
    procedure-argument-validation-with-predicate
    return-value-validation-with-predicate
    signature-rest-argument-validation-with-predicate

    ;; built-in object-type specification utilities, for internal use
    <top>-type-predicate
    <symbol>-value)
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
		  internal-applicable-record-destructor
		  record-type-method-retriever)
	    records.)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Sep 18,
    ;;2015)
    (prefix (only (ikarus.keywords)
		  keyword-hash
		  keyword->string)
	    keywords.)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Sep 25,
    ;;2015)
    (prefix (only (ikarus structs)
		  struct-field-method
		  struct-std)
	    structs.)
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
		    (cond ((method-retriever btd method-name.sym)
			   => (lambda (proc)
				(apply proc subject args)))
			  (else
			   (%recurse)))))
	      (else
	       (%recurse)))
      (%error-scheme-type-has-no-matching-method)))

  (define (%struct-object-call std)
    (apply (structs.struct-field-method std method-name.sym) subject args))

  (define (%record-object-call rtd)
    (define (%recurse)
      (%record-object-call (record-type-parent rtd)))
    (if rtd
	(cond ((records.record-type-method-retriever rtd)
	       => (lambda (method-retriever)
		    (cond ((method-retriever rtd method-name.sym)
			   => (lambda (proc)
				(apply proc subject args)))
			  (else
			   (%recurse)))))
	      (else
	       (%recurse)))
      (%error-record-type-has-no-matching-method)))

  (cond ((records.record-object? subject)
	 (%record-object-call (record-rtd subject)))

	((string?  subject)	(%built-in-scheme-object-call <string>-type-descriptor))
	((vector?  subject)	(%built-in-scheme-object-call <vector>-type-descriptor))
	((list?    subject)	(%built-in-scheme-object-call <list>-type-descriptor))
	((pair?    subject)	(%built-in-scheme-object-call <pair>-type-descriptor))
	((bytevector? subject)	(%built-in-scheme-object-call <bytevector>-type-descriptor))

	((fixnum?  subject)	(%built-in-scheme-object-call <fixnum>-type-descriptor))
	((flonum?  subject)	(%built-in-scheme-object-call <flonum>-type-descriptor))
	((ratnum?  subject)	(%built-in-scheme-object-call <ratnum>-type-descriptor))
	((bignum?  subject)	(%built-in-scheme-object-call <bignum>-type-descriptor))
	((compnum? subject)	(%built-in-scheme-object-call <compnum>-type-descriptor))
	((cflonum? subject)	(%built-in-scheme-object-call <cflonum>-type-descriptor))

	((port? subject)
	 (cond ((textual-input/output-port? subject)	(%built-in-scheme-object-call <textual-input/output-port>-type-descriptor))
	       ((binary-input/output-port?  subject)	(%built-in-scheme-object-call <binary-input/output-port>-type-descriptor))
	       ((textual-output-port?       subject)	(%built-in-scheme-object-call <textual-output-port>-type-descriptor))
	       ((binary-output-port?        subject)	(%built-in-scheme-object-call <binary-output-port>-type-descriptor))
	       ((textual-input-port?        subject)	(%built-in-scheme-object-call <textual-input-port>-type-descriptor))
	       ((binary-input-port?         subject)	(%built-in-scheme-object-call <binary-input-port>-type-descriptor))
	       (else
		(%error-object-type-has-no-methods-table))))

	((boolean? subject)	(%built-in-scheme-object-call <boolean>-type-descriptor))
	((char?    subject)	(%built-in-scheme-object-call <char>-type-descriptor))
	((symbol?  subject)	(%built-in-scheme-object-call <symbol>-type-descriptor))
	((keyword? subject)	(%built-in-scheme-object-call <keyword>-type-descriptor))

	((struct? subject)	(%struct-object-call (structs.struct-std subject)))

	((eq? subject (void))	(%built-in-scheme-object-call <void>-type-descriptor))

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
		;If this  type has methods: a  procedure to be applied  to the method
		;name  to retriever  the  method  implementation function;  otherwise
		;false.
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
	      (UID		(%datum->syntax (string->symbol (string-append "vicare:scheme-type:" type-name.str))))
	      (RETRIEVER	(if (null? (syntax->datum '((?method-name ?method-implementation-procedure) ...)))
				    #f
				  #'(lambda (btd method-name.sym)
				      (case method-name.sym
					((?method-name) ?method-implementation-procedure)
					...
					(else #f))))))
	   #'(define BTD-NAME
	       (make-scheme-type PARENT-NAME (quote UID)
				 (%build-scheme-type-uids-list (quote UID) PARENT-NAME)
				 RETRIEVER))
	   )))
      )))

(define (%build-scheme-type-uids-list this-uid parent-btd)
  (cons this-uid
	(if parent-btd
	    (%build-scheme-type-uids-list (scheme-type-uid    parent-btd)
					  (scheme-type-parent parent-btd))
	  '())))

;;; --------------------------------------------------------------------
;;; type helpers

(define (<top>-type-predicate obj)
  #t)

(case-define <symbol>-value
  ((sym)
   (symbol-value sym))
  ((sym val)
   (set-symbol-value! sym val)))

;;; --------------------------------------------------------------------
;;; built-in Scheme objects type descriptors

(define-constant <top>-type-descriptor
  (make-scheme-type #f 'vicare:scheme-type:<top> '() #f))

;;; immediate object types

(define-scheme-type <boolean>
    <top>)

(define-scheme-type <void>
    <top>)

(define-scheme-type <char>
    <top>)

(define-scheme-type <symbol>
    <top>
  (string		symbol->string)
  (hash			symbol-hash)
  (bound?		symbol-bound?)
  (value		<symbol>-value)
  (putprop		putprop)
  (getprop		getprop)
  (remprop		remprop)
  (property-list	property-list))

(define-scheme-type <keyword>
    <top>
  (symbol		keyword->symbol)
  (string		keywords.keyword->string)
  (hash			keywords.keyword-hash))

(define-scheme-type <pointer>
    <top>
  (null?		pointer-null?)
  (integer		pointer->integer)
  (=			pointer=?)
  (!=			pointer!=?)
  (<			pointer<?)
  (>			pointer>?)
  (<=			pointer<=?)
  (>=			pointer>=?)
  (add			pointer-add)
  (diff			pointer-diff)
  (clone		pointer-clone)
  (set-null!		set-pointer-null!))

(define-scheme-type <transcoder>
    <top>)

;;; procedures

(define-scheme-type <procedure>
    <top>)

(define-scheme-type <predicate>
    <procedure>)

;;; numeric types

(define-scheme-type <number>
    <top>)

(define-scheme-type <complex>
    <number>)

(define-scheme-type <real-valued>
    <complex>)

(define-scheme-type <real>
    <real-valued>)

(define-scheme-type <rational-valued>
    <real>)

(define-scheme-type <rational>
    <rational-valued>)

;;This "<integer-valued>" is a bit orphan: it is excluded from the hierarchy.
;;
(define-scheme-type <integer-valued>
    <rational-valued>)

;;Notice that "<integer>" is a "<rational>", not an "<integer-valued>".
;;
(define-scheme-type <integer>
    <rational>)

(define-scheme-type <exact-integer>
    <integer>)

(define-scheme-type <fixnum>
    <exact-integer>)

(define-scheme-type <flonum>
    <real-valued>)

(define-scheme-type <ratnum>
    <rational>)

(define-scheme-type <bignum>
    <exact-integer>)

(define-scheme-type <compnum>
    <complex>)

(define-scheme-type <cflonum>
    <complex>)

;;; compound types

(define-scheme-type <string>
    <top>)

(define-scheme-type <vector>
    <top>)

(define-scheme-type <pair>
    <top>
  (car car)
  (cdr cdr))

(define-scheme-type <list>
    <top>)

(define-scheme-type <bytevector>
    <top>)

(define-scheme-type <hashtable>
    <top>)

(define-scheme-type <hashtable-eq>
    <hashtable>)

(define-scheme-type <hashtable-eqv>
    <hashtable>)

(define-scheme-type <hashtable-equal>
    <hashtable>)

;;; records and structs

(define-scheme-type <struct>
    <top>)

(define-scheme-type <struct-type-descriptor>
    <struct>)

(define-scheme-type <record>
    <struct>)

(define-scheme-type <record-type-descriptor>
    <struct>)

(define-scheme-type <record-constructor-descriptor>
    <struct>)

(define-scheme-type <condition>
    <record>
  (print	print-condition))

(define-scheme-type <compound-condition>
    <condition>)

;;; input/output ports

(define-scheme-type <port>
    <top>)

(define-scheme-type <input-port>
    <port>)

(define-scheme-type <output-port>
    <port>)

(define-scheme-type <input/output-port>
    <port>)

(define-scheme-type <textual-port>
    <port>)

(define-scheme-type <binary-port>
    <port>)

(define-scheme-type <textual-input-port>
    <input-port>)

(define-scheme-type <textual-output-port>
    <output-port>)

(define-scheme-type <textual-input/output-port>
    <input/output-port>)

(define-scheme-type <binary-input-port>
    <input-port>)

(define-scheme-type <binary-output-port>
    <output-port>)

(define-scheme-type <binary-input/output-port>
    <input/output-port>)


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
