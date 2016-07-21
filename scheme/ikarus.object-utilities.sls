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
;;;Copyright (C) 2014, 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus.object-utilities)
  (export
    method-call-late-binding
    hash-function-late-binding
    interface-method-call-late-binding
    internal-delete
    ;; overloaded functions: late binding
    <overloaded-function-descriptor>-rtd
    <overloaded-function-descriptor>-rcd
    make-overloaded-function-descriptor
    overloaded-function-descriptor?
    overloaded-function-descriptor.register!
    overloaded-function-descriptor.select-matching-entry
    overloaded-function-late-binding
    #| end of EXPORT |# )
  (import (except (vicare)
		  method-call-late-binding)
    (ikarus records syntactic)
    (only (ikarus.core-type-descr)
	  core-type-descriptor?
	  core-type-descriptor.parent
	  core-type-descriptor.hash-function
	  core-type-descriptor.method-retriever
	  core-type-descriptor.implemented-interfaces
	  ;;
	  <empty-string>-ctd <nestring>-ctd
	  <empty-vector>-ctd <nevector>-ctd
	  <null>-ctd <nelist>-ctd <pair>-ctd
	  <bytevector>-ctd
	  <fixnum>-ctd <flonum>-ctd <ratnum>-ctd <bignum>-ctd <compnum>-ctd <cflonum>-ctd
	  <textual-input/output-port>-ctd <binary-input/output-port>-ctd <textual-output-port>-ctd
	  <binary-output-port>-ctd <textual-input-port>-ctd <binary-input-port>-ctd
	  <boolean>-ctd <char>-ctd <gensym>-ctd <symbol>-ctd <keyword>-ctd <pointer>-ctd <transcoder>-ctd
	  <void>-ctd <would-block>-ctd <eof>-ctd)
    (only (vicare system $fx)
	  $fxadd1)
    (only (vicare system $structs)
	  $struct-rtd
	  $set-std-printer!)
    (only (vicare system $records)
	  $record-type-method-retriever
	  $record-type-hash-function)
    (prefix (only (ikarus records procedural)
		  record-type-implemented-interfaces)
	    td::)
    (prefix (only (psyntax system $all)
		  internal-applicable-record-destructor)
	    system::)
    (prefix (only (ikarus.object-type-descr)
		  lambda-descriptors?
		  select-most-specific-lambda-descriptors
		  make-descriptors-signature
		  type-descriptor-of)
	    td::))


;;;; helpers for object-type method calls

(define* (method-call-late-binding method-name.sym td subject . args)
  ;;This function is called by the expansion of METHOD-CALL:
  ;;
  ;;   (method-call ?method-name ?subject-expr ?arg ...)
  ;;
  ;;when it  is impossible to  determine type signature of  the return values  of the
  ;;subject expression.   SUBJECT is the  result of  the subject expression;  ARGS is
  ;;null or a list of method arguments.
  ;;
  ;;The argument TD must be false or a predetermined type descriptor for SUBJECT.
  ;;
  (define (%error message)
    (raise
     (condition (make-method-late-binding-error)
		(make-who-condition 'method-call-late-binding)
		(make-message-condition message)
		(make-irritants-condition (list method-name.sym td subject args)))))
  (define (%error-invalid-type-descriptor-argument)
    (%error "invalid object type descriptor argument"))
  (define (%error-object-type-has-no-methods-table)
    (%error "object-type has no methods table"))
  (define (%error-record-type-has-no-matching-method)
    (%error "record-type has no matching method"))
  (define (%error-scheme-type-has-no-matching-method)
    (%error "scheme-type has no matching method"))

  (define (%built-in-scheme-object-call btd)
    (cond (((core-type-descriptor.method-retriever btd) method-name.sym)
	   => (lambda (proc)
		(apply proc subject args)))
	  (else
	   (%error-scheme-type-has-no-matching-method))))

  (define (%struct-object-call std)
    (apply (struct-field-method std method-name.sym) subject args))

  (define (%record-object-call rtd)
    (cond ((($record-type-method-retriever rtd) method-name.sym)
	   => (lambda (proc)
		(apply proc subject args)))
	  (else
	   (%error-record-type-has-no-matching-method))))

  (cond (td
	 ;;The type descriptor is known.
	 (cond ((record-type-descriptor? td)	(%record-object-call          td))
	       ((core-type-descriptor?   td)	(%built-in-scheme-object-call td))
	       ((struct-type-descriptor? td)	(%struct-object-call          td))
	       (else
		(%error-invalid-type-descriptor-argument))))

	((record-object? subject)
	 ;;We use  $STRUCT-RTD because it does  not care about the  opaqueness of the
	 ;;record object.
	 (%record-object-call ($struct-rtd subject)))

	((string?  subject)	(if (string-empty? subject)
				    (%built-in-scheme-object-call <empty-string>-ctd)
				  (%built-in-scheme-object-call <nestring>-ctd)))
	((vector?  subject)	(if (vector-empty? subject)
				    (%built-in-scheme-object-call <empty-vector>-ctd)
				  (%built-in-scheme-object-call <nevector>-ctd)))
	((list?    subject)	(if (pair? subject)
				    (%built-in-scheme-object-call <nelist>-ctd)
				  (%built-in-scheme-object-call <null>-ctd)))
	((pair?    subject)	(%built-in-scheme-object-call <pair>-ctd))
	((bytevector? subject)	(%built-in-scheme-object-call <bytevector>-ctd))

	((fixnum?  subject)	(%built-in-scheme-object-call <fixnum>-ctd))
	((flonum?  subject)	(%built-in-scheme-object-call <flonum>-ctd))
	((ratnum?  subject)	(%built-in-scheme-object-call <ratnum>-ctd))
	((bignum?  subject)	(%built-in-scheme-object-call <bignum>-ctd))
	((compnum? subject)	(%built-in-scheme-object-call <compnum>-ctd))
	((cflonum? subject)	(%built-in-scheme-object-call <cflonum>-ctd))

	((port? subject)
	 (cond ((textual-input/output-port? subject)	(%built-in-scheme-object-call <textual-input/output-port>-ctd))
	       ((binary-input/output-port?  subject)	(%built-in-scheme-object-call <binary-input/output-port>-ctd))
	       ((textual-output-port?       subject)	(%built-in-scheme-object-call <textual-output-port>-ctd))
	       ((binary-output-port?        subject)	(%built-in-scheme-object-call <binary-output-port>-ctd))
	       ((textual-input-port?        subject)	(%built-in-scheme-object-call <textual-input-port>-ctd))
	       ((binary-input-port?         subject)	(%built-in-scheme-object-call <binary-input-port>-ctd))
	       (else
		(%error-object-type-has-no-methods-table))))

	((boolean? subject)	(%built-in-scheme-object-call <boolean>-ctd))
	((char?    subject)	(%built-in-scheme-object-call <char>-ctd))
	((symbol?  subject)
	 (cond ((gensym? subject)
		(%built-in-scheme-object-call <gensym>-ctd))
	       (else
		(%built-in-scheme-object-call <symbol>-ctd))))
	((keyword? subject)	(%built-in-scheme-object-call <keyword>-ctd))
	((pointer? subject)	(%built-in-scheme-object-call <pointer>-ctd))
	((transcoder? subject)	(%built-in-scheme-object-call <transcoder>-ctd))

	((struct? subject)	(%struct-object-call (struct-std subject)))

	((void-object? subject)
	 (%built-in-scheme-object-call <void>-ctd))
	((would-block-object? subject)
	 (%built-in-scheme-object-call <would-block>-ctd))
	((eof-object? subject)
	 (%built-in-scheme-object-call <eof>-ctd))

	(else
	 (%error-object-type-has-no-methods-table))))


(define (hash-function-late-binding subject)
  (define (%error)
    (raise
     (condition (make-method-late-binding-error)
		(make-who-condition 'hash-function-late-binding)
		(make-message-condition "object-type has no hash function")
		(make-irritants-condition (list subject)))))

  (define (%built-in-scheme-object-call btd)
    (cond ((core-type-descriptor.hash-function btd)
	   => (lambda (proc)
		(proc subject)))
	  (else
	   (object-hash subject))))

  (cond ((record-object? subject)
	 ;;We use  $STRUCT-RTD because it does  not care about the  opaqueness of the
	 ;;record object.
	 (cond (($record-type-hash-function ($struct-rtd subject))
		=> (lambda (fun)
		     (fun subject)))
	       (else
		(record-hash subject))))

	((string?  subject)	(if (string-empty? subject)
				    (%built-in-scheme-object-call <empty-string>-ctd)
				  (%built-in-scheme-object-call <nestring>-ctd)))
	((vector?  subject)	(if (vector-empty? subject)
				    (%built-in-scheme-object-call <empty-vector>-ctd)
				  (%built-in-scheme-object-call <nevector>-ctd)))
	((list?    subject)	(if (pair? subject)
				    (%built-in-scheme-object-call <nelist>-ctd)
				  (%built-in-scheme-object-call <null>-ctd)))
	((pair?    subject)	(%built-in-scheme-object-call <pair>-ctd))
	((bytevector? subject)	(%built-in-scheme-object-call <bytevector>-ctd))

	((fixnum?  subject)	(%built-in-scheme-object-call <fixnum>-ctd))
	((flonum?  subject)	(%built-in-scheme-object-call <flonum>-ctd))
	((ratnum?  subject)	(%built-in-scheme-object-call <ratnum>-ctd))
	((bignum?  subject)	(%built-in-scheme-object-call <bignum>-ctd))
	((compnum? subject)	(%built-in-scheme-object-call <compnum>-ctd))
	((cflonum? subject)	(%built-in-scheme-object-call <cflonum>-ctd))

	((port? subject)
	 (cond ((textual-input/output-port? subject)	(%built-in-scheme-object-call <textual-input/output-port>-ctd))
	       ((binary-input/output-port?  subject)	(%built-in-scheme-object-call <binary-input/output-port>-ctd))
	       ((textual-output-port?       subject)	(%built-in-scheme-object-call <textual-output-port>-ctd))
	       ((binary-output-port?        subject)	(%built-in-scheme-object-call <binary-output-port>-ctd))
	       ((textual-input-port?        subject)	(%built-in-scheme-object-call <textual-input-port>-ctd))
	       ((binary-input-port?         subject)	(%built-in-scheme-object-call <binary-input-port>-ctd))
	       (else
		(%error))))

	((boolean? subject)	(%built-in-scheme-object-call <boolean>-ctd))
	((char?    subject)	(%built-in-scheme-object-call <char>-ctd))
	((symbol?  subject)
	 (cond ((gensym? subject)
		(%built-in-scheme-object-call <gensym>-ctd))
	       (else
		(%built-in-scheme-object-call <symbol>-ctd))))
	((keyword? subject)	(%built-in-scheme-object-call <keyword>-ctd))
	((pointer? subject)	(%built-in-scheme-object-call <pointer>-ctd))
	((transcoder? subject)	(%built-in-scheme-object-call <transcoder>-ctd))

	((struct? subject)
	 (struct-hash subject))

	((void-object? subject)
	 (%built-in-scheme-object-call <void>-ctd))
	((would-block-object? subject)
	 (%built-in-scheme-object-call <would-block>-ctd))
	((eof-object? subject)
	 (%built-in-scheme-object-call <eof>-ctd))

	(else
	 (%error))))


;;;; run-time utilities: run-time interface method application

(module (interface-method-call-late-binding)

  (define* (interface-method-call-late-binding interface.uid method-name.sym default-implementation subject operands)
    ;;Implement run-time dynamic dispatching of method calls to interface types.
    ;;
    ;;The argument INTERFACE.UID must be the UID of the interface-type.  The argument
    ;;METHOD-NAME.SYM must  be a  symbol representing  the name  of the  method.  The
    ;;argument SUBJECT must be the subject of the method call, the value that will be
    ;;bound to THIS.  The argument OPERANDS must  be a list of additional operands to
    ;;be appended to the call after SUBJECT.
    ;;
    (let ((des (td::type-descriptor-of subject)))
      (define (%error-no-interfaces)
	(%error "the subject's object-type does not implement interfaces"
		interface.uid method-name.sym subject des operands))
      (%method-call des (or (cond ((record-type-descriptor? des)
				   (td::record-type-implemented-interfaces des))
				  ((core-type-descriptor? des)
				   (core-type-descriptor.implemented-interfaces des))
				  (else
				   (%error-no-interfaces)))
			    (%error-no-interfaces))
		    interface.uid method-name.sym default-implementation
		    subject operands)))

;;; --------------------------------------------------------------------

  (define (%method-call des table interface.uid method-name.sym default-implementation subject operands)
    (cond ((vector-find (lambda (entry)
			  (eq? (car entry) interface.uid))
	     table)
	   => (lambda (table-entry)
		;;TABLE-ENTRY is a pair having: as  car, the interface UID; as cdr, a
		;;method retriever procedure.
		(cond (((cdr table-entry) method-name.sym)
		       => (lambda (method-implementation)
			    ;;Method found.   Apply it  to the operands  and return
			    ;;the application's return values.
			    (apply method-implementation subject operands)))
		      (default-implementation
			(apply default-implementation subject operands))
		      (else
		       (%error "the subject's object-type descriptor does not implement the requested interface method"
			       interface.uid method-name.sym subject des operands)))))
	  (else
	   (%error "the subject's object-type descriptor does not implement the requested interface"
		   interface.uid method-name.sym subject des operands))))

;;; --------------------------------------------------------------------

  (define (%error message interface.uid method-name.sym subject des operands)
    (raise
     (condition (make-interface-method-late-binding-error interface.uid method-name.sym subject des)
		(make-who-condition 'interface-method-call-late-binding)
		(make-message-condition message)
		(make-irritants-condition operands))))

  #| end of module: INTERFACE-METHOD-CALL-LATE-BINDING |# )


;;;; delete fallback implementation

(define (internal-delete obj)
  ;;This is  the fallback  implementation of  the DELETE  syntax.  When  the expander
  ;;cannot determine at expand-time the type of an object: this function is called to
  ;;recognise it and apply to it the appropriate destructor function.
  ;;
  ;;This  is not  a public  syntactic binding:  it is  exported only  by the  library
  ;;"(psyntax system $all)".
  ;;
  (cond ((record-object? obj)
	 ((system::internal-applicable-record-destructor obj) obj))
	((struct? obj)
	 ((struct-destructor obj) obj))
	((port? obj)
	 (close-port obj))
	(else
	 (assertion-violation 'delete
	   "unknown method to destroy object" obj))))


;;;; overloaded functions: late binding

(define-core-record-type <overloaded-function-descriptor>
  (nongenerative vicare:system:<overloaded-function-descriptor>)
  (sealed #t)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (fields
    (immutable	name		overloaded-function-descriptor.name)
		;Symbol representing the overloaded function name.
    (mutable	table		overloaded-function-descriptor.table overloaded-function-descriptor.table-set!)
		;An alist  having an  instance of <closure-type-descr>  as key  and a
		;procedure as value.
    #| end of FIELDS |# ))

;;; --------------------------------------------------------------------

(define* (overloaded-function-descriptor.register! {over.des		overloaded-function-descriptor?}
						   {lambda.des		td::lambda-descriptors?}
						   {implementation	procedure?})
  (overloaded-function-descriptor.table-set! over.des
					     (cons (cons lambda.des implementation)
						   (overloaded-function-descriptor.table over.des))))

(define* (overloaded-function-descriptor.select-matching-entry {over.des overloaded-function-descriptor?} operand*)
  (let ((rands.sig	(td::make-descriptors-signature (map td::type-descriptor-of operand*)))
	(closure-entry*	(overloaded-function-descriptor.table over.des)))
    ;;CLOSURE-ENTRY* is an alist having instances of <lambda-descriptors> as keys and
    ;;procedures as  values.  RANDS.SIG  is an instance  of "<descriptors-signature>"
    ;;representing the  types of the operands.   The return value is  the alist entry
    ;;which is the  most specific matching for the operands.   If no matching closure
    ;;is found: return false.
    (td::select-most-specific-lambda-descriptors closure-entry* rands.sig)))

(define* (overloaded-function-late-binding {over.des overloaded-function-descriptor?} . operand*)
  (cond ((overloaded-function-descriptor.select-matching-entry over.des operand*)
	 => (lambda (entry)
	      (apply (cdr entry) operand*)))
	(else
	 (raise
	  (condition (make-who-condition __who__)
		     (make-message-condition "no function matching the operands in overloaded function application")
		     (make-overloaded-function-late-binding-error over.des)
		     (make-irritants-condition operand*))))))


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.object-utilities")))

#| end of library |# )

;;; end of file
