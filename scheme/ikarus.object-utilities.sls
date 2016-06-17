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
		  method-call-late-binding
		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Tue Dec 15, 2015)
		  struct-field-method
		  struct-std
		  record-type-method-retriever
		  record-type-hash-function
		  make-method-late-binding-error
		  make-overloaded-function-late-binding-error
		  #| end of EXCEPT |# )
    (only (ikarus.core-type-descr)
	  core-type-descriptor.parent
	  core-type-descriptor.hash-function
	  core-type-descriptor.method-retriever
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
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Jun 17,
    ;;2016)
    (only (ikarus conditions)
	  make-method-late-binding-error
	  make-overloaded-function-late-binding-error)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (prefix (only (ikarus structs)
		  struct-field-method
		  struct-std)
	    structs::)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (prefix (only (ikarus records procedural)
		  record-type-method-retriever
		  record-type-hash-function)
	    system::)
    (prefix (only (psyntax system $all)
		  internal-applicable-record-destructor
		  ;;FIXME To be uncommented at  the next boot image rotation.  (Marco
		  ;;Maggi; Tue Dec 15, 2015)
		  #;record-type-method-retriever)
	    system::)
    (prefix (only (ikarus.object-type-descr)
		  closure-type-descr?
		  closure-type-descr.signature
		  case-lambda-descriptors.match-super-and-sub
		  case-lambda-descriptors.match-formals-against-operands
		  make-descriptors-signature
		  type-descriptor-of)
	    td::))


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
    (raise
     (condition (make-method-late-binding-error)
		(make-who-condition 'method-call-late-binding)
		(make-message-condition message)
		(make-irritants-condition (list method-name.sym subject args)))))
  (define (%error-object-type-has-no-methods-table)
    (%error "object type has no methods table"))
  (define (%error-record-type-has-no-matching-method)
    (%error "record type has no matching method"))
  (define (%error-scheme-type-has-no-matching-method)
    (%error "scheme type has no matching method"))

  (define (%apply-hash-function hash-func)
    (if (null? args)
	(hash-func subject)
      (procedure-argument-violation 'method-call-late-binding
	"hash function requires a single operand" (cons subject args))))

  (define (%built-in-scheme-object-call btd)
    (case method-name.sym
      ((hash)
       (let loop ((btd btd))
	 (cond ((core-type-descriptor.hash-function btd)
		=> %apply-hash-function)
	       ((core-type-descriptor.parent btd)
		=> loop)
	       (else
		(%apply-hash-function object-hash)))))
      (else
       (let loop ((btd btd))
	 (cond ((core-type-descriptor.method-retriever btd)
		=> (lambda (method-retriever)
		     (cond ((method-retriever method-name.sym)
			    => (lambda (proc)
				 (apply proc subject args)))
			   ((core-type-descriptor.parent btd)
			    => loop)
			   (else
			    (%error-scheme-type-has-no-matching-method)))))
	       ((core-type-descriptor.parent btd)
		=> loop)
	       (else
		(%error-scheme-type-has-no-matching-method)))))))

  (define (%struct-object-call std)
    (case method-name.sym
      ((hash)
       (%apply-hash-function struct-hash))
      (else
       (apply (structs::struct-field-method std method-name.sym) subject args))))

  (define (%record-object-call rtd)
    (case method-name.sym
      ((hash)
       (let loop ((rtd rtd))
	 (cond ((system::record-type-hash-function rtd)
		=> %apply-hash-function)
	       ((record-type-parent rtd)
		=> loop)
	       (else
		(%apply-hash-function record-hash)))))
      (else
       (let loop ((rtd rtd))
	 (cond ((system::record-type-method-retriever rtd)
		=> (lambda (method-retriever)
		     (cond ((method-retriever method-name.sym)
			    => (lambda (proc)
				 (apply proc subject args)))
			   ((record-type-parent rtd)
			    => loop)
			   (else
			    (%error-record-type-has-no-matching-method)))))
	       ((record-type-parent rtd)
		=> loop)
	       (else
		(%error-record-type-has-no-matching-method)))))))

  (cond ((record-object? subject)
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

	((struct? subject)	(%struct-object-call (structs::struct-std subject)))

	((eq? subject (void))
	 (%built-in-scheme-object-call <void>-ctd))
	((eq? subject (would-block-object))
	 (%built-in-scheme-object-call <would-block>-ctd))
	((eq? subject (eof-object))
	 (%built-in-scheme-object-call <eof>-ctd))

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

(define-record-type (<overloaded-function-descriptor> make-overloaded-function-descriptor overloaded-function-descriptor?)
  (nongenerative vicare:system:<overloaded-function-descriptor>)
  (sealed #t)
  (fields
    (mutable table		overloaded-function-descriptor.table overloaded-function-descriptor.table-set!)
		;An alist  having an  instance of <closure-type-descr>  as key  and a
		;procedure as value.
    #| end of FIELDS |# ))

(define <overloaded-function-descriptor>-rtd
  (record-type-descriptor <overloaded-function-descriptor>))

(define <overloaded-function-descriptor>-rcd
  (record-constructor-descriptor <overloaded-function-descriptor>))

;;; --------------------------------------------------------------------

(define* (overloaded-function-descriptor.register! {over.des		overloaded-function-descriptor?}
						   {closure.des		td::closure-type-descr?}
						   {implementation	procedure?})
  (overloaded-function-descriptor.table-set! over.des
					     (cons (cons closure.des implementation)
						   (overloaded-function-descriptor.table over.des))))

(define* (overloaded-function-descriptor.select-matching-entry {over.des overloaded-function-descriptor?} operand*)
  (let ((rands.sig (td::make-descriptors-signature (map td::type-descriptor-of operand*))))
    (fold-left
	(lambda (selected-entry entry)
	  ;;ENTRY is  a pair having  an instance of  <closure-type-descr> as
	  ;;car and a  procedure as cdr.  SELECTED-ENTRY is false  or a pair
	  ;;with the same format of ENTRY.
	  (let ((clambda.des (td::closure-type-descr.signature (car entry))))
	    (if (eq? 'exact-match (td::case-lambda-descriptors.match-formals-against-operands clambda.des rands.sig))
		(if selected-entry
		    (if (eq? 'exact-match (td::case-lambda-descriptors.match-super-and-sub
					   (td::closure-type-descr.signature (car selected-entry))
					   clambda.des))
			entry
		      selected-entry)
		  entry)
	      selected-entry)))
      #f (overloaded-function-descriptor.table over.des))))

(define* (overloaded-function-late-binding {over.des overloaded-function-descriptor?} . operand*)
  (cond ((overloaded-function-descriptor.select-matching-entry over.des operand*)
	 => (lambda (entry)
	      (apply (cdr entry) operand*)))
	(else
	 (raise
	  (condition (make-overloaded-function-late-binding-error)
		     (make-who-condition __who__)
		     (make-message-condition "no function matching the operands in overloaded function application")
		     (make-irritants-condition (list over.des operand*)))))))


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.object-utilities")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'define-scheme-type		'scheme-indent-function 2)
;; End:
