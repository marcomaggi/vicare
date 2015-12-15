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

    ;; built-in object-type specification utilities, for internal use
    <top>-constructor			<top>-type-predicate
    <boolean>-constructor		<null>-constructor
    <symbol>-value			<string>-for-each
    <vector>-map			<vector>-for-each
    <vector>-for-all			<vector>-exists
    <vector>-find
    <vector>-fold-right			<vector>-fold-left
    <vector>-sort			<vector>-sort!
    #| end of EXPORT |# )
  (import (except (vicare)
		  method-call-late-binding
		  any->symbol		any->string
		  always-true		always-false

		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Tue Dec 15, 2015)
		  keyword-hash		pointer-hash
		  string->keyword)
    (only (vicare system $fx)
	  $fxadd1)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (prefix (only (ikarus structs)
		  struct-field-method
		  struct-std)
	    structs::)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (prefix (only (ikarus lists)
		  nlist?)
	    lists::)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (only (ikarus.pointers)
	  pointer-hash)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Sep 18,
    ;;2015)
    (only (ikarus.keywords)
	  keyword-hash
	  string->keyword)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (prefix (only (ikarus records procedural)
		  record-type-method-retriever)
	    system::)
    (prefix (only (psyntax system $all)
		  internal-applicable-record-destructor
		  ;;FIXME To be uncommented at  the next boot image rotation.  (Marco
		  ;;Maggi; Tue Dec 15, 2015)
		  #;record-type-method-retriever)
	    system::))


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
    (apply (structs::struct-field-method std method-name.sym) subject args))

  (define (%record-object-call rtd)
    (define (%recurse)
      (%record-object-call (record-type-parent rtd)))
    (if rtd
	(cond ((system::record-type-method-retriever rtd)
	       => (lambda (method-retriever)
		    (cond ((method-retriever rtd method-name.sym)
			   => (lambda (proc)
				(apply proc subject args)))
			  (else
			   (%recurse)))))
	      (else
	       (%recurse)))
      (%error-record-type-has-no-matching-method)))

  (cond ((record-object? subject)
	 (%record-object-call (record-rtd subject)))

	((string?  subject)	(%built-in-scheme-object-call <string>-type-descriptor))
	((vector?  subject)	(%built-in-scheme-object-call <vector>-type-descriptor))
	((lists::nlist? subject)(%built-in-scheme-object-call <nlist>-type-descriptor))
	((null?    subject)	(%built-in-scheme-object-call <list>-type-descriptor))
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
	((pointer? subject)	(%built-in-scheme-object-call <pointer>-type-descriptor))
	((transcoder? subject)	(%built-in-scheme-object-call <transcoder>-type-descriptor))

	((struct? subject)	(%struct-object-call (structs::struct-std subject)))

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
  (cond ((record-object? obj)
	 ((system::internal-applicable-record-destructor obj) obj))
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


;;;; built-in object-types descriptor

;;Instances of this type are used at  run-time to describe the built-in Scheme object
;;types: pairs, fixnums, strings, et cetera.  Lexical variables bound to instances of
;;this type should be called BTD (as in "built-in type descriptor").
;;
(define-struct scheme-type
  (parent
		;False  if  this  type  has  no  parent;  otherwise  an  instance  of
		;"scheme-type" representing the parent of this type.
   uids-list
		;A list of symbols representing the  hierarchy of UIDs for this type.
		;The  first item  in the  list  is the  UID  of this  type, then  the
		;parent's UID, then the grandparent's UID, et cetera.
   method-retriever
		;If this  type has methods: a  procedure to be applied  to the method
		;name  to retriever  the  method  implementation function;  otherwise
		;false.
   ))


;;;; object type helpers: <top>

(define (<top>-type-predicate obj)
  #t)

(define (<top>-constructor obj)
  obj)


;;;; object type helpers: <boolean>

(define (<boolean>-constructor obj)
  (if obj #t #f))


;;;; object type helpers: <symbol>

(case-define <symbol>-value
  ((sym)
   (symbol-value sym))
  ((sym val)
   (set-symbol-value! sym val)))


;;;; object type helpers: <null>

(define (<null>-constructor)
  '())


;;;; object type helpers: <string>

(case-define <string>-for-each
  ((str func)
   (string-for-each func str))
  ((str func . str*)
   (apply string-for-each func str str*)))


;;;; object type helpers: <vector>

(case-define <vector>-for-each
  ((vec func)
   (vector-for-each func vec))
  ((vec func . vec*)
   (apply vector-for-each func vec vec*)))

(case-define <vector>-map
  ((vec func)
   (vector-map func vec))
  ((vec func . vec*)
   (apply vector-map func vec vec*)))

(case-define <vector>-for-all
  ((vec func)
   (vector-for-all func vec))
  ((vec func . vec*)
   (apply vector-for-all func vec vec*)))

(case-define <vector>-exists
  ((vec func)
   (vector-exists func vec))
  ((vec func . vec*)
   (apply vector-exists func vec vec*)))

(define (<vector>-find vec func)
  (vector-find func vec))

(case-define <vector>-fold-right
  ((vec combine knil)
   (vector-fold-right combine knil vec))
  ((vec combine knil . vec*)
   (apply vector-fold-right combine knil vec vec*)))

(case-define <vector>-fold-left
  ((vec combine knil)
   (vector-fold-left combine knil vec))
  ((vec combine knil . vec*)
   (apply vector-fold-left combine knil vec vec*)))

(define (<vector>-sort vec proc)
  (vector-sort proc vec))

(define (<vector>-sort! vec proc)
  (vector-sort! proc vec))


;;;; built-in object-types descriptors: definitions

(define-auxiliary-syntaxes methods)

(define-syntax (define-scheme-type stx)
  (syntax-case stx (methods)
    ((?kwd ?type-name ?parent-name ?maker ?pred)
     #'(?kwd ?type-name ?parent-name ?maker ?pred (methods)))
    ((_ ?type-name ?parent-name ?maker ?pred (methods (?method-name ?method-implementation-procedure) ...))
     (let* ((type-name.sym	(syntax->datum #'?type-name))
	    (parent-name.sexp	(syntax->datum #'?parent-name))
	    (type-uid.sym	(string->symbol (string-append "vicare:scheme-type:" (symbol->string type-name.sym))))
	    (type-uids-list	(cons type-uid.sym (if parent-name.sexp
						       (getprop parent-name.sexp 'type-uids-list)
						     '()))))
       (define (%datum->syntax obj)
	 (datum->syntax #'?type-name obj))
       (define (%mk-btd-name type.sym)
	 (%datum->syntax (string->symbol (string-append (symbol->string type.sym) "-type-descriptor"))))
       (putprop type-name.sym 'type-uids-list type-uids-list)
       ;;BTD stands for "Built-in Type Descriptor".
       (with-syntax
	   ((BTD-NAME		(%mk-btd-name type-name.sym))
	    (PARENT-NAME	(and parent-name.sexp
				     (%mk-btd-name parent-name.sexp)))
	    (TYPE-UIDS-LIST	#`(quote #,(%datum->syntax type-uids-list)))
	    (RETRIEVER		(if (null? (syntax->datum #'((?method-name ?method-implementation-procedure) ...)))
				    #f
				  #'(lambda (btd method-name.sym)
				      (case method-name.sym
					((?method-name) ?method-implementation-procedure)
					...
					(else #f))))))
	 #'(define BTD-NAME
	     (make-scheme-type PARENT-NAME TYPE-UIDS-LIST RETRIEVER)))))
    ))

;;; --------------------------------------------------------------------
;;; built-in Scheme objects type descriptors

(include "makefile.scheme-object-types.scm" #t)


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.object-utilities")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'define-scheme-type		'scheme-indent-function 2)
;; End:
