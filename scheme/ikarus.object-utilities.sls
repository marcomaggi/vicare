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
    ;; type methods
    method-call-late-binding

    ;; delete fallback
    internal-delete

    ;; Scheme type descriptor
    make-scheme-type-descriptor			scheme-type-descriptor?
    scheme-type-descriptor-name			scheme-type-descriptor-parent
    scheme-type-descriptor-type-predicate	scheme-type-descriptor-equality-predicate
    scheme-type-descriptor-comparison-procedure	scheme-type-descriptor-hash-function
    scheme-type-descriptor-uids-list		scheme-type-descriptor-method-retriever

    ;; built-in object-type specification utilities, for internal use
    <top>-constructor			<top>-type-predicate
    <boolean>-constructor
    <nelist>-constructor		<nelist>-type-predicate
    <symbol>-value			<string>-for-each

    <nestring>-constructor		<nestring>-type-predicate
    <empty-string>-constructor		<empty-string>-type-predicate

    <nevector>-constructor		<nevector>-type-predicate
    <empty-vector>-constructor		<empty-vector>-type-predicate
    <vector>-map			<vector>-for-each
    <vector>-for-all			<vector>-exists
    <vector>-find
    <vector>-fold-right			<vector>-fold-left
    <vector>-sort			<vector>-sort!

    <nebytevector>-constructor		<nebytevector>-type-predicate
    <empty-bytevector>-constructor	<empty-bytevector>-type-predicate

    #| end of EXPORT |# )
  (import (except (vicare)
		  method-call-late-binding

		  ;; Scheme type descriptor
		  <scheme-type-descriptor>
		  make-scheme-type-descriptor			scheme-type-descriptor?
		  scheme-type-descriptor-name			scheme-type-descriptor-parent
		  scheme-type-descriptor-type-predicate		scheme-type-descriptor-equality-predicate
		  scheme-type-descriptor-comparison-procedure	scheme-type-descriptor-hash-function
		  scheme-type-descriptor-uids-list		scheme-type-descriptor-method-retriever

		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Tue Dec 15, 2015)
		  true?					false?
		  zero-fixnum?
		  positive-bignum?			negative-bignum?
		  non-negative-bignum?			non-positive-bignum?
		  ratnum-positive?			ratnum-negative?
		  ratnum-non-positive?			ratnum-non-negative?
		  positive-ratnum?			negative-ratnum?
		  non-positive-ratnum?			non-negative-ratnum?
		  zero-flonum?
		  positive-zero-flonum?			negative-zero-flonum?
		  exact-compnum?			inexact-compnum?
		  zero-compnum?				non-zero-compnum?
		  zero-cflonum?				non-zero-cflonum?
		  hashtable-eq?				hashtable-eqv?
		  hashtable-equiv?
		  keyword-hash				pointer-hash
		  transcoder-hash			string->keyword
		  reader-annotation?
		  reader-annotation-expression		reader-annotation-stripped
		  reader-annotation-source		reader-annotation-textual-position
		  sentinel				sentinel?
		  ;;
		  compar-fixnum				compar-bignum
		  compar-exact-integer			compar-ratnum
		  compar-exact-real			compar-flonum
		  compar-real				compar-char
		  compar-string				compar-string-ci
		  compar-symbol				compar-boolean
		  compar-transcoder			compar-pointer
		  #| end of EXCEPT |# )
    (only (vicare system $fx)
	  $fxadd1)
    (only (vicare system $structs)
	  $struct-rtd)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Apr 12,
    ;;2016)
    (only (ikarus booleans)
	  true?
	  false?)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Tue Apr 5,
    ;;2016)
    (only (ikarus fixnums)
	  zero-fixnum?)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Tue Apr 5,
    ;;2016)
    (only (ikarus bignums)
	  positive-bignum?
	  negative-bignum?
	  non-negative-bignum?
	  non-positive-bignum?)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Tue Apr 5,
    ;;2016)
    (only (ikarus ratnums)
	  ratnum-positive?
	  ratnum-negative?
	  ratnum-non-positive?
	  ratnum-non-negative?
	  positive-ratnum?
	  negative-ratnum?
	  non-positive-ratnum?
	  non-negative-ratnum?)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Tue Apr 5,
    ;;2016)
    (only (ikarus numerics flonums)
	  zero-flonum?
	  positive-zero-flonum?
	  negative-zero-flonum?)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Tue Apr 5,
    ;;2016)
    (only (ikarus numerics complex-numbers)
	  exact-compnum?	inexact-compnum?
	  zero-compnum?		non-zero-compnum?	non-zero-inexact-compnum?
	  zero-cflonum?		non-zero-cflonum?)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (prefix (only (ikarus structs)
		  struct-field-method
		  struct-std)
	    structs::)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (only (ikarus.pointers)
	  pointer-hash)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Tue Apr 5,
    ;;2016)
    (only (ikarus hash-tables)
	  hashtable-eq?
	  hashtable-eqv?
	  hashtable-equiv?
	  transcoder-hash)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Sep 18,
    ;;2015)
    (only (ikarus.keywords)
	  keyword-hash
	  string->keyword)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Dec 15,
    ;;2015)
    (prefix (only (ikarus records procedural)
		  record-type-method-retriever
		  record-type-hash-function)
	    system::)
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Fri Apr 8,
    ;;2016)
    (only (ikarus.reader)
	  reader-annotation?
	  reader-annotation-expression
	  reader-annotation-stripped
	  reader-annotation-source
	  reader-annotation-textual-position)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Fri Apr 15,
    ;;2016)
    (only (ikarus unique-objects)
	  sentinel sentinel?)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Apr 26,
    ;;2016)
    (only (ikarus.comparison-procedures)
	   compar-fixnum
	   compar-bignum
	   compar-exact-integer
	   compar-ratnum
	   compar-exact-real
	   compar-flonum
	   compar-real
	   compar-char
	   compar-string
	   compar-string-ci
	   compar-symbol
	   compar-boolean
	   compar-transcoder
	   compar-pointer)
    ;;FIXME To be removed at the next boot image rotation.  (Marco Maggi; Tue Apr 26,
    ;;2016)
    (only (ikarus codecs)
	  transcoder=?)
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
    (error 'method-call-late-binding message method-name.sym subject args))
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
	 (cond ((scheme-type-descriptor-hash-function btd)
		=> %apply-hash-function)
	       ((scheme-type-descriptor-parent btd)
		=> loop)
	       (else
		(%apply-hash-function object-hash)))))
      (else
       (let loop ((btd btd))
	 (cond ((scheme-type-descriptor-method-retriever btd)
		=> (lambda (method-retriever)
		     (cond ((method-retriever method-name.sym)
			    => (lambda (proc)
				 (apply proc subject args)))
			   ((scheme-type-descriptor-parent btd)
			    => loop)
			   (else
			    (%error-scheme-type-has-no-matching-method)))))
	       ((scheme-type-descriptor-parent btd)
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
				    (%built-in-scheme-object-call <empty-string>-type-descriptor)
				  (%built-in-scheme-object-call <nestring>-type-descriptor)))
	((vector?  subject)	(if (vector-empty? subject)
				    (%built-in-scheme-object-call <empty-vector>-type-descriptor)
				  (%built-in-scheme-object-call <nevector>-type-descriptor)))
	((list?    subject)	(if (pair? subject)
				    (%built-in-scheme-object-call <nelist>-type-descriptor)
				  (%built-in-scheme-object-call <null>-type-descriptor)))
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

	((eq? subject (void))
	 (%built-in-scheme-object-call <void>-type-descriptor))
	((eq? subject (would-block-object))
	 (%built-in-scheme-object-call <would-block>-type-descriptor))
	((eq? subject (eof-object))
	 (%built-in-scheme-object-call <eof>-type-descriptor))

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


;;;; built-in object-types descriptor

;;Instances of this type are used at  run-time to describe the built-in Scheme object
;;types: pairs, fixnums, strings, et cetera.  Lexical variables bound to instances of
;;this type should be called BTD (as in "built-in type descriptor").
;;
(define-struct (scheme-type-descriptor make-scheme-type-descriptor scheme-type-descriptor?)
  (name
		;A symbol representing the name of this type.  For example: <string>.
   parent
		;False  if  this  type  has  no  parent;  otherwise  an  instance  of
		;"scheme-type-descriptor" representing the parent of this type.
   type-predicate
		;False or a function implementing the type predicate.
   equality-predicate
		;False or a function implementing the equality predicate.
   comparison-procedure
		;False or a function implementing the comparison procedure.
   hash-function
		;False or a function implementing the hash function.
   uids-list
		;A list of symbols representing the  hierarchy of UIDs for this type.
		;The  first item  in the  list  is the  UID  of this  type, then  the
		;parent's UID, then the grandparent's UID, et cetera.
   method-retriever))
		;If this  type has methods: a  procedure to be applied  to the method
		;name  (a symbol)  to  retrieve the  method implementation  function;
		;otherwise false.


;;;; object type helpers: <top>

(define (<top>-type-predicate obj)
  (not (void-object? obj)))

(define* (<top>-constructor {obj (not void-object?)})
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


;;;; object type helpers: <nelist>

(define (<nelist>-constructor obj . obj*)
  (cons obj obj*))

(define (<nelist>-type-predicate obj)
  (and (pair? obj)
       (list? obj)))


;;;; object type helpers: <string>

(define (<empty-string>-constructor)
  ;;Let's return an actually new string.
  ;;
  (string))

(define (<empty-string>-type-predicate obj)
  (and (string? obj)
       (string-empty? obj)))

;;; --------------------------------------------------------------------

(define (<nestring>-constructor obj . obj*)
  (apply string obj obj*))

(define (<nestring>-type-predicate obj)
  (and (string? obj)
       (not (string-empty? obj))))

;;; --------------------------------------------------------------------

(case-define <string>-for-each
  ((str func)
   (string-for-each func str))
  ((str func . str*)
   (apply string-for-each func str str*)))


;;;; object type helpers: <vector>, <nevector>, <empty-vector>

(define (<empty-vector>-constructor)
  ;;Let's return an actually new vector.
  ;;
  (vector))

(define (<empty-vector>-type-predicate obj)
  (and (vector? obj)
       (vector-empty? obj)))

;;; --------------------------------------------------------------------

(define (<nevector>-constructor obj . obj*)
  (apply vector obj obj*))

(define (<nevector>-type-predicate obj)
  (and (vector? obj)
       (not (vector-empty? obj))))

;;; --------------------------------------------------------------------

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


;;;; object type helpers: <bytevector>, <nebytevector>, <empty-bytevector>

(define (<empty-bytevector>-constructor)
  ;;Let's return an actually new bytevector.
  ;;
  (make-bytevector 0))

(define (<empty-bytevector>-type-predicate obj)
  (and (bytevector? obj)
       (bytevector-empty? obj)))

;;; --------------------------------------------------------------------

(case-define* <nebytevector>-constructor
  (({len positive-fixnum?})
   (make-vector len))
  (({len positive-fixnum?} {fill bytevector-byte-filler?})
   (make-vector len fill)))

(define (<nebytevector>-type-predicate obj)
  (and (bytevector? obj)
       (not (bytevector-empty? obj))))

(define (bytevector-byte-filler? obj)
  ;;Return  #t if  OBJ  is valid  as byte  filler  for new  bytevectors;
  ;;otherwise return #f.
  ;;
  (and (fixnum? obj)
       (fx>=? obj -128)
       (fx<=? obj +255)))


;;;; built-in object-types descriptors: definitions

(define-syntax* (define-scheme-type input-form.stx)
  (define (main stx)
    (syntax-case stx (methods)
      ((?kwd ?type-name ?parent-name . ?clauses)
       (let* ((clause*.stx	(syntax-clauses-unwrap #'?clauses synner))
	      (clause*.stx	(syntax-clauses-collapse clause*.stx))
	      (parsed-specs		(%parse-clauses clause*.stx))
	      ;;
	      (type-name.sym	(syntax->datum #'?type-name))
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
	     ((BTD-NAME			(%mk-btd-name type-name.sym))
	      (PARENT-BTD-NAME		(and parent-name.sexp (%mk-btd-name parent-name.sexp)))
	      (TYPE-PREDICATE		(parsed-specs-type-predicate		parsed-specs))
	      (EQUALITY-PREDICATE	(parsed-specs-equality-predicate	parsed-specs))
	      (COMPARISON-PROCEDURE	(parsed-specs-comparison-procedure	parsed-specs))
	      (HASH-FUNCTION		(parsed-specs-hash-function		parsed-specs))
	      (TYPE-UIDS-LIST		#`(quote #,(%datum->syntax type-uids-list)))
	      (RETRIEVER		(%make-methods-retriever-function parsed-specs)))
	   #'(begin
	       (define BTD-NAME
		 (make-scheme-type-descriptor (quote ?type-name) PARENT-BTD-NAME
					      TYPE-PREDICATE EQUALITY-PREDICATE COMPARISON-PROCEDURE HASH-FUNCTION
					      TYPE-UIDS-LIST RETRIEVER))
	       (export BTD-NAME)))))
      ))

  (define-constant LIST-OF-CLAUSES
    (syntax-clauses-validate-specs
     (list (make-syntax-clause-spec #'constructor		0 1 0 1      '() '())
	   (make-syntax-clause-spec #'predicate			0 1 1 1      '() '())
	   (make-syntax-clause-spec #'equality-predicate	0 1 1 1      '() '())
	   (make-syntax-clause-spec #'comparison-procedure	0 1 1 1      '() '())
	   (make-syntax-clause-spec #'hash-function		0 1 1 1      '() '())
	   (make-syntax-clause-spec #'methods			0 1 1 +inf.0 '() '()))))

  (define-record-type parsed-specs
    (fields
      (mutable constructor)
		;A  boolean or  an  identifier representing  the object  constructor.
		;When #f: this object type has  no constructor.  When #t: this object
		;type has  no constructor, but  the syntax  NEW must verify  that its
		;single argument is already an instance of this type.
      (mutable type-predicate)
		;False or an identifier representing  the type predicate.  When #f:
		;this object type has no type predicate.
      (mutable equality-predicate)
		;False or an identifier representing the equality predicate function.
		;When #f: this object type has no equality predicate.
      (mutable comparison-procedure)
		;False or an identifier  representing the comparison procedure.  When
		;#f: this object type has no comparison procedure.
      (mutable hash-function)
		;False or an identifier representing  the object hash function.  When
		;#f: this object type has no hash function.
      (mutable methods)
		;A possibly empty proper list of method specifications.
      #| end of FIELDS |# )
    (protocol
      (lambda (make-record)
	(lambda ()
	  (make-record #f #f #f #f #f '()))))
    #| end of DEFINE-RECORD-TYPE |# )

  (define (%parse-clauses clause*.stx)
    (syntax-clauses-fold-specs
     (lambda* ({parsed-specs parsed-specs?} {clause-spec syntax-clause-spec?} args)
       ;;ARGS is  a vector of  vectors holding the  values from the  clauses matching
       ;;CLAUSE-SPEC.
       (assert (fx=? 1 (vector-length args)))
       (let ((arg (vector-ref args 0)))
	 (case-identifiers (syntax-clause-spec-keyword clause-spec)
	   ((constructor)
	    (if (fxzero? (vector-length arg))
		(parsed-specs-constructor-set! parsed-specs #f)
	      (let ((id (vector-ref arg 0)))
		(unless (or (identifier? id) (boolean? id))
		  (synner "invalid constructor specification" id))
		(parsed-specs-constructor-set! parsed-specs id))))
	   ((predicate)
	    (let ((id (vector-ref arg 0)))
	      (unless (or (identifier? id) (boolean? id))
		(synner "invalid predicate specification" id))
	      (parsed-specs-type-predicate-set! parsed-specs id)))
	   ((equality-predicate)
	    (let ((id (vector-ref arg 0)))
	      (unless (or (identifier? id) (not id))
		(synner "invalid equality predicate specification" id))
	      (parsed-specs-equality-predicate-set! parsed-specs id)))
	   ((comparison-procedure)
	    (let ((id (vector-ref arg 0)))
	      (unless (or (identifier? id) (not id))
		(synner "invalid comparison procedure specification" id))
	      (parsed-specs-comparison-procedure-set! parsed-specs id)))
	   ((hash-function)
	    (let ((id (vector-ref arg 0)))
	      (unless (or (identifier? id) (not id))
		(synner "invalid hash function specification" id))
	      (parsed-specs-hash-function-set! parsed-specs id)))
	   ((methods)
	    (syntax-case arg ()
	      (#((?method-name ?method-implementation-procedure) ...)
	       (parsed-specs-methods-set! parsed-specs #'((?method-name ?method-implementation-procedure) ...)))
	      (_
	       (synner "invalid syntax in METHODS clause" arg))))))
       parsed-specs)
     (make-parsed-specs) LIST-OF-CLAUSES clause*.stx))

  (define (%make-methods-retriever-function specs)
    (syntax-case (parsed-specs-methods specs) ()
      (() #f)
      (((?method-name ?method-implementation-procedure) ...)
       (with-syntax
	   ((METHOD-NAME (datum->syntax #'?kwd 'method-name)))
	 #'(lambda (METHOD-NAME)
	     (case METHOD-NAME
	       ((?method-name) ?method-implementation-procedure)
	       ...
	       (else #f)))))))

  (main input-form.stx))

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
;; eval: (put 'define-scheme-type		'scheme-indent-function 2)
;; End:
