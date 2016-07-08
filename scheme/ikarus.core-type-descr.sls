;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: core type descriptors
;;;Date: Thu Jun 16, 2016
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
(library (ikarus.core-type-descr)
  (export
    ;; Scheme type descriptor
    <core-type-descriptor>-rtd			<core-type-descriptor>-rcd
    make-core-type-descriptor			core-type-descriptor?
    core-type-descriptor.name			core-type-descriptor.parent
    core-type-descriptor.type-predicate		core-type-descriptor.equality-predicate
    core-type-descriptor.comparison-procedure	core-type-descriptor.hash-function
    core-type-descriptor.uid
    core-type-descriptor.uids-list		core-type-descriptor.method-retriever
    core-type-descriptor.implemented-interfaces
    core-type-descriptor=?			core-type-descriptor.ancestor-des*
    core-type-descriptor.parent-and-child?

    ;; built-in object-type specification utilities, for internal use
    <top>-constructor			<top>-type-predicate
    <untyped>-constructor		<untyped>-type-predicate
    <boolean>-constructor
    <nelist>-constructor		<nelist>-type-predicate
    <symbol>-value			<string>-for-each

    <nestring>-constructor		<empty-string>-constructor

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
		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Tue Dec 15, 2015)
		  empty-string?				nestring?
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
		  zero-compnum?				non-zero-compnum?	non-zero-inexact-compnum?
		  zero-cflonum?				non-zero-cflonum?
		  hashtable-eq?				hashtable-eqv?
		  hashtable-equiv?
		  keyword-hash				pointer-hash
		  transcoder-hash			string->keyword
		  reader-annotation?
		  reader-annotation-expression		reader-annotation-stripped
		  reader-annotation-source		reader-annotation-textual-position
		  sentinel				sentinel?
		  transcoder=?
		  ;;
		  compar-fixnum				compar-bignum
		  compar-exact-integer			compar-ratnum
		  compar-exact-real			compar-flonum
		  compar-real				compar-char
		  compar-string				compar-string-ci
		  compar-symbol				compar-boolean
		  compar-transcoder			compar-pointer
		  #| end of EXCEPT |# )
    (ikarus records syntactic)
    (only (vicare system $fx)
	  $fxadd1)
    (only (vicare system $structs)
	  $struct-rtd
	  $set-std-printer!)
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
    ;;FIXME To be removed at the next  boot image rotation.  (Marco Maggi; Sun May 8,
    ;;2016)
    (only (ikarus strings)
	  empty-string?
	  nestring?)
    (prefix (only (psyntax system $all)
		  internal-applicable-record-destructor
		  ;;FIXME To be uncommented at  the next boot image rotation.  (Marco
		  ;;Maggi; Tue Dec 15, 2015)
		  #;record-type-method-retriever)
	    system::))


;;;; built-in object-types descriptor

;;Instances of this type are used at  run-time to describe the built-in Scheme object
;;types: pairs, fixnums, strings, et cetera.  Lexical variables bound to instances of
;;this type should be called BTD (as in "built-in type descriptor").
;;
(define-core-record-type <core-type-descriptor>
  (define-type-descriptors)
  (strip-angular-parentheses)
  (nongenerative vicare:descriptors:<core-type-descriptor>)
  (sealed #t)
  (fields
    (immutable name			core-type-descriptor.name)
		;A symbol representing the name of this type.  For example: <string>.
    (immutable parent			core-type-descriptor.parent)
		;False  if  this  type  has  no  parent;  otherwise  an  instance  of
		;"core-type-descriptor" representing the parent of this type.
    (immutable type-predicate		core-type-descriptor.type-predicate)
		;False or a function implementing the type predicate.
    (immutable equality-predicate	core-type-descriptor.equality-predicate)
		;False or a function implementing the equality predicate.
    (immutable comparison-procedure	core-type-descriptor.comparison-procedure)
		;False or a function implementing the comparison procedure.
    (immutable hash-function		core-type-descriptor.hash-function)
		;False or a function implementing the hash function.
    (immutable uids-list		core-type-descriptor.uids-list)
		;A list of symbols representing the  hierarchy of UIDs for this type.
		;The  first item  in the  list  is the  UID  of this  type, then  the
		;parent's UID, then the grandparent's UID, et cetera.
    (immutable method-retriever		core-type-descriptor.method-retriever)
		;If this  type has methods: a  procedure to be applied  to the method
		;name  (a symbol)  to  retrieve the  method implementation  function;
		;otherwise false.
    (immutable implemented-interfaces	core-type-descriptor.implemented-interfaces)
		;False  or  a  vector   of  pairs  representing  the  interface-types
		;implemented by this  object-type.  Each pair has: as car  the UID of
		;an interface-type; as cdr a method retriever procedure to be used by
		;the interface method callers.
    #| end of FIELDS |# )

  (custom-printer
    (lambda (reco port sub-printer)
      (define-syntax-rule (%display ?obj)
	(display ?obj port))
      (%display "#[ctd ")
      (%display (core-type-descriptor.name reco))
      (%display "]")))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define* (core-type-descriptor.uid {des core-type-descriptor?})
  (car (core-type-descriptor.uids-list des)))

(define* (core-type-descriptor=? {des1 core-type-descriptor?} {des2 core-type-descriptor?})
  (or (eq? des1 des2)
      (eq? (core-type-descriptor.name des1)
	   (core-type-descriptor.name des2))))

(define* (core-type-descriptor.parent-and-child? {super.des core-type-descriptor?} {sub.des core-type-descriptor?})
  (memq (car (core-type-descriptor.uids-list super.des))
	(core-type-descriptor.uids-list sub.des)))

(define* (core-type-descriptor.ancestor-des* {des core-type-descriptor?})
  (let recur ((des des))
    (cond ((core-type-descriptor.parent des)
	   => (lambda (parent.des)
		(cons parent.des (recur parent.des))))
	  (else '()))))


;;;; object type helpers: <top>

(define (<top>-type-predicate obj)
  (not (void-object? obj)))

(define* (<top>-constructor {obj (not void-object?)})
  obj)


;;;; object type helpers: <untyped>

(define* (<untyped>-type-predicate obj)
  (assertion-violation __who__ "attempt to use the type predicate of <untyped>" obj))

(define* (<untyped>-constructor args)
  (assertion-violation __who__ "attempt to use the type constructor of <untyped>" args))


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

;;; --------------------------------------------------------------------

(define (<nestring>-constructor obj . obj*)
  (apply string obj obj*))

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


;;;; built-in object-types descriptors: syntax utilities

(define-syntax (define-scheme-type input-form.stx)
  (define (main stx)
    (syntax-case stx (methods)
      ((_ ?type-name ?parent-name . ?clauses)
       (let* ((clause*.stx		(syntax-clauses-unwrap #'?clauses synner))
	      (clause*.stx		(syntax-clauses-collapse clause*.stx))
	      (parsed-specs		(%parse-clauses clause*.stx)))
	 (define (%mk-ctd-name-id type.sym)
	   (datum->syntax #'?type-name (string->symbol (string-append (symbol->string type.sym) "-ctd"))))
	 (let* ((type-name.sym		(syntax->datum #'?type-name))
		(parent-name.sexp	(syntax->datum #'?parent-name))
		(type-uid.sym		(string->symbol (string-append "vicare:core-type:" (symbol->string type-name.sym))))
		(type-uids-list.sexp	(cons type-uid.sym (if parent-name.sexp
							       (getprop parent-name.sexp 'type-uids-list)
							     '())))
		(parent-name.id		(and parent-name.sexp (%mk-ctd-name-id parent-name.sexp))))
	   (putprop type-name.sym 'type-uids-list type-uids-list.sexp)
	   ;;BTD stands for "Built-in Type Descriptor".
	   (with-syntax
	       ((CTD-NAME		(%mk-ctd-name-id type-name.sym))
		(PARENT-CTD-NAME	parent-name.id)
		(TYPE-PREDICATE		(parsed-specs-type-predicate		parsed-specs))
		(EQUALITY-PREDICATE	(parsed-specs-equality-predicate	parsed-specs))
		(COMPARISON-PROCEDURE	(parsed-specs-comparison-procedure	parsed-specs))
		(HASH-FUNCTION		(parsed-specs-hash-function		parsed-specs))
		(TYPE-UIDS-LIST		#`(quote #,(datum->syntax #'?type-name type-uids-list.sexp)))
		(METHOD-RETRIEVER	(%make-method-retriever-function parsed-specs parent-name.id))
		(IMPLEMENTED-INTERFACES	#f))
	     #'(begin
		 (define CTD-NAME
		   (make-core-type-descriptor (quote ?type-name) PARENT-CTD-NAME
					      TYPE-PREDICATE EQUALITY-PREDICATE COMPARISON-PROCEDURE HASH-FUNCTION
					      TYPE-UIDS-LIST METHOD-RETRIEVER IMPLEMENTED-INTERFACES))
		 (export CTD-NAME))))))
      ))

;;; --------------------------------------------------------------------

  (define-constant LIST-OF-CLAUSES
    (syntax-clauses-validate-specs
     (list (make-syntax-clause-spec #'constructor		0 1 0 1      '() '())
	   (make-syntax-clause-spec #'type-predicate		0 1 1 1      '() '())
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
      (mutable methods-table)
		;A possibly empty proper list of method specifications.
      #| end of FIELDS |# )
    (protocol
      (lambda (make-record)
	(lambda ()
	  (make-record #f  ;constructor
		       #f  ;type-predicate
		       #f  ;equality-predicate
		       #f  ;comparison-procedure
		       #f  ;hash-function
		       '() ;methods-table
		       ))))
    #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

  (define (%parse-clauses clause*.stx)
    (syntax-clauses-fold-specs combine (make-parsed-specs) LIST-OF-CLAUSES clause*.stx))

  (define* (combine {parsed-specs parsed-specs?} {clause-spec syntax-clause-spec?} args)
    ;;ARGS  is a  vector of  vectors  holding the  values from  the clauses  matching
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

	((type-predicate)
	 (let ((id (vector-ref arg 0)))
	   (unless (or (identifier? id) (boolean? id))
	     (synner "invalid type predicate specification" id))
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
	    (parsed-specs-methods-table-set! parsed-specs #'(((?method-name) ?method-implementation-procedure) ...)))
	   (_
	    (synner "invalid syntax in METHODS clause" arg))))))
    parsed-specs)

;;; --------------------------------------------------------------------

  (define (%make-method-retriever-function parsed-specs parent-name.id)
    (with-syntax
	(((HASH-METHOD-CLAUSE ...)	(cond ((parsed-specs-hash-function parsed-specs)
					       => (lambda (hash-function.id)
						    #`(((hash) #,hash-function.id))))
					      (else '())))
	 ((METHOD-CLAUSE ...)		(parsed-specs-methods-table parsed-specs)))

      (if parent-name.id
	  #`((lambda (parent-retriever)
	       (lambda (method-name)
		 (case method-name
		   HASH-METHOD-CLAUSE ...
		   METHOD-CLAUSE      ...
		   (else
		    (parent-retriever method-name)))))
	     (core-type-descriptor.method-retriever #,parent-name.id))
	#'(lambda (method-name)
	    (case method-name HASH-METHOD-CLAUSE ... METHOD-CLAUSE ... (else #f))))))

;;; --------------------------------------------------------------------

  (case-define synner
    ((message)
     (synner message #f))
    ((message subform)
     (syntax-violation (quote define-scheme-type) message input-form.stx subform)))

  (main input-form.stx))


;;;; built-in object-types descriptors: definitions

(include "makefile.scheme-object-types.scm" #t)


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.core-type-descr")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; eval: (put 'define-scheme-type		'scheme-indent-function 2)
;; End:
