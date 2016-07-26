;;; -*- coding: utf-8-unix -*-
;;;
;;;Copyright (c) 2015-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;;;; introduction to object-type specifications
;;
;;The hierarchy of the record types defined in this module is:
;;
;;   <object-type-spec>
;;           |
;;           |------------> <core-type-spec>
;;           |
;;           |------------> <struct-type-spec>
;;           |
;;           |------------> <record-type-spec>
;;           |
;;           |------------> <closure-type-spec>
;;           |
;;           |------------> <compound-condition-type-spec>
;;           |
;;           |------------> <union-type-spec>
;;           |
;;           |------------> <intersection-type-spec>
;;           |
;;           |------------> <complement-type-spec>
;;           |
;;           |------------> <ancestor-of-type-spec>
;;           |
;;	     |------------> <pair-type-spec>
;;           |
;;	     |------------> <pair-of-type-spec>
;;           |
;;	     |------------> <list-type-spec>
;;           |
;;	     |------------> <list-of-type-spec> --> <alist-type-spec>
;;           |
;;	     |------------> <vector-type-spec>
;;           |
;;	     |------------> <vector-of-type-spec>
;;	     |
;;	     |------------> <hashtable-type-spec>
;;	     |
;;	     |------------> <enumeration-type-spec>
;;	     |
;;	      ------------> <label-type-spec>
;;	     |
;;	      ------------> <interface-type-spec>
;;
;;The type "<object-type-spec>" has a field  PARENT-OTS that is used to represent the
;;hierarchy of Scheme-level object-types.
;;
;;All   the  built-in   Scheme  object   types  are   represented  by   instances  of
;;"<core-type-spec>".  So  "<top>", "<fixnum>", "<string>", "<list>"  et cetera are
;;represented by instances of "<core-type-spec>".
;;
;;The hierarchy of list types is as follows:
;;
;;   <list>
;;      |
;;      |---> <null>
;;      |
;;      |---> (list-of ?type) ---> <null>
;;      |
;;       ---> <nelist> ------------> (list ?type0 ?type ...)
;;
;;notice how "<null>" is considered a sub-type of both "<list>" and "(list-of ?type)"
;;annotations,  but *not*  of  "(list ?type0  ?type ..)";  this  special handling  is
;;implemented  in the  function OBJECT-TYPE-SPEC.MATCHING-SUPER-AND-SUB?.   "(list-of
;;?type)" annotations are represented  by instances of "<list-of-type-spec>".  "(list
;;?type0 ?type ...)"  annotations are represented by instances of "<list-type-spec>",
;;and represent non-empty lists.
;;
;;The hierarchy of vector types is as follows:
;;
;;   <vector>
;;      |
;;      |---> <empty-vector>
;;      |
;;      |---> (vector-of ?type) ---> <empty-vector>
;;      |
;;       ---> <nevector> ----------> (vector ?type0 ?type ...)
;;
;;notice  how  "<empty-vector>" is  considered  a  sub-type  of both  "<vector>"  and
;;"(vector-of  ?type)"  annotations,  but  *not*   of  "(vector  ?type0  ?type  ...)"
;;annotations;   this    special   handling   is   implemented    in   the   function
;;OBJECT-TYPE-SPEC.MATCHING-SUPER-AND-SUB?.   "(vector-of   ?type)"  annotations  are
;;represented by  instances of "<vector-of-type-spec>".  "(vector  ?type0 ?type ...)"
;;annotations  are represented  by instances  of "<vector-type-spec>",  and represent
;;non-empty vectors.
;;
;;The only  record type defined in  this module that  can be sub-typed at  the Scheme
;;level is "<record-type-spec>".
;;


(module (<object-type-spec>
	 <object-type-spec>-rtd				<object-type-spec>-rcd
	 object-type-spec?
	 object-type-spec.name				object-type-spec.type-annotation
	 object-type-spec.uids-list
	 object-type-spec.parent-ots			object-type-spec.type-predicate-stx
	 object-type-spec.constructor-stx		object-type-spec.destructor-stx
	 object-type-spec.equality-predicate		object-type-spec.comparison-procedure
	 object-type-spec.hash-function			object-type-spec.applicable-hash-function
	 object-type-spec.ancestor-ots*
	 object-type-spec.applicable-method-stx		object-type-spec.applicable-private-method-stx
	 object-type-spec.implemented-interfaces
	 object-type-spec.type-descriptor-core-expr
	 object-type-spec.single-value-validator-lambda-stx
	 object-type-spec.list-validator-lambda-stx
	 expression-expander-for-core-expressions

	 object-type-spec.matching-super-and-sub?	object-type-spec.compatible-super-and-sub?
	 object-type-spec=?
	 object-type-spec.common-ancestor		object-type-spec.procedure?
	 object-type-spec.list-sub-type?		object-type-spec.vector-sub-type?
	 object-type-specs-delete-duplicates

	 <core-type-spec>
	 <core-type-spec>-rtd				<core-type-spec>-rcd
	 make-core-type-spec				core-type-spec?
	 core-type-spec.type-descriptor-id

	 <closure-type-spec>
	 <closure-type-spec>-rtd			<closure-type-spec>-rcd
	 make-closure-type-spec				closure-type-spec?
	 closure-type-spec.signature			closure-type-spec.set-new-retvals-when-untyped!
	 closure-type-spec.thunk?			closure-type-spec.join
	 closure-type-spec.super-and-sub?
	 closure-type-spec.match-formals-against-operands

	 <struct-type-spec>
	 <struct-type-spec>-rtd				<struct-type-spec>-rcd
	 make-struct-type-spec				struct-type-spec?
	 struct-type-spec.std

	 <record-type-spec>
	 <record-type-spec>-rtd				<record-type-spec>-rcd
	 make-record-type-spec				record-type-spec?
	 record-type-spec.rtd-id			record-type-spec.rcd-id
	 record-type-spec.super-protocol-id		record-type-spec.virtual-method-signatures
	 simple-condition-object-type-spec?

	 <compound-condition-type-spec>
	 <compound-condition-type-spec>-rtd		<compound-condition-type-spec>-rcd
	 make-compound-condition-type-spec		compound-condition-type-spec?
	 compound-condition-type-spec.component-ots*

	 ;;;

	 <pair-type-spec>
	 <pair-type-spec>-rtd				<pair-type-spec>-rcd
	 make-pair-type-spec				pair-type-spec?
	 pair-type-spec.car-ots				pair-type-spec.cdr-ots

	 <pair-of-type-spec>
	 <pair-of-type-spec>-rtd			<pair-of-type-spec>-rcd
	 make-pair-of-type-spec				pair-of-type-spec?
	 pair-of-type-spec.item-ots

	 ;;;

	 <list-type-spec>
	 <list-type-spec>-rtd				<list-type-spec>-rcd
	 make-list-type-spec				list-type-spec?
	 list-type-spec.item-ots*			list-type-spec.length
	 list-type-spec.list-of-single-item?
	 make-null-or-list-type-spec

	 <list-of-type-spec>
	 <list-of-type-spec>-rtd			<list-of-type-spec>-rcd
	 make-list-of-type-spec				list-of-type-spec?
	 list-of-type-spec.item-ots

	 ;;;

	 <vector-type-spec>
	 <vector-type-spec>-rtd				<vector-type-spec>-rcd
	 make-vector-type-spec				vector-type-spec?
	 vector-type-spec.item-ots*

	 <vector-of-type-spec>
	 <vector-of-type-spec>-rtd			<vector-of-type-spec>-rcd
	 make-vector-of-type-spec			vector-of-type-spec?
	 vector-of-type-spec.item-ots

	 ;;;

	 <hashtable-type-spec>
	 <hashtable-type-spec>-rtd			<hashtable-type-spec>-rcd
	 make-hashtable-type-spec			hashtable-type-spec?
	 hashtable-type-spec.key-ots			hashtable-type-spec.val-ots

	 <alist-type-spec>
	 <alist-type-spec>-rtd				<alist-type-spec>-rcd
	 make-alist-type-spec				alist-type-spec?
	 alist-type-spec.key-ots			alist-type-spec.val-ots

	 <enumeration-type-spec>
	 <enumeration-type-spec>-rtd			<enumeration-type-spec>-rcd
	 make-enumeration-type-spec			enumeration-type-spec?
	 enumeration-type-spec.symbol*			enumeration-type-spec.member?

	 ;;;

	 <union-type-spec>
	 <union-type-spec>-rtd				<union-type-spec>-rcd
	 make-union-type-spec				union-of-type-specs
	 union-type-spec?				union-type-spec.item-ots*

	 <intersection-type-spec>
	 <intersection-type-spec>-rtd			<intersection-type-spec>-rcd
	 make-intersection-type-spec			intersection-of-type-specs
	 intersection-type-spec?			intersection-type-spec.item-ots*

	 <complement-type-spec>
	 <complement-type-spec>-rtd			<complement-type-spec>-rcd
	 make-complement-type-spec			complement-type-spec?
	 complement-type-spec.item-ots

	 ;;;

	 <ancestor-of-type-spec>
	 <ancestor-of-type-spec>-rtd			<ancestor-of-type-spec>-rcd
	 make-ancestor-of-type-spec			ancestor-of-type-spec?
	 ancestor-of-type-spec.item-ots			ancestor-of-type-spec.ancestor-ots*

	 ;;;

	 <label-type-spec>-rtd				<label-type-spec>-rcd
	 <label-type-spec>
	 make-label-type-spec				label-type-spec?

	 <interface-type-spec>-rtd			<interface-type-spec>-rcd
	 <interface-type-spec>
	 make-interface-type-spec			interface-type-spec?
	 interface-type-spec.type-descriptor-id
	 interface-type-spec.method-prototypes-table
	 assert-implemented-interface-type-and-implementer-interface-type
	 build-table-for-interface-types-and-implementer-object-type

	 ;;;

	 make-type-annotation
	 syntax-object.type-annotation?
	 type-annotation->object-type-spec
	 expression-expander-for-type-annotations
	 type-signature.type-descriptor-core-expr

	 #| end of export list |# )

  (import (only (vicare language-extensions syntaxes)
		define-equality/sorting-predicate))


;;;; helpers

(define (object-type-spec.list-type-spec? object.ots)
  (or (<list>-ots? object.ots)
      (<null>-ots? object.ots)
      (<nelist>-ots? object.ots)
      (list-type-spec? object.ots)
      (list-of-type-spec? object.ots)
      (and (pair-type-spec? object.ots)
	   (object-type-spec.list-type-spec? (pair-type-spec.cdr-ots object.ots)))
      (and (pair-of-type-spec? object.ots)
	   (object-type-spec.list-type-spec? (pair-of-type-spec.item-ots object.ots)))))

(define (object-type-spec.compatible-list-type-spec? object.ots)
  (or (object-type-spec.list-type-spec? object.ots)
      (object-type-spec.compatible-super-and-sub? (<list>-ots) object.ots)))

;;; --------------------------------------------------------------------

(define-syntax cond-with-predicates
  (syntax-rules (else)
    ((_ ?expr (?pred . ?body) ... (else . ?else-body))
     (let ((X ?expr))
       (cond ((?pred X) . ?body)
	     ...
	     (else . ?else-body))))
    ((_ ?expr (?pred . ?body) ...)
     (let ((X ?expr))
       (cond ((?pred X) . ?body)
	     ...)))
    ))

(define-syntax or-with-predicates
  (syntax-rules ()
    ((_ ?expr ?pred ...)
     (let ((X ?expr))
       (or (?pred X) ...)))
    ))

(define-syntax case-specification
  (syntax-rules (else
		 core-type-spec? record-type-spec? struct-type-spec?
		 list-type-spec? list-of-type-spec? vector-type-spec? vector-of-type-spec?
		 pair-type-spec? pair-of-type-spec? compound-condition-type-spec? enumeration-type-spec?
		 closure-type-spec?
		 hashtable-type-spec? union-type-spec? intersection-type-spec? complement-type-spec?
		 ancestor-of-type-spec?)
    ((_ ?expr
	(core-type-spec?		. ?body-core)
	(record-type-spec?		. ?body-record)
	(struct-type-spec?		. ?body-struct)
	(list-type-spec?		. ?body-list)
	(list-of-type-spec?		. ?body-list-of)
	(vector-type-spec?		. ?body-vector)
	(vector-of-type-spec?		. ?body-vector-of)
	(pair-type-spec?		. ?body-pair)
	(pair-of-type-spec?		. ?body-pair-of)
	(compound-condition-type-spec?	. ?body-compound-condition)
	(enumeration-type-spec?		. ?body-enumeration)
	(closure-type-spec?		. ?body-closure)
	(hashtable-type-spec?		. ?body-hashtable)
	(union-type-spec?		. ?body-union)
	(intersection-type-spec?	. ?body-intersection)
	(complement-type-spec?		. ?body-complement)
	(ancestor-of-type-spec?		. ?body-ancestor-of)
	(else				. ?body-else))
     (let ((des ?expr))
       (cond
	((core-type-spec? des)			. ?body-core)
	((record-type-spec? des)		. ?body-record)
	((struct-type-spec? des)		. ?body-struct)
	((list-type-spec? des)			. ?body-list)
	((list-of-type-spec? des)		. ?body-list-of)
	((vector-type-spec? des)		. ?body-vector)
	((vector-of-type-spec? des)		. ?body-vector-of)
	((pair-type-spec? des)			. ?body-pair)
	((pair-of-type-spec? des)		. ?body-pair-of)
	((compound-condition-type-spec? des)	. ?body-compound-condition)
	((enumeration-type-spec? des)		. ?body-enumeration)
	((closure-type-spec? des)		. ?body-closure)
	((hashtable-type-spec? des)		. ?body-hashtable)
	((union-type-spec? des)			. ?body-union)
	((intersection-type-spec? des)		. ?body-intersection)
	((complement-type-spec? des)		. ?body-complement)
	((ancestor-of-type-spec? des)		. ?body-ancestor-of)
	(else					. ?body-else))))
    ))


;;;; basic object-type specification
;;
;;This  record-type is  used as  base  type for  all the  Scheme objects  expand-time
;;specifications.
;;
;;We must handle this type as if it  is an "abstract" type: we must never instantiate
;;it directly, rather we  must define subtype and instantiate that.   This is why the
;;maker of "<object-type-spec>" is not exported by the module.
;;
(define-core-record-type <object-type-spec>
  (nongenerative vicare:expander:<object-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (fields
    (mutable name-or-maker		object-type-spec.name-or-maker object-type-spec.name-or-maker-set!)
		;A  procedure that,  applied to  this  OTS, returns  a syntax  object
		;representing the name of this type.  For some types it is the actual
		;type  identifier, for  example:  "<fixnum>,  "<string>".  For  other
		;types it is a syntax object like "(list-of <fixnum>)".

    (immutable uids-list	object-type-spec.uids-list)
		;A list of symbols uniquely identifying this type specification.

    (immutable parent-ots		object-type-spec.parent-ots)
		;False or an instance of "<object-type-spec>" representing the parent
		;of this object-type.
		;
		;We need to remember that:
		;
		;* When defining  a built-in Scheme type, the  following types cannot
		;be subtyped: <bottom>, <void>, <null>, <empty-vector>.
		;
		;* Besides  the built-in  Scheme types, the  pari subtypes,  the list
		;subtypes, the  vector subtypes, the only  object-type specifications
		;for which we might specify a parent are records.
		;
		;*  The  subtypes of  pairs,  lists  and  vectors cannot  be  further
		;subtyped.

    (immutable type-annotation-maker	object-type-spec.type-annotation-maker)
		;A  procedure that,  applied to  this  OTS, returns  a syntax  object
		;representing the type annotation of this type.

    (immutable constructor-stx		object-type-spec.constructor-stx)
		;A boolean value or a  syntax object representing a Scheme expression
		;that,  expanded  and  evaluated  at run-time,  returns  the  default
		;constructor function.
		;
		;When  this field  is #f:  this  object-type has  no constructor,  so
		;trying to use the syntax NEW will cause an expand-time exception.
		;
		;When  this field  is #t:  this object-type  has no  constructor, but
		;requires the object  to be supplied in its  already-built form.  For
		;example:
		;
		;   (new <fixnum> 123)
		;
		;must expand to:
		;
		;   (assert-signature-and-return (<fixnum>) 123)
		;
		;When this field  is a symbolic expression: the  constructor is meant
		;to be used as:
		;
		;   (?constructor ?arg ...)
		;
		;and called explicitly with the NEW syntax.
		;
		;The   constructor  can   be  a   syntax  or   core  operation   like
		;"$make-clean-vector" or a closure object  like "vector" or the maker
		;of R6RS records.

    (immutable destructor-stx		object-type-spec.destructor-stx)
		;False  or a  syntax object  representing a  Scheme expression  that,
		;expanded and  evaluated at run-time, returns  a destructor function.
		;The destructor is meant to be used as:
		;
		;   (?destructor ?instance)
		;
		;and called explicitly with the DELETE syntax.

    (mutable type-predicate		object-type-spec.type-predicate object-type-spec.type-predicate-set!)
		;Either false  or:
		;
		;* A  syntax object representing  a Scheme expression  that, expanded
		;and evaluated at run-time, returns a type predicate.
		;
		;* A  procedure that, applied  to this  OTS, returns a  syntax object
		;representing the  predicate.  The return  value is memoised  in this
		;field.
		;
		;The predicate is meant to be used as:
		;
		;   (?predicate ?object)
		;
		;and called explicitly with the IS-A? syntax.
		;
		;The type  predicate can be a  syntax or core operation  or a closure
		;object like "vector?" or the predicate of R6RS records.

    (immutable equality-predicate	object-type-spec.equality-predicate)
		;False or a syntax object  which, expanded and evaluated, returns the
		;equality predicate for this type.

    (immutable comparison-procedure	object-type-spec.comparison-procedure)
		;False or a syntax object  which, expanded and evaluated, returns the
		;comparison procedure for this type.

    (immutable hash-function		object-type-spec.hash-function)
		;False or a syntax object  which, expanded and evaluated, returns the
		;hash function for this type.

    (immutable methods-table-public	object-type-spec.methods-table-public)
		;Public methods table.  Null or an alist mapping symbols representing
		;the method  names to syntax objects  representing Scheme expressions
		;that,  expanded and  evaluated  at run-time,  return the  associated
		;method.  A method is meant to be used as:
		;
		;   (?method ?instance ?arg ...)
		;
		;and called explicitly with the METHOD-CALL syntax.
		;
		;This alist includes, as tail,  the parent's public methods alist (if
		;any).  This list  is used at expand-time to search  for methods with
		;normal non-private access.

    (immutable methods-table-protected	object-type-spec.methods-table-protected)
		;Protected  methods   table.   Null  or  an   alist  mapping  symbols
		;representing the method names  to syntax objects representing Scheme
		;expressions  that, expanded  and evaluated  at run-time,  return the
		;associated method.  A method is meant to be used as:
		;
		;   (?method ?instance ?arg ...)
		;
		;and called explicitly with the METHOD-CALL syntax.
		;
		;This alist includes,  as tail, the parent's  protected methods alist
		;(if any).  This  alist exists only to be appended  to the sub-type's
		;private methods alist.

    (immutable methods-table-private	object-type-spec.methods-table-private)
		;Private   methods  table.    Null  or   an  alist   mapping  symbols
		;representing the method names  to syntax objects representing Scheme
		;expressions  that, expanded  and evaluated  at run-time,  return the
		;associated method.  A method is meant to be used as:
		;
		;   (?method ?instance ?arg ...)
		;
		;and called explicitly with the METHOD-CALL syntax.
		;
		;This alist includes,  as tail, the parent's  protected methods alist
		;(if any).   This list is used  at expand-time to search  for methods
		;for  the THIS  special syntactic  binding representing  the implicit
		;first argument of methods.

    (immutable	implemented-iterfaces	object-type-spec.implemented-interfaces)
		;A (possibly empty) proper  list of "<interface-type-spec>" instances
		;representing the implemented interfaces.

    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (define* (make-object-type-spec name uids-list {parent.ots (or not object-type-spec?)}
				      type-annotation-maker
				      constructor-stx destructor-stx type-predicate-stx
				      equality-predicate.id comparison-procedure.id hash-function.id
				      methods-table-public methods-table-protected methods-table-private
				      implemented-interfaces)
	(let ((methods-table-public	(if parent.ots
					    (append methods-table-public (object-type-spec.methods-table-public parent.ots))
					  methods-table-public))
	      (methods-table-protected	(if parent.ots
					    (append methods-table-protected (object-type-spec.methods-table-protected parent.ots))
					  methods-table-protected))
	      (methods-table-private	(if parent.ots
					    (append methods-table-private (object-type-spec.methods-table-private parent.ots))
					  methods-table-private)))
	  (make-record name uids-list parent.ots type-annotation-maker
		       constructor-stx destructor-stx type-predicate-stx
		       equality-predicate.id comparison-procedure.id hash-function.id
		       methods-table-public methods-table-protected methods-table-private
		       implemented-interfaces)))
      make-object-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[object-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define* (object-type-spec.name {ots object-type-spec?})
  (let ((thing (object-type-spec.name-or-maker ots)))
    (if (procedure? thing)
	(thing ots)
      thing)))

(define* (object-type-spec.type-annotation {ots object-type-spec?})
  ((object-type-spec.type-annotation-maker ots) ots))

(define* (object-type-spec.type-predicate-stx {ots object-type-spec?})
  (let ((thing (object-type-spec.type-predicate ots)))
    (if (procedure? thing)
	(receive-and-return (pred.stx)
	    (thing ots)
	  (object-type-spec.type-predicate-set! ots pred.stx))
      ;;Here THING is either false or a syntax object.
      thing)))

(define (list-of-object-type-spec? obj)
  (if (pair? obj)
      (and (object-type-spec? (car obj))
	   (list-of-object-type-spec? (cdr obj)))
    (null? obj)))

(define (not-empty-list-of-object-type-spec? obj)
  (and (pair? obj)
       (list-of-object-type-spec? obj)))

(define* (object-type-spec.applicable-hash-function {ots object-type-spec?})
  (cond ((object-type-spec.hash-function ots))
	((object-type-spec.parent-ots ots)
	 => object-type-spec.applicable-hash-function)
	(else #f)))

(define* (object-type-spec.ancestor-ots* {ots object-type-spec?})
  ;;Return the,  possibly empty, list of  "<object-type-spec>" instances representing
  ;;the ancestors list of OTS.  OTS itself is *not* included in the list.
  ;;
  ;;When applied to  the OTS of "<condition>",  the return value is the  list of OTSs
  ;;for:
  ;;
  ;;  (<record> <struct> <top>)
  ;;
  (cond ((object-type-spec.parent-ots ots)
	 => (lambda (parent.ots)
	      (cons parent.ots (object-type-spec.ancestor-ots* parent.ots))))
	(else '())))

(define* (object-type-spec.applicable-method-stx {ots object-type-spec?} method-name.sym)
  ;;OTS must an  object-type specification record.  METHOD-NAME.SYM must  be a symbol
  ;;representing a method name in the object-type specification.
  ;;
  ;;If METHOD-NAME.SYM is  EQ?  to an object's public method  name: return a symbolic
  ;;expression (to be BLESSed later) representing a Scheme expression which, expanded
  ;;and  evaluated at  run-time, returns  the method's  applicable; otherwise  return
  ;;false.
  ;;
  ;;NOTE  We need  to remember  that the  METHODS-TABLE field  holds entries  for the
  ;;parent  of  OTS,  so  there  is  no  need  to  traverse  the  hierarchy  of  type
  ;;specifications.
  ;;
  (cond ((assq method-name.sym (object-type-spec.methods-table-public ots))
	 ;;The name is known; extract the method implementation object from the alist
	 ;;entry and return it.
	 => cdr)
	(else #f)))

(define* (object-type-spec.applicable-private-method-stx {ots object-type-spec?} method-name.sym)
  ;;OTS must an  object-type specification record.  METHOD-NAME.SYM must  be a symbol
  ;;representing a method name in the object-type specification.
  ;;
  ;;If METHOD-NAME.SYM is EQ?  to an  object's private method name: return a symbolic
  ;;expression (to be BLESSed later) representing a Scheme expression which, expanded
  ;;and  evaluated at  run-time, returns  the method's  applicable; otherwise  return
  ;;false.
  ;;
  ;;NOTE  We need  to remember  that the  METHODS-TABLE field  holds entries  for the
  ;;parent  of  OTS,  so  there  is  no  need  to  traverse  the  hierarchy  of  type
  ;;specifications.
  ;;
  (cond ((assq method-name.sym (object-type-spec.methods-table-private ots))
	 ;;The name is known; extract the method implementation object from the alist
	 ;;entry and return it.
	 => cdr)
	(else #f)))


;;;; basic object-type specification: matching super and sub types

(module (object-type-spec.matching-super-and-sub?)
  ;;NOTE   We  must   keep  this   module  in   sync  with   the  implementation   of
  ;;OBJECT-TYPE-DESCR.MATCHING-SUPER-AND-SUB?.  (Marco Maggi; Wed Jun 22, 2016)
  ;;
  ;;NOTE  This module  implements compatibility  condition for  label specifications,
  ;;which  the function  OBJECT-TYPE-DESCR.MATCHING-SUPER-AND-SUB? does  not.  (Marco
  ;;Maggi; Wed Jun 22, 2016)

  (define-syntax (super-and-sub? stx)
    (sys::syntax-case stx ()
      ((_ ?A ?B)
       (sys::syntax (object-type-spec.matching-super-and-sub? ?A ?B)))
      (?id
       (sys::identifier? (sys::syntax ?id))
       (sys::syntax object-type-spec.matching-super-and-sub?))
      ))

  (define* (object-type-spec.matching-super-and-sub? super.ots sub.ots)
    (cond
     ((eq? super.ots sub.ots))
     ((core-type-spec?  sub.ots)	(%matching-sub/core-type-spec  super.ots sub.ots))
     ((interface-type-spec? sub.ots)	(%matching-sub/interface-type-spec super.ots sub.ots))
     ((label-type-spec? sub.ots)	(%matching-sub/label-type-spec super.ots sub.ots))
     (else
      (case-specification super.ots
	(core-type-spec?		(%matching-super/core-type-spec		super.ots sub.ots))
	(record-type-spec?		(%matching-super/record-type-spec	super.ots sub.ots))
	(struct-type-spec?		(%matching-super/struct-type-spec	super.ots sub.ots))
	(list-type-spec?		(%matching-super/list-type-spec	        super.ots sub.ots))
	(list-of-type-spec?		(%matching-super/list-of-type-spec      super.ots sub.ots))
	(vector-type-spec?		(%matching-super/vector-type-spec	super.ots sub.ots))
	(vector-of-type-spec?		(%matching-super/vector-of-type-spec    super.ots sub.ots))
	(pair-type-spec?		(%matching-super/pair-type-spec		super.ots sub.ots))
	(pair-of-type-spec?		(%matching-super/pair-of-type-spec      super.ots sub.ots))
	(compound-condition-type-spec?	(%matching-super/compound-condition-type-spec super.ots sub.ots))
	(enumeration-type-spec?		(%matching-super/enumeration-type-spec  super.ots sub.ots))
	(closure-type-spec?		(%matching-super/closure-type-spec      super.ots sub.ots))
	(hashtable-type-spec?		(%matching-super/hashtable-type-spec    super.ots sub.ots))
	(union-type-spec?		(%matching-super/union-type-spec	super.ots sub.ots))
	(intersection-type-spec?	(%matching-super/intersection-type-spec super.ots sub.ots))
	(complement-type-spec?		(%matching-super/complement-type-spec   super.ots sub.ots))
	(ancestor-of-type-spec?		(%matching-super/ancestor-of-type-spec  super.ots sub.ots))
	(else
	 (cond-with-predicates super.ots
	   (interface-type-spec?	(%matching-super/interface-type-spec super.ots sub.ots))
	   (label-type-spec?		(%matching-super/label-type-spec super.ots sub.ots))
	   (else			#f)))))))

;;; --------------------------------------------------------------------

  (module (%matching-sub/core-type-spec)

    (define (%matching-sub/core-type-spec super.ots sub.ots)
      (or (<bottom>-ots? sub.ots)
	  (cond-with-predicates super.ots
	    (core-type-spec?
	     ;;Both the descriptors are instances  of "<core-type-spec>".  We do this
	     ;;case first because it is the most likely.
	     ;;
	     ;;(matching <top>		<void>)		=> no-match
	     ;;(matching <top>		<fixnum>)	=> exact-match
	     ;;(matching <fixnum>	<fixnum>)	=> exact-match
	     ;;(matching <number>	<fixnum>)	=> exact-match
	     ;;(matching <pair>		<nelist>)	=> exact-match
	     (cond ((<top>-ots? super.ots)
		    (cond-with-predicates sub.ots
		      (<void>-ots?		#f)
		      (else			#t)))
		   (($core-type-spec=? super.ots sub.ots))
		   ((core-type-spec.parent-and-child? super.ots sub.ots))
		   ((<pair>-ots? super.ots)
		    (<nelist>-ots? sub.ots))
		   (else #f)))

	    (union-type-spec?
	     ;;(super-and-sub? (or <fixnum> <string>) <fixnum>)	=> #t
	     ;;(super-and-sub? (or <fixnum> <string>) <string>)	=> #t
	     (union-type-spec.exists super.ots
	       (lambda (super-item.ots)
		 (super-and-sub? super-item.ots sub.ots))))

	    (intersection-type-spec?
	     ;;(super-and-sub? (and <zero> <fixnum>) <zero-fixnum>) => #t
	     (intersection-type-spec.for-all super.ots
	       (lambda (super-item.ots)
		 (super-and-sub? super-item.ots sub.ots))))

	    (complement-type-spec?
	     ;;(super-and-sub? (not <number>)	<fixnum>)	=> #f
	     ;;(super-and-sub? (not <fixnum>)	<string>)	=> #t
	     (%matching/super-complement/sub-core super.ots sub.ots))

	    (ancestor-of-type-spec?
	     ;;(super-and-sub? (ancestor-of <fixnum>) <exact-integer>) => #t
	     ;;(super-and-sub? (ancestor-of <number>) <exact-integer>) => #f
	     ;;(super-and-sub? (ancestor-of <fixnum>) <string>)        => #f
	     (ancestor-of-type-spec.exists super.ots
	       (lambda (super-ancestor.ots)
		 ($object-type-spec=? super-ancestor.ots sub.ots))))

	    (list-type-spec?
	     ;;(super-and-sub? (list <top>) <pair>)		=> #t
	     ;;(super-and-sub? (list <top>) <nelist>)	=> #f
	     (and (<pair>-ots? sub.ots)
		  (list-type-spec.list-of-single-item? super.ots)
		  (<top>-ots? (list-type-spec.car super.ots))))

	    (list-of-type-spec?
	     ;;(super-and-sub? (list-of <top>) <list>)
	     ;;(super-and-sub? (list-of <top>) <nelist>)
	     ;;(super-and-sub? (list-of ?type) <null>)
	     (or (and (or-with-predicates sub.ots <list>-ots? <nelist>-ots?)
		      (<top>-ots? (list-of-type-spec.item-ots super.ots)))
		 (<null>-ots? sub.ots)))

	    (vector-of-type-spec?
	     ;;(super-and-sub? (vector-of <top>) <vector>)
	     ;;(super-and-sub? (vector-of <top>) <nevector>)
	     ;;(super-and-sub? (vector-of ?type) <empty-vector>)
	     (or (and (or-with-predicates sub.ots <vector>-ots? <nevector>-ots?)
		      (<top>-ots? (vector-of-type-spec.item-ots super.ots)))
		 (<empty-vector>-ots? sub.ots)))

	    (pair-type-spec?
	     ;;(super-and-sub? (pair <top> <top>)	<pair>)		=> #t
	     ;;(super-and-sub? (pair <top> <list>)	<list>)		=> #f
	     ;;(super-and-sub? (pair <top> <list>)	<nelist>)	=> #t
	     (or (and (<pair>-ots? sub.ots)
		      (<top>-ots? (pair-type-spec.car-ots super.ots))
		      (<top>-ots? (pair-type-spec.cdr-ots super.ots)))
		 (and (<nelist>-ots? sub.ots)
		      (<top>-ots?  (pair-type-spec.car-ots super.ots))
		      (<list>-ots? (pair-type-spec.cdr-ots super.ots)))))

	    (pair-of-type-spec?
	     ;;(super-and-sub? (pair-of <top>) <pair>)
	     (and (<pair>-ots? sub.ots)
		  (<top>-ots? (pair-of-type-spec.item-ots super.ots))))

	    (label-type-spec?
	     (%matching-super/label-type-spec super.ots sub.ots))

	    (interface-type-spec?
	     (%matching-super/interface-type-spec super.ots sub.ots))

	    (else #f))))

    (define (%matching/super-complement/sub-core super.ots sub.ots)
      (let ((super-item.ots (complement-type-spec.item-ots super.ots)))
	(cond-with-predicates super-item.ots
	  ;;(super-and-sub? (not <top>) <fixnum>)		=> #f
	  ;;(super-and-sub? (not <top>) <top>)			=> #f
	  ;;(super-and-sub? (not <top>) <void>)			=> #f
	  ;;(matching (not <top>) <fixnum>)			=> possible-match
	  ;;(matching (not <top>) <top>)			=> no-match
	  ;;(matching (not <top>) <void>)			=> possible-match
	  (<top>-ots?			#f)

	  ;;(super-and-sub? (not <void>) <fixnum>)		=> #f
	  ;;(super-and-sub? (not <void>) <top>)			=> #f
	  ;;(super-and-sub? (not <void>) <void>			=> #f
	  ;;(matching (not <void>) <fixnum>)			=> possible-match
	  ;;(matching (not <void>) <top>)			=> possible-match
	  ;;(matching (not <void>) <void>)			=> no-match
	  (<void>-ots?			#f)

	  ;;(matching (not <bottom>)	<top>)			=> possible-match
	  ;;(matching (not <bottom>)	<void>)			=> possible-match
	  (<bottom>-ots?		#f)

	  (ancestor-of-type-spec?
	   ;;(super-and-sub? (not (ancestor-of <fixnum>)) <top>)		=> #f
	   ;;(super-and-sub? (not (ancestor-of <fixnum>)) <positive-fixnum>)	=> #t
	   ;;(super-and-sub? (not (ancestor-of <fixnum>)) <flonum>)		=> #t
	   (ancestor-of-type-spec.for-all super-item.ots
	     (lambda (super-item-ancestor.ots)
	       (not ($object-type-spec=? super-item-ancestor.ots sub.ots)))))

	  (else
	   (%matching-super/complement-type-spec super.ots sub.ots)))))

    #| end of module: %MATCHING-SUB/CORE-TYPE-SPEC |# )

;;; --------------------------------------------------------------------

  (define* (%matching-super/core-type-spec super.ots sub.ots)
    ;;In   this  continuation   we   know   that:  SUPER.OTS   is   an  instance   of
    ;;"<core-type-spec>"; SUB.OTS is *not* an instance of "<core-type-spec>".
    ;;
    (cond-with-predicates super.ots
      (<top>-ots?
       ;;(super-and-sub? <top>	(ancestor-of <fixnum>))		=> #t
       ;;(super-and-sub? <top>	(not <fixnum>))			=> #f
       ;;(super-and-sub? <top>	?record-type)			=> #t
       (cond-with-predicates sub.ots
	 (ancestor-of-type-spec?	(ancestor-of-type-spec.exists sub.ots <top>-ots?))
	 (complement-type-spec?		#f)
	 (else				#t)))
      ;;(super-and-sub? <pair>	(pair <fixnum> <flonum>))	=> #t
      ;;(super-and-sub? <pair>	(pair <fixnum> <flonum>))	=> #t
      ;;(super-and-sub? <pair>	(pair-of <fixnum>))		=> #t
      ;;(super-and-sub? <pair>	(list <fixnum>))		=> #t
      (<pair>-ots?			(or-with-predicates sub.ots
					  pair-type-spec? pair-of-type-spec? list-type-spec?))
      ;;(super-and-sub? <list>	(list <fixnum> <flonum>))	=> #t
      ;;(super-and-sub? <list>	(list-of <fixnum>))		=> #t
      ;;(super-and-sub? <list>	(pair <fixnum> <list>))		=> #t
      ;;(super-and-sub? <list>	(pair-of <list>))		=> #t
      (<list>-ots?			(or-with-predicates sub.ots
					  list-type-spec? list-of-type-spec? pair-type-spec?/list pair-of-type-spec?/list))
      ;;(super-and-sub? <nelist>	(list <fixnum> <flonum>))	=> #t
      ;;(super-and-sub? <nelist>	(list-of <fixnum>))		=> #f
      ;;(super-and-sub? <nelist>	(pair <fixnum> <list>))		=> #t
      ;;(super-and-sub? <nelist>	(pair-of <list>))		=> #t
      (<nelist>-ots?			(or-with-predicates sub.ots
					  list-type-spec? pair-type-spec?/list pair-of-type-spec?/list))
      ;;(super-and-sub? <vector>	(vector <fixnum>))		=> #t
      ;;(super-and-sub? <vector>	(vector-of <fixnum>))		=> #t
      (<vector>-ots?			(or-with-predicates sub.ots vector-type-spec? vector-of-type-spec?))
      ;;(super-and-sub? <nevector>	(vector <fixnum>))		=> #t
      ;;(super-and-sub? <nevector>	(vector-of <fixnum>))		=> #f
      (<nevector>-ots?			(vector-type-spec? sub.ots))
      ;;(super-and-sub? <symbol>	(enumeration ciao))		=> #t
      (<symbol>-ots?			(enumeration-type-spec? sub.ots))
      ;;(super-and-sub? <hashtable>	(hashtable <string> <fixnum>))	=> #t
      (<hashtable>-ots?			(hashtable-type-spec? sub.ots))
      ;;(super-and-sub? <condition>	&condition)			=> #t
      ;;(super-and-sub? <condition>	(compound &who &message))	=> #t
      (<condition>-ots?			(or-with-predicates sub.ots compound-condition-type-spec? simple-condition-object-type-spec?))
      ;;(super-and-sub? <compound-condition>	(compound &who &message))	=> #t
      (<compound-condition>-ots?	(or-with-predicates sub.ots compound-condition-type-spec?))
      ;;(super-and-sub? <record>	?record-type)			=> #t
      ;;(super-and-sub? <record>	&condition)			=> #t
      ;;(super-and-sub? <record>	(compound &who &message)	=> #t
      (<record>-ots?			(or-with-predicates sub.ots record-type-spec? compound-condition-type-spec?))
      ;;(super-and-sub? <struct>	?struct-type)			=> #t
      ;;(super-and-sub? <struct>	?record-type)			=> #t
      ;;(super-and-sub? <struct>	&condition)			=> #t
      ;;(super-and-sub? <struct>	(compound &who &message)	=> #t
      ;;(super-and-sub? <struct>	(hashtable <string> <fixnum>))	=> #t
      (<struct>-ots?			(or-with-predicates sub.ots
					  record-type-spec? compound-condition-type-spec? hashtable-type-spec? struct-type-spec?))
      ;;(super-and-sub? <procedure>	(lambda (<fixnum>) => (<fixnum>)))	=> #t
      (<procedure>-ots?			(closure-type-spec? sub.ots))
      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/record-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (record-type-spec?
       ;;This  branch  includes  the  case  of SUPER.OTS  and  SUB.OTS  being  simple
       ;;condition object types.
       (record-type-spec.super-and-sub? super.ots sub.ots))

      (compound-condition-type-spec?
       ;;(super-and-sub? &who (condition &who &irritants)) => #t
       (and (simple-condition-object-type-spec? super.ots)
	    (compound-condition-type-spec.exists sub.ots
	      (lambda (sub-item.ots)
		(super-and-sub? super.ots sub-item.ots)))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/struct-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (struct-type-spec?
       ($struct-type-spec=? super.ots sub.ots))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/pair-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (pair-type-spec?
       ;;(super-and-sub? (pair <number> <string>) (pair <fixnum> <string>))	=> #t
       (and (super-and-sub? (pair-type-spec.car-ots super.ots) (pair-type-spec.car-ots sub.ots))
	    (super-and-sub? (pair-type-spec.cdr-ots super.ots) (pair-type-spec.cdr-ots sub.ots))))

      (pair-of-type-spec?
       ;;(super-and-sub? (pair <number> <integer>) (pair-of <fixnum>))	=> #t
       (let ((sub-item.ots (pair-of-type-spec.item-ots sub.ots)))
	 (and (super-and-sub? (pair-type-spec.car-ots super.ots) sub-item.ots)
	      (super-and-sub? (pair-type-spec.cdr-ots super.ots) sub-item.ots))))

      (list-type-spec?
       ;;(super-and-sub? (pair <number> <integer>) (list <fixnum> <fixnum>))	=> #t
       ;;(super-and-sub? (pair <number> <null>)    (list <fixnum>))		=> #t
       ;;(super-and-sub? (pair <number> (list-of <fixnum>)) (list <fixnum> <fixnum>))	=> #t
       (let ((sub-item*.ots (list-type-spec.item-ots* sub.ots)))
	 (and (super-and-sub? (pair-type-spec.car-ots super.ots) (car sub-item*.ots))
	      (super-and-sub? (pair-type-spec.cdr-ots super.ots) (list-type-spec.cdr sub.ots)))))

      ;;Always false with LIST-OF as SUB.OTS, because a LIST-OF may be empty.
      ;;
      ;;(super-and-sub? (pair <number> <null>) (list-of <fixnum>))	=> #f
      (list-of-type-spec?	#f)

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/pair-of-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (pair-type-spec?
       ;;(super-and-sub? (pair-of <number>) (pair <fixnum> <fixnum>))	=> #t
       (let ((super-item.ots (pair-of-type-spec.item-ots super.ots)))
	 (and (super-and-sub? super-item.ots (pair-type-spec.car-ots sub.ots))
	      (super-and-sub? super-item.ots (pair-type-spec.cdr-ots sub.ots)))))

      (pair-of-type-spec?
       ;;(super-and-sub? (pair-of <number>) (pair-of <fixnum>))	=> #t
       (super-and-sub? (pair-of-type-spec.item-ots super.ots)
		       (pair-of-type-spec.item-ots sub.ots)))

      (list-type-spec?
       ;;(super-and-sub? (pair-of <list>) (list <list> <fixnum>))	=> #t
       ;;(super-and-sub? (pair-of (or <number> <list>))
       ;;                (list <fixnum> <list>))		=> #t
       (let ((super-item.ots (pair-of-type-spec.item-ots super.ots)))
	 (and (super-and-sub? super-item.ots (list-type-spec.car sub.ots))
	      (super-and-sub? super-item.ots (list-type-spec.cdr sub.ots)))))

      ;;Always false with LIST-OF as SUB.OTS, because a LIST-OF may be empty.
      ;;
      ;;(super-and-sub? (pair-of <list>) (list-of <list>))	=> #f
      ;;(super-and-sub? (pair-of (or <number> <list>))
      ;;                (list-of <fixnum>))			=> #f
      (list-of-type-spec?	#f)

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/list-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (pair-type-spec?
       ;;(super-and-sub? (list <number>) (pair <number> <null>))	=> #t
       (and (super-and-sub? (list-type-spec.car super.ots) (pair-type-spec.car-ots sub.ots))
	    (super-and-sub? (list-type-spec.cdr super.ots) (pair-type-spec.cdr-ots sub.ots))))

      (pair-of-type-spec?
       ;;(super-and-sub? (list <list>) (pair-of <list>))	=> #t
       (let ((sub-item.ots (pair-of-type-spec.item-ots sub.ots)))
	 (and (super-and-sub? (list-type-spec.car super.ots) sub-item.ots)
	      (super-and-sub? (list-type-spec.cdr super.ots) sub-item.ots))))

      (list-type-spec?
       ;;(super-and-sub? (list <number>) (list <fixnum>))	=> #t
       (and (= (list-type-spec.length super.ots)
	       (list-type-spec.length sub.ots))
	    (for-all super-and-sub?
	      (list-type-spec.item-ots* super.ots)
	      (list-type-spec.item-ots* sub.ots))))

      ;;Always false with LIST-OF as SUB.OTS, because a LIST-OF may be empty.
      ;;
      ;;(super-and-sub? (list <number>) (list-of <fixnum>))	=> #f
      (list-of-type-spec?	#f)

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/list-of-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (pair-type-spec?
       ;;(super-and-sub? (list-of <number>) (pair <fixnum> <null>))	=> #t
       ;;(super-and-sub? (list-of <number>)
       ;;                (pair <fixnum> (list-of <fixnum>)))		=> #t
       (let ((super-item.ots (list-of-type-spec.item-ots super.ots)))
	 (and (super-and-sub? super-item.ots (pair-type-spec.car-ots sub.ots))
	      (super-and-sub? super.ots      (pair-type-spec.cdr-ots sub.ots)))))

      (pair-of-type-spec?
       ;;(super-and-sub? (list-of <list>) (pair-of <list>))	=> #t
       (let ((sub-item.ots (pair-of-type-spec.item-ots sub.ots)))
	 (and (super-and-sub? (list-of-type-spec.item-ots super.ots) sub-item.ots)
	      (object-type-spec.list-type-spec? sub-item.ots))))

      (list-type-spec?
       ;;(super-and-sub? (list-of <number>) (list <fixnum> <flonum>))	=> #t
       (let ((super-item.ots (list-of-type-spec.item-ots super.ots)))
	 (list-type-spec.for-all sub.ots
	   (lambda (sub-item.ots)
	     (super-and-sub? super-item.ots sub-item.ots)))))

      (list-of-type-spec?
       ;;(super-and-sub? (list-of <number>) (list-of <fixnum>))		=> #t
       (super-and-sub? (list-of-type-spec.item-ots super.ots)
		       (list-of-type-spec.item-ots   sub.ots)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/vector-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      ;;(super-and-sub? (vector <number>) (vector <fixnum>))	=> #t
      (vector-type-spec?
       (and (= (vector-type-spec.length super.ots)
	       (vector-type-spec.length   sub.ots))
	    (for-all super-and-sub?
	      (vector-type-spec.item-ots* super.ots)
	      (vector-type-spec.item-ots*   sub.ots))))

      ;;No VECTOR-OF because a VECTOR-OF may be empty.

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/vector-of-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (vector-type-spec?
       ;;(super-and-sub? (vector-of <number>) (vector <fixnum>))	=> #t
       (let ((super-item.ots (vector-of-type-spec.item-ots super.ots)))
	 (vector-type-spec.for-all sub.ots
	   (lambda (sub-item.ots)
	     (super-and-sub? super-item.ots sub-item.ots)))))

      (vector-of-type-spec?
       ;;(super-and-sub? (vector-of <number>) (vector-of <fixnum>))	=> #t
       (super-and-sub? (vector-of-type-spec.item-ots super.ots)
		       (vector-of-type-spec.item-ots   sub.ots)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/compound-condition-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (compound-condition-type-spec?
       ;;(super-and-sub? (compound &who &message)
       ;;                (compound &who &message))	=> #t
       (compound-condition-type-spec.for-all super.ots
	 (lambda (super-component.ots)
	   (compound-condition-type-spec.exists sub.ots
	     (lambda (sub-component.ots)
	       (super-and-sub? super-component.ots sub-component.ots))))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/enumeration-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (enumeration-type-spec?
       ;;(super-and-sub? (enumeration ciao hello) (enumeration ciao)) => #t
       (let ((super*.sym (enumeration-type-spec.symbol* super.ots)))
	 (enumeration-type-spec.for-all sub.ots
	   (lambda (sub.sym)
	     (memq sub.sym super*.sym)))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/closure-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (closure-type-spec?
       (closure-type-spec.super-and-sub? super.ots sub.ots))
      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/hashtable-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (hashtable-type-spec?
       ;;(super-and-sub? (hashtable <number> <string>)
       ;;                (hashtable <fixnum> <string>))		=> #t
       (and (super-and-sub? (hashtable-type-spec.key-ots super.ots) (hashtable-type-spec.key-ots sub.ots))
	    (super-and-sub? (hashtable-type-spec.val-ots super.ots) (hashtable-type-spec.val-ots sub.ots))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%matching-super/union-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (union-type-spec?
       ;;(super-and-sub? (or <number> <string>)
       ;;                (or <fixnum> <string>))	=> #t
       (or (union-type-spec=? super.ots sub.ots)
	   (union-type-spec.for-all sub.ots
	     (lambda (sub-item.ots)
	       (union-type-spec.exists super.ots
		 (lambda (super-item.ots)
		   (super-and-sub? super-item.ots sub-item.ots)))))))

      (else
       ;;(super-and-sub? (or <number> <string>)
       ;;                (and <fixnum> <positive>))	=> #t
       (union-type-spec.exists super.ots
	 (lambda (super-item.ots)
	   (super-and-sub? super-item.ots sub.ots))))))

;;; --------------------------------------------------------------------

  (define (%matching-super/intersection-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (intersection-type-spec?
       ;;(super-and-sub? (and <number> <positive>)
       ;;                (and <fixnum> <positive>))	=> #t
       (or (intersection-type-spec=? super.ots sub.ots)
	   (intersection-type-spec.for-all super.ots
	     (lambda (super-item.ots)
	       (intersection-type-spec.for-all sub.ots
		 (lambda (sub-item.ots)
		   (super-and-sub? super-item.ots sub-item.ots)))))))

      (else
       ;;(super-and-sub? (and <number> <positive>)
       ;;                (or <positive-fixnum> <positive-bignum>))	=> #t
       (intersection-type-spec.for-all super.ots
	 (lambda (super-item.ots)
	   (super-and-sub? super-item.ots sub.ots))))))

;;; --------------------------------------------------------------------

  (define (%matching-super/complement-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (complement-type-spec?
       ;;(super-and-sub? (not <struct>) (not <record>)) => #t
       ;;(super-and-sub? (not <record>) (not <struct>)) => #f
       ;;(super-and-sub? (not <number>) (not <fixnum>)) => #f
       ;;(super-and-sub? (not <fixnum>) (not <number>)) => #t
       ;;(super-and-sub? (not <fixnum>) (not <string>)) => #f
       ;;(super-and-sub? (not <string>) (not <fixnum>)) => #f
       (super-and-sub? (complement-type-spec.item-ots   sub.ots)
		       (complement-type-spec.item-ots super.ots)))

      (else
       ;;(super-and-sub? (not <string>) <top>)		=> #f
       ;;(super-and-sub? (not <fixnum>) <flonum>)	=> #t
       ;;(super-and-sub? (not <struct>) <record>)	=> #f
       ;;(matching (not <string>) <top>)		=> possible-match
       ;;(matching (not <fixnum>) <flonum>)		=> exact-match
       ;;(matching (not <struct>) <record>)		=> possible-match
       ;;
       ;;(super-and-sub? (not <fixnum>) (or <fixnum> <flonum>))	=> #f
       ;;(matching (not <fixnum>) (or <fixnum> <flonum>))	=> possible-match
       ;;
       ;;Let's assume the following definitions:
       ;;
       ;;   (define-record-type <duo>)
       ;;   (define-record-type <alpha>)
       ;;   (define-record-type <beta>  (parent <alpha>))
       ;;   (define-record-type <delta> (parent <beta>))
       ;;
       ;;then:
       ;;
       ;;   (matching (not <alpha>)	<duo>)		=> exact-match
       ;;   (matching (not <alpha>)	<alpha>)	=> no-match
       ;;   (matching (not <alpha>)	<beta>)		=> no-match
       ;;   (matching (not <alpha>)	<delta>)	=> no-match
       ;;   (matching (not <beta>)	<duo>)		=> exact-match
       ;;   (matching (not <beta>)	<alpha>)	=> possible-match
       ;;   (matching (not <beta>)	<beta>)		=> no-match
       ;;   (matching (not <beta>)	<delta>)	=> no-match
       ;;   (matching (not <delta>)	<duo>)		=> exact-match
       ;;   (matching (not <delta>)	<alpha>)	=> possible-match
       ;;   (matching (not <delta>)	<beta>)		=> possible-match
       ;;   (matching (not <delta>)	<delta>)	=> no-match
       ;;
       (let ((super-item.ots (complement-type-spec.item-ots super.ots)))
	 (and (not (object-type-spec.matching-super-and-sub?   super-item.ots sub.ots))
	      (not (object-type-spec.compatible-super-and-sub? super-item.ots sub.ots)))))))

;;; --------------------------------------------------------------------

  (define (%matching-super/ancestor-of-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (ancestor-of-type-spec?
       ;;(define-record-type <alpha>)
       ;;(define-record-type <beta> (parent <alpha>))
       ;;
       ;;(super-and-sub? (ancestor-of <alpha>) (ancestor-of <alpha>))	=> #t
       ;;(super-and-sub? (ancestor-of <alpha>) (ancestor-of <beta>))	=> #f
       ;;(super-and-sub? (ancestor-of <beta>)  (ancestor-of <alpha>))	=> #t
       ;;(matching (ancestor-of <alpha>) (ancestor-of <alpha>))		=> exact-match
       ;;(matching (ancestor-of <alpha>) (ancestor-of <beta>))		=> possible-match
       ;;(matching (ancestor-of <beta>)  (ancestor-of <alpha>))		=> exact-match
       (super-and-sub? (ancestor-of-type-spec.item-ots sub.ots)
		       (ancestor-of-type-spec.item-ots super.ots)))

      (complement-type-spec?
       ;;(define-record-type <alpha>)
       ;;(define-record-type <beta> (parent <alpha>))
       ;;
       ;;(super-and-sub? (ancestor-of <alpha>) (not <alpha>))	=> #f
       ;;(super-and-sub? (ancestor-of <alpha>) (not <beta>))	=> #f
       ;;(super-and-sub? (ancestor-of <beta>)  (not <alpha>))	=> #f
       ;;(matching (ancestor-of <alpha>) (not <alpha>))	=> possible-match
       ;;(matching (ancestor-of <alpha>) (not <beta>))	=> possible-match
       ;;(matching (ancestor-of <beta>)  (not <alpha>))	=> possible-match
       #f)

      (else
       (ancestor-of-type-spec.exists super.ots
	 (lambda (ancestor-super.ots)
	   ($object-type-spec=? ancestor-super.ots sub.ots))))))

;;; --------------------------------------------------------------------

  (define (%matching-super/label-type-spec super.ots sub.ots)
    ;;(define-label <my-fixnum> (parent <fixnum>))
    ;;
    ;;(matching <my-fixnum>		<my-fixnum>)		=> exact-match
    ;;(matching <my-fixnum>		<fixnum>)		=> possible-match
    ;;(matching <my-fixnum>		<positive-fixnum>)	=> possible-match
    ;;(matching <my-fixnum>		<exact-integer>)	=> possible-match
    (cond-with-predicates sub.ots
      (label-type-spec?
       (super-and-sub? (object-type-spec.parent-ots super.ots)
		       (object-type-spec.parent-ots   sub.ots)))
      (interface-type-spec?
       #f)
      (else
       (if (label-type-spec.with-type-predicate? super.ots)
	   #f
	 (super-and-sub? (object-type-spec.parent-ots super.ots) sub.ots)))))

  (define (%matching-sub/label-type-spec super.ots sub.ots)
    ;;(define-label <my-fixnum> (parent <fixnum>))
    ;;
    ;;(matching <exact-integer>		<my-fixnum>)		=> exact-match
    ;;(matching <fixnum>		<my-fixnum>)		=> exact-match
    ;;(matching <positive-fixnum>	<my-fixnum>)		=> possible-match
    (let ((sub-item.ots (object-type-spec.parent-ots sub.ots)))
      (cond-with-predicates super.ots
	(label-type-spec?
	 (super-and-sub? (object-type-spec.parent-ots super.ots) sub-item.ots))
	(interface-type-spec?
	 #f)
	(else
	 (super-and-sub? super.ots sub-item.ots)))))

;;; --------------------------------------------------------------------

  (define (%matching-super/interface-type-spec super.ots sub.ots)
    (interface-type-spec.super-and-sub? super.ots sub.ots))

  (define (%matching-sub/interface-type-spec super.ots sub.ots)
    (cond-with-predicates super.ots
      (interface-type-spec?
       (%matching-super/interface-type-spec super.ots sub.ots))
      (<top>-ots?	#t)
      (else		#f)))

;;; --------------------------------------------------------------------

  (define (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots)
    (cond-with-predicates sub.ots
      (union-type-spec?
       (union-type-spec.for-all sub.ots
	 (lambda (sub-item.ots)
	   (super-and-sub? super.ots sub-item.ots))))

      (intersection-type-spec?
       (intersection-type-spec.for-all sub.ots
	 (lambda (sub-item.ots)
	   (super-and-sub? super.ots sub-item.ots))))

      (complement-type-spec?
       #f)

      (ancestor-of-type-spec?
       (ancestor-of-type-spec.exists sub.ots
	 (lambda (sub-ancestor.ots)
	   ($object-type-spec=? super.ots sub-ancestor.ots))))

      (else #f)))

  #| end of module: OBJECT-TYPE-SPEC.MATCHING-SUPER-AND-SUB? |# )


;;;; basic object-type specification: compatible super and sub types

(module (object-type-spec.compatible-super-and-sub?)
  ;;NOTE   We  must   keep  this   module  in   sync  with   the  implementation   of
  ;;OBJECT-TYPE-DESCR.COMPATIBLE-SUPER-AND-SUB?.  (Marco Maggi; Wed Jun 22, 2016)
  ;;
  ;;NOTE  This module  implements compatibility  condition for  label specifications,
  ;;which the function  OBJECT-TYPE-DESCR.COMPATIBLE-SUPER-AND-SUB? does not.  (Marco
  ;;Maggi; Wed Jun 22, 2016)

  (define (super-and-sub? super.ots sub.ots)
    (or (object-type-spec.matching-super-and-sub?   super.ots sub.ots)
	(object-type-spec.compatible-super-and-sub? super.ots sub.ots)))

  (define (object-type-spec.compatible-super-and-sub? super.ots sub.ots)
    ;;This  function is  used to  check  for non-matching  compatibility between  two
    ;;object-type    specifications.    It    is    meant   to    be   called    when
    ;;OBJECT-TYPE-SPEC.MATCHING-SUPER-AND-SUB? has  already returned #f  when applied
    ;;to the same operands.
    ;;
    ;;As example:  a "<number>" argument  matches a "<fixnum>" operand;  a "<fixnum>"
    ;;argument is compatible with a "<number>" operand.
    ;;
    (cond
     ((core-type-spec?  sub.ots)		(%compatible-sub/core-type-spec   super.ots sub.ots))
     ((interface-type-spec? sub.ots)		#f)
     ((label-type-spec? sub.ots)		(%compatible-sub/label-type-spec  super.ots sub.ots))
     (else
      (case-specification super.ots
	(core-type-spec?			(%compatible-super/core-type-spec	  super.ots sub.ots))
	(record-type-spec?			(%compatible-super/record-type-spec	  super.ots sub.ots))
	(struct-type-spec?			(%compatible-super/struct-type-spec	  super.ots sub.ots))
	(list-type-spec?			(%compatible-super/list-type-spec	  super.ots sub.ots))
	(list-of-type-spec?			(%compatible-super/list-of-type-spec      super.ots sub.ots))
	(vector-type-spec?			(%compatible-super/vector-type-spec	  super.ots sub.ots))
	(vector-of-type-spec?			(%compatible-super/vector-of-type-spec    super.ots sub.ots))
	(pair-type-spec?			(%compatible-super/pair-type-spec	  super.ots sub.ots))
	(pair-of-type-spec?			(%compatible-super/pair-of-type-spec      super.ots sub.ots))
	(compound-condition-type-spec?		(%compatible-super/compound-condition-type-spec super.ots sub.ots))
	(enumeration-type-spec?			(%compatible-super/enumeration-type-spec  super.ots sub.ots))
	(closure-type-spec?			(%compatible-super/closure-type-spec      super.ots sub.ots))
	(hashtable-type-spec?			(%compatible-super/hashtable-type-spec    super.ots sub.ots))
	(union-type-spec?			(%compatible-super/union-type-spec	  super.ots sub.ots))
	(intersection-type-spec?		(%compatible-super/intersection-type-spec super.ots sub.ots))
	(complement-type-spec?			(%compatible-super/complement-type-spec   super.ots sub.ots))
	(ancestor-of-type-spec?			(%compatible-super/ancestor-of-type-spec  super.ots sub.ots))
	(else
	 (cond-with-predicates super.ots
	   (interface-type-spec?	#f)
	   (label-type-spec?		(%compatible-super/label-type-spec super.ots sub.ots))
	   (else
	    (object-type-spec.matching-super-and-sub? sub.ots super.ots))))))))

;;; --------------------------------------------------------------------

  (module (%compatible-sub/core-type-spec)

    (define (%compatible-sub/core-type-spec super.ots sub.ots)
      (cond-with-predicates super.ots
	(core-type-spec?
	 ;;Both the specifications are core.
	 (%compatible-super/core-type-spec super.ots sub.ots))

	(union-type-spec?
	 (union-type-spec.exists super.ots
	   (lambda (super-item.ots)
	     (super-and-sub? super-item.ots sub.ots))))

	(intersection-type-spec?
	 (intersection-type-spec.for-all super.ots
	   (lambda (super-item.ots)
	     (super-and-sub? super-item.ots sub.ots))))

	(complement-type-spec?
	 (%compatible/super-complement/sub-core super.ots sub.ots))

	(ancestor-of-type-spec?
	 ;;The ANCESTOR-OF super-type matches if one of the ancestors is equal to the
	 ;;sub-type.  There is no compatibility  condition, only exact matching or no
	 ;;matching.
	 #f)

	(label-type-spec?
	 (%compatible-super/label-type-spec super.ots sub.ots))

	(else
	 (cond-with-predicates sub.ots
	   (<top>-ots?			#t)

	   (<list>-ots?
	    ;;(matching (list <fixnum>)     <list>)		=> possible-match
	    ;;(matching (list-of <fixnum>)  <list>)		=> possible-match
	    ;;(matching (pair <top> <null>) <list>)		=> possible-match
	    ;;(matching (alist <top> <top>) <list>)		=> possible-match
	    (cond-with-predicates super.ots
	      (list-type-spec?		#t)
	      (list-of-type-spec?	#t)
	      (pair-type-spec?		(object-type-spec.compatible-list-type-spec? (pair-type-spec.cdr-ots     super.ots)))
	      (pair-of-type-spec?	(object-type-spec.compatible-list-type-spec? (pair-of-type-spec.item-ots super.ots)))
	      (else
	       (object-type-spec.matching-super-and-sub? sub.ots super.ots))))

	   (<nelist>-ots?
	    ;;(matching (list <fixnum>)     <nelist>)		=> possible-match
	    ;;(matching (list-of <fixnum>)  <nelist>)		=> possible-match
	    ;;(matching (alist <top> <top>) <nelist>)		=> possible-match
	    (cond-with-predicates super.ots
	      (list-type-spec?		#t)
	      (list-of-type-spec?	#t)
	      (pair-type-spec?		(object-type-spec.compatible-list-type-spec? (pair-type-spec.cdr-ots     super.ots)))
	      (pair-of-type-spec?	(object-type-spec.compatible-list-type-spec? (pair-of-type-spec.item-ots super.ots)))
	      (else
	       (object-type-spec.matching-super-and-sub? sub.ots super.ots))))

	   (<null>-ots?
	    ;;(matching (list <fixnum>)     <null>)		=> no-match
	    ;;(matching (list-of <fixnum>)  <null>)		=> possible-match
	    ;;(matching (alist <top> <top>) <null>)		=> possible-match
	    (or-with-predicates super.ots
	      list-of-type-spec?))

	   (<pair>-ots?
	    ;;(matching (list <fixnum>)     <pair>)		=> possible-match
	    ;;(matching (list-of <fixnum>)  <pair>)		=> possible-match
	    ;;(matching (alist <top> <top>) <pair>)		=> possible-match
	    (or-with-predicates super.ots
	      pair-type-spec? pair-of-type-spec? list-type-spec? list-of-type-spec?))

	   (<vector>-ots?
	    ;;(matching (vector <fixnum>)     <vector>)		=> possible-match
	    ;;(matching (vector-of <fixnum>)  <vector>)		=> possible-match
	    (or-with-predicates super.ots
	      vector-type-spec? vector-of-type-spec?))

	   (<nevector>-ots?
	    ;;(matching (vector <fixnum>)     <nevector>)	=> possible-match
	    ;;(matching (vector-of <fixnum>)  <nevector>)	=> possible-match
	    (or-with-predicates super.ots
	      vector-type-spec? vector-of-type-spec?))

	   (<empty-vector>-ots?
	    ;;(matching (vector <fixnum>)     <empty-vector>)	=> no-match
	    ;;(matching (vector-of <fixnum>)  <empty-vector>)	=> possible-match
	    (or-with-predicates super.ots
	      vector-of-type-spec?))

	   (<struct>-ots?
	    ;;(matching (hashtable <top> <top>) <struct>)	=> possible-match
	    (or-with-predicates super.ots
	      record-type-spec? struct-type-spec? hashtable-type-spec?))

	   (<record>-ots?
	    (or-with-predicates super.ots
	      record-type-spec?))

	   (<procedure>-ots?
	    ;;(matching (lambda (<fixnum>) => (<string>))	<procedure>)	=> possible-match
	    (closure-type-spec? super.ots))

	   (else
	    (object-type-spec.matching-super-and-sub? sub.ots super.ots))))))

    (define (%compatible/super-complement/sub-core super.ots sub.ots)
      (let ((super-item.ots (complement-type-spec.item-ots super.ots)))
	(cond-with-predicates sub.ots
	  (<top>-ots?
	   ;;(matching (not <fixnum>)    <top>)		=> possible-match
	   ;;(matching (not <top>)       <top>)		=> no-match
	   ;;(matching (not <void>)      <top>)		=> possible-match
	   (cond-with-predicates super-item.ots
	     (<top>-ots?		#f)
	     (<void>-ots?		#t)
	     (else
	      (not (object-type-spec.matching-super-and-sub? super-item.ots sub.ots)))))

	  (<void>-ots?
	   ;;(matching (not <fixnum>)    <void>)	=> possible-match
	   ;;(matching (not <top>)       <void>)	=> possible-match
	   ;;(matching (not <void>)      <void>)	=> no-match
	   (cond-with-predicates super-item.ots
	     (<top>-ots?		#t)
	     (<void>-ots?		#f)
	     (else			#t)))

	  (else
	   ;;(matching (not <fixnum>) <fixnum>)			=> no-match
	   ;;(matching (not <fixnum>) <positive-fixnum>)	=> no-match
	   ;;(matching (not <fixnum>) <number>)			=> possible-match
	   ;;(matching (not <fixnum>) <flonum>)			=> possible-match
	   ;;(matching (not <flonum>) <fixnum>)			=> possible-match
	   (%compatible-super/complement-type-spec super.ots sub.ots)))))

    #| end of module: %COMPATIBLE-SUB/CORE-TYPE-SPEC |# )

;;; --------------------------------------------------------------------

  (module (%compatible-super/core-type-spec)

    (define (%compatible-super/core-type-spec super.ots sub.ots)
      ;;Here it is possible that both the specifications are core.
      ;;
      (cond-with-predicates super.ots
	(<list>-ots?
	 ;;(matching <list> <pair>)			=> possible-match
	 ;;(matching <list> (pair <top> <list>))	=> possible-match
	 ;;(matching <list> (pair-of <list>))		=> possible-match
	 ;;(matching <list> <top>)			=> possible-match
	 (cond-with-predicates sub.ots
	   (<pair>-ots?		#t)
	   (pair-type-spec?	(pair-type-spec?/compatible-list    sub.ots))
	   (pair-of-type-spec?	(pair-of-type-spec?/compatible-list sub.ots))
	   (else
	    (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

	(<nelist>-ots?
	 ;;(matching <nelist> <top>)			=> possible-match
	 ;;(matching <nelist> <list>)			=> possible-match
	 ;;(matching <nelist> <pair>)			=> possible-match
	 ;;(matching <nelist> (pair <top> <list>))	=> possible-match
	 ;;(matching <nelist> (pair-of <list>))		=> possible-match
	 ;;(matching <nelist> (list ...))		=> possible-match
	 ;;(matching <nelist> (list-of ...))		=> possible-match
	 (cond-with-predicates sub.ots
	   (<list>-ots?		#t)
	   (<pair>-ots?		#t)
	   (pair-type-spec?	(pair-type-spec?/compatible-list    sub.ots))
	   (pair-of-type-spec?	(pair-of-type-spec?/compatible-list sub.ots))
	   (list-of-type-spec?	#t)
	   (else
	    (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

	(<null>-ots?
	 ;;(matching <null> <list>)			=> possible-match
	 ;;(matching <null> (list-of <fixnum>))		=> possible-match
	 ;;(matching <null> (alist <top> <top>))	=> possible-match
	 (or (or-with-predicates sub.ots
	       <list>-ots? list-of-type-spec?)
	     (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots)))

	(<nevector>-ots?
	 ;;(matching <nevector> <vector>)		=> possible-match
	 ;;(matching <nevector> (vector-of <fixnum>))	=> possible-match
	 (or (or-with-predicates sub.ots
	       vector-of-type-spec? <vector>-ots?)
	     (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots)))

	(<empty-vector>-ots?
	 ;;(matching <empty-vector> <vector>)		=> possible-match
	 ;;(matching <empty-vector> (vector-of <fixnum>)) => possible-match
	 (or (or-with-predicates sub.ots
	       vector-of-type-spec? <vector>-ots?)
	     (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots)))

	(<pair>-ots?
	 ;;(matching <pair> <list>)			=> possible-match
	 ;;(matching <pair> (list-of <fixnum>))		=> possible-match
	 ;;(matching <pair> (alist <top> <top>))	=> possible-match
	 (or (or-with-predicates sub.ots
	       <list>-ots? list-of-type-spec?)
	     (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots)))

	;;(matching <bottom>	?type)		=> no-match
	(<bottom>-ots?		#f)

	(else
	 (cond-with-predicates sub.ots
	   (union-type-spec?
	    ;;(matching <fixnum> (or <fixnum> <string>))	=> possible-match
	    (union-type-spec.exists sub.ots
	      (lambda (sub-item.ots)
		(super-and-sub? super.ots sub-item.ots))))

	   (intersection-type-spec?
	    ;;(matching <fixnum> (and <positive> <exact-integer>))
	    (intersection-type-spec.for-all sub.ots
	      (lambda (sub-item.ots)
		(super-and-sub? super.ots sub-item.ots))))

	   (complement-type-spec?
	    (%compatible/super-core/sub-complement super.ots sub.ots))

	   (ancestor-of-type-spec?
	    ;;(matching <integer> (ancestor-or <fixnum>))	=> possible-match
	    (ancestor-of-type-spec.exists sub.ots
	      (lambda (sub-ancestor.ots)
		($object-type-spec=? super.ots sub-ancestor.ots))))

	   (else
	    ;;(matching <fixnum> <exact-integer>)
	    (object-type-spec.matching-super-and-sub? sub.ots super.ots))))))

    (define (%compatible/super-core/sub-complement super.ots sub.ots)
      (let ((sub-item.ots (complement-type-spec.item-ots sub.ots)))
	(cond ((<top>-ots? super.ots)
	       ;;(matching <top> (not <fixnum>))		=> possible-match
	       ;;(matching <top> (not <top>))			=> no-match
	       ;;(matching <top> (not <void>))			=> possible-match
	       (cond-with-predicates sub-item.ots
		 (<top>-ots?		#f)
		 (<void>-ots?		#t)
		 (else
		  (object-type-spec.top-sub-type? sub-item.ots))))

	      ((<void>-ots? super.ots)
	       ;;(matching <void> (not <fixnum>))		=> possible-match
	       ;;(matching <void> (not <top>))			=> possible-match
	       ;;(matching <void> (not <void>))			=> no-match
	       (cond-with-predicates sub-item.ots
		 (<void>-ots?		#f)
		 (<top>-ots?		#t)
		 (else
		  (object-type-spec.top-sub-type? sub-item.ots))))

	      (($object-type-spec=? super.ots sub-item.ots)
	       ;;(matching <top> (not <top>))			=> no-match
	       #f)

	      ((object-type-spec.matching-super-and-sub? super.ots sub-item.ots)
	       ;;(matching <top>           (not <fixnum>))	=> possible-match
	       ;;(matching <exact-integer> (not <fixnum>))	=> possible-match
	       #t)

	      ((object-type-spec.matching-super-and-sub? sub-item.ots super.ots)
	       ;;(matching <fixnum>          (not <top>))	=> possible-match
	       ;;(matching <positive-fixnum> (not <fixnum>))	=> possible-match
	       #t)

	      (else
	       ;;(matching <fixnum> (not <flonum>))	=> possible-match
	       #t))))

    #| end of module: %COMPATIBLE-SUPER/CORE-TYPE-SPEC |# )

;;; --------------------------------------------------------------------

  (module (%compatible-super/record-type-spec)

    (define (%compatible-super/record-type-spec super.ots sub.ots)
      (cond-with-predicates sub.ots
	;;This  branch  includes the  case  of  SUPER.OTS  and SUB.OTS  being  simple
	;;condition object types.
	(record-type-spec?
	 (record-type-spec.super-and-sub? sub.ots super.ots))
	(complement-type-spec?
	 (%compatible/super-record/sub-complement super.ots sub.ots))
	(else
	 (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

    (define (%compatible/super-record/sub-complement super.ots sub.ots)
      ;;In the comments, let's assume the following definitions:
      ;;
      ;;   (define-record-type <duo>)
      ;;   (define-record-type <alpha>)
      ;;   (define-record-type <beta>  (parent <alpha>))
      ;;
      (let ((sub-item.ots (complement-type-spec.item-ots sub.ots)))
	(cond (($object-type-spec=? super.ots sub-item.ots)
	       ;;(matching <alpha> (not <alpha>))	=> no-match
	       #f)
	      ((object-type-spec.matching-super-and-sub? super.ots sub-item.ots)
	       ;;(matching <alpha> (not <beta>))	=> possible-match
	       #t)
	      ((object-type-spec.matching-super-and-sub? sub-item.ots super.ots)
	       ;;(matching <beta>  (not <alpha>))	=> possible-match
	       #t)
	      (else
	       ;;(matching <alpha> (not <duo>))		=> possible-match
	       #t))))

    #| end of module: %COMPATIBLE-SUPER/RECORD-TYPE-SPEC |# )

;;; --------------------------------------------------------------------

  (define (%compatible-super/struct-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (struct-type-spec?		#f)
      (complement-type-spec?
       (let ((sub-item.ots (complement-type-spec.item-ots sub.ots)))
	 (cond
	  ((object-type-spec.matching-super-and-sub? super.ots sub-item.ots)
	   #f)
	  (else #t))))
      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/pair-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (pair-type-spec?
       ;;(matching (pair <fixnum> <fixnum>) (pair <number> <number>))	=> possible-match
       (and (super-and-sub? (pair-type-spec.car-ots super.ots) (pair-type-spec.car-ots sub.ots))
	    (super-and-sub? (pair-type-spec.cdr-ots super.ots) (pair-type-spec.cdr-ots sub.ots))))

      (pair-of-type-spec?
       ;;(matching (pair <list> <list>) (pair-of <list>))	=> possible-match
       (let ((sub-item.ots (pair-of-type-spec.item-ots sub.ots)))
	 (and (super-and-sub? (pair-type-spec.car-ots super.ots) sub-item.ots)
	      (super-and-sub? (pair-type-spec.cdr-ots super.ots) sub-item.ots))))

      (list-type-spec?
       ;;(matching (pair <fixnum> <list>) (list <number>))	=> possible-match
       (and (super-and-sub? (pair-type-spec.car-ots super.ots) (list-type-spec.car sub.ots))
	    (super-and-sub? (pair-type-spec.cdr-ots super.ots) (list-type-spec.cdr sub.ots))))

      (list-of-type-spec?
       ;;(matching (pair <fixnum> <list>) (list-of <fixnum>))	=> possible-match
       (and (super-and-sub? (pair-type-spec.car-ots super.ots) (list-of-type-spec.item-ots sub.ots))
	    (super-and-sub? (pair-type-spec.cdr-ots super.ots) sub.ots)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/pair-of-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (pair-type-spec?
       ;;(matching (pair-of (list-of <fixnum>)) (pair <list> <list>))	=> possible-match
       (let ((super-item.ots (pair-of-type-spec.item-ots super.ots)))
	 (and (super-and-sub? super-item.ots (pair-type-spec.car-ots sub.ots))
	      (super-and-sub? super-item.ots (pair-type-spec.cdr-ots sub.ots)))))

      (pair-of-type-spec?
       ;;(matching (pair-of <fixnum>) (pair <number> <number>))	=> possible-match
       (super-and-sub? (pair-of-type-spec.item-ots super.ots)
		       (pair-of-type-spec.item-ots   sub.ots)))

      (list-type-spec?
       ;;(matching (pair-of <list>) (list <list>))	=> possible-match
       (let ((super-item.ots (pair-of-type-spec.item-ots super.ots)))
	 (list-type-spec.for-all sub.ots (lambda (sub-item.ots)
					    (super-and-sub? super-item.ots sub-item.ots)))))

      (list-of-type-spec?
       ;;(matching (pair-of <list>) (list-of <list>))	=> possible-match
       (let ((super-item.ots (pair-of-type-spec.item-ots super.ots)))
	 (and
	  ;;for the super's car
	  (super-and-sub? super-item.ots (list-of-type-spec.item-ots sub.ots))
	  ;;for the super's cdr
	  (super-and-sub? super-item.ots sub.ots))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/list-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (list-type-spec?
       ;;(matching (list <fixnum>) (list <number>))	=> possible-match
       (for-all super-and-sub?
	 (list-type-spec.item-ots* super.ots)
	 (list-type-spec.item-ots*   sub.ots)))

      (list-of-type-spec?
       ;;(matching (list <fixnum>) (list-of <number>))	=> possible-match
       (let ((sub-item.ots (list-of-type-spec.item-ots sub.ots)))
	 (list-type-spec.for-all super.ots
	   (lambda (super-item.ots)
	     (super-and-sub? super-item.ots sub-item.ots)))))

      (pair-type-spec?
       ;;(matching (list <fixnum>) (pair <fixnum> <null>)) => possible-match
       (and (super-and-sub? (list-type-spec.car super.ots) (pair-type-spec.car-ots sub.ots))
	    (super-and-sub? (list-type-spec.cdr super.ots) (pair-type-spec.cdr-ots sub.ots))))

      (pair-of-type-spec?
       ;;(matching (list <list>) (pair-of <list>))	=> possible-match
       (let ((sub-item.ots (pair-of-type-spec.item-ots sub.ots)))
	 (and (super-and-sub? (list-type-spec.car super.ots) sub-item.ots)
	      (super-and-sub? (list-type-spec.cdr super.ots) sub-item.ots))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/list-of-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (list-type-spec?
       ;;(matching (list-of <fixnum>) (list <number>))		=> possible-match
       (let ((super-item.ots (list-of-type-spec.item-ots super.ots)))
	 (list-type-spec.for-all sub.ots
	   (lambda (sub-item.ots)
	     (super-and-sub? super-item.ots sub-item.ots)))))

      (list-of-type-spec?
       ;;(matching (list-of <fixnum>) (list-of <number>))	=> possible-match
       (super-and-sub? (list-of-type-spec.item-ots super.ots)
		       (list-of-type-spec.item-ots   sub.ots)))

      (pair-type-spec?
       ;;(matching (list-of <fixnum>) (pair <number> <null>))	=> possible-match
       (and (super-and-sub? (list-of-type-spec.item-ots super.ots) (pair-type-spec.car-ots sub.ots))
	    (super-and-sub? super.ots                              (pair-type-spec.cdr-ots sub.ots))))

      (pair-of-type-spec?
       ;;(matching (list-of <list>) (pair-of <nelist>))		=> possible-match
       (let ((sub-item.ots (pair-of-type-spec.item-ots sub.ots)))
	 (and (super-and-sub? (list-of-type-spec.item-ots super.ots) sub-item.ots)
	      (super-and-sub? super.ots                              sub-item.ots))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/vector-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (vector-type-spec?
       ;;(matching (vector <fixnum>) (vector <number>))		=> possible-match
       (and (= (vector-type-spec.length super.ots)
	       (vector-type-spec.length   sub.ots))
	    (for-all super-and-sub?
	      (vector-type-spec.item-ots* super.ots)
	      (vector-type-spec.item-ots*   sub.ots))))

      (vector-of-type-spec?
       ;;(matching (vector <fixnum>) (vector-of <number>))	=> possible-match
       (let ((sub-item.ots (vector-of-type-spec.item-ots sub.ots)))
	 (vector-type-spec.for-all super.ots
	   (lambda (super-item.ots)
	     (super-and-sub? super-item.ots sub-item.ots)))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/vector-of-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (vector-type-spec?
       ;;(matching (vector-of <fixnum>) (vector <number>))	=> possible-match
       (let ((super-item.ots (vector-of-type-spec.item-ots super.ots)))
	 (vector-type-spec.for-all sub.ots
	   (lambda (sub-item.ots)
	     (super-and-sub? super-item.ots sub-item.ots)))))

      (vector-of-type-spec?
       ;;(matching (vector-of <fixnum>) (vector-of <number>))	=> possible-match
       (super-and-sub? (vector-of-type-spec.item-ots super.ots)
		       (vector-of-type-spec.item-ots   sub.ots)))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/compound-condition-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (compound-condition-type-spec?
       (compound-condition-type-spec.for-all super.ots
	 (lambda (super-component.ots)
	   (compound-condition-type-spec.exists sub.ots
	     (lambda (sub-component.ots)
	       (super-and-sub? super-component.ots sub-component.ots))))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/enumeration-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (enumeration-type-spec?
       ;;(matching (enumeration ciao) (enumeration ciao hello))	=> possible-match
       (let ((sub*.sym (enumeration-type-spec.symbol* sub.ots)))
	 (enumeration-type-spec.for-all super.ots
	   (lambda (super.sym)
	     (memq super.sym sub*.sym)))))

      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/closure-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      ;;There is no compatibility criterion between "<closure-type-otscr>" instances,
      ;;either the match or not.
      (closure-type-spec?	#f)
      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/hashtable-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (hashtable-type-spec?
       (and (super-and-sub? (hashtable-type-spec.key-ots super.ots)
			    (hashtable-type-spec.key-ots sub.ots))
	    (super-and-sub? (hashtable-type-spec.val-ots super.ots)
			    (hashtable-type-spec.val-ots sub.ots))))
      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/union-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (union-type-spec?
       (union-type-spec.exists super.ots
	 (lambda (super-item.ots)
	   (union-type-spec.exists sub.ots
	     (lambda (sub-item.ots)
	       (super-and-sub? super-item.ots sub-item.ots))))))

      (else
       (union-type-spec.exists super.ots
	 (lambda (super-item.ots)
	   (super-and-sub? super-item.ots sub.ots))))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/intersection-type-spec super.ots sub.ots)
    (intersection-type-spec.for-all super.ots (lambda (super-item.ots)
						 (super-and-sub? super-item.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/complement-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (complement-type-spec?
       ;;(matching (not <number>) (not <fixnum>))	=> possible-match
       ;;(matching (not <fixnum>) (not <number>))	=> exact-match
       ;;(matching (not <fixnum>) (not <string>))	=> no-match
       (super-and-sub? (complement-type-spec.item-ots super.ots)
		       (complement-type-spec.item-ots   sub.ots)))
      (else
       ;;We must remember that here SUB.OTS is  never a core type spec.  Let's assume
       ;;the following definitions:
       ;;
       ;;   (define-record-type <duo>)
       ;;   (define-record-type <alpha>)
       ;;   (define-record-type <beta>  (parent <alpha>))
       ;;   (define-record-type <delta> (parent <beta>))
       ;;
       ;;then:
       ;;
       ;;   (matching (not <alpha>)	<duo>)		=> exact-match
       ;;   (matching (not <alpha>)	<alpha>)	=> no-match
       ;;   (matching (not <alpha>)	<beta>)		=> no-match
       ;;   (matching (not <alpha>)	<delta>)	=> no-match
       ;;   (matching (not <beta>)	<duo>)		=> exact-match
       ;;   (matching (not <beta>)	<alpha>)	=> possible-match
       ;;   (matching (not <beta>)	<beta>)		=> no-match
       ;;   (matching (not <beta>)	<delta>)	=> no-match
       ;;   (matching (not <delta>)	<duo>)		=> exact-match
       ;;   (matching (not <delta>)	<alpha>)	=> possible-match
       ;;   (matching (not <delta>)	<beta>)		=> possible-match
       ;;   (matching (not <delta>)	<delta>)	=> no-match
       ;;
       (let ((super-item.ots (complement-type-spec.item-ots super.ots)))
	 (and (not (object-type-spec.matching-super-and-sub? super-item.ots sub.ots))
	      (or (object-type-spec.compatible-super-and-sub? super-item.ots sub.ots)
		  (object-type-spec.matching-super-and-sub? sub.ots super-item.ots)))))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/ancestor-of-type-spec super.ots sub.ots)
    (cond-with-predicates sub.ots
      (ancestor-of-type-spec?
       ;;(define-record-type <duo>)
       ;;(define-record-type <alpha>)
       ;;(define-record-type <beta> (parent <alpha>))
       ;;
       ;;(matching (ancestor-of <alpha>) (ancestor-of <alpha>))		=> exact-match
       ;;(matching (ancestor-of <alpha>) (ancestor-of <beta>))		=> possible-match
       ;;(matching (ancestor-of <beta>)  (ancestor-of <alpha>))		=> exact-match
       ;;(matching (ancestor-of <alpha>) (ancestor-of <duo>))		=> no-match
       (object-type-spec.matching-super-and-sub? (ancestor-of-type-spec.item-ots super.ots)
						 (ancestor-of-type-spec.item-ots sub.ots)))
      (else
       (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots))))

;;; --------------------------------------------------------------------

  (define (%compatible-super/label-type-spec super.ots sub.ots)
    ;;(define-label <my-fixnum> (parent <fixnum>))
    ;;
    ;;(matching <my-fixnum>	<my-fixnum>)		=> exact-match
    ;;(matching <my-fixnum>	<fixnum>)		=> possible-match
    ;;(matching <my-fixnum>	<positive-fixnum>)	=> possible-match
    ;;(matching <my-fixnum>	<exact-integer>)	=> possible-match
    ;;
    (cond-with-predicates sub.ots
      (label-type-spec?
       (object-type-spec.compatible-super-and-sub? (object-type-spec.parent-ots super.ots)
						   (object-type-spec.parent-ots   sub.ots)))
      (else
       (let ((super-parent.ots (object-type-spec.parent-ots super.ots)))
	 (super-and-sub? super-parent.ots sub.ots)))))

  (define (%compatible-sub/label-type-spec super.ots sub.ots)
    ;;(define-label <my-fixnum> (parent <fixnum>))
    ;;
    ;;(matching <exact-integer>		<my-fixnum>)		=> exact-match
    ;;(matching <fixnum>		<my-fixnum>)		=> possible-match
    ;;(matching <positive-fixnum>	<my-fixnum>)		=> possible-match
    (cond-with-predicates super.ots
      (label-type-spec?
       (object-type-spec.compatible-super-and-sub? (object-type-spec.parent-ots super.ots)
						   (object-type-spec.parent-ots   sub.ots)))
      (else
       (let ((sub-parent.ots (object-type-spec.parent-ots sub.ots)))
	 (or ($object-type-spec=? super.ots sub-parent.ots)
	     (object-type-spec.compatible-super-and-sub? super.ots sub-parent.ots))))))

;;; --------------------------------------------------------------------

  (define (%matching-sub/union/intersection/complement/ancestor-of super.ots sub.ots)
    (cond-with-predicates sub.ots
      (union-type-spec?
       ;;(define-record-type <alpha>)
       ;;(matching <alpha> (or <alpha> <flonum>))	=> possible-match
       (union-type-spec.exists sub.ots
	 (lambda (sub-item.ots)
	   (super-and-sub? super.ots sub-item.ots))))

      (intersection-type-spec?
       ;;(define-record-type <alpha>)
       ;;(define-record-type <beta> (parent <alpha>))
       ;;(matching <beta> (and <alpha> <struct>))	=> possible-match
       (intersection-type-spec.for-all sub.ots
	 (lambda (sub-item.ots)
	   (super-and-sub? super.ots sub-item.ots))))

      (complement-type-spec?
       (let ((sub-item.ots (complement-type-spec.item-ots sub.ots)))
	 (cond (($object-type-spec=? super.ots sub-item.ots)
		;;(matching <fixnum> (not <fixnum>))		=> no-match
		#f)

	       ((object-type-spec.matching-super-and-sub? super.ots sub-item.ots)
		;;(matching <top>           (not <fixnum>))	=> possible-match
		;;(matching <exact-integer> (not <fixnum>))	=> possible-match
		#t)

	       ((object-type-spec.matching-super-and-sub? sub-item.ots super.ots)
		;;(matching <fixnum>          (not <top>))	=> possible-match
		;;(matching <positive-fixnum> (not <fixnum>))	=> possible-match
		#t)

	       (else
		;;(matching <fixnum> (not <flonum>))	=> possible-match
		#t))))

      (ancestor-of-type-spec?
       ;;(matching <top> (ancestor-of <fixnum>))	=> possible-match
       (ancestor-of-type-spec.exists sub.ots
	 (lambda (sub-ancestor.ots)
	   ($object-type-spec=? super.ots sub-ancestor.ots))))

      (else
       (object-type-spec.matching-super-and-sub? sub.ots super.ots))))

  #| end of module: OBJECT-TYPE-SPEC.COMPATIBLE-SUPER-AND-SUB? |# )


;;;; basic object-type specification: common ancestor

(define* (object-type-spec.common-ancestor {ots1 object-type-spec?} {ots2 object-type-spec?})
  ;;Search the hierarchies of OTS1 and OTS2 looking for a common ancestor.  Return an
  ;;instance of "<object-type-spec>" representing the ancestor's OTS.  If no ancestor
  ;;is found: return the OTS of "<top>".
  ;;
  (cond ((object-type-spec=? ots1 ots2)
	 ots1)

	((or (<top>-ots?     ots1)
	     (<untyped>-ots? ots1))
	 ots1)

	((or (<top>-ots?     ots2)
	     (<untyped>-ots? ots2))
	 ots2)

	((<bottom>-ots? ots1)
	 ots2)

	((<bottom>-ots? ots2)
	 ots1)

	((<void>-ots? ots1)
	 (<void>-ots))

	((<void>-ots? ots2)
	 (<void>-ots))

	((and (pair-of-type-spec? ots1)
	      (pair-of-type-spec? ots2))
	 (make-pair-of-type-spec (object-type-spec.common-ancestor (pair-of-type-spec.item-ots ots1)
								   (pair-of-type-spec.item-ots ots2))))

	((and (list-of-type-spec? ots1)
	      (list-of-type-spec? ots2))
	 (make-list-of-type-spec (object-type-spec.common-ancestor (list-of-type-spec.item-ots ots1)
								   (list-of-type-spec.item-ots ots2))))

	((and (vector-of-type-spec? ots1)
	      (vector-of-type-spec? ots2))
	 (make-vector-of-type-spec (object-type-spec.common-ancestor (vector-of-type-spec.item-ots ots1)
								     (vector-of-type-spec.item-ots ots2))))

	((and (pair-type-spec? ots1)
	      (pair-type-spec? ots2))
	 (make-pair-type-spec (object-type-spec.common-ancestor (pair-type-spec.car-ots ots1)
								(pair-type-spec.car-ots ots2))
			      (object-type-spec.common-ancestor (pair-type-spec.cdr-ots ots1)
								(pair-type-spec.cdr-ots ots2))))

	((and (list-type-spec? ots1)
	      (list-type-spec? ots2))
	 (if (= (list-type-spec.length ots1)
		(list-type-spec.length ots2))
	     (make-null-or-list-type-spec (map object-type-spec.common-ancestor
					    (list-type-spec.item-ots* ots1)
					    (list-type-spec.item-ots* ots2)))
	   (<list>-ots)))

	((and (vector-type-spec? ots1)
	      (vector-type-spec? ots2))
	 (if (= (vector-type-spec.length ots1)
		(vector-type-spec.length ots2))
	     (make-vector-type-spec (map object-type-spec.common-ancestor
				      (vector-type-spec.item-ots* ots1)
				      (vector-type-spec.item-ots* ots2)))
	   (<vector>-ots)))

	((and (compound-condition-type-spec? ots1)
	      (compound-condition-type-spec? ots2))
	 (<compound-condition>-ots))

	((union-type-spec? ots1)
	 (object-type-spec.common-ancestor (union-type-spec.common-ancestor ots1)
					   (cond ((union-type-spec? ots2)
						  (union-type-spec.common-ancestor ots2))
						 (else ots2))))

	((union-type-spec? ots2)
	 (object-type-spec.common-ancestor ots1 (union-type-spec.common-ancestor ots2)))

	((intersection-type-spec? ots1)
	 (object-type-spec.common-ancestor (intersection-type-spec.common-ancestor ots1)
					   (cond ((intersection-type-spec? ots2)
						  (intersection-type-spec.common-ancestor ots2))
						 (else ots2))))

	((intersection-type-spec? ots2)
	 (object-type-spec.common-ancestor ots1 (intersection-type-spec.common-ancestor ots2)))

	(else
	 (let scan-parents-of-ots1 ((ots1 ots1))
	   (let scan-parents-of-ots2 ((ots2 ots2))
	     (if ($object-type-spec=? ots1 ots2)
		 ots1
	       (cond ((object-type-spec.parent-ots ots2)
		      => scan-parents-of-ots2)
		     (else
		      (cond ((object-type-spec.parent-ots ots1)
			     => scan-parents-of-ots1)
			    (else
			     (<top>-ots)))))))))))


;;;; basic object-type specification: comparison and duplicates

(define-equality/sorting-predicate object-type-spec=? $object-type-spec=? object-type-spec?)

(define ($object-type-spec=? ots1 ots2)
  (or (eq? ots1 ots2)
      (case-specification ots1
	(core-type-spec?
	 (and (core-type-spec? ots2)
	      ($core-type-spec=? ots1 ots2)))

	(record-type-spec?
	 (and (record-type-spec? ots2)
	      ($record-type-spec=? ots1 ots2)))

	(struct-type-spec?
	 (and (struct-type-spec? ots2)
	      ($struct-type-spec=? ots1 ots2)))
;;;
	(list-type-spec?
	 (and (list-type-spec? ots2)
	      ($list-type-spec=? ots1 ots2)))

	(list-of-type-spec?
	 (and (list-of-type-spec? ots2)
	      ($list-of-type-spec=? ots1 ots2)))
;;;
	(vector-type-spec?
	 (and (vector-type-spec? ots2)
	      ($vector-type-spec=? ots1 ots2)))

	(vector-of-type-spec?
	 (and (vector-of-type-spec? ots2)
	      ($vector-of-type-spec=? ots1 ots2)))
;;;
	(pair-type-spec?
	 (and (pair-type-spec? ots2)
	      ($pair-type-spec=? ots1 ots2)))

	(pair-of-type-spec?
	 (and (pair-of-type-spec? ots2)
	      ($pair-of-type-spec=? ots1 ots2)))
;;;
	(compound-condition-type-spec?
	 (and (compound-condition-type-spec? ots2)
	      ($compound-condition-type-spec=? ots1 ots2)))

	(enumeration-type-spec?
	 (and (enumeration-type-spec? ots2)
	      ($enumeration-type-spec=? ots1 ots2)))

	(closure-type-spec?
	 (and (closure-type-spec? ots2)
	      ($closure-type-spec=? ots1 ots2)))

	(hashtable-type-spec?
	 (and (hashtable-type-spec? ots2)
	      ($hashtable-type-spec=? ots1 ots2)))
;;;
	(union-type-spec?
	 (and (union-type-spec? ots2)
	      ($union-type-spec=? ots1 ots2)))

	(intersection-type-spec?
	 (and (intersection-type-spec? ots2)
	      ($intersection-type-spec=? ots1 ots2)))

	(complement-type-spec?
	 (and (complement-type-spec? ots2)
	      ($complement-type-spec=? ots1 ots2)))

	(ancestor-of-type-spec?
	 (and (ancestor-of-type-spec? ots2)
	      ($ancestor-of-type-spec=? ots1 ots2)))
;;;
	(else
	 (cond-with-predicates ots1
	   (label-type-spec?
	    (and (label-type-spec? ots2)
		 ($label-type-spec=? ots1 ots2)))

	   (interface-type-spec?
	    (and (interface-type-spec? ots2)
		 ($interface-type-spec=? ots1 ots2)))

	   (else #f))))))

;;; --------------------------------------------------------------------

(define* (core-type-spec=? {ots1 core-type-spec?} {ots2 core-type-spec?})
  ($core-type-spec=? ots1 ots2))

(define ($core-type-spec=? ots1 ots2)
  (free-identifier=? (object-type-spec.name ots1)
		     (object-type-spec.name ots2)))

;;; --------------------------------------------------------------------

(define* (record-type-spec=? {ots1 record-type-spec?} {ots2 record-type-spec?})
  ($record-type-spec=? ots1 ots2))

(define ($record-type-spec=? ots1 ots2)
  (free-identifier=? (object-type-spec.name ots1)
		     (object-type-spec.name ots2)))

;;; --------------------------------------------------------------------

(define* (struct-type-spec=? {ots1 struct-type-spec?} {ots2 struct-type-spec?})
  ($struct-type-spec=? ots1 ots2))

(define ($struct-type-spec=? ots1 ots2)
  (free-identifier=? (object-type-spec.name ots1)
		     (object-type-spec.name ots2)))

;;; --------------------------------------------------------------------

(define* (pair-of-type-spec=? {ots1 pair-of-type-spec?} {ots2 pair-of-type-spec?})
  ($pair-of-type-spec=? ots1 ots2))

(define ($pair-of-type-spec=? ots1 ots2)
  ($object-type-spec=? (pair-of-type-spec.item-ots ots1)
		       (pair-of-type-spec.item-ots ots2)))

;;; --------------------------------------------------------------------

(define* (list-of-type-spec=? {ots1 list-of-type-spec?} {ots2 list-of-type-spec?})
  ($list-of-type-spec=? ots1 ots2))

(define ($list-of-type-spec=? ots1 ots2)
  ($object-type-spec=? (list-of-type-spec.item-ots ots1)
		       (list-of-type-spec.item-ots ots2)))

;;; --------------------------------------------------------------------

(define* (vector-of-type-spec=? {ots1 vector-of-type-spec?} {ots2 vector-of-type-spec?})
  ($vector-of-type-spec=? ots1 ots2))

(define ($vector-of-type-spec=? ots1 ots2)
  ($object-type-spec=? (vector-of-type-spec.item-ots ots1)
		       (vector-of-type-spec.item-ots ots2)))

;;; --------------------------------------------------------------------

(define* (pair-type-spec=? {ots1 pair-type-spec?} {ots2 pair-type-spec?})
  ($pair-type-spec=? ots1 ots2))

(define ($pair-type-spec=? ots1 ots2)
  (and ($object-type-spec=? (pair-type-spec.car-ots ots1)
			    (pair-type-spec.car-ots ots2))
       ($object-type-spec=? (pair-type-spec.cdr-ots ots1)
			    (pair-type-spec.cdr-ots ots2))))

;;; --------------------------------------------------------------------

(define* (list-type-spec=? {ots1 list-type-spec?} {ots2 list-type-spec?})
  ($list-type-spec=? ots1 ots2))

(define ($list-type-spec=? ots1 ots2)
  (let ((item1*.ots (list-type-spec.item-ots* ots1))
	(item2*.ots (list-type-spec.item-ots* ots2)))
    (and (= (length item1*.ots)
	    (length item2*.ots))
	 (for-all $object-type-spec=? item1*.ots item2*.ots))))

;;; --------------------------------------------------------------------

(define* (vector-type-spec=? {ots1 vector-type-spec?} {ots2 vector-type-spec?})
  ($vector-type-spec=? ots1 ots2))

(define ($vector-type-spec=? ots1 ots2)
  (let ((item1*.ots (vector-type-spec.item-ots* ots1))
	(item2*.ots (vector-type-spec.item-ots* ots2)))
    (and (= (length item1*.ots)
	    (length item2*.ots))
	 (for-all $object-type-spec=? item1*.ots item2*.ots))))

;;; --------------------------------------------------------------------

(define* (compound-condition-type-spec=? {ots1 compound-condition-type-spec?} {ots2 compound-condition-type-spec?})
  ($compound-condition-type-spec=? ots1 ots2))

(define ($compound-condition-type-spec=? ots1 ots2)
  ;;Return true  if the two compound  conditions have the same  components, otherwise
  ;;return false.
  ;;
  (and (= (compound-condition-type-spec.length ots1)
	  (compound-condition-type-spec.length ots2))
       (let super-loop ((component1*.ots (compound-condition-type-spec.component-ots* ots1))
			(component2*.ots (compound-condition-type-spec.component-ots* ots2)))
	 (if (pair? component1*.ots)
	     (let sub-loop ((component2*.ots	component2*.ots)
			    (leftover2*.ots	'()))
	       (if (pair? component2*.ots)
		   (if (condition-object-component-spec=? (car component1*.ots) (car component2*.ots))
		       ;;We discard this component2.
		       (super-loop (cdr component1*.ots) (append (cdr component2*.ots) leftover2*.ots))
		     ;;We add this component2 to the leftovers.
		     (sub-loop (cdr component2*.ots) (cons (car component2*.ots) leftover2*.ots)))
		 ;;There are more component2, but no more component1.
		 #f))
	   ;;There are no more component1: are there more component2?
	   (null? component2*.ots)))))

(define (condition-object-component-spec=? obj1 obj2)
  ;;A compound condition-object's component is either:
  ;;
  ;;1. A record-type  spec ("&condition"  or one  of its  sub-types).
  ;;
  ;;2. The core type specification "<condition>".  Currently disabled.
  ;;
  ;;3. The core type specification "<compound-condition>".  Currently disabled.
  ;;
  (cond-with-predicates obj1
    (record-type-spec?
     (and (record-type-spec? obj2)
	  (record-type-spec=? obj1 obj2)))

    ;; (<condition>-ots?
    ;;  (<condition>-ots? obj2))

    ;; (<compound-condition>-ots?
    ;;  (<compound-condition>-ots? obj2))

    (else #f)))

;;; --------------------------------------------------------------------

(define* (alist-type-spec=? {ots1 alist-type-spec?} {ots2 alist-type-spec?})
  ($alist-type-spec=? ots1 ots2))

(define ($alist-type-spec=? ots1 ots2)
  (and ($object-type-spec=? (alist-type-spec.key-ots ots1)
			    (alist-type-spec.key-ots ots2))
       ($object-type-spec=? (alist-type-spec.val-ots ots1)
			    (alist-type-spec.val-ots ots2))))

;;; --------------------------------------------------------------------

(define* (enumeration-type-spec=? {ots1 enumeration-type-spec?} {ots2 enumeration-type-spec?})
  ($enumeration-type-spec=? ots1 ots2))

(define ($enumeration-type-spec=? ots1 ots2)
  ;;Return  true if  the two  enumerations have  the same  symbols, otherwise  return
  ;;false.
  ;;
  (and (= (enumeration-type-spec.length ots1)
	  (enumeration-type-spec.length ots2))
       (let super-loop ((symbol1* (enumeration-type-spec.symbol* ots1))
			(symbol2* (enumeration-type-spec.symbol* ots2)))
	 (if (pair? symbol1*)
	     (let sub-loop ((symbol2*	symbol2*)
			    (leftover2*	'()))
	       (if (pair? symbol2*)
		   (if (eq? (car symbol1*) (car symbol2*))
		       ;;We discard this symbol2.
		       (super-loop (cdr symbol1*) (append (cdr symbol2*) leftover2*))
		     ;;We add this symbol2 to the leftovers.
		     (sub-loop (cdr symbol2*) (cons (car symbol2*) leftover2*)))
		 ;;There are more symbol2, but no more symbol1.
		 #f))
	   ;;There are no more symbol1: are there more symbol2?
	   (null? symbol2*)))))

;;; --------------------------------------------------------------------

(define* (hashtable-type-spec=? {ots1 hashtable-type-spec?} {ots2 hashtable-type-spec?})
  ($hashtable-type-spec=? ots1 ots2))

(define ($hashtable-type-spec=? ots1 ots2)
  (and (object-type-spec=? (hashtable-type-spec.key-ots ots1) (hashtable-type-spec.key-ots ots2))
       (object-type-spec=? (hashtable-type-spec.val-ots ots1) (hashtable-type-spec.val-ots ots2))))

;;; --------------------------------------------------------------------

(define* (closure-type-spec=? {ots1 closure-type-spec?} {ots2 closure-type-spec?})
  ($closure-type-spec=? ots1 ots2))

(define ($closure-type-spec=? ots1 ots2)
  (case-lambda-signature=? (closure-type-spec.signature ots1)
			   (closure-type-spec.signature ots2)))

;;; --------------------------------------------------------------------

(define* (union-type-spec=? {ots1 union-type-spec?} {ots2 union-type-spec?})
  ($union-type-spec=? ots1 ots2))

(define ($union-type-spec=? ots1 ots2)
  ;;Return true if the two unions have the same components, otherwise return false.
  ;;
  (and (= (union-type-spec.length ots1)
	  (union-type-spec.length ots2))
       (let super-loop ((item1*.ots (union-type-spec.item-ots* ots1))
			(item2*.ots (union-type-spec.item-ots* ots2)))
	 (if (pair? item1*.ots)
	     (let sub-loop ((item2*.ots		item2*.ots)
			    (leftover2*.ots	'()))
	       (if (pair? item2*.ots)
		   (if ($object-type-spec=? (car item1*.ots) (car item2*.ots))
		       ;;We discard this item2.
		       (super-loop (cdr item1*.ots) (append (cdr item2*.ots) leftover2*.ots))
		     ;;We add this item2 to the leftovers.
		     (sub-loop (cdr item2*.ots) (cons (car item2*.ots) leftover2*.ots)))
		 ;;There are more item2, but no more item1.
		 #f))
	   ;;There are no more item1: are there more item2?
	   (null? item2*.ots)))))

;;; --------------------------------------------------------------------

(define* (intersection-type-spec=? {ots1 intersection-type-spec?} {ots2 intersection-type-spec?})
  ($intersection-type-spec=? ots1 ots2))

(define ($intersection-type-spec=? ots1 ots2)
  ;;Return true if the two intersections have the same components, otherwise return false.
  ;;
  (and (= (intersection-type-spec.length ots1)
	  (intersection-type-spec.length ots2))
       (let super-loop ((item1*.ots (intersection-type-spec.item-ots* ots1))
			(item2*.ots (intersection-type-spec.item-ots* ots2)))
	 (if (pair? item1*.ots)
	     (let sub-loop ((item2*.ots	item2*.ots)
			    (leftover2*.ots	'()))
	       (if (pair? item2*.ots)
		   (if ($object-type-spec=? (car item1*.ots) (car item2*.ots))
		       ;;We discard this item2.
		       (super-loop (cdr item1*.ots) (append (cdr item2*.ots) leftover2*.ots))
		     ;;We add this item2 to the leftovers.
		     (sub-loop (cdr item2*.ots) (cons (car item2*.ots) leftover2*.ots)))
		 ;;There are more item2, but no more item1.
		 #f))
	   ;;There are no more item1: are there more item2?
	   (null? item2*.ots)))))

;;; --------------------------------------------------------------------

(define* (complement-type-spec=? {ots1 complement-type-spec?} {ots2 complement-type-spec?})
  ($complement-type-spec=? ots1 ots2))

(define ($complement-type-spec=? ots1 ots2)
  ($object-type-spec=? (complement-type-spec.item-ots ots1)
		       (complement-type-spec.item-ots ots2)))

;;; --------------------------------------------------------------------

(define* (ancestor-of-type-spec=? {ots1 ancestor-of-type-spec?} {ots2 ancestor-of-type-spec?})
  ($ancestor-of-type-spec=? ots1 ots2))

(define ($ancestor-of-type-spec=? ots1 ots2)
  ($object-type-spec=? (ancestor-of-type-spec.item-ots ots1)
		       (ancestor-of-type-spec.item-ots ots2)))

;;; --------------------------------------------------------------------

(define* (label-type-spec=? {ots1 label-type-spec?} {ots2 label-type-spec?})
  ($label-type-spec=? ots1 ots2))

(define ($label-type-spec=? ots1 ots2)
  (free-identifier=? (object-type-spec.name ots1)
		     (object-type-spec.name ots2)))

;;; --------------------------------------------------------------------

(define* (interface-type-spec=? {ots1 interface-type-spec?} {ots2 interface-type-spec?})
  ($interface-type-spec=? ots1 ots2))

(define ($interface-type-spec=? ots1 ots2)
  (free-identifier=? (object-type-spec.name ots1)
		     (object-type-spec.name ots2)))

;;; --------------------------------------------------------------------

(define (object-type-specs-delete-duplicates ell)
  ;;Recursive function.  Given the list of "<object-type-spec>" instances: remove the
  ;;duplicate ones and return a proper list of unique instances.
  ;;
  (if (pair? ell)
      (let ((head (car ell)))
	(cons head (object-type-specs-delete-duplicates
		    (remp (lambda (ots)
			    (object-type-spec=? ots head))
		      (cdr ell)))))
    '()))


;;;; basic object-type specification: special predicates

(define (object-type-spec.procedure? ots)
  (or (closure-type-spec? ots)
      (<procedure>-ots? ots)))

(define (object-type-spec.list-sub-type? ots)
  (%compare-super-with-sub-and-its-parents (<list>-ots) ots))

(define (object-type-spec.vector-sub-type? ots)
  (%compare-super-with-sub-and-its-parents (<vector>-ots) ots))

(define (object-type-spec.top-sub-type? item.ots)
  (cond-with-predicates item.ots
    (<void>-ots?	#f)
    (else		#t)))

(define (%compare-super-with-sub-and-its-parents super.ots sub.ots)
  ;;Recursive function.  Search SUPER.OTS  in the hierarchy  of SUB.OTS:  return true
  ;;when found, false otherwise.
  ;;
  ;;We need to remember that there may be multiple representations of the same object
  ;;types.  For  example: there  may be  multiple instances  of "<scheme-obect-type>"
  ;;representing the same type; so we need to compare them by name.
  ;;
  (cond ((eq? super.ots sub.ots))
	(($object-type-spec=? super.ots sub.ots))
	((object-type-spec.parent-ots sub.ots)
	 => (lambda (sub-parent.ots)
	      (%compare-super-with-sub-and-its-parents super.ots sub-parent.ots)))
	(else #f)))


;;;; basic object-type specification: type descriptors

(define expression-expander-for-core-expressions
  (make-parameter (lambda (stx)
		    (assertion-violation 'expression-expander-for-core-expressions
		      "parameter not initialised"))))

(module (object-type-spec.type-descriptor-core-expr
	 type-signature.type-descriptor-core-expr)

  (define-syntax-rule (stx-expr-to-core-expr ?expr)
    ((expression-expander-for-core-expressions) ?expr (current-inferior-lexenv)))

  (define* (object-type-spec.type-descriptor-core-expr {type.ots object-type-spec?})
    ;;Given an object-type specification: build and return a core language expression
    ;;which, compiled and evaluated, returns the associated run-time type descriptor.
    ;;
    (case-specification type.ots
      (core-type-spec?
       (stx-expr-to-core-expr (core-type-spec.type-descriptor-id type.ots)))

      (record-type-spec?
       (stx-expr-to-core-expr (record-type-spec.rtd-id type.ots)))

      (struct-type-spec?
       (let* ((std (struct-type-spec.std type.ots)))
	 (build-data no-source std)))

;;; --------------------------------------------------------------------

      (list-type-spec?
       (let ((item-des*.core-expr	(map object-type-spec.type-descriptor-core-expr (list-type-spec.item-ots* type.ots))))
	 (build-application no-source
	     (build-primref no-source 'make-list-type-descr)
	   (list (build-application no-source
		     (build-primref no-source 'list)
		   item-des*.core-expr)))))

      (list-of-type-spec?
       (cond-with-predicates type.ots
	 (alist-type-spec?
	  (let ((key-des.core-expr	(object-type-spec.type-descriptor-core-expr (alist-type-spec.key-ots type.ots)))
		(val-des.core-expr	(object-type-spec.type-descriptor-core-expr (alist-type-spec.val-ots type.ots))))
	    (build-application no-source
		(build-primref no-source 'make-alist-type-descr)
	      (list key-des.core-expr val-des.core-expr))))

	 (else
	  (let ((item-des.core-expr	(object-type-spec.type-descriptor-core-expr (list-of-type-spec.item-ots type.ots))))
	    (build-application no-source
		(build-primref no-source 'make-list-of-type-descr)
	      (list item-des.core-expr))))))

;;; --------------------------------------------------------------------

      (vector-type-spec?
       (let ((item-des*.core-expr	(map object-type-spec.type-descriptor-core-expr (vector-type-spec.item-ots* type.ots))))
	 (build-application no-source
	     (build-primref no-source 'make-vector-type-descr)
	   (list (build-application no-source
		     (build-primref no-source 'list)
		   item-des*.core-expr)))))

      (vector-of-type-spec?
       (let ((item-des.core-expr	(object-type-spec.type-descriptor-core-expr (vector-of-type-spec.item-ots type.ots))))
	 (build-application no-source
	     (build-primref no-source 'make-vector-of-type-descr)
	   (list item-des.core-expr))))

;;; --------------------------------------------------------------------

      (pair-type-spec?
       (let ((car-des.core-expr	(object-type-spec.type-descriptor-core-expr (pair-type-spec.car-ots type.ots)))
	     (cdr-des.core-expr	(object-type-spec.type-descriptor-core-expr (pair-type-spec.cdr-ots type.ots))))
	 (build-application no-source
	     (build-primref no-source 'make-pair-type-descr)
	   (list car-des.core-expr
		 cdr-des.core-expr))))

      (pair-of-type-spec?
       (let ((item-des.core-expr	(object-type-spec.type-descriptor-core-expr (pair-of-type-spec.item-ots type.ots))))
	 (build-application no-source
	     (build-primref no-source 'make-pair-of-type-descr)
	   (list item-des.core-expr))))

;;; --------------------------------------------------------------------

      (compound-condition-type-spec?
       (build-application no-source
	   (build-primref no-source 'make-compound-condition-type-descr)
	 (list (build-application no-source
		   (build-primref no-source 'list)
		 (map object-type-spec.type-descriptor-core-expr
		   (compound-condition-type-spec.component-ots* type.ots))))))

      (enumeration-type-spec?
       (build-application no-source
	   (build-primref no-source 'make-enumeration-type-descr)
	 (list (build-data no-source (enumeration-type-spec.symbol* type.ots)))))


      (closure-type-spec?
       (build-application no-source
	   (build-primref no-source 'make-closure-type-descr)
	 (list (case-lambda-signature.type-descriptor-core-expr (closure-type-spec.signature type.ots)))))

      (hashtable-type-spec?
       (let ((key-des.core-expr	(object-type-spec.type-descriptor-core-expr (hashtable-type-spec.key-ots type.ots)))
	     (val-des.core-expr	(object-type-spec.type-descriptor-core-expr (hashtable-type-spec.val-ots type.ots))))
	 (build-application no-source
	     (build-primref no-source 'make-hashtable-type-descr)
	   (list key-des.core-expr val-des.core-expr))))

;;; --------------------------------------------------------------------

      (union-type-spec?
       (build-application no-source
	   (build-primref no-source 'make-union-type-descr)
	 (list (build-application no-source
		   (build-primref no-source 'list)
		 (map object-type-spec.type-descriptor-core-expr (union-type-spec.item-ots* type.ots))))))

      (intersection-type-spec?
       (build-application no-source
	   (build-primref no-source 'make-intersection-type-descr)
	 (list (build-application no-source
		   (build-primref no-source 'list)
		 (map object-type-spec.type-descriptor-core-expr (intersection-type-spec.item-ots* type.ots))))))

      (complement-type-spec?
       (build-application no-source
	   (build-primref no-source 'make-complement-type-descr)
	 (list (object-type-spec.type-descriptor-core-expr (complement-type-spec.item-ots type.ots)))))

      (ancestor-of-type-spec?
       (build-application no-source
	   (build-primref no-source 'make-ancestor-of-type-descr)
	 (list (object-type-spec.type-descriptor-core-expr (ancestor-of-type-spec.item-ots type.ots)))))

;;; --------------------------------------------------------------------

      (else
       (cond-with-predicates type.ots
	 (label-type-spec?
	  (object-type-spec.type-descriptor-core-expr (object-type-spec.parent-ots type.ots)))

	 (interface-type-spec?
	  (stx-expr-to-core-expr (interface-type-spec.type-descriptor-id type.ots)))

	 (else
	  (assertion-violation __who__ "unknown object type specification" type.ots))))))

;;; --------------------------------------------------------------------

  (define* (case-lambda-signature.type-descriptor-core-expr {sig case-lambda-signature?})
    ;;Given  a  case-lambda  signature  object  build  and  return  a  core  language
    ;;expression that,  compiled and  evaluated, returns  the associated  instance of
    ;;"<case-lambda-descriptors>".
    ;;
    (build-application no-source
	(build-primref no-source 'make-case-lambda-descriptors)
      (list (build-application no-source
		(build-primref no-source 'list)
	      (map lambda-signature.type-descriptor-core-expr
		(case-lambda-signature.clause-signature* sig))))))

  (define* (lambda-signature.type-descriptor-core-expr {sig lambda-signature?})
    ;;Given a  lambda signature object  build and  return a core  language expression
    ;;that,   compiled   and   evaluated,   returns  the   associated   instance   of
    ;;"<lambda-descriptors>".
    ;;
    (build-application no-source
	(build-primref no-source 'make-lambda-descriptors)
      (list (type-signature.type-descriptor-core-expr (lambda-signature.retvals sig))
	    (type-signature.type-descriptor-core-expr (lambda-signature.argvals sig)))))

  (define* (type-signature.type-descriptor-core-expr {sig type-signature?})
    ;;Given a type signature object build and return a core language expression that,
    ;;compiled    and    evaluated,    returns    the    associated    instance    of
    ;;"<descriptors-signature>".
    ;;
    ;;As example, for a type signature:
    ;;
    ;;   (<fixnum> <string> . <list>)
    ;;
    ;;we build a core language expression equivalent to:
    ;;
    ;;   (make-descriptors-signature (cons* <fixnum>-ctd <string>-ctd <list>-ctd))
    ;;
    (build-application no-source
	(build-primref no-source 'make-descriptors-signature)
      (list (build-application no-source
		(build-primref no-source 'cons*)
	      (let recur ((specs (type-signature.object-type-specs sig)))
		(cond ((pair? specs)
		       (cons (object-type-spec.type-descriptor-core-expr (car specs))
			     (recur (cdr specs))))
		      ((null? specs)
		       (list (build-data no-source '())))
		      (else
		       (list (object-type-spec.type-descriptor-core-expr specs)))))))))

  #| end of module: OBJECT-TYPE-SPEC.TYPE-DESCRIPTOR-CORE-EXPR |# )


;;;; basic object-type specification: validating

(define* (object-type-spec.single-value-validator-lambda-stx {type.ots object-type-spec?} return-values?)
  ;;Build and return  a syntax object representing an expression  which, expanded and
  ;;evaluated, returns a procedure that validates an expression as returning a single
  ;;value of type TYPE.OTS.
  ;;
  ;;When RETURN-VALUES? is true, the returned expression is:
  ;;
  ;;   (lambda/std (?value ?value-index ?caller-who)
  ;;     (if (?type-predicate ?value)
  ;;         ?value
  ;;       (expression-return-value-violation ?caller-who
  ;;         "return value of invalid type" ?value-index '(is-a? _ ?type-name) ?value)))
  ;;
  ;;When RETURN-VALUES? is false, the returned expression is:
  ;;
  ;;   (lambda/std (?value ?value-index ?caller-who)
  ;;     (unless (?type-predicate ?value)
  ;;       (expression-return-value-violation ?caller-who
  ;;         "return value of invalid type" ?value-index '(is-a? _ ?type-name) ?value)))
  ;;
  (let* ((value.sym		(gensym "obj"))
	 (value-index.sym	(gensym "value-index"))
	 (caller-who.sym	(gensym "caller-who"))
	 (pred.stx		(object-type-spec.type-predicate-stx type.ots))
	 (name.stx		(object-type-spec.name type.ots))
	 (test.sexp		`(,pred.stx ,value.sym))
	 (raiser.sexp		`(expression-return-value-violation ,caller-who.sym
				   "return value of invalid type" ,value-index.sym '(is-a? _ ,name.stx) ,value.sym))
	 (body.sexp		(if return-values?
				    `(if ,test.sexp ,value.sym ,raiser.sexp)
				  `(unless ,test.sexp ,raiser.sexp))))
    (bless
     `(lambda/std (,value.sym ,value-index.sym ,caller-who.sym) ,body.sexp))))

(define* (object-type-spec.list-validator-lambda-stx {type.ots object-type-spec?} return-values?)
  ;;Build and return  a syntax object representing an expression  which, expanded and
  ;;evaluated, returns a  procedure that validates an expression as  returning a list
  ;;of values of type TYPE.OTS.
  ;;
  ;;When RETURN-VALUES? is true, the returned expression is:
  ;;
  ;; (lambda/std (?list-value ?first-value-index ?caller-who)
  ;;   (fold-left (lambda/std (?value-index ?value)
  ;;                ((lambda/std (?value ?value-index ?caller-who)
  ;;                   (unless (?type-predicate ?value)
  ;;                     (expression-return-value-violation ?caller-who
  ;;                       "return value of invalid type" ?value-index '(is-a? _ ?type-name) ?value)))
  ;;                 ?value ?value-index ?caller-who)
  ;;                (fxadd1 ?value-index))
  ;;     ?first-value-index ?list-value))
  ;;
  ;;When RETURN-VALUES? is false, the returned expression is:
  ;;
  ;; (lambda/std (?list-value ?first-value-index ?caller-who)
  ;;   (fold-left (lambda/std (?value-index ?value)
  ;;                ((lambda/std (?value ?value-index ?caller-who)
  ;;                   (unless (?type-predicate ?value)
  ;;                     (expression-return-value-violation ?caller-who
  ;;                       "return value of invalid type" ?value-index '(is-a? _ ?type-name) ?value)))
  ;;                 ?value ?value-index ?caller-who)
  ;;                (fxadd1 ?value-index))
  ;;     ?first-value-index ?list-value)
  ;;   ?list-value)
  ;;
  (let* ((list-value.sym	(gensym "list-value"))
	 (first-value-index.sym	(gensym "first-value-index"))
	 (caller-who.sym	(gensym "caller-who"))
	 (item-index.sym	(gensym "item-index"))
	 (item-value.sym	(gensym "item-value"))
	 (item-validator.stx	(object-type-spec.single-value-validator-lambda-stx type.ots #f))
	 (list-validator.sexp	`(fold-left (lambda/std (,item-index.sym ,item-value.sym)
					      (,item-validator.stx ,item-value.sym ,item-index.sym ,caller-who.sym)
					      (fxadd1 ,item-index.sym))
				   ,first-value-index.sym ,list-value.sym)))
    (bless
     `(lambda/std (,list-value.sym ,first-value-index.sym ,caller-who.sym)
	,@(if return-values?
	      `(,list-validator.sexp ,list-value.sym)
	    (list list-validator.sexp))))))


;;;; built-in Scheme object-type specification
;;
;;This record-type  is the  base type  for all  the type  specifications representing
;;Scheme  objects,  not  records,  not  structs.  Instances  of  this  type  are  the
;;object-type specifications for: <fixnum>, <flonum>, <string>, <list>, ...
;;
(define-core-record-type <core-type-spec>
  (nongenerative vicare:expander:<core-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable type-descriptor-id	core-type-spec.type-descriptor-id)
		;Syntactic     identifier     bound     to     an     instance     of
		;"<core-type-descriptor>".
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-core-type-spec {name identifier?} uids-list
				      {parent.ots (or not core-type-spec?)}
				      constructor.stx type-predicate.stx
				      equality-predicate.id comparison-procedure.id hash-function.id
				      type-descriptor.id methods-table)
	(let ((destructor.stx		#f)
	      (implemented-interfaces	'()))
	  ((make-object-type-spec name uids-list parent.ots core-type-spec.type-annotation-maker
				  constructor.stx destructor.stx type-predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   type-descriptor.id)))
      make-core-type-spec))
  (custom-printer
    (lambda (S port sub-printer)
      (display "#[core-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))
  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (core-type-spec.type-annotation-maker ots)
  (object-type-spec.name ots))

;;; --------------------------------------------------------------------

(define (core-type-spec.parent-and-child? super.ots sub.ots)
  (if (or (eq? super.ots sub.ots)
	  (free-identifier=? (object-type-spec.name super.ots)
			     (object-type-spec.name   sub.ots)))
      ;;In this case they are not super and sub.
      #f
    (let loop ((parent.ots (object-type-spec.parent-ots sub.ots)))
      (and parent.ots
	   (or (eq? super.ots sub.ots)
	       (free-identifier=? (object-type-spec.name super.ots)
				  (object-type-spec.name parent.ots))
	       (loop (object-type-spec.parent-ots parent.ots)))))))


;;;; Vicare's struct-type specification
;;
;;This record-type is  used as syntactic binding descriptor's  value for struct-types
;;defined by DEFINE-STRUCT.
;;
;;Lexical variables  bound to  instances of  this type  should be  called STS  (as in
;;"Struct-Type Spec") or STRUCT-OTS.
;;
(define-core-record-type <struct-type-spec>
  (nongenerative vicare:expander:<struct-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable std			struct-type-spec.std)
		;The struct-type descriptor object.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-struct-type-spec name std constructor.id predicate.id methods-table)
	(let* ((parent.ots		(<struct>-ots))
	       (uids-list		(cons (struct-type-symbol std) (object-type-spec.uids-list parent.ots)))
	       (destructor.stx		(bless `(internal-applicable-struct-type-destructor ,std)))
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	(core-prim-id 'struct-hash))
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name uids-list parent.ots struct-type-spec.type-annotation-maker
				  constructor.id destructor.stx predicate.id
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   std)))
      make-struct-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[struct-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-STRUCT-TYPE |# )

(define (struct-type-spec.type-annotation-maker ots)
  (object-type-spec.name ots))


;;;; R6RS's record-type specification
;;
;;This  record-type  is  used  as  syntactic  binding  descriptor's  value  for  R6RS
;;record-types.
;;
;;Lexical  variables bound  to instances  of this  type and  its sub-types  should be
;;called RTS (as in "Record-Type Spec") or RECORD-OTS.
;;
(define-core-record-type <record-type-spec>
  (nongenerative vicare:expander:<record-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable rtd-id				record-type-spec.rtd-id)
		;The syntactic identifier bound to the record-type descriptor.
    (immutable rcd-id				record-type-spec.rcd-id)
		;The syntactic identifier bound to the record-constructor descriptor.
    (immutable super-protocol-id		record-type-spec.super-protocol-id)
		;False  if  this  record-type has  no  super-type  record-constructor
		;descriptor;  otherwise   the  syntactic  identifier  to   which  the
		;super-RCD is bound.
    (immutable virtual-method-signatures	record-type-spec.virtual-method-signatures)
		;An alist having: as keys, symbols representing virtual method names;
		;as values:
		;
		;*  When  the  method  is   an  open  virtual  method:  instances  of
		;"<closure-type-spec>" representing the type signature of the method.
		;
		;* When the method has been sealed: the boolean false.
		;
		;The alist holds entries from the parent's record-type specification.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-record-type-spec {type-name identifier?} uid
				      rtd-id rcd-id super-protocol-id parent-name.id
				      constructor.stx destructor.stx predicate.stx
				      equality-predicate.id comparison-procedure.id hash-function.id
				      methods-table-public methods-table-protected methods-table-private
				      virtual-method-signatures
				      {implemented-interfaces list-of-interface-type-specs?})
	(let* ((parent.ots	(cond ((<record>-type-id? parent-name.id)
				       (<record>-ots))
				      ((<condition>-type-id? parent-name.id)
				       (<condition>-ots))
				      (else
				       (with-exception-handler
					   (lambda (E)
					     (raise (condition E (make-who-condition __who__))))
					 (lambda ()
					   (id->record-type-spec parent-name.id))))))
	       (uids-list	(cons uid (object-type-spec.uids-list parent.ots)))
	       (constructor.stx	(syntax-match constructor.stx ()
				  (#f
				   (bless `(record-constructor ,rcd-id)))
				  (_
				   constructor.stx)))
	       (pred		(syntax-match predicate.stx ()
				  (#f
				   make-record-type-predicate)
				  (_
				   predicate.stx)))
	       (hash-func.id	(syntax-match hash-function.id ()
				  (#f
				   (core-prim-id 'record-hash))
				  (_
				   hash-function.id))))
	  ((make-object-type-spec type-name uids-list parent.ots record-type-spec.type-annotation-maker
				  constructor.stx destructor.stx pred
				  equality-predicate.id comparison-procedure.id hash-func.id
				  methods-table-public methods-table-protected methods-table-private
				  implemented-interfaces)
	   rtd-id rcd-id super-protocol-id virtual-method-signatures)))
      make-record-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[record-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-record-type-predicate ots)
  (let ((rtd-id  (record-type-spec.rtd-id ots))
	(arg.sym (make-syntactic-identifier-for-temporary-variable)))
    (bless
     `(lambda/typed ({_ <boolean>} ,arg.sym)
	(record-and-rtd? ,arg.sym ,rtd-id)))))

(define (simple-condition-object-type-spec? ots)
  ;;Return true  if OTS is  represents a simple condition-object  type specification;
  ;;otherwise return false.  Examples:
  ;;
  ;;   (simple-condition-object-type-spec?
  ;;      (type-annotation->object-type-spec
  ;;         (core-type-id '&condition)))
  ;;   => #t
  ;;
  ;;   (simple-condition-object-type-spec?
  ;;      (type-annotation->object-type-spec
  ;;         (core-type-id '&message)))
  ;;   => #t
  ;;
  ;;   (simple-condition-object-type-spec?
  ;;      (type-annotation->object-type-spec
  ;;         (core-type-id '<compound-condition>)))
  ;;   => #f
  ;;
  ;;   (simple-condition-object-type-spec?
  ;;      (type-annotation->object-type-spec
  ;;         (core-type-id '<condition>)))
  ;;   => #f
  ;;
  ;;Let's remember that the type hierarchy for condition objects is this:
  ;;
  ;;   <condition> --> <compound-condition> --> <compound-condition-object-type>
  ;;        |
  ;;         --------> &condition --> &who
  ;;                       |
  ;;                       |--------> &message
  ;;                       |
  ;;                        --------> ... all the condition types ...
  ;;
  (and (record-type-spec? ots)
       (%compare-super-with-sub-and-its-parents (&condition-ots) ots)))

(define (record-type-spec.type-annotation-maker ots)
  (object-type-spec.name ots))

;;; --------------------------------------------------------------------

(define* (record-type-spec.super-and-sub? {super.ots record-type-spec?} {sub.ots record-type-spec?})
  (if (or (eq? super.ots sub.ots)
	  (free-identifier=? (object-type-spec.name super.ots)
			     (object-type-spec.name   sub.ots)))
      ;;In this case they are not super and sub.
      #f
    (let loop ((parent.ots (object-type-spec.parent-ots sub.ots)))
      (and parent.ots
	   (or (eq? super.ots sub.ots)
	       (free-identifier=? (object-type-spec.name super.ots)
				  (object-type-spec.name parent.ots))
	       (loop (object-type-spec.parent-ots parent.ots)))))))


;;;; compound condition object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<compound-condition>" representing compound condition objects of a known type.
;;
(define-core-record-type <compound-condition-type-spec>
  (nongenerative vicare:expander:<compound-condition-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable	component-ots*		compound-condition-type-spec.component-ots*)
		;A list of instances of  "<record-type-spec>" describing the types of
		;component condition objects.
    (mutable	memoised-length)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-compound-condition-type-spec component-type*.ots)
	(let* ((component-type*.ots	(%splice-component-specs component-type*.ots))
	       (name			compound-condition-type-spec.type-annotation-maker)
	       (parent.ots		(<compound-condition>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-compound-condition-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots compound-condition-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   component-type*.ots #f)))

      (define (%splice-component-specs component-type*.ots)
	(fold-right (lambda (component.ots knil)
		      (cond ((simple-condition-object-type-spec? component.ots)
			     (cons component.ots knil))
			    ((compound-condition-type-spec? component.ots)
			     (append (compound-condition-type-spec.component-ots* component.ots)
				     knil))
			    (else
			     (assertion-violation 'make-compound-condition-type-spec
			       "expected condition object-type specification as component of compound condition object-type"
			       component.ots))))
	  '() component-type*.ots))

      make-compound-condition-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[compound-condition-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (compound-condition-type-spec.type-annotation-maker ots)
  (cons (core-prim-id 'condition)
	(map object-type-spec.name (compound-condition-type-spec.component-ots* ots))))

(define (make-compound-condition-type-predicate ots)
  (let* ((component-type*.ots	(compound-condition-type-spec.component-ots* ots))
	 (component-pred*.stx	(map object-type-spec.type-predicate-stx component-type*.ots))
	 (obj.sym		(make-syntactic-identifier-for-temporary-variable "obj"))
	 (pred.sym		(make-syntactic-identifier-for-temporary-variable "pred"))
	 (item.sym		(make-syntactic-identifier-for-temporary-variable "item")))
    (bless
     `(lambda/typed ({_ <boolean>} ,obj.sym)
	(and (compound-condition? ,obj.sym)
	     (for-all (lambda (,pred.sym)
			(,pred.sym ,obj.sym))
	       (list ,@component-pred*.stx))
	     #t)))))

;;; --------------------------------------------------------------------

(define* (compound-condition-type-spec.exists {compound-condition.ots compound-condition-type-spec?} {proc procedure?})
  (exists proc (compound-condition-type-spec.component-ots* compound-condition.ots)))

(define* (compound-condition-type-spec.for-all {compound-condition.ots compound-condition-type-spec?} {proc procedure?})
  (for-all proc (compound-condition-type-spec.component-ots* compound-condition.ots)))

(define* (compound-condition-type-spec.length {ots compound-condition-type-spec?})
  (or (compound-condition-type-spec-memoised-length ots)
      (receive-and-return (len)
	  (length (compound-condition-type-spec.component-ots* ots))
	(compound-condition-type-spec-memoised-length-set! ots len))))


;;;; union object spec
;;
;;This record-type  is used as  syntactic binding  descriptor's value for  type union
;;types.
;;
(define-core-record-type <union-type-spec>
  (nongenerative vicare:expander:<union-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots*		union-type-spec.item-ots*)
		;A list of instances  of "<object-type-spec>" describing the optional
		;types.
    (mutable memoised-length		union-type-spec.memoised-length union-type-spec.memoised-length-set!)
		;Initialised   to  void.    This   field  memoises   the  result   of
		;UNION-TYPE-SPEC.LENGTH.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-union-type-spec {component-type*.ots not-empty-list-of-object-type-spec?})
	(let* ((name			union-type-spec.type-annotation-maker)
	       (parent.ots		(<top>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-union-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots union-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   component-type*.ots (void))))

      make-union-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[union-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (union-type-spec.type-annotation-maker ots)
  (cons (core-prim-id 'or)
	(map object-type-spec.name (union-type-spec.item-ots* ots))))

(define (make-union-type-predicate ots)
  (let* ((component-type*.ots (union-type-spec.item-ots* ots))
	 (component-pred*.stx (map object-type-spec.type-predicate-stx component-type*.ots)))
    (for-each (lambda (type.ots pred.stx)
		(unless pred.stx
		  (assertion-violation '<union-type-spec>
		    "impossible to generate union type predicate, component type has no predicate"
		    type.ots)))
      component-type*.ots component-pred*.stx)
    (let ((obj.sym	(make-syntactic-identifier-for-temporary-variable "obj"))
	  (pred.sym	(make-syntactic-identifier-for-temporary-variable "pred")))
      (bless
       `(lambda/typed ({_ <boolean>} ,obj.sym)
	  (and (exists (lambda (,pred.sym)
			 (,pred.sym ,obj.sym))
		 (list ,@component-pred*.stx))
	       #t))))))

;;; --------------------------------------------------------------------

(module (union-of-type-specs $union-of-type-specs)

  (case-define* union-of-type-specs
    (()
     (make-type-signature/fully-unspecified))
    (({sig object-type-spec?})
     sig)
    (({sig1 object-type-spec?} {sig2 object-type-spec?})
     ($union-of-type-specs (list sig1 sig2)))
    (({sig1 object-type-spec?} {sig2 object-type-spec?} {sig3 object-type-spec?} . {sig* object-type-spec?})
     ($union-of-type-specs (cons* sig1 sig2 sig3 sig*))))

  (define* ($union-of-type-specs component-type*.ots)
    (let* ((component-type*.ots (%splice-component-specs component-type*.ots))
	   (component-type*.ots (object-type-specs-delete-duplicates component-type*.ots)))
      (cond ((list-of-single-item? component-type*.ots)
	     (car component-type*.ots))
	    ;;If there is a "<void>": the whole union collapses to "<void>".  This is
	    ;;akin to: if one operand of multiplication is NaN, the result is NaN.
	    ((find <void>-ots? component-type*.ots))
	    (else
	     ;;If there are "<bottom>" components: we filter them out.
	     (let* ((component-type*.ots (remp <bottom>-ots? component-type*.ots))
		    ;;If a component  is sub-type of another  component: the sub-type
		    ;;must be filtered out.  We do the filtering twice: left-to-right
		    ;;and right-to-left.
		    (rev-component-type*.ots	(%remove-sub-types component-type*.ots     '()))
		    (component-type*.ots	(%remove-sub-types rev-component-type*.ots '()))
		    ;;If there are both "<true>"  and "<false>": we replace them with
		    ;;a single "<boolean>".
		    (component-type*.ots	(%collapse-booleans	 component-type*.ots))
		    (component-type*.ots	(%collapse-list-of	 component-type*.ots))
		    (component-type*.ots	(%collapse-enumerations  component-type*.ots))
		    (component-type*.ots	(%collapse-lists         component-type*.ots))
		    (component-type*.ots	(%collapse-strings       component-type*.ots))
		    (component-type*.ots	(%collapse-vectors       component-type*.ots)))
	       (if (list-of-single-item? component-type*.ots)
		   (car component-type*.ots)
		 (make-union-type-spec component-type*.ots)))))))

  (define (%splice-component-specs component-type*.ots)
    (fold-right (lambda (component.ots knil)
		  (cond ((union-type-spec? component.ots)
			 (append (union-type-spec.item-ots* component.ots)
				 knil))
			(else
			 (cons component.ots knil))))
      '() component-type*.ots))

  (define (%remove-sub-types in* out*)
    ;;Recursive function.  Remove from IN* the items that are sub-types of items that
    ;;follow them.  Build and return a list of the remaining items.  Examples:
    ;;
    ;;   (%remove-super-types (list (<fixnum>-ots) (<flonum>-ots))
    ;;                        '())
    ;;   => (list (<fixnum>-ots) (<flonum>-ots))
    ;;
    ;;   (%remove-super-types (list (<fixnum>-ots) (<exact-integer>-ots))
    ;;                        '())
    ;;   => (list (<exact-integer>-ots))
    ;;
    (if (pair? in*)
	(%remove-sub-types (cdr in*)
			   (let ((ots1 (car in*)))
			     (if (exists (lambda (ots2)
					   (object-type-spec.matching-super-and-sub? ots2 ots1))
				   (cdr in*))
				 out*
			       (cons ots1 out*))))
      out*))

  (define (%collapse-booleans component-type*.ots)
    ;;If  there are  both  "<true>" and  "<false>":  we replace  them  with a  single
    ;;"<boolean>".
    ;;
    (let ((has-true?	(exists <true>-ots?    component-type*.ots))
	  (has-false?	(exists <false>-ots?   component-type*.ots))
	  (has-boolean?	(exists <boolean>-ots? component-type*.ots)))
      (if (and has-true? has-false?)
	  (let ((component-type*.ots (remp (lambda (component-type.ots)
					     (or (<true>-ots?  component-type.ots)
						 (<false>-ots? component-type.ots)))
				       component-type*.ots)))
	    (if has-boolean?
		component-type*.ots
	      (cons (<boolean>-ots) component-type*.ots)))
	component-type*.ots)))

  (define (%collapse-list-of component-type*.ots)
    ;;The LIST-OF components are collapsed into a single one:
    ;;
    ;;   (or (list-of <fixnum>) (list-of <string>))
    ;;   ==> (list-of (or <fixnum> <string>))
    ;;
    (receive (list-of*.ots other*.ots)
	(partition list-of-type-spec? component-type*.ots)
      (cond ((null? list-of*.ots)
	     component-type*.ots)
	    ((list-of-single-item? list-of*.ots)
	     component-type*.ots)
	    (else
	     (let ((item*.ots (map list-of-type-spec.item-ots list-of*.ots)))
	       (cons (make-list-of-type-spec ($union-of-type-specs item*.ots))
		     other*.ots))))))

  (define (%collapse-enumerations component-type*.ots)
    ;;If there are multiple "<enumeration-type-spec>" among the components: join them
    ;;into a  single instance.
    ;;
    (receive (enum*.ots other*.ots)
	(partition enumeration-type-spec? component-type*.ots)
      (cond ((null? enum*.ots)
	     component-type*.ots)
	    ((list-of-single-item? enum*.ots)
	     component-type*.ots)
	    (else
	     (cons (make-enumeration-type-spec (fold-left (lambda (symbol* enum.ots)
							    (list-of-symbols.union symbol* (enumeration-type-spec.symbol* enum.ots)))
						 (enumeration-type-spec.symbol* (car enum*.ots))
						 (cdr enum*.ots)))
		   other*.ots)))))

  (define (%collapse-strings component-type*.ots)
    ;;If there  are both "<empty-string>"  and "<nestring>":  we replace them  with a
    ;;single "<string>".
    ;;
    (let ((has-empty?		(exists <empty-string>-ots? component-type*.ots))
	  (has-non-empty?	(exists <nestring>-ots?     component-type*.ots))
	  (has-string?		(exists <string>-ots?       component-type*.ots)))
      (if (and has-empty? has-non-empty?)
	  (let ((component-type*.ots (remp (lambda (component-type.ots)
					     (or (<empty-string>-ots? component-type.ots)
						 (<nestring>-ots?     component-type.ots)))
				       component-type*.ots)))
	    (if has-string?
		component-type*.ots
	      (cons (<string>-ots) component-type*.ots)))
	component-type*.ots)))

  (define (%collapse-lists component-type*.ots)
    ;;If  there are  both "<null>"  and  "<nelist>": we  replace them  with a  single
    ;;"<list>".
    ;;
    (let ((has-empty?		(exists <null>-ots?   component-type*.ots))
	  (has-non-empty?	(exists <nelist>-ots? component-type*.ots))
	  (has-list?		(exists <list>-ots?   component-type*.ots)))
      (if (and has-empty? has-non-empty?)
	  (let ((component-type*.ots (remp (lambda (component-type.ots)
					     (or (<null>-ots?   component-type.ots)
						 (<nelist>-ots? component-type.ots)))
				       component-type*.ots)))
	    (if has-list?
		component-type*.ots
	      (cons (<list>-ots) component-type*.ots)))
	component-type*.ots)))

  (define (%collapse-vectors component-type*.ots)
    ;;If there  are both "<empty-vector>"  and "<nevector>":  we replace them  with a
    ;;single "<vector>".
    ;;
    (let ((has-empty?		(exists <empty-vector>-ots? component-type*.ots))
	  (has-non-empty?	(exists <nevector>-ots?     component-type*.ots))
	  (has-vector?		(exists <vector>-ots?       component-type*.ots)))
      (if (and has-empty? has-non-empty?)
	  (let ((component-type*.ots (remp (lambda (component-type.ots)
					     (or (<empty-vector>-ots? component-type.ots)
						 (<nevector>-ots?     component-type.ots)))
				       component-type*.ots)))
	    (if has-vector?
		component-type*.ots
	      (cons (<vector>-ots) component-type*.ots)))
	component-type*.ots)))

  #| end of module: UNION-OF-TYPE-SPECS |# )

(define* (union-type-spec.length {union.ots union-type-spec?})
  (let ((mem (union-type-spec.memoised-length union.ots)))
    (if (void-object? mem)
	(receive-and-return (len)
	    (length (union-type-spec.item-ots* union.ots))
	  (union-type-spec.memoised-length-set! union.ots len))
      mem)))

(define (union-type-spec.common-ancestor ots)
  (let ((component.ots* (union-type-spec.item-ots* ots)))
    (fold-left object-type-spec.common-ancestor
      (car component.ots*)
      (cdr component.ots*))))

;;; --------------------------------------------------------------------

(define (union-type-spec.exists union.ots proc)
  (exists proc (union-type-spec.item-ots* union.ots)))

(define (union-type-spec.for-all union.ots proc)
  (for-all proc (union-type-spec.item-ots* union.ots)))


;;;; intersection object spec
;;
;;This record-type is  used as syntactic binding descriptor's  value for intersection
;;types.
;;
(define-core-record-type <intersection-type-spec>
  (nongenerative vicare:expander:<intersection-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots*		intersection-type-spec.item-ots*)
		;A list of instances  of "<object-type-spec>" describing the optional
		;types.
    (mutable memoised-length		intersection-type-spec.memoised-length intersection-type-spec.memoised-length-set!)
		;Initialised   to  void.    This   field  memoises   the  result   of
		;INTERSECTION-TYPE-SPEC.LENGTH.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-object-type-spec)
      (define* (make-intersection-type-spec {component-type*.ots not-empty-list-of-object-type-spec?})
	(let* ((name			intersection-type-spec.type-annotation-maker)
	       (parent.ots		(<top>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-intersection-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots intersection-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   component-type*.ots (void))))

      make-intersection-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[intersection-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (intersection-type-spec.type-annotation-maker ots)
  (cons (core-prim-id 'and)
	(map object-type-spec.name (intersection-type-spec.item-ots* ots))))

(define (make-intersection-type-predicate ots)
  (let* ((component-type*.ots (intersection-type-spec.item-ots* ots))
	 (component-pred*.stx (map object-type-spec.type-predicate-stx component-type*.ots)))
    (for-each (lambda (type.ots pred.stx)
		(unless pred.stx
		  (assertion-violation '<intersection-type-spec>
		    "impossible to generate intersection type predicate, component type has no predicate"
		    type.ots)))
      component-type*.ots component-pred*.stx)
    (let ((obj.sym	(make-syntactic-identifier-for-temporary-variable "obj"))
	  (pred.sym	(make-syntactic-identifier-for-temporary-variable "pred")))
      (bless
       `(lambda/typed ({_ <boolean>} ,obj.sym)
	  (and (for-all (lambda (,pred.sym)
			  (,pred.sym ,obj.sym))
		 (list ,@component-pred*.stx))
	       #t))))))

;;; --------------------------------------------------------------------

(module (intersection-of-type-specs)

  (define* (intersection-of-type-specs {component-type*.ots not-empty-list-of-object-type-spec?})
    (let* ((component-type*.ots (%splice-component-specs component-type*.ots))
	   (component-type*.ots (object-type-specs-delete-duplicates component-type*.ots)))
      (cond ((list-of-single-item? component-type*.ots)
	     (car component-type*.ots))
	    ;;If there is a "<void>": the whole union collapses to "<void>".  This is
	    ;;akin to: if one operand of multiplication is NaN, the result is NaN.
	    ((find <void>-ots? component-type*.ots))
	    (else
	     ;;If there are "<bottom>" components: we filter them out.
	     (let* ((component-type*.ots	(remp <bottom>-ots? component-type*.ots))
		    ;;If  a  component  is   super-type  of  another  component:  the
		    ;;super-type must  be filtered out.   We do the  filtering twice:
		    ;;left-to-right and right-to-left.
		    (rev-component-type*.ots	(%remove-super-types component-type*.ots     '()))
		    (component-type*.ots	(%remove-super-types rev-component-type*.ots '()))
		    (component-type*.ots	(%collapse-enumerations component-type*.ots)))
	       (if (list-of-single-item? component-type*.ots)
		   (car component-type*.ots)
		 (make-intersection-type-spec component-type*.ots)))))))

  (define (%splice-component-specs component-type*.ots)
    (fold-right (lambda (component.ots knil)
		  (cond ((intersection-type-spec? component.ots)
			 (append (intersection-type-spec.item-ots* component.ots)
				 knil))
			(else
			 (cons component.ots knil))))
      '() component-type*.ots))

  (define (%remove-super-types in* out*)
    ;;Recursive function.   Remove from IN* the  items that are super-types  of items
    ;;that follow them.  Build and return a list of the remaining items.  Examples:
    ;;
    ;;   (%remove-super-types (list (<fixnum>-ots) (<flonum>-ots))
    ;;                        '())
    ;;   => (list (<fixnum>-ots) (<flonum>-ots))
    ;;
    ;;   (%remove-super-types (list (<exact-integer>-ots) (<fixnum>-ots))
    ;;                        '())
    ;;   => (list (<fixnum>-ots))
    ;;
    (if (pair? in*)
	(%remove-super-types (cdr in*)
			   (let ((ots1 (car in*)))
			     (if (exists (lambda (ots2)
					   (object-type-spec.matching-super-and-sub? ots1 ots2))
				   (cdr in*))
				 out*
			       (cons ots1 out*))))
      out*))

  (define (%collapse-enumerations component-type*.ots)
    ;;If there are multiple "<enumeration-type-spec>" among the components: join them
    ;;into a  single instance.
    ;;
    (receive (enum*.ots other*.ots)
	(partition enumeration-type-spec? component-type*.ots)
      (cond ((null? enum*.ots)
	     component-type*.ots)
	    ((list-of-single-item? enum*.ots)
	     component-type*.ots)
	    (else
	     (let* ((symbol** (map enumeration-type-spec.symbol* enum*.ots))
		    (symbol*  (fold-left list-of-symbols.intersection (car symbol**) (cdr symbol**))))
	       (cons (make-enumeration-type-spec symbol*)
		     other*.ots))))))

  #| end of module: INTERSECTION-OF-TYPE-SPECS |# )

(define* (intersection-type-spec.length {intersection.ots intersection-type-spec?})
  (let ((mem (intersection-type-spec.memoised-length intersection.ots)))
    (if (void-object? mem)
	(receive-and-return (len)
	    (length (intersection-type-spec.item-ots* intersection.ots))
	  (intersection-type-spec.memoised-length-set! intersection.ots len))
      mem)))

(define (intersection-type-spec.common-ancestor ots)
  (let ((component.ots* (intersection-type-spec.item-ots* ots)))
    (fold-left object-type-spec.common-ancestor
      (car component.ots*)
      (cdr component.ots*))))

;;; --------------------------------------------------------------------

(define (intersection-type-spec.exists intersection.ots proc)
  (exists proc (intersection-type-spec.item-ots* intersection.ots)))

(define (intersection-type-spec.for-all intersection.ots proc)
  (for-all proc (intersection-type-spec.item-ots* intersection.ots)))


;;;; complement object spec
;;
;;This record-type  is used  as syntactic binding  descriptor's value  for complement
;;types.
;;
(define-core-record-type <complement-type-spec>
  (nongenerative vicare:expander:<complement-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots			complement-type-spec.item-ots)
		;An instance of "<object-type-spec>" describing the forbidden type.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-complement-type-spec {item-type.ots object-type-spec?})
	(let* ((name			make-complement-type-name)
	       (parent.ots		(<top>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-complement-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots complement-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   item-type.ots)))

      make-complement-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[complement-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-complement-type-name ots)
  (list (core-prim-id 'not)
	(object-type-spec.name (complement-type-spec.item-ots ots))))

(define (complement-type-spec.type-annotation-maker ots)
  (list (core-prim-id 'not)
	(object-type-spec.type-annotation (complement-type-spec.item-ots ots))))

(define (make-complement-predicate ots)
  (let* ((item-type.ots (complement-type-spec.item-ots ots))
	 (item-pred.stx (object-type-spec.type-predicate-stx item-type.ots)))
    (unless item-pred.stx
      (assertion-violation '<complement-type-spec>
	"impossible to generate complement type predicate, component type has no predicate"
	item-type.ots))
    (let ((obj.sym (make-syntactic-identifier-for-temporary-variable "obj")))
      (bless
       `(lambda/typed ({_ <boolean>} ,obj.sym)
	  (not (,item-pred.stx ,obj.sym)))))))

;;; --------------------------------------------------------------------

(define* (complement-type-spec.not {complement.ots complement-type-spec?} {proc procedure?})
  (not (proc (complement-type-spec.item-ots complement.ots))))



;;;; ancestor-of object spec
;;
;;This record-type is  used as syntactic binding descriptor's  value for ANCESTOR-OF
;;types.
;;
(define-core-record-type <ancestor-of-type-spec>
  (nongenerative vicare:expander:<ancestor-of-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots			ancestor-of-type-spec.item-ots)
    (immutable ancestor-ots*		ancestor-of-type-spec.ancestor-ots*)
		;A list of instances  of "<object-type-spec>" describing the optional
		;types.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-object-type-spec)
      (define* (make-ancestor-of-type-spec {type.ots object-type-spec?})
	(let* ((name			make-ancestor-of-type-name)
	       (ancestor*.ots		(object-type-spec.ancestor-ots* type.ots))
	       (parent.ots		(<top>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-ancestor-of-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots ancestor-of-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   type.ots ancestor*.ots)))

      make-ancestor-of-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[ancestor-of-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-ancestor-of-type-name ots)
  (list (core-prim-id 'ancestor-of)
	(object-type-spec.name (ancestor-of-type-spec.item-ots ots))))

(define (ancestor-of-type-spec.type-annotation-maker ots)
  (list (core-prim-id 'ancestor-of)
	(object-type-spec.type-annotation (ancestor-of-type-spec.item-ots ots))))

(define (make-ancestor-of-predicate ots)
  (let* ((ancestor*.ots		(ancestor-of-type-spec.ancestor-ots* ots))
	 (pred*.stx		(map object-type-spec.type-predicate-stx ancestor*.ots)))
    (for-each (lambda (ancestor.ots pred.stx)
		(unless pred.stx
		  (assertion-violation '<ancestor-of-type-spec>
		    "impossible to generate ancestor-of type predicate, component type has no predicate"
		    ancestor.ots)))
      ancestor*.ots pred*.stx)
    (let ((obj.sym	(make-syntactic-identifier-for-temporary-variable "obj"))
	  (pred.sym	(make-syntactic-identifier-for-temporary-variable "pred")))
      (bless
       `(lambda/typed ({_ <boolean>} ,obj.sym)
	  (and (exists (lambda (,pred.sym)
			 (,pred.sym ,obj.sym))
		 (list ,@pred*.stx))
	       #t))))))

;;; --------------------------------------------------------------------

(define* (ancestor-of-type-spec.exists {ancestor-of.ots ancestor-of-type-spec?} {proc procedure?})
  (exists  proc (ancestor-of-type-spec.ancestor-ots* ancestor-of.ots)))

(define* (ancestor-of-type-spec.for-all {ancestor-of.ots ancestor-of-type-spec?} {proc procedure?})
  (for-all proc (ancestor-of-type-spec.ancestor-ots* ancestor-of.ots)))


;;;; closure object signature spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<procedure>" representing closure objects defined in the source code.
;;
;;It is built  when expanding a DEFINE,  LAMBDA or CASE-LAMBDA form  to represent the
;;signature of arguments and return values.
;;
;;NOTE There is  no predicate sexp because,  at run-time, there is no  way to inspect
;;the signature of a closure object.
;;
(define-core-record-type <closure-type-spec>
  (nongenerative vicare:expander:<closure-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (mutable signature		closure-type-spec.signature closure-type-spec.signature-set!)
		;An instance  of "<case-lambda-signature>".   It is mutable  to allow
		;expand-time type propagation.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-closure-type-spec
	(({signature case-lambda-signature?})
	 ($make-closure-type-spec signature closure-type-spec.type-annotation-maker))
	(({signature case-lambda-signature?} name.stx)
	 ($make-closure-type-spec signature name.stx)))

      (define ($make-closure-type-spec signature name)
	(let ((parent.ots		(<procedure>-ots))
	      (constructor.stx		#f)
	      (destructor.stx		#f)
	      (predicate.stx		(core-prim-id 'procedure?))
	      (equality-predicate.id	#f)
	      (comparison-procedure.id	#f)
	      (hash-function.id		#f)
	      (methods-table		'())
	      (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots closure-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   signature)))

      make-closure-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[closure-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (closure-type-spec.type-annotation-maker ots)
  (let* ((signature	(closure-type-spec.signature ots))
	 (clause*	(map (lambda (clause-signature)
			       (list (type-signature.syntax-object (lambda-signature.argvals clause-signature))
				     (core-prim-id '=>)
				     (type-signature.syntax-object (lambda-signature.retvals clause-signature))))
			  (case-lambda-signature.clause-signature* signature))))
    (if (list-of-single-item? clause*)
	(cons (core-prim-id 'lambda) (car clause*))
      (cons (core-prim-id 'case-lambda) clause*))))

;;; --------------------------------------------------------------------

(define* (closure-type-spec.thunk? {ots closure-type-spec?})
  ;;Return true if at least one of the clause signatures in OTS accepts no arguments;
  ;;otherwise return false.
  ;;
  (exists (lambda (csig)
	    (let ((specs (lambda-signature.argvals.specs csig)))
	      (or (null? specs)
		  (<list>-ots? specs)
		  (list-of-type-spec? specs))))
    (case-lambda-signature.clause-signature* (closure-type-spec.signature ots))))

;;; --------------------------------------------------------------------

(define* (closure-type-spec.set-new-retvals-when-untyped! {closure.ots closure-type-spec?} retvals*.new-sig)
  (closure-type-spec.signature-set! closure.ots
    (make-case-lambda-signature
     (map (lambda (clause-signature retvals.new-sig)
	    ;;If  no   type  signature   was  specified  for   the  clause:   we  use
	    ;;RETVALS.NEW-SIG.
	    (if (type-signature.fully-unspecified? (lambda-signature.retvals clause-signature))
		(make-lambda-signature retvals.new-sig
				       (lambda-signature.argvals clause-signature))
	      clause-signature))
       (case-lambda-signature.clause-signature* (closure-type-spec.signature closure.ots))
       retvals*.new-sig))))

;;; --------------------------------------------------------------------

(define* (closure-type-spec.super-and-sub? {ots1 closure-type-spec?} {ots2 closure-type-spec?})
  (case-lambda-signature.super-and-sub? (closure-type-spec.signature ots1)
					(closure-type-spec.signature ots2)))

(define* (closure-type-spec.match-formals-against-operands {formals.ots  closure-type-spec?}
							   {operands.sig type-signature?})
  (case-lambda-signature.match-formals-against-operands (closure-type-spec.signature formals.ots) operands.sig))

(define* (closure-type-spec.join {ots1 closure-type-spec?} {ots2 closure-type-spec?})
  (make-closure-type-spec (case-lambda-signature.join (closure-type-spec.signature ots1)
						      (closure-type-spec.signature ots2))))


;;;; heterogeneous pair object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<pair>" representing pair of objects holding items of a known type.
;;
(define-core-record-type <pair-type-spec>
  (nongenerative vicare:expander:<pair-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable car-ots		pair-type-spec.car-ots)
		;An  instance of  "<object-type-spec>" representing  the type  of the
		;car.
    (immutable cdr-ots		pair-type-spec.cdr-ots)
		;An  instance of  "<object-type-spec>" representing  the type  of the
		;cdr.
    (mutable memoised-homogeneous?	pair-type-spec.memoised-homogeneous? pair-type-spec.memoised-homogeneous?-set!)
		;Initialised   to  void.    This   field  memoises   the  result   of
		;PAIR-TYPE-SPEC.HOMOGENEOUS?.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-pair-type-spec
	(({car.ots object-type-spec?} {cdr.ots object-type-spec?})
	 ($make-pair-type-spec car.ots cdr.ots make-pair-type-name))
	(({car.ots object-type-spec?} {cdr.ots object-type-spec?} {name.stx pair-name?})
	 ($make-pair-type-spec car.ots cdr.ots name.stx)))

      (define ($make-pair-type-spec car.ots cdr.ots name)
	 (let* ((parent.ots		(<pair>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		make-pair-predicate)
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(methods-table		'())
		(implemented-interfaces	'()))
	   ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				   parent.ots pair-type-spec.type-annotation-maker
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   methods-table methods-table methods-table implemented-interfaces)
	    car.ots cdr.ots (void))))

      (define (pair-name? name.stx)
	(syntax-match name.stx (pair nelist-of)
	  ((pair ?car-type ?cdr-type)
	   (and (syntax-object.type-annotation? ?car-type)
		(syntax-object.type-annotation? ?cdr-type))
	   #t)
	  ((nelist-of ?type)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      make-pair-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[pair-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-pair-type-name ots)
  (list (core-prim-id 'pair)
	(object-type-spec.name (pair-type-spec.car-ots ots))
	(object-type-spec.name (pair-type-spec.cdr-ots ots))))

(define (pair-type-spec.type-annotation-maker ots)
  (list (core-prim-id 'pair)
	(object-type-spec.type-annotation (pair-type-spec.car-ots ots))
	(object-type-spec.type-annotation (pair-type-spec.cdr-ots ots))))

(define (make-pair-predicate ots)
  (let ((car-pred.stx	(object-type-spec.type-predicate-stx (pair-type-spec.car-ots ots)))
	(cdr-pred.stx	(object-type-spec.type-predicate-stx (pair-type-spec.cdr-ots ots)))
	(obj.sym	(make-syntactic-identifier-for-temporary-variable "obj")))
    (bless
     `(lambda/typed ({_ <boolean>} ,obj.sym)
	(and (pair? ,obj.sym)
	     (,car-pred.stx (car ,obj.sym))
	     (,cdr-pred.stx (cdr ,obj.sym))
	     #t)))))

;;; --------------------------------------------------------------------

(define* (pair-type-spec.homogeneous? {pair.ots pair-type-spec?})
  (let ((mem (pair-type-spec.memoised-homogeneous? pair.ots)))
    (if (boolean? mem)
	mem
      (receive-and-return (bool)
	  (object-type-spec=? (pair-type-spec.car-ots pair.ots)
			      (pair-type-spec.cdr-ots pair.ots))
	(pair-type-spec.memoised-homogeneous?-set! pair.ots bool)))))

(define (pair-type-spec?/list ots)
  (and (pair-type-spec? ots)
       (object-type-spec.list-type-spec? (pair-type-spec.cdr-ots ots))))

(define (pair-type-spec?/compatible-list ots)
  (and (pair-type-spec? ots)
       (object-type-spec.compatible-list-type-spec? (pair-type-spec.cdr-ots ots))))


;;;; homogeneous pair object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<pair>" representing pair of objects holding items of the same type.
;;
(define-core-record-type <pair-of-type-spec>
  (nongenerative vicare:expander:<pair-of-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots		pair-of-type-spec.item-ots)
		;An  instance of  "<object-type-spec>" representing  the type  of the
		;both the car and cdr.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-pair-of-type-spec
	(({item.ots object-type-spec?})
	 ($make-pair-of-type-spec item.ots make-pair-of-type-name))
	(({item.ots object-type-spec?} {name.stx pair-of-name?})
	 ($make-pair-of-type-spec item.ots name.stx)))

      (define ($make-pair-of-type-spec item.ots name)
	 (let* ((parent.ots		(<pair>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		make-pair-of-predicate)
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(methods-table		'())
		(implemented-interfaces	'()))
	   ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				   parent.ots pair-of-type-spec.type-annotation-maker
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   methods-table methods-table methods-table implemented-interfaces)
	    item.ots)))

      (define (pair-of-name? name.stx)
	(syntax-match name.stx (pair-of)
	  ((pair-of ?item-type)
	   (syntax-object.type-annotation? ?item-type)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      make-pair-of-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[pair-of-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-pair-of-type-name ots)
  (list (core-prim-id 'pair-of)
	(object-type-spec.name (pair-of-type-spec.item-ots ots))))

(define (pair-of-type-spec.type-annotation-maker ots)
  (list (core-prim-id 'pair-of)
	(object-type-spec.type-annotation (pair-of-type-spec.item-ots ots))))

(define (make-pair-of-predicate ots)
  (let ((item-pred.stx	(object-type-spec.type-predicate-stx (pair-of-type-spec.item-ots ots)))
	(obj.sym	(make-syntactic-identifier-for-temporary-variable "obj")))
    (bless
     `(lambda/typed ({_ <boolean>} ,obj.sym)
	(and (pair? ,obj.sym)
	     (,item-pred.stx (car ,obj.sym))
	     (,item-pred.stx (cdr ,obj.sym))
	     #t)))))

;;; --------------------------------------------------------------------

(define (pair-of-type-spec?/list ots)
  (and (pair-of-type-spec? ots)
       (object-type-spec.list-type-spec? (pair-of-type-spec.item-ots ots))))

(define (pair-of-type-spec?/compatible-list ots)
  (and (pair-of-type-spec? ots)
       (object-type-spec.compatible-list-type-spec? (pair-of-type-spec.item-ots ots))))


;;;; heterogeneous list object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>"  representing non-empty  proper  list  objects holding  items  of a  known
;;heterogeneous type.
;;
(define-core-record-type <list-type-spec>
  (nongenerative vicare:expander:<list-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots*		list-type-spec.item-ots*)
		;A list of instances of  "<object-type-spec>" describing the types of
		;contained items.
    (mutable memoised-homogeneous?	list-type-spec.memoised-homogeneous? list-type-spec.memoised-homogeneous?-set!)
		;Initialised   to  void.    This   field  memoises   the  result   of
		;LIST-TYPE-SPEC.HOMOGENEOUS?.
    (mutable memoised-length		list-type-spec.memoised-length list-type-spec.memoised-length-set!)
		;Initialised   to  void.    This   field  memoises   the  result   of
		;LIST-TYPE-SPEC.LENGTH.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-list-type-spec
	(({item-type*.ots not-empty-list-of-object-type-spec?})
	 ($make-list-type-spec item-type*.ots make-list-type-name))
	(({item-type*.ots not-empty-list-of-object-type-spec?} {name.stx list-name?})
	 ($make-list-type-spec item-type*.ots name.stx)))

      (define ($make-list-type-spec item-type*.ots name)
	(let* ((parent.ots		(<nelist>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-list-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id #f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots list-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   item-type*.ots (void) (void))))

      (define (list-name? name.stx)
	(syntax-match name.stx (list)
	  ((list ?item-type* ...)
	   (for-all syntax-object.type-annotation? ?item-type*)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      make-list-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[list-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-list-type-name ots)
  (cons (core-prim-id 'list)
	(map object-type-spec.name (list-type-spec.item-ots* ots))))

(define (list-type-spec.type-annotation-maker ots)
  (cons (core-prim-id 'list)
	(map object-type-spec.type-annotation (list-type-spec.item-ots* ots))))

(define (make-list-type-predicate ots)
  (let ((item-pred*.stx (map object-type-spec.type-predicate-stx (list-type-spec.item-ots* ots)))
	(obj.sym	(make-syntactic-identifier-for-temporary-variable "obj"))
	(pred.sym	(make-syntactic-identifier-for-temporary-variable "pred"))
	(item.sym	(make-syntactic-identifier-for-temporary-variable "item")))
    (bless
     `(lambda/typed ({_ <boolean>} ,obj.sym)
	(and (list? ,obj.sym)
	     (for-all (lambda (,pred.sym ,item.sym)
			(,pred.sym ,item.sym))
	       (list ,@item-pred*.stx)
	       ,obj.sym)
	     #t)))))

;;; --------------------------------------------------------------------

(define* (make-null-or-list-type-spec {item*.ots list-of-object-type-spec?})
  (if (pair? item*.ots)
      (make-list-type-spec item*.ots)
    (<null>-ots)))

(define* (list-type-spec.homogeneous? {list.ots list-type-spec?})
  (let ((mem (list-type-spec.memoised-homogeneous? list.ots)))
    (if (boolean? mem)
	mem
      (receive-and-return (bool)
	  (apply object-type-spec=? (list-type-spec.item-ots* list.ots))
	(list-type-spec.memoised-homogeneous?-set! list.ots bool)))))

(define* (list-type-spec.length {list.ots list-type-spec?})
  (let ((mem (list-type-spec.memoised-length list.ots)))
    (if (void-object? mem)
	(receive-and-return (len)
	    (length (list-type-spec.item-ots* list.ots))
	  (list-type-spec.memoised-length-set! list.ots len))
      mem)))

(define* (list-type-spec.list-of-single-item? {list.ots list-type-spec?})
  (= 1 (list-type-spec.length list.ots)))

(define* (list-type-spec.exists {ots list-type-spec?} {proc procedure?})
  (exists proc (list-type-spec.item-ots* ots)))

(define* (list-type-spec.for-all {ots list-type-spec?} {proc procedure?})
  (for-all proc (list-type-spec.item-ots* ots)))

(define* (list-type-spec.car {ots list-type-spec?})
  (car (list-type-spec.item-ots* ots)))

(define* (list-type-spec.cdr {ots list-type-spec?})
  (make-null-or-list-type-spec (cdr (list-type-spec.item-ots* ots))))


;;;; homogeneous list object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>"  representing proper  list objects  holding items  of a  known homogeneous
;;type.
;;
(define-core-record-type <list-of-type-spec>
  (nongenerative vicare:expander:<list-of-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #f)
  (fields
    (immutable item-ots			list-of-type-spec.item-ots)
		;An instance of "<object-type-spec>" describing the type of contained
		;items.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-list-of-type-spec
	(({item-type.ots object-type-spec?})
	 ($make-list-of-type-spec item-type.ots make-list-of-type-name))
	(({item-type.ots object-type-spec?} {name.stx (or list-of-name? alist-name?)})
	 ($make-list-of-type-spec item-type.ots name.stx)))

      (define ($make-list-of-type-spec item-type.ots name)
	(let* ((parent.ots		(<list>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-list-of-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id #f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots list-of-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   item-type.ots)))

      (define (list-of-name? name.stx)
	(syntax-match name.stx (list-of)
	  ((list-of ?item-type)
	   (syntax-object.type-annotation? ?item-type)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      make-list-of-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[list-of-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-list-of-type-name ots)
  (list (list-of-id)
	(object-type-spec.name (list-of-type-spec.item-ots ots))))

(define (list-of-type-spec.type-annotation-maker ots)
  (list (list-of-id)
	(object-type-spec.name (list-of-type-spec.item-ots ots))))

(define (make-list-of-type-predicate ots)
  (let* ((item-type.ots	(list-of-type-spec.item-ots ots))
	 (item-pred.stx	(object-type-spec.type-predicate-stx item-type.ots))
	 (obj.sym	(make-syntactic-identifier-for-temporary-variable "obj"))
	 (pred.sym	(make-syntactic-identifier-for-temporary-variable "pred")))
    (bless
     `(letrec/checked ((,pred.sym (lambda ({_ <boolean>} ,obj.sym)
				    (if (pair? ,obj.sym)
					(and (,item-pred.stx (car ,obj.sym))
					     (,pred.sym      (cdr ,obj.sym))
					     #t)
				      (null? ,obj.sym)))))
	,pred.sym))))


;;;; alist object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>" representing alist objects holding keys and values of a known type.
;;
(define-core-record-type <alist-type-spec>
  (nongenerative vicare:expander:<alist-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <list-of-type-spec>)
  (sealed #t)
  (fields
    (immutable key-ots			alist-type-spec.key-ots)
		;An instance of "<object-type-spec>" describing the type of keys.
    (immutable val-ots			alist-type-spec.val-ots)
		;An instance of "<object-type-spec>" describing the type of values.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-list-of-type-spec)
      (case-define* make-alist-type-spec
	(({key.ots object-type-spec?} {value.ots object-type-spec?})
	 ((make-list-of-type-spec (make-pair-type-spec key.ots value.ots)) key.ots value.ots))
	(({key.ots object-type-spec?} {value.ots object-type-spec?} {name.stx alist-name?})
	 ((make-list-of-type-spec (make-pair-type-spec key.ots value.ots) name.stx) key.ots value.ots)))

      make-alist-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[alist-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (alist-name? name.stx)
  (syntax-match name.stx (alist)
    ((alist ?key-type ?value-type)
     (and (syntax-object.type-annotation? ?key-type)
	  (syntax-object.type-annotation? ?value-type))
     #t)
    (?type-id
     (identifier? ?type-id)
     #t)
    (else #f)))

(define (make-alist-type-name ots)
  (list (alist-id)
	(object-type-spec.name (alist-type-spec.key-ots   ots))
	(object-type-spec.name (alist-type-spec.val-ots ots))))

(define (alist-type-spec.type-annotation-maker ots)
  (list (alist-id)
	(object-type-spec.type-annotation (alist-type-spec.key-ots   ots))
	(object-type-spec.type-annotation (alist-type-spec.val-ots ots))))

(define (make-alist-type-predicate ots)
  ;;FIXME The generated  predicate can be made more efficient  by iterating only once
  ;;through the spine of the list.  (Marco Maggi; Mon Apr 4, 2016)
  (let ((key-pred.stx	(object-type-spec.type-predicate-stx (alist-type-spec.key-ots   ots)))
	(value-pred.stx	(object-type-spec.type-predicate-stx (alist-type-spec.val-ots ots)))
	(obj.sym	(make-syntactic-identifier-for-temporary-variable "obj"))
	(P.sym		(make-syntactic-identifier-for-temporary-variable "P")))
    (bless
     `(lambda/typed ({_ <boolean>} ,obj.sym)
	(and (list? ,obj.sym)
	     (for-all (lambda (,P.sym)
			(and (pair? ,P.sym)
			     (,key-pred.stx   (car ,P.sym))
			     (,value-pred.stx (cdr ,P.sym))))
	       ,obj.sym)
	     #t)))))


;;;; heterogeneous vector object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<vector>"  representing  non-empty  vector  objects   holding  items  of  a  known
;;heterogeneous type.
;;
(define-core-record-type <vector-type-spec>
  (nongenerative vicare:expander:<vector-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots*		vector-type-spec.item-ots*)
		;A vector  of instances of "<object-type-spec>"  describing the types
		;of contained items.
    (mutable memoised-homogeneous?	vector-type-spec.memoised-homogeneous? vector-type-spec.memoised-homogeneous?-set!)
		;Initialised   to  void.    This   field  memoises   the  result   of
		;VECTOR-TYPE-SPEC.HOMOGENEOUS?.
    (mutable memoised-length		vector-type-spec.memoised-length vector-type-spec.memoised-length-set!)
		;Initialised   to  void.    This   field  memoises   the  result   of
		;VECTOR-TYPE-SPEC.LENGTH.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-vector-type-spec
	(({item-type*.ots not-empty-list-of-object-type-spec?})
	 ($make-vector-type-spec item-type*.ots make-vector-type-name))
	(({item-type*.ots not-empty-list-of-object-type-spec?} {name.stx vector-name?})
	 ($make-vector-type-spec item-type*.ots name.stx)))

      (define ($make-vector-type-spec item-type*.ots name)
	(let* ((parent.ots		(<nevector>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-vector-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id #f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots vector-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   item-type*.ots (void) (void))))

      (define (vector-name? name.stx)
	(syntax-match name.stx (vector)
	  ((vector ?item-type* ...)
	   (for-all syntax-object.type-annotation? ?item-type*)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      make-vector-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[vector-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-vector-type-name ots)
  (cons (core-prim-id 'vector)
	(map object-type-spec.name (vector-type-spec.item-ots* ots))))

(define (vector-type-spec.type-annotation-maker ots)
  (cons (core-prim-id 'vector)
	(map object-type-spec.type-annotation (vector-type-spec.item-ots* ots))))

(define (make-vector-type-predicate ots)
  (let* ((item-pred*.stx	(map object-type-spec.type-predicate-stx (vector-type-spec.item-ots* ots)))
	 (obj.sym		(make-syntactic-identifier-for-temporary-variable "obj"))
	 (pred.sym		(make-syntactic-identifier-for-temporary-variable "pred"))
	 (item.sym		(make-syntactic-identifier-for-temporary-variable "item")))
    (bless
     `(lambda/typed ({_ <boolean>} ,obj.sym)
	(and (vector? ,obj.sym)
	     (vector-for-all (lambda (,pred.sym ,item.sym)
			       (,pred.sym ,item.sym))
	       (vector ,@item-pred*.stx)
	       ,obj.sym)
	     #t)))))

;;; --------------------------------------------------------------------

(define* (vector-type-spec.homogeneous? {vector.ots vector-type-spec?})
  (let ((mem (vector-type-spec.memoised-homogeneous? vector.ots)))
    (if (boolean? mem)
	mem
      (receive-and-return (bool)
	  (apply object-type-spec=? (vector-type-spec.item-ots* vector.ots))
	(vector-type-spec.memoised-homogeneous?-set! vector.ots bool)))))

(define* (vector-type-spec.length {vector.ots vector-type-spec?})
  (let ((mem (vector-type-spec.memoised-length vector.ots)))
    (if (void-object? mem)
	(receive-and-return (len)
	    (length (vector-type-spec.item-ots* vector.ots))
	  (vector-type-spec.memoised-length-set! vector.ots len))
      mem)))

(define* (vector-type-spec.exists {ots vector-type-spec?} {proc procedure?})
  (exists proc (vector-type-spec.item-ots* ots)))

(define* (vector-type-spec.for-all {ots vector-type-spec?} {proc procedure?})
  (for-all proc (vector-type-spec.item-ots* ots)))


;;;; homogeneous vector object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<vector>" representing vector objects holding items of a known type.
;;
(define-core-record-type <vector-of-type-spec>
  (nongenerative vicare:expander:<vector-of-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots			vector-of-type-spec.item-ots)
		;An instance of "<object-type-spec>" describing the type of contained
		;items.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-vector-of-type-spec
	(({item-type.ots object-type-spec?})
	 ($make-vector-of-type-spec item-type.ots make-vector-of-type-name))
	(({item-type.ots object-type-spec?} {name vector-of-name?})
	 ($make-vector-of-type-spec item-type.ots name)))

      (define ($make-vector-of-type-spec item-type.ots name)
	(let* ((parent.ots		(<vector>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-vector-of-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id #f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots vector-of-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   item-type.ots)))

      (define (vector-of-name? name.stx)
	(syntax-match name.stx (vector-of)
	  ((vector-of ?item-type)
	   (syntax-object.type-annotation? ?item-type)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      make-vector-of-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[vector-of-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-vector-of-type-name ots)
  (list (vector-of-id)
	(object-type-spec.name (vector-of-type-spec.item-ots ots))))

(define (vector-of-type-spec.type-annotation-maker ots)
  (list (vector-of-id)
	(object-type-spec.type-annotation (vector-of-type-spec.item-ots ots))))

(define (make-vector-of-type-predicate ots)
  (let ((item-pred.stx	(object-type-spec.type-predicate-stx (vector-of-type-spec.item-ots ots)))
	(obj.sym	(make-syntactic-identifier-for-temporary-variable "obj")))
    (bless
     `(lambda/typed (,obj.sym)
	(and (vector? ,obj.sym)
	     (vector-for-all ,item-pred.stx ,obj.sym)
	     #t)))))


;;;; hashtable object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<hashtable>" representing  hashtable objects  holding keys and  values of  a known
;;type.
;;
(define-core-record-type <hashtable-type-spec>
  (nongenerative vicare:expander:<hashtable-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable key-ots			hashtable-type-spec.key-ots)
		;An instance of "<object-type-spec>" describing the type of keys.
    (immutable val-ots			hashtable-type-spec.val-ots)
		;An instance of "<object-type-spec>" describing the type of values.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-hashtable-type-spec
	(({key-type.ots object-type-spec?} {value-type.ots object-type-spec?})
	 ($make-hashtable-type-spec key-type.ots value-type.ots make-hashtable-type-name))
	(({key-type.ots object-type-spec?} {value-type.ots object-type-spec?} {name.stx hashtable-name?})
	 ($make-hashtable-type-spec key-type.ots value-type.ots name.stx)))

      (define ($make-hashtable-type-spec key-type.ots value-type.ots name)
	(let* ((parent.ots		(<hashtable>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		#f)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id #f)
	       (hash-function.id	#f)
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots hashtable-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   key-type.ots value-type.ots)))

      (define (hashtable-name? name.stx)
	(syntax-match name.stx (hashtable)
	  ((hashtable ?key-type ?value-type)
	   (and (syntax-object.type-annotation? ?key-type)
		(syntax-object.type-annotation? ?value-type))
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      make-hashtable-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[hashtable-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (make-hashtable-type-name ots)
  (list (hashtable-id)
	(object-type-spec.name (hashtable-type-spec.key-ots   ots))
	(object-type-spec.name (hashtable-type-spec.val-ots ots))))

(define (hashtable-type-spec.type-annotation-maker ots)
  (list (hashtable-id)
	(object-type-spec.type-annotation (hashtable-type-spec.key-ots ots))
	(object-type-spec.type-annotation (hashtable-type-spec.val-ots ots))))


;;;; enumeration object spec
;;
;;This record-type  is used as  syntactic binding descriptor's value  for enumeration
;;types  defined  either  with  DEFINE-ENUMERATION   or  with  the  ENUMERATION  type
;;annotation.
;;
(define-core-record-type <enumeration-type-spec>
  (nongenerative vicare:expander:<enumeration-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable	symbol*			enumeration-type-spec.symbol*)
		;An proper list of symbols representing the enumeration universe.
    (mutable	memoised-length)
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-enumeration-type-spec
	(({symbol* list-of-symbols?})
	 ($make-enumeration-type-spec symbol* enumeration-type-spec.type-annotation-maker))
	(({symbol* list-of-symbols?} name.stx)
	 ($make-enumeration-type-spec symbol* name.stx)))

      (define ($make-enumeration-type-spec symbol* name)
	(let* ((symbol*			(list-of-symbols.delete-duplicates symbol*))
	       (parent.ots		(<symbol>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-enumeration-type-predicate)
	       (equality-predicate.id	(core-prim-id 'eq?))
	       (comparison-procedure.id #f)
	       (hash-function.id	(core-prim-id 'symbol-hash))
	       (methods-table		'())
	       (implemented-interfaces	'()))
	  ((make-object-type-spec name (object-type-spec.uids-list parent.ots)
				  parent.ots enumeration-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   symbol* #f)))

      make-enumeration-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[enumeration-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (enumeration-type-spec.type-annotation-maker ots)
  (cons (enumeration-id)
	(map make-syntactic-identifier-for-quoted-symbol (enumeration-type-spec.symbol* ots))))

(define (make-enumeration-type-predicate ots)
  (let ((symbol*	(enumeration-type-spec.symbol* ots))
	(obj.sym	(make-syntactic-identifier-for-temporary-variable "obj")))
    (bless
     `(lambda/typed ({_ <boolean>} ,obj.sym)
	(and (symbol? ,obj.sym)
	     (memq ,obj.sym (quote ,symbol*))
	     #t)))))

;;; --------------------------------------------------------------------

(define* (enumeration-type-spec.member? {ots enumeration-type-spec?} {sym symbol?})
  (and (memq sym (enumeration-type-spec.symbol* ots))
       #t))

(define* (enumeration-type-spec.length {ots enumeration-type-spec?})
  (or (enumeration-type-spec-memoised-length ots)
      (receive-and-return (len)
	  (length (enumeration-type-spec.symbol* ots))
	(enumeration-type-spec-memoised-length-set! ots len))))

(define* (enumeration-type-spec.for-all {des enumeration-type-spec?} {proc procedure?})
  (and (for-all proc (enumeration-type-spec.symbol* des))
       #t))


;;;; label spec
;;
;;This record-type  is used as syntactic  binding descriptor's value for  label types
;;defined with DEFINE-LABEL.
;;
(define-core-record-type <label-type-spec>
  (nongenerative vicare:expander:<label-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable with-type-predicate?	label-type-spec.with-type-predicate?)
		;Boolean, true if this label has  a custom type predicate.  This flag
		;is used when matching against other types.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-label-type-spec {type-name.id identifier?} {uid symbol?} parent.stx
				     constructor.stx destructor.stx type-predicate.stx
				     equality-predicate.id comparison-procedure.id hash-function.id
				     methods-table)
	(let* ((parent.ots		(with-exception-handler
					    (lambda (E)
					      (raise (condition E (make-who-condition __who__))))
					  (lambda ()
					    (type-annotation->object-type-spec parent.stx))))
	       (with-type-predicate?	(and type-predicate.stx #t))
	       (type-predicate.stx	(or type-predicate.stx (object-type-spec.type-predicate-stx parent.ots)))
	       (implemented-interfaces	'()))
	  ((make-object-type-spec type-name.id (cons uid (object-type-spec.uids-list parent.ots))
				  parent.ots label-type-spec.type-annotation-maker
				  constructor.stx destructor.stx type-predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  methods-table methods-table methods-table implemented-interfaces)
	   with-type-predicate?)))

      make-label-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define (label-type-spec.type-annotation-maker ots)
  (object-type-spec.name ots))


;;;; interface spec
;;
;;This  record-type is  used as  syntactic binding  descriptor's value  for interface
;;types defined with DEFINE-INTERFACE-TYPE.
;;

(define-core-record-type <interface-type-spec>
  (nongenerative vicare:expander:<interface-type-spec>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable	type-descriptor-id		interface-type-spec.type-descriptor-id)
		;A syntactic identifier bound to the run-time type descriptor.
    (immutable	method-prototypes-table		interface-type-spec.method-prototypes-table)
		;Alist having: as keys, symbols representing method names; as values,
		;pairs with the format:
		;
		;   (?closure-ots . ?has-default)
		;
		;where:   ?CLOSURE-OTS  is   an  instance   of  "<closure-type-spec>"
		;representing the methods' required  type signature; ?HASH-DEFAULT is
		;a   boolean,  true   if  this   method  prototype   has  a   default
		;implementation.
		;
		;This  alist   includes:  the  method  prototypes   defined  by  this
		;interface-type, the method prototypes  defined by the parent's type,
		;the method prototypes defined by the grand-parent's type, et cetera.
		;
		;If an  object-type implements  a concrete method  for each  entry in
		;this alist: that object-type implements this interface-type.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-interface-type-spec {type-name.id identifier?} {uid symbol?} {type-descriptor.id identifier?}
					 parent-name.ots method-prototypes-table methods-table
					 {implemented-interfaces list-of-interface-type-specs?})
	;;The  argument METHOD-PROTOTYPES-TABLE  must  be an  alist  having with  the
	;;format of the METHOD-PROTOTYPES-TABLE field.
	;;
	;;The  argument METHODS-TABLE  must  be  an alist  having:  as keys,  symbols
	;;representing the  method names; as  values, syntactic identifiers  bound to
	;;the method  implementation functions.  The method  implementation functions
	;;will   perform  run-time   dynamic  dispatch   to  the   concrete  method's
	;;implementations.  This table does *not* include the parent's methods.
	;;
	;;The argument IMPLEMENTED-INTERFACES must be a list of syntactic identifiers
	;;bound  to  the  interface-type   specifications  that  this  interface-type
	;;implements.
	;;
	(let* ((parent.ots		(or parent-name.ots (<top>-ots)))
	       (uid*			(cons uid (object-type-spec.uids-list parent.ots)))
	       ;;By setting the  predicate to ALWAYS-FALSE we  make run-time matching
	       ;;fail.
	       (type-predicate.stx	(core-prim-id 'always-false)))
	  ((make-object-type-spec type-name.id uid*
				  parent.ots interface-type-spec.type-annotation-maker
				  #f			  ;constructor-stx
				  #f			  ;destructor-stx
				  type-predicate.stx	  ;type-predicate-stx
				  #f			  ;equality-predicate.id
				  #f			  ;comparison-procedure.id
				  #f			  ;hash-function.id
				  methods-table		  ;methods-table-public
				  methods-table		  ;methods-table-protected
				  methods-table		  ;methods-table-private
				  implemented-interfaces) ;implemented-interfaces
	   type-descriptor.id method-prototypes-table)))

      make-interface-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define (interface-type-spec.type-annotation-maker ots)
  (object-type-spec.name ots))

(define (list-of-interface-type-specs? obj)
  (if (pair? obj)
      (and (interface-type-spec? (car obj))
	   (list-of-interface-type-specs? (cdr obj)))
    (null? obj)))


(module (interface-type-spec.super-and-sub?)

  (define* (interface-type-spec.super-and-sub? {super-iface.ots interface-type-spec?} {sub.ots object-type-spec?})
    ;;Return true if  the interface specification SUPER-IFACE.OTS is  a super-type of
    ;;the object-type specification SUB.OTS; otherwise return false.
    ;;
    ;;SUB.OTS is  a sub-type of  SUPER-IFACE.OTS if:  SUPER-IFACE.OTS is equal  to an
    ;;item in SUB.OTS's field IMPLEMENTED-INTERFACES;  or SUPER-IFACE.OTS is equal to
    ;;an item in SUB.OTS's parent's field IMPLEMENTED-INTERFACES; and so on.
    ;;
    ;;Use case: this  function is used when, in a  function application, we determine
    ;;if an operand's type is sub-type  of the corresponding argument's type and: the
    ;;argument's type is SUPER-IFACE.OTS, the operand's type is SUB.OTS.
    ;;
    ;;Use case:  this function is used  when, in a typed  variable initialisation, we
    ;;determine if a return value's type  is sub-type of the corresponding variable's
    ;;type and:  the variable's type is  SUPER-IFACE.OTS, the return value's  type is
    ;;SUB.OTS.
    ;;
    (cond ((interface-type-spec? sub.ots)
	   (%super-iface-and-sub-iface? super-iface.ots sub.ots))
	  (else
	   (%super-iface-and-sub-type? super-iface.ots sub.ots))))

  (define (%super-iface-and-sub-iface? super-iface.ots sub-iface.ots)
    (or (eq? super-iface.ots sub-iface.ots)
	($interface-type-spec=? super-iface.ots sub-iface.ots)
	(exists (lambda (sub-implemented-iface.ots)
		  (%super-iface-and-sub-iface? super-iface.ots sub-implemented-iface.ots))
	  (object-type-spec.implemented-interfaces sub-iface.ots))
	(cond ((object-type-spec.parent-ots sub-iface.ots)
	       => (lambda (sub-parent.ots)
		    (and (interface-type-spec? sub-parent.ots)
			 (%super-iface-and-sub-iface? super-iface.ots sub-parent.ots))))
	      (else #f))))

  (define (%super-iface-and-sub-type? super-iface.ots sub.ots)
    (cond ((exists (lambda (sub-implemented-iface.ots)
		     (%super-iface-and-sub-iface? super-iface.ots sub-implemented-iface.ots))
	     (object-type-spec.implemented-interfaces sub.ots)))
	  ((object-type-spec.parent-ots sub.ots)
	   => (lambda (sub-parent.ots)
		(%super-iface-and-sub-type? super-iface.ots sub-parent.ots)))
	  (else #f)))

  #| end of module: INTERFACE-TYPE-SPEC.SUPER-AND-SUB? |# )


(module (assert-implemented-interface-type-and-implementer-interface-type)

  (define __module_who__
    'assert-implemented-interface-type-and-implementer-interface-type)

  (define* (assert-implemented-interface-type-and-implementer-interface-type {super-iface.ots interface-type-spec?}
									     {sub-iface.ots interface-type-spec?})
    ;;This function must be used when, at expand-time, we define a new interface-type
    ;;that is meant to implement another interface-type.  It is used in the expansion
    ;;of DEFINE-INTERFACE-TYPE.
    ;;
    ;;Verify that the  specification SUB-IFACE.OTS implements all  the methods needed
    ;;by the specification SUPER-IFACE.OTS;  otherwise raise an exception.  SUB-IFACE
    ;;must  have a  method for  every method  in: SUPER-IFACE,  SUPER-IFACE's parent,
    ;;SUPER-IFACE's grand-parent, ...  Return unspecified values.
    ;;
    ;;When  this  function   is  successful:  SUPER-IFACE.OTS  is   a  super-type  of
    ;;SUB-IFACE.OTS.
    ;;
    ;;NOTE  We   need  to   remember  that   the  field   METHOD-PROTOTYPES-TABLE  of
    ;;"<interface-type-spec>" holds an  entry for each method  in the interface-type,
    ;;the interface-type's parent, the  interface-type's grand-parent, et cetera.  So
    ;;there is no need to traverse the hierarchy.
    ;;
    (for-each
	(lambda (super-method-prototype-entry)
	  ;;SUPER-METHOD-PROTOTYPE-ENTRY  is   a  pair  having:  as   car,  a  symbol
	  ;;representing    the   method    name;    as   cdr,    an   instance    of
	  ;;"<closure-type-spec>" representing the method's required type signature.
	  (object-type-spec.compatible-method-stx super-iface.ots sub-iface.ots
						  (car  super-method-prototype-entry)
						  (cadr super-method-prototype-entry)
						  (cddr super-method-prototype-entry)))
      (interface-type-spec.method-prototypes-table super-iface.ots)))

  (define* (object-type-spec.compatible-method-stx super-iface.ots sub-iface.ots
						   super-method-name.sym super-method-prototype.ots
						   super-method-has-default?)
    ;;SUB-IFACE.OTS must be the instance  of "<interface-type-spec>" that is meant to
    ;;implement the interface-type SUPER-IFACE.OTS.   SUPER-METHOD-NAME.SYM must be a
    ;;symbol  representing the  method name.   SUPER-METHOD-PROTOTYPE.OTS must  be an
    ;;instance  of   "<closure-type-spec>"  describing  the  requested   method  type
    ;;signature.
    ;;
    ;;If SUPER-METHOD-NAME.SYM is EQ?  to  a SUB-IFACE.OTS's method name: compare the
    ;;implemented  method's type  specification with  SUPER-METHOD-PROTOTYPE.OTS, the
    ;;comparison   is   successful   if   the  specification   is   a   sub-type   of
    ;;SUPER-METHOD-PROTOTYPE.OTS.   If  the  signatures   are  mismatched:  raise  an
    ;;exception.  If no method name matches: raise an exception.
    ;;
    ;;When this function  is successful: SUB-IFACE.OTS has a method  that can be used
    ;;when a function of type SUPER-METHOD-PROTOTYPE.OTS is required.
    ;;
    (define-syntax-rule (recursion ?sub-parent-ots)
      (object-type-spec.compatible-method-stx super-iface.ots ?sub-parent-ots
					      super-method-name.sym super-method-prototype.ots
					      super-method-has-default?))
    (cond ((assq super-method-name.sym (interface-type-spec.method-prototypes-table sub-iface.ots))
	   ;;The name is known; extract the  symbolic expression from the alist entry
	   ;;and return it.
	   => (lambda (sub-method-prototype-entry)
		(let ((sub-method-prototype.ots (cadr sub-method-prototype-entry)))
		  (unless (object-type-spec.matching-super-and-sub? super-method-prototype.ots sub-method-prototype.ots)
		    (raise
		     (condition
		       (make-who-condition __module_who__)
		       (make-message-condition "interface-type does not implement the specified interface-type: \
                                                mismatching method implementation")
		       (make-interface-implementation-mismatching-method-violation
			(object-type-spec.name sub-iface.ots)	;object-type-name
			(object-type-spec.name super-iface.ots) ;interface-type-name
			super-method-name.sym
			(object-type-spec.type-annotation super-method-prototype.ots) ;interface-method-signature
			(object-type-spec.type-annotation   sub-method-prototype.ots) ;object-method-signature
			)))))))
	  (else
	   (unless super-method-has-default?
	     (raise
	      (condition
		(make-who-condition __module_who__)
		(make-message-condition "interface-type does not implement the specified interface-type: \
                                         missing method implementation")
		(make-interface-implementation-missing-method-violation
		 (object-type-spec.name sub-iface.ots)
		 (object-type-spec.name super-iface.ots)
		 super-method-name.sym
		 (object-type-spec.type-annotation super-method-prototype.ots))))))))

  #| end of module: ASSERT-IMPLEMENTED-INTERFACE-TYPE-AND-IMPLEMENTER-INTERFACE-TYPE |# )


(module (build-table-for-interface-types-and-implementer-object-type)
  ;;This function must be used when, at expand-time, we define a new object-type that
  ;;is  meant  to implement  an  interface-type.   For example:  it  is  used in  the
  ;;expansion of DEFINE-RECORD-TYPE.
  ;;
  ;;The argument IFACE*.OTS  must be a list of  instances of "<interface-type-spec>".
  ;;The argument OBJECT.OTS must be an instance of "<object-type-spec>".  Verify that
  ;;OBJECT.OTS implements all  the methods needed by  the interface-types IFACE*.OTS;
  ;;otherwise raise an exception.
  ;;
  ;;Here is how do we do it for each IFACE*.OTS.  First we build a full methods table
  ;;holding an entry for each method in the METHOD-PROTOTYPES-TABLE of IFACE.OTS.  We
  ;;look   for  matching   methods  in:   OBJECT.OTS  itself,   OBJECT.OTS's  parent,
  ;;OBJECT.OTS's grand-parent, et cetera.
  ;;
  ;;Building the  methods table  makes sure that  OBJECT.OTS actually  implements the
  ;;interface-type IFACE.OTS.  So we can build  a syntax object representing a Scheme
  ;;expression which, expanded  and evaluated, returns the  run-time method retriever
  ;;procedure  for  this  interface-type.   Finally  we can  build  a  syntax  object
  ;;representing  a Scheme  expression  which, expanded  and  evaluated, returns  the
  ;;associated "implementation pair":
  ;;
  ;;   (?interface-uid . ?method-retriever)
  ;;
  ;;NOTE   We  need   to   remember  that   the   field  METHOD-PROTOTYPES-TABLE   of
  ;;"<interface-type-spec>" holds an entry for each method in the interface-type, the
  ;;interface-type's parent, the interface-type's  grand-parent, et cetera.  So there
  ;;is no need to traverse the hierarchy to gather the full list of required methods.
  ;;
  ;;Now,  it  is  automatic  that  OBJECT.OTS  also  implements:  IFACE.OTS's  parent
  ;;interface-type, IFACE.OTS's  grand-parent interface-type, et cetera.   It is also
  ;;automatic  that OBJECT.OTS  also implements:  the interface-types  implemented by
  ;;IFACE.OTS,   the  interface-types   implemented   by   IFACE.OTS's  parent,   the
  ;;interface-types implemented by IFACE.OTS's grand-parent, et cetera.
  ;;
  ;;So   we   traverse  the   hierarchy   of   IFACE.OTS  parents   and   implemented
  ;;interface-types,  building a  list of  "<interface-type-spec>" instances.   Then,
  ;;for:  each  interface-type  we  build  the syntax  object  that  will  build  the
  ;;implementation pair.   We look for methods  in the full methods  table, comparing
  ;;methods prototypes just by the symbol representing the name.
  ;;
  ;;Finally we put all the pair syntax objects in a list and return it.
  ;;
  ;;NOTE When this  function returns successfully: each IFACE.OTS is  a super-type of
  ;;OBJECT.OTS.
  ;;
  (define __module_who__
    'build-table-for-interface-types-and-implementer-object-type)

  (define* (build-table-for-interface-types-and-implementer-object-type {iface*.ots list-of-interface-type-specs?}
									{object.ots object-type-spec?})
    ;;Build  and  return  a  list  of symbolic  expressions  (to  be  BLESSed  later)
    ;;representing  Scheme  expressions which,  expanded  and  evaluated, return  the
    ;;implementation pairs for the arguments.
    ;;
    (bless
     (fold-left (lambda (knil iface.ots)
		  (append (single-interface-type-and-implementer-object-type iface.ots object.ots)
			  knil))
       '() iface*.ots)))

  (define (single-interface-type-and-implementer-object-type iface.ots object.ots)
    ;;Build  and  return  a  list  of symbolic  expressions  (to  be  BLESSed  later)
    ;;representing  Scheme  expressions which,  expanded  and  evaluated, return  the
    ;;implementation pairs for the arguments.
    ;;
    (let* ((full-methods-table	(%build-full-methods-table iface.ots object.ots))
	   (iface-pair.sexp	`(cons ,(%build-interface-type-uid-sexp iface.ots)
				       ,(%make-method-retriever-sexp full-methods-table))))
      (let* ((other-iface*.ots	(cdr (%build-list-of-interface-types iface.ots)))
	     (other-pair*.sexp	(%build-pairs-sexps other-iface*.ots full-methods-table)))
	(cons iface-pair.sexp other-pair*.sexp))))

;;; --------------------------------------------------------------------

  (module (%build-full-methods-table)

    (define (%build-full-methods-table iface.ots object.ots)
      (fold-left (lambda (table super-method-prototype-entry)
		   (cons (object-type-spec.compatible-method-stx iface.ots object.ots object.ots
								 (car super-method-prototype-entry)
								 (cadr super-method-prototype-entry)
								 (cddr super-method-prototype-entry))
			 table))
	'() (interface-type-spec.method-prototypes-table iface.ots)))

    (define (object-type-spec.compatible-method-stx iface.ots object.ots current.ots
						    super-method-name.sym super-method-prototype.ots
						    super-method-has-default?)
      ;;SUPER-METHOD-NAME.SYM  must  be  a   symbol  representing  the  method  name.
      ;;SUPER-METHOD-PROTOTYPE.OTS  must  be  an  instance  of  "<closure-type-spec>"
      ;;describing the requested method's type signature.
      ;;
      ;;If SUPER-METHOD-NAME.SYM is  EQ?  to a method name  in SUB-TYPE.OTS's methods
      ;;table:   compare   the   implemented   method's   type   specification   with
      ;;SUPER-METHOD-PROTOTYPE.OTS, the comparison is successful if the specification
      ;;is  a   sub-type  of  SUPER-METHOD-PROTOTYPE.OTS.   If   the  signatures  are
      ;;mismatched:  raise  an  exception.   If  no method  name  matches:  raise  an
      ;;exception.
      ;;
      ;;When this  function is successful: OBJECT.OTS  has a method that  can be used
      ;;when a function of type SUPER-METHOD-PROTOTYPE.OTS is required.
      ;;
      ;;When successful: return  a pair whose car is  SUPER-METHOD-NAME.SYM and whose
      ;;cdr is the  syntactic identifier bound to the  matching method implementation
      ;;procedure.
      ;;
      (define-syntax-rule (recursion ?parent.ots)
	(object-type-spec.compatible-method-stx iface.ots object.ots ?parent.ots
						super-method-name.sym super-method-prototype.ots
						super-method-has-default?))
      (cond ((assq super-method-name.sym (object-type-spec.methods-table-public current.ots))
	     => (lambda (entry)
		  (let* ((sub-method-procname.id	(cdr entry))
			 (sub-method-procedure.ots	((expression-expander-for-type-annotations)
							 sub-method-procname.id (current-inferior-lexenv))))
		    (if (object-type-spec.matching-super-and-sub? super-method-prototype.ots sub-method-procedure.ots)
			(cons super-method-name.sym sub-method-procname.id)
		      (raise
		       (condition
			 (make-who-condition __module_who__)
			 (make-message-condition "object-type does not implement the specified interface-type: \
                                                  mismatching method implementation")
			 (make-interface-implementation-mismatching-method-violation
			  (object-type-spec.name object.ots) ;object-type-name
			  (object-type-spec.name iface.ots)  ;interface-type-name
			  super-method-name.sym
			  (object-type-spec.type-annotation super-method-prototype.ots) ;interface-method-signature
			  (object-type-spec.type-annotation   sub-method-procedure.ots) ;object-method-signature
			  )))))))
	    ((object-type-spec.parent-ots current.ots)
	     => (lambda (parent.ots)
		  (recursion parent.ots)))
	    (else
	     (if super-method-has-default?
		 (cons super-method-name.sym #f)
	       (raise
		(condition
		  (make-who-condition __module_who__)
		  (make-message-condition "object-type does not implement the specified interface-type: \
                                         missing method implementation")
		  (make-interface-implementation-missing-method-violation
		   (object-type-spec.name object.ots)
		   (object-type-spec.name iface.ots)
		   super-method-name.sym
		   (object-type-spec.type-annotation super-method-prototype.ots))))))))

    #| end of module: %BUILD-FULL-METHODS-TABLE |# )

;;; --------------------------------------------------------------------

  (define (%make-method-retriever-sexp methods-table)
    ;;Build and  return a symbolic  expression (to  be BLESSed later)  representing a
    ;;Scheme  expression which,  expanded and  evaluated, returns  a run-time  method
    ;;retriever procedure for the methods in METHODS-TABLE.
    ;;
    ;;The  argument  METHODS-TABLE   must  be  a  alist  having:   as  keys,  symbols
    ;;representing method names; as values, syntactic identifiers bound to the method
    ;;implementation procedures.
    ;;
    (let ((method-name.id (make-syntactic-identifier-for-temporary-variable "method-name")))
      `(lambda (,method-name.id)
	 (case ,method-name.id
	   ,@(map (lambda (entry)
		    `((,(car entry)) ,(cdr entry)))
	       methods-table)
	   (else #f)))))

;;; --------------------------------------------------------------------

  (define* (%build-list-of-interface-types iface.ots)
    ;;Recursive function.   Traverse the  hierarchy of IFACE.OTS  building a  list of
    ;;"<interface-type-spec>"  instances that  includes:  IFACE.OTS  itself as  first
    ;;item;  all  the  parents  of  IFACE.OTS;  all  the  interfaces  implemented  by
    ;;IFACE.OTS;  all  the interfaces  implemented  by  IFACE.OTS's parent;  all  the
    ;;interfaces implemented by IFACE.OTS's grand-parent; et cetera.
    ;;
    (cons iface.ots
	  (append (fold-left (lambda (knil implemented-iface.ots)
			       (append (cons implemented-iface.ots (%build-list-of-interface-types implemented-iface.ots))
				       knil))
		    '() (object-type-spec.implemented-interfaces iface.ots))
		  (cond ((object-type-spec.parent-ots iface.ots)
			 => (lambda (parent.ots)
			      (if (interface-type-spec? parent.ots)
				  (%build-list-of-interface-types parent.ots)
				'())))
			(else '())))))

;;; --------------------------------------------------------------------

  (define (%build-pairs-sexps other-iface*.ots full-methods-table)
    ;;For  each   "<interface-type-spec>"  in   OTHER-IFACE*.OTS  build   a  symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated, returns the implementation pair for the interface-type.
    ;;The matching  methods are  searched in  the full  methods table,  comparing the
    ;;method names with EQ?.  Return the list of symbolic expressions.
    ;;
    (map (lambda (other-iface.ots)
	   `(cons ,(%build-interface-type-uid-sexp other-iface.ots)
		  ,(%make-method-retriever-sexp (map (lambda (method-prototype-entry)
						       (let ((method-name (car method-prototype-entry)))
							 (cons method-name (cdr (assq method-name full-methods-table)))))
						  (interface-type-spec.method-prototypes-table other-iface.ots)))))
      other-iface*.ots))

;;; --------------------------------------------------------------------

  (define (%build-interface-type-uid-sexp iface.ots)
    `(quote ,(car (object-type-spec.uids-list iface.ots))))

  #| end of module: BUILD-TABLE-FOR-INTERFACE-TYPES-AND-IMPLEMENTER-OBJECT-TYPE |# )


;;;; type annotations
;;
;;A type annotation is a syntax object  representing the type of a syntactic binding.
;;For example, in the form:
;;
;;   (let (({a <fixnum>} 1)
;;         ({b (list-of <flonum>)} '(1.1 2.2))
;;         ({b (list-of (list-of <string>))} '(("a" "b"))))
;;     ?body)
;;
;;the forms:
;;
;;   <fixnum>
;;   (list-of <flonum>)
;;   (list-of (list-of <string>))
;;
;;are type annotations.
;;

(case-define* syntax-object.type-annotation?
  ;;Parse the syntax object INPUT-FORM.STX as type annotation and return non-false if
  ;;it complies.
  ;;
  ((input-form.stx)
   (syntax-object.type-annotation? input-form.stx (current-inferior-lexenv) input-form.stx))
  ((input-form.stx lexenv)
   (syntax-object.type-annotation? input-form.stx lexenv input-form.stx))
  ((input-form.stx lexenv stx)
   (define-syntax recur
     (identifier-syntax
      (lambda (stx)
	(syntax-object.type-annotation? input-form.stx lexenv stx))))
   (define (%mk-type-signature type.ann)
     (let ((synner (lambda (message subform)
		     (syntax-violation __who__ message input-form.stx subform))))
       (make-type-signature (syntax-object->type-signature-specs type.ann lexenv synner))))
   (syntax-match stx (pair list vector pair-of list-of nelist-of vector-of nevector-of
			   hashtable alist condition enumeration
			   or and not lambda case-lambda => parent-of ancestor-of
			   type-predicate equality-predicate comparison-procedure hash-function
			   type-of)
     (?type-id
      (identifier? ?type-id)
      (try
	  (if (id->object-type-spec ?type-id lexenv) #t #f)
	(catch E
	  (else #f))))

     ((pair ?car-type ?cdr-type)
      (and (recur ?car-type)
	   (recur ?cdr-type)))

     ((list ?item-type* ...)
      (for-all recur ?item-type*))

     ((vector ?item-type* ...)
      (for-all recur ?item-type*))

     ((pair-of ?item-type)
      (recur ?item-type))

     ((list-of ?item-type)
      (recur ?item-type))

     ((nelist-of ?item-type)
      (recur ?item-type))

     ((vector-of ?item-type)
      (recur ?item-type))

     ;; ((nevector-of ?item-type)
     ;;  (recur ?item-type))

     ((hashtable ?key-type ?value-type)
      (and (recur ?key-type)
	   (recur ?value-type)))

     ((alist ?key-type ?value-type)
      (and (recur ?key-type)
	   (recur ?value-type)))

     ((enumeration ?symbol ?symbol* ...)
      (for-all identifier? (cons ?symbol ?symbol*)))

     ((lambda ?argtypes => ?rettypes)
      (and (%mk-type-signature ?argtypes)
	   (%mk-type-signature ?rettypes)
	   #t))

     ((case-lambda
	(?argtypes0 => ?rettypes0)
	(?argtypes* => ?rettypes*)
	...)
      (and (%mk-type-signature ?argtypes0)
	   (%mk-type-signature ?rettypes0)
	   (for-all %mk-type-signature ?argtypes*)
	   (for-all %mk-type-signature ?rettypes*)
	   #t))

     ((condition ?component-type* ...)
      (for-all recur ?component-type*))

     ((or ?component-type ?component-type* ...)
      (for-all recur (cons ?component-type ?component-type*)))

     ((and ?component-type ?component-type* ...)
      (for-all recur (cons ?component-type ?component-type*)))

     ((not ?item-type)
      (recur ?item-type))

     ((parent-of ?type)
      (recur ?type))

     ((ancestor-of ?type)
      (recur ?type))

     ((type-predicate ?type)
      (recur ?type))

     ((equality-predicate ?type)
      (recur ?type))

     ((comparison-procedure ?type)
      (recur ?type))

     ((hash-function ?type)
      (recur ?type))

     ((type-of ?expr)
      #t)

     (else #f))))

;;; --------------------------------------------------------------------

(define expression-expander-for-type-annotations
  (make-parameter (lambda (stx)
		    (assertion-violation 'expression-expander-for-type-annotations
		      "parameter not initialised"))))

(define (make-type-annotation annotation.stx)
  (type-annotation->object-type-spec annotation.stx (current-inferior-lexenv) annotation.stx))

(case-define* type-annotation->object-type-spec
  ((annotation.stx)
   (type-annotation->object-type-spec annotation.stx (current-inferior-lexenv) annotation.stx))
  ((annotation.stx lexenv)
   (type-annotation->object-type-spec annotation.stx lexenv annotation.stx))
  ((annotation.stx lexenv name.stx)
   ;;Let's think of:
   ;;
   ;;   (define-type <list-of-fixnums> (list-of <fixnum>))
   ;;
   ;;this function is called with:
   ;;
   ;;   #'(list-of <fixnum>)
   ;;
   ;;as ANNOTATION.STX argument and:
   ;;
   ;;   #'<list-of-fixnums>
   ;;
   ;;as NAME.STX argument.
   ;;
   (define (%mk-type-signature type.ann)
     (let ((synner (lambda (message subform)
		     (syntax-violation __who__ message annotation.stx subform))))
       (make-type-signature (syntax-object->type-signature-specs type.ann lexenv synner))))
   (syntax-match annotation.stx (pair list vector pair-of list-of nelist-of vector-of nevector-of
				      hashtable alist condition enumeration
				      or and not lambda case-lambda => parent-of ancestor-of
				      type-predicate equality-predicate comparison-procedure hash-function
				      type-of)
     (?type-id
      (identifier? ?type-id)
      (with-exception-handler
	  (lambda (E)
	    (raise (condition (make-who-condition __who__) E)))
	(lambda ()
	  (id->object-type-spec ?type-id lexenv))))

     ((pair ?car-type ?cdr-type)
      (let ((car.ots (type-annotation->object-type-spec ?car-type lexenv))
	    (cdr.ots (type-annotation->object-type-spec ?cdr-type lexenv)))
	(make-pair-type-spec car.ots cdr.ots name.stx)))

     ((list ?item-type* ...)
      (if (null? ?item-type*)
	  (<null>-ots)
	(make-list-type-spec (map (lambda (type.stx)
				    (type-annotation->object-type-spec type.stx lexenv))
			       ?item-type*)
			     name.stx)))

     ((vector ?item-type* ...)
      (if (null? ?item-type*)
	  (<empty-vector>-ots)
	(make-vector-type-spec (map (lambda (type.stx)
				      (type-annotation->object-type-spec type.stx lexenv))
				 ?item-type*)
			       name.stx)))

     ((pair-of ?item-type)
      (make-pair-of-type-spec (type-annotation->object-type-spec ?item-type lexenv)
			      name.stx))

     ((nelist-of ?item-type)
      (let ((type.ots (type-annotation->object-type-spec ?item-type lexenv)))
	(make-pair-type-spec type.ots (make-list-of-type-spec type.ots) name.stx)))

     ((list-of ?item-type)
      (make-list-of-type-spec (type-annotation->object-type-spec ?item-type lexenv)
			      name.stx))

     ((vector-of ?item-type)
      (make-vector-of-type-spec (type-annotation->object-type-spec ?item-type lexenv)
				name.stx))

     ;; ((nevector-of ?item-type)
     ;;  (let ((type.ots (type-annotation->object-type-spec ?item-type lexenv)))
     ;; 	(make-vector-type-spec type.ots name.stx)))

     ((hashtable ?key-type ?value-type)
      (make-hashtable-type-spec (type-annotation->object-type-spec ?key-type)
				(type-annotation->object-type-spec ?value-type)
				name.stx))

     ((alist ?key-type ?value-type)
      (make-alist-type-spec (type-annotation->object-type-spec ?key-type)
			    (type-annotation->object-type-spec ?value-type)
			    name.stx))

     ((enumeration ?symbol ?symbol* ...)
      (let ((sym*.stx (cons ?symbol ?symbol*)))
	(for-all (lambda (sym.stx)
		   (unless (identifier? sym.stx)
		     (syntax-violation __who__
		       "expected symbol object as component of enumeration"
		       annotation.stx sym.stx)))
	  sym*.stx)
	(let ((sym*.stx (delete-duplicate-identifiers sym*.stx)))
	  (make-enumeration-type-spec (syntax->datum sym*.stx)
				      (cons (enumeration-id) sym*.stx)))))

     ((condition)
      (make-compound-condition-type-spec '()))

     ((condition ?single-component-type)
      ;;We want:
      ;;
      ;;   (condition &who) == &who
      ;;   (condition (condition ...)) == (condition ...)
      ;;   (condition <compound-condition>) == <compound-condition>
      ;;
      (receive-and-return (ots)
	  (type-annotation->object-type-spec ?single-component-type lexenv)
	(unless (or (simple-condition-object-type-spec? ots)
		    (compound-condition-type-spec?      ots)
		    (<compound-condition>-ots?          ots)
		    (<condition>-ots?                   ots))
	  (syntax-violation __who__
	    "expected condition object as component of compound condition object"
	    annotation.stx ?single-component-type))))

     ((condition ?component-type ?component-type* ...)
      (make-compound-condition-type-spec (map (lambda (type.stx)
						(type-annotation->object-type-spec type.stx lexenv))
					   (cons ?component-type ?component-type*))))

     ((lambda ?argtypes => ?rettypes)
      (make-closure-type-spec (make-case-lambda-signature
			       (list (make-lambda-signature (%mk-type-signature ?rettypes)
							    (%mk-type-signature ?argtypes))))
			      name.stx))

     ((case-lambda (?argtypes0 => ?rettypes0) (?argtypes* => ?rettypes*) ...)
      (make-closure-type-spec (make-case-lambda-signature
			       (cons (make-lambda-signature
				      (%mk-type-signature ?rettypes0)
				      (%mk-type-signature ?argtypes0))
				     (map (lambda (argtypes.stx rettypes.stx)
					    (make-lambda-signature
					     (%mk-type-signature rettypes.stx)
					     (%mk-type-signature argtypes.stx)))
				       ?argtypes* ?rettypes*)))
			      name.stx))

     ((or ?single-component-type)
      (type-annotation->object-type-spec ?single-component-type lexenv))

     ((or ?component-type ?component-type* ...)
      ($union-of-type-specs (map (lambda (type.stx)
				   (type-annotation->object-type-spec type.stx lexenv))
			      (cons ?component-type ?component-type*))))

     ((and ?single-component-type)
      (type-annotation->object-type-spec ?single-component-type lexenv))

     ((and ?component-type ?component-type* ...)
      (intersection-of-type-specs (map (lambda (type.stx)
					 (type-annotation->object-type-spec type.stx lexenv))
				    (cons ?component-type ?component-type*))))

     ((not ?item-type)
      (let ((item.ots (type-annotation->object-type-spec ?item-type lexenv)))
	(make-complement-type-spec item.ots)))

     ((parent-of ?type)
      (or (object-type-spec.parent-ots (type-annotation->object-type-spec ?type lexenv))
	  (syntax-violation __who__ "type annotation has no parent" annotation.stx ?type)))

     ((ancestor-of ?type)
      ;;If ?TYPE has no ancestors: its ancestors list is null.
      (make-ancestor-of-type-spec (type-annotation->object-type-spec ?type lexenv)))

     ((type-predicate ?type)
      (type-annotation->object-type-spec (bless `(lambda (,?type) => (<boolean>))) lexenv))

     ((equality-predicate ?type)
      (type-annotation->object-type-spec (bless `(lambda (,?type ,?type) => (<boolean>))) lexenv))

     ((comparison-procedure ?type)
      (type-annotation->object-type-spec (bless `(lambda (,?type ,?type) => (<fixnum>)))  lexenv))

     ((hash-function ?type)
      (type-annotation->object-type-spec (bless `(lambda (,?type) => (<non-negative-fixnum>)))))

     ((type-of ?expr)
      ((expression-expander-for-type-annotations) ?expr lexenv))

     (else
      (syntax-violation __who__ "invalid type annotation" annotation.stx)))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
