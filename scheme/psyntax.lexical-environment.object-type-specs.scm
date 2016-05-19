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
;;           |------------> <scheme-type-spec>
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
;;	     |------------> <list-of-type-spec>
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
;;
;;The type "<object-type-spec>" has a field  PARENT-OTS that is used to represent the
;;hierarchy of Scheme-level object-types.
;;
;;All   the  built-in   Scheme  object   types  are   represented  by   instances  of
;;"<scheme-type-spec>".  So  "<top>", "<fixnum>", "<string>", "<list>"  et cetera are
;;represented by instances of "<scheme-type-spec>".
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
	 object-type-spec.parent-ots			object-type-spec.type-predicate-stx
	 object-type-spec.constructor-stx		object-type-spec.destructor-stx
	 object-type-spec.safe-accessor-stx		object-type-spec.safe-mutator-stx
	 object-type-spec.equality-predicate		object-type-spec.comparison-procedure
	 object-type-spec.hash-function			object-type-spec.applicable-hash-function
	 object-type-spec.applicable-method-stx		object-type-spec.ancestors-ots*
	 object-type-spec.single-value-validator-lambda-stx
	 object-type-spec.list-validator-lambda-stx

	 object-type-spec.matching-super-and-sub?	object-type-spec.compatible-super-and-sub?
	 object-type-spec=?
	 object-type-spec.common-ancestor		object-type-spec.procedure?
	 object-type-spec.list-sub-type?		object-type-spec.vector-sub-type?
	 object-type-specs-delete-duplicates

	 <scheme-type-spec>
	 <scheme-type-spec>-rtd				<scheme-type-spec>-rcd
	 make-scheme-type-spec				scheme-type-spec?
	 scheme-type-spec.type-descriptor-id

	 <closure-type-spec>
	 <closure-type-spec>-rtd			<closure-type-spec>-rcd
	 make-closure-type-spec				closure-type-spec?
	 closure-type-spec.signature			closure-type-spec.set-new-retvals-when-untyped!
	 closure-type-spec.thunk?

	 <struct-type-spec>
	 <struct-type-spec>-rtd				<struct-type-spec>-rcd
	 make-struct-type-spec				struct-type-spec?
	 struct-type-spec.std

	 <record-type-spec>
	 <record-type-spec>-rtd				<record-type-spec>-rcd
	 make-record-type-spec				record-type-spec?
	 record-type-spec.rtd-id			record-type-spec.rcd-id
	 record-type-spec.super-protocol-id
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
	 hashtable-type-spec.key-ots			hashtable-type-spec.value-ots

	 <alist-type-spec>
	 <alist-type-spec>-rtd				<alist-type-spec>-rcd
	 make-alist-type-spec				alist-type-spec?
	 alist-type-spec.key-ots			alist-type-spec.value-ots

	 <enumeration-type-spec>
	 <enumeration-type-spec>-rtd			<enumeration-type-spec>-rcd
	 make-enumeration-type-spec			enumeration-type-spec?
	 enumeration-type-spec.symbol*			enumeration-type-spec.member?

	 ;;;

	 <union-type-spec>
	 <union-type-spec>-rtd				<union-type-spec>-rcd
	 make-union-type-spec				union-of-type-specs
	 union-type-spec?				union-type-spec.component-ots*

	 <intersection-type-spec>
	 <intersection-type-spec>-rtd			<intersection-type-spec>-rcd
	 make-intersection-type-spec			intersection-of-type-specs
	 intersection-type-spec?			intersection-type-spec.component-ots*

	 <complement-type-spec>
	 <complement-type-spec>-rtd			<complement-type-spec>-rcd
	 make-complement-type-spec			complement-type-spec?
	 complement-type-spec.item-ots

	 ;;;

	 <ancestor-of-type-spec>
	 <ancestor-of-type-spec>-rtd			<ancestor-of-type-spec>-rcd
	 make-ancestor-of-type-spec			ancestor-of-type-spec?
	 ancestor-of-type-spec.item-ots			ancestor-of-type-spec.component-ots*

	 ;;;

	 <label-type-spec>-rtd				<label-type-spec>-rcd
	 <label-type-spec>
	 make-label-type-spec				label-type-spec?

	 ;;;

	 make-type-annotation
	 syntax-object.type-annotation?
	 type-annotation->object-type-spec
	 expression-expander-for-type-annotations

	 #| end of export list |# )

  (import (only (vicare language-extensions syntaxes)
		define-equality/sorting-predicate))


;;;; basic object-type specification
;;
;;This  record-type is  used as  base  type for  all the  Scheme objects  expand-time
;;specifications.
;;
;;We must handle this type as if it  is an "abstract" type: we must never instantiate
;;it directly, rather we  must define subtype and instantiate that.   This is why the
;;maker of "<object-type-spec>" is not exported by the module.
;;
(define-record-type (<object-type-spec> make-object-type-spec object-type-spec?)
  (nongenerative *3*vicare:expander:<object-type-spec>)
  (fields
    (mutable name-or-maker)
		;A  procedure that,  applied to  this  OTS, returns  a syntax  object
		;representing the name of this type.  For some types it is the actual
		;type  identifier, for  example:  "<fixnum>,  "<string>".  For  other
		;types it is a syntax object like "(list-of <fixnum>)".

    (immutable parent-ots		object-type-spec.parent-ots)
		;False or an instance of "<object-type-spec>" representing the parent
		;of this object-type.
		;
		;We need to remember that:
		;
		;* When defining  a built-in Scheme type, the  following types cannot
		;be subtyped: <no-return>, <void>, <null>, <empty-vector>.
		;
		;* Besides  the built-in  Scheme types, the  pari subtypes,  the list
		;subtypes, the  vector subtypes, the only  object-type specifications
		;for which we might specify a parent are records.
		;
		;*  The  subtypes of  pairs,  lists  and  vectors cannot  be  further
		;subtyped.

    (immutable type-annotation-maker)
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

    (mutable type-predicate)
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

    (immutable safe-accessors-table	object-type-spec.safe-accessors-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;syntax objects  representing Scheme  expressions that,  expanded and
		;evaluated at run-time, return the associated safe field accessor.  A
		;field accessor is meant to be used as:
		;
		;   (?accessor ?instance)
		;
		;and called explicitly with the SLOT-REF syntax.

    (immutable safe-mutators-table	object-type-spec.safe-mutators-table)
		;Null or  an alist  mapping symbols representing  the field  names to
		;syntax objects  representing Scheme  expressions that,  expanded and
		;evaluated at run-time, return the  associated safe field mutator.  A
		;field mutator is meant to be used as:
		;
		;   (?mutator ?instance ?new-field-value)
		;
		;and called explicitly with the SLOT-SET! syntax.

    (immutable methods-table		object-type-spec.methods-table)
		;Null or  an alist mapping  symbols representing the method  names to
		;syntax objects  representing Scheme  expressions that,  expanded and
		;evaluated at  run-time, return the  associated method.  A  method is
		;meant to be used as:
		;
		;   (?method ?instance ?arg ...)
		;
		;and called explicitly with the METHOD-CALL syntax.

    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (define* (make-object-type-spec name {parent.ots (or not object-type-spec?)}
				      type-annotation-maker
				      constructor-stx destructor-stx type-predicate-stx
				      equality-predicate.id comparison-procedure.id hash-function.id
				      safe-accessors-table safe-mutators-table methods-table)
	(make-record name parent.ots type-annotation-maker
		     constructor-stx destructor-stx type-predicate-stx
		     equality-predicate.id comparison-procedure.id hash-function.id
		     safe-accessors-table safe-mutators-table methods-table))
      make-object-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[object-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define <object-type-spec>-rtd
  (record-type-descriptor <object-type-spec>))

(define <object-type-spec>-rcd
  (record-constructor-descriptor <object-type-spec>))

;;; --------------------------------------------------------------------

(define* (object-type-spec.name {ots object-type-spec?})
  (let ((thing (<object-type-spec>-name-or-maker ots)))
    (if (procedure? thing)
	(thing ots)
      thing)))

(define* (object-type-spec.type-annotation {ots object-type-spec?})
  ((<object-type-spec>-type-annotation-maker ots) ots))

(define* (object-type-spec.type-predicate-stx {ots object-type-spec?})
  (let ((thing (<object-type-spec>-type-predicate ots)))
    (if (procedure? thing)
	(receive-and-return (pred.stx)
	    (thing ots)
	  (<object-type-spec>-type-predicate-set! ots pred.stx))
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

(define* (object-type-spec.ancestors-ots* {ots object-type-spec?})
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
	      (cons parent.ots (object-type-spec.ancestors-ots* parent.ots))))
	(else '())))

(module (object-type-spec.safe-accessor-stx
	 object-type-spec.safe-mutator-stx
	 object-type-spec.applicable-method-stx)

  (define* (object-type-spec.safe-accessor-stx {ots object-type-spec?} field-name.sym)
    ;;OTS must an object-type specification  record.  FIELD-NAME.SYM must be a symbol
    ;;representing a field name in the object-type specification.
    ;;
    ;;If  FIELD-NAME.SYM  is EQ?   to  an  object's  field  name: return  a  symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded  and  evaluated  at  run-time,  returns  the  field's  safe  accessor;
    ;;otherwise return false.
    ;;
    (%sexp-retriever ots field-name.sym object-type-spec.safe-accessors-table))

  (define* (object-type-spec.safe-mutator-stx {ots object-type-spec?} field-name.sym)
    ;;OTS must an object-type specification  record.  FIELD-NAME.SYM must be a symbol
    ;;representing a field name in the object-type specification.
    ;;
    ;;If FIELD-NAME.SYM is EQ?   to an object's field name and  the field is mutable:
    ;;return  a symbolic  expression  (to  be BLESSed  later)  representing a  Scheme
    ;;expression which, expanded and evaluated  at run-time, returns the field's safe
    ;;mutator.
    ;;
    ;;If FIELD-NAME.SYM is EQ?  to an object's field name and the field is immutable:
    ;;return the boolean true, so that a descriptive error message can be raised.
    ;;
    ;;Otherwise return false.
    ;;
    (%sexp-retriever ots field-name.sym object-type-spec.safe-mutators-table))

  (define* (object-type-spec.applicable-method-stx {ots object-type-spec?} method-name.sym)
    ;;OTS must an object-type specification record.  METHOD-NAME.SYM must be a symbol
    ;;representing a method name in the object-type specification.
    ;;
    ;;If  METHOD-NAME.SYM is  EQ?   to an  object's method  name:  return a  symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated at run-time,  returns the method's applicable; otherwise
    ;;return false.
    ;;
    (%sexp-retriever ots method-name.sym object-type-spec.methods-table))

  (define (%sexp-retriever ots name.sym table-getter)
    ;;TABLE-GETTER must be a function which, applied to the OTS, returns the required
    ;;association list.
    (cond ((assq name.sym (table-getter ots))
	   ;;The field name is known; extract  the symbolic expression from the alist
	   ;;entry and return it.
	   => cdr)
	  ((object-type-spec.parent-ots ots)
	   => (lambda (parent.ots)
		(%sexp-retriever parent.ots name.sym table-getter)))
	  (else #f)))

  #| end of module |# )


;;;; basic object-type specification: matching super and sub types

(define* (object-type-spec.matching-super-and-sub? {super.ots object-type-spec?} {sub.ots object-type-spec?})
  ;;This  is the  core of  the syntax  TYPE-SUPER-AND-SUB? and  it is  used in  other
  ;;contexts too.
  ;;
  ;;* In the context of an operator application:
  ;;
  ;;     (?operator ?operand ...)
  ;;
  ;;  return true  if a value of  type SUB.OTS used as operand  matches an operator's
  ;;  argument of type of SUPER.OTS.  In other words, return true if:
  ;;
  ;;     (type-super-and-sub ?argument-type-annotation (type-of ?operand))
  ;;
  ;;* In the context of a IS-A? syntax use:
  ;;
  ;;     (is-a? ?expr ?type-annotation)
  ;;
  ;;  return  true if the  type of ?EXPR  is a sub-type  of the type  annotation.  In
  ;;  other words, return true if:
  ;;
  ;;     (type-super-and-sub? ?type-annotation (type-of ?expr))
  ;;
  ;;
  ;;NOTE This  function is a single  point of truth:  it is the *only*  function that
  ;;determines if two OTSs are matching super-type and sub-type.
  ;;
  ;; (debug-print __who__
  ;; 	       (object-type-spec.name super.ots)
  ;; 	       (object-type-spec.name sub.ots))
  (cond ((eq? super.ots sub.ots))

	((<untyped>-ots? super.ots)
	 (not (or (<void>-ots?            sub.ots)
		  (<no-return>-ots?       sub.ots))))

	((<top>-ots? super.ots)
	 ;;Fast track: "<top>" is the super-type of all the types.
	 (not (or (<void>-ots?            sub.ots)
		  (<no-return>-ots?       sub.ots)
		  (ancestor-of-type-spec? sub.ots))))

	;;NOTE We do *not* insert here a branch like:
	;;
	;;   ((<top>-ots?     sub.ots) . ?body)
	;;   ((<untyped>-ots? sub.ots) . ?body)
	;;
	;;because "<top>"  or "<untyped>"  as sub-type  may match  or not,  we cannot
	;;establish it here.

	((and (or (scheme-type-spec? super.ots) (record-type-spec? super.ots) (struct-type-spec? super.ots))
	      (or (scheme-type-spec?   sub.ots) (record-type-spec?   sub.ots) (struct-type-spec?   sub.ots)))
	 ;;If we are here we know that we have to search for matching identifiers, so
	 ;;we loop locally.
	 (let recur ((sub.ots sub.ots))
	   ;; (debug-print 'direct-matching
	   ;; 		super.ots
	   ;; 		sub.ots
	   ;; 		(object-type-spec.name super.ots)
	   ;; 		(object-type-spec.name sub.ots)
	   ;; 		(free-identifier=? (object-type-spec.name super.ots)
	   ;; 				   (object-type-spec.name sub.ots)))
	   (cond ((free-identifier=? (object-type-spec.name super.ots)
				     (object-type-spec.name   sub.ots)))
		 ((object-type-spec.parent-ots sub.ots)
		  => (lambda (sub-parent.ots)
		       (or (eq? super.ots sub-parent.ots)
			   (recur sub-parent.ots))))
		 (else #f))))

;;; --------------------------------------------------------------------
;;; We want to do labels before unions

	((label-type-spec? super.ots)
	 (if (label-type-spec.with-type-predicate? super.ots)
	     ;;A label  with predicate used  as super-type  must never match:  it can
	     ;;only be  compatible.  This  is because  we want  to apply  the label's
	     ;;predicate to validate the value.
	     #f
	   ;;A label without  predicate use as super-type is an  alias for its parent
	   ;;type annotation: it matches if its parent matches.
	   (object-type-spec.matching-super-and-sub? (object-type-spec.parent-ots super.ots)
						     sub.ots)))

	((label-type-spec? sub.ots)
	 ;;A sub-type label matches its parent and its parent's ancectors.
	 (object-type-spec.matching-super-and-sub? super.ots
						   (object-type-spec.parent-ots sub.ots)))

;;; --------------------------------------------------------------------
;;; We really want to do the unions first.

	((union-type-spec? super.ots)
	 (cond ((union-type-spec? sub.ots)
		;;Every sub-component must match a super-component.
		;;
		;; (type-super-and-sub? (or <number> <vector>)
		;;                      (or <fixnum> (vector-of <string>)))
		;; => #t
		;;
		(for-all (lambda (sub-component.ots)
			   (exists (lambda (super-component.ots)
				     (object-type-spec.matching-super-and-sub? super-component.ots sub-component.ots))
			     (union-type-spec.component-ots* super.ots)))
		  (union-type-spec.component-ots* sub.ots)))
	       (else
		;; (type-super-and-sub? (or <number> <string>) <string>) => #t
		;; (type-super-and-sub? (or <number> <string>) <fixnum>) => #t
		(exists (lambda (super-component.ots)
			  (object-type-spec.matching-super-and-sub? super-component.ots sub.ots))
		  (union-type-spec.component-ots* super.ots)))))

	((union-type-spec? sub.ots)
	 (let ((sub-component-ots* (union-type-spec.component-ots* sub.ots)))
	   (cond ((ancestor-of-type-spec? super.ots)
		  (exists (lambda (super-component.ots)
			    (exists (lambda (sub-component.ots)
				      ($object-type-spec=? super-component.ots sub-component.ots))
			      sub-component-ots*))
		    (ancestor-of-type-spec.component-ots* super.ots)))
		 (else
		  (for-all (lambda (sub-component.ots)
			     (object-type-spec.matching-super-and-sub? super.ots sub-component.ots))
		    sub-component-ots*)))))

;;; --------------------------------------------------------------------
;;; We really want to do the intersections first.

	((intersection-type-spec? super.ots)
	 (cond ((intersection-type-spec? sub.ots)
		;;Every sub-component must match every super-component.
		;;
		;; (type-super-and-sub? (and <number> <positive>)
		;;                      (and <positive-fixnum>
		;;                           <positive-ratnum>))
		;; => #t
		(for-all (lambda (super-component.ots)
			   (for-all (lambda (sub-component.ots)
				      (object-type-spec.matching-super-and-sub? super-component.ots sub-component.ots))
			     (intersection-type-spec.component-ots* sub.ots)))
		  (intersection-type-spec.component-ots* super.ots)))
	       (else
		;; (type-super-and-sub? (and <exact> <positive>) <positive-fixnum>) => #t
		(for-all (lambda (super-component.ots)
			   (object-type-spec.matching-super-and-sub? super-component.ots sub.ots))
		  (intersection-type-spec.component-ots* super.ots)))))

	((intersection-type-spec? sub.ots)
	 (let ((sub-component-ots* (intersection-type-spec.component-ots* sub.ots)))
	   (cond ((ancestor-of-type-spec? super.ots)
		  (exists (lambda (super-component.ots)
			    (for-all (lambda (sub-component.ots)
				       ($object-type-spec=? super-component.ots sub-component.ots))
			      sub-component-ots*))
		    (ancestor-of-type-spec.component-ots* super.ots)))
		 (else
		  (for-all (lambda (sub-component.ots)
			     (object-type-spec.matching-super-and-sub? super.ots sub-component.ots))
		    sub-component-ots*)))))

;;; --------------------------------------------------------------------
;;; We really want to do the complements first.

	((complement-type-spec? super.ots)
	 (let ((super-item.ots (complement-type-spec.item-ots super.ots)))
	   (cond ((ancestor-of-type-spec? super-item.ots)
		  (not (object-type-spec.matching-super-and-sub? super-item.ots sub.ots)))
		 ((complement-type-spec? sub.ots)
		  ;;If something is not a "<number>", for sure it is not a "<fixnum>":
		  ;;
		  ;; (type-super-and-sub? (not <fixnum>)
		  ;;                      (not <number>)) => #t
		  ;;
		  ;; (type-super-and-sub? (not <number>)
		  ;;                      (not <string>)) => #f
		  ;;
		  (object-type-spec.matching-super-and-sub? (complement-type-spec.item-ots sub.ots) super-item.ots))
		 (else
		  ;; (type-super-and-sub? (not <string>) <fixnum>) => #t
		  ;; (type-super-and-sub? (not <fixnum>) <exact-integer>) => #t
		  (and (not (object-type-spec.matching-super-and-sub? super-item.ots sub.ots))
		       (not (object-type-spec.matching-super-and-sub? sub.ots super-item.ots)))))))

	((complement-type-spec? sub.ots)
	 #f)

;;; --------------------------------------------------------------------
;;; We really want to do the ancestors first.

	((ancestor-of-type-spec? super.ots)
	 (let ((super-component-ots* (ancestor-of-type-spec.component-ots* super.ots)))
	   (cond ((union-type-spec? sub.ots)
		  ;;This case is already done above, in the union stuff.
		  (let ((sub-component-ots* (union-type-spec.component-ots* sub.ots)))
		    (exists (lambda (super-component.ots)
			      (exists (lambda (sub-component.ots)
					($object-type-spec=? super-component.ots sub-component.ots))
				sub-component-ots*))
		      super-component-ots*)))
		 ((intersection-type-spec? sub.ots)
		  ;;This case is already done above, in the intersection stuff.
		  (let ((sub-component-ots* (intersection-type-spec.component-ots* sub.ots)))
		    (exists (lambda (super-component.ots)
			      (for-all (lambda (sub-component.ots)
					 ($object-type-spec=? super-component.ots sub-component.ots))
				sub-component-ots*))
		      super-component-ots*)))
		 (else
		  (exists (lambda (super-component.ots)
			    ($object-type-spec=? super-component.ots sub.ots))
		    super-component-ots*)))))

	((ancestor-of-type-spec? sub.ots)
	 (exists (lambda (sub-component.ots)
		   ($object-type-spec=? super.ots sub-component.ots))
	   (ancestor-of-type-spec.component-ots* sub.ots)))

;;; --------------------------------------------------------------------
;;; empty lists

	((<null>-ots? sub.ots)
	 ;;Special case:  we consider "<null>" as  sub-type of "<list>" and  of types
	 ;;represented by  "<list-of-type-spec>".  We need to  remember that "<null>"
	 ;;cannot be sub-typed.
	 (or (list-of-type-spec? super.ots)
	     (<list>-ots?        super.ots)
	     (<null>-ots?        super.ots)
	     (alist-type-spec?   super.ots)))

	((<null>-ots? super.ots)
	 ;;This matches only if SUB.OTS is itself "<null>".  But such case is handled
	 ;;above, so here we just return false.
	 #f)

;;; --------------------------------------------------------------------
;;; empty vectors

	((<empty-vector>-ots? sub.ots)
	 ;;Special case: we  consider "<empty-vector>" as sub-type  of "<vector>" and
	 ;;the  types represented  by "<vector-of-type-spec>".   We need  to remember
	 ;;that "<empty-vector>" cannot be sub-typed.
	 (or (vector-of-type-spec? super.ots)
	     (<vector>-ots?        super.ots)
	     (<empty-vector>-ots?  super.ots)))

	((<empty-vector>-ots? super.ots)
	 ;;This matches only if SUB.OTS is itself "<empty-vector>".  But such case is
	 ;;handled above, so here we just return false.
	 #f)

;;; --------------------------------------------------------------------
;;; pair type specifications

	((<pair>-ots? super.ots)
	 (or (<pair>-ots?        sub.ots)
	     (pair-type-spec?    sub.ots)
	     (pair-of-type-spec? sub.ots)
	     (<nelist>-ots?      sub.ots)
	     (list-type-spec?    sub.ots)))

	((pair-type-spec? super.ots)
	 (cond ((pair-type-spec? sub.ots)
		;; (type-super-and-sub? (pair <number> <number>) (pair <fixnum> <flonum>)) => #t
		(and (object-type-spec.matching-super-and-sub? (pair-type-spec.car-ots super.ots)
							       (pair-type-spec.car-ots sub.ots))
		     (object-type-spec.matching-super-and-sub? (pair-type-spec.cdr-ots super.ots)
							       (pair-type-spec.cdr-ots sub.ots))))
	       ((pair-of-type-spec? sub.ots)
		;; (type-super-and-sub? (pair <number> <number>) (pair-of <fixnum>)) => #t
		(let ((sub-item.ots (pair-of-type-spec.item-ots sub.ots)))
		  (and (object-type-spec.matching-super-and-sub? (pair-type-spec.car-ots super.ots) sub-item.ots)
		       (object-type-spec.matching-super-and-sub? (pair-type-spec.cdr-ots super.ots) sub-item.ots))))
	       ((list-type-spec? sub.ots)
		;; (type-super-and-sub? (pair <number> <null>) (list <number>)) => #t
		(let ((sub-item*.ots (list-type-spec.item-ots* sub.ots)))
		  (and (object-type-spec.matching-super-and-sub? (pair-type-spec.car-ots super.ots)
								 (car sub-item*.ots))
		       (object-type-spec.matching-super-and-sub? (pair-type-spec.cdr-ots super.ots)
								 (make-null-or-list-type-spec (cdr sub-item*.ots))))))
	       ;;No LIST-OF type here: a LIST-OF type annotation does not specify the
	       ;;number  of items,  while a  PAIR annotation  specifies at  least one
	       ;;item.
	       (else #f)))

	((pair-of-type-spec? super.ots)
	 (cond ((pair-of-type-spec? sub.ots)
		;; (type-super-and-sub? (pair-of <number>) (pair-of <fixnum>))
		(object-type-spec.matching-super-and-sub? (pair-of-type-spec.item-ots super.ots)
							  (pair-of-type-spec.item-ots sub.ots)))
	       ((pair-type-spec? sub.ots)
		;; (type-super-and-sub? (pair-of <number>) (pair <fixnum> <fixnum>))
		(let ((super-item.ots (pair-of-type-spec.item-ots super.ots)))
		  (and (object-type-spec.matching-super-and-sub? super-item.ots (pair-type-spec.car-ots sub.ots))
		       (object-type-spec.matching-super-and-sub? super-item.ots (pair-type-spec.cdr-ots sub.ots)))))
	       ((list-type-spec? sub.ots)
		;; (type-super-and-sub? (pair-of (or <number> <null>)) (list <fixnum>))
		(let ((super-item.ots	(pair-of-type-spec.item-ots super.ots))
		      (sub-item*.ots	(list-type-spec.item-ots* sub.ots)))
		  (and (object-type-spec.matching-super-and-sub? super-item.ots (car sub-item*.ots))
		       (object-type-spec.matching-super-and-sub? super-item.ots (make-null-or-list-type-spec (cdr sub-item*.ots))))))
	       ;;No LIST-OF type here: a LIST-OF type annotation does not specify the
	       ;;number of items,  while a PAIR-OF annotation specifies  at least one
	       ;;item.
	       (else #f)))

;;; --------------------------------------------------------------------
;;; list type specifications

	((<list>-ots? super.ots)
	 ;;Notice  that the  cases in  which SUB.OTS  is a  "<scheme-type-spec>" have
	 ;;already been handled above, with the exception of "<null>".
	 (cond ((or (<null>-ots?        sub.ots)
		    (list-of-type-spec? sub.ots)
		    (list-type-spec?    sub.ots)))
	       ((pair-type-spec? sub.ots)
		;;This matches if the cdr is a list.
		(object-type-spec.matching-super-and-sub? (<list>-ots) (pair-type-spec.cdr-ots sub.ots)))
	       ((pair-of-type-spec? sub.ots)
		;;This matches if the item is a list.
		(object-type-spec.matching-super-and-sub? (<list>-ots) (pair-of-type-spec.item-ots sub.ots)))
	       (else #f)))

	((<nelist>-ots? super.ots)
	 ;;Notice  that the  cases in  which SUB.OTS  is a  "<scheme-type-spec>" have
	 ;;already been handled above.
	 (cond ((list-type-spec? sub.ots))
	       ((pair-type-spec? sub.ots)
		;;This matches if the cdr is a list.
		(object-type-spec.matching-super-and-sub? (<list>-ots) (pair-type-spec.cdr-ots sub.ots)))
	       ((pair-of-type-spec? sub.ots)
		;;This matches if the item is a list.
		(object-type-spec.matching-super-and-sub? (<list>-ots) (pair-of-type-spec.item-ots sub.ots)))
	       (else #f)))

	((list-type-spec? super.ots)
	 ;;SUPER.OTS is a non-empty list holding a known and fixed number of items of
	 ;;known type.
	 (cond ((list-type-spec? sub.ots)
		;;We want the following:
		;;
		;;   (type-super-and-sub? (list <number>) (list <fixnum>))	=> #t
		;;
		(and (= (list-type-spec.length super.ots)
			(list-type-spec.length sub.ots))
		     (for-all object-type-spec.matching-super-and-sub?
		       (list-type-spec.item-ots* super.ots)
		       (list-type-spec.item-ots* sub.ots))))
	       ((pair-type-spec? sub.ots)
		;;We want:
		;;
		;;   (type-super-and-sub? (list <fixnum>) (pair <fixnum> <null>)) => #t
		;;
		(let ((super-item*.ots (list-type-spec.item-ots* super.ots)))
		  (and (object-type-spec.matching-super-and-sub? (car super-item*.ots)
								 (pair-type-spec.car-ots sub.ots))
		       (object-type-spec.matching-super-and-sub? (make-null-or-list-type-spec (cdr super-item*.ots))
								 (pair-type-spec.cdr-ots sub.ots)))))
	       ((pair-of-type-spec? sub.ots)
		;; (type-super-and-sub? (list <null>) (pair-of <null>)) => #t
		(let ((super-item*.ots	(list-type-spec.item-ots*   super.ots))
		      (sub-item.ots	(pair-of-type-spec.item-ots sub.ots)))
		  (and (object-type-spec.matching-super-and-sub? (car super-item*.ots) sub-item.ots)
		       (object-type-spec.matching-super-and-sub? (make-null-or-list-type-spec (cdr super-item*.ots)) sub-item.ots))))
	       ((alist-type-spec? sub.ots)
		(let ((sub-key.ots   (alist-type-spec.key-ots   sub.ots))
		      (sub-value.ots (alist-type-spec.value-ots sub.ots)))
		  (for-all (lambda (super-item.ots)
			     (cond ((pair-type-spec? super-item.ots)
				    ;; (type-super-and-sub? (list (pair <symbol> <number>) ...)
				    ;;                      (alist <symbol> <number>))
				    (and (object-type-spec.matching-super-and-sub? (pair-type-spec.car-ots super-item.ots)
										   sub-key.ots)
					 (object-type-spec.matching-super-and-sub? (pair-type-spec.cdr-ots super-item.ots)
										   sub-value.ots)))
				   ((pair-of-type-spec? super-item.ots)
				    ;; (type-super-and-sub? (alist <symbol> <number>)
				    ;;                      (list (pair-of (or <symbol> <number>)) ...))
				    (let ((super-super-item.ots (pair-of-type-spec.item-ots super-item.ots)))
				      (and (object-type-spec.matching-super-and-sub? sub-key.ots   super-super-item.ots)
					   (object-type-spec.matching-super-and-sub? sub-value.ots super-super-item.ots))))
				   (else #f)))
		    (list-type-spec.item-ots* super.ots))))
	       (else #f)))

	((list-of-type-spec? super.ots)
	 ;;SUPER.OTS is a  possibly empty list holding an unknown  number of items of
	 ;;known type.
	 (cond ((list-of-type-spec? sub.ots)
		;; (type-super-and-sub? (list-of <number>) (list-of <fixnum>)) => #t
		(object-type-spec.matching-super-and-sub? (list-of-type-spec.item-ots super.ots)
							  (list-of-type-spec.item-ots sub.ots)))
	       ((list-type-spec? sub.ots)
		;; (type-super-and-sub? (list-of <fixnum>) (list <fixnum>)) => #t
		;; (type-super-and-sub? (list-of <number>) (list <fixnum> <flonum>)) => #t
		(let ((super-item.ots (list-of-type-spec.item-ots super.ots)))
		  (for-all (lambda (sub-item.ots)
			     (object-type-spec.matching-super-and-sub? super-item.ots sub-item.ots))
		    (list-type-spec.item-ots* sub.ots))))
	       ((pair-type-spec? sub.ots)
		;; (type-super-and-sub? (list-of <fixnum>)
		;;                      (pair <fixnum> (list-of <fixnum>)))
		;; => #t
		(let ((super-item.ots	(list-of-type-spec.item-ots super.ots))
		      (sub-car.ots	(pair-type-spec.car-ots     sub.ots))
		      (sub-cdr.ots	(pair-type-spec.cdr-ots     sub.ots)))
		  (and (object-type-spec.matching-super-and-sub? super-item.ots sub-car.ots)
		       (object-type-spec.matching-super-and-sub? super.ots      sub-cdr.ots))))
	       ((alist-type-spec? sub.ots)
		(let ((super-item.ots (list-of-type-spec.item-ots super.ots)))
		  (cond ((pair-type-spec? super-item.ots)
			 ;; (type-super-and-sub? (list-of (pair <symbol> <number>))
			 ;;                      (alist <symbol> <number>))
			 (and (object-type-spec.matching-super-and-sub? (pair-type-spec.car-ots  super-item.ots)
									(alist-type-spec.key-ots sub.ots))
			      (object-type-spec.matching-super-and-sub? (pair-type-spec.cdr-ots    super-item.ots)
									(alist-type-spec.value-ots sub.ots))))
			((pair-of-type-spec? super-item.ots)
			 ;; (type-super-and-sub? (list-of (pair-of (or <symbol> <number>)))
			 ;;                      (alist <symbol> <number>))
			 (let ((super-super-item.ots (pair-of-type-spec.item-ots super-item.ots)))
			   (and (object-type-spec.matching-super-and-sub? super-super-item.ots (alist-type-spec.key-ots   sub.ots))
				(object-type-spec.matching-super-and-sub? super-super-item.ots (alist-type-spec.value-ots sub.ots)))))
			(else #f))))
	       ;;No PAIR-OF  types here: a  LIST-OF type annotation does  not specify
	       ;;the number of items, while  PAIR-OF annotations specify at least one
	       ;;item.  Notice that:
	       ;;
	       ;;   (type-super-and-sub? (list-of <fixnum>)
	       ;;                        (pair-of (or <fixnum> <null>)))
	       ;;   => #f
	       ;;
	       (else #f)))

;;; --------------------------------------------------------------------
;;; vector type specifications

	((<vector>-ots? super.ots)
	 ;;Notice  that the  cases in  which SUB.OTS  is a  "<scheme-type-spec>" have
	 ;;already been handled above.
	 (or (vector-of-type-spec? sub.ots)
	     (vector-type-spec?    sub.ots)))

	((<nevector>-ots? super.ots)
	 ;;Notice  that the  cases in  which SUB.OTS  is a  "<scheme-type-spec>" have
	 ;;already been handled above.
	 (vector-type-spec? sub.ots))

	((vector-type-spec? super.ots)
	 (cond ((vector-type-spec? sub.ots)
		;; (type-super-and-sub? (vector <number>) (vector <fixnum>)) => #t
		(and (= (vector-type-spec.length super.ots)
			(vector-type-spec.length sub.ots))
		     (for-all object-type-spec.matching-super-and-sub?
		       (vector-type-spec.item-ots* super.ots)
		       (vector-type-spec.item-ots* sub.ots))))
	       ;;No "(vector-of  ?type)" case here:  the "(vector ?type0  ?type ...)"
	       ;;type annotation  specifies at  least one  item, whiel  the VECTOR-OF
	       ;;annoation does not specify a length.
	       (else #f)))

	((vector-of-type-spec? super.ots)
	 (cond ((vector-of-type-spec? sub.ots)
		;; (type-super-and-sub? (vector-of <number>) (vector-of <fixnum>)) => #t
		(object-type-spec.matching-super-and-sub? (vector-of-type-spec.item-ots super.ots)
							  (vector-of-type-spec.item-ots sub.ots)))
	       ((vector-type-spec? sub.ots)
		;; (type-super-and-sub? (vector-of <fixnum>) (vector <fixnum>)) => #t
		;; (type-super-and-sub? (vector-of <number>) (vector <fixnum> <flonum>)) => #t
		(let ((super-item.ots (vector-of-type-spec.item-ots super.ots)))
		  (for-all (lambda (sub-item.ots)
			     (object-type-spec.matching-super-and-sub? super-item.ots sub-item.ots))
		    (vector-type-spec.item-ots* sub.ots))))
	       (else #f)))

;;; --------------------------------------------------------------------
;;; condition-object type specifications

	((compound-condition-type-spec? super.ots)
	 (cond ((compound-condition-type-spec? sub.ots)
		;;This is the case:
		;;
		;;   (is-a? (condition (make-who-condition 'ciao)
		;;                     (make-message-condition "ciao"))
		;;          (condition &who &message))
		;;
		;;every condition-object  type in  the super must  be present  in the
		;;sub.   Remember  that  all  the component  OTSs  are  instances  of
		;;"<record-type-spec>"  representing   a  Scheme-level   sub-type  of
		;;"&condition".
		(let ((sub-component*.ots (compound-condition-type-spec.component-ots* sub.ots)))
		  (for-all (lambda (super-component.ots)
			     (exists (lambda (sub-component.ots)
				       (%compare-super-with-sub-and-its-parents super-component.ots sub-component.ots))
			       sub-component*.ots))
		    (compound-condition-type-spec.component-ots* super.ots))))
	       (else #f)))

	((<compound-condition>-ots? super.ots)
	 ;;This is the case:
	 ;;
	 ;;   (is-a? ?expr <compound-condition>)
	 ;;
	 ;;we want a match, for example, in the following cases:
	 ;;
	 ;;   (is-a? (condition) <compound-condition>)
	 ;;   => #t
	 ;;
	 ;;   (is-a? (condition (make-who-condition 'ciao)
	 ;;                     (make-message-condition "ciao"))
	 ;;          <compound-condition>)
	 ;;   => #t
	 ;;
	 (%compare-super-with-sub-and-its-parents (<compound-condition>-ots) super.ots))

	((simple-condition-object-type-spec? super.ots)
	 (cond ((compound-condition-type-spec? sub.ots)
		;;This is the case:
		;;
		;;   (is-a? (condition (make-who-condition 'ciao)
		;;                     (make-message-condition "ciao"))
		;;          &who)
		;;   => #t
		;;
		;;   (is-a? (condition (make-who-condition 'ciao)
		;;                     (make-message-condition "ciao"))
		;;          &irritants)
		;;   => #f
		;;
		(let ((sub-component*.ots (compound-condition-type-spec.component-ots* sub.ots)))
		  (exists (lambda (sub-component.ots)
			    (%compare-super-with-sub-and-its-parents super.ots sub-component.ots))
		    sub-component*.ots)))
	       ((simple-condition-object-type-spec? sub.ots)
		;;This is the case:
		;;
		;;   (is-a? (make-who-condition 'ciao) &who)		=> #t
		;;   (is-a? (make-who-condition 'ciao) &message)	=> #f
		;;
		(%compare-super-with-sub-and-its-parents super.ots sub.ots))
	       (else #f)))

;;; --------------------------------------------------------------------
;;; closure type specifications

	((closure-type-spec? super.ots)
	 (cond ((closure-type-spec? sub.ots)
		;;For every  super-type signature there  must be a  matching sub-type
		;;signature; so that the sub can  be used everywhere the super can be
		;;used.  It does not matter if  the sub has clauses with non-matching
		;;signatures.
		(for-all (lambda (super.clause-signature)
			   (exists (lambda (sub.clause-signature)
				     (and (type-signature.super-and-sub? (lambda-signature.argvals super.clause-signature)
									 (lambda-signature.argvals sub.clause-signature))
					  (type-signature.super-and-sub? (lambda-signature.retvals super.clause-signature)
									 (lambda-signature.retvals sub.clause-signature))))
			     (case-lambda-signature.clause-signature* (closure-type-spec.signature sub.ots))))
		  (case-lambda-signature.clause-signature* (closure-type-spec.signature super.ots))))
	       (else #f)))

	((closure-type-spec? sub.ots)
	 (<procedure>-ots? super.ots))

;;; --------------------------------------------------------------------
;;; hashtables

	((hashtable-type-spec? super.ots)
	 (cond ((hashtable-type-spec? sub.ots)
		(and (object-type-spec.matching-super-and-sub? (hashtable-type-spec.key-ots super.ots)
							       (hashtable-type-spec.key-ots sub.ots))
		     (object-type-spec.matching-super-and-sub? (hashtable-type-spec.value-ots super.ots)
							       (hashtable-type-spec.value-ots sub.ots))))
	       (else #f)))

	((hashtable-type-spec? sub.ots)
	 (<hashtable>-ots? super.ots))

;;; --------------------------------------------------------------------
;;; alists

	((alist-type-spec? super.ots)
	 (cond ((alist-type-spec? sub.ots)
		(and (object-type-spec.matching-super-and-sub? (alist-type-spec.key-ots super.ots)
							       (alist-type-spec.key-ots sub.ots))
		     (object-type-spec.matching-super-and-sub? (alist-type-spec.value-ots super.ots)
							       (alist-type-spec.value-ots sub.ots))))
	       ((list-of-type-spec? sub.ots)
		(let ((sub-item.ots (list-of-type-spec.item-ots sub.ots)))
		  (cond ((pair-type-spec? sub-item.ots)
			 ;; (type-super-and-sub? (alist <symbol> <number>)
			 ;;                      (list-of (pair <symbol> <number>)))
			 (and (object-type-spec.matching-super-and-sub? (alist-type-spec.key-ots   super.ots)
									(pair-type-spec.car-ots    sub-item.ots))
			      (object-type-spec.matching-super-and-sub? (alist-type-spec.value-ots super.ots)
									(pair-type-spec.cdr-ots    sub-item.ots))))
			((pair-of-type-spec? sub-item.ots)
			 ;; (type-super-and-sub? (alist <symbol> <number>)
			 ;;                      (list-of (pair-of (or <symbol> <number>))))
			 (let ((sub-sub-item.ots (pair-of-type-spec.item-ots sub-item.ots)))
			   (and (object-type-spec.matching-super-and-sub? (alist-type-spec.key-ots   super.ots) sub-sub-item.ots)
				(object-type-spec.matching-super-and-sub? (alist-type-spec.value-ots super.ots) sub-sub-item.ots))))
			(else #f))))
	       ((list-type-spec? sub.ots)
		(let ((super-key.ots   (alist-type-spec.key-ots   super.ots))
		      (super-value.ots (alist-type-spec.value-ots super.ots)))
		  (for-all (lambda (sub-item.ots)
			     (cond ((pair-type-spec? sub-item.ots)
				    ;; (type-super-and-sub? (alist <symbol> <number>)
				    ;;                      (list (pair <symbol> <number>) ...))
				    (and (object-type-spec.matching-super-and-sub? super-key.ots
										   (pair-type-spec.car-ots sub-item.ots))
					 (object-type-spec.matching-super-and-sub? super-value.ots
										   (pair-type-spec.cdr-ots sub-item.ots))))
				   ((pair-of-type-spec? sub-item.ots)
				    ;; (type-super-and-sub? (alist <symbol> <number>)
				    ;;                      (list (pair-of (or <symbol> <number>)) ...))
				    (let ((sub-sub-item.ots (pair-of-type-spec.item-ots sub-item.ots)))
				      (and (object-type-spec.matching-super-and-sub? super-key.ots   sub-sub-item.ots)
					   (object-type-spec.matching-super-and-sub? super-value.ots sub-sub-item.ots))))
				   (else #f)))
		    (list-type-spec.item-ots* sub.ots))))
	       (else #f)))

;;; --------------------------------------------------------------------
;;; enumerations

	((enumeration-type-spec? super.ots)
	 (cond ((enumeration-type-spec? sub.ots)
		(list-of-symbols.subset? (enumeration-type-spec.symbol* sub.ots)
					 (enumeration-type-spec.symbol* super.ots)))
	       (else #f)))

;;; --------------------------------------------------------------------
;;; left-overs for SUB.OTS

	((or (<list>-ots?        sub.ots)
	     (<nelist>-ots?      sub.ots)
	     (list-type-spec?    sub.ots)
	     (list-of-type-spec? sub.ots))
	 ;;All the cases of SUPER.OTS that match have been handled above.
	 #f)

	((or (<vector>-ots?        sub.ots)
	     (vector-of-type-spec? sub.ots)
	     (vector-type-spec?    sub.ots))
	 ;;All the cases of SUPER.OTS that match have been handled above.
	 #f)

	((alist-type-spec? sub.ots)
	 (or (<list>-ots?   super.ots)
	     (<nelist>-ots? super.ots)))

;;; --------------------------------------------------------------------

	((object-type-spec.parent-ots sub.ots)
	 => (lambda (sub-parent.ots)
	      (object-type-spec.matching-super-and-sub? super.ots sub-parent.ots)))

	(else #f)))

(define (%compare-super-with-sub-and-its-parents super.ots sub.ots)
  ;;Recursive function.  Search SUPER.OTS  in the hierarchy  of SUB.OTS:  return true
  ;;when found, false otherwise.
  ;;
  ;;We need to remember that there may be multiple representations of the same object
  ;;types.  For  example: there  may be  multiple instances  of "<scheme-obect-type>"
  ;;representing the same type; so we need to compare them by name.
  ;;
  (cond (($object-type-spec=? super.ots sub.ots))
	((object-type-spec.parent-ots sub.ots)
	 => (lambda (sub-parent.ots)
	      (%compare-super-with-sub-and-its-parents super.ots sub-parent.ots)))
	(else #f)))


;;;; basic object-type specification: compatible super and sub types

(define* (object-type-spec.compatible-super-and-sub? {super.ots object-type-spec?} {sub.ots object-type-spec?})
  ;;This function  is used to check  for non-matching compatibility between  two type
  ;;specifications.       It      is      meant      to      be      called      when
  ;;OBJECT-TYPE-SPEC.MATCHING-SUPER-AND-SUB? has already returned  #f when applied to
  ;;the same operands.
  ;;
  ;;Usage example: when  applying a typed function  to a tuple of  operands, we check
  ;;the arguments' types  against the operands's types: if they  match, good; if they
  ;;are compatible we insert run-time validation; otherwise we raise an exception.
  ;;
  ;;As  example: a  "<number>" argument  matches a  "<fixnum>" operand;  a "<fixnum>"
  ;;argument is compatible with a "<number>" operand.
  ;;
  ;; (debug-print __who__
  ;; 	       (object-type-spec.name super.ots)
  ;; 	       (object-type-spec.name sub.ots))
  (define (%matching-or-compatible? super.ots sub.ots)
    (or (object-type-spec.matching-super-and-sub?   super.ots sub.ots)
	(object-type-spec.compatible-super-and-sub? super.ots sub.ots)))
  (cond

   ((<untyped>-ots? sub.ots)
    ;;Untyped is compatible with everything.
    #t)

;;; --------------------------------------------------------------------
;;; we want to do labels before unions

   ((label-type-spec? super.ots)
    (let ((super-parent.ots (object-type-spec.parent-ots super.ots)))
      (cond ((label-type-spec? sub.ots)
	     (%matching-or-compatible? super-parent.ots (object-type-spec.parent-ots sub.ots)))
	    (else
	     ;; (debug-print __who__
	     ;; 	       (object-type-spec.name super.ots)
	     ;; 	       (object-type-spec.name super-parent.ots)
	     ;; 	       (object-type-spec.name sub.ots))
	     (%matching-or-compatible? super-parent.ots sub.ots)))))

   ((label-type-spec? sub.ots)
    (%matching-or-compatible? super.ots (object-type-spec.parent-ots sub.ots)))

;;; --------------------------------------------------------------------

   ((union-type-spec? sub.ots)
    (exists (lambda (component-sub.ots)
	      ;; (debug-print (object-type-spec.name super.ots)
	      ;; 		(object-type-spec.name component-sub.ots)
	      ;; 		(object-type-spec.matching-super-and-sub? super.ots component-sub.ots))
	      (%matching-or-compatible? super.ots component-sub.ots))
      (union-type-spec.component-ots* sub.ots)))

;;; --------------------------------------------------------------------

   ((intersection-type-spec? super.ots)
    ;;"<exact-integer>" is not a "<string>" and it is an ancestor of "<fixnum>",
    ;;so:
    ;;
    ;;   (type-signature-matching ((and (not <fixnum>) (not <string>)))
    ;;                            (<exact-integer>)
    ;;   => possible-match
    ;;
    (for-all (lambda (super-component.ots)
	       (%matching-or-compatible? super-component.ots sub.ots))
      (intersection-type-spec.component-ots* super.ots)))

;;; --------------------------------------------------------------------

   ((complement-type-spec? super.ots)
    (cond ((complement-type-spec? sub.ots)
	   #f)
	  (else
	   (let ((super-item.ots (complement-type-spec.item-ots super.ots)))
	     (cond
	      ;; (type-signature-matching ((not (ancestor-of &condition)))
	      ;;                          (<condition>))
	      ;; => no-match
	      ((ancestor-of-type-spec? super-item.ots)
	       #f)
	      (($object-type-spec=? super-item.ots sub.ots)
	       #f)
	      (else
	       ;; (type-signature-matching ((not <fixnum>))
	       ;;                          (<exact-integer>))
	       ;; => possible-match
	       (object-type-spec.matching-super-and-sub? sub.ots super-item.ots)))))))

   ((complement-type-spec? sub.ots)
    #f)

;;; --------------------------------------------------------------------
;;; lists

   ((and (list-of-type-spec? super.ots)
	 (list-type-spec?    sub.ots))
    (let ((super-item.ots (list-of-type-spec.item-ots super.ots)))
      (for-all (lambda (sub-item.ots)
		 (object-type-spec.compatible-super-and-sub? super-item.ots sub-item.ots))
	(list-type-spec.item-ots* sub.ots))))

   ((list-of-type-spec? super.ots)
    (or (<nelist>-ots? sub.ots)
	(object-type-spec.matching-super-and-sub? sub.ots super.ots)))

   ((list-type-spec? super.ots)
    (or (<list>-ots?   sub.ots)
	(object-type-spec.matching-super-and-sub? sub.ots super.ots)))

   ((<nelist>-ots? super.ots)
    ;; (type-annotation-matching <nelist> (list-of <top>)) => possible-match
    (or (list-of-type-spec? sub.ots)
	(<pair>-ots?        sub.ots)
	(object-type-spec.matching-super-and-sub? sub.ots super.ots)))

   ((and (%compare-super-with-sub-and-its-parents (<list>-ots) super.ots)
	 (%compare-super-with-sub-and-its-parents (<pair>-ots) sub.ots))
    (cond ((pair-type-spec? sub.ots)
	   (object-type-spec.compatible-super-and-sub? (<list>-ots) (pair-type-spec.cdr-ots sub.ots)))
	  ((pair-of-type-spec? sub.ots)
	   (object-type-spec.compatible-super-and-sub? (<list>-ots) (pair-of-type-spec.item-ots sub.ots)))
	  (else #t)))

;;; --------------------------------------------------------------------

   ((and (vector-of-type-spec? super.ots)
	 (vector-type-spec? sub.ots))
    (let ((super-item.ots (vector-of-type-spec.item-ots super.ots)))
      (for-all (lambda (sub-item.ots)
		 (object-type-spec.compatible-super-and-sub? super-item.ots sub-item.ots))
	(vector-type-spec.item-ots* sub.ots))))

;;; --------------------------------------------------------------------
;;; closure type specifications

   ((closure-type-spec? super.ots)
    (cond ((closure-type-spec? sub.ots)
	   ;;For every  super-type signature there  must be a  matching sub-type
	   ;;signature; so that the sub can  be used everywhere the super can be
	   ;;used.  It does not matter if  the sub has clauses with non-matching
	   ;;signatures.
	   (for-all (lambda (super.clause-signature)
		      (exists (lambda (sub.clause-signature)
				(and (type-signature.compatible-super-and-sub? (lambda-signature.argvals super.clause-signature)
									       (lambda-signature.argvals sub.clause-signature))
				     (type-signature.compatible-super-and-sub? (lambda-signature.retvals super.clause-signature)
									       (lambda-signature.retvals sub.clause-signature))))
			(case-lambda-signature.clause-signature* (closure-type-spec.signature sub.ots))))
	     (case-lambda-signature.clause-signature* (closure-type-spec.signature super.ots))))
	  (else
	   (or (<procedure>-ots? sub.ots)
	       (<top>-ots?       sub.ots)))))

;;; --------------------------------------------------------------------

   ((object-type-spec.matching-super-and-sub? sub.ots super.ots))

   (else #f)))


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

	((<no-return>-ots? ots1)
	 ots2)

	((<no-return>-ots? ots2)
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
	     (make-list-type-spec (map object-type-spec.common-ancestor
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
  (cond ((or (eq? ots1 ots2)
	     (and (or (and (scheme-type-spec? ots1) (scheme-type-spec? ots2))
		      (and (record-type-spec? ots1) (record-type-spec? ots2))
		      (and (struct-type-spec? ots1) (struct-type-spec? ots2)))
		  (free-identifier=? (object-type-spec.name ots1)
				     (object-type-spec.name ots2)))))
;;;
	((closure-type-spec? ots1)
	 (and (closure-type-spec? ots2)
	      ($closure-type-spec=? ots1 ots2)))
;;;
	((list-of-type-spec? ots1)
	 (and (list-of-type-spec? ots2)
	      ($list-of-type-spec=? ots1 ots2)))

	((list-type-spec? ots1)
	 (and (list-type-spec? ots2)
	      ($list-type-spec=? ots1 ots2)))
;;;
	((vector-of-type-spec? ots1)
	 (and (vector-of-type-spec? ots2)
	      ($vector-of-type-spec=? ots1 ots2)))

	((vector-type-spec? ots1)
	 (and (vector-type-spec? ots2)
	      ($vector-type-spec=? ots1 ots2)))
;;;
	((pair-of-type-spec? ots1)
	 (and (pair-of-type-spec? ots2)
	      ($pair-of-type-spec=? ots1 ots2)))

	((pair-type-spec? ots1)
	 (and (pair-type-spec? ots2)
	      ($pair-type-spec=? ots1 ots2)))
;;;
	((compound-condition-type-spec? ots1)
	 (and (compound-condition-type-spec? ots2)
	      ($compound-condition-type-spec=? ots1 ots2)))

	((alist-type-spec? ots1)
	 (and (alist-type-spec? ots2)
	      ($alist-type-spec=? ots1 ots2)))
;;;
	((union-type-spec? ots1)
	 (and (union-type-spec? ots2)
	      ($union-type-spec=? ots1 ots2)))

	((intersection-type-spec? ots1)
	 (and (intersection-type-spec? ots2)
	      ($intersection-type-spec=? ots1 ots2)))

	((complement-type-spec? ots1)
	 (and (complement-type-spec? ots2)
	      ($complement-type-spec=? ots1 ots2)))

	((ancestor-of-type-spec? ots1)
	 (and (ancestor-of-type-spec? ots2)
	      ($ancestor-of-type-spec=? ots1 ots2)))
;;;
	(else #f)))

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
  (let ((component1*.ots (compound-condition-type-spec.component-ots* ots1))
	(component2*.ots (compound-condition-type-spec.component-ots* ots2)))
    (and (= (length component1*.ots)
	    (length component2*.ots))
	 (for-all (lambda (component1.ots)
		    (exists (lambda (component2.ots)
			      ($object-type-spec=? component1.ots component2.ots))
		      component2*.ots))
	   component1*.ots)
	 (for-all (lambda (component2.ots)
		    (exists (lambda (component1.ots)
			      ($object-type-spec=? component1.ots component2.ots))
		      component1*.ots))
	   component2*.ots))))

;;; --------------------------------------------------------------------

(define* (alist-type-spec=? {ots1 alist-type-spec?} {ots2 alist-type-spec?})
  ($alist-type-spec=? ots1 ots2))

(define ($alist-type-spec=? ots1 ots2)
  (and ($object-type-spec=? (alist-type-spec.key-ots ots1)
			    (alist-type-spec.key-ots ots2))
       ($object-type-spec=? (alist-type-spec.value-ots ots1)
			    (alist-type-spec.value-ots ots2))))

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
  (let ((component-ots1* (union-type-spec.component-ots* ots1))
	(component-ots2* (union-type-spec.component-ots* ots2)))
    (and (for-all (lambda (component-ots1)
		    (exists (lambda (component-ots2)
			      ($object-type-spec=? component-ots1 component-ots2))
		      component-ots2*))
	   component-ots1*)
	 (for-all (lambda (component-ots2)
		    (exists (lambda (component-ots1)
			      ($object-type-spec=? component-ots1 component-ots2))
		      component-ots1*))
	   component-ots2*))))

;;; --------------------------------------------------------------------

(define* (intersection-type-spec=? {ots1 intersection-type-spec?} {ots2 intersection-type-spec?})
  ($intersection-type-spec=? ots1 ots2))

(define ($intersection-type-spec=? ots1 ots2)
  (let ((component-ots1* (intersection-type-spec.component-ots* ots1))
	(component-ots2* (intersection-type-spec.component-ots* ots2)))
    (and (for-all (lambda (component-ots1)
		    (exists (lambda (component-ots2)
			      ($object-type-spec=? component-ots1 component-ots2))
		      component-ots2*))
	   component-ots1*)
	 (for-all (lambda (component-ots2)
		    (exists (lambda (component-ots1)
			      ($object-type-spec=? component-ots1 component-ots2))
		      component-ots1*))
	   component-ots2*))))

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
(define-record-type (<scheme-type-spec> make-scheme-type-spec scheme-type-spec?)
  (nongenerative *3*vicare:expander:<scheme-type-spec>)
  (parent <object-type-spec>)
  (fields
    (immutable type-descriptor-id	scheme-type-spec.type-descriptor-id)
		;Syntactic     identifier     bound     to     an     instance     of
		;"<scheme-type-descriptor>".
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-scheme-type-spec {name identifier?}
				      {parent.ots (or not scheme-type-spec?)}
				      constructor.stx type-predicate.stx
				      equality-predicate.id comparison-procedure.id hash-function.id
				      type-descriptor.id methods-table)
	(let ((destructor.stx	#f)
	      (accessors-table	'())
	      (mutators-table	'()))
	  ((make-object-type-spec name parent.ots scheme-type-spec.type-annotation-maker
				  constructor.stx destructor.stx type-predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   type-descriptor.id)))
      make-scheme-type-spec))
  (custom-printer
    (lambda (S port sub-printer)
      (display "#[scheme-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))
  #| end of DEFINE-RECORD-TYPE |# )

(define <scheme-type-spec>-rtd
  (record-type-descriptor <scheme-type-spec>))

(define <scheme-type-spec>-rcd
  (record-constructor-descriptor <scheme-type-spec>))

;;; --------------------------------------------------------------------

(define (scheme-type-spec.type-annotation-maker ots)
  (object-type-spec.name ots))


;;;; Vicare's struct-type specification
;;
;;This record-type is  used as syntactic binding descriptor's  value for struct-types
;;defined by DEFINE-STRUCT.
;;
;;Lexical variables  bound to  instances of  this type  should be  called STS  (as in
;;"Struct-Type Spec") or STRUCT-OTS.
;;
(define-record-type (<struct-type-spec> make-struct-type-spec struct-type-spec?)
  (nongenerative *3*vicare:expander:<struct-type-spec>)
  (parent <object-type-spec>)
  (fields
    (immutable std			struct-type-spec.std)
		;The struct-type descriptor object.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-struct-type-spec name std
				      constructor.id predicate.id
				      safe-accessors-table safe-mutators-table methods-table)
	(let ((parent.ots		(<struct>-ots))
	      (destructor.stx		(bless `(internal-applicable-struct-type-destructor ,std)))
	      (equality-predicate.id	#f)
	      (comparison-procedure.id	#f)
	      (hash-function.id		#f))
	  ((make-object-type-spec name parent.ots struct-type-spec.type-annotation-maker
				  constructor.id destructor.stx predicate.id
				  equality-predicate.id comparison-procedure.id hash-function.id
				  safe-accessors-table safe-mutators-table methods-table)
	   std)))
      make-struct-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[struct-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-STRUCT-TYPE |# )

(define <struct-type-spec>-rtd
  (record-type-descriptor <struct-type-spec>))

(define <struct-type-spec>-rcd
  (record-constructor-descriptor <struct-type-spec>))

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
(define-record-type (<record-type-spec> make-record-type-spec record-type-spec?)
  (nongenerative *3*vicare:expander:<record-type-spec>)
  (parent <object-type-spec>)
  (fields
    (immutable rtd-id			record-type-spec.rtd-id)
		;The syntactic identifier bound to the record-type descriptor.
    (immutable rcd-id			record-type-spec.rcd-id)
		;The syntactic identifier bound to the record-constructor descriptor.
    (immutable super-protocol-id		record-type-spec.super-protocol-id)
		;False  if  this  record-type has  no  super-type  record-constructor
		;descriptor;  otherwise   the  syntactic  identifier  to   which  the
		;super-RCD is bound.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-record-type-spec {type-name identifier?}
				      rtd-id rcd-id super-protocol-id parent-name.id
				      constructor.stx destructor.stx predicate.stx
				      equality-predicate.id comparison-procedure.id hash-function.id
				      safe-accessors-table safe-mutators-table methods-table)
	(let ((parent-name.ots	(cond ((<record>-type-id? parent-name.id)
				       (<record>-ots))
				      ((<condition>-type-id? parent-name.id)
				       (<condition>-ots))
				      (else
				       (with-exception-handler
					   (lambda (E)
					     (raise (condition E (make-who-condition __who__))))
					 (lambda ()
					   (id->record-type-spec parent-name.id))))))
	      (constructor.stx	(or constructor.stx
				    (bless `(record-constructor ,rcd-id))))
	      (pred		(or predicate.stx make-record-type-predicate)))
	  ((make-object-type-spec type-name parent-name.ots record-type-spec.type-annotation-maker
				  constructor.stx destructor.stx pred
				  equality-predicate.id comparison-procedure.id hash-function.id
				  safe-accessors-table safe-mutators-table methods-table)
	   rtd-id rcd-id super-protocol-id)))
      make-record-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[record-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define (make-record-type-predicate ots)
  (let ((rtd-id  (record-type-spec.rtd-id ots))
	(arg.sym (make-syntactic-identifier-for-temporary-variable)))
    (bless
     `(lambda/std (,arg.sym)
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

(define <record-type-spec>-rtd
  (record-type-descriptor <record-type-spec>))

(define <record-type-spec>-rcd
  (record-constructor-descriptor <record-type-spec>))

(define (record-type-spec.type-annotation-maker ots)
  (object-type-spec.name ots))


;;;; compound condition object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<compound-condition>" representing compound condition objects of a known type.
;;
(define-record-type (<compound-condition-type-spec> make-compound-condition-type-spec compound-condition-type-spec?)
  (nongenerative *3*vicare:expander:<compound-condition-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable component-ots*		compound-condition-type-spec.component-ots*)
		;A list of instances of  "<record-type-spec>" describing the types of
		;component condition objects.
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
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots compound-condition-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   component-type*.ots)))

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

(define <compound-condition-type-spec>-rtd
  (record-type-descriptor <compound-condition-type-spec>))

(define <compound-condition-type-spec>-rcd
  (record-constructor-descriptor <compound-condition-type-spec>))

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
     `(lambda (,obj.sym)
	(and (compound-condition? ,obj.sym)
	     (for-all (lambda (,pred.sym)
			(,pred.sym ,obj.sym))
	       (list ,@component-pred*.stx)))))))


;;;; union object spec
;;
;;This record-type  is used as  syntactic binding  descriptor's value for  type union
;;types.
;;
(define-record-type (<union-type-spec> make-union-type-spec union-type-spec?)
  (nongenerative *3*vicare:expander:<union-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable component-ots*		union-type-spec.component-ots*)
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
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots union-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   component-type*.ots (void))))

      make-union-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[union-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define <union-type-spec>-rtd
  (record-type-descriptor <union-type-spec>))

(define <union-type-spec>-rcd
  (record-constructor-descriptor <union-type-spec>))

;;; --------------------------------------------------------------------

(define (union-type-spec.type-annotation-maker ots)
  (cons (core-prim-id 'or)
	(map object-type-spec.name (union-type-spec.component-ots* ots))))

(define (make-union-type-predicate ots)
  (let* ((component-type*.ots (union-type-spec.component-ots* ots))
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
       `(lambda (,obj.sym)
	  (exists (lambda (,pred.sym)
		    (,pred.sym ,obj.sym))
	    (list ,@component-pred*.stx)))))))

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
	     ;;If there are "<no-return>" components: we filter them out.
	     (let* ((component-type*.ots (remp <no-return>-ots? component-type*.ots))
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
			 (append (union-type-spec.component-ots* component.ots)
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
	    (length (union-type-spec.component-ots* union.ots))
	  (union-type-spec.memoised-length-set! union.ots len))
      mem)))

(define (union-type-spec.common-ancestor ots)
  (let ((component.ots* (union-type-spec.component-ots* ots)))
    (fold-left object-type-spec.common-ancestor
      (car component.ots*)
      (cdr component.ots*))))


;;;; intersection object spec
;;
;;This record-type is  used as syntactic binding descriptor's  value for intersection
;;types.
;;
(define-record-type (<intersection-type-spec> make-intersection-type-spec intersection-type-spec?)
  (nongenerative *3*vicare:expander:<intersection-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable component-ots*		intersection-type-spec.component-ots*)
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
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots intersection-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   component-type*.ots (void))))

      make-intersection-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[intersection-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define <intersection-type-spec>-rtd
  (record-type-descriptor <intersection-type-spec>))

(define <intersection-type-spec>-rcd
  (record-constructor-descriptor <intersection-type-spec>))

;;; --------------------------------------------------------------------

(define (intersection-type-spec.type-annotation-maker ots)
  (cons (core-prim-id 'and)
	(map object-type-spec.name (intersection-type-spec.component-ots* ots))))

(define (make-intersection-type-predicate ots)
  (let* ((component-type*.ots (intersection-type-spec.component-ots* ots))
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
       `(lambda (,obj.sym)
	  (for-all (lambda (,pred.sym)
		     (,pred.sym ,obj.sym))
	    (list ,@component-pred*.stx)))))))

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
	     ;;If there are "<no-return>" components: we filter them out.
	     (let* ((component-type*.ots	(remp <no-return>-ots? component-type*.ots))
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
			 (append (intersection-type-spec.component-ots* component.ots)
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
	    (length (intersection-type-spec.component-ots* intersection.ots))
	  (intersection-type-spec.memoised-length-set! intersection.ots len))
      mem)))

(define (intersection-type-spec.common-ancestor ots)
  (let ((component.ots* (intersection-type-spec.component-ots* ots)))
    (fold-left object-type-spec.common-ancestor
      (car component.ots*)
      (cdr component.ots*))))


;;;; complement object spec
;;
;;This record-type  is used  as syntactic binding  descriptor's value  for complement
;;types.
;;
(define-record-type (<complement-type-spec> make-complement-type-spec complement-type-spec?)
  (nongenerative *3*vicare:expander:<complement-type-spec>)
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
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots complement-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   item-type.ots)))

      make-complement-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[complement-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define <complement-type-spec>-rtd
  (record-type-descriptor <complement-type-spec>))

(define <complement-type-spec>-rcd
  (record-constructor-descriptor <complement-type-spec>))

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
       `(lambda (,obj.sym)
	  (not (,item-pred.stx ,obj.sym)))))))


;;;; ancestor-of object spec
;;
;;This record-type is  used as syntactic binding descriptor's  value for ANCESTOR-OF
;;types.
;;
(define-record-type (<ancestor-of-type-spec> make-ancestor-of-type-spec ancestor-of-type-spec?)
  (nongenerative *3*vicare:expander:<ancestor-of-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots			ancestor-of-type-spec.item-ots)
    (immutable component-ots*		ancestor-of-type-spec.component-ots*)
		;A list of instances  of "<object-type-spec>" describing the optional
		;types.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-object-type-spec)
      (define* (make-ancestor-of-type-spec {type.ots object-type-spec?})
	(let* ((name			make-ancestor-of-type-name)
	       (ancestor*.ots		(object-type-spec.ancestors-ots* type.ots))
	       (parent.ots		(<top>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-ancestor-of-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots ancestor-of-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   type.ots ancestor*.ots)))

      make-ancestor-of-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[ancestor-of-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define <ancestor-of-type-spec>-rtd
  (record-type-descriptor <ancestor-of-type-spec>))

(define <ancestor-of-type-spec>-rcd
  (record-constructor-descriptor <ancestor-of-type-spec>))

;;; --------------------------------------------------------------------

(define (make-ancestor-of-type-name ots)
  (list (core-prim-id 'ancestor-of)
	(object-type-spec.name (ancestor-of-type-spec.item-ots ots))))

(define (ancestor-of-type-spec.type-annotation-maker ots)
  (list (core-prim-id 'ancestor-of)
	(object-type-spec.type-annotation (ancestor-of-type-spec.item-ots ots))))

(define (make-ancestor-of-predicate ots)
  (let* ((ancestor*.ots		(ancestor-of-type-spec.component-ots* ots))
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
       `(lambda (,obj.sym)
	  (exists (lambda (,pred.sym)
		    (,pred.sym ,obj.sym))
	    (list ,@pred*.stx)))))))


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
(define-record-type (<closure-type-spec> make-closure-type-spec closure-type-spec?)
  (nongenerative *3*vicare:expander:<closure-type-spec>)
  (parent <object-type-spec>)

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
	      (accessors-table		'())
	      (mutators-table		'())
	      (methods-table		'()))
	  ((make-object-type-spec name parent.ots closure-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   signature)))

      make-closure-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[closure-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define <closure-type-spec>-rtd
  (record-type-descriptor <closure-type-spec>))

(define <closure-type-spec>-rcd
  (record-constructor-descriptor <closure-type-spec>))

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


;;;; heterogeneous pair object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<pair>" representing pair of objects holding items of a known type.
;;
(define-record-type (<pair-type-spec> make-pair-type-spec pair-type-spec?)
  (nongenerative *3*vicare:expander:<pair-type-spec>)
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
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name parent.ots pair-type-spec.type-annotation-maker
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
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

(define <pair-type-spec>-rtd
  (record-type-descriptor <pair-type-spec>))

(define <pair-type-spec>-rcd
  (record-constructor-descriptor <pair-type-spec>))

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
     `(lambda (,obj.sym)
	(and (pair? ,obj.sym)
	     (,car-pred.stx (car ,obj.sym))
	     (,cdr-pred.stx (cdr ,obj.sym)))))))

;;; --------------------------------------------------------------------

(define* (pair-type-spec.homogeneous? {pair.ots pair-type-spec?})
  (let ((mem (pair-type-spec.memoised-homogeneous? pair.ots)))
    (if (boolean? mem)
	mem
      (receive-and-return (bool)
	  (object-type-spec=? (pair-type-spec.car-ots pair.ots)
			      (pair-type-spec.cdr-ots pair.ots))
	(pair-type-spec.memoised-homogeneous?-set! pair.ots bool)))))


;;;; homogeneous pair object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<pair>" representing pair of objects holding items of the same type.
;;
(define-record-type (<pair-of-type-spec> make-pair-of-type-spec pair-of-type-spec?)
  (nongenerative *3*vicare:expander:<pair-of-type-spec>)
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
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name parent.ots pair-of-type-spec.type-annotation-maker
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
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

(define <pair-of-type-spec>-rtd
  (record-type-descriptor <pair-of-type-spec>))

(define <pair-of-type-spec>-rcd
  (record-constructor-descriptor <pair-of-type-spec>))

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
     `(lambda (,obj.sym)
	(and (pair? ,obj.sym)
	     (,item-pred.stx (car ,obj.sym))
	     (,item-pred.stx (cdr ,obj.sym)))))))



;;;; heterogeneous list object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>"  representing non-empty  proper  list  objects holding  items  of a  known
;;heterogeneous type.
;;
(define-record-type (<list-type-spec> make-list-type-spec list-type-spec?)
  (nongenerative *3*vicare:expander:<list-type-spec>)
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
	       (accessors-table	'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots list-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
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

(define <list-type-spec>-rtd
  (record-type-descriptor <list-type-spec>))

(define <list-type-spec>-rcd
  (record-constructor-descriptor <list-type-spec>))

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
     `(lambda (,obj.sym)
	(and (list? ,obj.sym)
	     (for-all (lambda (,pred.sym ,item.sym)
			(,pred.sym ,item.sym))
	       (list ,@item-pred*.stx)
	       ,obj.sym))))))

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


;;;; homogeneous list object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>"  representing proper  list objects  holding items  of a  known homogeneous
;;type.
;;
(define-record-type (<list-of-type-spec> make-list-of-type-spec list-of-type-spec?)
  (nongenerative *3*vicare:expander:<list-of-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
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
	(({item-type.ots object-type-spec?} {name.stx list-of-name?})
	 ($make-list-of-type-spec item-type.ots name.stx)))

      (define ($make-list-of-type-spec item-type.ots name)
	(let* ((parent.ots		(<list>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-list-of-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id #f)
	       (hash-function.id	#f)
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots list-of-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
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

(define <list-of-type-spec>-rtd
  (record-type-descriptor <list-of-type-spec>))

(define <list-of-type-spec>-rcd
  (record-constructor-descriptor <list-of-type-spec>))

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
     `(letrec ((,pred.sym (lambda (,obj.sym)
			    (if (pair? ,obj.sym)
				(and (,item-pred.stx (car ,obj.sym))
				     (,pred.sym      (cdr ,obj.sym)))
			      (null? ,obj.sym)))))
	,pred.sym))))


;;;; heterogeneous vector object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<vector>"  representing  non-empty  vector  objects   holding  items  of  a  known
;;heterogeneous type.
;;
(define-record-type (<vector-type-spec> make-vector-type-spec vector-type-spec?)
  (nongenerative *3*vicare:expander:<vector-type-spec>)
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
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots vector-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
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

(define <vector-type-spec>-rtd
  (record-type-descriptor <vector-type-spec>))

(define <vector-type-spec>-rcd
  (record-constructor-descriptor <vector-type-spec>))

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
     `(lambda (,obj.sym)
	(and (vector? ,obj.sym)
	     (vector-for-all (lambda (,pred.sym ,item.sym)
			       (,pred.sym ,item.sym))
	       (vector ,@item-pred*.stx)
	       ,obj.sym))))))

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


;;;; homogeneous vector object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<vector>" representing vector objects holding items of a known type.
;;
(define-record-type (<vector-of-type-spec> make-vector-of-type-spec vector-of-type-spec?)
  (nongenerative *3*vicare:expander:<vector-of-type-spec>)
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
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots vector-of-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
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

(define <vector-of-type-spec>-rtd
  (record-type-descriptor <vector-of-type-spec>))

(define <vector-of-type-spec>-rcd
  (record-constructor-descriptor <vector-of-type-spec>))

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
     `(lambda (,obj.sym)
	(and (vector? ,obj.sym)
	     (vector-for-all ,item-pred.stx ,obj.sym))))))


;;;; hashtable object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<hashtable>" representing  hashtable objects  holding keys and  values of  a known
;;type.
;;
(define-record-type (<hashtable-type-spec> make-hashtable-type-spec hashtable-type-spec?)
  (nongenerative *3*vicare:expander:<hashtable-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable key-ots			hashtable-type-spec.key-ots)
		;An instance of "<object-type-spec>" describing the type of keys.
    (immutable value-ots		hashtable-type-spec.value-ots)
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
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots hashtable-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
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

(define <hashtable-type-spec>-rtd
  (record-type-descriptor <hashtable-type-spec>))

(define <hashtable-type-spec>-rcd
  (record-constructor-descriptor <hashtable-type-spec>))

;;; --------------------------------------------------------------------

(define (make-hashtable-type-name ots)
  (list (hashtable-id)
	(object-type-spec.name (hashtable-type-spec.key-ots   ots))
	(object-type-spec.name (hashtable-type-spec.value-ots ots))))

(define (hashtable-type-spec.type-annotation-maker ots)
  (list (hashtable-id)
	(object-type-spec.type-annotation (hashtable-type-spec.key-ots   ots))
	(object-type-spec.type-annotation (hashtable-type-spec.value-ots ots))))


;;;; alist object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>" representing alist objects holding keys and values of a known type.
;;
(define-record-type (<alist-type-spec> make-alist-type-spec alist-type-spec?)
  (nongenerative *3*vicare:expander:<alist-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable key-ots			alist-type-spec.key-ots)
		;An instance of "<object-type-spec>" describing the type of keys.
    (immutable value-ots		alist-type-spec.value-ots)
		;An instance of "<object-type-spec>" describing the type of values.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-alist-type-spec
	(({key-type.ots object-type-spec?} {value-type.ots object-type-spec?})
	 ($make-alist-type-spec key-type.ots value-type.ots make-alist-type-name))
	(({key-type.ots object-type-spec?} {value-type.ots object-type-spec?} {name.stx alist-name?})
	 ($make-alist-type-spec key-type.ots value-type.ots name.stx)))

      (define ($make-alist-type-spec key-type.ots value-type.ots name)
	(let* ((parent.ots		(<list>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		make-alist-type-predicate)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id #f)
	       (hash-function.id	#f)
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots alist-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   key-type.ots value-type.ots)))

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

      make-alist-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[alist-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define <alist-type-spec>-rtd
  (record-type-descriptor <alist-type-spec>))

(define <alist-type-spec>-rcd
  (record-constructor-descriptor <alist-type-spec>))

;;; --------------------------------------------------------------------

(define (make-alist-type-name ots)
  (list (alist-id)
	(object-type-spec.name (alist-type-spec.key-ots   ots))
	(object-type-spec.name (alist-type-spec.value-ots ots))))

(define (alist-type-spec.type-annotation-maker ots)
  (list (alist-id)
	(object-type-spec.type-annotation (alist-type-spec.key-ots   ots))
	(object-type-spec.type-annotation (alist-type-spec.value-ots ots))))

(define (make-alist-type-predicate ots)
  ;;FIXME The generated  predicate can be made more efficient  by iterating only once
  ;;through the spine of the list.  (Marco Maggi; Mon Apr 4, 2016)
  (let ((key-pred.stx	(object-type-spec.type-predicate-stx (alist-type-spec.key-ots   ots)))
	(value-pred.stx	(object-type-spec.type-predicate-stx (alist-type-spec.value-ots ots)))
	(obj.sym	(make-syntactic-identifier-for-temporary-variable "obj"))
	(P.sym		(make-syntactic-identifier-for-temporary-variable "P")))
    (bless
     `(lambda (,obj.sym)
	(and (list? ,obj.sym)
	     (for-all (lambda (,P.sym)
			(and (pair? ,P.sym)
			     (,key-pred.stx   (car ,P.sym))
			     (,value-pred.stx (cdr ,P.sym))))
	       ,obj.sym))))))


;;;; enumeration object spec
;;
;;This record-type  is used as  syntactic binding descriptor's value  for enumeration
;;types  defined  either  with  DEFINE-ENUMERATION   or  with  the  ENUMERATION  type
;;annotation.
;;
(define-record-type (<enumeration-type-spec> make-enumeration-type-spec enumeration-type-spec?)
  (nongenerative *3*vicare:expander:<enumeration-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable symbol*			enumeration-type-spec.symbol*)
		;An proper list of symbols representing the enumeration universe.
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
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name parent.ots enumeration-type-spec.type-annotation-maker
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   symbol*)))

      make-enumeration-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[enumeration-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define <enumeration-type-spec>-rtd
  (record-type-descriptor <enumeration-type-spec>))

(define <enumeration-type-spec>-rcd
  (record-constructor-descriptor <enumeration-type-spec>))

;;; --------------------------------------------------------------------

(define (enumeration-type-spec.type-annotation-maker ots)
  (cons (enumeration-id)
	(map make-syntactic-identifier-for-quoted-symbol (enumeration-type-spec.symbol* ots))))

(define (make-enumeration-type-predicate ots)
  (let ((symbol*	(enumeration-type-spec.symbol* ots))
	(obj.sym	(make-syntactic-identifier-for-temporary-variable "obj")))
    (bless
     `(lambda (,obj.sym)
	(and (symbol? ,obj.sym)
	     (memq ,obj.sym (quote ,symbol*))
	     #t)))))

;;; --------------------------------------------------------------------

(define* (enumeration-type-spec.member? {ots enumeration-type-spec?} {sym symbol?})
  (memq sym (enumeration-type-spec.symbol* ots)))


;;;; label spec
;;
;;This record-type  is used as syntactic  binding descriptor's value for  label types
;;defined with DEFINE-LABEL.
;;
(define-record-type (<label-type-spec> make-label-type-spec label-type-spec?)
  (nongenerative *3*vicare:expander:<label-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable with-type-predicate?	label-type-spec.with-type-predicate?)
		;Boolean, true if this label has  a custom type predicate.  This flag
		;is used when matching against other types.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-label-type-spec {type-name.id identifier?} parent.stx
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
	       (accessors-table		'())
	       (mutators-table		'()))
	  ((make-object-type-spec type-name.id parent.ots label-type-spec.type-annotation-maker
				  constructor.stx destructor.stx type-predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   with-type-predicate?)))

      make-label-type-spec))
  #| end of DEFINE-RECORD-TYPE |# )

(define <label-type-spec>-rtd
  (record-type-descriptor <label-type-spec>))

(define <label-type-spec>-rcd
  (record-constructor-descriptor <label-type-spec>))

(define (label-type-spec.type-annotation-maker ots)
  (object-type-spec.name ots))


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
   (syntax-match stx (pair list vector pair-of list-of nelist-of vector-of
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
   (syntax-match annotation.stx (pair list vector pair-of list-of nelist-of vector-of
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
	(if (<void>-ots? item.ots)
	    (<void>-ots)
	  (make-complement-type-spec item.ots))))

     ((parent-of ?type)
      (or (object-type-spec.parent-ots (type-annotation->object-type-spec ?type lexenv))
	  (syntax-violation __who__ "type annotation has no parent" annotation.stx ?type)))

     ((ancestor-of ?type)
      ;;If ?TYPE has no ancestors: its ancestors list is null.
      (make-ancestor-of-type-spec (type-annotation->object-type-spec ?type lexenv)))

     ((type-predicate ?type)
      (type-annotation->object-type-spec (bless
					  ;;FIXME  Can  we  do better  in  this  type
					  ;;annotation?  Notice that  when the ?TYPE,
					  ;;or it  hierarchy, contains  a label  or a
					  ;;union  things get  messy.  (Marco  Maggi;
					  ;;Fri Apr 29, 2016)
					  `(case-lambda
					     ((,?type)	=> (<true>))
					     ((<top>)	=> (<boolean>))))
					 lexenv))

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
