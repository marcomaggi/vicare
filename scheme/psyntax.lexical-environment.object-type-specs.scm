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
;;	      ------------> <hashtable-type-spec>
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
;;       ---> (list-of ?type) ---> <null>
;;                  |
;;                   ------------> (list ?type0 ?type ...)
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
;;       ---> (vector-of ?type) ---> <empty-vector>
;;                  |
;;                   --------------> (vector ?type0 ?type ...)
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
	 object-type-spec.name				object-type-spec.parent-ots
	 object-type-spec.constructor-stx		object-type-spec.destructor-stx
	 object-type-spec.type-predicate-stx
	 object-type-spec.safe-accessor-stx		object-type-spec.safe-mutator-stx
	 object-type-spec.equality-predicate-id		object-type-spec.comparison-procedure-id
	 object-type-spec.hash-function-id		object-type-spec.applicable-hash-function-id
	 object-type-spec.applicable-method-stx
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
	 closure-type-spec.signature

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
	 list-type-spec.item-ots*
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

	 ;;;

	 <union-type-spec>
	 <union-type-spec>-rtd				<union-type-spec>-rcd
	 make-union-type-spec				union-type-spec?
	 union-type-spec.component-ots*

	 <intersection-type-spec>
	 <intersection-type-spec>-rtd			<intersection-type-spec>-rcd
	 make-intersection-type-spec			intersection-type-spec?
	 intersection-type-spec.component-ots*

	 <complement-type-spec>
	 <complement-type-spec>-rtd			<complement-type-spec>-rcd
	 make-complement-type-spec			complement-type-spec?
	 complement-type-spec.item-ots

	 ;;;

	 syntax-object.type-annotation?
	 type-annotation->object-type-spec		tail-type-annotation->object-type-spec

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
  (nongenerative vicare:expander:<object-type-spec>)
  (fields
    (immutable name			object-type-spec.name)
		;A syntax  object representing the  "name" of this  object-type.  For
		;some types it is the actual type identifier, for example: "<fixnum>,
		;"<string>".  For  other types it  is a syntax object  like "(list-of
		;<fixnum>)".

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

    (immutable type-predicate-stx	object-type-spec.type-predicate-stx)
		;False  or a  syntax object  representing a  Scheme expression  that,
		;expanded and evaluated  at run-time, returns a  type predicate.  The
		;predicate is meant to be used as:
		;
		;   (?predicate ?object)
		;
		;and called explicitly with the IS-A? syntax.
		;
		;The type  predicate can be a  syntax or core operation  or a closure
		;object like "vector?" or the predicate of R6RS records.

    (immutable equality-predicate-id	object-type-spec.equality-predicate-id)
		;False or a syntactic identifier  bound to the equality predicate for
		;this type.

    (immutable comparison-procedure-id	object-type-spec.comparison-procedure-id)
		;False or  a syntactic identifier  bound to the  comparison procedure
		;for this type.

    (immutable hash-function-id		object-type-spec.hash-function-id)
		;False or a syntactic identifier bound  to the hash function for this
		;type.

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
				      constructor-stx destructor-stx type-predicate-stx
				      equality-predicate.id comparison-procedure.id hash-function.id
				      safe-accessors-table safe-mutators-table methods-table)
	(make-record name parent.ots
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

(define (list-of-object-type-spec? obj)
  (if (pair? obj)
      (and (object-type-spec? (car obj))
	   (list-of-object-type-spec? (cdr obj)))
    (null? obj)))

(define (not-empty-list-of-object-type-spec? obj)
  (and (pair? obj)
       (list-of-object-type-spec? obj)))

(define* (object-type-spec.applicable-hash-function-id {ots object-type-spec?})
  (cond ((object-type-spec.hash-function-id ots))
	((object-type-spec.parent-ots ots)
	 => object-type-spec.applicable-hash-function-id)
	(else #f)))

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
  (cond ((or (eq? super.ots sub.ots)
	     (and (or (and (scheme-type-spec? super.ots) (scheme-type-spec? sub.ots))
		      (and (record-type-spec? super.ots) (record-type-spec? sub.ots))
		      (and (struct-type-spec? super.ots) (struct-type-spec? sub.ots)))
		  (free-identifier=? (object-type-spec.name super.ots)
				     (object-type-spec.name sub.ots)))))

	((<top>-ots? super.ots)
	 ;;Fast track: "<top>" is the super-type of all the types.
	 #t)

;;; --------------------------------------------------------------------
;;; We really want to do the unions first.

	((union-type-spec? super.ots)
	 (cond ((union-type-spec? sub.ots)
		;; (type-super-and-sub? (union <number> <vector>)
		;;                      (union <fixnum> (vector-of <string>)))
		;; => #t
		(exists (lambda (component-super.ots)
			  (exists (lambda (component-sub.ots)
				    (object-type-spec.matching-super-and-sub? component-super.ots component-sub.ots))
			    (union-type-spec.component-ots* sub.ots)))
		  (union-type-spec.component-ots* super.ots)))
	       (else
		;; (type-super-and-sub? (union <number> <string>) <string>) => #t
		;; (type-super-and-sub? (union <number> <string>) <fixnum>) => #t
		(exists (lambda (component-super.ots)
			  (object-type-spec.matching-super-and-sub? component-super.ots sub.ots))
		  (union-type-spec.component-ots* super.ots)))))

	((union-type-spec? sub.ots)
	 (for-all (lambda (component-sub.ots)
		    (object-type-spec.matching-super-and-sub? super.ots component-sub.ots))
	   (union-type-spec.component-ots* sub.ots)))

;;; --------------------------------------------------------------------
;;; We really want to do the intersections first.

	((intersection-type-spec? super.ots)
	 (cond ((intersection-type-spec? sub.ots)
		;; (type-super-and-sub? (intersection <number> <positive>)
		;;                      (intersection <positive-fixnum>
		;;                                    <positive-ratnum>))
		;; => #t
		(for-all (lambda (component-super.ots)
			   (for-all (lambda (component-sub.ots)
				      (object-type-spec.matching-super-and-sub? component-super.ots component-sub.ots))
			     (intersection-type-spec.component-ots* sub.ots)))
		  (intersection-type-spec.component-ots* super.ots)))
	       (else
		;; (type-super-and-sub? (intersection <exact> <positive>) <positive-fixnum>) => #t
		(for-all (lambda (component-super.ots)
			   (object-type-spec.matching-super-and-sub? component-super.ots sub.ots))
		  (intersection-type-spec.component-ots* super.ots)))))

	((intersection-type-spec? sub.ots)
	 (for-all (lambda (component-sub.ots)
		    (object-type-spec.matching-super-and-sub? super.ots component-sub.ots))
	   (intersection-type-spec.component-ots* sub.ots)))

;;; --------------------------------------------------------------------
;;; We really want to do the complements first.

	((complement-type-spec? super.ots)
	 (cond ((complement-type-spec? sub.ots)
		;;If something is not a "<number>", for sure it is not a "<fixnum>":
		;;
		;; (type-super-and-sub? (complement <fixnum>)
		;;                      (complement <number>)) => #t
		;;
		;; (type-super-and-sub? (complement <number>)
		;;                      (complement <string>)) => #f
		;;
		(let ((super-item.ots	(complement-type-spec.item-ots super.ots))
		      (sub-item.ots	(complement-type-spec.item-ots sub.ots)))
		  (object-type-spec.matching-super-and-sub? sub-item.ots super-item.ots)))
	       (else
		;; (type-super-and-sub? (complement <string>) <fixnum>) => #t
		(not (object-type-spec.matching-super-and-sub? (complement-type-spec.item-ots super.ots)
							       sub.ots)))))

	((complement-type-spec? sub.ots)
	 #f)

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
		;; (type-super-and-sub? (pair-of (union <number> <null>)) (list <fixnum>))
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
	 (or (<null>-ots?        sub.ots)
	     (list-of-type-spec? sub.ots)
	     (list-type-spec?    sub.ots)))

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
	       ;;                        (pair-of (union <fixnum> <null>)))
	       ;;   => #f
	       ;;
	       (else #f)))

;;; --------------------------------------------------------------------
;;; vector type specifications

	((<vector>-ots? super.ots)
	 (or (<empty-vector>-ots?  sub.ots)
	     (vector-of-type-spec? sub.ots)
	     (vector-type-spec?    sub.ots)))

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
		(for-all (lambda (sub.clause-signature)
			   (exists (lambda (super.clause-signature)
				     (and (type-signature.super-and-sub? (clambda-clause-signature.argvals super.clause-signature)
									 (clambda-clause-signature.argvals sub.clause-signature))
					  (type-signature.super-and-sub? (clambda-clause-signature.retvals super.clause-signature)
									 (clambda-clause-signature.retvals sub.clause-signature))))
			     (clambda-signature.clause-signature* (closure-type-spec.signature super.ots))))
		  (clambda-signature.clause-signature* (closure-type-spec.signature sub.ots))))
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
;;; left-overs for SUB.OTS

	((or (<list>-ots?        sub.ots)
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
	 (<list>-ots? super.ots))

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
  (cond ((and (list-of-type-spec? super.ots)
	      (list-type-spec? sub.ots))
	 (let ((super-item.ots (list-of-type-spec.item-ots super.ots)))
	   (for-all (lambda (sub-item.ots)
		      (object-type-spec.compatible-super-and-sub? super-item.ots sub-item.ots))
	     (list-type-spec.item-ots* sub.ots))))

	((and (%compare-super-with-sub-and-its-parents (<list>-ots) super.ots)
	      (%compare-super-with-sub-and-its-parents (<pair>-ots) sub.ots))
	 (cond ((pair-type-spec? sub.ots)
		(object-type-spec.compatible-super-and-sub? (<list>-ots) (pair-type-spec.cdr-ots sub.ots)))
	       ((pair-of-type-spec? sub.ots)
		(object-type-spec.compatible-super-and-sub? (<list>-ots) (pair-of-type-spec.item-ots sub.ots)))
	       (else #t)))

	((and (vector-of-type-spec? super.ots)
	      (vector-type-spec? sub.ots))
	 (let ((super-item.ots (vector-of-type-spec.item-ots super.ots)))
	   (for-all (lambda (sub-item.ots)
		      (object-type-spec.compatible-super-and-sub? super-item.ots sub-item.ots))
	     (vector-type-spec.item-ots* sub.ots))))

	((union-type-spec? sub.ots)
	 (exists (lambda (component-sub.ots)
		   (object-type-spec.matching-super-and-sub? super.ots component-sub.ots))
	   (union-type-spec.component-ots* sub.ots)))

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

	((or (<top>-ots? ots1)
	     (<top>-ots? ots2))
	 ots1)

	((<no-return>-ots? ots1)
	 ots2)

	((<no-return>-ots? ots2)
	 ots1)

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

	((list-of-type-spec? ots1)
	 (and (list-of-type-spec? ots2)
	      ($list-of-type-spec=? ots1 ots2)))

	((vector-of-type-spec? ots1)
	 (and (vector-of-type-spec? ots2)
	      ($vector-of-type-spec=? ots1 ots2)))

	((pair-of-type-spec? ots1)
	 (and (pair-of-type-spec? ots2)
	      ($pair-of-type-spec=? ots1 ots2)))

	((list-type-spec? ots1)
	 (and (list-type-spec? ots2)
	      ($list-type-spec=? ots1 ots2)))

	((vector-type-spec? ots1)
	 (and (vector-type-spec? ots2)
	      ($vector-type-spec=? ots1 ots2)))

	((pair-type-spec? ots1)
	 (and (pair-type-spec? ots2)
	      ($pair-type-spec=? ots1 ots2)))

	((compound-condition-type-spec? ots1)
	 (and (compound-condition-type-spec? ots2)
	      ($compound-condition-type-spec=? ots1 ots2)))

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
  (nongenerative vicare:expander:<scheme-type-spec>)
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
	  ((make-object-type-spec name parent.ots
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


;;;; Vicare's struct-type specification
;;
;;This record-type is  used as syntactic binding descriptor's  value for struct-types
;;defined by DEFINE-STRUCT.
;;
;;Lexical variables  bound to  instances of  this type  should be  called STS  (as in
;;"Struct-Type Spec") or STRUCT-OTS.
;;
(define-record-type (<struct-type-spec> make-struct-type-spec struct-type-spec?)
  (nongenerative vicare:expander:<struct-type-spec>)
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
	  ((make-object-type-spec name parent.ots
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


;;;; R6RS's record-type specification
;;
;;This  record-type  is  used  as  syntactic  binding  descriptor's  value  for  R6RS
;;record-types.
;;
;;Lexical  variables bound  to instances  of this  type and  its sub-types  should be
;;called RTS (as in "Record-Type Spec") or RECORD-OTS.
;;
(define-record-type (<record-type-spec> make-record-type-spec record-type-spec?)
  (nongenerative vicare:expander:<record-type-spec>)
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
	      (predicate.stx	(or predicate.stx
				    (let ((arg.sym (gensym)))
				      (bless
				       `(lambda/std (,arg.sym)
					  (record-and-rtd? ,arg.sym ,rtd-id)))))))
	  ((make-object-type-spec type-name parent-name.ots
				  constructor.stx destructor.stx predicate.stx
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


;;;; compound condition object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<compound-condition>" representing compound condition objects of a known type.
;;
(define-record-type (<compound-condition-type-spec> make-compound-condition-type-spec compound-condition-type-spec?)
  (nongenerative vicare:expander:<compound-condition-type-spec>)
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
	(let* ((component-type*.ots	(%collapse-component-specs component-type*.ots))
	       (name.stx		(cons (core-prim-id 'condition)
					      (map object-type-spec.name component-type*.ots)))
	       (parent.ots		(<compound-condition>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		(make-compound-condition-predicate
					 (map object-type-spec.type-predicate-stx component-type*.ots)))
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name.stx parent.ots
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   component-type*.ots)))

      (define (%collapse-component-specs component-type*.ots)
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

      (define (make-compound-condition-predicate component-pred*.stx)
	(let ((obj.sym	(gensym "obj"))
	      (pred.sym	(gensym "pred"))
	      (item.sym	(gensym "item")))
	  (bless
	   `(lambda (,obj.sym)
	      (and (compound-condition? ,obj.sym)
		   (for-all (lambda (,pred.sym)
			      (,pred.sym ,obj.sym))
		     (list ,@component-pred*.stx)))))))

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


;;;; union object spec
;;
;;This record-type  is used as  syntactic binding  descriptor's value for  type union
;;types.
;;
(define-record-type (<union-type-spec> make-union-type-spec union-type-spec?)
  (nongenerative vicare:expander:<union-type-spec>)
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
	(let* ((component-type*.ots	(%collapse-component-specs component-type*.ots))
	       (name.stx		(cons (core-prim-id 'or)
					      (map object-type-spec.name component-type*.ots)))
	       (parent.ots		(<top>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		(make-union-predicate (map object-type-spec.type-predicate-stx component-type*.ots)))
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name.stx parent.ots
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   component-type*.ots (void))))

      (define (%collapse-component-specs component-type*.ots)
	(object-type-specs-delete-duplicates
	 (fold-right (lambda (component.ots knil)
		       (cond ((union-type-spec? component.ots)
			      (append (union-type-spec.component-ots* component.ots)
				      knil))
			     (else
			      (cons component.ots knil))))
	   '() component-type*.ots)))

      (define (make-union-predicate component-pred*.stx)
	(let ((obj.sym	(gensym "obj"))
	      (pred.sym	(gensym "pred")))
	  (bless
	   `(lambda (,obj.sym)
	      (exists (lambda (,pred.sym)
			(,pred.sym ,obj.sym))
		(list ,@component-pred*.stx))))))

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
;;This  record-type  is  used  as  syntactic  binding  descriptor's  value  for  type
;;intersection types.
;;
(define-record-type (<intersection-type-spec> make-intersection-type-spec intersection-type-spec?)
  (nongenerative vicare:expander:<intersection-type-spec>)
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
	(let* ((component-type*.ots	(%collapse-component-specs component-type*.ots))
	       (name.stx		(cons (core-prim-id 'and)
					      (map object-type-spec.name component-type*.ots)))
	       (parent.ots		(<top>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		(make-intersection-predicate (map object-type-spec.type-predicate-stx component-type*.ots)))
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name.stx parent.ots
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   component-type*.ots (void))))

      (define (%collapse-component-specs component-type*.ots)
	(object-type-specs-delete-duplicates
	 (fold-right (lambda (component.ots knil)
		       (cond ((intersection-type-spec? component.ots)
			      (append (intersection-type-spec.component-ots* component.ots)
				      knil))
			     (else
			      (cons component.ots knil))))
	   '() component-type*.ots)))

      (define (make-intersection-predicate component-pred*.stx)
	(let ((obj.sym	(gensym "obj"))
	      (pred.sym	(gensym "pred")))
	  (bless
	   `(lambda (,obj.sym)
	      (for-all (lambda (,pred.sym)
			 (,pred.sym ,obj.sym))
		(list ,@component-pred*.stx))))))

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
;;This  record-type  is  used  as  syntactic  binding  descriptor's  value  for  type
;;complement types.
;;
(define-record-type (<complement-type-spec> make-complement-type-spec complement-type-spec?)
  (nongenerative vicare:expander:<complement-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots			complement-type-spec.item-ots)
		;An instance of "<object-type-spec>" describing the forbidden type.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-complement-type-spec {item-type.ots object-type-spec?})
	(let* ((name.stx		(cons (core-prim-id 'not) (object-type-spec.name item-type.ots)))
	       (parent.ots		(<top>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		(make-complement-predicate (object-type-spec.type-predicate-stx item-type.ots)))
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	  ((make-object-type-spec name.stx parent.ots
				  constructor.stx destructor.stx predicate.stx
				  equality-predicate.id comparison-procedure.id hash-function.id
				  accessors-table mutators-table methods-table)
	   item-type.ots)))

      (define (make-complement-predicate item-pred.stx)
	(let ((obj.sym	(gensym "obj")))
	  (bless
	   `(lambda (,obj.sym)
	      (not (,item-pred.stx ,obj.sym))))))

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
  (nongenerative vicare:expander:<closure-type-spec>)
  (parent <object-type-spec>)

  (fields
    (immutable signature		closure-type-spec.signature)
		;An instance of "<callable-signature>".
    #| end of FIELDS |# )

  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-closure-type-spec
	(({signature callable-signature?})
	 (make-closure-type-spec signature (%mkname signature)))
	(({signature callable-signature?} name.stx)
	 (let ((parent.ots		(<procedure>-ots))
	       (constructor.stx		#f)
	       (destructor.stx		#f)
	       (predicate.stx		#f)
	       (equality-predicate.id	#f)
	       (comparison-procedure.id	#f)
	       (hash-function.id	#f)
	       (accessors-table		'())
	       (mutators-table		'())
	       (methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    signature))))

      (define (%mkname callable-signature)
	(let* ((clause* (map (lambda (clause-signature)
			       (list (type-signature.syntax-object (clambda-clause-signature.argvals clause-signature))
				     (core-prim-id '=>)
				     (type-signature.syntax-object (clambda-clause-signature.retvals clause-signature))))
			  (clambda-signature.clause-signature* callable-signature))))
	  (if (list-of-single-item? clause*)
	      (cons (core-prim-id 'lambda) (car clause*))
	    (cons (core-prim-id 'case-lambda) clause*))))

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

;; (define-record-type (<closure-clause> make-closure-clause closure-clause?)
;;   (nongenerative)
;;   (fields
;;     signature
;; 		;An instance of "<type-signature>" representing the type signature of
;; 		;this clause.
;;     unsafe-variant
;; 		;False if this clause has  no unsafe variant.  Otherwise, a syntactic
;; 		;identifier bound to the unsafe
;;     #| end of fields |# )
;;   (protocol
;;     (lambda (make-record)
;;       (define (make-closure-clause signature unsafe-variant)
;; 	(make-record signature unsafe-variant))
;;       make-closure-clause))
;;   #| end of DEFINE-RECORD-TYPE |# )


;;;; heterogeneous pair object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<pair>" representing pair of objects holding items of a known type.
;;
(define-record-type (<pair-type-spec> make-pair-type-spec pair-type-spec?)
  (nongenerative vicare:expander:<pair-type-spec>)
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
	 (make-pair-type-spec car.ots cdr.ots
			      (list (core-prim-id 'pair)
				    (object-type-spec.name car.ots)
				    (object-type-spec.name cdr.ots))))
	(({car.ots object-type-spec?} {cdr.ots object-type-spec?} {name.stx pair-name?})
	 (let* ((parent.ots		(<pair>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-pair-predicate (object-type-spec.type-predicate-stx car.ots)
							     (object-type-spec.type-predicate-stx cdr.ots)))
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    car.ots cdr.ots (void)))))

      (define (pair-name? name.stx)
	(syntax-match name.stx (pair)
	  ((pair ?car-type ?cdr-type)
	   (and (syntax-object.type-annotation? ?car-type)
		(syntax-object.type-annotation? ?cdr-type))
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      (define (make-pair-predicate car-pred.stx cdr-pred.stx)
	(let ((obj.sym (gensym "obj")))
	  (bless
	   `(lambda (,obj.sym)
	      (and (pair? ,obj.sym)
		   (,car-pred.stx (car ,obj.sym))
		   (,cdr-pred.stx (cdr ,obj.sym)))))))

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
  (nongenerative vicare:expander:<pair-of-type-spec>)
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
	 (make-pair-of-type-spec item.ots
				 (list (core-prim-id 'pair-of)
				       (object-type-spec.name item.ots))))
	(({item.ots object-type-spec?} {name.stx pair-of-name?})
	 (let* ((parent.ots		(<pair>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-pair-of-predicate (object-type-spec.type-predicate-stx item.ots)))
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    item.ots))))

      (define (pair-of-name? name.stx)
	(syntax-match name.stx (pair-of)
	  ((pair-of ?item-type)
	   (syntax-object.type-annotation? ?item-type)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      (define (make-pair-of-predicate item-pred.stx)
	(let ((obj.sym (gensym "obj")))
	  (bless
	   `(lambda (,obj.sym)
	      (and (pair? ,obj.sym)
		   (,item-pred.stx (car ,obj.sym))
		   (,item-pred.stx (cdr ,obj.sym)))))))

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


;;;; heterogeneous list object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>"  representing non-empty  proper  list  objects holding  items  of a  known
;;heterogeneous type.
;;
(define-record-type (<list-type-spec> make-list-type-spec list-type-spec?)
  (nongenerative vicare:expander:<list-type-spec>)
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
	 (make-list-type-spec item-type*.ots (cons (core-prim-id 'list)
						   (map object-type-spec.name item-type*.ots))))
	(({item-type*.ots not-empty-list-of-object-type-spec?} {name.stx list-name?})
	 (let* ((parent.ots		(<list>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-list-predicate (map object-type-spec.type-predicate-stx item-type*.ots)))
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    item-type*.ots (void) (void)))))

      (define (list-name? name.stx)
	(syntax-match name.stx (list)
	  ((list ?item-type* ...)
	   (for-all syntax-object.type-annotation? ?item-type*)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      (define (make-list-predicate item-pred*.stx)
	(let ((obj.sym	(gensym "obj"))
	      (pred.sym	(gensym "pred"))
	      (item.sym	(gensym "item")))
	  (bless
	   `(lambda (,obj.sym)
	      (and (list? ,obj.sym)
		   (for-all (lambda (,pred.sym ,item.sym)
			      (,pred.sym ,item.sym))
		     (list ,@item-pred*.stx)
		     ,obj.sym))))))

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


;;;; homogeneous list object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>"  representing proper  list objects  holding items  of a  known homogeneous
;;type.
;;
(define-record-type (<list-of-type-spec> make-list-of-type-spec list-of-type-spec?)
  (nongenerative vicare:expander:<list-of-type-spec>)
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
	 (make-list-of-type-spec item-type.ots (list (list-of-id) (object-type-spec.name item-type.ots))))
	(({item-type.ots object-type-spec?} {name.stx list-of-name?})
	 (let* ((parent.ots		(<list>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-list-of-predicate (object-type-spec.type-predicate-stx item-type.ots)))
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    item-type.ots))))

      (define (list-of-name? name.stx)
	(syntax-match name.stx (list-of)
	  ((list-of ?item-type)
	   (syntax-object.type-annotation? ?item-type)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      (define (make-list-of-predicate item-pred.stx)
	(let ((obj.sym	(gensym "obj"))
	      (pred.sym	(gensym "pred")))
	  (bless
	   `(letrec ((,pred.sym (lambda (,obj.sym)
				  (if (pair? ,obj.sym)
				      (and (,item-pred.stx (car ,obj.sym))
					   (,pred.sym      (cdr ,obj.sym)))
				    (null? ,obj.sym)))))
	      ,pred.sym))))

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


;;;; heterogeneous vector object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<vector>"  representing  non-empty  vector  objects   holding  items  of  a  known
;;heterogeneous type.
;;
(define-record-type (<vector-type-spec> make-vector-type-spec vector-type-spec?)
  (nongenerative vicare:expander:<vector-type-spec>)
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
	 (make-vector-type-spec item-type*.ots (cons (core-prim-id 'vector)
						     (map object-type-spec.name item-type*.ots))))
	(({item-type*.ots not-empty-list-of-object-type-spec?} {name.stx vector-name?})
	 (let* ((parent.ots		(<vector>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-vector-predicate (map object-type-spec.type-predicate-stx item-type*.ots)))
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    item-type*.ots (void) (void)))))

      (define (vector-name? name.stx)
	(syntax-match name.stx (vector)
	  ((vector ?item-type* ...)
	   (for-all syntax-object.type-annotation? ?item-type*)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      (define (make-vector-predicate item-pred*.stx)
	(let ((obj.sym	(gensym "obj"))
	      (pred.sym	(gensym "pred"))
	      (item.sym	(gensym "item")))
	  (bless
	   `(lambda (,obj.sym)
	      (and (vector? ,obj.sym)
		   (vector-for-all (lambda (,pred.sym ,item.sym)
				     (,pred.sym ,item.sym))
		     (vector ,@item-pred*.stx)
		     ,obj.sym))))))

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
  (nongenerative vicare:expander:<vector-of-type-spec>)
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
	 (make-vector-of-type-spec item-type.ots (list (vector-of-id) (object-type-spec.name item-type.ots))))
	(({item-type.ots object-type-spec?} {name vector-of-name?})
	 (let* ((parent.ots		(<vector>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-vector-of-predicate (object-type-spec.type-predicate-stx item-type.ots)))
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    item-type.ots))))

      (define (vector-of-name? name.stx)
	(syntax-match name.stx (vector-of)
	  ((vector-of ?item-type)
	   (syntax-object.type-annotation? ?item-type)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      (define (make-vector-of-predicate item-pred.stx)
	(let ((obj.sym	(gensym "obj")))
	  (bless
	   `(lambda (,obj.sym)
	      (and (vector? ,obj.sym)
		   (vector-for-all ,item-pred.stx ,obj.sym))))))

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


;;;; hashtable object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<hashtable>" representing  hashtable objects  holding keys and  values of  a known
;;type.
;;
(define-record-type (<hashtable-type-spec> make-hashtable-type-spec hashtable-type-spec?)
  (nongenerative vicare:expander:<hashtable-type-spec>)
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
	 (make-hashtable-type-spec key-type.ots value-type.ots
				   (list (hashtable-id)
					 (object-type-spec.name key-type.ots)
					 (object-type-spec.name value-type.ots))))
	(({key-type.ots object-type-spec?} {value-type.ots object-type-spec?} {name.stx hashtable-name?})
	 (let* ((parent.ots		(<hashtable>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		#f)
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    key-type.ots value-type.ots))))

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


;;;; alist object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<list>" representing alist objects holding keys and values of a known type.
;;
(define-record-type (<alist-type-spec> make-alist-type-spec alist-type-spec?)
  (nongenerative vicare:expander:<alist-type-spec>)
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
	 (make-alist-type-spec key-type.ots value-type.ots
			       (list (alist-id)
				     (object-type-spec.name key-type.ots)
				     (object-type-spec.name value-type.ots))))
	(({key-type.ots object-type-spec?} {value-type.ots object-type-spec?} {name.stx alist-name?})
	 (let* ((parent.ots		(<list>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-alist-predicate (object-type-spec.type-predicate-stx key-type.ots)
							      (object-type-spec.type-predicate-stx value-type.ots)))
		(equality-predicate.id	#f)
		(comparison-procedure.id #f)
		(hash-function.id	#f)
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   equality-predicate.id comparison-procedure.id hash-function.id
				   accessors-table mutators-table methods-table)
	    key-type.ots value-type.ots))))

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

      (define (make-alist-predicate key-pred.stx value-pred.stx)
	;;FIXME The generated predicate can be  made more efficient by iterating only
	;;once through the spine of the list.  (Marco Maggi; Mon Apr 4, 2016)
	(let ((obj.sym	(gensym "obj"))
	      (P.sym	(gensym "P")))
	  (bless
	   `(lambda (,obj.sym)
	      (and (list? ,obj.sym)
		   (for-all (lambda (,P.sym)
			      (and (pair? ,P.sym)
				   (,key-pred.stx   (car ,P.sym))
				   (,value-pred.stx (cdr ,P.sym))))
		     ,obj.sym))))))

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
   (syntax-match stx (pair list vector pair-of list-of vector-of hashtable alist condition
			   or and not lambda case-lambda =>)
     (?type-id
      (and (identifier? ?type-id)
	   (try
	       (id->object-type-spec ?type-id lexenv)
	     (catch E
	       (&syntactic-identifier-resolution
		#f))))
      ?type-id)

     ((pair ?car-type ?cdr-type)
      (list (core-prim-id 'pair)
	    (recur ?car-type)
	    (recur ?cdr-type)))

     ((list ?item-type* ...)
      (cons (core-prim-id 'list)
	    (map recur ?item-type*)))

     ((vector ?item-type* ...)
      (cons (core-prim-id 'vector)
	    (map recur ?item-type*)))

     ((pair-of ?item-type)
      (list (core-prim-id 'pair-of)
	    (recur ?item-type)))

     ((list-of ?item-type)
      (list (core-prim-id 'list-of)
	    (recur ?item-type)))

     ((vector-of ?item-type)
      (list (core-prim-id 'vector-of)
	    (recur ?item-type)))

     ((hashtable ?key-type ?value-type)
      (list (hashtable-id)
	    (recur ?key-type)
	    (recur ?value-type)))

     ((alist ?key-type ?value-type)
      (list (alist-id)
	    (recur ?key-type)
	    (recur ?value-type)))

     ((lambda ?argtypes => ?rettypes)
      (and (make-type-signature ?argtypes)
	   (make-type-signature ?rettypes))
      (list (core-prim-id 'lambda) ?rettypes (core-prim-id '=>) ?argtypes))

     ((case-lambda
	(?argtypes0 => ?rettypes0)
	(?argtypes* => ?rettypes*)
	...)
      (and (make-type-signature ?argtypes0)
	   (make-type-signature ?rettypes0)
	   (map make-type-signature ?argtypes*)
	   (map make-type-signature ?rettypes*))
      (cons* (core-prim-id 'case-lambda)
	     (list ?rettypes0 (core-prim-id '=>) ?argtypes0)
	     (map (lambda (argtypes.stx rettypes.stx)
		    (list argtypes.stx (core-prim-id '=>) rettypes.stx))
	       ?argtypes* ?rettypes*)))

     ((condition ?component-type* ...)
      (list (core-prim-id 'condition)
	    (map recur ?component-type*)))

     ((or ?component-type ?component-type* ...)
      (list (core-prim-id 'or)
	    (map recur (cons ?component-type ?component-type*))))

     ((and ?component-type ?component-type* ...)
      (list (core-prim-id 'and)
	    (map recur (cons ?component-type ?component-type*))))

     ((not ?item-type)
      (list (core-prim-id 'not)
	    (recur ?item-type)))

     (else #f))))

;;; --------------------------------------------------------------------

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
   (syntax-match annotation.stx (pair list vector pair-of list-of vector-of hashtable alist condition
				      or and not lambda case-lambda =>)
     (?type-id
      (identifier? ?type-id)
      (id->object-type-spec ?type-id lexenv))

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
      (make-closure-type-spec (make-clambda-signature
			       (list
				(make-clambda-clause-signature (make-type-signature ?rettypes)
							       (make-type-signature ?argtypes))))
			      name.stx))

     ((case-lambda (?argtypes0 => ?rettypes0) (?argtypes* => ?rettypes*) ...)
      (make-closure-type-spec (make-clambda-signature
			       (cons (make-clambda-clause-signature
				      (make-type-signature ?rettypes0)
				      (make-type-signature ?argtypes0))
				     (map (lambda (argtypes.stx rettypes.stx)
					    (make-clambda-clause-signature
					     (make-type-signature rettypes.stx)
					     (make-type-signature argtypes.stx)))
				       ?argtypes* ?rettypes*)))
			      name.stx))

     ((or ?single-component-type)
      (type-annotation->object-type-spec ?single-component-type lexenv))

     ((or ?component-type ?component-type* ...)
      (make-union-type-spec (map (lambda (type.stx)
				   (type-annotation->object-type-spec type.stx lexenv))
			      (cons ?component-type ?component-type*))))

     ((and ?single-component-type)
      (type-annotation->object-type-spec ?single-component-type lexenv))

     ((and ?component-type ?component-type* ...)
      (make-intersection-type-spec (map (lambda (type.stx)
					  (type-annotation->object-type-spec type.stx lexenv))
				     (cons ?component-type ?component-type*))))

     ((not ?item-type)
      (make-complement-type-spec (type-annotation->object-type-spec ?item-type lexenv)))

     (else
      (syntax-violation __who__ "invalid type annotation" annotation.stx)))))

;;; --------------------------------------------------------------------

(case-define* tail-type-annotation->object-type-spec
  ;;Let's consider the following LAMBDA syntaxes:
  ;;
  ;;   (lambda/typed {args <list>}				?body)
  ;;   (lambda/typed {args (list-of <fixnum>)}			?body)
  ;;   (lambda/typed ({a <fixnum>} . {rest <list>})		?body)
  ;;   (lambda/typed ({a <fixnum>} . {rest (list-of <fixnum>)})	?body)
  ;;
  ;;the  type annotations  for the  ARGS and  REST arguments  are special:  they must
  ;;represent  lists.  It  is  established that  such type  annotations  can be  only
  ;;"<list>" or  "(list-of ?type)".
  ;;
  ;;This  function  parses   such  type  annotations  and  returns   an  instance  of
  ;;"<object-type-spce>"  representing it.   If the  type annotation  is invalid:  an
  ;;exception is raised.
  ;;
  ((annotation.stx lexenv)
   (tail-type-annotation->object-type-spec annotation.stx lexenv annotation.stx))
  ((annotation.stx lexenv name.stx)
   (define (%error)
     (syntax-violation __who__ "invalid type annotation in tail position" annotation.stx))
   (syntax-match annotation.stx (<list> list-of)
     (<list>
      (<list>-ots))
     ((list-of ?type-ann)
      (make-list-of-type-spec (type-annotation->object-type-spec ?type-ann lexenv)
			      name.stx))
     ;;This allows:
     ;;
     ;;   (define-type <list-of-fixnums> (list-of <fixnum>))
     ;;   (define-type <some-list> <list>)
     ;;
     (?type-ann
      (identifier? ?type-ann)
      (receive-and-return (ots)
	  (id->object-type-spec ?type-ann lexenv)
	(unless (or (<list>-ots? ots)
		    (list-of-type-spec? ots))
	  (%error))))

     (else
      (%error)))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
