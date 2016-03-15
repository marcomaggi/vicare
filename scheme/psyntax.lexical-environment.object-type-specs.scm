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


(module (<object-type-spec>
	 object-type-spec?
	 object-type-spec.name				object-type-spec.parent-ots
	 object-type-spec.constructor-stx		object-type-spec.destructor-stx
	 object-type-spec.type-predicate-stx
	 object-type-spec.safe-accessor-stx		object-type-spec.safe-mutator-stx
	 object-type-spec.applicable-method-stx

	 object-type-spec.super-and-sub?		object-type-spec.matching-super-and-sub?
	 object-type-spec.common-ancestor
	 object-type-spec.procedure?
	 object-type-spec.list-sub-type?
	 object-type-spec.vector-sub-type?

	 <scheme-type-spec>
	 make-scheme-type-spec				scheme-type-spec?

	 <closure-type-spec>
	 make-closure-type-spec				closure-type-spec?
	 closure-type-spec.signature

	 <struct-type-spec>
	 make-struct-type-spec				struct-type-spec?
	 struct-type-spec.std

	 <record-type-spec>
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

	 syntax-object.parse-type-annotation			syntax-object.type-annotation?
	 type-annotation->object-type-specification

	 #| end of export list |# )


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
				      safe-accessors-table safe-mutators-table methods-table)
	(make-record name parent.ots
		     constructor-stx destructor-stx type-predicate-stx
		     safe-accessors-table safe-mutators-table methods-table))
      make-object-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[object-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------

(define (list-of-object-type-spec? obj)
  (if (pair? obj)
      (and (object-type-spec? (car obj))
	   (list-of-object-type-spec? (cdr obj)))
    (null? obj)))

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
    ;;If  FIELD-NAME.SYM  is EQ?   to  an  object's  field  name: return  a  symbolic
    ;;expression  (to  be BLESSed  later)  representing  a Scheme  expression  which,
    ;;expanded and evaluated at run-time, returns the field's safe mutator; otherwise
    ;;return false.
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

;;; --------------------------------------------------------------------
;;; ancestors predicates and inspection

(module (object-type-spec.super-and-sub?)

  (define* (object-type-spec.super-and-sub? {super.ots object-type-spec?} {sub.ots object-type-spec?})
    ;;Return true if SUB.OTS is a subtype of SUPER.OTS; otherwise return false.
    ;;
    ;;NOTE This function is  a single point of truth: it is  the *only* function that
    ;;determines if two OTSs are super-type and sub-type.
    ;;
    (cond ((<top>-ots? super.ots)
	   ;;Fast track.
	   #t)
	  ((<null>-ots? sub.ots)
	   ;;Special case: we consider "<null>"  as sub-type of "<list>" and sub-type
	   ;;of all  the sub-types of  "<list>".  We  need to remember  that "<null>"
	   ;;cannot be sub-typed.  The diagram of "<list>" sub-types is:
	   ;;
	   ;;   <list> ---> <null>
	   ;;     |
	   ;;     +-------> (list-of <fixnum>) ---> <null>
	   ;;     |
	   ;;     +-------> (list-of <string>) ---> <null>
	   ;;     |
	   ;;     ...
	   ;;
	   (%scan-parents-of-sub-ots (<list>-ots) super.ots))
	  ((<empty-vector>-ots? sub.ots)
	   ;;Special case: we consider "<empty-vector>" as sub-type of "<vector>" and
	   ;;sub-type of all  the sub-types of "<vector>".  We need  to remmeber that
	   ;;"<empty-vector>"  cannot  be  sub-typed.    The  diagram  of  "<vector>"
	   ;;sub-types is:
	   ;;
	   ;;   <vector> ---> <empty-vector>
	   ;;     |
	   ;;     +-------> (vector-of <fixnum>) ---> <empty-vector>
	   ;;     |
	   ;;     +-------> (vector-of <string>) ---> <empty-vector>
	   ;;     |
	   ;;     ...
	   ;;
	   (%scan-parents-of-sub-ots (<vector>-ots) super.ots))
	  (else
	   (%scan-parents-of-sub-ots super.ots sub.ots))))

  (define (%scan-parents-of-sub-ots super.ots sub.ots)
    (or (eq? sub.ots super.ots)
	(cond ((object-type-spec.parent-ots sub.ots)
	       => (lambda (sub-parent.ots)
		    (%scan-parents-of-sub-ots super.ots sub-parent.ots)))
	      (else #f))))

  #| end of module: OBJECT-TYPE-SPEC.SUPER-AND-SUB? |# )

;;; --------------------------------------------------------------------

(define* (object-type-spec.matching-super-and-sub? {super.ots object-type-spec?} {sub.ots object-type-spec?})
  ;;In the context of an operator application:
  ;;
  ;;   (?operator ?operand ...)
  ;;
  ;;return true  if a  value of type  SUB.OTS used as  operand matches  an operator's
  ;;argument of type of SUPER.OTS; otherwise return false.  This is more general than
  ;;simple "super  and sub"  because it  matches sublist  types, subvector  types and
  ;;procedure types.
  ;;
  ;;NOTE This  function is a single  point of truth:  it is the *only*  function that
  ;;determines if two OTSs are matching super-type and sub-type.
  ;;
  ;;NOTE We need to remember that the following OTSs cannot be subtyped:
  ;;
  ;;   <pair-type-spec>
  ;;   <list-type-spec>
  ;;   <vector-type-spec>
  ;;   <pair-of-type-spec>
  ;;   <list-of-type-spec>
  ;;   <vector-of-type-spec>
  ;;   <compound-condition-type-spec>
  ;;
  (define (%scan-parents-of-sub-ots super.ots sub.ots)
    (or (eq? sub.ots super.ots)
	(cond ((object-type-spec.parent-ots sub.ots)
	       => (lambda (sub-parent.ots)
		    (%scan-parents-of-sub-ots super.ots sub-parent.ots)))
	      (else #f))))

  (cond ((eq? super.ots sub.ots))

	((<top>-ots? super.ots)
	 ;;Fast track: "<top>" is the super-type of all the types.
	 #t)

	((<null>-ots? sub.ots)
	 ;;Special case: we consider "<null>" as sub-type of "<list>" and sub-type of
	 ;;all the sub-types  of "<list>".  We need to remember  that "<null>" cannot
	 ;;be sub-typed.  The diagram of "<list>" sub-types is:
	 ;;
	 ;;   <list> ---> <null>
	 ;;     |
	 ;;     +-------> (list-of <fixnum>) ---> <null>
	 ;;     |
	 ;;     +-------> (list-of <string>) ---> <null>
	 ;;     |
	 ;;     ...
	 ;;
	 ;;So if SUPER.OTS is "<list>" or one of its subtypes: match.
	 (%scan-parents-of-sub-ots (<list>-ots) super.ots))

	((<empty-vector>-ots? sub.ots)
	 ;;Special case: we  consider "<empty-vector>" as sub-type  of "<vector>" and
	 ;;sub-type of  all the sub-types  of "<vector>".   We need to  remmeber that
	 ;;"<empty-vector>" cannot be sub-typed.  The diagram of "<vector>" sub-types
	 ;;is:
	 ;;
	 ;;   <vector> ---> <empty-vector>
	 ;;     |
	 ;;     +-------> (vector-of <fixnum>) ---> <empty-vector>
	 ;;     |
	 ;;     +-------> (vector-of <string>) ---> <empty-vector>
	 ;;     |
	 ;;     ...
	 ;;
	 ;;So if SUPER.OTS is "<vector>" or one of its subtypes: match.
	 (%scan-parents-of-sub-ots (<vector>-ots) super.ots))

	((<list>-ots? sub.ots)
	 (or (<null>-ots?        sub.ots)
	     (list-of-type-spec? sub.ots)
	     (list-type-spec?    sub.ots)))

	((list-of-type-spec? sub.ots)
	 (cond ((list-of-type-spec? super.ots)
		(object-type-spec.matching-super-and-sub? (list-of-type-spec.item-ots super.ots)
							  (list-of-type-spec.item-ots sub.ots)))
	       (else
		(%scan-parents-of-sub-ots super.ots sub.ots))))

	((vector-of-type-spec? sub.ots)
	 (cond ((vector-of-type-spec? super.ots)
		(object-type-spec.matching-super-and-sub? (vector-of-type-spec.item-ots super.ots)
							  (vector-of-type-spec.item-ots sub.ots)))
	       (else
		(%scan-parents-of-sub-ots super.ots sub.ots))))

	((compound-condition-type-spec? super.ots)
	 (cond ((compound-condition-type-spec? sub.ots)
		;;This is the case:
		;;
		;;   (is-a? (condition (make-who-condition 'ciao)
		;;                     (make-message-condition "ciao"))
		;;          (condition &who &message))
		;;
		;;every condition-object type in the super must be present in the sub.
		(let ((super-component*.ots (compound-condition-type-spec.component-ots* super.ots))
		      (sub-component*.ots   (compound-condition-type-spec.component-ots* sub.ots)))
		  (for-all (lambda (super-component.ots)
			     (exists (lambda (sub-component.ots)
				       (object-type-spec.super-and-sub? super-component.ots sub-component.ots))
			       sub-component*.ots))
		    super-component*.ots)))
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
	 (object-type-spec.super-and-sub? (<compound-condition>-ots) super.ots))

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
			    (object-type-spec.super-and-sub? super.ots sub-component.ots))
		    sub-component*.ots)))
	       ((simple-condition-object-type-spec? sub.ots)
		;;This is the case:
		;;
		;;   (is-a? (make-who-condition 'ciao) &who)		=> #t
		;;   (is-a? (make-who-condition 'ciao) &message)	=> #f
		;;
		(object-type-spec.super-and-sub? super.ots sub.ots))
	       (else #f)))

	((object-type-spec.parent-ots sub.ots)
	 => (lambda (sub-parent.ots)
	      (object-type-spec.matching-super-and-sub? super.ots sub-parent.ots)))

	(else #f)))

;;; --------------------------------------------------------------------

(define (object-type-spec.common-ancestor ots1 ots2)
  ;;Search the hierarchies of OTS1 and OTS2 looking for a common ancestor.  Return an
  ;;instance of "<object-type-spec>" representing the ancestor's OTS.  If no ancestor
  ;;is found: return the OTS of "<top>".
  ;;
  (cond ((eq? ots1 ots2)
	 ots1)
	((or (<top>-ots? ots1)
	     (<top>-ots? ots2))
	 ots1)
	((<no-return>-ots? ots1)
	 ots2)
	((<no-return>-ots? ots2)
	 ots1)
	(else
	 (let scan-parents-of-ots1 ((ots1 ots1))
	   (let scan-parents-of-ots2 ((ots2 ots2))
	     (if (eq? ots1 ots2)
		 ots1
	       (cond ((object-type-spec.parent-ots ots2)
		      => scan-parents-of-ots2)
		     (else
		      (cond ((object-type-spec.parent-ots ots1)
			     => scan-parents-of-ots1)
			    (else
			     (<top>-ots)))))))))))

(define (object-type-spec.procedure? ots)
  (or (closure-type-spec? ots)
      (<procedure>-ots? ots)))

(define (object-type-spec.list-sub-type? ots)
  (object-type-spec.super-and-sub? (<list>-ots) ots))

(define (object-type-spec.vector-sub-type? ots)
  (object-type-spec.super-and-sub? (<vector>-ots) ots))


;;;; built-in Scheme object-type specification
;;
;;This record-type  is the  base type  for all  the type  specifications representing
;;Scheme  objects,  not  records,  not  structs.  Instances  of  this  type  are  the
;;object-type specifications for: <fixnum>, <flonum>, <string>, <list>, ...
;;
(define-record-type (<scheme-type-spec> make-scheme-type-spec scheme-type-spec?)
  (nongenerative vicare:expander:<scheme-type-spec>)
  (parent <object-type-spec>)
  (protocol
    (lambda (make-object-type-spec)
      (define* (make-scheme-type-spec {name identifier?}
				      {parent.ots (or not scheme-type-spec?)}
				      constructor.stx predicate.stx methods-table)
	(let ((destructor.stx	#f)
	      (accessors-table	'())
	      (mutators-table	'()))
	  ((make-object-type-spec name parent.ots
				  constructor.stx destructor.stx predicate.stx
				  accessors-table mutators-table methods-table))))
      make-scheme-type-spec))
  (custom-printer
    (lambda (S port sub-printer)
      (display "#[scheme-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))
  #| end of DEFINE-RECORD-TYPE |# )


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
	(let ((parent.ots	(<struct>-ots))
	      (destructor.stx	(bless `(internal-applicable-struct-type-destructor ,std))))
	  ((make-object-type-spec name parent.ots
				  constructor.id destructor.stx predicate.id
				  safe-accessors-table safe-mutators-table methods-table)
	   std)))
      make-struct-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[struct-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-STRUCT-TYPE |# )


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
				      safe-accessors-table safe-mutators-table methods-table)
	(let ((parent-name.ots	(cond ((<record>-type-id? parent-name.id)
				       (<record>-ots))
				      ((<condition>-type-id? parent-name.id)
				       (<condition>-ots))
				      (else
				       (id->record-type-specification __who__ #f parent-name.id (current-inferior-lexenv)))))
	      (constructor.stx	(or constructor.stx
				    (bless `(record-constructor ,rcd-id))))
	      (predicate.stx	(or predicate.stx
				    (let ((arg.sym (gensym)))
				      (bless
				       `(lambda/std (,arg.sym)
					  (record-and-rtd? ,arg.sym ,rtd-id)))))))
	  ((make-object-type-spec type-name parent-name.ots
				  constructor.stx destructor.stx predicate.stx
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
  ;;      (type-annotation->object-type-specification
  ;;         (core-type-id '&condition)))
  ;;   => #t
  ;;
  ;;   (simple-condition-object-type-spec?
  ;;      (type-annotation->object-type-specification
  ;;         (core-type-id '&message)))
  ;;   => #t
  ;;
  ;;   (simple-condition-object-type-spec?
  ;;      (type-annotation->object-type-specification
  ;;         (core-type-id '<compound-condition>)))
  ;;   => #f
  ;;
  ;;   (simple-condition-object-type-spec?
  ;;      (type-annotation->object-type-specification
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
       (object-type-spec.super-and-sub? (&condition-ots) ots)))


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
      (case-define* make-compound-condition-type-spec
	((component-type*.ots)
	 (make-compound-condition-type-spec component-type*.ots (cons (core-prim-id 'condition)
								      (map object-type-spec.name component-type*.ots))))
	((component-type*.ots {name.stx compound-condition-name?})
	 (let ((component-type*.ots (%collapse-component-specs component-type*.ots)))
	   (let* ((parent.ots		(<compound-condition>-ots))
		  (constructor.stx	#f)
		  (destructor.stx	#f)
		  (predicate.stx	(make-compound-condition-predicate
					 (map object-type-spec.type-predicate-stx component-type*.ots)))
		  (accessors-table	'())
		  (mutators-table	'())
		  (methods-table	'()))
	     ((make-object-type-spec name.stx parent.ots
				     constructor.stx destructor.stx predicate.stx
				     accessors-table mutators-table methods-table)
	      component-type*.ots)))))

      (define (compound-condition-name? name.stx)
	(syntax-match name.stx (condition)
	  ((condition ?component-type* ...)
	   (for-all syntax-object.type-annotation? ?component-type*)
	   #t)
	  (?type-id
	   (identifier? ?type-id)
	   #t)
	  (else #f)))

      (define (%collapse-component-specs component-type*.ots)
	(fold-right (lambda (component.ots knil)
		      (cond ((object-type-spec.super-and-sub? (&condition-ots) component.ots)
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
      (define* (make-closure-type-spec name {signature callable-signature?})
	(let ((parent.ots		(<procedure>-ots))
	      (constructor.stx		#f)
	      (destructor.stx		#f)
	      (predicate.stx		#f)
	      (accessors-table		'())
	      (mutators-table		'())
	      (methods-table		'()))
	  ((make-object-type-spec name parent.ots
				  constructor.stx destructor.stx predicate.stx
				  accessors-table mutators-table methods-table)
	   signature)))
      make-closure-type-spec))

  (custom-printer
    (lambda (S port sub-printer)
      (display "#[closure-type-spec " port)
      (display (object-type-spec.name S) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

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
	 (let* ((parent.ots		(core-prim-id '<pair>))
		(constructor.stx	#f)
		(destructor.stx	#f)
		(predicate.stx		(make-pair-predicate (object-type-spec.type-predicate-stx car.ots)
							     (object-type-spec.type-predicate-stx cdr.ots)))
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   accessors-table mutators-table methods-table)
	    car.ots cdr.ots))))

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
	 (let* ((parent.ots		(core-prim-id '<pair>))
		(constructor.stx	#f)
		(destructor.stx	#f)
		(predicate.stx		(make-pair-of-predicate (object-type-spec.type-predicate-stx item.ots)))
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
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
;;"<list>" representing  proper list objects  holding items of a  known heterogeneous
;;type.
;;
(define-record-type (<list-type-spec> make-list-type-spec list-type-spec?)
  (nongenerative vicare:expander:<list-type-spec>)
  (parent <object-type-spec>)
  (sealed #t)
  (fields
    (immutable item-ots*		list-type-spec.item-ots*)
		;A list of instances of  "<object-type-spec>" describing the types of
		;contained items.
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-list-type-spec
	(({item-type*.ots list-of-object-type-spec?})
	 (make-list-type-spec item-type*.ots (cons (core-prim-id 'list)
						   (map object-type-spec.name item-type*.ots))))
	(({item-type*.ots list-of-object-type-spec?} {name.stx list-name?})
	 (let* ((parent.ots		(<list>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-list-predicate (map object-type-spec.type-predicate-stx item-type*.ots)))
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   accessors-table mutators-table methods-table)
	    item-type*.ots))))

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
		(destructor.stx	#f)
		(predicate.stx		(make-list-of-predicate (object-type-spec.type-predicate-stx item-type.ots)))
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
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
;;"<vector>"  representing   proper  vector   objects  holding   items  of   a  known
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
    #| end of FIELDS |# )
  (protocol
    (lambda (make-object-type-spec)
      (case-define* make-vector-type-spec
	(({item-type*.ots list-of-object-type-spec?})
	 (make-vector-type-spec item-type*.ots (cons (core-prim-id 'vector)
						     (map object-type-spec.name item-type*.ots))))
	(({item-type*.ots list-of-object-type-spec?} {name.stx vector-name?})
	 (let* ((parent.ots		(<vector>-ots))
		(constructor.stx	#f)
		(destructor.stx		#f)
		(predicate.stx		(make-vector-predicate (map object-type-spec.type-predicate-stx item-type*.ots)))
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name.stx parent.ots
				   constructor.stx destructor.stx predicate.stx
				   accessors-table mutators-table methods-table)
	    item-type*.ots))))

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


;;;; homogeneous vector object spec
;;
;;This record-type is  used as syntactic binding descriptor's value  for sub-types of
;;"<vector>" representing vector objects holding items of a known type.
;;
;;There can  be only  one instance of  type "<vector-of-type-spec>"  representing a
;;vector collecting items of a specific type.
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
		(accessors-table	'())
		(mutators-table		'())
		(methods-table		'()))
	   ((make-object-type-spec name parent.ots
				   constructor.stx destructor.stx predicate.stx
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

(define* (syntax-object.parse-type-annotation stx)
  ;;Recursive function.  Parse the syntax object STX as type declaration and return a
  ;;fully unwrapped syntax object representing the same type declaration.
  ;;
  (syntax-match stx (pair list vector pair-of list-of vector-of condition)
    ((pair ?car-type ?cdr-type)
     (list (core-prim-id 'pair)
	   (syntax-object.parse-type-annotation ?car-type)
	   (syntax-object.parse-type-annotation ?cdr-type)))

    ((list ?item-type* ...)
     (cons (core-prim-id 'list)
	   (map syntax-object.parse-type-annotation ?item-type*)))

    ((vector ?item-type* ...)
     (cons (core-prim-id 'vector)
	   (map syntax-object.parse-type-annotation ?item-type*)))

    ((pair-of ?item-type)
     (list (core-prim-id 'pair-of)
	   (syntax-object.parse-type-annotation ?item-type)))

    ((list-of ?item-type)
     (list (core-prim-id 'list-of)
	   (syntax-object.parse-type-annotation ?item-type)))

    ((vector-of ?item-type)
     (list (core-prim-id 'vector-of)
	   (syntax-object.parse-type-annotation ?item-type)))

    ((condition ?component-type* ...)
     (list (core-prim-id 'condition)
	   (syntax-object.parse-type-annotation ?component-type*)))

    (?type-id
     (type-identifier? ?type-id)
     ?type-id)

    (else
     (syntax-violation __who__ "invalid type declaration" stx))))

(define* (syntax-object.type-annotation? stx)
  (guard (E (else #f))
    (and (syntax-object.parse-type-annotation stx)
	 #t)))

;;; --------------------------------------------------------------------

(case-define* type-annotation->object-type-specification
  ((annotation.stx lexenv)
   (type-annotation->object-type-specification annotation.stx lexenv annotation.stx))
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
   (syntax-match annotation.stx (pair list vector pair-of list-of vector-of condition)
     ((pair ?car-type ?cdr-type)
      (make-pair-type-spec (type-annotation->object-type-specification ?car-type lexenv)
			   (type-annotation->object-type-specification ?cdr-type lexenv)
			   name.stx))

     ((list ?item-type* ...)
      (make-list-type-spec (map (lambda (type.stx)
				  (type-annotation->object-type-specification type.stx lexenv))
			     ?item-type*)
			   name.stx))

     ((vector ?item-type* ...)
      (make-vector-type-spec (map (lambda (type.stx)
				    (type-annotation->object-type-specification type.stx lexenv))
			       ?item-type*)
			     name.stx))

     ((pair-of ?item-type)
      (make-pair-of-type-spec (type-annotation->object-type-specification ?item-type lexenv)
			      name.stx))

     ((list-of ?item-type)
      (make-list-of-type-spec (type-annotation->object-type-specification ?item-type lexenv)
			      name.stx))

     ((vector-of ?item-type)
      (make-vector-of-type-spec (type-annotation->object-type-specification ?item-type lexenv)
				name.stx))

     ((condition ?component-type* ...)
      (let ((specs (map (lambda (type.stx)
			  (type-annotation->object-type-specification type.stx lexenv))
		     ?component-type*)))
	;;We want:
	;;
	;;   (condition &who) == &who
	;;   (condition (condition ...)) == (condition ...)
	;;   (condition <compound-condition>) == <compound-condition>
	;;
	(if (list-of-single-item? specs)
	    (let ((ots (car specs)))
	      (cond ((or (simple-condition-object-type-spec? ots)
			 (compound-condition-type-spec?      ots)
			 (<compound-condition>-ots?          ots))
		     ots)
		    (else
		     (assertion-violation __who__
		       "expected condition object as component of compound condition object" annotation.stx))))
	  (make-compound-condition-type-spec specs name.stx))))

     (?type-id
      (type-identifier? ?type-id)
      (id->object-type-specification __who__ #f ?type-id lexenv))

     (else
      (assertion-violation __who__ "invalid type annotation" annotation.stx)))))


;;;; done

#| end of module |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
